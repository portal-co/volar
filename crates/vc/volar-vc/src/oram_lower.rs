// @reliability: experimental
// @ai: assisted
//! Symbolic-ORAM **S3**: lower a boolar circuit's symbolic-address
//! `StorageRead`/`StorageWrite` ops into in-circuit ORAM accesses, splitting the
//! guest into per-access segments.
//!
//! # Why splitting
//!
//! A garbled circuit evaluates in one pass, but an ORAM access needs a
//! round-trip to the *external* physical tree: compute the (secret) logical
//! address, reveal only the oblivious physical leaf, read that path, run the
//! client logic, write the path back. The path input depends on the leaf output
//! of the *same* access, so the access cannot live inside a single circuit
//! evaluation — the guest must be **split at each storage op** (the plan's
//! "per-access circuit splitting", approach (a)).
//!
//! # What this produces
//!
//! [`storage_to_oram`] turns a single-block [`BIrBlocks`] circuit whose storage
//! ops (on one [`StorageId`]) may have *symbolic* addresses into an
//! [`OramProgram`]:
//!
//! - The guest's live values are assigned fixed **tape slots**. Every compute
//!   segment is lowered to a uniform `tape -> tape` boolar circuit (unchanged
//!   slots pass through, costing no gates — the param wire is reused as the
//!   output wire).
//! - Each storage op becomes an [`Stage::Access`] carrying the tape slots of
//!   its address bits, write-data bit, and (for reads) result bit.
//! - The ORAM client logic is the [`crate::oram_gadget`] begin/access circuits,
//!   built once for the storage's geometry and reused for every access.
//!
//! [`run_concrete`] drives an [`OramProgram`] concretely (via `eval_biir`) with
//! the physical tree held externally in a [`volar_oram::OramTree`], mirroring
//! the S2 harness. Two-party driving and cross-step secret-state threading are
//! S4; the plaintext tree here is a correctness scaffold (encryption is S5).
//!
//! Each storage cell is one bit (boolar storage is bit-level), so the ORAM is
//! configured `data_bits = 1` — one block per cell, byte 0 bit 0 carrying the
//! value, matching the G1 convention.

use alloc::collections::BTreeMap;
use alloc::collections::BTreeSet;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};
use volar_ir_common::Node;
use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};

use crate::oram_gadget::{OramGadgetConfig, TreeCrypto, build_access, build_begin};

/// Which storage space to lower, and the ORAM geometry to serve it with.
#[derive(Clone, Copy, Debug)]
pub struct OramLowerConfig {
    /// The storage space whose ops become ORAM accesses.
    pub storage: StorageId,
    /// Path-ORAM tree depth (`num_leaves = 2^(levels-1)`).
    pub levels: usize,
    /// Bucket size `Z`.
    pub bucket_size: usize,
    /// Stash capacity (entries). `>= num_addrs` is always safe.
    pub max_stash: usize,
    /// When set (the default posture), the physical ORAM tree is **encrypted**
    /// (AES-128 per-node pads, encrypted valid bit, versioned pads) — the
    /// secure-by-default configuration. Tests set this to `false` for the fast
    /// plaintext scaffold.
    pub secure: bool,
    /// **Address narrowing**: when set, each storage address is truncated to its
    /// low `narrow_bits` bits and the ORAM spans `2^narrow_bits` cells. This is
    /// how a guest with a wide (e.g. 32-bit) symbolic address space runs on a
    /// feasible ORAM — minimal narrowing to 24 or 28 bits bounds the instance
    /// while keeping the spill/stack window intact. `None` uses the guest's
    /// full address width. The guest must keep its live addresses within the
    /// low `narrow_bits` window (a deployment/address-mapping concern).
    pub narrow_bits: Option<usize>,
}

/// An access's wiring into the guest tape.
#[derive(Clone, Debug)]
pub struct AccessInfo {
    /// `true` for a write, `false` for a read.
    pub write: bool,
    /// Tape slots of the address bits (LSB-first, `addr_bits` of them).
    pub addr_slots: Vec<usize>,
    /// Tape slot of the write-data bit (only meaningful for writes).
    pub wdata_slot: usize,
    /// Tape slot the read result bit is patched into (reads only).
    pub result_slot: Option<usize>,
}

/// One program stage. `Compute` maps the tape; `Access` runs one ORAM access.
#[derive(Clone)]
pub enum Stage {
    /// A `tape -> tape` boolar circuit (`params == tape_width`).
    Compute(BIrBlocks),
    /// One ORAM access over the tape slots in `AccessInfo`.
    Access(AccessInfo),
}

/// A symbolic-storage guest lowered to per-access segments + the ORAM gadget.
pub struct OramProgram {
    /// The ORAM geometry (`data_bits == 1`).
    pub oram: OramGadgetConfig,
    /// The ORAM begin circuit (posmap update), reused for every access.
    pub begin: BIrBlocks,
    /// The unified ORAM access circuit (absorb/select/rw/evict), reused.
    pub access: BIrBlocks,
    /// Tape width in bits (number of live guest values).
    pub tape_width: usize,
    /// Tape slot of each guest input param, in param order.
    pub input_slots: Vec<usize>,
    /// Tape slot of each guest output (the `Return` args), in order.
    pub output_slots: Vec<usize>,
    /// The stages, in order (Compute/Access alternating).
    pub stages: Vec<Stage>,
}

/// Errors from [`storage_to_oram`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OramLowerError {
    /// The circuit must be a single block with a `Jmp`-to-`Return` terminator.
    NotSingleBlockCircuit,
    /// A storage op targets a space other than the configured one.
    MixedStorage,
    /// Storage ops disagree on the address width.
    NonUniformAddress,
    /// The guest uses oracle calls, which this lowering does not handle.
    OracleUnsupported,
}

impl core::fmt::Display for OramLowerError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            OramLowerError::NotSingleBlockCircuit => {
                write!(f, "expected a single-block circuit")
            }
            OramLowerError::MixedStorage => write!(f, "storage op on a non-target space"),
            OramLowerError::NonUniformAddress => write!(f, "storage address widths disagree"),
            OramLowerError::OracleUnsupported => write!(f, "oracle calls are not supported"),
        }
    }
}

impl core::error::Error for OramLowerError {}

/// Raw var id of the value defined by statement `idx` (params occupy `0..P`).
fn stmt_var(num_params: usize, idx: usize) -> u32 {
    (num_params + idx) as u32
}

/// The vars a compute stmt reads (operands). Storage ops and oracles are
/// handled separately by the caller.
fn used_vars(stmt: &BIrStmt<IRVarId, StorageId>) -> Vec<u32> {
    match stmt {
        BIrStmt::Zero | BIrStmt::One => Vec::new(),
        BIrStmt::Not(a) => alloc::vec![a.0],
        BIrStmt::And(a, b) | BIrStmt::Or(a, b) | BIrStmt::Xor(a, b) => {
            alloc::vec![a.0, b.0]
        }
        BIrStmt::StorageRead { addr, .. } => addr.iter().map(|v| v.0).collect(),
        BIrStmt::StorageWrite { addr, src, .. } => {
            let mut v: Vec<u32> = addr.iter().map(|v| v.0).collect();
            v.push(src.0);
            v
        }
        _ => Vec::new(),
    }
}

/// Lower a single-block circuit's symbolic storage ops into an [`OramProgram`].
pub fn storage_to_oram<P: Clone>(
    circuit: &BIrBlocks<P>,
    cfg: &OramLowerConfig,
) -> Result<OramProgram, OramLowerError> {
    if circuit.blocks.len() != 1 {
        return Err(OramLowerError::NotSingleBlockCircuit);
    }
    let block = &circuit.blocks[0];
    // The terminator must be a Jmp-to-Return (circuit-shaped).
    let term_args: Vec<u32> = match &block.terminator {
        BIrTerminator::Jmp(t) if t.block == IRBlockTargetId::Return => {
            t.args.iter().map(|a| a.0).collect()
        }
        _ => return Err(OramLowerError::NotSingleBlockCircuit),
    };
    let num_params = block.params as usize;
    let n = block.stmts.len();

    // Locate the storage ops on the target space; reject oracles and other
    // spaces. Record each op's addr width (must be uniform).
    let mut storage_idx: Vec<usize> = Vec::new();
    let mut ab: Option<usize> = None;
    for (i, node) in block.stmts.iter().enumerate() {
        match &node.kind {
            BIrStmt::StorageRead { storage, addr, .. }
            | BIrStmt::StorageWrite { storage, addr, .. } => {
                if *storage != cfg.storage {
                    return Err(OramLowerError::MixedStorage);
                }
                match ab {
                    None => ab = Some(addr.len()),
                    Some(w) if w != addr.len() => return Err(OramLowerError::NonUniformAddress),
                    _ => {}
                }
                storage_idx.push(i);
            }
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::OracleProjectedBit { .. } => {
                return Err(OramLowerError::OracleUnsupported)
            }
            _ => {}
        }
    }
    let ab = ab.unwrap_or(0);
    // Address narrowing: truncate to the low `narrow_bits` bits if requested.
    let eff_ab = match cfg.narrow_bits {
        Some(n) => n.min(ab),
        None => ab,
    };
    let num_addrs = 1usize << eff_ab;

    // --- Liveness: backward sweep, recording the live set at each boundary. ---
    // A var needs a tape slot iff it is live across a segment boundary. The
    // boundaries are: start (params live-in), around each access, and the end
    // (terminator args).
    let is_storage: BTreeSet<usize> = storage_idx.iter().copied().collect();
    let mut slot_vars: BTreeSet<u32> = BTreeSet::new();
    let mut live: BTreeSet<u32> = term_args.iter().copied().collect();
    slot_vars.extend(live.iter().copied());
    // Walk backward. `live` is the set live *after* stmt i (live-in to what
    // follows). At a storage op, record the after-access set, then update for
    // the op and record the before-access set.
    for i in (0..n).rev() {
        if is_storage.contains(&i) {
            slot_vars.extend(live.iter().copied()); // after-access boundary
            let def = stmt_var(num_params, i);
            live.remove(&def);
            live.extend(used_vars(&block.stmts[i].kind));
            slot_vars.extend(live.iter().copied()); // before-access boundary
        } else {
            let def = stmt_var(num_params, i);
            live.remove(&def);
            live.extend(used_vars(&block.stmts[i].kind));
        }
    }
    // Params are always slotted so the driver can place inputs.
    for p in 0..num_params {
        slot_vars.insert(p as u32);
    }

    // Assign slots (sorted for determinism).
    let slot_of: BTreeMap<u32, usize> = slot_vars
        .iter()
        .copied()
        .enumerate()
        .map(|(s, v)| (v, s))
        .collect();
    let tape_width = slot_of.len();
    // slot -> the var it holds (to detect in-segment definitions).
    let mut slot_var: Vec<u32> = alloc::vec![0; tape_width];
    for (v, s) in &slot_of {
        slot_var[*s] = *v;
    }

    let input_slots: Vec<usize> = (0..num_params).map(|p| slot_of[&(p as u32)]).collect();
    let output_slots: Vec<usize> = term_args.iter().map(|v| slot_of[v]).collect();

    // --- Lower each compute segment to a tape -> tape circuit. ---
    // Segments are the runs of non-storage stmts between storage ops.
    let mut boundaries: Vec<usize> = Vec::new(); // stmt index of each storage op
    boundaries.extend(storage_idx.iter().copied());
    boundaries.push(n); // sentinel end

    // Build the ORAM gadget circuits once for this geometry. Encryption is the
    // default posture (`OramGadgetConfig::secure`); tests set `secure: false`
    // for the fast plaintext scaffold.
    let mut oram = if cfg.secure {
        OramGadgetConfig::secure(num_addrs, cfg.levels, cfg.bucket_size, 1)
    } else {
        OramGadgetConfig {
            num_addrs,
            levels: cfg.levels,
            bucket_size: cfg.bucket_size,
            data_bits: 1,
            max_stash: cfg.max_stash,
            encrypted: false,
            tree_key_bits: 0,
            encrypt_valid: false,
            keyed_leaf: false,
            versioned_pads: false,
            version_bits: 0,
        }
    };
    oram.max_stash = cfg.max_stash;
    let begin = build_begin(&oram);
    let access = build_access(&oram);

    let mut stages: Vec<Stage> = Vec::new();
    let mut prev = 0usize;
    for &b in &boundaries {
        // Lower the compute segment stmts[prev..b].
        let seg = lower_segment(block, prev, b, &slot_of, &slot_var, tape_width);
        stages.push(Stage::Compute(seg));
        // If b is a real storage op (not the sentinel), emit its access stage.
        if b < n {
            let info = match &block.stmts[b].kind {
                BIrStmt::StorageRead { addr, .. } => AccessInfo {
                    write: false,
                    addr_slots: addr.iter().take(eff_ab).map(|v| slot_of[&v.0]).collect(),
                    wdata_slot: 0,
                    result_slot: Some(slot_of[&stmt_var(num_params, b)]),
                },
                BIrStmt::StorageWrite { addr, src, .. } => AccessInfo {
                    write: true,
                    addr_slots: addr.iter().take(eff_ab).map(|v| slot_of[&v.0]).collect(),
                    wdata_slot: slot_of[&src.0],
                    result_slot: None,
                },
                _ => unreachable!("boundary is a storage op"),
            };
            stages.push(Stage::Access(info));
        }
        prev = b + 1;
    }

    Ok(OramProgram {
        oram,
        begin,
        access,
        tape_width,
        input_slots,
        output_slots,
        stages,
    })
}

/// Lower one compute segment (`block.stmts[start..end]`, all non-storage) to a
/// `tape -> tape` circuit. Unchanged slots pass through (param wire reused).
fn lower_segment<P: Clone>(
    block: &BIrBlock<P>,
    start: usize,
    end: usize,
    slot_of: &BTreeMap<u32, usize>,
    slot_var: &[u32],
    tape_width: usize,
) -> BIrBlocks {
    let num_params = block.params as usize;
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    // wire for a var: params are slots 0..tape_width; segment-local vars get
    // fresh wires at tape_width + (local index).
    let mut local: BTreeMap<u32, u32> = BTreeMap::new();
    let mut next_wire = tape_width as u32;

    let mut map_var = |v: u32, local: &BTreeMap<u32, u32>| -> u32 {
        if let Some(&w) = local.get(&v) {
            w
        } else {
            slot_of[&v] as u32
        }
    };

    for idx in start..end {
        let stmt = &block.stmts[idx].kind;
        let out = match stmt {
            BIrStmt::Zero => BIrStmt::Zero,
            BIrStmt::One => BIrStmt::One,
            BIrStmt::Not(a) => BIrStmt::Not(IRVarId(map_var(a.0, &local))),
            BIrStmt::And(a, b) => BIrStmt::And(
                IRVarId(map_var(a.0, &local)),
                IRVarId(map_var(b.0, &local)),
            ),
            BIrStmt::Or(a, b) => BIrStmt::Or(
                IRVarId(map_var(a.0, &local)),
                IRVarId(map_var(b.0, &local)),
            ),
            BIrStmt::Xor(a, b) => BIrStmt::Xor(
                IRVarId(map_var(a.0, &local)),
                IRVarId(map_var(b.0, &local)),
            ),
            // Storage ops and oracles never appear inside a compute segment.
            _ => unreachable!("non-compute stmt in segment"),
        };
        stmts.push(Node::new(out, (), None));
        local.insert(stmt_var(num_params, idx), next_wire);
        next_wire += 1;
    }

    // Outputs: for each slot, the new wire for its var (local if defined here,
    // else the pass-through param wire).
    let outputs: Vec<IRVarId> = (0..tape_width)
        .map(|s| {
            let v = slot_var[s];
            if let Some(&w) = local.get(&v) {
                IRVarId(w)
            } else {
                IRVarId(s as u32)
            }
        })
        .collect();

    BIrBlocks {
        blocks: alloc::vec![BIrBlock {
            params: tape_width as u32,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: outputs,
            }),
        }],
        pre_init: alloc::vec![],
    }
}

// --- concrete driver ---------------------------------------------------------

/// Minimal concrete evaluator for a single-block `Return`-terminated boolar
/// circuit (Zero/One/Not/And/Or/Xor only — the gadget and compute segments use
/// no other ops). Returns the output wires' values.
fn eval_circuit(circuit: &BIrBlocks, inputs: &[bool]) -> Vec<bool> {
    let block = &circuit.blocks[0];
    let mut wires: Vec<bool> = inputs.to_vec();
    for node in &block.stmts {
        let v = match node.kind {
            BIrStmt::Zero => false,
            BIrStmt::One => true,
            BIrStmt::Not(a) => !wires[a.0 as usize],
            BIrStmt::And(a, b) => wires[a.0 as usize] && wires[b.0 as usize],
            BIrStmt::Or(a, b) => wires[a.0 as usize] || wires[b.0 as usize],
            BIrStmt::Xor(a, b) => wires[a.0 as usize] ^ wires[b.0 as usize],
            _ => panic!("eval_circuit: unsupported stmt"),
        };
        wires.push(v);
    }
    match &block.terminator {
        BIrTerminator::Jmp(t) if t.block == IRBlockTargetId::Return => {
            t.args.iter().map(|a| wires[a.0 as usize]).collect()
        }
        _ => panic!("eval_circuit: not a circuit"),
    }
}

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

fn flatten_path<const Z: usize>(path: &[Bucket<Z, 1>], cfg: &OramGadgetConfig) -> Vec<bool> {
    // Encrypted trees store the whole `eb`-bit (ciphertext) entry in `data`;
    // plaintext trees store {valid, addr, leaf, data} fields.
    if cfg.encrypted {
        let eb = cfg.entry_bits();
        let mut v = Vec::new();
        for bucket in path {
            for e in &bucket.entries {
                for i in 0..eb {
                    v.push((e.data[i / 8] >> (i % 8)) & 1 == 1);
                }
            }
        }
        return v;
    }
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            let real = e.is_real();
            v.push(real);
            v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
            v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
            v.extend(enc(if real { e.data[0] as u64 } else { 0 }, cfg.data_bits));
        }
    }
    v
}

fn unflatten_path<const Z: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, 1>> {
    let eb = cfg.entry_bits();
    if cfg.encrypted {
        return (0..cfg.levels)
            .map(|level| Bucket {
                entries: core::array::from_fn(|s| {
                    let k = level * Z + s;
                    let mut data = [0u8; 1];
                    for i in 0..eb {
                        if bits[k * eb + i] {
                            data[i / 8] |= 1 << (i % 8);
                        }
                    }
                    OramEntry { addr: 0, leaf: 0, data }
                }),
            })
            .collect();
    }
    let mut out = Vec::new();
    for level in 0..cfg.levels {
        let mut entries = [OramEntry::dummy(); Z];
        for (s, slot) in entries.iter_mut().enumerate() {
            let k = level * Z + s;
            let e = &bits[k * eb..(k + 1) * eb];
            *slot = if e[0] {
                OramEntry {
                    addr: dec(&e[1..1 + cfg.addr_bits()]),
                    leaf: dec(&e[1 + cfg.addr_bits()..1 + cfg.addr_bits() + cfg.leaf_bits()]),
                    data: [dec(&e[1 + cfg.addr_bits() + cfg.leaf_bits()..]) as u8],
                }
            } else {
                OramEntry::dummy()
            };
        }
        out.push(Bucket { entries });
    }
    out
}

/// Splitmix64 leaf stream (deterministic test RNG only — NOT the production
/// leaf-PRF; see the plan's security model).
struct Splitmix64(u64);
impl Splitmix64 {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
}

/// A **persistent** concrete ORAM driver: holds the ORAM state (tree, posmap,
/// stash, access counter, leaf RNG, tree crypto) so a *looping* guest can run
/// many [`OramProgram`] steps against one shared ORAM. This is the concrete
/// counterpart of the two-party [`crate::oram_2pc::Oram2pc`] loop driver.
pub struct ConcreteOramDrive<const Z: usize> {
    tree: OramTree<Z, 1>,
    posmap_bits: Vec<bool>,
    stash_bits: Vec<bool>,
    counter: u64,
    leaf_rng: Splitmix64,
    crypto: TreeCrypto,
}

impl<const Z: usize> ConcreteOramDrive<Z> {
    /// Fresh drive for a given geometry (`cfg`), pre-formatting the tree when
    /// the config encrypts the valid bit.
    pub fn new(cfg: &OramGadgetConfig) -> Self {
        assert_eq!(cfg.bucket_size, Z, "bucket size must match the tree");
        assert_eq!(cfg.data_bits, 1, "bit-level storage");
        assert!(cfg.tree_block_bytes() <= 1, "run_concrete: B=1 tree needs entry_bits <= 8");
        let mut tree: OramTree<Z, 1> = OramTree::new(cfg.levels);
        let mut crypto = TreeCrypto::new([0xA5; 16], tree.buckets.len(), cfg);
        if cfg.encrypted {
            crypto.format_tree(cfg, &mut tree);
        }
        ConcreteOramDrive {
            posmap_bits: alloc::vec![false; cfg.num_addrs * cfg.leaf_bits()],
            stash_bits: alloc::vec![false; cfg.max_stash * cfg.entry_bits()],
            tree,
            counter: 0,
            leaf_rng: Splitmix64(0x5EED),
            crypto,
        }
    }

    /// The current tree (for inspection).
    pub fn tree(&self) -> &OramTree<Z, 1> {
        &self.tree
    }

    /// Run one [`OramProgram`] step against the shared ORAM, returning the guest
    /// outputs. Panics on ORAM stash overflow (a protocol abort).
    pub fn run_program(&mut self, program: &OramProgram, inputs: &[bool]) -> Vec<bool> {
        let cfg = &program.oram;
        let lb = cfg.leaf_bits();
        let ab = cfg.addr_bits();
        let db = cfg.data_bits;
        let eb = cfg.entry_bits();
        let n_path = cfg.levels * Z;
        let num_leaves = cfg.num_leaves() as u64;
        let tree_key_bits = self.crypto.key_bits();

        let mut tape = alloc::vec![false; program.tape_width];
        for (i, &b) in inputs.iter().enumerate() {
            tape[program.input_slots[i]] = b;
        }

        for stage in &program.stages {
            match stage {
                Stage::Compute(circuit) => {
                    tape = eval_circuit(circuit, &tape);
                }
                Stage::Access(info) => {
                    let addr = dec(&info.addr_slots.iter().map(|&s| tape[s]).collect::<Vec<_>>());
                    let new_leaf = self.leaf_rng.next() % num_leaves;

                    // begin: posmap update -> old_leaf
                    let mut begin_in = self.posmap_bits.clone();
                    begin_in.extend(enc(addr, ab));
                    begin_in.extend(enc(new_leaf, lb));
                    let begin_out = eval_circuit(&program.begin, &begin_in);
                    let old_leaf = dec(&begin_out[..lb]);
                    self.posmap_bits = begin_out[lb..].to_vec();

                    // extern: read the main path; access (main); write it back.
                    let main_path = self.tree.read_path(old_leaf);
                    let mut acc_in = self.stash_bits.clone();
                    acc_in.extend(flatten_path(&main_path, cfg));
                    acc_in.extend(enc(addr, ab));
                    acc_in.push(info.write);
                    acc_in.extend(enc(if info.write { tape[info.wdata_slot] as u64 } else { 0 }, db));
                    acc_in.extend(enc(old_leaf, lb));
                    acc_in.extend(enc(new_leaf, lb));
                    acc_in.push(false);
                    if cfg.encrypted {
                        acc_in.extend(tree_key_bits.iter().copied());
                        if cfg.versioned_pads {
                            for v in self.crypto.path_versions(&self.tree, old_leaf) {
                                acc_in.extend(enc(v, cfg.version_bits));
                            }
                        }
                    }
                    let acc_out = eval_circuit(&program.access, &acc_in);
                    assert!(!acc_out[0], "ORAM stash overflow");
                    let rdata = acc_out[1];
                    let new_path = &acc_out[1 + db..1 + db + n_path * eb];
                    self.stash_bits = acc_out[1 + db + n_path * eb..].to_vec();
                    let np = unflatten_path(new_path, cfg);
                    self.tree.write_path(old_leaf, &np);
                    self.crypto.bump_path(&self.tree, old_leaf);

                    // deterministic eviction (one pass, skipped on collision)
                    let evict_leaf = eviction_target(self.counter, num_leaves);
                    self.counter += 1;
                    if evict_leaf != old_leaf {
                        let epath = self.tree.read_path(evict_leaf);
                        let mut ev_in = self.stash_bits.clone();
                        ev_in.extend(flatten_path(&epath, cfg));
                        ev_in.extend(enc(0, ab));
                        ev_in.push(false);
                        ev_in.extend(enc(0, db));
                        ev_in.extend(enc(evict_leaf, lb));
                        ev_in.extend(enc(0, lb));
                        ev_in.push(true);
                        if cfg.encrypted {
                            ev_in.extend(tree_key_bits.iter().copied());
                            if cfg.versioned_pads {
                                for v in self.crypto.path_versions(&self.tree, evict_leaf) {
                                    ev_in.extend(enc(v, cfg.version_bits));
                                }
                            }
                        }
                        let ev_out = eval_circuit(&program.access, &ev_in);
                        assert!(!ev_out[0], "ORAM stash overflow (evict)");
                        let new_epath = &ev_out[1 + db..1 + db + n_path * eb];
                        self.stash_bits = ev_out[1 + db + n_path * eb..].to_vec();
                        let nep = unflatten_path(new_epath, cfg);
                        self.tree.write_path(evict_leaf, &nep);
                        self.crypto.bump_path(&self.tree, evict_leaf);
                    }

                    if let Some(slot) = info.result_slot {
                        tape[slot] = rdata;
                    }
                }
            }
        }

        program.output_slots.iter().map(|&s| tape[s]).collect()
    }
}

/// Drive an [`OramProgram`] concretely (via `eval_biir`) against an external
/// [`OramTree`], returning the guest outputs. The tree starts empty and is
/// returned for inspection. Panics on ORAM stash overflow (a protocol abort).
///
/// This is the S3 correctness driver: concrete, deterministic leaf RNG. Loops
/// use [`ConcreteOramDrive`] to share one ORAM across steps; two-party driving
/// is S4 (`crate::oram_2pc`).
pub fn run_concrete<const Z: usize>(
    program: &OramProgram,
    inputs: &[bool],
) -> (Vec<bool>, OramTree<Z, 1>) {
    let mut drive = ConcreteOramDrive::<Z>::new(&program.oram);
    let outputs = drive.run_program(program, inputs);
    // Destructure to return the tree.
    let ConcreteOramDrive { tree, .. } = drive;
    (outputs, tree)
}
