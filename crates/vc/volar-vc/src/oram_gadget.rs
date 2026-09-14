// @reliability: experimental
// @ai: assisted
//! In-circuit Path ORAM access gadget (symbolic-ORAM workstream, increment S1).
//!
//! This builds the **ORAM client logic** as a concrete-evaluable boolar
//! circuit, keeping the physical tree *external* — the architectural split at
//! the heart of symbolic garbled RAM (see `GRAM_SYMBOLIC_ORAM.md`):
//!
//! - **In-circuit (garbled in real deployment):** the position map, the stash,
//!   the target-block select/read/write, and the eviction packing. The logical
//!   address stays a circuit value end-to-end.
//! - **External (the "only oblivious accesses are revealed" seam):** the
//!   physical `read_path(leaf)` / `write_path(leaf, …)` against the
//!   evaluator-hosted tree. The circuit emits the leaf as an output; the host
//!   reads the path and feeds the bucket bits back as inputs.
//!
//! Because the leaf is uniform and re-randomized per access, revealing it leaks
//! nothing about the logical address (standard Path ORAM argument).
//!
//! # Decomposition
//!
//! One logical access splits at the extern boundary into two circuit kinds:
//!
//! - [`build_begin`]: `posmap.update(addr, new_leaf) -> old_leaf`, updating the
//!   position map. Run first; the revealed `old_leaf` drives `read_path`.
//! - [`build_access`]: absorb a path into the stash, (optionally) select and
//!   read/write the target block, then greedily evict along the path, emitting
//!   the buckets to write back. This one circuit serves *both* the main access
//!   (`evict_only = 0`) and the deterministic post-access eviction
//!   (`evict_only = 1`), which differ only in whether the select/read/write
//!   fires.
//!
//! The host interleaves these with tree I/O exactly as
//! `volar_oram::oram_access_local` does (the semantic oracle the S1 test checks
//! against): begin → read main path → access → write main path → read evict
//! path → access → write evict path.
//!
//! # Circuit encoding
//!
//! An ORAM entry is a flat little word, LSB-first per field:
//! `[valid:1, addr:addr_bits, leaf:leaf_bits, data:data_bits]`. Validity is an
//! explicit bit (the host maps `OramEntry`'s `u64::MAX`-dummy to `valid = 0`).
//! The position map is `num_addrs × leaf_bits`; the stash is
//! `max_stash × entry_bits`; a path is `levels × bucket_size × entry_bits`.
//!
//! # Correctness notes
//!
//! - Eviction uses **prefix equality**, not node-index arithmetic: an entry
//!   with leaf `e` may sit in the depth-`d` bucket of the path to leaf `L` iff
//!   the top-`d` bits of `e` and `L` agree (that bucket is an ancestor of
//!   `e`'s leaf). This keeps the placed block on its *own* path, preserving
//!   the Path ORAM invariant, and needs no division/shift on garbled indices.
//! - The stash is a fixed `max_stash`-slot array; an `overflow` output flag is
//!   raised if a real block could not be placed (stash exhaustion), which the
//!   host must treat as a protocol abort. With `max_stash ≥ num_addrs` no
//!   in-range access can overflow.

use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_oram::{OramEntry, OramTree};
use volar_spec::faest::aes::encrypt_block;

/// Compile-time geometry for the in-circuit ORAM gadget.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OramGadgetConfig {
    /// Number of distinct logical addresses the ORAM can serve.
    pub num_addrs: usize,
    /// Path-ORAM tree depth. `num_leaves = 2^(levels-1)`.
    pub levels: usize,
    /// Bucket size `Z` (entries per tree node).
    pub bucket_size: usize,
    /// Payload width of one block, in bits (`= B * 8` for byte blocks).
    pub data_bits: usize,
    /// Stash capacity in entries. `>= num_addrs` is always safe.
    pub max_stash: usize,
    /// When set, the physical tree is **encrypted**: `read_path` returns
    /// ciphertext and the access circuit decrypts on absorb / re-encrypts on
    /// evict under a secret `tree_key` input, so the tree-hosting evaluator
    /// cannot read block tags (the S5 obliviousness upgrade). The whole entry
    /// (`valid ++ addr ++ leaf ++ data`) is XORed with a per-node pad, so the
    /// evaluator cannot even distinguish real blocks from dummies.
    #[doc(hidden)]
    pub encrypted: bool,
    /// Width of the `tree_key` secret input (used only when `encrypted`).
    #[doc(hidden)]
    pub tree_key_bits: usize,
    /// When set (requires `encrypted`), the per-node pad also depends on a
    /// **per-node version** (bumped on every write to that node), supplied as a
    /// public per-path-node input. A stale block replayed by a malicious host
    /// was encrypted under an older version's pad, so it decrypts to garbage —
    /// versioned pads are the replay-protection mechanism (full replay
    /// *detection* needs an in-payload MAC, a malicious-security follow-up; the
    /// base threat model is semi-honest). The circuit decrypts with the current
    /// version and re-encrypts with `version + 1`.
    #[doc(hidden)]
    pub versioned_pads: bool,
    /// Width of each per-node version counter (used only when `versioned_pads`).
    #[doc(hidden)]
    pub version_bits: usize,
    /// When set (requires `encrypted`), the **`valid` bit is encrypted too** —
    /// the per-node pad covers the whole `eb`-bit entry, so the tree-hosting
    /// evaluator cannot even see the occupancy pattern (which slots hold real
    /// blocks). This closes the partial leak the plain `encrypted` mode leaves
    /// (valid plaintext so a zero tree reads as dummies). Because an all-zero
    /// ciphertext now decrypts to `pad` (garbage valid bit), the initial tree
    /// must be pre-formatted by the key holder — see [`slot_tweak_bytes`].
    #[doc(hidden)]
    pub encrypt_valid: bool,
    /// When set, the fresh leaf is computed **in-circuit** as
    /// `AES-128(leaf_key, counter)[0..leaf_bits]` (S6 keyed-leaf PRF), with
    /// `leaf_key` a secret input and `counter` a public per-access ordinal,
    /// instead of arriving as a `new_leaf` input. A publicly predictable leaf
    /// sequence would let the evaluator link an address to its previous access
    /// time; deriving the leaf from a garbled key keeps it unpredictable until
    /// it is revealed as the (oblivious) physical path.
    #[doc(hidden)]
    pub keyed_leaf: bool,
}

impl OramGadgetConfig {
    /// Bits to name a logical address.
    pub fn addr_bits(&self) -> usize {
        // Smallest k with 2^k >= num_addrs (integer ceil-log2; no_std-safe).
        let n = self.num_addrs.max(2);
        let mut k = 0;
        while (1usize << k) < n {
            k += 1;
        }
        k
    }
    /// Bits to name a leaf: `log2(num_leaves) = levels - 1`.
    pub fn leaf_bits(&self) -> usize {
        self.levels - 1
    }
    /// Number of leaves in the tree.
    pub fn num_leaves(&self) -> usize {
        1usize << self.leaf_bits()
    }
    /// Bits in one packed entry `[valid, addr, leaf, data]`.
    pub fn entry_bits(&self) -> usize {
        1 + self.addr_bits() + self.leaf_bits() + self.data_bits
    }
    /// Entries on one path (`levels * Z`).
    pub fn path_entries(&self) -> usize {
        self.levels * self.bucket_size
    }

    // --- begin-circuit layout -------------------------------------------
    /// Input width of the begin circuit. With `keyed_leaf` the `new_leaf`
    /// input is replaced by a 128-bit `leaf_key` plus a 64-bit public `counter`.
    pub fn begin_params(&self) -> usize {
        let tail = if self.keyed_leaf {
            128 + 64
        } else {
            self.leaf_bits()
        };
        self.num_addrs * self.leaf_bits() + self.addr_bits() + tail
    }
    /// Output width of the begin circuit. `old_leaf ++ posmap'`, plus the
    /// in-circuit-derived `new_leaf` (threaded to the access) when `keyed_leaf`.
    pub fn begin_outputs(&self) -> usize {
        let extra = if self.keyed_leaf { self.leaf_bits() } else { 0 };
        self.leaf_bits() + extra + self.num_addrs * self.leaf_bits()
    }

    // --- access-circuit layout ------------------------------------------
    /// Input width of the access circuit.
    pub fn access_params(&self) -> usize {
        let eb = self.entry_bits();
        let base = self.max_stash * eb            // stash
            + self.path_entries() * eb // path
            + self.addr_bits()         // addr
            + 1                        // op_write
            + self.data_bits           // wdata
            + self.leaf_bits()         // path_leaf
            + self.leaf_bits()         // new_leaf
            + 1; // evict_only
        // Encrypted tree: the secret `tree_key` is an additional input.
        (if self.encrypted {
            base + self.tree_key_bits
        } else {
            base
        }) + if self.versioned_pads {
            // One public version word per path *node* (not per slot).
            self.levels * self.version_bits
        } else {
            0
        }
    }
    /// Output width of the access circuit
    /// (`overflow ++ rdata ++ new_path ++ stash'`).
    pub fn access_outputs(&self) -> usize {
        let eb = self.entry_bits();
        1 + self.data_bits + self.path_entries() * eb + self.max_stash * eb
    }
}

/// Offset of an entry's `valid` bit.
fn off_valid() -> usize {
    0
}
/// Offset of an entry's `addr` field.
fn off_addr() -> usize {
    1
}
/// Offset of an entry's `leaf` field.
fn off_leaf(cfg: &OramGadgetConfig) -> usize {
    1 + cfg.addr_bits()
}
/// Offset of an entry's `data` field.
fn off_data(cfg: &OramGadgetConfig) -> usize {
    1 + cfg.addr_bits() + cfg.leaf_bits()
}

/// A boolar circuit under construction: params occupy var ids `0..params`,
/// each pushed gate takes the next id.
struct Builder {
    params: u32,
    stmts: Vec<Node<BIrStmt, ()>>,
}

impl Builder {
    fn new(params: u32) -> Self {
        Self {
            params,
            stmts: Vec::new(),
        }
    }
    fn gate(&mut self, s: BIrStmt) -> u32 {
        let id = self.params + self.stmts.len() as u32;
        self.stmts.push(Node::new(s, (), None));
        id
    }
    fn const0(&mut self) -> u32 {
        self.gate(BIrStmt::Zero)
    }
    fn const1(&mut self) -> u32 {
        self.gate(BIrStmt::One)
    }
    fn and(&mut self, a: u32, b: u32) -> u32 {
        self.gate(BIrStmt::And(IRVarId(a), IRVarId(b)))
    }
    fn or(&mut self, a: u32, b: u32) -> u32 {
        self.gate(BIrStmt::Or(IRVarId(a), IRVarId(b)))
    }
    fn xor(&mut self, a: u32, b: u32) -> u32 {
        self.gate(BIrStmt::Xor(IRVarId(a), IRVarId(b)))
    }
    fn not(&mut self, a: u32) -> u32 {
        self.gate(BIrStmt::Not(IRVarId(a)))
    }
    /// `MUX(c, a, b) = AND(c, XOR(a, b)) XOR b` (the `storage_to_mux_boolar`
    /// primitive).
    fn mux(&mut self, c: u32, a: u32, b: u32) -> u32 {
        let x = self.xor(a, b);
        let y = self.and(c, x);
        self.xor(y, b)
    }
    /// Bitwise [`Builder::mux`] over equal-length words.
    fn mux_word(&mut self, c: u32, a: &[u32], b: &[u32]) -> Vec<u32> {
        a.iter()
            .zip(b)
            .map(|(&ai, &bi)| self.mux(c, ai, bi))
            .collect()
    }
    /// `1` iff the LSB-first `bits` equal the constant `value`.
    fn eq_const(&mut self, bits: &[u32], value: u64) -> u32 {
        let mut acc: Option<u32> = None;
        for (j, &bit) in bits.iter().enumerate() {
            let set = (value >> j) & 1 == 1;
            let t = if set { bit } else { self.not(bit) };
            acc = Some(match acc {
                None => t,
                Some(a) => self.and(a, t),
            });
        }
        acc.unwrap_or_else(|| self.const1())
    }
    /// `1` iff LSB-first words `a` and `b` are equal (AND-fold of per-bit XNOR).
    fn eq_bits(&mut self, a: &[u32], b: &[u32]) -> u32 {
        let mut acc: Option<u32> = None;
        for (&ai, &bi) in a.iter().zip(b) {
            let x = self.xor(ai, bi);
            let xn = self.not(x);
            acc = Some(match acc {
                None => xn,
                Some(p) => self.and(p, xn),
            });
        }
        acc.unwrap_or_else(|| self.const1())
    }
    /// `1` iff the top-`depth` bits of LSB-first leaves `a` and `b` agree.
    /// `depth = 0` (the root) matches everything.
    fn prefix_eq(&mut self, a: &[u32], b: &[u32], depth: usize) -> u32 {
        let leaf_bits = a.len();
        let mut acc: Option<u32> = None;
        for j in 0..depth {
            let idx = leaf_bits - 1 - j;
            let x = self.xor(a[idx], b[idx]);
            let xn = self.not(x);
            acc = Some(match acc {
                None => xn,
                Some(p) => self.and(p, xn),
            });
        }
        acc.unwrap_or_else(|| self.const1())
    }
    /// 128-bit AES plaintext tweak naming one path slot: `(depth, zslot)` as
    /// constant bytes plus the top-`depth` bits of `path_leaf` (the node
    /// prefix). Only the prefix is included — not the full leaf — so every
    /// leaf whose path reaches a given physical node produces the *same* tweak
    /// for that node's slot, keeping the pad consistent across accesses.
    /// `zc`/`oc` are shared constant wires. AES keyed on the secret `tree_key`
    /// turns this public tweak into a pad the evaluator cannot reproduce.
    fn slot_tweak(
        &mut self,
        path_leaf: &[u32],
        depth: usize,
        zslot: usize,
        zc: u32,
        oc: u32,
    ) -> Vec<u32> {
        let lb = path_leaf.len();
        let mut tw = Vec::with_capacity(128);
        for i in 0..16 {
            let val = if i < 8 {
                (depth >> i) & 1 == 1
            } else {
                (zslot >> (i - 8)) & 1 == 1
            };
            tw.push(if val { oc } else { zc });
        }
        for j in 0..depth {
            tw.push(path_leaf[lb - 1 - j]);
        }
        while tw.len() < 128 {
            tw.push(zc);
        }
        tw
    }

    /// Like [`Builder::slot_tweak`], but mixes a per-node `version` word into
    /// the tweak at bits `32..32+version.len()` (S6 versioned pads). The caller
    /// supplies the version wires (public inputs); the circuit derives
    /// `version` and `version + 1` pads for decrypt and re-encrypt.
    fn slot_tweak_versioned(
        &mut self,
        path_leaf: &[u32],
        depth: usize,
        version: &[u32],
        zc: u32,
        oc: u32,
    ) -> Vec<u32> {
        let mut tw = self.slot_tweak(path_leaf, depth, 0, zc, oc);
        for (j, &vb) in version.iter().enumerate() {
            tw[32 + j] = vb;
        }
        tw
    }

    /// `version + 1` (LSB-first incrementer) for the re-encrypt pad.
    fn incr(&mut self, version: &[u32]) -> Vec<u32> {
        let mut carry = self.const1();
        version
            .iter()
            .map(|&b| {
                let s = self.xor(b, carry);
                carry = self.and(b, carry);
                s
            })
            .collect()
    }
    /// `value - 1` (LSB-first) with modular underflow. Callers use a public
    /// termination guard and therefore never execute an underflowing step.
    fn decr(&mut self, value: &[u32]) -> Vec<u32> {
        let mut borrow = self.const1();
        value
            .iter()
            .map(|&bit| {
                let diff = self.xor(bit, borrow);
                let not_bit = self.not(bit);
                borrow = self.and(not_bit, borrow);
                diff
            })
            .collect()
    }
    /// Inline a single-block sub-circuit: append its gates with var ids
    /// remapped (`inputs[i]` is the parent wire for sub-param `i`), returning
    /// the sub-circuit's output wires in the parent. Used to instantiate the
    /// AES PRF per path slot.
    fn inline_sub(&mut self, sub: &BIrBlocks, inputs: &[u32]) -> Vec<u32> {
        let block = &sub.blocks[0];
        assert_eq!(sub.blocks.len(), 1, "inline_sub: single-block circuit");
        assert_eq!(
            block.params as usize,
            inputs.len(),
            "inline_sub: input arity"
        );
        let mut remap: Vec<u32> = Vec::with_capacity(block.params as usize + block.stmts.len());
        remap.extend_from_slice(inputs);
        for stmt in &block.stmts {
            let s = remap_bir_stmt(&stmt.kind, &remap);
            let id = self.gate(s);
            remap.push(id);
        }
        match &block.terminator {
            BIrTerminator::Jmp(t) if t.block == IRBlockTargetId::Return => {
                t.args.iter().map(|a| remap[a.0 as usize]).collect()
            }
            _ => panic!("inline_sub: sub-circuit must end in a Return"),
        }
    }
    /// Bitwise-XOR a word with a pad (used for both decrypt and encrypt).
    fn xor_word(&mut self, a: &[u32], pad: &[u32]) -> Vec<u32> {
        a.iter()
            .zip(pad)
            .map(|(&ai, &pi)| self.xor(ai, pi))
            .collect()
    }
    fn finish(self, outputs: Vec<u32>) -> BIrBlocks {
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: self.params,
                stmts: self.stmts,
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: outputs.into_iter().map(IRVarId).collect(),
                }),
            }],
            pre_init: vec![],
        }
    }
}

/// `ceil(log2(x))`, integer-only (`f64::log2` is unavailable under `no_std`).
fn clog2(x: usize) -> usize {
    let mut b = 0;
    while (1usize << b) < x.max(1) {
        b += 1;
    }
    b
}

/// The 16-byte AES tweak naming a physical tree node — the scalar reference
/// form of the in-circuit `slot_tweak`, for harness-side pre-formatting of the
/// initial tree when `encrypt_valid` is set. `depth` is the node's level,
/// `zslot` the per-node AES variant (0 for the shared per-node pad), and
/// `prefix` the node's top-`depth` path bits (its index within the level).
///
/// With `encrypt_valid`, each initial (dummy) slot's ciphertext must equal its
/// pad so it decrypts to a zero/dummy entry; the key holder computes
/// `AES-128(tree_key, slot_tweak_bytes(d, 0, k))` and slices slot `zs`'s `eb`
/// bits from the output.
pub fn slot_tweak_bytes(depth: usize, zslot: usize, prefix: u64) -> [u8; 16] {
    let mut tw = [0u8; 16];
    tw[0] = depth as u8;
    tw[1] = zslot as u8;
    for j in 0..depth {
        let bit = (prefix >> (depth - 1 - j)) & 1;
        if bit == 1 {
            let pos = 16 + j;
            tw[pos / 8] |= 1 << (pos % 8);
        }
    }
    tw
}

/// The versioned variant of [`slot_tweak_bytes`] (S6 versioned pads): the
/// per-node `version` is mixed into the tweak at bits `32..32+version_bits`,
/// matching the in-circuit `slot_tweak_versioned`. Used by the harness to
/// pre-format and to track per-node versions.
pub fn slot_tweak_versioned_bytes(
    depth: usize,
    prefix: u64,
    version: u64,
    version_bits: usize,
) -> [u8; 16] {
    let mut tw = slot_tweak_bytes(depth, 0, prefix);
    for j in 0..version_bits {
        if (version >> j) & 1 == 1 {
            let pos = 32 + j;
            tw[pos / 8] |= 1 << (pos % 8);
        }
    }
    tw
}

impl OramGadgetConfig {
    /// The **secure-by-default** posture: the physical tree is encrypted
    /// (AES-128 per-node pads) with the `valid` bit encrypted too (occupancy
    /// hidden) and per-node versioned pads for replay protection. This is the
    /// recommended configuration; the plaintext scaffold (`encrypted: false`)
    /// remains for testing and for the S1–S4 correctness harnesses.
    ///
    /// The fresh leaf stays driver-provided (`keyed_leaf: false`); a deployment
    /// must feed it from a *secret* RNG (or enable `keyed_leaf` to derive it
    /// in-circuit). The tree must be pre-formatted by the key holder — see
    /// [`TreeCrypto::format_tree`] — and the driver tracks per-node versions.
    pub fn secure(num_addrs: usize, levels: usize, bucket_size: usize, data_bits: usize) -> Self {
        OramGadgetConfig {
            num_addrs,
            levels,
            bucket_size,
            data_bits,
            max_stash: 2 * levels + bucket_size + 16,
            encrypted: true,
            tree_key_bits: 128,
            encrypt_valid: true,
            keyed_leaf: false,
            versioned_pads: true,
            version_bits: 16,
        }
    }

    /// The tree byte width `B` needed so one `OramEntry`'s `data` holds the
    /// (encrypted) `entry_bits`-wide entry. For plaintext this is the data
    /// width; for encrypted it is `ceil(entry_bits / 8)`.
    pub fn tree_block_bytes(&self) -> usize {
        if self.encrypted {
            self.entry_bits().div_ceil(8)
        } else {
            self.data_bits.div_ceil(8).max(1)
        }
    }
}

/// Driver-side encryption state for an `encrypted` ORAM tree: the secret AES
/// tree key plus the per-node version counters (S6 versioned pads). Shared by
/// the concrete and two-party drivers so both format and version the tree
/// identically. The tree holds only ciphertext; the key holder pre-formats it.
#[derive(Clone)]
pub struct TreeCrypto {
    /// The AES-128 tree key (secret; the tree-hosting evaluator never learns it).
    pub key: [u8; 16],
    /// Per-node version counters, heap-indexed; bumped on every write.
    pub versions: Vec<u64>,
    /// Version width in bits (matches `OramGadgetConfig::version_bits`).
    pub version_bits: usize,
    /// Whether per-node versioning is active (`OramGadgetConfig::versioned_pads`).
    pub versioned: bool,
}

impl TreeCrypto {
    /// Fresh crypto state for a `num_nodes`-bucket tree under `key`.
    pub fn new(key: [u8; 16], num_nodes: usize, cfg: &OramGadgetConfig) -> Self {
        TreeCrypto {
            key,
            versions: alloc::vec![0; num_nodes],
            version_bits: cfg.version_bits,
            versioned: cfg.versioned_pads,
        }
    }

    /// The current AES pad for the tree node at (depth `d`, position `k`).
    pub fn node_pad(&self, d: usize, k: usize) -> [u8; 16] {
        let version = if self.versioned {
            self.versions[(1usize << d) - 1 + k]
        } else {
            0
        };
        let tw = if self.versioned {
            slot_tweak_versioned_bytes(d, k as u64, version, self.version_bits)
        } else {
            slot_tweak_bytes(d, 0, k as u64)
        };
        encrypt_block(&self.key, &tw)
    }

    /// Pre-format the tree for `encrypt_valid`: every slot is a dummy whose
    /// ciphertext equals its pad, so the circuit decrypts it to a zero entry.
    /// No-op when `encrypt_valid` is off (the all-zero tree reads as dummies).
    pub fn format_tree<const Z: usize, const B: usize>(
        &self,
        cfg: &OramGadgetConfig,
        tree: &mut OramTree<Z, B>,
    ) {
        if !cfg.encrypt_valid {
            return;
        }
        let eb = cfg.entry_bits();
        for d in 0..cfg.levels {
            for k in 0..(1usize << d) {
                let idx = (1usize << d) - 1 + k;
                let pad = self.node_pad(d, k);
                for zs in 0..Z {
                    let mut data = [0u8; B];
                    for i in 0..eb {
                        let gpos = zs * eb + i;
                        if (pad[gpos / 8] >> (gpos % 8)) & 1 == 1 {
                            data[i / 8] |= 1 << (i % 8);
                        }
                    }
                    tree.buckets[idx].entries[zs] = OramEntry {
                        addr: 0,
                        leaf: 0,
                        data,
                    };
                }
            }
        }
    }

    /// The current versions of the path-to-`leaf` nodes (root-to-leaf level
    /// order), laid out as the access circuit's version input expects.
    pub fn path_versions<const Z: usize, const B: usize>(
        &self,
        tree: &OramTree<Z, B>,
        leaf: u64,
    ) -> Vec<u64> {
        tree.path_indices(leaf)
            .iter()
            .map(|&i| self.versions[i])
            .collect()
    }

    /// Bump the versions of the path-to-`leaf` nodes after a write to that path.
    pub fn bump_path<const Z: usize, const B: usize>(&mut self, tree: &OramTree<Z, B>, leaf: u64) {
        if self.versioned {
            for &i in &tree.path_indices(leaf) {
                self.versions[i] += 1;
            }
        }
    }

    /// The tree key as circuit input bits (LSB-first per byte).
    pub fn key_bits(&self) -> Vec<bool> {
        self.key
            .iter()
            .flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1))
            .collect()
    }
}

/// Build a **sub-block extraction** circuit (S5c recursion): select entry `off`
/// from a block of `c` entries (each `eb` bits, LSB-first). Params:
/// `block[0..c*eb] ++ off[0..off_bits]`. Output: the selected entry (`eb` bits).
///
/// This is the recursive position map's entry extractor. A posmap block packs `c`
/// addresses' leaves, so it cannot be revealed wholesale; extraction runs
/// **in-circuit on the garbled block** and only the selected entry — a physical
/// leaf, safe to reveal — is output, while the other entries stay garbled.
pub fn build_extract(c: usize, eb: usize) -> BIrBlocks {
    let off_bits = clog2(c);
    let mut b = Builder::new((c * eb + off_bits) as u32);
    let block: Vec<u32> = (0..c * eb).map(|i| i as u32).collect();
    let off: Vec<u32> = (0..off_bits).map(|i| (c * eb + i) as u32).collect();
    let mut entry = Vec::new();
    for j in 0..eb {
        let mut sel = b.const0();
        for i in 0..c {
            let eq = b.eq_const(&off, i as u64);
            let t = b.and(eq, block[i * eb + j]);
            sel = b.xor(sel, t); // exactly one eq is 1, so XOR-folds to OR
        }
        entry.push(sel);
    }
    b.finish(entry)
}

/// Build a **sub-block update** circuit (S5c recursion): replace entry `off` in a
/// block of `c` entries with `new_entry`. Params: `block[0..c*eb] ++
/// off[0..off_bits] ++ new_entry[0..eb]`. Output: the new block (`c*eb` bits).
///
/// With [`build_extract`] this is the recursive posmap's read-modify-write: read
/// the block (extract), replace the accessed entry's leaf (update), write back —
/// all on garbled data so only the accessed leaf is ever revealed.
pub fn build_update(c: usize, eb: usize) -> BIrBlocks {
    let off_bits = clog2(c);
    let mut b = Builder::new((c * eb + off_bits + eb) as u32);
    let block: Vec<u32> = (0..c * eb).map(|i| i as u32).collect();
    let off: Vec<u32> = (0..off_bits).map(|i| (c * eb + i) as u32).collect();
    let new_entry: Vec<u32> = (0..eb).map(|i| (c * eb + off_bits + i) as u32).collect();
    let mut new_block = Vec::new();
    for i in 0..c {
        let eq = b.eq_const(&off, i as u64);
        for j in 0..eb {
            new_block.push(b.mux(eq, new_entry[j], block[i * eb + j]));
        }
    }
    b.finish(new_block)
}

/// Remap a boolean gate's operand var ids through `remap` (used by
/// [`Builder::inline_sub`]; only the pure-boolean variants an inlined gadget
/// uses are supported).
fn remap_bir_stmt(s: &BIrStmt, remap: &[u32]) -> BIrStmt {
    let r = |v: &IRVarId| IRVarId(remap[v.0 as usize]);
    match s {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(r(a), r(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(r(a), r(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(r(a), r(b)),
        BIrStmt::Not(a) => BIrStmt::Not(r(a)),
        other => panic!("inline_sub: unsupported stmt {other:?}"),
    }
}

/// Build the **begin** circuit: `posmap.update(addr, new_leaf) -> old_leaf`.
///
/// Inputs:  `[posmap: num_addrs×leaf_bits, addr: addr_bits, new_leaf: leaf_bits]`
/// Outputs: `[old_leaf: leaf_bits, posmap': num_addrs×leaf_bits]`
pub fn build_begin(cfg: &OramGadgetConfig) -> BIrBlocks {
    let (ab, lb, num_addrs) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.num_addrs);
    let mut b = Builder::new(cfg.begin_params() as u32);

    let mut posmap: Vec<Vec<u32>> = (0..num_addrs)
        .map(|i| (0..lb).map(|j| (i * lb + j) as u32).collect())
        .collect();
    let addr: Vec<u32> = (0..ab).map(|j| (num_addrs * lb + j) as u32).collect();
    let tail_off = num_addrs * lb + ab;
    let zc = b.const0();

    // The fresh leaf: either a `new_leaf` input, or computed in-circuit as
    // `AES-128(leaf_key, counter)[0..lb]` (S6 keyed-leaf PRF).
    let new_leaf: Vec<u32> = if cfg.keyed_leaf {
        let leaf_key: Vec<u32> = (0..128).map(|j| (tail_off + j) as u32).collect();
        let counter: Vec<u32> = (0..64).map(|j| (tail_off + 128 + j) as u32).collect();
        let mut tweak = counter;
        tweak.extend(vec![zc; 64]); // zero-pad the 64-bit counter to a 128-bit block
        let aes = crate::aes_gadget::build_aes128();
        let mut aes_in = leaf_key;
        aes_in.extend(tweak);
        let aes_out = b.inline_sub(&aes, &aes_in);
        aes_out[..lb].to_vec()
    } else {
        (0..lb).map(|j| (tail_off + j) as u32).collect()
    };

    let mut old_leaf: Vec<u32> = vec![zc; lb];
    for (i, cell) in posmap.iter_mut().enumerate() {
        let eq = b.eq_const(&addr, i as u64);
        let new_old = b.mux_word(eq, cell, &old_leaf);
        old_leaf = new_old;
        let new_cell = b.mux_word(eq, &new_leaf, cell);
        *cell = new_cell;
    }

    let mut out = old_leaf;
    if cfg.keyed_leaf {
        // Also output the in-circuit-derived new_leaf so the driver threads it
        // (garbled) to the access circuit rather than recomputing the PRF.
        out.extend_from_slice(&new_leaf);
    }
    for cell in &posmap {
        out.extend_from_slice(cell);
    }
    b.finish(out)
}

/// Build one split-key tree-formatting circuit for physical node
/// `depth`/`prefix`. It emits the encrypted representation of an all-zero
/// bucket, i.e. dummy entries. The 128 AES key inputs are deliberately the
/// only inputs, so roles can partition them 64/64 without materializing a
/// complete key. This is required before `encrypt_valid` paths can be read.
///
/// The output is `Z * entry_bits` ciphertext bits in slot order. With
/// versioned pads the supplied `version` is incorporated into the same tweak
/// layout as [`build_access`].
/// A directionally-owned durable material block operation.
///
/// The AES-XOR arithmetic is involutory, but its input owner and output
/// recipient are protocol facts. Naming them here prevents a storage adapter
/// from accidentally using the evaluator-ciphertext shape to save a
/// garbler-only false-label base, or vice versa.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MaterialBlockDirection {
    /// Garbler-private false-label base -> evaluator-private ciphertext.
    SealGarbler,
    /// Evaluator-private ciphertext -> garbler-private false-label base.
    OpenGarbler,
    /// Evaluator-private active label -> evaluator-private ciphertext.
    SealEvaluator,
    /// Evaluator-private ciphertext -> evaluator-private active label.
    OpenEvaluator,
}

/// Shared arithmetic for every [`MaterialBlockDirection`].
///
/// Inputs are `[key: 128, tweak: 128, material: 128]`; output is
/// `material XOR AES-128(key, tweak)`. Ownership is deliberately represented
/// by the direction-specific constructors below, rather than by the circuit
/// bits themselves: this preserves the one circuit identity while making the
/// split runner's input/output partition auditable at each call site.
fn build_material_block_xor() -> BIrBlocks {
    let mut b = Builder::new(384);
    let key: Vec<u32> = (0..128).collect();
    let tweak: Vec<u32> = (128..256).collect();
    let material: Vec<u32> = (256..384).collect();
    let aes = crate::aes_gadget::build_aes128();
    let mut aes_in = key;
    aes_in.extend(tweak);
    let pad = b.inline_sub(&aes, &aes_in);
    let output = b.xor_word(&material, &pad);
    b.finish(output)
}

/// Build `blocks` material AES-XOR operations under one 128-bit split key.
///
/// Input layout is `[key:128 | tweaks: blocks×128 | materials: blocks×128]`;
/// outputs are the encrypted/decrypted blocks in block order. This is an
/// encapsulation candidate: one split invocation amortizes framing, OT setup,
/// and the role-local transaction boundary across `blocks` packed values.
/// It deliberately does **not** claim AES-key-schedule sharing yet: each
/// inlined AES instance remains independent, so AND cost still scales linearly
/// with `blocks`. The sizing probe makes that limitation visible.
pub fn build_material_block_cipher_n(blocks: usize) -> BIrBlocks {
    assert!(blocks > 0, "material batch needs at least one block");
    let params = 128 + 2 * blocks * 128;
    let mut b = Builder::new(params as u32);
    let mut aes_input: Vec<u32> = (0..128).collect();
    for block in 0..blocks {
        let tweak_start = 128 + block * 128;
        aes_input.extend((tweak_start..tweak_start + 128).map(|wire| wire as u32));
    }
    // Shared-key schedule: the AES gadget expands the 128-bit split key once,
    // then encrypts all public tweaks under those round keys.
    let pads = b.inline_sub(&crate::aes_gadget::build_aes128_multi(blocks), &aes_input);
    let mut output = Vec::with_capacity(blocks * 128);
    for block in 0..blocks {
        let material_start = 128 + blocks * 128 + block * 128;
        let material: Vec<u32> = (material_start..material_start + 128)
            .map(|wire| wire as u32)
            .collect();
        output.extend(b.xor_word(&material, &pads[block * 128..(block + 1) * 128]));
    }
    b.finish(output)
}

/// One fixed-shape multi-block loop step.
///
/// Input layout: `[key:128 | tweak:128 | material:128 | remaining: counter_bits]`.
/// Output layout: `[material':128 | next_remaining:counter_bits | done:1]`.
/// `done` is true exactly for `remaining == 1`; a driver reveals it and feeds
/// `next_remaining` as held state into the following invocation. This is the
/// CFG-style alternative to fully unrolling an `n`-block material gadget.
pub fn build_material_block_loop_step(counter_bits: usize) -> BIrBlocks {
    assert!(
        counter_bits > 0 && counter_bits <= 64,
        "bounded public loop counter"
    );
    let mut b = Builder::new((384 + counter_bits) as u32);
    let key: Vec<u32> = (0..128).collect();
    let tweak: Vec<u32> = (128..256).collect();
    let material: Vec<u32> = (256..384).collect();
    let remaining: Vec<u32> = (384..384 + counter_bits as u32).collect();
    let aes = crate::aes_gadget::build_aes128();
    let mut aes_input = key;
    aes_input.extend(tweak);
    let pad = b.inline_sub(&aes, &aes_input);
    let mut output = b.xor_word(&material, &pad);
    output.extend(b.decr(&remaining));
    output.push(b.eq_const(&remaining, 1));
    b.finish(output)
}

/// Legacy/general material block arithmetic constructor. New protocol code
/// should use a direction-specific constructor below.
pub fn build_material_block_cipher() -> BIrBlocks {
    build_material_block_xor()
}

/// Build the garbler-base sealing circuit. Its final 128 input bits must be
/// `SplitInput::Garbler` and its output must be `EvaluatorReveal`.
pub fn build_material_seal_garbler_block() -> BIrBlocks {
    build_material_block_xor()
}

/// Build the garbler-base opening circuit. Its final 128 input bits must be
/// `SplitInput::Evaluator` and its output must be `GarblerReveal`.
pub fn build_material_open_garbler_block() -> BIrBlocks {
    build_material_block_xor()
}

/// Build evaluator-label sealing. Its material input and ciphertext output are
/// evaluator-private (`Evaluator` / `EvaluatorReveal`).
pub fn build_material_seal_evaluator_block() -> BIrBlocks {
    build_material_block_xor()
}

/// Build evaluator-label opening. Its ciphertext input and label output are
/// evaluator-private (`Evaluator` / `EvaluatorReveal`).
pub fn build_material_open_evaluator_block() -> BIrBlocks {
    build_material_block_xor()
}

/// Public 128-bit tweak for a role-local durable material block.
///
/// The fields are deliberately explicit: different roles use different
/// `region`s, while `slot`, monotonically increasing `version`, and `block`
/// make substitution/replay across transactions decrypt to an invalid label.
pub fn material_block_tweak(region: u8, slot: u64, version: u64, block: u32) -> [u8; 16] {
    material_block_tweak_checked(region, slot, version, block)
        .expect("durable material slot/version/block exceeds fixed tweak encoding")
}

/// Checked form of [`material_block_tweak`]. The fixed format has 48-bit slot
/// and version fields plus a 16-bit block field; rejecting larger values avoids
/// turning distinct durable transactions into one AES pad.
pub fn material_block_tweak_checked(
    region: u8,
    slot: u64,
    version: u64,
    block: u32,
) -> Option<[u8; 16]> {
    if slot > 0xFFFF_FFFF_FFFF || version > 0xFFFF_FFFF_FFFF || block > u16::MAX as u32 {
        return None;
    }
    let mut tweak = [0u8; 16];
    tweak[0] = 0xD4; // material-store domain separator
    tweak[1] = region;
    tweak[2..4].copy_from_slice(&(block as u16).to_le_bytes());
    // The bounded durable-store interface uses 48-bit public slot and version
    // counters. Refuse wider values at the adapter seam rather than folding
    // them, which could turn distinct transactions into one pad.
    tweak[4..10].copy_from_slice(&slot.to_le_bytes()[..6]);
    tweak[10..16].copy_from_slice(&version.to_le_bytes()[..6]);
    Some(tweak)
}

pub fn build_tree_node_formatter(
    cfg: &OramGadgetConfig,
    depth: usize,
    prefix: u64,
    version: u64,
) -> BIrBlocks {
    assert!(
        cfg.encrypted && cfg.encrypt_valid,
        "formatter needs encrypted valid bits"
    );
    assert_eq!(cfg.tree_key_bits, 128, "formatter uses AES-128");
    assert!(depth < cfg.levels, "node depth in tree");
    let eb = cfg.entry_bits();
    let mut b = Builder::new(128);
    let zc = b.const0();
    let oc = b.const1();
    let key: Vec<u32> = (0..128).collect();
    let mut tweak = vec![zc; 128];
    for bit in 0..8 {
        if (depth >> bit) & 1 != 0 {
            tweak[bit] = oc;
        }
    }
    for bit in 0..depth {
        if (prefix >> (depth - 1 - bit)) & 1 != 0 {
            tweak[16 + bit] = oc;
        }
    }
    if cfg.versioned_pads {
        for bit in 0..cfg.version_bits {
            if (version >> bit) & 1 != 0 {
                tweak[32 + bit] = oc;
            }
        }
    }
    let aes = crate::aes_gadget::build_aes128();
    let mut aes_in = key;
    aes_in.extend(tweak);
    let pad = b.inline_sub(&aes, &aes_in);
    assert!(
        cfg.bucket_size * eb <= 128,
        "one AES block covers formatted bucket"
    );
    b.finish(pad[..cfg.bucket_size * eb].to_vec())
}

/// Build the **access** circuit: absorb a path, optionally select/read/write
/// the target block, then evict along the path. Used for both the main access
/// (`evict_only = 0`) and the post-access eviction (`evict_only = 1`).
///
/// Inputs:  `[stash: max_stash×eb, path: (levels·Z)×eb, addr: ab, op_write: 1,
///           wdata: data_bits, path_leaf: lb, new_leaf: lb, evict_only: 1]`
/// Outputs: `[overflow: 1, rdata: data_bits, new_path: (levels·Z)×eb,
///           stash': max_stash×eb]`
pub fn build_access(cfg: &OramGadgetConfig) -> BIrBlocks {
    let (ab, lb, eb, db) = (
        cfg.addr_bits(),
        cfg.leaf_bits(),
        cfg.entry_bits(),
        cfg.data_bits,
    );
    let (z, levels, ms) = (cfg.bucket_size, cfg.levels, cfg.max_stash);
    let n_path = cfg.path_entries();

    // Input layout offsets.
    let stash_off = 0usize;
    let path_off = stash_off + ms * eb;
    let addr_off = path_off + n_path * eb;
    let op_off = addr_off + ab;
    let wdata_off = op_off + 1;
    let pleaf_off = wdata_off + db;
    let nleaf_off = pleaf_off + lb;
    let evonly_off = nleaf_off + lb;

    let mut b = Builder::new(cfg.access_params() as u32);

    let mut stash: Vec<Vec<u32>> = (0..ms)
        .map(|i| (0..eb).map(|j| (stash_off + i * eb + j) as u32).collect())
        .collect();
    let mut path: Vec<Vec<u32>> = (0..n_path)
        .map(|k| (0..eb).map(|j| (path_off + k * eb + j) as u32).collect())
        .collect();
    let addr: Vec<u32> = (0..ab).map(|j| (addr_off + j) as u32).collect();
    let op_write = op_off as u32;
    let wdata: Vec<u32> = (0..db).map(|j| (wdata_off + j) as u32).collect();
    let path_leaf: Vec<u32> = (0..lb).map(|j| (pleaf_off + j) as u32).collect();
    let new_leaf: Vec<u32> = (0..lb).map(|j| (nleaf_off + j) as u32).collect();
    let evict_only = evonly_off as u32;
    let tree_key: Vec<u32> = (0..cfg.tree_key_bits)
        .map(|j| (evonly_off + 1 + j) as u32)
        .collect();
    // Versioned pads: one public version word per path node, after the tree_key.
    let ver_off = evonly_off + 1 + cfg.tree_key_bits;
    let versions: Vec<Vec<u32>> = (0..cfg.levels)
        .map(|d| {
            (0..cfg.version_bits)
                .map(|j| (ver_off + d * cfg.version_bits + j) as u32)
                .collect()
        })
        .collect();

    let zc = b.const0();
    let oc = b.const1();
    let not_evict_only = b.not(evict_only);
    let mut overflow = zc;

    // Encrypted tree: precompute each path slot's pad, then decrypt the path the
    // host just read (the tree stores ciphertext). The same pad re-encrypts the
    // slot on write-back below, so a physical node has one consistent pad.
    //
    // The pad is `AES-128(tree_key, slot_tweak)` — a real PRF keyed on the
    // garbler-held `tree_key`, so the tree-hosting evaluator cannot recover
    // block tags from the ciphertext. One AES instance is inlined per path slot.
    //
    // Only the payload `[addr, leaf, data]` is encrypted; the `valid` bit stays
    // plaintext so an untouched (all-zero) tree slot reads back as a dummy
    // without any garbler-side formatting pass. (The evaluator therefore sees
    // the tree *occupancy* pattern — a partial leak hardened in S6 — but not the
    // address tags, leaf assignments, or data.)
    let mut pads: Vec<Vec<u32>> = Vec::new();
    let mut repads: Vec<Vec<u32>> = Vec::new();
    if cfg.encrypted {
        assert_eq!(cfg.tree_key_bits, 128, "encrypted tree uses an AES-128 key");
        // One AES per path *node* (levels of them, not levels*Z): the node's
        // 128-bit AES output is sliced across its Z slots. The tweak names the
        // node (depth + path_leaf prefix), not the slot, so all Z slots share
        // one evaluation. Cost: levels AES per access instead of levels*Z.
        //
        // Pad width `pw`: normally `eb-1` (payload only, `valid` plaintext so a
        // zero tree reads as dummies); with `encrypt_valid` it is `eb` (the whole
        // entry, hiding occupancy — but then the initial tree must be
        // pre-formatted by the key holder; see slot_tweak_bytes).
        let pw = if cfg.encrypt_valid { eb } else { eb - 1 };
        assert!(z * pw <= 128, "one AES block covers a node's slots' pads");
        let aes = crate::aes_gadget::build_aes128();
        for d in 0..cfg.levels {
            // zslot = 0: the tweak is node-unique; the slot index selects the
            // output slice instead. With versioned_pads the per-node version is
            // mixed into the tweak (decrypt with the current version).
            let dt = if cfg.versioned_pads {
                b.slot_tweak_versioned(&path_leaf, d, &versions[d], zc, oc)
            } else {
                b.slot_tweak(&path_leaf, d, 0, zc, oc)
            };
            let mut din = tree_key.clone();
            din.extend_from_slice(&dt);
            let dout = b.inline_sub(&aes, &din);
            for zs in 0..z {
                pads.push(dout[zs * pw..(zs + 1) * pw].to_vec());
            }
            // Re-encrypt pad: version+1 when versioned (this write bumps the
            // node's version); otherwise identical to the decrypt pad.
            if cfg.versioned_pads {
                let v1 = b.incr(&versions[d]);
                let rt = b.slot_tweak_versioned(&path_leaf, d, &v1, zc, oc);
                let mut rin = tree_key.clone();
                rin.extend_from_slice(&rt);
                let rout = b.inline_sub(&aes, &rin);
                for zs in 0..z {
                    repads.push(rout[zs * pw..(zs + 1) * pw].to_vec());
                }
            }
        }
        if !cfg.versioned_pads {
            repads = pads.clone();
        }
        for k in 0..n_path {
            path[k] = if cfg.encrypt_valid {
                // Whole-entry decrypt (valid included).
                b.xor_word(&path[k][..eb], &pads[k])
            } else {
                let mut e = vec![path[k][0]]; // valid stays plaintext
                e.extend(b.xor_word(&path[k][1..], &pads[k]));
                e
            };
        }
    }

    // 1. Absorb every real path entry into the first free stash slot.
    for pe in &path {
        let pe_valid = pe[off_valid()];
        let mut placed = zc;
        for slot in stash.iter_mut() {
            let slot_free = b.not(slot[off_valid()]);
            let not_placed = b.not(placed);
            let place = b.and(pe_valid, slot_free);
            let place = b.and(place, not_placed);
            let updated = b.mux_word(place, pe, slot);
            *slot = updated;
            placed = b.or(placed, place);
        }
        let not_placed = b.not(placed);
        let dropped = b.and(pe_valid, not_placed);
        overflow = b.or(overflow, dropped);
    }

    // 2. Select / read / write the target block (skipped when evict_only).
    let mut rdata: Vec<u32> = vec![zc; db];
    let mut found = zc;
    for s in 0..ms {
        let e = stash[s].clone();
        let eq = b.eq_bits(&e[off_addr()..off_leaf(cfg)], &addr);
        let nf = b.not(found);
        let m0 = b.and(e[off_valid()], eq);
        let m1 = b.and(m0, nf);
        let m = b.and(m1, not_evict_only);
        // read
        rdata = b.mux_word(m, &e[off_data(cfg)..eb], &rdata);
        // write
        let mw = b.and(m, op_write);
        let new_data = b.mux_word(mw, &wdata, &e[off_data(cfg)..eb]);
        // retag leaf to new_leaf
        let new_lf = b.mux_word(m, &new_leaf, &e[off_leaf(cfg)..off_data(cfg)]);
        let mut e2 = e.clone();
        e2[off_leaf(cfg)..off_data(cfg)].copy_from_slice(&new_lf);
        e2[off_data(cfg)..eb].copy_from_slice(&new_data);
        stash[s] = e2;
        found = b.or(found, m);
    }

    // 2b. First touch of this address: create the block in a free slot.
    let nf = b.not(found);
    let need_create = b.and(nf, not_evict_only);
    let create_data = b.mux_word(op_write, &wdata, &vec![zc; db]);
    let mut centry: Vec<u32> = Vec::with_capacity(eb);
    centry.push(oc);
    centry.extend_from_slice(&addr);
    centry.extend_from_slice(&new_leaf);
    centry.extend_from_slice(&create_data);
    let mut placed = zc;
    for slot in stash.iter_mut() {
        let slot_free = b.not(slot[off_valid()]);
        let not_placed = b.not(placed);
        let place0 = b.and(need_create, slot_free);
        let place = b.and(place0, not_placed);
        let updated = b.mux_word(place, &centry, slot);
        *slot = updated;
        placed = b.or(placed, place);
    }
    let not_placed = b.not(placed);
    let cdropped = b.and(need_create, not_placed);
    overflow = b.or(overflow, cdropped);

    // 3. Evict along `path_leaf`, deepest level first, filling each bucket.
    let mut placed_in: Vec<u32> = vec![zc; ms];
    let mut new_path: Vec<Vec<u32>> = (0..n_path).map(|_| vec![zc; eb]).collect();
    for level in (0..levels).rev() {
        for slot in 0..z {
            let bucket = level * z + slot;
            let mut found_slot = zc;
            for i in 0..ms {
                let e = stash[i].clone();
                let peq = b.prefix_eq(&e[off_leaf(cfg)..off_data(cfg)], &path_leaf, level);
                let nplaced = b.not(placed_in[i]);
                let e0 = b.and(e[off_valid()], nplaced);
                let e1 = b.and(e0, peq);
                let nfs = b.not(found_slot);
                let elig = b.and(e1, nfs);
                let cur = new_path[bucket].clone();
                let updated = b.mux_word(elig, &e, &cur);
                new_path[bucket] = updated;
                placed_in[i] = b.or(placed_in[i], elig);
                found_slot = b.or(found_slot, elig);
            }
        }
    }

    // 4. Whatever was not evicted stays in the stash (clear the placed bits).
    for i in 0..ms {
        let e = stash[i].clone();
        let nplaced = b.not(placed_in[i]);
        let new_valid = b.and(e[off_valid()], nplaced);
        let mut e2 = e;
        e2[off_valid()] = new_valid;
        stash[i] = e2;
    }

    // Encrypted tree: re-encrypt the evicted path before the host writes it
    // back. With versioned_pads this uses the bumped-version pads (`repads`), so
    // a stale ciphertext under the old version no longer decrypts correctly.
    if cfg.encrypted {
        for k in 0..n_path {
            new_path[k] = if cfg.encrypt_valid {
                b.xor_word(&new_path[k][..eb], &repads[k])
            } else {
                let mut e = vec![new_path[k][0]];
                e.extend(b.xor_word(&new_path[k][1..], &repads[k]));
                e
            };
        }
    }

    // Outputs: overflow, rdata, new_path, stash'.
    let mut out = vec![overflow];
    out.extend_from_slice(&rdata);
    for e in &new_path {
        out.extend_from_slice(e);
    }
    for e in &stash {
        out.extend_from_slice(e);
    }
    b.finish(out)
}
