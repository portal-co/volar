// @reliability: experimental
// @ai: assisted
//! S5c: **recursive position map** — the main ORAM's position map is itself
//! stored in a smaller ORAM, so the main access does *not* linear-scan an
//! O(num_addrs) posmap. Instead, resolving `posmap_0[addr]` is a level-1 ORAM
//! access; level 1's own (tiny) posmap is a plain in-circuit linear scan.
//!
//! This is the scaling piece that bounds the begin circuit: a flat ORAM's begin
//! is O(num_addrs) muxes; here the main access's leaf comes from a level-1
//! ORAM lookup (polylog), and only level 1's small posmap is linear-scanned.
//!
//! Demonstrated **concretely** (`eval_biir`), with the harness decoding the
//! intermediate posmap entries — the garbled-intermediate (two-party) version
//! composes the same nested accesses with S4-style label threading. Correctness
//! is checked against the flat reference `oram_access_local` on the main ORAM.
//!
//! Geometry: level 0 = 16 addresses of 1-byte data; c = 4 posmap entries per
//! level-1 block, so level 1 = 4 blocks of 16-bit posmap data (4 leaf values),
//! with level 1's own 4-entry posmap linear-scanned.

use volar_ir::boolar::BIrBlocks;
use volar_oram::{AccessOp, AccessResult, Bucket, OramClient, OramEntry, OramTree, eviction_target, oram_access_local};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};

// Level 0 (main data ORAM).
const N0: usize = 16;
const LEVELS0: usize = 5; // num_leaves_0 = 16
const Z0: usize = 2;
const DATA0: usize = 8; // 1-byte user data
// Level 1 (posmap ORAM): c entries of leaf_bits_0 per block.
const C: usize = 4;
const LEAF_BITS0: usize = 4; // log2(num_leaves_0) = log2(16)
const N1: usize = N0 / C; // 4
const LEVELS1: usize = 3; // num_leaves_1 = 4
const Z1: usize = 2;
const DATA1: usize = C * LEAF_BITS0; // 16 bits per posmap block

fn cfg(num_addrs: usize, levels: usize, z: usize, data_bits: usize) -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs,
        levels,
        bucket_size: z,
        data_bits,
        max_stash: 2 * levels + z + num_addrs,
        encrypted: false,
        tree_key_bits: 0,
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

fn enc_entry<const B: usize>(e: &OramEntry<B>, cfg: &OramGadgetConfig) -> Vec<bool> {
    let real = e.is_real();
    let mut v = vec![real];
    v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
    v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
    let data = e.data.iter().take(B).fold(0u64, |a, &b| (a << 8) | b as u64);
    v.extend(enc(if real { data } else { 0 }, cfg.data_bits));
    v
}
fn dec_entry<const B: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> OramEntry<B> {
    let (ab, lb, db) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.data_bits);
    if !bits[0] {
        return OramEntry::dummy();
    }
    let addr = dec(&bits[1..1 + ab]);
    let leaf = dec(&bits[1 + ab..1 + ab + lb]);
    let data = dec(&bits[1 + ab + lb..1 + ab + lb + db]);
    let mut bytes = [0u8; B];
    for (i, byte) in bytes.iter_mut().enumerate() {
        *byte = (data >> (8 * (B - 1 - i))) as u8;
    }
    OramEntry { addr, leaf, data: bytes }
}
fn flatten_path<const Z: usize, const B: usize>(path: &[Bucket<Z, B>], cfg: &OramGadgetConfig) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            v.extend(enc_entry(e, cfg));
        }
    }
    v
}
fn unflatten_path<const Z: usize, const B: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, B>> {
    let eb = cfg.entry_bits();
    (0..cfg.levels)
        .map(|level| Bucket {
            entries: core::array::from_fn(|s| dec_entry(&bits[(level * Z + s) * eb..(level * Z + s + 1) * eb], cfg)),
        })
        .collect()
}

struct Splitmix(u64);
impl Splitmix {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
}

/// One ORAM level: gadget circuits + threaded concrete state.
struct Level<const Z: usize, const BT: usize> {
    cfg: OramGadgetConfig,
    begin: Option<BIrBlocks>, // None => leaf supplied externally (level 0)
    access: BIrBlocks,
    posmap_bits: Vec<bool>,   // empty when begin is None
    stash_bits: Vec<bool>,
    tree: OramTree<Z, BT>,
    counter: u64,
    leaf_rng: Splitmix,
}

impl<const Z: usize, const BT: usize> Level<Z, BT> {
    /// A flat level with its own linear-scan posmap begin circuit.
    fn flat(cfg: OramGadgetConfig, seed: u64) -> Self {
        let begin = build_begin(&cfg);
        let access = build_access(&cfg);
        let lb = cfg.leaf_bits();
        let eb = cfg.entry_bits();
        let posmap_bits: Vec<bool> = (0..cfg.num_addrs).flat_map(|_| enc(0, lb)).collect();
        let stash_bits = vec![false; cfg.max_stash * eb];
        Level {
            cfg,
            begin: Some(begin),
            access,
            posmap_bits,
            stash_bits,
            tree: OramTree::new(cfg.levels),
            counter: 0,
            leaf_rng: Splitmix(seed),
        }
    }
    /// A recursive level: no local posmap (it lives in the level above); the
    /// leaf is resolved and supplied externally.
    fn recursive(cfg: OramGadgetConfig, seed: u64) -> Self {
        let access = build_access(&cfg);
        let eb = cfg.entry_bits();
        Level {
            cfg,
            begin: None,
            access,
            posmap_bits: Vec::new(),
            stash_bits: vec![false; cfg.max_stash * eb],
            tree: OramTree::new(cfg.levels),
            counter: 0,
            leaf_rng: Splitmix(seed),
        }
    }
    fn fresh_leaf(&mut self) -> u64 {
        self.leaf_rng.next() % self.cfg.num_leaves() as u64
    }

    /// Run the access circuit (no posmap update): absorb the path at
    /// `old_leaf`, select/read/write `addr`, evict, write back. Returns rdata.
    fn run_access(&mut self, addr: u64, op_write: bool, wdata: u64, old_leaf: u64, new_leaf: u64) -> u64 {
        let (ab, lb, db, eb) = (
            self.cfg.addr_bits(),
            self.cfg.leaf_bits(),
            self.cfg.data_bits,
            self.cfg.entry_bits(),
        );
        let n_path = self.cfg.path_entries();
        let num_leaves = self.cfg.num_leaves() as u64;

        let main_path = self.tree.read_path(old_leaf);
        let mut acc_in = self.stash_bits.clone();
        acc_in.extend(flatten_path::<Z, BT>(&main_path, &self.cfg));
        acc_in.extend(enc(addr, ab));
        acc_in.push(op_write);
        acc_in.extend(enc(wdata, db));
        acc_in.extend(enc(old_leaf, lb));
        acc_in.extend(enc(new_leaf, lb));
        acc_in.push(false);
        assert_eq!(acc_in.len(), self.cfg.access_params());
        let acc_out = volar_fuzz::interpreter::biir::eval_biir(&self.access, &acc_in).expect("access evals");
        assert!(!acc_out[0], "ORAM stash overflow");
        let rdata = dec(&acc_out[1..1 + db]);
        let new_path_bits = &acc_out[1 + db..1 + db + n_path * eb];
        self.stash_bits = acc_out[1 + db + n_path * eb..].to_vec();
        self.tree.write_path(old_leaf, &unflatten_path::<Z, BT>(new_path_bits, &self.cfg));

        // Deterministic eviction (one pass, skipped on collision).
        let evict_leaf = eviction_target(self.counter, num_leaves);
        self.counter += 1;
        if evict_leaf != old_leaf {
            let epath = self.tree.read_path(evict_leaf);
            let mut ev_in = self.stash_bits.clone();
            ev_in.extend(flatten_path::<Z, BT>(&epath, &self.cfg));
            ev_in.extend(enc(0, ab));
            ev_in.push(false);
            ev_in.extend(enc(0, db));
            ev_in.extend(enc(evict_leaf, lb));
            ev_in.extend(enc(0, lb));
            ev_in.push(true);
            let ev_out = volar_fuzz::interpreter::biir::eval_biir(&self.access, &ev_in).expect("evict evals");
            assert!(!ev_out[0], "ORAM stash overflow (evict)");
            let new_epath_bits = &ev_out[1 + db..1 + db + n_path * eb];
            self.stash_bits = ev_out[1 + db + n_path * eb..].to_vec();
            self.tree.write_path(evict_leaf, &unflatten_path::<Z, BT>(new_epath_bits, &self.cfg));
        }
        rdata
    }

    /// A flat-ORAM access: linear-scan begin resolves+updates the leaf, then
    /// the access circuit.
    fn access(&mut self, addr: u64, op_write: bool, wdata: u64, new_leaf: u64) -> u64 {
        let begin = self.begin.as_ref().expect("flat level has a begin").clone();
        let lb = self.cfg.leaf_bits();
        let ab = self.cfg.addr_bits();
        let mut begin_in = self.posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let begin_out = volar_fuzz::interpreter::biir::eval_biir(&begin, &begin_in).expect("begin evals");
        let old_leaf = dec(&begin_out[..lb]);
        self.posmap_bits = begin_out[lb..].to_vec();
        self.run_access(addr, op_write, wdata, old_leaf, new_leaf)
    }
}

/// A 2-level recursive ORAM: level 0 (data) + level 1 (posmap). Level 0's
/// posmap is stored as level 1's data; level 1's own posmap is a linear scan.
struct RecursiveOram {
    l0: Level<Z0, 1>, // data blocks (B0 = 1 byte)
    l1: Level<Z1, 2>, // posmap blocks (B1 = 2 bytes = 16 bits = C leaf values)
}

impl RecursiveOram {
    fn new() -> Self {
        RecursiveOram {
            l0: Level::recursive(cfg(N0, LEVELS0, Z0, DATA0), 0x1111),
            l1: Level::flat(cfg(N1, LEVELS1, Z1, DATA1), 0x2222),
        }
    }

    fn access(&mut self, addr: u64, op_write: bool, wdata: u8) -> u8 {
        // Resolve + update posmap_0[addr], stored in level 1 block addr/C at
        // offset addr%C. Read-modify-write level 1.
        let b1 = addr / C as u64;
        let off = (addr % C as u64) as usize;
        let nl0 = self.l0.fresh_leaf();
        // Read the posmap block from level 1.
        let nl1r = self.l1.fresh_leaf();
        let block = self.l1.access(b1, false, 0, nl1r);
        // Extract leaf_0 = entry[off]; compute the updated block.
        let shift = off * LEAF_BITS0;
        let mask = ((1u64 << LEAF_BITS0) - 1) << shift;
        let leaf_0 = (block & mask) >> shift;
        let new_block = (block & !mask) | (nl0 << shift);
        // Write the updated posmap block back to level 1.
        let nl1w = self.l1.fresh_leaf();
        self.l1.access(b1, true, new_block, nl1w);
        // Main access on level 0 with the resolved leaf.
        self.l0.run_access(addr, op_write, wdata as u64, leaf_0, nl0) as u8
    }
}

#[test]
fn s5c_recursive_posmap_matches_reference() {
    let mut roram = RecursiveOram::new();
    // Reference flat ORAM over the main level geometry.
    let mut ref_client = OramClient::<Z0, 1>::new(LEVELS0, N0 as u64);
    let mut ref_tree = OramTree::<Z0, 1>::new(LEVELS0);
    let mut ref_rng = Splitmix(0x9999);
    let mut model = [0u8; N0];

    // Ops: write all 16, read all 16, then random mixed ops.
    let mut ops: Vec<(u64, Option<u8>)> = Vec::new();
    for a in 0..N0 as u64 {
        ops.push((a, Some((a * 7 + 1) as u8)));
    }
    for a in 0..N0 as u64 {
        ops.push((a, None));
    }
    let mut op_rng = Splitmix(0xabcd);
    for _ in 0..20 {
        let a = op_rng.next() % N0 as u64;
        let w = if op_rng.next() % 2 == 0 { Some(op_rng.next() as u8) } else { None };
        ops.push((a, w));
    }

    for (step, &(addr, wdata)) in ops.iter().enumerate() {
        // Reference.
        let ref_op = match wdata {
            None => AccessOp::Read,
            Some(b) => AccessOp::Write([b]),
        };
        let ref_result = oram_access_local(&mut ref_client, &mut ref_tree, addr, ref_op, &mut || ref_rng.next());

        // Recursive circuit ORAM.
        let got = roram.access(addr, wdata.is_some(), wdata.unwrap_or(0));

        match (wdata, ref_result) {
            (None, AccessResult::ReadValue(bytes)) => {
                assert_eq!(got, bytes[0], "step {step}: recursive read({addr}) != reference");
                assert_eq!(got, model[addr as usize], "step {step}: recursive read({addr}) != model");
            }
            (Some(b), AccessResult::WriteAck) => {
                model[addr as usize] = b;
            }
            other => panic!("step {step}: unexpected pairing {other:?}"),
        }
    }
}
