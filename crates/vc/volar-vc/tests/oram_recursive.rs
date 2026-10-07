// @reliability: experimental
// @ai: assisted
//! S5c: **recursive position map** — the main ORAM's position map is itself
//! stored in a smaller ORAM, recursively, so no level linear-scans an
//! O(num_addrs) posmap. Resolving `posmap_i[addr]` is a level-(i+1) ORAM
//! access; only the deepest (base) level's tiny posmap is linear-scanned.
//!
//! This bounds the begin circuit: a flat ORAM's begin is O(num_addrs) muxes;
//! the recursive form replaces it with a chain of ORAM accesses (each polylog)
//! plus a tiny base-case scan — O(log² N) instead of O(N) for large memories.
//!
//! Generalised to **configurable depth** (`RecursiveOram::new(n_addrs, c, ..)`):
//! `c` posmap entries are packed per block, and the recursion bottoms out when a
//! level's address space is small enough to linear-scan. Demonstrated
//! **concretely** (`eval_biir`), the harness decoding the intermediate posmap
//! entries; the garbled-intermediate two-party version composes the same nested
//! accesses with S4-style label threading. Checked against `oram_access_local`.

use volar_ir::boolar::BIrBlocks;
use volar_oram::{
    AccessOp, AccessResult, Bucket, OramClient, OramEntry, OramTree, eviction_target,
    oram_access_local,
};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};

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
    let data = e
        .data
        .iter()
        .take(B)
        .fold(0u64, |a, &b| (a << 8) | b as u64);
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
    OramEntry {
        addr,
        leaf,
        data: bytes,
    }
}
fn flatten_path<const Z: usize, const B: usize>(
    path: &[Bucket<Z, B>],
    cfg: &OramGadgetConfig,
) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            v.extend(enc_entry(e, cfg));
        }
    }
    v
}
fn unflatten_path<const Z: usize, const B: usize>(
    bits: &[bool],
    cfg: &OramGadgetConfig,
) -> Vec<Bucket<Z, B>> {
    let eb = cfg.entry_bits();
    (0..cfg.levels)
        .map(|level| Bucket {
            entries: core::array::from_fn(|s| {
                dec_entry(&bits[(level * Z + s) * eb..(level * Z + s + 1) * eb], cfg)
            }),
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

fn ceil_log2(x: usize) -> usize {
    let mut b = 0;
    while (1usize << b) < x.max(1) {
        b += 1;
    }
    b
}

/// One recursion level's gadget + threaded concrete state.
struct RecLevel<const Z: usize, const B: usize> {
    cfg: OramGadgetConfig,
    begin: Option<BIrBlocks>, // Some only for the base (linear-scan) level
    access: BIrBlocks,
    posmap_bits: Vec<bool>, // empty unless base
    stash_bits: Vec<bool>,
    tree: OramTree<Z, B>,
    counter: u64,
    leaf_rng: Splitmix,
    is_base: bool,
}

impl<const Z: usize, const B: usize> RecLevel<Z, B> {
    fn new(cfg: OramGadgetConfig, is_base: bool, seed: u64) -> Self {
        let eb = cfg.entry_bits();
        let lb = cfg.leaf_bits();
        let begin = if is_base {
            Some(build_begin(&cfg))
        } else {
            None
        };
        let posmap_bits = if is_base {
            (0..cfg.num_addrs).flat_map(|_| enc(0, lb)).collect()
        } else {
            Vec::new()
        };
        RecLevel {
            access: build_access(&cfg),
            cfg,
            begin,
            posmap_bits,
            stash_bits: vec![false; cfg.max_stash * eb],
            tree: OramTree::new(cfg.levels),
            counter: 0,
            leaf_rng: Splitmix(seed),
            is_base,
        }
    }
    fn fresh_leaf(&mut self) -> u64 {
        self.leaf_rng.next() % self.cfg.num_leaves() as u64
    }
    /// Linear-scan posmap begin (base level): old_leaf = posmap[addr];
    /// posmap[addr] = new_leaf. Returns old_leaf.
    fn do_begin(&mut self, addr: u64, new_leaf: u64) -> u64 {
        let begin = self.begin.as_ref().expect("base level has begin").clone();
        let lb = self.cfg.leaf_bits();
        let ab = self.cfg.addr_bits();
        let mut begin_in = self.posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let out = volar_fuzz::interpreter::biir::eval_biir(&begin, &begin_in).expect("begin evals");
        self.posmap_bits = out[lb..].to_vec();
        dec(&out[..lb])
    }
    /// Run the access circuit (no posmap update) + deterministic eviction.
    fn run_access(
        &mut self,
        addr: u64,
        op_write: bool,
        wdata: u64,
        old_leaf: u64,
        new_leaf: u64,
    ) -> u64 {
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
        acc_in.extend(flatten_path::<Z, B>(&main_path, &self.cfg));
        acc_in.extend(enc(addr, ab));
        acc_in.push(op_write);
        acc_in.extend(enc(wdata, db));
        acc_in.extend(enc(old_leaf, lb));
        acc_in.extend(enc(new_leaf, lb));
        acc_in.push(false);
        let acc_out =
            volar_fuzz::interpreter::biir::eval_biir(&self.access, &acc_in).expect("access evals");
        assert!(!acc_out[0], "ORAM stash overflow");
        let rdata = dec(&acc_out[1..1 + db]);
        let npb = &acc_out[1 + db..1 + db + n_path * eb];
        self.stash_bits = acc_out[1 + db + n_path * eb..].to_vec();
        self.tree
            .write_path(old_leaf, &unflatten_path::<Z, B>(npb, &self.cfg));

        let evict_leaf = eviction_target(self.counter, num_leaves);
        self.counter += 1;
        if evict_leaf != old_leaf {
            let epath = self.tree.read_path(evict_leaf);
            let mut ev_in = self.stash_bits.clone();
            ev_in.extend(flatten_path::<Z, B>(&epath, &self.cfg));
            ev_in.extend(enc(0, ab));
            ev_in.push(false);
            ev_in.extend(enc(0, db));
            ev_in.extend(enc(evict_leaf, lb));
            ev_in.extend(enc(0, lb));
            ev_in.push(true);
            let ev_out = volar_fuzz::interpreter::biir::eval_biir(&self.access, &ev_in)
                .expect("evict evals");
            assert!(!ev_out[0], "ORAM stash overflow (evict)");
            let nepb = &ev_out[1 + db..1 + db + n_path * eb];
            self.stash_bits = ev_out[1 + db + n_path * eb..].to_vec();
            self.tree
                .write_path(evict_leaf, &unflatten_path::<Z, B>(nepb, &self.cfg));
        }
        rdata
    }
}

/// A configurable-depth recursive ORAM. `levels[0]` is the main (user-data)
/// ORAM; `levels[i]` (i>=1) stores `levels[i-1]`'s posmap; the last level is a
/// flat ORAM whose own posmap is a linear scan.
struct RecursiveOram<const Z: usize, const B: usize> {
    levels: Vec<RecLevel<Z, B>>,
    c: usize,
}

impl<const Z: usize, const B: usize> RecursiveOram<Z, B> {
    /// `n_addrs` main addresses, `data_bits_0` user data per address, `c`
    /// posmap entries packed per block. All block payloads fit in `B` bytes.
    fn new(n_addrs: usize, data_bits_0: usize, c: usize, base_threshold: usize) -> Self {
        // Compute the level geometries bottom-up... actually top-down: level 0
        // has n_addrs; level i has ceil(n_addrs / c^i) addrs. Stop when the
        // address space is small enough to linear-scan (the base).
        let mut levels = Vec::new();
        let mut n = n_addrs;
        let mut data_bits = data_bits_0;
        let mut seed = 0x1000u64;
        loop {
            let lvl_count = ceil_log2(n) + 1;
            let cfg = OramGadgetConfig {
                num_addrs: n,
                levels: lvl_count,
                bucket_size: Z,
                data_bits,
                // Logarithmic stash bound (the reference `OramClient` bound,
                // plus a margin) — this is what makes the recursion *scale*:
                // the always-sufficient `+ num_addrs` bound would make the
                // level-0 access circuit O(num_addrs), defeating the point.
                max_stash: 2 * lvl_count + Z + 16,
                encrypted: false,
                tree_key_bits: 0,
                encrypt_valid: false,
                keyed_leaf: false,
                versioned_pads: false,
                version_bits: 0,
            };
            // The NEXT level (if any) stores this level's posmap: c leaf values
            // of this level's leaf_bits, packed.
            let next_data_bits = c * cfg.leaf_bits();
            let is_base = n <= base_threshold || next_data_bits > 8 * B;
            levels.push(RecLevel::new(cfg, is_base, seed));
            seed = seed.wrapping_mul(0x9E37_79B9).wrapping_add(1);
            if is_base {
                break;
            }
            n = n.div_ceil(c);
            data_bits = next_data_bits;
        }
        RecursiveOram { levels, c }
    }

    /// Recursive access. Returns the `data_bits_0`-wide read value.
    fn access(&mut self, lvl: usize, addr: u64, op_write: bool, wdata: u64) -> u64 {
        let new_leaf = self.levels[lvl].fresh_leaf();
        let old_leaf = if self.levels[lvl].is_base {
            self.levels[lvl].do_begin(addr, new_leaf)
        } else {
            // Resolve leaf_lvl from the level below (lvl+1 stores lvl's posmap).
            let leaf_bits = self.levels[lvl].cfg.leaf_bits();
            let c = self.c as u64;
            let b = addr / c;
            let off = (addr % c) as usize;
            let block = self.access(lvl + 1, b, false, 0);
            let shift = off * leaf_bits;
            let mask = ((1u64 << leaf_bits) - 1) << shift;
            let leaf_here = (block & mask) >> shift;
            let new_block = (block & !mask) | (new_leaf << shift);
            self.access(lvl + 1, b, true, new_block);
            leaf_here
        };
        self.levels[lvl].run_access(addr, op_write, wdata, old_leaf, new_leaf)
    }
}

/// Drive a recursive ORAM over an op sequence, checking against the flat
/// reference on the main level geometry.
fn check_recursive<const Z: usize, const B: usize>(
    n_addrs: usize,
    data_bits_0: usize,
    c: usize,
    base_threshold: usize,
    ops: &[(u64, Option<u64>)],
    ref_seed: u64,
) {
    let mut roram = RecursiveOram::<Z, B>::new(n_addrs, data_bits_0, c, base_threshold);
    let levels0 = roram.levels[0].cfg.levels;
    assert!(data_bits_0 <= 64);
    let nbytes0 = data_bits_0.div_ceil(8);
    // Reference flat ORAM over the main level geometry.
    let mut ref_client = OramClient::<Z, 8>::new(levels0, n_addrs as u64);
    let mut ref_tree = OramTree::<Z, 8>::new(levels0);
    let mut ref_rng = Splitmix(ref_seed);
    let mut model = vec![0u64; n_addrs];

    for (step, &(addr, wdata)) in ops.iter().enumerate() {
        let mut bytes8 = [0u8; 8];
        if let Some(d) = wdata {
            bytes8[..nbytes0].copy_from_slice(&d.to_le_bytes()[..nbytes0]);
        }
        let ref_op = match wdata {
            None => AccessOp::Read,
            Some(_) => AccessOp::Write(bytes8),
        };
        let ref_result =
            oram_access_local(&mut ref_client, &mut ref_tree, addr, ref_op, &mut || {
                ref_rng.next()
            });

        let got = roram.access(0, addr, wdata.is_some(), wdata.unwrap_or(0));

        match (wdata, ref_result) {
            (None, AccessResult::ReadValue(bytes)) => {
                let want = u64::from_le_bytes(bytes);
                let mask = if data_bits_0 >= 64 {
                    u64::MAX
                } else {
                    (1u64 << data_bits_0) - 1
                };
                assert_eq!(
                    got & mask,
                    want & mask,
                    "step {step}: recursive read({addr}) != reference"
                );
                assert_eq!(
                    got & mask,
                    model[addr as usize] & mask,
                    "step {step}: read({addr}) != model"
                );
            }
            (Some(d), AccessResult::WriteAck) => {
                model[addr as usize] = d;
            }
            other => panic!("step {step}: unexpected pairing {other:?}"),
        }
    }
}

fn mixed_ops(n: usize, touched: usize, seed: u64, data_bits: usize) -> Vec<(u64, Option<u64>)> {
    let mut ops = Vec::new();
    let touched = touched.min(n);
    for a in 0..touched as u64 {
        ops.push((
            a,
            Some((a.wrapping_mul(7).wrapping_add(1)) & ((1u64 << data_bits.min(64)) - 1)),
        ));
    }
    for a in 0..touched as u64 {
        ops.push((a, None));
    }
    let mut rng = Splitmix(seed);
    for _ in 0..8 {
        let a = rng.next() % n as u64;
        let w = if rng.next() % 2 == 0 {
            Some(rng.next() & ((1u64 << data_bits.min(64)) - 1))
        } else {
            None
        };
        ops.push((a, w));
    }
    ops
}

/// 2-level recursion (small): N=16, c=4 -> levels [16, 4(base)].
#[test]
fn s5c_recursive_2level() {
    let ops = mixed_ops(16, 12, 0xabcd, 8);
    check_recursive::<2, 2>(16, 8, 4, 4, &ops, 0x9999);
}

/// 3-level recursion: N=64, c=4 -> levels [64, 16, 4(base)].
#[test]
fn s5c_recursive_3level() {
    let ops = mixed_ops(64, 10, 0x5678, 8);
    check_recursive::<2, 4>(64, 8, 4, 4, &ops, 0x7777);
}

/// 4-level recursion (heavyweight): N=256, c=4 -> levels [256, 64, 16,
/// 4(base)]. The main access's leaf comes from a chain of 3 recursive posmap
/// accesses; only the 4-entry base posmap is linear-scanned.
#[test]
#[ignore = "heavyweight: deep recursion; run with --ignored"]
fn s5c_recursive_4level() {
    let ops = mixed_ops(256, 24, 0x1234, 8);
    check_recursive::<2, 4>(256, 8, 4, 4, &ops, 0x7777);
}
