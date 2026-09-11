// @reliability: experimental
// @ai: assisted
//! S1 conformance test for the in-circuit ORAM gadget (`volar_vc::oram_gadget`).
//!
//! The gadget's begin/access circuits are run **concretely** under
//! `volar_fuzz::interpreter::biir::eval_biir`, with the physical tree kept
//! external (a real `volar_oram::OramTree` driven by the test harness). The
//! same random read/write sequence is applied to a reference
//! `volar_oram::oram_access_local` client, and every read's result is compared.
//! This validates that the ORAM client logic, expressed as a circuit, is a
//! correct ORAM — the correctness half of S1 (the garbling/reveal half is S2).

use volar_oram::{
    AccessOp, AccessResult, Bucket, OramClient, OramEntry, OramTree, eviction_target,
    oram_access_local,
};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};

// --- geometry --------------------------------------------------------------
const LEVELS: usize = 4;
const NUM_ADDRS: usize = 8;
const Z: usize = 4; // bucket size
const B: usize = 1; // block payload bytes (data_bits = 8)

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: NUM_ADDRS,
        levels: LEVELS,
        bucket_size: Z,
        data_bits: B * 8,
        // Generous stash so the S1 test never overflows; total real blocks
        // <= NUM_ADDRS, so this is ample margin.
        max_stash: 2 * LEVELS + Z + NUM_ADDRS,
        encrypted: false,
        tree_key_bits: 0,
        encrypt_valid: false,
        keyed_leaf: false,
    }
}

// --- bit helpers (LSB-first, matching the gadget's encoding) ---------------
fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

/// Flatten one entry to the circuit layout `[valid, addr, leaf, data]`.
fn enc_entry<const B: usize>(e: &OramEntry<B>, cfg: &OramGadgetConfig) -> Vec<bool> {
    let real = e.is_real();
    let mut v = vec![real];
    v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
    v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
    // Pack all B payload bytes LSB-first (little-endian byte order).
    let data = e.data.iter().take(B).fold(0u64, |a, &b| (a << 8) | b as u64);
    v.extend(enc(if real { data } else { 0 }, cfg.data_bits));
    v
}
fn dec_entry<const B: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> OramEntry<B> {
    let ab = cfg.addr_bits();
    let lb = cfg.leaf_bits();
    let db = cfg.data_bits;
    let valid = bits[0];
    if !valid {
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
    let mut out = Vec::new();
    for level in 0..cfg.levels {
        let mut entries = [OramEntry::dummy(); Z];
        for (s, slot) in entries.iter_mut().enumerate() {
            let k = level * Z + s;
            *slot = dec_entry(&bits[k * eb..(k + 1) * eb], cfg);
        }
        out.push(Bucket { entries });
    }
    out
}

/// Tiny deterministic RNG (splitmix64) for test leaf/data values.
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

/// Run an op sequence through both the circuit-ORAM and the reference
/// concretely, comparing every read against both the reference and an
/// independent addr->data model. Generic over bucket size `ZC` (B = 1 byte).
fn check_concrete<const ZC: usize>(
    cfg: &OramGadgetConfig,
    ops: &[(u64, Option<u8>)],
    seed: u64,
) {
    let begin = build_begin(cfg);
    let access = build_access(cfg);
    let (eb, lb, ab, db) = (cfg.entry_bits(), cfg.leaf_bits(), cfg.addr_bits(), cfg.data_bits);
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    let levels = cfg.levels;
    let num_addrs = cfg.num_addrs;

    // Circuit-side opaque state (threaded bit-vectors).
    let mut posmap_bits: Vec<bool> = (0..num_addrs).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits: Vec<bool> = vec![false; cfg.max_stash * eb];
    let mut tree = OramTree::<ZC, 1>::new(levels);
    let mut counter: u64 = 0;

    // Reference client/tree.
    let mut ref_client = OramClient::<ZC, 1>::new(levels, num_addrs as u64);
    let mut ref_tree = OramTree::<ZC, 1>::new(levels);

    let mut rng = Splitmix(seed);
    let mut ref_rng = Splitmix(seed ^ 0xffff_ffff_ffff_ffff);
    let mut model = vec![0u8; num_addrs];

    for (step, &(addr, wdata)) in ops.iter().enumerate() {
        // Reference.
        let ref_op = match wdata {
            None => AccessOp::Read,
            Some(b) => AccessOp::Write([b]),
        };
        let mut ref_rng_fn = | | ref_rng.next();
        let ref_result = oram_access_local(&mut ref_client, &mut ref_tree, addr, ref_op, &mut ref_rng_fn);

        // Circuit.
        let new_leaf = rng.next() % num_leaves;
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let begin_out = volar_fuzz::interpreter::biir::eval_biir(&begin, &begin_in)
            .expect("begin circuit evaluates");
        let old_leaf = dec(&begin_out[..lb]);
        posmap_bits = begin_out[lb..].to_vec();
        assert!(old_leaf < num_leaves, "old_leaf out of range");

        let main_path = tree.read_path(old_leaf);
        let path_bits = flatten_path::<ZC, 1>(&main_path, cfg);

        let mut acc_in = stash_bits.clone();
        acc_in.extend(path_bits);
        acc_in.extend(enc(addr, ab));
        acc_in.push(wdata.is_some());
        acc_in.extend(enc(wdata.unwrap_or(0) as u64, db));
        acc_in.extend(enc(old_leaf, lb));
        acc_in.extend(enc(new_leaf, lb));
        acc_in.push(false);
        assert_eq!(acc_in.len(), cfg.access_params());
        let acc_out = volar_fuzz::interpreter::biir::eval_biir(&access, &acc_in)
            .expect("access circuit evaluates");
        assert_eq!(acc_out.len(), cfg.access_outputs());
        assert!(!acc_out[0], "step {step}: ORAM stash overflow");
        let rdata = dec(&acc_out[1..1 + db]);
        let new_path_bits = &acc_out[1 + db..1 + db + n_path * eb];
        stash_bits = acc_out[1 + db + n_path * eb..].to_vec();
        assert_eq!(stash_bits.len(), cfg.max_stash * eb);

        tree.write_path(old_leaf, &unflatten_path::<ZC, 1>(new_path_bits, cfg));

        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let epath_bits = flatten_path::<ZC, 1>(&epath, cfg);
            let mut ev_in = stash_bits.clone();
            ev_in.extend(epath_bits);
            ev_in.extend(enc(0, ab));
            ev_in.push(false);
            ev_in.extend(enc(0, db));
            ev_in.extend(enc(evict_leaf, lb));
            ev_in.extend(enc(0, lb));
            ev_in.push(true); // evict_only
            let ev_out = volar_fuzz::interpreter::biir::eval_biir(&access, &ev_in)
                .expect("eviction evaluates");
            assert!(!ev_out[0], "step {step}: ORAM stash overflow during eviction");
            let new_epath_bits = &ev_out[1 + db..1 + db + n_path * eb];
            stash_bits = ev_out[1 + db + n_path * eb..].to_vec();
            tree.write_path(evict_leaf, &unflatten_path::<ZC, 1>(new_epath_bits, cfg));
        }

        match (wdata, ref_result) {
            (None, AccessResult::ReadValue(bytes)) => {
                assert_eq!(
                    rdata as u8, bytes[0],
                    "step {step}: read({addr}) mismatch: circuit={rdata:#x} ref={:#x}",
                    bytes[0]
                );
                assert_eq!(
                    rdata as u8, model[addr as usize],
                    "step {step}: read({addr}) != model: circuit={rdata:#x} model={:#x}",
                    model[addr as usize]
                );
            }
            (Some(b), AccessResult::WriteAck) => {
                model[addr as usize] = b;
            }
            other => panic!("step {step}: unexpected result pairing {other:?}"),
        }
    }
}

/// Run a random op sequence through both the circuit-ORAM and the reference,
/// comparing every read.
#[test]
fn s1_circuit_oram_matches_reference() {
    let cfg = cfg();
    let begin = build_begin(&cfg);
    let access = build_access(&cfg);
    // Sanity: circuits are circuit-shaped with the documented widths.
    assert_eq!(begin.blocks[0].params as usize, cfg.begin_params());
    assert_eq!(access.blocks[0].params as usize, cfg.access_params());

    let mut rng = Splitmix(0x1234_5678_9abc_def0);
    let mut ops: Vec<(u64, Option<u8>)> = Vec::new();
    for a in 0..NUM_ADDRS as u64 {
        ops.push((a, Some((a * 17 + 3) as u8)));
    }
    for a in 0..NUM_ADDRS as u64 {
        ops.push((a, None));
    }
    for _ in 0..40 {
        let a = rng.next() % NUM_ADDRS as u64;
        let w = if rng.next() % 2 == 0 {
            Some(rng.next() as u8)
        } else {
            None
        };
        ops.push((a, w));
    }
    check_concrete::<Z>(&cfg, &ops, 0x1234_5678_9abc_def0);
}

/// Concrete isolation of the S2 two-party geometry: run the exact small
/// geometry + op sequence through `eval_biir` to tell a circuit/geometry bug
/// apart from a two-party driving bug.
#[test]
fn s2_geometry_concrete() {
    let cfg = OramGadgetConfig {
        num_addrs: 4,
        levels: 3,
        bucket_size: 2,
        data_bits: 8,
        max_stash: 8,
        encrypted: false,
        tree_key_bits: 0,
        encrypt_valid: false,
        keyed_leaf: false,
    };
    let ops: [(u64, Option<u8>); 5] = [
        (2, Some(0xAB)),
        (2, None),
        (2, Some(0xCD)),
        (2, None),
        (0, None),
    ];
    check_concrete::<2>(&cfg, &ops, 0xdead_beef_cafe_f00d);
}
