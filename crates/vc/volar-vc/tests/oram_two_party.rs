// @reliability: experimental
// @ai: assisted
//! S2: a symbolic-address ORAM access run **two-party** (garbler + evaluator
//! over OT), with the physical tree kept external and only the per-access leaf
//! revealed. Builds on S1 (`oram_gadget`), whose circuits are reused unchanged.
//!
//! What this demonstrates over S1:
//! - The begin/access circuits run as a genuine two-party garbled computation
//!   (`VcEmbedder` + `evaluate_multi` over `LoopbackOt`), not just concrete
//!   `eval_biir`.
//! - The logical address / op / write-data are the **garbler's private inputs**
//!   (the evaluator — the tree host — never learns them); the physical path is
//!   the **evaluator's private input** (its own tree, OT-hidden from the
//!   garbler). The only value revealed to both is the oblivious `old_leaf`.
//!
//! Scope / honesty: the position map and stash are threaded through the trusted
//! single-process test harness as public inputs purely to check correctness of
//! a short access sequence. Keeping them *secret* across steps (re-based label
//! mappings that never decode to cleartext) is the S4 looping work, not S2. And
//! the plaintext tree means the evaluator can read block tags — full
//! obliviousness-from-the-evaluator needs the S5 encrypted tree. This test
//! pins the two-party mechanism and correctness.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_oram::{AccessOp, AccessResult, Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};
use volar_vc::{VcEmbedder, VcOutcome};

type N = U16;
type D = Sha256;

// S2 geometry (small, so the const generics stay manageable). Counts probed
// from the gadget's compiled schedules: begin I=12 A=20, access I=198 A=1999.
const NUM_ADDRS: usize = 4;
const LEVELS: usize = 3;
const Z: usize = 2;
const B: usize = 1;
const MAX_STASH: usize = 8;
const BEGIN_I: usize = 12;
const BEGIN_A: usize = 20;
const ACCESS_I: usize = 198;
const ACCESS_A: usize = 1999;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: NUM_ADDRS,
        levels: LEVELS,
        bucket_size: Z,
        data_bits: B * 8,
        max_stash: MAX_STASH,
        encrypted: false,
        tree_key_bits: 0,
        encrypt_valid: false,
        keyed_leaf: false,
        versioned_pads: false,
        version_bits: 0,
    }
}

fn det_bytes(seed: u8) -> Array<u8, N> {
    Array::clone_from_slice(&[seed; 16])
}
fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: det_bytes(seed),
    }
}

// --- bit helpers (LSB-first, matching the gadget) --------------------------
fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

fn flatten_path(path: &[Bucket<Z, B>], cfg: &OramGadgetConfig) -> Vec<bool> {
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
fn unflatten_path(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, B>> {
    let eb = cfg.entry_bits();
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

/// Run one circuit two-party. Inputs are the full bit-vector in layout order
/// plus a per-bit owner partition; returns the decoded (revealed) outputs.
fn run_2pc<const I: usize, const A: usize>(
    circuit: &volar_ir::boolar::BIrBlocks,
    partition: &[InputOwner],
    inputs: &[bool],
    ot: &mut LoopbackOt<N>,
) -> Vec<bool> {
    assert_eq!(inputs.len(), I, "input width");
    assert_eq!(partition.len(), I, "partition width");
    let secret = GlobalSecret::<N>::new(det_bytes(7));
    let labels: [Garble<N>; I] = core::array::from_fn(|i| det_label((i % 256) as u8));
    let embedder = VcEmbedder::<N, I, A>::with_secret(secret, labels);
    let schedule = VcEmbedder::<N, I, A>::compile(circuit).expect("circuit schedules");
    // Split inputs by owner, preserving input-index order.
    let mut public = Vec::new();
    let mut garbler = Vec::new();
    let mut evaluator = Vec::new();
    for (i, &bit) in inputs.iter().enumerate() {
        match partition[i] {
            InputOwner::Public => public.push(bit),
            InputOwner::Garbler => garbler.push(bit),
            InputOwner::Evaluator => evaluator.push(bit),
        }
    }
    match embedder.invoke_schedule::<D>(&schedule, partition, &public, &garbler, &evaluator, ot) {
        VcOutcome::Value(bits) => {
            // Cross-check: the two-party result must equal concrete eval.
            let concrete = volar_fuzz::interpreter::biir::eval_biir(circuit, inputs)
                .expect("concrete eval");
            if bits != concrete {
                let first = bits
                    .iter()
                    .zip(&concrete)
                    .position(|(a, b)| a != b)
                    .unwrap_or(usize::MAX);
                panic!(
                    "two-party != concrete: len {} vs {}, first diff at out-bit {first} (2pc={:?} concrete={:?}); inputs len {}",
                    bits.len(),
                    concrete.len(),
                    bits.get(first).copied(),
                    concrete.get(first).copied(),
                    inputs.len(),
                );
            }
            bits
        }
        other => panic!("two-party run aborted: {other:?}"),
    }
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

#[test]
fn s2_two_party_symbolic_access() {
    let cfg = cfg();
    let (ab, lb, db, eb) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.data_bits, cfg.entry_bits());
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    let begin = build_begin(&cfg);
    let access = build_access(&cfg);
    assert_eq!(begin.blocks[0].params as usize, BEGIN_I);
    assert_eq!(access.blocks[0].params as usize, ACCESS_I);

    let mut ot = LoopbackOt::<N>::new();
    let mut tree = OramTree::<Z, B>::new(LEVELS);
    let mut counter: u64 = 0;
    let mut leaf_rng = Splitmix(0xdead_beef_cafe_f00d); // garbler's leaf randomness
    let mut model = [0u8; NUM_ADDRS];

    // Secret-shared ORAM client state, threaded through the trusted harness.
    let mut posmap_bits: Vec<bool> = (0..NUM_ADDRS).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits: Vec<bool> = vec![false; MAX_STASH * eb];

    // begin partition: [posmap: Public, addr: Evaluator, new_leaf: Garbler]
    let begin_partition: Vec<InputOwner> = (0..BEGIN_I)
        .map(|i| {
            if i < NUM_ADDRS * lb {
                InputOwner::Public
            } else if i < NUM_ADDRS * lb + ab {
                InputOwner::Evaluator
            } else {
                InputOwner::Garbler
            }
        })
        .collect();
    // access partition: [stash Public, path Evaluator, addr Evaluator,
    //   op Evaluator, wdata Evaluator, path_leaf Public, new_leaf Garbler,
    //   evict_only Public]
    let path_end = MAX_STASH * eb + n_path * eb;
    let access_partition: Vec<InputOwner> = (0..ACCESS_I)
        .map(|i| {
            let addr_off = path_end;
            let op_off = addr_off + ab;
            let wdata_off = op_off + 1;
            let pleaf_off = wdata_off + db;
            let nleaf_off = pleaf_off + lb;
            let evonly_off = nleaf_off + lb;
            if i < MAX_STASH * eb {
                InputOwner::Public // stash
            } else if i < path_end {
                InputOwner::Evaluator // path (evaluator's tree)
            } else if i < op_off + 1 {
                InputOwner::Evaluator // addr + op_write
            } else if i < pleaf_off {
                InputOwner::Evaluator // wdata
            } else if i < nleaf_off {
                InputOwner::Public // path_leaf (revealed old_leaf)
            } else if i < evonly_off {
                InputOwner::Garbler // new_leaf
            } else {
                InputOwner::Public // evict_only
            }
        })
        .collect();

    // Ops: write then re-read with an overwrite, plus an untouched-address read.
    let ops: [(u64, Option<u8>); 5] = [
        (2, Some(0xAB)),
        (2, None),
        (2, Some(0xCD)),
        (2, None),
        (0, None),
    ];

    for (addr, wdata) in ops {
        let new_leaf = leaf_rng.next() % num_leaves; // garbler's fresh leaf
        // begin: posmap.update(addr, new_leaf) -> old_leaf
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let begin_out = run_2pc::<BEGIN_I, BEGIN_A>(&begin, &begin_partition, &begin_in, &mut ot);
        let old_leaf = dec(&begin_out[..lb]);
        posmap_bits = begin_out[lb..].to_vec();
        assert!(old_leaf < num_leaves, "old_leaf in range");

        // extern: evaluator reads the main path from its tree.
        let main_path = tree.read_path(old_leaf);

        // access (main)
        let mut acc_in = stash_bits.clone();
        acc_in.extend(flatten_path(&main_path, &cfg));
        acc_in.extend(enc(addr, ab));
        acc_in.push(wdata.is_some());
        acc_in.extend(enc(wdata.unwrap_or(0) as u64, db));
        acc_in.extend(enc(old_leaf, lb));
        acc_in.extend(enc(new_leaf, lb));
        acc_in.push(false);
        let acc_out = run_2pc::<ACCESS_I, ACCESS_A>(&access, &access_partition, &acc_in, &mut ot);
        assert!(!acc_out[0], "ORAM stash overflow");
        let rdata = dec(&acc_out[1..1 + db]);
        let new_path_bits = &acc_out[1 + db..1 + db + n_path * eb];
        stash_bits = acc_out[1 + db + n_path * eb..].to_vec();
        tree.write_path(old_leaf, &unflatten_path(new_path_bits, &cfg));

        // deterministic eviction (one pass, skipped on collision with old_leaf)
        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let mut ev_in = stash_bits.clone();
            ev_in.extend(flatten_path(&epath, &cfg));
            ev_in.extend(enc(0, ab));
            ev_in.push(false);
            ev_in.extend(enc(0, db));
            ev_in.extend(enc(evict_leaf, lb));
            ev_in.extend(enc(0, lb));
            ev_in.push(true); // evict_only
            let ev_out = run_2pc::<ACCESS_I, ACCESS_A>(&access, &access_partition, &ev_in, &mut ot);
            assert!(!ev_out[0], "ORAM stash overflow (evict)");
            let new_epath_bits = &ev_out[1 + db..1 + db + n_path * eb];
            stash_bits = ev_out[1 + db + n_path * eb..].to_vec();
            tree.write_path(evict_leaf, &unflatten_path(new_epath_bits, &cfg));
        }

        match wdata {
            Some(b) => model[addr as usize] = b,
            None => assert_eq!(
                rdata as u8, model[addr as usize],
                "read({addr}) must return last written byte"
            ),
        }
    }
}
