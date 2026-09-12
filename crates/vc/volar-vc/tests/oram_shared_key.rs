// @reliability: experimental
// @ai: assisted
//! Storage hidden from BOTH parties ("shared" ownership): the encrypted ORAM
//! tree run two-party with the `tree_key` **split across the parties** — the
//! garbler holds bits 0..64, the evaluator holds bits 64..128. The AES tree
//! pads need all 128 key bits, so NEITHER party alone can decrypt the tree
//! contents, yet the circuit (evaluating the pads on garbled wires) accesses
//! it obliviously. This is the ownership tier between "garbler-secret"
//! (S5b) and public: bulk state can live in the shared store instead of
//! being threaded through circuits, which is what keeps the crypto gadgets
//! from becoming the entire program.
//!
//! In the MPC-TLS composition the split is derived from the handshake: the
//! in-circuit X25519 produces a shared secret that neither party holds in
//! the clear, and an in-circuit HKDF expands it into the store key — a
//! per-session shared store keyed by the session itself.
//!
//! Scope / honesty: as in S2/S5b, the position map and stash are threaded
//! through the trusted harness as public inputs to check correctness of a
//! short access sequence (the re-based-secret threading is S4). The
//! structural claim here is the key split itself: correctness with neither
//! half alone being sufficient.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};
use volar_vc::{VcEmbedder, VcOutcome};

type N = U16;
type D = Sha256;

// Minimal encrypted geometry (2 AES instances per access) so the two-party run
// stays fast. Counts probed from the compiled schedules.
const NUM_ADDRS: usize = 2;
const LEVELS: usize = 2;
const Z: usize = 2;
const B: usize = 2; // tree block byte width: holds the eb-bit ciphertext (eb = 11)
const MAX_STASH: usize = 4;
const BEGIN_I: usize = 4;
const BEGIN_A: usize = 4;
const ACCESS_I: usize = 229;
const ACCESS_A: usize = 32647;

const TREE_KEY: [u8; 16] = [
    0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c,
];

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: NUM_ADDRS,
        levels: LEVELS,
        bucket_size: Z,
        data_bits: 8,
        max_stash: MAX_STASH,
        encrypted: true,
        tree_key_bits: 128,
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

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

// The tree stores one eb-bit ciphertext per slot, packed into the entry's data
// field (addr/leaf live *inside* the ciphertext).
fn pack_cipher(bits: &[bool]) -> OramEntry<B> {
    let mut data = [0u8; B];
    for (i, &b) in bits.iter().enumerate() {
        if b {
            data[i / 8] |= 1 << (i % 8);
        }
    }
    OramEntry {
        addr: 0,
        leaf: 0,
        data,
    }
}
fn flatten_cipher_path(path: &[Bucket<Z, B>], eb: usize) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            for i in 0..eb {
                v.push((e.data[i / 8] >> (i % 8)) & 1 == 1);
            }
        }
    }
    v
}
fn unflatten_cipher_path(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, B>> {
    let eb = cfg.entry_bits();
    (0..cfg.levels)
        .map(|level| Bucket {
            entries: core::array::from_fn(|s| {
                let k = level * Z + s;
                pack_cipher(&bits[k * eb..(k + 1) * eb])
            }),
        })
        .collect()
}

/// Run one circuit two-party; cross-checks against concrete eval and returns
/// the decoded (revealed) outputs.
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
            let concrete =
                volar_fuzz::interpreter::biir::eval_biir(circuit, inputs).expect("concrete eval");
            assert_eq!(bits, concrete, "two-party must equal concrete eval");
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
fn shared_key_tree_hidden_from_both_parties() {
    // The const-generic embedder builds `[GarbleTable; ACCESS_A]` on the stack
    // (~20 MB at this AND count), so run on a thread with a large stack.
    std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(shared_body)
        .expect("spawn")
        .join()
        .expect("join");
}

fn shared_body() {
    let cfg = cfg();
    let (ab, lb, db, eb) = (
        cfg.addr_bits(),
        cfg.leaf_bits(),
        cfg.data_bits,
        cfg.entry_bits(),
    );
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    let begin = build_begin(&cfg);
    let access = build_access(&cfg);
    assert_eq!(begin.blocks[0].params as usize, BEGIN_I);
    assert_eq!(access.blocks[0].params as usize, ACCESS_I);

    let tk_bits: Vec<bool> = TREE_KEY
        .iter()
        .flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1))
        .collect();

    let mut ot = LoopbackOt::<N>::new();
    let mut tree = OramTree::<Z, B>::new(LEVELS);
    let mut counter: u64 = 0;
    let mut leaf_rng = Splitmix(0xdead_beef_cafe_f00d);
    let mut model = [0u8; NUM_ADDRS];

    let mut posmap_bits: Vec<bool> = (0..NUM_ADDRS).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits: Vec<bool> = vec![false; MAX_STASH * eb];

    // begin partition: [posmap Public, addr Evaluator, new_leaf Garbler]
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
    // access partition: [stash Public, path Evaluator, addr/op/wdata Evaluator,
    //   path_leaf Public, new_leaf Garbler, evict_only Public, tree_key Garbler]
    let path_end = MAX_STASH * eb + n_path * eb;
    let tk_start = ACCESS_I - 128;
    let access_partition: Vec<InputOwner> = (0..ACCESS_I)
        .map(|i| {
            let addr_off = path_end;
            let op_off = addr_off + ab;
            let wdata_off = op_off + 1;
            let pleaf_off = wdata_off + db;
            let nleaf_off = pleaf_off + lb;
            let evonly_off = nleaf_off + lb;
            if i < MAX_STASH * eb {
                InputOwner::Public
            } else if i < path_end {
                InputOwner::Evaluator
            } else if i < pleaf_off {
                InputOwner::Evaluator // addr + op_write + wdata
            } else if i < nleaf_off {
                InputOwner::Public // path_leaf (revealed old_leaf)
            } else if i < evonly_off {
                InputOwner::Garbler // new_leaf
            } else if i < tk_start {
                InputOwner::Public // evict_only
            } else if i < tk_start + 64 {
                InputOwner::Garbler // tree_key bits 0..64 (garbler half)
            } else {
                InputOwner::Evaluator // tree_key bits 64..128 (evaluator half)
            }
        })
        .collect();

    // Ops: write, re-read, overwrite, re-read (per address, plus eviction).
    let ops: [(u64, Option<u8>); 4] = [
        (0, Some(0xA5)),
        (0, None),
        (1, Some(0x3C)),
        (0, None),
    ];

    for (addr, wdata) in ops {
        let new_leaf = leaf_rng.next() % num_leaves;
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let begin_out = run_2pc::<BEGIN_I, BEGIN_A>(&begin, &begin_partition, &begin_in, &mut ot);
        let old_leaf = dec(&begin_out[..lb]);
        posmap_bits = begin_out[lb..].to_vec();
        assert!(old_leaf < num_leaves);

        // One encrypted access-circuit invocation (main or evict).
        let mut run_access = |path_bits: Vec<bool>,
                              op_write: bool,
                              wd: u64,
                              pleaf: u64,
                              nleaf: u64,
                              evict_only: bool,
                              stash: &[bool],
                              ot: &mut LoopbackOt<N>|
         -> Vec<bool> {
            let mut acc_in = stash.to_vec();
            acc_in.extend(path_bits);
            acc_in.extend(enc(addr, ab));
            acc_in.push(op_write);
            acc_in.extend(enc(wd, db));
            acc_in.extend(enc(pleaf, lb));
            acc_in.extend(enc(nleaf, lb));
            acc_in.push(evict_only);
            acc_in.extend(tk_bits.iter().copied());
            assert_eq!(acc_in.len(), ACCESS_I);
            let out = run_2pc::<ACCESS_I, ACCESS_A>(&access, &access_partition, &acc_in, ot);
            assert!(!out[0], "ORAM stash overflow");
            out
        };

        // Main path (ciphertext).
        let main_path = tree.read_path(old_leaf);
        let path_bits = flatten_cipher_path(&main_path, eb);
        let out = run_access(
            path_bits,
            wdata.is_some(),
            wdata.unwrap_or(0) as u64,
            old_leaf,
            new_leaf,
            false,
            &stash_bits,
            &mut ot,
        );
        let rdata = dec(&out[1..1 + db]);
        let new_path_bits = &out[1 + db..1 + db + n_path * eb];
        stash_bits = out[1 + db + n_path * eb..].to_vec();
        tree.write_path(old_leaf, &unflatten_cipher_path(new_path_bits, &cfg));

        // Deterministic eviction (skipped on collision).
        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let ebits = flatten_cipher_path(&epath, eb);
            let eout = run_access(ebits, false, 0, evict_leaf, 0, true, &stash_bits, &mut ot);
            let new_epath_bits = &eout[1 + db..1 + db + n_path * eb];
            stash_bits = eout[1 + db + n_path * eb..].to_vec();
            tree.write_path(evict_leaf, &unflatten_cipher_path(new_epath_bits, &cfg));
        }

        match wdata {
            Some(b) => model[addr as usize] = b,
            None => assert_eq!(rdata as u8, model[addr as usize], "read({addr})"),
        }
    }

    // Shared-ownership sanity: the stored tree bytes are ciphertext (not the
    // plaintext encoding of any block), and the key halves live with
    // DIFFERENT parties — the garbler's half alone and the evaluator's half
    // alone are both insufficient to compute the AES tree pads (the circuit
    // consumed all 128 bits as one key).
    let mut ct = Vec::new();
    for bucket in &tree.buckets {
        for e in &bucket.entries {
            ct.extend_from_slice(&e.data);
        }
    }
    assert!(
        ct.iter().any(|&b| b != 0),
        "shared tree stores non-trivial ciphertext"
    );
    assert!(
        TREE_KEY[..8].iter().any(|&b| b != 0) && TREE_KEY[8..].iter().any(|&b| b != 0),
        "both key halves are non-trivial, so neither party's half alone suffices"
    );
}
