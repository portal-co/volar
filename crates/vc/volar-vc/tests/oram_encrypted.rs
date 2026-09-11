// @reliability: experimental
// @ai: assisted
//! S5a: the **encrypted** in-circuit ORAM tree, validated concretely.
//!
//! The gadget encrypts each tree block's `[addr, leaf, data]` payload in-circuit
//! with `AES-128(tree_key, slot_tweak)` (the `tree_key` is a secret input), so
//! the physical tree stores ciphertext and the tree-hosting evaluator cannot
//! read block tags. The `valid` bit stays plaintext so an untouched all-zero
//! tree reads as dummies with no formatting pass.
//!
//! This drives the encrypted begin/access circuits concretely (via `eval_biir`)
//! against a ciphertext-storing tree, checks every read against an independent
//! model, and asserts the stored bytes are key-dependent (i.e. the payloads are
//! actually encrypted).

use volar_fuzz::interpreter::biir::eval_biir;
use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};

const NUM_ADDRS: usize = 4;
const LEVELS: usize = 3;
const Z: usize = 2;
const B: usize = 2; // tree block byte width: holds the eb-bit ciphertext (eb = 13)
const MAX_STASH: usize = 2 * LEVELS + Z + NUM_ADDRS;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: NUM_ADDRS,
        levels: LEVELS,
        bucket_size: Z,
        data_bits: 8,
        max_stash: MAX_STASH,
        encrypted: true,
        tree_key_bits: 128,
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

/// The tree stores one eb-bit ciphertext per slot, packed into the entry's
/// `data` field (addr/leaf unused — they live *inside* the ciphertext).
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

/// Run the op sequence on an encrypted-tree ORAM with the given key, returning
/// the final tree (so the test can compare ciphertext across keys). Every read
/// is checked against the model.
fn run_encrypted(cfg: &OramGadgetConfig, tree_key: [u8; 16], ops: &[(u64, Option<u8>)]) -> OramTree<Z, B> {
    let begin = build_begin(cfg);
    let access = build_access(cfg);
    let (eb, lb, ab, db) = (
        cfg.entry_bits(),
        cfg.leaf_bits(),
        cfg.addr_bits(),
        cfg.data_bits,
    );
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    let num_addrs = cfg.num_addrs;

    let tk_bits: Vec<bool> = tree_key
        .iter()
        .flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1))
        .collect();
    assert_eq!(tk_bits.len(), 128);

    let mut posmap_bits: Vec<bool> = (0..num_addrs).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits: Vec<bool> = vec![false; cfg.max_stash * eb];
    let mut tree = OramTree::<Z, B>::new(cfg.levels);
    let mut counter: u64 = 0;
    let mut rng = Splitmix(0xabcd_ef01_2345_6789);
    let mut model = vec![0u8; num_addrs];

    for (step, &(addr, wdata)) in ops.iter().enumerate() {
        let new_leaf = rng.next() % num_leaves;
        // begin: posmap update.
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let begin_out = eval_biir(&begin, &begin_in).expect("begin evaluates");
        let old_leaf = dec(&begin_out[..lb]);
        posmap_bits = begin_out[lb..].to_vec();

        // One access-circuit invocation given a path (ciphertext) and flags.
        let mut run_access = |path_bits: Vec<bool>,
                              op_write: bool,
                              wd: u64,
                              pleaf: u64,
                              nleaf: u64,
                              evict_only: bool,
                              stash: &[bool]|
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
            assert_eq!(acc_in.len(), cfg.access_params());
            let out = eval_biir(&access, &acc_in).expect("access evaluates");
            assert!(!out[0], "step {step}: ORAM stash overflow");
            out
        };

        // Main path.
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
        );
        let rdata = dec(&out[1..1 + db]);
        let new_path_bits = &out[1 + db..1 + db + n_path * eb];
        stash_bits = out[1 + db + n_path * eb..].to_vec();
        tree.write_path(old_leaf, &unflatten_cipher_path(new_path_bits, cfg));

        // Deterministic post-access eviction (skipped on collision).
        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let ebits = flatten_cipher_path(&epath, eb);
            let eout = run_access(ebits, false, 0, evict_leaf, 0, true, &stash_bits);
            let new_epath_bits = &eout[1 + db..1 + db + n_path * eb];
            stash_bits = eout[1 + db + n_path * eb..].to_vec();
            tree.write_path(evict_leaf, &unflatten_cipher_path(new_epath_bits, cfg));
        }

        // Model check.
        match wdata {
            None => assert_eq!(
                rdata as u8, model[addr as usize],
                "step {step}: read({addr}) != model"
            ),
            Some(b) => model[addr as usize] = b,
        }
    }
    tree
}

/// The raw ciphertext bytes of every slot in the tree (for key-sensitivity).
fn tree_ciphertext(tree: &OramTree<Z, B>) -> Vec<u8> {
    let mut v = Vec::new();
    for bucket in &tree.buckets {
        for e in &bucket.entries {
            v.extend_from_slice(&e.data);
        }
    }
    v
}

/// Heavyweight: the access circuit inlines one AES-128 per path slot (~460k
/// ANDs at this geometry), so a full op sequence is slow under `eval_biir`.
/// The FAEST-style low-AND S-box (S6) is what brings this down. Run explicitly
/// with `cargo test -p volar-vc --test oram_encrypted -- --ignored`.
#[test]
#[ignore = "heavyweight: AES-per-slot circuit; S6 FAEST S-box reduces the cost"]
fn s5a_encrypted_oram_matches_model() {
    let cfg = cfg();
    let mut rng = Splitmix(0x1234_5678_9abc_def0);
    let mut ops: Vec<(u64, Option<u8>)> = Vec::new();
    for a in 0..NUM_ADDRS as u64 {
        ops.push((a, Some((a * 17 + 3) as u8)));
    }
    for a in 0..NUM_ADDRS as u64 {
        ops.push((a, None));
    }
    for _ in 0..12 {
        let a = rng.next() % NUM_ADDRS as u64;
        let w = if rng.next() % 2 == 0 {
            Some(rng.next() as u8)
        } else {
            None
        };
        ops.push((a, w));
    }

    let key_a = [0x11u8; 16];
    let key_b = [0x22u8; 16];
    let tree_a = run_encrypted(&cfg, key_a, &ops);
    let tree_b = run_encrypted(&cfg, key_b, &ops);

    // Obliviousness sanity: the stored ciphertext is key-dependent (so the
    // tree-hosting evaluator, which lacks the key, cannot read the payloads),
    // and it is not the trivial all-zero plaintext.
    let ca = tree_ciphertext(&tree_a);
    let cb = tree_ciphertext(&tree_b);
    assert_ne!(ca, cb, "ciphertext must depend on the tree key");
    assert!(
        ca.iter().any(|&b| b != 0),
        "ciphertext must be non-trivial after writes"
    );
}
