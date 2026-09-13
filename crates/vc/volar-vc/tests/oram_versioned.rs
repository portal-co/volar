// @reliability: experimental
// @ai: assisted
//! S6 hardening: **per-node versioned pads** (replay protection). The tree pad
//! for each physical node depends on a per-node *version* bumped on every write
//! to it, so a stale block replayed by a malicious host was encrypted under an
//! older version's pad and decrypts to garbage. The harness tracks versions
//! (public) and feeds the current per-path-node versions to the access circuit,
//! which decrypts with the current version and re-encrypts with `version + 1`.
//!
//! This is the mechanism; full replay *detection* additionally needs an
//! in-payload MAC (a malicious-security follow-up — the base threat model is
//! semi-honest). Here `valid` stays plaintext (`encrypt_valid: false`) so the
//! all-zero initial tree still reads as dummies and no pre-formatting is
//! needed; versioning composes with `encrypt_valid` orthogonally. Checked
//! against a model, plus a direct check that a node's pad is version-dependent.

use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::faest::aes::encrypt_block;
use volar_vc::oram_gadget::{
    OramGadgetConfig, build_access, build_begin, slot_tweak_versioned_bytes,
};

const Z: usize = 2;
const B: usize = 1;
const VB: usize = 8; // version width

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}
fn pack(bits: &[bool]) -> OramEntry<B> {
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
                pack(&bits[k * eb..(k + 1) * eb])
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

#[test]
fn s6_versioned_pads_replay_protection() {
    let cfg = OramGadgetConfig {
        num_addrs: 4,
        levels: 3,
        bucket_size: Z,
        data_bits: 1,
        max_stash: 2 * 3 + Z + 4,
        encrypted: true,
        tree_key_bits: 128,
        encrypt_valid: false,
        keyed_leaf: false,
        versioned_pads: true,
        version_bits: VB,
    };
    let (eb, lb, ab, db) = (
        cfg.entry_bits(),
        cfg.leaf_bits(),
        cfg.addr_bits(),
        cfg.data_bits,
    );
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    let tree_key: [u8; 16] = [
        0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f,
        0x3c,
    ];
    let tk_bits: Vec<bool> = tree_key
        .iter()
        .flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1))
        .collect();

    // Version-dependence sanity: a node's pad differs across versions.
    let p0 = encrypt_block(&tree_key, &slot_tweak_versioned_bytes(1, 0, 0, VB));
    let p1 = encrypt_block(&tree_key, &slot_tweak_versioned_bytes(1, 0, 1, VB));
    assert_ne!(p0, p1, "versioned pads must differ across versions");

    let begin = build_begin(&cfg);
    let access = build_access(&cfg);

    let mut posmap_bits: Vec<bool> = (0..cfg.num_addrs).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits = vec![false; cfg.max_stash * eb];
    let mut tree = OramTree::<Z, B>::new(cfg.levels);
    // Per-node versions (heap order), bumped on each write to that node.
    let mut versions = vec![0u64; tree.buckets.len()];

    let mut counter = 0u64;
    let mut rng = Splitmix(0x1234_5678);
    let mut model = [0u8; 4];

    let ops: [(u64, Option<u8>); 10] = [
        (0, Some(1)),
        (1, Some(0)),
        (0, None),
        (2, Some(1)),
        (1, Some(1)),
        (0, None),
        (2, None),
        (3, Some(1)),
        (1, None),
        (3, None),
    ];

    for (step, &(addr, w)) in ops.iter().enumerate() {
        let new_leaf = rng.next() % num_leaves;
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let bout =
            volar_fuzz::interpreter::biir::eval_biir(&begin, &begin_in).expect("begin evals");
        let old_leaf = dec(&bout[..lb]);
        posmap_bits = bout[lb..].to_vec();

        // One access-circuit invocation on `path_leaf`, feeding the current
        // per-node versions; the caller bumps those versions after write-back.
        let mut run_access = |tree: &OramTree<Z, B>,
                              versions: &[u64],
                              path_leaf: u64,
                              op_write: bool,
                              wd: u64,
                              nleaf: u64,
                              ev: bool,
                              stash: &[bool]|
         -> Vec<bool> {
            let path = tree.read_path(path_leaf);
            let idxs = tree.path_indices(path_leaf);
            let mut acc_in = stash.to_vec();
            acc_in.extend(flatten_cipher_path(&path, eb));
            acc_in.extend(enc(addr, ab));
            acc_in.push(op_write);
            acc_in.extend(enc(wd, db));
            acc_in.extend(enc(path_leaf, lb));
            acc_in.extend(enc(nleaf, lb));
            acc_in.push(ev);
            acc_in.extend(tk_bits.iter().copied());
            for d in 0..cfg.levels {
                acc_in.extend(enc(versions[idxs[d]], VB));
            }
            assert_eq!(acc_in.len(), cfg.access_params());
            let out =
                volar_fuzz::interpreter::biir::eval_biir(&access, &acc_in).expect("access evals");
            assert!(!out[0], "step {step}: stash overflow");
            out
        };

        // Main access.
        let out = run_access(
            &tree,
            &versions,
            old_leaf,
            w.is_some(),
            w.unwrap_or(0) as u64,
            new_leaf,
            false,
            &stash_bits,
        );
        let rdata = dec(&out[1..1 + db]) as u8;
        let new_path = unflatten_cipher_path(&out[1 + db..1 + db + n_path * eb], &cfg);
        stash_bits = out[1 + db + n_path * eb..].to_vec();
        let main_idxs = tree.path_indices(old_leaf);
        tree.write_path(old_leaf, &new_path);
        for &i in &main_idxs {
            versions[i] += 1; // the write bumped each main-path node's version
        }

        // Eviction pass (reads versions after the main write's bumps).
        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let eout = run_access(&tree, &versions, evict_leaf, false, 0, 0, true, &stash_bits);
            let nepath = unflatten_cipher_path(&eout[1 + db..1 + db + n_path * eb], &cfg);
            stash_bits = eout[1 + db + n_path * eb..].to_vec();
            let evict_idxs = tree.path_indices(evict_leaf);
            tree.write_path(evict_leaf, &nepath);
            for &i in &evict_idxs {
                versions[i] += 1;
            }
        }

        match w {
            Some(d) => model[addr as usize] = d,
            None => assert_eq!(rdata, model[addr as usize], "step {step}: read({addr})"),
        }
    }
}
