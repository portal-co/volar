// @reliability: experimental
// @ai: assisted
//! S6 hardening: **encrypt the `valid` bit**. The plain `encrypted` mode leaves
//! each entry's valid bit plaintext (so a zero tree reads as dummies), which
//! lets the tree-hosting evaluator see the *occupancy pattern* — which physical
//! slots hold real blocks. With `encrypt_valid`, the per-node pad covers the
//! whole `eb`-bit entry, so occupancy is hidden too.
//!
//! The cost: an all-zero ciphertext now decrypts to `pad` (garbage valid bit),
//! so the initial tree can no longer be all zeros — the key holder pre-formats
//! it, setting each dummy slot's ciphertext to its pad (`AES(key, tweak)`,
//! via [`slot_tweak_bytes`]) so it decrypts to a zero/dummy entry. This test
//! checks the pre-formatted encrypted ORAM matches a model and that the stored
//! dummies are genuine ciphertext (non-trivial, unlike the all-zero plaintext
//! tree).

use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::faest::aes::encrypt_block;
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin, slot_tweak_bytes};

const Z: usize = 2;
const B: usize = 1;

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter().enumerate().fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

/// Pack an `eb`-bit value (LSB-first) into an entry's `data` field.
fn pack(bits: &[bool]) -> OramEntry<B> {
    let mut data = [0u8; B];
    for (i, &b) in bits.iter().enumerate() {
        if b {
            data[i / 8] |= 1 << (i % 8);
        }
    }
    OramEntry { addr: 0, leaf: 0, data }
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

/// Pre-format the initial tree for `encrypt_valid`: every slot is a dummy whose
/// ciphertext equals its pad, so the circuit decrypts it to a zero/dummy entry.
fn format_tree(cfg: &OramGadgetConfig, tree_key: &[u8; 16]) -> OramTree<Z, B> {
    let eb = cfg.entry_bits();
    let mut tree = OramTree::<Z, B>::new(cfg.levels);
    for d in 0..cfg.levels {
        for k in 0..(1usize << d) {
            let idx = (1usize << d) - 1 + k;
            let aes_out = encrypt_block(tree_key, &slot_tweak_bytes(d, 0, k as u64));
            for zs in 0..Z {
                let mut data = [0u8; B];
                for i in 0..eb {
                    let gpos = zs * eb + i;
                    if (aes_out[gpos / 8] >> (gpos % 8)) & 1 == 1 {
                        data[i / 8] |= 1 << (i % 8);
                    }
                }
                tree.buckets[idx].entries[zs] = OramEntry { addr: 0, leaf: 0, data };
            }
        }
    }
    tree
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
fn s6_encrypt_valid_hides_occupancy() {
    let cfg = OramGadgetConfig {
        num_addrs: 4,
        levels: 3,
        bucket_size: Z,
        data_bits: 1,
        max_stash: 2 * 3 + Z + 4,
        encrypted: true,
        tree_key_bits: 128,
        encrypt_valid: true,
        keyed_leaf: false,
        versioned_pads: false,
        version_bits: 0,
    };
    let (eb, lb, ab, db) = (cfg.entry_bits(), cfg.leaf_bits(), cfg.addr_bits(), cfg.data_bits);
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    assert!(Z * eb <= 128, "one AES block covers a node's slots");
    let tree_key: [u8; 16] = [
        0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c,
    ];
    let tk_bits: Vec<bool> = tree_key.iter().flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1)).collect();

    let begin = build_begin(&cfg);
    let access = build_access(&cfg);

    let mut posmap_bits: Vec<bool> = (0..cfg.num_addrs).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits = vec![false; cfg.max_stash * eb];
    let mut tree = format_tree(&cfg, &tree_key);

    // The pre-formatted dummy slots are genuine ciphertext (non-trivial),
    // unlike the all-zero plaintext tree.
    let initial_bytes: Vec<u8> = tree.buckets.iter().flat_map(|b| b.entries.iter().map(|e| e.data[0])).collect();
    assert!(
        initial_bytes.iter().any(|&b| b != 0),
        "encrypt_valid: pre-formatted dummy tree must be non-trivial ciphertext"
    );

    let mut counter = 0u64;
    let mut rng = Splitmix(0xdead_beef);
    let mut model = [0u8; 4];

    let ops: [(u64, Option<u8>); 8] = [
        (0, Some(1)),
        (1, Some(0)),
        (2, Some(1)),
        (0, None),
        (1, None),
        (2, None),
        (3, Some(1)),
        (3, None),
    ];

    for (step, &(addr, w)) in ops.iter().enumerate() {
        let new_leaf = rng.next() % num_leaves;
        // begin.
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, ab));
        begin_in.extend(enc(new_leaf, lb));
        let bout = volar_fuzz::interpreter::biir::eval_biir(&begin, &begin_in).expect("begin evals");
        let old_leaf = dec(&bout[..lb]);
        posmap_bits = bout[lb..].to_vec();

        let mut run_access = |path_bits: Vec<bool>, op_write: bool, wd: u64, pleaf: u64, nleaf: u64, ev: bool, stash: &[bool]| -> Vec<bool> {
            let mut acc_in = stash.to_vec();
            acc_in.extend(path_bits);
            acc_in.extend(enc(addr, ab));
            acc_in.push(op_write);
            acc_in.extend(enc(wd, db));
            acc_in.extend(enc(pleaf, lb));
            acc_in.extend(enc(nleaf, lb));
            acc_in.push(ev);
            acc_in.extend(tk_bits.iter().copied());
            assert_eq!(acc_in.len(), cfg.access_params());
            let out = volar_fuzz::interpreter::biir::eval_biir(&access, &acc_in).expect("access evals");
            assert!(!out[0], "step {step}: stash overflow");
            out
        };

        // main.
        let mp = tree.read_path(old_leaf);
        let out = run_access(flatten_cipher_path(&mp, eb), w.is_some(), w.unwrap_or(0) as u64, old_leaf, new_leaf, false, &stash_bits);
        let rdata = dec(&out[1..1 + db]) as u8;
        let new_path = unflatten_cipher_path(&out[1 + db..1 + db + n_path * eb], &cfg);
        stash_bits = out[1 + db + n_path * eb..].to_vec();
        tree.write_path(old_leaf, &new_path);

        // eviction.
        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let ep = tree.read_path(evict_leaf);
            let eout = run_access(flatten_cipher_path(&ep, eb), false, 0, evict_leaf, 0, true, &stash_bits);
            let nepath = unflatten_cipher_path(&eout[1 + db..1 + db + n_path * eb], &cfg);
            stash_bits = eout[1 + db + n_path * eb..].to_vec();
            tree.write_path(evict_leaf, &nepath);
        }

        match w {
            Some(d) => model[addr as usize] = d,
            None => assert_eq!(rdata, model[addr as usize], "step {step}: read({addr})"),
        }
    }
}
