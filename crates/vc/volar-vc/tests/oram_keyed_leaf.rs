// @reliability: experimental
// @ai: assisted
//! S6 hardening: the **keyed-leaf PRF**. The fresh ORAM leaf is computed
//! in-circuit as `AES-128(leaf_key, counter)[0..leaf_bits]` with `leaf_key` a
//! secret input, instead of arriving as a `new_leaf` input from a public RNG.
//! A publicly predictable leaf sequence would let the evaluator link an address
//! to its previous access time; deriving the leaf from a garbled key keeps it
//! unpredictable until it is revealed as the (oblivious) physical path.
//!
//! This test checks (a) the begin circuit's derived `new_leaf` matches the
//! reference AES, and (b) a full ORAM run with keyed leaves matches a model —
//! the begin's garbled `new_leaf` output is threaded straight into the access
//! circuit (never decoded), mirroring the two-party re-based-label threading.

use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::faest::aes::encrypt_block;
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};

const Z: usize = 2;
const B: usize = 1;

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter().enumerate().fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}
fn enc_entry(e: &OramEntry<B>, cfg: &OramGadgetConfig) -> Vec<bool> {
    let real = e.is_real();
    let mut v = vec![real];
    v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
    v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
    v.extend(enc(if real { e.data[0] as u64 } else { 0 }, cfg.data_bits));
    v
}
fn dec_entry(bits: &[bool], cfg: &OramGadgetConfig) -> OramEntry<B> {
    let (ab, lb, db) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.data_bits);
    if !bits[0] {
        return OramEntry::dummy();
    }
    let addr = dec(&bits[1..1 + ab]);
    let leaf = dec(&bits[1 + ab..1 + ab + lb]);
    let data = dec(&bits[1 + ab + lb..1 + ab + lb + db]);
    OramEntry { addr, leaf, data: [data as u8] }
}
fn flatten_path(path: &[Bucket<Z, B>], cfg: &OramGadgetConfig) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            v.extend(enc_entry(e, cfg));
        }
    }
    v
}
fn unflatten_path(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, B>> {
    let eb = cfg.entry_bits();
    (0..cfg.levels)
        .map(|level| Bucket {
            entries: core::array::from_fn(|s| dec_entry(&bits[(level * Z + s) * eb..(level * Z + s + 1) * eb], cfg)),
        })
        .collect()
}

/// The keyed leaf for `counter`, as the begin circuit should derive it.
fn expected_leaf(leaf_key: &[u8; 16], counter: u64, lb: usize) -> u64 {
    let mut block = [0u8; 16];
    block[..8].copy_from_slice(&counter.to_le_bytes());
    let out = encrypt_block(leaf_key, &block);
    // The gadget slices the low `lb` bits of the 128-bit AES output (LSB-first).
    let full = u128::from_le_bytes(out);
    (full & ((1u128 << lb) - 1)) as u64
}

#[test]
fn s6_keyed_leaf_prf_matches_aes_and_oram_works() {
    let cfg = OramGadgetConfig {
        num_addrs: 8,
        levels: 4,
        bucket_size: Z,
        data_bits: 8,
        max_stash: 2 * 4 + Z + 8,
        encrypted: false,
        tree_key_bits: 0,
        encrypt_valid: false,
        keyed_leaf: true,
        versioned_pads: false,
        version_bits: 0,
    };
    let lb = cfg.leaf_bits();
    let begin = build_begin(&cfg);
    let access = build_access(&cfg);
    let leaf_key: [u8; 16] = [
        0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c,
    ];

    let mut posmap_bits: Vec<bool> = (0..cfg.num_addrs).flat_map(|_| enc(0, lb)).collect();
    let mut stash_bits = vec![false; cfg.max_stash * cfg.entry_bits()];
    let mut tree = OramTree::<Z, B>::new(cfg.levels);
    let mut counter = 0u64;
    let mut model = [0u8; 8];

    let ops: [(u64, Option<u8>); 8] = [
        (1, Some(0xA5)),
        (2, Some(0x3C)),
        (1, None),
        (2, None),
        (5, Some(0x77)),
        (1, Some(0x99)),
        (1, None),
        (5, None),
    ];

    for (step, &(addr, w)) in ops.iter().enumerate() {
        // begin: old_leaf = posmap[addr]; posmap[addr] = new_leaf (derived
        // in-circuit from leaf_key + counter). Also outputs new_leaf.
        let mut begin_in = posmap_bits.clone();
        begin_in.extend(enc(addr, cfg.addr_bits()));
        for byte in leaf_key {
            begin_in.extend(enc(byte as u64, 8));
        }
        begin_in.extend(enc(counter, 64));
        let out = volar_fuzz::interpreter::biir::eval_biir(&begin, &begin_in).expect("begin evals");
        let old_leaf = dec(&out[..lb]);
        let new_leaf = dec(&out[lb..2 * lb]);
        posmap_bits = out[2 * lb..].to_vec();
        // (a) the derived leaf matches the reference AES keyed-PRF.
        assert_eq!(
            new_leaf,
            expected_leaf(&leaf_key, counter, lb),
            "step {step}: keyed leaf != AES(key, counter)"
        );

        // access (main path), threading the begin's new_leaf.
        let db = cfg.data_bits;
        let eb = cfg.entry_bits();
        let n_path = cfg.path_entries();
        let path = tree.read_path(old_leaf);
        let mut acc_in = stash_bits.clone();
        acc_in.extend(flatten_path(&path, &cfg));
        acc_in.extend(enc(addr, cfg.addr_bits()));
        acc_in.push(w.is_some());
        acc_in.extend(enc(w.unwrap_or(0) as u64, db));
        acc_in.extend(enc(old_leaf, lb));
        acc_in.extend(enc(new_leaf, lb));
        acc_in.push(false);
        let acc_out = volar_fuzz::interpreter::biir::eval_biir(&access, &acc_in).expect("access evals");
        assert!(!acc_out[0], "step {step}: stash overflow");
        let rdata = dec(&acc_out[1..1 + db]) as u8;
        let new_path = unflatten_path(&acc_out[1 + db..1 + db + n_path * eb], &cfg);
        stash_bits = acc_out[1 + db + n_path * eb..].to_vec();
        tree.write_path(old_leaf, &new_path);

        // eviction pass.
        let evict_leaf = eviction_target(counter, cfg.num_leaves() as u64);
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let mut ev_in = stash_bits.clone();
            ev_in.extend(flatten_path(&epath, &cfg));
            ev_in.extend(enc(0, cfg.addr_bits()));
            ev_in.push(false);
            ev_in.extend(enc(0, db));
            ev_in.extend(enc(evict_leaf, lb));
            ev_in.extend(enc(0, lb));
            ev_in.push(true);
            let ev_out = volar_fuzz::interpreter::biir::eval_biir(&access, &ev_in).expect("evict evals");
            assert!(!ev_out[0], "step {step}: stash overflow (evict)");
            let nepath = unflatten_path(&ev_out[1 + db..1 + db + n_path * eb], &cfg);
            stash_bits = ev_out[1 + db + n_path * eb..].to_vec();
            tree.write_path(evict_leaf, &nepath);
        }
        counter += 1;

        // (b) the read/write matches the model.
        match w {
            Some(d) => model[addr as usize] = d,
            None => assert_eq!(rdata, model[addr as usize], "step {step}: read({addr})"),
        }
    }
}
