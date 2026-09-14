//! Atomic evaluator ciphertext-tree path handling.

use volar_vc::oram_ciphertext_tree::CiphertextTree;
use volar_vc::oram_gadget::OramGadgetConfig;

const Z: usize = 2;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: 4,
        levels: 3,
        bucket_size: Z,
        data_bits: 1,
        max_stash: 8,
        encrypted: true,
        tree_key_bits: 128,
        versioned_pads: false,
        version_bits: 0,
        encrypt_valid: false,
        keyed_leaf: false,
    }
}

#[test]
fn ciphertext_path_commit_is_single_use_and_epoch_bound() {
    let cfg = cfg();
    let mut tree = CiphertextTree::<Z>::new(&cfg).expect("tree");
    let open = tree.open(1).expect("open");
    assert_eq!(open.epoch(), 0);
    assert_eq!(open.bits().len(), tree.path_width());
    let mut writeback = open.bits().to_vec();
    writeback[0] = true;
    tree.commit(open, &writeback).expect("commit");
    assert_eq!(tree.epoch(), 1);

    let stale = tree.open(1).expect("fresh open");
    assert!(
        tree.commit(stale, &writeback[..writeback.len() - 1])
            .is_err()
    );
    assert_eq!(tree.epoch(), 1, "failed commit cannot mutate tree");

    let prepared = tree.prepare(1, 1).expect("prepare access");
    assert!(
        prepared.commit(0, &writeback).is_err(),
        "wrong paired epoch aborts"
    );
    assert_eq!(tree.epoch(), 1, "wrong paired epoch cannot mutate tree");
    let prepared = tree.prepare(1, 1).expect("retry prepare");
    prepared.commit(1, &writeback).expect("bound commit");
    assert_eq!(tree.epoch(), 2);
}

#[test]
fn tree_rejects_modes_without_split_key_formatter() {
    let mut cfg = cfg();
    cfg.encrypt_valid = true;
    assert!(CiphertextTree::<Z>::new(&cfg).is_err());
    cfg.encrypt_valid = false;
    cfg.versioned_pads = true;
    assert!(CiphertextTree::<Z>::new(&cfg).is_err());
}
