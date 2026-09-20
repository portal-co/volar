//! Atomic split-key encrypted access: role-local key drivers bind the strict
//! AES circuit to an evaluator-owned prepared ciphertext path over TCP.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::compile_schedule;
use volar_vc::oram_ciphertext_tree::CiphertextTree;
use volar_vc::oram_gadget::{OramGadgetConfig, build_access};
use volar_vc::oram_split::{
    EvaluatorEncryptedOramDriver, EvaluatorOramState, EvaluatorTreeKeyHalf,
    GarblerEncryptedOramDriver, GarblerOramState, GarblerTreeKeyHalf, SplitOramEvaluator,
    SplitOramGarbler,
};

type N = U16;
type D = Sha256;
const Z: usize = 2;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: 2,
        levels: 2,
        bucket_size: Z,
        data_bits: 1,
        max_stash: 6,
        encrypted: true,
        tree_key_bits: 128,
        versioned_pads: true,
        version_bits: 8,
        encrypt_valid: false,
        keyed_leaf: false,
    }
}

fn bases(count: usize, tag: u8) -> Vec<Garble<N>> {
    (0..count)
        .map(|i| Garble {
            base: Array::clone_from_slice(&[tag.wrapping_add(i as u8); 16]),
        })
        .collect()
}

#[test]
fn encrypted_access_commits_prepared_tree_and_advances_both_epochs() {
    let cfg = cfg();
    let schedule = compile_schedule(&build_access(&cfg)).expect("compile encrypted access");
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
    let stash_bases = bases(cfg.max_stash * cfg.entry_bits(), 0x10);
    let stash_labels = stash_bases
        .iter()
        .map(|base| secret.encode(base, false))
        .collect();
    let tape_bases = bases(2, 0x80); // address=1, write value=1
    let tape_labels = vec![
        secret.encode(&tape_bases[0], true),
        secret.encode(&tape_bases[1], true),
    ];

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let gschedule = schedule.clone();
    let gcfg = cfg.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xE1_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let split = SplitOramGarbler::new(
            secret,
            GarblerOramState::new(tape_bases, vec![], stash_bases),
        );
        let mut driver = GarblerEncryptedOramDriver::new::<D>(
            split,
            GarblerTreeKeyHalf::new([true; 64]),
            b"garbler input base seed",
        )
        .expect("garbler encrypted driver");
        let result = driver
            .run_access::<D>(
                &gcfg,
                &gschedule,
                true,
                &[0],
                Some(1),
                0,
                1,
                false,
                0,
                &[0, 0],
                &mut session,
                &mut ot,
            )
            .expect("garbler atomic access");
        assert!(!result.overflow);
        assert_eq!(driver.epoch(), 1);
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xE2_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let split = SplitOramEvaluator::new(EvaluatorOramState::new(tape_labels, vec![], stash_labels));
    let mut driver =
        EvaluatorEncryptedOramDriver::new(split, EvaluatorTreeKeyHalf::new([false; 64]));
    let mut tree = CiphertextTree::<Z>::new(&cfg).expect("ciphertext tree");
    let prepared = tree.prepare(0, 0).expect("prepare tree path");
    let result = driver
        .run_access::<D, Z>(
            &cfg,
            &schedule,
            prepared,
            true,
            &[0],
            Some(1),
            0,
            1,
            false,
            0,
            &mut session,
            &mut ot,
        )
        .expect("evaluator atomic access");
    assert!(!result.overflow);
    assert_eq!(driver.epoch(), 1);
    assert_eq!(tree.epoch(), 1);
    assert_eq!(tree.path_versions(0).expect("versions"), vec![1, 1]);
    garbler.join().expect("garbler join");
}
