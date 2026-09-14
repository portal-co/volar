//! The migrated plaintext access step keeps stash/read data opaque while only
//! the physical write-back path and overflow status cross the transport.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::compile_schedule;
use volar_vc::oram_gadget::{OramGadgetConfig, build_access};
use volar_vc::oram_split::{
    EvaluatorOramState, GarblerOramState, SplitOramEvaluator, SplitOramGarbler,
};

type N = U16;
type D = Sha256;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: 4,
        levels: 3,
        bucket_size: 2,
        data_bits: 1,
        max_stash: 8,
        encrypted: false,
        tree_key_bits: 0,
        versioned_pads: false,
        version_bits: 0,
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
fn split_access_reveals_only_writeback_path_over_tcp() {
    let cfg = cfg();
    let schedule = compile_schedule(&build_access(&cfg)).expect("compile access");
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
    let stash_bases = bases(cfg.max_stash * cfg.entry_bits(), 0x10);
    let stash_labels = stash_bases
        .iter()
        .map(|base| secret.encode(base, false))
        .collect();
    let tape_bases = bases(cfg.addr_bits() + 1, 0x80);
    // address = 1 (LSB first), write-data = 1
    let tape_labels = vec![
        secret.encode(&tape_bases[0], true),
        secret.encode(&tape_bases[1], false),
        secret.encode(&tape_bases[2], true),
    ];
    let path_width = cfg.path_entries() * cfg.entry_bits();
    let path = vec![false; path_width];
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let gschedule = schedule.clone();
    let gcfg = cfg.clone();
    let gpath = path.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xD1_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut driver = SplitOramGarbler::new(
            secret,
            GarblerOramState::new(tape_bases, vec![], stash_bases),
        );
        let result = driver
            .run_access::<D>(
                &gcfg,
                &gschedule,
                &gpath,
                true,
                &[0, 1],
                Some(2),
                0,
                1,
                false,
                None,
                &[],
                &mut session,
                &mut ot,
            )
            .expect("garbler access");
        assert!(!result.overflow);
        result.new_path
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xD2_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut driver =
        SplitOramEvaluator::new(EvaluatorOramState::new(tape_labels, vec![], stash_labels));
    let result = driver
        .run_access::<D>(
            &cfg,
            &schedule,
            &path,
            true,
            &[0, 1],
            Some(2),
            0,
            1,
            false,
            None,
            &[],
            &mut session,
            &mut ot,
        )
        .expect("evaluator access");
    assert!(!result.overflow);
    let garbler_path = garbler.join().expect("garbler join");
    assert_eq!(result.new_path, garbler_path);
    assert!(
        result.new_path.iter().any(|bit| *bit),
        "write-back path is nonempty"
    );
}
