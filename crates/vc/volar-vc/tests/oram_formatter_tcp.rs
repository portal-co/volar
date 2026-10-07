//! Split formatter protocol for encrypted-valid tree initialization.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::compile_schedule;
use volar_vc::oram_ciphertext_tree::CiphertextTree;
use volar_vc::oram_gadget::{OramGadgetConfig, build_tree_node_formatter};
use volar_vc::oram_split::{
    EvaluatorOramState, GarblerOramState, SplitOramEvaluator, SplitOramGarbler,
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
        versioned_pads: false,
        version_bits: 0,
        encrypt_valid: true,
        keyed_leaf: false,
    }
}

fn bases() -> Vec<Garble<N>> {
    (0..128)
        .map(|i| Garble {
            base: Array::clone_from_slice(&[i as u8; 16]),
        })
        .collect()
}

#[test]
fn split_formatter_initializes_every_encrypted_valid_node() {
    let cfg = cfg();
    let nodes: Vec<_> = (0..cfg.levels)
        .flat_map(|depth| (0..(1usize << depth)).map(move |prefix| (depth, prefix)))
        .collect();
    let schedules: Vec<_> = nodes
        .iter()
        .map(|&(depth, prefix)| {
            compile_schedule(&build_tree_node_formatter(&cfg, depth, prefix as u64, 0))
                .expect("format schedule")
        })
        .collect();
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let gbases = bases();
    let gschedules = schedules.clone();
    let gnodes = nodes.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xF1_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
        let mut driver =
            SplitOramGarbler::new(secret, GarblerOramState::new(vec![], vec![], vec![]));
        for schedule in gschedules {
            let out = driver
                .run_formatter::<D>(&schedule, &gbases, &[true; 64], &mut session, &mut ot)
                .expect("garbler formatter");
            assert_eq!(out.len(), cfg.bucket_size * cfg.entry_bits());
        }
        gnodes.len()
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xF2_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut driver = SplitOramEvaluator::<N>::new(EvaluatorOramState::new(vec![], vec![], vec![]));
    let mut tree = CiphertextTree::<Z>::new(&cfg).expect("tree");
    for ((depth, prefix), schedule) in nodes.iter().copied().zip(schedules) {
        let out = driver
            .run_formatter::<D>(&schedule, &[false; 64], &mut session, &mut ot)
            .expect("evaluator formatter");
        tree.install_formatted_node(depth, prefix, &out)
            .expect("install node");
    }
    assert!(tree.is_formatted());
    assert_eq!(garbler.join().expect("garbler join"), nodes.len());
    assert!(
        tree.open(0).is_ok(),
        "formatted encrypted-valid tree can open paths"
    );
}
