//! First migrated ORAM stage: a tape compute segment threads solely through
//! role-local split state over TCP and real OT.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::GateSchedule;
use volar_mpc::ot::SeedRng;
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::oram_split::{
    EvaluatorOramState, GarblerOramState, SplitOramEvaluator, SplitOramGarbler,
};

type N = U16;
type D = Sha256;

fn tape_identity() -> GateSchedule {
    GateSchedule {
        num_inputs: 2,
        gates: vec![],
        output: 0,
        outputs: Some(vec![0, 1]),
        storages: vec![],
        actions: vec![],
    }
}

#[test]
fn split_oram_compute_threads_role_local_tape_over_tcp() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
    let bases = vec![
        Garble {
            base: Array::clone_from_slice(&[0x11; 16]),
        },
        Garble {
            base: Array::clone_from_slice(&[0x22; 16]),
        },
    ];
    let labels = vec![
        secret.encode(&bases[0], true),
        secret.encode(&bases[1], false),
    ];
    let garbler_bases = bases.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xAA_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut driver =
            SplitOramGarbler::new(secret, GarblerOramState::new(garbler_bases, vec![], vec![]));
        driver
            .run_compute::<D>(&tape_identity(), &mut session, &mut ot)
            .expect("garbler compute");
        driver.state().tape().to_vec()
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xBB_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut driver = SplitOramEvaluator::new(EvaluatorOramState::new(labels, vec![], vec![]));
    driver
        .run_compute::<D>(&tape_identity(), &mut session, &mut ot)
        .expect("evaluator compute");
    let labels = driver.state().tape().to_vec();
    let bases = garbler.join().expect("garbler join");
    assert!(labels[0].open(&bases[0])[0] & 1 != 0);
    assert!(!(labels[1].open(&bases[1])[0] & 1 != 0));
}
