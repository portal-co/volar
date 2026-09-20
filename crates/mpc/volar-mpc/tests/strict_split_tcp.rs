//! The lower-level split runner uses real TCP and OT without a combined-role
//! state or loopback OT.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{Gate, GateSchedule, InputOwner};
use volar_spec::garble::{Garble, GlobalSecret};

const N_BYTES: usize = 16;
type N = U16;
type D = Sha256;

fn schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 2,
        gates: vec![Gate::Xor(0, 1)],
        output: 2,
        outputs: Some(vec![2]),
        storages: vec![],
        actions: vec![],
    }
}

#[test]
fn split_roles_preserve_output_relation_over_tcp() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let schedule = schedule();
    let garbler_schedule = schedule.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone session");
        let mut rng = SeedRng::new(0x51_11_7);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; N_BYTES]));
        let runner = SplitGarbler::new(secret);
        runner
            .run::<D>(
                &garbler_schedule,
                vec![
                    Garble {
                        base: Array::clone_from_slice(&[0x11; N_BYTES]),
                    },
                    Garble {
                        base: Array::clone_from_slice(&[0x22; N_BYTES]),
                    },
                ],
                &[InputOwner::Garbler, InputOwner::Evaluator],
                &[],
                &[true],
                &mut session,
                &mut ot,
            )
            .expect("garbler split run")
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone session");
    let mut rng = SeedRng::new(0x51_11_8);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let labels = SplitEvaluator::<N>::new()
        .run::<D>(
            &schedule,
            &[InputOwner::Garbler, InputOwner::Evaluator],
            &[false],
            &mut session,
            &mut ot,
        )
        .expect("evaluator split run");
    let bases = garbler.join().expect("garbler join");
    assert!(labels[0].open(&bases[0])[0] & 1 != 0, "true XOR false");
}
