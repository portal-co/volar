//! The inverse role-private split output: evaluator labels cross only for a
//! garbler-local decode and the evaluator receives no recovered bit.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler, SplitInput, SplitOutput};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{Gate, GateSchedule};
use volar_spec::garble::{Garble, GlobalSecret};

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
fn garbler_reveal_keeps_decoded_bit_off_evaluator_verdict() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let g_schedule = schedule();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0x6A_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let runner =
            SplitGarbler::<N>::new(GlobalSecret::new(Array::clone_from_slice(&[0x42; 16])));
        let result = runner
            .run_with_state::<D>(
                &g_schedule,
                vec![
                    Garble {
                        base: Array::clone_from_slice(&[0x11; 16]),
                    },
                    Garble {
                        base: Array::clone_from_slice(&[0x22; 16]),
                    },
                ],
                &[SplitInput::Garbler, SplitInput::Evaluator],
                &[],
                &[true],
                &[SplitOutput::GarblerReveal],
                &mut session,
                &mut ot,
            )
            .expect("garbler run");
        assert_eq!(result.revealed, vec![true]);
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0x6B_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let runner = SplitEvaluator::<N>::new();
    let (_labels, revealed) = runner
        .run_with_state::<D>(
            &schedule(),
            &[SplitInput::Garbler, SplitInput::Evaluator],
            &[false],
            &[],
            &[SplitOutput::GarblerReveal],
            &mut session,
            &mut ot,
        )
        .expect("evaluator run");
    assert!(
        revealed.is_empty(),
        "evaluator does not learn a garbler-only result"
    );
    garbler.join().expect("garbler join");
}
