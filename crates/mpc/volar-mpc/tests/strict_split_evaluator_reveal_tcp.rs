//! A split result can be decoded only by the evaluator without transmitting
//! its active output label to the garbler.

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

fn xor_schedule() -> GateSchedule {
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
fn evaluator_reveal_keeps_active_output_label_off_garbler_wire() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let schedule = xor_schedule();
    let garbler_schedule = schedule.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xEA11);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let runner =
            SplitGarbler::<N>::new(GlobalSecret::new(Array::clone_from_slice(&[0x42; 16])));
        let result = runner
            .run_with_state::<D>(
                &garbler_schedule,
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
                &[SplitOutput::EvaluatorReveal],
                &mut session,
                &mut ot,
            )
            .expect("garbler run");
        assert!(
            result.revealed.is_empty(),
            "garbler never decodes evaluator-only output"
        );
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xEA12);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let runner = SplitEvaluator::<N>::new();
    let (_labels, revealed) = runner
        .run_with_state::<D>(
            &schedule,
            &[SplitInput::Garbler, SplitInput::Evaluator],
            &[false],
            &[],
            &[SplitOutput::EvaluatorReveal],
            &mut session,
            &mut ot,
        )
        .expect("evaluator run");
    assert_eq!(revealed, vec![true]);
    garbler.join().expect("garbler join");
}
