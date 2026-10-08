//! Opaque split outputs do not cross the role seam.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler, SplitOutput};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{Gate, GateSchedule, InputOwner};
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
fn opaque_output_stays_role_local_over_tcp() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let garbler_schedule = schedule();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0x0A_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
        let result = SplitGarbler::new(secret)
            .run_with_outputs::<D>(
                &garbler_schedule,
                vec![
                    Garble {
                        base: Array::clone_from_slice(&[0x11; 16]),
                    },
                    Garble {
                        base: Array::clone_from_slice(&[0x22; 16]),
                    },
                ],
                &[InputOwner::Garbler, InputOwner::Evaluator],
                &[],
                &[true],
                &[SplitOutput::Opaque],
                &mut session,
                &mut ot,
            )
            .expect("garbler run");
        assert!(result.revealed.is_empty());
        assert_eq!(result.output_bases.len(), 1);
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0x0B_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let result = SplitEvaluator::<N>::new()
        .run_with_outputs::<D>(
            &schedule(),
            &[InputOwner::Garbler, InputOwner::Evaluator],
            &[false],
            &[SplitOutput::Opaque],
            &mut session,
            &mut ot,
        )
        .expect("evaluator run");
    assert_eq!(result.len(), 1);
    garbler.join().expect("garbler join");
}

#[test]
fn evaluator_reveal_decodes_role_local_output() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let garbler_schedule = schedule();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0x0A_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
        let result = SplitGarbler::new(secret)
            .run_with_outputs::<D>(
                &garbler_schedule,
                vec![
                    Garble {
                        base: Array::clone_from_slice(&[0x11; 16]),
                    },
                    Garble {
                        base: Array::clone_from_slice(&[0x22; 16]),
                    },
                ],
                &[InputOwner::Garbler, InputOwner::Evaluator],
                &[],
                &[true],
                &[SplitOutput::EvaluatorReveal],
                &mut session,
                &mut ot,
            )
            .expect("garbler run");
        assert!(result.revealed.is_empty());
        assert_eq!(result.output_bases.len(), 1);
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0x0B_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let (labels, revealed) = SplitEvaluator::<N>::new()
        .run_with_outputs_and_reveals::<D>(
            &schedule(),
            &[InputOwner::Garbler, InputOwner::Evaluator],
            &[false],
            &[SplitOutput::EvaluatorReveal],
            &mut session,
            &mut ot,
        )
        .expect("evaluator run");
    assert_eq!(labels.len(), 1);
    assert_eq!(revealed, vec![true]);
    garbler.join().expect("garbler join");
}

#[test]
fn opaque_output_between_reveals_preserves_verdict_order() {
    let schedule = GateSchedule {
        num_inputs: 2,
        gates: vec![Gate::Xor(0, 1), Gate::And(0, 1)],
        output: 2,
        outputs: Some(vec![2, 0, 3]),
        storages: vec![],
        actions: vec![],
    };
    let outputs = [
        SplitOutput::Reveal,
        SplitOutput::Opaque,
        SplitOutput::Reveal,
    ];
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let garbler_schedule = schedule.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA1_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
        let result = SplitGarbler::new(secret)
            .run_with_outputs::<D>(
                &garbler_schedule,
                vec![
                    Garble {
                        base: Array::clone_from_slice(&[0x11; 16]),
                    },
                    Garble {
                        base: Array::clone_from_slice(&[0x22; 16]),
                    },
                ],
                &[InputOwner::Public, InputOwner::Public],
                &[true, false],
                &[],
                &outputs,
                &mut session,
                &mut ot,
            )
            .expect("garbler run");
        assert_eq!(result.revealed, vec![true, false]);
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xA2_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let (_labels, revealed) = SplitEvaluator::<N>::new()
        .run_with_outputs_and_reveals::<D>(
            &schedule,
            &[InputOwner::Public, InputOwner::Public],
            &[],
            &outputs,
            &mut session,
            &mut ot,
        )
        .expect("evaluator run");
    assert_eq!(revealed, vec![true, false]);
    garbler.join().expect("garbler join");
}
