//! Threaded split state stays in separate role-local vectors across rounds.

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
fn held_input_reuses_split_opaque_output_over_tcp() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let first = xor_schedule();
    let second = xor_schedule();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0x5A_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let runner =
            SplitGarbler::new(GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16])));
        let first = runner
            .run_with_state::<D>(
                &first,
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
                &[SplitOutput::Opaque],
                &mut session,
                &mut ot,
            )
            .expect("opaque round");
        let second = runner
            .run_with_state::<D>(
                &second,
                vec![
                    first.output_bases[0].clone(),
                    Garble {
                        base: Array::clone_from_slice(&[0x33; 16]),
                    },
                ],
                &[SplitInput::Held, SplitInput::Public],
                &[false],
                &[],
                &[SplitOutput::Reveal],
                &mut session,
                &mut ot,
            )
            .expect("held round");
        assert_eq!(second.revealed, vec![true]);
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0x5B_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let runner = SplitEvaluator::<N>::new();
    let (first, _revealed) = runner
        .run_with_state::<D>(
            &xor_schedule(),
            &[SplitInput::Garbler, SplitInput::Evaluator],
            &[false],
            &[],
            &[SplitOutput::Opaque],
            &mut session,
            &mut ot,
        )
        .expect("opaque round");
    let (second, revealed) = runner
        .run_with_state::<D>(
            &xor_schedule(),
            &[SplitInput::Held, SplitInput::Public],
            &[],
            &first,
            &[SplitOutput::Reveal],
            &mut session,
            &mut ot,
        )
        .expect("held round");
    assert_eq!(second.len(), 1);
    assert_eq!(revealed, vec![true]);
    garbler.join().expect("garbler join");
}
