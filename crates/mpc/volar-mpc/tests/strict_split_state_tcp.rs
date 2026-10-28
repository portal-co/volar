//! Threaded split state stays in separate role-local vectors across scheduled
//! epochs and iterations.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir_common::{
    CommunicationGroup, EpochId, EpochManifest, IterationId, IterationManifest, ScheduleManifest,
    WorkerLane,
};
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler, SplitInput, SplitOutput};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{Gate, GateSchedule, MpcError, validate_gate_schedule_binding};
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

fn split_manifest() -> ScheduleManifest {
    let mut first = EpochManifest::new(EpochId(0), CommunicationGroup(0));
    first
        .iterations
        .push(IterationManifest::new(IterationId(0), 20, WorkerLane(0)));
    let mut second = EpochManifest::new(EpochId(1), CommunicationGroup(1));
    second
        .iterations
        .push(IterationManifest::new(IterationId(1), 21, WorkerLane(1)));
    let mut manifest = ScheduleManifest::new([3; 32], [4; 32], 8, 2, 2);
    manifest.epochs.push(first);
    manifest.epochs.push(second);
    manifest
}

#[test]
fn held_input_reuses_split_opaque_output_across_scheduled_epochs() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let manifest = split_manifest();
    let digest = [9; 32];
    let first = xor_schedule();
    let second = xor_schedule();
    let garbler_manifest = manifest.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0x5A_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let runner =
            SplitGarbler::new(GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16])));
        let mut opaque = None;
        let mut revealed = None;
        let executed = runner
            .drive_schedule(
                &garbler_manifest,
                45,
                digest,
                &mut session,
                &mut ot,
                |frame, runner, transport, ot| {
                    validate_gate_schedule_binding(&first, &garbler_manifest)
                        .map_err(|_| MpcError::MalformedSchedule)?;
                    match frame.iteration.expect("iteration frame").0 {
                        0 => {
                            opaque = Some(runner.run_with_state::<D>(
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
                                transport,
                                ot,
                            )?);
                        }
                        1 => {
                            validate_gate_schedule_binding(&second, &garbler_manifest)
                                .map_err(|_| MpcError::MalformedSchedule)?;
                            let opaque = opaque.as_ref().expect("first split round");
                            let result = runner.run_with_state::<D>(
                                &second,
                                vec![
                                    opaque.output_bases[0].clone(),
                                    Garble {
                                        base: Array::clone_from_slice(&[0x33; 16]),
                                    },
                                ],
                                &[SplitInput::Held, SplitInput::Public],
                                &[false],
                                &[],
                                &[SplitOutput::Reveal],
                                transport,
                                ot,
                            )?;
                            revealed = Some(result.revealed);
                        }
                        _ => return Err(MpcError::MalformedSchedule),
                    }
                    Ok(())
                },
            )
            .expect("scheduled garbler split rounds");
        assert_eq!(executed, 2);
        assert_eq!(revealed.expect("second split round"), vec![true]);
    });

    let first = xor_schedule();
    let second = xor_schedule();
    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0x5B_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let runner = SplitEvaluator::<N>::new();
    let mut held = None;
    let mut revealed = None;
    let executed = runner
        .drive_schedule(
            &manifest,
            45,
            digest,
            &mut session,
            &mut ot,
            |frame, runner, transport, ot| {
                match frame.iteration.expect("iteration frame").0 {
                    0 => {
                        validate_gate_schedule_binding(&first, &manifest)
                            .map_err(|_| MpcError::MalformedSchedule)?;
                        let (labels, _) = runner.run_with_state::<D>(
                            &first,
                            &[SplitInput::Garbler, SplitInput::Evaluator],
                            &[false],
                            &[],
                            &[SplitOutput::Opaque],
                            transport,
                            ot,
                        )?;
                        held = Some(labels);
                    }
                    1 => {
                        validate_gate_schedule_binding(&second, &manifest)
                            .map_err(|_| MpcError::MalformedSchedule)?;
                        let held_labels = held.as_ref().expect("first split round");
                        let (labels, bits) = runner.run_with_state::<D>(
                            &second,
                            &[SplitInput::Held, SplitInput::Public],
                            &[],
                            held_labels,
                            &[SplitOutput::Reveal],
                            transport,
                            ot,
                        )?;
                        assert_eq!(labels.len(), 1);
                        revealed = Some(bits);
                    }
                    _ => return Err(MpcError::MalformedSchedule),
                }
                Ok(())
            },
        )
        .expect("scheduled evaluator split rounds");
    assert_eq!(executed, 2);
    assert_eq!(revealed.expect("second split round"), vec![true]);
    garbler.join().expect("garbler join");
}
