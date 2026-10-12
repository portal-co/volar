//! Network test for role-private strict outputs. The conversion primitive is
//! used to move raw garbling-label bytes into/out of split-key encrypted ORAM:
//! each role persists only its local material and neither output path decodes a
//! Boolean value. The two rounds run inside a manifest-bound multi-epoch
//! lockstep schedule rather than relying on caller-side round ordering.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir_common::{
    CommunicationGroup, EpochId, EpochManifest, IterationId, IterationManifest, ScheduleManifest,
    WorkerLane,
};
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_chain::{ChainEvaluator, ChainFeed, ChainGarbler, ChainOut, ChainParty};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{GateSchedule, MpcError, validate_gate_schedule_binding};
use volar_spec::garble::GlobalSecret;

type N = U16;
type D = Sha256;

fn private_material_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 2,
        gates: vec![],
        output: 0,
        outputs: Some(vec![0, 1]),
        storages: vec![],
        actions: vec![],
    }
}

fn consume_material_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 2,
        gates: vec![volar_mpc::Gate::Xor(0, 1)],
        output: 2,
        outputs: Some(vec![2]),
        storages: vec![],
        actions: vec![],
    }
}

fn chain_manifest() -> ScheduleManifest {
    let mut first = EpochManifest::new(EpochId(0), CommunicationGroup(0));
    first
        .iterations
        .push(IterationManifest::new(IterationId(0), 30, WorkerLane(0)));
    let mut second = EpochManifest::new(EpochId(1), CommunicationGroup(1));
    second
        .iterations
        .push(IterationManifest::new(IterationId(1), 31, WorkerLane(1)));
    let mut manifest = ScheduleManifest::new([5; 32], [6; 32], 9, 2, 2);
    manifest.epochs.push(first);
    manifest.epochs.push(second);
    manifest
}

#[test]
fn strict_private_outputs_cross_scheduled_epochs_without_disclosure() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let manifest = chain_manifest();
    let digest = [11; 32];

    let garbler_manifest = manifest.clone();
    let garbler = thread::spawn(move || {
        let schedule = private_material_schedule();
        let consume = consume_material_schedule();
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone session");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut chain =
            ChainGarbler::<N>::new(GlobalSecret::new(Array::clone_from_slice(&[0x42; 16])));
        let mut consumed = None;
        let executed = chain
            .drive_schedule(
                &garbler_manifest,
                46,
                digest,
                &mut session,
                &mut ot,
                |frame, chain, transport, ot| {
                    match frame.iteration.expect("iteration frame").0 {
                        0 => {
                            validate_gate_schedule_binding(&schedule, &garbler_manifest)
                                .map_err(|_| MpcError::MalformedSchedule)?;
                            let revealed = chain.run_round::<D, _>(
                                &schedule,
                                &[ChainFeed::Garbler, ChainFeed::Eval],
                                &[],
                                &[true],
                                &[ChainOut::GarblerMaterial(0), ChainOut::EvaluatorMaterial(0)],
                                transport,
                                ot,
                            )?;
                            assert!(revealed.is_empty());
                            assert_eq!(chain.held_len(), 1, "only garbler base material persisted");
                        }
                        1 => {
                            validate_gate_schedule_binding(&consume, &garbler_manifest)
                                .map_err(|_| MpcError::MalformedSchedule)?;
                            consumed = Some(chain.run_round::<D, _>(
                                &consume,
                                &[ChainFeed::Held(0), ChainFeed::Held(0)],
                                &[],
                                &[],
                                &[ChainOut::Reveal],
                                transport,
                                ot,
                            )?);
                        }
                        _ => return Err(MpcError::MalformedSchedule),
                    }
                    Ok(())
                },
            )
            .expect("scheduled garbler chain");
        assert_eq!(executed, 2);
        consumed.expect("second scheduled chain round")
    });

    let schedule = private_material_schedule();
    let consume = consume_material_schedule();
    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone session");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut chain = ChainEvaluator::<N>::new();
    let mut consumed = None;
    let executed = chain
        .drive_schedule(
            &manifest,
            46,
            digest,
            &mut session,
            &mut ot,
            |frame, chain, transport, ot| {
                match frame.iteration.expect("iteration frame").0 {
                    0 => {
                        validate_gate_schedule_binding(&schedule, &manifest)
                            .map_err(|_| MpcError::MalformedSchedule)?;
                        let revealed = chain.run_round::<D, _>(
                            &schedule,
                            &[ChainFeed::Garbler, ChainFeed::Eval],
                            &[],
                            &[false],
                            &[ChainOut::GarblerMaterial(0), ChainOut::EvaluatorMaterial(0)],
                            transport,
                            ot,
                        )?;
                        assert!(
                            revealed.is_empty(),
                            "evaluator material does not decode to a bit"
                        );
                        assert_eq!(
                            chain.held_len(),
                            1,
                            "only evaluator label material persisted"
                        );
                    }
                    1 => {
                        validate_gate_schedule_binding(&consume, &manifest)
                            .map_err(|_| MpcError::MalformedSchedule)?;
                        consumed = Some(chain.run_round::<D, _>(
                            &consume,
                            &[ChainFeed::Held(0), ChainFeed::Held(0)],
                            &[],
                            &[],
                            &[ChainOut::Reveal],
                            transport,
                            ot,
                        )?);
                    }
                    _ => return Err(MpcError::MalformedSchedule),
                }
                Ok(())
            },
        )
        .expect("scheduled evaluator chain");
    assert_eq!(executed, 2);
    let evaluator = consumed.expect("second scheduled chain round");
    let garbler = garbler.join().expect("garbler join");

    assert_eq!(
        garbler,
        vec![false],
        "reused material preserves label relation"
    );
    assert_eq!(
        evaluator,
        vec![false],
        "reused material preserves label relation"
    );
}
