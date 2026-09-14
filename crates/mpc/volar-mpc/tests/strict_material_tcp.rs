//! Network test for role-private strict outputs. The conversion primitive is
//! used to move raw garbling-label bytes into/out of split-key encrypted ORAM:
//! garbler material returns only to the garbler, evaluator material only to the
//! evaluator. No loopback OT or shared in-process transport is involved.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::GateSchedule;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_chain::{ChainEvaluator, ChainFeed, ChainGarbler, ChainOut, ChainParty};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
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

#[test]
fn strict_private_outputs_cross_tcp_without_disclosing_to_other_role() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let schedule = private_material_schedule();
    let consume = consume_material_schedule();

    let garbler_schedule = schedule.clone();
    let garbler_consume = consume.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone session");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut chain =
            ChainGarbler::<N>::new(GlobalSecret::new(Array::clone_from_slice(&[0x42; 16])));
        chain
            .run_round::<D, _>(
                &garbler_schedule,
                &[ChainFeed::Garbler, ChainFeed::Eval],
                &[],
                &[true],
                &[ChainOut::GarblerMaterial(0), ChainOut::EvaluatorMaterial(0)],
                &mut session,
                &mut ot,
            )
            .expect("garbler material conversion");
        assert_eq!(chain.held_len(), 1, "only garbler base material persisted");
        chain
            .run_round::<D, _>(
                &garbler_consume,
                &[ChainFeed::Held(0), ChainFeed::Held(0)],
                &[],
                &[],
                &[ChainOut::Reveal],
                &mut session,
                &mut ot,
            )
            .expect("garbler consumes opaque material")
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone session");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut chain = ChainEvaluator::<N>::new();
    let evaluator = chain
        .run_round::<D, _>(
            &schedule,
            &[ChainFeed::Garbler, ChainFeed::Eval],
            &[],
            &[false],
            &[ChainOut::GarblerMaterial(0), ChainOut::EvaluatorMaterial(0)],
            &mut session,
            &mut ot,
        )
        .expect("evaluator material conversion");
    assert!(
        evaluator.is_empty(),
        "evaluator material does not decode to a bit"
    );
    assert_eq!(
        chain.held_len(),
        1,
        "only evaluator label material persisted"
    );
    let evaluator = chain
        .run_round::<D, _>(
            &consume,
            &[ChainFeed::Held(0), ChainFeed::Held(0)],
            &[],
            &[],
            &[ChainOut::Reveal],
            &mut session,
            &mut ot,
        )
        .expect("evaluator consumes opaque material");
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
