//! End-to-end durable held-material round trip over split AES and network OT.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_chain::{
    ChainEvaluator, ChainFeed, ChainGarbler, ChainOut, ChainParty, ChainStoragePhase, MaterialRole,
    StorageOperation,
};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{Gate, GateSchedule};
use volar_spec::garble::GlobalSecret;
use volar_vc::oram_material_store::{EvaluatorSplitKeyMaterialStore, GarblerSplitKeyMaterialStore};

type N = U16;
type D = Sha256;

fn material_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 2,
        gates: vec![],
        output: 0,
        outputs: Some(vec![0, 1]),
        storages: vec![],
        actions: vec![],
    }
}

fn consume_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 2,
        gates: vec![Gate::Xor(0, 1)],
        output: 2,
        outputs: Some(vec![2]),
        storages: vec![],
        actions: vec![],
    }
}

fn flush_ops() -> [StorageOperation; 1] {
    [StorageOperation::Store {
        slot: 0,
        owner: MaterialRole::Both,
    }]
}

fn prefetch_ops() -> [StorageOperation; 1] {
    [StorageOperation::Load {
        slot: 0,
        owner: MaterialRole::Both,
    }]
}

#[test]
fn encrypted_role_local_material_survives_a_later_held_round_over_tcp() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let produce = material_schedule();
    let consume = consume_schedule();
    let g_produce = produce.clone();
    let g_consume = consume.clone();

    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xDA_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let store = GarblerSplitKeyMaterialStore::<N>::new::<D>(
            GlobalSecret::new(Array::clone_from_slice(&[0x42; 16])),
            [true; 64],
            b"material-store-test",
        )
        .expect("garbler store");
        let mut chain = ChainGarbler::with_held_store(
            GlobalSecret::new(Array::clone_from_slice(&[0x42; 16])),
            store,
        );
        chain
            .run_round::<D, _>(
                &g_produce,
                &[ChainFeed::Garbler, ChainFeed::Eval],
                &[],
                &[true],
                &[ChainOut::GarblerMaterial(0), ChainOut::EvaluatorMaterial(0)],
                &mut session,
                &mut ot,
            )
            .expect("stage garbler base");
        chain
            .run_storage_phase::<D>(&flush_ops(), &mut session, &mut ot)
            .expect("encrypt material");
        chain
            .run_storage_phase::<D>(&prefetch_ops(), &mut session, &mut ot)
            .expect("open material");
        chain
            .run_round::<D, _>(
                &g_consume,
                &[ChainFeed::Held(0), ChainFeed::Held(0)],
                &[],
                &[],
                &[ChainOut::Reveal],
                &mut session,
                &mut ot,
            )
            .expect("consume opened base")
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xDB_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let store = EvaluatorSplitKeyMaterialStore::<N>::new([false; 64]).expect("evaluator store");
    let mut chain = ChainEvaluator::with_held_store(store);
    chain
        .run_round::<D, _>(
            &produce,
            &[ChainFeed::Garbler, ChainFeed::Eval],
            &[],
            &[false],
            &[ChainOut::GarblerMaterial(0), ChainOut::EvaluatorMaterial(0)],
            &mut session,
            &mut ot,
        )
        .expect("stage evaluator label");
    chain
        .run_storage_phase::<D>(&flush_ops(), &mut session, &mut ot)
        .expect("encrypt material");
    chain
        .run_storage_phase::<D>(&prefetch_ops(), &mut session, &mut ot)
        .expect("open material");
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
        .expect("consume opened label");
    assert_eq!(evaluator, vec![false]);
    assert_eq!(garbler.join().expect("garbler join"), vec![false]);
}
