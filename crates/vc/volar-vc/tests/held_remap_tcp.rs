//! A durable-restored held label can be rebound to a fresh opaque wire base.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_vc::held_remap::{DeferredLabelRemapPlan, deferred_remap_schedule};

type N = U16;
type D = Sha256;

#[test]
fn deferred_remap_rebases_opaque_label_without_decoding_over_tcp() {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let schedule = deferred_remap_schedule(1);
    let plan = DeferredLabelRemapPlan::one_bit();
    let garbler_schedule = schedule.clone();
    let garbler_inputs = plan.inputs.clone();
    let garbler_public_bits = plan.public_bits.clone();
    let garbler_outputs = plan.outputs.clone();

    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xD3FE_0001);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
        let held_base = Garble {
            base: Array::clone_from_slice(&[0x11; 16]),
        };
        let fresh_zero_base = Garble {
            base: Array::clone_from_slice(&[0x22; 16]),
        };
        let result = SplitGarbler::new(secret)
            .run_with_state::<D>(
                &garbler_schedule,
                vec![held_base.clone(), fresh_zero_base],
                &garbler_inputs,
                &garbler_public_bits,
                &[],
                &garbler_outputs,
                &mut session,
                &mut ot,
            )
            .expect("garbler remap");
        assert!(result.revealed.is_empty());
        assert_ne!(result.output_bases[0].base, held_base.base);
        result.output_bases[0].clone()
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xD3FE_0002);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
    let held_base = Garble {
        base: Array::clone_from_slice(&[0x11; 16]),
    };
    // The evaluator retains only the active label, not the logical bit at the
    // remap seam. This test chooses `true` solely to construct that label.
    let held_label: Eval<N> = secret.encode(&held_base, true);
    let (outputs, revealed) = SplitEvaluator::<N>::new()
        .run_with_state::<D>(
            &schedule,
            &plan.inputs,
            &[],
            &[held_label],
            &plan.outputs,
            &mut session,
            &mut ot,
        )
        .expect("evaluator remap");
    assert!(revealed.is_empty());
    let fresh_base = garbler.join().expect("garbler join");
    assert_eq!(secret.encode(&fresh_base, true).target, outputs[0].target);
}
