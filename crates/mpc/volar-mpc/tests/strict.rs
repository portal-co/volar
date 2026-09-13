// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Strict-session tests: the verdict is garbler-authenticated (the evaluator
//! returns output *labels*; the garbler decodes against private bases), and
//! the evaluator never receives the free-XOR delta (the schedule is
//! Not/One-free via `eliminate_nots`, so evaluation needs no `one_wire`).

use hybrid_array::{Array, typenum::U16};
use sha2::Sha256;
use volar_mpc::strict::{
    decode_output_label, eliminate_nots, garble_schedule_strict_dyn, run_evaluator_strict,
    run_garbler_strict,
};
use volar_mpc::{Gate, GateSchedule, InputOwner};
use volar_spec::garble::{Garble, GlobalSecret};

extern crate alloc;

type N = U16;
type D = Sha256;

fn det_bytes(seed: u8) -> Array<u8, N> {
    let mut a = Array::<u8, N>::default();
    for (i, b) in a.iter_mut().enumerate() {
        *b = (i as u8).wrapping_mul(37).wrapping_add(seed);
    }
    a[0] |= 1;
    a
}

fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: det_bytes(seed),
    }
}

/// Concrete semantics of a (possibly Not/One-carrying) schedule.
fn eval_concrete_multi(schedule: &GateSchedule, inputs: &[bool]) -> Vec<bool> {
    let mut wires: Vec<bool> = inputs.to_vec();
    for gate in &schedule.gates {
        let out = match *gate {
            Gate::Zero => false,
            Gate::One => true,
            Gate::Xor(a, b) => wires[a] ^ wires[b],
            Gate::And(a, b) => wires[a] & wires[b],
            Gate::Not(a) => !wires[a],
            Gate::StorageRead { .. } | Gate::StorageWrite { .. } => {
                panic!("concrete cross-check is boolean-only")
            }
            Gate::ActionBit { .. } => {
                panic!("concrete cross-check is boolean-only")
            }
        };
        wires.push(out);
    }
    schedule.output_wires().iter().map(|&w| wires[w]).collect()
}

/// A Not/One-heavy multi-output circuit:
///   in0=a in1=b in2=c in3=d
///   w4  = And(a, b)
///   w5  = Not(w4)             = !(a&b)
///   w6  = One
///   w7  = Xor(w5, w6)         = a&b
///   w8  = And(w5, c)          = !(a&b) & c
///   w9  = Xor(w7, w8)
///   w10 = Not(w9)
///   w11 = Xor(w10, d)
///   outputs = [w9, w10, w11, w4]
fn notty_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 4,
        gates: alloc::vec![
            Gate::And(0, 1),
            Gate::Not(4),
            Gate::One,
            Gate::Xor(5, 6),
            Gate::And(5, 2),
            Gate::Xor(7, 8),
            Gate::Not(9),
            Gate::Xor(10, 3),
        ],
        output: 9,
        outputs: Some(alloc::vec![9, 10, 11, 4]),
        actions: Vec::new(),
        storages: alloc::vec![],
    }
}

#[test]
fn eliminate_nots_shape() {
    let elim = eliminate_nots(&notty_schedule()).expect("eliminate");
    assert!(
        elim.schedule
            .gates
            .iter()
            .all(|g| !matches!(g, Gate::Not(_) | Gate::One)),
        "eliminated schedule is Not/One-free: {:?}",
        elim.schedule.gates
    );
    assert_eq!(elim.and_input_polarity.len(), elim.schedule.and_count());
    assert_eq!(
        elim.output_polarity.len(),
        elim.schedule.output_wires().len()
    );
    // The `One` gate became a `Zero` gate; some downstream alias carries a
    // flip (the circuit has complemented outputs).
    assert!(elim.output_polarity.iter().any(|&p| p));
}

/// In-process strict evaluation: garble the eliminated schedule, evaluate
/// with no delta anywhere in sight, decode with polarity. Matches concrete
/// semantics of the original (Not/One-carrying) schedule for all inputs.
#[test]
fn strict_in_process_matches_concrete_all_inputs() {
    let schedule = notty_schedule();
    let elim = eliminate_nots(&schedule).expect("eliminate");
    for bits in 0..16u8 {
        let inputs: Vec<bool> = (0..4).map(|i| (bits >> i) & 1 == 1).collect();
        let secret = GlobalSecret::<N>::new(det_bytes(0x5A ^ bits));
        let input_labels: Vec<Garble<N>> = (0..4).map(|i| det_label(i as u8 + 1)).collect();
        let exec = garble_schedule_strict_dyn::<N, D>(&elim, secret.clone(), input_labels.clone())
            .expect("garble");
        // Evaluator view: encoded input labels + tables; no one_wire/delta.
        let eval_inputs: Vec<volar_spec::garble::Eval<N>> = input_labels
            .iter()
            .zip(&inputs)
            .map(|(l, &b)| secret.encode(l, b))
            .collect();
        let setup = volar_mpc::DynEvalSetup {
            one_wire: volar_spec::garble::Eval::zero(),
            tables: exec.circuit.tables.clone(),
            output_label: Garble::zero(),
        };
        let out_labels = volar_mpc::DynGarbledExec::<N>::eval_labels_multi::<D>(
            &setup,
            &elim.schedule,
            &eval_inputs,
        )
        .expect("eval");
        // Garbler decodes against private bases + per-output polarity.
        let got: Vec<bool> = out_labels
            .iter()
            .zip(&exec.output_labels)
            .zip(&elim.output_polarity)
            .map(|((l, base), &p)| {
                decode_output_label(&secret, base, p, &l.target)
                    .expect("label matches a valid encoding")
            })
            .collect();
        let want = eval_concrete_multi(&schedule, &inputs);
        assert_eq!(got, want, "inputs {inputs:?}");
    }
}

/// Full strict session over a TCP loopback, honest both parties.
#[cfg(feature = "std")]
#[test]
fn strict_session_tcp_honest() {
    use volar_mpc::ot::SeedRng;
    use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};

    let schedule = notty_schedule();
    let elim = eliminate_nots(&schedule).expect("eliminate");
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    let inputs = [true, false, true, true]; // a, b, c, d
    let want = eval_concrete_multi(&schedule, &inputs);

    let secret = GlobalSecret::<N>::new(det_bytes(0x77));
    let input_labels: Vec<Garble<N>> = (0..4).map(|i| det_label(i as u8 + 11)).collect();
    let exec = garble_schedule_strict_dyn::<N, D>(&elim, secret, input_labels).expect("garble");

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    let g_elim = elim.schedule.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let public = [true]; // a
        let garbler_bits = [false]; // b
        run_garbler_strict::<N, D, _>(
            &exec,
            &elim,
            &partition,
            &public,
            &garbler_bits,
            &mut session,
            &mut ot,
        )
    });

    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let evaluator_bits = [true, true]; // c, d
    let eval_out = run_evaluator_strict::<N, D, _>(
        &g_elim,
        &partition,
        &evaluator_bits,
        &mut session,
        &mut ot,
    )
    .expect("evaluator run");

    let garb_out = garbler.join().expect("garbler join").expect("garbler run");
    assert_eq!(eval_out, want, "evaluator sees the authenticated verdict");
    assert_eq!(garb_out, want, "garbler-decoded verdict matches concrete");
}

/// A cheating evaluator who never evaluates: it fabricates an output label
/// and must be caught by the garbler's exact-match decode.
#[cfg(feature = "std")]
#[test]
fn strict_session_tcp_forged_output_rejected() {
    use volar_mpc::ot::SeedRng;
    use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
    use volar_mpc::{OtChannel, SessionFrame, Transport};

    let schedule = notty_schedule();
    let elim = eliminate_nots(&schedule).expect("eliminate");
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];

    let secret = GlobalSecret::<N>::new(det_bytes(0x42));
    let input_labels: Vec<Garble<N>> = (0..4).map(|i| det_label(i as u8 + 3)).collect();
    let exec = garble_schedule_strict_dyn::<N, D>(&elim, secret, input_labels).expect("garble");

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    let g_elim = elim.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        run_garbler_strict::<N, D, _>(
            &exec,
            &g_elim,
            &partition,
            &[true],
            &[false],
            &mut session,
            &mut ot,
        )
    });

    // Cheating evaluator: run the honest setup/OT intake (so the protocol
    // order holds), then send fabricated output labels.
    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);

    // Consume setup + owned inputs.
    let _setup = SessionFrame::decode(&session.recv()).expect("setup frame");
    let _owned = SessionFrame::decode(&session.recv()).expect("owned frame");
    // Complete the OTs (choice bits arbitrary).
    for _ in 0..2 {
        let _: Array<u8, N> = ot.receive(false);
    }
    // Fabricate: all-zero labels (never valid encodings of either value).
    let forged: Vec<Vec<u8>> = (0..elim.schedule.output_wires().len())
        .map(|_| alloc::vec![0u8; 16])
        .collect();
    session.send(&SessionFrame::OutputLabels(forged).encode());

    let garb = garbler.join().expect("garbler join");
    assert!(
        matches!(garb, Err(volar_mpc::MpcError::DecodeFailure)),
        "forged output labels are rejected: {garb:?}"
    );
}
