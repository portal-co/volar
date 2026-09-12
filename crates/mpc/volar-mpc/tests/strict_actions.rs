//! The strict-actions session: a schedule carrying `Gate::ActionBit` runs
//! two-party with the action executed by an evaluator-side host. Per call:
//! the evaluator ships the arg labels to the garbler, the garbler exact-match
//! decodes them (a forged label aborts), returns the logical bits, the
//! evaluator's host runs the action, and each result bit is delivered by
//! 1-of-2 OT against the pinned action-result base.

#![cfg(feature = "std")]

use volar_mpc::ot::SeedRng;
use volar_mpc::strict::{
    StrictActionHost, StrictGarbledFull, eliminate_nots, garble_schedule_strict_dyn_full,
    run_evaluator_strict_actions, run_garbler_strict_actions,
};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{ActionSpec, Gate, GateSchedule, InputOwner, MpcError};
use volar_spec::SpecRng;
use volar_spec::garble::{Garble, GlobalSecret};

type N = hybrid_array::typenum::U16;
type D = sha2::Sha256;

fn det_bytes(seed: u8) -> [u8; 16] {
    [seed; 16]
}

fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: det_bytes(seed).into(),
    }
}

/// The circuit:
///   in0 = guard, in1..in3 payload (all evaluator-owned).
///   action 0 "reverse": args = in0..in3, 4 output bits (host reverses),
///     fallback = in0..in3 (used when guard = 0).
///   action 1 "pair": guard = One, args = [in0], 2 output bits
///     (host returns [a, !a]).
///   out0 = reverse[0] ^ pair[0]; out1 = reverse[3] ^ pair[1].
fn action_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 4,
        gates: vec![
            Gate::One,                              // wire 4
            Gate::ActionBit { call: 0, bit: 0 },    // wire 5
            Gate::ActionBit { call: 0, bit: 1 },    // wire 6
            Gate::ActionBit { call: 0, bit: 2 },    // wire 7
            Gate::ActionBit { call: 0, bit: 3 },    // wire 8
            Gate::ActionBit { call: 1, bit: 0 },    // wire 9
            Gate::ActionBit { call: 1, bit: 1 },    // wire 10
            Gate::Xor(5, 9),                        // wire 11 = out0
            Gate::Xor(8, 10),                       // wire 12 = out1
        ],
        output: 11,
        outputs: Some(vec![11, 12]),
        storages: Vec::new(),
        actions: vec![
            ActionSpec {
                name: "reverse".into(),
                guard: 0,
                arg_wires: vec![0, 1, 2, 3],
                fallback_wires: vec![0, 1, 2, 3],
                num_bits: 4,
                guard_polarity: false,
                arg_polarity: Vec::new(),
                fallback_polarity: Vec::new(),
            },
            ActionSpec {
                name: "pair".into(),
                guard: 4,
                arg_wires: vec![0],
                fallback_wires: vec![4, 4],
                num_bits: 2,
                guard_polarity: false,
                arg_polarity: Vec::new(),
                fallback_polarity: Vec::new(),
            },
        ],
    }
}

/// Reference semantics (concrete).
fn reference(inputs: &[bool; 4], calls: &mut Vec<String>) -> [bool; 2] {
    let [g, a, b, c] = *inputs;
    let r = if g {
        calls.push("reverse".into());
        [c, b, a, g]
    } else {
        [g, a, b, c]
    };
    calls.push("pair".into());
    let v = [g, !g];
    [r[0] ^ v[0], r[3] ^ v[1]]
}

struct Host {
    calls: Vec<String>,
}

impl StrictActionHost for Host {
    fn action(&mut self, name: &str, args: &[bool]) -> Result<Vec<bool>, MpcError> {
        self.calls.push(name.to_string());
        match name {
            "reverse" => {
                assert_eq!(args.len(), 4);
                Ok(args.iter().rev().copied().collect())
            }
            "pair" => {
                assert_eq!(args.len(), 1);
                Ok(vec![args[0], !args[0]])
            }
            other => panic!("unknown action {other}"),
        }
    }
}

fn run_case(inputs: [bool; 4]) -> ([bool; 2], Vec<String>) {
    let schedule = action_schedule();
    let elim = eliminate_nots(&schedule).expect("eliminate");
    let partition = [InputOwner::Evaluator; 4];

    let secret = GlobalSecret::<N>::new(det_bytes(0x77).into());
    let input_labels: Vec<Garble<N>> = (0..4).map(|i| det_label(i as u8 + 11)).collect();
    let full: StrictGarbledFull<N> =
        garble_schedule_strict_dyn_full::<N, D>(&elim, secret, input_labels).expect("garble");

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    let g_elim = elim.schedule.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        run_garbler_strict_actions::<N, D, _>(
            &full,
            &elim,
            &partition,
            &[],
            &[],
            &mut session,
            &mut ot,
        )
    });

    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut host = Host { calls: Vec::new() };
    let eval_out = run_evaluator_strict_actions::<N, D, _>(
        &g_elim,
        &partition,
        &inputs,
        &mut session,
        &mut ot,
        &mut host,
    )
    .expect("evaluator run");

    let garb_out = garbler.join().expect("garbler join").expect("garbler run");
    assert_eq!(eval_out, garb_out, "parties agree on the verdict");
    ([eval_out[0], eval_out[1]], host.calls)
}

#[test]
fn strict_actions_guard_true_invokes_host() {
    let inputs = [true, false, true, true];
    let mut want_calls = Vec::new();
    let want = reference(&inputs, &mut want_calls);
    let (got, calls) = run_case(inputs);
    assert_eq!(got, want);
    assert_eq!(calls, want_calls);
}

#[test]
fn strict_actions_guard_false_uses_fallback() {
    let inputs = [false, true, false, true];
    let mut want_calls = Vec::new();
    let want = reference(&inputs, &mut want_calls);
    let (got, calls) = run_case(inputs);
    assert_eq!(got, want);
    assert_eq!(
        calls, want_calls,
        "guarded-off action must not invoke the host"
    );
}
