//! The strict-actions session: a schedule carrying `Gate::ActionBit` runs
//! two-party with the action executed by an evaluator-side host. Per call:
//! the evaluator ships the arg labels to the garbler, the garbler exact-match
//! decodes them (a forged label aborts), returns the logical bits, the
//! evaluator's host runs the action, and each result bit is delivered by
//! 1-of-2 OT against the pinned action-result base.

#![cfg(feature = "std")]

use volar_mpc::ot::{LoopbackOt, SeedRng};
use volar_mpc::strict::{
    StrictActionHost, StrictGarbledFull, decode_external_batch_action_reveal, eliminate_nots,
    garble_schedule_strict_dyn_full, offer_external_batch_action_result_labels,
    run_evaluator_strict_actions, run_garbler_strict_actions,
    run_garbler_strict_actions_garbler_host, validate_legacy_action_policy,
    validate_legacy_action_spec,
};
use volar_mpc::strict_cursor::StrictGateCursor;
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{
    ActionSpec, EvaluatorBatchExecutor, ExternalBatchAction, ExternalBatchActionHost,
    ExternalBatchFrame, ExternalBoundaryId, ExternalExecutor, ExternalRevealPolicy, Gate,
    GateSchedule, InputOwner, MpcError,
};
use volar_spec::garble::{Eval, Garble, GlobalSecret};

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
            Gate::One,                           // wire 4
            Gate::ActionBit { call: 0, bit: 0 }, // wire 5
            Gate::ActionBit { call: 0, bit: 1 }, // wire 6
            Gate::ActionBit { call: 0, bit: 2 }, // wire 7
            Gate::ActionBit { call: 0, bit: 3 }, // wire 8
            Gate::ActionBit { call: 1, bit: 0 }, // wire 9
            Gate::ActionBit { call: 1, bit: 1 }, // wire 10
            Gate::Xor(5, 9),                     // wire 11 = out0
            Gate::Xor(8, 10),                    // wire 12 = out1
        ],
        output: 11,
        outputs: Some(vec![11, 12]),
        storages: Vec::new(),
        actions: vec![
            ActionSpec {
                name: "reverse".into(),
                request_id: 0,
                action_ordinal: 0,
                execution: volar_mpc::ActionExecutionPolicy::legacy_evaluator(),
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
                request_id: 1,
                action_ordinal: 1,
                execution: volar_mpc::ActionExecutionPolicy::legacy_evaluator(),
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

impl ExternalBatchActionHost for Host {
    fn action(
        &mut self,
        registration: &ExternalBatchAction,
        args: &[bool],
    ) -> Result<Vec<bool>, MpcError> {
        StrictActionHost::action(self, &registration.name, args)
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
        &mut [],
    )
    .expect("evaluator run");

    let garb_out = garbler.join().expect("garbler join").expect("garbler run");
    assert_eq!(eval_out, garb_out, "parties agree on the verdict");
    ([eval_out[0], eval_out[1]], host.calls)
}

#[test]
fn batch_label_transport_binds_manifest_reveal_and_reinsertion() {
    let schedule = action_schedule();
    let elim = eliminate_nots(&schedule).unwrap();
    let secret = GlobalSecret::<N>::new(det_bytes(0x55).into());
    let inputs: Vec<Garble<N>> = (0..4).map(|index| det_label(20 + index)).collect();
    let full = garble_schedule_strict_dyn_full::<N, D>(&elim, secret, inputs).unwrap();
    let (manifest, binding) = full
        .external_action_manifest(ExternalBoundaryId(7), [1; 32], [2; 32])
        .unwrap();
    let logical_inputs = [true, false, true, true];
    let labels: Vec<Eval<N>> = full
        .exec
        .circuit
        .input_labels
        .iter()
        .zip(logical_inputs)
        .map(|(base, bit)| Eval {
            target: full.exec.circuit.secret.encode(base, bit).target,
        })
        .collect();
    let mut cursor = StrictGateCursor::new(&elim.schedule, &labels).unwrap();
    let reveal = cursor.action_batch_reveal(&manifest, binding, 0).unwrap();
    let clear = decode_external_batch_action_reveal(&full, &manifest, binding, 0, &reveal).unwrap();
    assert_eq!(
        clear,
        ExternalBatchFrame::ClearInputs {
            binding,
            request_id: 0,
            bits: vec![true, true, false, true, true, true, false, true, true],
        }
    );
    let mut ot = LoopbackOt::new();
    let result = ExternalBatchFrame::Result {
        binding,
        request_id: 0,
        bits: vec![true, true, false, true],
    };
    offer_external_batch_action_result_labels::<N, D>(
        &full, &manifest, binding, 0, &result, &mut ot,
    )
    .unwrap();
    cursor
        .reinsert_batch_action_result(&manifest, binding, 0, &result, &mut ot)
        .unwrap();
    assert_eq!(ot.pending(), 0);
}

#[test]
fn evaluator_batch_executor_drives_strict_label_reveal_and_reinsertion() {
    let schedule = action_schedule();
    let elim = eliminate_nots(&schedule).unwrap();
    let secret = GlobalSecret::<N>::new(det_bytes(0x41).into());
    let inputs: Vec<Garble<N>> = (0..4).map(|index| det_label(40 + index)).collect();
    let full = garble_schedule_strict_dyn_full::<N, D>(&elim, secret, inputs).unwrap();
    let manifest = volar_mpc::ExternalBatchManifest::from_actions(
        ExternalBoundaryId(9),
        &[full.exec.schedule.actions[0].clone()],
    )
    .unwrap();
    let binding = manifest.bind([9; 32], [10; 32]);
    let mut executor = EvaluatorBatchExecutor::new(
        manifest.clone(),
        binding,
        vec![ExternalBatchAction {
            request_id: 0,
            name: "reverse".into(),
            argument_bits: 4,
        }],
    )
    .unwrap();
    let clear_inputs = [true, false, true, true];
    let labels: Vec<Eval<N>> = full
        .exec
        .circuit
        .input_labels
        .iter()
        .zip(clear_inputs)
        .map(|(base, bit)| Eval {
            target: full.exec.circuit.secret.encode(base, bit).target,
        })
        .collect();
    let mut cursor = StrictGateCursor::new(&elim.schedule, &labels).unwrap();
    let reveal = cursor.action_batch_reveal(&manifest, binding, 0).unwrap();
    executor.accept_reveal(&reveal).unwrap();
    let clear = decode_external_batch_action_reveal(&full, &manifest, binding, 0, &reveal).unwrap();
    let mut host = Host { calls: Vec::new() };
    let result = executor.execute_clear_inputs(&clear, &mut host).unwrap();
    assert_eq!(host.calls, vec!["reverse"]);
    let mut ot = LoopbackOt::new();
    offer_external_batch_action_result_labels::<N, D>(
        &full, &manifest, binding, 0, &result, &mut ot,
    )
    .unwrap();
    cursor
        .reinsert_batch_action_result(&manifest, binding, 0, &result, &mut ot)
        .unwrap();
    executor
        .accept_reinserted(&ExternalBatchFrame::Reinserted {
            binding,
            request_id: 0,
        })
        .unwrap();
    assert!(executor.is_complete());
    assert_eq!(ot.pending(), 0);
}

fn run_garbler_executor_case(inputs: [bool; 4]) -> ([bool; 2], Vec<String>, Vec<String>) {
    let mut schedule = action_schedule();
    for action in &mut schedule.actions {
        action.execution.executor = ExternalExecutor::Garbler;
    }
    let elim = eliminate_nots(&schedule).expect("eliminate");
    let partition = [InputOwner::Evaluator; 4];
    let secret = GlobalSecret::<N>::new(det_bytes(0x67).into());
    let input_labels: Vec<Garble<N>> = (0..4).map(|i| det_label(i as u8 + 31)).collect();
    let full: StrictGarbledFull<N> =
        garble_schedule_strict_dyn_full::<N, D>(&elim, secret, input_labels).expect("garble");
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());
    let evaluator_schedule = elim.schedule.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA5A5);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut host = Host { calls: Vec::new() };
        let out = run_garbler_strict_actions_garbler_host::<N, D, _>(
            &full,
            &elim,
            &partition,
            &[],
            &[],
            &mut session,
            &mut ot,
            &mut host,
        );
        (out, host.calls)
    });
    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB5B5);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut evaluator_host = Host { calls: Vec::new() };
    let eval_out = run_evaluator_strict_actions::<N, D, _>(
        &evaluator_schedule,
        &partition,
        &inputs,
        &mut session,
        &mut ot,
        &mut evaluator_host,
        &mut [],
    )
    .expect("evaluator run");
    let (garbler_out, garbler_calls) = garbler.join().expect("garbler join");
    assert_eq!(eval_out, garbler_out.expect("garbler run"));
    (
        [eval_out[0], eval_out[1]],
        garbler_calls,
        evaluator_host.calls,
    )
}

#[test]
fn strict_garbler_executor_runs_host_over_batch_tcp_transport() {
    let inputs = [true, false, true, true];
    let mut want_calls = Vec::new();
    let want = reference(&inputs, &mut want_calls);
    let (got, garbler_calls, evaluator_calls) = run_garbler_executor_case(inputs);
    assert_eq!(got, want);
    assert_eq!(garbler_calls, want_calls);
    assert!(evaluator_calls.is_empty());
}

#[test]
fn strict_actions_reject_unimplemented_executor_or_reveal_policy() {
    let mut schedule = action_schedule();
    schedule.actions[0].execution.executor = ExternalExecutor::Garbler;
    assert_eq!(
        validate_legacy_action_policy(&schedule),
        Err(MpcError::UnsupportedExternalPolicy)
    );
    assert_eq!(
        validate_legacy_action_spec(&schedule.actions[0]),
        Err(MpcError::UnsupportedExternalPolicy)
    );
    schedule.actions[0].execution.executor = ExternalExecutor::Evaluator;
    schedule.actions[0].execution.reveal = ExternalRevealPolicy::ExecutorOnly;
    assert_eq!(
        validate_legacy_action_policy(&schedule),
        Err(MpcError::UnsupportedExternalPolicy)
    );
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
