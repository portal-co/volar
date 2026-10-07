//! End-to-end WAT action test: a real WASM module importing an action extern
//! (`portal_net.echo`) lowers through the full pipeline (waffle → vaffle →
//! IRBlocks → boolar → `Gate::ActionBit` schedule) and runs two-party through
//! the strict-actions session, with the action executed by a host-side
//! `StrictActionHost`.
//!
//! Semantics: `f(x) = echo(x) if x != 0 else fallback`, i.e. the action host
//! is invoked exactly when the guard is nonzero and the all-ones fallback is
//! used otherwise.

#![cfg(feature = "std")]

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict::{
    StrictActionHost, eliminate_nots, garble_schedule_strict_dyn_full,
    run_evaluator_strict_actions, run_garbler_strict_actions,
};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{GateSchedule, InputOwner, MpcError};
use volar_spec::SpecRng;
use volar_spec::garble::{Garble, GlobalSecret};

type N = U16;
type D = Sha256;

const WAT: &str = r#"(module
  (import "portal_net" "echo" (func $echo (param i64 i64 i64) (result i64)))
  (func $f (export "f") (param $x i64) (result i64)
    (call $echo (local.get $x) (local.get $x) (i64.const -1))))"#;

fn lower() -> GateSchedule {
    let bytes = wat::parse_str(WAT).expect("wat assembles");
    let mut wasm = portal_pc_waffle_frontend::from_wasm_bytes(
        &bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("wasm parses");
    portal_pc_waffle_frontend::expand_all_funcs(&mut wasm).expect("expand");

    let mut target =
        volar_vaffle_target::VaffleTarget::with_pointer_width(vaffle::PointerWidth::Bits32);
    // Action import: 1 real arg, guard = first param, fallback = last param.
    let config = volar_vaffle_target::WaffleImportConfig::new().with_action(
        "portal_net.echo",
        "net_echo",
        1,
    );
    let errors = volar_vaffle_target::lower_waffle_module(&wasm, &mut target, &config);
    assert!(errors.is_empty(), "lowering errors: {errors:?}");

    let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);
    let ir_circuit = volar_ir_passes::unroll_ir_everything(&blocks, &types).expect("unroll");
    assert!(ir_circuit.is_circuit(), "straight-line guest unrolls");

    let boolar = volar_ir_passes::lower_ir_to_boolar(&ir_circuit, &types);
    volar_vc::compile_schedule(&boolar).expect("compile_schedule")
}

/// Echo host: returns the args unchanged; records invocations.
struct EchoHost {
    calls: usize,
}

impl StrictActionHost for EchoHost {
    fn action(&mut self, name: &str, args: &[bool]) -> Result<Vec<bool>, MpcError> {
        assert_eq!(name, "net_echo");
        self.calls += 1;
        Ok(args.to_vec())
    }
}

fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: Array::clone_from_slice(&[seed; 16]),
    }
}

/// Reference semantics.
fn reference(x: u64, calls: &mut usize) -> u64 {
    if x != 0 {
        *calls += 1;
        x
    } else {
        u64::MAX
    }
}

fn run(x: u64) -> (u64, usize) {
    let sched = lower();
    let elim = eliminate_nots(&sched).expect("eliminate");
    assert_eq!(elim.schedule.actions.len(), 1);
    let n_in = sched.num_inputs;
    // The entry's 64-bit arg plus scaffold zeros; all evaluator-owned (the
    // client's input).
    let partition = vec![InputOwner::Evaluator; n_in];
    let mut eval_bits = vec![false; n_in];
    for i in 0..64 {
        eval_bits[i] = (x >> i) & 1 == 1;
    }

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x5Au8; 16]));
    let input_labels: Vec<Garble<N>> = (0..n_in).map(|i| det_label((i % 251) as u8 + 3)).collect();
    let full =
        garble_schedule_strict_dyn_full::<N, D>(&elim, secret, input_labels).expect("garble");

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    let g_sched = elim.schedule.clone();
    let g_partition = partition.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        run_garbler_strict_actions::<N, D, _>(
            &full,
            &elim,
            &g_partition,
            &[],
            &[],
            &mut session,
            &mut ot,
        )
    });

    // GRAM drivers for the schedule's storage spaces (the call-ABI spill
    // scaffold), one per space, over small ORAM trees.
    let spec = g_sched.storages[0].clone();
    let mut tree = volar_oram::OramTree::<4, 8>::new(spec.levels);
    let drive_secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x29u8; 16]));
    let mut drive: volar_vc::GramEvalDrive<D, N, 4, 8> = volar_vc::GramEvalDrive::new(
        &drive_secret,
        &mut tree,
        spec.levels,
        spec.num_cells,
        0x5EED,
    );

    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut host = EchoHost { calls: 0 };
    let mut gram: [&mut dyn volar_mpc::GramDrive<N>; 1] = [&mut drive];
    let eval_out = run_evaluator_strict_actions::<N, D, _>(
        &g_sched,
        &partition,
        &eval_bits,
        &mut session,
        &mut ot,
        &mut host,
        &mut gram,
    )
    .expect("evaluator run");

    let garb_out = garbler.join().expect("garbler join").expect("garbler run");
    assert_eq!(eval_out, garb_out);
    // Decode the 64 result bits (LSB-first) into a u64.
    let mut out = 0u64;
    for (i, b) in eval_out.iter().take(64).enumerate() {
        if *b {
            out |= 1 << i;
        }
    }
    (out, host.calls)
}

#[test]
fn wat_action_end_to_end() {
    for x in [0u64, 1, 42, u64::MAX] {
        let mut want_calls = 0;
        let want = reference(x, &mut want_calls);
        let (got, calls) = run(x);
        assert_eq!(got, want, "x={x:#x}");
        assert_eq!(calls, want_calls, "host invocation count, x={x:#x}");
    }
}
