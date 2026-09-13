//! P4b socket test: a real WAT guest drives `K` sequential send/recv rounds
//! through the strict-actions session over a REAL loopback TCP socket — the
//! first network-socket-actions end-to-end run and the first
//! significant-action-count test of the pipeline.
//!
//! The guest: `connect(port)`, then K rounds of `send(b_i); (st, b') =
//! recv(); acc += b'`, returning `acc`. The server flips each byte
//! (`b ^ 0xFF`).

#![cfg(feature = "std")]

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::net::SocketHost;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict::{
    eliminate_nots, garble_schedule_strict_dyn_full, run_evaluator_strict_actions,
    run_garbler_strict_actions,
};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_mpc::{GateSchedule, InputOwner};
use volar_spec::garble::{Garble, GlobalSecret};

type N = U16;
type D = Sha256;

fn guest_wat(k: usize) -> String {
    let mut body = String::new();
    // connect(guard=1, port=$port, fallback=0); drop status.
    body.push_str("(call $connect (i64.const 1) (local.get $port) (i64.const 0)) drop\n");
    for i in 0..k {
        let b = (i * 37 + 11) & 0xFF;
        // send(1, b, 0); drop status.
        body.push_str(&format!(
            "(call $send (i64.const 1) (i64.const {b}) (i64.const 0)) drop\n"
        ));
        // (status, byte) = recv(1, 0, 0); acc += byte.
        body.push_str("(call $recv (i64.const 1) (i64.const 0) (i64.const 0))\n");
        body.push_str("(local.set $byte) (local.set $st)\n");
        body.push_str("(local.set $acc (i64.add (local.get $acc) (local.get $byte)))\n");
    }
    format!(
        "(module\n  {}\n  (func $f (export \"f\") (param $port i64) (result i64)\n    (local $acc i64) (local $st i64) (local $byte i64)\n    {body}    (local.get $acc)))",
        volar_mpc::net::socket_imports_wat(),
    )
}

fn lower(k: usize) -> GateSchedule {
    let wat = guest_wat(k);
    let bytes = wat::parse_str(&wat).expect("wat assembles");
    let mut wasm = portal_pc_waffle_frontend::from_wasm_bytes(
        &bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("wasm parses");
    portal_pc_waffle_frontend::expand_all_funcs(&mut wasm).expect("expand");

    let mut target =
        volar_vaffle_target::VaffleTarget::with_pointer_width(vaffle::PointerWidth::Bits32);
    let mut config = volar_vaffle_target::WaffleImportConfig::new();
    for (waffle_name, action_name, n_args) in volar_mpc::net::socket_import_specs() {
        config = config.with_action(waffle_name, action_name, n_args);
    }
    let errors = volar_vaffle_target::lower_waffle_module(&wasm, &mut target, &config);
    assert!(errors.is_empty(), "lowering errors: {errors:?}");

    let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);
    let ir_circuit = volar_ir_passes::unroll_ir_everything(&blocks, &types).expect("unroll");
    assert!(ir_circuit.is_circuit(), "straight-line guest unrolls");
    let boolar = volar_ir_passes::lower_ir_to_boolar(&ir_circuit, &types);
    volar_vc::compile_schedule(&boolar).expect("compile_schedule")
}

/// Reference: the expected accumulator after `k` rounds of `b ^ 0xFF`.
fn reference(k: usize) -> u64 {
    (0..k).map(|i| (((i * 37 + 11) & 0xFF) ^ 0xFF) as u64).sum()
}

/// The test TCP server: read one byte, write back its complement, K times.
fn serve(listener: std::net::TcpListener, k: usize) {
    use std::io::{Read, Write};
    let (mut s, _) = listener.accept().expect("accept");
    let _ = s.set_nodelay(true);
    for _ in 0..k {
        let mut b = [0u8; 1];
        s.read_exact(&mut b).expect("read");
        s.write_all(&[b[0] ^ 0xFF]).expect("write");
        s.flush().expect("flush");
    }
}

fn run(k: usize) {
    let sched = lower(k);
    assert_eq!(
        sched.actions.len(),
        2 * k + 1,
        "connect + k sends + k recvs"
    );
    let elim = eliminate_nots(&sched).expect("eliminate");
    let n_in = sched.num_inputs;
    let partition = vec![InputOwner::Evaluator; n_in];

    let server = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let port = server.local_addr().unwrap().port();
    let server_thread = std::thread::spawn(move || serve(server, k));

    let mut eval_bits = vec![false; n_in];
    for i in 0..64 {
        eval_bits[i] = ((port as u64) >> i) & 1 == 1;
    }

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x5Au8; 16]));
    let input_labels: Vec<Garble<N>> = (0..n_in)
        .map(|i| Garble {
            base: Array::clone_from_slice(&[((i * 7 + 3) % 251) as u8; 16]),
        })
        .collect();
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

    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut host = SocketHost::new();
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
    server_thread.join().expect("server join");
    assert_eq!(eval_out, garb_out);
    let mut acc = 0u64;
    for (i, b) in eval_out.iter().take(64).enumerate() {
        if *b {
            acc |= 1 << i;
        }
    }
    assert_eq!(acc, reference(k), "k={k} rounds of send/recv accumulate");
}

/// Default-suite run: a handful of rounds.
#[test]
fn wat_socket_actions_end_to_end() {
    run(6);
}

/// The significant-action-count run: 97 action calls over the socket.
#[test]
#[ignore = "heavyweight: ~125s in debug (per-result-bit OTs)"]
fn wat_socket_actions_large_k() {
    run(48);
}
