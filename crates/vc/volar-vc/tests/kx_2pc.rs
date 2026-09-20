//! The two-party X25519 key exchange of the live TLS driver: the ladder
//! step / final-cswap circuits concretely (fast), and the full two-party
//! KX against a native X25519 implementation (ignored-heavyweight).

#![cfg(feature = "std")]

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_chain::{ChainOut, ChainParty};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::GlobalSecret;
use volar_vc::tls13_2pc::bits_of;
use volar_vc::x25519_gadget::{
    build_fe_mul, build_fe_square, build_final_cswap, build_x25519_step,
};

type N = U16;
type D = Sha256;

/// The final cswap: swap=0 keeps, swap=1 swaps, against a reference.
#[test]
fn final_cswap_concrete() {
    let c = build_final_cswap();
    for swap in [false, true] {
        let mut inputs = vec![swap];
        let x2 = bits_of(&[0x11u8; 32])[..255].to_vec();
        let z2 = bits_of(&[0x22u8; 32])[..255].to_vec();
        let x3 = bits_of(&[0x33u8; 32])[..255].to_vec();
        let z3 = bits_of(&[0x44u8; 32])[..255].to_vec();
        inputs.extend_from_slice(&x2);
        inputs.extend_from_slice(&z2);
        inputs.extend_from_slice(&x3);
        inputs.extend_from_slice(&z3);
        let out = eval_biir(&c, &inputs).expect("eval");
        assert_eq!(out.len(), 510);
        let (ox2, oz2) = out.split_at(255);
        if swap {
            assert_eq!(ox2, &x3[..]);
            assert_eq!(oz2, &z3[..]);
        } else {
            assert_eq!(ox2, &x2[..]);
            assert_eq!(oz2, &z2[..]);
        }
    }
}

/// The four KX circuits schedule (shape sanity; cheap).
#[test]
fn kx_circuits_schedule() {
    for c in [
        build_x25519_step(),
        build_final_cswap(),
        build_fe_square(),
        build_fe_mul(),
    ] {
        let s = volar_vc::compile_schedule(&c).expect("schedules");
        assert_eq!(s.storages.len(), 0);
    }
}

/// 256-bit pass-through circuit for the test-only reveal of the held
/// shared secret.
fn passthrough(n: usize) -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: n as u32,
            stmts: Vec::new(),
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: (0..n as u32).map(IRVarId).collect(),
            }),
        }],
        pre_init: vec![],
    }
}

/// The full two-party KX: run the ladder + cswap + inversion rounds over
/// the loopback MPC transport, then a pass-through reveal of the held
/// shared secret, checked against x25519-dalek.
#[test]
#[ignore = "heavyweight: ~520 two-party rounds over ~140M ANDs"]
fn kx_2pc_shared_secret_matches_dalek() {
    use volar_mpc::strict_chain::ChainFeed;

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    let client_scalar = [0x07u8; 32];
    let server_secret = x25519_dalek::StaticSecret::from([0x42u8; 32]);
    let server_pub = x25519_dalek::PublicKey::from(&server_secret).to_bytes();
    let want = server_secret
        .diffie_hellman(&x25519_dalek::PublicKey::from(
            &x25519_dalek::StaticSecret::from(client_scalar),
        ))
        .to_bytes();

    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut chain = volar_mpc::strict_chain::ChainGarbler::<N>::new(GlobalSecret::<N>::new(
            Array::<u8, N>::from([0x77u8; 16]),
        ));
        let (step, cswap, sq, mul) = kx_scheds();
        run_kx(
            &mut chain,
            &server_pub,
            &[],
            &step,
            &cswap,
            &sq,
            &mul,
            &mut session,
            &mut ot,
        );
        // Reveal the held shared secret via a pass-through round.
        let pt = volar_vc::compile_schedule(&passthrough(256)).expect("sched");
        let mut feeds: Vec<ChainFeed> = (0..255)
            .map(|i| ChainFeed::Held(volar_vc::tls13_live::LIVE_KX_SHARED + i))
            .collect();
        feeds.push(ChainFeed::Const);
        let out = chain
            .run_round::<D, _>(
                &pt,
                &feeds,
                &[false],
                &[],
                &vec![ChainOut::Reveal; 256],
                &mut session,
                &mut ot,
            )
            .expect("reveal round");
        bytes_of(&out)
    });

    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut chain = volar_mpc::strict_chain::ChainEvaluator::<N>::new();
    let mut k = client_scalar;
    k[0] &= 248;
    k[31] &= 127;
    k[31] |= 64;
    let scalar_bits = bits_of(&k)[..255].to_vec();
    let (step, cswap, sq, mul) = kx_scheds();
    run_kx(
        &mut chain,
        &server_pub,
        &scalar_bits,
        &step,
        &cswap,
        &sq,
        &mul,
        &mut session,
        &mut ot,
    );
    let pt = volar_vc::compile_schedule(&passthrough(256)).expect("sched");
    let mut feeds: Vec<ChainFeed> = (0..255)
        .map(|i| ChainFeed::Held(volar_vc::tls13_live::LIVE_KX_SHARED + i))
        .collect();
    feeds.push(ChainFeed::Const);
    let out = chain
        .run_round::<D, _>(
            &pt,
            &feeds,
            &[false],
            &[],
            &vec![ChainOut::Reveal; 256],
            &mut session,
            &mut ot,
        )
        .expect("reveal round");
    let got_eval = bytes_of(&out);

    let got_garb = garbler.join().expect("garbler join");
    assert_eq!(got_eval, want, "evaluator sees the X25519 shared secret");
    assert_eq!(got_garb, want, "garbler sees the X25519 shared secret");
}

// ---- harness ----

fn bytes_of(bits: &[bool]) -> [u8; 32] {
    let mut out = [0u8; 32];
    for (i, b) in bits.iter().enumerate().take(256) {
        if *b {
            out[i / 8] |= 1 << (i % 8);
        }
    }
    out
}

fn kx_scheds() -> (
    volar_mpc::GateSchedule,
    volar_mpc::GateSchedule,
    volar_mpc::GateSchedule,
    volar_mpc::GateSchedule,
) {
    (
        volar_vc::compile_schedule_optimized(&build_x25519_step()).expect("step"),
        volar_vc::compile_schedule_optimized(&build_final_cswap()).expect("cswap"),
        volar_vc::compile_schedule_optimized(&build_fe_square()).expect("sq"),
        volar_vc::compile_schedule_optimized(&build_fe_mul()).expect("mul"),
    )
}

/// Drive the public KX entry point of the live driver.
#[allow(clippy::too_many_arguments)]
fn run_kx<C, T>(
    chain: &mut C,
    server_pub: &[u8; 32],
    scalar_bits: &[bool],
    step: &volar_mpc::GateSchedule,
    cswap: &volar_mpc::GateSchedule,
    sq: &volar_mpc::GateSchedule,
    mul: &volar_mpc::GateSchedule,
    transport: &mut T,
    ot: &mut dyn volar_mpc::OtChannel<N>,
) where
    C: volar_mpc::strict_chain::ChainParty<N>,
    T: volar_mpc::Transport,
{
    volar_vc::tls13_live::run_kx_2pc::<N, D, C, T>(
        chain,
        server_pub,
        scalar_bits,
        step,
        cswap,
        sq,
        mul,
        transport,
        ot,
    )
    .expect("kx rounds");
}

/// One ladder step two-party against concrete eval (all four swap/kt
/// combos) — catches strict-round divergence on this circuit cheaply.
#[test]
fn kx_step_round_2pc_matches_concrete() {
    use volar_mpc::strict_chain::ChainFeed;
    let circuit = build_x25519_step();
    let sched = volar_vc::compile_schedule_optimized(&circuit).expect("sched");
    for (swap, kt) in [(false, false), (false, true), (true, true), (true, false)] {
        let inp = init_step_inputs(swap, kt);
        let want = eval_biir(&circuit, &inp).expect("concrete");
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = format!("{}", listener.local_addr().unwrap());
        let sched_g = sched.clone();
        let inp_g = inp.clone();
        let garbler =
            std::thread::spawn(move || {
                let transport = TcpTransport::accept(&listener).unwrap();
                let mut session = transport.try_clone().unwrap();
                let mut rng = SeedRng::new(0xA11CE);
                let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
                let mut chain = volar_mpc::strict_chain::ChainGarbler::<N>::new(
                    GlobalSecret::<N>::new(Array::<u8, N>::from([0x77u8; 16])),
                );
                let feeds: Vec<ChainFeed> = (0..1276)
                    .map(|_| ChainFeed::Const)
                    .chain([ChainFeed::Eval])
                    .collect();
                chain
                    .run_round::<D, _>(
                        &sched_g,
                        &feeds,
                        &inp_g[..1276],
                        &[],
                        &vec![ChainOut::Reveal; 1021],
                        &mut session,
                        &mut ot,
                    )
                    .expect("round")
            });
        let transport = TcpTransport::connect(&addr).unwrap();
        let mut session = transport.try_clone().unwrap();
        let mut rng = SeedRng::new(0xB0B);
        let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
        let mut chain = volar_mpc::strict_chain::ChainEvaluator::<N>::new();
        let feeds: Vec<ChainFeed> = (0..1276)
            .map(|_| ChainFeed::Const)
            .chain([ChainFeed::Eval])
            .collect();
        let got = chain
            .run_round::<D, _>(
                &sched,
                &feeds,
                &[],
                &[kt],
                &vec![ChainOut::Reveal; 1021],
                &mut session,
                &mut ot,
            )
            .expect("round");
        let garb = garbler.join().unwrap();
        assert_eq!(got, want, "evaluator 1-step (swap={swap} kt={kt})");
        assert_eq!(garb, want, "garbler 1-step (swap={swap} kt={kt})");
    }
}

/// Init-state step inputs: x2=1, z2=0, x3=u(=9), z3=1, x1=u, swap, kt.
fn init_step_inputs(swap: bool, kt: bool) -> Vec<bool> {
    let mut one = vec![false; 255];
    one[0] = true;
    let mut nine = vec![false; 255];
    nine[0] = true;
    nine[3] = true;
    let mut v = Vec::new();
    v.extend_from_slice(&one);
    v.extend_from_slice(&vec![false; 255]);
    v.extend_from_slice(&nine);
    v.extend_from_slice(&one);
    v.extend_from_slice(&nine);
    v.push(swap);
    v.push(kt);
    v
}

/// The KX driver's exact inversion slot-op sequence, simulated concretely
/// with eval_biir over the shared sq/mul circuits — catches a chain
/// transcription bug without a two-party run.
#[test]
fn kx_inversion_slot_ops_match_reference() {
    use volar_vc::x25519_gadget::scalar_ref::{Fp, fp_from_bytes, fp_invert, fp_mul, fp_to_bytes};
    let sq = build_fe_square();
    let mul = build_fe_mul();
    let fe_in = |w: &Fp| bits_of(&fp_to_bytes(w))[..255].to_vec();
    let fe_out = |out: &[bool]| -> Fp {
        let mut bytes = [0u8; 32];
        for (i, &b) in out.iter().take(255).enumerate() {
            if b {
                bytes[i / 8] |= 1 << (i % 8);
            }
        }
        bytes[31] &= 0x7f;
        fp_from_bytes(&bytes)
    };
    // Slot file: 0=T0 1=T1 2=T2 3=T3 4=Z2 5=X2 6=SHARED — mirrors
    // run_kx_2pc's op list (keep in sync!).
    enum Op {
        Sq(usize, usize),
        Mul(usize, usize, usize),
    }
    use Op::{Mul, Sq};
    const T0: usize = 0;
    const T1: usize = 1;
    const T2: usize = 2;
    const T3: usize = 3;
    const Z2: usize = 4;
    const X2: usize = 5;
    const SH: usize = 6;
    let mut ops: Vec<Op> = vec![
        Sq(T0, Z2),
        Sq(T1, T0),
        Sq(T1, T1),
        Mul(T1, Z2, T1),
        Mul(T0, T0, T1),
        Sq(T2, T0),
        Mul(T2, T1, T2),
        Sq(T1, T2),
    ];
    ops.extend((0..4).map(|_| Sq(T1, T1)));
    ops.push(Mul(T1, T1, T2));
    ops.push(Sq(T2, T1));
    ops.extend((0..9).map(|_| Sq(T2, T2)));
    ops.push(Mul(T2, T2, T1));
    ops.push(Sq(T3, T2));
    ops.extend((0..19).map(|_| Sq(T3, T3)));
    ops.push(Mul(T2, T3, T2));
    ops.push(Sq(T3, T2));
    ops.extend((0..9).map(|_| Sq(T3, T3)));
    ops.push(Mul(T1, T3, T1));
    ops.push(Sq(T3, T1));
    ops.extend((0..49).map(|_| Sq(T3, T3)));
    ops.push(Mul(T2, T3, T1));
    ops.push(Sq(T3, T2));
    ops.extend((0..99).map(|_| Sq(T3, T3)));
    ops.push(Mul(T2, T3, T2));
    ops.extend((0..50).map(|_| Sq(T2, T2)));
    ops.push(Mul(T1, T2, T1));
    ops.extend((0..5).map(|_| Sq(T1, T1)));
    ops.push(Mul(T0, T1, T0));
    ops.push(Mul(SH, X2, T0));

    // Canonical field elements (the ladder only produces < p values).
    let z: Fp = fp_from_bytes(&[0x2Bu8; 32]);
    let x: Fp = fp_from_bytes(&[0x4Du8; 32]);
    let mut slots: Vec<Fp> = vec![[0; 4]; 7];
    slots[Z2] = z;
    slots[X2] = x;
    for op in ops {
        match op {
            Sq(d, a) => {
                let out = eval_biir(&sq, &fe_in(&slots[a])).expect("sq");
                slots[d] = fe_out(&out);
            }
            Mul(d, a, b) => {
                let mut inp = fe_in(&slots[a]);
                inp.extend(fe_in(&slots[b]));
                let out = eval_biir(&mul, &inp).expect("mul");
                slots[d] = fe_out(&out);
            }
        }
    }
    let want = fp_mul(&x, &fp_invert(&z));
    assert_eq!(
        fp_to_bytes(&slots[SH]).to_vec(),
        fp_to_bytes(&want).to_vec(),
        "the KX slot-op inversion sequence must match x * z^-1"
    );
}
