//! Workstream G3 — WAT conformance capstone: a real waffle/WAT-lowered
//! function runs through the embedder's GRAM-backed two-party session over
//! every argument-visibility combination, matching concrete evaluation.
//!
//! This composes **G1 (storage via GRAM)** and the pipeline: a WAT guest is
//! lowered through the waffle frontend → `VaffleTarget` with a `VcConfig` (per
//! -arg visibility sides) → `IRBlocks` → unrolled to a single circuit →
//! bit-blasted to `BIrBlocks` with side propagation → scheduled to a
//! `GateSchedule` whose spill/reload `StorageWrite`/`StorageRead` ops become
//! GRAM gates over a real ORAM (with address compression folding the sparse
//! scaffold addresses down to a compact block space). The garbled two-party
//! run must match the concrete `eval_ir_circuit_step` for every visibility
//! combo — the D2 conformance contract.
//!
//! The guest is `f(x) = ((x*3) + 7) ^ 5`: straight-line with locals, so it
//! exercises the waffle spill scaffold (StorageWrite/StorageRead → GRAM)
//! without a control-flow loop. (The looping half of the direction is G2's
//! `loop_reveal.rs`; a full looping WAT guest composes the two.)

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::BIrBlocks;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_oram::OramTree;
use volar_side::SideId;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::{GramEvalDrive, VcEmbedder, VcOutcome};

type N = U16;
type D = Sha256;

fn det_bytes(seed: u8) -> Array<u8, N> {
    Array::clone_from_slice(&[seed; 16])
}

const GUEST_WAT: &str = r#"(module
  (func $f (export "f") (param $x i32) (result i32)
    (local $t i32)
    (local.set $t (i32.mul (local.get $x) (i32.const 3)))
    (local.set $t (i32.add (local.get $t) (i32.const 7)))
    (i32.xor (local.get $t) (i32.const 5))))"#;

fn guest_fn(x: u32) -> u32 {
    (x.wrapping_mul(3).wrapping_add(7)) ^ 5
}

/// Everything the conformance run needs from lowering one guest.
struct Lowered {
    boolar: BIrBlocks,
    /// Per-entry-param-bit sides (32 bits for the single i32 param).
    param_sides: Vec<Option<SideId>>,
    public: SideId,
    local: SideId,
    remote: SideId,
    /// The unrolled single-block IR circuit + type table (concrete reference).
    ir_circuit: volar_ir::ir::IRBlocks,
    types: volar_ir::ir::IRTypes,
}

fn lower_guest(vis: volar_vaffle_target::VcArg) -> Lowered {
    let bytes: &'static [u8] = Box::leak(wat::parse_str(GUEST_WAT).unwrap().into_boxed_slice());
    let module = portal_pc_waffle_frontend::from_wasm_bytes(
        bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("waffle parse");
    let vc = volar_vaffle_target::VcConfig::new().with_call("f", vec![vis]);
    let mut target =
        volar_vaffle_target::VaffleTarget::with_pointer_width(vaffle::PointerWidth::Bits32);
    let (errors, artifact) = volar_vaffle_target::lower_waffle_module_with_vc(
        &module,
        &mut target,
        &volar_vaffle_target::WaffleImportConfig::default(),
        &vc,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let param_sides = volar_vaffle_target::entry_param_sides(&target.module).expect("entry body");
    let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);
    let ir_circuit = volar_ir_passes::unroll_ir_everything(&blocks, &types).expect("unroll");
    assert!(
        ir_circuit.is_circuit(),
        "straight-line guest unrolls to a circuit"
    );

    let mut side_inputs = volar_ir_passes::lower_ir_to_boolar::SideInputs::default();
    side_inputs.param_sides.insert(0, param_sides.clone());
    let boolar = volar_ir_passes::lower_ir_to_boolar::lower_ir_to_boolar_with_sides(
        &ir_circuit,
        &types,
        &side_inputs,
    );
    Lowered {
        boolar,
        param_sides,
        public: artifact.handler.public,
        local: artifact.handler.local,
        remote: artifact.handler.remote,
        ir_circuit,
        types,
    }
}

/// Concrete reference: run the unrolled IR circuit with `x` in the low 32 bits
/// of its single 64-bit input word.
fn concrete_eval(l: &Lowered, x: u32) -> u32 {
    let mut input_bits = vec![false; 64];
    for i in 0..32 {
        input_bits[i] = (x >> i) & 1 == 1;
    }
    let mut storage = volar_fuzz::interpreter::ir::StorageMap::new();
    let out = volar_fuzz::interpreter::ir::eval_ir_circuit_step(
        &l.ir_circuit.blocks[0],
        &l.types,
        &[],
        &[input_bits],
        &mut storage,
    );
    // 32 single-bit outputs, LSB-first.
    let mut word = 0u32;
    for (i, ov) in out.iter().enumerate().take(32) {
        if ov[0] {
            word |= 1 << i;
        }
    }
    word
}

/// Build the input partition + per-owner bit strings for a 64-bit input whose
/// low 32 bits are `x` (side-tagged) and high 32 bits are scaffold (public 0).
fn build_inputs(l: &Lowered, x: u32) -> (Vec<InputOwner>, Vec<bool>, Vec<bool>, Vec<bool>) {
    let partition = volar_vc::partition_from_sides(64, l.public, l.local, l.remote, |i| {
        l.param_sides.get(i).copied().flatten()
    });
    let mut inbits = [false; 64];
    for i in 0..32 {
        inbits[i] = (x >> i) & 1 == 1;
    }
    let mut public = Vec::new();
    let mut garbler = Vec::new();
    let mut evaluator = Vec::new();
    for i in 0..64 {
        match partition[i] {
            InputOwner::Public => public.push(inbits[i]),
            InputOwner::Garbler => garbler.push(inbits[i]),
            InputOwner::Evaluator => evaluator.push(inbits[i]),
        }
    }
    (partition, public, garbler, evaluator)
}

/// Run the guest through the embedder's GRAM-backed two-party session,
/// returning the revealed 32-bit result. Monomorphized at the guest's shape
/// (64 inputs, `A` AND gates).
fn run_gram<const A: usize>(l: &Lowered, x: u32) -> u32 {
    let schedule = VcEmbedder::<N, 64, A>::compile(&l.boolar).expect("guest schedules");
    assert_eq!(schedule.num_inputs, 64);
    assert_eq!(
        schedule.storages.len(),
        1,
        "one GRAM storage space (the spill scaffold)"
    );
    // Non-vacuous: the spill scaffold's storage ops really became GRAM gates
    // (the ORAM driver below is exercised, not bypassed).
    let n_storage = schedule
        .gates
        .iter()
        .filter(|g| {
            matches!(
                g,
                volar_mpc::Gate::StorageRead { .. } | volar_mpc::Gate::StorageWrite { .. }
            )
        })
        .count();
    assert!(
        n_storage > 0,
        "spill scaffold must produce GRAM storage gates"
    );
    let spec = schedule.storages[0].clone();

    let secret = GlobalSecret::<N>::new(det_bytes(41));
    let labels: [Garble<N>; 64] = core::array::from_fn(|i| Garble {
        base: det_bytes((i as u8).wrapping_mul(7).wrapping_add(1)),
    });
    let embedder: VcEmbedder<N, 64, A> = VcEmbedder::with_secret(secret.clone(), labels);

    let (partition, public, garbler, evaluator) = build_inputs(l, x);
    const Z: usize = 4;
    const B: usize = 8;
    let mut tree = OramTree::<Z, B>::new(spec.levels);
    let mut drive: GramEvalDrive<D, N, Z, B> =
        GramEvalDrive::new(&secret, &mut tree, spec.levels, spec.num_cells, 0x5EED);
    let mut gram: [&mut dyn volar_mpc::GramDrive<N>; 1] = [&mut drive];
    let mut ot = LoopbackOt::<N>::new();

    match embedder.invoke_schedule_with_gram::<D>(
        &schedule, &partition, &public, &garbler, &evaluator, &mut ot, &mut gram,
    ) {
        VcOutcome::Value(bits) => {
            let mut word = 0u32;
            for (i, &b) in bits.iter().enumerate().take(32) {
                if b {
                    word |= 1 << i;
                }
            }
            word
        }
        other => panic!("GRAM run aborted: {other:?}"),
    }
}

/// The G3 capstone: the real WAT-lowered guest runs through the GRAM-backed
/// embedder over all three argument-visibility combos, matching concrete
/// evaluation. The spill scaffold's storage goes through the ORAM (G1), the
/// circuit is multi-output (32-bit result), and the input partition honors
/// each arg's vc visibility.
#[test]
fn wat_conformance_all_visibilities() {
    const A: usize = 186; // this guest's AND count (see probe)
    for vis in [
        volar_vaffle_target::VcArg::Private,
        volar_vaffle_target::VcArg::Blind,
        volar_vaffle_target::VcArg::Public,
    ] {
        let l = lower_guest(vis);
        for x in [0u32, 1, 5, 10, 0xDEAD_BEEF] {
            let expected = guest_fn(x);
            // The pipeline concrete reference agrees with the WAT semantics.
            assert_eq!(concrete_eval(&l, x), expected, "concrete eval f({x:#x})");
            // The GRAM-backed two-party run matches.
            let got = run_gram::<A>(&l, x);
            assert_eq!(got, expected, "GRAM run f({x:#x}) vis={vis:?}");
        }
    }
}
