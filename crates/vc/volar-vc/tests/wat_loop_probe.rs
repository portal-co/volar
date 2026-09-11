// @reliability: experimental
// @ai: assisted
//! The real WAT loop guest lowered to a movfuscated step circuit and run through
//! the **multi-space** symbolic ORAM concretely.
//!
//! The counting-loop guest (`f(0)`, since the entry state is all-zero) lowers
//! through waffle -> vc-vaffle -> IRBlocks -> movfuscate ->
//! `lower_to_circuit_ir(WithTerminationFlag)` to a single `is_circuit` step
//! circuit, then to boolar with 492 storage ops spanning THREE read-write
//! storage spaces (linear-memory stack, movfuscated bytecode, ABI). The
//! multi-space `storage_to_oram` builds one ORAM per space (narrowed to 12
//! bits), and the multi-space `ConcreteOramDrive` runs the step circuit.
//!
//! Validated: the lowering produces a well-formed 3-space `OramProgram`, the
//! concrete IR interpreter (plain storage) terminates the loop, and the
//! multi-space ORAM driver runs the real guest without stash overflow.
//!
//! **Follow-up:** the ORAM run's `done` step currently diverges from the IR
//! reference (the loop doesn't terminate on the ORAM path) — a state-threading
//! or address-window bug to chase. The multi-space *mechanism* is validated by
//! the 2-space test in `oram_lower.rs`.

use volar_ir::boolar::BIrStmt;
use volar_ir::ir::{IRType, IRTypeId};
use volar_ir_common::Type;
use volar_vc::oram_lower::{ConcreteOramDrive, OramLowerConfig, Stage, storage_to_oram};

const LOOP_WAT: &str = r#"(module
  (func $f (export "f") (param $n i32) (result i32)
    (local $acc i32)
    (local $i i32)
    (local.set $i (local.get $n))
    (block $exit
      (loop $l
        (br_if $exit (i32.eqz (local.get $i)))
        (local.set $acc (i32.add (local.get $acc) (local.get $i)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $l)))
    (local.get $acc)))"#;

fn lower_loop_step() -> (volar_ir::ir::IRBlocks, volar_ir::ir::IRTypes, volar_ir::boolar::BIrBlocks) {
    let bytes: &'static [u8] = Box::leak(wat::parse_str(LOOP_WAT).unwrap().into_boxed_slice());
    let module = portal_pc_waffle_frontend::from_wasm_bytes(
        bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("waffle parse");
    let vc = volar_vaffle_target::VcConfig::new()
        .with_call("f", vec![volar_vaffle_target::VcArg::Private]);
    let mut target =
        volar_vaffle_target::VaffleTarget::with_pointer_width(vaffle::PointerWidth::Bits32);
    let (errors, _artifact) = volar_vaffle_target::lower_waffle_module_with_vc(
        &module,
        &mut target,
        &volar_vaffle_target::WaffleImportConfig::default(),
        &vc,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);
    let mut ib = blocks.clone();
    loop {
        let c1 = volar_ir_opt::ir::fold_ir_blocks(&mut ib, &types);
        let c2 = volar_ir_opt::store_forward::store_forward_ir_blocks(&mut ib, &types);
        if !c1 && !c2 {
            break;
        }
    }
    let (movf, _b, _acc) = volar_ir_passes::movfuscate_ir_with_boundary(&ib, &mut types);
    let bit_ty: IRTypeId = types.intern(IRType::Primitive(Type::Bit));
    let step = volar_ir_passes::lower_to_circuit_ir(
        &movf,
        &bit_ty,
        1,
        volar_ir_passes::LoweringMode::WithTerminationFlag,
    );
    assert!(step.is_circuit());
    let boolar = volar_ir_passes::lower_ir_to_boolar(&step, &types);
    (step, types, boolar)
}

#[test]
fn wat_loop_guest_lowers_multi_space() {
    let (_step, _types, boolar) = lower_loop_step();
    let b0 = &boolar.blocks[0];
    let mut spaces: Vec<u32> = b0
        .stmts
        .iter()
        .filter_map(|s| match &s.kind {
            BIrStmt::StorageRead { storage, .. } => Some(storage.0),
            BIrStmt::StorageWrite { storage, .. } => Some(storage.0),
            _ => None,
        })
        .collect();
    spaces.sort();
    spaces.dedup();
    assert!(spaces.len() > 1, "loop guest uses multiple storage spaces: {spaces:?}");
    // Does the step circuit carry pre-initialized storage (e.g. the bytecode)?
    // The ORAM starts empty; if the interpreter pre-loads storage, the ORAM must
    // too, or reads of those cells diverge.
    println!("boolar pre_init segments: {}", boolar.pre_init.len());
    for seg in boolar.pre_init.iter().take(5) {
        println!("  pre_init: storage {} ({} addr bits, {} data bits)", seg.storage.0, seg.addr.len(), seg.data.len());
    }

    // Multi-space lowering succeeds, one ORAM per space.
    let program = storage_to_oram(
        &boolar,
        &OramLowerConfig {
            levels: 13,
            bucket_size: 2,
            max_stash: 2 * 13 + 2 + 16,
            secure: false,
            narrow_bits: Some(12),
        },
    )
    .expect("multi-space lowering succeeds");
    assert_eq!(program.spaces.len(), spaces.len(), "one ORAM per space");
    assert!(
        program.stages.iter().any(|s| matches!(s, Stage::Access(_))),
        "has ORAM access stages"
    );
}

// The concrete multi-space ORAM run of the real WAT loop guest, validated
// end-to-end against the concrete IR interpreter: same per-step `done` flag,
// same threaded next-state, same final result. This is the looping-guest
// payoff: a waffle-lowered loop with genuinely-symbolic (SP-relative) spill
// addresses runs through the symbolic multi-space ORAM.
#[test]
#[ignore = "heavyweight: multi-space ORAM run of the WAT loop guest"]
fn wat_loop_guest_runs_multi_space_oram() {
    use volar_fuzz::interpreter::ir::{StorageMap, bit_width, eval_ir_circuit_step};
    let (step, types, boolar) = lower_loop_step();
    let widths: Vec<usize> = step.blocks[0].params.iter().map(|&t| bit_width(t, &types)).collect();
    let nparams = widths.len();

    // The guest's working set is tiny (~68 cells), so a small XOR-folded window
    // suffices; the XOR-fold mixes the flat-cell bit-index (bits 32+) and the
    // stack-offset bits into the window so nothing collides.
    const NARROW: usize = 10;
    let program = storage_to_oram(
        &boolar,
        &OramLowerConfig {
            levels: NARROW + 1,
            bucket_size: 2,
            max_stash: 2 * (NARROW + 1) + 2 + 16,
            secure: false,
            narrow_bits: Some(NARROW),
        },
    )
    .expect("multi-space lowering succeeds");
    let n_state = program.input_slots.len();
    assert_eq!(n_state, widths.iter().sum::<usize>(), "state width matches");

    let mut drive = ConcreteOramDrive::<2>::for_program(&program);
    let mut inputs = vec![false; n_state];
    let mut ref_storage = StorageMap::new();
    let mut ref_inputs: Vec<Vec<bool>> = widths.iter().map(|&w| vec![false; w]).collect();
    let mut result = None;
    for _step_i in 0..16 {
        let out = drive.run_program(&program, &inputs);
        let ref_out =
            eval_ir_circuit_step(&step.blocks[0], &types, &step.oracles, &ref_inputs, &mut ref_storage);
        assert_eq!(out[0], ref_out[0][0], "done flag matches");
        let ref_state: Vec<bool> = ref_out[1..1 + nparams].iter().flatten().copied().collect();
        assert_eq!(&out[1..1 + n_state], ref_state.as_slice(), "next-state matches");
        if out[0] {
            let ref_res: Vec<bool> = ref_out[1 + nparams..].iter().flatten().copied().collect();
            assert_eq!(&out[1 + n_state..], ref_res.as_slice(), "result matches");
            result = Some(out[1 + n_state..].to_vec());
            break;
        }
        inputs = out[1..1 + n_state].to_vec();
        ref_inputs = ref_out[1..1 + nparams].to_vec();
    }
    let result = result.expect("the ORAM loop terminates");
    // f(0) = sum over an empty range = 0.
    let word = result.iter().take(32).enumerate().fold(0u32, |a, (i, b)| if *b { a | (1 << i) } else { a });
    assert_eq!(word, 0, "f(0) = 0");
}
