//! Real Wasm-memory guest lowered through VAFFLE and the symbolic ORAM split.

use volar_vc::compile_schedule;
use volar_vc::oram_gadget::{
    build_material_open_evaluator_block, build_material_open_garbler_block,
    build_material_seal_evaluator_block, build_material_seal_garbler_block,
};
use volar_vc::oram_lower::{OramLowerConfig, Stage, run_concrete, storage_to_oram};

const GUEST: &str = r#"(module
  (memory 1)
  (func (export "roundtrip") (param $x i32) (result i32)
    (i32.store8 (i32.const 0) (local.get $x))
    (i32.load8_u (i32.const 0)))
)"#;

fn lower() -> volar_ir::boolar::BIrBlocks {
    let bytes: &'static [u8] = Box::leak(wat::parse_str(GUEST).expect("WAT").into_boxed_slice());
    let module = portal_pc_waffle_frontend::from_wasm_bytes(
        bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("parse Wasm");
    let config = volar_vaffle_target::VcConfig::new()
        .with_call("roundtrip", vec![volar_vaffle_target::VcArg::Private]);
    let mut target =
        volar_vaffle_target::VaffleTarget::with_pointer_width(vaffle::PointerWidth::Bits32);
    let (errors, _) = volar_vaffle_target::lower_waffle_module_with_vc(
        &module,
        &mut target,
        &volar_vaffle_target::WaffleImportConfig::default(),
        &config,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);
    let circuit = volar_ir_passes::unroll_ir_everything(&blocks, &types).expect("unroll");
    let sides = volar_ir_passes::lower_ir_to_boolar::SideInputs::default();
    volar_ir_passes::lower_ir_to_boolar::lower_ir_to_boolar_with_sides(&circuit, &types, &sides)
}

#[test]
fn wasm_memory_store_load_is_lowered_to_oram_and_preserves_a_word() {
    let boolar = lower();
    let program = storage_to_oram(
        &boolar,
        &OramLowerConfig {
            levels: 4,
            bucket_size: 2,
            max_stash: 96,
            secure: false,
            shared_tree_key: false,
            // A Wasm byte address is 32 bits; the guest touches only its low
            // constant window, so this fixes a small experimental ORAM shape.
            narrow_bits: Some(6),
        },
    )
    .expect("memory lowers to ORAM");
    assert!(
        program
            .stages
            .iter()
            .any(|stage| matches!(stage, Stage::Access(_))),
        "Wasm memory access becomes ORAM access stages"
    );
    assert_eq!(
        program.oram.num_addrs, 64,
        "six-bit narrowed Wasm address window"
    );
    let accesses = program
        .stages
        .iter()
        .filter(|stage| matches!(stage, Stage::Access(_)))
        .count();
    eprintln!(
        "wasm-storage-oram: accesses={accesses}, tape_bits={}, cells={}, levels={}, stash={}",
        program.tape_width, program.oram.num_addrs, program.oram.levels, program.oram.max_stash,
    );

    let x = 0x1234_5678u32;
    let mut inputs = vec![false; program.input_slots.len()];
    for (bit, input) in inputs.iter_mut().enumerate().take(32) {
        *input = (x >> bit) & 1 != 0;
    }
    let (output, _tree) = run_concrete::<2>(&program, &inputs);
    let got = output
        .iter()
        .enumerate()
        .take(32)
        .fold(0u32, |word, (bit, set)| word | ((*set as u32) << bit));
    assert_eq!(got, x & 0xff);

    let shared = storage_to_oram(
        &boolar,
        &OramLowerConfig {
            levels: 4,
            bucket_size: 2,
            max_stash: 96,
            secure: true,
            shared_tree_key: true,
            narrow_bits: Some(6),
        },
    )
    .expect("shared-key memory lowers to ORAM");
    assert!(shared.oram.encrypted);
    assert!(!shared.oram.encrypt_valid && !shared.oram.versioned_pads);
    let access_ands = compile_schedule(&shared.access)
        .expect("shared access schedule")
        .and_count();
    let begin_ands = compile_schedule(&shared.begin)
        .expect("shared begin schedule")
        .and_count();
    let material_ands = [
        build_material_seal_garbler_block(),
        build_material_open_garbler_block(),
        build_material_seal_evaluator_block(),
        build_material_open_evaluator_block(),
    ]
    .iter()
    .map(|circuit| {
        compile_schedule(circuit)
            .expect("material schedule")
            .and_count()
    })
    .collect::<Vec<_>>();
    let held_round_trip_ands: usize = material_ands.iter().sum();
    let tape_material_round_trip_ands = shared.tape_width * held_round_trip_ands;
    let memory_access_ands = accesses * access_ands;
    assert!(
        tape_material_round_trip_ands > memory_access_ands,
        "the full tape must be encapsulated/cached rather than sealed on every storage epoch"
    );
    eprintln!(
        "wasm-split-key: begin_ands={begin_ands}, access_ands={access_ands}, \
         material_direction_ands={material_ands:?}, held_both_round_trip_ands={held_round_trip_ands}, \
         tape_material_round_trip_ands={tape_material_round_trip_ands}, \
         guest_memory_access_ands={memory_access_ands}",
    );
}
