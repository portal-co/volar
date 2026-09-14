//! Real LLVM IR memory guest imported through VAFFLE and symbolic ORAM.

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use volar_llvm_vaffle_import::import_module;
use volar_vc::compile_schedule;
use volar_vc::oram_gadget::{
    build_material_open_evaluator_block, build_material_open_garbler_block,
    build_material_seal_evaluator_block, build_material_seal_garbler_block,
};
use volar_vc::oram_lower::{OramLowerConfig, Stage, run_concrete, storage_to_oram};

const GUEST: &str = r#"
target datalayout = "e-p:32:32"
define i32 @roundtrip(i32 %x) {
entry:
  %slot = alloca i8, align 1
  %byte = trunc i32 %x to i8
  store i8 %byte, ptr %slot, align 1
  %result = load i8, ptr %slot, align 1
  %wide = zext i8 %result to i32
  ret i32 %wide
}
"#;

#[test]
fn llvm_alloca_store_load_is_lowered_to_oram_and_preserves_a_word() {
    let context = Context::create();
    let llvm = context
        .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
            GUEST.as_bytes(),
            "storage.ll",
        ))
        .expect("parse LLVM IR");
    let module = import_module(&llvm, &["roundtrip"]).expect("import LLVM memory guest");
    let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir_owned(module);
    let circuit = volar_ir_passes::unroll_ir_everything(&blocks, &types).expect("unroll");
    let sides = volar_ir_passes::lower_ir_to_boolar::SideInputs::default();
    let boolar = volar_ir_passes::lower_ir_to_boolar::lower_ir_to_boolar_with_sides(
        &circuit, &types, &sides,
    );
    let program = storage_to_oram(
        &boolar,
        &OramLowerConfig {
            levels: 4,
            bucket_size: 2,
            max_stash: 96,
            secure: false,
            shared_tree_key: false,
            narrow_bits: Some(6),
        },
    )
    .expect("LLVM memory lowers to ORAM");
    let accesses = program
        .stages
        .iter()
        .filter(|stage| matches!(stage, Stage::Access(_)))
        .count();
    assert!(accesses > 0);
    eprintln!(
        "llvm-storage-oram: accesses={accesses}, tape_bits={}, cells={}, levels={}, stash={}",
        program.tape_width, program.oram.num_addrs, program.oram.levels, program.oram.max_stash,
    );

    let x = 0x0BAD_F00Du32;
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
        "llvm-split-key: begin_ands={begin_ands}, access_ands={access_ands}, \
         material_direction_ands={material_ands:?}, held_both_round_trip_ands={held_round_trip_ands}, \
         tape_material_round_trip_ands={tape_material_round_trip_ands}, \
         guest_memory_access_ands={memory_access_ands}",
    );
}
