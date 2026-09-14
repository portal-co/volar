//! Real LLVM IR memory guest imported through VAFFLE and symbolic ORAM.

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use volar_llvm_vaffle_import::import_module;
use volar_vc::oram_lower::{OramLowerConfig, Stage, run_concrete, storage_to_oram};

const GUEST: &str = r#"
target datalayout = "e-p:32:32"
define i32 @roundtrip(i32 %x) {
entry:
  %slot = alloca i32, align 4
  store i32 %x, ptr %slot, align 4
  %result = load i32, ptr %slot, align 4
  ret i32 %result
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
    assert_eq!(got, x);
}
