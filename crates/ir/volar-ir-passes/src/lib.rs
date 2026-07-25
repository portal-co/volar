#![no_std]

extern crate alloc;

pub mod dispatch_accumulator;
pub mod lower_ir_to_boolar;
pub mod lower_lir;
pub mod lower_to_circuit;
pub mod movfuscate;
pub mod raise_to_z3;

pub use lower_ir_to_boolar::{ir_type_bits, lower_ir_to_boolar};
pub use lower_to_circuit::{
    LoweringMode, lower_to_circuit, lower_to_circuit_ir,
    lower_to_circuit_ir_with_control_provenance,
    lower_to_circuit_with_control_provenance,
};
pub use movfuscate::{
    movfuscate_biir, movfuscate_biir_with_control_provenance, movfuscate_ir,
    movfuscate_ir_with_boundary, movfuscate_ir_with_control_provenance, pc_bits_needed,
    remap_movfusc_accum_info, remap_movfusc_boundaries, remap_movfusc_boundary, thread_synthetic_slots,
    MovfuscAccumInfo, MovfuscAccumInit, MovfuscAccumStep, MovfuscBlockBoundary,
};
pub use raise_to_z3::raise_bits_to_z3;
