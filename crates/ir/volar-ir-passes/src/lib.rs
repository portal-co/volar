#![no_std]

extern crate alloc;

pub mod lower_ir_to_boolar;
pub mod lower_lir;
pub mod lower_to_circuit;
pub mod movfuscate;

pub use lower_ir_to_boolar::{ir_type_bits, lower_ir_to_boolar};
pub use lower_to_circuit::LoweringMode;
pub use movfuscate::{movfuscate_biir, movfuscate_ir, pc_bits_needed};
