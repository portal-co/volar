#![no_std]

extern crate alloc;

pub mod lower_lir;
pub mod lower_to_circuit;
pub mod movfuscate;

pub use lower_to_circuit::LoweringMode;
pub use movfuscate::{movfuscate_biir, movfuscate_ir, pc_bits_needed};
