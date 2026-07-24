// @reliability: experimental
// @ai: assisted
//! `volar-vaffle-target` — the LIR target and WAFFLE lowering for VAFFLE IR.
//!
//! Exports:
//! - [`VaffleTarget`] / [`VaffleValue`] / [`VaffleBlock`] — a [`LirTarget`]
//!   that builds a VAFFLE [`Module`] using the shared GF(2) bit-circuit
//!   arithmetic from [`volar_lir::circuits`].
//! - [`waffle_lower`] — translation of WAFFLE [`FunctionBody`] to VAFFLE,
//!   covering integer ops, branches, and direct function calls.

#![no_std]
extern crate alloc;

pub mod import_config;
pub mod lower_to_ir;
#[cfg(feature = "lazy-ir-plan")]
pub mod plan;
pub mod target;
pub mod vaffle_ssa;
pub mod waffle_lower;

pub use import_config::{WaffleImportConfig, WaffleImportKind};
pub use lower_to_ir::{lower_vaffle_to_ir, lower_vaffle_to_ir_with_control_provenance};
pub use target::{VaffleBlock, VaffleTarget, VaffleValue};
pub use waffle_lower::{
    lower_waffle_function, lower_waffle_function_lazy, lower_waffle_module,
    lower_waffle_module_with_metadata, UnsupportedOp, WasmMetadataMode,
};
