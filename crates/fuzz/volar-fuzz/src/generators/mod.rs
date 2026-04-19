//! Strategies and builder functions for generating valid IR.
//!
//! The builder functions (`interpret_biir`, `interpret_ir`, `interpret_vaffle`)
//! are always compiled and are used by both the proptest strategies (test-only)
//! and the `arbitrary` impls (for cargo-fuzz).
pub mod biir;
pub mod ir;
pub mod vaffle;
