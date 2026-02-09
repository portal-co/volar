//! Dynamic (heap-allocated) versions of key pieces from `crates/volar-spec`.
//!
//! This file re-implements selected types and helpers from the original
//! `volar-spec` crate using dynamic allocation (`Vec`) instead of
//! `GenericArray`/const generics. The intent is to keep the same
//! semantics while removing the compile-time size requirements.
//!
//! Original sources cited (kept here for clarity):
//! - crates/volar-spec/src/vole.rs
//! - crates/volar-spec/src/byte_gen.rs
//!
use alloc::vec;
use alloc::vec::Vec;
use core::ops::Mul;
pub mod byte_gen;
pub mod vole;
pub use byte_gen::*;
pub use vole::*;
