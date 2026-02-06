//! # Volar Compiler
//!
//! A compiler that parses Rust code from `volar-spec` and produces an IR
//! suitable for transpilation to other languages or dynamic code generation.
//!
//! This compiler focuses on the specific Rust features used in `volar-spec`:
//! - Generic structs and their type parameters
//! - Trait bounds and where clauses  
//! - Impl blocks with trait implementations (Add, Mul, BitXor, etc.)
//! - Methods with generic parameters
//! - Type-level computation patterns (typenum style)
//! - Closures and iterators
//! - Control flow (for loops, if-else, pattern matching)
//!
//! ## Modules
//!
//! - `ir`: The unified intermediate representation with domain-specific knowledge
//! - `parser`: syn-based parser that converts Rust AST directly to specialized IR
//! - `lowering`: Type resolution, operator analysis, monomorphization
//! - `printer`: Pretty-printer for IR

#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(feature = "std")]
use std::string::String;
#[cfg(not(feature = "std"))]
use alloc::string::String;

pub mod ir;
pub mod const_analysis;
#[cfg(feature = "parsing")]
pub mod parser;
pub mod lowering;
pub mod printer;
pub mod lowering_dyn;
pub mod manifest;

pub use ir::*;
pub use const_analysis::*;
#[cfg(feature = "parsing")]
pub use parser::*;
pub use lowering::*;
pub use printer::*;
pub use manifest::*;

/// Generate dynamic Rust code by lowering type-level lengths to runtime witnesses.
pub fn print_module_rust_dyn(module: &IrModule) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    printer::print_module(&lowered)
}

/// Generate dynamic Rust code with dependency manifests providing context.
pub fn print_module_rust_dyn_with_deps(module: &IrModule, deps: &[manifest::TypeManifest]) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    printer::print_module_with_deps(&lowered, deps)
}
