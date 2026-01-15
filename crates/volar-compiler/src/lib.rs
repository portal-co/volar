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

pub mod ir;
#[cfg(feature = "parsing")]
pub mod parser;
pub mod lowering;
pub mod printer;
pub mod printer_dyn;
pub mod lowering_dyn;

pub use ir::*;
#[cfg(feature = "parsing")]
pub use parser::*;
pub use lowering::*;
pub use printer::*;
pub use printer_dyn::*;
