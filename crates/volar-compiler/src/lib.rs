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
//! - `ir`: The generic intermediate representation with string-based types
//! - `specialized`: Domain-specific enums for types, traits, and methods
//! - `parser`: syn-based parser that converts Rust AST to IR
//! - `specialize`: Conversion from generic IR to specialized IR
//! - `lowering`: Type resolution, operator analysis, monomorphization
//! - `printer`: Pretty-printer for IR

pub mod ir;
pub mod specialized;
pub mod parser;
pub mod specialize;
pub mod lowering;
pub mod printer;

pub use ir::*;
pub use specialized::*;
pub use parser::*;
pub use specialize::*;
pub use lowering::*;
pub use printer::*;
