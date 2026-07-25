//! # Volar Noir Codegen
//!
//! `IrModule` → Noir source, hooking in at the AST layer (architecturally
//! parallel to `volar_compiler_passes::print_module_typescript`/
//! `print_module_rust_dyn`), not at the `LirTarget`/LIR layer used by the
//! C/LLVM backends. See `docs/noir-backend.md` for the full rationale.

#[cfg(feature = "std")]
extern crate std;

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod const_eval;
pub mod error;
pub mod lowering_noir;
pub mod printer_noir;

pub use error::NoirCodegenError;
pub use printer_noir::print_module_noir;
