//! # Volar Compiler Passes
//!
//! Analysis and transformation passes over the `volar-compiler` IR.

#[cfg(feature = "std")]
extern crate std;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(feature = "std")]
use std::string::String;

pub mod const_analysis;
pub mod dump_ir;
pub mod lowering;
pub mod lowering_dyn;
pub mod unpack_packed_bits;

pub use const_analysis::*;
pub use lowering::*;
pub use unpack_packed_bits::unpack_bits_in_type;

/// Generate dynamic Rust code by lowering type-level lengths to runtime witnesses.
pub fn print_module_rust_dyn(module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer::print_module(&lowered)
}

/// Generate dynamic Rust code with dependency manifests providing context.
pub fn print_module_rust_dyn_with_deps(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    deps: &[volar_compiler::manifest::TypeManifest],
) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer::print_module_with_deps(&lowered, deps)
}

/// Generate TypeScript code by lowering type-level lengths to runtime witnesses.
pub fn print_module_typescript(module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer_ts::print_module_ts(&lowered)
}

/// Generate TypeScript code with dependency manifests providing context.
pub fn print_module_typescript_with_deps(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    deps: &[volar_compiler::manifest::TypeManifest],
) -> String {
    let _ = deps;
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer_ts::print_module_ts(&lowered)
}
