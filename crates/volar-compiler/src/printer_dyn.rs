//! Dynamic IR Printer
//!
//! This module transforms the IR to produce dynamic Rust code.

use crate::lowering_dyn::lower_module_dyn;
use crate::printer::{print_module, print_module_with_deps};
use crate::ir::IrModule;
use crate::manifest::TypeManifest;

#[cfg(feature = "std")]
use std::string::String;
#[cfg(not(feature = "std"))]
use alloc::string::String;

/// Main entry point for generating dynamic Rust code
pub fn print_module_rust_dyn(module: &IrModule) -> String {
    let lowered = lower_module_dyn(module);
    print_module(&lowered)
}

/// Generate dynamic Rust code with dependency manifests providing context.
pub fn print_module_rust_dyn_with_deps(module: &IrModule, deps: &[TypeManifest]) -> String {
    let lowered = lower_module_dyn(module);
    print_module_with_deps(&lowered, deps)
}
