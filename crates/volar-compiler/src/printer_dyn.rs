//! Dynamic IR Printer
//!
//! This module transforms the IR to produce dynamic Rust code.

use crate::lowering_dyn::lower_module_dyn;
use crate::printer::print_module;
use crate::ir::IrModule;

#[cfg(feature = "std")]
use std::string::String;
#[cfg(not(feature = "std"))]
use alloc::string::String;

/// Main entry point for generating dynamic Rust code
pub fn print_module_rust_dyn(module: &IrModule) -> String {
    let lowered = lower_module_dyn(module);
    print_module(&lowered)
}
