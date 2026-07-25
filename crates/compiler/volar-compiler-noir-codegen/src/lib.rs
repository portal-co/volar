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

#[cfg(feature = "std")]
use std::{string::String, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

pub mod const_eval;
pub mod error;
pub mod lowering_noir;
pub mod printer_noir;

pub use error::NoirCodegenError;
pub use printer_noir::print_module_noir;

/// Print only the functions/methods transitively reachable from `seeds`
/// (types are always emitted in full), mirroring
/// `print_module_typescript_seeded`'s intent. Reuses
/// `volar_compiler::reachability::compute_reachable` unchanged.
///
/// This is a simpler filter than the TS printer's own `_seeded` variant
/// (which additionally threads witness maps, erased-type-param tracking,
/// and name-collision resolution through the reachability data for
/// TS-specific reasons) -- just function/method-name filtering, since
/// none of that TS-specific machinery applies to Noir.
pub fn print_module_noir_seeded(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    seeds: &[&str],
) -> Result<String, Vec<NoirCodegenError>> {
    use volar_compiler::reachability::compute_reachable;

    let reachable = compute_reachable(module, seeds);

    let mut pruned = module.clone();
    pruned.functions.retain(|f| reachable.fns.contains(&f.name));
    for imp in &mut pruned.impls {
        imp.items.retain(|item| match item {
            volar_compiler::ir::IrImplItem::Method(f) => reachable.method_names.contains(&f.name),
            volar_compiler::ir::IrImplItem::AssociatedType { .. } => true,
        });
    }
    pruned.impls.retain(|imp| !imp.items.is_empty());

    print_module_noir(&pruned)
}
