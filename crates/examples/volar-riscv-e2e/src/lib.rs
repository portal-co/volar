// @reliability: experimental
//! @ai: assisted
//! First end-to-end test: a RISC-V interpreter, proven through the real
//! WASM -> Volar-IR pipeline. See `docs/plan-to-have-an-synthetic-pike.md`
//! (kept outside this repo) for the full milestone plan.
//!
//! This module owns the one piece every milestone needs: turning a real
//! WASM binary into a fully-expanded `portal_pc_waffle_ir::Module` that
//! `volar_vaffle_target::lower_waffle_module` can consume.
//!
//! `portal_pc_waffle_frontend::from_wasm_bytes` returns functions as
//! `FuncDecl::Lazy` (bodies not yet decoded from the original bytecode).
//! `lower_waffle_module` only matches `FuncDecl::Body` — a `Lazy` function
//! is silently skipped, with no error reported. Every function must be
//! expanded via `expand_func` first.

use portal_pc_waffle_frontend::{FrontendOptions, Module as WModule, expand_func};

pub mod commit_mem_e2e;
pub mod interp;
pub mod mem_probe;
#[cfg(test)]
pub(crate) mod memory_check_driver;
pub mod signature;
#[cfg(test)]
pub(crate) mod split_driver;
pub mod wat_gen;

/// Parse a WASM binary into a fully-expanded WAFFLE `Module`, ready for
/// `volar_vaffle_target::lower_waffle_module`.
pub fn parse_and_expand(wasm_bytes: &[u8]) -> anyhow::Result<WModule<'_>> {
    let mut module = portal_pc_waffle_frontend::from_wasm_bytes(
        wasm_bytes,
        &FrontendOptions { debug: false },
    )?;
    let func_ids: Vec<_> = module.funcs.entries().map(|(id, _)| id).collect();
    for id in func_ids {
        expand_func(&mut module, id)?;
    }
    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_vaffle_target::VaffleTarget;
    use volar_vaffle_target::waffle_lower::lower_waffle_module;
    use volar_vaffle_target::import_config::WaffleImportConfig;

    /// Foundational spike: a real, hand-authored WAT binary, parsed via the
    /// real WASM frontend, lowered via the real `lower_waffle_module` --
    /// nothing hand-built at the WAFFLE-IR level. This is the smallest
    /// possible confirmation that "hand-authored WAT -> real .wasm bytes ->
    /// WAFFLE parse -> Volar IR" works at all, before the RISC-V
    /// interpreter is layered on top.
    #[test]
    fn trivial_wat_module_lowers_with_no_errors() {
        let wat_src = r#"
            (module
                (func (export "answer") (result i32)
                    i32.const 42))
        "#;
        let wasm_bytes = wat::parse_str(wat_src).expect("wat should assemble");

        let module = parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&module, &mut target, &WaffleImportConfig::default());

        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");
        assert_eq!(target.module.funcs.len(), 1, "expected exactly one lowered function");
    }
}
