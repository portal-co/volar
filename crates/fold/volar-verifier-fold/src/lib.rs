// @reliability: experimental
//! @ai: assisted
//! **Prove-the-verifier integration: compile-time side.** The build-side
//! terminals that lower a woven `Transparent` VOLE verifier `IrModule` to an
//! executable substrate — the compile-time counterpart to
//! `volar-verifier-iop-runtime`, which owns everything that *executes* the
//! result (see that crate's doc for the split).
//!
//! Both terminals are backend-agnostic: they lower whatever
//! `Tagged<Transparent, IrModule<IrFunction>>` they're given, regardless of
//! which `VerifierTraceSink` (see `crates/compiler/volar-weaver/src/vole.rs`)
//! wove it — no sink-specific bare names appear in either function.
//!
//! - [`emit_verifier_c`]: lower to **C source** via [`volar_c_backend::CBackend`].
//!   Unaffected by the `u128`/`LirType` gap (see `docs/agent-context/lir-u128-support.md`)
//!   as long as the module doesn't reference curve/`u128` spec functions — the
//!   VOLE gate-check path itself is fine.
//! - [`emit_verifier_rust`]: lower to **Rust source** via
//!   [`volar_weaver::vole::print_weaved_vole_module`] — the terminal for an
//!   `IopSink`-woven verifier (see that sink's doc), whose `IopChallenge`/
//!   `IopAccumulator`/etc. bare names can't go through the C/LIR path today.
//!   Same shape as the repo's other "print → real `rustc`" test harnesses
//!   (`AGENTS.md` rule 2) — `volar-verifier-iop-runtime`'s `run_iop_verifier`
//!   is what actually compiles and runs it.
//!
//! ## Discipline
//!
//! Both terminals take a [`Transparent`]-tagged module only: the verifier is
//! proven *without* zero-knowledge because the inner VOLE proof already
//! accounts for it — see `docs/agent-context/discipline.md` and
//! `docs/prove-the-verifier-iop.md`.

use volar_compiler::ir::{IrFunction, IrModule};
use volar_c_backend::CBackend;
use volar_discipline::{Tagged, Transparent};
use volar_lir_codegen::lower_module_with_opts;
use volar_lir_codegen::mono::MonoEnv;

/// **Pipeline terminal (C leg).** Lower a woven [`Transparent`] verifier
/// `IrModule` to **C source** via [`CBackend`] — the executable substrate when
/// the LLVM backend is unavailable.  Same lowering recipe as
/// `volar-c-backend`'s VOLE end-to-end tests
/// (`CBackend::new` → [`lower_module_with_opts`] → `finish`).
///
/// `env` carries the monomorphisation context (build it with
/// [`MonoEnv::new`]).  The discipline tag guarantees we never hand a ZK prover
/// module to the transparent C terminal.
pub fn emit_verifier_c(
    verifier: &Tagged<Transparent, IrModule<IrFunction>>,
    env: &MonoEnv,
) -> String {
    let mut backend = CBackend::new();
    lower_module_with_opts(verifier.inner(), &mut backend, env);
    backend.finish()
}

/// **Pipeline terminal (Rust leg).** Lower a woven [`Transparent`] verifier
/// `IrModule` to **Rust source** via
/// [`volar_weaver::vole::print_weaved_vole_module`] — real `rustc` is the
/// executable substrate here, not the C/LIR pipeline (see this module's
/// doc). The returned source still references any bare, externally-resolved
/// names a [`volar_weaver::vole::VerifierTraceSink`] introduced (e.g.
/// `IopChallenge`/`IopAccumulator`/`iop_fold_gate` for
/// [`volar_weaver::vole::IopSink`]) — `volar-verifier-iop-runtime` supplies
/// those when it actually compiles and runs this.
pub fn emit_verifier_rust(verifier: &Tagged<Transparent, IrModule<IrFunction>>) -> String {
    volar_weaver::vole::print_weaved_vole_module(verifier.inner())
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_compiler::ir::{
        ExternalKind, IrBlock, IrExpr, IrExprKind, IrFunction, IrLit, IrModule, IrType, PrimitiveType,
    };

    /// Minimal woven-verifier stand-in: a single function the C backend can
    /// lower without spec linkage.  Exercises [`emit_verifier_c`] end-to-end
    /// (the real verifier→C path is covered by `volar-c-backend`'s vole tests).
    fn minimal_module() -> Tagged<Transparent, IrModule<IrFunction>> {
        let func = IrFunction { no_inline: false,
            name: "verifier_ok".into(),
            module_path: vec![],
            generics: vec![],
            receiver: None,
            params: vec![],
            return_type: Some(IrType::Primitive(PrimitiveType::Bool)),
            where_clause: vec![],
            body: IrBlock {
                stmts: vec![],
                expr: Some(Box::new(IrExpr::new(IrExprKind::Lit(IrLit::Bool(true)), (), None))),
            },
            external_kind: ExternalKind::Normal,
        };
        Tagged::seal(IrModule {
            name: "woven_verifier".into(),
            functions: vec![func],
            structs: vec![],
            enums: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],
            consts: vec![],
        })
    }

    #[test]
    fn emit_verifier_c_produces_nonempty_source() {
        let module = minimal_module();
        let env = MonoEnv::new("verifier");
        let c_src = emit_verifier_c(&module, &env);
        assert!(!c_src.is_empty(), "C backend must emit source for the verifier module");
        assert!(c_src.contains("verifier_ok"), "emitted C should name the verifier function");
    }

    #[test]
    fn emit_verifier_rust_produces_nonempty_source() {
        let module = minimal_module();
        let rust_src = emit_verifier_rust(&module);
        assert!(!rust_src.is_empty(), "Rust printer must emit source for the verifier module");
        assert!(rust_src.contains("verifier_ok"), "emitted Rust should name the verifier function");
    }
}
