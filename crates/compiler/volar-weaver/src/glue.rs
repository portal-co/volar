// @reliability: experimental
// @ai: assisted
//! Continuation glue: bind committed state across a *skip* (fast-forwarded loop
//! iterations) or a *gap* (network outage) so a resumed proof segment stays
//! linked to the pre-skip / pre-gap commitments.  See
//! `docs/vole-continuation-bridge.md` §C.
//!
//! Two modes, unified behind [`GlueMode`]:
//!
//! - [`GlueMode::AdditiveHashCarry`] — the **bridge** path (replay-from-anchor).
//!   Memory is a single additive-multiset-hash field element already carried as
//!   loop state, and the resumed segment replays under the *same* VOLE anchor, so
//!   there is **nothing to re-commit**: the glue is the identity carry (the
//!   prover threads the accumulators unchanged, the verifier has nothing extra to
//!   check — consistency is closed by the existing drain).
//!
//! - [`GlueMode::XorRekey`] — the **dynamic-skip** path.  A committed snapshot is
//!   re-randomised under a fresh one-time-pad `key` wire (`rekeyed = snapshot +
//!   key`, a free VOLE `Add`/XOR) to start a new segment, and bound to the prior
//!   segment by the free linear consistency check
//!   `vole_rekey_verifier_check` (`q_rekeyed == q_snapshot + q_key`, no `Δ`).
//!   The key wires are fresh commitments the transport supplies
//!   (`ResilientVoleTransport::fresh_commit`).
//!
//! Both reuse the tested primitives in `volar_spec::vole::bridge`
//! (`vole_rekey_prover` / `vole_rekey_verifier_check`); this module just emits the
//! per-boundary-wire IR for an `ell`-wide carried state.

use alloc::{boxed::Box, format, string::String, vec, vec::Vec};

use volar_compiler::{
    ir::{
        ExternalKind, IrAnyFunction, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgModule,
        IrCfgTerminator, IrExpr, IrGenericParam, IrGenericParamKind, IrLit, IrModule, IrParam,
        IrPattern, IrStmt, IrType, IrWherePredicate, SpecBinOp,
    },
    linkage::LinkageSystem,
};

use crate::{clone_expr, ref_expr, var};
use crate::net::{
    add_output_t, array_size_bound, bool_type, clone_t, default_t, delta_type, mul_output_t,
    q_type, ref_to, vole_array_t_bound, vope_type,
};

/// Which continuation mechanism the glue emits.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GlueMode {
    /// Bridge / replay-from-anchor: identity carry (no re-commitment).
    AdditiveHashCarry,
    /// Dynamic skip: XOR-key re-commitment + free linear binding.
    XorRekey,
}

/// `<N: ArraySize, T>` where `N: VoleArray<T>`, `T: Clone + Add + Mul + Default +
/// PartialEq` — no transport generic (the glue is pure VOLE arithmetic).
fn glue_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    use volar_compiler::ir::{IrTraitBound, MathTrait, TraitKind};
    let generics = vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![array_size_bound()],
            default: None,
        },
        IrGenericParam {
            name: "T".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![],
            default: None,
        },
    ];
    let partial_eq_t = IrTraitBound {
        trait_kind: TraitKind::Math(MathTrait::PartialEq),
        type_args: vec![],
        assoc_bindings: vec![],
    };
    let where_clause = vec![
        IrWherePredicate::TypeBound {
            ty: IrType::TypeParam("N".into()),
            bounds: vec![vole_array_t_bound()],
        },
        IrWherePredicate::TypeBound {
            ty: IrType::TypeParam("T".into()),
            bounds: vec![clone_t(), add_output_t(), mul_output_t(), default_t(), partial_eq_t],
        },
    ];
    (generics, where_clause)
}

fn let_stmt(name: &str, init: IrExpr) -> IrStmt {
    IrStmt::Let { pattern: IrPattern::ident(name), ty: None, init: Some(init) }
}

/// `volar_spec::vole::bridge::vole_rekey_prover(snapshot, key)` — `wire + key`.
fn rekey_prover_call(snapshot: &str, key: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec![
                "volar_spec".into(), "vole".into(), "bridge".into(), "vole_rekey_prover".into(),
            ],
            type_args: vec![],
        }),
        args: vec![clone_expr(var(snapshot)), clone_expr(var(key))],
    }
}

/// `volar_spec::vole::bridge::vole_rekey_verifier_check(&q_snapshot, &q_key, &q_rekeyed)`
fn rekey_verifier_call(q_snapshot: &str, q_key: &str, q_rekeyed: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec![
                "volar_spec".into(),
                "vole".into(),
                "bridge".into(),
                "vole_rekey_verifier_check".into(),
            ],
            type_args: vec![],
        }),
        args: vec![ref_expr(var(q_snapshot)), ref_expr(var(q_key)), ref_expr(var(q_rekeyed))],
    }
}

// ============================================================================
// Prover
// ============================================================================

/// Weave the **prover** side of the continuation glue for an `ell`-wide carried
/// boundary state.
///
/// - [`GlueMode::XorRekey`]: signature
///   `fn continuation_glue_<NAME><N,T>(snapshot_0: Vope, .., key_0: Vope, ..) ->
///   (Vope, ..)` returning the re-keyed fresh-segment input wires.
/// - [`GlueMode::AdditiveHashCarry`]: signature
///   `fn continuation_glue_<NAME><N,T>(snapshot_0: Vope, ..) -> (Vope, ..)`
///   returning the snapshot unchanged (identity carry).
pub fn weave_continuation_glue_prover(
    ell: usize,
    mode: GlueMode,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(ell >= 1, "continuation glue needs at least one boundary wire");
    let (generics, where_clause) = glue_generics_and_where();

    let mut func_params: Vec<IrParam> =
        (0..ell).map(|i| IrParam { name: format!("snapshot_{i}"), ty: vope_type() }).collect();
    if mode == GlueMode::XorRekey {
        for i in 0..ell {
            func_params.push(IrParam { name: format!("key_{i}"), ty: vope_type() });
        }
    }

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut out_names: Vec<String> = Vec::with_capacity(ell);
    for i in 0..ell {
        match mode {
            GlueMode::XorRekey => {
                let nm = format!("rekeyed_{i}");
                stmts.push(let_stmt(&nm, rekey_prover_call(&format!("snapshot_{i}"), &format!("key_{i}"))));
                out_names.push(nm);
            }
            GlueMode::AdditiveHashCarry => {
                // Identity carry: re-bind to a fresh name so the return is uniform.
                let nm = format!("carry_{i}");
                stmts.push(let_stmt(&nm, clone_expr(var(&format!("snapshot_{i}")))));
                out_names.push(nm);
            }
        }
    }

    let ret_type = IrType::Tuple(vec![vope_type(); ell]);
    let block0 = IrCfgBlock {
        params: vec![],
        stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(IrExpr::Tuple(
            out_names.iter().map(|n| var(n)).collect(),
        ))),
    };
    finish_module(block0, generics, where_clause, func_params, Some(ret_type), name, linkage)
}

// ============================================================================
// Verifier
// ============================================================================

/// Weave the **verifier** side of the continuation glue.
///
/// - [`GlueMode::XorRekey`]: signature
///   `fn continuation_glue_verify_<NAME><N,T>(q_snapshot_0: Q, .., q_key_0: Q, ..,
///   q_rekeyed_0: Q, ..) -> bool` — `&&`-folds
///   `vole_rekey_verifier_check(&q_snapshot_i, &q_key_i, &q_rekeyed_i)`.
/// - [`GlueMode::AdditiveHashCarry`]: returns `true` (the carry is closed by the
///   existing drain; nothing extra to check here).
pub fn weave_continuation_glue_verifier(
    ell: usize,
    mode: GlueMode,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(ell >= 1, "continuation glue needs at least one boundary wire");
    let (generics, where_clause) = glue_generics_and_where();

    let mut func_params: Vec<IrParam> = vec![IrParam { name: "delta".into(), ty: ref_to(delta_type()) }];
    for i in 0..ell {
        func_params.push(IrParam { name: format!("q_snapshot_{i}"), ty: q_type() });
    }
    if mode == GlueMode::XorRekey {
        for i in 0..ell {
            func_params.push(IrParam { name: format!("q_key_{i}"), ty: q_type() });
        }
        for i in 0..ell {
            func_params.push(IrParam { name: format!("q_rekeyed_{i}"), ty: q_type() });
        }
    }

    let mut stmts: Vec<IrStmt> = vec![IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(IrExpr::Lit(IrLit::Bool(true))),
    }];
    if mode == GlueMode::XorRekey {
        for i in 0..ell {
            let ok = format!("ok_{i}");
            stmts.push(let_stmt(
                &ok,
                rekey_verifier_call(
                    &format!("q_snapshot_{i}"),
                    &format!("q_key_{i}"),
                    &format!("q_rekeyed_{i}"),
                ),
            ));
            stmts.push(IrStmt::Semi(IrExpr::Assign {
                left: Box::new(var("all_ok")),
                right: Box::new(IrExpr::Binary {
                    op: SpecBinOp::And,
                    left: Box::new(var("all_ok")),
                    right: Box::new(var(&ok)),
                }),
            }));
        }
    }

    let block0 = IrCfgBlock {
        params: vec![],
        stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(var("all_ok"))),
    };
    finish_module(
        block0, generics, where_clause, func_params, Some(bool_type()),
        &format!("verify_{name}"), linkage,
    )
}

fn finish_module(
    block0: IrCfgBlock,
    generics: Vec<IrGenericParam>,
    where_clause: Vec<IrWherePredicate>,
    func_params: Vec<IrParam>,
    return_type: Option<IrType>,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    let func = IrCfgFunction {
        name: format!("continuation_glue_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type,
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_continuation_glue_{name}"),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply_cfg(&mut module);
    }
    module
}

/// Print a continuation-glue CFG module to self-contained Rust source.
pub fn print_glue_module(module: &IrCfgModule) -> String {
    use volar_compiler::printer::{CfgModuleWriter, DisplayRust};
    use alloc::fmt::Write as _;

    let mut body = String::new();
    let _ = write!(body, "{}", DisplayRust(CfgModuleWriter { module, emit_async: false }));

    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "extern crate alloc;\n",
        "use core::ops::{Add, Mul};\n",
        "use hybrid_array::{Array, ArraySize};\n",
        "use cipher::consts::U1;\n",
        "use volar_spec::vole::{Delta, Q, Vope, VoleArray};\n",
        "use volar_spec::vole::bridge::{vole_rekey_prover, vole_rekey_verifier_check};\n",
        "\n",
    );

    let mut out = String::with_capacity(preamble.len() + body.len());
    out.push_str(preamble);
    out.push_str(&body);
    out
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::tests_common::run_compile_check_net;

    #[test]
    fn test_glue_xor_rekey_prover_compiles() {
        let m = weave_continuation_glue_prover(3, GlueMode::XorRekey, "skip", None);
        let code = print_glue_module(&m);
        run_compile_check_net(&code, "glue_xor_rekey_prover");
        assert!(code.contains("vole_rekey_prover("), "rekey applied per boundary wire");
    }

    #[test]
    fn test_glue_xor_rekey_verifier_compiles() {
        let m = weave_continuation_glue_verifier(3, GlueMode::XorRekey, "skip", None);
        let code = print_glue_module(&m);
        run_compile_check_net(&code, "glue_xor_rekey_verifier");
        assert!(code.contains("vole_rekey_verifier_check("), "binding checked per wire");
    }

    #[test]
    fn test_glue_additive_carry_is_identity() {
        let mp = weave_continuation_glue_prover(2, GlueMode::AdditiveHashCarry, "bridge", None);
        let code = print_glue_module(&mp);
        run_compile_check_net(&code, "glue_carry_prover");
        // Identity carry: no re-commitment in the bridge path.
        assert!(!code.contains("vole_rekey_prover("), "bridge carry does not re-commit");
        let mv = weave_continuation_glue_verifier(2, GlueMode::AdditiveHashCarry, "bridge", None);
        let codev = print_glue_module(&mv);
        run_compile_check_net(&codev, "glue_carry_verifier");
        assert!(!codev.contains("vole_rekey_verifier_check("), "bridge carry has no binding check");
    }
}
