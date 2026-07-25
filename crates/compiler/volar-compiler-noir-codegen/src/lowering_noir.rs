//! Pre-print validation pass.
//!
//! Structural decisions in this codebase (dead-code pruning, shadow-fixing,
//! generic-length lowering) happen as discrete AST passes before printing,
//! leaving the printer itself infallible — this pass follows the same
//! precedent for loop-bound legality, which is exactly this kind of
//! whole-function structural property that wants to be checked once, up
//! front, and short-circuit the entire `print_module_noir` call before any
//! text is generated.
//!
//! Note: `deshadow_module`-style pre-pass renaming (run by the TS printer
//! before it walks the tree) is not yet wired in here — hand-built v1 test
//! fixtures don't exercise shadowing, so this is a tracked gap, not a
//! silent one.

#[cfg(feature = "std")]
use std::{format, string::String, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::{format, string::String, vec::Vec};

use volar_compiler::ir::{
    IrBlock, IrExpr, IrExprKind, IrFunction, IrGenericParam, IrModule, IrStmtKind,
};

use crate::const_eval::eval_const_expr;
use crate::error::NoirCodegenError;

/// Walk every function body in `module` and collect *all* violations of the
/// v1 Noir backend's supported subset (not fail-fast — better diagnostics
/// for a multi-function module). `Ok(())` means every function body is
/// safe to print as-is.
pub fn validate_module(module: &IrModule<IrFunction>) -> Result<(), Vec<NoirCodegenError>> {
    let mut errors = Vec::new();
    for function in &module.functions {
        validate_function(function, &mut errors);
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn validate_function(function: &IrFunction, errors: &mut Vec<NoirCodegenError>) {
    validate_block(&function.body, &function.name, &function.generics, errors);
}

fn validate_block(
    block: &IrBlock,
    fn_name: &str,
    generics: &[IrGenericParam],
    errors: &mut Vec<NoirCodegenError>,
) {
    for stmt in &block.stmts {
        match &stmt.kind {
            IrStmtKind::Let { init: Some(e), .. } => validate_expr(e, fn_name, generics, errors),
            IrStmtKind::Let { init: None, .. } => {}
            IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => validate_expr(e, fn_name, generics, errors),
            _ => {}
        }
    }
    if let Some(e) = &block.expr {
        validate_expr(e, fn_name, generics, errors);
    }
}

fn describe(expr: &IrExpr) -> String {
    format!("{:?}", expr.kind)
}

/// Recursively validate an expression tree. `WhileLoop` is rejected
/// unconditionally (no exceptions — Noir constrained code cannot express
/// one, ever) and its subtree is *not* recursed into further, to avoid
/// cascading spurious errors from the same already-reported subtree.
fn validate_expr(
    expr: &IrExpr,
    fn_name: &str,
    generics: &[IrGenericParam],
    errors: &mut Vec<NoirCodegenError>,
) {
    match &expr.kind {
        IrExprKind::WhileLoop { .. } => {
            errors.push(NoirCodegenError::UnsupportedWhileLoop {
                function: fn_name.into(),
            });
        }

        IrExprKind::BoundedLoop {
            start, end, body, ..
        } => {
            if eval_const_expr(start, generics).is_none() {
                errors.push(NoirCodegenError::NonConstantLoopBound {
                    function: fn_name.into(),
                    reason: format!("start bound `{}` is not a compile-time constant", describe(start)),
                });
            }
            if eval_const_expr(end, generics).is_none() {
                errors.push(NoirCodegenError::NonConstantLoopBound {
                    function: fn_name.into(),
                    reason: format!("end bound `{}` is not a compile-time constant", describe(end)),
                });
            }
            validate_expr(start, fn_name, generics, errors);
            validate_expr(end, fn_name, generics, errors);
            validate_block(body, fn_name, generics, errors);
        }

        IrExprKind::IterLoop {
            collection, body, ..
        } => {
            // v1 policy: only a literal fixed-size collection has a length
            // derivable without a type-checked IR (IrExpr carries no type
            // annotation at this layer). A `Var`/computed collection would
            // need real type inference to size — tracked future work, not
            // silently accepted.
            let known_length = matches!(
                collection.kind,
                IrExprKind::FixedArray(_) | IrExprKind::Array(_) | IrExprKind::Repeat { .. }
            );
            if !known_length {
                errors.push(NoirCodegenError::NonConstantLoopBound {
                    function: fn_name.into(),
                    reason: format!(
                        "collection `{}` has no compile-time-known length without type \
                         inference (v1 only supports iterating a literal fixed-size array)",
                        describe(collection)
                    ),
                });
            }
            validate_expr(collection, fn_name, generics, errors);
            validate_block(body, fn_name, generics, errors);
        }

        // Everything else: just recurse to find nested loops.
        IrExprKind::Lit(_) | IrExprKind::Var(_) | IrExprKind::Path { .. } | IrExprKind::Continue
        | IrExprKind::Unreachable | IrExprKind::TypenumUsize { .. } | IrExprKind::LengthOf(_) => {}

        IrExprKind::Binary { left, right, .. } | IrExprKind::Assign { left, right }
        | IrExprKind::AssignOp { left, right, .. } => {
            validate_expr(left, fn_name, generics, errors);
            validate_expr(right, fn_name, generics, errors);
        }

        IrExprKind::Unary { expr, .. }
        | IrExprKind::Return(Some(expr))
        | IrExprKind::Break(Some(expr))
        | IrExprKind::Cast { expr, .. }
        | IrExprKind::Try(expr)
        | IrExprKind::Field { base: expr, .. } => {
            validate_expr(expr, fn_name, generics, errors);
        }

        IrExprKind::Return(None) | IrExprKind::Break(None) => {}

        IrExprKind::MethodCall { receiver, args, .. } => {
            validate_expr(receiver, fn_name, generics, errors);
            for a in args {
                validate_expr(a, fn_name, generics, errors);
            }
        }
        IrExprKind::Call { func, args } => {
            validate_expr(func, fn_name, generics, errors);
            for a in args {
                validate_expr(a, fn_name, generics, errors);
            }
        }
        IrExprKind::Index { base, index } => {
            validate_expr(base, fn_name, generics, errors);
            validate_expr(index, fn_name, generics, errors);
        }
        IrExprKind::StructExpr { fields, rest, .. } => {
            for (_, e) in fields {
                validate_expr(e, fn_name, generics, errors);
            }
            if let Some(r) = rest {
                validate_expr(r, fn_name, generics, errors);
            }
        }
        IrExprKind::Tuple(es) | IrExprKind::Array(es) | IrExprKind::FixedArray(es) => {
            for e in es {
                validate_expr(e, fn_name, generics, errors);
            }
        }
        IrExprKind::Repeat { elem, len } => {
            validate_expr(elem, fn_name, generics, errors);
            validate_expr(len, fn_name, generics, errors);
        }
        IrExprKind::ArrayGenerate { body, .. } => {
            validate_expr(body, fn_name, generics, errors);
        }
        IrExprKind::DefaultValue { .. } => {}
        IrExprKind::IterPipeline(_) => {
            // Desugared iterator pipelines are out of scope for v1 handling
            // beyond what's already flagged elsewhere; left as a no-op walk.
        }
        IrExprKind::RawMap { receiver, body, .. } => {
            validate_expr(receiver, fn_name, generics, errors);
            validate_expr(body, fn_name, generics, errors);
        }
        IrExprKind::RawZip { left, right, body, .. } => {
            validate_expr(left, fn_name, generics, errors);
            validate_expr(right, fn_name, generics, errors);
            validate_expr(body, fn_name, generics, errors);
        }
        IrExprKind::RawFold { receiver, init, body, .. } => {
            validate_expr(receiver, fn_name, generics, errors);
            validate_expr(init, fn_name, generics, errors);
            validate_expr(body, fn_name, generics, errors);
        }
        IrExprKind::Block(b) => validate_block(b, fn_name, generics, errors),
        IrExprKind::If { cond, then_branch, else_branch } => {
            validate_expr(cond, fn_name, generics, errors);
            validate_block(then_branch, fn_name, generics, errors);
            if let Some(e) = else_branch {
                validate_expr(e, fn_name, generics, errors);
            }
        }
        IrExprKind::Match { expr, arms } => {
            validate_expr(expr, fn_name, generics, errors);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    validate_expr(g, fn_name, generics, errors);
                }
                validate_expr(&arm.body, fn_name, generics, errors);
            }
        }
        IrExprKind::Closure { body, .. } => validate_expr(body, fn_name, generics, errors),
        IrExprKind::Range { start, end, .. } => {
            if let Some(s) = start {
                validate_expr(s, fn_name, generics, errors);
            }
            if let Some(e) = end {
                validate_expr(e, fn_name, generics, errors);
            }
        }

        // `IrExprKind` is `#[non_exhaustive]`; every variant known at the
        // time this was written is handled above.
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_compiler::ir::{ExternalKind, IrLit, SpecBinOp};

    fn expr(kind: IrExprKind) -> IrExpr {
        IrExpr { kind, prov: (), side: None }
    }

    fn stmt_expr(kind: IrExprKind) -> volar_compiler::ir::IrStmt {
        volar_compiler::ir::IrStmt {
            kind: IrStmtKind::Expr(expr(kind)),
            prov: (),
            side: None,
        }
    }

    fn function_with_body(name: &str, body: IrBlock) -> IrFunction {
        IrFunction {
            name: name.into(),
            module_path: Vec::new(),
            generics: Vec::new(),
            receiver: None,
            params: Vec::new(),
            return_type: None,
            where_clause: Vec::new(),
            body,
            external_kind: ExternalKind::Normal,
            no_inline: false,
        }
    }

    #[test]
    fn while_loop_is_rejected() {
        let body = IrBlock {
            stmts: vec![stmt_expr(IrExprKind::WhileLoop {
                cond: Box::new(expr(IrExprKind::Lit(IrLit::Bool(true)))),
                body: IrBlock { stmts: Vec::new(), expr: None },
            })],
            expr: None,
        };
        let f = function_with_body("has_while", body);
        let module = IrModule { name: "m".into(), functions: vec![f], ..Default::default() };
        let err = validate_module(&module).unwrap_err();
        assert_eq!(err.len(), 1);
        assert!(matches!(err[0], NoirCodegenError::UnsupportedWhileLoop { .. }));
    }

    #[test]
    fn literal_bound_loop_is_accepted() {
        let body = IrBlock {
            stmts: vec![stmt_expr(IrExprKind::BoundedLoop {
                var: "i".into(),
                start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
                end: Box::new(expr(IrExprKind::Lit(IrLit::Int(10)))),
                inclusive: false,
                body: IrBlock { stmts: Vec::new(), expr: None },
            })],
            expr: None,
        };
        let f = function_with_body("counts", body);
        let module = IrModule { name: "m".into(), functions: vec![f], ..Default::default() };
        assert!(validate_module(&module).is_ok());
    }

    #[test]
    fn witness_derived_bound_is_rejected() {
        // `end` is a plain runtime `Var` (e.g. a function parameter), not a
        // generic-const reference — this is exactly the "runtime-but-fixed"
        // bound Volar's AST permits but Noir cannot express.
        let body = IrBlock {
            stmts: vec![stmt_expr(IrExprKind::BoundedLoop {
                var: "i".into(),
                start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
                end: Box::new(expr(IrExprKind::Var("n".into()))),
                inclusive: false,
                body: IrBlock { stmts: Vec::new(), expr: None },
            })],
            expr: None,
        };
        let f = function_with_body("counts_to_n", body);
        let module = IrModule { name: "m".into(), functions: vec![f], ..Default::default() };
        let err = validate_module(&module).unwrap_err();
        assert_eq!(err.len(), 1);
        assert!(matches!(err[0], NoirCodegenError::NonConstantLoopBound { .. }));
    }

    #[test]
    fn generic_const_bound_loop_is_accepted() {
        let generics = vec![IrGenericParam {
            name: "N".into(),
            kind: volar_compiler::ir::IrGenericParamKind::Const,
            const_ty: None,
            bounds: Vec::new(),
            default: None,
        }];
        let body = IrBlock {
            stmts: vec![stmt_expr(IrExprKind::BoundedLoop {
                var: "i".into(),
                start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
                end: Box::new(expr(IrExprKind::Var("N".into()))),
                inclusive: false,
                body: IrBlock { stmts: Vec::new(), expr: None },
            })],
            expr: None,
        };
        let mut f = function_with_body("counts_to_n_generic", body);
        f.generics = generics;
        let module = IrModule { name: "m".into(), functions: vec![f], ..Default::default() };
        assert!(validate_module(&module).is_ok());
    }

    #[test]
    fn no_panic_on_rejected_module() {
        // Guard against the one behavior this pass must never exhibit:
        // panicking instead of returning `Err`.
        let body = IrBlock {
            stmts: vec![stmt_expr(IrExprKind::WhileLoop {
                cond: Box::new(expr(IrExprKind::Lit(IrLit::Bool(true)))),
                body: IrBlock {
                    stmts: vec![stmt_expr(IrExprKind::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(expr(IrExprKind::Var("x".into()))),
                        right: Box::new(expr(IrExprKind::Lit(IrLit::Int(1)))),
                    })],
                    expr: None,
                },
            })],
            expr: None,
        };
        let f = function_with_body("has_while_with_body", body);
        let module = IrModule { name: "m".into(), functions: vec![f], ..Default::default() };
        let result = std::panic::catch_unwind(|| validate_module(&module));
        assert!(result.is_ok(), "validate_module must not panic");
    }
}
