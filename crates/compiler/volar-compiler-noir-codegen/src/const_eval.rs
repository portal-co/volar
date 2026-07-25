//! "Is this `IrExpr` a Noir compile-time constant?" analysis.
//!
//! This is deliberately *not* modeled after `volar-lir-codegen`'s
//! `concrete_usize_expr`/`lower_bounded_loop`, which require a value to
//! fully resolve to a literal post-monomorphization. Noir loop bounds are
//! allowed to be *symbolic* generic-const expressions (e.g. a numeric
//! generic `N`, or `N + 1`) — the printer needs the unresolved symbolic
//! form preserved as printable source text, not a resolved literal.

#[cfg(feature = "std")]
use std::{boxed::Box, string::String};

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, string::String};

use core::fmt;

use volar_compiler::ir::{
    ArrayLength, IrExpr, IrExprKind, IrGenericParam, IrGenericParamKind, IrLit, IrType, SpecBinOp,
};
use volar_compiler_passes::const_analysis::{classify_generic_with_aliases, GenericKind};

/// A Noir-compile-time-constant expression, resolved or still symbolic.
///
/// Kept as a small tree (not just `Option<bool>`) because the printer must
/// re-emit the *exact* source text of the bound — `N + 1` must print as
/// `N + 1`, not be force-resolved to a number or discarded.
#[derive(Debug, Clone, PartialEq)]
pub enum NoirConstExpr {
    Literal(i128),
    /// A bare reference to a still-generic const parameter (e.g. `N`).
    GenericRef(String),
    Binary {
        op: SpecBinOp,
        left: Box<NoirConstExpr>,
        right: Box<NoirConstExpr>,
    },
    /// `LengthOf(ArrayLength)` that resolved to one of the above. Kept
    /// distinct from `Literal`/`GenericRef` purely for readability at call
    /// sites; it prints identically to its inner expression.
    ArrayLen(Box<NoirConstExpr>),
}

impl fmt::Display for NoirConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NoirConstExpr::Literal(n) => write!(f, "{n}"),
            NoirConstExpr::GenericRef(name) => write!(f, "{name}"),
            NoirConstExpr::Binary { op, left, right } => {
                write!(f, "{left} {} {right}", bin_op_str(*op))
            }
            NoirConstExpr::ArrayLen(inner) => write!(f, "{inner}"),
        }
    }
}

fn bin_op_str(op: SpecBinOp) -> &'static str {
    match op {
        SpecBinOp::Add => "+",
        SpecBinOp::Sub => "-",
        SpecBinOp::Mul => "*",
        SpecBinOp::Div => "/",
        _ => unreachable!("eval_const_expr only accepts arithmetic ops"),
    }
}

fn is_arith_op(op: SpecBinOp) -> bool {
    matches!(op, SpecBinOp::Add | SpecBinOp::Sub | SpecBinOp::Mul | SpecBinOp::Div)
}

/// Is `name` a generic parameter that Noir can treat as a compile-time
/// numeric-generic reference? True for `const N: T` params, and for
/// `Type`-kind params classified `GenericKind::Length` by their bounds
/// (e.g. `K: ArrayLength<T>`).
fn is_const_generic_name(name: &str, generics: &[IrGenericParam]) -> bool {
    generics
        .iter()
        .any(|g| g.name == name && is_length_generic(g, generics))
}

fn is_length_generic(param: &IrGenericParam, all: &[IrGenericParam]) -> bool {
    if param.kind == IrGenericParamKind::Const {
        return true;
    }
    matches!(
        classify_generic_with_aliases(param, &[all], &[]),
        GenericKind::Length
    )
}

/// Map an `ArrayLength` (a fixed-size array's length, as carried by
/// `IrType::Array`) to a `NoirConstExpr`. `Projection` lengths (associated-
/// type lengths like `<B as Trait>::OutputSize`) are unsupported in v1 —
/// they need trait-resolution machinery beyond this layer's reach.
pub fn eval_array_length(len: &ArrayLength, _generics: &[IrGenericParam]) -> Option<NoirConstExpr> {
    match len {
        ArrayLength::Const(n) => Some(NoirConstExpr::Literal(*n as i128)),
        ArrayLength::TypeNum(tn) => Some(NoirConstExpr::Literal(tn.to_usize() as i128)),
        ArrayLength::TypeParam(name) => Some(NoirConstExpr::GenericRef(name.clone())),
        ArrayLength::Projection { .. } => None,
    }
}

/// Evaluate a type in "const position" (only meaningful for
/// `IrExprKind::TypenumUsize { ty }`, where `ty` names a length rather than
/// carrying a runtime value).
fn eval_type_as_const(ty: &IrType, generics: &[IrGenericParam]) -> Option<NoirConstExpr> {
    match ty {
        IrType::TypeParam(name) if is_const_generic_name(name, generics) => {
            Some(NoirConstExpr::GenericRef(name.clone()))
        }
        _ => None,
    }
}

/// Is `expr` a Noir compile-time constant — a literal, or a (possibly
/// still-generic) expression built only from const-generic references and
/// arithmetic over them?
///
/// Handles: integer literals; `Var`/single-segment `Path` naming a const
/// generic; `TypenumUsize`; `LengthOf`; `Binary` over `Add/Sub/Mul/Div`
/// where both sides are themselves const. Everything else is `None` —
/// notably `Unary`, `MethodCall`, `Call`, and any control-flow expression,
/// none of which are valid Noir loop-bound shapes.
pub fn eval_const_expr(expr: &IrExpr, generics: &[IrGenericParam]) -> Option<NoirConstExpr> {
    match &expr.kind {
        IrExprKind::Lit(IrLit::Int(n)) => Some(NoirConstExpr::Literal(*n)),
        IrExprKind::Var(name) if is_const_generic_name(name, generics) => {
            Some(NoirConstExpr::GenericRef(name.clone()))
        }
        IrExprKind::Path { segments, .. } if segments.len() == 1 => {
            let name = &segments[0];
            if is_const_generic_name(name, generics) {
                Some(NoirConstExpr::GenericRef(name.clone()))
            } else {
                None
            }
        }
        IrExprKind::TypenumUsize { ty } => eval_type_as_const(ty, generics),
        IrExprKind::LengthOf(len) => {
            eval_array_length(len, generics).map(|c| NoirConstExpr::ArrayLen(Box::new(c)))
        }
        IrExprKind::Binary { op, left, right } if is_arith_op(*op) => {
            let l = eval_const_expr(left, generics)?;
            let r = eval_const_expr(right, generics)?;
            Some(NoirConstExpr::Binary {
                op: *op,
                left: Box::new(l),
                right: Box::new(r),
            })
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_compiler::ir::IrGenericParamKind;
    #[cfg(feature = "std")]
    use std::vec::Vec;
    #[cfg(not(feature = "std"))]
    use alloc::vec::Vec;

    fn expr(kind: IrExprKind) -> IrExpr {
        IrExpr {
            kind,
            prov: (),
            side: None,
        }
    }

    fn const_generic(name: &str) -> IrGenericParam {
        IrGenericParam {
            name: name.into(),
            kind: IrGenericParamKind::Const,
            const_ty: None,
            bounds: Vec::new(),
            default: None,
        }
    }

    #[test]
    fn literal_is_const() {
        let e = expr(IrExprKind::Lit(IrLit::Int(42)));
        assert_eq!(eval_const_expr(&e, &[]), Some(NoirConstExpr::Literal(42)));
    }

    #[test]
    fn symbolic_const_generic_is_still_const() {
        // The one genuinely new fact vs. any existing (LIR-level) analysis:
        // a still-generic reference to `N` counts as constant, because
        // Noir's own generic system accepts it natively.
        let generics = [const_generic("N")];
        let e = expr(IrExprKind::Var("N".into()));
        assert_eq!(
            eval_const_expr(&e, &generics),
            Some(NoirConstExpr::GenericRef("N".into()))
        );
    }

    #[test]
    fn non_generic_var_is_not_const() {
        let e = expr(IrExprKind::Var("x".into()));
        assert_eq!(eval_const_expr(&e, &[]), None);
    }

    #[test]
    fn binary_over_two_consts_is_const() {
        let generics = [const_generic("N")];
        let e = expr(IrExprKind::Binary {
            op: SpecBinOp::Add,
            left: Box::new(expr(IrExprKind::Var("N".into()))),
            right: Box::new(expr(IrExprKind::Lit(IrLit::Int(1)))),
        });
        let result = eval_const_expr(&e, &generics).unwrap();
        assert_eq!(result.to_string(), "N + 1");
    }

    #[test]
    fn binary_with_non_const_side_is_not_const() {
        let e = expr(IrExprKind::Binary {
            op: SpecBinOp::Add,
            left: Box::new(expr(IrExprKind::Var("x".into()))),
            right: Box::new(expr(IrExprKind::Lit(IrLit::Int(1)))),
        });
        assert_eq!(eval_const_expr(&e, &[]), None);
    }

    #[test]
    fn length_of_const_array_length() {
        let e = expr(IrExprKind::LengthOf(ArrayLength::Const(16)));
        assert_eq!(
            eval_const_expr(&e, &[]),
            Some(NoirConstExpr::ArrayLen(Box::new(NoirConstExpr::Literal(16))))
        );
    }

    #[test]
    fn length_of_projection_is_not_const() {
        let e = expr(IrExprKind::LengthOf(ArrayLength::Projection {
            r#type: Box::new(IrType::Unit),
            field: "OutputSize".into(),
            trait_path: Some("LengthDoubler".into()),
        }));
        assert_eq!(eval_const_expr(&e, &[]), None);
    }
}
