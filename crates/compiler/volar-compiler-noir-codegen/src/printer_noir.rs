//! `IrModule` → Noir source text.
//!
//! Hand-written recursive-match text emitter, mirroring the shape of
//! `volar_compiler::printer`/`printer_ts` (there is no shared `IrVisitor`
//! trait anywhere in this codebase — every consumer hand-rolls its own
//! full match, and this is a third one, not a new abstraction).
//!
//! v1 scope (see `docs/noir-backend.md` once written, and the approved
//! plan): straight-line expressions, `if`/`else`, direct function calls,
//! and scalar/bool typed functions with no generics, no loops, no structs.
//! Later milestones extend this file; unsupported constructs return a
//! `NoirCodegenError` rather than panicking or emitting broken text.

#[cfg(feature = "std")]
use std::{format, string::String, string::ToString, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::{format, string::String, string::ToString, vec::Vec};

use volar_compiler::ir::{
    IrBlock, IrExpr, IrExprKind, IrFunction, IrLit, IrModule, IrParam, IrPattern, IrStmtKind,
    IrType, PrimitiveType, SpecBinOp, SpecUnaryOp,
};

use crate::error::NoirCodegenError;
use crate::lowering_noir::validate_module;

/// Print `module` as Noir source. Runs the pre-print validation pass
/// first (see `lowering_noir`) and returns every violation found — this
/// never panics and never emits source text for a rejected module.
pub fn print_module_noir(module: &IrModule<IrFunction>) -> Result<String, Vec<NoirCodegenError>> {
    validate_module(module)?;

    let mut out = String::new();
    let mut errors = Vec::new();
    for function in &module.functions {
        match print_function(function) {
            Ok(text) => {
                out.push_str(&text);
                out.push_str("\n\n");
            }
            Err(e) => errors.push(e),
        }
    }
    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

fn print_function(function: &IrFunction) -> Result<String, NoirCodegenError> {
    if !function.generics.is_empty() {
        return Err(NoirCodegenError::Unsupported {
            function: function.name.clone(),
            reason: "generic functions are not yet supported (see milestone 4)".into(),
        });
    }
    if function.receiver.is_some() {
        return Err(NoirCodegenError::Unsupported {
            function: function.name.clone(),
            reason: "methods (functions with a receiver) are not yet supported".into(),
        });
    }

    let mut params = Vec::new();
    for p in &function.params {
        params.push(print_param(p, &function.name)?);
    }

    // Noir's entry point requires `pub` on the return type: the verifier
    // cannot retrieve a private witness, so a `main` returning a value at
    // all must return it publicly. Only `main` is an entry point — other
    // functions' return types are ordinary (private) values.
    let ret = match &function.return_type {
        None | Some(IrType::Unit) => String::new(),
        Some(ty) => {
            let pub_prefix = if function.name == "main" { "pub " } else { "" };
            format!(" -> {pub_prefix}{}", type_to_noir(ty, &function.name)?)
        }
    };

    let body = print_block(&function.body, &function.name)?;

    Ok(format!(
        "fn {}({}){} {{\n{}\n}}",
        function.name,
        params.join(", "),
        ret,
        indent(&body),
    ))
}

fn print_param(param: &IrParam, fn_name: &str) -> Result<String, NoirCodegenError> {
    Ok(format!("{}: {}", param.name, type_to_noir(&param.ty, fn_name)?))
}

/// Map an `IrType` to Noir source text.
///
/// `Reference { mutable: false, elem }` is transparently unwrapped to
/// `elem` (following the TS-printer precedent — Noir, like TS, has no
/// general first-class reference type). `Reference { mutable: true, .. }`
/// is left unsupported in v1 rather than silently dropping its mutation
/// semantics (see the approved plan's reference-handling discussion) —
/// a `mut`-parameter policy is a deliberate later decision, not a guess.
fn type_to_noir(ty: &IrType, fn_name: &str) -> Result<String, NoirCodegenError> {
    match ty {
        IrType::Primitive(p) => primitive_to_noir(*p, fn_name),
        IrType::Unit => Ok("()".into()),
        IrType::Tuple(elems) => {
            let mut parts = Vec::new();
            for e in elems {
                parts.push(type_to_noir(e, fn_name)?);
            }
            Ok(format!("({})", parts.join(", ")))
        }
        IrType::Reference { mutable: false, elem } => type_to_noir(elem, fn_name),
        IrType::Reference { mutable: true, .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "mutable reference types (&mut T) are not yet supported".into(),
        }),
        IrType::Array { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "array types are not yet supported (see milestone 5)".into(),
        }),
        IrType::Vector { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "Vector has no compile-time length in Noir; use a fixed-size array \
                     ([T; N]) instead"
                .into(),
        }),
        IrType::Struct { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "struct types are not yet supported (see milestone 5)".into(),
        }),
        IrType::TypeParam(_) | IrType::Param { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "generic type parameters are not yet supported (see milestone 4)".into(),
        }),
        IrType::Projection { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "associated-type projections are not supported".into(),
        }),
        IrType::Existential { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "`impl Trait` types are not supported".into(),
        }),
        IrType::FnPtr { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "function-pointer types are not supported in constrained Noir".into(),
        }),
        IrType::Never => Ok("!".into()),
        IrType::Infer => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "an unresolved (`_`) type reached codegen — incomplete type inference \
                     upstream"
                .into(),
        }),
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unrecognized IrType variant: {other:?}"),
        }),
    }
}

/// Primitive-type mapping. `Usize` defaults to `u64` (Noir has no `usize`);
/// this is a named verification item in the plan — confirm the idiomatic
/// target width against Noir docs before shipping.
fn primitive_to_noir(p: PrimitiveType, fn_name: &str) -> Result<String, NoirCodegenError> {
    match p {
        PrimitiveType::Bool => Ok("bool".into()),
        PrimitiveType::U8 => Ok("u8".into()),
        PrimitiveType::U32 => Ok("u32".into()),
        PrimitiveType::U64 => Ok("u64".into()),
        PrimitiveType::Usize => Ok("u64".into()),
        PrimitiveType::U128 => Ok("u128".into()),
        PrimitiveType::I128 => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "Noir has no 128-bit signed integer type".into(),
        }),
        PrimitiveType::Bit
        | PrimitiveType::Galois
        | PrimitiveType::Galois64
        | PrimitiveType::Galois128
        | PrimitiveType::Galois256
        | PrimitiveType::BitsInBytes
        | PrimitiveType::BitsInBytes64
        | PrimitiveType::Z3 => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!(
                "{p} is a GF(2^k)/GF(3) field-element type — supported via the \
                 volar-primitives software fallback (see milestone 5), not as a \
                 bare primitive type reference"
            ),
        }),
    }
}

fn print_block(block: &IrBlock, fn_name: &str) -> Result<String, NoirCodegenError> {
    let mut lines = Vec::new();
    for stmt in &block.stmts {
        lines.push(print_stmt(&stmt.kind, fn_name)?);
    }
    if let Some(tail) = &block.expr {
        lines.push(print_expr(tail, fn_name)?);
    }
    Ok(lines.join(";\n"))
}

fn print_stmt(stmt: &IrStmtKind, fn_name: &str) -> Result<String, NoirCodegenError> {
    match stmt {
        IrStmtKind::Let { pattern, ty, init } => {
            let name = ident_pattern_name(pattern, fn_name)?;
            let ty_ann = match ty {
                Some(t) => format!(": {}", type_to_noir(t, fn_name)?),
                None => String::new(),
            };
            let init_text = match init {
                Some(e) => format!(" = {}", print_expr(e, fn_name)?),
                None => String::new(),
            };
            Ok(format!("let {name}{ty_ann}{init_text}"))
        }
        IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => print_expr(e, fn_name),
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unrecognized statement kind: {other:?}"),
        }),
    }
}

fn ident_pattern_name(pattern: &IrPattern, fn_name: &str) -> Result<String, NoirCodegenError> {
    match pattern {
        IrPattern::Ident { mutable, name, subpat: None } => {
            Ok(if *mutable { format!("mut {name}") } else { name.clone() })
        }
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unsupported let-binding pattern: {other:?}"),
        }),
    }
}

fn print_expr(expr: &IrExpr, fn_name: &str) -> Result<String, NoirCodegenError> {
    match &expr.kind {
        IrExprKind::Lit(lit) => print_lit(lit),
        IrExprKind::Var(name) => Ok(name.clone()),
        IrExprKind::Path { segments, .. } => Ok(segments.join("::")),

        IrExprKind::Binary { op, left, right } => Ok(format!(
            "({} {} {})",
            print_expr(left, fn_name)?,
            bin_op_str(*op),
            print_expr(right, fn_name)?,
        )),

        // References are transparently unwrapped in v1: `&x`/`&mut x`/`*x`
        // all print as just the inner expression's text.
        IrExprKind::Unary { op: SpecUnaryOp::Ref | SpecUnaryOp::RefMut | SpecUnaryOp::Deref, expr } => {
            print_expr(expr, fn_name)
        }
        IrExprKind::Unary { op: SpecUnaryOp::Neg, expr } => {
            Ok(format!("(-{})", print_expr(expr, fn_name)?))
        }
        IrExprKind::Unary { op: SpecUnaryOp::Not, expr } => {
            Ok(format!("(!{})", print_expr(expr, fn_name)?))
        }

        IrExprKind::Call { func, args } => {
            let func_text = print_expr(func, fn_name)?;
            let mut arg_texts = Vec::new();
            for a in args {
                arg_texts.push(print_expr(a, fn_name)?);
            }
            Ok(format!("{}({})", func_text, arg_texts.join(", ")))
        }

        IrExprKind::Cast { expr, ty } => {
            Ok(format!("({} as {})", print_expr(expr, fn_name)?, type_to_noir(ty, fn_name)?))
        }

        IrExprKind::Assign { left, right } => {
            Ok(format!("{} = {}", print_expr(left, fn_name)?, print_expr(right, fn_name)?))
        }
        IrExprKind::AssignOp { op, left, right } => Ok(format!(
            "{} {}= {}",
            print_expr(left, fn_name)?,
            bin_op_str(*op),
            print_expr(right, fn_name)?,
        )),

        IrExprKind::Block(b) => Ok(format!("{{\n{}\n}}", indent(&print_block(b, fn_name)?))),

        IrExprKind::If { cond, then_branch, else_branch } => {
            let cond_text = print_expr(cond, fn_name)?;
            let then_text = print_block(then_branch, fn_name)?;
            let else_text = match else_branch {
                None => String::new(),
                Some(e) => format!(" else {}", print_else_arm(e, fn_name)?),
            };
            Ok(format!(
                "if {} {{\n{}\n}}{}",
                cond_text,
                indent(&then_text),
                else_text,
            ))
        }

        // Noir (as of 1.0.0-beta.24) has no `match` expression and no enum
        // type at all — confirmed against the official docs (the control-
        // flow page covers if/for/while with no mention of match; there is
        // no enums page; the 1.0 pre-release announcement lists "full
        // support for primitive types... and complex data structures
        // including arrays, tuples, vectors" with no mention of enums).
        // This is a genuine language-level absence, not just an unbuilt v1
        // restriction — revisit if a future Noir version adds it.
        IrExprKind::Match { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "Noir has no `match` expression or enum type (verified against \
                     current Noir docs) — restructure as nested `if`/`else` over the \
                     discriminating condition"
                .into(),
        }),

        IrExprKind::Return(Some(e)) => Ok(format!("return {}", print_expr(e, fn_name)?)),
        IrExprKind::Return(None) => Ok("return".into()),

        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("expression kind not yet supported in v1: {other:?}"),
        }),
    }
}

/// An `else` arm is itself an arbitrary boxed expression: either another
/// `If` (an `else if` chain) or a `Block`. Both print correctly through
/// the ordinary expression printer; anything else would not be valid Noir
/// `else` syntax on its own, so it's rejected explicitly rather than
/// emitted as-is.
fn print_else_arm(expr: &IrExpr, fn_name: &str) -> Result<String, NoirCodegenError> {
    match &expr.kind {
        IrExprKind::If { .. } | IrExprKind::Block(_) => print_expr(expr, fn_name),
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unsupported else-arm shape: {other:?}"),
        }),
    }
}

fn print_lit(lit: &IrLit) -> Result<String, NoirCodegenError> {
    match lit {
        IrLit::Int(n) => Ok(n.to_string()),
        IrLit::Bool(b) => Ok(b.to_string()),
        other => Err(NoirCodegenError::Unsupported {
            function: "<literal>".into(),
            reason: format!("literal kind not yet supported in v1: {other:?}"),
        }),
    }
}

fn bin_op_str(op: SpecBinOp) -> &'static str {
    match op {
        SpecBinOp::Add => "+",
        SpecBinOp::Sub => "-",
        SpecBinOp::Mul => "*",
        SpecBinOp::Div => "/",
        SpecBinOp::Rem => "%",
        SpecBinOp::BitAnd => "&",
        SpecBinOp::BitOr => "|",
        SpecBinOp::BitXor => "^",
        SpecBinOp::Shl => "<<",
        SpecBinOp::Shr => ">>",
        SpecBinOp::Eq => "==",
        SpecBinOp::Ne => "!=",
        SpecBinOp::Lt => "<",
        SpecBinOp::Le => "<=",
        SpecBinOp::Gt => ">",
        SpecBinOp::Ge => ">=",
        SpecBinOp::And => "&&",
        SpecBinOp::Or => "||",
    }
}

fn indent(text: &str) -> String {
    text.lines()
        .map(|l| if l.is_empty() { l.to_string() } else { format!("    {l}") })
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_compiler::ir::{ExternalKind, IrGenericParam};

    fn expr(kind: IrExprKind) -> IrExpr {
        IrExpr { kind, prov: (), side: None }
    }

    fn block(stmts: Vec<IrStmtKind>, tail: Option<IrExprKind>) -> IrBlock {
        IrBlock {
            stmts: stmts
                .into_iter()
                .map(|kind| volar_compiler::ir::IrStmt { kind, prov: (), side: None })
                .collect(),
            expr: tail.map(|k| Box::new(expr(k))),
        }
    }

    fn function(name: &str, params: Vec<IrParam>, return_type: Option<IrType>, body: IrBlock) -> IrFunction {
        IrFunction {
            name: name.into(),
            module_path: Vec::new(),
            generics: Vec::new(),
            receiver: None,
            params,
            return_type,
            where_clause: Vec::new(),
            body,
            external_kind: ExternalKind::Normal,
            no_inline: false,
        }
    }

    #[test]
    fn primitive_type_mapping() {
        assert_eq!(primitive_to_noir(PrimitiveType::Bool, "f").unwrap(), "bool");
        assert_eq!(primitive_to_noir(PrimitiveType::U8, "f").unwrap(), "u8");
        assert_eq!(primitive_to_noir(PrimitiveType::U32, "f").unwrap(), "u32");
        assert_eq!(primitive_to_noir(PrimitiveType::U64, "f").unwrap(), "u64");
        assert_eq!(primitive_to_noir(PrimitiveType::Usize, "f").unwrap(), "u64");
        assert_eq!(primitive_to_noir(PrimitiveType::U128, "f").unwrap(), "u128");
        assert!(primitive_to_noir(PrimitiveType::I128, "f").is_err());
        assert!(primitive_to_noir(PrimitiveType::Galois, "f").is_err());
    }

    #[test]
    fn straight_line_function_text() {
        let body = block(
            vec![],
            Some(IrExprKind::Binary {
                op: SpecBinOp::Add,
                left: Box::new(expr(IrExprKind::Var("a".into()))),
                right: Box::new(expr(IrExprKind::Var("b".into()))),
            }),
        );
        let f = function(
            "main",
            vec![
                IrParam { name: "a".into(), ty: IrType::Primitive(PrimitiveType::U32) },
                IrParam { name: "b".into(), ty: IrType::Primitive(PrimitiveType::U32) },
            ],
            Some(IrType::Primitive(PrimitiveType::U32)),
            body,
        );
        let text = print_function(&f).unwrap();
        // `main`'s return type must be `pub` — Noir's entry-point requirement.
        assert!(text.contains("-> pub u32"), "{text}");
        assert!(text.contains("(a + b)"), "{text}");
    }

    #[test]
    fn non_main_function_return_type_is_not_pub() {
        let body = block(vec![], Some(IrExprKind::Var("a".into())));
        let f = function(
            "helper",
            vec![IrParam { name: "a".into(), ty: IrType::Primitive(PrimitiveType::U32) }],
            Some(IrType::Primitive(PrimitiveType::U32)),
            body,
        );
        let text = print_function(&f).unwrap();
        assert!(text.contains("-> u32") && !text.contains("-> pub u32"), "{text}");
    }

    #[test]
    fn if_else_prints_as_value_yielding_expression() {
        let if_expr = expr(IrExprKind::If {
            cond: Box::new(expr(IrExprKind::Binary {
                op: SpecBinOp::Gt,
                left: Box::new(expr(IrExprKind::Var("a".into()))),
                right: Box::new(expr(IrExprKind::Var("b".into()))),
            })),
            then_branch: block(vec![], Some(IrExprKind::Var("a".into()))),
            else_branch: Some(Box::new(expr(IrExprKind::Block(block(
                vec![],
                Some(IrExprKind::Var("b".into())),
            ))))),
        });
        let text = print_expr(&if_expr, "f").unwrap();
        assert!(text.starts_with("if (a > b) {"), "{text}");
        assert!(text.contains("} else {"), "{text}");
    }

    #[test]
    fn match_is_rejected_with_a_clear_reason() {
        let m = expr(IrExprKind::Match {
            expr: Box::new(expr(IrExprKind::Var("a".into()))),
            arms: Vec::new(),
        });
        let err = print_expr(&m, "f").unwrap_err();
        match err {
            NoirCodegenError::Unsupported { reason, .. } => {
                assert!(reason.contains("match"), "{reason}");
            }
            other => panic!("expected Unsupported, got {other:?}"),
        }
    }

    #[test]
    fn generic_function_is_rejected_in_v1() {
        let f = function(
            "generic_fn",
            vec![],
            None,
            block(vec![], None),
        );
        let mut f = f;
        f.generics = vec![IrGenericParam {
            name: "T".into(),
            kind: volar_compiler::ir::IrGenericParamKind::Type,
            const_ty: None,
            bounds: Vec::new(),
            default: None,
        }];
        assert!(print_function(&f).is_err());
    }

    #[test]
    fn immutable_reference_type_is_transparently_unwrapped() {
        let ty = IrType::Reference { mutable: false, elem: Box::new(IrType::Primitive(PrimitiveType::U32)) };
        assert_eq!(type_to_noir(&ty, "f").unwrap(), "u32");
    }

    #[test]
    fn mutable_reference_type_is_unsupported_in_v1() {
        let ty = IrType::Reference { mutable: true, elem: Box::new(IrType::Primitive(PrimitiveType::U32)) };
        assert!(type_to_noir(&ty, "f").is_err());
    }
}
