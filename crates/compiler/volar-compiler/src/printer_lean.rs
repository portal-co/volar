//! Native Lean 4 printer for the spec IR.
//!
//! Unlike the TS/dyn printers, this emits *idiomatic* Lean: plain functions,
//! `structure`/`inductive`, `Vector` for fixed arrays, and explicit hash
//! parameters in place of the `Digest` trait idiom. The output is the
//! "cleaner representation" that voof proves equivalent to the Aeneas
//! extraction of the same Rust source (see voof docs §3b).
//!
//! v1 scope: the garbled-circuit spec subset. Unsupported constructs raise
//! `CompilerError::Unsupported` naming the item, so coverage grows
//! incrementally and loudly rather than silently emitting wrong code.

use crate::ir::*;
use core::fmt::Write;

#[cfg(feature = "std")]
use std::boxed::Box;
#[cfg(feature = "std")]
use std::collections::BTreeSet;
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::collections::BTreeSet;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// Infallible writes to the output String.
macro_rules! w {
    ($out:expr, $($t:tt)*) => { write!($out, $($t)*).unwrap() };
}
macro_rules! wln {
    ($out:expr, $($t:tt)*) => { writeln!($out, $($t)*).unwrap() };
}

/// Lean keywords we must not emit as identifiers.
const LEAN_RESERVED: &[&str] = &[
    "def", "theorem", "where", "match", "with", "if", "then", "else", "let", "do", "in", "fun",
    "forall", "open", "end", "namespace", "section", "structure", "inductive", "class", "instance",
    "import", "axiom", "example", "variable", "universe", "return", "by", "have", "show", "from",
    "infix", "infixl", "infixr", "notation", "macro", "syntax", "opaque", "partial", "mutual",
    "private", "protected", "public", "noncomputable", "meta", "except",
];

pub(crate) fn ident(name: &str) -> String {
    let mut s = name.replace('.', "_").replace('#', "_");
    if LEAN_RESERVED.contains(&s.as_str()) {
        s.push('_');
    }
    s
}

/// Type params bound by a typenum-style length trait become `Nat` length
/// variables in native Lean.
fn is_len_bound(k: &TraitKind) -> bool {
    // the parser sometimes routes known traits through Custom; accept both
    matches!(k, TraitKind::ArraySize | TraitKind::VoleArray)
        || matches!(k.to_string().as_str(), "ArraySize" | "VoleArray")
}

/// Rendering context: typenum length type-params → Nat variable names, and
/// `N: Div<Uk, Output = O>` projections → `o := n / k`.
#[derive(Default, Clone)]
struct LeanCx {
    /// Type-param name → Nat variable name (e.g. "N" → "n").
    len_params: Vec<(String, String)>,
    /// Exact-arithmetic mode (mxx-derived modules): map `u64`/`u128` to
    /// `Nat` so all arithmetic is exact and casts between machine int types
    /// are identities. Only sound where the source never wraps; the mxx
    /// lowerer enforces tower moduli < 2^63, so every u128 intermediate is
    /// < 2^126 and Rust's wrapping semantics coincide with Nat.
    exact_nat: bool,
    /// Type param O → (base len var, divisor), from `N: Div<Uk, Output=O>`.
    div_projections: Vec<(String, String, usize)>,
    /// A Digest-bound type param is present: functions take a `hash` param.
    digest: bool,
    /// Name of the Digest-bound type param (for `D::new()` detection).
    digest_ty: Option<String>,
    /// Variables that are `Fin n` array indices: printed as `v.val`.
    fin_vars: Vec<String>,
    /// Digest-using method/function names (for threading `hash` at call
    /// sites). v1: name-based, since the IR does not resolve receiver types.
    digest_methods: Vec<String>,
    /// Current indentation level (2 spaces per), for do-block layout.
    cur_indent: usize,
    /// Variables known to hold Bool (for Rust `^`/`&` → Lean `!=`/`&&`).
    bool_vars: Vec<String>,
    /// Length-param names that may appear in expression position (repeat
    /// lengths, loop bounds) and print as their Nat variable. Const
    /// generics always qualify (they could not print at all before);
    /// typenum-style params qualify only in exact mode, preserving the
    /// garble subset's output byte for byte.
    expr_len_vars: Vec<String>,
    /// Function name → its implicit Nat length parameters (Lean names), for
    /// exact-mode call sites: over `List` (length-erased) types, implicit
    /// length params are not synthesizable, so call sites pass them by
    /// name (`(n := n)`).
    fn_len_params: Vec<(String, Vec<String>)>,
}

impl LeanCx {
    /// Map a type param to its Nat length expression, if it is one.
    fn len_expr(&self, name: &str) -> Option<String> {
        // projections (O := n / 8) win over plain len params
        if let Some((_, base, k)) = self.div_projections.iter().find(|(p, _, _)| p == name) {
            return Some(format!("({base} / {k})"));
        }
        self.len_params
            .iter()
            .find(|(p, _)| p == name)
            .map(|(_, v)| v.clone())
    }
}

fn cx_for_ex(
    generics: &[IrGenericParam],
    where_clause: &[IrWherePredicate],
    exact_nat: bool,
) -> LeanCx {
    let mut cx = LeanCx { exact_nat, ..Default::default() };
    for p in generics {
        if p.bounds.iter().any(|b| is_len_bound(&b.trait_kind)) {
            cx.len_params.push((p.name.clone(), p.name.to_lowercase()));
        }
        // `const N: usize` parameters are Nat length variables in native
        // Lean, same as typenum-style length params — and they may appear
        // in expression position (`[0; N]`, `for i in 0..N`).
        if p.kind == IrGenericParamKind::Const
            && matches!(&p.const_ty, None | Some(IrType::Primitive(PrimitiveType::Usize)))
        {
            cx.len_params.push((p.name.clone(), p.name.to_lowercase()));
            cx.expr_len_vars.push(p.name.clone());
        }
        if p.bounds.iter().any(|b| b.trait_kind == TraitKind::Digest || b.trait_kind.to_string() == "Digest") {
            cx.digest = true;
            cx.digest_ty = Some(p.name.clone());
        }
    }
    if exact_nat {
        // exact mode: every length param may appear in expression position
        for (p, _) in cx.len_params.clone() {
            if !cx.expr_len_vars.contains(&p) {
                cx.expr_len_vars.push(p);
            }
        }
    }
    for pred in where_clause {
        let IrWherePredicate::TypeBound { ty, bounds } = pred;
        if let IrType::TypeParam(t) = ty {
            let Some(nv) = cx
                .len_params
                .iter()
                .find(|(p, _)| p == t)
                .map(|(_, v)| v.clone())
            else {
                continue;
            };
            for b in bounds {
                if b.trait_kind == TraitKind::Math(MathTrait::Div) || b.trait_kind.to_string() == "Div" {
                    let k = b
                        .type_args
                        .first()
                        .and_then(|a| match a {
                            IrType::Struct { kind, .. } => {
                                TypeNumConst::from_str(&kind.to_string()).map(|t| t.to_usize())
                            }
                            _ => None,
                        })
                        .unwrap_or(8);
                    for (assoc, aty) in &b.assoc_bindings {
                        if *assoc == AssociatedType::Output {
                            if let IrType::TypeParam(o) = aty {
                                cx.div_projections.push((o.clone(), nv.clone(), k));
                            }
                        }
                    }
                }
            }
        }
    }
    cx
}

fn len_lean(len: &ArrayLength, cx: &LeanCx) -> Result<String> {
    match len {
        ArrayLength::Const(n) => Ok(format!("{n}")),
        ArrayLength::TypeNum(t) => Ok(format!("{}", t.to_usize())),
        ArrayLength::TypeParam(name) => cx
            .len_expr(name)
            .ok_or_else(|| CompilerError::Unsupported(format!("length param {name}"))),
        ArrayLength::Projection { .. } => Err(CompilerError::Unsupported(
            "array length projection".to_string(),
        )),
    }
}

fn ty_lean(ty: &IrType, cx: &LeanCx) -> Result<String> {
    match ty {
        IrType::Primitive(p) => Ok(match p {
            PrimitiveType::Bool => "Bool".into(),
            PrimitiveType::U8 => "UInt8".into(),
            PrimitiveType::U32 => "UInt32".into(),
            PrimitiveType::U64 => {
                if cx.exact_nat {
                    "Nat".into() // exact mode: u64 is unbounded-exact
                } else {
                    "UInt64".into()
                }
            }
            PrimitiveType::Usize => "Nat".into(),
            PrimitiveType::I64 => "Int".into(),
            PrimitiveType::I128 => "Int".into(),
            PrimitiveType::U128 => "Nat".into(), // no UInt128 in Lean core
            other => return Err(CompilerError::Unsupported(format!("primitive {other:?}"))),
        }),
        // v1: fixed arrays become List (length documented at the type
        // boundary; Vector refinement is planned). The `len` is validated
        // to be known, so wrong lengths fail generation rather than being
        // silently dropped.
        IrType::Array { elem, len, .. } => {
            let _ = len_lean(len, cx)?;
            let e = ty_lean(elem, cx)?;
            Ok(if e.contains(' ') {
                format!("List ({e})")
            } else {
                format!("List {e}")
            })
        }
        IrType::Vector { elem } => {
            let e = ty_lean(elem, cx)?;
            Ok(if e.contains(' ') {
                format!("List ({e})")
            } else {
                format!("List {e}")
            })
        }
        IrType::Struct { kind, type_args } => {
            let name = ident(&kind.to_string());
            if type_args.is_empty() {
                Ok(name)
            } else {
                let args: Vec<String> = type_args
                    .iter()
                    .map(|a| match a {
                        IrType::TypeParam(p) => Ok(cx.len_expr(p).unwrap_or_else(|| ident(p))),
                        _ => ty_lean(a, cx),
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("({name} {})", args.join(" ")))
            }
        }
        IrType::TypeParam(name) => {
            if cx.digest_ty.as_deref() == Some(name) {
                return Ok("DigestSt".into());
            }
            Ok(cx.len_expr(name).unwrap_or_else(|| ident(name)))
        }
        IrType::Tuple(ts) => match ts.len() {
            0 => Ok("Unit".into()),
            2 => Ok(format!("({} × {})", ty_lean(&ts[0], cx)?, ty_lean(&ts[1], cx)?)),
            n => Err(CompilerError::Unsupported(format!("tuple arity {n}"))),
        },
        IrType::Unit => Ok("Unit".into()),
        IrType::Projection { base, trait_path, assoc, .. } => {
            let is_div = trait_path.as_deref() == Some("Div");
            if is_div && *assoc == AssociatedType::Output {
                if let IrType::TypeParam(base_name) = &**base {
                    if let Some(e) = cx.len_expr(base_name) {
                        // Div<Uk> — divisor from trait_args
                        return Ok(format!("({e} / 8)"));
                    }
                }
            }
            Err(CompilerError::Unsupported(format!(
                "projection {:?}",
                trait_path
            )))
        }
        IrType::Reference { elem, .. } => ty_lean(elem, cx),
        other => Err(CompilerError::Unsupported(format!(
            "type {:?}",
            std::mem::discriminant(other)
        ))),
    }
}

fn lit_lean(l: &IrLit) -> String {
    match l {
        IrLit::Int(i) => format!("({i})"),
        IrLit::Bool(b) => format!("{b}"),
        IrLit::Byte(b) => format!("({b} : UInt8)"),
        IrLit::Unit => "()".into(),
        other => format!("/* TODO lit {other:?} */ default"),
    }
}

fn binop_lean(op: &SpecBinOp) -> &'static str {
    match op {
        SpecBinOp::Add => "+",
        SpecBinOp::Sub => "-",
        SpecBinOp::Mul => "*",
        SpecBinOp::Div => "/",
        SpecBinOp::Rem => "%",
        SpecBinOp::BitAnd => "&&&",
        SpecBinOp::BitOr => "|||",
        SpecBinOp::BitXor => "^^^",
        SpecBinOp::Shl => "<<<",
        SpecBinOp::Shr => ">>>",
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

fn expr_lean(e: &IrExpr, cx: &LeanCx, out: &mut String) -> Result<()> {
    match &e.kind {
        IrExprKind::Lit(l) => w!(out, "{}", lit_lean(l)),
        IrExprKind::Var(v) => {
            if cx.fin_vars.contains(v) {
                w!(out, "{}.val", ident(v))
            } else if cx.expr_len_vars.contains(v) {
                if let Some(le) = cx.len_expr(v) {
                    // a const-generic length name in expression position
                    // (repeat lengths, loop bounds) is the Nat variable
                    w!(out, "{le}")
                } else {
                    w!(out, "{}", ident(v))
                }
            } else {
                w!(out, "{}", ident(v))
            }
        }
        IrExprKind::Path { segments, .. } => {
            // Type::item paths keep Lean dots (namespaces); lone segments are idents
            if segments.len() > 1 {
                w!(out, "{}", segments.iter().map(|s| ident(s)).collect::<Vec<_>>().join("."))
            } else {
                w!(out, "{}", ident(&segments[0]))
            }
        }
        IrExprKind::Binary { op, left, right } => {
            // Rust `^`/`&`/`|` on bools are Bool relations in Lean
            fn bool_op(cx: &LeanCx, e: &IrExpr) -> bool {
                match &e.kind {
                    IrExprKind::Var(v) => cx.bool_vars.contains(v),
                    IrExprKind::Lit(IrLit::Bool(_)) => true,
                    IrExprKind::Binary { op, left, right } => match op {
                        SpecBinOp::Eq | SpecBinOp::Ne | SpecBinOp::Lt | SpecBinOp::Le
                        | SpecBinOp::Gt | SpecBinOp::Ge | SpecBinOp::And | SpecBinOp::Or => true,
                        SpecBinOp::BitXor | SpecBinOp::BitAnd | SpecBinOp::BitOr => {
                            bool_op(cx, left) && bool_op(cx, right)
                        }
                        _ => false,
                    },
                    _ => false,
                }
            }
            if matches!(op, SpecBinOp::BitXor | SpecBinOp::BitAnd | SpecBinOp::BitOr)
                && bool_op(cx, left)
                && bool_op(cx, right)
            {
                w!(out, "(");
                expr_lean(left, cx, out)?;
                {
                    let bop = match op {
                        SpecBinOp::BitXor => "!=",
                        SpecBinOp::BitAnd => "&&",
                        _ => "||",
                    };
                    w!(out, " {bop} ");
                }
                expr_lean(right, cx, out)?;
                w!(out, ")");
                return Ok(());
            }
            w!(out, "(");
            expr_lean(left, cx, out)?;
            w!(out, " {} ", binop_lean(op));
            // Lean's UInt8 shifts take a UInt8 amount; the spec's shifts are
            // on UInt8 unless the left side was explicitly cast to Nat.
            let nat_left = matches!(&left.kind, IrExprKind::Cast { ty, .. }
                if matches!(&**ty, IrType::Primitive(PrimitiveType::Usize | PrimitiveType::U64)));
            // bitwise op on an indexed/field value: UInt8 operands (the
            // spec's bytes); ascribe small Nat literals to UInt8
            let bitwise = matches!(op, SpecBinOp::BitAnd | SpecBinOp::BitOr | SpecBinOp::BitXor);
            let left_is_bytes = matches!(&left.kind,
                IrExprKind::Index { .. } | IrExprKind::Field { .. });
            if bitwise && left_is_bytes && matches!(&right.kind, IrExprKind::Lit(IrLit::Int(_))) {
                w!(out, "(");
                expr_lean(right, cx, out)?;
                w!(out, " : UInt8)");
            } else if matches!(op, SpecBinOp::Shl | SpecBinOp::Shr) && !nat_left {
                w!(out, "(");
                expr_lean(right, cx, out)?;
                w!(out, ").toUInt8");
            } else {
                expr_lean(right, cx, out)?;
            }
            w!(out, ")")
        }
        IrExprKind::Unary { op, expr } => match op {
            SpecUnaryOp::Not => {
                w!(out, "(!");
                expr_lean(expr, cx, out)?;
                w!(out, ")")
            }
            SpecUnaryOp::Neg => {
                w!(out, "(-");
                expr_lean(expr, cx, out)?;
                w!(out, ")")
            }
            _ => expr_lean(expr, cx, out)?, // Ref/RefMut/Deref erased
        },
        IrExprKind::Field { base, field } => {
            expr_lean(base, cx, out)?;
            w!(out, ".{}", ident(field))
        }
        IrExprKind::Index { base, index } => {
            // range index = list slicing
            if let IrExprKind::Range { start, end, inclusive } = &index.kind {
                match (start, end) {
                    (Some(st), None) => {
                        let mut base_s = String::new();
                        expr_lean(base, cx, &mut base_s)?;
                        w!(out, "(List.drop ");
                        expr_lean(st, cx, out)?;
                        w!(out, " {base_s})");
                        return Ok(());
                    }
                    (None, Some(en)) => {
                        w!(out, ".take (");
                        expr_lean(en, cx, out)?;
                        if *inclusive {
                            w!(out, " + 1");
                        }
                        w!(out, ")");
                    }
                    (Some(st), Some(en)) => {
                        w!(out, " |> List.take (");
                        expr_lean(en, cx, out)?;
                        if *inclusive {
                            w!(out, " + 1");
                        }
                        w!(out, ") |> List.drop ");
                        expr_lean(st, cx, out)?;
                    }
                    (None, None) => {}
                }
                return Ok(());
            }
            // Lean's `]![` does not chain off an `Index` base: parenthesize
            // it. (Bare variables and field projections chain fine.)
            if matches!(&base.kind, IrExprKind::Index { .. }) {
                w!(out, "(");
                expr_lean(base, cx, out)?;
                w!(out, ")");
            } else {
                expr_lean(base, cx, out)?;
            }
            w!(out, "[");
            expr_lean(index, cx, out)?;
            w!(out, "]!")
        }
        IrExprKind::Cast { expr, ty } => {
            // exact-arithmetic mode: machine-int targets are all Nat/Int,
            // so casts are identities.
            if cx.exact_nat
                && matches!(
                    &**ty,
                    IrType::Primitive(
                        PrimitiveType::U64
                            | PrimitiveType::U128
                            | PrimitiveType::Usize
                            | PrimitiveType::I64
                            | PrimitiveType::I128
                    )
                )
            {
                w!(out, "(");
                expr_lean(expr, cx, out)?;
                return Ok(w!(out, ")"));
            }
            // v1: `as usize` from UInt8-ish → .toNat; `as u64` from
            // usize/Nat → UInt64.ofNat. Extend as the subset grows.
            match &**ty {
                IrType::Primitive(PrimitiveType::Usize) => {
                    w!(out, "(");
                    expr_lean(expr, cx, out)?;
                    w!(out, ").toNat")
                }
                IrType::Primitive(PrimitiveType::U64) => {
                    // sources of `as u64` in the subset are usize → Nat
                    w!(out, "(UInt64.ofNat ");
                    expr_lean(expr, cx, out)?;
                    w!(out, ")")
                }
                _ => {
                    let t = ty_lean(ty, cx)?;
                    w!(out, "(");
                    expr_lean(expr, cx, out)?;
                    w!(out, " : {t})")
                }
            }
        }
        IrExprKind::Call { func, args } => {
            w!(out, "(");
            expr_lean(func, cx, out)?;
            // exact mode: helper length params are implicit `{n : Nat}` but
            // not synthesizable from length-erased List arguments, so call
            // sites pass them by name. Turbofish const args (TypeParam
            // numerals / length names) map positionally onto the callee's
            // length params; calls without turbofish forward the caller's
            // same-named length variables.
            if cx.exact_nat {
                // Bare calls parse as `Var`, qualified calls as `Path`.
                let (callee, type_args): (String, &[IrType]) = match &func.kind {
                    IrExprKind::Var(v) => (v.clone(), &[]),
                    IrExprKind::Path { segments, type_args } => {
                        (segments.last().cloned().unwrap_or_default(), type_args)
                    }
                    _ => (String::new(), &[]),
                };
                {
                    let type_args = type_args;
                    if let Some(params) = cx
                        .fn_len_params
                        .iter()
                        .find(|(n, _)| *n == callee)
                        .map(|(_, p)| p.clone())
                        .filter(|p| !p.is_empty())
                    {
                        if type_args.is_empty() {
                            for p in &params {
                                w!(out, " ({p} := {p})");
                            }
                        } else {
                            if type_args.len() != params.len() {
                                return Err(CompilerError::Unsupported(format!(
                                    "call to {callee}: {} const args for {} length params",
                                    type_args.len(),
                                    params.len()
                                )));
                            }
                            for (p, a) in params.iter().zip(type_args.iter()) {
                                match a {
                                    IrType::TypeParam(s)
                                        if s.chars().all(|c| c.is_ascii_digit()) =>
                                    {
                                        w!(out, " ({p} := {s})")
                                    }
                                    IrType::TypeParam(s) => {
                                        let le = cx.len_expr(s).ok_or_else(|| {
                                            CompilerError::Unsupported(format!(
                                                "call to {callee}: const arg {s} is not a length param"
                                            ))
                                        })?;
                                        w!(out, " ({p} := {le})");
                                    }
                                    other => {
                                        return Err(CompilerError::Unsupported(format!(
                                            "call to {callee}: non-length const arg {:?}",
                                            std::mem::discriminant(other)
                                        )))
                                    }
                                }
                            }
                        }
                    }
                }
            }
            for a in args {
                w!(out, " ");
                expr_lean(a, cx, out)?;
            }
            w!(out, ")")
        }
        IrExprKind::Block(b) => {
            if b.stmts.is_empty() && b.expr.is_some() {
                expr_lean(b.expr.as_ref().unwrap(), cx, out)?;
            } else {
                wln!(out, "(Id.run do");
                let mut cx2 = cx.clone();
                cx2.cur_indent += 1;
                block_lean_id(b, &cx2, out)?;
                let pad = "  ".repeat(cx.cur_indent);
                w!(out, "{pad})");
            }
        }
        IrExprKind::If { cond, then_branch, else_branch } => {
            w!(out, "(if ");
            expr_lean(cond, cx, out)?;
            w!(out, " then ");
            if then_branch.stmts.is_empty() {
                if let Some(e) = &then_branch.expr {
                    expr_lean(e, cx, out)?;
                }
            } else {
                return Err(CompilerError::Unsupported("if with statement body".into()));
            }
            match else_branch {
                Some(e) => {
                    w!(out, " else ");
                    expr_lean(e, cx, out)?;
                }
                None => w!(out, " else ()"),
            }
            w!(out, ")")
        }
        IrExprKind::ArrayGenerate { len, index_var, body, .. } => {
            let n = len_lean(len, cx)?;
            w!(out, "(List.ofFn (fun ({} : Fin {}) => ", ident(index_var), n);
            let mut cx2 = cx.clone();
            cx2.fin_vars.push(index_var.clone());
            expr_lean(body, &cx2, out)?;
            w!(out, "))")
        }
        IrExprKind::FixedArray(items) | IrExprKind::Array(items) => {
            // Ascribe only leaf-literal lists (and empty lists); outer
            // nesting levels are inferred from the elements.
            let leaf = items.is_empty()
                || items.iter().all(|it| matches!(it.kind, IrExprKind::Lit(_)));
            w!(out, "([");
            for (i, it) in items.iter().enumerate() {
                if i > 0 {
                    w!(out, ", ");
                }
                expr_lean(it, cx, out)?;
            }
            if leaf {
                w!(out, "] : List {})", if cx.exact_nat { "Nat" } else { "UInt8" })
            } else {
                w!(out, "])")
            }
        }
        IrExprKind::StructExpr { fields, rest, .. } => {
            if rest.is_some() {
                return Err(CompilerError::Unsupported("struct update syntax".into()));
            }
            w!(out, "{{ ");
            for (i, (fname, fexpr)) in fields.iter().enumerate() {
                if i > 0 {
                    w!(out, ", ");
                }
                w!(out, "{} := ", ident(fname));
                expr_lean(fexpr, cx, out)?;
            }
            w!(out, " }}")
        }
        IrExprKind::Match { expr, arms } => {
            // split_first idiom: match on the list directly with cons patterns
            let split_first = matches!(&expr.kind, IrExprKind::MethodCall { method, .. }
                if matches!(method, MethodKind::Other(m) if m == "split_first"));
            if split_first {
                let IrExprKind::MethodCall { receiver, .. } = &expr.kind else {
                    unreachable!()
                };
                wln!(out, "(match ");
                expr_lean(receiver, cx, out)?;
                wln!(out, " with");
                for arm in arms {
                    w!(out, "  | ");
                    // None → [], Some((a, rest)) → a :: rest
                    match &arm.pattern {
                        IrPattern::TupleStruct { kind, elems } if kind.to_string().contains("Some") => {
                            if let Some(IrPattern::Tuple(items)) = elems.first() {
                                pattern_lean(&items[0], out);
                                w!(out, " :: ");
                                pattern_lean(&items[1], out);
                            }
                        }
                        _ => w!(out, "[]"),
                    }
                    wln!(out, " =>");
                    let mut cx2 = cx.clone();
                    cx2.cur_indent += 1;
                    // arm bodies are exprs; print inline
                    w!(out, "    ");
                    expr_lean(&arm.body, &cx2, out)?;
                    wln!(out, "");
                }
                w!(out, ")");
                return Ok(());
            }
            wln!(out, "(match ");
            expr_lean(expr, cx, out)?;
            wln!(out, " with");
            for arm in arms {
                w!(out, "  | ");
                pattern_lean(&arm.pattern, out);
                wln!(out, " =>");
                w!(out, "    ");
                expr_lean(&arm.body, cx, out)?;
                wln!(out, "");
            }
            w!(out, ")");
            return Ok(());
        }
        IrExprKind::IterPipeline(chain)
            if chain.steps.is_empty()
                && matches!(&chain.source, IterChainSource::Range { .. })
                && matches!(&chain.terminal, IterTerminal::Fold { .. }) =>
        {
            // `(start..end).fold(init, |acc, i| body)`
            //   → `(List.range' start (end - start)).foldl (fun acc i => body) init`
            let IterChainSource::Range { start, end, inclusive } = &chain.source else {
                unreachable!()
            };
            let IterTerminal::Fold { init, acc_var, elem_var, body } = &chain.terminal else {
                unreachable!()
            };
            w!(out, "((List.range' (");
            expr_lean(start, cx, out)?;
            w!(out, ") ((");
            expr_lean(end, cx, out)?;
            w!(out, ") - (");
            expr_lean(start, cx, out)?;
            if *inclusive {
                w!(out, ") + 1");
            } else {
                w!(out, ")");
            }
            w!(out, ")).foldl (fun ");
            pattern_lean(acc_var, out);
            w!(out, " ");
            pattern_lean(elem_var, out);
            w!(out, " => ");
            expr_lean(body, cx, out)?;
            w!(out, ") (");
            expr_lean(init, cx, out)?;
            w!(out, "))")
        }
        IrExprKind::Repeat { elem, len } => {
            w!(out, "(List.replicate (");
            expr_lean(len, cx, out)?;
            w!(out, ") (");
            expr_lean(elem, cx, out)?;
            w!(out, "))")
        }
        IrExprKind::DefaultValue { ty } => {
            if let Some(box_ir) = ty.as_ref() {
                if let IrType::Array { elem, len, .. } = &**box_ir {
                    let n = len_lean(len, cx)?;
                    let mut e = String::new();
                    expr_lean(
                        &IrExpr { kind: IrExprKind::DefaultValue { ty: Some(elem.clone()) }, prov: (), side: None },
                        cx,
                        &mut e,
                    )?;
                    w!(out, "(List.replicate {n} {e})");
                    return Ok(());
                }
            }
            let t = ty.as_ref().map(|t| ty_lean(t, cx)).transpose()?;
            match t.as_deref() {
                Some("UInt8") => w!(out, "(0 : UInt8)"),
                Some(t) => w!(out, "(default : {t})"),
                None => w!(out, "default"),
            }
        }
        IrExprKind::MethodCall { receiver, method, args, .. } => {
            match method {
                MethodKind::Known(StdMethod::Clone) => expr_lean(receiver, cx, out)?,
                MethodKind::Known(StdMethod::IsEmpty) => {
                    w!(out, "(");
                    expr_lean(receiver, cx, out)?;
                    w!(out, ").isEmpty")
                }
                MethodKind::Known(StdMethod::ToLeBytes) => {
                    w!(out, "(u64LeBytes ");
                    expr_lean(receiver, cx, out)?;
                    w!(out, ")")
                }
                MethodKind::Known(StdMethod::Update) => {
                    if !cx.digest {
                        return Err(CompilerError::Unsupported(format!("update outside digest: {:?}", receiver.kind)));
                    }
                    expr_lean(receiver, cx, out)?;
                    w!(out, " := DigestSt.update ");
                    expr_lean(receiver, cx, out)?;
                    w!(out, " (");
                    expr_lean(&args[0], cx, out)?;
                    w!(out, ")")
                }
                MethodKind::Known(StdMethod::Finalize) => {
                    if !cx.digest {
                        return Err(CompilerError::Unsupported("finalize outside digest".into()));
                    }
                    w!(out, "(DigestSt.finalize ");
                    expr_lean(receiver, cx, out)?;
                    w!(out, " hash)")
                }
                MethodKind::Known(m) => {
                    return Err(CompilerError::Unsupported(format!("std method {m:?}")))
                }
                MethodKind::Vole(v) => {
                    return Err(CompilerError::Unsupported(format!("vole method {v:?}")))
                }
                MethodKind::Other(name) => {
                    // spec method call: Lean dot notation; digest-using
                    // callees take `hash` as their last argument.
                    expr_lean(receiver, cx, out)?;
                    w!(out, ".{}", ident(name));
                    for a in args {
                        w!(out, " (");
                        expr_lean(a, cx, out)?;
                        w!(out, ")");
                    }
                    if cx.digest_methods.contains(name) {
                        w!(out, " hash");
                    }
                }
            }
        }
        other => {
            return Err(CompilerError::Unsupported(format!(
                "expr {:?}",
                std::mem::discriminant(other)
            )))
        }
    }
    Ok(())
}

fn block_lean_id(b: &IrBlock, cx: &LeanCx, out: &mut String) -> Result<()> {
    block_lean_mode(b, cx, out, true)
}

/// Does any AssignOp with a bitwise op target `var` anywhere in the block?
fn block_has_bitop_assign(b: &IrBlock, var: &str) -> bool {
    fn expr_has(e: &IrExpr, var: &str) -> bool {
        match &e.kind {
            IrExprKind::AssignOp { op, left, .. } => {
                matches!(op, SpecBinOp::BitOr | SpecBinOp::BitXor | SpecBinOp::BitAnd)
                    && matches!(&left.kind, IrExprKind::Var(v) if v == var)
            }
            IrExprKind::Block(b) => block_has_bitop_assign(b, var),
            IrExprKind::If { cond, then_branch, else_branch } => {
                expr_has(cond, var)
                    || block_has_bitop_assign(then_branch, var)
                    || else_branch.as_ref().is_some_and(|e| expr_has(e, var))
            }
            IrExprKind::BoundedLoop { body, .. } => block_has_bitop_assign(body, var),
            IrExprKind::Call { args, .. } | IrExprKind::MethodCall { args, .. } => {
                args.iter().any(|a| expr_has(a, var))
            }
            IrExprKind::Binary { left, right, .. } => {
                expr_has(left, var) || expr_has(right, var)
            }
            IrExprKind::ArrayGenerate { body, .. } => expr_has(body, var),
            IrExprKind::StructExpr { fields, .. } => {
                fields.iter().any(|(_, fe)| expr_has(fe, var))
            }
            IrExprKind::Field { base, .. } | IrExprKind::Index { base, .. } => {
                expr_has(base, var)
            }
            IrExprKind::Unary { expr, .. } | IrExprKind::Cast { expr, .. } => {
                expr_has(expr, var)
            }
            IrExprKind::Tuple(items) | IrExprKind::Array(items)
            | IrExprKind::FixedArray(items) => items.iter().any(|i| expr_has(i, var)),
            _ => false,
        }
    }
    b.stmts.iter().any(|st| match &st.kind {
        IrStmtKind::Let { init, .. } => init.as_ref().is_some_and(|e| expr_has(e, var)),
        IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => expr_has(e, var),
        _ => false,
    }) || b.expr.as_ref().is_some_and(|e| expr_has(e, var))
}

/// Collect variables assigned anywhere in a block (for `let mut` rebinding).
fn collect_assigned_block(b: &IrBlock, out: &mut BTreeSet<String>) {
    for st in &b.stmts {
        match &st.kind {
            IrStmtKind::Let { init, .. } => {
                if let Some(e) = init {
                    collect_assigned_expr(e, out);
                }
            }
            IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => collect_assigned_expr(e, out),
            _ => {}
        }
    }
    if let Some(e) = &b.expr {
        collect_assigned_expr(e, out);
    }
}

fn collect_assigned_expr(e: &IrExpr, out: &mut BTreeSet<String>) {
    match &e.kind {
        IrExprKind::Assign { left, right } => {
            match &left.kind {
                IrExprKind::Var(v) => {
                    out.insert(v.clone());
                }
                IrExprKind::Index { base, .. } => {
                    if let IrExprKind::Var(v) = &base.kind {
                        out.insert(v.clone());
                    }
                }
                _ => {}
            }
            collect_assigned_expr(right, out);
        }
        IrExprKind::AssignOp { left, right, .. } => {
            match &left.kind {
                IrExprKind::Var(v) => {
                    out.insert(v.clone());
                }
                IrExprKind::Index { base, .. } => {
                    if let IrExprKind::Var(v) = &base.kind {
                        out.insert(v.clone());
                    }
                }
                _ => {}
            }
            collect_assigned_expr(right, out);
        }
        IrExprKind::Binary { left, right, .. } => {
            collect_assigned_expr(left, out);
            collect_assigned_expr(right, out);
        }
        IrExprKind::Unary { expr, .. } => collect_assigned_expr(expr, out),
        IrExprKind::Call { func, args } => {
            collect_assigned_expr(func, out);
            for a in args {
                collect_assigned_expr(a, out);
            }
        }
        IrExprKind::MethodCall { receiver, args, .. } => {
            collect_assigned_expr(receiver, out);
            for a in args {
                collect_assigned_expr(a, out);
            }
        }
        IrExprKind::Block(b) => collect_assigned_block(b, out),
        IrExprKind::If { cond, then_branch, else_branch } => {
            collect_assigned_expr(cond, out);
            collect_assigned_block(then_branch, out);
            if let Some(e) = else_branch {
                collect_assigned_expr(e, out);
            }
        }
        IrExprKind::BoundedLoop { start, end, body, .. } => {
            collect_assigned_expr(start, out);
            collect_assigned_expr(end, out);
            collect_assigned_block(body, out);
        }
        IrExprKind::Match { expr, arms } => {
            collect_assigned_expr(expr, out);
            for arm in arms {
                collect_assigned_expr(&arm.body, out);
            }
        }
        _ => {}
    }
    if let IrExprKind::MethodCall { receiver, method, .. } = &e.kind {
        if matches!(method, MethodKind::Known(StdMethod::Update)) {
            if let IrExprKind::Var(v) = &receiver.kind {
                out.insert(v.clone());
            }
        }
    }
}

/// `D::new()` where D is the digest type param.
fn is_digest_new(e: &IrExpr, cx: &LeanCx) -> bool {
    let Some(dty) = &cx.digest_ty else { return false };
    let IrExprKind::Call { func, args } = &e.kind else { return false };
    if !args.is_empty() {
        return false;
    }
    let IrExprKind::Path { segments, .. } = &func.kind else { return false };
    segments.len() == 2 && &segments[0] == dty && segments[1] == "new"
}

/// Coarse scan: does this function body need `Id.run do` (mutation, loops,
/// or digest updates)? v1 uses the Debug representation; a dedicated
/// recursion can replace it if it ever false-positives.
fn body_needs_id(b: &IrBlock) -> bool {
    let dbg = format!("{b:?}");
    ["Assign", "BoundedLoop", "IterLoop", "Update"]
        .iter()
        .any(|t| dbg.contains(t))
}

fn block_lean(b: &IrBlock, cx: &LeanCx, out: &mut String) -> Result<()> {
    block_lean_mode(b, cx, out, false)
}

fn block_lean_mode(b: &IrBlock, cx: &LeanCx, out: &mut String, in_id: bool) -> Result<()> {
    let mut cx_local = cx.clone();
    let cx = &mut cx_local;
    let pad = "  ".repeat(cx.cur_indent);
    let mut i = 0;
    let stmts = &b.stmts;
    while i < stmts.len() {
        match &stmts[i].kind {
            IrStmtKind::Let { pattern, init, .. } => {
                let IrPattern::Ident { name, mutable, .. } = pattern else {
                    return Err(CompilerError::Unsupported("non-ident let pattern".into()));
                };
                w!(
                    out,
                    "{pad}{} {} := ",
                    if *mutable { "let mut" } else { "let" },
                    ident(name)
                );
                // `let mut v := 0` later used in bitwise assign-ops defaults
                // to Nat in Lean; the spec's accumulators are UInt8.
                let uint8_zero = *mutable
                    && matches!(&init.as_ref().map(|i| &i.kind),
                        Some(IrExprKind::Lit(IrLit::Int(0))))
                    && block_has_bitop_assign(b, name);
                if let Some(init) = init {
                    if matches!(&init.kind, IrExprKind::Binary { op, .. }
                        if matches!(op, SpecBinOp::Eq | SpecBinOp::Ne | SpecBinOp::Lt
                            | SpecBinOp::Le | SpecBinOp::Gt | SpecBinOp::Ge
                            | SpecBinOp::And | SpecBinOp::Or))
                        || matches!(&init.kind, IrExprKind::Lit(IrLit::Bool(_)))
                    {
                        cx.bool_vars.push(name.clone());
                    }
                    if is_digest_new(init, cx) {
                        w!(out, "DigestSt.new");
                    } else if uint8_zero {
                        w!(out, "(0 : UInt8)");
                    } else {
                        expr_lean(init, cx, out)?;
                    }
                } else {
                    w!(out, "default");
                }
                wln!(out, "");
            }
            IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => {
                w!(out, "{pad}");
                stmt_expr_lean(e, cx, out)?;
                wln!(out, "");
            }
            other => {
                return Err(CompilerError::Unsupported(format!(
                    "stmt {:?}",
                    std::mem::discriminant(other)
                )))
            }
        }
        i += 1;
    }
    if let Some(e) = &b.expr {
    if in_id {
            if let IrExprKind::Match { expr, arms } = &e.kind {
                let split_first = matches!(&expr.kind, IrExprKind::MethodCall { method, .. }
                    if matches!(method, MethodKind::Other(m) if m == "split_first"));
                if split_first {
                    let IrExprKind::MethodCall { receiver, .. } = &expr.kind else {
                        unreachable!()
                    };
                    w!(out, "{pad}match ");
                    expr_lean(receiver, cx, out)?;
                    wln!(out, " with");
                    let mut cx2 = cx.clone();
                    cx2.cur_indent += 1;
                    for arm in arms {
                        let pad2 = "  ".repeat(cx2.cur_indent);
                        w!(out, "{pad2}| ");
                        let is_some = matches!(&arm.pattern,
                            IrPattern::TupleStruct { kind, .. } if kind.to_string().contains("Some"));
                        if is_some {
                            if let IrPattern::TupleStruct { elems, .. } = &arm.pattern {
                                if let Some(IrPattern::Tuple(items)) = elems.first() {
                                    pattern_lean(&items[0], out);
                                    w!(out, " :: ");
                                    pattern_lean(&items[1], out);
                                }
                            }
                        } else {
                            w!(out, "[]");
                        }
                        wln!(out, " =>");
                        match &arm.body.kind {
                            IrExprKind::Block(ab) => block_lean_mode(ab, &cx2, out, true)?,
                            _ => {
                                w!(out, "{pad2}  return ");
                                expr_lean(&arm.body, &cx2, out)?;
                                wln!(out, "");
                            }
                        }
                    }
                    return Ok(());
                }
            }
            if let IrExprKind::If { cond, then_branch, else_branch } = &e.kind {
                w!(out, "{pad}if ");
                expr_lean(cond, cx, out)?;
                wln!(out, " then");
                let mut cx2 = cx.clone();
                cx2.cur_indent += 1;
                block_lean_mode(then_branch, &cx2, out, true)?;
                if let Some(else_b) = else_branch {
                    wln!(out, "{pad}else");
                    match &else_b.kind {
                        IrExprKind::Block(eb) => block_lean_mode(eb, &cx2, out, true)?,
                        _ => {
                            w!(out, "{pad}  ");
                            expr_lean(else_b, cx, out)?;
                            wln!(out, "");
                        }
                    }
                }
                return Ok(());
            }
        }
        w!(out, "{pad}");
        if in_id {
            w!(out, "return ");
        }
        expr_lean(e, cx, out)?;
        wln!(out, "");
    }
    Ok(())
}

fn pattern_lean(p: &IrPattern, out: &mut String) {
    match p {
        IrPattern::Ident { name, .. } => w!(out, "{}", ident(name)),
        IrPattern::Wild => w!(out, "_"),
        IrPattern::Lit(l) => w!(out, "{}", lit_lean(l)),
        IrPattern::Tuple(items) => {
            w!(out, "(");
            for (i, sub) in items.iter().enumerate() {
                if i > 0 {
                    w!(out, ", ");
                }
                pattern_lean(sub, out);
            }
            w!(out, ")");
        }
        IrPattern::TupleStruct { kind, elems } => {
            w!(out, "{}", ident(&kind.to_string()));
            for e in elems {
                w!(out, " (");
                pattern_lean(e, out);
                w!(out, ")");
            }
        }
        IrPattern::Ref { pat, .. } => pattern_lean(pat, out),
        other => w!(out, "/* TODO pattern {:?} */ _", std::mem::discriminant(other)),
    }
}

/// Flatten a nested index lvalue `v[i0][i1]...[ik]` into its base variable
/// and the index chain (outermost first).
fn collect_index_chain(e: &IrExpr) -> Result<(String, Vec<IrExpr>)> {
    let mut chain = Vec::new();
    let mut cur = e;
    loop {
        match &cur.kind {
            IrExprKind::Index { base, index } => {
                chain.push((**index).clone());
                cur = base;
            }
            IrExprKind::Var(v) => {
                chain.reverse();
                return Ok((v.clone(), chain));
            }
            _ => {
                return Err(CompilerError::Unsupported(
                    "index assign through a non-variable base".into(),
                ))
            }
        }
    }
}

/// Emit the nested `List.set` spine for `v[i0][i1]...[ik] := rhs`:
/// `v.set i0 ((v[i0]!).set i1 (...))`. The leaf rhs prints bare — the
/// enclosing level's parentheses are the wrap, matching the historical
/// single-level output byte for byte.
fn emit_set_chain(
    var: &str,
    chain: &[IrExpr],
    rhs: &IrExpr,
    cx: &LeanCx,
    out: &mut String,
) -> Result<()> {
    // read prefix for depth d: v, v[i0]!, v[i0]![i1]!, ...
    fn read_prefix(
        var: &str,
        chain: &[IrExpr],
        depth: usize,
        cx: &LeanCx,
        out: &mut String,
    ) -> Result<()> {
        w!(out, "{}", ident(var));
        for idx in &chain[..depth] {
            w!(out, "[");
            expr_lean(idx, cx, out)?;
            w!(out, "]!");
        }
        Ok(())
    }
    fn rec(
        var: &str,
        chain: &[IrExpr],
        depth: usize,
        rhs: &IrExpr,
        cx: &LeanCx,
        out: &mut String,
    ) -> Result<()> {
        if depth == chain.len() {
            expr_lean(rhs, cx, out)
        } else {
            read_prefix(var, chain, depth, cx, out)?;
            w!(out, ".set ");
            expr_lean(&chain[depth], cx, out)?;
            w!(out, " (");
            rec(var, chain, depth + 1, rhs, cx, out)?;
            w!(out, ")");
            Ok(())
        }
    }
    rec(var, chain, 0, rhs, cx, out)
}

/// Statement-position expressions (assign, return, loops).
fn stmt_expr_lean(e: &IrExpr, cx: &LeanCx, out: &mut String) -> Result<()> {
    match &e.kind {
        IrExprKind::Assign { left, right } => {
            match &left.kind {
                IrExprKind::Var(v) => {
                    w!(out, "{} := ", ident(v));
                    expr_lean(right, cx, out)
                }
                IrExprKind::Index { .. } => {
                    let (v, chain) = collect_index_chain(left)?;
                    w!(out, "{} := ", ident(&v));
                    emit_set_chain(&v, &chain, right, cx, out)
                }
                _ => Err(CompilerError::Unsupported("non-var assignment".into())),
            }
        }
        IrExprKind::AssignOp { op, left, right } => {
            match &left.kind {
                IrExprKind::Var(v) => {
                    w!(out, "{} := {} {} ", ident(v), ident(v), binop_lean(op));
                    expr_lean(right, cx, out)
                }
                IrExprKind::Index { .. } => {
                    let (v, chain) = collect_index_chain(left)?;
                    // rhs' := (v[i0]!..[ik]! <op> rhs), as typed IR.
                    let mut read_expr = IrExpr {
                        kind: IrExprKind::Var(v.clone()),
                        prov: (),
                        side: None,
                    };
                    for idx in &chain {
                        read_expr = IrExpr {
                            kind: IrExprKind::Index {
                                base: Box::new(read_expr),
                                index: Box::new((*idx).clone()),
                            },
                            prov: (),
                            side: None,
                        };
                    }
                    let combined = IrExpr {
                        kind: IrExprKind::Binary {
                            op: *op,
                            left: Box::new(read_expr),
                            right: Box::new((**right).clone()),
                        },
                        prov: (),
                        side: None,
                    };
                    w!(out, "{} := ", ident(&v));
                    emit_set_chain(&v, &chain, &combined, cx, out)
                }
                _ => Err(CompilerError::Unsupported("non-var assign-op".into())),
            }
        }
        IrExprKind::Return(e) => {
            w!(out, "return ");
            if let Some(e) = e {
                expr_lean(e, cx, out)?;
            }
            Ok(())
        }
        IrExprKind::If { cond, then_branch, else_branch }
            if !then_branch.stmts.is_empty()
                || matches!(else_branch.as_deref().map(|e| &e.kind), Some(IrExprKind::Block(b)) if !b.stmts.is_empty()) =>
        {
            // Statement-position `if` with block bodies (do mode). Mirrors
            // the tail-expression If layout in block_lean_mode.
            let pad = "  ".repeat(cx.cur_indent);
            w!(out, "if ");
            expr_lean(cond, cx, out)?;
            wln!(out, " then");
            let mut cx2 = cx.clone();
            cx2.cur_indent += 1;
            block_lean_mode(then_branch, &cx2, out, true)?;
            if let Some(else_b) = else_branch {
                wln!(out, "{pad}else");
                match &else_b.kind {
                    IrExprKind::Block(eb) => block_lean_mode(eb, &cx2, out, true)?,
                    _ => {
                        w!(out, "{pad}  ");
                        expr_lean(else_b, &cx2, out)?;
                        wln!(out, "");
                    }
                }
            }
            Ok(())
        }
        IrExprKind::BoundedLoop { var, start, end, inclusive, body } => {
            w!(out, "for h : {} in [", ident(var));
            expr_lean(start, cx, out)?;
            w!(out, ":");
            expr_lean(end, cx, out)?;
            if *inclusive {
                w!(out, " + 1");
            }
            wln!(out, "] do");
            {
                let mut cx2 = cx.clone();
                cx2.cur_indent += 1;
                block_lean(body, &cx2, out)?;
            }
            Ok(())
        }
        _ => expr_lean(e, cx, out),
    }
}

fn fn_lean(
    f: &IrFunction,
    cx: &LeanCx,
    out: &mut String,
    self_ty: Option<String>,
    self_fields: &[String],
) -> Result<()> {
    fn_lean_inner(f, cx, out, self_ty.clone(), self_fields).map_err(|e| {
        CompilerError::Unsupported(format!("{}: {e} (digest={}, generics={:?})", f.name, cx.digest,
            f.generics.iter().map(|g| format!("{}:{:?}", g.name, g.bounds.iter().map(|b| &b.trait_kind).collect::<Vec<_>>())).collect::<Vec<_>>()))
    })
}

fn fn_lean_inner(
    f: &IrFunction,
    cx: &LeanCx,
    out: &mut String,
    self_ty: Option<String>,
    self_fields: &[String],
) -> Result<()> {
    let name = if self_fields.contains(&f.name) {
        format!("{}_m", f.name)
    } else {
        f.name.clone()
    };
    w!(out, "def {}", ident(&name));
    for (p, v) in &cx.len_params {
        // projected params (O := n/8) are not independent binders
        if cx.div_projections.iter().any(|(q, _, _)| q == p) {
            continue;
        }
        w!(out, " {{{v} : Nat}}");
    }
    if let (Some(st), true) = (self_ty.clone(), f.receiver.is_some()) {
        w!(out, " (self : {st})");
    }
    for p in &f.params {
        w!(out, " ({} : {})", ident(&p.name), ty_lean(&p.ty, cx)?);
    }
    let body_dbg = format!("{:?}", f.body);
    let calls_digest = cx
        .digest_methods
        .iter()
        .any(|m| body_dbg.contains(&format!("Other(\"{m}\"")));
    if (cx.digest && body_dbg.contains("Finalize")) || calls_digest {
        w!(out, " (hash : List UInt8 → List UInt8)");
    }
    let mut cx = cx.clone();
    for p in &f.params {
        if matches!(&p.ty, IrType::Primitive(PrimitiveType::Bool)) {
            cx.bool_vars.push(p.name.clone());
        }
    }
    if let Some(rt) = &f.return_type {
        let mut r = ty_lean(rt, &cx)?;
        if r == "Self" {
            if let Some(st) = &self_ty {
                r = st.clone();
            }
        }
        w!(out, " : {r}");
    }
    let needs_id = body_needs_id(&f.body);
    if needs_id {
        wln!(out, " := Id.run do");
    } else {
        wln!(out, " := ");
    }
    // params mutated by the body are rebound as `let mut` locals (Lean do
    // blocks cannot mutate parameters)
    if needs_id {
        let mut assigned = BTreeSet::new();
        collect_assigned_block(&f.body, &mut assigned);
        for param in &f.params {
            if assigned.contains(&param.name) {
                wln!(out, "  let mut {} := {}", ident(&param.name), ident(&param.name));
            }
        }
    }
    let mut bcx = cx.clone();
    bcx.cur_indent = 1;
    if body_needs_id(&f.body) {
        block_lean_id(&f.body, &bcx, out)?;
    } else {
        block_lean(&f.body, &bcx, out)?;
    }
    Ok(())
}

/// Options for the native Lean printer.
#[derive(Default, Clone, Copy, Debug)]
pub struct LeanPrintOptions {
    /// Exact-arithmetic mode for mxx-derived modules: `u64`/`u128` map to
    /// `Nat`, casts between machine-int types are identities, and
    /// const-generic names may appear in expression position. Only sound
    /// when the source never wraps machine integers (the mxx lowerer
    /// enforces tower moduli < 2^63, so every `u128` intermediate is
    /// < 2^126).
    pub exact_nat: bool,
}

/// Print the selected items of a parsed module as one native Lean module.
pub fn print_module_lean(module: &IrModule<IrFunction>, only: &BTreeSet<String>) -> Result<String> {
    print_module_lean_with(module, only, LeanPrintOptions::default())
}

/// Print the selected items of a parsed module as one native Lean module,
/// with explicit options.
pub fn print_module_lean_with(
    module: &IrModule<IrFunction>,
    only: &BTreeSet<String>,
    opts: LeanPrintOptions,
) -> Result<String> {
    let mut module = module.clone();
    crate::printer_ts::deshadow_module(&mut module);
    let module = &module;
    let mut out = String::new();
    wln!(
        out,
        "-- Auto-generated Lean from volar-spec (volar-compiler printer_lean)."
    );
    wln!(
        out,
        "-- Native representation; voof proves this equivalent to the Aeneas"
    );
    wln!(out, "-- extraction of the same Rust source.");
    wln!(out, "");
    wln!(out, "/-- Digest accumulation state: the ordered list of absorbed byte");
    wln!(out, "    strings. Models `D: Digest`'s new/update/finalize. -/");
    wln!(out, "structure DigestSt where");
    wln!(out, "  parts : List (List UInt8) := []");
    wln!(out, "");
    wln!(out, "def DigestSt.new : DigestSt := {{}}");
    wln!(out, "");
    wln!(out, "def DigestSt.update (d : DigestSt) (x : List UInt8) : DigestSt := ⟨d.parts ++ [x]⟩");
    wln!(out, "");
    wln!(out, "def DigestSt.finalize (d : DigestSt) (hash : List UInt8 → List UInt8) : List UInt8 :=");
    wln!(out, "  hash (d.parts.flatten)");
    wln!(out, "");
    wln!(out, "/-- Little-endian byte expansion of a `UInt64`. -/");
    wln!(out, "def u64LeBytes (x : UInt64) : List UInt8 :=");
    wln!(out, "  (List.range 8).map (fun i => ((x >>> (8 * i.toUInt64)) &&& 0xff).toUInt8)");
    wln!(out, "");

    // Function name → implicit Nat length params (exact-mode call sites
    // pass these by name; see LeanCx::fn_len_params).
    let fn_len_params: Vec<(String, Vec<String>)> = module
        .functions
        .iter()
        .map(|f| {
            let fcx = cx_for_ex(&f.generics, &f.where_clause, opts.exact_nat);
            (f.name.clone(), fcx.len_params.iter().map(|(_, v)| v.clone()).collect())
        })
        .collect();

    let mut digest_methods: Vec<String> = Vec::new();
    for imp in &module.impls {
        for item in &imp.items {
            if let IrImplItem::Method(m) = item {
                if m.generics.iter().any(|g| {
                    g.bounds.iter().any(|b| {
                        b.trait_kind == TraitKind::Digest
                            || b.trait_kind.to_string() == "Digest"
                    })
                }) {
                    digest_methods.push(m.name.clone());
                }
            }
        }
    }

    for s in &module.structs {
        let name = s.kind.to_string();
        if !only.contains(&name) {
            continue;
        }
        let mut cx = cx_for_ex(&s.generics, &[], opts.exact_nat);
        cx.fn_len_params = fn_len_params.clone();
        w!(out, "structure {}", ident(&name));
        for (_, v) in &cx.len_params {
            if cx.div_projections.iter().any(|(q, _, _)| q == v) {
                continue;
            }
            w!(out, " ({} : Nat)", v);
        }
        wln!(out, " where");
        for field in &s.fields {
            wln!(out, "  {} : {}", ident(&field.name), ty_lean(&field.ty, &cx)?);
        }
        // all spec structures are inhabited (List/structure fields)
        wln!(out, "  deriving Inhabited");
        wln!(out, "");
    }

    for c in &module.consts {
        if !only.contains(&c.name) {
            continue;
        }
        let mut cx = cx_for_ex(&[], &[], opts.exact_nat);
        cx.fn_len_params = fn_len_params.clone();
        w!(out, "def {} : {} := ", ident(&c.name), ty_lean(&c.ty, &cx)?);
        expr_lean(&c.value, &cx, &mut out)?;
        wln!(out, "\n");
    }

    for f in &module.functions {
        if !only.contains(&f.name) {
            continue;
        }
        let mut cx = cx_for_ex(&f.generics, &f.where_clause, opts.exact_nat);
        cx.digest_methods = digest_methods.clone();
        cx.fn_len_params = fn_len_params.clone();
        fn_lean(f, &cx, &mut out, None, &[])?;
        wln!(out, "");
    }


    for s in &module.structs {
        let name = s.kind.to_string();
        if !only.contains(&name) {
            continue;
        }
        for imp in &module.impls {
            let self_is =
                matches!(&imp.self_ty, IrType::Struct { kind, .. } if kind.to_string() == name);
            // v1: inherent impls only; trait impls become Lean instances later
            if !self_is || imp.trait_.is_some() {
                continue;
            }
            // Lean requires callees before callers: methods referenced by
            // siblings (by name, dot-call) go first. One stable pass.
            let mut items: Vec<&IrImplItem> = imp.items.iter().collect();
            items.sort_by_key(|item| {
                if let IrImplItem::Method(m) = item {
                    // key 0 = called by a sibling (print first)
                    let called_by_sibling = imp.items.iter().any(|other| {
                        if let IrImplItem::Method(o) = other {
                            o.name != m.name
                                && format!("{:?}", o.body).contains(&format!("\"{}\"", m.name))
                        } else {
                            false
                        }
                    });
                    if called_by_sibling { 0 } else { 1 }
                } else {
                    0
                }
            });
            let mut printed = false;
            for item in items {
                if let IrImplItem::Method(m) = item {
                    let mut all_g = imp.generics.clone();
                    all_g.extend(m.generics.clone());
                    let mut all_w = imp.where_clause.clone();
                    all_w.extend(m.where_clause.clone());
                    let mut mcx = cx_for_ex(&all_g, &all_w, opts.exact_nat);
                    mcx.digest_methods = digest_methods.clone();
                    mcx.fn_len_params = fn_len_params.clone();
                    let mut st = ident(&name);
                    for (p, v) in &mcx.len_params {
                        if mcx.div_projections.iter().any(|(q, _, _)| q == p) {
                            continue;
                        }
                        w!(st, " {v}");
                    }
                    if !printed {
                        wln!(out, "namespace {}\n", ident(&name));
                        printed = true;
                    }
                    let field_names: Vec<String> =
                        s.fields.iter().map(|f| f.name.clone()).collect();
                    fn_lean(m, &mcx, &mut out, Some(st), &field_names)?;
                    wln!(out, "");
                }
            }
            if printed {
                wln!(out, "end {}\n", ident(&name));
            }
        }
    }

    Ok(out)
}
