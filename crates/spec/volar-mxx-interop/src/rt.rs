// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! Access to the bundled `mxx_rt.rs` runtime helpers, as IR.
//!
//! The helpers are plain spec-subset Rust (see `rust_rt/mxx_rt.rs`). They
//! are parsed into spec IR through the same `volar_compiler::parse_sources`
//! pipeline as hand-written spec code, so the IR a lowered program calls is
//! exactly the IR the normal spec backends see — no parallel construction.

use core::fmt::Write as _;
use volar_compiler::{IrExpr, IrExprKind, IrFunction, IrModule, IrType, SourceInput, parse_sources};

use crate::InteropError;

/// Render a lowered module as standalone Rust, without the dyn preamble's
/// spec-crate imports (`use typenum::...`, `use volar_common::...`).
///
/// The interop lowerer's output is self-contained (fixed-size arrays and
/// `u64`/`u128` arithmetic only), so the spec-crate preamble does not
/// apply. This uses the printer's structured `ModuleWriter` API — no text
/// manipulation of printer output.
///
/// /// @volar-allow-rust-text: test-fixture: standalone rendering exists so
/// interop tests can compile lowered modules with `rustc` directly (core
/// design rule 2); the Phase-1 spec integration consumes the IR through
/// the normal gen-specs pipeline, not this text.
pub fn print_standalone(module: &IrModule<IrFunction>) -> String {
    let mut out = String::new();
    let _ = write!(
        out,
        "{}",
        volar_compiler::printer::DisplayRust(volar_compiler::printer::ModuleWriter {
            module,
            emit_async: false,
        })
    );
    out
}

/// The bundled helper source, verbatim.
pub const MXX_RT_SOURCE: &str = include_str!("../rust_rt/mxx_rt.rs");

/// Parse the bundled helpers into spec IR functions.
///
/// The returned functions are in source order; callers typically keep only
/// the ones a lowered program actually references.
pub fn helper_functions() -> Result<Vec<IrFunction>, InteropError> {
    let sources = [SourceInput {
        source: MXX_RT_SOURCE,
        name: "mxx_rt",
    }];
    let module = parse_sources(&sources, "mxx_rt", &["mxx_rt".to_string()])
        .map_err(|e| InteropError::HelperParse(format!("{e:?}")))?;
    if !module.structs.is_empty()
        || !module.enums.is_empty()
        || !module.traits.is_empty()
        || !module.impls.is_empty()
        || !module.consts.is_empty()
    {
        return Err(InteropError::HelperParse(
            "mxx_rt.rs must contain only free functions".into(),
        ));
    }
    let mut functions = module.functions;
    for f in &mut functions {
        annotate_helper(f).map_err(|e| {
            InteropError::HelperParse(format!("in helper `{}`: {e}", f.name))
        })?;
    }
    Ok(functions)
}

/// Assign true array lengths to `ArrayGenerate` nodes in parsed helpers.
///
/// The parser records a placeholder length for turbofish-free
/// `core::array::from_fn` calls (Rust infers the length from context). The
/// Lean printer needs the real length at every level, so we align each
/// helper's nested `from_fn` spine with its declared return type's array
/// layers — the helpers are pure `from_fn` nests by construction (see the
/// closed-form discipline note in `rust_rt/mxx_rt.rs`).
fn annotate_helper(f: &mut IrFunction) -> Result<(), InteropError> {
    let ret = f.return_type.clone();
    for s in &mut f.body.stmts {
        annotate_stmt_lens(s)?;
    }
    if let Some(e) = &mut f.body.expr {
        annotate_expr_lens(e, ret.as_ref())?;
    }
    Ok(())
}

fn annotate_stmt_lens(s: &mut volar_compiler::IrStmt) -> Result<(), InteropError> {
    match &mut s.kind {
        volar_compiler::IrStmtKind::Let { init, .. } => {
            if let Some(e) = init {
                annotate_expr_lens(e, None)?;
            }
        }
        volar_compiler::IrStmtKind::Semi(e) | volar_compiler::IrStmtKind::Expr(e) => {
            annotate_expr_lens(e, None)?;
        }
        other => {
            return Err(InteropError::HelperParse(format!(
                "statement form in helpers not covered by lens annotation: {:?}",
                std::mem::discriminant(other)
            )))
        }
    }
    Ok(())
}

/// Annotate every `ArrayGenerate` reachable from `expr`; `ty` is the
/// expression's expected type when the parent knows it.
fn annotate_expr_lens(expr: &mut IrExpr, ty: Option<&IrType>) -> Result<(), InteropError> {
    match &mut expr.kind {
        IrExprKind::ArrayGenerate { len, body, .. } => {
            let Some(IrType::Array { len: true_len, elem, .. }) = ty else {
                return Err(InteropError::HelperParse(
                    "from_fn nest shallower than the helper's return type".into(),
                ));
            };
            *len = true_len.clone();
            annotate_expr_lens(body, Some(elem))
        }
        IrExprKind::Call { func, args } => {
            annotate_expr_lens(func, None)?;
            for a in args {
                annotate_expr_lens(a, None)?;
            }
            Ok(())
        }
        IrExprKind::Binary { left, right, .. } => {
            annotate_expr_lens(left, None)?;
            annotate_expr_lens(right, None)
        }
        IrExprKind::Unary { expr: e, .. } => annotate_expr_lens(e, None),
        IrExprKind::Cast { expr: e, .. } => annotate_expr_lens(e, None),
        IrExprKind::Index { base, index } => {
            annotate_expr_lens(base, None)?;
            annotate_expr_lens(index, None)
        }
        IrExprKind::If { cond, then_branch, else_branch } => {
            annotate_expr_lens(cond, None)?;
            for s in &mut then_branch.stmts {
                annotate_stmt_lens(s)?;
            }
            if let Some(e) = &mut then_branch.expr {
                annotate_expr_lens(e, None)?;
            }
            if let Some(e) = else_branch {
                annotate_expr_lens(e, None)?;
            }
            Ok(())
        }
        IrExprKind::Block(b) => {
            for s in &mut b.stmts {
                annotate_stmt_lens(s)?;
            }
            // A block's type is its tail expression's type.
            if let Some(e) = &mut b.expr {
                annotate_expr_lens(e, ty)?;
            }
            Ok(())
        }
        IrExprKind::Repeat { elem, len } => {
            annotate_expr_lens(elem, None)?;
            annotate_expr_lens(len, None)
        }
        IrExprKind::FixedArray(items) | IrExprKind::Array(items) => {
            for it in items {
                annotate_expr_lens(it, None)?;
            }
            Ok(())
        }
        IrExprKind::IterPipeline(chain) => {
            if let volar_compiler::IterTerminal::Fold { init, body, .. } = &mut chain.terminal {
                annotate_expr_lens(init, None)?;
                annotate_expr_lens(body, None)?;
            }
            if let volar_compiler::IterChainSource::Range { start, end, .. } = &mut chain.source {
                annotate_expr_lens(start, None)?;
                annotate_expr_lens(end, None)?;
            }
            Ok(())
        }
        IrExprKind::Closure { body, .. } => annotate_expr_lens(body, None),
        IrExprKind::MethodCall { receiver, args, .. } => {
            annotate_expr_lens(receiver, None)?;
            for a in args {
                annotate_expr_lens(a, None)?;
            }
            Ok(())
        }
        IrExprKind::Tuple(items) => {
            for it in items {
                annotate_expr_lens(it, None)?;
            }
            Ok(())
        }
        IrExprKind::StructExpr { fields, rest, .. } => {
            for (_, f) in fields {
                annotate_expr_lens(f, None)?;
            }
            if let Some(r) = rest {
                annotate_expr_lens(r, None)?;
            }
            Ok(())
        }
        IrExprKind::Match { expr: e, arms } => {
            annotate_expr_lens(e, None)?;
            for arm in arms {
                if let Some(g) = &mut arm.guard {
                    annotate_expr_lens(g, None)?;
                }
                annotate_expr_lens(&mut arm.body, None)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}
