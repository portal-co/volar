//! Small, reusable `IrStmt`/`IrExpr` construction helpers, mirroring the
//! text-template shapes `split_driver.rs` used to build by hand
//! (`format!("let {name} = {expr};\n")` and friends) -- the toolkit behind
//! the driver's migration from hand-written Rust text to a real,
//! `lower_module_monomorphized`-representable AST (see the user's own
//! framing: "the runtime-loop driver should shift to AST CFG instead of
//! manual Rust").
//!
//! Kept deliberately small: only what `split_driver.rs` actually needs, not
//! a general-purpose IR-building library. `volar-weaver`'s own `vole.rs`
//! has an equivalent (larger, `pub(crate)`-only) toolkit; this one is a
//! separate, independent copy rather than a shared dependency, since the
//! two crates' own construction needs only partially overlap and
//! `vole.rs`'s helpers aren't exported.

use volar_compiler::ir::{
    IrClosureParam, IrExpr, IrExprKind, IrLit, IrPattern, IrStmt, IrStmtKind, IrType, MethodKind,
    PrimitiveType, SpecBinOp,
};

pub fn ir_expr(kind: IrExprKind) -> IrExpr {
    IrExpr::new(kind, (), None)
}

pub fn ir_stmt(kind: IrStmtKind) -> IrStmt {
    IrStmt::new(kind, (), None)
}

pub fn var(name: &str) -> IrExpr {
    ir_expr(IrExprKind::Var(name.to_string()))
}

pub fn int_lit(n: i128) -> IrExpr {
    ir_expr(IrExprKind::Lit(IrLit::Int(n)))
}

pub fn str_lit(s: &str) -> IrExpr {
    ir_expr(IrExprKind::Lit(IrLit::Str(s.to_string())))
}

pub fn clone_expr(e: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::MethodCall {
        receiver: Box::new(e),
        method: MethodKind::from_str("clone"),
        type_args: vec![],
        args: vec![],
    })
}

pub fn ref_expr(e: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Unary {
        op: volar_compiler::ir::SpecUnaryOp::Ref,
        expr: Box::new(e),
    })
}

pub fn ref_mut_expr(e: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Unary {
        op: volar_compiler::ir::SpecUnaryOp::RefMut,
        expr: Box::new(e),
    })
}

/// `<name>.clone()`
pub fn clone_var(name: &str) -> IrExpr {
    clone_expr(var(name))
}

/// `&mut <name>[..]` -- pass a `Box<[T; N]>` pool by mutable SLICE
/// reference, for a callee whose own param type is `&mut [T]`. Deliberately
/// re-slices via an explicit `[..]` rather than relying on deref coercion
/// at the call site (`ref_mut_expr(var(name))`, i.e. plain `&mut <name>`):
/// that single-step coercion is what `Vec<T>` provides for free, but does
/// NOT reliably auto-apply for `Box<[T; N]>` (a two-step coercion -- deref
/// through `Box`, then unsize `[T; N]` to `[T]`) -- confirmed by a real
/// `E0308` type mismatch the first time `_synth_pool`'s declaration moved
/// from `Vec` to `Box<[T; N]>`. Mirrors `volar-weaver`'s own
/// `slice_ref_mut_expr` (same bug, same fix, independently applied here
/// since the driver's own pool declarations aren't reachable from that
/// crate).
pub fn slice_ref_mut_expr(name: &str) -> IrExpr {
    ref_mut_expr(ir_expr(IrExprKind::Index {
        base: Box::new(var(name)),
        index: Box::new(ir_expr(IrExprKind::Range {
            start: None,
            end: None,
            inclusive: false,
        })),
    }))
}

/// `base.field` (used for `tup.0`/`tup.1`-style tuple field access).
pub fn field_expr(base: &str, field: &str) -> IrExpr {
    ir_expr(IrExprKind::Field {
        base: Box::new(var(base)),
        field: field.to_string(),
    })
}

/// `base[idx]` (literal integer index).
pub fn index_lit_expr(base: &str, idx: usize) -> IrExpr {
    ir_expr(IrExprKind::Index {
        base: Box::new(var(base)),
        index: Box::new(int_lit(idx as i128)),
    })
}

/// `<base_expr>.<field>` -- the general form of [`field_expr`], for a base
/// that's already a structured expression (not just a bare name).
pub fn field_expr_on(base: IrExpr, field: &str) -> IrExpr {
    ir_expr(IrExprKind::Field {
        base: Box::new(base),
        field: field.to_string(),
    })
}

/// Parse a small, tightly-scoped subset of Rust expression syntax into a
/// real `IrExpr` tree -- NOT a general Rust parser, just enough for the
/// pre-formatted "leaf" text `split_driver.rs`'s own callers
/// (`mem_probe.rs`/`wat_gen.rs`) build for oracle-bit expressions
/// (`"witness[step].s2_bits[0]"`, or a plain `"true"`/`"false"` literal):
/// an identifier or boolean literal, optionally followed by any number of
/// `.field` / `[index]` postfix operations (an index is itself parsed
/// recursively, so both `arr[i][j]` and `witness[step].bits[k]` work).
/// Exists because `IrExprKind::Var`'s own printer-side guardrail (see this
/// module's `var`) rejects non-identifier text outright -- these strings
/// need to become real `Index`/`Field` nodes, not be smuggled through
/// `Var`. Panics on anything outside this shape: a genuinely new leaf
/// pattern needs a real `IrExpr` built directly by its own caller, not
/// silently mis-parsed here.
pub fn parse_leaf_expr(s: &str) -> IrExpr {
    let s = s.trim();
    if s == "true" {
        return ir_expr(IrExprKind::Lit(IrLit::Bool(true)));
    }
    if s == "false" {
        return ir_expr(IrExprKind::Lit(IrLit::Bool(false)));
    }
    if !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit()) {
        return int_lit(
            s.parse()
                .unwrap_or_else(|_| panic!("parse_leaf_expr: bad integer {s:?}")),
        );
    }
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
        i += 1;
    }
    assert!(
        i > 0,
        "parse_leaf_expr: expected identifier at start of {s:?}"
    );
    let mut expr = var(&s[..i]);
    let mut rest = &s[i..];
    while !rest.is_empty() {
        if let Some(after) = rest.strip_prefix('.') {
            let ab = after.as_bytes();
            let mut j = 0;
            while j < ab.len() && (ab[j].is_ascii_alphanumeric() || ab[j] == b'_') {
                j += 1;
            }
            assert!(
                j > 0,
                "parse_leaf_expr: expected field name after '.' in {s:?}"
            );
            expr = field_expr_on(expr, &after[..j]);
            rest = &after[j..];
        } else if let Some(after) = rest.strip_prefix('[') {
            let close = after
                .find(']')
                .unwrap_or_else(|| panic!("parse_leaf_expr: unmatched '[' in {s:?}"));
            let inner = parse_leaf_expr(&after[..close]);
            expr = ir_expr(IrExprKind::Index {
                base: Box::new(expr),
                index: Box::new(inner),
            });
            rest = &after[close + 1..];
        } else {
            panic!("parse_leaf_expr: unexpected trailing text {rest:?} in {s:?}");
        }
    }
    expr
}

/// A bare, unqualified function call: `name(args...)`.
pub fn call_expr(name: &str, args: Vec<IrExpr>) -> IrExpr {
    path_call_expr(&[name], args)
}

/// A (possibly qualified) function call: `seg0::seg1::...(args...)` --
/// e.g. `path_call_expr(&["Gf128","from_u64"], [x])` for `Gf128::from_u64(x)`.
pub fn path_call_expr(segments: &[&str], args: Vec<IrExpr>) -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path {
            segments: segments.iter().map(|s| s.to_string()).collect(),
            type_args: vec![],
        })),
        args,
    })
}

/// `[elems...]`
pub fn array_lit_expr(elems: Vec<IrExpr>) -> IrExpr {
    ir_expr(IrExprKind::FixedArray(elems))
}

/// `(elems...)` -- `()` for empty, `(x,)` for exactly one (a bare `(x)`
/// would be a parenthesized expression, not a 1-tuple).
pub fn tuple_lit_expr(elems: Vec<IrExpr>) -> IrExpr {
    if elems.is_empty() {
        ir_expr(IrExprKind::Lit(IrLit::Unit))
    } else {
        ir_expr(IrExprKind::Tuple(elems))
    }
}

/// `let <name> = <init>;` (immutable, untyped).
pub fn let_stmt(name: &str, init: IrExpr) -> IrStmt {
    ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::ident(name),
        ty: None,
        init: Some(init),
    })
}

/// `let <name>: <ty> = <init>;`
pub fn let_typed_stmt(name: &str, ty: IrType, init: IrExpr) -> IrStmt {
    ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::ident(name),
        ty: Some(ty),
        init: Some(init),
    })
}

/// `let (<names...>) = <init>;` -- `names.len() == 1` still emits real
/// tuple-destructuring syntax (`let (x,) = ..;`), matching
/// `IrPattern::Tuple`'s own printer support for the single-element case
/// (see `crates/compiler/volar-compiler/src/printer.rs`'s own fix for
/// exactly this -- a bare `(x)` pattern is a parenthesized single pattern,
/// not a 1-tuple destructure).
pub fn let_tuple_stmt(names: &[String], init: IrExpr) -> IrStmt {
    let pat = IrPattern::Tuple(names.iter().map(|n| IrPattern::ident(n.clone())).collect());
    ir_stmt(IrStmtKind::Let {
        pattern: pat,
        ty: None,
        init: Some(init),
    })
}

/// `<lhs> = <rhs>;` (a bare assignment statement, not a `let`).
pub fn assign_stmt(lhs: IrExpr, rhs: IrExpr) -> IrStmt {
    ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
        left: Box::new(lhs),
        right: Box::new(rhs),
    })))
}

/// `<pool>[<idx>] = <value>; <pool>_written[<idx>] = true;` -- a pool's own
/// direct indexed write (see `volar-weaver`'s own `pool_index_write_stmt`,
/// the same convention, independently applied here since the driver's own
/// `_synth_pool`/`_w_pool` writes aren't reachable from that crate).
pub fn pool_write_stmts(pool: &str, written: &str, idx: usize, value: IrExpr) -> [IrStmt; 2] {
    [
        assign_stmt(index_lit_expr(pool, idx), value),
        assign_stmt(
            index_lit_expr(written, idx),
            ir_expr(IrExprKind::Lit(IrLit::Bool(true))),
        ),
    ]
}

/// `core::array::from_fn(|<idx_var>| <body>)` -- a plain builtin `[T; N]`
/// array (matches `volar_compiler::ir::box_new_array_expr`'s own inner
/// construction exactly, minus the enclosing `Box::new`).
pub fn array_from_fn_expr(idx_var: &str, body: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path {
            segments: vec!["core".into(), "array".into(), "from_fn".into()],
            type_args: vec![],
        })),
        args: vec![ir_expr(IrExprKind::Closure {
            params: vec![IrClosureParam {
                pattern: IrPattern::ident(idx_var),
                ty: None,
            }],
            ret_type: None,
            body: Box::new(body),
        })],
    })
}

/// `<a> * <b>`
pub fn mul_expr(a: IrExpr, b: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Mul,
        left: Box::new(a),
        right: Box::new(b),
    })
}

/// `<a> + <b>`
pub fn add_expr(a: IrExpr, b: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Add,
        left: Box::new(a),
        right: Box::new(b),
    })
}

/// `<a> as <ty>`
pub fn cast_expr(a: IrExpr, ty: IrType) -> IrExpr {
    ir_expr(IrExprKind::Cast {
        expr: Box::new(a),
        ty: Box::new(ty),
    })
}

/// `__assert_true(<cond>, "<msg>")` -- a real function call, not the
/// `assert!` macro (this IR has no macro-invocation support at all, see
/// `volar-weaver`'s own `pool_decl_stmt` doc for the same constraint).
/// Requires the surrounding hand-written driver harness to define
/// `fn __assert_true(cond: bool, msg: &str) { assert!(cond, "{}", msg); }`
/// once (mem_probe.rs/wat_gen.rs's own preamble text, alongside their
/// existing `vope_zero`/`sample_g`/etc. helpers) -- the macro itself still
/// exists in the codebase, just not inside anything IR-generated. `msg` is
/// necessarily a static, build-time string (no runtime step-number
/// interpolation, unlike the old text version's `"step {} ...", step_expr`
/// -- `format!` is a macro too) -- an honest, minor diagnostic-detail
/// simplification, not a correctness change: the assertion still fires
/// correctly, it just can't name which real step it fired on.
pub fn assert_true_stmt(cond: IrExpr, msg: &str) -> IrStmt {
    ir_stmt(IrStmtKind::Semi(call_expr(
        "__assert_true",
        vec![cond, str_lit(msg)],
    )))
}

/// `let <name>: [Gf128; and_count] = core::array::from_fn(|k| Gf128::from_u64(
///   ((<step_expr> as u64) * 10_000_000 + <and_gate_seed>u64 * 100_000) * 1_000_003 + k as u64
/// ));` -- the per-block/chunk/finish fold-challenge array, identical
/// formula at all 3 call sites (see `split_driver.rs`'s own doc: "a plain
/// deterministic arithmetic formula, not real Fiat-Shamir/verifier
/// randomness" -- soundness doesn't depend on this being unpredictable
/// here, this is a driven *test*, not a real deployment).
pub fn r_ands_decl_stmt(
    name: &str,
    and_count: usize,
    step_expr: &str,
    and_gate_seed: u64,
) -> IrStmt {
    let step_as_u64 = cast_expr(var(step_expr), IrType::Primitive(PrimitiveType::U64));
    let seed_term = mul_expr(step_as_u64, int_lit(10_000_000));
    let seed_term = add_expr(
        seed_term,
        mul_expr(int_lit(and_gate_seed as i128), int_lit(100_000)),
    );
    let scaled = mul_expr(seed_term, int_lit(1_000_003));
    let k_as_u64 = cast_expr(var("k"), IrType::Primitive(PrimitiveType::U64));
    let body = path_call_expr(&["Gf128", "from_u64"], vec![add_expr(scaled, k_as_u64)]);
    let gf128_ty = IrType::Struct {
        kind: volar_compiler::ir::StructKind::Custom("Gf128".into()),
        type_args: vec![],
    };
    let ty = IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(gf128_ty),
        len: volar_compiler::ir::ArrayLength::Const(and_count),
    };
    let_typed_stmt(name, ty, array_from_fn_expr("k", body))
}

/// The `_synth_pool_{vope,q}[_written]` declarations (see
/// `split_driver.rs`'s own doc on why this pool is declared fresh inside
/// the real runtime loop's own body, unlike `_w_pool`, which is
/// loop-persistent) -- statically sized via `Box<[T; total_vars]>`
/// (`volar_compiler::ir::box_array_type`/`box_new_array_expr`, the same
/// heap-allocation abstraction `volar-weaver`'s own pools use), not `Vec`.
pub fn synth_pool_decl_stmts(total_vars: usize) -> Vec<IrStmt> {
    use volar_compiler::ir::{box_array_type, box_new_array_expr};
    let vope_ty = IrType::Struct {
        kind: volar_compiler::ir::StructKind::Custom("Vope".into()),
        type_args: vec![
            IrType::TypeParam("N".into()),
            IrType::TypeParam("Galois".into()),
            IrType::TypeParam("cipher::consts::U1".into()),
        ],
    };
    let q_ty = IrType::Struct {
        kind: volar_compiler::ir::StructKind::Custom("Q".into()),
        type_args: vec![
            IrType::TypeParam("N".into()),
            IrType::TypeParam("Galois".into()),
        ],
    };
    let vope_default = path_call_expr(&["Vope", "default"], vec![]);
    let q_default = path_call_expr(&["Q", "default"], vec![]);
    let false_lit = ir_expr(IrExprKind::Lit(IrLit::Bool(false)));
    vec![
        ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident("_synth_pool_vope").as_mut(),
            ty: Some(box_array_type(vope_ty.clone(), total_vars)),
            init: Some(box_new_array_expr(vope_ty, vope_default, total_vars)),
        }),
        ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident("_synth_pool_vope_written").as_mut(),
            ty: Some(box_array_type(
                IrType::Primitive(PrimitiveType::Bool),
                total_vars,
            )),
            init: Some(box_new_array_expr(
                IrType::Primitive(PrimitiveType::Bool),
                false_lit.clone(),
                total_vars,
            )),
        }),
        ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident("_synth_pool_q").as_mut(),
            ty: Some(box_array_type(q_ty.clone(), total_vars)),
            init: Some(box_new_array_expr(q_ty, q_default, total_vars)),
        }),
        ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident("_synth_pool_q_written").as_mut(),
            ty: Some(box_array_type(
                IrType::Primitive(PrimitiveType::Bool),
                total_vars,
            )),
            init: Some(box_new_array_expr(
                IrType::Primitive(PrimitiveType::Bool),
                false_lit,
                total_vars,
            )),
        }),
    ]
}

/// Render a flat statement sequence (no enclosing braces, no trailing
/// expression) as Rust source text -- the bridge back to
/// `split_driver.rs`'s existing `String`-returning public API, so today's
/// callers (`mem_probe.rs`/`wat_gen.rs`) need no changes at all. A future
/// caller wanting the real AST instead of text uses the `_ir` value these
/// statements came from directly.
pub fn print_stmts(stmts: &[IrStmt]) -> String {
    use volar_compiler::printer::{DisplayRust, StmtWriter};
    let mut out = String::new();
    for stmt in stmts {
        out.push_str(
            &DisplayRust(StmtWriter {
                stmt,
                level: 0,
                ctx: None,
            })
            .to_string(),
        );
    }
    out
}
