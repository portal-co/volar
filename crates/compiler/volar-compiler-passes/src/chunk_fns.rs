// @reliability: normal
//! Function-body chunking pass.
//!
//! Splits large flat function bodies into chains of smaller helper functions.
//! Intended for woven code (VOLE prover/verifier, garbler/evaluator) where a
//! single function can contain thousands of `let` bindings, causing Rust
//! compiler / LLVM slowness.
//!
//! # Eligibility
//!
//! A function is chunked only when ALL of:
//! - `external_kind == ExternalKind::Normal`
//! - `receiver.is_none()` (not a method)
//! - Every statement in `body.stmts` is `IrStmt::Let`
//! - `body.stmts.len() > threshold`
//! - Every `Let` stmt has `ty: Some(_)` (explicit type annotation)
//!
//! # Output shape
//!
//! For a function `fn foo(a: A, b: &mut B) { let x0 = ..; ... let xN = ..; expr }`
//! with two chunks `[0..K)` and `[K..N)`, the pass produces:
//!
//! ```text
//! fn foo(a: A, b: &mut B) -> RetTy {
//!     let _c0 = foo_chunk_0(a, b, ());
//!     foo_chunk_1(a, b, _c0)
//! }
//!
//! fn foo_chunk_0(a: A, b: &mut B, _chunk_in: ()) -> (T_live0, T_live1) { ... }
//! fn foo_chunk_1(a: A, b: &mut B, _chunk_in: (T_live0, T_live1)) -> RetTy { ... }
//! ```

#[cfg(feature = "std")]
use std::collections::{BTreeMap, BTreeSet};
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::string::String;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::collections::{BTreeMap, BTreeSet};
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use volar_compiler::deshadow::pattern_names;
use volar_compiler::ir::{
    ExternalKind, IrBlock, IrExpr, IrFunction, IrIterChain, IrModule, IrParam, IrPattern, IrStmt,
    IrType, IterChainSource, IterStep, IterTerminal,
};

// ============================================================================
// Public API
// ============================================================================

/// Split oversized function bodies in `module` into helper chains.
///
/// Ineligible functions (non-Normal, methods, non-Let stmts, missing types)
/// are passed through unchanged.
pub fn chunk_function_bodies(
    module: &IrModule<IrFunction>,
    threshold: usize,
) -> IrModule<IrFunction> {
    let mut new_fns: Vec<IrFunction> = Vec::new();

    for func in &module.functions {
        if !is_eligible(func, threshold) {
            new_fns.push(func.clone());
            continue;
        }
        let (dispatcher, helpers) = chunk_one_function(func, threshold);
        new_fns.push(dispatcher);
        new_fns.extend(helpers);
    }

    IrModule {
        name: module.name.clone(),
        structs: module.structs.clone(),
        enums: module.enums.clone(),
        traits: module.traits.clone(),
        impls: module.impls.clone(),
        functions: new_fns,
        type_aliases: module.type_aliases.clone(),
        consts: module.consts.clone(),
    }
}

// ============================================================================
// Eligibility check
// ============================================================================

fn is_eligible(func: &IrFunction, threshold: usize) -> bool {
    func.external_kind == ExternalKind::Normal
        && func.receiver.is_none()
        && func.body.stmts.len() > threshold
        && func.body.stmts.iter().all(|s| matches!(s, IrStmt::Let { ty: Some(_), .. }))
}

// ============================================================================
// Core chunking logic
// ============================================================================

fn chunk_one_function(func: &IrFunction, threshold: usize) -> (IrFunction, Vec<IrFunction>) {
    let stmts = &func.body.stmts;
    let n = stmts.len();
    let per_chunk = threshold;

    // Collect (name, type) for every Let-bound variable.
    let mut binding_types: BTreeMap<String, IrType> = BTreeMap::new();
    for stmt in stmts {
        if let IrStmt::Let { pattern, ty: Some(ty), .. } = stmt {
            for name in pattern_names(pattern) {
                binding_types.insert(name, ty.clone());
            }
        }
    }

    // Compute live-across variable sets at each chunk boundary.
    // live_across[k] = variables live from chunks 0..k into chunks k..N.
    let num_chunks = (n + per_chunk - 1) / per_chunk;
    let boundaries: Vec<usize> = (1..num_chunks).map(|k| k * per_chunk).collect();

    let live_across: Vec<Vec<(String, IrType)>> = boundaries.iter().map(|&split| {
        // defined_before = all names bound in stmts[0..split]
        let mut defined_before: BTreeSet<String> = BTreeSet::new();
        for stmt in &stmts[..split] {
            if let IrStmt::Let { pattern, .. } = stmt {
                for name in pattern_names(pattern) {
                    defined_before.insert(name);
                }
            }
        }
        // used_after = all Var-refs in stmts[split..] and body.expr
        let mut used_after: BTreeSet<String> = BTreeSet::new();
        for stmt in &stmts[split..] {
            if let IrStmt::Let { init: Some(e), .. } = stmt {
                collect_vars_in_expr(e, &mut used_after);
            }
        }
        if let Some(e) = &func.body.expr {
            collect_vars_in_expr(e, &mut used_after);
        }
        // live = intersection, sorted for deterministic output
        let mut live: Vec<(String, IrType)> = defined_before.iter()
            .filter(|name| used_after.contains(*name))
            .filter_map(|name| binding_types.get(name).map(|ty| (name.clone(), ty.clone())))
            .collect();
        live.sort_by(|a, b| a.0.cmp(&b.0));
        live
    }).collect();

    // Build helper functions.
    let mut helpers: Vec<IrFunction> = Vec::new();
    let base_name = &func.name;

    for (chunk_idx, _) in (0..num_chunks).enumerate() {
        let stmt_start = chunk_idx * per_chunk;
        let stmt_end = (stmt_start + per_chunk).min(n);
        let is_last = chunk_idx == num_chunks - 1;

        // live-in from previous chunk boundary (index chunk_idx - 1)
        let live_in: &[(String, IrType)] = if chunk_idx == 0 {
            &[]
        } else {
            &live_across[chunk_idx - 1]
        };

        // live-out to next chunk boundary (index chunk_idx)
        let live_out: &[(String, IrType)] = if is_last {
            &[]
        } else {
            &live_across[chunk_idx]
        };

        let helper_name = format!("{base_name}_chunk_{chunk_idx}");

        // chunk_in param type: () for first chunk, tuple of live-in types otherwise.
        let chunk_in_ty = if live_in.is_empty() {
            IrType::Unit
        } else {
            IrType::Tuple(live_in.iter().map(|(_, ty)| ty.clone()).collect())
        };
        let mut helper_params: Vec<IrParam> = func.params.clone();
        helper_params.push(IrParam { name: "_chunk_in".into(), ty: chunk_in_ty });

        // Return type: tuple of live-out vars (or original return type for last chunk).
        let return_type = if is_last {
            func.return_type.clone()
        } else {
            Some(if live_out.is_empty() {
                IrType::Unit
            } else {
                IrType::Tuple(live_out.iter().map(|(_, ty)| ty.clone()).collect())
            })
        };

        // Body: destructure chunk_in, then the segment's stmts, then return expr.
        let mut body_stmts: Vec<IrStmt> = Vec::new();

        // Destructure live-in tuple: `let (v0, v1, ..) = _chunk_in;`
        if !live_in.is_empty() {
            let tuple_pat = IrPattern::Tuple(
                live_in.iter().map(|(name, _)| IrPattern::Ident {
                    mutable: false,
                    name: name.clone(),
                    subpat: None,
                }).collect()
            );
            body_stmts.push(IrStmt::Let {
                pattern: tuple_pat,
                ty: None,
                init: Some(IrExpr::Var("_chunk_in".into())),
            });
        }

        // The segment's own statements.
        body_stmts.extend(stmts[stmt_start..stmt_end].iter().cloned());

        // Return expression: live-out tuple for non-last; original body.expr for last.
        let body_expr = if is_last {
            func.body.expr.clone()
        } else {
            let tuple_exprs: Vec<IrExpr> = live_out.iter()
                .map(|(name, _)| IrExpr::Var(name.clone()))
                .collect();
            Some(Box::new(if tuple_exprs.len() == 1 {
                tuple_exprs.into_iter().next().unwrap()
            } else {
                IrExpr::Tuple(tuple_exprs)
            }))
        };

        helpers.push(IrFunction {
            name: helper_name,
            module_path: func.module_path.clone(),
            generics: func.generics.clone(),
            receiver: None,
            params: helper_params,
            return_type,
            where_clause: func.where_clause.clone(),
            body: IrBlock { stmts: body_stmts, stmt_provs: Vec::new(), expr: body_expr },
            external_kind: ExternalKind::Normal,
        });
    }

    // Build dispatcher: the original function calls helpers in sequence.
    let dispatcher = build_dispatcher(func, &helpers, &live_across, num_chunks);

    (dispatcher, helpers)
}

// ============================================================================
// Dispatcher builder
// ============================================================================

fn build_dispatcher(
    func: &IrFunction,
    helpers: &[IrFunction],
    live_across: &[Vec<(String, IrType)>],
    num_chunks: usize,
) -> IrFunction {
    // Original param names for forwarding.
    let param_exprs: Vec<IrExpr> = func.params.iter()
        .map(|p| IrExpr::Var(p.name.clone()))
        .collect();

    let mut body_stmts: Vec<IrStmt> = Vec::new();

    for chunk_idx in 0..num_chunks {
        let helper_name = &helpers[chunk_idx].name;
        let is_last = chunk_idx == num_chunks - 1;

        // chunk_in arg: () for first, _c{k-1} for others.
        let chunk_in_arg = if chunk_idx == 0 {
            IrExpr::Tuple(Vec::new())
        } else {
            IrExpr::Var(format!("_c{}", chunk_idx - 1))
        };

        let mut call_args: Vec<IrExpr> = param_exprs.clone();
        call_args.push(chunk_in_arg);

        let call_expr = IrExpr::Call {
            func: Box::new(IrExpr::Var(helper_name.clone())),
            args: call_args,
        };

        if is_last {
            // Final call becomes the block's tail expression (no let-binding).
            let disp_body = IrBlock {
                stmts: body_stmts,
                stmt_provs: Vec::new(),
                expr: Some(Box::new(call_expr)),
            };
            return IrFunction {
                name: func.name.clone(),
                module_path: func.module_path.clone(),
                generics: func.generics.clone(),
                receiver: func.receiver.clone(),
                params: func.params.clone(),
                return_type: func.return_type.clone(),
                where_clause: func.where_clause.clone(),
                body: disp_body,
                external_kind: ExternalKind::Normal,
            };
        } else {
            // Intermediate: `let _c{chunk_idx} = helper_chunk_X(...);`
            let out_ty = {
                let live_out = &live_across[chunk_idx];
                if live_out.is_empty() {
                    IrType::Unit
                } else {
                    IrType::Tuple(live_out.iter().map(|(_, ty)| ty.clone()).collect())
                }
            };
            body_stmts.push(IrStmt::Let {
                pattern: IrPattern::Ident {
                    mutable: false,
                    name: format!("_c{chunk_idx}"),
                    subpat: None,
                },
                ty: Some(out_ty),
                init: Some(call_expr),
            });
        }
    }

    // Unreachable if num_chunks >= 1.
    unreachable!("chunk_one_function called with num_chunks == 0")
}

// ============================================================================
// Variable reference collector
// ============================================================================

/// Collect all `IrExpr::Var` names reachable in `expr` into `out`.
pub fn collect_vars_in_expr(expr: &IrExpr, out: &mut BTreeSet<String>) {
    match expr {
        IrExpr::Var(v) => { out.insert(v.clone()); }
        IrExpr::Binary { left, right, .. }
        | IrExpr::Assign { left, right }
        | IrExpr::AssignOp { left, right, .. } => {
            collect_vars_in_expr(left, out);
            collect_vars_in_expr(right, out);
        }
        IrExpr::Unary { expr: e, .. }
        | IrExpr::Cast { expr: e, .. }
        | IrExpr::Try(e)
        | IrExpr::Return(Some(e))
        | IrExpr::Break(Some(e)) => collect_vars_in_expr(e, out),
        IrExpr::Field { base, .. } => collect_vars_in_expr(base, out),
        IrExpr::Index { base, index } => {
            collect_vars_in_expr(base, out);
            collect_vars_in_expr(index, out);
        }
        IrExpr::Call { func, args } => {
            collect_vars_in_expr(func, out);
            for a in args { collect_vars_in_expr(a, out); }
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            collect_vars_in_expr(receiver, out);
            for a in args { collect_vars_in_expr(a, out); }
        }
        IrExpr::Block(b) => collect_vars_in_block(b, out),
        IrExpr::If { cond, then_branch, else_branch } => {
            collect_vars_in_expr(cond, out);
            collect_vars_in_block(then_branch, out);
            if let Some(e) = else_branch { collect_vars_in_expr(e, out); }
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, e) in fields { collect_vars_in_expr(e, out); }
            if let Some(r) = rest { collect_vars_in_expr(r, out); }
        }
        IrExpr::Array(elems) | IrExpr::Tuple(elems) | IrExpr::FixedArray(elems) => {
            for e in elems { collect_vars_in_expr(e, out); }
        }
        IrExpr::Closure { body, .. } => collect_vars_in_expr(body, out),
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start { collect_vars_in_expr(s, out); }
            if let Some(e) = end { collect_vars_in_expr(e, out); }
        }
        IrExpr::IterPipeline(chain) => collect_vars_in_iter_chain(chain, out),
        IrExpr::ArrayGenerate { body, .. } => collect_vars_in_expr(body, out),
        IrExpr::BoundedLoop { start, end, body, .. } => {
            collect_vars_in_expr(start, out);
            collect_vars_in_expr(end, out);
            collect_vars_in_block(body, out);
        }
        IrExpr::IterLoop { collection, body, .. } => {
            collect_vars_in_expr(collection, out);
            collect_vars_in_block(body, out);
        }
        IrExpr::WhileLoop { cond, body } => {
            collect_vars_in_expr(cond, out);
            collect_vars_in_block(body, out);
        }
        IrExpr::Repeat { elem, len } => {
            collect_vars_in_expr(elem, out);
            collect_vars_in_expr(len, out);
        }
        IrExpr::RawMap { receiver, body, .. } => {
            collect_vars_in_expr(receiver, out);
            collect_vars_in_expr(body, out);
        }
        IrExpr::RawZip { left, right, body, .. } => {
            collect_vars_in_expr(left, out);
            collect_vars_in_expr(right, out);
            collect_vars_in_expr(body, out);
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            collect_vars_in_expr(receiver, out);
            collect_vars_in_expr(init, out);
            collect_vars_in_expr(body, out);
        }
        IrExpr::Match { expr: scrutinee, arms } => {
            collect_vars_in_expr(scrutinee, out);
            for arm in arms {
                if let Some(g) = &arm.guard { collect_vars_in_expr(g, out); }
                collect_vars_in_expr(&arm.body, out);
            }
        }
        IrExpr::Lit(_)
        | IrExpr::Path { .. }
        | IrExpr::DefaultValue { .. }
        | IrExpr::LengthOf(_)
        | IrExpr::TypenumUsize { .. }
        | IrExpr::Return(None)
        | IrExpr::Break(None)
        | IrExpr::Continue
        | IrExpr::Unreachable => {}
        _ => {}
    }
}

fn collect_vars_in_block(block: &IrBlock, out: &mut BTreeSet<String>) {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { init: Some(e), .. }
            | IrStmt::Semi(e)
            | IrStmt::Expr(e) => collect_vars_in_expr(e, out),
            IrStmt::Let { init: None, .. } => {}
            _ => {}
        }
    }
    if let Some(e) = &block.expr { collect_vars_in_expr(e, out); }
}

fn collect_vars_in_iter_chain(chain: &IrIterChain, out: &mut BTreeSet<String>) {
    match &chain.source {
        IterChainSource::Method { collection, .. } => collect_vars_in_expr(collection, out),
        IterChainSource::Range { start, end, .. } => {
            collect_vars_in_expr(start, out);
            collect_vars_in_expr(end, out);
        }
        IterChainSource::Zip { left, right } => {
            collect_vars_in_iter_chain(left, out);
            collect_vars_in_iter_chain(right, out);
        }
    }
    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => collect_vars_in_expr(body, out),
            IterStep::Enumerate
            | IterStep::Take { .. }
            | IterStep::Skip { .. }
            | IterStep::Chain { .. } => {}
        }
    }
    match &chain.terminal {
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => {}
        IterTerminal::Fold { init, body, .. } => {
            collect_vars_in_expr(init, out);
            collect_vars_in_expr(body, out);
        }
    }
}
