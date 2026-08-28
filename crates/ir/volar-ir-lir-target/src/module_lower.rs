// @reliability: experimental
// @ai: assisted
//! Lower an [`IrModule`] to Volar IR, inlining all local function calls.
//!
//! # Algorithm
//!
//! 1. **Call-graph scan** — for each function, collect which other local
//!    functions it calls by walking its `IrExpr` tree.
//! 2. **Topological sort** — order functions so that every callee is lowered
//!    before its callers (DFS post-order).  Mutually-recursive functions will
//!    leave at least one callee un-inlined at the point it is first used
//!    (the call remains as an unresolved `call_extern`); this is unavoidable
//!    for infinite recursion and is documented as a limitation.
//! 3. **Per-function lowering** — each function is lowered with a fresh
//!    [`VolarIrTarget`] that has all previously-lowered locals registered as
//!    inlinable externs via [`VolarIrTarget::add_extern`].  The
//!    [`VolarIrTarget`] calls [`VolarIrTarget::inline_blocks`] at each
//!    `call_extern` site, inlining both single- and multi-block callees.
//!
//! The resulting [`IRBlocks`] are the **intermediate IR** that captures the
//! full circuit for each function after inlining.

use std::collections::{BTreeMap, BTreeSet};

use volar_compiler::ir::{
    IrBlock, IrExpr, IrExprKind, IrFunction, IrIterChain, IrMatchArm, IrModule, IrStmt, IrStmtKind,
    IterChainSource, IterStep, IterTerminal,
};
use volar_ir::ir::IRBlocks;
use volar_lir_codegen::{
    lower_function_with_registry, mono::MonoEnv, structs::build_struct_registry,
};

use crate::VolarIrTarget;

// ============================================================================
// Public entry point
// ============================================================================

/// Lower all functions in `module` to Volar IR, inlining local function calls.
///
/// Functions are sorted so callees are lowered before callers.  Each function
/// gets a fresh [`VolarIrTarget`] with previously-lowered locals registered
/// as inlinable externs; [`VolarIrTarget::inline_blocks`] handles both
/// single- and multi-block callees.
///
/// Returns `(name, IRBlocks)` pairs in the original module order.
pub fn lower_module_inlining(module: &IrModule<IrFunction>) -> Vec<(String, IRBlocks)> {
    let n = module.functions.len();

    // ---- Build name→index map --------------------------------------------
    let name_to_idx: BTreeMap<&str, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.name.as_str(), i))
        .collect();

    // ---- Build call dependency graph -------------------------------------
    // deps[i] = set of local function indices called (directly) by function i.
    let deps: Vec<BTreeSet<usize>> = module
        .functions
        .iter()
        .map(|f| {
            let mut called = BTreeSet::new();
            collect_calls_block(&f.body, &name_to_idx, &mut called);
            called
        })
        .collect();

    // ---- Topological sort (callee-first DFS post-order) ------------------
    let order = topo_sort(n, &deps);

    // ---- Lower each function, registering it for subsequent callers ------
    // This inlining path does no monomorphization, so a default (empty) env.
    let env = MonoEnv::new("");
    let mut lowered: BTreeMap<String, IRBlocks> = BTreeMap::new();
    for func_idx in order {
        let func = &module.functions[func_idx];
        let mut target = VolarIrTarget::new();

        // Build struct registry: registers struct layouts with the target
        // and returns a typed registry for the lowering pass.
        let registry = build_struct_registry(module, &mut target, &env);

        // Register all previously-lowered local functions as inlinable.
        for (name, blocks) in &lowered {
            target.add_extern(name.clone(), blocks.clone());
        }

        lower_function_with_registry(func, &mut target, &registry, &env, &module.structs);

        for (name, blocks) in target.take_completed() {
            lowered.insert(name, blocks);
        }
    }

    // ---- Return in original module order ---------------------------------
    module
        .functions
        .iter()
        .filter_map(|f| lowered.remove(&f.name).map(|b| (f.name.clone(), b)))
        .collect()
}

// ============================================================================
// Topological sort (DFS post-order = callee before caller)
// ============================================================================

fn topo_sort(n: usize, deps: &[BTreeSet<usize>]) -> Vec<usize> {
    let mut visited = vec![false; n];
    let mut order = Vec::with_capacity(n);
    for i in 0..n {
        if !visited[i] {
            dfs_post(i, deps, &mut visited, &mut order);
        }
    }
    order
}

fn dfs_post(
    node: usize,
    deps: &[BTreeSet<usize>],
    visited: &mut Vec<bool>,
    order: &mut Vec<usize>,
) {
    if visited[node] {
        return;
    }
    visited[node] = true;
    // Visit callees first.
    for &dep in &deps[node] {
        dfs_post(dep, deps, visited, order);
    }
    order.push(node);
}

// ============================================================================
// Call graph scanning — collect called local function names from an IrExpr tree
// ============================================================================

fn collect_calls_block(
    block: &IrBlock,
    name_to_idx: &BTreeMap<&str, usize>,
    out: &mut BTreeSet<usize>,
) {
    for stmt in &block.stmts {
        match &stmt.kind {
            IrStmtKind::Let { init: Some(e), .. } => collect_calls_expr(e, name_to_idx, out),
            IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => collect_calls_expr(e, name_to_idx, out),
            IrStmtKind::Let { init: None, .. } => {}
            _ => {}
        }
    }
    if let Some(e) = &block.expr {
        collect_calls_expr(e, name_to_idx, out);
    }
}

fn collect_calls_expr(
    expr: &IrExpr,
    name_to_idx: &BTreeMap<&str, usize>,
    out: &mut BTreeSet<usize>,
) {
    match &expr.kind {
        // ---- The interesting case: free function call ---------------------
        IrExprKind::Call { func, args } => {
            let callee_name: Option<&str> = match &func.kind {
                IrExprKind::Path { segments, .. } => segments.last().map(|s| s.as_str()),
                IrExprKind::Var(n) => Some(n.as_str()),
                _ => None,
            };
            if let Some(name) = callee_name {
                if let Some(&idx) = name_to_idx.get(name) {
                    out.insert(idx);
                }
            }
            collect_calls_expr(func, name_to_idx, out);
            for a in args {
                collect_calls_expr(a, name_to_idx, out);
            }
        }

        // ---- Recursive cases ---------------------------------------------
        IrExprKind::Binary { left, right, .. } => {
            collect_calls_expr(left, name_to_idx, out);
            collect_calls_expr(right, name_to_idx, out);
        }
        IrExprKind::Unary { expr: inner, .. } | IrExprKind::Cast { expr: inner, .. } => {
            collect_calls_expr(inner, name_to_idx, out);
        }
        IrExprKind::Return(Some(inner))
        | IrExprKind::Break(Some(inner))
        | IrExprKind::Try(inner) => {
            collect_calls_expr(inner, name_to_idx, out);
        }
        IrExprKind::MethodCall { receiver, args, .. } => {
            collect_calls_expr(receiver, name_to_idx, out);
            for a in args {
                collect_calls_expr(a, name_to_idx, out);
            }
        }
        IrExprKind::Field { base, .. } => collect_calls_expr(base, name_to_idx, out),
        IrExprKind::Index { base, index } => {
            collect_calls_expr(base, name_to_idx, out);
            collect_calls_expr(index, name_to_idx, out);
        }
        IrExprKind::StructExpr { fields, rest, .. } => {
            for (_, e) in fields {
                collect_calls_expr(e, name_to_idx, out);
            }
            if let Some(r) = rest {
                collect_calls_expr(r, name_to_idx, out);
            }
        }
        IrExprKind::Tuple(elems) | IrExprKind::Array(elems) | IrExprKind::FixedArray(elems) => {
            for e in elems {
                collect_calls_expr(e, name_to_idx, out);
            }
        }
        IrExprKind::Repeat { elem, len } => {
            collect_calls_expr(elem, name_to_idx, out);
            collect_calls_expr(len, name_to_idx, out);
        }
        IrExprKind::ArrayGenerate { body, .. } => collect_calls_expr(body, name_to_idx, out),
        IrExprKind::RawMap { receiver, body, .. } => {
            collect_calls_expr(receiver, name_to_idx, out);
            collect_calls_expr(body, name_to_idx, out);
        }
        IrExprKind::RawZip {
            left, right, body, ..
        } => {
            collect_calls_expr(left, name_to_idx, out);
            collect_calls_expr(right, name_to_idx, out);
            collect_calls_expr(body, name_to_idx, out);
        }
        IrExprKind::RawFold {
            receiver,
            init,
            body,
            ..
        } => {
            collect_calls_expr(receiver, name_to_idx, out);
            collect_calls_expr(init, name_to_idx, out);
            collect_calls_expr(body, name_to_idx, out);
        }
        IrExprKind::BoundedLoop {
            start, end, body, ..
        } => {
            collect_calls_expr(start, name_to_idx, out);
            collect_calls_expr(end, name_to_idx, out);
            collect_calls_block(body, name_to_idx, out);
        }
        IrExprKind::IterLoop {
            collection, body, ..
        } => {
            collect_calls_expr(collection, name_to_idx, out);
            collect_calls_block(body, name_to_idx, out);
        }
        IrExprKind::WhileLoop { cond, body } => {
            collect_calls_expr(cond, name_to_idx, out);
            collect_calls_block(body, name_to_idx, out);
        }
        IrExprKind::Block(b) => collect_calls_block(b, name_to_idx, out),
        IrExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_calls_expr(cond, name_to_idx, out);
            collect_calls_block(then_branch, name_to_idx, out);
            if let Some(e) = else_branch {
                collect_calls_expr(e, name_to_idx, out);
            }
        }
        IrExprKind::Match { expr: inner, arms } => {
            collect_calls_expr(inner, name_to_idx, out);
            for IrMatchArm { guard, body, .. } in arms {
                if let Some(g) = guard {
                    collect_calls_expr(g, name_to_idx, out);
                }
                collect_calls_expr(body, name_to_idx, out);
            }
        }
        IrExprKind::Closure { body, .. } => collect_calls_expr(body, name_to_idx, out),
        IrExprKind::Assign { left, right } | IrExprKind::AssignOp { left, right, .. } => {
            collect_calls_expr(left, name_to_idx, out);
            collect_calls_expr(right, name_to_idx, out);
        }
        IrExprKind::IterPipeline(chain) => collect_calls_chain(chain, name_to_idx, out),

        IrExprKind::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_calls_expr(s, name_to_idx, out);
            }
            if let Some(e) = end {
                collect_calls_expr(e, name_to_idx, out);
            }
        }
        // Leaf nodes — no sub-expressions containing calls.
        IrExprKind::Lit(_)
        | IrExprKind::Var(_)
        | IrExprKind::Path { .. }
        | IrExprKind::Continue
        | IrExprKind::Unreachable
        | IrExprKind::LengthOf(_)
        | IrExprKind::DefaultValue { .. }
        | IrExprKind::TypenumUsize { .. }
        | IrExprKind::Return(None)
        | IrExprKind::Break(None) => {}
        _ => {}
    }
}

fn collect_calls_chain(
    chain: &IrIterChain,
    name_to_idx: &BTreeMap<&str, usize>,
    out: &mut BTreeSet<usize>,
) {
    // Source
    match &chain.source {
        IterChainSource::Method { collection, .. } => {
            collect_calls_expr(collection, name_to_idx, out)
        }
        IterChainSource::Range { start, end, .. } => {
            collect_calls_expr(start, name_to_idx, out);
            collect_calls_expr(end, name_to_idx, out);
        }
        IterChainSource::Zip { left, right } => {
            collect_calls_chain(left, name_to_idx, out);
            collect_calls_chain(right, name_to_idx, out);
        }
    }
    // Steps
    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => collect_calls_expr(body, name_to_idx, out),
            IterStep::Take { count } | IterStep::Skip { count } => {
                collect_calls_expr(count, name_to_idx, out)
            }
            IterStep::Chain { other } => collect_calls_chain(other, name_to_idx, out),
            IterStep::Enumerate => {}
        }
    }
    // Terminal
    match &chain.terminal {
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => {}
        IterTerminal::Fold { init, body, .. } => {
            collect_calls_expr(init, name_to_idx, out);
            collect_calls_expr(body, name_to_idx, out);
        }
    }
}
