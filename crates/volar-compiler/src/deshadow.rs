//! De-shadowing analysis pass for IR blocks.
//!
//! Rust allows `let x = f(x);` to shadow an outer `x`, reading the old value
//! before binding the new one.  TypeScript's `const x = f(x);` is a TDZ
//! (Temporal Dead Zone) violation — the new `x` is in scope on the RHS.
//!
//! This pass walks an `IrBlock`, tracks which variable names are currently in
//! scope, and renames any `Let` binding whose name would shadow an in-scope
//! name **and** whose initializer references that name.  The rename applies to
//! the binding **and** to all subsequent references in the enclosing block, but
//! not to references in the initializer (which must still see the old name).
//!
//! The pass is idempotent and can be applied to any `IrBlock`.

use crate::ir::{
    IrBlock, IrClosureParam, IrExpr, IrIterChain, IrPattern, IrStmt,
    IterChainSource, IterStep, IterTerminal,
};

#[cfg(feature = "std")]
use std::{
    boxed::Box,
    collections::{HashMap, HashSet},
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
#[cfg(not(feature = "std"))]
use hashbrown::{HashMap, HashSet};

// ── public API ──────────────────────────────────────────────────────────────

/// Remove self-referencing shadows from `block`.
///
/// `outer_names` is the set of names already in scope when `block` begins
/// (e.g. function parameters, field-binding lets).
pub fn deshadow_block(block: &mut IrBlock, outer_names: &HashSet<String>) {
    let mut scope = outer_names.clone();
    deshadow_stmts(&mut block.stmts, &mut block.expr, &mut scope);
}

// ── helpers ─────────────────────────────────────────────────────────────────

/// Collect all identifier names bound by a pattern.
pub fn pattern_names(pat: &IrPattern) -> Vec<String> {
    let mut out = Vec::new();
    collect_pattern_names(pat, &mut out);
    out
}

fn collect_pattern_names(pat: &IrPattern, out: &mut Vec<String>) {
    match pat {
        IrPattern::Ident { name, subpat, .. } => {
            out.push(name.clone());
            if let Some(sp) = subpat {
                collect_pattern_names(sp, out);
            }
        }
        IrPattern::Tuple(elems) | IrPattern::Slice(elems) | IrPattern::Or(elems) => {
            for p in elems {
                collect_pattern_names(p, out);
            }
        }
        IrPattern::Struct { fields, .. } => {
            for (_, p) in fields {
                collect_pattern_names(p, out);
            }
        }
        IrPattern::TupleStruct { elems, .. } => {
            for p in elems {
                collect_pattern_names(p, out);
            }
        }
        IrPattern::Ref { pat, .. } => collect_pattern_names(pat, out),
        IrPattern::Wild | IrPattern::Lit(_) | IrPattern::Rest => {}
    }
}

/// Does `expr` contain a free reference to any name in `names`?
fn expr_refs_any(expr: &IrExpr, names: &HashSet<String>) -> bool {
    if names.is_empty() {
        return false;
    }
    match expr {
        IrExpr::Var(v) => names.contains(v.as_str()),
        IrExpr::Binary { left, right, .. }
        | IrExpr::Assign { left, right }
        | IrExpr::AssignOp { left, right, .. } => {
            expr_refs_any(left, names) || expr_refs_any(right, names)
        }
        IrExpr::Unary { expr: e, .. }
        | IrExpr::Cast { expr: e, .. }
        | IrExpr::Try(e)
        | IrExpr::Return(Some(e))
        | IrExpr::Break(Some(e)) => expr_refs_any(e, names),
        IrExpr::Field { base, .. } => expr_refs_any(base, names),
        IrExpr::Index { base, index } => {
            expr_refs_any(base, names) || expr_refs_any(index, names)
        }
        IrExpr::Call { func, args } => {
            expr_refs_any(func, names) || args.iter().any(|a| expr_refs_any(a, names))
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            expr_refs_any(receiver, names) || args.iter().any(|a| expr_refs_any(a, names))
        }
        IrExpr::Block(b) => block_refs_any(b, names),
        IrExpr::If { cond, then_branch, else_branch } => {
            expr_refs_any(cond, names)
                || block_refs_any(then_branch, names)
                || else_branch.as_ref().map_or(false, |e| expr_refs_any(e, names))
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            fields.iter().any(|(_, e)| expr_refs_any(e, names))
                || rest.as_ref().map_or(false, |r| expr_refs_any(r, names))
        }
        IrExpr::Array(elems) | IrExpr::Tuple(elems) => {
            elems.iter().any(|e| expr_refs_any(e, names))
        }
        IrExpr::Closure { body, .. } => expr_refs_any(body, names),
        IrExpr::Range { start, end, .. } => {
            start.as_ref().map_or(false, |s| expr_refs_any(s, names))
                || end.as_ref().map_or(false, |e| expr_refs_any(e, names))
        }
        IrExpr::IterPipeline(chain) => iter_chain_refs_any(chain, names),
        IrExpr::ArrayGenerate { body, .. } => expr_refs_any(body, names),
        IrExpr::BoundedLoop { start, end, body, .. } => {
            expr_refs_any(start, names)
                || expr_refs_any(end, names)
                || block_refs_any(body, names)
        }
        IrExpr::IterLoop { collection, body, .. } => {
            expr_refs_any(collection, names) || block_refs_any(body, names)
        }
        IrExpr::Repeat { elem, len } => {
            expr_refs_any(elem, names) || expr_refs_any(len, names)
        }
        IrExpr::RawMap { receiver, body, .. } => {
            expr_refs_any(receiver, names) || expr_refs_any(body, names)
        }
        IrExpr::RawZip { left, right, body, .. } => {
            expr_refs_any(left, names) || expr_refs_any(right, names) || expr_refs_any(body, names)
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            expr_refs_any(receiver, names)
                || expr_refs_any(init, names)
                || expr_refs_any(body, names)
        }
        IrExpr::Match { expr: scrutinee, arms } => {
            expr_refs_any(scrutinee, names)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().map_or(false, |g| expr_refs_any(g, names))
                        || expr_refs_any(&arm.body, names)
                })
        }
        IrExpr::Lit(_)
        | IrExpr::Path { .. }
        | IrExpr::DefaultValue { .. }
        | IrExpr::ArrayDefault { .. }
        | IrExpr::LengthOf(_)
        | IrExpr::TypenumUsize { .. }
        | IrExpr::Return(None)
        | IrExpr::Break(None)
        | IrExpr::Continue | IrExpr::Unreachable => false,
    }
}

fn block_refs_any(block: &IrBlock, names: &HashSet<String>) -> bool {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { init, .. } => {
                if let Some(e) = init {
                    if expr_refs_any(e, names) {
                        return true;
                    }
                }
            }
            IrStmt::Semi(e) | IrStmt::Expr(e) => {
                if expr_refs_any(e, names) {
                    return true;
                }
            }
        }
    }
    if let Some(e) = &block.expr {
        if expr_refs_any(e, names) {
            return true;
        }
    }
    false
}

fn iter_chain_refs_any(chain: &IrIterChain, names: &HashSet<String>) -> bool {
    let source_refs = match &chain.source {
        IterChainSource::Method { collection, .. } => expr_refs_any(collection, names),
        IterChainSource::Range { start, end, .. } => {
            expr_refs_any(start, names) || expr_refs_any(end, names)
        }
        IterChainSource::Zip { left, right } => {
            iter_chain_refs_any(left, names) || iter_chain_refs_any(right, names)
        }
    };
    if source_refs {
        return true;
    }

    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => {
                if expr_refs_any(body, names) {
                    return true;
                }
            }
            IterStep::Enumerate | IterStep::Take { .. } | IterStep::Skip { .. }
            | IterStep::Chain { .. } => {}
        }
    }

    match &chain.terminal {
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => false,
        IterTerminal::Fold { init, body, .. } => {
            expr_refs_any(init, names) || expr_refs_any(body, names)
        }
    }
}

// ── rename infrastructure ───────────────────────────────────────────────────

/// Generate a fresh name that is not in `taken`.
/// Strips any existing `_N` suffix to keep names readable (e.g. `x_1` → `x_2`
/// instead of `x_1_1`).
fn fresh_name(base: &str, taken: &HashSet<String>) -> String {
    // Strip trailing _N suffix if present
    let root = if let Some(idx) = base.rfind('_') {
        if base[idx + 1..].chars().all(|c| c.is_ascii_digit()) {
            &base[..idx]
        } else {
            base
        }
    } else {
        base
    };
    for i in 1u32.. {
        let candidate = format!("{}_{}", root, i);
        if !taken.contains(&candidate) {
            return candidate;
        }
    }
    unreachable!()
}

/// Rename a pattern binding: if any `Ident` in `pat` has `name == old`,
/// change it to `new_name`.
fn rename_in_pattern(pat: &mut IrPattern, old: &str, new_name: &str) {
    match pat {
        IrPattern::Ident { name, subpat, .. } => {
            if name == old {
                *name = new_name.to_string();
            }
            if let Some(sp) = subpat {
                rename_in_pattern(sp, old, new_name);
            }
        }
        IrPattern::Tuple(elems) | IrPattern::Slice(elems) | IrPattern::Or(elems) => {
            for p in elems {
                rename_in_pattern(p, old, new_name);
            }
        }
        IrPattern::Struct { fields, .. } => {
            for (_, p) in fields {
                rename_in_pattern(p, old, new_name);
            }
        }
        IrPattern::TupleStruct { elems, .. } => {
            for p in elems {
                rename_in_pattern(p, old, new_name);
            }
        }
        IrPattern::Ref { pat, .. } => rename_in_pattern(pat, old, new_name),
        IrPattern::Wild | IrPattern::Lit(_) | IrPattern::Rest => {}
    }
}

fn rename_var_in_block(block: &mut IrBlock, old: &str, new_name: &str) {
    for stmt in &mut block.stmts {
        rename_var_in_stmt(stmt, old, new_name);
    }
    if let Some(e) = &mut block.expr {
        rename_var_in_expr(e, old, new_name);
    }
}

fn rename_var_in_stmt(stmt: &mut IrStmt, old: &str, new_name: &str) {
    match stmt {
        IrStmt::Let { pattern, init, .. } => {
            rename_in_pattern(pattern, old, new_name);
            if let Some(e) = init {
                rename_var_in_expr(e, old, new_name);
            }
        }
        IrStmt::Semi(e) | IrStmt::Expr(e) => {
            rename_var_in_expr(e, old, new_name);
        }
    }
}

fn rename_var_in_expr(expr: &mut IrExpr, old: &str, new_name: &str) {
    match expr {
        IrExpr::Var(v) => {
            if v == old {
                *v = new_name.to_string();
            }
        }
        IrExpr::Binary { left, right, .. }
        | IrExpr::Assign { left, right }
        | IrExpr::AssignOp { left, right, .. } => {
            rename_var_in_expr(left, old, new_name);
            rename_var_in_expr(right, old, new_name);
        }
        IrExpr::Unary { expr: e, .. }
        | IrExpr::Cast { expr: e, .. }
        | IrExpr::Try(e)
        | IrExpr::Return(Some(e))
        | IrExpr::Break(Some(e)) => rename_var_in_expr(e, old, new_name),
        IrExpr::Field { base, .. } => rename_var_in_expr(base, old, new_name),
        IrExpr::Index { base, index } => {
            rename_var_in_expr(base, old, new_name);
            rename_var_in_expr(index, old, new_name);
        }
        IrExpr::Call { func, args } => {
            rename_var_in_expr(func, old, new_name);
            for a in args {
                rename_var_in_expr(a, old, new_name);
            }
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            rename_var_in_expr(receiver, old, new_name);
            for a in args {
                rename_var_in_expr(a, old, new_name);
            }
        }
        IrExpr::Block(b) => rename_var_in_block(b, old, new_name),
        IrExpr::If { cond, then_branch, else_branch } => {
            rename_var_in_expr(cond, old, new_name);
            rename_var_in_block(then_branch, old, new_name);
            if let Some(eb) = else_branch {
                rename_var_in_expr(eb, old, new_name);
            }
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, e) in fields {
                rename_var_in_expr(e, old, new_name);
            }
            if let Some(r) = rest {
                rename_var_in_expr(r, old, new_name);
            }
        }
        IrExpr::Array(elems) | IrExpr::Tuple(elems) => {
            for e in elems {
                rename_var_in_expr(e, old, new_name);
            }
        }
        IrExpr::Closure { params, body, .. } => {
            let closure_binds = params
                .iter()
                .flat_map(|p| pattern_names(&p.pattern))
                .any(|n| n == old);
            if !closure_binds {
                rename_var_in_expr(body, old, new_name);
            }
        }
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start {
                rename_var_in_expr(s, old, new_name);
            }
            if let Some(e) = end {
                rename_var_in_expr(e, old, new_name);
            }
        }
        IrExpr::IterPipeline(chain) => rename_var_in_iter_chain(chain, old, new_name),
        IrExpr::ArrayGenerate { index_var, body, .. } => {
            if index_var != old {
                rename_var_in_expr(body, old, new_name);
            }
        }
        IrExpr::BoundedLoop { var, start, end, body, .. } => {
            rename_var_in_expr(start, old, new_name);
            rename_var_in_expr(end, old, new_name);
            if var != old {
                rename_var_in_block(body, old, new_name);
            }
        }
        IrExpr::IterLoop { pattern, collection, body } => {
            rename_var_in_expr(collection, old, new_name);
            let loop_binds = pattern_names(pattern).iter().any(|n| n == old);
            if !loop_binds {
                rename_var_in_block(body, old, new_name);
            }
        }
        IrExpr::Repeat { elem, len } => {
            rename_var_in_expr(elem, old, new_name);
            rename_var_in_expr(len, old, new_name);
        }
        IrExpr::RawMap { receiver, elem_var, body } => {
            rename_var_in_expr(receiver, old, new_name);
            let rebinds = pattern_names(elem_var).iter().any(|n| n == old);
            if !rebinds {
                rename_var_in_expr(body, old, new_name);
            }
        }
        IrExpr::RawZip { left, right, left_var, right_var, body } => {
            rename_var_in_expr(left, old, new_name);
            rename_var_in_expr(right, old, new_name);
            let l_binds = pattern_names(left_var);
            let r_binds = pattern_names(right_var);
            let rebinds = l_binds.iter().chain(r_binds.iter()).any(|n| n == old);
            if !rebinds {
                rename_var_in_expr(body, old, new_name);
            }
        }
        IrExpr::RawFold { receiver, init, acc_var, elem_var, body } => {
            rename_var_in_expr(receiver, old, new_name);
            rename_var_in_expr(init, old, new_name);
            let a_binds = pattern_names(acc_var);
            let e_binds = pattern_names(elem_var);
            let rebinds = a_binds.iter().chain(e_binds.iter()).any(|n| n == old);
            if !rebinds {
                rename_var_in_expr(body, old, new_name);
            }
        }
        IrExpr::Match { expr: scrutinee, arms } => {
            rename_var_in_expr(scrutinee, old, new_name);
            for arm in arms {
                let arm_binds = pattern_names(&arm.pattern).iter().any(|n| n == old);
                if !arm_binds {
                    if let Some(g) = &mut arm.guard {
                        rename_var_in_expr(g, old, new_name);
                    }
                    rename_var_in_expr(&mut arm.body, old, new_name);
                }
            }
        }
        IrExpr::Lit(_)
        | IrExpr::Path { .. }
        | IrExpr::DefaultValue { .. }
        | IrExpr::ArrayDefault { .. }
        | IrExpr::LengthOf(_)
        | IrExpr::TypenumUsize { .. }
        | IrExpr::Return(None)
        | IrExpr::Break(None)
        | IrExpr::Continue | IrExpr::Unreachable => {}
    }
}

fn rename_var_in_iter_chain(chain: &mut IrIterChain, old: &str, new_name: &str) {
    match &mut chain.source {
        IterChainSource::Method { collection, .. } => {
            rename_var_in_expr(collection, old, new_name);
        }
        IterChainSource::Range { start, end, .. } => {
            rename_var_in_expr(start, old, new_name);
            rename_var_in_expr(end, old, new_name);
        }
        IterChainSource::Zip { left, right } => {
            rename_var_in_iter_chain(left, old, new_name);
            rename_var_in_iter_chain(right, old, new_name);
        }
    }
    for step in &mut chain.steps {
        match step {
            IterStep::Map { var, body }
            | IterStep::Filter { var, body }
            | IterStep::FilterMap { var, body }
            | IterStep::FlatMap { var, body } => {
                let rebinds = pattern_names(var).iter().any(|n| n == old);
                if !rebinds {
                    rename_var_in_expr(body, old, new_name);
                }
            }
            IterStep::Enumerate | IterStep::Take { .. } | IterStep::Skip { .. }
            | IterStep::Chain { .. } => {}
        }
    }
    match &mut chain.terminal {
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => {}
        IterTerminal::Fold { acc_var, elem_var, init, body } => {
            rename_var_in_expr(init, old, new_name);
            let a_binds = pattern_names(acc_var);
            let e_binds = pattern_names(elem_var);
            let rebinds = a_binds.iter().chain(e_binds.iter()).any(|n| n == old);
            if !rebinds {
                rename_var_in_expr(body, old, new_name);
            }
        }
    }
}

// ── core deshadowing logic ──────────────────────────────────────────────────

/// Walk a list of statements, tracking scope.  When we encounter a `Let`
/// binding that shadows an in-scope name and whose initializer references
/// that name, we rename the new binding to a fresh name and rewrite all
/// subsequent statements/expressions.
fn deshadow_stmts(
    stmts: &mut Vec<IrStmt>,
    tail_expr: &mut Option<Box<IrExpr>>,
    scope: &mut HashSet<String>,
) {
    let mut i = 0;
    while i < stmts.len() {
        // First, recurse into sub-expressions for nested blocks/closures
        match &mut stmts[i] {
            IrStmt::Semi(e) | IrStmt::Expr(e) => {
                deshadow_expr(e, scope);
            }
            IrStmt::Let { init, .. } => {
                if let Some(e) = init {
                    deshadow_expr(e, scope);
                }
            }
        }

        // Now check for shadowing in Let bindings.
        // We collect renames in a separate pass to avoid holding borrows
        // across the rename step.
        let renames: Vec<(String, String)> = if let IrStmt::Let { pattern, init, .. } = &stmts[i] {
            let bound = pattern_names(pattern);
            let mut out = Vec::new();
            for name in &bound {
                if scope.contains(name) {
                    let refs_name = if let Some(e) = init.as_ref() {
                        let check_set: HashSet<String> =
                            core::iter::once(name.clone()).collect();
                        expr_refs_any(e, &check_set)
                    } else {
                        false
                    };
                    if refs_name {
                        let fresh = fresh_name(name, scope);
                        out.push((name.clone(), fresh));
                    }
                }
            }
            out
        } else {
            Vec::new()
        };

        if !renames.is_empty() {
            // Rename the pattern bindings in stmts[i]
            if let IrStmt::Let { pattern, .. } = &mut stmts[i] {
                for (old, new_name) in &renames {
                    rename_in_pattern(pattern, old, new_name);
                }
            }
            // Rename in all subsequent statements (not the current init!)
            for (old, new_name) in &renames {
                for j in (i + 1)..stmts.len() {
                    rename_var_in_stmt(&mut stmts[j], old, new_name);
                }
                // Also rename in the tail expression
                if let Some(te) = tail_expr.as_mut() {
                    rename_var_in_expr(te, old, new_name);
                }
            }
            // Add the fresh names to scope
            for (_, new_name) in &renames {
                scope.insert(new_name.clone());
            }
        }

        // Add all bound names (after possible rename) to scope
        if let IrStmt::Let { pattern, .. } = &stmts[i] {
            let bound_after = pattern_names(pattern);
            for name in bound_after {
                scope.insert(name);
            }
        }

        i += 1;
    }

    // Finally, recurse into the tail expression for nested blocks
    if let Some(te) = tail_expr.as_mut() {
        deshadow_expr(te, scope);
    }
}

/// Recurse into an expression to deshadow any nested blocks.
fn deshadow_expr(expr: &mut IrExpr, scope: &mut HashSet<String>) {
    match expr {
        IrExpr::Block(b) => {
            let inner = scope.clone();
            deshadow_block(b, &inner);
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            deshadow_expr(cond, scope);
            let then_scope = scope.clone();
            deshadow_block(then_branch, &then_scope);
            if let Some(eb) = else_branch {
                deshadow_expr(eb, scope);
            }
        }
        IrExpr::BoundedLoop { var, start, end, body, .. } => {
            deshadow_expr(start, scope);
            deshadow_expr(end, scope);
            let mut inner = scope.clone();
            inner.insert(var.clone());
            deshadow_block(body, &inner);
        }
        IrExpr::IterLoop { pattern, collection, body } => {
            deshadow_expr(collection, scope);
            let mut inner = scope.clone();
            for n in pattern_names(pattern) {
                inner.insert(n);
            }
            deshadow_block(body, &inner);
        }
        IrExpr::Closure { params, body, .. } => {
            let mut inner = scope.clone();
            for p in params.iter() {
                for n in pattern_names(&p.pattern) {
                    inner.insert(n);
                }
            }
            deshadow_expr(body, &mut inner);
        }
        IrExpr::Binary { left, right, .. }
        | IrExpr::Assign { left, right }
        | IrExpr::AssignOp { left, right, .. } => {
            deshadow_expr(left, scope);
            deshadow_expr(right, scope);
        }
        IrExpr::Unary { expr: e, .. }
        | IrExpr::Cast { expr: e, .. }
        | IrExpr::Try(e)
        | IrExpr::Return(Some(e))
        | IrExpr::Break(Some(e)) => deshadow_expr(e, scope),
        IrExpr::Field { base, .. } => deshadow_expr(base, scope),
        IrExpr::Index { base, index } => {
            deshadow_expr(base, scope);
            deshadow_expr(index, scope);
        }
        IrExpr::Call { func, args } => {
            deshadow_expr(func, scope);
            for a in args {
                deshadow_expr(a, scope);
            }
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            deshadow_expr(receiver, scope);
            for a in args {
                deshadow_expr(a, scope);
            }
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, e) in fields {
                deshadow_expr(e, scope);
            }
            if let Some(r) = rest {
                deshadow_expr(r, scope);
            }
        }
        IrExpr::Array(elems) | IrExpr::Tuple(elems) => {
            for e in elems {
                deshadow_expr(e, scope);
            }
        }
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start {
                deshadow_expr(s, scope);
            }
            if let Some(e) = end {
                deshadow_expr(e, scope);
            }
        }
        IrExpr::IterPipeline(chain) => deshadow_iter_chain(chain, scope),
        IrExpr::ArrayGenerate { index_var, body, .. } => {
            let mut inner = scope.clone();
            inner.insert(index_var.clone());
            deshadow_expr(body, &mut inner);
        }
        IrExpr::Repeat { elem, len } => {
            deshadow_expr(elem, scope);
            deshadow_expr(len, scope);
        }
        IrExpr::RawMap { receiver, elem_var, body } => {
            deshadow_expr(receiver, scope);
            let mut inner = scope.clone();
            for n in pattern_names(elem_var) {
                inner.insert(n);
            }
            deshadow_expr(body, &mut inner);
        }
        IrExpr::RawZip { left, right, left_var, right_var, body } => {
            deshadow_expr(left, scope);
            deshadow_expr(right, scope);
            let mut inner = scope.clone();
            for n in pattern_names(left_var) {
                inner.insert(n);
            }
            for n in pattern_names(right_var) {
                inner.insert(n);
            }
            deshadow_expr(body, &mut inner);
        }
        IrExpr::RawFold { receiver, init, acc_var, elem_var, body } => {
            deshadow_expr(receiver, scope);
            deshadow_expr(init, scope);
            let mut inner = scope.clone();
            for n in pattern_names(acc_var) {
                inner.insert(n);
            }
            for n in pattern_names(elem_var) {
                inner.insert(n);
            }
            deshadow_expr(body, &mut inner);
        }
        IrExpr::Match { expr: scrutinee, arms } => {
            deshadow_expr(scrutinee, scope);
            for arm in arms {
                let mut inner = scope.clone();
                for n in pattern_names(&arm.pattern) {
                    inner.insert(n);
                }
                if let Some(g) = &mut arm.guard {
                    deshadow_expr(g, &mut inner);
                }
                deshadow_expr(&mut arm.body, &mut inner);
            }
        }
        IrExpr::Var(_)
        | IrExpr::Lit(_)
        | IrExpr::Path { .. }
        | IrExpr::DefaultValue { .. }
        | IrExpr::ArrayDefault { .. }
        | IrExpr::LengthOf(_)
        | IrExpr::TypenumUsize { .. }
        | IrExpr::Return(None)
        | IrExpr::Break(None)
        | IrExpr::Continue | IrExpr::Unreachable => {}
    }
}

fn deshadow_iter_chain(chain: &mut IrIterChain, scope: &mut HashSet<String>) {
    match &mut chain.source {
        IterChainSource::Method { collection, .. } => deshadow_expr(collection, scope),
        IterChainSource::Range { start, end, .. } => {
            deshadow_expr(start, scope);
            deshadow_expr(end, scope);
        }
        IterChainSource::Zip { left, right } => {
            deshadow_iter_chain(left, scope);
            deshadow_iter_chain(right, scope);
        }
    }
    for step in &mut chain.steps {
        match step {
            IterStep::Map { var, body }
            | IterStep::Filter { var, body }
            | IterStep::FilterMap { var, body }
            | IterStep::FlatMap { var, body } => {
                let mut inner = scope.clone();
                for n in pattern_names(var) {
                    inner.insert(n);
                }
                deshadow_expr(body, &mut inner);
            }
            IterStep::Enumerate | IterStep::Take { .. } | IterStep::Skip { .. }
            | IterStep::Chain { .. } => {}
        }
    }
    match &mut chain.terminal {
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => {}
        IterTerminal::Fold { acc_var, elem_var, init, body } => {
            deshadow_expr(init, scope);
            let mut inner = scope.clone();
            for n in pattern_names(acc_var) {
                inner.insert(n);
            }
            for n in pattern_names(elem_var) {
                inner.insert(n);
            }
            deshadow_expr(body, &mut inner);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{SpecBinOp, IrLit};

    fn var(name: &str) -> IrExpr {
        IrExpr::Var(name.to_string())
    }

    fn lit_int(n: i128) -> IrExpr {
        IrExpr::Lit(IrLit::Int(n))
    }

    fn let_stmt(name: &str, init: IrExpr) -> IrStmt {
        IrStmt::Let {
            pattern: IrPattern::ident(name),
            ty: None,
            init: Some(init),
        }
    }

    #[test]
    fn simple_shadow_renamed() {
        // let x = 1;
        // let x = x + 1;  // shadows: should become let x_1 = x + 1
        let mut block = IrBlock {
            stmts: vec![
                let_stmt("x", lit_int(1)),
                let_stmt(
                    "x",
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(var("x")),
                        right: Box::new(lit_int(1)),
                    },
                ),
                IrStmt::Semi(var("x")), // should reference x_1
            ],
            expr: None,
        };
        let outer = HashSet::new();
        deshadow_block(&mut block, &outer);

        // The second let should be renamed to x_1
        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, init, .. } = &block.stmts[1] {
            assert_eq!(name, "x_1");
            // The init should still reference the OLD x (not renamed)
            if let Some(IrExpr::Binary { left, .. }) = init {
                assert_eq!(**left, var("x"));
            } else {
                panic!("expected binary init");
            }
        } else {
            panic!("expected let");
        }

        // The subsequent Semi should reference x_1
        assert_eq!(block.stmts[2], IrStmt::Semi(var("x_1")));
    }

    #[test]
    fn no_shadow_no_rename() {
        // let x = 1;
        // let y = x;  // no shadow
        let mut block = IrBlock {
            stmts: vec![let_stmt("x", lit_int(1)), let_stmt("y", var("x"))],
            expr: None,
        };
        let outer = HashSet::new();
        deshadow_block(&mut block, &outer);

        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, .. } = &block.stmts[1] {
            assert_eq!(name, "y"); // unchanged
        } else {
            panic!("expected let");
        }
    }

    #[test]
    fn shadow_without_ref_no_rename() {
        // let x = 1;
        // let x = 2;  // shadows but doesn't reference old x
        let mut block = IrBlock {
            stmts: vec![let_stmt("x", lit_int(1)), let_stmt("x", lit_int(2))],
            expr: None,
        };
        let outer = HashSet::new();
        deshadow_block(&mut block, &outer);

        // No rename needed — the init doesn't reference `x`
        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, .. } = &block.stmts[1] {
            assert_eq!(name, "x"); // unchanged
        } else {
            panic!("expected let");
        }
    }

    #[test]
    fn param_shadow() {
        // Function param `bad` in scope, then let bad = clone(bad)
        let mut block = IrBlock {
            stmts: vec![
                let_stmt(
                    "bad",
                    IrExpr::Call {
                        func: Box::new(var("structuredClone")),
                        args: vec![var("bad")],
                    },
                ),
                IrStmt::Semi(var("bad")), // should become bad_1
            ],
            expr: None,
        };
        let outer: HashSet<String> = ["bad".to_string()].into_iter().collect();
        deshadow_block(&mut block, &outer);

        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, init, .. } = &block.stmts[0] {
            assert_eq!(name, "bad_1");
            // init should still reference `bad` (the param)
            if let Some(IrExpr::Call { args, .. }) = init {
                assert_eq!(args[0], var("bad"));
            }
        } else {
            panic!("expected let");
        }
        assert_eq!(block.stmts[1], IrStmt::Semi(var("bad_1")));
    }

    #[test]
    fn nested_loop_shadow() {
        // Outer: map lambda introduces `l`
        // Inner: const l = f(l, s, m)  inside for body
        let mut block = IrBlock {
            stmts: vec![
                let_stmt(
                    "l",
                    IrExpr::Call {
                        func: Box::new(var("fieldAdd")),
                        args: vec![
                            IrExpr::Call {
                                func: Box::new(var("fieldMul")),
                                args: vec![var("l"), var("s")],
                            },
                            var("m"),
                        ],
                    },
                ),
                IrStmt::Semi(var("l")), // should become l_1
            ],
            expr: None,
        };
        let outer: HashSet<String> = ["l".to_string(), "s".to_string(), "m".to_string()]
            .into_iter()
            .collect();
        deshadow_block(&mut block, &outer);

        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, init, .. } = &block.stmts[0] {
            assert_eq!(name, "l_1");
            // Init references old `l`
            if let Some(IrExpr::Call { args, .. }) = init {
                if let IrExpr::Call { args: inner_args, .. } = &args[0] {
                    assert_eq!(inner_args[0], var("l"));
                }
            }
        }
        assert_eq!(block.stmts[1], IrStmt::Semi(var("l_1")));
    }

    #[test]
    fn implicit_return_renamed() {
        // let x = 1;
        // let x = x + 1;
        // (expr: x)  — implicit return
        let mut block = IrBlock {
            stmts: vec![
                let_stmt("x", lit_int(1)),
                let_stmt(
                    "x",
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(var("x")),
                        right: Box::new(lit_int(1)),
                    },
                ),
            ],
            expr: Some(Box::new(var("x"))),
        };
        let outer = HashSet::new();
        deshadow_block(&mut block, &outer);

        // The implicit return should reference x_1
        assert_eq!(*block.expr.unwrap(), var("x_1"));
    }

    #[test]
    fn triple_shadow() {
        // let x = 1;
        // let x = x + 1;  → x_1  (init refs x, subsequent become x_1)
        // let x = x + 1;  → the 2nd rename turned this into `let x_1 = x_1 + 1`
        //                     which self-shadows x_1, so it becomes x_2
        let mut block = IrBlock {
            stmts: vec![
                let_stmt("x", lit_int(1)),
                let_stmt(
                    "x",
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(var("x")),
                        right: Box::new(lit_int(1)),
                    },
                ),
                let_stmt(
                    "x",
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(var("x")),
                        right: Box::new(lit_int(1)),
                    },
                ),
                IrStmt::Semi(var("x")),
            ],
            expr: None,
        };
        let outer = HashSet::new();
        deshadow_block(&mut block, &outer);

        // Second let: x → x_1
        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, .. } = &block.stmts[1] {
            assert_eq!(name, "x_1");
        }
        // Third let was renamed x→x_1 by the first pass, so it's now
        // `let x_1 = x_1 + 1` which shadows x_1 → becomes x_2
        if let IrStmt::Let { pattern: IrPattern::Ident { name, .. }, .. } = &block.stmts[2] {
            assert_eq!(name, "x_2");
        }
        assert_eq!(block.stmts[3], IrStmt::Semi(var("x_2")));
    }
}
