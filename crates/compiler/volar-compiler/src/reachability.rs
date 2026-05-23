//! Call-graph reachability analysis for lazy/seeded backend emission.
//!
//! Given a module and a set of seed function names, computes the transitive
//! closure of all functions reachable from those seeds via direct calls.

#[cfg(feature = "std")]
use std::collections::{HashMap, HashSet, VecDeque};
#[cfg(feature = "std")]
use std::{string::String, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::collections::{BTreeMap as HashMap, BTreeSet as HashSet, VecDeque};
#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

use crate::ir::{IrBlock, IrExpr, IrFunction, IrModule, IrStmt, MethodKind};
use crate::printer_ts::is_namespace_prefix;

/// Compute the set of function names transitively reachable from `seeds`
/// within `module`.  Returns a `HashSet<String>` of bare function names.
/// If `seeds` is empty, returns an empty set.
pub fn compute_reachable(
    module: &IrModule<IrFunction>,
    seeds: &[&str],
) -> HashSet<String> {
    if seeds.is_empty() {
        return HashSet::new();
    }

    let name_to_idx: HashMap<&str, usize> = module.functions.iter()
        .enumerate()
        .map(|(i, f)| (f.name.as_str(), i))
        .collect();

    let callees: Vec<Vec<usize>> = module.functions.iter()
        .map(|f| collect_callees_fn(f, &name_to_idx))
        .collect();

    let mut visited: HashSet<usize> = HashSet::new();
    let mut queue: VecDeque<usize> = VecDeque::new();

    for &seed in seeds {
        if let Some(&idx) = name_to_idx.get(seed) {
            if visited.insert(idx) {
                queue.push_back(idx);
            }
        }
    }

    while let Some(idx) = queue.pop_front() {
        for &callee_idx in &callees[idx] {
            if visited.insert(callee_idx) {
                queue.push_back(callee_idx);
            }
        }
    }

    visited.into_iter()
        .map(|i| module.functions[i].name.clone())
        .collect()
}

// ---------------------------------------------------------------------------

fn collect_callees_fn(func: &IrFunction, idx_map: &HashMap<&str, usize>) -> Vec<usize> {
    let mut out = Vec::new();
    collect_block(&func.body, idx_map, &mut out);
    out
}

fn collect_block(block: &IrBlock, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { init: Some(e), .. } | IrStmt::Semi(e) | IrStmt::Expr(e) => {
                collect_expr(e, idx_map, out);
            }
            _ => {}
        }
    }
    if let Some(tail) = &block.expr {
        collect_expr(tail, idx_map, out);
    }
}

fn collect_expr(expr: &IrExpr, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    match expr {
        IrExpr::Call { func, args } => {
            match func.as_ref() {
                IrExpr::Var(name) => add_callee(name, idx_map, out),
                IrExpr::Path { segments, .. } => {
                    let last = segments.iter()
                        .map(|s| s.as_str())
                        .filter(|s| !is_namespace_prefix(s))
                        .last();
                    if let Some(name) = last {
                        add_callee(name, idx_map, out);
                    }
                }
                other => collect_expr(other, idx_map, out),
            }
            for a in args { collect_expr(a, idx_map, out); }
        }
        IrExpr::MethodCall { receiver, method, args, .. } => {
            collect_expr(receiver, idx_map, out);
            if let MethodKind::Other(name) = method {
                add_callee(name, idx_map, out);
            }
            for a in args { collect_expr(a, idx_map, out); }
        }
        IrExpr::Block(b) => collect_block(b, idx_map, out),
        IrExpr::If { cond, then_branch, else_branch } => {
            collect_expr(cond, idx_map, out);
            collect_block(then_branch, idx_map, out);
            if let Some(eb) = else_branch { collect_expr(eb, idx_map, out); }
        }
        IrExpr::Match { expr: e, arms } => {
            collect_expr(e, idx_map, out);
            for arm in arms { collect_expr(&arm.body, idx_map, out); }
        }
        IrExpr::BoundedLoop { start, end, body, .. } => {
            collect_expr(start, idx_map, out);
            collect_expr(end, idx_map, out);
            collect_block(body, idx_map, out);
        }
        IrExpr::IterLoop { collection, body, .. } => {
            collect_expr(collection, idx_map, out);
            collect_block(body, idx_map, out);
        }
        IrExpr::WhileLoop { cond, body } => {
            collect_expr(cond, idx_map, out);
            collect_block(body, idx_map, out);
        }
        IrExpr::Return(Some(e)) | IrExpr::Break(Some(e)) => collect_expr(e, idx_map, out),
        IrExpr::Unary { expr: e, .. } => collect_expr(e, idx_map, out),
        IrExpr::Cast { expr: e, .. } => collect_expr(e, idx_map, out),
        IrExpr::Try(e) => collect_expr(e, idx_map, out),
        IrExpr::Field { base, .. } => collect_expr(base, idx_map, out),
        IrExpr::Index { base, index } => {
            collect_expr(base, idx_map, out);
            collect_expr(index, idx_map, out);
        }
        IrExpr::Binary { left, right, .. } | IrExpr::AssignOp { left, right, .. } => {
            collect_expr(left, idx_map, out);
            collect_expr(right, idx_map, out);
        }
        IrExpr::Assign { left, right } => {
            collect_expr(left, idx_map, out);
            collect_expr(right, idx_map, out);
        }
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start { collect_expr(s, idx_map, out); }
            if let Some(e) = end   { collect_expr(e, idx_map, out); }
        }
        IrExpr::Array(elems) | IrExpr::Tuple(elems) | IrExpr::FixedArray(elems) => {
            for el in elems { collect_expr(el, idx_map, out); }
        }
        IrExpr::Repeat { elem, len } => {
            collect_expr(elem, idx_map, out);
            collect_expr(len, idx_map, out);
        }
        IrExpr::ArrayGenerate { body, .. } => collect_expr(body, idx_map, out),
        IrExpr::Closure { body, .. } => collect_expr(body, idx_map, out),
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, v) in fields { collect_expr(v, idx_map, out); }
            if let Some(r) = rest { collect_expr(r, idx_map, out); }
        }
        IrExpr::RawMap { receiver, body, .. } => {
            collect_expr(receiver, idx_map, out);
            collect_expr(body, idx_map, out);
        }
        IrExpr::RawZip { left, right, body, .. } => {
            collect_expr(left, idx_map, out);
            collect_expr(right, idx_map, out);
            collect_expr(body, idx_map, out);
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            collect_expr(receiver, idx_map, out);
            collect_expr(init, idx_map, out);
            collect_expr(body, idx_map, out);
        }
        // Leaves: Lit, Var, Path, Return(None), Break(None), Continue, Unreachable,
        //         DefaultValue, LengthOf, TypenumUsize, IterPipeline
        _ => {}
    }
}

fn add_callee(name: &str, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    if let Some(&idx) = idx_map.get(name) {
        out.push(idx);
    }
}
