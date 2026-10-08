//! Call-graph reachability analysis for lazy/seeded backend emission.
//!
//! Given a module and a set of seed function names, computes the transitive
//! closure of all functions reachable from those seeds via direct calls.
//! Impl method bodies are also considered: any standalone function called
//! from an impl method is included if the method name is reachable from seeds.

#[cfg(feature = "std")]
use std::collections::{HashMap, HashSet, VecDeque};
#[cfg(feature = "std")]
use std::{string::String, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::collections::{BTreeMap as HashMap, BTreeSet as HashSet, VecDeque};
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};
#[cfg(feature = "std")]
use std::format;

use crate::ir::{
    IrBlock, IrExpr, IrExprKind, IrFunction, IrImplItem, IrIterChain, IrModule, IrStmt, IrStmtKind,
    IterChainSource, IterStep, IterTerminal, MethodKind,
};
use crate::printer_ts::is_namespace_prefix;

/// Result of reachability analysis: reachable standalone functions and
/// method names (for impl-block filtering in the backends).
pub struct ReachabilityResult {
    /// Names of reachable standalone module functions.
    pub fns: HashSet<String>,
    /// Method names transitively called from reachable functions.
    /// Backends use this to filter which impl methods to emit.
    pub method_names: HashSet<String>,
}

/// Compute reachability from `seeds`, integrating impl method expansion into
/// the BFS so standalone functions called from impl methods are included.
pub fn compute_reachable(module: &IrModule<IrFunction>, seeds: &[&str]) -> ReachabilityResult {
    if seeds.is_empty() {
        return ReachabilityResult {
            fns: HashSet::new(),
            method_names: HashSet::new(),
        };
    }

    let name_to_idx: HashMap<&str, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.name.as_str(), i))
        .collect();

    // For each impl method (keyed by method name), collect:
    //   - standalone function indices called from its body
    //   - method names called from its body (for transitive expansion)
    let mut method_to_fn_callees: HashMap<String, Vec<usize>> = HashMap::new();
    let mut method_to_method_calls: HashMap<String, Vec<String>> = HashMap::new();
    for imp in &module.impls {
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                let class_name = crate::printer_ts::self_ty_name(&imp.self_ty);
                let qualified_name = format!("{class_name}.{}", func.name);
                let fn_callees = collect_callees_fn(func, &name_to_idx);
                let method_calls = collect_method_calls_fn(func);
                method_to_fn_callees
                    .entry(func.name.clone())
                    .or_default()
                    .extend(fn_callees.iter().copied());
                method_to_fn_callees
                    .entry(qualified_name.clone())
                    .or_default()
                    .extend(fn_callees);
                method_to_method_calls
                    .entry(func.name.clone())
                    .or_default()
                    .extend(method_calls.iter().cloned());
                method_to_method_calls
                    .entry(qualified_name)
                    .or_default()
                    .extend(method_calls);
            }
        }
    }
    // Second pass: propagate one extra hop (impl method → impl method → standalone fn).
    // Clone keys first to avoid borrow issues.
    let impl_method_names: Vec<String> = method_to_fn_callees.keys().cloned().collect();
    for name in &impl_method_names {
        if let Some(sub_methods) = method_to_method_calls.get(name.as_str()).cloned() {
            for sub in sub_methods {
                if let Some(extra) = method_to_fn_callees.get(sub.as_str()).cloned() {
                    method_to_fn_callees
                        .entry(name.clone())
                        .or_default()
                        .extend(extra);
                }
            }
        }
    }

    // Build augmented callee graph: each function's direct callees PLUS
    // standalone functions reachable through impl methods it calls.
    let callees: Vec<Vec<usize>> = module
        .functions
        .iter()
        .map(|f| {
            let mut out = collect_callees_fn(f, &name_to_idx);
            let method_calls = collect_method_calls_fn(f);
            for mname in &method_calls {
                if let Some(extra) = method_to_fn_callees.get(mname.as_str()) {
                    out.extend_from_slice(extra);
                }
                // One more hop: methods called by the impl method
                if let Some(sub_methods) = method_to_method_calls.get(mname.as_str()) {
                    for sub in sub_methods {
                        if let Some(extra2) = method_to_fn_callees.get(sub.as_str()) {
                            out.extend_from_slice(extra2);
                        }
                    }
                }
            }
            out.sort_unstable();
            out.dedup();
            out
        })
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

    // Collect methods called by reachable standalone functions. Methods called
    // from a reachable impl method are followed within that impl, keeping helper
    // methods such as `splen` when a reachable `log_splen` calls them without
    // pulling in same-named methods from unrelated types.
    let mut method_names: HashSet<String> = visited
        .iter()
        .flat_map(|&idx| collect_method_calls_fn(&module.functions[idx]))
        .collect();
    let mut methods_by_class: HashMap<String, HashSet<String>> = HashMap::new();
    let mut method_calls_by_key: HashMap<String, Vec<String>> = HashMap::new();
    for imp in &module.impls {
        let class_name = crate::printer_ts::self_ty_name(&imp.self_ty);
        for item in &imp.items {
            let IrImplItem::Method(func) = item else {
                continue;
            };
            methods_by_class
                .entry(class_name.clone())
                .or_default()
                .insert(func.name.clone());
            method_calls_by_key.insert(
                format!("{class_name}.{}", func.name),
                collect_method_calls_fn(func),
            );
        }
    }

    let mut method_queue = VecDeque::new();
    for (class_name, names) in &methods_by_class {
        for name in names {
            let key = format!("{class_name}.{name}");
            if (method_names.contains(name) || method_names.contains(&key))
                && method_names.insert(key.clone())
            {
                method_queue.push_back(key);
            }
        }
    }
    while let Some(method_key) = method_queue.pop_front() {
        let Some((class_name, _)) = method_key.split_once('.') else {
            continue;
        };
        let Some(callees) = method_calls_by_key.get(&method_key) else {
            continue;
        };
        for callee in callees {
            let callee_key = if callee.contains('.') {
                callee.clone()
            } else if methods_by_class
                .get(class_name)
                .is_some_and(|names| names.contains(callee))
            {
                format!("{class_name}.{callee}")
            } else {
                continue;
            };
            if method_calls_by_key.contains_key(&callee_key)
                && method_names.insert(callee_key.clone())
            {
                method_queue.push_back(callee_key);
            }
        }
    }

    let mut fns: HashSet<String> = visited
        .into_iter()
        .map(|i| module.functions[i].name.clone())
        .collect();

    // Constants are always emitted regardless of seeding.
    // Any function they call must also be included.
    for c in &module.consts {
        let mut const_callees: Vec<usize> = Vec::new();
        collect_expr(&c.value, &name_to_idx, &mut const_callees);
        for idx in const_callees {
            fns.insert(module.functions[idx].name.clone());
        }
    }

    ReachabilityResult { fns, method_names }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn collect_callees_fn(func: &IrFunction, idx_map: &HashMap<&str, usize>) -> Vec<usize> {
    let mut out = Vec::new();
    collect_block(&func.body, idx_map, &mut out);
    out
}

fn collect_method_calls_fn(func: &IrFunction) -> Vec<String> {
    let mut out = Vec::new();
    collect_method_names_block(&func.body, &mut out);
    out
}

fn collect_block(block: &IrBlock, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    for stmt in &block.stmts {
        match &stmt.kind {
            IrStmtKind::Let { init: Some(e), .. } | IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => {
                collect_expr(e, idx_map, out);
            }
            IrStmtKind::LetElse {
                init, else_expr, ..
            } => {
                if let Some(init) = init {
                    collect_expr(init, idx_map, out);
                }
                collect_expr(else_expr, idx_map, out);
            }
            _ => {}
        }
    }
    if let Some(tail) = &block.expr {
        collect_expr(tail, idx_map, out);
    }
}

fn collect_method_names_block(block: &IrBlock, out: &mut Vec<String>) {
    for stmt in &block.stmts {
        match &stmt.kind {
            IrStmtKind::Let { init: Some(e), .. } | IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => {
                collect_method_names_expr(e, out);
            }
            IrStmtKind::LetElse {
                init, else_expr, ..
            } => {
                if let Some(init) = init {
                    collect_method_names_expr(init, out);
                }
                collect_method_names_expr(else_expr, out);
            }
            _ => {}
        }
    }
    if let Some(tail) = &block.expr {
        collect_method_names_expr(tail, out);
    }
}

fn collect_expr(expr: &IrExpr, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    match &expr.kind {
        IrExprKind::Call { func, args } => {
            match &func.kind {
                IrExprKind::Var(name) => add_callee(name, idx_map, out),
                IrExprKind::Path { segments, .. } => {
                    let last = segments
                        .iter()
                        .map(|s| s.as_str())
                        .filter(|s| !is_namespace_prefix(s))
                        .last();
                    if let Some(name) = last {
                        add_callee(name, idx_map, out);
                    }
                }
                _ => collect_expr(func, idx_map, out),
            }
            for a in args {
                collect_expr(a, idx_map, out);
            }
        }
        IrExprKind::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            collect_expr(receiver, idx_map, out);
            if let MethodKind::Other(name) = method {
                add_callee(name, idx_map, out);
            }
            for a in args {
                collect_expr(a, idx_map, out);
            }
        }
        IrExprKind::Block(b) => collect_block(b, idx_map, out),
        IrExprKind::IterPipeline(chain) => collect_iter_chain(chain, idx_map, out),
        IrExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_expr(cond, idx_map, out);
            collect_block(then_branch, idx_map, out);
            if let Some(eb) = else_branch {
                collect_expr(eb, idx_map, out);
            }
        }
        IrExprKind::Match { expr: e, arms } => {
            collect_expr(e, idx_map, out);
            for arm in arms {
                collect_expr(&arm.body, idx_map, out);
            }
        }
        IrExprKind::BoundedLoop {
            start, end, body, ..
        } => {
            collect_expr(start, idx_map, out);
            collect_expr(end, idx_map, out);
            collect_block(body, idx_map, out);
        }
        IrExprKind::IterLoop {
            collection, body, ..
        } => {
            collect_expr(collection, idx_map, out);
            collect_block(body, idx_map, out);
        }
        IrExprKind::WhileLoop { cond, body } => {
            collect_expr(cond, idx_map, out);
            collect_block(body, idx_map, out);
        }
        IrExprKind::Return(Some(e)) | IrExprKind::Break(Some(e)) => collect_expr(e, idx_map, out),
        IrExprKind::Unary { expr: e, .. } => collect_expr(e, idx_map, out),
        IrExprKind::Cast { expr: e, .. } => collect_expr(e, idx_map, out),
        IrExprKind::Try(e) => collect_expr(e, idx_map, out),
        IrExprKind::Field { base, .. } => collect_expr(base, idx_map, out),
        IrExprKind::Index { base, index } => {
            collect_expr(base, idx_map, out);
            collect_expr(index, idx_map, out);
        }
        IrExprKind::Binary { left, right, .. } | IrExprKind::AssignOp { left, right, .. } => {
            collect_expr(left, idx_map, out);
            collect_expr(right, idx_map, out);
        }
        IrExprKind::Assign { left, right } => {
            collect_expr(left, idx_map, out);
            collect_expr(right, idx_map, out);
        }
        IrExprKind::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_expr(s, idx_map, out);
            }
            if let Some(e) = end {
                collect_expr(e, idx_map, out);
            }
        }
        IrExprKind::Array(elems) | IrExprKind::Tuple(elems) | IrExprKind::FixedArray(elems) => {
            for el in elems {
                collect_expr(el, idx_map, out);
            }
        }
        IrExprKind::Repeat { elem, len } | IrExprKind::VecRepeat { elem, len } => {
            collect_expr(elem, idx_map, out);
            collect_expr(len, idx_map, out);
        }
        IrExprKind::ArrayGenerate { body, .. } => collect_expr(body, idx_map, out),
        IrExprKind::Closure { body, .. } => collect_expr(body, idx_map, out),
        IrExprKind::StructExpr { fields, rest, .. } => {
            for (_, v) in fields {
                collect_expr(v, idx_map, out);
            }
            if let Some(r) = rest {
                collect_expr(r, idx_map, out);
            }
        }
        IrExprKind::RawMap { receiver, body, .. } => {
            collect_expr(receiver, idx_map, out);
            collect_expr(body, idx_map, out);
        }
        IrExprKind::RawZip {
            left, right, body, ..
        } => {
            collect_expr(left, idx_map, out);
            collect_expr(right, idx_map, out);
            collect_expr(body, idx_map, out);
        }
        IrExprKind::RawFold {
            receiver,
            init,
            body,
            ..
        } => {
            collect_expr(receiver, idx_map, out);
            collect_expr(init, idx_map, out);
            collect_expr(body, idx_map, out);
        }
        _ => {}
    }
}

fn collect_method_names_expr(expr: &IrExpr, out: &mut Vec<String>) {
    match &expr.kind {
        IrExprKind::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            collect_method_names_expr(receiver, out);
            if let MethodKind::Other(name) = method {
                out.push(name.clone());
            }
            for a in args {
                collect_method_names_expr(a, out);
            }
        }
        IrExprKind::Call { func, args } => {
            if let IrExprKind::Path { segments, .. } = &func.kind {
                let path: Vec<&str> = segments
                    .iter()
                    .map(String::as_str)
                    .filter(|segment| !is_namespace_prefix(segment))
                    .collect();
                if path.len() >= 2 {
                    // Qualified paths such as `Factory::make()` are associated
                    // functions, not receiver method calls. Record a class-qualified
                    // name so same-named methods on unrelated types stay unreachable.
                    out.push(format!("{}.{}", path[path.len() - 2], path[path.len() - 1]));
                }
            }
            collect_method_names_expr(func, out);
            for a in args {
                collect_method_names_expr(a, out);
            }
        }
        IrExprKind::Block(b) => collect_method_names_block(b, out),
        IrExprKind::IterPipeline(chain) => collect_iter_chain_method_names(chain, out),
        IrExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_method_names_expr(cond, out);
            collect_method_names_block(then_branch, out);
            if let Some(eb) = else_branch {
                collect_method_names_expr(eb, out);
            }
        }
        IrExprKind::Match { expr: e, arms } => {
            collect_method_names_expr(e, out);
            for arm in arms {
                collect_method_names_expr(&arm.body, out);
            }
        }
        IrExprKind::BoundedLoop {
            start, end, body, ..
        } => {
            collect_method_names_expr(start, out);
            collect_method_names_expr(end, out);
            collect_method_names_block(body, out);
        }
        IrExprKind::IterLoop {
            collection, body, ..
        } => {
            collect_method_names_expr(collection, out);
            collect_method_names_block(body, out);
        }
        IrExprKind::WhileLoop { cond, body } => {
            collect_method_names_expr(cond, out);
            collect_method_names_block(body, out);
        }
        IrExprKind::Return(Some(e)) | IrExprKind::Break(Some(e)) => {
            collect_method_names_expr(e, out)
        }
        IrExprKind::Unary { expr: e, .. } => collect_method_names_expr(e, out),
        IrExprKind::Cast { expr: e, .. } => collect_method_names_expr(e, out),
        IrExprKind::Try(e) => collect_method_names_expr(e, out),
        IrExprKind::Field { base, .. } => collect_method_names_expr(base, out),
        IrExprKind::Index { base, index } => {
            collect_method_names_expr(base, out);
            collect_method_names_expr(index, out);
        }
        IrExprKind::Binary { left, right, .. } | IrExprKind::AssignOp { left, right, .. } => {
            collect_method_names_expr(left, out);
            collect_method_names_expr(right, out);
        }
        IrExprKind::Assign { left, right } => {
            collect_method_names_expr(left, out);
            collect_method_names_expr(right, out);
        }
        IrExprKind::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_method_names_expr(s, out);
            }
            if let Some(e) = end {
                collect_method_names_expr(e, out);
            }
        }
        IrExprKind::Array(elems) | IrExprKind::Tuple(elems) | IrExprKind::FixedArray(elems) => {
            for el in elems {
                collect_method_names_expr(el, out);
            }
        }
        IrExprKind::Repeat { elem, len } | IrExprKind::VecRepeat { elem, len } => {
            collect_method_names_expr(elem, out);
            collect_method_names_expr(len, out);
        }
        IrExprKind::ArrayGenerate { body, .. } => collect_method_names_expr(body, out),
        IrExprKind::Closure { body, .. } => collect_method_names_expr(body, out),
        IrExprKind::StructExpr { fields, rest, .. } => {
            for (_, v) in fields {
                collect_method_names_expr(v, out);
            }
            if let Some(r) = rest {
                collect_method_names_expr(r, out);
            }
        }
        IrExprKind::RawMap { receiver, body, .. } => {
            collect_method_names_expr(receiver, out);
            collect_method_names_expr(body, out);
        }
        IrExprKind::RawZip {
            left, right, body, ..
        } => {
            collect_method_names_expr(left, out);
            collect_method_names_expr(right, out);
            collect_method_names_expr(body, out);
        }
        IrExprKind::RawFold {
            receiver,
            init,
            body,
            ..
        } => {
            collect_method_names_expr(receiver, out);
            collect_method_names_expr(init, out);
            collect_method_names_expr(body, out);
        }
        _ => {}
    }
}

fn collect_iter_chain(chain: &IrIterChain, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    match &chain.source {
        IterChainSource::Method { collection, .. } => collect_expr(collection, idx_map, out),
        IterChainSource::Range { start, end, .. } => {
            collect_expr(start, idx_map, out);
            collect_expr(end, idx_map, out);
        }
        IterChainSource::Zip { left, right } => {
            collect_iter_chain(left, idx_map, out);
            collect_iter_chain(right, idx_map, out);
        }
    }

    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => collect_expr(body, idx_map, out),
            IterStep::Take { count } | IterStep::Skip { count } => {
                collect_expr(count, idx_map, out);
            }
            IterStep::Chain { other } => collect_iter_chain(other, idx_map, out),
            IterStep::Enumerate => {}
        }
    }

    if let IterTerminal::Fold { init, body, .. } = &chain.terminal {
        collect_expr(init, idx_map, out);
        collect_expr(body, idx_map, out);
    }
}

fn collect_iter_chain_method_names(chain: &IrIterChain, out: &mut Vec<String>) {
    match &chain.source {
        IterChainSource::Method { collection, .. } => collect_method_names_expr(collection, out),
        IterChainSource::Range { start, end, .. } => {
            collect_method_names_expr(start, out);
            collect_method_names_expr(end, out);
        }
        IterChainSource::Zip { left, right } => {
            collect_iter_chain_method_names(left, out);
            collect_iter_chain_method_names(right, out);
        }
    }

    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => collect_method_names_expr(body, out),
            IterStep::Take { count } | IterStep::Skip { count } => {
                collect_method_names_expr(count, out);
            }
            IterStep::Chain { other } => collect_iter_chain_method_names(other, out),
            IterStep::Enumerate => {}
        }
    }

    if let IterTerminal::Fold { init, body, .. } = &chain.terminal {
        collect_method_names_expr(init, out);
        collect_method_names_expr(body, out);
    }
}

fn add_callee(name: &str, idx_map: &HashMap<&str, usize>, out: &mut Vec<usize>) {
    if let Some(&idx) = idx_map.get(name) {
        out.push(idx);
    }
}
