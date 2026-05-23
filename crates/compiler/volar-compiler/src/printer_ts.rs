// @reliability: normal
// @ai: assisted
//! TypeScript code emitter for dyn-lowered IR.
//!
//! Consumes an `IrModule` (typically after `lowering_dyn`) and renders it as
//! TypeScript source text.  Structs become classes, impl methods attach to the
//! class body, and iterator pipelines map to `Array.prototype` methods.
//!
//! ## Impl merging
//!
//! Rust allows multiple `impl` blocks for the same struct with different type
//! specializations (e.g. `impl DeltaDyn<BitsInBytes>` vs
//! `impl DeltaDyn<BitsInBytes64>`).  In TypeScript a class can only have one
//! definition of each method.  We detect these collisions in a pre-analysis
//! pass and emit merged methods that dispatch at runtime via `instanceof`.
//!
//! ## Statement-expression boundary
//!
//! Rust treats `for`, `if` (without value), and other statements as
//! expressions of type `()`.  When such an expression appears as the *tail*
//! of a block in expression position we must **not** emit `return for(...)`.
//! Instead we emit the loop as a statement (no `return`).

use core::fmt::{self, Write};

#[cfg(feature = "std")]
use std::collections::BTreeMap;
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::deshadow::deshadow_block;
use crate::ir::*;

// ============================================================================
// WITNESS ANALYSIS — collect runtime witnesses needed per function
// ============================================================================
//
// Rust trait bounds carry type-level information (associated types, constructors,
// default values) that vanish in TypeScript.  We recover them as explicit runtime
// values passed through a `ctx` object.
//
// For each function / method we scan the IR body and collect `WitnessKind`s:
//   - Projection { type_param, field } — e.g. `B::OutputSize` → `ctx.B_OutputSize`
//   - Constructor { type_param }       — e.g. `D::new()`      → `ctx.newD()`
//   - Default { type_param }           — e.g. `O::default()`  → `ctx.defaultO()`
//
// Functions that need witnesses get `ctx: { B_OutputSize: number, … }` prepended
// to their parameter list.  Callers forward their own `ctx`.

/// A single runtime witness that a function body requires.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum WitnessKind {
    /// `<T>::OutputSize` or `<T as Trait>::Assoc` — a numeric constant
    Projection { type_param: String, field: String },
    /// `T::new()` — a factory call (typically `Digest::new`)
    Constructor { type_param: String },
    /// `T::default()` — produce the zero / identity element for a type param
    Default { type_param: String },
    /// `T` used as a value (any static call `T::method()` or bare `T`).
    /// The witness is the class constructor itself — `ctx.TClass`.
    /// Unlike Constructor/Default, generics are preserved: `ctx.TClass: typeof Galois`.
    Class { type_param: String },
    /// `mem::size_of_val(&x)` where `x: T` — byte count of one field element.
    /// Emitted as `ctx.sizeOfT: bigint`.
    SizeOf { type_param: String },
}

/// The set of witnesses needed by one function (or one merged method).
#[derive(Debug, Clone, Default)]
struct WitnessNeeds {
    needs: Vec<WitnessKind>,
}

impl WitnessNeeds {
    fn is_empty(&self) -> bool {
        self.needs.is_empty()
    }

    fn add(&mut self, kind: WitnessKind) {
        if !self.needs.contains(&kind) {
            self.needs.push(kind);
        }
    }

    fn merge(&mut self, other: &WitnessNeeds) {
        for k in &other.needs {
            self.add(k.clone());
        }
    }

    /// Sort for deterministic output.
    fn sorted(&mut self) {
        self.needs.sort();
    }
}

/// Recursively scan an expression for witness requirements.
fn scan_expr_witnesses(expr: &IrExpr, out: &mut WitnessNeeds, declared_generics: &[String]) {
    match expr {
        // DefaultValue with a type-param type → need a default witness.
        // DefaultValue with an Array type → scan the length for projections,
        // and recursively scan the element type for default witnesses.
        IrExpr::DefaultValue { ty: Some(ty) } => {
            match ty.as_ref() {
                IrType::TypeParam(name) => {
                    if declared_generics.contains(name) || is_crypto_type_param(name) {
                        out.add(WitnessKind::Default {
                            type_param: name.clone(),
                        });
                    }
                }
                IrType::Array { len, elem, .. } => {
                    scan_array_length_witnesses(len, out);
                    // Recursively check element type for defaults
                    scan_type_for_default_witnesses(elem, out, declared_generics);
                }
                _ => {}
            }
        }
        // LengthOf with a Projection → need a projection witness
        IrExpr::LengthOf(len) => {
            scan_array_length_witnesses(len, out);
        }
        // ArrayGenerate may contain a projection in its length
        IrExpr::ArrayGenerate {
            len,
            index_var: _,
            body,
            ..
        } => {
            scan_array_length_witnesses(len, out);
            scan_expr_witnesses(body, out, declared_generics);
        }
        // TypenumUsize with a Projection type
        IrExpr::TypenumUsize { ty } => {
            if let IrType::Projection { base, assoc, .. } = ty.as_ref() {
                if let IrType::TypeParam(name) = base.as_ref() {
                    out.add(WitnessKind::Projection {
                        type_param: name.clone(),
                        field: format!("{}", assoc),
                    });
                }
            }
        }
        // Call to T::new(), T::default(), or any other T::method() on a type param
        IrExpr::Call { func, args } => {
            if let IrExpr::Path { segments, .. } = func.as_ref() {
                if segments.len() == 2 {
                    let type_name = &segments[0];
                    let method = &segments[1];
                    let is_type_param = declared_generics.contains(type_name)
                        || is_crypto_type_param(type_name);
                    if is_type_param {
                        if method == "new" {
                            out.add(WitnessKind::Constructor {
                                type_param: type_name.clone(),
                            });
                        } else if method == "default" {
                            out.add(WitnessKind::Default {
                                type_param: type_name.clone(),
                            });
                        } else {
                            // Any other T::method() — need the class witness
                            out.add(WitnessKind::Class {
                                type_param: type_name.clone(),
                            });
                        }
                    }
                }
                // size_of_val(x) / size_of::<T>() via path — need SizeOf witness
                let filtered: Vec<&str> = segments.iter()
                    .map(|s| s.as_str())
                    .filter(|s| !matches!(*s, "super"|"crate"|"self"|"mem"|"std"|"core"|"alloc"))
                    .collect();
                if matches!(filtered.as_slice(), ["size_of_val"] | ["size_of"]) {
                    for tp in declared_generics {
                        out.add(WitnessKind::SizeOf { type_param: tp.clone() });
                    }
                }
            }
            // size_of_val as a bare Var (most common in lowered IR)
            if let IrExpr::Var(n) = func.as_ref() {
                if n == "size_of_val" || n == "size_of" {
                    for tp in declared_generics {
                        out.add(WitnessKind::SizeOf { type_param: tp.clone() });
                    }
                }
            }
            scan_expr_witnesses(func, out, declared_generics);
            for a in args {
                scan_expr_witnesses(a, out, declared_generics);
            }
        }
        // Recurse into all sub-expressions
        IrExpr::Binary { left, right, .. } | IrExpr::Assign { left, right } => {
            scan_expr_witnesses(left, out, declared_generics);
            scan_expr_witnesses(right, out, declared_generics);
        }
        IrExpr::AssignOp { left, right, .. } => {
            scan_expr_witnesses(left, out, declared_generics);
            scan_expr_witnesses(right, out, declared_generics);
        }
        IrExpr::Unary { expr, .. }
        | IrExpr::Return(Some(expr))
        | IrExpr::Cast { expr, .. }
        | IrExpr::Try(expr) => {
            scan_expr_witnesses(expr, out, declared_generics);
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            scan_expr_witnesses(receiver, out, declared_generics);
            for a in args {
                scan_expr_witnesses(a, out, declared_generics);
            }
        }
        IrExpr::Field { base, .. } => scan_expr_witnesses(base, out, declared_generics),
        IrExpr::Index { base, index } => {
            scan_expr_witnesses(base, out, declared_generics);
            scan_expr_witnesses(index, out, declared_generics);
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, e) in fields {
                scan_expr_witnesses(e, out, declared_generics);
            }
            if let Some(r) = rest {
                scan_expr_witnesses(r, out, declared_generics);
            }
        }
        IrExpr::Tuple(es) | IrExpr::Array(es) | IrExpr::FixedArray(es) => {
            for e in es {
                scan_expr_witnesses(e, out, declared_generics);
            }
        }
        IrExpr::Repeat { elem, len } => {
            scan_expr_witnesses(elem, out, declared_generics);
            scan_expr_witnesses(len, out, declared_generics);
        }
        IrExpr::Block(b) => scan_block_witnesses(b, out, declared_generics),
        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            scan_expr_witnesses(cond, out, declared_generics);
            scan_block_witnesses(then_branch, out, declared_generics);
            if let Some(eb) = else_branch {
                scan_expr_witnesses(eb, out, declared_generics);
            }
        }
        IrExpr::BoundedLoop {
            start, end, body, ..
        } => {
            scan_expr_witnesses(start, out, declared_generics);
            scan_expr_witnesses(end, out, declared_generics);
            scan_block_witnesses(body, out, declared_generics);
        }
        IrExpr::IterLoop {
            collection, body, ..
        } => {
            scan_expr_witnesses(collection, out, declared_generics);
            scan_block_witnesses(body, out, declared_generics);
        }
        IrExpr::WhileLoop { cond, body } => {
            scan_expr_witnesses(cond, out, declared_generics);
            scan_block_witnesses(body, out, declared_generics);
        }
        IrExpr::Closure { body, .. } => scan_expr_witnesses(body, out, declared_generics),
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start {
                scan_expr_witnesses(s, out, declared_generics);
            }
            if let Some(e) = end {
                scan_expr_witnesses(e, out, declared_generics);
            }
        }
        IrExpr::Match { expr, arms } => {
            scan_expr_witnesses(expr, out, declared_generics);
            for arm in arms {
                scan_expr_witnesses(&arm.body, out, declared_generics);
            }
        }
        IrExpr::IterPipeline(chain) => scan_iter_chain_witnesses(chain, out, declared_generics),
        IrExpr::RawMap { receiver, body, .. } => {
            scan_expr_witnesses(receiver, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        IrExpr::RawZip {
            left, right, body, ..
        } => {
            scan_expr_witnesses(left, out, declared_generics);
            scan_expr_witnesses(right, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        IrExpr::RawFold {
            receiver,
            init,
            body,
            ..
        } => {
            scan_expr_witnesses(receiver, out, declared_generics);
            scan_expr_witnesses(init, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        // Bare `T` used as a value (e.g. passed to a function expecting a class)
        IrExpr::Var(name) => {
            if declared_generics.contains(name) || is_crypto_type_param(name) {
                out.add(WitnessKind::Class { type_param: name.clone() });
            }
        }
        IrExpr::Path { segments, .. } if segments.len() == 1 => {
            let name = &segments[0];
            if declared_generics.contains(name) || is_crypto_type_param(name) {
                out.add(WitnessKind::Class { type_param: name.clone() });
            }
        }
        _ => {} // Lit, Path(multi-seg), Break, Continue, Unreachable, etc.
    }
}

fn scan_block_witnesses(block: &IrBlock, out: &mut WitnessNeeds, declared_generics: &[String]) {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { init: Some(e), .. } | IrStmt::Semi(e) | IrStmt::Expr(e) => {
                scan_expr_witnesses(e, out, declared_generics);
            }
            _ => {}
        }
    }
    if let Some(e) = &block.expr {
        scan_expr_witnesses(e, out, declared_generics);
    }
}

fn scan_iter_chain_witnesses(
    chain: &IrIterChain,
    out: &mut WitnessNeeds,
    declared_generics: &[String],
) {
    match &chain.source {
        IterChainSource::Method { collection, .. } => {
            scan_expr_witnesses(collection, out, declared_generics);
        }
        IterChainSource::Range { start, end, .. } => {
            scan_expr_witnesses(start, out, declared_generics);
            scan_expr_witnesses(end, out, declared_generics);
        }
        IterChainSource::Zip { left, right } => {
            scan_iter_chain_witnesses(left, out, declared_generics);
            scan_iter_chain_witnesses(right, out, declared_generics);
        }
    }
    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => {
                scan_expr_witnesses(body, out, declared_generics);
            }
            IterStep::Take { count } | IterStep::Skip { count } => {
                scan_expr_witnesses(count, out, declared_generics);
            }
            IterStep::Chain { other } => scan_iter_chain_witnesses(other, out, declared_generics),
            IterStep::Enumerate => {}
        }
    }
    match &chain.terminal {
        IterTerminal::Fold { init, body, .. } => {
            scan_expr_witnesses(init, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => {}
    }
}

fn scan_array_length_witnesses(len: &ArrayLength, out: &mut WitnessNeeds) {
    if let ArrayLength::Projection { r#type, field, .. } = len {
        if let IrType::TypeParam(name) = r#type.as_ref() {
            out.add(WitnessKind::Projection {
                type_param: name.clone(),
                field: field.clone(),
            });
        }
    }
}

/// Recursively scan a type for default-witness requirements.
/// Used when `DefaultValue { ty: Array<T, N> }` has a nested element type
/// that itself requires a default witness (e.g. `T` is a type param).
fn scan_type_for_default_witnesses(
    ty: &IrType,
    out: &mut WitnessNeeds,
    declared_generics: &[String],
) {
    match ty {
        IrType::TypeParam(name) => {
            if declared_generics.contains(name) || is_crypto_type_param(name) {
                out.add(WitnessKind::Default {
                    type_param: name.clone(),
                });
            }
        }
        IrType::Array { elem, len, .. } => {
            scan_array_length_witnesses(len, out);
            scan_type_for_default_witnesses(elem, out, declared_generics);
        }
        _ => {}
    }
}

/// Compute witness needs for a function (or method).
///
/// `extra_generics` should include impl-level generics when scanning a method,
/// so that type params declared on the impl (not just the method) are recognised
/// as needing witnesses.
fn compute_function_witnesses(
    func: &IrFunction,
    extra_generics: &[&IrGenericParam],
) -> WitnessNeeds {
    let mut declared: Vec<String> = func.generics.iter().map(|g| g.name.clone()).collect();
    for g in extra_generics {
        if !declared.contains(&g.name) {
            declared.push(g.name.clone());
        }
    }
    let mut needs = WitnessNeeds::default();
    scan_block_witnesses(&func.body, &mut needs, &declared);
    needs.sorted();
    needs
}

/// Compute merged witness needs for a set of method variants.
fn compute_merged_witnesses(variants: &[ImplMethod<'_>]) -> WitnessNeeds {
    let mut needs = WitnessNeeds::default();
    for v in variants {
        let impl_generics: Vec<&IrGenericParam> = v.imp.generics.iter().collect();
        needs.merge(&compute_function_witnesses(v.func, &impl_generics));
    }
    needs.sorted();
    needs
}

/// Returns true if a name is a type parameter that typically comes from a crypto
/// trait bound (Digest, LengthDoubler, etc.) and might need witnesses.
fn is_crypto_type_param(name: &str) -> bool {
    matches!(
        name,
        "B" | "D" | "N" | "O" | "T" | "A" | "Q" | "M" | "U" | "R" | "X" | "Y"
    )
}

/// Primitive field-element classes that are tuple structs in Rust
/// and need `new` in TypeScript.
fn is_primitive_class(name: &str) -> bool {
    matches!(
        name,
        "Bit" | "Galois" | "Galois64" | "BitsInBytes" | "BitsInBytes64" | "Z3"
    )
}

/// Get the TypeScript parameter name for a witness in the `ctx` object.
fn witness_ctx_field(kind: &WitnessKind) -> String {
    match kind {
        WitnessKind::Projection { type_param, field } => format!("{}_{}", type_param, field),
        WitnessKind::Constructor { type_param } => format!("new{}", type_param),
        WitnessKind::Default { type_param } => format!("default{}", type_param),
        WitnessKind::Class { type_param } => format!("{}Class", type_param),
        WitnessKind::SizeOf { type_param } => format!("sizeOf{}", type_param),
    }
}

/// Get the TypeScript type for a witness field.
fn witness_ctx_type(kind: &WitnessKind) -> &'static str {
    match kind {
        WitnessKind::Projection { .. } => "bigint",
        WitnessKind::Constructor { .. } => "() => any",
        WitnessKind::Default { .. } => "() => any",
        // Class witness: a constructor function with static methods accessible on it.
        WitnessKind::Class { .. } => "{ new(...args: any[]): any } & Record<string, (...args: any[]) => any>",
        WitnessKind::SizeOf { .. } => "bigint",
    }
}

/// Write the `ctx` parameter type inline: `ctx: { B_OutputSize: number, newD: () => any }`
fn write_ctx_param(needs: &WitnessNeeds, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ctx: {{ ")?;
    for (i, kind) in needs.needs.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}: {}", witness_ctx_field(kind), witness_ctx_type(kind))?;
    }
    write!(f, " }}")
}

/// Build a map from internal function names to their witness needs.
/// Used so that callers know they must forward `ctx` when calling these functions.
fn build_module_witness_map(module: &IrModule<IrFunction>) -> BTreeMap<Vec<String>, WitnessNeeds> {
    let mut map: BTreeMap<Vec<String>, WitnessNeeds> = BTreeMap::new();

    // First pass: direct witness needs.
    // Top-level functions use full IrPath as key; methods use vec!["ClassName.methodName"].
    for func in &module.functions {
        let needs = compute_function_witnesses(func, &[]);
        if !needs.is_empty() {
            let bare = if func.name.starts_with("r#") { &func.name[2..] } else { &func.name };
            let key = item_irpath(&func.module_path, bare);
            map.insert(key, needs);
        }
    }
    for imp in &module.impls {
        let impl_generics: Vec<&IrGenericParam> = imp.generics.iter().collect();
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                let needs = compute_function_witnesses(func, &impl_generics);
                if !needs.is_empty() {
                    let class_name = self_ty_name(&imp.self_ty);
                    let method_name = ts_method_name(&func.name, imp.trait_.as_ref());
                    // Methods are keyed by a single-element vec to distinguish from top-level fns
                    let key: Vec<String> = core::iter::once(format!("{}.{}", class_name, method_name)).collect();
                    map.insert(key, needs);
                }
            }
        }
    }

    // Second pass: transitive closure — if a function body calls another function
    // that needs witnesses, the caller also needs those witnesses (so it can
    // forward `ctx`).  We iterate until no new needs are added.
    //
    // Collect all (IrPath key, body) pairs for scanning.
    let mut all_funcs: Vec<(Vec<String>, &IrBlock)> = Vec::new();
    for func in &module.functions {
        let bare = if func.name.starts_with("r#") { &func.name[2..] } else { &func.name };
        let key = item_irpath(&func.module_path, bare);
        all_funcs.push((key, &func.body));
    }
    for imp in &module.impls {
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                let class_name = self_ty_name(&imp.self_ty);
                let method_name = ts_method_name(&func.name, imp.trait_.as_ref());
                let key: Vec<String> = core::iter::once(format!("{}.{}", class_name, method_name)).collect();
                all_funcs.push((key, &func.body));
            }
        }
    }

    loop {
        let mut changed = false;
        for (caller_key, body) in &all_funcs {
            let callee_names = collect_call_targets(body);
            let mut extra = WitnessNeeds::default();
            for callee in &callee_names {
                // Resolve bare callee name by last segment
                if let Some(callee_needs) = map.iter()
                    .find(|(k, _)| k.last().map(|s| s.as_str()) == Some(callee.as_str()))
                    .map(|(_, v)| v)
                {
                    extra.merge(callee_needs);
                }
            }
            if extra.is_empty() {
                continue;
            }
            let entry = map.entry(caller_key.clone()).or_default();
            let before = entry.needs.len();
            entry.merge(&extra);
            entry.sorted();
            if entry.needs.len() > before {
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    map
}

/// Compute the set of bare item names that appear in more than one origin crate.
/// These items need `$`-qualified TS names to avoid collisions.
fn compute_name_collisions(module: &IrModule<IrFunction>) -> std::collections::HashSet<String> {
    use std::collections::HashMap;
    let mut counts: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
    for f in &module.functions {
        counts.entry(f.name.clone())
            .or_default()
            .insert(f.module_path.first().cloned().unwrap_or_default());
    }
    for s in &module.structs {
        counts.entry(s.kind.to_string())
            .or_default()
            .insert(s.module_path.first().cloned().unwrap_or_default());
    }
    counts.into_iter()
        .filter(|(_, crates)| crates.len() > 1)
        .map(|(name, _)| name)
        .collect()
}

/// Collect the simple names of all functions called in a block (non-method calls only).
fn collect_call_targets(block: &IrBlock) -> Vec<String> {
    let mut targets = Vec::new();
    collect_call_targets_block(block, &mut targets);
    targets
}

fn collect_call_targets_block(block: &IrBlock, targets: &mut Vec<String>) {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { init: Some(e), .. } | IrStmt::Semi(e) | IrStmt::Expr(e) => {
                collect_call_targets_expr(e, targets);
            }
            _ => {}
        }
    }
    if let Some(e) = &block.expr {
        collect_call_targets_expr(e, targets);
    }
}

fn collect_call_targets_expr(expr: &IrExpr, targets: &mut Vec<String>) {
    match expr {
        IrExpr::Call { func, args } => {
            if let IrExpr::Path { segments, .. } = func.as_ref() {
                if segments.len() == 1 {
                    targets.push(segments[0].clone());
                }
            }
            collect_call_targets_expr(func, targets);
            for a in args {
                collect_call_targets_expr(a, targets);
            }
        }
        IrExpr::Binary { left, right, .. }
        | IrExpr::Assign { left, right }
        | IrExpr::AssignOp { left, right, .. } => {
            collect_call_targets_expr(left, targets);
            collect_call_targets_expr(right, targets);
        }
        IrExpr::Unary { expr, .. }
        | IrExpr::Return(Some(expr))
        | IrExpr::Cast { expr, .. }
        | IrExpr::Try(expr)
        | IrExpr::Field { base: expr, .. } => {
            collect_call_targets_expr(expr, targets);
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            collect_call_targets_expr(receiver, targets);
            for a in args {
                collect_call_targets_expr(a, targets);
            }
        }
        IrExpr::Index { base, index } => {
            collect_call_targets_expr(base, targets);
            collect_call_targets_expr(index, targets);
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, e) in fields {
                collect_call_targets_expr(e, targets);
            }
            if let Some(r) = rest {
                collect_call_targets_expr(r, targets);
            }
        }
        IrExpr::Tuple(es) | IrExpr::Array(es) | IrExpr::FixedArray(es) => {
            for e in es {
                collect_call_targets_expr(e, targets);
            }
        }
        IrExpr::Repeat { elem, len } => {
            collect_call_targets_expr(elem, targets);
            collect_call_targets_expr(len, targets);
        }
        IrExpr::Block(b) => collect_call_targets_block(b, targets),
        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_call_targets_expr(cond, targets);
            collect_call_targets_block(then_branch, targets);
            if let Some(eb) = else_branch {
                collect_call_targets_expr(eb, targets);
            }
        }
        IrExpr::BoundedLoop {
            start, end, body, ..
        } => {
            collect_call_targets_expr(start, targets);
            collect_call_targets_expr(end, targets);
            collect_call_targets_block(body, targets);
        }
        IrExpr::IterLoop {
            collection, body, ..
        } => {
            collect_call_targets_expr(collection, targets);
            collect_call_targets_block(body, targets);
        }
        IrExpr::WhileLoop { cond, body } => {
            collect_call_targets_expr(cond, targets);
            collect_call_targets_block(body, targets);
        }
        IrExpr::Closure { body, .. } => collect_call_targets_expr(body, targets),
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_call_targets_expr(s, targets);
            }
            if let Some(e) = end {
                collect_call_targets_expr(e, targets);
            }
        }
        IrExpr::Match { expr, arms } => {
            collect_call_targets_expr(expr, targets);
            for arm in arms {
                collect_call_targets_expr(&arm.body, targets);
            }
        }
        IrExpr::IterPipeline(chain) => collect_call_targets_chain(chain, targets),
        IrExpr::RawMap { receiver, body, .. } | IrExpr::RawFold { receiver, body, .. } => {
            collect_call_targets_expr(receiver, targets);
            collect_call_targets_expr(body, targets);
        }
        IrExpr::RawZip {
            left, right, body, ..
        } => {
            collect_call_targets_expr(left, targets);
            collect_call_targets_expr(right, targets);
            collect_call_targets_expr(body, targets);
        }
        _ => {}
    }
}

fn collect_call_targets_chain(chain: &IrIterChain, targets: &mut Vec<String>) {
    match &chain.source {
        IterChainSource::Method { collection, .. } => {
            collect_call_targets_expr(collection, targets)
        }
        IterChainSource::Range { start, end, .. } => {
            collect_call_targets_expr(start, targets);
            collect_call_targets_expr(end, targets);
        }
        IterChainSource::Zip { left, right } => {
            collect_call_targets_chain(left, targets);
            collect_call_targets_chain(right, targets);
        }
    }
    for step in &chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => {
                collect_call_targets_expr(body, targets);
            }
            IterStep::Take { count } | IterStep::Skip { count } => {
                collect_call_targets_expr(count, targets)
            }
            IterStep::Chain { other } => collect_call_targets_chain(other, targets),
            IterStep::Enumerate => {}
        }
    }
    match &chain.terminal {
        IterTerminal::Fold { init, body, .. } => {
            collect_call_targets_expr(init, targets);
            collect_call_targets_expr(body, targets);
        }
        _ => {}
    }
}

// ============================================================================
// PUBLIC API
// ============================================================================

/// Render an `IrModule` as a complete TypeScript source file.
///
/// Clones the module and applies a deshadowing pass to all function/method
/// bodies before emitting.  The deshadow pass turns Rust-legal `let x = f(x)`
/// shadows into TS-legal `const x_1 = f(x)`.
pub fn print_module_ts(module: &IrModule<IrFunction>) -> String {
    print_module_ts_with_imports(module, &[])
}

/// Render an `IrModule` as TypeScript with ESM `import` statements for remote specs.
///
/// `remotes` is typically obtained from [`LinkageSystem::remote_refs`].
/// Each remote entry emits one `import { TypeA, TypeB } from 'package'` line
/// before the module body.
pub fn print_module_ts_with_imports(
    module: &IrModule<IrFunction>,
    remotes: &[crate::linkage::RemoteSpecRef<'_>],
) -> String {
    let mut module = module.clone();
    deshadow_module(&mut module);

    let witness_map = build_module_witness_map(&module);
    let erased = collect_erased_type_params(&module);
    // IrPath-keyed tuple struct set
    let tuple_structs: std::collections::HashSet<Vec<String>> = module.structs.iter()
        .filter(|s| s.is_tuple)
        .map(|s| item_irpath(&s.module_path, &s.kind.to_string()))
        .collect();
    // Pre-pass: bare names that appear in more than one origin crate → get $-qualified TS names
    let name_collisions = compute_name_collisions(&module);
    // Enum names for detecting Enum::Variant(...) constructor call sites
    let enum_names: std::collections::HashSet<String> = module.enums.iter()
        .map(|e| e.kind.to_string())
        .collect();
    let cx = TsContext {
        witness_map: &witness_map,
        name_collisions: &name_collisions,
        erased_type_params: erased,
        self_type: None,
        tuple_structs: &tuple_structs,
        class_witnesses: Vec::new(),
        mut_refs: Vec::new(),
        enum_names: &enum_names,
        var_types: core::cell::RefCell::new(Vec::new()),
    };
    let local_names: std::collections::HashSet<String> = module.structs.iter()
        .map(|s| s.kind.to_string())
        .collect();
    let mut out = String::new();
    // ESM imports for remote specs
    let _ = write!(out, "{}", TsImportsWriter { remotes });
    let _ = write!(out, "{}", TsFmt(TsPreambleWriter { local_names: &local_names }, &cx));
    let _ = write!(out, "{}", TsFmt(TsModuleWriter { module: &module }, &cx));
    out
}

/// Emits ESM `import` statements for remote-linked specs.
struct TsImportsWriter<'a> {
    remotes: &'a [crate::linkage::RemoteSpecRef<'a>],
}

impl<'a> fmt::Display for TsImportsWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for remote in self.remotes {
            if !remote.type_names.is_empty() {
                writeln!(
                    f,
                    "import {{ {} }} from '{}';",
                    remote.type_names.join(", "),
                    remote.ts_package()
                )?;
            }
        }
        if !self.remotes.is_empty() {
            writeln!(f)?;
        }
        Ok(())
    }
}

/// Render an `IrCfgModule` as a complete TypeScript source file.
///
/// CFG functions are emitted as `while (true) { switch (__state) { case N: ... } }`
/// state machines, matching the Rust CFG printer strategy.  Auxiliary (non-CFG)
/// functions use the standard `TsFunctionWriter` with full deshadowing and
/// witness analysis (they are copies of spec functions that may have shadows
/// and ctx-style witness patterns).
pub fn print_cfg_module_ts(module: &IrCfgModule) -> String {
    // Extract auxiliary (flat) functions and build a temporary flat IrModule so
    // we can reuse deshadow + witness analysis on them.
    let aux_functions: Vec<IrFunction> = module.functions.iter().filter_map(|f| {
        if let IrAnyFunction::Flat(f) = f { Some(f.clone()) } else { None }
    }).collect();

    let mut flat: IrModule<IrFunction> = IrModule {
        name: module.name.clone(),
        structs: module.structs.clone(),
        enums: module.enums.clone(),
        traits: module.traits.clone(),
        impls: module.impls.clone(),
        functions: aux_functions,
        type_aliases: module.type_aliases.clone(),
        consts: module.consts.clone(),
    };
    deshadow_module(&mut flat);

    let witness_map = build_module_witness_map(&flat);
    let erased = collect_erased_type_params(&flat);
    let tuple_structs_flat: std::collections::HashSet<Vec<String>> = flat.structs.iter()
        .filter(|s| s.is_tuple)
        .map(|s| item_irpath(&s.module_path, &s.kind.to_string()))
        .collect();
    let name_collisions_flat = compute_name_collisions(&flat);
    let enum_names_flat: std::collections::HashSet<String> = flat.enums.iter()
        .map(|e| e.kind.to_string())
        .collect();
    let cx = TsContext {
        witness_map: &witness_map,
        name_collisions: &name_collisions_flat,
        erased_type_params: erased,
        self_type: None,
        tuple_structs: &tuple_structs_flat,
        class_witnesses: Vec::new(),
        mut_refs: Vec::new(),
        enum_names: &enum_names_flat,
        var_types: core::cell::RefCell::new(Vec::new()),
    };

    // Reassemble the CFG module with deshadowed auxiliary functions.
    let cfg_functions: Vec<IrAnyFunction> = module.functions.iter().filter_map(|f| {
        if let IrAnyFunction::Cfg(f) = f { Some(IrAnyFunction::Cfg(f.clone())) } else { None }
    }).collect();
    let deshadowed_aux: Vec<IrAnyFunction> = flat.functions.into_iter().map(IrAnyFunction::Flat).collect();
    let deshadowed: IrCfgModule = IrModule {
        name: module.name.clone(),
        structs: flat.structs,
        enums: flat.enums,
        traits: flat.traits,
        impls: flat.impls,
        functions: cfg_functions.into_iter().chain(deshadowed_aux).collect(),
        type_aliases: flat.type_aliases,
        consts: flat.consts,
    };

    let mut out = String::new();
    let _ = write!(
        out,
        "{}",
        TsFmt(TsCfgModuleWriter { module: &deshadowed }, &cx)
    );
    out
}

/// Apply deshadowing to every function and method body in the module.
fn deshadow_module(module: &mut IrModule<IrFunction>) {
    use std::collections::HashSet;

    // Top-level functions
    for func in &mut module.functions {
        let mut outer: HashSet<String> = HashSet::new();
        for p in &func.params {
            outer.insert(p.name.clone());
        }
        deshadow_block(&mut func.body, &outer);
    }

    // Impl methods
    for imp in &mut module.impls {
        for item in &mut imp.items {
            if let IrImplItem::Method(method) = item {
                let mut outer: HashSet<String> = HashSet::new();
                // `self` is always in scope for methods
                outer.insert("self".to_string());
                for p in &method.params {
                    outer.insert(p.name.clone());
                }
                deshadow_block(&mut method.body, &outer);
            }
        }
    }
}

// ============================================================================
// Shared context threaded through all writers
// ============================================================================

/// A mutable-reference variable in scope: `(var_name, array_expr_str, index_var)`.
/// Variables in this list are references with shape `{"*": value}`.
/// Reading `*var` emits `array[Number(index)]`; writing `*var = x` emits `array[Number(index)] = x`.
#[derive(Clone)]
struct MutRef {
    var: String,
    array_expr: String,
    index_var: String,
}

/// Immutable context passed to every TS writer.
struct TsContext<'a> {
    /// Per-function witness needs keyed by IrPath (`module_path + name`) for
    /// top-level functions, and `vec!["ClassName.methodName"]` for methods.
    witness_map: &'a BTreeMap<Vec<String>, WitnessNeeds>,
    /// Bare function names that appear in more than one origin module.
    /// These are emitted as `module_stem$name` in TS to avoid collisions.
    name_collisions: &'a std::collections::HashSet<String>,
    /// Type parameter names that have been erased (Fn-bounded, AsRef-bounded, etc.)
    /// and should be printed as `any` in type positions.
    erased_type_params: Vec<String>,
    /// Class name in scope when emitting inside a class body. `Self` maps to `this`.
    self_type: Option<String>,
    /// Struct names that are tuple structs — their constructors use positional `new T(x)` syntax.
    /// Keyed by IrPath (module_path + struct_name).
    tuple_structs: &'a std::collections::HashSet<Vec<String>>,
    /// Type-param names that have a Class witness in the current function scope.
    /// When a bare `G` (Var/Path) is seen, emit `ctx.GClass` instead.
    class_witnesses: Vec<String>,
    /// Mutable-reference variables in scope (from `iter_mut` loops).
    mut_refs: Vec<MutRef>,
    /// Enum type names in this module. Used to detect `Enum::Variant(...)` constructor calls
    /// and emit `Enum_Variant(...)` (factory function) instead of `Enum.Variant(...)`.
    enum_names: &'a std::collections::HashSet<String>,
    /// Variable name → type-param name, built from `let x: T = ...` bindings as we
    /// emit statements.  Used to resolve `size_of_val(x)` → `ctx.sizeOfT`.
    /// Uses interior mutability so we can accumulate bindings through `&TsContext`.
    var_types: core::cell::RefCell<Vec<(String, String)>>,
}

impl<'a> TsContext<'a> {
    /// Look up witness needs for a bare function name (last segment of IrPath).
    fn get_witness_needs(&self, bare_name: &str) -> Option<&WitnessNeeds> {
        self.witness_map.iter()
            .find(|(k, _)| k.last().map(|s| s.as_str()) == Some(bare_name))
            .map(|(_, v)| v)
    }

    /// Check if a bare struct/function name is a tuple struct (by last segment of IrPath).
    fn is_tuple_struct(&self, bare_name: &str) -> bool {
        self.tuple_structs.iter()
            .any(|k| k.last().map(|s| s.as_str()) == Some(bare_name))
    }

    /// Return the TS-qualified name for an item: bare name if unique, `stem$name` if colliding.
    fn ts_qualified_name(&self, module_path: &[String], bare_name: &str) -> String {
        if self.name_collisions.contains(bare_name) && !module_path.is_empty() {
            // Use the parent module stem (second-to-last element of the full path) as qualifier
            let stem = module_path.last().map(|s| s.as_str()).unwrap_or("_");
            format!("{}${}", stem, bare_name)
        } else {
            bare_name.to_string()
        }
    }

    fn with_self_type(&self, name: &str) -> TsContext<'a> {
        TsContext {
            witness_map: self.witness_map,
            name_collisions: self.name_collisions,
            erased_type_params: self.erased_type_params.clone(),
            self_type: Some(name.to_string()),
            tuple_structs: self.tuple_structs,
            class_witnesses: self.class_witnesses.clone(),
            mut_refs: self.mut_refs.clone(),
            enum_names: self.enum_names,
            var_types: core::cell::RefCell::new(self.var_types.borrow().clone()),
        }
    }

    fn with_class_witnesses(&self, witnesses: Vec<String>) -> TsContext<'a> {
        TsContext {
            witness_map: self.witness_map,
            name_collisions: self.name_collisions,
            erased_type_params: self.erased_type_params.clone(),
            self_type: self.self_type.clone(),
            tuple_structs: self.tuple_structs,
            class_witnesses: witnesses,
            mut_refs: self.mut_refs.clone(),
            enum_names: self.enum_names,
            var_types: core::cell::RefCell::new(self.var_types.borrow().clone()),
        }
    }

    fn with_mut_ref(&self, mut_ref: MutRef) -> TsContext<'a> {
        let mut mut_refs = self.mut_refs.clone();
        mut_refs.push(mut_ref);
        TsContext {
            witness_map: self.witness_map,
            name_collisions: self.name_collisions,
            erased_type_params: self.erased_type_params.clone(),
            self_type: self.self_type.clone(),
            tuple_structs: self.tuple_structs,
            class_witnesses: self.class_witnesses.clone(),
            mut_refs,
            enum_names: self.enum_names,
            var_types: core::cell::RefCell::new(self.var_types.borrow().clone()),
        }
    }

    fn register_var_type(&self, name: &str, type_param: &str) {
        self.var_types.borrow_mut().push((name.to_string(), type_param.to_string()));
    }

    fn lookup_var_type<'b>(&'b self, name: &str) -> Option<String> {
        self.var_types.borrow().iter().rev()
            .find(|(k, _)| k == name)
            .map(|(_, v)| v.clone())
    }

    fn find_mut_ref(&self, var: &str) -> Option<&MutRef> {
        self.mut_refs.iter().rev().find(|r| r.var == var)
    }
}

/// Collect type parameter names that are erased in TS (Fn-bounded, AsRef-bounded)
/// across the entire module.  These will be printed as `any`.
fn collect_erased_type_params(module: &IrModule<IrFunction>) -> Vec<String> {
    // First, collect all type-param names that would survive into TS generics
    // (type-kind, no Fn/AsRef/Math/Into bounds).
    let mut surviving = Vec::new();
    let mut all_seen = Vec::new();

    let classify = |g: &IrGenericParam, surviving: &mut Vec<String>, all_seen: &mut Vec<String>| {
        if g.kind != IrGenericParamKind::Type {
            return;
        }
        if !all_seen.contains(&g.name) {
            all_seen.push(g.name.clone());
        }
        let is_filtered = g.bounds.iter().any(|b| {
            matches!(
                &b.trait_kind,
                TraitKind::Fn(..)
                    | TraitKind::AsRef(..)
                    | TraitKind::Math(..)
                    | TraitKind::Into(..)
            )
        });
        // Also consider boundless single-letter params that look like
        // associated-type outputs (O, A, U, M, Q, R, X, Y).
        let is_assoc_output = g.bounds.is_empty() && is_crypto_type_param(&g.name);
        if !is_filtered && !is_assoc_output {
            if !surviving.contains(&g.name) {
                surviving.push(g.name.clone());
            }
        }
    };

    for s in &module.structs {
        for g in &s.generics {
            classify(g, &mut surviving, &mut all_seen);
        }
    }
    for func in &module.functions {
        for g in &func.generics {
            classify(g, &mut surviving, &mut all_seen);
        }
    }
    for imp in &module.impls {
        // Classify impl-level generics (e.g. `impl<T: Add> ... for Foo`).
        for g in &imp.generics {
            classify(g, &mut surviving, &mut all_seen);
        }
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                for g in &func.generics {
                    classify(g, &mut surviving, &mut all_seen);
                }
            }
        }
    }

    // Also collect type params referenced inside Fn bounds' return types
    // (e.g. F: FnMut(&[u8]) -> X means X is an indirect type param)
    let add_fn_return_params = |generics: &[IrGenericParam], all_seen: &mut Vec<String>| {
        for g in generics {
            for b in &g.bounds {
                if let TraitKind::Fn(_, ret) = &b.trait_kind {
                    if let IrType::TypeParam(name) = ret.as_ref() {
                        if !all_seen.contains(name) {
                            all_seen.push(name.clone());
                        }
                    }
                }
            }
        }
    };
    for func in &module.functions {
        add_fn_return_params(&func.generics, &mut all_seen);
    }
    for imp in &module.impls {
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                add_fn_return_params(&func.generics, &mut all_seen);
            }
        }
    }

    // Erased = seen but not surviving
    all_seen
        .into_iter()
        .filter(|name| !surviving.contains(name))
        .collect()
}

// ============================================================================
// Display adapter (same pattern as printer.rs)
// ============================================================================

trait TsBackend {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result;
}

struct TsFmt<'a, T: TsBackend>(T, &'a TsContext<'a>);

impl<'a, T: TsBackend> fmt::Display for TsFmt<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.ts_fmt(f, self.1)
    }
}

// ============================================================================
// PRE-ANALYSIS: detect overlapping impl methods
// ============================================================================

/// A single concrete method from an `impl` block, together with the type
/// specialization of the `self_ty` (if any).
#[derive(Debug, Clone)]
struct ImplMethod<'a> {
    func: &'a IrFunction,
    imp: &'a IrImpl,
    /// Concrete type args on self_ty, e.g. `[BitsInBytes]` for `impl Foo<BitsInBytes>`.
    self_type_args: Vec<&'a IrType>,
}

/// For one struct, the set of methods grouped by name.
/// Methods that only appear once go straight through.
/// Methods that appear multiple times (from type-specialized impls) get merged.
#[derive(Debug)]
struct ClassAnalysis<'a> {
    /// Methods that have a unique name — emit verbatim.
    unique_methods: Vec<ImplMethod<'a>>,
    /// Methods with the same name from different specializations — need dispatch.
    merged_methods: Vec<MergedMethod<'a>>,
}

#[derive(Debug)]
struct MergedMethod<'a> {
    name: String,
    /// Each variant: (type_constraint_description, concrete_type_arg, impl_method)
    variants: Vec<ImplMethod<'a>>,
}

fn analyze_class_impls<'a>(impls: &[&'a IrImpl]) -> ClassAnalysis<'a> {
    // Gather all methods keyed by name
    let mut by_name: BTreeMap<String, Vec<ImplMethod<'a>>> = BTreeMap::new();

    for imp in impls {
        let self_type_args: Vec<&IrType> = match &imp.self_ty {
            IrType::Struct { type_args, .. } => type_args.iter().collect(),
            _ => Vec::new(),
        };
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                let name = ts_method_name(&func.name, imp.trait_.as_ref());
                by_name.entry(name).or_default().push(ImplMethod {
                    func,
                    imp,
                    self_type_args: self_type_args.clone(),
                });
            }
        }
    }

    let mut unique_methods = Vec::new();
    let mut merged_methods = Vec::new();

    for (name, methods) in by_name {
        if methods.len() == 1 {
            unique_methods.push(methods.into_iter().next().unwrap());
        } else {
            // Check if they actually differ by type specialization
            // If all come from the same generic impl (no concrete type args),
            // just take the first one.
            let has_specialization = methods.iter().any(|m| {
                m.self_type_args
                    .iter()
                    .any(|t| !matches!(t, IrType::TypeParam(_)))
            });
            if has_specialization {
                merged_methods.push(MergedMethod {
                    name,
                    variants: methods,
                });
            } else {
                // All generic — just take the first
                unique_methods.push(methods.into_iter().next().unwrap());
            }
        }
    }

    ClassAnalysis {
        unique_methods,
        merged_methods,
    }
}

/// For a concrete type arg, return the runtime check expression.
/// E.g. `BitsInBytes` → `this.delta[0] instanceof BitsInBytes`.
fn runtime_type_check(ty: &IrType, struct_fields: &[IrField]) -> Option<String> {
    let class_name = match ty {
        IrType::Primitive(PrimitiveType::BitsInBytes) => "BitsInBytes",
        IrType::Primitive(PrimitiveType::BitsInBytes64) => "BitsInBytes64",
        IrType::Primitive(PrimitiveType::Galois) => "Galois",
        IrType::Primitive(PrimitiveType::Galois64) => "Galois64",
        IrType::Primitive(PrimitiveType::Bit) => "Bit",
        IrType::Primitive(PrimitiveType::Z3) => "Z3",
        IrType::Struct { kind, .. } => return Some(format!("/* {} */ true", kind)),
        _ => return None,
    };

    // Find the first field that uses the type parameter T to check against
    for field in struct_fields {
        if is_phantom_field(field) {
            continue;
        }
        match &field.ty {
            // Vec<T> field — check first element
            IrType::Vector { .. } => {
                return Some(format!("this.$f{}[0] instanceof {}", field.name, class_name));
            }
            // Vec<Vec<T>> field — check first element of first element
            IrType::Array { elem, .. } if matches!(elem.as_ref(), IrType::Vector { .. }) => {
                return Some(format!(
                    "this.$f{}[0]?.[0] instanceof {}",
                    field.name, class_name
                ));
            }
            _ => {}
        }
    }

    // Fallback: just use a comment
    Some(format!("/* T is {} */ true", class_name))
}

// ============================================================================
// STATEMENT-LIKE EXPRESSION DETECTION
// ============================================================================

/// Returns `true` if `expr` is a loop or other construct that cannot appear
/// after `return` in TypeScript (i.e. it's inherently a statement, not an
/// expression that produces a value).
fn is_statement_like(expr: &IrExpr) -> bool {
    match expr {
        IrExpr::BoundedLoop { .. } | IrExpr::IterLoop { .. } | IrExpr::WhileLoop { .. } => true,
        // An `if` without an else branch, or where branches are statement-like
        IrExpr::If {
            else_branch: None, ..
        } => true,
        IrExpr::If {
            then_branch,
            else_branch: Some(eb),
            ..
        } => {
            // If the then-branch's tail expression is statement-like, the whole if is
            let then_is_stmt = then_branch
                .expr
                .as_ref()
                .map_or(true, |e| is_statement_like(e));
            let else_is_stmt = is_statement_like(eb);
            then_is_stmt && else_is_stmt
        }
        IrExpr::Block(b) => b.expr.as_ref().map_or(true, |e| is_statement_like(e)),
        IrExpr::Assign { .. } | IrExpr::AssignOp { .. } => true,
        _ => false,
    }
}

// ============================================================================
// PREAMBLE
// ============================================================================

/// Names of classes that come from the hand-written runtime and are imported
/// from `./index` unless the module already defines them.
const RUNTIME_CLASSES: &[&str] = &[
    "Bit", "Galois", "Galois64", "BitsInBytes", "BitsInBytes64", "Z3",
];

struct TsPreambleWriter<'a> {
    /// Struct/class names that are defined in this module (not imported).
    local_names: &'a std::collections::HashSet<String>,
}

impl<'a> TsBackend for TsPreambleWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, _cx: &TsContext<'_>) -> fmt::Result {
        writeln!(f, "// Auto-generated TypeScript from volar-spec")?;
        writeln!(
            f,
            "// Type-level lengths have been converted to runtime number witnesses"
        )?;
        writeln!(f)?;
        // Import helper functions and interfaces; skip class names that are generated locally.
        let imported_classes: Vec<&str> = RUNTIME_CLASSES
            .iter()
            .filter(|&&n| !self.local_names.contains(n))
            .copied()
            .collect();
        writeln!(f, "import {{")?;
        writeln!(f, "  type Cloneable,")?;
        writeln!(f, "  type FieldElement,")?;
        writeln!(f, "  type BlockEncrypt,")?;
        writeln!(f, "  type Digest,")?;
        writeln!(f, "  type LengthDoubler,")?;
        for cls in &imported_classes {
            writeln!(f, "  {},", cls)?;
        }
        writeln!(f, "  fieldAdd,")?;
        writeln!(f, "  fieldSub,")?;
        writeln!(f, "  fieldMul,")?;
        writeln!(f, "  fieldBitxor,")?;
        writeln!(f, "  fieldBitor,")?;
        writeln!(f, "  fieldBitand,")?;
        writeln!(f, "  fieldShl,")?;
        writeln!(f, "  fieldShr,")?;
        writeln!(f, "  fieldEq,")?;
        writeln!(f, "  fieldNe,")?;
        writeln!(f, "  ilog2,")?;
        writeln!(f, "  wrappingAdd,")?;
        writeln!(f, "  wrappingSub,")?;
        writeln!(f, "  asRefU8,")?;
        writeln!(f, "  u32_from_le_bytes,")?;
        writeln!(f, "  u64_from_le_bytes,")?;
        writeln!(f, "  u128_from_le_bytes,")?;
        writeln!(f, "}} from \"./index\";")?;
        writeln!(f)?;
        // Stub types for external sha3/digest types referenced from generated code.
        writeln!(f, "type Shake128 = any; type Shake256 = any; type Sha3_256 = any;")?;
        writeln!(f, "type DigestUpdate = any;")?;
        // Function aliases (re-exports from other modules in the spec).
        // aes128_encrypt is `aes::encrypt_block` re-exported under a different name.
        writeln!(f, "declare const aes128_encrypt: typeof encrypt_block;")?;
        writeln!(f)?;
        // Canonical Option/Result classes.
        writeln!(f, "class Some<T> {{ constructor(public _0: T) {{}} }}")?;
        writeln!(f, "class Ok<T> {{ constructor(public _0: T) {{}} }}")?;
        writeln!(f, "class Err<E = unknown> {{ constructor(public _0: E) {{}} }}")?;
        writeln!(f, "type Vec<T> = T[];")?;
        writeln!(f, "type Option<T> = T | undefined;")?;
        writeln!(f, "type Result<T, E = unknown> = T;")?;
        // Minimal-runtime clone: spread arrays, shallow-copy objects with prototype.
        writeln!(f, "function __clone<T>(x: T): T {{")?;
        writeln!(f, "  if (Array.isArray(x)) return ([...x] as unknown) as T;")?;
        writeln!(f, "  if (x !== null && typeof x === 'object') return Object.assign(Object.create(Object.getPrototypeOf(x)), x) as T;")?;
        writeln!(f, "  return x;")?;
        writeln!(f, "}}")?;
        writeln!(f, "function __zeroValue<T>(val: T): T {{")?;
        writeln!(f, "  if (typeof val === 'bigint') return 0n as any;")?;
        writeln!(f, "  if (Array.isArray(val)) return [] as any;")?;
        writeln!(f, "  if (val !== null && typeof val === 'object' && typeof (val as any).__zero === 'function') return (val as any).__zero();")?;
        writeln!(f, "  return val;")?;
        writeln!(f, "}}")?;
        writeln!(f, "function __take<T>(val: T, setter: (v: T) => void): T {{ setter(__zeroValue(val)); return val; }}")?;
        writeln!(f, "function __equals(a: any, b: any): boolean {{ return fieldEq(a, b); }}")?;
        writeln!(f)?;
        Ok(())
    }
}

// ============================================================================
// MODULE
// ============================================================================

struct TsModuleWriter<'a> {
    module: &'a IrModule<IrFunction>,
}

impl<'a> TsBackend for TsModuleWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        // Type aliases (e.g. `type Zq = u32` → `type Zq = bigint`)
        for ta in &self.module.type_aliases {
            ts_write_type_alias(f, ta, cx)?;
        }
        if !self.module.type_aliases.is_empty() {
            writeln!(f)?;
        }

        // Group impl items by self_ty struct name so we can merge into class bodies.
        let mut impl_groups: BTreeMap<String, Vec<&IrImpl>> = BTreeMap::new();
        for imp in &self.module.impls {
            let key = self_ty_name(&imp.self_ty);
            impl_groups.entry(key).or_default().push(imp);
        }

        // Emit structs with their collected methods
        for s in &self.module.structs {
            let name = s.kind.to_string();
            let impls = impl_groups.remove(&name).unwrap_or_default();
            TsClassWriter { s, impls: &impls }.ts_fmt(f, cx)?;
            writeln!(f)?;
        }

        // Enums → tagged union types
        for e in &self.module.enums {
            ts_write_enum(f, e, cx)?;
            writeln!(f)?;
        }

        // Emit const declarations after classes/enums so that forward references
        // (e.g. `new U256(...)`) resolve to already-declared classes.
        // Deduplicate by name: merged modules may contain the same const multiple times.
        {
            let mut seen_consts: std::collections::HashSet<&str> = std::collections::HashSet::new();
            let mut any = false;
            for c in &self.module.consts {
                if seen_consts.insert(c.name.as_str()) {
                    write!(f, "export const {} = ", c.name)?;
                    TsExprWriter { expr: &c.value }.ts_fmt(f, cx)?;
                    writeln!(f, ";")?;
                    any = true;
                }
            }
            if any {
                writeln!(f)?;
            }
        }

        // Group standalone functions by name; merge duplicates with arg-count dispatch.
        let mut fn_groups: BTreeMap<String, Vec<&IrFunction>> = BTreeMap::new();
        for func in &self.module.functions {
            let fn_name = if func.name.starts_with("r#") {
                func.name[2..].to_string()
            } else {
                func.name.clone()
            };
            fn_groups.entry(fn_name).or_default().push(func);
        }
        for (fn_name, variants) in &fn_groups {
            let empty = WitnessNeeds::default();
            let needs = cx.get_witness_needs(&fn_name).unwrap_or(&empty);
            if variants.len() == 1 {
                TsFunctionWriter {
                    func: variants[0],
                    indent: 0,
                    witness_needs: needs,
                }
                .ts_fmt(f, cx)?;
            } else {
                // Multiple definitions — emit a merged dispatcher
                TsMergedFunctionWriter {
                    name: fn_name,
                    variants,
                    witness_needs: needs,
                }
                .ts_fmt(f, cx)?;
            }
            writeln!(f)?;
        }

        // Any impls whose struct isn't in this module — emit as standalone functions.
        // Group by method name and merge duplicates.
        let mut orphan_fns: BTreeMap<String, Vec<&IrFunction>> = BTreeMap::new();
        for (struct_name, impls) in &impl_groups {
            for imp in impls {
                for item in &imp.items {
                    if let IrImplItem::Method(func) = item {
                        let fn_name = if func.name.starts_with("r#") {
                            func.name[2..].to_string()
                        } else {
                            func.name.clone()
                        };
                        let _ = struct_name; // the struct may be the key for witness lookup
                        orphan_fns.entry(fn_name).or_default().push(func);
                    }
                }
            }
        }
        for (fn_name, variants) in &orphan_fns {
            let empty = WitnessNeeds::default();
            let needs = cx.get_witness_needs(&fn_name).unwrap_or(&empty);
            if variants.len() == 1 {
                TsFunctionWriter { func: variants[0], indent: 0, witness_needs: needs }
                    .ts_fmt(f, cx)?;
            } else {
                TsMergedFunctionWriter { name: fn_name, variants, witness_needs: needs }
                    .ts_fmt(f, cx)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

// ============================================================================
// CLASS (struct + merged impls)
// ============================================================================

struct TsClassWriter<'a> {
    s: &'a IrStruct,
    impls: &'a [&'a IrImpl],
}

impl<'a> TsBackend for TsClassWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let name = self.s.kind.to_string();
        let struct_generics = &self.s.generics;
        let _used_params = struct_used_type_params(self.s);

        write!(f, "export class {}", name)?;
        // Don't prune class-level type params — methods may reference them
        TsGenericsWriter {
            generics: struct_generics,
            used_only: None,
        }
        .ts_fmt(f, cx)?;
        writeln!(f, " {{")?;

        // Constructor with non-phantom fields
        let fields: Vec<&IrField> = self
            .s
            .fields
            .iter()
            .filter(|field| !is_phantom_field(field))
            .collect();

        // Declare fields with definite assignment assertion.
        // Numeric (tuple) fields use bracket notation `[N]`; named fields use `.name`.
        for (idx, field) in fields.iter().enumerate() {
            let ts_name = ts_field_name(&field.name, idx);
            // Numeric field: `[N]!: T;`  named field: `name!: T;`
            if ts_name.starts_with('[') {
                write!(f, "  {}!: ", ts_name)?;
            } else {
                write!(f, "  {}!: ", ts_name)?;
            }
            TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
            writeln!(f, ";")?;
        }
        writeln!(f)?;

        if self.s.is_tuple {
            // Tuple struct: positional constructor  new Foo(val)
            // Parameters use `_N` names; field assignment uses bracket notation `this[N]`.
            write!(f, "  constructor(")?;
            for (i, field) in fields.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "_{}: ", i)?;
                TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
            }
            writeln!(f, ") {{")?;
            for (i, _field) in fields.iter().enumerate() {
                writeln!(f, "    this[{}] = _{};", i, i)?;
            }
            writeln!(f, "  }}")?;
        } else {
            // Named struct: object-initializer constructor  new Foo({ field: val })
            writeln!(f, "  constructor(init: {{ ")?;
            for (i, field) in fields.iter().enumerate() {
                let ts_name = ts_field_name(&field.name, i);
                let comma = if i + 1 < fields.len() { "," } else { "" };
                write!(f, "    {}: ", ts_name)?;
                TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
                writeln!(f, "{}", comma)?;
            }
            writeln!(f, "  }}) {{")?;
            writeln!(f, "    Object.assign(this, init);")?;
            writeln!(f, "  }}")?;
        }

        // Emit __zero(): zeroes out all fields and returns a new zeroed instance.
        // Called by __take/__zeroValue for class-typed values.
        writeln!(f, "  __zero(): this {{")?;
        if self.s.is_tuple {
            write!(f, "    return new (this.constructor as any)(")?;
            for (i, _) in fields.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "__zeroValue(this[{}])", i)?;
            }
            writeln!(f, ") as this;")?;
        } else {
            write!(f, "    return new (this.constructor as any)({{ ")?;
            for (i, field) in fields.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                let ts_name = ts_field_name(&field.name, i);
                write!(f, "{}: __zeroValue(this.{})", ts_name, ts_name)?;
            }
            writeln!(f, " }}) as this;")?;
        }
        writeln!(f, "  }}")?;

        // Run analysis to detect overlapping methods
        let analysis = analyze_class_impls(self.impls);
        // Build `ClassName<T, U>` string for Self references in method bodies.
        let self_type_str = {
            let type_params: Vec<&str> = self.s.generics.iter()
                .filter(|g| g.kind == IrGenericParamKind::Type)
                .map(|g| g.name.as_str())
                .collect();
            if type_params.is_empty() {
                name.clone()
            } else {
                format!("{}<{}>", name, type_params.join(", "))
            }
        };
        let cx_class = cx.with_self_type(&self_type_str);

        // Emit unique methods
        let class_name = name.clone();
        for im in &analysis.unique_methods {
            writeln!(f)?;
            let method_name = ts_method_name(&im.func.name, im.imp.trait_.as_ref());
            let key = format!("{}.{}", class_name, method_name);
            let empty = WitnessNeeds::default();
            let needs = cx.get_witness_needs(&key).unwrap_or(&empty);
            TsMethodWriter {
                func: im.func,
                imp: im.imp,
                struct_generics: &self.s.generics,
                indent: 1,
                witness_needs: needs,
            }
            .ts_fmt(f, &cx_class)?;
        }

        // Emit merged methods (runtime dispatch)
        for mm in &analysis.merged_methods {
            writeln!(f)?;
            // Merge witness needs from all variants (including transitive)
            let mut needs = WitnessNeeds::default();
            for v in &mm.variants {
                let method_name = ts_method_name(&v.func.name, v.imp.trait_.as_ref());
                let key = format!("{}.{}", class_name, method_name);
                if let Some(v_needs) = cx.get_witness_needs(&key) {
                    needs.merge(v_needs);
                }
            }
            needs.sorted();
            TsMergedMethodWriter {
                merged: mm,
                struct_fields: &self.s.fields,
                struct_generics: &self.s.generics,
                indent: 1,
                witness_needs: &needs,
            }
            .ts_fmt(f, &cx_class)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

/// Returns true if `ty` contains a `Self` type parameter.
fn ir_type_uses_self_ref(ty: &IrType) -> bool {
    match ty {
        IrType::TypeParam(name) => name == "Self",
        IrType::Struct { type_args, .. } => type_args.iter().any(ir_type_uses_self_ref),
        IrType::Vector { elem } => ir_type_uses_self_ref(elem),
        IrType::Array { elem, .. } => ir_type_uses_self_ref(elem),
        IrType::Tuple(elems) => elems.iter().any(ir_type_uses_self_ref),
        IrType::Reference { elem, .. } => ir_type_uses_self_ref(elem),
        IrType::Projection { base, .. } => ir_type_uses_self_ref(base),
        _ => false,
    }
}

/// Returns true if `ty` contains any reference to a type param whose name is in `params`.
fn ir_type_uses_any_param(ty: &IrType, params: &std::collections::HashSet<&str>) -> bool {
    match ty {
        IrType::TypeParam(name) => params.contains(name.as_str()),
        IrType::Struct { type_args, .. } => type_args.iter().any(|a| ir_type_uses_any_param(a, params)),
        IrType::Vector { elem } => ir_type_uses_any_param(elem, params),
        IrType::Array { elem, .. } => ir_type_uses_any_param(elem, params),
        IrType::Tuple(elems) => elems.iter().any(|a| ir_type_uses_any_param(a, params)),
        IrType::Reference { elem, .. } => ir_type_uses_any_param(elem, params),
        IrType::Projection { base, .. } => ir_type_uses_any_param(base, params),
        _ => false,
    }
}

// ============================================================================
// METHOD (single, inside a class)
// ============================================================================

struct TsMethodWriter<'a> {
    func: &'a IrFunction,
    imp: &'a IrImpl,
    struct_generics: &'a [IrGenericParam],
    indent: usize,
    witness_needs: &'a WitnessNeeds,
}

impl<'a> TsMethodWriter<'a> {}

impl<'a> TsBackend for TsMethodWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let name = ts_method_name(&self.func.name, self.imp.trait_.as_ref());

        let is_static = self.func.receiver.is_none();
        // Static methods: keep the full self_type (including type params) so that `Self` in
        // return types expands to the full generic class name (e.g. `Foo<T>`). The class-level
        // type params that appear in static methods are re-declared as method-level type params
        // below, so they are valid in static scope.
        let cx_static;
        let cx = if is_static {
            if cx.self_type.is_some() {
                cx_static = TsContext {
                    witness_map: cx.witness_map,
                    name_collisions: cx.name_collisions,
                    erased_type_params: cx.erased_type_params.clone(),
                    self_type: cx.self_type.clone(),
                    tuple_structs: cx.tuple_structs,
                    class_witnesses: cx.class_witnesses.clone(),
                    mut_refs: cx.mut_refs.clone(),
                    enum_names: cx.enum_names,
                    var_types: core::cell::RefCell::new(cx.var_types.borrow().clone()),
                };
                &cx_static
            } else {
                cx
            }
        } else {
            cx
        };
        if is_static {
            write!(f, "{}static {}", ind, name)?;
        } else {
            write!(f, "{}{}", ind, name)?;
        }

        let used_params = function_used_type_params(self.func);

        // Class-level type params (from non-trait impl's generics). These are the params
        // declared on the surrounding `class Foo<T>`. In TypeScript:
        //   - Instance methods: T is already in scope → don't re-declare it at method level
        //     (re-declaring shadows the class T, causing TS2719).
        //   - Static methods: T is NOT in scope → must declare it at method level, otherwise
        //     TypeScript emits TS2302 ("static members cannot reference class type parameters").
        // Class-level type params: those that appear both in impl generics AND in the struct's
        // own generics. For non-trait impls this is all impl type generics. For trait impls we
        // check against struct_generics so that e.g. `impl<T> Clone for DeltaDyn<T>` treats T
        // as a class-level param (avoiding TS2719 from `clone<T>` shadowing the class T).
        let struct_param_names: std::collections::HashSet<&str> = self.struct_generics
            .iter()
            .filter(|g| g.kind == IrGenericParamKind::Type)
            .map(|g| g.name.as_str())
            .collect();
        let class_type_params: std::collections::HashSet<&str> = self.imp.generics.iter()
            .filter(|g| {
                g.kind == IrGenericParamKind::Type
                    && (self.imp.trait_.is_none() || struct_param_names.contains(g.name.as_str()))
            })
            .map(|g| g.name.as_str())
            .collect();

        // When `Self` appears in the return type or params, all impl-level type params are
        // implicitly used (TypeParam("Self") is excluded by collect_type_param_refs_inner).
        // Extend used_params to include all imp generics if Self is referenced.
        let self_referenced = {
            let mut types: Vec<&IrType> = self.func.params.iter().map(|p| &p.ty).collect();
            if let Some(ret) = &self.func.return_type { types.push(ret); }
            types.iter().any(|t| ir_type_uses_self_ref(t))
        };
        let mut effective_used_params: Vec<String> = used_params.clone();
        if self_referenced {
            for g in &self.imp.generics {
                if g.kind == IrGenericParamKind::Type {
                    let s = g.name.clone();
                    if !effective_used_params.contains(&s) {
                        effective_used_params.push(s);
                    }
                }
            }
        }

        let mut all_fn_generics: Vec<IrGenericParam>;
        if !is_static && !class_type_params.is_empty() {
            // Instance method on a generic class: remove func-level generics that shadow class params.
            all_fn_generics = self.func.generics.iter()
                .filter(|g| !class_type_params.contains(g.name.as_str()))
                .cloned()
                .collect();
        } else {
            // Static method or trait impl: use original func generics, then merge impl-level ones.
            all_fn_generics = self.func.generics.clone();
            for g in &self.imp.generics {
                if g.kind == IrGenericParamKind::Type
                    && effective_used_params.contains(&g.name)
                    && !all_fn_generics.iter().any(|eg| eg.name == g.name)
                    && !self.witness_needs.needs.iter().any(|w| {
                        matches!(w, WitnessKind::Class { type_param } if type_param == &g.name)
                    })
                {
                    all_fn_generics.push(g.clone());
                }
            }
        }

        TsGenericsWriter {
            generics: &all_fn_generics,
            used_only: Some(&effective_used_params),
        }
        .ts_fmt(f, cx)?;

        write!(f, "(")?;
        let mut first_param = true;
        // Emit ctx parameter if we have witness needs
        if !self.witness_needs.is_empty() {
            write_ctx_param(self.witness_needs, f)?;
            first_param = false;
        }
        let params: Vec<&IrParam> = self.func.params.iter().collect();
        for p in params.iter() {
            if !first_param {
                write!(f, ", ")?;
            }
            first_param = false;
            write!(f, "{}: ", ts_param_name(&p.name))?;
            write_param_type(p, &self.func.generics, f, cx)?;
        }
        write!(f, ")")?;

        // For instance methods only: suppress the return type annotation when it references the
        // class's type params. The body constructs a concrete type that doesn't match the generic
        // T, which TypeScript rejects as TS2322. Without the annotation TypeScript infers the
        // return type from the body.
        let suppress_ret = if !is_static && !class_type_params.is_empty() {
            self.func.return_type.as_ref()
                .map(|r| ir_type_uses_any_param(r, &class_type_params) || ir_type_uses_self_ref(r))
                .unwrap_or(false)
        } else {
            false
        };

        if let Some(ret) = &self.func.return_type {
            if !suppress_ret {
                write!(f, ": ")?;
                TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
            }
        }

        writeln!(f)?;
        // Extend context with class witnesses active in this method body.
        let class_wit_names: Vec<String> = self.witness_needs.needs.iter()
            .filter_map(|k| if let WitnessKind::Class { type_param } = k { Some(type_param.clone()) } else { None })
            .collect();
        let cx_fn = if class_wit_names.is_empty() { None } else { Some(cx.with_class_witnesses(class_wit_names)) };
        let cx_body = cx_fn.as_ref().map(|c| c as &TsContext<'_>).unwrap_or(cx);
        TsBlockWriter {
            block: &self.func.body,
            indent: self.indent,
        }
        .ts_fmt(f, cx_body)?;
        writeln!(f)?;
        Ok(())
    }
}

// ============================================================================
// MERGED METHOD (runtime dispatch over type-specialized impls)
// ============================================================================

struct TsMergedMethodWriter<'a> {
    merged: &'a MergedMethod<'a>,
    struct_fields: &'a [IrField],
    struct_generics: &'a [IrGenericParam],
    indent: usize,
    witness_needs: &'a WitnessNeeds,
}

impl<'a> TsBackend for TsMergedMethodWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let ind2 = "  ".repeat(self.indent + 1);

        // Use the first variant to determine the signature shape
        let first = &self.merged.variants[0];

        let is_static = first.func.receiver.is_none();
        if is_static {
            write!(f, "{}static {}", ind, self.merged.name)?;
        } else {
            write!(f, "{}{}", ind, self.merged.name)?;
        }

        // Collect all generics across variants (union)
        let mut all_generics: Vec<&IrGenericParam> = Vec::new();
        for v in &self.merged.variants {
            for g in &v.func.generics {
                if !all_generics.iter().any(|eg| eg.name == g.name) {
                    all_generics.push(g);
                }
            }
        }
        // Filter for the generics writer
        let generics_owned: Vec<IrGenericParam> = all_generics
            .iter()
            .filter(|p| {
                p.kind == IrGenericParamKind::Type
                    && !p
                        .bounds
                        .iter()
                        .any(|b| matches!(&b.trait_kind, TraitKind::Fn(..)))
                    && !p
                        .bounds
                        .iter()
                        .any(|b| matches!(&b.trait_kind, TraitKind::AsRef(..)))
            })
            .map(|p| (*p).clone())
            .collect();
        TsGenericsWriter {
            generics: &generics_owned,
            used_only: None,
        }
        .ts_fmt(f, cx)?;

        // Parameters — use the broadest signature (first variant's params)
        write!(f, "(")?;
        let mut first_param = true;
        if !self.witness_needs.is_empty() {
            write_ctx_param(self.witness_needs, f)?;
            first_param = false;
        }
        let params: Vec<&IrParam> = first.func.params.iter().collect();
        for p in params.iter() {
            if !first_param {
                write!(f, ", ")?;
            }
            first_param = false;
            write!(f, "{}: ", ts_param_name(&p.name))?;
            write_param_type(p, &first.func.generics, f, cx)?;
        }
        write!(f, ")")?;

        // Return type — use first variant's, unless it references class-level type params
        // in which case suppress it (body returns concrete types which would fail TS2322).
        let class_params: std::collections::HashSet<&str> = self.struct_generics
            .iter()
            .filter(|g| g.kind == IrGenericParamKind::Type)
            .map(|g| g.name.as_str())
            .collect();
        // Suppress return type if it references class type params OR is Self
        // (Self expands to the class type which implicitly uses all class params).
        let suppress_ret = !is_static && !class_params.is_empty()
            && first.func.return_type.as_ref()
                .map(|r| ir_type_uses_any_param(r, &class_params) || ir_type_uses_self_ref(r))
                .unwrap_or(false);
        if !suppress_ret {
            if let Some(ret) = &first.func.return_type {
                write!(f, ": ")?;
                TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
            }
        }

        writeln!(f)?;
        writeln!(f, "{}{{", ind)?;

        // Emit if/else chain for each variant
        for (i, variant) in self.merged.variants.iter().enumerate() {
            let check = if let Some(ty) = variant.self_type_args.first() {
                runtime_type_check(ty, self.struct_fields).unwrap_or_else(|| "true".to_string())
            } else {
                "true".to_string()
            };

            if i == 0 {
                writeln!(f, "{}if ({}) {{", ind2, check)?;
            } else if i + 1 < self.merged.variants.len() {
                writeln!(f, "{}}} else if ({}) {{", ind2, check)?;
            } else {
                writeln!(f, "{}}} else {{", ind2)?;
            }

            // Emit the variant's body inline
            let body = &variant.func.body;
            let inner_ind = "  ".repeat(self.indent + 2);
            for stmt in &body.stmts {
                TsStmtWriter {
                    stmt,
                    indent: self.indent + 2,
                }
                .ts_fmt(f, cx)?;
            }
            if let Some(tail) = &body.expr {
                if is_statement_like(tail) {
                    write!(f, "{}", inner_ind)?;
                    TsExprWriter { expr: tail }.ts_fmt(f, cx)?;
                    writeln!(f, ";")?;
                } else {
                    write!(f, "{}return ", inner_ind)?;
                    TsExprWriter { expr: tail }.ts_fmt(f, cx)?;
                    writeln!(f, ";")?;
                }
            }
        }
        writeln!(f, "{}}}", ind2)?;

        writeln!(f, "{}}}", ind)?;
        Ok(())
    }
}

// ============================================================================
// FUNCTION (top-level)
// ============================================================================

struct TsFunctionWriter<'a> {
    func: &'a IrFunction,
    indent: usize,
    witness_needs: &'a WitnessNeeds,
}

impl<'a> TsBackend for TsFunctionWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let name = if self.func.name.starts_with("r#") {
            &self.func.name[2..]
        } else {
            &self.func.name
        };
        write!(f, "{}export function {}", ind, name)?;
        let used_params = function_used_type_params(self.func);
        TsGenericsWriter {
            generics: &self.func.generics,
            used_only: Some(&used_params),
        }
        .ts_fmt(f, cx)?;
        write!(f, "(")?;
        let mut first_param = true;
        if !self.witness_needs.is_empty() {
            write_ctx_param(self.witness_needs, f)?;
            first_param = false;
        }
        for p in self.func.params.iter() {
            if !first_param {
                write!(f, ", ")?;
            }
            first_param = false;
            write!(f, "{}: ", ts_param_name(&p.name))?;
            write_param_type(p, &self.func.generics, f, cx)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &self.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
        }
        writeln!(f)?;
        // Extend context with class witnesses active in this function body.
        let class_wit_names: Vec<String> = self.witness_needs.needs.iter()
            .filter_map(|k| if let WitnessKind::Class { type_param } = k { Some(type_param.clone()) } else { None })
            .collect();
        let cx_fn = if class_wit_names.is_empty() { None } else { Some(cx.with_class_witnesses(class_wit_names)) };
        let cx_body = cx_fn.as_ref().map(|c| c as &TsContext<'_>).unwrap_or(cx);
        TsBlockWriter {
            block: &self.func.body,
            indent: self.indent,
        }
        .ts_fmt(f, cx_body)?;
        writeln!(f)?;
        Ok(())
    }
}

// ============================================================================
// GENERICS
// ============================================================================

struct TsGenericsWriter<'a> {
    generics: &'a [IrGenericParam],
    /// If Some, only emit params whose names are in this set.
    used_only: Option<&'a [String]>,
}

impl<'a> TsBackend for TsGenericsWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, _cx: &TsContext<'_>) -> fmt::Result {
        let type_params: Vec<&IrGenericParam> = self
            .generics
            .iter()
            .filter(|p| {
                if p.kind != IrGenericParamKind::Type {
                    return false;
                }
                if p.bounds
                    .iter()
                    .any(|b| matches!(&b.trait_kind, TraitKind::Fn(..)))
                {
                    return false;
                }
                if p.bounds
                    .iter()
                    .any(|b| matches!(&b.trait_kind, TraitKind::AsRef(..)))
                {
                    return false;
                }
                // If we have a used_only filter, check membership
                if let Some(used) = self.used_only {
                    return used.contains(&p.name);
                }
                true
            })
            .collect();
        if type_params.is_empty() {
            return Ok(());
        }
        write!(f, "<")?;
        for (i, p) in type_params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", p.name)?;
        }
        write!(f, ">")?;
        Ok(())
    }
}

// ============================================================================
// MERGED FUNCTION (top-level, multiple definitions with the same name)
// ============================================================================

struct TsMergedFunctionWriter<'a> {
    name: &'a str,
    variants: &'a [&'a IrFunction],
    witness_needs: &'a WitnessNeeds,
}

impl<'a> TsBackend for TsMergedFunctionWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        // Emit a single function that dispatches on argument count.
        // Each variant gets a case matching its total param count.
        write!(f, "export function {}(...__args: any[]): any {{", self.name)?;
        writeln!(f)?;
        for (vi, variant) in self.variants.iter().enumerate() {
            let n_params = variant.params.len()
                + if self.witness_needs.is_empty() { 0 } else { 1 };
            write!(f, "  if (__args.length === {}) {{", n_params)?;
            writeln!(f)?;
            // Unpack args
            let mut idx = 0usize;
            if !self.witness_needs.is_empty() {
                writeln!(f, "    const ctx = __args[{}];", idx)?;
                idx += 1;
            }
            for p in &variant.params {
                writeln!(f, "    const {} = __args[{}];", ts_param_name(&p.name), idx)?;
                idx += 1;
            }
            // Emit body
            let class_wit_names: Vec<String> = self.witness_needs.needs.iter()
                .filter_map(|k| if let WitnessKind::Class { type_param } = k { Some(type_param.clone()) } else { None })
                .collect();
            let cx_fn = if class_wit_names.is_empty() { None } else { Some(cx.with_class_witnesses(class_wit_names)) };
            let cx_body = cx_fn.as_ref().map(|c| c as &TsContext<'_>).unwrap_or(cx);
            write!(f, "    return (() => ")?;
            TsBlockWriter { block: &variant.body, indent: 0 }.ts_fmt(f, cx_body)?;
            writeln!(f, ")();")?;
            write!(f, "  }}")?;
            if vi + 1 < self.variants.len() { write!(f, " else")?; }
            writeln!(f)?;
        }
        writeln!(f, "  throw new Error(\"{}(): no matching variant for \" + __args.length + \" args\");", self.name)?;
        writeln!(f, "}}")
    }
}

// ============================================================================
// TYPE
// ============================================================================

struct TsTypeWriter<'a> {
    ty: &'a IrType,
}

impl<'a> TsBackend for TsTypeWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        match self.ty {
            IrType::Primitive(p) => write!(f, "{}", ts_primitive(p))?,
            IrType::Vector { elem } => {
                TsTypeWriter { ty: elem }.ts_fmt(f, cx)?;
                write!(f, "[]")?;
            }
            IrType::Array { kind, elem, .. } => {
                // All array/slice types emit as T[] — `readonly` is omitted since
                // mutability is not tracked consistently in the lowered IR.
                let _ = kind;
                TsTypeWriter { ty: elem }.ts_fmt(f, cx)?;
                write!(f, "[]")?;
            }
            IrType::Struct { kind, type_args } => {
                let name = kind.to_string();
                if name == "Option" && type_args.len() == 1 {
                    write!(f, "(")?;
                    TsTypeWriter { ty: &type_args[0] }.ts_fmt(f, cx)?;
                    write!(f, " | undefined)")?;
                    return Ok(());
                }
                // `digest::Output<D>` / `Output<D>` — a fixed-length byte array.
                if name == "Output" {
                    return write!(f, "bigint[]");
                }
                write!(f, "{}", name)?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        TsTypeWriter { ty: arg }.ts_fmt(f, cx)?;
                    }
                    write!(f, ">")?;
                }
            }
            IrType::TypeParam(p) => {
                if p == "Self" {
                    if let Some(name) = &cx.self_type {
                        write!(f, "{}", name)?;
                    } else {
                        write!(f, "any")?;
                    }
                } else if cx.erased_type_params.contains(p) {
                    write!(f, "any")?;
                } else {
                    write!(f, "{}", p)?;
                }
            }
            IrType::Tuple(elems) => {
                write!(f, "[")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsTypeWriter { ty: elem }.ts_fmt(f, cx)?;
                }
                write!(f, "]")?;
            }
            IrType::Unit => write!(f, "void")?,
            IrType::Reference { elem, .. } => {
                TsTypeWriter { ty: elem }.ts_fmt(f, cx)?;
            }
            IrType::Projection { base, assoc, .. } => {
                // `Self::Output` / `T::Output` in arithmetic trait impls (Add, Mul, …)
                // almost always resolves to Self. Emit the class name when in impl context.
                if matches!(assoc, crate::ir::AssociatedType::Output) {
                    if let Some(self_ty) = &cx.self_type {
                        write!(f, "{}", self_ty)?;
                        return Ok(());
                    }
                }
                write!(f, "number /* ")?;
                TsTypeWriter { ty: base }.ts_fmt(f, cx)?;
                write!(f, "::{} */", assoc)?;
            }
            IrType::Existential { bounds } => {
                if let Some(b) = bounds.first() {
                    match &b.trait_kind {
                        TraitKind::Fn(input, ret) => {
                            let param_ty = match input {
                                FnInput::BytesSlice => "Uint8Array",
                                FnInput::Size => "number",
                                FnInput::Bool => "boolean",
                            };
                            write!(f, "(arg: {}) => ", param_ty)?;
                            TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
                            return Ok(());
                        }
                        TraitKind::AsRef(ty) => {
                            TsTypeWriter { ty }.ts_fmt(f, cx)?;
                            return Ok(());
                        }
                        _ => {}
                    }
                }
                write!(f, "unknown /* impl ")?;
                for (i, b) in bounds.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", b)?;
                }
                write!(f, " */")?;
            }
            IrType::FnPtr { params, ret } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "arg{}: ", i)?;
                    TsTypeWriter { ty: p }.ts_fmt(f, cx)?;
                }
                write!(f, ") => ")?;
                TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
            }
            IrType::Never => write!(f, "never")?,
            IrType::Infer => write!(f, "unknown")?,
            IrType::Param { path } => write!(f, "{}", path.join("."))?,
        }
        Ok(())
    }
}

// ============================================================================
// BLOCK — handles the statement/expression boundary correctly
// ============================================================================

struct TsBlockWriter<'a> {
    block: &'a IrBlock,
    indent: usize,
}

impl<'a> TsBackend for TsBlockWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        writeln!(f, "{}{{", ind)?;
        for stmt in &self.block.stmts {
            TsStmtWriter {
                stmt,
                indent: self.indent + 1,
            }
            .ts_fmt(f, cx)?;
        }
        if let Some(e) = &self.block.expr {
            let inner = "  ".repeat(self.indent + 1);
            if is_statement_like(e) {
                // Emit as a statement — do NOT prefix with `return`
                write!(f, "{}", inner)?;
                emit_statement_expr(e, self.indent + 1, f, cx)?;
                writeln!(f)?;
            } else {
                write!(f, "{}return ", inner)?;
                TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                writeln!(f, ";")?;
            }
        }
        write!(f, "{}}}", ind)?;
        Ok(())
    }
}

/// Emit an expression that is statement-like (loop, assignment, etc.) as a
/// proper statement. This avoids `return for(...)` in the output.
fn emit_statement_expr(
    e: &IrExpr,
    indent: usize,
    f: &mut fmt::Formatter<'_>,
    cx: &TsContext<'_>,
) -> fmt::Result {
    match e {
        IrExpr::BoundedLoop {
            var,
            start,
            end,
            inclusive,
            body,
        } => {
            // All numeric values are bigint; use `+= 1n` not `++`.
            write!(f, "for (let {} = ", var)?;
            TsExprWriter { expr: start }.ts_fmt(f, cx)?;
            write!(f, "; {} {} ", var, if *inclusive { "<=" } else { "<" })?;
            TsExprWriter { expr: end }.ts_fmt(f, cx)?;
            write!(f, "; {} += 1n) ", var)?;
            TsBlockWriter {
                block: body,
                indent,
            }
            .ts_fmt(f, cx)?;
        }
        IrExpr::IterLoop {
            pattern,
            collection,
            body,
        } => {
            // `iter_mut()`: lower to an indexed loop with mutable-reference semantics.
            // The {"*": value} shape is used conceptually; writes go through to the array
            // via index tracking (ad-hoc optimization: direct indexed write, not getter/setter).
            // Future: replace with getter/setter reference objects for general correctness.
            if let IrExpr::MethodCall { receiver, method, args, .. } = collection.as_ref() {
                if matches!(method, MethodKind::Other(s) if s == "iter_mut") && args.is_empty() {
                    let arr_str = format!("{}", TsFmt(TsExprWriter { expr: receiver }, cx));
                    let idx_var = format!("__mut_{}", indent);  // fresh per indent level
                    let var_name = match pattern {
                        IrPattern::Ident { name, .. } => name.clone(),
                        _ => "__mut_elem".to_string(),
                    };
                    let cx_body = cx.with_mut_ref(MutRef {
                        var: var_name,
                        array_expr: arr_str.clone(),
                        index_var: idx_var.clone(),
                    });
                    writeln!(f, "for (let {} = 0n; {} < BigInt({}.length); {} += 1n) {}",
                        idx_var, idx_var, arr_str, idx_var, "{")?;
                    TsBlockWriter { block: body, indent }.ts_fmt(f, &cx_body)?;
                    write!(f, "}}")?;
                    return Ok(());
                }
            }
            write!(f, "for (const ")?;
            TsPatternWriter { pat: pattern }.ts_fmt(f, cx)?;
            write!(f, " of ")?;
            TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
            write!(f, ") ")?;
            TsBlockWriter {
                block: body,
                indent,
            }
            .ts_fmt(f, cx)?;
        }
        IrExpr::WhileLoop { cond, body } => {
            write!(f, "while (")?;
            TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
            write!(f, ") ")?;
            TsBlockWriter { block: body, indent }.ts_fmt(f, cx)?;
        }
        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            write!(f, "if (")?;
            TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
            write!(f, ") ")?;
            TsBlockWriter {
                block: then_branch,
                indent,
            }
            .ts_fmt(f, cx)?;
            if let Some(eb) = else_branch {
                write!(f, " else ")?;
                match eb.as_ref() {
                    IrExpr::Block(b) => {
                        TsBlockWriter { block: b, indent }.ts_fmt(f, cx)?;
                    }
                    other if is_statement_like(other) => {
                        emit_statement_expr(other, indent, f, cx)?;
                    }
                    other => {
                        write!(f, "{{ ")?;
                        TsExprWriter { expr: other }.ts_fmt(f, cx)?;
                        write!(f, "; }}")?;
                    }
                }
            }
        }
        IrExpr::Block(b) => {
            TsBlockWriter { block: b, indent }.ts_fmt(f, cx)?;
        }
        IrExpr::Assign { left, right } => {
            // `*byte = expr` where byte is a mutable ref → `arr[Number(i)] = expr`
            if let IrExpr::Unary { op: SpecUnaryOp::Deref, expr: inner } = left.as_ref() {
                if let IrExpr::Var(v) = inner.as_ref() {
                    if let Some(r) = cx.find_mut_ref(v) {
                        let arr = r.array_expr.clone();
                        let idx = r.index_var.clone();
                        write!(f, "{}[Number({})] = ", arr, idx)?;
                        TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                        return write!(f, ";");
                    }
                }
            }
            TsExprWriter { expr: left }.ts_fmt(f, cx)?;
            write!(f, " = ")?;
            TsExprWriter { expr: right }.ts_fmt(f, cx)?;
            write!(f, ";")?;
        }
        IrExpr::AssignOp { op, left, right } => {
            // For ops that work on field elements (bitxor, shl, etc.), desugar
            // `x op= y` → `x = fieldOp(x, y)` so the helper can dispatch on type.
            if let Some(helper) = bin_op_helper(*op) {
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, " = {}(", helper)?;
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, ", ")?;
                TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                write!(f, ");")?;
            } else {
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, " {}= ", ts_bin_op(*op))?;
                TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                write!(f, ";")?;
            }
        }
        other => {
            // Fallback: just emit as expression statement
            TsExprWriter { expr: other }.ts_fmt(f, cx)?;
            write!(f, ";")?;
        }
    }
    Ok(())
}

// ============================================================================
// STATEMENT
// ============================================================================

struct TsStmtWriter<'a> {
    stmt: &'a IrStmt,
    indent: usize,
}

impl<'a> TsBackend for TsStmtWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        match self.stmt {
            IrStmt::Let { pattern, ty, init } => {
                // Track `let x: T = ...` bindings so size_of_val(x) → ctx.sizeOfT.
                if let Some(IrType::TypeParam(tp)) = ty {
                    collect_pattern_var_types(pattern, tp, cx);
                }
                let is_mutable = pattern_is_mutable(pattern);
                let kw = if is_mutable { "let" } else { "const" };
                write!(f, "{}{} ", ind, kw)?;
                TsPatternWriter { pat: pattern }.ts_fmt(f, cx)?;
                if let Some(t) = ty {
                    write!(f, ": ")?;
                    TsTypeWriter { ty: t }.ts_fmt(f, cx)?;
                }
                if let Some(i) = init {
                    write!(f, " = ")?;
                    // `let x: T[] = y.0` — `.0` on a Vec/Array lowered type is transparent:
                    // the wrapper struct was erased, so `y` IS the array. Skip the `[0]`.
                    let is_vec_binding = matches!(ty,
                        Some(IrType::Vector { .. }) | Some(IrType::Array { .. }));
                    let init_is_field_zero = matches!(i, IrExpr::Field { field, .. } if field == "0");
                    if is_vec_binding && init_is_field_zero {
                        if let IrExpr::Field { base, .. } = i {
                            TsExprWriter { expr: base }.ts_fmt(f, cx)?;
                        } else {
                            TsExprWriter { expr: i }.ts_fmt(f, cx)?;
                        }
                    } else {
                        TsExprWriter { expr: i }.ts_fmt(f, cx)?;
                    }
                }
                writeln!(f, ";")?;
            }
            IrStmt::Semi(e) => {
                // Statement-like expressions get special treatment
                if is_statement_like(e) {
                    write!(f, "{}", ind)?;
                    emit_statement_expr(e, self.indent, f, cx)?;
                    writeln!(f)?;
                } else {
                    write!(f, "{}", ind)?;
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                    writeln!(f, ";")?;
                }
            }
            IrStmt::Expr(e) => {
                if is_statement_like(e) {
                    write!(f, "{}", ind)?;
                    emit_statement_expr(e, self.indent, f, cx)?;
                    writeln!(f)?;
                } else {
                    write!(f, "{}", ind)?;
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                    writeln!(f, ";")?;
                }
            }
        }
        Ok(())
    }
}

// ============================================================================
// EXPRESSION
// ============================================================================

struct TsExprWriter<'a> {
    expr: &'a IrExpr,
}

impl<'a> TsBackend for TsExprWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        match self.expr {
            IrExpr::Lit(l) => ts_literal(l, f)?,
            IrExpr::Var(v) => {
                let name = if v == "self" {
                    "this".to_string()
                } else if v == "None" {
                    "undefined".to_string()
                } else if cx.class_witnesses.contains(v) {
                    // Type param used as a value → class witness reference
                    format!("ctx.{}Class", v)
                } else {
                    escape_ts_reserved(v)
                };
                write!(f, "{}", name)?;
            }
            IrExpr::Binary { op, left, right } => {
                match op {
                    SpecBinOp::Eq => {
                        write!(f, "__equals(")?;
                        TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                        write!(f, ", ")?;
                        TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                        return write!(f, ")");
                    }
                    SpecBinOp::Ne => {
                        write!(f, "!__equals(")?;
                        TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                        write!(f, ", ")?;
                        TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                        return write!(f, ")");
                    }
                    _ => {}
                }
                if let Some(helper) = bin_op_helper(*op) {
                    write!(f, "{}(", helper)?;
                    TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                    write!(f, ", ")?;
                    TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                } else {
                    write!(f, "(")?;
                    TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                    write!(f, " {} ", ts_bin_op(*op))?;
                    TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
            }
            IrExpr::Unary { op, expr } => match op {
                SpecUnaryOp::Neg => {
                    write!(f, "-")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                }
                SpecUnaryOp::Not => {
                    write!(f, "!")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                }
                SpecUnaryOp::Deref => {
                    // Deref a mutable reference `{"*": v}` → emit `var["*"]`
                    if let IrExpr::Var(v) = expr.as_ref() {
                        if let Some(r) = cx.find_mut_ref(v) {
                            return write!(f, "{}[Number({})]", r.array_expr, r.index_var);
                        }
                    }
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                }
                SpecUnaryOp::Ref | SpecUnaryOp::RefMut => {
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                }
            },
            IrExpr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                emit_method_call(receiver, method, args, f, cx)?;
            }
            IrExpr::Call { func, args } => {
                // Check for Some(x)/None via Path or Var
                if let IrExpr::Path { segments, .. } = func.as_ref() {
                    if segments.len() == 1 {
                        let name = &segments[0];
                        match name.as_str() {
                            "Some" if args.len() == 1 => {
                                // Transparent: Some(x) = x (None = undefined)
                                return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                            }
                            "None" if args.is_empty() => return write!(f, "undefined"),
                            "Ok" if args.len() == 1 => {
                                return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                            }
                            "Err" if args.len() == 1 => {
                                return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                            }
                            // Vec<T> = T[]; coercion is identity
                            "Vec" if args.len() == 1 => {
                                return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                            }
                            _ if is_primitive_class(name) || cx.is_tuple_struct(name) => {
                                write!(f, "new {}(", name)?;
                                let mut first = true;
                                for arg in args.iter().filter(|a| !is_phantom_arg(a)) {
                                    if !first { write!(f, ", ")?; }
                                    first = false;
                                    TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                                }
                                return write!(f, ")");
                            }
                            _ => {}
                        }
                    }
                }
                if let IrExpr::Var(v) = func.as_ref() {
                    match v.as_str() {
                        "Some" if args.len() == 1 => {
                            return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                        }
                        "None" if args.is_empty() => return write!(f, "undefined"),
                        "Ok" if args.len() == 1 => {
                            return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                        }
                        "Err" if args.len() == 1 => {
                            return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                        }
                        // `Vec(x)` — Vec<T> = T[], coercion is identity
                        "Vec" if args.len() == 1 => {
                            return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                        }
                        // size_of_val(p) → ctx.sizeOfT (looked up from var_types)
                        "size_of_val" | "size_of" => {
                            let tp = if !args.is_empty() {
                                let inner = unwrap_ref_expr(&args[0]);
                                if let IrExpr::Var(name) = inner { cx.lookup_var_type(name) } else { None }
                            } else { None };
                            return if let Some(tp) = tp {
                                write!(f, "ctx.sizeOf{}", tp)
                            } else {
                                write!(f, "ctx.sizeOf_unknown")
                            };
                        }
                        _ => {}
                    }
                    // Tuple struct constructors → new ClassName(args...)
                    if is_primitive_class(v) || cx.is_tuple_struct(v) {
                        write!(f, "new {}(", v)?;
                        let mut first = true;
                        for arg in args.iter().filter(|a| !is_phantom_arg(a)) {
                            if !first { write!(f, ", ")?; }
                            first = false;
                            TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                        }
                        return write!(f, ")");
                    }
                }
                if let IrExpr::Path { segments, .. } = func.as_ref() {
                    if segments.len() == 2 {
                        // Trait-qualified calls on external traits: `DigestUpdate::update(h, x)`
                        // → `h.update(x)`. The first arg becomes the receiver.
                        let is_trait_namespace = matches!(
                            segments[0].as_str(),
                            "DigestUpdate" | "Digest" | "Into"
                        );
                        if is_trait_namespace && !args.is_empty() {
                            let method = &segments[1];
                            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
                            write!(f, ".{}(", method)?;
                            for (i, arg) in args[1..].iter().enumerate() {
                                if i > 0 { write!(f, ", ")?; }
                                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                            }
                            return write!(f, ")");
                        }
                        // Resolve `Self` to the current class name.
                        let self_resolved;
                        let type_name = if segments[0] == "Self" {
                            if let Some(st) = &cx.self_type {
                                self_resolved = st.split('<').next().unwrap_or(st).to_string();
                                &self_resolved
                            } else {
                                &segments[0]
                            }
                        } else {
                            &segments[0]
                        };
                        let method = &segments[1];
                        let is_type_param = is_crypto_type_param(type_name)
                            || cx.class_witnesses.iter().any(|w| w == type_name.as_str());
                        if is_type_param {
                            match method.as_str() {
                                "new" => {
                                    // T::new() → ctx.newT()  (Constructor witness, backward compat)
                                    write!(f, "ctx.new{}(", type_name)?;
                                    for (i, arg) in args.iter().enumerate() {
                                        if i > 0 { write!(f, ", ")?; }
                                        TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                                    }
                                    return write!(f, ")");
                                }
                                "default" => {
                                    // T::default() → ctx.defaultT()  (Default witness, backward compat)
                                    return write!(f, "ctx.default{}()", type_name);
                                }
                                _ => {
                                    // T::method() → ctx.TClass.method()  (Class witness)
                                    write!(f, "ctx.{}Class.{}(", type_name, method)?;
                                    for (i, arg) in args.iter().enumerate() {
                                        if i > 0 { write!(f, ", ")?; }
                                        TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                                    }
                                    return write!(f, ")");
                                }
                            }
                        }
                    }
                    // Check if target is an internal function that needs ctx forwarding
                    if segments.len() == 1 {
                        let target_name = &segments[0];
                        if cx.get_witness_needs(target_name).is_some() {
                            write!(f, "{}(ctx", target_name)?;
                            for arg in args.iter().filter(|a| !is_phantom_arg(a)) {
                                write!(f, ", ")?;
                                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                            }
                            return write!(f, ")");
                        }
                    }
                    // size_of_val(p) / size_of::<T>() → ctx.sizeOfT
                    {
                        let filtered: Vec<&str> = segments.iter()
                            .map(|s| s.as_str())
                            .filter(|s| !matches!(*s, "super"|"crate"|"self"|"mem"|"std"|"core"|"alloc"))
                            .collect();
                        if matches!(filtered.as_slice(), ["size_of_val"] | ["size_of"]) {
                            // Try to resolve from var_types; fall back to first SizeOf witness
                            let tp = if !args.is_empty() {
                                let inner = unwrap_ref_expr(&args[0]);
                                if let IrExpr::Var(name) = inner {
                                    cx.lookup_var_type(name)
                                } else { None }
                            } else { None };
                            if let Some(tp) = tp {
                                return write!(f, "ctx.sizeOf{}", tp);
                            }
                            // Fallback: emit first SizeOf witness from the current function
                            // (works when there's only one type param, which is the common case)
                            return write!(f, "ctx.sizeOf_unknown");
                        }
                    }
                    // mem::take(expr) → __take(expr, y => expr = y)
                    {
                        let filtered: Vec<&str> = segments.iter()
                            .map(|s| s.as_str())
                            .filter(|s| !matches!(*s, "super" | "crate" | "self" | "mem" | "std" | "core" | "alloc"))
                            .collect();
                        if filtered == ["take"] && !args.is_empty() {
                            let inner = unwrap_ref_expr(&args[0]);
                            write!(f, "__take(")?;
                            TsExprWriter { expr: inner }.ts_fmt(f, cx)?;
                            write!(f, ", y => ")?;
                            TsExprWriter { expr: inner }.ts_fmt(f, cx)?;
                            return write!(f, " = y)");
                        }
                    }
                    // Vec::new() with no args → [] (an empty call [] () is not valid TS)
                    {
                        let segs: Vec<&str> = segments.iter()
                            .map(|s| s.as_str())
                            .filter(|s| !matches!(*s, "super" | "crate" | "self"))
                            .collect();
                        if segs.len() == 2 && segs[0] == "Vec" && segs[1] == "new" && args.is_empty() {
                            return write!(f, "[]");
                        }
                        // Enum::Variant(...) → EnumName_VariantName(...) factory function
                        if segs.len() == 2 && cx.enum_names.contains(segs[0]) {
                            write!(f, "{}_{}", segs[0], segs[1])?;
                            write!(f, "(")?;
                            let mut first = true;
                            for arg in args.iter().filter(|a| !is_phantom_arg(a)) {
                                if !first { write!(f, ", ")?; }
                                first = false;
                                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                            }
                            return write!(f, ")");
                        }
                    }
                }
                TsExprWriter { expr: func }.ts_fmt(f, cx)?;
                write!(f, "(")?;
                let mut first = true;
                for arg in args.iter().filter(|a| !is_phantom_arg(a)) {
                    if !first { write!(f, ", ")?; }
                    first = false;
                    TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                }
                write!(f, ")")?;
            }
            IrExpr::Field { base, field } => {
                TsExprWriter { expr: base }.ts_fmt(f, cx)?;
                write!(f, "{}", ts_field_access(field))?;
            }
            IrExpr::Index { base, index } => {
                // TODO: Slice operations should be represented as a dedicated
                // IrExpr::Slice variant in the IR rather than Index+Range.
                // For now, detect Index with a Range index and emit .slice().
                if let IrExpr::Range {
                    start,
                    end,
                    inclusive,
                } = index.as_ref()
                {
                    // .slice() takes number args — wrap bigint with Number()
                    TsExprWriter { expr: base }.ts_fmt(f, cx)?;
                    write!(f, ".slice(")?;
                    if let Some(s) = start {
                        write!(f, "Number(")?;
                        TsExprWriter { expr: s }.ts_fmt(f, cx)?;
                        write!(f, ")")?;
                    } else {
                        write!(f, "0")?;
                    }
                    if let Some(e) = end {
                        write!(f, ", Number(")?;
                        TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                        if *inclusive {
                            write!(f, " + 1n")?;
                        }
                        write!(f, ")")?;
                    }
                    write!(f, ")")?;
                } else {
                    TsExprWriter { expr: base }.ts_fmt(f, cx)?;
                    write!(f, "[Number(")?;
                    TsExprWriter { expr: index }.ts_fmt(f, cx)?;
                    write!(f, ")]")?;
                }
            }
            IrExpr::Path { segments, .. } => {
                // Single-segment path that is a class witness → ctx.TClass
                if segments.len() == 1 && cx.class_witnesses.contains(&segments[0]) {
                    write!(f, "ctx.{}Class", &segments[0])?;
                } else {
                    // Replace `Self` in path segments with the current class name.
                    let resolved: Vec<String> = segments.iter().map(|s| {
                        if s == "Self" {
                            cx.self_type.as_deref().unwrap_or("Self").to_string()
                        } else {
                            s.clone()
                        }
                    }).collect();
                    emit_path(&resolved, f)?;
                }
            }
            IrExpr::StructExpr { kind, fields, .. } => {
                // Strip Rust module prefixes (super::, crate::, self::) from type names.
                let raw = kind.to_string();
                let base = raw.rsplit("::").next().unwrap_or(raw.as_str());
                // `Self` in a struct expression → the current class name.
                let name = if base == "Self" {
                    cx.self_type.as_deref().unwrap_or("Self").to_string()
                } else {
                    base.to_string()
                };
                let real_fields: Vec<&(String, IrExpr)> = fields
                    .iter()
                    .filter(|(n, v)| {
                        !n.starts_with("_phantom") && !is_phantom_arg(v)
                    })
                    .collect();
                write!(f, "new {}({{ ", name)?;
                for (i, (field_name, val)) in real_fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "$f{}: ", field_name)?;
                    TsExprWriter { expr: val }.ts_fmt(f, cx)?;
                }
                write!(f, " }})")?;
            }
            IrExpr::Tuple(elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                }
                write!(f, "]")?;
            }
            IrExpr::Array(elems) | IrExpr::FixedArray(elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                }
                write!(f, "]")?;
            }
            IrExpr::Repeat { elem, len } => {
                write!(f, "Array.from({{length: Number(")?;
                TsExprWriter { expr: len }.ts_fmt(f, cx)?;
                write!(f, ")}}, () => ")?;
                TsExprWriter { expr: elem }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IrExpr::Block(b) => {
                // Expression-position block → IIFE
                write!(f, "(() => ")?;
                TsBlockWriter {
                    block: b,
                    indent: 0,
                }
                .ts_fmt(f, cx)?;
                write!(f, ")()")?;
            }
            IrExpr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                // If in expression position → IIFE with if/else
                write!(f, "(() => {{ if (")?;
                TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
                write!(f, ") ")?;
                TsBlockWriter {
                    block: then_branch,
                    indent: 0,
                }
                .ts_fmt(f, cx)?;
                if let Some(eb) = else_branch {
                    write!(f, " else ")?;
                    match eb.as_ref() {
                        IrExpr::Block(b) => {
                            TsBlockWriter {
                                block: b,
                                indent: 0,
                            }
                            .ts_fmt(f, cx)?;
                        }
                        IrExpr::If { .. } => {
                            // Nested if-else — recurse but strip IIFE wrapper
                            emit_if_chain(eb, f, cx)?;
                        }
                        _ => {
                            write!(f, "{{ return ")?;
                            TsExprWriter { expr: eb }.ts_fmt(f, cx)?;
                            write!(f, "; }}")?;
                        }
                    }
                }
                write!(f, " }})()")?;
            }
            // Loops as expressions — wrap in IIFE when they appear in expr context
            IrExpr::BoundedLoop {
                var,
                start,
                end,
                inclusive,
                body,
            } => {
                write!(f, "for (let {} = ", var)?;
                TsExprWriter { expr: start }.ts_fmt(f, cx)?;
                write!(f, "; {} {} ", var, if *inclusive { "<=" } else { "<" })?;
                TsExprWriter { expr: end }.ts_fmt(f, cx)?;
                write!(f, "; {} += 1n) ", var)?;
                TsBlockWriter {
                    block: body,
                    indent: 0,
                }
                .ts_fmt(f, cx)?;
            }
            IrExpr::IterLoop {
                pattern,
                collection,
                body,
            } => {
                write!(f, "for (const ")?;
                TsPatternWriter { pat: pattern }.ts_fmt(f, cx)?;
                write!(f, " of ")?;
                TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
                write!(f, ") ")?;
                TsBlockWriter {
                    block: body,
                    indent: 0,
                }
                .ts_fmt(f, cx)?;
            }
            IrExpr::WhileLoop { cond, body } => {
                write!(f, "while (")?;
                TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
                write!(f, ") ")?;
                TsBlockWriter { block: body, indent: 0 }.ts_fmt(f, cx)?;
            }
            IrExpr::IterPipeline(chain) => {
                TsIterChainWriter { chain }.ts_fmt(f, cx)?;
            }
            IrExpr::RawMap {
                receiver,
                elem_var,
                body,
            } => {
                TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
                write!(f, ".map((")?;
                TsPatternWriter { pat: elem_var }.ts_fmt(f, cx)?;
                write!(f, ": any) => ")?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IrExpr::RawZip {
                left,
                right,
                left_var,
                right_var,
                body,
            } => {
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, ".map((")?;
                TsPatternWriter { pat: left_var }.ts_fmt(f, cx)?;
                write!(f, ", __i) => {{ const ")?;
                TsPatternWriter { pat: right_var }.ts_fmt(f, cx)?;
                write!(f, " = ")?;
                TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                write!(f, "[__i]; return ")?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, "; }})")?;
            }
            IrExpr::RawFold {
                receiver,
                init,
                acc_var,
                elem_var,
                body,
            } => {
                TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
                write!(f, ".reduce((")?;
                TsPatternWriter { pat: acc_var }.ts_fmt(f, cx)?;
                write!(f, ": any, ")?;
                TsPatternWriter { pat: elem_var }.ts_fmt(f, cx)?;
                write!(f, ": any) => ")?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, ", ")?;
                TsExprWriter { expr: init }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IrExpr::Closure { params, body, .. } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsPatternWriter { pat: &p.pattern }.ts_fmt(f, cx)?;
                    if let Some(t) = &p.ty {
                        write!(f, ": ")?;
                        TsTypeWriter { ty: t }.ts_fmt(f, cx)?;
                    }
                }
                write!(f, ") => ")?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
            }
            IrExpr::Range {
                start,
                end,
                inclusive,
            } => {
                // Range → Array.from with bigint elements
                write!(f, "Array.from({{length: Number(")?;
                if let Some(e) = end {
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                    if *inclusive {
                        write!(f, " + 1n")?;
                    }
                } else {
                    write!(f, "0n")?;
                }
                if let Some(s) = start {
                    write!(f, " - ")?;
                    TsExprWriter { expr: s }.ts_fmt(f, cx)?;
                    write!(f, ")}}, (_, __i) => BigInt(__i) + ")?;
                    TsExprWriter { expr: s }.ts_fmt(f, cx)?;
                } else {
                    write!(f, ")}}, (_, __i) => BigInt(__i)")?;
                }
                write!(f, ")")?;
            }
            IrExpr::Assign { left, right } => {
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, " = ")?;
                TsExprWriter { expr: right }.ts_fmt(f, cx)?;
            }
            IrExpr::AssignOp { op, left, right } => {
                if let Some(helper) = bin_op_helper(*op) {
                    TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                    write!(f, " = {}(", helper)?;
                    TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                    write!(f, ", ")?;
                    TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                } else {
                    TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                    write!(f, " {}= ", ts_bin_op(*op))?;
                    TsExprWriter { expr: right }.ts_fmt(f, cx)?;
                }
            }
            IrExpr::Return(e) => {
                write!(f, "return")?;
                if let Some(e) = e {
                    write!(f, " ")?;
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                }
            }
            IrExpr::Cast { expr, ty } => match ty.as_ref() {
                IrType::Primitive(PrimitiveType::U32) => {
                    write!(f, "Number(")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IrType::Primitive(PrimitiveType::U64) => {
                    write!(f, "BigInt(")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IrType::Primitive(PrimitiveType::U8) => {
                    write!(f, "((")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                    write!(f, ") & 0xFFn)")?;
                }
                IrType::Primitive(PrimitiveType::Usize) => {
                    write!(f, "Number(")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                _ => {
                    write!(f, "(")?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                    write!(f, " as unknown as ")?;
                    TsTypeWriter { ty }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
            },
            IrExpr::TypenumUsize { ty } => {
                // If this is a projection on a type param, use the witness
                if let IrType::Projection { base, assoc, .. } = ty.as_ref() {
                    if let IrType::TypeParam(name) = base.as_ref() {
                        let key = witness_ctx_field(&WitnessKind::Projection {
                            type_param: name.clone(),
                            field: format!("{}", assoc),
                        });
                        write!(f, "ctx.{}", key)?;
                    } else {
                        write!(f, "/* <")?;
                        TsTypeWriter { ty }.ts_fmt(f, cx)?;
                        write!(f, " as Unsigned>.USIZE */")?;
                    }
                } else {
                    write!(f, "/* <")?;
                    TsTypeWriter { ty }.ts_fmt(f, cx)?;
                    write!(f, " as Unsigned>.USIZE */")?;
                }
            }
            IrExpr::Unreachable => write!(f, "(() => {{ throw new Error(\"unreachable\"); }})()")?,
            IrExpr::DefaultValue { ty } => {
                if let Some(t) = ty {
                    match t.as_ref() {
                        IrType::TypeParam(name) => {
                            // Type-param default → use witness
                            write!(f, "ctx.default{}()", name)?;
                        }
                        IrType::Array { len, elem, .. } => {
                            write!(f, "Array.from({{length: Number(")?;
                            ts_length(len, f, cx)?;
                            write!(f, ")}}, () => ")?;
                            ts_default_value(elem, f, cx)?;
                            write!(f, ")")?;
                        }
                        _ => {
                            ts_default_value(t, f, cx)?;
                        }
                    }
                } else {
                    write!(f, "undefined")?;
                }
            }
            IrExpr::LengthOf(len) => {
                ts_length(len, f, cx)?;
            }
            IrExpr::ArrayGenerate {
                len,
                index_var,
                body,
                ..
            } => {
                // index_var is bigint (all integers are bigint)
                write!(f, "Array.from({{length: Number(")?;
                ts_length(len, f, cx)?;
                write!(f, ")}}, (_, __raw_{}) => {{ const {} = BigInt(__raw_{}); return ", index_var, index_var, index_var)?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, "; }})")?;
            }
            IrExpr::Match { expr, arms } => {
                write!(f, "(() => {{ ")?;
                write!(f, "const __match = ")?;
                TsExprWriter { expr }.ts_fmt(f, cx)?;
                write!(f, "; ")?;
                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        write!(f, " else ")?;
                    }
                    if i + 1 < arms.len() {
                        write!(f, "if (")?;
                        ts_pattern_condition(&arm.pattern, "__match", f)?;
                        write!(f, ") {{ ")?;
                    } else {
                        write!(f, "{{ ")?;
                    }
                    ts_emit_pattern_bindings(&arm.pattern, "__match", f)?;
                    // Don't double-wrap if the body is already a return/break.
                    if let IrExpr::Return(Some(inner)) = &arm.body {
                        write!(f, "return ")?;
                        TsExprWriter { expr: inner }.ts_fmt(f, cx)?;
                    } else if let IrExpr::Return(None) = &arm.body {
                        write!(f, "return undefined")?;
                    } else {
                        write!(f, "return ")?;
                        TsExprWriter { expr: &arm.body }.ts_fmt(f, cx)?;
                    }
                    write!(f, "; }}")?;
                }
                write!(f, " }})()")?;
            }
            IrExpr::Break(_) => write!(f, "break")?,
            IrExpr::Continue => write!(f, "continue")?,
            IrExpr::Try(e) => {
                TsExprWriter { expr: e }.ts_fmt(f, cx)?;
            }
            _ => {
                write!(
                    f,
                    "undefined /* unsupported: {:?} */",
                    std::any::type_name::<IrExpr>()
                )?;
            }
        }
        Ok(())
    }
}

/// Emit an `if` chain without the outer IIFE wrapper.
/// Used for `else if` chains inside an already-open IIFE.
fn emit_if_chain(expr: &IrExpr, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
    if let IrExpr::If {
        cond,
        then_branch,
        else_branch,
    } = expr
    {
        write!(f, "if (")?;
        TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
        write!(f, ") ")?;
        TsBlockWriter {
            block: then_branch,
            indent: 0,
        }
        .ts_fmt(f, cx)?;
        if let Some(eb) = else_branch {
            write!(f, " else ")?;
            match eb.as_ref() {
                IrExpr::Block(b) => {
                    TsBlockWriter {
                        block: b,
                        indent: 0,
                    }
                    .ts_fmt(f, cx)?;
                }
                IrExpr::If { .. } => {
                    emit_if_chain(eb, f, cx)?;
                }
                _ => {
                    write!(f, "{{ return ")?;
                    TsExprWriter { expr: eb }.ts_fmt(f, cx)?;
                    write!(f, "; }}")?;
                }
            }
        }
    }
    Ok(())
}

/// Emit a method call expression with smart translation.
fn emit_method_call(
    receiver: &IrExpr,
    method: &MethodKind,
    args: &[IrExpr],
    f: &mut fmt::Formatter<'_>,
    cx: &TsContext<'_>,
) -> fmt::Result {
    match method {
        MethodKind::Known(m) => emit_known_method_call(receiver, *m, args, f, cx),
        MethodKind::Vole(v) => {
            let name = format!("{:?}", v).to_lowercase();
            emit_other_method_call(receiver, &name, args, f, cx)
        }
        MethodKind::Other(name) => emit_other_method_call(receiver, name, args, f, cx),
    }
}

/// Emit a method call for a [`StdMethod`] — the typed fast path.
fn emit_known_method_call(
    receiver: &IrExpr,
    method: StdMethod,
    args: &[IrExpr],
    f: &mut fmt::Formatter<'_>,
    cx: &TsContext<'_>,
) -> fmt::Result {
    match method {
        StdMethod::WrappingAdd if args.len() == 1 => {
            write!(f, "wrappingAdd(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::WrappingSub if args.len() == 1 => {
            write!(f, "wrappingSub(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Ilog2 if args.is_empty() => {
            write!(f, "ilog2(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Clone if args.is_empty() => {
            write!(f, "__clone(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Into if args.is_empty() => TsExprWriter { expr: receiver }.ts_fmt(f, cx),
        StdMethod::Cloned if args.is_empty() => TsExprWriter { expr: receiver }.ts_fmt(f, cx),
        StdMethod::ToVec if args.is_empty() => {
            write!(f, "[...")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "]")
        }
        StdMethod::AsSlice if args.is_empty() => TsExprWriter { expr: receiver }.ts_fmt(f, cx),
        StdMethod::Len if args.is_empty() => {
            write!(f, "BigInt(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".length)")
        }
        StdMethod::Contains if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".includes(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Get if args.len() == 1 => {
            // arr.get(j) → optional chaining, returns T | undefined
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "?.[")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "]")
        }
        StdMethod::UnwrapOrDefault if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, " ?? 0)")
        }
        StdMethod::Unwrap if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")!")
        }
        StdMethod::Expect if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")!")
        }
        StdMethod::MapOr if args.len() == 2 => {
            // Option.map_or(default, f) → (x != null ? f(x) : default)
            write!(f, "((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") != null ? (")?;
            TsExprWriter { expr: &args[1] }.ts_fmt(f, cx)?;
            write!(f, ")(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") : (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::Update if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".update(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Finalize if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".finalize()")
        }
        StdMethod::Bitxor if args.len() == 1 => {
            write!(f, "fieldBitxor(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Shl if args.len() == 1 => {
            write!(f, "fieldShl(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Shr if args.len() == 1 => {
            write!(f, "fieldShr(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        // AsRef / Deref with an index arg → subscript access
        StdMethod::AsRef | StdMethod::Deref if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "[(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")]")
        }
        StdMethod::At if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "[(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")]")
        }
        StdMethod::At if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".at(0)")
        }
        StdMethod::IsEmpty if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".length === 0)")
        }
        StdMethod::UnwrapOr if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, " ?? (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::Default if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".default()")
        }
        StdMethod::From if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".from(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::AsPtr | StdMethod::Deref if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            Ok(())
        }
        StdMethod::ToUsize if args.is_empty() => {
            write!(f, "Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::ToString if args.is_empty() => {
            write!(f, "String(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::CheckedAdd | StdMethod::SaturatingAdd if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, " + (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::CheckedSub | StdMethod::SaturatingSub if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, " - (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::WrappingMul if args.len() == 1 => {
            write!(f, "BigInt(Math.imul(Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "), Number(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")))")
        }
        StdMethod::WrappingNeg if args.is_empty() => {
            write!(f, "((-((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")) & 0xFFFFFFFFn))")
        }
        StdMethod::OverflowingAdd if args.len() == 1 => {
            write!(f, "[wrappingAdd(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "), false]")
        }
        StdMethod::OverflowingSub if args.len() == 1 => {
            write!(f, "[wrappingSub(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "), false]")
        }
        // Vec/slice operations → array equivalents
        StdMethod::CopyFromSlice if args.len() == 1 => {
            // dst.copy_from_slice(src) → dst.splice(0, src.length, ...src)
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").splice(0, (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ").length, ...(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::Rev if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").slice().reverse()")
        }
        StdMethod::Sort if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").sort()")
        }
        StdMethod::Fill if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").fill(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Truncate if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").splice(Number(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::Push if args.len() == 1 => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").push(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Pop if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ").pop()")
        }
        // Vec::new() / Vec::with_capacity(n) → [] / new Array(n)
        StdMethod::New if args.is_empty() => write!(f, "[]"),
        StdMethod::WithCapacity if args.len() == 1 => {
            write!(f, "new Array(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::EncryptBlock if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".encrypt_block(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        // Integer bit-shift (masking to 32-bit like Rust semantics, bigint-safe)
        StdMethod::WrappingShl if args.len() == 1 => {
            write!(f, "(((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") << (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")) & 0xFFFFFFFFn)")
        }
        StdMethod::WrappingShr if args.len() == 1 => {
            write!(f, "((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> (")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "))")
        }
        StdMethod::SaturatingMul if args.len() == 1 => {
            write!(f, "BigInt(Math.imul(Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "), Number(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")))")
        }
        StdMethod::CountOnes if args.is_empty() => {
            // popcount — no native, use a simple approximation
            write!(f, "/* count_ones */ ((() => {{ let _n = ")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", _c = 0; while (_n) {{ _c += _n & 1; _n >>>= 1; }} return _c; }})()")
        }
        StdMethod::LeadingZeros if args.is_empty() => {
            write!(f, "Math.clz32(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::TrailingZeros if args.is_empty() => {
            // Negate to isolate lowest set bit, then leading_zeros gives trailing count
            write!(f, "Math.clz32((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") & -((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") | 0))")
        }
        StdMethod::FromLeBytes if args.len() == 1 => {
            // u32::from_le_bytes([b0, b1, b2, b3]) — all bigint
            write!(f, "((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[0n] | ((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[1n] << 8n) | ((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[2n] << 16n) | ((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[3n] << 24n))")
        }
        StdMethod::ToLeBytes if args.is_empty() => {
            write!(f, "[(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") & 0xFFn, ((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> 8n) & 0xFFn, ((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> 16n) & 0xFFn, ((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> 24n) & 0xFFn]")
        }
        StdMethod::FromBeBytes if args.len() == 1 => {
            write!(f, "((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[3n] | ((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[2n] << 8n) | ((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[1n] << 16n) | ((")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")[0n] << 24n))")
        }
        StdMethod::ToBeBytes if args.is_empty() => {
            write!(f, "[((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> 24n) & 0xFFn, ((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> 16n) & 0xFFn, ((")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") >> 8n) & 0xFFn, (")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ") & 0xFFn]")
        }
        StdMethod::Pow if args.len() == 1 => {
            write!(f, "BigInt(Math.pow(Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "), Number(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")))")
        }
        StdMethod::Abs if args.is_empty() => {
            write!(f, "Math.abs(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        StdMethod::Min if args.len() == 1 => {
            write!(f, "BigInt(Math.min(Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "), Number(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")))")
        }
        StdMethod::Max if args.len() == 1 => {
            write!(f, "BigInt(Math.max(Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "), Number(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")))")
        }
        // Fall through to generic emission using the snake_case name
        _ => emit_other_method_call(receiver, method.as_str(), args, f, cx),
    }
}

/// Emit a generic method call as `receiver.name(args…)`.
///
/// This is the fallback path for domain-specific methods that have no
/// TypeScript-specific translation.  Both guardrails fire here in debug
/// builds:
/// - Ident-char check (inherited from the Rust printer's convention).
/// - Misrouting check: the name must *not* be one that `StdMethod` knows
///   about; those names belong in [`emit_known_method_call`] and have
///   specialised TypeScript translations.
fn emit_other_method_call(
    receiver: &IrExpr,
    name: &str,
    args: &[IrExpr],
    f: &mut fmt::Formatter<'_>,
    cx: &TsContext<'_>,
) -> fmt::Result {
    debug_assert!(
        name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
        "TS emit: method name is not a valid identifier: {name:?}"
    );
    debug_assert!(
        StdMethod::try_from_str(name).is_none(),
        "TS emit: {name:?} is a well-known StdMethod but was routed to \
         emit_other_method_call; use MethodKind::Known(StdMethod::…) so the \
         TypeScript backend can apply the correct translation"
    );
    // Option/Result query methods
    match name {
        "is_some" => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            return write!(f, ") != null");
        }
        "is_none" => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            return write!(f, ") == null");
        }
        "is_ok" => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            return write!(f, ") instanceof Ok");
        }
        "is_err" => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            return write!(f, ") instanceof Err");
        }
        "flatten" => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            return write!(f, ".flat()");
        }
        "collect" => {
            // collect() is a no-op in TS — arrays are already concrete
            return TsExprWriter { expr: receiver }.ts_fmt(f, cx);
        }
        "next_power_of_two" => {
            // Round up to next power of 2
            write!(f, "BigInt(1 << Math.ceil(Math.log2(Number(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            return write!(f, "))))");
        }
        "all" => {
            // .all(|x| pred) → emit as .every()
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".every(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
            }
            return write!(f, ")");
        }
        "extend_from_slice" | "extend" => {
            // Vec::extend_from_slice(slice) → arr.push(...slice)
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".push(...(")?;
            if let Some(arg) = args.first() {
                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
            }
            return write!(f, "))");
        }
        // PartialEq trait methods → __equals
        "eq" if args.len() == 1 => {
            write!(f, "__equals(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            return write!(f, ")");
        }
        "ne" if args.len() == 1 => {
            write!(f, "!__equals(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            return write!(f, ")");
        }
        _ => {}
    }
    // Numeric literals need parens before method access in JS/TS (e.g. `(1).method()`).
    let needs_parens = matches!(receiver, IrExpr::Lit(IrLit::Int(_) | IrLit::Float(_)));
    if needs_parens { write!(f, "(")?; }
    TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
    if needs_parens { write!(f, ")")?; }
    write!(f, ".{}(", name)?;
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
    }
    write!(f, ")")
}

/// Emit a path expression with smart mapping.
fn emit_path(segments: &[String], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // Strip Rust module-nav prefixes that don't exist in TS.
    // Also strip known Rust crate/module namespace prefixes so calls like
    // `volar_primitives::gf_mul_u8` become just `gf_mul_u8`.
    let segs: Vec<&str> = segments
        .iter()
        .map(|s| s.as_str())
        .filter(|s| !matches!(
            *s,
            "super" | "crate" | "self"
            | "volar_primitives" | "volar_common" | "volar_spec"
            | "alloc" | "std" | "core" | "vec" | "mem"
        ))
        .collect();

    if segs.len() == 2 && segs[0] == "AsRef" && segs[1] == "as_ref" {
        write!(f, "asRefU8")
    } else if segs.len() == 2 && segs[0] == "D" && segs[1] == "new" {
        write!(f, "new D")
    } else if segs.len() == 2 && segs[0] == "Vec" && segs[1] == "new" {
        write!(f, "[]")
    } else if segs.len() == 2 && segs[0] == "Vec" && segs[1] == "with_capacity" {
        write!(f, "/* Vec::with_capacity */ Array")
    // Rust primitive integer byte conversions: u32/u64/u128::from_le_bytes
    } else if segs.len() == 2 && (segs[0] == "u32" || segs[0] == "u64" || segs[0] == "u128")
        && segs[1] == "from_le_bytes"
    {
        write!(f, "{}_from_le_bytes", segs[0])
    // size_of_val / size_of — handled at call site; shouldn't reach here as a bare path
    } else if segs.len() == 1 && (segs[0] == "size_of_val" || segs[0] == "size_of") {
        write!(f, "ctx.sizeOf_unknown")
    } else if segs.len() == 1 && segs[0] == "None" {
        write!(f, "undefined")
    } else if segs.is_empty() {
        Ok(())
    } else {
        write!(f, "{}", segs.join("."))
    }
}

// ============================================================================
// ITERATOR CHAIN
// ============================================================================

struct TsIterChainWriter<'a> {
    chain: &'a IrIterChain,
}

impl<'a> TsBackend for TsIterChainWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        TsIterSourceWriter {
            source: &self.chain.source,
        }
        .ts_fmt(f, cx)?;

        for step in &self.chain.steps {
            match step {
                IterStep::Map { var, body } => {
                    write!(f, ".map((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ": any) => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::Filter { var, body } => {
                    write!(f, ".filter((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ": any) => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::FilterMap { var, body } => {
                    write!(f, ".map((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ": any) => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ").filter((__x: any) => __x !== undefined)")?;
                }
                IterStep::FlatMap { var, body } => {
                    write!(f, ".flatMap((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ": any) => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::Enumerate => {
                    write!(
                        f,
                        ".map((val: any, i: number) => [i, val] as [number, typeof val])"
                    )?;
                }
                IterStep::Take { count } => {
                    write!(f, ".slice(0, Number(")?;
                    TsExprWriter { expr: count }.ts_fmt(f, cx)?;
                    write!(f, "))")?;
                }
                IterStep::Skip { count } => {
                    write!(f, ".slice(Number(")?;
                    TsExprWriter { expr: count }.ts_fmt(f, cx)?;
                    write!(f, "))")?;
                }
                IterStep::Chain { other } => {
                    write!(f, ".concat(")?;
                    TsIterChainWriter { chain: other }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
            }
        }

        match &self.chain.terminal {
            IterTerminal::Collect | IterTerminal::CollectTyped(_) => {
                // Identity — .map()/.filter() already produce arrays in JS
            }
            IterTerminal::Fold {
                init,
                acc_var,
                elem_var,
                body,
            } => {
                write!(f, ".reduce((")?;
                TsPatternWriter { pat: acc_var }.ts_fmt(f, cx)?;
                write!(f, ": any, ")?;
                TsPatternWriter { pat: elem_var }.ts_fmt(f, cx)?;
                write!(f, ": any) => ")?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, ", ")?;
                TsExprWriter { expr: init }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IterTerminal::Lazy => {}
        }

        Ok(())
    }
}

struct TsIterSourceWriter<'a> {
    source: &'a IterChainSource,
}

impl<'a> TsBackend for TsIterSourceWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        match self.source {
            IterChainSource::Method { collection, method } => match method {
                IterMethod::Iter | IterMethod::IntoIter => {
                    TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
                }
                IterMethod::Chars => {
                    TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
                    write!(f, ".split(\"\")")?;
                }
                IterMethod::Bytes => {
                    write!(f, "Array.from(")?;
                    TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterMethod::Flatten => {
                    // Vec<Vec<T>>.flatten() → .flat() in JS/TS
                    TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
                    write!(f, ".flat()")?;
                }
            },
            IterChainSource::Range {
                start,
                end,
                inclusive,
            } => {
                write!(f, "Array.from({{length: Number(")?;
                TsExprWriter { expr: end }.ts_fmt(f, cx)?;
                if *inclusive {
                    write!(f, " + 1n")?;
                }
                write!(f, " - ")?;
                TsExprWriter { expr: start }.ts_fmt(f, cx)?;
                write!(f, ")}}, (_, __i) => BigInt(__i) + ")?;
                TsExprWriter { expr: start }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IterChainSource::Zip { left, right } => {
                TsIterChainWriter { chain: left }.ts_fmt(f, cx)?;
                write!(f, ".map((__a: any, __i: number) => [__a, ")?;
                TsIterChainWriter { chain: right }.ts_fmt(f, cx)?;
                write!(f, "[__i]] as [typeof __a, any])")?;
            }
        }
        Ok(())
    }
}

// ============================================================================
// PATTERN
// ============================================================================

struct TsPatternWriter<'a> {
    pat: &'a IrPattern,
}

impl<'a> TsBackend for TsPatternWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        match self.pat {
            IrPattern::Ident { name, .. } => {
                write!(f, "{}", escape_ts_reserved(name))?;
            }
            IrPattern::Wild => write!(f, "_")?,
            IrPattern::Tuple(elems) => {
                write!(f, "[")?;
                for (i, p) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsPatternWriter { pat: p }.ts_fmt(f, cx)?;
                }
                write!(f, "]")?;
            }
            IrPattern::TupleStruct { kind, elems } => {
                if elems.len() == 1 {
                    TsPatternWriter { pat: &elems[0] }.ts_fmt(f, cx)?;
                } else {
                    write!(f, "/* {}(...) */ [", kind)?;
                    for (i, p) in elems.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        TsPatternWriter { pat: p }.ts_fmt(f, cx)?;
                    }
                    write!(f, "]")?;
                }
            }
            IrPattern::Struct { fields, rest, .. } => {
                write!(f, "{{ ")?;
                for (i, (name, p)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    // Field is $f-prefixed; always use "{ $fname: pat }" to bind
                    // the original (unprefixed) name as the local variable.
                    write!(f, "$f{}: ", name)?;
                    TsPatternWriter { pat: p }.ts_fmt(f, cx)?;
                }
                let _ = rest;
                write!(f, " }}")?;
            }
            IrPattern::Ref { pat, .. } => {
                TsPatternWriter { pat }.ts_fmt(f, cx)?;
            }
            IrPattern::Lit(l) => {
                ts_literal(l, f)?;
            }
            IrPattern::Or(pats) => {
                if let Some(first) = pats.first() {
                    TsPatternWriter { pat: first }.ts_fmt(f, cx)?;
                }
            }
            IrPattern::Slice(pats) => {
                write!(f, "[")?;
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsPatternWriter { pat: p }.ts_fmt(f, cx)?;
                }
                write!(f, "]")?;
            }
            IrPattern::Rest => write!(f, "...__rest")?,
        }
        Ok(())
    }
}

// ============================================================================
// HELPERS
// ============================================================================

fn ts_primitive(p: &PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::Bool => "boolean",
        PrimitiveType::U8 | PrimitiveType::U32 | PrimitiveType::Usize
        | PrimitiveType::U64 | PrimitiveType::I128 | PrimitiveType::U128 => "bigint",
        PrimitiveType::Bit => "Bit",
        PrimitiveType::Galois => "Galois",
        PrimitiveType::Galois64 => "Galois64",
        PrimitiveType::Galois128 => "Galois128",
        PrimitiveType::Galois256 => "Galois256",
        PrimitiveType::BitsInBytes => "BitsInBytes",
        PrimitiveType::BitsInBytes64 => "BitsInBytes64",
        PrimitiveType::Z3 => "Z3",
    }
}

fn ts_bin_op(op: SpecBinOp) -> &'static str {
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
        SpecBinOp::Eq => "===",
        SpecBinOp::Ne => "!==",
        SpecBinOp::Lt => "<",
        SpecBinOp::Le => "<=",
        SpecBinOp::Gt => ">",
        SpecBinOp::Ge => ">=",
        SpecBinOp::And => "&&",
        SpecBinOp::Or => "||",
    }
}

fn bin_op_helper(op: SpecBinOp) -> Option<&'static str> {
    match op {
        SpecBinOp::Add => Some("fieldAdd"),
        SpecBinOp::Sub => Some("fieldSub"),
        SpecBinOp::Mul => Some("fieldMul"),
        SpecBinOp::BitXor => Some("fieldBitxor"),
        SpecBinOp::BitOr => Some("fieldBitor"),
        SpecBinOp::BitAnd => Some("fieldBitand"),
        SpecBinOp::Shl => Some("fieldShl"),
        SpecBinOp::Shr => Some("fieldShr"),
        _ => None,
    }
}

fn ts_literal(l: &IrLit, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match l {
        IrLit::Int(n) => write!(f, "{}n", n),
        IrLit::Float(v) => write!(f, "{}", v),
        IrLit::Bool(b) => write!(f, "{}", b),
        IrLit::Char(c) => write!(f, "\"{}\"", c),
        IrLit::Str(s) => write!(f, "\"{}\"", s),
        IrLit::ByteStr(_) => write!(f, "new Uint8Array([/* byte string */])"),
        IrLit::Byte(b) => write!(f, "{}n", b),
        IrLit::Unit => write!(f, "undefined"),
    }
}

fn ts_length(len: &ArrayLength, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
    match len {
        ArrayLength::Const(n) => write!(f, "{}", n),
        ArrayLength::TypeParam(p) => write!(f, "{}", p.to_lowercase()),
        ArrayLength::Projection { r#type, field, .. } => {
            // Use ctx witness for type-param projections
            if let IrType::TypeParam(name) = r#type.as_ref() {
                let key = witness_ctx_field(&WitnessKind::Projection {
                    type_param: name.clone(),
                    field: field.clone(),
                });
                write!(f, "ctx.{}", key)
            } else {
                write!(f, "/* ")?;
                TsTypeWriter { ty: r#type }.ts_fmt(f, cx)?;
                write!(f, "::{} */ 0", field)
            }
        }
        ArrayLength::TypeNum(tn) => write!(f, "{}", tn.to_usize()),
    }
}

fn ts_default_value(ty: &IrType, f: &mut fmt::Formatter<'_>, cx: &TsContext) -> fmt::Result {
    match ty {
        IrType::Primitive(p) => match p {
            PrimitiveType::Bool => write!(f, "false"),
            PrimitiveType::U8 | PrimitiveType::U32 | PrimitiveType::Usize
            | PrimitiveType::U64 | PrimitiveType::I128 | PrimitiveType::U128 => write!(f, "0n"),
            PrimitiveType::Bit => write!(f, "Bit.default()"),
            PrimitiveType::Galois => write!(f, "Galois.default()"),
            PrimitiveType::Galois64 => write!(f, "Galois64.default()"),
            PrimitiveType::Galois128 => write!(f, "Galois128.default()"),
            PrimitiveType::Galois256 => write!(f, "Galois256.default()"),
            PrimitiveType::BitsInBytes => write!(f, "BitsInBytes.default()"),
            PrimitiveType::BitsInBytes64 => write!(f, "BitsInBytes64.default()"),
            PrimitiveType::Z3 => write!(f, "Z3.default()"),
        },
        IrType::Array { elem, len, .. } => {
            write!(f, "Array.from({{length: Number(")?;
            ts_length(len, f, cx)?;
            write!(f, ")}}, () => ")?;
            ts_default_value(elem, f, cx)?;
            write!(f, ")")
        }
        IrType::Vector { .. } => write!(f, "[]"),
        IrType::TypeParam(name) => {
            // Type-param default → use witness
            write!(f, "ctx.default{}()", name)
        }
        IrType::Infer => write!(f, "undefined"),
        _ => write!(f, "undefined /* default for {:?} */", ty),
    }
}

/// Returns the canonical name used when emitting this method call in
/// TypeScript.  Most callers should use [`emit_method_call`] directly;
/// this helper is kept for witness-scanning code that needs the name
/// without full emission.
fn ts_method_call_name(method: &MethodKind) -> String {
    // Guardrail: Other must not wrap a well-known StdMethod name.
    method.debug_assert_not_misrouted();
    match method {
        MethodKind::Known(m) => m.as_str().to_string(),
        MethodKind::Vole(v) => format!("{:?}", v).to_lowercase(),
        MethodKind::Other(s) => s.clone(),
    }
}

fn ts_method_name(rust_name: &str, trait_ref: Option<&IrTraitRef>) -> String {
    if let Some(tr) = trait_ref {
        match &tr.kind {
            TraitKind::Math(m) => {
                if let Some(name) = m.method_name() {
                    return name.to_string();
                }
            }
            TraitKind::Into(_) => return "into".to_string(),
            TraitKind::AsRef(_) => return "asRef".to_string(),
            _ => {}
        }
    }
    let name = if rust_name.starts_with("r#") {
        &rust_name[2..]
    } else {
        rust_name
    };
    match name {
        "static" => "static_".to_string(),
        "delete" => "delete_".to_string(),
        "class" => "class_".to_string(),
        _ => name.to_string(),
    }
}

fn ts_param_name(name: &str) -> &str {
    match name {
        "self" => "this",
        _ => name,
    }
}

/// Escape TypeScript/JavaScript reserved words used as variable names.
///
/// Rust allows variable names like `new`, `type`, `delete` etc. that are
/// reserved in TypeScript. Append `_` to make them valid identifiers.
fn escape_ts_reserved(name: &str) -> String {
    match name {
        "new" | "delete" | "type" | "class" | "extends" | "implements"
        | "interface" | "package" | "private" | "protected" | "public"
        | "static" | "yield" | "enum" | "in" | "instanceof" | "typeof"
        | "var" | "void" | "with" | "super" | "import" | "export"
        | "default" | "from" | "of" | "let" | "const" | "function"
        | "return" | "throw" | "catch" | "finally" | "try" | "switch"
        | "case" | "break" | "continue" | "debugger" | "null"
        | "undefined" | "true" | "false" | "this" | "if" | "else"
        | "for" | "while" | "do" => format!("{}_", name),
        _ => name.to_string(),
    }
}

/// Convert a Rust field name to a valid TypeScript property name.
///
/// Numeric or empty field names use bracket notation `[N]` in TS (works for
/// tuple struct class fields and array element access alike).
/// Register all ident bindings in `pat` as having type param `tp` in `cx.var_types`.
fn collect_pattern_var_types(pat: &IrPattern, tp: &str, cx: &TsContext<'_>) {
    match pat {
        IrPattern::Ident { name, .. } if name != "_" => {
            cx.register_var_type(name, tp);
        }
        IrPattern::Tuple(pats) | IrPattern::TupleStruct { elems: pats, .. } => {
            for p in pats { collect_pattern_var_types(p, tp, cx); }
        }
        IrPattern::Ref { pat, .. } => collect_pattern_var_types(pat, tp, cx),
        _ => {}
    }
}

fn ts_field_name(name: &str, positional_index: usize) -> String {
    if name.is_empty() || name.chars().next().map_or(false, |c| c.is_ascii_digit()) {
        format!("[{}]", positional_index)
    } else {
        format!("$f{}", name)
    }
}

fn ts_field_access(name: &str) -> String {
    if name.chars().next().map_or(false, |c| c.is_ascii_digit()) {
        let idx: usize = name.parse().unwrap_or(0);
        format!("[{}]", idx)
    } else {
        format!(".$f{}", name)
    }
}

fn is_phantom_field(field: &IrField) -> bool {
    field.name == "_phantom"
        || field.name.starts_with("_phantom")
        || matches!(&field.ty, IrType::Struct { kind, .. } if kind.to_string().contains("PhantomData"))
}

/// TS has no PhantomData — drop any call argument that is a bare `PhantomData` path.
fn is_phantom_arg(expr: &IrExpr) -> bool {
    matches!(expr, IrExpr::Path { segments, .. } if segments.last().map(|s| s == "PhantomData").unwrap_or(false))
}

fn pattern_is_mutable(pat: &IrPattern) -> bool {
    match pat {
        IrPattern::Ident { mutable, .. } => *mutable,
        _ => false,
    }
}

fn self_ty_name(ty: &IrType) -> String {
    match ty {
        IrType::Struct { kind, .. } => kind.to_string(),
        IrType::TypeParam(p) => p.clone(),
        _ => format!("{}", ty),
    }
}

fn ts_pattern_condition(
    pat: &IrPattern,
    match_var: &str,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match pat {
        IrPattern::Lit(l) => {
            write!(f, "{} === ", match_var)?;
            ts_literal(l, f)
        }
        IrPattern::Ident { .. } | IrPattern::Wild => write!(f, "true"),
        IrPattern::TupleStruct { kind, .. } => {
            let name = kind.to_string();
            let variant = name.rsplit("::").next().unwrap_or(&name);
            match variant {
                "Some" | "Ok" => {
                    write!(f, "{} !== null && {} !== undefined", match_var, match_var)
                }
                "None" | "Err" => {
                    write!(f, "{} === null || {} === undefined", match_var, match_var)
                }
                _ => write!(f, "true /* {} */", name),
            }
        }
        _ => write!(f, "true /* pattern {:?} */", pat),
    }
}

/// Emit pattern binding statements into the match arm body.
/// E.g., `Some(s)` → `const s = __match;`
fn ts_emit_pattern_bindings(
    pat: &IrPattern,
    match_var: &str,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match pat {
        IrPattern::Ident { name, mutable, .. }
            // Don't bind: wildcards, type-constructor names (uppercase start), or "None"
            if name != "_"
                && name.chars().next().map_or(false, |c| c.is_lowercase())
        => {
            let kw = if *mutable { "let" } else { "const" };
            writeln!(f, "{} {} = {};", kw, escape_ts_reserved(name), match_var)
        }
        IrPattern::TupleStruct { kind, elems } => {
            let name = kind.to_string();
            let variant = name.rsplit("::").next().unwrap_or(&name);
            // For Some/None/Ok/Err (transparent): bind inner directly to match_var.
            // For tuple structs: access via [N] bracket notation.
            let transparent = matches!(variant, "Some" | "Ok" | "Err" | "None");
            if elems.len() == 1 {
                let field = if transparent {
                    match_var.to_string()
                } else {
                    format!("{}[0]", match_var)
                };
                ts_emit_pattern_bindings(&elems[0], &field, f)?;
            } else {
                for (i, elem) in elems.iter().enumerate() {
                    let field = if transparent {
                        format!("{}[{}]", match_var, i)
                    } else {
                        format!("{}[{}]", match_var, i)
                    };
                    ts_emit_pattern_bindings(elem, &field, f)?;
                }
            }
            Ok(())
        }
        IrPattern::Tuple(elems) => {
            for (i, elem) in elems.iter().enumerate() {
                ts_emit_pattern_bindings(elem, &format!("{}[{}]", match_var, i), f)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn resolve_fn_generic(
    type_name: &str,
    generics: &[IrGenericParam],
    cx: &TsContext<'_>,
) -> Option<String> {
    for g in generics {
        if g.name == type_name {
            for b in &g.bounds {
                match &b.trait_kind {
                    TraitKind::Fn(input, ret) => {
                        let param_ty = match input {
                            FnInput::BytesSlice => "Uint8Array",
                            FnInput::Size => "number",
                            FnInput::Bool => "boolean",
                        };
                        let ret_str = format!("{}", TsFmt(TsTypeRefWriter { ty: ret }, cx));
                        return Some(format!("(arg: {}) => {}", param_ty, ret_str));
                    }
                    TraitKind::AsRef(inner) => {
                        let inner_str = format!("{}", TsFmt(TsTypeRefWriter { ty: inner }, cx));
                        return Some(inner_str);
                    }
                    _ => {}
                }
            }
        }
    }
    None
}

/// Write a parameter's type, resolving Fn-bounded generics inline.
/// Unwrap reference wrappers to get the inner type for param-type resolution.
fn unwrap_ref(ty: &IrType) -> &IrType {
    match ty {
        IrType::Reference { elem, .. } => unwrap_ref(elem),
        other => other,
    }
}

fn unwrap_ref_expr(e: &IrExpr) -> &IrExpr {
    match e {
        IrExpr::Unary { op: SpecUnaryOp::Ref | SpecUnaryOp::RefMut, expr } => unwrap_ref_expr(expr),
        other => other,
    }
}

fn write_param_type(
    p: &IrParam,
    generics: &[IrGenericParam],
    f: &mut fmt::Formatter<'_>,
    cx: &TsContext<'_>,
) -> fmt::Result {
    // Resolve against the inner type (unwrapping &/&mut wrappers).
    let inner = unwrap_ref(&p.ty);
    if let IrType::TypeParam(tp) = inner {
        if let Some(fn_type) = resolve_fn_generic(tp, generics, cx) {
            return write!(f, "{}", fn_type);
        }
        // Any type param with no TS-expressible bounds (Fn/AsRef) should be erased to `any`.
        // This covers: R (SpecRng), D/D_ (Digest), T (FieldElement), etc.
        // Calling methods on these types would cause TS2339; `any` allows arbitrary calls.
        let has_useful_bounds = generics.iter().any(|g| {
            g.name == tp.as_str() && g.bounds.iter().any(|b| {
                matches!(&b.trait_kind, TraitKind::Fn(..) | TraitKind::AsRef(..))
            })
        });
        if !has_useful_bounds {
            return write!(f, "any");
        }
    }
    // Existential params (impl Trait) with no useful TS bounds → `any`.
    if let IrType::Existential { bounds } = inner {
        let has_useful = bounds.iter().any(|b| {
            matches!(&b.trait_kind, TraitKind::Fn(..) | TraitKind::AsRef(..))
        });
        if !has_useful {
            return write!(f, "any");
        }
    }
    if let IrType::Existential { bounds } = &p.ty {
        if let Some(b) = bounds.first() {
            match &b.trait_kind {
                TraitKind::AsRef(inner) => {
                    return TsTypeWriter { ty: inner }.ts_fmt(f, cx);
                }
                TraitKind::Fn(input, ret) => {
                    let param_ty = match input {
                        FnInput::BytesSlice => "Uint8Array",
                        FnInput::Size => "number",
                        FnInput::Bool => "boolean",
                    };
                    write!(f, "(arg: {}) => ", param_ty)?;
                    return TsTypeWriter { ty: ret }.ts_fmt(f, cx);
                }
                _ => {}
            }
        }
    }
    TsTypeWriter { ty: &p.ty }.ts_fmt(f, cx)
}

struct TsTypeRefWriter<'a> {
    ty: &'a IrType,
}

impl<'a> TsBackend for TsTypeRefWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        TsTypeWriter { ty: self.ty }.ts_fmt(f, cx)
    }
}

// ============================================================================
// TYPE PARAMETER USAGE ANALYSIS
// ============================================================================

/// Returns the set of type parameter names that are actually referenced
/// in a list of types (used to prune unused generics).
fn collect_type_param_refs(types: &[&IrType]) -> Vec<String> {
    let mut refs = Vec::new();
    for ty in types {
        collect_type_param_refs_inner(ty, &mut refs);
    }
    refs.sort();
    refs.dedup();
    refs
}

fn collect_type_param_refs_inner(ty: &IrType, refs: &mut Vec<String>) {
    match ty {
        IrType::TypeParam(p) => {
            if p != "Self" {
                refs.push(p.clone());
            }
        }
        IrType::Vector { elem } | IrType::Reference { elem, .. } => {
            collect_type_param_refs_inner(elem, refs);
        }
        IrType::Array { elem, .. } => {
            collect_type_param_refs_inner(elem, refs);
        }
        IrType::Struct { type_args, .. } => {
            for arg in type_args {
                collect_type_param_refs_inner(arg, refs);
            }
        }
        IrType::Tuple(elems) => {
            for e in elems {
                collect_type_param_refs_inner(e, refs);
            }
        }
        IrType::FnPtr { params, ret } => {
            for p in params {
                collect_type_param_refs_inner(p, refs);
            }
            collect_type_param_refs_inner(ret, refs);
        }
        IrType::Existential { bounds } => {
            for b in bounds {
                match &b.trait_kind {
                    TraitKind::Fn(_, ret) => collect_type_param_refs_inner(ret, refs),
                    TraitKind::AsRef(inner) => collect_type_param_refs_inner(inner, refs),
                    _ => {}
                }
            }
        }
        IrType::Projection { base, .. } => {
            collect_type_param_refs_inner(base, refs);
        }
        _ => {}
    }
}

/// Collect type params used in parameter types and return type of a function.
fn function_used_type_params(func: &IrFunction) -> Vec<String> {
    let mut types: Vec<&IrType> = func.params.iter().map(|p| &p.ty).collect();
    if let Some(ret) = &func.return_type {
        types.push(ret);
    }
    collect_type_param_refs(&types)
}

/// Collect type params used in struct field types.
fn struct_used_type_params(s: &IrStruct) -> Vec<String> {
    let types: Vec<&IrType> = s
        .fields
        .iter()
        .filter(|f| !is_phantom_field(f))
        .map(|f| &f.ty)
        .collect();
    collect_type_param_refs(&types)
}

/// Filter generics to only those actually used.
fn filter_used_generics<'a>(
    generics: &'a [IrGenericParam],
    used: &[String],
) -> Vec<&'a IrGenericParam> {
    generics
        .iter()
        .filter(|g| {
            if g.kind != IrGenericParamKind::Type {
                return false;
            }
            if g.bounds
                .iter()
                .any(|b| matches!(&b.trait_kind, TraitKind::Fn(..)))
            {
                return false;
            }
            if g.bounds
                .iter()
                .any(|b| matches!(&b.trait_kind, TraitKind::AsRef(..)))
            {
                return false;
            }
            used.contains(&g.name)
        })
        .collect()
}

// ============================================================================
// CFG MODULE — IrCfgModule → TypeScript (state-machine style)
// ============================================================================

/// Renders an `IrCfgModule` as TypeScript.
///
/// Structs → classes, enums → tagged unions, auxiliary functions → regular
/// TS functions, CFG functions → `while(true) { switch(__state) {...} }`
/// state machines.
pub(crate) struct TsCfgModuleWriter<'a> {
    pub module: &'a IrCfgModule,
}

impl<'a> TsBackend for TsCfgModuleWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        // Structs → TS classes (no impls to merge for CFG modules)
        for s in &self.module.structs {
            TsClassWriter {
                s,
                impls: &[],
            }
            .ts_fmt(f, cx)?;
            writeln!(f)?;
        }

        // Enums → tagged union types
        for e in &self.module.enums {
            ts_write_enum(f, e, cx)?;
            writeln!(f)?;
        }

        // Type aliases
        for ta in &self.module.type_aliases {
            ts_write_type_alias(f, ta, cx)?;
            writeln!(f)?;
        }

        let empty = WitnessNeeds::default();
        for func in &self.module.functions {
            match func {
                IrAnyFunction::Flat(func) => {
                    // TypeStub functions carry only signature info for LIR codegen;
                    // the TS output imports the real implementation.
                    if func.external_kind == ExternalKind::TypeStub {
                        continue;
                    }
                    TsFunctionWriter {
                        func,
                        indent: 0,
                        witness_needs: &empty,
                    }
                    .ts_fmt(f, cx)?;
                    writeln!(f)?;
                }
                IrAnyFunction::Cfg(func) => {
                    TsCfgFunctionWriter { func, indent: 0 }.ts_fmt(f, cx)?;
                    writeln!(f)?;
                }
            }
        }

        Ok(())
    }
}

// ── Enum → tagged-union emitter ───────────────────────────────────────────

/// Emit an `IrEnum` as a TypeScript tagged-union type + factory functions.
///
/// ```ts
/// type MyEnum = { tag: "VariantA", _0: number } | { tag: "VariantB" };
/// function MyEnum_VariantA(_0: number): MyEnum { return { tag: "VariantA", _0 }; }
/// function MyEnum_VariantB(): MyEnum { return { tag: "VariantB" }; }
/// ```
fn ts_write_enum(
    f: &mut fmt::Formatter<'_>,
    e: &IrEnum,
    cx: &TsContext<'_>,
) -> fmt::Result {
    let name = e.kind.to_string();

    // Type definition: union of per-variant objects
    write!(f, "export type {}", name)?;
    // Generics
    if !e.generics.is_empty() {
        let type_only: Vec<&IrGenericParam> = e
            .generics
            .iter()
            .filter(|g| g.kind == IrGenericParamKind::Type)
            .collect();
        if !type_only.is_empty() {
            write!(f, "<")?;
            for (i, g) in type_only.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", g.name)?;
            }
            write!(f, ">")?;
        }
    }
    write!(f, " =")?;

    if e.variants.is_empty() {
        writeln!(f, " never;")?;
        return Ok(());
    }

    writeln!(f)?;
    for (vi, v) in e.variants.iter().enumerate() {
        write!(f, "  | {{ tag: \"{}\"", v.name)?;
        match &v.fields {
            IrEnumVariantData::Unit => {}
            IrEnumVariantData::Tuple(types) => {
                for (fi, ty) in types.iter().enumerate() {
                    write!(f, ", _{}: ", fi)?;
                    TsTypeWriter { ty }.ts_fmt(f, cx)?;
                }
            }
            IrEnumVariantData::Struct(fields) => {
                for field in fields {
                    write!(f, ", $f{}: ", field.name)?;
                    TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
                }
            }
        }
        write!(f, " }}")?;
        if vi + 1 < e.variants.len() {
            writeln!(f)?;
        }
    }
    writeln!(f, ";")?;

    // Factory functions for each variant
    for v in &e.variants {
        write!(f, "export function {}_{}", name, v.name)?;
        write!(f, "(")?;
        match &v.fields {
            IrEnumVariantData::Unit => {}
            IrEnumVariantData::Tuple(types) => {
                for (fi, ty) in types.iter().enumerate() {
                    if fi > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "_{}: ", fi)?;
                    TsTypeWriter { ty }.ts_fmt(f, cx)?;
                }
            }
            IrEnumVariantData::Struct(fields) => {
                for (fi, field) in fields.iter().enumerate() {
                    if fi > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", field.name)?;
                    TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
                }
            }
        }
        write!(f, "): {} {{ return {{ tag: \"{}\"", name, v.name)?;
        match &v.fields {
            IrEnumVariantData::Unit => {}
            IrEnumVariantData::Tuple(types) => {
                for (fi, _) in types.iter().enumerate() {
                    write!(f, ", _{}", fi)?;
                }
            }
            IrEnumVariantData::Struct(fields) => {
                for field in fields {
                    write!(f, ", $f{}: {}", field.name, field.name)?;
                }
            }
        }
        writeln!(f, " }}; }}")?;
    }

    Ok(())
}

// ── Type alias emitter ────────────────────────────────────────────────────

fn ts_write_type_alias(
    f: &mut fmt::Formatter<'_>,
    ta: &IrTypeAlias,
    cx: &TsContext<'_>,
) -> fmt::Result {
    write!(f, "export type {}", ta.name)?;
    if !ta.generics.is_empty() {
        let type_only: Vec<&IrGenericParam> = ta
            .generics
            .iter()
            .filter(|g| g.kind == IrGenericParamKind::Type)
            .collect();
        if !type_only.is_empty() {
            write!(f, "<")?;
            for (i, g) in type_only.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", g.name)?;
            }
            write!(f, ">")?;
        }
    }
    write!(f, " = ")?;
    TsTypeWriter { ty: &ta.target }.ts_fmt(f, cx)?;
    writeln!(f, ";")?;
    Ok(())
}

// ── CFG Function → state machine ──────────────────────────────────────────

/// Renders a single `IrCfgFunction` as an `export function` with a state-machine body.
struct TsCfgFunctionWriter<'a> {
    func: &'a IrCfgFunction,
    indent: usize,
}

impl<'a> TsBackend for TsCfgFunctionWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let func = self.func;
        let blocks = &func.body.blocks;

        // ── Signature ─────────────────────────────────────────────────────
        write!(f, "{}export function {}", ind, func.name)?;
        // Emit type-only generics (skip const generics — TS doesn't have them)
        let type_generics: Vec<&IrGenericParam> = func
            .generics
            .iter()
            .filter(|g| g.kind == IrGenericParamKind::Type)
            .collect();
        if !type_generics.is_empty() {
            write!(f, "<")?;
            for (i, g) in type_generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", g.name)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (i, p) in func.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", ts_param_name(&p.name))?;
            TsTypeWriter { ty: &p.ty }.ts_fmt(f, cx)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
        }

        // ── Body ──────────────────────────────────────────────────────────
        writeln!(f, " {{")?;

        // Single-block fast path (no state machine needed)
        if blocks.len() == 1 {
            let blk = &blocks[0];
            for stmt in &blk.stmts {
                TsStmtWriter {
                    stmt,
                    indent: self.indent + 1,
                }
                .ts_fmt(f, cx)?;
            }
            match &blk.terminator {
                IrCfgTerminator::Return(None) => {}
                IrCfgTerminator::Return(Some(expr)) => {
                    let inner = "  ".repeat(self.indent + 1);
                    write!(f, "{}return ", inner)?;
                    TsExprWriter { expr }.ts_fmt(f, cx)?;
                    writeln!(f, ";")?;
                }
                other => {
                    ts_write_cfg_terminator(f, other, self.indent + 1, cx)?;
                }
            }
            writeln!(f, "{}}}", ind)?;
            return Ok(());
        }

        // ── State machine ─────────────────────────────────────────────────
        ts_write_state_machine(f, func, self.indent + 1, cx)?;
        writeln!(f, "{}}}", ind)?;
        Ok(())
    }
}

/// Emit the `while(true) { switch(__state) { ... } }` body.
fn ts_write_state_machine(
    f: &mut fmt::Formatter<'_>,
    func: &IrCfgFunction,
    base: usize,
    cx: &TsContext<'_>,
) -> fmt::Result {
    let blocks = &func.body.blocks;
    let l0 = "  ".repeat(base);
    let l1 = "  ".repeat(base + 1);
    let l2 = "  ".repeat(base + 2);

    writeln!(f, "{}let __state = 0;", l0)?;

    // Declare block-param slots for blocks 1.. (block 0 uses function params).
    for (bidx, blk) in blocks.iter().enumerate().skip(1) {
        for (pidx, param) in blk.params.iter().enumerate() {
            write!(f, "{}let __b{}_p{}: ", l0, bidx, pidx)?;
            TsTypeWriter { ty: &param.ty }.ts_fmt(f, cx)?;
            writeln!(f, " | undefined = undefined;")?;
        }
    }

    writeln!(f, "{}while (true) {{", l0)?;
    writeln!(f, "{}switch (__state) {{", l1)?;

    for (bidx, blk) in blocks.iter().enumerate() {
        writeln!(f, "{}case {}: {{", l1, bidx)?;

        // Bind block params from their slots (block 0 uses function params).
        if bidx > 0 {
            for (pidx, param) in blk.params.iter().enumerate() {
                writeln!(
                    f,
                    "{}let {} = __b{}_p{}!;",
                    l2, param.name, bidx, pidx
                )?;
                writeln!(f, "{}__b{}_p{} = undefined;", l2, bidx, pidx)?;
            }
        }

        // Statements
        for stmt in &blk.stmts {
            TsStmtWriter {
                stmt,
                indent: base + 2,
            }
            .ts_fmt(f, cx)?;
        }

        // Terminator
        ts_write_cfg_terminator(f, &blk.terminator, base + 2, cx)?;

        writeln!(f, "{}}}", l1)?; // end case block
    }

    writeln!(f, "{}default: throw new Error(\"unreachable\");", l1)?;
    writeln!(f, "{}}}", l1)?; // end switch
    writeln!(f, "{}}}", l0)?; // end while
    Ok(())
}

/// Emit a CFG terminator in TypeScript.
fn ts_write_cfg_terminator(
    f: &mut fmt::Formatter<'_>,
    term: &IrCfgTerminator,
    indent: usize,
    cx: &TsContext<'_>,
) -> fmt::Result {
    let ind = "  ".repeat(indent);
    let inner = "  ".repeat(indent + 1);
    match term {
        IrCfgTerminator::Return(None) => {
            writeln!(f, "{}return;", ind)?;
        }
        IrCfgTerminator::Return(Some(expr)) => {
            write!(f, "{}return ", ind)?;
            TsExprWriter { expr }.ts_fmt(f, cx)?;
            writeln!(f, ";")?;
        }
        IrCfgTerminator::Goto(jump) => {
            ts_write_jump_setup(f, jump, indent, cx)?;
            writeln!(f, "{}continue;", ind)?;
        }
        IrCfgTerminator::CondGoto { cond, then_, else_ } => {
            write!(f, "{}if (", ind)?;
            TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
            writeln!(f, ") {{")?;
            ts_write_jump_setup(f, then_, indent + 1, cx)?;
            writeln!(f, "{}}} else {{", ind)?;
            ts_write_jump_setup(f, else_, indent + 1, cx)?;
            writeln!(f, "{}}}", ind)?;
            writeln!(f, "{}continue;", ind)?;
        }
    }
    Ok(())
}

/// Assign jump args to block-param slots and set `__state`.
fn ts_write_jump_setup(
    f: &mut fmt::Formatter<'_>,
    jump: &IrCfgJump,
    indent: usize,
    cx: &TsContext<'_>,
) -> fmt::Result {
    let ind = "  ".repeat(indent);
    for (pidx, arg) in jump.args.iter().enumerate() {
        write!(f, "{}__b{}_p{} = ", ind, jump.target, pidx)?;
        TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
        writeln!(f, ";")?;
    }
    writeln!(f, "{}__state = {};", ind, jump.target)?;
    Ok(())
}
