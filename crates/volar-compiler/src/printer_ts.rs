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
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::collections::BTreeMap;

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap;

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
}

/// The set of witnesses needed by one function (or one merged method).
#[derive(Debug, Clone, Default)]
struct WitnessNeeds {
    needs: Vec<WitnessKind>,
}

impl WitnessNeeds {
    fn is_empty(&self) -> bool { self.needs.is_empty() }

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
        // DefaultValue with a type-param type → need a default witness
        IrExpr::DefaultValue { ty: Some(ty) } => {
            if let IrType::TypeParam(name) = ty.as_ref() {
                if declared_generics.contains(name) || is_crypto_type_param(name) {
                    out.add(WitnessKind::Default { type_param: name.clone() });
                }
            }
        }
        // LengthOf with a Projection → need a projection witness
        IrExpr::LengthOf(len) => {
            scan_array_length_witnesses(len, out);
        }
        // ArrayDefault may contain a projection in its length
        IrExpr::ArrayDefault { len, elem_ty: _, .. } => {
            scan_array_length_witnesses(len, out);
        }
        // ArrayGenerate may contain a projection in its length
        IrExpr::ArrayGenerate { len, index_var: _, body, .. } => {
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
        // Call to T::new() or T::default()
        IrExpr::Call { func, args } => {
            if let IrExpr::Path { segments, .. } = func.as_ref() {
                if segments.len() == 2 {
                    let type_name = &segments[0];
                    let method = &segments[1];
                    if method == "new" && (declared_generics.contains(type_name) || is_crypto_type_param(type_name)) {
                        out.add(WitnessKind::Constructor { type_param: type_name.clone() });
                    } else if method == "default" && (declared_generics.contains(type_name) || is_crypto_type_param(type_name)) {
                        out.add(WitnessKind::Default { type_param: type_name.clone() });
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
        IrExpr::Unary { expr, .. } | IrExpr::Return(Some(expr)) | IrExpr::Cast { expr, .. } | IrExpr::Try(expr) => {
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
        IrExpr::Tuple(es) | IrExpr::Array(es) => {
            for e in es { scan_expr_witnesses(e, out, declared_generics); }
        }
        IrExpr::Repeat { elem, len } => {
            scan_expr_witnesses(elem, out, declared_generics);
            scan_expr_witnesses(len, out, declared_generics);
        }
        IrExpr::Block(b) => scan_block_witnesses(b, out, declared_generics),
        IrExpr::If { cond, then_branch, else_branch } => {
            scan_expr_witnesses(cond, out, declared_generics);
            scan_block_witnesses(then_branch, out, declared_generics);
            if let Some(eb) = else_branch { scan_expr_witnesses(eb, out, declared_generics); }
        }
        IrExpr::BoundedLoop { start, end, body, .. } => {
            scan_expr_witnesses(start, out, declared_generics);
            scan_expr_witnesses(end, out, declared_generics);
            scan_block_witnesses(body, out, declared_generics);
        }
        IrExpr::IterLoop { collection, body, .. } => {
            scan_expr_witnesses(collection, out, declared_generics);
            scan_block_witnesses(body, out, declared_generics);
        }
        IrExpr::Closure { body, .. } => scan_expr_witnesses(body, out, declared_generics),
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start { scan_expr_witnesses(s, out, declared_generics); }
            if let Some(e) = end { scan_expr_witnesses(e, out, declared_generics); }
        }
        IrExpr::Match { expr, arms } => {
            scan_expr_witnesses(expr, out, declared_generics);
            for arm in arms { scan_expr_witnesses(&arm.body, out, declared_generics); }
        }
        IrExpr::IterPipeline(chain) => scan_iter_chain_witnesses(chain, out, declared_generics),
        IrExpr::RawMap { receiver, body, .. } => {
            scan_expr_witnesses(receiver, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        IrExpr::RawZip { left, right, body, .. } => {
            scan_expr_witnesses(left, out, declared_generics);
            scan_expr_witnesses(right, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            scan_expr_witnesses(receiver, out, declared_generics);
            scan_expr_witnesses(init, out, declared_generics);
            scan_expr_witnesses(body, out, declared_generics);
        }
        _ => {} // Lit, Var, Path, Break, Continue, Unreachable, etc.
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

fn scan_iter_chain_witnesses(chain: &IrIterChain, out: &mut WitnessNeeds, declared_generics: &[String]) {
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

/// Compute witness needs for a function.
fn compute_function_witnesses(func: &IrFunction) -> WitnessNeeds {
    let declared: Vec<String> = func.generics.iter().map(|g| g.name.clone()).collect();
    let mut needs = WitnessNeeds::default();
    scan_block_witnesses(&func.body, &mut needs, &declared);
    needs.sorted();
    needs
}

/// Compute merged witness needs for a set of method variants.
fn compute_merged_witnesses(variants: &[ImplMethod<'_>]) -> WitnessNeeds {
    let mut needs = WitnessNeeds::default();
    for v in variants {
        needs.merge(&compute_function_witnesses(v.func));
    }
    needs.sorted();
    needs
}

/// Returns true if a name is a type parameter that typically comes from a crypto
/// trait bound (Digest, LengthDoubler, etc.) and might need witnesses.
fn is_crypto_type_param(name: &str) -> bool {
    matches!(name, "B" | "D" | "O" | "T" | "A" | "Q" | "M" | "U" | "R" | "X" | "Y")
}

/// Get the TypeScript parameter name for a witness in the `ctx` object.
fn witness_ctx_field(kind: &WitnessKind) -> String {
    match kind {
        WitnessKind::Projection { type_param, field } => format!("{}_{}", type_param, field),
        WitnessKind::Constructor { type_param } => format!("new{}", type_param),
        WitnessKind::Default { type_param } => format!("default{}", type_param),
    }
}

/// Get the TypeScript type for a witness field.
fn witness_ctx_type(kind: &WitnessKind) -> &'static str {
    match kind {
        WitnessKind::Projection { .. } => "number",
        WitnessKind::Constructor { .. } => "() => any",
        WitnessKind::Default { .. } => "() => any",
    }
}

/// Write the `ctx` parameter type inline: `ctx: { B_OutputSize: number, newD: () => any }`
fn write_ctx_param(needs: &WitnessNeeds, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ctx: {{ ")?;
    for (i, kind) in needs.needs.iter().enumerate() {
        if i > 0 { write!(f, ", ")?; }
        write!(f, "{}: {}", witness_ctx_field(kind), witness_ctx_type(kind))?;
    }
    write!(f, " }}")
}

/// Build a map from internal function names to their witness needs.
/// Used so that callers know they must forward `ctx` when calling these functions.
fn build_module_witness_map(module: &IrModule) -> BTreeMap<String, WitnessNeeds> {
    let mut map = BTreeMap::new();
    for func in &module.functions {
        let needs = compute_function_witnesses(func);
        if !needs.is_empty() {
            let name = if func.name.starts_with("r#") { func.name[2..].to_string() } else { func.name.clone() };
            map.insert(name, needs);
        }
    }
    // Also scan impl methods
    for imp in &module.impls {
        for item in &imp.items {
            if let IrImplItem::Method(func) = item {
                let needs = compute_function_witnesses(func);
                if !needs.is_empty() {
                    let class_name = self_ty_name(&imp.self_ty);
                    let method_name = ts_method_name(&func.name, imp.trait_.as_ref());
                    let key = format!("{}.{}", class_name, method_name);
                    map.insert(key, needs);
                }
            }
        }
    }
    map
}

// ============================================================================
// PUBLIC API
// ============================================================================

/// Render an `IrModule` as a complete TypeScript source file.
pub fn print_module_ts(module: &IrModule) -> String {
    let witness_map = build_module_witness_map(module);
    let cx = TsContext { witness_map: &witness_map };
    let mut out = String::new();
    let _ = write!(out, "{}", TsFmt(TsPreambleWriter, &cx));
    let _ = write!(out, "{}", TsFmt(TsModuleWriter { module }, &cx));
    out
}

// ============================================================================
// Shared context threaded through all writers
// ============================================================================

/// Immutable context passed to every TS writer.
struct TsContext<'a> {
    /// Per-function witness needs keyed by `"functionName"` or `"ClassName.methodName"`.
    witness_map: &'a BTreeMap<String, WitnessNeeds>,
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
            let has_specialization = methods.iter()
                .any(|m| m.self_type_args.iter().any(|t| !matches!(t, IrType::TypeParam(_))));
            if has_specialization {
                merged_methods.push(MergedMethod { name, variants: methods });
            } else {
                // All generic — just take the first
                unique_methods.push(methods.into_iter().next().unwrap());
            }
        }
    }

    ClassAnalysis { unique_methods, merged_methods }
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
        IrType::Struct { kind, .. } => return Some(format!("/* {} */ true", kind)),
        _ => return None,
    };

    // Find the first field that uses the type parameter T to check against
    for field in struct_fields {
        if is_phantom_field(field) { continue; }
        match &field.ty {
            // Vec<T> field — check first element
            IrType::Vector { .. } => {
                return Some(format!("this.{}[0] instanceof {}", field.name, class_name));
            }
            // Vec<Vec<T>> field — check first element of first element
            IrType::Array { elem, .. } if matches!(elem.as_ref(), IrType::Vector { .. }) => {
                return Some(format!("this.{}[0]?.[0] instanceof {}", field.name, class_name));
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
        IrExpr::BoundedLoop { .. } | IrExpr::IterLoop { .. } => true,
        // An `if` without an else branch, or where branches are statement-like
        IrExpr::If { else_branch: None, .. } => true,
        IrExpr::If { then_branch, else_branch: Some(eb), .. } => {
            // If the then-branch's tail expression is statement-like, the whole if is
            let then_is_stmt = then_branch.expr.as_ref()
                .map_or(true, |e| is_statement_like(e));
            let else_is_stmt = is_statement_like(eb);
            then_is_stmt && else_is_stmt
        }
        IrExpr::Block(b) => {
            b.expr.as_ref().map_or(true, |e| is_statement_like(e))
        }
        IrExpr::Assign { .. } | IrExpr::AssignOp { .. } => true,
        _ => false,
    }
}

// ============================================================================
// PREAMBLE
// ============================================================================

struct TsPreambleWriter;

impl TsBackend for TsPreambleWriter {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        writeln!(f, "// Auto-generated TypeScript from volar-spec")?;
        writeln!(f, "// Type-level lengths have been converted to runtime number witnesses")?;
        // NO
        // writeln!(f, "// @ts-nocheck — generated code uses dynamic patterns that need runtime dispatch")?;
        writeln!(f)?;
        writeln!(f, "import {{")?;
        writeln!(f, "  type Cloneable,")?;
        writeln!(f, "  type FieldElement,")?;
        writeln!(f, "  type BlockEncrypt,")?;
        writeln!(f, "  type Digest,")?;
        writeln!(f, "  type LengthDoubler,")?;
        writeln!(f, "  Bit,")?;
        writeln!(f, "  Galois,")?;
        writeln!(f, "  Galois64,")?;
        writeln!(f, "  BitsInBytes,")?;
        writeln!(f, "  BitsInBytes64,")?;
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
        writeln!(f, "  commit,")?;
        writeln!(f, "  doubleVec,")?;
        writeln!(f, "  wrappingAdd,")?;
        writeln!(f, "  wrappingSub,")?;
        writeln!(f, "  asRefU8,")?;
        writeln!(f, "}} from \"volar-runtime\";")?;
        writeln!(f)?;
        Ok(())
    }
}

// ============================================================================
// MODULE
// ============================================================================

struct TsModuleWriter<'a> {
    module: &'a IrModule,
}

impl<'a> TsBackend for TsModuleWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
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

        // Emit standalone functions
        for func in &self.module.functions {
            let needs = compute_function_witnesses(func);
            TsFunctionWriter { func, indent: 0, witness_needs: &needs }.ts_fmt(f, cx)?;
            writeln!(f)?;
        }

        // Any impls whose struct isn't in this module
        for (name, impls) in &impl_groups {
            writeln!(f, "// Orphan impls for {}", name)?;
            for imp in impls {
                for item in &imp.items {
                    if let IrImplItem::Method(func) = item {
                        let needs = compute_function_witnesses(func);
                        TsFunctionWriter { func, indent: 0, witness_needs: &needs }.ts_fmt(f, cx)?;
                        writeln!(f)?;
                    }
                }
            }
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
        let used_params = struct_used_type_params(self.s);

        write!(f, "export class {}", name)?;
        TsGenericsWriter { generics: struct_generics, used_only: Some(&used_params) }.ts_fmt(f, cx)?;
        writeln!(f, " {{")?;

        // Constructor with non-phantom fields — takes an initializer object
        let fields: Vec<&IrField> = self.s.fields.iter()
            .filter(|field| !is_phantom_field(field))
            .collect();

        // Declare fields
        for field in &fields {
            write!(f, "  {}: ", field.name)?;
            TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
            writeln!(f, ";")?;
        }
        writeln!(f)?;
        writeln!(f, "  constructor(init: {{ ")?;
        for (i, field) in fields.iter().enumerate() {
            let comma = if i + 1 < fields.len() { "," } else { "" };
            write!(f, "    {}: ", field.name)?;
            TsTypeWriter { ty: &field.ty }.ts_fmt(f, cx)?;
            writeln!(f, "{}", comma)?;
        }
        writeln!(f, "  }}) {{")?;
        writeln!(f, "    Object.assign(this, init);")?;
        writeln!(f, "  }}")?;

        // Run analysis to detect overlapping methods
        let analysis = analyze_class_impls(self.impls);

        // Emit unique methods
        for im in &analysis.unique_methods {
            writeln!(f)?;
            let needs = compute_function_witnesses(im.func);
            TsMethodWriter { func: im.func, imp: im.imp, indent: 1, witness_needs: &needs }.ts_fmt(f, cx)?;
        }

        // Emit merged methods (runtime dispatch)
        for mm in &analysis.merged_methods {
            writeln!(f)?;
            let needs = compute_merged_witnesses(&mm.variants);
            TsMergedMethodWriter {
                merged: mm,
                struct_fields: &self.s.fields,
                indent: 1,
                witness_needs: &needs,
            }.ts_fmt(f, cx)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

// ============================================================================
// METHOD (single, inside a class)
// ============================================================================

struct TsMethodWriter<'a> {
    func: &'a IrFunction,
    imp: &'a IrImpl,
    indent: usize,
    witness_needs: &'a WitnessNeeds,
}

impl<'a> TsMethodWriter<'a> {
    fn resolve_fn_type(&self, type_name: &str) -> Option<String> {
        resolve_fn_generic(type_name, &self.func.generics)
    }
}

impl<'a> TsBackend for TsMethodWriter<'a> {
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let name = ts_method_name(&self.func.name, self.imp.trait_.as_ref());

        let is_static = self.func.receiver.is_none();
        if is_static {
            write!(f, "{}static {}", ind, name)?;
        } else {
            write!(f, "{}{}", ind, name)?;
        }

        let used_params = function_used_type_params(self.func);
        TsGenericsWriter { generics: &self.func.generics, used_only: Some(&used_params) }.ts_fmt(f, cx)?;

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

        if let Some(ret) = &self.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
        }

        writeln!(f)?;
        TsBlockWriter { block: &self.func.body, indent: self.indent }.ts_fmt(f, cx)?;
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
        let generics_owned: Vec<IrGenericParam> = all_generics.iter()
            .filter(|p| {
                p.kind == IrGenericParamKind::Type
                    && !p.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::Fn(..)))
                    && !p.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::AsRef(..)))
            })
            .map(|p| (*p).clone())
            .collect();
        TsGenericsWriter { generics: &generics_owned, used_only: None }.ts_fmt(f, cx)?;

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

        // Return type — use first variant's
        if let Some(ret) = &first.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.ts_fmt(f, cx)?;
        }

        writeln!(f)?;
        writeln!(f, "{}{{", ind)?;

        // Emit if/else chain for each variant
        for (i, variant) in self.merged.variants.iter().enumerate() {
            let check = if let Some(ty) = variant.self_type_args.first() {
                runtime_type_check(ty, self.struct_fields)
                    .unwrap_or_else(|| "true".to_string())
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
                TsStmtWriter { stmt, indent: self.indent + 2 }.ts_fmt(f, cx)?;
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
        let name = if self.func.name.starts_with("r#") { &self.func.name[2..] } else { &self.func.name };
        write!(f, "{}export function {}", ind, name)?;
        let used_params = function_used_type_params(self.func);
        TsGenericsWriter { generics: &self.func.generics, used_only: Some(&used_params) }.ts_fmt(f, cx)?;
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
        TsBlockWriter { block: &self.func.body, indent: self.indent }.ts_fmt(f, cx)?;
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
    fn ts_fmt(&self, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
        let type_params: Vec<&IrGenericParam> = self.generics.iter()
            .filter(|p| {
                if p.kind != IrGenericParamKind::Type {
                    return false;
                }
                if p.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::Fn(..))) {
                    return false;
                }
                if p.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::AsRef(..))) {
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
                if *kind == ArrayKind::Slice {
                    write!(f, "readonly ")?;
                    TsTypeWriter { ty: elem }.ts_fmt(f, cx)?;
                    write!(f, "[]")?;
                } else {
                    TsTypeWriter { ty: elem }.ts_fmt(f, cx)?;
                    write!(f, "[]")?;
                }
            }
            IrType::Struct { kind, type_args } => {
                let name = kind.to_string();
                if name == "Option" && type_args.len() == 1 {
                    write!(f, "(")?;
                    TsTypeWriter { ty: &type_args[0] }.ts_fmt(f, cx)?;
                    write!(f, " | undefined)")?;
                    return Ok(());
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
                    // TypeScript's polymorphic `this` is too strict for our use.
                    // We'd need the enclosing class name, but in a type-writer we
                    // don't have that context.  Emit `any` as a safe fallback —
                    // callers that need precision should cast.
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
            TsStmtWriter { stmt, indent: self.indent + 1 }.ts_fmt(f, cx)?;
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
fn emit_statement_expr(e: &IrExpr, indent: usize, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
    match e {
        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            write!(f, "for (let {} = ", var)?;
            TsExprWriter { expr: start }.ts_fmt(f, cx)?;
            write!(f, "; {} {} ", var, if *inclusive { "<=" } else { "<" })?;
            TsExprWriter { expr: end }.ts_fmt(f, cx)?;
            write!(f, "; {}++) ", var)?;
            TsBlockWriter { block: body, indent }.ts_fmt(f, cx)?;
        }
        IrExpr::IterLoop { pattern, collection, body } => {
            write!(f, "for (const ")?;
            TsPatternWriter { pat: pattern }.ts_fmt(f, cx)?;
            write!(f, " of ")?;
            TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
            write!(f, ") ")?;
            TsBlockWriter { block: body, indent }.ts_fmt(f, cx)?;
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            write!(f, "if (")?;
            TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
            write!(f, ") ")?;
            TsBlockWriter { block: then_branch, indent }.ts_fmt(f, cx)?;
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
            TsExprWriter { expr: left }.ts_fmt(f, cx)?;
            write!(f, " = ")?;
            TsExprWriter { expr: right }.ts_fmt(f, cx)?;
            write!(f, ";")?;
        }
        IrExpr::AssignOp { op, left, right } => {
            TsExprWriter { expr: left }.ts_fmt(f, cx)?;
            write!(f, " {}= ", ts_bin_op(*op))?;
            TsExprWriter { expr: right }.ts_fmt(f, cx)?;
            write!(f, ";")?;
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
                    TsExprWriter { expr: i }.ts_fmt(f, cx)?;
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
                let name = if v == "self" { "this" } else { v.as_str() };
                write!(f, "{}", name)?;
            }
            IrExpr::Binary { op, left, right } => {
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
            IrExpr::Unary { op, expr } => {
                match op {
                    SpecUnaryOp::Neg => {
                        write!(f, "-")?;
                        TsExprWriter { expr }.ts_fmt(f, cx)?;
                    }
                    SpecUnaryOp::Not => {
                        write!(f, "!")?;
                        TsExprWriter { expr }.ts_fmt(f, cx)?;
                    }
                    SpecUnaryOp::Deref | SpecUnaryOp::Ref | SpecUnaryOp::RefMut => {
                        TsExprWriter { expr }.ts_fmt(f, cx)?;
                    }
                }
            }
            IrExpr::MethodCall { receiver, method, args, .. } => {
                emit_method_call(receiver, method, args, f, cx)?;
            }
            IrExpr::Call { func, args } => {
                if let IrExpr::Path { segments, .. } = func.as_ref() {
                    // Some(x) → x
                    if segments.len() == 1 && segments[0] == "Some" && args.len() == 1 {
                        return TsExprWriter { expr: &args[0] }.ts_fmt(f, cx);
                    }
                    // None → undefined
                    if segments.len() == 1 && segments[0] == "None" && args.is_empty() {
                        return write!(f, "undefined");
                    }
                    // T::new() → ctx.newT()  (constructor witness)
                    if segments.len() == 2 && segments[1] == "new" {
                        let type_name = &segments[0];
                        if is_crypto_type_param(type_name) {
                            write!(f, "ctx.new{}(", type_name)?;
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 { write!(f, ", ")?; }
                                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                            }
                            return write!(f, ")");
                        }
                    }
                    // T::default() → ctx.defaultT()  (default witness)
                    if segments.len() == 2 && segments[1] == "default" {
                        let type_name = &segments[0];
                        if is_crypto_type_param(type_name) {
                            return write!(f, "ctx.default{}()", type_name);
                        }
                    }
                    // Check if target is an internal function that needs ctx forwarding
                    if segments.len() == 1 {
                        let target_name = &segments[0];
                        if cx.witness_map.contains_key(target_name.as_str()) {
                            write!(f, "{}(ctx", target_name)?;
                            for arg in args {
                                write!(f, ", ")?;
                                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                            }
                            return write!(f, ")");
                        }
                    }
                }
                TsExprWriter { expr: func }.ts_fmt(f, cx)?;
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
                }
                write!(f, ")")?;
            }
            IrExpr::Field { base, field } => {
                TsExprWriter { expr: base }.ts_fmt(f, cx)?;
                write!(f, ".{}", field)?;
            }
            IrExpr::Index { base, index } => {
                TsExprWriter { expr: base }.ts_fmt(f, cx)?;
                write!(f, "[")?;
                TsExprWriter { expr: index }.ts_fmt(f, cx)?;
                write!(f, "]")?;
            }
            IrExpr::Path { segments, .. } => {
                emit_path(segments, f)?;
            }
            IrExpr::StructExpr { kind, fields, .. } => {
                let name = kind.to_string();
                let real_fields: Vec<&(String, IrExpr)> = fields.iter()
                    .filter(|(n, _)| !n.starts_with("_phantom"))
                    .collect();
                write!(f, "new {}({{ ", name)?;
                for (i, (field_name, val)) in real_fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", field_name)?;
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
            IrExpr::Array(elems) => {
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
                write!(f, "Array.from({{length: ")?;
                TsExprWriter { expr: len }.ts_fmt(f, cx)?;
                write!(f, "}}, () => ")?;
                TsExprWriter { expr: elem }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IrExpr::Block(b) => {
                // Expression-position block → IIFE
                write!(f, "(() => ")?;
                TsBlockWriter { block: b, indent: 0 }.ts_fmt(f, cx)?;
                write!(f, ")()")?;
            }
            IrExpr::If { cond, then_branch, else_branch } => {
                // If in expression position → IIFE with if/else
                write!(f, "(() => {{ if (")?;
                TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
                write!(f, ") ")?;
                TsBlockWriter { block: then_branch, indent: 0 }.ts_fmt(f, cx)?;
                if let Some(eb) = else_branch {
                    write!(f, " else ")?;
                    match eb.as_ref() {
                        IrExpr::Block(b) => {
                            TsBlockWriter { block: b, indent: 0 }.ts_fmt(f, cx)?;
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
            IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
                write!(f, "for (let {} = ", var)?;
                TsExprWriter { expr: start }.ts_fmt(f, cx)?;
                write!(f, "; {} {} ", var, if *inclusive { "<=" } else { "<" })?;
                TsExprWriter { expr: end }.ts_fmt(f, cx)?;
                write!(f, "; {}++) ", var)?;
                TsBlockWriter { block: body, indent: 0 }.ts_fmt(f, cx)?;
            }
            IrExpr::IterLoop { pattern, collection, body } => {
                write!(f, "for (const ")?;
                TsPatternWriter { pat: pattern }.ts_fmt(f, cx)?;
                write!(f, " of ")?;
                TsExprWriter { expr: collection }.ts_fmt(f, cx)?;
                write!(f, ") ")?;
                TsBlockWriter { block: body, indent: 0 }.ts_fmt(f, cx)?;
            }
            IrExpr::IterPipeline(chain) => {
                TsIterChainWriter { chain }.ts_fmt(f, cx)?;
            }
            IrExpr::RawMap { receiver, elem_var, body } => {
                TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
                write!(f, ".map((")?;
                TsPatternWriter { pat: elem_var }.ts_fmt(f, cx)?;
                write!(f, ") => ")?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IrExpr::RawZip { left, right, left_var, right_var, body } => {
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
            IrExpr::RawFold { receiver, init, acc_var, elem_var, body } => {
                TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
                write!(f, ".reduce((")?;
                TsPatternWriter { pat: acc_var }.ts_fmt(f, cx)?;
                write!(f, ", ")?;
                TsPatternWriter { pat: elem_var }.ts_fmt(f, cx)?;
                write!(f, ") => ")?;
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
            IrExpr::Range { start, end, inclusive } => {
                write!(f, "Array.from({{length: ")?;
                if let Some(e) = end {
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                    if *inclusive {
                        write!(f, " + 1")?;
                    }
                } else {
                    write!(f, "0")?;
                }
                if let Some(s) = start {
                    write!(f, " - ")?;
                    TsExprWriter { expr: s }.ts_fmt(f, cx)?;
                    write!(f, "}}, (_, i) => i + ")?;
                    TsExprWriter { expr: s }.ts_fmt(f, cx)?;
                } else {
                    write!(f, "}}, (_, i) => i")?;
                }
                write!(f, ")")?;
            }
            IrExpr::Assign { left, right } => {
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, " = ")?;
                TsExprWriter { expr: right }.ts_fmt(f, cx)?;
            }
            IrExpr::AssignOp { op, left, right } => {
                TsExprWriter { expr: left }.ts_fmt(f, cx)?;
                write!(f, " {}= ", ts_bin_op(*op))?;
                TsExprWriter { expr: right }.ts_fmt(f, cx)?;
            }
            IrExpr::Return(e) => {
                write!(f, "return")?;
                if let Some(e) = e {
                    write!(f, " ")?;
                    TsExprWriter { expr: e }.ts_fmt(f, cx)?;
                }
            }
            IrExpr::Cast { expr, ty } => {
                match ty.as_ref() {
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
                        write!(f, ") & 0xFF)")?;
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
                }
            }
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
            IrExpr::ArrayDefault { elem_ty, len } => {
                write!(f, "Array.from({{length: ")?;
                ts_length(len, f, cx)?;
                write!(f, "}}, () => ")?;
                if let Some(ty) = elem_ty {
                    ts_default_value(ty, f)?;
                } else {
                    write!(f, "undefined")?;
                }
                write!(f, ")")?;
            }
            IrExpr::DefaultValue { ty } => {
                if let Some(t) = ty {
                    if let IrType::TypeParam(name) = t.as_ref() {
                        // Type-param default → use witness
                        write!(f, "ctx.default{}()", name)?;
                    } else {
                        ts_default_value(t, f)?;
                    }
                } else {
                    write!(f, "undefined")?;
                }
            }
            IrExpr::LengthOf(len) => {
                ts_length(len, f, cx)?;
            }
            IrExpr::ArrayGenerate { len, index_var, body, .. } => {
                write!(f, "Array.from({{length: ")?;
                ts_length(len, f, cx)?;
                write!(f, "}}, (_, {}) => ", index_var)?;
                TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                write!(f, ")")?;
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
                        write!(f, ") {{ return ")?;
                    } else {
                        write!(f, "{{ return ")?;
                    }
                    TsExprWriter { expr: &arm.body }.ts_fmt(f, cx)?;
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
                write!(f, "undefined /* unsupported: {:?} */", std::any::type_name::<IrExpr>())?;
            }
        }
        Ok(())
    }
}

/// Emit an `if` chain without the outer IIFE wrapper.
/// Used for `else if` chains inside an already-open IIFE.
fn emit_if_chain(expr: &IrExpr, f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
    if let IrExpr::If { cond, then_branch, else_branch } = expr {
        write!(f, "if (")?;
        TsExprWriter { expr: cond }.ts_fmt(f, cx)?;
        write!(f, ") ")?;
        TsBlockWriter { block: then_branch, indent: 0 }.ts_fmt(f, cx)?;
        if let Some(eb) = else_branch {
            write!(f, " else ")?;
            match eb.as_ref() {
                IrExpr::Block(b) => {
                    TsBlockWriter { block: b, indent: 0 }.ts_fmt(f, cx)?;
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
fn emit_method_call(receiver: &IrExpr, method: &MethodKind, args: &[IrExpr], f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
    let name = ts_method_call_name(method);
    match name.as_str() {
        "wrapping_add" if args.len() == 1 => {
            write!(f, "wrappingAdd(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "wrapping_sub" if args.len() == 1 => {
            write!(f, "wrappingSub(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "ilog2" if args.is_empty() => {
            write!(f, "ilog2(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "clone" if args.is_empty() => {
            write!(f, "structuredClone(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "into" if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)
        }
        "cloned" if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)
        }
        "to_vec" if args.is_empty() => {
            write!(f, "[...")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "]")
        }
        "as_slice" if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)
        }
        "len" if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".length")
        }
        "contains" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".includes(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "get" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "?.[")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, "]")
        }
        "unwrap_or_default" if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, " ?? 0)")
        }
        "map_or" if args.len() == 2 => {
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
        "update" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".update(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "finalize" if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".finalize()")
        }
        "bitxor" if args.len() == 1 => {
            write!(f, "fieldBitxor(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "shl" if args.len() == 1 => {
            write!(f, "fieldShl(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "shr" if args.len() == 1 => {
            write!(f, "fieldShr(")?;
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")")
        }
        "at" | "valueOf" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, "[(")?;
            TsExprWriter { expr: &args[0] }.ts_fmt(f, cx)?;
            write!(f, ")]")
        }
        "at" if args.is_empty() => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".at(0)")
        }
        _ => {
            TsExprWriter { expr: receiver }.ts_fmt(f, cx)?;
            write!(f, ".{}(", name)?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                TsExprWriter { expr: arg }.ts_fmt(f, cx)?;
            }
            write!(f, ")")
        }
    }
}

/// Emit a path expression with smart mapping.
fn emit_path(segments: &[String], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if segments.len() == 2 && segments[0] == "AsRef" && segments[1] == "as_ref" {
        write!(f, "asRefU8")
    } else if segments.len() == 2 && segments[0] == "D" && segments[1] == "new" {
        write!(f, "new D")
    } else if segments.len() == 1 && segments[0] == "double_vec" {
        write!(f, "doubleVec")
    } else if segments.len() == 1 && segments[0] == "commit" {
        write!(f, "commit")
    } else if segments.len() == 1 && segments[0] == "Some" {
        // Option::Some(x) → x (TS uses `T | undefined`)
        write!(f, "")
    } else if segments.len() == 1 && segments[0] == "None" {
        write!(f, "undefined")
    } else {
        write!(f, "{}", segments.join("."))
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
        TsIterSourceWriter { source: &self.chain.source }.ts_fmt(f, cx)?;

        for step in &self.chain.steps {
            match step {
                IterStep::Map { var, body } => {
                    write!(f, ".map((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::Filter { var, body } => {
                    write!(f, ".filter((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::FilterMap { var, body } => {
                    write!(f, ".map((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ").filter((__x) => __x !== undefined)")?;
                }
                IterStep::FlatMap { var, body } => {
                    write!(f, ".flatMap((")?;
                    TsPatternWriter { pat: var }.ts_fmt(f, cx)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::Enumerate => {
                    write!(f, ".map((val, i) => [i, val] as [number, typeof val])")?;
                }
                IterStep::Take { count } => {
                    write!(f, ".slice(0, ")?;
                    TsExprWriter { expr: count }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
                }
                IterStep::Skip { count } => {
                    write!(f, ".slice(")?;
                    TsExprWriter { expr: count }.ts_fmt(f, cx)?;
                    write!(f, ")")?;
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
            IterTerminal::Fold { init, acc_var, elem_var, body } => {
                write!(f, ".reduce((")?;
                TsPatternWriter { pat: acc_var }.ts_fmt(f, cx)?;
                write!(f, ", ")?;
                TsPatternWriter { pat: elem_var }.ts_fmt(f, cx)?;
                write!(f, ") => ")?;
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
            IterChainSource::Method { collection, method } => {
                match method {
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
                }
            }
            IterChainSource::Range { start, end, inclusive } => {
                write!(f, "Array.from({{length: ")?;
                TsExprWriter { expr: end }.ts_fmt(f, cx)?;
                if *inclusive {
                    write!(f, " + 1")?;
                }
                write!(f, " - ")?;
                TsExprWriter { expr: start }.ts_fmt(f, cx)?;
                write!(f, "}}, (_, __i) => __i + ")?;
                TsExprWriter { expr: start }.ts_fmt(f, cx)?;
                write!(f, ")")?;
            }
            IterChainSource::Zip { left, right } => {
                TsIterChainWriter { chain: left }.ts_fmt(f, cx)?;
                write!(f, ".map((__a, __i) => [__a, ")?;
                TsIterChainWriter { chain: right }.ts_fmt(f, cx)?;
                write!(f, "[__i]] as [typeof __a, unknown])")?;
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
                write!(f, "{}", name)?;
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
                    if let IrPattern::Ident { name: pat_name, .. } = p {
                        if pat_name == name {
                            write!(f, "{}", name)?;
                        } else {
                            write!(f, "{}: ", name)?;
                            TsPatternWriter { pat: p }.ts_fmt(f, cx)?;
                        }
                    } else {
                        write!(f, "{}: ", name)?;
                        TsPatternWriter { pat: p }.ts_fmt(f, cx)?;
                    }
                }
                if *rest {
                    // Don't emit ...__rest — it causes duplicate identifier errors.
                    // The rest is unused in our generated code anyway.
                }
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
        PrimitiveType::U8 | PrimitiveType::U32 | PrimitiveType::Usize => "number",
        PrimitiveType::U64 | PrimitiveType::I128 => "bigint",
        PrimitiveType::Bit => "Bit",
        PrimitiveType::Galois => "Galois",
        PrimitiveType::Galois64 => "Galois64",
        PrimitiveType::BitsInBytes => "BitsInBytes",
        PrimitiveType::BitsInBytes64 => "BitsInBytes64",
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
        IrLit::Int(n) => {
            if *n >= -(1i128 << 53) && *n <= (1i128 << 53) {
                write!(f, "{}", n)
            } else {
                write!(f, "BigInt(\"{}\")", n)
            }
        }
        IrLit::Float(v) => write!(f, "{}", v),
        IrLit::Bool(b) => write!(f, "{}", b),
        IrLit::Char(c) => write!(f, "\"{}\"", c),
        IrLit::Str(s) => write!(f, "\"{}\"", s),
        IrLit::ByteStr(_) => write!(f, "new Uint8Array([/* byte string */])"),
        IrLit::Byte(b) => write!(f, "{}", b),
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

fn ts_default_value(ty: &IrType, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match ty {
        IrType::Primitive(p) => match p {
            PrimitiveType::Bool => write!(f, "false"),
            PrimitiveType::U8 | PrimitiveType::U32 | PrimitiveType::Usize => write!(f, "0"),
            PrimitiveType::U64 | PrimitiveType::I128 => write!(f, "0n"),
            PrimitiveType::Bit => write!(f, "Bit.default()"),
            PrimitiveType::Galois => write!(f, "Galois.default()"),
            PrimitiveType::Galois64 => write!(f, "Galois64.default()"),
            PrimitiveType::BitsInBytes => write!(f, "BitsInBytes.default()"),
            PrimitiveType::BitsInBytes64 => write!(f, "BitsInBytes64.default()"),
        },
        IrType::Vector { .. } | IrType::Array { .. } => write!(f, "[]"),
        _ => write!(f, "undefined /* default for {:?} */", ty),
    }
}

fn ts_method_call_name(method: &MethodKind) -> String {
    match method {
        MethodKind::Std(s) => {
            match s.as_str() {
                "clone" => "clone".to_string(),
                "into" => "into".to_string(),
                "len" => "length".to_string(),
                "is_empty" => "length === 0 ? true : false; /* ".to_string(),
                "contains" => "includes".to_string(),
                "unwrap" | "unwrap_or_default" => "unwrap_or_default".to_string(),
                "as_ref" => "valueOf".to_string(),
                "as_slice" => "slice".to_string(),
                "get" => "get".to_string(),
                "to_usize" | "to_string" => s.clone(),
                "wrapping_add" => "wrapping_add".to_string(),
                "wrapping_sub" => "wrapping_sub".to_string(),
                "ilog2" => "ilog2".to_string(),
                "bitxor" => "bitxor".to_string(),
                "shl" => "shl".to_string(),
                "shr" => "shr".to_string(),
                "map_or" => "map_or".to_string(),
                "deref" => "valueOf".to_string(),
                _ => s.clone(),
            }
        }
        MethodKind::Vole(v) => format!("{:?}", v).to_lowercase(),
        MethodKind::Unknown(s) => s.clone(),
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
    let name = if rust_name.starts_with("r#") { &rust_name[2..] } else { rust_name };
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

fn is_phantom_field(field: &IrField) -> bool {
    field.name == "_phantom"
        || field.name.starts_with("_phantom")
        || matches!(&field.ty, IrType::Struct { kind, .. } if kind.to_string().contains("PhantomData"))
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

fn ts_pattern_condition(pat: &IrPattern, match_var: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match pat {
        IrPattern::Lit(l) => {
            write!(f, "{} === ", match_var)?;
            ts_literal(l, f)
        }
        IrPattern::Ident { .. } => write!(f, "true"),
        IrPattern::Wild => write!(f, "true"),
        _ => write!(f, "true /* pattern {:?} */", pat),
    }
}

fn resolve_fn_generic(type_name: &str, generics: &[IrGenericParam]) -> Option<String> {
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
                        let dummy_map = BTreeMap::new();
                        let dummy_cx = TsContext { witness_map: &dummy_map };
                        let ret_str = format!("{}", TsFmt(TsTypeRefWriter { ty: ret }, &dummy_cx));
                        return Some(format!("(arg: {}) => {}", param_ty, ret_str));
                    }
                    TraitKind::AsRef(inner) => {
                        let dummy_map = BTreeMap::new();
                        let dummy_cx = TsContext { witness_map: &dummy_map };
                        let inner_str = format!("{}", TsFmt(TsTypeRefWriter { ty: inner }, &dummy_cx));
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
fn write_param_type(p: &IrParam, generics: &[IrGenericParam], f: &mut fmt::Formatter<'_>, cx: &TsContext<'_>) -> fmt::Result {
    if let IrType::TypeParam(tp) = &p.ty {
        if let Some(fn_type) = resolve_fn_generic(tp, generics) {
            return write!(f, "{}", fn_type);
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
    let types: Vec<&IrType> = s.fields.iter()
        .filter(|f| !is_phantom_field(f))
        .map(|f| &f.ty)
        .collect();
    collect_type_param_refs(&types)
}

/// Filter generics to only those actually used.
fn filter_used_generics<'a>(generics: &'a [IrGenericParam], used: &[String]) -> Vec<&'a IrGenericParam> {
    generics.iter()
        .filter(|g| {
            if g.kind != IrGenericParamKind::Type {
                return false;
            }
            if g.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::Fn(..))) {
                return false;
            }
            if g.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::AsRef(..))) {
                return false;
            }
            used.contains(&g.name)
        })
        .collect()
}
