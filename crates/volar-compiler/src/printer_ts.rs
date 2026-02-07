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
// PUBLIC API
// ============================================================================

/// Render an `IrModule` as a complete TypeScript source file.
pub fn print_module_ts(module: &IrModule) -> String {
    let mut out = String::new();
    let _ = write!(out, "{}", TsFmt(TsPreambleWriter));
    let _ = write!(out, "{}", TsFmt(TsModuleWriter { module }));
    out
}

// ============================================================================
// Display adapter (same pattern as printer.rs)
// ============================================================================

trait TsBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

struct TsFmt<T: TsBackend>(T);

impl<T: TsBackend> fmt::Display for TsFmt<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            TsClassWriter { s, impls: &impls }.fmt(f)?;
            writeln!(f)?;
        }

        // Emit standalone functions
        for func in &self.module.functions {
            TsFunctionWriter { func, indent: 0 }.fmt(f)?;
            writeln!(f)?;
        }

        // Any impls whose struct isn't in this module
        for (name, impls) in &impl_groups {
            writeln!(f, "// Orphan impls for {}", name)?;
            for imp in impls {
                for item in &imp.items {
                    if let IrImplItem::Method(func) = item {
                        TsFunctionWriter { func, indent: 0 }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.s.kind.to_string();
        let struct_generics = &self.s.generics;

        write!(f, "export class {}", name)?;
        TsGenericsWriter { generics: struct_generics }.fmt(f)?;
        writeln!(f, " {{")?;

        // Constructor with non-phantom fields
        let fields: Vec<&IrField> = self.s.fields.iter()
            .filter(|field| !is_phantom_field(field))
            .collect();

        writeln!(f, "  constructor(")?;
        for (i, field) in fields.iter().enumerate() {
            let comma = if i + 1 < fields.len() { "," } else { "," };
            write!(f, "    public {}: ", field.name)?;
            TsTypeWriter { ty: &field.ty }.fmt(f)?;
            writeln!(f, "{}", comma)?;
        }
        writeln!(f, "  ) {{}}")?;

        // Run analysis to detect overlapping methods
        let analysis = analyze_class_impls(self.impls);

        // Emit unique methods
        for im in &analysis.unique_methods {
            writeln!(f)?;
            TsMethodWriter { func: im.func, imp: im.imp, indent: 1 }.fmt(f)?;
        }

        // Emit merged methods (runtime dispatch)
        for mm in &analysis.merged_methods {
            writeln!(f)?;
            TsMergedMethodWriter {
                merged: mm,
                struct_fields: &self.s.fields,
                indent: 1,
            }.fmt(f)?;
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
}

impl<'a> TsMethodWriter<'a> {
    fn resolve_fn_type(&self, type_name: &str) -> Option<String> {
        resolve_fn_generic(type_name, &self.func.generics)
    }
}

impl<'a> TsBackend for TsMethodWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let name = ts_method_name(&self.func.name, self.imp.trait_.as_ref());

        let is_static = self.func.receiver.is_none();
        if is_static {
            write!(f, "{}static {}", ind, name)?;
        } else {
            write!(f, "{}{}", ind, name)?;
        }

        TsGenericsWriter { generics: &self.func.generics }.fmt(f)?;

        write!(f, "(")?;
        let params: Vec<&IrParam> = self.func.params.iter().collect();
        for (i, p) in params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", ts_param_name(&p.name))?;
            write_param_type(p, &self.func.generics, f)?;
        }
        write!(f, ")")?;

        if let Some(ret) = &self.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.fmt(f)?;
        }

        writeln!(f)?;
        TsBlockWriter { block: &self.func.body, indent: self.indent }.fmt(f)?;
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
}

impl<'a> TsBackend for TsMergedMethodWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        TsGenericsWriter { generics: &generics_owned }.fmt(f)?;

        // Parameters — use the broadest signature (first variant's params)
        write!(f, "(")?;
        let params: Vec<&IrParam> = first.func.params.iter().collect();
        for (i, p) in params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", ts_param_name(&p.name))?;
            write_param_type(p, &first.func.generics, f)?;
        }
        write!(f, ")")?;

        // Return type — use first variant's
        if let Some(ret) = &first.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.fmt(f)?;
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
                TsStmtWriter { stmt, indent: self.indent + 2 }.fmt(f)?;
            }
            if let Some(tail) = &body.expr {
                if is_statement_like(tail) {
                    write!(f, "{}", inner_ind)?;
                    TsExprWriter { expr: tail }.fmt(f)?;
                    writeln!(f, ";")?;
                } else {
                    write!(f, "{}return ", inner_ind)?;
                    TsExprWriter { expr: tail }.fmt(f)?;
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
}

impl<'a> TsBackend for TsFunctionWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let name = if self.func.name.starts_with("r#") { &self.func.name[2..] } else { &self.func.name };
        write!(f, "{}export function {}", ind, name)?;
        TsGenericsWriter { generics: &self.func.generics }.fmt(f)?;
        write!(f, "(")?;
        for (i, p) in self.func.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", ts_param_name(&p.name))?;
            write_param_type(p, &self.func.generics, f)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &self.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.fmt(f)?;
        }
        writeln!(f)?;
        TsBlockWriter { block: &self.func.body, indent: self.indent }.fmt(f)?;
        writeln!(f)?;
        Ok(())
    }
}

// ============================================================================
// GENERICS
// ============================================================================

struct TsGenericsWriter<'a> {
    generics: &'a [IrGenericParam],
}

impl<'a> TsBackend for TsGenericsWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            IrType::Primitive(p) => write!(f, "{}", ts_primitive(p))?,
            IrType::Vector { elem } => {
                TsTypeWriter { ty: elem }.fmt(f)?;
                write!(f, "[]")?;
            }
            IrType::Array { kind, elem, .. } => {
                if *kind == ArrayKind::Slice {
                    write!(f, "readonly ")?;
                    TsTypeWriter { ty: elem }.fmt(f)?;
                    write!(f, "[]")?;
                } else {
                    TsTypeWriter { ty: elem }.fmt(f)?;
                    write!(f, "[]")?;
                }
            }
            IrType::Struct { kind, type_args } => {
                let name = kind.to_string();
                if name == "Option" && type_args.len() == 1 {
                    write!(f, "(")?;
                    TsTypeWriter { ty: &type_args[0] }.fmt(f)?;
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
                        TsTypeWriter { ty: arg }.fmt(f)?;
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
                    TsTypeWriter { ty: elem }.fmt(f)?;
                }
                write!(f, "]")?;
            }
            IrType::Unit => write!(f, "void")?,
            IrType::Reference { elem, .. } => {
                TsTypeWriter { ty: elem }.fmt(f)?;
            }
            IrType::Projection { base, assoc, .. } => {
                write!(f, "number /* ")?;
                TsTypeWriter { ty: base }.fmt(f)?;
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
                            TsTypeWriter { ty: ret }.fmt(f)?;
                            return Ok(());
                        }
                        TraitKind::AsRef(ty) => {
                            TsTypeWriter { ty }.fmt(f)?;
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
                    TsTypeWriter { ty: p }.fmt(f)?;
                }
                write!(f, ") => ")?;
                TsTypeWriter { ty: ret }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        writeln!(f, "{}{{", ind)?;
        for stmt in &self.block.stmts {
            TsStmtWriter { stmt, indent: self.indent + 1 }.fmt(f)?;
        }
        if let Some(e) = &self.block.expr {
            let inner = "  ".repeat(self.indent + 1);
            if is_statement_like(e) {
                // Emit as a statement — do NOT prefix with `return`
                write!(f, "{}", inner)?;
                emit_statement_expr(e, self.indent + 1, f)?;
                writeln!(f)?;
            } else {
                write!(f, "{}return ", inner)?;
                TsExprWriter { expr: e }.fmt(f)?;
                writeln!(f, ";")?;
            }
        }
        write!(f, "{}}}", ind)?;
        Ok(())
    }
}

/// Emit an expression that is statement-like (loop, assignment, etc.) as a
/// proper statement. This avoids `return for(...)` in the output.
fn emit_statement_expr(e: &IrExpr, indent: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match e {
        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            write!(f, "for (let {} = ", var)?;
            TsExprWriter { expr: start }.fmt(f)?;
            write!(f, "; {} {} ", var, if *inclusive { "<=" } else { "<" })?;
            TsExprWriter { expr: end }.fmt(f)?;
            write!(f, "; {}++) ", var)?;
            TsBlockWriter { block: body, indent }.fmt(f)?;
        }
        IrExpr::IterLoop { pattern, collection, body } => {
            write!(f, "for (const ")?;
            TsPatternWriter { pat: pattern }.fmt(f)?;
            write!(f, " of ")?;
            TsExprWriter { expr: collection }.fmt(f)?;
            write!(f, ") ")?;
            TsBlockWriter { block: body, indent }.fmt(f)?;
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            write!(f, "if (")?;
            TsExprWriter { expr: cond }.fmt(f)?;
            write!(f, ") ")?;
            TsBlockWriter { block: then_branch, indent }.fmt(f)?;
            if let Some(eb) = else_branch {
                write!(f, " else ")?;
                match eb.as_ref() {
                    IrExpr::Block(b) => {
                        TsBlockWriter { block: b, indent }.fmt(f)?;
                    }
                    other if is_statement_like(other) => {
                        emit_statement_expr(other, indent, f)?;
                    }
                    other => {
                        write!(f, "{{ ")?;
                        TsExprWriter { expr: other }.fmt(f)?;
                        write!(f, "; }}")?;
                    }
                }
            }
        }
        IrExpr::Block(b) => {
            TsBlockWriter { block: b, indent }.fmt(f)?;
        }
        IrExpr::Assign { left, right } => {
            TsExprWriter { expr: left }.fmt(f)?;
            write!(f, " = ")?;
            TsExprWriter { expr: right }.fmt(f)?;
            write!(f, ";")?;
        }
        IrExpr::AssignOp { op, left, right } => {
            TsExprWriter { expr: left }.fmt(f)?;
            write!(f, " {}= ", ts_bin_op(*op))?;
            TsExprWriter { expr: right }.fmt(f)?;
            write!(f, ";")?;
        }
        other => {
            // Fallback: just emit as expression statement
            TsExprWriter { expr: other }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        match self.stmt {
            IrStmt::Let { pattern, ty, init } => {
                let is_mutable = pattern_is_mutable(pattern);
                let kw = if is_mutable { "let" } else { "const" };
                write!(f, "{}{} ", ind, kw)?;
                TsPatternWriter { pat: pattern }.fmt(f)?;
                if let Some(t) = ty {
                    write!(f, ": ")?;
                    TsTypeWriter { ty: t }.fmt(f)?;
                }
                if let Some(i) = init {
                    write!(f, " = ")?;
                    TsExprWriter { expr: i }.fmt(f)?;
                }
                writeln!(f, ";")?;
            }
            IrStmt::Semi(e) => {
                // Statement-like expressions get special treatment
                if is_statement_like(e) {
                    write!(f, "{}", ind)?;
                    emit_statement_expr(e, self.indent, f)?;
                    writeln!(f)?;
                } else {
                    write!(f, "{}", ind)?;
                    TsExprWriter { expr: e }.fmt(f)?;
                    writeln!(f, ";")?;
                }
            }
            IrStmt::Expr(e) => {
                if is_statement_like(e) {
                    write!(f, "{}", ind)?;
                    emit_statement_expr(e, self.indent, f)?;
                    writeln!(f)?;
                } else {
                    write!(f, "{}", ind)?;
                    TsExprWriter { expr: e }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.expr {
            IrExpr::Lit(l) => ts_literal(l, f)?,
            IrExpr::Var(v) => {
                let name = if v == "self" { "this" } else { v.as_str() };
                write!(f, "{}", name)?;
            }
            IrExpr::Binary { op, left, right } => {
                if let Some(helper) = bin_op_helper(*op) {
                    write!(f, "{}(", helper)?;
                    TsExprWriter { expr: left }.fmt(f)?;
                    write!(f, ", ")?;
                    TsExprWriter { expr: right }.fmt(f)?;
                    write!(f, ")")?;
                } else {
                    write!(f, "(")?;
                    TsExprWriter { expr: left }.fmt(f)?;
                    write!(f, " {} ", ts_bin_op(*op))?;
                    TsExprWriter { expr: right }.fmt(f)?;
                    write!(f, ")")?;
                }
            }
            IrExpr::Unary { op, expr } => {
                match op {
                    SpecUnaryOp::Neg => {
                        write!(f, "-")?;
                        TsExprWriter { expr }.fmt(f)?;
                    }
                    SpecUnaryOp::Not => {
                        write!(f, "!")?;
                        TsExprWriter { expr }.fmt(f)?;
                    }
                    SpecUnaryOp::Deref | SpecUnaryOp::Ref | SpecUnaryOp::RefMut => {
                        TsExprWriter { expr }.fmt(f)?;
                    }
                }
            }
            IrExpr::MethodCall { receiver, method, args, .. } => {
                emit_method_call(receiver, method, args, f)?;
            }
            IrExpr::Call { func, args } => {
                // Check for Some(x) → x and None → undefined
                if let IrExpr::Path { segments, .. } = func.as_ref() {
                    if segments.len() == 1 && segments[0] == "Some" && args.len() == 1 {
                        return TsExprWriter { expr: &args[0] }.fmt(f);
                    }
                    if segments.len() == 1 && segments[0] == "None" && args.is_empty() {
                        return write!(f, "undefined");
                    }
                }
                TsExprWriter { expr: func }.fmt(f)?;
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: arg }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrExpr::Field { base, field } => {
                TsExprWriter { expr: base }.fmt(f)?;
                write!(f, ".{}", field)?;
            }
            IrExpr::Index { base, index } => {
                TsExprWriter { expr: base }.fmt(f)?;
                write!(f, "[")?;
                TsExprWriter { expr: index }.fmt(f)?;
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
                // Use Object.assign pattern to handle field ordering:
                // new ClassName(Object.assign(new ClassName(0 as any,...), {f1: v1, f2: v2}))
                // Simpler: emit as positional but we need the right order.
                // Since we can't look up the struct here, emit a helper:
                write!(f, "Object.assign(Object.create({}.prototype), {{ ", name)?;
                for (i, (field_name, val)) in real_fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", field_name)?;
                    TsExprWriter { expr: val }.fmt(f)?;
                }
                write!(f, " }})")?;
            }
            IrExpr::Tuple(elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: e }.fmt(f)?;
                }
                write!(f, "]")?;
            }
            IrExpr::Array(elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: e }.fmt(f)?;
                }
                write!(f, "]")?;
            }
            IrExpr::Repeat { elem, len } => {
                write!(f, "Array.from({{length: ")?;
                TsExprWriter { expr: len }.fmt(f)?;
                write!(f, "}}, () => ")?;
                TsExprWriter { expr: elem }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Block(b) => {
                // Expression-position block → IIFE
                write!(f, "(() => ")?;
                TsBlockWriter { block: b, indent: 0 }.fmt(f)?;
                write!(f, ")()")?;
            }
            IrExpr::If { cond, then_branch, else_branch } => {
                // If in expression position → IIFE with if/else
                write!(f, "(() => {{ if (")?;
                TsExprWriter { expr: cond }.fmt(f)?;
                write!(f, ") ")?;
                TsBlockWriter { block: then_branch, indent: 0 }.fmt(f)?;
                if let Some(eb) = else_branch {
                    write!(f, " else ")?;
                    match eb.as_ref() {
                        IrExpr::Block(b) => {
                            TsBlockWriter { block: b, indent: 0 }.fmt(f)?;
                        }
                        IrExpr::If { .. } => {
                            // Nested if-else — recurse but strip IIFE wrapper
                            emit_if_chain(eb, f)?;
                        }
                        _ => {
                            write!(f, "{{ return ")?;
                            TsExprWriter { expr: eb }.fmt(f)?;
                            write!(f, "; }}")?;
                        }
                    }
                }
                write!(f, " }})()")?;
            }
            // Loops as expressions — wrap in IIFE when they appear in expr context
            IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
                write!(f, "for (let {} = ", var)?;
                TsExprWriter { expr: start }.fmt(f)?;
                write!(f, "; {} {} ", var, if *inclusive { "<=" } else { "<" })?;
                TsExprWriter { expr: end }.fmt(f)?;
                write!(f, "; {}++) ", var)?;
                TsBlockWriter { block: body, indent: 0 }.fmt(f)?;
            }
            IrExpr::IterLoop { pattern, collection, body } => {
                write!(f, "for (const ")?;
                TsPatternWriter { pat: pattern }.fmt(f)?;
                write!(f, " of ")?;
                TsExprWriter { expr: collection }.fmt(f)?;
                write!(f, ") ")?;
                TsBlockWriter { block: body, indent: 0 }.fmt(f)?;
            }
            IrExpr::IterPipeline(chain) => {
                TsIterChainWriter { chain }.fmt(f)?;
            }
            IrExpr::RawMap { receiver, elem_var, body } => {
                TsExprWriter { expr: receiver }.fmt(f)?;
                write!(f, ".map((")?;
                TsPatternWriter { pat: elem_var }.fmt(f)?;
                write!(f, ") => ")?;
                TsExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::RawZip { left, right, left_var, right_var, body } => {
                TsExprWriter { expr: left }.fmt(f)?;
                write!(f, ".map((")?;
                TsPatternWriter { pat: left_var }.fmt(f)?;
                write!(f, ", __i) => {{ const ")?;
                TsPatternWriter { pat: right_var }.fmt(f)?;
                write!(f, " = ")?;
                TsExprWriter { expr: right }.fmt(f)?;
                write!(f, "[__i]; return ")?;
                TsExprWriter { expr: body }.fmt(f)?;
                write!(f, "; }})")?;
            }
            IrExpr::RawFold { receiver, init, acc_var, elem_var, body } => {
                TsExprWriter { expr: receiver }.fmt(f)?;
                write!(f, ".reduce((")?;
                TsPatternWriter { pat: acc_var }.fmt(f)?;
                write!(f, ", ")?;
                TsPatternWriter { pat: elem_var }.fmt(f)?;
                write!(f, ") => ")?;
                TsExprWriter { expr: body }.fmt(f)?;
                write!(f, ", ")?;
                TsExprWriter { expr: init }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Closure { params, body, .. } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsPatternWriter { pat: &p.pattern }.fmt(f)?;
                    if let Some(t) = &p.ty {
                        write!(f, ": ")?;
                        TsTypeWriter { ty: t }.fmt(f)?;
                    }
                }
                write!(f, ") => ")?;
                TsExprWriter { expr: body }.fmt(f)?;
            }
            IrExpr::Range { start, end, inclusive } => {
                write!(f, "Array.from({{length: ")?;
                if let Some(e) = end {
                    TsExprWriter { expr: e }.fmt(f)?;
                    if *inclusive {
                        write!(f, " + 1")?;
                    }
                } else {
                    write!(f, "0")?;
                }
                if let Some(s) = start {
                    write!(f, " - ")?;
                    TsExprWriter { expr: s }.fmt(f)?;
                    write!(f, "}}, (_, i) => i + ")?;
                    TsExprWriter { expr: s }.fmt(f)?;
                } else {
                    write!(f, "}}, (_, i) => i")?;
                }
                write!(f, ")")?;
            }
            IrExpr::Assign { left, right } => {
                TsExprWriter { expr: left }.fmt(f)?;
                write!(f, " = ")?;
                TsExprWriter { expr: right }.fmt(f)?;
            }
            IrExpr::AssignOp { op, left, right } => {
                TsExprWriter { expr: left }.fmt(f)?;
                write!(f, " {}= ", ts_bin_op(*op))?;
                TsExprWriter { expr: right }.fmt(f)?;
            }
            IrExpr::Return(e) => {
                write!(f, "return")?;
                if let Some(e) = e {
                    write!(f, " ")?;
                    TsExprWriter { expr: e }.fmt(f)?;
                }
            }
            IrExpr::Cast { expr, ty } => {
                match ty.as_ref() {
                    IrType::Primitive(PrimitiveType::U32) => {
                        write!(f, "Number(")?;
                        TsExprWriter { expr }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    IrType::Primitive(PrimitiveType::U64) => {
                        write!(f, "BigInt(")?;
                        TsExprWriter { expr }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    IrType::Primitive(PrimitiveType::U8) => {
                        write!(f, "((")?;
                        TsExprWriter { expr }.fmt(f)?;
                        write!(f, ") & 0xFF)")?;
                    }
                    IrType::Primitive(PrimitiveType::Usize) => {
                        write!(f, "Number(")?;
                        TsExprWriter { expr }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    _ => {
                        write!(f, "(")?;
                        TsExprWriter { expr }.fmt(f)?;
                        write!(f, " as unknown as ")?;
                        TsTypeWriter { ty }.fmt(f)?;
                        write!(f, ")")?;
                    }
                }
            }
            IrExpr::TypenumUsize { ty } => {
                write!(f, "/* <")?;
                TsTypeWriter { ty }.fmt(f)?;
                write!(f, " as Unsigned>.USIZE */")?;
            }
            IrExpr::Unreachable => write!(f, "(() => {{ throw new Error(\"unreachable\"); }})()")?,
            IrExpr::ArrayDefault { elem_ty, len } => {
                write!(f, "Array.from({{length: ")?;
                ts_length(len, f)?;
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
                    ts_default_value(t, f)?;
                } else {
                    write!(f, "undefined")?;
                }
            }
            IrExpr::LengthOf(len) => {
                ts_length(len, f)?;
            }
            IrExpr::ArrayGenerate { len, index_var, body, .. } => {
                write!(f, "Array.from({{length: ")?;
                ts_length(len, f)?;
                write!(f, "}}, (_, {}) => ", index_var)?;
                TsExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Match { expr, arms } => {
                write!(f, "(() => {{ ")?;
                write!(f, "const __match = ")?;
                TsExprWriter { expr }.fmt(f)?;
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
                    TsExprWriter { expr: &arm.body }.fmt(f)?;
                    write!(f, "; }}")?;
                }
                write!(f, " }})()")?;
            }
            IrExpr::Break(_) => write!(f, "break")?,
            IrExpr::Continue => write!(f, "continue")?,
            IrExpr::Try(e) => {
                TsExprWriter { expr: e }.fmt(f)?;
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
fn emit_if_chain(expr: &IrExpr, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let IrExpr::If { cond, then_branch, else_branch } = expr {
        write!(f, "if (")?;
        TsExprWriter { expr: cond }.fmt(f)?;
        write!(f, ") ")?;
        TsBlockWriter { block: then_branch, indent: 0 }.fmt(f)?;
        if let Some(eb) = else_branch {
            write!(f, " else ")?;
            match eb.as_ref() {
                IrExpr::Block(b) => {
                    TsBlockWriter { block: b, indent: 0 }.fmt(f)?;
                }
                IrExpr::If { .. } => {
                    emit_if_chain(eb, f)?;
                }
                _ => {
                    write!(f, "{{ return ")?;
                    TsExprWriter { expr: eb }.fmt(f)?;
                    write!(f, "; }}")?;
                }
            }
        }
    }
    Ok(())
}

/// Emit a method call expression with smart translation.
fn emit_method_call(receiver: &IrExpr, method: &MethodKind, args: &[IrExpr], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let name = ts_method_call_name(method);
    match name.as_str() {
        "wrapping_add" if args.len() == 1 => {
            write!(f, "wrappingAdd(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "wrapping_sub" if args.len() == 1 => {
            write!(f, "wrappingSub(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "ilog2" if args.is_empty() => {
            write!(f, "ilog2(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ")")
        }
        "clone" if args.is_empty() => {
            write!(f, "structuredClone(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ")")
        }
        "into" if args.is_empty() => {
            TsExprWriter { expr: receiver }.fmt(f)
        }
        "cloned" if args.is_empty() => {
            TsExprWriter { expr: receiver }.fmt(f)
        }
        "to_vec" if args.is_empty() => {
            write!(f, "[...")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, "]")
        }
        "as_slice" if args.is_empty() => {
            TsExprWriter { expr: receiver }.fmt(f)
        }
        "len" if args.is_empty() => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ".length")
        }
        "contains" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ".includes(")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "get" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, "?.[")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, "]")
        }
        "unwrap_or_default" if args.is_empty() => {
            write!(f, "(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, " ?? 0)")
        }
        "map_or" if args.len() == 2 => {
            // Option.map_or(default, f) → (x != null ? f(x) : default)
            write!(f, "((")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ") != null ? (")?;
            TsExprWriter { expr: &args[1] }.fmt(f)?;
            write!(f, ")(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ") : (")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, "))")
        }
        "update" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ".update(")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "finalize" if args.is_empty() => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ".finalize()")
        }
        "bitxor" if args.len() == 1 => {
            write!(f, "fieldBitxor(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "shl" if args.len() == 1 => {
            write!(f, "fieldShl(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "shr" if args.len() == 1 => {
            write!(f, "fieldShr(")?;
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ", ")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")")
        }
        "at" | "valueOf" if args.len() == 1 => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, "[(")?;
            TsExprWriter { expr: &args[0] }.fmt(f)?;
            write!(f, ")]")
        }
        "at" if args.is_empty() => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ".at(0)")
        }
        _ => {
            TsExprWriter { expr: receiver }.fmt(f)?;
            write!(f, ".{}(", name)?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                TsExprWriter { expr: arg }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        TsIterSourceWriter { source: &self.chain.source }.fmt(f)?;

        for step in &self.chain.steps {
            match step {
                IterStep::Map { var, body } => {
                    write!(f, ".map((")?;
                    TsPatternWriter { pat: var }.fmt(f)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Filter { var, body } => {
                    write!(f, ".filter((")?;
                    TsPatternWriter { pat: var }.fmt(f)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::FilterMap { var, body } => {
                    write!(f, ".map((")?;
                    TsPatternWriter { pat: var }.fmt(f)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.fmt(f)?;
                    write!(f, ").filter((__x) => __x !== undefined)")?;
                }
                IterStep::FlatMap { var, body } => {
                    write!(f, ".flatMap((")?;
                    TsPatternWriter { pat: var }.fmt(f)?;
                    write!(f, ") => ")?;
                    TsExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Enumerate => {
                    write!(f, ".map((val, i) => [i, val] as [number, typeof val])")?;
                }
                IterStep::Take { count } => {
                    write!(f, ".slice(0, ")?;
                    TsExprWriter { expr: count }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Skip { count } => {
                    write!(f, ".slice(")?;
                    TsExprWriter { expr: count }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Chain { other } => {
                    write!(f, ".concat(")?;
                    TsIterChainWriter { chain: other }.fmt(f)?;
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
                TsPatternWriter { pat: acc_var }.fmt(f)?;
                write!(f, ", ")?;
                TsPatternWriter { pat: elem_var }.fmt(f)?;
                write!(f, ") => ")?;
                TsExprWriter { expr: body }.fmt(f)?;
                write!(f, ", ")?;
                TsExprWriter { expr: init }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.source {
            IterChainSource::Method { collection, method } => {
                match method {
                    IterMethod::Iter | IterMethod::IntoIter => {
                        TsExprWriter { expr: collection }.fmt(f)?;
                    }
                    IterMethod::Chars => {
                        TsExprWriter { expr: collection }.fmt(f)?;
                        write!(f, ".split(\"\")")?;
                    }
                    IterMethod::Bytes => {
                        write!(f, "Array.from(")?;
                        TsExprWriter { expr: collection }.fmt(f)?;
                        write!(f, ")")?;
                    }
                }
            }
            IterChainSource::Range { start, end, inclusive } => {
                write!(f, "Array.from({{length: ")?;
                TsExprWriter { expr: end }.fmt(f)?;
                if *inclusive {
                    write!(f, " + 1")?;
                }
                write!(f, " - ")?;
                TsExprWriter { expr: start }.fmt(f)?;
                write!(f, "}}, (_, __i) => __i + ")?;
                TsExprWriter { expr: start }.fmt(f)?;
                write!(f, ")")?;
            }
            IterChainSource::Zip { left, right } => {
                TsIterChainWriter { chain: left }.fmt(f)?;
                write!(f, ".map((__a, __i) => [__a, ")?;
                TsIterChainWriter { chain: right }.fmt(f)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                    TsPatternWriter { pat: p }.fmt(f)?;
                }
                write!(f, "]")?;
            }
            IrPattern::TupleStruct { kind, elems } => {
                if elems.len() == 1 {
                    TsPatternWriter { pat: &elems[0] }.fmt(f)?;
                } else {
                    write!(f, "/* {}(...) */ [", kind)?;
                    for (i, p) in elems.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        TsPatternWriter { pat: p }.fmt(f)?;
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
                            TsPatternWriter { pat: p }.fmt(f)?;
                        }
                    } else {
                        write!(f, "{}: ", name)?;
                        TsPatternWriter { pat: p }.fmt(f)?;
                    }
                }
                if *rest {
                    // Don't emit ...__rest — it causes duplicate identifier errors.
                    // The rest is unused in our generated code anyway.
                }
                write!(f, " }}")?;
            }
            IrPattern::Ref { pat, .. } => {
                TsPatternWriter { pat }.fmt(f)?;
            }
            IrPattern::Lit(l) => {
                ts_literal(l, f)?;
            }
            IrPattern::Or(pats) => {
                if let Some(first) = pats.first() {
                    TsPatternWriter { pat: first }.fmt(f)?;
                }
            }
            IrPattern::Slice(pats) => {
                write!(f, "[")?;
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsPatternWriter { pat: p }.fmt(f)?;
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

fn ts_length(len: &ArrayLength, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match len {
        ArrayLength::Const(n) => write!(f, "{}", n),
        ArrayLength::TypeParam(p) => write!(f, "{}", p.to_lowercase()),
        ArrayLength::Projection { r#type, field, .. } => {
            write!(f, "/* ")?;
            TsTypeWriter { ty: r#type }.fmt(f)?;
            write!(f, "::{} */ 0", field)
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
                        let ret_str = format!("{}", TsFmt(TsTypeRefWriter { ty: ret }));
                        return Some(format!("(arg: {}) => {}", param_ty, ret_str));
                    }
                    TraitKind::AsRef(inner) => {
                        let inner_str = format!("{}", TsFmt(TsTypeRefWriter { ty: inner }));
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
fn write_param_type(p: &IrParam, generics: &[IrGenericParam], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let IrType::TypeParam(tp) = &p.ty {
        if let Some(fn_type) = resolve_fn_generic(tp, generics) {
            return write!(f, "{}", fn_type);
        }
    }
    if let IrType::Existential { bounds } = &p.ty {
        if let Some(b) = bounds.first() {
            match &b.trait_kind {
                TraitKind::AsRef(inner) => {
                    return TsTypeWriter { ty: inner }.fmt(f);
                }
                TraitKind::Fn(input, ret) => {
                    let param_ty = match input {
                        FnInput::BytesSlice => "Uint8Array",
                        FnInput::Size => "number",
                        FnInput::Bool => "boolean",
                    };
                    write!(f, "(arg: {}) => ", param_ty)?;
                    return TsTypeWriter { ty: ret }.fmt(f);
                }
                _ => {}
            }
        }
    }
    TsTypeWriter { ty: &p.ty }.fmt(f)
}

struct TsTypeRefWriter<'a> {
    ty: &'a IrType,
}

impl<'a> TsBackend for TsTypeRefWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        TsTypeWriter { ty: self.ty }.fmt(f)
    }
}
