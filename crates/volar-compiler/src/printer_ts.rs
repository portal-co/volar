//! TypeScript code emitter for dyn-lowered IR.
//!
//! Consumes an `IrModule` (typically after `lowering_dyn`) and renders it as
//! TypeScript source text.  Structs become classes, impl methods attach to the
//! class body, and iterator pipelines map to `Array.prototype` methods.

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
// PREAMBLE
// ============================================================================

struct TsPreambleWriter;

impl TsBackend for TsPreambleWriter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "// Auto-generated TypeScript from volar-spec")?;
        writeln!(f, "// Type-level lengths have been converted to runtime number witnesses")?;
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
        // Group impl items by self_ty so we can merge them into class bodies.
        // Key: struct name → Vec of (trait_ref, items)
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

        // Any impls whose struct isn't in this module (shouldn't happen normally)
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

        // Collect all type params from the struct + all impls
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

        // Methods from impls
        for imp in self.impls {
            for item in &imp.items {
                match item {
                    IrImplItem::Method(func) => {
                        writeln!(f)?;
                        TsMethodWriter { func, imp, indent: 1 }.fmt(f)?;
                    }
                    IrImplItem::AssociatedType { .. } => {
                        // TypeScript doesn't have associated types
                    }
                }
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

// ============================================================================
// METHOD (inside a class)
// ============================================================================

struct TsMethodWriter<'a> {
    func: &'a IrFunction,
    imp: &'a IrImpl,
    indent: usize,
}

impl<'a> TsMethodWriter<'a> {
    /// If a generic param is Fn-bounded, return the TS function type string.
    fn resolve_fn_type(&self, type_name: &str) -> Option<String> {
        resolve_fn_generic(type_name, &self.func.generics)
    }
}

impl<'a> TsBackend for TsMethodWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ind = "  ".repeat(self.indent);
        let name = ts_method_name(&self.func.name, self.imp.trait_.as_ref());

        // Determine if this is a static method (no receiver)
        let is_static = self.func.receiver.is_none();
        if is_static {
            write!(f, "{}static {}", ind, name)?;
        } else {
            write!(f, "{}{}", ind, name)?;
        }

        // Method-level generics (exclude the struct's own generics)
        TsGenericsWriter { generics: &self.func.generics }.fmt(f)?;

        // Parameters (skip receiver — it's implicit `this`)
        write!(f, "(")?;
        let params: Vec<&IrParam> = self.func.params.iter().collect();
        for (i, p) in params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", ts_param_name(&p.name))?;
            // Resolve Fn-bounded generics inline
            if let IrType::TypeParam(tp) = &p.ty {
                if let Some(fn_type) = self.resolve_fn_type(tp) {
                    write!(f, "{}", fn_type)?;
                } else {
                    TsTypeWriter { ty: &p.ty }.fmt(f)?;
                }
            } else if let IrType::Existential { bounds } = &p.ty {
                // `impl AsRef<[u8]>` → just the inner type
                if let Some(b) = bounds.first() {
                    match &b.trait_kind {
                        TraitKind::AsRef(inner) => {
                            TsTypeWriter { ty: inner }.fmt(f)?;
                        }
                        TraitKind::Fn(input, ret) => {
                            let param_ty = match input {
                                FnInput::BytesSlice => "Uint8Array",
                                FnInput::Size => "number",
                                FnInput::Bool => "boolean",
                            };
                            write!(f, "(arg: {}) => ", param_ty)?;
                            TsTypeWriter { ty: ret }.fmt(f)?;
                        }
                        _ => TsTypeWriter { ty: &p.ty }.fmt(f)?,
                    }
                } else {
                    TsTypeWriter { ty: &p.ty }.fmt(f)?;
                }
            } else {
                TsTypeWriter { ty: &p.ty }.fmt(f)?;
            }
        }
        write!(f, ")")?;

        // Return type
        if let Some(ret) = &self.func.return_type {
            write!(f, ": ")?;
            TsTypeWriter { ty: ret }.fmt(f)?;
        }

        // Body
        writeln!(f)?;
        TsBlockWriter { block: &self.func.body, indent: self.indent }.fmt(f)?;
        writeln!(f)?;
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
            if let IrType::TypeParam(tp) = &p.ty {
                if let Some(fn_type) = resolve_fn_generic(tp, &self.func.generics) {
                    write!(f, "{}", fn_type)?;
                } else {
                    TsTypeWriter { ty: &p.ty }.fmt(f)?;
                }
            } else if let IrType::Existential { bounds } = &p.ty {
                if let Some(b) = bounds.first() {
                    match &b.trait_kind {
                        TraitKind::AsRef(inner) => {
                            TsTypeWriter { ty: inner }.fmt(f)?;
                        }
                        TraitKind::Fn(input, ret) => {
                            let param_ty = match input {
                                FnInput::BytesSlice => "Uint8Array",
                                FnInput::Size => "number",
                                FnInput::Bool => "boolean",
                            };
                            write!(f, "(arg: {}) => ", param_ty)?;
                            TsTypeWriter { ty: ret }.fmt(f)?;
                        }
                        _ => TsTypeWriter { ty: &p.ty }.fmt(f)?,
                    }
                } else {
                    TsTypeWriter { ty: &p.ty }.fmt(f)?;
                }
            } else {
                TsTypeWriter { ty: &p.ty }.fmt(f)?;
            }
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
        // Filter out lifetime params, const params (already lowered to runtime),
        // and Fn-like params (become function params instead of generics)
        let type_params: Vec<&IrGenericParam> = self.generics.iter()
            .filter(|p| {
                if p.kind != IrGenericParamKind::Type {
                    return false;
                }
                // Skip Fn-like generic params — they become regular function type params
                if p.bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::Fn(..))) {
                    return false;
                }
                // Skip AsRef-like params too
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
                // Special-case Option → T | undefined
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
                    // In TS, `Self` doesn't exist — emit `this` for return types
                    // or just emit the type param name and let TS infer
                    write!(f, "this")?;
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
                // Erase references
                TsTypeWriter { ty: elem }.fmt(f)?;
            }
            IrType::Projection { base, assoc, .. } => {
                // Best effort: just use the assoc name as a number
                write!(f, "number /* ")?;
                TsTypeWriter { ty: base }.fmt(f)?;
                write!(f, "::{} */", assoc)?;
            }
            IrType::Existential { bounds } => {
                // Map Fn-like bounds to TS function types
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
// BLOCK
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
            write!(f, "{}return ", inner)?;
            TsExprWriter { expr: e }.fmt(f)?;
            writeln!(f, ";")?;
        }
        write!(f, "{}}}", ind)?;
        Ok(())
    }
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
                write!(f, "{}", ind)?;
                TsExprWriter { expr: e }.fmt(f)?;
                writeln!(f, ";")?;
            }
            IrStmt::Expr(e) => {
                write!(f, "{}", ind)?;
                TsExprWriter { expr: e }.fmt(f)?;
                writeln!(f, ";")?;
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
                // Use helpers for field-element ops, infix for plain number ops
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
                        // Erase ref/deref in TypeScript
                        TsExprWriter { expr }.fmt(f)?;
                    }
                }
            }
            IrExpr::MethodCall { receiver, method, args, .. } => {
                let name = ts_method_call_name(method);
                match name.as_str() {
                    "wrapping_add" if args.len() == 1 => {
                        write!(f, "wrappingAdd(")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ", ")?;
                        TsExprWriter { expr: &args[0] }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    "wrapping_sub" if args.len() == 1 => {
                        write!(f, "wrappingSub(")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ", ")?;
                        TsExprWriter { expr: &args[0] }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    "ilog2" if args.is_empty() => {
                        write!(f, "ilog2(")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    "clone" if args.is_empty() => {
                        // For primitives, clone is identity; for objects, call .clone()
                        // We emit structuredClone as a safe default
                        write!(f, "structuredClone(")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    "into" if args.is_empty() => {
                        // Type conversion — just pass through
                        TsExprWriter { expr: receiver }.fmt(f)?;
                    }
                    "cloned" if args.is_empty() => {
                        // .cloned() on iterators → identity (TS arrays are already values)
                        TsExprWriter { expr: receiver }.fmt(f)?;
                    }
                    "to_vec" if args.is_empty() => {
                        write!(f, "[...")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, "]")?;
                    }
                    "as_slice" if args.is_empty() => {
                        TsExprWriter { expr: receiver }.fmt(f)?;
                    }
                    "len" if args.is_empty() => {
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ".length")?;
                    }
                    "contains" if args.len() == 1 => {
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ".includes(")?;
                        TsExprWriter { expr: &args[0] }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    "get" if args.len() == 1 => {
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, "?.[")?;
                        TsExprWriter { expr: &args[0] }.fmt(f)?;
                        write!(f, "]")?;
                    }
                    "unwrap_or_default" if args.is_empty() => {
                        write!(f, "(")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, " ?? 0)")?;
                    }
                    "update" if args.len() == 1 => {
                        // Digest.update
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ".update(")?;
                        TsExprWriter { expr: &args[0] }.fmt(f)?;
                        write!(f, ")")?;
                    }
                    "finalize" if args.is_empty() => {
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ".finalize()")?;
                    }
                    "bitxor" if args.len() == 1 => {
                        write!(f, "fieldBitxor(")?;
                        TsExprWriter { expr: receiver }.fmt(f)?;
                        write!(f, ", ")?;
                        TsExprWriter { expr: &args[0] }.fmt(f)?;
                        write!(f, ")")?;
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
                        write!(f, ")")?;
                    }
                }
            }
            IrExpr::Call { func, args } => {
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
                // No turbofish in TS; map known paths
                if segments.len() == 2 && segments[0] == "AsRef" && segments[1] == "as_ref" {
                    write!(f, "asRefU8")?;
                } else if segments.len() == 2 && segments[0] == "D" && segments[1] == "new" {
                    // D::new() → new D() — but D is a type param for Digest
                    write!(f, "new D")?;
                } else if segments.len() == 1 && segments[0] == "double_vec" {
                    write!(f, "doubleVec")?;
                } else if segments.len() == 1 && segments[0] == "commit" {
                    write!(f, "commit")?;
                } else if segments.len() >= 2 {
                    write!(f, "{}", segments.join("."))?;
                } else {
                    write!(f, "{}", segments.join("."))?;
                }
            }
            IrExpr::StructExpr { kind, fields, .. } => {
                let name = kind.to_string();
                // Filter out phantom fields
                let real_fields: Vec<&(String, IrExpr)> = fields.iter()
                    .filter(|(n, _)| !n.starts_with("_phantom"))
                    .collect();
                write!(f, "new {}(", name)?;
                for (i, (_, val)) in real_fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TsExprWriter { expr: val }.fmt(f)?;
                }
                write!(f, ")")?;
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
                // If in expression position → ternary or IIFE
                // For simplicity, always emit if/else statement form
                write!(f, "(() => {{ if (")?;
                TsExprWriter { expr: cond }.fmt(f)?;
                write!(f, ") ")?;
                TsBlockWriter { block: then_branch, indent: 0 }.fmt(f)?;
                if let Some(eb) = else_branch {
                    write!(f, " else ")?;
                    // else_branch is an IrExpr (could be another If or Block)
                    match eb.as_ref() {
                        IrExpr::Block(b) => {
                            TsBlockWriter { block: b, indent: 0 }.fmt(f)?;
                        }
                        IrExpr::If { .. } => {
                            TsExprWriter { expr: eb }.fmt(f)?;
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
                // Ranges in TS → Array.from helper
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
                // Map Rust `as` casts to TS
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
                // This should have been lowered, but render best-effort
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
                // Emit as IIFE with if/else chain
                write!(f, "(() => {{ ")?;
                write!(f, "const __match = ")?;
                TsExprWriter { expr }.fmt(f)?;
                write!(f, "; ")?;
                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        write!(f, " else ")?;
                    }
                    // Simplified: treat all patterns as `else` (last) or condition
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
                // Just emit the inner expression (no ? operator in TS)
                TsExprWriter { expr: e }.fmt(f)?;
            }
            _ => {
                write!(f, "undefined /* unsupported: {:?} */", std::any::type_name::<IrExpr>())?;
            }
        }
        Ok(())
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
        // Source
        TsIterSourceWriter { source: &self.chain.source }.fmt(f)?;

        // Steps
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

        // Terminal
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
            IterTerminal::Lazy => {
                // No terminal — used by for-of loops etc.
            }
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
                // .iter()/.into_iter() are identity on TS arrays — just emit the array
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
                // zip: left.map((a, i) => [a, right[i]])
                // But we need to track through... just use the left + index into right
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
                // In TS, destructure the wrapper: `const { _0: x } = expr`
                // But in expression context, we just use the inner binding
                if elems.len() == 1 {
                    // Newtype destructuring: `const x = expr.value` handled at stmt level
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
                    // Check if destructuring to same name
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
                    if !fields.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "...__rest")?;
                }
                write!(f, " }}")?;
            }
            IrPattern::Ref { pat, .. } => {
                // Erase ref
                TsPatternWriter { pat }.fmt(f)?;
            }
            IrPattern::Lit(l) => {
                ts_literal(l, f)?;
            }
            IrPattern::Or(pats) => {
                // TS doesn't support or-patterns in destructuring
                // Just emit the first one
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

/// For binary ops that might operate on field elements, return the helper
/// function name.  When both operands are known primitives (`number`/`bigint`),
/// we use native infix instead — but we can't know operand types at print
/// time without a type environment.  So we unconditionally use helpers for
/// arithmetic/bitwise ops and let the helpers dispatch based on operand type.
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
        // Comparison and logical ops are always safe as infix
        _ => None,
    }
}

fn ts_literal(l: &IrLit, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match l {
        IrLit::Int(n) => {
            // If the value fits in a safe integer, emit as number; else bigint
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
            // In the dyn-lowered IR these become <<Type>::Field as Unsigned>::to_usize()
            // In TS, the lowering should have already replaced this with a runtime var
            // Fallback: emit a comment
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
                // Map Rust std methods to TS equivalents
                "clone" => "clone".to_string(),
                "into" => "into".to_string(),
                "len" => "length".to_string(),
                "is_empty" => "length === 0 ? true : false; /* ".to_string(), // hack
                "contains" => "includes".to_string(),
                "unwrap" | "unwrap_or_default" => "valueOf".to_string(),
                "as_ref" => "valueOf".to_string(),
                "as_slice" => "slice".to_string(),
                "get" => "at".to_string(),
                "to_usize" | "to_string" => s.clone(),
                "wrapping_add" => "wrapping_add".to_string(),
                "wrapping_sub" => "wrapping_sub".to_string(),
                "ilog2" => "ilog2".to_string(),
                "bitxor" => "bitxor".to_string(),
                "deref" => "valueOf".to_string(),
                _ => s.clone(),
            }
        }
        MethodKind::Vole(v) => format!("{:?}", v).to_lowercase(),
        MethodKind::Unknown(s) => s.clone(),
    }
}

/// Map trait impl method names to TS method names on the class.
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
    // Escape TS reserved words
    let name = if rust_name.starts_with("r#") { &rust_name[2..] } else { rust_name };
    match name {
        "static" => "static_".to_string(),
        "delete" => "delete_".to_string(),
        "class" => "class_".to_string(),
        _ => name.to_string(),
    }
}

fn ts_param_name(name: &str) -> &str {
    // `self` doesn't appear as a param in TS (it's `this`)
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

/// If a generic parameter named `type_name` has an Fn-like trait bound,
/// return the corresponding TypeScript function type string.
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
                        // Format the return type
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

/// Lightweight type renderer for use outside fmt::Formatter context.
struct TsTypeRefWriter<'a> {
    ty: &'a IrType,
}

impl<'a> TsBackend for TsTypeRefWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        TsTypeWriter { ty: self.ty }.fmt(f)
    }
}
