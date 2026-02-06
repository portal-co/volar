use core::fmt::{self, Write};

#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::ir::*;

// ============================================================================
// TRAIT + DISPLAY ADAPTER
// ============================================================================

/// Backend trait for rendering IR nodes to Rust source text.
///
/// Implementors hold a reference to an IR node plus any extra context
/// (indentation, config, deps) and write themselves via `fmt`.
pub trait RustBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

/// Display adapter — wraps any `RustBackend` to implement `core::fmt::Display`.
///
/// Usage: `format!("{}", DisplayRust(SomeWriter { ... }))`
pub struct DisplayRust<T: RustBackend>(pub T);

impl<T: RustBackend> fmt::Display for DisplayRust<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

// ============================================================================
// WRITER STRUCTS
// ============================================================================

/// Renders an `IrModule` body (structs, traits, impls, functions) without preamble.
pub struct ModuleWriter<'a> {
    pub module: &'a IrModule,
}

pub struct StructWriter<'a> {
    pub s: &'a IrStruct,
}

pub struct TraitWriter<'a> {
    pub t: &'a IrTrait,
}

pub struct ImplWriter<'a> {
    pub i: &'a IrImpl,
}

pub struct FunctionWriter<'a> {
    pub f: &'a IrFunction,
    pub level: usize,
    pub is_trait_item: bool,
}

pub struct GenericsWriter<'a> {
    pub generics: &'a [IrGenericParam],
}

pub struct WhereClauseWriter<'a> {
    pub where_clause: &'a [IrWherePredicate],
}

pub struct TraitBoundWriter<'a> {
    pub bound: &'a IrTraitBound,
}

pub struct TypeWriter<'a> {
    pub ty: &'a IrType,
}

pub struct BlockWriter<'a> {
    pub block: &'a IrBlock,
    pub level: usize,
}

pub struct StmtWriter<'a> {
    pub stmt: &'a IrStmt,
    pub level: usize,
}

pub struct ExprWriter<'a> {
    pub expr: &'a IrExpr,
}

/// Writes an expression as part of an iterator chain (without final `.collect()`).
pub struct ExprChainWriter<'a> {
    pub expr: &'a IrExpr,
}

pub struct PatternWriter<'a> {
    pub pat: &'a IrPattern,
}

pub struct ReceiverWriter {
    pub r: IrReceiver,
}

// ============================================================================
// IMPLEMENTATIONS
// ============================================================================

impl<'a> RustBackend for ModuleWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.module.structs {
            StructWriter { s }.fmt(f)?;
            writeln!(f)?;
        }
        for t in &self.module.traits {
            TraitWriter { t }.fmt(f)?;
            writeln!(f)?;
        }
        for i in &self.module.impls {
            ImplWriter { i }.fmt(f)?;
            writeln!(f)?;
        }
        for func in &self.module.functions {
            FunctionWriter {
                f: func,
                level: 0,
                is_trait_item: false,
            }
            .fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'a> RustBackend for StructWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "#[derive(Debug, Default)]")?;
        write!(f, "pub struct {}", self.s.kind)?;
        GenericsWriter {
            generics: &self.s.generics,
        }
        .fmt(f)?;

        if self.s.is_tuple {
            write!(f, "(")?;
            for (i, field) in self.s.fields.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "pub ")?;
                TypeWriter { ty: &field.ty }.fmt(f)?;
            }
            writeln!(f, ");")?;
        } else {
            writeln!(f, " {{")?;
            for field in &self.s.fields {
                write!(f, "    pub {}: ", field.name)?;
                TypeWriter { ty: &field.ty }.fmt(f)?;
                writeln!(f, ",")?;
            }
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}

impl<'a> RustBackend for TraitWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pub trait {}", self.t.kind)?;
        GenericsWriter {
            generics: &self.t.generics,
        }
        .fmt(f)?;
        if !self.t.super_traits.is_empty() {
            write!(f, ": ")?;
            for (j, st) in self.t.super_traits.iter().enumerate() {
                if j > 0 {
                    write!(f, " + ")?;
                }
                TraitBoundWriter { bound: st }.fmt(f)?;
            }
        }
        writeln!(f, " {{")?;
        for item in &self.t.items {
            match item {
                IrTraitItem::Method(m) => {
                    write!(f, "    fn {}", m.name)?;
                    GenericsWriter {
                        generics: &m.generics,
                    }
                    .fmt(f)?;
                    write!(f, "(")?;
                    if let Some(r) = m.receiver {
                        ReceiverWriter { r }.fmt(f)?;
                        if !m.params.is_empty() {
                            write!(f, ", ")?;
                        }
                    }
                    for (i, p) in m.params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: ", p.name)?;
                        TypeWriter { ty: &p.ty }.fmt(f)?;
                    }
                    write!(f, ")")?;
                    if let Some(ret) = &m.return_type {
                        write!(f, " -> ")?;
                        TypeWriter { ty: ret }.fmt(f)?;
                    }
                    writeln!(f, ";")?;
                }
                IrTraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                } => {
                    write!(f, "    type {}", name)?;
                    if !bounds.is_empty() {
                        write!(f, ": ")?;
                        for (j, b) in bounds.iter().enumerate() {
                            if j > 0 {
                                write!(f, " + ")?;
                            }
                            TraitBoundWriter { bound: b }.fmt(f)?;
                        }
                    }
                    if let Some(def) = default {
                        write!(f, " = ")?;
                        TypeWriter { ty: def }.fmt(f)?;
                    }
                    writeln!(f, ";")?;
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> RustBackend for ImplWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "impl ")?;
        GenericsWriter {
            generics: &self.i.generics,
        }
        .fmt(f)?;
        if let Some(t) = &self.i.trait_ {
            write!(f, " {}", t.kind)?;
            if !t.type_args.is_empty() {
                write!(f, "<")?;
                for (idx, arg) in t.type_args.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    TypeWriter { ty: arg }.fmt(f)?;
                }
                write!(f, ">")?;
            }
            write!(f, " for ")?;
        } else {
            write!(f, " ")?;
        }
        TypeWriter { ty: &self.i.self_ty }.fmt(f)?;
        WhereClauseWriter {
            where_clause: &self.i.where_clause,
        }
        .fmt(f)?;
        writeln!(f, " {{")?;
        for item in &self.i.items {
            match item {
                IrImplItem::Method(func) => FunctionWriter {
                    f: func,
                    level: 1,
                    is_trait_item: self.i.trait_.is_some(),
                }
                .fmt(f)?,
                IrImplItem::AssociatedType { name, ty } => {
                    write!(f, "    type {} = ", name)?;
                    TypeWriter { ty }.fmt(f)?;
                    writeln!(f, ";")?;
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> RustBackend for FunctionWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = "    ".repeat(self.level);
        if !self.is_trait_item {
            write!(f, "{}pub fn {}", indent, self.f.name)?;
        } else {
            write!(f, "{}fn {}", indent, self.f.name)?;
        }
        GenericsWriter {
            generics: &self.f.generics,
        }
        .fmt(f)?;
        write!(f, "(")?;
        if let Some(r) = self.f.receiver {
            ReceiverWriter { r }.fmt(f)?;
            if !self.f.params.is_empty() {
                write!(f, ", ")?;
            }
        }
        for (i, p) in self.f.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "mut {}: ", p.name)?;
            TypeWriter { ty: &p.ty }.fmt(f)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &self.f.return_type {
            write!(f, " -> ")?;
            TypeWriter { ty: ret }.fmt(f)?;
        }
        WhereClauseWriter {
            where_clause: &self.f.where_clause,
        }
        .fmt(f)?;
        writeln!(f)?;
        BlockWriter {
            block: &self.f.body,
            level: self.level,
        }
        .fmt(f)?;
        writeln!(f)?;
        Ok(())
    }
}

impl<'a> RustBackend for GenericsWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, p) in self.generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                if p.kind == IrGenericParamKind::Lifetime {
                    write!(f, "'{}", p.name)?;
                } else {
                    write!(f, "{}", p.name)?;
                }
                if !p.bounds.is_empty() {
                    write!(f, ": ")?;
                    for (j, b) in p.bounds.iter().enumerate() {
                        if j > 0 {
                            write!(f, " + ")?;
                        }
                        TraitBoundWriter { bound: b }.fmt(f)?;
                    }
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<'a> RustBackend for WhereClauseWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.where_clause.is_empty() {
            write!(f, " where ")?;
            for (idx, wp) in self.where_clause.iter().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }
                match wp {
                    IrWherePredicate::TypeBound { ty, bounds } => {
                        TypeWriter { ty }.fmt(f)?;
                        write!(f, ": ")?;
                        for (j, b) in bounds.iter().enumerate() {
                            if j > 0 {
                                write!(f, " + ")?;
                            }
                            TraitBoundWriter { bound: b }.fmt(f)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for TraitBoundWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.bound.trait_kind {
            TraitKind::Into(ty) => {
                write!(f, "Into<")?;
                TypeWriter { ty }.fmt(f)?;
                write!(f, ">")?;
            }
            TraitKind::AsRef(ty) => {
                write!(f, "AsRef<")?;
                TypeWriter { ty }.fmt(f)?;
                write!(f, ">")?;
            }
            TraitKind::Fn(inp, ty) => {
                let inp_str = match inp {
                    FnInput::BytesSlice => "&[u8]",
                    FnInput::Size => "usize",
                    FnInput::Bool => "bool",
                };
                write!(f, "FnMut({}) -> ", inp_str)?;
                TypeWriter { ty }.fmt(f)?;
            }
            _ => write!(f, "{}", self.bound)?,
        }
        Ok(())
    }
}

impl<'a> RustBackend for TypeWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            IrType::Primitive(p) => write!(f, "{}", p)?,
            IrType::Vector { elem } => {
                write!(f, "Vec<")?;
                TypeWriter { ty: elem }.fmt(f)?;
                write!(f, ">")?;
            }
            IrType::Array { kind, elem, len: _ } => {
                if *kind == ArrayKind::Slice {
                    write!(f, "[")?;
                    TypeWriter { ty: elem }.fmt(f)?;
                    write!(f, "]")?;
                } else {
                    write!(f, "Vec<")?;
                    TypeWriter { ty: elem }.fmt(f)?;
                    write!(f, ">")?;
                }
            }
            IrType::Struct { kind, type_args } => {
                write!(f, "{}", kind)?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        TypeWriter { ty: arg }.fmt(f)?;
                    }
                    write!(f, ">")?;
                }
            }
            IrType::TypeParam(p) => write!(f, "{}", p)?,
            IrType::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    TypeWriter { ty: elem }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrType::Unit => write!(f, "()")?,
            IrType::Reference { mutable, elem } => {
                write!(f, "&{}", if *mutable { "mut " } else { "" })?;
                TypeWriter { ty: elem }.fmt(f)?;
            }
            IrType::Projection {
                base,
                trait_path,
                trait_args,
                assoc,
            } => {
                if let IrType::TypeParam(p) = base.as_ref() {
                    if p == "Self" && trait_args.is_empty() {
                        write!(f, "Self::{}", assoc)?;
                        return Ok(());
                    }
                }
                write!(f, "<")?;
                TypeWriter { ty: base }.fmt(f)?;
                let trait_name = trait_path.as_deref().unwrap_or("_");
                write!(f, " as {}", trait_name)?;
                if !trait_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in trait_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        TypeWriter { ty: arg }.fmt(f)?;
                    }
                    write!(f, ">")?;
                }
                write!(f, ">::{}", assoc)?;
            }
            IrType::Existential { bounds } => {
                write!(f, "impl ")?;
                for (i, b) in bounds.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    TraitBoundWriter { bound: b }.fmt(f)?;
                }
            }
            _ => write!(f, "_")?,
        }
        Ok(())
    }
}

impl<'a> RustBackend for BlockWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = "    ".repeat(self.level);
        writeln!(f, "{}{{", indent)?;
        for stmt in &self.block.stmts {
            StmtWriter {
                stmt,
                level: self.level + 1,
            }
            .fmt(f)?;
        }
        if let Some(e) = &self.block.expr {
            write!(f, "{}    ", indent)?;
            ExprWriter { expr: e }.fmt(f)?;
            writeln!(f)?;
        }
        write!(f, "{}}}", indent)?;
        Ok(())
    }
}

impl<'a> RustBackend for StmtWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = "    ".repeat(self.level);
        write!(f, "{}", indent)?;
        match self.stmt {
            IrStmt::Let { pattern, ty, init } => {
                write!(f, "let ")?;
                PatternWriter { pat: pattern }.fmt(f)?;
                if let Some(t) = ty {
                    write!(f, ": ")?;
                    TypeWriter { ty: t }.fmt(f)?;
                }
                if let Some(i) = init {
                    write!(f, " = ")?;
                    ExprWriter { expr: i }.fmt(f)?;
                }
                writeln!(f, ";")?;
            }
            IrStmt::Semi(e) => {
                ExprWriter { expr: e }.fmt(f)?;
                writeln!(f, ";")?;
            }
            IrStmt::Expr(e) => {
                ExprWriter { expr: e }.fmt(f)?;
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for ExprChainWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.expr {
            IrExpr::ArrayMap {
                array,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: array }.fmt(f)?;
                write!(f, ".map(|{}| ", elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterSource { collection, method } => {
                ExprChainWriter { expr: collection }.fmt(f)?;
                let mname = match method {
                    IterMethod::Iter => "iter",
                    IterMethod::IntoIter => "into_iter",
                    IterMethod::Chars => "chars",
                    IterMethod::Bytes => "bytes",
                    _ => "iter",
                };
                write!(f, ".{}()", mname)?;
            }
            IrExpr::IterEnumerate { iter } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".enumerate()")?;
            }
            IrExpr::IterFilter {
                iter,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".filter(|{}| ", elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterTake { iter, count } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".take(")?;
                ExprWriter { expr: count }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterSkip { iter, count } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".skip(")?;
                ExprWriter { expr: count }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterChain { left, right } => {
                ExprChainWriter { expr: left }.fmt(f)?;
                write!(f, ".chain(")?;
                ExprWriter { expr: right }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterFlatMap {
                iter,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".flat_map(|{}| ", elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterFilterMap {
                iter,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".filter_map(|{}| ", elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::ArrayZip {
                left,
                right,
                left_var,
                right_var,
                body,
            } => {
                ExprChainWriter { expr: left }.fmt(f)?;
                write!(f, ".zip(")?;
                ExprWriter { expr: right }.fmt(f)?;
                write!(f, ").map(|({}, {})| ", left_var, right_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                let name = method_name(method);
                ExprChainWriter { expr: receiver }.fmt(f)?;
                write!(f, ".{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ExprWriter { expr: arg }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            _ => {
                ExprWriter { expr: self.expr }.fmt(f)?;
                write!(f, ".into_iter()")?;
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for ExprWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.expr {
            IrExpr::Lit(l) => write!(f, "{}", l)?,
            IrExpr::Var(v) => write!(f, "{}", v)?,
            IrExpr::Binary { op, left, right } => {
                write!(f, "(")?;
                ExprWriter { expr: left }.fmt(f)?;
                write!(f, " {} ", bin_op_str(*op))?;
                ExprWriter { expr: right }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                let name = method_name(method);
                ExprWriter { expr: receiver }.fmt(f)?;
                write!(f, ".{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ExprWriter { expr: arg }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrExpr::Call { func, args } => {
                ExprWriter { expr: func }.fmt(f)?;
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ExprWriter { expr: arg }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrExpr::Field { base, field } => {
                ExprWriter { expr: base }.fmt(f)?;
                write!(f, ".{}", field)?;
            }
            IrExpr::Index { base, index } => {
                ExprWriter { expr: base }.fmt(f)?;
                write!(f, "[")?;
                ExprWriter { expr: index }.fmt(f)?;
                write!(f, "]")?;
            }
            IrExpr::Block(b) => BlockWriter {
                block: b,
                level: 0,
            }
            .fmt(f)?,
            IrExpr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "if ")?;
                ExprWriter { expr: cond }.fmt(f)?;
                BlockWriter {
                    block: then_branch,
                    level: 0,
                }
                .fmt(f)?;
                if let Some(eb) = else_branch {
                    write!(f, " else ")?;
                    ExprWriter { expr: eb }.fmt(f)?;
                }
            }
            IrExpr::BoundedLoop {
                var,
                start,
                end,
                inclusive,
                body,
            } => {
                write!(f, "for {} in ", var)?;
                ExprWriter { expr: start }.fmt(f)?;
                write!(f, "{} ", if *inclusive { "..=" } else { ".." })?;
                ExprWriter { expr: end }.fmt(f)?;
                BlockWriter {
                    block: body,
                    level: 0,
                }
                .fmt(f)?;
            }
            IrExpr::IterLoop {
                pattern,
                collection,
                body,
            } => {
                write!(f, "for ")?;
                PatternWriter { pat: pattern }.fmt(f)?;
                write!(f, " in ")?;
                ExprWriter { expr: collection }.fmt(f)?;
                BlockWriter {
                    block: body,
                    level: 0,
                }
                .fmt(f)?;
            }
            IrExpr::ArrayMap {
                array,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: array }.fmt(f)?;
                write!(f, ".map(|{}| ", elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ").collect::<Vec<_>>()")?;
            }
            IrExpr::ArrayZip {
                left,
                right,
                left_var,
                right_var,
                body,
            } => {
                ExprChainWriter { expr: left }.fmt(f)?;
                write!(f, ".zip(")?;
                ExprWriter { expr: right }.fmt(f)?;
                write!(f, ").map(|({}, {})| ", left_var, right_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ").collect::<Vec<_>>()")?;
            }
            IrExpr::ArrayFold {
                array,
                init,
                acc_var,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: array }.fmt(f)?;
                write!(f, ".fold(")?;
                ExprWriter { expr: init }.fmt(f)?;
                write!(f, ", |{}, {}| ", acc_var, elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::IterFold {
                iter,
                init,
                acc_var,
                elem_var,
                body,
            } => {
                ExprChainWriter { expr: iter }.fmt(f)?;
                write!(f, ".fold(")?;
                ExprWriter { expr: init }.fmt(f)?;
                write!(f, ", |{}, {}| ", acc_var, elem_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Path {
                segments,
                type_args,
            } => {
                if let Some("new" | "from_mut_slice") = segments.last().map(|a| a.as_str()) {
                    let l = segments.last().unwrap();
                    let prefix = &segments[..segments.len() - 1];
                    write!(f, "{}", prefix.join("::"))?;
                    if !type_args.is_empty() {
                        write!(f, "::<")?;
                        for (i, arg) in type_args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            TypeWriter { ty: arg }.fmt(f)?;
                        }
                        write!(f, ">")?;
                    }
                    write!(f, "::{}", l)?;
                } else {
                    write!(f, "{}", segments.join("::"))?;
                    if !type_args.is_empty() {
                        write!(f, "::<")?;
                        for (i, arg) in type_args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            TypeWriter { ty: arg }.fmt(f)?;
                        }
                        write!(f, ">")?;
                    }
                }
            }
            IrExpr::Closure { params, body, .. } => {
                write!(f, "|")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    PatternWriter { pat: &p.pattern }.fmt(f)?;
                }
                write!(f, "| ")?;
                ExprWriter { expr: body }.fmt(f)?;
            }
            IrExpr::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(s) = start {
                    ExprWriter { expr: s }.fmt(f)?;
                }
                write!(f, "{}", if *inclusive { "..=" } else { ".." })?;
                if let Some(e) = end {
                    ExprWriter { expr: e }.fmt(f)?;
                }
            }
            IrExpr::Assign { left, right } => {
                ExprWriter { expr: left }.fmt(f)?;
                write!(f, " = ")?;
                ExprWriter { expr: right }.fmt(f)?;
            }
            IrExpr::StructExpr { kind, fields, .. } => {
                write!(f, "{} {{ ", kind)?;
                for (i, (name, val)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", name)?;
                    ExprWriter { expr: val }.fmt(f)?;
                }
                write!(f, " }}")?;
            }
            IrExpr::Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ExprWriter { expr: e }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrExpr::Array(elems) => {
                write!(f, "vec![")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ExprWriter { expr: e }.fmt(f)?;
                }
                write!(f, "]")?;
            }
            IrExpr::Repeat { elem, len } => {
                write!(f, "[")?;
                ExprWriter { expr: elem }.fmt(f)?;
                write!(f, "; ")?;
                ExprWriter { expr: len }.fmt(f)?;
                write!(f, "]")?;
            }
            IrExpr::Cast { expr, ty } => {
                write!(f, "(")?;
                ExprWriter { expr }.fmt(f)?;
                write!(f, " as ")?;
                TypeWriter { ty }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Return(e) => {
                write!(f, "return")?;
                if let Some(e) = e {
                    write!(f, " ")?;
                    ExprWriter { expr: e }.fmt(f)?;
                }
            }
            IrExpr::TypenumUsize { ty } => {
                write!(f, "<")?;
                TypeWriter { ty }.fmt(f)?;
                write!(f, " as typenum::Unsigned>::USIZE")?;
            }
            IrExpr::Unreachable => write!(f, "unreachable!()")?,
            _ => {
                let msg = format!("{:?}", self.expr).replace('"', "'");
                write!(
                    f,
                    "compile_error!(\"Unsupported expression in printer: {}\")",
                    msg
                )?;
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for PatternWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pat {
            IrPattern::Ident { mutable, name, .. } => {
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", name)?;
            }
            IrPattern::Wild => write!(f, "_")?,
            IrPattern::Tuple(elems) => {
                write!(f, "(")?;
                for (i, p) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    PatternWriter { pat: p }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrPattern::TupleStruct { kind, elems } => {
                write!(f, "{}(", kind)?;
                for (i, p) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    PatternWriter { pat: p }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrPattern::Struct {
                kind,
                fields,
                rest,
            } => {
                write!(f, "{} {{ ", kind)?;
                for (i, (name, p)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", name)?;
                    PatternWriter { pat: p }.fmt(f)?;
                }
                if *rest {
                    if !fields.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "..")?;
                }
                write!(f, " }}")?;
            }
            _ => write!(f, "..")?,
        }
        Ok(())
    }
}

impl RustBackend for ReceiverWriter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.r {
            IrReceiver::Value => write!(f, "self"),
            IrReceiver::Ref => write!(f, "&self"),
            IrReceiver::RefMut => write!(f, "&mut self"),
        }
    }
}

// ============================================================================
// HELPERS
// ============================================================================

fn method_name(method: &MethodKind) -> String {
    match method {
        MethodKind::Std(s) => s.clone(),
        MethodKind::Crypto(c) => {
            let debug_name = format!("{:?}", c);
            let mut snake = String::new();
            for (i, ch) in debug_name.chars().enumerate() {
                if ch.is_uppercase() && i > 0 {
                    snake.push('_');
                }
                snake.push(ch.to_ascii_lowercase());
            }
            snake
        }
        MethodKind::Vole(v) => format!("{:?}", v).to_lowercase(),
        MethodKind::Unknown(s) => s.clone(),
    }
}

fn bin_op_str(op: SpecBinOp) -> &'static str {
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
        SpecBinOp::Eq => "==",
        SpecBinOp::Ne => "!=",
        SpecBinOp::Lt => "<",
        SpecBinOp::Le => "<=",
        SpecBinOp::Gt => ">",
        SpecBinOp::Ge => ">=",
        _ => "+",
    }
}

// ============================================================================
// CONVENIENCE API
// ============================================================================

/// Render a full module with the standard preamble as a String.
///
/// This preserves backward compatibility with the old `print_module` API.
pub fn print_module(module: &IrModule) -> String {
    let mut out = String::new();
    write_preamble(&mut out);
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));
    out
}

/// Write the standard Rust preamble (imports, helpers) to a string.
fn write_preamble(out: &mut String) {
    // File header for Rust
    let _ = writeln!(out, "//! Auto-generated dynamic types from volar-spec");
    let _ = writeln!(
        out,
        "//! Type-level lengths have been converted to runtime usize witnesses"
    );
    let _ = writeln!(out);
    let _ = writeln!(out, "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]");
    let _ = writeln!(out, "extern crate alloc;");
    let _ = writeln!(out, "use alloc::vec::Vec;");
    let _ = writeln!(out, "use alloc::vec;");
    let _ = writeln!(
        out,
        "use core::ops::{{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr}};"
    );
    let _ = writeln!(out, "use core::marker::PhantomData;");
    let _ = writeln!(out, "use typenum::Unsigned;");
    let _ = writeln!(out, "use cipher::{{BlockEncrypt, Block}};");
    let _ = writeln!(out, "use digest::Digest;");
    let _ = writeln!(out, "use volar_common::hash_commitment::commit;");
    let _ = writeln!(out);
    let _ = writeln!(out, "/// Compute integer log2");
    let _ = writeln!(out, "#[inline]");
    let _ = writeln!(out, "pub fn ilog2(x: usize) -> u32 {{");
    let _ = writeln!(out, "    usize::BITS - x.leading_zeros() - 1");
    let _ = writeln!(out, "}}");
    let _ = writeln!(out);
}
