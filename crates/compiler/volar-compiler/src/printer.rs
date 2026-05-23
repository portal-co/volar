// @reliability: normal
// @ai: assisted
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
    pub module: &'a IrModule<IrFunction>,
}

pub struct StructWriter<'a> {
    pub s: &'a IrStruct,
}

pub struct EnumWriter<'a> {
    pub e: &'a IrEnum,
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

/// Writes a flat `IrIterChain` as Rust method-chain syntax.
pub struct IterChainWriter<'a> {
    pub chain: &'a IrIterChain,
}

/// Writes the source part of an iterator chain.
struct IterChainSourceWriter<'a> {
    pub source: &'a IterChainSource,
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
        for e in &self.module.enums {
            EnumWriter { e }.fmt(f)?;
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
        if self.s.derives.is_empty() {
            writeln!(f, "#[derive(Debug, Default)]")?;
        } else {
            writeln!(f, "#[derive({})]", self.s.derives.join(", "))?;
        }
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

impl<'a> RustBackend for EnumWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.e.derives.is_empty() {
            writeln!(f, "#[derive(Debug)]")?;
        } else {
            writeln!(f, "#[derive({})]", self.e.derives.join(", "))?;
        }
        write!(f, "pub enum {}", self.e.kind)?;
        GenericsWriter {
            generics: &self.e.generics,
        }
        .fmt(f)?;
        writeln!(f, " {{")?;
        for variant in &self.e.variants {
            match &variant.fields {
                IrEnumVariantData::Unit => {
                    writeln!(f, "    {},", variant.name)?;
                }
                IrEnumVariantData::Tuple(types) => {
                    write!(f, "    {}(", variant.name)?;
                    for (i, ty) in types.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        TypeWriter { ty }.fmt(f)?;
                    }
                    writeln!(f, "),")?;
                }
                IrEnumVariantData::Struct(fields) => {
                    writeln!(f, "    {} {{", variant.name)?;
                    for field in fields {
                        // Enum variant fields inherit the enum's visibility —
                        // Rust forbids explicit `pub` on them.
                        write!(f, "        {}: ", field.name)?;
                        TypeWriter { ty: &field.ty }.fmt(f)?;
                        writeln!(f, ",")?;
                    }
                    writeln!(f, "    }},")?;
                }
            }
        }
        writeln!(f, "}}")?;
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
        TypeWriter {
            ty: &self.i.self_ty,
        }
        .fmt(f)?;
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
        // Emit #[volar_action] attribute for action functions.
        if self.f.external_kind == ExternalKind::Action {
            writeln!(f, "{}#[volar_action]", indent)?;
        }
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
                } else if p.kind == IrGenericParamKind::Const {
                    let ty_str = match &p.const_ty {
                        Some(ty) => format!("{}", DisplayRust(TypeWriter { ty })),
                        None => "usize".into(),
                    };
                    write!(f, "const {}: {}", p.name, ty_str)?;
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
            _ => {
                // Guardrail: Custom must not wrap a well-known typed variant name.
                self.bound.trait_kind.debug_assert_not_misrouted();
                write!(f, "{}", self.bound)?;
            }
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
            IrType::Array { kind, elem, len } => {
                match kind {
                    ArrayKind::Slice => {
                        write!(f, "[")?;
                        TypeWriter { ty: elem }.fmt(f)?;
                        write!(f, "]")?;
                    }
                    ArrayKind::FixedArray => {
                        // `[T; n]` — length is a const *expression* (integer), not a typenum type.
                        write!(f, "[")?;
                        TypeWriter { ty: elem }.fmt(f)?;
                        write!(f, "; ")?;
                        fmt_fixed_array_len(f, len)?;
                        write!(f, "]")?;
                    }
                    ArrayKind::GenericArray => {
                        // `Array<T, N>` — length is a typenum type-level constant.
                        write!(f, "Array<")?;
                        TypeWriter { ty: elem }.fmt(f)?;
                        write!(f, ", ")?;
                        let len_ty = array_length_as_static_type(len);
                        TypeWriter { ty: &len_ty }.fmt(f)?;
                        write!(f, ">")?;
                    }
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
                // Single-element tuples need a trailing comma: (T,)
                if elems.len() == 1 {
                    write!(f, ",")?;
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

// ============================================================================
// Iterator chain printing
// ============================================================================

impl<'a> RustBackend for IterChainSourceWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.source {
            IterChainSource::Method { collection, method } => {
                ExprWriter { expr: collection }.fmt(f)?;
                let mname = match method {
                    IterMethod::Iter => "iter",
                    IterMethod::IntoIter => "into_iter",
                    IterMethod::Chars => "chars",
                    IterMethod::Bytes => "bytes",
                    IterMethod::Flatten => "flatten",
                };
                write!(f, ".{}()", mname)?;
            }
            IterChainSource::Range {
                start,
                end,
                inclusive,
            } => {
                write!(f, "(")?;
                ExprWriter { expr: start }.fmt(f)?;
                write!(f, "{}", if *inclusive { "..=" } else { ".." })?;
                ExprWriter { expr: end }.fmt(f)?;
                write!(f, ")")?;
            }
            IterChainSource::Zip { left, right } => {
                IterChainWriter { chain: left }.fmt_no_terminal(f)?;
                write!(f, ".zip(")?;
                IterChainWriter { chain: right }.fmt_no_terminal(f)?;
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

impl<'a> IterChainWriter<'a> {
    /// Write source + steps, but NOT the terminal.
    fn fmt_no_terminal(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        IterChainSourceWriter {
            source: &self.chain.source,
        }
        .fmt(f)?;
        for step in &self.chain.steps {
            match step {
                IterStep::Map { var, body } => {
                    write!(f, ".map(|")?;
                    PatternWriter { pat: var }.fmt(f)?;
                    write!(f, "| ")?;
                    ExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Filter { var, body } => {
                    write!(f, ".filter(|")?;
                    PatternWriter { pat: var }.fmt(f)?;
                    write!(f, "| ")?;
                    ExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::FilterMap { var, body } => {
                    write!(f, ".filter_map(|")?;
                    PatternWriter { pat: var }.fmt(f)?;
                    write!(f, "| ")?;
                    ExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::FlatMap { var, body } => {
                    write!(f, ".flat_map(|")?;
                    PatternWriter { pat: var }.fmt(f)?;
                    write!(f, "| ")?;
                    ExprWriter { expr: body }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Enumerate => {
                    write!(f, ".enumerate()")?;
                }
                IterStep::Take { count } => {
                    write!(f, ".take(")?;
                    ExprWriter { expr: count }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Skip { count } => {
                    write!(f, ".skip(")?;
                    ExprWriter { expr: count }.fmt(f)?;
                    write!(f, ")")?;
                }
                IterStep::Chain { other } => {
                    write!(f, ".chain(")?;
                    IterChainWriter { chain: other }.fmt_no_terminal(f)?;
                    write!(f, ")")?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for IterChainWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_no_terminal(f)?;
        match &self.chain.terminal {
            IterTerminal::Collect => {
                write!(f, ".collect::<Vec<_>>()")?;
            }
            IterTerminal::CollectTyped(ty) => {
                write!(f, ".collect::<Vec<")?;
                TypeWriter { ty }.fmt(f)?;
                write!(f, ">>()")?;
            }
            IterTerminal::Fold {
                init,
                acc_var,
                elem_var,
                body,
            } => {
                write!(f, ".fold(")?;
                ExprWriter { expr: init }.fmt(f)?;
                write!(f, ", |")?;
                PatternWriter { pat: acc_var }.fmt(f)?;
                write!(f, ", ")?;
                PatternWriter { pat: elem_var }.fmt(f)?;
                write!(f, "| ")?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IterTerminal::Lazy => {
                // No terminal — the chain is consumed by something else (e.g. for-in loop)
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for ExprChainWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.expr {
            IrExpr::MethodCall {
                receiver,
                method,
                args,
                type_args,
            } => {
                let name = method_name(method);
                ExprChainWriter { expr: receiver }.fmt(f)?;
                write!(f, ".{}", name)?;
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
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ExprWriter { expr: arg }.fmt(f)?;
                }
                write!(f, ")")?;
            }
            IrExpr::IterPipeline(chain) => {
                IterChainWriter { chain }.fmt_no_terminal(f)?;
            }
            IrExpr::RawMap { .. } | IrExpr::RawZip { .. } | IrExpr::RawFold { .. } => {
                // In chain context, emit the raw op then .into_iter()
                ExprWriter { expr: self.expr }.fmt(f)?;
                write!(f, ".into_iter()")?;
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
            IrExpr::Var(v) => {
                debug_assert!(
                    v == "self" || v.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                    "IrExpr::Var contains non-ident string {:?}. Use Path, Call, or other IR nodes.",
                    v
                );
                write!(f, "{}", v)?;
            }
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
                type_args,
            } => {
                let name = method_name(method);
                ExprWriter { expr: receiver }.fmt(f)?;
                write!(f, ".{}", name)?;
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
                write!(f, "(")?;
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
                debug_assert!(
                    field.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
                        || field.parse::<usize>().is_ok(),
                    "Field name is not a valid ident or index: {:?}",
                    field
                );
                ExprWriter { expr: base }.fmt(f)?;
                write!(f, ".{}", field)?;
            }
            IrExpr::Index { base, index } => {
                ExprWriter { expr: base }.fmt(f)?;
                write!(f, "[")?;
                ExprWriter { expr: index }.fmt(f)?;
                write!(f, "]")?;
            }
            IrExpr::Block(b) => BlockWriter { block: b, level: 0 }.fmt(f)?,
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
                debug_assert!(
                    var.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                    "BoundedLoop var is not a valid ident: {:?}",
                    var
                );
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
            IrExpr::WhileLoop { cond, body } => {
                write!(f, "while ")?;
                ExprWriter { expr: cond }.fmt(f)?;
                BlockWriter { block: body, level: 0 }.fmt(f)?;
            }
            IrExpr::IterPipeline(chain) => {
                IterChainWriter { chain }.fmt(f)?;
            }
            IrExpr::RawMap {
                receiver,
                elem_var,
                body,
            } => {
                ExprWriter { expr: receiver }.fmt(f)?;
                write!(f, ".map(|")?;
                PatternWriter { pat: elem_var }.fmt(f)?;
                write!(f, "| ")?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::RawZip {
                left,
                right,
                left_var,
                right_var,
                body,
            } => {
                ExprWriter { expr: left }.fmt(f)?;
                write!(f, ".zip(")?;
                ExprWriter { expr: right }.fmt(f)?;
                write!(f, ", |")?;
                PatternWriter { pat: left_var }.fmt(f)?;
                write!(f, ", ")?;
                PatternWriter { pat: right_var }.fmt(f)?;
                write!(f, "| ")?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::RawFold {
                receiver,
                init,
                acc_var,
                elem_var,
                body,
            } => {
                ExprWriter { expr: receiver }.fmt(f)?;
                write!(f, ".fold(")?;
                ExprWriter { expr: init }.fmt(f)?;
                write!(f, ", |")?;
                PatternWriter { pat: acc_var }.fmt(f)?;
                write!(f, ", ")?;
                PatternWriter { pat: elem_var }.fmt(f)?;
                write!(f, "| ")?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Path {
                segments,
                type_args,
            } => {
                debug_assert!(
                    segments.iter().all(|s| s
                        .chars()
                        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '!')),
                    "Path segments contain non-ident strings: {:?}",
                    segments
                );
                // When path has 2+ segments and type_args, put turbofish on
                // the type prefix (e.g. AsRef::<[u8]>::as_ref, Vec::<u8>::new).
                if segments.len() >= 2 && !type_args.is_empty() {
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
                // Trailing comma for 1-element tuples: (x,) not (x)
                if elems.len() == 1 {
                    write!(f, ",")?;
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
            IrExpr::FixedArray(elems) => {
                write!(f, "[")?;
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
            IrExpr::Break(e) => {
                write!(f, "break")?;
                if let Some(e) = e {
                    write!(f, " ")?;
                    ExprWriter { expr: e }.fmt(f)?;
                }
            }
            IrExpr::Continue => write!(f, "continue")?,
            IrExpr::AssignOp { op, left, right } => {
                ExprWriter { expr: left }.fmt(f)?;
                write!(f, " {}= ", bin_op_str(*op))?;
                ExprWriter { expr: right }.fmt(f)?;
            }
            IrExpr::Try(e) => {
                ExprWriter { expr: e }.fmt(f)?;
                write!(f, "?")?;
            }
            IrExpr::TypenumUsize { ty } => {
                write!(f, "<")?;
                TypeWriter { ty }.fmt(f)?;
                write!(f, " as typenum::Unsigned>::USIZE")?;
            }
            IrExpr::Unreachable => write!(f, "unreachable!()")?,
            IrExpr::DefaultValue { ty } => {
                if let Some(t) = ty {
                    // Array/Vector types: vec![<elem>::default(); len]
                    if let IrType::Array { elem, len, .. } = t.as_ref() {
                        write!(f, "vec![")?;
                        if matches!(elem.as_ref(), &IrType::Infer) {
                            write!(f, "Default::default()")?;
                        } else {
                            write!(f, "<")?;
                            TypeWriter { ty: elem }.fmt(f)?;
                            write!(f, ">::default()")?;
                        }
                        write!(f, "; ")?;
                        match len {
                            ArrayLength::Const(n) => write!(f, "{}", n)?,
                            ArrayLength::TypeParam(p) => write!(f, "{}", p.to_lowercase())?,
                            ArrayLength::Projection { r#type, field, .. } => {
                                write!(f, "<<")?;
                                TypeWriter { ty: r#type }.fmt(f)?;
                                write!(f, ">::{} as Unsigned>::to_usize()", field)?;
                            }
                            _ => write!(f, "todo!(\"length\")")?,
                        }
                        write!(f, "]")?;
                    } else {
                        // Use disambiguation syntax <T>::default() to handle
                        // types with angle brackets like Vec<T>
                        write!(f, "<")?;
                        TypeWriter { ty: t }.fmt(f)?;
                        write!(f, ">::default()")?;
                    }
                } else {
                    write!(f, "Default::default()")?;
                }
            }
            IrExpr::LengthOf(len) => {
                match len {
                    ArrayLength::Const(n) => write!(f, "{}", n)?,
                    ArrayLength::TypeParam(p) => write!(f, "{}::USIZE", p)?,
                    ArrayLength::Projection { r#type, field, .. } => {
                        // Emit <<T>::Assoc as Unsigned>::to_usize() for compile-time resolution
                        write!(f, "<<")?;
                        TypeWriter { ty: r#type }.fmt(f)?;
                        write!(f, ">::{} as Unsigned>::to_usize()", field)?;
                    }
                    _ => write!(f, "0")?,
                }
            }
            IrExpr::ArrayGenerate {
                elem_ty,
                len,
                index_var,
                body,
            } => {
                // Static form: Array::<ElemTy, Len>::from_fn(|index_var| body)
                debug_assert!(
                    index_var
                        .chars()
                        .all(|c| c.is_ascii_alphanumeric() || c == '_'),
                    "ArrayGenerate index_var is not a valid ident: {:?}",
                    index_var
                );
                write!(f, "Array::<")?;
                match elem_ty {
                    Some(ty) => TypeWriter { ty }.fmt(f)?,
                    None => write!(f, "_")?,
                }
                write!(f, ", ")?;
                let len_ty = array_length_as_static_type(len);
                TypeWriter { ty: &len_ty }.fmt(f)?;
                write!(f, ">::from_fn(|{}| ", index_var)?;
                ExprWriter { expr: body }.fmt(f)?;
                write!(f, ")")?;
            }
            IrExpr::Unary { op, expr } => match op {
                SpecUnaryOp::Neg => {
                    write!(f, "-")?;
                    ExprWriter { expr }.fmt(f)?;
                }
                SpecUnaryOp::Not => {
                    write!(f, "!")?;
                    ExprWriter { expr }.fmt(f)?;
                }
                SpecUnaryOp::Deref => {
                    write!(f, "*")?;
                    ExprWriter { expr }.fmt(f)?;
                }
                SpecUnaryOp::Ref => {
                    write!(f, "&")?;
                    ExprWriter { expr }.fmt(f)?;
                }
                SpecUnaryOp::RefMut => {
                    write!(f, "&mut ")?;
                    ExprWriter { expr }.fmt(f)?;
                }
            },
            IrExpr::Match { expr, arms } => {
                write!(f, "match ")?;
                ExprWriter { expr }.fmt(f)?;
                writeln!(f, " {{")?;
                for arm in arms {
                    write!(f, "    ")?;
                    PatternWriter { pat: &arm.pattern }.fmt(f)?;
                    write!(f, " => ")?;
                    ExprWriter { expr: &arm.body }.fmt(f)?;
                    writeln!(f, ",")?;
                }
                write!(f, "}}")?;
            }
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
            IrPattern::Struct { kind, fields, rest } => {
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
    // Guardrail: Other must not wrap a well-known StdMethod name.
    method.debug_assert_not_misrouted();
    let name = match method {
        MethodKind::Known(m) => m.as_str().to_string(),
        MethodKind::Vole(v) => format!("{:?}", v).to_lowercase(),
        MethodKind::Other(s) => s.clone(),
    };
    // Existing ident-char guardrail (also checked inside debug_assert_not_misrouted
    // for Other, but retained here to cover Vole-derived names too).
    debug_assert!(
        name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
        "Method name is not a valid ident: {:?}",
        name
    );
    name
}

/// Format an `ArrayLength` as a Rust const *expression* — suitable for
/// use as the size in `[T; n]` fixed-array types.
///
/// - `Const(n)` → integer literal `n`.
/// - `TypeParam(p)` → bare ident `p` (valid when `p` is a `const` generic).
/// - Projection → `<<T>::Assoc as Unsigned>::USIZE`.
fn fmt_fixed_array_len(f: &mut fmt::Formatter<'_>, len: &ArrayLength) -> fmt::Result {
    match len {
        ArrayLength::Const(n) => write!(f, "{}", n),
        ArrayLength::TypeParam(p) => write!(f, "{}", p),
        ArrayLength::TypeNum(tn) => write!(f, "{:?}::USIZE", tn),
        ArrayLength::Projection { r#type, field, .. } => {
            write!(f, "<<")?;
            TypeWriter { ty: r#type }.fmt(f)?;
            write!(f, ">::{} as Unsigned>::USIZE", field)
        }
        _ => write!(f, "0"),
    }
}

/// Convert an `ArrayLength` to an `IrType` suitable for use as a static type argument
/// (e.g. inside `Array<Elem, Len>` or `Array::<Elem, Len>::from_fn(...)`).
///
/// - `TypeParam("N")` → `TypeParam("N")` (preserves case — the static name).
/// - `Const(n)` → `TypeParam("typenum::U{n}")` (approximation for small literals).
/// - `TypeNum(tn)` → `TypeParam("{tn:?}")` (e.g. `U8`, `U16`).
/// - `Projection { type, field }` → `Projection { ... }` (e.g. `<T as Trait>::Assoc`).
fn array_length_as_static_type(len: &ArrayLength) -> IrType {
    match len {
        ArrayLength::TypeParam(name) => IrType::TypeParam(name.clone()),
        ArrayLength::Const(n) => IrType::TypeParam(format!("typenum::U{}", n)),
        ArrayLength::TypeNum(tn) => IrType::TypeParam(format!("{:?}", tn)),
        ArrayLength::Projection {
            r#type,
            field,
            trait_path,
        } => IrType::Projection {
            base: r#type.clone(),
            trait_path: trait_path.clone(),
            trait_args: Vec::new(),
            assoc: AssociatedType::from_str(field),
        },
        _ => IrType::TypeParam("_".to_string()),
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
        SpecBinOp::And => "&&",
        SpecBinOp::Or => "||",
    }
}

// ============================================================================
// CONVENIENCE API
// ============================================================================

/// Render a full module with the standard preamble as a String.
///
/// This preserves backward compatibility with the old `print_module` API.
pub fn print_module(module: &IrModule<IrFunction>) -> String {
    print_module_with_deps(module, &[])
}

/// Render a module with the standard preamble plus data-driven dependency imports.
pub fn print_module_with_deps(module: &IrModule<IrFunction>, deps: &[crate::manifest::TypeManifest]) -> String {
    print_module_with_remotes(module, deps, &[])
}

/// Render a module with manifest deps **and** remote-linked spec imports.
///
/// `remotes` is typically obtained from [`LinkageSystem::remote_refs`].
pub fn print_module_with_remotes(
    module: &IrModule<IrFunction>,
    deps: &[crate::manifest::TypeManifest],
    remotes: &[crate::linkage::RemoteSpecRef<'_>],
) -> String {
    let mut out = String::new();
    let _ = write!(out, "{}", DisplayRust(DynPreambleWriter { deps, remotes }));
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));
    out
}

/// Data-driven preamble writer.
///
/// Generates standard imports plus `pub use` statements for manifest deps and
/// remote-linked specs.
pub struct DynPreambleWriter<'a> {
    pub deps: &'a [crate::manifest::TypeManifest],
    pub remotes: &'a [crate::linkage::RemoteSpecRef<'a>],
}

impl<'a> RustBackend for DynPreambleWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // File header
        writeln!(f, "//! Auto-generated dynamic types from volar-spec")?;
        writeln!(
            f,
            "//! Type-level lengths have been converted to runtime usize witnesses"
        )?;
        writeln!(f)?;
        writeln!(
            f,
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]"
        )?;
        writeln!(f, "extern crate alloc;")?;
        writeln!(f, "use alloc::vec::Vec;")?;
        writeln!(f, "use alloc::vec;")?;
        writeln!(
            f,
            "use core::ops::{{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr}};"
        )?;
        writeln!(f, "use core::marker::PhantomData;")?;
        writeln!(f, "use typenum::Unsigned;")?;
        writeln!(f, "use cipher::BlockCipherEncrypt;")?;
        writeln!(f, "use digest::Digest;")?;
        writeln!(f, "use volar_common::hash_commitment::commit;")?;
        writeln!(f, "use volar_common::length_doubling::LengthDoubler;")?;
        writeln!(
            f,
            "use volar_primitives::{{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64}};"
        )?;
        writeln!(f, "use hybrid_array::Array;")?;
        writeln!(f)?;

        // Dependency re-exports
        for dep in self.deps {
            let crate_ident = dep.crate_name.replace('-', "_");
            // Collect struct names and trait names from the manifest
            let mut type_names: Vec<String> = Vec::new();
            for s in &dep.module.structs {
                type_names.push(s.kind.to_string());
            }
            for t in &dep.module.traits {
                type_names.push(t.kind.to_string());
            }
            if !type_names.is_empty() {
                writeln!(f, "pub use {}::{{{}}};", crate_ident, type_names.join(", "))?;
            }
        }
        if !self.deps.is_empty() {
            writeln!(f)?;
        }

        // Remote spec re-exports (from LinkageSystem::remote_refs)
        for remote in self.remotes {
            let crate_ident = remote.rust_crate.replace('-', "_");
            if !remote.type_names.is_empty() {
                writeln!(f, "pub use {}::{{{}}};", crate_ident, remote.type_names.join(", "))?;
            }
        }
        if !self.remotes.is_empty() {
            writeln!(f)?;
        }

        // Helper functions
        writeln!(f, "/// Compute integer log2")?;
        writeln!(f, "#[inline]")?;
        writeln!(f, "pub fn ilog2(x: usize) -> u32 {{")?;
        writeln!(f, "    usize::BITS - x.leading_zeros() - 1")?;
        writeln!(f, "}}")?;
        writeln!(f)?;
        writeln!(
            f,
            "/// Bridge: call LengthDoubler::double on a Vec<u8>, converting to/from Array"
        )?;
        writeln!(f, "#[inline]")?;
        writeln!(
            f,
            "pub fn double_vec<B: LengthDoubler>(v: Vec<u8>) -> [Vec<u8>; 2] {{"
        )?;
        writeln!(
            f,
            "    let arr = hybrid_array::Array::try_from(v.as_slice()).expect(\"double_vec: length mismatch\");"
        )?;
        writeln!(f, "    let [a, b] = B::double(arr);")?;
        writeln!(f, "    [a.to_vec(), b.to_vec()]")?;
        writeln!(f, "}}")?;
        writeln!(f)?;
        Ok(())
    }
}

// ============================================================================
// CFG MODULE PRINTING
// ============================================================================

/// Renders an `IrCfgModule` body without preamble.
pub struct CfgModuleWriter<'a> {
    pub module: &'a IrCfgModule,
}

/// Renders a single `IrCfgFunction` as a Rust `pub fn` with a state-machine loop body.
pub struct CfgFunctionWriter<'a> {
    pub func: &'a IrCfgFunction,
    pub level: usize,
}

impl<'a> RustBackend for CfgModuleWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.module.structs {
            StructWriter { s }.fmt(f)?;
            writeln!(f)?;
        }
        for e in &self.module.enums {
            EnumWriter { e }.fmt(f)?;
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
            match func {
                IrAnyFunction::Flat(func) => {
                    // TypeStub functions carry only signature info for LIR codegen;
                    // the Rust output imports the real implementation via `use`.
                    if func.external_kind == ExternalKind::TypeStub {
                        continue;
                    }
                    FunctionWriter { f: func, level: 0, is_trait_item: false }.fmt(f)?;
                    writeln!(f)?;
                }
                IrAnyFunction::Cfg(func) => {
                    CfgFunctionWriter { func, level: 0 }.fmt(f)?;
                    writeln!(f)?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> RustBackend for CfgFunctionWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = "    ".repeat(self.level);
        let func = self.func;
        let blocks = &func.body.blocks;

        // ── Function signature ────────────────────────────────────────────────
        write!(f, "{}pub fn {}", indent, func.name)?;
        GenericsWriter {
            generics: &func.generics,
        }
        .fmt(f)?;
        write!(f, "(")?;
        if let Some(r) = func.receiver {
            ReceiverWriter { r }.fmt(f)?;
            if !func.params.is_empty() {
                write!(f, ", ")?;
            }
        }
        for (i, p) in func.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "mut {}: ", p.name)?;
            TypeWriter { ty: &p.ty }.fmt(f)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &func.return_type {
            write!(f, " -> ")?;
            TypeWriter { ty: ret }.fmt(f)?;
        }
        WhereClauseWriter {
            where_clause: &func.where_clause,
        }
        .fmt(f)?;
        writeln!(f, " {{")?;

        // If the function has only one block with a Return terminator (no
        // block params besides the function params), emit a plain body.
        if blocks.len() == 1 {
            let blk = &blocks[0];
            for stmt in &blk.stmts {
                StmtWriter {
                    stmt,
                    level: self.level + 1,
                }
                .fmt(f)?;
            }
            let i1 = "    ".repeat(self.level + 1);
            let i2 = "    ".repeat(self.level + 2);
            match &blk.terminator {
                IrCfgTerminator::Return(None) => {}
                IrCfgTerminator::Return(Some(expr)) => {
                    write!(f, "{}    ", indent)?;
                    ExprWriter { expr }.fmt(f)?;
                    writeln!(f)?;
                }
                _ => {
                    write_cfg_terminator(f, &blk.terminator, &i1, &i2)?;
                }
            }
            writeln!(f, "{}}}", indent)?;
            return Ok(());
        }

        // ── State-machine loop ────────────────────────────────────────────────
        write_state_machine(f, func, self.level + 1)?;
        writeln!(f, "{}}}", indent)?;
        Ok(())
    }
}

/// Emit the state-machine loop body for a CFG function.
///
/// Generated structure:
/// ```ignore
/// let mut __state: usize = 0;
/// // per-block-param Option slots (blocks 1..):
/// let mut __b1_p0: Option<ParamType> = None;
/// // ...
/// loop {
///     match __state {
///         0 => {
///             // stmts...
///             // terminator
///         }
///         1 => { ... }
///         _ => unreachable!(),
///     }
/// }
/// ```
fn write_state_machine(
    f: &mut fmt::Formatter<'_>,
    func: &IrCfgFunction,
    base_level: usize,
) -> fmt::Result {
    let blocks = &func.body.blocks;
    // base_level: nesting level of the function body content (function params are at base_level-1)
    // body stmts: base_level
    // loop body: base_level + 1
    // match arm body: base_level + 2
    // stmt inside match arm: base_level + 3
    let l0 = "    ".repeat(base_level);
    let l1 = "    ".repeat(base_level + 1);
    let l2 = "    ".repeat(base_level + 2);
    let l3 = "    ".repeat(base_level + 3);

    writeln!(f, "{}let mut __state: usize = 0;", l0)?;

    // Declare Option slots for block params of blocks 1.. (block 0 uses function params).
    for (bidx, blk) in blocks.iter().enumerate().skip(1) {
        for (pidx, param) in blk.params.iter().enumerate() {
            write!(f, "{}let mut __b{}_p{}: Option<", l0, bidx, pidx)?;
            TypeWriter { ty: &param.ty }.fmt(f)?;
            writeln!(f, "> = None;")?;
        }
    }

    writeln!(f, "{}loop {{", l0)?;
    writeln!(f, "{}match __state {{", l1)?;

    for (bidx, blk) in blocks.iter().enumerate() {
        writeln!(f, "{}{} => {{", l1, bidx)?;

        // Bind block params from their Option slots (block 0 uses function params directly).
        if bidx > 0 {
            for (pidx, param) in blk.params.iter().enumerate() {
                writeln!(
                    f,
                    "{}let mut {} = __b{}_p{}.take().unwrap();",
                    l2, param.name, bidx, pidx
                )?;
            }
        }

        // Emit statements.
        for stmt in &blk.stmts {
            StmtWriter {
                stmt,
                level: base_level + 2,
            }
            .fmt(f)?;
        }

        // Terminator.
        write_cfg_terminator(f, &blk.terminator, &l2, &l3)?;

        writeln!(f, "{}}}", l1)?; // end match arm
    }

    writeln!(f, "{}_ => unreachable!(),", l1)?;
    writeln!(f, "{}}}", l1)?; // end match
    writeln!(f, "{}}}", l0)?; // end loop
    Ok(())
}

fn write_cfg_terminator(
    f: &mut fmt::Formatter<'_>,
    term: &IrCfgTerminator,
    indent: &str,
    inner_indent: &str,
) -> fmt::Result {
    match term {
        IrCfgTerminator::Return(None) => {
            writeln!(f, "{}return;", indent)?;
        }
        IrCfgTerminator::Return(Some(expr)) => {
            write!(f, "{}return ", indent)?;
            ExprWriter { expr }.fmt(f)?;
            writeln!(f, ";")?;
        }
        IrCfgTerminator::Goto(jump) => {
            write_jump_setup(f, jump, indent)?;
            writeln!(f, "{}continue;", indent)?;
        }
        IrCfgTerminator::CondGoto { cond, then_, else_ } => {
            write!(f, "{}if ", indent)?;
            ExprWriter { expr: cond }.fmt(f)?;
            writeln!(f, " {{")?;
            write_jump_setup(f, then_, inner_indent)?;
            writeln!(f, "{}}} else {{", indent)?;
            write_jump_setup(f, else_, inner_indent)?;
            writeln!(f, "{}}}", indent)?;
            writeln!(f, "{}continue;", indent)?;
        }
    }
    Ok(())
}

/// Emits the assignments that load a jump's args into the target block's Option slots,
/// then sets `__state`.
fn write_jump_setup(f: &mut fmt::Formatter<'_>, jump: &IrCfgJump, indent: &str) -> fmt::Result {
    for (pidx, arg) in jump.args.iter().enumerate() {
        write!(f, "{}__b{}_p{} = Some(", indent, jump.target, pidx)?;
        ExprWriter { expr: arg }.fmt(f)?;
        writeln!(f, ");")?;
    }
    writeln!(f, "{}__state = {};", indent, jump.target)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    extern crate std;
    #[allow(unused_imports)]
    use std::prelude::rust_2021::*;
    use std::format;
    use std::string::{String, ToString};
    use std::vec;
    use std::vec::Vec;
    use super::*;
    use crate::ir::{
        ExternalKind, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump, IrCfgTerminator, IrExpr,
        IrLit, IrParam, IrType, PrimitiveType,
    };

    fn minimal_cfg_fn(name: &str, blocks: Vec<IrCfgBlock>) -> IrCfgFunction {
        IrCfgFunction {
            name: name.to_string(),
            generics: vec![],
            receiver: None,
            params: vec![],
            return_type: None,
            where_clause: vec![],
            external_kind: ExternalKind::Normal,
            body: IrCfgBody { blocks },
        }
    }

    fn render(func: &IrCfgFunction) -> String {
        format!("{}", DisplayRust(CfgFunctionWriter { func, level: 0 }))
    }

    // ── Single-block fast path ────────────────────────────────────────────────

    #[test]
    fn single_block_return_none_no_state_machine() {
        let func = minimal_cfg_fn(
            "foo",
            vec![IrCfgBlock {
                params: vec![],
                stmts: vec![],
                stmt_provs: vec![],
                terminator: IrCfgTerminator::Return(None),
            }],
        );
        let out = render(&func);
        assert!(out.contains("pub fn foo()"), "missing signature: {}", out);
        assert!(
            !out.contains("__state"),
            "single-block should not emit state machine: {}",
            out
        );
        assert!(!out.contains("loop {"), "should not emit loop: {}", out);
    }

    #[test]
    fn single_block_return_expr_no_state_machine() {
        let func = minimal_cfg_fn(
            "bar",
            vec![IrCfgBlock {
                params: vec![],
                stmts: vec![],
                stmt_provs: vec![],
                terminator: IrCfgTerminator::Return(Some(IrExpr::Lit(IrLit::Bool(true)))),
            }],
        );
        let out = render(&func);
        assert!(out.contains("true"), "missing return value: {}", out);
        assert!(!out.contains("__state"), "should not emit state machine: {}", out);
    }

    // ── Multi-block: state-machine structure ──────────────────────────────────

    #[test]
    fn multi_block_emits_state_machine_skeleton() {
        let func = minimal_cfg_fn(
            "state_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: vec![] }),
                },
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
            ],
        );
        let out = render(&func);
        assert!(out.contains("let mut __state: usize = 0;"), "missing __state decl: {}", out);
        assert!(out.contains("loop {"), "missing loop: {}", out);
        assert!(out.contains("match __state {"), "missing match: {}", out);
        assert!(out.contains("_ => unreachable!(),"), "missing unreachable arm: {}", out);
    }

    #[test]
    fn multi_block_goto_sets_state_and_continues() {
        let func = minimal_cfg_fn(
            "goto_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: vec![] }),
                },
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
            ],
        );
        let out = render(&func);
        assert!(out.contains("__state = 1;"), "expected __state = 1: {}", out);
        assert!(out.contains("continue;"), "expected continue: {}", out);
    }

    #[test]
    fn multi_block_return_in_arm() {
        let func = minimal_cfg_fn(
            "ret_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: vec![] }),
                },
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(Some(IrExpr::Lit(IrLit::Int(42)))),
                },
            ],
        );
        let out = render(&func);
        assert!(out.contains("return 42;"), "expected return 42: {}", out);
    }

    // ── Block params: Option slot declaration ─────────────────────────────────

    #[test]
    fn block_params_declare_option_slots() {
        let func = minimal_cfg_fn(
            "param_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Goto(IrCfgJump {
                        target: 1,
                        args: vec![IrExpr::Lit(IrLit::Bool(false))],
                    }),
                },
                IrCfgBlock {
                    params: vec![IrParam {
                        name: "x".to_string(),
                        ty: IrType::Primitive(PrimitiveType::Bool),
                    }],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
            ],
        );
        let out = render(&func);
        assert!(
            out.contains("let mut __b1_p0: Option<bool> = None;"),
            "missing Option slot decl: {}",
            out
        );
    }

    #[test]
    fn jump_args_loaded_into_option_slots() {
        let func = minimal_cfg_fn(
            "arg_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Goto(IrCfgJump {
                        target: 1,
                        args: vec![IrExpr::Var("val".to_string())],
                    }),
                },
                IrCfgBlock {
                    params: vec![IrParam {
                        name: "y".to_string(),
                        ty: IrType::Primitive(PrimitiveType::Bool),
                    }],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
            ],
        );
        let out = render(&func);
        assert!(
            out.contains("__b1_p0 = Some(val);"),
            "expected slot assignment: {}",
            out
        );
    }

    #[test]
    fn block_params_taken_at_arm_entry() {
        let func = minimal_cfg_fn(
            "take_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Goto(IrCfgJump {
                        target: 1,
                        args: vec![IrExpr::Lit(IrLit::Int(0))],
                    }),
                },
                IrCfgBlock {
                    params: vec![IrParam {
                        name: "acc".to_string(),
                        ty: IrType::Primitive(PrimitiveType::U32),
                    }],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
            ],
        );
        let out = render(&func);
        assert!(
            out.contains("let mut acc = __b1_p0.take().unwrap();"),
            "expected take().unwrap() binding: {}",
            out
        );
    }

    // ── CondGoto ──────────────────────────────────────────────────────────────

    #[test]
    fn cond_goto_emits_if_else_structure() {
        let func = minimal_cfg_fn(
            "cond_fn",
            vec![
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::CondGoto {
                        cond: IrExpr::Var("flag".to_string()),
                        then_: IrCfgJump { target: 1, args: vec![] },
                        else_: IrCfgJump { target: 2, args: vec![] },
                    },
                },
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
                IrCfgBlock {
                    params: vec![],
                    stmts: vec![],
                    stmt_provs: vec![],
                    terminator: IrCfgTerminator::Return(None),
                },
            ],
        );
        let out = render(&func);
        assert!(out.contains("if flag {"), "expected if cond: {}", out);
        assert!(out.contains("} else {"), "expected else: {}", out);
        // both branches set __state
        assert!(out.contains("__state = 1;"), "expected __state = 1: {}", out);
        assert!(out.contains("__state = 2;"), "expected __state = 2: {}", out);
        assert!(out.contains("continue;"), "expected continue: {}", out);
    }

    // ── Enum round-trip: parse → print ───────────────────────────────────────

    #[test]
    fn enum_unit_variants_round_trip() {
        let source = r#"
            pub enum Color {
                Red,
                Green,
                Blue,
            }
        "#;
        let module = parse_source(source, "test", &[]).unwrap();
        assert_eq!(module.enums.len(), 1);
        let e = &module.enums[0];
        assert_eq!(e.kind.to_string(), "Color");
        assert_eq!(e.variants.len(), 3);
        assert_eq!(e.variants[0].name, "Red");
        assert!(matches!(e.variants[0].fields, crate::ir::IrEnumVariantData::Unit));

        let out = format!("{}", DisplayRust(ModuleWriter { module: &module }));
        assert!(out.contains("pub enum Color"), "missing enum decl: {}", out);
        assert!(out.contains("Red,"), "missing Red variant: {}", out);
        assert!(out.contains("Green,"), "missing Green variant: {}", out);
        assert!(out.contains("Blue,"), "missing Blue variant: {}", out);
    }

    #[test]
    fn enum_tuple_variants_round_trip() {
        let source = r#"
            pub enum Expr {
                Lit(u32),
                Add(u32, u32),
            }
        "#;
        let module = parse_source(source, "test", &[]).unwrap();
        assert_eq!(module.enums.len(), 1);
        let e = &module.enums[0];
        assert_eq!(e.variants.len(), 2);
        assert_eq!(e.variants[0].name, "Lit");
        assert!(matches!(&e.variants[0].fields, crate::ir::IrEnumVariantData::Tuple(tys) if tys.len() == 1));

        let out = format!("{}", DisplayRust(ModuleWriter { module: &module }));
        assert!(out.contains("Lit(u32)"), "missing Lit(u32): {}", out);
        assert!(out.contains("Add(u32, u32)"), "missing Add(u32, u32): {}", out);
    }

    #[test]
    fn enum_struct_variants_round_trip() {
        let source = r#"
            pub enum Message {
                Quit,
                Move { x: i32, y: i32 },
                Write(bool),
            }
        "#;
        let module = parse_source(source, "test", &[]).unwrap();
        assert_eq!(module.enums.len(), 1);
        let e = &module.enums[0];
        assert_eq!(e.variants.len(), 3);
        assert_eq!(e.variants[1].name, "Move");
        assert!(matches!(&e.variants[1].fields, crate::ir::IrEnumVariantData::Struct(fields) if fields.len() == 2));

        let out = format!("{}", DisplayRust(ModuleWriter { module: &module }));
        assert!(out.contains("Quit,"), "missing Quit: {}", out);
        assert!(out.contains("Move {"), "missing Move struct variant: {}", out);
        assert!(out.contains("x: i32"), "missing x field: {}", out);
        assert!(out.contains("Write(bool)"), "missing Write(bool): {}", out);
    }

    #[test]
    fn enum_with_generics_round_trip() {
        let source = r#"
            pub enum Option<T> {
                None,
                Some(T),
            }
        "#;
        let module = parse_source(source, "test", &[]).unwrap();
        assert_eq!(module.enums.len(), 1);
        let e = &module.enums[0];
        assert_eq!(e.generics.len(), 1);
        assert_eq!(e.generics[0].name, "T");

        let out = format!("{}", DisplayRust(ModuleWriter { module: &module }));
        assert!(out.contains("pub enum Option<T>"), "missing generic enum: {}", out);
        assert!(out.contains("None,"), "missing None: {}", out);
        assert!(out.contains("Some(T)"), "missing Some(T): {}", out);
    }

    #[test]
    fn oram_core_parses_successfully() {
        // Verify that volar-oram-core's total-Rust source is parseable
        // by the compiler. This is a critical integration check: the
        // ORAM server code must compile through the volar-compiler
        // pipeline to become circuit code.
        let source = r#"
            #[derive(Clone, Copy, PartialEq, Eq)]
            pub struct OramEntry<const B: usize> {
                pub addr: u64,
                pub leaf: u64,
                pub data: [u8; B],
            }

            impl<const B: usize> OramEntry<B> {
                pub fn dummy() -> Self {
                    Self {
                        addr: !0u64,
                        leaf: 0,
                        data: [0u8; B],
                    }
                }

                pub fn is_real(&self) -> bool {
                    self.addr != !0u64
                }
            }

            #[derive(Clone, Copy)]
            pub struct Bucket<const Z: usize, const B: usize> {
                pub entries: [OramEntry<B>; Z],
            }

            impl<const Z: usize, const B: usize> Bucket<Z, B> {
                pub fn empty() -> Self {
                    Self {
                        entries: [OramEntry::dummy(); Z],
                    }
                }
            }

            #[derive(Clone, Copy)]
            pub enum ServerRequest<const Z: usize, const B: usize, const L: usize> {
                ReadPath { leaf: u64 },
                WritePath {
                    leaf: u64,
                    buckets: [Bucket<Z, B>; L],
                },
            }

            #[derive(Clone, Copy)]
            pub enum ServerResponse<const Z: usize, const B: usize, const L: usize> {
                PathBuckets { buckets: [Bucket<Z, B>; L] },
                Ack,
            }

            pub fn path_indices<const L: usize>(leaf: u64) -> [usize; L] {
                let mut path = [0usize; L];
                let mut idx = 0usize;
                for level in 1..L {
                    let bit_pos = L - 1 - level;
                    if (leaf >> bit_pos) & 1 == 0 {
                        idx = 2 * idx + 1;
                    } else {
                        idx = 2 * idx + 2;
                    }
                    path[level] = idx;
                }
                path
            }

            pub fn read_path<const Z: usize, const B: usize, const L: usize, const N: usize>(
                tree_buckets: &[Bucket<Z, B>; N],
                leaf: u64,
            ) -> [Bucket<Z, B>; L] {
                let path = path_indices::<L>(leaf);
                let mut result = [Bucket::empty(); L];
                for i in 0..L {
                    result[i] = tree_buckets[path[i]];
                }
                result
            }

            pub fn write_path<const Z: usize, const B: usize, const L: usize, const N: usize>(
                tree_buckets: &mut [Bucket<Z, B>; N],
                leaf: u64,
                new_buckets: &[Bucket<Z, B>; L],
            ) {
                let path = path_indices::<L>(leaf);
                for i in 0..L {
                    tree_buckets[path[i]] = new_buckets[i];
                }
            }

            pub fn server_step<const Z: usize, const B: usize, const L: usize, const N: usize>(
                tree_buckets: &mut [Bucket<Z, B>; N],
                request: ServerRequest<Z, B, L>,
            ) -> ServerResponse<Z, B, L> {
                match request {
                    ServerRequest::ReadPath { leaf } => {
                        let buckets = read_path::<Z, B, L, N>(tree_buckets, leaf);
                        ServerResponse::PathBuckets { buckets }
                    }
                    ServerRequest::WritePath { leaf, buckets } => {
                        write_path::<Z, B, L, N>(tree_buckets, leaf, &buckets);
                        ServerResponse::Ack
                    }
                }
            }
        "#;
        let module = parse_source(source, "oram_core", &[]).unwrap();

        // Should have parsed: 2 structs, 2 enums, 2 impls, 4 functions
        assert_eq!(module.structs.len(), 2, "expected 2 structs (OramEntry, Bucket)");
        assert_eq!(module.enums.len(), 2, "expected 2 enums (ServerRequest, ServerResponse)");
        assert_eq!(module.impls.len(), 2, "expected 2 impls");
        assert_eq!(module.functions.len(), 4, "expected 4 functions");

        // Verify the round-trip prints without error
        let out = format!("{}", DisplayRust(ModuleWriter { module: &module }));
        assert!(out.contains("struct OramEntry"), "missing OramEntry struct: {}", out);
        assert!(out.contains("struct Bucket"), "missing Bucket struct: {}", out);
        assert!(out.contains("enum ServerRequest"), "missing ServerRequest enum: {}", out);
        assert!(out.contains("enum ServerResponse"), "missing ServerResponse enum: {}", out);
        assert!(out.contains("fn path_indices"), "missing path_indices fn: {}", out);
        assert!(out.contains("fn server_step"), "missing server_step fn: {}", out);

        // Verify derives are preserved through parse-print round-trip
        assert!(out.contains("#[derive(Clone, Copy, PartialEq, Eq)]"), "missing OramEntry derives: {}", out);
        // Bucket and enums have derive(Clone, Copy)
        let derive_clone_copy_count = out.matches("#[derive(Clone, Copy)]").count();
        assert_eq!(derive_clone_copy_count, 3, "expected 3 derive(Clone, Copy) (Bucket + 2 enums): {}", out);
    }
}
