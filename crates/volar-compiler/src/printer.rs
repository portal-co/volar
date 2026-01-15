use core::fmt::Write;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;
#[cfg(feature = "std")]
use std::format;

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::format;

use crate::ir::*;

pub fn print_module(module: &IrModule) -> String {
    let mut out = String::new();
    
    // File header for Rust
    writeln!(out, "//! Auto-generated dynamic types from volar-spec").unwrap();
    writeln!(out, "//! Type-level lengths have been converted to runtime usize witnesses").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]").unwrap();
    writeln!(out, "extern crate alloc;").unwrap();
    writeln!(out, "use alloc::vec::Vec;").unwrap();
    writeln!(out, "use alloc::vec;").unwrap();
    writeln!(out, "use core::ops::{{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr}};").unwrap();
    writeln!(out, "use typenum::Unsigned;").unwrap();
    writeln!(out, "use cipher::BlockEncrypt;").unwrap();
    writeln!(out, "use digest::Digest;").unwrap();
    writeln!(out, "use volar_common::hash_commitment::commit;").unwrap();
    writeln!(out).unwrap();
    
    writeln!(out, "/// Block cipher that can encrypt blocks and be created from a 32-byte key").unwrap();
    writeln!(out, "pub trait ByteBlockEncrypt: BlockEncrypt + From<[u8; 32]> {{}}").unwrap();
    writeln!(out, "impl<T: BlockEncrypt + From<[u8; 32]>> ByteBlockEncrypt for T {{}}").unwrap();
    writeln!(out).unwrap();
    
    writeln!(out, "// Primitive field types from volar-primitives").unwrap();
    writeln!(out, "pub use volar_primitives::{{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64}};").unwrap();
    writeln!(out).unwrap();

    writeln!(out, "/// Compute integer log2").unwrap();
    writeln!(out, "#[inline]").unwrap();
    writeln!(out, "pub fn ilog2(x: usize) -> u32 {{").unwrap();
    writeln!(out, "    usize::BITS - x.leading_zeros() - 1").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();

    for s in &module.structs {
        write_struct(&mut out, s, 0);
        writeln!(out).unwrap();
    }

    for t in &module.traits {
        write_trait(&mut out, t, 0);
        writeln!(out).unwrap();
    }

    for i in &module.impls {
        write_impl(&mut out, i, 0);
        writeln!(out).unwrap();
    }

    for f in &module.functions {
        write_function(&mut out, f, 0, false);
        writeln!(out).unwrap();
    }

    out
}

fn write_struct(out: &mut String, s: &IrStruct, _level: usize) {
    writeln!(out, "#[derive(Clone, Debug, Default)]").unwrap();
    write!(out, "pub struct {}", s.kind).unwrap();
    write_generics(out, &s.generics);
    
    if s.is_tuple {
        write!(out, "(").unwrap();
        for (i, f) in s.fields.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            write!(out, "pub ").unwrap();
            write_type(out, &f.ty);
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, " {{").unwrap();
        for f in &s.fields {
            write!(out, "    pub {}: ", f.name).unwrap();
            write_type(out, &f.ty);
            writeln!(out, ",").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }
}

fn write_trait(out: &mut String, t: &IrTrait, _level: usize) {
    write!(out, "pub trait {}", t.kind).unwrap();
    write_generics(out, &t.generics);
    writeln!(out, " {{").unwrap();
    for item in &t.items {
        match item {
            IrTraitItem::Method(m) => {
                write!(out, "    fn {}(", m.name).unwrap();
                if let Some(r) = m.receiver {
                    write_receiver(out, r);
                    if !m.params.is_empty() { write!(out, ", ").unwrap(); }
                }
                for (i, p) in m.params.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write!(out, "{}: ", p.name).unwrap();
                    write_type(out, &p.ty);
                }
                write!(out, ")").unwrap();
                if let Some(ret) = &m.return_type {
                    write!(out, " -> ").unwrap();
                    write_type(out, ret);
                }
                writeln!(out, ";").unwrap();
            }
            IrTraitItem::AssociatedType { name, .. } => {
                writeln!(out, "    type {:?};", name).unwrap();
            }
        }
    }
    writeln!(out, "}}").unwrap();
}

fn write_impl(out: &mut String, i: &IrImpl, _level: usize) {
    write!(out, "impl ").unwrap();
    write_generics(out, &i.generics);
    if let Some(t) = &i.trait_ {
        write!(out, " {} for ", t.kind).unwrap();
    } else {
        write!(out, " ").unwrap();
    }
    write_type(out, &i.self_ty);
    if !i.where_clause.is_empty() {
        write!(out, " where ").unwrap();
        for (idx, wp) in i.where_clause.iter().enumerate() {
            if idx > 0 { write!(out, ", ").unwrap(); }
            match wp {
                IrWherePredicate::TypeBound { ty, bounds } => {
                    write_type(out, ty);
                    write!(out, ": ").unwrap();
                    for (j, b) in bounds.iter().enumerate() {
                        if j > 0 { write!(out, " + ").unwrap(); }
                        write_trait_bound(out, b);
                    }
                }
            }
        }
    }
    writeln!(out, " {{").unwrap();
    for item in &i.items {
        match item {
            IrImplItem::Method(f) => write_function(out, f, 1, i.trait_.is_some()),
            IrImplItem::AssociatedType { name, ty } => {
                write!(out, "    type {:?} = ", name).unwrap();
                write_type(out, ty);
                writeln!(out, ";").unwrap();
            }
        }
    }
    writeln!(out, "}}").unwrap();
}

fn write_function(out: &mut String, f: &IrFunction, level: usize, is_trait_item: bool) {
    let indent = "    ".repeat(level);
    if !is_trait_item {
        write!(out, "{}pub fn {}(", indent, f.name).unwrap();
    } else {
        write!(out, "{}fn {}(", indent, f.name).unwrap();
    }
    if let Some(r) = f.receiver {
        write_receiver(out, r);
        if !f.params.is_empty() { write!(out, ", ").unwrap(); }
    }
    for (i, p) in f.params.iter().enumerate() {
        if i > 0 { write!(out, ", ").unwrap(); }
        write!(out, "mut {}: ", p.name).unwrap();
        write_type(out, &p.ty);
    }
    write!(out, ")").unwrap();
    if let Some(ret) = &f.return_type {
        write!(out, " -> ").unwrap();
        write_type(out, ret);
    }
    if !f.where_clause.is_empty() {
        write!(out, " where ").unwrap();
        for (idx, wp) in f.where_clause.iter().enumerate() {
            if idx > 0 { write!(out, ", ").unwrap(); }
            match wp {
                IrWherePredicate::TypeBound { ty, bounds } => {
                    write_type(out, ty);
                    write!(out, ": ").unwrap();
                    for (j, b) in bounds.iter().enumerate() {
                        if j > 0 { write!(out, " + ").unwrap(); }
                        write_trait_bound(out, b);
                    }
                }
            }
        }
    }
    writeln!(out).unwrap();
    write_block(out, &f.body, level);
    writeln!(out).unwrap();
}

fn write_generics(out: &mut String, generics: &[IrGenericParam]) {
    if !generics.is_empty() {
        write!(out, "<").unwrap();
        for (i, p) in generics.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            write!(out, "{}", p.name).unwrap();
            if !p.bounds.is_empty() {
                write!(out, ": ").unwrap();
                for (j, b) in p.bounds.iter().enumerate() {
                    if j > 0 { write!(out, " + ").unwrap(); }
                    write_trait_bound(out, b);
                }
            }
        }
        write!(out, ">").unwrap();
    }
}

fn write_trait_bound(out: &mut String, bound: &IrTraitBound) {
    match &bound.trait_kind {
        TraitKind::Into(ty) => {
            write!(out, "Into<").unwrap();
            write_type(out, ty);
            write!(out, ">").unwrap();
        }
        TraitKind::AsRef(ty) => {
            write!(out, "AsRef<").unwrap();
            write_type(out, ty);
            write!(out, ">").unwrap();
        }
        TraitKind::Fn(inp, ty) => {
            let inp_str = match inp {
                FnInput::BytesSlice => "&[u8]",
                FnInput::Size => "usize",
                FnInput::Bool => "bool",
            };
            write!(out, "FnMut({}) -> ", inp_str).unwrap();
            write_type(out, ty);
        }
        _ => write!(out, "{}", bound).unwrap(),
    }
}

fn write_type(out: &mut String, ty: &IrType) {
    match ty {
        IrType::Primitive(p) => write!(out, "{}", p).unwrap(),
        IrType::Vector { elem } => {
            write!(out, "Vec<").unwrap();
            write_type(out, elem);
            write!(out, ">").unwrap();
        }
        IrType::Array { kind, elem, len } => {
            if *kind == ArrayKind::Slice {
                write!(out, "[").unwrap();
                write_type(out, elem);
                write!(out, "]").unwrap();
            } else {
                write!(out, "Vec<").unwrap();
                write_type(out, elem);
                write!(out, ">").unwrap();
            }
        }
        IrType::Struct { kind, type_args } => {
            write!(out, "{}", kind).unwrap();
            if !type_args.is_empty() {
                write!(out, "<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 { write!(out, ", "); }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrType::TypeParam(p) => write!(out, "{}", p).unwrap(),
        IrType::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", "); }
                write_type(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrType::Unit => write!(out, "()").unwrap(),
        IrType::Reference { mutable, elem } => {
            write!(out, "&{}", if *mutable { "mut " } else { "" }).unwrap();
            write_type(out, elem);
        }
        IrType::Projection { base, trait_args, assoc } => {
            write!(out, "<").unwrap();
            write_type(out, base);
            write!(out, " as _").unwrap();
            if !trait_args.is_empty() {
                write!(out, "<").unwrap();
                for (i, arg) in trait_args.iter().enumerate() {
                    if i > 0 { write!(out, ", "); }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
            write!(out, ">::{:?}", assoc).unwrap();
        }
        IrType::Existential { bounds } => {
            write!(out, "impl ").unwrap();
            for (i, b) in bounds.iter().enumerate() {
                if i > 0 { write!(out, " + "); }
                write_trait_bound(out, b);
            }
        }
        _ => write!(out, "_").unwrap(),
    }
}

fn write_block(out: &mut String, block: &IrBlock, level: usize) {
    let indent = "    ".repeat(level);
    writeln!(out, "{}{{", indent).unwrap();
    for stmt in &block.stmts {
        write_stmt(out, stmt, level + 1);
    }
    if let Some(e) = &block.expr {
        write!(out, "{}    ", indent).unwrap();
        write_expr(out, e);
        writeln!(out).unwrap();
    }
    write!(out, "{}}}", indent).unwrap();
}

fn write_stmt(out: &mut String, stmt: &IrStmt, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}", indent).unwrap();
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            write!(out, "let ").unwrap();
            write_pattern(out, pattern);
            if let Some(t) = ty {
                write!(out, ": ").unwrap();
                write_type(out, t);
            }
            if let Some(i) = init {
                write!(out, " = ").unwrap();
                write_expr(out, i);
            }
            writeln!(out, ";").unwrap();
        }
        IrStmt::Semi(e) => {
            write_expr(out, e);
            writeln!(out, ";").unwrap();
        }
        IrStmt::Expr(e) => {
            write_expr(out, e);
            writeln!(out).unwrap();
        }
    }
}

fn write_expr(out: &mut String, expr: &IrExpr) {
    match expr {
        IrExpr::Lit(l) => write!(out, "{}", l).unwrap(),
        IrExpr::Var(v) => write!(out, "{}", v).unwrap(),
        IrExpr::Binary { op, left, right } => {
            write!(out, "(").unwrap();
            write_expr(out, left);
            write!(out, " {} ", bin_op_str(*op)).unwrap();
            write_expr(out, right);
            write!(out, ")").unwrap();
        }
        IrExpr::MethodCall { receiver, method, args, .. } => {
            write_expr(out, receiver);
            let name = match method {
                MethodKind::Std(s) => s.clone(),
                MethodKind::Crypto(c) => format!("{:?}", c).to_lowercase(),
                MethodKind::Vole(v) => format!("{:?}", v).to_lowercase(),
                MethodKind::Unknown(s) => s.clone(),
            };
            write!(out, ".{}(", name).unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr(out, arg);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Call { func, args } => {
            write_expr(out, func);
            write!(out, "(").unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr(out, arg);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Field { base, field } => {
            write_expr(out, base);
            write!(out, ".{}", field).unwrap();
        }
        IrExpr::Index { base, index } => {
            write_expr(out, base);
            write!(out, "[").unwrap();
            write_expr(out, index);
            write!(out, "]").unwrap();
        }
        IrExpr::Block(b) => write_block(out, b, 0),
        IrExpr::If { cond, then_branch, else_branch } => {
            write!(out, "if ").unwrap();
            write_expr(out, cond);
            write_block(out, then_branch, 0);
            if let Some(eb) = else_branch {
                write!(out, " else ").unwrap();
                write_expr(out, eb);
            }
        }
        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            write!(out, "for {} in ", var).unwrap();
            write_expr(out, start);
            write!(out, "{} ", if *inclusive { "..=" } else { ".." }).unwrap();
            write_expr(out, end);
            write_block(out, body, 0);
        }
        IrExpr::IterLoop { pattern, collection, body } => {
            write!(out, "for ").unwrap();
            write_pattern(out, pattern);
            write!(out, " in ").unwrap();
            write_expr(out, collection);
            write_block(out, body, 0);
        }
        IrExpr::ArrayMap { array, elem_var, body } => {
            write_expr(out, array);
            write!(out, ".into_iter().map(|{}| ", elem_var).unwrap();
            write_expr(out, body);
            write!(out, ").collect::<Vec<_>>()").unwrap();
        }
        IrExpr::ArrayZip { left, right, left_var, right_var, body } => {
            write_expr(out, left);
            write!(out, ".into_iter().zip(").unwrap();
            write_expr(out, right);
            write!(out, ").map(|({}, {})| ", left_var, right_var).unwrap();
            write_expr(out, body);
            write!(out, ").collect::<Vec<_>>()").unwrap();
        }
        IrExpr::ArrayFold { array, init, acc_var, elem_var, body } => {
            write_expr(out, array);
            write!(out, ".iter().fold(").unwrap();
            write_expr(out, init);
            write!(out, ", |{}, {}| ", acc_var, elem_var).unwrap();
            write_expr(out, body);
            write!(out, ")").unwrap();
        }
        IrExpr::ArrayGenerate { index_var, body, len, .. } => {
            let len_str = match len {
                ArrayLength::Const(n) => n.to_string(),
                ArrayLength::TypeNum(tn) => tn.to_usize().to_string(),
                ArrayLength::TypeParam(p) => p.to_lowercase(),
                ArrayLength::Computed(e) => {
                    let mut s = String::new();
                    write_expr(&mut s, e);
                    s
                }
            };
            write!(out, "(0..{}).map(|{}| ", len_str, index_var).unwrap();
            write_expr(out, body);
            write!(out, ").collect::<Vec<_>>()").unwrap();
        }
        IrExpr::Unary { op, expr } => {
            match op {
                SpecUnaryOp::Neg => write!(out, "-").unwrap(),
                SpecUnaryOp::Not => write!(out, "!").unwrap(),
                SpecUnaryOp::Deref => write!(out, "*").unwrap(),
                SpecUnaryOp::Ref => write!(out, "&").unwrap(),
                SpecUnaryOp::RefMut => write!(out, "&mut ").unwrap(),
            }
            write_expr(out, expr);
        }
        IrExpr::Path { segments, type_args } => {
            write!(out, "{}", segments.join("::")).unwrap();
            if !type_args.is_empty() {
                write!(out, "::<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 { write!(out, ", "); }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrExpr::Closure { params, body, .. } => {
            write!(out, "|").unwrap();
            for (i, p) in params.iter().enumerate() {
                if i > 0 { write!(out, ", "); }
                write_pattern(out, &p.pattern);
            }
            write!(out, "| ").unwrap();
            write_expr(out, body);
        }
        IrExpr::Range { start, end, inclusive } => {
            if let Some(s) = start { write_expr(out, s); }
            write!(out, "{}", if *inclusive { "..=" } else { ".." }).unwrap();
            if let Some(e) = end { write_expr(out, e); }
        }
        IrExpr::Assign { left, right } => {
            write_expr(out, left);
            write!(out, " = ").unwrap();
            write_expr(out, right);
        }
        IrExpr::StructExpr { kind, fields, .. } => {
            write!(out, "{} {{ ", kind).unwrap();
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write!(out, "{}: ", name).unwrap();
                write_expr(out, val);
            }
            write!(out, " }}").unwrap();
        }
        IrExpr::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr(out, e);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Array(elems) => {
            write!(out, "vec![").unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr(out, e);
            }
            write!(out, "]").unwrap();
        }
        IrExpr::Repeat { elem, len } => {
            write!(out, "vec![").unwrap();
            write_expr(out, elem);
            write!(out, "; ").unwrap();
            write_expr(out, len);
            write!(out, "]").unwrap();
        }
        IrExpr::Cast { expr, ty } => {
            write!(out, "(").unwrap();
            write_expr(out, expr);
            write!(out, " as ").unwrap();
            write_type(out, ty);
            write!(out, ")").unwrap();
        }
        IrExpr::Return(e) => {
            write!(out, "return").unwrap();
            if let Some(e) = e {
                write!(out, " ").unwrap();
                write_expr(out, e);
            }
        }
        IrExpr::Macro { name, tokens } => {
            if name == "typenum_usize" {
                write!(out, "<{} as typenum::Unsigned>::USIZE", tokens).unwrap();
            } else {
                write!(out, "{}!({})", name, tokens).unwrap();
            }
        }
        _ => {
            let msg = format!("{:?}", expr).replace('"', "'");
            write!(out, "compile_error!(\"Unsupported expression in printer: {}\")", msg).unwrap();
        }
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

fn write_pattern(out: &mut String, pat: &IrPattern) {
    match pat {
        IrPattern::Ident { name, .. } => write!(out, "{}", name).unwrap(),
        IrPattern::Wild => write!(out, "_").unwrap(),
        IrPattern::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, p) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", "); }
                write_pattern(out, p);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::TupleStruct { kind, elems } => {
            write!(out, "{}(", kind).unwrap();
            for (i, p) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", "); }
                write_pattern(out, p);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::Struct { kind, fields, rest } => {
            write!(out, "{} {{ ", kind).unwrap();
            for (i, (name, p)) in fields.iter().enumerate() {
                if i > 0 { write!(out, ", "); }
                write!(out, "{}: ", name).unwrap();
                write_pattern(out, p);
            }
            if *rest {
                if !fields.is_empty() { write!(out, ", "); }
                write!(out, "..").unwrap();
            }
            write!(out, " }}").unwrap();
        }
        _ => write!(out, "..").unwrap(),
    }
}

fn write_receiver(out: &mut String, r: IrReceiver) {
    match r {
        IrReceiver::Value => write!(out, "self").unwrap(),
        IrReceiver::Ref => write!(out, "&self").unwrap(),
        IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
    }
}
