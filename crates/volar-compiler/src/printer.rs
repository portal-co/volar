use core::fmt::Write;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::format;

use crate::ir::*;

pub fn print_module(module: &IrModule) -> String {
    let mut out = String::new();
    writeln!(out, "module {} {{", module.name).unwrap();

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
        write_function(&mut out, f, 0);
        writeln!(out).unwrap();
    }

    writeln!(out, "}}").unwrap();
    out
}

fn write_type_alias(_out: &mut String, _alias: &IrTypeAlias, _level: usize) {
    // TODO
}

fn write_struct(out: &mut String, s: &IrStruct, _level: usize) {
    write!(out, "struct {}", s.kind).unwrap();
    write_generics(out, &s.generics);
    
    if s.is_tuple {
        write!(out, "(").unwrap();
        for (i, f) in s.fields.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            write_type(out, &f.ty);
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, " {{").unwrap();
        for f in &s.fields {
            write!(out, "    {}: ", f.name).unwrap();
            write_type(out, &f.ty);
            writeln!(out, ",").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }
}

fn write_trait(out: &mut String, t: &IrTrait, _level: usize) {
    write!(out, "trait {}", t.kind).unwrap();
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
        write!(out, "{} for ", t.kind).unwrap();
    }
    write_type(out, &i.self_ty);
    writeln!(out, " {{").unwrap();
    for item in &i.items {
        match item {
            IrImplItem::Method(f) => write_function(out, f, 1),
            IrImplItem::AssociatedType { name, ty } => {
                write!(out, "    type {:?} = ", name).unwrap();
                write_type(out, ty);
                writeln!(out, ";").unwrap();
            }
        }
    }
    writeln!(out, "}}").unwrap();
}

fn write_function(out: &mut String, f: &IrFunction, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}fn {}(", indent, f.name).unwrap();
    if let Some(r) = f.receiver {
        write_receiver(out, r);
        if !f.params.is_empty() { write!(out, ", ").unwrap(); }
    }
    for (i, p) in f.params.iter().enumerate() {
        if i > 0 { write!(out, ", ").unwrap(); }
        write!(out, "{}: ", p.name).unwrap();
        write_type(out, &p.ty);
    }
    write!(out, ")").unwrap();
    if let Some(ret) = &f.return_type {
        write!(out, " -> ").unwrap();
        write_type(out, ret);
    }
    writeln!(out).unwrap();
    if !f.where_clause.is_empty() {
        write_where_clause(out, &f.where_clause, level);
    }
    write_block(out, &f.body, level);
    writeln!(out).unwrap();
}

fn write_generics(out: &mut String, generics: &[IrGenericParam]) {
    if !generics.is_empty() {
        write!(out, "<").unwrap();
        for (i, p) in generics.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            write!(out, "{}", p.name).unwrap();
        }
        write!(out, ">").unwrap();
    }
}

fn write_trait_bound(out: &mut String, bound: &IrTraitBound) {
    write!(out, "{}", bound.trait_kind).unwrap();
    if !bound.type_args.is_empty() {
        write!(out, "<").unwrap();
        for (i, arg) in bound.type_args.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            write_type(out, arg);
        }
        write!(out, ">").unwrap();
    }
}

fn write_where_clause(out: &mut String, where_clause: &[IrWherePredicate], level: usize) {
    let indent = "    ".repeat(level);
    writeln!(out, "{}where", indent).unwrap();
    for (i, pred) in where_clause.iter().enumerate() {
        write!(out, "{}    ", indent).unwrap();
        match pred {
            IrWherePredicate::TypeBound { ty, bounds } => {
                write_type(out, ty);
                write!(out, ": ").unwrap();
                for (j, bound) in bounds.iter().enumerate() {
                    if j > 0 { write!(out, " + ").unwrap(); }
                    write_trait_bound(out, bound);
                }
            }
        }
        if i < where_clause.len() - 1 { writeln!(out, ",").unwrap(); }
    }
}

fn write_type(out: &mut String, ty: &IrType) {
    match ty {
        IrType::Primitive(p) => write!(out, "{}", p).unwrap(),
        IrType::Array { kind: _, elem, len } => {
            write!(out, "[").unwrap();
            write_type(out, elem);
            write!(out, "; ").unwrap();
            match len {
                ArrayLength::Const(n) => write!(out, "{}", n).unwrap(),
                ArrayLength::TypeNum(tn) => write!(out, "{:?}", tn).unwrap(),
                ArrayLength::TypeParam(p) => write!(out, "{}", p).unwrap(),
                ArrayLength::Computed(e) => write_expr(out, e),
            }
            write!(out, "]").unwrap();
        }
        IrType::Struct { kind, type_args } => {
            write!(out, "{}", kind).unwrap();
            if !type_args.is_empty() {
                write!(out, "<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrType::TypeParam(p) => write!(out, "{}", p).unwrap(),
        IrType::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_type(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrType::Unit => write!(out, "()").unwrap(),
        IrType::Reference { mutable, elem } => {
            write!(out, "&{}", if *mutable { "mut " } else { "" }).unwrap();
            write_type(out, elem);
        }
        IrType::Projection { base, assoc } => {
            write!(out, "<").unwrap();
            write_type(out, base);
            write!(out, " as _>::{:?}", assoc).unwrap();
        }
        IrType::Existential { bounds } => {
            write!(out, "impl ").unwrap();
            for (i, bound) in bounds.iter().enumerate() {
                if i > 0 { write!(out, " + ").unwrap(); }
                write_trait_bound(out, bound);
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
            write!(out, " {:?} ", op).unwrap();
            write_expr(out, right);
            write!(out, ")").unwrap();
        }
        IrExpr::Unary { op, expr } => {
            write!(out, "{:?}(", op).unwrap();
            write_expr(out, expr);
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
        IrExpr::MethodCall { receiver, method, args, .. } => {
            write_expr(out, receiver);
            write!(out, ".{:?}(", method).unwrap();
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
            write!(out, ".map(|{}| ", elem_var).unwrap();
            write_expr(out, body);
            write!(out, ")").unwrap();
        }
        IrExpr::ArrayZip { left, right, left_var, right_var, body } => {
            write_expr(out, left);
            write!(out, ".zip(").unwrap();
            write_expr(out, right);
            write!(out, ").map(|({}, {})| ", left_var, right_var).unwrap();
            write_expr(out, body);
            write!(out, ")").unwrap();
        }
        _ => write!(out, "...").unwrap(),
    }
}

fn write_pattern(out: &mut String, pat: &IrPattern) {
    match pat {
        IrPattern::Ident { name, .. } => write!(out, "{}", name).unwrap(),
        IrPattern::Wild => write!(out, "_").unwrap(),
        _ => write!(out, "...").unwrap(),
    }
}

fn write_receiver(out: &mut String, r: IrReceiver) {
    match r {
        IrReceiver::Value => write!(out, "self").unwrap(),
        IrReceiver::Ref => write!(out, "&self").unwrap(),
        IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
    }
}
