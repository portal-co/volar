//! Pretty-printer for the unified specialized IR.

use crate::ir::*;
use std::fmt::Write;

pub fn print_module(module: &IrModule) -> String {
    let mut out = String::new();
    writeln!(out, "// Module: {}\n", module.name).unwrap();

    // Print uses
    if !module.uses.is_empty() {
        writeln!(out, "// Uses:").unwrap();
        for u in &module.uses {
            writeln!(out, "use {};", u).unwrap();
        }
        writeln!(out).unwrap();
    }

    // Print structs
    for s in &module.structs {
        write_struct(&mut out, s, 0);
        writeln!(out).unwrap();
    }

    // Print traits
    for t in &module.traits {
        write_trait(&mut out, t, 0);
        writeln!(out).unwrap();
    }

    // Print type aliases
    for ta in &module.type_aliases {
        write_type_alias(&mut out, ta, 0);
        writeln!(out).unwrap();
    }

    // Print impls
    for i in &module.impls {
        write_impl(&mut out, i, 0);
        writeln!(out).unwrap();
    }

    // Print standalone functions
    for f in &module.functions {
        write_function(&mut out, f, 0);
        writeln!(out).unwrap();
    }

    out
}

fn write_type_alias(out: &mut String, alias: &IrTypeAlias, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}pub type {}", indent, alias.name).unwrap();
    write_generics(out, &alias.generics);
    write!(out, " = ").unwrap();
    write_type(out, &alias.target);
    writeln!(out, ";").unwrap();
}

fn write_struct(out: &mut String, s: &IrStruct, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}pub struct {}", indent, s.kind).unwrap();
    write_generics(out, &s.generics);
    
    if s.is_tuple {
        write!(out, "(").unwrap();
        for (i, field) in s.fields.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            if field.public { write!(out, "pub ").unwrap(); }
            write_type(out, &field.ty);
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, " {{").unwrap();
        for field in &s.fields {
            write!(out, "{}    ", indent).unwrap();
            if field.public { write!(out, "pub ").unwrap(); }
            write!(out, "{}: ", field.name).unwrap();
            write_type(out, &field.ty);
            writeln!(out, ",").unwrap();
        }
        writeln!(out, "{}}}", indent).unwrap();
    }
}

fn write_trait(out: &mut String, t: &IrTrait, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}pub trait {}", indent, t.kind).unwrap();
    write_generics(out, &t.generics);
    
    if !t.super_traits.is_empty() {
        write!(out, ": ").unwrap();
        for (i, bound) in t.super_traits.iter().enumerate() {
            if i > 0 { write!(out, " + ").unwrap(); }
            write_trait_bound(out, bound);
        }
    }
    
    writeln!(out, " {{").unwrap();
    for item in &t.items {
        match item {
            IrTraitItem::Method(m) => {
                write!(out, "{}    fn {}", indent, m.name).unwrap();
                write_generics(out, &m.generics);
                write!(out, "(").unwrap();
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
                write_where_clause(out, &m.where_clause, level + 1);
                writeln!(out, ";").unwrap();
            }
            IrTraitItem::AssociatedType { name, bounds, default } => {
                write!(out, "{}    type {:?}", indent, name).unwrap();
                if !bounds.is_empty() {
                    write!(out, ": ").unwrap();
                    for (i, bound) in bounds.iter().enumerate() {
                        if i > 0 { write!(out, " + ").unwrap(); }
                        write_trait_bound(out, bound);
                    }
                }
                if let Some(def) = default {
                    write!(out, " = ").unwrap();
                    write_type(out, def);
                }
                writeln!(out, ";").unwrap();
            }
        }
    }
    writeln!(out, "{}}}", indent).unwrap();
}

fn write_impl(out: &mut String, i: &IrImpl, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}impl", indent).unwrap();
    write_generics(out, &i.generics);
    write!(out, " ").unwrap();
    
    if let Some(tr) = &i.trait_ {
        write!(out, "{} for ", tr.kind).unwrap();
        if !tr.type_args.is_empty() {
            write!(out, "<").unwrap();
            for (idx, arg) in tr.type_args.iter().enumerate() {
                if idx > 0 { write!(out, ", ").unwrap(); }
                write_type(out, arg);
            }
            write!(out, "> ").unwrap();
        }
    }
    
    write_type(out, &i.self_ty);
    write_where_clause(out, &i.where_clause, level);
    writeln!(out, " {{").unwrap();
    
    for item in &i.items {
        match item {
            IrImplItem::Method(f) => write_function(out, f, level + 1),
            IrImplItem::AssociatedType { name, ty } => {
                write!(out, "{}    type {:?} = ", indent, name).unwrap();
                write_type(out, ty);
                writeln!(out, ";").unwrap();
            }
        }
    }
    
    writeln!(out, "{}}}", indent).unwrap();
}

fn write_function(out: &mut String, f: &IrFunction, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}fn {}", indent, f.name).unwrap();
    write_generics(out, &f.generics);
    write!(out, "(").unwrap();
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
    write_where_clause(out, &f.where_clause, level);
    writeln!(out, " ").unwrap();
    write_block(out, &f.body, level);
    writeln!(out).unwrap();
}

fn write_generics(out: &mut String, generics: &[IrGenericParam]) {
    if generics.is_empty() { return; }
    write!(out, "<").unwrap();
    for (i, p) in generics.iter().enumerate() {
        if i > 0 { write!(out, ", ").unwrap(); }
        write!(out, "{}", p.name).unwrap();
        if !p.bounds.is_empty() {
            write!(out, ": ").unwrap();
            for (j, bound) in p.bounds.iter().enumerate() {
                if j > 0 { write!(out, " + ").unwrap(); }
                write_trait_bound(out, bound);
            }
        }
        if let Some(def) = &p.default {
            write!(out, " = ").unwrap();
            write_type(out, def);
        }
    }
    write!(out, ">").unwrap();
}

fn write_trait_bound(out: &mut String, bound: &IrTraitBound) {
    write!(out, "{}", bound.trait_kind).unwrap();
    if !bound.type_args.is_empty() || !bound.assoc_bindings.is_empty() {
        write!(out, "<").unwrap();
        let mut first = true;
        for arg in &bound.type_args {
            if !first { write!(out, ", ").unwrap(); }
            write_type(out, arg);
            first = false;
        }
        for (name, ty) in &bound.assoc_bindings {
            if !first { write!(out, ", ").unwrap(); }
            write!(out, "{:?} = ", name).unwrap();
            write_type(out, ty);
            first = false;
        }
        write!(out, ">").unwrap();
    }
}

fn write_where_clause(out: &mut String, where_clause: &[IrWherePredicate], level: usize) {
    if where_clause.is_empty() { return; }
    let indent = "    ".repeat(level);
    writeln!(out, "\n{}where", indent).unwrap();
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
        IrType::ImplTrait(bounds) => {
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
        IrExpr::Lit(l) => write!(out, "{}", l.to_string()).unwrap(),
        IrExpr::Var(v) => write!(out, "{}", v).unwrap(),
        IrExpr::Path { segments, type_args } => {
            write!(out, "{}", segments.join("::")).unwrap();
            if !type_args.is_empty() {
                write!(out, "::<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrExpr::Binary { op, left, right } => {
            write!(out, "(").unwrap();
            write_expr(out, left);
            write!(out, " {:?} ", op).unwrap();
            write_expr(out, right);
            write!(out, ")").unwrap();
        }
        IrExpr::Unary { op, expr } => {
            write!(out, "{:?}", op).unwrap();
            write_expr(out, expr);
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
        IrExpr::Call { func, args } => {
            write_expr(out, func);
            write!(out, "(").unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr(out, arg);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::BoundedLoop { var, start, end, body, .. } => {
            write!(out, "for {} in ", var).unwrap();
            write_expr(out, start);
            write!(out, "..").unwrap();
            write_expr(out, end);
            write!(out, " ").unwrap();
            write_block(out, body, 0);
        }
        IrExpr::ArrayGenerate { index_var, body, .. } => {
            write!(out, "GenericArray::generate(|{}| ", index_var).unwrap();
            write_expr(out, body);
            write!(out, ")").unwrap();
        }
        _ => write!(out, "todo!()").unwrap(),
    }
}

fn write_pattern(out: &mut String, pat: &IrPattern) {
    match pat {
        IrPattern::Ident { mutable, name, .. } => {
            if *mutable { write!(out, "mut ").unwrap(); }
            write!(out, "{}", name).unwrap();
        }
        IrPattern::Wild => write!(out, "_").unwrap(),
        _ => write!(out, "_").unwrap(),
    }
}

fn write_receiver(out: &mut String, r: IrReceiver) {
    match r {
        IrReceiver::Value => write!(out, "self").unwrap(),
        IrReceiver::Ref => write!(out, "&self").unwrap(),
        IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
    }
}
