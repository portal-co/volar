//! Utilities for pretty-printing the IR.
//!
//! This module provides human-readable output of the IR which is useful
//! for debugging and understanding the parsed code structure.

use crate::ir::*;
use std::fmt::Write;

/// Pretty-print an IR module
pub fn print_module(module: &IrModule) -> String {
    let mut out = String::new();
    
    writeln!(&mut out, "// Module: {}", module.name).unwrap();
    writeln!(&mut out).unwrap();

    // Uses
    if !module.uses.is_empty() {
        writeln!(&mut out, "// Uses:").unwrap();
        for u in &module.uses {
            let path = u.path.join("::");
            if u.glob {
                writeln!(&mut out, "use {}::*;", path).unwrap();
            } else if let Some(alias) = &u.alias {
                writeln!(&mut out, "use {} as {};", path, alias).unwrap();
            } else {
                writeln!(&mut out, "use {};", path).unwrap();
            }
        }
        writeln!(&mut out).unwrap();
    }

    // Type aliases
    for alias in &module.type_aliases {
        write_type_alias(&mut out, alias, 0);
    }

    // Structs
    for s in &module.structs {
        write_struct(&mut out, s, 0);
    }

    // Traits
    for t in &module.traits {
        write_trait(&mut out, t, 0);
    }

    // Functions
    for f in &module.functions {
        write_function(&mut out, f, 0);
    }

    // Impls
    for i in &module.impls {
        write_impl(&mut out, i, 0);
    }

    out
}

fn indent(out: &mut String, level: usize) {
    for _ in 0..level {
        out.push_str("    ");
    }
}

fn write_type_alias(out: &mut String, alias: &IrTypeAlias, level: usize) {
    indent(out, level);
    write!(out, "type {}", alias.name).unwrap();
    write_generics(out, &alias.generics);
    write!(out, " = ").unwrap();
    write_type(out, &alias.ty);
    writeln!(out, ";").unwrap();
    writeln!(out).unwrap();
}

fn write_struct(out: &mut String, s: &IrStruct, level: usize) {
    indent(out, level);
    write!(out, "pub struct {}", s.name).unwrap();
    write_generics(out, &s.generics);
    
    if s.fields.is_empty() {
        writeln!(out, ";").unwrap();
    } else if s.is_tuple {
        write!(out, "(").unwrap();
        for (i, field) in s.fields.iter().enumerate() {
            if i > 0 {
                write!(out, ", ").unwrap();
            }
            if field.visibility == Visibility::Public {
                write!(out, "pub ").unwrap();
            }
            write_type(out, &field.ty);
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, " {{").unwrap();
        for field in &s.fields {
            indent(out, level + 1);
            if field.visibility == Visibility::Public {
                write!(out, "pub ").unwrap();
            }
            write!(out, "{}: ", field.name).unwrap();
            write_type(out, &field.ty);
            writeln!(out, ",").unwrap();
        }
        indent(out, level);
        writeln!(out, "}}").unwrap();
    }
    writeln!(out).unwrap();
}

fn write_trait(out: &mut String, t: &IrTrait, level: usize) {
    indent(out, level);
    write!(out, "pub trait {}", t.name).unwrap();
    write_generics(out, &t.generics);
    
    if !t.super_traits.is_empty() {
        write!(out, ": ").unwrap();
        for (i, bound) in t.super_traits.iter().enumerate() {
            if i > 0 {
                write!(out, " + ").unwrap();
            }
            write_trait_bound(out, bound);
        }
    }
    
    writeln!(out, " {{").unwrap();
    
    for item in &t.items {
        match item {
            IrTraitItem::Method(sig) => {
                indent(out, level + 1);
                write!(out, "fn {}", sig.name).unwrap();
                write_generics(out, &sig.generics);
                write!(out, "(").unwrap();
                if let Some(recv) = &sig.receiver {
                    match recv {
                        IrReceiver::Value => write!(out, "self").unwrap(),
                        IrReceiver::Ref => write!(out, "&self").unwrap(),
                        IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
                    }
                    if !sig.params.is_empty() {
                        write!(out, ", ").unwrap();
                    }
                }
                for (i, param) in sig.params.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write!(out, "{}: ", param.name).unwrap();
                    write_type(out, &param.ty);
                }
                write!(out, ")").unwrap();
                if let Some(ret) = &sig.return_type {
                    write!(out, " -> ").unwrap();
                    write_type(out, ret);
                }
                writeln!(out, ";").unwrap();
            }
            IrTraitItem::AssociatedType { name, bounds, default } => {
                indent(out, level + 1);
                write!(out, "type {}", name).unwrap();
                if !bounds.is_empty() {
                    write!(out, ": ").unwrap();
                    for (i, bound) in bounds.iter().enumerate() {
                        if i > 0 {
                            write!(out, " + ").unwrap();
                        }
                        write_trait_bound(out, bound);
                    }
                }
                if let Some(def) = default {
                    write!(out, " = ").unwrap();
                    write_type(out, def);
                }
                writeln!(out, ";").unwrap();
            }
            IrTraitItem::Const { name, ty, default } => {
                indent(out, level + 1);
                write!(out, "const {}: ", name).unwrap();
                write_type(out, ty);
                if let Some(def) = default {
                    write!(out, " = ").unwrap();
                    write_expr(out, def, 0);
                }
                writeln!(out, ";").unwrap();
            }
        }
    }
    
    indent(out, level);
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn write_impl(out: &mut String, imp: &IrImpl, level: usize) {
    indent(out, level);
    write!(out, "impl").unwrap();
    write_generics(out, &imp.generics);
    write!(out, " ").unwrap();
    
    if let Some(trait_ref) = &imp.trait_ {
        write!(out, "{}", trait_ref.path.join("::")).unwrap();
        if !trait_ref.type_args.is_empty() {
            write!(out, "<").unwrap();
            for (i, arg) in trait_ref.type_args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_type(out, arg);
            }
            write!(out, ">").unwrap();
        }
        write!(out, " for ").unwrap();
    }
    
    write_type(out, &imp.self_ty);
    write_where_clause(out, &imp.where_clause, level);
    writeln!(out, " {{").unwrap();
    
    for item in &imp.items {
        match item {
            IrImplItem::Method(f) => {
                write_function(out, f, level + 1);
            }
            IrImplItem::AssociatedType { name, ty } => {
                indent(out, level + 1);
                write!(out, "type {} = ", name).unwrap();
                write_type(out, ty);
                writeln!(out, ";").unwrap();
            }
            IrImplItem::Const { name, ty, value } => {
                indent(out, level + 1);
                write!(out, "const {}: ", name).unwrap();
                write_type(out, ty);
                write!(out, " = ").unwrap();
                write_expr(out, value, level + 1);
                writeln!(out, ";").unwrap();
            }
        }
    }
    
    indent(out, level);
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn write_function(out: &mut String, f: &IrFunction, level: usize) {
    indent(out, level);
    write!(out, "fn {}", f.name).unwrap();
    write_generics(out, &f.generics);
    write!(out, "(").unwrap();
    
    let mut first = true;
    if let Some(recv) = &f.receiver {
        first = false;
        match recv {
            IrReceiver::Value => write!(out, "self").unwrap(),
            IrReceiver::Ref => write!(out, "&self").unwrap(),
            IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
        }
    }
    
    for param in &f.params {
        if !first {
            write!(out, ", ").unwrap();
        }
        first = false;
        write!(out, "{}: ", param.name).unwrap();
        write_type(out, &param.ty);
    }
    write!(out, ")").unwrap();
    
    if let Some(ret) = &f.return_type {
        write!(out, " -> ").unwrap();
        write_type(out, ret);
    }
    
    write_where_clause(out, &f.where_clause, level);
    writeln!(out, " {{").unwrap();
    write_block(out, &f.body, level + 1);
    indent(out, level);
    writeln!(out, "}}").unwrap();
}

fn write_generics(out: &mut String, generics: &[IrGenericParam]) {
    if generics.is_empty() {
        return;
    }
    
    write!(out, "<").unwrap();
    for (i, param) in generics.iter().enumerate() {
        if i > 0 {
            write!(out, ", ").unwrap();
        }
        write!(out, "{}", param.name).unwrap();
        if !param.bounds.is_empty() {
            write!(out, ": ").unwrap();
            for (j, bound) in param.bounds.iter().enumerate() {
                if j > 0 {
                    write!(out, " + ").unwrap();
                }
                write_trait_bound(out, bound);
            }
        }
        if let Some(default) = &param.default {
            write!(out, " = ").unwrap();
            write_type(out, default);
        }
    }
    write!(out, ">").unwrap();
}

fn write_where_clause(out: &mut String, preds: &[IrWherePredicate], _level: usize) {
    if preds.is_empty() {
        return;
    }
    
    write!(out, "\nwhere\n").unwrap();
    for pred in preds {
        write!(out, "    ").unwrap();
        match pred {
            IrWherePredicate::TypeBound { ty, bounds } => {
                write_type(out, ty);
                write!(out, ": ").unwrap();
                for (i, bound) in bounds.iter().enumerate() {
                    if i > 0 {
                        write!(out, " + ").unwrap();
                    }
                    write_trait_bound(out, bound);
                }
            }
            IrWherePredicate::Lifetime { name, bounds } => {
                write!(out, "'{}", name).unwrap();
                if !bounds.is_empty() {
                    write!(out, ": ").unwrap();
                    for (i, b) in bounds.iter().enumerate() {
                        if i > 0 {
                            write!(out, " + ").unwrap();
                        }
                        write!(out, "'{}", b).unwrap();
                    }
                }
            }
        }
        writeln!(out, ",").unwrap();
    }
}

fn write_trait_bound(out: &mut String, bound: &IrTraitBound) {
    write!(out, "{}", bound.path.join("::")).unwrap();
    
    let has_args = !bound.type_args.is_empty() || !bound.assoc_type_bindings.is_empty();
    if has_args {
        write!(out, "<").unwrap();
        let mut first = true;
        for arg in &bound.type_args {
            if !first {
                write!(out, ", ").unwrap();
            }
            first = false;
            write_type(out, arg);
        }
        for (name, ty) in &bound.assoc_type_bindings {
            if !first {
                write!(out, ", ").unwrap();
            }
            first = false;
            write!(out, "{} = ", name).unwrap();
            write_type(out, ty);
        }
        write!(out, ">").unwrap();
    }
}

fn write_type(out: &mut String, ty: &IrType) {
    match ty {
        IrType::Path { segments, type_args } => {
            write!(out, "{}", segments.join("::")).unwrap();
            if !type_args.is_empty() {
                write!(out, "<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrType::Reference { mutable, elem } => {
            if *mutable {
                write!(out, "&mut ").unwrap();
            } else {
                write!(out, "&").unwrap();
            }
            write_type(out, elem);
        }
        IrType::Slice(elem) => {
            write!(out, "[").unwrap();
            write_type(out, elem);
            write!(out, "]").unwrap();
        }
        IrType::Array { elem, len: _ } => {
            write!(out, "[").unwrap();
            write_type(out, elem);
            write!(out, "; _]").unwrap();
        }
        IrType::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_type(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrType::Unit => write!(out, "()").unwrap(),
        IrType::Never => write!(out, "!").unwrap(),
        IrType::Infer => write!(out, "_").unwrap(),
        IrType::Projection { base, assoc, type_args } => {
            write!(out, "<").unwrap();
            write_type(out, base);
            write!(out, ">::{}", assoc).unwrap();
            if !type_args.is_empty() {
                write!(out, "<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrType::FnPtr { params, ret } => {
            write!(out, "fn(").unwrap();
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_type(out, param);
            }
            write!(out, ") -> ").unwrap();
            write_type(out, ret);
        }
        IrType::ImplTrait(bounds) => {
            write!(out, "impl ").unwrap();
            for (i, bound) in bounds.iter().enumerate() {
                if i > 0 {
                    write!(out, " + ").unwrap();
                }
                write_trait_bound(out, bound);
            }
        }
        IrType::DynTrait(bounds) => {
            write!(out, "dyn ").unwrap();
            for (i, bound) in bounds.iter().enumerate() {
                if i > 0 {
                    write!(out, " + ").unwrap();
                }
                write_trait_bound(out, bound);
            }
        }
    }
}

fn write_block(out: &mut String, block: &IrBlock, level: usize) {
    for stmt in &block.stmts {
        write_stmt(out, stmt, level);
    }
    if let Some(expr) = &block.expr {
        indent(out, level);
        write_expr(out, expr, level);
        writeln!(out).unwrap();
    }
}

fn write_stmt(out: &mut String, stmt: &IrStmt, level: usize) {
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            indent(out, level);
            write!(out, "let ").unwrap();
            write_pattern(out, pattern);
            if let Some(t) = ty {
                write!(out, ": ").unwrap();
                write_type(out, t);
            }
            if let Some(e) = init {
                write!(out, " = ").unwrap();
                write_expr(out, e, level);
            }
            writeln!(out, ";").unwrap();
        }
        IrStmt::Semi(expr) => {
            indent(out, level);
            write_expr(out, expr, level);
            writeln!(out, ";").unwrap();
        }
        IrStmt::Expr(expr) => {
            indent(out, level);
            write_expr(out, expr, level);
            writeln!(out).unwrap();
        }
        IrStmt::Item(item) => {
            match item {
                IrItemStmt::Fn(f) => write_function(out, f, level),
                IrItemStmt::Struct(s) => write_struct(out, s, level),
                IrItemStmt::Const { name, ty, value } => {
                    indent(out, level);
                    write!(out, "const {}: ", name).unwrap();
                    write_type(out, ty);
                    write!(out, " = ").unwrap();
                    write_expr(out, value, level);
                    writeln!(out, ";").unwrap();
                }
            }
        }
    }
}

fn write_pattern(out: &mut String, pat: &IrPattern) {
    match pat {
        IrPattern::Ident { mutable, name, subpat } => {
            if *mutable {
                write!(out, "mut ").unwrap();
            }
            write!(out, "{}", name).unwrap();
            if let Some(sub) = subpat {
                write!(out, " @ ").unwrap();
                write_pattern(out, sub);
            }
        }
        IrPattern::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::Struct { path, fields, rest } => {
            write!(out, "{} {{ ", path.join("::")).unwrap();
            for (i, (name, pat)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: ", name).unwrap();
                write_pattern(out, pat);
            }
            if *rest {
                if !fields.is_empty() {
                    write!(out, ", ").unwrap();
                }
                write!(out, "..").unwrap();
            }
            write!(out, " }}").unwrap();
        }
        IrPattern::TupleStruct { path, elems } => {
            write!(out, "{}(", path.join("::")).unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::Wild => write!(out, "_").unwrap(),
        IrPattern::Lit(lit) => write_lit(out, lit),
        IrPattern::Ref { mutable, pat } => {
            if *mutable {
                write!(out, "&mut ").unwrap();
            } else {
                write!(out, "&").unwrap();
            }
            write_pattern(out, pat);
        }
        IrPattern::Or(pats) => {
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    write!(out, " | ").unwrap();
                }
                write_pattern(out, p);
            }
        }
        IrPattern::Slice(elems) => {
            write!(out, "[").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern(out, elem);
            }
            write!(out, "]").unwrap();
        }
        IrPattern::Rest => write!(out, "..").unwrap(),
    }
}

fn write_expr(out: &mut String, expr: &IrExpr, level: usize) {
    match expr {
        IrExpr::Lit(lit) => write_lit(out, lit),
        IrExpr::Path { segments, type_args } => {
            write!(out, "{}", segments.join("::")).unwrap();
            if !type_args.is_empty() {
                write!(out, "::<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrExpr::Binary { op, left, right } => {
            write!(out, "(").unwrap();
            write_expr(out, left, level);
            write!(out, " {} ", bin_op_str(op)).unwrap();
            write_expr(out, right, level);
            write!(out, ")").unwrap();
        }
        IrExpr::Unary { op, expr: e } => {
            write!(out, "{}", unary_op_str(op)).unwrap();
            write_expr(out, e, level);
        }
        IrExpr::Call { func, args } => {
            write_expr(out, func, level);
            write!(out, "(").unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr(out, arg, level);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::MethodCall { receiver, method, type_args, args } => {
            write_expr(out, receiver, level);
            write!(out, ".{}", method).unwrap();
            if !type_args.is_empty() {
                write!(out, "::<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write_type(out, arg);
                }
                write!(out, ">").unwrap();
            }
            write!(out, "(").unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr(out, arg, level);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Field { base, field } => {
            write_expr(out, base, level);
            write!(out, ".{}", field).unwrap();
        }
        IrExpr::Index { base, index } => {
            write_expr(out, base, level);
            write!(out, "[").unwrap();
            write_expr(out, index, level);
            write!(out, "]").unwrap();
        }
        IrExpr::Struct { path, fields, rest } => {
            write!(out, "{} {{ ", path.join("::")).unwrap();
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: ", name).unwrap();
                write_expr(out, val, level);
            }
            if let Some(r) = rest {
                if !fields.is_empty() {
                    write!(out, ", ").unwrap();
                }
                write!(out, "..").unwrap();
                write_expr(out, r, level);
            }
            write!(out, " }}").unwrap();
        }
        IrExpr::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr(out, elem, level);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Array(elems) => {
            write!(out, "[").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr(out, elem, level);
            }
            write!(out, "]").unwrap();
        }
        IrExpr::Block(block) => {
            writeln!(out, "{{").unwrap();
            write_block(out, block, level + 1);
            indent(out, level);
            write!(out, "}}").unwrap();
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            write!(out, "if ").unwrap();
            write_expr(out, cond, level);
            writeln!(out, " {{").unwrap();
            write_block(out, then_branch, level + 1);
            indent(out, level);
            write!(out, "}}").unwrap();
            if let Some(else_expr) = else_branch {
                write!(out, " else ").unwrap();
                write_expr(out, else_expr, level);
            }
        }
        IrExpr::ForLoop { pattern, iter, body } => {
            write!(out, "for ").unwrap();
            write_pattern(out, pattern);
            write!(out, " in ").unwrap();
            write_expr(out, iter, level);
            writeln!(out, " {{").unwrap();
            write_block(out, body, level + 1);
            indent(out, level);
            write!(out, "}}").unwrap();
        }
        IrExpr::While { cond, body } => {
            write!(out, "while ").unwrap();
            write_expr(out, cond, level);
            writeln!(out, " {{").unwrap();
            write_block(out, body, level + 1);
            indent(out, level);
            write!(out, "}}").unwrap();
        }
        IrExpr::Loop { body } => {
            writeln!(out, "loop {{").unwrap();
            write_block(out, body, level + 1);
            indent(out, level);
            write!(out, "}}").unwrap();
        }
        IrExpr::Return(e) => {
            write!(out, "return").unwrap();
            if let Some(val) = e {
                write!(out, " ").unwrap();
                write_expr(out, val, level);
            }
        }
        IrExpr::Break(e) => {
            write!(out, "break").unwrap();
            if let Some(val) = e {
                write!(out, " ").unwrap();
                write_expr(out, val, level);
            }
        }
        IrExpr::Continue => write!(out, "continue").unwrap(),
        IrExpr::Closure { params, ret_type, body } => {
            write!(out, "|").unwrap();
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern(out, &param.pattern);
                if let Some(ty) = &param.ty {
                    write!(out, ": ").unwrap();
                    write_type(out, ty);
                }
            }
            write!(out, "|").unwrap();
            if let Some(ret) = ret_type {
                write!(out, " -> ").unwrap();
                write_type(out, ret);
            }
            write!(out, " ").unwrap();
            write_expr(out, body, level);
        }
        IrExpr::Ref { mutable, expr: e } => {
            if *mutable {
                write!(out, "&mut ").unwrap();
            } else {
                write!(out, "&").unwrap();
            }
            write_expr(out, e, level);
        }
        IrExpr::Deref(e) => {
            write!(out, "*").unwrap();
            write_expr(out, e, level);
        }
        IrExpr::Cast { expr: e, ty } => {
            write_expr(out, e, level);
            write!(out, " as ").unwrap();
            write_type(out, ty);
        }
        IrExpr::Assign { left, right } => {
            write_expr(out, left, level);
            write!(out, " = ").unwrap();
            write_expr(out, right, level);
        }
        IrExpr::AssignOp { op, left, right } => {
            write_expr(out, left, level);
            write!(out, " {}= ", bin_op_str(op)).unwrap();
            write_expr(out, right, level);
        }
        IrExpr::Range { start, end, inclusive } => {
            if let Some(s) = start {
                write_expr(out, s, level);
            }
            if *inclusive {
                write!(out, "..=").unwrap();
            } else {
                write!(out, "..").unwrap();
            }
            if let Some(e) = end {
                write_expr(out, e, level);
            }
        }
        IrExpr::Match { expr: e, arms } => {
            write!(out, "match ").unwrap();
            write_expr(out, e, level);
            writeln!(out, " {{").unwrap();
            for arm in arms {
                indent(out, level + 1);
                write_pattern(out, &arm.pattern);
                if let Some(guard) = &arm.guard {
                    write!(out, " if ").unwrap();
                    write_expr(out, guard, level + 1);
                }
                write!(out, " => ").unwrap();
                write_expr(out, &arm.body, level + 1);
                writeln!(out, ",").unwrap();
            }
            indent(out, level);
            write!(out, "}}").unwrap();
        }
        IrExpr::Macro { path, tokens } => {
            write!(out, "{}!({})", path.join("::"), tokens).unwrap();
        }
        IrExpr::Paren(e) => {
            write!(out, "(").unwrap();
            write_expr(out, e, level);
            write!(out, ")").unwrap();
        }
        IrExpr::Try(e) => {
            write_expr(out, e, level);
            write!(out, "?").unwrap();
        }
        IrExpr::Await(e) => {
            write_expr(out, e, level);
            write!(out, ".await").unwrap();
        }
        IrExpr::Repeat { elem, len } => {
            write!(out, "[").unwrap();
            write_expr(out, elem, level);
            write!(out, "; ").unwrap();
            write_expr(out, len, level);
            write!(out, "]").unwrap();
        }
        IrExpr::Unsafe(block) => {
            writeln!(out, "unsafe {{").unwrap();
            write_block(out, block, level + 1);
            indent(out, level);
            write!(out, "}}").unwrap();
        }
        IrExpr::Let { pattern, expr: e } => {
            write!(out, "let ").unwrap();
            write_pattern(out, pattern);
            write!(out, " = ").unwrap();
            write_expr(out, e, level);
        }
    }
}

fn write_lit(out: &mut String, lit: &IrLit) {
    match lit {
        IrLit::Int(n) => write!(out, "{}", n).unwrap(),
        IrLit::Float(f) => write!(out, "{}", f).unwrap(),
        IrLit::Bool(b) => write!(out, "{}", b).unwrap(),
        IrLit::Char(c) => write!(out, "'{}'", c).unwrap(),
        IrLit::Str(s) => write!(out, "\"{}\"", s).unwrap(),
        IrLit::ByteStr(bs) => write!(out, "b\"{:?}\"", bs).unwrap(),
        IrLit::Byte(b) => write!(out, "b'{}'", *b as char).unwrap(),
    }
}

fn bin_op_str(op: &IrBinOp) -> &'static str {
    match op {
        IrBinOp::Add => "+",
        IrBinOp::Sub => "-",
        IrBinOp::Mul => "*",
        IrBinOp::Div => "/",
        IrBinOp::Rem => "%",
        IrBinOp::BitAnd => "&",
        IrBinOp::BitOr => "|",
        IrBinOp::BitXor => "^",
        IrBinOp::Shl => "<<",
        IrBinOp::Shr => ">>",
        IrBinOp::Eq => "==",
        IrBinOp::Ne => "!=",
        IrBinOp::Lt => "<",
        IrBinOp::Le => "<=",
        IrBinOp::Gt => ">",
        IrBinOp::Ge => ">=",
        IrBinOp::And => "&&",
        IrBinOp::Or => "||",
    }
}

fn unary_op_str(op: &IrUnaryOp) -> &'static str {
    match op {
        IrUnaryOp::Neg => "-",
        IrUnaryOp::Not => "!",
        IrUnaryOp::Deref => "*",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;

    #[test]
    fn test_print_simple_struct() {
        let source = r#"
            pub struct Delta<N: ArrayLength<T>, T> {
                pub delta: GenericArray<T, N>,
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        let printed = print_module(&module);
        
        assert!(printed.contains("pub struct Delta"));
        assert!(printed.contains("pub delta:"));
        println!("{}", printed);
    }

    #[test]
    fn test_print_impl_block() {
        let source = r#"
            impl<N: ArrayLength<T>, T: Clone> Delta<N, T> {
                pub fn clone(&self) -> Self {
                    let Delta { delta } = self;
                    Delta {
                        delta: delta.clone(),
                    }
                }
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        let printed = print_module(&module);
        
        assert!(printed.contains("impl<N:"));
        assert!(printed.contains("fn clone"));
        println!("{}", printed);
    }
}
