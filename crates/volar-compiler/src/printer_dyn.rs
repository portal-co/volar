use core::fmt::Write;
#[cfg(feature = "std")]
use std::{string::{String, ToString}, vec::Vec, format};

#[cfg(not(feature = "std"))]
use alloc::{string::{String, ToString}, vec::Vec, format};

use crate::ir::*;

pub fn print_module_rust_dyn(module: &IrModule) -> String {
    let mut out = String::new();
    writeln!(out, "use alloc::vec::Vec;").unwrap();
    writeln!(out, "use core::ops::{{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Neg, Not}};").unwrap();
    writeln!(out).unwrap();

    for s in &module.structs {
        write_struct_dyn(&mut out, s);
        writeln!(out).unwrap();
    }

    for i in &module.impls {
        write_impl_dyn(&mut out, i);
        writeln!(out).unwrap();
    }

    for f in &module.functions {
        write_function_dyn(&mut out, f, 0);
        writeln!(out).unwrap();
    }

    out
}

fn write_struct_dyn(out: &mut String, s: &IrStruct) {
    let name = format!("{}Dyn", s.kind);
    write!(out, "pub struct {}", name).unwrap();
    
    // Filter out array length generics and replace with T
    let mut generics = Vec::new();
    let mut has_t = false;
    for p in &s.generics {
        if p.kind == IrGenericParamKind::Type {
            if p.name == "T" {
                has_t = true;
            }
            generics.push(p.name.clone());
        }
    }
    if !has_t {
        // Many structs in volar use T for elements
    }

    if !generics.is_empty() {
        write!(out, "<{}>", generics.join(", ")).unwrap();
    }
    
    if s.is_tuple {
        write!(out, "(").unwrap();
        for (i, f) in s.fields.iter().enumerate() {
            if i > 0 { write!(out, ", ").unwrap(); }
            write_type_dyn(out, &f.ty);
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, " {{").unwrap();
        for f in &s.fields {
            write!(out, "    pub {}: ", f.name).unwrap();
            write_type_dyn(out, &f.ty);
            writeln!(out, ",").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }
}

fn write_impl_dyn(out: &mut String, i: &IrImpl) {
    let self_name = match &i.self_ty {
        IrType::Struct { kind, .. } => format!("{}Dyn", kind),
        _ => return,
    };

    write!(out, "impl").unwrap();
    let mut generics = Vec::new();
    for p in &i.generics {
         if p.kind == IrGenericParamKind::Type {
             generics.push(p.name.clone());
         }
    }
    if !generics.is_empty() {
        write!(out, "<{}>", generics.join(", ")).unwrap();
    }

    write!(out, " {}", self_name).unwrap();
    if !generics.is_empty() {
        write!(out, "<{}>", generics.join(", ")).unwrap();
    }

    writeln!(out, " {{").unwrap();
    for item in &i.items {
        match item {
            IrImplItem::Method(f) => write_function_dyn(out, f, 1),
            _ => {}
        }
    }
    writeln!(out, "}}").unwrap();
}

fn write_function_dyn(out: &mut String, f: &IrFunction, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}pub fn {}(", indent, f.name).unwrap();
    if let Some(r) = f.receiver {
        match r {
            IrReceiver::Value => write!(out, "self").unwrap(),
            IrReceiver::Ref => write!(out, "&self").unwrap(),
            IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
        }
        if !f.params.is_empty() { write!(out, ", ").unwrap(); }
    }
    for (i, p) in f.params.iter().enumerate() {
        if i > 0 { write!(out, ", ").unwrap(); }
        write!(out, "{}: ", p.name).unwrap();
        write_type_dyn(out, &p.ty);
    }
    write!(out, ")").unwrap();
    if let Some(ret) = &f.return_type {
        write!(out, " -> ").unwrap();
        write_type_dyn(out, ret);
    }
    writeln!(out).unwrap();
    write_block_dyn(out, &f.body, level);
    writeln!(out).unwrap();
}

fn write_type_dyn(out: &mut String, ty: &IrType) {
    match ty {
        IrType::Primitive(p) => write!(out, "{}", p).unwrap(),
        IrType::Array { elem, .. } => {
            write!(out, "Vec<").unwrap();
            write_type_dyn(out, elem);
            write!(out, ">").unwrap();
        }
        IrType::Struct { kind, type_args } => {
            write!(out, "{}Dyn", kind).unwrap();
            if !type_args.is_empty() {
                write!(out, "<").unwrap();
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write_type_dyn(out, arg);
                }
                write!(out, ">").unwrap();
            }
        }
        IrType::TypeParam(p) => write!(out, "{}", p).unwrap(),
        IrType::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_type_dyn(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrType::Unit => write!(out, "()").unwrap(),
        IrType::Reference { mutable, elem } => {
            write!(out, "&{}", if *mutable { "mut " } else { "" }).unwrap();
            write_type_dyn(out, elem);
        }
        _ => write!(out, "_").unwrap(),
    }
}

fn write_block_dyn(out: &mut String, block: &IrBlock, level: usize) {
    let indent = "    ".repeat(level);
    writeln!(out, "{}{{", indent).unwrap();
    for stmt in &block.stmts {
        write_stmt_dyn(out, stmt, level + 1);
    }
    if let Some(e) = &block.expr {
        write!(out, "{}    ", indent).unwrap();
        write_expr_dyn(out, e);
        writeln!(out).unwrap();
    }
    write!(out, "{}}}", indent).unwrap();
}

fn write_stmt_dyn(out: &mut String, stmt: &IrStmt, level: usize) {
    let indent = "    ".repeat(level);
    write!(out, "{}", indent).unwrap();
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            write!(out, "let ").unwrap();
            write_pattern_dyn(out, pattern);
            if let Some(t) = ty {
                write!(out, ": ").unwrap();
                write_type_dyn(out, t);
            }
            if let Some(i) = init {
                write!(out, " = ").unwrap();
                write_expr_dyn(out, i);
            }
            writeln!(out, ";").unwrap();
        }
        IrStmt::Semi(e) => {
            write_expr_dyn(out, e);
            writeln!(out, ";").unwrap();
        }
        IrStmt::Expr(e) => {
            write_expr_dyn(out, e);
            writeln!(out).unwrap();
        }
    }
}

fn write_expr_dyn(out: &mut String, expr: &IrExpr) {
    match expr {
        IrExpr::Lit(l) => write!(out, "{}", l).unwrap(),
        IrExpr::Var(v) => write!(out, "{}", v).unwrap(),
        IrExpr::Binary { op, left, right } => {
            write!(out, "(").unwrap();
            write_expr_dyn(out, left);
            write!(out, " {} ", match op {
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
                _ => "???",
            }).unwrap();
            write_expr_dyn(out, right);
            write!(out, ")").unwrap();
        }
        IrExpr::Unary { op, expr } => {
            match op {
                SpecUnaryOp::Neg => write!(out, "-").unwrap(),
                SpecUnaryOp::Not => write!(out, "!").unwrap(),
                SpecUnaryOp::Deref => write!(out, "*").unwrap(),
                SpecUnaryOp::Ref => write!(out, "&").unwrap(),
                SpecUnaryOp::RefMut => write!(out, "&mut ").unwrap(),
            }
            write_expr_dyn(out, expr);
        }
        IrExpr::Call { func, args } => {
            write_expr_dyn(out, func);
            write!(out, "(").unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr_dyn(out, arg);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::MethodCall { receiver, method, args, .. } => {
            write_expr_dyn(out, receiver);
            let method_name = match method {
                MethodKind::Std(s) => s.clone(),
                MethodKind::Vole(v) => match v {
                     VoleMethod::Remap => "remap".to_string(),
                     VoleMethod::RotateLeft => "rotate_left".to_string(),
                },
                MethodKind::Crypto(c) => format!("{:?}", c).to_lowercase(),
                MethodKind::Unknown(s) => s.clone(),
            };
            write!(out, ".{}(", method_name).unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr_dyn(out, arg);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Field { base, field } => {
            write_expr_dyn(out, base);
            write!(out, ".{}", field).unwrap();
        }
        IrExpr::Index { base, index } => {
            write_expr_dyn(out, base);
            write!(out, "[").unwrap();
            write_expr_dyn(out, index);
            write!(out, "]").unwrap();
        }
        IrExpr::Block(b) => write_block_dyn(out, b, 0),
        IrExpr::ArrayGenerate { index_var, body, .. } => {
            write!(out, "(0..n).map(|{}| ", index_var).unwrap();
            write_expr_dyn(out, body);
            write!(out, ").collect()").unwrap();
        }
        IrExpr::ArrayMap { array, elem_var, body } => {
            write_expr_dyn(out, array);
            write!(out, ".iter().map(|{}| ", elem_var).unwrap();
            write_expr_dyn(out, body);
            write!(out, ").collect()").unwrap();
        }
        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            write!(out, "for {} in ", var).unwrap();
            write_expr_dyn(out, start);
            write!(out, "{} ", if *inclusive { "..=" } else { ".." }).unwrap();
            write_expr_dyn(out, end);
            write_block_dyn(out, body, 0);
        }
        _ => write!(out, "todo!()").unwrap(),
    }
}

fn write_pattern_dyn(out: &mut String, pat: &IrPattern) {
    match pat {
        IrPattern::Ident { name, .. } => write!(out, "{}", name).unwrap(),
        IrPattern::Wild => write!(out, "_").unwrap(),
        _ => write!(out, "todo!()").unwrap(),
    }
}
