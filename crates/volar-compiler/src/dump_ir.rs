//! Human-readable IR dump for debugging.
//!
//! Produces a concise text representation of `IrModule` that is easy to scan
//! in a terminal or feed to an AI for analysis.  The format is intentionally
//! compact: one line per simple expression, indented blocks for nesting.
//!
//! Usage:
//!   `dump_module(&module)` → `String`

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

use core::fmt::Write;

use crate::ir::*;

/// Dump an entire module to a human-readable string.
pub fn dump_module(module: &IrModule) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "=== Module: {} ===", module.name);
    let _ = writeln!(out);

    // Structs
    for s in &module.structs {
        dump_struct(&mut out, s, 0);
        let _ = writeln!(out);
    }

    // Traits
    for t in &module.traits {
        dump_trait(&mut out, t, 0);
        let _ = writeln!(out);
    }

    // Impls
    for imp in &module.impls {
        dump_impl(&mut out, imp, 0);
        let _ = writeln!(out);
    }

    // Free functions
    for func in &module.functions {
        dump_function(&mut out, func, 0);
        let _ = writeln!(out);
    }

    // Type aliases
    for alias in &module.type_aliases {
        let _ = writeln!(
            out,
            "type {}{} = {}",
            alias.name,
            fmt_generics(&alias.generics),
            alias.target
        );
    }

    out
}

// ── helpers ─────────────────────────────────────────────────────────────────

fn indent(out: &mut String, level: usize) {
    for _ in 0..level {
        out.push_str("  ");
    }
}

fn fmt_generics(generics: &[IrGenericParam]) -> String {
    if generics.is_empty() {
        return String::new();
    }
    let parts: Vec<String> = generics
        .iter()
        .map(|g| {
            let mut s = g.name.clone();
            if !g.bounds.is_empty() {
                s.push_str(": ");
                let bounds: Vec<String> = g.bounds.iter().map(|b| format!("{:?}", b)).collect();
                s.push_str(&bounds.join(" + "));
            }
            s
        })
        .collect();
    format!("<{}>", parts.join(", "))
}

fn fmt_params(params: &[IrParam]) -> String {
    params
        .iter()
        .map(|p| format!("{}: {}", p.name, p.ty))
        .collect::<Vec<_>>()
        .join(", ")
}

fn fmt_type(ty: &IrType) -> String {
    format!("{}", ty)
}

fn fmt_receiver(recv: &Option<IrReceiver>) -> &'static str {
    match recv {
        Some(IrReceiver::Ref) => "&self, ",
        Some(IrReceiver::RefMut) => "&mut self, ",
        Some(IrReceiver::Value) => "self, ",
        None => "",
    }
}

// ── struct ──────────────────────────────────────────────────────────────────

fn dump_struct(out: &mut String, s: &IrStruct, level: usize) {
    indent(out, level);
    let _ = writeln!(out, "struct {}{} {{", s.kind, fmt_generics(&s.generics));
    for f in &s.fields {
        indent(out, level + 1);
        let _ = writeln!(out, "{}: {},", f.name, f.ty);
    }
    indent(out, level);
    let _ = writeln!(out, "}}");
}

// ── trait ───────────────────────────────────────────────────────────────────

fn dump_trait(out: &mut String, t: &IrTrait, level: usize) {
    indent(out, level);
    let _ = writeln!(out, "trait {:?}{} {{", t.kind, fmt_generics(&t.generics));
    for item in &t.items {
        match item {
            IrTraitItem::Method(sig) => {
                indent(out, level + 1);
                let ret = sig
                    .return_type
                    .as_ref()
                    .map(|t| format!(" -> {}", t))
                    .unwrap_or_default();
                let _ = writeln!(
                    out,
                    "fn {}{}({}{}){};",
                    sig.name,
                    fmt_generics(&sig.generics),
                    fmt_receiver(&sig.receiver),
                    fmt_params(&sig.params),
                    ret,
                );
            }
            IrTraitItem::AssociatedType { name, bounds, .. } => {
                indent(out, level + 1);
                if bounds.is_empty() {
                    let _ = writeln!(out, "type {};", name);
                } else {
                    let _ = writeln!(out, "type {}: ...;", name);
                }
            }
        }
    }
    indent(out, level);
    let _ = writeln!(out, "}}");
}

// ── impl ────────────────────────────────────────────────────────────────────

fn dump_impl(out: &mut String, imp: &IrImpl, level: usize) {
    indent(out, level);
    let trait_str = imp
        .trait_
        .as_ref()
        .map(|t| format!("{:?} for ", t.kind))
        .unwrap_or_default();
    let _ = writeln!(
        out,
        "impl{} {}{} {{",
        fmt_generics(&imp.generics),
        trait_str,
        imp.self_ty,
    );
    for item in &imp.items {
        match item {
            IrImplItem::Method(func) => {
                dump_function(out, func, level + 1);
            }
            IrImplItem::AssociatedType { name, ty } => {
                indent(out, level + 1);
                let _ = writeln!(out, "type {:?} = {};", name, ty);
            }
        }
    }
    indent(out, level);
    let _ = writeln!(out, "}}");
}

// ── function ────────────────────────────────────────────────────────────────

fn dump_function(out: &mut String, func: &IrFunction, level: usize) {
    indent(out, level);
    let ret = func
        .return_type
        .as_ref()
        .map(|t| format!(" -> {}", t))
        .unwrap_or_default();
    let _ = writeln!(
        out,
        "fn {}{}({}{}){} {{",
        func.name,
        fmt_generics(&func.generics),
        fmt_receiver(&func.receiver),
        fmt_params(&func.params),
        ret,
    );
    dump_block(out, &func.body, level + 1);
    indent(out, level);
    let _ = writeln!(out, "}}");
}

// ── block / statements ──────────────────────────────────────────────────────

fn dump_block(out: &mut String, block: &IrBlock, level: usize) {
    for stmt in &block.stmts {
        dump_stmt(out, stmt, level);
    }
    if let Some(expr) = &block.expr {
        indent(out, level);
        let _ = write!(out, "=> ");
        dump_expr(out, expr, level);
        let _ = writeln!(out);
    }
}

fn dump_stmt(out: &mut String, stmt: &IrStmt, level: usize) {
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            indent(out, level);
            let _ = write!(out, "let {}", fmt_pattern(pattern));
            if let Some(t) = ty {
                let _ = write!(out, ": {}", t);
            }
            if let Some(e) = init {
                let _ = write!(out, " = ");
                dump_expr(out, e, level);
            }
            let _ = writeln!(out, ";");
        }
        IrStmt::Semi(expr) => {
            indent(out, level);
            dump_expr(out, expr, level);
            let _ = writeln!(out, ";");
        }
        IrStmt::Expr(expr) => {
            indent(out, level);
            dump_expr(out, expr, level);
            let _ = writeln!(out);
        }
    }
}

// ── pattern ─────────────────────────────────────────────────────────────────

fn fmt_pattern(pat: &IrPattern) -> String {
    match pat {
        IrPattern::Ident {
            name,
            mutable,
            subpat,
        } => {
            let mut s = String::new();
            if *mutable {
                s.push_str("mut ");
            }
            s.push_str(name);
            if let Some(sub) = subpat {
                s.push_str(" @ ");
                s.push_str(&fmt_pattern(sub));
            }
            s
        }
        IrPattern::Wild => "_".to_string(),
        IrPattern::Tuple(pats) => {
            let inner: Vec<String> = pats.iter().map(fmt_pattern).collect();
            format!("({})", inner.join(", "))
        }
        IrPattern::TupleStruct { kind, elems } => {
            let inner: Vec<String> = elems.iter().map(fmt_pattern).collect();
            format!("{}({})", kind, inner.join(", "))
        }
        IrPattern::Struct { kind, fields, rest } => {
            let fs: Vec<String> = fields
                .iter()
                .map(|(fname, fpat)| {
                    let p = fmt_pattern(fpat);
                    if p == *fname {
                        fname.clone()
                    } else {
                        format!("{}: {}", fname, p)
                    }
                })
                .collect();
            let dots = if *rest { ", .." } else { "" };
            format!("{} {{ {}{} }}", kind, fs.join(", "), dots)
        }
        IrPattern::Lit(lit) => format!("{}", lit),
        IrPattern::Ref { mutable, pat } => {
            if *mutable {
                format!("&mut {}", fmt_pattern(pat))
            } else {
                format!("&{}", fmt_pattern(pat))
            }
        }
        IrPattern::Or(pats) => {
            let inner: Vec<String> = pats.iter().map(fmt_pattern).collect();
            inner.join(" | ")
        }
        IrPattern::Slice(pats) => {
            let inner: Vec<String> = pats.iter().map(fmt_pattern).collect();
            format!("[{}]", inner.join(", "))
        }
        IrPattern::Rest => "..".to_string(),
    }
}

// ── expressions ─────────────────────────────────────────────────────────────

fn dump_expr(out: &mut String, expr: &IrExpr, level: usize) {
    match expr {
        IrExpr::Lit(lit) => {
            let _ = write!(out, "{}", lit);
        }
        IrExpr::Var(name) => {
            let _ = write!(out, "{}", name);
        }
        IrExpr::Binary { op, left, right } => {
            let _ = write!(out, "(");
            dump_expr(out, left, level);
            let _ = write!(out, " {:?} ", op);
            dump_expr(out, right, level);
            let _ = write!(out, ")");
        }
        IrExpr::Unary { op, expr: inner } => {
            let _ = write!(out, "{:?}", op);
            dump_expr(out, inner, level);
        }
        IrExpr::Call { func, args } => {
            dump_expr(out, func, level);
            let _ = write!(out, "(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                dump_expr(out, arg, level);
            }
            let _ = write!(out, ")");
        }
        IrExpr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            dump_expr(out, receiver, level);
            let _ = write!(out, ".{:?}(", method);
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                dump_expr(out, arg, level);
            }
            let _ = write!(out, ")");
        }
        IrExpr::Field { base, field } => {
            dump_expr(out, base, level);
            let _ = write!(out, ".{}", field);
        }
        IrExpr::Index { base, index } => {
            dump_expr(out, base, level);
            let _ = write!(out, "[");
            dump_expr(out, index, level);
            let _ = write!(out, "]");
        }
        IrExpr::Block(block) => {
            let _ = writeln!(out, "{{");
            dump_block(out, block, level + 1);
            indent(out, level);
            let _ = write!(out, "}}");
        }
        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let _ = write!(out, "if ");
            dump_expr(out, cond, level);
            let _ = writeln!(out, " {{");
            dump_block(out, &then_branch, level + 1);
            indent(out, level);
            if let Some(else_b) = else_branch {
                let _ = write!(out, "}} else ");
                dump_expr(out, else_b, level);
            } else {
                let _ = write!(out, "}}");
            }
        }
        IrExpr::Return(inner) => {
            let _ = write!(out, "return");
            if let Some(v) = inner {
                let _ = write!(out, " ");
                dump_expr(out, v, level);
            }
        }
        IrExpr::Assign { left, right } => {
            dump_expr(out, left, level);
            let _ = write!(out, " = ");
            dump_expr(out, right, level);
        }
        IrExpr::AssignOp { op, left, right } => {
            dump_expr(out, left, level);
            let _ = write!(out, " {:?}= ", op);
            dump_expr(out, right, level);
        }
        IrExpr::Range {
            start,
            end,
            inclusive,
        } => {
            if let Some(s) = start {
                dump_expr(out, s, level);
            }
            let _ = write!(out, "{}", if *inclusive { "..=" } else { ".." });
            if let Some(e) = end {
                dump_expr(out, e, level);
            }
        }
        IrExpr::Cast { expr: inner, ty } => {
            dump_expr(out, inner, level);
            let _ = write!(out, " as {}", ty);
        }
        IrExpr::Closure {
            params,
            body,
            ret_type,
        } => {
            let ps: Vec<String> = params
                .iter()
                .map(|p| {
                    let pat_str = fmt_pattern(&p.pattern);
                    if let Some(t) = &p.ty {
                        format!("{}: {}", pat_str, t)
                    } else {
                        pat_str
                    }
                })
                .collect();
            let _ = write!(out, "|{}|", ps.join(", "));
            if let Some(rt) = ret_type {
                let _ = write!(out, " -> {} ", rt);
            } else {
                let _ = write!(out, " ");
            }
            dump_expr(out, body, level);
        }
        IrExpr::StructExpr {
            kind,
            type_args,
            fields,
            rest,
        } => {
            let _ = write!(out, "{}", kind);
            if !type_args.is_empty() {
                let args: Vec<String> = type_args.iter().map(|t| format!("{}", t)).collect();
                let _ = write!(out, "::<{}>", args.join(", "));
            }
            let _ = write!(out, " {{ ");
            for (i, (fname, fval)) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                let _ = write!(out, "{}: ", fname);
                dump_expr(out, fval, level);
            }
            if let Some(r) = rest {
                let _ = write!(out, ", ..");
                dump_expr(out, r, level);
            }
            let _ = write!(out, " }}");
        }
        IrExpr::Tuple(elems) => {
            let _ = write!(out, "(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                dump_expr(out, e, level);
            }
            let _ = write!(out, ")");
        }
        IrExpr::Path {
            segments,
            type_args,
        } => {
            let _ = write!(out, "{}", segments.join("::"));
            if !type_args.is_empty() {
                let args: Vec<String> = type_args.iter().map(|t| format!("{}", t)).collect();
                let _ = write!(out, "::<{}>", args.join(", "));
            }
        }
        IrExpr::BoundedLoop {
            var,
            start,
            end,
            inclusive,
            body,
        } => {
            let _ = write!(out, "for {} in ", var);
            dump_expr(out, start, level);
            let _ = write!(out, "{}", if *inclusive { "..=" } else { ".." });
            dump_expr(out, end, level);
            let _ = writeln!(out, " {{");
            dump_block(out, body, level + 1);
            indent(out, level);
            let _ = write!(out, "}}");
        }
        IrExpr::IterPipeline(chain) => {
            dump_iter_chain(out, chain, level);
        }
        IrExpr::Match {
            expr: scrutinee,
            arms,
        } => {
            let _ = write!(out, "match ");
            dump_expr(out, scrutinee, level);
            let _ = writeln!(out, " {{");
            for arm in arms {
                indent(out, level + 1);
                let _ = write!(out, "{}", fmt_pattern(&arm.pattern));
                if let Some(g) = &arm.guard {
                    let _ = write!(out, " if ");
                    dump_expr(out, g, level + 1);
                }
                let _ = write!(out, " => ");
                dump_expr(out, &arm.body, level + 1);
                let _ = writeln!(out, ",");
            }
            indent(out, level);
            let _ = write!(out, "}}");
        }
        IrExpr::DefaultValue { ty } => match ty {
            Some(t) => {
                let _ = write!(out, "DefaultValue({})", t);
            }
            None => {
                let _ = write!(out, "DefaultValue(?)");
            }
        },
        IrExpr::LengthOf(len) => {
            let _ = write!(out, "LengthOf({:?})", len);
        }
        IrExpr::ArrayGenerate {
            elem_ty,
            len,
            index_var,
            body,
        } => {
            let ty_str = elem_ty
                .as_ref()
                .map(|t| format!("<{}>", t))
                .unwrap_or_default();
            let _ = write!(out, "ArrayGenerate{}[", ty_str);
            let _ = write!(out, "{:?}; |{}| ", len, index_var);
            dump_expr(out, body, level);
            let _ = write!(out, "]");
        }
        IrExpr::TypenumUsize { ty } => {
            let _ = write!(out, "TypenumUsize({})", ty);
        }
        IrExpr::Unreachable => {
            let _ = write!(out, "unreachable!()");
        }
        IrExpr::Try(inner) => {
            dump_expr(out, inner, level);
            let _ = write!(out, "?");
        }
        IrExpr::Array(elems) => {
            let _ = write!(out, "[");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                dump_expr(out, e, level);
            }
            let _ = write!(out, "]");
        }
        IrExpr::Repeat { elem, len } => {
            let _ = write!(out, "[");
            dump_expr(out, elem, level);
            let _ = write!(out, "; ");
            dump_expr(out, len, level);
            let _ = write!(out, "]");
        }
        IrExpr::RawMap {
            receiver,
            elem_var,
            body,
        } => {
            dump_expr(out, receiver, level);
            let _ = write!(out, ".raw_map(|{}| ", fmt_pattern(elem_var));
            dump_expr(out, body, level);
            let _ = write!(out, ")");
        }
        IrExpr::RawZip {
            left,
            right,
            left_var,
            right_var,
            body,
        } => {
            dump_expr(out, left, level);
            let _ = write!(out, ".raw_zip(");
            dump_expr(out, right, level);
            let _ = write!(
                out,
                ", |{}, {}| ",
                fmt_pattern(left_var),
                fmt_pattern(right_var)
            );
            dump_expr(out, body, level);
            let _ = write!(out, ")");
        }
        IrExpr::RawFold {
            receiver,
            init,
            acc_var,
            elem_var,
            body,
        } => {
            dump_expr(out, receiver, level);
            let _ = write!(out, ".raw_fold(");
            dump_expr(out, init, level);
            let _ = write!(
                out,
                ", |{}, {}| ",
                fmt_pattern(acc_var),
                fmt_pattern(elem_var)
            );
            dump_expr(out, body, level);
            let _ = write!(out, ")");
        }
        IrExpr::IterLoop {
            pattern,
            collection,
            body,
        } => {
            let _ = write!(out, "for {} in ", fmt_pattern(pattern));
            dump_expr(out, collection, level);
            let _ = writeln!(out, " {{");
            dump_block(out, body, level + 1);
            indent(out, level);
            let _ = write!(out, "}}");
        }
        IrExpr::Break(val) => {
            let _ = write!(out, "break");
            if let Some(v) = val {
                let _ = write!(out, " ");
                dump_expr(out, v, level);
            }
        }
        IrExpr::Continue => {
            let _ = write!(out, "continue");
        }
    }
}

// ── iter chain ──────────────────────────────────────────────────────────────

fn dump_iter_chain(out: &mut String, chain: &IrIterChain, level: usize) {
    // Source
    match &chain.source {
        IterChainSource::Method { collection, method } => {
            dump_expr(out, collection, level);
            let _ = write!(out, ".{:?}()", method);
        }
        IterChainSource::Range {
            start,
            end,
            inclusive,
        } => {
            let _ = write!(out, "(");
            dump_expr(out, start, level);
            let _ = write!(out, "{}", if *inclusive { "..=" } else { ".." });
            dump_expr(out, end, level);
            let _ = write!(out, ")");
        }
        IterChainSource::Zip { left, right } => {
            dump_iter_chain(out, left, level);
            let _ = write!(out, ".zip(");
            dump_iter_chain(out, right, level);
            let _ = write!(out, ")");
        }
    }

    // Steps
    for step in &chain.steps {
        match step {
            IterStep::Map { var, body } => {
                let _ = write!(out, ".map(|{}| ", fmt_pattern(var));
                dump_expr(out, body, level);
                let _ = write!(out, ")");
            }
            IterStep::Filter { var, body } => {
                let _ = write!(out, ".filter(|{}| ", fmt_pattern(var));
                dump_expr(out, body, level);
                let _ = write!(out, ")");
            }
            IterStep::FilterMap { var, body } => {
                let _ = write!(out, ".filter_map(|{}| ", fmt_pattern(var));
                dump_expr(out, body, level);
                let _ = write!(out, ")");
            }
            IterStep::FlatMap { var, body } => {
                let _ = write!(out, ".flat_map(|{}| ", fmt_pattern(var));
                dump_expr(out, body, level);
                let _ = write!(out, ")");
            }
            IterStep::Enumerate => {
                let _ = write!(out, ".enumerate()");
            }
            IterStep::Take { count } => {
                let _ = write!(out, ".take(");
                dump_expr(out, count, level);
                let _ = write!(out, ")");
            }
            IterStep::Skip { count } => {
                let _ = write!(out, ".skip(");
                dump_expr(out, count, level);
                let _ = write!(out, ")");
            }
            IterStep::Chain { other } => {
                let _ = write!(out, ".chain(");
                dump_iter_chain(out, other, level);
                let _ = write!(out, ")");
            }
        }
    }

    // Terminal
    match &chain.terminal {
        IterTerminal::Collect => {
            let _ = write!(out, ".collect()");
        }
        IterTerminal::CollectTyped(ty) => {
            let _ = write!(out, ".collect::<Vec<{}>>() ", ty);
        }
        IterTerminal::Fold {
            init,
            acc_var,
            elem_var,
            body,
        } => {
            let _ = write!(out, ".fold(");
            dump_expr(out, init, level);
            let _ = write!(
                out,
                ", |{}, {}| ",
                fmt_pattern(acc_var),
                fmt_pattern(elem_var)
            );
            dump_expr(out, body, level);
            let _ = write!(out, ")");
        }
        IterTerminal::Lazy => {
            let _ = write!(out, " /* lazy */");
        }
    }
}
