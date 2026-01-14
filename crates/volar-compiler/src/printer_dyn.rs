//! Dynamic IR Printer
//!
//! This module transforms the IR to produce dynamic Rust code where:
//! - Type-level length parameters (N: ArrayLength, etc.) become runtime `usize` fields
//! - `GenericArray<T, N>` becomes `Vec<T>`
//! - Struct names get a `Dyn` suffix
//! - Cipher and hash generics (B: BlockCipher, D: Digest) remain as generic type parameters

use core::{fmt::Write, mem::take};
#[cfg(feature = "std")]
use std::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use alloc::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};

use crate::ir::{IrGenericParamKind, *};

use crate::ir::*;

/// Classification of generic parameters
#[derive(Debug, Clone, PartialEq)]
enum GenericKind {
    /// Length/size parameter - becomes runtime usize
    Length,
    /// Crypto trait (BlockCipher, Digest) - remains generic
    Crypto,
    /// Regular type parameter - remains generic  
    Type,
}

/// Analyze a generic parameter to determine its kind
fn classify_generic(param: &IrGenericParam) -> GenericKind {
    // Check bounds for crypto traits
    for bound in &param.bounds {
        match &bound.trait_kind {
            TraitKind::Crypto(CryptoTrait::ArrayLength) | TraitKind::Math(MathTrait::Unsigned) => {
                return GenericKind::Length;
            }
            TraitKind::Crypto(CryptoTrait::BlockCipher)
            | TraitKind::Crypto(CryptoTrait::Digest)
            | TraitKind::Crypto(CryptoTrait::Rng)
            | TraitKind::Crypto(CryptoTrait::ByteBlockEncrypt)
            | TraitKind::Crypto(CryptoTrait::VoleArray) => return GenericKind::Crypto,
            TraitKind::Math(_) => {}
            k => {
                // panic!("Unhandled trait kind in classify_generic: {:?}", k);
            }
        }
    }

    GenericKind::Type
}

// /// Check if a type parameter name represents a length
// fn is_length_type_param(mut name: &str) -> bool {
//     name = name.trim();
//     if name.len() == 1 {
//         let c = name.chars().next().unwrap();
//         return matches!(c, 'N' | 'M' | 'K' | 'L' | 'S' | 'X');
//     }
//     if name.len() == 2
//         && name.chars().next().unwrap().is_ascii_uppercase()
//         && name.chars().nth(1).unwrap().is_ascii_digit()
//     {
//         return true;
//     }
//     false
// }

/// Information about a struct's witness fields
#[derive(Debug, Clone, Default)]
struct StructInfo {
    /// Names of lifetime parameters
    lifetimes: Vec<String>,
    /// Names of length witness fields (lowercase)
    length_witnesses: Vec<String>,
    /// Names of type parameters that remain generic
    type_params: Vec<(String, String)>,
}

/// Main entry point for generating dynamic Rust code
pub fn print_module_rust_dyn(module: &IrModule) -> String {
    // First pass: collect struct information
    let mut struct_info: BTreeMap<String, StructInfo> = BTreeMap::new();

    for s in &module.structs {
        let mut info = StructInfo::default();
        for p in &s.generics {
            if p.kind == IrGenericParamKind::Lifetime {
                info.lifetimes.push(format!("'{}", p.name));
            } else {
                match classify_generic(p) {
                    GenericKind::Length => info.length_witnesses.push(p.name.to_lowercase()),
                    GenericKind::Crypto | GenericKind::Type => info.type_params.push((
                        p.name.clone(),
                        p.bounds
                            .iter()
                            .map(|b| bname(b))
                            .collect::<Vec<_>>()
                            .join(" + "),
                    )),
                }
            }
        }
        struct_info.insert(s.kind.to_string(), info);
    }

    let mut out = String::new();

    // File header
    writeln!(out, "//! Auto-generated dynamic types from volar-spec").unwrap();
    writeln!(
        out,
        "//! Type-level lengths have been converted to runtime usize witnesses"
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(
        out,
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case)]"
    )
    .unwrap();
    writeln!(out).unwrap();

    // Imports
    writeln!(out, "extern crate alloc;").unwrap();
    writeln!(out, "use alloc::vec::Vec;").unwrap();
    writeln!(out, "use alloc::vec;").unwrap();
    writeln!(
        out,
        "use core::ops::{{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor}};"
    )
    .unwrap();
    writeln!(out).unwrap();

    // Re-export primitives
    writeln!(out, "// Primitive field types from volar-primitives").unwrap();
    writeln!(
        out,
        "pub use volar_primitives::{{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64}};"
    )
    .unwrap();
    writeln!(out).unwrap();

    // Helper function
    writeln!(out, "/// Compute integer log2").unwrap();
    writeln!(out, "#[inline]").unwrap();
    writeln!(out, "pub fn ilog2(x: usize) -> u32 {{").unwrap();
    writeln!(out, "    usize::BITS - x.leading_zeros() - 1").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();

    // Generate structs
    for s in &module.structs {
        write_struct_dyn(&mut out, s, &struct_info);
        writeln!(out).unwrap();
    }

    // Generate impls
    for i in &module.impls {
        write_impl_dyn(&mut out, i, &struct_info);
        writeln!(out).unwrap();
    }

    // Generate free functions
    for f in &module.functions {
        write_function_dyn(&mut out, f, 0, None, &BTreeMap::new(), &struct_info);
        writeln!(out).unwrap();
    }

    out
}

fn write_struct_dyn(out: &mut String, s: &IrStruct, struct_info: &BTreeMap<String, StructInfo>) {
    let info = match struct_info.get(&s.kind.to_string()).cloned() {
        Some(v) => v,
        None => {
            return write!(
                out,
                "compile_error!(\"Struct info missing for {}\")",
                s.kind
            )
            .unwrap();
        }
    };
    let mut cur_params = BTreeMap::new();
    for w in info.length_witnesses.iter() {
        cur_params.insert(w.clone(), (format!(""), GenericKind::Length));
    }
    for t in info.type_params.iter() {
        cur_params.insert(t.0.clone(), (format!(""), GenericKind::Type));
    }
    let name = format!("{}Dyn", s.kind);

    // Derive common traits (no Default for structs with references)
    let has_refs = info.lifetimes.len() > 0;
    if has_refs {
        writeln!(out, "#[derive(Clone, Debug)]").unwrap();
    } else {
        writeln!(out, "#[derive(Clone, Debug, Default)]").unwrap();
    }
    write!(out, "pub struct {}", name).unwrap();

    // Generic parameters: lifetimes first, then type params
    let mut all_generics = Vec::new();
    all_generics.extend(info.lifetimes.iter().cloned().map(|a| (a, None)));
    all_generics.extend(info.type_params.iter().cloned().map(|(n, b)| (n, Some(b))));

    if !all_generics.is_empty() {
        write!(
            out,
            "<{}>",
            all_generics
                .iter()
                .map(|(n, b)| {
                    if let Some(bounds) = b {
                        format!("{}: {}", n, bounds)
                    } else {
                        n.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
        .unwrap();
    }

    if s.is_tuple {
        // Tuple struct
        write!(out, "(").unwrap();
        // First: length witnesses
        for (i, _) in info.length_witnesses.iter().enumerate() {
            if i > 0 {
                write!(out, ", ").unwrap();
            }
            write!(out, "pub usize").unwrap();
        }
        // Then: actual fields
        for (i, f) in s.fields.iter().enumerate() {
            if i > 0 || !info.length_witnesses.is_empty() {
                write!(out, ", ").unwrap();
            }
            write!(out, "pub ").unwrap();
            write_type_dyn(out, &f.ty, &cur_params, struct_info);
        }
        writeln!(out, ");").unwrap();
    } else {
        // Named struct
        writeln!(out, " {{").unwrap();

        // Length witness fields first
        for w in &info.length_witnesses {
            writeln!(out, "    pub {}: usize,", w).unwrap();
        }

        // Regular fields
        for f in &s.fields {
            write!(out, "    pub {}: ", f.name).unwrap();
            write_type_dyn(out, &f.ty, &cur_params, struct_info);
            writeln!(out, ",").unwrap();
        }

        writeln!(out, "}}").unwrap();
    }
}
fn bname(b: &IrTraitBound) -> String {
    match &b.trait_kind {
        TraitKind::Crypto(c) => format!("{:?}", c),
        TraitKind::Math(math_trait) => format!("{:?}", math_trait),
        TraitKind::External { path } => format!(
            "compile_error!(\"External trait bounds not supported in dyn code: {:?}\")",
            path
        ),
        TraitKind::Custom(path) => format!(
            "compile_error!(\"External trait bounds not supported in dyn code: {}\")",
            path
        ),
        TraitKind::Into(t) => format!(
            "Into<{}>",
            match &**t {
                IrType::TypeParam(t) => t.clone(),
                a => format!("compile_error!(\"Into bounds not supported in dyn code {a:?}\")"),
            }
        ),
        TraitKind::AsRef(t) => format!(
            "AsRef<{}>",
            match &**t {
                IrType::TypeParam(t) => t.clone(),
                IrType::Array {
                    kind: ArrayKind::Slice,
                    elem,
                    len,
                } => match &**elem {
                    IrType::Primitive(PrimitiveType::U8) => "[u8]".to_string(),
                    a => format!(
                        "compile_error!(\"AsRef bounds with slice elem not supported in dyn code {a:?}\")"
                    ),
                },
                a => format!("compile_error!(\"AsRef bounds not supported in dyn code {a:?}\")"),
            }
        ),
        TraitKind::Expand(t) => format!(
            "FnMut(&[u8]) -> {}",
            match &**t {
                IrType::TypeParam(t) => t.clone(),
                a => format!("compile_error!(\"Expand bounds not supported in dyn code {a:?}\")"),
            }
        ),
    }
}
fn pname(p: &IrGenericParam) -> String {
    match &*p.bounds {
        [] => p.name.clone(),
        x => format!(
            "{}: {}",
            p.name,
            x.iter().map(|b| bname(b)).collect::<Vec<_>>().join(" + ")
        ),
    }
}
fn write_impl_dyn(out: &mut String, i: &IrImpl, struct_info: &BTreeMap<String, StructInfo>) {
    let generics = i
        .generics
        .iter()
        .map(|p| (p.name.clone(), (format!(""), classify_generic(p))))
        .collect::<BTreeMap<_, _>>();
    let (self_name, concrete_type_args) = match &i.self_ty {
        IrType::Struct { kind, type_args } => {
            // Collect concrete types from the impl
            let mut concrete = Vec::new();
            for arg in type_args {
                match arg {
                    IrType::TypeParam(p) => match struct_info.get(&kind.to_string()) {
                        Some(info) => {
                            // Check if this type param is a length witness
                            if info.length_witnesses.contains(&p.to_lowercase()) {
                                // Skip length params
                            } else {
                                concrete.push(p.clone());
                            }
                        }
                        None => {
                            concrete.push(p.clone());
                        }
                    },
                    IrType::Primitive(_) | IrType::Struct { .. } => {
                        let mut s = String::new();
                        write_type_dyn(&mut s, arg, &generics, struct_info);
                        concrete.push(s);
                    }
                    _ => {}
                }
            }
            (kind.to_string(), concrete)
        }
        _ => return,
    };

    let info = struct_info.get(&self_name).cloned().unwrap_or_default();
    let dyn_name = format!("{}Dyn", self_name);

    // Collect type params for this impl (excluding length params)
    let mut impl_type_params = Vec::new();
    let mut cur_params = BTreeMap::new();
    for p in &i.generics {
        let c = classify_generic(p);
        cur_params.insert(p.name.clone(), (format!(""), c.clone()));
        if c != GenericKind::Length && !info.length_witnesses.contains(&p.name.to_lowercase()) {
            impl_type_params.push(pname(p));
        }
    }

    write!(out, "impl").unwrap();
    if !impl_type_params.is_empty() {
        write!(out, "<{}>", impl_type_params.join(", ")).unwrap();
    }

    write!(out, " {}", dyn_name).unwrap();

    // Use concrete types if available, otherwise use type params from struct
    if !concrete_type_args.is_empty() {
        write!(out, "<{}>", concrete_type_args.join(", ")).unwrap();
    } else if !info.type_params.is_empty() {
        write!(
            out,
            "<{}>",
            info.type_params
                .iter()
                .map(|(n, b)| {
                    if !b.is_empty() {
                        format!("{}: {}", n, b)
                    } else {
                        n.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
        .unwrap();
    }

    writeln!(out, " {{").unwrap();

    for item in &i.items {
        if let IrImplItem::Method(f) = item {
            write_function_dyn(out, f, 1, Some(&self_name), &cur_params, struct_info);
        }
    }

    writeln!(out, "}}").unwrap();
}

fn write_function_dyn(
    out: &mut String,
    f: &IrFunction,
    level: usize,
    self_struct: Option<&str>,
    cur_params: &BTreeMap<String, (String, GenericKind)>,
    struct_info: &BTreeMap<String, StructInfo>,
) {
    let indent = "    ".repeat(level);

    // Collect length params that need to be passed as usize arguments
    let mut length_params = Vec::new();
    let mut type_params = Vec::new();
    let mut cur_params = cur_params.clone();

    for p in &f.generics {
        let k = classify_generic(p);
        cur_params.insert(p.name.clone(), (format!(""), k.clone()));
        match k {
            GenericKind::Length => length_params.push(p.name.clone()),
            _ => type_params.push(pname(p)),
        }
    }

    write!(out, "{}pub fn {}", indent, f.name).unwrap();

    // Generic type parameters (non-length)
    if !type_params.is_empty() {
        write!(out, "<{}>", type_params.join(", ")).unwrap();
    }

    write!(out, "(").unwrap();

    let mut param_count = 0;

    // Receiver
    if let Some(r) = f.receiver {
        match r {
            IrReceiver::Value => write!(out, "self").unwrap(),
            IrReceiver::Ref => write!(out, "&self").unwrap(),
            IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
        }
        param_count += 1;
    }

    // Length parameters as usize
    for lp in &length_params {
        if param_count > 0 {
            write!(out, ", ").unwrap();
        }
        write!(out, "{}: usize", lp.to_lowercase()).unwrap();
        param_count += 1;
    }

    // Regular parameters
    for p in &f.params {
        if param_count > 0 {
            write!(out, ", ").unwrap();
        }
        write!(out, "{}: ", p.name).unwrap();
        write_type_dyn(out, &p.ty, &cur_params, struct_info);
        param_count += 1;
    }

    write!(out, ")").unwrap();

    // Return type
    if let Some(ret) = &f.return_type {
        write!(out, " -> ").unwrap();
        write_type_dyn(out, ret, &cur_params, struct_info);
    }

    writeln!(out, " {{").unwrap();

    let body_indent = "    ".repeat(level + 1);

    // Unpack length witnesses from self if this is a method
    if f.receiver.is_some() {
        if let Some(struct_name) = self_struct {
            if let Some(info) = struct_info.get(struct_name) {
                for w in &info.length_witnesses {
                    writeln!(out, "{}let {} = self.{};", body_indent, w, w).unwrap();
                }
            }
        }
    }

    // Function body
    let ctx = ExprContext {
        struct_info,
        has_self: f.receiver.is_some(),
        self_struct,
        cur_params: &cur_params,
    };

    for stmt in &f.body.stmts {
        write_stmt_dyn(out, stmt, level + 1, &ctx);
    }

    if let Some(e) = &f.body.expr {
        write!(out, "{}", body_indent).unwrap();
        write_expr_dyn(out, e, &ctx);
        writeln!(out).unwrap();
    }

    writeln!(out, "{}}}", indent).unwrap();
}

fn write_type_dyn(
    out: &mut String,
    ty: &IrType,
    cur_params: &BTreeMap<String, (String, GenericKind)>,
    struct_info: &BTreeMap<String, StructInfo>,
) -> bool {
    match ty {
        IrType::Primitive(p) => write!(out, "{}", p).unwrap(),

        IrType::Array { elem, .. } | IrType::Vector { elem } => {
            // GenericArray<T, N> -> Vec<T>
            write!(out, "Vec<").unwrap();
            write_type_dyn(out, elem, cur_params, struct_info);
            write!(out, ">").unwrap();
        }

        IrType::Struct { kind, type_args } => {
            // Add Dyn suffix to struct names
            write!(out, "{}Dyn", kind).unwrap();

            // Filter out length type params
            let filtered: Vec<_> = type_args.clone();

            if !filtered.is_empty() {
                write!(out, "<").unwrap();
                let mut skip = true;
                for (i, arg) in filtered.iter().enumerate() {
                    if !take(&mut skip) {
                        write!(out, ", ").unwrap();
                    }
                    if !write_type_dyn(out, arg, &cur_params, struct_info) {
                        skip = true;
                    }
                }
                write!(out, ">").unwrap();
            }
        }

        IrType::Param { path } => {
            let full_path = path.join("::");
            write!(out, "<{} as typenum::Unsigned>::USIZE", full_path).unwrap();
        }

        IrType::TypeParam(p) => {
            if p == "Self" {
                write!(out, "Self").unwrap();
                return true;
            }
            // Length params shouldn't appear in types after transformation
            match struct_info
                .get(p)
                .map(|info| info.length_witnesses.contains(&p.to_lowercase()))
                .or_else(|| cur_params.get(p).map(|a| a.1 == GenericKind::Length))
            {
                Some(false) => {}
                Some(true) => {
                    return false;
                }
                None => {
                    write!(out, "compile_error!(\"Unknown type parameter: {}\")", p).unwrap();
                    return true;
                }
            }
            write!(out, "{}", p).unwrap();
        }

        IrType::Tuple(elems) => {
            write!(out, "(").unwrap();
            let mut skip = true;
            for (i, elem) in elems.iter().enumerate() {
                if take(&mut skip) {
                    write!(out, ", ").unwrap();
                }
                if !write_type_dyn(out, elem, cur_params, struct_info) {
                    skip = true;
                }
            }
            write!(out, ")").unwrap();
        }

        IrType::Unit => write!(out, "()").unwrap(),

        IrType::Reference { mutable, elem, .. } => {
            // For struct fields, we need a lifetime - use 'a as default
            write!(out, "&'a {}", if *mutable { "mut " } else { "" }).unwrap();
            return write_type_dyn(out, elem, cur_params, struct_info);
        }

        ty => write!(out, "compile_error!(\"Unsupported type {ty:?}\")").unwrap(),
    }
    return true;
}

/// Context for expression generation
struct ExprContext<'a> {
    struct_info: &'a BTreeMap<String, StructInfo>,
    cur_params: &'a BTreeMap<String, (String, GenericKind)>,
    has_self: bool,
    self_struct: Option<&'a str>,
}

fn write_stmt_dyn(out: &mut String, stmt: &IrStmt, level: usize, ctx: &ExprContext) {
    let indent = "    ".repeat(level);

    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            write!(out, "{}let ", indent).unwrap();

            // Only add mut for simple ident patterns, not for destructuring
            let needs_mut = match pattern {
                IrPattern::Ident { mutable, .. } => *mutable,
                IrPattern::Wild => false,
                // For struct/tuple patterns, don't add outer mut
                _ => false,
            };

            if needs_mut {
                write!(out, "mut ").unwrap();
            }

            write_pattern_dyn(out, pattern, ctx.cur_params, ctx.struct_info);

            if let Some(t) = ty {
                write!(out, ": ").unwrap();
                write_type_dyn(out, t, ctx.cur_params, ctx.struct_info);
            }

            if let Some(i) = init {
                write!(out, " = ").unwrap();
                write_expr_dyn(out, i, ctx);
            }

            writeln!(out, ";").unwrap();
        }

        IrStmt::Semi(e) => {
            write!(out, "{}", indent).unwrap();
            write_expr_dyn(out, e, ctx);
            writeln!(out, ";").unwrap();
        }

        IrStmt::Expr(e) => {
            write!(out, "{}", indent).unwrap();
            write_expr_dyn(out, e, ctx);
            writeln!(out).unwrap();
        }
    }
}

fn write_expr_dyn(out: &mut String, expr: &IrExpr, ctx: &ExprContext) {
    macro_rules! is_length_type_param {
        ($v:expr) => {
            match $v {
                v => match ctx
                    .struct_info
                    .get(v)
                    .map(|info| info.length_witnesses.contains(&v.to_lowercase()))
                    .or_else(|| {
                        ctx.cur_params
                            .get(v)
                            .map(|(_, k)| *k == GenericKind::Length)
                    }) {
                    Some(v) => v,
                    None => false,
                },
            }
        };
    }
    match expr {
        IrExpr::Lit(l) => write!(out, "{}", l).unwrap(),

        IrExpr::Var(v) => {
            // Length params should be lowercase local variables
            if is_length_type_param!(v) {
                write!(out, "{}", v.to_lowercase()).unwrap();
            } else {
                write!(out, "{}", v).unwrap();
            }
        }

        IrExpr::Binary { op, left, right } => {
            write!(out, "(").unwrap();
            write_expr_dyn(out, left, ctx);
            write!(out, " {} ", bin_op_str(*op)).unwrap();
            write_expr_dyn(out, right, ctx);
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
            write_expr_dyn(out, expr, ctx);
        }

        IrExpr::Call { func, args } => {
            // Check if this is a call to a length param (like N() -> n)
            if let IrExpr::Var(v) = func.as_ref() {
                if is_length_type_param!(v) && args.is_empty() {
                    write!(out, "{}", v.to_lowercase()).unwrap();
                    return;
                }
            }

            // Handle GenericArray::default() -> Vec::new()
            if let IrExpr::Path { segments, .. } = func.as_ref() {
                if let [receiver, path] = &segments[..] {
                    if (path == "default" || path == "new") && args.is_empty() {
                        write!(
                            out,
                            "{}::new()",
                            match &**receiver {
                                "GenericArray" => "Vec",
                                a => a,
                            }
                        )
                        .unwrap();
                        return;
                    }
                    if path == "to_usize" {
                        if !args.is_empty() {
                            write!(
                                out,
                                "compile_error!(\"to_usize should have no arguments (got {})\")",
                                args.len()
                            )
                            .unwrap();
                            return;
                        }
                        // GenericArray::to_usize() -> n

                        write!(out, "{}", receiver.to_lowercase()).unwrap();
                        return;
                    }
                }
                // todo!("Handle other path calls: {segments:?}");
                write!(
                    out,
                    "compile_error!(\"Unhandled path call: {:?}\")",
                    segments
                )
                .unwrap();
                return;
            } else {
                write_expr_dyn(out, func, ctx);
                write!(out, "(").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write_expr_dyn(out, arg, ctx);
                }
                write!(out, ")").unwrap();
            }
        }

        IrExpr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let method_name = match method {
                MethodKind::Std(s) => s.clone(),
                MethodKind::Vole(v) => match v {
                    VoleMethod::Remap => "remap".to_string(),
                    VoleMethod::RotateLeft => "rotate_left".to_string(),
                },
                MethodKind::Crypto(c) => format!("{:?}", c).to_lowercase(),
                MethodKind::Unknown(s) => s.clone(),
            };

            // Special case: to_usize() on a type becomes just the variable
            if method_name == "to_usize" {
                // Check if receiver is a path to a length type param
                match receiver.as_ref() {
                    IrExpr::Var(v) if is_length_type_param!(v) => {
                        write!(out, "{}", v.to_lowercase()).unwrap();
                        return;
                    }
                    _ => {
                        write!(out, "<").unwrap();
                        write_expr_dyn(out, receiver, ctx);
                        write!(out, " as typenum::Unsigned>::USIZE").unwrap();
                        return;
                    }
                }
            }

            // Handle ilog2 on type params: N::to_usize().ilog2() -> ilog2(n)
            if method_name == "ilog2" {
                write!(out, "ilog2(").unwrap();
                write_expr_dyn(out, receiver, ctx);
                write!(out, ")").unwrap();
                return;
            }

            write_expr_dyn(out, receiver, ctx);
            write!(out, ".{}(", method_name).unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr_dyn(out, arg, ctx);
            }
            write!(out, ")").unwrap();
        }

        IrExpr::Field { base, field } => {
            write_expr_dyn(out, base, ctx);
            write!(out, ".{}", field).unwrap();
        }

        IrExpr::Index { base, index } => {
            write_expr_dyn(out, base, ctx);
            write!(out, "[").unwrap();
            write_expr_dyn(out, index, ctx);
            write!(out, "]").unwrap();
        }

        IrExpr::Block(b) => {
            writeln!(out, "{{").unwrap();
            for stmt in &b.stmts {
                write_stmt_dyn(out, stmt, 1, ctx);
            }
            if let Some(e) = &b.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, ctx);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
        }

        IrExpr::ArrayGenerate {
            index_var,
            body,
            len,
            ..
        } => {
            let len_str = match len {
                ArrayLength::Const(n) => n.to_string(),
                ArrayLength::TypeNum(tn) => tn.to_usize().to_string(),
                ArrayLength::TypeParam(p) => p.to_lowercase(),
                ArrayLength::Computed(e) => {
                    let mut s = String::new();
                    write_expr_dyn(&mut s, e, ctx);
                    s
                }
            };
            write!(out, "(0..{}).map(|{}| ", len_str, index_var).unwrap();
            write_expr_dyn(out, body, ctx);
            write!(out, ").collect()").unwrap();
        }

        IrExpr::ArrayMap {
            array,
            elem_var,
            body,
        } => {
            write_expr_dyn(out, array, ctx);
            write!(out, ".iter().map(|{}| ", elem_var).unwrap();
            write_expr_dyn(out, body, ctx);
            write!(out, ").collect()").unwrap();
        }

        IrExpr::ArrayZip {
            left,
            right,
            left_var,
            right_var,
            body,
        } => {
            write_expr_dyn(out, left, ctx);
            write!(out, ".iter().zip(").unwrap();
            write_expr_dyn(out, right, ctx);
            write!(out, ".iter()).map(|({}, {})| ", left_var, right_var).unwrap();
            write_expr_dyn(out, body, ctx);
            write!(out, ").collect()").unwrap();
        }

        IrExpr::ArrayFold {
            array,
            init,
            acc_var,
            elem_var,
            body,
        } => {
            write_expr_dyn(out, array, ctx);
            write!(out, ".iter().fold(").unwrap();
            write_expr_dyn(out, init, ctx);
            write!(out, ", |{}, {}| ", acc_var, elem_var).unwrap();
            write_expr_dyn(out, body, ctx);
            write!(out, ")").unwrap();
        }

        IrExpr::BoundedLoop {
            var,
            start,
            end,
            inclusive,
            body,
        } => {
            write!(out, "for {} in ", var).unwrap();
            write_expr_dyn(out, start, ctx);
            write!(out, "{}", if *inclusive { "..=" } else { ".." }).unwrap();
            write_expr_dyn(out, end, ctx);
            writeln!(out, " {{").unwrap();
            for stmt in &body.stmts {
                write_stmt_dyn(out, stmt, 1, ctx);
            }
            if let Some(e) = &body.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, ctx);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
        }

        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            write!(out, "if ").unwrap();
            write_expr_dyn(out, cond, ctx);
            writeln!(out, " {{").unwrap();
            for stmt in &then_branch.stmts {
                write_stmt_dyn(out, stmt, 1, ctx);
            }
            if let Some(e) = &then_branch.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, ctx);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
            if let Some(eb) = else_branch {
                write!(out, " else ").unwrap();
                write_expr_dyn(out, eb, ctx);
            }
        }

        IrExpr::Match { expr, arms } => {
            write!(out, "match ").unwrap();
            write_expr_dyn(out, expr, ctx);
            writeln!(out, " {{").unwrap();
            for arm in arms {
                write!(out, "    ").unwrap();
                write_pattern_dyn(out, &arm.pattern, ctx.cur_params, ctx.struct_info);
                write!(out, " => ").unwrap();
                write_expr_dyn(out, &arm.body, ctx);
                writeln!(out, ",").unwrap();
            }
            write!(out, "}}").unwrap();
        }

        IrExpr::Return(e) => {
            write!(out, "return").unwrap();
            if let Some(e) = e {
                write!(out, " ").unwrap();
                write_expr_dyn(out, e, ctx);
            }
        }

        IrExpr::Break(e) => {
            write!(out, "break").unwrap();
            if let Some(e) = e {
                write!(out, " ").unwrap();
                write_expr_dyn(out, e, ctx);
            }
        }

        IrExpr::Continue => write!(out, "continue").unwrap(),

        IrExpr::Assign { left, right } => {
            write_expr_dyn(out, left, ctx);
            write!(out, " = ").unwrap();
            write_expr_dyn(out, right, ctx);
        }

        IrExpr::AssignOp { op, left, right } => {
            write_expr_dyn(out, left, ctx);
            write!(out, " {}= ", bin_op_str(*op)).unwrap();
            write_expr_dyn(out, right, ctx);
        }

        IrExpr::Path { segments, .. } => {
            // Handle GenericArray paths
            if segments.len() >= 1 && segments[0] == "GenericArray" {
                if segments.len() == 2 && segments[1] == "default" {
                    write!(out, "Vec::new").unwrap();
                    return;
                } else if segments.len() == 2 && segments[1] == "generate" {
                    // Will be handled in Call
                    write!(out, "todo_generate").unwrap();
                    return;
                }
            }

            // Handle paths like N::to_usize() -> n
            if segments.len() >= 1 && is_length_type_param!(&segments[0]) {
                if segments.len() == 1 {
                    write!(out, "{}", segments[0].to_lowercase()).unwrap();
                    return;
                }
                if segments.len() == 2 && segments[1] == "to_usize" {
                    write!(out, "{}", segments[0].to_lowercase()).unwrap();
                    return;
                }
            }

            // Handle associated constants like B::BlockSize, D::OutputSize
            if segments.len() == 2 {
                let type_name = &segments[0];
                let assoc = &segments[1];
                if assoc == "BlockSize" || assoc == "OutputSize" {
                    // These are typically cipher/hash sizes - emit as runtime lookup
                    write!(out, "{}::{}", type_name, assoc).unwrap();
                    return;
                }
            }

            write!(out, "{}", segments.join("::")).unwrap();
        }

        IrExpr::Closure { params, body, .. } => {
            write!(out, "|").unwrap();
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern_dyn(out, &p.pattern, ctx.cur_params, ctx.struct_info);
            }
            write!(out, "| ").unwrap();
            write_expr_dyn(out, body, ctx);
        }

        IrExpr::Cast { expr, ty } => {
            // Add parens around casts for operator precedence safety
            write!(out, "(").unwrap();
            write_expr_dyn(out, expr, ctx);
            write!(out, " as ").unwrap();
            write_type_dyn(out, ty, ctx.cur_params, ctx.struct_info);
            write!(out, ")").unwrap();
        }

        IrExpr::Try(e) => {
            write_expr_dyn(out, e, ctx);
            write!(out, "?").unwrap();
        }

        IrExpr::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr_dyn(out, e, ctx);
            }
            write!(out, ")").unwrap();
        }

        IrExpr::Array(elems) => {
            write!(out, "vec![").unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_expr_dyn(out, e, ctx);
            }
            write!(out, "]").unwrap();
        }

        IrExpr::StructExpr { kind, fields, .. } => {
            write!(out, "{}Dyn {{ ", kind).unwrap();

            // Write explicit fields
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: ", name).unwrap();
                write_expr_dyn(out, val, ctx);
            }

            // Add length witnesses if needed
            if let Some(info) = ctx.struct_info.get(&kind.to_string()) {
                for (i, w) in info.length_witnesses.iter().enumerate() {
                    if !fields.is_empty() || i > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write!(out, "{}: {}", w, w).unwrap();
                }
            }

            write!(out, " }}").unwrap();
        }
        IrExpr::IterLoop {
            pattern,
            collection,
            body,
        } => {
            write!(out, "for ").unwrap();
            write_pattern_dyn(out, pattern, ctx.cur_params, ctx.struct_info);
            write!(out, " in ").unwrap();
            write_expr_dyn(out, collection, ctx);
            writeln!(out, " {{").unwrap();
            for stmt in &body.stmts {
                write_stmt_dyn(out, stmt, 1, ctx);
            }
            if let Some(e) = &body.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, ctx);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
        }

        e => write!(out, "compile_error!(\"Unsupported expression {e:?}\")").unwrap(),
    }
}

fn write_pattern_dyn(
    out: &mut String,
    pat: &IrPattern,
    cur_params: &BTreeMap<String, (String, GenericKind)>,
    struct_info: &BTreeMap<String, StructInfo>,
) {
    match pat {
        IrPattern::Ident { name, .. } => write!(out, "{}", name).unwrap(),
        IrPattern::Wild => write!(out, "_").unwrap(),
        IrPattern::Tuple(pats) => {
            write!(out, "(").unwrap();
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern_dyn(out, p, cur_params, struct_info);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::TupleStruct { kind, elems } => {
            // Don't add Dyn suffix to primitive wrapper types
            let kind_str = kind.to_string();
            // Handle Self pattern specially - it will be transformed at a higher level
            if kind_str == "Self" {
                write!(out, "Self(").unwrap();
            } else if matches!(
                kind_str.as_str(),
                "BitsInBytes"
                    | "BitsInBytes64"
                    | "Bit"
                    | "Galois"
                    | "Galois64"
                    | "Some"
                    | "None"
                    | "Ok"
                    | "Err"
            ) {
                write!(out, "{}(", kind).unwrap();
            } else {
                write!(out, "{}Dyn(", kind).unwrap();
            }
            for (i, p) in elems.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern_dyn(out, p, cur_params, struct_info);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::Struct { kind, fields, rest } => {
            // Don't add Dyn suffix to primitive wrapper types or Self
            let kind_str = kind.to_string();
            if kind_str == "Self" {
                write!(out, "Self {{ ").unwrap();
            } else if matches!(
                kind_str.as_str(),
                "BitsInBytes" | "BitsInBytes64" | "Bit" | "Galois" | "Galois64"
            ) {
                write!(out, "{} {{ ", kind).unwrap();
            } else {
                write!(out, "{}Dyn {{ ", kind).unwrap();
            }
            for (i, (name, p)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: ", name).unwrap();
                write_pattern_dyn(out, p, cur_params, struct_info);
            }
            // Always add .. for Dyn structs since they have extra witness fields
            if *rest
                || (kind_str != "Self"
                    && !matches!(
                        kind_str.as_str(),
                        "BitsInBytes" | "BitsInBytes64" | "Bit" | "Galois" | "Galois64"
                    ))
            {
                if !fields.is_empty() {
                    write!(out, ", ").unwrap();
                }
                write!(out, "..").unwrap();
            }
            write!(out, " }}").unwrap();
        }
        IrPattern::Ref { mutable, pat } => {
            write!(out, "&{}", if *mutable { "mut " } else { "" }).unwrap();
            write_pattern_dyn(out, pat, cur_params, struct_info);
        }
        IrPattern::Slice(pats) => {
            write!(out, "[").unwrap();
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write_pattern_dyn(out, p, cur_params, struct_info);
            }
            write!(out, "]").unwrap();
        }
        IrPattern::Lit(l) => write!(out, "{}", l).unwrap(),
        IrPattern::Or(pats) => {
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    write!(out, " | ").unwrap();
                }
                write_pattern_dyn(out, p, cur_params, struct_info);
            }
        }
        IrPattern::Rest => write!(out, "..").unwrap(),
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
