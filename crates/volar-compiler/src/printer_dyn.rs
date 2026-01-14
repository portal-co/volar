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
fn classify_generic(param: &IrGenericParam, all_params: &[&[IrGenericParam]]) -> GenericKind {
    // Helper: check whether a given `IrType` refers (directly or indirectly) to a length-type parameter.
    fn type_refers_to_length(
        ty: &IrType,
        all_params: &[&[IrGenericParam]],
        visited: &mut Vec<String>,
    ) -> bool {
        match ty {
            IrType::TypeParam(name) => is_length_name(name, all_params, visited),
            IrType::Struct { type_args, .. } => {
                for ta in type_args {
                    if type_refers_to_length(ta, all_params, visited) {
                        return true;
                    }
                }
                false
            }
            IrType::Projection { base, assoc: _ } => {
                // For projections like `<B as _>::BlockSize`, check the base type
                type_refers_to_length(base, all_params, visited)
            }
            IrType::Param { path } => {
                // For paths like `N::Something`, treat the first segment as a type param name
                if let Some(first) = path.first() {
                    return is_length_name(first, all_params, visited);
                }
                false
            }
            IrType::Array { elem, .. }
            | IrType::Vector { elem }
            | IrType::Reference { elem, .. } => type_refers_to_length(elem, all_params, visited),
            IrType::Tuple(elems) => elems
                .iter()
                .any(|e| type_refers_to_length(e, all_params, visited)),
            _ => false,
        }
    }

    // Helper: find a generic param definition by name across the provided generic sets.
    fn find_param<'a>(
        name: &str,
        all_params: &'a [&'a [IrGenericParam]],
    ) -> Option<&'a IrGenericParam> {
        for set in all_params {
            for p in *set {
                if p.name == name {
                    return Some(p);
                }
            }
        }
        None
    }

    // Recursive search by param name to determine if it is a length. Uses `visited` to avoid cycles.
    fn is_length_name(
        name: &str,
        all_params: &[&[IrGenericParam]],
        visited: &mut Vec<String>,
    ) -> bool {
        if visited.contains(&name.to_string()) {
            return false;
        }
        visited.push(name.to_string());

        if let Some(p) = find_param(name, all_params) {
            for bound in &p.bounds {
                match &bound.trait_kind {
                    TraitKind::Crypto(CryptoTrait::ArrayLength)
                    | TraitKind::Math(MathTrait::Unsigned) => {
                        return true;
                    }
                    TraitKind::Math(m)
                        if matches!(
                            m,
                            MathTrait::Add | MathTrait::Sub | MathTrait::Mul | MathTrait::Div
                        ) =>
                    {
                        // If any type-arg or associated binding refers to a length, treat this as length
                        for arg in &bound.type_args {
                            if type_refers_to_length(arg, all_params, visited) {
                                return true;
                            }
                        }
                        for (_n, ty) in &bound.assoc_bindings {
                            if type_refers_to_length(ty, all_params, visited) {
                                return true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        false
    }

    // First pass: check this param's own bounds for explicit indications
    for bound in &param.bounds {
        match &bound.trait_kind {
            TraitKind::Crypto(CryptoTrait::ArrayLength) | TraitKind::Math(MathTrait::Unsigned) => {
                return GenericKind::Length;
            }
            TraitKind::Crypto(CryptoTrait::BlockCipher)
            | TraitKind::Crypto(CryptoTrait::Digest)
            | TraitKind::Crypto(CryptoTrait::Rng)
            | TraitKind::Crypto(CryptoTrait::ByteBlockEncrypt) => return GenericKind::Crypto,
            // VoleArray behaves like a length (wraps ArrayLength)
            TraitKind::Crypto(CryptoTrait::VoleArray) => return GenericKind::Length,
            TraitKind::Math(m)
                if matches!(
                    m,
                    MathTrait::Add | MathTrait::Sub | MathTrait::Mul | MathTrait::Div
                ) =>
            {
                // If any referenced type in the bound refers to a length, classify as Length
                let mut visited = Vec::new();
                for arg in &bound.type_args {
                    if type_refers_to_length(arg, all_params, &mut visited) {
                        return GenericKind::Length;
                    }
                }
                for (_n, ty) in &bound.assoc_bindings {
                    if type_refers_to_length(ty, all_params, &mut visited) {
                        return GenericKind::Length;
                    }
                }
            }
            _ => {}
        }
    }

    // Final pass: recursively search any param this param might alias to (by name)
    let mut visited = Vec::new();
    if is_length_name(&param.name, all_params, &mut visited) {
        return GenericKind::Length;
    }

    GenericKind::Type
}

fn is_length_param(name: &str, all_params: &[IrGenericParam]) -> bool {
    // Check if it's already explicitly marked as length
    for p in all_params {
        if p.name == name {
            for bound in &p.bounds {
                if matches!(
                    bound.trait_kind,
                    TraitKind::Crypto(CryptoTrait::ArrayLength)
                        | TraitKind::Math(MathTrait::Unsigned)
                ) {
                    return true;
                }
            }
        }
    }

    false
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
    /// Original generics in declaration order with their kind (Length/Crypto/Type) and param kind
    orig_generics: Vec<(String, GenericKind, IrGenericParamKind)>,
    /// Whether a manual `Clone` impl exists for this struct
    manual_clone: bool,
}

// Crypto-detection helpers: these are used to decide when associated numeric
// witnesses should be emitted (only at witness-access sites or explicit
// crypto-method calls).
fn expr_uses_crypto(e: &IrExpr) -> bool {
    match e {
        IrExpr::MethodCall { method, .. } => matches!(method, MethodKind::Crypto(_)),
        IrExpr::Path { segments, .. } => {
            if segments.len() == 2 {
                let assoc = &segments[1];
                return assoc == "BlockSize" || assoc == "OutputSize";
            }
            false
        }
        IrExpr::Call { func, args } => {
            if let IrExpr::Path { segments, .. } = func.as_ref() {
                let joined = segments.join("::").to_lowercase();
                if joined.contains("digest")
                    || joined.contains("commit")
                    || joined.contains("encrypt")
                {
                    return true;
                }
            }
            args.iter().any(|a| expr_uses_crypto(a))
        }
        IrExpr::Block(b) => {
            b.stmts.iter().any(|s| stmt_uses_crypto(s))
                || b.expr.as_ref().map_or(false, |e| expr_uses_crypto(e))
        }
        IrExpr::ArrayGenerate { body, .. } => expr_uses_crypto(body),
        IrExpr::ArrayMap { body, .. } => expr_uses_crypto(body),
        IrExpr::ArrayZip { body, .. } => expr_uses_crypto(body),
        IrExpr::ArrayFold { init, body, .. } => expr_uses_crypto(init) || expr_uses_crypto(body),
        IrExpr::Unary { expr, .. } => expr_uses_crypto(expr),
        IrExpr::Binary { left, right, .. } => expr_uses_crypto(left) || expr_uses_crypto(right),
        IrExpr::If {
            then_branch,
            else_branch,
            ..
        } => {
            then_branch.stmts.iter().any(|s| stmt_uses_crypto(s))
                || then_branch
                    .expr
                    .as_ref()
                    .map_or(false, |e| expr_uses_crypto(e))
                || else_branch.as_ref().map_or(false, |e| expr_uses_crypto(e))
        }
        IrExpr::Match { arms, expr, .. } => {
            expr_uses_crypto(expr) || arms.iter().any(|a| expr_uses_crypto(&a.body))
        }
        IrExpr::Closure { body, .. } => expr_uses_crypto(body),
        _ => false,
    }
}

fn stmt_uses_crypto(s: &IrStmt) -> bool {
    match s {
        IrStmt::Let { init, .. } => init.as_ref().map_or(false, |e| expr_uses_crypto(e)),
        IrStmt::Semi(e) => expr_uses_crypto(e),
        IrStmt::Expr(e) => expr_uses_crypto(e),
    }
}

/// Main entry point for generating dynamic Rust code
pub fn print_module_rust_dyn(module: &IrModule) -> String {
    // Note: crypto-detection helpers are defined at module level (see below)

    // First pass: collect struct information

    // First pass: collect struct information
    let mut struct_info: BTreeMap<String, StructInfo> = BTreeMap::new();

    for s in &module.structs {
        let mut info = StructInfo::default();
        // Build a local cur_params map for this struct so bounds can be
        // formatted in the context of the struct's own generics.
        let mut cur_for_struct: BTreeMap<String, (String, GenericKind)> = BTreeMap::new();
        for p in &s.generics {
            let kind = if p.kind == IrGenericParamKind::Lifetime {
                // lifetimes are represented separately
                GenericKind::Type
            } else {
                classify_generic(p, &[&s.generics])
            };
            // record original generic ordering and kinds
            info.orig_generics
                .push((p.name.clone(), kind.clone(), p.kind.clone()));

            if p.kind == IrGenericParamKind::Lifetime {
                info.lifetimes.push(format!("'{}", p.name));
                // also insert into cur map so bounds can reference lifetimes
                cur_for_struct.insert(p.name.clone(), (format!(""), kind.clone()));
            } else {
                // Insert a placeholder so bname/pname can inspect kinds
                cur_for_struct.insert(p.name.clone(), (format!(""), kind.clone()));
                match kind {
                    GenericKind::Length => info.length_witnesses.push(p.name.to_lowercase()),
                    GenericKind::Crypto | GenericKind::Type => {
                        info.type_params.push((
                            p.name.clone(),
                            p.bounds
                                .iter()
                                .map(|b| bname(b, &cur_for_struct, &struct_info))
                                .collect::<Vec<_>>()
                                .join(" + "),
                        ));
                    }
                }
            }
        }
        struct_info.insert(s.kind.to_string(), info);
    }

    // Mark structs that have manual impls for common derived traits (Clone)
    for im in &module.impls {
        if let IrType::Struct { kind, .. } = &im.self_ty {
            if let Some(tr) = &im.trait_ {
                if format!("{}", tr.kind) == "Clone" {
                    if let Some(entry) = struct_info.get_mut(&kind.to_string()) {
                        entry.manual_clone = true;
                    }
                }
            }
        }
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
        "use core::ops::{{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr}};"
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
        write_function_dyn(&mut out, f, 0, None, &BTreeMap::new(), &struct_info, None);
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

    // Derive common traits (no Default for structs with references).
    // Avoid deriving `Clone` when a manual `Clone` impl exists for the struct.
    let has_refs = info.lifetimes.len() > 0;
    let mut derives: Vec<&str> = Vec::new();
    if !info.manual_clone {
        derives.push("Clone");
    }
    derives.push("Debug");
    if !has_refs {
        derives.push("Default");
    }
    writeln!(out, "#[derive({})]", derives.join(", ")).unwrap();
    write!(out, "pub struct {}", name).unwrap();

    // Generic parameters: lifetimes first, then type params
    let mut all_generics = Vec::new();
    all_generics.extend(info.lifetimes.iter().cloned().map(|a| (a, None)));
    all_generics.extend(info.type_params.iter().cloned().map(|(n, b)| {
        if b.is_empty() {
            (n, None)
        } else {
            (n, Some(b))
        }
    }));

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
fn bname(
    b: &IrTraitBound,
    cur_params: &BTreeMap<String, (String, GenericKind)>,
    struct_info: &BTreeMap<String, StructInfo>,
) -> String {
    // Helper to format an `IrType` using the existing `write_type_dyn` codepath.
    fn type_to_string(
        ty: &IrType,
        cur_params: &BTreeMap<String, (String, GenericKind)>,
        struct_info: &BTreeMap<String, StructInfo>,
    ) -> String {
        let mut s = String::new();
        write_type_dyn(&mut s, ty, cur_params, struct_info);
        s
    }

    // Helper to append type args and associated bindings to a base trait name.
    fn with_args_and_assoc(
        base: String,
        b: &IrTraitBound,
        cur_params: &BTreeMap<String, (String, GenericKind)>,
        struct_info: &BTreeMap<String, StructInfo>,
    ) -> String {
        let mut parts: Vec<String> = Vec::new();
        for ta in &b.type_args {
            parts.push(type_to_string(ta, cur_params, struct_info));
        }
        for (name, ty) in &b.assoc_bindings {
            let s = type_to_string(ty, cur_params, struct_info);
            // Decide if this associated binding is a numeric witness. If so, only
            // emit it when the trait is a cryptographic trait (we need the
            // numeric witness only at witness-access sites for crypto).
            let is_numeric = s.contains("USIZE") || s.chars().all(|c| c.is_ascii_digit());
            let base_is_crypto = matches!(&b.trait_kind, TraitKind::Crypto(_));
            if is_numeric && !base_is_crypto {
                continue;
            }
            parts.push(format!("{:?} = {}", name, s));
        }
        if parts.is_empty() {
            base
        } else {
            format!("{}<{}>", base, parts.join(", "))
        }
    }

    match &b.trait_kind {
        TraitKind::Crypto(c) => with_args_and_assoc(format!("{:?}", c), b, cur_params, struct_info),
        TraitKind::Math(math_trait) => {
            with_args_and_assoc(format!("{:?}", math_trait), b, cur_params, struct_info)
        }
        TraitKind::External { path } => format!(
            "compile_error!(\"External trait bounds not supported in dyn code: {:?}\")",
            path
        ),
        TraitKind::Custom(path) => format!(
            "compile_error!(\"External trait bounds not supported in dyn code: {}\")",
            path
        ),
        TraitKind::Into(t) => {
            return format!("Into<{}>", type_to_string(&**t, cur_params, struct_info));
        }
        TraitKind::AsRef(t) => {
            return format!("AsRef<{}>", type_to_string(&**t, cur_params, struct_info));
        }
        TraitKind::Fn(i, t) => {
            return format!(
                "FnMut({}) -> {}",
                match i {
                    FnInput::BytesSlice => "&[u8]",
                    FnInput::Size => "usize",
                },
                type_to_string(&**t, cur_params, struct_info)
            );
        }
        }
}
fn pname(
    p: &IrGenericParam,
    cur_params: &BTreeMap<String, (String, GenericKind)>,
    struct_info: &BTreeMap<String, StructInfo>,
) -> String {
    match &*p.bounds {
        [] => p.name.clone(),
        x => format!(
            "{}: {}",
            p.name,
            x.iter()
                .map(|b| bname(b, cur_params, struct_info))
                .collect::<Vec<_>>()
                .join(" + ")
        ),
    }
    }
fn write_impl_dyn(out: &mut String, i: &IrImpl, struct_info: &BTreeMap<String, StructInfo>) {
    let generics = i
        .generics
        .iter()
        .map(|p| {
            (
                p.name.clone(),
                (format!(""), classify_generic(p, &[&i.generics])),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let (self_name, concrete_type_args) = match &i.self_ty {
        IrType::Struct { kind, type_args } => {
            // Collect concrete types from the impl, aligning with the struct's original generics
            let info_local = struct_info
                .get(&kind.to_string())
                .cloned()
                .unwrap_or_default();
            let mut concrete = Vec::new();
            for (idx, arg) in type_args.iter().enumerate() {
                if let Some((_, gkind, param_kind)) = info_local.orig_generics.get(idx) {
                    if *param_kind == IrGenericParamKind::Lifetime {
                        continue;
                    }
                    if *gkind == GenericKind::Length {
                        continue;
                    }
                }
                let mut s = String::new();
                if write_type_dyn(&mut s, arg, &generics, struct_info) {
                    concrete.push(s);
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
    // Map of param -> collected bounds (including declared bounds and where-clause bounds)
    let mut collected_bounds: BTreeMap<String, Vec<String>> = BTreeMap::new();

    // First pass: populate `cur_params` with all generics so bounds may
    // reference each other when formatting (avoids Unknown type parameter).
    for p in &i.generics {
        let c = classify_generic(p, &[&i.generics]);
        cur_params.insert(p.name.clone(), (format!(""), c.clone()));
        if c != GenericKind::Length && !info.length_witnesses.contains(&p.name.to_lowercase()) {
            collected_bounds.insert(p.name.clone(), Vec::new());
        }
    }

    // Second pass: format declared bounds into `collected_bounds` using the
    // fully-populated `cur_params` so bounds that reference other generics
    // (e.g., `T: Add<U>`) are printed correctly. Bounds that reference other
    // generics will instead be emitted in the `where` clause so the other
    // generic becomes constrained by a predicate and is allowed on impl.
    let mut where_parts: Vec<String> = Vec::new();
    // helper: check whether a type refers to some generic other than `self_name`
    fn type_refers_to_other(
        ty: &IrType,
        cur_params: &BTreeMap<String, (String, GenericKind)>,
        self_name: &str,
    ) -> bool {
        match ty {
            IrType::TypeParam(n) => n != self_name && cur_params.contains_key(n),
            IrType::Struct { type_args, .. } => type_args
                .iter()
                .any(|ta| type_refers_to_other(ta, cur_params, self_name)),
            IrType::Projection { base, assoc: _ } => {
                type_refers_to_other(base, cur_params, self_name)
            }
            IrType::Param { path } => path
                .first()
                .map(|s| s != self_name && cur_params.contains_key(s))
                .unwrap_or(false),
            IrType::Array { elem, .. }
            | IrType::Vector { elem }
            | IrType::Reference { elem, .. } => type_refers_to_other(elem, cur_params, self_name),
            IrType::Tuple(elems) => elems
                .iter()
                .any(|e| type_refers_to_other(e, cur_params, self_name)),
            _ => false,
        }
    }

    for p in &i.generics {
        if let Some(vec) = collected_bounds.get_mut(&p.name) {
            for b in &p.bounds {
                vec.push(bname(b, &cur_params, struct_info));
            }
        }
    }

    // Merge where-clause bounds: these may add bounds to existing type params.
    for wp in &i.where_clause {
        match wp {
            IrWherePredicate::TypeBound { ty, bounds } => {
                if let IrType::TypeParam(name) = ty {
                    // Skip length type params
                    if cur_params
                        .get(name)
                        .map(|(_, k)| *k == GenericKind::Length)
                        .unwrap_or(false)
                    {
                        continue;
                    }
                    if let Some(vec) = collected_bounds.get_mut(name) {
                        for b in bounds {
                            vec.push(bname(b, &cur_params, struct_info));
                        }
                    }
                }
                // Other predicate targets (Struct, Projection, etc.) are ignored for now
            }
        }
    }

    // Build impl_type_params strings in the original generic order
    for p in &i.generics {
        if let Some(bounds) = collected_bounds.get(&p.name) {
            if !bounds.is_empty() {
                impl_type_params.push(format!("{}: {}", p.name, bounds.join(" + ")));
            } else {
                // include bare param name
                impl_type_params.push(p.name.clone());
            }
        }
    }

    write!(out, "impl").unwrap();
    if !impl_type_params.is_empty() {
        write!(out, "<{}>", impl_type_params.join(", ")).unwrap();
    }

    // If this impl references a trait, emit a trait impl header: `impl<...> Trait<Args> for Type<...>`
    if let Some(tr) = &i.trait_ {
        // format trait name and type args
        let mut trait_name = format!("{}", tr.kind);
        if !tr.type_args.is_empty() {
            let mut ta_parts: Vec<String> = Vec::new();
            for ta in &tr.type_args {
                let mut s = String::new();
                write_type_dyn(&mut s, ta, &cur_params, struct_info);
                ta_parts.push(s);
            }
            trait_name = format!("{}<{}>", trait_name, ta_parts.join(", "));
        }

        write!(out, " {} for {}", trait_name, dyn_name).unwrap();

        // append self-type generic args
        if !concrete_type_args.is_empty() && concrete_type_args.len() == info.type_params.len() {
            // Only emit concrete type args when they match the declared dyn type parameter arity
            write!(out, "<{}>", concrete_type_args.join(", ")).unwrap();
        } else if !info.type_params.is_empty() {
            write!(out, "<{}>",
                info.type_params
                    .iter()
                    .map(|(n, b)| if !b.is_empty() { format!("{}: {}", n, b) } else { n.clone() })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .unwrap();
        }
    } else {
        // inherent impl (no trait)
        write!(out, " {}", dyn_name).unwrap();
        if !concrete_type_args.is_empty() {
            write!(out, "<{}>", concrete_type_args.join(", ")).unwrap();
        } else if !info.type_params.is_empty() {
            write!(out, "<{}>",
                info.type_params
                    .iter()
                    .map(|(n, b)| if !b.is_empty() { format!("{}: {}", n, b) } else { n.clone() })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .unwrap();
        }
    }

    // Emit a `where` clause derived from the original impl where-predicates
    // (still emit even if we merged some bounds above). This helps preserve
    // more complex predicates (projections, external targets) that can't be
    // folded into individual generic bounds.
    for wp in &i.where_clause {
        match wp {
            IrWherePredicate::TypeBound { ty, bounds } => {
                // Skip length-type params in where clause emission
                if let IrType::TypeParam(name) = ty {
                    if cur_params
                        .get(name)
                        .map(|(_, k)| *k == GenericKind::Length)
                        .unwrap_or(false)
                    {
                        continue;
                    }
                }
                // Format the left-hand type
                let mut lhs = String::new();
                write_type_dyn(&mut lhs, ty, &cur_params, struct_info);
                // Format bounds
                let bstr = bounds
                    .iter()
                    .map(|b| bname(b, &cur_params, struct_info))
                    .collect::<Vec<_>>()
                    .join(" + ");
                where_parts.push(format!("{}: {}", lhs, bstr));
            }
        }
    }

    // If there are any where predicates, emit them before the impl body.
    if !where_parts.is_empty() {
        write!(out, " where {}", where_parts.join(", ")).unwrap();
    }

    // Detect unconstrained type generics (no collected bounds) and annotate
    // the generated output with a comment containing the generated file line
    // number so the user can inspect it.
    let mut unconstrained: Vec<String> = Vec::new();
    for p in &i.generics {
        if let Some(vec) = collected_bounds.get(&p.name) {
            // Exclude length params and lifetimes
            if vec.is_empty() {
                if cur_params
                    .get(&p.name)
                    .map(|(_, k)| *k != GenericKind::Length)
                    .unwrap_or(false)
                {
                    unconstrained.push(p.name.clone());
                }
            }
        }
    }
    if !unconstrained.is_empty() {
        // compute current line number in generated output (1-based)
        let current_line = out.lines().count() + 1;
        writeln!(
            out,
            "// UNCONSTRAINED GENERICS at generated.rs line {}: {}",
            current_line,
            unconstrained.join(", ")
        )
        .unwrap();
    }

    writeln!(out, " {{").unwrap();

    for item in &i.items {
        match item {
            IrImplItem::Method(f) => {
                write_function_dyn(out, f, 1, Some(&self_name), &cur_params, struct_info, i.trait_.as_ref());
            }
            IrImplItem::AssociatedType { name, ty } => {
                // emit associated type binding inside trait impls
                let an = match name {
                    AssociatedType::Output => "Output".to_string(),
                    AssociatedType::Key => "Key".to_string(),
                    AssociatedType::BlockSize => "BlockSize".to_string(),
                    AssociatedType::OutputSize => "OutputSize".to_string(),
                    AssociatedType::TotalLoopCount => "TotalLoopCount".to_string(),
                    AssociatedType::Other(s) => s.clone(),
                };
                write!(out, "    type {} = ", an).unwrap();
                write_type_dyn(out, ty, &cur_params, struct_info);
                writeln!(out, ";").unwrap();
            }
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
    trait_ref: Option<&IrTraitRef>,
) {
    let indent = "    ".repeat(level);

    // Collect length params that need to be passed as usize arguments
    let mut length_params = Vec::new();
    let mut type_params = Vec::new();
    let mut cur_params = cur_params.clone();

    for p in &f.generics {
        let k = classify_generic(p, &[&f.generics]);
        cur_params.insert(p.name.clone(), (format!(""), k.clone()));
        match k {
            GenericKind::Length => length_params.push(p.name.clone()),
            _ => type_params.push(pname(p, &cur_params, struct_info)),
        }
    }

    // Trait impl methods are not `pub` in impl blocks.
    if trait_ref.is_some() {
        write!(out, "{}fn {}", indent, f.name).unwrap();
    } else {
        write!(out, "{}pub fn {}", indent, f.name).unwrap();
    }

    // Generic type parameters (non-length)
    if !type_params.is_empty() {
        write!(out, "<{}>", type_params.join(", ")).unwrap();
    }

    // Prepare to merge where-clause bounds into function-level type param bounds.
    // We collect updated `type_params`, `where_parts`, and unconstrained markers
    // now, but defer emitting the `where` clause until after parameters and
    // return type have been written (to produce valid Rust signatures).
    let mut deferred_where_parts: Vec<String> = Vec::new();
    let mut deferred_unconstrained: Vec<String> = Vec::new();
    if !f.where_clause.is_empty() {
        // Build map of typeparam -> bounds
        let mut fn_bounds: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for tp in &f.generics {
            let k = classify_generic(tp, &[&f.generics]);
            if k != GenericKind::Length {
                fn_bounds.insert(
                    tp.name.clone(),
                    tp.bounds
                        .iter()
                        .map(|b| bname(b, &cur_params, struct_info))
                        .collect(),
                );
            }
        }
        for wp in &f.where_clause {
            if let IrWherePredicate::TypeBound { ty, bounds } = wp {
                if let IrType::TypeParam(name) = ty {
                    if cur_params
                        .get(name)
                        .map(|(_, k)| *k == GenericKind::Length)
                        .unwrap_or(false)
                    {
                        continue;
                    }
                    if let Some(vec) = fn_bounds.get_mut(name) {
                        for b in bounds {
                            vec.push(bname(b, &cur_params, struct_info));
                        }
                    }
                }
            }
        }
        // Rebuild type_params list replacing entries with bounds from fn_bounds
        type_params = f
            .generics
            .iter()
            .filter_map(|tp| {
                if let Some(bounds) = fn_bounds.get(&tp.name) {
                    if !bounds.is_empty() {
                        Some(format!("{}: {}", tp.name, bounds.join(" + ")))
                    } else {
                        Some(tp.name.clone())
                    }
                } else {
                    None
                }
            })
            .collect();

        // Build deferred where parts
        for wp in &f.where_clause {
            if let IrWherePredicate::TypeBound { ty, bounds } = wp {
                // Skip length params
                if let IrType::TypeParam(name) = ty {
                    if cur_params
                        .get(name)
                        .map(|(_, k)| *k == GenericKind::Length)
                        .unwrap_or(false)
                    {
                        continue;
                    }
                }
                let mut lhs = String::new();
                write_type_dyn(&mut lhs, ty, &cur_params, struct_info);
                let bstr = bounds
                    .iter()
                    .map(|b| bname(b, &cur_params, struct_info))
                    .collect::<Vec<_>>()
                    .join(" + ");
                deferred_where_parts.push(format!("{}: {}", lhs, bstr));
            }
        }

        // Detect unconstrained generics at function level and record them for
        // deferred emission alongside the where-clause.
        for tp in &f.generics {
            let k = classify_generic(tp, &[&f.generics]);
            if k != GenericKind::Length {
                let existing = fn_bounds
                    .get(&tp.name)
                    .map(|v| v.is_empty())
                    .unwrap_or(true);
                if existing {
                    deferred_unconstrained.push(tp.name.clone());
                }
            }
        }
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

    // Return type: if this is an implementation of an operator trait that
    // has an associated `Output` type, emit `Self::Output` instead of the
    // concrete `OutputDyn` type to use the trait-associated output.
    if let Some(ret) = &f.return_type {
        let mut emitted = false;
        if let Some(tr) = trait_ref {
            if let TraitKind::Math(mt) = &tr.kind {
                match mt {
                    MathTrait::Add
                    | MathTrait::Mul
                    | MathTrait::Div
                    | MathTrait::Rem
                    | MathTrait::BitAnd
                    | MathTrait::BitOr
                    | MathTrait::BitXor
                    | MathTrait::Shl
                    | MathTrait::Shr => {
                        write!(out, " -> Self::Output").unwrap();
                        emitted = true;
                    }
                    _ => {}
                }
            }
        }
        if !emitted {
            write!(out, " -> ").unwrap();
            write_type_dyn(out, ret, &cur_params, struct_info);
        }
    }

    // Emit any deferred where-clause parts collected earlier (after params/return)
    if !deferred_where_parts.is_empty() {
        write!(out, " where {}", deferred_where_parts.join(", ")).unwrap();
    }
    // Emit deferred unconstrained generics annotation if present
    if !deferred_unconstrained.is_empty() {
        let current_line = out.lines().count() + 1;
        writeln!(
            out,
            "// UNCONSTRAINED GENERICS at generated.rs line {}: {}",
            current_line,
            deferred_unconstrained.join(", ")
        )
        .unwrap();
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
            // Use `StructInfo.orig_generics` to decide which type_args correspond
            // to non-length, non-lifetime generics so the printed arity matches
            // the declared `Dyn` struct. Fall back to printing any non-length
            // args if struct info is missing.
            if let Some(info) = struct_info.get(&kind.to_string()) {
                let mut parts: Vec<String> = Vec::new();
                for (idx, arg) in type_args.iter().enumerate() {
                    if let Some((_, gkind, param_kind)) = info.orig_generics.get(idx) {
                        if *param_kind == IrGenericParamKind::Lifetime {
                            continue;
                        }
                        if *gkind == GenericKind::Length {
                            continue;
                        }
                    }
                    let mut s = String::new();
                    if write_type_dyn(&mut s, arg, &cur_params, struct_info) {
                        parts.push(s);
                    }
                }
                if !parts.is_empty() {
                    write!(out, "<{}>", parts.join(", ")).unwrap();
                }
            } else {
                // Fallback: print any non-length type args
                let mut parts: Vec<String> = Vec::new();
                for arg in type_args.iter() {
                    let mut s = String::new();
                    if write_type_dyn(&mut s, arg, &cur_params, struct_info) {
                        parts.push(s);
                    }
                }
                if !parts.is_empty() {
                    write!(out, "<{}>", parts.join(", ")).unwrap();
                }
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
            let mut parts: Vec<String> = Vec::new();
            for elem in elems.iter() {
                let mut s = String::new();
                if write_type_dyn(&mut s, elem, cur_params, struct_info) {
                    parts.push(s);
                }
            }
            write!(out, "({})", parts.join(", ")).unwrap();
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
                    // Emit an immediate runtime witness for the associated type.
                    // Wrap in a block so we can create the witness inline.
                    write!(
                        out,
                        "({{ let w = <{} as typenum::Unsigned>::USIZE; w }})",
                        type_name
                    )
                    .unwrap();
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
