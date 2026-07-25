//! `IrModule` → Noir source text.
//!
//! Hand-written recursive-match text emitter, mirroring the shape of
//! `volar_compiler::printer`/`printer_ts` (there is no shared `IrVisitor`
//! trait anywhere in this codebase — every consumer hand-rolls its own
//! full match, and this is a third one, not a new abstraction).
//!
//! v1 scope (see `docs/noir-backend.md` once written, and the approved
//! plan): straight-line expressions, `if`/`else`, direct function calls,
//! and scalar/bool typed functions with no generics, no loops, no structs.
//! Later milestones extend this file; unsupported constructs return a
//! `NoirCodegenError` rather than panicking or emitting broken text.

#[cfg(feature = "std")]
use std::{format, string::String, string::ToString, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::{format, string::String, string::ToString, vec::Vec};

use volar_compiler::ir::{
    IrBlock, IrExpr, IrExprKind, IrFunction, IrGenericParam, IrLit, IrModule, IrParam, IrPattern,
    IrStmtKind, IrType, PrimitiveType, SpecBinOp, SpecUnaryOp,
};
use volar_compiler_passes::const_analysis::{classify_generic_with_aliases, GenericKind};

use crate::error::NoirCodegenError;
use crate::lowering_noir::validate_module;

/// Print `module` as Noir source. Runs the pre-print validation pass
/// first (see `lowering_noir`) and returns every violation found — this
/// never panics and never emits source text for a rejected module.
pub fn print_module_noir(module: &IrModule<IrFunction>) -> Result<String, Vec<NoirCodegenError>> {
    validate_module(module)?;

    let mut out = String::new();
    let mut errors = Vec::new();

    // Empirically confirmed (`nargo check`): unlike Rust, Noir does not
    // put `Add`/`Sub`/`Mul`/etc. in scope for `impl` purposes without an
    // explicit `use` -- a bare `impl Add for Galois` fails with "Trait Add
    // not found" otherwise. Collect exactly the trait names actually used
    // so the import list doesn't grow unboundedly.
    let mut used_traits = Vec::new();
    for imp in &module.impls {
        if let Some(tr) = &imp.trait_ {
            if let Ok(name) = trait_kind_to_noir_name(&tr.kind) {
                if !used_traits.contains(&name) {
                    used_traits.push(name);
                }
            }
        }
    }
    if !used_traits.is_empty() {
        out.push_str(&format!("use std::ops::{{{}}};\n\n", used_traits.join(", ")));
    }

    for s in &module.structs {
        // `GenericArray` is a structural alias for Volar's own generic-
        // array crate type, not a user struct declaration Noir needs to
        // see -- Noir already has native fixed-size arrays. Not yet
        // empirically verified against a real GenericArray fixture; skip
        // rather than guess a (possibly wrong) declaration.
        if s.kind == volar_compiler::ir::StructKind::GenericArray {
            continue;
        }
        match print_struct(s) {
            Ok(text) => {
                out.push_str(&text);
                out.push_str("\n\n");
            }
            Err(e) => errors.push(e),
        }
    }
    for imp in &module.impls {
        match print_impl(imp) {
            Ok(text) => {
                out.push_str(&text);
                out.push_str("\n\n");
            }
            Err(e) => errors.push(e),
        }
    }
    for function in &module.functions {
        match print_function(function) {
            Ok(text) => {
                out.push_str(&text);
                out.push_str("\n\n");
            }
            Err(e) => errors.push(e),
        }
    }
    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

/// Print an `impl` block. Trait impls are restricted in v1 to the
/// `MathTrait` operator-overload set (`Add`/`Sub`/`Mul`/`Div`/`BitAnd`/
/// `BitOr`/`BitXor`/`Shl`/`Shr`/`Not`/`Neg`) -- confirmed against Noir's
/// actual stdlib trait definitions (`noir_stdlib/src/ops/{arith,bit}.nr`):
/// same trait names and method names as Rust's `std::ops` (`fn add(self,
/// other: Self) -> Self`, `fn bitxor(self, other: Self) -> Self`, etc.),
/// so the method name text needs no remapping -- `IrFunction.name` already
/// carries the right Noir method name straight from the source. This is
/// what makes the `volar-primitives` GF(2^k)/GF(3) software-fallback
/// strategy fall out "for free": those types are ordinary structs with
/// exactly these operator-overload impls, parsed as ordinary source (see
/// the approved plan). Comparison/clone/default traits and any non-`Math`
/// `TraitKind` are unsupported in v1 (trait-bound/translation work beyond
/// this narrow set is milestone 6 territory).
fn print_impl(imp: &volar_compiler::ir::IrImpl) -> Result<String, NoirCodegenError> {
    let self_ty_text = type_to_noir(&imp.self_ty, "<impl>")?;
    let generics_text = print_generics(&imp.generics, "<impl>")?;

    let header = match &imp.trait_ {
        None => format!("impl{generics_text} {self_ty_text}"),
        Some(tr) => {
            let trait_name = trait_kind_to_noir_name(&tr.kind)?;
            format!("impl{generics_text} {trait_name} for {self_ty_text}")
        }
    };

    let mut methods = Vec::new();
    for item in &imp.items {
        match item {
            volar_compiler::ir::IrImplItem::Method(f) => methods.push(print_function(f)?),
            volar_compiler::ir::IrImplItem::AssociatedType { name, .. } => {
                return Err(NoirCodegenError::Unsupported {
                    function: "<impl>".into(),
                    reason: format!("associated type `{name}` in an impl block is not yet supported"),
                });
            }
        }
    }

    Ok(format!("{header} {{\n{}\n}}", indent(&methods.join("\n\n"))))
}

fn trait_kind_to_noir_name(kind: &volar_compiler::ir::TraitKind) -> Result<&'static str, NoirCodegenError> {
    use volar_compiler::ir::{MathTrait, TraitKind};
    match kind {
        TraitKind::Math(MathTrait::Add) => Ok("Add"),
        TraitKind::Math(MathTrait::Sub) => Ok("Sub"),
        TraitKind::Math(MathTrait::Mul) => Ok("Mul"),
        TraitKind::Math(MathTrait::Div) => Ok("Div"),
        TraitKind::Math(MathTrait::BitAnd) => Ok("BitAnd"),
        TraitKind::Math(MathTrait::BitOr) => Ok("BitOr"),
        TraitKind::Math(MathTrait::BitXor) => Ok("BitXor"),
        TraitKind::Math(MathTrait::Shl) => Ok("Shl"),
        TraitKind::Math(MathTrait::Shr) => Ok("Shr"),
        TraitKind::Math(MathTrait::Not) => Ok("Not"),
        TraitKind::Math(MathTrait::Neg) => Ok("Neg"),
        other => Err(NoirCodegenError::Unsupported {
            function: "<impl>".into(),
            reason: format!(
                "trait `{other:?}` is not yet translated to Noir trait syntax \
                 (see milestone 6)"
            ),
        }),
    }
}

fn print_struct(s: &volar_compiler::ir::IrStruct) -> Result<String, NoirCodegenError> {
    let name = s.kind.to_string();
    // Empirically confirmed (`nargo check`): Noir has no tuple-struct
    // syntax at all (`struct Galois(u8);` is a parse error -- "Expected a
    // '{' but found '('"). Volar's own `volar-primitives` fallback types
    // (Bit/Galois/etc.) are all tuple structs, so this is a real, current
    // gap blocking that specific fallback, not a hypothetical one --
    // rejected clearly rather than emitting invalid syntax. Converting to
    // a synthesized named-field struct (and correspondingly rewriting
    // `.0`-style field access/tuple-construction call syntax throughout
    // the expression printer) is tracked future work.
    if s.is_tuple {
        return Err(NoirCodegenError::Unsupported {
            function: name.clone(),
            reason: format!(
                "`{name}` is a tuple struct -- Noir has no tuple-struct syntax \
                 (confirmed via nargo check); needs conversion to a named-field \
                 struct plus rewriting `.0`-style access, not yet implemented"
            ),
        });
    }
    let generics_text = print_generics(&s.generics, &name)?;
    let mut fields = Vec::new();
    for f in &s.fields {
        fields.push(format!("    {}: {},", f.name, type_to_noir(&f.ty, &name)?));
    }
    Ok(format!(
        "struct {}{} {{\n{}\n}}",
        name,
        generics_text,
        fields.join("\n"),
    ))
}

fn print_function(function: &IrFunction) -> Result<String, NoirCodegenError> {
    let generics_text = print_generics(&function.generics, &function.name)?;

    let mut params = Vec::new();
    match function.receiver {
        None => {}
        Some(volar_compiler::ir::IrReceiver::Value) => params.push("self".to_string()),
        Some(volar_compiler::ir::IrReceiver::Ref) => params.push("&self".to_string()),
        Some(volar_compiler::ir::IrReceiver::RefMut) => {
            return Err(NoirCodegenError::Unsupported {
                function: function.name.clone(),
                reason: "methods taking &mut self are not yet supported".into(),
            });
        }
    }
    for p in &function.params {
        params.push(print_param(p, &function.name)?);
    }

    // Noir's entry point requires `pub` on the return type: the verifier
    // cannot retrieve a private witness, so a `main` returning a value at
    // all must return it publicly. Only `main` is an entry point — other
    // functions' return types are ordinary (private) values.
    let ret = match &function.return_type {
        None | Some(IrType::Unit) => String::new(),
        Some(ty) => {
            let pub_prefix = if function.name == "main" { "pub " } else { "" };
            format!(" -> {pub_prefix}{}", type_to_noir(ty, &function.name)?)
        }
    };

    let body = print_block(&function.body, &function.name, &function.generics)?;

    Ok(format!(
        "fn {}{}({}){} {{\n{}\n}}",
        function.name,
        generics_text,
        params.join(", "),
        ret,
        indent(&body),
    ))
}

fn print_param(param: &IrParam, fn_name: &str) -> Result<String, NoirCodegenError> {
    Ok(format!("{}: {}", param.name, type_to_noir(&param.ty, fn_name)?))
}

/// Print a generics list as Noir generic-parameter syntax, or `""` if empty.
///
/// Reuses `classify_generic_with_aliases` (unchanged) to split each param
/// into `GenericKind::Length` vs `GenericKind::Type` — this is the core
/// "avoid monomorphization" mechanism from the approved plan. Where
/// `lowering_dyn` converts every `Length`-kind generic into a runtime
/// `usize` parameter (Rust-dyn/TS have no const generics), this printer
/// takes the opposite branch: `Length`-kind params are *retained* as real
/// generic parameters and printed as Noir numeric generics, since Noir has
/// them natively (`fn foo<let N: u32>(...)`) — confirmed exact syntax
/// (the `let` keyword is mandatory, default type `u32`) against current
/// Noir docs.
fn print_generics(generics: &[IrGenericParam], fn_name: &str) -> Result<String, NoirCodegenError> {
    if generics.is_empty() {
        return Ok(String::new());
    }
    let all_params: [&[IrGenericParam]; 1] = [generics];
    let mut parts = Vec::new();
    for g in generics {
        match classify_generic_with_aliases(g, &all_params, &[]) {
            GenericKind::Length => {
                let const_ty = match &g.const_ty {
                    Some(t) => type_to_noir(t, fn_name)?,
                    None => "u32".into(),
                };
                parts.push(format!("let {}: {}", g.name, const_ty));
            }
            GenericKind::Type => {
                if !g.bounds.is_empty() {
                    return Err(NoirCodegenError::Unsupported {
                        function: fn_name.into(),
                        reason: format!(
                            "generic type parameter `{}` has trait bounds, which are not \
                             yet translated to Noir trait syntax (see milestone 6)",
                            g.name
                        ),
                    });
                }
                parts.push(g.name.clone());
            }
        }
    }
    Ok(format!("<{}>", parts.join(", ")))
}

/// Map an `IrType` to Noir source text.
///
/// `Reference { mutable: false, elem }` is transparently unwrapped to
/// `elem` (following the TS-printer precedent — Noir, like TS, has no
/// general first-class reference type). `Reference { mutable: true, .. }`
/// is left unsupported in v1 rather than silently dropping its mutation
/// semantics (see the approved plan's reference-handling discussion) —
/// a `mut`-parameter policy is a deliberate later decision, not a guess.
fn type_to_noir(ty: &IrType, fn_name: &str) -> Result<String, NoirCodegenError> {
    match ty {
        IrType::Primitive(p) => primitive_to_noir(*p, fn_name),
        IrType::Unit => Ok("()".into()),
        IrType::Tuple(elems) => {
            let mut parts = Vec::new();
            for e in elems {
                parts.push(type_to_noir(e, fn_name)?);
            }
            Ok(format!("({})", parts.join(", ")))
        }
        IrType::Reference { mutable: false, elem } => type_to_noir(elem, fn_name),
        IrType::Reference { mutable: true, .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "mutable reference types (&mut T) are not yet supported".into(),
        }),
        // `kind` (GenericArray/FixedArray/Slice) doesn't affect the Noir
        // type text -- Noir has one fixed-array construct. `Slice`
        // conceptually *is* a runtime-length view (Rust `[T]`), so it's
        // rejected the same way `Vector` is below, via the same length
        // resolution failing to apply (a slice's `len` isn't a real
        // `ArrayLength` in the first place at this layer).
        IrType::Array { elem, len, .. } => {
            let elem_text = type_to_noir(elem, fn_name)?;
            let len_text = crate::const_eval::eval_array_length(len, &[])
                .map(|c| c.to_string())
                .ok_or_else(|| NoirCodegenError::UnsupportedArrayLength {
                    function: fn_name.into(),
                    reason: format!("array length {len:?} does not resolve to a Noir constant"),
                })?;
            Ok(format!("[{elem_text}; {len_text}]"))
        }
        IrType::Vector { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "Vector has no compile-time length in Noir; use a fixed-size array \
                     ([T; N]) instead"
                .into(),
        }),
        IrType::Struct { kind: volar_compiler::ir::StructKind::GenericArray, type_args } => {
            Err(NoirCodegenError::Unsupported {
                function: fn_name.into(),
                reason: format!(
                    "GenericArray<{}> is not yet supported -- its type_args shape hasn't \
                     been verified against a real fixture yet (see milestone 5 notes)",
                    type_args.len()
                ),
            })
        }
        IrType::Struct { kind, type_args } => {
            if type_args.is_empty() {
                Ok(kind.to_string())
            } else {
                let mut parts = Vec::new();
                for t in type_args {
                    parts.push(type_to_noir(t, fn_name)?);
                }
                Ok(format!("{kind}<{}>", parts.join(", ")))
            }
        }
        // A `TypeParam` reference to a declared generic prints as its bare
        // name; `print_generics` is responsible for rejecting anything
        // Noir can't express in the generic-parameter list itself, so by
        // the time a reference is printed here it's already been accepted.
        IrType::TypeParam(name) => Ok(name.clone()),
        IrType::Param { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "IrType::Param (multi-segment path type reference) has not been \
                     observed reaching this layer yet — treated as unsupported until a \
                     real fixture demonstrates the shape needed"
                .into(),
        }),
        // `Self::Output` (empirically the exact shape the parser produces
        // for a Rust `impl Add for X { type Output = X; fn add(...) ->
        // Self::Output }` method signature -- confirmed by parsing real
        // `volar-primitives` source) resolves to plain `Self`: Noir's own
        // arithmetic traits (`noir_stdlib/src/ops/{arith,bit}.nr`) have no
        // `Output` associated type at all -- `fn add(self, other: Self) ->
        // Self` returns `Self` directly. This is a narrow, targeted rule
        // for exactly this pattern, not a general associated-type
        // resolution system (that's out of scope -- see the `Unsupported`
        // fallback below for every other projection shape).
        IrType::Projection { base, assoc: volar_compiler::ir::AssociatedType::Output, .. } => {
            type_to_noir(base, fn_name)
        }
        IrType::Projection { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "associated-type projections (other than the arithmetic-trait \
                     `Self::Output` pattern) are not supported"
                .into(),
        }),
        IrType::Existential { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "`impl Trait` types are not supported".into(),
        }),
        IrType::FnPtr { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "function-pointer types are not supported in constrained Noir".into(),
        }),
        IrType::Never => Ok("!".into()),
        IrType::Infer => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "an unresolved (`_`) type reached codegen — incomplete type inference \
                     upstream"
                .into(),
        }),
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unrecognized IrType variant: {other:?}"),
        }),
    }
}

/// Primitive-type mapping. `Usize` defaults to `u64` (Noir has no `usize`);
/// this is a named verification item in the plan — confirm the idiomatic
/// target width against Noir docs before shipping.
fn primitive_to_noir(p: PrimitiveType, fn_name: &str) -> Result<String, NoirCodegenError> {
    match p {
        PrimitiveType::Bool => Ok("bool".into()),
        PrimitiveType::U8 => Ok("u8".into()),
        PrimitiveType::U32 => Ok("u32".into()),
        PrimitiveType::U64 => Ok("u64".into()),
        PrimitiveType::Usize => Ok("u64".into()),
        PrimitiveType::U128 => Ok("u128".into()),
        PrimitiveType::I128 => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "Noir has no 128-bit signed integer type".into(),
        }),
        // GF(2^k)/GF(3) field-element types -- supported via the
        // `volar-primitives` software fallback (see milestone 5 notes),
        // not any Noir-native field arithmetic. Empirically confirmed
        // (by parsing the real `volar-primitives` source) that the
        // compiler's own parser tags a *reference* to one of these types
        // (e.g. an impl's `self_ty`, a method parameter/return type) as
        // `IrType::Primitive(PrimitiveType::Galois)`, distinct from the
        // `IrType::Struct(StructKind::Custom("Galois"))` shape used when
        // the struct's own *declaration* is parsed -- both must map to
        // the same Noir identifier for the emitted source to be
        // internally consistent, so this prints the bare name rather
        // than rejecting.
        PrimitiveType::Bit => Ok("Bit".into()),
        PrimitiveType::Galois => Ok("Galois".into()),
        PrimitiveType::Galois64 => Ok("Galois64".into()),
        PrimitiveType::Galois128 => Ok("Galois128".into()),
        PrimitiveType::Galois256 => Ok("Galois256".into()),
        PrimitiveType::BitsInBytes => Ok("BitsInBytes".into()),
        PrimitiveType::BitsInBytes64 => Ok("BitsInBytes64".into()),
        PrimitiveType::Z3 => Ok("Z3".into()),
    }
}

fn print_block(block: &IrBlock, fn_name: &str, generics: &[IrGenericParam]) -> Result<String, NoirCodegenError> {
    let mut lines = Vec::new();
    for stmt in &block.stmts {
        lines.push(print_stmt(&stmt.kind, fn_name, generics)?);
    }
    if let Some(tail) = &block.expr {
        lines.push(print_expr(tail, fn_name, generics)?);
    }
    Ok(lines.join(";\n"))
}

fn print_stmt(stmt: &IrStmtKind, fn_name: &str, generics: &[IrGenericParam]) -> Result<String, NoirCodegenError> {
    match stmt {
        IrStmtKind::Let { pattern, ty, init } => {
            let name = ident_pattern_name(pattern, fn_name)?;
            let ty_ann = match ty {
                Some(t) => format!(": {}", type_to_noir(t, fn_name)?),
                None => String::new(),
            };
            let init_text = match init {
                Some(e) => format!(" = {}", print_expr(e, fn_name, generics)?),
                None => String::new(),
            };
            Ok(format!("let {name}{ty_ann}{init_text}"))
        }
        IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => print_expr(e, fn_name, generics),
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unrecognized statement kind: {other:?}"),
        }),
    }
}

fn ident_pattern_name(pattern: &IrPattern, fn_name: &str) -> Result<String, NoirCodegenError> {
    match pattern {
        IrPattern::Ident { mutable, name, subpat: None } => {
            Ok(if *mutable { format!("mut {name}") } else { name.clone() })
        }
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unsupported let-binding pattern: {other:?}"),
        }),
    }
}

fn print_expr(expr: &IrExpr, fn_name: &str, generics: &[IrGenericParam]) -> Result<String, NoirCodegenError> {
    match &expr.kind {
        IrExprKind::Lit(lit) => print_lit(lit),
        IrExprKind::Var(name) => Ok(name.clone()),
        IrExprKind::Path { segments, .. } => Ok(segments.join("::")),

        IrExprKind::Binary { op, left, right } => Ok(format!(
            "({} {} {})",
            print_expr(left, fn_name, generics)?,
            bin_op_str(*op),
            print_expr(right, fn_name, generics)?,
        )),

        // References are transparently unwrapped in v1: `&x`/`&mut x`/`*x`
        // all print as just the inner expression's text.
        IrExprKind::Unary { op: SpecUnaryOp::Ref | SpecUnaryOp::RefMut | SpecUnaryOp::Deref, expr } => {
            print_expr(expr, fn_name, generics)
        }
        IrExprKind::Unary { op: SpecUnaryOp::Neg, expr } => {
            Ok(format!("(-{})", print_expr(expr, fn_name, generics)?))
        }
        IrExprKind::Unary { op: SpecUnaryOp::Not, expr } => {
            Ok(format!("(!{})", print_expr(expr, fn_name, generics)?))
        }

        IrExprKind::Call { func, args } => {
            let func_text = print_expr(func, fn_name, generics)?;
            let mut arg_texts = Vec::new();
            for a in args {
                arg_texts.push(print_expr(a, fn_name, generics)?);
            }
            Ok(format!("{}({})", func_text, arg_texts.join(", ")))
        }

        // `MethodKind::Other(name)` is a plain user-defined method (e.g.
        // an inherent or trait-impl method reachable via `print_impl`) --
        // direct 1:1 translation, `receiver.name(args)`. `Known(StdMethod)`
        // (a curated safe subset -- `Len`/`WrappingAdd`/`WrappingSub`/
        // `Min`/`Max`/`Pow`, per the plan) and `Vole(_)` (backend-specific,
        // not applicable to Noir) are both deferred to milestone 6.
        IrExprKind::MethodCall { receiver, method: volar_compiler::ir::MethodKind::Other(name), args, .. } => {
            let receiver_text = print_expr(receiver, fn_name, generics)?;
            let mut arg_texts = Vec::new();
            for a in args {
                arg_texts.push(print_expr(a, fn_name, generics)?);
            }
            Ok(format!("{receiver_text}.{name}({})", arg_texts.join(", ")))
        }
        IrExprKind::MethodCall { method, .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!(
                "method kind `{method:?}` is not yet translated to Noir (see milestone 6 \
                 for the curated StdMethod subset)"
            ),
        }),

        IrExprKind::Cast { expr, ty } => {
            Ok(format!("({} as {})", print_expr(expr, fn_name, generics)?, type_to_noir(ty, fn_name)?))
        }

        IrExprKind::Field { base, field } => {
            Ok(format!("{}.{field}", print_expr(base, fn_name, generics)?))
        }

        IrExprKind::Index { base, index } => Ok(format!(
            "{}[{}]",
            print_expr(base, fn_name, generics)?,
            print_expr(index, fn_name, generics)?,
        )),

        // Array literal `[a, b, c]` -- direct 1:1 translation, Noir's
        // array-literal syntax matches Rust's (empirically confirmed via
        // nargo execute, see the milestone-5 integration tests).
        //
        // `Array` and `FixedArray` print identically here, matching the
        // TS printer's own precedent (`printer_ts.rs`, `IrExprKind::Array
        // | IrExprKind::FixedArray` share an arm). This corrects an
        // initial assumption in this file: despite `FixedArray`'s doc
        // comment framing `Array` as "the vec![]-shaped variant," the
        // parser (`parser.rs`, `Expr::Array` handling) actually produces
        // plain `IrExprKind::Array` for *every* real `[a, b, c]` literal in
        // parsed Rust source -- `vec![...]` heap-allocation is a choice the
        // *Rust-dyn printer* makes for its own output, not a distinction
        // the AST itself carries. A Noir backend has no heap/Vec at all,
        // so both variants mean exactly the same thing here: a fixed-size
        // array literal.
        IrExprKind::Array(elems) | IrExprKind::FixedArray(elems) => {
            let mut parts = Vec::new();
            for e in elems {
                parts.push(print_expr(e, fn_name, generics)?);
            }
            Ok(format!("[{}]", parts.join(", ")))
        }
        IrExprKind::Repeat { elem, len } => Ok(format!(
            "[{}; {}]",
            print_expr(elem, fn_name, generics)?,
            print_expr(len, fn_name, generics)?,
        )),
        IrExprKind::Tuple(elems) => {
            let mut parts = Vec::new();
            for e in elems {
                parts.push(print_expr(e, fn_name, generics)?);
            }
            Ok(format!("({})", parts.join(", ")))
        }

        // Struct-update syntax (`..rest`) is not yet supported -- Noir's
        // exact support for it hasn't been empirically verified, and
        // expanding it correctly requires knowing the struct's full field
        // list (available from the module's struct table, not plumbed
        // into this expression printer yet). Plain field-by-field literals
        // work today; `rest` is a clean v1 boundary.
        IrExprKind::StructExpr { kind, rest: Some(_), .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!(
                "struct-update syntax (`..rest`) on `{kind}` is not yet supported \
                 (see milestone 5 notes) -- use an explicit field-by-field literal"
            ),
        }),
        IrExprKind::StructExpr { kind, fields, rest: None, .. } => {
            let mut parts = Vec::new();
            for (name, value) in fields {
                parts.push(format!("{name}: {}", print_expr(value, fn_name, generics)?));
            }
            Ok(format!("{kind} {{ {} }}", parts.join(", ")))
        }

        IrExprKind::Assign { left, right } => {
            Ok(format!("{} = {}", print_expr(left, fn_name, generics)?, print_expr(right, fn_name, generics)?))
        }
        IrExprKind::AssignOp { op, left, right } => Ok(format!(
            "{} {}= {}",
            print_expr(left, fn_name, generics)?,
            bin_op_str(*op),
            print_expr(right, fn_name, generics)?,
        )),

        IrExprKind::Block(b) => Ok(format!("{{\n{}\n}}", indent(&print_block(b, fn_name, generics)?))),

        IrExprKind::If { cond, then_branch, else_branch } => {
            let cond_text = print_expr(cond, fn_name, generics)?;
            let then_text = print_block(then_branch, fn_name, generics)?;
            let else_text = match else_branch {
                None => String::new(),
                Some(e) => format!(" else {}", print_else_arm(e, fn_name, generics)?),
            };
            Ok(format!(
                "if {} {{\n{}\n}}{}",
                cond_text,
                indent(&then_text),
                else_text,
            ))
        }

        // Loop-bound legality was already fully checked by the pre-print
        // validation pass (`lowering_noir::validate_module`) — this
        // re-derives the same `NoirConstExpr` (cheap, pure) rather than
        // smuggling it through the shared `IrModule` tree, per the plan's
        // design note. Defensive `NonConstantLoopBound` fallback below
        // covers the case where this printer is ever called without going
        // through validation first — never a panic either way.
        IrExprKind::BoundedLoop { var, start, end, inclusive, body } => {
            let start_text = crate::const_eval::eval_const_expr(start, generics)
                .map(|c| c.to_string())
                .ok_or_else(|| NoirCodegenError::NonConstantLoopBound {
                    function: fn_name.into(),
                    reason: "start bound is not a compile-time constant".into(),
                })?;
            let end_text = crate::const_eval::eval_const_expr(end, generics)
                .map(|c| c.to_string())
                .ok_or_else(|| NoirCodegenError::NonConstantLoopBound {
                    function: fn_name.into(),
                    reason: "end bound is not a compile-time constant".into(),
                })?;
            let range_op = if *inclusive { "..=" } else { ".." };
            let body_text = print_block(body, fn_name, generics)?;
            Ok(format!(
                "for {var} in {start_text}{range_op}{end_text} {{\n{}\n}}",
                indent(&body_text),
            ))
        }

        // v1 policy (see `lowering_noir`): only a literal fixed-size
        // collection is accepted, so `collection` always prints via the
        // ordinary expression printer here.
        IrExprKind::IterLoop { pattern, collection, body } => {
            let pat_text = ident_pattern_name(pattern, fn_name)?;
            let coll_text = print_expr(collection, fn_name, generics)?;
            let body_text = print_block(body, fn_name, generics)?;
            Ok(format!(
                "for {pat_text} in {coll_text} {{\n{}\n}}",
                indent(&body_text),
            ))
        }

        // Noir (as of 1.0.0-beta.24) has no `match` expression and no enum
        // type at all — confirmed against the official docs (the control-
        // flow page covers if/for/while with no mention of match; there is
        // no enums page; the 1.0 pre-release announcement lists "full
        // support for primitive types... and complex data structures
        // including arrays, tuples, vectors" with no mention of enums).
        // This is a genuine language-level absence, not just an unbuilt v1
        // restriction — revisit if a future Noir version adds it.
        IrExprKind::Match { .. } => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: "Noir has no `match` expression or enum type (verified against \
                     current Noir docs) — restructure as nested `if`/`else` over the \
                     discriminating condition"
                .into(),
        }),

        IrExprKind::Return(Some(e)) => Ok(format!("return {}", print_expr(e, fn_name, generics)?)),
        IrExprKind::Return(None) => Ok("return".into()),

        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("expression kind not yet supported in v1: {other:?}"),
        }),
    }
}

/// An `else` arm is itself an arbitrary boxed expression: either another
/// `If` (an `else if` chain) or a `Block`. Both print correctly through
/// the ordinary expression printer; anything else would not be valid Noir
/// `else` syntax on its own, so it's rejected explicitly rather than
/// emitted as-is.
fn print_else_arm(expr: &IrExpr, fn_name: &str, generics: &[IrGenericParam]) -> Result<String, NoirCodegenError> {
    match &expr.kind {
        IrExprKind::If { .. } | IrExprKind::Block(_) => print_expr(expr, fn_name, generics),
        other => Err(NoirCodegenError::Unsupported {
            function: fn_name.into(),
            reason: format!("unsupported else-arm shape: {other:?}"),
        }),
    }
}

fn print_lit(lit: &IrLit) -> Result<String, NoirCodegenError> {
    match lit {
        IrLit::Int(n) => Ok(n.to_string()),
        IrLit::Bool(b) => Ok(b.to_string()),
        other => Err(NoirCodegenError::Unsupported {
            function: "<literal>".into(),
            reason: format!("literal kind not yet supported in v1: {other:?}"),
        }),
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

fn indent(text: &str) -> String {
    text.lines()
        .map(|l| if l.is_empty() { l.to_string() } else { format!("    {l}") })
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_compiler::ir::{ExternalKind, IrGenericParam};

    fn expr(kind: IrExprKind) -> IrExpr {
        IrExpr { kind, prov: (), side: None }
    }

    fn block(stmts: Vec<IrStmtKind>, tail: Option<IrExprKind>) -> IrBlock {
        IrBlock {
            stmts: stmts
                .into_iter()
                .map(|kind| volar_compiler::ir::IrStmt { kind, prov: (), side: None })
                .collect(),
            expr: tail.map(|k| Box::new(expr(k))),
        }
    }

    fn function(name: &str, params: Vec<IrParam>, return_type: Option<IrType>, body: IrBlock) -> IrFunction {
        IrFunction {
            name: name.into(),
            module_path: Vec::new(),
            generics: Vec::new(),
            receiver: None,
            params,
            return_type,
            where_clause: Vec::new(),
            body,
            external_kind: ExternalKind::Normal,
            no_inline: false,
        }
    }

    #[test]
    fn primitive_type_mapping() {
        assert_eq!(primitive_to_noir(PrimitiveType::Bool, "f").unwrap(), "bool");
        assert_eq!(primitive_to_noir(PrimitiveType::U8, "f").unwrap(), "u8");
        assert_eq!(primitive_to_noir(PrimitiveType::U32, "f").unwrap(), "u32");
        assert_eq!(primitive_to_noir(PrimitiveType::U64, "f").unwrap(), "u64");
        assert_eq!(primitive_to_noir(PrimitiveType::Usize, "f").unwrap(), "u64");
        assert_eq!(primitive_to_noir(PrimitiveType::U128, "f").unwrap(), "u128");
        assert!(primitive_to_noir(PrimitiveType::I128, "f").is_err());
    }

    #[test]
    fn gf_primitive_types_map_to_bare_struct_names() {
        // These must match the corresponding `Custom(name)` struct
        // declaration name exactly, since the parser tags usage sites
        // (self_ty, param/return types) with `Primitive(Galois)` but
        // struct *declarations* with `Struct(Custom("Galois"))` -- both
        // need to emit the same Noir identifier.
        assert_eq!(primitive_to_noir(PrimitiveType::Bit, "f").unwrap(), "Bit");
        assert_eq!(primitive_to_noir(PrimitiveType::Galois, "f").unwrap(), "Galois");
        assert_eq!(primitive_to_noir(PrimitiveType::Galois64, "f").unwrap(), "Galois64");
        assert_eq!(primitive_to_noir(PrimitiveType::Z3, "f").unwrap(), "Z3");
    }

    #[test]
    fn self_output_projection_resolves_to_self() {
        let ty = IrType::Projection {
            base: Box::new(IrType::TypeParam("Self".into())),
            trait_path: None,
            trait_args: Vec::new(),
            assoc: volar_compiler::ir::AssociatedType::Output,
        };
        assert_eq!(type_to_noir(&ty, "f").unwrap(), "Self");
    }

    #[test]
    fn other_projection_shapes_remain_unsupported() {
        let ty = IrType::Projection {
            base: Box::new(IrType::TypeParam("B".into())),
            trait_path: Some("BlockEncrypt".into()),
            trait_args: Vec::new(),
            assoc: volar_compiler::ir::AssociatedType::BlockSize,
        };
        assert!(type_to_noir(&ty, "f").is_err());
    }

    #[test]
    fn straight_line_function_text() {
        let body = block(
            vec![],
            Some(IrExprKind::Binary {
                op: SpecBinOp::Add,
                left: Box::new(expr(IrExprKind::Var("a".into()))),
                right: Box::new(expr(IrExprKind::Var("b".into()))),
            }),
        );
        let f = function(
            "main",
            vec![
                IrParam { name: "a".into(), ty: IrType::Primitive(PrimitiveType::U32) },
                IrParam { name: "b".into(), ty: IrType::Primitive(PrimitiveType::U32) },
            ],
            Some(IrType::Primitive(PrimitiveType::U32)),
            body,
        );
        let text = print_function(&f).unwrap();
        // `main`'s return type must be `pub` — Noir's entry-point requirement.
        assert!(text.contains("-> pub u32"), "{text}");
        assert!(text.contains("(a + b)"), "{text}");
    }

    #[test]
    fn non_main_function_return_type_is_not_pub() {
        let body = block(vec![], Some(IrExprKind::Var("a".into())));
        let f = function(
            "helper",
            vec![IrParam { name: "a".into(), ty: IrType::Primitive(PrimitiveType::U32) }],
            Some(IrType::Primitive(PrimitiveType::U32)),
            body,
        );
        let text = print_function(&f).unwrap();
        assert!(text.contains("-> u32") && !text.contains("-> pub u32"), "{text}");
    }

    #[test]
    fn if_else_prints_as_value_yielding_expression() {
        let if_expr = expr(IrExprKind::If {
            cond: Box::new(expr(IrExprKind::Binary {
                op: SpecBinOp::Gt,
                left: Box::new(expr(IrExprKind::Var("a".into()))),
                right: Box::new(expr(IrExprKind::Var("b".into()))),
            })),
            then_branch: block(vec![], Some(IrExprKind::Var("a".into()))),
            else_branch: Some(Box::new(expr(IrExprKind::Block(block(
                vec![],
                Some(IrExprKind::Var("b".into())),
            ))))),
        });
        let text = print_expr(&if_expr, "f", &[]).unwrap();
        assert!(text.starts_with("if (a > b) {"), "{text}");
        assert!(text.contains("} else {"), "{text}");
    }

    #[test]
    fn match_is_rejected_with_a_clear_reason() {
        let m = expr(IrExprKind::Match {
            expr: Box::new(expr(IrExprKind::Var("a".into()))),
            arms: Vec::new(),
        });
        let err = print_expr(&m, "f", &[]).unwrap_err();
        match err {
            NoirCodegenError::Unsupported { reason, .. } => {
                assert!(reason.contains("match"), "{reason}");
            }
            other => panic!("expected Unsupported, got {other:?}"),
        }
    }

    #[test]
    fn literal_bound_loop_prints_as_noir_for() {
        let loop_expr = expr(IrExprKind::BoundedLoop {
            var: "i".into(),
            start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
            end: Box::new(expr(IrExprKind::Lit(IrLit::Int(10)))),
            inclusive: false,
            body: block(vec![], None),
        });
        let text = print_expr(&loop_expr, "f", &[]).unwrap();
        assert!(text.starts_with("for i in 0..10 {"), "{text}");
    }

    #[test]
    fn inclusive_loop_uses_inclusive_range_operator() {
        let loop_expr = expr(IrExprKind::BoundedLoop {
            var: "i".into(),
            start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
            end: Box::new(expr(IrExprKind::Lit(IrLit::Int(10)))),
            inclusive: true,
            body: block(vec![], None),
        });
        let text = print_expr(&loop_expr, "f", &[]).unwrap();
        assert!(text.starts_with("for i in 0..=10 {"), "{text}");
    }

    #[test]
    fn generic_const_bound_loop_prints_symbolic_bound() {
        let generics = vec![IrGenericParam {
            name: "N".into(),
            kind: volar_compiler::ir::IrGenericParamKind::Const,
            const_ty: None,
            bounds: Vec::new(),
            default: None,
        }];
        let loop_expr = expr(IrExprKind::BoundedLoop {
            var: "i".into(),
            start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
            end: Box::new(expr(IrExprKind::Var("N".into()))),
            inclusive: false,
            body: block(vec![], None),
        });
        let text = print_expr(&loop_expr, "f", &generics).unwrap();
        assert!(text.starts_with("for i in 0..N {"), "{text}");
    }

    #[test]
    fn defensive_rejection_of_non_constant_bound_at_print_time() {
        // The pre-print validation pass is expected to catch this first in
        // the normal `print_module_noir` path; this exercises the printer's
        // own defensive fallback directly (never a panic either way).
        let loop_expr = expr(IrExprKind::BoundedLoop {
            var: "i".into(),
            start: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
            end: Box::new(expr(IrExprKind::Var("n".into()))),
            inclusive: false,
            body: block(vec![], None),
        });
        let err = print_expr(&loop_expr, "f", &[]).unwrap_err();
        assert!(matches!(err, NoirCodegenError::NonConstantLoopBound { .. }));
    }

    #[test]
    fn bare_type_generic_prints_as_noir_generic() {
        let mut f = function(
            "identity",
            vec![IrParam { name: "x".into(), ty: IrType::TypeParam("T".into()) }],
            Some(IrType::TypeParam("T".into())),
            block(vec![], Some(IrExprKind::Var("x".into()))),
        );
        f.generics = vec![IrGenericParam {
            name: "T".into(),
            kind: volar_compiler::ir::IrGenericParamKind::Type,
            const_ty: None,
            bounds: Vec::new(),
            default: None,
        }];
        let text = print_function(&f).unwrap();
        assert!(text.starts_with("fn identity<T>("), "{text}");
    }

    #[test]
    fn bounded_type_generic_is_rejected_in_v1() {
        let mut f = function("bounded_fn", vec![], None, block(vec![], None));
        f.generics = vec![IrGenericParam {
            name: "T".into(),
            kind: volar_compiler::ir::IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![volar_compiler::ir::IrTraitBound {
                trait_kind: volar_compiler::ir::TraitKind::Custom("SomeTrait".into()),
                type_args: Vec::new(),
                assoc_bindings: Vec::new(),
            }],
            default: None,
        }];
        assert!(print_function(&f).is_err());
    }

    #[test]
    fn const_generic_prints_as_noir_numeric_generic() {
        let mut f = function(
            "make_zero",
            vec![],
            Some(IrType::Primitive(PrimitiveType::U32)),
            block(vec![], Some(IrExprKind::Lit(IrLit::Int(0)))),
        );
        f.generics = vec![IrGenericParam {
            name: "N".into(),
            kind: volar_compiler::ir::IrGenericParamKind::Const,
            const_ty: None,
            bounds: Vec::new(),
            default: None,
        }];
        let text = print_function(&f).unwrap();
        // Noir's numeric-generic syntax requires the `let` keyword and
        // defaults to `u32` when unspecified (confirmed against current
        // Noir docs).
        assert!(text.starts_with("fn make_zero<let N: u32>("), "{text}");
    }

    #[test]
    fn immutable_reference_type_is_transparently_unwrapped() {
        let ty = IrType::Reference { mutable: false, elem: Box::new(IrType::Primitive(PrimitiveType::U32)) };
        assert_eq!(type_to_noir(&ty, "f").unwrap(), "u32");
    }

    #[test]
    fn mutable_reference_type_is_unsupported_in_v1() {
        let ty = IrType::Reference { mutable: true, elem: Box::new(IrType::Primitive(PrimitiveType::U32)) };
        assert!(type_to_noir(&ty, "f").is_err());
    }

    #[test]
    fn array_type_with_const_length() {
        let ty = IrType::Array {
            kind: volar_compiler::ir::ArrayKind::FixedArray,
            elem: Box::new(IrType::Primitive(PrimitiveType::U32)),
            len: volar_compiler::ir::ArrayLength::Const(4),
        };
        assert_eq!(type_to_noir(&ty, "f").unwrap(), "[u32; 4]");
    }

    #[test]
    fn array_type_with_generic_length() {
        let ty = IrType::Array {
            kind: volar_compiler::ir::ArrayKind::FixedArray,
            elem: Box::new(IrType::Primitive(PrimitiveType::U32)),
            len: volar_compiler::ir::ArrayLength::TypeParam("N".into()),
        };
        assert_eq!(type_to_noir(&ty, "f").unwrap(), "[u32; N]");
    }

    #[test]
    fn vector_type_is_unsupported() {
        let ty = IrType::Vector { elem: Box::new(IrType::Primitive(PrimitiveType::U32)) };
        assert!(type_to_noir(&ty, "f").is_err());
    }

    #[test]
    fn plain_struct_type_mapping() {
        let ty = IrType::Struct {
            kind: volar_compiler::ir::StructKind::Custom("Point".into()),
            type_args: Vec::new(),
        };
        assert_eq!(type_to_noir(&ty, "f").unwrap(), "Point");
    }

    #[test]
    fn generic_array_struct_kind_is_unsupported_in_v1() {
        let ty = IrType::Struct {
            kind: volar_compiler::ir::StructKind::GenericArray,
            type_args: vec![IrType::Primitive(PrimitiveType::U8)],
        };
        assert!(type_to_noir(&ty, "f").is_err());
    }

    #[test]
    fn fixed_array_literal_prints_directly() {
        let e = expr(IrExprKind::FixedArray(vec![
            expr(IrExprKind::Lit(IrLit::Int(1))),
            expr(IrExprKind::Lit(IrLit::Int(2))),
            expr(IrExprKind::Lit(IrLit::Int(3))),
        ]));
        assert_eq!(print_expr(&e, "f", &[]).unwrap(), "[1, 2, 3]");
    }

    #[test]
    fn array_variant_prints_same_as_fixed_array() {
        // Real parsed Rust source always produces `Array`, never
        // `FixedArray`, for plain `[a, b, c]` syntax -- see the doc
        // comment on the `Array | FixedArray` match arm.
        let e = expr(IrExprKind::Array(vec![
            expr(IrExprKind::Lit(IrLit::Int(1))),
            expr(IrExprKind::Lit(IrLit::Int(2))),
        ]));
        assert_eq!(print_expr(&e, "f", &[]).unwrap(), "[1, 2]");
    }

    #[test]
    fn index_expr_prints_as_bracket_index() {
        let e = expr(IrExprKind::Index {
            base: Box::new(expr(IrExprKind::Var("arr".into()))),
            index: Box::new(expr(IrExprKind::Lit(IrLit::Int(0)))),
        });
        assert_eq!(print_expr(&e, "f", &[]).unwrap(), "arr[0]");
    }

    #[test]
    fn field_expr_prints_as_dot_access() {
        let e = expr(IrExprKind::Field {
            base: Box::new(expr(IrExprKind::Var("p".into()))),
            field: "x".into(),
        });
        assert_eq!(print_expr(&e, "f", &[]).unwrap(), "p.x");
    }

    #[test]
    fn struct_expr_prints_as_struct_literal() {
        let e = expr(IrExprKind::StructExpr {
            kind: volar_compiler::ir::StructKind::Custom("Point".into()),
            type_args: Vec::new(),
            fields: vec![
                ("x".into(), expr(IrExprKind::Lit(IrLit::Int(1)))),
                ("y".into(), expr(IrExprKind::Lit(IrLit::Int(2)))),
            ],
            rest: None,
        });
        assert_eq!(print_expr(&e, "f", &[]).unwrap(), "Point { x: 1, y: 2 }");
    }

    #[test]
    fn struct_update_syntax_is_unsupported_in_v1() {
        let e = expr(IrExprKind::StructExpr {
            kind: volar_compiler::ir::StructKind::Custom("Point".into()),
            type_args: Vec::new(),
            fields: vec![("x".into(), expr(IrExprKind::Lit(IrLit::Int(1))))],
            rest: Some(Box::new(expr(IrExprKind::Var("other".into())))),
        });
        assert!(print_expr(&e, "f", &[]).is_err());
    }

    #[test]
    fn struct_declaration_prints_fields() {
        let s = volar_compiler::ir::IrStruct {
            kind: volar_compiler::ir::StructKind::Custom("Point".into()),
            module_path: Vec::new(),
            generics: Vec::new(),
            fields: vec![
                volar_compiler::ir::IrField { name: "x".into(), ty: IrType::Primitive(PrimitiveType::U32), public: true },
                volar_compiler::ir::IrField { name: "y".into(), ty: IrType::Primitive(PrimitiveType::U32), public: true },
            ],
            is_tuple: false,
            native_volar_type: None,
            derives: Vec::new(),
        };
        let text = print_struct(&s).unwrap();
        assert!(text.starts_with("struct Point {"), "{text}");
        assert!(text.contains("x: u32,"), "{text}");
        assert!(text.contains("y: u32,"), "{text}");
    }

    fn galois_add_method() -> IrFunction {
        IrFunction {
            name: "add".into(),
            module_path: Vec::new(),
            generics: Vec::new(),
            receiver: Some(volar_compiler::ir::IrReceiver::Value),
            params: vec![IrParam {
                name: "other".into(),
                ty: IrType::Struct { kind: volar_compiler::ir::StructKind::Custom("Self".into()), type_args: Vec::new() },
            }],
            return_type: Some(IrType::Struct { kind: volar_compiler::ir::StructKind::Custom("Self".into()), type_args: Vec::new() }),
            where_clause: Vec::new(),
            body: block(vec![], Some(IrExprKind::Var("other".into()))),
            external_kind: ExternalKind::Normal,
            no_inline: false,
        }
    }

    #[test]
    fn trait_impl_prints_method_with_self_receiver() {
        let imp = volar_compiler::ir::IrImpl {
            generics: Vec::new(),
            trait_: Some(volar_compiler::ir::IrTraitRef {
                kind: volar_compiler::ir::TraitKind::Math(volar_compiler::ir::MathTrait::Add),
                type_args: Vec::new(),
            }),
            self_ty: IrType::Struct { kind: volar_compiler::ir::StructKind::Custom("Galois".into()), type_args: Vec::new() },
            where_clause: Vec::new(),
            items: vec![volar_compiler::ir::IrImplItem::Method(galois_add_method())],
        };
        let text = print_impl(&imp).unwrap();
        assert!(text.starts_with("impl Add for Galois {"), "{text}");
        assert!(text.contains("fn add(self, other: Self)"), "{text}");
    }

    #[test]
    fn inherent_impl_has_no_trait_header() {
        let imp = volar_compiler::ir::IrImpl {
            generics: Vec::new(),
            trait_: None,
            self_ty: IrType::Struct { kind: volar_compiler::ir::StructKind::Custom("Galois".into()), type_args: Vec::new() },
            where_clause: Vec::new(),
            items: vec![volar_compiler::ir::IrImplItem::Method(galois_add_method())],
        };
        let text = print_impl(&imp).unwrap();
        assert!(text.starts_with("impl Galois {"), "{text}");
    }

    #[test]
    fn non_math_trait_impl_is_unsupported_in_v1() {
        let imp = volar_compiler::ir::IrImpl {
            generics: Vec::new(),
            trait_: Some(volar_compiler::ir::IrTraitRef {
                kind: volar_compiler::ir::TraitKind::Custom("Digest".into()),
                type_args: Vec::new(),
            }),
            self_ty: IrType::Struct { kind: volar_compiler::ir::StructKind::Custom("Hasher".into()), type_args: Vec::new() },
            where_clause: Vec::new(),
            items: Vec::new(),
        };
        assert!(print_impl(&imp).is_err());
    }

    #[test]
    fn ref_mut_receiver_is_unsupported_in_v1() {
        let mut f = galois_add_method();
        f.receiver = Some(volar_compiler::ir::IrReceiver::RefMut);
        assert!(print_function(&f).is_err());
    }

    #[test]
    fn ref_receiver_prints_as_ampersand_self() {
        let mut f = galois_add_method();
        f.receiver = Some(volar_compiler::ir::IrReceiver::Ref);
        let text = print_function(&f).unwrap();
        assert!(text.contains("fn add(&self, other: Self)"), "{text}");
    }

    #[test]
    fn other_method_call_prints_as_dot_call() {
        let e = expr(IrExprKind::MethodCall {
            receiver: Box::new(expr(IrExprKind::Var("g1".into()))),
            method: volar_compiler::ir::MethodKind::Other("add".into()),
            type_args: Vec::new(),
            args: vec![expr(IrExprKind::Var("g2".into()))],
        });
        assert_eq!(print_expr(&e, "f", &[]).unwrap(), "g1.add(g2)");
    }

    #[test]
    fn known_std_method_call_is_unsupported_in_v1() {
        let e = expr(IrExprKind::MethodCall {
            receiver: Box::new(expr(IrExprKind::Var("arr".into()))),
            method: volar_compiler::ir::MethodKind::Known(volar_compiler::ir::StdMethod::Len),
            type_args: Vec::new(),
            args: Vec::new(),
        });
        assert!(print_expr(&e, "f", &[]).is_err());
    }

    #[test]
    fn use_ops_import_is_emitted_only_when_needed() {
        let module_with_ops = IrModule {
            name: "m".into(),
            impls: vec![volar_compiler::ir::IrImpl {
                generics: Vec::new(),
                trait_: Some(volar_compiler::ir::IrTraitRef {
                    kind: volar_compiler::ir::TraitKind::Math(volar_compiler::ir::MathTrait::Add),
                    type_args: Vec::new(),
                }),
                self_ty: IrType::Struct { kind: volar_compiler::ir::StructKind::Custom("Galois".into()), type_args: Vec::new() },
                where_clause: Vec::new(),
                items: vec![volar_compiler::ir::IrImplItem::Method(galois_add_method())],
            }],
            ..Default::default()
        };
        let text = print_module_noir(&module_with_ops).unwrap();
        assert!(text.starts_with("use std::ops::{Add};"), "{text}");

        let module_without_ops = IrModule { name: "m".into(), ..Default::default() };
        let text = print_module_noir(&module_without_ops).unwrap();
        assert!(!text.contains("use std::ops"), "{text}");
    }
}
