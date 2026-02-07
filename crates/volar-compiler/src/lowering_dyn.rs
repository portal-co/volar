//! Dynamic IR Lowering
//!
//! This module transforms the specialized volar-spec IR into a dynamic IR
//! where type-level lengths become runtime usize witnesses.

use crate::const_analysis::*;
use crate::ir::*;

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(feature = "std")]
use std::string::String;

#[cfg(feature = "std")]
use std::{boxed::Box, collections::BTreeMap, format, string::ToString, vec, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::BTreeMap, format, string::ToString, vec, vec::Vec};

/// Information about a struct's witness fields
#[derive(Debug, Clone, Default)]
pub struct StructInfo {
    pub kind: StructKind,
    /// Names of lifetime parameters
    pub lifetimes: Vec<String>,
    /// Names of length witness fields (lowercase)
    pub length_witnesses: Vec<String>,
    /// Names of type parameters that remain generic (name, bounds_string)
    pub type_params: Vec<(String, Vec<IrTraitBound>)>,
    /// Original generics in declaration order with their kind (Length/Type) and param kind
    pub orig_generics: Vec<(String, GenericKind, IrGenericParamKind)>,
    /// Defaults for generics (None if no default)
    pub generic_defaults: Vec<Option<IrType>>,
    /// Whether a manual `Clone` impl exists for this struct
    pub manual_clone: bool,
    /// Whether the lowered struct needs a _phantom field for unused type params
    pub needs_phantom: bool,
}
/// Context for lowering
pub struct LoweringContext {
    pub struct_info: BTreeMap<String, StructInfo>,
    /// Trait names that are length aliases (e.g., VoleArray: ArrayLength).
    pub length_aliases: Vec<String>,
}

impl LoweringContext {
    /// Get length aliases as string slices for use with `classify_generic_with_aliases`.
    fn aliases(&self) -> Vec<&str> {
        self.length_aliases.iter().map(|s| s.as_str()).collect()
    }
}

impl LoweringContext {
    pub fn new(module: &IrModule) -> Self {
        Self::new_with_deps(module, &[])
    }

    /// Build lowering context from a module and its dependency manifests.
    ///
    /// Dependency structs are registered so that lowering can see their
    /// generic structure (lengths vs type params, etc.) when generating
    /// dynamic code that references them.
    pub fn new_with_deps(module: &IrModule, deps: &[crate::manifest::TypeManifest]) -> Self {
        let mut struct_info = BTreeMap::new();

        // Discover length-alias traits (e.g., VoleArray: ArrayLength → length alias).
        // Fixed-point iteration to handle transitive chains.
        let mut length_aliases: Vec<String> = Vec::new();
        let mut changed = true;
        while changed {
            changed = false;
            for t in &module.traits {
                if let TraitKind::Custom(name) = &t.kind {
                    if length_aliases.contains(name) {
                        continue;
                    }
                    let aliases_ref: Vec<&str> = length_aliases.iter().map(|s| s.as_str()).collect();
                    let is_alias = t.super_traits.iter().any(|st| {
                        crate::const_analysis::is_length_bound_with_aliases(st, &aliases_ref)
                    });
                    if is_alias {
                        length_aliases.push(name.clone());
                        changed = true;
                    }
                }
            }
        }
        let aliases_ref: Vec<&str> = length_aliases.iter().map(|s| s.as_str()).collect();

        // Register dependency structs first
        for dep in deps {
            for s in &dep.module.structs {
                let mut info = StructInfo::default();
                info.kind = s.kind.clone();
                for p in &s.generics {
                    let kind = if p.kind == IrGenericParamKind::Lifetime {
                        GenericKind::Type
                    } else {
                        classify_generic_with_aliases(p, &[&s.generics], &aliases_ref)
                    };
                    info.orig_generics
                        .push((p.name.clone(), kind.clone(), p.kind.clone()));
                    info.generic_defaults.push(p.default.clone());
                    if p.kind == IrGenericParamKind::Lifetime {
                        info.lifetimes.push(p.name.clone());
                    } else {
                        match kind {
                            GenericKind::Length => info.length_witnesses.push(p.name.to_lowercase()),
                            GenericKind::Type => {
                                info.type_params.push((p.name.clone(), p.bounds.clone()));
                            }
                        }
                    }
                }
                // Don't overwrite if already registered by an earlier dep
                struct_info.entry(s.kind.to_string()).or_insert(info);
            }
        }

        // Pass 1: Basic info from the module's own structs (overrides deps)
        for s in &module.structs {
            let mut info = StructInfo::default();
            info.kind = s.kind.clone();

            for p in &s.generics {
                let kind = if p.kind == IrGenericParamKind::Lifetime {
                    GenericKind::Type
                } else {
                    classify_generic_with_aliases(p, &[&s.generics], &aliases_ref)
                };

                info.orig_generics
                    .push((p.name.clone(), kind.clone(), p.kind.clone()));
                info.generic_defaults.push(p.default.clone());

                if p.kind == IrGenericParamKind::Lifetime {
                    info.lifetimes.push(p.name.clone());
                } else {
                    match kind {
                        GenericKind::Length => info.length_witnesses.push(p.name.to_lowercase()),
                        GenericKind::Type => {
                            info.type_params.push((p.name.clone(), p.bounds.clone()));
                        }
                    }
                }
            }
            struct_info.insert(s.kind.to_string(), info);
        }

        // Pass 2: Determine which structs need phantom data
        for s in &module.structs {
            let kind_str = s.kind.to_string();
            if let Some(info) = struct_info.get_mut(&kind_str) {
                // Collect type params used in fields
                let used_type_params: Vec<String> = s
                    .fields
                    .iter()
                    .flat_map(|f| collect_type_params(&f.ty))
                    .collect();

                // Check if any type params are unused
                let unused_params: Vec<_> = info
                    .type_params
                    .iter()
                    .filter(|(name, _)| !used_type_params.contains(name))
                    .collect();

                info.needs_phantom = !unused_params.is_empty();
            }
        }

        // Pass 3: Manual clones
        for im in &module.impls {
            if let IrType::Struct { kind, .. } = &im.self_ty {
                if let Some(tr) = &im.trait_ {
                    if let TraitKind::Math(MathTrait::Clone) = &tr.kind {
                        if let Some(entry) = struct_info.get_mut(&kind.to_string()) {
                            entry.manual_clone = true;
                        }
                    }
                }
            }
        }

        Self { struct_info, length_aliases }
    }
}

pub fn lower_module_dyn(module: &IrModule) -> IrModule {
    let ctx = LoweringContext::new(module);
    let mut lowered = IrModule {
        name: format!("{}_dyn", module.name),
        ..Default::default()
    };

    for s in &module.structs {
        lowered.structs.push(lower_struct_dyn(s, &ctx));
    }

    // Emit custom traits (LengthDoubler, PuncturableLengthDoubler, VoleArray, etc.)
    for t in &module.traits {
        if let TraitKind::Custom(_) = &t.kind {
            // Skip VoleArray — it's just an alias for ArrayLength
            if let TraitKind::Custom(name) = &t.kind {
                if name == "VoleArray" {
                    continue;
                }
            }
            lowered.traits.push(lower_trait_dyn(t, &ctx));
        }
    }

    for im in &module.impls {
        // Skip blanket impls for marker/alias traits that don't apply in dynamic context
        if let Some(tr) = &im.trait_ {
            if let TraitKind::Custom(name) = &tr.kind {
                // Skip blanket impls where self_ty is a type param (e.g., impl<T: ...> Foo for T)
                if let IrType::TypeParam(_) = &im.self_ty {
                    if name == "ByteBlockEncrypt" {
                        continue;
                    }
                }
                // Skip marker trait impls (VoleArray is an alias for ArrayLength)
                if name == "VoleArray" {
                    continue;
                }
            }
        }
        lowered.impls.push(lower_impl_dyn(im, &ctx));
    }

    for f in &module.functions {
        lowered
            .functions
            .push(lower_function_dyn(f, &ctx, None, None, &BTreeMap::new(), false));
    }

    lowered
}

/// Extract constant witness values from a type (e.g., K=U0 in Vope<N, T, U0>)
fn extract_constant_witnesses(ty: &IrType, ctx: &LoweringContext) -> BTreeMap<String, usize> {
    let mut result = BTreeMap::new();
    if let IrType::Struct { kind, type_args } = ty {
        let kind_str = kind.to_string();
        if let Some(info) = ctx.struct_info.get(&kind_str) {
            let mut type_arg_idx = 0;
            for (name, gkind, pkind) in &info.orig_generics {
                if *pkind == IrGenericParamKind::Lifetime {
                    continue;
                }
                if *gkind == GenericKind::Length {
                    let lowered_name = name.to_lowercase();
                    if let Some(ta) = type_args.get(type_arg_idx) {
                        if let IrType::TypeParam(p) = ta {
                            if let Some(tn) = TypeNumConst::from_str(p) {
                                result.insert(lowered_name, tn.to_usize());
                            }
                        }
                    }
                }
                type_arg_idx += 1;
            }
        }
    }
    result
}

fn lower_trait_dyn(t: &crate::ir::IrTrait, ctx: &LoweringContext) -> crate::ir::IrTrait {
    use crate::ir::{IrTrait, IrTraitItem, IrMethodSig};
    let empty_gen: Vec<IrGenericParam> = Vec::new();
    IrTrait {
        kind: t.kind.clone(),
        generics: lower_generics_dyn(&t.generics, &[], ctx),
        super_traits: t
            .super_traits
            .iter()
            .filter_map(|st| lower_trait_bound_dyn(st, &empty_gen, &empty_gen))
            .collect(),
        items: t
            .items
            .iter()
            .map(|item| match item {
                IrTraitItem::Method(sig) => IrTraitItem::Method(IrMethodSig {
                    name: sig.name.clone(),
                    generics: lower_generics_dyn(&sig.generics, &t.generics, ctx),
                    receiver: sig.receiver.clone(),
                    params: sig
                        .params
                        .iter()
                        .map(|p| IrParam {
                            name: p.name.clone(),
                            ty: lower_type_dyn(&p.ty, ctx, &empty_gen),
                        })
                        .collect(),
                    return_type: sig
                        .return_type
                        .as_ref()
                        .map(|rt| lower_type_dyn(rt, ctx, &empty_gen)),
                    where_clause: Vec::new(),
                }),
                IrTraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                } => {
                    // For associated types that had length bounds (ArrayLength, Unsigned),
                    // keep Unsigned so that ::to_usize() works at projection sites.
                    let had_length_bound = bounds.iter().any(|b| {
                        is_length_bound(b) || matches!(&b.trait_kind, TraitKind::Math(MathTrait::Unsigned))
                            || matches!(&b.trait_kind, TraitKind::Custom(n) if n == "ArrayLength")
                    });
                    let mut new_bounds: Vec<_> = bounds
                        .iter()
                        .filter_map(|b| lower_trait_bound_dyn(b, &empty_gen, &empty_gen))
                        .collect();
                    if had_length_bound && !new_bounds.iter().any(|b| matches!(&b.trait_kind, TraitKind::Math(MathTrait::Unsigned))) {
                        new_bounds.push(IrTraitBound {
                            trait_kind: TraitKind::Math(MathTrait::Unsigned),
                            type_args: Vec::new(),
                            assoc_bindings: Vec::new(),
                        });
                    }
                    IrTraitItem::AssociatedType {
                        name: name.clone(),
                        bounds: new_bounds,
                        default: default
                            .as_ref()
                            .map(|d| lower_type_dyn(d, ctx, &empty_gen)),
                    }
                },
            })
            .collect(),
    }
}

fn lower_struct_dyn(s: &IrStruct, ctx: &LoweringContext) -> IrStruct {
    let info = ctx.struct_info.get(&s.kind.to_string()).unwrap();
    let mut fields = Vec::new();

    // Don't add lifetimes since we convert &[T] to Vec<T>
    let generics = lower_generics_dyn(&s.generics, &[], ctx);

    // Add length witnesses as public fields
    for w in &info.length_witnesses {
        fields.push(IrField {
            name: w.clone(),
            ty: IrType::Primitive(PrimitiveType::Usize),
            public: true,
        });
    }

    // Transform existing fields
    for f in s.fields.iter() {
        fields.push(IrField {
            name: f.name.clone(),
            ty: lower_type_dyn_for_field(&f.ty, ctx, &[]),
            public: f.public,
        });
    }

    // Check for unused type params and add PhantomData
    let used_type_params: Vec<String> = fields
        .iter()
        .flat_map(|f| collect_type_params(&f.ty))
        .collect();

    let mut phantom_types = Vec::new();
    for g in &generics {
        if g.kind == IrGenericParamKind::Type && !used_type_params.contains(&g.name) {
            phantom_types.push(g.name.clone());
        }
    }

    if !phantom_types.is_empty() {
        // Create PhantomData<(T1, T2, ...)> field
        let phantom_ty = if phantom_types.len() == 1 {
            IrType::TypeParam(phantom_types[0].clone())
        } else {
            IrType::Tuple(
                phantom_types
                    .iter()
                    .map(|n| IrType::TypeParam(n.clone()))
                    .collect(),
            )
        };
        fields.push(IrField {
            name: "_phantom".to_string(),
            ty: IrType::Struct {
                kind: StructKind::Custom("PhantomData".to_string()),
                type_args: vec![phantom_ty],
            },
            public: false,
        });
    }

    IrStruct {
        kind: StructKind::from_str(&format!("{}Dyn", s.kind)),
        generics,
        fields,
        is_tuple: s.is_tuple,
    }
}

fn collect_type_params(ty: &IrType) -> Vec<String> {
    match ty {
        IrType::TypeParam(name) => vec![name.clone()],
        IrType::Vector { elem } => collect_type_params(elem),
        IrType::Array { elem, .. } => collect_type_params(elem),
        IrType::Struct { type_args, .. } => {
            type_args.iter().flat_map(collect_type_params).collect()
        }
        IrType::Reference { elem, .. } => collect_type_params(elem),
        IrType::Tuple(elems) => elems.iter().flat_map(collect_type_params).collect(),
        _ => Vec::new(),
    }
}

fn lower_impl_dyn(im: &IrImpl, ctx: &LoweringContext) -> IrImpl {
    let mut items = Vec::new();
    let self_kind = match &im.self_ty {
        IrType::Struct { kind, .. } => kind.to_string(),
        _ => String::new(),
    };

    // Extract constant witness values from self_ty (e.g., K=U0 in impl Vope<N, T, U0>)
    let constant_witnesses = extract_constant_witnesses(&im.self_ty, ctx);

    let mut impl_gen = im.generics.clone();
    // Add where-clause bounds to impl_gen for classification
    for wp in &im.where_clause {
        match wp {
            IrWherePredicate::TypeBound {
                ty: IrType::TypeParam(name),
                bounds,
            } => {
                if let Some(p) = impl_gen.iter_mut().find(|p| &p.name == name) {
                    p.bounds.extend(bounds.clone());
                }
            }
            _ => {}
        }
    }

    for item in &im.items {
        match item {
            IrImplItem::Method(f) => {
                items.push(IrImplItem::Method(lower_function_dyn(
                    f,
                    ctx,
                    Some(&self_kind),
                    Some(&impl_gen),
                    &constant_witnesses,
                    im.trait_.is_some(),
                )));
            }
            IrImplItem::AssociatedType { name, ty } => {
                items.push(IrImplItem::AssociatedType {
                    name: name.clone(),
                    ty: lower_type_dyn(ty, ctx, &impl_gen),
                });
            }
        }
    }

    IrImpl {
        generics: lower_generics_dyn(&impl_gen, &[], ctx),
        trait_: im.trait_.as_ref().map(|tr| IrTraitRef {
            kind: tr.kind.clone(),
            type_args: tr
                .type_args
                .iter()
                .map(|ta| lower_type_dyn(ta, ctx, &impl_gen))
                .collect(),
        }),
        self_ty: lower_type_dyn(&im.self_ty, ctx, &impl_gen),
        where_clause: lower_where_clause_dyn(&im.where_clause, ctx, &impl_gen),
        items,
    }
}

fn lower_function_dyn(
    f: &IrFunction,
    ctx: &LoweringContext,
    self_struct: Option<&str>,
    impl_generics: Option<&[IrGenericParam]>,
    constant_witnesses: &BTreeMap<String, usize>,
    is_trait_impl: bool,
) -> IrFunction {
    let mut params = Vec::new();
    let empty_gen = Vec::new();
    let impl_gen = impl_generics.unwrap_or(&empty_gen);

    let mut fn_gen = f.generics.clone();
    for wp in &f.where_clause {
        match wp {
            IrWherePredicate::TypeBound {
                ty: IrType::TypeParam(name),
                bounds,
            } => {
                if let Some(p) = fn_gen.iter_mut().find(|p| &p.name == name) {
                    p.bounds.extend(bounds.clone());
                }
            }
            _ => {}
        }
    }

    // Length generics become parameters (skip for trait impl methods — values from self)
    if !is_trait_impl {
        for p in &fn_gen {
            let kind = classify_generic_with_aliases(p, &[&fn_gen, impl_gen], &ctx.aliases());
            if kind == GenericKind::Length {
                let name = p.name.to_lowercase();
                if !params.iter().any(|param: &IrParam| param.name == name) {
                    params.push(IrParam {
                        name,
                        ty: IrType::Primitive(PrimitiveType::Usize),
                    });
                }
            }
        }
    }

    // Static methods also need impl-level length params
    if f.receiver.is_none() && !is_trait_impl {
        for p in impl_gen {
            let kind = classify_generic_with_aliases(p, &[impl_gen], &ctx.aliases());
            if kind == GenericKind::Length {
                let name = p.name.to_lowercase();
                if !params.iter().any(|param: &IrParam| param.name == name) {
                    params.push(IrParam {
                        name,
                        ty: IrType::Primitive(PrimitiveType::Usize),
                    });
                }
            }
        }
    }

    // (Projection witnesses like B::OutputSize are kept static — resolved at
    // compile time from type params, not injected as runtime params.)

    for p in &f.params {
        params.push(IrParam {
            name: p.name.clone(),
            ty: lower_type_dyn(&p.ty, ctx, &fn_gen),
        });
    }

    let mut body = lower_block_dyn(
        &f.body,
        ctx,
        &fn_gen
            .iter()
            .cloned()
            .chain(impl_gen.iter().cloned())
            .collect::<Vec<_>>(),
    );

    // Unpack witnesses in methods
    if f.receiver.is_some() {
        if let Some(sname) = self_struct {
            if let Some(info) = ctx.struct_info.get(sname) {
                let mut unpacks = Vec::new();
                // Collect param names to detect conflicts with field bindings
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                for w in &info.length_witnesses {
                    // If a field binding would shadow a parameter, rename the parameter
                    if let Some(param) = params.iter_mut().find(|p| &p.name == w) {
                        let renamed = format!("{}_param", w);
                        let old_name = param.name.clone();
                        param.name = renamed.clone();
                        // Rewrite references in the body from old_name to renamed
                        rename_var_in_block(&mut body, &old_name, &renamed);
                    }
                    unpacks.push(IrStmt::Let {
                        pattern: IrPattern::Ident {
                            mutable: false,
                            name: w.clone(),
                            subpat: None,
                        },
                        ty: Some(IrType::Primitive(PrimitiveType::Usize)),
                        init: Some(IrExpr::Field {
                            base: Box::new(IrExpr::Var("self".to_string())),
                            field: w.clone(),
                        }),
                    });
                }
                unpacks.extend(body.stmts);
                body.stmts = unpacks;
            }
        }
    } else {
        // For static methods, inject constant witnesses as local variables
        if !constant_witnesses.is_empty() {
            let mut bindings = Vec::new();
            for (name, value) in constant_witnesses {
                // Only add if not already a parameter
                if !params.iter().any(|p| &p.name == name) {
                    bindings.push(IrStmt::Let {
                        pattern: IrPattern::Ident {
                            mutable: false,
                            name: name.clone(),
                            subpat: None,
                        },
                        ty: Some(IrType::Primitive(PrimitiveType::Usize)),
                        init: Some(IrExpr::Lit(IrLit::Int(*value as i128))),
                    });
                }
            }
            bindings.extend(body.stmts);
            body.stmts = bindings;
        }
    }

    // Combine impl generics and function generics for where clause context
    // Only include params that would survive lowering (non-Length, non-Lifetime)
    let mut combined_gen: Vec<IrGenericParam> = Vec::new();
    for p in fn_gen.iter() {
        let kind = classify_generic_with_aliases(p, &[&fn_gen, impl_gen], &ctx.aliases());
        if p.kind != IrGenericParamKind::Lifetime && kind != GenericKind::Length {
            combined_gen.push(p.clone());
        }
    }
    for p in impl_gen {
        let kind = classify_generic_with_aliases(p, &[impl_gen, &fn_gen], &ctx.aliases());
        if p.kind != IrGenericParamKind::Lifetime && kind != GenericKind::Length {
            if !combined_gen.iter().any(|cp| cp.name == p.name) {
                combined_gen.push(p.clone());
            }
        }
    }


    IrFunction {
        name: f.name.clone(),
        generics: lower_generics_dyn(&f.generics, impl_gen, ctx),
        receiver: f.receiver,
        params,
        return_type: f
            .return_type
            .as_ref()
            .map(|rt| lower_type_dyn(rt, ctx, &fn_gen)),
        where_clause: lower_where_clause_dyn(&f.where_clause, ctx, &combined_gen),
        body,
    }
}

fn lower_generics_dyn(
    generics: &[IrGenericParam],
    extra_ctx: &[IrGenericParam],
    ctx: &LoweringContext,
) -> Vec<IrGenericParam> {
    let aliases = ctx.aliases();
    generics
        .iter()
        .filter(|p| {
            let kind = classify_generic_with_aliases(p, &[generics, extra_ctx], &aliases);
            p.kind != IrGenericParamKind::Lifetime && kind != GenericKind::Length
        })
        .map(|p| {
            let mut p = p.clone();
            p.bounds = p
                .bounds
                .iter()
                .filter_map(|b| lower_trait_bound_dyn(b, generics, extra_ctx))
                .collect();
            p
        })
        .collect()
}

fn lower_trait_bound_dyn(
    b: &IrTraitBound,
    _generics: &[IrGenericParam],
    _extra_ctx: &[IrGenericParam],
) -> Option<IrTraitBound> {
    if is_length_bound(b) {
        return None;
    }
    match &b.trait_kind {
        TraitKind::Math(MathTrait::Unsigned) => return None,
        TraitKind::Custom(name) if name == "ArrayLength" => return None,
        TraitKind::Custom(name) if name == "VoleArray" => return None,
        _ => {}
    }
    let b = b.clone();
    Some(b)
}

fn lower_type_dyn(ty: &IrType, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrType {
    lower_type_dyn_inner(ty, ctx, fn_gen, false)
}

fn lower_type_dyn_for_field(
    ty: &IrType,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> IrType {
    lower_type_dyn_inner(ty, ctx, fn_gen, true)
}

fn lower_type_dyn_inner(
    ty: &IrType,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
    in_struct_field: bool,
) -> IrType {
    match ty {
        IrType::Array { kind, elem, len } => {
            if *kind == ArrayKind::Slice || *kind == ArrayKind::FixedArray {
                IrType::Array {
                    kind: *kind,
                    elem: Box::new(lower_type_dyn_inner(elem, ctx, fn_gen, in_struct_field)),
                    len: len.clone(),
                }
            } else {
                IrType::Vector {
                    elem: Box::new(lower_type_dyn_inner(elem, ctx, fn_gen, in_struct_field)),
                }
            }
        }
        IrType::Struct { kind, type_args } => {
            let mut lowered_args = Vec::new();
            let kind_str = kind.to_string();
            if let Some(info) = ctx.struct_info.get(&kind_str) {
                let non_lifetime_generics: Vec<_> = info
                    .orig_generics
                    .iter()
                    .filter(|(_, _, pk)| *pk != IrGenericParamKind::Lifetime)
                    .collect();

                for (idx, arg) in type_args.iter().enumerate() {
                    if let Some((_, gkind, _)) = non_lifetime_generics.get(idx) {
                        if *gkind == GenericKind::Length {
                            continue;
                        }
                    }
                    lowered_args.push(lower_type_dyn_inner(arg, ctx, fn_gen, in_struct_field));
                }
            } else {
                lowered_args = type_args
                    .iter()
                    .map(|ta| lower_type_dyn_inner(ta, ctx, fn_gen, in_struct_field))
                    .collect();
            }

            let new_kind = if kind_str == "GenericArray"
                || kind_str == "Option"
                || kind_str == "Result"
                || kind_str == "Box"
                || kind_str == "Vec"
            {
                kind.clone()
            } else {
                StructKind::from_str(&format!("{}Dyn", kind))
            };

            IrType::Struct {
                kind: new_kind,
                type_args: lowered_args,
            }
        }
        IrType::Reference { mutable, elem } => {
            // In struct fields, convert &[T] to Vec<T> to avoid lifetime issues
            if in_struct_field {
                if let IrType::Array {
                    kind: ArrayKind::Slice,
                    elem: inner,
                    ..
                } = elem.as_ref()
                {
                    return IrType::Vector {
                        elem: Box::new(lower_type_dyn_inner(inner, ctx, fn_gen, in_struct_field)),
                    };
                }
            }
            IrType::Reference {
                mutable: *mutable,
                elem: Box::new(lower_type_dyn_inner(elem, ctx, fn_gen, in_struct_field)),
            }
        }
        IrType::Tuple(elems) => IrType::Tuple(
            elems
                .iter()
                .map(|e| lower_type_dyn_inner(e, ctx, fn_gen, in_struct_field))
                .collect(),
        ),
        IrType::TypeParam(p) => IrType::TypeParam(p.clone()),
        _ => ty.clone(),
    }
}

fn lower_where_clause_dyn(
    wc: &[IrWherePredicate],
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> Vec<IrWherePredicate> {
    // Collect all known type params from fn_gen
    let known_params: Vec<String> = fn_gen
        .iter()
        .filter(|p| p.kind != IrGenericParamKind::Lifetime)
        .map(|p| p.name.clone())
        .collect();

    wc.iter()
        .filter_map(|wp| {
            match wp {
                IrWherePredicate::TypeBound { ty, bounds } => {
                    let ty = lower_type_dyn(ty, ctx, fn_gen);

                    // Skip predicates for type params that look like lengths
                    if let IrType::TypeParam(p) = &ty {
                        if p.len() == 1 && p.chars().next().unwrap().is_uppercase() {
                            if !p.starts_with('T') && !p.starts_with('B') && !p.starts_with('D') {
                                return None;
                            }
                        }
                        // Skip if the type param is not in known params (undefined)
                        if !known_params.contains(p) {
                            return None;
                        }
                    }

                    // Skip predicates with projections that have unknown traits
                    if has_unresolved_projection(&ty) {
                        return None;
                    }

                    let bounds: Vec<_> = bounds
                        .iter()
                        .filter_map(|b| lower_trait_bound_dyn(b, fn_gen, &[]))
                        .collect();
                    if bounds.is_empty() {
                        return None;
                    }

                    // Check if any bound references undefined types
                    for b in &bounds {
                        if has_unresolved_projection_in_bound(b) {
                            return None;
                        }
                        // Also check for undefined type params in bounds
                        if has_undefined_type_params_in_bound(b, &known_params) {
                            return None;
                        }
                    }

                    Some(IrWherePredicate::TypeBound { ty, bounds })
                }
            }
        })
        .collect()
}

fn has_unresolved_projection(ty: &IrType) -> bool {
    match ty {
        IrType::Projection { trait_path, .. } => trait_path.is_none(),
        IrType::Vector { elem } | IrType::Reference { elem, .. } => has_unresolved_projection(elem),
        IrType::Array { elem, .. } => has_unresolved_projection(elem),
        IrType::Struct { type_args, .. } => type_args.iter().any(has_unresolved_projection),
        IrType::Tuple(elems) => elems.iter().any(has_unresolved_projection),
        _ => false,
    }
}

fn has_unresolved_projection_in_bound(b: &IrTraitBound) -> bool {
    b.type_args.iter().any(has_unresolved_projection)
}

fn has_undefined_type_params_in_bound(b: &IrTraitBound, known_params: &[String]) -> bool {
    for arg in &b.type_args {
        if has_undefined_type_param(arg, known_params) {
            return true;
        }
    }
    // Check Output type in associated type bindings if any
    // The output type is embedded in IrTraitBound via special handling
    false
}

fn has_undefined_type_param(ty: &IrType, known_params: &[String]) -> bool {
    match ty {
        IrType::TypeParam(p) => {
            // Single uppercase letter that's not a known param
            if p.len() == 1 && p.chars().next().unwrap().is_uppercase() {
                !known_params.contains(p)
            } else {
                false
            }
        }
        IrType::Struct { type_args, .. } => type_args
            .iter()
            .any(|a| has_undefined_type_param(a, known_params)),
        IrType::Reference { elem, .. } | IrType::Vector { elem } => {
            has_undefined_type_param(elem, known_params)
        }
        IrType::Tuple(elems) => elems
            .iter()
            .any(|e| has_undefined_type_param(e, known_params)),
        IrType::Array { elem, .. } => has_undefined_type_param(elem, known_params),
        _ => false,
    }
}

/// Rename all occurrences of `Var(old)` to `Var(new_name)` in a block, recursively.
/// Also renames references in IrPattern::Ident if they match.
fn rename_var_in_block(block: &mut IrBlock, old: &str, new_name: &str) {
    for stmt in &mut block.stmts {
        rename_var_in_stmt(stmt, old, new_name);
    }
    if let Some(e) = &mut block.expr {
        rename_var_in_expr(e, old, new_name);
    }
}

fn rename_var_in_stmt(stmt: &mut IrStmt, old: &str, new_name: &str) {
    match stmt {
        IrStmt::Let { pattern, init, .. } => {
            // Don't rename the binding itself — if the let introduces a new var
            // with the same name, it shadows.
            if let Some(e) = init {
                rename_var_in_expr(e, old, new_name);
            }
        }
        IrStmt::Semi(e) | IrStmt::Expr(e) => {
            rename_var_in_expr(e, old, new_name);
        }
    }
}

fn rename_var_in_expr(expr: &mut IrExpr, old: &str, new_name: &str) {
    match expr {
        IrExpr::Var(v) => {
            if v == old { *v = new_name.to_string(); }
        }
        IrExpr::Binary { left, right, .. } => {
            rename_var_in_expr(left, old, new_name);
            rename_var_in_expr(right, old, new_name);
        }
        IrExpr::Unary { expr: e, .. } => {
            rename_var_in_expr(e, old, new_name);
        }
        IrExpr::Field { base, .. } => {
            rename_var_in_expr(base, old, new_name);
        }
        IrExpr::Index { base, index } => {
            rename_var_in_expr(base, old, new_name);
            rename_var_in_expr(index, old, new_name);
        }
        IrExpr::Call { func, args } => {
            rename_var_in_expr(func, old, new_name);
            for a in args { rename_var_in_expr(a, old, new_name); }
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            rename_var_in_expr(receiver, old, new_name);
            for a in args { rename_var_in_expr(a, old, new_name); }
        }
        IrExpr::Block(b) => {
            rename_var_in_block(b, old, new_name);
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            rename_var_in_expr(cond, old, new_name);
            rename_var_in_block(then_branch, old, new_name);
            if let Some(eb) = else_branch { rename_var_in_expr(eb, old, new_name); }
        }
        IrExpr::StructExpr { fields, rest, .. } => {
            for (_, e) in fields { rename_var_in_expr(e, old, new_name); }
            if let Some(r) = rest { rename_var_in_expr(r, old, new_name); }
        }
        IrExpr::Array(elems) | IrExpr::Tuple(elems) => {
            for e in elems { rename_var_in_expr(e, old, new_name); }
        }
        IrExpr::Cast { expr: e, .. } | IrExpr::Try(e) => {
            rename_var_in_expr(e, old, new_name);
        }
        IrExpr::Return(Some(e)) | IrExpr::Break(Some(e)) => {
            rename_var_in_expr(e, old, new_name);
        }
        IrExpr::Closure { body, .. } => {
            rename_var_in_expr(body, old, new_name);
        }
        IrExpr::Assign { left, right } | IrExpr::AssignOp { left, right, .. } => {
            rename_var_in_expr(left, old, new_name);
            rename_var_in_expr(right, old, new_name);
        }
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start { rename_var_in_expr(s, old, new_name); }
            if let Some(e) = end { rename_var_in_expr(e, old, new_name); }
        }
        IrExpr::IterPipeline(chain) => {
            rename_var_in_iter_chain(chain, old, new_name);
        }
        IrExpr::ArrayGenerate { body, .. } => {
            rename_var_in_expr(body, old, new_name);
        }
        IrExpr::BoundedLoop { start, end, body, .. } => {
            rename_var_in_expr(start, old, new_name);
            rename_var_in_expr(end, old, new_name);
            rename_var_in_block(body, old, new_name);
        }
        IrExpr::IterLoop { collection, body, .. } => {
            rename_var_in_expr(collection, old, new_name);
            rename_var_in_block(body, old, new_name);
        }
        IrExpr::Repeat { elem, len } => {
            rename_var_in_expr(elem, old, new_name);
            rename_var_in_expr(len, old, new_name);
        }
        IrExpr::RawMap { receiver, body, .. } => {
            rename_var_in_expr(receiver, old, new_name);
            rename_var_in_expr(body, old, new_name);
        }
        IrExpr::RawZip { left, right, body, .. } => {
            rename_var_in_expr(left, old, new_name);
            rename_var_in_expr(right, old, new_name);
            rename_var_in_expr(body, old, new_name);
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            rename_var_in_expr(receiver, old, new_name);
            rename_var_in_expr(init, old, new_name);
            rename_var_in_expr(body, old, new_name);
        }
        IrExpr::Match { expr: scrutinee, arms } => {
            rename_var_in_expr(scrutinee, old, new_name);
            for arm in arms {
                rename_var_in_expr(&mut arm.body, old, new_name);
            }
        }
        IrExpr::DefaultValue { .. }
        | IrExpr::Lit(_)
        | IrExpr::Path { .. }
        | IrExpr::LengthOf(_)
        | IrExpr::TypenumUsize { .. }
        | IrExpr::Return(None)
        | IrExpr::Break(None)
        | IrExpr::Continue
        | IrExpr::Unreachable => {}
    }
}

fn rename_var_in_iter_chain(chain: &mut IrIterChain, old: &str, new_name: &str) {
    match &mut chain.source {
        IterChainSource::Method { collection, .. } => {
            rename_var_in_expr(collection, old, new_name);
        }
        IterChainSource::Range { start, end, .. } => {
            rename_var_in_expr(start, old, new_name);
            rename_var_in_expr(end, old, new_name);
        }
        IterChainSource::Zip { left, right } => {
            rename_var_in_iter_chain(left, old, new_name);
            rename_var_in_iter_chain(right, old, new_name);
        }
    }
    for step in &mut chain.steps {
        match step {
            IterStep::Map { body, .. }
            | IterStep::Filter { body, .. }
            | IterStep::FilterMap { body, .. }
            | IterStep::FlatMap { body, .. } => {
                rename_var_in_expr(body, old, new_name);
            }
            IterStep::Take { count } | IterStep::Skip { count } => {
                rename_var_in_expr(count, old, new_name);
            }
            IterStep::Chain { other } => {
                rename_var_in_iter_chain(other, old, new_name);
            }
            IterStep::Enumerate => {}
        }
    }
    match &mut chain.terminal {
        IterTerminal::Fold { init, body, .. } => {
            rename_var_in_expr(init, old, new_name);
            rename_var_in_expr(body, old, new_name);
        }
        IterTerminal::Collect | IterTerminal::CollectTyped(_) | IterTerminal::Lazy => {}
    }
}

fn lower_block_dyn(block: &IrBlock, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrBlock {
    IrBlock {
        stmts: block
            .stmts
            .iter()
            .map(|s| lower_stmt_dyn(s, ctx, fn_gen))
            .collect(),
        expr: block
            .expr
            .as_ref()
            .map(|e| Box::new(lower_expr_dyn(e, ctx, fn_gen))),
    }
}

fn lower_stmt_dyn(s: &IrStmt, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrStmt {
    match s {
        IrStmt::Let { pattern, ty, init } => IrStmt::Let {
            pattern: lower_pattern_dyn(pattern, ctx),
            ty: ty.as_ref().map(|t| lower_type_dyn(t, ctx, fn_gen)),
            init: init.as_ref().map(|i| lower_expr_dyn(i, ctx, fn_gen)),
        },
        IrStmt::Semi(e) => IrStmt::Semi(lower_expr_dyn(e, ctx, fn_gen)),
        IrStmt::Expr(e) => IrStmt::Expr(lower_expr_dyn(e, ctx, fn_gen)),
    }
}

fn lower_pattern_dyn(p: &IrPattern, ctx: &LoweringContext) -> IrPattern {
    match p {
        IrPattern::Ident {
            mutable,
            name,
            subpat,
        } => IrPattern::Ident {
            mutable: *mutable,
            name: name.clone(),
            subpat: subpat
                .as_ref()
                .map(|sp| Box::new(lower_pattern_dyn(sp, ctx))),
        },
        IrPattern::Tuple(elems) => {
            IrPattern::Tuple(elems.iter().map(|e| lower_pattern_dyn(e, ctx)).collect())
        }
        IrPattern::Struct { kind, fields, rest } => {
            let kind_str = kind.to_string();
            // Only rename if it's one of our structs, also handle Self
            let (new_kind, needs_rest) = if ctx.struct_info.contains_key(&kind_str) {
                // For our structs, we add witness fields, so always use ..
                (StructKind::from_str(&format!("{}Dyn", kind)), true)
            } else if kind_str == "Self" {
                // Self in impl blocks also needs .. because the impl is for our structs
                (kind.clone(), true)
            } else {
                (kind.clone(), *rest)
            };
            IrPattern::Struct {
                kind: new_kind,
                fields: fields
                    .iter()
                    .map(|(n, p)| (n.clone(), lower_pattern_dyn(p, ctx)))
                    .collect(),
                rest: needs_rest,
            }
        }
        IrPattern::TupleStruct { kind, elems } => {
            let kind_str = kind.to_string();
            // Only rename if it's one of our structs
            let new_kind = if ctx.struct_info.contains_key(&kind_str) {
                StructKind::from_str(&format!("{}Dyn", kind))
            } else {
                kind.clone()
            };
            IrPattern::TupleStruct {
                kind: new_kind,
                elems: elems.iter().map(|e| lower_pattern_dyn(e, ctx)).collect(),
            }
        }
        IrPattern::Slice(elems) => {
            IrPattern::Slice(elems.iter().map(|e| lower_pattern_dyn(e, ctx)).collect())
        }
        IrPattern::Ref { mutable, pat } => IrPattern::Ref {
            mutable: *mutable,
            pat: Box::new(lower_pattern_dyn(pat, ctx)),
        },
        IrPattern::Or(pats) => {
            IrPattern::Or(pats.iter().map(|p| lower_pattern_dyn(p, ctx)).collect())
        }
        _ => p.clone(),
    }
}

fn lower_expr_dyn(e: &IrExpr, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrExpr {
    match e {
        IrExpr::Lit(_) => e.clone(),
        IrExpr::Var(v) => {
            if v.len() == 1 && v.chars().next().unwrap().is_uppercase() {
                if fn_gen
                    .iter()
                    .any(|p| p.name == *v && classify_generic_with_aliases(p, &[fn_gen], &ctx.aliases()) == GenericKind::Length)
                {
                    return IrExpr::Var(v.to_lowercase());
                }
            }
            if v == "GenericArray" {
                return IrExpr::Path {
                    segments: vec!["Vec".to_string()],
                    type_args: vec![],
                };
            }
            e.clone()
        }
        IrExpr::Path {
            segments,
            type_args,
        } => {
            let mut segments = segments.clone();
            if segments.len() > 0 && segments[0] == "GenericArray" {
                segments[0] = "Vec".to_string();
            }
            IrExpr::Path {
                segments,
                type_args: type_args
                    .iter()
                    .map(|ta| lower_type_dyn(ta, ctx, fn_gen))
                    .collect(),
            }
        }
        IrExpr::Binary { op, left, right } => IrExpr::Binary {
            op: *op,
            left: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
            right: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
        },
        IrExpr::MethodCall {
            receiver,
            method,
            type_args,
            args,
        } => {
            let method = method.clone();
            let mut args: Vec<IrExpr> = args
                .iter()
                .map(|a| lower_expr_dyn(a, ctx, fn_gen))
                .collect();

            if let MethodKind::Vole(VoleMethod::Remap) = method {
                if args.len() == 1 {
                    args.insert(0, IrExpr::Field {
                        base: Box::new(IrExpr::Var("self".to_string())),
                        field: "n".to_string(),
                    });
                }
            }

            if let MethodKind::Unknown(ref name) = method {
                if name == "encrypt_block" {
                    if let Some(a) = args.get(0).cloned() {
                        args[0] = IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Block".to_string(), "from_mut_slice".to_string()],
                                type_args: vec![IrType::TypeParam("B".to_string())],
                            }),
                            args: vec![a],
                        };
                    }
                }
            }

            if let MethodKind::Std(name) = &method {
                if name == "to_usize" {
                    match receiver.as_ref() {
                        IrExpr::Var(v)
                            if v.len() == 1 && v.chars().next().unwrap().is_uppercase() =>
                        {
                            return IrExpr::Var(v.to_lowercase());
                        }
                        IrExpr::Path { segments, .. }
                            if segments.len() == 2 && segments[1] == "OutputSize" =>
                        {
                            return IrExpr::TypenumUsize {
                                ty: Box::new(IrType::Projection {
                                    base: Box::new(IrType::TypeParam(segments[0].clone())),
                                    trait_path: None,
                                    trait_args: Vec::new(),
                                    assoc: AssociatedType::from_str(&segments[1]),
                                }),
                            };
                        }
                        _ => {}
                    }
                }
            }
            let lowered = IrExpr::MethodCall {
                receiver: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
                method: method.clone(),
                type_args: type_args
                    .iter()
                    .map(|ta| lower_type_dyn(ta, ctx, fn_gen))
                    .collect(),
                args,
            };
            // Digest::finalize() returns GenericArray → wrap with .to_vec()
            if matches!(&method, MethodKind::Unknown(n) if n == "finalize") {
                IrExpr::MethodCall {
                    receiver: Box::new(lowered),
                    method: MethodKind::Std("to_vec".to_string()),
                    type_args: vec![],
                    args: vec![],
                }
            } else if matches!(&method, MethodKind::Std(n) if n == "as_ref") {
                // Disambiguate as_ref() → AsRef::<[u8]>::as_ref(&x)
                let lowered_receiver = lower_expr_dyn(receiver, ctx, fn_gen);
                IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: vec!["AsRef".to_string(), "as_ref".to_string()],
                        type_args: vec![IrType::Array {
                            kind: ArrayKind::Slice,
                            elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                            len: ArrayLength::TypeParam("_".to_string()),
                        }],
                    }),
                    args: vec![IrExpr::Unary {
                        op: crate::ir::SpecUnaryOp::Ref,
                        expr: Box::new(lowered_receiver),
                    }],
                }
            } else {
                lowered
            }
        }
        IrExpr::Call { func, args } => {
            let args: Vec<IrExpr> = args
                .iter()
                .map(|a| lower_expr_dyn(a, ctx, fn_gen))
                .collect();
            let mut func = lower_expr_dyn(func, ctx, fn_gen);

            // If the func is a lowercased length param (like n, k, k2),
            // and there are no args, just return the variable
            if args.is_empty() {
                if let IrExpr::Var(name) = &func {
                    // Check if it looks like a lowercased length param
                    let is_length_var = name
                        .chars()
                        .next()
                        .map(|c| c.is_lowercase())
                        .unwrap_or(false)
                        && name.chars().all(|c| c.is_lowercase() || c.is_ascii_digit());
                    if is_length_var {
                        return func;
                    }
                }
            }

            let mut new_func = None;
            // Check for type-param static method calls like B::double(x)
            // These need special wrapping for Vec<u8> ↔ GenericArray bridging
            let func_name = match &func {
                IrExpr::Var(name) => Some(name.clone()),
                IrExpr::Path { segments, .. } if segments.len() >= 1 => {
                    Some(segments.last().unwrap().clone())
                }
                _ => None,
            };
            let func_qualifier = match &func {
                IrExpr::Path { segments, .. } if segments.len() == 2 => {
                    Some(segments[0].clone())
                }
                _ => None,
            };
            if let Some(name) = &func_name {
                // B::double(x) → double_vec::<B>(x) — bridges Vec<u8> ↔ GenericArray
                if name == "double" {
                    if let Some(qual) = &func_qualifier {
                        // Use the qualifier as the type param (e.g., B::double → double_vec::<B>)
                        new_func = Some(IrExpr::Path {
                            segments: vec!["double_vec".to_string()],
                            type_args: vec![IrType::TypeParam(qual.clone())],
                        });
                    } else if let Some(b_param) = fn_gen.iter().find(|p| {
                        p.name.starts_with('B')
                            && p.bounds.iter().any(|b| matches!(&b.trait_kind,
                                TraitKind::Custom(n) if n == "LengthDoubler" || n == "BlockEncrypt" || n == "BlockCipher"))
                    }) {
                        new_func = Some(IrExpr::Path {
                            segments: vec!["double_vec".to_string()],
                            type_args: vec![IrType::TypeParam(b_param.name.clone())],
                        });
                    }
                } else if name == "create_vole_from_material"
                    || name == "create_vole_from_material_expanded"
                {
                    if let Some(b_param) = fn_gen.iter().find(|p| {
                        p.name.starts_with('B')
                            && p.bounds.iter().any(|b| matches!(&b.trait_kind,
                                TraitKind::Custom(n) if n == "LengthDoubler" || n == "BlockEncrypt" || n == "BlockCipher"))
                    }) {
                        new_func = Some(IrExpr::Path {
                            segments: vec![name.clone()],
                            type_args: [IrType::TypeParam(b_param.name.clone())]
                                .into_iter()
                                .chain(vec![
                                    IrType::Infer;
                                    match &**name {
                                        "create_vole_from_material" => 1,
                                        "create_vole_from_material_expanded" => 3,
                                        _ => 0,
                                    }
                                ])
                                .collect(),
                        });
                    } else {
                        todo!(
                            "error: could not find B generic parameter for function {} (fn_gen: {fn_gen:?})",
                            name
                        );
                    }
                } else if name == "commit" {
                    if let Some(d_param) = fn_gen.iter().find(|p| {
                        p.name.starts_with('D')
                            && p.bounds.iter().any(|b| matches!(&b.trait_kind,
                                TraitKind::Custom(n) if n == "Digest"))
                    }) {
                        new_func = Some(IrExpr::Path {
                            segments: vec![name.clone()],
                            type_args: vec![IrType::TypeParam(d_param.name.clone())],
                        });
                    } else {
                        todo!(
                            "error: could not find D generic parameter for function {} (fn_gen: {fn_gen:?})",
                            name
                        );
                    }
                }
            }
            if let Some(nf) = new_func {
                func = nf;
            }

            let mut final_args = args;

            // Rename tuple struct constructors (e.g., CommitmentCore → CommitmentCoreDyn)
            // and append PhantomData if the struct needs it
            let func_name = match &func {
                IrExpr::Path { segments, .. } => segments.last().map(|s| s.clone()),
                IrExpr::Var(n) => Some(n.clone()),
                _ => None,
            };
            if let Some(ref name) = func_name {
                if let Some(info) = ctx.struct_info.get(name.as_str()) {
                    let dyn_name = format!("{}Dyn", name);
                    match &mut func {
                        IrExpr::Path { segments, .. } => {
                            if let Some(last) = segments.last_mut() {
                                *last = dyn_name;
                            }
                        }
                        IrExpr::Var(n) => *n = dyn_name,
                        _ => {}
                    }
                    // For tuple struct constructors, append PhantomData if needed
                    if info.needs_phantom {
                        final_args.push(IrExpr::Path {
                            segments: vec!["PhantomData".to_string()],
                            type_args: vec![],
                        });
                    }
                }
            }

            IrExpr::Call {
                func: Box::new(func),
                args: final_args,
            }
        }
        IrExpr::StructExpr {
            kind,
            type_args,
            fields,
            rest,
        } => {
            let kind_str = kind.to_string();
            let new_kind = if kind_str == "Option" || kind_str == "Result" {
                kind.clone()
            } else if ctx.struct_info.contains_key(&kind_str) {
                StructKind::from_str(&format!("{}Dyn", kind))
            } else {
                kind.clone()
            };

            let mut lowered_fields: Vec<_> = fields
                .iter()
                .map(|(n, v)| (n.clone(), lower_expr_dyn(v, ctx, fn_gen)))
                .collect();

            if let Some(info) = ctx.struct_info.get(&kind_str) {
                // Map from witness name to the corresponding type arg if it's a typenum constant
                let mut witness_values: BTreeMap<String, Option<usize>> = BTreeMap::new();

                // Build mapping of witness name -> typenum value from type_args
                let mut type_arg_idx = 0;
                for (name, gkind, pkind) in &info.orig_generics {
                    if *pkind == IrGenericParamKind::Lifetime {
                        continue;
                    }
                    if *gkind == GenericKind::Length {
                        let lowered_name = name.to_lowercase();
                        // Get the corresponding type arg if available
                        if let Some(ta) = type_args.get(type_arg_idx) {
                            if let IrType::TypeParam(p) = ta {
                                if let Some(tn) = TypeNumConst::from_str(p) {
                                    witness_values.insert(lowered_name, Some(tn.to_usize()));
                                }
                            }
                        }
                    }
                    type_arg_idx += 1;
                }

                for w in &info.length_witnesses {
                    if !lowered_fields.iter().any(|(n, _)| n == w) {
                        let value = if let Some(Some(val)) = witness_values.get(w) {
                            IrExpr::Lit(IrLit::Int(*val as i128))
                        } else {
                            // Check if there's a default value for this witness
                            let default_val = info
                                .orig_generics
                                .iter()
                                .position(|(name, gkind, _)| {
                                    *gkind == GenericKind::Length && name.to_lowercase() == *w
                                })
                                .and_then(|idx| info.generic_defaults.get(idx).cloned().flatten())
                                .and_then(|ty| {
                                    if let IrType::TypeParam(p) = &ty {
                                        TypeNumConst::from_str(p).map(|tn| tn.to_usize())
                                    } else {
                                        None
                                    }
                                });

                            if let Some(def) = default_val {
                                IrExpr::Lit(IrLit::Int(def as i128))
                            } else {
                                // Use 0 as a placeholder to allow compilation
                                // The user will need to fix these manually
                                IrExpr::Lit(IrLit::Int(0))
                            }
                        };
                        lowered_fields.push((w.clone(), value));
                    }
                }

                // Add PhantomData if the struct has unused type params
                if info.needs_phantom && !lowered_fields.iter().any(|(n, _)| n == "_phantom") {
                    // Add _phantom: PhantomData
                    lowered_fields.push((
                        "_phantom".to_string(),
                        IrExpr::Path {
                            segments: vec!["PhantomData".to_string()],
                            type_args: Vec::new(),
                        },
                    ));
                }
            }

            IrExpr::StructExpr {
                kind: new_kind,
                type_args: type_args
                    .iter()
                    .map(|ta| lower_type_dyn(ta, ctx, fn_gen))
                    .collect(),
                fields: lowered_fields,
                rest: rest
                    .as_ref()
                    .map(|r| Box::new(lower_expr_dyn(r, ctx, fn_gen))),
            }
        }
        // ArrayGenerate → IterPipeline: (0..len).map(|index_var| body).collect()
        IrExpr::ArrayGenerate {
            elem_ty,
            len,
            index_var,
            body,
        } => {
            let len_expr = array_length_to_expr(len, fn_gen, ctx);
            let terminal = match elem_ty {
                Some(ty) => IterTerminal::CollectTyped(lower_type_dyn(ty, ctx, fn_gen)),
                None => IterTerminal::Collect,
            };
            IrExpr::IterPipeline(IrIterChain {
                source: IterChainSource::Range {
                    start: Box::new(IrExpr::Lit(IrLit::Int(0))),
                    end: Box::new(len_expr),
                    inclusive: false,
                },
                steps: vec![IterStep::Map {
                    var: IrPattern::ident(index_var.clone()),
                    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
                }],
                terminal,
            })
        }
        // DefaultValue → recursively expand into concrete expressions.
        // No DefaultValue nodes should survive past dyn-lowering.
        IrExpr::DefaultValue { ty } => {
            lower_default_value(ty.as_deref(), ctx, fn_gen)
        }
        IrExpr::LengthOf(len) => array_length_to_expr(len, fn_gen, ctx),
        IrExpr::Field { base, field } => IrExpr::Field {
            base: Box::new(lower_expr_dyn(base, ctx, fn_gen)),
            field: field.clone(),
        },
        IrExpr::Index { base, index } => IrExpr::Index {
            base: Box::new(lower_expr_dyn(base, ctx, fn_gen)),
            index: Box::new(lower_expr_dyn(index, ctx, fn_gen)),
        },
        IrExpr::Unary { op, expr } => IrExpr::Unary {
            op: *op,
            expr: Box::new(lower_expr_dyn(expr, ctx, fn_gen)),
        },
        IrExpr::Block(b) => IrExpr::Block(lower_block_dyn(b, ctx, fn_gen)),
        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => IrExpr::If {
            cond: Box::new(lower_expr_dyn(cond, ctx, fn_gen)),
            then_branch: lower_block_dyn(then_branch, ctx, fn_gen),
            else_branch: else_branch
                .as_ref()
                .map(|eb| Box::new(lower_expr_dyn(eb, ctx, fn_gen))),
        },
        IrExpr::BoundedLoop {
            var,
            start,
            end,
            inclusive,
            body,
        } => IrExpr::BoundedLoop {
            var: var.clone(),
            start: Box::new(lower_expr_dyn(start, ctx, fn_gen)),
            end: Box::new(lower_expr_dyn(end, ctx, fn_gen)),
            inclusive: *inclusive,
            body: lower_block_dyn(body, ctx, fn_gen),
        },
        IrExpr::IterLoop {
            pattern,
            collection,
            body,
        } => IrExpr::IterLoop {
            pattern: lower_pattern_dyn(pattern, ctx),
            collection: Box::new(lower_expr_dyn(collection, ctx, fn_gen)),
            body: lower_block_dyn(body, ctx, fn_gen),
        },
        IrExpr::IterPipeline(chain) => {
            IrExpr::IterPipeline(lower_iter_chain_dyn(chain, ctx, fn_gen))
        },
        // RawMap → receiver.into_iter().map(|var| body).collect()
        IrExpr::RawMap { receiver, elem_var, body } => {
            IrExpr::IterPipeline(crate::ir::IrIterChain {
                source: crate::ir::IterChainSource::Method {
                    collection: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
                    method: crate::ir::IterMethod::IntoIter,
                },
                steps: vec![crate::ir::IterStep::Map {
                    var: elem_var.clone(),
                    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
                }],
                terminal: crate::ir::IterTerminal::Collect,
            })
        },
        // RawZip → left.into_iter().zip(right.into_iter()).map(|(a,b)| body).collect()
        IrExpr::RawZip { left, right, left_var, right_var, body } => {
            IrExpr::IterPipeline(crate::ir::IrIterChain {
                source: crate::ir::IterChainSource::Zip {
                    left: Box::new(crate::ir::IrIterChain {
                        source: crate::ir::IterChainSource::Method {
                            collection: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
                            method: crate::ir::IterMethod::IntoIter,
                        },
                        steps: Vec::new(),
                        terminal: crate::ir::IterTerminal::Lazy,
                    }),
                    right: Box::new(crate::ir::IrIterChain {
                        source: crate::ir::IterChainSource::Method {
                            collection: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
                            method: crate::ir::IterMethod::IntoIter,
                        },
                        steps: Vec::new(),
                        terminal: crate::ir::IterTerminal::Lazy,
                    }),
                },
                steps: vec![crate::ir::IterStep::Map {
                    var: IrPattern::Tuple(vec![left_var.clone(), right_var.clone()]),
                    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
                }],
                terminal: crate::ir::IterTerminal::Collect,
            })
        },
        // RawFold → receiver.into_iter().fold(init, |acc, elem| body)
        IrExpr::RawFold { receiver, init, acc_var, elem_var, body } => {
            IrExpr::IterPipeline(crate::ir::IrIterChain {
                source: crate::ir::IterChainSource::Method {
                    collection: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
                    method: crate::ir::IterMethod::IntoIter,
                },
                steps: Vec::new(),
                terminal: crate::ir::IterTerminal::Fold {
                    init: Box::new(lower_expr_dyn(init, ctx, fn_gen)),
                    acc_var: acc_var.clone().as_mut(),
                    elem_var: elem_var.clone(),
                    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
                },
            })
        },
        IrExpr::Match { expr, arms } => IrExpr::Match {
            expr: Box::new(lower_expr_dyn(expr, ctx, fn_gen)),
            arms: arms
                .iter()
                .map(|arm| IrMatchArm {
                    pattern: lower_pattern_dyn(&arm.pattern, ctx),
                    guard: arm.guard.as_ref().map(|g| lower_expr_dyn(g, ctx, fn_gen)),
                    body: lower_expr_dyn(&arm.body, ctx, fn_gen),
                })
                .collect(),
        },
        IrExpr::Closure {
            params,
            ret_type,
            body,
        } => IrExpr::Closure {
            params: params.clone(),
            ret_type: ret_type
                .as_ref()
                .map(|rt| Box::new(lower_type_dyn(rt, ctx, fn_gen))),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IrExpr::Cast { expr, ty } => IrExpr::Cast {
            expr: Box::new(lower_expr_dyn(expr, ctx, fn_gen)),
            ty: Box::new(lower_type_dyn(ty, ctx, fn_gen)),
        },
        IrExpr::Return(e) => IrExpr::Return(
            e.as_ref()
                .map(|expr| Box::new(lower_expr_dyn(expr, ctx, fn_gen))),
        ),
        IrExpr::Tuple(elems) => IrExpr::Tuple(
            elems
                .iter()
                .map(|e| lower_expr_dyn(e, ctx, fn_gen))
                .collect(),
        ),
        IrExpr::Array(elems) => IrExpr::Array(
            elems
                .iter()
                .map(|e| lower_expr_dyn(e, ctx, fn_gen))
                .collect(),
        ),
        IrExpr::Repeat { elem, len } => IrExpr::Repeat {
            elem: Box::new(lower_expr_dyn(elem, ctx, fn_gen)),
            len: Box::new(lower_expr_dyn(len, ctx, fn_gen)),
        },
        IrExpr::Assign { left, right } => IrExpr::Assign {
            left: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
            right: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
        },
        IrExpr::Unreachable => IrExpr::Unreachable,
        IrExpr::Range {
            start,
            end,
            inclusive,
        } => IrExpr::Range {
            start: match start.as_ref() {
                None => None,
                Some(a) => Some(Box::new(lower_expr_dyn(a, ctx, fn_gen))),
            },
            end: match end.as_ref() {
                None => None,
                Some(a) => Some(Box::new(lower_expr_dyn(a, ctx, fn_gen))),
            },
            inclusive: *inclusive,
        },
        e => todo!("error: lower_expr_dyn not implemented for {:?}", e),
    }
}

fn lower_iter_chain_dyn(
    chain: &IrIterChain,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> IrIterChain {
    IrIterChain {
        source: lower_iter_chain_source_dyn(&chain.source, ctx, fn_gen),
        steps: chain
            .steps
            .iter()
            .map(|s| lower_iter_step_dyn(s, ctx, fn_gen))
            .collect(),
        terminal: lower_iter_terminal_dyn(&chain.terminal, ctx, fn_gen),
    }
}

fn lower_iter_chain_source_dyn(
    source: &IterChainSource,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> IterChainSource {
    match source {
        IterChainSource::Method { collection, method } => IterChainSource::Method {
            collection: Box::new(lower_expr_dyn(collection, ctx, fn_gen)),
            method: *method,
        },
        IterChainSource::Range {
            start,
            end,
            inclusive,
        } => IterChainSource::Range {
            start: Box::new(lower_expr_dyn(start, ctx, fn_gen)),
            end: Box::new(lower_expr_dyn(end, ctx, fn_gen)),
            inclusive: *inclusive,
        },
        IterChainSource::Zip { left, right } => IterChainSource::Zip {
            left: Box::new(lower_iter_chain_dyn(left, ctx, fn_gen)),
            right: Box::new(lower_iter_chain_dyn(right, ctx, fn_gen)),
        },
    }
}

fn lower_iter_step_dyn(
    step: &IterStep,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> IterStep {
    match step {
        IterStep::Map { var, body } => IterStep::Map {
            var: var.clone(),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IterStep::Filter { var, body } => IterStep::Filter {
            var: var.clone(),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IterStep::FilterMap { var, body } => IterStep::FilterMap {
            var: var.clone(),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IterStep::FlatMap { var, body } => IterStep::FlatMap {
            var: var.clone(),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IterStep::Enumerate => IterStep::Enumerate,
        IterStep::Take { count } => IterStep::Take {
            count: Box::new(lower_expr_dyn(count, ctx, fn_gen)),
        },
        IterStep::Skip { count } => IterStep::Skip {
            count: Box::new(lower_expr_dyn(count, ctx, fn_gen)),
        },
        IterStep::Chain { other } => IterStep::Chain {
            other: Box::new(lower_iter_chain_dyn(other, ctx, fn_gen)),
        },
    }
}

fn lower_iter_terminal_dyn(
    terminal: &IterTerminal,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> IterTerminal {
    match terminal {
        IterTerminal::Collect => IterTerminal::Collect,
        IterTerminal::CollectTyped(ty) => IterTerminal::CollectTyped(lower_type_dyn(ty, ctx, fn_gen)),
        IterTerminal::Fold {
            init,
            acc_var,
            elem_var,
            body,
        } => IterTerminal::Fold {
            init: Box::new(lower_expr_dyn(init, ctx, fn_gen)),
            acc_var: acc_var.clone().as_mut(),
            elem_var: elem_var.clone(),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IterTerminal::Lazy => IterTerminal::Lazy,
    }
}

fn lower_array_length(len: &ArrayLength, fn_gen: &[IrGenericParam], ctx: &LoweringContext) -> ArrayLength {
    match len {
        ArrayLength::Projection { r#type, field, trait_path } => {
            resolve_projection_as_length(r#type, field, trait_path.as_deref(), fn_gen, ctx)
        }
        ArrayLength::TypeParam(p) => {
            // Check if this is a typenum constant
            if let Some(tn) = TypeNumConst::from_str(p) {
                ArrayLength::Const(tn.to_usize())
            } else if p.contains("::") {
                // Path like B::BlockSize → parse as projection
                let parts: Vec<&str> = p.split("::").collect();
                if parts.len() == 2 {
                    resolve_projection_as_length(
                        &IrType::TypeParam(parts[0].to_string()),
                        parts[1],
                        None,
                        fn_gen,
                        ctx,
                    )
                } else {
                    ArrayLength::TypeParam(p.to_lowercase())
                }
            } else {
                // Otherwise lowercase it to a runtime variable
                ArrayLength::TypeParam(p.to_lowercase())
            }
        }
        ArrayLength::TypeNum(tn) => ArrayLength::Const(tn.to_usize()),
        ArrayLength::Const(_) => len.clone(),
    }
}

/// Resolve a projection `<base_ty>::field` that can stay as an `ArrayLength`.
///
/// For type-param projections on type generics (e.g. `B::OutputSize`), keeps
/// the projection for compile-time resolution.
///
/// For Self projections, returns a runtime variable.
///
/// For length-param projections where arithmetic *could* apply, still returns
/// a variable name (the caller should use `array_length_to_expr` to get the
/// full `IrExpr::Binary`).
fn resolve_projection_as_length(
    base_ty: &IrType,
    field: &str,
    trait_path: Option<&str>,
    fn_gen: &[IrGenericParam],
    ctx: &LoweringContext,
) -> ArrayLength {
    match base_ty {
        IrType::TypeParam(p) if p == "Self" => {
            ArrayLength::TypeParam(validate_ident(&format!("self_{}", field.to_lowercase())))
        }
        IrType::TypeParam(p) => {
            let param = fn_gen.iter().find(|g| &g.name == p);
            let is_length = param.map_or(false, |g| {
                classify_generic_with_aliases(g, &[fn_gen], &ctx.aliases()) == GenericKind::Length
            });
            if is_length {
                // Length param projection — return as variable name.
                // Arithmetic resolution happens in array_length_to_expr().
                ArrayLength::TypeParam(validate_ident(&format!("{}_{}", p.to_lowercase(), field.to_lowercase())))
            } else {
                // Type param → keep as compile-time projection
                ArrayLength::Projection {
                    r#type: Box::new(base_ty.clone()),
                    field: field.to_string(),
                    trait_path: trait_path.map(|s| s.to_string()),
                }
            }
        }
        IrType::Projection { .. } => {
            // Nested projection — keep as-is for compile-time resolution
            ArrayLength::Projection {
                r#type: Box::new(base_ty.clone()),
                field: field.to_string(),
                trait_path: trait_path.map(|s| s.to_string()),
            }
        }
        _ => {
            ArrayLength::TypeParam(validate_ident(&format!("len_{}", field.to_lowercase())))
        }
    }
}

/// Convert an `ArrayLength` into a runtime `IrExpr` representing a `usize` value.
///
/// This is the primary entry point for converting type-level lengths into
/// Recursively lower a `DefaultValue` into concrete expressions.
///
/// This is the heart of the unified default-value lowering. Instead of leaving
/// `DefaultValue` nodes for the printer to interpret, we expand them here:
///
/// - `Array<T, N>` → `IterPipeline(0..N_expr).map(_ => lower_default_value(T))`
/// - `Primitive(u8/u32/usize)` → `Lit(0)`
/// - `Primitive(u64/i128)` → `Lit(0n)`
/// - `Primitive(Bool)` → `Lit(false)`
/// - `Primitive(Bit/Galois/...)` → `MethodCall(ClassName, "default", [])`
/// - `TypeParam(T)` → `Call(Field(ctx, "defaultT"), [])`  (witness)
/// - `Infer` / `None` → `Lit(0)` (best-effort fallback)
/// - `Vector<T>` → `Lit([])` (empty vec)
///
/// For arrays, the length is resolved through the same witness machinery as
/// everywhere else (`array_length_to_expr`), and `N` placeholders are resolved
/// to the unique length generic when unambiguous.
fn lower_default_value(
    ty: Option<&IrType>,
    ctx: &LoweringContext,
    fn_gen: &[IrGenericParam],
) -> IrExpr {
    let Some(ty) = ty else {
        panic!(
            "DefaultValue with no type information (bare `GenericArray::default()` without \
             turbofish?). Add explicit type parameters to the source, e.g. \
             `GenericArray::<T, N>::default()`."
        );
    };

    match ty {
        IrType::Primitive(p) => match p {
            PrimitiveType::Bool => IrExpr::Lit(IrLit::Bool(false)),
            PrimitiveType::U8 | PrimitiveType::U32 | PrimitiveType::Usize => {
                IrExpr::Lit(IrLit::Int(0))
            }
            PrimitiveType::U64 | PrimitiveType::I128 => {
                // 0n for bigint — use Int(0), the fold accumulator context
                // will handle bigint coercion at the call site.
                IrExpr::Lit(IrLit::Int(0))
            }
            PrimitiveType::Bit
            | PrimitiveType::Galois
            | PrimitiveType::Galois64
            | PrimitiveType::BitsInBytes
            | PrimitiveType::BitsInBytes64 => {
                // Emit `Bit.default()` etc.
                IrExpr::MethodCall {
                    receiver: Box::new(IrExpr::Path {
                        segments: vec![format!("{}", p)],
                        type_args: vec![],
                    }),
                    method: MethodKind::Std("default".to_string()),
                    args: vec![],
                    type_args: vec![],
                }
            }
        },

        IrType::Array { elem, len, .. } => {
            // Resolve placeholder "N" when GenericArray::default() had no turbofish
            let resolved_len = match len {
                ArrayLength::TypeParam(p) if p == "N" => {
                    let has_n = fn_gen.iter().any(|g| {
                        g.name == "N"
                            && classify_generic_with_aliases(g, &[fn_gen], &ctx.aliases())
                                == GenericKind::Length
                    });
                    if !has_n {
                        let length_gens: Vec<_> = fn_gen
                            .iter()
                            .filter(|g| {
                                classify_generic_with_aliases(g, &[fn_gen], &ctx.aliases())
                                    == GenericKind::Length
                            })
                            .collect();
                        if length_gens.len() == 1 {
                            ArrayLength::TypeParam(length_gens[0].name.clone())
                        } else {
                            len.clone()
                        }
                    } else {
                        len.clone()
                    }
                }
                _ => len.clone(),
            };
            let len_expr = array_length_to_expr(&resolved_len, fn_gen, ctx);

            // Recurse: lower the element default
            let lowered_elem_ty = lower_type_dyn(elem, ctx, fn_gen);
            let default_body = lower_default_value(Some(&lowered_elem_ty), ctx, fn_gen);
            let terminal = IterTerminal::CollectTyped(lowered_elem_ty);

            IrExpr::IterPipeline(IrIterChain {
                source: IterChainSource::Range {
                    start: Box::new(IrExpr::Lit(IrLit::Int(0))),
                    end: Box::new(len_expr),
                    inclusive: false,
                },
                steps: vec![IterStep::Map {
                    var: IrPattern::Wild,
                    body: Box::new(default_body),
                }],
                terminal,
            })
        }

        IrType::Vector { elem } => {
            // Empty vec default → DefaultValue so the TS printer emits `[]`
            IrExpr::DefaultValue {
                ty: Some(Box::new(IrType::Vector {
                    elem: Box::new(lower_type_dyn(elem, ctx, fn_gen)),
                })),
            }
        }

        IrType::TypeParam(name) => {
            // Type-param default → witness call: ctx.defaultT()
            IrExpr::Call {
                func: Box::new(IrExpr::Field {
                    base: Box::new(IrExpr::Var("ctx".to_string())),
                    field: format!("default{}", name),
                }),
                args: vec![],
            }
        }

        IrType::Infer => {
            panic!(
                "DefaultValue with inferred element type (`IrType::Infer`). This typically \
                 comes from bare `GenericArray::default()` without turbofish. Add explicit \
                 type parameters to the Rust source, e.g. \
                 `GenericArray::<ElemType, LenType>::default()`."
            );
        }

        _ => {
            // Fallback for other types (structs, tuples, etc.)
            // Leave as DefaultValue for the printer to handle
            IrExpr::DefaultValue {
                ty: Some(Box::new(lower_type_dyn(ty, ctx, fn_gen))),
            }
        }
    }
}

/// Convert an `ArrayLength` to a runtime expression. Handles type-param lengths,
/// runtime expressions. Unlike `lower_array_length` (which stays in the
/// `ArrayLength` domain), this produces proper `IrExpr` nodes — including
/// `IrExpr::Binary` for arithmetic projections like `K2: Add<K>` → `k2 + k`.
fn array_length_to_expr(len: &ArrayLength, fn_gen: &[IrGenericParam], ctx: &LoweringContext) -> IrExpr {
    // Handle Logarithm2 projections: <X as Logarithm2>::Output → ilog2(X_expr)
    if let ArrayLength::Projection { r#type, field, trait_path } = len {
        if trait_path.as_deref() == Some("Logarithm2") && field == "Output" {
            // Convert the base type to a length expression, then wrap in ilog2()
            let inner_len = convert_array_length_from_type_for_lowering(r#type);
            let inner_expr = array_length_to_expr(&inner_len, fn_gen, ctx);
            return IrExpr::Call {
                func: Box::new(IrExpr::Path { segments: vec!["ilog2".to_string()], type_args: vec![] }),
                args: vec![inner_expr],
            };
        }

        // Try arithmetic resolution for projections on length params
        if let Some(expr) = resolve_projection_as_expr(r#type, field, fn_gen, ctx) {
            return expr;
        }
    }
    // Also handle "K::Output" in TypeParam form
    if let ArrayLength::TypeParam(p) = len {
        if p.contains("::") {
            let parts: Vec<&str> = p.split("::").collect();
            if parts.len() == 2 {
                if let Some(expr) = resolve_projection_as_expr(
                    &IrType::TypeParam(parts[0].to_string()),
                    parts[1],
                    fn_gen,
                    ctx,
                ) {
                    return expr;
                }
            }
        }
    }
    // Fall back to lower_array_length → convert to expr
    let lowered = lower_array_length(len, fn_gen, ctx);
    match &lowered {
        ArrayLength::Const(n) => IrExpr::Lit(IrLit::Int(*n as i128)),
        ArrayLength::TypeParam(p) => IrExpr::Var(p.clone()),
        // Projections stay as LengthOf — printed as <<T>::Assoc as Unsigned>::to_usize()
        _ => IrExpr::LengthOf(lowered),
    }
}

/// Convert an IrType to an ArrayLength for use in lowering.
/// This handles type params, projections (D::OutputSize), and typenum constants.
fn convert_array_length_from_type_for_lowering(ty: &IrType) -> ArrayLength {
    match ty {
        IrType::TypeParam(name) => ArrayLength::TypeParam(name.clone()),
        IrType::Projection { base, assoc, trait_path, .. } => {
            let field = match assoc {
                AssociatedType::Output => "Output",
                AssociatedType::OutputSize => "OutputSize",
                AssociatedType::BlockSize => "BlockSize",
                AssociatedType::Key => "Key",
                AssociatedType::TotalLoopCount => "TotalLoopCount",
                AssociatedType::Other(name) => name,
            };
            ArrayLength::Projection {
                r#type: base.clone(),
                field: field.to_string(),
                trait_path: trait_path.clone(),
            }
        }
        _ => ArrayLength::TypeParam("N".to_string()),
    }
}

/// Try to resolve a projection `<base_ty>::field` into an `IrExpr`.
///
/// Returns `Some(IrExpr::Binary { .. })` for arithmetic projections on length
/// params (e.g., `K2: Add<K>` → `Binary { Var("k2"), Add, Var("k") }`).
///
/// Returns `None` if this isn't an arithmetic projection — the caller should
/// fall back to `lower_array_length`.
fn resolve_projection_as_expr(
    base_ty: &IrType,
    field: &str,
    fn_gen: &[IrGenericParam],
    ctx: &LoweringContext,
) -> Option<IrExpr> {
    let p = match base_ty {
        IrType::TypeParam(p) if p != "Self" => p,
        _ => return None,
    };
    let param = fn_gen.iter().find(|g| &g.name == p)?;
    let is_length = classify_generic_with_aliases(param, &[fn_gen], &ctx.aliases()) == GenericKind::Length;
    if !is_length || field != "Output" {
        return None;
    }
    resolve_arithmetic_output_expr(p, param, fn_gen, ctx)
}

/// Given a length param `P` with bound `P: Add<Rhs>` (or Sub, Mul),
/// resolve `P::Output` to `Binary { Var("p"), op, rhs_expr }`.
fn resolve_arithmetic_output_expr(
    param_name: &str,
    param: &IrGenericParam,
    fn_gen: &[IrGenericParam],
    ctx: &LoweringContext,
) -> Option<IrExpr> {
    for bound in &param.bounds {
        let op = match &bound.trait_kind {
            TraitKind::Math(MathTrait::Add) => SpecBinOp::Add,
            TraitKind::Math(MathTrait::Sub) => SpecBinOp::Sub,
            TraitKind::Math(MathTrait::Mul) => SpecBinOp::Mul,
            _ => continue,
        };
        if let Some(rhs_ty) = bound.type_args.first() {
            let rhs = type_to_length_ir_expr(rhs_ty, fn_gen, ctx)?;
            let lhs = IrExpr::Var(validate_ident(&param_name.to_lowercase()));
            return Some(IrExpr::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            });
        }
    }
    None
}

/// Convert a type used as an arithmetic operand into a runtime `IrExpr`.
fn type_to_length_ir_expr(ty: &IrType, fn_gen: &[IrGenericParam], ctx: &LoweringContext) -> Option<IrExpr> {
    match ty {
        IrType::TypeParam(p) => {
            if let Some(tn) = TypeNumConst::from_str(p) {
                Some(IrExpr::Lit(IrLit::Int(tn.to_usize() as i128)))
            } else {
                Some(IrExpr::Var(validate_ident(&p.to_lowercase())))
            }
        }
        IrType::Projection { base, assoc, trait_path, .. } => {
            let field_name = assoc.to_string();
            // Try arithmetic resolution first
            if let Some(expr) = resolve_projection_as_expr(base, &field_name, fn_gen, ctx) {
                return Some(expr);
            }
            // Fall back to LengthOf for static projections
            let lowered = resolve_projection_as_length(base, &field_name, trait_path.as_deref(), fn_gen, ctx);
            match lowered {
                ArrayLength::Const(n) => Some(IrExpr::Lit(IrLit::Int(n as i128))),
                ArrayLength::TypeParam(s) => Some(IrExpr::Var(s)),
                _ => Some(IrExpr::LengthOf(lowered)),
            }
        }
        _ => None,
    }
}

/// Validate that a string is a valid Rust identifier (or parenthesized expression).
/// Panics if the string contains characters that could be injected code.
fn validate_ident(s: &str) -> String {
    // Allow lowercase alphanumeric, underscores
    if s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') && !s.is_empty() {
        s.to_string()
    } else {
        panic!("Invalid identifier in lowering: {:?}. \
                This usually means an expression was incorrectly stuffed into a name. \
                Use proper IrExpr nodes instead.", s);
    }
}




#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::*;

    /// Helper: create a Length generic param with given bounds
    fn length_param(name: &str, bounds: Vec<IrTraitBound>) -> IrGenericParam {
        IrGenericParam {
            name: name.to_string(),
            kind: IrGenericParamKind::Type,
            bounds: {
                let mut b = vec![IrTraitBound {
                    trait_kind: TraitKind::Math(MathTrait::Unsigned),
                    type_args: Vec::new(),
                    assoc_bindings: Vec::new(),
                }];
                b.extend(bounds);
                b
            },
            default: None,
        }
    }

    /// Helper: create a Type generic param
    fn type_param(name: &str, bounds: Vec<IrTraitBound>) -> IrGenericParam {
        IrGenericParam {
            name: name.to_string(),
            kind: IrGenericParamKind::Type,
            bounds,
            default: None,
        }
    }

    fn empty_ctx() -> LoweringContext {
        LoweringContext {
            struct_info: BTreeMap::new(),
            length_aliases: Vec::new(),
        }
    }

    fn add_bound(rhs: &str) -> IrTraitBound {
        IrTraitBound {
            trait_kind: TraitKind::Math(MathTrait::Add),
            type_args: vec![IrType::TypeParam(rhs.to_string())],
            assoc_bindings: Vec::new(),
        }
    }

    fn sub_bound(rhs: &str) -> IrTraitBound {
        IrTraitBound {
            trait_kind: TraitKind::Math(MathTrait::Sub),
            type_args: vec![IrType::TypeParam(rhs.to_string())],
            assoc_bindings: Vec::new(),
        }
    }

    fn mul_bound(rhs: &str) -> IrTraitBound {
        IrTraitBound {
            trait_kind: TraitKind::Math(MathTrait::Mul),
            type_args: vec![IrType::TypeParam(rhs.to_string())],
            assoc_bindings: Vec::new(),
        }
    }

    fn custom_bound(name: &str) -> IrTraitBound {
        IrTraitBound {
            trait_kind: TraitKind::Custom(name.to_string()),
            type_args: Vec::new(),
            assoc_bindings: Vec::new(),
        }
    }

    /// Helper to assert an IrExpr is Binary { Var(lhs), op, Var(rhs) }
    fn assert_binary_var_var(expr: &IrExpr, expected_lhs: &str, expected_op: SpecBinOp, expected_rhs: &str) {
        match expr {
            IrExpr::Binary { left, op, right } => {
                assert_eq!(*op, expected_op);
                assert_eq!(**left, IrExpr::Var(expected_lhs.to_string()));
                assert_eq!(**right, IrExpr::Var(expected_rhs.to_string()));
            }
            other => panic!("Expected Binary, got {:?}", other),
        }
    }

    /// Helper to assert an IrExpr is Binary { Var(lhs), op, Lit(rhs) }
    fn assert_binary_var_lit(expr: &IrExpr, expected_lhs: &str, expected_op: SpecBinOp, expected_rhs: i128) {
        match expr {
            IrExpr::Binary { left, op, right } => {
                assert_eq!(*op, expected_op);
                assert_eq!(**left, IrExpr::Var(expected_lhs.to_string()));
                assert_eq!(**right, IrExpr::Lit(IrLit::Int(expected_rhs)));
            }
            other => panic!("Expected Binary, got {:?}", other),
        }
    }

    // ================================================================
    // lower_array_length tests
    // ================================================================

    #[test]
    fn test_lower_typenum_const() {
        let ctx = empty_ctx();
        let len = ArrayLength::TypeParam("U16".to_string());
        let result = lower_array_length(&len, &[], &ctx);
        assert_eq!(result, ArrayLength::Const(16));
    }

    #[test]
    fn test_lower_length_param_simple() {
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![])];
        let len = ArrayLength::TypeParam("N".to_string());
        let result = lower_array_length(&len, &gens, &ctx);
        assert_eq!(result, ArrayLength::TypeParam("n".to_string()));
    }

    #[test]
    fn test_lower_projection_on_type_param_kept_static() {
        let ctx = empty_ctx();
        let gens = vec![type_param("B", vec![custom_bound("LengthDoubler")])];
        let len = ArrayLength::Projection {
            r#type: Box::new(IrType::TypeParam("B".to_string())),
            field: "OutputSize".to_string(),
            trait_path: None,
        };
        let result = lower_array_length(&len, &gens, &ctx);
        assert!(matches!(result, ArrayLength::Projection { .. }));
    }

    #[test]
    fn test_lower_projection_add_output_stays_typeparam() {
        // lower_array_length does NOT resolve arithmetic — it returns a variable name.
        // Arithmetic resolution only happens in array_length_to_expr.
        let ctx = empty_ctx();
        let gens = vec![
            length_param("K", vec![]),
            length_param("K2", vec![add_bound("K")]),
        ];
        let len = ArrayLength::Projection {
            r#type: Box::new(IrType::TypeParam("K2".to_string())),
            field: "Output".to_string(),
            trait_path: None,
        };
        let result = lower_array_length(&len, &gens, &ctx);
        assert_eq!(result, ArrayLength::TypeParam("k2_output".to_string()));
    }

    #[test]
    fn test_lower_self_projection() {
        let ctx = empty_ctx();
        let len = ArrayLength::Projection {
            r#type: Box::new(IrType::TypeParam("Self".to_string())),
            field: "Output".to_string(),
            trait_path: None,
        };
        let result = lower_array_length(&len, &[], &ctx);
        assert_eq!(result, ArrayLength::TypeParam("self_output".to_string()));
    }

    #[test]
    fn test_lower_path_notation_b_outputsize() {
        let ctx = empty_ctx();
        let gens = vec![type_param("B", vec![custom_bound("LengthDoubler")])];
        let len = ArrayLength::TypeParam("B::OutputSize".to_string());
        let result = lower_array_length(&len, &gens, &ctx);
        assert!(matches!(result, ArrayLength::Projection { .. }));
    }

    #[test]
    fn test_lower_nested_projection_kept_static() {
        let ctx = empty_ctx();
        let gens = vec![type_param("D", vec![custom_bound("Digest")])];
        let nested_base = IrType::Projection {
            base: Box::new(IrType::TypeParam("D".to_string())),
            trait_path: Some("Digest".to_string()),
            trait_args: Vec::new(),
            assoc: AssociatedType::OutputSize,
        };
        let len = ArrayLength::Projection {
            r#type: Box::new(nested_base),
            field: "Output".to_string(),
            trait_path: None,
        };
        let result = lower_array_length(&len, &gens, &ctx);
        assert!(matches!(result, ArrayLength::Projection { .. }));
    }

    // ================================================================
    // array_length_to_expr tests (arithmetic resolution)
    // ================================================================

    #[test]
    fn test_expr_const() {
        let ctx = empty_ctx();
        let result = array_length_to_expr(&ArrayLength::TypeParam("U16".to_string()), &[], &ctx);
        assert_eq!(result, IrExpr::Lit(IrLit::Int(16)));
    }

    #[test]
    fn test_expr_length_param() {
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![])];
        let result = array_length_to_expr(&ArrayLength::TypeParam("N".to_string()), &gens, &ctx);
        assert_eq!(result, IrExpr::Var("n".to_string()));
    }

    #[test]
    fn test_expr_type_projection() {
        let ctx = empty_ctx();
        let gens = vec![type_param("B", vec![custom_bound("LengthDoubler")])];
        let result = array_length_to_expr(
            &ArrayLength::Projection {
                r#type: Box::new(IrType::TypeParam("B".to_string())),
                field: "OutputSize".to_string(),
                trait_path: None,
            },
            &gens,
            &ctx,
        );
        assert!(matches!(result, IrExpr::LengthOf(ArrayLength::Projection { .. })));
    }

    #[test]
    fn test_expr_add_output() {
        // K2: Add<K> → K2::Output = Binary(Var("k2"), Add, Var("k"))
        let ctx = empty_ctx();
        let gens = vec![
            length_param("K", vec![]),
            length_param("K2", vec![add_bound("K")]),
        ];
        let result = array_length_to_expr(
            &ArrayLength::Projection {
                r#type: Box::new(IrType::TypeParam("K2".to_string())),
                field: "Output".to_string(),
                trait_path: None,
            },
            &gens,
            &ctx,
        );
        assert_binary_var_var(&result, "k2", SpecBinOp::Add, "k");
    }

    #[test]
    fn test_expr_sub_typenum() {
        // N: Sub<U1> → N::Output = Binary(Var("n"), Sub, Lit(1))
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![sub_bound("U1")])];
        let result = array_length_to_expr(
            &ArrayLength::Projection {
                r#type: Box::new(IrType::TypeParam("N".to_string())),
                field: "Output".to_string(),
                trait_path: None,
            },
            &gens,
            &ctx,
        );
        assert_binary_var_lit(&result, "n", SpecBinOp::Sub, 1);
    }

    #[test]
    fn test_expr_mul_output() {
        // N: Mul<K> → N::Output = Binary(Var("n"), Mul, Var("k"))
        let ctx = empty_ctx();
        let gens = vec![
            length_param("N", vec![mul_bound("K")]),
            length_param("K", vec![]),
        ];
        let result = array_length_to_expr(
            &ArrayLength::Projection {
                r#type: Box::new(IrType::TypeParam("N".to_string())),
                field: "Output".to_string(),
                trait_path: None,
            },
            &gens,
            &ctx,
        );
        assert_binary_var_var(&result, "n", SpecBinOp::Mul, "k");
    }

    #[test]
    fn test_expr_mul_typenum() {
        // K: Mul<U2> → K::Output = Binary(Var("k"), Mul, Lit(2))
        let ctx = empty_ctx();
        let gens = vec![length_param("K", vec![mul_bound("U2")])];
        let result = array_length_to_expr(
            &ArrayLength::Projection {
                r#type: Box::new(IrType::TypeParam("K".to_string())),
                field: "Output".to_string(),
                trait_path: None,
            },
            &gens,
            &ctx,
        );
        assert_binary_var_lit(&result, "k", SpecBinOp::Mul, 2);
    }

    #[test]
    fn test_expr_path_notation_add() {
        // "K2::Output" in TypeParam form with K2: Add<K>
        let ctx = empty_ctx();
        let gens = vec![
            length_param("K", vec![]),
            length_param("K2", vec![add_bound("K")]),
        ];
        let result = array_length_to_expr(
            &ArrayLength::TypeParam("K2::Output".to_string()),
            &gens,
            &ctx,
        );
        assert_binary_var_var(&result, "k2", SpecBinOp::Add, "k");
    }

    // ================================================================
    // resolve_arithmetic_output_expr tests
    // ================================================================

    #[test]
    fn test_resolve_no_arithmetic_bound() {
        let ctx = empty_ctx();
        let param = length_param("K", vec![]);
        let result = resolve_arithmetic_output_expr("K", &param, &[param.clone()], &ctx);
        assert!(result.is_none());
    }

    #[test]
    fn test_resolve_add_with_typenum_rhs() {
        let ctx = empty_ctx();
        let param = length_param("K", vec![add_bound("U1")]);
        let result = resolve_arithmetic_output_expr("K", &param, &[param.clone()], &ctx);
        let expr = result.unwrap();
        assert_binary_var_lit(&expr, "k", SpecBinOp::Add, 1);
    }

    // ================================================================
    // LengthOf expr lowering
    // ================================================================

    #[test]
    fn test_lower_lengthof_projection_type_param() {
        let ctx = empty_ctx();
        let gens = vec![type_param("B", vec![custom_bound("LengthDoubler")])];
        let expr = IrExpr::LengthOf(ArrayLength::Projection {
            r#type: Box::new(IrType::TypeParam("B".to_string())),
            field: "OutputSize".to_string(),
            trait_path: None,
        });
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        assert!(matches!(result, IrExpr::LengthOf(ArrayLength::Projection { .. })));
    }

    #[test]
    fn test_lower_lengthof_add_output() {
        let ctx = empty_ctx();
        let gens = vec![
            length_param("K", vec![]),
            length_param("K2", vec![add_bound("K")]),
        ];
        let expr = IrExpr::LengthOf(ArrayLength::Projection {
            r#type: Box::new(IrType::TypeParam("K2".to_string())),
            field: "Output".to_string(),
            trait_path: None,
        });
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        assert_binary_var_var(&result, "k2", SpecBinOp::Add, "k");
    }

    #[test]
    fn test_lower_lengthof_const() {
        let ctx = empty_ctx();
        let expr = IrExpr::LengthOf(ArrayLength::TypeParam("U16".to_string()));
        let result = lower_expr_dyn(&expr, &ctx, &[]);
        assert_eq!(result, IrExpr::Lit(IrLit::Int(16)));
    }

    // ================================================================
    // ArrayGenerate → IterPipeline
    // ================================================================

    #[test]
    fn test_array_generate_becomes_iter_pipeline() {
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![])];
        let expr = IrExpr::ArrayGenerate {
            elem_ty: None,
            len: ArrayLength::TypeParam("N".to_string()),
            index_var: "i".to_string(),
            body: Box::new(IrExpr::Var("i".to_string())),
        };
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        match result {
            IrExpr::IterPipeline(chain) => {
                match &chain.source {
                    IterChainSource::Range { start, end, inclusive } => {
                        assert_eq!(**start, IrExpr::Lit(IrLit::Int(0)));
                        assert_eq!(**end, IrExpr::Var("n".to_string()));
                        assert!(!inclusive);
                    }
                    other => panic!("Expected Range source, got {:?}", other),
                }
                assert_eq!(chain.steps.len(), 1);
                match &chain.steps[0] {
                    IterStep::Map { var, body } => {
                        assert_eq!(*var, IrPattern::ident("i"));
                        assert_eq!(**body, IrExpr::Var("i".to_string()));
                    }
                    other => panic!("Expected Map step, got {:?}", other),
                }
                assert_eq!(chain.terminal, IterTerminal::Collect);
            }
            other => panic!("Expected IterPipeline, got {:?}", other),
        }
    }

    #[test]
    fn test_array_generate_with_add_projection_len() {
        let ctx = empty_ctx();
        let gens = vec![
            length_param("K", vec![]),
            length_param("K2", vec![add_bound("K")]),
        ];
        let expr = IrExpr::ArrayGenerate {
            elem_ty: None,
            len: ArrayLength::Projection {
                r#type: Box::new(IrType::TypeParam("K2".to_string())),
                field: "Output".to_string(),
                trait_path: None,
            },
            index_var: "j".to_string(),
            body: Box::new(IrExpr::Var("j".to_string())),
        };
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        match result {
            IrExpr::IterPipeline(chain) => {
                match &chain.source {
                    IterChainSource::Range { end, .. } => {
                        assert_binary_var_var(end, "k2", SpecBinOp::Add, "k");
                    }
                    other => panic!("Expected Range source, got {:?}", other),
                }
            }
            other => panic!("Expected IterPipeline, got {:?}", other),
        }
    }

    #[test]
    fn test_array_generate_with_const_len() {
        let ctx = empty_ctx();
        let expr = IrExpr::ArrayGenerate {
            elem_ty: None,
            len: ArrayLength::TypeParam("U16".to_string()),
            index_var: "i".to_string(),
            body: Box::new(IrExpr::Var("i".to_string())),
        };
        let result = lower_expr_dyn(&expr, &ctx, &[]);
        match result {
            IrExpr::IterPipeline(chain) => {
                match &chain.source {
                    IterChainSource::Range { end, .. } => {
                        assert_eq!(**end, IrExpr::Lit(IrLit::Int(16)));
                    }
                    other => panic!("Expected Range source, got {:?}", other),
                }
            }
            other => panic!("Expected IterPipeline, got {:?}", other),
        }
    }

    // ================================================================
    // DefaultValue for array types → IterPipeline with recursive defaults
    // ================================================================

    #[test]
    #[should_panic(expected = "DefaultValue with inferred element type")]
    fn test_array_default_infer_panics() {
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![])];
        let expr = IrExpr::DefaultValue {
            ty: Some(Box::new(IrType::Array {
                kind: ArrayKind::GenericArray,
                elem: Box::new(IrType::Infer),
                len: ArrayLength::TypeParam("N".to_string()),
            })),
        };
        // Should panic because Infer element type is not allowed
        let _ = lower_expr_dyn(&expr, &ctx, &gens);
    }

    #[test]
    fn test_array_default_becomes_iter_pipeline() {
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![])];
        let expr = IrExpr::DefaultValue {
            ty: Some(Box::new(IrType::Array {
                kind: ArrayKind::GenericArray,
                elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                len: ArrayLength::TypeParam("N".to_string()),
            })),
        };
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        match result {
            IrExpr::IterPipeline(chain) => {
                match &chain.source {
                    IterChainSource::Range { start, end, .. } => {
                        assert_eq!(**start, IrExpr::Lit(IrLit::Int(0)));
                        assert_eq!(**end, IrExpr::Var("n".to_string()));
                    }
                    other => panic!("Expected Range source, got {:?}", other),
                }
                assert_eq!(chain.steps.len(), 1);
                match &chain.steps[0] {
                    IterStep::Map { var, body } => {
                        assert_eq!(*var, IrPattern::Wild);
                        // u8 element type → lowered to Lit(0)
                        assert_eq!(**body, IrExpr::Lit(IrLit::Int(0)));
                    }
                    other => panic!("Expected Map step, got {:?}", other),
                }
                assert!(matches!(chain.terminal, IterTerminal::CollectTyped(_)));
            }
            other => panic!("Expected IterPipeline, got {:?}", other),
        }
    }

    #[test]
    fn test_array_default_with_typed_elem() {
        let ctx = empty_ctx();
        let gens = vec![length_param("N", vec![])];
        let expr = IrExpr::DefaultValue {
            ty: Some(Box::new(IrType::Array {
                kind: ArrayKind::GenericArray,
                elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                len: ArrayLength::TypeParam("N".to_string()),
            })),
        };
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        match result {
            IrExpr::IterPipeline(chain) => {
                assert_eq!(chain.steps.len(), 1);
                match &chain.steps[0] {
                    IterStep::Map { var, body } => {
                        assert_eq!(*var, IrPattern::Wild);
                        // u8 element type → lowered to Lit(0)
                        assert_eq!(**body, IrExpr::Lit(IrLit::Int(0)));
                    }
                    other => panic!("Expected Map step, got {:?}", other),
                }
            }
            other => panic!("Expected IterPipeline, got {:?}", other),
        }
    }

    #[test]
    fn test_array_default_with_projection_len() {
        let ctx = empty_ctx();
        let gens = vec![type_param("B", vec![custom_bound("LengthDoubler")])];
        let expr = IrExpr::DefaultValue {
            ty: Some(Box::new(IrType::Array {
                kind: ArrayKind::GenericArray,
                elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                len: ArrayLength::Projection {
                    r#type: Box::new(IrType::TypeParam("B".to_string())),
                    field: "OutputSize".to_string(),
                    trait_path: None,
                },
            })),
        };
        let result = lower_expr_dyn(&expr, &ctx, &gens);
        match result {
            IrExpr::IterPipeline(chain) => {
                match &chain.source {
                    IterChainSource::Range { end, .. } => {
                        // The lowering should convert the Projection length to a ctx field access
                        // (not an ArrayLength::Projection, since it becomes an expr now)
                    }
                    other => panic!("Expected Range source, got {:?}", other),
                }
            }
            other => panic!("Expected IterPipeline, got {:?}", other),
        }
    }

    // ================================================================
    // validate_ident tests
    // ================================================================

    #[test]
    fn test_validate_ident_simple() {
        assert_eq!(validate_ident("n"), "n");
        assert_eq!(validate_ident("k2"), "k2");
        assert_eq!(validate_ident("self_output"), "self_output");
        assert_eq!(validate_ident("b_outputsize"), "b_outputsize");
    }

    #[test]
    #[should_panic(expected = "Invalid identifier")]
    fn test_validate_ident_rejects_parens() {
        validate_ident("(k2 + k)");
    }

    #[test]
    #[should_panic(expected = "Invalid identifier")]
    fn test_validate_ident_rejects_spaces() {
        validate_ident("k2 + k");
    }

    #[test]
    #[should_panic(expected = "Invalid identifier")]
    fn test_validate_ident_rejects_empty() {
        validate_ident("");
    }

    #[test]
    #[should_panic(expected = "Invalid identifier")]
    fn test_validate_ident_rejects_colons() {
        validate_ident("B::OutputSize");
    }

    #[test]
    fn test_logarithm2_projection_becomes_ilog2() {
        let ctx = empty_ctx();
        let gens = vec![type_param("D", vec![custom_bound("Digest")])];
        // <D::OutputSize as Logarithm2>::Output
        let len = ArrayLength::Projection {
            r#type: Box::new(IrType::Projection {
                base: Box::new(IrType::TypeParam("D".to_string())),
                trait_path: None,
                trait_args: Vec::new(),
                assoc: AssociatedType::OutputSize,
            }),
            field: "Output".to_string(),
            trait_path: Some("Logarithm2".to_string()),
        };
        let result = array_length_to_expr(&len, &gens, &ctx);
        // Should be Call(ilog2, [LengthOf(<D>::OutputSize)])
        match &result {
            IrExpr::Call { func, args } => {
                match func.as_ref() {
                    IrExpr::Path { segments, .. } => {
                        assert_eq!(segments, &["ilog2".to_string()]);
                    }
                    other => panic!("Expected Path func, got {:?}", other),
                }
                assert_eq!(args.len(), 1);
                // The inner arg should be LengthOf for D::OutputSize
                assert!(matches!(&args[0], IrExpr::LengthOf(ArrayLength::Projection { .. })));
            }
            other => panic!("Expected Call(ilog2), got {:?}", other),
        }
    }
}

#[cfg(test)]
mod rename_tests {
    use super::*;
    use crate::ir::*;
    
    #[test]
    fn rename_var_in_closure() {
        let mut block = IrBlock {
            stmts: vec![IrStmt::Expr(IrExpr::MethodCall {
                receiver: Box::new(IrExpr::Var("self".to_string())),
                method: MethodKind::Unknown("remap".to_string()),
                type_args: vec![],
                args: vec![
                    IrExpr::Field {
                        base: Box::new(IrExpr::Var("self".to_string())),
                        field: "n".to_string(),
                    },
                    IrExpr::Closure {
                        params: vec![IrClosureParam {
                            pattern: IrPattern::ident("a"),
                            ty: None,
                        }],
                        ret_type: None,
                        body: Box::new(IrExpr::MethodCall {
                            receiver: Box::new(IrExpr::Var("a".to_string())),
                            method: MethodKind::Unknown("wrapping_sub".to_string()),
                            type_args: vec![],
                            args: vec![IrExpr::Var("n".to_string())],
                        }),
                    },
                ],
            })],
            expr: None,
        };
        rename_var_in_block(&mut block, "n", "n_param");
        // Check that Var("n") in the closure body was renamed
        if let IrStmt::Expr(IrExpr::MethodCall { args, .. }) = &block.stmts[0] {
            if let IrExpr::Closure { body, .. } = &args[1] {
                if let IrExpr::MethodCall { args: inner_args, .. } = body.as_ref() {
                    assert_eq!(inner_args[0], IrExpr::Var("n_param".to_string()));
                    return;
                }
            }
        }
        panic!("Could not find renamed var in closure body");
    }
}
