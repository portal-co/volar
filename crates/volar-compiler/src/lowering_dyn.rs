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
use std::{boxed::Box, collections::{BTreeMap, BTreeSet}, format, string::ToString, vec, vec::Vec};

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, collections::{BTreeMap, BTreeSet}, format, string::ToString, vec, vec::Vec};

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
    /// Projection witness params injected per function, keyed by function name.
    /// E.g., "create_vole_from_material" → ["b_outputsize"]
    pub fn_projection_witnesses: BTreeMap<String, Vec<String>>,
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

        // Pre-compute projection witnesses for all functions (free functions and impl methods)
        let mut fn_projection_witnesses = BTreeMap::new();
        for f in &module.functions {
            let witnesses = collect_projection_witnesses_fn(f);
            if !witnesses.is_empty() {
                fn_projection_witnesses
                    .insert(f.name.clone(), witnesses.into_iter().collect());
            }
        }
        for im in &module.impls {
            for item in &im.items {
                if let IrImplItem::Method(f) = item {
                    let witnesses = collect_projection_witnesses_fn(f);
                    if !witnesses.is_empty() {
                        fn_projection_witnesses
                            .insert(f.name.clone(), witnesses.into_iter().collect());
                    }
                }
            }
        }

        Self { struct_info, length_aliases, fn_projection_witnesses }
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
                } => IrTraitItem::AssociatedType {
                    name: name.clone(),
                    bounds: bounds
                        .iter()
                        .filter_map(|b| lower_trait_bound_dyn(b, &empty_gen, &empty_gen))
                        .collect(),
                    default: default
                        .as_ref()
                        .map(|d| lower_type_dyn(d, ctx, &empty_gen)),
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

    // Inject projection witness parameters (e.g., b_outputsize: usize from B::OutputSize)
    // Skip for trait impl methods — the trait signature is fixed.
    if !is_trait_impl {
        let proj_witnesses = collect_projection_witnesses_fn(f);
        for name in &proj_witnesses {
            if !params.iter().any(|p: &IrParam| &p.name == name) {
                params.push(IrParam {
                    name: name.clone(),
                    ty: IrType::Primitive(PrimitiveType::Usize),
                });
            }
        }
    }

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
                for w in &info.length_witnesses {
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

    // Post-lowering: scan lowered body for projection witness variables that aren't
    // already defined as params. This handles methods that call functions with
    // projection witness params — the callee's witnesses get passed through.
    // Skip for trait impl methods — the trait signature is fixed.
    if !is_trait_impl {
        let all_witness_names: BTreeSet<String> = ctx
            .fn_projection_witnesses
            .values()
            .flat_map(|ws| ws.iter().cloned())
            .collect();
        let mut referenced_witnesses = BTreeSet::new();
        collect_var_refs_matching(&body, &all_witness_names, &mut referenced_witnesses);
        for w in &referenced_witnesses {
            if !params.iter().any(|p: &IrParam| &p.name == w) {
                // Check if it's available from self (struct witness field)
                let available_from_self = if let Some(sname) = self_struct {
                    ctx.struct_info
                        .get(sname)
                        .map_or(false, |info| info.length_witnesses.contains(w))
                } else {
                    false
                };
                if !available_from_self {
                    params.push(IrParam {
                        name: w.clone(),
                        ty: IrType::Primitive(PrimitiveType::Usize),
                    });
                }
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
                return IrExpr::Var("Vec".to_string());
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
                    args.insert(0, IrExpr::Var("n".to_string()));
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
            IrExpr::MethodCall {
                receiver: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
                method,
                type_args: type_args
                    .iter()
                    .map(|ta| lower_type_dyn(ta, ctx, fn_gen))
                    .collect(),
                args,
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
            if let IrExpr::Var(name) = &func {
                if name == "create_vole_from_material"
                    || name == "create_vole_from_material_expanded"
                    || name == "double"
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

            // Prepend projection witness arguments for known callee functions
            let callee_name = match &func {
                IrExpr::Var(name) => Some(name.as_str()),
                IrExpr::Path { segments, .. } => segments.last().map(|s| s.as_str()),
                _ => None,
            };
            let mut final_args = Vec::new();
            if let Some(name) = callee_name {
                if let Some(witnesses) = ctx.fn_projection_witnesses.get(name) {
                    for w in witnesses {
                        // Only prepend if not already present as an argument
                        if !final_args.iter().any(|a| matches!(a, IrExpr::Var(v) if v == w)) {
                            final_args.push(IrExpr::Var(w.clone()));
                        }
                    }
                }
            }
            final_args.extend(args);

            // Rename tuple struct constructors (e.g., CommitmentCore → CommitmentCoreDyn)
            let func_name = match &func {
                IrExpr::Path { segments, .. } => segments.last().map(|s| s.clone()),
                IrExpr::Var(n) => Some(n.clone()),
                _ => None,
            };
            if let Some(ref name) = func_name {
                if ctx.struct_info.contains_key(name.as_str()) {
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
        IrExpr::ArrayGenerate {
            elem_ty,
            len,
            index_var,
            body,
        } => {
            let lowered_len = lower_array_length(len);
            IrExpr::ArrayGenerate {
                elem_ty: elem_ty
                    .as_ref()
                    .map(|t| Box::new(lower_type_dyn(t, ctx, fn_gen))),
                len: lowered_len,
                index_var: index_var.clone(),
                body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
            }
        }
        IrExpr::ArrayDefault { elem_ty, len } => {
            let lowered_len = lower_array_length(len);
            IrExpr::ArrayDefault {
                elem_ty: elem_ty
                    .as_ref()
                    .map(|t| Box::new(lower_type_dyn(t, ctx, fn_gen))),
                len: lowered_len,
            }
        }
        IrExpr::LengthOf(len) => {
            let lowered = lower_array_length(len);
            match &lowered {
                ArrayLength::Const(n) => IrExpr::Lit(crate::ir::IrLit::Int(*n as i128)),
                ArrayLength::TypeParam(p) => IrExpr::Var(p.clone()),
                _ => IrExpr::LengthOf(lowered),
            }
        }
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
                    var: format!("({}, {})", left_var, right_var),
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
                    acc_var: acc_var.clone(),
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
        IterTerminal::Fold {
            init,
            acc_var,
            elem_var,
            body,
        } => IterTerminal::Fold {
            init: Box::new(lower_expr_dyn(init, ctx, fn_gen)),
            acc_var: acc_var.clone(),
            elem_var: elem_var.clone(),
            body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
        },
        IterTerminal::Lazy => IterTerminal::Lazy,
    }
}

fn lower_array_length(len: &ArrayLength) -> ArrayLength {
    match len {
        ArrayLength::Projection { r#type, field } => {
            // Type-level projection like <B>::OutputSize → becomes a runtime variable name
            let base_name = match r#type.as_ref() {
                IrType::TypeParam(p) => p.to_lowercase(),
                _ => "len".to_string(),
            };
            ArrayLength::TypeParam(format!("{}_{}", base_name, field.to_lowercase()))
        }
        ArrayLength::TypeParam(p) => {
            // Check if this is a typenum constant
            if let Some(tn) = TypeNumConst::from_str(p) {
                ArrayLength::Const(tn.to_usize())
            } else if p.contains("::") {
                // Path like B::BlockSize → runtime variable
                let parts: Vec<&str> = p.split("::").collect();
                if parts.len() == 2 {
                    ArrayLength::TypeParam(format!("{}_{}", parts[0].to_lowercase(), parts[1].to_lowercase()))
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

// ============================================================================
// PROJECTION WITNESS COLLECTION
// ============================================================================

/// Compute the runtime witness name for an ArrayLength::Projection.
fn projection_witness_name(len: &ArrayLength) -> Option<String> {
    match len {
        ArrayLength::Projection { r#type, field } => {
            let base = match r#type.as_ref() {
                IrType::TypeParam(p) => p.to_lowercase(),
                _ => return None,
            };
            Some(format!("{}_{}", base, field.to_lowercase()))
        }
        ArrayLength::TypeParam(p) if p.contains("::") => {
            let parts: Vec<&str> = p.split("::").collect();
            if parts.len() == 2 {
                Some(format!("{}_{}", parts[0].to_lowercase(), parts[1].to_lowercase()))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Collect all projection-derived witness names from expressions.
fn collect_projection_witnesses_expr(expr: &IrExpr, out: &mut BTreeSet<String>) {
    match expr {
        IrExpr::LengthOf(len) => {
            if let Some(name) = projection_witness_name(len) {
                out.insert(name);
            }
        }
        IrExpr::ArrayDefault { len, .. } => {
            if let Some(name) = projection_witness_name(len) {
                out.insert(name);
            }
        }
        IrExpr::ArrayGenerate { len, body, .. } => {
            if let Some(name) = projection_witness_name(len) {
                out.insert(name);
            }
            collect_projection_witnesses_expr(body, out);
        }
        IrExpr::Repeat { elem, len } => {
            collect_projection_witnesses_expr(elem, out);
            collect_projection_witnesses_expr(len, out);
        }
        IrExpr::Binary { left, right, .. } => {
            collect_projection_witnesses_expr(left, out);
            collect_projection_witnesses_expr(right, out);
        }
        IrExpr::Unary { expr, .. } => {
            collect_projection_witnesses_expr(expr, out);
        }
        IrExpr::Call { args, .. } => {
            for a in args {
                collect_projection_witnesses_expr(a, out);
            }
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            collect_projection_witnesses_expr(receiver, out);
            for a in args {
                collect_projection_witnesses_expr(a, out);
            }
        }
        IrExpr::Field { base, .. } => {
            collect_projection_witnesses_expr(base, out);
        }
        IrExpr::Index { base, index } => {
            collect_projection_witnesses_expr(base, out);
            collect_projection_witnesses_expr(index, out);
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            collect_projection_witnesses_expr(cond, out);
            collect_projection_witnesses_block(then_branch, out);
            if let Some(e) = else_branch {
                collect_projection_witnesses_expr(e, out);
            }
        }
        IrExpr::Block(b) => {
            collect_projection_witnesses_block(b, out);
        }
        IrExpr::BoundedLoop { body, .. } => {
            collect_projection_witnesses_block(body, out);
        }
        IrExpr::IterLoop { collection, body, .. } => {
            collect_projection_witnesses_expr(collection, out);
            collect_projection_witnesses_block(body, out);
        }
        IrExpr::Closure { body, .. } => {
            collect_projection_witnesses_expr(body, out);
        }
        IrExpr::Assign { left, right } => {
            collect_projection_witnesses_expr(left, out);
            collect_projection_witnesses_expr(right, out);
        }
        IrExpr::StructExpr { fields, .. } => {
            for (_, e) in fields {
                collect_projection_witnesses_expr(e, out);
            }
        }
        IrExpr::Tuple(elems) | IrExpr::Array(elems) => {
            for e in elems {
                collect_projection_witnesses_expr(e, out);
            }
        }
        IrExpr::Cast { expr, .. } => {
            collect_projection_witnesses_expr(expr, out);
        }
        IrExpr::Return(Some(e)) => {
            collect_projection_witnesses_expr(e, out);
        }
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_projection_witnesses_expr(s, out);
            }
            if let Some(e) = end {
                collect_projection_witnesses_expr(e, out);
            }
        }
        IrExpr::RawMap { receiver, body, .. } => {
            collect_projection_witnesses_expr(receiver, out);
            collect_projection_witnesses_expr(body, out);
        }
        IrExpr::RawZip { left, right, body, .. } => {
            collect_projection_witnesses_expr(left, out);
            collect_projection_witnesses_expr(right, out);
            collect_projection_witnesses_expr(body, out);
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            collect_projection_witnesses_expr(receiver, out);
            collect_projection_witnesses_expr(init, out);
            collect_projection_witnesses_expr(body, out);
        }
        IrExpr::IterPipeline(chain) => {
            collect_projection_witnesses_chain(chain, out);
        }
        IrExpr::Match { expr, arms } => {
            collect_projection_witnesses_expr(expr, out);
            for arm in arms {
                collect_projection_witnesses_expr(&arm.body, out);
            }
        }
        _ => {}
    }
}

fn collect_projection_witnesses_chain(chain: &crate::ir::IrIterChain, out: &mut BTreeSet<String>) {
    match &chain.source {
        crate::ir::IterChainSource::Method { collection, .. } => {
            collect_projection_witnesses_expr(collection, out);
        }
        crate::ir::IterChainSource::Zip { left, right } => {
            collect_projection_witnesses_chain(left, out);
            collect_projection_witnesses_chain(right, out);
        }
        crate::ir::IterChainSource::Range { start, end, .. } => {
            collect_projection_witnesses_expr(start, out);
            collect_projection_witnesses_expr(end, out);
        }
    }
    for step in &chain.steps {
        match step {
            crate::ir::IterStep::Map { body, .. }
            | crate::ir::IterStep::Filter { body, .. }
            | crate::ir::IterStep::FilterMap { body, .. }
            | crate::ir::IterStep::FlatMap { body, .. } => {
                collect_projection_witnesses_expr(body, out);
            }
            crate::ir::IterStep::Take { count }
            | crate::ir::IterStep::Skip { count } => {
                collect_projection_witnesses_expr(count, out);
            }
            crate::ir::IterStep::Chain { other } => {
                collect_projection_witnesses_chain(other, out);
            }
            crate::ir::IterStep::Enumerate => {}
        }
    }
    match &chain.terminal {
        crate::ir::IterTerminal::Fold { init, body, .. } => {
            collect_projection_witnesses_expr(init, out);
            collect_projection_witnesses_expr(body, out);
        }
        _ => {}
    }
}

/// Collect all projection-derived witness names from types (array lengths).
fn collect_projection_witnesses_type(ty: &IrType, out: &mut BTreeSet<String>) {
    match ty {
        IrType::Array { len, elem, .. } => {
            if let Some(name) = projection_witness_name(len) {
                out.insert(name);
            }
            collect_projection_witnesses_type(elem, out);
        }
        IrType::Vector { elem } | IrType::Reference { elem, .. } => {
            collect_projection_witnesses_type(elem, out);
        }
        IrType::Tuple(elems) => {
            for e in elems {
                collect_projection_witnesses_type(e, out);
            }
        }
        IrType::Struct { type_args, .. } => {
            for a in type_args {
                collect_projection_witnesses_type(a, out);
            }
        }
        _ => {}
    }
}

/// Collect all projection-derived witness names from a statement.
fn collect_projection_witnesses_stmt(stmt: &IrStmt, out: &mut BTreeSet<String>) {
    match stmt {
        IrStmt::Let { ty, init, .. } => {
            if let Some(t) = ty {
                collect_projection_witnesses_type(t, out);
            }
            if let Some(e) = init {
                collect_projection_witnesses_expr(e, out);
            }
        }
        IrStmt::Expr(e) | IrStmt::Semi(e) => {
            collect_projection_witnesses_expr(e, out);
        }
    }
}

/// Collect projection witness names from a block.
fn collect_projection_witnesses_block(block: &IrBlock, out: &mut BTreeSet<String>) {
    for stmt in &block.stmts {
        collect_projection_witnesses_stmt(stmt, out);
    }
    if let Some(e) = &block.expr {
        collect_projection_witnesses_expr(e, out);
    }
}

/// Collect all projection witness names from a function (params, return type, body).
fn collect_projection_witnesses_fn(f: &IrFunction) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for p in &f.params {
        collect_projection_witnesses_type(&p.ty, &mut out);
    }
    if let Some(rt) = &f.return_type {
        collect_projection_witnesses_type(rt, &mut out);
    }
    collect_projection_witnesses_block(&f.body, &mut out);
    out
}

/// Collect `Var` names from a lowered IrBlock that match a set of known witness names.
fn collect_var_refs_matching(block: &IrBlock, known: &BTreeSet<String>, out: &mut BTreeSet<String>) {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { init: Some(e), .. } | IrStmt::Expr(e) | IrStmt::Semi(e) => {
                collect_var_refs_matching_expr(e, known, out);
            }
            _ => {}
        }
    }
    if let Some(e) = &block.expr {
        collect_var_refs_matching_expr(e, known, out);
    }
}

fn collect_var_refs_matching_expr(expr: &IrExpr, known: &BTreeSet<String>, out: &mut BTreeSet<String>) {
    match expr {
        IrExpr::Var(name) => {
            if known.contains(name) {
                out.insert(name.clone());
            }
        }
        IrExpr::Call { func, args } => {
            collect_var_refs_matching_expr(func, known, out);
            for a in args {
                collect_var_refs_matching_expr(a, known, out);
            }
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            collect_var_refs_matching_expr(receiver, known, out);
            for a in args {
                collect_var_refs_matching_expr(a, known, out);
            }
        }
        IrExpr::Binary { left, right, .. } => {
            collect_var_refs_matching_expr(left, known, out);
            collect_var_refs_matching_expr(right, known, out);
        }
        IrExpr::Unary { expr, .. } => {
            collect_var_refs_matching_expr(expr, known, out);
        }
        IrExpr::Field { base, .. } => {
            collect_var_refs_matching_expr(base, known, out);
        }
        IrExpr::Index { base, index } => {
            collect_var_refs_matching_expr(base, known, out);
            collect_var_refs_matching_expr(index, known, out);
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            collect_var_refs_matching_expr(cond, known, out);
            collect_var_refs_matching(then_branch, known, out);
            if let Some(e) = else_branch {
                collect_var_refs_matching_expr(e, known, out);
            }
        }
        IrExpr::Block(b) => {
            collect_var_refs_matching(b, known, out);
        }
        IrExpr::BoundedLoop { body, .. } => {
            collect_var_refs_matching(body, known, out);
        }
        IrExpr::IterLoop { collection, body, .. } => {
            collect_var_refs_matching_expr(collection, known, out);
            collect_var_refs_matching(body, known, out);
        }
        IrExpr::Closure { body, .. } => {
            collect_var_refs_matching_expr(body, known, out);
        }
        IrExpr::Assign { left, right } => {
            collect_var_refs_matching_expr(left, known, out);
            collect_var_refs_matching_expr(right, known, out);
        }
        IrExpr::StructExpr { fields, .. } => {
            for (_, e) in fields {
                collect_var_refs_matching_expr(e, known, out);
            }
        }
        IrExpr::Tuple(elems) | IrExpr::Array(elems) => {
            for e in elems {
                collect_var_refs_matching_expr(e, known, out);
            }
        }
        IrExpr::Cast { expr, .. } => {
            collect_var_refs_matching_expr(expr, known, out);
        }
        IrExpr::Return(Some(e)) => {
            collect_var_refs_matching_expr(e, known, out);
        }
        IrExpr::Range { start, end, .. } => {
            if let Some(s) = start { collect_var_refs_matching_expr(s, known, out); }
            if let Some(e) = end { collect_var_refs_matching_expr(e, known, out); }
        }
        IrExpr::Repeat { elem, len } => {
            collect_var_refs_matching_expr(elem, known, out);
            collect_var_refs_matching_expr(len, known, out);
        }
        IrExpr::IterPipeline(chain) => {
            collect_var_refs_matching_chain(chain, known, out);
        }
        IrExpr::Match { expr, arms } => {
            collect_var_refs_matching_expr(expr, known, out);
            for arm in arms {
                collect_var_refs_matching_expr(&arm.body, known, out);
            }
        }
        IrExpr::ArrayGenerate { body, .. } => {
            collect_var_refs_matching_expr(body, known, out);
        }
        IrExpr::RawMap { receiver, body, .. } => {
            collect_var_refs_matching_expr(receiver, known, out);
            collect_var_refs_matching_expr(body, known, out);
        }
        IrExpr::RawZip { left, right, body, .. } => {
            collect_var_refs_matching_expr(left, known, out);
            collect_var_refs_matching_expr(right, known, out);
            collect_var_refs_matching_expr(body, known, out);
        }
        IrExpr::RawFold { receiver, init, body, .. } => {
            collect_var_refs_matching_expr(receiver, known, out);
            collect_var_refs_matching_expr(init, known, out);
            collect_var_refs_matching_expr(body, known, out);
        }
        _ => {}
    }
}

fn collect_var_refs_matching_chain(chain: &crate::ir::IrIterChain, known: &BTreeSet<String>, out: &mut BTreeSet<String>) {
    match &chain.source {
        crate::ir::IterChainSource::Method { collection, .. } => {
            collect_var_refs_matching_expr(collection, known, out);
        }
        crate::ir::IterChainSource::Zip { left, right } => {
            collect_var_refs_matching_chain(left, known, out);
            collect_var_refs_matching_chain(right, known, out);
        }
        crate::ir::IterChainSource::Range { start, end, .. } => {
            collect_var_refs_matching_expr(start, known, out);
            collect_var_refs_matching_expr(end, known, out);
        }
    }
    for step in &chain.steps {
        match step {
            crate::ir::IterStep::Map { body, .. }
            | crate::ir::IterStep::Filter { body, .. }
            | crate::ir::IterStep::FilterMap { body, .. }
            | crate::ir::IterStep::FlatMap { body, .. } => {
                collect_var_refs_matching_expr(body, known, out);
            }
            crate::ir::IterStep::Take { count }
            | crate::ir::IterStep::Skip { count } => {
                collect_var_refs_matching_expr(count, known, out);
            }
            crate::ir::IterStep::Chain { other } => {
                collect_var_refs_matching_chain(other, known, out);
            }
            crate::ir::IterStep::Enumerate => {}
        }
    }
    match &chain.terminal {
        crate::ir::IterTerminal::Fold { init, body, .. } => {
            collect_var_refs_matching_expr(init, known, out);
            collect_var_refs_matching_expr(body, known, out);
        }
        _ => {}
    }
}
