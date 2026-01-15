//! Dynamic IR Lowering
//!
//! This module transforms the specialized volar-spec IR into a dynamic IR
//! where type-level lengths become runtime usize witnesses.

use crate::ir::*;

#[cfg(feature = "std")]
use std::string::String;
#[cfg(not(feature = "std"))]
use alloc::string::String;

#[cfg(feature = "std")]
use std::{
    boxed::Box,
    collections::BTreeMap,
    string::ToString,
    vec,
    vec::Vec,
    format,
};

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    collections::BTreeMap,
    string::ToString,
    vec,
    vec::Vec,
    format,
};

/// Classification of generic parameters
#[derive(Debug, Clone, PartialEq)]
pub enum GenericKind {
    /// Length/size parameter - becomes runtime usize
    Length,
    /// Crypto trait (BlockCipher, Digest) - remains generic
    Crypto,
    /// Regular type parameter - remains generic  
    Type,
}

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
    /// Original generics in declaration order with their kind (Length/Crypto/Type) and param kind
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
}

impl LoweringContext {
    pub fn new(module: &IrModule) -> Self {
        let mut struct_info = BTreeMap::new();

        // Pass 1: Basic info
        for s in &module.structs {
            let mut info = StructInfo::default();
            info.kind = s.kind.clone();
            
            for p in &s.generics {
                let kind = if p.kind == IrGenericParamKind::Lifetime {
                    GenericKind::Type
                } else {
                    classify_generic(p, &[&s.generics])
                };
                
                info.orig_generics.push((p.name.clone(), kind.clone(), p.kind.clone()));
                info.generic_defaults.push(p.default.clone());

                if p.kind == IrGenericParamKind::Lifetime {
                    info.lifetimes.push(p.name.clone());
                } else {
                    match kind {
                        GenericKind::Length => info.length_witnesses.push(p.name.to_lowercase()),
                        GenericKind::Crypto | GenericKind::Type => {
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
                let used_type_params: Vec<String> = s.fields.iter()
                    .flat_map(|f| collect_type_params(&f.ty))
                    .collect();
                
                // Check if any type params are unused
                let unused_params: Vec<_> = info.type_params.iter()
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

        Self { struct_info }
    }
}

pub fn classify_generic(param: &IrGenericParam, all_params: &[&[IrGenericParam]]) -> GenericKind {
    if param.kind == IrGenericParamKind::Const {
        return GenericKind::Length;
    }

    // Direct indicators in bounds
    for bound in &param.bounds {
        if is_length_bound(bound) {
            return GenericKind::Length;
        }
        if is_crypto_bound(bound) {
            return GenericKind::Crypto;
        }
        // Fn/FnMut/FnOnce bounds indicate a closure type parameter, not a length
        if is_fn_bound(bound) {
            return GenericKind::Type;
        }
    }

    // Recursive check via param name
    let mut visited = Vec::new();
    if is_length_name(&param.name, all_params, &mut visited) {
        return GenericKind::Length;
    }

    // If the param has *any* bounds (that aren't length-related), it's a type param
    // This catches cases like U: Mul<T, Output = O> + Clone
    if !param.bounds.is_empty() {
        return GenericKind::Type;
    }

    // Default: assume it's a type parameter
    // We no longer use the aggressive single-letter heuristic since it causes
    // false positives (e.g., F, U, O, A, Q are often type params, not lengths)
    GenericKind::Type
}

fn is_length_bound(bound: &IrTraitBound) -> bool {
    matches!(
        &bound.trait_kind,
        TraitKind::Crypto(CryptoTrait::ArrayLength)
            | TraitKind::Crypto(CryptoTrait::VoleArray)
            | TraitKind::Math(MathTrait::Unsigned)
    )
}

fn is_crypto_bound(bound: &IrTraitBound) -> bool {
    matches!(
        &bound.trait_kind,
        TraitKind::Crypto(CryptoTrait::BlockCipher)
            | TraitKind::Crypto(CryptoTrait::Digest)
            | TraitKind::Crypto(CryptoTrait::Rng)
            | TraitKind::Crypto(CryptoTrait::ByteBlockEncrypt)
    )
}

fn is_fn_bound(bound: &IrTraitBound) -> bool {
    matches!(&bound.trait_kind, TraitKind::Fn(_, _))
}

fn is_math_op_bound(bound: &IrTraitBound) -> bool {
    matches!(
        &bound.trait_kind,
        TraitKind::Math(MathTrait::Add)
            | TraitKind::Math(MathTrait::Sub)
            | TraitKind::Math(MathTrait::Mul)
            | TraitKind::Math(MathTrait::Div)
    )
}

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
            if is_length_bound(bound) {
                return true;
            }
            if is_math_op_bound(bound) {
                for arg in &bound.type_args {
                    if type_refers_to_length(arg, all_params, visited) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn type_refers_to_length(
    ty: &IrType,
    all_params: &[&[IrGenericParam]],
    visited: &mut Vec<String>,
) -> bool {
    match ty {
        IrType::TypeParam(name) => is_length_name(name, all_params, visited),
        IrType::Struct { type_args, .. } => type_args
            .iter()
            .any(|ta| type_refers_to_length(ta, all_params, visited)),
        IrType::Projection { base, .. } => type_refers_to_length(base, all_params, visited),
        IrType::Param { path } => path
            .first()
            .map(|s| is_length_name(s, all_params, visited))
            .unwrap_or(false),
        IrType::Array { elem, .. }
        | IrType::Vector { elem }
        | IrType::Reference { elem, .. } => type_refers_to_length(elem, all_params, visited),
        IrType::Tuple(elems) => elems
            .iter()
            .any(|e| type_refers_to_length(e, all_params, visited)),
        _ => false,
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

    for im in &module.impls {
        // Skip ByteBlockEncrypt blanket impl (it's in the header)
        if let Some(tr) = &im.trait_ {
            if let TraitKind::Crypto(CryptoTrait::ByteBlockEncrypt) = &tr.kind {
                if let IrType::TypeParam(_) = &im.self_ty {
                    continue;
                }
            }
            // Skip VoleArray trait impl (it's a marker trait that doesn't apply in dynamic context)
            if let TraitKind::Crypto(CryptoTrait::VoleArray) = &tr.kind {
                continue;
            }
        }
        lowered.impls.push(lower_impl_dyn(im, &ctx));
    }

    for f in &module.functions {
        lowered.functions.push(lower_function_dyn(f, &ctx, None, None, &BTreeMap::new()));
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

fn lower_struct_dyn(s: &IrStruct, ctx: &LoweringContext) -> IrStruct {
    let info = ctx.struct_info.get(&s.kind.to_string()).unwrap();
    let mut fields = Vec::new();

    // Don't add lifetimes since we convert &[T] to Vec<T>
    let generics = lower_generics_dyn(&s.generics, &[]);

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
    let used_type_params: Vec<String> = fields.iter()
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
            IrType::Tuple(phantom_types.iter().map(|n| IrType::TypeParam(n.clone())).collect())
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
        IrType::Struct { type_args, .. } => type_args.iter().flat_map(collect_type_params).collect(),
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
            IrWherePredicate::TypeBound { ty: IrType::TypeParam(name), bounds } => {
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
                items.push(IrImplItem::Method(lower_function_dyn(f, ctx, Some(&self_kind), Some(&impl_gen), &constant_witnesses)));
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
        generics: lower_generics_dyn(&impl_gen, &[]),
        trait_: im.trait_.as_ref().map(|tr| IrTraitRef {
            kind: tr.kind.clone(), 
            type_args: tr.type_args.iter().map(|ta| lower_type_dyn(ta, ctx, &impl_gen)).collect(),
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
) -> IrFunction {
    let mut params = Vec::new();
    let empty_gen = Vec::new();
    let impl_gen = impl_generics.unwrap_or(&empty_gen);
    
    let mut fn_gen = f.generics.clone();
    for wp in &f.where_clause {
        match wp {
            IrWherePredicate::TypeBound { ty: IrType::TypeParam(name), bounds } => {
                if let Some(p) = fn_gen.iter_mut().find(|p| &p.name == name) {
                    p.bounds.extend(bounds.clone());
                }
            }
            _ => {}
        }
    }

    // Length generics become parameters
    for p in &fn_gen {
        let kind = classify_generic(p, &[&fn_gen, impl_gen]);
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

    // Static methods also need impl-level length params
    if f.receiver.is_none() {
        for p in impl_gen {
            let kind = classify_generic(p, &[impl_gen]);
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

    for p in &f.params {
        params.push(IrParam {
            name: p.name.clone(),
            ty: lower_type_dyn(&p.ty, ctx, &fn_gen),
        });
    }

    let mut body = lower_block_dyn(&f.body, ctx, &fn_gen);

    // Unpack witnesses in methods
    if f.receiver.is_some() {
        if let Some(sname) = self_struct {
            if let Some(info) = ctx.struct_info.get(sname) {
                let mut unpacks = Vec::new();
                for w in &info.length_witnesses {
                    unpacks.push(IrStmt::Let {
                        pattern: IrPattern::Ident { mutable: false, name: w.clone(), subpat: None },
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
                        pattern: IrPattern::Ident { mutable: false, name: name.clone(), subpat: None },
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
        let kind = classify_generic(p, &[&fn_gen, impl_gen]);
        if p.kind != IrGenericParamKind::Lifetime && kind != GenericKind::Length {
            combined_gen.push(p.clone());
        }
    }
    for p in impl_gen {
        let kind = classify_generic(p, &[impl_gen, &fn_gen]);
        if p.kind != IrGenericParamKind::Lifetime && kind != GenericKind::Length {
            if !combined_gen.iter().any(|cp| cp.name == p.name) {
                combined_gen.push(p.clone());
            }
        }
    }

    IrFunction {
        name: f.name.clone(),
        generics: lower_generics_dyn(&f.generics, impl_gen),
        receiver: f.receiver,
        params,
        return_type: f.return_type.as_ref().map(|rt| lower_type_dyn(rt, ctx, &fn_gen)),
        where_clause: lower_where_clause_dyn(&f.where_clause, ctx, &combined_gen),
        body,
    }
}

fn lower_generics_dyn(generics: &[IrGenericParam], extra_ctx: &[IrGenericParam]) -> Vec<IrGenericParam> {
    generics.iter()
        .filter(|p| {
            let kind = classify_generic(p, &[generics, extra_ctx]);
            p.kind != IrGenericParamKind::Lifetime && kind != GenericKind::Length
        })
        .map(|p| {
            let mut p = p.clone();
            p.bounds = p.bounds.iter().filter_map(|b| lower_trait_bound_dyn(b, generics, extra_ctx)).collect();
            p
        })
        .collect()
}

fn lower_trait_bound_dyn(b: &IrTraitBound, _generics: &[IrGenericParam], _extra_ctx: &[IrGenericParam]) -> Option<IrTraitBound> {
    if is_length_bound(b) { return None; } match &b.trait_kind { TraitKind::Math(MathTrait::Unsigned) => return None, TraitKind::Crypto(CryptoTrait::ArrayLength | CryptoTrait::VoleArray) => return None, _ => {} }
    let b = b.clone();
    Some(b)
}

fn lower_type_dyn(ty: &IrType, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrType {
    lower_type_dyn_inner(ty, ctx, fn_gen, false)
}

fn lower_type_dyn_for_field(ty: &IrType, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrType {
    lower_type_dyn_inner(ty, ctx, fn_gen, true)
}

fn lower_type_dyn_inner(ty: &IrType, ctx: &LoweringContext, fn_gen: &[IrGenericParam], in_struct_field: bool) -> IrType {
    match ty {
        IrType::Array { kind, elem, len } => {
            if *kind == ArrayKind::Slice {
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
                let non_lifetime_generics: Vec<_> = info.orig_generics.iter()
                    .filter(|(_, _, pk)| *pk != IrGenericParamKind::Lifetime)
                    .collect();
                
                for (idx, arg) in type_args.iter().enumerate() {
                    if let Some((_, gkind, _)) = non_lifetime_generics.get(idx) {
                        if *gkind == GenericKind::Length { continue; }
                    }
                    lowered_args.push(lower_type_dyn_inner(arg, ctx, fn_gen, in_struct_field));
                }
            } else {
                lowered_args = type_args.iter().map(|ta| lower_type_dyn_inner(ta, ctx, fn_gen, in_struct_field)).collect();
            }
            
            let new_kind = if kind_str == "GenericArray" || kind_str == "Option" || kind_str == "Result" || kind_str == "Box" || kind_str == "Vec" {
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
                if let IrType::Array { kind: ArrayKind::Slice, elem: inner, .. } = elem.as_ref() {
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
        IrType::Tuple(elems) => IrType::Tuple(elems.iter().map(|e| lower_type_dyn_inner(e, ctx, fn_gen, in_struct_field)).collect()),
        IrType::TypeParam(p) => {
            IrType::TypeParam(p.clone())
        }
        _ => ty.clone(),
    }
}

fn lower_where_clause_dyn(wc: &[IrWherePredicate], ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> Vec<IrWherePredicate> {
    // Collect all known type params from fn_gen
    let known_params: Vec<String> = fn_gen.iter()
        .filter(|p| p.kind != IrGenericParamKind::Lifetime)
        .map(|p| p.name.clone())
        .collect();
    
    wc.iter().filter_map(|wp| {
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
                
                let bounds: Vec<_> = bounds.iter().filter_map(|b| lower_trait_bound_dyn(b, fn_gen, &[])).collect();
                if bounds.is_empty() { return None; }
                
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
                
                Some(IrWherePredicate::TypeBound {
                    ty,
                    bounds,
                })
            }
        }
    }).collect()
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
        IrType::Struct { type_args, .. } => {
            type_args.iter().any(|a| has_undefined_type_param(a, known_params))
        }
        IrType::Reference { elem, .. } | IrType::Vector { elem } => {
            has_undefined_type_param(elem, known_params)
        }
        IrType::Tuple(elems) => {
            elems.iter().any(|e| has_undefined_type_param(e, known_params))
        }
        IrType::Array { elem, .. } => {
            has_undefined_type_param(elem, known_params)
        }
        _ => false,
    }
}

fn lower_block_dyn(block: &IrBlock, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrBlock {
    IrBlock {
        stmts: block.stmts.iter().map(|s| lower_stmt_dyn(s, ctx, fn_gen)).collect(),
        expr: block.expr.as_ref().map(|e| Box::new(lower_expr_dyn(e, ctx, fn_gen))),
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
        IrPattern::Ident { mutable, name, subpat } => IrPattern::Ident {
            mutable: *mutable,
            name: name.clone(),
            subpat: subpat.as_ref().map(|sp| Box::new(lower_pattern_dyn(sp, ctx))),
        },
        IrPattern::Tuple(elems) => IrPattern::Tuple(
            elems.iter().map(|e| lower_pattern_dyn(e, ctx)).collect()
        ),
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
                fields: fields.iter().map(|(n, p)| (n.clone(), lower_pattern_dyn(p, ctx))).collect(),
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
        IrPattern::Slice(elems) => IrPattern::Slice(
            elems.iter().map(|e| lower_pattern_dyn(e, ctx)).collect()
        ),
        IrPattern::Ref { mutable, pat } => IrPattern::Ref {
            mutable: *mutable,
            pat: Box::new(lower_pattern_dyn(pat, ctx)),
        },
        IrPattern::Or(pats) => IrPattern::Or(
            pats.iter().map(|p| lower_pattern_dyn(p, ctx)).collect()
        ),
        _ => p.clone(),
    }
}

fn lower_expr_dyn(e: &IrExpr, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrExpr {
    match e {
        IrExpr::Var(v) => {
            if v.len() == 1 && v.chars().next().unwrap().is_uppercase() {
                if !v.starts_with('T') && !v.starts_with('B') && !v.starts_with('D') {
                    return IrExpr::Var(v.to_lowercase());
                }
            }
            if v == "GenericArray" {
                return IrExpr::Var("Vec".to_string());
            }
            e.clone()
        }
        IrExpr::Path { segments, type_args } => {
            let mut segments = segments.clone();
            let mut type_args = type_args.clone();
            if segments.len() > 0 && segments[0] == "GenericArray" {
                segments[0] = "Vec".to_string();
            }
            if segments.len() == 2 && segments[0] == "Vec" && (segments[1] == "default" || segments[1] == "generate") {
                segments[1] = "new".to_string();
                type_args = Vec::new();
            }
            if segments.len() == 2 && segments[1] == "to_usize" {
                // Check if first segment is a length param (single uppercase letter, or uppercase+digit like K2)
                let first = &segments[0];
                let is_length_param = first.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) &&
                    first.chars().all(|c| c.is_uppercase() || c.is_ascii_digit());
                if is_length_param {
                    return IrExpr::Var(first.to_lowercase());
                }
            }
            IrExpr::Path {
                segments,
                type_args: type_args.iter().map(|ta| lower_type_dyn(ta, ctx, fn_gen)).collect(),
            }
        }
        IrExpr::Binary { op, left, right } => IrExpr::Binary {
            op: *op,
            left: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
            right: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
        },
        IrExpr::MethodCall { receiver, method, type_args, args } => {
            let method = method.clone();
            let mut args: Vec<IrExpr> = args.iter().map(|a| lower_expr_dyn(a, ctx, fn_gen)).collect();
            
            if let MethodKind::Vole(VoleMethod::Remap) = method {
                if args.len() == 1 {
                    args.insert(0, IrExpr::Var("n".to_string()));
                }
            }

            if let MethodKind::Std(name) = &method {
                if name == "to_usize" {
                    match receiver.as_ref() {
                        IrExpr::Var(v) if v.len() == 1 && v.chars().next().unwrap().is_uppercase() => {
                            return IrExpr::Var(v.to_lowercase());
                        }
                        IrExpr::Path { segments, .. } if segments.len() == 2 && segments[1] == "OutputSize" => {
                            return IrExpr::Macro { name: "typenum_usize".to_string(), tokens: format!("{}::{}", segments[0], segments[1]) };
                        }
                        _ => {}
                    }
                }
            }
            IrExpr::MethodCall {
                receiver: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
                method,
                type_args: type_args.iter().map(|ta| lower_type_dyn(ta, ctx, fn_gen)).collect(),
                args,
            }
        }
        IrExpr::Call { func, args } => {
            let args: Vec<IrExpr> = args.iter().map(|a| lower_expr_dyn(a, ctx, fn_gen)).collect();
            let mut func = lower_expr_dyn(func, ctx, fn_gen);

            // If the func is a lowercased length param (like n, k, k2), 
            // and there are no args, just return the variable
            if args.is_empty() {
                if let IrExpr::Var(name) = &func {
                    // Check if it looks like a lowercased length param
                    let is_length_var = name.chars().next().map(|c| c.is_lowercase()).unwrap_or(false) &&
                        name.chars().all(|c| c.is_lowercase() || c.is_ascii_digit());
                    if is_length_var {
                        return func;
                    }
                }
            }

            let mut new_func = None;
            if let IrExpr::Var(name) = &func {
                if name == "create_vole_from_material" || name == "create_vole_from_material_expanded" || name == "double" {
                    if let Some(b_param) = fn_gen.iter().find(|p| classify_generic(p, &[fn_gen]) == GenericKind::Crypto && p.name.starts_with('B')) {
                        new_func = Some(IrExpr::Path { segments: vec![name.clone()], type_args: vec![IrType::TypeParam(b_param.name.clone())] });
                    }
                } else if name == "commit" {
                    if let Some(d_param) = fn_gen.iter().find(|p| classify_generic(p, &[fn_gen]) == GenericKind::Crypto && p.name.starts_with('D')) {
                        new_func = Some(IrExpr::Path { segments: vec![name.clone()], type_args: vec![IrType::TypeParam(d_param.name.clone())] });
                    }
                }
            }
            if let Some(nf) = new_func {
                func = nf;
            }

            IrExpr::Call {
                func: Box::new(func),
                args,
            }
        }
        IrExpr::StructExpr { kind, type_args, fields, rest } => {
            let kind_str = kind.to_string();
            let new_kind = if kind_str == "Option" || kind_str == "Result" {
                kind.clone()
            } else if ctx.struct_info.contains_key(&kind_str) {
                StructKind::from_str(&format!("{}Dyn", kind))
            } else {
                kind.clone()
            };

            let mut lowered_fields: Vec<_> = fields.iter().map(|(n, v)| (n.clone(), lower_expr_dyn(v, ctx, fn_gen))).collect();
            
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
                            let default_val = info.orig_generics.iter()
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
                    lowered_fields.push(("_phantom".to_string(), IrExpr::Path {
                        segments: vec!["PhantomData".to_string()],
                        type_args: Vec::new(),
                    }));
                }
            }

            IrExpr::StructExpr {
                kind: new_kind,
                type_args: type_args.iter().map(|ta| lower_type_dyn(ta, ctx, fn_gen)).collect(),
                fields: lowered_fields,
                rest: rest.as_ref().map(|r| Box::new(lower_expr_dyn(r, ctx, fn_gen))),
            }
        }
        IrExpr::ArrayGenerate { elem_ty, len, index_var, body } => {
            let lowered_len = lower_array_length(len);
            IrExpr::ArrayGenerate {
                elem_ty: elem_ty.as_ref().map(|t| Box::new(lower_type_dyn(t, ctx, fn_gen))),
                len: lowered_len,
                index_var: index_var.clone(),
                body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
            }
        }
        IrExpr::Field { base, field } => {
            IrExpr::Field {
                base: Box::new(lower_expr_dyn(base, ctx, fn_gen)),
                field: field.clone(),
            }
        }
        IrExpr::Index { base, index } => {
            IrExpr::Index {
                base: Box::new(lower_expr_dyn(base, ctx, fn_gen)),
                index: Box::new(lower_expr_dyn(index, ctx, fn_gen)),
            }
        }
        IrExpr::Unary { op, expr } => {
            IrExpr::Unary {
                op: *op,
                expr: Box::new(lower_expr_dyn(expr, ctx, fn_gen)),
            }
        }
        IrExpr::Block(b) => {
            IrExpr::Block(lower_block_dyn(b, ctx, fn_gen))
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            IrExpr::If {
                cond: Box::new(lower_expr_dyn(cond, ctx, fn_gen)),
                then_branch: lower_block_dyn(then_branch, ctx, fn_gen),
                else_branch: else_branch.as_ref().map(|eb| Box::new(lower_expr_dyn(eb, ctx, fn_gen))),
            }
        }
        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            IrExpr::BoundedLoop {
                var: var.clone(),
                start: Box::new(lower_expr_dyn(start, ctx, fn_gen)),
                end: Box::new(lower_expr_dyn(end, ctx, fn_gen)),
                inclusive: *inclusive,
                body: lower_block_dyn(body, ctx, fn_gen),
            }
        }
        IrExpr::IterLoop { pattern, collection, body } => {
            IrExpr::IterLoop {
                pattern: lower_pattern_dyn(pattern, ctx),
                collection: Box::new(lower_expr_dyn(collection, ctx, fn_gen)),
                body: lower_block_dyn(body, ctx, fn_gen),
            }
        }
        IrExpr::ArrayMap { array, elem_var, body } => {
            IrExpr::ArrayMap {
                array: Box::new(lower_expr_dyn(array, ctx, fn_gen)),
                elem_var: elem_var.clone(),
                body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
            }
        }
        IrExpr::ArrayZip { left, right, left_var, right_var, body } => {
            IrExpr::ArrayZip {
                left: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
                right: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
                left_var: left_var.clone(),
                right_var: right_var.clone(),
                body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
            }
        }
        IrExpr::ArrayFold { array, init, acc_var, elem_var, body } => {
            IrExpr::ArrayFold {
                array: Box::new(lower_expr_dyn(array, ctx, fn_gen)),
                init: Box::new(lower_expr_dyn(init, ctx, fn_gen)),
                acc_var: acc_var.clone(),
                elem_var: elem_var.clone(),
                body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
            }
        }
        IrExpr::Match { expr, arms } => {
            IrExpr::Match {
                expr: Box::new(lower_expr_dyn(expr, ctx, fn_gen)),
                arms: arms.iter().map(|arm| IrMatchArm {
                    pattern: lower_pattern_dyn(&arm.pattern, ctx),
                    guard: arm.guard.as_ref().map(|g| lower_expr_dyn(g, ctx, fn_gen)),
                    body: lower_expr_dyn(&arm.body, ctx, fn_gen),
                }).collect(),
            }
        }
        IrExpr::Closure { params, ret_type, body } => {
            IrExpr::Closure {
                params: params.clone(),
                ret_type: ret_type.as_ref().map(|rt| Box::new(lower_type_dyn(rt, ctx, fn_gen))),
                body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
            }
        }
        IrExpr::Cast { expr, ty } => {
            IrExpr::Cast {
                expr: Box::new(lower_expr_dyn(expr, ctx, fn_gen)),
                ty: Box::new(lower_type_dyn(ty, ctx, fn_gen)),
            }
        }
        IrExpr::Return(e) => {
            IrExpr::Return(e.as_ref().map(|expr| Box::new(lower_expr_dyn(expr, ctx, fn_gen))))
        }
        IrExpr::Tuple(elems) => {
            IrExpr::Tuple(elems.iter().map(|e| lower_expr_dyn(e, ctx, fn_gen)).collect())
        }
        IrExpr::Array(elems) => {
            IrExpr::Array(elems.iter().map(|e| lower_expr_dyn(e, ctx, fn_gen)).collect())
        }
        IrExpr::Repeat { elem, len } => {
            IrExpr::Repeat {
                elem: Box::new(lower_expr_dyn(elem, ctx, fn_gen)),
                len: Box::new(lower_expr_dyn(len, ctx, fn_gen)),
            }
        }
        _ => e.clone(),
    }
}

fn lower_array_length(len: &ArrayLength) -> ArrayLength {
    match len {
        ArrayLength::TypeParam(p) => {
            // Check if this is a typenum constant
            if let Some(tn) = TypeNumConst::from_str(p) {
                ArrayLength::Const(tn.to_usize())
            } else if p.contains("::") {
                // This is a type projection like B::BlockSize
                // Keep as computed expression that uses typenum_usize macro
                ArrayLength::Computed(Box::new(IrExpr::Macro {
                    name: "typenum_usize".to_string(),
                    tokens: p.clone(),
                }))
            } else {
                // Otherwise lowercase it to a runtime variable
                ArrayLength::TypeParam(p.to_lowercase())
            }
        }
        ArrayLength::TypeNum(tn) => ArrayLength::Const(tn.to_usize()),
        ArrayLength::Computed(e) => len.clone(), // Keep as-is for now
        ArrayLength::Const(_) => len.clone(),
    }
}
