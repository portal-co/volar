//! Dynamic IR Lowering
//!
//! This module transforms the specialized volar-spec IR into a dynamic IR
//! where type-level lengths become runtime usize witnesses.

use crate::ir::*;
use core::mem::take;
#[cfg(feature = "std")]
use std::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use alloc::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
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

        // Pass 2: Manual clones
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

    // Direct length indicators
    for bound in &param.bounds {
        if is_length_bound(bound) {
            return GenericKind::Length;
        }
        if is_crypto_bound(bound) {
            return GenericKind::Crypto;
        }
    }

    // Recursive check via param name
    let mut visited = Vec::new();
    if is_length_name(&param.name, all_params, &mut visited) {
        return GenericKind::Length;
    }

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
        lowered.impls.push(lower_impl_dyn(im, &ctx));
    }

    for f in &module.functions {
        lowered.functions.push(lower_function_dyn(f, &ctx, None, None));
    }

    lowered
}

fn lower_struct_dyn(s: &IrStruct, ctx: &LoweringContext) -> IrStruct {
    let info = ctx.struct_info.get(&s.kind.to_string()).unwrap();
    let mut fields = Vec::new();

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
            ty: lower_type_dyn(&f.ty, ctx, &[]),
            public: f.public,
        });
    }

    IrStruct {
        kind: StructKind::from_str(&format!("{}Dyn", s.kind)),
        generics: lower_generics_dyn(&s.generics, &[]),
        fields,
        is_tuple: s.is_tuple,
    }
}

fn lower_impl_dyn(im: &IrImpl, ctx: &LoweringContext) -> IrImpl {
    let mut items = Vec::new();
    let self_kind = match &im.self_ty {
        IrType::Struct { kind, .. } => kind.to_string(),
        _ => String::new(),
    };

    for item in &im.items {
        match item {
            IrImplItem::Method(f) => {
                items.push(IrImplItem::Method(lower_function_dyn(f, ctx, Some(&self_kind), Some(&im.generics))));
            }
            IrImplItem::AssociatedType { name, ty } => {
                items.push(IrImplItem::AssociatedType {
                    name: name.clone(),
                    ty: lower_type_dyn(ty, ctx, &im.generics),
                });
            }
        }
    }

    IrImpl {
        generics: lower_generics_dyn(&im.generics, &[]),
        trait_: im.trait_.as_ref().map(|tr| IrTraitRef {
            kind: tr.kind.clone(), // TODO: lower trait kind if it contains types
            type_args: tr.type_args.iter().map(|ta| lower_type_dyn(ta, ctx, &im.generics)).collect(),
        }),
        self_ty: lower_type_dyn(&im.self_ty, ctx, &im.generics),
        where_clause: lower_where_clause_dyn(&im.where_clause, ctx, &im.generics),
        items,
    }
}

fn lower_function_dyn(
    f: &IrFunction,
    ctx: &LoweringContext,
    self_struct: Option<&str>,
    impl_generics: Option<&[IrGenericParam]>,
) -> IrFunction {
    let mut params = Vec::new();
    let mut generics_to_check = f.generics.clone();
    let empty_gen = Vec::new();
    let impl_gen = impl_generics.unwrap_or(&empty_gen);
    
    // Length generics become parameters
    for p in &f.generics {
        let kind = classify_generic(p, &[&f.generics, impl_gen]);
        if kind == GenericKind::Length {
            params.push(IrParam {
                name: p.name.to_lowercase(),
                ty: IrType::Primitive(PrimitiveType::Usize),
            });
        }
    }

    // Static methods also need impl-level length params
    if f.receiver.is_none() {
        for p in impl_gen {
            let kind = classify_generic(p, &[impl_gen]);
            if kind == GenericKind::Length {
                params.push(IrParam {
                    name: p.name.to_lowercase(),
                    ty: IrType::Primitive(PrimitiveType::Usize),
                });
            }
        }
    }

    for p in &f.params {
        params.push(IrParam {
            name: p.name.clone(),
            ty: lower_type_dyn(&p.ty, ctx, &f.generics),
        });
    }

    let mut body = lower_block_dyn(&f.body, ctx, &f.generics);

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
    }

    IrFunction {
        name: f.name.clone(),
        generics: lower_generics_dyn(&f.generics, impl_gen),
        receiver: f.receiver,
        params,
        return_type: f.return_type.as_ref().map(|rt| lower_type_dyn(rt, ctx, &f.generics)),
        where_clause: lower_where_clause_dyn(&f.where_clause, ctx, &f.generics),
        body,
    }
}

fn lower_generics_dyn(generics: &[IrGenericParam], extra_ctx: &[IrGenericParam]) -> Vec<IrGenericParam> {
    generics.iter()
        .filter(|p| p.kind != IrGenericParamKind::Lifetime && classify_generic(p, &[generics, extra_ctx]) != GenericKind::Length)
        .cloned()
        .collect()
}

fn lower_type_dyn(ty: &IrType, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrType {
    match ty {
        IrType::Array { kind, elem, len } => {
            if *kind == ArrayKind::Slice {
                IrType::Array {
                    kind: *kind,
                    elem: Box::new(lower_type_dyn(elem, ctx, fn_gen)),
                    len: len.clone(),
                }
            } else {
                IrType::Vector {
                    elem: Box::new(lower_type_dyn(elem, ctx, fn_gen)),
                }
            }
        }
        IrType::Struct { kind, type_args } => {
            let mut lowered_args = Vec::new();
            if let Some(info) = ctx.struct_info.get(&kind.to_string()) {
                let non_lifetime_generics: Vec<_> = info.orig_generics.iter()
                    .filter(|(_, _, pk)| *pk != IrGenericParamKind::Lifetime)
                    .collect();
                
                for (idx, arg) in type_args.iter().enumerate() {
                    if let Some((_, gkind, _)) = non_lifetime_generics.get(idx) {
                        if *gkind == GenericKind::Length { continue; }
                    }
                    lowered_args.push(lower_type_dyn(arg, ctx, fn_gen));
                }
            } else {
                lowered_args = type_args.iter().map(|ta| lower_type_dyn(ta, ctx, fn_gen)).collect();
            }
            IrType::Struct {
                kind: StructKind::from_str(&format!("{}Dyn", kind)),
                type_args: lowered_args,
            }
        }
        IrType::Reference { mutable, elem } => IrType::Reference {
            mutable: *mutable,
            elem: Box::new(lower_type_dyn(elem, ctx, fn_gen)),
        },
        IrType::Tuple(elems) => IrType::Tuple(elems.iter().map(|e| lower_type_dyn(e, ctx, fn_gen)).collect()),
        IrType::TypeParam(p) => {
            // If it's a length param, it effectively becomes usize in dynamic context
            // But usually we want to replace it with actual variable access in expressions.
            // In type positions (like field types), we've already handled this in lower_struct_dyn.
            IrType::TypeParam(p.clone())
        }
        _ => ty.clone(),
    }
}

fn lower_where_clause_dyn(wc: &[IrWherePredicate], ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> Vec<IrWherePredicate> {
    wc.iter().filter_map(|wp| {
        match wp {
            IrWherePredicate::TypeBound { ty, bounds } => {
                // TODO: filter out length-based bounds
                Some(IrWherePredicate::TypeBound {
                    ty: lower_type_dyn(ty, ctx, fn_gen),
                    bounds: bounds.clone(),
                })
            }
        }
    }).collect()
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
            pattern: pattern.clone(),
            ty: ty.as_ref().map(|t| lower_type_dyn(t, ctx, fn_gen)),
            init: init.as_ref().map(|i| lower_expr_dyn(i, ctx, fn_gen)),
        },
        IrStmt::Semi(e) => IrStmt::Semi(lower_expr_dyn(e, ctx, fn_gen)),
        IrStmt::Expr(e) => IrStmt::Expr(lower_expr_dyn(e, ctx, fn_gen)),
    }
}

fn lower_expr_dyn(e: &IrExpr, ctx: &LoweringContext, fn_gen: &[IrGenericParam]) -> IrExpr {
    match e {
        IrExpr::Var(v) => {
            // If v is a known length generic, it should be lowercase
            // This is a bit simplified, ideally we'd check against fn_gen/struct_info
            if v.len() == 1 && v.chars().next().unwrap().is_uppercase() {
                IrExpr::Var(v.to_lowercase())
            } else {
                e.clone()
            }
        }
        IrExpr::Binary { op, left, right } => IrExpr::Binary {
            op: *op,
            left: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
            right: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
        },
        IrExpr::MethodCall { receiver, method, type_args, args } => {
            // Special case for to_usize
            if let MethodKind::Std(name) = method {
                if name == "to_usize" {
                    if let IrExpr::Var(v) = receiver.as_ref() {
                        return IrExpr::Var(v.to_lowercase());
                    }
                }
            }
            IrExpr::MethodCall {
                receiver: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
                method: method.clone(),
                type_args: type_args.iter().map(|ta| lower_type_dyn(ta, ctx, fn_gen)).collect(),
                args: args.iter().map(|a| lower_expr_dyn(a, ctx, fn_gen)).collect(),
            }
        }
        IrExpr::Call { func, args } => {
            // Special cases for GenericArray::default etc
            IrExpr::Call {
                func: Box::new(lower_expr_dyn(func, ctx, fn_gen)),
                args: args.iter().map(|a| lower_expr_dyn(a, ctx, fn_gen)).collect(),
            }
        }
        IrExpr::StructExpr { kind, type_args, fields, rest } => {
            IrExpr::StructExpr {
                kind: StructKind::from_str(&format!("{}Dyn", kind)),
                type_args: type_args.iter().map(|ta| lower_type_dyn(ta, ctx, fn_gen)).collect(),
                fields: fields.iter().map(|(n, v)| (n.clone(), lower_expr_dyn(v, ctx, fn_gen))).collect(),
                rest: rest.as_ref().map(|r| Box::new(lower_expr_dyn(r, ctx, fn_gen))),
            }
        }
        _ => e.clone(),
    }
}
