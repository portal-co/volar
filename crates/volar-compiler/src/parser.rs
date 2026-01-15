//! Parser module for converting Rust source code directly into specialized IR.

use crate::ir::*;
#[cfg(feature = "std")]
use std::{
    boxed::Box,
    collections::{HashMap, HashSet},
    format,
    string::{String, ToString},
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec::Vec,
};
#[cfg(not(feature = "std"))]
use hashbrown::{HashMap, HashSet};

use syn::PathArguments;
#[cfg(feature = "parsing")]
use syn::{Expr, FnArg, GenericParam, Item, Pat, ReturnType, Type, Visibility, parse_file};

pub fn parse_sources(sources: &[(&str, &str)], module_name: &str) -> Result<IrModule> {
    let mut module = IrModule {
        name: module_name.to_string(),
        ..Default::default()
    };

    for (source, _sub_name) in sources {
        let file = parse_file(source).map_err(|e| CompilerError::ParseError(e.to_string()))?;
        for item in &file.items {
            match item {
                Item::Struct(s) => module.structs.push(convert_struct(s)?),
                Item::Trait(t) => module.traits.push(convert_trait(t)?),
                Item::Impl(i) => module.impls.push(convert_impl(i)?),
                Item::Fn(f) => module.functions.push(convert_function(f)?),
                Item::Type(t) => module.type_aliases.push(convert_type_alias(t)?),
                Item::Use(u) => {}
                _ => {}
            }
        }
    }

    Ok(module)
}

pub fn parse_source(source: &str, name: &str) -> Result<IrModule> {
    parse_sources(&[(source, name)], name)
}

fn convert_struct(s: &syn::ItemStruct) -> Result<IrStruct> {
    Ok(IrStruct {
        kind: StructKind::from_str(&s.ident.to_string()),
        generics: s
            .generics
            .params
            .iter()
            .map(convert_generic_param)
            .collect::<Result<Vec<_>>>()?,
        fields: match &s.fields {
            syn::Fields::Named(fields) => fields
                .named
                .iter()
                .map(convert_field)
                .collect::<Result<Vec<_>>>()?,
            syn::Fields::Unnamed(fields) => fields
                .unnamed
                .iter()
                .map(convert_field)
                .collect::<Result<Vec<_>>>()?,
            syn::Fields::Unit => Vec::new(),
        },
        is_tuple: matches!(s.fields, syn::Fields::Unnamed(_)),
    })
}

fn convert_field(f: &syn::Field) -> Result<IrField> {
    Ok(IrField {
        name: f.ident.as_ref().map(|i| i.to_string()).unwrap_or_default(),
        ty: convert_type(&f.ty)?,
        public: matches!(f.vis, Visibility::Public(_)),
    })
}

fn convert_trait(t: &syn::ItemTrait) -> Result<IrTrait> {
    Ok(IrTrait {
        kind: TraitKind::from_path(&[t.ident.to_string()]),
        generics: t
            .generics
            .params
            .iter()
            .map(convert_generic_param)
            .collect::<Result<Vec<_>>>()?,
        super_traits: t
            .supertraits
            .iter()
            .map(|bound| {
                if let syn::TypeParamBound::Trait(tb) = bound {
                    Ok(Some(convert_trait_bound(tb)?))
                } else {
                    Ok(None)
                }
            })
            .collect::<Result<Vec<Option<_>>>>()?
            .into_iter()
            .flatten()
            .collect(),
        items: t
            .items
            .iter()
            .map(convert_trait_item)
            .collect::<Result<Vec<Option<_>>>>()?
            .into_iter()
            .flatten()
            .collect(),
    })
}

fn convert_trait_item(item: &syn::TraitItem) -> Result<Option<IrTraitItem>> {
    match item {
        syn::TraitItem::Fn(m) => Ok(Some(IrTraitItem::Method(IrMethodSig {
            name: m.sig.ident.to_string(),
            generics: m
                .sig
                .generics
                .params
                .iter()
                .map(convert_generic_param)
                .collect::<Result<Vec<_>>>()?,
            receiver: m.sig.receiver().map(convert_receiver),
            params: m
                .sig
                .inputs
                .iter()
                .map(|arg| {
                    if let FnArg::Typed(pt) = arg {
                        Ok(Some(IrParam {
                            name: extract_pat_name(&pt.pat),
                            ty: convert_type(&pt.ty)?,
                        }))
                    } else {
                        Ok(None)
                    }
                })
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect(),
            return_type: match &m.sig.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(convert_type(ty)?),
            },
            where_clause: m
                .sig
                .generics
                .where_clause
                .as_ref()
                .map(|wc| {
                    wc.predicates
                        .iter()
                        .map(convert_where_predicate)
                        .collect::<Result<Vec<_>>>()
                })
                .transpose()?
                .unwrap_or_default(),
        }))),
        syn::TraitItem::Type(ty) => Ok(Some(IrTraitItem::AssociatedType {
            name: AssociatedType::from_str(&ty.ident.to_string()),
            bounds: ty
                .bounds
                .iter()
                .map(|bound| {
                    if let syn::TypeParamBound::Trait(tb) = bound {
                        Ok(Some(convert_trait_bound(tb)?))
                    } else {
                        Ok(None)
                    }
                })
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect(),
            default: ty
                .default
                .as_ref()
                .map(|(_, t)| convert_type(t))
                .transpose()?,
        })),
        _ => Ok(None),
    }
}

fn convert_impl(i: &syn::ItemImpl) -> Result<IrImpl> {
    Ok(IrImpl {
        generics: i
            .generics
            .params
            .iter()
            .map(convert_generic_param)
            .collect::<Result<Vec<_>>>()?,
        trait_: i
            .trait_
            .as_ref()
            .map(|(_, path, _)| {
                Ok::<_, CompilerError>(IrTraitRef {
                    kind: TraitKind::from_path(
                        &path
                            .segments
                            .iter()
                            .map(|s| s.ident.to_string())
                            .collect::<Vec<_>>(),
                    ),
                    type_args: path
                        .segments
                        .last()
                        .and_then(|s| {
                            if let syn::PathArguments::AngleBracketed(args) = &s.arguments {
                                Some(
                                    args.args
                                        .iter()
                                        .map(convert_generic_arg)
                                        .collect::<Result<Vec<Option<_>>>>(),
                                )
                            } else {
                                None
                            }
                        })
                        .transpose()?
                        .map(|v| v.into_iter().flatten().collect())
                        .unwrap_or_default(),
                })
            })
            .transpose()?,
        self_ty: convert_type(&i.self_ty)?,
        where_clause: i
            .generics
            .where_clause
            .as_ref()
            .map(|wc| {
                wc.predicates
                    .iter()
                    .map(convert_where_predicate)
                    .collect::<Result<Vec<_>>>()
            })
            .transpose()?
            .unwrap_or_default(),
        items: i
            .items
            .iter()
            .map(convert_impl_item)
            .collect::<Result<Vec<Option<_>>>>()?
            .into_iter()
            .flatten()
            .collect(),
    })
}

fn convert_impl_item(item: &syn::ImplItem) -> Result<Option<IrImplItem>> {
    match item {
        syn::ImplItem::Fn(m) => Ok(Some(IrImplItem::Method(IrFunction {
            name: m.sig.ident.to_string(),
            generics: m
                .sig
                .generics
                .params
                .iter()
                .map(convert_generic_param)
                .collect::<Result<Vec<_>>>()?,
            receiver: m.sig.receiver().map(convert_receiver),
            params: m
                .sig
                .inputs
                .iter()
                .map(|arg| {
                    if let FnArg::Typed(pt) = arg {
                        Ok(Some(IrParam {
                            name: extract_pat_name(&pt.pat),
                            ty: convert_type(&pt.ty)?,
                        }))
                    } else {
                        Ok(None)
                    }
                })
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect(),
            return_type: match &m.sig.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(convert_type(ty)?),
            },
            where_clause: m
                .sig
                .generics
                .where_clause
                .as_ref()
                .map(|wc| {
                    wc.predicates
                        .iter()
                        .map(convert_where_predicate)
                        .collect::<Result<Vec<_>>>()
                })
                .transpose()?
                .unwrap_or_default(),
            body: convert_block(&m.block)?,
        }))),
        syn::ImplItem::Type(ty) => Ok(Some(IrImplItem::AssociatedType {
            name: AssociatedType::from_str(&ty.ident.to_string()),
            ty: convert_type(&ty.ty)?,
        })),
        _ => Ok(None),
    }
}

fn convert_function(f: &syn::ItemFn) -> Result<IrFunction> {
    Ok(IrFunction {
        name: f.sig.ident.to_string(),
        generics: f
            .sig
            .generics
            .params
            .iter()
            .map(convert_generic_param)
            .collect::<Result<Vec<_>>>()?,
        receiver: None,
        params: f
            .sig
            .inputs
            .iter()
            .map(|arg| {
                if let FnArg::Typed(pt) = arg {
                    Ok(Some(IrParam {
                        name: extract_pat_name(&pt.pat),
                        ty: convert_type(&pt.ty)?,
                    }))
                } else {
                    Ok(None)
                }
            })
            .collect::<Result<Vec<Option<_>>>>()?
            .into_iter()
            .flatten()
            .collect(),
        return_type: match &f.sig.output {
            ReturnType::Default => None,
            ReturnType::Type(_, ty) => Some(convert_type(ty)?),
        },
        where_clause: f
            .sig
            .generics
            .where_clause
            .as_ref()
            .map(|wc| {
                wc.predicates
                    .iter()
                    .map(convert_where_predicate)
                    .collect::<Result<Vec<_>>>()
            })
            .transpose()?
            .unwrap_or_default(),
        body: convert_block(&f.block)?,
    })
}

fn convert_type_alias(t: &syn::ItemType) -> Result<IrTypeAlias> {
    Ok(IrTypeAlias {
        name: t.ident.to_string(),
        generics: t
            .generics
            .params
            .iter()
            .map(convert_generic_param)
            .collect::<Result<Vec<_>>>()?,
        target: convert_type(&t.ty)?,
    })
}

fn convert_generic_param(p: &GenericParam) -> Result<IrGenericParam> {
    match p {
        GenericParam::Type(tp) => Ok(IrGenericParam {
            name: tp.ident.to_string(),
            kind: IrGenericParamKind::Type,
            bounds: tp
                .bounds
                .iter()
                .map(|bound| {
                    if let syn::TypeParamBound::Trait(tb) = bound {
                        Ok(Some(convert_trait_bound(tb)?))
                    } else {
                        Ok(None)
                    }
                })
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect(),
            default: tp.default.as_ref().map(convert_type).transpose()?,
        }),
        GenericParam::Const(cp) => Ok(IrGenericParam {
            name: cp.ident.to_string(),
            kind: IrGenericParamKind::Const,
            bounds: Vec::new(),
            default: None, // TODO: handle const param default
        }),
        GenericParam::Lifetime(lp) => Ok(IrGenericParam {
            name: lp.lifetime.ident.to_string(),
            kind: IrGenericParamKind::Lifetime,
            bounds: Vec::new(),
            default: None,
        }),
    }
}

fn convert_trait_bound(b: &syn::TraitBound) -> Result<IrTraitBound> {
    let last_segment = b
        .path
        .segments
        .last()
        .ok_or_else(|| CompilerError::ParseError("Empty trait path".to_string()))?;

    let path_names: Vec<String> = b
        .path
        .segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect();

    let trait_kind = match path_names.last().map(|s| s.as_str()) {
        Some("Into") => {
            if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
                if let Some(syn::GenericArgument::Type(ty)) = args.args.first() {
                    TraitKind::Into(Box::new(convert_type(ty)?))
                } else {
                    TraitKind::from_path(&path_names)
                }
            } else {
                TraitKind::from_path(&path_names)
            }
        }
        Some("AsRef") => {
            if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
                if let Some(syn::GenericArgument::Type(ty)) = args.args.first() {
                    TraitKind::AsRef(Box::new(convert_type(ty)?))
                } else {
                    TraitKind::from_path(&path_names)
                }
            } else {
                TraitKind::from_path(&path_names)
            }
        }
        Some("FnMut") | Some("Fn") => {
            if let syn::PathArguments::Parenthesized(args) = &last_segment.arguments {
                // We support a single input which must be either `&[u8]` or `usize`.
                let input_kind = if args.inputs.len() == 1 {
                    match &args.inputs.first().unwrap() {
                        syn::Type::Reference(r) => {
                            // check for &[u8]
                            if let syn::Type::Slice(s) = &*r.elem {
                                if let syn::Type::Path(p) = &*s.elem {
                                    if let Some(ident) = p.path.segments.last() {
                                        if ident.ident == "u8" {
                                            Some(crate::ir::FnInput::BytesSlice)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        syn::Type::Path(p) => {
                            if let Some(ident) = p.path.segments.last() {
                                if ident.ident == "usize" {
                                    Some(crate::ir::FnInput::Size)
                                } else if ident.ident == "bool" {
                                    Some(crate::ir::FnInput::Bool)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                match &args.output {
                    syn::ReturnType::Type(_, ty) => {
                        if let Some(inp) = input_kind {
                            TraitKind::Fn(inp, Box::new(convert_type(ty)?))
                        } else {
                            TraitKind::from_path(&path_names)
                        }
                    }
                    _ => TraitKind::from_path(&path_names),
                }
            } else {
                TraitKind::from_path(&path_names)
            }
        }
        _ => TraitKind::from_path(&path_names),
    };

    let mut type_args = Vec::new();
    let mut assoc_bindings = Vec::new();

    if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
        for arg in &args.args {
            match arg {
                syn::GenericArgument::Type(ty) => {
                    type_args.push(convert_type(ty)?);
                }
                syn::GenericArgument::AssocType(assoc) => {
                    assoc_bindings.push((
                        AssociatedType::from_str(&assoc.ident.to_string()),
                        convert_type(&assoc.ty)?,
                    ));
                }
                _ => {}
            }
        }
    }

    Ok(IrTraitBound {
        trait_kind,
        type_args,
        assoc_bindings,
    })
}

fn convert_where_predicate(p: &syn::WherePredicate) -> Result<IrWherePredicate> {
    match p {
        syn::WherePredicate::Type(pt) => Ok(IrWherePredicate::TypeBound {
            ty: convert_type(&pt.bounded_ty)?,
            bounds: pt
                .bounds
                .iter()
                .map(|bound| {
                    if let syn::TypeParamBound::Trait(tb) = bound {
                        Ok(Some(convert_trait_bound(tb)?))
                    } else {
                        Ok(None)
                    }
                })
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect(),
        }),
        _ => Err(CompilerError::Unsupported(format!(
            "Where predicate: {:?}",
            p
        ))),
    }
}

fn convert_receiver(r: &syn::Receiver) -> IrReceiver {
    if r.reference.is_some() {
        if r.mutability.is_some() {
            IrReceiver::RefMut
        } else {
            IrReceiver::Ref
        }
    } else {
        IrReceiver::Value
    }
}

fn convert_generic_arg(arg: &syn::GenericArgument) -> Result<Option<IrType>> {
    match arg {
        syn::GenericArgument::Type(ty) => Ok(Some(convert_type(ty)?)),
        syn::GenericArgument::Const(c) => {
            if let syn::Expr::Lit(l) = c {
                if let syn::Lit::Int(i) = &l.lit {
                    return Ok(Some(IrType::TypeParam(i.base10_digits().to_string())));
                }
            }
            Ok(None)
        }
        _ => Ok(None),
    }
}

fn convert_type(ty: &Type) -> Result<IrType> {
    match ty {
        Type::Path(p) => {
            // Check for qualified path like <T as Trait>::Output
            // qself is Some when we have <...> before the path
            if let Some(qself) = &p.qself {
                // Convert the base type (the T in <T as Trait>)
                let base = convert_type(&qself.ty)?;

                // The trait arguments are in the segment at qself.position
                let mut trait_args = Vec::new();
                if let Some(segment) = p.path.segments.get(qself.position) {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        for arg in &args.args {
                            if let Some(ty) = convert_generic_arg(arg)? {
                                trait_args.push(ty);
                            }
                        }
                    }
                }

                // The associated type name is the last segment
                if let Some(last) = p.path.segments.last() {
                    let assoc_name = last.ident.to_string();
                    let assoc = AssociatedType::from_str(&assoc_name);
                    return Ok(IrType::Projection {
                        base: Box::new(base),
                        trait_args,
                        assoc,
                    });
                }
            }

            // Check for associated type path like T::Output
            // These have multiple segments where the first is a type param
            if p.path.segments.len() == 2 && p.qself.is_none() {
                let first = &p.path.segments[0];
                let second = &p.path.segments[1];
                let first_name = first.ident.to_string();
                let second_name = second.ident.to_string();

                // Check if first segment looks like a type parameter (single uppercase letter or PascalCase)
                // and second segment is an associated type name
                if first.arguments.is_empty()
                    && first_name
                        .chars()
                        .next()
                        .map(|c| c.is_uppercase())
                        .unwrap_or(false)
                {
                    let assoc = AssociatedType::from_str(&second_name);
                    return Ok(IrType::Projection {
                        base: Box::new(IrType::TypeParam(first_name)),
                        trait_args: Vec::new(),
                        assoc,
                    });
                }
            }

            let last = p
                .path
                .segments
                .last()
                .ok_or_else(|| CompilerError::InvalidType("Empty path".to_string()))?;
            let name = last.ident.to_string();
            if name == "Unsigned" {
                return Ok(IrType::Param {
                    path: p
                        .path
                        .segments
                        .iter()
                        .map(|s| s.ident.to_string())
                        .filter(|x| x != "Unsigned")
                        .collect(),
                });
            }

            if let Some(prim) = PrimitiveType::from_str(&name) {
                return Ok(IrType::Primitive(prim));
            }

            if let Some(tn) = TypeNumConst::from_str(&name) {
                return Ok(IrType::TypeParam(name));
            }

            let type_args = if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                args.args
                    .iter()
                    .map(convert_generic_arg)
                    .collect::<Result<Vec<Option<_>>>>()?
                    .into_iter()
                    .flatten()
                    .collect()
            } else {
                Vec::new()
            };

            if name == "GenericArray" && type_args.len() >= 2 {
                return Ok(IrType::Array {
                    kind: ArrayKind::GenericArray,
                    elem: Box::new(type_args[0].clone()),
                    len: convert_array_length_from_type(&type_args[1])?,
                });
            }

            if p.path.segments.len() == 1
                && type_args.is_empty()
                && name
                    .chars()
                    .next()
                    .map(|c| c.is_uppercase())
                    .unwrap_or(false)
            {
                // Potential type param
                return Ok(IrType::TypeParam(name));
            }

            Ok(IrType::Struct {
                kind: StructKind::from_str(&name),
                type_args,
            })
        }
        Type::Reference(r) => Ok(IrType::Reference {
            mutable: r.mutability.is_some(),
            elem: Box::new(convert_type(&r.elem)?),
        }),
        Type::Slice(s) => Ok(IrType::Array {
            kind: ArrayKind::Slice,
            elem: Box::new(convert_type(&s.elem)?),
            len: ArrayLength::Const(0), // Placeholder
        }),
        Type::Array(a) => Ok(IrType::Array {
            kind: ArrayKind::FixedArray,
            elem: Box::new(convert_type(&a.elem)?),
            len: convert_array_length_from_syn_expr(&a.len)?,
        }),
        Type::Tuple(t) => {
            if t.elems.is_empty() {
                Ok(IrType::Unit)
            } else {
                Ok(IrType::Tuple(
                    t.elems
                        .iter()
                        .map(convert_type)
                        .collect::<Result<Vec<_>>>()?,
                ))
            }
        }
        Type::ImplTrait(it) => Ok(IrType::Existential {
            bounds: it
                .bounds
                .iter()
                .map(|bound| {
                    if let syn::TypeParamBound::Trait(tb) = bound {
                        Ok(Some(convert_trait_bound(tb)?))
                    } else {
                        Ok(None)
                    }
                })
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect(),
        }),
        Type::Infer(_) => Ok(IrType::Infer),
        _ => Err(CompilerError::Unsupported(format!("Type: {:?}", ty))),
    }
}

fn convert_array_length_from_type(ty: &IrType) -> Result<ArrayLength> {
    match ty {
        IrType::Primitive(_) => Ok(ArrayLength::TypeNum(TypeNumConst::U8)), // Simplified
        IrType::TypeParam(name) => Ok(ArrayLength::TypeParam(name.clone())),
        IrType::Struct { kind, .. } => Ok(ArrayLength::TypeParam(kind.to_string())), // Common for GenericArray<T, BlockSize>
        IrType::Projection { base, assoc, .. } => {
            // Handle projections like Self::BlockSize, T::Output, <T as Trait>::Output
            // Convert to a type param string representation
            let base_str = match base.as_ref() {
                IrType::TypeParam(name) => name.clone(),
                _ => "Self".to_string(),
            };
            let assoc_str = match assoc {
                AssociatedType::Output => "Output",
                AssociatedType::Key => "Key",
                AssociatedType::BlockSize => "BlockSize",
                AssociatedType::OutputSize => "OutputSize",
                AssociatedType::TotalLoopCount => "TotalLoopCount",
                AssociatedType::Other(name) => name,
            };
            Ok(ArrayLength::TypeParam(format!(
                "{}::{}",
                base_str, assoc_str
            )))
        }
        _ => Err(CompilerError::InvalidType(format!(
            "Invalid array length type: {:?}",
            ty
        ))),
    }
}

fn convert_array_length_from_syn_expr(expr: &syn::Expr) -> Result<ArrayLength> {
    match expr {
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(n),
            ..
        }) => Ok(ArrayLength::Const(n.base10_parse().unwrap_or(0))),
        _ => Ok(ArrayLength::Computed(Box::new(convert_expr(expr)?))),
    }
}

fn convert_expr(expr: &Expr) -> Result<IrExpr> {
    match expr {
        Expr::Lit(l) => Ok(IrExpr::Lit(convert_lit(&l.lit)?)),
        Expr::Path(p) => {
            // Handle qualified paths like `<D::OutputSize as Unsigned>::to_usize()`
            if p.qself.is_some() {
                // Try to convert the qself base type into a readable segment like "D::OutputSize"
                let q = p.qself.as_ref().unwrap();
                let base_ir = convert_type(&q.ty)?;
                let base_str = match base_ir {
                    IrType::TypeParam(name) => name,
                    IrType::Projection { base, assoc, .. } => {
                        let base_name = match *base {
                            IrType::TypeParam(n) => n,
                            IrType::Struct { kind, .. } => kind.to_string(),
                            IrType::Param { path } => path.join("::"),
                            _ => "Self".to_string(),
                        };
                        let assoc_name = match assoc {
                            AssociatedType::Output => "Output".to_string(),
                            AssociatedType::Key => "Key".to_string(),
                            AssociatedType::BlockSize => "BlockSize".to_string(),
                            AssociatedType::OutputSize => "OutputSize".to_string(),
                            AssociatedType::TotalLoopCount => "TotalLoopCount".to_string(),
                            AssociatedType::Other(s) => s.clone(),
                        };
                        format!("{}::{}", base_name, assoc_name)
                    }
                    IrType::Struct { kind, .. } => kind.to_string(),
                    IrType::Param { path } => path.join("::"),
                    _ => "Self".to_string(),
                };
                // The remaining path segments (after the qualified `<...>::`) are in p.path.segments
                let last =
                    p.path.segments.last().ok_or_else(|| {
                        CompilerError::ParseError("Empty qualified path".to_string())
                    })?;
                let mut segments: Vec<String> = Vec::new();
                segments.push(base_str);
                segments.push(last.ident.to_string());
                return Ok(IrExpr::Path {
                    segments,
                    type_args: p
                        .path
                        .segments
                        .iter()
                        .flat_map(|s| {
                            if let PathArguments::AngleBracketed(args) = &s.arguments {
                                args.args
                                    .iter()
                                    .filter_map(|arg| convert_generic_arg(arg).ok().flatten())
                                    .collect::<Vec<_>>()
                            } else {
                                Vec::new()
                            }
                        })
                        .collect(),
                });
            }

            let segments: Vec<String> = p
                .path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect();
            if segments.len() == 1 {
                Ok(IrExpr::Var(segments[0].clone()))
            } else {
                Ok(IrExpr::Path {
                    segments,
                    type_args: p
                        .path
                        .segments
                        .iter()
                        .flat_map(|s| {
                            if let PathArguments::AngleBracketed(args) = &s.arguments {
                                args.args
                                    .iter()
                                    .filter_map(|arg| convert_generic_arg(arg).ok().flatten())
                                    .collect::<Vec<_>>()
                            } else {
                                Vec::new()
                            }
                        })
                        .collect(),
                })
            }
        }
        Expr::Binary(b) => Ok(IrExpr::Binary {
            op: convert_bin_op(b.op),
            left: Box::new(convert_expr(&b.left)?),
            right: Box::new(convert_expr(&b.right)?),
        }),
        Expr::Unary(u) => Ok(IrExpr::Unary {
            op: convert_unary_op(u.op),
            expr: Box::new(convert_expr(&u.expr)?),
        }),
        Expr::Call(c) => convert_call(&c.func, &c.args.iter().collect::<Vec<_>>()),
        Expr::MethodCall(m) => convert_method_call(
            &m.receiver,
            &m.method.to_string(),
            &m.args.iter().collect::<Vec<_>>(),
        ),
        Expr::Field(f) => Ok(IrExpr::Field {
            base: Box::new(convert_expr(&f.base)?),
            field: match &f.member {
                syn::Member::Named(i) => i.to_string(),
                syn::Member::Unnamed(i) => i.index.to_string(),
            },
        }),
        Expr::Index(i) => Ok(IrExpr::Index {
            base: Box::new(convert_expr(&i.expr)?),
            index: Box::new(convert_expr(&i.index)?),
        }),
        Expr::Range(r) => Ok(IrExpr::Binary {
            op: if matches!(r.limits, syn::RangeLimits::Closed(_)) {
                SpecBinOp::RangeInclusive
            } else {
                SpecBinOp::Range
            },
            left: Box::new(
                r.start
                    .as_ref()
                    .map(|e| convert_expr(e))
                    .transpose()?
                    .unwrap_or(IrExpr::Lit(IrLit::Int(0))),
            ),
            right: Box::new(
                r.end
                    .as_ref()
                    .map(|e| convert_expr(e))
                    .transpose()?
                    .unwrap_or(IrExpr::Lit(IrLit::Int(0))),
            ),
        }),
        Expr::Paren(p) => convert_expr(&p.expr),
        Expr::Block(b) => Ok(IrExpr::Block(convert_block(&b.block)?)),
        Expr::If(i) => Ok(IrExpr::If {
            cond: Box::new(convert_expr(&i.cond)?),
            then_branch: convert_block(&i.then_branch)?,
            else_branch: i
                .else_branch
                .as_ref()
                .map(|(_, e)| Ok::<_, CompilerError>(Box::new(convert_expr(e)?)))
                .transpose()?,
        }),
        Expr::ForLoop(f) => convert_for_loop(&f.pat, &f.expr, &f.body),
        Expr::While(_) | Expr::Loop(_) => Err(CompilerError::UnboundedLoop),
        Expr::Match(m) => Ok(IrExpr::Match {
            expr: Box::new(convert_expr(&m.expr)?),
            arms: m
                .arms
                .iter()
                .map(convert_match_arm)
                .collect::<Result<Vec<_>>>()?,
        }),
        Expr::Closure(c) => Ok(IrExpr::Closure {
            params: c
                .inputs
                .iter()
                .map(|p| {
                    Ok(IrClosureParam {
                        pattern: convert_pattern(p),
                        ty: None,
                    })
                })
                .collect::<Result<Vec<_>>>()?,
            ret_type: None,
            body: Box::new(convert_expr(&c.body)?),
        }),
        Expr::Reference(r) => Ok(IrExpr::Unary {
            op: if r.mutability.is_some() {
                SpecUnaryOp::RefMut
            } else {
                SpecUnaryOp::Ref
            },
            expr: Box::new(convert_expr(&r.expr)?),
        }),
        Expr::Cast(c) => Ok(IrExpr::Cast {
            expr: Box::new(convert_expr(&c.expr)?),
            ty: Box::new(convert_type(&c.ty)?),
        }),
        Expr::Return(r) => Ok(IrExpr::Return(
            r.expr
                .as_ref()
                .map(|e| Ok::<_, CompilerError>(Box::new(convert_expr(e)?)))
                .transpose()?,
        )),
        Expr::Break(b) => Ok(IrExpr::Break(
            b.expr
                .as_ref()
                .map(|e| Ok::<_, CompilerError>(Box::new(convert_expr(e)?)))
                .transpose()?,
        )),
        Expr::Continue(_) => Ok(IrExpr::Continue),
        Expr::Assign(a) => Ok(IrExpr::Assign {
            left: Box::new(convert_expr(&a.left)?),
            right: Box::new(convert_expr(&a.right)?),
        }),
        Expr::Struct(s) => {
            let last = s.path.segments.last().unwrap();
            let type_args = if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                args.args
                    .iter()
                    .filter_map(|arg| convert_generic_arg(arg).ok().flatten())
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            Ok(IrExpr::StructExpr {
                kind: StructKind::from_str(&last.ident.to_string()),
                type_args,
                fields: s
                    .fields
                    .iter()
                    .map(|f| {
                        Ok((
                            match &f.member {
                                syn::Member::Named(i) => i.to_string(),
                                syn::Member::Unnamed(i) => i.index.to_string(),
                            },
                            convert_expr(&f.expr)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
                rest: s
                    .rest
                    .as_ref()
                    .map(|e| Ok::<_, CompilerError>(Box::new(convert_expr(e)?)))
                    .transpose()?,
            })
        }
        Expr::Repeat(r) => Ok(IrExpr::Repeat {
            elem: Box::new(convert_expr(&r.expr)?),
            len: Box::new(convert_expr(&r.len)?),
        }),
        Expr::Array(a) => Ok(IrExpr::Array(
            a.elems
                .iter()
                .map(convert_expr)
                .collect::<Result<Vec<_>>>()?,
        )),
        Expr::Tuple(t) => Ok(IrExpr::Tuple(
            t.elems
                .iter()
                .map(convert_expr)
                .collect::<Result<Vec<_>>>()?,
        )),
        Expr::Macro(m) => Ok(IrExpr::Macro {
            name: m.mac.path.segments.last().unwrap().ident.to_string(),
            tokens: m.mac.tokens.to_string(),
        }),
        _ => Err(CompilerError::Unsupported(format!("{:?}", expr))),
    }
}

fn convert_call(func: &Expr, args: &[&Expr]) -> Result<IrExpr> {
    if let Expr::Path(p) = func {
        let path_str = p
            .path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::");
        let params = p
            .path
            .segments
            .iter()
            .flat_map(|s| {
                if let syn::PathArguments::AngleBracketed(args) = &s.arguments {
                    Some(
                        args.args
                            .iter()
                            .map(convert_generic_arg)
                            .collect::<Result<Vec<Option<_>>>>(),
                    )
                } else {
                    None
                }
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .flatten()
            .collect::<Vec<_>>();
        if (path_str == "GenericArray::generate" || path_str.ends_with("::generate"))
            && args.len() == 1
        {
            if let Expr::Closure(c) = args[0] {
                // Type parameters may be inferred or come from complex paths
                // Try to extract what we can
                let elem_ty = params.get(0).cloned().map(Box::new);
                let len = if params.len() >= 2 {
                    match &params[1] {
                        IrType::TypeParam(name) => ArrayLength::TypeParam(name.clone()),
                        IrType::Projection { base, assoc, .. } => {
                            let base_str = match base.as_ref() {
                                IrType::TypeParam(name) => name.clone(),
                                _ => "Self".to_string(),
                            };
                            let assoc_str = match assoc {
                                AssociatedType::Output => "Output",
                                AssociatedType::BlockSize => "BlockSize",
                                AssociatedType::OutputSize => "OutputSize",
                                AssociatedType::Other(name) => name,
                                _ => "Output",
                            };
                            ArrayLength::TypeParam(format!("{}::{}", base_str, assoc_str))
                        }
                        _ => ArrayLength::TypeParam("N".to_string()),
                    }
                } else {
                    ArrayLength::TypeParam("N".to_string()) // Placeholder for inferred
                };
                return Ok(IrExpr::ArrayGenerate {
                    elem_ty,
                    len,
                    index_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }
        if (path_str == "core::array::from_fn" || path_str.ends_with("::from_fn"))
            && args.len() == 1
        {
            if let Expr::Closure(c) = args[0] {
                // Type parameters may be inferred or come from complex paths
                let elem_ty = params.get(0).cloned().map(Box::new);
                let len = if params.len() >= 2 {
                    match &params[1] {
                        IrType::TypeParam(name) => ArrayLength::TypeParam(name.clone()),
                        IrType::Projection { base, assoc, .. } => {
                            let base_str = match base.as_ref() {
                                IrType::TypeParam(name) => name.clone(),
                                _ => "Self".to_string(),
                            };
                            let assoc_str = match assoc {
                                AssociatedType::Output => "Output",
                                AssociatedType::BlockSize => "BlockSize",
                                AssociatedType::OutputSize => "OutputSize",
                                AssociatedType::Other(name) => name,
                                _ => "Output",
                            };
                            ArrayLength::TypeParam(format!("{}::{}", base_str, assoc_str))
                        }
                        _ => ArrayLength::TypeParam("N".to_string()),
                    }
                } else {
                    ArrayLength::TypeParam("N".to_string()) // Placeholder for inferred
                };
                return Ok(IrExpr::ArrayGenerate {
                    elem_ty,
                    len,
                    index_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }
    }
    Ok(IrExpr::Call {
        func: Box::new(convert_expr(func)?),
        args: args
            .iter()
            .map(|a| convert_expr(a))
            .collect::<Result<Vec<_>>>()?,
    })
}

fn convert_method_call(receiver: &Expr, method: &str, args: &[&Expr]) -> Result<IrExpr> {
    match method {
        "map" if args.len() == 1 => {
            if let Expr::Closure(c) = args[0] {
                return Ok(IrExpr::ArrayMap {
                    array: Box::new(convert_expr(receiver)?),
                    elem_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }
        "zip" if args.len() == 2 => {
            if let Expr::Closure(c) = args[1] {
                return Ok(IrExpr::ArrayZip {
                    left: Box::new(convert_expr(receiver)?),
                    right: Box::new(convert_expr(args[0])?),
                    left_var: extract_pat_name(&c.inputs[0]),
                    right_var: extract_pat_name(&c.inputs[1]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }
        _ => {}
    }
    Ok(IrExpr::MethodCall {
        receiver: Box::new(convert_expr(receiver)?),
        method: MethodKind::from_str(method),
        type_args: Vec::new(),
        args: args
            .iter()
            .map(|a| convert_expr(a))
            .collect::<Result<Vec<_>>>()?,
    })
}

fn convert_for_loop(pat: &Pat, iter: &Expr, body: &syn::Block) -> Result<IrExpr> {
    if let Expr::Range(r) = iter {
        return Ok(IrExpr::BoundedLoop {
            var: extract_pat_name(pat),
            start: Box::new(
                r.start
                    .as_ref()
                    .map(|e| convert_expr(e))
                    .transpose()?
                    .unwrap_or(IrExpr::Lit(IrLit::Int(0))),
            ),
            end: Box::new(
                r.end
                    .as_ref()
                    .map(|e| convert_expr(e))
                    .transpose()?
                    .unwrap_or(IrExpr::Lit(IrLit::Int(0))),
            ),
            inclusive: matches!(r.limits, syn::RangeLimits::Closed(_)),
            body: convert_block(body)?,
        });
    }
    Ok(IrExpr::IterLoop {
        pattern: convert_pattern(pat),
        collection: Box::new(convert_expr(iter)?),
        body: convert_block(body)?,
    })
}

fn convert_block(block: &syn::Block) -> Result<IrBlock> {
    let mut stmts = Vec::new();
    let mut expr = None;
    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last = i == block.stmts.len() - 1;
        match stmt {
            syn::Stmt::Local(l) => {
                // Extract type annotation from type-annotated patterns like `let x: T = ...`
                let ty = if let Pat::Type(pt) = &l.pat {
                    convert_type(&pt.ty).ok()
                } else {
                    None
                };
                stmts.push(IrStmt::Let {
                    pattern: convert_pattern(&l.pat),
                    ty,
                    init: l
                        .init
                        .as_ref()
                        .map(|init| convert_expr(&init.expr))
                        .transpose()?,
                });
            }
            syn::Stmt::Expr(e, semi) => {
                if is_last && semi.is_none() {
                    expr = Some(Box::new(convert_expr(e)?));
                } else {
                    stmts.push(IrStmt::Semi(convert_expr(e)?));
                }
            }
            _ => {}
        }
    }
    Ok(IrBlock { stmts, expr })
}

fn convert_match_arm(arm: &syn::Arm) -> Result<IrMatchArm> {
    Ok(IrMatchArm {
        pattern: convert_pattern(&arm.pat),
        guard: arm
            .guard
            .as_ref()
            .map(|(_, e)| convert_expr(e))
            .transpose()?,
        body: convert_expr(&arm.body)?,
    })
}

fn convert_pattern(pat: &Pat) -> IrPattern {
    match pat {
        Pat::Ident(pi) => IrPattern::Ident {
            mutable: pi.mutability.is_some(),
            name: pi.ident.to_string(),
            subpat: pi
                .subpat
                .as_ref()
                .map(|(_, p)| Box::new(convert_pattern(p))),
        },
        Pat::Wild(_) => IrPattern::Wild,
        Pat::Tuple(pt) => IrPattern::Tuple(pt.elems.iter().map(convert_pattern).collect()),
        Pat::TupleStruct(pts) => {
            let path_str = path_to_string(&pts.path);
            let kind = StructKind::from_str(&path_str);
            IrPattern::TupleStruct {
                kind,
                elems: pts.elems.iter().map(convert_pattern).collect(),
            }
        }
        Pat::Struct(ps) => {
            let path_str = path_to_string(&ps.path);
            let kind = StructKind::from_str(&path_str);
            let fields = ps
                .fields
                .iter()
                .map(|f| {
                    let name = match &f.member {
                        syn::Member::Named(id) => id.to_string(),
                        syn::Member::Unnamed(idx) => idx.index.to_string(),
                    };
                    (name, convert_pattern(&f.pat))
                })
                .collect();
            IrPattern::Struct {
                kind,
                fields,
                rest: ps.rest.is_some(),
            }
        }
        Pat::Slice(ps) => IrPattern::Slice(ps.elems.iter().map(convert_pattern).collect()),
        Pat::Lit(pl) => {
            if let Ok(lit) = convert_lit(&pl.lit) {
                IrPattern::Lit(lit)
            } else {
                IrPattern::Wild
            }
        }
        Pat::Reference(pr) => IrPattern::Ref {
            mutable: pr.mutability.is_some(),
            pat: Box::new(convert_pattern(&pr.pat)),
        },
        Pat::Or(po) => IrPattern::Or(po.cases.iter().map(convert_pattern).collect()),
        Pat::Rest(_) => IrPattern::Rest,
        // Handle type-annotated patterns like `let mut x: T = ...`
        Pat::Type(pt) => convert_pattern(&pt.pat),
        _ => IrPattern::Wild,
    }
}

fn path_to_string(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

fn extract_pat_name(pat: &Pat) -> String {
    if let Pat::Ident(pi) = pat {
        pi.ident.to_string()
    } else {
        "_".to_string()
    }
}

fn convert_lit(lit: &syn::Lit) -> Result<IrLit> {
    match lit {
        syn::Lit::Int(n) => Ok(IrLit::Int(
            n.base10_parse()
                .map_err(|e| CompilerError::ParseError(e.to_string()))?,
        )),
        syn::Lit::Bool(b) => Ok(IrLit::Bool(b.value)),
        syn::Lit::Str(s) => Ok(IrLit::Str(s.value())),
        syn::Lit::Float(f) => Ok(IrLit::Float(
            f.base10_parse()
                .map_err(|e| CompilerError::ParseError(e.to_string()))?,
        )),
        syn::Lit::Byte(b) => Ok(IrLit::Byte(b.value())),
        syn::Lit::ByteStr(bs) => Ok(IrLit::ByteStr(bs.value())),
        syn::Lit::Char(c) => Ok(IrLit::Char(c.value())),
        _ => Err(CompilerError::Unsupported(format!("Literal: {:?}", lit))),
    }
}

fn convert_bin_op(op: syn::BinOp) -> SpecBinOp {
    match op {
        syn::BinOp::Add(_) => SpecBinOp::Add,
        syn::BinOp::Sub(_) => SpecBinOp::Sub,
        syn::BinOp::Mul(_) => SpecBinOp::Mul,
        syn::BinOp::Div(_) => SpecBinOp::Div,
        syn::BinOp::Rem(_) => SpecBinOp::Rem,
        syn::BinOp::BitAnd(_) => SpecBinOp::BitAnd,
        syn::BinOp::BitOr(_) => SpecBinOp::BitOr,
        syn::BinOp::BitXor(_) => SpecBinOp::BitXor,
        syn::BinOp::Shl(_) => SpecBinOp::Shl,
        syn::BinOp::Shr(_) => SpecBinOp::Shr,
        syn::BinOp::Eq(_) => SpecBinOp::Eq,
        syn::BinOp::Ne(_) => SpecBinOp::Ne,
        syn::BinOp::Lt(_) => SpecBinOp::Lt,
        syn::BinOp::Le(_) => SpecBinOp::Le,
        syn::BinOp::Gt(_) => SpecBinOp::Gt,
        syn::BinOp::Ge(_) => SpecBinOp::Ge,
        syn::BinOp::And(_) => SpecBinOp::And,
        syn::BinOp::Or(_) => SpecBinOp::Or,
        _ => SpecBinOp::Add, // Fallback for things like AddAssign which we handle via Assign + Binary
    }
}

fn convert_unary_op(op: syn::UnOp) -> SpecUnaryOp {
    match op {
        syn::UnOp::Not(_) => SpecUnaryOp::Not,
        syn::UnOp::Neg(_) => SpecUnaryOp::Neg,
        syn::UnOp::Deref(_) => SpecUnaryOp::Deref,
        _ => SpecUnaryOp::Not,
    }
}
