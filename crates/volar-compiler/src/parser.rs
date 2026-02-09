//! Parser module for converting Rust source code directly into specialized IR.

use crate::ir::*;
use std::borrow::ToOwned;
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

                // The trait arguments are in the segment at qself.position - 1
                // (the last segment of the trait path in <T as Trait<Args>>::Assoc)
                let mut trait_args = Vec::new();
                let trait_seg_idx = if qself.position > 0 {
                    qself.position - 1
                } else {
                    0
                };
                if let Some(segment) = p.path.segments.get(trait_seg_idx) {
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

                    // Extract trait path - in syn, qself.position points to one
                    // past the last segment of the trait path.
                    // E.g., for `<T as Trait>::Output`:
                    //   path.segments = [Trait, Output], qself.position = 1
                    // So the trait is at segments[qself.position - 1].
                    let trait_path =
                        if qself.position > 0 && qself.position <= p.path.segments.len() {
                            Some(p.path.segments[qself.position - 1].ident.to_string())
                        } else if p.path.segments.len() > 1 {
                            // Fallback: use second-to-last segment
                            Some(p.path.segments[p.path.segments.len() - 2].ident.to_string())
                        } else {
                            None
                        };

                    return Ok(IrType::Projection {
                        base: Box::new(base),
                        trait_path,
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
                        trait_path: None, // Simple T::Assoc doesn't have explicit trait
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
        IrType::Projection {
            base,
            assoc,
            trait_path,
            ..
        } => {
            // Handle projections like Self::BlockSize, T::Output, <T as Trait>::Output
            // Convert to a type param string representation
            let base_str = format!("{}", base);
            let assoc_str = match assoc {
                AssociatedType::Output => "Output",
                AssociatedType::Key => "Key",
                AssociatedType::BlockSize => "BlockSize",
                AssociatedType::OutputSize => "OutputSize",
                AssociatedType::TotalLoopCount => "TotalLoopCount",
                AssociatedType::Other(name) => name,
            };
            Ok(ArrayLength::Projection {
                r#type: base.clone(),
                field: assoc_str.to_owned(),
                trait_path: trait_path.clone(),
            })
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
        syn::Expr::Path(p) => {
            let name = p
                .path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect::<Vec<_>>()
                .join("::");
            Ok(ArrayLength::TypeParam(name))
        }
        _ => {
            // Fallback: stringify the expression as a TypeParam
            let s = quote::quote!(#expr).to_string();
            Ok(ArrayLength::TypeParam(s))
        }
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
        Expr::Range(r) => Ok(IrExpr::Range {
            start: r
                .start
                .as_ref()
                .map(|e| convert_expr(e))
                .transpose()?
                .map(Box::new),
            end: r
                .end
                .as_ref()
                .map(|e| convert_expr(e))
                .transpose()?
                .map(Box::new),
            inclusive: matches!(r.limits, syn::RangeLimits::Closed(_)),
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
        Expr::Macro(m) => {
            let name = m.mac.path.segments.last().unwrap().ident.to_string();
            if name == "unreachable" {
                return Ok(IrExpr::Unreachable);
            }
            if name == "todo" || name == "unimplemented" {
                // Treat as a placeholder expression
                let mut segs = Vec::new();
                segs.push(format!("{}!", name));
                return Ok(IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: segs,
                        type_args: Vec::new(),
                    }),
                    args: Vec::new(),
                });
            }
            // For vec!, parse as an array literal
            if name == "vec" {
                let tokens = m.mac.tokens.clone();
                // Wrap in brackets and parse as ExprArray
                let bracketed =
                    proc_macro2::TokenStream::from_iter([proc_macro2::TokenTree::Group(
                        proc_macro2::Group::new(proc_macro2::Delimiter::Bracket, tokens),
                    )]);
                let as_array: syn::Result<syn::ExprArray> = syn::parse2(bracketed);
                if let Ok(arr) = as_array {
                    let ir_elems = arr
                        .elems
                        .iter()
                        .map(convert_expr)
                        .collect::<Result<Vec<_>>>()?;
                    return Ok(IrExpr::Array(ir_elems));
                }
                // Fallback: empty vec
                return Ok(IrExpr::Array(Vec::new()));
            }
            // For typenum_usize, parse the token stream as a type and convert it
            if name == "typenum_usize" {
                let parsed_ty: Type = syn::parse2(m.mac.tokens.clone()).map_err(|e| {
                    CompilerError::ParseError(format!(
                        "Failed to parse typenum_usize tokens: {}",
                        e
                    ))
                })?;
                let ir_ty = convert_type(&parsed_ty)?;
                return Ok(IrExpr::TypenumUsize {
                    ty: Box::new(ir_ty),
                });
            }

            Err(CompilerError::Unsupported(format!("macro: {}", name)))
        }
        _ => Err(CompilerError::Unsupported(format!("{:?}", expr))),
    }
}

/// Convert an `IrType` (from type parameters) into an `ArrayLength`.
fn type_to_array_length(ty: &IrType) -> ArrayLength {
    match ty {
        IrType::TypeParam(name) => ArrayLength::TypeParam(name.clone()),
        IrType::Projection {
            base,
            assoc,
            trait_path,
            ..
        } => {
            let assoc_str = match assoc {
                AssociatedType::Output => "Output",
                AssociatedType::BlockSize => "BlockSize",
                AssociatedType::OutputSize => "OutputSize",
                AssociatedType::Other(name) => name,
                _ => "Output",
            };
            ArrayLength::Projection {
                r#type: base.clone(),
                field: assoc_str.to_string(),
                trait_path: trait_path.clone(),
            }
        }
        _ => ArrayLength::TypeParam("N".to_string()),
    }
}

/// Extract (elem_ty, len) from type parameters on GenericArray-style constructors.
fn extract_array_type_params(params: &[IrType]) -> (Option<Box<IrType>>, ArrayLength) {
    let elem_ty = params.first().cloned().map(Box::new);
    let len = if params.len() >= 2 {
        type_to_array_length(&params[1])
    } else {
        ArrayLength::TypeParam("N".to_string()) // Placeholder for inferred
    };
    (elem_ty, len)
}

fn convert_call(func: &Expr, args: &[&Expr]) -> Result<IrExpr> {
    if let Expr::Path(p) = func {
        let segments: Vec<String> = p
            .path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect();
        let path_str = segments.join("::");
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

        // GenericArray::generate(|i| body) or Type::generate(|i| body)
        if segments.last().map(|s| s.as_str()) == Some("generate") && args.len() == 1 {
            if let Expr::Closure(c) = args[0] {
                if params.is_empty() {
                    return Err(CompilerError::ParseError(format!(
                        "{}::generate() requires explicit type parameters <T, N>",
                        segments[..segments.len() - 1].join("::")
                    )));
                }
                let (elem_ty, len) = extract_array_type_params(&params);
                return Ok(IrExpr::ArrayGenerate {
                    elem_ty,
                    len,
                    index_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }

        // core::array::from_fn(|i| body)
        if (path_str == "core::array::from_fn" || path_str.ends_with("::from_fn"))
            && args.len() == 1
        {
            if let Expr::Closure(c) = args[0] {
                let (elem_ty, len) = extract_array_type_params(&params);
                return Ok(IrExpr::ArrayGenerate {
                    elem_ty,
                    len,
                    index_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }

        // GenericArray::<T, N>::default() — array filled with default values
        // Unified into DefaultValue with an Array type so that dyn-lowering
        // can recursively expand nested defaults.
        if segments.last().map(|s| s.as_str()) == Some("default")
            && args.is_empty()
            && segments.len() >= 2
            && segments[0] != "O"
        // O::default() is a scalar default, not array
        {
            let prefix = &segments[..segments.len() - 1];
            if prefix == ["GenericArray"]
                || prefix.iter().any(|s| s == "GenericArray" || s == "Vec")
            {
                let (elem_ty, len) = extract_array_type_params(&params);
                let array_ty = match elem_ty {
                    Some(elem) => Some(Box::new(IrType::Array {
                        kind: ArrayKind::GenericArray,
                        elem,
                        len,
                    })),
                    None => {
                        return Err(CompilerError::InvalidType(format!(
                            "Bare `{}::default()` without turbofish type parameters. \
                             Add explicit types, e.g. `GenericArray::<ElemType, LenType>::default()`.",
                            prefix.join("::")
                        )));
                    }
                };
                return Ok(IrExpr::DefaultValue { ty: array_ty });
            }
        }

        // N::to_usize() — length parameter witness
        if segments.last().map(|s| s.as_str()) == Some("to_usize")
            && args.is_empty()
            && segments.len() == 2
        {
            let first = &segments[0];
            // Uppercase single-letter or letter+digits → length param
            let is_length_param = first
                .chars()
                .next()
                .map(|c| c.is_uppercase())
                .unwrap_or(false)
                && first.len() <= 3;
            if is_length_param {
                return Ok(IrExpr::LengthOf(ArrayLength::TypeParam(first.clone())));
            }
        }
    }

    // <B::OutputSize as Unsigned>::to_usize() — qualified path to_usize
    if let Expr::Path(p) = func {
        if let Some(qself) = &p.qself {
            if let Some(last) = p.path.segments.last() {
                if last.ident == "to_usize" && args.is_empty() {
                    let base_ty = convert_type(&qself.ty)?;
                    let len = type_to_array_length(&base_ty);
                    return Ok(IrExpr::LengthOf(len));
                }
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

// ============================================================================
// Iterator chain builder — walks raw syn AST to produce flat IrIterChain
// ============================================================================

/// The result of peeling one layer off an iterator chain.
/// Contains the accumulated steps (outermost first, reversed later) and the source.
struct PeeledChain {
    source: crate::ir::IterChainSource,
    /// Steps collected from innermost to outermost (push order).
    steps: Vec<crate::ir::IterStep>,
}

/// Try to peel a syn expression into an iterator chain source + steps.
/// Returns `None` if the expression is not an iterator chain (no source found).
fn peel_iter_chain(syn_expr: &Expr) -> Result<Option<PeeledChain>> {
    match syn_expr {
        Expr::MethodCall(m) => {
            let method_name = m.method.to_string();
            let args: Vec<&Expr> = m.args.iter().collect();
            match method_name.as_str() {
                // Source methods — these are the base of the chain
                "iter" | "into_iter" | "chars" | "bytes" if args.is_empty() => {
                    let method = match method_name.as_str() {
                        "iter" => crate::ir::IterMethod::Iter,
                        "into_iter" => crate::ir::IterMethod::IntoIter,
                        "chars" => crate::ir::IterMethod::Chars,
                        "bytes" => crate::ir::IterMethod::Bytes,
                        _ => crate::ir::IterMethod::Iter,
                    };
                    Ok(Some(PeeledChain {
                        source: crate::ir::IterChainSource::Method {
                            collection: Box::new(convert_expr(&m.receiver)?),
                            method,
                        },
                        steps: Vec::new(),
                    }))
                }
                // Intermediate steps — peel the receiver and push a step
                "enumerate" if args.is_empty() => {
                    let inner = peel_iter_chain(&m.receiver)?;
                    Ok(inner.map(|mut p| {
                        p.steps.push(crate::ir::IterStep::Enumerate);
                        p
                    }))
                }
                "map" if args.len() == 1 => {
                    if let Expr::Closure(c) = args[0] {
                        let inner = peel_iter_chain(&m.receiver)?;
                        if let Some(mut p) = inner {
                            p.steps.push(crate::ir::IterStep::Map {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(p));
                        }
                    }
                    Ok(None)
                }
                "filter" if args.len() == 1 => {
                    if let Expr::Closure(c) = args[0] {
                        let inner = peel_iter_chain(&m.receiver)?;
                        if let Some(mut p) = inner {
                            p.steps.push(crate::ir::IterStep::Filter {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(p));
                        }
                    }
                    Ok(None)
                }
                "filter_map" if args.len() == 1 => {
                    if let Expr::Closure(c) = args[0] {
                        let inner = peel_iter_chain(&m.receiver)?;
                        if let Some(mut p) = inner {
                            p.steps.push(crate::ir::IterStep::FilterMap {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(p));
                        }
                    }
                    Ok(None)
                }
                "flat_map" if args.len() == 1 => {
                    if let Expr::Closure(c) = args[0] {
                        let inner = peel_iter_chain(&m.receiver)?;
                        if let Some(mut p) = inner {
                            p.steps.push(crate::ir::IterStep::FlatMap {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(p));
                        }
                    }
                    Ok(None)
                }
                "take" if args.len() == 1 => {
                    let inner = peel_iter_chain(&m.receiver)?;
                    if let Some(mut p) = inner {
                        p.steps.push(crate::ir::IterStep::Take {
                            count: Box::new(convert_expr(args[0])?),
                        });
                        return Ok(Some(p));
                    }
                    Ok(None)
                }
                "skip" if args.len() == 1 => {
                    let inner = peel_iter_chain(&m.receiver)?;
                    if let Some(mut p) = inner {
                        p.steps.push(crate::ir::IterStep::Skip {
                            count: Box::new(convert_expr(args[0])?),
                        });
                        return Ok(Some(p));
                    }
                    Ok(None)
                }
                "chain" if args.len() == 1 => {
                    let inner = peel_iter_chain(&m.receiver)?;
                    if let Some(mut p) = inner {
                        // Try to peel the chain argument as an iterator chain
                        let other = match peel_iter_chain(args[0])? {
                            Some(other_p) => crate::ir::IrIterChain {
                                source: other_p.source,
                                steps: other_p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                            None => {
                                // Not a chain — wrap as IntoIter on the expression
                                crate::ir::IrIterChain {
                                    source: crate::ir::IterChainSource::Method {
                                        collection: Box::new(convert_expr(args[0])?),
                                        method: crate::ir::IterMethod::IntoIter,
                                    },
                                    steps: Vec::new(),
                                    terminal: crate::ir::IterTerminal::Lazy,
                                }
                            }
                        };
                        p.steps.push(crate::ir::IterStep::Chain {
                            other: Box::new(other),
                        });
                        return Ok(Some(p));
                    }
                    Ok(None)
                }
                // zip — creates a Zip source from two chains
                "zip" if args.len() == 1 => {
                    // The receiver is the left chain, arg is the right
                    let left = peel_iter_chain(&m.receiver)?;
                    if let Some(left_p) = left {
                        let right = match peel_iter_chain(args[0])? {
                            Some(right_p) => crate::ir::IrIterChain {
                                source: right_p.source,
                                steps: right_p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                            None => crate::ir::IrIterChain {
                                source: crate::ir::IterChainSource::Method {
                                    collection: Box::new(convert_expr(args[0])?),
                                    method: crate::ir::IterMethod::IntoIter,
                                },
                                steps: Vec::new(),
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                        };
                        let left_chain = crate::ir::IrIterChain {
                            source: left_p.source,
                            steps: left_p.steps,
                            terminal: crate::ir::IterTerminal::Lazy,
                        };
                        return Ok(Some(PeeledChain {
                            source: crate::ir::IterChainSource::Zip {
                                left: Box::new(left_chain),
                                right: Box::new(right),
                            },
                            steps: Vec::new(),
                        }));
                    }
                    Ok(None)
                }
                // Terminal methods — if receiver peels, produce a chain with terminal.
                // These are handled by try_build_iter_chain, not here.
                // fold/collect are terminals and should not appear as intermediate steps.
                _ => Ok(None),
            }
        }
        Expr::Range(r) => {
            // A range is an iterator source
            let start = r
                .start
                .as_ref()
                .map(|e| convert_expr(e))
                .transpose()?
                .unwrap_or(IrExpr::Lit(IrLit::Int(0)));
            let end = r
                .end
                .as_ref()
                .map(|e| convert_expr(e))
                .transpose()?
                .unwrap_or(IrExpr::Lit(IrLit::Int(0)));
            Ok(Some(PeeledChain {
                source: crate::ir::IterChainSource::Range {
                    start: Box::new(start),
                    end: Box::new(end),
                    inclusive: matches!(r.limits, syn::RangeLimits::Closed(_)),
                },
                steps: Vec::new(),
            }))
        }
        // Parenthesized expressions — unwrap
        Expr::Paren(p) => peel_iter_chain(&p.expr),
        _ => Ok(None),
    }
}

/// Try to build a complete `IrIterChain` from the current method call context.
/// Returns `None` if the expression is not an iterator chain.
fn try_build_iter_chain(
    receiver: &Expr,
    method: &str,
    args: &[&Expr],
) -> Result<Option<crate::ir::IrIterChain>> {
    match method {
        "fold" if args.len() == 2 => {
            if let Expr::Closure(c) = args[1] {
                let peeled = peel_iter_chain(receiver)?;
                if let Some(p) = peeled {
                    return Ok(Some(crate::ir::IrIterChain {
                        source: p.source,
                        steps: p.steps,
                        terminal: crate::ir::IterTerminal::Fold {
                            init: Box::new(convert_expr(args[0])?),
                            acc_var: extract_ir_pattern(&c.inputs[0]),
                            elem_var: extract_ir_pattern(&c.inputs[1]),
                            body: Box::new(convert_expr(&c.body)?),
                        },
                    }));
                }
            }
            Ok(None)
        }
        "collect" if args.is_empty() => {
            let peeled = peel_iter_chain(receiver)?;
            Ok(peeled.map(|p| crate::ir::IrIterChain {
                source: p.source,
                steps: p.steps,
                terminal: crate::ir::IterTerminal::Collect,
            }))
        }
        // map/filter/etc. that don't have a terminal — they produce a lazy chain
        "map" | "filter" | "filter_map" | "flat_map" | "enumerate" | "take" | "skip" | "chain"
        | "zip" => {
            // Reconstruct the full call as an Expr::MethodCall to let peel_iter_chain handle it
            // We can't do that easily without owning the syn tree, so instead we peel the receiver
            // and add this step manually.
            let peeled = peel_iter_chain(receiver)?;
            if let Some(mut p) = peeled {
                match method {
                    "map" if args.len() == 1 => {
                        if let Expr::Closure(c) = args[0] {
                            p.steps.push(crate::ir::IterStep::Map {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(crate::ir::IrIterChain {
                                source: p.source,
                                steps: p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            }));
                        }
                    }
                    "filter" if args.len() == 1 => {
                        if let Expr::Closure(c) = args[0] {
                            p.steps.push(crate::ir::IterStep::Filter {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(crate::ir::IrIterChain {
                                source: p.source,
                                steps: p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            }));
                        }
                    }
                    "filter_map" if args.len() == 1 => {
                        if let Expr::Closure(c) = args[0] {
                            p.steps.push(crate::ir::IterStep::FilterMap {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(crate::ir::IrIterChain {
                                source: p.source,
                                steps: p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            }));
                        }
                    }
                    "flat_map" if args.len() == 1 => {
                        if let Expr::Closure(c) = args[0] {
                            p.steps.push(crate::ir::IterStep::FlatMap {
                                var: extract_ir_pattern(&c.inputs[0]),
                                body: Box::new(convert_expr(&c.body)?),
                            });
                            return Ok(Some(crate::ir::IrIterChain {
                                source: p.source,
                                steps: p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            }));
                        }
                    }
                    "enumerate" if args.is_empty() => {
                        p.steps.push(crate::ir::IterStep::Enumerate);
                        return Ok(Some(crate::ir::IrIterChain {
                            source: p.source,
                            steps: p.steps,
                            terminal: crate::ir::IterTerminal::Lazy,
                        }));
                    }
                    "take" if args.len() == 1 => {
                        p.steps.push(crate::ir::IterStep::Take {
                            count: Box::new(convert_expr(args[0])?),
                        });
                        return Ok(Some(crate::ir::IrIterChain {
                            source: p.source,
                            steps: p.steps,
                            terminal: crate::ir::IterTerminal::Lazy,
                        }));
                    }
                    "skip" if args.len() == 1 => {
                        p.steps.push(crate::ir::IterStep::Skip {
                            count: Box::new(convert_expr(args[0])?),
                        });
                        return Ok(Some(crate::ir::IrIterChain {
                            source: p.source,
                            steps: p.steps,
                            terminal: crate::ir::IterTerminal::Lazy,
                        }));
                    }
                    "chain" if args.len() == 1 => {
                        let other = match peel_iter_chain(args[0])? {
                            Some(other_p) => crate::ir::IrIterChain {
                                source: other_p.source,
                                steps: other_p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                            None => crate::ir::IrIterChain {
                                source: crate::ir::IterChainSource::Method {
                                    collection: Box::new(convert_expr(args[0])?),
                                    method: crate::ir::IterMethod::IntoIter,
                                },
                                steps: Vec::new(),
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                        };
                        p.steps.push(crate::ir::IterStep::Chain {
                            other: Box::new(other),
                        });
                        return Ok(Some(crate::ir::IrIterChain {
                            source: p.source,
                            steps: p.steps,
                            terminal: crate::ir::IterTerminal::Lazy,
                        }));
                    }
                    "zip" if args.len() == 1 => {
                        let right = match peel_iter_chain(args[0])? {
                            Some(right_p) => crate::ir::IrIterChain {
                                source: right_p.source,
                                steps: right_p.steps,
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                            None => crate::ir::IrIterChain {
                                source: crate::ir::IterChainSource::Method {
                                    collection: Box::new(convert_expr(args[0])?),
                                    method: crate::ir::IterMethod::IntoIter,
                                },
                                steps: Vec::new(),
                                terminal: crate::ir::IterTerminal::Lazy,
                            },
                        };
                        let left_chain = crate::ir::IrIterChain {
                            source: p.source,
                            steps: p.steps,
                            terminal: crate::ir::IterTerminal::Lazy,
                        };
                        return Ok(Some(crate::ir::IrIterChain {
                            source: crate::ir::IterChainSource::Zip {
                                left: Box::new(left_chain),
                                right: Box::new(right),
                            },
                            steps: Vec::new(),
                            terminal: crate::ir::IterTerminal::Lazy,
                        }));
                    }
                    _ => {}
                }
            }
            Ok(None)
        }
        // iter/into_iter/etc. as bare source (no steps or terminal)
        "iter" | "into_iter" | "chars" | "bytes" if args.is_empty() => {
            let method = match method {
                "iter" => crate::ir::IterMethod::Iter,
                "into_iter" => crate::ir::IterMethod::IntoIter,
                "chars" => crate::ir::IterMethod::Chars,
                "bytes" => crate::ir::IterMethod::Bytes,
                _ => crate::ir::IterMethod::Iter,
            };
            Ok(Some(crate::ir::IrIterChain {
                source: crate::ir::IterChainSource::Method {
                    collection: Box::new(convert_expr(receiver)?),
                    method,
                },
                steps: Vec::new(),
                terminal: crate::ir::IterTerminal::Lazy,
            }))
        }
        _ => Ok(None),
    }
}

fn convert_method_call(receiver: &Expr, method: &str, args: &[&Expr]) -> Result<IrExpr> {
    // First, try to build a flat iterator chain
    if let Some(chain) = try_build_iter_chain(receiver, method, args)? {
        return Ok(IrExpr::IterPipeline(chain));
    }

    // Non-iterator .zip(other, |a, b| body) — GenericArray style
    if method == "zip" && args.len() == 2 {
        if let Expr::Closure(c) = args[1] {
            let left_var = extract_ir_pattern(&c.inputs[0]);
            let right_var = extract_ir_pattern(&c.inputs[1]);
            return Ok(IrExpr::RawZip {
                left: Box::new(convert_expr(receiver)?),
                right: Box::new(convert_expr(args[0])?),
                left_var,
                right_var,
                body: Box::new(convert_expr(&c.body)?),
            });
        }
    }

    // Non-iterator .map(|x| body) — GenericArray / [T; N] style
    if method == "map" && args.len() == 1 {
        if let Expr::Closure(c) = args[0] {
            if c.inputs.len() == 1 {
                return Ok(IrExpr::RawMap {
                    receiver: Box::new(convert_expr(receiver)?),
                    elem_var: extract_ir_pattern(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }
    }

    // Non-iterator .fold(init, |acc, elem| body) — GenericArray style
    if method == "fold" && args.len() == 2 {
        if let Expr::Closure(c) = args[1] {
            if c.inputs.len() == 2 {
                return Ok(IrExpr::RawFold {
                    receiver: Box::new(convert_expr(receiver)?),
                    init: Box::new(convert_expr(args[0])?),
                    acc_var: extract_ir_pattern(&c.inputs[0]),
                    elem_var: extract_ir_pattern(&c.inputs[1]),
                    body: Box::new(convert_expr(&c.body)?),
                });
            }
        }
    }

    // Default: generic method call
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
    match pat {
        Pat::Ident(pi) => pi.ident.to_string(),
        Pat::Tuple(t) => {
            let names: Vec<String> = t.elems.iter().map(|p| extract_pat_name(p)).collect();
            format!("({})", names.join(", "))
        }
        Pat::TupleStruct(ts) => {
            let names: Vec<String> = ts.elems.iter().map(|p| extract_pat_name(p)).collect();
            let path = ts
                .path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect::<Vec<_>>()
                .join("::");
            format!("{}({})", path, names.join(", "))
        }
        Pat::Wild(_) => "_".to_string(),
        Pat::Reference(r) => {
            let inner = extract_pat_name(&r.pat);
            if r.mutability.is_some() {
                format!("ref mut {}", inner)
            } else {
                format!("&{}", inner)
            }
        }
        _ => "_".to_string(),
    }
}

/// Convert a `syn::Pat` to an `IrPattern`.
fn extract_ir_pattern(pat: &Pat) -> IrPattern {
    match pat {
        Pat::Ident(pi) => IrPattern::Ident {
            mutable: pi.mutability.is_some(),
            name: pi.ident.to_string(),
            subpat: None,
        },
        Pat::Tuple(t) => IrPattern::Tuple(t.elems.iter().map(extract_ir_pattern).collect()),
        Pat::Wild(_) => IrPattern::Wild,
        Pat::Reference(r) => {
            // References in closure patterns are rare but handle them
            extract_ir_pattern(&r.pat)
        }
        _ => IrPattern::Wild,
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
