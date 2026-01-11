//! Parser module for converting Rust source code into our IR.
//!
//! Uses the `syn` crate to parse Rust source and then converts to our IR format.

use crate::ir::*;
use syn::{
    self, BinOp, Expr, Fields, FnArg, GenericArgument, GenericParam, ImplItem,
    Item, ItemFn, ItemImpl, ItemStruct, ItemTrait, Lit, Member, Pat, Path, PathArguments,
    ReturnType, Stmt, TraitItem, Type, TypeParamBound, UnOp, WherePredicate,
};

/// Parses a Rust source file into an IR module
pub fn parse_source(source: &str, module_name: &str) -> Result<IrModule, syn::Error> {
    let file = syn::parse_file(source)?;
    let mut module = IrModule::new(module_name);

    // Process use statements
    for item in &file.items {
        if let Item::Use(use_item) = item {
            module.uses.extend(convert_use(&use_item.tree));
        }
    }

    // Process items
    for item in &file.items {
        match item {
            Item::Struct(s) => {
                module.structs.push(convert_struct(s));
            }
            Item::Trait(t) => {
                module.traits.push(convert_trait(t));
            }
            Item::Impl(i) => {
                module.impls.push(convert_impl(i));
            }
            Item::Fn(f) => {
                module.functions.push(convert_function(f));
            }
            Item::Type(t) => {
                module.type_aliases.push(convert_type_alias(t));
            }
            Item::Mod(_m) => {
                // For now, we don't recursively process modules
                // Could be extended to handle inline modules
            }
            _ => {}
        }
    }

    Ok(module)
}

/// Parse multiple source files into a single IR module
pub fn parse_sources(sources: &[(&str, &str)], module_name: &str) -> Result<IrModule, syn::Error> {
    let mut combined = IrModule::new(module_name);

    for (source, sub_name) in sources {
        let sub_module = parse_source(source, sub_name)?;
        combined.structs.extend(sub_module.structs);
        combined.traits.extend(sub_module.traits);
        combined.impls.extend(sub_module.impls);
        combined.functions.extend(sub_module.functions);
        combined.type_aliases.extend(sub_module.type_aliases);
        combined.uses.extend(sub_module.uses);
    }

    Ok(combined)
}

fn convert_use(tree: &syn::UseTree) -> Vec<IrUse> {
    match tree {
        syn::UseTree::Path(p) => {
            let mut uses = convert_use(&p.tree);
            for u in &mut uses {
                u.path.insert(0, p.ident.to_string());
            }
            uses
        }
        syn::UseTree::Name(n) => {
            vec![IrUse {
                path: vec![n.ident.to_string()],
                alias: None,
                glob: false,
            }]
        }
        syn::UseTree::Rename(r) => {
            vec![IrUse {
                path: vec![r.ident.to_string()],
                alias: Some(r.rename.to_string()),
                glob: false,
            }]
        }
        syn::UseTree::Glob(_) => {
            vec![IrUse {
                path: vec![],
                alias: None,
                glob: true,
            }]
        }
        syn::UseTree::Group(g) => g.items.iter().flat_map(convert_use).collect(),
    }
}

fn convert_type_alias(t: &syn::ItemType) -> IrTypeAlias {
    IrTypeAlias {
        name: t.ident.to_string(),
        generics: convert_generics(&t.generics),
        ty: convert_type(&t.ty),
    }
}

fn convert_struct(s: &ItemStruct) -> IrStruct {
    let fields = match &s.fields {
        Fields::Named(named) => named
            .named
            .iter()
            .map(|f| IrField {
                name: f.ident.as_ref().map(|i| i.to_string()).unwrap_or_default(),
                ty: convert_type(&f.ty),
                visibility: convert_visibility(&f.vis),
            })
            .collect(),
        Fields::Unnamed(unnamed) => unnamed
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, f)| IrField {
                name: i.to_string(),
                ty: convert_type(&f.ty),
                visibility: convert_visibility(&f.vis),
            })
            .collect(),
        Fields::Unit => vec![],
    };

    IrStruct {
        name: s.ident.to_string(),
        generics: convert_generics(&s.generics),
        fields,
        is_tuple: matches!(s.fields, Fields::Unnamed(_)),
    }
}

fn convert_trait(t: &ItemTrait) -> IrTrait {
    let super_traits = t
        .supertraits
        .iter()
        .filter_map(|bound| {
            if let TypeParamBound::Trait(trait_bound) = bound {
                Some(convert_trait_bound_from_path(&trait_bound.path))
            } else {
                None
            }
        })
        .collect();

    let items = t.items.iter().map(convert_trait_item).collect();

    IrTrait {
        name: t.ident.to_string(),
        generics: convert_generics(&t.generics),
        super_traits,
        items,
    }
}

fn convert_trait_item(item: &TraitItem) -> IrTraitItem {
    match item {
        TraitItem::Fn(method) => IrTraitItem::Method(IrMethodSignature {
            name: method.sig.ident.to_string(),
            generics: convert_generics(&method.sig.generics),
            receiver: method.sig.receiver().map(convert_receiver),
            params: method
                .sig
                .inputs
                .iter()
                .filter_map(|arg| {
                    if let FnArg::Typed(pat_type) = arg {
                        Some(IrParam {
                            name: pat_to_string(&pat_type.pat),
                            ty: convert_type(&pat_type.ty),
                        })
                    } else {
                        None
                    }
                })
                .collect(),
            return_type: convert_return_type(&method.sig.output),
            where_clause: method
                .sig
                .generics
                .where_clause
                .as_ref()
                .map(|w| convert_where_clause(w))
                .unwrap_or_default(),
        }),
        TraitItem::Type(assoc_type) => IrTraitItem::AssociatedType {
            name: assoc_type.ident.to_string(),
            bounds: assoc_type
                .bounds
                .iter()
                .filter_map(|b| {
                    if let TypeParamBound::Trait(t) = b {
                        Some(convert_trait_bound_from_path(&t.path))
                    } else {
                        None
                    }
                })
                .collect(),
            default: assoc_type.default.as_ref().map(|(_, ty)| convert_type(ty)),
        },
        TraitItem::Const(c) => IrTraitItem::Const {
            name: c.ident.to_string(),
            ty: convert_type(&c.ty),
            default: c.default.as_ref().map(|(_, expr)| convert_expr(expr)),
        },
        _ => IrTraitItem::Method(IrMethodSignature {
            name: String::new(),
            generics: vec![],
            receiver: None,
            params: vec![],
            return_type: None,
            where_clause: vec![],
        }),
    }
}

fn convert_impl(i: &ItemImpl) -> IrImpl {
    let trait_ = i.trait_.as_ref().map(|(_, path, _)| IrTraitRef {
        path: path.segments.iter().map(|s| s.ident.to_string()).collect(),
        type_args: extract_type_args_from_path(path),
    });

    let items = i.items.iter().map(convert_impl_item).collect();

    IrImpl {
        generics: convert_generics(&i.generics),
        trait_,
        self_ty: convert_type(&i.self_ty),
        where_clause: i
            .generics
            .where_clause
            .as_ref()
            .map(|w| convert_where_clause(w))
            .unwrap_or_default(),
        items,
    }
}

fn convert_impl_item(item: &ImplItem) -> IrImplItem {
    match item {
        ImplItem::Fn(method) => IrImplItem::Method(IrFunction {
            name: method.sig.ident.to_string(),
            generics: convert_generics(&method.sig.generics),
            receiver: method.sig.receiver().map(convert_receiver),
            params: method
                .sig
                .inputs
                .iter()
                .filter_map(|arg| {
                    if let FnArg::Typed(pat_type) = arg {
                        Some(IrParam {
                            name: pat_to_string(&pat_type.pat),
                            ty: convert_type(&pat_type.ty),
                        })
                    } else {
                        None
                    }
                })
                .collect(),
            return_type: convert_return_type(&method.sig.output),
            where_clause: method
                .sig
                .generics
                .where_clause
                .as_ref()
                .map(|w| convert_where_clause(w))
                .unwrap_or_default(),
            body: convert_block(&method.block),
        }),
        ImplItem::Type(assoc_type) => IrImplItem::AssociatedType {
            name: assoc_type.ident.to_string(),
            ty: convert_type(&assoc_type.ty),
        },
        ImplItem::Const(c) => IrImplItem::Const {
            name: c.ident.to_string(),
            ty: convert_type(&c.ty),
            value: convert_expr(&c.expr),
        },
        _ => IrImplItem::AssociatedType {
            name: String::new(),
            ty: IrType::Unit,
        },
    }
}

fn convert_function(f: &ItemFn) -> IrFunction {
    IrFunction {
        name: f.sig.ident.to_string(),
        generics: convert_generics(&f.sig.generics),
        receiver: f.sig.receiver().map(convert_receiver),
        params: f
            .sig
            .inputs
            .iter()
            .filter_map(|arg| {
                if let FnArg::Typed(pat_type) = arg {
                    Some(IrParam {
                        name: pat_to_string(&pat_type.pat),
                        ty: convert_type(&pat_type.ty),
                    })
                } else {
                    None
                }
            })
            .collect(),
        return_type: convert_return_type(&f.sig.output),
        where_clause: f
            .sig
            .generics
            .where_clause
            .as_ref()
            .map(|w| convert_where_clause(w))
            .unwrap_or_default(),
        body: convert_block(&f.block),
    }
}

fn convert_receiver(recv: &syn::Receiver) -> IrReceiver {
    if recv.reference.is_some() {
        if recv.mutability.is_some() {
            IrReceiver::RefMut
        } else {
            IrReceiver::Ref
        }
    } else {
        IrReceiver::Value
    }
}

fn convert_visibility(vis: &syn::Visibility) -> Visibility {
    match vis {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(r) => {
            if r.path.is_ident("crate") {
                Visibility::Crate
            } else {
                Visibility::Private
            }
        }
        syn::Visibility::Inherited => Visibility::Private,
    }
}

fn convert_generics(generics: &syn::Generics) -> Vec<IrGenericParam> {
    generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(type_param) => Some(IrGenericParam {
                name: type_param.ident.to_string(),
                bounds: type_param
                    .bounds
                    .iter()
                    .filter_map(|b| {
                        if let TypeParamBound::Trait(t) = b {
                            Some(convert_trait_bound_from_path(&t.path))
                        } else {
                            None
                        }
                    })
                    .collect(),
                default: type_param.default.as_ref().map(convert_type),
            }),
            GenericParam::Const(const_param) => Some(IrGenericParam {
                name: const_param.ident.to_string(),
                bounds: vec![],
                default: const_param.default.as_ref().map(convert_expr_to_type),
            }),
            GenericParam::Lifetime(_) => None,
        })
        .collect()
}

fn convert_expr_to_type(expr: &Expr) -> IrType {
    // For const generics, we represent the default value as a type
    // This is a simplification
    IrType::Path {
        segments: vec![format!("{:?}", expr)],
        type_args: vec![],
    }
}

fn convert_where_clause(where_clause: &syn::WhereClause) -> Vec<IrWherePredicate> {
    where_clause
        .predicates
        .iter()
        .map(|pred| match pred {
            WherePredicate::Type(type_pred) => IrWherePredicate::TypeBound {
                ty: convert_type(&type_pred.bounded_ty),
                bounds: type_pred
                    .bounds
                    .iter()
                    .filter_map(|b| {
                        if let TypeParamBound::Trait(t) = b {
                            Some(convert_trait_bound_from_path(&t.path))
                        } else {
                            None
                        }
                    })
                    .collect(),
            },
            WherePredicate::Lifetime(lifetime_pred) => IrWherePredicate::Lifetime {
                name: lifetime_pred.lifetime.ident.to_string(),
                bounds: lifetime_pred
                    .bounds
                    .iter()
                    .map(|lt| lt.ident.to_string())
                    .collect(),
            },
            _ => IrWherePredicate::TypeBound {
                ty: IrType::Unit,
                bounds: vec![],
            },
        })
        .collect()
}

fn convert_trait_bound_from_path(path: &Path) -> IrTraitBound {
    let mut assoc_type_bindings = vec![];
    let mut type_args = vec![];

    if let Some(last) = path.segments.last() {
        if let PathArguments::AngleBracketed(args) = &last.arguments {
            for arg in &args.args {
                match arg {
                    GenericArgument::Type(ty) => {
                        type_args.push(convert_type(ty));
                    }
                    GenericArgument::AssocType(assoc) => {
                        assoc_type_bindings.push((assoc.ident.to_string(), convert_type(&assoc.ty)));
                    }
                    _ => {}
                }
            }
        }
    }

    IrTraitBound {
        path: path.segments.iter().map(|s| s.ident.to_string()).collect(),
        type_args,
        assoc_type_bindings,
    }
}

fn extract_type_args_from_path(path: &Path) -> Vec<IrType> {
    path.segments
        .last()
        .map(|seg| {
            if let PathArguments::AngleBracketed(args) = &seg.arguments {
                args.args
                    .iter()
                    .filter_map(|arg| {
                        if let GenericArgument::Type(ty) = arg {
                            Some(convert_type(ty))
                        } else {
                            None
                        }
                    })
                    .collect()
            } else {
                vec![]
            }
        })
        .unwrap_or_default()
}

fn convert_type(ty: &Type) -> IrType {
    match ty {
        Type::Path(type_path) => {
            let segments: Vec<String> = type_path
                .path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect();

            let type_args = type_path
                .path
                .segments
                .last()
                .map(|seg| {
                    if let PathArguments::AngleBracketed(args) = &seg.arguments {
                        args.args
                            .iter()
                            .filter_map(|arg| match arg {
                                GenericArgument::Type(ty) => Some(convert_type(ty)),
                                _ => None,
                            })
                            .collect()
                    } else {
                        vec![]
                    }
                })
                .unwrap_or_default();

            // Check for associated type projection
            if let Some(qself) = &type_path.qself {
                return IrType::Projection {
                    base: Box::new(convert_type(&qself.ty)),
                    assoc: segments.join("::"),
                    type_args,
                };
            }

            IrType::Path {
                segments,
                type_args,
            }
        }
        Type::Reference(reference) => IrType::Reference {
            mutable: reference.mutability.is_some(),
            elem: Box::new(convert_type(&reference.elem)),
        },
        Type::Slice(slice) => IrType::Slice(Box::new(convert_type(&slice.elem))),
        Type::Array(array) => IrType::Array {
            elem: Box::new(convert_type(&array.elem)),
            len: Box::new(convert_expr(&array.len)),
        },
        Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                IrType::Unit
            } else {
                IrType::Tuple(tuple.elems.iter().map(convert_type).collect())
            }
        }
        Type::BareFn(fn_type) => IrType::FnPtr {
            params: fn_type.inputs.iter().map(|arg| convert_type(&arg.ty)).collect(),
            ret: Box::new(convert_return_type(&fn_type.output).unwrap_or(IrType::Unit)),
        },
        Type::ImplTrait(impl_trait) => IrType::ImplTrait(
            impl_trait
                .bounds
                .iter()
                .filter_map(|b| {
                    if let TypeParamBound::Trait(t) = b {
                        Some(convert_trait_bound_from_path(&t.path))
                    } else {
                        None
                    }
                })
                .collect(),
        ),
        Type::TraitObject(trait_obj) => IrType::DynTrait(
            trait_obj
                .bounds
                .iter()
                .filter_map(|b| {
                    if let TypeParamBound::Trait(t) = b {
                        Some(convert_trait_bound_from_path(&t.path))
                    } else {
                        None
                    }
                })
                .collect(),
        ),
        Type::Never(_) => IrType::Never,
        Type::Infer(_) => IrType::Infer,
        Type::Paren(paren) => convert_type(&paren.elem),
        Type::Group(group) => convert_type(&group.elem),
        _ => IrType::Unit,
    }
}

fn convert_return_type(ret: &ReturnType) -> Option<IrType> {
    match ret {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => Some(convert_type(ty)),
    }
}

fn convert_block(block: &syn::Block) -> IrBlock {
    let mut stmts = Vec::new();
    let mut final_expr = None;

    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last = i == block.stmts.len() - 1;
        match stmt {
            Stmt::Local(local) => {
                // In syn 2.0, type annotations are part of PatType patterns
                let (pattern, ty) = extract_pattern_type(&local.pat);
                stmts.push(IrStmt::Let {
                    pattern,
                    ty,
                    init: local.init.as_ref().map(|init| convert_expr(&init.expr)),
                });
            }
            Stmt::Item(item) => {
                if let Some(item_stmt) = convert_item_stmt(item) {
                    stmts.push(IrStmt::Item(item_stmt));
                }
            }
            Stmt::Expr(expr, semi) => {
                if is_last && semi.is_none() {
                    final_expr = Some(Box::new(convert_expr(expr)));
                } else if semi.is_some() {
                    stmts.push(IrStmt::Semi(convert_expr(expr)));
                } else {
                    stmts.push(IrStmt::Expr(convert_expr(expr)));
                }
            }
            Stmt::Macro(mac) => {
                let ir_expr = IrExpr::Macro {
                    path: mac.mac.path.segments.iter().map(|s| s.ident.to_string()).collect(),
                    tokens: mac.mac.tokens.to_string(),
                };
                if is_last && mac.semi_token.is_none() {
                    final_expr = Some(Box::new(ir_expr));
                } else {
                    stmts.push(IrStmt::Semi(ir_expr));
                }
            }
        }
    }

    IrBlock {
        stmts,
        expr: final_expr,
    }
}

fn convert_item_stmt(item: &Item) -> Option<IrItemStmt> {
    match item {
        Item::Fn(f) => Some(IrItemStmt::Fn(convert_function(f))),
        Item::Struct(s) => Some(IrItemStmt::Struct(convert_struct(s))),
        Item::Const(c) => Some(IrItemStmt::Const {
            name: c.ident.to_string(),
            ty: convert_type(&c.ty),
            value: convert_expr(&c.expr),
        }),
        _ => None,
    }
}

/// Extract the pattern and optional type from a pattern (handles PatType)
fn extract_pattern_type(pat: &Pat) -> (IrPattern, Option<IrType>) {
    match pat {
        Pat::Type(pat_type) => {
            (convert_pattern(&pat_type.pat), Some(convert_type(&pat_type.ty)))
        }
        _ => (convert_pattern(pat), None),
    }
}

fn convert_pattern(pat: &Pat) -> IrPattern {
    match pat {
        Pat::Ident(ident) => IrPattern::Ident {
            mutable: ident.mutability.is_some(),
            name: ident.ident.to_string(),
            subpat: ident.subpat.as_ref().map(|(_, p)| Box::new(convert_pattern(p))),
        },
        Pat::Tuple(tuple) => IrPattern::Tuple(tuple.elems.iter().map(convert_pattern).collect()),
        Pat::Struct(s) => IrPattern::Struct {
            path: s.path.segments.iter().map(|seg| seg.ident.to_string()).collect(),
            fields: s
                .fields
                .iter()
                .map(|f| {
                    let name = match &f.member {
                        Member::Named(ident) => ident.to_string(),
                        Member::Unnamed(index) => index.index.to_string(),
                    };
                    (name, convert_pattern(&f.pat))
                })
                .collect(),
            rest: s.rest.is_some(),
        },
        Pat::TupleStruct(ts) => IrPattern::TupleStruct {
            path: ts.path.segments.iter().map(|seg| seg.ident.to_string()).collect(),
            elems: ts.elems.iter().map(convert_pattern).collect(),
        },
        Pat::Slice(slice) => IrPattern::Slice(slice.elems.iter().map(convert_pattern).collect()),
        Pat::Wild(_) => IrPattern::Wild,
        Pat::Lit(lit) => IrPattern::Lit(convert_lit_expr(&lit.lit)),
        Pat::Reference(r) => IrPattern::Ref {
            mutable: r.mutability.is_some(),
            pat: Box::new(convert_pattern(&r.pat)),
        },
        Pat::Or(or) => IrPattern::Or(or.cases.iter().map(convert_pattern).collect()),
        Pat::Rest(_) => IrPattern::Rest,
        Pat::Paren(paren) => convert_pattern(&paren.pat),
        _ => IrPattern::Wild,
    }
}

fn convert_expr(expr: &Expr) -> IrExpr {
    match expr {
        Expr::Lit(lit) => IrExpr::Lit(convert_lit_expr(&lit.lit)),
        Expr::Path(path) => IrExpr::Path {
            segments: path.path.segments.iter().map(|s| s.ident.to_string()).collect(),
            type_args: extract_type_args_from_path(&path.path),
        },
        Expr::Binary(binary) => IrExpr::Binary {
            op: convert_bin_op(&binary.op),
            left: Box::new(convert_expr(&binary.left)),
            right: Box::new(convert_expr(&binary.right)),
        },
        Expr::Unary(unary) => IrExpr::Unary {
            op: convert_unary_op(&unary.op),
            expr: Box::new(convert_expr(&unary.expr)),
        },
        Expr::Call(call) => IrExpr::Call {
            func: Box::new(convert_expr(&call.func)),
            args: call.args.iter().map(convert_expr).collect(),
        },
        Expr::MethodCall(method_call) => IrExpr::MethodCall {
            receiver: Box::new(convert_expr(&method_call.receiver)),
            method: method_call.method.to_string(),
            type_args: method_call
                .turbofish
                .as_ref()
                .map(|args| {
                    args.args
                        .iter()
                        .filter_map(|arg| {
                            if let GenericArgument::Type(ty) = arg {
                                Some(convert_type(ty))
                            } else {
                                None
                            }
                        })
                        .collect()
                })
                .unwrap_or_default(),
            args: method_call.args.iter().map(convert_expr).collect(),
        },
        Expr::Field(field) => IrExpr::Field {
            base: Box::new(convert_expr(&field.base)),
            field: match &field.member {
                Member::Named(ident) => ident.to_string(),
                Member::Unnamed(index) => index.index.to_string(),
            },
        },
        Expr::Index(index) => IrExpr::Index {
            base: Box::new(convert_expr(&index.expr)),
            index: Box::new(convert_expr(&index.index)),
        },
        Expr::Struct(s) => IrExpr::Struct {
            path: s.path.segments.iter().map(|seg| seg.ident.to_string()).collect(),
            fields: s
                .fields
                .iter()
                .map(|f| {
                    let name = match &f.member {
                        Member::Named(ident) => ident.to_string(),
                        Member::Unnamed(index) => index.index.to_string(),
                    };
                    (name, convert_expr(&f.expr))
                })
                .collect(),
            rest: s.rest.as_ref().map(|r| Box::new(convert_expr(r))),
        },
        Expr::Tuple(tuple) => IrExpr::Tuple(tuple.elems.iter().map(convert_expr).collect()),
        Expr::Array(array) => IrExpr::Array(array.elems.iter().map(convert_expr).collect()),
        Expr::Repeat(repeat) => IrExpr::Repeat {
            elem: Box::new(convert_expr(&repeat.expr)),
            len: Box::new(convert_expr(&repeat.len)),
        },
        Expr::Block(block) => IrExpr::Block(convert_block(&block.block)),
        Expr::If(if_expr) => IrExpr::If {
            cond: Box::new(convert_expr(&if_expr.cond)),
            then_branch: convert_block(&if_expr.then_branch),
            else_branch: if_expr.else_branch.as_ref().map(|(_, e)| Box::new(convert_expr(e))),
        },
        Expr::Match(match_expr) => IrExpr::Match {
            expr: Box::new(convert_expr(&match_expr.expr)),
            arms: match_expr
                .arms
                .iter()
                .map(|arm| IrMatchArm {
                    pattern: convert_pattern(&arm.pat),
                    guard: arm.guard.as_ref().map(|(_, e)| convert_expr(e)),
                    body: convert_expr(&arm.body),
                })
                .collect(),
        },
        Expr::ForLoop(for_loop) => IrExpr::ForLoop {
            pattern: convert_pattern(&for_loop.pat),
            iter: Box::new(convert_expr(&for_loop.expr)),
            body: convert_block(&for_loop.body),
        },
        Expr::While(while_loop) => IrExpr::While {
            cond: Box::new(convert_expr(&while_loop.cond)),
            body: convert_block(&while_loop.body),
        },
        Expr::Loop(loop_expr) => IrExpr::Loop {
            body: convert_block(&loop_expr.body),
        },
        Expr::Break(break_expr) => IrExpr::Break(break_expr.expr.as_ref().map(|e| Box::new(convert_expr(e)))),
        Expr::Continue(_) => IrExpr::Continue,
        Expr::Return(ret) => IrExpr::Return(ret.expr.as_ref().map(|e| Box::new(convert_expr(e)))),
        Expr::Closure(closure) => IrExpr::Closure {
            params: closure
                .inputs
                .iter()
                .map(|p| IrClosureParam {
                    pattern: convert_pattern(p),
                    ty: None,
                })
                .collect(),
            ret_type: match &closure.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(Box::new(convert_type(ty))),
            },
            body: Box::new(convert_expr(&closure.body)),
        },
        Expr::Reference(reference) => IrExpr::Ref {
            mutable: reference.mutability.is_some(),
            expr: Box::new(convert_expr(&reference.expr)),
        },
        Expr::Cast(cast) => IrExpr::Cast {
            expr: Box::new(convert_expr(&cast.expr)),
            ty: Box::new(convert_type(&cast.ty)),
        },
        Expr::Range(range) => IrExpr::Range {
            start: range.start.as_ref().map(|e| Box::new(convert_expr(e))),
            end: range.end.as_ref().map(|e| Box::new(convert_expr(e))),
            inclusive: matches!(range.limits, syn::RangeLimits::Closed(_)),
        },
        Expr::Assign(assign) => IrExpr::Assign {
            left: Box::new(convert_expr(&assign.left)),
            right: Box::new(convert_expr(&assign.right)),
        },
        Expr::Try(try_expr) => IrExpr::Try(Box::new(convert_expr(&try_expr.expr))),
        Expr::Await(await_expr) => IrExpr::Await(Box::new(convert_expr(&await_expr.base))),
        Expr::Macro(mac) => IrExpr::Macro {
            path: mac.mac.path.segments.iter().map(|s| s.ident.to_string()).collect(),
            tokens: mac.mac.tokens.to_string(),
        },
        Expr::Paren(paren) => IrExpr::Paren(Box::new(convert_expr(&paren.expr))),
        Expr::Unsafe(unsafe_block) => IrExpr::Unsafe(convert_block(&unsafe_block.block)),
        Expr::Let(let_expr) => IrExpr::Let {
            pattern: convert_pattern(&let_expr.pat),
            expr: Box::new(convert_expr(&let_expr.expr)),
        },
        _ => IrExpr::Lit(IrLit::Int(0)),
    }
}

fn convert_lit_expr(lit: &Lit) -> IrLit {
    match lit {
        Lit::Int(int) => IrLit::Int(int.base10_parse().unwrap_or(0)),
        Lit::Float(float) => IrLit::Float(float.base10_parse().unwrap_or(0.0)),
        Lit::Bool(b) => IrLit::Bool(b.value),
        Lit::Char(c) => IrLit::Char(c.value()),
        Lit::Str(s) => IrLit::Str(s.value()),
        Lit::ByteStr(bs) => IrLit::ByteStr(bs.value()),
        Lit::Byte(b) => IrLit::Byte(b.value()),
        _ => IrLit::Int(0),
    }
}

fn convert_bin_op(op: &BinOp) -> IrBinOp {
    match op {
        BinOp::Add(_) | BinOp::AddAssign(_) => IrBinOp::Add,
        BinOp::Sub(_) | BinOp::SubAssign(_) => IrBinOp::Sub,
        BinOp::Mul(_) | BinOp::MulAssign(_) => IrBinOp::Mul,
        BinOp::Div(_) | BinOp::DivAssign(_) => IrBinOp::Div,
        BinOp::Rem(_) | BinOp::RemAssign(_) => IrBinOp::Rem,
        BinOp::BitAnd(_) | BinOp::BitAndAssign(_) => IrBinOp::BitAnd,
        BinOp::BitOr(_) | BinOp::BitOrAssign(_) => IrBinOp::BitOr,
        BinOp::BitXor(_) | BinOp::BitXorAssign(_) => IrBinOp::BitXor,
        BinOp::Shl(_) | BinOp::ShlAssign(_) => IrBinOp::Shl,
        BinOp::Shr(_) | BinOp::ShrAssign(_) => IrBinOp::Shr,
        BinOp::Eq(_) => IrBinOp::Eq,
        BinOp::Ne(_) => IrBinOp::Ne,
        BinOp::Lt(_) => IrBinOp::Lt,
        BinOp::Le(_) => IrBinOp::Le,
        BinOp::Gt(_) => IrBinOp::Gt,
        BinOp::Ge(_) => IrBinOp::Ge,
        BinOp::And(_) => IrBinOp::And,
        BinOp::Or(_) => IrBinOp::Or,
        _ => IrBinOp::Add,
    }
}

fn convert_unary_op(op: &UnOp) -> IrUnaryOp {
    match op {
        UnOp::Neg(_) => IrUnaryOp::Neg,
        UnOp::Not(_) => IrUnaryOp::Not,
        UnOp::Deref(_) => IrUnaryOp::Deref,
        _ => IrUnaryOp::Not,
    }
}

fn pat_to_string(pat: &Pat) -> String {
    match pat {
        Pat::Ident(ident) => ident.ident.to_string(),
        Pat::Wild(_) => "_".to_string(),
        _ => "_".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_struct() {
        let source = r#"
            pub struct Delta<N: ArrayLength<T>, T> {
                pub delta: GenericArray<T, N>,
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        assert_eq!(module.structs.len(), 1);
        assert_eq!(module.structs[0].name, "Delta");
        assert_eq!(module.structs[0].generics.len(), 2);
    }

    #[test]
    fn test_parse_impl_block() {
        let source = r#"
            impl<N: ArrayLength<T>, T> Delta<N, T> {
                pub fn remap<M: ArrayLength<T>>(&self, mut f: impl FnMut(usize) -> usize) -> Delta<M, T>
                where
                    T: Clone,
                {
                    todo!()
                }
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        assert_eq!(module.impls.len(), 1);
        assert_eq!(module.impls[0].items.len(), 1);
    }

    #[test]
    fn test_parse_trait_impl() {
        let source = r#"
            impl<N: ArrayLength<T>, T: Add<U>, U> Add<Vope<N, U>> for Vope<N, T> {
                type Output = Vope<N, T::Output>;
                fn add(self, rhs: Vope<N, U>) -> Self::Output {
                    todo!()
                }
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        assert_eq!(module.impls.len(), 1);
        assert!(module.impls[0].trait_.is_some());
    }
}
