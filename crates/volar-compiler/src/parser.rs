//! Parser module for converting Rust source code directly into specialized IR.

use syn::{
    parse_file, Expr, FnArg, GenericParam, Item, Pat, ReturnType, Type, Visibility,
};
use crate::ir::*;

pub fn parse_sources(sources: &[(&str, &str)], module_name: &str) -> Result<IrModule, String> {
    let mut module = IrModule {
        name: module_name.to_string(),
        ..Default::default()
    };

    for (source, _sub_name) in sources {
        let file = parse_file(source).map_err(|e| format!("Failed to parse: {}", e))?;
        for item in &file.items {
            match item {
                Item::Struct(s) => module.structs.push(convert_struct(s)),
                Item::Trait(t) => module.traits.push(convert_trait(t)),
                Item::Impl(i) => module.impls.push(convert_impl(i)),
                Item::Fn(f) => module.functions.push(convert_function(f)),
                Item::Type(t) => module.type_aliases.push(convert_type_alias(t)),
                Item::Use(_) => { /* TODO: track uses if needed */ }
                _ => {}
            }
        }
    }

    Ok(module)
}

pub fn parse_source(source: &str, name: &str) -> Result<IrModule, String> {
    parse_sources(&[(source, name)], name)
}

fn convert_struct(s: &syn::ItemStruct) -> IrStruct {
    IrStruct {
        kind: StructKind::from_str(&s.ident.to_string()),
        generics: s.generics.params.iter().map(convert_generic_param).collect(),
        fields: match &s.fields {
            syn::Fields::Named(fields) => fields.named.iter().map(convert_field).collect(),
            syn::Fields::Unnamed(fields) => fields.unnamed.iter().map(convert_field).collect(),
            syn::Fields::Unit => Vec::new(),
        },
        is_tuple: matches!(s.fields, syn::Fields::Unnamed(_)),
    }
}

fn convert_field(f: &syn::Field) -> IrField {
    IrField {
        name: f.ident.as_ref().map(|i| i.to_string()).unwrap_or_default(),
        ty: convert_type(&f.ty),
        public: matches!(f.vis, Visibility::Public(_)),
    }
}

fn convert_trait(t: &syn::ItemTrait) -> IrTrait {
    IrTrait {
        kind: TraitKind::from_path(&[t.ident.to_string()]),
        generics: t.generics.params.iter().map(convert_generic_param).collect(),
        super_traits: t.supertraits.iter().filter_map(|bound| {
            if let syn::TypeParamBound::Trait(tb) = bound {
                Some(convert_trait_bound(tb))
            } else {
                None
            }
        }).collect(),
        items: t.items.iter().filter_map(convert_trait_item).collect(),
    }
}

fn convert_trait_item(item: &syn::TraitItem) -> Option<IrTraitItem> {
    match item {
        syn::TraitItem::Fn(m) => Some(IrTraitItem::Method(IrMethodSig {
            name: m.sig.ident.to_string(),
            generics: m.sig.generics.params.iter().map(convert_generic_param).collect(),
            receiver: m.sig.receiver().map(convert_receiver),
            params: m.sig.inputs.iter().filter_map(|arg| {
                if let FnArg::Typed(pt) = arg {
                    Some(IrParam {
                        name: extract_pat_name(&pt.pat),
                        ty: convert_type(&pt.ty),
                    })
                } else {
                    None
                }
            }).collect(),
            return_type: match &m.sig.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(convert_type(ty)),
            },
            where_clause: m.sig.generics.where_clause.as_ref()
                .map(|wc| wc.predicates.iter().map(convert_where_predicate).collect())
                .unwrap_or_default(),
        })),
        syn::TraitItem::Type(ty) => Some(IrTraitItem::AssociatedType {
            name: AssociatedType::from_str(&ty.ident.to_string()),
            bounds: ty.bounds.iter().filter_map(|bound| {
                if let syn::TypeParamBound::Trait(tb) = bound {
                    Some(convert_trait_bound(tb))
                } else {
                    None
                }
            }).collect(),
            default: ty.default.as_ref().map(|(_, t)| convert_type(t)),
        }),
        _ => None,
    }
}

fn convert_impl(i: &syn::ItemImpl) -> IrImpl {
    IrImpl {
        generics: i.generics.params.iter().map(convert_generic_param).collect(),
        trait_: i.trait_.as_ref().map(|(_, path, _)| IrTraitRef {
            kind: TraitKind::from_path(&path.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<_>>()),
            type_args: path.segments.last()
                .and_then(|s| {
                    if let syn::PathArguments::AngleBracketed(args) = &s.arguments {
                        Some(args.args.iter().filter_map(|arg| {
                            if let syn::GenericArgument::Type(ty) = arg {
                                Some(convert_type(ty))
                            } else {
                                None
                            }
                        }).collect())
                    } else {
                        None
                    }
                })
                .unwrap_or_default(),
        }),
        self_ty: convert_type(&i.self_ty),
        where_clause: i.generics.where_clause.as_ref()
            .map(|wc| wc.predicates.iter().map(convert_where_predicate).collect())
            .unwrap_or_default(),
        items: i.items.iter().filter_map(convert_impl_item).collect(),
    }
}

fn convert_impl_item(item: &syn::ImplItem) -> Option<IrImplItem> {
    match item {
        syn::ImplItem::Fn(m) => Some(IrImplItem::Method(IrFunction {
            name: m.sig.ident.to_string(),
            generics: m.sig.generics.params.iter().map(convert_generic_param).collect(),
            receiver: m.sig.receiver().map(convert_receiver),
            params: m.sig.inputs.iter().filter_map(|arg| {
                if let FnArg::Typed(pt) = arg {
                    Some(IrParam {
                        name: extract_pat_name(&pt.pat),
                        ty: convert_type(&pt.ty),
                    })
                } else {
                    None
                }
            }).collect(),
            return_type: match &m.sig.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(convert_type(ty)),
            },
            where_clause: m.sig.generics.where_clause.as_ref()
                .map(|wc| wc.predicates.iter().map(convert_where_predicate).collect())
                .unwrap_or_default(),
            body: convert_block(&m.block),
        })),
        syn::ImplItem::Type(ty) => Some(IrImplItem::AssociatedType {
            name: AssociatedType::from_str(&ty.ident.to_string()),
            ty: convert_type(&ty.ty),
        }),
        _ => None,
    }
}

fn convert_function(f: &syn::ItemFn) -> IrFunction {
    IrFunction {
        name: f.sig.ident.to_string(),
        generics: f.sig.generics.params.iter().map(convert_generic_param).collect(),
        receiver: None,
        params: f.sig.inputs.iter().filter_map(|arg| {
            if let FnArg::Typed(pt) = arg {
                Some(IrParam {
                    name: extract_pat_name(&pt.pat),
                    ty: convert_type(&pt.ty),
                })
            } else {
                None
            }
        }).collect(),
        return_type: match &f.sig.output {
            ReturnType::Default => None,
            ReturnType::Type(_, ty) => Some(convert_type(ty)),
        },
        where_clause: f.sig.generics.where_clause.as_ref()
            .map(|wc| wc.predicates.iter().map(convert_where_predicate).collect())
            .unwrap_or_default(),
        body: convert_block(&f.block),
    }
}

fn convert_type_alias(t: &syn::ItemType) -> IrTypeAlias {
    IrTypeAlias {
        name: t.ident.to_string(),
        generics: t.generics.params.iter().map(convert_generic_param).collect(),
        target: convert_type(&t.ty),
    }
}

fn convert_generic_param(p: &GenericParam) -> IrGenericParam {
    match p {
        GenericParam::Type(tp) => IrGenericParam {
            name: tp.ident.to_string(),
            bounds: tp.bounds.iter().filter_map(|bound| {
                if let syn::TypeParamBound::Trait(tb) = bound {
                    Some(convert_trait_bound(tb))
                } else {
                    None
                }
            }).collect(),
            default: tp.default.as_ref().map(convert_type),
        },
        _ => IrGenericParam { name: "_".to_string(), bounds: Vec::new(), default: None },
    }
}

fn convert_trait_bound(b: &syn::TraitBound) -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::from_path(&b.path.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<_>>()),
        type_args: b.path.segments.last()
            .and_then(|s| {
                if let syn::PathArguments::AngleBracketed(args) = &s.arguments {
                    Some(args.args.iter().filter_map(|arg| {
                        if let syn::GenericArgument::Type(ty) = arg {
                            Some(convert_type(ty))
                        } else {
                            None
                        }
                    }).collect())
                } else {
                    None
                }
            })
            .unwrap_or_default(),
        assoc_bindings: b.path.segments.last()
            .and_then(|s| {
                if let syn::PathArguments::AngleBracketed(args) = &s.arguments {
                    Some(args.args.iter().filter_map(|arg| {
                        if let syn::GenericArgument::AssocType(at) = arg {
                            Some((AssociatedType::from_str(&at.ident.to_string()), convert_type(&at.ty)))
                        } else {
                            None
                        }
                    }).collect())
                } else {
                    None
                }
            })
            .unwrap_or_default(),
    }
}

fn convert_where_predicate(p: &syn::WherePredicate) -> IrWherePredicate {
    match p {
        syn::WherePredicate::Type(pt) => IrWherePredicate::TypeBound {
            ty: convert_type(&pt.bounded_ty),
            bounds: pt.bounds.iter().filter_map(|bound| {
                if let syn::TypeParamBound::Trait(tb) = bound {
                    Some(convert_trait_bound(tb))
                } else {
                    None
                }
            }).collect(),
        },
        _ => IrWherePredicate::TypeBound { ty: IrType::Infer, bounds: Vec::new() },
    }
}

fn convert_receiver(r: &syn::Receiver) -> IrReceiver {
    if r.reference.is_some() {
        if r.mutability.is_some() { IrReceiver::RefMut } else { IrReceiver::Ref }
    } else {
        IrReceiver::Value
    }
}

fn convert_type(ty: &Type) -> IrType {
    match ty {
        Type::Path(p) => {
            let last = p.path.segments.last().unwrap();
            let name = last.ident.to_string();
            
            if let Some(prim) = PrimitiveType::from_str(&name) {
                return IrType::Primitive(prim);
            }
            
            if let Some(tn) = TypeNumConst::from_str(&name) {
                return IrType::Primitive(match tn {
                    _ => PrimitiveType::Usize, // Simplify typenum constants as usize
                });
            }
            
            let type_args = if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                args.args.iter().filter_map(|arg| {
                    if let syn::GenericArgument::Type(ty) = arg {
                        Some(convert_type(ty))
                    } else {
                        None
                    }
                }).collect()
            } else {
                Vec::new()
            };
            
            if name == "GenericArray" && type_args.len() >= 2 {
                return IrType::Array {
                    kind: ArrayKind::GenericArray,
                    elem: Box::new(type_args[0].clone()),
                    len: convert_array_length_from_type(&type_args[1]),
                };
            }
            
            if p.path.segments.len() == 1 && type_args.is_empty() {
                // Potential type param
                return IrType::TypeParam(name);
            }
            
            IrType::Struct {
                kind: StructKind::from_str(&name),
                type_args,
            }
        }
        Type::Reference(r) => IrType::Reference {
            mutable: r.mutability.is_some(),
            elem: Box::new(convert_type(&r.elem)),
        },
        Type::Slice(s) => IrType::Array {
            kind: ArrayKind::Slice,
            elem: Box::new(convert_type(&s.elem)),
            len: ArrayLength::Const(0), // Placeholder
        },
        Type::Array(a) => IrType::Array {
            kind: ArrayKind::FixedArray,
            elem: Box::new(convert_type(&a.elem)),
            len: convert_array_length_from_syn_expr(&a.len),
        },
        Type::Tuple(t) => {
            if t.elems.is_empty() { IrType::Unit }
            else { IrType::Tuple(t.elems.iter().map(convert_type).collect()) }
        }
        _ => IrType::Infer,
    }
}

fn convert_array_length_from_type(ty: &IrType) -> ArrayLength {
    match ty {
        IrType::Primitive(_) => ArrayLength::TypeNum(TypeNumConst::U8), // Simplified
        IrType::TypeParam(name) => ArrayLength::TypeParam(name.clone()),
        _ => ArrayLength::Const(0),
    }
}

fn convert_array_length_from_syn_expr(expr: &syn::Expr) -> ArrayLength {
    match expr {
        syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(n), .. }) => {
            ArrayLength::Const(n.base10_parse().unwrap_or(0))
        }
        _ => ArrayLength::Computed(Box::new(convert_expr(expr))),
    }
}

fn convert_expr(expr: &Expr) -> IrExpr {
    match expr {
        Expr::Lit(l) => IrExpr::Lit(convert_lit(&l.lit)),
        Expr::Path(p) => {
            let segments: Vec<String> = p.path.segments.iter().map(|s| s.ident.to_string()).collect();
            if segments.len() == 1 {
                IrExpr::Var(segments[0].clone())
            } else {
                IrExpr::Path { segments, type_args: Vec::new() }
            }
        }
        Expr::Binary(b) => IrExpr::Binary {
            op: convert_bin_op(b.op),
            left: Box::new(convert_expr(&b.left)),
            right: Box::new(convert_expr(&b.right)),
        },
        Expr::Unary(u) => IrExpr::Unary {
            op: convert_unary_op(u.op),
            expr: Box::new(convert_expr(&u.expr)),
        },
        Expr::Call(c) => convert_call(&c.func, &c.args.iter().collect::<Vec<_>>()),
        Expr::MethodCall(m) => convert_method_call(&m.receiver, &m.method.to_string(), &m.args.iter().collect::<Vec<_>>()),
        Expr::Field(f) => IrExpr::Field {
            base: Box::new(convert_expr(&f.base)),
            field: match &f.member {
                syn::Member::Named(i) => i.to_string(),
                syn::Member::Unnamed(i) => i.index.to_string(),
            },
        },
        Expr::Index(i) => IrExpr::Index {
            base: Box::new(convert_expr(&i.expr)),
            index: Box::new(convert_expr(&i.index)),
        },
        Expr::Block(b) => IrExpr::Block(convert_block(&b.block)),
        Expr::If(i) => IrExpr::If {
            cond: Box::new(convert_expr(&i.cond)),
            then_branch: convert_block(&i.then_branch),
            else_branch: i.else_branch.as_ref().map(|(_, e)| Box::new(convert_expr(e))),
        },
        Expr::ForLoop(f) => convert_for_loop(&f.pat, &f.expr, &f.body),
        Expr::While(_) | Expr::Loop(_) => IrExpr::Macro {
            name: "compile_error".to_string(),
            tokens: "\"Unbounded loops are not allowed\"".to_string(),
        },
        Expr::Match(m) => IrExpr::Match {
            expr: Box::new(convert_expr(&m.expr)),
            arms: m.arms.iter().map(convert_match_arm).collect(),
        },
        Expr::Closure(c) => IrExpr::Closure {
            params: c.inputs.iter().map(|p| IrClosureParam {
                pattern: convert_pattern(p),
                ty: None,
            }).collect(),
            ret_type: None,
            body: Box::new(convert_expr(&c.body)),
        },
        Expr::Reference(r) => IrExpr::Ref {
            mutable: r.mutability.is_some(),
            expr: Box::new(convert_expr(&r.expr)),
        },
        Expr::Cast(c) => IrExpr::Cast {
            expr: Box::new(convert_expr(&c.expr)),
            ty: Box::new(convert_type(&c.ty)),
        },
        Expr::Return(r) => IrExpr::Return(r.expr.as_ref().map(|e| Box::new(convert_expr(e)))),
        Expr::Break(b) => IrExpr::Break(b.expr.as_ref().map(|e| Box::new(convert_expr(e)))),
        Expr::Continue(_) => IrExpr::Continue,
        Expr::Assign(a) => IrExpr::Assign {
            left: Box::new(convert_expr(&a.left)),
            right: Box::new(convert_expr(&a.right)),
        },
        _ => IrExpr::Var("todo".to_string()),
    }
}

fn convert_call(func: &Expr, args: &[&Expr]) -> IrExpr {
    if let Expr::Path(p) = func {
        let path_str = p.path.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<_>>().join("::");
        if (path_str == "GenericArray::generate" || path_str.ends_with("::generate")) && args.len() == 1 {
            if let Expr::Closure(c) = args[0] {
                return IrExpr::ArrayGenerate {
                    elem_ty: None,
                    len: ArrayLength::TypeParam("N".to_string()),
                    index_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)),
                };
            }
        }
    }
    IrExpr::Call {
        func: Box::new(convert_expr(func)),
        args: args.iter().map(|a| convert_expr(a)).collect(),
    }
}

fn convert_method_call(receiver: &Expr, method: &str, args: &[&Expr]) -> IrExpr {
    match method {
        "map" if args.len() == 1 => {
            if let Expr::Closure(c) = args[0] {
                return IrExpr::ArrayMap {
                    array: Box::new(convert_expr(receiver)),
                    elem_var: extract_pat_name(&c.inputs[0]),
                    body: Box::new(convert_expr(&c.body)),
                };
            }
        }
        "zip" if args.len() == 2 => {
            if let Expr::Closure(c) = args[1] {
                return IrExpr::ArrayZip {
                    left: Box::new(convert_expr(receiver)),
                    right: Box::new(convert_expr(args[0])),
                    left_var: extract_pat_name(&c.inputs[0]),
                    right_var: extract_pat_name(&c.inputs[1]),
                    body: Box::new(convert_expr(&c.body)),
                };
            }
        }
        _ => {}
    }
    IrExpr::MethodCall {
        receiver: Box::new(convert_expr(receiver)),
        method: MethodKind::from_str(method),
        type_args: Vec::new(),
        args: args.iter().map(|a| convert_expr(a)).collect(),
    }
}

fn convert_for_loop(pat: &Pat, iter: &Expr, body: &syn::Block) -> IrExpr {
    if let Expr::Range(r) = iter {
        return IrExpr::BoundedLoop {
            var: extract_pat_name(pat),
            start: Box::new(r.start.as_ref().map(|e| convert_expr(e)).unwrap_or(IrExpr::Lit(IrLit::Int(0)))),
            end: Box::new(r.end.as_ref().map(|e| convert_expr(e)).unwrap_or(IrExpr::Lit(IrLit::Int(0)))),
            inclusive: matches!(r.limits, syn::RangeLimits::Closed(_)),
            body: convert_block(body),
        };
    }
    IrExpr::IterLoop {
        pattern: convert_pattern(pat),
        collection: Box::new(convert_expr(iter)),
        body: convert_block(body),
    }
}

fn convert_block(block: &syn::Block) -> IrBlock {
    let mut stmts = Vec::new();
    let mut expr = None;
    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last = i == block.stmts.len() - 1;
        match stmt {
            syn::Stmt::Local(l) => stmts.push(IrStmt::Let {
                pattern: convert_pattern(&l.pat),
                ty: None,
                init: l.init.as_ref().map(|init| convert_expr(&init.expr)),
            }),
            syn::Stmt::Expr(e, semi) => {
                if is_last && semi.is_none() { expr = Some(Box::new(convert_expr(e))); }
                else { stmts.push(IrStmt::Semi(convert_expr(e))); }
            }
            _ => {}
        }
    }
    IrBlock { stmts, expr }
}

fn convert_match_arm(arm: &syn::Arm) -> IrMatchArm {
    IrMatchArm {
        pattern: convert_pattern(&arm.pat),
        guard: arm.guard.as_ref().map(|(_, e)| convert_expr(e)),
        body: convert_expr(&arm.body),
    }
}

fn convert_pattern(pat: &Pat) -> IrPattern {
    match pat {
        Pat::Ident(pi) => IrPattern::Ident {
            mutable: pi.mutability.is_some(),
            name: pi.ident.to_string(),
            subpat: pi.subpat.as_ref().map(|(_, p)| Box::new(convert_pattern(p))),
        },
        Pat::Wild(_) => IrPattern::Wild,
        _ => IrPattern::Wild,
    }
}

fn extract_pat_name(pat: &Pat) -> String {
    if let Pat::Ident(pi) = pat { pi.ident.to_string() } else { "_".to_string() }
}

fn convert_lit(lit: &syn::Lit) -> IrLit {
    match lit {
        syn::Lit::Int(n) => IrLit::Int(n.base10_parse().unwrap_or(0)),
        syn::Lit::Bool(b) => IrLit::Bool(b.value),
        syn::Lit::Str(s) => IrLit::Str(s.value()),
        _ => IrLit::Int(0),
    }
}

fn convert_bin_op(op: syn::BinOp) -> SpecBinOp {
    match op {
        syn::BinOp::Add(_) => SpecBinOp::Add,
        syn::BinOp::Sub(_) => SpecBinOp::Sub,
        syn::BinOp::Mul(_) => SpecBinOp::Mul,
        syn::BinOp::Div(_) => SpecBinOp::Div,
        syn::BinOp::BitXor(_) => SpecBinOp::BitXor,
        syn::BinOp::Eq(_) => SpecBinOp::Eq,
        syn::BinOp::And(_) => SpecBinOp::And,
        syn::BinOp::Or(_) => SpecBinOp::Or,
        _ => SpecBinOp::Add,
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
