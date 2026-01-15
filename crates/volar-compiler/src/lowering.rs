//! Specialized analysis and transformation of the unified IR.

use crate::ir::*;
#[cfg(feature = "std")]
use std::{collections::{BTreeMap, BTreeSet}, string::{String, ToString}, vec::Vec, boxed::Box, format};

#[cfg(not(feature = "std"))]
use alloc::{collections::{BTreeMap, BTreeSet}, string::{String, ToString}, vec::Vec, boxed::Box, format};

/// Context for type resolution and analysis
#[derive(Debug, Clone, Default)]
pub struct TypeContext {
    pub structs: BTreeMap<String, IrStruct>,
    pub traits: BTreeMap<String, IrTrait>,
    pub trait_impls: Vec<IrImpl>,
    pub assoc_types: BTreeMap<(String, AssociatedType), IrType>,
}

impl TypeContext {
    pub fn from_module(module: &IrModule) -> Self {
        let mut ctx = Self::default();

        for s in &module.structs {
            ctx.structs.insert(s.kind.to_string(), s.clone());
        }

        for t in &module.traits {
            ctx.traits.insert(t.kind.to_string(), t.clone());
        }

        for imp in &module.impls {
            ctx.trait_impls.push(imp.clone());

            if let Some(_trait_ref) = &imp.trait_ {
                let _self_ty = type_to_string(&imp.self_ty);

                for item in &imp.items {
                    if let IrImplItem::AssociatedType { name, ty } = item {
                        ctx.assoc_types.insert((_self_ty.clone(), name.clone()), ty.clone());
                    }
                }
            }
        }

        ctx
    }

    /// Substitute type parameters with concrete types
    pub fn substitute(&self, ty: &IrType, mapping: &BTreeMap<String, IrType>) -> IrType {
        match ty {
            IrType::TypeParam(name) => {
                if let Some(concrete) = mapping.get(name) {
                    concrete.clone()
                } else {
                    ty.clone()
                }
            }
            IrType::Array { kind, elem, len } => IrType::Array {
                kind: *kind,
                elem: Box::new(self.substitute(elem, mapping)),
                len: len.clone(), // TODO: substitute computed len
            },
            IrType::Struct { kind, type_args } => IrType::Struct {
                kind: kind.clone(),
                type_args: type_args.iter().map(|t| self.substitute(t, mapping)).collect(),
            },
            IrType::Projection { base, assoc, trait_path, .. } => {
                let sub_base = self.substitute(base, mapping);
                let base_str = type_to_string(&sub_base);

                if let Some(resolved) = self.assoc_types.get(&(base_str, assoc.clone())) {
                    resolved.clone()
                } else {
                    IrType::Projection {
                        base: Box::new(sub_base),
                        trait_path: trait_path.clone(),
                        trait_args: Vec::new(),
                        assoc: assoc.clone(),
                    }
                }
            }
            IrType::Reference { mutable, elem } => IrType::Reference {
                mutable: *mutable,
                elem: Box::new(self.substitute(elem, mapping)),
            },
            IrType::Tuple(elems) => {
                IrType::Tuple(elems.iter().map(|t| self.substitute(t, mapping)).collect())
            }
            _ => ty.clone(),
        }
    }
}

/// Convert a type to a stable string representation
pub fn type_to_string(ty: &IrType) -> String {
    match ty {
        IrType::Primitive(p) => p.to_string(),
        IrType::Struct { kind, type_args } => {
            if type_args.is_empty() {
                kind.to_string()
            } else {
                format!(
                    "{}<{}>",
                    kind,
                    type_args
                        .iter()
                        .map(type_to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        IrType::TypeParam(name) => name.clone(),
        IrType::Array { kind: _, elem, len: _ } => format!("[{}; _]", type_to_string(elem)),
        IrType::Tuple(elems) => format!(
            "({})",
            elems
                .iter()
                .map(type_to_string)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        IrType::Unit => "()".to_string(),
        IrType::Reference { mutable, elem } => {
            format!("&{}{}", if *mutable { "mut " } else { "" }, type_to_string(elem))
        }
        IrType::Projection { base, assoc, .. } => {
            format!("<{} as _>::{:?}", type_to_string(base), assoc)
        }
        _ => "_".to_string(),
    }
}

/// Analysis of operator implementations
#[derive(Debug, Clone, Default)]
pub struct OperatorAnalysis {
    /// Maps (trait, self_ty, rhs_ty) -> output_ty
    pub binary_ops: BTreeMap<(String, String, String), IrType>,
}

impl OperatorAnalysis {
    pub fn from_module(module: &IrModule) -> Self {
        let mut analysis = Self::default();

        for imp in &module.impls {
            if let Some(trait_ref) = &imp.trait_ {
                let _self_ty = type_to_string(&imp.self_ty);

                // Handle binary ops from standard library traits
                let rhs_ty = if trait_ref.type_args.len() >= 1 {
                    type_to_string(&trait_ref.type_args[0])
                } else {
                    _self_ty.clone()
                };

                // Look for Output associated type
                for item in &imp.items {
                    if let IrImplItem::AssociatedType { name, ty } = item {
                        if matches!(name, AssociatedType::Output) {
                            let _trait_name = trait_ref.kind.to_string();
                            analysis.binary_ops.insert(
                                (_trait_name, _self_ty.clone(), rhs_ty.clone()),
                                ty.clone(),
                            );
                        }
                    }
                }
            }
        }

        analysis
    }
}

/// Utility for tracking used types to guide monomorphization or code gen
pub fn collect_type_refs(module: &IrModule) -> BTreeSet<String> {
    let mut refs = BTreeSet::new();
    for s in &module.structs {
        for field in &s.fields {
            collect_type_refs_in_type(&field.ty, &mut refs);
        }
    }
    for imp in &module.impls {
        collect_type_refs_in_type(&imp.self_ty, &mut refs);
        for item in &imp.items {
            if let IrImplItem::Method(f) = item {
                collect_type_refs_in_function(f, &mut refs);
            }
        }
    }
    refs
}

fn collect_type_refs_in_function(f: &IrFunction, refs: &mut BTreeSet<String>) {
    for param in &f.params {
        collect_type_refs_in_type(&param.ty, refs);
    }
    if let Some(ret) = &f.return_type {
        collect_type_refs_in_type(ret, refs);
    }
    collect_type_refs_in_block(&f.body, refs);
}

fn collect_type_refs_in_block(block: &IrBlock, refs: &mut BTreeSet<String>) {
    for stmt in &block.stmts {
        match stmt {
            IrStmt::Let { ty, init, .. } => {
                if let Some(t) = ty { collect_type_refs_in_type(t, refs); }
                if let Some(i) = init { collect_type_refs_in_expr(i, refs); }
            }
            IrStmt::Semi(e) | IrStmt::Expr(e) => collect_type_refs_in_expr(e, refs),
        }
    }
    if let Some(e) = &block.expr {
        collect_type_refs_in_expr(e, refs);
    }
}

fn collect_type_refs_in_expr(expr: &IrExpr, refs: &mut BTreeSet<String>) {
    match expr {
        IrExpr::Binary { left, right, .. } => {
            collect_type_refs_in_expr(left, refs);
            collect_type_refs_in_expr(right, refs);
        }
        IrExpr::MethodCall { receiver, args, .. } => {
            collect_type_refs_in_expr(receiver, refs);
            for arg in args { collect_type_refs_in_expr(arg, refs); }
        }
        IrExpr::Call { func, args } => {
            collect_type_refs_in_expr(func, refs);
            for arg in args { collect_type_refs_in_expr(arg, refs); }
        }
        IrExpr::Block(b) => collect_type_refs_in_block(b, refs),
        _ => {}
    }
}

fn collect_type_refs_in_type(ty: &IrType, refs: &mut BTreeSet<String>) {
    match ty {
        IrType::Struct { kind, type_args } => {
            refs.insert(kind.to_string());
            for arg in type_args { collect_type_refs_in_type(arg, refs); }
        }
        IrType::Array { elem, .. } => collect_type_refs_in_type(elem, refs),
        IrType::Reference { elem, .. } => collect_type_refs_in_type(elem, refs),
        IrType::Tuple(elems) => {
            for e in elems { collect_type_refs_in_type(e, refs); }
        }
        _ => {}
    }
}
