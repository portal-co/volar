//! Constant analysis: classifies generics and associated types as type-level constants.
//!
//! This module provides reusable classification logic for determining whether
//! a generic parameter or trait associated type represents a type-level constant
//! (e.g., a `typenum` length). Extracted from `lowering_dyn` so it can be
//! consumed by multiple passes (dyn lowering, trait analysis, future backends)
//! without coupling them together.

use crate::ir::*;

#[cfg(feature = "std")]
use std::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use alloc::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};

// ============================================================================
// GENERIC KIND CLASSIFICATION
// ============================================================================

/// Classification of a generic parameter or associated type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericKind {
    /// Type-level constant (length/size) — becomes runtime usize in dyn lowering
    Length,
    /// Regular type parameter — remains generic
    Type,
}

/// Classify a generic parameter based on its bounds and context.
///
/// `all_params` provides surrounding generic parameter sets for recursive
/// name-based resolution (e.g., `K: ArrayLength<...>` referenced by another
/// param via `Mul<K>`).
pub fn classify_generic(param: &IrGenericParam, all_params: &[&[IrGenericParam]]) -> GenericKind {
    if param.kind == IrGenericParamKind::Const {
        return GenericKind::Length;
    }

    // Direct indicators in bounds
    for bound in &param.bounds {
        if is_length_bound(bound) {
            return GenericKind::Length;
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

    // Default: type parameter (includes crypto traits, custom traits, etc.)
    GenericKind::Type
}

// ============================================================================
// BOUND CLASSIFICATION PREDICATES
// ============================================================================

/// Returns true if the trait bound indicates a type-level length/size.
///
/// Recognises `ArrayLength` and `Unsigned`.
pub fn is_length_bound(bound: &IrTraitBound) -> bool {
    match &bound.trait_kind {
        TraitKind::Math(MathTrait::Unsigned) => true,
        TraitKind::Custom(name) => name == "ArrayLength",
        _ => false,
    }
}

/// Returns true if the trait bound indicates a type-level length/size,
/// also considering custom traits whose supertraits include a length trait.
///
/// `length_alias_traits` is the set of custom trait names known to be
/// length aliases (derived from module trait definitions).
pub fn is_length_bound_with_aliases(bound: &IrTraitBound, length_alias_traits: &[&str]) -> bool {
    if is_length_bound(bound) {
        return true;
    }
    if let TraitKind::Custom(name) = &bound.trait_kind {
        return length_alias_traits.contains(&name.as_str());
    }
    false
}

/// Returns true if the trait bound is an Fn-like trait (Fn, FnMut, FnOnce).
pub fn is_fn_bound(bound: &IrTraitBound) -> bool {
    matches!(&bound.trait_kind, TraitKind::Fn(_, _))
}

/// Returns true if the trait bound is a math operator trait (Add, Sub, Mul, Div).
pub fn is_math_op_bound(bound: &IrTraitBound) -> bool {
    matches!(
        &bound.trait_kind,
        TraitKind::Math(MathTrait::Add)
            | TraitKind::Math(MathTrait::Sub)
            | TraitKind::Math(MathTrait::Mul)
            | TraitKind::Math(MathTrait::Div)
    )
}

// ============================================================================
// NAME-BASED LENGTH RESOLUTION (recursive)
// ============================================================================

/// Look up a generic parameter by name across multiple parameter sets.
pub fn find_param<'a>(
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

/// Determine if a parameter name transitively refers to a length parameter.
fn is_length_name(name: &str, all_params: &[&[IrGenericParam]], visited: &mut Vec<String>) -> bool {
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

/// Determine if a type transitively refers to a length parameter.
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
        IrType::Array { elem, .. } | IrType::Vector { elem } | IrType::Reference { elem, .. } => {
            type_refers_to_length(elem, all_params, visited)
        }
        IrType::Tuple(elems) => elems
            .iter()
            .any(|e| type_refers_to_length(e, all_params, visited)),
        _ => false,
    }
}

// ============================================================================
// ASSOCIATED TYPE CLASSIFICATION
// ============================================================================

/// Classify a trait associated type's bounds to determine if it represents
/// a type-level constant.
///
/// An associated type bounded by `ArrayLength<T>`, a length-alias custom trait, or `Unsigned`
/// is a type-level constant (will become a runtime `usize` in dyn lowering,
/// or a compile-time integer in C/HDL backends).
pub fn classify_assoc_type_bounds(bounds: &[IrTraitBound]) -> GenericKind {
    for bound in bounds {
        if is_length_bound(bound) {
            return GenericKind::Length;
        }
    }
    GenericKind::Type
}

/// Like `classify_assoc_type_bounds`, but also considers custom length-alias traits.
pub fn classify_assoc_type_bounds_with_aliases(
    bounds: &[IrTraitBound],
    length_aliases: &[&str],
) -> GenericKind {
    for bound in bounds {
        if is_length_bound_with_aliases(bound, length_aliases) {
            return GenericKind::Length;
        }
    }
    GenericKind::Type
}

/// Like `classify_generic`, but also considers custom length-alias traits.
pub fn classify_generic_with_aliases(
    param: &IrGenericParam,
    all_params: &[&[IrGenericParam]],
    length_aliases: &[&str],
) -> GenericKind {
    if param.kind == IrGenericParamKind::Const {
        return GenericKind::Length;
    }

    for bound in &param.bounds {
        if is_length_bound_with_aliases(bound, length_aliases) {
            return GenericKind::Length;
        }
        if is_fn_bound(bound) {
            return GenericKind::Type;
        }
    }

    let mut visited = Vec::new();
    if is_length_name(&param.name, all_params, &mut visited) {
        return GenericKind::Length;
    }

    if !param.bounds.is_empty() {
        return GenericKind::Type;
    }

    GenericKind::Type
}

// ============================================================================
// CONST ANALYSIS (module-level results)
// ============================================================================

/// Results of constant analysis over an `IrModule`.
///
/// This is a pure analysis result struct — it does not own or modify the IR.
/// Kept separate from `TypeContext` (which holds type resolution state and
/// trait definitions) to avoid mixing concerns.
#[derive(Debug, Clone, Default)]
pub struct ConstAnalysis {
    /// For each struct: classification of each generic parameter.
    /// Key: struct name. Value: vec of (param_name, GenericKind, IrGenericParamKind).
    pub struct_generics: BTreeMap<String, Vec<(String, GenericKind, IrGenericParamKind)>>,

    /// For each trait associated type: its classification.
    /// Key: (trait_name, assoc_type_name). Value: GenericKind.
    /// `GenericKind::Length` means the associated type is a type-level constant.
    pub trait_assoc_kinds: BTreeMap<(String, String), GenericKind>,

    /// For each impl block's associated type assignment: classification inherited
    /// from the trait definition.
    /// Key: (self_ty_string, assoc_type_name). Value: GenericKind.
    pub impl_assoc_kinds: BTreeMap<(String, String), GenericKind>,

    /// Custom trait names that are length aliases (their supertraits include
    /// `ArrayLength`, `Unsigned`, or another length alias).
    pub length_alias_traits: Vec<String>,
}

impl ConstAnalysis {
    /// Run constant analysis over the entire module.
    pub fn from_module(module: &IrModule) -> Self {
        let mut result = Self::default();

        // Pass 0: discover length-alias custom traits.
        // A custom trait is a length alias if any of its supertraits is a
        // known length bound (ArrayLength, Unsigned) or another length alias.
        // We iterate to a fixed point to handle transitive chains.
        let mut changed = true;
        while changed {
            changed = false;
            for t in &module.traits {
                if let TraitKind::Custom(name) = &t.kind {
                    if result.length_alias_traits.contains(name) {
                        continue;
                    }
                    let aliases_ref: Vec<&str> =
                        result.length_alias_traits.iter().map(|s| s.as_str()).collect();
                    let is_alias = t.super_traits.iter().any(|st| {
                        is_length_bound_with_aliases(st, &aliases_ref)
                    });
                    if is_alias {
                        result.length_alias_traits.push(name.clone());
                        changed = true;
                    }
                }
            }
        }

        let aliases_ref: Vec<&str> =
            result.length_alias_traits.iter().map(|s| s.as_str()).collect();

        // Pass 1: classify struct generics
        for s in &module.structs {
            let classified: Vec<_> = s
                .generics
                .iter()
                .map(|p| {
                    let kind = if p.kind == IrGenericParamKind::Lifetime {
                        GenericKind::Type
                    } else {
                        classify_generic_with_aliases(p, &[&s.generics], &aliases_ref)
                    };
                    (p.name.clone(), kind, p.kind.clone())
                })
                .collect();
            result
                .struct_generics
                .insert(s.kind.to_string(), classified);
        }

        // Pass 2: classify trait associated types
        for t in &module.traits {
            let trait_name = t.kind.to_string();
            for item in &t.items {
                if let IrTraitItem::AssociatedType { name, bounds, .. } = item {
                    let assoc_name = format!("{:?}", name);
                    let kind = classify_assoc_type_bounds_with_aliases(bounds, &aliases_ref);
                    result
                        .trait_assoc_kinds
                        .insert((trait_name.clone(), assoc_name), kind);
                }
            }
        }

        // Pass 3: classify impl associated type assignments by inheriting from trait
        for imp in &module.impls {
            if let Some(trait_ref) = &imp.trait_ {
                let trait_name = trait_ref.kind.to_string();
                let self_ty_str = format!("{}", imp.self_ty);

                for item in &imp.items {
                    if let IrImplItem::AssociatedType { name, .. } = item {
                        let assoc_name = format!("{:?}", name);
                        // Inherit classification from the trait definition
                        let kind = result
                            .trait_assoc_kinds
                            .get(&(trait_name.clone(), assoc_name.clone()))
                            .cloned()
                            .unwrap_or(GenericKind::Type);
                        result
                            .impl_assoc_kinds
                            .insert((self_ty_str.clone(), assoc_name), kind);
                    }
                }
            }
        }

        result
    }

    /// Is this associated type a type-level constant in the given trait?
    pub fn is_type_level_const(&self, trait_name: &str, assoc_name: &str) -> bool {
        self.trait_assoc_kinds
            .get(&(trait_name.to_string(), assoc_name.to_string()))
            .map(|k| *k == GenericKind::Length)
            .unwrap_or(false)
    }

    /// Is this impl's associated type a type-level constant?
    pub fn is_impl_type_level_const(&self, self_ty: &str, assoc_name: &str) -> bool {
        self.impl_assoc_kinds
            .get(&(self_ty.to_string(), assoc_name.to_string()))
            .map(|k| *k == GenericKind::Length)
            .unwrap_or(false)
    }
}
