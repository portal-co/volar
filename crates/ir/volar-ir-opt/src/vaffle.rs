// @reliability: experimental
// @ai: assisted
//! Constant-folding pass for VAFFLE (`Module`).
//!
//! Iterates over every `FuncDecl::Body` in a [`vaffle::Module`] and folds
//! constant-valued computations in place.  Only `Value::Op` variants are
//! modified; `Param`, `Call`, `Output`, and pointer variants are left
//! untouched.
//!
//! # Conservative rules
//! - No alias propagation — only fold to constants.
//! - `AES8` / `Galois64` monomials inside `Poly` are skipped (see
//!   `common::fold_poly_in_place`).

use alloc::{collections::BTreeMap, vec::Vec};
use vaffle::{FuncBody, FuncDecl, Module, Terminator, Value, ValueId};
use volar_ir_common::{Constant, Stmt, TypeId, TypeTable};

use crate::common::{
    constant_is_zero, constant_or, constant_rol, constant_ror, constant_shl, fold_poly_in_place,
    mask_constant, merge_poly_into, stmt_output_type, type_bit_width,
};

// ============================================================================
// Public API
// ============================================================================

/// Simplify all function bodies in `module` in place until no further changes
/// occur.
///
/// Returns `true` if any function body was modified.
pub fn fold_vaffle_module(module: &mut Module) -> bool {
    // Split the struct borrow into disjoint field borrows so that the borrow
    // checker accepts simultaneous immutable access to `types` and mutable
    // access to `funcs`.
    fold_vaffle_module_inner(&module.types, &mut module.funcs)
}

// ============================================================================
// Internal helpers
// ============================================================================

fn fold_vaffle_module_inner(
    types: &TypeTable,
    funcs: &mut alloc::vec::Vec<FuncDecl>,
) -> bool {
    let mut any_changed = false;
    for func in funcs.iter_mut() {
        if let FuncDecl::Body(body) = func {
            loop {
                if !fold_vaffle_body_once(body, types) {
                    break;
                }
                any_changed = true;
            }
        }
    }
    any_changed
}

/// One forward simplification pass over all values in a VAFFLE function body.
///
/// VAFFLE uses global `ValueId` indices (not block-local), so a single
/// `const_map` and `type_map` are maintained for the whole body.  No alias
/// propagation is performed — only folds to `Stmt::Const`.
fn fold_vaffle_body_once(body: &mut FuncBody, types: &TypeTable) -> bool {
    let mut const_map: BTreeMap<ValueId, Constant> = BTreeMap::new();
    let mut type_map: BTreeMap<ValueId, TypeId> = BTreeMap::new();
    // poly_map: vid → (coeffs, constant, TypeId) for surviving Poly values.
    let mut poly_map: BTreeMap<ValueId, (BTreeMap<Vec<ValueId>, u8>, Constant, TypeId)> =
        BTreeMap::new();
    let mut changed = false;

    // Seed type_map from all Param values.
    for (i, val) in body.values.iter().enumerate() {
        if let Value::Param { ty, .. } = val {
            type_map.insert(ValueId(i), *ty);
        }
    }

    for i in 0..body.values.len() {
        let vid = ValueId(i);

        // Clone the stmt for analysis; this releases the immutable borrow of
        // `body.values[i]` before we potentially mutate it below.
        let stmt: Stmt<ValueId> = match &body.values[i] {
            Value::Op(s) => {
                if let Some(ty) = stmt_output_type(s) {
                    type_map.insert(vid, ty);
                }
                s.clone()
            }
            // Non-Op values are not foldable by this pass.
            _ => continue,
        };

        match &stmt {
            // ── Already a constant: just record it. ────────────────────────
            Stmt::Const(c, _) => {
                const_map.insert(vid, *c);
            }

            // ── Polynomial: attempt in-place folding + merging. ────────────
            Stmt::Poly { ty, coeffs, constant } => {
                let has_foldable = coeffs
                    .iter()
                    .any(|(k, _)| k.iter().any(|v| const_map.contains_key(v)));
                let can_mask = type_bit_width(*ty, types).is_some();

                // Check if any singleton key refers to a known Poly.
                let has_mergeable = coeffs.iter().any(|(key, &coeff)| {
                    coeff & 1 != 0
                        && key.len() == 1
                        && poly_map
                            .get(&key[0])
                            .map_or(false, |(_, _, src_ty)| *src_ty == *ty)
                });

                if has_foldable || can_mask || has_mergeable {
                    // Phase A: fold in-place.
                    if let Value::Op(Stmt::Poly {
                        ty: poly_ty,
                        coeffs: c,
                        constant: k,
                    }) = &mut body.values[i]
                    {
                        let ty_copy = *poly_ty;
                        if fold_poly_in_place(ty_copy, c, k, &const_map, &type_map, types) {
                            changed = true;
                        }
                    }

                    // Phase B: poly merging.
                    if let Value::Op(Stmt::Poly {
                        ty: poly_ty,
                        coeffs: c,
                        constant: k,
                    }) = &mut body.values[i]
                    {
                        let poly_ty_val = *poly_ty;
                        let singleton_srcs: Vec<ValueId> = c
                            .iter()
                            .filter_map(|(key, &coeff)| {
                                if coeff & 1 != 0 && key.len() == 1 {
                                    let v = key[0];
                                    if let Some((_, _, src_ty)) = poly_map.get(&v) {
                                        if *src_ty == poly_ty_val {
                                            return Some(v);
                                        }
                                    }
                                }
                                None
                            })
                            .collect();

                        for src_var in singleton_srcs {
                            if let Some((src_coeffs, src_const, _)) = poly_map.get(&src_var) {
                                let src_coeffs = src_coeffs.clone();
                                let src_const = *src_const;
                                if merge_poly_into(c, k, &src_var, &src_coeffs, src_const) {
                                    changed = true;
                                }
                            }
                        }

                        // Re-fold after merging.
                        if changed {
                            fold_poly_in_place(poly_ty_val, c, k, &const_map, &type_map, types);
                        }
                    }

                    // Check whether poly collapsed to a constant.
                    let collapsed = match &body.values[i] {
                        Value::Op(Stmt::Poly {
                            coeffs: c,
                            constant: k,
                            ty: t,
                        }) if c.is_empty() => Some((*k, *t)),
                        _ => None,
                    };
                    if let Some((c, ty)) = collapsed {
                        body.values[i] = Value::Op(Stmt::Const(c, ty));
                        const_map.insert(vid, c);
                        changed = true;
                    } else {
                        // Record surviving Poly for downstream merging.
                        if let Value::Op(Stmt::Poly { coeffs: c, constant: k, ty: t }) =
                            &body.values[i]
                        {
                            poly_map.insert(vid, (c.clone(), *k, *t));
                        }
                    }
                } else if coeffs.is_empty() {
                    // Empty poly with no folding opportunity → constant directly.
                    body.values[i] = Value::Op(Stmt::Const(*constant, *ty));
                    const_map.insert(vid, *constant);
                    changed = true;
                } else {
                    // No folding possible — record as-is for downstream merging.
                    poly_map.insert(vid, (coeffs.clone(), *constant, *ty));
                }
            }

            // ── Rotate left: fold if source is constant. ───────────────────
            Stmt::Rol { src, ty, n } => {
                if let Some(&c) = const_map.get(src) {
                    if let Some(w) = type_bit_width(*ty, types) {
                        let result = mask_constant(constant_rol(c, w, *n), w);
                        body.values[i] = Value::Op(Stmt::Const(result, *ty));
                        const_map.insert(vid, result);
                        changed = true;
                    }
                }
            }

            // ── Rotate right: fold if source is constant. ──────────────────
            Stmt::Ror { src, ty, n } => {
                if let Some(&c) = const_map.get(src) {
                    if let Some(w) = type_bit_width(*ty, types) {
                        let result = mask_constant(constant_ror(c, w, *n), w);
                        body.values[i] = Value::Op(Stmt::Const(result, *ty));
                        const_map.insert(vid, result);
                        changed = true;
                    }
                }
            }

            // ── Splat: broadcast LSB across all bits. ─────────────────────
            Stmt::Splat { src, ty } => {
                if let Some(&c) = const_map.get(src) {
                    if let Some(w) = type_bit_width(*ty, types) {
                        let result = if c.lo & 1 != 0 {
                            mask_constant(Constant { hi: u128::MAX, lo: u128::MAX }, w)
                        } else {
                            Constant { hi: 0, lo: 0 }
                        };
                        body.values[i] = Value::Op(Stmt::Const(result, *ty));
                        const_map.insert(vid, result);
                        changed = true;
                    }
                }
            }

            // ── Transmute: bit-reinterpret, just mask to dst width. ────────
            Stmt::Transmute { src, dst_ty, .. } => {
                if let Some(&c) = const_map.get(src) {
                    if let Some(dst_w) = type_bit_width(*dst_ty, types) {
                        let result = mask_constant(c, dst_w);
                        body.values[i] = Value::Op(Stmt::Const(result, *dst_ty));
                        const_map.insert(vid, result);
                        changed = true;
                    }
                }
            }

            // ── Merge: fold only when ALL parts are known constants. ────────
            Stmt::Merge { parts, ty } => {
                if parts.iter().all(|v| const_map.contains_key(v)) {
                    if let Some(total_w) = type_bit_width(*ty, types) {
                        let mut result = Constant { hi: 0, lo: 0 };
                        let mut offset = 0usize;
                        for v in parts {
                            let part_c = *const_map.get(v).unwrap();
                            let part_w = type_map
                                .get(v)
                                .and_then(|&tid| type_bit_width(tid, types))
                                .unwrap_or(1);
                            let shifted =
                                constant_shl(mask_constant(part_c, part_w), offset);
                            result = constant_or(result, shifted);
                            offset += part_w;
                            if offset >= total_w {
                                break;
                            }
                        }
                        let result = mask_constant(result, total_w);
                        body.values[i] = Value::Op(Stmt::Const(result, *ty));
                        const_map.insert(vid, result);
                        changed = true;
                    }
                }
            }

            // Everything else is not foldable by this pass.
            _ => {}
        }
    }

    // Dead branch removal: fold IfNonzero / Table when condition is known.
    for block in body.blocks.iter_mut() {
        changed |= fold_vaffle_terminator_dead_branch(&mut block.terminator, &const_map);
    }

    changed
}

// ============================================================================
// Dead branch removal
// ============================================================================

fn fold_vaffle_terminator_dead_branch(
    term: &mut Terminator,
    const_map: &BTreeMap<ValueId, Constant>,
) -> bool {
    match term {
        Terminator::IfNonzero { cond, then_target, else_target } => {
            if let Some(&c) = const_map.get(cond) {
                let tgt = if c.lo & 1 != 0 { then_target } else { else_target };
                let new_target = vaffle::Target {
                    block: tgt.block,
                    args: tgt.args.clone(),
                };
                *term = Terminator::Jump(new_target);
                return true;
            }
        }
        Terminator::Table { index, targets, default_target } => {
            if let Some(&c) = const_map.get(index) {
                let idx = c.lo as usize;
                let tgt = if idx < targets.len() { &targets[idx] } else { default_target };
                let new_target = vaffle::Target {
                    block: tgt.block,
                    args: tgt.args.clone(),
                };
                *term = Terminator::Jump(new_target);
                return true;
            }
        }
        _ => {}
    }
    false
}
