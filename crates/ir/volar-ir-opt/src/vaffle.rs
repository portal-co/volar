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

use alloc::collections::BTreeMap;
use vaffle::{Block, BlockId, FuncBody, FuncDecl, Module, SigId, Value, ValueId};
use volar_ir_common::{Constant, Stmt, TypeId, TypeTable};

use crate::common::{
    constant_or, constant_rol, constant_ror, constant_shl, fold_poly_in_place, mask_constant,
    stmt_output_type, type_bit_width,
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

            // ── Polynomial: attempt in-place folding. ──────────────────────
            Stmt::Poly { ty, coeffs, constant } => {
                let has_foldable = coeffs
                    .iter()
                    .any(|(k, _)| k.iter().any(|v| const_map.contains_key(v)));
                let can_mask = type_bit_width(*ty, types).is_some();

                if has_foldable || can_mask {
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
                    }
                } else if coeffs.is_empty() {
                    // Empty poly with no folding opportunity → constant directly.
                    body.values[i] = Value::Op(Stmt::Const(*constant, *ty));
                    const_map.insert(vid, *constant);
                    changed = true;
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

    changed
}
