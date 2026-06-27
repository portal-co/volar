// @reliability: experimental
// @ai: assisted
//! Constant-folding pass for Volar IR (`IRBlocks`).

use alloc::{collections::BTreeMap, vec::Vec};
use volar_ir::ir::{IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRTypes, IRVarId};
use volar_ir_common::{Constant, Stmt, TypeId};

use crate::common::{
    apply_aliases_to_stmt, canon_alias, constant_is_zero, constant_rol, constant_ror, fold_poly_in_place, mask_constant, merge_poly_into, stmt_output_type,
    type_bit_width,
};

// ============================================================================
// Public API
// ============================================================================

/// Simplify each block of `blocks` in place until no further changes occur.
///
/// Returns `true` if any block was modified.
pub fn fold_ir_blocks<P: Clone + Default>(blocks: &mut IRBlocks<P>, types: &IRTypes) -> bool {
    let mut any_changed = false;
    for block in blocks.blocks.iter_mut() {
        loop {
            if !fold_ir_block_once(block, types) {
                break;
            }
            any_changed = true;
        }
    }
    any_changed
}

// ============================================================================
// Internal helpers
// ============================================================================

/// One forward simplification pass over a single Volar IR block.
fn fold_ir_block_once<P: Clone + Default>(block: &mut IRBlock<P>, types: &IRTypes) -> bool {
    let mut const_map: BTreeMap<IRVarId, Constant> = BTreeMap::new();
    let mut type_map: BTreeMap<IRVarId, TypeId> = BTreeMap::new();
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    // poly_map: var → (coeffs, constant, TypeId) for surviving Poly stmts.
    let mut poly_map: BTreeMap<IRVarId, (BTreeMap<Vec<IRVarId>, u8>, Constant, TypeId)> =
        BTreeMap::new();
    let mut changed = false;

    // Seed type_map from block params.
    for (idx, &tid) in block.params.iter().enumerate() {
        type_map.insert(IRVarId(idx as u32), tid);
    }

    let base = block.params.len() as u32;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        // Step 1: apply alias substitutions to this stmt's operands.
        if apply_aliases_to_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        // Step 2: record output type.
        if let Some(ty) = stmt_output_type(&block.stmts[i]) {
            type_map.insert(rv, ty);
        }

        // Step 3: compute the action to take.
        let action = compute_action(rv, &block.stmts[i], types, &const_map, &type_map);

        // Step 4: apply the action.
        match action {
            IrAction::RecordConst(c) => {
                const_map.insert(rv, c);
            }
            IrAction::FoldToConst(c, ty) => {
                block.stmts[i] = Stmt::Const(c, ty);
                const_map.insert(rv, c);
                changed = true;
            }
            IrAction::FoldToAlias(v) => {
                // Record alias for downstream use. Don't change the stmt so
                // semantics are preserved across passes.
                alias_map.insert(rv, v);
                if let Some(&c) = const_map.get(&v) {
                    const_map.insert(rv, c);
                }
                // changed is set when downstream operands are rewritten.
            }
            IrAction::FoldPoly => {
                // Phase A: fold in-place.
                let ty = type_map.get(&rv).copied().unwrap_or(TypeId(0));
                {
                    if let Stmt::Poly { coeffs, constant, .. } = &mut block.stmts[i] {
                        if fold_poly_in_place(ty, coeffs, constant, &const_map, &type_map, types) {
                            changed = true;
                        }
                    }
                }

                // Phase B: poly merging — substitute any singleton key that
                // refers to a previously seen Poly (with matching TypeId).
                {
                    if let Stmt::Poly { coeffs, constant, ty: poly_ty } = &mut block.stmts[i] {
                        let poly_ty_val = *poly_ty;
                        let singleton_srcs: Vec<IRVarId> = coeffs
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
                                if merge_poly_into(coeffs, constant, &src_var, &src_coeffs, src_const) {
                                    changed = true;
                                }
                            }
                        }

                        // Re-fold after merging.
                        if changed {
                            fold_poly_in_place(poly_ty_val, coeffs, constant, &const_map, &type_map, types);
                        }
                    }
                }

                // Phase C: if poly collapsed, convert to Const or record alias.
                let replacement = match &block.stmts[i] {
                    Stmt::Poly { coeffs, constant, ty: poly_ty } if coeffs.is_empty() => {
                        Some(IrPolyResult::Const(*constant, *poly_ty))
                    }
                    Stmt::Poly { coeffs, constant, .. }
                        if coeffs.len() == 1
                            && constant_is_zero(*constant)
                            && coeffs
                                .iter()
                                .next()
                                .map(|(k, &c)| k.len() == 1 && c & 1 != 0)
                                .unwrap_or(false) =>
                    {
                        let v = *coeffs.iter().next().unwrap().0.first().unwrap();
                        Some(IrPolyResult::Alias(v))
                    }
                    _ => None,
                };
                match replacement {
                    Some(IrPolyResult::Const(c, ty)) => {
                        block.stmts[i] = Stmt::Const(c, ty);
                        const_map.insert(rv, c);
                        changed = true;
                    }
                    Some(IrPolyResult::Alias(v)) => {
                        alias_map.insert(rv, v);
                        if let Some(&c) = const_map.get(&v) {
                            const_map.insert(rv, c);
                        }
                        // Don't change stmt; alias propagation handles uses.
                    }
                    None => {
                        // Record surviving Poly in poly_map for downstream merging.
                        if let Stmt::Poly { coeffs, constant, ty: poly_ty } = &block.stmts[i] {
                            poly_map.insert(rv, (coeffs.clone(), *constant, *poly_ty));
                        }
                    }
                }
            }
            IrAction::NoChange => {}
        }
    }

    // Rewrite the terminator through alias_map.
    changed |= apply_aliases_to_ir_terminator(&mut block.terminator, &alias_map);

    // Dead branch removal: fold JumpCond / JumpTable when condition is known.
    changed |= fold_ir_terminator_dead_branch(&mut block.terminator, &const_map);

    changed
}

// ============================================================================
// Action computation
// ============================================================================

enum IrAction {
    /// The stmt is already `Const(c)` — just record `c`.
    RecordConst(Constant),
    /// Replace this stmt with `Const(c, ty)`.
    FoldToConst(Constant, TypeId),
    /// Record an alias `rv → v` (stmt already computes the right value).
    FoldToAlias(IRVarId),
    /// Attempt in-place poly simplification.
    FoldPoly,
    /// Nothing to simplify.
    NoChange,
}

enum IrPolyResult {
    Const(Constant, TypeId),
    Alias(IRVarId),
}

fn compute_action(
    _rv: IRVarId,
    stmt: &Stmt<IRVarId, IRVarId>,
    types: &IRTypes,
    const_map: &BTreeMap<IRVarId, Constant>,
    type_map: &BTreeMap<IRVarId, TypeId>,
) -> IrAction {
    match stmt {
        Stmt::Const(c, _) => IrAction::RecordConst(*c),

        Stmt::Poly { coeffs, constant, ty } => {
            // Check if any var is in const_map or if the constant can be masked.
            let any_foldable = coeffs.iter().any(|(key, _)| {
                key.iter().any(|v| const_map.contains_key(v))
            });
            let can_mask = type_bit_width(*ty, types).is_some();
            if any_foldable || can_mask {
                IrAction::FoldPoly
            } else if coeffs.is_empty() {
                // Empty poly with no folding needed → Const.
                IrAction::FoldToConst(*constant, *ty)
            } else {
                IrAction::NoChange
            }
        }

        Stmt::Rol { src, ty, n } => {
            if let Some(&c) = const_map.get(src) {
                if let Some(w) = type_bit_width(*ty, types) {
                    let result = constant_rol(c, w, *n);
                    return IrAction::FoldToConst(result, *ty);
                }
            }
            IrAction::NoChange
        }

        Stmt::Ror { src, ty, n } => {
            if let Some(&c) = const_map.get(src) {
                if let Some(w) = type_bit_width(*ty, types) {
                    let result = constant_ror(c, w, *n);
                    return IrAction::FoldToConst(result, *ty);
                }
            }
            IrAction::NoChange
        }

        Stmt::Splat { src, ty } => {
            if let Some(&c) = const_map.get(src) {
                if let Some(w) = type_bit_width(*ty, types) {
                    // Splat: broadcast LSB of src across all `w` bits.
                    let bit = c.lo & 1;
                    let result = if bit != 0 {
                        mask_constant(Constant { hi: u128::MAX, lo: u128::MAX }, w)
                    } else {
                        Constant { hi: 0, lo: 0 }
                    };
                    return IrAction::FoldToConst(result, *ty);
                }
            }
            IrAction::NoChange
        }

        Stmt::Transmute { src, src_ty: _, dst_ty } => {
            if let Some(&c) = const_map.get(src) {
                // Transmute is a bit-reinterpretation; just mask to dst width.
                if let Some(dst_w) = type_bit_width(*dst_ty, types) {
                    let result = mask_constant(c, dst_w);
                    return IrAction::FoldToConst(result, *dst_ty);
                }
            }
            IrAction::NoChange
        }

        Stmt::Merge { parts, ty } => {
            // Fold only if ALL parts are known constants.
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
                        // Shift part into position.
                        let shifted = crate::common::constant_shl(
                            mask_constant(part_c, part_w),
                            offset,
                        );
                        result = crate::common::constant_or(result, shifted);
                        offset += part_w;
                        if offset >= total_w {
                            break;
                        }
                    }
                    return IrAction::FoldToConst(mask_constant(result, total_w), *ty);
                }
            }
            IrAction::NoChange
        }

        // Everything else is not foldable by this pass.
        _ => IrAction::NoChange,
    }
}

// ============================================================================
// Alias application to IRTerminator
// ============================================================================

fn apply_aliases_to_ir_target_id(
    target: &mut IRBlockTargetId,
    alias_map: &BTreeMap<IRVarId, IRVarId>,
) -> bool {
    if let IRBlockTargetId::Dyn(v) = target {
        let c = canon_alias(alias_map, *v);
        if c != *v {
            *v = c;
            return true;
        }
    }
    false
}

fn apply_aliases_to_args(
    args: &mut [IRVarId],
    alias_map: &BTreeMap<IRVarId, IRVarId>,
) -> bool {
    let mut changed = false;
    for v in args.iter_mut() {
        let c = canon_alias(alias_map, *v);
        if c != *v { *v = c; changed = true; }
    }
    changed
}

pub(crate) fn apply_aliases_to_ir_terminator(
    term: &mut IRTerminator,
    alias_map: &BTreeMap<IRVarId, IRVarId>,
) -> bool {
    if alias_map.is_empty() {
        return false;
    }
    let mut changed = false;
    match term {
        IRTerminator::Jmp { func, args } => {
            changed |= apply_aliases_to_ir_target_id(func, alias_map);
            changed |= apply_aliases_to_args(args, alias_map);
        }
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            let c = canon_alias(alias_map, *condition);
            if c != *condition { *condition = c; changed = true; }
            changed |= apply_aliases_to_ir_target_id(true_block, alias_map);
            changed |= apply_aliases_to_args(true_args, alias_map);
            changed |= apply_aliases_to_ir_target_id(false_block, alias_map);
            changed |= apply_aliases_to_args(false_args, alias_map);
        }
        IRTerminator::JumpTable { index, cases } => {
            let c = canon_alias(alias_map, *index);
            if c != *index { *index = c; changed = true; }
            for (_, (target, args)) in cases.iter_mut() {
                changed |= apply_aliases_to_ir_target_id(target, alias_map);
                changed |= apply_aliases_to_args(args, alias_map);
            }
        }
    }
    changed
}

// ============================================================================
// Dead branch removal
// ============================================================================

/// Fold `JumpCond` / `JumpTable` terminators when the condition is a known
/// constant.  Returns `true` if the terminator was replaced.
fn fold_ir_terminator_dead_branch(
    term: &mut IRTerminator,
    const_map: &BTreeMap<IRVarId, Constant>,
) -> bool {
    match term {
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            if let Some(&c) = const_map.get(condition) {
                let (tgt, args) = if c.lo & 1 != 0 {
                    (true_block.clone(), true_args.clone())
                } else {
                    (false_block.clone(), false_args.clone())
                };
                *term = IRTerminator::Jmp { func: tgt, args };
                return true;
            }
        }
        IRTerminator::JumpTable { index, cases } => {
            if let Some(&c) = const_map.get(index) {
                if let Some((target, args)) = cases.get(&c).cloned() {
                    *term = IRTerminator::Jmp { func: target, args };
                    return true;
                }
            }
        }
        _ => {}
    }
    false
}
