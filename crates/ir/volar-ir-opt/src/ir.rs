// @reliability: experimental
// @ai: assisted
//! Constant-folding pass for Volar IR (`IRBlocks`).

use alloc::{collections::BTreeMap, vec, vec::Vec};
use volar_ir::ir::{IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRTerminator, IRTypes, IRVarId};
use volar_ir_common::{Constant, Node, Stmt, TypeId};

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
pub fn fold_ir_blocks<P: Clone>(blocks: &mut IRBlocks<P>, types: &IRTypes) -> bool {
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

/// Remove statements whose result is never referenced by anything live
/// (a later statement, or the terminator), per block.
///
/// Sound at per-block granularity: Volar IR blocks are self-contained --
/// nothing outside a block can reference one of its statements except via
/// the block's own declared params (never removed here), so a statement
/// unreferenced within its own block is unreferenced, period.
///
/// Side-effecting statements are never removed regardless of whether
/// their result is used, per `Stmt`'s own documented semantics:
/// `StorageWrite` and `ActionCall` always; every `ActionOutput` is kept
/// alive as long as its own `ActionCall` is (which is unconditional, so
/// transitively every `ActionOutput` is too). `OracleCall`/`OracleOutput`
/// and `Rng` are ordinary DCE candidates (pure, or "may be DCE'd only
/// when demonstrably unused" for `Rng`) -- no special-casing needed
/// beyond normal liveness, since `OracleOutput`'s own `call` operand
/// naturally keeps a still-referenced `OracleCall` alive.
///
/// Returns `true` if any block was modified.
pub fn dce_ir_blocks<P: Clone>(blocks: &mut IRBlocks<P>, _types: &IRTypes) -> bool {
    let mut any_changed = false;
    for block in blocks.blocks.iter_mut() {
        if dce_ir_block_once(block).0 {
            any_changed = true;
        }
    }
    any_changed
}

/// Like [`dce_ir_blocks`], but also returns each block's own cumulative
/// var-id remap (old `IRVarId.0` -> new `IRVarId.0`; identity for a block
/// DCE left untouched) — lets a caller holding external var-id-based
/// metadata computed against the *pre*-DCE block (e.g. movfuscation's own
/// [`MovfuscBlockBoundary`]/[`MovfuscAccumInfo`], neither of which live in
/// this crate) translate that metadata to stay valid post-DCE, instead of
/// it silently going stale. `fold_ir_blocks`/`store_forward_ir_blocks`
/// need no equivalent: both only rewrite statements in place (constant
/// folding) or redirect operand references via an alias map (store
/// forwarding) — neither ever changes a statement's own index or a
/// block's own `stmts.len()`, so var ids they touch are stable by
/// construction.
pub fn dce_ir_blocks_with_remap<P: Clone>(
    blocks: &mut IRBlocks<P>,
    _types: &IRTypes,
) -> (bool, Vec<BTreeMap<u32, u32>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap) = dce_ir_block_once(block);
        any_changed |= changed;
        remaps.push(remap);
    }
    (any_changed, remaps)
}

fn collect_terminator_vars(term: &IRTerminator) -> Vec<IRVarId> {
    let mut out = Vec::new();
    let _ = term.clone().map(&mut out, |acc: &mut Vec<IRVarId>, v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
        acc.push(v);
        Ok(v)
    });
    out
}

fn collect_stmt_vars(stmt: &volar_ir::ir::IRStmt) -> Vec<IRVarId> {
    let mut out = Vec::new();
    let _ = stmt.clone().map_var(
        &mut out,
        &mut |acc: &mut Vec<IRVarId>, v: IRVarId| -> Result<IRVarId, core::convert::Infallible> { acc.push(v); Ok(v) },
        &mut |_, ty| Ok(ty),
        &mut |_, s| Ok(s),
    );
    out
}

/// Returns `(changed, remap)` — `remap` maps every pre-call `IRVarId.0` to
/// its post-call `IRVarId.0` (identity for every id when `changed` is
/// `false`).
fn dce_ir_block_once<P: Clone>(block: &mut IRBlock<P>) -> (bool, BTreeMap<u32, u32>) {
    let n_params = block.params.len();
    let n_stmts = block.stmts.len();
    let mut must_keep = vec![false; n_stmts];
    for i in 0..n_stmts {
        if matches!(&block.stmts[i].kind, Stmt::StorageWrite { .. } | Stmt::ActionCall { .. }) {
            must_keep[i] = true;
        }
    }
    for i in 0..n_stmts {
        if let Stmt::ActionOutput { call, .. } = &block.stmts[i].kind {
            if (call.0 as usize) >= n_params {
                let call_idx = call.0 as usize - n_params;
                if must_keep.get(call_idx).copied().unwrap_or(false) {
                    must_keep[i] = true;
                }
            }
        }
    }

    let mut live = must_keep.clone();
    for v in collect_terminator_vars(&block.terminator) {
        if (v.0 as usize) >= n_params {
            let idx = v.0 as usize - n_params;
            if idx < n_stmts {
                live[idx] = true;
            }
        }
    }
    for i in (0..n_stmts).rev() {
        if live[i] {
            for v in collect_stmt_vars(&block.stmts[i].kind) {
                if (v.0 as usize) >= n_params {
                    let oidx = v.0 as usize - n_params;
                    if oidx < i {
                        live[oidx] = true;
                    }
                }
            }
        }
    }

    if live.iter().all(|&l| l) {
        let identity: BTreeMap<u32, u32> = (0..(n_params + n_stmts) as u32).map(|v| (v, v)).collect();
        return (false, identity);
    }

    let mut remap: BTreeMap<u32, u32> = BTreeMap::new();
    for p in 0..n_params {
        remap.insert(p as u32, p as u32);
    }
    let mut new_idx = n_params as u32;
    for i in 0..n_stmts {
        if live[i] {
            remap.insert((n_params + i) as u32, new_idx);
            new_idx += 1;
        }
    }
    let remap_var = |v: IRVarId| -> IRVarId {
        IRVarId(*remap.get(&v.0).unwrap_or_else(|| panic!(
            "dce_ir_block_once: var {} referenced by a live statement/terminator but not itself live -- \
             violates the invariant that operands are always defined earlier in the same block", v.0,
        )))
    };

    let mut new_stmts = Vec::with_capacity(new_idx as usize - n_params);
    for i in 0..n_stmts {
        if live[i] {
            let node = block.stmts[i].clone();
            let new_kind = node.kind.clone().map_var(
                &mut (),
                &mut |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> { Ok(remap_var(v)) },
                &mut |_, ty| Ok(ty),
                &mut |_, s| Ok(s),
            ).unwrap();
            new_stmts.push(Node { kind: new_kind, ..node });
        }
    }
    let new_term = block.terminator.clone().map(
        &mut (),
        |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> { Ok(remap_var(v)) },
    ).unwrap();

    block.stmts = new_stmts;
    block.terminator = new_term;
    (true, remap)
}

// ============================================================================
// Internal helpers
// ============================================================================

/// One forward simplification pass over a single Volar IR block.
fn fold_ir_block_once<P: Clone>(block: &mut IRBlock<P>, types: &IRTypes) -> bool {
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
        if apply_aliases_to_stmt(&mut block.stmts[i].kind, &alias_map) {
            changed = true;
        }

        // Step 2: record output type.
        if let Some(ty) = stmt_output_type(&block.stmts[i].kind) {
            type_map.insert(rv, ty);
        }

        // Step 3: compute the action to take.
        let action = compute_action(rv, &block.stmts[i].kind, types, &const_map, &type_map);

        // Step 4: apply the action.
        match action {
            IrAction::RecordConst(c) => {
                const_map.insert(rv, c);
            }
            IrAction::FoldToConst(c, ty) => {
                block.stmts[i].kind = Stmt::Const(c, ty);
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
                    if let Stmt::Poly { coeffs, constant, .. } = &mut block.stmts[i].kind {
                        if fold_poly_in_place(ty, coeffs, constant, &const_map, &type_map, types) {
                            changed = true;
                        }
                    }
                }

                // Phase B: poly merging — substitute any singleton key that
                // refers to a previously seen Poly (with matching TypeId).
                {
                    if let Stmt::Poly { coeffs, constant, ty: poly_ty } = &mut block.stmts[i].kind {
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
                let replacement = match &block.stmts[i].kind {
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
                        block.stmts[i].kind = Stmt::Const(c, ty);
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
                        if let Stmt::Poly { coeffs, constant, ty: poly_ty } = &block.stmts[i].kind {
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
        IRTerminator::Jmp { target } => {
            changed |= apply_aliases_to_ir_target_id(&mut target.dest, alias_map);
            changed |= apply_aliases_to_args(&mut target.args, alias_map);
        }
        IRTerminator::JumpCond {
            condition,
            then_target,
            else_target,
        } => {
            let c = canon_alias(alias_map, *condition);
            if c != *condition { *condition = c; changed = true; }
            changed |= apply_aliases_to_ir_target_id(&mut then_target.dest, alias_map);
            changed |= apply_aliases_to_args(&mut then_target.args, alias_map);
            changed |= apply_aliases_to_ir_target_id(&mut else_target.dest, alias_map);
            changed |= apply_aliases_to_args(&mut else_target.args, alias_map);
        }
        IRTerminator::JumpTable { index, cases } => {
            let c = canon_alias(alias_map, *index);
            if c != *index { *index = c; changed = true; }
            for branch in cases.values_mut() {
                changed |= apply_aliases_to_ir_target_id(&mut branch.dest, alias_map);
                changed |= apply_aliases_to_args(&mut branch.args, alias_map);
            }
        }
        _ => {}
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
            then_target,
            else_target,
        } => {
            if let Some(&c) = const_map.get(condition) {
                let branch = if c.lo & 1 != 0 {
                    then_target.clone()
                } else {
                    else_target.clone()
                };
                *term = IRTerminator::Jmp { target: branch };
                return true;
            }
        }
        IRTerminator::JumpTable { index, cases } => {
            if let Some(&c) = const_map.get(index) {
                if let Some(branch) = cases.get(&c).cloned() {
                    *term = IRTerminator::Jmp { target: branch };
                    return true;
                }
            }
        }
        _ => {}
    }
    false
}

#[cfg(test)]
mod dce_tests {
    use super::*;
    use volar_ir::ir::{IRBlock, IRType, IRTypeId};
    use volar_ir_common::Type;

    fn bit() -> IRTypeId { IRTypeId(0) }
    fn types_with_bit() -> IRTypes {
        IRTypes(alloc::vec![IRType::Primitive(Type::Bit)])
    }

    #[test]
    fn dce_removes_genuinely_dead_stmt_and_renumbers_survivors() {
        // params: [p0: Bit]
        // stmts: [0]=Const(1,Bit) DEAD (never referenced),
        //        [1]=Const(0,Bit) live (used by terminator's Jmp arg)
        // terminator: Jmp(Return, [var 2])  -- var 2 = stmts[1], i.e. the live Const(0)
        let mut types = types_with_bit();
        let block = IRBlock {
            params: alloc::vec![bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Const(Constant { hi: 0, lo: 1 }, bit()), (), None),
                Node::new(Stmt::Const(Constant { hi: 0, lo: 0 }, bit()), (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(2)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let changed = dce_ir_blocks(&mut blocks, &mut types);
        assert!(changed, "the dead Const(1) statement must be removed");
        assert_eq!(blocks.blocks[0].stmts.len(), 1, "only the live Const(0) statement should remain");
        match &blocks.blocks[0].stmts[0].kind {
            Stmt::Const(c, _) => assert_eq!(c.lo, 0, "the surviving statement must be the live Const(0), not the dead Const(1)"),
            other => panic!("expected a Const stmt, got {other:?}"),
        }
        // Terminator's own var reference must be renumbered: stmts[1] moved to index 0,
        // so its var id shifts from 2 (params.len()=1 + stmt-index 1) to 1 (params.len()=1 + stmt-index 0).
        match &blocks.blocks[0].terminator {
            IRTerminator::Jmp { target } => assert_eq!(target.args, alloc::vec![IRVarId(1)], "terminator's own var reference must be renumbered after removal"),
            other => panic!("expected Jmp, got {other:?}"),
        }
    }

    #[test]
    fn dce_keeps_storage_write_even_though_unused() {
        // A StorageWrite's own "result" is never referenced by anything,
        // but the statement itself must survive (it's a side effect).
        let mut types = types_with_bit();
        let block = IRBlock {
            params: alloc::vec![bit(), bit()], // [addr, src]
            stmts: alloc::vec![
                Node::new(Stmt::StorageWrite {
                    storage: volar_ir_common::StorageId(0), src: IRVarId(1), ty: bit(), addr: IRVarId(0),
                }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let changed = dce_ir_blocks(&mut blocks, &mut types);
        assert!(!changed, "a StorageWrite must never be removed, even though its own result is unused");
        assert_eq!(blocks.blocks[0].stmts.len(), 1);
    }
}
