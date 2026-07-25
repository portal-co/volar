// @reliability: experimental
// @ai: assisted
//! Constant-folding pass for Volar IR (`IRBlocks`).

use alloc::{collections::{BTreeMap, BTreeSet}, vec, vec::Vec};
use volar_ir::ir::{IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRType, IRTypes, IRVarId};
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
        if dce_ir_block_once(block, &[]).0 {
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
        let (changed, remap) = dce_ir_block_once(block, &[]);
        any_changed |= changed;
        remaps.push(remap);
    }
    (any_changed, remaps)
}

// ============================================================================
// Poly batching
// ============================================================================

/// Batch-merge structurally-identical width-1 `Poly` statements that
/// differ in exactly one operand into a single wide `Poly`, so
/// `emit_poly_wide` (the weaver's own loop-collapsing optimization for
/// `width > 1` `Poly`s, `crates/compiler/volar-weaver/src/vole.rs`) can
/// handle the whole group as one statement instead of N. Each original
/// `Poly`'s own output var id is preserved (rewritten to a `Shuffle`
/// extracting its own lane from the new wide `Poly`), so nothing
/// downstream needs to change -- and that `Shuffle` weaves for free
/// (aliased directly to the source bit, no new statement) once the
/// source is a `WireRepr::Vec`/`Array` entry, per this session's own
/// `emit_shuffle` fix.
///
/// Motivation: the real RISC-V interpreter's own combined circuit has
/// 68,107 AND-bearing `Poly` statements, almost all already width=1
/// (nothing left for `emit_poly_wide` to collapse *within* one
/// statement) -- but many share the exact same shape (e.g.
/// movfuscation's own `is_active_i · touched_slot_k` accumulation
/// formula, repeated per `(block, slot)` pair, varying only in which
/// slot). This pass targets exactly that redundancy.
///
/// Deliberately conservative: only merges a group when there is an
/// EXACT, mechanically-verified single-variable substitution mapping one
/// member's own `coeffs` onto every other member's (never an
/// approximation, and never more than one differing variable) -- this
/// can only ever miss a real batching opportunity (safe, just less
/// optimal), never merge two structurally different `Poly`s. Designed to
/// run as a generic `IRBlocks` pass -- usable both *before* movfuscation
/// (per original block) and *after* (on the single combined block).
///
/// Requires `&mut IRTypes` (unlike `fold_ir_blocks`/`dce_ir_blocks`)
/// since merging needs to intern the new wide `Vec(width, Bit)` type.
///
/// Returns `true` if any block was modified.
pub fn batch_ir_blocks<P: Clone>(blocks: &mut IRBlocks<P>, types: &mut IRTypes) -> bool {
    let mut any_changed = false;
    for block in blocks.blocks.iter_mut() {
        if batch_ir_block_once(block, types) {
            any_changed = true;
        }
    }
    any_changed
}

/// Every distinct variable referenced anywhere in `coeffs`.
fn poly_vars(coeffs: &BTreeMap<Vec<IRVarId>, u8>) -> BTreeSet<IRVarId> {
    coeffs.keys().flatten().copied().collect()
}

/// Substitute every occurrence of `from` with `to` throughout `coeffs`,
/// re-sorting each monomial's own var list (required: `Stmt::Poly`'s own
/// doc mandates sorted monomial keys) and combining monomials that
/// collide after substitution via GF(2) coefficient XOR (dropping any
/// that cancel to an even coefficient) -- mirrors `merge_poly_into`'s own
/// GF(2) discipline elsewhere in this crate.
fn substitute_var(coeffs: &BTreeMap<Vec<IRVarId>, u8>, from: IRVarId, to: IRVarId) -> BTreeMap<Vec<IRVarId>, u8> {
    let mut out: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    for (mono, &c) in coeffs {
        let mut new_mono: Vec<IRVarId> = mono.iter().map(|&v| if v == from { to } else { v }).collect();
        new_mono.sort();
        let entry = out.entry(new_mono).or_insert(0);
        *entry ^= c;
    }
    out.retain(|_, c| *c & 1 != 0);
    out
}

/// A reserved sentinel used only as a canonicalization placeholder inside
/// this pass -- never written into a real block (`IRVarId`'s own space is
/// dense from 0, so `u32::MAX` is always free).
const POLY_BATCH_SENTINEL: u32 = u32::MAX;

/// One batchable group: every member as `(stmt_index, hole_var)` -- the
/// specific variable that member uses in place of the group's own single
/// substituted position. `hole_var` is expressed in the block's
/// *original* (pre-rewrite) numbering.
struct PolyBatch {
    ty: TypeId,
    /// `stmt_index` of whichever member first opened this batch --
    /// used only to look up that member's own original `coeffs` as the
    /// substitution template during the rewrite phase.
    template_idx: usize,
    hole_var_in_template: IRVarId,
    members: Vec<(usize, IRVarId)>,
}

/// One forward pass over a single block: find and merge batchable `Poly`
/// groups. Returns `true` if the block was modified.
fn batch_ir_block_once<P: Clone>(block: &mut IRBlock<P>, types: &mut IRTypes) -> bool {
    let n_params = block.params.len();

    // ---- Phase 1: discover candidate batches (read-only). -----------------
    //
    // A statement can be proposed as a member of several *candidate*
    // batches at once (one per choice of which of its own variables is
    // "the hole") -- resolved to at most one real membership in the
    // dedup step below, so no statement is ever rewritten twice.
    let mut canon_map: BTreeMap<(TypeId, BTreeMap<Vec<IRVarId>, u8>), usize> = BTreeMap::new();
    let mut batches: Vec<PolyBatch> = Vec::new();

    for i in 0..block.stmts.len() {
        let (ty, coeffs) = match &block.stmts[i].kind {
            Stmt::Poly { ty, coeffs, .. } => (*ty, coeffs),
            _ => continue,
        };
        if type_bit_width(ty, types) != Some(1) {
            continue;
        }
        let vars = poly_vars(coeffs);
        if vars.is_empty() {
            continue;
        }

        let mut joined = false;
        for &hole in &vars {
            // Canonicalize by substituting `hole` with the sentinel: two
            // statements batchable via a single-var substitution always
            // produce IDENTICAL canonical forms (same monomials, same
            // coefficients, same sentinel position) -- this key is exact,
            // not an approximation, so a match here is already correct;
            // the reconstruction check below is a redundant belt-and-
            // braces confirmation, not load-bearing for correctness.
            let canon = substitute_var(coeffs, hole, IRVarId(POLY_BATCH_SENTINEL));
            let key = (ty, canon);
            if let Some(&bi) = canon_map.get(&key) {
                let template_coeffs = match &block.stmts[batches[bi].template_idx].kind {
                    Stmt::Poly { coeffs, .. } => coeffs.clone(),
                    _ => continue,
                };
                let reconstructed = substitute_var(&template_coeffs, batches[bi].hole_var_in_template, hole);
                if &reconstructed == coeffs && batches[bi].ty == ty {
                    batches[bi].members.push((i, hole));
                    joined = true;
                    break;
                }
            }
        }
        if joined {
            continue;
        }

        // No existing batch matched under any hole choice -- open a new
        // (as yet singleton) candidate batch for every choice; a later
        // statement matching any of these joins there.
        for &hole in &vars {
            let canon = substitute_var(coeffs, hole, IRVarId(POLY_BATCH_SENTINEL));
            let key = (ty, canon);
            canon_map.entry(key).or_insert_with(|| {
                batches.push(PolyBatch { ty, template_idx: i, hole_var_in_template: hole, members: vec![(i, hole)] });
                batches.len() - 1
            });
        }
    }

    // ---- Phase 1.5: enforce SSA ordering -----------------------------------
    //
    // The new wide Poly (and the Merge feeding it) must be inserted at the
    // group's own earliest member position, so every original member's own
    // Shuffle (at or after that position) can reference it. But a
    // *non-earliest* member's own hole var can itself be defined ANYWHERE
    // before *that member's own* original position -- possibly at or after
    // the group's earliest member. Such a member's hole var would not yet
    // be defined at the insertion point, violating "operands defined
    // earlier": drop it from the batch (its own Poly just stays unmerged).
    // The group's own earliest member is never affected: its hole var is
    // structurally guaranteed defined before its own position, which IS
    // the insertion point.
    for batch in &mut batches {
        let min_idx = batch.members.iter().map(|(idx, _)| *idx).min().unwrap();
        batch.members.retain(|&(_, hole)| {
            (hole.0 as usize) < n_params || (hole.0 as usize - n_params) < min_idx
        });
    }

    // ---- Phase 2: resolve overlaps (a statement can appear as a member ----
    // of several candidate batches -- greedily accept the largest first,
    // skipping any batch that overlaps an already-claimed statement).
    let mut order: Vec<usize> = (0..batches.len()).collect();
    order.sort_by_key(|&bi| core::cmp::Reverse(batches[bi].members.len()));
    let mut claimed: BTreeSet<usize> = BTreeSet::new();
    let mut accepted: Vec<usize> = Vec::new();
    for bi in order {
        if batches[bi].members.len() < 2 || batches[bi].members.len() > 64 {
            continue; // no benefit, or beyond emit_poly_wide's own width<=64 scope
        }
        if batches[bi].members.iter().any(|(idx, _)| claimed.contains(idx)) {
            continue;
        }
        for (idx, _) in &batches[bi].members {
            claimed.insert(*idx);
        }
        accepted.push(bi);
    }
    if accepted.is_empty() {
        return false;
    }

    // ---- Phase 3: rewrite. Single forward pass building new_stmts + a -----
    // var-id remap, inserting each accepted batch's own Merge+wide-Poly
    // pair right before its lowest-indexed member (preserving the
    // "operands always defined earlier" invariant), and replacing every
    // member's own original position with a `Shuffle` extracting its own
    // lane.
    let bit_ty = types.bit();

    let mut insert_before: BTreeMap<usize, usize> = BTreeMap::new();
    let mut member_of: BTreeMap<usize, usize> = BTreeMap::new();
    let mut sorted_members: BTreeMap<usize, Vec<(usize, IRVarId)>> = BTreeMap::new();
    for &bi in &accepted {
        let mut members = batches[bi].members.clone();
        members.sort_by_key(|(idx, _)| *idx);
        let min_idx = members[0].0;
        insert_before.insert(min_idx, bi);
        for &(idx, _) in &members {
            member_of.insert(idx, bi);
        }
        sorted_members.insert(bi, members);
    }

    let mut new_stmts: Vec<Node<IRStmt, P>> = Vec::with_capacity(block.stmts.len() + accepted.len());
    let mut remap: BTreeMap<u32, u32> = (0..n_params as u32).map(|v| (v, v)).collect();
    let mut next_var = n_params as u32;
    let mut batch_wide_var: BTreeMap<usize, u32> = BTreeMap::new();

    for i in 0..block.stmts.len() {
        if let Some(&bi) = insert_before.get(&i) {
            let members = &sorted_members[&bi];
            let width = members.len();
            let wide_ty = types.intern(IRType::Vec(width, bit_ty));

            // Merge: bundle every member's own (remapped) hole var into
            // one wide value, LSB-first by ascending original stmt index.
            let merge_parts: Vec<IRVarId> = members.iter()
                .map(|&(_, hole)| IRVarId(*remap.get(&hole.0).unwrap_or(&hole.0)))
                .collect();
            let merge_var = next_var; next_var += 1;
            new_stmts.push(Node { kind: Stmt::Merge { parts: merge_parts, ty: wide_ty }, ..block.stmts[i].clone() });

            // Wide Poly: the template's own coeffs, with every non-hole
            // var remapped and the hole var replaced by the Merge's own
            // new var id.
            let (template_coeffs, ) = match &block.stmts[batches[bi].template_idx].kind {
                Stmt::Poly { coeffs, .. } => (coeffs.clone(), ),
                _ => unreachable!("template_idx always points at a Poly (checked at open time)"),
            };
            let remapped_template: BTreeMap<Vec<IRVarId>, u8> = template_coeffs.iter()
                .map(|(mono, &c)| {
                    let mut new_mono: Vec<IRVarId> = mono.iter()
                        .map(|v| IRVarId(*remap.get(&v.0).unwrap_or(&v.0)))
                        .collect();
                    new_mono.sort();
                    (new_mono, c)
                })
                .collect();
            let template_hole_remapped = IRVarId(*remap.get(&batches[bi].hole_var_in_template.0).unwrap_or(&batches[bi].hole_var_in_template.0));
            let wide_coeffs = substitute_var(&remapped_template, template_hole_remapped, IRVarId(merge_var));

            // Combined constant: bit j = member j's own original
            // constant's own bit 0 (each member is width=1).
            let mut lo: u128 = 0;
            for (j, &(orig_idx, _)) in members.iter().enumerate() {
                if let Stmt::Poly { constant, .. } = &block.stmts[orig_idx].kind {
                    if constant.lo & 1 != 0 {
                        lo |= 1u128 << j;
                    }
                }
            }
            let wide_poly_var = next_var; next_var += 1;
            new_stmts.push(Node {
                kind: Stmt::Poly { ty: wide_ty, coeffs: wide_coeffs, constant: Constant { hi: 0, lo } },
                ..block.stmts[i].clone()
            });
            batch_wide_var.insert(bi, wide_poly_var);
        }

        if let Some(&bi) = member_of.get(&i) {
            let members = &sorted_members[&bi];
            let lane = members.iter().position(|(idx, _)| *idx == i).unwrap();
            let wide_var = batch_wide_var[&bi];
            let new_var = next_var; next_var += 1;
            new_stmts.push(Node {
                kind: Stmt::Shuffle { result_bits: vec![(lane as u8, IRVarId(wide_var))], ty: bit_ty },
                ..block.stmts[i].clone()
            });
            remap.insert((n_params + i) as u32, new_var);
            continue;
        }

        let new_var = next_var; next_var += 1;
        let old_kind = block.stmts[i].kind.clone();
        let new_kind = old_kind.map_var(
            &mut (),
            &mut |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
                Ok(IRVarId(*remap.get(&v.0).unwrap_or(&v.0)))
            },
            &mut |_, ty| Ok(ty),
            &mut |_, s| Ok(s),
        ).unwrap();
        new_stmts.push(Node { kind: new_kind, ..block.stmts[i].clone() });
        remap.insert((n_params + i) as u32, new_var);
    }

    let new_term = block.terminator.clone().map(
        &mut (),
        |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
            Ok(IRVarId(*remap.get(&v.0).unwrap_or(&v.0)))
        },
    ).unwrap();

    block.stmts = new_stmts;
    block.terminator = new_term;
    true
}

/// As [`dce_ir_blocks_with_remap`], but additionally treats every var id in
/// `extra_live` as an implicit root, exactly like a terminator reference.
///
/// For callers holding *external* var-id-based metadata that isn't part of
/// the block's own terminator or statements -- e.g. movfuscation's own
/// `MovfuscBlockBoundary`/`MovfuscAccumInfo` (which live outside this crate,
/// per `dce_ir_blocks_with_remap`'s own doc comment, and reference
/// block-local var ids directly). Without this, `dce_ir_block_once`'s
/// liveness analysis -- sound only when nothing outside a block can
/// reference one of its statements except via the block's own declared
/// params or terminator -- can silently strip a variable such metadata
/// still needs, and the caller's own remap application then panics far
/// downstream of the actual cause ("no remap entry for var N") with no
/// indication *why* that var was considered dead.
///
/// Only meaningful for a single-block `IRBlocks` (movfuscated circuits are
/// always exactly one block); `extra_live` is applied to every block for
/// simplicity, which is a correct no-op for any block that doesn't define
/// those var ids in its own local space.
pub fn dce_ir_blocks_with_remap_and_roots<P: Clone>(
    blocks: &mut IRBlocks<P>,
    _types: &IRTypes,
    extra_live: &[u32],
) -> (bool, Vec<BTreeMap<u32, u32>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap) = dce_ir_block_once(block, extra_live);
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
/// `false`). `extra_live` additionally roots any var id in this block's own
/// statement range, exactly like a terminator reference — see
/// [`dce_ir_blocks_with_remap_and_roots`]'s own doc for why this exists.
fn dce_ir_block_once<P: Clone>(block: &mut IRBlock<P>, extra_live: &[u32]) -> (bool, BTreeMap<u32, u32>) {
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
    for &v in extra_live {
        if (v as usize) >= n_params {
            let idx = v as usize - n_params;
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

#[cfg(test)]
mod batch_tests {
    use super::*;
    use volar_ir::ir::{IRBlock, IRType, IRTypeId};
    use volar_ir_common::Type;

    fn bit() -> IRTypeId { IRTypeId(0) }
    fn types_with_bit() -> IRTypes {
        IRTypes(alloc::vec![IRType::Primitive(Type::Bit)])
    }

    /// params: [a, b, c]. stmts: `a·b` (var 3), `a·c` (var 4), differing
    /// only in the second AND operand -- the textbook
    /// `is_active · touched_slot_k` shape this pass exists for. Both feed
    /// the terminator directly, so both must survive as real values.
    #[test]
    fn batches_two_and_gates_differing_in_one_operand() {
        let mut types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let c = IRVarId(2);
        let block = IRBlock {
            params: alloc::vec![bit(), bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly {
                    ty: bit(),
                    coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]),
                    constant: Constant { hi: 0, lo: 0 },
                }, (), None),
                Node::new(Stmt::Poly {
                    ty: bit(),
                    coeffs: BTreeMap::from([(alloc::vec![a, c], 1u8)]),
                    constant: Constant { hi: 0, lo: 0 },
                }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(3), IRVarId(4)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);

        let changed = batch_ir_blocks(&mut blocks, &mut types);
        assert!(changed, "two same-shape Polys differing in one operand must be batched");

        let stmts = &blocks.blocks[0].stmts;
        assert_eq!(stmts.len(), 4, "expected Merge + wide Poly + 2 Shuffles, got: {stmts:?}");

        let (merge_parts, merge_ty) = match &stmts[0].kind {
            Stmt::Merge { parts, ty } => (parts.clone(), *ty),
            other => panic!("expected Merge at position 0, got {other:?}"),
        };
        assert_eq!(merge_parts, alloc::vec![b, c], "merge must bundle the two VARYING operands, in original statement order");
        assert_eq!(types.0[merge_ty.0 as usize], IRType::Vec(2, bit()), "merge output must be a width-2 Bit vector");

        let (wide_coeffs, wide_ty, wide_const) = match &stmts[1].kind {
            Stmt::Poly { ty, coeffs, constant } => (coeffs.clone(), *ty, *constant),
            other => panic!("expected wide Poly at position 1, got {other:?}"),
        };
        assert_eq!(wide_ty, merge_ty, "wide Poly's own output type must match the Merge's own wide type");
        assert_eq!(wide_const, Constant { hi: 0, lo: 0 });
        let merge_var = IRVarId(3); // Merge is the first new statement -> var (n_params + 0)
        assert_eq!(wide_coeffs, BTreeMap::from([(alloc::vec![a, merge_var], 1u8)]), "wide Poly must keep the SHARED operand `a` broadcast and reference the merged wide value in place of the varying one");

        match &stmts[2].kind {
            Stmt::Shuffle { result_bits, ty } => {
                assert_eq!(result_bits, &alloc::vec![(0u8, IRVarId(4))], "first original statement (a·b) must extract lane 0");
                assert_eq!(*ty, bit());
            }
            other => panic!("expected Shuffle at position 2, got {other:?}"),
        }
        match &stmts[3].kind {
            Stmt::Shuffle { result_bits, ty } => {
                assert_eq!(result_bits, &alloc::vec![(1u8, IRVarId(4))], "second original statement (a·c) must extract lane 1");
                assert_eq!(*ty, bit());
            }
            other => panic!("expected Shuffle at position 3, got {other:?}"),
        }

        // Both original var ids (3, 4) must still resolve to something
        // usable -- the terminator (which referenced them directly) must
        // be remapped to the new Shuffle statements' own var ids (5, 6),
        // not left dangling or silently dropped.
        match &blocks.blocks[0].terminator {
            IRTerminator::Jmp { target } => assert_eq!(target.args, alloc::vec![IRVarId(5), IRVarId(6)], "terminator must be remapped to the new Shuffle statements' own var ids"),
            other => panic!("expected Jmp, got {other:?}"),
        }
    }

    /// Three Polys, two of which share a batchable shape (`a·b`/`a·c`)
    /// and one genuinely unrelated (`d·e`, disjoint variables entirely)
    /// -- the unrelated one must survive completely untouched (still a
    /// plain, unmerged `Poly`), proving this pass doesn't over-merge.
    #[test]
    fn leaves_unrelated_poly_untouched() {
        let mut types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let c = IRVarId(2);
        let d = IRVarId(3);
        let e = IRVarId(4);
        let block = IRBlock {
            params: alloc::vec![bit(), bit(), bit(), bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None),
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, c], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None),
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![d, e], 1u8)]), constant: Constant { hi: 0, lo: 1 } }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(5), IRVarId(6), IRVarId(7)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);

        let changed = batch_ir_blocks(&mut blocks, &mut types);
        assert!(changed);

        let stmts = &blocks.blocks[0].stmts;
        assert_eq!(stmts.len(), 5, "Merge + wide Poly + 2 Shuffles for the batched pair, plus the untouched d·e Poly: {stmts:?}");
        match &stmts[4].kind {
            Stmt::Poly { coeffs, constant, .. } => {
                assert_eq!(coeffs, &BTreeMap::from([(alloc::vec![d, e], 1u8)]), "the unrelated Poly's own coeffs must survive verbatim");
                assert_eq!(*constant, Constant { hi: 0, lo: 1 });
            }
            other => panic!("expected the untouched d·e Poly at position 4, got {other:?}"),
        }
    }

    /// No batchable pair at all (every Poly genuinely distinct) -> no
    /// change, block left completely untouched.
    #[test]
    fn no_batchable_pair_is_a_noop() {
        let mut types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let c = IRVarId(2);
        let block = IRBlock {
            params: alloc::vec![bit(), bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None),
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, c], 1u8), (alloc::vec![b, c], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(3), IRVarId(4)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let changed = batch_ir_blocks(&mut blocks, &mut types);
        assert!(!changed, "a degree-2-monomial-count mismatch (1 vs 2) must never be batched");
        assert_eq!(blocks.blocks[0].stmts.len(), 2);
    }
}
