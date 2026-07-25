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
        if batch_ir_block_once(block, types, None).0 {
            any_changed = true;
        }
    }
    any_changed
}

/// As [`batch_ir_blocks_with_remap`], but also returns, for each block, a
/// map from every brand-new var id it created (the wide `Poly` plus its
/// own feeding `Merge`, one pair per accepted batch) to the list of
/// member var ids -- in the block's own *pre-call* numbering -- that
/// batch was built from. A batch-created var has no pre-optimization
/// identity of its own (unlike every other var, which survives from
/// before this call under `remap`), so a caller reconstructing
/// provenance/region info across a pass (e.g. to decide which final
/// statements need hoisting into a shared region -- see
/// `docs/interpreter-honest-e2e-zk-plan.md`'s "Cross-chunk locality"
/// section) needs this separately.
pub fn batch_ir_blocks_with_remap_and_members<P: Clone>(
    blocks: &mut IRBlocks<P>,
    types: &mut IRTypes,
) -> (bool, Vec<BTreeMap<u32, u32>>, Vec<BTreeMap<u32, Vec<u32>>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    let mut members = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap, new_var_members) = batch_ir_block_once(block, types, None);
        any_changed |= changed;
        remaps.push(remap);
        members.push(new_var_members);
    }
    (any_changed, remaps, members)
}

/// As [`batch_ir_blocks_with_remap`], but never groups two `Poly`s into
/// the same batch unless `region_of[i] == region_of[j]` for their own
/// statement indices -- see [`cse_ir_blocks_with_regions`]'s own doc for
/// why this exists (the same split-weave per-original-block locality
/// constraint, confirmed by direct testing to matter for batching too).
pub fn batch_ir_blocks_with_regions<P: Clone>(
    blocks: &mut IRBlocks<P>,
    types: &mut IRTypes,
    region_of: &[u32],
) -> (bool, Vec<BTreeMap<u32, u32>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap, _new_var_members) = batch_ir_block_once(block, types, Some(region_of));
        any_changed |= changed;
        remaps.push(remap);
    }
    (any_changed, remaps)
}

/// As [`batch_ir_blocks`], but also returns each block's own cumulative
/// var-id remap (old `IRVarId.0` -> new `IRVarId.0`; identity for a block
/// left untouched) -- same purpose as [`dce_ir_blocks_with_remap`]/
/// [`cse_ir_blocks_with_remap`]: lets a caller holding external
/// var-id-based metadata (`MovfuscBlockBoundary`/`MovfuscAccumInfo`)
/// translate it to stay valid post-batching. Batching inserts new
/// statements (the `Merge` + wide `Poly` per accepted group), so every
/// later statement's own var id shifts -- always compose this remap into
/// any cumulative remap.
pub fn batch_ir_blocks_with_remap<P: Clone>(
    blocks: &mut IRBlocks<P>,
    types: &mut IRTypes,
) -> (bool, Vec<BTreeMap<u32, u32>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap, _new_var_members) = batch_ir_block_once(block, types, None);
        any_changed |= changed;
        remaps.push(remap);
    }
    (any_changed, remaps)
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
/// groups. Returns `(changed, remap, new_var_members)` -- `remap` maps
/// every pre-call `IRVarId.0` to its post-call `IRVarId.0` (identity for
/// every id when `changed` is `false`); `new_var_members` maps every
/// brand-new var id (the wide `Poly` + its feeding `Merge`, per accepted
/// batch) to the pre-call var ids of that batch's own members -- see
/// [`batch_ir_blocks_with_remap_and_members`]'s own doc for why this is
/// needed.
fn batch_ir_block_once<P: Clone>(block: &mut IRBlock<P>, types: &mut IRTypes, region_of: Option<&[u32]>) -> (bool, BTreeMap<u32, u32>, BTreeMap<u32, Vec<u32>>) {
    let n_params = block.params.len();

    // ---- Phase 1: discover candidate batches (read-only). -----------------
    //
    // A statement can be proposed as a member of several *candidate*
    // batches at once (one per choice of which of its own variables is
    // "the hole") -- resolved to at most one real membership in the
    // dedup step below, so no statement is ever rewritten twice. Keying
    // on `region_of[i]` too (when given) means two statements from
    // different regions never join the same batch -- see
    // `batch_ir_blocks_with_regions`'s own doc for why.
    let mut canon_map: BTreeMap<(u32, TypeId, BTreeMap<Vec<IRVarId>, u8>), usize> = BTreeMap::new();
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
        let region = region_of.map(|r| r[i]).unwrap_or(0);

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
            let key = (region, ty, canon);
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
            let key = (region, ty, canon);
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
        let identity: BTreeMap<u32, u32> = (0..(n_params + block.stmts.len()) as u32).map(|v| (v, v)).collect();
        return (false, identity, BTreeMap::new());
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
    let mut new_var_members: BTreeMap<u32, Vec<u32>> = BTreeMap::new();

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

            let member_pre_vars: Vec<u32> = members.iter().map(|&(idx, _)| (n_params + idx) as u32).collect();
            new_var_members.insert(merge_var, member_pre_vars.clone());
            new_var_members.insert(wide_poly_var, member_pre_vars);
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
    (true, remap, new_var_members)
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
// Common subexpression elimination
// ============================================================================

/// Deduplicate statements that compute the exact same value: two pure
/// statements with identical kind + operands (after applying any
/// dedup already discovered earlier in the same forward pass) collapse
/// to one -- every later occurrence becomes an alias for the first,
/// instead of a redundant repeated computation. Existing DCE/`batch_ir_blocks`
/// then have more to work with: DCE can remove statements whose only use
/// was itself now-deduplicated away, and `batch_ir_blocks`'s own
/// single-variable-substitution matching sees fewer spurious differences
/// once genuinely-identical sub-expressions (e.g. movfuscation's own
/// `is_active_i` MUX selector, or an identical MUXed value recomputed for
/// two different purposes) collapse to one shared reference.
///
/// Only pure statement kinds are considered (`Poly`, `Merge`, `Shuffle`,
/// `Rol`, `Ror`, `Splat`, `Transmute`, `Const`) -- side-effecting or
/// storage-addressed statements (`StorageRead`/`StorageWrite`/
/// `OracleCall`/`ActionCall`/`Rng`/etc.) are left untouched, matching
/// `dce_ir_blocks`'s own established scope split: storage aliasing is
/// `store_forward_ir_blocks`'s own, more careful job (has to reason about
/// intervening writes), not this pass's.
///
/// Deliberately exact, not approximate: two statements only collapse when
/// their `Stmt` values (kind + every operand, post-remap) are literally
/// equal -- this can only ever miss a real deduplication opportunity
/// (e.g. two Polys that are algebraically equal but not syntactically
/// identical), never merge two different computations.
///
/// Returns `true` if any block was modified.
pub fn cse_ir_blocks<P: Clone>(blocks: &mut IRBlocks<P>, _types: &IRTypes) -> bool {
    let mut any_changed = false;
    for block in blocks.blocks.iter_mut() {
        if cse_ir_block_once(block, None).0 {
            any_changed = true;
        }
    }
    any_changed
}

/// As [`cse_ir_blocks`], but also returns each block's own cumulative
/// var-id remap (old `IRVarId.0` -> new `IRVarId.0`; identity for a block
/// CSE left untouched) -- same purpose as [`dce_ir_blocks_with_remap`]:
/// lets a caller holding external var-id-based metadata (e.g.
/// movfuscation's own `MovfuscBlockBoundary`/`MovfuscAccumInfo`)
/// translate it to stay valid post-CSE. Unlike `fold_ir_blocks`/
/// `store_forward_ir_blocks`, CSE genuinely changes statement indices
/// (deduplication removes statements), so this remap is not the identity
/// in general -- always compose it into any cumulative remap, the same
/// way `dce_ir_blocks_with_remap`'s own output must be.
pub fn cse_ir_blocks_with_remap<P: Clone>(
    blocks: &mut IRBlocks<P>,
    _types: &IRTypes,
) -> (bool, Vec<BTreeMap<u32, u32>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap) = cse_ir_block_once(block, None);
        any_changed |= changed;
        remaps.push(remap);
    }
    (any_changed, remaps)
}

/// As [`cse_ir_blocks_with_remap`], but never deduplicates two statements
/// unless `region_of[i] == region_of[j]` for their own statement indices
/// `i`/`j` (`region_of.len()` must equal the (single) block's own
/// `stmts.len()`). Exists for the post-movfuscation, single-combined-
/// block case: the split-weave's own per-original-block chunking
/// (`MovfuscBlockBoundary`/`MovfuscAccumInfo`) assumes a chunk function
/// only ever needs the shared prefix plus its own `[start, end)` range --
/// unconstrained CSE can dedup two statements from *different* original
/// blocks, relocating the surviving one outside a chunk that still needs
/// it (confirmed via direct testing: this produces a "no entry found for
/// key" panic in the weaver, not a silent wrong answer). Region-aware
/// CSE never crosses that boundary, at the cost of missing any
/// cross-region duplicate (which is where most of the unconstrained
/// win came from -- see `docs/interpreter-honest-e2e-zk-plan.md`'s own
/// "Poly batching" section for the measured before/after).
pub fn cse_ir_blocks_with_regions<P: Clone>(
    blocks: &mut IRBlocks<P>,
    _types: &IRTypes,
    region_of: &[u32],
) -> (bool, Vec<BTreeMap<u32, u32>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap) = cse_ir_block_once(block, Some(region_of));
        any_changed |= changed;
        remaps.push(remap);
    }
    (any_changed, remaps)
}

/// Remap every operand var id in `kind` through `remap`, GF(2)-safely for
/// `Poly`.
///
/// A plain `Stmt::map_var` on `Poly`'s own `coeffs` remaps each
/// monomial's own var list independently and `.collect()`s the results
/// back into a `BTreeMap` -- if `remap` ever sends two *different*
/// monomials to the *same* key (only possible when `remap` is genuinely
/// many-to-one, e.g. CSE's own dedup map -- never happens for a purely
/// injective renumbering like `batch_ir_block_once`'s own compaction
/// remap, which never sends two different old vars to the same new var),
/// `BTreeMap::collect`'s "last write wins" silently drops one term
/// instead of combining them. That's wrong: e.g. `a XOR b` where `b` gets
/// deduplicated onto `a` must become the constant 0 (GF(2): `a XOR a =
/// 0`), not silently `a`. Reuses the same XOR-combine-then-drop-even-
/// coefficients discipline as `batch_ir_blocks`'s own `substitute_var`.
fn remap_stmt_operands(kind: IRStmt, remap: &BTreeMap<u32, u32>) -> IRStmt {
    if let Stmt::Poly { ty, coeffs, constant } = &kind {
        let mut new_coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        for (mono, &c) in coeffs {
            let mut new_mono: Vec<IRVarId> = mono.iter().map(|v| IRVarId(*remap.get(&v.0).unwrap_or(&v.0))).collect();
            new_mono.sort();
            let entry = new_coeffs.entry(new_mono).or_insert(0);
            *entry ^= c;
        }
        new_coeffs.retain(|_, c| *c & 1 != 0);
        return Stmt::Poly { ty: *ty, coeffs: new_coeffs, constant: *constant };
    }
    kind.map_var(
        &mut (),
        &mut |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
            Ok(IRVarId(*remap.get(&v.0).unwrap_or(&v.0)))
        },
        &mut |_, ty| Ok(ty),
        &mut |_, s| Ok(s),
    ).unwrap()
}

/// One forward pass over a single block: dedup pure statements and
/// compact the result (removed statements shift every later var id).
/// Returns `(changed, remap)` -- `remap` maps every pre-call `IRVarId.0`
/// to its post-call `IRVarId.0` (identity for every id when `changed` is
/// `false`).
fn cse_ir_block_once<P: Clone>(block: &mut IRBlock<P>, region_of: Option<&[u32]>) -> (bool, BTreeMap<u32, u32>) {
    let n_params = block.params.len();
    let mut remap: BTreeMap<u32, u32> = (0..n_params as u32).map(|v| (v, v)).collect();
    let mut canon_map: BTreeMap<(u32, IRStmt), u32> = BTreeMap::new();
    let mut new_stmts: Vec<Node<IRStmt, P>> = Vec::with_capacity(block.stmts.len());
    let mut changed = false;

    for i in 0..block.stmts.len() {
        let old_var = (n_params + i) as u32;
        let region = region_of.map(|r| r[i]).unwrap_or(0);
        let remapped_kind = remap_stmt_operands(block.stmts[i].kind.clone(), &remap);

        let is_pure = matches!(
            remapped_kind,
            Stmt::Poly { .. } | Stmt::Merge { .. } | Stmt::Shuffle { .. }
                | Stmt::Rol { .. } | Stmt::Ror { .. } | Stmt::Splat { .. }
                | Stmt::Transmute { .. } | Stmt::Const(..)
        );

        if is_pure {
            if let Some(&existing_new_var) = canon_map.get(&(region, remapped_kind.clone())) {
                remap.insert(old_var, existing_new_var);
                changed = true;
                continue;
            }
        }

        let new_var = (n_params + new_stmts.len()) as u32;
        if new_var != old_var || remapped_kind != block.stmts[i].kind {
            changed = true;
        }
        remap.insert(old_var, new_var);
        if is_pure {
            canon_map.insert((region, remapped_kind.clone()), new_var);
        }
        new_stmts.push(Node { kind: remapped_kind, ..block.stmts[i].clone() });
    }

    if !changed {
        return (false, remap);
    }

    let new_term = block.terminator.clone().map(
        &mut (),
        |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
            Ok(IRVarId(*remap.get(&v.0).unwrap_or(&v.0)))
        },
    ).unwrap();

    block.stmts = new_stmts;
    block.terminator = new_term;
    (true, remap)
}

/// Given a single-block `IRBlocks` (the movfuscated circuit) plus, for
/// each of its own *current* statements, the set of original "regions"
/// it serves (`region_sets[i]`, one entry per statement -- typically
/// computed by a caller that ran unconstrained `cse_ir_blocks_with_remap`/
/// `batch_ir_blocks_with_remap_and_members` and tracked, for every
/// surviving/created var, which pre-optimization regions contributed to
/// it), physically reorders the block so every statement whose own
/// `region_sets[i]` has more than one entry -- or is empty (unknown;
/// treated conservatively) -- moves into a leading "region 0"
/// (shared-prefix) group, immediately followed by each remaining
/// region's own statements in their original relative order. Also folds
/// any statement whose *own* single region is already `0` into that same
/// leading group (a no-op move, since it's already there).
///
/// This implements "hoist cross-chunk-shared statements into
/// shared_prefix and extend the boundary metadata to match" (see
/// `docs/interpreter-honest-e2e-zk-plan.md`'s "Cross-chunk locality"
/// section) as an alternative to constraining CSE/batch not to produce
/// such statements in the first place (`cse_ir_blocks_with_regions`/
/// `batch_ir_blocks_with_regions`): run CSE/batch fully unconstrained for
/// maximum optimization, then relocate only the statements that actually
/// need to be visible to more than one split-weave chunk function.
///
/// **Topological validity is preserved by construction, not by
/// re-sorting**: a statement in single-region group `R` (`R != 0`) can
/// only ever reference a var either (a) originally defined in region `R`
/// itself (preserved: within-group order is untouched), or (b) a var
/// that CSE deduplicated or batch created -- and *any* such var's own
/// `region_sets` entry is, by construction, either a strict superset of
/// `{R}` or otherwise multi-region (since matching/batching requires
/// byte-identical operands, which themselves must already be visible
/// wherever the match occurs) -- so it is always already assigned to the
/// leading group. There is no case where a single-region statement
/// references another *different* single-region statement, so relative
/// order between distinct non-zero groups is irrelevant to correctness.
///
/// Returns `(changed, remap, region_ranges)`:
/// - `remap` maps every pre-call `IRVarId.0` to its post-call `IRVarId.0`
///   (identity when `changed` is `false`).
/// - `region_ranges` maps every region id that appeared in `region_sets`
///   as a singleton (i.e. every `group_key`, including `0`) to its own
///   *contiguous* `[start, end)` var-id range in the **post-call**
///   numbering -- safe to use directly as a `MovfuscBlockBoundary`/
///   `MovfuscAccumInfo` range's own `start`/`end`. This is deliberately
///   NOT the same as remapping the *old* `start`/`end` var ids through
///   `remap`: the specific var that used to sit at a region's old `start`
///   may itself have been hoisted away (multi-region), so remapping it
///   directly would point at wherever *that var* ended up (inside the
///   shared group), not at the true new start of the region's own
///   remaining, still-contiguous statements.
pub fn hoist_shared_statements<P: Clone>(
    blocks: &mut IRBlocks<P>,
    region_sets: &[BTreeSet<u32>],
) -> (bool, Vec<BTreeMap<u32, u32>>, Vec<BTreeMap<u32, (u32, u32)>>) {
    let mut any_changed = false;
    let mut remaps = Vec::with_capacity(blocks.blocks.len());
    let mut ranges = Vec::with_capacity(blocks.blocks.len());
    for block in blocks.blocks.iter_mut() {
        let (changed, remap, region_ranges) = hoist_shared_statements_once(block, region_sets);
        any_changed |= changed;
        remaps.push(remap);
        ranges.push(region_ranges);
    }
    (any_changed, remaps, ranges)
}

fn hoist_shared_statements_once<P: Clone>(
    block: &mut IRBlock<P>,
    region_sets: &[BTreeSet<u32>],
) -> (bool, BTreeMap<u32, u32>, BTreeMap<u32, (u32, u32)>) {
    let n_params = block.params.len();
    assert_eq!(region_sets.len(), block.stmts.len(), "region_sets must have exactly one entry per statement");

    let group_key = |i: usize| -> u32 {
        let set = &region_sets[i];
        if set.len() == 1 { *set.iter().next().unwrap() } else { 0 }
    };

    // Rank groups by first original appearance, with group 0 always rank 0
    // (guaranteed leading regardless of where it first literally appears).
    let mut group_rank: BTreeMap<u32, usize> = BTreeMap::new();
    group_rank.insert(0, 0);
    let mut next_rank = 1usize;
    for i in 0..block.stmts.len() {
        let g = group_key(i);
        group_rank.entry(g).or_insert_with(|| {
            let r = next_rank;
            next_rank += 1;
            r
        });
    }

    let mut new_order: Vec<usize> = (0..block.stmts.len()).collect();
    new_order.sort_by_key(|&i| (group_rank[&group_key(i)], i));

    // Every group's members end up contiguous in `new_order` (a stable
    // sort keyed on group rank) -- record each group's own [min, max] new
    // index while it's cheap to do so, in the SAME pass regardless of
    // whether anything actually moved.
    let mut region_ranges: BTreeMap<u32, (u32, u32)> = BTreeMap::new();
    for (new_idx, &old_idx) in new_order.iter().enumerate() {
        let g = group_key(old_idx);
        let v = (n_params + new_idx) as u32;
        region_ranges.entry(g).and_modify(|(_, end)| *end = v + 1).or_insert((v, v + 1));
    }

    if new_order.iter().enumerate().all(|(new_i, &old_i)| new_i == old_i) {
        let identity: BTreeMap<u32, u32> = (0..(n_params + block.stmts.len()) as u32).map(|v| (v, v)).collect();
        return (false, identity, region_ranges);
    }

    let mut remap: BTreeMap<u32, u32> = (0..n_params as u32).map(|v| (v, v)).collect();
    for (new_idx, &old_idx) in new_order.iter().enumerate() {
        remap.insert((n_params + old_idx) as u32, (n_params + new_idx) as u32);
    }

    let mut new_stmts: Vec<Node<IRStmt, P>> = Vec::with_capacity(block.stmts.len());
    for &old_idx in &new_order {
        let new_kind = remap_stmt_operands(block.stmts[old_idx].kind.clone(), &remap);
        new_stmts.push(Node { kind: new_kind, ..block.stmts[old_idx].clone() });
    }
    let new_term = block.terminator.clone().map(
        &mut (),
        |_: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
            Ok(IRVarId(*remap.get(&v.0).unwrap_or(&v.0)))
        },
    ).unwrap();

    block.stmts = new_stmts;
    block.terminator = new_term;
    (true, remap, region_ranges)
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

#[cfg(test)]
mod cse_tests {
    use super::*;
    use volar_ir::ir::{IRBlock, IRTypeId};
    use volar_ir_common::Type;

    fn bit() -> IRTypeId { IRTypeId(0) }
    fn types_with_bit() -> IRTypes {
        IRTypes(alloc::vec![IRType::Primitive(Type::Bit)])
    }

    /// params: [a, b]. stmts: `a·b` (var 2), `a·b` again (var 3, byte-for-
    /// byte identical), then a consumer that references ONLY the
    /// duplicate (var 4). The duplicate must collapse to an alias of the
    /// first, and the consumer's own reference to var 3 must be remapped
    /// to var 2's own new position.
    #[test]
    fn dedups_two_identical_polys() {
        let types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let and_poly = || Stmt::Poly {
            ty: bit(),
            coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]),
            constant: Constant { hi: 0, lo: 0 },
        };
        let block = IRBlock {
            params: alloc::vec![bit(), bit()],
            stmts: alloc::vec![
                Node::new(and_poly(), (), None),
                Node::new(and_poly(), (), None),
                // consumer: NOT of the SECOND copy alone (var 3, soon
                // deduped) -- no collision, just confirms remapping.
                Node::new(Stmt::Poly {
                    ty: bit(),
                    coeffs: BTreeMap::from([(alloc::vec![IRVarId(3)], 1u8)]),
                    constant: Constant { hi: 0, lo: 1 },
                }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(4)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);

        let changed = cse_ir_blocks(&mut blocks, &types);
        assert!(changed, "two byte-for-byte identical Polys must be deduplicated");

        let stmts = &blocks.blocks[0].stmts;
        assert_eq!(stmts.len(), 2, "the duplicate must be gone entirely, not just aliased-but-kept: {stmts:?}");
        match &stmts[0].kind {
            Stmt::Poly { coeffs, .. } => assert_eq!(coeffs, &BTreeMap::from([(alloc::vec![a, b], 1u8)])),
            other => panic!("expected the surviving a·b Poly at position 0, got {other:?}"),
        }
        match &stmts[1].kind {
            // Must now reference the SAME surviving var (IRVarId(2)) --
            // the consumer's own reference to the now-removed duplicate
            // (originally var 3) must have been remapped, not left
            // dangling.
            Stmt::Poly { coeffs, .. } => assert_eq!(coeffs, &BTreeMap::from([(alloc::vec![IRVarId(2)], 1u8)])),
            other => panic!("expected the consumer Poly at position 1, got {other:?}"),
        }
        match &blocks.blocks[0].terminator {
            IRTerminator::Jmp { target } => assert_eq!(target.args, alloc::vec![IRVarId(3)], "terminator must be remapped to the consumer's own new (compacted) var id"),
            other => panic!("expected Jmp, got {other:?}"),
        }
    }

    /// The GF(2)-safety case `remap_stmt_operands` exists for: a consumer
    /// references BOTH the surviving var (2) and the about-to-be-deduped
    /// duplicate (3) in the SAME `Poly` (`var2 XOR var3`). Since var 3
    /// dedups onto var 2, this is algebraically `var2 XOR var2 = 0` (GF(2))
    /// -- a naive per-monomial remap-and-collect would instead silently
    /// keep one of the two now-identical-key entries (wrong: `= var2`).
    #[test]
    fn poly_remap_is_gf2_safe_on_monomial_collision() {
        let types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let and_poly = || Stmt::Poly {
            ty: bit(),
            coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]),
            constant: Constant { hi: 0, lo: 0 },
        };
        let block = IRBlock {
            params: alloc::vec![bit(), bit()],
            stmts: alloc::vec![
                Node::new(and_poly(), (), None),
                Node::new(and_poly(), (), None),
                // consumer: var2 XOR var3 -- BOTH operands, one of which
                // is about to be deduped onto the other.
                Node::new(Stmt::Poly {
                    ty: bit(),
                    coeffs: BTreeMap::from([(alloc::vec![IRVarId(2)], 1u8), (alloc::vec![IRVarId(3)], 1u8)]),
                    constant: Constant { hi: 0, lo: 0 },
                }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(4)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);

        let changed = cse_ir_blocks(&mut blocks, &types);
        assert!(changed);

        let stmts = &blocks.blocks[0].stmts;
        assert_eq!(stmts.len(), 2, "{stmts:?}");
        match &stmts[1].kind {
            Stmt::Poly { coeffs, constant, .. } => {
                assert!(coeffs.is_empty(), "var2 XOR var2 must collapse to the empty monomial set (always 0), not silently keep one term: {coeffs:?}");
                assert_eq!(constant.lo & 1, 0, "must evaluate to constant 0");
            }
            other => panic!("expected the collapsed consumer Poly at position 1, got {other:?}"),
        }
    }

    /// Two Polys with the SAME operands but a DIFFERENT `Stmt` variant
    /// (`Poly` vs. `Merge`) must never collapse -- `Stmt`'s own derived
    /// `Eq` already distinguishes variants, this just confirms CSE relies
    /// on that rather than comparing operand lists loosely.
    #[test]
    fn different_stmt_kinds_never_collapse() {
        let types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let block = IRBlock {
            params: alloc::vec![bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None),
                Node::new(Stmt::Merge { parts: alloc::vec![a, b], ty: bit() }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(2), IRVarId(3)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let changed = cse_ir_blocks(&mut blocks, &types);
        assert!(!changed, "different Stmt variants must never be treated as duplicates");
        assert_eq!(blocks.blocks[0].stmts.len(), 2);
    }

    /// Storage reads are explicitly out of scope (left to
    /// `store_forward_ir_blocks`'s own, more careful handling) -- two
    /// identical `StorageRead`s must both survive untouched.
    #[test]
    fn storage_reads_are_never_deduplicated() {
        let types = types_with_bit();
        let addr = IRVarId(0);
        let block = IRBlock {
            params: alloc::vec![bit()],
            stmts: alloc::vec![
                Node::new(Stmt::StorageRead { storage: volar_ir_common::StorageId(0), ty: bit(), addr }, (), None),
                Node::new(Stmt::StorageRead { storage: volar_ir_common::StorageId(0), ty: bit(), addr }, (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(1), IRVarId(2)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let changed = cse_ir_blocks(&mut blocks, &types);
        assert!(!changed, "StorageRead is explicitly out of CSE's scope");
        assert_eq!(blocks.blocks[0].stmts.len(), 2);
    }

    /// Two byte-for-byte identical Polys that WOULD dedup under
    /// unconstrained CSE must NOT dedup when they fall in different
    /// regions -- the split-weave per-original-block locality guard.
    #[test]
    fn region_aware_cse_never_crosses_a_region_boundary() {
        let types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let and_poly = || Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } };
        let block = IRBlock {
            params: alloc::vec![bit(), bit()],
            stmts: alloc::vec![
                Node::new(and_poly(), (), None), // region 0
                Node::new(and_poly(), (), None), // region 1 -- must NOT dedup with the above
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(2), IRVarId(3)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let region_of = [0u32, 1u32];
        let (changed, _) = cse_ir_blocks_with_regions(&mut blocks, &types, &region_of);
        assert!(!changed, "identical Polys in different regions must never be deduplicated");
        assert_eq!(blocks.blocks[0].stmts.len(), 2);

        // Sanity: the SAME input, unconstrained, DOES dedup -- confirms
        // the region constraint is what's blocking it, not some other
        // difference between the two Polys.
        let mut types2 = types_with_bit();
        let block2 = IRBlock {
            params: alloc::vec![bit(), bit()],
            stmts: alloc::vec![Node::new(and_poly(), (), None), Node::new(and_poly(), (), None)],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(2), IRVarId(3)]) },
        };
        let mut blocks2: IRBlocks = IRBlocks::new(alloc::vec![block2]);
        let changed2 = cse_ir_blocks(&mut blocks2, &mut types2);
        assert!(changed2, "sanity: unconstrained CSE must dedup the same input");
    }

    /// Same guard for `batch_ir_blocks`: two Polys that WOULD batch
    /// (single-variable substitution) must not batch across regions.
    #[test]
    fn region_aware_batch_never_crosses_a_region_boundary() {
        let mut types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let c = IRVarId(2);
        let block = IRBlock {
            params: alloc::vec![bit(), bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None), // region 0
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, c], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None), // region 1
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(3), IRVarId(4)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let region_of = [0u32, 1u32];
        let (changed, _) = batch_ir_blocks_with_regions(&mut blocks, &mut types, &region_of);
        assert!(!changed, "batchable Polys in different regions must never be merged");
        assert_eq!(blocks.blocks[0].stmts.len(), 2);
    }
}

#[cfg(test)]
mod hoist_tests {
    use super::*;
    use volar_ir::ir::{IRBlock, IRType, IRTypeId};
    use volar_ir_common::Type;

    fn bit() -> IRTypeId { IRTypeId(0) }
    fn types_with_bit() -> IRTypes {
        IRTypes(alloc::vec![IRType::Primitive(Type::Bit)])
    }

    /// 4 statements: `a·b` (region 0, already shared), `Const 1`
    /// (region 1, unrelated), `a·c` (region 1's *own* first occurrence of
    /// what a later CSE pass decided is shared with region 2), and a
    /// consumer of `a·c` tagged region 2 (the cross-region reference).
    /// `a·c`'s own `region_sets` entry is `{1, 2}` (multi-region -> must
    /// hoist), even though it originally sat *after* the unrelated
    /// region-1 `Const` -- exercising real physical reordering, not just
    /// a reclassification that happens to already be in place.
    #[test]
    fn hoists_a_multi_region_statement_ahead_of_an_unrelated_earlier_statement() {
        let _types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let c = IRVarId(2);
        let block = IRBlock {
            params: alloc::vec![bit(), bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None), // var 3, region {0}
                Node::new(Stmt::Const(Constant { hi: 0, lo: 1 }, bit()), (), None), // var 4, region {1}, unrelated
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, c], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None), // var 5, region {1,2}
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![IRVarId(5)], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None), // var 6, region {2}
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(3), IRVarId(4), IRVarId(5), IRVarId(6)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let region_sets: Vec<BTreeSet<u32>> = alloc::vec![
            BTreeSet::from([0]),
            BTreeSet::from([1]),
            BTreeSet::from([1, 2]),
            BTreeSet::from([2]),
        ];

        let (changed, remaps, region_ranges) = hoist_shared_statements(&mut blocks, &region_sets);
        assert!(changed, "the multi-region statement must be physically relocated");

        let stmts = &blocks.blocks[0].stmts;
        assert_eq!(stmts.len(), 4);
        match &stmts[0].kind {
            Stmt::Poly { coeffs, .. } => assert_eq!(coeffs, &BTreeMap::from([(alloc::vec![a, b], 1u8)]), "region-0 statement stays first"),
            other => panic!("expected a·b first, got {other:?}"),
        }
        match &stmts[1].kind {
            Stmt::Poly { coeffs, .. } => assert_eq!(coeffs, &BTreeMap::from([(alloc::vec![a, c], 1u8)]), "the multi-region a·c must be hoisted to position 1, ahead of the unrelated Const"),
            other => panic!("expected the hoisted a·c at position 1, got {other:?}"),
        }
        match &stmts[2].kind {
            Stmt::Const(c, _) => assert_eq!(*c, Constant { hi: 0, lo: 1 }, "the unrelated region-1 Const is pushed after the hoisted statement"),
            other => panic!("expected the Const at position 2, got {other:?}"),
        }
        match &stmts[3].kind {
            Stmt::Poly { coeffs, .. } => assert_eq!(coeffs, &BTreeMap::from([(alloc::vec![IRVarId(4)], 1u8)]), "the consumer's own reference to a·c must be remapped to a·c's new var id (4)"),
            other => panic!("expected the consumer Poly last, got {other:?}"),
        }
        match &blocks.blocks[0].terminator {
            IRTerminator::Jmp { target } => assert_eq!(
                target.args,
                alloc::vec![IRVarId(3), IRVarId(5), IRVarId(4), IRVarId(6)],
                "terminator refs must track each statement's own new position"
            ),
            other => panic!("expected Jmp, got {other:?}"),
        }
        assert_eq!(remaps.len(), 1);
        assert_eq!(remaps[0].get(&5), Some(&4), "old var 5 (a·c) must now resolve to var 4");
        assert_eq!(remaps[0].get(&4), Some(&5), "old var 4 (Const) must now resolve to var 5");

        assert_eq!(region_ranges.len(), 1);
        assert_eq!(
            region_ranges[0],
            BTreeMap::from([(0u32, (3u32, 5u32)), (1u32, (5u32, 6u32)), (2u32, (6u32, 7u32))]),
            "group 0 (shared) must cover the two hoisted/existing-shared statements at [3,5), \
             region 1's own remainder shrinks to just the Const at [5,6), region 2's own consumer stays at [6,7)"
        );
    }

    /// Every statement already in its own single region (no multi-region
    /// entries at all, and none already tagged region 0) -> the original
    /// order already satisfies "region 0 first" trivially (there is no
    /// region 0 statement at all here), so nothing needs to move.
    #[test]
    fn no_multi_region_statements_is_a_noop() {
        let _types = types_with_bit();
        let a = IRVarId(0);
        let b = IRVarId(1);
        let block = IRBlock {
            params: alloc::vec![bit(), bit()],
            stmts: alloc::vec![
                Node::new(Stmt::Poly { ty: bit(), coeffs: BTreeMap::from([(alloc::vec![a, b], 1u8)]), constant: Constant { hi: 0, lo: 0 } }, (), None),
                Node::new(Stmt::Const(Constant { hi: 0, lo: 1 }, bit()), (), None),
            ],
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, alloc::vec![IRVarId(2), IRVarId(3)]),
            },
        };
        let mut blocks: IRBlocks = IRBlocks::new(alloc::vec![block]);
        let region_sets: Vec<BTreeSet<u32>> = alloc::vec![BTreeSet::from([1]), BTreeSet::from([2])];
        let (changed, _, _) = hoist_shared_statements(&mut blocks, &region_sets);
        assert!(!changed);
        assert_eq!(blocks.blocks[0].stmts.len(), 2);
    }
}
