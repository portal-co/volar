// @reliability: experimental
// @ai: assisted
//! Store-to-load forwarding passes for all three IR layers.
//!
//! Within a single block, when a `StorageRead` follows a `StorageWrite` to the
//! same `(StorageId, type_key, addr_var)`, the load result is replaced by an
//! alias pointing to the stored value.  All downstream uses of the read result
//! are rewritten through the alias map, and the terminator is rewritten too.
//!
//! # Invalidation policy
//! A write to `(S, T)` — same `StorageId` AND same `TypeId` / `bit_width` —
//! clears **all** cached reads for that `(S, T)` pair, regardless of address.

use alloc::{collections::BTreeMap, vec, vec::Vec};
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId};
use volar_ir_common::{StorageId, TypeId};
use vaffle::{FuncBody, FuncDecl, Module, Value, ValueId};

use crate::common::canon_alias;
use crate::ir::apply_aliases_to_ir_terminator;
use crate::biir::apply_aliases_to_biir_terminator;

// ============================================================================
// Volar IR
// ============================================================================

type IrStoreCache = BTreeMap<(StorageId, TypeId, IRVarId), IRVarId>;

/// Apply store-to-load forwarding to `blocks`, propagating store caches across
/// block boundaries with param injection when needed.
///
/// For single-block programs this is equivalent to a within-block pass.
/// For multi-block programs an RPO traversal propagates outgoing caches to
/// successors via variable translation.  Single-predecessor blocks may receive
/// param injections to carry source values not already in their param list.
///
/// Returns `true` if any block was modified.
pub fn store_forward_ir_blocks<P: Clone + Default>(blocks: &mut IRBlocks<P>) -> bool {
    let n = blocks.blocks.len();
    if n == 0 {
        return false;
    }
    if n == 1 {
        let (changed, _) =
            store_forward_ir_block_with_cache(&mut blocks.blocks[0], BTreeMap::new());
        return changed;
    }

    // ── Build CFG ────────────────────────────────────────────────────────────
    let succs: Vec<Vec<usize>> = (0..n)
        .map(|i| ir_terminator_succ_blocks(&blocks.blocks[i].terminator))
        .collect();

    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, ss) in succs.iter().enumerate() {
        for &s in ss {
            if !preds[s].contains(&i) {
                preds[s].push(i);
            }
        }
    }

    let rpo = compute_rpo(n, 0, &succs);

    // ── RPO pass ─────────────────────────────────────────────────────────────
    let mut outgoing_caches: Vec<Option<IrStoreCache>> = vec![None; n];
    let mut any_changed = false;

    for &bi in &rpo {
        let incoming: IrStoreCache = if preds[bi].is_empty() {
            BTreeMap::new()
        } else if preds[bi].len() == 1 {
            // Single predecessor: translate with param injection.
            let pi = preds[bi][0];
            match &outgoing_caches[pi] {
                None => BTreeMap::new(),
                Some(pred_cache) => {
                    translate_ir_cache_with_injection(
                        pred_cache, pi, bi, blocks, &mut any_changed,
                    )
                }
            }
        } else {
            // Multiple predecessors: merge with param injection.
            merge_ir_caches_with_injection(
                &preds[bi], &outgoing_caches, blocks, bi, &mut any_changed,
            )
        };

        let (changed, out) =
            store_forward_ir_block_with_cache(&mut blocks.blocks[bi], incoming);
        any_changed |= changed;
        outgoing_caches[bi] = Some(out);
    }

    any_changed
}

/// Process a single IR block starting from `incoming` cache.
///
/// Returns `(changed, outgoing_cache)`.
fn store_forward_ir_block_with_cache<P: Clone + Default>(
    block: &mut IRBlock<P>,
    incoming: IrStoreCache,
) -> (bool, IrStoreCache) {
    let mut cache = incoming;
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    let mut changed = false;

    let base = block.params.len() as u32;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        if apply_aliases_to_ir_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        let stmt = block.stmts[i].clone();

        use volar_ir_common::Stmt;
        match &stmt {
            Stmt::StorageWrite { storage, src, ty, addr } => {
                let src = canon_alias(&alias_map, *src);
                let addr = canon_alias(&alias_map, *addr);
                cache.retain(|(s, t, _), _| !(s == storage && t == ty));
                cache.insert((*storage, *ty, addr), src);
            }
            Stmt::StorageRead { storage, ty, addr } => {
                let addr = canon_alias(&alias_map, *addr);
                let key = (*storage, *ty, addr);
                if let Some(&src) = cache.get(&key) {
                    alias_map.insert(rv, src);
                    changed = true;
                }
            }
            _ => {}
        }
    }

    changed |= apply_aliases_to_ir_terminator(&mut block.terminator, &alias_map);

    (changed, cache)
}

// ── Volar IR cross-block helpers ─────────────────────────────────────────────

/// Collect successor block indices from an `IRTerminator`.
fn ir_terminator_succ_blocks(term: &IRTerminator) -> Vec<usize> {
    let mut out = Vec::new();
    match term {
        IRTerminator::Jmp { func, .. } => {
            if let IRBlockTargetId::Block(b) = func {
                out.push(b.0 as usize);
            }
        }
        IRTerminator::JumpCond { true_block, false_block, .. } => {
            if let IRBlockTargetId::Block(b) = true_block {
                out.push(b.0 as usize);
            }
            if let IRBlockTargetId::Block(b) = false_block {
                if !out.contains(&(b.0 as usize)) {
                    out.push(b.0 as usize);
                }
            }
        }
        IRTerminator::JumpTable { cases, .. } => {
            for (_, (target, _)) in cases {
                if let IRBlockTargetId::Block(b) = target {
                    let idx = b.0 as usize;
                    if !out.contains(&idx) {
                        out.push(idx);
                    }
                }
            }
        }
    }
    out
}

/// Get the args list for a specific edge from `term` to `target_block`.
///
/// Returns the first matching edge's args.  For a JumpCond where both branches
/// go to the same block, only the first (true) edge is returned; the
/// intersection-based approach handles the second.
fn ir_edge_args(term: &IRTerminator, target_block: usize) -> Option<Vec<IRVarId>> {
    match term {
        IRTerminator::Jmp { func: IRBlockTargetId::Block(b), args }
            if b.0 as usize == target_block =>
        {
            Some(args.clone())
        }
        IRTerminator::JumpCond {
            true_block: IRBlockTargetId::Block(b), true_args, ..
        } if b.0 as usize == target_block => {
            Some(true_args.clone())
        }
        IRTerminator::JumpCond {
            false_block: IRBlockTargetId::Block(b), false_args, ..
        } if b.0 as usize == target_block => {
            Some(false_args.clone())
        }
        IRTerminator::JumpTable { cases, .. } => {
            for (_, (target, args)) in cases {
                if let IRBlockTargetId::Block(b) = target {
                    if b.0 as usize == target_block {
                        return Some(args.clone());
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Append `extra_args` to every edge in `term` that targets `target_block`.
fn add_ir_args_to_edges(term: &mut IRTerminator, target_block: usize, extra_args: &[IRVarId]) {
    match term {
        IRTerminator::Jmp { func: IRBlockTargetId::Block(b), args }
            if b.0 as usize == target_block =>
        {
            args.extend_from_slice(extra_args);
        }
        IRTerminator::JumpCond {
            true_block, true_args, false_block, false_args, ..
        } => {
            if let IRBlockTargetId::Block(b) = true_block {
                if b.0 as usize == target_block {
                    true_args.extend_from_slice(extra_args);
                }
            }
            if let IRBlockTargetId::Block(b) = false_block {
                if b.0 as usize == target_block {
                    false_args.extend_from_slice(extra_args);
                }
            }
        }
        IRTerminator::JumpTable { cases, .. } => {
            for (_, (target, args)) in cases.iter_mut() {
                if let IRBlockTargetId::Block(b) = target {
                    if b.0 as usize == target_block {
                        args.extend_from_slice(extra_args);
                    }
                }
            }
        }
        _ => {}
    }
}

/// Build an alias map that shifts all stmt-generated IRVarIds in a block.
///
/// Vars `0..old_base` (params) are unchanged.  Vars `old_base..old_base+n`
/// (stmts) are shifted to `old_base+shift..old_base+shift+n`.
// ── Direct shift helpers ─────────────────────────────────────────────────────
//
// When injecting `k` new params into a block, existing stmt IRVarIds must be
// shifted by `k`.  Previously this used `build_shift_map` to create a
// BTreeMap and then `apply_aliases_to_*` (which calls `canon_alias`) to apply
// it.  But `canon_alias` follows alias chains, so a shift map like
// `{v4→v5, v5→v6}` would chain `v4→v5→v6` instead of the intended `v4→v5`.
//
// These helpers apply the shift as a single-level transform, avoiding chaining.

/// Shift `v` in-place if it falls in `[old_base, old_base + n_stmts)`.
#[inline]
fn shift_var(v: &mut IRVarId, old_base: u32, n_stmts: u32, shift: u32) {
    if v.0 >= old_base && v.0 < old_base + n_stmts {
        v.0 += shift;
    }
}

/// Shift all `IRVarId` references in a `Stmt<IRVarId, IRVarId>`.
fn shift_ir_stmt_vars(
    stmt: &mut volar_ir_common::Stmt<IRVarId, IRVarId>,
    old_base: u32,
    n_stmts: u32,
    shift: u32,
) {
    use volar_ir_common::Stmt;
    match stmt {
        Stmt::Const(_, _) | Stmt::Rng { .. } => {}
        Stmt::Transmute { src, .. }
        | Stmt::Rol { src, .. }
        | Stmt::Ror { src, .. }
        | Stmt::Splat { src, .. } => {
            shift_var(src, old_base, n_stmts, shift);
        }
        Stmt::StorageRead { addr, .. } => {
            shift_var(addr, old_base, n_stmts, shift);
        }
        Stmt::StorageWrite { src, addr, .. } => {
            shift_var(src, old_base, n_stmts, shift);
            shift_var(addr, old_base, n_stmts, shift);
        }
        Stmt::Merge { parts, .. } => {
            for p in parts.iter_mut() {
                shift_var(p, old_base, n_stmts, shift);
            }
        }
        Stmt::Shuffle { result_bits, .. } => {
            for (_, v) in result_bits.iter_mut() {
                shift_var(v, old_base, n_stmts, shift);
            }
        }
        Stmt::Poly { coeffs, .. } => {
            // Shift vars inside monomial keys.  Since keys are sorted
            // `Vec<IRVarId>` in a BTreeMap, changing a var may change the
            // sort order, so we rebuild the map.
            let old = core::mem::take(coeffs);
            for (mut key, coeff) in old {
                for v in key.iter_mut() {
                    shift_var(v, old_base, n_stmts, shift);
                }
                key.sort();
                *coeffs.entry(key).or_insert(0) ^= coeff;
            }
            coeffs.retain(|_, c| *c & 1 != 0);
        }
        Stmt::OracleCall { args, .. } => {
            for a in args.iter_mut() {
                shift_var(a, old_base, n_stmts, shift);
            }
        }
        Stmt::OracleOutput { call, .. } | Stmt::ActionOutput { call, .. } => {
            shift_var(call, old_base, n_stmts, shift);
        }
        Stmt::ActionCall { guard, args, fallbacks, .. } => {
            shift_var(guard, old_base, n_stmts, shift);
            for a in args.iter_mut() {
                shift_var(a, old_base, n_stmts, shift);
            }
            for f in fallbacks.iter_mut() {
                shift_var(f, old_base, n_stmts, shift);
            }
        }
    }
}

/// Shift all `IRVarId` references in an `IRTerminator`.
fn shift_ir_terminator_vars(
    term: &mut IRTerminator,
    old_base: u32,
    n_stmts: u32,
    shift: u32,
) {
    let shift_args = |args: &mut Vec<IRVarId>| {
        for v in args.iter_mut() {
            shift_var(v, old_base, n_stmts, shift);
        }
    };
    let shift_target_id = |tid: &mut IRBlockTargetId| {
        if let IRBlockTargetId::Dyn(v) = tid {
            shift_var(v, old_base, n_stmts, shift);
        }
    };
    match term {
        IRTerminator::Jmp { func, args } => {
            shift_target_id(func);
            shift_args(args);
        }
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            shift_var(condition, old_base, n_stmts, shift);
            shift_target_id(true_block);
            shift_args(true_args);
            shift_target_id(false_block);
            shift_args(false_args);
        }
        IRTerminator::JumpTable { index, cases } => {
            shift_var(index, old_base, n_stmts, shift);
            for (_, (target, args)) in cases.iter_mut() {
                shift_target_id(target);
                shift_args(args);
            }
        }
    }
}

/// Shift all `IRVarId` references in a `BIrStmt`.
fn shift_biir_stmt_vars(
    stmt: &mut BIrStmt,
    old_base: u32,
    n_stmts: u32,
    shift: u32,
) {
    match stmt {
        BIrStmt::And(a, b) | BIrStmt::Or(a, b) | BIrStmt::Xor(a, b) => {
            shift_var(a, old_base, n_stmts, shift);
            shift_var(b, old_base, n_stmts, shift);
        }
        BIrStmt::Not(a) => {
            shift_var(a, old_base, n_stmts, shift);
        }
        BIrStmt::StorageRead { addr, .. } => {
            for v in addr.iter_mut() {
                shift_var(v, old_base, n_stmts, shift);
            }
        }
        BIrStmt::StorageWrite { src, addr, .. } => {
            shift_var(src, old_base, n_stmts, shift);
            for v in addr.iter_mut() {
                shift_var(v, old_base, n_stmts, shift);
            }
        }
        BIrStmt::OracleCall { args, .. } => {
            for a in args.iter_mut() {
                shift_var(a, old_base, n_stmts, shift);
            }
        }
        BIrStmt::OracleBit { call, .. } | BIrStmt::ActionBit { call, .. } => {
            shift_var(call, old_base, n_stmts, shift);
        }
        BIrStmt::ActionCall { guard, args, fallback, .. } => {
            shift_var(guard, old_base, n_stmts, shift);
            for a in args.iter_mut() {
                shift_var(a, old_base, n_stmts, shift);
            }
            for f in fallback.iter_mut() {
                shift_var(f, old_base, n_stmts, shift);
            }
        }
        // Zero, One, Rng: no var references.
        _ => {}
    }
}

/// Shift all `IRVarId` references in a `BIrTarget`.
fn shift_biir_target_vars(
    target: &mut BIrTarget,
    old_base: u32,
    n_stmts: u32,
    shift: u32,
) {
    for v in target.args.iter_mut() {
        shift_var(v, old_base, n_stmts, shift);
    }
    if let IRBlockTargetId::Dyn(v) = &mut target.block {
        shift_var(v, old_base, n_stmts, shift);
    }
}

/// Shift all `IRVarId` references in a `BIrTerminator`.
fn shift_biir_terminator_vars(
    term: &mut BIrTerminator,
    old_base: u32,
    n_stmts: u32,
    shift: u32,
) {
    match term {
        BIrTerminator::Jmp(target) => {
            shift_biir_target_vars(target, old_base, n_stmts, shift);
        }
        BIrTerminator::CondJmp { val, then_target, else_target } => {
            shift_var(val, old_base, n_stmts, shift);
            shift_biir_target_vars(then_target, old_base, n_stmts, shift);
            shift_biir_target_vars(else_target, old_base, n_stmts, shift);
        }
    }
}

/// Translate a predecessor's outgoing cache into the successor's var space,
/// injecting params when the source var is not already passed as an arg.
///
/// Only used for single-predecessor blocks.
fn translate_ir_cache_with_injection<P: Clone + Default>(
    pred_cache: &IrStoreCache,
    pred_idx: usize,
    target_idx: usize,
    blocks: &mut IRBlocks<P>,
    changed: &mut bool,
) -> IrStoreCache {
    let args = match ir_edge_args(&blocks.blocks[pred_idx].terminator, target_idx) {
        Some(a) => a,
        None => return BTreeMap::new(),
    };

    // Build translation: pred_var → target_param.
    let mut trans: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    for (i, &arg) in args.iter().enumerate() {
        trans.entry(arg).or_insert(IRVarId(i as u32));
    }

    let old_n_params = blocks.blocks[target_idx].params.len() as u32;
    let mut result: IrStoreCache = BTreeMap::new();
    let mut injections: Vec<(IRVarId, TypeId)> = Vec::new(); // (pred src var, type)

    for (&(s, t, addr_p), &src_p) in pred_cache {
        if let Some(&addr_b) = trans.get(&addr_p) {
            if let Some(&src_b) = trans.get(&src_p) {
                // Both translate — no injection needed.
                result.insert((s, t, addr_b), src_b);
            } else {
                // src not passed — inject a new param.
                let new_idx = old_n_params + injections.len() as u32;
                let src_b = IRVarId(new_idx);
                result.insert((s, t, addr_b), src_b);
                trans.insert(src_p, src_b);
                injections.push((src_p, t));
            }
        }
        // If addr_p doesn't translate, skip — can't forward.
    }

    if !injections.is_empty() {
        let shift = injections.len() as u32;
        let n_stmts = blocks.blocks[target_idx].stmts.len();

        // Add param types to the target block.
        for &(_, type_id) in &injections {
            blocks.blocks[target_idx].params.push(type_id);
        }

        // Shift existing stmt IRVarIds (stmts and terminator).
        let n_stmts_u32 = n_stmts as u32;
        for stmt in blocks.blocks[target_idx].stmts.iter_mut() {
            shift_ir_stmt_vars(stmt, old_n_params, n_stmts_u32, shift);
        }
        shift_ir_terminator_vars(&mut blocks.blocks[target_idx].terminator, old_n_params, n_stmts_u32, shift);

        // Add provenance entries for new params (stmts provs are separate;
        // params don't need provenance, but stmt_provs length must still
        // match stmts length — which it does since we didn't add stmts).

        // Add source vars as extra args to every edge from predecessor to target.
        let extra_args: Vec<IRVarId> = injections.iter().map(|&(v, _)| v).collect();
        add_ir_args_to_edges(&mut blocks.blocks[pred_idx].terminator, target_idx, &extra_args);

        *changed = true;
    }

    result
}

/// Merge multiple predecessors' caches into the successor's var space, with
/// param injection for source values that differ across predecessors.
///
/// Forwards entries where ALL predecessors have a cached write to the same
/// `(StorageId, TypeId, addr_in_target)`.  If every predecessor's source var
/// translates to the same target param, no injection is needed.  When sources
/// differ (the "phi" case), a new parameter is injected and each predecessor
/// passes its own source value as the argument for that param position.
fn merge_ir_caches_with_injection<P: Clone + Default>(
    pred_indices: &[usize],
    outgoing_caches: &[Option<IrStoreCache>],
    blocks: &mut IRBlocks<P>,
    target_idx: usize,
    changed: &mut bool,
) -> IrStoreCache {
    let n_preds = pred_indices.len();

    // ── Phase 1: Build translation maps and translate each pred's cache ──────
    //
    // For each predecessor we translate the cache entries' *addr* to the target
    // block's parameter namespace (via existing edge args) and keep *src* in
    // the predecessor's own namespace — we may need to inject it later.
    //
    // translated[pred_pos] maps (S, T, addr_target) → src_in_pred_namespace
    // trans_maps[pred_pos] maps pred_var → target_param

    let mut trans_maps: Vec<BTreeMap<IRVarId, IRVarId>> = Vec::with_capacity(n_preds);
    let mut translated: Vec<BTreeMap<(StorageId, TypeId, IRVarId), IRVarId>> =
        Vec::with_capacity(n_preds);

    for &pi in pred_indices {
        let pred_cache = match &outgoing_caches[pi] {
            Some(c) => c,
            None => return BTreeMap::new(), // predecessor not yet processed
        };

        let args = match ir_edge_args(&blocks.blocks[pi].terminator, target_idx) {
            Some(a) => a,
            None => return BTreeMap::new(),
        };

        let mut trans: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
        for (i, &arg) in args.iter().enumerate() {
            trans.entry(arg).or_insert(IRVarId(i as u32));
        }

        let entries: BTreeMap<_, _> = pred_cache
            .iter()
            .filter_map(|(&(s, t, addr_p), &src_p)| {
                let addr_t = *trans.get(&addr_p)?;
                Some(((s, t, addr_t), src_p))
            })
            .collect();

        trans_maps.push(trans);
        translated.push(entries);
    }

    // ── Phase 2: Find keys common to ALL predecessors ────────────────────────

    let common_keys: Vec<(StorageId, TypeId, IRVarId)> = if translated.is_empty() {
        Vec::new()
    } else {
        translated[0]
            .keys()
            .filter(|k| translated[1..].iter().all(|te| te.contains_key(k)))
            .cloned()
            .collect()
    };

    if common_keys.is_empty() {
        return BTreeMap::new();
    }

    // ── Phase 3: Determine which entries need injection ──────────────────────

    let old_n_params = blocks.blocks[target_idx].params.len() as u32;
    let mut result: IrStoreCache = BTreeMap::new();
    // Each injection: (TypeId for the new param, [src_in_pred_namespace per pred])
    let mut injections: Vec<(TypeId, Vec<IRVarId>)> = Vec::new();

    for (s, t, addr_t) in common_keys {
        // Collect each predecessor's source var (in its own namespace).
        let src_per_pred: Vec<IRVarId> = (0..n_preds)
            .map(|pp| translated[pp][&(s, t, addr_t)])
            .collect();

        // Try to translate all source vars to target namespace.
        let translated_srcs: Vec<Option<IRVarId>> = src_per_pred
            .iter()
            .enumerate()
            .map(|(pp, &src_p)| trans_maps[pp].get(&src_p).copied())
            .collect();

        // If all translate to the same target var, no injection needed.
        let all_same = translated_srcs.iter().all(|s| s.is_some())
            && translated_srcs.windows(2).all(|w| w[0] == w[1]);

        if all_same {
            result.insert((s, t, addr_t), translated_srcs[0].unwrap());
        } else {
            // Inject a new param — each predecessor will pass its own src value.
            let new_param_idx = old_n_params + injections.len() as u32;
            result.insert((s, t, addr_t), IRVarId(new_param_idx));
            injections.push((t, src_per_pred));
        }
    }

    // ── Phase 4: Apply injections ────────────────────────────────────────────

    if !injections.is_empty() {
        let shift = injections.len() as u32;
        let n_stmts = blocks.blocks[target_idx].stmts.len();

        // Add param types to the target block.
        for &(type_id, _) in &injections {
            blocks.blocks[target_idx].params.push(type_id);
        }

        // Shift existing stmt IRVarIds (stmts + terminator).
        let n_stmts_u32 = n_stmts as u32;
        for stmt in blocks.blocks[target_idx].stmts.iter_mut() {
            shift_ir_stmt_vars(stmt, old_n_params, n_stmts_u32, shift);
        }
        shift_ir_terminator_vars(
            &mut blocks.blocks[target_idx].terminator,
            old_n_params,
            n_stmts_u32,
            shift,
        );

        // Extend each predecessor's edges with the injected source vars.
        for (pp, &pi) in pred_indices.iter().enumerate() {
            let extra_args: Vec<IRVarId> = injections
                .iter()
                .map(|(_, srcs)| srcs[pp])
                .collect();
            add_ir_args_to_edges(
                &mut blocks.blocks[pi].terminator,
                target_idx,
                &extra_args,
            );
        }

        *changed = true;
    }

    result
}

// ============================================================================
// Boolar IR
// ============================================================================

type BiirStoreCache = BTreeMap<(StorageId, usize, Vec<IRVarId>), IRVarId>;

/// Apply store-to-load forwarding to `blocks`, propagating store caches across
/// block boundaries with param injection when needed.
///
/// Returns `true` if any block was modified.
pub fn store_forward_biir_blocks<P: Clone + Default>(blocks: &mut BIrBlocks<P>) -> bool {
    let n = blocks.blocks.len();
    if n == 0 {
        return false;
    }
    if n == 1 {
        let (changed, _) =
            store_forward_biir_block_with_cache(&mut blocks.blocks[0], BTreeMap::new());
        return changed;
    }

    // ── Build CFG ────────────────────────────────────────────────────────────
    let succs: Vec<Vec<usize>> = (0..n)
        .map(|i| biir_terminator_succ_blocks(&blocks.blocks[i].terminator))
        .collect();

    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, ss) in succs.iter().enumerate() {
        for &s in ss {
            if !preds[s].contains(&i) {
                preds[s].push(i);
            }
        }
    }

    let rpo = compute_rpo(n, 0, &succs);

    // ── RPO pass ─────────────────────────────────────────────────────────────
    let mut outgoing_caches: Vec<Option<BiirStoreCache>> = vec![None; n];
    let mut any_changed = false;

    for &bi in &rpo {
        let incoming: BiirStoreCache = if preds[bi].is_empty() {
            BTreeMap::new()
        } else if preds[bi].len() == 1 {
            let pi = preds[bi][0];
            match &outgoing_caches[pi] {
                None => BTreeMap::new(),
                Some(pred_cache) => {
                    translate_biir_cache_with_injection(
                        pred_cache, pi, bi, blocks, &mut any_changed,
                    )
                }
            }
        } else {
            // Multiple predecessors: merge with param injection.
            merge_biir_caches_with_injection(
                &preds[bi], &outgoing_caches, blocks, bi, &mut any_changed,
            )
        };

        let (changed, out) =
            store_forward_biir_block_with_cache(&mut blocks.blocks[bi], incoming);
        any_changed |= changed;
        outgoing_caches[bi] = Some(out);
    }

    any_changed
}

/// Process a single BIR block starting from `incoming` cache.
///
/// Returns `(changed, outgoing_cache)`.
fn store_forward_biir_block_with_cache<P: Clone + Default>(
    block: &mut BIrBlock<P>,
    incoming: BiirStoreCache,
) -> (bool, BiirStoreCache) {
    let mut cache = incoming;
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    let mut changed = false;

    let base = block.params;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        if crate::biir::apply_aliases_to_biir_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        let stmt = block.stmts[i].clone();

        match &stmt {
            BIrStmt::StorageWrite { storage, src, bit_width, addr } => {
                let src = canon_alias(&alias_map, *src);
                let addr: Vec<IRVarId> = addr.iter().map(|v| canon_alias(&alias_map, *v)).collect();
                cache.retain(|(s, w, _), _| !(s == storage && w == bit_width));
                cache.insert((*storage, *bit_width, addr), src);
            }
            BIrStmt::StorageRead { storage, bit_width, addr } => {
                let addr: Vec<IRVarId> = addr.iter().map(|v| canon_alias(&alias_map, *v)).collect();
                let key = (*storage, *bit_width, addr);
                if let Some(&src) = cache.get(&key) {
                    alias_map.insert(rv, src);
                    changed = true;
                }
            }
            _ => {}
        }
    }

    changed |= apply_aliases_to_biir_terminator(&mut block.terminator, &alias_map);

    (changed, cache)
}

// ── BIR cross-block helpers ──────────────────────────────────────────────────

/// Collect successor block indices from a `BIrTerminator`.
fn biir_terminator_succ_blocks(term: &BIrTerminator) -> Vec<usize> {
    let mut out = Vec::new();
    match term {
        BIrTerminator::Jmp(target) => {
            if let IRBlockTargetId::Block(b) = &target.block {
                out.push(b.0 as usize);
            }
        }
        BIrTerminator::CondJmp { then_target, else_target, .. } => {
            if let IRBlockTargetId::Block(b) = &then_target.block {
                out.push(b.0 as usize);
            }
            if let IRBlockTargetId::Block(b) = &else_target.block {
                let idx = b.0 as usize;
                if !out.contains(&idx) {
                    out.push(idx);
                }
            }
        }
    }
    out
}

/// Get the args list for a specific edge from `term` to `target_block`.
fn biir_edge_args(term: &BIrTerminator, target_block: usize) -> Option<Vec<IRVarId>> {
    match term {
        BIrTerminator::Jmp(target) => {
            if let IRBlockTargetId::Block(b) = &target.block {
                if b.0 as usize == target_block {
                    return Some(target.args.clone());
                }
            }
            None
        }
        BIrTerminator::CondJmp { then_target, else_target, .. } => {
            if let IRBlockTargetId::Block(b) = &then_target.block {
                if b.0 as usize == target_block {
                    return Some(then_target.args.clone());
                }
            }
            if let IRBlockTargetId::Block(b) = &else_target.block {
                if b.0 as usize == target_block {
                    return Some(else_target.args.clone());
                }
            }
            None
        }
    }
}

/// Append `extra_args` to every edge in `term` that targets `target_block`.
fn add_biir_args_to_edges(
    term: &mut BIrTerminator,
    target_block: usize,
    extra_args: &[IRVarId],
) {
    match term {
        BIrTerminator::Jmp(target) => {
            if let IRBlockTargetId::Block(b) = &target.block {
                if b.0 as usize == target_block {
                    target.args.extend_from_slice(extra_args);
                }
            }
        }
        BIrTerminator::CondJmp { then_target, else_target, .. } => {
            if let IRBlockTargetId::Block(b) = &then_target.block {
                if b.0 as usize == target_block {
                    then_target.args.extend_from_slice(extra_args);
                }
            }
            if let IRBlockTargetId::Block(b) = &else_target.block {
                if b.0 as usize == target_block {
                    else_target.args.extend_from_slice(extra_args);
                }
            }
        }
    }
}

/// Translate a predecessor's outgoing cache into the successor's var space,
/// injecting params when the source var is not already passed as an arg.
///
/// Only used for single-predecessor BIR blocks.
fn translate_biir_cache_with_injection<P: Clone + Default>(
    pred_cache: &BiirStoreCache,
    pred_idx: usize,
    target_idx: usize,
    blocks: &mut BIrBlocks<P>,
    changed: &mut bool,
) -> BiirStoreCache {
    let args = match biir_edge_args(&blocks.blocks[pred_idx].terminator, target_idx) {
        Some(a) => a,
        None => return BTreeMap::new(),
    };

    let mut trans: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    for (i, &arg) in args.iter().enumerate() {
        trans.entry(arg).or_insert(IRVarId(i as u32));
    }

    let old_n_params = blocks.blocks[target_idx].params;
    let mut result: BiirStoreCache = BTreeMap::new();
    let mut injections: Vec<IRVarId> = Vec::new(); // pred src vars to inject

    for ((s, w, addr_p), &src_p) in pred_cache {
        // Translate each bit of the address vec; skip entry if any bit is untranslatable.
        let addr_b: Vec<IRVarId> = match addr_p.iter().map(|v| trans.get(v).copied()).collect::<Option<Vec<_>>>() {
            Some(a) => a,
            None => continue,
        };
        if let Some(&src_b) = trans.get(&src_p) {
            result.insert((*s, *w, addr_b), src_b);
        } else {
            let new_idx = old_n_params + injections.len() as u32;
            let src_b = IRVarId(new_idx);
            result.insert((*s, *w, addr_b), src_b);
            trans.insert(src_p, src_b);
            injections.push(src_p);
        }
    }

    if !injections.is_empty() {
        let shift = injections.len() as u32;
        let n_stmts = blocks.blocks[target_idx].stmts.len();

        // Increment param count.
        blocks.blocks[target_idx].params += shift;

        // Shift existing stmt IRVarIds.
        let n_stmts_u32 = n_stmts as u32;
        for stmt in blocks.blocks[target_idx].stmts.iter_mut() {
            shift_biir_stmt_vars(stmt, old_n_params, n_stmts_u32, shift);
        }
        shift_biir_terminator_vars(&mut blocks.blocks[target_idx].terminator, old_n_params, n_stmts_u32, shift);

        // Add source vars as extra args to predecessor edges.
        add_biir_args_to_edges(
            &mut blocks.blocks[pred_idx].terminator,
            target_idx,
            &injections,
        );

        *changed = true;
    }

    result
}

/// Merge multiple predecessors' BIR caches into the successor's var space,
/// with param injection for source values that differ across predecessors.
fn merge_biir_caches_with_injection<P: Clone + Default>(
    pred_indices: &[usize],
    outgoing_caches: &[Option<BiirStoreCache>],
    blocks: &mut BIrBlocks<P>,
    target_idx: usize,
    changed: &mut bool,
) -> BiirStoreCache {
    let n_preds = pred_indices.len();

    let mut trans_maps: Vec<BTreeMap<IRVarId, IRVarId>> = Vec::with_capacity(n_preds);
    let mut translated: Vec<BTreeMap<(StorageId, usize, Vec<IRVarId>), IRVarId>> =
        Vec::with_capacity(n_preds);

    for &pi in pred_indices {
        let pred_cache = match &outgoing_caches[pi] {
            Some(c) => c,
            None => return BTreeMap::new(),
        };

        let args = match biir_edge_args(&blocks.blocks[pi].terminator, target_idx) {
            Some(a) => a,
            None => return BTreeMap::new(),
        };

        let mut trans: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
        for (i, &arg) in args.iter().enumerate() {
            trans.entry(arg).or_insert(IRVarId(i as u32));
        }

        let entries: BTreeMap<_, _> = pred_cache
            .iter()
            .filter_map(|((s, w, addr_p), &src_p)| {
                let addr_t: Vec<IRVarId> = addr_p.iter().map(|v| trans.get(v).copied()).collect::<Option<Vec<_>>>()?;
                Some(((*s, *w, addr_t), src_p))
            })
            .collect();

        trans_maps.push(trans);
        translated.push(entries);
    }

    let common_keys: Vec<(StorageId, usize, Vec<IRVarId>)> = if translated.is_empty() {
        Vec::new()
    } else {
        translated[0]
            .keys()
            .filter(|k| translated[1..].iter().all(|te| te.contains_key(k)))
            .cloned()
            .collect()
    };

    if common_keys.is_empty() {
        return BTreeMap::new();
    }

    let old_n_params = blocks.blocks[target_idx].params;
    let mut result: BiirStoreCache = BTreeMap::new();
    // Each injection: [src_in_pred_namespace per pred]
    let mut injections: Vec<Vec<IRVarId>> = Vec::new();

    for (s, w, addr_t) in common_keys {
        let key = (s, w, addr_t);
        let src_per_pred: Vec<IRVarId> = (0..n_preds)
            .map(|pp| translated[pp][&key])
            .collect();

        let translated_srcs: Vec<Option<IRVarId>> = src_per_pred
            .iter()
            .enumerate()
            .map(|(pp, &src_p)| trans_maps[pp].get(&src_p).copied())
            .collect();

        let all_same = translated_srcs.iter().all(|s| s.is_some())
            && translated_srcs.windows(2).all(|w| w[0] == w[1]);

        if all_same {
            result.insert(key, translated_srcs[0].unwrap());
        } else {
            let new_param_idx = old_n_params + injections.len() as u32;
            result.insert(key, IRVarId(new_param_idx));
            injections.push(src_per_pred);
        }
    }

    if !injections.is_empty() {
        let shift = injections.len() as u32;
        let n_stmts = blocks.blocks[target_idx].stmts.len();

        // Increment param count.
        blocks.blocks[target_idx].params += shift;

        // Shift existing stmt IRVarIds.
        let n_stmts_u32 = n_stmts as u32;
        for stmt in blocks.blocks[target_idx].stmts.iter_mut() {
            shift_biir_stmt_vars(stmt, old_n_params, n_stmts_u32, shift);
        }
        shift_biir_terminator_vars(&mut blocks.blocks[target_idx].terminator, old_n_params, n_stmts_u32, shift);

        // Extend each predecessor's edges.
        for (pp, &pi) in pred_indices.iter().enumerate() {
            let extra_args: Vec<IRVarId> = injections
                .iter()
                .map(|srcs| srcs[pp])
                .collect();
            add_biir_args_to_edges(
                &mut blocks.blocks[pi].terminator,
                target_idx,
                &extra_args,
            );
        }

        *changed = true;
    }

    result
}

// ============================================================================
// VAFFLE
// ============================================================================

/// Apply store-to-load forwarding to all function bodies in `module`.
///
/// Returns `true` if any body was modified.
pub fn store_forward_vaffle_module(module: &mut Module) -> bool {
    let mut any_changed = false;
    for func in module.funcs.iter_mut() {
        if let FuncDecl::Body(body) = func {
            any_changed |= store_forward_vaffle_body(body);
        }
    }
    any_changed
}

/// Cache type: (StorageId, TypeId, addr_var) → src_var.
type VaffleCache = BTreeMap<(StorageId, TypeId, ValueId), ValueId>;

/// Process a single body with an RPO pass that propagates store caches across
/// block boundaries.
///
/// For single-block bodies this degenerates to the original within-block pass.
/// For multi-block bodies, values written in a dominating block and read in a
/// dominated block are forwarded directly (VAFFLE `ValueId`s are global, so the
/// cross-block source is accessible in the successor's `value_table`).
fn store_forward_vaffle_body(body: &mut FuncBody) -> bool {
    let n = body.blocks.len();

    if n == 1 {
        // Fast path: no CFG needed.
        let (changed, _) =
            store_forward_vaffle_block_with_cache(body, 0, BTreeMap::new());
        return changed;
    }

    // Build successor / predecessor maps from terminators.
    let mut succs: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, block) in body.blocks.iter().enumerate() {
        for s in vaffle_block_succs(&block.terminator) {
            succs[i].push(s);
            preds[s].push(i);
        }
    }

    let entry = body.entry.0;
    let rpo = compute_rpo(n, entry, &succs);

    let mut outgoing_caches: Vec<Option<VaffleCache>> = vec![None; n];
    let mut any_changed = false;

    for &block_idx in &rpo {
        // Intersect predecessor outgoing caches to build the incoming cache.
        // For blocks with no processed predecessors (entry or unreachable)
        // the incoming cache is empty, giving conservative behaviour.
        let incoming: VaffleCache = if preds[block_idx].is_empty() {
            BTreeMap::new()
        } else {
            intersect_vaffle_caches(&preds[block_idx], &outgoing_caches)
        };

        let (changed, outgoing) =
            store_forward_vaffle_block_with_cache(body, block_idx, incoming);
        any_changed |= changed;
        outgoing_caches[block_idx] = Some(outgoing);
    }

    any_changed
}

/// Compute the RPO (reverse post-order) traversal of the CFG.
fn compute_rpo(n: usize, entry: usize, succs: &[Vec<usize>]) -> Vec<usize> {
    let mut visited = vec![false; n];
    let mut post_order: Vec<usize> = Vec::with_capacity(n);
    dfs_post(entry, succs, &mut visited, &mut post_order);
    post_order.reverse();
    post_order
}

fn dfs_post(
    node: usize,
    succs: &[Vec<usize>],
    visited: &mut Vec<bool>,
    post: &mut Vec<usize>,
) {
    if visited[node] {
        return;
    }
    visited[node] = true;
    for &s in &succs[node] {
        dfs_post(s, succs, visited, post);
    }
    post.push(node);
}

/// Collect the block indices that `term` jumps to.
fn vaffle_block_succs(term: &vaffle::Terminator) -> Vec<usize> {
    use vaffle::Terminator;
    match term {
        Terminator::Return { .. } | Terminator::ReturnCall { .. } => Vec::new(),
        Terminator::Jump(t) => alloc::vec![t.block.0],
        Terminator::IfNonzero { then_target, else_target, .. } => {
            alloc::vec![then_target.block.0, else_target.block.0]
        }
        Terminator::Table { targets, default_target, .. } => {
            let mut v: Vec<usize> = targets.iter().map(|t| t.block.0).collect();
            v.push(default_target.block.0);
            v
        }
    }
}

/// Intersect multiple predecessor outgoing caches: keep only entries where
/// every predecessor agrees on the same source `ValueId`.
fn intersect_vaffle_caches(
    pred_indices: &[usize],
    outgoing: &[Option<VaffleCache>],
) -> VaffleCache {
    if pred_indices.is_empty() {
        return BTreeMap::new();
    }
    // Start with first predecessor's cache (clone required for intersection).
    let mut acc: VaffleCache = match &outgoing[pred_indices[0]] {
        Some(c) => c.clone(),
        None => return BTreeMap::new(),
    };
    // Intersect with every other predecessor.
    for &pi in &pred_indices[1..] {
        match &outgoing[pi] {
            Some(c) => acc.retain(|k, v| c.get(k) == Some(v)),
            None => {
                acc.clear();
                break;
            }
        }
    }
    acc
}

/// Extracted, `Copy`-only fields from a VAFFLE `StorageWrite` or `StorageRead`.
enum VaffleStoreAction {
    Write { storage: StorageId, src: ValueId, ty: TypeId, addr: ValueId },
    Read  { storage: StorageId, ty: TypeId, addr: ValueId },
}

/// Forward stores in a single block starting from `incoming` cache.
///
/// Returns `(changed, outgoing_cache)`.  The `outgoing_cache` reflects all
/// writes that survive to the end of the block and can be propagated to
/// successors.
fn store_forward_vaffle_block_with_cache(
    body: &mut FuncBody,
    block_idx: usize,
    incoming: VaffleCache,
) -> (bool, VaffleCache) {
    let mut cache = incoming;
    let mut alias_map: BTreeMap<ValueId, ValueId> = BTreeMap::new();
    let mut changed = false;

    let stmt_vids: Vec<ValueId> = body.blocks[block_idx].stmts.clone();

    for vid in stmt_vids {
        // Apply existing aliases to this value's operands.
        if apply_aliases_to_vaffle_value(&mut body.values[vid.0], &alias_map) {
            changed = true;
        }

        // Extract only the Copy fields we need — Value doesn't implement Clone.
        use volar_ir_common::Stmt;
        let action: Option<VaffleStoreAction> = match &body.values[vid.0] {
            Value::Op(Stmt::StorageWrite { storage, src, ty, addr }) => {
                Some(VaffleStoreAction::Write {
                    storage: *storage,
                    src: *src,
                    ty: *ty,
                    addr: *addr,
                })
            }
            Value::Op(Stmt::StorageRead { storage, ty, addr }) => {
                Some(VaffleStoreAction::Read {
                    storage: *storage,
                    ty: *ty,
                    addr: *addr,
                })
            }
            _ => None,
        };

        match action {
            Some(VaffleStoreAction::Write { storage, src, ty, addr }) => {
                let src = canon_alias(&alias_map, src);
                let addr = canon_alias(&alias_map, addr);
                cache.retain(|(s, t, _), _| !(s == &storage && t == &ty));
                cache.insert((storage, ty, addr), src);
            }
            Some(VaffleStoreAction::Read { storage, ty, addr }) => {
                let addr = canon_alias(&alias_map, addr);
                let key = (storage, ty, addr);
                if let Some(&src) = cache.get(&key) {
                    // Forward: downstream uses of `vid` become uses of `src`.
                    // Because VAFFLE ValueIds are global and `value_table`
                    // accumulates across blocks, `src` is always accessible
                    // in the current block even if it was defined earlier.
                    alias_map.insert(vid, src);
                    changed = true;
                }
            }
            None => {}
        }
    }

    // Rewrite the block terminator through alias_map.
    if !alias_map.is_empty() {
        changed |= apply_aliases_to_vaffle_terminator(
            &mut body.blocks[block_idx].terminator,
            &alias_map,
        );
    }

    (changed, cache)
}

// ============================================================================
// Alias application helpers (Volar IR stmt)
// ============================================================================

fn apply_aliases_to_ir_stmt(
    stmt: &mut volar_ir_common::Stmt<IRVarId, IRVarId>,
    alias_map: &BTreeMap<IRVarId, IRVarId>,
) -> bool {
    crate::common::apply_aliases_to_stmt(stmt, alias_map)
}

// ============================================================================
// Alias application helpers (VAFFLE)
// ============================================================================

fn apply_aliases_to_vaffle_value(
    value: &mut Value,
    alias_map: &BTreeMap<ValueId, ValueId>,
) -> bool {
    if alias_map.is_empty() {
        return false;
    }
    use volar_ir_common::Stmt;
    let stmt = match value {
        Value::Op(s) => s,
        _ => return false,
    };
    apply_aliases_to_vaffle_stmt(stmt, alias_map)
}

fn apply_aliases_to_vaffle_stmt(
    stmt: &mut volar_ir_common::Stmt<ValueId>,
    alias_map: &BTreeMap<ValueId, ValueId>,
) -> bool {
    use volar_ir_common::Stmt;
    let mut changed = false;
    match stmt {
        Stmt::StorageRead { addr, .. } => {
            let c = canon_alias(alias_map, *addr);
            if c != *addr { *addr = c; changed = true; }
        }
        Stmt::StorageWrite { src, addr, .. } => {
            let cs = canon_alias(alias_map, *src);
            if cs != *src { *src = cs; changed = true; }
            let ca = canon_alias(alias_map, *addr);
            if ca != *addr { *addr = ca; changed = true; }
        }
        Stmt::Poly { coeffs, .. } => {
            let old = core::mem::take(coeffs);
            for (key, coeff) in old {
                if coeff & 1 == 0 {
                    changed = true;
                    continue;
                }
                let new_key: Vec<ValueId> = key
                    .iter()
                    .map(|&v| {
                        let w = canon_alias(alias_map, v);
                        if w != v { changed = true; }
                        w
                    })
                    .collect();
                *coeffs.entry(new_key).or_insert(0) ^= coeff;
            }
            coeffs.retain(|_, c| *c & 1 != 0);
        }
        Stmt::Transmute { src, .. }
        | Stmt::Rol { src, .. }
        | Stmt::Ror { src, .. }
        | Stmt::Splat { src, .. } => {
            let c = canon_alias(alias_map, *src);
            if c != *src { *src = c; changed = true; }
        }
        Stmt::Merge { parts, .. } => {
            for p in parts.iter_mut() {
                let c = canon_alias(alias_map, *p);
                if c != *p { *p = c; changed = true; }
            }
        }
        Stmt::Shuffle { result_bits, .. } => {
            for (_, v) in result_bits.iter_mut() {
                let c = canon_alias(alias_map, *v);
                if c != *v { *v = c; changed = true; }
            }
        }
        Stmt::OracleCall { args, .. } => {
            for a in args.iter_mut() {
                let c = canon_alias(alias_map, *a);
                if c != *a { *a = c; changed = true; }
            }
        }
        Stmt::OracleOutput { call, .. } => {
            let c = canon_alias(alias_map, *call);
            if c != *call { *call = c; changed = true; }
        }
        Stmt::ActionCall { guard, args, fallbacks, .. } => {
            let cg = canon_alias(alias_map, *guard);
            if cg != *guard { *guard = cg; changed = true; }
            for a in args.iter_mut() {
                let c = canon_alias(alias_map, *a);
                if c != *a { *a = c; changed = true; }
            }
            for f in fallbacks.iter_mut() {
                let c = canon_alias(alias_map, *f);
                if c != *f { *f = c; changed = true; }
            }
        }
        Stmt::ActionOutput { call, .. } => {
            let c = canon_alias(alias_map, *call);
            if c != *call { *call = c; changed = true; }
        }
        Stmt::Const(_, _) | Stmt::Rng { .. } => {}
    }
    changed
}

fn apply_aliases_to_vaffle_terminator(
    term: &mut vaffle::Terminator,
    alias_map: &BTreeMap<ValueId, ValueId>,
) -> bool {
    use vaffle::Terminator;
    let mut changed = false;
    match term {
        Terminator::Return { values } => {
            for v in values.iter_mut() {
                let c = canon_alias(alias_map, *v);
                if c != *v { *v = c; changed = true; }
            }
        }
        Terminator::Jump(target) => {
            changed |= apply_aliases_to_vaffle_target(target, alias_map);
        }
        Terminator::IfNonzero { cond, then_target, else_target } => {
            let c = canon_alias(alias_map, *cond);
            if c != *cond { *cond = c; changed = true; }
            changed |= apply_aliases_to_vaffle_target(then_target, alias_map);
            changed |= apply_aliases_to_vaffle_target(else_target, alias_map);
        }
        Terminator::Table { index, targets, default_target } => {
            let c = canon_alias(alias_map, *index);
            if c != *index { *index = c; changed = true; }
            for t in targets.iter_mut() {
                changed |= apply_aliases_to_vaffle_target(t, alias_map);
            }
            changed |= apply_aliases_to_vaffle_target(default_target, alias_map);
        }
        _ => {}
    }
    changed
}

fn apply_aliases_to_vaffle_target(
    target: &mut vaffle::Target,
    alias_map: &BTreeMap<ValueId, ValueId>,
) -> bool {
    let mut changed = false;
    for v in target.args.iter_mut() {
        let c = canon_alias(alias_map, *v);
        if c != *v { *v = c; changed = true; }
    }
    changed
}
