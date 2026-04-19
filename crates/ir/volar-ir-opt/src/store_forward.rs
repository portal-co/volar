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
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt};
use volar_ir::ir::{IRBlock, IRBlocks, IRVarId};
use volar_ir_common::{StorageId, TypeId};
use vaffle::{FuncBody, FuncDecl, Module, Value, ValueId};

use crate::common::canon_alias;
use crate::ir::apply_aliases_to_ir_terminator;
use crate::biir::apply_aliases_to_biir_terminator;

// ============================================================================
// Volar IR
// ============================================================================

/// Apply store-to-load forwarding to every block in `blocks`.
///
/// Returns `true` if any block was modified.
pub fn store_forward_ir_blocks<P: Clone + Default>(blocks: &mut IRBlocks<P>) -> bool {
    let mut any_changed = false;
    for block in blocks.blocks.iter_mut() {
        any_changed |= store_forward_ir_block(block);
    }
    any_changed
}

fn store_forward_ir_block<P: Clone + Default>(block: &mut IRBlock<P>) -> bool {
    // Cache: (StorageId, TypeId, addr_var) -> src_var
    let mut cache: BTreeMap<(StorageId, TypeId, IRVarId), IRVarId> = BTreeMap::new();
    // Alias map: rv -> forwarded var
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    let mut changed = false;

    let base = block.params.len() as u32;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        // Apply existing aliases to this stmt's operands first.
        if apply_aliases_to_ir_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        // Snapshot the stmt (to avoid borrow issues).
        let stmt = block.stmts[i].clone();

        use volar_ir_common::Stmt;
        match &stmt {
            Stmt::StorageWrite { storage, src, ty, addr } => {
                let src = canon_alias(&alias_map, *src);
                let addr = canon_alias(&alias_map, *addr);
                // Invalidate all reads for (storage, ty).
                cache.retain(|(s, t, _), _| !(s == storage && t == ty));
                // Cache this write.
                cache.insert((*storage, *ty, addr), src);
            }
            Stmt::StorageRead { storage, ty, addr } => {
                let addr = canon_alias(&alias_map, *addr);
                let key = (*storage, *ty, addr);
                if let Some(&src) = cache.get(&key) {
                    // Forward: alias rv -> src.
                    alias_map.insert(rv, src);
                    changed = true;
                } else {
                    // Record this read in the cache (for potential future
                    // write-then-read chains — reads don't invalidate).
                    // We don't cache reads themselves for forwarding purposes.
                }
            }
            _ => {}
        }
    }

    // Rewrite terminator.
    changed |= apply_aliases_to_ir_terminator(&mut block.terminator, &alias_map);

    changed
}

// ============================================================================
// Boolar IR
// ============================================================================

/// Apply store-to-load forwarding to every block in `blocks`.
///
/// Returns `true` if any block was modified.
pub fn store_forward_biir_blocks<P: Clone + Default>(blocks: &mut BIrBlocks<P>) -> bool {
    let mut any_changed = false;
    for block in blocks.0.iter_mut() {
        any_changed |= store_forward_biir_block(block);
    }
    any_changed
}

fn store_forward_biir_block<P: Clone + Default>(block: &mut BIrBlock<P>) -> bool {
    // Cache: (StorageId, bit_width, addr_var) -> src_var
    let mut cache: BTreeMap<(StorageId, usize, IRVarId), IRVarId> = BTreeMap::new();
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    let mut changed = false;

    let base = block.params;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        // Apply aliases to this stmt's operands.
        if crate::biir::apply_aliases_to_biir_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        let stmt = block.stmts[i].clone();

        match &stmt {
            BIrStmt::StorageWrite { storage, src, bit_width, addr } => {
                let src = canon_alias(&alias_map, *src);
                let addr = canon_alias(&alias_map, *addr);
                cache.retain(|(s, w, _), _| !(s == storage && w == bit_width));
                cache.insert((*storage, *bit_width, addr), src);
            }
            BIrStmt::StorageRead { storage, bit_width, addr } => {
                let addr = canon_alias(&alias_map, *addr);
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

    changed
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
