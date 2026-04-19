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

use alloc::{collections::BTreeMap, vec::Vec};
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt};
use volar_ir::ir::{IRBlock, IRBlocks, IRVarId};
use volar_ir_common::{Constant, StorageId, TypeId};
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

fn store_forward_vaffle_body(body: &mut FuncBody) -> bool {
    let mut any_changed = false;
    // Process each block independently (single-block bodies are the common case).
    for block_idx in 0..body.blocks.len() {
        any_changed |= store_forward_vaffle_block(body, block_idx);
    }
    any_changed
}

/// Extracted, `Copy`-only fields from a VAFFLE `StorageWrite` or `StorageRead`.
enum VaffleStoreAction {
    Write { storage: StorageId, src: ValueId, ty: TypeId, addr: ValueId },
    Read  { storage: StorageId, ty: TypeId, addr: ValueId },
}

fn store_forward_vaffle_block(body: &mut FuncBody, block_idx: usize) -> bool {
    // Cache: (StorageId, TypeId, addr_var) -> src_var
    let mut cache: BTreeMap<(StorageId, TypeId, ValueId), ValueId> = BTreeMap::new();
    let mut alias_map: BTreeMap<ValueId, ValueId> = BTreeMap::new();
    let mut changed = false;

    let stmt_vids: Vec<ValueId> = body.blocks[block_idx].stmts.clone();

    for vid in stmt_vids {
        // Apply aliases to this value's operands in the values array.
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

    changed
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
