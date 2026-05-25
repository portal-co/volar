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
use volar_ir_common::{Constant, StorageId, TypeId, TypeTable};
use vaffle::{FuncBody, FuncDecl, Module, Value, ValueId};

use crate::common::{
    canon_alias, constant_and, constant_is_zero, constant_or, constant_rol, constant_ror,
    constant_shl, constant_xor, mask_constant, stmt_output_type, type_bit_width,
};
use crate::ir::apply_aliases_to_ir_terminator;
use crate::biir::apply_aliases_to_biir_terminator;

// ============================================================================
// Address disambiguation
// ============================================================================

/// Max distinct addresses tracked per (StorageId, TypeId/bit_width) slot.
const MAX_CONCURRENT_STORES: usize = 16;

const ALL_ONES: Constant = Constant { hi: u128::MAX, lo: u128::MAX };

/// Per-bit abstract value: `known` marks which bit positions we know,
/// `value` holds their values (only meaningful where `known` has a 1-bit).
///
/// Used alongside the GF(2) poly check to catch cases like `Merge` of adder
/// output bits, where individual bits are provably distinct even though the
/// whole-word poly XOR has variable terms.
#[derive(Clone, Copy)]
struct KnownBits {
    known: Constant,
    value: Constant,
}

impl Default for KnownBits {
    fn default() -> Self {
        KnownBits { known: Constant { hi: 0, lo: 0 }, value: Constant { hi: 0, lo: 0 } }
    }
}

impl KnownBits {
    fn from_const(c: Constant, width: usize) -> Self {
        let m = mask_constant(ALL_ONES, width);
        KnownBits { known: m, value: constant_and(c, m) }
    }
    /// True if any bit position is definitely 1 in the XOR of `self` and `other`.
    fn xor_has_one_bit(self, other: KnownBits) -> bool {
        let both = constant_and(self.known, other.known);
        !constant_is_zero(both)
            && !constant_is_zero(constant_and(both, constant_xor(self.value, other.value)))
    }
}

/// Set bit `bit` (0-indexed) in a 256-bit `Constant`.
fn set_bit_in_constant(mut c: Constant, bit: usize) -> Constant {
    if bit < 128 { c.lo |= 1u128 << bit; }
    else if bit < 256 { c.hi |= 1u128 << (bit - 128); }
    c
}

/// GF(2) polynomial representation of an address: `(monomials, constant)`.
/// Monomial coefficients are mod-2; XOR-ing two of these gives their difference.
type IrPolyRepr = (BTreeMap<Vec<IRVarId>, u8>, Constant);

/// Extract the GF(2) polynomial representation of an IR address variable.
fn ir_addr_poly(
    v: IRVarId,
    const_map: &BTreeMap<IRVarId, Constant>,
    poly_map: &BTreeMap<IRVarId, IrPolyRepr>,
) -> IrPolyRepr {
    if let Some(&c) = const_map.get(&v) {
        return (BTreeMap::new(), c);
    }
    if let Some(p) = poly_map.get(&v) {
        return p.clone();
    }
    let mut m = BTreeMap::new();
    m.insert(alloc::vec![v], 1u8);
    (m, Constant { hi: 0, lo: 0 })
}

/// GF(2) poly check: true iff the symbolic XOR of `a` and `b` is a nonzero constant.
fn ir_polys_xor_nonzero_const(
    a: IRVarId,
    b: IRVarId,
    const_map: &BTreeMap<IRVarId, Constant>,
    poly_map: &BTreeMap<IRVarId, IrPolyRepr>,
) -> bool {
    let (mut ca, ka) = ir_addr_poly(a, const_map, poly_map);
    let (cb, kb) = ir_addr_poly(b, const_map, poly_map);
    for (key, coeff) in cb {
        let e = ca.entry(key).or_insert(0);
        *e ^= coeff;
    }
    ca.retain(|_, v| *v & 1 != 0);
    ca.is_empty() && !constant_is_zero(constant_xor(ka, kb))
}

/// Return `true` iff `a` and `b` are provably distinct addresses.
///
/// Runs two complementary checks:
/// 1. GF(2) polynomial XOR — catches same-base + different-constant patterns.
/// 2. Bitwise known-bits XOR — catches `Merge`/`Rol`/`Ror`/`Splat`/`Shuffle`
///    patterns where individual bits are definitively distinct.
fn ir_addrs_provably_different(
    a: IRVarId,
    b: IRVarId,
    const_map: &BTreeMap<IRVarId, Constant>,
    poly_map: &BTreeMap<IRVarId, IrPolyRepr>,
    known_bits_map: &BTreeMap<IRVarId, KnownBits>,
) -> bool {
    if a == b { return false; }
    if ir_polys_xor_nonzero_const(a, b, const_map, poly_map) { return true; }
    let kb_a = known_bits_map.get(&a).copied().unwrap_or_default();
    let kb_b = known_bits_map.get(&b).copied().unwrap_or_default();
    kb_a.xor_has_one_bit(kb_b)
}

/// Compute `KnownBits` and output bit-width for one IR statement result.
fn ir_stmt_known_bits(
    stmt: &volar_ir_common::Stmt<IRVarId, IRVarId>,
    known_bits_map: &BTreeMap<IRVarId, KnownBits>,
    width_map: &BTreeMap<IRVarId, usize>,
    types: &TypeTable,
) -> (KnownBits, Option<usize>) {
    use volar_ir_common::Stmt;
    let get_w = |ty: TypeId| type_bit_width(ty, types).unwrap_or(0);
    let get_kb = |v: &IRVarId| known_bits_map.get(v).copied().unwrap_or_default();
    let get_pw = |v: &IRVarId| width_map.get(v).copied().unwrap_or(0);

    match stmt {
        Stmt::Const(c, ty) => {
            let w = get_w(*ty);
            (KnownBits::from_const(*c, w), Some(w))
        }
        Stmt::Poly { coeffs, constant, ty } => {
            let w = get_w(*ty);
            let kb = if coeffs.is_empty() {
                KnownBits::from_const(*constant, w)
            } else {
                KnownBits::default()
            };
            (kb, Some(w))
        }
        Stmt::Transmute { src, dst_ty, .. } => {
            let w = get_w(*dst_ty);
            let kb = get_kb(src);
            (KnownBits { known: mask_constant(kb.known, w), value: mask_constant(kb.value, w) }, Some(w))
        }
        Stmt::Merge { parts, ty } => {
            let total_w = get_w(*ty);
            let mut known = Constant { hi: 0, lo: 0 };
            let mut value = Constant { hi: 0, lo: 0 };
            let mut offset = 0usize;
            for part in parts {
                let part_w = get_pw(part);
                if part_w == 0 { break; }
                let kb = get_kb(part);
                let sk = constant_shl(mask_constant(kb.known, part_w), offset);
                let sv = constant_shl(mask_constant(kb.value, part_w), offset);
                known = constant_or(known, sk);
                value = constant_or(value, sv);
                offset += part_w;
                if offset >= total_w { break; }
            }
            (KnownBits { known, value }, Some(total_w))
        }
        Stmt::Rol { src, ty, n } => {
            let w = get_w(*ty);
            let kb = get_kb(src);
            (KnownBits { known: constant_rol(kb.known, w, *n), value: constant_rol(kb.value, w, *n) }, Some(w))
        }
        Stmt::Ror { src, ty, n } => {
            let w = get_w(*ty);
            let kb = get_kb(src);
            (KnownBits { known: constant_ror(kb.known, w, *n), value: constant_ror(kb.value, w, *n) }, Some(w))
        }
        Stmt::Splat { src, ty } => {
            let w = get_w(*ty);
            let kb = get_kb(src);
            let kb_out = if kb.known.lo & 1 != 0 {
                let fill = if kb.value.lo & 1 != 0 { ALL_ONES } else { Constant { hi: 0, lo: 0 } };
                KnownBits { known: mask_constant(ALL_ONES, w), value: mask_constant(fill, w) }
            } else {
                KnownBits::default()
            };
            (kb_out, Some(w))
        }
        Stmt::Shuffle { result_bits, ty } => {
            let w = get_w(*ty);
            let mut known = Constant { hi: 0, lo: 0 };
            let mut value = Constant { hi: 0, lo: 0 };
            for (out_bit, (src_bit, src_var)) in result_bits.iter().enumerate() {
                if out_bit >= 256 { break; }
                let kb = get_kb(src_var);
                let sb = *src_bit as usize;
                let (ka, va) = if sb < 128 {
                    ((kb.known.lo >> sb) & 1, (kb.value.lo >> sb) & 1)
                } else if sb < 256 {
                    ((kb.known.hi >> (sb - 128)) & 1, (kb.value.hi >> (sb - 128)) & 1)
                } else {
                    (0, 0)
                };
                if ka != 0 {
                    known = set_bit_in_constant(known, out_bit);
                    if va != 0 { value = set_bit_in_constant(value, out_bit); }
                }
            }
            (KnownBits { known, value }, Some(w))
        }
        _ => {
            let w = stmt_output_type(stmt)
                .and_then(|ty| type_bit_width(ty, types))
                .unwrap_or(0);
            (KnownBits::default(), if w > 0 { Some(w) } else { None })
        }
    }
}

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
pub fn store_forward_ir_blocks<P: Clone + Default>(
    blocks: &mut IRBlocks<P>,
    types: &TypeTable,
) -> bool {
    let n = blocks.blocks.len();
    if n == 0 {
        return false;
    }
    if n == 1 {
        let (changed, _) =
            store_forward_ir_block_with_cache(&mut blocks.blocks[0], BTreeMap::new(), types);
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
            store_forward_ir_block_with_cache(&mut blocks.blocks[bi], incoming, types);
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
    types: &TypeTable,
) -> (bool, IrStoreCache) {
    let mut cache = incoming;
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    let mut const_map: BTreeMap<IRVarId, Constant> = BTreeMap::new();
    let mut addr_poly_map: BTreeMap<IRVarId, IrPolyRepr> = BTreeMap::new();
    let mut known_bits_map: BTreeMap<IRVarId, KnownBits> = BTreeMap::new();
    let mut width_map: BTreeMap<IRVarId, usize> = BTreeMap::new();
    let mut changed = false;

    let base = block.params.len() as u32;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        if apply_aliases_to_ir_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        let stmt = block.stmts[i].clone();

        use volar_ir_common::Stmt;
        // Track constants and polynomials for GF(2) disambiguation.
        match &stmt {
            Stmt::Const(c, _) => { const_map.insert(rv, *c); }
            Stmt::Poly { coeffs, constant, .. } => {
                addr_poly_map.insert(rv, (coeffs.clone(), *constant));
            }
            _ => {}
        }
        // Track bitwise known bits for the complementary bitwise check.
        let (kb, w) = ir_stmt_known_bits(&stmt, &known_bits_map, &width_map, types);
        known_bits_map.insert(rv, kb);
        if let Some(w) = w { width_map.insert(rv, w); }

        match &stmt {
            Stmt::StorageWrite { storage, src, ty, addr } => {
                let src = canon_alias(&alias_map, *src);
                let addr = canon_alias(&alias_map, *addr);
                let slot_count = cache.keys()
                    .filter(|(s, t, _)| s == storage && t == ty)
                    .count();
                if slot_count >= MAX_CONCURRENT_STORES {
                    cache.retain(|(s, t, _), _| !(s == storage && t == ty));
                } else {
                    cache.retain(|(s, t, cached_addr), _| {
                        if s != storage || t != ty { return true; }
                        ir_addrs_provably_different(
                            addr, *cached_addr, &const_map, &addr_poly_map, &known_bits_map,
                        )
                    });
                }
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
pub(crate) fn shift_var(v: &mut IRVarId, old_base: u32, n_stmts: u32, shift: u32) {
    if v.0 >= old_base && v.0 < old_base + n_stmts {
        v.0 += shift;
    }
}

/// Shift all `IRVarId` references in a `Stmt<IRVarId, IRVarId>`.
pub(crate) fn shift_ir_stmt_vars(
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
pub(crate) fn shift_ir_terminator_vars(
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
fn biir_addrs_provably_different(
    addr_a: &[IRVarId],
    addr_b: &[IRVarId],
    known_map: &BTreeMap<IRVarId, Option<bool>>,
) -> bool {
    if addr_a.len() != addr_b.len() {
        return true;
    }
    for (a_bit, b_bit) in addr_a.iter().zip(addr_b.iter()) {
        if a_bit == b_bit { continue; }
        let ka = known_map.get(a_bit).copied().flatten();
        let kb = known_map.get(b_bit).copied().flatten();
        if matches!((ka, kb), (Some(true), Some(false)) | (Some(false), Some(true))) {
            return true;
        }
    }
    false
}

fn store_forward_biir_block_with_cache<P: Clone + Default>(
    block: &mut BIrBlock<P>,
    incoming: BiirStoreCache,
) -> (bool, BiirStoreCache) {
    let mut cache = incoming;
    let mut alias_map: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    let mut known_map: BTreeMap<IRVarId, Option<bool>> = BTreeMap::new();
    let mut changed = false;

    let base = block.params;

    for i in 0..block.stmts.len() {
        let rv = IRVarId(base + i as u32);

        if crate::biir::apply_aliases_to_biir_stmt(&mut block.stmts[i], &alias_map) {
            changed = true;
        }

        let stmt = block.stmts[i].clone();

        // Update known-bits map for this value.
        let known: Option<bool> = match &stmt {
            BIrStmt::Zero => Some(false),
            BIrStmt::One  => Some(true),
            BIrStmt::Not(a) => {
                known_map.get(&canon_alias(&alias_map, *a)).copied().flatten().map(|b| !b)
            }
            BIrStmt::And(a, b) => {
                let ka = known_map.get(&canon_alias(&alias_map, *a)).copied().flatten();
                let kb = known_map.get(&canon_alias(&alias_map, *b)).copied().flatten();
                match (ka, kb) {
                    (Some(false), _) | (_, Some(false)) => Some(false),
                    (Some(true), Some(true)) => Some(true),
                    _ => None,
                }
            }
            BIrStmt::Or(a, b) => {
                let ka = known_map.get(&canon_alias(&alias_map, *a)).copied().flatten();
                let kb = known_map.get(&canon_alias(&alias_map, *b)).copied().flatten();
                match (ka, kb) {
                    (Some(true), _) | (_, Some(true)) => Some(true),
                    (Some(false), Some(false)) => Some(false),
                    _ => None,
                }
            }
            BIrStmt::Xor(a, b) => {
                let ka = known_map.get(&canon_alias(&alias_map, *a)).copied().flatten();
                let kb = known_map.get(&canon_alias(&alias_map, *b)).copied().flatten();
                ka.and_then(|a| kb.map(|b| a ^ b))
            }
            _ => None,
        };
        known_map.insert(rv, known);

        match &stmt {
            BIrStmt::StorageWrite { storage, src, bit_width, addr } => {
                let src = canon_alias(&alias_map, *src);
                let addr: Vec<IRVarId> = addr.iter().map(|v| canon_alias(&alias_map, *v)).collect();
                let slot_count = cache.keys()
                    .filter(|(s, w, _)| s == storage && w == bit_width)
                    .count();
                if slot_count >= MAX_CONCURRENT_STORES {
                    cache.retain(|(s, w, _), _| !(s == storage && w == bit_width));
                } else {
                    cache.retain(|(s, w, cached_addr), _| {
                        if s != storage || w != bit_width { return true; }
                        biir_addrs_provably_different(&addr, cached_addr, &known_map)
                    });
                }
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
// VAFFLE — address disambiguation helpers
// ============================================================================

/// Extract the GF(2) polynomial representation of a VAFFLE address value.
fn vaffle_addr_poly(
    values: &[Value],
    v: ValueId,
) -> (BTreeMap<Vec<ValueId>, u8>, Constant) {
    use volar_ir_common::Stmt;
    match &values[v.0] {
        Value::Op(Stmt::Const(c, _)) => (BTreeMap::new(), *c),
        Value::Op(Stmt::Poly { coeffs, constant, .. }) => (coeffs.clone(), *constant),
        _ => {
            let mut m = BTreeMap::new();
            m.insert(alloc::vec![v], 1u8);
            (m, Constant { hi: 0, lo: 0 })
        }
    }
}

/// GF(2) poly check for VAFFLE addresses.
fn vaffle_polys_xor_nonzero_const(values: &[Value], a: ValueId, b: ValueId) -> bool {
    let (mut ca, ka) = vaffle_addr_poly(values, a);
    let (cb, kb) = vaffle_addr_poly(values, b);
    for (key, coeff) in cb {
        let e = ca.entry(key).or_insert(0);
        *e ^= coeff;
    }
    ca.retain(|_, v| *v & 1 != 0);
    ca.is_empty() && !constant_is_zero(constant_xor(ka, kb))
}

/// Compute `KnownBits` for one VAFFLE value, given already-computed results for
/// earlier values (forward pass over the global value table).
fn vaffle_value_known_bits(
    values: &[Value],
    kb_vec: &[KnownBits],
    width_vec: &[usize],
    v: ValueId,
    types: &TypeTable,
) -> (KnownBits, usize) {
    use volar_ir_common::Stmt;
    let get_w = |ty: TypeId| type_bit_width(ty, types).unwrap_or(0);
    let get_kb = |u: ValueId| kb_vec.get(u.0).copied().unwrap_or_default();
    let get_pw = |u: ValueId| width_vec.get(u.0).copied().unwrap_or(0);

    match &values[v.0] {
        Value::Op(Stmt::Const(c, ty)) => {
            let w = get_w(*ty);
            (KnownBits::from_const(*c, w), w)
        }
        Value::Op(Stmt::Poly { coeffs, constant, ty }) => {
            let w = get_w(*ty);
            let kb = if coeffs.is_empty() {
                KnownBits::from_const(*constant, w)
            } else {
                KnownBits::default()
            };
            (kb, w)
        }
        Value::Op(Stmt::Transmute { src, dst_ty, .. }) => {
            let w = get_w(*dst_ty);
            let kb = get_kb(*src);
            (KnownBits { known: mask_constant(kb.known, w), value: mask_constant(kb.value, w) }, w)
        }
        Value::Op(Stmt::Merge { parts, ty }) => {
            let total_w = get_w(*ty);
            let mut known = Constant { hi: 0, lo: 0 };
            let mut value = Constant { hi: 0, lo: 0 };
            let mut offset = 0usize;
            for &part in parts {
                let part_w = get_pw(part);
                if part_w == 0 { break; }
                let kb = get_kb(part);
                let sk = constant_shl(mask_constant(kb.known, part_w), offset);
                let sv = constant_shl(mask_constant(kb.value, part_w), offset);
                known = constant_or(known, sk);
                value = constant_or(value, sv);
                offset += part_w;
                if offset >= total_w { break; }
            }
            (KnownBits { known, value }, total_w)
        }
        Value::Op(Stmt::Rol { src, ty, n }) => {
            let w = get_w(*ty);
            let kb = get_kb(*src);
            (KnownBits { known: constant_rol(kb.known, w, *n), value: constant_rol(kb.value, w, *n) }, w)
        }
        Value::Op(Stmt::Ror { src, ty, n }) => {
            let w = get_w(*ty);
            let kb = get_kb(*src);
            (KnownBits { known: constant_ror(kb.known, w, *n), value: constant_ror(kb.value, w, *n) }, w)
        }
        Value::Op(Stmt::Splat { src, ty }) => {
            let w = get_w(*ty);
            let kb = get_kb(*src);
            let kb_out = if kb.known.lo & 1 != 0 {
                let fill = if kb.value.lo & 1 != 0 { ALL_ONES } else { Constant { hi: 0, lo: 0 } };
                KnownBits { known: mask_constant(ALL_ONES, w), value: mask_constant(fill, w) }
            } else {
                KnownBits::default()
            };
            (kb_out, w)
        }
        Value::Op(Stmt::Shuffle { result_bits, ty }) => {
            let w = get_w(*ty);
            let mut known = Constant { hi: 0, lo: 0 };
            let mut value = Constant { hi: 0, lo: 0 };
            for (out_bit, (src_bit, src_var)) in result_bits.iter().enumerate() {
                if out_bit >= 256 { break; }
                let kb = get_kb(*src_var);
                let sb = *src_bit as usize;
                let (ka, va) = if sb < 128 {
                    ((kb.known.lo >> sb) & 1, (kb.value.lo >> sb) & 1)
                } else if sb < 256 {
                    ((kb.known.hi >> (sb - 128)) & 1, (kb.value.hi >> (sb - 128)) & 1)
                } else {
                    (0, 0)
                };
                if ka != 0 {
                    known = set_bit_in_constant(known, out_bit);
                    if va != 0 { value = set_bit_in_constant(value, out_bit); }
                }
            }
            (KnownBits { known, value }, w)
        }
        Value::Param { ty, .. } => {
            let w = get_w(*ty);
            (KnownBits::default(), w)
        }
        _ => (KnownBits::default(), 0),
    }
}

/// Pre-compute `KnownBits` for every value in a VAFFLE function body.
fn compute_vaffle_known_bits(body: &FuncBody, types: &TypeTable) -> alloc::vec::Vec<KnownBits> {
    let n = body.values.len();
    let mut kb_vec = alloc::vec![KnownBits::default(); n];
    let mut width_vec = alloc::vec![0usize; n];
    for i in 0..n {
        let (kb, w) = vaffle_value_known_bits(&body.values, &kb_vec, &width_vec, ValueId(i), types);
        kb_vec[i] = kb;
        width_vec[i] = w;
    }
    kb_vec
}

/// Return `true` iff VAFFLE address values `a` and `b` are provably distinct.
fn vaffle_addrs_provably_different(
    values: &[Value],
    kb_vec: &[KnownBits],
    a: ValueId,
    b: ValueId,
) -> bool {
    if a == b { return false; }
    if vaffle_polys_xor_nonzero_const(values, a, b) { return true; }
    let kb_a = kb_vec.get(a.0).copied().unwrap_or_default();
    let kb_b = kb_vec.get(b.0).copied().unwrap_or_default();
    kb_a.xor_has_one_bit(kb_b)
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
            any_changed |= store_forward_vaffle_body(body, &module.types);
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
fn store_forward_vaffle_body(body: &mut FuncBody, types: &TypeTable) -> bool {
    let kb_vec = compute_vaffle_known_bits(body, types);
    let n = body.blocks.len();

    if n == 1 {
        // Fast path: no CFG needed.
        let (changed, _) =
            store_forward_vaffle_block_with_cache(body, &kb_vec, 0, BTreeMap::new());
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
            store_forward_vaffle_block_with_cache(body, &kb_vec, block_idx, incoming);
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
    kb_vec: &[KnownBits],
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
                let slot_count = cache.keys()
                    .filter(|(s, t, _)| *s == storage && *t == ty)
                    .count();
                if slot_count >= MAX_CONCURRENT_STORES {
                    cache.retain(|(s, t, _), _| !(*s == storage && *t == ty));
                } else {
                    let vals = &body.values;
                    cache.retain(|(s, t, cached_addr), _| {
                        if *s != storage || *t != ty { return true; }
                        vaffle_addrs_provably_different(vals, kb_vec, addr, *cached_addr)
                    });
                }
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

// ============================================================================
// Unit tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use volar_ir::ir::{IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId};
    use volar_ir_common::{Constant, Stmt, StorageId, TypeId};

    fn jmp_self() -> IRTerminator {
        IRTerminator::Jmp {
            func: IRBlockTargetId::Block(IRBlockId(0)),
            args: vec![],
        }
    }

    fn make_block(params: Vec<TypeId>, stmts: Vec<Stmt<IRVarId, IRVarId>>) -> IRBlock<()> {
        let n = stmts.len();
        IRBlock { params, stmts, stmt_provs: vec![(); n], terminator: jmp_self() }
    }

    fn const_addr(lo: u128) -> Stmt<IRVarId, IRVarId> {
        Stmt::Const(Constant { hi: 0, lo }, TypeId(0))
    }

    #[test]
    fn two_const_addr_writes_both_forwarded() {
        // Block:
        //   v0 = const(0)               ← address 0
        //   v1 = const(1)               ← address 1
        //   v2 = const(42)              ← value stored at addr 0
        //   v3 = const(99)              ← value stored at addr 1
        //        store(S, v2, T, v0)
        //        store(S, v3, T, v1)
        //   v6 = load(S, T, v0)         ← should forward to v2
        //   v7 = load(S, T, v1)         ← should forward to v3
        let st = StorageId(0);
        let ty = TypeId(0);
        let stmts = vec![
            const_addr(0),                                                             // v0
            const_addr(1),                                                             // v1
            Stmt::Const(Constant { hi: 0, lo: 42 }, TypeId(0)),                       // v2
            Stmt::Const(Constant { hi: 0, lo: 99 }, TypeId(0)),                       // v3
            Stmt::StorageWrite { storage: st, src: IRVarId(2), ty, addr: IRVarId(0) }, // v4
            Stmt::StorageWrite { storage: st, src: IRVarId(3), ty, addr: IRVarId(1) }, // v5
            Stmt::StorageRead { storage: st, ty, addr: IRVarId(0) },                   // v6 → alias v2
            Stmt::StorageRead { storage: st, ty, addr: IRVarId(1) },                   // v7 → alias v3
        ];
        let mut blocks = IRBlocks::new(vec![make_block(vec![], stmts)]);
        let types = volar_ir_common::TypeTable::new();
        let changed = store_forward_ir_blocks(&mut blocks, &types);
        assert!(changed, "expected forwarding to occur");
    }

    #[test]
    fn opaque_addr_write_drops_cache() {
        // p0 (IRVarId(0)): opaque param address.
        //   v1 = const(0)               ← constant address
        //   v2 = const(42)              ← value
        //        store(S, v2, T, v1)    ← populate cache for const addr
        //        store(S, v2, T, p0)    ← opaque addr — cannot prove distinct → flush
        //   v5 = load(S, T, v1)         ← NOT forwarded (cache flushed)
        let st = StorageId(0);
        let ty = TypeId(0);
        let stmts = vec![
            Stmt::Const(Constant { hi: 0, lo: 0 }, TypeId(0)),                        // v1
            Stmt::Const(Constant { hi: 0, lo: 42 }, TypeId(0)),                       // v2
            Stmt::StorageWrite { storage: st, src: IRVarId(2), ty, addr: IRVarId(1) }, // v3
            Stmt::StorageWrite { storage: st, src: IRVarId(2), ty, addr: IRVarId(0) }, // v4 (opaque)
            Stmt::StorageRead { storage: st, ty, addr: IRVarId(1) },                   // v5 — not forwarded
        ];
        let mut blocks = IRBlocks::new(vec![make_block(vec![TypeId(0)], stmts)]);
        let types = volar_ir_common::TypeTable::new();
        // Should not panic; cache conservatively flushed by the opaque write.
        let _changed = store_forward_ir_blocks(&mut blocks, &types);
    }
}
