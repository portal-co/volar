//! Generator for structurally valid single-function VAFFLE [`Module`]s.
//!
//! # Design
//!
//! Mirrors [`crate::generators::ir`]: raw integer data is interpreted into a
//! valid module by clamping all indices.  The generated module has:
//!
//! - One `FuncDecl::Body` (no imports).
//! - One block (`BlockId(0)`), which is also the entry block.
//! - Only `Const`, linear `Poly` (XOR of one or two same-width vars), `Rol`,
//!   and `Ror` values — no calls, pointers, or stores.
//! - `Terminator::Return { values: all_value_ids }`.
//!
//! # Raw-data approach
//!
//! 1. **Raw data** — integer tuples with no structural constraints.
//! 2. **Interpretation** — [`interpret_vaffle`] converts them into a valid
//!    `(Module, FuncId, Vec<usize>)` (module, function id, param bit-widths).

use std::collections::BTreeMap;

use vaffle::{Block, BlockId, FuncBody, FuncDecl, FuncId, Module, SigDecl, SigId, Target, Terminator, Value, ValueId};
use volar_ir_common::{Constant, IrType, Stmt, StorageId, Type, TypeId, TypeTable};

use crate::interpreter::ir::primitive_width;
use crate::generators::ir::{PRIM_TYPES, RawIrStmt, RawTypeIdx};

// ============================================================================
// Public API
// ============================================================================

/// Convert raw data into a single-function, single-block VAFFLE `Module`.
///
/// Returns `(module, FuncId(0), param_widths)` where `param_widths[i]` is the
/// bit-width expected for input `i`.
pub fn interpret_vaffle(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
) -> (Module, FuncId, Vec<usize>) {
    let mut type_table = TypeTable::new();

    // Intern param types.
    let param_type_ids: Vec<TypeId> = raw_param_type_idxs
        .iter()
        .map(|&idx| type_table.primitive(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
        .collect();

    let param_widths: Vec<usize> = param_type_ids
        .iter()
        .map(|&tid| match &type_table.0[tid.0 as usize] {
            IrType::Primitive(t) => primitive_width(*t),
            _ => unreachable!(),
        })
        .collect();

    let n_params = param_type_ids.len();

    // Track (TypeId, bit_width) for each value so we can generate compatible stmts.
    let mut var_info: Vec<(TypeId, usize)> = param_type_ids
        .iter()
        .zip(param_widths.iter())
        .map(|(&tid, &w)| (tid, w))
        .collect();

    // Build body.values (initially populated with Param entries).
    let mut values: Vec<Value> = param_type_ids
        .iter()
        .enumerate()
        .map(|(i, &tid)| Value::Param {
            block: BlockId(0),
            ty: tid,
            idx: i,
        })
        .collect();

    // Stmt ValueIds (referenced in Block::stmts).
    let mut stmt_vids: Vec<ValueId> = Vec::new();

    for (kind, a, b, c_lo, c_hi) in raw_stmts.iter().copied() {
        let n_vars = var_info.len();

        let (stmt, result_tid, result_w) = if n_vars == 0 || kind % 3 == 0 {
            // ── Const ───────────────────────────────────────────────────────
            let type_idx = (a as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = type_table.primitive(ty);
            let w = primitive_width(ty);
            (Stmt::Const(Constant { lo: c_lo, hi: c_hi }, tid), tid, w)
        } else if kind % 3 == 1 {
            // ── Linear Poly (XOR of 1–2 same-width vars) ────────────────────
            let v0_idx = (a as usize) % n_vars;
            let (v0_tid, v0_w) = var_info[v0_idx];

            let same_w: Vec<usize> = var_info
                .iter()
                .enumerate()
                .filter(|(_, (_, w))| *w == v0_w)
                .map(|(i, _)| i)
                .collect();

            let mut coeffs = BTreeMap::new();
            coeffs.insert(vec![ValueId(v0_idx)], 1u8);

            if same_w.len() > 1 {
                let v1_idx = same_w[(b as usize) % same_w.len()];
                if v1_idx != v0_idx {
                    let mut key = vec![ValueId(v0_idx), ValueId(v1_idx)];
                    key.sort();
                    coeffs.clear();
                    coeffs.insert(key, 1u8);
                }
            }

            let c = mask_const(Constant { lo: c_lo, hi: c_hi }, v0_w);
            (Stmt::Poly { ty: v0_tid, coeffs, constant: c }, v0_tid, v0_w)
        } else {
            // ── Rol / Ror ────────────────────────────────────────────────────
            let v_idx = (a as usize) % n_vars;
            let (v_tid, v_w) = var_info[v_idx];
            let n_rot = if v_w > 0 { (b as usize) % v_w } else { 0 };
            let src = ValueId(v_idx);
            let s = if kind % 2 == 0 {
                Stmt::Rol { src, ty: v_tid, n: n_rot }
            } else {
                Stmt::Ror { src, ty: v_tid, n: n_rot }
            };
            (s, v_tid, v_w)
        };

        let stmt_vid = ValueId(n_params + stmt_vids.len());
        var_info.push((result_tid, result_w));
        values.push(Value::Op(stmt));
        stmt_vids.push(stmt_vid);
    }

    // Return all value IDs from the single block.
    let all_vids: Vec<ValueId> = (0..values.len()).map(ValueId).collect();

    // Build the SigDecl: params = entry param types, results = all var types.
    let sig_results: Vec<TypeId> = var_info.iter().map(|(tid, _)| *tid).collect();
    let sig = SigDecl {
        params: param_type_ids.clone(),
        results: sig_results,
    };

    // Build the single block.
    let block = Block {
        params: param_type_ids
            .iter()
            .enumerate()
            .map(|(i, &tid)| (ValueId(i), tid))
            .collect(),
        stmts: stmt_vids,
        terminator: Terminator::Return { values: all_vids },
    };

    let body = FuncBody {
        sig: SigId(0),
        blocks: vec![block],
        values,
        entry: BlockId(0),
    };

    let module = Module {
        types: type_table,
        oracles: vec![],
        actions: vec![],
        funcs: vec![FuncDecl::Body(body)],
        sigs: vec![sig],
        exports: BTreeMap::new(),
    };

    (module, FuncId(0), param_widths)
}

// ============================================================================
// Extended interpreter: storage ops
// ============================================================================

/// Like [`interpret_vaffle`] but uses a 5-way `kind % 5` dispatch to also
/// emit `StorageRead` and `StorageWrite` statements.
///
/// `StorageWrite` is void (produces no usable output); its `ValueId` slot still
/// exists in `body.values` but is excluded from `var_info` so it cannot be
/// used as a meaningful operand.
pub fn interpret_vaffle_extended(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
) -> (Module, FuncId, Vec<usize>) {
    interpret_vaffle_extended_inner(raw_param_type_idxs, raw_stmts, BlockId(0))
}

/// Shared implementation for single-block and multi-block extended bodies.
///
/// `block_id` is the VAFFLE `BlockId` that params belong to.
/// Returns `(type_table, param_type_ids, param_widths, var_info, values, stmt_vids)`.
#[allow(clippy::type_complexity)]
fn build_vaffle_extended_block(
    type_table: &mut volar_ir_common::TypeTable,
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
    block_id: BlockId,
    // var_info from preceding blocks (for cross-block references)
    initial_var_info: Vec<(TypeId, usize, ValueId)>,
    // offset for new ValueId assignment
    value_offset: usize,
) -> (
    Vec<TypeId>,         // param_type_ids
    Vec<usize>,          // param_widths
    Vec<(TypeId, usize, ValueId)>, // var_info (all vars including preceding + this block's)
    Vec<Value>,          // new values (params + stmts for this block)
    Vec<ValueId>,        // stmt_vids for this block
) {
    use volar_ir_common::{Constant, IrType, Type};
    let param_type_ids: Vec<TypeId> = raw_param_type_idxs
        .iter()
        .map(|&idx| type_table.primitive(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
        .collect();

    let param_widths: Vec<usize> = param_type_ids
        .iter()
        .map(|&tid| match &type_table.0[tid.0 as usize] {
            IrType::Primitive(t) => primitive_width(*t),
            _ => unreachable!(),
        })
        .collect();

    let n_params = param_type_ids.len();

    // var_info: starts with preceding blocks' vars, then this block's params.
    let mut var_info: Vec<(TypeId, usize, ValueId)> = initial_var_info;

    let mut new_values: Vec<Value> = Vec::new();

    // Add params to var_info and new_values.
    for (i, (&tid, &w)) in param_type_ids.iter().zip(param_widths.iter()).enumerate() {
        let vid = ValueId(value_offset + new_values.len());
        new_values.push(Value::Param { block: block_id, ty: tid, idx: i });
        var_info.push((tid, w, vid));
    }

    let param_value_offset = value_offset + n_params;
    let mut stmt_vids: Vec<ValueId> = Vec::new();

    for (kind, a, b, c_lo, c_hi) in raw_stmts.iter().copied() {
        let n_vars = var_info.len();
        let vid = ValueId(param_value_offset + stmt_vids.len());

        if n_vars == 0 || kind % 5 == 0 {
            let type_idx = (a as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = type_table.primitive(ty);
            let w = primitive_width(ty);
            new_values.push(Value::Op(Stmt::Const(Constant { lo: c_lo, hi: c_hi }, tid)));
            stmt_vids.push(vid);
            var_info.push((tid, w, vid));
        } else if kind % 5 == 1 {
            let v0_idx = (a as usize) % n_vars;
            let (v0_tid, v0_w, v0_id) = var_info[v0_idx];
            let same_w: Vec<usize> = var_info
                .iter()
                .enumerate()
                .filter(|(_, (_, w, _))| *w == v0_w)
                .map(|(i, _)| i)
                .collect();
            let mut coeffs = std::collections::BTreeMap::new();
            coeffs.insert(vec![v0_id], 1u8);
            if same_w.len() > 1 {
                let v1_vi_idx = (b as usize) % same_w.len();
                let (_, _, v1_id) = var_info[same_w[v1_vi_idx]];
                if v1_id != v0_id {
                    let mut key = vec![v0_id, v1_id];
                    key.sort();
                    coeffs.clear();
                    coeffs.insert(key, 1u8);
                }
            }
            let c = mask_const(Constant { lo: c_lo, hi: c_hi }, v0_w);
            new_values.push(Value::Op(Stmt::Poly { ty: v0_tid, coeffs, constant: c }));
            stmt_vids.push(vid);
            var_info.push((v0_tid, v0_w, vid));
        } else if kind % 5 == 2 {
            let v_idx = (a as usize) % n_vars;
            let (v_tid, v_w, v_id) = var_info[v_idx];
            let n_rot = if v_w > 0 { (b as usize) % v_w } else { 0 };
            let s = if kind % 2 == 0 {
                Stmt::Rol { src: v_id, ty: v_tid, n: n_rot }
            } else {
                Stmt::Ror { src: v_id, ty: v_tid, n: n_rot }
            };
            new_values.push(Value::Op(s));
            stmt_vids.push(vid);
            var_info.push((v_tid, v_w, vid));
        } else if kind % 5 == 3 {
            let store_id = StorageId(a % 4);
            let src_idx = (b as usize) % n_vars;
            let (src_tid, _, src_id) = var_info[src_idx];
            let addr_idx = (c_lo as usize) % n_vars;
            let addr_id = var_info[addr_idx].2;
            new_values.push(Value::Op(Stmt::StorageWrite {
                storage: store_id,
                src: src_id,
                ty: src_tid,
                addr: addr_id,
            }));
            stmt_vids.push(vid);
            // void — do NOT add to var_info
        } else {
            let store_id = StorageId(a % 4);
            let type_idx = (b as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = type_table.primitive(ty);
            let w = primitive_width(ty);
            let addr_idx = (c_lo as usize) % n_vars;
            let addr_id = var_info[addr_idx].2;
            new_values.push(Value::Op(Stmt::StorageRead {
                storage: store_id,
                ty: tid,
                addr: addr_id,
            }));
            stmt_vids.push(vid);
            var_info.push((tid, w, vid));
        }
    }

    (param_type_ids, param_widths, var_info, new_values, stmt_vids)
}

fn interpret_vaffle_extended_inner(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
    block_id: BlockId,
) -> (Module, FuncId, Vec<usize>) {
    let mut type_table = volar_ir_common::TypeTable::new();

    let (param_type_ids, param_widths, var_info, new_values, stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            raw_param_type_idxs,
            raw_stmts,
            block_id,
            vec![],
            0,
        );

    let n_params = param_type_ids.len();

    // Return all value IDs.
    let total_values = new_values.len();
    let all_vids: Vec<ValueId> = (0..total_values).map(ValueId).collect();

    let sig_results: Vec<TypeId> = {
        let fallback_tid = param_type_ids
            .first()
            .copied()
            .unwrap_or_else(|| type_table.primitive(volar_ir_common::Type::Bit));
        (0..total_values)
            .map(|i| {
                if i < n_params {
                    param_type_ids[i]
                } else {
                    var_info
                        .iter()
                        .find(|(_, _, vid)| vid.0 == i)
                        .map(|(tid, _, _)| *tid)
                        .unwrap_or(fallback_tid)
                }
            })
            .collect()
    };

    let sig = SigDecl { params: param_type_ids.clone(), results: sig_results };

    let block = Block {
        params: param_type_ids
            .iter()
            .enumerate()
            .map(|(i, &tid)| (ValueId(i), tid))
            .collect(),
        stmts: stmt_vids,
        terminator: Terminator::Return { values: all_vids },
    };

    let body = FuncBody {
        sig: SigId(0),
        blocks: vec![block],
        values: new_values,
        entry: BlockId(0),
    };

    let module = Module {
        types: type_table,
        oracles: vec![],
        actions: vec![],
        funcs: vec![FuncDecl::Body(body)],
        sigs: vec![sig],
        exports: std::collections::BTreeMap::new(),
    };

    (module, FuncId(0), param_widths)
}

// ============================================================================
// Multi-block interpreter: two-block CFG for cross-block forwarding tests
// ============================================================================

/// Build a two-block VAFFLE module:
///
/// - **Block 0 (entry)**: function params + stmts from `raw_stmts_b0`.
///   Terminates with `Jump(B1, args=[])`.
/// - **Block 1**: no params; stmts from `raw_stmts_b1` which may reference
///   Block 0's `ValueId`s directly (VAFFLE `ValueId`s are global and the
///   interpreter's `value_table` accumulates across blocks).
///   Terminates with `Return { values: all_vids }`.
///
/// This layout guarantees Block 0 always executes before Block 1 (no branches),
/// making it a reliable test bed for cross-block store-to-load forwarding.
pub fn interpret_vaffle_multiblock(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts_b0: &[RawIrStmt],
    raw_stmts_b1: &[RawIrStmt],
) -> (Module, FuncId, Vec<usize>) {
    use volar_ir_common::Type;

    let mut type_table = volar_ir_common::TypeTable::new();

    // --- Block 0 ---
    let (param_type_ids, param_widths, var_info_after_b0, b0_values, b0_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            raw_param_type_idxs,
            raw_stmts_b0,
            BlockId(0),
            vec![],
            0,
        );
    let n_params = param_type_ids.len();
    let b0_value_count = b0_values.len(); // how many values are in B0 (params + stmts)

    // Block 0 terminator: Jump to Block 1 with no args.
    // Block 1 has no params — it references B0 values via global ValueIds.
    let b0_term = Terminator::Jump(Target { block: BlockId(1), args: vec![] });

    // --- Block 1 ---
    // var_info carries over all of B0's non-void vars so B1 stmts can reference them.
    let (_, _, var_info_after_b1, b1_values, b1_stmt_vids) = build_vaffle_extended_block(
        &mut type_table,
        &[], // B1 has no params
        raw_stmts_b1,
        BlockId(1),
        var_info_after_b0,
        b0_value_count, // B1 ValueIds start where B0's end
    );

    // Block 1 terminator: Return all values from both blocks.
    let total_values = b0_value_count + b1_values.len();
    let all_vids: Vec<ValueId> = (0..total_values).map(ValueId).collect();
    let b1_term = Terminator::Return { values: all_vids.clone() };

    // Merge all values into a single flat array (B0 first, then B1).
    let mut all_values: Vec<Value> = b0_values;
    all_values.extend(b1_values);

    // Build SigDecl: params = entry params, results = all values (both blocks).
    let fallback_tid = param_type_ids
        .first()
        .copied()
        .unwrap_or_else(|| type_table.primitive(Type::Bit));
    let sig_results: Vec<TypeId> = (0..total_values)
        .map(|i| {
            if i < n_params {
                param_type_ids[i]
            } else {
                var_info_after_b1
                    .iter()
                    .find(|(_, _, vid)| vid.0 == i)
                    .map(|(tid, _, _)| *tid)
                    .unwrap_or(fallback_tid)
            }
        })
        .collect();

    let sig = SigDecl { params: param_type_ids.clone(), results: sig_results };

    let block0 = Block {
        params: param_type_ids
            .iter()
            .enumerate()
            .map(|(i, &tid)| (ValueId(i), tid))
            .collect(),
        stmts: b0_stmt_vids,
        terminator: b0_term,
    };
    let block1 = Block {
        params: vec![],
        stmts: b1_stmt_vids,
        terminator: b1_term,
    };

    let body = FuncBody {
        sig: SigId(0),
        blocks: vec![block0, block1],
        values: all_values,
        entry: BlockId(0),
    };

    let module = Module {
        types: type_table,
        oracles: vec![],
        actions: vec![],
        funcs: vec![FuncDecl::Body(body)],
        sigs: vec![sig],
        exports: std::collections::BTreeMap::new(),
    };

    (module, FuncId(0), param_widths)
}

// ============================================================================
// Helpers
// ============================================================================

/// Zero out bits above `width` in a `Constant`.
fn mask_const(c: Constant, width: usize) -> Constant {
    if width == 0 {
        return Constant { lo: 0, hi: 0 };
    }
    if width >= 256 {
        return c;
    }
    if width >= 128 {
        let hi_mask = if width == 256 {
            u128::MAX
        } else {
            (1u128 << (width - 128)) - 1
        };
        Constant { lo: c.lo, hi: c.hi & hi_mask }
    } else {
        let lo_mask = if width == 128 {
            u128::MAX
        } else {
            (1u128 << width) - 1
        };
        Constant { lo: c.lo & lo_mask, hi: 0 }
    }
}

// ============================================================================
// Proptest strategies (test-only)
// ============================================================================

#[cfg(test)]
pub use strategies::*;

#[cfg(test)]
mod strategies {
    use super::*;
    use crate::interpreter::ir::IrValue;
    use proptest::prelude::*;

    /// Generate a valid single-function VAFFLE module with matching inputs.
    ///
    /// Returns `(module, FuncId(0), inputs)`.
    pub fn gen_vaffle_and_inputs(
    ) -> impl Strategy<Value = (Module, FuncId, Vec<IrValue>)> {
        proptest::collection::vec(any::<u8>(), 0usize..=4usize).prop_flat_map(
            |raw_param_types| {
                // Compute per-param bit widths to generate matching inputs.
                let widths: Vec<usize> = raw_param_types
                    .iter()
                    .map(|&idx| {
                        primitive_width(PRIM_TYPES[idx as usize % PRIM_TYPES.len()])
                    })
                    .collect();
                let total_bits: usize = widths.iter().sum();

                let raw_stmts = proptest::collection::vec(
                    (
                        any::<u8>(),
                        any::<u32>(),
                        any::<u32>(),
                        any::<u128>(),
                        any::<u128>(),
                    ),
                    0usize..=8usize,
                );
                let input_bits =
                    proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts, input_bits).prop_map(move |(raw_stmts, input_bits)| {
                    let (module, func_id, _param_widths) =
                        interpret_vaffle(&raw_param_types, &raw_stmts);

                    // Split flat input bits into per-param IrValues.
                    let inputs: Vec<IrValue> = {
                        let mut off = 0;
                        widths
                            .iter()
                            .map(|&w| {
                                let v = input_bits[off..off + w].to_vec();
                                off += w;
                                v
                            })
                            .collect()
                    };

                    (module, func_id, inputs)
                })
            },
        )
    }

    /// Like [`gen_vaffle_and_inputs`] but uses [`interpret_vaffle_extended`] so
    /// the generated module may contain `StorageRead`/`StorageWrite` values.
    ///
    /// Used for property H (`store_forward_vaffle_module` preserves semantics).
    pub fn gen_vaffle_extended_and_inputs(
    ) -> impl Strategy<Value = (Module, FuncId, Vec<IrValue>)> {
        proptest::collection::vec(any::<u8>(), 0usize..=4usize).prop_flat_map(
            |raw_param_types| {
                let widths: Vec<usize> = raw_param_types
                    .iter()
                    .map(|&idx| {
                        primitive_width(PRIM_TYPES[idx as usize % PRIM_TYPES.len()])
                    })
                    .collect();
                let total_bits: usize = widths.iter().sum();

                let raw_stmts = proptest::collection::vec(
                    (
                        any::<u8>(),
                        any::<u32>(),
                        any::<u32>(),
                        any::<u128>(),
                        any::<u128>(),
                    ),
                    0usize..=8usize,
                );
                let input_bits =
                    proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts, input_bits).prop_map(move |(raw_stmts, input_bits)| {
                    let (module, func_id, _param_widths) =
                        interpret_vaffle_extended(&raw_param_types, &raw_stmts);

                    let inputs: Vec<IrValue> = {
                        let mut off = 0;
                        widths
                            .iter()
                            .map(|&w| {
                                let v = input_bits[off..off + w].to_vec();
                                off += w;
                                v
                            })
                            .collect()
                    };

                    (module, func_id, inputs)
                })
            },
        )
    }

    /// Two-block VAFFLE module with `StorageRead`/`StorageWrite` across blocks.
    ///
    /// Block 0 (entry) jumps unconditionally to Block 1, which returns all
    /// values.  Block 1's stmts may reference Block 0's `ValueId`s directly.
    /// This exercises cross-block store-to-load forwarding in
    /// `store_forward_vaffle_module`.
    pub fn gen_vaffle_multiblock_and_inputs(
    ) -> impl Strategy<Value = (Module, FuncId, Vec<IrValue>)> {
        proptest::collection::vec(any::<u8>(), 0usize..=4usize).prop_flat_map(
            |raw_param_types| {
                let widths: Vec<usize> = raw_param_types
                    .iter()
                    .map(|&idx| {
                        primitive_width(PRIM_TYPES[idx as usize % PRIM_TYPES.len()])
                    })
                    .collect();
                let total_bits: usize = widths.iter().sum();

                let raw_tuple = (
                    any::<u8>(),
                    any::<u32>(),
                    any::<u32>(),
                    any::<u128>(),
                    any::<u128>(),
                );
                let raw_stmts_b0 = proptest::collection::vec(raw_tuple.clone(), 0usize..=6usize);
                let raw_stmts_b1 = proptest::collection::vec(raw_tuple, 0usize..=6usize);
                let input_bits = proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts_b0, raw_stmts_b1, input_bits).prop_map(
                    move |(raw_stmts_b0, raw_stmts_b1, input_bits)| {
                        let (module, func_id, _param_widths) = interpret_vaffle_multiblock(
                            &raw_param_types,
                            &raw_stmts_b0,
                            &raw_stmts_b1,
                        );

                        let inputs: Vec<IrValue> = {
                            let mut off = 0;
                            widths
                                .iter()
                                .map(|&w| {
                                    let v = input_bits[off..off + w].to_vec();
                                    off += w;
                                    v
                                })
                                .collect()
                        };

                        (module, func_id, inputs)
                    },
                )
            },
        )
    }
}
