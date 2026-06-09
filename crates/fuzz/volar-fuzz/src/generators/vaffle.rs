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
use volar_ir_common::{Constant, IrType, OracleDecl, Stmt, StorageId, Type, TypeId, TypeTable};

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
        stmt_provs: vec![(); stmt_vids.len()],
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
        pre_init: vec![],
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
    // oracle declarations for this module (may be empty)
    oracle_decls: &[volar_ir_common::OracleDecl],
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

        let use_oracle = !oracle_decls.is_empty() && n_vars > 0 && (kind % 7 >= 5);

        if n_vars == 0 || kind % 7 == 0 {
            let type_idx = (a as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = type_table.primitive(ty);
            let w = primitive_width(ty);
            new_values.push(Value::Op(Stmt::Const(Constant { lo: c_lo, hi: c_hi }, tid)));
            stmt_vids.push(vid);
            var_info.push((tid, w, vid));
        } else if kind % 7 == 1 {
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
        } else if kind % 7 == 2 {
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
        } else if kind % 7 == 3 {
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
        } else if kind % 7 == 4 {
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
        } else if use_oracle {
            // ── OracleCall + OracleOutput[0] ─────────────────────────────────
            let oracle_idx = (a as usize) % oracle_decls.len();
            let decl = &oracle_decls[oracle_idx];

            let args: Vec<ValueId> = decl
                .params
                .iter()
                .map(|&param_tid| {
                    let matching = var_info.iter().find(|(tid, _, _)| *tid == param_tid);
                    let fallback = &var_info[(a as usize) % n_vars];
                    matching.unwrap_or(fallback).2
                })
                .collect();

            let output_tys = decl.results.clone();
            let result_ty = type_table.intern(IrType::Tuple(output_tys.clone()));

            // OracleCall aggregate at vid.
            new_values.push(Value::Op(Stmt::OracleCall {
                name: decl.name.clone(),
                args,
                output_tys: output_tys.clone(),
                result_ty,
            }));
            stmt_vids.push(vid);
            // aggregate NOT added to var_info

            // OracleOutput[0] at vid+1.
            if !output_tys.is_empty() {
                let out_tid = output_tys[0];
                let out_w = crate::interpreter::ir::bit_width(out_tid, type_table);
                let out_vid = ValueId(param_value_offset + stmt_vids.len());
                new_values.push(Value::Op(Stmt::OracleOutput {
                    call: vid,
                    idx: 0,
                    ty: out_tid,
                }));
                stmt_vids.push(out_vid);
                var_info.push((out_tid, out_w, out_vid));
            }
        }
    }

    (param_type_ids, param_widths, var_info, new_values, stmt_vids)
}

/// Build a small oracle declaration list from a raw seed byte.
fn build_vaffle_oracle_decls(type_table: &mut TypeTable, raw: u8) -> Vec<OracleDecl> {
    let n = (raw as usize) % 3;
    (0..n)
        .map(|i| {
            let param_ty = PRIM_TYPES[(raw as usize + i) % PRIM_TYPES.len()];
            let result_ty = PRIM_TYPES[(raw as usize + i + 1) % PRIM_TYPES.len()];
            OracleDecl {
                name: format!("o{i}"),
                params: vec![type_table.primitive(param_ty)],
                results: vec![type_table.primitive(result_ty)],
            }
        })
        .collect()
}

fn interpret_vaffle_extended_inner(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
    block_id: BlockId,
) -> (Module, FuncId, Vec<usize>) {
    let mut type_table = volar_ir_common::TypeTable::new();

    let oracle_seed = raw_param_type_idxs.first().copied().unwrap_or(0);
    let oracle_decls = build_vaffle_oracle_decls(&mut type_table, oracle_seed);

    let (param_type_ids, param_widths, var_info, new_values, stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            raw_param_type_idxs,
            raw_stmts,
            block_id,
            vec![],
            0,
            &oracle_decls,
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
        stmt_provs: vec![(); stmt_vids.len()],
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
        oracles: oracle_decls,
        actions: vec![],
        funcs: vec![FuncDecl::Body(body)],
        sigs: vec![sig],
        exports: std::collections::BTreeMap::new(),
        pre_init: vec![],
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

    let oracle_seed = raw_param_type_idxs.first().copied().unwrap_or(0);
    let oracle_decls = build_vaffle_oracle_decls(&mut type_table, oracle_seed);

    // --- Block 0 ---
    let (param_type_ids, param_widths, var_info_after_b0, b0_values, b0_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            raw_param_type_idxs,
            raw_stmts_b0,
            BlockId(0),
            vec![],
            0,
            &oracle_decls,
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
        &oracle_decls,
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
        stmt_provs: vec![(); b0_stmt_vids.len()],
        stmts: b0_stmt_vids,
        terminator: b0_term,
    };
    let block1 = Block {
        params: vec![],
        stmt_provs: vec![(); b1_stmt_vids.len()],
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
        pre_init: vec![],
    };

    (module, FuncId(0), param_widths)
}

// ============================================================================
// Diamond interpreter: four-block diamond CFG for multi-predecessor tests
// ============================================================================

/// Build a four-block diamond VAFFLE module:
///
/// ```text
///       B0 (entry)
///      /         \
///    B1           B2
///      \         /
///       B3 (merge)
/// ```
///
/// - **Block 0**: function params + stmts from `raw_stmts_b0`.
///   Terminates with `IfNonzero { cond: ValueId(0), then→B1, else→B2 }`.
///   First param is forced to `Bit` (1-bit) for the branch condition.
/// - **Block 1** / **Block 2**: no params; stmts reference B0's `ValueId`s
///   directly (VAFFLE `ValueId`s are global).  Terminate with `Jump(B3)`.
/// - **Block 3**: no params; stmts reference only B0's `ValueId`s (B1/B2 are
///   conditional so their values are unsafe to use in B3).
///   Terminates with `Return { values: B0_vids ++ B3_vids }` — skips B1/B2.
pub fn interpret_vaffle_diamond(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts_b0: &[RawIrStmt],
    raw_stmts_b1: &[RawIrStmt],
    raw_stmts_b2: &[RawIrStmt],
    raw_stmts_b3: &[RawIrStmt],
) -> (Module, FuncId, Vec<usize>) {
    use volar_ir_common::Type;

    let mut type_table = volar_ir_common::TypeTable::new();

    let oracle_seed = raw_param_type_idxs.first().copied().unwrap_or(0);
    let oracle_decls = build_vaffle_oracle_decls(&mut type_table, oracle_seed);

    // Ensure at least one Bit param for the condition.
    let mut adjusted_params: Vec<RawTypeIdx> = raw_param_type_idxs.to_vec();
    if adjusted_params.is_empty() {
        adjusted_params.push(0);
    }
    adjusted_params[0] = 0; // force Bit

    // ── Block 0 (entry) ──────────────────────────────────────────────────────
    let (param_type_ids, param_widths, var_info_after_b0, b0_values, b0_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            &adjusted_params,
            raw_stmts_b0,
            BlockId(0),
            vec![],
            0,
            &oracle_decls,
        );
    let b0_value_count = b0_values.len();

    // B0 terminator: IfNonzero on first param (ValueId(0)), then→B1, else→B2.
    let b0_term = Terminator::IfNonzero {
        cond: ValueId(0),
        then_target: Target { block: BlockId(1), args: vec![] },
        else_target: Target { block: BlockId(2), args: vec![] },
    };

    // ── Block 1 (true branch) ────────────────────────────────────────────────
    // No params — references B0 values via global ValueIds.
    let (_b1_ptids, _b1_pwidths, _var_info_after_b1, b1_values, b1_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            &[], // no params
            raw_stmts_b1,
            BlockId(1),
            var_info_after_b0.clone(), // carry B0's var_info
            b0_value_count,
            &oracle_decls,
        );
    let b1_value_count = b1_values.len();

    // B1 → B3
    let b1_term = Terminator::Jump(Target { block: BlockId(3), args: vec![] });

    // ── Block 2 (false branch) ───────────────────────────────────────────────
    let b2_value_offset = b0_value_count + b1_value_count;
    let (_b2_ptids, _b2_pwidths, _var_info_after_b2, b2_values, b2_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            &[], // no params
            raw_stmts_b2,
            BlockId(2),
            var_info_after_b0.clone(), // carry B0's var_info (not B1's)
            b2_value_offset,
            &oracle_decls,
        );
    let b2_value_count = b2_values.len();

    // B2 → B3
    let b2_term = Terminator::Jump(Target { block: BlockId(3), args: vec![] });

    // ── Block 3 (merge) ──────────────────────────────────────────────────────
    let b3_value_offset = b0_value_count + b1_value_count + b2_value_count;
    let (_b3_ptids, _b3_pwidths, var_info_after_b3, b3_values, b3_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            &[], // no params
            raw_stmts_b3,
            BlockId(3),
            var_info_after_b0.clone(), // only B0 vars (not B1/B2 — conditional)
            b3_value_offset,
            &oracle_decls,
        );

    // Return B0 + B3 values only (B1/B2 values are conditional).
    let mut ret_vids: Vec<ValueId> = (0..b0_value_count).map(ValueId).collect();
    for (_, _, vid) in &var_info_after_b3 {
        // Add B3 stmt values (those with ValueId >= b3_value_offset).
        if vid.0 >= b3_value_offset {
            ret_vids.push(*vid);
        }
    }
    // Also add B3 stmt vids that might be void (StorageWrite) and thus not in var_info.
    for &vid in &b3_stmt_vids {
        if vid.0 >= b3_value_offset && !ret_vids.contains(&vid) {
            ret_vids.push(vid);
        }
    }

    let b3_term = Terminator::Return { values: ret_vids.clone() };

    // Merge all values into a single flat array.
    let mut all_values: Vec<Value> = b0_values;
    all_values.extend(b1_values);
    all_values.extend(b2_values);
    all_values.extend(b3_values);

    // Build SigDecl.
    let n_params = param_type_ids.len();
    let fallback_tid = param_type_ids
        .first()
        .copied()
        .unwrap_or_else(|| type_table.primitive(Type::Bit));

    // Collect all var_info entries for type lookups.
    let all_var_info: Vec<(TypeId, usize, ValueId)> = {
        let mut v = var_info_after_b0.clone();
        // Add B3 entries that are new.
        for entry in &var_info_after_b3 {
            if entry.2 .0 >= b3_value_offset {
                v.push(*entry);
            }
        }
        v
    };

    let sig_results: Vec<TypeId> = ret_vids
        .iter()
        .map(|vid| {
            if vid.0 < n_params {
                param_type_ids[vid.0]
            } else {
                all_var_info
                    .iter()
                    .find(|(_, _, v)| v.0 == vid.0)
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
        stmt_provs: vec![(); b0_stmt_vids.len()],
        stmts: b0_stmt_vids,
        terminator: b0_term,
    };
    let block1 = Block {
        params: vec![],
        stmt_provs: vec![(); b1_stmt_vids.len()],
        stmts: b1_stmt_vids,
        terminator: b1_term,
    };
    let block2 = Block {
        params: vec![],
        stmt_provs: vec![(); b2_stmt_vids.len()],
        stmts: b2_stmt_vids,
        terminator: b2_term,
    };
    let block3 = Block {
        params: vec![],
        stmt_provs: vec![(); b3_stmt_vids.len()],
        stmts: b3_stmt_vids,
        terminator: b3_term,
    };

    let body = FuncBody {
        sig: SigId(0),
        blocks: vec![block0, block1, block2, block3],
        values: all_values,
        entry: BlockId(0),
    };

    let module = Module {
        types: type_table,
        oracles: oracle_decls,
        actions: vec![],
        funcs: vec![FuncDecl::Body(body)],
        sigs: vec![sig],
        exports: std::collections::BTreeMap::new(),
        pre_init: vec![],
    };

    (module, FuncId(0), param_widths)
}

// ============================================================================
// Two-function VAFFLE module
// ============================================================================

/// Build a two-function VAFFLE module exercising `Value::Call` and `Value::Output`.
///
/// - **func_1** (FuncId(1)): a pure single-block function built from `raw_stmts_f1`.
///   Its result types are exactly the types of all its values (params + stmts).
/// - **func_0** (FuncId(0)): a single-block function built from `raw_stmts_f0`.
///   After its own stmts, it emits a `Value::Call { func: FuncId(1), args }` whose
///   arguments are chosen from func_0's available vars (matched by type; fallback
///   to const zero when no match). Each call output is then extracted with
///   `Value::Output`. The return includes func_0's own values plus call outputs.
///
/// Returns `(module, FuncId(0), func_0_param_widths)`.
pub fn interpret_vaffle_two_func(
    raw_param_type_idxs_f0: &[RawTypeIdx],
    raw_stmts_f0: &[RawIrStmt],
    raw_param_type_idxs_f1: &[RawTypeIdx],
    raw_stmts_f1: &[RawIrStmt],
) -> (Module, FuncId, Vec<usize>) {
    use volar_ir_common::Type;

    let mut type_table = volar_ir_common::TypeTable::new();

    let oracle_seed = raw_param_type_idxs_f0.first().copied().unwrap_or(0);
    let oracle_decls = build_vaffle_oracle_decls(&mut type_table, oracle_seed);

    // ── Build func_1 (callee) ─────────────────────────────────────────────────
    // func_1 is a pure function; no calls inside it.
    let (f1_param_type_ids, _f1_param_widths, f1_var_info, f1_values, f1_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            raw_param_type_idxs_f1,
            raw_stmts_f1,
            BlockId(0),
            vec![],
            0,
            &oracle_decls,
        );

    let f1_total = f1_values.len();
    let f1_all_vids: Vec<ValueId> = (0..f1_total).map(ValueId).collect();

    // func_1 sig: params from param types; results are the types of all values.
    let fallback_bit_tid = type_table.primitive(Type::Bit);
    let f1_result_tys: Vec<TypeId> = (0..f1_total)
        .map(|i| {
            f1_var_info
                .iter()
                .find(|(_, _, vid)| vid.0 == i)
                .map(|(tid, _, _)| *tid)
                .unwrap_or(fallback_bit_tid)
        })
        .collect();

    let f1_sig = SigDecl { params: f1_param_type_ids.clone(), results: f1_result_tys.clone() };

    let f1_block = Block {
        params: f1_param_type_ids
            .iter()
            .enumerate()
            .map(|(i, &tid)| (ValueId(i), tid))
            .collect(),
        stmt_provs: vec![(); f1_stmt_vids.len()],
        stmts: f1_stmt_vids,
        terminator: Terminator::Return { values: f1_all_vids },
    };

    let f1_body = FuncBody {
        sig: SigId(1),
        blocks: vec![f1_block],
        values: f1_values,
        entry: BlockId(0),
    };

    // ── Build func_0 (caller) ─────────────────────────────────────────────────
    let (f0_param_type_ids, f0_param_widths, mut f0_var_info, mut f0_values, mut f0_stmt_vids) =
        build_vaffle_extended_block(
            &mut type_table,
            raw_param_type_idxs_f0,
            raw_stmts_f0,
            BlockId(0),
            vec![],
            0,
            &oracle_decls,
        );

    // Emit Value::Call { func: FuncId(1), args }.
    // For each of func_1's param types, pick a var from f0_var_info with matching type,
    // or fall back to a Const zero of the right type.
    let call_vid = ValueId(f0_values.len());
    let call_args: Vec<ValueId> = f1_param_type_ids
        .iter()
        .enumerate()
        .map(|(i, &needed_tid)| {
            // Find first var in f0 with matching type.
            if let Some(&(_, _, vid)) = f0_var_info.iter().find(|(tid, _, _)| *tid == needed_tid) {
                vid
            } else {
                // Emit a Const zero of the right type and use it.
                let const_vid = ValueId(f0_values.len() + i);
                // We'll push Const values after the loop; for now just record the vid.
                // Actually we need to do this inside the loop — push now.
                let _ = const_vid; // avoid unused warning
                // Recompute: we may have pushed consts already above this iteration.
                let cur_vid = ValueId(f0_values.len());
                let w = f0_var_info
                    .iter()
                    .find(|(tid, _, _)| *tid == needed_tid)
                    .map(|(_, w, _)| *w)
                    .unwrap_or(1);
                f0_values.push(Value::Op(Stmt::Const(
                    volar_ir_common::Constant { lo: 0, hi: 0 },
                    needed_tid,
                )));
                f0_stmt_vids.push(cur_vid);
                f0_var_info.push((needed_tid, w, cur_vid));
                cur_vid
            }
        })
        .collect();

    // Now emit the Call value itself.
    f0_values.push(Value::Call { func: FuncId(1), args: call_args });
    f0_stmt_vids.push(call_vid);

    // Emit Value::Output for each of func_1's outputs.
    let mut output_vids: Vec<ValueId> = Vec::new();
    for (out_idx, &out_tid) in f1_result_tys.iter().enumerate() {
        let out_vid = ValueId(f0_values.len());
        let w = type_table
            .0
            .get(out_tid.0 as usize)
            .map(|ty| match ty {
                volar_ir_common::IrType::Primitive(t) => primitive_width(*t),
                _ => 1,
            })
            .unwrap_or(1);
        f0_values.push(Value::Output { value: call_vid, idx: out_idx });
        f0_stmt_vids.push(out_vid);
        f0_var_info.push((out_tid, w, out_vid));
        output_vids.push(out_vid);
    }

    // func_0 returns all its own values + call outputs (call_vid itself is a sentinel, skip it).
    let f0_own_vids: Vec<ValueId> = (0..call_vid.0).map(ValueId).collect();
    let mut f0_return_vids = f0_own_vids;
    f0_return_vids.extend_from_slice(&output_vids);

    // Build func_0 result types for the sig.
    let f0_result_tys: Vec<TypeId> = f0_return_vids
        .iter()
        .map(|vid| {
            f0_var_info
                .iter()
                .find(|(_, _, v)| v.0 == vid.0)
                .map(|(tid, _, _)| *tid)
                .unwrap_or(fallback_bit_tid)
        })
        .collect();

    let f0_sig = SigDecl { params: f0_param_type_ids.clone(), results: f0_result_tys };

    let f0_block = Block {
        params: f0_param_type_ids
            .iter()
            .enumerate()
            .map(|(i, &tid)| (ValueId(i), tid))
            .collect(),
        stmt_provs: vec![(); f0_stmt_vids.len()],
        stmts: f0_stmt_vids,
        terminator: Terminator::Return { values: f0_return_vids },
    };

    let f0_body = FuncBody {
        sig: SigId(0),
        blocks: vec![f0_block],
        values: f0_values,
        entry: BlockId(0),
    };

    let module = Module {
        types: type_table,
        oracles: oracle_decls,
        actions: vec![],
        funcs: vec![FuncDecl::Body(f0_body), FuncDecl::Body(f1_body)],
        sigs: vec![f0_sig, f1_sig],
        exports: std::collections::BTreeMap::new(),
        pre_init: vec![],
    };

    (module, FuncId(0), f0_param_widths)
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

    /// Four-block diamond VAFFLE module with `StorageRead`/`StorageWrite`.
    ///
    /// B0 branches on the first Bit param to B1 (true) or B2 (false).
    /// B1 and B2 both merge into B3.  This exercises multi-predecessor
    /// store-to-load forwarding in `store_forward_vaffle_module`.
    pub fn gen_vaffle_diamond_and_inputs(
    ) -> impl Strategy<Value = (Module, FuncId, Vec<IrValue>)> {
        // At least 1 param (first forced to Bit for the condition).
        proptest::collection::vec(any::<u8>(), 1usize..=4usize).prop_flat_map(
            |raw_param_types| {
                let mut adjusted = raw_param_types.clone();
                adjusted[0] = 0; // force Bit
                let widths: Vec<usize> = adjusted
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
                let raw_stmts_b0 = proptest::collection::vec(raw_tuple.clone(), 0usize..=4usize);
                let raw_stmts_b1 = proptest::collection::vec(raw_tuple.clone(), 0usize..=4usize);
                let raw_stmts_b2 = proptest::collection::vec(raw_tuple.clone(), 0usize..=4usize);
                let raw_stmts_b3 = proptest::collection::vec(raw_tuple, 0usize..=4usize);
                let input_bits = proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts_b0, raw_stmts_b1, raw_stmts_b2, raw_stmts_b3, input_bits).prop_map(
                    move |(raw_stmts_b0, raw_stmts_b1, raw_stmts_b2, raw_stmts_b3, input_bits)| {
                        let (module, func_id, _param_widths) = interpret_vaffle_diamond(
                            &raw_param_types,
                            &raw_stmts_b0,
                            &raw_stmts_b1,
                            &raw_stmts_b2,
                            &raw_stmts_b3,
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
