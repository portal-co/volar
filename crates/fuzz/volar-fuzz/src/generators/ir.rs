//! Generator for structurally valid single-block `IRBlocks<()>`.
//!
//! # Scope
//!
//! - Only primitive types `Bit`, `_8`, `_16`, `_32`, `_64`, `_128` are used.
//! - Only `Const`, linear `Poly` (XOR of vars of equal width), `Rol`, and `Ror`
//!   stmts are generated.
//! - No oracle, action, rng, or storage stmts.
//! - Single-block DAG only (`Jmp(Return)` terminator).
//!
//! # Raw-data approach
//!
//! Like [`crate::gen::biir`], generation is split into:
//! 1. **Raw data** — integer tuples with no structural constraints.
//! 2. **Interpretation** — [`interpret_ir`] converts raw data into a valid
//!    `(IRBlocks<()>, IRTypes)` by clamping indices and matching types.

use volar_ir::ir::{IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRVarId};
use volar_ir_common::{Constant, IrType, Stmt, StorageId, Type, TypeId, TypeTable};

use crate::interpreter::ir::primitive_width;

// ============================================================================
// Constants
// ============================================================================

/// Primitive types used in generated IR (chosen for u128-representability).
pub const PRIM_TYPES: &[Type] = &[
    Type::Bit,
    Type::_8,
    Type::_16,
    Type::_32,
    Type::_64,
    Type::_128,
];

// ============================================================================
// Raw data types
// ============================================================================

/// Index into [`PRIM_TYPES`] for a primitive type choice.
pub type RawTypeIdx = u8;

/// Raw data for a single IR statement.
/// `(kind, a, b, c_lo, c_hi)` — kind selects the stmt variant; a/b are
/// var/type indices; c_lo/c_hi form a 256-bit constant.
pub type RawIrStmt = (u8, u32, u32, u128, u128);

// ============================================================================
// Interpreter: raw data → (IRBlocks, IRTypes, input widths)
// ============================================================================

/// Convert raw data into a single-block `(IRBlocks<()>, IRTypes)`.
///
/// Returns `(blocks, types, param_widths)` where `param_widths[i]` is the
/// number of bits expected for input `i`.  The terminator returns all vars
/// (params + stmts).
pub fn interpret_ir(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
) -> (IRBlocks<()>, TypeTable, Vec<usize>) {
    let mut types = TypeTable::new();

    // Intern param types and record their widths.
    let param_type_ids: Vec<TypeId> = raw_param_type_idxs
        .iter()
        .map(|&idx| types.primitive(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
        .collect();

    let param_widths: Vec<usize> = param_type_ids
        .iter()
        .map(|&tid| match &types.0[tid.0 as usize] {
            IrType::Primitive(t) => primitive_width(*t),
            _ => unreachable!(),
        })
        .collect();

    // Track (TypeId, bit_width) for each var (params first).
    let mut var_info: Vec<(TypeId, usize)> = param_type_ids
        .iter()
        .zip(param_widths.iter())
        .map(|(&tid, &w)| (tid, w))
        .collect();

    let mut stmts: Vec<IRStmt> = Vec::new();

    for (kind, a, b, c_lo, c_hi) in raw_stmts.iter().copied() {
        let n_vars = var_info.len();

        let (stmt, result_tid, result_w) = if n_vars == 0 || kind % 3 == 0 {
            // ── Const ───────────────────────────────────────────────────────
            let type_idx = (a as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = types.primitive(ty);
            let w = primitive_width(ty);
            (Stmt::Const(Constant { lo: c_lo, hi: c_hi }, tid), tid, w)
        } else if kind % 3 == 1 {
            // ── Linear Poly (XOR of 1–2 vars of the same width) ─────────────
            let v0_idx = (a as usize) % n_vars;
            let (v0_tid, v0_w) = var_info[v0_idx];

            // Find vars (indices) with the same bit-width as v0.
            let same_w: Vec<usize> = var_info
                .iter()
                .enumerate()
                .filter(|(_, (_, w))| *w == v0_w)
                .map(|(i, _)| i)
                .collect();

            let mut coeffs = std::collections::BTreeMap::new();
            coeffs.insert(vec![IRVarId(v0_idx as u32)], 1u8);

            if same_w.len() > 1 {
                let v1_pick = (b as usize) % same_w.len();
                let v1_idx = same_w[v1_pick];
                if v1_idx != v0_idx {
                    // Keep keys sorted — IRVarId is ordered by .0
                    let mut key = vec![IRVarId(v0_idx as u32), IRVarId(v1_idx as u32)];
                    key.sort();
                    // Replace the single-var entry with the two-var monomial.
                    coeffs.clear();
                    coeffs.insert(key, 1u8);
                }
            }

            // Mask constant to the output width so it stays in-range.
            let c = mask_const(Constant { lo: c_lo, hi: c_hi }, v0_w);
            (Stmt::Poly { ty: v0_tid, coeffs, constant: c }, v0_tid, v0_w)
        } else {
            // ── Rol / Ror ────────────────────────────────────────────────────
            let v_idx = (a as usize) % n_vars;
            let (v_tid, v_w) = var_info[v_idx];
            let n_rot = if v_w > 0 { (b as usize) % v_w } else { 0 };
            let src = IRVarId(v_idx as u32);
            let stmt = if kind % 2 == 0 {
                Stmt::Rol { src, ty: v_tid, n: n_rot }
            } else {
                Stmt::Ror { src, ty: v_tid, n: n_rot }
            };
            (stmt, v_tid, v_w)
        };

        var_info.push((result_tid, result_w));
        stmts.push(stmt);
    }

    // Terminator: return every var (params + stmts).
    let total_vars = param_type_ids.len() + stmts.len();
    let ret_args: Vec<IRVarId> = (0..total_vars as u32).map(IRVarId).collect();
    let n = stmts.len();

    let block = IRBlock {
        params: param_type_ids,
        stmts,
        stmt_provs: vec![(); n],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: ret_args,
        },
    };

    (IRBlocks::new(vec![block]), types, param_widths)
}

/// Zero-out bits above `width` in a `Constant` (keeps c.lo if width <= 128).
fn mask_const(c: Constant, width: usize) -> Constant {
    if width == 0 {
        return Constant { lo: 0, hi: 0 };
    }
    if width >= 256 {
        return c;
    }
    if width >= 128 {
        let hi_mask = if width == 256 { u128::MAX } else { (1u128 << (width - 128)) - 1 };
        Constant { lo: c.lo, hi: c.hi & hi_mask }
    } else {
        let lo_mask = if width == 128 { u128::MAX } else { (1u128 << width) - 1 };
        Constant { lo: c.lo & lo_mask, hi: 0 }
    }
}

// ============================================================================
// Shared 5-way stmt builder for extended IR generation
// ============================================================================

/// Build stmts using the 5-way `kind % 5` dispatch:
///   0 = Const, 1 = Poly, 2 = Rol/Ror, 3 = StorageWrite (void), 4 = StorageRead
///
/// * `types`     — type table, may be extended with new primitive types.
/// * `var_info`  — `(TypeId, bit_width, IRVarId)` for all vars available as
///                 operands.  Updated in-place: non-void results are appended.
/// * `raw_stmts` — raw generation data.
/// * `stmt_base` — IRVarId offset for the first stmt (= number of block params
///                 in the current block).
///
/// Returns the built statement list.
fn build_extended_ir_stmts(
    types: &mut TypeTable,
    var_info: &mut Vec<(TypeId, usize, IRVarId)>,
    raw_stmts: &[RawIrStmt],
    stmt_base: u32,
) -> Vec<IRStmt> {
    let mut stmts: Vec<IRStmt> = Vec::new();

    for (kind, a, b, c_lo, c_hi) in raw_stmts.iter().copied() {
        let n_vars = var_info.len();
        let rv = IRVarId(stmt_base + stmts.len() as u32);

        if n_vars == 0 || kind % 5 == 0 {
            // ── Const ───────────────────────────────────────────────────────
            let type_idx = (a as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = types.primitive(ty);
            let w = primitive_width(ty);
            stmts.push(Stmt::Const(Constant { lo: c_lo, hi: c_hi }, tid));
            var_info.push((tid, w, rv));
        } else if kind % 5 == 1 {
            // ── Linear Poly (XOR of 1–2 vars of the same width) ─────────────
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
            stmts.push(Stmt::Poly { ty: v0_tid, coeffs, constant: c });
            var_info.push((v0_tid, v0_w, rv));
        } else if kind % 5 == 2 {
            // ── Rol / Ror ────────────────────────────────────────────────────
            let v_idx = (a as usize) % n_vars;
            let (v_tid, v_w, v_id) = var_info[v_idx];
            let n_rot = if v_w > 0 { (b as usize) % v_w } else { 0 };
            let stmt = if kind % 2 == 0 {
                Stmt::Rol { src: v_id, ty: v_tid, n: n_rot }
            } else {
                Stmt::Ror { src: v_id, ty: v_tid, n: n_rot }
            };
            stmts.push(stmt);
            var_info.push((v_tid, v_w, rv));
        } else if kind % 5 == 3 {
            // ── StorageWrite (void — not added to var_info) ──────────────────
            let store_id = StorageId(a % 4);
            let src_idx = (b as usize) % n_vars;
            let (src_tid, _, src_id) = var_info[src_idx];
            let addr_idx = (c_lo as usize % n_vars) as usize;
            let addr_id = var_info[addr_idx].2;
            stmts.push(Stmt::StorageWrite {
                storage: store_id,
                src: src_id,
                ty: src_tid,
                addr: addr_id,
            });
            // void result — do NOT add to var_info
        } else {
            // ── StorageRead ──────────────────────────────────────────────────
            let store_id = StorageId(a % 4);
            let type_idx = (b as usize) % PRIM_TYPES.len();
            let ty = PRIM_TYPES[type_idx];
            let tid = types.primitive(ty);
            let w = primitive_width(ty);
            let addr_idx = (c_lo as usize % n_vars) as usize;
            let addr_id = var_info[addr_idx].2;
            stmts.push(Stmt::StorageRead {
                storage: store_id,
                ty: tid,
                addr: addr_id,
            });
            var_info.push((tid, w, rv));
        }
    }

    stmts
}

// ============================================================================
// Extended interpreter: storage ops + JumpCond
// ============================================================================

/// Like [`interpret_ir`] but also emits `StorageRead`, `StorageWrite` stmts
/// and may produce a `JumpCond` terminator.
///
/// `kind % 5` selects:
///   0 = Const, 1 = Poly, 2 = Rol/Ror, 3 = StorageWrite (void), 4 = StorageRead
///
/// `var_info` tracks only non-void vars so they can safely be used as operands.
/// `StorageWrite` stmts still consume a slot (and var ID) in the block but are
/// not added to `var_info`.
pub fn interpret_ir_extended(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts: &[RawIrStmt],
) -> (IRBlocks<()>, TypeTable, Vec<usize>) {
    let mut types = TypeTable::new();

    let param_type_ids: Vec<TypeId> = raw_param_type_idxs
        .iter()
        .map(|&idx| types.primitive(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
        .collect();

    let param_widths: Vec<usize> = param_type_ids
        .iter()
        .map(|&tid| match &types.0[tid.0 as usize] {
            IrType::Primitive(t) => primitive_width(*t),
            _ => unreachable!(),
        })
        .collect();

    // var_info: (TypeId, bit_width, actual IRVarId) — only non-void vars.
    let mut var_info: Vec<(TypeId, usize, IRVarId)> = param_type_ids
        .iter()
        .zip(param_widths.iter())
        .enumerate()
        .map(|(i, (&tid, &w))| (tid, w, IRVarId(i as u32)))
        .collect();

    let n_params = param_type_ids.len();
    let stmts = build_extended_ir_stmts(&mut types, &mut var_info, raw_stmts, n_params as u32);

    // Terminator: JumpCond on first Bit-typed var if one exists, else Jmp(Return).
    let total_vars = n_params + stmts.len();
    let ret_args: Vec<IRVarId> = (0..total_vars as u32).map(IRVarId).collect();
    let n = stmts.len();

    let bit_tid = types.primitive(Type::Bit);
    let cond_var = var_info.iter().find(|(tid, _, _)| *tid == bit_tid).map(|(_, _, id)| *id);
    let terminator = if let Some(cond) = cond_var {
        IRTerminator::JumpCond {
            condition: cond,
            true_block: IRBlockTargetId::Return,
            true_args: ret_args.clone(),
            false_block: IRBlockTargetId::Return,
            false_args: ret_args,
        }
    } else {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args: ret_args }
    };

    let block = IRBlock {
        params: param_type_ids,
        stmts,
        stmt_provs: vec![(); n],
        terminator,
    };

    (IRBlocks::new(vec![block]), types, param_widths)
}

// ============================================================================
// Multi-block interpreter: two-block CFG for cross-block forwarding tests
// ============================================================================

/// Build a two-block `IRBlocks<()>`:
///
/// - **Block 0 (entry)**: function params + stmts from `raw_stmts_b0` (5-way
///   dispatch including `StorageRead`/`StorageWrite`).  Terminates with
///   `Jmp(Block(1), all_non_void_vars)`.
/// - **Block 1**: params = types of B0's non-void vars; stmts from
///   `raw_stmts_b1` (same 5-way dispatch using B1-local var IDs).  Terminates
///   with `Jmp(Return, all_b1_vars)`.
///
/// This layout guarantees Block 0 always executes before Block 1 (no branches),
/// making it a reliable test bed for cross-block store-to-load forwarding.
pub fn interpret_ir_multiblock(
    raw_param_type_idxs: &[RawTypeIdx],
    raw_stmts_b0: &[RawIrStmt],
    raw_stmts_b1: &[RawIrStmt],
) -> (IRBlocks<()>, TypeTable, Vec<usize>) {
    let mut types = TypeTable::new();

    // ── Block 0 ──────────────────────────────────────────────────────────────
    let param_type_ids: Vec<TypeId> = raw_param_type_idxs
        .iter()
        .map(|&idx| types.primitive(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
        .collect();

    let param_widths: Vec<usize> = param_type_ids
        .iter()
        .map(|&tid| match &types.0[tid.0 as usize] {
            IrType::Primitive(t) => primitive_width(*t),
            _ => unreachable!(),
        })
        .collect();

    let n_params = param_type_ids.len();
    let mut var_info_b0: Vec<(TypeId, usize, IRVarId)> = param_type_ids
        .iter()
        .zip(param_widths.iter())
        .enumerate()
        .map(|(i, (&tid, &w))| (tid, w, IRVarId(i as u32)))
        .collect();

    let stmts_b0 = build_extended_ir_stmts(
        &mut types, &mut var_info_b0, raw_stmts_b0, n_params as u32,
    );

    // B0 terminator: jump to Block(1), passing all non-void vars as args.
    let b0_jump_args: Vec<IRVarId> = var_info_b0.iter().map(|(_, _, id)| *id).collect();
    let n_b0_stmts = stmts_b0.len();
    let b0_term = IRTerminator::Jmp {
        func: IRBlockTargetId::Block(IRBlockId(1)),
        args: b0_jump_args,
    };

    // ── Block 1 ──────────────────────────────────────────────────────────────
    // B1 params correspond to B0's non-void vars.
    let b1_param_types: Vec<TypeId> = var_info_b0.iter().map(|(tid, _, _)| *tid).collect();
    let n_b1_params = b1_param_types.len();

    // B1 var_info: same types/widths as B0's non-void vars, but with B1-local
    // IRVarIds (0..n_b1_params).
    let mut var_info_b1: Vec<(TypeId, usize, IRVarId)> = var_info_b0
        .iter()
        .enumerate()
        .map(|(i, (tid, w, _))| (*tid, *w, IRVarId(i as u32)))
        .collect();

    let stmts_b1 = build_extended_ir_stmts(
        &mut types, &mut var_info_b1, raw_stmts_b1, n_b1_params as u32,
    );

    // B1 terminator: return all B1 vars (params + stmts).
    let total_b1_vars = n_b1_params + stmts_b1.len();
    let b1_ret_args: Vec<IRVarId> = (0..total_b1_vars as u32).map(IRVarId).collect();
    let n_b1_stmts = stmts_b1.len();
    let b1_term = IRTerminator::Jmp {
        func: IRBlockTargetId::Return,
        args: b1_ret_args,
    };

    let block0 = IRBlock {
        params: param_type_ids,
        stmts: stmts_b0,
        stmt_provs: vec![(); n_b0_stmts],
        terminator: b0_term,
    };
    let block1 = IRBlock {
        params: b1_param_types,
        stmts: stmts_b1,
        stmt_provs: vec![(); n_b1_stmts],
        terminator: b1_term,
    };

    (IRBlocks::new(vec![block0, block1]), types, param_widths)
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

    /// Generate a valid single-block `IRBlocks<()>` with matching inputs.
    ///
    /// Returns `(blocks, types, inputs)`.
    pub fn gen_ir_and_inputs(
    ) -> impl Strategy<Value = (IRBlocks<()>, TypeTable, Vec<IrValue>)> {
        // Generate param type indices.
        proptest::collection::vec(any::<u8>(), 0usize..=4usize).prop_flat_map(
            |raw_param_types| {
                // Compute how many bits each param needs so we can generate inputs.
                let widths: Vec<usize> = raw_param_types
                    .iter()
                    .map(|&idx| primitive_width(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
                    .collect();
                let total_bits: usize = widths.iter().sum();

                // Generate raw stmts + flat input bits together.
                let raw_stmts = proptest::collection::vec(
                    (any::<u8>(), any::<u32>(), any::<u32>(), any::<u128>(), any::<u128>()),
                    0usize..=8usize,
                );
                let input_bits = proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts, input_bits).prop_map(move |(raw_stmts, input_bits)| {
                    let (blocks, types, _param_widths) =
                        interpret_ir(&raw_param_types, &raw_stmts);

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

                    (blocks, types, inputs)
                })
            },
        )
    }

    /// Like [`gen_ir_and_inputs`] but uses [`interpret_ir_extended`] so the
    /// generated IR may contain `StorageRead`/`StorageWrite` stmts and a
    /// `JumpCond` terminator.
    ///
    /// Used for property H (`store_forward_ir_blocks` preserves semantics).
    pub fn gen_ir_extended_and_inputs(
    ) -> impl Strategy<Value = (IRBlocks<()>, TypeTable, Vec<IrValue>)> {
        proptest::collection::vec(any::<u8>(), 0usize..=4usize).prop_flat_map(
            |raw_param_types| {
                let widths: Vec<usize> = raw_param_types
                    .iter()
                    .map(|&idx| primitive_width(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
                    .collect();
                let total_bits: usize = widths.iter().sum();

                let raw_stmts = proptest::collection::vec(
                    (any::<u8>(), any::<u32>(), any::<u32>(), any::<u128>(), any::<u128>()),
                    0usize..=8usize,
                );
                let input_bits = proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts, input_bits).prop_map(move |(raw_stmts, input_bits)| {
                    let (blocks, types, _param_widths) =
                        interpret_ir_extended(&raw_param_types, &raw_stmts);

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

                    (blocks, types, inputs)
                })
            },
        )
    }

    /// Two-block `IRBlocks<()>` with `StorageRead`/`StorageWrite` across blocks.
    ///
    /// Block 0 (entry) jumps unconditionally to Block 1, passing all non-void
    /// vars.  Block 1 has params matching those vars, builds its own stmts, and
    /// returns all B1 vars.  This exercises cross-block store-to-load
    /// forwarding in `store_forward_ir_blocks`.
    pub fn gen_ir_multiblock_and_inputs(
    ) -> impl Strategy<Value = (IRBlocks<()>, TypeTable, Vec<IrValue>)> {
        proptest::collection::vec(any::<u8>(), 0usize..=4usize).prop_flat_map(
            |raw_param_types| {
                let widths: Vec<usize> = raw_param_types
                    .iter()
                    .map(|&idx| primitive_width(PRIM_TYPES[idx as usize % PRIM_TYPES.len()]))
                    .collect();
                let total_bits: usize = widths.iter().sum();

                let raw_tuple = (any::<u8>(), any::<u32>(), any::<u32>(), any::<u128>(), any::<u128>());
                let raw_stmts_b0 = proptest::collection::vec(raw_tuple.clone(), 0usize..=6usize);
                let raw_stmts_b1 = proptest::collection::vec(raw_tuple, 0usize..=6usize);
                let input_bits = proptest::collection::vec(any::<bool>(), total_bits);

                (raw_stmts_b0, raw_stmts_b1, input_bits).prop_map(
                    move |(raw_stmts_b0, raw_stmts_b1, input_bits)| {
                        let (blocks, types, _param_widths) = interpret_ir_multiblock(
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

                        (blocks, types, inputs)
                    },
                )
            },
        )
    }
}
