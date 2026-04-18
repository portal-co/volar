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

use volar_ir::ir::{IRBlock, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRVarId};
use volar_ir_common::{Constant, IrType, Stmt, Type, TypeId, TypeTable};

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
            (Stmt::Poly { coeffs, constant: c }, v0_tid, v0_w)
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
}
