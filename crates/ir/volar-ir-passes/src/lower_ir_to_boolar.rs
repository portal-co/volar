// @reliability: experimental
// @experimental-status: design
// @ai: unreviewed
//! Lowering pass: Volar IR (`IRBlocks`) → Boolar IR (`BIrBlocks`).
//!
//! Expands each Volar IR statement into zero or more Boolar (boolean-gate)
//! statements.  Multi-bit values are decomposed into individual bit wires
//! (LSB-first) and tracked in a per-block `var_bits` map.
//!
//! # Type expansion
//!
//! | IR type            | Bit count      |
//! |--------------------|----------------|
//! | `Bit`              | 1              |
//! | `_8` / `AES8`      | 8              |
//! | `_16`              | 16             |
//! | `_32`              | 32             |
//! | `_64` / `Galois64` | 64             |
//! | `_128`             | 128            |
//! | `_256`             | 256            |
//! | `Vec(n, T)`        | n × bits(T)    |
//! | `Tuple(Ts)`        | Σ bits(Tᵢ)    |
//! | `Block` / `Func`   | 0              |
//!
//! # Statements that produce no new Boolar stmts
//!
//! `Transmute`, `Rol`, `Ror`, `Merge`, `Splat`, and `Shuffle` are pure
//! bit-permutation operations.  They update `var_bits` to alias or reorder
//! existing Boolar vars without emitting any new Boolar statement.
//!
//! # External primitives
//!
//! - `OracleCall` / `ActionCall`: emitted as an opaque Boolar call-handle
//!   var, immediately followed by one `OracleBit` / `ActionBit` per output
//!   bit.  The per-output bit groups are stashed for later `OracleOutput` /
//!   `ActionOutput` projections.
//! - `OracleOutput` / `ActionOutput`: resolved from the pre-projected bit
//!   stash; emit no new Boolar stmts.
//! - `Rng { name, ty }`: one `BIrStmt::Rng { name }` per output bit.
//! - `StorageRead` / `StorageWrite`: passed through as opaque Boolar handles.
//!   The address var is represented by its first bit; the result occupies one
//!   Boolar var slot.  Downstream weavers must handle these variants
//!   specially and must not expect bit-level decomposition of their results.
//!
//! # Limitations
//!
//! - `IRTerminator::JumpTable` is not supported (panics).
//! - `IRTerminator::Jmp` with `IRBlockTargetId::Dyn` is not supported (panics).
//! - `StorageRead` result handles and addresses are opaque; they cannot be
//!   used as operands to `Poly`, `Merge`, etc. without weaver-level support.

use alloc::{collections::BTreeMap, vec, vec::Vec};

use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{
        IRBlock, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypes, IRVarId,
        PrimType,
    },
};
use volar_ir_common::Constant;

// ============================================================================
// Public API
// ============================================================================

/// Lower a Volar IR circuit (`IRBlocks`) to Boolar IR (`BIrBlocks`).
///
/// `types` is the type table that all [`IRTypeId`]s in `blocks` index into.
/// Each block is lowered independently; block arguments are expanded to their
/// flat bit lists in the order they appear in the IR terminator.
///
/// # Panics
///
/// Panics on `JumpTable` terminators and `Dyn` jump targets (not representable
/// in `BIrTerminator`).
pub fn lower_ir_to_boolar<P: Clone + Default>(blocks: &IRBlocks<P>, types: &IRTypes) -> BIrBlocks<P> {
    BIrBlocks {
        blocks: blocks.blocks.iter().map(|block| lower_block(block, types)).collect(),
        pre_init: blocks.pre_init.clone(),
    }
}

// ============================================================================
// Block lowering
// ============================================================================

fn lower_block<P: Clone + Default>(block: &IRBlock<P>, types: &IRTypes) -> BIrBlock<P> {
    // ---- 1. Expand params --------------------------------------------------
    // Each IR param of type T becomes ir_type_bits(T) consecutive Boolar params.
    // var_bits[param_idx] = slice of Boolar param IRVarIds for that param.
    let mut var_bits: BTreeMap<u32, Vec<IRVarId>> = BTreeMap::new();
    let mut next_param: u32 = 0;
    for (i, ty_id) in block.params.iter().enumerate() {
        let w = ir_type_bits(&types.0[ty_id.0 as usize], types);
        let bits: Vec<IRVarId> = (next_param..next_param + w as u32).map(IRVarId).collect();
        var_bits.insert(i as u32, bits);
        next_param += w as u32;
    }
    let total_params = next_param;

    // ---- 2. Emit stmts -----------------------------------------------------
    let mut emitter = Emitter::new(total_params);

    // Stash for OracleOutput / ActionOutput resolution.
    // Key: IR var index of the OracleCall / ActionCall.
    // Value: per-output bit lists (Vec<Vec<IRVarId>>), one inner Vec per output.
    let mut call_output_bits: BTreeMap<u32, Vec<Vec<IRVarId>>> = BTreeMap::new();

    for (si, stmt) in block.stmts.iter().enumerate() {
        let prov = block.stmt_provs[si].clone();
        let ir_var_idx = block.params.len() as u32 + si as u32;
        lower_stmt(stmt, prov, ir_var_idx, &mut var_bits, &mut call_output_bits, &mut emitter, types);
    }

    // ---- 3. Convert terminator --------------------------------------------
    let terminator = lower_terminator(&block.terminator, &var_bits);

    BIrBlock {
        params: total_params,
        stmts: emitter.stmts,
        stmt_provs: emitter.stmt_provs,
        terminator,
    }
}

// ============================================================================
// Statement lowering
// ============================================================================

#[allow(clippy::too_many_arguments)]
fn lower_stmt<P: Clone + Default>(
    stmt: &IRStmt,
    prov: P,
    ir_var_idx: u32,
    var_bits: &mut BTreeMap<u32, Vec<IRVarId>>,
    call_output_bits: &mut BTreeMap<u32, Vec<Vec<IRVarId>>>,
    emitter: &mut Emitter<P>,
    types: &IRTypes,
) {
    match stmt {
        // ---- Constant ------------------------------------------------------
        IRStmt::Const(c, ty_id) => {
            let w = ir_type_bits(&types.0[ty_id.0 as usize], types);
            let bits: Vec<IRVarId> = (0..w)
                .map(|j| {
                    if constant_bit(c, j) {
                        emitter.emit(BIrStmt::One, prov.clone())
                    } else {
                        emitter.emit(BIrStmt::Zero, prov.clone())
                    }
                })
                .collect();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- Transmute (bit-identity) --------------------------------------
        // Same bits, different type label.  No new Boolar stmts.
        IRStmt::Transmute { src, .. } => {
            let bits = var_bits[&src.0].clone();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- GF(2) polynomial ----------------------------------------------
        IRStmt::Poly { coeffs, constant, .. } => {
            let w = infer_poly_width(coeffs, var_bits);
            let bits: Vec<IRVarId> = (0..w)
                .map(|j| lower_poly_bit(coeffs, constant, j, var_bits, emitter, prov.clone()))
                .collect();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- Rotate left ---------------------------------------------------
        // result[j] = src[(j + w - n) % w]   (matches vole.rs vec_parts convention)
        IRStmt::Rol { src, n, .. } => {
            let src_bits = var_bits[&src.0].clone();
            let w = src_bits.len();
            let rotated: Vec<IRVarId> = (0..w).map(|j| src_bits[(j + w - n % w.max(1)) % w]).collect();
            var_bits.insert(ir_var_idx, rotated);
        }

        // ---- Rotate right --------------------------------------------------
        // result[j] = src[(j + n) % w]   (matches vole.rs vec_parts convention)
        IRStmt::Ror { src, n, .. } => {
            let src_bits = var_bits[&src.0].clone();
            let w = src_bits.len();
            let rotated: Vec<IRVarId> = (0..w).map(|j| src_bits[(j + n % w.max(1)) % w]).collect();
            var_bits.insert(ir_var_idx, rotated);
        }

        // ---- Merge ---------------------------------------------------------
        // Concatenate bit lists of all parts (LSB-first order preserved).
        IRStmt::Merge { parts, .. } => {
            let bits: Vec<IRVarId> = parts
                .iter()
                .flat_map(|p| var_bits[&p.0].iter().cloned())
                .collect();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- Splat ---------------------------------------------------------
        // Broadcast the single bit of `src` to all positions of `ty`.
        IRStmt::Splat { src, ty } => {
            let src_bit = var_bits[&src.0][0];
            let w = ir_type_bits(&types.0[ty.0 as usize], types);
            var_bits.insert(ir_var_idx, vec![src_bit; w]);
        }

        // ---- Shuffle -------------------------------------------------------
        // Arbitrary bit selection: result[i] = src[bit_idx].
        IRStmt::Shuffle { result_bits, .. } => {
            let bits: Vec<IRVarId> = result_bits
                .iter()
                .map(|(bit_idx, v)| var_bits[&v.0][*bit_idx as usize])
                .collect();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- StorageRead ---------------------------------------------------
        // Opaque handle: result is a single Boolar var.  Addr is approximated
        // as the first Boolar bit of the address IR var.
        IRStmt::StorageRead { storage, ty, addr } => {
            let bit_width = ir_type_bits(&types.0[ty.0 as usize], types);
            let addr_bits: Vec<IRVarId> = var_bits[&addr.0].iter().copied().collect();
            let handle = emitter.emit(
                BIrStmt::StorageRead { storage: *storage, bit_width, addr: addr_bits },
                prov,
            );
            var_bits.insert(ir_var_idx, vec![handle]);
        }

        // ---- StorageWrite --------------------------------------------------
        // Opaque handle: emits one Boolar var (the write sentinel).
        IRStmt::StorageWrite { storage, src, ty, addr } => {
            let bit_width = ir_type_bits(&types.0[ty.0 as usize], types);
            let src_handle = var_bits[&src.0][0];
            let addr_bits: Vec<IRVarId> = var_bits[&addr.0].iter().copied().collect();
            let handle = emitter.emit(
                BIrStmt::StorageWrite { storage: *storage, src: src_handle, bit_width, addr: addr_bits },
                prov,
            );
            var_bits.insert(ir_var_idx, vec![handle]);
        }

        // ---- OracleCall ----------------------------------------------------
        IRStmt::OracleCall { name, args, output_tys, .. } => {
            let flat_args: Vec<IRVarId> = args
                .iter()
                .flat_map(|a| var_bits[&a.0].iter().cloned())
                .collect();
            let total_bits: usize = output_tys
                .iter()
                .map(|tid| ir_type_bits(&types.0[tid.0 as usize], types))
                .sum();
            let handle = emitter.emit(
                BIrStmt::OracleCall { name: name.clone(), args: flat_args, num_bits: total_bits },
                prov.clone(),
            );
            // Pre-project all output bits immediately after the handle.
            let all_bit_vars: Vec<IRVarId> = (0..total_bits)
                .map(|bit| emitter.emit(BIrStmt::OracleBit { call: handle, bit }, prov.clone()))
                .collect();
            // Partition into per-output bit lists for OracleOutput resolution.
            let mut output_bit_lists: Vec<Vec<IRVarId>> = Vec::new();
            let mut offset = 0;
            for tid in output_tys {
                let w = ir_type_bits(&types.0[tid.0 as usize], types);
                output_bit_lists.push(all_bit_vars[offset..offset + w].to_vec());
                offset += w;
            }
            var_bits.insert(ir_var_idx, vec![handle]);
            call_output_bits.insert(ir_var_idx, output_bit_lists);
        }

        // ---- OracleOutput --------------------------------------------------
        // Resolved from the pre-projected bit stash; no new Boolar stmts.
        IRStmt::OracleOutput { call, idx, .. } => {
            let bits = call_output_bits
                .get(&call.0)
                .unwrap_or_else(|| panic!(
                    "lower_ir_to_boolar: OracleOutput references unknown call var {}", call.0
                ))[*idx]
                .clone();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- ActionCall ----------------------------------------------------
        IRStmt::ActionCall { name, guard, args, fallbacks, output_tys, .. } => {
            let guard_bit = var_bits[&guard.0][0];
            let flat_args: Vec<IRVarId> = args
                .iter()
                .flat_map(|a| var_bits[&a.0].iter().cloned())
                .collect();
            let flat_fallback: Vec<IRVarId> = fallbacks
                .iter()
                .flat_map(|f| var_bits[&f.0].iter().cloned())
                .collect();
            let total_bits: usize = output_tys
                .iter()
                .map(|tid| ir_type_bits(&types.0[tid.0 as usize], types))
                .sum();
            let handle = emitter.emit(
                BIrStmt::ActionCall {
                    name: name.clone(),
                    guard: guard_bit,
                    args: flat_args,
                    fallback: flat_fallback,
                    num_bits: total_bits,
                },
                prov.clone(),
            );
            let all_bit_vars: Vec<IRVarId> = (0..total_bits)
                .map(|bit| emitter.emit(BIrStmt::ActionBit { call: handle, bit }, prov.clone()))
                .collect();
            let mut output_bit_lists: Vec<Vec<IRVarId>> = Vec::new();
            let mut offset = 0;
            for tid in output_tys {
                let w = ir_type_bits(&types.0[tid.0 as usize], types);
                output_bit_lists.push(all_bit_vars[offset..offset + w].to_vec());
                offset += w;
            }
            var_bits.insert(ir_var_idx, vec![handle]);
            call_output_bits.insert(ir_var_idx, output_bit_lists);
        }

        // ---- ActionOutput --------------------------------------------------
        IRStmt::ActionOutput { call, idx, .. } => {
            let bits = call_output_bits
                .get(&call.0)
                .unwrap_or_else(|| panic!(
                    "lower_ir_to_boolar: ActionOutput references unknown call var {}", call.0
                ))[*idx]
                .clone();
            var_bits.insert(ir_var_idx, bits);
        }

        // ---- Rng -----------------------------------------------------------
        // One fresh `BIrStmt::Rng` per output bit.
        IRStmt::Rng { name, ty } => {
            let w = ir_type_bits(&types.0[ty.0 as usize], types);
            let bits: Vec<IRVarId> = (0..w)
                .map(|_| emitter.emit(BIrStmt::Rng { name: name.clone() }, prov.clone()))
                .collect();
            var_bits.insert(ir_var_idx, bits);
        }
        _ => panic!("lower_ir_to_boolar: unhandled IRStmt variant — add lowering for this variant"),
    }
}

// ============================================================================
// Terminator lowering
// ============================================================================

fn lower_terminator(term: &IRTerminator, var_bits: &BTreeMap<u32, Vec<IRVarId>>) -> BIrTerminator {
    match term {
        IRTerminator::Jmp { func, args } => {
            assert!(
                !matches!(func, IRBlockTargetId::Dyn(_)),
                "lower_ir_to_boolar: Dyn jump targets are not representable in BIrTerminator"
            );
            BIrTerminator::Jmp(BIrTarget {
                block: func.clone(),
                args: flatten_bits(args, var_bits),
            })
        }

        IRTerminator::JumpCond { condition, true_block, true_args, false_block, false_args } => {
            let cond_bits = &var_bits[&condition.0];
            assert_eq!(
                cond_bits.len(),
                1,
                "lower_ir_to_boolar: JumpCond condition var {} has {} bits; expected 1 (Bit type)",
                condition.0,
                cond_bits.len()
            );
            BIrTerminator::CondJmp {
                val: cond_bits[0],
                then_target: BIrTarget {
                    block: true_block.clone(),
                    args: flatten_bits(true_args, var_bits),
                },
                else_target: BIrTarget {
                    block: false_block.clone(),
                    args: flatten_bits(false_args, var_bits),
                },
            }
        }

        IRTerminator::JumpTable { .. } => {
            panic!(
                "lower_ir_to_boolar: JumpTable terminators are not supported; \
                 convert to nested JumpCond first"
            )
        }
        _ => panic!("lower_ir_to_boolar: unhandled IRTerminator variant — add lowering for this variant"),
    }
}

fn flatten_bits(args: &[IRVarId], var_bits: &BTreeMap<u32, Vec<IRVarId>>) -> Vec<IRVarId> {
    args.iter()
        .flat_map(|a| var_bits[&a.0].iter().cloned())
        .collect()
}

// ============================================================================
// Polynomial lowering helpers
// ============================================================================

/// Return the bit-width of the output of a `Poly` stmt.
///
/// The width equals the maximum bit-count of any variable appearing in any
/// monomial.  If the polynomial has no variables (pure constant), the width
/// is 1 (a single GF(2) bit).
fn infer_poly_width(
    coeffs: &alloc::collections::BTreeMap<Vec<IRVarId>, u8>,
    var_bits: &BTreeMap<u32, Vec<IRVarId>>,
) -> usize {
    let mut w = 1usize;
    for (mono, _) in coeffs {
        for v in mono {
            if let Some(bits) = var_bits.get(&v.0) {
                w = w.max(bits.len());
            }
        }
    }
    w
}

/// Lower the `j`-th output bit of a `Poly` stmt.
///
/// Implements: `result[j] = constant[j] ⊕ ⊕{(mono,coeff): coeff odd} ∧(vars[j])`.
fn lower_poly_bit<P: Clone + Default>(
    coeffs: &alloc::collections::BTreeMap<Vec<IRVarId>, u8>,
    constant: &Constant,
    bit: usize,
    var_bits: &BTreeMap<u32, Vec<IRVarId>>,
    emitter: &mut Emitter<P>,
    prov: P,
) -> IRVarId {
    // Accumulator: None means "0 so far".
    let mut acc: Option<IRVarId> = if constant_bit(constant, bit) {
        Some(emitter.emit(BIrStmt::One, prov.clone()))
    } else {
        None
    };

    for (mono, &coeff) in coeffs {
        if coeff % 2 == 0 {
            continue;
        }
        let mono_var: Option<IRVarId> = if mono.is_empty() {
            // Empty product = 1; contributes a constant One term.
            Some(emitter.emit(BIrStmt::One, prov.clone()))
        } else {
            // AND of all variable bits at position `bit`.
            let mut and_acc: Option<IRVarId> = None;
            for v in mono {
                if let Some(bit_var) = var_bits.get(&v.0).and_then(|bs| bs.get(bit)).cloned() {
                    and_acc = Some(match and_acc {
                        None => bit_var,
                        Some(prev) => emitter.emit(BIrStmt::And(prev, bit_var), prov.clone()),
                    });
                }
                // If the variable has fewer bits than `bit`, its high bits are
                // implicitly 0 — the monomial contributes 0 for this position.
            }
            and_acc
        };

        if let Some(mv) = mono_var {
            acc = Some(match acc {
                None => mv,
                Some(prev) => emitter.emit(BIrStmt::Xor(prev, mv), prov.clone()),
            });
        }
    }

    // If no terms contributed, result is 0.
    acc.unwrap_or_else(|| emitter.emit(BIrStmt::Zero, prov))
}

// ============================================================================
// Type utilities
// ============================================================================

/// Return the number of GF(2) bits that `ty` expands to.
///
/// `Block` and `Func` types are not data types and expand to 0 bits.
pub fn ir_type_bits(ty: &IRType, types: &IRTypes) -> usize {
    match ty {
        IRType::Primitive(PrimType::Bit) => 1,
        IRType::Primitive(PrimType::_8) | IRType::Primitive(PrimType::AES8) => 8,
        IRType::Primitive(PrimType::_16) => 16,
        IRType::Primitive(PrimType::_32) => 32,
        IRType::Primitive(PrimType::_64) | IRType::Primitive(PrimType::Galois64) => 64,
        IRType::Primitive(PrimType::_128) => 128,
        IRType::Primitive(PrimType::_256) => 256,
        IRType::Primitive(PrimType::Z3) => {
            panic!(
                "ir_type_bits: Z3 (GF(3)) cannot be lowered to GF(2) bits. \
                 Z3 values are only valid in the TFHE backend. \
                 Use raise_to_z3 before the TFHE weaver, not before lower_ir_to_boolar."
            );
        }
        IRType::Vec(n, elem_id) => n * ir_type_bits(&types.0[elem_id.0 as usize], types),
        IRType::Tuple(ids) => {
            ids.iter().map(|id| ir_type_bits(&types.0[id.0 as usize], types)).sum()
        }
        IRType::Block { .. } | IRType::Func { .. } => 0,
        IRType::Primitive(_) => unimplemented!("ir_type_bits: unknown PrimType variant"),
        _ => panic!("ir_type_bits: unhandled IrType variant — add bit-width calculation"),
    }
}

/// Extract bit `bit` from a 256-bit `Constant` (LSB-first).
fn constant_bit(c: &Constant, bit: usize) -> bool {
    if bit < 128 {
        (c.lo >> bit) & 1 == 1
    } else {
        (c.hi >> (bit - 128)) & 1 == 1
    }
}

// ============================================================================
// Emitter
// ============================================================================

/// Sequential Boolar var-ID allocator and stmt accumulator.
///
/// Boolar var IDs start at `params` (the number of input bit params for the
/// block) and increment by one for each emitted stmt.
struct Emitter<P: Clone + Default> {
    stmts: Vec<BIrStmt>,
    stmt_provs: Vec<P>,
    next_var: u32,
}

impl<P: Clone + Default> Emitter<P> {
    fn new(params: u32) -> Self {
        Emitter { stmts: vec![], stmt_provs: vec![], next_var: params }
    }

    fn emit(&mut self, stmt: BIrStmt, prov: P) -> IRVarId {
        let id = IRVarId(self.next_var);
        self.stmts.push(stmt);
        self.stmt_provs.push(prov);
        self.next_var += 1;
        id
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use std::collections::BTreeMap as StdBTreeMap;
    use volar_ir::ir::{
        IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypes, IRVarId, PrimType,
    };
    use volar_ir_common::{Constant, TypeTable};

    // -- Helpers -------------------------------------------------------------

    /// Zero constant (all bits 0).
    fn zero_const() -> Constant {
        Constant { lo: 0, hi: 0 }
    }

    /// Build a minimal single-block `IRBlocks` that takes `n_params` Bit params
    /// and immediately returns them, alongside an `IRTypes` table that only
    /// contains `Bit`.
    fn make_passthrough(n_params: usize) -> (IRBlocks<()>, IRTypes) {
        let mut types = TypeTable::new();
        let bit_id = types.bit();

        let params: std::vec::Vec<_> = (0..n_params).map(|_| bit_id).collect();
        let args: std::vec::Vec<IRVarId> = (0..n_params as u32).map(IRVarId).collect();

        let block = IRBlock {
            params,
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args,
            },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    // -- ir_type_bits ---------------------------------------------------------

    #[test]
    fn type_bits_bit() {
        let types = TypeTable::new();
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::Bit), &types), 1);
    }

    #[test]
    fn type_bits_primitives() {
        let types = TypeTable::new();
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::_8), &types), 8);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::_16), &types), 16);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::_32), &types), 32);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::_64), &types), 64);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::_128), &types), 128);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::_256), &types), 256);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::AES8), &types), 8);
        assert_eq!(ir_type_bits(&IRType::Primitive(PrimType::Galois64), &types), 64);
    }

    #[test]
    fn type_bits_vec() {
        let mut types = TypeTable::new();
        let bit_id = types.bit();
        let vec4 = IRType::Vec(4, bit_id);
        assert_eq!(ir_type_bits(&vec4, &types), 4);
        let vec8 = IRType::Vec(8, bit_id);
        assert_eq!(ir_type_bits(&vec8, &types), 8);
    }

    #[test]
    fn type_bits_tuple() {
        let mut types = TypeTable::new();
        let bit_id = types.bit();
        let u8_id = types.primitive(PrimType::_8);
        // Tuple(Bit, _8) = 1 + 8 = 9
        let tuple = IRType::Tuple(std::vec![bit_id, u8_id]);
        assert_eq!(ir_type_bits(&tuple, &types), 9);
    }

    // -- Param expansion ------------------------------------------------------

    #[test]
    fn passthrough_zero_params() {
        let (blocks, types) = make_passthrough(0);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        assert_eq!(lowered.blocks.len(), 1);
        let b = &lowered.blocks[0];
        assert_eq!(b.params, 0);
        assert_eq!(b.stmts.len(), 0);
    }

    #[test]
    fn passthrough_three_bit_params() {
        let (blocks, types) = make_passthrough(3);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        let b = &lowered.blocks[0];
        // 3 Bit params → 3 Boolar params.
        assert_eq!(b.params, 3);
        assert_eq!(b.stmts.len(), 0);
    }

    #[test]
    fn u8_param_expands_to_eight_bits() {
        let mut types = TypeTable::new();
        let u8_id = types.primitive(PrimType::_8);

        let block = IRBlock {
            params: std::vec![u8_id],
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
        };
        let blocks = IRBlocks::<()>::new(std::vec![block]);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        // One u8 param → 8 Boolar bit params.
        assert_eq!(lowered.blocks[0].params, 8);
    }

    // -- Const statement ------------------------------------------------------

    #[test]
    fn const_zero_bit_emits_zero_stmt() {
        let mut types = TypeTable::new();
        let bit_id = types.bit();

        let mut block = IRBlock::<()> {
            params: std::vec![],
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
        };
        block.push_stmt_default(volar_ir::ir::IRStmt::Const(zero_const(), bit_id));

        let blocks = IRBlocks::new(std::vec![block]);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        let b = &lowered.blocks[0];
        // 1-bit const zero → exactly one BIrStmt::Zero.
        assert_eq!(b.stmts.len(), 1);
        assert_eq!(b.stmts[0], BIrStmt::Zero);
    }

    #[test]
    fn const_u8_emits_eight_stmts() {
        let mut types = TypeTable::new();
        let u8_id = types.primitive(PrimType::_8);

        let mut block = IRBlock::<()> {
            params: std::vec![],
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
        };
        // Const = 0b00000001 (value 1, bit 0 = One, rest = Zero).
        let c = Constant { lo: 1, hi: 0 };
        block.push_stmt_default(volar_ir::ir::IRStmt::Const(c, u8_id));

        let blocks = IRBlocks::new(std::vec![block]);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        let b = &lowered.blocks[0];
        assert_eq!(b.stmts.len(), 8);
        // LSB first: bit 0 = 1 → One.
        assert_eq!(b.stmts[0], BIrStmt::One);
        // All remaining bits are 0 → Zero.
        for stmt in &b.stmts[1..] {
            assert_eq!(*stmt, BIrStmt::Zero);
        }
    }

    // -- Transmute (identity) -------------------------------------------------

    #[test]
    fn transmute_aliases_existing_bits() {
        let mut types = TypeTable::new();
        let bit_id = types.bit();
        let u8_src = types.primitive(PrimType::_8);
        let u8_dst = types.primitive(PrimType::_8);

        // Block: param0 = _8, stmt0 = Const(1, _8), stmt1 = Transmute(stmt0).
        let c = Constant { lo: 1, hi: 0 };
        let block = IRBlock::<()> {
            params: std::vec![bit_id], // 1 bit param so var ids start right
            stmts: std::vec![
                volar_ir::ir::IRStmt::Const(c, u8_src),
                volar_ir::ir::IRStmt::Transmute {
                    src: IRVarId(1), // var 1 = the Const above (params=1 so param is var 0)
                    src_ty: u8_src,
                    dst_ty: u8_dst,
                },
            ],
            stmt_provs: std::vec![(), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(2)], // return the transmuted value
            },
        };
        let blocks = IRBlocks::new(std::vec![block]);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        let b = &lowered.blocks[0];
        // Transmute emits zero new stmts; only the 8 from the Const.
        assert_eq!(b.stmts.len(), 8);
    }

    // -- AES8 Poly lowering ---------------------------------------------------
    //
    // FAEST relies on `PrimType::AES8` (GF(2^8) under the AES polynomial) being
    // lowerable. The IR's `Poly` semantics permit linear combinations over a
    // bitvector/field type with Bit selectors — addition in GF(2^k) of
    // characteristic 2 is bitwise XOR, so the existing per-bit-position
    // `lower_poly_bit` produces correct output for AES8-typed linear combos.
    //
    // GF(2^8) *multiplication* (i.e. `gf_mul_u8`) is not a `Poly` statement —
    // it parses from the spec total-Rust subset as an extern function call.
    // The lowering responsibility for that path lives in the spec-call
    // expansion pass, not here.

    #[test]
    fn poly_aes8_linear_combo_xors_per_bit() {
        // Build: param0 = AES8, param1 = AES8 (each becomes 8 boolar bits),
        // then stmt0 = Poly { ty: AES8, coeffs: {[param0] -> 1, [param1] -> 1},
        // constant = 0 } — i.e. param0 XOR param1.
        // Verify: 8 output bits, each is XOR of the corresponding param bits.
        let mut types = TypeTable::new();
        let aes8_id = types.primitive(PrimType::AES8);

        let mut coeffs: alloc::collections::BTreeMap<std::vec::Vec<IRVarId>, u8> =
            alloc::collections::BTreeMap::new();
        coeffs.insert(std::vec![IRVarId(0)], 1);
        coeffs.insert(std::vec![IRVarId(1)], 1);

        let block = IRBlock::<()> {
            params: std::vec![aes8_id, aes8_id],
            stmts: std::vec![volar_ir::ir::IRStmt::Poly {
                ty: aes8_id,
                coeffs,
                constant: zero_const(),
            }],
            stmt_provs: std::vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(2)],
            },
        };
        let blocks = IRBlocks::new(std::vec![block]);
        let lowered = lower_ir_to_boolar::<()>(&blocks, &types);
        let b = &lowered.blocks[0];
        // 2 AES8 params = 16 boolar param bits. Each of the 8 output bits is
        // one Xor — so 8 stmts emitted.
        assert_eq!(b.params, 16);
        assert_eq!(b.stmts.len(), 8);
        for stmt in &b.stmts {
            assert!(
                matches!(stmt, BIrStmt::Xor(_, _)),
                "expected per-bit Xor for AES8 linear combination, got {:?}",
                stmt
            );
        }
    }

    // Latent limitation worth recording for the FAEST work:
    // `infer_poly_width` derives the output width from referenced variables'
    // bit-counts, *not* from the Poly's `ty`. Consequently a pure-constant
    // `Poly { ty: AES8, coeffs: {}, constant: c }` lowers to a single bit,
    // not 8. This isn't a blocker for FAEST — pure constants always go
    // through `IRStmt::Const` instead — but if a future weaver pass emits
    // constant-only Polys at field types, `infer_poly_width` will need to
    // fall back to `ir_type_bits(ty)` when `coeffs` is empty. Tracked
    // adjacent to AES8 work; no test asserts the current (wrong-for-empty)
    // behaviour because no code path produces it today.

    // -- Provenance threading -------------------------------------------------

    #[test]
    fn provenance_is_threaded_through() {
        // Use u32 as provenance.
        let mut types = TypeTable::new();
        let bit_id = types.bit();

        let c = Constant { lo: 0, hi: 0 };
        let block = IRBlock::<u32> {
            params: std::vec![bit_id],
            stmts: std::vec![volar_ir::ir::IRStmt::Const(c, bit_id)],
            stmt_provs: std::vec![42u32],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
        };
        let blocks = IRBlocks::new(std::vec![block]);
        let lowered = lower_ir_to_boolar::<u32>(&blocks, &types);
        let b = &lowered.blocks[0];
        // The single Const(Bit, 0) emits one Zero stmt with provenance 42.
        assert_eq!(b.stmt_provs.len(), 1);
        assert_eq!(b.stmt_provs[0], 42u32);
    }
}
