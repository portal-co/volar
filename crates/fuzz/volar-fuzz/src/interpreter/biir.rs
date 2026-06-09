//! Concrete evaluator for `BIrBlocks<()>`.
//!
//! Evaluates a Boolar circuit block-by-block, following terminators, and
//! returning the output bit-vector when a `Return` target is reached.
//!
//! # Limitations
//! - Action and RNG statements **panic** — the generator never produces them.
//! - Oracle calls use the FNV-1a hash oracle (hashing the oracle name + inputs).
//! - A self-looping block (movfuscated form) will execute until it reaches a
//!   `Return` branch or until `MAX_ITERS` is exceeded, at which point
//!   `None` is returned.

use crate::generators::oracle::hash_oracle;
use crate::interpreter::ir::bits_to_u64;
use std::collections::BTreeMap;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};

/// Storage map for BIR evaluation: keyed by `(StorageId, address_as_u64)`.
///
/// Each BIR variable is a single bit; the `addr: Vec<IRVarId>` in
/// `StorageRead`/`StorageWrite` is an N-bit address collapsed to a `u64`
/// via `bits_to_u64`.
pub type BIrStorageMap = BTreeMap<(StorageId, u64), bool>;

/// Maximum number of times block 0 may be re-entered (loop guard for
/// movfuscated / iterating circuits).
pub const MAX_ITERS: usize = 512;

/// Evaluate `blocks` starting at block 0 with `inputs` bound to the entry
/// block's params.
///
/// Returns the output bits (the `args` of the first `Jmp(Return)` reached),
/// or `None` if execution loops more than [`MAX_ITERS`] times without
/// terminating.
pub fn eval_biir(blocks: &BIrBlocks<()>, inputs: &[bool]) -> Option<Vec<bool>> {
    eval_biir_with_limit(blocks, inputs, MAX_ITERS)
}

/// Like [`eval_biir`] but with a caller-supplied loop limit.
///
/// Returns `None` if block 0 is re-entered more than `max_iters` times.
pub fn eval_biir_with_limit(
    blocks: &BIrBlocks<()>,
    inputs: &[bool],
    max_iters: usize,
) -> Option<Vec<bool>> {
    let mut loop_count = 0usize;
    let mut current_block: usize = 0;
    let mut current_inputs: Vec<bool> = inputs.to_vec();
    let mut storage: BIrStorageMap = BTreeMap::new();

    loop {
        if current_block == 0 {
            loop_count += 1;
            if loop_count > max_iters {
                return None;
            }
        }

        let block = &blocks.blocks[current_block];
        let output = eval_block(block, &current_inputs, &mut storage)?;

        match output {
            BlockResult::Return(bits) => return Some(bits),
            BlockResult::Jump { target, args } => {
                current_block = target;
                current_inputs = args;
            }
        }
    }
}

/// Result of executing a single block to its terminator.
enum BlockResult {
    Return(Vec<bool>),
    Jump { target: usize, args: Vec<bool> },
}

/// Evaluate one `BIrBlock`, returning either a `Return` result or the next
/// block index and its argument values.
fn eval_block(block: &BIrBlock<()>, params: &[bool], storage: &mut BIrStorageMap) -> Option<BlockResult> {
    assert_eq!(
        params.len(),
        block.params as usize,
        "input count mismatch: block expects {} params, got {}",
        block.params,
        params.len()
    );

    // Build the variable table: params first (indices 0..params), then stmts.
    let mut vars: BTreeMap<u32, bool> = BTreeMap::new();
    // Side-table for OracleCall aggregates: var_id → Vec<bool> (one bit per output bit).
    let mut oracle_agg: BTreeMap<u32, Vec<bool>> = BTreeMap::new();

    for (i, &v) in params.iter().enumerate() {
        vars.insert(i as u32, v);
    }

    let base = block.params;
    for (i, stmt) in block.stmts.iter().enumerate() {
        let id = base + i as u32;
        let val = eval_stmt(stmt, id, &vars, &mut oracle_agg, storage);
        vars.insert(id, val);
    }

    // Evaluate terminator.
    let result = match &block.terminator {
        BIrTerminator::Jmp(target) => resolve_target(target, &vars),
        BIrTerminator::CondJmp {
            val,
            then_target,
            else_target,
        } => {
            let cond = get(&vars, val);
            if cond {
                resolve_target(then_target, &vars)
            } else {
                resolve_target(else_target, &vars)
            }
        }
        _ => panic!("eval_biir: unhandled BIrTerminator variant — add evaluation for this variant"),
    };
    Some(result)
}

/// Evaluate a single boolean gate statement.
///
/// `stmt_id` is the SSA variable ID assigned to this stmt (used as the oracle agg key).
fn eval_stmt(
    stmt: &BIrStmt,
    stmt_id: u32,
    vars: &BTreeMap<u32, bool>,
    oracle_agg: &mut BTreeMap<u32, Vec<bool>>,
    storage: &mut BIrStorageMap,
) -> bool {
    match stmt {
        BIrStmt::Zero => false,
        BIrStmt::One => true,
        BIrStmt::And(a, b) => get(vars, a) & get(vars, b),
        BIrStmt::Or(a, b) => get(vars, a) | get(vars, b),
        BIrStmt::Xor(a, b) => get(vars, a) ^ get(vars, b),
        BIrStmt::Not(a) => !get(vars, a),
        BIrStmt::OracleCall { name, args, num_bits } => {
            // Hash the oracle name bytes as a u32 seed.
            let oracle_idx: u32 = name.bytes().fold(0u32, |h, b| h.wrapping_mul(31).wrapping_add(b as u32));
            let flat_inputs: Vec<bool> = args.iter().map(|v| get(vars, v)).collect();
            let out = hash_oracle(oracle_idx, &flat_inputs, *num_bits);
            oracle_agg.insert(stmt_id, out);
            false // sentinel; actual bits extracted via OracleBit
        }
        BIrStmt::OracleBit { call, bit } => {
            oracle_agg
                .get(&call.0)
                .and_then(|bits| bits.get(*bit))
                .copied()
                .unwrap_or(false)
        }
        BIrStmt::ActionCall { .. } => panic!("eval_biir: ActionCall not supported"),
        BIrStmt::ActionBit { .. } => panic!("eval_biir: ActionBit not supported"),
        BIrStmt::Rng { .. } => panic!("eval_biir: Rng not supported"),
        BIrStmt::StorageRead { storage: store_id, bit_width: _, addr } => {
            let addr_bits: Vec<bool> = addr.iter().map(|v| get(vars, v)).collect();
            let addr_u64 = bits_to_u64(&addr_bits);
            storage.get(&(*store_id, addr_u64)).copied().unwrap_or(false)
        }
        BIrStmt::StorageWrite { storage: store_id, src, bit_width: _, addr } => {
            let src_val = get(vars, src);
            let addr_bits: Vec<bool> = addr.iter().map(|v| get(vars, v)).collect();
            let addr_u64 = bits_to_u64(&addr_bits);
            storage.insert((*store_id, addr_u64), src_val);
            false // dummy zero bit
        }
        _ => panic!("eval_biir: unhandled BIrStmt variant — add evaluation for this variant"),
    }
}

/// Resolve a `BIrTarget` to a `BlockResult`.
fn resolve_target(target: &BIrTarget, vars: &BTreeMap<u32, bool>) -> BlockResult {
    let args: Vec<bool> = target.args.iter().map(|id| get(vars, id)).collect();
    match &target.block {
        IRBlockTargetId::Return => BlockResult::Return(args),
        IRBlockTargetId::Block(b) => BlockResult::Jump {
            target: b.0 as usize,
            args,
        },
        IRBlockTargetId::Dyn(_) => panic!("eval_biir: Dyn jump target not supported"),
        _ => panic!("eval_biir: unhandled IRBlockTargetId variant — add evaluation for this variant"),
    }
}

/// Look up a variable, panicking with a clear message if it is missing.
fn get(vars: &BTreeMap<u32, bool>, id: &IRVarId) -> bool {
    *vars
        .get(&id.0)
        .unwrap_or_else(|| panic!("eval_biir: var {} not found in current scope", id.0))
}

// ============================================================================
// Unit tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
    use volar_ir::ir::{IRBlockId, IRBlockTargetId, IRVarId};

    fn ret_target(args: Vec<IRVarId>) -> BIrTarget {
        BIrTarget {
            block: IRBlockTargetId::Return,
            args,
        }
    }

    fn block_target(idx: u32, args: Vec<IRVarId>) -> BIrTarget {
        BIrTarget {
            block: IRBlockTargetId::Block(IRBlockId(idx)),
            args,
        }
    }

    fn simple_block(params: u32, stmts: Vec<BIrStmt>, term: BIrTerminator) -> BIrBlock<()> {
        let n = stmts.len();
        BIrBlock {
            params,
            stmt_provs: vec![(); n],
            stmts,
            terminator: term,
        }
    }

    #[test]
    fn const_zero_returns_false() {
        // Single block: emit Zero, return it.
        let v0 = IRVarId(0); // param
        let v1 = IRVarId(1); // stmt: Zero
        let blocks = BIrBlocks { blocks: vec![simple_block(
            1,
            vec![BIrStmt::Zero],
            BIrTerminator::Jmp(ret_target(vec![v1])),
        )], pre_init: vec![] };
        assert_eq!(eval_biir(&blocks, &[true]), Some(vec![false]));
    }

    #[test]
    fn identity_circuit_passes_input() {
        // Single block: return the single param unchanged.
        let v0 = IRVarId(0);
        let blocks = BIrBlocks { blocks: vec![simple_block(
            1,
            vec![],
            BIrTerminator::Jmp(ret_target(vec![v0])),
        )], pre_init: vec![] };
        assert_eq!(eval_biir(&blocks, &[true]), Some(vec![true]));
        assert_eq!(eval_biir(&blocks, &[false]), Some(vec![false]));
    }

    #[test]
    fn not_gate_inverts_input() {
        let v0 = IRVarId(0);
        let v1 = IRVarId(1); // NOT v0
        let blocks = BIrBlocks { blocks: vec![simple_block(
            1,
            vec![BIrStmt::Not(v0)],
            BIrTerminator::Jmp(ret_target(vec![v1])),
        )], pre_init: vec![] };
        assert_eq!(eval_biir(&blocks, &[false]), Some(vec![true]));
        assert_eq!(eval_biir(&blocks, &[true]), Some(vec![false]));
    }

    #[test]
    fn and_gate() {
        let v0 = IRVarId(0);
        let v1 = IRVarId(1);
        let v2 = IRVarId(2); // AND(v0, v1)
        let blocks = BIrBlocks { blocks: vec![simple_block(
            2,
            vec![BIrStmt::And(v0, v1)],
            BIrTerminator::Jmp(ret_target(vec![v2])),
        )], pre_init: vec![] };
        assert_eq!(eval_biir(&blocks, &[false, false]), Some(vec![false]));
        assert_eq!(eval_biir(&blocks, &[false, true]), Some(vec![false]));
        assert_eq!(eval_biir(&blocks, &[true, false]), Some(vec![false]));
        assert_eq!(eval_biir(&blocks, &[true, true]), Some(vec![true]));
    }

    #[test]
    fn xor_gate() {
        let v0 = IRVarId(0);
        let v1 = IRVarId(1);
        let v2 = IRVarId(2); // XOR(v0, v1)
        let blocks = BIrBlocks { blocks: vec![simple_block(
            2,
            vec![BIrStmt::Xor(v0, v1)],
            BIrTerminator::Jmp(ret_target(vec![v2])),
        )], pre_init: vec![] };
        assert_eq!(eval_biir(&blocks, &[false, false]), Some(vec![false]));
        assert_eq!(eval_biir(&blocks, &[false, true]), Some(vec![true]));
        assert_eq!(eval_biir(&blocks, &[true, false]), Some(vec![true]));
        assert_eq!(eval_biir(&blocks, &[true, true]), Some(vec![false]));
    }

    #[test]
    fn multi_block_cond_jump() {
        // Block 0: param v0; if v0 goto block 1 else block 2.
        // Block 1: no params; return One.
        // Block 2: no params; return Zero.
        let v0 = IRVarId(0);
        let v_one = IRVarId(0); // block 1: Zero stmts, so first stmt is at index 0
        // block 1 has 0 params, so first stmt var is IRVarId(0)
        let blocks = BIrBlocks { blocks: vec![
            simple_block(
                1,
                vec![],
                BIrTerminator::CondJmp {
                    val: v0,
                    then_target: block_target(1, vec![]),
                    else_target: block_target(2, vec![]),
                },
            ),
            simple_block(
                0,
                vec![BIrStmt::One],
                BIrTerminator::Jmp(ret_target(vec![IRVarId(0)])),
            ),
            simple_block(
                0,
                vec![BIrStmt::Zero],
                BIrTerminator::Jmp(ret_target(vec![IRVarId(0)])),
            ),
        ], pre_init: vec![] };
        assert_eq!(eval_biir(&blocks, &[true]), Some(vec![true]));
        assert_eq!(eval_biir(&blocks, &[false]), Some(vec![false]));
    }

    #[test]
    fn self_loop_terminates() {
        // Movfuscated single block: loop if v0=1, return [] if v0=0.
        // params: (loop_again: bool, _ignored: bool)
        // stmts: Zero (const false)
        // terminator: if v0 (loop_again) goto self with args [Zero, Zero], else return []
        let v_loop = IRVarId(0);
        let v_zero = IRVarId(2); // params=2, stmt 0 → IRVarId(2)
        let blocks = BIrBlocks { blocks: vec![simple_block(
            2,
            vec![BIrStmt::Zero],
            BIrTerminator::CondJmp {
                val: v_loop,
                then_target: block_target(0, vec![v_zero, v_zero]),
                else_target: ret_target(vec![]),
            },
        )], pre_init: vec![] };
        // When loop_again=false, returns immediately.
        assert_eq!(eval_biir(&blocks, &[false, false]), Some(vec![]));
        // When loop_again=true → loops once (next iter gets loop_again=false via Zero) → returns.
        assert_eq!(eval_biir(&blocks, &[true, false]), Some(vec![]));
    }
}
