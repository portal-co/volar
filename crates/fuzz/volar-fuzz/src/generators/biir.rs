//! Generator for structurally valid `BIrBlocks<()>`.
//!
//! # Design
//!
//! All generation is split into two layers:
//!
//! 1. **Raw data** — plain integers that carry no structural invariants.
//! 2. **Interpretation** — [`interpret_biir`] converts raw data into a
//!    `BIrBlocks` by clamping all indices to their valid ranges.
//!
//! This keeps the proptest strategy trivially simple (just generate integer
//! tuples) while centralising all invariant-enforcement in one pure function.
//! The same interpretation logic is reused by the `arbitrary::biir` impl for
//! cargo-fuzz.
//!
//! # Structure invariants guaranteed
//!
//! - Variable references always point to previously defined SSA vars (params
//!   or earlier stmts in the same block).
//! - Jump targets are either `Return` or forward block indices (`block_idx+1
//!   ..n_blocks`), so the generated CFG is a DAG — no back-edges, guaranteed
//!   termination.
//! - Args passed to a jump target match the target block's param count.
//! - All Return terminators across all blocks have the **same arity**
//!   (`ret_arity`), which is required by both `movfuscate_biir` and
//!   `lower_to_circuit`.
//! - Only non-oracle/non-action/non-storage/non-rng stmts are generated
//!   (`Zero`, `One`, `And`, `Or`, `Xor`, `Not`).

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockId, IRBlockTargetId, IRVarId};

// ============================================================================
// Raw data types
// ============================================================================

/// Raw data for a single boolean-gate statement.
/// `(kind, a, b)` — interpreted as gate type + two operand indices.
pub type RawStmt = (u8, u32, u32);

/// Raw data for a single jump target.
/// `(choice, args)` — choice selects Return vs a forward block index;
/// args provide the var indices for the target's params.
pub type RawTarget = (u8, Vec<u32>);

/// Raw data for a block terminator.
/// `(kind, cond, then_target, else_target)`.
pub type RawTerm = (u8, u32, RawTarget, RawTarget);

/// Raw data for a complete block.
pub type RawBlock = (Vec<RawStmt>, RawTerm);

// ============================================================================
// Interpreter: raw data → BIrBlocks
// ============================================================================

/// Convert raw integer data into a structurally valid `BIrBlocks<()>`.
///
/// `param_counts[i]` is the number of params for block `i`.
/// `raw_blocks[i]` contains the raw stmts and terminator for block `i`.
/// The two vecs must have the same length (>= 1).
///
/// `raw_ret_arity` is a raw byte used to derive the consistent Return arity
/// (clamped to `min_vars` across all blocks so every Return can be satisfied).
pub fn interpret_biir(
    param_counts: Vec<u32>,
    raw_blocks: Vec<RawBlock>,
    raw_ret_arity: u8,
) -> BIrBlocks<()> {
    let n_blocks = param_counts.len();
    assert_eq!(raw_blocks.len(), n_blocks);
    assert!(n_blocks >= 1);

    // ── Phase 1: build all stmt lists ────────────────────────────────────────
    // We need to know n_vars per block before we can fix ret_arity.
    let all_stmts: Vec<Vec<BIrStmt>> = param_counts
        .iter()
        .zip(raw_blocks.iter())
        .map(|(&n_params, (raw_stmts, _))| {
            let mut stmts: Vec<BIrStmt> = Vec::with_capacity(raw_stmts.len());
            for &(kind, a, b) in raw_stmts {
                let n_avail = n_params + stmts.len() as u32;
                stmts.push(make_stmt(kind, a, b, n_avail));
            }
            stmts
        })
        .collect();

    // ── Fix ret_arity ────────────────────────────────────────────────────────
    // All Return targets across all blocks must produce the same number of
    // args.  Clamp to the minimum n_vars so every block can satisfy it.
    let min_vars: u32 = param_counts
        .iter()
        .zip(all_stmts.iter())
        .map(|(&np, stmts)| np + stmts.len() as u32)
        .min()
        .unwrap_or(0);

    let ret_arity: u32 = if min_vars == 0 {
        0
    } else {
        (raw_ret_arity as u32) % (min_vars + 1)
    };

    // ── Phase 2: build terminators + assemble blocks ─────────────────────────
    let blocks: Vec<BIrBlock<()>> = param_counts
        .iter()
        .zip(raw_blocks.iter())
        .zip(all_stmts.into_iter())
        .enumerate()
        .map(|(i, ((&n_params, (_, raw_term)), stmts))| {
            let n_vars = n_params + stmts.len() as u32;
            let terminator =
                make_term(i, n_blocks, n_vars, &param_counts, raw_term, ret_arity);
            let n = stmts.len();
            BIrBlock {
                params: n_params,
                stmts,
                stmt_provs: vec![(); n],
                terminator,
            }
        })
        .collect();

    BIrBlocks(blocks)
}

// ============================================================================
// Internal helpers
// ============================================================================

fn make_stmt(kind: u8, a: u32, b: u32, n_avail: u32) -> BIrStmt {
    if n_avail == 0 {
        // No vars in scope — only nullary gates.
        if kind & 1 == 0 {
            BIrStmt::Zero
        } else {
            BIrStmt::One
        }
    } else {
        let av = a % n_avail;
        let bv = b % n_avail;
        match kind % 6 {
            0 => BIrStmt::Zero,
            1 => BIrStmt::One,
            2 => BIrStmt::And(IRVarId(av), IRVarId(bv)),
            3 => BIrStmt::Or(IRVarId(av), IRVarId(bv)),
            4 => BIrStmt::Xor(IRVarId(av), IRVarId(bv)),
            _ => BIrStmt::Not(IRVarId(av)),
        }
    }
}

fn make_term(
    block_idx: usize,
    n_blocks: usize,
    n_vars: u32,
    param_counts: &[u32],
    raw_term: &RawTerm,
    ret_arity: u32,
) -> BIrTerminator {
    let (kind, cond, then_raw, else_raw) = raw_term;
    let then_target =
        make_target(block_idx, n_blocks, n_vars, param_counts, then_raw, ret_arity);

    // CondJmp requires at least one var for the condition wire.
    if n_vars == 0 || kind % 2 == 0 {
        BIrTerminator::Jmp(then_target)
    } else {
        let else_target =
            make_target(block_idx, n_blocks, n_vars, param_counts, else_raw, ret_arity);
        BIrTerminator::CondJmp {
            val: IRVarId(cond % n_vars),
            then_target,
            else_target,
        }
    }
}

fn make_target(
    block_idx: usize,
    n_blocks: usize,
    n_vars: u32,
    param_counts: &[u32],
    raw: &RawTarget,
    ret_arity: u32,
) -> BIrTarget {
    let (choice, raw_args) = raw;

    // If no vars are available we cannot supply args to any target that needs
    // them.  Since ret_arity == 0 is guaranteed when min_vars == 0 (and thus
    // n_vars == 0), Return is always safe here.
    if n_vars == 0 {
        return BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        };
    }

    // Valid choices: 0 = Return, 1..=n_forward = forward block indices.
    let n_forward = n_blocks - block_idx - 1;
    let n_choices = 1 + n_forward;
    let choice_idx = (*choice as usize) % n_choices;

    if choice_idx == 0 {
        // Return — always produce exactly `ret_arity` args.
        // Because ret_arity <= min_vars <= n_vars, we can always satisfy this.
        let args: Vec<IRVarId> = if ret_arity == 0 {
            vec![]
        } else {
            (0..ret_arity as usize)
                .map(|i| {
                    let raw_v = raw_args.get(i).copied().unwrap_or(0);
                    IRVarId(raw_v % n_vars)
                })
                .collect()
        };
        BIrTarget {
            block: IRBlockTargetId::Return,
            args,
        }
    } else {
        // Forward block: choice_idx-1 forward slots after block_idx.
        let target_block = block_idx + choice_idx;
        let needed = param_counts[target_block] as usize;
        // n_vars > 0 is guaranteed here (checked above).
        let args: Vec<IRVarId> = (0..needed)
            .map(|i| {
                let raw_v = raw_args.get(i).copied().unwrap_or(0);
                IRVarId(raw_v % n_vars)
            })
            .collect();
        BIrTarget {
            block: IRBlockTargetId::Block(IRBlockId(target_block as u32)),
            args,
        }
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
    use proptest::prelude::*;

    /// Generate a valid DAG `BIrBlocks<()>` (1–3 blocks, no back-edges).
    pub fn gen_biir() -> impl Strategy<Value = BIrBlocks<()>> {
        gen_biir_and_inputs().prop_map(|(blocks, _)| blocks)
    }

    /// Generate a valid DAG `BIrBlocks<()>` paired with matching entry-block inputs.
    pub fn gen_biir_and_inputs() -> impl Strategy<Value = (BIrBlocks<()>, Vec<bool>)> {
        // Step 1: choose the number of blocks.
        (1usize..=3usize).prop_flat_map(|n_blocks| {
            // Step 2: choose param counts for all blocks.
            proptest::collection::vec(0u32..=4u32, n_blocks).prop_flat_map(
                move |param_counts| {
                    let pc = param_counts.clone();
                    let n_entry_params = param_counts[0] as usize;

                    // Step 3: generate raw block data + ret_arity + entry inputs.
                    let raw_blocks_strat =
                        proptest::collection::vec(gen_raw_block(), n_blocks);
                    let inputs_strat =
                        proptest::collection::vec(any::<bool>(), n_entry_params);
                    let ret_arity_strat = any::<u8>();

                    (raw_blocks_strat, inputs_strat, ret_arity_strat).prop_map(
                        move |(raw_blocks, inputs, raw_ret_arity)| {
                            (interpret_biir(pc.clone(), raw_blocks, raw_ret_arity), inputs)
                        },
                    )
                },
            )
        })
    }

    fn gen_raw_block() -> impl Strategy<Value = RawBlock> {
        let raw_stmts = proptest::collection::vec(
            (any::<u8>(), any::<u32>(), any::<u32>()),
            0usize..=8usize,
        );
        let raw_term = gen_raw_term();
        (raw_stmts, raw_term)
    }

    fn gen_raw_term() -> impl Strategy<Value = RawTerm> {
        (
            any::<u8>(),
            any::<u32>(),
            gen_raw_target(),
            gen_raw_target(),
        )
    }

    fn gen_raw_target() -> impl Strategy<Value = RawTarget> {
        (
            any::<u8>(),
            proptest::collection::vec(any::<u32>(), 0usize..=4usize),
        )
    }
}
