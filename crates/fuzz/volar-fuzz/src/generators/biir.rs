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
use volar_ir::ir::{IRBlockId, IRBlockTargetId, IRVarId, StorageId};

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
// Extended stmt builder (8-way dispatch with storage ops)
// ============================================================================

/// Build a single BIR stmt using 8-way dispatch:
///   0 = Zero, 1 = One, 2 = And, 3 = Or, 4 = Xor, 5 = Not,
///   6 = StorageWrite, 7 = StorageRead
///
/// `n_avail` is the count of usable operand vars (excludes void StorageWrite
/// results).  StorageWrite returns a dummy `false` bit and is NOT added to the
/// usable set — the caller must handle that.
///
/// Returns `(stmt, is_void)`.  `is_void` is `true` for StorageWrite.
fn make_stmt_extended(kind: u8, a: u32, b: u32, n_avail: u32) -> (BIrStmt, bool) {
    if n_avail == 0 {
        if kind & 1 == 0 {
            (BIrStmt::Zero, false)
        } else {
            (BIrStmt::One, false)
        }
    } else {
        let av = a % n_avail;
        let bv = b % n_avail;
        match kind % 8 {
            0 => (BIrStmt::Zero, false),
            1 => (BIrStmt::One, false),
            2 => (BIrStmt::And(IRVarId(av), IRVarId(bv)), false),
            3 => (BIrStmt::Or(IRVarId(av), IRVarId(bv)), false),
            4 => (BIrStmt::Xor(IRVarId(av), IRVarId(bv)), false),
            5 => (BIrStmt::Not(IRVarId(av)), false),
            6 => {
                // StorageWrite: store src=av at addr=bv, bit_width=1
                let store_id = StorageId(a % 4);
                (BIrStmt::StorageWrite {
                    storage: store_id,
                    src: IRVarId(av),
                    bit_width: 1,
                    addr: vec![IRVarId(bv)],
                }, true) // void
            }
            _ => {
                // StorageRead: read from addr=av, bit_width=1
                let store_id = StorageId(a % 4);
                (BIrStmt::StorageRead {
                    storage: store_id,
                    bit_width: 1,
                    addr: vec![IRVarId(av)],
                }, false)
            }
        }
    }
}

// ============================================================================
// Extended interpreter: single-block BIR with storage ops
// ============================================================================

/// Like [`interpret_biir`] but uses the 8-way [`make_stmt_extended`] dispatch
/// so the generated BIR may contain `StorageRead` and `StorageWrite` stmts.
///
/// Produces a single-block program that returns all vars.
pub fn interpret_biir_extended(
    n_params: u32,
    raw_stmts: &[RawStmt],
) -> BIrBlocks<()> {
    let mut stmts: Vec<BIrStmt> = Vec::with_capacity(raw_stmts.len());
    // Track indices of usable (non-void) vars.  Params 0..n_params are all
    // usable.  StorageWrite results are void and excluded.
    let mut usable: Vec<u32> = (0..n_params).collect();

    for &(kind, a, b) in raw_stmts {
        let n_avail = usable.len() as u32;
        // Remap raw operand indices through the usable set.
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable[(a as usize) % usable.len()], usable[(b as usize) % usable.len()])
        };
        let var_id = n_params + stmts.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts.push(stmt);
        if !is_void {
            usable.push(var_id);
        }
    }

    // Terminator: return all vars (params + stmts).
    let total = n_params + stmts.len() as u32;
    let ret_args: Vec<IRVarId> = (0..total).map(IRVarId).collect();
    let n = stmts.len();

    let block = BIrBlock {
        params: n_params,
        stmts,
        stmt_provs: vec![(); n],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: ret_args,
        }),
    };

    BIrBlocks(vec![block])
}

// ============================================================================
// Multi-block interpreter: two-block BIR for cross-block forwarding tests
// ============================================================================

/// Build a two-block `BIrBlocks<()>`:
///
/// - **Block 0 (entry)**: `n_params` params + stmts from `raw_stmts_b0`
///   (8-way dispatch).  Terminates with `Jmp(Block(1), all_vars)` — all vars
///   including void StorageWrite results (which produce dummy `false` bits).
/// - **Block 1**: params = B0's total var count (params + stmts); stmts from
///   `raw_stmts_b1` (8-way dispatch with B1-local var IDs).  Terminates with
///   `Jmp(Return, all_b1_vars)`.
///
/// This layout makes Block 0 always execute before Block 1, exercising
/// cross-block store-to-load forwarding.
pub fn interpret_biir_multiblock(
    n_params: u32,
    raw_stmts_b0: &[RawStmt],
    raw_stmts_b1: &[RawStmt],
) -> BIrBlocks<()> {
    // ── Block 0 ──────────────────────────────────────────────────────────────
    let mut stmts_b0: Vec<BIrStmt> = Vec::with_capacity(raw_stmts_b0.len());
    let mut usable_b0: Vec<u32> = (0..n_params).collect();

    for &(kind, a, b) in raw_stmts_b0 {
        let n_avail = usable_b0.len() as u32;
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable_b0[(a as usize) % usable_b0.len()], usable_b0[(b as usize) % usable_b0.len()])
        };
        let var_id = n_params + stmts_b0.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts_b0.push(stmt);
        if !is_void {
            usable_b0.push(var_id);
        }
    }

    let total_b0 = n_params + stmts_b0.len() as u32;
    // B0 terminator: jump to Block(1), passing ALL vars (including void).
    let b0_args: Vec<IRVarId> = (0..total_b0).map(IRVarId).collect();
    let n_b0 = stmts_b0.len();
    let b0_term = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Block(IRBlockId(1)),
        args: b0_args,
    });

    // ── Block 1 ──────────────────────────────────────────────────────────────
    // B1 params = total_b0 (all B0 vars passed as args).
    let n_b1_params = total_b0;
    let mut stmts_b1: Vec<BIrStmt> = Vec::with_capacity(raw_stmts_b1.len());
    // B1 usable set starts with all B1 params (which correspond to B0 usable
    // vars re-indexed to B1-local positions).  We must track which B0 vars
    // were usable so we only reference non-void vars as operands.
    //
    // Since ALL B0 vars (including void) were passed, a B1 param at position
    // `i` is usable iff B0's var `i` was usable.  We build the B1 usable set
    // accordingly.
    let b0_usable_set: std::collections::BTreeSet<u32> = usable_b0.iter().copied().collect();
    let mut usable_b1: Vec<u32> = (0..total_b0).filter(|i| b0_usable_set.contains(i)).collect();

    for &(kind, a, b) in raw_stmts_b1 {
        let n_avail = usable_b1.len() as u32;
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable_b1[(a as usize) % usable_b1.len()], usable_b1[(b as usize) % usable_b1.len()])
        };
        let var_id = n_b1_params + stmts_b1.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts_b1.push(stmt);
        if !is_void {
            usable_b1.push(var_id);
        }
    }

    let total_b1 = n_b1_params + stmts_b1.len() as u32;
    let b1_ret_args: Vec<IRVarId> = (0..total_b1).map(IRVarId).collect();
    let n_b1 = stmts_b1.len();
    let b1_term = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: b1_ret_args,
    });

    let block0 = BIrBlock {
        params: n_params,
        stmts: stmts_b0,
        stmt_provs: vec![(); n_b0],
        terminator: b0_term,
    };
    let block1 = BIrBlock {
        params: n_b1_params,
        stmts: stmts_b1,
        stmt_provs: vec![(); n_b1],
        terminator: b1_term,
    };

    BIrBlocks(vec![block0, block1])
}

// ============================================================================
// Diamond-CFG interpreter: four-block diamond for multi-predecessor tests
// ============================================================================

/// Build a four-block diamond-shaped `BIrBlocks<()>`:
///
/// ```text
///       B0 (entry)
///      /          \
///    B1 (true)   B2 (false)
///      \          /
///       B3 (merge)
/// ```
///
/// - **Block 0**: `n_params` params (min 1 for the condition) + stmts from
///   `raw_stmts_b0` (8-way dispatch).  Terminates with `CondJmp(param_0,
///   Block(1), Block(2))`, passing all vars to both targets.
/// - **Block 1 / Block 2**: params = B0's total var count; stmts from
///   `raw_stmts_b1` / `raw_stmts_b2`.  Each terminates with `Jmp(Block(3),
///   params_only)`.
/// - **Block 3 (merge)**: params = B0's total var count; stmts from
///   `raw_stmts_b3`.  Terminates with `Jmp(Return, all_b3_vars)`.
pub fn interpret_biir_diamond(
    n_params: u32,
    raw_stmts_b0: &[RawStmt],
    raw_stmts_b1: &[RawStmt],
    raw_stmts_b2: &[RawStmt],
    raw_stmts_b3: &[RawStmt],
) -> BIrBlocks<()> {
    let n_params = n_params.max(1); // need at least 1 for the condition

    // ── Block 0 ──────────────────────────────────────────────────────────────
    let mut stmts_b0: Vec<BIrStmt> = Vec::new();
    let mut usable_b0: Vec<u32> = (0..n_params).collect();

    for &(kind, a, b) in raw_stmts_b0 {
        let n_avail = usable_b0.len() as u32;
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable_b0[(a as usize) % usable_b0.len()], usable_b0[(b as usize) % usable_b0.len()])
        };
        let var_id = n_params + stmts_b0.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts_b0.push(stmt);
        if !is_void {
            usable_b0.push(var_id);
        }
    }

    let total_b0 = n_params + stmts_b0.len() as u32;
    let b0_all_args: Vec<IRVarId> = (0..total_b0).map(IRVarId).collect();
    let n_b0 = stmts_b0.len();
    let b0_term = BIrTerminator::CondJmp {
        val: IRVarId(0),
        then_target: BIrTarget {
            block: IRBlockTargetId::Block(IRBlockId(1)),
            args: b0_all_args.clone(),
        },
        else_target: BIrTarget {
            block: IRBlockTargetId::Block(IRBlockId(2)),
            args: b0_all_args,
        },
    };

    // ── Block 1 (true branch) ────────────────────────────────────────────────
    let n_b1_params = total_b0;
    let b0_usable_set: std::collections::BTreeSet<u32> = usable_b0.iter().copied().collect();
    let mut usable_b1: Vec<u32> = (0..total_b0).filter(|i| b0_usable_set.contains(i)).collect();
    let mut stmts_b1: Vec<BIrStmt> = Vec::new();

    for &(kind, a, b) in raw_stmts_b1 {
        let n_avail = usable_b1.len() as u32;
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable_b1[(a as usize) % usable_b1.len()], usable_b1[(b as usize) % usable_b1.len()])
        };
        let var_id = n_b1_params + stmts_b1.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts_b1.push(stmt);
        if !is_void {
            usable_b1.push(var_id);
        }
    }

    let n_b1 = stmts_b1.len();
    let b1_to_b3_args: Vec<IRVarId> = (0..n_b1_params).map(IRVarId).collect();
    let b1_term = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Block(IRBlockId(3)),
        args: b1_to_b3_args,
    });

    // ── Block 2 (false branch) ───────────────────────────────────────────────
    let n_b2_params = total_b0;
    let mut usable_b2: Vec<u32> = (0..total_b0).filter(|i| b0_usable_set.contains(i)).collect();
    let mut stmts_b2: Vec<BIrStmt> = Vec::new();

    for &(kind, a, b) in raw_stmts_b2 {
        let n_avail = usable_b2.len() as u32;
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable_b2[(a as usize) % usable_b2.len()], usable_b2[(b as usize) % usable_b2.len()])
        };
        let var_id = n_b2_params + stmts_b2.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts_b2.push(stmt);
        if !is_void {
            usable_b2.push(var_id);
        }
    }

    let n_b2 = stmts_b2.len();
    let b2_to_b3_args: Vec<IRVarId> = (0..n_b2_params).map(IRVarId).collect();
    let b2_term = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Block(IRBlockId(3)),
        args: b2_to_b3_args,
    });

    // ── Block 3 (merge) ──────────────────────────────────────────────────────
    let n_b3_params = total_b0;
    let mut usable_b3: Vec<u32> = (0..total_b0).filter(|i| b0_usable_set.contains(i)).collect();
    let mut stmts_b3: Vec<BIrStmt> = Vec::new();

    for &(kind, a, b) in raw_stmts_b3 {
        let n_avail = usable_b3.len() as u32;
        let (mapped_a, mapped_b) = if n_avail == 0 {
            (0u32, 0u32)
        } else {
            (usable_b3[(a as usize) % usable_b3.len()], usable_b3[(b as usize) % usable_b3.len()])
        };
        let var_id = n_b3_params + stmts_b3.len() as u32;
        let (stmt, is_void) = make_stmt_extended(kind, mapped_a, mapped_b, n_avail);
        stmts_b3.push(stmt);
        if !is_void {
            usable_b3.push(var_id);
        }
    }

    let total_b3 = n_b3_params + stmts_b3.len() as u32;
    let n_b3 = stmts_b3.len();
    let b3_ret_args: Vec<IRVarId> = (0..total_b3).map(IRVarId).collect();
    let b3_term = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: b3_ret_args,
    });

    BIrBlocks(vec![
        BIrBlock { params: n_params, stmts: stmts_b0, stmt_provs: vec![(); n_b0], terminator: b0_term },
        BIrBlock { params: n_b1_params, stmts: stmts_b1, stmt_provs: vec![(); n_b1], terminator: b1_term },
        BIrBlock { params: n_b2_params, stmts: stmts_b2, stmt_provs: vec![(); n_b2], terminator: b2_term },
        BIrBlock { params: n_b3_params, stmts: stmts_b3, stmt_provs: vec![(); n_b3], terminator: b3_term },
    ])
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

    /// Single-block `BIrBlocks<()>` with `StorageRead`/`StorageWrite` stmts
    /// (8-way dispatch).  Paired with matching entry-block inputs.
    pub fn gen_biir_extended_and_inputs() -> impl Strategy<Value = (BIrBlocks<()>, Vec<bool>)> {
        (0u32..=4u32).prop_flat_map(|n_params| {
            let raw_stmts = proptest::collection::vec(
                (any::<u8>(), any::<u32>(), any::<u32>()),
                0usize..=8usize,
            );
            let inputs = proptest::collection::vec(any::<bool>(), n_params as usize);

            (raw_stmts, inputs).prop_map(move |(raw_stmts, inputs)| {
                (interpret_biir_extended(n_params, &raw_stmts), inputs)
            })
        })
    }

    /// Two-block `BIrBlocks<()>` with `StorageRead`/`StorageWrite` across
    /// blocks (8-way dispatch).  Block 0 jumps unconditionally to Block 1.
    pub fn gen_biir_multiblock_and_inputs() -> impl Strategy<Value = (BIrBlocks<()>, Vec<bool>)> {
        (0u32..=4u32).prop_flat_map(|n_params| {
            let raw_tuple = (any::<u8>(), any::<u32>(), any::<u32>());
            let raw_stmts_b0 = proptest::collection::vec(raw_tuple.clone(), 0usize..=6usize);
            let raw_stmts_b1 = proptest::collection::vec(raw_tuple, 0usize..=6usize);
            let inputs = proptest::collection::vec(any::<bool>(), n_params as usize);

            (raw_stmts_b0, raw_stmts_b1, inputs).prop_map(
                move |(raw_stmts_b0, raw_stmts_b1, inputs)| {
                    (interpret_biir_multiblock(n_params, &raw_stmts_b0, &raw_stmts_b1), inputs)
                },
            )
        })
    }

    /// Four-block diamond `BIrBlocks<()>` with `StorageRead`/`StorageWrite`.
    ///
    /// B0 branches on the first param to B1 (true) or B2 (false).
    /// B1 and B2 both merge into B3.  This exercises multi-predecessor
    /// store-to-load forwarding with param injection.
    pub fn gen_biir_diamond_and_inputs() -> impl Strategy<Value = (BIrBlocks<()>, Vec<bool>)> {
        (1u32..=4u32).prop_flat_map(|n_params| {
            let raw_tuple = (any::<u8>(), any::<u32>(), any::<u32>());
            let raw_stmts_b0 = proptest::collection::vec(raw_tuple.clone(), 0usize..=4usize);
            let raw_stmts_b1 = proptest::collection::vec(raw_tuple.clone(), 0usize..=4usize);
            let raw_stmts_b2 = proptest::collection::vec(raw_tuple.clone(), 0usize..=4usize);
            let raw_stmts_b3 = proptest::collection::vec(raw_tuple, 0usize..=4usize);
            let inputs = proptest::collection::vec(any::<bool>(), n_params as usize);

            (raw_stmts_b0, raw_stmts_b1, raw_stmts_b2, raw_stmts_b3, inputs).prop_map(
                move |(raw_stmts_b0, raw_stmts_b1, raw_stmts_b2, raw_stmts_b3, inputs)| {
                    (interpret_biir_diamond(n_params, &raw_stmts_b0, &raw_stmts_b1, &raw_stmts_b2, &raw_stmts_b3), inputs)
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
