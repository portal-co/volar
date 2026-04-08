// @reliability: normal
//! @ai: assisted
//! Lowering pass: movfuscated Boolar IR → single-block circuit.
//!
//! Converts a `BIrBlocks` with a self-loop (back-edge to block 0) into a plain
//! circuit by unrolling up to `limit` iterations and value-gating all conditional
//! exits with boolean multiplexers.
//!
//! # Value gating
//!
//! At each unrolled iteration `k` the terminator produces:
//! - `done[k]` — a circuit wire that is 1 if the loop exited at iteration `k`.
//! - `result[k]` — the would-be return wires if `done[k]` is 1.
//! - `next_args` — the arguments to carry into iteration `k+1`.
//!
//! A MUX cascade (right-to-left) selects the first "done" result:
//! ```text
//! mux(done[0], result[0], mux(done[1], result[1], ... fallback))
//! ```
//! where `fallback` is the final `current_state` (the state after `limit` steps
//! if the loop never terminated).
//!
//! # Gate cost
//! - Each iteration replicates the original gate list.
//! - Each output bit requires 1 AND + 2 XOR per MUX level.
//! - The done-OR cascade adds 4 gates (NOT, NOT, AND, NOT) per additional iteration.

use super::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{IRBlockId, IRBlockTargetId, IRVarId},
};
use alloc::{vec, vec::Vec};
use alloc::collections::BTreeMap;

// ============================================================================
// Public API
// ============================================================================

/// Output mode for the lowered circuit.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LoweringMode {
    /// Return only the gated output bits (same width as the original return).
    Unconditional,
    /// Prepend a single done-flag bit to the return values.
    ///
    /// The flag is `true` iff the loop terminated within `limit` steps.
    /// This is the **restated condition**: the termination predicate expressed
    /// as a circuit output wire, allowing callers to verify valid termination.
    WithTerminationFlag,
}

/// Lower a movfuscated `BIrBlocks` to a single-block circuit.
///
/// - If `blocks.is_circuit()` is already true, returns `blocks.clone()` with no work.
/// - For a **single-block self-loop** (block 0 with a `CondJmp` whose one target is
///   `Block(0)` and the other is `Return`): unrolls `limit` iterations, produces
///   `limit × |stmts|` gate replicas plus MUX/OR overhead.
/// - For a **single-block unconditional loop** (`Jmp(Block(0))`): unrolls `limit`
///   iterations; the output is the state after `limit` steps (loop never terminates).
///
/// # Panics
/// - If `blocks` has more than one block (multi-block DAG not yet implemented).
/// - If a back-edge targets any block other than block 0.
/// - If `IRBlockTargetId::Dyn` is encountered.
pub fn lower_to_circuit(blocks: &BIrBlocks, limit: u32, mode: LoweringMode) -> BIrBlocks {
    if blocks.is_circuit() {
        return blocks.clone();
    }

    assert_eq!(
        blocks.0.len(),
        1,
        "lower_to_circuit: multi-block DAG lowering is not yet implemented; \
         only single-block self-loops are currently supported"
    );

    let block0 = &blocks.0[0];
    let p = block0.params as usize; // number of circuit input params

    // Emitter owns the accumulating stmt list and var-ID counter.
    let mut emitter = Emitter::new(p as u32);

    // current_state[j] = circuit var ID currently holding block param j.
    // Initially the circuit inputs (IRVarId 0..P-1) map 1-to-1.
    let mut current_state: Vec<u32> = (0..p as u32).collect();

    // Per-iteration outputs from the terminator.
    let mut done_vars: Vec<u32> = Vec::new();
    let mut result_wires: Vec<Vec<u32>> = Vec::new(); // [k][b] = circuit var

    for _k in 0..limit as usize {
        // Build substitution map: original SSA id → circuit var id.
        let mut var_map: BTreeMap<u32, u32> = BTreeMap::new();
        for (j, &cv) in current_state.iter().enumerate() {
            var_map.insert(j as u32, cv);
        }

        // Re-emit all block stmts with fresh circuit var IDs.
        for (i, stmt) in block0.stmts.iter().enumerate() {
            let out_id = emitter.emit(subst_stmt(stmt, &var_map));
            // Map original stmt result (p + i) → fresh circuit var.
            var_map.insert(p as u32 + i as u32, out_id);
        }

        // Process terminator to extract (done, result, next_args).
        let (done_v, result_v, next_v) =
            process_terminator(&block0.terminator, &var_map, &mut emitter, &current_state);

        done_vars.push(done_v);
        result_wires.push(result_v);
        current_state = next_v;
    }

    // Determine output width (number of return bits).
    // When limit == 0 or the loop never returns (all Jmp(Block(0))),
    // use the current_state width as output width.
    let output_width = result_wires.first().map_or(current_state.len(), |r| r.len());

    // ---- MUX cascade (right-to-left over iterations) ----
    //
    // Start from the fallback: the state after all `limit` steps.
    let mut gated: Vec<u32> = {
        let mut v = current_state.clone();
        // Normalise length to output_width (pads or truncates if needed —
        // should never be needed for well-formed circuits).
        v.resize(output_width, *v.last().unwrap_or(&0));
        v
    };

    for k in (0..done_vars.len()).rev() {
        let mut new_gated = Vec::with_capacity(output_width);
        for b in 0..output_width {
            let a = *result_wires[k].get(b).unwrap_or(&gated[b]);
            let b_wire = gated[b];
            new_gated.push(emit_mux(&mut emitter, done_vars[k], a, b_wire));
        }
        gated = new_gated;
    }

    // ---- OR cascade for the overall done flag ----
    let overall_done = if done_vars.is_empty() {
        // limit == 0: emit a constant Zero (loop never ran, never terminated).
        emitter.emit(BIrStmt::Zero)
    } else {
        let mut acc = done_vars[0];
        for k in 1..done_vars.len() {
            acc = emit_or(&mut emitter, acc, done_vars[k]);
        }
        acc
    };

    // ---- Assemble output circuit ----
    let mut ret_args: Vec<IRVarId> = Vec::new();
    if mode == LoweringMode::WithTerminationFlag {
        ret_args.push(IRVarId(overall_done));
    }
    for &g in &gated {
        ret_args.push(IRVarId(g));
    }

    let out_block = BIrBlock {
        params: p as u32,
        stmts: emitter.stmts,
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: ret_args,
        }),
    };

    BIrBlocks(vec![out_block])
}

// ============================================================================
// Terminator processing
// ============================================================================

/// Analyse a block terminator and return `(done_wire, result_wires, next_args)`.
///
/// - `done_wire`: circuit var that is 1 when this iteration exits.
/// - `result_wires`: circuit vars for the return value when done.
/// - `next_args`: circuit vars to use as the next iteration's block params.
fn process_terminator(
    terminator: &BIrTerminator,
    var_map: &BTreeMap<u32, u32>,
    emitter: &mut Emitter,
    current_state: &[u32],
) -> (u32, Vec<u32>, Vec<u32>) {
    let lookup = |id: &IRVarId| -> u32 {
        *var_map
            .get(&id.0)
            .unwrap_or_else(|| panic!("lower_to_circuit: var {} not found in map", id.0))
    };

    match terminator {
        BIrTerminator::Jmp(target) => match &target.block {
            IRBlockTargetId::Return => {
                // Unconditional return: always done.
                let one_id = emitter.emit(BIrStmt::One);
                let result_v: Vec<u32> = target.args.iter().map(lookup).collect();
                // next_args is irrelevant (done=1 will gate it away); reuse result.
                (one_id, result_v.clone(), result_v)
            }
            IRBlockTargetId::Block(IRBlockId(0)) => {
                // Unconditional back-edge to block 0: loop continues, never done.
                let zero_id = emitter.emit(BIrStmt::Zero);
                let next_v: Vec<u32> = target.args.iter().map(lookup).collect();
                // result_wires are irrelevant (done=0); use current_state as placeholder.
                (zero_id, current_state.to_vec(), next_v)
            }
            IRBlockTargetId::Block(IRBlockId(b)) => {
                panic!(
                    "lower_to_circuit: Jmp to non-zero block {} is not supported \
                     (only back-edges to block 0 are handled)",
                    b
                );
            }
            IRBlockTargetId::Dyn(_) => {
                panic!("lower_to_circuit: dynamic dispatch (Dyn) is not supported");
            }
        },

        BIrTerminator::CondJmp { val, then_target, else_target } => {
            let val_cv = lookup(val);

            match (&then_target.block, &else_target.block) {
                // val=1 → Return; val=0 → Block(0).
                (IRBlockTargetId::Return, IRBlockTargetId::Block(IRBlockId(0))) => {
                    let result_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    let next_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    (val_cv, result_v, next_v)
                }

                // val=1 → Block(0); val=0 → Return.
                (IRBlockTargetId::Block(IRBlockId(0)), IRBlockTargetId::Return) => {
                    let not_val = emitter.emit(BIrStmt::Not(IRVarId(val_cv)));
                    let result_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    let next_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    (not_val, result_v, next_v)
                }

                // Both targets return — done = 1, result = mux(val, then, else).
                (IRBlockTargetId::Return, IRBlockTargetId::Return) => {
                    let one_id = emitter.emit(BIrStmt::One);
                    let then_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    let else_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    assert_eq!(
                        then_v.len(),
                        else_v.len(),
                        "lower_to_circuit: CondJmp Return targets have different arg counts"
                    );
                    let result_v: Vec<u32> = then_v
                        .iter()
                        .zip(else_v.iter())
                        .map(|(&a, &b)| emit_mux(emitter, val_cv, a, b))
                        .collect();
                    (one_id, result_v.clone(), result_v)
                }

                (IRBlockTargetId::Block(IRBlockId(a)), IRBlockTargetId::Block(IRBlockId(b))) => {
                    panic!(
                        "lower_to_circuit: CondJmp with both targets being blocks \
                         ({}, {}) is not supported",
                        a, b
                    );
                }

                _ => panic!(
                    "lower_to_circuit: unsupported CondJmp target combination \
                     (Dyn or non-zero Block)"
                ),
            }
        }
    }
}

// ============================================================================
// Gate emitters
// ============================================================================

/// Emit `mux(s, a, b) = XOR(AND(s, XOR(a, b)), b)`.
///
/// Returns the circuit var ID of the result.
/// Cost: 1 AND + 2 XOR.
fn emit_mux(emitter: &mut Emitter, s: u32, a: u32, b: u32) -> u32 {
    let xab = emitter.emit(BIrStmt::Xor(IRVarId(a), IRVarId(b)));
    let sel = emitter.emit(BIrStmt::And(IRVarId(s), IRVarId(xab)));
    emitter.emit(BIrStmt::Xor(IRVarId(sel), IRVarId(b)))
}

/// Emit `OR(a, b) = NOT(AND(NOT(a), NOT(b)))`.
///
/// Returns the circuit var ID of the result.
/// Cost: 2 NOT + 1 AND + 1 NOT = 4 gates.
fn emit_or(emitter: &mut Emitter, a: u32, b: u32) -> u32 {
    let na = emitter.emit(BIrStmt::Not(IRVarId(a)));
    let nb = emitter.emit(BIrStmt::Not(IRVarId(b)));
    let nand = emitter.emit(BIrStmt::And(IRVarId(na), IRVarId(nb)));
    emitter.emit(BIrStmt::Not(IRVarId(nand)))
}

// ============================================================================
// Helpers
// ============================================================================

/// Apply `var_map` to all operands of a `BIrStmt`, returning a new stmt
/// with circuit var IDs substituted for original SSA IDs.
fn subst_stmt(stmt: &BIrStmt, var_map: &BTreeMap<u32, u32>) -> BIrStmt {
    let s = |id: &IRVarId| -> IRVarId {
        IRVarId(
            *var_map
                .get(&id.0)
                .unwrap_or_else(|| panic!("lower_to_circuit: var {} not in map during subst", id.0)),
        )
    };
    match stmt {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(s(a), s(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(s(a), s(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(s(a), s(b)),
        BIrStmt::Not(a) => BIrStmt::Not(s(a)),
    }
}

/// Sequential var-ID allocator and stmt accumulator.
///
/// The invariant `next_id == params + stmts.len()` must hold at all times;
/// call [`emit`](Emitter::emit) once per stmt to maintain it.
struct Emitter {
    stmts: Vec<BIrStmt>,
    next_id: u32,
}

impl Emitter {
    fn new(first_id: u32) -> Self {
        Self { stmts: Vec::new(), next_id: first_id }
    }

    /// Push `stmt`, assign it the next sequential circuit var ID, and return that ID.
    fn emit(&mut self, stmt: BIrStmt) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
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
    use crate::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
    use crate::ir::{IRBlockId, IRBlockTargetId, IRVarId};

    /// Single-bit self-loop: params=1, stmts=[One], CondJmp(param[0] → Return, else Block(0) with One).
    ///
    /// Semantics: "if input bit is 1, return it; else loop with 1".
    /// After at most 1 iteration, output is always 1.
    fn build_simple_loop() -> BIrBlocks {
        BIrBlocks(std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::One], // IRVarId(1) = constant 1
            terminator: BIrTerminator::CondJmp {
                val: IRVarId(0), // condition = input bit
                then_target: BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)], // return input bit
                },
                else_target: BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(0)),
                    args: std::vec![IRVarId(1)], // loop with One
                },
            },
        }])
    }

    #[test]
    fn test_already_circuit_passthrough() {
        // A circuit should be returned unchanged.
        let circuit = BIrBlocks(std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            }),
        }]);
        let result = lower_to_circuit(&circuit, 3, LoweringMode::Unconditional);
        assert_eq!(result, circuit);
    }

    #[test]
    fn test_lower_simple_loop_is_circuit() {
        let blocks = build_simple_loop();
        assert!(!blocks.is_circuit(), "precondition: not yet a circuit");
        let lowered = lower_to_circuit(&blocks, 3, LoweringMode::Unconditional);
        assert!(lowered.is_circuit(), "lowered result must satisfy is_circuit()");
        assert_eq!(lowered.0[0].params, 1, "param count must be preserved");
    }

    #[test]
    fn test_lower_unconditional_return_width() {
        let blocks = build_simple_loop();
        let lowered = lower_to_circuit(&blocks, 3, LoweringMode::Unconditional);
        // Unconditional: return has exactly 1 arg (same as original return width).
        match &lowered.0[0].terminator {
            BIrTerminator::Jmp(t) => {
                assert_eq!(
                    t.args.len(),
                    1,
                    "Unconditional mode: return arg count must match original (1)"
                );
                assert_eq!(t.block, IRBlockTargetId::Return);
            }
            _ => panic!("expected Jmp(Return) terminator"),
        }
    }

    #[test]
    fn test_lower_with_termination_flag_return_width() {
        let blocks = build_simple_loop();
        let lowered = lower_to_circuit(&blocks, 3, LoweringMode::WithTerminationFlag);
        assert!(lowered.is_circuit());
        // WithTerminationFlag: return has 1 extra arg (done flag) prepended.
        match &lowered.0[0].terminator {
            BIrTerminator::Jmp(t) => {
                assert_eq!(
                    t.args.len(),
                    2,
                    "WithTerminationFlag mode: return arg count must be 1 (done) + 1 (output)"
                );
            }
            _ => panic!("expected Jmp(Return) terminator"),
        }
    }

    #[test]
    fn test_lower_limit_zero() {
        // limit=0 → no unrolling, output is current_state = input params.
        let blocks = build_simple_loop();
        let lowered = lower_to_circuit(&blocks, 0, LoweringMode::Unconditional);
        assert!(lowered.is_circuit());
        // With limit=0, no iteration stmts. Only the Zero constant and MUX overhead stmts.
        // Return args reference the fallback (current_state = input params = [0]).
    }

    #[test]
    fn test_gate_count_grows_with_limit() {
        let blocks = build_simple_loop();
        let l3 = lower_to_circuit(&blocks, 3, LoweringMode::Unconditional);
        let l6 = lower_to_circuit(&blocks, 6, LoweringMode::Unconditional);
        assert!(
            l6.0[0].stmts.len() > l3.0[0].stmts.len(),
            "more iterations → more gates"
        );
    }

    #[test]
    fn test_lower_unconditional_jmp_block0() {
        // Pure loop: always Jmp(Block(0)). Output = state after limit steps.
        let blocks = BIrBlocks(std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::Not(IRVarId(0))], // flip the bit each step
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(0)),
                args: std::vec![IRVarId(1)], // loop with NOT(input)
            }),
        }]);
        let lowered = lower_to_circuit(&blocks, 4, LoweringMode::Unconditional);
        assert!(lowered.is_circuit());
        assert_eq!(lowered.0[0].params, 1);
    }

    #[test]
    fn test_lower_both_return_condjmp() {
        // CondJmp where both targets return: always done, result = mux(val, then, else).
        let blocks = BIrBlocks(std::vec![BIrBlock {
            params: 2, // two input bits: selector and value
            stmts: std::vec![],
            terminator: BIrTerminator::CondJmp {
                val: IRVarId(0), // select on bit 0
                then_target: BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(1)], // return bit 1 if val=1
                },
                else_target: BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)], // return bit 0 if val=0
                },
            },
        }]);
        // Not a circuit (has CondJmp).
        assert!(!blocks.is_circuit());
        let lowered = lower_to_circuit(&blocks, 1, LoweringMode::Unconditional);
        assert!(lowered.is_circuit());
        // Output width = 1 (one return arg from each branch).
        match &lowered.0[0].terminator {
            BIrTerminator::Jmp(t) => assert_eq!(t.args.len(), 1),
            _ => panic!(),
        }
    }
}
