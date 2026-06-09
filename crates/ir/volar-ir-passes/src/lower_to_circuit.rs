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
//!
//! # Dynamic skip & the continuation binding (VCB §C)
//!
//! This pass is purely **Boolar** (boolean gates); it has no notion of VOLE
//! commitments.  The "dynamic skip" continuation — fast-forwarding a segment and
//! resuming a *fresh* committed segment bound to the prior one — therefore does
//! **not** live here: the binding is a VOLE-level (XOR-key re-commitment + free
//! linear check) concern emitted by `volar_weaver::glue::weave_continuation_glue_*`
//! at the segment boundary.  The only thing this pass needs to hand off is the
//! boundary's **carried-state width** (the `ell` the glue re-keys); that is what
//! [`lower_to_circuit_with_boundary`] returns alongside the lowered circuit.
//! Static lowering ([`lower_to_circuit`]) is unchanged.

use volar_ir::{
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
pub fn lower_to_circuit<P: Clone>(blocks: &BIrBlocks<P>, limit: u32, mode: LoweringMode) -> BIrBlocks<P> {
    if blocks.is_circuit() {
        return blocks.clone();
    }

    assert_eq!(
        blocks.blocks.len(),
        1,
        "lower_to_circuit: multi-block DAG lowering is not yet implemented; \
         only single-block self-loops are currently supported"
    );

    let block0 = &blocks.blocks[0];
    let p = block0.params as usize; // number of circuit input params

    // Provenance for infrastructure gates (MUX cascade, loop control constants).
    // Use the first source statement's provenance; degenerate empty blocks panic.
    let ctrl_prov: &P = block0.stmt_provs.first()
        .expect("lower_to_circuit: block has no statements; cannot infer provenance for infrastructure gates");

    // Emitter owns the accumulating stmt list and var-ID counter.
    let mut emitter = Emitter::<P>::new(p as u32);

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

        // Re-emit all block stmts with fresh circuit var IDs, carrying provenance.
        for (i, stmt) in block0.stmts.iter().enumerate() {
            let prov = block0.stmt_provs.get(i).cloned().unwrap_or_else(|| ctrl_prov.clone());
            let out_id = emitter.emit(subst_stmt(stmt, &var_map), prov);
            // Map original stmt result (p + i) → fresh circuit var.
            var_map.insert(p as u32 + i as u32, out_id);
        }

        // Process terminator to extract (done, result, next_args).
        let (done_v, result_v, next_v) =
            process_terminator(&block0.terminator, &var_map, &mut emitter, &current_state, ctrl_prov);

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
            new_gated.push(emit_mux(&mut emitter, done_vars[k], a, b_wire, ctrl_prov));
        }
        gated = new_gated;
    }

    // ---- OR cascade for the overall done flag ----
    let overall_done = if done_vars.is_empty() {
        // limit == 0: emit a constant Zero (loop never ran, never terminated).
        emitter.emit(BIrStmt::Zero, ctrl_prov.clone())
    } else {
        let mut acc = done_vars[0];
        for k in 1..done_vars.len() {
            acc = emit_or(&mut emitter, acc, done_vars[k], ctrl_prov);
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
        stmt_provs: emitter.stmt_provs,
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: ret_args,
        }),
    };

    BIrBlocks { blocks: vec![out_block], pre_init: vec![] }
}

/// Boundary metadata for binding a lowered (skipped) segment to a resumed
/// segment via the VOLE continuation glue
/// (`volar_weaver::glue::weave_continuation_glue_*`).
///
/// The skip binding re-keys the segment's carried state under a fresh one-time
/// pad and proves the link with a free linear check; this struct reports how
/// wide that carried state is so the caller can size the glue (`ell =
/// state_width`) without re-deriving it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SkipBoundary {
    /// Number of carried state wires at the segment boundary — the `ell` the
    /// XOR-key re-commitment glue re-keys.
    pub state_width: usize,
    /// Whether a termination/done flag is prepended to the lowered outputs.
    pub has_done_flag: bool,
}

/// Lower as [`lower_to_circuit`], additionally returning the [`SkipBoundary`] so
/// a caller can attach the dynamic-skip continuation glue at the segment
/// boundary.  Static lowering behaviour is identical to [`lower_to_circuit`].
pub fn lower_to_circuit_with_boundary<P: Clone>(
    blocks: &BIrBlocks<P>,
    limit: u32,
    mode: LoweringMode,
) -> (BIrBlocks<P>, SkipBoundary) {
    let circuit = lower_to_circuit(blocks, limit, mode);
    let has_done_flag = mode == LoweringMode::WithTerminationFlag;
    // The single output block returns `[done?] ++ state`; the carried state is
    // everything but the optional leading done flag.
    let ret_len = match &circuit.blocks[0].terminator {
        BIrTerminator::Jmp(t) => t.args.len(),
        _ => 0,
    };
    let state_width = ret_len.saturating_sub(has_done_flag as usize);
    (circuit, SkipBoundary { state_width, has_done_flag })
}

// ============================================================================
// Terminator processing
// ============================================================================

/// Analyse a block terminator and return `(done_wire, result_wires, next_args)`.
///
/// - `done_wire`: circuit var that is 1 when this iteration exits.
/// - `result_wires`: circuit vars for the return value when done.
/// - `next_args`: circuit vars to use as the next iteration's block params.
fn process_terminator<P: Clone>(
    terminator: &BIrTerminator,
    var_map: &BTreeMap<u32, u32>,
    emitter: &mut Emitter<P>,
    current_state: &[u32],
    ctrl_prov: &P,
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
                let one_id = emitter.emit(BIrStmt::One, ctrl_prov.clone());
                let result_v: Vec<u32> = target.args.iter().map(lookup).collect();
                // next_args is irrelevant (done=1 will gate it away); reuse result.
                (one_id, result_v.clone(), result_v)
            }
            IRBlockTargetId::Block(IRBlockId(0)) => {
                // Unconditional back-edge to block 0: loop continues, never done.
                let zero_id = emitter.emit(BIrStmt::Zero, ctrl_prov.clone());
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
            _ => panic!("lower_to_circuit: unhandled IRBlockTargetId variant — add handling for this variant"),
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
                    let not_val = emitter.emit(BIrStmt::Not(IRVarId(val_cv)), ctrl_prov.clone());
                    let result_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    let next_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    (not_val, result_v, next_v)
                }

                // Both targets return — done = 1 always, result = mux(val, then, else).
                // next_v uses current_state as a don't-care placeholder so that
                // subsequent (dead) unrolled iterations still have a valid var_map.
                (IRBlockTargetId::Return, IRBlockTargetId::Return) => {
                    let one_id = emitter.emit(BIrStmt::One, ctrl_prov.clone());
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
                        .map(|(&a, &b)| emit_mux(emitter, val_cv, a, b, ctrl_prov))
                        .collect();
                    (one_id, result_v, current_state.to_vec())
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
        _ => panic!("lower_to_circuit: unhandled BIrTerminator variant — add handling for this variant"),
    }
}

// ============================================================================
// Gate emitters
// ============================================================================

/// Emit `mux(s, a, b) = XOR(AND(s, XOR(a, b)), b)`.
///
/// Returns the circuit var ID of the result.
/// Cost: 1 AND + 2 XOR.
fn emit_mux<P: Clone>(emitter: &mut Emitter<P>, s: u32, a: u32, b: u32, prov: &P) -> u32 {
    let xab = emitter.emit(BIrStmt::Xor(IRVarId(a), IRVarId(b)), prov.clone());
    let sel = emitter.emit(BIrStmt::And(IRVarId(s), IRVarId(xab)), prov.clone());
    emitter.emit(BIrStmt::Xor(IRVarId(sel), IRVarId(b)), prov.clone())
}

/// Emit `OR(a, b) = NOT(AND(NOT(a), NOT(b)))`.
///
/// Returns the circuit var ID of the result.
/// Cost: 2 NOT + 1 AND + 1 NOT = 4 gates.
fn emit_or<P: Clone>(emitter: &mut Emitter<P>, a: u32, b: u32, prov: &P) -> u32 {
    let na = emitter.emit(BIrStmt::Not(IRVarId(a)), prov.clone());
    let nb = emitter.emit(BIrStmt::Not(IRVarId(b)), prov.clone());
    let nand = emitter.emit(BIrStmt::And(IRVarId(na), IRVarId(nb)), prov.clone());
    emitter.emit(BIrStmt::Not(IRVarId(nand)), prov.clone())
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
        // External primitives: substitute operand var-IDs, carry everything else through.
        BIrStmt::OracleCall { name, args, num_bits } => BIrStmt::OracleCall {
            name: name.clone(),
            args: args.iter().map(s).collect(),
            num_bits: *num_bits,
        },
        BIrStmt::OracleBit { call, bit } => BIrStmt::OracleBit { call: s(call), bit: *bit },
        BIrStmt::ActionCall { name, guard, args, fallback, num_bits } => BIrStmt::ActionCall {
            name: name.clone(),
            guard: s(guard),
            args: args.iter().map(s).collect(),
            fallback: fallback.iter().map(s).collect(),
            num_bits: *num_bits,
        },
        BIrStmt::ActionBit { call, bit } => BIrStmt::ActionBit { call: s(call), bit: *bit },
        BIrStmt::Rng { name } => BIrStmt::Rng { name: name.clone() },
        BIrStmt::StorageRead { storage, bit_width, addr } => BIrStmt::StorageRead {
            storage: *storage,
            bit_width: *bit_width,
            addr: addr.iter().map(|v| s(v)).collect(),
        },
        BIrStmt::StorageWrite { storage, src, bit_width, addr } => BIrStmt::StorageWrite {
            storage: *storage,
            src: s(src),
            bit_width: *bit_width,
            addr: addr.iter().map(|v| s(v)).collect(),
        },
        _ => panic!("subst_stmt: unhandled BIrStmt variant — add substitution for this variant"),
    }
}

/// Sequential var-ID allocator and stmt accumulator.
///
/// The invariant `next_id == params + stmts.len()` must hold at all times;
/// call [`emit`](Emitter::emit) once per stmt to maintain it.
struct Emitter<P: Clone = ()> {
    stmts: Vec<BIrStmt>,
    stmt_provs: Vec<P>,
    next_id: u32,
}

impl<P: Clone> Emitter<P> {
    fn new(first_id: u32) -> Self {
        Self { stmts: Vec::new(), stmt_provs: Vec::new(), next_id: first_id }
    }

    /// Push `stmt` with a provenance annotation, assign it the next sequential
    /// circuit var ID, and return that ID.
    fn emit(&mut self, stmt: BIrStmt, prov: P) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
        self.stmt_provs.push(prov);
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
    use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
    use volar_ir::ir::{IRBlockId, IRBlockTargetId, IRVarId};

    /// Single-bit self-loop: params=1, stmts=[One], CondJmp(param[0] → Return, else Block(0) with One).
    ///
    /// Semantics: "if input bit is 1, return it; else loop with 1".
    /// After at most 1 iteration, output is always 1.
    fn build_simple_loop() -> BIrBlocks {
        BIrBlocks { blocks: std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::One], // IRVarId(1) = constant 1
            stmt_provs: std::vec![()],
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
        }], pre_init: std::vec![] }
    }

    #[test]
    fn test_already_circuit_passthrough() {
        // A circuit should be returned unchanged.
        let circuit: BIrBlocks<()> = BIrBlocks { blocks: std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            }),
        }], pre_init: std::vec![] };
        let result = lower_to_circuit(&circuit, 3, LoweringMode::Unconditional);
        assert_eq!(result, circuit);
    }

    #[test]
    fn test_skip_boundary_reports_state_width() {
        let blocks = build_simple_loop(); // 1 state wire
        let (circ_u, b_u) = lower_to_circuit_with_boundary(&blocks, 3, LoweringMode::Unconditional);
        assert!(circ_u.is_circuit());
        assert_eq!(b_u.state_width, 1, "one carried state wire");
        assert!(!b_u.has_done_flag);
        // WithTerminationFlag prepends a done flag; carried state width is unchanged.
        let (_circ_f, b_f) = lower_to_circuit_with_boundary(&blocks, 3, LoweringMode::WithTerminationFlag);
        assert_eq!(b_f.state_width, 1, "done flag excluded from state width");
        assert!(b_f.has_done_flag);
    }

    #[test]
    fn test_lower_simple_loop_is_circuit() {
        let blocks = build_simple_loop();
        assert!(!blocks.is_circuit(), "precondition: not yet a circuit");
        let lowered = lower_to_circuit(&blocks, 3, LoweringMode::Unconditional);
        assert!(lowered.is_circuit(), "lowered result must satisfy is_circuit()");
        assert_eq!(lowered.blocks[0].params, 1, "param count must be preserved");
    }

    #[test]
    fn test_lower_unconditional_return_width() {
        let blocks = build_simple_loop();
        let lowered = lower_to_circuit(&blocks, 3, LoweringMode::Unconditional);
        // Unconditional: return has exactly 1 arg (same as original return width).
        match &lowered.blocks[0].terminator {
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
        match &lowered.blocks[0].terminator {
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
            l6.blocks[0].stmts.len() > l3.blocks[0].stmts.len(),
            "more iterations → more gates"
        );
    }

    #[test]
    fn test_lower_unconditional_jmp_block0() {
        // Pure loop: always Jmp(Block(0)). Output = state after limit steps.
        let blocks = BIrBlocks { blocks: std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::Not(IRVarId(0))], // flip the bit each step
            stmt_provs: std::vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(0)),
                args: std::vec![IRVarId(1)], // loop with NOT(input)
            }),
        }], pre_init: std::vec![] };
        let lowered = lower_to_circuit(&blocks, 4, LoweringMode::Unconditional);
        assert!(lowered.is_circuit());
        assert_eq!(lowered.blocks[0].params, 1);
    }

    #[test]
    fn test_lower_both_return_condjmp() {
        // CondJmp where both targets return: always done, result = mux(val, then, else).
        let blocks: BIrBlocks<()> = BIrBlocks { blocks: std::vec![BIrBlock {
            params: 2, // two input bits: selector and value
            stmts: std::vec![BIrStmt::Zero],
            stmt_provs: std::vec![()],
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
        }], pre_init: std::vec![] };
        // Not a circuit (has CondJmp).
        assert!(!blocks.is_circuit());
        let lowered = lower_to_circuit(&blocks, 1, LoweringMode::Unconditional);
        assert!(lowered.is_circuit());
        // Output width = 1 (one return arg from each branch).
        match &lowered.blocks[0].terminator {
            BIrTerminator::Jmp(t) => assert_eq!(t.args.len(), 1),
            _ => panic!(),
        }
    }
}
