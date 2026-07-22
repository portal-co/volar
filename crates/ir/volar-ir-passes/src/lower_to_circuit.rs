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
    ir::{
        IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator,
        IRTypeId, IRVarId,
    },
};
use volar_ir_common::Constant;
use alloc::{vec, vec::Vec};
use alloc::collections::BTreeMap;

use crate::dispatch_accumulator::{
    DispatchBitPrimitives, DispatchSlotPrimitives, emit_select_bit, emit_select_slot,
};
use crate::movfuscate::subst_ir;

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
    lower_to_circuit_impl(blocks, limit, mode, None)
}

/// As [`lower_to_circuit`], but accepts explicit control provenance for a
/// statement-free loop. The provenance must identify the source control
/// context responsible for the generated circuit infrastructure.
pub fn lower_to_circuit_with_control_provenance<P: Clone>(
    blocks: &BIrBlocks<P>,
    limit: u32,
    mode: LoweringMode,
    control_prov: &P,
) -> BIrBlocks<P> {
    lower_to_circuit_impl(blocks, limit, mode, Some(control_prov))
}

fn lower_to_circuit_impl<P: Clone>(
    blocks: &BIrBlocks<P>,
    limit: u32,
    mode: LoweringMode,
    control_prov: Option<&P>,
) -> BIrBlocks<P> {
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
    // Prefer an actual statement and otherwise require the caller's explicit
    // frontend/control provenance.
    let ctrl_prov: &P = block0.stmts.first().map(|n| &n.prov)
        .or(control_prov)
        .expect("lower_to_circuit: block has no statements; supply explicit control provenance");

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
            let prov = stmt.prov.clone();
            let out_id = emitter.emit(subst_stmt(&stmt.kind, &var_map), prov);
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
        // Normalise length to output_width. NOT actually dead: unlike
        // `lower_to_circuit_ir` (which keeps state/return as separate,
        // non-overlapping segments -- see its own `process_terminator_ir`),
        // this older BIr entry point still conflates them the way bug #2
        // (see `docs/agent-context`/memory) found and fixed for the IR path
        // only. Confirmed still exercised by a legitimate existing unit
        // test (`test_lower_both_return_condjmp`, state width 2 vs return
        // width 1) -- this is deliberately untouched legacy Boolar-IR
        // behavior (see `docs/agent-context/boolar-ir-conflicts.md`), not
        // dead code; don't assume it's safe to remove or assert against.
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
    stmts: Vec<volar_ir_common::Node<BIrStmt, P>>,
    next_id: u32,
}

impl<P: Clone> Emitter<P> {
    fn new(first_id: u32) -> Self {
        Self { stmts: Vec::new(), next_id: first_id }
    }

    /// Push `stmt` with a provenance annotation, assign it the next sequential
    /// circuit var ID, and return that ID.
    fn emit(&mut self, stmt: BIrStmt, prov: P) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(volar_ir_common::Node::new(stmt, prov, None));
        id
    }
}

// ============================================================================
// Volar IR (`IRBlocks`) support
// ============================================================================
//
// A parallel entry point for `movfuscate_ir`'s output, rather than routing
// through Boolar IR (`lower_ir_to_boolar`) first: booleanizing loses
// multi-output/handle structure that maps awkwardly onto "exactly one bit
// per value", and this pipeline never needs a boolean circuit at all —
// `weave_vole_prover_ir_with_mode`/`weave_vole_verifier_ir_with_mode`
// already consume `IRBlocks` directly (and are the only entry points that
// support `StorageMode::Commitment`). Same shape as [`lower_to_circuit`]
// (unroll the self-loop, mux-cascade the per-iteration results, OR-cascade
// the done flags) — reusing the exact arithmetic `movfuscate_ir` itself
// uses ([`crate::dispatch_accumulator`]'s `emit_select_bit`/
// `emit_select_slot`) and its exact statement-substitution function
// ([`subst_ir`]), so the two passes share formulas, not just shape.

/// Sequential var-ID allocator + stmt accumulator for [`lower_to_circuit_ir`].
///
/// [`DispatchBitPrimitives`]/[`DispatchSlotPrimitives`] methods take no
/// provenance parameter, so the provenance for the *next* emitted stmt is
/// staged via [`set_prov`](Self::set_prov) before each call — the same
/// pattern `movfuscate::IrCtx` uses internally.
struct IrEmitter<P: Clone> {
    stmts: Vec<volar_ir_common::Node<IRStmt, P>>,
    next_id: u32,
    bit_type_id: IRTypeId,
    prov: P,
}

impl<P: Clone> IrEmitter<P> {
    fn new(first_id: u32, bit_type_id: IRTypeId, ctrl_prov: P) -> Self {
        Self { stmts: Vec::new(), next_id: first_id, bit_type_id, prov: ctrl_prov }
    }

    fn set_prov(&mut self, prov: P) {
        self.prov = prov;
    }

    fn push(&mut self, stmt: IRStmt) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(volar_ir_common::Node::new(stmt, self.prov.clone(), None));
        id
    }

    fn emit_poly(&mut self, coeffs: BTreeMap<Vec<IRVarId>, u8>, constant_lo: u128, ty: IRTypeId) -> u32 {
        self.push(IRStmt::Poly { ty, coeffs, constant: Constant { hi: 0, lo: constant_lo } })
    }
}

impl<P: Clone> DispatchBitPrimitives for IrEmitter<P> {
    fn emit_zero_bit(&mut self) -> u32 {
        let bt = self.bit_type_id.clone();
        self.push(IRStmt::Const(Constant { hi: 0, lo: 0 }, bt))
    }
    fn emit_one_bit(&mut self) -> u32 {
        let bt = self.bit_type_id.clone();
        self.push(IRStmt::Const(Constant { hi: 0, lo: 1 }, bt))
    }
    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a;
        }
        let mut key = vec![IRVarId(a), IRVarId(b)];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        let bt = self.bit_type_id.clone();
        self.emit_poly(coeffs, 0, bt)
    }
    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return self.emit_zero_bit();
        }
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        coeffs.insert(vec![IRVarId(b)], 1);
        let bt = self.bit_type_id.clone();
        self.emit_poly(coeffs, 0, bt)
    }
    fn emit_not(&mut self, a: u32) -> u32 {
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        let bt = self.bit_type_id.clone();
        self.emit_poly(coeffs, 1, bt)
    }
}

impl<P: Clone> DispatchSlotPrimitives for IrEmitter<P> {
    type SlotTy = IRTypeId;

    fn emit_zero_slot(&mut self, ty: &IRTypeId) -> u32 {
        let t = ty.clone();
        self.push(IRStmt::Const(Constant { hi: 0, lo: 0 }, t))
    }
    fn emit_gate(&mut self, is_active: u32, val: u32, ty: &IRTypeId) -> u32 {
        if is_active == val {
            return is_active;
        }
        let mut key = vec![IRVarId(is_active), IRVarId(val)];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        self.emit_poly(coeffs, 0, ty.clone())
    }
    fn emit_field_add(&mut self, a: u32, b: u32, ty: &IRTypeId) -> u32 {
        if a == b {
            return self.emit_zero_slot(ty);
        }
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        coeffs.insert(vec![IRVarId(b)], 1);
        self.emit_poly(coeffs, 0, ty.clone())
    }
}

/// Extract the result type embedded in an `IRStmt`. Mirrors [`subst_ir`]'s
/// variant coverage exactly (same enum, read instead of rewritten);
/// `StorageWrite` carries no payload type of its own and is conventionally
/// `Bit` (an ack marker), matching `movfuscate::IrCtx`'s own convention for
/// the same statement kind.
fn ir_stmt_result_type(stmt: &IRStmt, bit_type_id: &IRTypeId) -> IRTypeId {
    match stmt {
        IRStmt::StorageRead { ty, .. } => ty.clone(),
        IRStmt::StorageWrite { .. } => bit_type_id.clone(),
        IRStmt::Const(_, ty) => ty.clone(),
        IRStmt::Transmute { dst_ty, .. } => dst_ty.clone(),
        IRStmt::Poly { ty, .. } => ty.clone(),
        IRStmt::Rol { ty, .. } => ty.clone(),
        IRStmt::Ror { ty, .. } => ty.clone(),
        IRStmt::Merge { ty, .. } => ty.clone(),
        IRStmt::Splat { ty, .. } => ty.clone(),
        IRStmt::Shuffle { ty, .. } => ty.clone(),
        IRStmt::OracleCall { result_ty, .. } => result_ty.clone(),
        IRStmt::OracleOutput { ty, .. } => ty.clone(),
        IRStmt::ActionCall { result_ty, .. } => result_ty.clone(),
        IRStmt::ActionOutput { ty, .. } => ty.clone(),
        IRStmt::Rng { ty, .. } => ty.clone(),
        _ => panic!("ir_stmt_result_type: unhandled IRStmt variant — add a case for this variant"),
    }
}

/// If `terminator` has a `Return` target (`Jmp` or either arm of a
/// `JumpCond`), resolve each of its args' types via `orig_var_types`.
fn return_target_types(terminator: &IRTerminator, orig_var_types: &[IRTypeId]) -> Option<Vec<IRTypeId>> {
    let ret_args: &[IRVarId] = match terminator {
        IRTerminator::Jmp { target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. } } => args,
        IRTerminator::JumpCond {
            then_target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. }, ..
        } => args,
        IRTerminator::JumpCond {
            else_target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. }, ..
        } => args,
        _ => return None,
    };
    Some(ret_args.iter().map(|id| orig_var_types[id.0 as usize].clone()).collect())
}

/// Analyse an `IRTerminator` and return `(done_wire, result_wires, next_args)`.
/// Mirrors [`process_terminator`] exactly, for `IRTerminator`'s `Jmp`/
/// `JumpCond` shape instead of `BIrTerminator`'s `Jmp`/`CondJmp`. Caller must
/// have already staged the control provenance via `emitter.set_prov(..)`.
fn process_terminator_ir<P: Clone>(
    terminator: &IRTerminator,
    var_map: &[u32],
    emitter: &mut IrEmitter<P>,
    current_state: &[u32],
    orig_var_types: &[IRTypeId],
) -> (u32, Vec<u32>, Vec<u32>) {
    let lookup = |id: &IRVarId| -> u32 { var_map[id.0 as usize] };

    match terminator {
        IRTerminator::Jmp { target } => match &target.dest {
            IRBlockTargetId::Return => {
                let one_id = emitter.emit_one_bit();
                let result_v: Vec<u32> = target.args.iter().map(lookup).collect();
                (one_id, result_v.clone(), result_v)
            }
            IRBlockTargetId::Block(IRBlockId(0)) => {
                // `done` is always false here, so `result_v` is never
                // selected by the caller's return-segment MUX cascade
                // (see `emit_select_slot`'s `a` operand) -- empty rather
                // than state-shaped, since it holds no real return
                // contribution to pad or otherwise size against.
                let zero_id = emitter.emit_zero_bit();
                let next_v: Vec<u32> = target.args.iter().map(lookup).collect();
                (zero_id, Vec::new(), next_v)
            }
            IRBlockTargetId::Block(IRBlockId(b)) => {
                panic!(
                    "lower_to_circuit_ir: Jmp to non-zero block {} is not supported \
                     (only back-edges to block 0 are handled)",
                    b
                );
            }
            IRBlockTargetId::Dyn(_) => {
                panic!("lower_to_circuit_ir: dynamic dispatch (Dyn) is not supported");
            }
            _ => panic!("lower_to_circuit_ir: unhandled IRBlockTargetId variant — add handling for this variant"),
        },

        IRTerminator::JumpCond { condition, then_target, else_target } => {
            let val_cv = lookup(condition);

            match (&then_target.dest, &else_target.dest) {
                (IRBlockTargetId::Return, IRBlockTargetId::Block(IRBlockId(0))) => {
                    // `result_v` is exactly the Return arm's own args -- no
                    // padding to `current_state.len()`. State and return are
                    // separate, non-overlapping output segments (see the
                    // caller's assembly step): the resumable state a
                    // multi-call driver threads into the next call is
                    // `next_v` (already returned below, unconditionally),
                    // never a truncated/padded view of the return value.
                    let result_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    let next_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    (val_cv, result_v, next_v)
                }

                (IRBlockTargetId::Block(IRBlockId(0)), IRBlockTargetId::Return) => {
                    let not_val = emitter.emit_not(val_cv);
                    let result_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    let next_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    (not_val, result_v, next_v)
                }

                (IRBlockTargetId::Return, IRBlockTargetId::Return) => {
                    let one_id = emitter.emit_one_bit();
                    let then_v: Vec<u32> = then_target.args.iter().map(lookup).collect();
                    let else_v: Vec<u32> = else_target.args.iter().map(lookup).collect();
                    assert_eq!(
                        then_v.len(),
                        else_v.len(),
                        "lower_to_circuit_ir: JumpCond Return targets have different arg counts"
                    );
                    // Both paths return -- no `Block(0)` continuation exists
                    // anywhere in this terminator, so there is no multi-call
                    // "next state" concern here (unlike the asymmetric arms
                    // above): the mux'd result is genuinely the *entire*
                    // output by design, not a truncated view of a larger
                    // resumable state. Left un-padded on purpose.
                    let result_v: Vec<u32> = then_v
                        .iter()
                        .zip(else_v.iter())
                        .zip(then_target.args.iter())
                        .map(|((&a, &b), orig_id)| {
                            let ty = orig_var_types[orig_id.0 as usize].clone();
                            emit_select_slot(emitter, val_cv, a, b, &ty)
                        })
                        .collect();
                    (one_id, result_v, current_state.to_vec())
                }

                (IRBlockTargetId::Block(IRBlockId(a)), IRBlockTargetId::Block(IRBlockId(b))) => {
                    panic!(
                        "lower_to_circuit_ir: JumpCond with both targets being blocks \
                         ({}, {}) is not supported",
                        a, b
                    );
                }

                _ => panic!(
                    "lower_to_circuit_ir: unsupported JumpCond target combination \
                     (Dyn or non-zero Block)"
                ),
            }
        }
        IRTerminator::JumpTable { .. } => {
            panic!("lower_to_circuit_ir: JumpTable is not supported")
        }
        _ => panic!("lower_to_circuit_ir: unhandled IRTerminator variant — add handling for this variant"),
    }
}

/// Lower a movfuscated `IRBlocks` (i.e. [`crate::movfuscate::movfuscate_ir`]'s
/// output) to a single-block circuit, satisfying `IRBlocks::is_circuit()` —
/// the direct Volar-IR analogue of [`lower_to_circuit`]. `bit_type_id` is the
/// `IRTypeId` for `IRType::Bit` in the caller's type table (already interned
/// by `movfuscate_ir` itself).
///
/// - If `blocks.is_circuit()` is already true, returns `blocks.clone()` with no work.
/// - For a **single-block self-loop** (`JumpCond` with one target `Block(0)`
///   and the other `Return`): unrolls `limit` iterations, MUX-cascading the
///   per-iteration results with [`crate::dispatch_accumulator::emit_select_slot`]
///   (typed — state/return slots need not be `Bit`).
/// - For a **single-block unconditional loop** (`Jmp(Block(0))`): unrolls
///   `limit` iterations; the output is the state after `limit` steps.
///
/// # Panics
/// - If `blocks` has more than one block (multi-block DAG not yet implemented).
/// - If a back-edge targets any block other than block 0.
/// - If `IRBlockTargetId::Dyn` or `IRTerminator::JumpTable` is encountered.
pub fn lower_to_circuit_ir<P: Clone>(
    blocks: &IRBlocks<P>,
    bit_type_id: &IRTypeId,
    limit: u32,
    mode: LoweringMode,
) -> IRBlocks<P> {
    lower_to_circuit_ir_impl(blocks, bit_type_id, limit, mode, None)
}

/// As [`lower_to_circuit_ir`], but accepts explicit control provenance for a
/// statement-free loop.
pub fn lower_to_circuit_ir_with_control_provenance<P: Clone>(
    blocks: &IRBlocks<P>,
    bit_type_id: &IRTypeId,
    limit: u32,
    mode: LoweringMode,
    control_prov: &P,
) -> IRBlocks<P> {
    lower_to_circuit_ir_impl(blocks, bit_type_id, limit, mode, Some(control_prov))
}

fn lower_to_circuit_ir_impl<P: Clone>(
    blocks: &IRBlocks<P>,
    bit_type_id: &IRTypeId,
    limit: u32,
    mode: LoweringMode,
    control_prov: Option<&P>,
) -> IRBlocks<P> {
    if blocks.is_circuit() {
        return blocks.clone();
    }

    assert_eq!(
        blocks.blocks.len(),
        1,
        "lower_to_circuit_ir: multi-block DAG lowering is not yet implemented; \
         only single-block self-loops (movfuscate_ir's output) are currently supported"
    );

    let block0 = &blocks.blocks[0];
    let p = block0.params.len();

    let ctrl_prov: P = block0.stmts.first().map(|n| n.prov.clone())
        .or_else(|| control_prov.cloned())
        .expect("lower_to_circuit_ir: block has no statements; supply explicit control provenance");

    // Each original var's result type, computed once: params first, then one
    // per stmt (substitution never changes types, only var-id references, so
    // this table is valid for every unrolled iteration).
    let mut orig_var_types: Vec<IRTypeId> = block0.params.clone();
    for stmt in &block0.stmts {
        orig_var_types.push(ir_stmt_result_type(&stmt.kind, bit_type_id));
    }

    let mut emitter = IrEmitter::<P>::new(p as u32, bit_type_id.clone(), ctrl_prov.clone());
    let mut current_state: Vec<u32> = (0..p as u32).collect();

    let mut done_vars: Vec<u32> = Vec::new();
    let mut result_wires: Vec<Vec<u32>> = Vec::new();

    for _k in 0..limit as usize {
        let mut var_map: Vec<u32> = current_state.clone();

        for stmt in &block0.stmts {
            emitter.set_prov(stmt.prov.clone());
            let mapped = subst_ir(&stmt.kind, &var_map);
            let out_id = emitter.push(mapped);
            var_map.push(out_id);
        }

        emitter.set_prov(ctrl_prov.clone());
        let (done_v, result_v, next_v) =
            process_terminator_ir(&block0.terminator, &var_map, &mut emitter, &current_state, &orig_var_types);

        done_vars.push(done_v);
        result_wires.push(result_v);
        current_state = next_v;
    }

    // State and return are separate, non-overlapping output segments --
    // never MUX'd against each other, since their real per-slot types can
    // (and for a scalar-returning loop with wider loop-carried state,
    // genuinely do) differ. State needs no cross-iteration MUX at all:
    // `current_state` already holds the correct final next-state, threaded
    // by plain reassignment each unrolled iteration above. Only the return
    // value needs the right-to-left "first done wins" cascade, since later
    // unrolled iterations may keep "executing" (garbage past the real
    // halt) and must not overwrite an earlier iteration's real result.
    let ret_types: Vec<IRTypeId> = return_target_types(&block0.terminator, &orig_var_types)
        .unwrap_or_default();
    let ret_width = ret_types.len();

    let mut ret_gated: Vec<u32> = vec![0u32; ret_width];
    for k in (0..done_vars.len()).rev() {
        let mut new_ret_gated = Vec::with_capacity(ret_width);
        for b in 0..ret_width {
            let a = *result_wires[k].get(b).unwrap_or(&ret_gated[b]);
            let b_wire = ret_gated[b];
            emitter.set_prov(ctrl_prov.clone());
            new_ret_gated.push(emit_select_slot(&mut emitter, done_vars[k], a, b_wire, &ret_types[b]));
        }
        ret_gated = new_ret_gated;
    }

    let mut gated: Vec<u32> = current_state.clone();
    gated.extend(ret_gated);

    // ---- OR cascade for the overall done flag: OR(a,b) = select(a, 1, b) ----
    let overall_done = if done_vars.is_empty() {
        emitter.set_prov(ctrl_prov.clone());
        emitter.emit_zero_bit()
    } else {
        emitter.set_prov(ctrl_prov.clone());
        let one_bit = emitter.emit_one_bit();
        let mut acc = done_vars[0];
        for k in 1..done_vars.len() {
            emitter.set_prov(ctrl_prov.clone());
            acc = emit_select_bit(&mut emitter, acc, one_bit, done_vars[k]);
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

    let out_block = IRBlock {
        params: block0.params.clone(),
        stmts: emitter.stmts,
        terminator: IRTerminator::Jmp {
            target: IRBranchTarget::new(IRBlockTargetId::Return, ret_args),
        },
    };

    IRBlocks {
        oracles: blocks.oracles.clone(),
        actions: blocks.actions.clone(),
        rngs: blocks.rngs.clone(),
        blocks: vec![out_block],
        pre_init: blocks.pre_init.clone(),
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
    use volar_ir_common::Node;

    /// Single-bit self-loop: params=1, stmts=[One], CondJmp(param[0] → Return, else Block(0) with One).
    ///
    /// Semantics: "if input bit is 1, return it; else loop with 1".
    /// After at most 1 iteration, output is always 1.
    fn build_simple_loop() -> BIrBlocks {
        BIrBlocks { blocks: std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::One].into_iter().map(|s| Node::new(s, (), None)).collect(), // IRVarId(1) = constant 1
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
            stmts: std::vec![BIrStmt::Not(IRVarId(0))].into_iter().map(|s| Node::new(s, (), None)).collect(), // flip the bit each step
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
            stmts: std::vec![BIrStmt::Zero].into_iter().map(|s| Node::new(s, (), None)).collect(),
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

    // ------------------------------------------------------------------------
    // `lower_to_circuit_ir` (Volar IR) — same fixtures, ported to `IRStmt`/
    // `IRTerminator`, checking the same structural properties.
    // ------------------------------------------------------------------------
    mod ir {
        use super::*;
        use volar_ir::ir::{IRType, IRTypes};
        use volar_ir_common::Type;

        /// Single-bit self-loop, Volar-IR shape: params=[Bit], stmts=[Const(1)],
        /// JumpCond(param[0] → Return, else Block(0) with the constant).
        /// Same semantics as `build_simple_loop`: "if input bit is 1, return
        /// it; else loop with 1".
        fn build_simple_ir_loop() -> (IRBlocks<()>, IRTypeId) {
            let mut types = IRTypes(std::vec![]);
            let bit_ty = types.intern(IRType::Primitive(Type::Bit));
            let blocks = IRBlocks {
                oracles: std::vec![],
                actions: std::vec![],
                rngs: std::vec![],
                blocks: std::vec![IRBlock {
                    params: std::vec![bit_ty],
                    stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 1 }, bit_ty)]
                        .into_iter()
                        .map(|s| Node::new(s, (), None))
                        .collect(),
                    terminator: IRTerminator::JumpCond {
                        condition: IRVarId(0),
                        then_target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)]),
                        else_target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(0)), std::vec![IRVarId(1)]),
                    },
                }],
                pre_init: std::vec![],
            };
            (blocks, bit_ty)
        }

        #[test]
        fn test_lower_simple_ir_loop_is_circuit() {
            let (blocks, bit_ty) = build_simple_ir_loop();
            assert!(!blocks.is_circuit(), "precondition: not yet a circuit");
            let lowered = lower_to_circuit_ir(&blocks, &bit_ty, 3, LoweringMode::Unconditional);
            assert!(lowered.is_circuit(), "lowered result must satisfy is_circuit()");
            assert_eq!(lowered.blocks[0].params, std::vec![bit_ty], "param types must be preserved");
        }

        #[test]
        fn test_lower_ir_unconditional_return_width() {
            // State (1 slot) and return (1 slot) are separate, always-both-
            // present, non-overlapping output segments -- 2 total, not 1.
            let (blocks, bit_ty) = build_simple_ir_loop();
            let lowered = lower_to_circuit_ir(&blocks, &bit_ty, 3, LoweringMode::Unconditional);
            match &lowered.blocks[0].terminator {
                IRTerminator::Jmp { target } => {
                    assert_eq!(target.args.len(), 2, "Unconditional mode: 1 (state) + 1 (return)");
                    assert_eq!(target.dest, IRBlockTargetId::Return);
                }
                _ => panic!("expected Jmp(Return) terminator"),
            }
        }

        #[test]
        fn test_lower_ir_with_termination_flag_return_width() {
            let (blocks, bit_ty) = build_simple_ir_loop();
            let lowered = lower_to_circuit_ir(&blocks, &bit_ty, 3, LoweringMode::WithTerminationFlag);
            assert!(lowered.is_circuit());
            match &lowered.blocks[0].terminator {
                IRTerminator::Jmp { target } => {
                    assert_eq!(target.args.len(), 3, "WithTerminationFlag mode: 1 (done) + 1 (state) + 1 (return)");
                }
                _ => panic!("expected Jmp(Return) terminator"),
            }
        }

        #[test]
        fn test_ir_gate_count_grows_with_limit() {
            let (blocks, bit_ty) = build_simple_ir_loop();
            let l3 = lower_to_circuit_ir(&blocks, &bit_ty, 3, LoweringMode::Unconditional);
            let l6 = lower_to_circuit_ir(&blocks, &bit_ty, 6, LoweringMode::Unconditional);
            assert!(l6.blocks[0].stmts.len() > l3.blocks[0].stmts.len(), "more iterations → more gates");
        }

        #[test]
        fn test_lower_ir_unconditional_jmp_block0() {
            // Pure loop: always Jmp(Block(0)). Output = state after limit steps.
            let mut types = IRTypes(std::vec![]);
            let bit_ty = types.intern(IRType::Primitive(Type::Bit));
            let blocks: IRBlocks<()> = IRBlocks {
                oracles: std::vec![],
                actions: std::vec![],
                rngs: std::vec![],
                blocks: std::vec![IRBlock {
                    params: std::vec![bit_ty],
                    // flip the bit each step: NOT(param0) = Poly{[0]:1, const:1}
                    stmts: std::vec![IRStmt::Poly {
                        ty: bit_ty,
                        coeffs: {
                            let mut m = BTreeMap::new();
                            m.insert(std::vec![IRVarId(0)], 1u8);
                            m
                        },
                        constant: Constant { hi: 0, lo: 1 },
                    }]
                    .into_iter()
                    .map(|s| Node::new(s, (), None))
                    .collect(),
                    terminator: IRTerminator::Jmp {
                        target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(0)), std::vec![IRVarId(1)]),
                    },
                }],
                pre_init: std::vec![],
            };
            let lowered = lower_to_circuit_ir(&blocks, &bit_ty, 4, LoweringMode::Unconditional);
            assert!(lowered.is_circuit());
            assert_eq!(lowered.blocks[0].params, std::vec![bit_ty]);
        }

        #[test]
        fn test_lower_ir_both_return_jumpcond() {
            // JumpCond where both targets return: always done, result = mux(val, then, else).
            // State (2 slots, unconditionally passed through unchanged since
            // neither arm has a Block(0) continuation) and return (1 slot)
            // are still separate, always-both-present segments -- 3 total.
            let mut types = IRTypes(std::vec![]);
            let bit_ty = types.intern(IRType::Primitive(Type::Bit));
            let blocks: IRBlocks<()> = IRBlocks {
                oracles: std::vec![],
                actions: std::vec![],
                rngs: std::vec![],
                blocks: std::vec![IRBlock {
                    params: std::vec![bit_ty, bit_ty], // selector, value
                    stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, bit_ty)]
                        .into_iter()
                        .map(|s| Node::new(s, (), None))
                        .collect(),
                    terminator: IRTerminator::JumpCond {
                        condition: IRVarId(0),
                        then_target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(1)]),
                        else_target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)]),
                    },
                }],
                pre_init: std::vec![],
            };
            assert!(!blocks.is_circuit());
            let lowered = lower_to_circuit_ir(&blocks, &bit_ty, 1, LoweringMode::Unconditional);
            assert!(lowered.is_circuit());
            match &lowered.blocks[0].terminator {
                IRTerminator::Jmp { target } => assert_eq!(target.args.len(), 3),
                _ => panic!(),
            }
        }
    }
}
