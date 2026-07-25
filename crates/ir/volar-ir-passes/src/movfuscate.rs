// @reliability: normal
// @ai: assisted
//! Movfuscation pass: combine a multi-block [`BIrBlocks`] / [`IRBlocks`] into
//! a single self-looping block that dispatches via a binary-encoded PC.
//!
//! # Combined block layout
//!
//! ```text
//! params = [pc_bit_0 … pc_bit_{k-1}, state_0 … state_{w-1}]
//! ```
//! where `k = ⌈log₂(N)⌉` and `w = max(block param count)`.
//! PC bits are always `IRType::Bit`; state slots carry the types of the
//! corresponding original block params (which need not be `Bit`).
//!
//! # Dispatch
//!
//! For each original block `i`, the combined block:
//!
//! 1. Decodes `is_active_i = (pc_bits == binary(i))` via AND / NOT (always Bit).
//! 2. Re-emits block `i`'s stmts with `state_vars[0..params_i]` as params.
//! 3. Evaluates block `i`'s terminator →
//!    `(done_i, next_pc_i, next_state_i, ret_i)`.
//!
//! Because exactly one `is_active_i = 1` per execution step, outputs are
//! accumulated via `is_active * val` (scalar multiplication in the value's
//! field) and field addition — which collapses to XOR/AND for `Bit` types
//! and to proper field ops for extension-field types:
//!
//! ```text
//! done          = Σ_i  is_active_i · done_i           (GF(2) XOR/AND)
//! next_pc[j]    = Σ_i  is_active_i · next_pc_i[j]     (GF(2))
//! next_state[s] = Σ_i  is_active_i · next_state_i[s]  (field of slot s)
//! ret[m]        = Σ_i  is_active_i · ret_i[m]         (field of slot m)
//! ```
//!
//! # Terminator of the combined block
//!
//! ```text
//! JumpCond(done, Return(ret), Block(0, [next_pc…, next_state…]))
//! ```
//!
//! # Field type support (IRBlocks)
//!
//! `movfuscate_ir` accepts blocks whose params may have any scalar `IRType`
//! (`Bit`, `Galois8AES`, `Galois64`, or `Vec`).  All blocks that share a
//! state slot at position `k` must agree on its type.  The generated
//! dispatch arithmetic uses `IRStmt::Poly` with the appropriate field
//! semantics:
//!
//! - **Gate** (`is_active · val` for `is_active: Bit`, `val: T`):
//!   `Poly { {[is_active, val]: 1u8}, constant: 0 }` — scalar multiplication
//!   of a field element by a GF(2) element embedded in T.
//! - **Field add** (`a + b` for `a, b: T`):
//!   `Poly { {[a]: 1u8, [b]: 1u8}, constant: 0 }` — field addition.
//! - **Typed select** (`select(bit, a: T, b: T) = bit·(a+b) + b`): three
//!   `Poly` stmts.

use alloc::{vec, vec::Vec};
use alloc::collections::{BTreeMap, BTreeSet};
use volar_ir_common::{Constant, PreInitSegment, StorageId, Type};

use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{
        IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRType, IRTypeId,
        IRTypes, IRVarId,
    },
};

// ============================================================================
// Utility
// ============================================================================

/// Minimum number of bits needed to address `n` distinct blocks.
///
/// Returns `0` for `n ≤ 1`.
pub fn pc_bits_needed(n: usize) -> usize {
    if n <= 1 {
        return 0;
    }
    (usize::BITS - (n - 1).leading_zeros()) as usize
}

/// Number of bits in `ty` for the purpose of equality checking in
/// `emit_eq_const_ir`.  `Block` types are treated as `_32` (block indices
/// are stored as 32-bit integers at runtime).
fn bit_width_for_eq(ir_types: &[IRType], ty: IRTypeId) -> usize {
    match &ir_types[ty.0 as usize] {
        IRType::Primitive(p) => match p {
            Type::Bit     => 1,
            Type::_8      => 8,
            Type::_16     => 16,
            Type::_32     => 32,
            Type::_64     => 64,
            Type::_128    => 128,
            Type::_256    => 256,
            Type::AES8    => 8,
            Type::Galois64 => 64,
            _ => panic!("bit_width_for_eq: unsupported primitive type {:?}", p),
        },
        IRType::Block { .. } => 32,
        other => panic!("bit_width_for_eq: unsupported type {:?}", other),
    }
}

// ============================================================================
// Per-block terminator result
// ============================================================================

/// Decomposed result of one block's terminator in the combined dispatch.
pub struct TermResult {
    /// `1` if this block exits (returns), `0` if it continues looping.
    pub done: u32,
    /// Binary-encoded next PC — `pc_width` Bit vars (LSB-first).
    /// Don't-care when `done = 1`.
    pub next_pc_bits: Vec<u32>,
    /// Next state — one var per state slot. Don't-care when `done = 1`.
    pub next_state: Vec<u32>,
    /// Return values — one var per return slot. Don't-care when `done = 0`.
    pub ret_vals: Vec<u32>,
}

// ============================================================================
// Generic trait
// ============================================================================

/// Abstraction over Boolar IR and Volar IR for the movfuscation algorithm.
///
/// # Type parameters
/// `SlotTy` — an opaque descriptor for the type of a state or return slot.
/// For `BIrBlocks` this is `()` (all slots are `Bit`).
/// For `IRBlocks` this is `IRTypeId`.
///
/// # Var IDs
/// All variable IDs are plain `u32`; wrapper types are applied only in
/// [`build_output`](MovfuscCtx::build_output).
///
/// # Separation of Bit and field operations
/// - **Bit operations** (`emit_zero_bit`, `emit_one_bit`, `emit_and_bit`,
///   `emit_xor_bit`, `emit_not`) are used for PC bits and the done signal.
/// - **Slot operations** (`emit_zero_slot`, `emit_gate`, `emit_field_add`)
///   are typed and used for state and return value slots.  For `BIrBlocks`
///   these are identical to the Bit operations since all slots are `Bit`.
pub trait MovfuscCtx {
    /// The blocks collection type (input and output).
    type Blocks: Clone;
    /// Opaque type descriptor for a state or return slot.
    type SlotTy: Clone;

    // ---- Structure queries -------------------------------------------------

    fn num_blocks(blocks: &Self::Blocks) -> usize;
    fn block_param_count(blocks: &Self::Blocks, i: usize) -> usize;
    /// Number of values in the return tuple shared by all `Return` exits.
    fn return_val_width(blocks: &Self::Blocks) -> usize;

    /// Current next-var-id counter, i.e. how many vars (params + stmts)
    /// have been allocated so far. A pure query -- emits nothing -- used
    /// by [`movfuscate`] to record each original block's own contiguous
    /// var-id range in the combined output, for later splitting (Milestone
    /// 1.5 Step B: `crates/compiler/volar-weaver` weaves one Rust function
    /// per range instead of one function for the whole combined block).
    fn stmt_position(&self) -> u32;

    // ---- Primitive Bit operations (PC bits and done signal) ----------------

    fn emit_zero_bit(&mut self) -> u32;
    fn emit_one_bit(&mut self) -> u32;
    /// `a AND b` — both operands must be Bit-typed.
    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32;
    /// `a XOR b` — both operands must be Bit-typed.
    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32;
    /// `NOT a` — operand must be Bit-typed.
    fn emit_not(&mut self, a: u32) -> u32;

    // ---- Typed slot operations (state and return slots) --------------------

    /// Emit a zero of type `ty`.
    fn emit_zero_slot(&mut self, ty: &Self::SlotTy) -> u32;

    /// Emit `is_active · val` — scalar multiplication of a field element
    /// `val: ty` by a Bit `is_active`.
    ///
    /// Semantics: if `is_active = 1`, result = `val`; if `is_active = 0`,
    /// result = `0` (additive identity in `ty`'s field).
    fn emit_gate(&mut self, is_active: u32, val: u32, ty: &Self::SlotTy) -> u32;

    /// Emit `a + b` (field addition) where both `a` and `b` have type `ty`.
    fn emit_field_add(&mut self, a: u32, b: u32, ty: &Self::SlotTy) -> u32;

    // ---- Block processing --------------------------------------------------

    /// Re-emit the stmts of `block_idx`, mapping its params to the first
    /// `block_param_count(blocks, block_idx)` entries of `state_vars`.
    ///
    /// `is_active` is `block_idx`'s own dispatch condition (from
    /// `emit_is_block`, computed once by the caller right before this
    /// call). Every mid-block statement here runs unconditionally on
    /// every combined-block invocation regardless of which original
    /// block is logically active (movfuscation gates only the four
    /// *terminator* outputs — done/next_pc/next_state/ret — via the
    /// caller's own `Σ_i is_active_i · x_i` accumulation). Side-effecting
    /// statements have no such accumulation to fall back on, so
    /// implementations that emit `StorageWrite` MUST gate its own
    /// effective value with `is_active` themselves (e.g. read the
    /// current value at the same address and `emit_select_slot(is_active,
    /// real_src, current_value, ty)` as the write's `src`), or every
    /// inactive block's own stale/irrelevant local recomputation
    /// silently corrupts storage on every step it isn't genuinely active.
    ///
    /// Returns the full var table `[remapped_params…, fresh_stmt_vars…]`.
    fn emit_block_stmts(
        &mut self,
        blocks: &Self::Blocks,
        block_idx: usize,
        state_vars: &[u32],
        is_active: u32,
    ) -> Vec<u32>;

    /// Decompose `block_idx`'s terminator into the four dispatch outputs.
    ///
    /// `state_slot_types[k]` is the type of state slot `k`.
    /// `return_slot_types[m]` is the type of return slot `m`.
    fn emit_block_terminator(
        &mut self,
        blocks: &Self::Blocks,
        block_idx: usize,
        block_vals: &[u32],
        pc_width: usize,
        state_slot_types: &[Self::SlotTy],
        return_slot_types: &[Self::SlotTy],
    ) -> TermResult;

    /// Consume the context and assemble the final single-block module.
    fn build_output(
        self,
        combined_params: usize,
        done_var: u32,
        loop_vars: Vec<u32>,
        ret_vars: Vec<u32>,
    ) -> Self::Blocks;

    // ---- Default Bit-select (for PC and done) ------------------------------
    //
    // The default impls delegate to the shared helpers in
    // [`crate::dispatch_accumulator`] via a small adapter shim so that
    // `volar-ir-virt` and `movfuscate` share the same formulas.

    /// `select(cond, a, b) = AND(cond, XOR(a, b)) XOR b` — all Bit operands.
    fn emit_select_bit(&mut self, cond: u32, a: u32, b: u32) -> u32 {
        let mut shim = MovfuscShim { inner: self };
        crate::dispatch_accumulator::emit_select_bit(&mut shim, cond, a, b)
    }

    /// `select(cond, a, b) = gate(cond, a+b) + b` — typed field operands.
    ///
    /// This is `cond · (a ⊕ b) ⊕ b` expressed via field ops, which works for
    /// any field type including `Bit`.
    fn emit_select_slot(
        &mut self,
        cond: u32,
        a: u32,
        b: u32,
        ty: &Self::SlotTy,
    ) -> u32 {
        // Inlined directly (rather than via the shim) because
        // DispatchSlotPrimitives::SlotTy is a concrete associated type
        // and the generic helper needs it, which would require yet
        // another adapter.  Keeping this as the authoritative
        // formula-keeper site.
        let xab = self.emit_field_add(a, b, ty);
        let sel = self.emit_gate(cond, xab, ty);
        self.emit_field_add(sel, b, ty)
    }

    /// Emit `is_active` for `block_idx`: AND-of-(NOT-or-identity) per PC bit.
    fn emit_is_block(&mut self, pc_vars: &[u32], block_idx: usize) -> u32 {
        let mut shim = MovfuscShim { inner: self };
        crate::dispatch_accumulator::emit_is_block(&mut shim, pc_vars, block_idx)
    }

    /// Emit constant-bit vars for `block_idx` in `pc_width` bits (LSB-first).
    fn encode_pc_bits(&mut self, block_idx: usize, pc_width: usize) -> Vec<u32> {
        let mut shim = MovfuscShim { inner: self };
        crate::dispatch_accumulator::encode_pc_bits(&mut shim, block_idx, pc_width)
    }
}

/// Adapter between `MovfuscCtx` and
/// [`crate::dispatch_accumulator::DispatchBitPrimitives`].
///
/// We cannot directly `impl DispatchBitPrimitives for T: MovfuscCtx`
/// due to orphan rules, so a local shim borrows the ctx and forwards
/// each call.
struct MovfuscShim<'a, C: MovfuscCtx + ?Sized> {
    inner: &'a mut C,
}

impl<'a, C: MovfuscCtx + ?Sized> crate::dispatch_accumulator::DispatchBitPrimitives
    for MovfuscShim<'a, C>
{
    fn emit_zero_bit(&mut self) -> u32 {
        self.inner.emit_zero_bit()
    }
    fn emit_one_bit(&mut self) -> u32 {
        self.inner.emit_one_bit()
    }
    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32 {
        self.inner.emit_and_bit(a, b)
    }
    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32 {
        self.inner.emit_xor_bit(a, b)
    }
    fn emit_not(&mut self, a: u32) -> u32 {
        self.inner.emit_not(a)
    }
}

// ============================================================================
// Generic movfuscation algorithm
// ============================================================================

/// Milestone 1.5 Step B boundary metadata: one original block `i`'s own
/// contiguous var-id range `[start, end)` in the combined movfuscated
/// output's stmt list, plus the specific var ids the *trailing* cross-block
/// accumulation phase (`Σ_i is_active_i · x_i`, emitted right after every
/// block's own range) reads from this block. A weaver splitting `[start,
/// end)` into its own function must expose `is_active`/`done`/
/// `next_pc_bits`/`next_state`/`ret_vals` as that function's return values
/// -- the accumulation phase (or a combiner reproducing it) becomes that
/// function's caller.
///
/// Only meaningful for a circuit produced with `limit == 1` in a
/// subsequent `lower_to_circuit_ir` call: that's the only case where
/// `lower_to_circuit_ir`'s own var-id numbering for the unrolled output is
/// guaranteed identical to the movfuscated input's (identity `var_map` for
/// one iteration) -- these ranges do not (yet) account for the renumbering
/// `lower_to_circuit_ir` does when unrolling more than once.
#[derive(Clone, Debug)]
pub struct MovfuscBlockBoundary {
    pub start: u32,
    pub end: u32,
    pub is_active: u32,
    pub done: u32,
    pub next_pc_bits: Vec<u32>,
    pub next_state: Vec<u32>,
    pub ret_vals: Vec<u32>,
}

/// The fixed, one-time initialization of the cross-block accumulation
/// phase's running state (`done_acc = bit_zero`, `next_pc = [bit_zero; k]`,
/// `next_state`/`ret_vals` zero-slots) -- analogous to the main per-block
/// loop's shared `bit_zero` prefix, but specific to the accumulation phase.
/// `[start, end)` is small and gate-free (`state_width + ret_width` zero
/// allocations); the `_init` fields are the running-state var ids *before*
/// any block's contribution has been folded in.
#[derive(Clone, Debug)]
pub struct MovfuscAccumInit {
    pub start: u32,
    pub end: u32,
    pub done_acc: u32,
    pub next_pc: Vec<u32>,
    pub next_state: Vec<u32>,
    pub ret_vals: Vec<u32>,
}

/// One original block `i`'s own contiguous contribution to the cross-block
/// accumulation phase (`Σ_i is_active_i · x_i` folded in block-major order,
/// so every accumulation kind -- done/next_pc/next_state/ret_vals -- for
/// block `i` lands in one range), plus the running accumulator state
/// *after* folding it in. A weaver chunking the accumulation phase into
/// groups of blocks needs: for a chunk covering blocks `[lo, hi)`, the
/// *running state* from `steps[lo-1]` (or [`MovfuscAccumInit`] if
/// `lo == 0`) as that chunk-function's own input, and `steps[hi-1]`'s
/// running state as its output -- turning the "one combiner needs every
/// block's state at once" problem into "each chunk-function needs only
/// its own chunk's blocks' state, plus the previous chunk's running total".
#[derive(Clone, Debug)]
pub struct MovfuscAccumStep {
    pub start: u32,
    pub end: u32,
    pub done_acc: u32,
    pub next_pc: Vec<u32>,
    pub next_state: Vec<u32>,
    pub ret_vals: Vec<u32>,
}

/// The whole cross-block accumulation phase's boundary metadata: the
/// one-time [`MovfuscAccumInit`] followed by one [`MovfuscAccumStep`] per
/// original block, in order.
#[derive(Clone, Debug)]
pub struct MovfuscAccumInfo {
    pub init: MovfuscAccumInit,
    pub steps: Vec<MovfuscAccumStep>,
}

/// Translate every var id in `boundary` through `remap` (old `IRVarId.0` ->
/// new `IRVarId.0`, e.g. from [`volar_ir_opt::ir::dce_ir_blocks_with_remap`]
/// — not depended on directly by this crate, since `volar-ir-passes` has no
/// dependency on `volar-ir-opt`; callers own the remap and just pass it
/// through). `fold_ir_blocks`/`store_forward_ir_blocks` need no equivalent
/// call here — neither ever changes a statement's own index (constant
/// folding rewrites in place; store forwarding only redirects operand
/// references via an alias map), so `[start, end)` ranges and every
/// individually-named var id in the boundary structs stay valid across
/// both unmodified. Only a pass that actually deletes/reorders statements
/// (currently: DCE) requires this.
///
/// Panics if `remap` has no entry for a referenced var — this would mean
/// the pass considered that var dead and removed it, even though the
/// boundary metadata still needs it; that's a real bug in whichever pass
/// produced `remap` (or in how it's being applied), not something to
/// paper over silently.
pub fn remap_movfusc_boundary(boundary: &MovfuscBlockBoundary, remap: &BTreeMap<u32, u32>) -> MovfuscBlockBoundary {
    let r = |v: u32| *remap.get(&v).unwrap_or_else(|| panic!(
        "remap_movfusc_boundary: var {v} referenced by boundary metadata has no remap entry -- \
         a downstream pass removed a variable this boundary still needs"
    ));
    MovfuscBlockBoundary {
        start: r(boundary.start),
        end: r(boundary.end),
        is_active: r(boundary.is_active),
        done: r(boundary.done),
        next_pc_bits: boundary.next_pc_bits.iter().map(|&v| r(v)).collect(),
        next_state: boundary.next_state.iter().map(|&v| r(v)).collect(),
        ret_vals: boundary.ret_vals.iter().map(|&v| r(v)).collect(),
    }
}

/// As [`remap_movfusc_boundary`], applied to every boundary in a slice —
/// the common case (one boundary per original block).
pub fn remap_movfusc_boundaries(boundaries: &[MovfuscBlockBoundary], remap: &BTreeMap<u32, u32>) -> Vec<MovfuscBlockBoundary> {
    boundaries.iter().map(|b| remap_movfusc_boundary(b, remap)).collect()
}

/// As [`remap_movfusc_boundary`], for the cross-block accumulation phase's
/// own boundary metadata.
pub fn remap_movfusc_accum_info(info: &MovfuscAccumInfo, remap: &BTreeMap<u32, u32>) -> MovfuscAccumInfo {
    let r = |v: u32| *remap.get(&v).unwrap_or_else(|| panic!(
        "remap_movfusc_accum_info: var {v} referenced by accum-info metadata has no remap entry -- \
         a downstream pass removed a variable this metadata still needs"
    ));
    let remap_init = |init: &MovfuscAccumInit| MovfuscAccumInit {
        start: r(init.start),
        end: r(init.end),
        done_acc: r(init.done_acc),
        next_pc: init.next_pc.iter().map(|&v| r(v)).collect(),
        next_state: init.next_state.iter().map(|&v| r(v)).collect(),
        ret_vals: init.ret_vals.iter().map(|&v| r(v)).collect(),
    };
    let remap_step = |step: &MovfuscAccumStep| MovfuscAccumStep {
        start: r(step.start),
        end: r(step.end),
        done_acc: r(step.done_acc),
        next_pc: step.next_pc.iter().map(|&v| r(v)).collect(),
        next_state: step.next_state.iter().map(|&v| r(v)).collect(),
        ret_vals: step.ret_vals.iter().map(|&v| r(v)).collect(),
    };
    MovfuscAccumInfo {
        init: remap_init(&info.init),
        steps: info.steps.iter().map(remap_step).collect(),
    }
}

/// Combine all blocks of `blocks` into a single self-looping block.
///
/// `state_slot_types[k]` — type of state slot `k` (length = max block param
/// count).  `return_slot_types[m]` — type of return value `m`.
///
/// Returns the single-block module, each original block's own
/// [`MovfuscBlockBoundary`] (empty if `blocks` already had one block --
/// nothing was combined, so there is nothing to report boundaries for),
/// and the accumulation phase's own [`MovfuscAccumInfo`] (its `steps` is
/// also empty in the single-block case, since there is no accumulation
/// phase at all then). If `blocks` already has one block, the module is
/// returned unchanged via
/// `Clone`.
/// `watch`: `(orig_block_idx, orig_var_id)` pairs -- for each, on that
/// block's own emission, records `(orig_block_idx, orig_var_id,
/// combined_block_var_id)` into the returned `Vec` (via `block_vals`,
/// which IS the orig-var-id-indexed map from the block's own emission).
/// Temporary diagnostic hook: lets a caller find where an arbitrary
/// pre-movfuscation value (e.g. a specific `JumpCond`'s own condition
/// var, identified via a static def-use trace) ends up in the *combined*
/// circuit's own var numbering, without re-deriving the emission-order
/// offset by hand (error-prone -- `emit_is_block` emits a variable,
/// unknown-in-advance number of statements before `emit_block_stmts`
/// even starts). Empty `watch` costs nothing beyond one slice-is-empty
/// check per block.
pub fn movfuscate<C: MovfuscCtx>(
    mut ctx: C,
    blocks: &C::Blocks,
    state_slot_types: Vec<C::SlotTy>,
    return_slot_types: Vec<C::SlotTy>,
    watch: &[(usize, u32)],
) -> (C::Blocks, Vec<MovfuscBlockBoundary>, MovfuscAccumInfo, Vec<(usize, u32, u32)>) {
    let n = C::num_blocks(blocks);
    assert!(n >= 1, "movfuscate: empty block list");
    if n == 1 {
        let empty_init = MovfuscAccumInit { start: 0, end: 0, done_acc: 0, next_pc: Vec::new(), next_state: Vec::new(), ret_vals: Vec::new() };
        return (blocks.clone(), Vec::new(), MovfuscAccumInfo { init: empty_init, steps: Vec::new() }, Vec::new());
    }

    let pc_width = pc_bits_needed(n);
    let state_width = state_slot_types.len();
    let ret_width = return_slot_types.len();
    let combined_params = pc_width + state_width;

    // Combined block parameter var IDs:
    //   [0, pc_width)               — Bit PC bits (0 = entry block 0)
    //   [pc_width, combined_params)  — typed state slots
    let pc_vars: Vec<u32> = (0..pc_width as u32).collect();
    let state_vars: Vec<u32> = (pc_width as u32..combined_params as u32).collect();

    // Shared Bit zero for PC and done accumulators.
    let bit_zero = ctx.emit_zero_bit();

    // ---- Per-block evaluation -----------------------------------------------

    struct BlockResult {
        is_active: u32,
        done: u32,
        next_pc_bits: Vec<u32>,
        next_state: Vec<u32>,
        ret_vals: Vec<u32>,
    }

    let mut results: Vec<BlockResult> = Vec::with_capacity(n);
    // Milestone 1.5 Step B: record each original block's own contiguous
    // var-id range in the combined output (`stmt_position()` before/after
    // its `is_active`/stmts/terminator processing), so a later weaving
    // pass can split the combined block's gates back out per original
    // block without re-deriving these boundaries. Purely additive --
    // does not affect what `ctx` emits or the resulting `C::Blocks`.
    let mut block_ranges: Vec<(u32, u32)> = Vec::with_capacity(n);
    let mut watch_results: Vec<(usize, u32, u32)> = Vec::new();
    for i in 0..n {
        let range_start = ctx.stmt_position();
        let is_active = ctx.emit_is_block(&pc_vars, i);
        let block_vals = ctx.emit_block_stmts(blocks, i, &state_vars, is_active);
        for &(wb, wv) in watch {
            if wb == i {
                if let Some(&combined) = block_vals.get(wv as usize) {
                    watch_results.push((wb, wv, combined));
                }
            }
        }
        let term = ctx.emit_block_terminator(
            blocks,
            i,
            &block_vals,
            pc_width,
            &state_slot_types,
            &return_slot_types,
        );
        block_ranges.push((range_start, ctx.stmt_position()));
        results.push(BlockResult {
            is_active,
            done: term.done,
            next_pc_bits: term.next_pc_bits,
            next_state: term.next_state,
            ret_vals: term.ret_vals,
        });
    }

    // Boundary metadata: each block's own stmt range plus its "exported"
    // interface -- the exact var ids the *trailing* cross-block
    // accumulation phase (below) reads from this block. A weaver splitting
    // this block's range into its own function must expose these as that
    // function's return values (the combiner/trailing phase becomes that
    // function's caller).
    let block_boundaries: Vec<MovfuscBlockBoundary> = block_ranges
        .into_iter()
        .zip(results.iter())
        .map(|((start, end), br)| MovfuscBlockBoundary {
            start,
            end,
            is_active: br.is_active,
            done: br.done,
            next_pc_bits: br.next_pc_bits.clone(),
            next_state: br.next_state.clone(),
            ret_vals: br.ret_vals.clone(),
        })
        .collect();

    // ---- Accumulate across mutually-exclusive active bits ------------------
    //
    // Exactly one is_active_i = 1 per valid step.  Scalar-mult then field-add
    // selects the active block's contribution:
    //   result = Σ_i  (is_active_i · x_i)
    //
    // Block-major (each block's full done/next_pc/next_state/ret_vals
    // contribution in one contiguous range, rather than four separate
    // all-blocks loops) -- purely a reordering of the same commutative/
    // associative XOR/field-add operations, so it doesn't change the
    // final `done_acc`/`next_pc`/`next_state`/`ret_vals` values, only the
    // intermediate statement order. This lets a later weaver chunk the
    // accumulation phase too (Milestone 1.5 Step B: the combiner itself
    // must not need every block's exported state simultaneously) via
    // `accum_init`/`accum_steps` below.

    let accum_init_start = ctx.stmt_position();
    let mut done_acc = bit_zero;
    let mut next_pc = vec![bit_zero; pc_width];
    // Seeded directly from each slot's own *incoming* value (`state_vars`,
    // the combined block's own params for the state slots -- see above),
    // not a zero constant -- no new statement needed, referencing an
    // existing param requires none (this circuit is fundamentally a
    // looped, return-to-parameter construction: a slot's own value when no
    // block in a given step touches it *is* its own incoming param, not a
    // free-floating zero).
    //
    // This is what makes the tunnelled-slot skip below exact:
    // `Σ_i is_active_i · br_i.next_state[k]` and
    // `state_vars[k] ⊕ Σ_{i: touches k} is_active_i · (br_i.next_state[k] ⊕ state_vars[k])`
    // are the *same* value (an algebraic identity given `Σ_i is_active_i = 1`
    // and GF(2) distributivity -- see `docs/interpreter-honest-e2e-zk-plan.md`),
    // so skipping the (typically many) blocks that don't touch slot `k`
    // costs nothing. Consumers (the split-weave, `crates/compiler/volar-weaver/src/vole.rs`)
    // must bind the circuit's own top-level params (`insert_w_wires` or
    // equivalent) wherever they resolve `accum_info`'s own var ids, exactly
    // as they already do for ordinary block processing -- this was the
    // one real gap (a stray type-only probe context that skipped that
    // binding), now fixed there too.
    let mut next_state: Vec<u32> = state_vars.clone();
    let mut ret_vals: Vec<u32> = return_slot_types
        .iter()
        .map(|ty| ctx.emit_zero_slot(ty))
        .collect();
    let accum_init = MovfuscAccumInit {
        start: accum_init_start,
        end: ctx.stmt_position(),
        done_acc,
        next_pc: next_pc.clone(),
        next_state: next_state.clone(),
        ret_vals: ret_vals.clone(),
    };

    let mut accum_steps: Vec<MovfuscAccumStep> = Vec::with_capacity(n);
    for br in &results {
        let range_start = ctx.stmt_position();
        let g = ctx.emit_and_bit(br.is_active, br.done);
        done_acc = ctx.emit_xor_bit(done_acc, g);
        for j in 0..pc_width {
            let g = ctx.emit_and_bit(br.is_active, br.next_pc_bits[j]);
            next_pc[j] = ctx.emit_xor_bit(next_pc[j], g);
        }
        for k in 0..state_width {
            // Tunnelled/unchanged-state-slot elimination: this block's own
            // contribution for slot `k` is exactly its own incoming value
            // (`state_vars[k]`) -- i.e. this block's original logic never
            // wrote slot `k` at all. Its contribution to the accumulation
            // is then provably a no-op (see the seed's own comment above)
            // -- skip the gate + add entirely instead of emitting a
            // statement pair that would just reproduce `next_state[k]`
            // unchanged. Sound by construction: this can only under-detect
            // (a semantically untouched slot reachable via a differently-
            // numbered but equal var would just miss the optimization,
            // never break correctness), never over-detect.
            if br.next_state[k] != state_vars[k] {
                let g = ctx.emit_gate(br.is_active, br.next_state[k], &state_slot_types[k]);
                next_state[k] = ctx.emit_field_add(next_state[k], g, &state_slot_types[k]);
            }
        }
        for m in 0..ret_width {
            let g = ctx.emit_gate(br.is_active, br.ret_vals[m], &return_slot_types[m]);
            ret_vals[m] = ctx.emit_field_add(ret_vals[m], g, &return_slot_types[m]);
        }
        accum_steps.push(MovfuscAccumStep {
            start: range_start,
            end: ctx.stmt_position(),
            done_acc,
            next_pc: next_pc.clone(),
            next_state: next_state.clone(),
            ret_vals: ret_vals.clone(),
        });
    }
    let accum_info = MovfuscAccumInfo { init: accum_init, steps: accum_steps };

    // loop_vars = [next_pc_bits…, next_state…]
    let mut loop_vars = Vec::with_capacity(combined_params);
    loop_vars.extend_from_slice(&next_pc);
    loop_vars.extend_from_slice(&next_state);

    (
        ctx.build_output(combined_params, done_acc, loop_vars, ret_vals),
        block_boundaries,
        accum_info,
        watch_results,
    )
}

// ============================================================================
// BIrBlocks implementation  (SlotTy = () — all slots are Bit)
// ============================================================================

fn subst_biir(stmt: &BIrStmt, var_map: &[u32]) -> BIrStmt {
    let s = |id: &IRVarId| IRVarId(var_map[id.0 as usize]);
    match stmt {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(s(a), s(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(s(a), s(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(s(a), s(b)),
        BIrStmt::Not(a) => BIrStmt::Not(s(a)),
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
            storage: storage.clone(),
            bit_width: *bit_width,
            addr: addr.iter().map(|v| s(v)).collect(),
        },
        BIrStmt::StorageWrite { storage, src, bit_width, addr } => BIrStmt::StorageWrite {
            storage: storage.clone(),
            src: s(src),
            bit_width: *bit_width,
            addr: addr.iter().map(|v| s(v)).collect(),
        },
        _ => panic!("subst_biir: unhandled BIrStmt variant — add substitution for this variant"),
    }
}

struct BIrCtx<P: Clone = ()> {
    stmts: Vec<volar_ir_common::Node<BIrStmt, P>>,
    next_id: u32,
    ctrl_prov: P,
}

impl<P: Clone> BIrCtx<P> {
    fn new(first_id: u32, ctrl_prov: P) -> Self {
        Self { stmts: Vec::new(), next_id: first_id, ctrl_prov }
    }

    fn push(&mut self, stmt: BIrStmt, prov: P) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(volar_ir_common::Node::new(stmt, prov, None));
        id
    }

    fn process_biir_target(
        &mut self,
        target: &BIrTarget,
        block_vals: &[u32],
        pc_width: usize,
        state_width: usize,
        ret_width: usize,
    ) -> (u32, Vec<u32>, Vec<u32>, Vec<u32>) {
        let lookup = |id: &IRVarId| block_vals[id.0 as usize];
        match &target.block {
            IRBlockTargetId::Return => {
                let done = self.emit_one_bit();
                let next_pc = self.encode_pc_bits(0, pc_width);
                let next_state = (0..state_width).map(|_| self.emit_zero_bit()).collect();
                let mut ret: Vec<u32> = target.args.iter().map(lookup).collect();
                while ret.len() < ret_width {
                    ret.push(self.emit_zero_bit());
                }
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Block(IRBlockId(j)) => {
                let done = self.emit_zero_bit();
                let next_pc = self.encode_pc_bits(*j as usize, pc_width);
                let mut next_state: Vec<u32> = target.args.iter().map(lookup).collect();
                while next_state.len() < state_width {
                    next_state.push(self.emit_zero_bit());
                }
                let ret = (0..ret_width).map(|_| self.emit_zero_bit()).collect();
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Dyn(_) => {
                panic!("movfuscate_biir: Dyn jump targets are not supported")
            }
            _ => panic!("movfuscate_biir: unhandled IRBlockTargetId variant — add handling for this variant"),
        }
    }
}


impl<P: Clone> MovfuscCtx for BIrCtx<P> {
    type Blocks = BIrBlocks<P>;
    /// All BIr slots are `Bit`; no type information needed.
    type SlotTy = ();

    fn num_blocks(blocks: &BIrBlocks<P>) -> usize {
        blocks.blocks.len()
    }

    fn block_param_count(blocks: &BIrBlocks<P>, i: usize) -> usize {
        blocks.blocks[i].params as usize
    }

    fn stmt_position(&self) -> u32 {
        self.next_id
    }

    fn return_val_width(blocks: &BIrBlocks<P>) -> usize {
        for block in &blocks.blocks {
            match &block.terminator {
                BIrTerminator::Jmp(t) if matches!(t.block, IRBlockTargetId::Return) => {
                    return t.args.len()
                }
                BIrTerminator::CondJmp { then_target, .. }
                    if matches!(then_target.block, IRBlockTargetId::Return) =>
                {
                    return then_target.args.len()
                }
                BIrTerminator::CondJmp { else_target, .. }
                    if matches!(else_target.block, IRBlockTargetId::Return) =>
                {
                    return else_target.args.len()
                }
                _ => {}
            }
        }
        0
    }

    // Bit ops ----------------------------------------------------------------

    fn emit_zero_bit(&mut self) -> u32 {
        let p = self.ctrl_prov.clone();
        self.push(BIrStmt::Zero, p)
    }

    fn emit_one_bit(&mut self) -> u32 {
        let p = self.ctrl_prov.clone();
        self.push(BIrStmt::One, p)
    }

    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a; // idempotent
        }
        let p = self.ctrl_prov.clone();
        self.push(BIrStmt::And(IRVarId(a), IRVarId(b)), p)
    }

    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            let p = self.ctrl_prov.clone();
            return self.push(BIrStmt::Zero, p);
        }
        let p = self.ctrl_prov.clone();
        self.push(BIrStmt::Xor(IRVarId(a), IRVarId(b)), p)
    }

    fn emit_not(&mut self, a: u32) -> u32 {
        let p = self.ctrl_prov.clone();
        self.push(BIrStmt::Not(IRVarId(a)), p)
    }

    // Slot ops (SlotTy = ()) = Bit ops --------------------------------------

    fn emit_zero_slot(&mut self, _ty: &()) -> u32 {
        self.emit_zero_bit()
    }

    fn emit_gate(&mut self, is_active: u32, val: u32, _ty: &()) -> u32 {
        self.emit_and_bit(is_active, val)
    }

    fn emit_field_add(&mut self, a: u32, b: u32, _ty: &()) -> u32 {
        self.emit_xor_bit(a, b)
    }

    // Block processing -------------------------------------------------------

    fn emit_block_stmts(
        &mut self,
        blocks: &BIrBlocks<P>,
        block_idx: usize,
        state_vars: &[u32],
        _is_active: u32,
    ) -> Vec<u32> {
        // NOTE: unlike the IRBlocks impl below, this legacy BIr path does
        // not special-case StorageWrite (or gate it on `_is_active`) --
        // per `docs/agent-context/boolar-ir-conflicts.md`, Boolar-IR/BIr
        // is deliberately left untouched unless a real caller needs it
        // fixed. If BIr-level circuits ever use StorageWrite for real,
        // this has the same ungated-side-effect issue the IRBlocks impl
        // just fixed.
        let block = &blocks.blocks[block_idx];
        let p = block.params as usize;
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());
        var_map.extend_from_slice(&state_vars[..p]);
        for stmt in block.stmts.iter() {
            let prov = stmt.prov.clone();
            let mapped = subst_biir(&stmt.kind, &var_map);
            let id = self.push(mapped, prov);
            var_map.push(id);
        }
        var_map
    }

    fn emit_block_terminator(
        &mut self,
        blocks: &BIrBlocks<P>,
        block_idx: usize,
        block_vals: &[u32],
        pc_width: usize,
        state_slot_types: &[()],
        return_slot_types: &[()],
    ) -> TermResult {
        let state_width = state_slot_types.len();
        let ret_width = return_slot_types.len();
        let block = &blocks.blocks[block_idx];
        match &block.terminator {
            BIrTerminator::Jmp(target) => {
                let (done, next_pc_bits, next_state, ret_vals) = self.process_biir_target(
                    target,
                    block_vals,
                    pc_width,
                    state_width,
                    ret_width,
                );
                TermResult { done, next_pc_bits, next_state, ret_vals }
            }
            BIrTerminator::CondJmp { val, then_target, else_target } => {
                let cond = block_vals[val.0 as usize];
                let (t_done, t_npc, t_ns, t_ret) = self.process_biir_target(
                    then_target,
                    block_vals,
                    pc_width,
                    state_width,
                    ret_width,
                );
                let (e_done, e_npc, e_ns, e_ret) = self.process_biir_target(
                    else_target,
                    block_vals,
                    pc_width,
                    state_width,
                    ret_width,
                );
                // All values are Bit so bit-select == slot-select here.
                let done = self.emit_select_bit(cond, t_done, e_done);
                let next_pc_bits = (0..pc_width)
                    .map(|j| self.emit_select_bit(cond, t_npc[j], e_npc[j]))
                    .collect();
                let next_state = (0..state_width)
                    .map(|k| self.emit_select_bit(cond, t_ns[k], e_ns[k]))
                    .collect();
                let ret_vals = (0..ret_width)
                    .map(|m| self.emit_select_bit(cond, t_ret[m], e_ret[m]))
                    .collect();
                TermResult { done, next_pc_bits, next_state, ret_vals }
            }
            _ => panic!("emit_block_terminator: unhandled BIrTerminator variant — add handling for this variant"),
        }
    }

    fn build_output(
        self,
        combined_params: usize,
        done_var: u32,
        loop_vars: Vec<u32>,
        ret_vars: Vec<u32>,
    ) -> BIrBlocks<P> {
        BIrBlocks { blocks: vec![BIrBlock {
            params: combined_params as u32,
            stmts: self.stmts,
            terminator: BIrTerminator::CondJmp {
                val: IRVarId(done_var),
                then_target: BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: ret_vars.into_iter().map(IRVarId).collect(),
                },
                else_target: BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(0)),
                    args: loop_vars.into_iter().map(IRVarId).collect(),
                },
            },
        }], pre_init: vec![] }
    }
}

// ============================================================================
// IRBlocks implementation  (SlotTy = IRTypeId — full field type support)
// ============================================================================

// ---- Substitution ----------------------------------------------------------

pub(crate) fn subst_ir(stmt: &IRStmt, var_map: &[u32]) -> IRStmt {
    let s = |id: &IRVarId| IRVarId(var_map[id.0 as usize]);
    match stmt {
        IRStmt::StorageRead { storage, ty, addr } => IRStmt::StorageRead { storage: *storage, ty: ty.clone(), addr: s(addr) },
        IRStmt::StorageWrite { storage, src, ty, addr } => {
            IRStmt::StorageWrite { storage: *storage, src: s(src), ty: ty.clone(), addr: s(addr) }
        }
        IRStmt::Const(c, ty) => IRStmt::Const(*c, ty.clone()),
        IRStmt::Transmute { src, src_ty, dst_ty } => {
            IRStmt::Transmute { src: s(src), src_ty: src_ty.clone(), dst_ty: dst_ty.clone() }
        }
        IRStmt::Poly { ty, coeffs, constant } => IRStmt::Poly {
            ty: *ty,
            coeffs: coeffs
                .iter()
                .map(|(vars, &coeff)| {
                    let mut nv: Vec<IRVarId> = vars.iter().map(s).collect();
                    nv.sort();
                    (nv, coeff)
                })
                .collect(),
            constant: *constant,
        },
        IRStmt::Rol { src, ty, n } => IRStmt::Rol { src: s(src), ty: ty.clone(), n: *n },
        IRStmt::Ror { src, ty, n } => IRStmt::Ror { src: s(src), ty: ty.clone(), n: *n },
        IRStmt::Merge { parts, ty } => {
            IRStmt::Merge { parts: parts.iter().map(s).collect(), ty: ty.clone() }
        }
        IRStmt::Splat { src, ty } => IRStmt::Splat { src: s(src), ty: ty.clone() },
        IRStmt::Shuffle { result_bits, ty } => IRStmt::Shuffle {
            result_bits: result_bits.iter().map(|(b, v)| (*b, s(v))).collect(),
            ty: ty.clone(),
        },
        IRStmt::OracleCall { name, args, output_tys, result_ty } => IRStmt::OracleCall {
            name: name.clone(),
            args: args.iter().map(s).collect(),
            output_tys: output_tys.clone(),
            result_ty: result_ty.clone(),
        },
        IRStmt::OracleOutput { call, idx, ty } =>
            IRStmt::OracleOutput { call: s(call), idx: *idx, ty: ty.clone() },
        IRStmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } =>
            IRStmt::ActionCall {
                name: name.clone(),
                guard: s(guard),
                args: args.iter().map(s).collect(),
                fallbacks: fallbacks.iter().map(s).collect(),
                output_tys: output_tys.clone(),
                result_ty: result_ty.clone(),
            },
        IRStmt::ActionOutput { call, idx, ty } =>
            IRStmt::ActionOutput { call: s(call), idx: *idx, ty: ty.clone() },
        IRStmt::Rng { name, ty } => IRStmt::Rng { name: name.clone(), ty: ty.clone() },
        _ => panic!("subst_ir: unhandled IRStmt variant — add substitution for this variant"),
    }
}

// ---- Type inference --------------------------------------------------------

/// Find the "wider" of two type IDs, treating `Bit` as the additive identity
/// for promotion (`Bit` × `T` = `T`).
///
/// Panics if two non-Bit types are incompatible (different non-Bit type IDs).
fn promote_type(a: &IRTypeId, b: &IRTypeId, ir_types: &[IRType]) -> IRTypeId {
    let a_is_bit = matches!(ir_types[a.0 as usize], IRType::Primitive(Type::Bit));
    let b_is_bit = matches!(ir_types[b.0 as usize], IRType::Primitive(Type::Bit));
    if a_is_bit {
        b.clone()
    } else if b_is_bit {
        a.clone()
    } else if a.0 == b.0 {
        a.clone()
    } else {
        panic!(
            "movfuscate_ir: incompatible types in polynomial: {:?} and {:?}",
            ir_types[a.0 as usize],
            ir_types[b.0 as usize]
        )
    }
}

/// Infer the result type of a `Poly` stmt: the widest field type among all
/// variable operands, with `Bit` as the identity for promotion.
fn infer_poly_result_type(
    coeffs: &BTreeMap<Vec<IRVarId>, u8>,
    var_types: &[IRTypeId],
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
) -> IRTypeId {
    let mut result = bit_type_id.clone();
    for (vars, _) in coeffs {
        for var in vars {
            let ty = &var_types[var.0 as usize];
            result = promote_type(&result, ty, ir_types);
        }
    }
    result
}

/// Infer the result type of any `IRStmt`.
fn infer_stmt_result_type(
    stmt: &IRStmt,
    var_types: &[IRTypeId],
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
) -> IRTypeId {
    match stmt {
        IRStmt::Const(_, ty) | IRStmt::StorageRead { ty, .. } => ty.clone(),
        IRStmt::Transmute { dst_ty, .. } => dst_ty.clone(),
        IRStmt::Poly { coeffs, .. } => {
            infer_poly_result_type(coeffs, var_types, ir_types, bit_type_id)
        }
        IRStmt::Rol { ty, .. }
        | IRStmt::Ror { ty, .. }
        | IRStmt::Merge { ty, .. }
        | IRStmt::Splat { ty, .. } => ty.clone(),
        // StorageWrite has no meaningful result; use Bit as a placeholder.
        IRStmt::StorageWrite { .. } => bit_type_id.clone(),
        // Shuffle carries its result type explicitly.
        IRStmt::Shuffle { ty, .. } => ty.clone(),
        // Oracle/action call results are pre-interned tuple types.
        IRStmt::OracleCall { result_ty, .. } | IRStmt::ActionCall { result_ty, .. } =>
            result_ty.clone(),
        // Output projections carry their concrete scalar type.
        IRStmt::OracleOutput { ty, .. } | IRStmt::ActionOutput { ty, .. } => ty.clone(),
        // RNG produces a fresh value of the declared type.
        IRStmt::Rng { ty, .. } => ty.clone(),
        _ => panic!("infer_stmt_result_type: unhandled IRStmt variant — add type inference for this variant"),
    }
}

// ---- Pre-pass: infer all variable types in a block (static, before emission)

fn infer_block_var_types<P: Clone>(
    block: &IRBlock<P>,
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
) -> Vec<IRTypeId> {
    let mut var_types: Vec<IRTypeId> = block.params.clone();
    for stmt in &block.stmts {
        let ty = infer_stmt_result_type(&stmt.kind, &var_types, ir_types, bit_type_id);
        var_types.push(ty);
    }
    var_types
}

// ---- IrCtx -----------------------------------------------------------------

/// How a block's own param at a given position resolves to a physical
/// state slot in the combined block's flat state vector — always built by
/// [`compute_static_slot_classes`]'s sound, edge-aware analysis.
enum SlotAllocation {
    /// `slot_of[block_idx][position]` for a specific, known block, plus
    /// `sig_fallback[type-signature]` for a `Dyn` target's own runtime
    /// dispatch (see `compute_static_slot_classes`'s own doc comment on
    /// why both are needed).
    PerBlock {
        slot_of: Vec<Vec<(usize, usize)>>,
        sig_fallback: BTreeMap<Vec<u32>, Vec<(usize, usize)>>,
    },
}

struct IrCtx<P: Clone = ()> {
    stmts: Vec<volar_ir_common::Node<IRStmt, P>>,
    /// Provenance to attach to the next emitted stmt (cloned on `push_typed`).
    /// Set by `emit_block_stmts` before each source stmt; synthetic stmts inherit
    /// the last set provenance (no reset to default).
    pending_prov: P,
    /// Fallback provenance for infrastructure gates that have no direct source
    /// (e.g. block-dispatch constants, loop control).  Derived from the first
    /// available source statement in the input circuit.
    ctrl_prov: P,
    next_id: u32,
    bit_type_id: IRTypeId,
    /// `Vec(pc_width, Bit)` — the type used for block-references in storage.
    vec_pc_type_id: IRTypeId,
    /// Types of all vars emitted so far (params + stmts).
    var_types: Vec<IRTypeId>,
    /// Clone of the types table used for type inference.
    ir_types: Vec<IRType>,
    /// Param types of the combined block (for build_output).
    combined_param_types: Vec<IRTypeId>,
    /// Number of Bit vars needed to binary-encode any block index.
    pc_width: usize,
    /// Per-block tracking of `Block`-typed vars.
    ///
    /// Keyed by **original var ID within the block currently being processed**
    /// (same index space as `block_vals` / `var_map`).
    /// Value: `(bit_vars, block_param_sig)` where `bit_vars` are the
    /// combined-block var IDs of the `pc_width` Bit slots that binary-encode
    /// the block reference, and `block_param_sig` is the `params` field from
    /// `IRType::Block { params }`.
    ///
    /// Cleared at the start of each `emit_block_stmts` call.
    block_var_to_bits: BTreeMap<u32, (Vec<u32>, Vec<IRTypeId>)>,
    /// How a block's own param at position `k` resolves to a physical
    /// state slot — see [`SlotAllocation`].
    slot_alloc: SlotAllocation,
}

impl<P: Clone> IrCtx<P> {
    fn new(
        first_id: u32,
        bit_type_id: IRTypeId,
        vec_pc_type_id: IRTypeId,
        combined_param_types: Vec<IRTypeId>,
        ir_types: Vec<IRType>,
        pc_width: usize,
        ctrl_prov: P,
        slot_alloc: SlotAllocation,
    ) -> Self {
        let var_types = combined_param_types.clone();
        Self {
            stmts: Vec::new(),
            pending_prov: ctrl_prov.clone(),
            ctrl_prov,
            next_id: first_id,
            bit_type_id,
            vec_pc_type_id,
            var_types,
            ir_types,
            combined_param_types,
            pc_width,
            block_var_to_bits: BTreeMap::new(),
            slot_alloc,
        }
    }

    /// Resolve `params` (a block's own param-type list, in original-param
    /// order) to `[(slot_start, slot_count), …]` — see [`SlotAllocation`]
    /// for the two schemes this can dispatch to. `block_idx` must be the
    /// *actual* block these params belong to when known (needed by
    /// `PerBlock`); pass `None` only for a `Dyn` target's own signature,
    /// where no concrete target block is known ahead of time.
    fn slot_map_for(&self, block_idx: Option<usize>, params: &[IRTypeId]) -> Vec<(usize, usize)> {
        let SlotAllocation::PerBlock { slot_of, sig_fallback } = &self.slot_alloc;
        match block_idx {
            Some(b) => slot_of[b].clone(),
            // A zero-width signature needs no lookup -- there's nothing to
            // scatter, regardless of whether any actual block happens to
            // declare an (unrelated) empty param list.
            None if params.is_empty() => Vec::new(),
            None => {
                let key: Vec<u32> = params.iter().map(|t| t.0).collect();
                sig_fallback.get(&key).unwrap_or_else(|| {
                    panic!(
                        "movfuscate_ir: Dyn target signature {key:?} has no matching block \
                         anywhere in the function -- sig_fallback must be built from the same \
                         block set this signature comes from"
                    )
                }).clone()
            }
        }
    }

    /// Scatter `args` into the expanded state slots described by
    /// `target_slot_map`.  `Block`-typed args (slot_count > 1) are expanded
    /// from `block_var_to_bits`; ordinary args are taken from `block_vals`.
    /// Unset slots are filled with typed zero constants.
    fn scatter_args_to_state(
        &mut self,
        args: &[IRVarId],
        block_vals: &[u32],
        target_slot_map: &[(usize, usize)],
        state_slot_types: &[IRTypeId],
    ) -> Vec<u32> {
        let state_width = state_slot_types.len();
        let mut next_state: Vec<Option<u32>> = vec![None; state_width];

        for (arg_idx, arg) in args.iter().enumerate() {
            if arg_idx >= target_slot_map.len() {
                break;
            }
            let (slot_start, slot_count) = target_slot_map[arg_idx];
            // Check block_var_to_bits to determine if the arg is Block-typed.
            // Using slot_count > 1 alone is wrong when pc_width == 1 (Block
            // also expands to exactly 1 slot in that case).
            if let Some(entry) = self.block_var_to_bits.get(&arg.0) {
                // Block-typed arg — scatter its Bit encoding across the slots.
                let bits = entry.0.clone();
                for (j, &bit) in bits.iter().enumerate() {
                    if slot_start + j < state_width {
                        next_state[slot_start + j] = Some(bit);
                    }
                }
            } else if slot_start < state_width {
                next_state[slot_start] = Some(block_vals[arg.0 as usize]);
            }
        }

        // Fill unset slots by passing the combined block's own *current*
        // incoming value straight through, instead of zeroing them.
        //
        // A block that jumps to a target not covering slot `k` isn't
        // declaring "slot k is now zero" -- it simply doesn't touch slot
        // `k`, the same "doesn't own this slot" principle already used for
        // arity/type-mismatched params elsewhere in this file. Zero-filling
        // here was silently wiping any state a block doesn't explicitly
        // thread through on every hop between the block that last wrote it
        // and the block that next reads it -- fine for a single, linear
        // chain of blocks (never previously exercised any other shape), but
        // wrong the moment a real program's control flow has *any* block
        // along that chain that doesn't itself care about slot `k` (e.g. a
        // register-dispatch diamond's own trampoline blocks), which
        // shows up as loop-carried state (like a program's own `halted`
        // exit flag) reverting to zero the very next step after being set.
        // Slot `k` is the combined block's own param `pc_width + k`
        // (`combined_param_types` is `[Bit×pc_width] ++ state_slot_types`,
        // and `IrCtx::new`'s own `first_id` starts right after those
        // params), so this is just that var id, not a fresh computation.
        let pc_width = self.pc_width as u32;
        next_state
            .into_iter()
            .enumerate()
            .map(|(k, v)| v.unwrap_or(pc_width + k as u32))
            .collect()
    }

    /// Build the `target_slot_map` for a Dyn jump target whose expected
    /// parameter types come from a `Block { params: sig }` type.
    fn slot_map_from_sig(&self, sig: &[IRTypeId]) -> Vec<(usize, usize)> {
        self.slot_map_for(None, sig)
    }

    /// Emit a stmt and record its result type.
    ///
    /// Clones `self.pending_prov` without resetting it, so subsequent synthetic
    /// stmts inherit the last staged source provenance rather than a default.
    /// Source stmts set `pending_prov` via `emit_block_stmts` before calling here.
    fn push_typed(&mut self, stmt: IRStmt, result_type: IRTypeId) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(volar_ir_common::Node::new(stmt, self.pending_prov.clone(), None));
        self.var_types.push(result_type);
        id
    }

    /// Emit a `Poly` with the given coefficients and constant, inferring the
    /// result type from the operand types tracked in `self.var_types`.
    fn emit_poly(&mut self, coeffs: BTreeMap<Vec<IRVarId>, u8>, constant_lo: u128) -> u32 {
        let result_type = infer_poly_result_type(
            &coeffs,
            &self.var_types,
            &self.ir_types,
            &self.bit_type_id,
        );
        self.push_typed(
            IRStmt::Poly { ty: result_type, coeffs, constant: Constant { hi: 0, lo: constant_lo } },
            result_type,
        )
    }

    /// Extract bit `bit_j` of `src` as a fresh Bit-typed var.
    fn emit_shuffle_bit(&mut self, src: u32, bit_j: u8) -> u32 {
        let bt = self.bit_type_id;
        self.push_typed(
            IRStmt::Shuffle { result_bits: vec![(bit_j, IRVarId(src))], ty: bt },
            bt,
        )
    }

    /// Emit `val XOR const_k` (field addition in GF(2^n)) using `Poly`.
    fn emit_poly_xor_const(&mut self, val: u32, const_k: Constant, ty: IRTypeId) -> u32 {
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![IRVarId(val)], 1u8);
        self.push_typed(IRStmt::Poly { ty, coeffs, constant: const_k }, ty)
    }

    /// Emit a Bit var that is `1` iff `val == const_k`.
    ///
    /// For `Bit`-typed `val`: direct identity or NOT.
    /// For wider types: compute `diff = val XOR const_k` via `Poly`, then AND
    /// the NOT of each bit of `diff` (checking all bits are zero).
    fn emit_eq_const_ir(&mut self, val: u32, const_k: Constant, idx_ty: IRTypeId) -> u32 {
        if matches!(self.ir_types[idx_ty.0 as usize], IRType::Primitive(Type::Bit)) {
            return if const_k.lo == 0 { self.emit_not(val) } else { val };
        }
        let diff = self.emit_poly_xor_const(val, const_k, idx_ty);
        let check_width = bit_width_for_eq(&self.ir_types, idx_ty);
        let mut is_zero = self.emit_one_bit();
        for j in 0..check_width as u8 {
            let bit_j = self.emit_shuffle_bit(diff, j);
            let not_j = self.emit_not(bit_j);
            is_zero   = self.emit_and_bit(is_zero, not_j);
        }
        is_zero
    }

    /// Decompose a single `IRBlockTargetId + args` for the dispatch table.
    ///
    /// `blocks` is needed for static `Block(j)` targets to look up the target
    /// block's param layout, so that `Block`-typed args are scattered into the
    /// correct expanded state slots.
    fn process_ir_target(
        &mut self,
        target_block: &IRBlockTargetId,
        args: &[IRVarId],
        block_vals: &[u32],
        pc_width: usize,
        state_slot_types: &[IRTypeId],
        return_slot_types: &[IRTypeId],
        blocks: &IRBlocks<P>,
    ) -> (u32, Vec<u32>, Vec<u32>, Vec<u32>) {
        let ret_width = return_slot_types.len();
        match target_block {
            IRBlockTargetId::Return => {
                let done = self.emit_one_bit();
                let next_pc = self.encode_pc_bits(0, pc_width);
                let next_state = state_slot_types
                    .iter()
                    .map(|ty| {
                        let t = ty.clone();
                        self.emit_zero_slot(&t)
                    })
                    .collect();
                // Expand any Block-typed return args into their Bit encoding.
                let mut ret: Vec<u32> = Vec::new();
                for arg in args {
                    if let Some(entry) = self.block_var_to_bits.get(&arg.0) {
                        let bits = entry.0.clone();
                        ret.extend_from_slice(&bits);
                    } else {
                        ret.push(block_vals[arg.0 as usize]);
                    }
                }
                // DIAG: this zero-pad loop assumes every Return-bearing
                // block's own arg shape matches `return_slot_types` (taken
                // from whichever Return-bearing block `compute_return_slot_types`
                // happens to see first) -- had no assertion backing that
                // assumption. Panic if it's ever actually exercised (short)
                // or silently overflowed (long), to find out.
                assert!(
                    ret.len() <= ret_width,
                    "DIAG: movfuscate_ir return-slot overflow -- this block's own \
                     Return args expand to {} slots, wider than the global \
                     ret_width={} (taken from a different Return-bearing block)",
                    ret.len(), ret_width,
                );
                assert_eq!(
                    ret.len(), ret_width,
                    "DIAG: movfuscate_ir return-slot underflow -- this block's own \
                     Return args expand to {} slots, narrower than the global \
                     ret_width={} (the zero-pad loop would have silently fired here)",
                    ret.len(), ret_width,
                );
                for m in ret.len()..ret_width {
                    let ty = return_slot_types[m].clone();
                    ret.push(self.emit_zero_slot(&ty));
                }
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Block(IRBlockId(j)) => {
                let done = self.emit_zero_bit();
                let next_pc = self.encode_pc_bits(*j as usize, pc_width);
                // Use the target block's expanded param layout to scatter args
                // into the correct global state slots (Block args → Bit slots).
                let target_slot_map =
                    self.slot_map_for(Some(*j as usize), &blocks.blocks[*j as usize].params);
                let next_state = self.scatter_args_to_state(
                    args, block_vals, &target_slot_map, state_slot_types,
                );
                let ret = return_slot_types
                    .iter()
                    .map(|ty| {
                        let t = ty.clone();
                        self.emit_zero_slot(&t)
                    })
                    .collect();
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Dyn(IRVarId(v)) => {
                // The var `v` must be a Block-typed var tracked in
                // block_var_to_bits.  Its Bit encoding becomes the next PC.
                let (next_pc_bits, sig) = {
                    let entry = self.block_var_to_bits.get(v).unwrap_or_else(|| {
                        panic!(
                            "movfuscate_ir: Dyn target var {} is not a tracked \
                             IRType::Block var; only Block-typed vars may be \
                             used as Dyn jump targets",
                            v
                        )
                    });
                    (entry.0.clone(), entry.1.clone())
                };
                let done = self.emit_zero_bit();
                // The Block type’s `params` signature gives the expected
                // interface of the target, used to scatter call arguments.
                let target_slot_map = self.slot_map_from_sig(&sig);
                let next_state = self.scatter_args_to_state(
                    args, block_vals, &target_slot_map, state_slot_types,
                );
                let ret = return_slot_types
                    .iter()
                    .map(|ty| {
                        let t = ty.clone();
                        self.emit_zero_slot(&t)
                    })
                    .collect();
                (done, next_pc_bits, next_state, ret)
            }
            _ => panic!("process_ir_target: unhandled IRBlockTargetId variant — add handling for this variant"),
        }
    }
}

impl<P: Clone> MovfuscCtx for IrCtx<P> {
    type Blocks = IRBlocks<P>;
    type SlotTy = IRTypeId;

    fn num_blocks(blocks: &IRBlocks<P>) -> usize {
        blocks.blocks.len()
    }

    fn block_param_count(blocks: &IRBlocks<P>, i: usize) -> usize {
        blocks.blocks[i].params.len()
    }

    fn stmt_position(&self) -> u32 {
        self.next_id
    }

    fn return_val_width(blocks: &IRBlocks<P>) -> usize {
        for block in &blocks.blocks {
            match &block.terminator {
                IRTerminator::Jmp { target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. } } => {
                    return args.len()
                }
                IRTerminator::JumpCond {
                    then_target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. },
                    ..
                } => return args.len(),
                IRTerminator::JumpCond {
                    else_target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. },
                    ..
                } => return args.len(),
                _ => {}
            }
        }
        0
    }

    // Bit ops (all produce Bit-typed vars) -----------------------------------

    fn emit_zero_bit(&mut self) -> u32 {
        let bt = self.bit_type_id.clone();
        self.push_typed(IRStmt::Const(Constant { hi: 0, lo: 0 }, bt.clone()), bt)
    }

    fn emit_one_bit(&mut self) -> u32 {
        let bt = self.bit_type_id.clone();
        self.push_typed(IRStmt::Const(Constant { hi: 0, lo: 1 }, bt.clone()), bt)
    }

    /// `a AND b` = `Poly { {[a,b]: 1u8}, constant: 0 }`.
    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a; // idempotent
        }
        let mut key = vec![IRVarId(a), IRVarId(b)];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        self.emit_poly(coeffs, 0)
    }

    /// `a XOR b` = `Poly { {[a]: 1u8, [b]: 1u8}, constant: 0 }`.
    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return self.emit_zero_bit();
        }
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        coeffs.insert(vec![IRVarId(b)], 1);
        self.emit_poly(coeffs, 0)
    }

    /// `NOT a` = `Poly { {[a]: 1u8}, constant: 1 }` (i.e. `1 + a` in GF(2)).
    fn emit_not(&mut self, a: u32) -> u32 {
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        self.emit_poly(coeffs, 1)
    }

    // Typed slot ops ---------------------------------------------------------

    /// Zero of type `ty`.
    fn emit_zero_slot(&mut self, ty: &IRTypeId) -> u32 {
        let t = ty.clone();
        self.push_typed(IRStmt::Const(Constant { hi: 0, lo: 0 }, t.clone()), t)
    }

    /// `is_active · val` — scalar multiplication of `val: ty` by a Bit.
    ///
    /// Represented as `Poly { {[is_active, val]: 1u8}, constant: 0 }`.
    /// Result type = `ty` (the wider field absorbs the embedded Bit).
    fn emit_gate(&mut self, is_active: u32, val: u32, ty: &IRTypeId) -> u32 {
        // Short-circuit: AND(x, x) = x  (idempotent; both must be same var)
        if is_active == val {
            return is_active;
        }
        // Canonical order for the monomial key.
        let mut key = vec![IRVarId(is_active), IRVarId(val)];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        let t = ty.clone();
        self.push_typed(
            IRStmt::Poly { ty: t, coeffs, constant: Constant { hi: 0, lo: 0 } },
            t,
        )
    }

    /// `a + b` — field addition, both operands have type `ty`.
    ///
    /// Represented as `Poly { {[a]: 1u8, [b]: 1u8}, constant: 0 }`.
    fn emit_field_add(&mut self, a: u32, b: u32, ty: &IRTypeId) -> u32 {
        if a == b {
            return self.emit_zero_slot(ty); // a + a = 0 in characteristic-2 fields
        }
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        coeffs.insert(vec![IRVarId(b)], 1);
        let t = ty.clone();
        self.push_typed(
            IRStmt::Poly { ty: t, coeffs, constant: Constant { hi: 0, lo: 0 } },
            t,
        )
    }

    // Block processing -------------------------------------------------------

    fn emit_block_stmts(
        &mut self,
        blocks: &IRBlocks<P>,
        block_idx: usize,
        state_vars: &[u32],
        is_active: u32,
    ) -> Vec<u32> {
        let block = &blocks.blocks[block_idx];
        let p = block.params.len();
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());

        // Reset per-block Block-var tracking.
        self.block_var_to_bits.clear();

        // Map each original param to its combined-block state var(s).
        // Block-typed params expand to `pc_width` consecutive Bit state vars;
        // all other params are 1-to-1.
        let slot_map = self.slot_map_for(Some(block_idx), &block.params);
        for (k, (slot_start, slot_count)) in slot_map.iter().enumerate() {
            let is_block =
                matches!(self.ir_types[block.params[k].0 as usize], IRType::Block { .. });
            if is_block {
                let bits: Vec<u32> =
                    state_vars[*slot_start..*slot_start + *slot_count].to_vec();
                let sig = match &self.ir_types[block.params[k].0 as usize] {
                    IRType::Block { params } => params.clone(),
                    _ => unreachable!(),
                };
                self.block_var_to_bits.insert(k as u32, (bits.clone(), sig));
                // Use the first Bit var as the representative in var_map.
                // Block vars must only appear in Dyn terminators or Block-typed
                // jump args; the representative is never used in field stmts.
                var_map.push(bits[0]);
            } else {
                var_map.push(state_vars[*slot_start]);
            }
        }

        // Emit stmts with substitution, handling Block-typed Const specially.
        for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
            // Stage this stmt's source provenance; `push_typed` will clone it.
            self.pending_prov = stmt.prov.clone();
            let mapped = subst_ir(&stmt.kind, &var_map);
            let orig_var_id = (p + stmt_idx) as u32;

            // Block-typed Const: encode the referenced block index as
            // `pc_width` Bit constants.  We skip emitting the Const stmt
            // itself so the combined block stays free of Block-typed vars.
            if let IRStmt::Const(ref cnst, ref ty_id) = mapped {
                let ty_idx = ty_id.0 as usize;
                if matches!(self.ir_types[ty_idx], IRType::Block { .. }) {
                    let block_ref = cnst.lo as usize;
                    let bits: Vec<u32> = (0..self.pc_width)
                        .map(|j| {
                            if (block_ref >> j) & 1 == 1 {
                                self.emit_one_bit()
                            } else {
                                self.emit_zero_bit()
                            }
                        })
                        .collect();
                    let sig = match &self.ir_types[ty_idx] {
                        IRType::Block { params } => params.clone(),
                        _ => unreachable!(),
                    };
                    let rep = if !bits.is_empty() { bits[0] } else { self.emit_zero_bit() };
                    self.block_var_to_bits.insert(orig_var_id, (bits, sig));
                    var_map.push(rep);
                    continue;
                }
            }

            // ---- StorageWrite with a Block-typed source --------------------
            // The source var lives in `block_var_to_bits`; merge its PC bits
            // into a Vec(pc_width, Bit) and write to storage ID 2n (even).
            // Non-Block StorageWrite remaps to storage ID 2n+1 (odd).
            if let IRStmt::StorageWrite { storage, src, ty, addr } = &mapped {
                // `src` is the post-substitution combined-block var ID.
                // `block_var_to_bits` is keyed by *original* IR var IDs, so
                // we must look up via the pre-substitution src from `stmt`.
                let orig_src_id = match &stmt.kind {
                    IRStmt::StorageWrite { src, .. } => src.0,
                    _ => unreachable!(),
                };
                if let Some((bits, _sig)) = self.block_var_to_bits.get(&orig_src_id).cloned() {
                    // Block-typed write: merge PC bits into Vec and store in even lane.
                    let parts: Vec<IRVarId> = bits.iter().map(|&b| IRVarId(b)).collect();
                    let vec_pc_ty = self.vec_pc_type_id;
                    let merged = self.push_typed(
                        IRStmt::Merge { parts, ty: vec_pc_ty },
                        vec_pc_ty,
                    );
                    let new_storage = StorageId(storage.0 * 2);
                    let write_addr = *addr;
                    // Gate the write's own effective value on `is_active`:
                    // mid-block statements (including this one) run on
                    // EVERY combined-block call regardless of whether
                    // block_idx is the block actually dispatched this step
                    // (movfuscation only gates the four terminator outputs,
                    // not side-effecting statements — see this trait
                    // method's own doc comment). Without this, every
                    // inactive block's own stale recomputation silently
                    // overwrites storage on every step it isn't active.
                    let current = self.push_typed(
                        IRStmt::StorageRead { storage: new_storage, ty: vec_pc_ty, addr: write_addr },
                        vec_pc_ty,
                    );
                    let gated_src = self.emit_select_slot(is_active, merged, current, &vec_pc_ty);
                    let id = self.push_typed(
                        IRStmt::StorageWrite { storage: new_storage, src: IRVarId(gated_src), ty: vec_pc_ty, addr: write_addr },
                        self.bit_type_id,
                    );
                    var_map.push(id);
                    continue;
                } else {
                    // Non-block write: remap to odd storage lane.
                    let new_storage = StorageId(storage.0 * 2 + 1);
                    let res_ty = infer_stmt_result_type(&mapped, &self.var_types, &self.ir_types, &self.bit_type_id);
                    let write_ty = *ty;
                    let write_addr = *addr;
                    let write_src = *src;
                    // Same is_active gating as the Block-typed arm above.
                    let current = self.push_typed(
                        IRStmt::StorageRead { storage: new_storage, ty: write_ty, addr: write_addr },
                        write_ty,
                    );
                    let gated_src = self.emit_select_slot(is_active, write_src.0, current, &write_ty);
                    let id = self.push_typed(
                        IRStmt::StorageWrite { storage: new_storage, src: IRVarId(gated_src), ty: write_ty, addr: write_addr },
                        res_ty,
                    );
                    var_map.push(id);
                    continue;
                }
            }

            // ---- StorageRead: if result type is Block, decompose Vec into PC bits.
            // Block-typed reads come from even storage IDs; others from odd.
            if let IRStmt::StorageRead { storage, ty, addr } = &mapped {
                let ty_idx = ty.0 as usize;
                if matches!(self.ir_types[ty_idx], IRType::Block { .. }) {
                    // Read Vec(pc_width, Bit) from even lane.
                    let new_storage = StorageId(storage.0 * 2);
                    let merged = self.push_typed(
                        IRStmt::StorageRead { storage: new_storage, ty: self.vec_pc_type_id, addr: *addr },
                        self.vec_pc_type_id,
                    );
                    // Decompose Vec back into individual Bit vars via Shuffle.
                    let bits: Vec<u32> = (0..self.pc_width)
                        .map(|j| {
                            let shuffled = self.push_typed(
                                IRStmt::Shuffle {
                                    result_bits: vec![(j as u8, IRVarId(merged))],
                                    ty: self.bit_type_id,
                                },
                                self.bit_type_id,
                            );
                            shuffled
                        })
                        .collect();
                    let sig = match &self.ir_types[ty_idx] {
                        IRType::Block { params } => params.clone(),
                        _ => unreachable!(),
                    };
                    let rep = if !bits.is_empty() { bits[0] } else { self.emit_zero_bit() };
                    self.block_var_to_bits.insert(orig_var_id, (bits, sig));
                    var_map.push(rep);
                    continue;
                } else {
                    // Non-block read: remap to odd storage lane.
                    let new_storage = StorageId(storage.0 * 2 + 1);
                    let res_ty = infer_stmt_result_type(&mapped, &self.var_types, &self.ir_types, &self.bit_type_id);
                    let id = self.push_typed(
                        IRStmt::StorageRead { storage: new_storage, ty: *ty, addr: *addr },
                        res_ty,
                    );
                    var_map.push(id);
                    continue;
                }
            }

            // Ordinary stmt — infer type and emit.
            let ty = infer_stmt_result_type(
                &mapped,
                &self.var_types,
                &self.ir_types,
                &self.bit_type_id,
            );
            let id = self.push_typed(mapped, ty);
            var_map.push(id);
        }
        var_map
    }

    fn emit_block_terminator(
        &mut self,
        blocks: &IRBlocks<P>,
        block_idx: usize,
        block_vals: &[u32],
        pc_width: usize,
        state_slot_types: &[IRTypeId],
        return_slot_types: &[IRTypeId],
    ) -> TermResult {
        let block = &blocks.blocks[block_idx];
        let state_width = state_slot_types.len();
        let ret_width = return_slot_types.len();

        match &block.terminator {
            IRTerminator::Jmp { target } => {
                let (done, next_pc_bits, next_state, ret_vals) = self.process_ir_target(
                    &target.dest,
                    &target.args,
                    block_vals,
                    pc_width,
                    state_slot_types,
                    return_slot_types,
                    blocks,
                );
                TermResult { done, next_pc_bits, next_state, ret_vals }
            }
            IRTerminator::JumpCond {
                condition,
                then_target,
                else_target,
            } => {
                let cond = block_vals[condition.0 as usize];
                let (t_done, t_npc, t_ns, t_ret) = self.process_ir_target(
                    &then_target.dest,
                    &then_target.args,
                    block_vals,
                    pc_width,
                    state_slot_types,
                    return_slot_types,
                    blocks,
                );
                let (e_done, e_npc, e_ns, e_ret) = self.process_ir_target(
                    &else_target.dest,
                    &else_target.args,
                    block_vals,
                    pc_width,
                    state_slot_types,
                    return_slot_types,
                    blocks,
                );
                // Bit-select for done and PC.
                let done = self.emit_select_bit(cond, t_done, e_done);
                let next_pc_bits = (0..pc_width)
                    .map(|j| self.emit_select_bit(cond, t_npc[j], e_npc[j]))
                    .collect();
                // Typed (field) select for state and return.
                let next_state = (0..state_width)
                    .map(|k| {
                        self.emit_select_slot(cond, t_ns[k], e_ns[k], &state_slot_types[k])
                    })
                    .collect();
                let ret_vals = (0..ret_width)
                    .map(|m| {
                        self.emit_select_slot(cond, t_ret[m], e_ret[m], &return_slot_types[m])
                    })
                    .collect();
                TermResult { done, next_pc_bits, next_state, ret_vals }
            }
            IRTerminator::JumpTable { index, cases } => {
                let idx_val = block_vals[index.0 as usize];
                let idx_ty  = self.var_types[idx_val as usize];
                let state_width = state_slot_types.len();
                let ret_width   = return_slot_types.len();

                let mut done_acc = self.emit_zero_bit();
                let mut npc_acc: Vec<u32> =
                    (0..pc_width).map(|_| self.emit_zero_bit()).collect();
                let mut ns_acc: Vec<u32> = state_slot_types
                    .iter()
                    .map(|ty| self.emit_zero_slot(ty))
                    .collect();
                let mut ret_acc: Vec<u32> = return_slot_types
                    .iter()
                    .map(|ty| self.emit_zero_slot(ty))
                    .collect();

                for (const_k, branch_target) in cases {
                    let is_case = self.emit_eq_const_ir(idx_val, *const_k, idx_ty);
                    let (done_k, npc_k, ns_k, ret_k) = self.process_ir_target(
                        &branch_target.dest,
                        &branch_target.args,
                        block_vals,
                        pc_width,
                        state_slot_types,
                        return_slot_types,
                        blocks,
                    );
                    let g = self.emit_and_bit(is_case, done_k);
                    done_acc = self.emit_xor_bit(done_acc, g);
                    for j in 0..pc_width {
                        let g = self.emit_and_bit(is_case, npc_k[j]);
                        npc_acc[j] = self.emit_xor_bit(npc_acc[j], g);
                    }
                    for k in 0..state_width {
                        let g = self.emit_gate(is_case, ns_k[k], &state_slot_types[k]);
                        ns_acc[k] = self.emit_field_add(ns_acc[k], g, &state_slot_types[k]);
                    }
                    for m in 0..ret_width {
                        let g = self.emit_gate(is_case, ret_k[m], &return_slot_types[m]);
                        ret_acc[m] = self.emit_field_add(ret_acc[m], g, &return_slot_types[m]);
                    }
                }

                TermResult {
                    done: done_acc,
                    next_pc_bits: npc_acc,
                    next_state: ns_acc,
                    ret_vals: ret_acc,
                }
            }
            _ => panic!("movfuscate: unhandled IRTerminator variant — add handling for this variant"),
        }
    }

    fn build_output(
        self,
        _combined_params: usize,
        done_var: u32,
        loop_vars: Vec<u32>,
        ret_vars: Vec<u32>,
    ) -> IRBlocks<P> {
        IRBlocks::new(vec![IRBlock {
            // Combined block's params: [Bit×pc_width, state_slot_types…]
            params: self.combined_param_types,
            stmts: self.stmts,
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(done_var),
                then_target: IRBranchTarget::new(
                    IRBlockTargetId::Return,
                    ret_vars.into_iter().map(IRVarId).collect(),
                ),
                else_target: IRBranchTarget::new(
                    IRBlockTargetId::Block(IRBlockId(0)),
                    loop_vars.into_iter().map(IRVarId).collect(),
                ),
            },
        }])
    }
}

// ============================================================================
// Pre-pass helpers for IRBlocks entry point
// ============================================================================

/// What distinguishes one position's expansion from another's, for the
/// purposes of deciding whether two blocks' params at the same raw position
/// `k` can share one combined-block slot (sub-)group or need separate ones.
///
/// `Block`-typed params always expand identically regardless of their own
/// `params` signature (the signature only matters for `Dyn`-target
/// argument scattering, handled separately via `block_var_to_bits`), so
/// `Block` itself carries no further payload here. At `pc_width == 1`, a
/// `Block`-typed param and a plain `Bit`-typed param both expand to exactly
/// one `Bit` slot and are historically treated as interchangeable at a
/// shared position (`compute_position_groups` folds this case into one
/// `Scalar(bit_type_id)` signature, not two).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum SlotSig {
    Block,
    Scalar(u32),
}

impl SlotSig {
    fn of(ty_id: &IRTypeId, ir_types: &[IRType], pc_width: usize, bit_type_id: &IRTypeId) -> Self {
        if matches!(ir_types[ty_id.0 as usize], IRType::Block { .. }) {
            if pc_width == 1 {
                SlotSig::Scalar(bit_type_id.0)
            } else {
                SlotSig::Block
            }
        } else {
            SlotSig::Scalar(ty_id.0)
        }
    }

    fn slot_count(&self, pc_width: usize) -> usize {
        match self {
            SlotSig::Block => pc_width,
            SlotSig::Scalar(_) => 1,
        }
    }
}

/// Minimal union-find (disjoint-set) over `usize` node ids, path-compressed
/// on `find`. Used by [`compute_static_slot_classes`] to group `(block,
/// position)` param occurrences that provably carry the *same* logical
/// value, instead of ones that merely happen to share a raw structural
/// position (see that function's own doc comment for why the distinction
/// matters).
struct UnionFind {
    parent: Vec<u32>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind { parent: (0..n as u32).collect() }
    }

    fn find(&mut self, x: usize) -> usize {
        let mut root = x;
        while self.parent[root] as usize != root {
            root = self.parent[root] as usize;
        }
        let mut cur = x;
        while cur != root {
            let next = self.parent[cur] as usize;
            self.parent[cur] = root as u32;
            cur = next;
        }
        root
    }

    fn union(&mut self, a: usize, b: usize) {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra != rb {
            self.parent[ra] = rb as u32;
        }
    }
}

/// Resolve an `IRBlockTargetId::Dyn(v)` target to a concrete block index,
/// when `v` is provably a compile-time-constant block reference within
/// `block`'s own statements (an `IRStmt::Const` of `IRType::Block { .. }`
/// type). VAFFLE's own lowering (`lower_to_ir.rs`) always encodes call
/// -return continuations this way — syntactically a `Dyn` jump, but `v`
/// itself is always a literal reference to one specific, statically-known
/// block, never a genuinely runtime-varying value; movfuscate's own
/// emission (`emit_block_stmts`'s Block-typed-`Const` handling) already
/// relies on exactly this fact (`cnst.lo as usize` as the target block
/// index) to build `block_var_to_bits`. Returns `None` for a `v` that
/// isn't such a constant (e.g. a param, or itself the result of a real
/// runtime merge of multiple possible continuations) — genuinely
/// unresolvable, not just untried.
fn resolve_dyn_target<P: Clone>(block: &IRBlock<P>, v: IRVarId, n_blocks: usize) -> Option<usize> {
    let idx = v.0 as usize;
    if idx < block.params.len() {
        return None;
    }
    let stmt = block.stmts.get(idx - block.params.len())?;
    let IRStmt::Const(cnst, _ty) = &stmt.kind else { return None };
    let target = cnst.lo as usize;
    (target < n_blocks).then_some(target)
}

/// Sound, edge-aware replacement for [`compute_position_groups`]/
/// [`compute_expanded_state_slot_types`]'s `(position, type)`-keyed
/// dedup, which only agrees to share one physical slot when two blocks'
/// params happen to structurally coincide — completely unaware of whether
/// they're actually the *same logical value*. Two unrelated blocks (e.g.
/// two independent copies of the same generic "5-way register dispatch"
/// diamond chain, one for reading `rs1`, one for writing `rd` after an
/// unrelated opcode) can have same-typed params at the same raw position
/// purely by structural coincidence, and get silently aliased onto one
/// physical slot — a real value written by one gets clobbered by the
/// other's completely unrelated write the next time either fires. This
/// was the confirmed root cause of Milestone 1.6's "real interpreter
/// halts but computes the wrong answer" bug (see project memory).
///
/// This function instead grants a `(block, position)` param a *shared*
/// physical slot with another `(block', position')` only when there's a
/// concrete, provable dataflow edge connecting them: a jump's arg at that
/// position is a **literal pass-through** of the jumping block's own
/// incoming param (checked via `IRBlock::push_stmt`'s own numbering
/// convention — a block's params occupy var ids `0..params.len()`, so
/// `arg.0 < params.len()` means "this is one of my own incoming params,
/// unmodified", not a freshly computed value). Freshly computed values
/// simply seed a new slot at the target position; ordinary SSA merges
/// (some predecessors pass-through, others supply a fresh value) work
/// correctly as-is, since union-find just accumulates whichever edges
/// happen to alias. Under-merging (treating a same-value chain that isn't
/// a *literal* var-for-var pass-through as "fresh") is always safe — it
/// only costs an extra slot, never causes aliasing; the reverse
/// (over-merging) is what caused the bug, so this errs conservative.
///
/// A `Dyn(v)` target is traced exactly like a static `Block(j)` edge
/// whenever [`resolve_dyn_target`] can prove `v` is a literal constant
/// reference to block `j` (the overwhelmingly common case for VAFFLE
/// -lowered IR — see that function's own doc comment).
///
/// A *genuinely* unresolvable `Dyn` target (e.g. a continuation loaded
/// from the stack — real runtime polymorphism, not just an untraced
/// constant) is handled conservatively instead of aborting: `v`'s own
/// declared type (`IRType::Block { params: sig }`) is inferred, and the
/// edge is unioned against *every* block in the whole function whose own
/// param-type list equals `sig` — i.e. every block this Dyn value could
/// possibly resolve to at runtime, since its actual target isn't known
/// until PC dispatch. This is strictly more precise than the old
/// position-only `compute_position_groups` scheme (it's still real edge
/// tracing, just fanned out to every plausible landing site instead of
/// one certain one), and — same as the pass-through/fresh-value
/// distinction elsewhere in this function — errs toward *extra* slots
/// rather than ever aliasing two unrelated values.
/// Collect every one of `block`'s own params that `start` (a var id within
/// `block`) transitively data-depends on, into `found` (stops accumulating,
/// but still marks visited, once `found.len() > 1` — the caller only ever
/// cares whether the result is "exactly one", so further detail past
/// ambiguity is wasted work).
///
/// A literal reference (`start` itself is a param) is the trivial base case
/// (subsumes the old `arg.0 < params.len()` check, which only handled this
/// exact case). Otherwise, walks `start`'s own defining statement's
/// operands (via `Stmt::map_var`, so every current and future `IRStmt`
/// variant is covered without a hand-maintained match) and recurses.
/// `visited` memoizes per-var results within one query so a wide fan-in
/// (e.g. a 64-part `Merge`) doesn't revisit shared sub-expressions — bounds
/// total work to `O(block.stmts)` per query regardless of the block's own
/// internal DAG shape.
///
/// This is the generalization that makes `x1 = x1 + 1` (a `Poly`, not a
/// literal var reference) recognized as "the same value, updated", and a
/// same-block *argument swap* (`Jmp` args `[p1, p0]` instead of `[p0,
/// p1]`) recognized as "target position 0 is source's own position 1,
/// unchanged" — real Volar IR practically never threads a value across a
/// block boundary as a bare literal param reference at the *same* index
/// (WAFFLE materializes even unchanged pass-throughs as their own
/// statement), so the old literal-only, same-index-only check found
/// ~zero pass-throughs on real circuits. A value that depends on *zero*
/// params (e.g. a `Const` placeholder for a position this edge doesn't
/// care about) or *multiple* params (a genuine combination, e.g. `x1 +
/// x5`) is correctly left unlinked either way — only an unambiguous
/// single-param dependency establishes "same identity".
fn reachable_params<P: Clone>(
    block: &IRBlock<P>,
    start: IRVarId,
    visited: &mut BTreeSet<u32>,
    found: &mut BTreeSet<u32>,
) {
    if (start.0 as usize) < block.params.len() {
        found.insert(start.0);
        return;
    }
    if found.len() > 1 || !visited.insert(start.0) {
        return;
    }
    let Some(stmt) = block.stmts.get(start.0 as usize - block.params.len()) else { return };
    let _ = stmt.kind.clone().map_var(
        &mut (),
        &mut |_ctx: &mut (), v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
            reachable_params(block, v, visited, found);
            Ok(v)
        },
        &mut |_ctx: &mut (), ty: IRTypeId| -> Result<IRTypeId, core::convert::Infallible> { Ok(ty) },
        &mut |_ctx: &mut (), s: StorageId| -> Result<StorageId, core::convert::Infallible> { Ok(s) },
    );
}

fn compute_static_slot_classes<P: Clone>(
    blocks: &IRBlocks<P>,
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
    pc_width: usize,
) -> (Vec<IRTypeId>, Vec<Vec<(usize, usize)>>, BTreeMap<Vec<u32>, Vec<(usize, usize)>>) {
    let n = blocks.blocks.len();
    let mut node_base = vec![0usize; n];
    let mut total_nodes = 0usize;
    for (i, b) in blocks.blocks.iter().enumerate() {
        node_base[i] = total_nodes;
        total_nodes += b.params.len();
    }
    let node_id = |block_idx: usize, position: usize| node_base[block_idx] + position;
    let mut uf = UnionFind::new(total_nodes);

    // Every block's own param-type signature, keyed by content, for the
    // unresolvable-Dyn fallback below. `IRTypeId` doesn't implement `Ord`
    // and this is a tiny, once-per-function table, so a linear scan per
    // lookup is fine — avoids a bespoke `Ord`/hash impl just for this.
    let block_sig = |i: usize| -> &[IRTypeId] { &blocks.blocks[i].params };
    let same_sig = |a: &[IRTypeId], b: &[IRTypeId]| a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.0 == y.0);

    let union_with_target = |uf: &mut UnionFind, src_block: usize, tgt: usize, args: &[IRVarId]| {
        let tgt_params = blocks.blocks[tgt].params.len();
        for (idx, arg) in args.iter().enumerate() {
            if idx >= tgt_params {
                break;
            }
            // Which (if any) single one of src_block's own params does
            // this arg unambiguously trace back to? Zero (a fresh/
            // independent value, e.g. a `Const` placeholder) or multiple
            // (a genuine combination, e.g. `x1 + x5`) both correctly stay
            // unlinked -- only "exactly one" establishes "same identity,
            // possibly updated, possibly reordered".
            let mut visited = BTreeSet::new();
            let mut found = BTreeSet::new();
            reachable_params(&blocks.blocks[src_block], *arg, &mut visited, &mut found);
            if found.len() == 1 {
                let p = *found.iter().next().unwrap() as usize;
                // The same source param can be the sole traced dependency
                // for more than one arg position in the same edge (e.g.
                // broadcast to two differently-typed targets) -- only
                // union when the types actually agree, else this is not
                // really "the same slot", just a coincidental data
                // dependency; leave it unlinked (a fresh slot) rather
                // than forcing a type-conflicting merge.
                if blocks.blocks[src_block].params[p].0 == blocks.blocks[tgt].params[idx].0 {
                    uf.union(node_id(src_block, p), node_id(tgt, idx));
                }
            }
        }
    };

    let visit_edge = |uf: &mut UnionFind, src_block: usize, dest: &IRBlockTargetId, args: &[IRVarId]| {
        match dest {
            IRBlockTargetId::Block(IRBlockId(j)) => union_with_target(uf, src_block, *j as usize, args),
            IRBlockTargetId::Dyn(v) => {
                if let Some(j) = resolve_dyn_target(&blocks.blocks[src_block], *v, n) {
                    union_with_target(uf, src_block, j, args);
                } else {
                    let var_types = infer_block_var_types(&blocks.blocks[src_block], ir_types, bit_type_id);
                    if let Some(ty) = var_types.get(v.0 as usize) {
                        if let IRType::Block { params: sig } = &ir_types[ty.0 as usize] {
                            for tgt in 0..n {
                                if same_sig(block_sig(tgt), sig) {
                                    union_with_target(uf, src_block, tgt, args);
                                }
                            }
                        }
                    }
                }
            }
            IRBlockTargetId::Return => {}
            _ => {}
        }
    };

    for (i, block) in blocks.blocks.iter().enumerate() {
        match &block.terminator {
            IRTerminator::Jmp { target } => visit_edge(&mut uf, i, &target.dest, &target.args),
            IRTerminator::JumpCond { then_target, else_target, .. } => {
                visit_edge(&mut uf, i, &then_target.dest, &then_target.args);
                visit_edge(&mut uf, i, &else_target.dest, &else_target.args);
            }
            IRTerminator::JumpTable { cases, .. } => {
                for target in cases.values() {
                    visit_edge(&mut uf, i, &target.dest, &target.args);
                }
            }
            _ => {}
        }
    }

    // Assign physical slots: walk (block, position) in a fixed,
    // deterministic order (block index, then param index); the first time
    // a given union-find root is seen, hand it the next available offset.
    let mut root_to_slot: BTreeMap<usize, (SlotSig, usize, usize)> = BTreeMap::new();
    let mut state_slot_types: Vec<IRTypeId> = Vec::new();
    let mut next_offset = 0usize;
    let mut slot_of: Vec<Vec<(usize, usize)>> = Vec::with_capacity(n);
    for (i, block) in blocks.blocks.iter().enumerate() {
        let mut this_block_slots = Vec::with_capacity(block.params.len());
        for (p, ty_id) in block.params.iter().enumerate() {
            let root = uf.find(node_id(i, p));
            let sig = SlotSig::of(ty_id, ir_types, pc_width, bit_type_id);
            let (offset, count) = match root_to_slot.get(&root) {
                Some(&(existing_sig, offset, count)) => {
                    assert_eq!(
                        existing_sig, sig,
                        "movfuscate_ir: DIAG same-value slot class disagrees on type at \
                         block {i} position {p} (existing={existing_sig:?} new={sig:?}) -- \
                         a literal var-for-var pass-through edge should always be well-typed; \
                         this indicates an ill-typed edge was traced as a pass-through",
                    );
                    (offset, count)
                }
                None => {
                    let count = sig.slot_count(pc_width);
                    let offset = next_offset;
                    next_offset += count;
                    match sig {
                        SlotSig::Block => {
                            for _ in 0..count {
                                state_slot_types.push(bit_type_id.clone());
                            }
                        }
                        SlotSig::Scalar(tid) => state_slot_types.push(IRTypeId(tid)),
                    }
                    root_to_slot.insert(root, (sig, offset, count));
                    (offset, count)
                }
            };
            this_block_slots.push((offset, count));
        }
        slot_of.push(this_block_slots);
    }

    // `Dyn` targets are traced above by resolved *target block index* (or
    // fanned out across every same-signature block when unresolvable), but
    // at actual emission time (`process_ir_target`'s own `Dyn` arm) only
    // the target's *type signature* is known -- the whole point of a `Dyn`
    // jump is that its concrete destination is chosen at runtime, encoded
    // as bits in the state vector itself, not statically known even when
    // `resolve_dyn_target` *could* prove one particular value of it.
    // Since every block sharing a given signature was guaranteed above to
    // end up with an identical slot layout (either because it's the one
    // resolved target of some edge, or because the signature-fallback
    // path unions *all* same-signature blocks together whenever any edge
    // needs it), picking any one representative block with that signature
    // is sound and sufficient for `slot_map_from_sig` to resolve by
    // signature alone.
    let mut sig_fallback: BTreeMap<Vec<u32>, Vec<(usize, usize)>> = BTreeMap::new();
    for (i, block) in blocks.blocks.iter().enumerate() {
        let key: Vec<u32> = block.params.iter().map(|t| t.0).collect();
        sig_fallback.entry(key).or_insert_with(|| slot_of[i].clone());
    }

    (state_slot_types, slot_of, sig_fallback)
}

/// Infer the expanded types of the return values by scanning the first
/// `Return` terminator found in the module.
///
/// `Block`-typed return values are expanded to `pc_width` `Bit` slots each.
fn compute_return_slot_types<P: Clone>(
    blocks: &IRBlocks<P>,
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
    pc_width: usize,
) -> Vec<IRTypeId> {
    for block in &blocks.blocks {
        let var_types = infer_block_var_types(block, ir_types, bit_type_id);
        let ret_args: Option<&Vec<IRVarId>> = match &block.terminator {
            IRTerminator::Jmp { target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. } } => Some(args),
            IRTerminator::JumpCond {
                then_target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. }, ..
            } => Some(args),
            IRTerminator::JumpCond {
                else_target: IRBranchTarget { dest: IRBlockTargetId::Return, args, .. }, ..
            } => Some(args),
            _ => None,
        };
        if let Some(args) = ret_args {
            let mut expanded = Vec::new();
            for id in args {
                let ty_id = &var_types[id.0 as usize];
                if matches!(ir_types[ty_id.0 as usize], IRType::Block { .. }) {
                    for _ in 0..pc_width {
                        expanded.push(bit_type_id.clone());
                    }
                } else {
                    expanded.push(ty_id.clone());
                }
            }
            return expanded;
        }
    }
    vec![]
}

// ============================================================================
// Public entry points
// ============================================================================

/// Movfuscate a `BIrBlocks` module into a single self-looping block.
///
/// All slot types are `Bit` (the only type in Boolar IR).
/// Single-block input is returned unchanged.
/// Source statement provenances are carried through. Infrastructure gates use
/// the first source-statement provenance in the circuit; use
/// [`movfuscate_biir_with_control_provenance`] when a valid circuit has no
/// statements and the caller has an explicit control provenance.
pub fn movfuscate_biir<P: Clone>(blocks: &BIrBlocks<P>) -> BIrBlocks<P> {
    movfuscate_biir_impl(blocks, None)
}

/// As [`movfuscate_biir`], but permits a statement-free multi-block circuit.
///
/// `control_prov` must name the enclosing frontend/control source responsible
/// for the circuit. It is used only when no input statement can supply the
/// provenance for generated dispatch gates; it is never synthesized here.
pub fn movfuscate_biir_with_control_provenance<P: Clone>(
    blocks: &BIrBlocks<P>,
    control_prov: &P,
) -> BIrBlocks<P> {
    movfuscate_biir_impl(blocks, Some(control_prov))
}

fn movfuscate_biir_impl<P: Clone>(blocks: &BIrBlocks<P>, control_prov: Option<&P>) -> BIrBlocks<P> {
    let n = blocks.blocks.len();
    if n == 1 { return blocks.clone(); }
    let pc_width = pc_bits_needed(n);
    let state_width = blocks.blocks.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let combined_params = pc_width + state_width;
    let ctrl_prov = blocks.blocks.iter().flat_map(|b| b.stmts.iter()).map(|n| &n.prov).next()
        .cloned()
        .or_else(|| control_prov.cloned())
        .expect("movfuscate_biir: circuit has no statements; supply explicit control provenance");
    let ctx = BIrCtx::<P>::new(combined_params as u32, ctrl_prov);
    let state_slot_types = vec![(); state_width];
    let ret_width = BIrCtx::<P>::return_val_width(blocks);
    let return_slot_types = vec![(); ret_width];
    let (mut result, _block_ranges, _accum_info, _watch) = movfuscate(ctx, blocks, state_slot_types, return_slot_types, &[]);
    result.pre_init = blocks.pre_init.clone();
    result
}

/// Movfuscate an `IRBlocks` module into a single self-looping block.
///
/// Supports any scalar `IRType` in block params: `Bit`, `Galois8AES`,
/// `Galois64`, and `Vec`.  All blocks sharing a state slot at position `k`
/// must agree on its type.  The caller supplies `[0; pc_width]` followed by
/// the original entry-block inputs when invoking the combined block.
///
/// `types` is used for type inference; an `IRType::Bit` entry is added if
/// absent.  Single-block input is returned unchanged.
pub fn movfuscate_ir<P: Clone>(blocks: &IRBlocks<P>, types: &mut IRTypes) -> IRBlocks<P> {
    movfuscate_ir_impl(blocks, types, &[], None).0
}

/// As [`movfuscate_ir`], but permits a statement-free multi-block circuit.
///
/// `control_prov` must identify the enclosing frontend/control source for
/// infrastructure statements when no input statement can provide it.
pub fn movfuscate_ir_with_control_provenance<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    control_prov: &P,
) -> IRBlocks<P> {
    movfuscate_ir_impl(blocks, types, &[], Some(control_prov)).0
}

/// As [`movfuscate_ir`], but additionally returns each original block's own
/// [`MovfuscBlockBoundary`] -- Milestone 1.5 Step B boundary metadata a
/// weaver can use to split the combined block's gates back out per
/// original block (e.g. one woven Rust function per range) instead of
/// weaving one function for the whole thing -- and the accumulation
/// phase's own [`MovfuscAccumInfo`], letting the combiner itself be
/// chunked into groups of blocks instead of needing every block's state
/// at once. Both empty (their `steps`) if `blocks` already had one block.
pub fn movfuscate_ir_with_boundary<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
) -> (IRBlocks<P>, Vec<MovfuscBlockBoundary>, MovfuscAccumInfo) {
    let (result, boundaries, accum_info, _watch) = movfuscate_ir_impl(blocks, types, &[], None);
    (result, boundaries, accum_info)
}

/// Like [`movfuscate_ir_with_boundary`], but also resolves each
/// `(orig_block_idx, orig_var_id)` pair in `watch` to its own combined-
/// circuit var id (see [`movfuscate`]'s own doc comment on `watch`).
/// Temporary diagnostic entry point: lets a caller trace an arbitrary
/// pre-movfuscation value's runtime bit values via `eval_ir_circuit_step`
/// without re-deriving where it landed in the combined block by hand.
pub fn movfuscate_ir_with_boundary_and_watch<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    watch: &[(usize, u32)],
) -> (IRBlocks<P>, Vec<MovfuscBlockBoundary>, MovfuscAccumInfo, Vec<(usize, u32, u32)>) {
    movfuscate_ir_impl(blocks, types, watch, None)
}

/// Diagnostic (temporary, not used by any real pipeline): dumps
/// `compute_static_slot_classes`'s own `slot_of[block_idx][position]`
/// layout plus `state_slot_types.len()`, so a caller in a different crate
/// can inspect the new edge-aware allocation directly without running a
/// full (slow) circuit evaluation.
/// Dump a single statement's shape by `(block, var)` -- `var` may be a
/// param (no stmt) or index into `stmts` (`var.0 - params.len()`).
pub fn debug_dump_stmt<P: Clone>(blocks: &IRBlocks<P>, block_idx: usize, var: IRVarId) -> alloc::string::String {
    let b = &blocks.blocks[block_idx];
    if (var.0 as usize) < b.params.len() {
        alloc::format!("var {} = param #{} (ty={:?})", var.0, var.0, b.params[var.0 as usize])
    } else {
        let si = var.0 as usize - b.params.len();
        alloc::format!("var {} = stmts[{si}] = {:?}", var.0, b.stmts[si].kind)
    }
}

pub fn debug_dump_slot_of<P: Clone>(blocks: &IRBlocks<P>, types: &IRTypes) -> alloc::string::String {
    use alloc::string::String;
    use core::fmt::Write;
    let bit_type_id = {
        let existing = types.0.iter().position(|t| matches!(t, IRType::Primitive(Type::Bit)));
        IRTypeId(existing.expect("Bit type must already be interned") as u32)
    };
    let pc_width = pc_bits_needed(blocks.blocks.len());
    let (state_slot_types, slot_of, _sig_fallback) =
        compute_static_slot_classes(blocks, &types.0, &bit_type_id, pc_width);
    let mut out = String::new();
    let _ = writeln!(out, "n_blocks={} pc_width={} state_width={}", blocks.blocks.len(), pc_width, state_slot_types.len());
    for (i, slots) in slot_of.iter().enumerate() {
        let b = &blocks.blocks[i];
        let _ = writeln!(out, "block {i}: params={:?} n_stmts={} slots={slots:?} term={:?}", b.params, b.stmts.len(), b.terminator);
    }
    out
}

fn movfuscate_ir_impl<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    watch: &[(usize, u32)],
    control_prov: Option<&P>,
) -> (IRBlocks<P>, Vec<MovfuscBlockBoundary>, MovfuscAccumInfo, Vec<(usize, u32, u32)>) {
    // Ensure IRType::Bit is present in the types table.
    let bit_type_id = types.intern(IRType::Primitive(Type::Bit));

    let n = blocks.blocks.len();
    if n == 1 {
        let empty_init = MovfuscAccumInit { start: 0, end: 0, done_acc: 0, next_pc: Vec::new(), next_state: Vec::new(), ret_vals: Vec::new() };
        return (blocks.clone(), Vec::new(), MovfuscAccumInfo { init: empty_init, steps: Vec::new() }, Vec::new());
    }

    // Intern Vec(pc_width, Bit) for block-reference storage.
    let pc_width = pc_bits_needed(n);
    let vec_pc_type_id = if pc_width > 0 {
        types.intern(IRType::Vec(pc_width, bit_type_id))
    } else {
        bit_type_id
    };

    let ir_types = types.0.clone();

    // Compute per-slot types from the original blocks, expanding Block
    // params, via the sound, edge-aware scheme -- including `Dyn` targets,
    // resolved either to one known constant block (VAFFLE's own call
    // -return continuations are always this shape) or, for a genuinely
    // unresolvable one, conservatively fanned out to every block sharing
    // its type signature (see `compute_static_slot_classes`'s own doc
    // comment).
    let (state_slot_types, slot_of, sig_fallback) = compute_static_slot_classes(blocks, &ir_types, &bit_type_id, pc_width);
    let slot_alloc = SlotAllocation::PerBlock { slot_of, sig_fallback };
    let return_slot_types =
        compute_return_slot_types(blocks, &ir_types, &bit_type_id, pc_width);

    // Combined block param types: [Bit×pc_width] ++ expanded state_slot_types
    let combined_param_types: Vec<IRTypeId> = (0..pc_width)
        .map(|_| bit_type_id.clone())
        .chain(state_slot_types.iter().cloned())
        .collect();

    let combined_params = pc_width + state_slot_types.len();
    let ctrl_prov = blocks.blocks.iter().flat_map(|b| b.stmts.iter()).map(|n| &n.prov).next()
        .cloned()
        .or_else(|| control_prov.cloned())
        .expect("movfuscate_ir: circuit has no statements; supply explicit control provenance");
    let ctx = IrCtx::<P>::new(
        combined_params as u32,
        bit_type_id,
        vec_pc_type_id,
        combined_param_types,
        ir_types,
        pc_width,
        ctrl_prov,
        slot_alloc,
    );
    let (mut result, block_ranges, accum_info, watch_results) = movfuscate(ctx, blocks, state_slot_types, return_slot_types, watch);
    // `pre_init` segments name storage lanes in the *pre-movfuscation*
    // numbering, but every StorageRead/StorageWrite statement just emitted
    // above was remapped to `id*2` (Block-typed value) or `id*2+1`
    // (everything else) -- see the per-statement remap a few dozen lines up
    // in `combine_block`. Left un-remapped, a segment's initial bytes are
    // seeded into a storage lane no statement in the combined block ever
    // reads from, so the circuit silently runs against all-zero memory
    // instead of the real pre-initialized contents.
    result.pre_init = blocks
        .pre_init
        .iter()
        .map(|seg| {
            let is_block = matches!(types.0[seg.ty.0 as usize], IRType::Block { .. });
            let new_storage = if is_block {
                StorageId(seg.storage.0 * 2)
            } else {
                StorageId(seg.storage.0 * 2 + 1)
            };
            PreInitSegment { storage: new_storage, ..seg.clone() }
        })
        .collect();
    (result, block_ranges, accum_info, watch_results)
}


// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
    use volar_ir::ir::{
        IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRType, IRTypeId,
        IRTypes, IRVarId};
    use volar_ir_common::{Constant, Node};

    // =========================================================================
    // pc_bits_needed
    // =========================================================================

    #[test]
    fn test_pc_bits_needed_values() {
        assert_eq!(pc_bits_needed(1), 0);
        assert_eq!(pc_bits_needed(2), 1);
        assert_eq!(pc_bits_needed(3), 2);
        assert_eq!(pc_bits_needed(4), 2);
        assert_eq!(pc_bits_needed(5), 3);
        assert_eq!(pc_bits_needed(8), 3);
        assert_eq!(pc_bits_needed(9), 4);
        assert_eq!(pc_bits_needed(16), 4);
        assert_eq!(pc_bits_needed(17), 5);
    }

    // =========================================================================
    // BIrBlocks movfuscation
    // =========================================================================

    fn two_block_dag() -> BIrBlocks {
        BIrBlocks { blocks: std::vec![
            BIrBlock {
                params: 1,
                stmts: std::vec![BIrStmt::Not(IRVarId(0))].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: BIrTerminator::CondJmp {
                    val: IRVarId(1),
                    then_target: BIrTarget {
                        block: IRBlockTargetId::Block(IRBlockId(1)),
                        args: std::vec![IRVarId(1)],
                    },
                    else_target: BIrTarget {
                        block: IRBlockTargetId::Block(IRBlockId(1)),
                        args: std::vec![IRVarId(0)],
                    },
                },
            },
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                }),
            },
        ], pre_init: std::vec![] }
    }

    #[test]
    fn test_biir_single_block_passthrough() {
        let single = BIrBlocks { blocks: std::vec![BIrBlock {
            params: 2,
            stmts: std::vec![BIrStmt::And(IRVarId(0), IRVarId(1))].into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: std::vec![IRVarId(2)],
            }),
        }], pre_init: std::vec![] };
        let result = movfuscate_biir(&single);
        assert_eq!(result, single);
    }

    #[test]
    fn test_biir_two_block_dag_is_movfuscated() {
        assert!(movfuscate_biir(&two_block_dag()).is_movfuscated());
    }

    #[test]
    fn test_biir_two_block_dag_param_count() {
        let result = movfuscate_biir(&two_block_dag());
        assert_eq!(result.blocks[0].params, 2); // pc(1) + state(1)
    }

    #[test]
    fn test_biir_two_block_dag_terminator_shape() {
        let result = movfuscate_biir(&two_block_dag());
        match &result.blocks[0].terminator {
            BIrTerminator::CondJmp { then_target, else_target, .. } => {
                assert_eq!(then_target.block, IRBlockTargetId::Return);
                assert_eq!(else_target.block, IRBlockTargetId::Block(IRBlockId(0)));
                assert_eq!(else_target.args.len(), 2);
            }
            other => panic!("expected CondJmp, got {:?}", other),
        }
    }

    #[test]
    fn test_biir_ret_width_preserved() {
        match &movfuscate_biir(&two_block_dag()).blocks[0].terminator {
            BIrTerminator::CondJmp { then_target, .. } => {
                assert_eq!(then_target.args.len(), 1);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_biir_three_block_chain() {
        let blocks = BIrBlocks { blocks: std::vec![
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                }),
            },
            BIrBlock {
                params: 1,
                stmts: std::vec![BIrStmt::Not(IRVarId(0))].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(2)),
                    args: std::vec![IRVarId(1)],
                }),
            },
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                }),
            },
        ], pre_init: std::vec![] };
        let result = movfuscate_biir(&blocks);
        assert!(result.is_movfuscated());
        assert_eq!(result.blocks[0].params, 3); // pc(2) + state(1)
        match &result.blocks[0].terminator {
            BIrTerminator::CondJmp { then_target, else_target, .. } => {
                assert_eq!(then_target.block, IRBlockTargetId::Return);
                assert_eq!(else_target.args.len(), 3);
            }
            _ => panic!("expected CondJmp"),
        }
    }

    #[test]
    fn test_biir_self_loop_passthrough() {
        let blocks = BIrBlocks { blocks: std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::Not(IRVarId(0))].into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(0)),
                args: std::vec![IRVarId(1)],
            }),
        }], pre_init: std::vec![] };
        let result = movfuscate_biir(&blocks);
        assert_eq!(result, blocks);
    }

    #[test]
    fn test_biir_four_block_pc_width() {
        let make_pass = |dst: u32| BIrBlock::<()> {
            params: 1,
            stmts: std::vec![BIrStmt::Zero].into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(dst)),
                args: std::vec![IRVarId(0)],
            }),
        };
        let blocks = BIrBlocks { blocks: std::vec![
            make_pass(1),
            make_pass(2),
            make_pass(3),
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                }),
            },
        ], pre_init: std::vec![] };
        let result = movfuscate_biir(&blocks);
        assert!(result.is_movfuscated());
        assert_eq!(result.blocks[0].params, 3); // pc(2) + state(1)
    }

    // =========================================================================
    // IRBlocks movfuscation — Bit-only (regression)
    // =========================================================================

    fn bit_types() -> IRTypes {
        IRTypes(std::vec![IRType::Primitive(Type::Bit)])
    }

    fn two_block_ir_bit() -> (IRBlocks, IRTypes) {
        let types = bit_types();
        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![IRTypeId(0)],
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, IRTypeId(0))].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(0)],) },
            },
            IRBlock {
                params: std::vec![IRTypeId(0)],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_ir_bit_single_block_passthrough() {
        let mut types = bit_types();
        let blocks: IRBlocks<()> = IRBlocks::new(std::vec![IRBlock {
            params: std::vec![IRTypeId(0)],
            stmts: std::vec![],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
        }]);
        assert_eq!(movfuscate_ir(&blocks, &mut types), blocks);
    }

    #[test]
    fn test_ir_bit_two_block_dag_is_movfuscated() {
        let (blocks, mut types) = two_block_ir_bit();
        assert!(movfuscate_ir(&blocks, &mut types).is_movfuscated());
    }

    #[test]
    fn test_ir_bit_two_block_dag_param_count_and_types() {
        let (blocks, mut types) = two_block_ir_bit();
        let result = movfuscate_ir(&blocks, &mut types);
        assert_eq!(result.blocks[0].params.len(), 2); // pc(1) + state(1)
        for tid in &result.blocks[0].params {
            assert!(matches!(types.0[tid.0 as usize], IRType::Primitive(Type::Bit)));
        }
    }

    #[test]
    fn test_ir_bit_two_block_terminator_shape() {
        let (blocks, mut types) = two_block_ir_bit();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.blocks[0].terminator {
            IRTerminator::JumpCond {
                then_target, else_target, ..
            } => {
                assert_eq!(then_target.dest, IRBlockTargetId::Return);
                assert_eq!(then_target.args.len(), 1);
                assert_eq!(else_target.dest, IRBlockTargetId::Block(IRBlockId(0)));
                assert_eq!(else_target.args.len(), 2);
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    #[test]
    fn test_ir_bit_two_block_boundary_ranges_are_contiguous_and_cover_all_stmts() {
        let (blocks, mut types) = two_block_ir_bit();
        let (result, block_ranges, _accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);
        assert_eq!(block_ranges.len(), 2, "one range per original block");

        let combined_params = result.blocks[0].params.len() as u32;
        // Contiguous: block 0 starts right after params (plus the one
        // shared `bit_zero` stmt emitted before the per-block loop), block
        // 1 starts right where block 0 ends, and block 1 ends at the last
        // stmt.
        assert_eq!(block_ranges[0].start, combined_params + 1, "first range starts right after params + bit_zero");
        assert_eq!(block_ranges[0].end, block_ranges[1].start, "ranges are back-to-back, no gap");
        // The last range ends before the final cross-block accumulation
        // phase (done/next_pc/next_state/ret_vals, combining *all* blocks'
        // results together) -- that phase necessarily comes after every
        // per-block range, so it's strictly less than the combined block's
        // total stmt count, not equal to it.
        let total_stmts = combined_params + result.blocks[0].stmts.len() as u32;
        assert!(
            block_ranges[1].end < total_stmts,
            "last range ({}) must end before the final accumulation phase's stmts ({total_stmts})",
            block_ranges[1].end
        );
        // Every range is non-empty and strictly increasing. Exported var
        // ids are always defined by the time this block's range ends --
        // but not necessarily *within* [start, end) itself: `is_active`
        // for a single-PC-bit block can be the PC param var directly (no
        // gate needed for "bit == 1"), so it may reference a param
        // (var id < combined_params) rather than a freshly emitted stmt.
        for b in &block_ranges {
            assert!(b.start < b.end, "range ({}, {}) must be non-empty", b.start, b.end);
            assert!(b.is_active < b.end);
            assert!(b.done < b.end);
        }
    }

    #[test]
    fn test_ir_bit_two_block_boundary_matches_plain_movfuscate_ir() {
        // movfuscate_ir_with_boundary must produce byte-identical IRBlocks
        // output to movfuscate_ir -- the boundary tracking is purely
        // additive, must never change what gets emitted.
        let (blocks, mut types_a) = two_block_ir_bit();
        let mut types_b = types_a.clone();
        let plain = movfuscate_ir(&blocks, &mut types_a);
        let (with_boundary, _ranges, _accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types_b);
        assert_eq!(plain, with_boundary);
    }

    // =========================================================================
    // IRBlocks movfuscation — Galois8AES state slots
    // =========================================================================

    /// Two-block module with Galois8AES params:
    ///   Block 0 (1 param `a: G8`): Jmp(Block(1), [a])
    ///   Block 1 (1 param `x: G8`): Jmp(Return, [x])
    fn two_block_ir_g8() -> (IRBlocks, IRTypes) {
        // types[0] = Bit, types[1] = Galois8AES
        let types = IRTypes(std::vec![IRType::Primitive(Type::Bit), IRType::Primitive(Type::AES8)]);
        let g8 = IRTypeId(1);
        let bit = IRTypeId(0);
        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, bit)].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(0)],) },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_ir_g8_two_block_is_movfuscated() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
    }

    #[test]
    fn test_ir_g8_combined_param_types() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        // pc(1) + state(1: G8) = 2 params
        assert_eq!(result.blocks[0].params.len(), 2);
        // First param (PC) must be Bit
        assert!(
            matches!(types.0[result.blocks[0].params[0].0 as usize], IRType::Primitive(Type::Bit)),
            "PC param must be Bit"
        );
        // Second param (state slot 0) must be Galois8AES
        assert!(
            matches!(types.0[result.blocks[0].params[1].0 as usize], IRType::Primitive(Type::AES8)),
            "state slot 0 must be Galois8AES"
        );
    }

    #[test]
    fn test_ir_g8_terminator_shape() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.blocks[0].terminator {
            IRTerminator::JumpCond { then_target, else_target, .. } => {
                assert_eq!(then_target.args.len(), 1, "ret_width = 1 (one G8 value)");
                assert_eq!(else_target.args.len(), 2, "loop-back = pc(1) + state(1)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    #[test]
    fn test_ir_g8_gate_stmts_are_poly() {
        // The dispatch overhead must include Poly stmts (gate and field-add
        // for the G8 state slot), not raw And/Xor BIr stmts.
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        let has_poly = result.blocks[0]
            .stmts
            .iter()
            .any(|node| matches!(&node.kind, IRStmt::Poly { .. }));
        assert!(has_poly, "combined block must contain Poly stmts for field dispatch");
    }

    /// Verify that gate Poly stmts for G8 slots are degree-2 (two-var monomial).
    #[test]
    fn test_ir_g8_gate_poly_is_degree2() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        // Find at least one degree-2 Poly (the gate op: is_active * val).
        let degree2 = result.blocks[0].stmts.iter().any(|node| {
            if let IRStmt::Poly { coeffs, .. } = &node.kind {
                coeffs.keys().any(|mono| mono.len() == 2)
            } else {
                false
            }
        });
        assert!(degree2, "expected at least one degree-2 Poly for the gate operation");
    }

    // =========================================================================
    // IRBlocks movfuscation — mixed Bit + Galois8AES params
    // =========================================================================

    /// Two blocks, different param arity:
    ///   Block 0: params (a: G8, b: Bit); JumpCond(b, Block(1,[a]), Return([a]))
    ///   Block 1: params (x: G8);         Jmp(Return, [x])
    #[test]
    fn test_ir_mixed_types_two_block() {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit), IRType::Primitive(Type::AES8)]);
        let bit = IRTypeId(0);
        let g8 = IRTypeId(1);

        let blocks: IRBlocks<()> = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![g8.clone(), bit.clone()],
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, bit.clone())].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::JumpCond {
                    condition: IRVarId(1), // b
                    then_target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(0)]), // a
                    else_target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)]), // a
                },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);

        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // pc_width=1, state_width=2 (block 0 has 2 params: G8 at slot 0, Bit at slot 1)
        assert_eq!(result.blocks[0].params.len(), 3, "pc(1) + state(2)");

        // Param types: [Bit (PC), G8 (slot 0), Bit (slot 1)]
        let p = &result.blocks[0].params;
        assert!(matches!(types.0[p[0].0 as usize], IRType::Primitive(Type::Bit)), "PC slot");
        assert!(matches!(types.0[p[1].0 as usize], IRType::Primitive(Type::AES8)), "state slot 0");
        assert!(matches!(types.0[p[2].0 as usize], IRType::Primitive(Type::Bit)), "state slot 1");

        match &result.blocks[0].terminator {
            IRTerminator::JumpCond { then_target, else_target, .. } => {
                assert_eq!(then_target.args.len(), 1, "ret_width = 1 (G8)");
                assert_eq!(else_target.args.len(), 3, "loop-back = pc(1) + state(2)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    /// Tunnelled/unchanged-state-slot elimination: block 0 modifies only
    /// state slot 0 (leaving slot 1 an exact pass-through of its own
    /// entry param); block 1 modifies only slot 1 (leaving slot 0 an
    /// exact pass-through). Verifies both the *mechanism* (no new
    /// accumulation var is allocated for an untouched slot -- the step's
    /// own `next_state[k]` stays exactly the previous step's, i.e.
    /// `accum_init`'s or the prior block's) and that end-to-end
    /// movfuscation still produces a valid, correctly-typed combined
    /// block. Both blocks loop back via `Jmp` (not `Return`) deliberately:
    /// a `Return`-bound terminator always resets `next_state` to fresh
    /// zeros for *every* slot regardless of its own args (nothing carries
    /// forward past a real return -- see `process_ir_target`'s
    /// `IRBlockTargetId::Return` arm), so it can never exercise the
    /// pass-through case this test targets; `Jmp`/`JumpCond` to another
    /// block is the actual dominant shape in the real interpreter (almost
    /// every block loops back into dispatch, not a real `Return`).
    #[test]
    fn test_ir_tunnelled_state_slot_skips_accumulation_for_untouched_blocks() {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);

        let blocks: IRBlocks<()> = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![bit.clone(), bit.clone()], // slot0, slot1
                stmts: std::vec![IRStmt::Poly {
                    ty: bit.clone(),
                    coeffs: std::collections::BTreeMap::from([(std::vec![IRVarId(0)], 1u8)]),
                    constant: Constant { hi: 0, lo: 1 },
                }].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    // slot0 <- Not(slot0) (touched); slot1 <- slot1 (untouched pass-through)
                    target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(2), IRVarId(1)]),
                },
            },
            IRBlock {
                params: std::vec![bit.clone(), bit.clone()], // slot0, slot1
                stmts: std::vec![IRStmt::Poly {
                    ty: bit.clone(),
                    coeffs: std::collections::BTreeMap::from([(std::vec![IRVarId(1)], 1u8)]),
                    constant: Constant { hi: 0, lo: 1 },
                }].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    // slot0 <- slot0 (untouched pass-through); slot1 <- Not(slot1) (touched)
                    target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(0)), std::vec![IRVarId(0), IRVarId(2)]),
                },
            },
            // Block 2: unreachable from 0/1 in this synthetic example, but
            // movfuscation processes every block unconditionally regardless
            // of reachability (each gets its own independent is_active/
            // dispatch entry) -- this just gives `compute_return_slot_types`
            // a real `Return` to derive `ret_width` from, without disturbing
            // blocks 0/1's own Jmp-based pass-through behavior under test.
            IRBlock {
                params: std::vec![bit.clone(), bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)]),
                },
            },
        ]);

        let (result, _boundary, accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);
        assert!(result.is_movfuscated());
        assert_eq!(accum_info.steps.len(), 3);

        // Block 0 (steps[0]) touches slot 0 (new var, different from the
        // seed) but not slot 1 (must stay exactly accum_init's own var --
        // no gate/add pair was emitted for it).
        assert_ne!(accum_info.steps[0].next_state[0], accum_info.init.next_state[0], "block 0 touches slot 0 -- must allocate a new accumulation var");
        assert_eq!(accum_info.steps[0].next_state[1], accum_info.init.next_state[1], "block 0 does not touch slot 1 -- must skip accumulation entirely, not just skip the change");

        // Block 1 (steps[1]) touches slot 1 (new var, different from
        // steps[0]'s) but not slot 0 (must carry steps[0]'s own var
        // forward unchanged).
        assert_eq!(accum_info.steps[1].next_state[0], accum_info.steps[0].next_state[0], "block 1 does not touch slot 0 -- must carry the running value forward untouched");
        assert_ne!(accum_info.steps[1].next_state[1], accum_info.steps[0].next_state[1], "block 1 touches slot 1 -- must allocate a new accumulation var");

        // End-to-end sanity: still a well-typed, valid combined block (not
        // asserting an exact param count here -- irrelevant to what this
        // test targets, and sensitive to slot-splitting details of the
        // 3rd, return-only block that aren't this test's concern).
        for ty_id in &result.blocks[0].params {
            assert!(matches!(types.0[ty_id.0 as usize], IRType::Primitive(Type::Bit)), "every combined param must still be a valid, well-typed Bit slot");
        }
    }

    /// A genuine position-type *collision* (not just an arity gap): both
    /// blocks have a param at position 0 *and* position 1, but the types
    /// are swapped between them (block 0: [Bit, G8], block 1: [G8, Bit]).
    /// Before this fix, `movfuscate_ir` would panic here ("block 1 has type
    /// ... but an earlier block had a different non-Block type there") --
    /// this is exactly the class of bug found in the real RISC-V
    /// interpreter's own movfuscated circuit (a synthetic trampoline
    /// block's param colliding, by raw position only, with an unrelated
    /// real loop-body block's own differently-typed param). Each colliding
    /// position must now split into two separate, non-aliasing slot groups
    /// instead.
    #[test]
    fn test_ir_position_type_collision_splits_into_separate_slots() {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit), IRType::Primitive(Type::AES8)]);
        let bit = IRTypeId(0);
        let g8 = IRTypeId(1);

        let blocks: IRBlocks<()> = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![bit.clone(), g8.clone()], // position 0: Bit, position 1: G8
                // A trivial stmt -- `movfuscate_ir` needs at least one
                // real statement somewhere to derive provenance for its
                // own synthetic infrastructure gates.
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, bit.clone())].into_iter().map(|s| Node::new(s, (), None)).collect(),
                // Jump to block 1, which expects [G8, Bit] -- swap args to
                // stay internally type-correct (var 1 is G8, var 0 is Bit).
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(1), IRVarId(0)]),
                },
            },
            IRBlock {
                params: std::vec![g8.clone(), bit.clone()], // position 0: G8 (collides with block 0's Bit), position 1: Bit (collides with block 0's G8)
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0), IRVarId(1)]),
                },
            },
        ]);

        // Must not panic -- the real point of this test.
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());

        // This is a *structural* position collision (block 0's position 0
        // is Bit, block 1's position 0 is G8) but NOT a genuine same-slot
        // collision: block 0's own Jmp explicitly swaps its args
        // (`[IRVarId(1), IRVarId(0)]`), so block 0's position-1 G8 param
        // is what actually flows into block 1's position-0 G8 param (and
        // block 0's position-0 Bit param into block 1's position-1 Bit
        // param) -- a real, provable dataflow edge, not a coincidence.
        // `compute_static_slot_classes`'s edge-aware allocation traces
        // this correctly and unifies each pair onto ONE shared slot
        // (2 slots total), rather than the old position+type-keyed
        // `compute_position_groups`'s blind 4-slot split (which didn't
        // look at args at all, just positions) -- pc(1) + state(2).
        assert_eq!(result.blocks[0].params.len(), 3, "pc(1) + state(2 dataflow-unified slots, not 4 positionally-split ones)");

        let p = &result.blocks[0].params;
        let is_bit = |tid: &IRTypeId| matches!(types.0[tid.0 as usize], IRType::Primitive(Type::Bit));
        let is_g8 = |tid: &IRTypeId| matches!(types.0[tid.0 as usize], IRType::Primitive(Type::AES8));
        assert!(is_bit(&p[0]), "PC slot must be Bit");
        assert!(
            (is_g8(&p[1]) && is_bit(&p[2])) || (is_bit(&p[1]) && is_g8(&p[2])),
            "exactly one Bit and one G8 state slot, order depends on traversal order"
        );
    }

    /// A *genuine* position+type collision (no edge connects the two
    /// blocks at all, so there's no dataflow evidence they're the same
    /// value) must still resolve safely -- i.e. NOT be merged onto one
    /// slot just because they coincidentally share position+type. This is
    /// exactly the shape of bug `compute_position_groups`'s pure
    /// structural matching couldn't tell apart from a real shared value
    /// (see project memory: Milestone 1.6's "real interpreter computes
    /// the wrong answer" bug).
    #[test]
    fn test_ir_unrelated_same_position_same_type_blocks_get_separate_slots() {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);

        let blocks: IRBlocks<()> = IRBlocks::new(std::vec![
            // Block 0: entry, unconditionally jumps to block 1 with a
            // FRESH value (not one of its own params) -- e.g. a constant.
            IRBlock {
                params: std::vec![],
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, bit.clone())]
                    .into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(0)]),
                },
            },
            // Block 1: position 0 is Bit -- receives block 0's constant.
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)]),
                },
            },
        ]);

        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // Block 1 is never re-entered and block 0 has no params of its own
        // to alias with it, so block 1's own position-0 slot is entirely
        // private to it: pc(1) + state(1).
        assert_eq!(result.blocks[0].params.len(), 2, "pc(1) + state(1 private slot)");
    }

    /// Regression guard for the common case: blocks that genuinely *agree*
    /// on type at a shared position must still consolidate onto one slot
    /// (no spurious splitting just because collision-splitting now exists).
    #[test]
    fn test_ir_position_type_agreement_still_dedups() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // pc(1) + state(1) -- both blocks agree G8 at position 0, one slot.
        assert_eq!(result.blocks[0].params.len(), 2, "agreeing blocks must not split into extra slots");
    }

    // =========================================================================
    // IRBlocks movfuscation — Poly stmt in block body
    // =========================================================================

    #[test]
    fn test_ir_poly_body_subst_correct() {
        // Block 0 (a: G8): c = Poly{[a]:1, constant:0} (= identity); Jmp(B1,[c])
        // Block 1 (x: G8): Jmp(Return,[x])
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit), IRType::Primitive(Type::AES8)]);
        let g8 = IRTypeId(1);
        let mut coeffs = BTreeMap::new();
        coeffs.insert(std::vec![IRVarId(0)], 1u8);
        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![IRStmt::Poly {
                    ty: g8,
                    coeffs,
                    constant: Constant { hi: 0, lo: 0 },
                }].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(1)]) }, // the Poly result,
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // Must contain at least the re-emitted Poly from block 0's body.
        let poly_count = result.blocks[0]
            .stmts
            .iter()
            .filter(|node| matches!(&node.kind, IRStmt::Poly { .. }))
            .count();
        assert!(poly_count >= 1, "re-emitted Poly should appear in combined block");
    }

    // =========================================================================
    // IRBlocks movfuscation — IRType::Block param (Dyn via param)
    // =========================================================================

    /// Two blocks where BOTH carry a `Block{[]}`-typed param so the state
    /// layout is consistent:
    ///
    /// - Block 0: params=[cont: Block{[]}], terminator=Jmp(Dyn(cont), [])
    /// - Block 1: params=[cont: Block{[]}], terminator=Jmp(Return, [])
    ///
    /// The caller initialises state0 with the binary encoding of block 1 (= 1).
    /// Execution: B0 active → next_pc = [state0] → B1 active → done.
    fn two_block_dyn_param() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);
        // Register IRType::Block{[]} and get its type ID.
        let block_ty_id = IRTypeId(types.0.len() as u32);
        types.0.push(IRType::Block { params: std::vec![] });

        let blocks = IRBlocks::new(std::vec![
            // Block 0: Dyn(cont, [])
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 0 }, bit.clone())].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Dyn(IRVarId(0)), std::vec![]), // cont
                },
            },
            // Block 1: Return
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_ir_block_dyn_param_is_movfuscated() {
        let (blocks, mut types) = two_block_dyn_param();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
    }

    #[test]
    fn test_ir_block_dyn_param_combined_param_count() {
        let (blocks, mut types) = two_block_dyn_param();
        let result = movfuscate_ir(&blocks, &mut types);
        // pc_width=1 (2 blocks). Both blocks' own `cont` param has type
        // `Block{params: []}` -- an empty signature that doesn't actually
        // equal *either* block's own real (1-param) declared param list,
        // so `compute_static_slot_classes`'s signature-based Dyn fallback
        // (matching a Dyn value's own inferred signature against real
        // blocks' declared param lists) finds no candidate to union with
        // and each block's `cont` gets its own private slot -- 2 state
        // slots, not 1 shared one like the old position+type scheme. Same
        // "extra slots, never aliasing" safety tradeoff as
        // `test_ir_block_pass_arg_param_layout`; see its own comment.
        // Combined params: [pc0, state0, state1] = 3.
        assert_eq!(result.blocks[0].params.len(), 3, "pc(1) + state(2 separate Block-param slots)");
        // Both combined params must be Bit.
        for tid in &result.blocks[0].params {
            assert!(
                matches!(types.0[tid.0 as usize], IRType::Primitive(Type::Bit)),
                "all combined params must be Bit"
            );
        }
    }

    #[test]
    fn test_ir_block_dyn_param_terminator_shape() {
        let (blocks, mut types) = two_block_dyn_param();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.blocks[0].terminator {
            IRTerminator::JumpCond { then_target, else_target, .. } => {
                assert_eq!(then_target.dest, IRBlockTargetId::Return);
                assert_eq!(then_target.args.len(), 0, "ret_width = 0");
                assert_eq!(else_target.dest, IRBlockTargetId::Block(IRBlockId(0)));
                // See `test_ir_block_dyn_param_combined_param_count`'s own
                // comment for why this is 3 (pc(1) + state(2)), not 2.
                assert_eq!(else_target.args.len(), 3, "loop-back args = pc(1) + state(2)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    // =========================================================================
    // IRBlocks movfuscation — IRType::Block Const (Dyn via constant)
    // =========================================================================

    /// Two blocks where block 0 creates a static reference to block 1 via
    /// `Const(1, Block{[]})` and immediately jumps to it:
    ///
    /// - Block 0: params=[], stmts=[c=Const(1,Block{[]})], Jmp(Dyn(c),[])
    /// - Block 1: params=[], Jmp(Return,[])
    fn two_block_dyn_const() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let block_ty_id = IRTypeId(types.0.len() as u32);
        types.0.push(IRType::Block { params: std::vec![] });

        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![],
                stmts: std::vec![
                    IRStmt::Const(
                        Constant { hi: 0, lo: 1 }, // block index 1
                        block_ty_id.clone(),
                    ),
                ].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Dyn(IRVarId(0)), std::vec![]), // c (stmt result)
                },
            },
            IRBlock {
                params: std::vec![],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_ir_block_dyn_const_is_movfuscated() {
        let (blocks, mut types) = two_block_dyn_const();
        assert!(movfuscate_ir(&blocks, &mut types).is_movfuscated());
    }

    #[test]
    fn test_ir_block_dyn_const_param_count() {
        let (blocks, mut types) = two_block_dyn_const();
        let result = movfuscate_ir(&blocks, &mut types);
        // Both blocks have no params → state_width = 0; pc_width = 1.
        // Combined params: [pc0: Bit] only.
        assert_eq!(result.blocks[0].params.len(), 1, "only the PC bit");
        assert!(matches!(types.0[result.blocks[0].params[0].0 as usize], IRType::Primitive(Type::Bit)));
    }

    #[test]
    fn test_ir_block_dyn_const_no_block_typed_stmts_in_output() {
        // The combined block must not contain any Block-typed stmts.
        let (blocks, mut types) = two_block_dyn_const();
        let result = movfuscate_ir(&blocks, &mut types);
        for node in &result.blocks[0].stmts {
            if let IRStmt::Const(_, ty_id) = &node.kind {
                assert!(
                    !matches!(types.0[ty_id.0 as usize], IRType::Block { .. }),
                    "combined block must not contain Block-typed Const stmts"
                );
            }
        }
    }

    // =========================================================================
    // IRBlocks movfuscation — Dyn with args
    // =========================================================================

    /// Block 0 creates a static reference to block 1 (which takes a Bit param)
    /// and calls it with a constant-1 Bit value:
    ///
    /// - Block 0: params=[], stmts=[c=Const(1,Block{[Bit]}), one=Const(1,Bit)],
    ///            Jmp(Dyn(c), [one])
    /// - Block 1: params=[x: Bit], Jmp(Return, [x])
    ///
    /// After movfuscation the combined block should return the value that was
    /// passed as `one` = 1 (via state slot 0).
    fn two_block_dyn_with_args() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);
        let block_ty_id = IRTypeId(types.0.len() as u32);
        types.0.push(IRType::Block { params: std::vec![bit.clone()] });

        let blocks = IRBlocks::new(std::vec![
            // Block 0: create ref to block 1, call it with `one`.
            IRBlock {
                params: std::vec![],
                stmts: std::vec![
                    // stmt 0: c = Const(1, Block{[Bit]})  (block index = 1)
                    IRStmt::Const(Constant { hi: 0, lo: 1 }, block_ty_id.clone()),
                    // stmt 1: one = Const(1, Bit)
                    IRStmt::Const(Constant { hi: 0, lo: 1 }, bit.clone()),
                ].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Dyn(IRVarId(0)), std::vec![IRVarId(1)]), // c, one
                },
            },
            // Block 1: take x: Bit and return it.
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_ir_block_dyn_with_args_is_movfuscated() {
        let (blocks, mut types) = two_block_dyn_with_args();
        assert!(movfuscate_ir(&blocks, &mut types).is_movfuscated());
    }

    #[test]
    fn test_ir_block_dyn_with_args_param_layout() {
        let (blocks, mut types) = two_block_dyn_with_args();
        let result = movfuscate_ir(&blocks, &mut types);
        // pc_width=1, state_width=1 (block 1's param x: Bit → slot 0).
        // Combined params: [pc0: Bit, state0: Bit].
        assert_eq!(result.blocks[0].params.len(), 2, "pc(1) + state(1)");
        for tid in &result.blocks[0].params {
            assert!(matches!(types.0[tid.0 as usize], IRType::Primitive(Type::Bit)));
        }
    }

    #[test]
    fn test_ir_block_dyn_with_args_ret_width() {
        let (blocks, mut types) = two_block_dyn_with_args();
        let result = movfuscate_ir(&blocks, &mut types);
        // Block 1 returns x: Bit → ret_width = 1.
        match &result.blocks[0].terminator {
            IRTerminator::JumpCond { then_target, else_target, .. } => {
                assert_eq!(then_target.args.len(), 1, "return one Bit value");
                assert_eq!(else_target.args.len(), 2, "loop-back = pc(1) + state(1)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    // =========================================================================
    // IRBlocks movfuscation — Block param passed as static jump arg
    // =========================================================================

    /// Block 0 takes no params and immediately jumps to Block 1, passing a
    /// static reference to Block 2 as a `Block{[]}`-typed argument:
    ///
    /// - Block 0: params=[],    Jmp(Block(1), [Const_ref_to_2])
    ///   (realized via a Const stmt in block 0)
    /// - Block 1: params=[cont: Block{[]}], Jmp(Dyn(cont), [])
    /// - Block 2: params=[cont: Block{[]}], Jmp(Return, [])
    fn three_block_pass_block_arg() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let block_ty_id = IRTypeId(types.0.len() as u32);
        types.0.push(IRType::Block { params: std::vec![] });

        let blocks = IRBlocks::new(std::vec![
            // Block 0: create a ref to block 2 and pass it to block 1.
            IRBlock {
                params: std::vec![],
                stmts: std::vec![
                    IRStmt::Const(Constant { hi: 0, lo: 2 }, block_ty_id.clone()),
                ].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(0)]) }, // pass the Block ref,
            },
            // Block 1: holds a cont and Dyn-jumps to it.
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Dyn(IRVarId(0)), std::vec![],) },
            },
            // Block 2: the target of the continuation.
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_ir_block_pass_arg_is_movfuscated() {
        let (blocks, mut types) = three_block_pass_block_arg();
        assert!(movfuscate_ir(&blocks, &mut types).is_movfuscated());
    }

    #[test]
    fn test_ir_block_pass_arg_param_layout() {
        let (blocks, mut types) = three_block_pass_block_arg();
        let result = movfuscate_ir(&blocks, &mut types);
        // 3 blocks → pc_width = 2.
        //
        // `compute_static_slot_classes`'s edge-aware allocation gives
        // block 1's own `cont` param and block 2's own `cont` param
        // SEPARATE slots here (2 Bit slots each = 4 Bit state), not one
        // shared pair (2 Bit) like the old position+type scheme -- it
        // only traces a `Dyn(v)` target to a literal constant when `v`
        // is a `Const` *within the same block* (see `resolve_dyn_target`);
        // here the constant reference to block 2 is created in block 0
        // and threaded one hop through block 1's own param before being
        // Dyn-jumped, so it isn't traced as a literal, and the signature
        // -based fallback can't help either (nothing else in this tiny
        // fixture shares `cont`'s exact `Block{[]}` signature by a real
        // edge). This is strictly *safe* (never aliases two different
        // values; costs extra slots, not correctness) -- see
        // `compute_static_slot_classes`'s own doc comment.
        // Combined params: [pc0, pc1, state0..3] = 6.
        assert_eq!(result.blocks[0].params.len(), 6, "pc(2) + state(4 Bit: 2 separate Block conts)");
        for tid in &result.blocks[0].params {
            assert!(matches!(types.0[tid.0 as usize], IRType::Primitive(Type::Bit)));
        }
    }

    #[test]
    fn test_ir_block_pass_arg_terminator_shape() {
        let (blocks, mut types) = three_block_pass_block_arg();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.blocks[0].terminator {
            IRTerminator::JumpCond { then_target, else_target, .. } => {
                assert_eq!(then_target.args.len(), 0, "ret_width = 0");
                // See `test_ir_block_pass_arg_param_layout`'s own comment
                // for why this is 6 (pc(2) + state(4)), not 4.
                assert_eq!(else_target.args.len(), 6, "loop-back = pc(2) + state(4)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    // =========================================================================
    // IRBlocks movfuscation — block storage (StorageRead/StorageWrite lane split)
    // =========================================================================

    /// Build a 2-block circuit with a non-Block `StorageWrite` in block 0:
    ///
    /// - Block 0: params=[addr: Bit],
    ///            stmts=[_w = StorageWrite(storage=5, src=addr, ty=Bit, addr=addr)],
    ///            Jmp(Block(1), [addr])
    /// - Block 1: params=[x: Bit], Jmp(Return, [x])
    ///
    /// After movfuscation, the StorageWrite storage ID must be remapped to the
    /// odd lane: `5 * 2 + 1 = 11`.
    fn two_block_nonblock_storage_write() -> (IRBlocks, IRTypes) {
        let types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);
        let storage_in = StorageId(5);
        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![IRStmt::StorageWrite {
                    storage: storage_in,
                    src: IRVarId(0),
                    ty: bit.clone(),
                    addr: IRVarId(0),
                }].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(0)],) },
            },
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_storage_write_nonblock_uses_odd_lane() {
        let (blocks, mut types) = two_block_nonblock_storage_write();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // Find the StorageWrite in the combined block and verify it got the odd lane ID.
        let odd_lane = StorageId(5 * 2 + 1);
        let has_write = result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::StorageWrite { storage, .. } if *storage == odd_lane)
        });
        assert!(has_write, "non-Block StorageWrite must be remapped to odd lane 11");
        // Verify the original even ID 10 does not appear.
        let no_even = !result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::StorageWrite { storage, .. } if *storage == StorageId(5 * 2))
        });
        assert!(no_even, "even lane must not be used for non-Block write");
    }

    /// Build a 2-block circuit with a non-Block `StorageRead` in block 0:
    ///
    /// - Block 0: params=[addr: Bit],
    ///            stmts=[v = StorageRead(storage=3, ty=Bit, addr=addr)],
    ///            Jmp(Block(1), [v])
    /// - Block 1: params=[x: Bit], Jmp(Return, [x])
    ///
    /// After movfuscation, StorageRead must be remapped to odd lane: `3*2+1 = 7`.
    fn two_block_nonblock_storage_read() -> (IRBlocks, IRTypes) {
        let types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);
        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![IRStmt::StorageRead {
                    storage: StorageId(3),
                    ty: bit.clone(),
                    addr: IRVarId(0),
                }].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(1)), std::vec![IRVarId(1)]) }, // the read result,
            },
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![IRVarId(0)],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_storage_read_nonblock_uses_odd_lane() {
        let (blocks, mut types) = two_block_nonblock_storage_read();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        let odd_lane = StorageId(3 * 2 + 1);
        let has_read = result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::StorageRead { storage, .. } if *storage == odd_lane)
        });
        assert!(has_read, "non-Block StorageRead must be remapped to odd lane 7");
        let no_even = !result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::StorageRead { storage, .. } if *storage == StorageId(3 * 2))
        });
        assert!(no_even, "even lane must not be used for non-Block read");
    }

    /// Block-typed StorageWrite uses the even lane.
    ///
    /// - Block 0: params=[addr: Bit],
    ///            stmts=[c = Const(1, Block{[]}), _w = StorageWrite(storage=2, src=c, ...)],
    ///            Jmp(Dyn(c), [])
    /// - Block 1: params=[cont: Block{[]}], Jmp(Return, [])
    ///
    /// After movfuscation, the StorageWrite must:
    ///   1. Use even storage lane: `2 * 2 = 4`.
    ///   2. Be preceded by a Merge stmt (to pack the PC bits).
    fn two_block_block_storage_write() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes(std::vec![IRType::Primitive(Type::Bit)]);
        let bit = IRTypeId(0);
        let block_ty = IRTypeId(types.0.len() as u32);
        types.0.push(IRType::Block { params: std::vec![] });

        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![
                    // stmt 0: c = Const(1, Block{[]})
                    IRStmt::Const(Constant { hi: 0, lo: 1 }, block_ty.clone()),
                    // stmt 1: _w = StorageWrite(storage=2, src=c, ty=Block{[]}, addr=addr)
                    IRStmt::StorageWrite {
                        storage: StorageId(2),
                        src: IRVarId(1), // c (var 1, since params=[bit] → var 0 = addr, var 1 = c)
                        ty: block_ty.clone(),
                        addr: IRVarId(0),
                    },
                ].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Dyn(IRVarId(1)), std::vec![]), // c
                },
            },
            IRBlock {
                params: std::vec![block_ty.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![],) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_storage_write_block_uses_even_lane() {
        let (blocks, mut types) = two_block_block_storage_write();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // Even lane: 2 * 2 = 4.
        let even_lane = StorageId(2 * 2);
        let has_even_write = result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::StorageWrite { storage, .. } if *storage == even_lane)
        });
        assert!(has_even_write, "Block-typed StorageWrite must use even lane 4");
        // Odd lane must not appear for this write.
        let no_odd = !result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::StorageWrite { storage, .. } if *storage == StorageId(2 * 2 + 1))
        });
        assert!(no_odd, "odd lane must not be used for Block-typed write");
    }

    #[test]
    fn test_storage_write_block_emits_merge_stmt() {
        let (blocks, mut types) = two_block_block_storage_write();
        let result = movfuscate_ir(&blocks, &mut types);
        // A Merge stmt must appear to pack the Block's PC bits into a Vec.
        let has_merge = result.blocks[0].stmts.iter().any(|node| {
            matches!(&node.kind, IRStmt::Merge { .. })
        });
        assert!(has_merge, "Block-typed StorageWrite must emit a Merge stmt to pack PC bits");
    }

    // =========================================================================
    // remap_movfusc_boundary / remap_movfusc_accum_info
    // =========================================================================

    fn sample_boundary() -> MovfuscBlockBoundary {
        MovfuscBlockBoundary {
            start: 10, end: 20, is_active: 11, done: 12,
            next_pc_bits: vec![13, 14], next_state: vec![15, 16], ret_vals: vec![17],
        }
    }

    fn sample_accum_info() -> MovfuscAccumInfo {
        MovfuscAccumInfo {
            init: MovfuscAccumInit {
                start: 0, end: 5, done_acc: 1, next_pc: vec![2], next_state: vec![3], ret_vals: vec![4],
            },
            steps: vec![MovfuscAccumStep {
                start: 5, end: 10, done_acc: 6, next_pc: vec![7], next_state: vec![8], ret_vals: vec![9],
            }],
        }
    }

    #[test]
    fn test_remap_movfusc_boundary_identity() {
        let boundary = sample_boundary();
        let identity: BTreeMap<u32, u32> = (0..30).map(|v| (v, v)).collect();
        let remapped = remap_movfusc_boundary(&boundary, &identity);
        assert_eq!(remapped.start, boundary.start);
        assert_eq!(remapped.end, boundary.end);
        assert_eq!(remapped.is_active, boundary.is_active);
        assert_eq!(remapped.done, boundary.done);
        assert_eq!(remapped.next_pc_bits, boundary.next_pc_bits);
        assert_eq!(remapped.next_state, boundary.next_state);
        assert_eq!(remapped.ret_vals, boundary.ret_vals);
    }

    #[test]
    fn test_remap_movfusc_boundary_translates_every_field() {
        let boundary = sample_boundary();
        // Shift every referenced id up by 100 (simulating DCE renumbering
        // after removing 100 earlier dead statements).
        let mut remap: BTreeMap<u32, u32> = BTreeMap::new();
        for v in [10u32, 20, 11, 12, 13, 14, 15, 16, 17] {
            remap.insert(v, v + 100);
        }
        let remapped = remap_movfusc_boundary(&boundary, &remap);
        assert_eq!(remapped.start, 110);
        assert_eq!(remapped.end, 120);
        assert_eq!(remapped.is_active, 111);
        assert_eq!(remapped.done, 112);
        assert_eq!(remapped.next_pc_bits, vec![113, 114]);
        assert_eq!(remapped.next_state, vec![115, 116]);
        assert_eq!(remapped.ret_vals, vec![117]);
    }

    #[test]
    #[should_panic(expected = "no remap entry")]
    fn test_remap_movfusc_boundary_panics_on_missing_entry() {
        let boundary = sample_boundary();
        let empty: BTreeMap<u32, u32> = BTreeMap::new();
        let _ = remap_movfusc_boundary(&boundary, &empty);
    }

    #[test]
    fn test_remap_movfusc_accum_info_translates_every_field() {
        let info = sample_accum_info();
        let remap: BTreeMap<u32, u32> = (0..=10u32).map(|v| (v, v + 1000)).collect();
        let remapped = remap_movfusc_accum_info(&info, &remap);
        assert_eq!(remapped.init.start, 1000);
        assert_eq!(remapped.init.end, 1005);
        assert_eq!(remapped.init.done_acc, 1001);
        assert_eq!(remapped.init.next_pc, vec![1002]);
        assert_eq!(remapped.init.next_state, vec![1003]);
        assert_eq!(remapped.init.ret_vals, vec![1004]);
        assert_eq!(remapped.steps.len(), 1);
        assert_eq!(remapped.steps[0].start, 1005);
        assert_eq!(remapped.steps[0].done_acc, 1006);
        assert_eq!(remapped.steps[0].next_pc, vec![1007]);
        assert_eq!(remapped.steps[0].next_state, vec![1008]);
        assert_eq!(remapped.steps[0].ret_vals, vec![1009]);
    }

    #[test]
    fn test_remap_movfusc_boundaries_maps_a_slice() {
        let boundaries = vec![sample_boundary(), sample_boundary()];
        let identity: BTreeMap<u32, u32> = (0..30).map(|v| (v, v)).collect();
        let remapped = remap_movfusc_boundaries(&boundaries, &identity);
        assert_eq!(remapped.len(), 2);
        assert_eq!(remapped[0].start, boundaries[0].start);
    }
}
