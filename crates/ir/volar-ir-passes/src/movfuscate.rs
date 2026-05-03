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
use alloc::collections::BTreeMap;
use volar_ir_common::{Constant, StorageId, Type};

use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{
        IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId,
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
    /// Returns the full var table `[remapped_params…, fresh_stmt_vars…]`.
    fn emit_block_stmts(
        &mut self,
        blocks: &Self::Blocks,
        block_idx: usize,
        state_vars: &[u32],
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

/// Combine all blocks of `blocks` into a single self-looping block.
///
/// `state_slot_types[k]` — type of state slot `k` (length = max block param
/// count).  `return_slot_types[m]` — type of return value `m`.
///
/// Returns the single-block module.  If `blocks` already has one block, it
/// is returned unchanged via `Clone`.
pub fn movfuscate<C: MovfuscCtx>(
    mut ctx: C,
    blocks: &C::Blocks,
    state_slot_types: Vec<C::SlotTy>,
    return_slot_types: Vec<C::SlotTy>,
) -> C::Blocks {
    let n = C::num_blocks(blocks);
    assert!(n >= 1, "movfuscate: empty block list");
    if n == 1 {
        return blocks.clone();
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
    for i in 0..n {
        let is_active = ctx.emit_is_block(&pc_vars, i);
        let block_vals = ctx.emit_block_stmts(blocks, i, &state_vars);
        let term = ctx.emit_block_terminator(
            blocks,
            i,
            &block_vals,
            pc_width,
            &state_slot_types,
            &return_slot_types,
        );
        results.push(BlockResult {
            is_active,
            done: term.done,
            next_pc_bits: term.next_pc_bits,
            next_state: term.next_state,
            ret_vals: term.ret_vals,
        });
    }

    // ---- Accumulate across mutually-exclusive active bits ------------------
    //
    // Exactly one is_active_i = 1 per valid step.  Scalar-mult then field-add
    // selects the active block's contribution:
    //   result = Σ_i  (is_active_i · x_i)

    // done and PC: Bit accumulation
    let mut done_acc = bit_zero;
    for br in &results {
        let g = ctx.emit_and_bit(br.is_active, br.done);
        done_acc = ctx.emit_xor_bit(done_acc, g);
    }

    let mut next_pc = vec![bit_zero; pc_width];
    for br in &results {
        for j in 0..pc_width {
            let g = ctx.emit_and_bit(br.is_active, br.next_pc_bits[j]);
            next_pc[j] = ctx.emit_xor_bit(next_pc[j], g);
        }
    }

    // state and return: typed (field) accumulation
    let mut next_state: Vec<u32> = state_slot_types
        .iter()
        .map(|ty| ctx.emit_zero_slot(ty))
        .collect();
    for br in &results {
        for k in 0..state_width {
            let g = ctx.emit_gate(br.is_active, br.next_state[k], &state_slot_types[k]);
            next_state[k] = ctx.emit_field_add(next_state[k], g, &state_slot_types[k]);
        }
    }

    let mut ret_vals: Vec<u32> = return_slot_types
        .iter()
        .map(|ty| ctx.emit_zero_slot(ty))
        .collect();
    for br in &results {
        for m in 0..ret_width {
            let g = ctx.emit_gate(br.is_active, br.ret_vals[m], &return_slot_types[m]);
            ret_vals[m] = ctx.emit_field_add(ret_vals[m], g, &return_slot_types[m]);
        }
    }

    // loop_vars = [next_pc_bits…, next_state…]
    let mut loop_vars = Vec::with_capacity(combined_params);
    loop_vars.extend_from_slice(&next_pc);
    loop_vars.extend_from_slice(&next_state);

    ctx.build_output(combined_params, done_acc, loop_vars, ret_vals)
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
    }
}

struct BIrCtx<P: Clone + Default = ()> {
    stmts: Vec<BIrStmt>,
    stmt_provs: Vec<P>,
    next_id: u32,
}

impl<P: Clone + Default> BIrCtx<P> {
    fn new(first_id: u32) -> Self {
        Self { stmts: Vec::new(), stmt_provs: Vec::new(), next_id: first_id }
    }

    fn push(&mut self, stmt: BIrStmt, prov: P) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
        self.stmt_provs.push(prov);
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
        }
    }
}


impl<P: Clone + Default> MovfuscCtx for BIrCtx<P> {
    type Blocks = BIrBlocks<P>;
    /// All BIr slots are `Bit`; no type information needed.
    type SlotTy = ();

    fn num_blocks(blocks: &BIrBlocks<P>) -> usize {
        blocks.0.len()
    }

    fn block_param_count(blocks: &BIrBlocks<P>, i: usize) -> usize {
        blocks.0[i].params as usize
    }

    fn return_val_width(blocks: &BIrBlocks<P>) -> usize {
        for block in &blocks.0 {
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
        self.push(BIrStmt::Zero, P::default())
    }

    fn emit_one_bit(&mut self) -> u32 {
        self.push(BIrStmt::One, P::default())
    }

    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a; // idempotent
        }
        self.push(BIrStmt::And(IRVarId(a), IRVarId(b)), P::default())
    }

    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return self.push(BIrStmt::Zero, P::default());
        }
        self.push(BIrStmt::Xor(IRVarId(a), IRVarId(b)), P::default())
    }

    fn emit_not(&mut self, a: u32) -> u32 {
        self.push(BIrStmt::Not(IRVarId(a)), P::default())
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
    ) -> Vec<u32> {
        let block = &blocks.0[block_idx];
        let p = block.params as usize;
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());
        var_map.extend_from_slice(&state_vars[..p]);
        for (i, stmt) in block.stmts.iter().enumerate() {
            let prov = block.stmt_provs.get(i).cloned().unwrap_or_default();
            let mapped = subst_biir(stmt, &var_map);
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
        let block = &blocks.0[block_idx];
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
        }
    }

    fn build_output(
        self,
        combined_params: usize,
        done_var: u32,
        loop_vars: Vec<u32>,
        ret_vars: Vec<u32>,
    ) -> BIrBlocks<P> {
        BIrBlocks(vec![BIrBlock {
            params: combined_params as u32,
            stmts: self.stmts,
            stmt_provs: self.stmt_provs,
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
        }])
    }
}

// ============================================================================
// IRBlocks implementation  (SlotTy = IRTypeId — full field type support)
// ============================================================================

// ---- Substitution ----------------------------------------------------------

fn subst_ir(stmt: &IRStmt, var_map: &[u32]) -> IRStmt {
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
    }
}

// ---- Pre-pass: infer all variable types in a block (static, before emission)

fn infer_block_var_types<P: Clone + Default>(
    block: &IRBlock<P>,
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
) -> Vec<IRTypeId> {
    let mut var_types: Vec<IRTypeId> = block.params.clone();
    for stmt in &block.stmts {
        let ty = infer_stmt_result_type(stmt, &var_types, ir_types, bit_type_id);
        var_types.push(ty);
    }
    var_types
}

// ---- IrCtx -----------------------------------------------------------------

/// Compute the expanded state-slot layout for one block's params.
///
/// Each `Block`-typed param expands to `pc_width` `Bit` slots; every other
/// param occupies exactly one slot.  Returns `[(slot_start, slot_count), …]`
/// in original-param order.
fn param_to_slot_map(params: &[IRTypeId], ir_types: &[IRType], pc_width: usize) -> Vec<(usize, usize)> {
    let mut map = Vec::with_capacity(params.len());
    let mut offset = 0;
    for ty_id in params {
        let count = if matches!(ir_types[ty_id.0 as usize], IRType::Block { .. }) {
            pc_width
        } else {
            1
        };
        map.push((offset, count));
        offset += count;
    }
    map
}

struct IrCtx<P: Clone + Default = ()> {
    stmts: Vec<IRStmt>,
    stmt_provs: Vec<P>,
    /// Provenance to attach to the next emitted stmt (consumed on `push_typed`).
    /// Set to `P::default()` after each consumption, so synthetic stmts always
    /// carry a default provenance unless explicitly staged here first.
    pending_prov: P,
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
}

impl<P: Clone + Default> IrCtx<P> {
    fn new(
        first_id: u32,
        bit_type_id: IRTypeId,
        vec_pc_type_id: IRTypeId,
        combined_param_types: Vec<IRTypeId>,
        ir_types: Vec<IRType>,
        pc_width: usize,
    ) -> Self {
        let var_types = combined_param_types.clone();
        Self {
            stmts: Vec::new(),
            stmt_provs: Vec::new(),
            pending_prov: P::default(),
            next_id: first_id,
            bit_type_id,
            vec_pc_type_id,
            var_types,
            ir_types,
            combined_param_types,
            pc_width,
            block_var_to_bits: BTreeMap::new(),
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

        // Fill unset slots with typed zero constants.
        next_state
            .into_iter()
            .enumerate()
            .map(|(k, v)| {
                v.unwrap_or_else(|| {
                    let ty = state_slot_types[k].clone();
                    self.emit_zero_slot(&ty)
                })
            })
            .collect()
    }

    /// Build the `target_slot_map` for a Dyn jump target whose expected
    /// parameter types come from a `Block { params: sig }` type.
    fn slot_map_from_sig(&self, sig: &[IRTypeId]) -> Vec<(usize, usize)> {
        param_to_slot_map(sig, &self.ir_types, self.pc_width)
    }

    /// Emit a stmt and record its result type.
    ///
    /// Consumes `self.pending_prov` (resetting it to `P::default()`) so that
    /// source stmts staged via `pending_prov = prov` carry the right provenance,
    /// while all synthetic stmts automatically get `P::default()`.
    fn push_typed(&mut self, stmt: IRStmt, result_type: IRTypeId) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
        self.var_types.push(result_type);
        let prov = core::mem::replace(&mut self.pending_prov, P::default());
        self.stmt_provs.push(prov);
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
                let target_slot_map = param_to_slot_map(
                    &blocks.blocks[*j as usize].params,
                    &self.ir_types,
                    self.pc_width,
                );
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
        }
    }
}

impl<P: Clone + Default> MovfuscCtx for IrCtx<P> {
    type Blocks = IRBlocks<P>;
    type SlotTy = IRTypeId;

    fn num_blocks(blocks: &IRBlocks<P>) -> usize {
        blocks.blocks.len()
    }

    fn block_param_count(blocks: &IRBlocks<P>, i: usize) -> usize {
        blocks.blocks[i].params.len()
    }

    fn return_val_width(blocks: &IRBlocks<P>) -> usize {
        for block in &blocks.blocks {
            match &block.terminator {
                IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => {
                    return args.len()
                }
                IRTerminator::JumpCond {
                    true_block: IRBlockTargetId::Return,
                    true_args,
                    ..
                } => return true_args.len(),
                IRTerminator::JumpCond {
                    false_block: IRBlockTargetId::Return,
                    false_args,
                    ..
                } => return false_args.len(),
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
    ) -> Vec<u32> {
        let block = &blocks.blocks[block_idx];
        let p = block.params.len();
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());

        // Reset per-block Block-var tracking.
        self.block_var_to_bits.clear();

        // Map each original param to its combined-block state var(s).
        // Block-typed params expand to `pc_width` consecutive Bit state vars;
        // all other params are 1-to-1.
        let slot_map = param_to_slot_map(&block.params, &self.ir_types, self.pc_width);
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
            // Stage this stmt's source provenance; `push_typed` will consume it.
            self.pending_prov = block.stmt_provs.get(stmt_idx).cloned().unwrap_or_default();
            let mapped = subst_ir(stmt, &var_map);
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
                let orig_src_id = match stmt {
                    IRStmt::StorageWrite { src, .. } => src.0,
                    _ => unreachable!(),
                };
                if let Some((bits, _sig)) = self.block_var_to_bits.get(&orig_src_id).cloned() {
                    // Block-typed write: merge PC bits into Vec and store in even lane.
                    let parts: Vec<IRVarId> = bits.iter().map(|&b| IRVarId(b)).collect();
                    let merged = self.push_typed(
                        IRStmt::Merge { parts, ty: self.vec_pc_type_id },
                        self.vec_pc_type_id,
                    );
                    let new_storage = StorageId(storage.0 * 2);
                    let id = self.push_typed(
                        IRStmt::StorageWrite { storage: new_storage, src: IRVarId(merged), ty: self.vec_pc_type_id, addr: *addr },
                        self.bit_type_id,
                    );
                    var_map.push(id);
                    continue;
                } else {
                    // Non-block write: remap to odd storage lane.
                    let new_storage = StorageId(storage.0 * 2 + 1);
                    let res_ty = infer_stmt_result_type(&mapped, &self.var_types, &self.ir_types, &self.bit_type_id);
                    let id = self.push_typed(
                        IRStmt::StorageWrite { storage: new_storage, src: *src, ty: *ty, addr: *addr },
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
            IRTerminator::Jmp { func, args } => {
                let (done, next_pc_bits, next_state, ret_vals) = self.process_ir_target(
                    func,
                    args,
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
                true_block,
                true_args,
                false_block,
                false_args,
            } => {
                let cond = block_vals[condition.0 as usize];
                let (t_done, t_npc, t_ns, t_ret) = self.process_ir_target(
                    true_block,
                    true_args,
                    block_vals,
                    pc_width,
                    state_slot_types,
                    return_slot_types,
                    blocks,
                );
                let (e_done, e_npc, e_ns, e_ret) = self.process_ir_target(
                    false_block,
                    false_args,
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
            IRTerminator::JumpTable { .. } => {
                unimplemented!(
                    "movfuscate_ir: JumpTable terminators are not yet supported; \
                     lower to JumpCond chains before movfuscating"
                )
            }
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
            stmt_provs: self.stmt_provs,
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(done_var),
                true_block: IRBlockTargetId::Return,
                true_args: ret_vars.into_iter().map(IRVarId).collect(),
                false_block: IRBlockTargetId::Block(IRBlockId(0)),
                false_args: loop_vars.into_iter().map(IRVarId).collect(),
            },
        }])
    }
}

// ============================================================================
// Pre-pass helpers for IRBlocks entry point
// ============================================================================

/// Compute the expanded state-slot types across all blocks.
///
/// Each original param position `k` contributes one or more slots:
/// - `IRType::Block { .. }` → `pc_width` consecutive `Bit` slots.
/// - Any other type           → one slot of that type.
///
/// All blocks that have a param at position `k` must agree on expansion size.
/// Non-`Block` types must additionally be identical.
/// Mixing `Block` and plain `Bit` is allowed when `pc_width == 1` (both
/// expand to exactly one `Bit` slot).
fn compute_expanded_state_slot_types<P: Clone + Default>(
    blocks: &IRBlocks<P>,
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
    pc_width: usize,
) -> Vec<IRTypeId> {
    let max_param_count = blocks.blocks.iter().map(|b| b.params.len()).max().unwrap_or(0);
    let mut result = Vec::new();

    for k in 0..max_param_count {
        let mut agreed_count: Option<usize> = None;
        let mut non_block_ty: Option<IRTypeId> = None;
        let mut any_block = false;

        for (bi, block) in blocks.blocks.iter().enumerate() {
            if k >= block.params.len() {
                continue;
            }
            let ty_id = &block.params[k];
            let (count, is_block) =
                if matches!(ir_types[ty_id.0 as usize], IRType::Block { .. }) {
                    (pc_width, true)
                } else {
                    (1, false)
                };

            if let Some(prev) = agreed_count {
                assert_eq!(
                    prev, count,
                    "movfuscate_ir: Block-type expansion size mismatch at param {k} \
                     in block {bi}: earlier blocks expand to {prev} slot(s) but \
                     this block expands to {count}"
                );
            } else {
                agreed_count = Some(count);
            }

            if is_block {
                any_block = true;
            } else if let Some(ref existing) = non_block_ty {
                assert_eq!(
                    existing.0, ty_id.0,
                    "movfuscate_ir: block {bi} has type {:?} at param {k}, \
                     but an earlier block had a different non-Block type there",
                    ir_types[ty_id.0 as usize]
                );
            } else {
                non_block_ty = Some(ty_id.clone());
            }
        }

        let count = agreed_count.expect("param position without any owning block");
        if any_block {
            // Block expansion: all slots are Bit.
            for _ in 0..count {
                result.push(bit_type_id.clone());
            }
        } else {
            result.push(non_block_ty.expect("non-Block param without concrete type"));
        }
    }

    result
}

/// Infer the expanded types of the return values by scanning the first
/// `Return` terminator found in the module.
///
/// `Block`-typed return values are expanded to `pc_width` `Bit` slots each.
fn compute_return_slot_types<P: Clone + Default>(
    blocks: &IRBlocks<P>,
    ir_types: &[IRType],
    bit_type_id: &IRTypeId,
    pc_width: usize,
) -> Vec<IRTypeId> {
    for block in &blocks.blocks {
        let var_types = infer_block_var_types(block, ir_types, bit_type_id);
        let ret_args: Option<&Vec<IRVarId>> = match &block.terminator {
            IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => Some(args),
            IRTerminator::JumpCond {
                true_block: IRBlockTargetId::Return, true_args, ..
            } => Some(true_args),
            IRTerminator::JumpCond {
                false_block: IRBlockTargetId::Return, false_args, ..
            } => Some(false_args),
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
/// Source statement provenances are carried through; synthetic dispatch gates
/// receive `P::default()`.
pub fn movfuscate_biir<P: Clone + Default>(blocks: &BIrBlocks<P>) -> BIrBlocks<P> {
    let n = blocks.0.len();
    let pc_width = pc_bits_needed(n);
    let state_width = blocks.0.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let combined_params = pc_width + state_width;
    let ctx = BIrCtx::<P>::new(combined_params as u32);
    let state_slot_types = vec![(); state_width];
    let ret_width = BIrCtx::<P>::return_val_width(blocks);
    let return_slot_types = vec![(); ret_width];
    movfuscate(ctx, blocks, state_slot_types, return_slot_types)
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
pub fn movfuscate_ir<P: Clone + Default>(blocks: &IRBlocks<P>, types: &mut IRTypes) -> IRBlocks<P> {
    // Ensure IRType::Bit is present in the types table.
    let bit_type_id = types.intern(IRType::Primitive(Type::Bit));

    // Intern Vec(pc_width, Bit) for block-reference storage.
    let n = blocks.blocks.len();
    let pc_width = pc_bits_needed(n);
    let vec_pc_type_id = if pc_width > 0 {
        types.intern(IRType::Vec(pc_width, bit_type_id))
    } else {
        bit_type_id
    };

    let ir_types = types.0.clone();

    // Compute per-slot types from the original blocks, expanding Block params.
    let state_slot_types =
        compute_expanded_state_slot_types(blocks, &ir_types, &bit_type_id, pc_width);
    let return_slot_types =
        compute_return_slot_types(blocks, &ir_types, &bit_type_id, pc_width);

    // Combined block param types: [Bit×pc_width] ++ expanded state_slot_types
    let combined_param_types: Vec<IRTypeId> = (0..pc_width)
        .map(|_| bit_type_id.clone())
        .chain(state_slot_types.iter().cloned())
        .collect();

    let combined_params = pc_width + state_slot_types.len();
    let ctx = IrCtx::<P>::new(
        combined_params as u32,
        bit_type_id,
        vec_pc_type_id,
        combined_param_types,
        ir_types,
        pc_width,
    );
    movfuscate(ctx, blocks, state_slot_types, return_slot_types)
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
        IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId,
        IRTypes, IRVarId,
    };
    use volar_ir_common::Constant;

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
        BIrBlocks(std::vec![
            BIrBlock {
                params: 1,
                stmts: std::vec![BIrStmt::Not(IRVarId(0))],
                stmt_provs: std::vec![()],
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
                stmt_provs: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                }),
            },
        ])
    }

    #[test]
    fn test_biir_single_block_passthrough() {
        let single = BIrBlocks(std::vec![BIrBlock {
            params: 2,
            stmts: std::vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
            stmt_provs: std::vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: std::vec![IRVarId(2)],
            }),
        }]);
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
        assert_eq!(result.0[0].params, 2); // pc(1) + state(1)
    }

    #[test]
    fn test_biir_two_block_dag_terminator_shape() {
        let result = movfuscate_biir(&two_block_dag());
        match &result.0[0].terminator {
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
        match &movfuscate_biir(&two_block_dag()).0[0].terminator {
            BIrTerminator::CondJmp { then_target, .. } => {
                assert_eq!(then_target.args.len(), 1);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_biir_three_block_chain() {
        let blocks = BIrBlocks(std::vec![
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                }),
            },
            BIrBlock {
                params: 1,
                stmts: std::vec![BIrStmt::Not(IRVarId(0))],
                stmt_provs: std::vec![()],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(2)),
                    args: std::vec![IRVarId(1)],
                }),
            },
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                }),
            },
        ]);
        let result = movfuscate_biir(&blocks);
        assert!(result.is_movfuscated());
        assert_eq!(result.0[0].params, 3); // pc(2) + state(1)
        match &result.0[0].terminator {
            BIrTerminator::CondJmp { then_target, else_target, .. } => {
                assert_eq!(then_target.block, IRBlockTargetId::Return);
                assert_eq!(else_target.args.len(), 3);
            }
            _ => panic!("expected CondJmp"),
        }
    }

    #[test]
    fn test_biir_self_loop_passthrough() {
        let blocks = BIrBlocks(std::vec![BIrBlock {
            params: 1,
            stmts: std::vec![BIrStmt::Not(IRVarId(0))],
            stmt_provs: std::vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(0)),
                args: std::vec![IRVarId(1)],
            }),
        }]);
        let result = movfuscate_biir(&blocks);
        assert_eq!(result, blocks);
    }

    #[test]
    fn test_biir_four_block_pc_width() {
        let make_pass = |dst: u32| BIrBlock::<()> {
            params: 1,
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(dst)),
                args: std::vec![IRVarId(0)],
            }),
        };
        let blocks = BIrBlocks(std::vec![
            make_pass(1),
            make_pass(2),
            make_pass(3),
            BIrBlock {
                params: 1,
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                }),
            },
        ]);
        let result = movfuscate_biir(&blocks);
        assert!(result.is_movfuscated());
        assert_eq!(result.0[0].params, 3); // pc(2) + state(1)
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
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                },
            },
            IRBlock {
                params: std::vec![IRTypeId(0)],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
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
            stmt_provs: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
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
                true_block, true_args, false_block, false_args, ..
            } => {
                assert_eq!(*true_block, IRBlockTargetId::Return);
                assert_eq!(true_args.len(), 1);
                assert_eq!(*false_block, IRBlockTargetId::Block(IRBlockId(0)));
                assert_eq!(false_args.len(), 2);
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
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
        let blocks = IRBlocks::new(std::vec![
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
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
            IRTerminator::JumpCond { true_args, false_args, .. } => {
                assert_eq!(true_args.len(), 1, "ret_width = 1 (one G8 value)");
                assert_eq!(false_args.len(), 2, "loop-back = pc(1) + state(1)");
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
            .any(|s| matches!(s, IRStmt::Poly { .. }));
        assert!(has_poly, "combined block must contain Poly stmts for field dispatch");
    }

    /// Verify that gate Poly stmts for G8 slots are degree-2 (two-var monomial).
    #[test]
    fn test_ir_g8_gate_poly_is_degree2() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        // Find at least one degree-2 Poly (the gate op: is_active * val).
        let degree2 = result.blocks[0].stmts.iter().any(|s| {
            if let IRStmt::Poly { coeffs, .. } = s {
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
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::JumpCond {
                    condition: IRVarId(1), // b
                    true_block: IRBlockTargetId::Block(IRBlockId(1)),
                    true_args: std::vec![IRVarId(0)], // a
                    false_block: IRBlockTargetId::Return,
                    false_args: std::vec![IRVarId(0)], // a
                },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
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
            IRTerminator::JumpCond { true_args, false_args, .. } => {
                assert_eq!(true_args.len(), 1, "ret_width = 1 (G8)");
                assert_eq!(false_args.len(), 3, "loop-back = pc(1) + state(2)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
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
                }],
                stmt_provs: std::vec![()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(1)], // the Poly result
                },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
            },
        ]);
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // Must contain at least the re-emitted Poly from block 0's body.
        let poly_count = result.blocks[0]
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::Poly { .. }))
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
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(IRVarId(0)), // cont
                    args: std::vec![],
                },
            },
            // Block 1: Return
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![],
                },
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
        // pc_width=1 (2 blocks), state_width=1 (Block expands to 1 Bit slot)
        // → combined params = 2
        assert_eq!(result.blocks[0].params.len(), 2, "pc(1) + state(1 Bit for Block)");
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
            IRTerminator::JumpCond { true_block, true_args, false_block, false_args, .. } => {
                assert_eq!(*true_block, IRBlockTargetId::Return);
                assert_eq!(true_args.len(), 0, "ret_width = 0");
                assert_eq!(*false_block, IRBlockTargetId::Block(IRBlockId(0)));
                assert_eq!(false_args.len(), 2, "loop-back args = pc(1) + state(1)");
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
                ],
                stmt_provs: std::vec![()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(IRVarId(0)), // c (stmt result)
                    args: std::vec![],
                },
            },
            IRBlock {
                params: std::vec![],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![],
                },
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
        for stmt in &result.blocks[0].stmts {
            if let IRStmt::Const(_, ty_id) = stmt {
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
                ],
                stmt_provs: std::vec![(), ()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(IRVarId(0)), // c
                    args: std::vec![IRVarId(1)],            // one
                },
            },
            // Block 1: take x: Bit and return it.
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
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
            IRTerminator::JumpCond { true_args, false_args, .. } => {
                assert_eq!(true_args.len(), 1, "return one Bit value");
                assert_eq!(false_args.len(), 2, "loop-back = pc(1) + state(1)");
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
                ],
                stmt_provs: std::vec![()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)], // pass the Block ref
                },
            },
            // Block 1: holds a cont and Dyn-jumps to it.
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(IRVarId(0)),
                    args: std::vec![],
                },
            },
            // Block 2: the target of the continuation.
            IRBlock {
                params: std::vec![block_ty_id.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![],
                },
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
        // state_width = max expanded param count:
        //   block 0: 0 params → 0 expanded slots
        //   block 1: 1 Block param → 2 Bit slots
        //   block 2: 1 Block param → 2 Bit slots
        // → state_width = 2 Bit slots.
        // Combined params: [pc0: Bit, pc1: Bit, state0: Bit, state1: Bit] = 4.
        assert_eq!(result.blocks[0].params.len(), 4, "pc(2) + state(2 Bit for Block)");
        for tid in &result.blocks[0].params {
            assert!(matches!(types.0[tid.0 as usize], IRType::Primitive(Type::Bit)));
        }
    }

    #[test]
    fn test_ir_block_pass_arg_terminator_shape() {
        let (blocks, mut types) = three_block_pass_block_arg();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.blocks[0].terminator {
            IRTerminator::JumpCond { true_args, false_args, .. } => {
                assert_eq!(true_args.len(), 0, "ret_width = 0");
                assert_eq!(false_args.len(), 4, "loop-back = pc(2) + state(2)");
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
                }],
                stmt_provs: std::vec![()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                },
            },
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
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
        let has_write = result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::StorageWrite { storage, .. } if *storage == odd_lane)
        });
        assert!(has_write, "non-Block StorageWrite must be remapped to odd lane 11");
        // Verify the original even ID 10 does not appear.
        let no_even = !result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::StorageWrite { storage, .. } if *storage == StorageId(5 * 2))
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
                }],
                stmt_provs: std::vec![()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(1)], // the read result
                },
            },
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
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
        let has_read = result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::StorageRead { storage, .. } if *storage == odd_lane)
        });
        assert!(has_read, "non-Block StorageRead must be remapped to odd lane 7");
        let no_even = !result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::StorageRead { storage, .. } if *storage == StorageId(3 * 2))
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
                ],
                stmt_provs: std::vec![(), ()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(IRVarId(1)), // c
                    args: std::vec![],
                },
            },
            IRBlock {
                params: std::vec![block_ty.clone()],
                stmts: std::vec![],
                stmt_provs: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![],
                },
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
        let has_even_write = result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::StorageWrite { storage, .. } if *storage == even_lane)
        });
        assert!(has_even_write, "Block-typed StorageWrite must use even lane 4");
        // Odd lane must not appear for this write.
        let no_odd = !result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::StorageWrite { storage, .. } if *storage == StorageId(2 * 2 + 1))
        });
        assert!(no_odd, "odd lane must not be used for Block-typed write");
    }

    #[test]
    fn test_storage_write_block_emits_merge_stmt() {
        let (blocks, mut types) = two_block_block_storage_write();
        let result = movfuscate_ir(&blocks, &mut types);
        // A Merge stmt must appear to pack the Block's PC bits into a Vec.
        let has_merge = result.blocks[0].stmts.iter().any(|s| {
            matches!(s, IRStmt::Merge { .. })
        });
        assert!(has_merge, "Block-typed StorageWrite must emit a Merge stmt to pack PC bits");
    }
}
