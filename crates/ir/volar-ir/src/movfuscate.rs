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
use volar_ir_common::Constant;

use crate::{
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

    /// `select(cond, a, b) = AND(cond, XOR(a, b)) XOR b` — all Bit operands.
    fn emit_select_bit(&mut self, cond: u32, a: u32, b: u32) -> u32 {
        let xab = self.emit_xor_bit(a, b);
        let sel = self.emit_and_bit(cond, xab);
        self.emit_xor_bit(sel, b)
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
        let xab = self.emit_field_add(a, b, ty);
        let sel = self.emit_gate(cond, xab, ty);
        self.emit_field_add(sel, b, ty)
    }

    /// Emit `is_active` for `block_idx`: AND-of-(NOT-or-identity) per PC bit.
    fn emit_is_block(&mut self, pc_vars: &[u32], block_idx: usize) -> u32 {
        if pc_vars.is_empty() {
            return self.emit_one_bit();
        }
        let bit0 = if (block_idx & 1) == 1 {
            pc_vars[0]
        } else {
            self.emit_not(pc_vars[0])
        };
        let mut acc = bit0;
        for j in 1..pc_vars.len() {
            let bitj = if (block_idx >> j) & 1 == 1 {
                pc_vars[j]
            } else {
                self.emit_not(pc_vars[j])
            };
            acc = self.emit_and_bit(acc, bitj);
        }
        acc
    }

    /// Emit constant-bit vars for `block_idx` in `pc_width` bits (LSB-first).
    fn encode_pc_bits(&mut self, block_idx: usize, pc_width: usize) -> Vec<u32> {
        (0..pc_width)
            .map(|j| {
                if (block_idx >> j) & 1 == 1 {
                    self.emit_one_bit()
                } else {
                    self.emit_zero_bit()
                }
            })
            .collect()
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
    }
}

struct BIrCtx {
    stmts: Vec<BIrStmt>,
    next_id: u32,
}

impl BIrCtx {
    fn new(first_id: u32) -> Self {
        Self { stmts: Vec::new(), next_id: first_id }
    }

    fn push(&mut self, stmt: BIrStmt) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
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

impl MovfuscCtx for BIrCtx {
    type Blocks = BIrBlocks;
    /// All BIr slots are `Bit`; no type information needed.
    type SlotTy = ();

    fn num_blocks(blocks: &BIrBlocks) -> usize {
        blocks.0.len()
    }

    fn block_param_count(blocks: &BIrBlocks, i: usize) -> usize {
        blocks.0[i].params as usize
    }

    fn return_val_width(blocks: &BIrBlocks) -> usize {
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
        self.push(BIrStmt::Zero)
    }

    fn emit_one_bit(&mut self) -> u32 {
        self.push(BIrStmt::One)
    }

    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a; // idempotent
        }
        self.push(BIrStmt::And(IRVarId(a), IRVarId(b)))
    }

    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return self.push(BIrStmt::Zero);
        }
        self.push(BIrStmt::Xor(IRVarId(a), IRVarId(b)))
    }

    fn emit_not(&mut self, a: u32) -> u32 {
        self.push(BIrStmt::Not(IRVarId(a)))
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
        blocks: &BIrBlocks,
        block_idx: usize,
        state_vars: &[u32],
    ) -> Vec<u32> {
        let block = &blocks.0[block_idx];
        let p = block.params as usize;
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());
        var_map.extend_from_slice(&state_vars[..p]);
        for stmt in &block.stmts {
            let mapped = subst_biir(stmt, &var_map);
            let id = self.push(mapped);
            var_map.push(id);
        }
        var_map
    }

    fn emit_block_terminator(
        &mut self,
        blocks: &BIrBlocks,
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
    ) -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
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
        IRStmt::StorageRead { ty, addr } => IRStmt::StorageRead { ty: ty.clone(), addr: s(addr) },
        IRStmt::StorageWrite { src, ty, addr } => {
            IRStmt::StorageWrite { src: s(src), ty: ty.clone(), addr: s(addr) }
        }
        IRStmt::Const(c, ty) => IRStmt::Const(*c, ty.clone()),
        IRStmt::Transmute { src, src_ty, dst_ty } => {
            IRStmt::Transmute { src: s(src), src_ty: src_ty.clone(), dst_ty: dst_ty.clone() }
        }
        IRStmt::Poly { coeffs, constant } => IRStmt::Poly {
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
    }
}

// ---- Type inference --------------------------------------------------------

/// Find the "wider" of two type IDs, treating `Bit` as the additive identity
/// for promotion (`Bit` × `T` = `T`).
///
/// Panics if two non-Bit types are incompatible (different non-Bit type IDs).
fn promote_type(a: &IRTypeId, b: &IRTypeId, ir_types: &[IRType]) -> IRTypeId {
    let a_is_bit = matches!(ir_types[a.0 as usize], IRType::Bit);
    let b_is_bit = matches!(ir_types[b.0 as usize], IRType::Bit);
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
    }
}

// ---- Pre-pass: infer all variable types in a block (static, before emission)

fn infer_block_var_types(
    block: &IRBlock,
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

struct IrCtx {
    stmts: Vec<IRStmt>,
    next_id: u32,
    bit_type_id: IRTypeId,
    /// Types of all vars emitted so far (params + stmts).
    var_types: Vec<IRTypeId>,
    /// Clone of the types table used for type inference.
    ir_types: Vec<IRType>,
    /// Param types of the combined block (for build_output).
    combined_param_types: Vec<IRTypeId>,
}

impl IrCtx {
    fn new(
        first_id: u32,
        bit_type_id: IRTypeId,
        combined_param_types: Vec<IRTypeId>,
        ir_types: Vec<IRType>,
    ) -> Self {
        let var_types = combined_param_types.clone();
        Self {
            stmts: Vec::new(),
            next_id: first_id,
            bit_type_id,
            var_types,
            ir_types,
            combined_param_types,
        }
    }

    /// Emit a stmt and record its result type.
    fn push_typed(&mut self, stmt: IRStmt, result_type: IRTypeId) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
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
            IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: constant_lo } },
            result_type,
        )
    }

    /// Decompose a single `IRBlockTargetId + args` for the dispatch table.
    fn process_ir_target(
        &mut self,
        target_block: &IRBlockTargetId,
        args: &[IRVarId],
        block_vals: &[u32],
        pc_width: usize,
        state_slot_types: &[IRTypeId],
        return_slot_types: &[IRTypeId],
    ) -> (u32, Vec<u32>, Vec<u32>, Vec<u32>) {
        let state_width = state_slot_types.len();
        let ret_width = return_slot_types.len();
        let lookup = |id: &IRVarId| block_vals[id.0 as usize];
        match target_block {
            IRBlockTargetId::Return => {
                let done = self.emit_one_bit();
                let next_pc = self.encode_pc_bits(0, pc_width);
                let next_state = state_slot_types
                    .iter()
                    .map(|ty| self.emit_zero_slot(ty))
                    .collect();
                let mut ret: Vec<u32> = args.iter().map(lookup).collect();
                for m in ret.len()..ret_width {
                    ret.push(self.emit_zero_slot(&return_slot_types[m]));
                }
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Block(IRBlockId(j)) => {
                let done = self.emit_zero_bit();
                let next_pc = self.encode_pc_bits(*j as usize, pc_width);
                let mut next_state: Vec<u32> = args.iter().map(lookup).collect();
                for k in next_state.len()..state_width {
                    next_state.push(self.emit_zero_slot(&state_slot_types[k]));
                }
                let ret =
                    return_slot_types.iter().map(|ty| self.emit_zero_slot(ty)).collect();
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Dyn(_) => {
                panic!("movfuscate_ir: Dyn jump targets are not supported")
            }
        }
    }
}

impl MovfuscCtx for IrCtx {
    type Blocks = IRBlocks;
    type SlotTy = IRTypeId;

    fn num_blocks(blocks: &IRBlocks) -> usize {
        blocks.0.len()
    }

    fn block_param_count(blocks: &IRBlocks, i: usize) -> usize {
        blocks.0[i].params.len()
    }

    fn return_val_width(blocks: &IRBlocks) -> usize {
        for block in &blocks.0 {
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
            IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 0 } },
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
            IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 0 } },
            t,
        )
    }

    // Block processing -------------------------------------------------------

    fn emit_block_stmts(
        &mut self,
        blocks: &IRBlocks,
        block_idx: usize,
        state_vars: &[u32],
    ) -> Vec<u32> {
        let block = &blocks.0[block_idx];
        let p = block.params.len();
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());
        var_map.extend_from_slice(&state_vars[..p]);
        for stmt in &block.stmts {
            let mapped = subst_ir(stmt, &var_map);
            // Infer result type using the running var_types table.
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
        blocks: &IRBlocks,
        block_idx: usize,
        block_vals: &[u32],
        pc_width: usize,
        state_slot_types: &[IRTypeId],
        return_slot_types: &[IRTypeId],
    ) -> TermResult {
        let block = &blocks.0[block_idx];
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
                );
                let (e_done, e_npc, e_ns, e_ret) = self.process_ir_target(
                    false_block,
                    false_args,
                    block_vals,
                    pc_width,
                    state_slot_types,
                    return_slot_types,
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
    ) -> IRBlocks {
        IRBlocks(vec![IRBlock {
            // Combined block's params: [Bit×pc_width, state_slot_types…]
            params: self.combined_param_types,
            stmts: self.stmts,
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

/// Compute the type of each state slot across all blocks.
///
/// Slot `k`'s type is taken from the first block that has a param at
/// position `k`; all other blocks with a param at `k` must agree.
fn compute_state_slot_types(blocks: &IRBlocks) -> Vec<IRTypeId> {
    let state_width =
        blocks.0.iter().map(|b| b.params.len()).max().unwrap_or(0);
    (0..state_width)
        .map(|k| {
            let mut slot_ty: Option<IRTypeId> = None;
            for (bi, block) in blocks.0.iter().enumerate() {
                if k < block.params.len() {
                    let ty = block.params[k].clone();
                    if let Some(ref existing) = slot_ty {
                        assert_eq!(
                            existing.0, ty.0,
                            "movfuscate_ir: block {bi} has type {:?} at param {k}, \
                             but an earlier block had a different type there",
                            ty
                        );
                    } else {
                        slot_ty = Some(ty);
                    }
                }
            }
            slot_ty.expect("state slot without any owning block")
        })
        .collect()
}

/// Infer the types of the return values by scanning the first `Return`
/// terminator found in the module.
fn compute_return_slot_types(blocks: &IRBlocks, ir_types: &[IRType], bit_type_id: &IRTypeId) -> Vec<IRTypeId> {
    for block in &blocks.0 {
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
            return args.iter().map(|id| var_types[id.0 as usize].clone()).collect();
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
pub fn movfuscate_biir(blocks: &BIrBlocks) -> BIrBlocks {
    let n = blocks.0.len();
    let pc_width = pc_bits_needed(n);
    let state_width = blocks.0.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let combined_params = pc_width + state_width;
    let ctx = BIrCtx::new(combined_params as u32);
    let state_slot_types = vec![(); state_width];
    let ret_width = BIrCtx::return_val_width(blocks);
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
pub fn movfuscate_ir(blocks: &IRBlocks, types: &mut IRTypes) -> IRBlocks {
    // Ensure IRType::Bit is present in the types table.
    let bit_type_id = types
        .0
        .iter()
        .position(|t| matches!(t, IRType::Bit))
        .map(|i| IRTypeId(i as u32))
        .unwrap_or_else(|| {
            let id = types.0.len();
            types.0.push(IRType::Bit);
            IRTypeId(id as u32)
        });

    let ir_types = types.0.clone();

    // Compute per-slot types from the original blocks.
    let state_slot_types = compute_state_slot_types(blocks);
    let return_slot_types =
        compute_return_slot_types(blocks, &ir_types, &bit_type_id);

    // Combined block param types: [Bit×pc_width] ++ state_slot_types
    let n = blocks.0.len();
    let pc_width = pc_bits_needed(n);
    let combined_param_types: Vec<IRTypeId> = (0..pc_width)
        .map(|_| bit_type_id.clone())
        .chain(state_slot_types.iter().cloned())
        .collect();

    let combined_params = pc_width + state_slot_types.len();
    let ctx = IrCtx::new(
        combined_params as u32,
        bit_type_id,
        combined_param_types,
        ir_types,
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
    use crate::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
    use crate::ir::{
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
        ])
    }

    #[test]
    fn test_biir_single_block_passthrough() {
        let single = BIrBlocks(std::vec![BIrBlock {
            params: 2,
            stmts: std::vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
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
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                }),
            },
            BIrBlock {
                params: 1,
                stmts: std::vec![BIrStmt::Not(IRVarId(0))],
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
        let make_pass = |dst: u32| BIrBlock {
            params: 1,
            stmts: std::vec![],
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
        IRTypes(std::vec![IRType::Bit])
    }

    fn two_block_ir_bit() -> (IRBlocks, IRTypes) {
        let types = bit_types();
        let blocks = IRBlocks(std::vec![
            IRBlock {
                params: std::vec![IRTypeId(0)],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                },
            },
            IRBlock {
                params: std::vec![IRTypeId(0)],
                stmts: std::vec![],
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
        let blocks = IRBlocks(std::vec![IRBlock {
            params: std::vec![IRTypeId(0)],
            stmts: std::vec![],
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
        assert_eq!(result.0[0].params.len(), 2); // pc(1) + state(1)
        for tid in &result.0[0].params {
            assert!(matches!(types.0[tid.0 as usize], IRType::Bit));
        }
    }

    #[test]
    fn test_ir_bit_two_block_terminator_shape() {
        let (blocks, mut types) = two_block_ir_bit();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.0[0].terminator {
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
        let types = IRTypes(std::vec![IRType::Bit, IRType::Galois8AES]);
        let g8 = IRTypeId(1);
        let blocks = IRBlocks(std::vec![
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(0)],
                },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
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
        assert_eq!(result.0[0].params.len(), 2);
        // First param (PC) must be Bit
        assert!(
            matches!(types.0[result.0[0].params[0].0 as usize], IRType::Bit),
            "PC param must be Bit"
        );
        // Second param (state slot 0) must be Galois8AES
        assert!(
            matches!(types.0[result.0[0].params[1].0 as usize], IRType::Galois8AES),
            "state slot 0 must be Galois8AES"
        );
    }

    #[test]
    fn test_ir_g8_terminator_shape() {
        let (blocks, mut types) = two_block_ir_g8();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.0[0].terminator {
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
        let has_poly = result.0[0]
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
        let degree2 = result.0[0].stmts.iter().any(|s| {
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
        let mut types = IRTypes(std::vec![IRType::Bit, IRType::Galois8AES]);
        let bit = IRTypeId(0);
        let g8 = IRTypeId(1);

        let blocks = IRBlocks(std::vec![
            IRBlock {
                params: std::vec![g8.clone(), bit.clone()],
                stmts: std::vec![],
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
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
            },
        ]);

        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // pc_width=1, state_width=2 (block 0 has 2 params: G8 at slot 0, Bit at slot 1)
        assert_eq!(result.0[0].params.len(), 3, "pc(1) + state(2)");

        // Param types: [Bit (PC), G8 (slot 0), Bit (slot 1)]
        let p = &result.0[0].params;
        assert!(matches!(types.0[p[0].0 as usize], IRType::Bit), "PC slot");
        assert!(matches!(types.0[p[1].0 as usize], IRType::Galois8AES), "state slot 0");
        assert!(matches!(types.0[p[2].0 as usize], IRType::Bit), "state slot 1");

        match &result.0[0].terminator {
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
        let mut types = IRTypes(std::vec![IRType::Bit, IRType::Galois8AES]);
        let g8 = IRTypeId(1);
        let mut coeffs = BTreeMap::new();
        coeffs.insert(std::vec![IRVarId(0)], 1u8);
        let blocks = IRBlocks(std::vec![
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![IRStmt::Poly {
                    coeffs,
                    constant: Constant { hi: 0, lo: 0 },
                }],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(1)], // the Poly result
                },
            },
            IRBlock {
                params: std::vec![g8.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
            },
        ]);
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // Must contain at least the re-emitted Poly from block 0's body.
        let poly_count = result.0[0]
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::Poly { .. }))
            .count();
        assert!(poly_count >= 1, "re-emitted Poly should appear in combined block");
    }
}
