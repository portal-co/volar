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
//! The caller starts execution by supplying `[0; k] ++ original_inputs`.
//!
//! # Dispatch
//!
//! For each original block `i`, the combined block:
//!
//! 1. Decodes `is_active_i = (pc_bits == binary(i))` via AND / NOT.
//! 2. Re-emits block `i`'s stmts with `state_vars[0..params_i]` as params.
//! 3. Evaluates block `i`'s terminator →
//!    `(done_i, next_pc_i, next_state_i, ret_i)`.
//!
//! Because exactly one `is_active_i = 1` per execution step, outputs are
//! XOR-accumulated (XOR == OR over mutually exclusive terms):
//!
//! ```text
//! done          = XOR_i (is_active_i AND done_i)
//! next_pc[j]    = XOR_i (is_active_i AND next_pc_i[j])   ∀ j < k
//! next_state[s] = XOR_i (is_active_i AND next_state_i[s]) ∀ s < w
//! ret[m]        = XOR_i (is_active_i AND ret_i[m])        ∀ m < ret_width
//! ```
//!
//! # Terminator of the combined block
//!
//! ```text
//! CondJmp(done, Return(ret), Block(0, [next_pc…, next_state…]))
//! ```

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
/// Returns `0` for `n ≤ 1` (single block; no PC needed).
pub fn pc_bits_needed(n: usize) -> usize {
    if n <= 1 {
        return 0;
    }
    // ceil(log2(n)) = bits needed to represent n-1
    (usize::BITS - (n - 1).leading_zeros()) as usize
}

// ============================================================================
// Per-block terminator result
// ============================================================================

/// Decomposed result of one original block's terminator in the combined block.
pub struct TermResult {
    /// `1` if this block exits (returns), `0` if it continues looping.
    pub done: u32,
    /// Binary-encoded PC for the next block — `pc_width` elements (LSB-first).
    /// Semantically don't-care when `done = 1`.
    pub next_pc_bits: Vec<u32>,
    /// State to carry into the next iteration — `state_width` elements.
    /// Semantically don't-care when `done = 1`.
    pub next_state: Vec<u32>,
    /// Return values — `ret_width` elements.
    /// Semantically don't-care when `done = 0`.
    pub ret_vals: Vec<u32>,
}

// ============================================================================
// Generic trait
// ============================================================================

/// Abstraction over Boolar IR and Volar IR for the movfuscation algorithm.
///
/// An implementor maintains:
/// - An internal statement accumulator (the growing stmt list).
/// - A var-ID counter starting immediately after the combined block's
///   parameter count.
///
/// All variable IDs are plain `u32`; IR-specific wrapper types (`IRVarId`
/// etc.) are applied only in [`build_output`](MovfuscCtx::build_output).
pub trait MovfuscCtx {
    /// The blocks collection type (input and output).
    type Blocks: Clone;

    // ---- Structure queries (no receiver needed) ----------------------------

    fn num_blocks(blocks: &Self::Blocks) -> usize;
    fn block_param_count(blocks: &Self::Blocks, i: usize) -> usize;
    /// Element count of the return tuple common to all `Return` terminators.
    /// Returns `0` when the module has no `Return` terminators.
    fn return_val_width(blocks: &Self::Blocks) -> usize;

    // ---- Primitive boolean emission ----------------------------------------

    fn emit_zero(&mut self) -> u32;
    fn emit_one(&mut self) -> u32;
    fn emit_and(&mut self, a: u32, b: u32) -> u32;
    fn emit_xor(&mut self, a: u32, b: u32) -> u32;
    fn emit_not(&mut self, a: u32) -> u32;

    // ---- IR-specific block processing --------------------------------------

    /// Re-emit the stmts of block `block_idx`, mapping its params to
    /// `state_vars[0..block_param_count(blocks, block_idx)]`.
    ///
    /// Returns the full var table `[remapped_params…, fresh_stmt_vars…]`
    /// so terminators can look up any original SSA id.
    fn emit_block_stmts(
        &mut self,
        blocks: &Self::Blocks,
        block_idx: usize,
        state_vars: &[u32],
    ) -> Vec<u32>;

    /// Analyse block `block_idx`'s terminator and emit any helper
    /// computations needed to compute the dispatch outputs.
    fn emit_block_terminator(
        &mut self,
        blocks: &Self::Blocks,
        block_idx: usize,
        block_vals: &[u32],
        pc_width: usize,
        state_width: usize,
        ret_width: usize,
    ) -> TermResult;

    /// Consume the context and assemble the final single-block module.
    ///
    /// - `combined_params` — `pc_width + state_width`.
    /// - `done_var`  — var that is `1` when execution should return.
    /// - `loop_vars` — `[next_pc_bits…, next_state…]` forwarded to `Block(0)`.
    /// - `ret_vars`  — values to return when `done_var = 1`.
    fn build_output(
        self,
        combined_params: usize,
        done_var: u32,
        loop_vars: Vec<u32>,
        ret_vars: Vec<u32>,
    ) -> Self::Blocks;

    // ---- Default implementations (IR-agnostic) ----------------------------

    /// `select(cond, a, b) = AND(cond, XOR(a, b)) XOR b`
    ///
    /// Cost: 1 AND + 2 XOR.
    fn emit_select(&mut self, cond: u32, a: u32, b: u32) -> u32 {
        let xab = self.emit_xor(a, b);
        let sel = self.emit_and(cond, xab);
        self.emit_xor(sel, b)
    }

    /// Emit `is_active` for `block_idx`: checks that `pc_vars` (binary,
    /// LSB-first) equals `block_idx`.
    ///
    /// Cost: at most `2·pc_width − 1` gates.
    fn emit_is_block(&mut self, pc_vars: &[u32], block_idx: usize) -> u32 {
        if pc_vars.is_empty() {
            // pc_width = 0  →  N = 1; single block, always active.
            return self.emit_one();
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
            acc = self.emit_and(acc, bitj);
        }
        acc
    }

    /// Emit constant-bit vars encoding `block_idx` in `pc_width` bits
    /// (LSB-first).  Each bit is a freshly emitted `Zero` or `One` constant.
    fn encode_pc_bits(&mut self, block_idx: usize, pc_width: usize) -> Vec<u32> {
        (0..pc_width)
            .map(|j| {
                if (block_idx >> j) & 1 == 1 {
                    self.emit_one()
                } else {
                    self.emit_zero()
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
/// Returns the combined module.  If `blocks` already has exactly one block
/// it is returned unchanged (via `Clone`).
pub fn movfuscate<C: MovfuscCtx>(mut ctx: C, blocks: &C::Blocks) -> C::Blocks {
    let n = C::num_blocks(blocks);
    assert!(n >= 1, "movfuscate: empty block list");
    if n == 1 {
        return blocks.clone();
    }

    let pc_width = pc_bits_needed(n);
    let state_width = (0..n)
        .map(|i| C::block_param_count(blocks, i))
        .max()
        .unwrap_or(0);
    let ret_width = C::return_val_width(blocks);
    let combined_params = pc_width + state_width;

    // Combined block parameter var IDs:
    //   [0, pc_width)             — PC bits (all-zero encoding = block 0)
    //   [pc_width, combined_params) — state slots
    let pc_vars: Vec<u32> = (0..pc_width as u32).collect();
    let state_vars: Vec<u32> = (pc_width as u32..combined_params as u32).collect();

    // Shared zero constant used as the accumulator seed.  It is a single SSA
    // value (referenced, not copied), so it only adds one stmt.
    let zero = ctx.emit_zero();

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
        let term =
            ctx.emit_block_terminator(blocks, i, &block_vals, pc_width, state_width, ret_width);
        results.push(BlockResult {
            is_active,
            done: term.done,
            next_pc_bits: term.next_pc_bits,
            next_state: term.next_state,
            ret_vals: term.ret_vals,
        });
    }

    // ---- XOR-accumulate across mutually exclusive active bits ---------------
    //
    // Exactly one is_active_i = 1 per valid execution step, therefore:
    //   XOR_i(is_active_i AND x_i)  ==  OR_i(is_active_i AND x_i)  ==  x_{active}

    let mut done_acc = zero;
    for br in &results {
        let g = ctx.emit_and(br.is_active, br.done);
        done_acc = ctx.emit_xor(done_acc, g);
    }

    let mut next_pc = vec![zero; pc_width];
    for br in &results {
        for j in 0..pc_width {
            let g = ctx.emit_and(br.is_active, br.next_pc_bits[j]);
            next_pc[j] = ctx.emit_xor(next_pc[j], g);
        }
    }

    let mut next_state = vec![zero; state_width];
    for br in &results {
        for k in 0..state_width {
            let g = ctx.emit_and(br.is_active, br.next_state[k]);
            next_state[k] = ctx.emit_xor(next_state[k], g);
        }
    }

    let mut ret_vals = vec![zero; ret_width];
    for br in &results {
        for m in 0..ret_width {
            let g = ctx.emit_and(br.is_active, br.ret_vals[m]);
            ret_vals[m] = ctx.emit_xor(ret_vals[m], g);
        }
    }

    // Loop-back args = [next_pc_bits…, next_state…]
    let mut loop_vars = Vec::with_capacity(combined_params);
    loop_vars.extend_from_slice(&next_pc);
    loop_vars.extend_from_slice(&next_state);

    ctx.build_output(combined_params, done_acc, loop_vars, ret_vals)
}

// ============================================================================
// BIrBlocks implementation
// ============================================================================

/// Substitute every `IRVarId` operand in `stmt` through `var_map[id.0]`.
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

    /// Decompose a single `BIrTarget` into the four dispatch outputs.
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
                let done = self.emit_one();
                let next_pc = self.encode_pc_bits(0, pc_width); // don't care
                let next_state = (0..state_width).map(|_| self.emit_zero()).collect();
                let mut ret: Vec<u32> = target.args.iter().map(lookup).collect();
                while ret.len() < ret_width {
                    ret.push(self.emit_zero());
                }
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Block(IRBlockId(j)) => {
                let done = self.emit_zero();
                let next_pc = self.encode_pc_bits(*j as usize, pc_width);
                let mut next_state: Vec<u32> = target.args.iter().map(lookup).collect();
                while next_state.len() < state_width {
                    next_state.push(self.emit_zero());
                }
                let ret = (0..ret_width).map(|_| self.emit_zero()).collect();
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

    fn emit_zero(&mut self) -> u32 {
        self.push(BIrStmt::Zero)
    }

    fn emit_one(&mut self) -> u32 {
        self.push(BIrStmt::One)
    }

    fn emit_and(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a; // AND(x, x) = x — idempotent; no new stmt
        }
        self.push(BIrStmt::And(IRVarId(a), IRVarId(b)))
    }

    fn emit_xor(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return self.push(BIrStmt::Zero); // XOR(x, x) = 0
        }
        self.push(BIrStmt::Xor(IRVarId(a), IRVarId(b)))
    }

    fn emit_not(&mut self, a: u32) -> u32 {
        self.push(BIrStmt::Not(IRVarId(a)))
    }

    fn emit_block_stmts(
        &mut self,
        blocks: &BIrBlocks,
        block_idx: usize,
        state_vars: &[u32],
    ) -> Vec<u32> {
        let block = &blocks.0[block_idx];
        let p = block.params as usize;
        let mut var_map: Vec<u32> = Vec::with_capacity(p + block.stmts.len());
        // Params map to the first p state slots.
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
        state_width: usize,
        ret_width: usize,
    ) -> TermResult {
        let block = &blocks.0[block_idx];
        match &block.terminator {
            BIrTerminator::Jmp(target) => {
                let (done, next_pc_bits, next_state, ret_vals) =
                    self.process_biir_target(target, block_vals, pc_width, state_width, ret_width);
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
                // Select between the two branches based on the condition bit.
                let done = self.emit_select(cond, t_done, e_done);
                let next_pc_bits = (0..pc_width)
                    .map(|j| self.emit_select(cond, t_npc[j], e_npc[j]))
                    .collect();
                let next_state = (0..state_width)
                    .map(|k| self.emit_select(cond, t_ns[k], e_ns[k]))
                    .collect();
                let ret_vals = (0..ret_width)
                    .map(|m| self.emit_select(cond, t_ret[m], e_ret[m]))
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
// IRBlocks implementation
// ============================================================================
//
// Restriction: all block params (= state slots) must be `IRType::Bit`.
// The PC bits we inject are also `Bit`-typed.  All boolean helper operations
// (PC decoding, select, XOR accumulation) are expressed as `IRStmt::Poly`
// over GF(2), which is the native polynomial representation for Bit types.

/// Substitute every `IRVarId` in `stmt` through `var_map[id.0]`.
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
                    nv.sort(); // maintain canonical monomial ordering
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

struct IrCtx {
    stmts: Vec<IRStmt>,
    next_id: u32,
    bit_type_id: IRTypeId,
}

impl IrCtx {
    fn new(first_id: u32, bit_type_id: IRTypeId) -> Self {
        Self { stmts: Vec::new(), next_id: first_id, bit_type_id }
    }

    fn push_ir(&mut self, stmt: IRStmt) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.stmts.push(stmt);
        id
    }

    /// Emit a GF(2) polynomial: `Σ coeff·monomial + constant` over Bit.
    fn emit_poly(
        &mut self,
        coeffs: BTreeMap<Vec<IRVarId>, u8>,
        constant_lo: u128,
    ) -> u32 {
        self.push_ir(IRStmt::Poly {
            coeffs,
            constant: Constant { hi: 0, lo: constant_lo },
        })
    }

    /// Decompose a single `IRBlockTargetId + args` into the four dispatch
    /// outputs.
    fn process_ir_target(
        &mut self,
        target_block: &IRBlockTargetId,
        args: &[IRVarId],
        block_vals: &[u32],
        pc_width: usize,
        state_width: usize,
        ret_width: usize,
    ) -> (u32, Vec<u32>, Vec<u32>, Vec<u32>) {
        let lookup = |id: &IRVarId| block_vals[id.0 as usize];
        match target_block {
            IRBlockTargetId::Return => {
                let done = self.emit_one();
                let next_pc = self.encode_pc_bits(0, pc_width); // don't care
                let next_state = (0..state_width).map(|_| self.emit_zero()).collect();
                let mut ret: Vec<u32> = args.iter().map(lookup).collect();
                while ret.len() < ret_width {
                    ret.push(self.emit_zero());
                }
                (done, next_pc, next_state, ret)
            }
            IRBlockTargetId::Block(IRBlockId(j)) => {
                let done = self.emit_zero();
                let next_pc = self.encode_pc_bits(*j as usize, pc_width);
                let mut next_state: Vec<u32> = args.iter().map(lookup).collect();
                while next_state.len() < state_width {
                    next_state.push(self.emit_zero());
                }
                let ret = (0..ret_width).map(|_| self.emit_zero()).collect();
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

    // Boolean operations encoded as GF(2) polynomials over `IRType::Bit`.

    fn emit_zero(&mut self) -> u32 {
        let bt = self.bit_type_id.clone();
        self.push_ir(IRStmt::Const(Constant { hi: 0, lo: 0 }, bt))
    }

    fn emit_one(&mut self) -> u32 {
        let bt = self.bit_type_id.clone();
        self.push_ir(IRStmt::Const(Constant { hi: 0, lo: 1 }, bt))
    }

    /// `a AND b` = degree-2 monomial `{[a,b]: 1}` in GF(2).
    fn emit_and(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a; // a AND a = a (idempotent)
        }
        let mut key = vec![IRVarId(a), IRVarId(b)];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        self.emit_poly(coeffs, 0)
    }

    /// `a XOR b` = two degree-1 monomials `{[a]: 1, [b]: 1}` in GF(2).
    fn emit_xor(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return self.emit_zero(); // XOR(x, x) = 0
        }
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        coeffs.insert(vec![IRVarId(b)], 1);
        self.emit_poly(coeffs, 0)
    }

    /// `NOT a` = `1 + a` = `{[a]: 1}` with constant `1` in GF(2).
    fn emit_not(&mut self, a: u32) -> u32 {
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(vec![IRVarId(a)], 1);
        self.emit_poly(coeffs, 1)
    }

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
            let id = self.push_ir(mapped);
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
        state_width: usize,
        ret_width: usize,
    ) -> TermResult {
        let block = &blocks.0[block_idx];
        match &block.terminator {
            IRTerminator::Jmp { func, args } => {
                let (done, next_pc_bits, next_state, ret_vals) = self.process_ir_target(
                    func,
                    args,
                    block_vals,
                    pc_width,
                    state_width,
                    ret_width,
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
                    state_width,
                    ret_width,
                );
                let (e_done, e_npc, e_ns, e_ret) = self.process_ir_target(
                    false_block,
                    false_args,
                    block_vals,
                    pc_width,
                    state_width,
                    ret_width,
                );
                let done = self.emit_select(cond, t_done, e_done);
                let next_pc_bits = (0..pc_width)
                    .map(|j| self.emit_select(cond, t_npc[j], e_npc[j]))
                    .collect();
                let next_state = (0..state_width)
                    .map(|k| self.emit_select(cond, t_ns[k], e_ns[k]))
                    .collect();
                let ret_vals = (0..ret_width)
                    .map(|m| self.emit_select(cond, t_ret[m], e_ret[m]))
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
        combined_params: usize,
        done_var: u32,
        loop_vars: Vec<u32>,
        ret_vars: Vec<u32>,
    ) -> IRBlocks {
        let bit = self.bit_type_id.clone();
        IRBlocks(vec![IRBlock {
            params: vec![bit; combined_params],
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
// Public entry points
// ============================================================================

/// Movfuscate a `BIrBlocks` module into a single self-looping block.
///
/// The result satisfies `is_movfuscated()`.  If the input already has exactly
/// one block it is returned unchanged (via `Clone`).
///
/// The combined block has `pc_bits_needed(N) + max_params` parameters.
/// The caller must supply `[0; pc_width] ++ original_block_0_inputs` when
/// invoking the combined entry point.
pub fn movfuscate_biir(blocks: &BIrBlocks) -> BIrBlocks {
    let n = blocks.0.len();
    let pc_width = pc_bits_needed(n);
    let state_width = blocks.0.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let combined_params = pc_width + state_width;
    let ctx = BIrCtx::new(combined_params as u32);
    movfuscate(ctx, blocks)
}

/// Movfuscate an `IRBlocks` module into a single self-looping block.
///
/// **Restriction**: all block params must be `IRType::Bit`.  The function
/// panics with a descriptive message if any non-Bit param is found.
///
/// `types` is searched for an existing `IRType::Bit` entry; one is appended
/// if absent.  The combined block's params are all typed as `Bit`.
///
/// The result satisfies `is_movfuscated()`.  If the input already has exactly
/// one block it is returned unchanged (via `Clone`).
pub fn movfuscate_ir(blocks: &IRBlocks, types: &mut IRTypes) -> IRBlocks {
    // Validate: every block param must be IRType::Bit.
    for (bi, block) in blocks.0.iter().enumerate() {
        for (pi, tid) in block.params.iter().enumerate() {
            assert!(
                matches!(types.0[tid.0 as usize], IRType::Bit),
                "movfuscate_ir: block {bi} param {pi} has non-Bit type \
                 ({:?}); only IRType::Bit params are supported",
                types.0[tid.0 as usize],
            );
        }
    }

    // Find or insert IRType::Bit.
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

    let n = blocks.0.len();
    let pc_width = pc_bits_needed(n);
    let state_width = blocks.0.iter().map(|b| b.params.len()).max().unwrap_or(0);
    let combined_params = pc_width + state_width;
    let ctx = IrCtx::new(combined_params as u32, bit_type_id);
    movfuscate(ctx, blocks)
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

    /// Two-block DAG:
    ///   Block 0 (1 param `a`): `c = NOT(a)`
    ///             CondJmp(c, Block(1,[c]), Block(1,[a]))
    ///   Block 1 (1 param `x`): Jmp(Return, [x])
    ///
    /// Semantics: always returns `NOT(a)` (both branches pass NOT(a) or a,
    /// selected by `c = NOT(a)`, so result = c when c=1, a when c=0 = 0).
    fn two_block_dag() -> BIrBlocks {
        BIrBlocks(std::vec![
            BIrBlock {
                params: 1,
                stmts: std::vec![BIrStmt::Not(IRVarId(0))], // var 1 = NOT(a)
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
        assert_eq!(result, single, "single-block input must be returned unchanged");
    }

    #[test]
    fn test_biir_two_block_dag_is_movfuscated() {
        let result = movfuscate_biir(&two_block_dag());
        assert!(result.is_movfuscated(), "result must have exactly one block");
    }

    #[test]
    fn test_biir_two_block_dag_param_count() {
        let result = movfuscate_biir(&two_block_dag());
        // pc_width=1 (2 blocks → 1 bit), state_width=1 (both blocks: 1 param)
        assert_eq!(result.0[0].params, 2, "combined params = pc_width(1) + state_width(1)");
    }

    #[test]
    fn test_biir_two_block_dag_terminator_shape() {
        let result = movfuscate_biir(&two_block_dag());
        match &result.0[0].terminator {
            BIrTerminator::CondJmp { then_target, else_target, .. } => {
                assert_eq!(then_target.block, IRBlockTargetId::Return, "then = Return");
                assert_eq!(
                    else_target.block,
                    IRBlockTargetId::Block(IRBlockId(0)),
                    "else = Block(0) (self-loop)"
                );
                assert_eq!(else_target.args.len(), 2, "loop-back args = pc(1) + state(1)");
            }
            other => panic!("expected CondJmp, got {:?}", other),
        }
    }

    #[test]
    fn test_biir_ret_width_preserved() {
        let result = movfuscate_biir(&two_block_dag());
        match &result.0[0].terminator {
            BIrTerminator::CondJmp { then_target, .. } => {
                assert_eq!(then_target.args.len(), 1, "ret_width must equal original (1)");
            }
            _ => panic!("expected CondJmp"),
        }
    }

    /// Three-block chain: B0 → B1 → B2 → Return.
    /// Needs 2 PC bits; combined params = 2 + 1 = 3.
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
        // pc_width = ceil(log2(3)) = 2, state_width = 1 → combined = 3
        assert_eq!(result.0[0].params, 3);
        match &result.0[0].terminator {
            BIrTerminator::CondJmp { then_target, else_target, .. } => {
                assert_eq!(then_target.block, IRBlockTargetId::Return);
                assert_eq!(else_target.block, IRBlockTargetId::Block(IRBlockId(0)));
                assert_eq!(else_target.args.len(), 3, "loop-back = pc(2) + state(1)");
                assert_eq!(then_target.args.len(), 1, "ret_width = 1");
            }
            _ => panic!("expected CondJmp"),
        }
    }

    /// A single-block self-loop must pass through unchanged.
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
        assert_eq!(result, blocks, "single-block self-loop must be returned unchanged");
    }

    /// Four-block module: exercises a 2-bit PC addressing all 4 blocks.
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
        // pc_width = ceil(log2(4)) = 2, state_width = 1 → combined = 3
        assert_eq!(result.0[0].params, 3);
    }

    // =========================================================================
    // IRBlocks movfuscation
    // =========================================================================

    fn bit_types_table() -> IRTypes {
        IRTypes(std::vec![IRType::Bit])
    }

    /// Two-block IR DAG (all-Bit):
    ///   Block 0 (1 Bit param): Jmp(Block(1), [param])
    ///   Block 1 (1 Bit param): Jmp(Return,   [param])
    fn two_block_ir() -> (IRBlocks, IRTypes) {
        let types = bit_types_table();
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
    fn test_ir_single_block_passthrough() {
        let mut types = bit_types_table();
        let blocks = IRBlocks(std::vec![IRBlock {
            params: std::vec![IRTypeId(0)],
            stmts: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
        }]);
        let result = movfuscate_ir(&blocks, &mut types);
        assert_eq!(result, blocks, "single-block input must be returned unchanged");
    }

    #[test]
    fn test_ir_two_block_dag_is_movfuscated() {
        let (blocks, mut types) = two_block_ir();
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
    }

    #[test]
    fn test_ir_two_block_dag_param_count_and_types() {
        let (blocks, mut types) = two_block_ir();
        let result = movfuscate_ir(&blocks, &mut types);
        // pc_width=1 (2 blocks), state_width=1 → combined=2
        assert_eq!(result.0[0].params.len(), 2, "combined params = pc(1) + state(1)");
        for tid in &result.0[0].params {
            assert!(
                matches!(types.0[tid.0 as usize], IRType::Bit),
                "all combined params must be Bit-typed"
            );
        }
    }

    #[test]
    fn test_ir_two_block_dag_terminator_shape() {
        let (blocks, mut types) = two_block_ir();
        let result = movfuscate_ir(&blocks, &mut types);
        match &result.0[0].terminator {
            IRTerminator::JumpCond {
                true_block,
                true_args,
                false_block,
                false_args,
                ..
            } => {
                assert_eq!(*true_block, IRBlockTargetId::Return, "true = Return");
                assert_eq!(true_args.len(), 1, "ret_width = 1");
                assert_eq!(*false_block, IRBlockTargetId::Block(IRBlockId(0)), "false = Block(0)");
                assert_eq!(false_args.len(), 2, "loop-back = pc(1) + state(1)");
            }
            other => panic!("expected JumpCond, got {:?}", other),
        }
    }

    /// IRBlocks with a Poly stmt: ensures subst_ir correctly remaps vars.
    #[test]
    fn test_ir_with_poly_stmt() {
        let mut types = bit_types_table();
        let bit = IRTypeId(0);
        // Block 0: param a; c = Poly{[a]: 1, constant: 1} = NOT(a); Jmp(Block(1), [c])
        // Block 1: param x; Jmp(Return, [x])
        let mut coeffs = BTreeMap::new();
        coeffs.insert(std::vec![IRVarId(0)], 1u8);
        let blocks = IRBlocks(std::vec![
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![IRStmt::Poly {
                    coeffs,
                    constant: Constant { hi: 0, lo: 1 },
                }],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(1)),
                    args: std::vec![IRVarId(1)], // NOT(a)
                },
            },
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
            },
        ]);
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // The combined block must have stmts (at minimum the re-emitted Poly + overhead).
        assert!(
            !result.0[0].stmts.is_empty(),
            "combined block must have stmts from the original Poly plus dispatch overhead"
        );
    }

    /// IRBlocks with a JumpCond (conditional branch).
    #[test]
    fn test_ir_condjmp_two_blocks() {
        let mut types = bit_types_table();
        let bit = IRTypeId(0);
        // Block 0: params (a, b); JumpCond(a, Block(1,[b]), Return([a]))
        // Block 1: params (x);    Jmp(Return, [x])
        let blocks = IRBlocks(std::vec![
            IRBlock {
                params: std::vec![bit.clone(), bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::JumpCond {
                    condition: IRVarId(0),
                    true_block: IRBlockTargetId::Block(IRBlockId(1)),
                    true_args: std::vec![IRVarId(1)],
                    false_block: IRBlockTargetId::Return,
                    false_args: std::vec![IRVarId(0)],
                },
            },
            IRBlock {
                params: std::vec![bit.clone()],
                stmts: std::vec![],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)],
                },
            },
        ]);
        let result = movfuscate_ir(&blocks, &mut types);
        assert!(result.is_movfuscated());
        // pc_width=1, state_width=2 (block 0 has 2 params) → combined=3
        assert_eq!(result.0[0].params.len(), 3, "combined = pc(1) + state(2)");
        match &result.0[0].terminator {
            IRTerminator::JumpCond { true_args, false_args, .. } => {
                assert_eq!(true_args.len(), 1, "ret_width = 1");
                assert_eq!(false_args.len(), 3, "loop-back = pc(1) + state(2)");
            }
            _ => panic!("expected JumpCond"),
        }
    }
}
