// @reliability: normal
// @ai: assisted
//! Shared dispatch-accumulator helpers used by `movfuscate` and
//! `volar-ir-virt`.
//!
//! Both passes need the same primitive constructions on a boolean
//! accumulator abstraction:
//!
//! * `emit_is_block(pc, k)` — decode `pc == binary(k)` as a Bit.
//! * `encode_pc_bits(k, n)` — emit constant `pc` bits for block index `k`.
//! * `emit_select_bit(cond, a, b)` — Bit-typed mux.
//! * `emit_select_slot(cond, a, b, ty)` — field-typed mux.
//!
//! These are defined here as free generic functions over two small
//! traits: [`DispatchBitPrimitives`] for pure Bit operations, and
//! [`DispatchSlotPrimitives`] (which extends [`DispatchBitPrimitives`])
//! for the typed slot gate / field-add operations used by multi-slot
//! state accumulation.
//!
//! The `movfuscate` and `volar-ir-virt` code that owns a handler-emission
//! context implements these two traits and delegates to the helpers
//! below.  Any updates to the shared formulas happen here once.

use alloc::vec::Vec;

/// Bit-typed primitive operations.
///
/// Callers that only need `emit_is_block`, `encode_pc_bits`, and
/// `emit_select_bit` implement this trait.
pub trait DispatchBitPrimitives {
    fn emit_zero_bit(&mut self) -> u32;
    fn emit_one_bit(&mut self) -> u32;
    fn emit_and_bit(&mut self, a: u32, b: u32) -> u32;
    fn emit_xor_bit(&mut self, a: u32, b: u32) -> u32;
    fn emit_not(&mut self, a: u32) -> u32;
}

/// Typed slot operations on top of the Bit primitives.
///
/// `SlotTy` is an opaque descriptor for the value type of a state or
/// return slot.  For pure-Bit accumulators (e.g. `BIrBlocks`) use
/// `SlotTy = ()`.
pub trait DispatchSlotPrimitives: DispatchBitPrimitives {
    type SlotTy;
    fn emit_zero_slot(&mut self, ty: &Self::SlotTy) -> u32;
    /// `is_active · val` — scalar multiplication of a field element
    /// `val: ty` by a Bit `is_active`.
    fn emit_gate(&mut self, is_active: u32, val: u32, ty: &Self::SlotTy) -> u32;
    /// `a + b` — field addition where both operands have type `ty`.
    fn emit_field_add(&mut self, a: u32, b: u32, ty: &Self::SlotTy) -> u32;
}

// ============================================================================
// Shared helpers
// ============================================================================

/// `select(cond, a, b) = AND(cond, XOR(a, b)) XOR b` — all Bit operands.
pub fn emit_select_bit<C: DispatchBitPrimitives>(c: &mut C, cond: u32, a: u32, b: u32) -> u32 {
    let xab = c.emit_xor_bit(a, b);
    let sel = c.emit_and_bit(cond, xab);
    c.emit_xor_bit(sel, b)
}

/// `select(cond, a, b) = gate(cond, a+b) + b` — typed field operands.
///
/// Works for any field type including `Bit`.
pub fn emit_select_slot<C: DispatchSlotPrimitives>(
    c: &mut C,
    cond: u32,
    a: u32,
    b: u32,
    ty: &C::SlotTy,
) -> u32 {
    let xab = c.emit_field_add(a, b, ty);
    let sel = c.emit_gate(cond, xab, ty);
    c.emit_field_add(sel, b, ty)
}

/// Emit `is_active` for `block_idx`: AND-of-(NOT-or-identity) per PC bit.
pub fn emit_is_block<C: DispatchBitPrimitives>(c: &mut C, pc_vars: &[u32], block_idx: usize) -> u32 {
    if pc_vars.is_empty() {
        return c.emit_one_bit();
    }
    let bit0 = if (block_idx & 1) == 1 {
        pc_vars[0]
    } else {
        c.emit_not(pc_vars[0])
    };
    let mut acc = bit0;
    for j in 1..pc_vars.len() {
        let bitj = if (block_idx >> j) & 1 == 1 {
            pc_vars[j]
        } else {
            c.emit_not(pc_vars[j])
        };
        acc = c.emit_and_bit(acc, bitj);
    }
    acc
}

/// Emit constant-bit vars encoding `block_idx` in `pc_width` bits
/// (LSB-first).
pub fn encode_pc_bits<C: DispatchBitPrimitives>(
    c: &mut C,
    block_idx: usize,
    pc_width: usize,
) -> Vec<u32> {
    (0..pc_width)
        .map(|j| {
            if (block_idx >> j) & 1 == 1 {
                c.emit_one_bit()
            } else {
                c.emit_zero_bit()
            }
        })
        .collect()
}
