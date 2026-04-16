// @reliability: normal
// @ai: assisted
//! Generic bit-circuit algorithms, shared by [`VolarIrTarget`] and [`VaffleTarget`].
//!
//! # `BitCircuitBuilder`
//!
//! A trait with two required primitives — [`bc_const`](BitCircuitBuilder::bc_const)
//! and [`bc_poly`](BitCircuitBuilder::bc_poly) — from which every other bit
//! operation and every arithmetic algorithm is derived by default impls.
//! Backends may override individual operations for efficiency (e.g.,
//! `VolarIrTarget` overrides [`bc_carry3`](BitCircuitBuilder::bc_carry3) with
//! a single degree-3 `Poly` stmt rather than three ANDs and two XORs).
//!
//! # Algorithms
//!
//! All algorithms are standalone free functions generic over `B: BitCircuitBuilder`.
//! They operate on `Vec<B::Bit>` slices representing *N*-bit integers, LSB first.

extern crate alloc;

use alloc::{collections::BTreeMap, vec, vec::Vec};

// ============================================================================
// Trait
// ============================================================================

/// Primitive interface for building GF(2) bit-circuit computations.
///
/// Implementors supply two required methods:
///
/// * [`bc_const`](Self::bc_const) — emit a constant-0 or constant-1 bit.
/// * [`bc_poly`](Self::bc_poly) — emit a GF(2) multivariate polynomial stmt
///   and return the SSA variable holding its result.
///
/// Everything else — XOR, AND, NOT, OR, MUX, carry, and all integer arithmetic
/// algorithms — is derived from these two via default implementations.
pub trait BitCircuitBuilder {
    /// The SSA variable type for a single GF(2) bit.
    ///
    /// Must be `Clone` (values are referenced multiple times in circuits) and
    /// `Ord` (monomial keys in the Poly coefficient map are sorted `Vec<Bit>`).
    type Bit: Clone + Ord;

    /// Emit a constant bit with value `val`.
    fn bc_const(&mut self, val: bool) -> Self::Bit;

    /// Emit a GF(2) polynomial: Σ `coeff * ∏(monomial)` + `constant` (mod 2).
    ///
    /// `coeffs` maps each sorted monomial (product of variables) to its GF(2)
    /// coefficient (only the low bit is significant).  `constant` is the
    /// degree-0 term (again, only its low bit matters).
    ///
    /// Returns the SSA variable that holds this polynomial's result.
    fn bc_poly(
        &mut self,
        coeffs: BTreeMap<Vec<Self::Bit>, u8>,
        constant: u128,
    ) -> Self::Bit;

    // ---- Derived single-bit ops (may be overridden for efficiency) ----------

    /// `a XOR b` — `Poly { {[a]:1, [b]:1}, constant:0 }`.
    fn bc_xor(&mut self, a: Self::Bit, b: Self::Bit) -> Self::Bit {
        let mut c = BTreeMap::new();
        c.insert(vec![a], 1u8);
        c.insert(vec![b], 1u8);
        self.bc_poly(c, 0)
    }

    /// `a AND b` — `Poly { {[a,b]:1}, constant:0 }`.
    fn bc_and(&mut self, a: Self::Bit, b: Self::Bit) -> Self::Bit {
        let mut key = vec![a, b];
        key.sort();
        let mut c = BTreeMap::new();
        c.insert(key, 1u8);
        self.bc_poly(c, 0)
    }

    /// `NOT a` — `Poly { {[a]:1}, constant:1 }` (i.e. `1 + a` in GF(2)).
    fn bc_not(&mut self, a: Self::Bit) -> Self::Bit {
        let mut c = BTreeMap::new();
        c.insert(vec![a], 1u8);
        self.bc_poly(c, 1)
    }

    /// `a OR b` — De Morgan: `NOT(NOT(a) AND NOT(b))`.
    fn bc_or(&mut self, a: Self::Bit, b: Self::Bit) -> Self::Bit {
        let na = self.bc_not(a);
        let nb = self.bc_not(b);
        let nand = self.bc_and(na, nb);
        self.bc_not(nand)
    }

    /// `MUX(cond, a, b)` = `AND(cond, XOR(a,b)) XOR b`.
    fn bc_select(&mut self, cond: Self::Bit, a: Self::Bit, b: Self::Bit) -> Self::Bit {
        let xab = self.bc_xor(a, b.clone());
        let sel = self.bc_and(cond, xab);
        self.bc_xor(sel, b)
    }

    /// Majority of three bits: `(a∧b) ⊕ (a∧c) ⊕ (b∧c)`.
    ///
    /// Used as the carry output of a full adder.  The default is generic
    /// (3 ANDs + 2 XORs); backends with native 3-variable Poly stmts should
    /// override this.
    fn bc_carry3(&mut self, a: Self::Bit, b: Self::Bit, c: Self::Bit) -> Self::Bit {
        let mut ab = vec![a.clone(), b.clone()]; ab.sort();
        let mut ac = vec![a.clone(), c.clone()]; ac.sort();
        let mut bc = vec![b.clone(), c.clone()]; bc.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(ab, 1u8);
        coeffs.insert(ac, 1u8);
        coeffs.insert(bc, 1u8);
        self.bc_poly(coeffs, 0)
    }
}

// ============================================================================
// Arithmetic algorithms — all generic over B: BitCircuitBuilder
// ============================================================================

/// Ripple-carry adder: `a + x + carry_in`.  Overflow is silently discarded.
pub fn bc_add<B: BitCircuitBuilder>(
    b: &mut B,
    a: &[B::Bit],
    x: &[B::Bit],
    carry_in: bool,
) -> Vec<B::Bit> {
    let n = a.len();
    assert_eq!(n, x.len(), "bc_add: operand widths must match");
    let mut carry = b.bc_const(carry_in);
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        let sum = {
            let t = b.bc_xor(a[i].clone(), x[i].clone());
            b.bc_xor(t, carry.clone())
        };
        let new_carry = b.bc_carry3(a[i].clone(), x[i].clone(), carry);
        out.push(sum);
        carry = new_carry;
    }
    out
}

/// Two's-complement subtraction: `a - x = a + ~x + 1`.
pub fn bc_sub<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    let not_x: Vec<B::Bit> = x.iter().map(|xi| b.bc_not(xi.clone())).collect();
    bc_add(b, a, &not_x, true)
}

/// Two's-complement negation: `~a + 1`.
pub fn bc_neg<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit]) -> Vec<B::Bit> {
    let zeros: Vec<B::Bit> = (0..a.len()).map(|_| b.bc_const(false)).collect();
    let not_a: Vec<B::Bit> = a.iter().map(|ai| b.bc_not(ai.clone())).collect();
    bc_add(b, &not_a, &zeros, true)
}

/// Absolute value: `if MSB then -a else a`.
pub fn bc_abs<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit]) -> Vec<B::Bit> {
    let n = a.len();
    let sign = a[n - 1].clone();
    let neg = bc_neg(b, a);
    a.iter()
        .zip(neg.iter())
        .map(|(pos, neg_b)| b.bc_select(sign.clone(), neg_b.clone(), pos.clone()))
        .collect()
}

/// Shift-and-add multiplier: lower *n* bits of `a * x`.
pub fn bc_mul<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    let n = a.len();
    let mut acc: Vec<B::Bit> = (0..n).map(|_| b.bc_const(false)).collect();
    for i in 0..n {
        let pp: Vec<B::Bit> = (0..n)
            .map(|k| {
                if k < i {
                    b.bc_const(false)
                } else {
                    b.bc_and(a[i].clone(), x[k - i].clone())
                }
            })
            .collect();
        acc = bc_add(b, &acc, &pp, false);
    }
    acc
}

/// Restoring long-division: quotient of `a / x` (unsigned).
pub fn bc_udiv<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    let n = a.len();
    let mut r: Vec<B::Bit> = (0..n).map(|_| b.bc_const(false)).collect();
    let mut q: Vec<B::Bit> = (0..n).map(|_| b.bc_const(false)).collect();
    for i in (0..n).rev() {
        // r = (r << 1) | a[i]
        let mut new_r = Vec::with_capacity(n);
        new_r.push(a[i].clone());
        for k in 0..n - 1 {
            new_r.push(r[k].clone());
        }
        r = new_r;
        // diff = r - x via full subtractor chain
        let mut borrow = b.bc_const(false);
        let mut diff = Vec::with_capacity(n);
        for k in 0..n {
            let not_rk = b.bc_not(r[k].clone());
            let d = {
                let t = b.bc_xor(r[k].clone(), x[k].clone());
                b.bc_xor(t, borrow.clone())
            };
            let new_borrow = b.bc_carry3(not_rk, x[k].clone(), borrow);
            diff.push(d);
            borrow = new_borrow;
        }
        let no_borrow = b.bc_not(borrow);
        q[i] = no_borrow.clone();
        let r_copy = r.clone();
        r = r_copy
            .iter()
            .zip(diff.iter())
            .map(|(ri, di)| b.bc_select(no_borrow.clone(), di.clone(), ri.clone()))
            .collect();
    }
    q
}

/// Signed division: sign-correct restoring division.
pub fn bc_sdiv<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    let n = a.len();
    let sign_a = a[n - 1].clone();
    let sign_x = x[n - 1].clone();
    let abs_a = bc_abs(b, a);
    let abs_x = bc_abs(b, x);
    let abs_q = bc_udiv(b, &abs_a, &abs_x);
    let signs_differ = b.bc_xor(sign_a, sign_x);
    let neg_q = bc_neg(b, &abs_q);
    abs_q
        .iter()
        .zip(neg_q.iter())
        .map(|(pq, nq)| b.bc_select(signs_differ.clone(), nq.clone(), pq.clone()))
        .collect()
}

fn barrel_stages(n: usize, shift_bits: usize) -> usize {
    if n <= 1 {
        return 1;
    }
    let from_n = (usize::BITS - (n - 1).leading_zeros()) as usize;
    from_n.min(shift_bits).max(1)
}

/// Left shift.
pub fn bc_shl<B: BitCircuitBuilder>(b: &mut B, val: &[B::Bit], shift: &[B::Bit]) -> Vec<B::Bit> {
    let n = val.len();
    let stages = barrel_stages(n, shift.len());
    let mut cur = val.to_vec();
    for k in 0..stages {
        let step = 1usize << k;
        let sb = shift[k].clone();
        cur = (0..n)
            .map(|j| {
                if j < step {
                    let z = b.bc_const(false);
                    b.bc_select(sb.clone(), z, cur[j].clone())
                } else {
                    b.bc_select(sb.clone(), cur[j - step].clone(), cur[j].clone())
                }
            })
            .collect();
    }
    cur
}

/// Logical right shift (fills with zeros).
pub fn bc_lshr<B: BitCircuitBuilder>(
    b: &mut B,
    val: &[B::Bit],
    shift: &[B::Bit],
) -> Vec<B::Bit> {
    let n = val.len();
    let stages = barrel_stages(n, shift.len());
    let mut cur = val.to_vec();
    for k in 0..stages {
        let step = 1usize << k;
        let sb = shift[k].clone();
        cur = (0..n)
            .map(|j| {
                if j + step >= n {
                    let z = b.bc_const(false);
                    b.bc_select(sb.clone(), z, cur[j].clone())
                } else {
                    b.bc_select(sb.clone(), cur[j + step].clone(), cur[j].clone())
                }
            })
            .collect();
    }
    cur
}

/// Arithmetic right shift (sign-extends).
pub fn bc_ashr<B: BitCircuitBuilder>(
    b: &mut B,
    val: &[B::Bit],
    shift: &[B::Bit],
) -> Vec<B::Bit> {
    let n = val.len();
    let sign = val[n - 1].clone();
    let stages = barrel_stages(n, shift.len());
    let mut cur = val.to_vec();
    for k in 0..stages {
        let step = 1usize << k;
        let sb = shift[k].clone();
        cur = (0..n)
            .map(|j| {
                if j + step >= n {
                    b.bc_select(sb.clone(), sign.clone(), cur[j].clone())
                } else {
                    b.bc_select(sb.clone(), cur[j + step].clone(), cur[j].clone())
                }
            })
            .collect();
    }
    cur
}

// ---- Comparisons -----------------------------------------------------------

/// `a == x` (all-bits-equal).
pub fn bc_eq<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> B::Bit {
    if a.is_empty() {
        return b.bc_const(true);
    }
    let not_xors: Vec<B::Bit> = a
        .iter()
        .zip(x.iter())
        .map(|(ai, xi)| {
            let xor = b.bc_xor(ai.clone(), xi.clone());
            b.bc_not(xor)
        })
        .collect();
    let mut acc = not_xors[0].clone();
    for i in 1..not_xors.len() {
        acc = b.bc_and(acc, not_xors[i].clone());
    }
    acc
}

/// `a != x`.
pub fn bc_ne<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> B::Bit {
    let eq = bc_eq(b, a, x);
    b.bc_not(eq)
}

/// Unsigned `a < x`: final borrow of the ripple subtractor.
pub fn bc_ult<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> B::Bit {
    let n = a.len();
    let mut borrow = b.bc_const(false);
    for i in 0..n {
        let not_ai = b.bc_not(a[i].clone());
        borrow = b.bc_carry3(not_ai, x[i].clone(), borrow);
    }
    borrow
}

/// Unsigned `a <= x`.
pub fn bc_ule<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> B::Bit {
    let gt = bc_ult(b, x, a);
    b.bc_not(gt)
}

/// Signed `a < x`.
pub fn bc_slt<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> B::Bit {
    let n = a.len();
    let sign_a = a[n - 1].clone();
    let sign_x = x[n - 1].clone();
    let not_sign_x = b.bc_not(sign_x.clone());
    let a_neg_x_pos = b.bc_and(sign_a.clone(), not_sign_x);
    let ult = bc_ult(b, a, x);
    let signs_xor = b.bc_xor(sign_a, sign_x);
    let signs_eq = b.bc_not(signs_xor);
    let same_sign_lt = b.bc_and(signs_eq, ult);
    b.bc_or(a_neg_x_pos, same_sign_lt)
}

/// Signed `a <= x`.
pub fn bc_sle<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> B::Bit {
    let sgt = bc_slt(b, x, a);
    b.bc_not(sgt)
}

// ============================================================================
// Vector helpers (apply a bit-op across paired bit vectors)
// ============================================================================

/// Pointwise XOR of two equal-length bit vectors.
pub fn bc_xor_vec<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    a.iter().zip(x).map(|(ai, xi)| b.bc_xor(ai.clone(), xi.clone())).collect()
}

/// Pointwise AND of two equal-length bit vectors.
pub fn bc_and_vec<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    a.iter().zip(x).map(|(ai, xi)| b.bc_and(ai.clone(), xi.clone())).collect()
}

/// Pointwise OR of two equal-length bit vectors.
pub fn bc_or_vec<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit], x: &[B::Bit]) -> Vec<B::Bit> {
    a.iter().zip(x).map(|(ai, xi)| b.bc_or(ai.clone(), xi.clone())).collect()
}

/// Pointwise NOT of a bit vector.
pub fn bc_not_vec<B: BitCircuitBuilder>(b: &mut B, a: &[B::Bit]) -> Vec<B::Bit> {
    a.iter().map(|ai| b.bc_not(ai.clone())).collect()
}

/// Pointwise MUX of two equal-length bit vectors.
pub fn bc_select_vec<B: BitCircuitBuilder>(
    b: &mut B,
    cond: B::Bit,
    a: &[B::Bit],
    x: &[B::Bit],
) -> Vec<B::Bit> {
    a.iter()
        .zip(x)
        .map(|(ai, xi)| b.bc_select(cond.clone(), ai.clone(), xi.clone()))
        .collect()
}

// ============================================================================
// Storage emission trait (for StorageRead / StorageWrite)
// ============================================================================

use volar_ir_common::{StorageId, TypeId};

/// Trait for targets that can emit `StorageRead` and `StorageWrite` stmts.
///
/// The address is always a **multi-bit integer** passed as `&[Self::Bit]`
/// (LSB first, the same representation used throughout the bit-circuit layer).
/// Each backend's [`compose_address`](StorageEmitter::compose_address) packages
/// these bits into a single SSA var of type `Vec(N, Bit)` — a properly-typed
/// wide address — before emitting the storage stmt.
///
/// # Address type
///
/// Storage addresses use `IrType::Vec(N, bit_tid)` so that the Volar IR type
/// system tracks the address width explicitly.  When the `StackPtr` base and
/// offset are both compile-time constants, the emitted `Merge` constant-folds
/// through `bc_const` and the resulting address var is also a constant.
pub trait StorageEmitter: BitCircuitBuilder {
    /// Compose N bit-circuit vars into a single SSA var of type `Vec(N, Bit)`.
    ///
    /// This is a required method so each backend can intern the appropriate
    /// `Vec(N, bit_tid)` type in its own type table and emit a `Merge` stmt
    /// (or equivalent) that records the output type.
    ///
    /// For N = 1, backends may return the single bit directly.
    fn compose_address(&mut self, bits: &[Self::Bit]) -> Self::Bit;

    /// Emit `StorageRead` with a wide address.
    ///
    /// `addr_bits` is the full N-bit address produced by
    /// [`StackPtr::materialize`].  The implementation calls
    /// [`compose_address`](Self::compose_address) to package it, then emits
    /// the read stmt.
    fn emit_read(
        &mut self,
        storage: StorageId,
        ty: TypeId,
        addr_bits: &[Self::Bit],
    ) -> Self::Bit;

    /// Emit `StorageWrite` with a wide address.
    fn emit_write(
        &mut self,
        storage: StorageId,
        src: Self::Bit,
        ty: TypeId,
        addr_bits: &[Self::Bit],
    );
}

// ============================================================================
// Stack pointer with constant-offset folding
// ============================================================================

/// A stack pointer decomposed into a bit-circuit base and an accumulated
/// compile-time constant offset.
///
/// The constant offset is tracked symbolically: `sp.advance(4)` never emits
/// any gates.  Only [`materialize`](StackPtr::materialize) composes the
/// two halves into a concrete address (via [`bc_add`]) — and when the offset
/// is zero, even that is a no-op.
///
/// # Constant folding
///
/// When the base itself is constant (all bits were created via `bc_const`),
/// `bc_add` in the circuit backend will produce constants for every bit of the
/// result, achieving full compile-time folding of `const_base + const_offset`.
#[derive(Clone, Debug)]
pub struct StackPtr<Bit: Clone> {
    /// Base address bits, LSB first.
    base: Vec<Bit>,
    /// Compile-time constant offset accumulated since the last materialisation.
    offset: u64,
}

impl<Bit: Clone + Ord> StackPtr<Bit> {
    /// Create a stack pointer from an existing set of address bits.
    pub fn new(base: Vec<Bit>) -> Self {
        StackPtr { base, offset: 0 }
    }

    /// Create a fully-constant stack pointer.  All bits are emitted as
    /// `bc_const` — maximally foldable.
    pub fn from_const<B: BitCircuitBuilder<Bit = Bit>>(b: &mut B, val: u64, width: usize) -> Self {
        let base = (0..width).map(|i| b.bc_const((val >> i) & 1 != 0)).collect();
        StackPtr { base, offset: 0 }
    }

    /// Advance by a compile-time constant.  **No gates emitted.**
    pub fn advance(&mut self, n: u64) {
        self.offset = self.offset.wrapping_add(n);
    }

    /// Retreat by a compile-time constant.  **No gates emitted.**
    pub fn retreat(&mut self, n: u64) {
        self.offset = self.offset.wrapping_sub(n);
    }

    /// Advance by a variable (circuit-time) amount.  Materialises the current
    /// pointer, adds `amount`, and resets the constant offset to 0.
    pub fn advance_var<B: BitCircuitBuilder<Bit = Bit>>(        &mut self,
        b: &mut B,
        amount: &[Bit],
    ) {
        let current = self.materialize(b);
        self.base = bc_add(b, &current, amount, false);
        self.offset = 0;
    }

    /// Produce the concrete address bits: `base + offset`.
    ///
    /// When `offset == 0` this returns the base directly (zero cost).
    /// When `offset != 0` it emits one `bc_add` with a constant operand.
    pub fn materialize<B: BitCircuitBuilder<Bit = Bit>>(&self, b: &mut B) -> Vec<Bit> {
        if self.offset == 0 {
            return self.base.clone();
        }
        let w = self.base.len();
        let offset_bits: Vec<Bit> = (0..w)
            .map(|i| b.bc_const((self.offset >> i) & 1 != 0))
            .collect();
        bc_add(b, &self.base, &offset_bits, false)
    }

    /// Snapshot the current pointer state (for passing to callees).
    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    /// Bit width of the pointer.
    pub fn width(&self) -> usize {
        self.base.len()
    }
}

// ============================================================================
// Frame layout + call convention helpers
// ============================================================================

/// Layout of one function's stack frame.
///
/// All offsets are in *storage slots* (one slot = one `StorageRead`/`Write`).
/// A `Bit`-typed value occupies 1 slot; a 32-bit integer occupies 32 slots.
#[derive(Clone, Debug)]
pub struct FrameLayout {
    /// `(slot_offset, slot_count, type_id)` for each parameter, in order.
    pub params: Vec<(u64, u64, TypeId)>,
    /// `(slot_offset, slot_count, type_id)` for the return value, if any.
    pub ret: Option<(u64, u64, TypeId)>,
    /// Total frame size in storage slots.
    pub size: u64,
    /// Which storage space the frame lives in.
    pub storage: StorageId,
}

/// Write argument bits into a stack frame via `StorageWrite`.
///
/// For each parameter `i`, writes `args[i][j]` to slot `sp + param_offset + j`.
/// The address is the full N-bit value from [`StackPtr::materialize`],
/// passed directly to [`StorageEmitter::emit_write`] which composes it
/// into a `Vec(N, Bit)` typed var internally.
pub fn frame_push_args<B: StorageEmitter>(
    b: &mut B,
    sp: &StackPtr<B::Bit>,
    layout: &FrameLayout,
    args: &[Vec<B::Bit>],
) {
    for (i, (param_off, slot_count, ty)) in layout.params.iter().enumerate() {
        let mut slot_sp = sp.clone();
        slot_sp.advance(*param_off);
        for j in 0..(*slot_count as usize) {
            let mut addr_sp = slot_sp.clone();
            addr_sp.advance(j as u64);
            let addr = addr_sp.materialize(b);
            b.emit_write(layout.storage, args[i][j].clone(), *ty, &addr);
        }
    }
}

/// Read argument bits from a stack frame via `StorageRead`.
///
/// Returns one `Vec<Bit>` per parameter.
pub fn frame_read_args<B: StorageEmitter>(
    b: &mut B,
    sp: &StackPtr<B::Bit>,
    layout: &FrameLayout,
) -> Vec<Vec<B::Bit>> {
    layout.params.iter().map(|(param_off, slot_count, ty)| {
        let mut slot_sp = sp.clone();
        slot_sp.advance(*param_off);
        (0..(*slot_count as usize)).map(|j| {
            let mut addr_sp = slot_sp.clone();
            addr_sp.advance(j as u64);
            let addr = addr_sp.materialize(b);
            b.emit_read(layout.storage, *ty, &addr)
        }).collect()
    }).collect()
}

/// Write return-value bits into a stack frame.
pub fn frame_write_ret<B: StorageEmitter>(
    b: &mut B,
    sp: &StackPtr<B::Bit>,
    layout: &FrameLayout,
    ret_bits: &[B::Bit],
) {
    if let Some((ret_off, slot_count, ty)) = &layout.ret {
        let mut slot_sp = sp.clone();
        slot_sp.advance(*ret_off);
        for j in 0..(*slot_count as usize) {
            let mut addr_sp = slot_sp.clone();
            addr_sp.advance(j as u64);
            let addr = addr_sp.materialize(b);
            b.emit_write(layout.storage, ret_bits[j].clone(), *ty, &addr);
        }
    }
}

/// Read return-value bits from a stack frame.
pub fn frame_read_ret<B: StorageEmitter>(
    b: &mut B,
    sp: &StackPtr<B::Bit>,
    layout: &FrameLayout,
) -> Vec<B::Bit> {
    match &layout.ret {
        None => vec![],
        Some((ret_off, slot_count, ty)) => {
            let mut slot_sp = sp.clone();
            slot_sp.advance(*ret_off);
            (0..(*slot_count as usize)).map(|j| {
                let mut addr_sp = slot_sp.clone();
                addr_sp.advance(j as u64);
                let addr = addr_sp.materialize(b);
                b.emit_read(layout.storage, *ty, &addr)
            }).collect()
        }
    }
}
