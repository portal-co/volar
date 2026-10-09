// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Shared BitGC circuit structure and the legacy reference gate-equation
//! adapter.
//!
//! The paper's exact small/fast equations are now recorded in
//! `docs/research/bitgc-paper-pinning.md` and implemented separately by
//! `super::pinned`. `ReferenceEquation` still models only the earlier
//! self-consistent reference equations and communication shape; it is not
//! the paper construction and makes no security claim. The legacy
//! `super::small` path remains reference-only and is quarantined from
//! protected-session admission.
//!
//! This module also contains common wire, circuit, and record types used by
//! the pinned driver. Their presence does not imply that the legacy equation
//! or the current SWHE profile is suitable for protected data.
//!
//! TODO(provider-ledger: BITGC-LEDGER-04): replace or explicitly quarantine
//! the remaining `ReferenceEquation` path and add an equation differential
//! test against the paper-pinned driver.

use alloc::vec::Vec;

use super::ring;
use super::swhe::{Ciphertext, Context, Error as SwheError, KeySwitchKeySet};

/// The garbler's per-wire state: zero-label `A0 ∈ Z_p` and the explicit
/// permutation bit `π` ([ZLL+26] §3).
///
/// In the reference adapter `A0` is itself a bit (`{0, 1} ⊂ Z_p`) drawn
/// from the expanded pseudorandom material; the pinned construction gives
/// `A0` full plaintext-space width.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WireState {
    /// Zero-label `A0`, canonical `mod p`.
    pub a0: u64,
    /// Permutation bit `π` (stored explicitly — the §3 decoupling).
    pub pi: bool,
}

impl WireState {
    /// The active label for masked bit `b = v ⊕ π`:
    /// `L = A0 + b·Δ (mod p)`.
    pub fn active_label(&self, masked_bit: bool, delta: &GlobalDelta, p: u64) -> u64 {
        if masked_bit {
            (self.a0 + delta.value()) % p
        } else {
            self.a0
        }
    }
}

/// The garbler's global offset `Δ` with `LSB(Δ) = 1` ([ZLL+26] §3),
/// canonical `mod p`. Because `p` is prime and `Δ < p` is nonzero, `Δ`
/// is invertible `mod p` — the evaluator uses `Δ⁻¹` to unwrap masked
/// payloads from active labels.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GlobalDelta {
    delta: u64,
}

impl GlobalDelta {
    /// Construct from an odd, nonzero, canonical value `mod p`.
    pub fn new(delta: u64, plaintext_modulus: u64) -> Option<Self> {
        if delta == 0 || delta >= plaintext_modulus || delta & 1 == 0 {
            return None;
        }
        Some(Self { delta })
    }

    /// The offset value (odd, canonical mod p).
    pub const fn value(&self) -> u64 {
        self.delta
    }

    /// `Δ⁻¹ mod p`; `None` if `p` is composite and `Δ` shares a factor.
    pub fn inverse(&self, plaintext_modulus: u64) -> Option<u64> {
        ring::inverse_mod(self.delta, plaintext_modulus)
    }
}

/// One communicated stitching record ([ZLL+26] Table 1 shape).
///
/// Records are typed bits; byte packing is a transport-adapter concern
/// (Phase 4.4), not part of this construction module.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Stitch {
    /// One-bit record (small: AND and XOR gates; tiny: AND gates).
    ///
    /// Reference semantics: output-permutation-bit correction —
    /// `π_c = π_c_candidate ⊕ stitch`, where the candidate comes from the
    /// expanded material. This gives the bit a real function (per-gate
    /// permutation-bit randomization) while the paper's exact stitching
    /// equation is pending (BITGC-LEDGER-04).
    OneBit(bool),
    /// Five-bit record (fast: AND gates). Bit 0 is the
    /// permutation-bit correction as in [`Stitch::OneBit`]; bits 1–4 are
    /// reserved for the paper's no-online-multiplication correction bits
    /// and currently carry an evaluator-verified integrity pattern
    /// (`s, ¬s, s, ¬s`). `bits & !0x1F` is always zero.
    FiveBits(u8),
}

impl Stitch {
    /// Communication cost in bits ([ZLL+26] Table 1).
    pub const fn bits(&self) -> usize {
        match self {
            Self::OneBit(_) => 1,
            Self::FiveBits(_) => 5,
        }
    }

    /// The permutation-bit correction carried by the record.
    pub fn correction(&self) -> bool {
        match self {
            Self::OneBit(b) => *b,
            Self::FiveBits(b) => b & 1 == 1,
        }
    }

    /// Build the fast-variant five-bit record from a correction bit.
    pub fn five(correction: bool) -> Self {
        let s = u8::from(correction);
        // Integrity pattern s,¬s,s,¬s in bits 1..=4 (reserved positions).
        let pattern = (s << 1)
            | (u8::from(!correction & true) << 2)
            | (s << 3)
            | (u8::from(!correction & true) << 4);
        Self::FiveBits(s | pattern)
    }

    /// Verify the fast-variant integrity pattern.
    pub fn check_five(&self) -> Result<bool, StitchError> {
        match *self {
            Self::OneBit(_) => Err(StitchError::WrongVariant),
            Self::FiveBits(b) => {
                if b & !0x1F != 0 {
                    return Err(StitchError::NonCanonicalBit);
                }
                let s = b & 1;
                let expected = Self::five(s == 1);
                if *self != expected {
                    return Err(StitchError::NonCanonicalBit);
                }
                Ok(s == 1)
            }
        }
    }
}

/// Errors from the BitGC common layer.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StitchError {
    /// Record stream exhausted mid-circuit.
    Exhausted,
    /// Record variant does not match the gate being processed.
    WrongVariant,
    /// A record payload or pattern is not canonical.
    NonCanonicalBit,
    /// A free-XOR payload exceeded the supported reduction bound.
    PayloadOverflow,
    /// Underlying SWHE failure.
    Crypto(SwheError),
}

impl core::fmt::Display for StitchError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Exhausted => f.write_str("stitching record stream is exhausted"),
            Self::WrongVariant => f.write_str("stitching record variant mismatch"),
            Self::NonCanonicalBit => f.write_str("stitching record is not canonical"),
            Self::PayloadOverflow => f.write_str("free-XOR payload exceeds reduction bound"),
            Self::Crypto(_) => f.write_str("underlying SWHE error"),
        }
    }
}

impl core::error::Error for StitchError {}

impl From<SwheError> for StitchError {
    fn from(e: SwheError) -> Self {
        Self::Crypto(e)
    }
}

/// The evaluator's expanded per-wire material: encryptions of the
/// pseudorandom zero-label `A0` and permutation-bit candidate `π`
/// derived from the homomorphically expanded seed ([LWYY25b] structure).
///
/// Each expanded ciphertext carries its bit in exactly one SIMD slot
/// (the PRG's one-hot extraction); `slot_a0`/`slot_pi` record which.
/// Consumers — wire-state decryption, input-label encryption, and
/// output opening — must use these indices rather than assuming a slot.
#[derive(Clone, Debug)]
pub struct ExpandedWire {
    /// `Enc(A0)`, bit in slot `slot_a0`.
    pub ct_a0: Ciphertext,
    /// `Enc(π_candidate)`, bit in slot `slot_pi`.
    pub ct_pi: Ciphertext,
    /// Slot of the `A0` bit in `ct_a0`.
    pub slot_a0: usize,
    /// Slot of the `π` bit in `ct_pi`.
    pub slot_pi: usize,
}

/// The expanded material attached to one gate: both inputs and the
/// output wire.
#[derive(Clone, Debug)]
pub struct ExpandedGate {
    /// Expanded material for input wire `a`.
    pub a: ExpandedWire,
    /// Expanded material for input wire `b`.
    pub b: ExpandedWire,
    /// Expanded material for the output wire.
    pub out: ExpandedWire,
}

/// A minimal Boolean circuit description for the construction drivers.
///
/// The compiler-facing geometry export (`volar-weaver`, Phase 4) produces
/// the same shape from `BIrBlocks`.
#[derive(Clone, Debug)]
pub struct Circuit {
    /// Number of input wires (`0..inputs`).
    pub inputs: usize,
    /// Gates in topological order.
    pub gates: Vec<Gate>,
    /// Output wire indices.
    pub outputs: Vec<usize>,
}

impl Circuit {
    /// Total wire count (`inputs + gates`).
    pub fn wires(&self) -> usize {
        self.inputs + self.gates.len()
    }

    /// Validate topology: indices in range, topological order, outputs.
    pub fn validate(&self) -> Result<(), StitchError> {
        let wires = self.wires();
        for (i, gate) in self.gates.iter().enumerate() {
            let out = self.inputs + i;
            let (a, b) = gate.operands();
            if a >= out || b >= out || out >= wires {
                return Err(StitchError::WrongVariant);
            }
        }
        if self.outputs.iter().any(|&o| o >= wires) {
            return Err(StitchError::WrongVariant);
        }
        Ok(())
    }

    /// Evaluate in the clear over `0/1` values (test oracle).
    pub fn eval_plain(&self, inputs: &[bool]) -> Result<Vec<bool>, StitchError> {
        if inputs.len() != self.inputs {
            return Err(StitchError::WrongVariant);
        }
        self.validate()?;
        let mut values = alloc::vec![false; self.wires()];
        values[..self.inputs].copy_from_slice(inputs);
        for (i, gate) in self.gates.iter().enumerate() {
            let (a, b) = gate.operands();
            values[self.inputs + i] = match gate {
                Gate::And { .. } => values[a] & values[b],
                Gate::Xor { .. } => values[a] ^ values[b],
                Gate::Not { .. } => !values[a],
            };
        }
        Ok(self.outputs.iter().map(|&o| values[o]).collect())
    }
}

/// One Boolean gate; the output wire is implicit (`inputs + gate_index`).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Gate {
    /// `out = a ∧ b`.
    And {
        /// First input wire.
        a: usize,
        /// Second input wire.
        b: usize,
    },
    /// `out = a ⊕ b`.
    Xor {
        /// First input wire.
        a: usize,
        /// Second input wire.
        b: usize,
    },
    /// `out = ¬a` (logical NOT). Free in BitGC — it is an XOR with the
    /// constant 1 and carries no communication (its one input is also
    /// passed as the second operand for binary-shape callers).
    Not {
        /// Input wire.
        a: usize,
    },
}

impl Gate {
    /// Operand wire indices. A unary gate returns its single input for
    /// both slots (callers that need a binary shape).
    pub const fn operands(&self) -> (usize, usize) {
        match *self {
            Self::And { a, b } | Self::Xor { a, b } => (a, b),
            Self::Not { a } => (a, a),
        }
    }

    /// The implicit output wire for a gate at position `index`.
    pub const fn out_at(&self, inputs: usize, index: usize) -> usize {
        inputs + index
    }
}

/// The per-gate stitching-equation seam (BITGC-LEDGER-04).
///
/// `garble_*` run on the garbler over clear [`WireState`]s and produce
/// the communicated record. `eval_*` run on the evaluator over active
/// label ciphertexts (`Enc(L)`, `L = A0 + b·Δ`) plus the expanded
/// material, and produce `Enc(L_out)`.
///
/// Free-XOR variants may carry **payloads**: an evaluator wire value may
/// encrypt `A0 + m·Δ` with `m ∈ {0, …, K}` a small integer whose parity
/// is the masked bit. AND inputs are parity-reduced before use (see
/// [`parity_coeffs`]); `K` is capped at [`MAX_PAYLOAD`].
pub trait GateEquation {
    /// Garble an AND gate: produce the stitching record. `out.pi` is the
    /// expanded permutation-bit candidate; the record's correction bit
    /// defines the final `π_c = out.pi ⊕ correction`.
    fn garble_and(
        &self,
        gate_index: usize,
        a: &WireState,
        b: &WireState,
        out: &WireState,
    ) -> Stitch;

    /// Evaluate an AND gate homomorphically.
    fn eval_and(
        &self,
        ctx: &Context,
        la: &Ciphertext,
        lb: &Ciphertext,
        expanded: &ExpandedGate,
        delta: &GlobalDelta,
        ksk: &KeySwitchKeySet,
        stitch: &Stitch,
    ) -> Result<Ciphertext, StitchError>;

    /// Garble an XOR gate. Charged variants emit a one-bit record; free
    /// variants emit nothing and require `out.pi == a.pi ⊕ b.pi` and
    /// `out.a0 == a.a0 + b.a0 (mod p)`.
    fn garble_xor(&self, p: u64, a: &WireState, b: &WireState, out: &WireState) -> Option<Stitch>;

    /// Evaluate an XOR gate. Charged variants consume [`Stitch::OneBit`]
    /// and return a bit-payload label; free variants ignore the record
    /// and return a payload-carrying label (`m ≤ 2·incoming`).
    fn eval_xor(
        &self,
        ctx: &Context,
        la: &Ciphertext,
        lb: &Ciphertext,
        expanded: &ExpandedGate,
        delta: &GlobalDelta,
        ksk: &KeySwitchKeySet,
        stitch: Option<&Stitch>,
    ) -> Result<Ciphertext, StitchError>;

    /// Whether XOR gates are charged one bit (small) or free (fast).
    fn xor_is_charged(&self) -> bool;
}

/// Maximum free-XOR payload before parity reduction is required.
pub const MAX_PAYLOAD: usize = 4;

/// Lagrange coefficients of the parity map `m ↦ m mod 2` over
/// `{0, …, k_max}` in `Z_p`, lowest degree first. Requires `p > k_max`.
pub fn parity_coeffs(p: u64, k_max: usize) -> Result<Vec<u64>, StitchError> {
    if k_max == 0 || k_max > (p - 1) as usize {
        return Err(StitchError::PayloadOverflow);
    }
    // c(m) = sum_{j=1,3,5..} prod_{k != j} (m - k) / (j - k)
    let mut coeffs = alloc::vec![0_u64; k_max + 1];
    let points: Vec<u64> = (0..=k_max as u64).collect();
    for (j, &x_j) in points.iter().enumerate() {
        if x_j % 2 == 0 {
            continue; // target value 0 contributes nothing
        }
        // Numerator polynomial prod_{k != j} (m - k), and denominator.
        let mut numerator = alloc::vec![1_u64];
        let mut denominator = 1_u64;
        for (k, &x_k) in points.iter().enumerate() {
            if k == j {
                continue;
            }
            // numerator *= (m - x_k)
            let mut next = alloc::vec![0_u64; numerator.len() + 1];
            for (i, &c) in numerator.iter().enumerate() {
                next[i + 1] = ring::add_mod(next[i + 1], c, p);
                next[i] = ring::sub_mod(next[i], ring::mul_mod(c, x_k, p), p);
            }
            numerator = next;
            let diff = if x_j > x_k {
                x_j - x_k
            } else {
                p - (x_k - x_j)
            };
            denominator = ring::mul_mod(denominator, diff % p, p);
        }
        let inv = ring::inverse_mod(denominator, p).ok_or(StitchError::PayloadOverflow)?;
        for (i, &c) in numerator.iter().enumerate() {
            coeffs[i] = ring::add_mod(coeffs[i], ring::mul_mod(c, inv, p), p);
        }
    }
    Ok(coeffs)
}

#[cfg(any(test, feature = "bitgc-reference"))]
/// Reference gate-equation adapter (small and fast flavors).
///
/// **Not paper-pinned** (BITGC-LEDGER-04) — see module docs. Semantics:
///
/// - **AND**: the evaluator unwraps masked payloads
///   `m = (L − A0)·Δ⁻¹`, parity-reduces to masked bits `b`, computes
///   `v_ab = (b_a ⊕ π_a)(b_b ⊕ π_b)` homomorphically, applies the
///   expanded output candidate and the record's correction bit, and
///   re-wraps as `L_c = A0_c + b_c·Δ`.
/// - **XOR (charged)**: `b_c = b_a ⊕ b_b ⊕ stitch` with
///   `stitch = π_a ⊕ π_b ⊕ π_c` — the record is functional, not padding.
/// - **XOR (free)**: payload add. `A0_out = A0_a + A0_b` is set by the
///   garbler, so `L_a + L_b = A0_out + (b_a + b_b)·Δ` is the output
///   label with payload `m = b_a + b_b` — zero communication, zero
///   multiplications, matching the fast variant's Table 1 row.
#[cfg(any(test, feature = "bitgc-reference"))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ReferenceEquation {
    xor_charged: bool,
}

#[cfg(any(test, feature = "bitgc-reference"))]
impl ReferenceEquation {
    /// Small-variant adapter: XOR gates are charged one bit
    /// ([ZLL+26] §4, Table 1).
    pub const SMALL: Self = Self { xor_charged: true };

    /// Fast-variant adapter: XOR gates are free ([ZLL+26] §5.1, Table 1).
    pub const FAST: Self = Self { xor_charged: false };

    /// Unwrap `Enc(A0 + m·Δ)` to `Enc(m)`, aligning levels first (a
    /// payload-carrying label may sit below the base material after
    /// multiplications).
    fn unwrap(
        &self,
        ctx: &Context,
        label: &Ciphertext,
        expanded: &ExpandedWire,
        delta: &GlobalDelta,
    ) -> Result<Ciphertext, StitchError> {
        let p = ctx.parameters().plaintext_modulus;
        let inv = delta.inverse(p).ok_or(StitchError::NonCanonicalBit)?;
        let ct_a0 = ctx.switch_to(&expanded.ct_a0, label.level())?;
        let diff = ctx.sub(label, &ct_a0)?;
        Ok(ctx.mul_plain(&diff, &ctx.constant_slots(inv))?)
    }

    /// Wrap `Enc(b)` into `Enc(A0 + b·Δ)` *at the online base level* —
    /// the level the next gate's inputs must sit at. Both the scaled
    /// payload and the expanded `A0` are switched there.
    fn wrap(
        &self,
        ctx: &Context,
        masked: &Ciphertext,
        expanded: &ExpandedWire,
        delta: &GlobalDelta,
        online_base: usize,
    ) -> Result<Ciphertext, StitchError> {
        let scaled = ctx.mul_plain(masked, &ctx.constant_slots(delta.value()))?;
        let scaled = ctx.switch_to(&scaled, online_base)?;
        let ct_a0 = ctx.switch_to(&expanded.ct_a0, online_base)?;
        Ok(ctx.add(&scaled, &ct_a0)?)
    }

    /// Homomorphic bit XOR for `0/1` payloads: `a ⊕ b = a + b − 2ab`.
    ///
    /// [`Context::mul`] returns its product one level down; the result is
    /// switched back up so the caller's level invariant (everything at
    /// the current top) holds between gates.
    fn xor_bits(
        &self,
        ctx: &Context,
        a: &Ciphertext,
        b: &Ciphertext,
        ksk: &KeySwitchKeySet,
    ) -> Result<Ciphertext, StitchError> {
        let top = ctx.max_level();
        // Lift operands to the top level before multiplying: the
        // reference adapter's switch-up/down arithmetic is validated
        // end-to-end at top level; mid-chain multiplication noise is a
        // pinned-parameter concern (BITGC-LEDGER-03).
        let a = ctx.switch_to(a, top)?;
        let b = ctx.switch_to(b, top)?;
        let level = top;
        let sum = ctx.add(&a, &b)?;
        let product = ctx.mul(&a, &b, ksk)?;
        let doubled = ctx.mul_plain(&product, &ctx.constant_slots(2))?;
        let out = ctx.sub(&sum, &doubled)?;
        Ok(ctx.switch_to(&out, level)?)
    }

    /// Homomorphic bit complement `1 − x`.
    fn complement(&self, ctx: &Context, x: &Ciphertext) -> Result<Ciphertext, StitchError> {
        let p = ctx.parameters().plaintext_modulus;
        let neg = ctx.mul_plain(x, &ctx.constant_slots(p - 1))?;
        Ok(ctx.add_plain(&neg, &ctx.constant_slots(1))?)
    }

    /// Parity-reduce a payload ciphertext to a bit.
    ///
    /// Payloads are canonical: `L = A0 + m·Δ` holds `mod p` with
    /// `m ∈ {0, …, MAX_PAYLOAD}` because free-XOR label addition sums at
    /// most two bit payloads per gate. The unwrap `(L − A0)·Δ⁻¹`
    /// therefore yields that same canonical `m`, and a degree-`k_max`
    /// interpolation is exact. (Deep free-XOR chains accumulate larger
    /// payloads; the reference adapter caps at [`MAX_PAYLOAD`].)
    fn parity(
        &self,
        ctx: &Context,
        m: &Ciphertext,
        k_max: usize,
        ksk: &KeySwitchKeySet,
    ) -> Result<Ciphertext, StitchError> {
        if k_max == 1 {
            return Ok(m.clone());
        }
        let p = ctx.parameters().plaintext_modulus;
        let coeffs = parity_coeffs(p, k_max)?;
        // Horner with ciphertext multiplications; each `mul` drops a
        // level and is switched back so the next iteration is level-even.
        //
        // `m^0` cannot be `mul_plain(m, 0)`: the zero plaintext has no
        // level (an empty limb vector), which would corrupt later
        // `add_plain`. Use an explicit one-ciphertext instead.
        // Horner: acc starts at Enc(coeffs[k_max]) — *not* one plus the
        // coefficient. Zero coefficients are skipped so level-0 `embed`
        // of an all-zero plaintext never fires.
        let level = m.level();
        let mut acc = ctx.mul_plain(
            &ctx.encrypt_one_like(m)?,
            &ctx.constant_slots(coeffs[k_max]),
        )?;
        for i in (0..k_max).rev() {
            acc = ctx.switch_to(&ctx.mul(&acc, m, ksk)?, level)?;
            if coeffs[i] != 0 {
                acc = ctx.add_plain(&acc, &ctx.constant_slots(coeffs[i]))?;
            }
        }
        Ok(acc)
    }
}

#[cfg(any(test, feature = "bitgc-reference"))]
impl GateEquation for ReferenceEquation {
    fn garble_and(
        &self,
        gate_index: usize,
        a: &WireState,
        b: &WireState,
        out: &WireState,
    ) -> Stitch {
        // Reference correction derivation: deterministic in the gate and
        // wire states (the pinned construction derives it from the
        // expanded material and Δ). Functional, not padding.
        let bit = ((gate_index as u64)
            .wrapping_mul(0x9E37_79B9)
            .wrapping_add(a.a0)
            .wrapping_add(b.a0)
            .wrapping_add(u64::from(out.pi))
            >> 63)
            & 1
            == 1;
        if self.xor_charged {
            Stitch::OneBit(bit)
        } else {
            Stitch::five(bit)
        }
    }

    fn eval_and(
        &self,
        ctx: &Context,
        la: &Ciphertext,
        lb: &Ciphertext,
        expanded: &ExpandedGate,
        delta: &GlobalDelta,
        ksk: &KeySwitchKeySet,
        stitch: &Stitch,
    ) -> Result<Ciphertext, StitchError> {
        let correction = match (self.xor_charged, stitch) {
            (true, Stitch::OneBit(b)) => *b,
            (false, s @ Stitch::FiveBits(_)) => s.check_five()?,
            _ => return Err(StitchError::WrongVariant),
        };
        // Unwrap payloads and reduce to masked bits. Charged-XOR circuits
        // carry bit payloads everywhere; free-XOR circuits may carry small
        // integer payloads from payload-adding XOR gates.
        let k_max = if self.xor_charged { 1 } else { MAX_PAYLOAD };
        let m_a = self.unwrap(ctx, la, &expanded.a, delta)?;
        let m_b = self.unwrap(ctx, lb, &expanded.b, delta)?;
        let b_a = self.parity(ctx, &m_a, k_max, ksk)?;
        let b_b = self.parity(ctx, &m_b, k_max, ksk)?;
        // v_ab = (b_a ⊕ π_a)(b_b ⊕ π_b). The expanded π ciphertexts sit
        // at the online base level, which is the top level at gate time.
        let x_a = self.xor_bits(ctx, &b_a, &expanded.a.ct_pi, ksk)?;
        let x_b = self.xor_bits(ctx, &b_b, &expanded.b.ct_pi, ksk)?;
        let top = ctx.max_level();
        let x_a = ctx.switch_to(&x_a, top)?;
        let x_b = ctx.switch_to(&x_b, top)?;
        let v_ab = ctx.switch_to(&ctx.mul(&x_a, &x_b, ksk)?, top)?;
        // b_c = v_ab ⊕ π_c_candidate ⊕ correction
        let mut b_c = self.xor_bits(ctx, &v_ab, &expanded.out.ct_pi, ksk)?;
        if correction {
            b_c = self.complement(ctx, &b_c)?;
        }
        self.wrap(ctx, &b_c, &expanded.out, delta, ctx.max_level())
    }

    fn garble_xor(&self, p: u64, a: &WireState, b: &WireState, out: &WireState) -> Option<Stitch> {
        if self.xor_charged {
            // Charged correction: stitch = π_a ⊕ π_b ⊕ π_c.
            Some(Stitch::OneBit(a.pi ^ b.pi ^ out.pi))
        } else {
            // Free-XOR structural rule ([ZLL+26] free-XOR contribution).
            debug_assert_eq!(out.pi, a.pi ^ b.pi, "free-XOR requires π_out = π_a ⊕ π_b");
            debug_assert_eq!(
                out.a0,
                (a.a0 + b.a0) % p,
                "free-XOR requires A0_out = A0_a + A0_b"
            );
            None
        }
    }

    fn eval_xor(
        &self,
        ctx: &Context,
        la: &Ciphertext,
        lb: &Ciphertext,
        expanded: &ExpandedGate,
        delta: &GlobalDelta,
        ksk: &KeySwitchKeySet,
        stitch: Option<&Stitch>,
    ) -> Result<Ciphertext, StitchError> {
        if self.xor_charged {
            let bit = match stitch {
                Some(Stitch::OneBit(b)) => *b,
                _ => return Err(StitchError::WrongVariant),
            };
            let b_a = self.unwrap(ctx, la, &expanded.a, delta)?;
            let b_b = self.unwrap(ctx, lb, &expanded.b, delta)?;
            let mut b_c = self.xor_bits(ctx, &b_a, &b_b, ksk)?;
            if bit {
                b_c = self.complement(ctx, &b_c)?;
            }
            self.wrap(ctx, &b_c, &expanded.out, delta, ctx.max_level())
        } else {
            if stitch.is_some() {
                return Err(StitchError::WrongVariant);
            }
            // Free: payload add, L_a + L_b = A0_out + (b_a + b_b)·Δ.
            Ok(ctx.add(la, lb)?)
        }
    }

    fn xor_is_charged(&self) -> bool {
        self.xor_charged
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn global_delta_requires_odd_nonzero() {
        assert!(GlobalDelta::new(5, 101).is_some());
        assert_eq!(GlobalDelta::new(4, 101), None);
        assert_eq!(GlobalDelta::new(0, 101), None);
        assert_eq!(GlobalDelta::new(101, 101), None);
        let delta = GlobalDelta::new(5, 101).unwrap();
        assert_eq!(delta.inverse(101), Some(81)); // 5·81 = 405 ≡ 1 (mod 101)
    }

    #[test]
    fn stitch_record_shapes() {
        assert_eq!(Stitch::OneBit(false).bits(), 1);
        assert_eq!(Stitch::five(true).bits(), 5);
        assert_eq!(Stitch::five(true).check_five(), Ok(true));
        assert_eq!(Stitch::five(false).check_five(), Ok(false));
        assert_eq!(
            Stitch::FiveBits(0x20).check_five(),
            Err(StitchError::NonCanonicalBit)
        );
        assert_eq!(
            Stitch::FiveBits(0b0_0011).check_five(),
            Err(StitchError::NonCanonicalBit)
        );
        assert_eq!(
            Stitch::OneBit(true).check_five(),
            Err(StitchError::WrongVariant)
        );
    }

    #[test]
    fn parity_polynomials() {
        let p = 257;
        for k_max in 2..=MAX_PAYLOAD {
            let coeffs = parity_coeffs(p, k_max).unwrap();
            for m in 0..=k_max as u64 {
                let mut acc = 0;
                let mut power = 1;
                for &c in &coeffs {
                    acc = ring::add_mod(acc, ring::mul_mod(c, power, p), p);
                    power = ring::mul_mod(power, m, p);
                }
                assert_eq!(acc, m % 2, "k_max={k_max} m={m}");
            }
        }
    }

    #[test]
    fn circuit_plain_eval() {
        // (x0 & x1) ^ x2, then ^ (x1 & x2)
        let circuit = Circuit {
            inputs: 3,
            gates: alloc::vec![
                Gate::And { a: 0, b: 1 },
                Gate::And { a: 1, b: 2 },
                Gate::Xor { a: 3, b: 4 },
                Gate::Xor { a: 5, b: 2 },
            ],
            outputs: alloc::vec![6],
        };
        circuit.validate().unwrap();
        for bits in 0..8u8 {
            let inputs = [bits & 1 == 1, bits & 2 == 2, bits & 4 == 4];
            let expect = (inputs[0] & inputs[1]) ^ (inputs[1] & inputs[2]) ^ inputs[2];
            assert_eq!(circuit.eval_plain(&inputs).unwrap(), alloc::vec![expect]);
        }
    }
}
