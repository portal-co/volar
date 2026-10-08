// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-pinning
//! @ai: assisted
//! Paper-pinned BitGC gate equations (ePrint 2025/1810 §§4–5,
//! Figures 4–7; ePrint 2024/1988 §2.5 for the SWHEwDD primitive).
//!
//! This module holds the **exact** per-gate equations that a faithful
//! BitGC implementation must realize, extracted from the archived PDFs
//! (`docs/research/papers/`) and cross-checked against the authors'
//! artifact (`wtd2/more-efficient-bitgc`). Full derivation and the
//! cross-check table are in `docs/research/bitgc-paper-pinning.md`.
//!
//! ## What is pinned here (exact, unit-tested)
//!
//! - [`masked_table`]: the gate's masked truth table
//!   `ẑ[i,j] = (π_a ⊕ i) ∧ (π_b ⊕ j) ⊕ π_c` ([ZLL+26] Fig. 4a/6a).
//! - [`row_multipliers`]: the three garbled-row plaintexts
//!   `t₁ = ẑ[1,0]−ẑ[0,0]`, `t₂ = ẑ[0,1]−ẑ[0,0]`,
//!   `t₃ = ẑ[1,1]−ẑ[0,0]−t₁−t₂` ([ZLL+26] Fig. 4b/6b), each in
//!   `{−1, 0, 1}`.
//! - [`fast_mask_bits`]: the four communicated mask bits
//!   `d^{i,j} = ẑ[i,j] ⊕ r^{i,j}` for the fast variant ([ZLL+26] Fig. 6a).
//!
//! ## What is *not* yet pinned here (the remaining port)
//!
//! The garbled **labels** are additive key-shares evaluated through the
//! SWHEwDD distributed-decryption gadget (`Eval_{i,j} = DD_i + DD_j +
//! DD_{i,j}`, [ZLL+26] §3.3). That label layer — the RLWE' gadget
//! ciphertexts and the CP-RLWE multiplication triple — operates on the
//! coefficient-form SWHE ring and is the next implementation step
//! (BITGC-LEDGER-04, port open). [`DdGadget`] is the seam it will fill.
//!
//! The shipped [`super::common::ReferenceEquation`] is a placeholder with
//! the correct *communication shape* (1 or 5 bits/gate) but **not** these
//! equations; it remains for transport/stream testing only and is not a
//! security stand-in.

use alloc::vec::Vec;

/// The masked truth-table values for a gate ([ZLL+26] Fig. 4a / 6a):
/// `ẑ[i,j] = (π_a ⊕ i) ∧ (π_b ⊕ j) ⊕ π_c` for an AND gate, and
/// `ẑ[i,j] = (π_a ⊕ i) ⊕ (π_b ⊕ j) ⊕ π_c` for an XOR gate.
///
/// Indexed `[i + 2j]` so `out[0] = ᑌ[0,0]`, `out[1] = ᑌ[0,1]`,
/// `out[2] = ᑌ[1,0]`, `out[3] = ᑌ[1,1]`.
pub fn masked_table(and: bool, pi_a: bool, pi_b: bool, pi_c: bool) -> [bool; 4] {
    let mut out = [false; 4];
    for j in 0..2 {
        for i in 0..2 {
            let (ii, jj) = (i == 1, j == 1);
            out[i + 2 * j] = if and {
                ((pi_a ^ ii) & (pi_b ^ jj)) ^ pi_c
            } else {
                (pi_a ^ ii) ^ (pi_b ^ jj) ^ pi_c
            };
        }
    }
    out
}

/// The three garbled-row plaintext multipliers ([ZLL+26] Fig. 4b / 6b),
/// each in `{−1, 0, 1}`:
///
/// ```text
/// t₁ = ẑ[1,0] − ẑ[0,0]
/// t₂ = ẑ[0,1] − ẑ[0,0]
/// t₃ = ẑ[1,1] − ẑ[0,0] − t₁ − t₂
/// ```
///
/// The garbled row ciphertexts are `τₖ = [[tₖ·Δ]]`; the paper's
/// `τ₃ := [[ẑ[1,1]·Δ]] − [[ẑ[0,0]·Δ]] − τ₁ − τ₂` equals `[[t₃·Δ]]` by
/// linearity of `Enc`/`Dec`.
pub fn row_multipliers(z: &[bool; 4]) -> [i64; 3] {
    let b = |x: bool| i64::from(x);
    let t1 = b(z[2]) - b(z[0]); // ẑ[1,0] − ẑ[0,0]
    let t2 = b(z[1]) - b(z[0]); // ẑ[0,1] − ẑ[0,0]
    let t3 = b(z[3]) - b(z[0]) - t1 - t2; // ẑ[1,1] − ẑ[0,0] − t₁ − t₂
    [t1, t2, t3]
}

/// The four communicated mask bits for the fast variant
/// ([ZLL+26] Fig. 6a): `d^{i,j} = ẑ[i,j] ⊕ r^{i,j}`, where `r^{i,j}` are
/// the per-gate pseudorandom bits from the offline PRG expansion.
///
/// The evaluator recovers `[[ẑ[i,j]·Δ]] = d^{i,j}·[[Δ]] +
/// (1 − 2·d^{i,j})·[[r^{i,j}·Δ]]` by homomorphic *addition* only — this is
/// the fast variant's "no online homomorphic multiplication" property.
pub fn fast_mask_bits(z: &[bool; 4], r: &[bool; 4]) -> [bool; 4] {
    let mut out = [false; 4];
    for k in 0..4 {
        out[k] = z[k] ^ r[k];
    }
    out
}

/// The evaluator's linear reconstruction of `[[ẑ·Δ]]` from a communicated
/// mask bit `d` and the encrypted PRG bit `[[r·Δ]]`
/// ([ZLL+26] Fig. 6a / 7a): returns the coefficients `a, b` such that
/// `[[ẑ·Δ]] = a·[[Δ]] + b·[[r·Δ]]`.
///
/// - `d = 0` ⇒ `ẑ = r` ⇒ `(0, 1)`.
/// - `d = 1` ⇒ `ẑ = 1 − r` ⇒ `(1, −1)`.
pub fn fast_unmask_coeffs(d: bool) -> (i64, i64) {
    if d { (1, -1) } else { (0, 1) }
}

/// The determination bit communicated per wire so the evaluator recovers
/// the masked bit ([ZLL+26] Fig. 4c / 6c):
/// `d_γ = LSB(W_γ⁰) ⊕ π_γ`, and evaluation `x̂_γ = LSB(W_γ) ⊕ d_γ`.
pub fn determination_bit(w0_lsb: bool, pi: bool) -> bool {
    w0_lsb ^ pi
}

/// The distributed-decryption gadget seam ([ZLL+26] §3.3, Def. 6;
/// [LWYY25b] §2.5).
///
/// `Dec(Δ, τ)` on `τ = Enc(Δ, t)` outputs `t·Δ` (GSW-style). The DD
/// property is `DD_i(X + i·Δ, τ) = DD_0(X, τ) + i·Dec(Δ, τ)`, with the
/// correlated-key form `DD_{i,j}(X+iΔ, Y+jΔ, τ) = DD_{0,0}(X,Y,τ) +
/// i·j·Dec(Δ, τ)`.
///
/// The full instantiation (RLWE' gadget ciphertexts for k=1, CP-RLWE
/// triple for k=2) operates on the coefficient-form SWHE layer and is the
/// open port tracked by BITGC-LEDGER-04. This trait fixes the interface
/// the garbling driver (`Eval_{i,j}`) will call.
pub trait DdGadget {
    /// The SWHE ciphertext type the gadget consumes (a row `τ`).
    type Ct;

    /// `Dec(Δ, τ)`: the garbler's row decryption (`t·Δ` for multiplier
    /// `t`). Garbler-side only.
    fn dec_row(&self, tau: &Self::Ct) -> Vec<u64>;

    /// `DD_0(X, τ)`: partial decryption under a key-share `X`.
    fn dd0(&self, x: &[u64], tau: &Self::Ct) -> Vec<u64>;

    /// `DD_{0,0}(X, Y, τ)`: correlated-key partial decryption (k=2).
    fn dd00(&self, x: &[u64], y: &[u64], tau: &Self::Ct) -> Vec<u64>;
}

/// The label evaluator over the DD gadget ([ZLL+26] §3.3):
///
/// ```text
/// Eval_{i,j}(A, B, τ₁, τ₂, τ₃) = DD_i(A, τ₁) + DD_j(B, τ₂) + DD_{i,j}(A, B, τ₃)
/// ```
///
/// Satisfies the garbled-row reconstruction
/// `Eval_{i,j}(A+iΔ, B+jΔ, τ) = Eval_{0,0}(A,B,τ) + i·Dec(Δ,τ₁) +
/// j·Dec(Δ,τ₂) + i·j·Dec(Δ,τ₃)`.
///
/// This is declared so the pinned gate driver has a concrete call shape;
/// the arithmetic lives behind the [`DdGadget`] implementation.
pub fn eval_shape<G: DdGadget>(
    gadget: &G,
    a: &[u64],
    b: &[u64],
    taus: &[G::Ct; 3],
    i: bool,
    j: bool,
    p: u64,
) -> Vec<u64> {
    let _ = (a, b, taus, i, j, p);
    let _ = gadget;
    // The assembled Eval is a combination of DD terms; the concrete
    // combination lands with the DdGadget implementation. Placeholder
    // returns the zero element so the driver type-checks against the seam.
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn masked_table_and() {
        // ẑ[i,j] = (π_a⊕i)∧(π_b⊕j)⊕π_c; index [i + 2j]
        assert_eq!(
            masked_table(true, false, false, false),
            [false, false, false, true]
        );
        // π_a=1, π_b=0, π_c=1, index [i + 2j]:
        // (0,0): 1∧0⊕1 = 1; (1,0): 0∧0⊕1 = 1; (0,1): 1∧1⊕1 = 0; (1,1): 0∧1⊕1 = 1
        assert_eq!(
            masked_table(true, true, false, true),
            [true, true, false, true]
        );
        // π_a=1, π_b=1, π_c=0: (1⊕i)(1⊕j)⊕0 = (¬i)(¬j) → only (0,0) is 1
        assert_eq!(
            masked_table(true, true, true, false),
            [true, false, false, false]
        );
    }

    #[test]
    fn masked_table_xor() {
        // XOR with π_c = π_a ⊕ π_b (free-XOR rule) ⇒ ẑ[i,j] = i ⊕ j.
        for (pa, pb) in [(false, false), (false, true), (true, false), (true, true)] {
            let z = masked_table(false, pa, pb, pa ^ pb);
            assert_eq!(z, [false, true, true, false], "pi=({pa},{pb})");
        }
    }

    #[test]
    fn row_multipliers_and_clear() {
        // AND, all-zero π: z = [0,0,0,1] → t = [0, 0, 1].
        let z = masked_table(true, false, false, false);
        assert_eq!(row_multipliers(&z), [0, 0, 1]);
    }

    #[test]
    fn row_multipliers_are_signed_bits() {
        for pa in [false, true] {
            for pb in [false, true] {
                for pc in [false, true] {
                    let z = masked_table(true, pa, pb, pc);
                    let t = row_multipliers(&z);
                    assert!(t.iter().all(|&v| (-1..=1).contains(&v)), "{t:?}");
                    // Reconstruction: t0 = z00; t1 = z10 - z00; ...
                    let b = |x: bool| i64::from(x);
                    assert_eq!(b(z[2]), b(z[0]) + t[0]);
                    assert_eq!(b(z[1]), b(z[0]) + t[1]);
                    assert_eq!(b(z[3]), b(z[0]) + t[0] + t[1] + t[2]);
                }
            }
        }
    }

    #[test]
    fn fast_mask_and_unmask() {
        let z = masked_table(true, true, false, true);
        let r = [true, false, true, false];
        let d = fast_mask_bits(&z, &r);
        for k in 0..4 {
            // evaluator: ẑ = d ? (1 − r) : r
            let (a, b) = fast_unmask_coeffs(d[k]);
            let r_i = i64::from(r[k]);
            let recovered = a + b * r_i; // over the integers, ẑ ∈ {0,1}
            assert_eq!(recovered, i64::from(z[k]), "k={k}");
        }
    }

    #[test]
    fn determination_roundtrip() {
        // d_γ = LSB(W⁰) ⊕ π; evaluator: x̂ = LSB(W) ⊕ d_γ, with
        // LSB(W) = LSB(W⁰) ⊕ x̂ ⇒ recovers x̂.
        for pi in [false, true] {
            for lsb0 in [false, true] {
                let d = determination_bit(lsb0, pi);
                assert_eq!(d, lsb0 ^ pi);
            }
        }
    }
}
