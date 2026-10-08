// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-pinning
//! @ai: assisted
//! Paper-pinned BitGC construction (ePrint 2025/1810 §§4–5, Figures 4–7;
//! ePrint 2024/1988 §2.5 for the SWHEwDD primitive).
//!
//! This module implements the **actual** BitGC-small and BitGC-fast gate
//! semantics, superseding the placeholder
//! [`super::common::ReferenceEquation`]. Equations are extracted in
//! `docs/research/bitgc-paper-pinning.md` and cross-checked against the
//! authors' artifact (`wtd2/more-efficient-bitgc`) — they agree.
//!
//! # Label algebra ([ZLL+26] §3, Fig. 4; [LWYY25b] §2.5)
//!
//! Labels live in `R_p = Z_p[X]/(X^N+1)` in coefficient form (LSB =
//! constant coefficient). `Δ` is the SWHE secret key, a ternary ring
//! element with `LSB(Δ) = 1`. Each wire `w` has a permutation bit `π_w`
//! and a zero-label `W_w^0`; the active label for masked bit `x̂ = v⊕π` is
//!
//! ```text
//! W_w = W_w^{π_w} + x̂_w·Δ,   where W_w^{π} := W_w^0 + π·Δ
//! ```
//!
//! (so `LSB(W_w) = LSB(W_w^0) ⊕ x̂_w`, because `LSB(Δ) = 1`).
//!
//! # Gate evaluation ([ZLL+26] Fig. 4–5, Fig. 6–7)
//!
//! For a gate `(α,β,γ)` with masked truth table `ẑ[i,j]` ([`masked_table`])
//! and row multipliers `t₁,t₂,t₃` ([`row_multipliers`]), the evaluator's
//! output label is
//!
//! ```text
//! W_γ = Eval_{x̂_α, x̂_β}(W_α, W_β, τ₁,τ₂,τ₃) + PRF_K(γ)
//!     = DD_{x̂_α}(W_α,τ₁) + DD_{x̂_β}(W_β,τ₂) + DD_{x̂_α,x̂_β}(W_α,W_β,τ₃) + PRF_K(γ)
//! ```
//!
//! and the masked output bit is `x̂_γ = LSB(W_γ) ⊕ d_γ` with the
//! communicated determination bit `d_γ = LSB(W_γ^0) ⊕ π_γ`.
//!
//! # The reference DD gadget (honesty boundary)
//!
//! The paper's DD property ([ZLL+26] Def. 6) is realized here by an
//! **exact `R_p` gadget**: a row ciphertext `τ_k = (c0, c1)` over `R_p`
//! with `c0 + c1·Δ = t_k·Δ` (exact, no noise), and
//!
//! ```text
//! DD_i(A, τ)     := c1·A + i·c0                       (k=1)
//! DD_{i,j}(A,B,τ):= c1·HssMul_{i,j}(A,B) + i·j·c0     (k=2)
//! ```
//!
//! for which the DD identities hold **exactly** over `R_p`:
//! `DD_i(A0+iΔ, τ) − DD_0(A0, τ) = i·Dec(Δ,τ)` and
//! `DD_{i,j}(A0+iΔ, B0+jΔ, τ) − DD_{0,0}(A0,B0,τ) = i·j·Dec(Δ,τ)`.
//!
//! This gadget is **cryptographically hollow**: `c0, c1` over `R_p` leak
//! `t·Δ` to anyone holding `Δ`'s complement, and there is no lattice
//! noise. It exists to pin and test the *garbling algebra* — the labels,
//! masked bits, determination bits, and truth-table reconstruction —
//! independently of the lattice layer. The lattice SWHEwDD instantiation
//! (RLWE' gadget ciphertexts for k=1, CP-RLWE triple for k=2, per
//! [ZLL+26] App. A) plugs into the same [`DdGadget`] seam and is tracked
//! by BITGC-LEDGER-04 (port) and BITGC-LEDGER-03 (security review).
//!
//! **Do not use for protected data.**

use alloc::vec::Vec;

use super::ring;

/// A ring element of `R_p` in coefficient form (`Vec<u64>`, length = ring
/// degree). Labels, Δ, and DD terms all use this.
pub type LabelPoly = Vec<u64>;

/// The masked truth-table values for a gate ([ZLL+26] Fig. 4a / 6a):
/// `ẑ[i,j] = (π_a ⊕ i) ∧ (π_b ⊕ j) ⊕ π_c` for AND, and
/// `ẑ[i,j] = (π_a ⊕ i) ⊕ (π_b ⊕ j) ⊕ π_c` for XOR.
///
/// Indexed `[i + 2j]`: `out[0] = ẑ[0,0]`, `out[1] = ẑ[1,0]`,
/// `out[2] = ẑ[0,1]`, `out[3] = ẑ[1,1]`.
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
/// Row ciphertexts are `τₖ = [[tₖ·Δ]]`; the paper's
/// `τ₃ := [[ẑ[1,1]Δ]] − [[ẑ[0,0]Δ]] − τ₁ − τ₂` is `[[t₃·Δ]]` by linearity.
pub fn row_multipliers(z: &[bool; 4]) -> [i64; 3] {
    let b = |x: bool| i64::from(x);
    // Convention: z[i + 2j] = ẑ[i,j].
    let t1 = b(z[1]) - b(z[0]); // ẑ[1,0] − ẑ[0,0]
    let t2 = b(z[2]) - b(z[0]); // ẑ[0,1] − ẑ[0,0]
    let t3 = b(z[3]) - b(z[0]) - t1 - t2; // ẑ[1,1] − ẑ[0,0] − t₁ − t₂
    [t1, t2, t3]
}

/// The four communicated mask bits for the fast variant
/// ([ZLL+26] Fig. 6a): `d^{i,j} = ẑ[i,j] ⊕ r^{i,j}`.
pub fn fast_mask_bits(z: &[bool; 4], r: &[bool; 4]) -> [bool; 4] {
    let mut out = [false; 4];
    for k in 0..4 {
        out[k] = z[k] ^ r[k];
    }
    out
}

/// The evaluator's linear reconstruction of `[[ẑ·Δ]]` from a
/// communicated mask bit `d` and the encrypted PRG bit `[[r·Δ]]`
/// ([ZLL+26] Fig. 6a / 7a): coefficients `(a, b)` with
/// `[[ẑ·Δ]] = a·[[Δ]] + b·[[r·Δ]]`. `d=0 ⇒ (0, 1)`; `d=1 ⇒ (1, −1)`.
pub fn fast_unmask_coeffs(d: bool) -> (i64, i64) {
    if d { (1, -1) } else { (0, 1) }
}

/// The determination bit communicated per wire ([ZLL+26] Fig. 4c / 6c):
/// `d_γ = LSB(W_γ⁰) ⊕ π_γ`; evaluation recovers `x̂_γ = LSB(W_γ) ⊕ d_γ`.
pub fn determination_bit(w0_lsb: bool, pi: bool) -> bool {
    w0_lsb ^ pi
}

// ─────────────────────────────────────────────────────────────────────
// Δ and labels
// ─────────────────────────────────────────────────────────────────────

/// The global offset Δ: the SWHE secret key, ternary, `LSB(Δ) = 1`
/// ([LWYY25b] §2.5). Garbler-held; never sent.
#[derive(Clone, Debug)]
pub struct Delta {
    /// Coefficient form, canonical `mod p`; `coeffs[0]` is odd.
    pub coeffs: LabelPoly,
    /// The plaintext modulus `p` (odd).
    pub p: u64,
}

impl Delta {
    /// Build Δ from ternary coefficients in `{−1, 0, 1}`, forcing
    /// `coeffs[0] = 1` so `LSB(Δ) = 1`.
    pub fn from_ternary(coeffs: &[i64], p: u64) -> Self {
        let mut c: Vec<u64> = coeffs
            .iter()
            .map(|&v| if v < 0 { p - 1 } else { (v as u64) % p })
            .collect();
        if c.is_empty() {
            c.push(1);
        }
        c[0] = 1;
        Self { coeffs: c, p }
    }

    /// `−Δ` (additive inverse) in coefficient form.
    pub fn neg(&self) -> LabelPoly {
        self.coeffs.iter().map(|&c| if c == 0 { 0 } else { self.p - c }).collect()
    }
}

/// A wire's garbler-side label state: the zero-label `W^0` and the
/// permutation bit `π`. The one-label is `W^1 = W^0 + Δ` in the cleaned-up
/// convention used here (see module docs; equivalent to the paper's
/// `W^0 + (−1)^{π_0}·Δ` with the sign absorbed into the choice of `W^0`).
#[derive(Clone, Debug)]
pub struct WireLabel {
    /// Zero-label, coefficients `mod p`.
    pub w0: LabelPoly,
    /// Permutation bit.
    pub pi: bool,
}

impl WireLabel {
    /// The `W^π` base label (`W^0 + π·Δ`).
    pub fn base(&self, delta: &Delta) -> LabelPoly {
        if self.pi {
            ring::add_coeffs(&self.w0, &delta.coeffs, delta.p)
        } else {
            self.w0.clone()
        }
    }

    /// The active label for masked bit `x̂`: `W^π + x̂·Δ`.
    pub fn active(&self, masked: bool, delta: &Delta) -> LabelPoly {
        let base = self.base(delta);
        if masked {
            ring::add_coeffs(&base, &delta.coeffs, delta.p)
        } else {
            base
        }
    }

    /// `LSB(W^0)`: constant coefficient mod 2.
    pub fn w0_lsb(&self) -> bool {
        self.w0[0] % 2 == 1
    }
}

/// `LSB(W)` of an evaluator-side label (constant coefficient mod 2).
pub fn label_lsb(w: &LabelPoly) -> bool {
    !w.is_empty() && w[0] % 2 == 1
}

// ─────────────────────────────────────────────────────────────────────
// The reference DD gadget (exact over R_p; cryptographically hollow)
// ─────────────────────────────────────────────────────────────────────

/// A row ciphertext `τ = (c0, c1)` over `R_p` with `c0 + c1·Δ = t·Δ`
/// exactly (the reference DD gadget's ciphertext form).
///
/// Generated by the garbler from a row multiplier `t ∈ {−1, 0, 1}`:
/// sample `c1` uniform, set `c0 := t·Δ − c1·Δ (mod p)`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RowCt {
    /// `c0 = t·Δ − c1·Δ`.
    pub c0: LabelPoly,
    /// `c1`, uniform.
    pub c1: LabelPoly,
}

/// The reference DD gadget over `R_p` (exact DD identities; see module
/// docs for the honesty boundary). Holds Δ (garbler-side row generation)
/// and the CP-RLWE-style multiplication triple for the k=2 term.
#[derive(Clone, Debug)]
pub struct ReferenceDd {
    p: u64,
    /// The garbler's Δ (kept for row generation and `dec_row`).
    delta: Delta,
    /// CP-RLWE-style triple `(U, V, W)` with `Δ·U + V = 0` and
    /// `Δ²·U + W = −Δ` (mod p), so that `HssMul` below produces a valid
    /// share of the product label. `U` uniform.
    triple: (LabelPoly, LabelPoly, LabelPoly),
}

impl ReferenceDd {
    /// Build the gadget from Δ and uniform randomness for `c1`s and `U`.
    ///
    /// `random` fills coefficient vectors; callers must use a CSPRNG for
    /// any non-test use (test-only `ZeroNoise`-style sources are fine for
    /// determinism but produce no security).
    pub fn new(delta: Delta, degree: usize, random: &mut dyn FnMut(&mut [u64])) -> Self {
        let p = delta.p;
        let mut uniform = || {
            let mut v = alloc::vec![0_u64; degree];
            random(&mut v);
            v.iter_mut().for_each(|x| *x %= p);
            v
        };
        let u = uniform();
        // V = −Δ·U;  W = −Δ − Δ²·U  (mod p), so ΔU + V = 0, Δ²U + W = −Δ.
        let du = ring::negacyclic_mul_coeffs(&delta.coeffs, &u, p);
        let v = du.iter().map(|&x| if x == 0 { 0 } else { p - x }).collect();
        let d2 = ring::negacyclic_mul_coeffs(&delta.coeffs, &delta.coeffs, p);
        let d2u = ring::negacyclic_mul_coeffs(&d2, &u, p);
        let w = ring::sub_coeffs(&delta.neg(), &d2u, p);
        Self { p, delta, triple: (u, v, w) }
    }

    /// The plaintext modulus.
    pub const fn p(&self) -> u64 {
        self.p
    }

    /// Generate a row ciphertext for multiplier `t ∈ {−1, 0, 1}`.
    ///
    /// `c1` uniform; `c0 := t·Δ − c1·Δ (mod p)` so `c0 + c1·Δ = t·Δ`.
    pub fn row(&self, t: i64, random: &mut dyn FnMut(&mut [u64])) -> RowCt {
        let p = self.p;
        let degree = self.delta.coeffs.len();
        let mut c1 = alloc::vec![0_u64; degree];
        random(&mut c1);
        c1.iter_mut().for_each(|x| *x %= p);
        // c0 = t·Δ − c1·Δ
        let td = if t >= 0 {
            delta_scaled(&self.delta, t as u64, p)
        } else {
            delta_scaled(&self.delta, (-t) as u64, p)
                .iter()
                .map(|&x| if x == 0 { 0 } else { p - x })
                .collect()
        };
        let c1d = ring::negacyclic_mul_coeffs(&c1, &self.delta.coeffs, p);
        let c0 = ring::sub_coeffs(&td, &c1d, p);
        RowCt { c0, c1 }
    }

    /// `Dec(Δ, τ) = c0 + c1·Δ = t·Δ` (garbler-side).
    pub fn dec_row(&self, tau: &RowCt) -> LabelPoly {
        let c1d = ring::negacyclic_mul_coeffs(&tau.c1, &self.delta.coeffs, self.p);
        ring::add_coeffs(&tau.c0, &c1d, self.p)
    }

    /// `DD_0(A, τ) = c1·A` (k=1, exact over `R_p`).
    pub fn dd0(&self, a: &LabelPoly, tau: &RowCt) -> LabelPoly {
        ring::negacyclic_mul_coeffs(&tau.c1, a, self.p)
    }

    /// `DD_i(A, τ) = c1·A + i·c0` (k=1).
    pub fn dd_i(&self, a: &LabelPoly, i: bool, tau: &RowCt) -> LabelPoly {
        let base = self.dd0(a, tau);
        if i { ring::add_coeffs(&base, &tau.c0, self.p) } else { base }
    }

    /// The k=2 share product with the CP-RLWE triple
    /// ([ZLL+26] §6 / [ILL25b]; artifact `HSSMul`):
    ///
    /// ```text
    /// HssMul_{i,j}(A, B) = A·B·U + (i·B + j·A)·V − i·j·W
    /// ```
    ///
    /// With `A = A0 + iΔ`, `B = B0 + jΔ` and the triple's identities, this
    /// equals `U·A0·B0 + i·j·Δ` (mod p): a share of the product label with
    /// product masked bit.
    pub fn hss_mul(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool) -> LabelPoly {
        let p = self.p;
        let (u, v, w) = &self.triple;
        let ab = ring::negacyclic_mul_coeffs(a, b, p);
        let mut out = ring::negacyclic_mul_coeffs(&ab, u, p);
        if i {
            let bv = ring::negacyclic_mul_coeffs(b, v, p);
            out = ring::add_coeffs(&out, &bv, p);
        }
        if j {
            let av = ring::negacyclic_mul_coeffs(a, v, p);
            out = ring::add_coeffs(&out, &av, p);
        }
        if i && j {
            out = ring::sub_coeffs(&out, w, p);
        }
        out
    }

    /// `DD_{i,j}(A, B, τ) = c1·HssMul_{i,j}(A,B) + i·j·c0` (k=2).
    pub fn dd_ij(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool, tau: &RowCt) -> LabelPoly {
        let share = self.hss_mul(a, i, b, j);
        let base = ring::negacyclic_mul_coeffs(&tau.c1, &share, self.p);
        if i && j { ring::add_coeffs(&base, &tau.c0, self.p) } else { base }
    }

    /// The gate evaluator ([ZLL+26] §3.3):
    /// `Eval_{i,j}(A, B, τ) = DD_i(A,τ₁) + DD_j(B,τ₂) + DD_{i,j}(A,B,τ₃)`.
    pub fn eval(
        &self,
        a: &LabelPoly,
        i: bool,
        b: &LabelPoly,
        j: bool,
        taus: &[RowCt; 3],
    ) -> LabelPoly {
        let mut out = self.dd_i(a, i, &taus[0]);
        out = ring::add_coeffs(&out, &self.dd_i(b, j, &taus[1]), self.p);
        ring::add_coeffs(&out, &self.dd_ij(a, i, b, j, &taus[2]), self.p)
    }
}

fn delta_scaled(delta: &Delta, k: u64, p: u64) -> LabelPoly {
    delta.coeffs.iter().map(|&c| ring::mul_mod(c, k, p)).collect()
}

// ─────────────────────────────────────────────────────────────────────
// Gate drivers (Fig. 4–7)
// ─────────────────────────────────────────────────────────────────────

/// A garbled gate's communicated material for the pinned construction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateRecord {
    /// BitGC-small AND: 1 determination bit.
    SmallAnd(bool),
    /// BitGC-small XOR: 1 mask bit (`d_γ = π_α ⊕ π_β ⊕ π_γ`).
    SmallXor(bool),
    /// BitGC-fast AND: 4 PRG mask bits + 1 determination bit.
    FastAnd { masks: [bool; 4], det: bool },
    /// BitGC-fast XOR: free (no record emitted).
    FastXor,
}

impl GateRecord {
    /// Communication in bits ([ZLL+26] Table 1).
    pub const fn bits(&self) -> usize {
        match self {
            Self::SmallAnd(_) | Self::SmallXor(_) => 1,
            Self::FastAnd { .. } => 5,
            Self::FastXor => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_delta(degree: usize, p: u64) -> Delta {
        let mut coeffs = alloc::vec![0_i64; degree];
        coeffs[0] = 1; // LSB(Δ)=1
        coeffs[1 % degree] = -1;
        if degree > 2 {
            coeffs[2] = 1;
        }
        Delta::from_ternary(&coeffs, p)
    }

    fn test_random(seed: u64) -> impl FnMut(&mut [u64]) {
        let mut state = seed;
        move |out: &mut [u64]| {
            for v in out.iter_mut() {
                state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                *v = state >> 33;
            }
        }
    }

    #[test]
    fn masked_table_and() {
        assert_eq!(
            masked_table(true, false, false, false),
            [false, false, false, true]
        );
        // π_a=1, π_b=0, π_c=1, index [i + 2j]:
        // (0,0): 1∧0⊕1 = 1; (1,0): 0∧0⊕1 = 1; (0,1): 1∧1⊕1 = 0; (1,1): 0∧1⊕1 = 1
        assert_eq!(masked_table(true, true, false, true), [true, true, false, true]);
        // π_a=1, π_b=1, π_c=0: (1⊕i)(1⊕j) = (¬i)(¬j) → only (0,0) is 1
        assert_eq!(masked_table(true, true, true, false), [true, false, false, false]);
    }

    #[test]
    fn masked_table_xor() {
        // Free-XOR rule π_c = π_a ⊕ π_b ⇒ ẑ[i,j] = i ⊕ j.
        for (pa, pb) in [(false, false), (false, true), (true, false), (true, true)] {
            assert_eq!(masked_table(false, pa, pb, pa ^ pb), [false, true, true, false]);
        }
    }

    #[test]
    fn row_multipliers_reconstruct() {
        // AND, all-zero π: only ẑ[1,1] = 1 → z = [0,0,0,1], t = [0,0,1].
        assert_eq!(row_multipliers(&masked_table(true, false, false, false)), [0, 0, 1]);
        // AND, π_a=1: ẑ[i,j] = (¬i)∧j → z = [0, 0, 1, 0] → t = [0, 1, −1].
        assert_eq!(row_multipliers(&masked_table(true, true, false, false)), [0, 1, -1]);
        // General reconstruction for all π combinations.
        for pa in [false, true] {
            for pb in [false, true] {
                for pc in [false, true] {
                    let z = masked_table(true, pa, pb, pc);
                    let t = row_multipliers(&z);
                    assert!(t.iter().all(|&v| (-1..=1).contains(&v)));
                    let b = |x: bool| i64::from(x);
                    // ẑ[i,j] = ẑ00 + i·t1 + j·t2 + i·j·t3 (bilinear interp.)
                    for j in 0..2usize {
                        for i in 0..2usize {
                            // ẑ[i,j] = ẑ00 + i·t1 + j·t2 + i·j·t3
                            let v = b(z[0])
                                + t[0] * i64::from(i == 1)
                                + t[1] * i64::from(j == 1)
                                + t[2] * i64::from(i == 1 && j == 1);
                            assert_eq!(v, b(z[i + 2 * j]), "pa={pa} pb={pb} pc={pc} i={i} j={j}");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn fast_mask_roundtrip() {
        let z = masked_table(true, true, false, true);
        let r = [true, false, true, false];
        let d = fast_mask_bits(&z, &r);
        for k in 0..4 {
            let (a, b) = fast_unmask_coeffs(d[k]);
            assert_eq!(a + b * i64::from(r[k]), i64::from(z[k]), "k={k}");
        }
    }

    #[test]
    fn dd_linear_identity() {
        // DD_i(A0 + iΔ, τ) − DD_0(A0, τ) = i·Dec(Δ, τ) exactly over R_p.
        let p = 97u64;
        let degree = 8;
        let delta = test_delta(degree, p);
        let mut rnd = test_random(0x1234);
        let dd = ReferenceDd::new(delta.clone(), degree, &mut rnd);
        let tau = dd.row(1, &mut rnd); // t = 1
        let a0: LabelPoly = (0..degree).map(|i| (i as u64 * 37 + 5) % p).collect();
        let m = dd.dec_row(&tau);
        for i in [false, true] {
            let a = if i { ring::add_coeffs(&a0, &delta.coeffs, p) } else { a0.clone() };
            let lhs = dd.dd_i(&a, i, &tau);
            let rhs = ring::add_coeffs(&dd.dd0(&a0, &tau), &if i { m.clone() } else { alloc::vec![0; degree] }, p);
            assert_eq!(lhs, rhs, "i={i}");
        }
    }

    #[test]
    fn dd_correlated_identity() {
        // DD_{i,j}(A0+iΔ, B0+jΔ, τ) − DD_{0,0}(A0,B0,τ) = i·j·Dec(Δ,τ).
        let p = 97u64;
        let degree = 8;
        let delta = test_delta(degree, p);
        let mut rnd = test_random(0xbeef);
        let dd = ReferenceDd::new(delta.clone(), degree, &mut rnd);
        let tau = dd.row(-1, &mut rnd); // t = −1
        let a0: LabelPoly = (0..degree).map(|i| (i as u64 * 11 + 3) % p).collect();
        let b0: LabelPoly = (0..degree).map(|i| (i as u64 * 23 + 7) % p).collect();
        let m = dd.dec_row(&tau);
        let base = dd.dd_ij(&a0, false, &b0, false, &tau);
        for (i, j) in [(false, false), (true, false), (false, true), (true, true)] {
            let a = if i { ring::add_coeffs(&a0, &delta.coeffs, p) } else { a0.clone() };
            let b = if j { ring::add_coeffs(&b0, &delta.coeffs, p) } else { b0.clone() };
            let lhs = dd.dd_ij(&a, i, &b, j, &tau);
            let corr = if i && j { m.clone() } else { alloc::vec![0; degree] };
            let rhs = ring::add_coeffs(&base, &corr, p);
            assert_eq!(lhs, rhs, "i={i} j={j}");
        }
    }

    #[test]
    fn hss_mul_product_share() {
        // HssMul(A0+iΔ, B0+jΔ) = U·A0·B0 + i·j·Δ (mod p).
        let p = 97u64;
        let degree = 8;
        let delta = test_delta(degree, p);
        let mut rnd = test_random(77);
        let dd = ReferenceDd::new(delta.clone(), degree, &mut rnd);
        let a0: LabelPoly = (0..degree).map(|i| (i as u64 * 13 + 1) % p).collect();
        let b0: LabelPoly = (0..degree).map(|i| (i as u64 * 29 + 2) % p).collect();
        let u = &dd.triple.0;
        let base_share = {
            let ab = ring::negacyclic_mul_coeffs(&a0, &b0, p);
            ring::negacyclic_mul_coeffs(&ab, u, p)
        };
        assert_eq!(dd.hss_mul(&a0, false, &b0, false), base_share);
        for (i, j) in [(true, false), (false, true), (true, true)] {
            let a = if i { ring::add_coeffs(&a0, &delta.coeffs, p) } else { a0.clone() };
            let b = if j { ring::add_coeffs(&b0, &delta.coeffs, p) } else { b0.clone() };
            let got = dd.hss_mul(&a, i, &b, j);
            let expect = if i && j {
                ring::add_coeffs(&base_share, &delta.coeffs, p)
            } else {
                base_share.clone()
            };
            assert_eq!(got, expect, "i={i} j={j}");
        }
    }
}
