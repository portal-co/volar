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
        self.coeffs
            .iter()
            .map(|&c| if c == 0 { 0 } else { self.p - c })
            .collect()
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
        Self {
            p,
            delta,
            triple: (u, v, w),
        }
    }

    /// The plaintext modulus.
    pub const fn p(&self) -> u64 {
        self.p
    }

    /// The garbler's Δ (row generation / garbler-side decryption).
    pub const fn delta(&self) -> &Delta {
        &self.delta
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
        if i {
            ring::add_coeffs(&base, &tau.c0, self.p)
        } else {
            base
        }
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
        if i && j {
            ring::add_coeffs(&base, &tau.c0, self.p)
        } else {
            base
        }
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
    delta
        .coeffs
        .iter()
        .map(|&c| ring::mul_mod(c, k, p))
        .collect()
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

// ─────────────────────────────────────────────────────────────────────
// Circuit drivers (garble / evaluate whole circuits)
// ─────────────────────────────────────────────────────────────────────

use super::Variant;
use super::common::{Circuit, Gate};
use crate::SpecRng;

/// The garbler's per-circuit PRF key ([ZLL+26] Fig. 4 `PRF_K(γ)`). The
/// per-wire offset is derived deterministically so garbler and evaluator
/// agree; it is an additive `R_p` mask that cancels from the LSB.
///
/// The reference driver uses a counter-based stream from `key` (a KDF,
/// not a proven PRF); production swaps in a real PRF behind the same
/// seam (BITGC-LEDGER-03).
#[derive(Clone, Debug)]
pub struct PrfKey(pub [u64; 4]);

impl PrfKey {
    /// The per-wire additive offset `PRF_K(w)` as an `R_p` element.
    pub fn offset(&self, wire: usize, degree: usize, p: u64) -> LabelPoly {
        let mut out = alloc::vec![0_u64; degree];
        let mut s0 = self.0[0] ^ (wire as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);
        let mut s1 = self.0[1].wrapping_add(wire as u64);
        let mut s2 = self.0[2] ^ self.0[0];
        let mut s3 = self.0[3].wrapping_add(self.0[1]);
        for c in out.iter_mut() {
            // xoshiro-style mixing; content is arbitrary but deterministic.
            let t = s1 << 17;
            s2 ^= s0;
            s3 ^= s1;
            s1 ^= s2;
            s0 ^= s3;
            s2 ^= t;
            s3 = s3.rotate_left(45);
            *c = s3.wrapping_add(s0) % p;
        }
        out
    }
}

/// Row arithmetic: scale a row ciphertext by a small integer `k`
/// (component-wise). Preserves the decryption relation:
/// `(k·c0) + (k·c1)·Δ = k·(c0 + c1·Δ) = (k·t)·Δ`.
fn scale_row(row: &RowCt, k: i64, p: u64) -> RowCt {
    let scale = |poly: &LabelPoly, k: u64| -> LabelPoly {
        poly.iter().map(|&c| ring::mul_mod(c, k, p)).collect()
    };
    let neg = |poly: LabelPoly| -> LabelPoly {
        poly.iter()
            .map(|&c| if c == 0 { 0 } else { p - c })
            .collect()
    };
    if k >= 0 {
        RowCt {
            c0: scale(&row.c0, k as u64),
            c1: scale(&row.c1, k as u64),
        }
    } else {
        RowCt {
            c0: neg(scale(&row.c0, (-k) as u64)),
            c1: neg(scale(&row.c1, (-k) as u64)),
        }
    }
}

/// Row subtraction (component-wise); `Dec(Δ, τ−τ') = Dec(Δ,τ) − Dec(Δ,τ')`.
fn sub_rows(a: &RowCt, b: &RowCt, p: u64) -> RowCt {
    RowCt {
        c0: ring::sub_coeffs(&a.c0, &b.c0, p),
        c1: ring::sub_coeffs(&a.c1, &b.c1, p),
    }
}

/// The XOR gate's fixed row triple ([ZLL+26] Fig. 4 Case 2 / Fig. 6 Case
/// 2), derived from the global `[[Δ]]` row (`delta_ct`) and the mask bit
/// `d = π_α ⊕ π_β ⊕ π_γ`:
///
/// ```text
/// τ₁ = τ₂ = (1 − 2d)·[[Δ]];   τ₃ = −τ₁ − τ₂ = −2(1 − 2d)·[[Δ]]
/// ```
///
/// For fast free-XOR `d = 0`, giving `τ₁ = τ₂ = [[Δ]]`, `τ₃ = −2[[Δ]]`.
/// This is why XOR needs no per-gate table: the evaluator derives the
/// rows from the one global `[[Δ]]` (fast) plus the single communicated
/// bit (small).
pub fn xor_rows(delta_ct: &RowCt, d: bool, p: u64) -> [RowCt; 3] {
    let m: i64 = if d { -1 } else { 1 };
    [
        scale_row(delta_ct, m, p),
        scale_row(delta_ct, m, p),
        scale_row(delta_ct, -2 * m, p),
    ]
}

/// The fast-AND table reconstruction ([ZLL+26] Fig. 6a / 7a), shared by
/// garbler and evaluator: from the global `[[Δ]]`, the four encrypted PRG
/// bits `[[r^{i,j}·Δ]]`, and the four communicated mask bits
/// `d^{i,j} = ẑ[i,j] ⊕ r^{i,j}`, rebuild the row triple by **addition
/// only** (no online multiplication):
///
/// ```text
/// Z_k  := [[ẑ_k·Δ]] = d_k·[[Δ]] + (1 − 2·d_k)·[[r_k·Δ]]
/// τ₁   := Z_{1,0} − Z_{0,0};  τ₂ := Z_{0,1} − Z_{0,0}
/// τ₃   := Z_{1,1} − Z_{0,0} − τ₁ − τ₂
/// ```
///
/// Indexed `[i + 2j]` like [`masked_table`].
pub fn fast_and_rows(delta_ct: &RowCt, r: &[RowCt; 4], d: &[bool; 4], p: u64) -> [RowCt; 3] {
    let mut z: Vec<RowCt> = Vec::with_capacity(4);
    for k in 0..4 {
        // d=0 ⇒ Z = [[r·Δ]];  d=1 ⇒ Z = [[Δ]] − [[r·Δ]].
        z.push(if d[k] {
            sub_rows(delta_ct, &r[k], p)
        } else {
            r[k].clone()
        });
    }
    let t1 = sub_rows(&z[1], &z[0], p);
    let t2 = sub_rows(&z[2], &z[0], p);
    let t3_pre = sub_rows(&z[3], &z[0], p);
    let t3 = sub_rows(&sub_rows(&t3_pre, &t1, p), &t2, p);
    [t1, t2, t3]
}

/// Per-AND-gate offline material in a pinned garbling.
#[derive(Clone, Debug)]
pub enum AndRows {
    /// BitGC-small: the final garbled rows `(τ₁,τ₂,τ₃)` (the masked
    /// truth table depends on PRG-derived `π`'s only, so it is fully
    /// precomputable offline).
    Small([RowCt; 3]),
    /// BitGC-fast: the four encrypted PRG mask bits `[[r^{i,j}·Δ]]`.
    /// The final rows are derived from these plus the communicated mask
    /// bits via [`fast_and_rows`] (addition only — the paper's
    /// no-online-multiplication mechanism).
    Fast([RowCt; 4]),
}

/// The garbler's side of a pinned-BitGC garbling.
#[derive(Clone, Debug)]
pub struct PinnedGarbling {
    /// Per-wire garbler label state (`W^0`, realized `π`), indexed by
    /// wire. For fast AND gates the output `π_γ` is sampled during
    /// garbling and recorded here.
    pub labels: Vec<WireLabel>,
    /// The global `[[Δ]]` row (setup's `Enc(Δ, Δ)`); the basis for every
    /// XOR gate's derived rows.
    pub delta_ct: RowCt,
    /// Per-AND-gate offline material, in circuit order over AND gates.
    pub and_rows: Vec<AndRows>,
    /// Communicated online records, one per gate (fast XOR contributes a
    /// zero-bit marker so records stay gate-aligned), in circuit order.
    pub records: Vec<GateRecord>,
}

/// Errors from the pinned circuit drivers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DriverError {
    /// Circuit failed topological validation.
    BadCircuit,
    /// Wrong number of input masked bits / labels / permutation bits.
    BadInputCount,
    /// Ran out of communicated records or tables mid-evaluation.
    MissingRecord,
    /// A record or table had the wrong shape for the gate/variant.
    WrongRecord,
}

fn sample_poly(degree: usize, p: u64, random: &mut dyn SpecRng) -> LabelPoly {
    let mut out = alloc::vec![0_u64; degree];
    for c in out.iter_mut() {
        *c = (random.next_u32() as u64) % p;
    }
    out
}

fn fill_random(buf: &mut [u64], p: u64, random: &mut dyn SpecRng) {
    for v in buf.iter_mut() {
        *v = (random.next_u32() as u64) % p;
    }
}

/// The garbler's gate-output label computation ([ZLL+26] Fig. 4c):
///
/// ```text
/// W_γ^{π_γ} := Eval_{0,0}(W_α^{π_α}, W_β^{π_β}, τ₁,τ₂,τ₃) − ẑ[0,0]·Δ + PRF_K(γ)
/// ```
///
/// Returns the output wire's label state (storing `W^0`, so `w0_lsb()` is
/// the true `LSB(W^0)` used by the determination bit).
fn gate_output_label(
    dd: &ReferenceDd,
    wa_base: &LabelPoly,
    wb_base: &LabelPoly,
    taus: &[RowCt; 3],
    z00: bool,
    prf: &PrfKey,
    wire: usize,
    out_pi: bool,
) -> WireLabel {
    let delta = dd.delta();
    let p = dd.p();
    let degree = delta.coeffs.len();
    let mut out = dd.eval(wa_base, false, wb_base, false, taus);
    if z00 {
        out = ring::sub_coeffs(&out, &delta.coeffs, p);
    }
    out = ring::add_coeffs(&out, &prf.offset(wire, degree, p), p);
    let w0 = if out_pi {
        ring::sub_coeffs(&out, &delta.coeffs, p)
    } else {
        out
    };
    WireLabel { w0, pi: out_pi }
}

/// Garble a circuit with the pinned construction ([ZLL+26] Fig. 4/6).
///
/// `dd` is the shared DD gadget (the evaluation key: garbler and
/// evaluator must use the same `(U,V,W)` triple or the `Eval`
/// reconstruction identity breaks). `pis` holds one permutation bit per
/// wire (offline PRG stream); for the fast variant, AND-gate output
/// `π_γ`'s are sampled here instead and the realized values recorded in
/// the returned garbling. `prg_bits` supplies the fast variant's four
/// PRG mask bits per AND gate (flattened in circuit order).
pub fn garble_circuit(
    variant: Variant,
    circuit: &Circuit,
    dd: &ReferenceDd,
    pis: &[bool],
    prf: &PrfKey,
    prg_bits: &[bool],
    random: &mut dyn SpecRng,
) -> Result<PinnedGarbling, DriverError> {
    circuit.validate().map_err(|_| DriverError::BadCircuit)?;
    let delta = dd.delta();
    let p = dd.p();
    let degree = delta.coeffs.len();
    if pis.len() != circuit.wires() {
        return Err(DriverError::BadInputCount);
    }

    // Global [[Δ]] (setup: Enc(Δ,Δ) is a t=1 row under the reference
    // gadget).
    let delta_ct = dd.row(1, &mut |buf| fill_random(buf, p, random));

    let mut pi: Vec<bool> = pis.to_vec();
    let mut labels: Vec<WireLabel> = Vec::with_capacity(circuit.wires());
    let mut and_rows: Vec<AndRows> = Vec::new();
    let mut records = Vec::new();
    let mut fast_and = 0usize;

    for w in 0..circuit.wires() {
        if labels.len() <= w {
            labels.push(WireLabel {
                w0: sample_poly(degree, p, random),
                pi: pi[w],
            });
        }
        if w < circuit.inputs {
            continue;
        }
        let gate = circuit.gates[w - circuit.inputs];
        let (a, b) = gate.operands();
        let wa = labels[a].base(delta);
        let wb = labels[b].base(delta);
        match (variant, gate) {
            (Variant::Small, Gate::And { .. }) | (Variant::Tiny, Gate::And { .. }) => {
                let z = masked_table(true, pi[a], pi[b], pi[w]);
                let t = row_multipliers(&z);
                let taus = [
                    dd.row(t[0], &mut |buf| fill_random(buf, p, random)),
                    dd.row(t[1], &mut |buf| fill_random(buf, p, random)),
                    dd.row(t[2], &mut |buf| fill_random(buf, p, random)),
                ];
                let out = gate_output_label(dd, &wa, &wb, &taus, z[0], prf, w, pi[w]);
                let det = determination_bit(out.w0_lsb(), pi[w]);
                labels[w] = out;
                and_rows.push(AndRows::Small(taus));
                records.push(GateRecord::SmallAnd(det));
            }
            (Variant::Small, Gate::Xor { .. }) | (Variant::Tiny, Gate::Xor { .. }) => {
                let d = pi[a] ^ pi[b] ^ pi[w];
                let taus = xor_rows(&delta_ct, d, p);
                let out = gate_output_label(dd, &wa, &wb, &taus, d, prf, w, pi[w]);
                labels[w] = out;
                records.push(GateRecord::SmallXor(d));
            }
            (Variant::Fast, Gate::And { .. }) => {
                // AND output π_γ sampled uniformly; 4 PRG mask bits.
                let out_pi = (random.next_u32() & 1) == 1;
                pi[w] = out_pi;
                let z = masked_table(true, pi[a], pi[b], out_pi);
                let r: [bool; 4] = [
                    prg_bits.get(4 * fast_and).copied().unwrap_or(false),
                    prg_bits.get(4 * fast_and + 1).copied().unwrap_or(false),
                    prg_bits.get(4 * fast_and + 2).copied().unwrap_or(false),
                    prg_bits.get(4 * fast_and + 3).copied().unwrap_or(false),
                ];
                fast_and += 1;
                let masks = fast_mask_bits(&z, &r);
                let r_rows = [
                    dd.row(i64::from(r[0]), &mut |buf| fill_random(buf, p, random)),
                    dd.row(i64::from(r[1]), &mut |buf| fill_random(buf, p, random)),
                    dd.row(i64::from(r[2]), &mut |buf| fill_random(buf, p, random)),
                    dd.row(i64::from(r[3]), &mut |buf| fill_random(buf, p, random)),
                ];
                let taus = fast_and_rows(&delta_ct, &r_rows, &masks, p);
                let out = gate_output_label(dd, &wa, &wb, &taus, z[0], prf, w, out_pi);
                let det = determination_bit(out.w0_lsb(), out_pi);
                labels[w] = out;
                and_rows.push(AndRows::Fast(r_rows));
                records.push(GateRecord::FastAnd { masks, det });
            }
            (Variant::Fast, Gate::Xor { .. }) => {
                // Free XOR: π_γ = π_α ⊕ π_β, rows derived from [[Δ]]
                // (d = 0), no communicated bits.
                let out_pi = pi[a] ^ pi[b];
                pi[w] = out_pi;
                let taus = xor_rows(&delta_ct, false, p);
                let out = gate_output_label(dd, &wa, &wb, &taus, false, prf, w, out_pi);
                labels[w] = out;
                records.push(GateRecord::FastXor);
            }
        }
    }

    Ok(PinnedGarbling {
        labels,
        delta_ct,
        and_rows,
        records,
    })
}

/// The evaluator's input encoding ([ZLL+26] `En`): for input wire `i`
/// with clear bit `x_i`, the masked bit is `x̂_i = x_i ⊕ π_i` and the
/// active label is `W_i = W_i^{π_i} + x̂_i·Δ`.
pub fn encode_inputs(
    garbling: &PinnedGarbling,
    delta: &Delta,
    inputs: &[bool],
    input_count: usize,
) -> Result<Vec<(LabelPoly, bool)>, DriverError> {
    if inputs.len() != input_count {
        return Err(DriverError::BadInputCount);
    }
    let mut out = Vec::with_capacity(input_count);
    for (i, &x) in inputs.iter().enumerate() {
        let masked = x ^ garbling.labels[i].pi;
        out.push((garbling.labels[i].active(masked, delta), masked));
    }
    Ok(out)
}

/// The shared gate-evaluation step ([ZLL+26] Fig. 5c / 7c):
/// `W_γ = Eval_{x̂_α,x̂_β}(W_α,W_β,τ₁,τ₂,τ₃) + PRF_K(γ)`.
#[allow(clippy::too_many_arguments)]
fn eval_gate(
    dd: &ReferenceDd,
    a: &LabelPoly,
    i: bool,
    b: &LabelPoly,
    j: bool,
    taus: &[RowCt; 3],
    prf: &PrfKey,
    wire: usize,
    degree: usize,
    p: u64,
) -> LabelPoly {
    let out = dd.eval(a, i, b, j, taus);
    ring::add_coeffs(&out, &prf.offset(wire, degree, p), p)
}

/// Evaluate a garbled circuit with the pinned construction
/// ([ZLL+26] Fig. 5/7).
///
/// `dd` must be the same gadget (same `(U,V,W)` triple) the garbler
/// used. `delta_ct` and `and_rows` are the offline material from
/// [`PinnedGarbling`]; `records` are the online communicated bits;
/// `prf` is the shared PRF key. For each gate the evaluator runs
/// `W_γ = Eval_{x̂_α,x̂_β}(W_α,W_β,τ) + PRF_K(γ)` — with rows taken from
/// the offline table (small AND), reconstructed by addition from the
/// mask bits (fast AND), or derived from `[[Δ]]` (both XOR forms) — and
/// recovers the masked bit via the determination bit (AND) or pure XOR
/// (XOR). Returns the **masked** output bits; open with [`open_output`].
///
/// **Honesty boundary:** with the reference DD gadget this is an exact,
/// no-noise evaluation of the garbling algebra. It is **not** the
/// production distributed-decryption path (lattice DD + hash-match
/// opening). See the module docs and BITGC-LEDGER-04.
pub fn evaluate_circuit(
    variant: Variant,
    circuit: &Circuit,
    dd: &ReferenceDd,
    delta_ct: &RowCt,
    and_rows: &[AndRows],
    records: &[GateRecord],
    prf: &PrfKey,
    inputs: &[(LabelPoly, bool)],
) -> Result<Vec<bool>, DriverError> {
    circuit.validate().map_err(|_| DriverError::BadCircuit)?;
    if inputs.len() != circuit.inputs {
        return Err(DriverError::BadInputCount);
    }
    let delta = dd.delta();
    let p = dd.p();
    let degree = delta.coeffs.len();
    let mut labels: Vec<LabelPoly> = Vec::with_capacity(circuit.wires());
    let mut masked: Vec<bool> = Vec::with_capacity(circuit.wires());
    for (w, m) in inputs {
        labels.push(w.clone());
        masked.push(*m);
    }
    let mut rec = records.iter();
    let mut and_iter = and_rows.iter();

    for (gi, gate) in circuit.gates.iter().enumerate() {
        let w = circuit.inputs + gi;
        let (a, b) = gate.operands();
        match (variant, gate) {
            (Variant::Small, Gate::And { .. }) | (Variant::Tiny, Gate::And { .. }) => {
                let d = match rec.next() {
                    Some(GateRecord::SmallAnd(d)) => *d,
                    Some(_) => return Err(DriverError::WrongRecord),
                    None => return Err(DriverError::MissingRecord),
                };
                let taus = match and_iter.next() {
                    Some(AndRows::Small(rows)) => rows,
                    Some(_) => return Err(DriverError::WrongRecord),
                    None => return Err(DriverError::MissingRecord),
                };
                let out = eval_gate(
                    dd, &labels[a], masked[a], &labels[b], masked[b], taus, prf, w, degree, p,
                );
                let m = label_lsb(&out) ^ d;
                labels.push(out);
                masked.push(m);
            }
            (Variant::Small, Gate::Xor { .. }) | (Variant::Tiny, Gate::Xor { .. }) => {
                let d = match rec.next() {
                    Some(GateRecord::SmallXor(d)) => *d,
                    Some(_) => return Err(DriverError::WrongRecord),
                    None => return Err(DriverError::MissingRecord),
                };
                let taus = xor_rows(delta_ct, d, p);
                let out = eval_gate(
                    dd, &labels[a], masked[a], &labels[b], masked[b], &taus, prf, w, degree, p,
                );
                labels.push(out);
                masked.push(masked[a] ^ masked[b] ^ d);
            }
            (Variant::Fast, Gate::And { .. }) => {
                let (masks, det) = match rec.next() {
                    Some(GateRecord::FastAnd { masks, det }) => (*masks, *det),
                    Some(_) => return Err(DriverError::WrongRecord),
                    None => return Err(DriverError::MissingRecord),
                };
                let r_rows = match and_iter.next() {
                    Some(AndRows::Fast(rows)) => rows,
                    Some(_) => return Err(DriverError::WrongRecord),
                    None => return Err(DriverError::MissingRecord),
                };
                let taus = fast_and_rows(delta_ct, r_rows, &masks, p);
                let out = eval_gate(
                    dd, &labels[a], masked[a], &labels[b], masked[b], &taus, prf, w, degree, p,
                );
                let m = label_lsb(&out) ^ det;
                labels.push(out);
                masked.push(m);
            }
            (Variant::Fast, Gate::Xor { .. }) => {
                // Free XOR: consume the zero-bit marker; rows derive
                // from [[Δ]]; masked bits add mod 2.
                match rec.next() {
                    Some(GateRecord::FastXor) => {}
                    Some(_) => return Err(DriverError::WrongRecord),
                    None => return Err(DriverError::MissingRecord),
                }
                let taus = xor_rows(delta_ct, false, p);
                let out = eval_gate(
                    dd, &labels[a], masked[a], &labels[b], masked[b], &taus, prf, w, degree, p,
                );
                labels.push(out);
                masked.push(masked[a] ^ masked[b]);
            }
        }
    }

    Ok(circuit.outputs.iter().map(|&o| masked[o]).collect())
}

/// Open a masked output bit with the wire's permutation bit
/// ([ZLL+26] `De`): `x = x̂ ⊕ π`. The garbler reveals `π` on output
/// wires (semi-honest test harness; production uses hash-match).
pub fn open_output(masked: bool, pi: bool) -> bool {
    masked ^ pi
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitgc::common::{Circuit, Gate};

    /// A deterministic `SpecRng` for tests.
    struct TestRng(u64);
    impl crate::SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self
                .0
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            (self.0 >> 33) as u32
        }
    }

    /// The gated test circuit `(x0 ∧ x1) ⊕ x2`: 3 inputs, And(0,1)→w3,
    /// Xor(3,2)→w4, output w4.
    fn test_circuit() -> Circuit {
        Circuit {
            inputs: 3,
            gates: alloc::vec![Gate::And { a: 0, b: 1 }, Gate::Xor { a: 3, b: 2 }],
            outputs: alloc::vec![4],
        }
    }

    fn test_prf() -> PrfKey {
        PrfKey([
            0x0123_4567_89ab_cdef,
            0x0fed_cba9_8765_4321,
            0xaaaa_bbbb_cccc_dddd,
            0x1111_2222_3333_4444,
        ])
    }

    /// Full garble+evaluate round-trip for one variant over all 8 inputs.
    fn roundtrip(variant: Variant, inputs: [bool; 3]) -> bool {
        // A large odd prime so the per-gate LSB wrap-around probability
        // (~N/p for a degree-8 ring) is negligible; the paper uses
        // log p ≈ 60 for a < 2^-40 failure target (§6).
        let p = 4_611_686_018_427_387_919u64; // 2^62 + 135, prime
        let degree = 8;
        let delta = test_delta(degree, p);
        let circuit = test_circuit();
        let wires = circuit.wires();
        let mut rng = TestRng(0xdead_beef_cafe_f00d);
        let prf = test_prf();

        // The shared DD gadget (evaluation key): garbler and evaluator
        // use the same (U,V,W) triple.
        let dd = ReferenceDd::new(delta.clone(), degree, &mut |buf| {
            for v in buf.iter_mut() {
                *v = (rng.next_u32() as u64) % p;
            }
        });

        // Offline permutation bits (one per wire) + fast PRG mask bits.
        let pis: Vec<bool> = (0..wires).map(|_| (rng.next_u32() & 1) == 1).collect();
        let and_count = circuit
            .gates
            .iter()
            .filter(|g| matches!(g, Gate::And { .. }))
            .count();
        let prg_bits: Vec<bool> = (0..4 * and_count)
            .map(|_| (rng.next_u32() & 1) == 1)
            .collect();

        let garbling = garble_circuit(variant, &circuit, &dd, &pis, &prf, &prg_bits, &mut rng)
            .expect("garble");
        let encoded =
            encode_inputs(&garbling, dd.delta(), &inputs, circuit.inputs).expect("encode");
        let masked_out = evaluate_circuit(
            variant,
            &circuit,
            &dd,
            &garbling.delta_ct,
            &garbling.and_rows,
            &garbling.records,
            &prf,
            &encoded,
        )
        .expect("evaluate");

        // Open against the garbler's realized output π.
        open_output(masked_out[0], garbling.labels[circuit.outputs[0]].pi)
    }

    #[test]
    fn small_circuit_truth_table() {
        let expect = |i: [bool; 3]| (i[0] & i[1]) ^ i[2];
        for x0 in [false, true] {
            for x1 in [false, true] {
                for x2 in [false, true] {
                    let inputs = [x0, x1, x2];
                    assert_eq!(
                        roundtrip(Variant::Small, inputs),
                        expect(inputs),
                        "{inputs:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn fast_circuit_truth_table() {
        let expect = |i: [bool; 3]| (i[0] & i[1]) ^ i[2];
        for x0 in [false, true] {
            for x1 in [false, true] {
                for x2 in [false, true] {
                    let inputs = [x0, x1, x2];
                    assert_eq!(
                        roundtrip(Variant::Fast, inputs),
                        expect(inputs),
                        "{inputs:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn record_bit_counts() {
        // Communication accounting matches [ZLL+26] Table 1.
        let p = 4_611_686_018_427_387_919u64;
        let degree = 8;
        let delta = test_delta(degree, p);
        let circuit = test_circuit();
        let wires = circuit.wires();
        let mut rng = TestRng(0x1234_5678);
        let prf = test_prf();
        let dd = ReferenceDd::new(delta.clone(), degree, &mut |buf| {
            for v in buf.iter_mut() {
                *v = (rng.next_u32() as u64) % p;
            }
        });
        let pis: Vec<bool> = (0..wires).map(|_| (rng.next_u32() & 1) == 1).collect();
        let and_count = circuit
            .gates
            .iter()
            .filter(|g| matches!(g, Gate::And { .. }))
            .count();
        let prg_bits: Vec<bool> = (0..4 * and_count)
            .map(|_| (rng.next_u32() & 1) == 1)
            .collect();

        for (variant, want) in [(Variant::Small, 2usize), (Variant::Fast, 5usize)] {
            let g = garble_circuit(variant, &circuit, &dd, &pis, &prf, &prg_bits, &mut rng)
                .expect("garble");
            let total: usize = g.records.iter().map(GateRecord::bits).sum();
            assert_eq!(total, want, "variant {variant:?}");
        }
    }

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
                state = state
                    .wrapping_mul(6364136223846793005)
                    .wrapping_add(1442695040888963407);
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
        assert_eq!(
            masked_table(true, true, false, true),
            [true, true, false, true]
        );
        // π_a=1, π_b=1, π_c=0: (1⊕i)(1⊕j) = (¬i)(¬j) → only (0,0) is 1
        assert_eq!(
            masked_table(true, true, true, false),
            [true, false, false, false]
        );
    }

    #[test]
    fn masked_table_xor() {
        // Free-XOR rule π_c = π_a ⊕ π_b ⇒ ẑ[i,j] = i ⊕ j.
        for (pa, pb) in [(false, false), (false, true), (true, false), (true, true)] {
            assert_eq!(
                masked_table(false, pa, pb, pa ^ pb),
                [false, true, true, false]
            );
        }
    }

    #[test]
    fn row_multipliers_reconstruct() {
        // AND, all-zero π: only ẑ[1,1] = 1 → z = [0,0,0,1], t = [0,0,1].
        assert_eq!(
            row_multipliers(&masked_table(true, false, false, false)),
            [0, 0, 1]
        );
        // AND, π_a=1: ẑ[i,j] = (¬i)∧j → z = [0, 0, 1, 0] → t = [0, 1, −1].
        assert_eq!(
            row_multipliers(&masked_table(true, true, false, false)),
            [0, 1, -1]
        );
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
            let a = if i {
                ring::add_coeffs(&a0, &delta.coeffs, p)
            } else {
                a0.clone()
            };
            let lhs = dd.dd_i(&a, i, &tau);
            let rhs = ring::add_coeffs(
                &dd.dd0(&a0, &tau),
                &if i { m.clone() } else { alloc::vec![0; degree] },
                p,
            );
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
            let a = if i {
                ring::add_coeffs(&a0, &delta.coeffs, p)
            } else {
                a0.clone()
            };
            let b = if j {
                ring::add_coeffs(&b0, &delta.coeffs, p)
            } else {
                b0.clone()
            };
            let lhs = dd.dd_ij(&a, i, &b, j, &tau);
            let corr = if i && j {
                m.clone()
            } else {
                alloc::vec![0; degree]
            };
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
            let a = if i {
                ring::add_coeffs(&a0, &delta.coeffs, p)
            } else {
                a0.clone()
            };
            let b = if j {
                ring::add_coeffs(&b0, &delta.coeffs, p)
            } else {
                b0.clone()
            };
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
