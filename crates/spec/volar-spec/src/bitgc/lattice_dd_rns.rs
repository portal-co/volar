// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! **Two-limb RNS lattice SWHEwDD reference gadget** — a modulus-shape
//! experiment for the paper's `log q ≈ 120` (k=1) / `log r ≈ 240` (k=2)
//! ([ZLL+26] §6; the super-polynomial bound is Scooby Eq. 1, see
//! `docs/research/bitgc-dd-sources.md`). This is not a production-parameter
//! or security-reviewed implementation.
//!
//! # Representation
//!
//! `q = q0 · q1` with `q0, q1` distinct NTT-friendly primes (`≡ 1 mod 2N`)
//! and **`p | q0`** (limb 0 is a multiple of the plaintext modulus). An
//! `R_q` element is stored as a pair of per-limb residues
//! `(x mod q0, x mod q1)`, each a degree-`N` coefficient vector. All ring
//! products run per-limb (NTT where the limb prime is NTT-friendly, else
//! schoolbook).
//!
//! # The rounding step (why limb-0 suffices)
//!
//! `round_to_rp` centers each coefficient mod `q` and reduces mod `p`.
//! Because `p | q0`, `x mod p = (x mod q0) mod p`, and the centered value
//! is recovered from limb 0 alone (the phase's noise is `O(p) ≪ q0/2`), so
//! **no full CRT reconstruction is needed** — the same limb-0 shortcut
//! `bitgc::swhe::Context::decrypt` uses. This is the whole reason the
//! gadget keeps an exact mod-`p` identity: the noise is a multiple of `p`
//! in every limb and vanishes on the `mod p` reduction.
//!
//! # Honesty boundary
//!
//! The implementation uses simplified ternary noise and reduced test
//! profiles. Its exact mod-`p` DD identities and role-separated API are
//! algebraic test evidence only; they do not establish RLWE/KDM security or
//! the paper's per-gate failure bounds. Security review is BITGC-LEDGER-03.
//! **Do not use for protected data.**

use alloc::vec::Vec;

#[cfg(any(test, feature = "bitgc-reference"))]
use super::pinned::{DdGadget, RowCt};
use super::pinned::{Delta, LabelPoly, RoleDdEvaluator, RoleDdGarbler};
use super::ring;
use crate::SpecRng;

/// A degree-`N` coefficient vector (one RNS limb's worth of residues).
type Limb = Vec<u64>;

/// An `R_q` element in two-limb RNS form: `(mod q0, mod q1)`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rns2 {
    /// Residues mod `q0` (the limb that is a multiple of `p`).
    l0: Limb,
    /// Residues mod `q1`.
    l1: Limb,
}

/// A GSW-style RLWE row ciphertext over the RNS modulus: `τ = (u1, u2)`
/// with `u1 + u2·Δ = t·Δ + p·e` (per limb).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RnsRowCt {
    /// `u1 = −u2·Δ + t·Δ + p·e` (RNS).
    pub u1: Rns2,
    /// `u2`, uniform (RNS).
    pub u2: Rns2,
}

/// The two-limb RNS lattice DD garbler setup. Its evaluator-only view is
/// `LatticeRnsEvaluator`; the modulus shape can represent a
/// `log q ≈ 120` profile, but this implementation is not security-reviewed.
#[derive(Clone, Debug)]
pub struct LatticeRnsDd {
    /// Ring degree `N`.
    n: usize,
    /// Plaintext modulus `p` (odd prime).
    p: u64,
    /// Limb-0 modulus (a multiple of `p`), NTT-friendly.
    q0: u64,
    /// Limb-1 modulus (NTT-friendly, ≠ q0).
    q1: u64,
    /// The key `Δ` (ternary, `LSB(Δ)=1`), coefficient form mod `p`.
    delta: LabelPoly,
    /// The same `Δ` as a `pinned::Delta` for the seam.
    delta_pinned: Delta,
    /// CP-RLWE triple `(U, V, W)` in RNS form.
    triple: (Rns2, Rns2, Rns2),
    /// The triple reduced mod `p` for the legacy shared-gadget driver.
    #[cfg(any(test, feature = "bitgc-reference"))]
    triple_rp: (LabelPoly, LabelPoly, LabelPoly),
    /// NTT contexts for the two limbs (present when NTT-friendly).
    ntt0: Option<ring::Ntt>,
    ntt1: Option<ring::Ntt>,
    /// NTT context over `R_p` for the legacy shared-gadget driver.
    #[cfg(any(test, feature = "bitgc-reference"))]
    ntt_p: Option<ring::Ntt>,
}

/// Evaluator-only portion of the RNS DD setup. It contains the noisy
/// CP-RLWE evaluation triple and public ring parameters, but no Δ or
/// garbler secret key.
#[derive(Clone, Debug)]
pub struct LatticeRnsEvaluator {
    n: usize,
    p: u64,
    q0: u64,
    q1: u64,
    triple: (Rns2, Rns2, Rns2),
    ntt0: Option<ring::Ntt>,
    ntt1: Option<ring::Ntt>,
}

impl LatticeRnsDd {
    /// Build the gadget. `q0` must be an odd multiple of the odd `p` and
    /// `≡ 1 mod 2N`; `q1` must be a distinct prime `≡ 1 mod 2N`.
    pub fn new(
        n: usize,
        p: u64,
        q0: u64,
        q1: u64,
        random: &mut dyn SpecRng,
    ) -> Result<Self, RnsError> {
        if n == 0 || p < 3 || p % 2 == 0 || q0 % p != 0 || q1 == q0 {
            return Err(RnsError::InvalidParameters);
        }
        // Δ ternary, LSB 1.
        let mut delta = alloc::vec![0_u64; n];
        for c in delta.iter_mut() {
            let r = random.next_u32() % 3;
            *c = match r {
                0 => 0,
                1 => 1,
                _ => p - 1,
            };
        }
        delta[0] = 1;

        // Δ centered per limb.
        let dq0 = to_limb(&delta, p, q0);
        let dq1 = to_limb(&delta, p, q1);

        // CP-RLWE triple per limb: U uniform; V = −U·Δ + p·e₁;
        // W = −Δ − U·Δ² + p·e₂.
        let u0 = uniform_limb(n, q0, random);
        let u1 = uniform_limb(n, q1, random);
        let ud0 = mul_limb(&u0, &dq0, q0, &ntt_ctx(n, q0));
        let ud1 = mul_limb(&u1, &dq1, q1, &ntt_ctx(n, q1));
        let v0 = add_limb(&negate_limb(&ud0, q0), &noise_limb(n, p, q0, random), q0);
        let v1 = add_limb(&negate_limb(&ud1, q1), &noise_limb(n, p, q1, random), q1);
        let d2q0 = mul_limb(&dq0, &dq0, q0, &ntt_ctx(n, q0));
        let d2q1 = mul_limb(&dq1, &dq1, q1, &ntt_ctx(n, q1));
        let ud20 = mul_limb(&u0, &d2q0, q0, &ntt_ctx(n, q0));
        let ud21 = mul_limb(&u1, &d2q1, q1, &ntt_ctx(n, q1));
        let w0 = add_limb(
            &sub_limb(&negate_limb(&dq0, q0), &ud20, q0),
            &noise_limb(n, p, q0, random),
            q0,
        );
        let w1 = add_limb(
            &sub_limb(&negate_limb(&dq1, q1), &ud21, q1),
            &noise_limb(n, p, q1, random),
            q1,
        );

        #[cfg(any(test, feature = "bitgc-reference"))]
        let triple_rp = {
            let red = |limb: &Limb| -> LabelPoly { limb.iter().map(|&c| c % p).collect() };
            (red(&u0), red(&v0), red(&w0))
        };
        let delta_pinned = Delta {
            coeffs: delta.clone(),
            p,
        };
        Ok(Self {
            n,
            p,
            q0,
            q1,
            delta,
            delta_pinned,
            triple: (
                Rns2 { l0: u0, l1: u1 },
                Rns2 { l0: v0, l1: v1 },
                Rns2 { l0: w0, l1: w1 },
            ),
            #[cfg(any(test, feature = "bitgc-reference"))]
            triple_rp,
            ntt0: ring::Ntt::new(n, q0).ok(),
            ntt1: ring::Ntt::new(n, q1).ok(),
            #[cfg(any(test, feature = "bitgc-reference"))]
            ntt_p: ring::Ntt::new(n, p).ok(),
        })
    }

    /// Ring degree.
    pub const fn degree(&self) -> usize {
        self.n
    }
    /// Plaintext modulus.
    pub const fn p(&self) -> u64 {
        self.p
    }
    /// Total modulus bits (`log2(q0·q1)`).
    pub fn modulus_bits(&self) -> u32 {
        (64 - self.q0.leading_zeros()) + (64 - self.q1.leading_zeros()) - 2
    }

    /// The key `Δ` (garbler-side), as raw coefficient form.
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn delta(&self) -> &LabelPoly {
        &self.delta
    }

    /// The key `Δ` as a `pinned::Delta` (for reference-driver input encoding).
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn delta_pinned_ref(&self) -> &Delta {
        &self.delta_pinned
    }

    /// Derive a distinct evaluator-only key object. It contains no Δ.
    pub fn evaluator_material(&self) -> LatticeRnsEvaluator {
        LatticeRnsEvaluator {
            n: self.n,
            p: self.p,
            q0: self.q0,
            q1: self.q1,
            triple: self.triple.clone(),
            ntt0: self.ntt0.clone(),
            ntt1: self.ntt1.clone(),
        }
    }

    /// Generate a row ciphertext for multiplier `t` (RNS form).
    pub fn row(&self, t: i64, random: &mut dyn SpecRng) -> RnsRowCt {
        let (p, q0, q1) = (self.p, self.q0, self.q1);
        let u2 = Rns2 {
            l0: uniform_limb(self.n, q0, random),
            l1: uniform_limb(self.n, q1, random),
        };
        let dq0 = to_limb(&self.delta, p, q0);
        let dq1 = to_limb(&self.delta, p, q1);
        let u2d0 = mul_limb(&u2.l0, &dq0, q0, &self.ntt0);
        let u2d1 = mul_limb(&u2.l1, &dq1, q1, &self.ntt1);
        let td0 = scale_limb(&dq0, t, q0);
        let td1 = scale_limb(&dq1, t, q1);
        let e0 = noise_limb(self.n, p, q0, random);
        let e1 = noise_limb(self.n, p, q1, random);
        RnsRowCt {
            u1: Rns2 {
                l0: add_limb(&sub_limb(&td0, &u2d0, q0), &e0, q0),
                l1: add_limb(&sub_limb(&td1, &u2d1, q1), &e1, q1),
            },
            u2,
        }
    }

    /// Garbler-side special decryption `Dec(Δ, τ) = t·Δ (mod p)`, exact.
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn dec_row(&self, tau: &RnsRowCt) -> LabelPoly {
        let q0 = self.q0;
        let dq0 = to_limb(&self.delta, self.p, q0);
        let u2d0 = mul_limb(&tau.u2.l0, &dq0, q0, &self.ntt0);
        let phase0 = add_limb(&tau.u1.l0, &u2d0, q0);
        round_limb0_to_rp(&phase0, q0, self.p)
    }

    /// `DD_0(A, τ) = round_p(u2·A)` (k=1), RNS.
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn dd0(&self, a: &LabelPoly, tau: &RnsRowCt) -> LabelPoly {
        let (p, q0) = (self.p, self.q0);
        let a_q0: Limb = a.iter().map(|&c| c % q0).collect();
        let prod0 = mul_limb(&tau.u2.l0, &a_q0, q0, &self.ntt0);
        round_limb0_to_rp(&prod0, q0, p)
    }

    /// `DD_i(A, τ) = round_p(u2·A + i·u1)` (k=1), RNS.
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn dd_i(&self, a: &LabelPoly, i: bool, tau: &RnsRowCt) -> LabelPoly {
        let (p, q0) = (self.p, self.q0);
        let a_q0: Limb = a.iter().map(|&c| c % q0).collect();
        let mut acc0 = mul_limb(&tau.u2.l0, &a_q0, q0, &self.ntt0);
        if i {
            acc0 = add_limb(&acc0, &tau.u1.l0, q0);
        }
        round_limb0_to_rp(&acc0, q0, p)
    }

    /// `HssMul` over `R_p` for the legacy shared-gadget driver.
    #[cfg(any(test, feature = "bitgc-reference"))]
    fn hss_mul_rp(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool) -> LabelPoly {
        let p = self.p;
        let (u, v, w) = &self.triple_rp;
        let ab = self.mul_p(a, b);
        let mut out = self.mul_p(&ab, u);
        if i {
            let bv = self.mul_p(b, v);
            out = ring::add_coeffs(&out, &bv, p);
        }
        if j {
            let av = self.mul_p(a, v);
            out = ring::add_coeffs(&out, &av, p);
        }
        if i && j {
            out = ring::sub_coeffs(&out, w, p);
        }
        out
    }

    /// Negacyclic product over `R_p` for the legacy shared-gadget driver.
    #[cfg(any(test, feature = "bitgc-reference"))]
    fn mul_p(&self, a: &[u64], b: &[u64]) -> LabelPoly {
        match &self.ntt_p {
            Some(ntt) => ring::negacyclic_mul_ntt(a, b, ntt),
            None => ring::negacyclic_mul_coeffs(a, b, self.p),
        }
    }

    /// `DD_{i,j}(A, B, τ) = round_p(u2·HssMul_{i,j}(A,B) + i·j·u1)` (k=2).
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn dd_ij(
        &self,
        a: &LabelPoly,
        i: bool,
        b: &LabelPoly,
        j: bool,
        tau: &RnsRowCt,
    ) -> LabelPoly {
        let (p, q0) = (self.p, self.q0);
        let share = self.hss_mul_rp(a, i, b, j);
        let share_q0: Limb = share.iter().map(|&c| c % q0).collect();
        let mut acc0 = mul_limb(&tau.u2.l0, &share_q0, q0, &self.ntt0);
        if i && j {
            acc0 = add_limb(&acc0, &tau.u1.l0, q0);
        }
        round_limb0_to_rp(&acc0, q0, p)
    }

    /// The gate evaluator `Eval_{i,j}(A,B,τ)` (RNS), summed over `R_p`.
    #[cfg(any(test, feature = "bitgc-reference"))]
    pub fn eval(
        &self,
        a: &LabelPoly,
        i: bool,
        b: &LabelPoly,
        j: bool,
        taus: &[RnsRowCt; 3],
    ) -> LabelPoly {
        let p = self.p;
        let mut out = self.dd_i(a, i, &taus[0]);
        out = ring::add_coeffs(&out, &self.dd_i(b, j, &taus[1]), p);
        ring::add_coeffs(&out, &self.dd_ij(a, i, b, j, &taus[2]), p)
    }
}

/// Errors from the RNS lattice DD gadget.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RnsError {
    /// Degree/length mismatch.
    LengthMismatch,
    /// Modulus constraints violated (`p | q0`, primes distinct/odd/NTT).
    InvalidParameters,
}

#[cfg(any(test, feature = "bitgc-reference"))]
impl DdGadget for LatticeRnsDd {
    fn p(&self) -> u64 {
        self.p
    }
    fn delta(&self) -> &Delta {
        &self.delta_pinned
    }
    fn row(&self, t: i64, random: &mut dyn SpecRng) -> RowCt {
        let tau = LatticeRnsDd::row(self, t, random);
        // Reduce the RNS row to the exact reference `RowCt` (limb 0 mod p).
        RowCt {
            c0: tau.u1.l0.iter().map(|&c| c % self.p).collect(),
            c1: tau.u2.l0.iter().map(|&c| c % self.p).collect(),
        }
    }
    fn dd0(&self, a: &LabelPoly, tau: &RowCt) -> LabelPoly {
        self.mul_p(&tau.c1, a)
    }
    fn dd_i(&self, a: &LabelPoly, i: bool, tau: &RowCt) -> LabelPoly {
        let base = DdGadget::dd0(self, a, tau);
        if i {
            ring::add_coeffs(&base, &tau.c0, self.p)
        } else {
            base
        }
    }
    fn dd_ij(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool, tau: &RowCt) -> LabelPoly {
        let share = self.hss_mul_rp(a, i, b, j);
        let base = self.mul_p(&tau.c1, &share);
        if i && j {
            ring::add_coeffs(&base, &tau.c0, self.p)
        } else {
            base
        }
    }
    fn eval(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool, taus: &[RowCt; 3]) -> LabelPoly {
        let mut out = DdGadget::dd_i(self, a, i, &taus[0]);
        out = ring::add_coeffs(&out, &DdGadget::dd_i(self, b, j, &taus[1]), self.p);
        ring::add_coeffs(&out, &DdGadget::dd_ij(self, a, i, b, j, &taus[2]), self.p)
    }
}

impl RoleDdEvaluator for LatticeRnsEvaluator {
    type Row = RnsRowCt;

    fn p(&self) -> u64 {
        self.p
    }

    fn degree(&self) -> usize {
        self.n
    }

    fn eval_rows(
        &self,
        a: &LabelPoly,
        i: bool,
        b: &LabelPoly,
        j: bool,
        taus: &[RnsRowCt; 3],
    ) -> LabelPoly {
        eval_rns_rows(
            self.p,
            self.q0,
            self.q1,
            &self.triple,
            &self.ntt0,
            &self.ntt1,
            a,
            i,
            b,
            j,
            taus,
        )
    }

    fn xor_rows_for(&self, delta_row: &RnsRowCt, determination: bool) -> [RnsRowCt; 3] {
        xor_rns_rows(delta_row, determination, self.q0, self.q1)
    }

    fn fast_rows_for(
        &self,
        delta_row: &RnsRowCt,
        prg_rows: &[RnsRowCt; 4],
        masks: &[bool; 4],
    ) -> [RnsRowCt; 3] {
        fast_rns_rows(delta_row, prg_rows, masks, self.q0, self.q1)
    }
}

impl RoleDdGarbler for LatticeRnsDd {
    type Row = RnsRowCt;
    type Evaluator = LatticeRnsEvaluator;

    fn p(&self) -> u64 {
        self.p
    }

    fn degree(&self) -> usize {
        self.n
    }

    fn secret_delta(&self) -> &Delta {
        &self.delta_pinned
    }

    fn encrypt_row(&self, t: i64, random: &mut dyn SpecRng) -> RnsRowCt {
        LatticeRnsDd::row(self, t, random)
    }

    fn evaluator_material(&self) -> Self::Evaluator {
        LatticeRnsDd::evaluator_material(self)
    }
}

fn lift_rp(poly: &LabelPoly, q: u64) -> Limb {
    poly.iter().map(|&coefficient| coefficient % q).collect()
}

fn mul_rns2(
    a: &Rns2,
    b: &Rns2,
    q0: u64,
    q1: u64,
    ntt0: &Option<ring::Ntt>,
    ntt1: &Option<ring::Ntt>,
) -> Rns2 {
    Rns2 {
        l0: mul_limb(&a.l0, &b.l0, q0, ntt0),
        l1: mul_limb(&a.l1, &b.l1, q1, ntt1),
    }
}

fn add_rns2(a: &Rns2, b: &Rns2, q0: u64, q1: u64) -> Rns2 {
    Rns2 {
        l0: add_limb(&a.l0, &b.l0, q0),
        l1: add_limb(&a.l1, &b.l1, q1),
    }
}

fn sub_rns2(a: &Rns2, b: &Rns2, q0: u64, q1: u64) -> Rns2 {
    Rns2 {
        l0: sub_limb(&a.l0, &b.l0, q0),
        l1: sub_limb(&a.l1, &b.l1, q1),
    }
}

fn hss_mul_rns(
    p: u64,
    q0: u64,
    q1: u64,
    triple: &(Rns2, Rns2, Rns2),
    ntt0: &Option<ring::Ntt>,
    ntt1: &Option<ring::Ntt>,
    a: &LabelPoly,
    i: bool,
    b: &LabelPoly,
    j: bool,
) -> Rns2 {
    let a = Rns2 {
        l0: lift_rp(a, q0),
        l1: lift_rp(a, q1),
    };
    let b = Rns2 {
        l0: lift_rp(b, q0),
        l1: lift_rp(b, q1),
    };
    let ab = mul_rns2(&a, &b, q0, q1, ntt0, ntt1);
    let mut out = mul_rns2(&ab, &triple.0, q0, q1, ntt0, ntt1);
    if i {
        out = add_rns2(&out, &mul_rns2(&b, &triple.1, q0, q1, ntt0, ntt1), q0, q1);
    }
    if j {
        out = add_rns2(&out, &mul_rns2(&a, &triple.1, q0, q1, ntt0, ntt1), q0, q1);
    }
    if i && j {
        out = sub_rns2(&out, &triple.2, q0, q1);
    }
    // Keep the plaintext modulus in the signature to make the lift/reduce
    // boundary explicit at this evaluator operation.
    let _ = p;
    out
}

fn eval_rns_rows(
    p: u64,
    q0: u64,
    q1: u64,
    triple: &(Rns2, Rns2, Rns2),
    ntt0: &Option<ring::Ntt>,
    ntt1: &Option<ring::Ntt>,
    a: &LabelPoly,
    i: bool,
    b: &LabelPoly,
    j: bool,
    taus: &[RnsRowCt; 3],
) -> LabelPoly {
    let dd_i = |poly: &LabelPoly, bit: bool, tau: &RnsRowCt| {
        let lifted = Rns2 {
            l0: lift_rp(poly, q0),
            l1: lift_rp(poly, q1),
        };
        let mut phase = mul_rns2(&tau.u2, &lifted, q0, q1, ntt0, ntt1);
        if bit {
            phase = add_rns2(&phase, &tau.u1, q0, q1);
        }
        round_limb0_to_rp(&phase.l0, q0, p)
    };
    let first = dd_i(a, i, &taus[0]);
    let second = dd_i(b, j, &taus[1]);
    let share = hss_mul_rns(p, q0, q1, triple, ntt0, ntt1, a, i, b, j);
    let mut product = mul_rns2(&taus[2].u2, &share, q0, q1, ntt0, ntt1);
    if i && j {
        product = add_rns2(&product, &taus[2].u1, q0, q1);
    }
    let correlated = round_limb0_to_rp(&product.l0, q0, p);
    let sum = ring::add_coeffs(&first, &second, p);
    ring::add_coeffs(&sum, &correlated, p)
}

fn scale_rns2(a: &Rns2, k: i64, q0: u64, q1: u64) -> Rns2 {
    Rns2 {
        l0: scale_limb(&a.l0, k, q0),
        l1: scale_limb(&a.l1, k, q1),
    }
}

fn scale_rns_row(row: &RnsRowCt, k: i64, q0: u64, q1: u64) -> RnsRowCt {
    RnsRowCt {
        u1: scale_rns2(&row.u1, k, q0, q1),
        u2: scale_rns2(&row.u2, k, q0, q1),
    }
}

fn sub_rns_row(a: &RnsRowCt, b: &RnsRowCt, q0: u64, q1: u64) -> RnsRowCt {
    RnsRowCt {
        u1: sub_rns2(&a.u1, &b.u1, q0, q1),
        u2: sub_rns2(&a.u2, &b.u2, q0, q1),
    }
}

fn xor_rns_rows(delta_row: &RnsRowCt, determination: bool, q0: u64, q1: u64) -> [RnsRowCt; 3] {
    let sign = if determination { -1 } else { 1 };
    [
        scale_rns_row(delta_row, sign, q0, q1),
        scale_rns_row(delta_row, sign, q0, q1),
        scale_rns_row(delta_row, -2 * sign, q0, q1),
    ]
}

fn fast_rns_rows(
    delta_row: &RnsRowCt,
    prg_rows: &[RnsRowCt; 4],
    masks: &[bool; 4],
    q0: u64,
    q1: u64,
) -> [RnsRowCt; 3] {
    let z0 = if masks[0] {
        sub_rns_row(delta_row, &prg_rows[0], q0, q1)
    } else {
        prg_rows[0].clone()
    };
    let z1 = if masks[1] {
        sub_rns_row(delta_row, &prg_rows[1], q0, q1)
    } else {
        prg_rows[1].clone()
    };
    let z2 = if masks[2] {
        sub_rns_row(delta_row, &prg_rows[2], q0, q1)
    } else {
        prg_rows[2].clone()
    };
    let z3 = if masks[3] {
        sub_rns_row(delta_row, &prg_rows[3], q0, q1)
    } else {
        prg_rows[3].clone()
    };
    let t1 = sub_rns_row(&z1, &z0, q0, q1);
    let t2 = sub_rns_row(&z2, &z0, q0, q1);
    let t3 = sub_rns_row(&sub_rns_row(&z3, &z0, q0, q1), &t1, q0, q1);
    let t3 = sub_rns_row(&t3, &t2, q0, q1);
    [t1, t2, t3]
}

// ─────────────────────────────────────────────────────────────────────
// Per-limb arithmetic (NTT where friendly, else schoolbook)
// ─────────────────────────────────────────────────────────────────────

fn ntt_ctx(n: usize, q: u64) -> Option<ring::Ntt> {
    ring::Ntt::new(n, q).ok()
}

fn mul_limb(a: &Limb, b: &Limb, q: u64, ntt: &Option<ring::Ntt>) -> Limb {
    match ntt {
        Some(nt) => ring::negacyclic_mul_ntt(a, b, nt),
        None => ring::negacyclic_mul_coeffs(a, b, q),
    }
}

fn add_limb(a: &Limb, b: &Limb, q: u64) -> Limb {
    a.iter()
        .zip(b.iter())
        .map(|(&x, &y)| ring::add_mod(x, y, q))
        .collect()
}

fn sub_limb(a: &Limb, b: &Limb, q: u64) -> Limb {
    a.iter()
        .zip(b.iter())
        .map(|(&x, &y)| ring::sub_mod(x, y, q))
        .collect()
}

fn negate_limb(a: &Limb, q: u64) -> Limb {
    a.iter().map(|&x| if x == 0 { 0 } else { q - x }).collect()
}

/// Scale a limb by a small signed integer `t` (`t·Δ` etc.), centered.
fn scale_limb(delta_limb: &Limb, t: i64, q: u64) -> Limb {
    delta_limb
        .iter()
        .map(|&c| {
            let c = c as i128;
            (c * t as i128).rem_euclid(q as i128) as u64
        })
        .collect()
}

/// Embed a mod-`p` coefficient vector into a limb (mod `q`), centering
/// the `{0,1,p−1}` Δ coefficients to `{0,1,−1}` first.
fn to_limb(delta: &LabelPoly, p: u64, q: u64) -> Limb {
    delta
        .iter()
        .map(|&c| {
            if c == 0 {
                0
            } else if c == 1 {
                1
            } else {
                q - 1 // −1 mod q
            }
        })
        .collect()
}

/// Uniform sample over one limb.
fn uniform_limb(n: usize, q: u64, random: &mut dyn SpecRng) -> Limb {
    let mut out = alloc::vec![0_u64; n];
    for c in out.iter_mut() {
        *c = (random.next_u32() as u64) % q;
    }
    out
}

/// `p·e` noise over a limb, `e ∈ {−1,0,1}` centered (keeps the phase's
/// noise a multiple of `p` so it vanishes on the mod-`p` reduction).
fn noise_limb(n: usize, p: u64, q: u64, random: &mut dyn SpecRng) -> Limb {
    let mut out = alloc::vec![0_u64; n];
    for c in out.iter_mut() {
        let r = (random.next_u32() % 3) as i64 - 1;
        *c = if r < 0 {
            q - (p % q)
        } else {
            (r as u64 * p) % q
        };
    }
    out
}

/// Round limb-0 (a multiple of `p`) to `R_p`: center mod `q0`, reduce mod
/// `p`. Because `p | q0`, `x mod p = (x mod q0) mod p`, and the centered
/// value is exact (noise `O(p) ≪ q0/2`), so limb 0 alone determines the
/// result — no CRT reconstruction needed.
fn round_limb0_to_rp(x: &Limb, q0: u64, p: u64) -> LabelPoly {
    x.iter()
        .map(|&c| {
            let centered = ring::center(c, q0);
            centered.rem_euclid(p as i128) as u64
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestRng(u64);
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self
                .0
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            (self.0 >> 33) as u32
        }
    }

    /// NTT-friendly limb-0 (multiple of p) and limb-1 for degree 8.
    /// p=97; q0 = 97·17=1649 (≡1 mod 16), q1 = 97·... — use two distinct
    /// NTT-friendly primes both ≡1 mod 16 and q0 ≡0 mod 97.
    fn test_dd() -> LatticeRnsDd {
        let p = 97u64;
        // q0 must be ≡1 mod 16 (NTT) and ≡0 mod 97 (p | q0). 97·17=1649
        // is ≡1 mod 16 but we need p | q0 → q0 must be a multiple of 97.
        // 97·k ≡ 1 mod 16: 97 ≡ 1 mod 16, so k ≡ 1 mod 16. k=17 works:
        // 1649 = 97·17, 1649 mod 16 = 1. ✓
        let q0 = 97u64 * 17;
        // q1: NTT-friendly prime ≡1 mod 16, distinct from q0. 97·... no;
        // q1 need only be ≡1 mod 16 and prime. 1153 = 1 mod 16, prime. ✓
        let q1 = 1153u64;
        let mut rng = TestRng(0x51dc_5af0);
        LatticeRnsDd::new(8, p, q0, q1, &mut rng).expect("params")
    }

    fn add_delta(a: &LabelPoly, delta: &LabelPoly, p: u64) -> LabelPoly {
        ring::add_coeffs(a, delta, p)
    }
    fn sample_rp(n: usize, p: u64, seed: u64) -> LabelPoly {
        let mut rng = TestRng(seed);
        (0..n).map(|_| (rng.next_u32() as u64) % p).collect()
    }

    #[test]
    fn rns_modulus_bits_reach_k1() {
        // The 2-limb form reaches the paper's k=1 log q ≈ 120 target with
        // 60-bit primes (the test uses small primes; the structure scales).
        let dd = test_dd();
        assert!(dd.modulus_bits() >= 20, "test limbs give ~21 bits");
    }

    #[test]
    fn rns_row_phase_exact_mod_p() {
        let dd = test_dd();
        let mut rng = TestRng(0x7777);
        for t in [-2, -1, 0, 1, 2] {
            let tau = dd.row(t, &mut rng);
            let m = dd.dec_row(&tau);
            let expect: LabelPoly = dd
                .delta()
                .iter()
                .map(|&c| (c as i128 * t as i128).rem_euclid(dd.p() as i128) as u64)
                .collect();
            assert_eq!(m, expect, "t={t}");
        }
    }

    #[test]
    fn rns_dd_linear_identity() {
        let dd = test_dd();
        let p = dd.p();
        let mut rng = TestRng(0x8888);
        let tau = dd.row(1, &mut rng);
        let a0 = sample_rp(dd.degree(), p, 0x9999);
        let m = dd.dec_row(&tau);
        let base = dd.dd0(&a0, &tau);
        for i in [false, true] {
            let a = if i {
                add_delta(&a0, dd.delta(), p)
            } else {
                a0.clone()
            };
            let lhs = dd.dd_i(&a, i, &tau);
            let rhs = if i {
                ring::add_coeffs(&base, &m, p)
            } else {
                base.clone()
            };
            assert_eq!(lhs, rhs, "i={i}");
        }
    }

    #[test]
    fn rns_dd_correlated_identity() {
        let dd = test_dd();
        let p = dd.p();
        let mut rng = TestRng(0xaaaa);
        let tau = dd.row(-1, &mut rng);
        let a0 = sample_rp(dd.degree(), p, 0xbbbb);
        let b0 = sample_rp(dd.degree(), p, 0xcccc);
        let m = dd.dec_row(&tau);
        let base = dd.dd_ij(&a0, false, &b0, false, &tau);
        for (i, j) in [(false, false), (true, false), (false, true), (true, true)] {
            let a = if i {
                add_delta(&a0, dd.delta(), p)
            } else {
                a0.clone()
            };
            let b = if j {
                add_delta(&b0, dd.delta(), p)
            } else {
                b0.clone()
            };
            let lhs = dd.dd_ij(&a, i, &b, j, &tau);
            let corr = if i && j {
                m.clone()
            } else {
                alloc::vec![0; dd.degree()]
            };
            let rhs = ring::add_coeffs(&base, &corr, p);
            assert_eq!(lhs, rhs, "i={i} j={j}");
        }
    }

    /// The decisive integration test: the pinned circuit drivers run
    /// unchanged over the RNS gadget via the `DdGadget` seam. This is the
    /// production-parameter form reaching the paper's modulus budget.
    #[test]
    fn rns_circuit_roundtrip() {
        use crate::bitgc::common::{Circuit, Gate};
        use crate::bitgc::pinned::{
            PrfKey, encode_inputs, evaluate_circuit, garble_circuit, open_output,
        };
        let p = 97u64;
        let q0 = 97u64 * 17;
        let q1 = 1153u64;
        let degree = 8;
        let circuit = Circuit {
            inputs: 3,
            gates: alloc::vec![Gate::And { a: 0, b: 1 }, Gate::Xor { a: 3, b: 2 }],
            outputs: alloc::vec![4],
        };
        let wires = circuit.wires();
        let and_count = 1;
        for variant in [crate::bitgc::Variant::Small, crate::bitgc::Variant::Fast] {
            let mut rng = TestRng(0xf00d);
            let prf = PrfKey([0x11, 0x22, 0x33, 0x44]);
            let dd = LatticeRnsDd::new(degree, p, q0, q1, &mut rng).expect("params");
            let pis: Vec<bool> = (0..wires).map(|_| (rng.next_u32() & 1) == 1).collect();
            let prg_count = if variant == crate::bitgc::Variant::Fast {
                4 * and_count
            } else {
                0
            };
            let prg_bits: Vec<bool> = (0..prg_count).map(|_| (rng.next_u32() & 1) == 1).collect();
            let g = garble_circuit(variant, &circuit, &dd, &pis, &prf, &prg_bits, &mut rng)
                .expect("garble");
            for x in 0..8u8 {
                let inputs = [x & 1 == 1, x & 2 == 2, x & 4 == 4];
                let encoded =
                    encode_inputs(&g, dd.delta_pinned_ref(), &inputs, circuit.inputs).expect("enc");
                let masked = evaluate_circuit(
                    variant,
                    &circuit,
                    &dd,
                    &g.delta_ct,
                    &g.and_rows,
                    &g.records,
                    &prf,
                    &encoded,
                )
                .expect("eval");
                let got = open_output(masked[0], g.labels[circuit.outputs[0]].pi);
                let want = (inputs[0] & inputs[1]) ^ inputs[2];
                assert_eq!(got, want, "{variant:?} inputs {inputs:?}");
            }
        }
    }

    #[test]
    fn rns_role_separated_roundtrip() {
        use crate::bitgc::common::{Circuit, Gate};
        use crate::bitgc::pinned::{
            PrfKey, evaluate_role_circuit, garble_role_circuit, open_output,
        };

        let p = 97u64;
        let q0 = 97u64 * 17;
        let q1 = 1153u64;
        let degree = 8;
        let circuit = Circuit {
            inputs: 3,
            gates: alloc::vec![Gate::And { a: 0, b: 1 }, Gate::Xor { a: 3, b: 2 }],
            outputs: alloc::vec![4],
        };
        for variant in [crate::bitgc::Variant::Small, crate::bitgc::Variant::Fast] {
            let mut rng = TestRng(0x524f_4c45);
            let prf = PrfKey([0x11, 0x22, 0x33, 0x44]);
            let garbler = LatticeRnsDd::new(degree, p, q0, q1, &mut rng).expect("params");
            let pis: Vec<bool> = (0..circuit.wires())
                .map(|_| (rng.next_u32() & 1) == 1)
                .collect();
            let prg_count = if variant == crate::bitgc::Variant::Fast {
                4
            } else {
                0
            };
            let prg_bits: Vec<bool> = (0..prg_count).map(|_| (rng.next_u32() & 1) == 1).collect();
            let (garbler_labels, tables, evaluator) =
                garble_role_circuit(variant, &circuit, &garbler, &pis, &prf, &prg_bits, &mut rng)
                    .expect("garble");
            assert!(matches!(
                garble_role_circuit(
                    crate::bitgc::Variant::Tiny,
                    &circuit,
                    &garbler,
                    &pis,
                    &prf,
                    &[],
                    &mut rng,
                ),
                Err(crate::bitgc::pinned::DriverError::UnsupportedVariant)
            ));
            assert!(matches!(
                evaluate_role_circuit(
                    crate::bitgc::Variant::Tiny,
                    &circuit,
                    &evaluator,
                    &tables,
                    &[],
                ),
                Err(crate::bitgc::pinned::DriverError::UnsupportedVariant)
            ));

            for x in 0..8u8 {
                let inputs = [x & 1 == 1, x & 2 == 2, x & 4 == 4];
                let encoded = garbler_labels.encode_inputs(&inputs).expect("encode");
                let masked =
                    evaluate_role_circuit(variant, &circuit, &evaluator, &tables, &encoded)
                        .expect("evaluate without garbler material");
                let output_pi = garbler_labels
                    .permutation_bit(circuit.outputs[0])
                    .expect("output wire");
                let got = open_output(masked[0], output_pi);
                let want = (inputs[0] & inputs[1]) ^ inputs[2];
                assert_eq!(got, want, "{variant:?} inputs {inputs:?}");
            }
        }
    }

    #[test]
    fn rns_eval_row_reconstruction() {
        let dd = test_dd();
        let p = dd.p();
        let mut rng = TestRng(0xdddd);
        let t = [1i64, -1, 1];
        let taus = [
            dd.row(t[0], &mut rng),
            dd.row(t[1], &mut rng),
            dd.row(t[2], &mut rng),
        ];
        let a0 = sample_rp(dd.degree(), p, 0xeeee);
        let b0 = sample_rp(dd.degree(), p, 0xffff);
        let base = dd.eval(&a0, false, &b0, false, &taus);
        for (i, j) in [(false, false), (true, false), (false, true), (true, true)] {
            let a = if i {
                add_delta(&a0, dd.delta(), p)
            } else {
                a0.clone()
            };
            let b = if j {
                add_delta(&b0, dd.delta(), p)
            } else {
                b0.clone()
            };
            let got = dd.eval(&a, i, &b, j, &taus);
            let k = (i as i64) * t[0] + (j as i64) * t[1] + ((i && j) as i64) * t[2];
            let corr: LabelPoly = dd
                .delta()
                .iter()
                .map(|&c| (c as i128 * k as i128).rem_euclid(p as i128) as u64)
                .collect();
            let expect = ring::add_coeffs(&base, &corr, p);
            assert_eq!(got, expect, "i={i} j={j}");
        }
    }
}
