// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-pinning
//! @ai: assisted
//! The **lattice SWHEwDD gadget** — the production DD primitive for the
//! pinned BitGC construction, porting [LWYY25b] §2.5/§5 and [ZLL+26]
//! App. A onto the coefficient-form ring. This is the lattice analogue of
//! the exact [`super::pinned::ReferenceDd`], which is cryptographically
//! hollow; here the row ciphertexts are real RLWE encryptions carrying
//! Gaussian-style noise.
//!
//! # Construction ([LWYY25b] §2.5, §5)
//!
//! The key is `Δ ∈ R` ternary with `LSB(Δ) = 1`. A **row ciphertext**
//! encrypting a row multiplier `t ∈ {−1,0,1,±2}` is a GSW-style RLWE
//! ciphertext under `Δ`:
//!
//! ```text
//! τ = (u1, u2) over R_q,   u1 = −u2·Δ + t·Δ + p·e
//! ```
//!
//! so the phase `u1 + u2·Δ = t·Δ + p·e` holds mod `q` (BGV-style
//! invariant, no `q/p` scaling). The special-decryption
//! `Dec(Δ, τ) = u1 + u2·Δ ≈ t·Δ` recovers `t·Δ` up to the noise `p·e`.
//!
//! # Distributed decryption — the key correctness theorem
//!
//! The DD operations work mod `q` and then **round to `R_p`** (center mod
//! `q`, reduce mod `p`). Because the noise is a multiple of `p`, it
//! vanishes under this reduction, so the DD identities hold **exactly**
//! over `R_p` (probability 1, no wrap term — unlike the label-LSB
//! wrap term analyzed in `docs/research/bitgc-paper-pinning.md`):
//!
//! ```text
//! DD_i(A, τ)     := round_p(u2·A + i·u1)              (k=1)
//!   ⇒ DD_i(A0 + iΔ, τ) = DD_0(A0, τ) + i·t·Δ           (mod p, exact)
//! DD_{i,j}(A,B,τ):= round_p(c1·HssMul_{i,j}(A,B) + i·j·c0)   (k=2)
//! ```
//!
//! The k=2 `HssMul` uses a **CP-RLWE triple** `(U,V,W)` over `R_q`
//! ([ZLL+26] App. A / [ILL25b]; the artifact's `HSSMul`):
//!
//! ```text
//! U·Δ + V   = p·e₁        (≈ 0)
//! U·Δ² + W  = −Δ + p·e₂   (≈ −Δ)
//! HssMul_{i,j}(A,B) = A·B·U + (i·B + j·A)·V − i·j·W   (over R_q)
//!   ⇒ ≡ U·A0·B0 + i·j·Δ  (mod p, exact)
//! ```
//!
//! # Relation to `ReferenceDd`
//!
//! Mod-`p` reduction of a lattice row `(u1, u2)` yields `(c0, c1) =
//! (u1 mod p, u2 mod p)` with `c0 + c1·Δ ≡ t·Δ (mod p)` — exactly the
//! reference gadget's `RowCt`. So the lattice gadget **degrades to the
//! reference gadget** under `round_p`, which is why the pinned circuit
//! drivers (`garble_circuit`/`evaluate_circuit`) are written against the
//! shared [`DdGadgetLattice`] seam and run unchanged over either.
//!
//! # Honesty boundary
//!
//! This gadget carries real RLWE noise and the identities are exact mod
//! `p`, but it is a **reference implementation for the spec crate**: it
//! uses schoolbook `O(N²)` negacyclic multiplication (no NTT) and a
//! simple symmetric-noise sampler, and it does not implement the
//! bit-decomposition / extended-gadget variants the paper uses for the
//! 128-bit parameter profile. Security rests on RLWE with KDM-style
//! key-dependent messages (`t·Δ` under key `Δ`), which is **assumed** by
//! the paper and flagged for independent review under BITGC-LEDGER-03.
//! Parameter shapes (log p ≈ 60, log q ≈ 120 for k=1, log r ≈ 240 for
//! k=2, N_PRG = 2^15) are in [ZLL+26] §6.
//!
//! **Do not use for protected data** until BITGC-LEDGER-03 passes.

use alloc::vec::Vec;

use super::pinned::{DdGadget, Delta, LabelPoly, RowCt};
use super::ring;
use crate::SpecRng;

/// Reduce a lattice row ciphertext `(u1, u2)` over `R_q` to the exact
/// `R_p` reference form `(c0, c1) = (u1 mod p, u2 mod p)`.
///
/// Because the phase `u1 + u2·Δ = t·Δ + p·e` reduces to
/// `c0 + c1·Δ ≡ t·Δ (mod p)` (the `p·e` noise vanishes), the result is a
/// valid reference [`RowCt`]. This is the concrete statement that the
/// lattice gadget degrades to the reference gadget under mod-`p`
/// reduction.
fn rowct_from_lattice(tau: &LatticeRowCt, p: u64) -> RowCt {
    let red = |poly: &RqPoly| -> LabelPoly { poly.iter().map(|&c| c % p).collect() };
    RowCt {
        c0: red(&tau.u1),
        c1: red(&tau.u2),
    }
}

impl DdGadget for LatticeDd {
    fn p(&self) -> u64 {
        self.p
    }

    fn delta(&self) -> &Delta {
        // Reinterpret the stored `RpPoly` Δ as a `pinned::Delta`. The
        // `pinned::Delta` invariant (ternary, LSB 1) is maintained by
        // `LatticeDd::new`. We store the `pinned::Delta` alongside for
        // the seam; see the field.
        &self.delta_pinned
    }

    fn row(&self, t: i64, random: &mut dyn SpecRng) -> RowCt {
        // Generate a real lattice row (RLWE over `R_q` with noise), then
        // reduce to the exact reference form the drivers consume.
        let tau = LatticeDd::row(self, t, random);
        rowct_from_lattice(&tau, self.p)
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
        // HssMul with the reduced (mod-p) triple, then c1·share + i·j·c0.
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

/// A ring element over `R_q` (coefficient form, `Vec<u64>`, length =
/// degree). Distinct from [`super::pinned::LabelPoly`] only in modulus.
pub type RqPoly = Vec<u64>;

/// The plaintext/label ring element over `R_p` (coefficient form).
pub type RpPoly = Vec<u64>;

/// Errors from the lattice DD gadget.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LatticeError {
    /// A degree / length mismatch.
    LengthMismatch,
    /// The modulus constraints (`p | q`, both odd) were violated.
    InvalidParameters,
}

/// A GSW-style RLWE row ciphertext `τ = (u1, u2)` over `R_q` with
/// `u1 + u2·Δ = t·Δ + p·e`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LatticeRowCt {
    /// `u1 = −u2·Δ + t·Δ + p·e`.
    pub u1: RqPoly,
    /// `u2`, uniform over `R_q`.
    pub u2: RqPoly,
}

/// The lattice DD gadget context: ring degree, moduli, key `Δ`, and the
/// CP-RLWE triple for k=2. Created garbler-side; the row-generation and
/// `dec_row` methods use `Δ`, the DD methods do not (evaluator-safe).
#[derive(Clone, Debug)]
pub struct LatticeDd {
    /// Ring degree `N`.
    n: usize,
    /// Plaintext modulus `p` (odd).
    p: u64,
    /// Ciphertext modulus `q` (odd multiple of `p`).
    q: u64,
    /// The key `Δ` (ternary, `LSB(Δ)=1`), coefficient form mod `p`.
    delta: RpPoly,
    /// The same `Δ` as a `pinned::Delta` for the [`DdGadget`] seam.
    delta_pinned: Delta,
    /// CP-RLWE triple `(U, V, W)` over `R_q` (see module docs).
    triple: (RqPoly, RqPoly, RqPoly),
    /// The triple reduced mod `p`, for the driver-facing seam's k=2
    /// `HssMul` (the relations hold mod `p` exactly).
    triple_rp: (RpPoly, RpPoly, RpPoly),
    /// NTT context over `R_q`, present when `q` is NTT-friendly
    /// (`q ≡ 1 mod 2N`). When absent the schoolbook path is used.
    ntt_q: Option<ring::Ntt>,
    /// NTT context over `R_p`, present when `p ≡ 1 mod 2N`.
    ntt_p: Option<ring::Ntt>,
}

impl LatticeDd {
    /// Build the gadget: sample `Δ` ternary with `LSB(Δ)=1` and the
    /// CP-RLWE triple over `R_q`.
    ///
    /// `q` must be an odd multiple of the odd `p` (so the mod-`p`
    /// reduction of the phase is exact). `random` must be a CSPRNG for
    /// any non-test use.
    pub fn new(n: usize, p: u64, q: u64, random: &mut dyn SpecRng) -> Result<Self, LatticeError> {
        if n == 0 || p < 3 || p % 2 == 0 || q % p != 0 || q % 2 == 0 {
            return Err(LatticeError::InvalidParameters);
        }
        // Δ ternary in {−1,0,1}, forcing coeff 0 = 1 (LSB(Δ)=1).
        let mut delta = alloc::vec![0_u64; n];
        for c in delta.iter_mut() {
            // Map 2 bits to {0, ±1} with a slight bias toward 0.
            let r = random.next_u32() % 3;
            *c = match r {
                0 => 0,
                1 => 1,
                _ => p - 1, // −1 mod p
            };
        }
        delta[0] = 1;
        // Δ as an R_q element (centered: −1 ↦ q−1) for phase arithmetic.
        let delta_q: RqPoly = delta
            .iter()
            .map(|&c| {
                if c == 0 {
                    0
                } else if c == 1 {
                    1
                } else {
                    q - 1
                }
            })
            .collect();

        // CP-RLWE triple over R_q:
        //   U uniform;  V = −U·Δ + p·e₁;  W = −Δ − U·Δ² + p·e₂.
        let u = sample_uniform_rq(n, q, random);
        let ud = negacyclic_mul_rq(&u, &delta_q, q);
        let e1 = sample_noise_rq(n, p, q, random);
        let v = add_rq(&negate_rq(&ud, q), &e1, q);
        let d2_q = negacyclic_mul_rq(&delta_q, &delta_q, q);
        let ud2 = negacyclic_mul_rq(&u, &d2_q, q);
        let e2 = sample_noise_rq(n, p, q, random);
        // W = −Δ − U·Δ² + p·e₂  (mod q), with −Δ centered over R_q.
        let neg_delta_q = negate_rq(&delta_q, q);
        let w = add_rq(&sub_rq(&neg_delta_q, &ud2, q), &e2, q);
        // The mod-p shadow of the triple (relations hold mod p exactly).
        let red = |poly: &RqPoly| -> RpPoly { poly.iter().map(|&c| c % p).collect() };
        let triple_rp = (red(&u), red(&v), red(&w));
        let delta_pinned = Delta {
            coeffs: delta.clone(),
            p,
        };
        // NTT contexts where the moduli are NTT-friendly.
        let ntt_q = ring::Ntt::new(n, q).ok();
        let ntt_p = ring::Ntt::new(n, p).ok();
        Ok(Self {
            n,
            p,
            q,
            delta,
            delta_pinned,
            triple: (u, v, w),
            triple_rp,
            ntt_q,
            ntt_p,
        })
    }

    /// Negacyclic product over `R_q` — NTT when `q` is friendly, else
    /// schoolbook.
    fn mul_q(&self, a: &[u64], b: &[u64]) -> RqPoly {
        match &self.ntt_q {
            Some(ntt) => ring::negacyclic_mul_ntt(a, b, ntt),
            None => negacyclic_mul_rq(a, b, self.q),
        }
    }

    /// Negacyclic product over `R_p` — NTT when `p` is friendly, else
    /// schoolbook.
    fn mul_p(&self, a: &[u64], b: &[u64]) -> RpPoly {
        match &self.ntt_p {
            Some(ntt) => ring::negacyclic_mul_ntt(a, b, ntt),
            None => ring::negacyclic_mul_coeffs(a, b, self.p),
        }
    }

    /// `HssMul` over `R_p` using the reduced triple (driver-facing).
    fn hss_mul_rp(&self, a: &RpPoly, i: bool, b: &RpPoly, j: bool) -> RpPoly {
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

    /// Ring degree.
    pub const fn degree(&self) -> usize {
        self.n
    }

    /// Plaintext modulus.
    pub const fn p(&self) -> u64 {
        self.p
    }

    /// Ciphertext modulus.
    pub const fn q(&self) -> u64 {
        self.q
    }

    /// The key `Δ` (garbler-side).
    pub fn delta(&self) -> &RpPoly {
        &self.delta
    }

    /// Generate a row ciphertext for multiplier `t ∈ {−1,0,1,±2}`
    /// ([ZLL+26] Fig. 4b/6b; XOR uses `t₃ = ±2`).
    ///
    /// `τ = (u1, u2)`, `u2` uniform over `R_q`,
    /// `u1 = −u2·Δ + t·Δ + p·e`.
    pub fn row(&self, t: i64, random: &mut dyn SpecRng) -> LatticeRowCt {
        let (n, p, q) = (self.n, self.p, self.q);
        let u2 = sample_uniform_rq(n, q, random);
        // Δ as R_q (centered).
        let delta_q: RqPoly = self
            .delta
            .iter()
            .map(|&c| {
                if c == 0 {
                    0
                } else if c == 1 {
                    1
                } else {
                    q - 1
                }
            })
            .collect();
        let u2d = self.mul_q(&u2, &delta_q);
        // t·Δ over R_q (t may be ±2 for the XOR τ₃ row).
        let td: RqPoly = delta_q
            .iter()
            .map(|&c| {
                let c = c as i128;
                let v = c * t as i128;
                v.rem_euclid(q as i128) as u64
            })
            .collect();
        let e = sample_noise_rq(n, p, q, random);
        let u1 = add_rq(&sub_rq(&td, &u2d, q), &e, q);
        LatticeRowCt { u1, u2 }
    }

    /// Garbler-side special decryption `Dec(Δ, τ) = u1 + u2·Δ`
    /// (`≈ t·Δ`), reduced to `R_p` (noise vanishes: `= t·Δ` mod p).
    pub fn dec_row(&self, tau: &LatticeRowCt) -> RpPoly {
        let q = self.q;
        let delta_q: RqPoly = self
            .delta
            .iter()
            .map(|&c| {
                if c == 0 {
                    0
                } else if c == 1 {
                    1
                } else {
                    q - 1
                }
            })
            .collect();
        let u2d = self.mul_q(&tau.u2, &delta_q);
        let phase = add_rq(&tau.u1, &u2d, q);
        round_to_rp(&phase, q, self.p)
    }

    /// `DD_0(A, τ) = round_p(u2·A)` (k=1).
    pub fn dd0(&self, a: &RpPoly, tau: &LatticeRowCt) -> RpPoly {
        // A (mod p) embedded into R_q; product over R_q; round to R_p.
        let a_q: RqPoly = a.iter().map(|&c| c % self.q).collect();
        let prod = self.mul_q(&tau.u2, &a_q);
        round_to_rp(&prod, self.q, self.p)
    }

    /// `DD_i(A, τ) = round_p(u2·A + i·u1)` (k=1).
    pub fn dd_i(&self, a: &RpPoly, i: bool, tau: &LatticeRowCt) -> RpPoly {
        let mut acc = self.dd0_raw(a, tau);
        if i {
            acc = add_rq(&acc, &tau.u1, self.q);
        }
        round_to_rp(&acc, self.q, self.p)
    }

    /// Raw (unrounded, mod-q) `u2·A` — shared by `dd0`/`dd_i`.
    fn dd0_raw(&self, a: &RpPoly, tau: &LatticeRowCt) -> RqPoly {
        let a_q: RqPoly = a.iter().map(|&c| c % self.q).collect();
        self.mul_q(&tau.u2, &a_q)
    }

    /// The k=2 share product with the CP-RLWE triple (over `R_q`):
    /// `HssMul_{i,j}(A,B) = A·B·U + (i·B + j·A)·V − i·j·W`.
    ///
    /// With `A = A0+iΔ`, `B = B0+jΔ` and the triple's relations, this is
    /// `≡ U·A0·B0 + i·j·Δ (mod p)` exactly after rounding.
    pub fn hss_mul(&self, a: &RpPoly, i: bool, b: &RpPoly, j: bool) -> RpPoly {
        let q = self.q;
        let (u, v, w) = &self.triple;
        let a_q: RqPoly = a.iter().map(|&c| c % q).collect();
        let b_q: RqPoly = b.iter().map(|&c| c % q).collect();
        let ab = self.mul_q(&a_q, &b_q);
        let mut acc = self.mul_q(&ab, u);
        if i {
            let bv = self.mul_q(&b_q, v);
            acc = add_rq(&acc, &bv, q);
        }
        if j {
            let av = self.mul_q(&a_q, v);
            acc = add_rq(&acc, &av, q);
        }
        if i && j {
            acc = sub_rq(&acc, w, q);
        }
        round_to_rp(&acc, q, self.p)
    }

    /// `DD_{i,j}(A, B, τ) = round_p(u2·HssMul_{i,j}(A,B) + i·j·u1)` (k=2).
    pub fn dd_ij(&self, a: &RpPoly, i: bool, b: &RpPoly, j: bool, tau: &LatticeRowCt) -> RpPoly {
        let q = self.q;
        let share = self.hss_mul_raw(a, i, b, j);
        let mut acc = self.mul_q(&tau.u2, &share);
        if i && j {
            acc = add_rq(&acc, &tau.u1, q);
        }
        round_to_rp(&acc, q, self.p)
    }

    /// Raw (mod-q) `HssMul` share — shared by `hss_mul`/`dd_ij`.
    fn hss_mul_raw(&self, a: &RpPoly, i: bool, b: &RpPoly, j: bool) -> RqPoly {
        let q = self.q;
        let (u, v, w) = &self.triple;
        let a_q: RqPoly = a.iter().map(|&c| c % q).collect();
        let b_q: RqPoly = b.iter().map(|&c| c % q).collect();
        let ab = self.mul_q(&a_q, &b_q);
        let mut acc = self.mul_q(&ab, u);
        if i {
            let bv = self.mul_q(&b_q, v);
            acc = add_rq(&acc, &bv, q);
        }
        if j {
            let av = self.mul_q(&a_q, v);
            acc = add_rq(&acc, &av, q);
        }
        if i && j {
            acc = sub_rq(&acc, w, q);
        }
        acc
    }

    /// The gate evaluator ([ZLL+26] §3.3):
    /// `Eval_{i,j}(A, B, τ) = DD_i(A,τ₁) + DD_j(B,τ₂) + DD_{i,j}(A,B,τ₃)`,
    /// summed over `R_p`.
    pub fn eval(
        &self,
        a: &RpPoly,
        i: bool,
        b: &RpPoly,
        j: bool,
        taus: &[LatticeRowCt; 3],
    ) -> RpPoly {
        let p = self.p;
        let mut out = self.dd_i(a, i, &taus[0]);
        out = add_rp(&out, &self.dd_i(b, j, &taus[1]), p);
        add_rp(&out, &self.dd_ij(a, i, b, j, &taus[2]), p)
    }
}

// ─────────────────────────────────────────────────────────────────────
// R_q / R_p coefficient arithmetic (schoolbook negacyclic; no NTT)
// ─────────────────────────────────────────────────────────────────────

/// Negacyclic convolution mod `q` (schoolbook, `O(N²)`).
///
/// Accumulates in signed 128-bit (the negacyclic wrap subtracts), which
/// is safe for the reference profile: with degree `N` and moduli `< 2^63`,
/// `|sum| < N·q² ≪ 2^127`. Reduction to `[0, q)` happens once at the end.
fn negacyclic_mul_rq(a: &[u64], b: &[u64], q: u64) -> RqPoly {
    let n = a.len();
    let mut out = alloc::vec![0_i128; n];
    for (i, &ai) in a.iter().enumerate() {
        if ai == 0 {
            continue;
        }
        // Center `a` so products stay small (equivalent mod q).
        let ai = ring::center(ai, q);
        for (j, &bj) in b.iter().enumerate() {
            if bj == 0 {
                continue;
            }
            let bj = ring::center(bj, q);
            let prod = ai * bj;
            if i + j < n {
                out[i + j] += prod;
            } else {
                // X^N = −1: wrap with sign flip.
                out[i + j - n] -= prod;
            }
        }
    }
    out.iter()
        .map(|&c| c.rem_euclid(q as i128) as u64)
        .collect()
}

fn add_rq(a: &[u64], b: &[u64], q: u64) -> RqPoly {
    a.iter()
        .zip(b.iter())
        .map(|(&x, &y)| ring::add_mod(x, y, q))
        .collect()
}

fn sub_rq(a: &[u64], b: &[u64], q: u64) -> RqPoly {
    a.iter()
        .zip(b.iter())
        .map(|(&x, &y)| ring::sub_mod(x, y, q))
        .collect()
}

fn negate_rq(a: &[u64], q: u64) -> RqPoly {
    a.iter().map(|&x| if x == 0 { 0 } else { q - x }).collect()
}

fn add_rp(a: &[u64], b: &[u64], p: u64) -> RpPoly {
    a.iter()
        .zip(b.iter())
        .map(|(&x, &y)| ring::add_mod(x, y, p))
        .collect()
}

/// Round an `R_q` element to `R_p`: center each coefficient mod `q` and
/// reduce mod `p`. Noise that is a multiple of `p` vanishes.
fn round_to_rp(x: &RqPoly, q: u64, p: u64) -> RpPoly {
    x.iter()
        .map(|&c| {
            let centered = ring::center(c, q);
            centered.rem_euclid(p as i128) as u64
        })
        .collect()
}

/// Uniform sample over `R_q`.
fn sample_uniform_rq(n: usize, q: u64, random: &mut dyn SpecRng) -> RqPoly {
    let mut out = alloc::vec![0_u64; n];
    for c in out.iter_mut() {
        *c = (random.next_u32() as u64) % q;
    }
    out
}

/// Gaussian-style noise scaled by `p`: returns `p·e` over `R_q` with `e`
/// a small centered polynomial (coeffs in `{−1,0,1}` mod `q` after the
/// `p` scaling). This keeps the phase's noise a multiple of `p` so it
/// vanishes on the mod-`p` reduction.
fn sample_noise_rq(n: usize, p: u64, q: u64, random: &mut dyn SpecRng) -> RqPoly {
    let mut out = alloc::vec![0_u64; n];
    for c in out.iter_mut() {
        let r = (random.next_u32() % 3) as i64 - 1; // {−1,0,1}
        // p·e mod q, with e ∈ {−1,0,1} centered.
        let pe = if r < 0 {
            q - (p % q) // −p mod q
        } else {
            (r as u64 * p) % q
        };
        *c = pe;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitgc::Variant;
    use crate::bitgc::common::{Circuit, Gate};
    use crate::bitgc::pinned::{
        PrfKey, encode_inputs, evaluate_circuit, garble_circuit, open_output,
    };

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

    /// Test params: small degree, p=97, q = 97·(2^k)+... an odd multiple.
    /// We use a modest q for the schoolbook reference; the paper uses
    /// log p ≈ 60, log q ≈ 120 (k=1). Documented as a reduced profile.
    fn test_dd() -> LatticeDd {
        let p = 97u64;
        let q = 97u64 * 3 * 5 * 7 * 11 * 13; // odd multiple of p
        let mut rng = TestRng(0xabcd_1234_5678_ef90);
        LatticeDd::new(8, p, q, &mut rng).expect("params")
    }

    fn sample_rp(n: usize, p: u64, seed: u64) -> RpPoly {
        let mut rng = TestRng(seed);
        (0..n).map(|_| (rng.next_u32() as u64) % p).collect()
    }

    fn add_delta(a: &RpPoly, delta: &RpPoly, p: u64) -> RpPoly {
        add_rp(a, delta, p)
    }

    #[test]
    fn row_phase_is_exact_mod_p() {
        // dec_row(τ) = t·Δ mod p exactly (noise vanishes).
        let dd = test_dd();
        let mut rng = TestRng(0x1111);
        for t in [-2, -1, 0, 1, 2] {
            let tau = dd.row(t, &mut rng);
            let m = dd.dec_row(&tau);
            // Expected: t·Δ mod p.
            let delta = dd.delta();
            let expect: RpPoly = delta
                .iter()
                .map(|&c| {
                    let c = c as i128;
                    (c * t as i128).rem_euclid(dd.p() as i128) as u64
                })
                .collect();
            assert_eq!(m, expect, "t={t}");
        }
    }

    #[test]
    fn dd_linear_identity_exact() {
        // DD_i(A0 + iΔ, τ) = DD_0(A0, τ) + i·Dec(Δ,τ) exactly mod p.
        let dd = test_dd();
        let p = dd.p();
        let mut rng = TestRng(0x2222);
        let tau = dd.row(1, &mut rng);
        let a0 = sample_rp(dd.degree(), p, 0x3333);
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
                add_rp(&base, &m, p)
            } else {
                base.clone()
            };
            assert_eq!(lhs, rhs, "i={i}");
        }
    }

    #[test]
    fn dd_correlated_identity_exact() {
        // DD_{i,j}(A0+iΔ, B0+jΔ, τ) = DD_{0,0}(A0,B0,τ) + i·j·Dec(Δ,τ).
        let dd = test_dd();
        let p = dd.p();
        let mut rng = TestRng(0x4444);
        let tau = dd.row(-1, &mut rng);
        let a0 = sample_rp(dd.degree(), p, 0x5555);
        let b0 = sample_rp(dd.degree(), p, 0x6666);
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
            let rhs = add_rp(&base, &corr, p);
            assert_eq!(lhs, rhs, "i={i} j={j}");
        }
    }

    #[test]
    fn hss_mul_product_share_exact() {
        // HssMul(A0+iΔ, B0+jΔ) = U·A0·B0 + i·j·Δ (mod p).
        let dd = test_dd();
        let p = dd.p();
        let a0 = sample_rp(dd.degree(), p, 0x7777);
        let b0 = sample_rp(dd.degree(), p, 0x8888);
        let base_share = dd.hss_mul(&a0, false, &b0, false);
        for (i, j) in [(true, false), (false, true), (true, true)] {
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
            let got = dd.hss_mul(&a, i, &b, j);
            let expect = if i && j {
                add_rp(&base_share, dd.delta(), p)
            } else {
                base_share.clone()
            };
            assert_eq!(got, expect, "i={i} j={j}");
        }
    }

    /// The decisive integration test: the pinned circuit drivers
    /// ([`garble_circuit`]/[`evaluate_circuit`]) run **unchanged** over the
    /// lattice gadget via the [`DdGadget`] seam — the same `(x0∧x1)⊕x2`
    /// truth table the reference gadget passes. This proves the lattice
    /// gadget's rows are valid `RowCt`s and the DD algebra composes.
    fn circuit_roundtrip(variant: Variant, inputs: [bool; 3]) -> bool {
        let p = 4_611_686_018_427_387_919u64; // 2^62+135, prime
        let q = p.wrapping_mul(3); // odd multiple of p
        let degree = 8;
        let circuit = Circuit {
            inputs: 3,
            gates: alloc::vec![Gate::And { a: 0, b: 1 }, Gate::Xor { a: 3, b: 2 }],
            outputs: alloc::vec![4],
        };
        let wires = circuit.wires();
        let mut rng = TestRng(0x0bad_c0de_1234_5678);
        let prf = PrfKey([0x1111, 0x2222, 0x3333, 0x4444]);

        let dd = LatticeDd::new(degree, p, q, &mut rng).expect("params");
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
        let delta_p = crate::bitgc::pinned::Delta {
            coeffs: dd.delta().clone(),
            p: dd.p(),
        };
        let encoded = encode_inputs(&garbling, &delta_p, &inputs, circuit.inputs).expect("encode");
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
        open_output(masked_out[0], garbling.labels[circuit.outputs[0]].pi)
    }

    #[test]
    fn lattice_small_circuit_truth_table() {
        let expect = |i: [bool; 3]| (i[0] & i[1]) ^ i[2];
        for x0 in [false, true] {
            for x1 in [false, true] {
                for x2 in [false, true] {
                    let inputs = [x0, x1, x2];
                    assert_eq!(
                        circuit_roundtrip(Variant::Small, inputs),
                        expect(inputs),
                        "{inputs:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn lattice_fast_circuit_truth_table() {
        let expect = |i: [bool; 3]| (i[0] & i[1]) ^ i[2];
        for x0 in [false, true] {
            for x1 in [false, true] {
                for x2 in [false, true] {
                    let inputs = [x0, x1, x2];
                    assert_eq!(
                        circuit_roundtrip(Variant::Fast, inputs),
                        expect(inputs),
                        "{inputs:?}"
                    );
                }
            }
        }
    }

    /// NTT-friendly test profile: p=97 (≡1 mod 16), q=97·17 (≡1 mod 16),
    /// degree 8 — both moduli admit a negacyclic NTT, exercising the
    /// fast path (`mul_q`/`mul_p` → NTT).
    fn ntt_dd() -> LatticeDd {
        let p = 97u64;
        let q = 97u64 * 17; // 1649 ≡ 1 mod 16
        let mut rng = TestRng(0xfeed_5678_9abc_def0);
        LatticeDd::new(8, p, q, &mut rng).expect("params")
    }

    #[test]
    fn ntt_path_is_taken() {
        // Both moduli NTT-friendly ⇒ both contexts present.
        let dd = ntt_dd();
        assert!(dd.ntt_q.is_some(), "q=1649 should be NTT-friendly for N=8");
        assert!(dd.ntt_p.is_some(), "p=97 should be NTT-friendly for N=8");
    }

    #[test]
    fn ntt_mul_matches_schoolbook() {
        // mul_q/mul_p (NTT path) must equal the schoolbook reference.
        let dd = ntt_dd();
        let n = dd.degree();
        let p = dd.p();
        let q = dd.q();
        let a: RpPoly = (0..n).map(|i| (i as u64 * 31 + 7) % p).collect();
        let b: RpPoly = (0..n).map(|i| (i as u64 * 17 + 3) % p).collect();
        assert_eq!(dd.mul_p(&a, &b), ring::negacyclic_mul_coeffs(&a, &b, p));
        let a_q: RqPoly = (0..n).map(|i| (i as u64 * 123 + 45) % q).collect();
        let b_q: RqPoly = (0..n).map(|i| (i as u64 * 87 + 11) % q).collect();
        assert_eq!(dd.mul_q(&a_q, &b_q), negacyclic_mul_rq(&a_q, &b_q, q));
    }

    #[test]
    fn ntt_dd_identities_exact() {
        // The DD identities hold under the NTT path too.
        let dd = ntt_dd();
        let p = dd.p();
        let mut rng = TestRng(0x1a2b_3c4d);
        let tau = DdGadget::row(&dd, 1, &mut rng);
        let a0 = sample_rp(dd.degree(), p, 0x5e6f);
        // DD_i identity via the seam (NTT-accelerated mod-p ops).
        let base = DdGadget::dd0(&dd, &a0, &tau);
        let a1 = add_delta(&a0, dd.delta(), p);
        let lhs = DdGadget::dd_i(&dd, &a1, true, &tau);
        // dec via a fresh lattice row for the same t=1: Dec = t·Δ = Δ.
        let expect = add_rp(&base, dd.delta(), p);
        assert_eq!(lhs, expect, "DD_1(A0+Δ, τ) = DD_0(A0, τ) + Δ");
    }

    #[test]
    fn eval_row_reconstruction() {
        // Eval_{i,j}(A0+iΔ, B0+jΔ) = Eval_{0,0}(A0,B0) + (i·t1 + j·t2 +
        // ij·t3)·Δ exactly — the garbled-row reconstruction property.
        let dd = test_dd();
        let p = dd.p();
        let mut rng = TestRng(0x9999);
        let t = [1i64, -1, 1];
        let taus = [
            dd.row(t[0], &mut rng),
            dd.row(t[1], &mut rng),
            dd.row(t[2], &mut rng),
        ];
        let a0 = sample_rp(dd.degree(), p, 0xaaaa);
        let b0 = sample_rp(dd.degree(), p, 0xbbbb);
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
            // Correction = (i·t1 + j·t2 + ij·t3)·Δ
            let k = (i as i64) * t[0] + (j as i64) * t[1] + ((i && j) as i64) * t[2];
            let corr: RpPoly = dd
                .delta()
                .iter()
                .map(|&c| {
                    let c = c as i128;
                    (c * k as i128).rem_euclid(p as i128) as u64
                })
                .collect();
            let expect = add_rp(&base, &corr, p);
            assert_eq!(got, expect, "i={i} j={j}");
        }
    }
}
