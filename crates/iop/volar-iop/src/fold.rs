// @reliability: experimental
// @ai: assisted
//! **Phase 1**: the per-gate fold — O(1) memory: a fixed-size accumulator
//! `(W, E, u)` threaded through the woven verifier's loop and updated once
//! per AND gate via Nova-style relaxed-R1CS cross-term folding algebra
//! (same shape as `volar_fold::nifs`'s `cross_term_z`/fold formulas), but
//! **native** to this crate's `GF(2^k)` tower field ([`crate::field`])
//! rather than an unrelated prime field. See `docs/prove-the-verifier-iop.md`
//! for the full design and the honest caveat on per-gate Fiat–Shamir
//! soundness.
//!
//! Because there is no cross-field cast here, the target relation is
//! `and_check_r1cs`'s original 3-constraint/8-variable shape, used
//! directly — no bit-expansion gadget of the kind a cross-field embedding
//! would otherwise require. This module is a deliberate, standalone
//! reimplementation (not a shared generic with `volar-fold`) — kept
//! separate so `volar-fold`'s own `Scalar`/`EdPoint`/Pedersen types never
//! need genericizing.

use alloc::vec;
use alloc::vec::Vec;

use crate::field::Field;

/// Sparse R1CS over a generic tower field `F` — same shape as
/// `volar_fold::r1cs::R1CS`, just field-generic.
#[derive(Clone)]
pub struct R1CS<F: Field> {
    pub num_cons: usize,
    /// Total columns of `z` (`= num_vars_w + 1`; the last column is `u`).
    pub num_vars: usize,
    pub a: Vec<(usize, usize, F)>,
    pub b: Vec<(usize, usize, F)>,
    pub c: Vec<(usize, usize, F)>,
}

fn matvec<F: Field>(mat: &[(usize, usize, F)], z: &[F], num_cons: usize) -> Vec<F> {
    let mut out = vec![F::ZERO; num_cons];
    for (row, col, val) in mat {
        out[*row] = out[*row].add(&val.mul(&z[*col]));
    }
    out
}

impl<F: Field> R1CS<F> {
    /// `z = [w ‖ u]`.
    pub fn full_z(&self, w: &[F], u: &F) -> Vec<F> {
        let mut z = w.to_vec();
        z.push(*u);
        z
    }
    pub fn eval_abc(&self, z: &[F]) -> (Vec<F>, Vec<F>, Vec<F>) {
        (
            matvec(&self.a, z, self.num_cons),
            matvec(&self.b, z, self.num_cons),
            matvec(&self.c, z, self.num_cons),
        )
    }
    /// Relaxed satisfaction: `(Az)∘(Bz) = u·(Cz) + E`.
    pub fn is_satisfied_relaxed(&self, w: &[F], e: &[F], u: &F) -> bool {
        let z = self.full_z(w, u);
        let (az, bz, cz) = self.eval_abc(&z);
        (0..self.num_cons).all(|i| az[i].mul(&bz[i]) == u.mul(&cz[i]).add(&e[i]))
    }
}

/// Column indices for [`and_check_r1cs`]'s 7-witness-slot layout.
pub const K_A: usize = 0;
pub const K_B: usize = 1;
pub const K_C: usize = 2;
pub const DELTA: usize = 3;
pub const V_HAT: usize = 4;
pub const P1: usize = 5;
pub const P2: usize = 6;
const U_COL: usize = 7;

/// The per-gate verifier check `K_a·K_b + V̂ = K_c·Δ`, native to `F` — no
/// `GF(2^k)→F_ℓ` cast, so no bit-expansion is needed to keep it sound.
pub fn and_check_r1cs<F: Field>() -> R1CS<F> {
    let one = F::ONE;
    let neg_one = one.neg();
    R1CS {
        num_cons: 3,
        num_vars: 8,
        a: vec![(0, K_A, one), (1, K_C, one), (2, P1, one), (2, V_HAT, one), (2, P2, neg_one)],
        b: vec![(0, K_B, one), (1, DELTA, one), (2, U_COL, one)],
        c: vec![(0, P1, one), (1, P2, one)],
    }
}

/// Build the 7-slot satisfying witness `[K_a,K_b,K_c,Δ,V̂,P1,P2]` for one
/// gate — the field-generic analogue of `volar_spec::fold::gate_witness`.
pub fn gate_witness<F: Field>(k_a: F, k_b: F, k_c: F, delta: F, v_hat: F) -> Vec<F> {
    let p1 = k_a.mul(&k_b);
    let p2 = k_c.mul(&delta);
    vec![k_a, k_b, k_c, delta, v_hat, p1, p2]
}

/// Cross term `T` (length `num_cons`) — the field-generic analogue of
/// `volar_fold::nifs::cross_term_z`.
pub fn cross_term_z<F: Field>(r1cs: &R1CS<F>, w1: &[F], u1: &F, w2: &[F], u2: &F) -> Vec<F> {
    let z1 = r1cs.full_z(w1, u1);
    let z2 = r1cs.full_z(w2, u2);
    let (az1, bz1, cz1) = r1cs.eval_abc(&z1);
    let (az2, bz2, cz2) = r1cs.eval_abc(&z2);
    (0..r1cs.num_cons)
        .map(|i| {
            let cross = az1[i].mul(&bz2[i]).add(&az2[i].mul(&bz1[i]));
            let sub = u1.mul(&cz2[i]).add(&u2.mul(&cz1[i]));
            cross.sub(&sub)
        })
        .collect()
}

/// The threaded fold-accumulator state: `None` until the first gate is
/// folded in, then the running `(W, E, u)` — genuinely fixed-size
/// (`W`: 7 slots, `E`: 3 slots) regardless of how many gates have been
/// folded.
#[derive(Clone)]
pub struct IopAccumulator<F: Field> {
    inner: Option<(Vec<F>, Vec<F>, F)>,
}

impl<F: Field> Default for IopAccumulator<F> {
    fn default() -> Self {
        IopAccumulator { inner: None }
    }
}

impl<F: Field> IopAccumulator<F> {
    pub fn fresh() -> Self {
        Self::default()
    }
    pub fn witness(&self) -> Option<(&[F], &[F], &F)> {
        self.inner.as_ref().map(|(w, e, u)| (w.as_slice(), e.as_slice(), u))
    }
}

/// Fold one AND gate's native-field values into `state` via the fixed
/// `and_check_r1cs` shape — no bit-expansion, no `GF(2^k)→F_ℓ` lift needed.
/// `r` is this gate's fold challenge (see the module doc's pointer to the
/// per-gate Fiat–Shamir honest-scope note).
pub fn fold_gate<F: Field>(state: IopAccumulator<F>, k_a: F, k_b: F, k_c: F, delta: F, v_hat: F, r: F) -> IopAccumulator<F> {
    let r1cs = and_check_r1cs::<F>();
    let gate_w = gate_witness(k_a, k_b, k_c, delta, v_hat);
    match state.inner {
        None => IopAccumulator { inner: Some((gate_w, vec![F::ZERO; r1cs.num_cons], F::ONE)) },
        Some((w1, e1, u1)) => {
            let t = cross_term_z(&r1cs, &w1, &u1, &gate_w, &F::ONE);
            let w: Vec<F> = w1.iter().zip(gate_w.iter()).map(|(x, y)| x.add(&r.mul(y))).collect();
            // The incoming gate is fresh (u2 = 1, E2 = 0), so its r² term
            // vanishes.
            let e: Vec<F> = e1.iter().zip(t.iter()).map(|(x, ti)| x.add(&r.mul(ti))).collect();
            let u = u1.add(&r);
            IopAccumulator { inner: Some((w, e, u)) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::Gf128;

    fn honest_gate(a: u8, b: u8, c: u8, d: u8) -> (Gf128, Gf128, Gf128, Gf128, Gf128) {
        // Build small Gf128 values embedding a byte in the lowest limb, and
        // pick v_hat so the check holds: K_a·K_b + V̂ = K_c·Δ.
        use crate::field::{Ext, Field as _};
        fn lift(byte: u8) -> Gf128 {
            // Embed a GF(2^8) byte as a Gf128 element with all-zero higher limbs.
            let g = volar_primitives::Galois(byte);
            let g16 = Ext::new(g, volar_primitives::Galois::ZERO);
            let g32 = Ext::new(g16, crate::field::Gf16::ZERO);
            let g64 = Ext::new(g32, crate::field::Gf32::ZERO);
            Ext::new(g64, crate::field::Gf64::ZERO)
        }
        let (ka, kb, kc, delta) = (lift(a), lift(b), lift(c), lift(d));
        let v_hat = kc.mul(&delta).sub(&ka.mul(&kb));
        (ka, kb, kc, delta, v_hat)
    }

    #[test]
    fn honest_gate_witness_satisfies_and_check_r1cs() {
        let (ka, kb, kc, delta, v_hat) = honest_gate(3, 5, 7, 11);
        let r1cs = and_check_r1cs::<Gf128>();
        let w = gate_witness(ka, kb, kc, delta, v_hat);
        let e = vec![Gf128::ZERO; r1cs.num_cons];
        assert!(r1cs.is_satisfied_relaxed(&w, &e, &Gf128::ONE));
    }

    #[test]
    fn tampered_gate_witness_fails_and_check_r1cs() {
        let (ka, kb, kc, delta, mut v_hat) = honest_gate(3, 5, 7, 11);
        v_hat = v_hat.add(&Gf128::ONE);
        let r1cs = and_check_r1cs::<Gf128>();
        let w = gate_witness(ka, kb, kc, delta, v_hat);
        let e = vec![Gf128::ZERO; r1cs.num_cons];
        assert!(!r1cs.is_satisfied_relaxed(&w, &e, &Gf128::ONE));
    }

    #[test]
    fn single_gate_fold_is_relaxed_satisfied() {
        use crate::transcript::FromBytes as _;
        let (ka, kb, kc, delta, v_hat) = honest_gate(2, 3, 4, 5);
        let r = Gf128::from_u64(0xabcd); // arbitrary nonzero challenge
        let state = fold_gate(IopAccumulator::fresh(), ka, kb, kc, delta, v_hat, r);
        let (w, e, u) = state.witness().expect("folded after one gate");
        assert_eq!(*u, Gf128::ONE, "fresh instance has u = 1");
        let r1cs = and_check_r1cs::<Gf128>();
        assert!(r1cs.is_satisfied_relaxed(w, e, u));
    }

    #[test]
    fn multi_gate_fold_chain_is_relaxed_satisfied() {
        use crate::transcript::FromBytes as _;
        let mut state = IopAccumulator::fresh();
        let r1cs = and_check_r1cs::<Gf128>();
        for i in 1u8..=5 {
            let (ka, kb, kc, delta, v_hat) = honest_gate(i, i + 1, i + 2, i + 3);
            let r = Gf128::from_u64(i as u64);
            state = fold_gate(state, ka, kb, kc, delta, v_hat, r);
        }
        let (w, e, u) = state.witness().expect("folded after five gates");
        assert!(r1cs.is_satisfied_relaxed(w, e, u), "chain of honest gates must stay relaxed-satisfied");
    }

    #[test]
    fn dishonest_gate_breaks_the_fold() {
        use crate::transcript::FromBytes as _;
        let mut state = IopAccumulator::fresh();
        let r1cs = and_check_r1cs::<Gf128>();
        for (idx, i) in (1u8..=5).enumerate() {
            let (ka, kb, kc, delta, mut v_hat) = honest_gate(i, i + 1, i + 2, i + 3);
            if idx == 2 {
                v_hat = v_hat.add(&Gf128::ONE); // tamper the third gate
            }
            let r = Gf128::from_u64(i as u64);
            state = fold_gate(state, ka, kb, kc, delta, v_hat, r);
        }
        let (w, e, u) = state.witness().expect("folded after five gates");
        assert!(!r1cs.is_satisfied_relaxed(w, e, u), "a tampered gate must break the fold");
    }
}
