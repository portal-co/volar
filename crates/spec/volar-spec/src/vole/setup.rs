// @reliability: experimental
//! @ai: assisted
//! VOLE setup via Correlated-OT.
//!
//! Produces honest VOLE shares for input wires of a Quicksilver-style proof,
//! using the C-OT functionality from [`crate::ot::IdealCot`]. Replaces the
//! hand-fabricated setup (Δ=1, masks=0) used by earlier tests so that:
//!
//! - `Δ` is sampled uniformly from a non-zero subset of `T` (verifier-only),
//! - per-wire masks `v` are uniform random (prover never sees `Δ` directly),
//! - the standard VOLE relation `q_w = v_w + u_w · Δ` holds for every wire.
//!
//! # AND-gate output wires
//!
//! AND outputs do **not** get a fresh OT. The verifier derives `q_and`
//! analytically from the prover-sent `hat`:
//!
//! ```text
//! K_a · K_b + hat == K_and · Δ
//!   ⇒  K_and = (K_a · K_b + hat) · Δ⁻¹
//! ```
//!
//! This requires `T: Invert` and `Δ` non-zero per lane. See
//! [`derive_and_q`].

use core::ops::{Add, Mul};

use cipher::consts::U1;

use crate::{Array, ArraySize, SpecRng};
use crate::field::Invert;
use crate::ot::IdealCot;
use crate::vole::{Delta, Q, VoleArray, Vope};

/// Sample a `Δ` whose every lane is non-zero — required when the verifier
/// later derives an AND output share via [`derive_and_q`] (which inverts Δ).
///
/// Statistical distance from uniform over `T` per lane is ≤ Pr(zero); for
/// `T = Galois` (GF(2^8)) this is 1/256.
pub fn random_nonzero_delta<N, T, R>(
    rng: &mut R,
    sample_t: impl Fn(&mut R) -> T,
    is_zero: impl Fn(&T) -> bool,
) -> Delta<N, T>
where
    N: ArraySize,
    R: SpecRng,
{
    Delta {
        delta: Array::<T, N>::from_fn(|_| {
            let mut x = sample_t(rng);
            // Bounded retries: with Galois the chance of repeated zeros is
            // negligible, but be safe against pathological samplers.
            let mut tries = 0usize;
            while is_zero(&x) && tries < 64 {
                x = sample_t(rng);
                tries += 1;
            }
            x
        }),
    }
}

/// Lift a `bool` to a uniform-shape `Array<T, N>` (broadcast).
fn lift_bit<N: ArraySize, T: Clone>(bit_t: T) -> Array<T, N> {
    Array::<T, N>::from_fn(|_| bit_t.clone())
}

/// Commit a single bit to a fresh VOLE wire via one C-OT call.
///
/// Returns `(prover_vope, verifier_q)`. In a real protocol the prover and
/// verifier would each see only one half; this single-process API is used
/// to drive the ideal functionality from tests.
pub fn vole_commit_bit<N, T, R>(
    cot: &IdealCot<N, T>,
    rng: &mut R,
    sample_t: impl Fn(&mut R) -> T,
    bit_to_t: impl Fn(bool) -> T,
    bit: bool,
) -> (Vope<N, T, U1>, Q<N, T>)
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
    R: SpecRng,
{
    // Sender's row r0 doubles as the verifier's q (q = r0).
    // Receiver's v = r0 + b·Δ. Then q = v + b·Δ ⇒ q = v + u·Δ. ✓
    let (r0, v) = cot.cot(rng, sample_t, bit);

    let u_t = bit_to_t(bit);
    let u_row: Array<T, N> = lift_bit(u_t);
    let u: Array<Array<T, N>, U1> = Array::<Array<T, N>, U1>::from_fn(|_| {
        Array::<T, N>::from_fn(|i| u_row[i].clone())
    });
    let q = Array::<T, N>::from_fn(|i| r0[i].clone());
    (Vope { u, v }, Q { q })
}

/// Verifier-side derivation of the AND output share.
///
/// Computes `q_and` such that the Quicksilver constraint
/// `K_a · K_b + hat == K_and · Δ` holds element-wise:
///
/// ```text
/// q_and[i] = (q_a[i] · q_b[i] + hat[i]) · Δ[i]⁻¹
/// ```
///
/// Requires every lane of Δ to be non-zero (use [`random_nonzero_delta`]).
pub fn derive_and_q<N, T>(
    delta: &Delta<N, T>,
    q_a: &Q<N, T>,
    q_b: &Q<N, T>,
    hat: &Array<T, N>,
) -> Q<N, T>
where
    N: ArraySize,
    T: Clone + Add<Output = T> + Mul<Output = T> + Invert + Default,
{
    Q {
        q: Array::<T, N>::from_fn(|i| {
            let lhs = q_a.q[i].clone() * q_b.q[i].clone() + hat[i].clone();
            lhs * delta.delta[i].invert()
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::Galois;
    use crate::ot::IdealCot;
    use crate::vole::prove::{vole_and_prover_step, vole_and_verifier_check};

    /// SplitMix64-based deterministic test RNG.
    struct TestRng(u64);
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            (z ^ (z >> 31)) as u32
        }
    }

    type N16 = cipher::consts::U16;

    fn sample_g<R: SpecRng>(r: &mut R) -> Galois {
        Galois(r.next_u8())
    }
    fn lift_bit_g(b: bool) -> Galois {
        Galois(if b { 1 } else { 0 })
    }
    fn is_zero_g(g: &Galois) -> bool {
        g.0 == 0
    }

    fn make_cot(rng: &mut TestRng) -> IdealCot<N16, Galois> {
        let delta = random_nonzero_delta::<N16, Galois, _>(rng, sample_g, is_zero_g);
        IdealCot::new(delta)
    }

    /// q_w == v_w + u_w · Δ for every lane.
    #[test]
    fn commit_bit_relation_holds() {
        let mut rng = TestRng(0x0123_4567_89AB_CDEF);
        let cot = make_cot(&mut rng);

        for &bit in &[false, true, false, true] {
            let (vope, q) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, bit);
            for i in 0..16 {
                let expected = vope.v[i] + vope.u[0][i] * cot.delta.delta[i];
                assert_eq!(q.q[i], expected, "lane {i}, bit {bit}: VOLE relation broken");
            }
        }
    }

    /// Full Quicksilver loop: setup wires for an AND gate, run prover step,
    /// derive verifier's q_and, and check.
    #[test]
    fn quicksilver_and_gate_round_trip() {
        let mut rng = TestRng(0xFEED_FACE_DEAD_BEEF);
        let cot = make_cot(&mut rng);

        for &(a, b) in &[(false, false), (false, true), (true, false), (true, true)] {
            let (vope_a, q_a) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, a);
            let (vope_b, q_b) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, b);

            let (_vope_c, hat) = vole_and_prover_step(vope_a, vope_b);
            let q_and = derive_and_q(&cot.delta, &q_a, &q_b, &hat);

            let (_q_out, ok) = vole_and_verifier_check(&cot.delta, &q_a, &q_b, &q_and, &hat);
            assert!(ok, "honest prover rejected for (a={a}, b={b})");
        }
    }

    /// Tampering with `hat` after the prover step must cause rejection.
    #[test]
    fn tampered_hat_rejected() {
        let mut rng = TestRng(0xAAAA_BBBB_CCCC_DDDD);
        let cot = make_cot(&mut rng);

        let (vope_a, q_a) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);
        let (vope_b, q_b) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);

        let (_vope_c, hat) = vole_and_prover_step(vope_a, vope_b);
        let q_and = derive_and_q(&cot.delta, &q_a, &q_b, &hat);

        let mut bad_hat = Array::<Galois, N16>::from_fn(|i| hat[i]);
        bad_hat[0] = bad_hat[0] + Galois(1); // flip one lane

        let (_q_out, ok) = vole_and_verifier_check(&cot.delta, &q_a, &q_b, &q_and, &bad_hat);
        assert!(!ok, "verifier accepted tampered hat");
    }

    /// Tampering with `q_and` after derivation must cause rejection.
    #[test]
    fn tampered_q_and_rejected() {
        let mut rng = TestRng(0x1111_2222_3333_4444);
        let cot = make_cot(&mut rng);

        let (vope_a, q_a) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);
        let (vope_b, q_b) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, false);

        let (_vope_c, hat) = vole_and_prover_step(vope_a, vope_b);
        let mut q_and = derive_and_q(&cot.delta, &q_a, &q_b, &hat);
        q_and.q[0] = q_and.q[0] + Galois(1);

        let (_q_out, ok) = vole_and_verifier_check(&cot.delta, &q_a, &q_b, &q_and, &hat);
        assert!(!ok, "verifier accepted tampered q_and");
    }

    /// Stress: 64 random (a,b) trials must all verify with random Δ each round.
    #[test]
    fn quicksilver_random_stress() {
        let mut rng = TestRng(0xC0FFEE_C0FFEE);
        for trial in 0..64u32 {
            let cot = make_cot(&mut rng);
            let a = (rng.next_u32() & 1) == 1;
            let b = (rng.next_u32() & 1) == 1;
            let (vope_a, q_a) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, a);
            let (vope_b, q_b) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, b);
            let (_, hat) = vole_and_prover_step(vope_a, vope_b);
            let q_and = derive_and_q(&cot.delta, &q_a, &q_b, &hat);
            let (_, ok) = vole_and_verifier_check(&cot.delta, &q_a, &q_b, &q_and, &hat);
            assert!(ok, "trial {trial} failed for (a={a}, b={b})");
        }
    }
}
