// @reliability: experimental
//! @ai: assisted
//! Ideal Correlated-OT (C-OT) functionality.
//!
//! # Functionality
//!
//! C-OT lets a sender (holding a global secret `Δ`) and a receiver (holding
//! a choice bit `b`) jointly produce:
//! - sender output `r0` — a fresh uniformly-random row in `Array<T, N>`,
//! - receiver output `v = r0 + b·Δ`.
//!
//! In characteristic 2 (e.g. `T = Galois`, GF(2^8)), `+` is XOR.
//!
//! # Use in VOLE setup
//!
//! Setting `q_w = r0` and `v_w = r0 + b·Δ` gives the standard VOLE relation
//! `q_w = v_w + u_w · Δ` (with `u_w = b` lifted to `T`):
//!
//! ```text
//! v_w + u_w·Δ = (r0 + b·Δ) + b·Δ = r0    (= q_w)   in char 2
//! ```
//!
//! # Security caveat
//!
//! [`IdealCot`] holds Δ in a single struct that exposes both sender and
//! receiver entry points. It models the **ideal functionality** for testing
//! and is unsafe to expose to an untrusted prover. A real C-OT requires a
//! transport channel and an OT-extension protocol (see crate-level docs).

use core::ops::{Add, Mul};

use crate::{Array, ArraySize, SpecRng};
use crate::vole::{Delta, VoleArray};

/// Ideal Correlated-OT functionality.
///
/// Holds the verifier's global secret `Δ`. Each call to [`IdealCot::cot`]
/// runs one C-OT instance.
pub struct IdealCot<N: ArraySize, T> {
    /// Global secret held by the C-OT sender (= verifier in VOLE setup).
    pub delta: Delta<N, T>,
}

impl<N: VoleArray<T>, T> IdealCot<N, T>
where
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
{
    /// Construct an ideal C-OT instance with the given global secret.
    pub fn new(delta: Delta<N, T>) -> Self {
        Self { delta }
    }

    /// Run a single C-OT instance.
    ///
    /// Sampling `T` is delegated to `sample_t` so this works over any field
    /// type (callers pass e.g. `|r| Galois(r.next_u8())`).
    ///
    /// Returns `(r0, v)`:
    /// - `r0` is the sender's output (a uniform fresh row).
    /// - `v = r0 + b·Δ` is the receiver's output.
    pub fn cot<R: SpecRng>(
        &self,
        rng: &mut R,
        sample_t: impl Fn(&mut R) -> T,
        b: bool,
    ) -> (Array<T, N>, Array<T, N>) {
        let r0 = Array::<T, N>::from_fn(|_| sample_t(rng));
        let v = if b {
            Array::<T, N>::from_fn(|i| r0[i].clone() + self.delta.delta[i].clone())
        } else {
            Array::<T, N>::from_fn(|i| r0[i].clone())
        };
        (r0, v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::Galois;
    use crate::vole::Delta;

    /// Tiny xorshift-style PRNG so tests don't depend on `rand`.
    struct TestRng(u64);
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            // SplitMix64 step.
            self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            (z ^ (z >> 31)) as u32
        }
    }

    fn sample_galois<R: SpecRng>(r: &mut R) -> Galois {
        Galois(r.next_u8())
    }

    type N16 = cipher::consts::U16;

    fn random_delta<R: SpecRng>(r: &mut R) -> Delta<N16, Galois> {
        Delta {
            delta: Array::<Galois, N16>::from_fn(|_| {
                // avoid zero so callers that need invertible Δ can reuse this
                let mut x = r.next_u8();
                if x == 0 { x = 1; }
                Galois(x)
            }),
        }
    }

    #[test]
    fn cot_b_false_returns_r0() {
        let mut rng = TestRng(0xDEAD_BEEF);
        let cot = IdealCot::<N16, Galois>::new(random_delta(&mut rng));
        let (r0, v) = cot.cot(&mut rng, sample_galois, false);
        assert_eq!(r0, v, "v must equal r0 when choice bit is 0");
    }

    #[test]
    fn cot_b_true_returns_r0_plus_delta() {
        let mut rng = TestRng(0xCAFE_F00D);
        let cot = IdealCot::<N16, Galois>::new(random_delta(&mut rng));
        let (r0, v) = cot.cot(&mut rng, sample_galois, true);
        for i in 0..16 {
            assert_eq!(
                v[i], r0[i] + cot.delta.delta[i],
                "lane {i}: v should equal r0 + Δ"
            );
        }
    }

    #[test]
    fn cot_yields_fresh_rows() {
        // Two consecutive calls with the same choice bit must produce distinct
        // r0 (with overwhelming probability) — confirms sampling consumes rng.
        let mut rng = TestRng(0x1234_5678);
        let cot = IdealCot::<N16, Galois>::new(random_delta(&mut rng));
        let (r0_a, _) = cot.cot(&mut rng, sample_galois, false);
        let (r0_b, _) = cot.cot(&mut rng, sample_galois, false);
        assert_ne!(r0_a, r0_b);
    }
}
