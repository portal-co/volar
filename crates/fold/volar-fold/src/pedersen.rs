// @reliability: experimental
// @ai: assisted
//! Additively-homomorphic **Pedersen vector commitment** over Ed25519,
//! `Commit(x⃗; ρ) = Σ_i x_i·G_i + ρ·H`, used to commit the folding witness and
//! the boundary state.
//!
//! ## SOUNDNESS CAVEAT (must be discharged)
//! For binding, the generators `{G_i, H}` must have **unknown pairwise
//! discrete-log relations** — i.e. be derived by a *hash-to-curve* (random
//! oracle), not by scalar-multiplying a known base.  `volar_spec::curve` does not
//! yet provide hash-to-curve, so [`PedersenParams::setup`] currently derives
//! `G_i = [s_i]·B` with **known** `s_i`.  The commitment is then correctly
//! *homomorphic* (folding works, tests pass) but **not binding** — a malicious
//! prover who knows the `s_i` can open a commitment two ways.  Replace the
//! derivation with hash-to-curve before relying on soundness (tracked alongside
//! the `link::BoundaryLink` embedding as an isolated crypto task).

use alloc::vec::Vec;

use volar_spec::curve::{ed_add, ed_scalar_mul, EdPoint};

use crate::scalar::Scalar;

/// Index used to derive the blinding generator `H`.
const H_INDEX: u64 = 0xFFFF_FFFF_FFFF_FFFF;

/// Public Pedersen generators.
#[derive(Clone)]
pub struct PedersenParams {
    /// Message generators `G_0..G_{n-1}`.
    pub g: Vec<EdPoint>,
    /// Blinding generator `H`.
    pub h: EdPoint,
}

/// `[k]·P` with a `Scalar` exponent.
pub fn scalar_mul(p: &EdPoint, k: &Scalar) -> EdPoint {
    ed_scalar_mul(p, &k.to_bytes_le())
}

impl PedersenParams {
    /// Derive `n` message generators + a blinding generator from a public seed.
    ///
    /// **Not binding** until hash-to-curve is available (see the module caveat).
    pub fn setup(n: usize, seed: u64) -> PedersenParams {
        let base = EdPoint::base();
        let mut g = Vec::with_capacity(n);
        for i in 0..n {
            let s = derive_scalar(seed, i as u64 + 1);
            g.push(ed_scalar_mul(&base, &s.to_bytes_le()));
        }
        let h = ed_scalar_mul(&base, &derive_scalar(seed, H_INDEX).to_bytes_le());
        PedersenParams { g, h }
    }

    /// `Σ_i x_i·G_i + ρ·H`.  Requires `x.len() <= self.g.len()`.
    pub fn commit(&self, x: &[Scalar], blind: &Scalar) -> EdPoint {
        assert!(x.len() <= self.g.len(), "pedersen: message longer than generators");
        let mut acc = scalar_mul(&self.h, blind);
        for (xi, gi) in x.iter().zip(self.g.iter()) {
            acc = ed_add(&acc, &scalar_mul(gi, xi));
        }
        acc
    }
}

/// A nothing-up-my-sleeve scalar from `(seed, idx)`.  Placeholder (not a random
/// oracle) — see the module caveat.
fn derive_scalar(seed: u64, idx: u64) -> Scalar {
    let a = seed
        .wrapping_mul(0x9E37_79B9_7F4A_7C15)
        .wrapping_add(idx.wrapping_mul(0xBF58_476D_1CE4_E5B9))
        .wrapping_add(1);
    let b = idx
        .wrapping_mul(0x94D0_49BB_1331_11EB)
        .wrapping_add(seed)
        .wrapping_add(0x1234_5678_9abc_def1);
    Scalar::from_limbs([a, b, idx.wrapping_add(1), seed | 1])
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use alloc::vec::Vec;

    #[test]
    fn commit_is_additively_homomorphic() {
        let p = PedersenParams::setup(3, 42);
        let a = [Scalar::from_u64(3), Scalar::from_u64(5), Scalar::from_u64(7)];
        let b = [Scalar::from_u64(11), Scalar::from_u64(13), Scalar::from_u64(17)];
        let ra = Scalar::from_u64(99);
        let rb = Scalar::from_u64(123);

        let ca = p.commit(&a, &ra);
        let cb = p.commit(&b, &rb);
        let sum: Vec<Scalar> = a.iter().zip(b.iter()).map(|(x, y)| x.add(y)).collect();
        let csum = p.commit(&sum, &ra.add(&rb));
        assert_eq!(ed_add(&ca, &cb), csum);
    }

    #[test]
    fn commit_scales_homomorphically() {
        let p = PedersenParams::setup(2, 7);
        let a = [Scalar::from_u64(4), Scalar::from_u64(9)];
        let r = Scalar::from_u64(5);
        let k = Scalar::from_u64(6);
        let lhs = scalar_mul(&p.commit(&a, &r), &k);
        let scaled: Vec<Scalar> = a.iter().map(|x| x.mul(&k)).collect();
        let rhs = p.commit(&scaled, &r.mul(&k));
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn distinct_generators() {
        let p = PedersenParams::setup(3, 1);
        assert_ne!(p.g[0], p.g[1]);
        assert_ne!(p.g[1], p.g[2]);
        assert_ne!(p.g[0], p.h);
    }
}
