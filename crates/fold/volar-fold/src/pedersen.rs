// @reliability: experimental
// @ai: assisted
//! Additively-homomorphic **Pedersen vector commitment** over Ed25519,
//! `Commit(x⃗; ρ) = Σ_i x_i·G_i + ρ·H`, used to commit the folding witness and
//! the boundary state.
//!
//! ## Binding
//! The generators `{G_i, H}` are derived by **hash-to-curve**
//! ([`volar_spec::curve::hash_to_curve`], try-and-increment, SHA3-256 RO), so
//! their pairwise discrete-log relations are unknown in the ROM — the commitment
//! is binding.  (Earlier scaffold versions derived `G_i = [s_i]·B` with known
//! `s_i`, which was homomorphic but *not* binding; that is fixed here.)  The
//! `seed` only domain-separates independent generator sets.

use alloc::vec::Vec;

use volar_spec::curve::{ed_add, ed_double, ed_scalar_mul, hash_to_curve, EdPoint};

use crate::scalar::Scalar;

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

/// Extract the `width`-bit window of a little-endian scalar at bit offset `off`.
fn scalar_window(limbs: &[u64; 4], off: usize, width: usize) -> u64 {
    if off >= 256 {
        return 0;
    }
    let limb = off / 64;
    let bit = off % 64;
    let lo = limbs[limb] >> bit;
    let hi = if bit == 0 || limb + 1 >= 4 { 0 } else { limbs[limb + 1] << (64 - bit) };
    (lo | hi) & ((1u64 << width) - 1)
}

/// Multi-scalar multiplication `Σ_i scalars[i]·points[i]` by the Pippenger
/// bucket method — `O(n)` point adds per window instead of a 256-doubling
/// scalar-mul per point.  This is the folding hot path (committing the witness).
pub fn msm(points: &[EdPoint], scalars: &[Scalar]) -> EdPoint {
    assert_eq!(points.len(), scalars.len());
    if points.is_empty() {
        return EdPoint::IDENTITY;
    }
    const C: usize = 8; // window width
    const NUM_WINDOWS: usize = 256 / C; // 32
    let mut acc = EdPoint::IDENTITY;
    for w in (0..NUM_WINDOWS).rev() {
        // acc <<= C  (C doublings)
        for _ in 0..C {
            acc = ed_double(&acc);
        }
        // Accumulate points into 2^C − 1 buckets by their window digit.
        let mut buckets = alloc::vec![EdPoint::IDENTITY; (1 << C) - 1];
        for (p, s) in points.iter().zip(scalars.iter()) {
            let digit = scalar_window(&s.0, w * C, C) as usize;
            if digit != 0 {
                buckets[digit - 1] = ed_add(&buckets[digit - 1], p);
            }
        }
        // window_sum = Σ_d d·bucket[d] via running-sum trick.
        let mut running = EdPoint::IDENTITY;
        let mut window_sum = EdPoint::IDENTITY;
        for b in (0..buckets.len()).rev() {
            running = ed_add(&running, &buckets[b]);
            window_sum = ed_add(&window_sum, &running);
        }
        acc = ed_add(&acc, &window_sum);
    }
    acc
}

impl PedersenParams {
    /// Derive `n` message generators + a blinding generator by hash-to-curve.
    /// `seed` domain-separates independent generator sets.  Binding in the ROM.
    pub fn setup(n: usize, seed: u64) -> PedersenParams {
        let mut g = Vec::with_capacity(n);
        for i in 0..n {
            // Domain-separate generator index from the blinding generator.
            g.push(hash_to_curve(b"volar-fold/pedersen/G", seed ^ (i as u64).wrapping_shl(1)));
        }
        let h = hash_to_curve(b"volar-fold/pedersen/H", seed);
        PedersenParams { g, h }
    }

    /// `Σ_i x_i·G_i + ρ·H` via Pippenger MSM (the blinder term is folded in as the
    /// extra pair `(H, ρ)`).  Requires `x.len() <= self.g.len()`.
    pub fn commit(&self, x: &[Scalar], blind: &Scalar) -> EdPoint {
        assert!(x.len() <= self.g.len(), "pedersen: message longer than generators");
        let mut points: Vec<EdPoint> = self.g[..x.len()].to_vec();
        points.push(self.h);
        let mut scalars: Vec<Scalar> = x.to_vec();
        scalars.push(*blind);
        msm(&points, &scalars)
    }
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
    fn msm_matches_naive_sum() {
        let pts: Vec<EdPoint> = (0..6u64).map(|i| volar_spec::curve::hash_to_curve(b"msm/test", i)).collect();
        let scs: Vec<Scalar> = [3u64, 0, 1, 0xdead_beef, 255, 256]
            .iter()
            .map(|&v| Scalar::from_u64(v))
            .collect();
        // naive Σ scalar_mul
        let mut naive = EdPoint::IDENTITY;
        for (p, s) in pts.iter().zip(scs.iter()) {
            naive = ed_add(&naive, &scalar_mul(p, s));
        }
        assert_eq!(msm(&pts, &scs), naive, "Pippenger MSM disagrees with naive sum");
    }

    #[test]
    fn distinct_generators() {
        let p = PedersenParams::setup(3, 1);
        assert_ne!(p.g[0], p.g[1]);
        assert_ne!(p.g[1], p.g[2]);
        assert_ne!(p.g[0], p.h);
    }
}
