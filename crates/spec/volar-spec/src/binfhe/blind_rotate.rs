// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! GINX blind rotation.
//!
//! Given an LWE ciphertext `(a, b)` mod `q` (profile invariant `q = 2N`)
//! and a test polynomial `v(X)` over `Z_Q`, computes the RLWE encryption
//!
//! ```text
//! ACC = X^{-(b - <a, s>)} * v(X)
//! ```
//!
//! by initializing the accumulator with `X^{-b} * v(X)` and applying, for
//! each key bit `s_i`, `CMUX(RGSW(s_i), X^{a_i} * ACC, ACC)`. Because
//! `q = 2N`, the torus-to-exponent map is the exact identity on
//! `Z_{2N}` (no rounding term): a ciphertext whose phase is `phi mod q`
//! rotates by exactly `phi mod 2N` positions.
//!
//! Cost: one RGSW external product per non-zero mask component — the
//! dominant per-bootstrap cost, and the quantity the weaver's multi-value
//! and circuit-bootstrap optimizations amortize.

use crate::binfhe::lwe::LweCiphertext;
use crate::binfhe::rgsw::{RgswCiphertext, binfhe_rgsw_cmux};
use crate::binfhe::rlwe::{RlweCiphertext, binfhe_rlwe_rotate, binfhe_rlwe_trivial};

/// Exact exponent of a `Z_q` value in `Z_{2N}` (valid because `q = 2N`).
#[inline]
fn exponent<const LOG_Q_LWE: u32, const BIG_N: usize>(x: u32) -> usize {
    debug_assert_eq!(
        1usize << LOG_Q_LWE,
        2 * BIG_N,
        "profile invariant violated: q must equal 2N"
    );
    (x as usize) & (2 * BIG_N - 1)
}

/// Blind rotation with a caller-supplied test polynomial (already reduced
/// mod `2^LOG_Q`).
pub fn binfhe_blind_rotate<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
>(
    ct: &LweCiphertext<N_LWE>,
    test_poly: &[u32; BIG_N],
    bsk: &[RgswCiphertext<BIG_N, BS_ELL>],
) -> RlweCiphertext<BIG_N> {
    debug_assert_eq!(bsk.len(), N_LWE, "one RGSW row per LWE key bit");
    let two_n = 2 * BIG_N;

    // ACC = X^{-b} * v(X).
    let b_exp = exponent::<LOG_Q_LWE, BIG_N>(ct.b);
    let mut acc = binfhe_rlwe_trivial::<BIG_N, LOG_Q>(test_poly);
    if b_exp != 0 {
        acc = binfhe_rlwe_rotate::<BIG_N, LOG_Q>(&acc, two_n - b_exp);
    }

    for (i, row) in bsk.iter().enumerate() {
        let a_exp = exponent::<LOG_Q_LWE, BIG_N>(ct.a[i]);
        if a_exp != 0 {
            let rotated = binfhe_rlwe_rotate::<BIG_N, LOG_Q>(&acc, a_exp);
            acc = binfhe_rgsw_cmux::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG>(row, &rotated, &acc);
        }
    }
    acc
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::keys;
    use crate::binfhe::params::toy;
    use crate::binfhe::rlwe::RlweSecretKey;
    use crate::binfhe::lwe::{LweSecretKey, binfhe_gen_lwe_secret_key};
    use crate::SpecRng;

    struct TestRng(u64);
    impl TestRng {
        fn new(seed: u64) -> Self {
            Self(seed)
        }
    }
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            z = z ^ (z >> 31);
            z as u32
        }
    }

    /// Independent clear decryption: per-coefficient phase via a directly
    /// written negacyclic convolution (shares no code with rlwe.rs).
    fn clear_phase(ct: &RlweCiphertext<{ toy::BIG_N }>, key: &[u32; toy::BIG_N]) -> [u32; toy::BIG_N] {
        let mut phase = [0u32; toy::BIG_N];
        for i in 0..toy::BIG_N {
            let mut product = 0u32;
            for k in 0..toy::BIG_N {
                let idx = if i >= k { i - k } else { toy::BIG_N + i - k };
                let term = ct.a[k].wrapping_mul(key[idx]);
                product = if i >= k {
                    product.wrapping_add(term)
                } else {
                    product.wrapping_sub(term)
                };
            }
            phase[i] = ct.b[i].wrapping_sub(product) & 0xFF;
        }
        phase
    }

    /// Independent clear rotation model over `Z_{2^8}`.
    fn clear_rotate(poly: &[u32; toy::BIG_N], exp: usize) -> [u32; toy::BIG_N] {
        let n = toy::BIG_N;
        let base = exp % (2 * n);
        let mut out = [0u32; toy::BIG_N];
        for (i, &c) in poly.iter().enumerate() {
            let dest = i + base;
            let sign = (dest / n) % 2 == 1;
            let v = if sign { 256u32.wrapping_sub(c) & 0xFF } else { c };
            let slot = dest % n;
            out[slot] = (out[slot] + v) & 0xFF;
        }
        out
    }

    #[test]
    fn blind_rotation_decrypts_to_clear_rotation() {
        let mut rng = TestRng::new(0xB17D);
        let lwe_sk: LweSecretKey<{ toy::N_LWE }> = binfhe_gen_lwe_secret_key(&mut rng);
        let rlwe_sk: RlweSecretKey<{ toy::BIG_N }> =
            crate::binfhe::rlwe::binfhe_gen_rlwe_secret_key(&mut rng);
        let bk = keys::binfhe_gen_bootstrapping_key::<
            { toy::N_LWE },
            { toy::BIG_N },
            8,
            7,
            8,
            { toy::BS_ELL },
            { toy::BS_BASE_LOG },
            { toy::KS_ELL },
            { toy::KS_BASE_LOG },
            0,
            _,
        >(&lwe_sk, &rlwe_sk, &mut rng);

        // Arbitrary test polynomial content.
        let test_poly: [u32; toy::BIG_N] =
            core::array::from_fn(|i| (i as u32 * 13 + 5) & 0xFF);

        // Hand-crafted ciphertexts on the exact exponent grid (every value
        // of Z_128 is an exact exponent because q = 2N).
        for (seed, body_exp) in [(1u64, 0usize), (2, 1), (3, 37), (4, 64), (5, 127)] {
            let mut rng = TestRng::new(seed);
            let mut a = [0u32; toy::N_LWE];
            for ai in a.iter_mut() {
                *ai = rng.next_u32() & 0x7F;
            }
            let ct = LweCiphertext {
                a,
                b: (body_exp as u32) & 0x7F,
            };
            // Clear phase exponent: phi = b - <a, s> mod 2N.
            let mut phi = ct.b as i64;
            for i in 0..toy::N_LWE {
                phi -= (ct.a[i] as i64) * (lwe_sk.key[i] as i64);
            }
            let phi = phi.rem_euclid(128) as usize;

            let acc = binfhe_blind_rotate::<
                { toy::N_LWE },
                { toy::BIG_N },
                8,
                7,
                { toy::BS_ELL },
                { toy::BS_BASE_LOG },
            >(&ct, &test_poly, &bk.bsk);
            let phase = clear_phase(&acc, &rlwe_sk.key);
            // Expected: X^{-phi} * v(X), via the independent clear model.
            let expected = clear_rotate(&test_poly, 2 * toy::BIG_N - phi);
            assert_eq!(phase, expected, "blind rotation phase_exp={phi}");
        }
    }
}
