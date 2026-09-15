// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! RGSW ciphertexts, the external product, and the RLWE-level CMUX.
//!
//! Representation (GINX form): an RGSW encryption of a bit `m` under ring
//! key `s` is a list of `ELL` rows; row `j` with gadget factor
//! `g_j = 2^shift_j` (see [`gadget`]) is a pair of RLWE ciphertexts
//!
//! ```text
//! rlwe0_j: phase = -m * g_j * s(X)   (gadget factor in the a-column)
//! rlwe1_j: phase =  m * g_j          (gadget factor in the b-column)
//! ```
//!
//! The external product `RGSW(m) ⊡ ct` decomposes both polynomials of `ct`
//! with the gadget and returns `sum_j a_j * row0_j + b_j * row1_j`, whose
//! phase is `m * phase(ct)` plus RGSW noise:
//!
//! ```text
//! sum_j a_j * (-m g_j s) + b_j * (m g_j)
//!   = m * (sum_j b_j g_j - s * sum_j a_j g_j)
//!   = m * (b - a * s)            (exact covering decomposition)
//! ```

use crate::SpecRng;
use crate::binfhe::gadget;
use crate::binfhe::rlwe::{
    RlweCiphertext, RlweSecretKey, poly_mul_neg, rlwe_add, rlwe_encrypt_scalar, rlwe_sub,
};
use crate::binfhe::torus;

/// One RGSW row: gadget factor in the a-column (`rlwe0`) and b-column
/// (`rlwe1`).
#[derive(Clone, Copy, Debug)]
pub struct RgswRow<const N: usize> {
    pub rlwe0: RlweCiphertext<N>,
    pub rlwe1: RlweCiphertext<N>,
}

/// RGSW ciphertext with `ELL` decomposition levels.
#[derive(Clone, Debug)]
pub struct RgswCiphertext<const N: usize, const ELL: usize> {
    pub rows: [RgswRow<N>; ELL],
}

/// Encrypt a bit as RGSW under `sk`.
pub fn rgsw_encrypt<
    const N: usize,
    const LOG: u32,
    const ELL: usize,
    const BASE_LOG: u32,
    const ETA: u32,
    R: SpecRng,
>(
    m: bool,
    sk: &RlweSecretKey<N>,
    rng: &mut R,
) -> RgswCiphertext<N, ELL> {
    let rows = core::array::from_fn(|j| {
        let g = gadget::level_factor::<LOG>(BASE_LOG, j);
        let contrib = if m { g } else { 0 };
        // a-column: encrypt zero, add m * g_j to a[0] so the phase carries
        // -m * g_j * s(X).
        let mut rlwe0 = rlwe_encrypt_scalar::<N, LOG, ETA, R>(0, sk, rng);
        rlwe0.a[0] = torus::add::<LOG>(rlwe0.a[0], contrib);
        // b-column: encrypt m * g_j in the constant coefficient.
        let rlwe1 = rlwe_encrypt_scalar::<N, LOG, ETA, R>(contrib, sk, rng);
        RgswRow { rlwe0, rlwe1 }
    });
    RgswCiphertext { rows }
}

/// External product `RGSW(m) ⊡ ct -> RLWE` with phase `m * phase(ct)` plus
/// RGSW noise.
pub fn external_product<
    const N: usize,
    const LOG: u32,
    const ELL: usize,
    const BASE_LOG: u32,
>(
    c: &RgswCiphertext<N, ELL>,
    ct: &RlweCiphertext<N>,
) -> RlweCiphertext<N> {
    let a_dec = gadget::poly_decompose::<N, LOG, ELL, BASE_LOG>(&ct.a);
    let b_dec = gadget::poly_decompose::<N, LOG, ELL, BASE_LOG>(&ct.b);

    let mut out_a = [0u32; N];
    let mut out_b = [0u32; N];
    for j in 0..ELL {
        let row = &c.rows[j];
        let a0 = poly_mul_neg::<N, LOG>(&a_dec[j], &row.rlwe0.a);
        let a1 = poly_mul_neg::<N, LOG>(&a_dec[j], &row.rlwe0.b);
        let b0 = poly_mul_neg::<N, LOG>(&b_dec[j], &row.rlwe1.a);
        let b1 = poly_mul_neg::<N, LOG>(&b_dec[j], &row.rlwe1.b);
        for k in 0..N {
            out_a[k] = out_a[k].wrapping_add(a0[k]).wrapping_add(b0[k]);
            out_b[k] = out_b[k].wrapping_add(a1[k]).wrapping_add(b1[k]);
        }
    }
    for k in 0..N {
        out_a[k] = torus::reduce::<LOG>(out_a[k]);
        out_b[k] = torus::reduce::<LOG>(out_b[k]);
    }
    RlweCiphertext { a: out_a, b: out_b }
}

/// CMUX: `d0` if `m = 0`, `d1` if `m = 1`, computed as
/// `d0 + C ⊡ (d1 - d0)`.
pub fn cmux<const N: usize, const LOG: u32, const ELL: usize, const BASE_LOG: u32>(
    c: &RgswCiphertext<N, ELL>,
    d1: &RlweCiphertext<N>,
    d0: &RlweCiphertext<N>,
) -> RlweCiphertext<N> {
    let diff = rlwe_sub::<N, LOG>(d1, d0);
    let prod = external_product::<N, LOG, ELL, BASE_LOG>(c, &diff);
    rlwe_add::<N, LOG>(d0, &prod)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::params::toy;
    use crate::binfhe::rlwe::{rlwe_phase, rlwe_trivial};

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

    /// Independent phase model: decrypt every coefficient with a directly
    /// written negacyclic convolution (not `rlwe_phase`).
    fn clear_phase<const N: usize>(ct: &RlweCiphertext<N>, key: &[u32; N]) -> [u32; N] {
        let mut phase = [0u32; N];
        for i in 0..N {
            let mut product = 0u32;
            for k in 0..N {
                let idx = if i >= k { i - k } else { N + i - k };
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

    #[test]
    fn external_product_scales_phase_by_the_encrypted_bit() {
        let mut rng = TestRng::new(0xE7);
        let sk = crate::binfhe::rlwe::gen_rlwe_secret_key::<{ toy::BIG_N }, _>(&mut rng);

        // Arbitrary operand content.
        let mut content = rlwe_trivial::<{ toy::BIG_N }, 8>(&[0u32; toy::BIG_N]);
        for i in 0..toy::BIG_N {
            content.b[i] = (i as u32 * 37 + 11) & 0xFF;
        }

        for bit in [false, true] {
            let c = rgsw_encrypt::<{ toy::BIG_N }, 8, { toy::BS_ELL }, { toy::BS_BASE_LOG }, 0, _>(
                bit, &sk, &mut rng,
            );
            let out = external_product::<{ toy::BIG_N }, 8, { toy::BS_ELL }, { toy::BS_BASE_LOG }>(
                &c, &content,
            );
            let phase = clear_phase(&out, &sk.key);
            for i in 0..toy::BIG_N {
                let expected = if bit { content.b[i] } else { 0 };
                assert_eq!(
                    phase[i], expected,
                    "external product coefficient {i}, bit={bit}"
                );
            }
        }
    }

    #[test]
    fn cmux_selects_the_correct_operand() {
        let mut rng = TestRng::new(0xC1);
        let sk = crate::binfhe::rlwe::gen_rlwe_secret_key::<{ toy::BIG_N }, _>(&mut rng);
        let mut d0 = rlwe_trivial::<{ toy::BIG_N }, 8>(&[0u32; toy::BIG_N]);
        let mut d1 = rlwe_trivial::<{ toy::BIG_N }, 8>(&[0u32; toy::BIG_N]);
        for i in 0..toy::BIG_N {
            d0.b[i] = (i as u32 * 17 + 1) & 0xFF;
            d1.b[i] = (i as u32 * 29 + 7) & 0xFF;
        }
        for bit in [false, true] {
            let c = rgsw_encrypt::<{ toy::BIG_N }, 8, { toy::BS_ELL }, { toy::BS_BASE_LOG }, 0, _>(
                bit, &sk, &mut rng,
            );
            let out = cmux::<{ toy::BIG_N }, 8, { toy::BS_ELL }, { toy::BS_BASE_LOG }>(
                &c, &d1, &d0,
            );
            let phase = clear_phase(&out, &sk.key);
            let expected = if bit { &d1 } else { &d0 };
            for i in 0..toy::BIG_N {
                assert_eq!(phase[i], expected.b[i], "cmux coefficient {i}, bit={bit}");
            }
        }
    }
}
