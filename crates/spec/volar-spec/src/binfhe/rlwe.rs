// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! RLWE over `Z_Q[X]/(X^N + 1)` with schoolbook negacyclic convolution.
//!
//! All arithmetic is `u32` wrapping arithmetic reduced to `Z_Q` at store
//! time; since `LOG_Q <= 32`, computing in `Z_{2^32}` and reducing at the
//! end is exact (`2^LOG_Q` divides `2^32`). Polynomial multiplication is
//! O(N^2) — reference quality; a negacyclic NTT is a later, separately
//! tested optimization (plan §3).
//!
//! # Sample extraction
//!
//! [`binfhe_sample_extract`] maps an RLWE ciphertext to an LWE ciphertext of
//! dimension N whose phase is the constant coefficient of the RLWE phase:
//! `a_lwe[0] = a[0]`, `a_lwe[i] = -a[N - i]` for `i > 0`, `b_lwe = b[0]`.
//! This is the standard negacyclic extraction identity (Micciancio &
//! Polyakov, ePrint 2020/086, p.11); the M2 tests check it against an
//! independently written convolution.

use crate::SpecRng;
use crate::binfhe::lwe::BinfheLweCiphertext;
use crate::binfhe::sampler;
use crate::binfhe::torus;

/// Binary RLWE secret key polynomial (coefficients 0/1 as `u32`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinfheRlweSecretKey<const N: usize> {
    pub key: [u32; N],
}

/// RLWE ciphertext `(a, b)` with `b = a * s + e + m` (negacyclic).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BinfheRlweCiphertext<const N: usize> {
    pub a: [u32; N],
    pub b: [u32; N],
}

/// Generate a binary RLWE secret key.
pub fn binfhe_gen_rlwe_secret_key<const N: usize, R: SpecRng>(rng: &mut R) -> BinfheRlweSecretKey<N> {
    let mut key = [0u32; N];
    for k in key.iter_mut() {
        *k = (rng.next_u32() & 1) as u32;
    }
    BinfheRlweSecretKey { key }
}

/// Negacyclic polynomial product mod `(X^N + 1, 2^LOG)`.
pub fn binfhe_poly_mul_neg<const N: usize, const LOG: u32>(a: &[u32; N], b: &[u32; N]) -> [u32; N] {
    let mut result = [0u32; N];
    for i in 0..N {
        for j in 0..N {
            let deg = i + j;
            let term = a[i].wrapping_mul(b[j]);
            if deg < N {
                result[deg] = result[deg].wrapping_add(term);
            } else {
                result[deg - N] = result[deg - N].wrapping_sub(term);
            }
        }
    }
    for r in result.iter_mut() {
        *r = torus::reduce::<LOG>(*r);
    }
    result
}

/// Multiply by `X^exp` mod `X^N + 1`; exponents are taken mod `2N` and the
/// negacyclic wrap negates.
pub fn binfhe_poly_rotate<const N: usize, const LOG: u32>(p: &[u32; N], exp: usize) -> [u32; N] {
    let exp = exp % (2 * N);
    let mut result = [0u32; N];
    for (i, &coeff) in p.iter().enumerate() {
        let dest = i + exp;
        if dest < N {
            result[dest] = result[dest].wrapping_add(coeff);
        } else if dest < 2 * N {
            result[dest - N] = result[dest - N].wrapping_sub(coeff);
        } else {
            result[dest - 2 * N] = result[dest - 2 * N].wrapping_add(coeff);
        }
    }
    for r in result.iter_mut() {
        *r = torus::reduce::<LOG>(*r);
    }
    result
}

/// Encrypt a polynomial message. `msg` coefficients must already be reduced
/// mod `2^LOG`.
pub fn binfhe_rlwe_encrypt_poly<const N: usize, const LOG: u32, const ETA: u32, R: SpecRng>(
    msg: &[u32; N],
    sk: &BinfheRlweSecretKey<N>,
    rng: &mut R,
) -> BinfheRlweCiphertext<N> {
    let a: [u32; N] = core::array::from_fn(|_| torus::reduce::<LOG>(rng.next_u32()));
    let mut b = binfhe_poly_mul_neg::<N, LOG>(&a, &sk.key);
    for i in 0..N {
        b[i] = torus::reduce::<LOG>(
            b[i]
                .wrapping_add(sampler::sample_error::<LOG, ETA, R>(rng))
                .wrapping_add(msg[i]),
        );
    }
    BinfheRlweCiphertext { a, b }
}

/// Encrypt a scalar placed in the constant coefficient.
pub fn binfhe_rlwe_encrypt_scalar<const N: usize, const LOG: u32, const ETA: u32, R: SpecRng>(
    m: u32,
    sk: &BinfheRlweSecretKey<N>,
    rng: &mut R,
) -> BinfheRlweCiphertext<N> {
    let mut msg = [0u32; N];
    msg[0] = m;
    binfhe_rlwe_encrypt_poly::<N, LOG, ETA, R>(&msg, sk, rng)
}

/// Phase polynomial `b - a * s`.
pub fn binfhe_rlwe_phase<const N: usize, const LOG: u32>(
    ct: &BinfheRlweCiphertext<N>,
    sk: &BinfheRlweSecretKey<N>,
) -> [u32; N] {
    let product = binfhe_poly_mul_neg::<N, LOG>(&ct.a, &sk.key);
    let mut phase = [0u32; N];
    for i in 0..N {
        phase[i] = torus::torus_sub::<LOG>(ct.b[i], product[i]);
    }
    phase
}

/// Extract the constant-coefficient LWE ciphertext (dimension N, same
/// modulus). See the module docs for the extraction identity.
pub fn binfhe_sample_extract<const N: usize, const LOG: u32>(
    ct: &BinfheRlweCiphertext<N>,
) -> BinfheLweCiphertext<N> {
    let mut a_lwe = [0u32; N];
    a_lwe[0] = ct.a[0];
    for i in 1..N {
        a_lwe[i] = torus::torus_neg::<LOG>(ct.a[N - i]);
    }
    BinfheLweCiphertext {
        a: a_lwe,
        b: ct.b[0],
    }
}

/// Ciphertext addition.
pub fn binfhe_rlwe_add<const N: usize, const LOG: u32>(
    x: &BinfheRlweCiphertext<N>,
    y: &BinfheRlweCiphertext<N>,
) -> BinfheRlweCiphertext<N> {
    let mut out = *x;
    for i in 0..N {
        out.a[i] = torus::torus_add::<LOG>(out.a[i], y.a[i]);
        out.b[i] = torus::torus_add::<LOG>(out.b[i], y.b[i]);
    }
    out
}

/// Ciphertext subtraction.
pub fn binfhe_rlwe_sub<const N: usize, const LOG: u32>(
    x: &BinfheRlweCiphertext<N>,
    y: &BinfheRlweCiphertext<N>,
) -> BinfheRlweCiphertext<N> {
    let mut out = *x;
    for i in 0..N {
        out.a[i] = torus::torus_sub::<LOG>(out.a[i], y.a[i]);
        out.b[i] = torus::torus_sub::<LOG>(out.b[i], y.b[i]);
    }
    out
}

/// Multiply the ciphertext by `X^exp` (rotates both polynomials).
pub fn binfhe_rlwe_rotate<const N: usize, const LOG: u32>(
    ct: &BinfheRlweCiphertext<N>,
    exp: usize,
) -> BinfheRlweCiphertext<N> {
    BinfheRlweCiphertext {
        a: binfhe_poly_rotate::<N, LOG>(&ct.a, exp),
        b: binfhe_poly_rotate::<N, LOG>(&ct.b, exp),
    }
}

/// Trivial encryption of a polynomial (`a = 0`); the phase is the message.
pub fn binfhe_rlwe_trivial<const N: usize, const LOG: u32>(msg: &[u32; N]) -> BinfheRlweCiphertext<N> {
    BinfheRlweCiphertext {
        a: [0u32; N],
        b: *msg,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::params::toy;

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

    /// Independent clear rotation: multiply by X^exp in
    /// `Z_{2^8}[X]/(X^N+1)` by placing coefficients at destinations, with
    /// the negacyclic sign tracked explicitly. Deliberately shares no code
    /// with `binfhe_poly_rotate`.
    fn clear_rotate<const N: usize>(p: &[u32; N], exp: usize) -> [u32; N] {
        // X^exp with exp' = exp mod 2N; crossing a multiple of N negates.
        let base = exp % (2 * N);
        let mut out = [0u32; N];
        for (i, &c) in p.iter().enumerate() {
            let dest = i + base;
            let sign = (dest / N) % 2 == 1;
            let v = if sign { 256u32.wrapping_sub(c) & 0xFF } else { c };
            let slot = dest % N;
            out[slot] = (out[slot] + v) & 0xFF;
        }
        out
    }

    #[test]
    fn poly_rotate_matches_monomial_multiplication() {
        let mut rng = TestRng::new(0xA07);
        let p: [u32; toy::BIG_N] = core::array::from_fn(|_| rng.next_u32() & 0xFF);
        for exp in [0usize, 1, 5, 63, 64, 65, 127, 128, 129, 255] {
            // Independent model: build X^exp explicitly and convolve.
            let exp = exp % (2 * toy::BIG_N);
            let mut mono = [0u32; toy::BIG_N];
            if exp < toy::BIG_N {
                mono[exp] = 1;
            } else {
                mono[exp - toy::BIG_N] = 255; // -1 mod 2^8
            }
            let expected = binfhe_poly_mul_neg::<{ toy::BIG_N }, 8>(&p, &mono);
            let got = binfhe_poly_rotate::<{ toy::BIG_N }, 8>(&p, exp);
            assert_eq!(got, expected, "rotation by {exp}");
            // And against the second independent model.
            assert_eq!(got, clear_rotate(&p, exp), "clear rotation by {exp}");
        }
    }

    #[test]
    fn rlwe_encrypt_decrypt_roundtrip() {
        let mut rng = TestRng::new(0xB0B);
        let sk = binfhe_gen_rlwe_secret_key::<{ toy::BIG_N }, _>(&mut rng);
        let msg: [u32; toy::BIG_N] = core::array::from_fn(|i| (i as u32 * 7 + 3) & 0xFF);
        let ct = binfhe_rlwe_encrypt_poly::<{ toy::BIG_N }, 8, 0, _>(&msg, &sk, &mut rng);
        assert_eq!(binfhe_rlwe_phase::<{ toy::BIG_N }, 8>(&ct, &sk), msg);
    }

    #[test]
    fn sample_extract_matches_direct_convolution() {
        // Independently compute the constant coefficient of the phase as
        // b[0] - sum_i a[i]*s[N-i]-with-sign and check the extracted LWE
        // phase equals it (ePrint 2020/086 p.11 identity).
        let mut rng = TestRng::new(0xC0C);
        let sk = binfhe_gen_rlwe_secret_key::<{ toy::BIG_N }, _>(&mut rng);
        let msg: [u32; toy::BIG_N] = core::array::from_fn(|i| (i as u32 * 0x1B + 1) & 0xFF);
        let ct = binfhe_rlwe_encrypt_poly::<{ toy::BIG_N }, 8, 0, _>(&msg, &sk, &mut rng);

        // Direct: phase[0] = b[0] - (a*s)[0] with
        // (a*s)[0] = a0*s0 - sum_{i=1..N} a[i]*s[N-i].
        let mut product0 = ct.a[0].wrapping_mul(sk.key[0]);
        for i in 1..toy::BIG_N {
            product0 = product0.wrapping_sub(ct.a[i].wrapping_mul(sk.key[toy::BIG_N - i]));
        }
        let expected_phase0 = ct.b[0].wrapping_sub(product0) & 0xFF;

        let extracted = binfhe_sample_extract::<{ toy::BIG_N }, 8>(&ct);
        // LWE phase of the extraction under the RLWE key reused as LWE key.
        let mut dot = 0u32;
        for i in 0..toy::BIG_N {
            dot = dot.wrapping_add(extracted.a[i].wrapping_mul(sk.key[i]));
        }
        let got = extracted.b.wrapping_sub(dot) & 0xFF;
        assert_eq!(got, expected_phase0);
        assert_eq!(got, msg[0], "extracted phase must be the constant message");
    }
}
