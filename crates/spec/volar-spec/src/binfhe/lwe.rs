// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! LWE secret keys, ciphertexts, and exact linear operations.
//!
//! An LWE ciphertext `(a, b)` under a binary key `s` over `Z_M`
//! (`M = 2^LOG_M`) has phase `b - <a, s> = m * Delta + e`. The ciphertext
//! type is generic only over the key dimension; the modulus is a function
//! parameter because the pipeline deliberately moves one logical ciphertext
//! through the modulus chain `q -> Q -> modKS -> q` (see `params.rs`).
//!
//! # Wire encoding
//!
//! A Boolean wire is an LWE ciphertext mod `q = 2^LOG_Q_LWE` with
//! `Delta = q / 2^(K + 1)` where `K` is the maximum LUT arity of the
//! circuit (`K >= 1`; `K = 1` is the classic `{0, q/4}` encoding). All
//! wires in one circuit share one `K`. See `mod.rs` for the full
//! convention.

use crate::SpecRng;
use crate::binfhe::sampler;
use crate::binfhe::torus;

/// Binary LWE secret key.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LweSecretKey<const N: usize> {
    pub key: [u8; N],
}

/// LWE ciphertext over an implicit power-of-two modulus.
///
/// All components are stored reduced modulo the operation's modulus.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LweCiphertext<const N: usize> {
    pub a: [u32; N],
    pub b: u32,
}

/// The canonical Boolean wire delta for maximum LUT arity `K`:
/// `Delta = q / 2^(K+1)`. Requires `K + 1 <= LOG_Q_LWE` (profiles guarantee
/// `K + 2 <= LOG_Q_LWE`, leaving one more bit for the centering offset).
pub const fn wire_delta<const LOG_Q_LWE: u32>(k_max: usize) -> u32 {
    1u32 << (LOG_Q_LWE - 1 - k_max as u32)
}

/// Generate a binary LWE secret key.
pub fn gen_lwe_secret_key<const N: usize, R: SpecRng>(rng: &mut R) -> LweSecretKey<N> {
    let mut key = [0u8; N];
    let mut i = 0;
    while i < N {
        let mut word = rng.next_u32();
        let take = (N - i).min(32);
        for _ in 0..take {
            key[i] = (word & 1) as u8;
            word >>= 1;
            i += 1;
        }
    }
    LweSecretKey { key }
}

/// Encrypt bit `m` at encoding `delta` modulo `2^LOG_M` with CBD-`ETA`
/// noise.
pub fn lwe_encrypt<const N: usize, const LOG_M: u32, const ETA: u32, R: SpecRng>(
    m: bool,
    delta: u32,
    sk: &LweSecretKey<N>,
    rng: &mut R,
) -> LweCiphertext<N> {
    let msg = if m { delta } else { 0 };
    lwe_encrypt_raw::<N, LOG_M, ETA, R>(msg, sk, rng)
}

/// Encrypt a raw (already scaled) phase value. Used for key-switching-key
/// construction, where the payload is a gadget-multiple of a key bit.
pub fn lwe_encrypt_raw<const N: usize, const LOG_M: u32, const ETA: u32, R: SpecRng>(
    msg: u32,
    sk: &LweSecretKey<N>,
    rng: &mut R,
) -> LweCiphertext<N> {
    let mut a = [0u32; N];
    for ai in a.iter_mut() {
        *ai = torus::reduce::<LOG_M>(rng.next_u32());
    }
    let mut dot = 0u32;
    for i in 0..N {
        dot = dot.wrapping_add(a[i].wrapping_mul(sk.key[i] as u32));
    }
    let e = sampler::sample_error::<LOG_M, ETA, R>(rng);
    let b = torus::reduce::<LOG_M>(dot.wrapping_add(e).wrapping_add(msg));
    LweCiphertext { a, b }
}

/// Phase `b - <a, s>` modulo `2^LOG_M`. For a fresh ciphertext this is
/// `m * delta + e`.
pub fn lwe_phase<const N: usize, const LOG_M: u32>(
    ct: &LweCiphertext<N>,
    sk: &LweSecretKey<N>,
) -> u32 {
    let mut dot = 0u32;
    for i in 0..N {
        dot = dot.wrapping_add(ct.a[i].wrapping_mul(sk.key[i] as u32));
    }
    torus::reduce::<LOG_M>(ct.b.wrapping_sub(dot))
}

/// Decode a phase as the nearer of `{0, delta}` modulo `2^LOG_M`.
///
/// `delta` must divide `2^LOG_M` and be a power of two (both hold for
/// [`wire_delta`]). Decision: `phase` decodes to 1 iff
/// `(phase - delta/2) mod M < delta`.
pub const fn lwe_decode<const LOG_M: u32>(phase: u32, delta: u32) -> bool {
    torus::reduce::<LOG_M>(phase.wrapping_sub(delta / 2)) < delta
}

/// Decrypt a Boolean wire ciphertext.
pub fn lwe_decrypt<const N: usize, const LOG_M: u32>(
    ct: &LweCiphertext<N>,
    sk: &LweSecretKey<N>,
    delta: u32,
) -> bool {
    lwe_decode::<LOG_M>(lwe_phase::<N, LOG_M>(ct, sk), delta)
}

/// Exact ciphertext addition.
pub fn lwe_add<const N: usize, const LOG_M: u32>(
    x: &LweCiphertext<N>,
    y: &LweCiphertext<N>,
) -> LweCiphertext<N> {
    let mut a = [0u32; N];
    for i in 0..N {
        a[i] = torus::add::<LOG_M>(x.a[i], y.a[i]);
    }
    LweCiphertext {
        a,
        b: torus::add::<LOG_M>(x.b, y.b),
    }
}

/// Exact ciphertext subtraction.
pub fn lwe_sub<const N: usize, const LOG_M: u32>(
    x: &LweCiphertext<N>,
    y: &LweCiphertext<N>,
) -> LweCiphertext<N> {
    let mut a = [0u32; N];
    for i in 0..N {
        a[i] = torus::sub::<LOG_M>(x.a[i], y.a[i]);
    }
    LweCiphertext {
        a,
        b: torus::sub::<LOG_M>(x.b, y.b),
    }
}

/// Exact ciphertext negation.
pub fn lwe_neg<const N: usize, const LOG_M: u32>(x: &LweCiphertext<N>) -> LweCiphertext<N> {
    let mut a = [0u32; N];
    for i in 0..N {
        a[i] = torus::neg::<LOG_M>(x.a[i]);
    }
    LweCiphertext {
        a,
        b: torus::neg::<LOG_M>(x.b),
    }
}

/// Exact scaling by a cleartext integer (multi-input selector weights).
pub fn lwe_scale<const N: usize, const LOG_M: u32>(
    x: &LweCiphertext<N>,
    c: u32,
) -> LweCiphertext<N> {
    let mut a = [0u32; N];
    for i in 0..N {
        a[i] = torus::mul_exact::<LOG_M>(x.a[i], c);
    }
    LweCiphertext {
        a,
        b: torus::mul_exact::<LOG_M>(x.b, c),
    }
}

/// Exact addition of a cleartext constant to the body.
pub fn lwe_add_const<const N: usize, const LOG_M: u32>(
    x: &LweCiphertext<N>,
    c: u32,
) -> LweCiphertext<N> {
    LweCiphertext {
        a: x.a,
        b: torus::add::<LOG_M>(x.b, c),
    }
}

/// Free NOT gate: `phase -> Delta - phase`. Exact for any wire encoding.
pub fn binfhe_not<const N: usize, const LOG_M: u32>(
    x: &LweCiphertext<N>,
    delta: u32,
) -> LweCiphertext<N> {
    let mut out = lwe_neg::<N, LOG_M>(x);
    out.b = torus::add::<LOG_M>(out.b, delta);
    out
}

/// Trivial (mask-zero) encryption of a cleartext Boolean at `delta`.
pub fn binfhe_trivial<const N: usize, const LOG_M: u32>(m: bool, delta: u32) -> LweCiphertext<N> {
    LweCiphertext {
        a: [0u32; N],
        b: if m { delta } else { 0 },
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

    const LOG_Q: u32 = toy::LOG_Q_LWE;

    #[test]
    fn keygen_is_deterministic_and_binary() {
        let a = gen_lwe_secret_key::<{ toy::N_LWE }, _>(&mut TestRng::new(5));
        let b = gen_lwe_secret_key::<{ toy::N_LWE }, _>(&mut TestRng::new(5));
        let c = gen_lwe_secret_key::<{ toy::N_LWE }, _>(&mut TestRng::new(6));
        assert_eq!(a, b, "same seed must reproduce the key");
        assert_ne!(a, c, "different seeds must differ");
        assert!(a.key.iter().all(|&k| k <= 1), "binary key");
    }

    #[test]
    fn encrypt_decrypt_roundtrip_all_wire_encodings() {
        // K = 1, 2, 3 (Delta = q/4, q/8, q/16) on the exact toy profile.
        let sk = gen_lwe_secret_key::<{ toy::N_LWE }, _>(&mut TestRng::new(1));
        for k in 1..=3u32 {
            let delta = wire_delta::<LOG_Q>(k as usize);
            for m in [false, true] {
                for seed in 0..8u64 {
                    let mut rng = TestRng::new(seed * 16 + k as u64);
                    let ct = lwe_encrypt::<{ toy::N_LWE }, LOG_Q, { toy::CBD_ETA }, _>(
                        m, delta, &sk, &mut rng,
                    );
                    assert_eq!(
                        lwe_decrypt::<{ toy::N_LWE }, LOG_Q>(&ct, &sk, delta),
                        m,
                        "K={k} m={m}"
                    );
                    // The toy profile is noiseless: the phase must be the
                    // exact canonical representative, not merely in-range.
                    assert_eq!(
                        lwe_phase::<{ toy::N_LWE }, LOG_Q>(&ct, &sk),
                        if m { delta } else { 0 },
                        "K={k} m={m} canonical phase"
                    );
                }
            }
        }
    }

    #[test]
    fn linear_ops_are_exact_ciphertext_arithmetic() {
        let sk = gen_lwe_secret_key::<{ toy::N_LWE }, _>(&mut TestRng::new(2));
        let delta = wire_delta::<LOG_Q>(2); // K = 2 -> q/8 = 16
        let mut rng = TestRng::new(20);
        let ct_a = lwe_encrypt::<{ toy::N_LWE }, LOG_Q, 0, _>(true, delta, &sk, &mut rng);
        let ct_b = lwe_encrypt::<{ toy::N_LWE }, LOG_Q, 0, _>(false, delta, &sk, &mut rng);

        // add: delta + 0 = delta
        let s = lwe_add::<{ toy::N_LWE }, LOG_Q>(&ct_a, &ct_b);
        assert_eq!(lwe_phase::<{ toy::N_LWE }, LOG_Q>(&s, &sk), delta);
        // sub
        let d = lwe_sub::<{ toy::N_LWE }, LOG_Q>(&ct_a, &ct_b);
        assert_eq!(lwe_phase::<{ toy::N_LWE }, LOG_Q>(&d, &sk), delta);
        // neg: -delta mod q
        let n = lwe_neg::<{ toy::N_LWE }, LOG_Q>(&ct_a);
        assert_eq!(lwe_phase::<{ toy::N_LWE }, LOG_Q>(&n, &sk), 128 - delta);
        // scale by exact integer (selector weight)
        let w = lwe_scale::<{ toy::N_LWE }, LOG_Q>(&ct_a, 2);
        assert_eq!(lwe_phase::<{ toy::N_LWE }, LOG_Q>(&w, &sk), 2 * delta);
        // add_const
        let c = lwe_add_const::<{ toy::N_LWE }, LOG_Q>(&ct_b, delta);
        assert_eq!(lwe_phase::<{ toy::N_LWE }, LOG_Q>(&c, &sk), delta);
    }

    #[test]
    fn not_gate_and_trivial_are_exact_and_composable() {
        let sk = gen_lwe_secret_key::<{ toy::N_LWE }, _>(&mut TestRng::new(3));
        let delta = wire_delta::<LOG_Q>(3);
        let mut rng = TestRng::new(30);
        for m in [false, true] {
            let ct = lwe_encrypt::<{ toy::N_LWE }, LOG_Q, 0, _>(m, delta, &sk, &mut rng);
            let not = binfhe_not::<{ toy::N_LWE }, LOG_Q>(&ct, delta);
            assert_eq!(
                lwe_phase::<{ toy::N_LWE }, LOG_Q>(&not, &sk),
                if m { 0 } else { delta },
                "NOT({m}) canonical phase"
            );
            // Double negation restores the exact ciphertext.
            let nn = binfhe_not::<{ toy::N_LWE }, LOG_Q>(&not, delta);
            assert_eq!(nn, ct);
            // Trivial ciphertexts behave like encrypted ones.
            let t = binfhe_trivial::<{ toy::N_LWE }, LOG_Q>(m, delta);
            assert_eq!(lwe_phase::<{ toy::N_LWE }, LOG_Q>(&t, &sk), if m { delta } else { 0 });
            assert_eq!(lwe_decrypt::<{ toy::N_LWE }, LOG_Q>(&t, &sk, delta), m);
        }
    }

    #[test]
    fn decode_boundary_is_midway() {
        let delta = 32u32; // q = 128
        assert!(!lwe_decode::<7>(0, delta));
        assert!(!lwe_decode::<7>(15, delta));
        assert!(lwe_decode::<7>(16, delta));
        assert!(lwe_decode::<7>(32, delta));
        assert!(lwe_decode::<7>(47, delta));
        assert!(!lwe_decode::<7>(48, delta));
        assert!(!lwe_decode::<7>(96, delta));
        // Wraparound near the modulus.
        assert!(!lwe_decode::<7>(127, delta));
    }
}
