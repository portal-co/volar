// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Bootstrapping and key-switching keys, and the key-switching operation.
//!
//! - The **bootstrapping key** holds `RGSW(s_i)` (under the RLWE key) for
//!   each of the `N_LWE` LWE key bits, encrypted at the ring modulus
//!   `Q = 2^LOG_Q`.
//! - The **key-switching key** holds, for each of the `BIG_N` RLWE key
//!   coefficients `s'_i` and each level `j`, an LWE ciphertext of
//!   `s'_i * 2^shift_j` under the LWE key, at the intermediate modulus
//!   `2^LOG_MOD_KS`.
//!
//! [`key_switch`] converts an `LweCiphertext<BIG_N>` at modulus
//! `2^LOG_MOD_KS` into an `LweCiphertext<N_LWE>` at the same modulus. The
//! modulus switches that move the ciphertext `Q -> modKS -> q` live in
//! [`crate::binfhe::modswitch`]; the full pipeline composition lives in
//! [`crate::binfhe::pbs`].

use crate::SpecRng;
use crate::binfhe::gadget;
use crate::binfhe::lwe::{LweCiphertext, LweSecretKey, lwe_encrypt_raw};
use crate::binfhe::rlwe::RlweSecretKey;
use crate::binfhe::rgsw::{RgswCiphertext, rgsw_encrypt};
use crate::binfhe::torus;

/// Key-switching key: `ksk[i][j]` encrypts `s'_i * 2^shift_j` under the
/// LWE key at modulus `2^LOG_MOD_KS`.
#[derive(Clone, Debug)]
pub struct KeySwitchingKey<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize> {
    /// Heap-held: at Std128 dimensions the array form would be ~18 MB and
    /// overflow the stack during construction.
    pub ksk: alloc::vec::Vec<[LweCiphertext<N_LWE>; KS_ELL]>,
}

/// The full evaluation key: bootstrapping key plus key-switching key.
///
/// The modulus and decomposition constants are part of the type identity
/// through the const generics of the contained ciphertexts; a key generated
/// for one profile cannot be passed to another profile's operations.
#[derive(Clone, Debug)]
pub struct BootstrappingKey<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
> {
    /// Heap-held: at Std128 dimensions the array form would be ~36 MB.
    pub bsk: alloc::vec::Vec<RgswCiphertext<BIG_N, BS_ELL>>,
    pub ksk: KeySwitchingKey<N_LWE, BIG_N, KS_ELL>,
}

/// Generate the evaluation key. Noise is CBD-`ETA` at each entry's own
/// modulus (ring modulus for the BSK, `2^LOG_MOD_KS` for the KSK).
pub fn gen_bootstrapping_key<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const ETA: u32,
    R: SpecRng,
>(
    lwe_sk: &LweSecretKey<N_LWE>,
    rlwe_sk: &RlweSecretKey<BIG_N>,
    rng: &mut R,
) -> BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> {
    let bsk = (0..N_LWE)
        .map(|i| {
            rgsw_encrypt::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG, ETA, R>(
                lwe_sk.key[i] != 0,
                rlwe_sk,
                rng,
            )
        })
        .collect();
    let ksk = KeySwitchingKey {
        ksk: (0..BIG_N)
            .map(|i| {
                core::array::from_fn(|j| {
                    let msg = rlwe_sk.key[i]
                        .wrapping_mul(gadget::level_factor::<LOG_MOD_KS>(KS_BASE_LOG, j));
                    lwe_encrypt_raw::<N_LWE, LOG_MOD_KS, ETA, R>(
                        torus::reduce::<LOG_MOD_KS>(msg),
                        lwe_sk,
                        rng,
                    )
                })
            })
            .collect(),
    };
    BootstrappingKey { bsk, ksk }
}

/// Key switching: `LweCiphertext<BIG_N> -> LweCiphertext<N_LWE>` at the
/// shared modulus `2^LOG_MOD_KS`.
///
/// `out = (0, b) - sum_{i,j} d_j(a_i) * KSK[i][j]` where `d_j` is the exact
/// covering decomposition of `a_i` (see [`gadget`]).
pub fn key_switch<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_MOD_KS: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
>(
    ct: &LweCiphertext<BIG_N>,
    ksk: &KeySwitchingKey<N_LWE, BIG_N, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let mut out_a = [0u32; N_LWE];
    let mut out_b = ct.b;
    for i in 0..BIG_N {
        let digits = gadget::decompose::<LOG_MOD_KS, KS_ELL, KS_BASE_LOG>(ct.a[i]);
        for j in 0..KS_ELL {
            let d = digits[j];
            if d == 0 {
                continue;
            }
            let entry = &ksk.ksk[i][j];
            for k in 0..N_LWE {
                out_a[k] = out_a[k].wrapping_sub(d.wrapping_mul(entry.a[k]));
            }
            out_b = out_b.wrapping_sub(d.wrapping_mul(entry.b));
        }
    }
    for k in 0..N_LWE {
        out_a[k] = torus::reduce::<LOG_MOD_KS>(out_a[k]);
    }
    LweCiphertext {
        a: out_a,
        b: torus::reduce::<LOG_MOD_KS>(out_b),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::lwe::{gen_lwe_secret_key, lwe_decrypt, lwe_encrypt, lwe_phase};
    use crate::binfhe::params::toy;
    use crate::binfhe::rlwe::gen_rlwe_secret_key;

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

    fn toy_bk(
        seed: u64,
    ) -> (
        LweSecretKey<{ toy::N_LWE }>,
        RlweSecretKey<{ toy::BIG_N }>,
        BootstrappingKey<{ toy::N_LWE }, { toy::BIG_N }, { toy::BS_ELL }, { toy::KS_ELL }>,
    ) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = gen_lwe_secret_key(&mut rng);
        let rlwe_sk = gen_rlwe_secret_key(&mut rng);
        let bk = gen_bootstrapping_key::<
            { toy::N_LWE },
            { toy::BIG_N },
            { toy::LOG_Q },
            { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS },
            { toy::BS_ELL },
            { toy::BS_BASE_LOG },
            { toy::KS_ELL },
            { toy::KS_BASE_LOG },
            { toy::CBD_ETA },
            _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        (lwe_sk, rlwe_sk, bk)
    }

    #[test]
    fn keygen_is_deterministic() {
        let (_, _, a) = toy_bk(11);
        let (_, _, b) = toy_bk(11);
        assert_eq!(a.ksk.ksk[0][0], b.ksk.ksk[0][0]);
        // RgswCiphertext derives Debug/Clone but not PartialEq; compare the
        // b-bodies of the first row as a determinism witness.
        assert_eq!(a.bsk[0].rows[0].rlwe1.b, b.bsk[0].rows[0].rlwe1.b);
    }

    #[test]
    fn key_switch_roundtrip_is_exact_on_toy() {
        let (lwe_sk, rlwe_sk, bk) = toy_bk(12);
        // Encrypt under the *RLWE* key reinterpreted as an LWE key (this is
        // exactly what sample extraction produces) at the key-switch
        // modulus, with delta = modKS/4.
        let source_sk = LweSecretKey::<{ toy::BIG_N }> {
            key: core::array::from_fn(|i| rlwe_sk.key[i] as u8),
        };
        let delta = 1u32 << (toy::LOG_MOD_KS - 2);
        let mut rng = TestRng::new(1212);
        for m in [false, true] {
            let ct_big = lwe_encrypt::<{ toy::BIG_N }, { toy::LOG_MOD_KS }, 0, _>(
                m, delta, &source_sk, &mut rng,
            );
            let ct_small = key_switch::<
                { toy::N_LWE },
                { toy::BIG_N },
                { toy::LOG_MOD_KS },
                { toy::KS_ELL },
                { toy::KS_BASE_LOG },
            >(&ct_big, &bk.ksk);
            assert_eq!(
                lwe_phase::<{ toy::N_LWE }, { toy::LOG_MOD_KS }>(&ct_small, &lwe_sk),
                if m { delta } else { 0 },
                "switched phase must be canonical for {m}"
            );
            assert_eq!(
                lwe_decrypt::<{ toy::N_LWE }, { toy::LOG_MOD_KS }>(&ct_small, &lwe_sk, delta),
                m
            );
        }
    }
}
