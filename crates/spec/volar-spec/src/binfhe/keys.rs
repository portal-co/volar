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
//! [`binfhe_key_switch`] converts an `BinfheLweCiphertext<BIG_N>` at modulus
//! `2^LOG_MOD_KS` into an `BinfheLweCiphertext<N_LWE>` at the same modulus. The
//! modulus switches that move the ciphertext `Q -> modKS -> q` live in
//! [`crate::binfhe::modswitch`]; the full pipeline composition lives in
//! [`crate::binfhe::pbs`].

use crate::SpecRng;
use crate::binfhe::gadget;
use crate::binfhe::lwe::{BinfheLweCiphertext, BinfheLweSecretKey, binfhe_lwe_encrypt_raw};
use crate::binfhe::rlwe::BinfheRlweSecretKey;
use crate::binfhe::rgsw::{BinfheRgswCiphertext, binfhe_rgsw_encrypt};
use crate::binfhe::torus;

/// A borrowed view of a key-switching key (zero heap).
///
/// Sized entirely by const generics; the referenced storage may be a
/// stack-allocated fixed array on targets with ample stack (the Volar-IR
/// LLVM target and similar) or the heap buffer of an owned
/// [`BinfheKeySwitchingKey`] on native targets. See the plan
/// (`docs/fhe/vec-elimination-and-linter-plan.md` §4.2).
#[derive(Clone, Copy, Debug)]
pub struct BinfheKeySwitchingKeyRef<'a, const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize> {
    pub ksk: &'a [[BinfheLweCiphertext<N_LWE>; KS_ELL]],
}

/// A borrowed view of a bootstrapping key (zero heap).
#[derive(Clone, Copy, Debug)]
pub struct BinfheBootstrappingKeyRef<
    'a,
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
> {
    pub bsk: &'a [BinfheRgswCiphertext<BIG_N, BS_ELL>],
    pub ksk: BinfheKeySwitchingKeyRef<'a, N_LWE, BIG_N, KS_ELL>,
}

/// Read access to a key-switching key, abstract over owned vs borrowed
/// storage so the pipeline operations run on either without copying.
pub trait AsKeySwitchingKey<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize> {
    fn ksk_rows(&self) -> &[[BinfheLweCiphertext<N_LWE>; KS_ELL]];
}

/// Read access to a bootstrapping key.
pub trait AsBootstrappingKey<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>
{
    fn bsk_rows(&self) -> &[BinfheRgswCiphertext<BIG_N, BS_ELL>];
    fn ksk_ref(&self) -> BinfheKeySwitchingKeyRef<'_, N_LWE, BIG_N, KS_ELL>;
}

impl<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize>
    AsKeySwitchingKey<N_LWE, BIG_N, KS_ELL> for BinfheKeySwitchingKey<N_LWE, BIG_N, KS_ELL>
{
    fn ksk_rows(&self) -> &[[BinfheLweCiphertext<N_LWE>; KS_ELL]] {
        &self.ksk
    }
}

impl<'a, const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize>
    AsKeySwitchingKey<N_LWE, BIG_N, KS_ELL> for BinfheKeySwitchingKeyRef<'a, N_LWE, BIG_N, KS_ELL>
{
    fn ksk_rows(&self) -> &[[BinfheLweCiphertext<N_LWE>; KS_ELL]] {
        self.ksk
    }
}

impl<const N_LWE: usize, const BIG_N: usize, const BS_ELL: usize, const KS_ELL: usize>
    AsBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>
    for BinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>
{
    fn bsk_rows(&self) -> &[BinfheRgswCiphertext<BIG_N, BS_ELL>] {
        &self.bsk
    }
    fn ksk_ref(&self) -> BinfheKeySwitchingKeyRef<'_, N_LWE, BIG_N, KS_ELL> {
        BinfheKeySwitchingKeyRef { ksk: &self.ksk.ksk }
    }
}

impl<'a, const N_LWE: usize, const BIG_N: usize, const BS_ELL: usize, const KS_ELL: usize>
    AsBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>
    for BinfheBootstrappingKeyRef<'a, N_LWE, BIG_N, BS_ELL, KS_ELL>
{
    fn bsk_rows(&self) -> &[BinfheRgswCiphertext<BIG_N, BS_ELL>] {
        self.bsk
    }
    fn ksk_ref(&self) -> BinfheKeySwitchingKeyRef<'_, N_LWE, BIG_N, KS_ELL> {
        self.ksk
    }
}

impl<const N_LWE: usize, const BIG_N: usize, const BS_ELL: usize, const KS_ELL: usize>
    BinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>
{
    /// Borrow this owned key as a zero-copy view.
    pub fn as_ref(&self) -> BinfheBootstrappingKeyRef<'_, N_LWE, BIG_N, BS_ELL, KS_ELL> {
        BinfheBootstrappingKeyRef {
            bsk: &self.bsk,
            ksk: BinfheKeySwitchingKeyRef { ksk: &self.ksk.ksk },
        }
    }
}

/// Key-switching key: `ksk[i][j]` encrypts `s'_i * 2^shift_j` under the
/// LWE key at modulus `2^LOG_MOD_KS`.
#[derive(Clone, Debug)]
/// @volar-allow-vec: eval-key-store: owned variant for native execution;
/// ~18 MB at Std128 cannot live on the native stack. Use
/// [`BinfheKeySwitchingKeyRef`] (zero-heap) on stack-rich virtual targets.
pub struct BinfheKeySwitchingKey<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize> {
    /// Heap-held: at Std128 dimensions the array form would be ~18 MB and
    /// overflow the stack during construction.
    pub ksk: alloc::vec::Vec<[BinfheLweCiphertext<N_LWE>; KS_ELL]>,
}

/// The full evaluation key: bootstrapping key plus key-switching key.
///
/// The modulus and decomposition constants are part of the type identity
/// through the const generics of the contained ciphertexts; a key generated
/// for one profile cannot be passed to another profile's operations.
#[derive(Clone, Debug)]
/// @volar-allow-vec: eval-key-store: owned variant for native execution;
/// ~36 MB at Std128 cannot live on the native stack. Use
/// [`BinfheBootstrappingKeyRef`] (zero-heap) on stack-rich virtual targets.
pub struct BinfheBootstrappingKey<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
> {
    /// Heap-held: at Std128 dimensions the array form would be ~36 MB.
    pub bsk: alloc::vec::Vec<BinfheRgswCiphertext<BIG_N, BS_ELL>>,
    pub ksk: BinfheKeySwitchingKey<N_LWE, BIG_N, KS_ELL>,
}

/// Generate the evaluation key. Noise is CBD-`ETA` at each entry's own
/// modulus (ring modulus for the BSK, `2^LOG_MOD_KS` for the KSK).
/// @volar-allow-vec: eval-key-store: collects into the owned key buffers
/// (native execution); borrowed views skip this allocation on virtual targets.
pub fn binfhe_gen_bootstrapping_key<
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
    lwe_sk: &BinfheLweSecretKey<N_LWE>,
    rlwe_sk: &BinfheRlweSecretKey<BIG_N>,
    rng: &mut R,
) -> BinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> {
    let bsk = (0..N_LWE)
        .map(|i| {
            binfhe_rgsw_encrypt::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG, ETA, R>(
                lwe_sk.key[i] != 0,
                rlwe_sk,
                rng,
            )
        })
        .collect();
    let ksk = BinfheKeySwitchingKey {
        ksk: (0..BIG_N)
            .map(|i| {
                core::array::from_fn(|j| {
                    let msg = rlwe_sk.key[i]
                        .wrapping_mul(gadget::level_factor::<LOG_MOD_KS>(KS_BASE_LOG, j));
                    binfhe_lwe_encrypt_raw::<N_LWE, LOG_MOD_KS, ETA, R>(
                        torus::reduce::<LOG_MOD_KS>(msg),
                        lwe_sk,
                        rng,
                    )
                })
            })
            .collect(),
    };
    BinfheBootstrappingKey { bsk, ksk }
}

/// Key switching: `BinfheLweCiphertext<BIG_N> -> BinfheLweCiphertext<N_LWE>` at the
/// shared modulus `2^LOG_MOD_KS`.
///
/// `out = (0, b) - sum_{i,j} d_j(a_i) * KSK[i][j]` where `d_j` is the exact
/// covering decomposition of `a_i` (see [`gadget`]).
pub fn binfhe_key_switch<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_MOD_KS: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    K: AsKeySwitchingKey<N_LWE, BIG_N, KS_ELL> + ?Sized,
>(
    ct: &BinfheLweCiphertext<BIG_N>,
    ksk: &K,
) -> BinfheLweCiphertext<N_LWE> {
    let ksk_rows = ksk.ksk_rows();
    let mut out_a = [0u32; N_LWE];
    let mut out_b = ct.b;
    for i in 0..BIG_N {
        let digits = gadget::gadget_decompose::<LOG_MOD_KS, KS_ELL, KS_BASE_LOG>(ct.a[i]);
        for j in 0..KS_ELL {
            let d = digits[j];
            if d == 0 {
                continue;
            }
            let entry = &ksk_rows[i][j];
            for k in 0..N_LWE {
                out_a[k] = out_a[k].wrapping_sub(d.wrapping_mul(entry.a[k]));
            }
            out_b = out_b.wrapping_sub(d.wrapping_mul(entry.b));
        }
    }
    for k in 0..N_LWE {
        out_a[k] = torus::reduce::<LOG_MOD_KS>(out_a[k]);
    }
    BinfheLweCiphertext {
        a: out_a,
        b: torus::reduce::<LOG_MOD_KS>(out_b),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::lwe::{binfhe_gen_lwe_secret_key, binfhe_lwe_decrypt, binfhe_lwe_encrypt, lwe_phase};
    use crate::binfhe::params::toy;
    use crate::binfhe::rlwe::binfhe_gen_rlwe_secret_key;

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
        BinfheLweSecretKey<{ toy::N_LWE }>,
        BinfheRlweSecretKey<{ toy::BIG_N }>,
        BinfheBootstrappingKey<{ toy::N_LWE }, { toy::BIG_N }, { toy::BS_ELL }, { toy::KS_ELL }>,
    ) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = binfhe_gen_lwe_secret_key(&mut rng);
        let rlwe_sk = binfhe_gen_rlwe_secret_key(&mut rng);
        let bk = binfhe_gen_bootstrapping_key::<
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
        // BinfheRgswCiphertext derives Debug/Clone but not PartialEq; compare the
        // b-bodies of the first row as a determinism witness.
        assert_eq!(a.bsk[0].rows[0].rlwe1.b, b.bsk[0].rows[0].rlwe1.b);
    }

    /// Zero-heap pipeline: build key material in stack-allocated arrays,
    /// view it through `BinfheBootstrappingKeyRef`, and run key switching through
    /// the shared operation surface with no owned `BinfheBootstrappingKey`/`Vec`.
    #[test]
    fn borrowed_key_view_runs_with_zero_heap_storage() {
        use crate::binfhe::lwe::{binfhe_lwe_decrypt, binfhe_lwe_encrypt, lwe_phase};
        let mut rng = TestRng::new(0xB0E0);
        let lwe_sk = binfhe_gen_lwe_secret_key(&mut rng);
        let rlwe_sk = crate::binfhe::rlwe::binfhe_gen_rlwe_secret_key(&mut rng);
        // Generate into an owned key, then move the rows into stack arrays.
        let owned = binfhe_gen_bootstrapping_key::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, { toy::CBD_ETA }, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        // Stack-allocated storage (toy dims fit on the stack).
        let bsk_store: [crate::binfhe::rgsw::BinfheRgswCiphertext<{ toy::BIG_N }, { toy::BS_ELL }>; { toy::N_LWE }] =
            owned.bsk.try_into().unwrap();
        let ksk_store: [[BinfheLweCiphertext<{ toy::N_LWE }>; { toy::KS_ELL }]; { toy::BIG_N }] =
            owned.ksk.ksk.try_into().unwrap();
        let borrowed = BinfheBootstrappingKeyRef {
            bsk: &bsk_store,
            ksk: BinfheKeySwitchingKeyRef { ksk: &ksk_store },
        };
        // Key switching through the borrowed view.
        let source_sk = BinfheLweSecretKey::<{ toy::BIG_N }> {
            key: core::array::from_fn(|i| rlwe_sk.key[i] as u8),
        };
        let delta = 1u32 << (toy::LOG_MOD_KS - 2);
        let mut rng = TestRng::new(0xBEEF);
        for m in [false, true] {
            let ct_big = binfhe_lwe_encrypt::<{ toy::BIG_N }, { toy::LOG_MOD_KS }, 0, _>(
                m, delta, &source_sk, &mut rng,
            );
            let out = binfhe_key_switch::<
                { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_MOD_KS },
                { toy::KS_ELL }, { toy::KS_BASE_LOG }, _,
            >(&ct_big, &borrowed.ksk);
            assert_eq!(
                binfhe_lwe_decrypt::<{ toy::N_LWE }, { toy::LOG_MOD_KS }>(&out, &lwe_sk, delta),
                m,
                "borrowed-view key switch for {m}"
            );
            assert_eq!(
                lwe_phase::<{ toy::N_LWE }, { toy::LOG_MOD_KS }>(&out, &lwe_sk),
                if m { delta } else { 0 }
            );
        }
    }

    #[test]
    fn key_switch_roundtrip_is_exact_on_toy() {
        let (lwe_sk, rlwe_sk, bk) = toy_bk(12);
        // Encrypt under the *RLWE* key reinterpreted as an LWE key (this is
        // exactly what sample extraction produces) at the key-switch
        // modulus, with delta = modKS/4.
        let source_sk = BinfheLweSecretKey::<{ toy::BIG_N }> {
            key: core::array::from_fn(|i| rlwe_sk.key[i] as u8),
        };
        let delta = 1u32 << (toy::LOG_MOD_KS - 2);
        let mut rng = TestRng::new(1212);
        for m in [false, true] {
            let ct_big = binfhe_lwe_encrypt::<{ toy::BIG_N }, { toy::LOG_MOD_KS }, 0, _>(
                m, delta, &source_sk, &mut rng,
            );
            let ct_small = binfhe_key_switch::<
                { toy::N_LWE },
                { toy::BIG_N },
                { toy::LOG_MOD_KS },
                { toy::KS_ELL },
                { toy::KS_BASE_LOG },
                _,
            >(&ct_big, &bk.ksk);
            assert_eq!(
                lwe_phase::<{ toy::N_LWE }, { toy::LOG_MOD_KS }>(&ct_small, &lwe_sk),
                if m { delta } else { 0 },
                "switched phase must be canonical for {m}"
            );
            assert_eq!(
                binfhe_lwe_decrypt::<{ toy::N_LWE }, { toy::LOG_MOD_KS }>(&ct_small, &lwe_sk, delta),
                m
            );
        }
    }
}
