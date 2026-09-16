// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Circuit bootstrapping (LWE -> RGSW), after CGGI17 (Chillotti, Gama,
//! Georgieva, Izabachène, "Faster Packed Homomorphic Operations and
//! Efficient Circuit Bootstrapping for TFHE", ASIACRYPT 2017, ePrint
//! 2017/430, §4), with the private key switch of CGGI16 (ePrint 2016/870);
//! cross-checked against the Apache-2.0 TFHEpp implementation
//! (`src/circuitbootstrapping.cpp`, `PrivKeySwitch` in
//! `include/tfhe/keyswitch.hpp`).
//!
//! # Construction
//!
//! For each bootstrapping-gadget level `j` (factor `g_j`):
//!
//! 1. **Level extraction.** One programmable bootstrap of the input wire
//!    with the constant-per-bin test polynomial `f(0) = 0`, `f(1) = g_j`,
//!    stopping after sample extraction: an `LWE_N` ciphertext with phase
//!    `m * g_j` under the *ring* key `s'`. (No key switch and no modulus
//!    switch: the private key switch operates at the ring modulus `Q`.)
//! 2. **Private key switching** of the extracted ciphertext, twice:
//!    - b-column (`f(x) = -x`): phase `m * g_j`;
//!    - a-column (`f(x) = x * s'(X)`): phase `-m * g_j * s'(X)`;
//!
//!    exactly the RGSW row invariant of [`crate::binfhe::rgsw`], so the
//!    output composes with `binfhe_external_product`/`binfhe_rgsw_cmux` like a directly
//!    encrypted RGSW ciphertext.
//!
//! # Private key switching
//!
//! For a key-affine target function the switching key holds, per source
//! coefficient `i in [0, N)` and level `l`, an RLWE encryption of
//! `f(s'_i) * g_l`; body terms hold `f(-1) * g_l` (the phase convention
//! `b - <a, s'>` treats the body as a coefficient with key `-1`). The
//! switch decomposes each mask coefficient and the body with the exact
//! covering gadget decomposition and accumulates digit-times-key-entry, so
//!
//! ```text
//! phase(out) = sum_i a_i * f(s'_i) + b * f(-1)
//! ```
//!
//! With `f(x) = -x`: `phase = b - <a, s'> = phi` (the b-column). With
//! `f(x) = x * s'(X)`: `phase = <a, s'> * s' - b * s' = -phi * s'` (the
//! a-column).
//!
//! # Cost and noise
//!
//! `BS_ELL` blind rotations plus `2 * BS_ELL` private key switches per
//! converted wire. Output noise is private-switch noise (digits times key
//! noise), which a subsequent external product multiplies by gadget
//! digits; the plan's failure-budget accounting tracks this, and any
//! `std128`-scale circuit-bootstrap claim waits for the plan §9 gate.

use crate::SpecRng;
use crate::binfhe::blind_rotate::binfhe_blind_rotate;
use crate::binfhe::gadget;
use crate::binfhe::keys::{BinfheBootstrappingKey, binfhe_gen_bootstrapping_key};
use crate::binfhe::lwe::{BinfheLweCiphertext, BinfheLweSecretKey, binfhe_lwe_add_const, wire_delta};
use crate::binfhe::rgsw::{BinfheRgswCiphertext, BinfheRgswRow};
use crate::binfhe::rlwe::{BinfheRlweCiphertext, BinfheRlweSecretKey, binfhe_rlwe_encrypt_poly, binfhe_sample_extract};
use crate::binfhe::torus;

/// Private key-switching key for circuit bootstrapping.
/// @volar-allow-vec: eval-key-store: per-column RLWE rows are large
/// (BIG_N * PRIV_ELL ciphertexts) and heap-held for native execution.
#[derive(Clone, Debug)]
pub struct PrivateKeySwitchingKey<const BIG_N: usize, const PRIV_ELL: usize> {
    /// a-column entries: `a_col[i][l]` encrypts `s'_i * s'(X) * g_l`
    /// (zero when `s'_i = 0`).
    pub a_col: alloc::vec::Vec<[BinfheRlweCiphertext<BIG_N>; PRIV_ELL]>,
    /// b-column entries: `b_col[i][l]` encrypts the constant `-s'_i * g_l`.
    pub b_col: alloc::vec::Vec<[BinfheRlweCiphertext<BIG_N>; PRIV_ELL]>,
    /// a-column body terms: encrypt `-s'(X) * g_l`.
    pub a_body: [BinfheRlweCiphertext<BIG_N>; PRIV_ELL],
    /// b-column body terms: encrypt the constant `g_l`.
    pub b_body: [BinfheRlweCiphertext<BIG_N>; PRIV_ELL],
}

/// The complete circuit-bootstrapping evaluation key: the ordinary
/// bootstrapping key (drives the per-level extractions) plus the private
/// key-switching key.
#[derive(Clone, Debug)]
pub struct CircuitBootstrappingKey<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
    const PRIV_ELL: usize,
> {
    pub bk: BinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
    pub privksk: PrivateKeySwitchingKey<BIG_N, PRIV_ELL>,
}

/// Encrypt `msg * g_l` under the ring key (helper for keygen).
fn encrypt_scaled_poly<const BIG_N: usize, const LOG_Q: u32, const ETA: u32, R: SpecRng>(
    msg: &[u32; BIG_N],
    level: usize,
    base_log: u32,
    sk: &BinfheRlweSecretKey<BIG_N>,
    rng: &mut R,
) -> BinfheRlweCiphertext<BIG_N> {
    let g = gadget::level_factor::<LOG_Q>(base_log, level);
    let mut scaled = [0u32; BIG_N];
    for i in 0..BIG_N {
        scaled[i] = torus::mul_exact::<LOG_Q>(msg[i], g);
    }
    binfhe_rlwe_encrypt_poly::<BIG_N, LOG_Q, ETA, R>(&scaled, sk, rng)
}

/// Generate the circuit-bootstrapping key.
pub fn gen_circuit_bootstrapping_key<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const PRIV_ELL: usize,
    const PRIV_BASE_LOG: u32,
    const ETA: u32,
    R: SpecRng,
>(
    lwe_sk: &BinfheLweSecretKey<N_LWE>,
    rlwe_sk: &BinfheRlweSecretKey<BIG_N>,
    rng: &mut R,
) -> CircuitBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, PRIV_ELL> {
    let bk = binfhe_gen_bootstrapping_key::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, ETA, R,
    >(lwe_sk, rlwe_sk, rng);

    let zero = [0u32; BIG_N];
    let neg_one_const: [u32; BIG_N] = {
        let mut p = [0u32; BIG_N];
        p[0] = torus::torus_neg::<LOG_Q>(1);
        p
    };
    let neg_sk: [u32; BIG_N] = core::array::from_fn(|i| torus::torus_neg::<LOG_Q>(rlwe_sk.key[i]));
    let one_const: [u32; BIG_N] = {
        let mut p = [0u32; BIG_N];
        p[0] = 1;
        p
    };

    // Column families are heap-allocated: at Std128 dimensions the pair is
    // tens of MB and cannot live on a test thread's stack (plan §10 M8).
    let mut a_col = alloc::vec::Vec::with_capacity(BIG_N);
    for i in 0..BIG_N {
        // f(s'_i) = s'_i * s'(X).
        let msg = if rlwe_sk.key[i] == 1 { &rlwe_sk.key } else { &zero };
        a_col.push(core::array::from_fn(|l| {
            encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(msg, l, PRIV_BASE_LOG, rlwe_sk, rng)
        }));
    }
    let mut b_col = alloc::vec::Vec::with_capacity(BIG_N);
    for i in 0..BIG_N {
        // f(s'_i) = -s'_i (scalar).
        let msg = if rlwe_sk.key[i] == 1 { &neg_one_const } else { &zero };
        b_col.push(core::array::from_fn(|l| {
            encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(msg, l, PRIV_BASE_LOG, rlwe_sk, rng)
        }));
    }
    let a_body = core::array::from_fn(|l| {
        encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(&neg_sk, l, PRIV_BASE_LOG, rlwe_sk, rng)
    });
    let b_body = core::array::from_fn(|l| {
        encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(&one_const, l, PRIV_BASE_LOG, rlwe_sk, rng)
    });

    CircuitBootstrappingKey {
        bk,
        privksk: PrivateKeySwitchingKey {
            a_col,
            b_col,
            a_body,
            b_body,
        },
    }
}

/// Private key switch on one column family: gadget_decompose each source mask
/// coefficient and the body, accumulate digit-times-entry.
fn priv_ks<const BIG_N: usize, const LOG_Q: u32, const PRIV_ELL: usize, const PRIV_BASE_LOG: u32>(
    src: &BinfheLweCiphertext<BIG_N>,
    col: &[[BinfheRlweCiphertext<BIG_N>; PRIV_ELL]],
    body: &[BinfheRlweCiphertext<BIG_N>; PRIV_ELL],
) -> BinfheRlweCiphertext<BIG_N> {
    let mut out = BinfheRlweCiphertext {
        a: [0u32; BIG_N],
        b: [0u32; BIG_N],
    };
    for i in 0..BIG_N {
        let digits = gadget::gadget_decompose::<LOG_Q, PRIV_ELL, PRIV_BASE_LOG>(src.a[i]);
        for (l, &d) in digits.iter().enumerate() {
            if d == 0 {
                continue;
            }
            let entry = &col[i][l];
            for k in 0..BIG_N {
                out.a[k] = out.a[k].wrapping_add(d.wrapping_mul(entry.a[k]));
                out.b[k] = out.b[k].wrapping_add(d.wrapping_mul(entry.b[k]));
            }
        }
    }
    let digits = gadget::gadget_decompose::<LOG_Q, PRIV_ELL, PRIV_BASE_LOG>(src.b);
    for (l, &d) in digits.iter().enumerate() {
        if d == 0 {
            continue;
        }
        let entry = &body[l];
        for k in 0..BIG_N {
            out.a[k] = out.a[k].wrapping_add(d.wrapping_mul(entry.a[k]));
            out.b[k] = out.b[k].wrapping_add(d.wrapping_mul(entry.b[k]));
        }
    }
    for k in 0..BIG_N {
        out.a[k] = torus::reduce::<LOG_Q>(out.a[k]);
        out.b[k] = torus::reduce::<LOG_Q>(out.b[k]);
    }
    out
}

/// Level-`j` test polynomial: bins `[0, Delta_pos) -> 0`,
/// `[Delta_pos, 2*Delta_pos) -> g_j`, zero elsewhere (an in-budget selector
/// phase never leaves `[0, 2*Delta_pos)`).
fn level_test_poly<const BIG_N: usize, const LOG_Q: u32>(
    level: usize,
    bs_base_log: u32,
    k_max: usize,
) -> [u32; BIG_N] {
    let width = BIG_N >> k_max;
    let g = gadget::level_factor::<LOG_Q>(bs_base_log, level);
    let mut poly = [0u32; BIG_N];
    for p in width..(2 * width) {
        poly[p] = g;
    }
    poly
}

/// Circuit bootstrap: convert a Boolean wire (LWE under the small key,
/// phase `{0, Delta}` at `q`, `Delta = q / 2^(K_MAX+1)`) into an RGSW
/// ciphertext of the same bit under the ring key.
pub fn circuit_bootstrap<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const PRIV_ELL: usize,
    const PRIV_BASE_LOG: u32,
>(
    ct: &BinfheLweCiphertext<N_LWE>,
    cbk: &CircuitBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, PRIV_ELL>,
    k_max: usize,
) -> BinfheRgswCiphertext<BIG_N, BS_ELL> {
    let delta = wire_delta::<LOG_Q_LWE>(k_max as usize);
    // Center the wire's bin: phase becomes m * Delta + Delta/2.
    let centered = binfhe_lwe_add_const::<N_LWE, LOG_Q_LWE>(ct, delta / 2);
    let rows = core::array::from_fn(|j| {
        let test_poly = level_test_poly::<BIG_N, LOG_Q>(j, BS_BASE_LOG, k_max);
        let acc = binfhe_blind_rotate::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG>(
            &centered,
            &test_poly,
            &cbk.bk.bsk,
        );
        // LWE_N at ring modulus with phase m * g_j under the ring key.
        let extracted = binfhe_sample_extract::<BIG_N, LOG_Q>(&acc);
        let rlwe0 = priv_ks::<BIG_N, LOG_Q, PRIV_ELL, PRIV_BASE_LOG>(
            &extracted,
            &cbk.privksk.a_col,
            &cbk.privksk.a_body,
        );
        let rlwe1 = priv_ks::<BIG_N, LOG_Q, PRIV_ELL, PRIV_BASE_LOG>(
            &extracted,
            &cbk.privksk.b_col,
            &cbk.privksk.b_body,
        );
        BinfheRgswRow { rlwe0, rlwe1 }
    });
    BinfheRgswCiphertext { rows }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::lwe::{binfhe_gen_lwe_secret_key, binfhe_lwe_encrypt};
    use crate::binfhe::params::toy;
    use crate::binfhe::rgsw::{binfhe_rgsw_cmux, binfhe_external_product};
    use crate::binfhe::rlwe::{binfhe_gen_rlwe_secret_key, binfhe_rlwe_trivial};

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

    type ToyCbk = CircuitBootstrappingKey<
        { toy::N_LWE },
        { toy::BIG_N },
        { toy::BS_ELL },
        { toy::KS_ELL },
        { toy::PRIV_ELL },
    >;

    fn toy_cb_keys(seed: u64) -> (crate::binfhe::lwe::BinfheLweSecretKey<{ toy::N_LWE }>, BinfheRlweSecretKey<{ toy::BIG_N }>, ToyCbk) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = binfhe_gen_lwe_secret_key(&mut rng);
        let rlwe_sk = binfhe_gen_rlwe_secret_key(&mut rng);
        let cbk = gen_circuit_bootstrapping_key::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
            { toy::CBD_ETA }, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        (lwe_sk, rlwe_sk, cbk)
    }

    /// Independent per-coefficient phase model (shares no code with
    /// `binfhe_rlwe_phase`).
    fn clear_phase(ct: &BinfheRlweCiphertext<{ toy::BIG_N }>, key: &[u32; toy::BIG_N]) -> [u32; toy::BIG_N] {
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
            phase[i] = ct.b[i].wrapping_sub(product) & 0x7F;
        }
        phase
    }

    #[test]
    fn circuit_bootstrap_rows_satisfy_the_rgsw_invariant() {
        let (lwe_sk, rlwe_sk, cbk) = toy_cb_keys(0xCB01);
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(2);
        for m in [false, true] {
            let mut rng = TestRng::new(42 + m as u64);
            let wire = binfhe_lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(
                m, delta, &lwe_sk, &mut rng,
            );
            let rgsw = circuit_bootstrap::<
                { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                { toy::BS_ELL }, { toy::BS_BASE_LOG }, { toy::KS_ELL },
                { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
            >(&wire, &cbk, 2);
            // Row invariant: phase(rlwe0_j) = -m * g_j * s'(X),
            // phase(rlwe1_j) = m * g_j (constant).
            for (j, row) in rgsw.rows.iter().enumerate() {
                let g = gadget::level_factor::<{ toy::LOG_Q }>(toy::BS_BASE_LOG, j);
                let p0 = clear_phase(&row.rlwe0, &rlwe_sk.key);
                let p1 = clear_phase(&row.rlwe1, &rlwe_sk.key);
                for i in 0..toy::BIG_N {
                    let expect0 = if m { torus::torus_neg::<7>(g.wrapping_mul(rlwe_sk.key[i])) } else { 0 };
                    assert_eq!(p0[i], expect0, "a-column level {j} coeff {i}, m={m}");
                    let expect1 = if m && i == 0 { g } else { 0 };
                    assert_eq!(p1[i], expect1, "b-column level {j} coeff {i}, m={m}");
                }
            }
        }
    }

    #[test]
    fn circuit_bootstrapped_rgsw_drives_external_products() {
        let (lwe_sk, rlwe_sk, cbk) = toy_cb_keys(0xCB02);
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(2);
        // Arbitrary content polynomial as a trivial RLWE.
        let mut content = binfhe_rlwe_trivial::<{ toy::BIG_N }, 7>(&[0u32; toy::BIG_N]);
        for i in 0..toy::BIG_N {
            content.b[i] = (i as u32 * 11 + 5) & 0x7F;
        }
        for m in [false, true] {
            let mut rng = TestRng::new(77 + m as u64);
            let wire = binfhe_lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(
                m, delta, &lwe_sk, &mut rng,
            );
            let rgsw = circuit_bootstrap::<
                { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                { toy::BS_ELL }, { toy::BS_BASE_LOG }, { toy::KS_ELL },
                { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
            >(&wire, &cbk, 2);
            let out = binfhe_external_product::<
                { toy::BIG_N }, 7, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            >(&rgsw, &content);
            let phase = clear_phase(&out, &rlwe_sk.key);
            for i in 0..toy::BIG_N {
                let expected = if m { content.b[i] } else { 0 };
                assert_eq!(phase[i], expected, "external product coeff {i}, m={m}");
            }
        }
    }

    #[test]
    fn circuit_bootstrapped_cmux_tree_selects() {
        // 2-level oblivious select over 4 RLWE contents with two CB'd
        // address bits; compare against plaintext selection.
        let (lwe_sk, rlwe_sk, cbk) = toy_cb_keys(0xCB03);
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(2);
        let mut contents = alloc::vec::Vec::new();
        for c in 0..4u32 {
            let mut ct = binfhe_rlwe_trivial::<{ toy::BIG_N }, 7>(&[0u32; toy::BIG_N]);
            for i in 0..toy::BIG_N {
                ct.b[i] = ((i as u32 * 7) ^ (c * 31)) & 0x7F;
            }
            contents.push(ct);
        }
        for addr in 0..4usize {
            let mut rng = TestRng::new(900 + addr as u64);
            let mut select = |bit: bool| {
                let wire = binfhe_lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(
                    bit, delta, &lwe_sk, &mut rng,
                );
                circuit_bootstrap::<
                    { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                    { toy::BS_ELL }, { toy::BS_BASE_LOG }, { toy::KS_ELL },
                    { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
                >(&wire, &cbk, 2)
            };
            let s0 = select(addr & 1 == 1);
            let s1 = select(addr & 2 == 2);
            let lo = binfhe_rgsw_cmux::<{ toy::BIG_N }, 7, { toy::BS_ELL }, { toy::BS_BASE_LOG }>(
                &s0, &contents[1], &contents[0],
            );
            let hi = binfhe_rgsw_cmux::<{ toy::BIG_N }, 7, { toy::BS_ELL }, { toy::BS_BASE_LOG }>(
                &s0, &contents[3], &contents[2],
            );
            let out = binfhe_rgsw_cmux::<{ toy::BIG_N }, 7, { toy::BS_ELL }, { toy::BS_BASE_LOG }>(
                &s1, &hi, &lo,
            );
            let phase = clear_phase(&out, &rlwe_sk.key);
            for i in 0..toy::BIG_N {
                assert_eq!(phase[i], contents[addr].b[i], "addr {addr} coeff {i}");
            }
        }
    }

    /// §9 evidence: the Std128 circuit-bootstrap path currently fails its
    /// selection budget.
    ///
    /// This is the first Std128-scale CB differential (M8's smoke was
    /// wire-only; CB was previously exercised only at the noiseless `toy`
    /// profile, which cannot fail). The selector/CMUX output at Std128
    /// (real `CBD_ETA = 16` noise, `q = 2048`) does not meet the decode
    /// margin, mirroring the noise-budget suite's arity-3 finding at
    /// `q = 128`. Ignored by default and expected to fail until the §9
    /// failure recomputation drives a parameter/budget fix; a green run is
    /// the signal that the CB path meets its recorded budget.
    #[test]
    #[ignore = "§9 evidence: Std128 CB is over its selection budget; see doc comment"]
    fn std128_circuit_bootstrapped_cmux_selects() {
        use crate::binfhe::params::std128;
        let mut rng = TestRng::new(0xCB128);
        let lwe_sk = binfhe_gen_lwe_secret_key::<{ std128::N_LWE }, _>(&mut rng);
        let rlwe_sk = binfhe_gen_rlwe_secret_key::<{ std128::BIG_N }, _>(&mut rng);
        let cbk = gen_circuit_bootstrapping_key::<
            { std128::N_LWE }, { std128::BIG_N }, { std128::LOG_Q },
            { std128::LOG_Q_LWE }, { std128::LOG_MOD_KS }, { std128::BS_ELL },
            { std128::BS_BASE_LOG }, { std128::KS_ELL }, { std128::KS_BASE_LOG },
            { std128::PRIV_ELL }, { std128::PRIV_BASE_LOG }, { std128::CBD_ETA }, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        let delta = wire_delta::<{ std128::LOG_Q_LWE }>(2);
        let mut content0 = binfhe_rlwe_trivial::<{ std128::BIG_N }, { std128::LOG_Q }>(
            &[0u32; std128::BIG_N],
        );
        let mut content1 = binfhe_rlwe_trivial::<{ std128::BIG_N }, { std128::LOG_Q }>(
            &[0u32; std128::BIG_N],
        );
        for i in 0..std128::BIG_N {
            content0.b[i] = (i as u32 * 3 + 1) & 0x7F;
            content1.b[i] = (i as u32 * 5 + 2) & 0x7F;
        }
        for m in [false, true] {
            let mut rng = TestRng::new(0xCB200 + m as u64);
            let wire = binfhe_lwe_encrypt::<
                { std128::N_LWE }, { std128::LOG_Q_LWE }, { std128::CBD_ETA }, _,
            >(m, delta, &lwe_sk, &mut rng);
            let rgsw = circuit_bootstrap::<
                { std128::N_LWE }, { std128::BIG_N }, { std128::LOG_Q },
                { std128::LOG_Q_LWE }, { std128::BS_ELL }, { std128::BS_BASE_LOG },
                { std128::KS_ELL }, { std128::PRIV_ELL }, { std128::PRIV_BASE_LOG },
            >(&wire, &cbk, 2);
            let out = crate::binfhe::rgsw::binfhe_rgsw_cmux::<
                { std128::BIG_N }, { std128::LOG_Q }, { std128::BS_ELL }, { std128::BS_BASE_LOG },
            >(&rgsw, &content1, &content0);
            let expected = if m { &content1 } else { &content0 };
            for i in 0..std128::BIG_N {
                let phase = crate::binfhe::rlwe::binfhe_rlwe_phase::<{ std128::BIG_N }, { std128::LOG_Q }>(
                    &out, &rlwe_sk,
                );
                assert_eq!(phase[i], expected.b[i], "coeff {i}, m={m}");
            }
        }
    }

    #[test]
    fn keygen_is_deterministic() {
        let (_, _, a) = toy_cb_keys(0xCB04);
        let (_, _, b) = toy_cb_keys(0xCB04);
        assert_eq!(a.privksk.b_body[0].b, b.privksk.b_body[0].b);
        assert_eq!(a.bk.ksk.ksk[0][0], b.bk.ksk.ksk[0][0]);
    }
}
