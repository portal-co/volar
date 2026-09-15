// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Programmable bootstrapping and the Boolean gate set.
//!
//! The full bootstrap pipeline (plan §3):
//!
//! ```text
//! LWE_n(q) --blind rotate-->  RLWE(Q) accumulator
//!          --sample extract--> LWE_N(Q)
//!          --mod switch-->      LWE_N(modKS)
//!          --key switch-->      LWE_n(modKS)
//!          --mod switch-->      LWE_n(q), canonical {0, Delta} wire
//! ```
//!
//! All Boolean gates are LUT evaluations ([`binfhe_lut_read`]) over wires
//! encoded at `Delta = q / 2^(K_MAX+1)`; see [`crate::binfhe::lut`] for the
//! selector and table construction. Every output is refreshed to the
//! canonical encoding, so outputs compose as inputs to further gates.
//!
//! With the noiseless `toy` profile every stage is exact and the output
//! phase is *exactly* `{0, Delta}`; the tests assert that canonical-phase
//! property, not just decode correctness.

use crate::binfhe::blind_rotate::blind_rotate;
use crate::binfhe::keys::{BootstrappingKey, key_switch};
use crate::binfhe::lut::{Lut, fill_test_poly, table_is_constant};
use crate::binfhe::lwe::{
    LweCiphertext, binfhe_trivial, lwe_add, lwe_add_const, lwe_scale, wire_delta,
};
use crate::binfhe::modswitch::mod_switch_lwe;
use crate::binfhe::rlwe::sample_extract;

/// Programmable bootstrap of one LWE ciphertext with an explicit test
/// polynomial (ring-modulus scale). This is the fixed-polynomial
/// primitive; Boolean semantics enter through [`binfhe_lut_read`].
pub fn binfhe_pbs_core<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
>(
    ct: &LweCiphertext<N_LWE>,
    test_poly: &[u32; BIG_N],
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let acc = blind_rotate::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG>(
        ct, test_poly, &bk.bsk,
    );
    let extracted = sample_extract::<BIG_N, LOG_Q>(&acc);
    let at_ks = mod_switch_lwe::<BIG_N, LOG_Q, LOG_MOD_KS>(&extracted);
    let switched = key_switch::<N_LWE, BIG_N, LOG_MOD_KS, KS_ELL, KS_BASE_LOG>(&at_ks, &bk.ksk);
    mod_switch_lwe::<N_LWE, LOG_MOD_KS, LOG_Q_LWE>(&switched)
}

/// Read a validated table at an encrypted address: one programmable
/// bootstrap evaluating `lut` on the encrypted address bits (LSB first).
///
/// The selector is `sum_j 2^j * addr_j + Delta/2` — exact integer weights,
/// computed by ciphertext-linear scaling. The output is a canonical wire at
/// the shared encoding `Delta = q / 2^(K_MAX+1)`.
pub fn binfhe_lut_read<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const ADDR_BITS: usize,
    const TABLE_LEN: usize,
    const K_MAX: usize,
>(
    addr: &[LweCiphertext<N_LWE>; ADDR_BITS],
    lut: &Lut<ADDR_BITS, TABLE_LEN, BIG_N, LOG_Q, LOG_Q_LWE, K_MAX>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let delta = wire_delta::<LOG_Q_LWE>(K_MAX);
    if lut.is_constant() {
        return binfhe_trivial::<N_LWE, LOG_Q_LWE>(lut.constant_value(), delta);
    }
    // combined = sum_j 2^j * addr_j, then center the bins.
    let mut combined = binfhe_trivial::<N_LWE, LOG_Q_LWE>(false, 0);
    for (j, bit) in addr.iter().enumerate() {
        let scaled = lwe_scale::<N_LWE, LOG_Q_LWE>(bit, 1u32 << j);
        combined = lwe_add::<N_LWE, LOG_Q_LWE>(&combined, &scaled);
    }
    combined = lwe_add_const::<N_LWE, LOG_Q_LWE>(&combined, delta / 2);
    binfhe_pbs_core::<
        N_LWE,
        BIG_N,
        LOG_Q,
        LOG_Q_LWE,
        LOG_MOD_KS,
        BS_ELL,
        BS_BASE_LOG,
        KS_ELL,
        KS_BASE_LOG,
    >(&combined, lut.test_polynomial(), bk)
}

/// Runtime-table LUT read: the shared executor for the plan interpreter
/// ([`crate::binfhe::plan::execute_plan`]) and weaver-generated code.
///
/// Same semantics as [`binfhe_lut_read`] but with the table as a runtime
/// slice (length `2^k`, `k = inputs.len()`); `k_max` is the circuit-wide
/// maximum arity fixing the wire encoding. Panics if the shape is invalid —
/// callers validate with [`check_lut_shape`](crate::binfhe::lut::check_lut_shape)
/// first (the plan's `validate()` does).
pub fn binfhe_lut_read_dyn<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
>(
    inputs: &[LweCiphertext<N_LWE>],
    table: &[bool],
    k_max: usize,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let delta = wire_delta::<LOG_Q_LWE>(k_max as usize);
    if table_is_constant(table) {
        return binfhe_trivial::<N_LWE, LOG_Q_LWE>(table[0], delta);
    }
    let arity = table.len().trailing_zeros() as usize;
    assert_eq!(inputs.len(), arity, "LUT arity must match the table");
    let test_poly = fill_test_poly::<BIG_N>(
        table,
        arity,
        k_max as usize,
        LOG_Q,
        LOG_Q_LWE,
    );
    let mut combined = binfhe_trivial::<N_LWE, LOG_Q_LWE>(false, 0);
    for (j, bit) in inputs.iter().enumerate() {
        let scaled = lwe_scale::<N_LWE, LOG_Q_LWE>(bit, 1u32 << j);
        combined = lwe_add::<N_LWE, LOG_Q_LWE>(&combined, &scaled);
    }
    combined = lwe_add_const::<N_LWE, LOG_Q_LWE>(&combined, delta / 2);
    binfhe_pbs_core::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
    >(&combined, &test_poly, bk)
}

/// Macro-generating gate wrappers would obscure the const wiring; each
/// gate is written out with its associated-constant table.

/// AND gate: one programmable bootstrap.
pub fn binfhe_gate_and<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const K_MAX: usize,
>(
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    binfhe_lut_read_dyn::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
    >(&[a, b], &[false, false, false, true], K_MAX, bk)
}

/// OR gate: one programmable bootstrap.
pub fn binfhe_gate_or<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const K_MAX: usize,
>(
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    binfhe_lut_read_dyn::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
    >(&[a, b], &[false, true, true, true], K_MAX, bk)
}

/// XOR gate: one programmable bootstrap (unlike the legacy raw linear XOR,
/// the output is a refreshed canonical wire and composes freely).
pub fn binfhe_gate_xor<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const K_MAX: usize,
>(
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    binfhe_lut_read_dyn::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
    >(&[a, b], &[false, true, true, false], K_MAX, bk)
}

/// CMUX (oblivious select) as a 3-input LUT: `sel ? a : b` with
/// `addr = sel + 2*a + 4*b`. One programmable bootstrap. Requires
/// `K_MAX >= 3`.
pub fn binfhe_cmux<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const K_MAX: usize,
>(
    sel: LweCiphertext<N_LWE>,
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    // addr = sel + 2a + 4b; f(sel,a,b) = sel ? a : b.
    const TABLE: [bool; 8] = [false, false, false, true, true, false, true, true];
    binfhe_lut_read_dyn::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
    >(&[sel, a, b], &TABLE, K_MAX, bk)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::keys::gen_bootstrapping_key;
    use crate::binfhe::lwe::{
        LweSecretKey, binfhe_not, gen_lwe_secret_key, lwe_decrypt, lwe_encrypt, lwe_phase,
    };
    use crate::binfhe::params::{toy, toy_noisy};
    use crate::binfhe::rlwe::{RlweSecretKey, gen_rlwe_secret_key};
    use crate::SpecRng;
    use alloc::vec::Vec;

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

    type ToyBk = BootstrappingKey<{ toy::N_LWE }, { toy::BIG_N }, { toy::BS_ELL }, { toy::KS_ELL }>;

    fn toy_keys(seed: u64) -> (LweSecretKey<{ toy::N_LWE }>, ToyBk) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = gen_lwe_secret_key(&mut rng);
        let rlwe_sk: RlweSecretKey<{ toy::BIG_N }> = gen_rlwe_secret_key(&mut rng);
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
        (lwe_sk, bk)
    }

    fn toy_encrypt(
        m: bool,
        k_max: u32,
        sk: &LweSecretKey<{ toy::N_LWE }>,
        seed: u64,
    ) -> LweCiphertext<{ toy::N_LWE }> {
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(k_max as usize);
        let mut rng = TestRng::new(seed);
        lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(m, delta, sk, &mut rng)
    }

    /// Assert canonical phase AND decode (toy is exact).
    fn assert_wire(
        ct: &LweCiphertext<{ toy::N_LWE }>,
        expected: bool,
        k_max: u32,
        sk: &LweSecretKey<{ toy::N_LWE }>,
        context: &str,
    ) {
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(k_max as usize);
        assert_eq!(
            lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(ct, sk),
            if expected { delta } else { 0 },
            "{context}: canonical phase"
        );
        assert_eq!(
            lwe_decrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(ct, sk, delta),
            expected,
            "{context}: decode"
        );
    }

    #[test]
    fn gate_truth_tables_and_canonical_phases() {
        for key_seed in [0xBEEF, 1, 42] {
            let (sk, bk) = toy_keys(key_seed);
            for a in [false, true] {
                for b in [false, true] {
                    let ca = toy_encrypt(a, 2, &sk, 100 + a as u64 + key_seed);
                    let cb = toy_encrypt(b, 2, &sk, 200 + b as u64 + key_seed);
                    assert_wire(
                        &binfhe_gate_and::<
                            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 2,
                        >(ca, cb, &bk),
                        a && b, 2, &sk, "AND",
                    );
                    assert_wire(
                        &binfhe_gate_or::<
                            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 2,
                        >(ca, cb, &bk),
                        a || b, 2, &sk, "OR",
                    );
                    assert_wire(
                        &binfhe_gate_xor::<
                            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 2,
                        >(ca, cb, &bk),
                        a ^ b, 2, &sk, "XOR",
                    );
                }
            }
        }
    }

    #[test]
    fn one_bit_luts_at_classic_quarter_encoding() {
        // K_MAX = 1 -> Delta = q/4, the classic {0, q/4} wire encoding.
        let (sk, bk) = toy_keys(0x1D);
        type L1 = Lut<1, 2, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 1>;
        let identity = L1::IDENTITY.unwrap();
        let not = L1::NOT.unwrap();
        for m in [false, true] {
            let ct = toy_encrypt(m, 1, &sk, 300 + m as u64);
            assert_wire(
                &binfhe_lut_read::<
                    { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                    { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                    { toy::KS_ELL }, { toy::KS_BASE_LOG }, 1, 2, 1,
                >(&[ct], &identity, &bk),
                m, 1, &sk, "identity PBS",
            );
            assert_wire(
                &binfhe_lut_read::<
                    { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                    { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                    { toy::KS_ELL }, { toy::KS_BASE_LOG }, 1, 2, 1,
                >(&[ct], &not, &bk),
                !m, 1, &sk, "NOT PBS",
            );
        }
    }

    #[test]
    fn multi_input_lut_matches_brute_force() {
        // 4-input arbitrary table on toy (K_MAX = 4).
        let (sk, bk) = toy_keys(0x44);
        let mut table = [false; 16];
        for (i, e) in table.iter_mut().enumerate() {
            *e = (i * 5 + 1) % 7 < 3;
        }
        let lut =
            Lut::<4, 16, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 4>::new(table)
                .unwrap();
        for addr in 0..16usize {
            let bits: [LweCiphertext<{ toy::N_LWE }>; 4] = core::array::from_fn(|j| {
                toy_encrypt((addr >> j) & 1 == 1, 4, &sk, 400 + addr as u64 * 8 + j as u64)
            });
            assert_wire(
                &binfhe_lut_read::<
                    { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                    { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                    { toy::KS_ELL }, { toy::KS_BASE_LOG }, 4, 16, 4,
                >(&bits, &lut, &bk),
                table[addr], 4, &sk, "4-input LUT",
            );
        }
    }

    #[test]
    fn cmux_lut_selects() {
        let (sk, bk) = toy_keys(0xC5);
        for sel in [false, true] {
            for a in [false, true] {
                for b in [false, true] {
                    let cs = toy_encrypt(sel, 3, &sk, 501 + sel as u64);
                    let ca = toy_encrypt(a, 3, &sk, 502 + a as u64);
                    let cb = toy_encrypt(b, 3, &sk, 503 + b as u64);
                    assert_wire(
                        &binfhe_cmux::<
                            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 3,
                        >(cs, ca, cb, &bk),
                        if sel { a } else { b },
                        3, &sk, "CMUX",
                    );
                }
            }
        }
    }

    #[test]
    fn constant_lut_is_free_and_exact() {
        let (sk, bk) = toy_keys(0xC05);
        let c = Lut::<2, 4, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 2>::new(
            [true, true, true, true],
        )
        .unwrap();
        let ca = toy_encrypt(true, 2, &sk, 601);
        let cb = toy_encrypt(false, 2, &sk, 602);
        let out = binfhe_lut_read::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 2, 4, 2,
        >(&[ca, cb], &c, &bk);
        // Constant path returns a trivial ciphertext: mask is all zero.
        assert!(out.a.iter().all(|&x| x == 0));
        assert_wire(&out, true, 2, &sk, "constant LUT");
    }

    #[test]
    fn composable_gate_dags_stay_canonical() {
        // A fixed pseudo-random DAG over AND/OR/XOR/NOT/CMUX, checked at
        // every node for exact canonical phase (zero-noise toy).
        let (sk, bk) = toy_keys(0xDA6);
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(3);
        let inputs = [false, true, false, true];
        let mut wires: Vec<bool> = inputs.to_vec();
        let mut cts: Vec<LweCiphertext<{ toy::N_LWE }>> = inputs
            .iter()
            .enumerate()
            .map(|(i, &v)| toy_encrypt(v, 3, &sk, 700 + i as u64))
            .collect();
        // Deterministic pseudo-random op sequence (LCG).
        let mut state = 0x1234_5678u64;
        let mut next = || {
            state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            (state >> 33) as usize
        };
        for _ in 0..24 {
            let n = wires.len();
            let op = next() % 5;
            let i = next() % n;
            let j = next() % n;
            let (value, ct) = match op {
                0 => {
                    let k = next() % n;
                    (
                        if wires[i] { wires[j] } else { wires[k] },
                        binfhe_cmux::<
                            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 3,
                        >(cts[i], cts[j], cts[k], &bk),
                    )
                }
                1 => (
                    wires[i] && wires[j],
                    binfhe_gate_and::<
                        { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                        { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                        { toy::KS_ELL }, { toy::KS_BASE_LOG }, 3,
                    >(cts[i], cts[j], &bk),
                ),
                2 => (
                    wires[i] || wires[j],
                    binfhe_gate_or::<
                        { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                        { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                        { toy::KS_ELL }, { toy::KS_BASE_LOG }, 3,
                    >(cts[i], cts[j], &bk),
                ),
                3 => (
                    wires[i] ^ wires[j],
                    binfhe_gate_xor::<
                        { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                        { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                        { toy::KS_ELL }, { toy::KS_BASE_LOG }, 3,
                    >(cts[i], cts[j], &bk),
                ),
                _ => (
                    !wires[i],
                    binfhe_not::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(&cts[i], delta),
                ),
            };
            wires.push(value);
            cts.push(ct);
        }
        for (i, (&expected, ct)) in wires.iter().zip(cts.iter()).enumerate() {
            assert_eq!(
                lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(ct, &sk),
                if expected { delta } else { 0 },
                "DAG node {i} canonical phase"
            );
        }
    }

    #[test]
    fn noisy_pipeline_decodes_with_margin() {
        // toy_noisy exercises the real noise path: nonzero CBD noise in the
        // BSK/KSK/fresh encryptions, both modulus switches. The profile is
        // sized to be quiet; over this fixed deterministic corpus every
        // bootstrap must succeed and the *observed* output phase error must
        // stay well under Delta/2 = 8 (q=128, K=2).
        type NBk = BootstrappingKey<
            { toy_noisy::N_LWE },
            { toy_noisy::BIG_N },
            { toy_noisy::BS_ELL },
            { toy_noisy::KS_ELL },
        >;
        let mut rng = TestRng::new(0x9015);
        let lwe_sk = gen_lwe_secret_key::<{ toy_noisy::N_LWE }, _>(&mut rng);
        let rlwe_sk = gen_rlwe_secret_key::<{ toy_noisy::BIG_N }, _>(&mut rng);
        let bk: NBk = gen_bootstrapping_key::<
            { toy_noisy::N_LWE },
            { toy_noisy::BIG_N },
            { toy_noisy::LOG_Q },
            { toy_noisy::LOG_Q_LWE },
            { toy_noisy::LOG_MOD_KS },
            { toy_noisy::BS_ELL },
            { toy_noisy::BS_BASE_LOG },
            { toy_noisy::KS_ELL },
            { toy_noisy::KS_BASE_LOG },
            { toy_noisy::CBD_ETA },
            _,
        >(&lwe_sk, &rlwe_sk, &mut rng);

        let delta = wire_delta::<{ toy_noisy::LOG_Q_LWE }>(2);
        let mut max_err = 0u32;
        for seed in 0..32u64 {
            let a = seed % 2 == 0;
            let b = seed % 3 == 0;
            let mut ra = TestRng::new(1000 + seed);
            let mut rb = TestRng::new(2000 + seed);
            let ca = lwe_encrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }, { toy_noisy::CBD_ETA }, _>(
                a, delta, &lwe_sk, &mut ra,
            );
            let cb = lwe_encrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }, { toy_noisy::CBD_ETA }, _>(
                b, delta, &lwe_sk, &mut rb,
            );
            let out = binfhe_gate_and::<
                { toy_noisy::N_LWE }, { toy_noisy::BIG_N }, { toy_noisy::LOG_Q },
                { toy_noisy::LOG_Q_LWE }, { toy_noisy::LOG_MOD_KS }, { toy_noisy::BS_ELL },
                { toy_noisy::BS_BASE_LOG }, { toy_noisy::KS_ELL }, { toy_noisy::KS_BASE_LOG }, 2,
            >(ca, cb, &bk);
            let phase = lwe_phase::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }>(&out, &lwe_sk);
            let expected = if a && b { delta } else { 0 };
            let err = phase.wrapping_sub(expected) & 0x7F;
            let err = err.min((-((err as i32))).rem_euclid(128) as u32);
            max_err = max_err.max(err);
            assert_eq!(
                lwe_decrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }>(&out, &lwe_sk, delta),
                a && b,
                "noisy AND seed {seed}"
            );
        }
        assert!(max_err < 8, "observed max output phase error {max_err} must stay under Delta/2 = 8");
    }
}

