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

use crate::binfhe::blind_rotate::binfhe_blind_rotate;
use crate::binfhe::keys::{BootstrappingKey, binfhe_key_switch};
use crate::binfhe::lut::{Lut, fill_test_poly, table_is_constant};
use crate::binfhe::lwe::{
    LweCiphertext, binfhe_trivial, binfhe_lwe_add, binfhe_lwe_add_const, binfhe_lwe_scale, wire_delta,
};
use crate::binfhe::modswitch::mod_switch_lwe;
use crate::binfhe::rlwe::binfhe_sample_extract;

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
    BK: crate::binfhe::keys::AsBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> + ?Sized,
>(
    ct: &LweCiphertext<N_LWE>,
    test_poly: &[u32; BIG_N],
    bk: &BK,
) -> LweCiphertext<N_LWE> {
    let acc = binfhe_blind_rotate::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG>(
        ct,
        test_poly,
        bk.bsk_rows(),
    );
    let extracted = binfhe_sample_extract::<BIG_N, LOG_Q>(&acc);
    let at_ks = mod_switch_lwe::<BIG_N, LOG_Q, LOG_MOD_KS>(&extracted);
    let switched = binfhe_key_switch::<N_LWE, BIG_N, LOG_MOD_KS, KS_ELL, KS_BASE_LOG, _>(
        &at_ks,
        &bk.ksk_ref(),
    );
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
        let scaled = binfhe_lwe_scale::<N_LWE, LOG_Q_LWE>(bit, 1u32 << j);
        combined = binfhe_lwe_add::<N_LWE, LOG_Q_LWE>(&combined, &scaled);
    }
    combined = binfhe_lwe_add_const::<N_LWE, LOG_Q_LWE>(&combined, delta / 2);
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
        _,
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
        let scaled = binfhe_lwe_scale::<N_LWE, LOG_Q_LWE>(bit, 1u32 << j);
        combined = binfhe_lwe_add::<N_LWE, LOG_Q_LWE>(&combined, &scaled);
    }
    combined = binfhe_lwe_add_const::<N_LWE, LOG_Q_LWE>(&combined, delta / 2);
    binfhe_pbs_core::<
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, _,
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
    use crate::binfhe::keys::binfhe_gen_bootstrapping_key;
    use crate::binfhe::lwe::{
        LweSecretKey, binfhe_not, binfhe_gen_lwe_secret_key, binfhe_lwe_decrypt, binfhe_lwe_encrypt, lwe_phase,
    };
    use crate::binfhe::params::{toy, toy_noisy};
    use crate::binfhe::rlwe::{RlweSecretKey, binfhe_gen_rlwe_secret_key};
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
        let lwe_sk = binfhe_gen_lwe_secret_key(&mut rng);
        let rlwe_sk: RlweSecretKey<{ toy::BIG_N }> = binfhe_gen_rlwe_secret_key(&mut rng);
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
        binfhe_lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(m, delta, sk, &mut rng)
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
            binfhe_lwe_decrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(ct, sk, delta),
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
        let lwe_sk = binfhe_gen_lwe_secret_key::<{ toy_noisy::N_LWE }, _>(&mut rng);
        let rlwe_sk = binfhe_gen_rlwe_secret_key::<{ toy_noisy::BIG_N }, _>(&mut rng);
        let bk: NBk = binfhe_gen_bootstrapping_key::<
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
            let ca = binfhe_lwe_encrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }, { toy_noisy::CBD_ETA }, _>(
                a, delta, &lwe_sk, &mut ra,
            );
            let cb = binfhe_lwe_encrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }, { toy_noisy::CBD_ETA }, _>(
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
                binfhe_lwe_decrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }>(&out, &lwe_sk, delta),
                a && b,
                "noisy AND seed {seed}"
            );
        }
        assert!(max_err < 8, "observed max output phase error {max_err} must stay under Delta/2 = 8");
    }

    // ── M8 noise-budget suite (plan §6.6) ───────────────────────────────

    type NoisyBk = BootstrappingKey<
        { toy_noisy::N_LWE },
        { toy_noisy::BIG_N },
        { toy_noisy::BS_ELL },
        { toy_noisy::KS_ELL },
    >;

    fn noisy_keys(seed: u64) -> (LweSecretKey<{ toy_noisy::N_LWE }>, NoisyBk) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = binfhe_gen_lwe_secret_key::<{ toy_noisy::N_LWE }, _>(&mut rng);
        let rlwe_sk = binfhe_gen_rlwe_secret_key::<{ toy_noisy::BIG_N }, _>(&mut rng);
        let bk = binfhe_gen_bootstrapping_key::<
            { toy_noisy::N_LWE }, { toy_noisy::BIG_N }, { toy_noisy::LOG_Q },
            { toy_noisy::LOG_Q_LWE }, { toy_noisy::LOG_MOD_KS }, { toy_noisy::BS_ELL },
            { toy_noisy::BS_BASE_LOG }, { toy_noisy::KS_ELL }, { toy_noisy::KS_BASE_LOG },
            { toy_noisy::CBD_ETA }, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        (lwe_sk, bk)
    }

    fn noisy_encrypt(
        m: bool,
        k_max: usize,
        sk: &LweSecretKey<{ toy_noisy::N_LWE }>,
        seed: u64,
    ) -> LweCiphertext<{ toy_noisy::N_LWE }> {
        let delta = wire_delta::<{ toy_noisy::LOG_Q_LWE }>(k_max);
        let mut rng = TestRng::new(seed);
        binfhe_lwe_encrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }, { toy_noisy::CBD_ETA }, _>(
            m, delta, sk, &mut rng,
        )
    }

    /// Centered absolute phase error (mod 128) against a canonical value.
    fn centered_err(phase: u32, expected: u32) -> u32 {
        let diff = (phase as i32 - expected as i32).rem_euclid(128);
        (diff.min(128 - diff)) as u32
    }

    /// §6.6: a seeded corpus of mixed bootstraps must exhibit zero
    /// decryption failures and phase errors well under the decode margin.
    /// The corpus size and seed set are fixed, making this an exact
    /// regression fixture: any noise-model regression changes the numbers
    /// deterministically.
    #[test]
    fn noise_budget_corpus_zero_failures() {
        let (sk, bk) = noisy_keys(0xB06E7);
        let delta = wire_delta::<{ toy_noisy::LOG_Q_LWE }>(2);
        let mut failures = 0u32;
        let mut max_err = 0u32;
        let mut bootstrap_count = 0u32;
        // 256 bootstraps over 4 key/encryption seed domains, mixing AND
        // and OR tables (both arity-2 selectors: worst weight 3 on fresh
        // input noise).
        for trial in 0..256u64 {
            let a = trial % 2 == 0;
            let b = trial % 3 != 0;
            let use_and = trial % 5 != 0;
            let ca = noisy_encrypt(a, 2, &sk, 10_000 + trial);
            let cb = noisy_encrypt(b, 2, &sk, 20_000 + trial);
            let out = if use_and {
                binfhe_gate_and::<
                    { toy_noisy::N_LWE }, { toy_noisy::BIG_N }, { toy_noisy::LOG_Q },
                    { toy_noisy::LOG_Q_LWE }, { toy_noisy::LOG_MOD_KS }, { toy_noisy::BS_ELL },
                    { toy_noisy::BS_BASE_LOG }, { toy_noisy::KS_ELL }, { toy_noisy::KS_BASE_LOG }, 2,
                >(ca, cb, &bk)
            } else {
                binfhe_gate_or::<
                    { toy_noisy::N_LWE }, { toy_noisy::BIG_N }, { toy_noisy::LOG_Q },
                    { toy_noisy::LOG_Q_LWE }, { toy_noisy::LOG_MOD_KS }, { toy_noisy::BS_ELL },
                    { toy_noisy::BS_BASE_LOG }, { toy_noisy::KS_ELL }, { toy_noisy::KS_BASE_LOG }, 2,
                >(ca, cb, &bk)
            };
            bootstrap_count += 1;
            let expected = if use_and { a && b } else { a || b };
            let phase = lwe_phase::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }>(&out, &sk);
            let canonical = if expected { delta } else { 0 };
            max_err = max_err.max(centered_err(phase, canonical));
            if binfhe_lwe_decrypt::<{ toy_noisy::N_LWE }, { toy_noisy::LOG_Q_LWE }>(&out, &sk, delta)
                != expected
            {
                failures += 1;
            }
        }
        assert_eq!(failures, 0, "{failures}/{bootstrap_count} bootstrap failures");
        // The analytic decode margin is Delta/2 = 8; the observed errors
        // must remain at or below half the margin with slack to spare.
        assert!(
            max_err <= 4,
            "observed max phase error {max_err} exceeds half the decode margin (4)"
        );
    }

    /// §6.6 selector-margin accounting: the profile admits a cone only
    /// when `(2^k - 1) * input_noise_bound < Delta/2`. At `q = 128` with
    /// fresh-input bound 1 (CBD_ETA = 1) that admits arity 2 and rejects
    /// arity 3 — the failure the earlier draft of this suite measured
    /// before the accounting was made explicit.
    #[test]
    fn noise_budget_selector_margin_accounting() {
        use crate::binfhe::params::selector_margin;
        let log_q = toy_noisy::LOG_Q_LWE; // 7
        // Fresh inputs (|e| <= 1): arity 2 in budget, arity 3 out.
        assert!(selector_margin(log_q, 2, 1));
        assert!(!selector_margin(log_q, 3, 1));
        // Refreshed wires: the corpus above bounds the observed output
        // error by 2 at this profile; arity 2 stays in budget (3 * 2 < 8),
        // while arity 3 is out of budget for any nonzero input error
        // (weight 7 >= margin 4 already at bound 1; only a zero-noise
        // profile can afford it, e.g. the exact toy fixture).
        assert!(selector_margin(log_q, 2, 2));
        assert!(!selector_margin(log_q, 3, 1));
        assert!(!selector_margin(log_q, 3, 2));
        // Sanity: with error-free inputs any arity fits (no amplification).
        assert!(selector_margin(log_q, 3, 0));
        // std128 (q = 2048) with an (unvalidated, illustrative) refreshed
        // bound of 2^5: arity 2 is in budget (3 * 32 = 96 < 128) but
        // arity 3 is not (7 * 32 = 224 >= 64). Wide-cone fusion at real
        // parameters needs either a quieter eval path or refreshed-input
        // discipline — precisely what the weaver's budget check prices.
        assert!(selector_margin(crate::binfhe::params::std128::LOG_Q_LWE, 2, 32));
        assert!(!selector_margin(crate::binfhe::params::std128::LOG_Q_LWE, 3, 32));
        // And with quiet wires (bound 1), arity 3 fits at std128.
        assert!(selector_margin(crate::binfhe::params::std128::LOG_Q_LWE, 3, 1));
    }

    /// Std128-shaped smoke test: full keygen and one AND gate at the
    /// OpenFHE-STD128 transcription (`n = 556`, `N = 1024`, `Q = 2^27`).
    ///
    /// **Ignored by default**: schoolbook arithmetic makes this roughly a
    /// minute in release mode. Run explicitly with:
    ///
    /// ```sh
    /// cargo test -p volar-spec --lib binfhe --release -- --ignored
    /// ```
    ///
    /// This exercises the real parameter shape end to end; it makes no
    /// security claim (plan §9 gate).
    #[test]
    #[ignore]
    fn std128_smoke_keygen_and_and_gate() {
        use crate::binfhe::params::std128;
        let mut rng = TestRng::new(0x57D128);
        let lwe_sk = binfhe_gen_lwe_secret_key::<{ std128::N_LWE }, _>(&mut rng);
        let rlwe_sk = binfhe_gen_rlwe_secret_key::<{ std128::BIG_N }, _>(&mut rng);
        let bk = binfhe_gen_bootstrapping_key::<
            { std128::N_LWE }, { std128::BIG_N }, { std128::LOG_Q },
            { std128::LOG_Q_LWE }, { std128::LOG_MOD_KS }, { std128::BS_ELL },
            { std128::BS_BASE_LOG }, { std128::KS_ELL }, { std128::KS_BASE_LOG },
            { std128::CBD_ETA }, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        let delta = wire_delta::<{ std128::LOG_Q_LWE }>(2);
        for (a, b) in [(false, false), (false, true), (true, false), (true, true)] {
            let mut ra = TestRng::new(100 + a as u64);
            let mut rb = TestRng::new(200 + b as u64);
            let ca = binfhe_lwe_encrypt::<
                { std128::N_LWE }, { std128::LOG_Q_LWE }, { std128::CBD_ETA }, _,
            >(a, delta, &lwe_sk, &mut ra);
            let cb = binfhe_lwe_encrypt::<
                { std128::N_LWE }, { std128::LOG_Q_LWE }, { std128::CBD_ETA }, _,
            >(b, delta, &lwe_sk, &mut rb);
            let out = binfhe_gate_and::<
                { std128::N_LWE }, { std128::BIG_N }, { std128::LOG_Q },
                { std128::LOG_Q_LWE }, { std128::LOG_MOD_KS }, { std128::BS_ELL },
                { std128::BS_BASE_LOG }, { std128::KS_ELL }, { std128::KS_BASE_LOG }, 2,
            >(ca, cb, &bk);
            assert_eq!(
                binfhe_lwe_decrypt::<{ std128::N_LWE }, { std128::LOG_Q_LWE }>(&out, &lwe_sk, delta),
                a && b,
                "std128 AND({a},{b})"
            );
        }
    }
}

