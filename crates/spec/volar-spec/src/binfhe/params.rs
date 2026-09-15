// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Parameter profiles for the `binfhe` construction.
//!
//! Each profile is a module of `pub const` values plus type aliases. The
//! const-generic types in [`crate::binfhe`] are instantiated with these
//! constants, so two different profiles never share a ciphertext or key
//! type: cross-profile mixing is a compile-time type error.
//!
//! # Source and claim status
//!
//! | Profile | Source | Claim |
//! |---|---|---|
//! | [`toy`] | none | Exact, noiseless correctness fixture only. Not a parameter set. |
//! | [`toy_noisy`] | none | Small-scale *noisy* fixture for noise-budget tests. Not a parameter set. |
//! | [`std128`] | OpenFHE `binfhecontext.cpp` `STD128` row (BSD 2-Clause), see below | Unvalidated transcription; **not** a Volar security claim until plan §9 runs. |
//!
//! `std128` source row, format `{bits, cycOrder, latParam, modq, modKS, Bks,
//! Bg, Brk, autoKeys, keyDist, stdDev}`:
//!
//! ```text
//! { STD128, { 27, 2048, 556, 2048, 32768, 32, 128, 64, 10, UNIFORM_TERNARY, 3.19 } }
//! ```
//!
//! Deviations from the source row, each recorded for the plan §9 gate:
//!
//! 1. **Binary secrets** instead of `UNIFORM_TERNARY`, for both the LWE and
//!    RLWE keys. Plain GINX blind rotation applies `X^{a_i}` under an
//!    encrypted selector bit and does not directly express ternary
//!    `s_i = -1`; binary secrets keep the construction exact. The security
//!    estimate in plan §9 must use binary secrets.
//! 2. **Centered binomial noise** with `CBD_ETA = 16` (exact `sigma =
//!    sqrt(ETA/2) = 2.828`) instead of `stdDev = 3.19`, keeping the sampler
//!    integer-only and deterministic (FIPS 203 §4.2.2-style mechanics).
//! 3. `Brk`/`autoKeys` (LMKCDEY automorphism path) are out of scope.
//!
//! # Profile invariants (checked by [`check_profile`])
//!
//! 1. `q = 2 * BIG_N` — exact torus-to-ring exponent mapping.
//! 2. `LOG_Q >= LOG_Q_LWE`, `LOG_MOD_KS` between them — modulus chain.
//! 3. `BS_ELL * BS_BASE_LOG >= LOG_Q` and `KS_ELL * KS_BASE_LOG >=
//!    LOG_MOD_KS` — gadget decompositions exactly cover their moduli (see
//!    [`crate::binfhe::gadget`]); with covering decompositions and zero
//!    noise the whole pipeline is exact.
//! 4. `q >= 2^(K_MAX + 2)` — the half-bin centering offset for the largest
//!    supported LUT arity is integral (see [`crate::binfhe::lut`]).

/// Largest LUT arity a profile's modulus chain supports exactly:
/// `K_MAX = LOG_Q_LWE - 2`.
pub const fn max_lut_arity(log_q_lwe: u32) -> u32 {
    log_q_lwe.saturating_sub(2)
}

/// Compile-time-checkable profile invariant assertion.
///
/// Every profile module exposes a `check()` that calls this; the checks are
/// `const` and are also exercised by unit tests so a malformed profile fails
/// loudly in either context.
pub const fn check_profile(
    n_lwe: usize,
    big_n: usize,
    log_q: u32,
    log_q_lwe: u32,
    log_mod_ks: u32,
    bs_base_log: u32,
    bs_ell: usize,
    ks_base_log: u32,
    ks_ell: usize,
) {
    assert!(big_n.is_power_of_two(), "BIG_N must be a power of two");
    assert!(n_lwe > 0, "N_LWE must be non-zero");
    // Invariant 1: q == 2N.
    assert!(
        (1usize << log_q_lwe) == 2 * big_n,
        "profile invariant: q = 2^LOG_Q_LWE must equal 2 * BIG_N"
    );
    // Invariant 2: modulus chain Q >= modKS >= q.
    assert!(log_q >= log_mod_ks, "LOG_Q >= LOG_MOD_KS required");
    assert!(log_mod_ks >= log_q_lwe, "LOG_MOD_KS >= LOG_Q_LWE required");
    assert!(log_q <= 32, "power-of-two moduli up to 2^32 are representable");
    // Invariant 3: covering gadget decompositions.
    assert!(
        bs_ell as u32 * bs_base_log >= log_q,
        "BS_ELL * BS_BASE_LOG must cover LOG_Q"
    );
    assert!(
        ks_ell as u32 * ks_base_log >= log_mod_ks,
        "KS_ELL * KS_BASE_LOG must cover LOG_MOD_KS"
    );
    // Invariant 4 is per-circuit (max arity); the profile-level cap is
    // LOG_Q_LWE - 2 (see max_lut_arity).
}

/// Exact, noiseless correctness fixture. Not a parameter set: all gadget
/// decompositions exactly cover their moduli and the noise sampler is
/// disabled, so every intermediate is bit-exact and canonical-phase
/// assertions are exact equalities.
pub mod toy {
    /// LWE secret-key dimension.
    pub const N_LWE: usize = 8;
    /// RLWE ring dimension (power of two).
    pub const BIG_N: usize = 64;
    /// Ring modulus `Q = 2^8`.
    pub const LOG_Q: u32 = 8;
    /// LWE modulus `q = 2^7 = 2 * BIG_N`.
    pub const LOG_Q_LWE: u32 = 7;
    /// Key-switching modulus `2^8` (identity switch from Q).
    pub const LOG_MOD_KS: u32 = 8;
    /// Bootstrapping gadget base `2^4`, two levels covering `LOG_Q = 8`.
    pub const BS_BASE_LOG: u32 = 4;
    pub const BS_ELL: usize = 2;
    /// Key-switching gadget base `2^4`, two levels covering `LOG_MOD_KS = 8`.
    pub const KS_BASE_LOG: u32 = 4;
    pub const KS_ELL: usize = 2;
    /// Centered-binomial width; `0` disables noise entirely.
    pub const CBD_ETA: u32 = 0;

    /// Const-checked at profile definition time.
    const _: () = super::check_profile(
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_BASE_LOG, BS_ELL, KS_BASE_LOG, KS_ELL,
    );
}

/// Small-scale noisy fixture for noise-budget tests. Not a parameter set.
///
/// Sized so that per-bootstrap failure is rare but the full noise path
/// (RGSW external products, key switching, modulus switches) is exercised:
/// the blind-rotation accumulator noise tolerance is `Q / (2q) = 2^8`, a
/// ~3.5-sigma margin for the configured decomposition, and key switching
/// runs at `modKS = 2^14` so its noise is scaled down by `modKS / q = 2^7`
/// before the final decode.
pub mod toy_noisy {
    pub const N_LWE: usize = 8;
    pub const BIG_N: usize = 64;
    pub const LOG_Q: u32 = 16;
    pub const LOG_Q_LWE: u32 = 7;
    pub const LOG_MOD_KS: u32 = 14;
    pub const BS_BASE_LOG: u32 = 4;
    pub const BS_ELL: usize = 4;
    pub const KS_BASE_LOG: u32 = 4;
    pub const KS_ELL: usize = 4;
    pub const CBD_ETA: u32 = 2;

    const _: () = super::check_profile(
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_BASE_LOG, BS_ELL, KS_BASE_LOG, KS_ELL,
    );
}

/// Transcription of OpenFHE's published `STD128` binFHE parameter set
/// (BSD 2-Clause source), with the deviations recorded in this module's
/// top-level documentation. **Not** a Volar security claim: the plan §9
/// gate (lattice-estimator run plus failure-probability recomputation) must
/// be executed and recorded before any security or failure-rate statement.
pub mod std128 {
    pub const N_LWE: usize = 556;
    pub const BIG_N: usize = 1024;
    pub const LOG_Q: u32 = 27;
    pub const LOG_Q_LWE: u32 = 11;
    pub const LOG_MOD_KS: u32 = 15;
    pub const BS_BASE_LOG: u32 = 7;
    pub const BS_ELL: usize = 4;
    pub const KS_BASE_LOG: u32 = 5;
    pub const KS_ELL: usize = 3;
    pub const CBD_ETA: u32 = 16;

    const _: () = super::check_profile(
        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_BASE_LOG, BS_ELL, KS_BASE_LOG, KS_ELL,
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profile_invariants_hold_at_compile_time() {
        // The `const _: () = check_profile(...)` items in each profile module
        // already failed compilation if violated; this test additionally
        // exercises the runtime path and documents the intended invariants.
        const fn profile_ok(
            n_lwe: usize,
            big_n: usize,
            log_q: u32,
            log_q_lwe: u32,
            log_mod_ks: u32,
            bs_base_log: u32,
            bs_ell: usize,
            ks_base_log: u32,
            ks_ell: usize,
        ) -> bool {
            big_n.is_power_of_two()
                && n_lwe > 0
                && (1usize << log_q_lwe) == 2 * big_n
                && log_q >= log_mod_ks
                && log_mod_ks >= log_q_lwe
                && log_q <= 32
                && bs_ell as u32 * bs_base_log >= log_q
                && ks_ell as u32 * ks_base_log >= log_mod_ks
        }
        assert!(profile_ok(
            toy::N_LWE,
            toy::BIG_N,
            toy::LOG_Q,
            toy::LOG_Q_LWE,
            toy::LOG_MOD_KS,
            toy::BS_BASE_LOG,
            toy::BS_ELL,
            toy::KS_BASE_LOG,
            toy::KS_ELL
        ));
        assert!(profile_ok(
            toy_noisy::N_LWE,
            toy_noisy::BIG_N,
            toy_noisy::LOG_Q,
            toy_noisy::LOG_Q_LWE,
            toy_noisy::LOG_MOD_KS,
            toy_noisy::BS_BASE_LOG,
            toy_noisy::BS_ELL,
            toy_noisy::KS_BASE_LOG,
            toy_noisy::KS_ELL
        ));
        assert!(profile_ok(
            std128::N_LWE,
            std128::BIG_N,
            std128::LOG_Q,
            std128::LOG_Q_LWE,
            std128::LOG_MOD_KS,
            std128::BS_BASE_LOG,
            std128::BS_ELL,
            std128::KS_BASE_LOG,
            std128::KS_ELL
        ));
    }

    #[test]
    fn arity_caps_leave_room_for_half_bin_offset() {
        // K_MAX = LOG_Q_LWE - 2, so q / 2^(K_MAX + 2) = 1 stays integral.
        assert_eq!(max_lut_arity(toy::LOG_Q_LWE), 5);
        assert_eq!(max_lut_arity(std128::LOG_Q_LWE), 9);
    }

    #[test]
    #[should_panic]
    fn malformed_profile_wrong_lwe_modulus_is_rejected() {
        // q != 2N must fail: LOG_Q_LWE = 6 -> q = 64 != 2 * 64.
        check_profile(4, 64, 8, 6, 8, 4, 2, 4, 2);
    }

    #[test]
    #[should_panic]
    fn malformed_profile_non_covering_decomposition_is_rejected() {
        // BS_ELL * BS_BASE_LOG = 6 < LOG_Q = 8 must fail.
        check_profile(4, 64, 8, 7, 8, 3, 2, 4, 2);
    }
}
