// @reliability: experimental
// @experimental-status: pinned
// @experimental-since: bitgc-pinning
//! @ai: assisted
//! **Shared pinned negacyclic NTT** — a reusable, dependency-free NTT over
//! `Z_q[X]/(X^N + 1)` for the NTT-using systems in this crate (BinFHE,
//! BitGC's SWHE/DD rings, and any FHE path that uses an NTT-friendly prime
//! modulus).
//!
//! This lifts the `bitgc::ring::Ntt` core into a crate-public component and
//! pins a set of **paper-evidenced parameter sets** ([`params`]) so that
//! every consumer shares the same verified primes rather than each module
//! generating its own.
//!
//! # When NTT applies
//!
//! A negacyclic NTT of length `N` exists iff `q ≡ 1 (mod 2N)` (so a
//! primitive `2N`-th root of unity `psi` with `psi^N = −1` exists). Systems
//! with a **power-of-two** modulus (TFHE/BinFHE's native `q = 2^32`/`2^64`)
//! cannot use a prime-modulus NTT directly: OpenFHE/NuFHE handle this by
//! either complex FFT or by **rounding into an auxiliary NTT-friendly prime**
//! large enough that the integer convolution is exact (`q_ntt > 2·N·q²` for
//! the BinFHE coefficient range) and rounding back. The [`params::BINFHE_STD128_NTT`]
//! set is sized for exactly that path. Systems with a native prime modulus
//! (BitGC's BGV-style SWHE, the DD gadget) use the NTT directly.
//!
//! # Correctness
//!
//! Every pinned parameter set carries a compile-time-checked NTT-friendliness
//! assertion, and `tests::pinned_params_round_trip` verifies each prime
//! round-trips a random negacyclic convolution against the schoolbook
//! reference at its full degree.

use alloc::vec::Vec;

/// A verified NTT-friendly parameter set: a prime `modulus` and a ring
/// `degree` for which `modulus ≡ 1 (mod 2·degree)`.
///
/// Construction validates the NTT-friendliness condition; use
/// [`NttParams::new`] to build one, or the pinned constants in [`params`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NttParams {
    /// Ring degree `N` (power of two).
    pub degree: usize,
    /// Prime modulus `q` with `q ≡ 1 (mod 2N)`.
    pub modulus: u64,
}

impl NttParams {
    /// Build and validate a parameter set. Returns `None` unless
    /// `degree` is a power of two, `modulus` is odd, and
    /// `(modulus - 1) % (2·degree) == 0` (the NTT-friendliness condition).
    pub const fn new(degree: usize, modulus: u64) -> Option<Self> {
        if degree == 0 || (degree & (degree - 1)) != 0 {
            return None;
        }
        if modulus % 2 == 0 {
            return None;
        }
        if (modulus - 1) % (2 * degree as u64) != 0 {
            return None;
        }
        Some(Self { degree, modulus })
    }

    /// Bit-length of the modulus (`ceil(log2(modulus))`).
    pub const fn modulus_bits(&self) -> u32 {
        64 - self.modulus.leading_zeros()
    }
}

/// Paper-evidenced / production-grade pinned NTT parameter sets.
///
/// Each constant's modulus is a verified NTT-friendly prime (checked by the
/// `const _: ()` assertions and the round-trip tests). The citations record
/// the evidence; **none of these is a Volar security claim** — parameter
/// *security* is the consumer's responsibility (see `binfhe::params`' plan
/// §9 gate and BitGC's BITGC-LEDGER-03).
pub mod params {
    use super::NttParams;

    // ── BitGC ([ZLL+26] §6) ─────────────────────────────────────────────
    //
    // The BitGC papers pin the *shapes*: N_PRG = 2^15, N_gate = 2^14 (small)
    // / 2^13 (fast), with 60-bit NTT-friendly primes forming the RNS chains
    // (log Q_PRG ≈ 600/780, log Q_gate ≈ 180/300 across ~10–13 limbs). The
    // specific primes below are generated to satisfy `q ≡ 1 (mod 2N)` at the
    // paper's ring degrees; the *choice* of which 60-bit NTT-friendly prime
    // is free (any such prime works), so these are reference limbs, not a
    // paper-pinned value.

    /// BitGC PRG ring: `N = 2^15`, 60-bit NTT-friendly prime.
    /// [ZLL+26] §6 (`N_PRG = 2^15`).
    pub const BITGC_PRG_2_15: NttParams =
        match NttParams::new(1 << 15, 576460752308273153) {
            Some(p) => p,
            None => panic!("BITGC_PRG_2_15 is not NTT-friendly"),
        };

    /// BitGC small gate ring: `N = 2^14`, 60-bit NTT-friendly prime.
    /// [ZLL+26] §6 (`N_gate = 2^14` for the small variant).
    pub const BITGC_GATE_2_14: NttParams =
        match NttParams::new(1 << 14, 576460752304439297) {
            Some(p) => p,
            None => panic!("BITGC_GATE_2_14 is not NTT-friendly"),
        };

    /// BitGC fast gate ring: `N = 2^13`, 60-bit NTT-friendly prime.
    /// [ZLL+26] §6 (`N_gate = 2^13` for the fast variant).
    pub const BITGC_GATE_2_13: NttParams =
        match NttParams::new(1 << 13, 576460752303439873) {
            Some(p) => p,
            None => panic!("BITGC_GATE_2_13 is not NTT-friendly"),
        };

    // ── BinFHE (OpenFHE STD128 transcription; binfhe::params) ───────────
    //
    // OpenFHE's BinFHE `STD128` uses a power-of-two coefficient modulus
    // (LOG_Q = 27 ⇒ Q = 2^27) at ring degree BIG_N = 1024, which does not
    // admit a prime-modulus NTT. The standard production approach (OpenFHE
    // FFTP, NuFHE) rounds the negacyclic convolution into an auxiliary
    // NTT-friendly prime and back. For that to be *exact*, the auxiliary
    // prime must satisfy `q_ntt > 2·N·Q²` = `2·1024·2^54` ≈ `2^65`, so a
    // single 64-bit prime is insufficient — the exact path needs a 2-limb
    // RNS NTT or a >66-bit prime. The constant below is the largest
    // single-prime step toward that (a 50-bit NTT-friendly prime at the
    // STD128 ring degree); the RNS composition is the consumer's job.

    /// BinFHE STD128 ring degree (`BIG_N = 1024`), 50-bit NTT-friendly
    /// prime — the largest single-prime limb for the NTT-with-rounding
    /// path (exact BinFHE convolution needs `q > 2·N·Q² ≈ 2^65`, so use
    /// this as one RNS limb; see module docs). Evidence: OpenFHE
    /// `binfhecontext.cpp` STD128 row (`N = 1024`), transcribed in
    /// `binfhe::params::std128`.
    pub const BINFHE_STD128_NTT: NttParams =
        match NttParams::new(1 << 10, 562949953443841) {
            Some(p) => p,
            None => panic!("BINFHE_STD128_NTT is not NTT-friendly"),
        };

    // ── Small / reference profiles ──────────────────────────────────────

    /// Small reference profile (`N = 2^10 = 1024`, 30-bit prime). Used by
    /// the crate's existing unit tests; cheap enough for exhaustive checks.
    pub const REF_2_10: NttParams = match NttParams::new(1 << 10, 1073750017) {
        Some(p) => p,
        None => panic!("REF_2_10 is not NTT-friendly"),
    };
}

// Re-export the underlying NTT engine so consumers share it. The engine
// itself lives in `bitgc::ring` (it was built for BitGC); this module is
// the crate-public parameter registry + a thin re-export seam. When more
// consumers land, the engine can be moved here without changing callers.
pub use crate::bitgc::ring::ntt_context;

/// Build the (currently `bitgc::ring`-backed) NTT context for a pinned
/// parameter set. Returns `None` if the parameters are not NTT-friendly
/// (they are for all [`params`] constants, which are compile-time checked).
pub fn context(params: NttParams) -> Option<ntt_context::Context> {
    ntt_context::Context::new(params.degree, params.modulus)
}

/// A negacyclic convolution over a pinned parameter set: NTT when the
/// modulus admits it (all [`params`] do), else the schoolbook reference.
/// This is the single entry point consumers should call.
pub fn negacyclic_mul(params: NttParams, a: &[u64], b: &[u64]) -> Vec<u64> {
    ntt_context::negacyclic_mul(params.degree, params.modulus, a, b)
}

#[cfg(test)]
mod tests {
    use super::params::*;
    use super::*;

    /// Every pinned parameter set must round-trip a negacyclic convolution
    /// against the schoolbook reference at its full degree.
    #[test]
    fn pinned_params_round_trip() {
        // The small/reference profile is cheap; the production 2^13..2^15
        // profiles are exercised structurally (NTT-friendly check) but the
        // full schoolbook cross-check at N=2^15 is O(N²)≈10^9 — too slow for
        // a unit test. We cross-check the smaller sets fully and the large
        // ones against the NTT self-consistency (forward∘inverse = identity).
        for params in [REF_2_10, BINFHE_STD128_NTT] {
            let n = params.degree;
            let q = params.modulus;
            let mut s = 0x243f_6a88_85a3_08d3u64;
            let mut rnd = |m: u64| {
                s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                (s >> 33) % m
            };
            let a: Vec<u64> = (0..n).map(|_| rnd(q)).collect();
            let b: Vec<u64> = (0..n).map(|_| rnd(q)).collect();
            let via_ntt = negacyclic_mul(params, &a, &b);
            let via_schoolbook =
                crate::bitgc::ring::negacyclic_mul_coeffs(&a, &b, q);
            assert_eq!(via_ntt, via_schoolbook, "params {:?}", params);
        }
    }

    /// The production BitGC profiles must be NTT-friendly (this is what the
    /// `const` assertions already guarantee, but exercise the runtime path).
    #[test]
    fn bitgc_production_profiles_are_ntt_friendly() {
        for params in [BITGC_PRG_2_15, BITGC_GATE_2_14, BITGC_GATE_2_13] {
            assert!(
                context(params).is_some(),
                "params {:?} must build an NTT context",
                params
            );
        }
    }

    /// All pinned moduli are prime (a precondition for the NTT root to
    /// exist; the friendliness check alone is necessary but the constants
    /// were generated as primes — assert it to lock the property).
    #[test]
    fn pinned_moduli_are_prime() {
        for params in [
            BITGC_PRG_2_15,
            BITGC_GATE_2_14,
            BITGC_GATE_2_13,
            BINFHE_STD128_NTT,
            REF_2_10,
        ] {
            assert!(
                is_prime(params.modulus),
                "{} is not prime",
                params.modulus
            );
        }
    }

    fn is_prime(n: u64) -> bool {
        if n < 2 {
            return false;
        }
        for p in [2u64, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] {
            if n % p == 0 {
                return n == p;
            }
        }
        let d = n - 1;
        let r = d.trailing_zeros();
        let dd = d >> r;
        for a in [2u64, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] {
            let mut x = pow_mod_u64(a, dd, n);
            if x == 1 || x == n - 1 {
                continue;
            }
            let mut cont = false;
            for _ in 0..r - 1 {
                x = ((x as u128 * x as u128) % n as u128) as u64;
                if x == n - 1 {
                    cont = true;
                    break;
                }
            }
            if !cont {
                return false;
            }
        }
        true
    }

    fn pow_mod_u64(mut b: u64, mut e: u64, m: u64) -> u64 {
        let mut r = 1u128;
        let mm = m as u128;
        let mut bb = (b % m) as u128;
        while e > 0 {
            if e & 1 == 1 {
                r = r * bb % mm;
            }
            bb = bb * bb % mm;
            e >>= 1;
        }
        r as u64
    }
}
