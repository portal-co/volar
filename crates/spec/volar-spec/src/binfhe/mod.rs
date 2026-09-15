// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! `binfhe` — Track-V2 Boolean FHE construction (plan:
//! `docs/fhe/binfhe-v2-implementation-plan.md`).
//!
//! This module family is an **entirely separate** construction from the
//! legacy `crate::tfhe` module (Track S). It neither patches nor inherits
//! correctness from Track S; see the two-track decision in
//! `docs/fhe/tfhe-two-track-cleanup-plan.md`.
//!
//! # Construction
//!
//! CGGI/GINX-style fully homomorphic encryption over power-of-two moduli:
//!
//! - LWE ciphertexts over `Z_q` with `q = 2^LOG_Q_LWE` and the profile
//!   invariant `q = 2N` (N = ring dimension), so the torus-to-ring-exponent
//!   map for blind rotation is exact.
//! - RLWE/RGSW over `Z_Q[X]/(X^N + 1)` with `Q = 2^LOG_Q`, `Q >= q`.
//! - GINX blind rotation with RGSW-encrypted binary LWE key bits.
//! - Programmable bootstrapping with validated multi-input lookup tables
//!   ([`lut`]); all Boolean gates are LUT evaluations.
//! - Circuit bootstrapping (LWE -> RGSW) for RGSW wires ([`circuit_bs`]).
//! - A serializable [`plan`] structure so the weaver and the interpreters
//!   share one bootstrap schedule.
//!
//! # Wire encoding
//!
//! A Boolean wire is an LWE ciphertext with phase in `{0, Delta}` where
//! `Delta = q / 2^(K+1)` and `K` is the maximum LUT arity used in the
//! circuit (`K >= 1`; `K = 1` gives the classic `{0, q/4}` encoding).
//! The uniform multi-input selector construction (plan §3, §7) combines
//! `k <= K` address bits with exact integer weights `2^j` and evaluates the
//! table in one blind rotation. Profile invariant: `q >= 2^(K+2)` so the
//! half-bin centering offset is integral.
//!
//! # Parameters
//!
//! Parameters come from typed profile modules ([`params`]). The `std128`
//! profile transcribes OpenFHE's published `STD128` set (BSD 2-Clause
//! source); it is **not** a Volar security claim until the validation gate
//! in plan §9 is executed. `toy` is an exact, noiseless correctness
//! fixture. `toy_noisy` exercises real noise at small scale.
//!
//! # Determinism
//!
//! Fully deterministic given the caller's [`SpecRng`](crate::SpecRng);
//! integer-only arithmetic; `#![no_std]` + `alloc`.

// Milestone-scoped module list; extended as later milestones land.
pub mod lwe;
pub mod params;
pub mod sampler;
pub mod torus;
