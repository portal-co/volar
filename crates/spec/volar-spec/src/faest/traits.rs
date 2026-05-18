// @reliability: experimental
//! @ai: assisted
//! Traits that decouple `sign.rs` from the specific QuickSilver proof backend.
//!
//! [`FaestAesProver`] is the dependency-inversion point: `sign` and `verify`
//! are generic over it, so the FAEST weaver-generated prover can be plugged
//! in once the K=2 circuit pipeline is complete — without touching the
//! sign/verify orchestration logic.
//!
//! ## Backends
//!
//! | Backend | Description |
//! |---|---|
//! | [`StubFaestAesProver`] | Hash-based stub (self-consistent but does not bind to AES witness) |
//! | Weaver-generated | `weave_faest_prover_with_mode` output implementing this trait |
//!
//! ## Pinning
//!
//! Any `impl FaestAesProver` produced by the FAEST weaver with
//! [`FaestAesMode`](crate::faest::sign::FaestParams) carries the same
//! K=2 sbox Vopes for every AES gate, so composed proofs that include AES
//! subcircuits always emit identical proof material regardless of context.

extern crate alloc;
use alloc::vec::Vec;

use super::convert_to_vole::BigVoleProver;
use super::universal_hash::{UniversalHashKey, UniversalHashOutput, vole_hash};

/// The QuickSilver proof values sent from prover to verifier.
///
/// In full FAEST these encode the A-hat / B-hat / C-hat commitments from
/// the QuickSilver constraint system over the AES witness.  In the stub
/// backend, A-hat is a hash of the VOLE u vector and B/C are zero — the
/// structure is self-consistent (sign ↔ verify round-trips) but does not
/// bind to the AES witness cryptographically.
#[derive(Clone, Debug)]
pub struct QuickSilverProof {
    /// A-hat: prover's hash commitment.
    pub a_hat: Vec<u8>,
    /// B-hat: linear term (zero in the stub).
    pub b_hat: Vec<u8>,
    /// C-hat base (before counter is appended for grinding).
    pub c_hat_base: Vec<u8>,
}

/// Abstracts the QuickSilver AES-witness proof computation.
///
/// ## Dependency chain
///
/// `sign(sk, pk, msg, iv_seed, prover: &impl FaestAesProver)` calls
/// `prover.prove_aes_witness(…)` to obtain the [`QuickSilverProof`]
/// values.  The weaver-generated backend will use the K=2 sbox Vopes
/// produced by the prover function; the stub backend uses a hash.
///
/// ## Composability / FAEST spec pinning
///
/// The weaver-generated implementation is parameterised by
/// `FaestAesMode { params: FaestParams::Faest128s }`, which ensures all
/// AES instances in a composed proof carry identical K=2 sbox Vopes.  Any
/// function that accepts `impl FaestAesProver` can be audited to confirm
/// it only uses FAEST-spec-compliant proof material.
pub trait FaestAesProver {
    /// Compute A-hat, B-hat, C-hat from the VOLE correlation + hash key.
    ///
    /// `big_vole` contains the prover's `u` vector and the τ per-row
    /// correction vectors.  `hash_key` is derived from `chall_2`.
    ///
    /// The returned [`QuickSilverProof`] is fed into `chall_3` derivation
    /// and the grinding loop.
    fn prove_aes_witness(
        &self,
        big_vole: &BigVoleProver,
        hash_key: &UniversalHashKey,
    ) -> QuickSilverProof;

    /// Verify that the prover's QuickSilver proof is consistent with the
    /// verifier's Q vector and Δ.
    ///
    /// Default: accept (stub — replaced by real check once K=2 dispatch
    /// lands and the circuit is available to the verifier).
    fn verify_aes_proof(
        &self,
        _proof: &QuickSilverProof,
        _hash_key: &UniversalHashKey,
        _q_vec: &[u8],
        _delta: &[u8],
    ) -> bool {
        true
    }
}

// ─── Stub backend ─────────────────────────────────────────────────────────────

/// Hash-based stub prover — self-consistent but does not bind to the
/// AES witness.  Suitable for testing the sign/verify pipeline before the
/// full K=2 circuit dispatch is wired up.
///
/// A-hat = VOLEHash(key, u), B-hat = 0, C-hat = 0.
pub struct StubFaestAesProver;

impl FaestAesProver for StubFaestAesProver {
    fn prove_aes_witness(
        &self,
        big_vole: &BigVoleProver,
        hash_key: &UniversalHashKey,
    ) -> QuickSilverProof {
        let a_hat_out: UniversalHashOutput = vole_hash(hash_key, &big_vole.u);
        let a_hat: Vec<u8> = a_hat_out.h0.0.to_le_bytes()
            .iter()
            .chain(a_hat_out.h1.0.to_le_bytes().iter())
            .cloned()
            .collect();
        let len = a_hat.len();
        QuickSilverProof {
            a_hat,
            b_hat: alloc::vec![0u8; len],
            c_hat_base: alloc::vec![0u8; len],
        }
    }
}
