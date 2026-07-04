// @reliability: experimental
//! @ai: assisted
//! **IOP-based prove-the-verifier**: a parallel backend to
//! [`volar_fold`]'s Nova-style "prove-the-verifier" (see
//! `docs/prove-the-verifier.md`), described in
//! `docs/prove-the-verifier-iop.md`. Both paths solve the same problem
//! (succinctly attest that the VOLE verifier's per-gate checks all held);
//! this one stays **native** to the VOLE field (`GF(2^k)`, no
//! `GF(2^k)→F_ℓ` embedding) and produces a genuinely succinct,
//! Merkle+Fiat–Shamir-compiled proof rather than an `O(|F|)` native opening.
//!
//! Two phases, kept in separate modules:
//! - [`fold`]: the per-gate accumulator, O(1) memory, structurally identical
//!   to `NovaFoldSink`/`fold_and_gate` — see that module's doc.
//! - [`ligero`]: the one-shot finalization IOP over the small, fixed-size
//!   final accumulator — the actual new Merkle+Fiat–Shamir backend, and
//!   what implements `volar_fold::verifier::compress_with_snark`'s stubbed
//!   role, natively.
#![no_std]

extern crate alloc;

pub mod field;
pub mod merkle;
pub mod transcript;
pub mod fold;
pub mod ligero;
pub mod verifier;

pub use fold::IopAccumulator;
pub use verifier::{prove_verifier_iop, verify_iop, IopProof, IopVerifierFold};
