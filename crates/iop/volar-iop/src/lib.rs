// @reliability: experimental
//! @ai: assisted
//! **IOP-based prove-the-verifier**: succinctly attest that the VOLE
//! verifier's per-gate checks all held, described in full in
//! `docs/prove-the-verifier-iop.md`. Stays **native** to the VOLE field
//! (`GF(2^k)`, no cross-field embedding) and produces a genuinely succinct,
//! Merkle+Fiat–Shamir-compiled proof rather than an `O(|F|)` native opening.
//!
//! Two phases, kept in separate modules:
//! - [`fold`]: the per-gate accumulator, O(1) memory, folded via Nova-style
//!   relaxed-R1CS cross-term algebra — see that module's doc.
//! - [`ligero`]: the one-shot finalization IOP over the small, fixed-size
//!   final accumulator — the Merkle+Fiat–Shamir backend that makes the
//!   result succinct instead of a native opening.
#![no_std]

extern crate alloc;

pub mod field;
pub mod fold;
pub mod ligero;
pub mod merkle;
pub mod transcript;
pub mod verifier;

pub use fold::IopAccumulator;
pub use verifier::{IopProof, IopVerifierFold, prove_verifier_iop, verify_iop};
