// @reliability: experimental
// @ai: assisted
//! **VOLE Continuation Bridge — IVC folding** (`volar-fold`).
//!
//! A *Nova-minus-the-zkSNARK* folding scheme used by the VOLE Continuation
//! Bridge to make a network-gap proof **succinct in the gap length `m`**: the
//! `m` cleartext gap steps are folded into a single relaxed-R1CS instance of
//! size `O(|F|)`, then the folded instance is checked **natively and
//! interactively** by the verifier (no final SNARK compression — online is
//! cheap and interactive). Because there is no in-circuit verifier (no
//! recursion), a **single** elliptic-curve group suffices (no cycle of curves);
//! we reuse Ed25519 from [`volar_spec::curve`].
//!
//! See `docs/vcb-ivc-folding.md` for the full cryptographic design and
//! `docs/vole-continuation-bridge.md` §3.3 for the bridge context.
//!
//! ## Modules
//! - [`scalar`] — the curve scalar field `F_ℓ` (Ed25519 group order).
//! - [`pedersen`] — additively-homomorphic Pedersen vector commitment over Ed25519.
//! - [`r1cs`] — (relaxed) R1CS constraint systems and satisfaction.
//! - [`nifs`] — the Nova non-interactive folding step.
//! - [`verify`] — native interactive verification of a folded instance.
//! - [`ivc`] — the gap-folding driver + boundary commitments.
//! - [`link`] — the generic [`link::BoundaryLink`] embedding trait (the
//!   binary-field ↔ prime-field linking is a swappable component).
//! - [`bridge_adapter`] — `FoldingBridge<L>`, a resilience-bridge strategy.
//!
//! ## Status (honest)
//! The folding machinery (scalar field, commitment, R1CS, NIFS, native verify,
//! IVC) is concrete and self-tested. The VOLE-MAC ↔ commitment **linking** is
//! abstracted behind [`link::BoundaryLink`]; the shipped [`link::DummyLink`] is a
//! placeholder (**not sound**) so the bridge wires end-to-end — a cryptographically
//! sound embedding is the isolated open task (see the doc).

#![no_std]
extern crate alloc;

pub mod scalar;
pub mod pedersen;
pub mod r1cs;
pub mod nifs;
pub mod verify;
pub mod ivc;
pub mod link;
pub mod bridge_adapter;

pub use scalar::Scalar;
