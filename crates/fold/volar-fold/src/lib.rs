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
//! - [`transport`] — `FoldingTransport`, a concrete `ResilientVoleTransport`
//!   that drives the bridge from generated code (`prover_bridge` /
//!   `verifier_bridge` → a real `GapVerdict`).
//!
//! ## Status (honest)
//! The folding machinery is concrete and self-tested: a **Montgomery** scalar
//! field ([`scalar`]), a Pedersen commitment with **binding** hash-to-curve
//! generators and a **Pippenger MSM** ([`pedersen`]), R1CS/NIFS/native-verify/IVC,
//! and [`keccak`] (SHA3-256, anchored to the `sha3` crate).  The chosen
//! **dual-preimage Keccak** boundary embedding is now concrete on the **folding
//! leg**: [`keccak_r1cs`] arithmetizes Keccak-256 in R1CS over `F_ℓ`
//! (cross-checked against the reference), and [`link::KeccakDigestLink`] proves
//! the folding-committed boundary hashes (in-circuit) to a public digest while
//! binding it to the Pedersen commitment — strictly sounder than the
//! [`link::DummyLink`] placeholder.  The **VOLE leg's** Keccak circuit is also
//! built — `volar-weaver`'s `emit_keccak256` boolean gadget (sha3-verified).
//! **Remaining:** the gap-boundary *weave* (lower `emit_keccak256` over the
//! committed boundary wires + constrain its output to the same digest, in the
//! hybrid/storage prover+verifier, `docs/boundary-link-embedding.md` §VOLE side);
//! plus the in-circuit composition of the Keccak block into the folded instance +
//! Fiat–Shamir (refinements, `docs/vcb-ivc-folding.md` §5).

#![no_std]
extern crate alloc;

pub mod scalar;
pub mod pedersen;
pub mod keccak;
pub mod keccak_r1cs;
pub mod r1cs;
pub mod nifs;
pub mod verify;
pub mod ivc;
pub mod link;
pub mod bridge_adapter;
pub mod transport;

pub use scalar::Scalar;
