// @reliability: experimental
// @ai: assisted
//! Transport-agnostic network interface for Volar VOLE-ZK proving and verifying.
//!
//! This crate defines the [`VoleTransport`] trait that generated network-aware
//! prover/verifier functions (produced by `volar-weaver`'s `net` module) accept
//! as a parameter.  Concrete implementations — TCP, WebSocket, in-memory pipes —
//! are the user's responsibility; this crate contains only the typed interface.
//!
//! ## Protocol shape
//!
//! All interaction is typed in terms of VOLE-native types (`Array<T, N>`, `bool`)
//! rather than raw bytes.  Serialisation and framing are left to the transport.
//!
//! Three method groups exist on a single trait:
//!
//! - **Single-shot**: prover sends all AND-gate hats at once; verifier replies.
//! - **Streaming loop**: prover sends one hat-batch per loop iteration with a
//!   `is_sentinel` flag that signals loop termination; verifier replies after
//!   the sentinel.
//! - **Storage (Commitment mode)**: prover relays memory-access trace entries
//!   so the verifier can run the offline multiset hash check.

#![no_std]
extern crate alloc;

use alloc::vec::Vec;
use hybrid_array::{Array, ArraySize};
use typenum::U1;
use volar_spec::vole::{VoleArray, Vope};

/// Pluggable IO interface for VOLE-ZK network sessions.
///
/// The generated prover/verifier functions (from `volar-weaver` with the `net`
/// feature) take `&mut impl VoleTransport<N, T>` as a parameter.  You supply
/// the concrete type — it can wrap a TCP stream, a WebSocket, an in-memory
/// channel, or anything else that can exchange the VOLE-native message types.
///
/// All methods return `Result<_, Self::Error>`.  Error handling is `?`-propagated
/// in the generated code so the session function returns early on IO failure.
pub trait VoleTransport<N: ArraySize, T> {
    /// Transport-level error type.
    type Error;

    // ── Single-shot ──────────────────────────────────────────────────────────

    /// Prover → Verifier: send all AND-gate hat values at once.
    ///
    /// Called once by the prover at the end of proof computation.
    fn send_hats(&mut self, hats: &[Array<T, N>]) -> Result<(), Self::Error>;

    /// Verifier ← Prover: receive the AND-gate hat values.
    ///
    /// `and_count` is the statically-known number of AND gates in the circuit;
    /// the verifier uses it to know how many elements to expect.
    fn recv_hats(&mut self, and_count: usize) -> Result<Vec<Array<T, N>>, Self::Error>;

    /// Verifier → Prover: send the proof verdict.
    fn send_verdict(&mut self, ok: bool) -> Result<(), Self::Error>;

    /// Prover ← Verifier: receive the proof verdict.
    fn recv_verdict(&mut self) -> Result<bool, Self::Error>;

    // ── Streaming loop ───────────────────────────────────────────────────────

    /// Prover → Verifier: send one iteration's AND-gate hats.
    ///
    /// `is_sentinel = true` signals that the loop's done-flag fired on this
    /// iteration; the verifier will send the verdict after receiving the
    /// sentinel.
    fn send_iteration(
        &mut self,
        hats: &[Array<T, N>],
        is_sentinel: bool,
    ) -> Result<(), Self::Error>;

    /// Verifier ← Prover: receive one iteration's hats plus the sentinel flag.
    ///
    /// `and_count` is the number of AND gates per iteration (statically known
    /// from the circuit).  The returned `bool` is `true` if this is the final
    /// iteration (sentinel received).
    fn recv_iteration(
        &mut self,
        and_count: usize,
    ) -> Result<(Vec<Array<T, N>>, bool), Self::Error>;

    // ── Storage (Commitment mode) ─────────────────────────────────────────────

    /// Prover → Verifier: send all memory-trace entries for the offline
    /// multiset consistency check (only used in Commitment storage mode).
    fn send_storage_entries(
        &mut self,
        entries: &[VoleStorageEntry<N, T>],
    ) -> Result<(), Self::Error>;

    /// Verifier ← Prover: receive `count` memory-trace entries.
    fn recv_storage_entries(
        &mut self,
        count: usize,
    ) -> Result<Vec<VoleStorageEntry<N, T>>, Self::Error>;
}

/// One oracle-read memory access entry sent from prover to verifier.
///
/// Carries the committed wire values (the `u`-component of each VOPE) so the
/// verifier can reconstruct the multiset hash check independently.
pub struct VoleStorageEntry<N: ArraySize, T> {
    /// `u`-component (first array lane) of the address wire's VOPE.
    pub addr: Array<T, N>,
    /// `u`-component (first array lane) of the value wire's VOPE.
    pub value: Array<T, N>,
    /// `true` for write accesses, `false` for reads.
    pub is_write: bool,
    /// Monotonically increasing timestamp within the proof.
    pub timestamp: u32,
}

/// Extract the boolean bit from the first lane of a degree-1 VOPE.
///
/// A degree-1 VOPE `(u, v)` represents a committed wire `x` where `u[0]`
/// encodes the actual bit value of `x` in the field `T`.  This function reads
/// `u[0][0]` (the first element of the first coefficient array) and compares it
/// to `T::default()` (the field zero).
///
/// This is used by generated loop code to test the circuit's done-flag wire.
pub fn vope_bit<N, T>(v: &Vope<N, T, U1>) -> bool
where
    N: VoleArray<T>,
    T: Default + PartialEq,
{
    &v.u[0][0] != &T::default()
}
