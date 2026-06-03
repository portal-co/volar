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

// ============================================================================
// Resilience layer — hybrid (network-resilient) VOLE sessions
// ============================================================================
//
// The hybrid network VOLE weaver (`volar-weaver::weave_hybrid_net_vole_*`)
// reacts to the network cutting off mid-proof: it switches to a cleartext
// (non-ZK) "no-op" gap path that consumes no VOLE correlations, then runs a
// **resumption bridge** on reconnect.  Generated code is strategy-agnostic — it
// calls only the [`ResilientVoleTransport`] methods below, so the resumption
// bridge can be swapped (best-effort / VCB-RX / VCB-IVC) without regenerating.
//
// See `docs/vole-continuation-bridge.md` for the cryptographic specification.

/// Outcome classification of a non-propagating iteration send.
///
/// The generated hybrid loop uses [`ResilientVoleTransport::try_send_iteration`]
/// (which returns `Result<bool, _>`) so it can *branch* on a disconnect instead
/// of `?`-propagating it.  This enum documents the three logical outcomes and is
/// available to transport implementors that want a richer classification API.
pub enum SendOutcome<E> {
    /// Send succeeded; stay on the ZK streaming path.
    Continue,
    /// Recoverable disconnect; enter the cleartext gap path and try to reconnect.
    Disconnect,
    /// Unrecoverable error; the session must abort, surfacing this error.
    Fatal(E),
}

/// Material identifying the re-committed boundary state after a gap.
///
/// For the best-effort strategy this is just the fresh VOLE commitments to the
/// resumed live state `S_{k+m}`.  For the sound VOLE Continuation Bridge it also
/// carries the boundary commitments / folding proof (see the design doc); those
/// fields are added by the concrete strategy implementation.
pub struct ResumeToken<N: ArraySize, T> {
    /// The resumed live-state wires. For the sound replay-from-anchor bridge
    /// these are the *anchor* wires (the iteration-input state the verifier
    /// still holds `Q`s for); the gap is then re-proven by replaying from them.
    /// For a re-commitment strategy they are fresh `Vope` commitments instead.
    pub resume_state: Vec<Vope<N, T, U1>>,
    /// Carried multiset-hash memory accumulator wires (`mem_produce`,
    /// `mem_consume`) in Commitment storage mode — a single field element each,
    /// so memory is carried across the gap in O(1) regardless of cell count.
    /// Empty when the circuit has no committed storage.
    pub mem_acc: Vec<Vope<N, T, U1>>,
}

/// Whether the unauthenticated gap `[start, end)` was retroactively proven.
pub enum GapVerdict {
    /// The gap was bridged by a sound continuation proof (VCB).
    Proven,
    /// The gap ran in the clear and is *not* proven; the final verdict is
    /// qualified "valid except iterations `[start, end)`".
    Unproven { start: u32, end: u32 },
}

/// Network transport with disconnect-aware resumption, layered on
/// [`VoleTransport`].
///
/// All methods have best-effort default implementations, so any existing
/// `VoleTransport` implementor opts in with a bare
/// `impl ResilientVoleTransport<N, T> for MyTransport {}`.  Override the methods
/// to add error classification, real reconnection, or a sound resumption bridge
/// (see `docs/vole-continuation-bridge.md`).
pub trait ResilientVoleTransport<N: ArraySize, T>: VoleTransport<N, T> {
    /// Send one iteration's hats, classifying any error instead of propagating.
    ///
    /// Returns `Ok(true)` if the iteration was sent (stay on the ZK path),
    /// `Ok(false)` on a recoverable disconnect (enter the cleartext gap path),
    /// or `Err(_)` for an unrecoverable error (the generated code `?`-propagates
    /// it).  Default: any transport error maps to a recoverable `Ok(false)`.
    fn try_send_iteration(&mut self, hats: &[Array<T, N>], is_sentinel: bool) -> Result<bool, Self::Error> {
        Ok(self.send_iteration(hats, is_sentinel).is_ok())
    }

    /// Attempt to re-establish the connection. `Ok(true)` = reconnected.
    ///
    /// Default: `Ok(false)` — "reconnection unsupported"; the generated gap loop
    /// then runs the cleartext path to completion.
    fn try_reconnect(&mut self) -> Result<bool, Self::Error> {
        Ok(false)
    }

    /// Prover side of the resumption bridge after a gap of `gap_len` iterations.
    ///
    /// Default (best-effort): no-op — the resumed state rides on the re-opened
    /// streaming loop / VOLE setup, and the gap is left unproven.
    fn prover_bridge(&mut self, _token: &ResumeToken<N, T>, _gap_len: u32) -> Result<(), Self::Error> {
        Ok(())
    }

    /// Verifier side of the resumption bridge.  Returns whether the `gap_len`
    /// gap was retroactively proven.
    ///
    /// Default (best-effort): returns [`GapVerdict::Unproven`] with a
    /// gap-relative interval `[0, gap_len)`; the generated verifier offsets it by
    /// its absolute iteration counter and records the qualified verdict.
    fn verifier_bridge(&mut self, gap_len: u32) -> Result<GapVerdict, Self::Error> {
        Ok(GapVerdict::Unproven { start: 0, end: gap_len })
    }

    /// Draw `bits.len()` **fresh** VOLE correlations from the transport's setup
    /// pool and commit each bit, returning the prover-side `Vope` wires.
    ///
    /// This is the real re-commitment primitive (cf. the structural
    /// [`recommit_bit`] placeholder).  The single-process `vole_commit_bit`
    /// (`volar_spec::vole::setup`) emits *both* halves and so cannot be called
    /// from two-party generated code; instead the transport owns the prover's
    /// correlation pool and the verifier obtains the matching `Q`s from its own
    /// pool during the bridge handshake.
    ///
    /// Used by the dynamic-skip / XOR-key re-commitment path
    /// (`docs/vole-continuation-bridge.md` §C).  The default panics — only
    /// transports that re-commit (rather than replay from the anchor) need it.
    fn fresh_commit(&mut self, _bits: &[bool]) -> Result<Vec<Vope<N, T, U1>>, Self::Error> {
        unimplemented!("fresh_commit: implement for transports that re-commit resumed state")
    }
}

/// Re-commit a cleartext bit into a degree-1 VOPE wire after a gap.
///
/// **Placeholder re-commitment.** A real resumption must draw a *fresh* VOLE
/// correlation for the resumed wire (see `docs/vole-continuation-bridge.md`
/// §3.2); this helper instead encodes the bit with the *public* constant wire
/// `one` (`1` → `one`, `0` → `one + one`, which is the field zero in `GF(2^k)`).
/// It is structurally valid and lets the generated hybrid prover resume the
/// streaming loop, but it is **not hiding** — replace it with a setup-backed
/// fresh commitment when implementing a sound [`GapVerdict::Proven`] bridge.
pub fn recommit_bit<N, T>(one: &Vope<N, T, U1>, bit: bool) -> Vope<N, T, U1>
where
    N: VoleArray<T>,
    T: Clone + core::ops::Add<Output = T>,
    Vope<N, T, U1>: Clone + core::ops::Add<Output = Vope<N, T, U1>>,
{
    if bit {
        one.clone()
    } else {
        one.clone() + one.clone()
    }
}
