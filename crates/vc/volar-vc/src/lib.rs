// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! # volar-vc — a vc-spec two-party verifiable-compute embedder
//!
//! This crate implements the **embedder** half of [vc-spec Draft 0.1]
//! (two-party verifiable computation over WebAssembly): the local and remote
//! parties each run an embedder that invokes an unmodified guest, with each
//! argument tagged `public` / `private` / `blind`, and results revealed to
//! both parties. The embedder drives Volar's semi-honest garbled-circuit MPC
//! (`volar-mpc`) as the execution engine and Volar's vc WAFFLE→VAFFLE lowering
//! (`volar-vaffle-target`'s `vc.rs`, surfaced as `BIrBlocks`) as the guest
//! compiler.
//!
//! [vc-spec Draft 0.1]: https://sinui0.github.io/vc-spec
//!
//! ## What lives here
//!
//! - [`GateSchedule` compilation](schedule): turn a circuit-fused
//!   [`BIrBlocks`] into a [`volar_mpc::GateSchedule`], so any circuit the
//!   weaver produces can run through the MPC session layer (not just
//!   hand-built schedules). `Or` gates are pre-expanded by De Morgan, exactly
//!   as the weaver does.
//! - [`partition`](partition): bridge per-input-bit [`SideId`] metadata to the
//!   session layer's [`InputOwner`] partition, from the local party's
//!   perspective (public = known to both, local private = garbler, remote
//!   blind = evaluator).
//! - The [`embedder`] API: `invoke`, `mem_write`, `mem_read`, `mem_reveal`,
//!   with vc-spec outcome mapping (`Value` / `Trap` / `Abort` / `Error`).
//!
//! ## vc-spec implementation-defined behaviors (required documentation)
//!
//! The spec leaves the following to each embedder; this crate's choices:
//!
//! - **Control flow on symbolic values: supported, by movfuscation.** The
//!   pipeline collapses a guest's branching CFG into a single oblivious
//!   dispatch block, so every branch is *evaluated* and the selected result
//!   forwarded; there is no early-exit. Cost: the circuit size is the sum of
//!   all branch bodies (no dynamic dispatch savings), which is the documented
//!   oblivious-execution cost model.
//! - **Symbolic addressing: supported via the MUX/GRAM storage layer.**
//!   Reads/writes with a symbolic (private/blind) address lower to a
//!   linear-scan MUX over the bounded memory image (Workstream A adds the
//!   sub-linear GRAM gadget); small memories always take the MUX path. The
//!   address bound is the guest's declared memory pages.
//! - **Resource limits:** the circuit is materialized in full before
//!   garbling, so guest size is bounded by host memory; there is no fuel. The
//!   embedder reports oversized guests as [`VcOutcome::Abort`].
//! - **Abort reporting:** a transport or protocol failure (malformed frame,
//!   premature close, OT failure) maps to [`VcOutcome::Abort`]; a genuine
//!   WASM trap maps to [`VcOutcome::Trap`]; `mem_read` of symbolic bytes maps
//!   to [`VcOutcome::Error`]. Aborts are distinguishable from traps, as the
//!   spec requires.
//! - **Host functions:** only the VCI reveal/wait imports (`vc.reveal_i32` /
//!   `i64` + `_wait`) are recognized; any other import lowers to an
//!   oracle/action extern per `WaffleImportConfig`.
//!
//! ## Security position
//!
//! Semi-honest (honest-but-curious) two-party security. The evaluator learns
//! only the output labels it is entitled to (its own inputs arrive via
//! 1-of-2 OT, never in cleartext); the garbler learns nothing about the
//! evaluator's inputs beyond the output. Malicious-security tiers
//! (cut-and-choose, authenticated garbling, DEAP-style deferred consistency)
//! are out of scope for this crate's `very-unstable` landing.

#![no_std]

extern crate alloc;

pub mod embedder;
pub mod partition;
pub mod schedule;

pub use embedder::{VcEmbedder, VcOutcome, VcVisibility};
pub use partition::partition_from_sides;
pub use schedule::{ScheduleError, compile_schedule};
