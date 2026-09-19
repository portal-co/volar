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

pub mod aes_extern;
pub mod aes_gadget;
/// Provider-neutral validation and inline composition for finite FHE circuit
/// programs. See `docs/fhe/circuit-provider-abi.md`.
pub mod circuit_provider;
pub mod circuit_provider_storage;
pub mod embedder;
/// Dependency-aware batching for deferred actions, pure oracles, and storage
/// requests at MPC circuit boundaries.
pub mod external_boundary;
pub mod faest_owf;
/// Stateless fused-GC handoff after an imported FHE module exchanges its
/// ciphertext/plaintext boundary values.
pub mod fhe_transition;
pub mod held_remap;
/// Provider-neutral scheduling for pre-FHE ORAM reads, lazy held-material
/// openings, and existing public-address storage optimizations.
pub mod hybrid_storage;
pub mod oram_2pc;
pub mod oram_batch;
pub mod oram_ciphertext_tree;
pub mod oram_gadget;
pub mod oram_host;
pub mod oram_lower;
pub mod oram_material;
pub mod oram_material_store;
pub mod oram_split;
pub mod partition;
pub mod schedule;
pub mod sha_gadget;
pub mod tls13;
pub mod tls13_2pc;
#[cfg(feature = "std")]
pub mod tls13_2pc_correlate;
pub mod tls13_extern;
pub mod tls13_held;
#[cfg(feature = "std")]
pub mod tls13_live;
pub mod x25519_gadget;

pub use circuit_provider::{
    CiphertextUse, CircuitProviderComposition, CircuitProviderError, CircuitProviderGeometry,
    CircuitProviderPreparationError, CircuitProviderPrograms, DecryptReason, KeyUse,
    ProviderProgramKind, ProviderWireTracker, ValidatedCircuitProvider, WireConversionDemand,
    WireResidence, optimize_circuit_provider_composition, prepare_unbounded_provider_program,
};
pub use circuit_provider_storage::{
    BaseCopyRange, BaseStorageCell, BaseStorageEpoch, BaseStorageId, InvocationCache,
    ProviderCacheFinish, ProviderCacheId, ProviderCacheRegistry, ProviderInvocationId,
    ProviderLoopGeometry, ProviderLoopResult, ProviderLoopStep, ProviderStorageInvocation,
    ReadonlyAccessKind, ReadonlyStorageError, ReadonlyStorageLayout, ReadonlyStorageManifest,
    ReadonlyStorageRequest, ReadonlyStorageSource, SecretBaseRead,
};
pub use embedder::{VcEmbedder, VcOutcome, VcVisibility};
pub use external_boundary::{
    ExternalBatch, ExternalBatchLimits, ExternalBoundaryError, ExternalBoundaryPlan,
    ExternalRequest, ExternalRequestId, ExternalRequestKind, plan_external_boundaries,
};
pub use fhe_transition::{FheTransitionError, fuse_after, isolate_stateless_module};
pub use hybrid_storage::{
    DeferredChunkId, DeferredInputSlot, HeldMarkerTunnel, HeldMarkerTunnelError,
    HeldMaterialMarker, HeldMaterialPreOpen, InferredSelect, InferredSelectFragment,
    InferredSelectGraph, InferredSelectNode, OramReadPreRun, OramReadToken, PreFheStoragePlan,
    PreFheStoragePlanError, PublicAddressStoragePlan, TunneledHeldMaterialMarker,
    infer_selects_from_poly, tunnel_held_material_markers,
};
pub use oram_host::{GramEvalDrive, OramHost, OramHostError, OramHostShim};
pub use partition::partition_from_sides;
pub use schedule::{ScheduleError, compile_schedule, compile_schedule_optimized};
