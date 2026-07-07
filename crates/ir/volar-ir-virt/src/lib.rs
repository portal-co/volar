#![no_std]
// @reliability: experimental
// @ai: assisted
//! Virtualization transform for Volar IR (`IRBlocks`) and Boolar IR
//! (`BIrBlocks`).
//!
//! The pass converts a multi-block module into a bounded set of
//! *instruction handlers* (one per unique block skeleton, modulo constant
//! values and jump target ids) plus an interpreter loop that fetches
//! `(handler_idx, immediates)` tuples from a bytecode table.  The main
//! compile-time win is that the backend prints (or lowers) one body per
//! unique handler rather than one body per original block.
//!
//! See [`VirtualizeConfig`] for the knobs and [`virtualize_ir`] /
//! [`virtualize_bir`] for the concrete entry points.
//!
//! # Dispatch modes
//!
//! * [`DispatchMode::Public`] (default) — the dispatcher emits an
//!   [`IRTerminator::JumpTable`] (IR) or a balanced `CondJmp` tree (BIR) on
//!   the handler index.  Assumes the PC is public (i.e. the computed
//!   `handler_idx` does not depend on any witness wire); this is true of
//!   every Volar program whose control flow is known to all parties.
//! * [`DispatchMode::Oblivious`] — the dispatcher runs every handler every
//!   step and multiplexes outputs with `is_active · val + (1-is_active) ·
//!   fallback` accumulators.  Safe for witness-dependent control flow at a
//!   larger per-step cost.
//!
//! # Storage initialization
//!
//! Static bytecode table and per-slot storage lanes are emitted as
//! [`PreInitSegment`] entries on the output module's `pre_init` field
//! (same construct WASM data segments and the VOLE weaver use).  The setup
//! block only performs dynamic work (keyed commitment params, entry-param
//! register routing).  A structured [`VirtBytecode`] side artifact is always
//! returned alongside the module for tests and backends.

extern crate alloc;

pub mod adaptive_emit;
pub mod adaptive_cfg;
pub mod bir;
pub mod bytecode;
pub mod canon;
pub mod ctx;
pub mod hash;
pub mod ir;
pub mod layout;
pub mod preinit;
pub mod cfg_hints;
pub mod split;

pub use adaptive_cfg::AdaptiveSplitConfig;
pub use bytecode::{
    AppendedRegionKind, AppendedRegionMeta, BytecodeEntry, BytecodeRowKind, HandlerImmSchema,
    OperandMode, TripCount, VirtBytecode,
};
pub use canon::{BirHandlerKey, BlockImmediates, HandlerKey, ImmediateKind, IrHandlerKey};
pub use ctx::VirtOutput;
pub use hash::{CommitmentConfig, IrEmitter, IrHashAlgorithm, SipHash48, XorFoldHash32};
pub use ir::{virtualize_ir, virtualize_ir_committed};
pub use bir::virtualize_bir;
pub use split::plan_adaptive_split;

use volar_ir_common::StorageId;

/// How the dispatcher routes control to the active handler every step.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DispatchMode {
    /// Cheap public dispatch via `IRTerminator::JumpTable` (IR) or a balanced
    /// `CondJmp` tree (BIR).  Assumes the handler index is a public value.
    Public,
    /// Oblivious dispatch via movfuscate-style accumulators — every handler
    /// runs every step, outputs combined with `is_active · val + fallback`.
    Oblivious,
}

/// How aggressively blocks are canonicalised before deduplication.
///
/// Only [`DedupPolicy::ConstantsAndTargets`] is implemented in v1.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum DedupPolicy {
    /// Lift `Stmt::Const` values, the constant term of `Stmt::Poly`, and
    /// every jump target block id into per-block immediates.  Everything
    /// else is structural.
    ConstantsAndTargets,
    /// Also lift scalar fields that only change by parameter (e.g.
    /// `Rol.n`, `Ror.n`, `Shuffle.result_bits`, BIR storage ids, per-call
    /// names).  Reserved for a future pass — currently panics.
    Maximal,
}

/// Configuration knobs for [`virtualize_ir`] / [`virtualize_bir`].
#[derive(Clone, Debug)]
pub struct VirtualizeConfig {
    pub dispatch: DispatchMode,
    pub dedup: DedupPolicy,
    /// Storage space used to hold the bytecode table (seeded via `pre_init`).
    /// Must not collide with any `StorageId` the input module already reads
    /// from or writes to.
    pub bytecode_storage: StorageId,
    /// When `true`, handlers jump directly to their successor handler via an
    /// inline `JumpTable`, eliminating the two-block dispatcher→dispatch
    /// indirection on the hot path.  Each handler arm emits one extra
    /// dispatch sub-block.  Only supported for IR (`virtualize_ir`); BIR
    /// direct dispatch is not yet implemented (would cause O(n²) block growth).
    pub direct_dispatch: bool,
    /// Adaptive split: SharedCore cross-block dedup and RerollLoop intra-block
    /// rerolling. See [`AdaptiveSplitConfig`] and `docs/agent-context/virt-adaptive-split-adr.md`.
    pub adaptive_split: AdaptiveSplitConfig,
}

impl Default for VirtualizeConfig {
    fn default() -> Self {
        Self {
            dispatch: DispatchMode::Public,
            dedup: DedupPolicy::ConstantsAndTargets,
            bytecode_storage: StorageId::VIRT_BYTECODE,
            direct_dispatch: false,
            adaptive_split: AdaptiveSplitConfig::default(),
        }
    }
}
