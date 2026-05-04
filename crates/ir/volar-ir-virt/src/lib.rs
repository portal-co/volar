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
//! # Bytecode forms
//!
//! * [`BytecodeForm::InIr`] — the setup block `StorageWrite`s every
//!   bytecode entry into a dedicated [`StorageId`] before dispatch.  Keeps
//!   the entire transform inside the Volar IR / Boolar IR semantics.
//! * [`BytecodeForm::External`] — a [`VirtBytecode`] data artifact is
//!   returned alongside the IR for backends (Rust / TS / C) that prefer to
//!   materialise the table as a `const` array plus a small runtime shim.
//! * [`BytecodeForm::Both`] (default) — emit both.

extern crate alloc;

pub mod bir;
pub mod bytecode;
pub mod canon;
pub mod ctx;
pub mod hash;
pub mod ir;

pub use bytecode::{BytecodeEntry, HandlerImmSchema, VirtBytecode};
pub use canon::{BirHandlerKey, BlockImmediates, HandlerKey, ImmediateKind, IrHandlerKey};
pub use ctx::VirtOutput;
pub use hash::{CommitmentConfig, IrEmitter, IrHashAlgorithm, XorFoldHash32};
pub use ir::{virtualize_ir, virtualize_ir_committed};
pub use bir::virtualize_bir;

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

/// Where the bytecode table is materialised.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BytecodeForm {
    /// Setup block initialises a [`StorageId`] with the bytecode, and the
    /// dispatcher fetches entries via [`StorageRead`].
    ///
    /// [`StorageRead`]: volar_ir_common::Stmt::StorageRead
    InIr,
    /// Bytecode is returned as a [`VirtBytecode`] data artifact on the side.
    /// The IR output contains a dispatcher that assumes immediates are
    /// supplied by the backend's runtime shim rather than emitted via
    /// `StorageRead`.
    External,
    /// Emit both forms: the IR setup block seeds the storage, and the
    /// [`VirtBytecode`] artifact is returned for backends that want it.
    Both,
}

impl BytecodeForm {
    /// Whether this form asks the pass to emit the in-IR setup block.
    pub fn wants_in_ir(self) -> bool {
        matches!(self, BytecodeForm::InIr | BytecodeForm::Both)
    }
    /// Whether this form asks the pass to return a [`VirtBytecode`] artifact.
    pub fn wants_external(self) -> bool {
        matches!(self, BytecodeForm::External | BytecodeForm::Both)
    }
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
    pub bytecode_form: BytecodeForm,
    pub dedup: DedupPolicy,
    /// Storage space used to hold the bytecode table when
    /// [`BytecodeForm::wants_in_ir`] is true.  Must not collide with any
    /// `StorageId` the input module already reads from or writes to.
    pub bytecode_storage: StorageId,
    /// When `true`, handlers jump directly to their successor handler via an
    /// inline `JumpTable`, eliminating the two-block dispatcher→dispatch
    /// indirection on the hot path.  Each handler arm emits one extra
    /// dispatch sub-block.  Only supported for IR (`virtualize_ir`); BIR
    /// direct dispatch is not yet implemented (would cause O(n²) block growth).
    pub direct_dispatch: bool,
}

impl Default for VirtualizeConfig {
    fn default() -> Self {
        Self {
            dispatch: DispatchMode::Public,
            bytecode_form: BytecodeForm::Both,
            dedup: DedupPolicy::ConstantsAndTargets,
            bytecode_storage: StorageId::VIRT_BYTECODE,
            direct_dispatch: false,
        }
    }
}
