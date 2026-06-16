// @reliability: experimental
// @ai: assisted
//! External bytecode artefact produced by [`crate::virtualize_ir`] /
//! [`crate::virtualize_bir`].
//!
//! The artefact is an inert data structure — it contains no IR
//! statements, no variable ids, no crypto state.  Backends consume it by
//! emitting a `const` array plus a small dispatch shim that reads
//! `(handler_idx, immediates)` tuples and calls the matching handler.
//!
//! The canonical storage form is [`PreInitSegment`] entries on the output
//! module; this artefact is a structured side view of the same data.

use alloc::vec::Vec;

use crate::canon::ImmediateKind;
use volar_ir::ir::IRBlockId;
use volar_ir_common::Constant;

/// The shape of one handler's immediate parameters.
///
/// `kinds[i]` is the kind of the i-th immediate the handler consumes.  All
/// bytecode entries whose `handler_idx` matches this schema must supply
/// exactly `kinds.len()` immediates in the same order.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct HandlerImmSchema {
    pub kinds: Vec<ImmediateKind>,
}

/// Kind of an appended bytecode region (after outer program rows).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AppendedRegionKind {
    /// Stepping sub-interpreter (cross-block dedup).
    SharedCore {
        members: Vec<(usize, u32, u32)>,
    },
    /// Counted repeat — typically one descriptor row.
    RerollLoop {
        owner_block: usize,
        body_handler_idx: u32,
        trip_count: TripCount,
        operand_mode: OperandMode,
    },
}

/// How reroll operands are sourced (v1: register file only).
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum OperandMode {
    #[default]
    RegisterFile,
    /// Reserved for MUX-tree / RAM optimization (ADR deferred).
    RamMux,
}

/// Trip count for a rerolled loop.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TripCount {
    Fixed(u32),
    /// Index into the descriptor row's immediate slots.
    BytecodeSlot(usize),
}

/// Metadata for one appended chunk in the unified flat table.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AppendedRegionMeta {
    pub pc_start: u32,
    pub pc_end: u32,
    pub kind: AppendedRegionKind,
}

/// Row kind tag for unified bytecode entries.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum BytecodeRowKind {
    #[default]
    Outer,
    SharedCoreStep,
    RerollDescriptor,
}

/// A single bytecode entry — one row in the unified flat table.
///
/// `handler_idx` selects which handler runs for this pc; `consts` and
/// `targets` are the concrete values threaded into the handler via
/// immediate parameters (parallel to the schema stored in
/// [`VirtBytecode::handler_schemas`]).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BytecodeEntry {
    pub handler_idx: u32,
    pub consts: Vec<Constant>,
    pub targets: Vec<IRBlockId>,
    pub row_kind: BytecodeRowKind,
}

/// Full bytecode artefact returned by the pass.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VirtBytecode {
    /// Number of unique handlers (`== handler_schemas.len()`).
    pub n_handlers: usize,
    /// Per-handler immediate schema.  Indexed by `handler_idx`.
    pub handler_schemas: Vec<HandlerImmSchema>,
    /// One entry per global pc (outer rows + appended rows).
    pub entries: Vec<BytecodeEntry>,
    /// Rows `0..outer_block_count` are the outer program.
    pub outer_block_count: usize,
    /// Appended region metadata.
    pub regions: Vec<AppendedRegionMeta>,
}

impl VirtBytecode {
    /// Construct an empty artefact.
    pub fn new() -> Self {
        Self {
            n_handlers: 0,
            handler_schemas: Vec::new(),
            entries: Vec::new(),
            outer_block_count: 0,
            regions: Vec::new(),
        }
    }

    /// Number of entries in the bytecode table.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the bytecode table is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Default for VirtBytecode {
    fn default() -> Self {
        Self::new()
    }
}

impl BytecodeEntry {
    pub fn outer(handler_idx: u32, consts: Vec<Constant>, targets: Vec<IRBlockId>) -> Self {
        Self {
            handler_idx,
            consts,
            targets,
            row_kind: BytecodeRowKind::Outer,
        }
    }
}
