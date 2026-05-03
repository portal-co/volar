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
//! The in-IR bytecode form (a setup block full of `StorageWrite`s) is
//! emitted alongside whenever [`crate::BytecodeForm::wants_in_ir`] is
//! true; the two forms carry identical information.

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

/// A single bytecode entry — one row per original block index.
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
}

/// Full bytecode artefact returned by the pass.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VirtBytecode {
    /// Number of unique handlers (`== handler_schemas.len()`).
    pub n_handlers: usize,
    /// Per-handler immediate schema.  Indexed by `handler_idx`.
    pub handler_schemas: Vec<HandlerImmSchema>,
    /// One entry per original block.  `entries[pc].handler_idx` must be
    /// `< n_handlers` and the entry's immediates must match
    /// `handler_schemas[entries[pc].handler_idx]`.
    pub entries: Vec<BytecodeEntry>,
}

impl VirtBytecode {
    /// Construct an empty artefact.
    pub fn new() -> Self {
        Self {
            n_handlers: 0,
            handler_schemas: Vec::new(),
            entries: Vec::new(),
        }
    }

    /// Number of entries in the bytecode table (= original block count).
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
