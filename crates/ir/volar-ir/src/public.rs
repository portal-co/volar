// @reliability: normal
// @ai: assisted
//! Public-variable tracking for the CFG weaver.
//!
//! A [`PublicSet`] records which SSA variables are known to hold
//! cleartext (public) values at compile time.  The weaver uses this to
//! decide whether a branch condition can be lowered to a direct Rust
//! `if/else` or must be handled obliviously (e.g. via CMUX).
//!
//! **Rules:**
//! - `Const` stmts are always public.
//! - `Transmute` from a public source is public.
//! - Everything else is treated as encrypted unless explicitly marked.

use alloc::collections::BTreeSet;
use crate::ir::IRVarId;

/// Tracks which SSA variables carry cleartext (public) values.
///
/// `IRVarId`s are stored by their inner `u32` index to avoid a dependency
/// on the full `IRVarId` equality impl in `BTreeSet`.
#[derive(Clone, Debug, Default)]
pub struct PublicSet(BTreeSet<u32>);

impl PublicSet {
    /// Create an empty set (no variables are public).
    pub fn new() -> Self {
        PublicSet(BTreeSet::new())
    }

    /// Mark `v` as a public (cleartext) variable.
    pub fn mark_public(&mut self, v: IRVarId) {
        self.0.insert(v.0);
    }

    /// Returns `true` if `v` is known to be public.
    pub fn is_public(&self, v: IRVarId) -> bool {
        self.0.contains(&v.0)
    }

    /// If all `srcs` are public, mark `dst` as public too and return `true`.
    ///
    /// Used for propagating publicness through `Transmute` and other
    /// pure bit-renaming operations.
    pub fn propagate_if_all_public(&mut self, srcs: &[IRVarId], dst: IRVarId) -> bool {
        if srcs.iter().all(|s| self.is_public(*s)) {
            self.mark_public(dst);
            true
        } else {
            false
        }
    }
}
