#![no_std]
// @reliability: normal
//! @ai: assisted
//! Generic per-value "side" tagging for multi-actor/multi-party IR pipelines.
//!
//! This crate defines [`SideId`] and [`SideHandler`], the sibling mechanism to
//! `volar_provenance`'s [`ProvenanceHandler`](https://docs.rs/volar-provenance)
//! for a different axis of per-value metadata: *which actor, party, or role a
//! value belongs to* (e.g. ZK witness vs. statement, FHE plaintext vs.
//! ciphertext, garbler vs. evaluator), and therefore what protection it needs.
//!
//! # Design invariant: a side's meaning is never invented here
//!
//! [`SideId`] is an opaque numeric identifier with no built-in semantics —
//! exactly as `IRVarId`/`StorageId` carry no semantics beyond identity. Only a
//! [`SideHandler`] implementation decides what a given side (or the absence of
//! one) means for its domain. This crate ships no ZK/FHE/garbling-specific
//! vocabulary; that vocabulary is defined downstream, next to the code that
//! consumes it (e.g. `VoleProtection`, `FheProtection`).
//!
//! # Relationship to other Volar systems
//!
//! - **Independent of `volar-discipline`**: `Tagged<Zk/Transparent, T>` is a
//!   whole-module, compile-time typestate boundary. Side is a within-IR,
//!   per-value concept. Neither is layered on or derived from the other.
//! - **Sibling of `volar-provenance`**: provenance (`P`) is free-form,
//!   caller-chosen debug/tooling metadata threaded through as a generic type
//!   parameter. A side is always "which of N parties/roles owns this value" —
//!   represented as one fixed, opaque id attached directly to a node, not a
//!   second generic parameter threaded through every IR type's signature.
//!
//! # Built-in handlers
//!
//! | Handler | Output | Behaviour |
//! |---------|--------|-----------|
//! | [`UniformProtection<T>`] | `T` | Every side (and `None`) maps to the same fixed value |
//! | [`TableProtection<T>`] | `T` | Explicit per-`SideId` map with a default fallback |
//!
//! # Default propagation
//!
//! [`propagate`] implements the common rule for values *derived* from
//! operands (binary/unary ops, field projections, …): inherit the operands'
//! common side if they agree, otherwise `None`. Only true introduction points
//! (literals, params, oracle/action outputs) need an explicit side from the
//! frontend or weaver config.

extern crate alloc;

use alloc::collections::btree_map::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

/// Opaque identifier naming which actor/party/role a value belongs to.
///
/// Carries no semantics of its own — meaning is assigned entirely by a
/// [`SideHandler`], parallel to how `IRVarId`/`StorageId` are opaque indices.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[cfg_attr(feature = "rkyv", rkyv(attr(derive(PartialEq, Eq, PartialOrd, Ord))))]
pub struct SideId(pub u32);

/// Strategy that resolves a side identifier into the protection a value
/// belonging to that side requires.
///
/// This is the single audited place that decides "what does this side mean
/// *here*" — implementations replace scattered `Vec<bool>`/`BTreeSet<u32>`
/// flags with one type-checked call site. Dependents (inside this repo or in
/// their own multi-party codebases) supply their own implementation; this
/// crate only defines the contract.
pub trait SideHandler {
    /// The protection (or role, or any other domain-specific meaning)
    /// resolved for a side.
    type Protection;

    /// Resolve `side`'s protection. Implementations decide what an absent
    /// side (`None`) means for their domain — this trait stays policy-free.
    fn protection(&self, side: Option<SideId>) -> Self::Protection;
}

/// Default side-propagation rule for a value *derived* from operands: it
/// inherits the common side of its operands if they agree (including "all
/// unassigned"), or `None` if they disagree.
///
/// Disagreement is left to the caller/handler rather than guessed at — a
/// statement whose operands belong to different sides is exactly the
/// situation an introduction point (not propagation) should resolve
/// explicitly.
pub fn propagate(operands: &[Option<SideId>]) -> Option<SideId> {
    let mut result: Option<SideId> = None;
    for &operand in operands {
        match (result, operand) {
            (None, x) => result = x,
            (Some(a), Some(b)) if a == b => {}
            (Some(_), None) => {}
            (Some(_), Some(_)) => return None,
        }
    }
    result
}

// ============================================================================
// Built-in handlers
// ============================================================================

/// Side handler that maps every side (and the absence of one) to the same
/// fixed protection.
///
/// The zero-effort default for code that doesn't yet distinguish sides.
pub struct UniformProtection<T: Clone>(pub T);

impl<T: Clone> SideHandler for UniformProtection<T> {
    type Protection = T;
    #[inline]
    fn protection(&self, _side: Option<SideId>) -> T {
        self.0.clone()
    }
}

/// Side handler backed by an explicit `SideId -> Protection` map, with a
/// default for any side not present in the map (including `None`).
///
/// The direct, type-checked replacement for hand-rolled `BTreeSet<u32>` /
/// `Vec<bool>` flag collections.
#[derive(Clone, Debug)]
pub struct TableProtection<T: Clone> {
    table: BTreeMap<SideId, T>,
    default: T,
}

impl<T: Clone> TableProtection<T> {
    /// Create a table whose unassigned/unknown sides resolve to `default`.
    pub fn new(default: T) -> Self {
        Self { table: BTreeMap::new(), default }
    }

    /// Assign `protection` to `side`, returning `self` for chaining.
    pub fn with(mut self, side: SideId, protection: T) -> Self {
        self.table.insert(side, protection);
        self
    }

    /// Assign `protection` to `side` in place.
    pub fn insert(&mut self, side: SideId, protection: T) {
        self.table.insert(side, protection);
    }
}

impl<T: Clone> SideHandler for TableProtection<T> {
    type Protection = T;
    #[inline]
    fn protection(&self, side: Option<SideId>) -> T {
        side.and_then(|s| self.table.get(&s).cloned()).unwrap_or_else(|| self.default.clone())
    }
}

/// Human-readable name registry for [`SideId`]s, for debugging and printing.
///
/// Mirrors the id-interning pattern used by `volar_ir_common::TypeTable`.
/// Repeated [`intern`](Self::intern) calls with the same name return the same
/// id.
#[derive(Clone, Debug, Default)]
pub struct SideTable {
    names: Vec<String>,
}

impl SideTable {
    /// Create an empty side table.
    pub fn new() -> Self {
        Self { names: Vec::new() }
    }

    /// Intern `name`, returning its `SideId`. Subsequent calls with the same
    /// name return the same id rather than allocating a new one.
    pub fn intern(&mut self, name: &str) -> SideId {
        if let Some(idx) = self.names.iter().position(|n| n == name) {
            return SideId(idx as u32);
        }
        let id = SideId(self.names.len() as u32);
        self.names.push(String::from(name));
        id
    }

    /// Look up the name interned for `id`, if any.
    pub fn name(&self, id: SideId) -> Option<&str> {
        self.names.get(id.0 as usize).map(String::as_str)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    // ---- SideId --------------------------------------------------------------

    #[test]
    fn side_id_equality_is_by_value() {
        assert_eq!(SideId(3), SideId(3));
        assert_ne!(SideId(3), SideId(4));
    }

    // ---- UniformProtection -----------------------------------------------------

    #[test]
    fn uniform_protection_ignores_side() {
        let h = UniformProtection("witness");
        assert_eq!(h.protection(None), "witness");
        assert_eq!(h.protection(Some(SideId(0))), "witness");
        assert_eq!(h.protection(Some(SideId(7))), "witness");
    }

    // ---- TableProtection -------------------------------------------------------

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    enum Protection {
        Witness,
        Statement,
    }

    #[test]
    fn table_protection_resolves_assigned_sides() {
        let h = TableProtection::new(Protection::Statement)
            .with(SideId(0), Protection::Witness)
            .with(SideId(1), Protection::Witness);
        assert_eq!(h.protection(Some(SideId(0))), Protection::Witness);
        assert_eq!(h.protection(Some(SideId(1))), Protection::Witness);
    }

    #[test]
    fn table_protection_falls_back_to_default() {
        let h = TableProtection::new(Protection::Statement).with(SideId(0), Protection::Witness);
        assert_eq!(h.protection(Some(SideId(99))), Protection::Statement);
        assert_eq!(h.protection(None), Protection::Statement);
    }

    #[test]
    fn table_protection_insert_mutates_in_place() {
        let mut h = TableProtection::new(Protection::Statement);
        h.insert(SideId(5), Protection::Witness);
        assert_eq!(h.protection(Some(SideId(5))), Protection::Witness);
    }

    // ---- propagate --------------------------------------------------------------

    #[test]
    fn propagate_empty_is_none() {
        assert_eq!(propagate(&[]), None);
    }

    #[test]
    fn propagate_all_none_is_none() {
        assert_eq!(propagate(&[None, None]), None);
    }

    #[test]
    fn propagate_single_side_is_that_side() {
        assert_eq!(propagate(&[Some(SideId(1))]), Some(SideId(1)));
    }

    #[test]
    fn propagate_agreeing_sides_propagate() {
        assert_eq!(propagate(&[Some(SideId(2)), Some(SideId(2)), Some(SideId(2))]), Some(SideId(2)));
    }

    #[test]
    fn propagate_mixed_none_and_agreeing_side_propagates() {
        assert_eq!(propagate(&[None, Some(SideId(2)), None, Some(SideId(2))]), Some(SideId(2)));
    }

    #[test]
    fn propagate_disagreeing_sides_is_none() {
        assert_eq!(propagate(&[Some(SideId(1)), Some(SideId(2))]), None);
    }

    #[test]
    fn propagate_disagreement_short_circuits_regardless_of_order() {
        assert_eq!(propagate(&[Some(SideId(2)), None, Some(SideId(1))]), None);
    }

    // ---- SideTable --------------------------------------------------------------

    #[test]
    fn side_table_interns_distinct_names_to_distinct_ids() {
        let mut t = SideTable::new();
        let alice = t.intern("alice");
        let bob = t.intern("bob");
        assert_ne!(alice, bob);
        assert_eq!(t.name(alice), Some("alice"));
        assert_eq!(t.name(bob), Some("bob"));
    }

    #[test]
    fn side_table_intern_is_idempotent_for_repeated_names() {
        let mut t = SideTable::new();
        let a1 = t.intern("alice");
        let a2 = t.intern("alice");
        assert_eq!(a1, a2);
    }

    #[test]
    fn side_table_name_lookup_of_unknown_id_is_none() {
        let t = SideTable::new();
        assert_eq!(t.name(SideId(0)), None);
    }
}
