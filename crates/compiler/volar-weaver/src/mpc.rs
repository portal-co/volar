// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Mutual-privacy side vocabulary for garbled-circuit weaving.
//!
//! This is the MPC analogue of [`crate::vole`]'s [`VoleProtection`] /
//! [`VoleSideAssignments`]: the policy-free [`volar_side`] machinery resolves
//! *which party* an IR value belongs to, and this module gives that the
//! garbled-circuit meaning — public (both parties), garbler-private, or
//! evaluator-private. It is the weaver-side vocabulary the `volar-mpc`
//! session layer's [`InputOwner`](volar_mpc::InputOwner) partition mirrors.
//!
//! # The protection lattice
//!
//! Two-party garbled circuits have three input visibilities:
//!
//! | Protection | Meaning | How its label reaches the evaluator |
//! |------------|---------|-------------------------------------|
//! | [`MpcProtection::Public`] | known to both | garbler encodes and sends the selected label |
//! | [`MpcProtection::Garbler`] | garbler-only | garbler encodes and sends the selected label |
//! | [`MpcProtection::Evaluator`] | evaluator-only | one 1-of-2 OT per bit |
//!
//! Propagation ([`volar_side::propagate`]) keeps a derived value on its
//! operands' common side and yields `None` on disagreement. [`MpcResolver`]
//! supplies the *join* for that disagreement: a value combining two distinct
//! private sides stays private (it is computable only inside the garbled
//! circuit, never in cleartext by a single party), while anything touching a
//! public side but no private side is public.
//!
//! # Relationship to the session layer
//!
//! The weaver decides, per circuit input bit, which of the three sets it
//! belongs to; the `volar-mpc` session layer then delivers labels for those
//! sets by the matching mechanism. This module only assigns the side
//! vocabulary and resolves it — the actual OT / label-delivery flow lives in
//! `volar-mpc`.

use alloc::collections::BTreeMap;
use alloc::string::String;

use volar_side::SideId;

/// What an MPC-side value is, in the two-party garbled-circuit sense: public
/// (known to both), garbler-private, or evaluator-private.
///
/// The `volar-side` vocabulary for the garbling weaver — resolved from a
/// [`SideId`] by any [`SideHandler`]. This is the three-point analogue of
/// [`VoleProtection`](crate::vole::VoleProtection)'s two-point
/// witness/statement split.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MpcProtection {
    /// Public cleartext value, known to both parties (`bool`).
    Public,
    /// Private to the garbler; the garbler encodes the selected label and
    /// sends it to the evaluator.
    Garbler,
    /// Private to the evaluator; the selected label reaches the evaluator via
    /// 1-of-2 OT (garbler is OT sender).
    Evaluator,
}

impl MpcProtection {
    /// Is this value private to exactly one party (i.e. carried as a garbled
    /// label rather than a cleartext bit)?
    pub fn is_private(self) -> bool {
        matches!(self, MpcProtection::Garbler | MpcProtection::Evaluator)
    }
}

/// Resolves the *join* of operand protections when [`volar_side::propagate`]
/// reports disagreement (`None`): which protection a value derived from
/// mixed-side operands carries.
///
/// The rule for two-party garbled circuits:
///
/// - **public ∧ public = public.** A function of only public values is public.
/// - **anything ∧ private = private.** Once any operand is one party's
///   private value, the result is computable only inside the garbled circuit;
///   neither party can hold it in cleartext. Combining *both* private sides
///   still yields a private value — the circuit mixes them, and the output
///   wire carries a label, not a cleartext bit.
///
/// So the lattice is `Public < Private`, with the two private parties
/// (garbler/evaluator) collapsing to the single "private" join — the label
/// does not record *which* party's privacy dominates, only that the value is
/// no longer public.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct MpcResolver;

impl MpcResolver {
    /// Join a set of operand protections into the result's protection.
    ///
    /// Empty (a constant or zero-operand node) is [`MpcProtection::Public`];
    /// otherwise the join is `Public` iff *every* operand is public. A mixed
    /// set yields the privacy of whichever private party is present; when
    /// *both* private parties appear the result is still private, and we
    /// report it as [`MpcProtection::Garbler`] — an arbitrary but fixed choice,
    /// since the label on a mixed wire is not attributable to a single party
    /// (neither can hold it in cleartext). Callers that need to distinguish
    /// "attributable to one party" from "mixed" should consult the operand
    /// sides directly rather than the joined protection.
    pub fn join(operands: impl IntoIterator<Item = MpcProtection>) -> MpcProtection {
        let mut any = false;
        let mut first_private: Option<MpcProtection> = None;
        for p in operands {
            any = true;
            if p.is_private() && first_private.is_none() {
                first_private = Some(p);
            }
        }
        match first_private {
            Some(p) => p,
            None => {
                if any {
                    // All operands public.
                    MpcProtection::Public
                } else {
                    // Constant / zero-operand node.
                    MpcProtection::Public
                }
            }
        }
    }
}

/// Side assignment for a garbled circuit's introduction points: circuit input
/// params and action output bits.
///
/// Mirrors [`VoleSideAssignments`](crate::vole::VoleSideAssignments): a
/// `BIrBlock`'s params aren't individually `Node`-wrapped and an action's
/// output bits have no IR node of their own, so these introduction points
/// take an explicit side from the weaver caller rather than reading one off
/// the IR. The MPC version assigns *which party* each introduction point
/// belongs to.
#[derive(Clone, Debug, Default)]
pub struct MpcSideAssignments {
    /// Side for circuit input param `i` (0-based), if assigned.
    pub input_sides: BTreeMap<u32, SideId>,
    /// Side for action `name`'s output bit `j` (0-based), if assigned.
    pub action_sides: BTreeMap<String, BTreeMap<usize, SideId>>,
}

impl MpcSideAssignments {
    /// Assign `side` to circuit input param `idx`, returning `self` for chaining.
    pub fn with_input(mut self, idx: u32, side: SideId) -> Self {
        self.input_sides.insert(idx, side);
        self
    }

    /// Assign `side` to action `name`'s output bit `bit`, returning `self` for chaining.
    pub fn with_action_output(mut self, name: &str, bit: usize, side: SideId) -> Self {
        self.action_sides.entry(name.into()).or_default().insert(bit, side);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_side::SideTable;

    use MpcProtection::*;

    #[test]
    fn join_all_public_is_public() {
        assert_eq!(MpcResolver::join([Public, Public]), Public);
        assert_eq!(MpcResolver::join([Public]), Public);
    }

    #[test]
    fn join_empty_is_public() {
        // A constant / zero-operand node has no private operand.
        assert_eq!(MpcResolver::join([]), Public);
    }

    #[test]
    fn join_any_private_is_private() {
        // Public mixed with a private operand is private.
        assert!(MpcResolver::join([Public, Garbler]).is_private());
        assert!(MpcResolver::join([Public, Evaluator]).is_private());
        assert!(MpcResolver::join([Garbler]).is_private());
        assert!(MpcResolver::join([Evaluator]).is_private());
    }

    #[test]
    fn join_single_private_party_is_attributable() {
        assert_eq!(MpcResolver::join([Public, Garbler]), Garbler);
        assert_eq!(MpcResolver::join([Evaluator, Public]), Evaluator);
    }

    #[test]
    fn join_both_private_parties_stays_private() {
        // Mixing the garbler's and evaluator's private values still yields a
        // private value (computable only in-circuit); the reported party is a
        // fixed arbitrary choice, but it must be private.
        let j = MpcResolver::join([Garbler, Evaluator]);
        assert!(j.is_private());
        let j2 = MpcResolver::join([Evaluator, Garbler, Public]);
        assert!(j2.is_private());
    }

    #[test]
    fn assignments_chain_and_store() {
        let mut table = SideTable::new();
        let public = table.intern("public");
        let alice = table.intern("alice");
        let bob = table.intern("bob");

        let a = MpcSideAssignments::default()
            .with_input(0, public)
            .with_input(1, alice)
            .with_input(2, bob)
            .with_action_output("rand", 0, alice);

        assert_eq!(a.input_sides.get(&0), Some(&public));
        assert_eq!(a.input_sides.get(&1), Some(&alice));
        assert_eq!(a.input_sides.get(&2), Some(&bob));
        assert_eq!(a.action_sides.get("rand").and_then(|m| m.get(&0)), Some(&alice));
    }
}
