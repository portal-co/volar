// @reliability: normal
//! @ai: assisted
//! Provenance handler traits and built-in implementations.
//!
//! This crate defines [`ProvenanceHandler`] and [`DualProvenanceHandler`], the
//! strategy traits that control how per-statement provenance annotations are
//! mapped when IR passes cross type boundaries (e.g. weaving `BIrBlocks<P>`
//! into `IrModule<Q>`).
//!
//! # Design invariant: provenance is never invented
//!
//! There is no `Default` bound on `P` and no `synthetic()` method.  Every
//! statement's provenance must come from *some* source statement in the input
//! IR — either propagated unchanged or mapped through a handler.  Passes that
//! expand one source statement into N derived statements clone the source
//! provenance for all N.  This makes every output statement traceable back to
//! an input statement, enabling debuggability and downstream source tracking.
//!
//! # Single-input handlers
//!
//! Three built-in handlers cover the common cases:
//!
//! | Handler | Output | Behaviour |
//! |---------|--------|-----------|
//! | [`NoProvenance`] | `()` | Discards all annotations |
//! | [`KeepProvenance`] | `P` | Clones input provenance unchanged |
//! | [`MapProvenance<F>`] | `Q` | Applies a closure `Fn(&P) -> Q` |
//!
//! # Dual-input handlers
//!
//! [`DualProvenanceHandler`] is used by passes that combine two IR inputs with
//! different provenance types (e.g., `substitute_vaffle_with_handler` which
//! merges a host `Module<P>` with a replacement `Module<Q>`).
//!
//! # Example
//!
//! ```ignore
//! use volar_provenance::{ProvenanceHandler, MapProvenance};
//!
//! let handler = MapProvenance(|p: &GateProv| p.line_number);
//! let module = weave_evaluator_with_handler(&circuit, "name", None, &handler);
//! ```

#![no_std]

/// Strategy for mapping circuit-level provenance `P` into output provenance
/// during a single-input IR transformation.
///
/// Implement this trait to control how provenance annotations flow across
/// pass boundaries.  The [`map`](Self::map) method converts each input
/// annotation.  There is intentionally no `synthetic()` method: every output
/// statement's provenance must derive from a source statement's provenance.
pub trait ProvenanceHandler<P: Clone> {
    /// The provenance type carried by the output IR.
    type Output: Clone;

    /// Map an input provenance annotation to the output type.
    fn map(&self, prov: &P) -> Self::Output;

    /// Return the QuickSilver polynomial degree to use for an AND gate whose
    /// provenance is `prov`.
    ///
    /// - `1` (default) → standard K=1 `vole_and_prover_step` with a hat.
    /// - `2` → K=2 `mul_generalized` without a hat; the full product
    ///   polynomial is tracked in a `Vope<N,T,U2>` and verified by direct
    ///   evaluation at Δ without a hat correction.
    ///
    /// Override this in FAEST-aware handlers to route S-box AND gates to K=2
    /// (or K=3 in a future extension).
    fn gate_degree(&self, _prov: &P) -> usize {
        1
    }
}

/// Strategy for combining provenance from two independently-annotated IR
/// inputs into a single output provenance type.
///
/// Used by passes that merge two IR sources with different provenance types,
/// such as `substitute_vaffle_with_handler` which inlines a replacement
/// `Module<Q>` into a host `Module<P>`.
///
/// As with [`ProvenanceHandler`], there is no `synthetic()` — every statement
/// must come from one or both inputs.
pub trait DualProvenanceHandler<P1: Clone, P2: Clone> {
    /// The provenance type carried by the output IR.
    type Output: Clone;

    /// Map a statement that originated entirely from the left (host) input.
    fn map_left(&self, p1: &P1) -> Self::Output;

    /// Map a statement that originated entirely from the right (replacement) input.
    fn map_right(&self, p2: &P2) -> Self::Output;

    /// Combine provenance for a statement jointly attributed to both inputs.
    fn merge(&self, p1: &P1, p2: &P2) -> Self::Output;
}

// ============================================================================
// Single-input built-in handlers
// ============================================================================

/// Provenance handler that discards all annotations.
///
/// This is the handler used by backwards-compatible wrapper functions.
/// It maps every `P` to `()`.
pub struct NoProvenance;

impl<P: Clone> ProvenanceHandler<P> for NoProvenance {
    type Output = ();
    #[inline]
    fn map(&self, _prov: &P) -> () {}
}

/// Provenance handler that forwards input provenance unchanged.
///
/// Every input provenance value is cloned into the output.
pub struct KeepProvenance;

impl<P: Clone> ProvenanceHandler<P> for KeepProvenance {
    type Output = P;
    #[inline]
    fn map(&self, prov: &P) -> P {
        prov.clone()
    }
}

/// Provenance handler that applies a closure `F: Fn(&P) -> Q`.
///
/// # Example
///
/// ```ignore
/// let handler = MapProvenance(|p: &GateProv| p.line_number);
/// ```
pub struct MapProvenance<F>(pub F);

impl<P, Q, F> ProvenanceHandler<P> for MapProvenance<F>
where
    P: Clone,
    Q: Clone,
    F: Fn(&P) -> Q,
{
    type Output = Q;
    #[inline]
    fn map(&self, prov: &P) -> Q {
        (self.0)(prov)
    }
}

// ============================================================================
// Dual-input built-in handlers
// ============================================================================

/// Dual handler that keeps only the left (host) provenance.
///
/// `map_right` and `merge` both call `map_left`, discarding the right side.
pub struct KeepLeft;

impl<P1: Clone, P2: Clone> DualProvenanceHandler<P1, P2> for KeepLeft {
    type Output = P1;
    #[inline]
    fn map_left(&self, p1: &P1) -> P1 { p1.clone() }
    #[inline]
    fn map_right(&self, _p2: &P2) -> P1 { panic!("KeepLeft: cannot produce P1 from a right-only statement with no left provenance") }
    #[inline]
    fn merge(&self, p1: &P1, _p2: &P2) -> P1 { p1.clone() }
}

/// Dual handler that keeps only the right (replacement) provenance.
///
/// `map_left` panics since there is no right-side provenance to fall back to.
pub struct KeepRight;

impl<P1: Clone, P2: Clone> DualProvenanceHandler<P1, P2> for KeepRight {
    type Output = P2;
    #[inline]
    fn map_left(&self, _p1: &P1) -> P2 { panic!("KeepRight: cannot produce P2 from a left-only statement with no right provenance") }
    #[inline]
    fn map_right(&self, p2: &P2) -> P2 { p2.clone() }
    #[inline]
    fn merge(&self, _p1: &P1, p2: &P2) -> P2 { p2.clone() }
}

/// Dual handler that applies separate closures for left, right, and merge.
///
/// # Type parameters
/// - `FL: Fn(&P1) -> Q`
/// - `FR: Fn(&P2) -> Q`
/// - `FM: Fn(&P1, &P2) -> Q`
pub struct MergePair<FL, FR, FM>(pub FL, pub FR, pub FM);

impl<P1, P2, Q, FL, FR, FM> DualProvenanceHandler<P1, P2> for MergePair<FL, FR, FM>
where
    P1: Clone,
    P2: Clone,
    Q: Clone,
    FL: Fn(&P1) -> Q,
    FR: Fn(&P2) -> Q,
    FM: Fn(&P1, &P2) -> Q,
{
    type Output = Q;
    #[inline]
    fn map_left(&self, p1: &P1) -> Q { (self.0)(p1) }
    #[inline]
    fn map_right(&self, p2: &P2) -> Q { (self.1)(p2) }
    #[inline]
    fn merge(&self, p1: &P1, p2: &P2) -> Q { (self.2)(p1, p2) }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    // ---- NoProvenance -------------------------------------------------------

    #[test]
    fn no_provenance_map_returns_unit() {
        let h = NoProvenance;
        assert_eq!(h.map(&42u32), ());
        assert_eq!(h.map(&"hello"), ());
    }

    // ---- KeepProvenance -----------------------------------------------------

    #[test]
    fn keep_provenance_map_clones_value() {
        let h = KeepProvenance;
        assert_eq!(h.map(&7u32), 7u32);
        assert_eq!(h.map(&100u32), 100u32);
    }

    #[test]
    fn keep_provenance_map_preserves_string() {
        let h = KeepProvenance;
        let s = std::string::String::from("abc");
        assert_eq!(h.map(&s), s);
    }

    // ---- MapProvenance ------------------------------------------------------

    #[test]
    fn map_provenance_applies_closure() {
        let h = MapProvenance(|x: &u32| *x * 2);
        assert_eq!(h.map(&3u32), 6u32);
        assert_eq!(h.map(&0u32), 0u32);
    }

    #[test]
    fn map_provenance_type_conversion() {
        let h = MapProvenance(|x: &u32| *x != 0);
        assert_eq!(h.map(&0u32), false);
        assert_eq!(h.map(&1u32), true);
        assert_eq!(h.map(&99u32), true);
    }

    // ---- Trait object usage -------------------------------------------------

    #[test]
    fn provenance_handler_via_trait_object() {
        fn run(handler: &dyn ProvenanceHandler<u32, Output = ()>, val: u32) {
            let _ = handler.map(&val);
        }
        run(&NoProvenance, 42);
    }

    // ---- DualProvenanceHandler ----------------------------------------------

    #[test]
    fn keep_left_maps_left() {
        let h = KeepLeft;
        assert_eq!(<KeepLeft as DualProvenanceHandler<u32, u32>>::map_left(&h, &5u32), 5u32);
        assert_eq!(<KeepLeft as DualProvenanceHandler<u32, u32>>::merge(&h, &5u32, &99u32), 5u32);
    }

    #[test]
    fn keep_right_maps_right() {
        let h = KeepRight;
        assert_eq!(<KeepRight as DualProvenanceHandler<u32, u32>>::map_right(&h, &7u32), 7u32);
        assert_eq!(<KeepRight as DualProvenanceHandler<u32, u32>>::merge(&h, &1u32, &7u32), 7u32);
    }

    #[test]
    fn merge_pair_applies_closures() {
        let h = MergePair(
            |x: &u32| *x * 10,
            |x: &u32| *x * 100,
            |a: &u32, b: &u32| a + b,
        );
        assert_eq!(h.map_left(&3u32), 30u32);
        assert_eq!(h.map_right(&3u32), 300u32);
        assert_eq!(h.merge(&3u32, &4u32), 7u32);
    }
}
