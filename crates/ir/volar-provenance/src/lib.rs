// @reliability: normal
//! @ai: assisted
//! Provenance handler trait and built-in implementations.
//!
//! This crate defines [`ProvenanceHandler`], the strategy trait that controls
//! how per-statement provenance annotations are mapped when IR passes cross
//! type boundaries (e.g. weaving `BIrBlocks<P>` into `IrModule<Q>`).
//!
//! Three built-in handlers cover the common cases:
//!
//! | Handler | Output | Behaviour |
//! |---------|--------|-----------|
//! | [`NoProvenance`] | `()` | Discards all annotations |
//! | [`KeepProvenance`] | `P` | Clones input provenance unchanged |
//! | [`MapProvenance<F>`] | `Q` | Applies a closure `Fn(&P) -> Q` |
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
/// during an IR transformation.
///
/// Implement this trait to control how provenance annotations flow across
/// pass boundaries.  The [`map`](Self::map) method converts each input
/// annotation; [`synthetic`](Self::synthetic) produces a value for
/// statements that have no corresponding input gate (setup code, return
/// scaffolding, etc.).
pub trait ProvenanceHandler<P: Clone + Default> {
    /// The provenance type carried by the output IR.
    type Output: Clone + Default;

    /// Map an input provenance annotation to the output type.
    fn map(&self, prov: &P) -> Self::Output;

    /// Produce a provenance value for synthetic statements that have no
    /// corresponding input gate.
    fn synthetic(&self) -> Self::Output {
        Self::Output::default()
    }

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

/// Provenance handler that discards all annotations.
///
/// This is the handler used by backwards-compatible wrapper functions.
/// It maps every `P` to `()`.
pub struct NoProvenance;

impl<P: Clone + Default> ProvenanceHandler<P> for NoProvenance {
    type Output = ();
    #[inline]
    fn map(&self, _prov: &P) -> () {}
    #[inline]
    fn synthetic(&self) -> () {}
}

/// Provenance handler that forwards input provenance unchanged.
///
/// Requires `P: Clone + Default`.  Every input provenance value is cloned
/// into the output; synthetic statements receive `P::default()`.
pub struct KeepProvenance;

impl<P: Clone + Default> ProvenanceHandler<P> for KeepProvenance {
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
    P: Clone + Default,
    Q: Clone + Default,
    F: Fn(&P) -> Q,
{
    type Output = Q;
    #[inline]
    fn map(&self, prov: &P) -> Q {
        (self.0)(prov)
    }
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

    #[test]
    fn no_provenance_synthetic_returns_unit() {
        let h = NoProvenance;
        assert_eq!(<NoProvenance as ProvenanceHandler<u32>>::synthetic(&h), ());
    }

    // ---- KeepProvenance -----------------------------------------------------

    #[test]
    fn keep_provenance_map_clones_value() {
        let h = KeepProvenance;
        assert_eq!(h.map(&7u32), 7u32);
        assert_eq!(h.map(&100u32), 100u32);
    }

    #[test]
    fn keep_provenance_synthetic_returns_default() {
        let h = KeepProvenance;
        let synthetic: u32 = h.synthetic();
        assert_eq!(synthetic, u32::default());
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
    fn map_provenance_synthetic_returns_output_default() {
        let h = MapProvenance(|x: &u32| *x * 2);
        // synthetic() returns Q::default() = 0u32
        assert_eq!(h.synthetic(), 0u32);
    }

    #[test]
    fn map_provenance_type_conversion() {
        // Map u32 → bool: non-zero becomes true.
        let h = MapProvenance(|x: &u32| *x != 0);
        assert_eq!(h.map(&0u32), false);
        assert_eq!(h.map(&1u32), true);
        assert_eq!(h.map(&99u32), true);
    }

    // ---- Trait object usage -------------------------------------------------

    #[test]
    fn provenance_handler_via_trait_object() {
        // Verify the trait is object-safe by using it via a reference.
        fn run(handler: &dyn ProvenanceHandler<u32, Output = ()>, val: u32) {
            let _ = handler.map(&val);
            let _ = handler.synthetic();
        }
        run(&NoProvenance, 42);
    }
}
