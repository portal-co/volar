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
