// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! Error type for the mxx → spec-IR lowerer.

use thiserror::Error;

/// Errors from lowering an mxx validated graph to spec IR.
///
/// `Unsupported` is the *loud coverage* arm: it always names the scope,
/// node id, node kind, and why it cannot lower, so gaps surface at
/// generation time rather than as silently wrong code.
#[derive(Debug, Error)]
pub enum InteropError {
    /// A node kind (or a shape of an otherwise supported kind) that the
    /// lowerer does not cover yet.
    #[error("unsupported node {node} in scope {scope}: {kind} — {detail}")]
    Unsupported {
        /// Scope the node lives in (debug form of `FrozenGraphScopeId`).
        scope: String,
        /// mxx node id.
        node: u64,
        /// Node kind name.
        kind: String,
        /// Why this shape is not covered.
        detail: String,
    },
    /// The graph contains scopes other than the root (subgraphs, loop
    /// bodies). Phase 0 lowers root-only graphs.
    #[error("graph has non-root scopes, which this lowerer does not support yet: {0:?}")]
    NonRootScopes(Vec<String>),
    /// A wire referenced by the frozen graph has no concrete type in the
    /// validated graph (should not happen for a validated graph).
    #[error("wire {0} has no concrete type in the validated graph")]
    MissingWireType(String),
    /// A compile-time expression (scale factor, constant coefficient)
    /// could not be evaluated under the graph's parameter bindings.
    #[error("failed to evaluate compile-time expression: {0}")]
    Expr(String),
    /// An integer constant or input value does not fit the `i128` spec
    /// representation of mxx `Int` wires.
    #[error("integer constant does not fit the i128 spec representation: {0}")]
    OversizedInt(String),
    /// The bundled `mxx_rt.rs` helper source failed to parse into IR.
    #[error("failed to parse bundled mxx_rt helpers into spec IR: {0}")]
    HelperParse(String),
    /// A root-scope input carries an artifact binding; spec elements are
    /// pure functions and cannot consume produced artifacts.
    #[error("root-scope input {name} uses an artifact binding, which spec elements cannot consume")]
    ArtifactInput {
        /// Input name.
        name: String,
    },
    /// A named graph output refers to a wire the lowerer never produced.
    #[error("graph output {name} refers to a wire the lowerer did not produce")]
    MissingOutput {
        /// Output name.
        name: String,
    },
}
