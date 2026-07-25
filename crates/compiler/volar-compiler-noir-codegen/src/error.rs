//! Diagnostics for the `IrModule` → Noir codegen pass.
//!
//! These are surfaced as `Result::Err`, never a panic and never silently
//! broken Noir source — see `docs/noir-backend.md` for the policy this
//! enum encodes.

#[cfg(feature = "std")]
use std::string::String;

#[cfg(not(feature = "std"))]
use alloc::string::String;

/// Something in an `IrModule` cannot be expressed in the v1 Noir backend.
///
/// Every variant names the offending function so multiple violations across
/// a module can be reported together instead of failing on the first one.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum NoirCodegenError {
    #[error(
        "fn `{function}`: while-loops cannot be expressed in constrained Noir \
         (constrained code has no runtime-conditional loop primitive); \
         consider a bounded loop over a fixed maximum iteration count with \
         per-iteration masking (unconstrained-assisted dynamic loops are \
         deferred, not v1)"
    )]
    UnsupportedWhileLoop { function: String },

    #[error(
        "fn `{function}`: loop bound is not a Noir compile-time constant: {reason} \
         (constrained `for` loops require a literal or generic-const-expression bound)"
    )]
    NonConstantLoopBound { function: String, reason: String },

    #[error(
        "fn `{function}`: requires unconstrained (Brillig) semantics: {reason} \
         (unconstrained functions are deferred, not v1)"
    )]
    UnconstrainedRequired { function: String, reason: String },

    #[error(
        "fn `{function}`: uses Noir's native Field type: {reason} \
         (Field-based optimization is deferred, not v1 — use typed integers)"
    )]
    FieldRequired { function: String, reason: String },

    #[error(
        "fn `{function}`: requires an escaping/aliased/returned reference: {reason} \
         (Noir forbids returning references or storing them in structs/arrays; \
         full reference support is deferred, not v1)"
    )]
    EscapingReference { function: String, reason: String },

    #[error("fn `{function}`: unsupported array length: {reason}")]
    UnsupportedArrayLength { function: String, reason: String },

    #[error("fn `{function}`: unsupported construct: {reason}")]
    Unsupported { function: String, reason: String },
}
