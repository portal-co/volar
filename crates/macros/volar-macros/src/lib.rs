//! Proc macros for Volar generated code.
//!
//! # `#[volar_action]`
//!
//! Attribute macro applied to action function declarations emitted by the
//! Volar weaver.  An "action" is an external interaction point in a compiled
//! circuit — e.g. an ORAM client round-trip, an oracle query, or any other
//! operation whose concrete behavior depends on the deployment context.
//!
//! The weaver emits a function declaration with a fallback body (returns the
//! fallback values) and tags it with `#[volar_action(...)]`.  The attribute
//! carries metadata about the action (name, output publicness, etc.).
//!
//! ## Current behavior
//!
//! Identity transform — the function declaration is passed through unchanged.
//! The fallback body provides type-correct default behavior so the generated
//! code compiles and can be tested in isolation.
//!
//! ## Future behavior
//!
//! Different builds / feature flags can swap this proc macro to inject:
//! - Actual ORAM client-server communication
//! - Interactive proof protocol steps
//! - Simulation / instrumentation wrappers
//!
//! The function declaration remains the call target; the attribute controls
//! what the function *does*.

// @reliability: experimental
// @ai: assisted

use proc_macro::TokenStream;

/// Mark a function as a Volar action entry point.
///
/// # Usage (in generated code)
///
/// ```ignore
/// #[volar_action(name = "oram_begin_0")]
/// fn oram_begin_0(guard: bool, fb_leaf: [bool; 64], addr: [bool; 64]) -> ([bool; 64],) {
///     // fallback: return fallback values
///     let _ = (guard, addr);
///     (fb_leaf,)
/// }
/// ```
///
/// The attribute is currently an identity transform — the function compiles
/// and runs using its fallback body.  Future versions will use the metadata
/// to inject context-dependent behavior.
#[proc_macro_attribute]
pub fn volar_action(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Identity transform: pass through the function declaration unchanged.
    // The fallback body provides type-correct default behavior.
    item
}
