//! Configuration for IR lowering passes.
//!
//! [`IrLoweringConfig`] controls machine-specific parameters used during
//! lowering from Volar IR to LIR (and ultimately to machine code or other
//! targets).  It is intentionally free of dependencies so it can be shared
//! across all crates in the pipeline.

#![no_std]

/// Parameters that govern how IR is lowered to LIR.
///
/// Construct via [`Default::default()`] for sensible 64-bit host settings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IrLoweringConfig {
    /// Native word size in bits (must be a power of two, ≥8).  Determines
    /// which integer types map to single LIR words.  Default: 64.
    pub word_bits: u32,

    /// Pointer size in bits.  Usually equals `word_bits` on flat-memory
    /// targets.  Default: 64.
    pub pointer_bits: u32,

    /// Maximum size (in bits) of an aggregate that is passed by value rather
    /// than spilled to a storage slot.  Set to `usize::MAX` to never spill.
    /// Default: `usize::MAX`.
    pub aggregate_byval_limit: usize,

    /// When `true`, the LIR backend may emit native aggregate types (structs,
    /// arrays) rather than always flattening to scalars.  Default: `false`.
    pub native_aggregates: bool,
}

impl Default for IrLoweringConfig {
    fn default() -> Self {
        Self {
            word_bits: 64,
            pointer_bits: 64,
            aggregate_byval_limit: usize::MAX,
            native_aggregates: false,
        }
    }
}
