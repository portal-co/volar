//! Fuzzing and property-testing harness for Volar IR passes.
//!
//! # Structure
//!
//! - [`interpreter`] — concrete evaluators for `BIrBlocks` and `IRBlocks`.
//! - [`gen`] — proptest strategies that produce structurally valid IR.
//! - [`properties`] — proptest property tests (compiled as `#[test]` items).
//! - [`arbitrary`] — `arbitrary::Arbitrary` impls for cargo-fuzz targets.

pub mod arbitrary;
pub mod generators;
pub mod interpreter;
#[cfg(test)]
pub mod properties;
