//! `arbitrary::Arbitrary` impls for cargo-fuzz targets.
//!
//! These use the same raw-data interpretation layer as the proptest strategies
//! in [`crate::gen`], but driven by `arbitrary::Unstructured` instead of
//! proptest's random sources.
pub mod biir;
pub mod ir;
