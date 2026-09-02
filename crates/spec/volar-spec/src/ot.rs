// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Oblivious Transfer primitives.
//!
//! Correlated-OT (C-OT) used by VOLE setup ([`crate::vole::setup`]) and by
//! the Ferret / SoftSpoken / LWE stack in [`ferret`] and [`stack`].
//!
//! [`IdealCot`] remains the in-process ideal functionality. Networked
//! protocols compose [`lwe`] (base OT) → [`softspoken`] (extension) →
//! Ferret (PCG amplifier).

pub mod ideal_cot;
pub use ideal_cot::IdealCot;

pub mod base;
pub mod base_ot;
pub mod ferret;
pub mod group;
pub mod iknp;
pub mod lwe;
pub mod softspoken;
pub mod stack;
pub mod two_party;
pub mod wire;
