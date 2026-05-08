// @reliability: experimental
//! @ai: assisted
//! Oblivious Transfer primitives.
//!
//! This module provides Correlated-OT (C-OT) used by VOLE setup
//! ([`crate::vole::setup`]) and reusable by other protocols (garbled-circuit
//! input transfer, MPC).
//!
//! # Current state
//!
//! Only an **ideal** C-OT functionality ([`IdealCot`]) is implemented. It
//! performs the OT honestly within a single in-process struct — sender's
//! `Δ` and receiver's choice bit are both visible at the call site.
//!
//! This is sufficient for unit-testing higher-level protocols (VOLE setup,
//! Quicksilver AND check) but is **insecure** as a standalone primitive: a
//! real deployment must replace it with a network-realised OT (Chou-Orlandi
//! base OT + IKNP-style extension over a transport channel).

pub mod ideal_cot;
pub use ideal_cot::IdealCot;
