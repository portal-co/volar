// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-phase-1
//! @ai: assisted
//! Experimental BitGC-style rate-one garbling variants.
//!
//! This module tree implements the **structure** of the three optimized
//! BitGC variants from Zhang, Liu, Yang, Lu, Yu, Wang, and Weng, *BitGC
//! Made (More) Efficient* (EUROCRYPT 2026; IACR ePrint 2025/1810), built on
//! the original BitGC of Liu, Wang, Yang, and Yu (EUROCRYPT 2025; ePrint
//! 2024/1988). See `docs/bitgc-variants-plan.md` for the plan, the
//! paper-reference map, and the paper-pinned status table.
//!
//! # Pinning status (read before relying on any equation)
//!
//! The per-gate stitching equations are **not yet paper-pinned**: the
//! ePrint PDF was not retrievable from the development host, so the exact
//! construction equations of [ZLL+26] Figures 4–5 (small) and §5.1 (fast)
//! are pending (ledger row BITGC-LEDGER-04 in
//! `docs/fhe/future-provider-integration-ledger.md`). All gate semantics
//! live behind the [`GateEquation`] seam in [`common`]; the shipped
//! [`common::ReferenceEquation`] is a self-consistent reference adapter
//! with the *communication shape* the paper specifies (1 bit per AND and
//! per XOR for small, 5 bits per AND and free XOR for fast) but it is
//! **not** the paper's construction and makes no security claim.
//!
//! # Deliberate boundaries
//!
//! - Semi-honest structure only; no active-security variant ([LWYY25a] is
//!   out of scope).
//! - Security assumption of the target constructions: Ring-LWE with
//!   key-dependent-message (KDM) security. No profile here is validated
//!   against a parameter table; the 128-bit claim is deferred to
//!   BITGC-LEDGER-03.
//! - No wire format: all values are typed in-memory values. Framing is a
//!   separate adapter concern (mirrors `tinylabels` discipline).
//! - Caller-supplied randomness and noise via [`swhe::RandomSource`] /
//!   [`swhe::NoiseSource`]; `ZeroNoise` exists for deterministic tests and
//!   must never be used with protected data.
//! - `no_std + alloc`, no external arithmetic dependencies (ADR
//!   `docs/adr/0003-bitgc-ring-substrate.md`).
//!
//! [ZLL+26]: <https://eprint.iacr.org/2025/1810>
//! [LWYY25a]: <https://eprint.iacr.org/2025/232>

pub mod common;
pub mod pinned;
pub mod prg;
pub mod ring;
pub mod seed;
pub mod small;
pub mod swhe;

/// The three optimized BitGC variants from ePrint 2025/1810.
///
/// `Tiny` is declared for completeness but is **deferred** by maintainer
/// decision (2026-10-08, ledger row BITGC-LEDGER-06): the authors' public
/// artifact ships no `tiny` mode, so §5.2 must be implemented from the
/// pinned paper text only.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Variant {
    /// 1 bit per AND gate, 1 bit per XOR gate ([ZLL+26] §4).
    Small,
    /// 5 bits per AND gate, free XOR ([ZLL+26] §5.1).
    Fast,
    /// 1 bit per AND gate, free XOR ([ZLL+26] §5.2). Deferred.
    Tiny,
}

impl Variant {
    /// Communication bits this variant charges for one AND gate
    /// ([ZLL+26] Table 1).
    pub const fn bits_per_and(self) -> usize {
        match self {
            Self::Small | Self::Tiny => 1,
            Self::Fast => 5,
        }
    }

    /// Communication bits this variant charges for one XOR gate
    /// ([ZLL+26] Table 1).
    pub const fn bits_per_xor(self) -> usize {
        match self {
            Self::Small => 1,
            Self::Fast | Self::Tiny => 0,
        }
    }
}
