// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Experimental BitGC algebra, reference adapters, and evaluator-safe test
//! drivers.
//!
//! The small/fast equations are recorded in
//! `docs/research/bitgc-paper-pinning.md` from ePrint 2025/1810 §§4–6 and
//! ePrint 2024/1988 §2.5, and cross-checked against the authors' artifact.
//! That evidence pins the gate algebra; it does **not** make the current
//! SWHE/DD implementation, parameters, or session protocol deployable.
//! The legacy `ReferenceEquation`, `small`, `lattice_dd`, and shared-gadget
//! driver are available only under `bitgc-reference` (or tests). The new
//! role-separated path is in `pinned` and `lattice_dd_rns`; its algebraic
//! tests do not establish protocol security.
//!
//! # Safety boundary
//!
//! - BitGC admission remains disabled for protected sessions; HalfGate uses
//!   its separate admission path.
//! - The role-separated evaluator object contains no Δ/SWHE secret key, but
//!   input OT, bounded transcript frames, authenticated output opening, and
//!   independent parameter/noise review remain open under BITGC-LEDGER-07 and
//!   BITGC-LEDGER-03.
//! - Test profiles and exact mod-p shadows establish algebraic behavior only;
//!   they make no RLWE/KDM security claim.
//! - `no_std + alloc`, no external arithmetic dependencies (ADR
//!   `docs/adr/0003-bitgc-ring-substrate.md`).
//!
//! [ZLL+26]: <https://eprint.iacr.org/2025/1810>
//! [LWYY25b]: <https://eprint.iacr.org/2024/1988>

pub mod common;
#[cfg(any(test, feature = "bitgc-reference"))]
pub mod lattice_dd;
pub mod lattice_dd_rns;
pub mod pinned;
pub mod prg;
pub mod ring;
pub mod seed;
#[cfg(any(test, feature = "bitgc-reference"))]
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
