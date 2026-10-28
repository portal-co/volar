// @reliability: normal
//! @ai: assisted
//! Field markers: zero-sized types carrying extension/prime field identity
//! at compile time — the same marker pattern as `volar-discipline`'s
//! `Zk`/`Transparent` typestate and `volar-iop`'s `BetaOf` tower-level
//! markers.
//!
//! A marker names one entry of the shared registry
//! ([`volar_ir_common::field_markers`]): the registry is the single place an
//! irreducible polynomial or modulus is written, these impls merely re-export
//! its constants, and the `volar-compiler` parser resolves the same marker
//! names to the same specs. One marker therefore pins one field across native
//! execution, the AST, LIR, and VAFFLE — the anti-embedding-gap measure of
//! `docs/warp-vole-verifier-zksnark-plan.md` §5.2 (in the `volar-ir` repo).
//!
//! The existing `PrimitiveType::{Galois, Galois64, Galois128, Galois256, Z3}`
//! AST primitives remain as aliases of these markers: both spellings resolve
//! to the same registry field spec.

/// Marker for a binary extension field `GF(2^DEGREE)` in polynomial basis
/// over `GF(2)`, with modulus `IRREDUCIBLE` (low coefficient first, monic,
/// length `DEGREE + 1`).
///
/// Not intended to be implemented downstream: add a registry entry in
/// `volar-ir-common`'s `field_markers` and a marker here together.
pub trait ExtFieldMarker: Copy + core::fmt::Debug + 'static {
    /// The reserved source-level name, e.g. `Gf2_128`.
    const NAME: &'static str;
    /// Extension degree over `GF(2)`.
    const DEGREE: u32;
    /// Irreducible polynomial, low coefficient first, length `DEGREE + 1`.
    const IRREDUCIBLE: &'static [u64];
}

/// Marker for a prime field of `p = 2^K - N`, canonical per
/// `volar-ir-common`'s `prime.rs` interning rules (`K` in `2..=256`,
/// `N < 2^(K-1)`, `p` prime).
pub trait PrimeFieldMarker: Copy + core::fmt::Debug + 'static {
    /// The reserved source-level name, e.g. `Fp3`.
    const NAME: &'static str;
    /// Bit length of `p`.
    const K: u32;
    /// `N` as little-endian `u64` limbs; `p = 2^K - N`.
    const N: &'static [u64];
}

macro_rules! ext_field_marker {
    ($marker:ident, $spec:expr) => {
        impl ExtFieldMarker for $marker {
            const NAME: &'static str = $spec.name;
            const DEGREE: u32 = $spec.degree;
            const IRREDUCIBLE: &'static [u64] = $spec.irreducible;
        }
    };
}

macro_rules! prime_field_marker {
    ($marker:ident, $spec:expr) => {
        impl PrimeFieldMarker for $marker {
            const NAME: &'static str = $spec.name;
            const K: u32 = $spec.k;
            const N: &'static [u64] = $spec.n;
        }
    };
}

/// Marker for the AES field `GF(2^8)` (`x^8 + x^4 + x^3 + x + 1`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_8;
ext_field_marker!(Gf2_8, volar_ir_common::field_markers::GF2_8);

/// Marker for `GF(2^64)` (`x^64 + x^4 + x^3 + x + 1`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_64;
ext_field_marker!(Gf2_64, volar_ir_common::field_markers::GF2_64);

/// Marker for the VOLE field `GF(2^128)` (`x^128 + x^7 + x^2 + x + 1`) —
/// the WARP accumulation field per the plan's §5.4 decision.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_128;
ext_field_marker!(Gf2_128, volar_ir_common::field_markers::GF2_128);

/// Marker for `GF(2^256)` (`x^256 + x^10 + x^5 + x^2 + 1`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_256;
ext_field_marker!(Gf2_256, volar_ir_common::field_markers::GF2_256);

/// Marker for GF(3) (`p = 2^2 - 1`), the `Z3` primitive's field.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Fp3;
prime_field_marker!(Fp3, volar_ir_common::field_markers::FP3);

#[cfg(test)]
mod tests {
    use super::*;
    use core::mem::size_of;

    #[test]
    fn markers_are_zero_sized() {
        assert_eq!(size_of::<Gf2_8>(), 0);
        assert_eq!(size_of::<Gf2_64>(), 0);
        assert_eq!(size_of::<Gf2_128>(), 0);
        assert_eq!(size_of::<Gf2_256>(), 0);
        assert_eq!(size_of::<Fp3>(), 0);
    }

    #[test]
    fn marker_consts_are_the_registry_entries() {
        assert_eq!(Gf2_8::NAME, "Gf2_8");
        assert_eq!(Gf2_8::DEGREE, 8);
        assert_eq!(Gf2_8::IRREDUCIBLE[8], 1);
        assert_eq!(Gf2_64::DEGREE, 64);
        assert_eq!(Gf2_128::NAME, "Gf2_128");
        assert_eq!(Gf2_128::DEGREE, 128);
        assert_eq!(Gf2_128::IRREDUCIBLE.len(), 129);
        assert_eq!(Gf2_256::DEGREE, 256);
        assert_eq!(Fp3::NAME, "Fp3");
        assert_eq!(Fp3::K, 2);
        assert_eq!(Fp3::N, &[1]);
    }

    #[test]
    fn every_registry_marker_has_a_native_marker_type() {
        // The two tables must stay in lockstep: a registry entry with no
        // native marker (or vice versa) breaks the "one marker pins one
        // field across native and IR" invariant.
        let native: &[&str] = &[
            Gf2_8::NAME,
            Gf2_64::NAME,
            Gf2_128::NAME,
            Gf2_256::NAME,
            Fp3::NAME,
        ];
        let registered: alloc::vec::Vec<&str> = volar_ir_common::field_markers::EXT_FIELD_MARKERS
            .iter()
            .map(|spec| spec.name)
            .chain(
                volar_ir_common::field_markers::PRIME_FIELD_MARKERS
                    .iter()
                    .map(|spec| spec.name),
            )
            .collect();
        for name in &registered {
            assert!(native.contains(name), "registry marker {name} has no native marker type");
        }
        assert_eq!(native.len(), registered.len());
    }
}
