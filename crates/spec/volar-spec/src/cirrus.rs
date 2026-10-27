// @reliability: normal
//! @ai: assisted
//! Cirrus bindings to the Volar spec's hash/cipher + VOLE API (H6 of
//! `docs/hash-cipher-generic-plan.md`). **Auto-generated** by
//! `volar-codegen cirrus` — do not hand-edit; the drift-check test below
//! recomputes the digest of the backing sources and fails on any change.

use super::*;

// --- The hash/cipher backends (H2/H3/H4) ---
pub use crate::blake2s;
pub use crate::ascon;
pub use crate::keccak;

// --- The VOLE / OT / field surface Cirrus's `cirrus-volar-vole` consumes ---
pub use crate::field;
pub use crate::ot;
pub use crate::vole;

/// The hash backend selected for Cirrus's low-spec symbolic executors.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CirrusHashBackend {
    /// Blake2s (Cirrus-native default).
    Blake2s,
    /// Ascon (in-circuit / AEAD).
    Ascon,
    /// Keccak (on-chain).
    Keccak,
}

/// The default hash backend for Cirrus's low-spec symbolic executors.
pub const CIRrus_DEFAULT_HASH: CirrusHashBackend = CirrusHashBackend::Blake2s;

#[cfg(test)]
mod tests {
    #[test]
    fn cirrus_bindings_drift_check() {
        use super::blake2s::blake2s_256;
        let mut combined = alloc::vec::Vec::new();
        for src in [
            include_str!("blake2s.rs"),
            include_str!("ascon.rs"),
            include_str!("keccak.rs"),
        ] {
            combined.extend_from_slice(src.as_bytes());
        }
        let digest = blake2s_256(&combined);
        assert_eq!(
            digest,
            super::SPEC_HASH_SOURCES_DIGEST,
            "Cirrus bindings drift: regenerate with `volar-codegen cirrus`."
        );
    }
}

/// The committed digest of the spec's hash/cipher module sources.
pub const SPEC_HASH_SOURCES_DIGEST: [u8; 32] = [189, 135, 23, 134, 108, 248, 95, 76, 53, 231, 139, 133, 61, 87, 41, 93, 33, 0, 157, 124, 23, 253, 232, 133, 4, 231, 73, 217, 42, 180, 173, 194];
