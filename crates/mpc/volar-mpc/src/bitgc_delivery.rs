//! BitGC garbling-scheme profile selection and resource admission.
//!
//! Mirrors [`crate::input_labels`]'s delivery-seam discipline: the default
//! garbling scheme stays the half-gate baseline; the BitGC variants
//! ([`volar_spec::bitgc`]) are an explicit, server-only opt-in selected
//! before a transcript begins. This module owns the public profile type,
//! the circuit-geometry manifest the offline phase is sized from, and
//! resource admission. It does not implement the construction — that is
//! `volar_spec::bitgc` — and it does not change input-label delivery.
//!
//! # Pinning status
//!
//! The BitGC construction modules are experimental reference adapters
//! (gate equations pending BITGC-LEDGER-04). Selecting a BitGC profile
//! while the ledger rows are TODO must remain an explicit, documented
//! decision.

use volar_spec::bitgc::Variant;

/// The garbling scheme for a session.
///
/// `HalfGate` (the default) is the existing streaming baseline
/// ([`volar_spec::garble`]). The `BitGc*` variants are the rate-one
/// lattice-based schemes of ePrint 2025/1810, implemented in
/// [`volar_spec::bitgc`] as an optional substitute for large circuits
/// where communication dominates.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GcScheme {
    /// Half-gate baseline (default; [ZRE15]-style tables).
    HalfGate,
    /// BitGC-small: 1 bit per AND, 1 bit per XOR ([ZLL+26] §4).
    BitGcSmall,
    /// BitGC-fast: 5 bits per AND, free XOR ([ZLL+26] §5.1).
    BitGcFast,
    /// BitGC-tiny: 1 bit per AND, free XOR ([ZLL+26] §5.2).
    ///
    /// Deferred (BITGC-LEDGER-06): the public artifact has no `tiny`
    /// mode, so this variant is constructed only from the pinned paper
    /// text and currently has no implementation. Selecting it is an
    /// error until then.
    BitGcTiny,
}

impl Default for GcScheme {
    fn default() -> Self {
        Self::HalfGate
    }
}

impl GcScheme {
    /// The construction variant in `volar_spec::bitgc`, if any.
    pub const fn bitgc_variant(self) -> Option<Variant> {
        match self {
            Self::HalfGate => None,
            Self::BitGcSmall => Some(Variant::Small),
            Self::BitGcFast => Some(Variant::Fast),
            Self::BitGcTiny => Some(Variant::Tiny),
        }
    }

    /// Whether this scheme is implemented and selectable today.
    pub const fn is_available(self) -> bool {
        !matches!(self, Self::BitGcTiny)
    }
}

/// Public identity of one BitGC session's offline material.
///
/// Sized from the circuit geometry (which the caller derives from a fixed
/// schedule or compiled circuit); binds the session and scheme so
/// expanded material cannot be replayed across sessions or variants.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BitGcManifest {
    /// Session binding supplied by the enclosing transcript protocol.
    pub session_id: [u8; 32],
    /// Digest of the circuit/schedule being garbled.
    pub circuit_digest: [u8; 32],
    /// Selected scheme.
    pub scheme: GcScheme,
    /// Circuit wire count (drives the offline expansion size).
    pub wires: u64,
    /// AND-gate count (drives the online record stream size).
    pub and_gates: u64,
}

/// Resource budget checked before a BitGC session is admitted.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BitGcResourceBudget {
    /// Maximum expanded ciphertexts retained at once.
    pub max_expanded_cts: u64,
    /// Maximum bytes of SWHE material (ciphertexts + keys) resident.
    pub max_resident_bytes: u64,
}

/// Resource request computed from a manifest and the SWHE profile.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BitGcResourceRequest {
    /// Expanded ciphertexts: `2 × wires` (an `A0` and a `π` per wire).
    pub expanded_cts: u64,
    /// Estimated resident bytes for the caller's profile.
    pub resident_bytes: u64,
}

/// Admission decision error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BitGcAdmissionError {
    /// The selected scheme is not implemented (e.g. `BitGcTiny`).
    UnavailableScheme,
    /// The request exceeds the configured budget.
    OverBudget,
}

/// Admit a BitGC session: scheme must be implemented and within budget.
pub fn admit(
    manifest: &BitGcManifest,
    budget: &BitGcResourceBudget,
    ciphertext_bytes: u64,
) -> Result<BitGcResourceRequest, BitGcAdmissionError> {
    if !manifest.scheme.is_available() {
        return Err(BitGcAdmissionError::UnavailableScheme);
    }
    let expanded_cts = manifest.wires.saturating_mul(2);
    let resident_bytes = expanded_cts.saturating_mul(ciphertext_bytes);
    let request = BitGcResourceRequest {
        expanded_cts,
        resident_bytes,
    };
    if request.expanded_cts > budget.max_expanded_cts
        || request.resident_bytes > budget.max_resident_bytes
    {
        return Err(BitGcAdmissionError::OverBudget);
    }
    Ok(request)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn manifest(scheme: GcScheme) -> BitGcManifest {
        BitGcManifest {
            session_id: [1; 32],
            circuit_digest: [2; 32],
            scheme,
            wires: 1_000,
            and_gates: 300,
        }
    }

    #[test]
    fn defaults_and_variants() {
        assert_eq!(GcScheme::default(), GcScheme::HalfGate);
        assert_eq!(GcScheme::HalfGate.bitgc_variant(), None);
        assert_eq!(GcScheme::BitGcSmall.bitgc_variant(), Some(Variant::Small));
        assert_eq!(GcScheme::BitGcFast.bitgc_variant(), Some(Variant::Fast));
        assert!(!GcScheme::BitGcTiny.is_available());
    }

    #[test]
    fn admission() {
        let budget = BitGcResourceBudget {
            max_expanded_cts: 10_000,
            max_resident_bytes: 1 << 30,
        };
        // ok
        let req = admit(&manifest(GcScheme::BitGcSmall), &budget, 1_000).unwrap();
        assert_eq!(req.expanded_cts, 2_000);
        assert_eq!(req.resident_bytes, 2_000_000);
        // tiny rejected
        assert_eq!(
            admit(&manifest(GcScheme::BitGcTiny), &budget, 1_000),
            Err(BitGcAdmissionError::UnavailableScheme)
        );
        // over budget
        let tight = BitGcResourceBudget {
            max_expanded_cts: 100,
            max_resident_bytes: 1 << 30,
        };
        assert_eq!(
            admit(&manifest(GcScheme::BitGcSmall), &tight, 1_000),
            Err(BitGcAdmissionError::OverBudget)
        );
    }
}
