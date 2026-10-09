//! BitGC garbling-scheme profile selection and resource admission.
//!
//! Mirrors [`crate::input_labels`]'s delivery-seam discipline. The default
//! garbling scheme stays the half-gate baseline. This module records BitGC
//! profile identities and resource budgets, but rejects every BitGC profile
//! until the role-separated session protocol is implemented and reviewed.
//! It does not implement the construction or change input-label delivery.
//!
//! # Pinning status
//!
//! The BitGC reference and pinned modules are not a deployable session. In
//! particular, the current Cirrus adapter gives evaluator state the garbler
//! secret key. `is_available` therefore admits only HalfGate until
//! BITGC-LEDGER-07 passes its role-separation and protocol gates.

use volar_spec::bitgc::Variant;

/// The garbling scheme for a session.
///
/// `HalfGate` (the default) is the existing streaming baseline
/// ([`volar_spec::garble`]). The `BitGc*` variants name constructions from
/// ePrint 2025/1810, but are unavailable for session admission until the
/// shared role-separated protocol passes its review gates.
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

    /// Whether this scheme is safe to admit to a session today.
    ///
    /// BitGC remains unavailable until role-separated key material,
    /// authenticated output opening, and the reviewed session protocol land.
    /// TODO(provider-ledger: BITGC-LEDGER-07): enable only after the ledger
    /// criteria pass; reference equations are not a production substitute.
    pub const fn is_available(self) -> bool {
        matches!(self, Self::HalfGate)
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
    /// The selected scheme is not available for secure session admission.
    UnavailableScheme,
    /// The request exceeds the configured budget.
    OverBudget,
}

/// Admit a BitGC session: it must select an available BitGC profile and fit
/// the configured BitGC resource budget. Half-gate sessions use their own
/// admission path and must not be charged this SWHE estimate.
pub fn admit(
    manifest: &BitGcManifest,
    budget: &BitGcResourceBudget,
    ciphertext_bytes: u64,
) -> Result<BitGcResourceRequest, BitGcAdmissionError> {
    if manifest.scheme.bitgc_variant().is_none() || !manifest.scheme.is_available() {
        return Err(BitGcAdmissionError::UnavailableScheme);
    }
    let expanded_cts = manifest.wires.saturating_mul(2);
    let resident_bytes = expanded_cts.saturating_mul(ciphertext_bytes);
    let request = BitGcResourceRequest {
        expanded_cts,
        resident_bytes,
    };
    if !within_budget(request, budget) {
        return Err(BitGcAdmissionError::OverBudget);
    }
    Ok(request)
}

fn within_budget(request: BitGcResourceRequest, budget: &BitGcResourceBudget) -> bool {
    request.expanded_cts <= budget.max_expanded_cts
        && request.resident_bytes <= budget.max_resident_bytes
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
        assert!(GcScheme::HalfGate.is_available());
        assert!(!GcScheme::BitGcSmall.is_available());
        assert!(!GcScheme::BitGcFast.is_available());
        assert!(!GcScheme::BitGcTiny.is_available());
    }

    #[test]
    fn admission() {
        let budget = BitGcResourceBudget {
            max_expanded_cts: 10_000,
            max_resident_bytes: 1 << 30,
        };
        // All BitGC profiles are rejected until the shared session passes
        // the role-separation and security gates.
        for scheme in [
            GcScheme::BitGcSmall,
            GcScheme::BitGcFast,
            GcScheme::BitGcTiny,
        ] {
            assert_eq!(
                admit(&manifest(scheme), &budget, 1_000),
                Err(BitGcAdmissionError::UnavailableScheme)
            );
        }
        // Half-gate is available, but its resources belong to its own
        // admission path rather than this BitGC/SWHE estimator.
        assert_eq!(
            admit(&manifest(GcScheme::HalfGate), &budget, 1_000),
            Err(BitGcAdmissionError::UnavailableScheme)
        );

        // Keep the budget arithmetic covered even while every BitGC profile
        // is fail-closed at the availability gate.
        let tight = BitGcResourceBudget {
            max_expanded_cts: 100,
            max_resident_bytes: 1 << 30,
        };
        assert!(!within_budget(
            BitGcResourceRequest {
                expanded_cts: 2_000,
                resident_bytes: 2_000_000,
            },
            &tight,
        ));
    }
}
