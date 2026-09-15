//! Batched external input-label delivery seam.
//!
//! Strict runners historically invoke [`crate::OtChannel`] once per evaluator
//! input wire. That remains the default and is represented here by
//! [`DirectOtDelivery`]. This module makes the public batch shape explicit so
//! a resource-heavy, opt-in TinyLabels adapter can later replace only delivery
//! of *external input labels* without changing garbled-table streaming,
//! intermediate labels, or durable material.
//!
//! The interface is deliberately batch-oriented. A Ring-LWE preprocessing
//! protocol must not be hidden behind one per-bit `receive` call: callers need
//! to know its manifest, profile, use counter, and resource admission before
//! the session begins.

use alloc::collections::BTreeSet;
use alloc::vec::Vec;

use hybrid_array::Array;
use volar_spec::vole::VoleArray;

use crate::OtChannel;

/// Public owner of the Boolean choices for a manifest.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputLabelOwner {
    /// The evaluator supplies the input choices and receives active labels.
    Evaluator,
}

/// Public profile selected before a transcript begins.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputLabelProfile {
    /// The default: one normal OT delivery per selected label.
    DirectOt,
    /// Reserved for the explicit server-only TinyLabels adapter. Selecting this
    /// without that adapter is an error; it is never an automatic fallback.
    TinyLabelsServer,
}

/// Canonical public identity of one ordered external-label batch.
///
/// The caller supplies digests rather than a schedule value so this execution
/// crate stays independent of compiler IR serialization. The enclosing
/// session is responsible for deriving these from a domain-separated
/// transcript and canonical schedule/manifest encoding.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InputLabelManifest {
    /// Session binding supplied by the enclosing transcript protocol.
    pub session_id: [u8; 32],
    /// Digest of the circuit/schedule that consumes these labels.
    pub circuit_digest: [u8; 32],
    /// Public input owner; only evaluator-owned batches are currently valid.
    pub owner: InputLabelOwner,
    /// Ordered circuit input-wire positions. Order is label-delivery order.
    pub wire_positions: Vec<u32>,
    /// Exact label width expected by the selected garbling construction.
    pub label_bytes: usize,
}

/// Invalid public manifest or delivery request.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputLabelError {
    /// TinyLabels requires explicit adapter support and cannot silently become
    /// a direct-OT execution.
    UnsupportedProfile,
    /// Label width differs from the active garbling label representation.
    WrongLabelWidth,
    /// The public manifest repeats a circuit input position.
    DuplicateWirePosition,
    /// The label-pair or choice count differs from the manifest count.
    LengthMismatch,
}

impl InputLabelManifest {
    /// Validate one public batch identity.
    pub fn new(
        session_id: [u8; 32],
        circuit_digest: [u8; 32],
        wire_positions: Vec<u32>,
        label_bytes: usize,
    ) -> Result<Self, InputLabelError> {
        let mut seen = BTreeSet::new();
        if !wire_positions.iter().all(|position| seen.insert(*position)) {
            return Err(InputLabelError::DuplicateWirePosition);
        }
        Ok(Self {
            session_id,
            circuit_digest,
            owner: InputLabelOwner::Evaluator,
            wire_positions,
            label_bytes,
        })
    }

    /// Number of selected labels in delivery order.
    pub fn len(&self) -> usize {
        self.wire_positions.len()
    }

    /// Whether this public batch has no labels.
    pub fn is_empty(&self) -> bool {
        self.wire_positions.is_empty()
    }
}

/// Batched selected-label delivery.
///
/// Both methods are called in the same public manifest order. Implementations
/// must return exactly one active label per manifest position and must not
/// decode a choice or expose a label to the wrong role.
pub trait InputLabelDelivery<N: VoleArray<u8>> {
    /// Garbler side: prepare/deliver two labels for every manifest position.
    fn send(
        &mut self,
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        labels: &[[Array<u8, N>; 2]],
    ) -> Result<(), InputLabelError>;

    /// Evaluator side: obtain exactly one active label for every choice.
    fn receive(
        &mut self,
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        choices: &[bool],
    ) -> Result<Vec<Array<u8, N>>, InputLabelError>;
}

/// Default batched adapter over the existing per-wire OT interface.
///
/// This is intentionally the default profile. It provides a conformance target
/// for a future TinyLabels implementation without making the runner learn
/// Ring-LWE setup or framing details.
pub struct DirectOtDelivery<'a, N: VoleArray<u8>> {
    ot: &'a mut dyn OtChannel<N>,
}

impl<'a, N: VoleArray<u8>> DirectOtDelivery<'a, N> {
    /// Wrap an existing role-local OT channel.
    pub fn new(ot: &'a mut dyn OtChannel<N>) -> Self {
        Self { ot }
    }

    fn validate(
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        count: usize,
    ) -> Result<(), InputLabelError> {
        if profile != InputLabelProfile::DirectOt {
            return Err(InputLabelError::UnsupportedProfile);
        }
        if manifest.label_bytes != N::USIZE {
            return Err(InputLabelError::WrongLabelWidth);
        }
        if manifest.len() != count {
            return Err(InputLabelError::LengthMismatch);
        }
        Ok(())
    }
}

impl<N: VoleArray<u8>> InputLabelDelivery<N> for DirectOtDelivery<'_, N> {
    fn send(
        &mut self,
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        labels: &[[Array<u8, N>; 2]],
    ) -> Result<(), InputLabelError> {
        Self::validate(profile, manifest, labels.len())?;
        for pair in labels {
            self.ot.send([&pair[0], &pair[1]]);
        }
        Ok(())
    }

    fn receive(
        &mut self,
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        choices: &[bool],
    ) -> Result<Vec<Array<u8, N>>, InputLabelError> {
        Self::validate(profile, manifest, choices.len())?;
        Ok(choices
            .iter()
            .map(|choice| self.ot.receive(*choice))
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use hybrid_array::Array;
    use typenum::U16;

    use super::{
        DirectOtDelivery, InputLabelDelivery, InputLabelError, InputLabelManifest,
        InputLabelProfile,
    };
    use crate::ot::LoopbackOt;

    #[test]
    fn direct_delivery_preserves_order_and_exactly_one_selected_label() {
        type N = U16;
        let manifest = InputLabelManifest::new([1; 32], [2; 32], vec![7, 3], 16)
            .expect("unique wire positions");
        let first = [
            Array::<u8, N>::from_fn(|_| 0x10),
            Array::<u8, N>::from_fn(|_| 0x11),
        ];
        let second = [
            Array::<u8, N>::from_fn(|_| 0x20),
            Array::<u8, N>::from_fn(|_| 0x21),
        ];
        let mut ot = LoopbackOt::new();
        DirectOtDelivery::new(&mut ot)
            .send(
                InputLabelProfile::DirectOt,
                &manifest,
                &[first.clone(), second.clone()],
            )
            .expect("send");
        let received = DirectOtDelivery::new(&mut ot)
            .receive(InputLabelProfile::DirectOt, &manifest, &[true, false])
            .expect("receive");
        assert_eq!(received, vec![first[1].clone(), second[0].clone()]);
    }

    #[test]
    fn manifests_reject_duplicates_and_direct_adapter_rejects_tinylabels_fallback() {
        assert_eq!(
            InputLabelManifest::new([0; 32], [0; 32], vec![1, 1], 16),
            Err(InputLabelError::DuplicateWirePosition)
        );
        type N = U16;
        let manifest = InputLabelManifest::new([0; 32], [0; 32], vec![], 16).unwrap();
        let mut ot = LoopbackOt::<N>::new();
        assert_eq!(
            DirectOtDelivery::new(&mut ot).receive(
                InputLabelProfile::TinyLabelsServer,
                &manifest,
                &[]
            ),
            Err(InputLabelError::UnsupportedProfile)
        );
    }
}
