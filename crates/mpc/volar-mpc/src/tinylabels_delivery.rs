//! Explicit server-only TinyLabels delivery admission.
//!
//! This module is available only with `std + tinylabels`. It binds a public
//! [`InputLabelManifest`] to a resource budget and monotonic use counter before
//! delegating the actual Ring-LWE stage exchange to a caller-supplied protocol
//! implementation. This is deliberate: the shared `volar_spec::tinylabels`
//! core still lacks a reviewed sampler and canonical polynomial payload codec,
//! so this module must not fabricate a cryptographic wire protocol from its
//! in-memory staging values.
//!
//! The default [`crate::input_labels::DirectOtDelivery`] remains the only
//! normal input-delivery adapter. TinyLabels requires explicit construction of
//! [`TinyLabelsServerDelivery`] and cannot silently fall back to direct OT.

use alloc::vec::Vec;

use hybrid_array::Array;
use volar_spec::tinylabels::frame::{Frame, FrameBinding};
use volar_spec::vole::VoleArray;

use crate::input_labels::{
    InputLabelDelivery, InputLabelError, InputLabelManifest, InputLabelProfile,
};

/// Public resource limits admitted before a TinyLabels session begins.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TinyLabelsResourceBudget {
    /// Maximum frame payload accepted from the peer.
    pub max_frame_payload: usize,
    /// Maximum public reusable/offline bytes the server profile may retain.
    pub max_reusable_bytes: u64,
    /// Maximum public per-use bytes the server profile may retain.
    pub max_per_use_bytes: u64,
}

/// Public resource declaration for one explicitly selected TinyLabels profile.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TinyLabelsResourceRequest {
    /// Public reusable/offline storage required by this profile.
    pub reusable_bytes: u64,
    /// Public per-use storage required by this profile.
    pub per_use_bytes: u64,
}

/// Session-admission failure.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TinyLabelsAdmissionError {
    /// TinyLabels is defined only for the explicit TinyLabels profile.
    WrongProfile,
    /// The current garbling labels are not the required 16-byte format.
    WrongLabelWidth,
    /// The requested public storage exceeds the pre-agreed budget.
    ResourceBudgetExceeded,
    /// A caller attempted to reuse or skip a monotonic use number.
    UnexpectedUseCounter,
    /// A received frame did not bind to this admitted session/manifest/use.
    WrongFrameBinding,
}

/// Immutable admission for one session and ordered input-label manifest.
#[derive(Clone, Debug)]
pub struct TinyLabelsAdmission {
    manifest: InputLabelManifest,
    parameter_fingerprint: [u8; 32],
    budget: TinyLabelsResourceBudget,
    next_use_counter: u64,
}

impl TinyLabelsAdmission {
    /// Admit a server-only TinyLabels session.
    ///
    /// `manifest_digest` is intentionally supplied by the session transcript
    /// layer. It must be the canonical digest of `manifest`; this execution
    /// crate does not define compiler serialization.
    pub fn admit(
        profile: InputLabelProfile,
        manifest: InputLabelManifest,
        parameter_fingerprint: [u8; 32],
        budget: TinyLabelsResourceBudget,
        request: TinyLabelsResourceRequest,
    ) -> Result<Self, TinyLabelsAdmissionError> {
        if profile != InputLabelProfile::TinyLabelsServer {
            return Err(TinyLabelsAdmissionError::WrongProfile);
        }
        if manifest.label_bytes != 16 {
            return Err(TinyLabelsAdmissionError::WrongLabelWidth);
        }
        if request.reusable_bytes > budget.max_reusable_bytes
            || request.per_use_bytes > budget.max_per_use_bytes
        {
            return Err(TinyLabelsAdmissionError::ResourceBudgetExceeded);
        }
        Ok(Self {
            manifest,
            parameter_fingerprint,
            budget,
            next_use_counter: 0,
        })
    }

    /// Borrow the public manifest admitted for this profile.
    pub fn manifest(&self) -> &InputLabelManifest {
        &self.manifest
    }

    /// Maximum inbound payload accepted by [`Self::validate_frame`].
    pub fn max_frame_payload(&self) -> usize {
        self.budget.max_frame_payload
    }

    /// Begin exactly the next monotonic use and return its required binding.
    pub fn begin_use(
        &mut self,
        manifest_digest: [u8; 32],
        use_counter: u64,
    ) -> Result<FrameBinding, TinyLabelsAdmissionError> {
        if use_counter != self.next_use_counter {
            return Err(TinyLabelsAdmissionError::UnexpectedUseCounter);
        }
        self.next_use_counter = self.next_use_counter.saturating_add(1);
        Ok(FrameBinding {
            parameter_fingerprint: self.parameter_fingerprint,
            session_id: self.manifest.session_id,
            manifest_digest,
            use_counter,
        })
    }

    /// Decode one bounded frame and require its complete public binding.
    pub fn validate_frame(
        &self,
        bytes: &[u8],
        binding: FrameBinding,
    ) -> Result<Frame, TinyLabelsAdmissionError> {
        let frame = Frame::decode(bytes, self.budget.max_frame_payload)
            .map_err(|_| TinyLabelsAdmissionError::WrongFrameBinding)?;
        if frame.binding != binding {
            return Err(TinyLabelsAdmissionError::WrongFrameBinding);
        }
        Ok(frame)
    }
}

/// The role-specific protocol implementation behind the admitted adapter.
///
/// This is a **batch** interface. Implementations must use the shared
/// TinyLabels construction and transcript-bound frames; treating it as one
/// per-bit OT operation would erase the resource and latency facts that caused
/// the explicit profile in the first place.
pub trait TinyLabelsBatchProtocol<N: VoleArray<u8>> {
    /// Garbler side: deliver one manifest-ordered selected-label batch.
    fn send_batch(
        &mut self,
        admission: &TinyLabelsAdmission,
        binding: FrameBinding,
        labels: &[[Array<u8, N>; 2]],
    ) -> Result<(), TinyLabelsAdmissionError>;

    /// Evaluator side: recover one manifest-ordered active-label batch.
    fn receive_batch(
        &mut self,
        admission: &TinyLabelsAdmission,
        binding: FrameBinding,
        choices: &[bool],
    ) -> Result<Vec<Array<u8, N>>, TinyLabelsAdmissionError>;
}

/// Feature-gated TinyLabels adapter at the batched delivery seam.
///
/// It validates profile and label width before delegating. A concrete
/// [`TinyLabelsBatchProtocol`] is intentionally not shipped yet because its
/// sampler and polynomial codec remain cryptographic review gates.
pub struct TinyLabelsServerDelivery<'a, N: VoleArray<u8>, P> {
    admission: &'a TinyLabelsAdmission,
    binding: FrameBinding,
    protocol: P,
    _label: core::marker::PhantomData<N>,
}

impl<'a, N: VoleArray<u8>, P> TinyLabelsServerDelivery<'a, N, P> {
    /// Bind a protocol implementation to one admitted use.
    pub fn new(admission: &'a TinyLabelsAdmission, binding: FrameBinding, protocol: P) -> Self {
        Self {
            admission,
            binding,
            protocol,
            _label: core::marker::PhantomData,
        }
    }

    fn validate(&self, profile: InputLabelProfile, count: usize) -> Result<(), InputLabelError> {
        if profile != InputLabelProfile::TinyLabelsServer {
            return Err(InputLabelError::UnsupportedProfile);
        }
        if self.admission.manifest.label_bytes != N::USIZE {
            return Err(InputLabelError::WrongLabelWidth);
        }
        if self.admission.manifest.len() != count {
            return Err(InputLabelError::LengthMismatch);
        }
        Ok(())
    }
}

impl<N: VoleArray<u8>, P: TinyLabelsBatchProtocol<N>> InputLabelDelivery<N>
    for TinyLabelsServerDelivery<'_, N, P>
{
    fn send(
        &mut self,
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        labels: &[[Array<u8, N>; 2]],
    ) -> Result<(), InputLabelError> {
        self.validate(profile, labels.len())?;
        if manifest != self.admission.manifest() {
            return Err(InputLabelError::LengthMismatch);
        }
        self.protocol
            .send_batch(self.admission, self.binding, labels)
            .map_err(|_| InputLabelError::UnsupportedProfile)
    }

    fn receive(
        &mut self,
        profile: InputLabelProfile,
        manifest: &InputLabelManifest,
        choices: &[bool],
    ) -> Result<Vec<Array<u8, N>>, InputLabelError> {
        self.validate(profile, choices.len())?;
        if manifest != self.admission.manifest() {
            return Err(InputLabelError::LengthMismatch);
        }
        self.protocol
            .receive_batch(self.admission, self.binding, choices)
            .map_err(|_| InputLabelError::UnsupportedProfile)
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use hybrid_array::Array;
    use typenum::U16;
    use volar_spec::tinylabels::frame::{Frame, Stage};

    use super::{
        TinyLabelsAdmission, TinyLabelsAdmissionError, TinyLabelsResourceBudget,
        TinyLabelsResourceRequest,
    };
    use crate::input_labels::{InputLabelManifest, InputLabelProfile};

    fn admission() -> TinyLabelsAdmission {
        TinyLabelsAdmission::admit(
            InputLabelProfile::TinyLabelsServer,
            InputLabelManifest::new([1; 32], [2; 32], vec![3, 7], 16).unwrap(),
            [4; 32],
            TinyLabelsResourceBudget {
                max_frame_payload: 32,
                max_reusable_bytes: 128,
                max_per_use_bytes: 64,
            },
            TinyLabelsResourceRequest {
                reusable_bytes: 128,
                per_use_bytes: 64,
            },
        )
        .unwrap()
    }

    #[test]
    fn admission_binds_frames_to_exact_monotonic_use() {
        let mut admission = admission();
        let binding = admission.begin_use([5; 32], 0).unwrap();
        let frame = Frame {
            stage: Stage::ReusableCiphertext,
            binding,
            payload: vec![9; 8],
        };
        assert_eq!(
            admission.validate_frame(&frame.encode(), binding),
            Ok(frame)
        );
        assert_eq!(
            admission.begin_use([5; 32], 0),
            Err(TinyLabelsAdmissionError::UnexpectedUseCounter)
        );
        let wrong = admission.begin_use([6; 32], 1).unwrap();
        assert_eq!(
            admission.validate_frame(
                &Frame {
                    stage: Stage::Complete,
                    binding: wrong,
                    payload: vec![]
                }
                .encode(),
                binding
            ),
            Err(TinyLabelsAdmissionError::WrongFrameBinding)
        );
    }

    #[test]
    fn admission_rejects_budget_and_non_u16_labels() {
        let manifest = InputLabelManifest::new([0; 32], [0; 32], vec![], 8).unwrap();
        assert!(matches!(
            TinyLabelsAdmission::admit(
                InputLabelProfile::TinyLabelsServer,
                manifest,
                [0; 32],
                TinyLabelsResourceBudget {
                    max_frame_payload: 0,
                    max_reusable_bytes: 0,
                    max_per_use_bytes: 0,
                },
                TinyLabelsResourceRequest {
                    reusable_bytes: 0,
                    per_use_bytes: 0,
                },
            ),
            Err(TinyLabelsAdmissionError::WrongLabelWidth)
        ));
        let _ = Array::<u8, U16>::default();
    }
}
