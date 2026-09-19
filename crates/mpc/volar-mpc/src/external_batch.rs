//! Public manifest shape for a deferred MPC external boundary.
//!
//! This module does not execute an action or oracle. It binds the public
//! request identity/order/executor/output geometry that a future strict batch
//! transport will authenticate before revealing inputs or reinserting labels.

use alloc::collections::BTreeSet;
use alloc::vec::Vec;

use digest::Digest;
use sha2::Sha256;

use crate::{ActionExecutionPolicy, ActionSpec, MpcError};

/// Public identity of a boundary within one circuit/session execution.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExternalBoundaryId(pub u64);

/// One action entry in a boundary manifest.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExternalActionManifestEntry {
    pub request_id: u64,
    pub action_ordinal: u64,
    pub execution: ActionExecutionPolicy,
    pub output_bits: usize,
}

/// Public, deterministic action section of an external boundary manifest.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalBatchManifest {
    pub boundary: ExternalBoundaryId,
    pub actions: Vec<ExternalActionManifestEntry>,
}

/// Session- and circuit-bound digest of one public external boundary manifest.
///
/// This is a compact domain-separated binding record, not a claim that the
/// legacy action transport has become a replay-safe batch protocol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExternalBatchBinding(pub [u8; 32]);

/// Maximum public action entries accepted from an external-batch frame.
///
/// This is a parser allocation bound, not a scheduling resource policy.
pub const EXTERNAL_BATCH_MAX_ACTIONS: usize = 4_096;
/// Maximum label/result entries accepted in one external-batch frame.
pub const EXTERNAL_BATCH_MAX_VALUES: usize = 65_536;
/// Maximum bytes in one individual external-batch value.
pub const EXTERNAL_BATCH_MAX_VALUE_BYTES: usize = 1 << 20;

/// Versioned wire envelope for future strict external boundary batches.
///
/// The envelope is intentionally transport-neutral and does not itself reveal
/// a logical bit or run a host. Every non-manifest phase echoes the manifest
/// binding and request ID, so a role cannot reinterpret same-shaped material
/// from another boundary as its own request.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternalBatchFrame {
    Manifest {
        binding: ExternalBatchBinding,
        manifest: ExternalBatchManifest,
    },
    Reveal {
        binding: ExternalBatchBinding,
        request_id: u64,
        labels: Vec<Vec<u8>>,
    },
    ClearInputs {
        binding: ExternalBatchBinding,
        request_id: u64,
        bits: Vec<bool>,
    },
    Result {
        binding: ExternalBatchBinding,
        request_id: u64,
        bits: Vec<bool>,
    },
    Reinserted {
        binding: ExternalBatchBinding,
        request_id: u64,
    },
}

/// Fail-closed external-batch frame parse or local-manifest validation error.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExternalBatchFrameError {
    Malformed,
    TooLarge,
    ManifestMismatch,
    RequestMismatch,
    ResultWidthMismatch,
    UnexpectedPhase,
}

/// Public progress state for the conservative ordered action-batch envelope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExternalBatchPhase {
    Reveal,
    ClearInputs,
    Result,
    Reinserted,
    Complete,
}

/// Fail-closed validator for one admitted batch's conservative action flow.
///
/// It enforces the current common transcript order
/// `Reveal → ClearInputs → Result → Reinserted` for every manifest action in
/// action-chain order. It does not authorize any executor: strict adapters
/// must separately check their executor/reveal capability before creating a
/// transcript. This state machine deliberately contains no labels or result
/// bits beyond bounded frame validation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalBatchTranscript {
    manifest: ExternalBatchManifest,
    binding: ExternalBatchBinding,
    next_action: usize,
    phase: ExternalBatchPhase,
}

impl ExternalBatchTranscript {
    /// Start after both roles independently validated the manifest frame.
    pub fn new(manifest: ExternalBatchManifest, binding: ExternalBatchBinding) -> Self {
        let phase = if manifest.actions.is_empty() {
            ExternalBatchPhase::Complete
        } else {
            ExternalBatchPhase::Reveal
        };
        Self {
            manifest,
            binding,
            next_action: 0,
            phase,
        }
    }

    pub const fn phase(&self) -> ExternalBatchPhase {
        self.phase
    }

    pub const fn is_complete(&self) -> bool {
        matches!(self.phase, ExternalBatchPhase::Complete)
    }

    /// Accept one received/sent batch phase frame, validating identity, width,
    /// and exact request/action-chain order before advancing.
    pub fn accept(&mut self, frame: &ExternalBatchFrame) -> Result<(), ExternalBatchFrameError> {
        frame.validate_request(&self.manifest, self.binding)?;
        let expected = self
            .manifest
            .actions
            .get(self.next_action)
            .ok_or(ExternalBatchFrameError::UnexpectedPhase)?;
        let request_id = match frame {
            ExternalBatchFrame::Reveal { request_id, .. }
            | ExternalBatchFrame::ClearInputs { request_id, .. }
            | ExternalBatchFrame::Result { request_id, .. }
            | ExternalBatchFrame::Reinserted { request_id, .. } => *request_id,
            ExternalBatchFrame::Manifest { .. } => {
                return Err(ExternalBatchFrameError::UnexpectedPhase);
            }
        };
        if request_id != expected.request_id {
            return Err(ExternalBatchFrameError::RequestMismatch);
        }
        self.phase = match (self.phase, frame) {
            (ExternalBatchPhase::Reveal, ExternalBatchFrame::Reveal { .. }) => {
                ExternalBatchPhase::ClearInputs
            }
            (ExternalBatchPhase::ClearInputs, ExternalBatchFrame::ClearInputs { .. }) => {
                ExternalBatchPhase::Result
            }
            (ExternalBatchPhase::Result, ExternalBatchFrame::Result { .. }) => {
                ExternalBatchPhase::Reinserted
            }
            (ExternalBatchPhase::Reinserted, ExternalBatchFrame::Reinserted { .. }) => {
                self.next_action += 1;
                if self.next_action == self.manifest.actions.len() {
                    ExternalBatchPhase::Complete
                } else {
                    ExternalBatchPhase::Reveal
                }
            }
            _ => return Err(ExternalBatchFrameError::UnexpectedPhase),
        };
        Ok(())
    }
}

impl ExternalBatchManifest {
    /// Construct the canonical action manifest for `actions`.
    ///
    /// Action specs must have unique request identities and a contiguous,
    /// increasing source action chain. A caller may select one contiguous
    /// action-chain slice; a future multi-boundary runner must enforce
    /// cross-boundary continuity.
    pub fn from_actions(
        boundary: ExternalBoundaryId,
        actions: &[ActionSpec],
    ) -> Result<Self, MpcError> {
        let mut entries: Vec<_> = actions
            .iter()
            .map(|action| ExternalActionManifestEntry {
                request_id: action.request_id,
                action_ordinal: action.action_ordinal,
                execution: action.execution,
                output_bits: action.num_bits,
            })
            .collect();
        entries.sort_by_key(|entry| entry.action_ordinal);
        Self::from_entries(boundary, entries).map_err(|_| MpcError::MalformedSchedule)
    }
}

impl ExternalBatchManifest {
    fn from_entries(
        boundary: ExternalBoundaryId,
        entries: Vec<ExternalActionManifestEntry>,
    ) -> Result<Self, ExternalBatchFrameError> {
        if entries.len() > EXTERNAL_BATCH_MAX_ACTIONS {
            return Err(ExternalBatchFrameError::TooLarge);
        }
        let mut ids = BTreeSet::new();
        let first_ordinal = entries.first().map_or(0, |entry| entry.action_ordinal);
        for (offset, entry) in entries.iter().enumerate() {
            if !ids.insert(entry.request_id)
                || entry.action_ordinal != first_ordinal + offset as u64
                || entry.output_bits == 0
            {
                return Err(ExternalBatchFrameError::Malformed);
            }
        }
        Ok(Self {
            boundary,
            actions: entries,
        })
    }

    /// Bind this public manifest to caller-provided session and circuit
    /// digests. The field layout is canonical and length-delimited through the
    /// fixed-width action entries; malformed schedules cannot affect it.
    pub fn bind(&self, session_digest: [u8; 32], circuit_digest: [u8; 32]) -> ExternalBatchBinding {
        let mut bytes = Vec::with_capacity(8 + 32 + 32 + self.actions.len() * 80);
        bytes.extend_from_slice(b"volar.external-boundary.v1");
        bytes.extend_from_slice(&self.boundary.0.to_le_bytes());
        bytes.extend_from_slice(&session_digest);
        bytes.extend_from_slice(&circuit_digest);
        bytes.extend_from_slice(&(self.actions.len() as u64).to_le_bytes());
        for action in &self.actions {
            bytes.extend_from_slice(&action.request_id.to_le_bytes());
            bytes.extend_from_slice(&action.action_ordinal.to_le_bytes());
            bytes.push(match action.execution.executor {
                crate::ExternalExecutor::Garbler => 0,
                crate::ExternalExecutor::Evaluator => 1,
            });
            bytes.push(match action.execution.reveal {
                crate::ExternalRevealPolicy::ExecutorOnly => 0,
                crate::ExternalRevealPolicy::BothRoles => 1,
            });
            bytes.extend_from_slice(&action.execution.fingerprint);
            bytes.extend_from_slice(&(action.output_bits as u64).to_le_bytes());
        }
        let mut digest = Sha256::new();
        digest.update(b"volar.external-boundary.binding");
        digest.update(bytes);
        ExternalBatchBinding(digest.finalize().into())
    }
}

impl ExternalBatchFrame {
    /// Encode this version-one envelope. Receivers must validate its manifest
    /// against their independently derived schedule/session binding.
    pub fn encode(&self) -> Vec<u8> {
        let mut out = Vec::new();
        match self {
            Self::Manifest { binding, manifest } => {
                out.push(0);
                out.extend_from_slice(&binding.0);
                put_u64(&mut out, manifest.boundary.0);
                put_u32(&mut out, manifest.actions.len() as u32);
                for action in &manifest.actions {
                    put_u64(&mut out, action.request_id);
                    put_u64(&mut out, action.action_ordinal);
                    out.push(match action.execution.executor {
                        crate::ExternalExecutor::Garbler => 0,
                        crate::ExternalExecutor::Evaluator => 1,
                    });
                    out.push(match action.execution.reveal {
                        crate::ExternalRevealPolicy::ExecutorOnly => 0,
                        crate::ExternalRevealPolicy::BothRoles => 1,
                    });
                    out.extend_from_slice(&action.execution.fingerprint);
                    put_u64(&mut out, action.output_bits as u64);
                }
            }
            Self::Reveal {
                binding,
                request_id,
                labels,
            } => {
                out.push(1);
                put_binding(&mut out, binding);
                put_u64(&mut out, *request_id);
                put_values(&mut out, labels);
            }
            Self::ClearInputs {
                binding,
                request_id,
                bits,
            } => {
                out.push(2);
                put_binding(&mut out, binding);
                put_u64(&mut out, *request_id);
                put_bits(&mut out, bits);
            }
            Self::Result {
                binding,
                request_id,
                bits,
            } => {
                out.push(3);
                put_binding(&mut out, binding);
                put_u64(&mut out, *request_id);
                put_bits(&mut out, bits);
            }
            Self::Reinserted {
                binding,
                request_id,
            } => {
                out.push(4);
                put_binding(&mut out, binding);
                put_u64(&mut out, *request_id);
            }
        }
        out
    }

    /// Decode only bounded, exact-length version-one envelopes.
    pub fn decode(bytes: &[u8]) -> Result<Self, ExternalBatchFrameError> {
        let mut reader = BatchReader { bytes, position: 0 };
        let frame = match reader.byte()? {
            0 => {
                let binding = reader.binding()?;
                let boundary = ExternalBoundaryId(reader.u64()?);
                let count = reader.count(EXTERNAL_BATCH_MAX_ACTIONS)?;
                let mut actions = Vec::with_capacity(count);
                for _ in 0..count {
                    let request_id = reader.u64()?;
                    let action_ordinal = reader.u64()?;
                    let executor = match reader.byte()? {
                        0 => crate::ExternalExecutor::Garbler,
                        1 => crate::ExternalExecutor::Evaluator,
                        _ => return Err(ExternalBatchFrameError::Malformed),
                    };
                    let reveal = match reader.byte()? {
                        0 => crate::ExternalRevealPolicy::ExecutorOnly,
                        1 => crate::ExternalRevealPolicy::BothRoles,
                        _ => return Err(ExternalBatchFrameError::Malformed),
                    };
                    let fingerprint = reader.array()?;
                    let output_bits = usize::try_from(reader.u64()?)
                        .map_err(|_| ExternalBatchFrameError::TooLarge)?;
                    actions.push(ExternalActionManifestEntry {
                        request_id,
                        action_ordinal,
                        execution: ActionExecutionPolicy {
                            executor,
                            reveal,
                            fingerprint,
                        },
                        output_bits,
                    });
                }
                Self::Manifest {
                    binding,
                    manifest: ExternalBatchManifest::from_entries(boundary, actions)?,
                }
            }
            1 => Self::Reveal {
                binding: reader.binding()?,
                request_id: reader.u64()?,
                labels: reader.values()?,
            },
            2 => Self::ClearInputs {
                binding: reader.binding()?,
                request_id: reader.u64()?,
                bits: reader.bits()?,
            },
            3 => Self::Result {
                binding: reader.binding()?,
                request_id: reader.u64()?,
                bits: reader.bits()?,
            },
            4 => Self::Reinserted {
                binding: reader.binding()?,
                request_id: reader.u64()?,
            },
            _ => return Err(ExternalBatchFrameError::Malformed),
        };
        if reader.position == bytes.len() {
            Ok(frame)
        } else {
            Err(ExternalBatchFrameError::Malformed)
        }
    }

    /// Ensure a peer-provided manifest frame exactly matches the locally
    /// derived manifest and session/circuit binding before any reveal phase.
    pub fn validate_manifest(
        &self,
        expected_manifest: &ExternalBatchManifest,
        expected_binding: ExternalBatchBinding,
    ) -> Result<(), ExternalBatchFrameError> {
        match self {
            Self::Manifest { binding, manifest }
                if *binding == expected_binding && manifest == expected_manifest =>
            {
                Ok(())
            }
            _ => Err(ExternalBatchFrameError::ManifestMismatch),
        }
    }

    /// Validate a non-manifest phase against the admitted public manifest.
    ///
    /// `Result` frames must contain exactly the declared action output width;
    /// all phases must echo the current binding and name a manifest request.
    /// Reveal/clear-input geometry is intentionally left to the request's
    /// declaration adapter because the action manifest does not carry its
    /// guard/argument/fallback widths.
    pub fn validate_request(
        &self,
        manifest: &ExternalBatchManifest,
        binding: ExternalBatchBinding,
    ) -> Result<(), ExternalBatchFrameError> {
        let (frame_binding, request_id, result_width) = match self {
            Self::Manifest { .. } => return Err(ExternalBatchFrameError::RequestMismatch),
            Self::Reveal {
                binding,
                request_id,
                ..
            }
            | Self::ClearInputs {
                binding,
                request_id,
                ..
            }
            | Self::Reinserted {
                binding,
                request_id,
            } => (*binding, *request_id, None),
            Self::Result {
                binding,
                request_id,
                bits,
            } => (*binding, *request_id, Some(bits.len())),
        };
        if frame_binding != binding {
            return Err(ExternalBatchFrameError::ManifestMismatch);
        }
        let action = manifest
            .actions
            .iter()
            .find(|action| action.request_id == request_id)
            .ok_or(ExternalBatchFrameError::RequestMismatch)?;
        if let Some(width) = result_width
            && width != action.output_bits
        {
            return Err(ExternalBatchFrameError::ResultWidthMismatch);
        }
        Ok(())
    }
}

fn put_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}
fn put_u64(out: &mut Vec<u8>, value: u64) {
    out.extend_from_slice(&value.to_le_bytes());
}
fn put_binding(out: &mut Vec<u8>, binding: &ExternalBatchBinding) {
    out.extend_from_slice(&binding.0);
}
fn put_values(out: &mut Vec<u8>, values: &[Vec<u8>]) {
    put_u32(out, values.len() as u32);
    for value in values {
        put_u32(out, value.len() as u32);
        out.extend_from_slice(value);
    }
}
fn put_bits(out: &mut Vec<u8>, bits: &[bool]) {
    put_u32(out, bits.len() as u32);
    out.extend(bits.iter().map(|bit| *bit as u8));
}

struct BatchReader<'a> {
    bytes: &'a [u8],
    position: usize,
}

impl<'a> BatchReader<'a> {
    fn byte(&mut self) -> Result<u8, ExternalBatchFrameError> {
        let value = *self
            .bytes
            .get(self.position)
            .ok_or(ExternalBatchFrameError::Malformed)?;
        self.position += 1;
        Ok(value)
    }
    fn take(&mut self, len: usize) -> Result<&'a [u8], ExternalBatchFrameError> {
        let end = self
            .position
            .checked_add(len)
            .ok_or(ExternalBatchFrameError::TooLarge)?;
        let slice = self
            .bytes
            .get(self.position..end)
            .ok_or(ExternalBatchFrameError::Malformed)?;
        self.position = end;
        Ok(slice)
    }
    fn u32(&mut self) -> Result<u32, ExternalBatchFrameError> {
        let bytes = self.take(4)?;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }
    fn u64(&mut self) -> Result<u64, ExternalBatchFrameError> {
        let bytes = self.take(8)?;
        Ok(u64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]))
    }
    fn array(&mut self) -> Result<[u8; 32], ExternalBatchFrameError> {
        let bytes = self.take(32)?;
        let mut out = [0; 32];
        out.copy_from_slice(bytes);
        Ok(out)
    }
    fn binding(&mut self) -> Result<ExternalBatchBinding, ExternalBatchFrameError> {
        Ok(ExternalBatchBinding(self.array()?))
    }
    fn count(&mut self, maximum: usize) -> Result<usize, ExternalBatchFrameError> {
        let count = usize::try_from(self.u32()?).map_err(|_| ExternalBatchFrameError::TooLarge)?;
        if count > maximum {
            Err(ExternalBatchFrameError::TooLarge)
        } else {
            Ok(count)
        }
    }
    fn values(&mut self) -> Result<Vec<Vec<u8>>, ExternalBatchFrameError> {
        let count = self.count(EXTERNAL_BATCH_MAX_VALUES)?;
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            let len = self.count(EXTERNAL_BATCH_MAX_VALUE_BYTES)?;
            values.push(self.take(len)?.to_vec());
        }
        Ok(values)
    }
    fn bits(&mut self) -> Result<Vec<bool>, ExternalBatchFrameError> {
        let count = self.count(EXTERNAL_BATCH_MAX_VALUES)?;
        let mut bits = Vec::with_capacity(count);
        for _ in 0..count {
            match self.byte()? {
                0 => bits.push(false),
                1 => bits.push(true),
                _ => return Err(ExternalBatchFrameError::Malformed),
            }
        }
        Ok(bits)
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;
    use crate::{ExternalExecutor, ExternalRevealPolicy};

    fn action(request_id: u64, ordinal: u64) -> ActionSpec {
        ActionSpec {
            name: "test".into(),
            request_id,
            action_ordinal: ordinal,
            execution: ActionExecutionPolicy {
                executor: ExternalExecutor::Evaluator,
                reveal: ExternalRevealPolicy::BothRoles,
                fingerprint: [request_id as u8; 32],
            },
            guard: 0,
            arg_wires: Vec::new(),
            fallback_wires: vec![0],
            num_bits: 1,
            guard_polarity: false,
            arg_polarity: Vec::new(),
            fallback_polarity: Vec::new(),
        }
    }

    #[test]
    fn external_batch_frames_are_exact_bound_and_canonical() {
        let manifest = ExternalBatchManifest::from_actions(
            ExternalBoundaryId(9),
            &[action(12, 3), action(13, 4)],
        )
        .unwrap();
        let binding = manifest.bind([4; 32], [5; 32]);
        let frame = ExternalBatchFrame::Manifest {
            binding,
            manifest: manifest.clone(),
        };
        let encoded = frame.encode();
        let decoded = ExternalBatchFrame::decode(&encoded).unwrap();
        assert_eq!(decoded, frame);
        decoded.validate_manifest(&manifest, binding).unwrap();
        ExternalBatchFrame::Result {
            binding,
            request_id: 12,
            bits: vec![true],
        }
        .validate_request(&manifest, binding)
        .unwrap();
        assert_eq!(
            ExternalBatchFrame::Result {
                binding,
                request_id: 12,
                bits: vec![true, false],
            }
            .validate_request(&manifest, binding),
            Err(ExternalBatchFrameError::ResultWidthMismatch)
        );
        assert_eq!(
            ExternalBatchFrame::Reinserted {
                binding,
                request_id: 99,
            }
            .validate_request(&manifest, binding),
            Err(ExternalBatchFrameError::RequestMismatch)
        );
        let mut trailing = encoded;
        trailing.push(0);
        assert_eq!(
            ExternalBatchFrame::decode(&trailing),
            Err(ExternalBatchFrameError::Malformed)
        );
        assert_eq!(
            ExternalBatchFrame::Reveal {
                binding,
                request_id: 12,
                labels: vec![vec![1, 2]],
            }
            .encode(),
            ExternalBatchFrame::Reveal {
                binding,
                request_id: 12,
                labels: vec![vec![1, 2]],
            }
            .encode()
        );
    }

    #[test]
    fn transcript_enforces_ordered_complete_reinsertion() {
        let manifest = ExternalBatchManifest::from_actions(
            ExternalBoundaryId(2),
            &[action(12, 0), action(13, 1)],
        )
        .unwrap();
        let binding = manifest.bind([1; 32], [2; 32]);
        let mut transcript = ExternalBatchTranscript::new(manifest, binding);
        for request_id in [12, 13] {
            for frame in [
                ExternalBatchFrame::Reveal {
                    binding,
                    request_id,
                    labels: vec![],
                },
                ExternalBatchFrame::ClearInputs {
                    binding,
                    request_id,
                    bits: vec![],
                },
                ExternalBatchFrame::Result {
                    binding,
                    request_id,
                    bits: vec![true],
                },
                ExternalBatchFrame::Reinserted {
                    binding,
                    request_id,
                },
            ] {
                transcript.accept(&frame).unwrap();
            }
        }
        assert!(transcript.is_complete());
        let mut wrong_order = ExternalBatchTranscript::new(
            ExternalBatchManifest::from_actions(ExternalBoundaryId(3), &[action(8, 0)]).unwrap(),
            ExternalBatchManifest::from_actions(ExternalBoundaryId(3), &[action(8, 0)])
                .unwrap()
                .bind([1; 32], [2; 32]),
        );
        assert_eq!(
            wrong_order.accept(&ExternalBatchFrame::Result {
                binding: wrong_order.binding,
                request_id: 8,
                bits: vec![true],
            }),
            Err(ExternalBatchFrameError::UnexpectedPhase)
        );
    }

    #[test]
    fn canonicalizes_source_action_order_and_rejects_gaps() {
        let manifest = ExternalBatchManifest::from_actions(
            ExternalBoundaryId(4),
            &[action(99, 1), action(42, 0)],
        )
        .unwrap();
        assert_eq!(manifest.actions[0].request_id, 42);
        assert_eq!(manifest.actions[1].request_id, 99);
        assert_ne!(
            manifest.bind([1; 32], [2; 32]),
            manifest.bind([1; 32], [3; 32])
        );
        assert!(
            ExternalBatchManifest::from_actions(
                ExternalBoundaryId(4),
                &[action(1, 0), action(2, 2)],
            )
            .is_err()
        );
        assert!(
            ExternalBatchManifest::from_actions(
                ExternalBoundaryId(5),
                &[action(7, 4), action(8, 5)],
            )
            .is_ok()
        );
    }
}
