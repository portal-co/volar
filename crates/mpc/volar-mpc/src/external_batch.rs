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

impl ExternalBatchManifest {
    /// Construct the canonical action manifest for `actions`.
    ///
    /// Action specs must have unique request identities and a contiguous,
    /// increasing source action chain. The caller may select a subset only if
    /// it is a contiguous action-chain prefix at this boundary; a later batch
    /// constructor will enforce cross-boundary continuity.
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
        let mut ids = BTreeSet::new();
        for (expected, entry) in entries.iter().enumerate() {
            if !ids.insert(entry.request_id)
                || entry.action_ordinal != expected as u64
                || entry.output_bits == 0
            {
                return Err(MpcError::MalformedSchedule);
            }
        }
        Ok(Self {
            boundary,
            actions: entries,
        })
    }
}

impl ExternalBatchManifest {
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
    }
}
