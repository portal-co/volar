//! Public manifest shape for a deferred MPC external boundary.
//!
//! This module does not execute an action or oracle. It binds the public
//! request identity/order/executor/output geometry that a future strict batch
//! transport will authenticate before revealing inputs or reinserting labels.

use alloc::collections::BTreeSet;
use alloc::vec::Vec;

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
        assert!(
            ExternalBatchManifest::from_actions(
                ExternalBoundaryId(4),
                &[action(1, 0), action(2, 2)],
            )
            .is_err()
        );
    }
}
