//! Generic deferred external-boundary planner for MPC circuits.
//!
//! The planner is deliberately host- and protocol-neutral. It receives public
//! request metadata and dependency edges, produces deterministic batches, and
//! never sees a plaintext, garbled label, storage address, or action result.
//! Strict-GC, strict-chain, VC, and provider adapters execute the resulting
//! batches with their own reveal/reinsertion protocols.
//!
//! Action requests preserve one total side-effect chain. Oracle requests are
//! pure and can be deduplicated by a caller-provided canonical equivalence
//! fingerprint. Storage retains its existing order only through explicit
//! dependency edges; the generic planner adds no action/storage interference
//! edge.

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

use volar_ir_common::{ActionExecutionPolicy, OracleExecutionKind, OracleExecutionPolicy};

/// Stable public identity of one external request occurrence.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExternalRequestId(pub u64);

/// Public class of a requested boundary operation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExternalRequestKind {
    /// Ordered conditional external side effect.
    Action(ActionExecutionPolicy),
    /// Pure deterministic external computation.
    Oracle(OracleExecutionPolicy),
    /// Existing storage/ORAM work. Ordering is supplied by dependency edges.
    Storage,
}

/// Public request metadata supplied by a lowering/scheduling adapter.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalRequest {
    pub id: ExternalRequestId,
    pub kind: ExternalRequestKind,
    /// Result / argument dependencies represented by other external requests.
    pub depends_on: Vec<ExternalRequestId>,
    /// A pure oracle is retained only when demanded by a surviving consumer.
    /// Actions and storage are live whenever present in the input.
    pub demanded: bool,
    /// Canonical pure-oracle equivalence key. `None` disables deduplication.
    /// The key must include declaration/profile/fingerprint, argument wire
    /// identity, geometry, and public domain separation.
    pub oracle_equivalence: Option<[u8; 32]>,
}

/// A canonical one-way boundary batch. Request order is protocol/audit order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalBatch {
    pub requests: Vec<ExternalRequestId>,
}

/// The immutable public scheduling result.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalBoundaryPlan {
    pub batches: Vec<ExternalBatch>,
    /// Every deduplicated oracle occurrence maps to its retained
    /// representative. Non-oracle requests and representatives map to self.
    pub representative: BTreeMap<ExternalRequestId, ExternalRequestId>,
}

/// Limits that split a ready set into multiple deterministic batches. They
/// never remove or reorder requests.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExternalBatchLimits {
    pub max_requests: usize,
}

impl Default for ExternalBatchLimits {
    fn default() -> Self {
        Self { max_requests: 64 }
    }
}

/// Fail-closed public planning errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternalBoundaryError {
    ZeroBatchCapacity,
    DuplicateRequest(ExternalRequestId),
    UnknownDependency {
        request: ExternalRequestId,
        dependency: ExternalRequestId,
    },
    DependencyCycle,
    OraclePolicyMismatch(ExternalRequestId),
}

/// Build maximal deterministic batches subject to `limits`.
///
/// Actions are always emitted in occurrence-ID chain order. This intentionally
/// gives the action chain no default relation to storage: a ready storage
/// request may co-batch with the next ready action. Oracles are grouped by
/// canonical equivalence only after validating they are pure assigned oracles;
/// replicated oracle execution remains deliberately unsupported until a
/// consistency protocol exists.
pub fn plan_external_boundaries(
    requests: &[ExternalRequest],
    limits: ExternalBatchLimits,
) -> Result<ExternalBoundaryPlan, ExternalBoundaryError> {
    if limits.max_requests == 0 {
        return Err(ExternalBoundaryError::ZeroBatchCapacity);
    }

    let mut by_id = BTreeMap::new();
    for request in requests {
        if by_id.insert(request.id, request).is_some() {
            return Err(ExternalBoundaryError::DuplicateRequest(request.id));
        }
    }
    for request in requests {
        for &dependency in &request.depends_on {
            if !by_id.contains_key(&dependency) {
                return Err(ExternalBoundaryError::UnknownDependency {
                    request: request.id,
                    dependency,
                });
            }
        }
        if let ExternalRequestKind::Oracle(policy) = request.kind {
            if policy.execution == OracleExecutionKind::Replicated {
                return Err(ExternalBoundaryError::OraclePolicyMismatch(request.id));
            }
        }
    }

    let mut representative = BTreeMap::new();
    let mut oracle_keys = BTreeMap::<[u8; 32], ExternalRequestId>::new();
    for request in requests {
        let retained = match request.kind {
            ExternalRequestKind::Oracle(_) if !request.demanded => false,
            _ => true,
        };
        if !retained {
            continue;
        }
        if matches!(request.kind, ExternalRequestKind::Oracle(_)) {
            if let Some(key) = request.oracle_equivalence {
                if let Some(&first) = oracle_keys.get(&key) {
                    representative.insert(request.id, first);
                    continue;
                }
                oracle_keys.insert(key, request.id);
            }
        }
        representative.insert(request.id, request.id);
    }

    // A deduplicated request inherits dependencies through its representative.
    // An alias is not a runnable request and therefore never becomes pending.
    let runnable: BTreeSet<_> = representative
        .iter()
        .filter_map(|(&id, &rep)| (id == rep).then_some(id))
        .collect();
    let actions: Vec<_> = runnable
        .iter()
        .copied()
        .filter(|id| matches!(by_id[id].kind, ExternalRequestKind::Action(_)))
        .collect();
    let mut next_action = 0usize;
    let mut complete = BTreeSet::new();
    let mut batches = Vec::new();

    while complete.len() != runnable.len() {
        let mut ready = Vec::new();
        // The next action is the only action allowed to become ready. This is
        // the total source action chain; it does not constrain storage/oracle.
        if let Some(&id) = actions.get(next_action) {
            let request = by_id[&id];
            if dependencies_complete(request, &representative, &complete) {
                ready.push(id);
            }
        }
        for &id in &runnable {
            if complete.contains(&id) || matches!(by_id[&id].kind, ExternalRequestKind::Action(_)) {
                continue;
            }
            let request = by_id[&id];
            if dependencies_complete(request, &representative, &complete) {
                ready.push(id);
            }
        }
        // Stable category ordering: storage, single allowed action, oracle.
        ready.sort_by_key(|id| match by_id[id].kind {
            ExternalRequestKind::Storage => (0u8, id.0),
            ExternalRequestKind::Action(_) => (1, id.0),
            ExternalRequestKind::Oracle(_) => (2, id.0),
        });
        ready.truncate(limits.max_requests);
        if ready.is_empty() {
            return Err(ExternalBoundaryError::DependencyCycle);
        }
        for &id in &ready {
            if matches!(by_id[&id].kind, ExternalRequestKind::Action(_)) {
                next_action += 1;
            }
            complete.insert(id);
        }
        batches.push(ExternalBatch { requests: ready });
    }

    // Every retained representative is explicit; aliases are visible to the
    // consumer as result-binding redirects.
    for &id in &runnable {
        representative.entry(id).or_insert(id);
    }
    Ok(ExternalBoundaryPlan {
        batches,
        representative,
    })
}

fn dependencies_complete(
    request: &ExternalRequest,
    representative: &BTreeMap<ExternalRequestId, ExternalRequestId>,
    complete: &BTreeSet<ExternalRequestId>,
) -> bool {
    request.depends_on.iter().all(|dependency| {
        representative
            .get(dependency)
            .map(|representative| complete.contains(representative))
            // An undemanded pure oracle has no materialized result. A caller
            // that references it will therefore be rejected as a cycle rather
            // than silently obtaining an absent value.
            .unwrap_or(false)
    })
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;
    use volar_ir_common::{ExternalExecutor, ExternalRevealPolicy};

    fn action(id: u64, deps: Vec<ExternalRequestId>) -> ExternalRequest {
        ExternalRequest {
            id: ExternalRequestId(id),
            kind: ExternalRequestKind::Action(ActionExecutionPolicy {
                executor: ExternalExecutor::Evaluator,
                reveal: ExternalRevealPolicy::BothRoles,
                fingerprint: [id as u8; 32],
            }),
            depends_on: deps,
            demanded: false,
            oracle_equivalence: None,
        }
    }

    fn oracle(id: u64, key: u8, demanded: bool, deps: Vec<ExternalRequestId>) -> ExternalRequest {
        ExternalRequest {
            id: ExternalRequestId(id),
            kind: ExternalRequestKind::Oracle(OracleExecutionPolicy {
                execution: OracleExecutionKind::Assigned,
                executor: ExternalExecutor::Evaluator,
                reveal: ExternalRevealPolicy::BothRoles,
                fingerprint: [key; 32],
            }),
            depends_on: deps,
            demanded,
            oracle_equivalence: Some([key; 32]),
        }
    }

    fn storage(id: u64, deps: Vec<ExternalRequestId>) -> ExternalRequest {
        ExternalRequest {
            id: ExternalRequestId(id),
            kind: ExternalRequestKind::Storage,
            depends_on: deps,
            demanded: true,
            oracle_equivalence: None,
        }
    }

    #[test]
    fn pure_oracles_cse_and_batch_with_independent_storage() {
        let plan = plan_external_boundaries(
            &[
                oracle(1, 7, true, vec![]),
                oracle(2, 7, true, vec![]),
                storage(3, vec![]),
            ],
            ExternalBatchLimits::default(),
        )
        .unwrap();
        assert_eq!(
            plan.representative[&ExternalRequestId(2)],
            ExternalRequestId(1)
        );
        assert_eq!(
            plan.batches,
            vec![ExternalBatch {
                requests: vec![ExternalRequestId(3), ExternalRequestId(1)]
            }]
        );
    }

    #[test]
    fn actions_preserve_chain_but_independent_storage_cobatches() {
        let plan = plan_external_boundaries(
            &[action(10, vec![]), action(20, vec![]), storage(5, vec![])],
            ExternalBatchLimits { max_requests: 3 },
        )
        .unwrap();
        assert_eq!(
            plan.batches,
            vec![
                ExternalBatch {
                    requests: vec![ExternalRequestId(5), ExternalRequestId(10)]
                },
                ExternalBatch {
                    requests: vec![ExternalRequestId(20)]
                },
            ]
        );
    }

    #[test]
    fn demand_and_value_edges_delay_oracles_without_reordering_actions() {
        let plan = plan_external_boundaries(
            &[
                action(10, vec![]),
                action(20, vec![]),
                oracle(30, 3, true, vec![ExternalRequestId(20)]),
                oracle(40, 4, false, vec![]),
            ],
            ExternalBatchLimits::default(),
        )
        .unwrap();
        assert_eq!(
            plan.batches,
            vec![
                ExternalBatch {
                    requests: vec![ExternalRequestId(10)]
                },
                ExternalBatch {
                    requests: vec![ExternalRequestId(20)]
                },
                ExternalBatch {
                    requests: vec![ExternalRequestId(30)]
                },
            ]
        );
        assert!(!plan.representative.contains_key(&ExternalRequestId(40)));
    }
}
