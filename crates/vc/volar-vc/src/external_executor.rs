//! Clear-value executor for an already-admitted external boundary plan.
//!
//! This module is the merger between the generic planner's storage, action,
//! and pure-oracle batches. It deliberately operates on values supplied by a
//! separate reveal adapter and returns values for a separate reinsertion
//! adapter. It is therefore useful for deterministic host-order testing and
//! for binding a future strict label transport without putting label mechanics
//! into the optimizer.

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use volar_ir_common::{ExternalExecutor, ExternalRevealPolicy, OracleExecutionKind};

use crate::external_boundary::{
    ExternalBatch, ExternalBoundaryPlan, ExternalRequest, ExternalRequestId, ExternalRequestKind,
};

/// Clear geometry and local host metadata for one planned request.
///
/// `id` must identify an exact planner request; `name` is local host routing
/// metadata and never substitutes for that identity.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternalExecutionRequest {
    Action {
        id: ExternalRequestId,
        name: String,
        guard: bool,
        args: Vec<bool>,
        fallback: Vec<bool>,
    },
    Oracle {
        id: ExternalRequestId,
        name: String,
        args: Vec<bool>,
        output_bits: usize,
    },
    Storage {
        id: ExternalRequestId,
        output_bits: usize,
    },
}

impl ExternalExecutionRequest {
    pub const fn id(&self) -> ExternalRequestId {
        match self {
            Self::Action { id, .. } | Self::Oracle { id, .. } | Self::Storage { id, .. } => *id,
        }
    }
}

/// Host callbacks for one admitted external boundary.
///
/// Storage, actions, and oracles are separate callbacks so an implementation
/// cannot accidentally obtain action semantics by registering an oracle, or
/// bypass storage-chain execution through a host action.
pub trait ExternalBoundaryHost {
    fn storage(&mut self, request: ExternalRequestId) -> Result<Vec<bool>, ExternalExecutionError>;
    fn action(
        &mut self,
        request: ExternalRequestId,
        name: &str,
        args: &[bool],
    ) -> Result<Vec<bool>, ExternalExecutionError>;
    fn oracle(
        &mut self,
        request: ExternalRequestId,
        name: &str,
        args: &[bool],
    ) -> Result<Vec<bool>, ExternalExecutionError>;
}

/// Result values produced by an external boundary, keyed by request identity.
///
/// Values are clear only at this adapter seam. A strict transport must replace
/// them with result labels before it resumes a secret circuit segment.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ExternalBoundaryExecution {
    pub results: BTreeMap<ExternalRequestId, Vec<bool>>,
}

/// Fail-closed boundary executor error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternalExecutionError {
    DuplicateExecutionRequest(ExternalRequestId),
    MissingExecutionRequest(ExternalRequestId),
    UnexpectedExecutionKind(ExternalRequestId),
    UnsupportedPolicy(ExternalRequestId),
    InvalidGeometry(ExternalRequestId),
    ResultWidthMismatch(ExternalRequestId),
    PlannerInconsistency(ExternalRequestId),
    HostFailure,
}

/// Merge and execute one planner result in its exact batch order.
///
/// The currently executable profile is the explicitly marked evaluator +
/// `BothRoles` legacy mode. This is intentional: accepting other policy modes
/// here would falsely claim a label transport/reinsertion protocol exists for
/// them. Pure oracles must be `Assigned`; replicated-oracle admission remains
/// owned by a future consistency protocol.
pub fn execute_external_boundary_plan(
    plan: &ExternalBoundaryPlan,
    requests: &[ExternalRequest],
    executions: &[ExternalExecutionRequest],
    host: &mut dyn ExternalBoundaryHost,
) -> Result<ExternalBoundaryExecution, ExternalExecutionError> {
    let mut planned = BTreeMap::new();
    for request in requests {
        if planned.insert(request.id, request).is_some() {
            return Err(ExternalExecutionError::PlannerInconsistency(request.id));
        }
    }
    let mut by_id = BTreeMap::new();
    for execution in executions {
        if by_id.insert(execution.id(), execution).is_some() {
            return Err(ExternalExecutionError::DuplicateExecutionRequest(
                execution.id(),
            ));
        }
    }

    let mut output = ExternalBoundaryExecution::default();
    for batch in &plan.batches {
        execute_batch(batch, &planned, &by_id, host, &mut output)?;
    }
    // A CSE alias is never a runnable request. Its reinserted result must be
    // the representative result, not a second host invocation.
    for (&id, &representative) in &plan.representative {
        let result = output
            .results
            .get(&representative)
            .cloned()
            .ok_or(ExternalExecutionError::PlannerInconsistency(id))?;
        output.results.insert(id, result);
    }
    Ok(output)
}

fn execute_batch(
    batch: &ExternalBatch,
    planned: &BTreeMap<ExternalRequestId, &ExternalRequest>,
    executions: &BTreeMap<ExternalRequestId, &ExternalExecutionRequest>,
    host: &mut dyn ExternalBoundaryHost,
    output: &mut ExternalBoundaryExecution,
) -> Result<(), ExternalExecutionError> {
    for &id in &batch.requests {
        if output.results.contains_key(&id) {
            return Err(ExternalExecutionError::PlannerInconsistency(id));
        }
        let request = planned
            .get(&id)
            .copied()
            .ok_or(ExternalExecutionError::PlannerInconsistency(id))?;
        let execution = executions
            .get(&id)
            .copied()
            .ok_or(ExternalExecutionError::MissingExecutionRequest(id))?;
        let result = match (&request.kind, execution) {
            (
                ExternalRequestKind::Storage,
                ExternalExecutionRequest::Storage { output_bits, .. },
            ) => {
                let result = host.storage(id)?;
                exact_width(id, &result, *output_bits)?;
                result
            }
            (
                ExternalRequestKind::Action(policy),
                ExternalExecutionRequest::Action {
                    name,
                    guard,
                    args,
                    fallback,
                    ..
                },
            ) => {
                if policy.executor != ExternalExecutor::Evaluator
                    || policy.reveal != ExternalRevealPolicy::BothRoles
                {
                    return Err(ExternalExecutionError::UnsupportedPolicy(id));
                }
                // The planner action record has no output geometry because it
                // is protocol-neutral; action fallback is the canonical local
                // geometry source for the execution/reinsertion seam.
                if fallback.is_empty() {
                    return Err(ExternalExecutionError::InvalidGeometry(id));
                }
                let result = if *guard {
                    let result = host.action(id, name, args)?;
                    exact_width(id, &result, fallback.len())?;
                    result
                } else {
                    fallback.clone()
                };
                result
            }
            (
                ExternalRequestKind::Oracle(policy),
                ExternalExecutionRequest::Oracle {
                    name,
                    args,
                    output_bits,
                    ..
                },
            ) => {
                if policy.execution != OracleExecutionKind::Assigned
                    || policy.executor != ExternalExecutor::Evaluator
                    || policy.reveal != ExternalRevealPolicy::BothRoles
                {
                    return Err(ExternalExecutionError::UnsupportedPolicy(id));
                }
                if *output_bits == 0 {
                    return Err(ExternalExecutionError::InvalidGeometry(id));
                }
                let result = host.oracle(id, name, args)?;
                exact_width(id, &result, *output_bits)?;
                result
            }
            _ => return Err(ExternalExecutionError::UnexpectedExecutionKind(id)),
        };
        output.results.insert(id, result);
    }
    Ok(())
}

fn exact_width(
    id: ExternalRequestId,
    result: &[bool],
    expected: usize,
) -> Result<(), ExternalExecutionError> {
    if result.len() == expected {
        Ok(())
    } else {
        Err(ExternalExecutionError::ResultWidthMismatch(id))
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;
    use crate::external_boundary::{
        ExternalBatchLimits, ExternalRequestKind, plan_external_boundaries,
    };
    use volar_ir_common::{ActionExecutionPolicy, OracleExecutionPolicy};

    struct Host {
        log: Vec<&'static str>,
    }

    impl ExternalBoundaryHost for Host {
        fn storage(&mut self, _: ExternalRequestId) -> Result<Vec<bool>, ExternalExecutionError> {
            self.log.push("storage");
            Ok(vec![true])
        }

        fn action(
            &mut self,
            _: ExternalRequestId,
            _: &str,
            _: &[bool],
        ) -> Result<Vec<bool>, ExternalExecutionError> {
            self.log.push("action");
            Ok(vec![false])
        }

        fn oracle(
            &mut self,
            _: ExternalRequestId,
            _: &str,
            _: &[bool],
        ) -> Result<Vec<bool>, ExternalExecutionError> {
            self.log.push("oracle");
            Ok(vec![true])
        }
    }

    fn action_policy() -> ActionExecutionPolicy {
        ActionExecutionPolicy::legacy_evaluator()
    }

    #[test]
    fn merges_storage_actions_and_oracles_in_planned_batch_order() {
        let storage_id = ExternalRequestId(1);
        let action_id = ExternalRequestId(2);
        let oracle_id = ExternalRequestId(3);
        let requests = vec![
            ExternalRequest {
                id: storage_id,
                kind: ExternalRequestKind::Storage,
                depends_on: vec![],
                demanded: true,
                action_ordinal: None,
                oracle_equivalence: None,
            },
            ExternalRequest {
                id: action_id,
                kind: ExternalRequestKind::Action(action_policy()),
                depends_on: vec![],
                demanded: true,
                action_ordinal: Some(0),
                oracle_equivalence: None,
            },
            ExternalRequest {
                id: oracle_id,
                kind: ExternalRequestKind::Oracle(OracleExecutionPolicy::legacy_evaluator()),
                depends_on: vec![action_id],
                demanded: true,
                action_ordinal: None,
                oracle_equivalence: None,
            },
        ];
        let plan = plan_external_boundaries(&requests, ExternalBatchLimits::default()).unwrap();
        let executions = vec![
            ExternalExecutionRequest::Storage {
                id: storage_id,
                output_bits: 1,
            },
            ExternalExecutionRequest::Action {
                id: action_id,
                name: "action".into(),
                guard: true,
                args: vec![],
                fallback: vec![true],
            },
            ExternalExecutionRequest::Oracle {
                id: oracle_id,
                name: "oracle".into(),
                args: vec![],
                output_bits: 1,
            },
        ];
        let mut host = Host { log: vec![] };
        let result =
            execute_external_boundary_plan(&plan, &requests, &executions, &mut host).unwrap();
        assert_eq!(host.log, vec!["storage", "action", "oracle"]);
        assert_eq!(result.results[&storage_id], vec![true]);
        assert_eq!(result.results[&action_id], vec![false]);
        assert_eq!(result.results[&oracle_id], vec![true]);
    }

    #[test]
    fn guarded_off_action_uses_fallback_without_host_call() {
        let id = ExternalRequestId(1);
        let requests = vec![ExternalRequest {
            id,
            kind: ExternalRequestKind::Action(action_policy()),
            depends_on: vec![],
            demanded: true,
            action_ordinal: Some(0),
            oracle_equivalence: None,
        }];
        let plan = plan_external_boundaries(&requests, ExternalBatchLimits::default()).unwrap();
        let executions = vec![ExternalExecutionRequest::Action {
            id,
            name: "not_called".into(),
            guard: false,
            args: vec![true],
            fallback: vec![false],
        }];
        let mut host = Host { log: vec![] };
        let result =
            execute_external_boundary_plan(&plan, &requests, &executions, &mut host).unwrap();
        assert!(host.log.is_empty());
        assert_eq!(result.results[&id], vec![false]);
    }
}
