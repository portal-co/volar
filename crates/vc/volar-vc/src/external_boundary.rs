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
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
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
    /// Source action-chain occurrence. Required for actions and ignored for
    /// pure oracles/storage. It is intentionally separate from `id`: a stable
    /// request identifier is not necessarily lexical side-effect order.
    pub action_ordinal: Option<u64>,
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

/// Exact source projection that will receive one request result bit.
///
/// This is public planning metadata only. A strict/VC adapter must still bind
/// it to authenticated session/circuit/boundary material before reinsertion.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExternalResultProjection {
    pub request: ExternalRequestId,
    pub bit: usize,
}

/// Boundary plan extracted from one fused Boolar circuit.
///
/// `projections` is keyed by the exact `IRVarId` produced by an external
/// projection/read. It is intentionally not keyed by source spelling: the
/// latter is not stable enough for result reinsertion bookkeeping.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoolarExternalBoundaryPlan {
    pub plan: ExternalBoundaryPlan,
    pub projections: BTreeMap<IRVarId, ExternalResultProjection>,
}

/// Fail-closed errors while extracting external requests from fused Boolar.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolarBoundaryError {
    NotACircuit,
    MissingActionPolicy(String),
    MissingOraclePolicy(String),
    OracleOutputWidthMismatch {
        name: String,
        declared: usize,
        call: usize,
    },
    ActionOutputWidthMismatch {
        request: ExternalRequestId,
    },
    InvalidOutputBit {
        request: ExternalRequestId,
        bit: usize,
    },
    InconsistentOracleOccurrence(u64),
    Planner(ExternalBoundaryError),
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
    MissingActionOrdinal(ExternalRequestId),
    DuplicateActionOrdinal(u64),
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
    let mut action_ordinals = BTreeSet::new();
    for request in requests {
        if matches!(request.kind, ExternalRequestKind::Action(_)) {
            let ordinal = request
                .action_ordinal
                .ok_or(ExternalBoundaryError::MissingActionOrdinal(request.id))?;
            if !action_ordinals.insert(ordinal) {
                return Err(ExternalBoundaryError::DuplicateActionOrdinal(ordinal));
            }
        }
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
    let mut actions: Vec<_> = runnable
        .iter()
        .copied()
        .filter(|id| matches!(by_id[id].kind, ExternalRequestKind::Action(_)))
        .collect();
    actions.sort_by_key(|id| by_id[id].action_ordinal.expect("validated action ordinal"));
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

/// Extract and plan external requests from a single fused Boolar circuit.
///
/// Callers must supply policy registries from validated declarations. Unlike
/// the legacy schedule compiler, this function never invents an evaluator
/// policy for a missing declaration. It conservatively leaves
/// `oracle_equivalence` unset: a source adapter needs a declaration/profile
/// aware canonical argument encoding before it may authorize cross-occurrence
/// oracle CSE.
///
/// Storage requests are linked in their source storage-chain order. Action and
/// storage receive no artificial cross-chain edge; ordinary value dependencies
/// remain explicit. The returned projection map provides the exact source wire
/// identity a later reinsertion adapter must satisfy.
pub fn plan_boolar_external_boundaries<P: Clone>(
    circuit: &BIrBlocks<P>,
    action_policies: &[(String, ActionExecutionPolicy)],
    oracle_policies: &[(String, OracleExecutionPolicy, usize)],
    limits: ExternalBatchLimits,
) -> Result<BoolarExternalBoundaryPlan, BoolarBoundaryError> {
    if !circuit.is_circuit() || circuit.blocks.len() != 1 {
        return Err(BoolarBoundaryError::NotACircuit);
    }
    let block = &circuit.blocks[0];
    let BIrTerminator::Jmp(target) = &block.terminator else {
        return Err(BoolarBoundaryError::NotACircuit);
    };
    if target.block != IRBlockTargetId::Return {
        return Err(BoolarBoundaryError::NotACircuit);
    }

    let raw_id = |ordinal: usize| IRVarId((block.params as usize + ordinal) as u32);
    let request_id = |ordinal: usize| ExternalRequestId(raw_id(ordinal).0 as u64);
    let mut requests = Vec::<ExternalRequest>::new();
    let mut request_index = BTreeMap::<ExternalRequestId, usize>::new();
    let mut projections = BTreeMap::<IRVarId, ExternalResultProjection>::new();
    let mut call_requests = BTreeMap::<IRVarId, (ExternalRequestId, usize)>::new();
    let mut direct_oracles = BTreeMap::<
        u64,
        (
            ExternalRequestId,
            String,
            Vec<IRVarId>,
            OracleExecutionPolicy,
        ),
    >::new();
    let mut next_action_ordinal = 0u64;
    let mut previous_storage = None;

    let dependencies_for =
        |vars: &[IRVarId], projections: &BTreeMap<IRVarId, ExternalResultProjection>| {
            vars.iter()
                .filter_map(|var| projections.get(var).map(|projection| projection.request))
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
        };
    let action_policy = |name: &str| {
        action_policies
            .iter()
            .find(|(candidate, _)| candidate == name)
            .map(|(_, policy)| *policy)
            .ok_or_else(|| BoolarBoundaryError::MissingActionPolicy(name.into()))
    };
    let oracle_policy = |name: &str| {
        oracle_policies
            .iter()
            .find(|(candidate, _, _)| candidate == name)
            .map(|(_, policy, output_bits)| (*policy, *output_bits))
            .ok_or_else(|| BoolarBoundaryError::MissingOraclePolicy(name.into()))
    };

    for (ordinal, statement) in block.stmts.iter().enumerate() {
        let produced = raw_id(ordinal);
        let id = request_id(ordinal);
        match &statement.kind {
            BIrStmt::ActionCall {
                name,
                guard,
                args,
                fallback,
                num_bits,
            } => {
                if *num_bits == 0 || fallback.len() != *num_bits {
                    return Err(BoolarBoundaryError::ActionOutputWidthMismatch { request: id });
                }
                let policy = action_policy(name)?;
                let mut inputs = Vec::with_capacity(1 + args.len() + fallback.len());
                inputs.push(*guard);
                inputs.extend(args.iter().copied());
                inputs.extend(fallback.iter().copied());
                let dependencies = dependencies_for(&inputs, &projections);
                requests.push(ExternalRequest {
                    id,
                    kind: ExternalRequestKind::Action(policy),
                    depends_on: dependencies,
                    demanded: true,
                    action_ordinal: Some(next_action_ordinal),
                    oracle_equivalence: None,
                });
                request_index.insert(id, requests.len() - 1);
                call_requests.insert(produced, (id, *num_bits));
                next_action_ordinal += 1;
            }
            BIrStmt::ActionBit { call, bit } => {
                let Some(&(call_id, num_bits)) = call_requests.get(call) else {
                    return Err(BoolarBoundaryError::InvalidOutputBit {
                        request: id,
                        bit: *bit,
                    });
                };
                if *bit >= num_bits {
                    return Err(BoolarBoundaryError::InvalidOutputBit {
                        request: call_id,
                        bit: *bit,
                    });
                }
                projections.insert(
                    produced,
                    ExternalResultProjection {
                        request: call_id,
                        bit: *bit,
                    },
                );
            }
            BIrStmt::OracleCall {
                name,
                args,
                num_bits,
            } => {
                let (policy, declared_output_bits) = oracle_policy(name)?;
                if *num_bits != declared_output_bits {
                    return Err(BoolarBoundaryError::OracleOutputWidthMismatch {
                        name: name.clone(),
                        declared: declared_output_bits,
                        call: *num_bits,
                    });
                }
                let dependencies = dependencies_for(args, &projections);
                requests.push(ExternalRequest {
                    id,
                    kind: ExternalRequestKind::Oracle(policy),
                    depends_on: dependencies,
                    demanded: false,
                    action_ordinal: None,
                    oracle_equivalence: None,
                });
                request_index.insert(id, requests.len() - 1);
                call_requests.insert(produced, (id, *num_bits));
            }
            BIrStmt::OracleProjectedBit { call, bit } => {
                let Some(&(call_id, num_bits)) = call_requests.get(call) else {
                    return Err(BoolarBoundaryError::InvalidOutputBit {
                        request: id,
                        bit: *bit,
                    });
                };
                if *bit >= num_bits {
                    return Err(BoolarBoundaryError::InvalidOutputBit {
                        request: call_id,
                        bit: *bit,
                    });
                }
                projections.insert(
                    produced,
                    ExternalResultProjection {
                        request: call_id,
                        bit: *bit,
                    },
                );
            }
            BIrStmt::OracleBit {
                name,
                args,
                bit,
                occurrence,
            } => {
                let (policy, declared_output_bits) = oracle_policy(name)?;
                if *bit >= declared_output_bits {
                    return Err(BoolarBoundaryError::InvalidOutputBit {
                        request: id,
                        bit: *bit,
                    });
                }
                let call_id = match direct_oracles.get(occurrence) {
                    Some((existing, existing_name, existing_args, existing_policy)) => {
                        if existing_name != name
                            || existing_args != args
                            || existing_policy != &policy
                        {
                            return Err(BoolarBoundaryError::InconsistentOracleOccurrence(
                                *occurrence,
                            ));
                        }
                        *existing
                    }
                    None => {
                        let dependencies = dependencies_for(args, &projections);
                        requests.push(ExternalRequest {
                            id,
                            kind: ExternalRequestKind::Oracle(policy),
                            depends_on: dependencies,
                            demanded: false,
                            action_ordinal: None,
                            oracle_equivalence: None,
                        });
                        request_index.insert(id, requests.len() - 1);
                        direct_oracles
                            .insert(*occurrence, (id, name.clone(), args.clone(), policy));
                        id
                    }
                };
                projections.insert(
                    produced,
                    ExternalResultProjection {
                        request: call_id,
                        bit: *bit,
                    },
                );
            }
            BIrStmt::StorageRead { addr, .. } => {
                let mut dependencies = dependencies_for(addr, &projections);
                if let Some(previous) = previous_storage {
                    dependencies.push(previous);
                }
                dependencies.sort();
                dependencies.dedup();
                requests.push(ExternalRequest {
                    id,
                    kind: ExternalRequestKind::Storage,
                    depends_on: dependencies,
                    demanded: true,
                    action_ordinal: None,
                    oracle_equivalence: None,
                });
                request_index.insert(id, requests.len() - 1);
                previous_storage = Some(id);
                projections.insert(
                    produced,
                    ExternalResultProjection {
                        request: id,
                        bit: 0,
                    },
                );
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                let mut inputs = addr.clone();
                inputs.push(*src);
                let mut dependencies = dependencies_for(&inputs, &projections);
                if let Some(previous) = previous_storage {
                    dependencies.push(previous);
                }
                dependencies.sort();
                dependencies.dedup();
                requests.push(ExternalRequest {
                    id,
                    kind: ExternalRequestKind::Storage,
                    depends_on: dependencies,
                    demanded: true,
                    action_ordinal: None,
                    oracle_equivalence: None,
                });
                request_index.insert(id, requests.len() - 1);
                previous_storage = Some(id);
            }
            _ => {}
        }
    }

    // A projection used by an ordinary boolean gate, an external argument, or
    // the returned circuit result is a real demand. Mark it before computing
    // transitive oracle demand through request dependency edges.
    let mut demanded = BTreeSet::new();
    for statement in &block.stmts {
        for var in boolar_statement_inputs(&statement.kind) {
            if let Some(projection) = projections.get(&var) {
                demanded.insert(projection.request);
            }
        }
    }
    for &var in &target.args {
        if let Some(projection) = projections.get(&var) {
            demanded.insert(projection.request);
        }
    }
    loop {
        let mut changed = false;
        for request in &requests {
            let live = request.demanded || demanded.contains(&request.id);
            if live {
                for dependency in &request.depends_on {
                    if let Some(&index) = request_index.get(dependency)
                        && matches!(requests[index].kind, ExternalRequestKind::Oracle(_))
                        && demanded.insert(*dependency)
                    {
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    for request in &mut requests {
        if matches!(request.kind, ExternalRequestKind::Oracle(_)) {
            request.demanded = demanded.contains(&request.id);
        }
    }

    let plan = plan_external_boundaries(&requests, limits).map_err(BoolarBoundaryError::Planner)?;
    Ok(BoolarExternalBoundaryPlan { plan, projections })
}

fn boolar_statement_inputs(statement: &BIrStmt) -> Vec<IRVarId> {
    match statement {
        BIrStmt::And(left, right) | BIrStmt::Or(left, right) | BIrStmt::Xor(left, right) => {
            vec![*left, *right]
        }
        BIrStmt::Not(value) => vec![*value],
        BIrStmt::OracleCall { args, .. } | BIrStmt::OracleBit { args, .. } => args.clone(),
        BIrStmt::ActionCall {
            guard,
            args,
            fallback,
            ..
        } => {
            let mut inputs = Vec::with_capacity(1 + args.len() + fallback.len());
            inputs.push(*guard);
            inputs.extend(args.iter().copied());
            inputs.extend(fallback.iter().copied());
            inputs
        }
        BIrStmt::StorageRead { addr, .. } => addr.clone(),
        BIrStmt::StorageWrite { src, addr, .. } => {
            let mut inputs = addr.clone();
            inputs.push(*src);
            inputs
        }
        _ => Vec::new(),
    }
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
    use volar_ir::boolar::{BIrBlock, BIrTarget};
    use volar_ir_common::{ExternalExecutor, ExternalRevealPolicy, Node};

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
            action_ordinal: Some(id),
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
            action_ordinal: None,
            oracle_equivalence: Some([key; 32]),
        }
    }

    fn storage(id: u64, deps: Vec<ExternalRequestId>) -> ExternalRequest {
        ExternalRequest {
            id: ExternalRequestId(id),
            kind: ExternalRequestKind::Storage,
            depends_on: deps,
            demanded: true,
            action_ordinal: None,
            oracle_equivalence: None,
        }
    }

    #[test]
    fn boolar_extraction_tracks_exact_projections_and_value_dependencies() {
        let circuit = BIrBlocks {
            blocks: vec![BIrBlock {
                params: 2,
                stmts: vec![
                    Node::new(
                        BIrStmt::ActionCall {
                            name: "act".into(),
                            guard: IRVarId(0),
                            args: vec![IRVarId(1)],
                            fallback: vec![IRVarId(1)],
                            num_bits: 1,
                        },
                        (),
                        None,
                    ),
                    Node::new(
                        BIrStmt::ActionBit {
                            call: IRVarId(2),
                            bit: 0,
                        },
                        (),
                        None,
                    ),
                    Node::new(
                        BIrStmt::OracleBit {
                            name: "pure".into(),
                            args: vec![IRVarId(3)],
                            bit: 0,
                            occurrence: 7,
                        },
                        (),
                        None,
                    ),
                    Node::new(BIrStmt::Xor(IRVarId(3), IRVarId(4)), (), None),
                ],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(5)],
                }),
            }],
            pre_init: vec![],
        };
        let actions = [("act".into(), ActionExecutionPolicy::legacy_evaluator())];
        let oracles = [("pure".into(), OracleExecutionPolicy::legacy_evaluator(), 1)];
        let extracted = plan_boolar_external_boundaries(
            &circuit,
            &actions,
            &oracles,
            ExternalBatchLimits::default(),
        )
        .unwrap();
        assert_eq!(
            extracted.plan.batches,
            vec![
                ExternalBatch {
                    requests: vec![ExternalRequestId(2)]
                },
                ExternalBatch {
                    requests: vec![ExternalRequestId(4)]
                },
            ]
        );
        assert_eq!(
            extracted.projections[&IRVarId(3)],
            ExternalResultProjection {
                request: ExternalRequestId(2),
                bit: 0,
            }
        );
        assert_eq!(
            extracted.projections[&IRVarId(4)],
            ExternalResultProjection {
                request: ExternalRequestId(4),
                bit: 0,
            }
        );
    }

    #[test]
    fn boolar_extraction_rejects_unvalidated_external_geometry() {
        let circuit = BIrBlocks {
            blocks: vec![BIrBlock {
                params: 1,
                stmts: vec![Node::new(
                    BIrStmt::OracleBit {
                        name: "pure".into(),
                        args: vec![IRVarId(0)],
                        bit: 1,
                        occurrence: 0,
                    },
                    (),
                    None,
                )],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(1)],
                }),
            }],
            pre_init: vec![],
        };
        let error = plan_boolar_external_boundaries(
            &circuit,
            &[],
            &[("pure".into(), OracleExecutionPolicy::legacy_evaluator(), 1)],
            ExternalBatchLimits::default(),
        )
        .unwrap_err();
        assert_eq!(
            error,
            BoolarBoundaryError::InvalidOutputBit {
                request: ExternalRequestId(1),
                bit: 1,
            }
        );
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
