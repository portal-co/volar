//! Validated VC-facing registry for declared external MPC primitives.
//!
//! The registry is public compiler/planning metadata only. It does not install
//! a host callback or authorize a transport: strict execution remains
//! fail-closed until a matching executor/reveal adapter exists.

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir_common::{ActionExecutionPolicy, OracleExecutionPolicy};

use crate::external_boundary::{
    BoolarBoundaryError, BoolarExternalBoundaryPlan, ExternalBatchLimits,
    plan_boolar_external_boundaries,
};

/// Validated public declaration metadata for a side-effecting action.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActionExternalRegistration {
    pub name: String,
    pub output_bits: usize,
    pub execution: ActionExecutionPolicy,
}

/// Validated public declaration metadata for a deterministic pure oracle.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OracleExternalRegistration {
    pub name: String,
    pub output_bits: usize,
    pub execution: OracleExecutionPolicy,
}

/// Registry construction/validation failure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternalRegistryError {
    EmptyName,
    ZeroOutputWidth(String),
    DuplicateAction(String),
    DuplicateOracle(String),
    DuplicateFingerprint([u8; 32]),
    ActionOutputWidthMismatch {
        name: String,
        declared: usize,
        call: usize,
    },
}

/// One explicit VC external declaration registry.
///
/// Registration is keyed by textual declaration name because Boolar call forms
/// currently name declarations. Result/request bookkeeping continues to use
/// exact IR identities in the boundary planner; names never become wire keys.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct VcExternalRegistry {
    actions: BTreeMap<String, ActionExternalRegistration>,
    oracles: BTreeMap<String, OracleExternalRegistration>,
}

impl VcExternalRegistry {
    /// Construct a registry, rejecting ambiguous name/fingerprint admission.
    pub fn new(
        actions: impl IntoIterator<Item = ActionExternalRegistration>,
        oracles: impl IntoIterator<Item = OracleExternalRegistration>,
    ) -> Result<Self, ExternalRegistryError> {
        let mut registry = Self::default();
        let mut fingerprints = BTreeMap::<[u8; 32], ()>::new();
        for action in actions {
            validate_name_and_width(&action.name, action.output_bits)?;
            if registry.actions.contains_key(&action.name) {
                return Err(ExternalRegistryError::DuplicateAction(action.name));
            }
            // The explicit legacy evaluator compatibility constructor uses a
            // zero placeholder until frontend declarations carry a real
            // fingerprint. Do not make multiple migrated legacy declarations
            // impossible to register solely for sharing that sentinel.
            if action.execution.fingerprint != [0; 32]
                && fingerprints
                    .insert(action.execution.fingerprint, ())
                    .is_some()
            {
                return Err(ExternalRegistryError::DuplicateFingerprint(
                    action.execution.fingerprint,
                ));
            }
            registry.actions.insert(action.name.clone(), action);
        }
        for oracle in oracles {
            validate_name_and_width(&oracle.name, oracle.output_bits)?;
            if registry.oracles.contains_key(&oracle.name) {
                return Err(ExternalRegistryError::DuplicateOracle(oracle.name));
            }
            if oracle.execution.fingerprint != [0; 32]
                && fingerprints
                    .insert(oracle.execution.fingerprint, ())
                    .is_some()
            {
                return Err(ExternalRegistryError::DuplicateFingerprint(
                    oracle.execution.fingerprint,
                ));
            }
            registry.oracles.insert(oracle.name.clone(), oracle);
        }
        Ok(registry)
    }

    pub fn action(&self, name: &str) -> Option<&ActionExternalRegistration> {
        self.actions.get(name)
    }

    pub fn oracle(&self, name: &str) -> Option<&OracleExternalRegistration> {
        self.oracles.get(name)
    }

    /// Validate action result geometry before a fused circuit reaches schedule
    /// lowering. Oracle geometry is also rechecked by the boundary extractor,
    /// including direct `OracleBit` calls.
    pub fn validate_boolar<P: Clone>(
        &self,
        circuit: &BIrBlocks<P>,
    ) -> Result<(), ExternalRegistryError> {
        for block in &circuit.blocks {
            for statement in &block.stmts {
                if let BIrStmt::ActionCall { name, num_bits, .. } = &statement.kind
                    && let Some(action) = self.action(name)
                    && *num_bits != action.output_bits
                {
                    return Err(ExternalRegistryError::ActionOutputWidthMismatch {
                        name: name.clone(),
                        declared: action.output_bits,
                        call: *num_bits,
                    });
                }
            }
        }
        Ok(())
    }

    /// Plan a fused circuit using only this registry's explicit policies and
    /// geometries. Missing names remain a fail-closed planner error.
    pub fn plan_boolar<P: Clone>(
        &self,
        circuit: &BIrBlocks<P>,
        limits: ExternalBatchLimits,
    ) -> Result<BoolarExternalBoundaryPlan, RegistryPlanningError> {
        self.validate_boolar(circuit)?;
        let actions: Vec<_> = self
            .actions
            .values()
            .map(|entry| (entry.name.clone(), entry.execution))
            .collect();
        let oracles: Vec<_> = self
            .oracles
            .values()
            .map(|entry| (entry.name.clone(), entry.execution, entry.output_bits))
            .collect();
        plan_boolar_external_boundaries(circuit, &actions, &oracles, limits)
            .map_err(RegistryPlanningError::Boundary)
    }
}

fn validate_name_and_width(name: &str, output_bits: usize) -> Result<(), ExternalRegistryError> {
    if name.is_empty() {
        Err(ExternalRegistryError::EmptyName)
    } else if output_bits == 0 {
        Err(ExternalRegistryError::ZeroOutputWidth(name.into()))
    } else {
        Ok(())
    }
}

/// Error from combining VC declaration registry validation and boundary
/// planning. Callers should map this to VC abort rather than falling back to
/// evaluator hosting.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RegistryPlanningError {
    Registry(ExternalRegistryError),
    Boundary(BoolarBoundaryError),
}

impl From<ExternalRegistryError> for RegistryPlanningError {
    fn from(error: ExternalRegistryError) -> Self {
        Self::Registry(error)
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;
    use volar_ir::boolar::{BIrBlock, BIrTarget, BIrTerminator};
    use volar_ir::ir::{IRBlockTargetId, IRVarId};
    use volar_ir_common::{ExternalExecutor, ExternalRevealPolicy, Node};

    fn registry() -> VcExternalRegistry {
        VcExternalRegistry::new(
            [ActionExternalRegistration {
                name: "action".into(),
                output_bits: 1,
                execution: ActionExecutionPolicy {
                    executor: ExternalExecutor::Evaluator,
                    reveal: ExternalRevealPolicy::BothRoles,
                    fingerprint: [1; 32],
                },
            }],
            [OracleExternalRegistration {
                name: "oracle".into(),
                output_bits: 1,
                execution: OracleExecutionPolicy::legacy_evaluator(),
            }],
        )
        .unwrap()
    }

    #[test]
    fn compatibility_fingerprint_sentinel_does_not_block_distinct_legacy_declarations() {
        let actions = [
            ActionExternalRegistration {
                name: "one".into(),
                output_bits: 1,
                execution: ActionExecutionPolicy::legacy_evaluator(),
            },
            ActionExternalRegistration {
                name: "two".into(),
                output_bits: 1,
                execution: ActionExecutionPolicy::legacy_evaluator(),
            },
        ];
        assert!(VcExternalRegistry::new(actions, []).is_ok());
    }

    #[test]
    fn registry_drives_fail_closed_boolar_boundary_planning() {
        let circuit = BIrBlocks {
            blocks: vec![BIrBlock {
                params: 1,
                stmts: vec![
                    Node::new(
                        BIrStmt::ActionCall {
                            name: "action".into(),
                            guard: IRVarId(0),
                            args: vec![],
                            fallback: vec![IRVarId(0)],
                            num_bits: 1,
                        },
                        (),
                        None,
                    ),
                    Node::new(
                        BIrStmt::ActionBit {
                            call: IRVarId(1),
                            bit: 0,
                        },
                        (),
                        None,
                    ),
                ],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(2)],
                }),
            }],
            pre_init: vec![],
        };
        let planned = registry()
            .plan_boolar(&circuit, ExternalBatchLimits::default())
            .unwrap();
        assert_eq!(planned.plan.batches.len(), 1);
        assert_eq!(planned.plan.batches[0].requests.len(), 1);
    }

    #[test]
    fn action_geometry_mismatch_is_not_legacy_fallback() {
        let circuit = BIrBlocks {
            blocks: vec![BIrBlock {
                params: 1,
                stmts: vec![Node::new(
                    BIrStmt::ActionCall {
                        name: "action".into(),
                        guard: IRVarId(0),
                        args: vec![],
                        fallback: vec![IRVarId(0), IRVarId(0)],
                        num_bits: 2,
                    },
                    (),
                    None,
                )],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(0)],
                }),
            }],
            pre_init: vec![],
        };
        assert!(matches!(
            registry().plan_boolar(&circuit, ExternalBatchLimits::default()),
            Err(RegistryPlanningError::Registry(
                ExternalRegistryError::ActionOutputWidthMismatch { .. }
            ))
        ));
    }
}
