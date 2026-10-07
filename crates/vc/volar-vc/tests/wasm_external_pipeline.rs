//! WASM → VAFFLE → Volar IR → Boolar external metadata/pipeline fixture.
//!
//! This uses actual named WASM imports and explicit `WaffleImportConfig`
//! mappings. It therefore proves that imported-guest declarations reach the
//! same VC external registry boundary as structural LLVM imports, without
//! allowing a name-derived evaluator-host fallback.

use volar_ir_common::{
    ActionExecutionPolicy, ExternalExecutor, ExternalRevealPolicy, OracleExecutionKind,
    OracleExecutionPolicy,
};
use volar_ir_passes::lower_ir_to_boolar;
use volar_vaffle_target::{
    VaffleTarget, WaffleImportConfig, lower_vaffle_to_ir_owned, lower_waffle_module,
};
use volar_vc::{
    ActionExternalRegistration, ExternalBatchLimits, OracleExternalRegistration, VcExternalRegistry,
};

const EXTERNAL_WAT: &str = r#"(module
  (import "portal" "pure" (func $pure (param i32) (result i32)))
  (import "portal" "act" (func $act (param i32 i32 i32) (result i32)))
  (func $entry (export "entry") (param $x i32) (param $guard i32) (result i32)
    (local $oracle_result i32)
    (local.set $oracle_result (call $pure (local.get $x)))
    (call $act
      (local.get $guard)
      (local.get $oracle_result)
      (local.get $x))))"#;

#[test]
fn wasm_external_declarations_reach_vc_boundary_planner() {
    let bytes = wat::parse_str(EXTERNAL_WAT).expect("WAT assembles");
    let mut wasm = portal_pc_waffle_frontend::from_wasm_bytes(
        &bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("WASM parses");
    portal_pc_waffle_frontend::expand_all_funcs(&mut wasm).expect("WASM functions expand");

    let oracle = OracleExecutionPolicy {
        execution: OracleExecutionKind::Assigned,
        executor: ExternalExecutor::Evaluator,
        reveal: ExternalRevealPolicy::BothRoles,
        fingerprint: [0x51; 32],
    };
    let action = ActionExecutionPolicy {
        executor: ExternalExecutor::Evaluator,
        reveal: ExternalRevealPolicy::BothRoles,
        fingerprint: [0x52; 32],
    };
    let config = WaffleImportConfig::new()
        .with_oracle_execution("portal.pure", "pure", oracle)
        .with_action_execution("portal.act", "act", 1, action);
    let mut target = VaffleTarget::new();
    let errors = lower_waffle_module(&wasm, &mut target, &config);
    assert!(
        errors.is_empty(),
        "WASM external lowering errors: {errors:?}"
    );

    let (ir, types) = lower_vaffle_to_ir_owned(target.module);
    let ir = volar_ir_passes::unroll_ir_everything(&ir, &types).expect("finite IR unroll");
    let boolar = lower_ir_to_boolar(&ir, &types);

    let registry = VcExternalRegistry::new(
        [ActionExternalRegistration {
            name: "act".into(),
            output_bits: 32,
            execution: action,
        }],
        [OracleExternalRegistration {
            name: "pure".into(),
            output_bits: 32,
            execution: oracle,
        }],
    )
    .expect("explicit WASM declaration registry");
    let plan = registry
        .plan_boolar(&boolar, ExternalBatchLimits::default())
        .expect("Boolar external boundary plan");
    let planned_requests: Vec<_> = plan
        .plan
        .batches
        .iter()
        .flat_map(|batch| batch.requests.iter())
        .collect();
    assert!(
        planned_requests.len() >= 2,
        "oracle and action are both planned"
    );
    assert!(
        plan.projections.len() >= 2,
        "both external outputs are projected"
    );
}
