//! LLVM → VAFFLE → Volar IR → Boolar external metadata/pipeline fixture.

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use volar_ir_common::{
    ActionExecutionPolicy, ExternalExecutor, ExternalRevealPolicy, OracleExecutionKind,
    OracleExecutionPolicy,
};
use volar_ir_passes::lower_ir_to_boolar;
use volar_llvm_vaffle_import::{LlvmImportConfig, import_module_with_config};
use volar_vaffle_target::lower_vaffle_to_ir_owned;
use volar_vc::{
    ActionExternalRegistration, ExternalBatchLimits, OracleExternalRegistration, VcExternalRegistry,
};

#[test]
fn llvm_external_declarations_reach_vc_boundary_planner() {
    let source = r#"
declare i32 @pure(i32)
declare i32 @act(i1, i32, i32)
define i32 @entry(i32 %x, i1 %guard) {
entry:
  %o = call i32 @pure(i32 %x)
  %a = call i32 @act(i1 %guard, i32 %o, i32 %x)
  ret i32 %a
}
"#;
    let context = Context::create();
    let llvm = context
        .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
            source.as_bytes(),
            "vc-llvm-external.ll",
        ))
        .expect("valid LLVM fixture");
    let oracle = OracleExecutionPolicy {
        execution: OracleExecutionKind::Assigned,
        executor: ExternalExecutor::Evaluator,
        reveal: ExternalRevealPolicy::BothRoles,
        fingerprint: [0x41; 32],
    };
    let action = ActionExecutionPolicy {
        executor: ExternalExecutor::Evaluator,
        reveal: ExternalRevealPolicy::BothRoles,
        fingerprint: [0x42; 32],
    };
    let module = import_module_with_config(
        &llvm,
        &["entry"],
        LlvmImportConfig::default()
            .with_oracle_execution("pure", oracle)
            .with_action_execution("act", 1, action),
    )
    .expect("LLVM external calls import");
    let (ir, types) = lower_vaffle_to_ir_owned(module);
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
    .expect("explicit LLVM declaration registry");
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
