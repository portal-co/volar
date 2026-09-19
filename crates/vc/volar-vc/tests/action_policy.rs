//! Explicit executor policy follows action declarations into strict schedules.

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::{ActionExecutionPolicy, ExternalExecutor, ExternalRevealPolicy};
use volar_vc::schedule::compile_schedule_with_action_policies;

fn action_circuit() -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![
                Node::new(
                    BIrStmt::ActionCall {
                        name: "host_action".into(),
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
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            }),
        }],
        pre_init: vec![],
    }
}

#[test]
fn compiler_carries_explicit_action_executor_policy() {
    let policy = ActionExecutionPolicy {
        executor: ExternalExecutor::Garbler,
        reveal: ExternalRevealPolicy::ExecutorOnly,
        fingerprint: [0xA5; 32],
    };
    let schedule = compile_schedule_with_action_policies(
        &action_circuit(),
        &[("host_action".into(), policy)],
    )
    .unwrap();
    assert_eq!(schedule.actions.len(), 1);
    assert_eq!(schedule.actions[0].execution, policy);
}
