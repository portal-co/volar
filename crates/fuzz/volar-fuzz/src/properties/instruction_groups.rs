//! Property coverage for well-formed advisory instruction-group fixtures.
//!
//! These fixtures exercise declaration tables, typed captured inputs, and
//! per-node metadata without pretending an advisory group has an optimisation
//! consumer yet. Both interpreters intentionally ignore metadata, so evaluation
//! remains the ungrouped scalar reference behavior.

use proptest::prelude::*;
use vaffle::FuncDecl;
use volar_ir_common::InstructionGroupId;
use volar_vaffle_target::lower_vaffle_to_ir;

use crate::generators::ir::gen_ir_with_advisory_group_and_inputs;
use crate::generators::vaffle::gen_vaffle_with_advisory_group_and_inputs;
use crate::interpreter::ir::eval_ir;
use crate::interpreter::vaffle::eval_vaffle;

proptest! {
    #[test]
    fn advisory_ir_fixture_has_typed_instance_and_member_stack(
        (blocks, types, inputs) in gen_ir_with_advisory_group_and_inputs()
    ) {
        prop_assert_eq!(blocks.instruction_groups.len(), 1);
        prop_assert_eq!(blocks.instruction_group_instances.len(), 1);
        let instance = &blocks.instruction_group_instances[0];
        prop_assert_eq!(instance.id, InstructionGroupId(0));
        prop_assert_eq!(instance.decl.0, 0);
        for node in &blocks.blocks[0].stmts {
            prop_assert_eq!(node.instruction_groups().stack(), &[InstructionGroupId(0)]);
        }
        prop_assert!(eval_ir(&blocks, &types, &inputs).is_some());
    }

    #[test]
    fn advisory_vaffle_fixture_survives_lowering(
        (module, func_id, inputs) in gen_vaffle_with_advisory_group_and_inputs()
    ) {
        prop_assert_eq!(module.instruction_groups.len(), 1);
        let FuncDecl::Body(body) = &module.funcs[func_id.0] else {
            prop_assert!(false, "generator must produce a function body");
            return Ok(());
        };
        prop_assert_eq!(body.instruction_group_instances.len(), 1);
        for node in &body.values {
            if !matches!(node.kind, vaffle::Value::Param { .. }) {
                prop_assert_eq!(node.instruction_groups().stack(), &[InstructionGroupId(0)]);
            }
        }
        prop_assert!(eval_vaffle(&module, func_id, &inputs).is_some());

        let (ir, _types) = lower_vaffle_to_ir(&module);
        prop_assert_eq!(ir.instruction_groups, module.instruction_groups);
        prop_assert_eq!(ir.instruction_group_instances.len(), 1);
        prop_assert!(ir.blocks.iter().flat_map(|block| &block.stmts)
            .any(|node| node.instruction_groups().stack() == &[InstructionGroupId(0)]));
    }
}