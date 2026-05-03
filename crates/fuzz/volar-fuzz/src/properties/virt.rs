// @reliability: experimental
// @ai: assisted
//! Property — `virtualize_ir` and `virtualize_bir` preserve semantics
//! under Public dispatch.
//!
//! For generated single-block IR, virtualisation expands it into a
//! setup + dispatcher + handler layout.  Evaluating the original and
//! the virtualised module on the same inputs must yield the same
//! outputs.
//!
//! Oblivious dispatch is not fuzzed here because `movfuscate_ir` does
//! not yet support `IRTerminator::JumpTable` (see
//! `crates/ir/volar-ir-virt/tests/fhe_integration.rs` for the full
//! discussion).  The Public-dispatch path is the primary correctness
//! story for v1 and is exercised below.

use proptest::prelude::*;
use volar_ir_virt::{virtualize_bir, virtualize_ir, BytecodeForm, DispatchMode, VirtualizeConfig};

use crate::generators::ir::gen_ir_and_inputs;
use crate::interpreter::biir::eval_biir;
use crate::interpreter::ir::eval_ir;
use volar_ir::ir::IRBlocks;
use volar_ir_passes::lower_ir_to_boolar;

fn cfg_public_in_ir() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

proptest! {
    #[test]
    fn prop_virt_ir_preserves_semantics(
        (ir, types, inputs) in gen_ir_and_inputs()
    ) {
        // `gen_ir_and_inputs` produces single-block IR.  For single-block
        // inputs, `virtualize_ir` still composes correctly (1 handler +
        // setup + dispatcher = 3 blocks) and should be semantics-
        // preserving.
        let ir_out = match eval_ir(&ir, &types, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        let mut types_mut = types.clone();
        let virt = virtualize_ir::<()>(&ir, &mut types_mut, &cfg_public_in_ir());

        let virt_out = match eval_ir(&virt.blocks, &types_mut, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(
                    false,
                    "virtualize_ir output did not terminate under eval_ir"
                );
                return Ok(());
            }
        };

        prop_assert_eq!(
            virt_out, ir_out,
            "virtualize_ir (Public/InIr) changed the IR semantics"
        );
    }

    #[test]
    fn prop_virt_bir_preserves_semantics(
        (ir, types, inputs) in gen_ir_and_inputs()
    ) {
        // Route through lower_ir_to_boolar to obtain a BIR module, then
        // virtualise.
        use crate::interpreter::ir::bit_flatten;

        let ir_out = match eval_ir(&ir, &types, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };
        let bir = lower_ir_to_boolar(&ir, &types);
        let flat_inputs = bit_flatten(&inputs);

        // Sanity: the lowered BIR must itself be semantics-preserving
        // (property D).  If it doesn't evaluate, skip.
        let Some(_bir_out) = eval_biir(&bir, &flat_inputs) else {
            return Ok(());
        };

        let virt = virtualize_bir(&bir, &cfg_public_in_ir());

        let virt_out = match eval_biir(&virt.blocks, &flat_inputs) {
            Some(v) => v,
            None => {
                prop_assert!(
                    false,
                    "virtualize_bir output did not terminate under eval_biir"
                );
                return Ok(());
            }
        };

        // Compare against the lowered IR's flat output bits.
        let expected_bits: Vec<bool> = ir_out.into_iter().flatten().collect();
        prop_assert_eq!(
            virt_out,
            expected_bits,
            "virtualize_bir (Public/InIr) changed the BIR semantics"
        );
    }

    #[test]
    fn prop_virt_ir_does_not_panic(
        (ir, types, _inputs) in gen_ir_and_inputs()
    ) {
        let mut types_mut = types.clone();
        let _ = virtualize_ir::<()>(&ir, &mut types_mut, &cfg_public_in_ir());
    }

    #[test]
    fn prop_virt_bir_does_not_panic(
        (ir, types, _inputs) in gen_ir_and_inputs()
    ) {
        let bir: volar_ir::boolar::BIrBlocks = lower_ir_to_boolar(&ir, &types);
        let _ = virtualize_bir(&bir, &cfg_public_in_ir());
    }
}

// Suppress an unused-import warning when the generator module changes.
#[allow(dead_code)]
fn _keep_irblocks_alive() -> Option<IRBlocks<()>> { None }
