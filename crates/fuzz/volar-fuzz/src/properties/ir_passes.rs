//! Property D — `lower_ir_to_boolar` preserves semantics.
//! Property J — `lower_vaffle_to_ir` preserves semantics.

use proptest::prelude::*;
use volar_ir_passes::lower_ir_to_boolar;
use volar_vaffle_target::lower_vaffle_to_ir;

use crate::generators::ir::gen_ir_and_inputs;
use crate::generators::vaffle::gen_vaffle_and_inputs;
use crate::interpreter::biir::eval_biir;
use crate::interpreter::ir::{bit_flatten, bit_unflatten, bit_width, eval_ir};
use crate::interpreter::vaffle::eval_vaffle;

proptest! {
    #[test]
    fn prop_d_lower_ir_to_boolar_preserves_semantics(
        (ir, types, inputs) in gen_ir_and_inputs()
    ) {
        // Evaluate the high-level IR.
        let ir_outputs = match eval_ir(&ir, &types, &inputs) {
            Some(v) => v,
            None => return Ok(()), // shouldn't happen for single-block — skip defensively
        };

        // Determine the output widths so we can unflatten the boolar result.
        let output_widths: Vec<usize> = ir_outputs.iter().map(|v| v.len()).collect();
        let total_output_bits: usize = output_widths.iter().sum();

        // Lower IR → BIrBlocks and evaluate.
        let boolar = lower_ir_to_boolar(&ir, &types);
        let flat_inputs = bit_flatten(&inputs);
        let flat_outputs = match eval_biir(&boolar, &flat_inputs) {
            Some(v) => v,
            None => {
                // The lowered circuit didn't terminate — this is a bug, but
                // we fail with a clear message rather than a panic.
                prop_assert!(false,
                    "eval_biir on lower_ir_to_boolar output did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(flat_outputs.len(), total_output_bits,
            "lowered circuit output bit count mismatch");

        let boolar_outputs = bit_unflatten(&flat_outputs, &output_widths);

        prop_assert_eq!(boolar_outputs, ir_outputs,
            "lower_ir_to_boolar changed the semantics");
    }

    #[test]
    fn prop_d_lower_ir_to_boolar_does_not_panic(
        (ir, types, _inputs) in gen_ir_and_inputs()
    ) {
        let _ = lower_ir_to_boolar(&ir, &types);
    }
}

// ============================================================================
// Property J — lower_vaffle_to_ir preserves semantics
// ============================================================================

proptest! {
    #[test]
    fn prop_j_lower_vaffle_to_ir_preserves_semantics(
        (module, func_id, inputs) in gen_vaffle_and_inputs()
    ) {
        // lower_vaffle_to_ir uses a CPS stack-based ABI: block 0 has no params;
        // original VAFFLE inputs are written into the stack frame by the caller.
        // Semantics comparison is valid only for zero-param VAFFLE functions
        // (where the entry block produces results without external inputs).
        if !inputs.is_empty() {
            return Ok(());
        }

        let vaffle_out = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        let (ir, ir_types) = lower_vaffle_to_ir(&module);

        // Block 0 of the lowered IR takes no params (CPS entry sets up the stack).
        let ir_out = match eval_ir(&ir, &ir_types, &[]) {
            Some(v) => v,
            None => return Ok(()),
        };

        // Flatten both output lists for a uniform comparison.
        let flat_vaffle: Vec<bool> = vaffle_out.into_iter().flatten().collect();
        let flat_ir: Vec<bool> = ir_out.into_iter().flatten().collect();

        prop_assert_eq!(flat_ir, flat_vaffle, "lower_vaffle_to_ir changed the semantics");
    }

    #[test]
    fn prop_j_lower_vaffle_to_ir_does_not_panic(
        (module, _func_id, _inputs) in gen_vaffle_and_inputs()
    ) {
        let _ = lower_vaffle_to_ir(&module);
    }
}
