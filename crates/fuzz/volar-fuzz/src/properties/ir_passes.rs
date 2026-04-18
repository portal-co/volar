//! Property D — `lower_ir_to_boolar` preserves semantics.
//!
//! `bit_unflatten(eval_biir(lower_ir_to_boolar(ir), bit_flatten(inputs)), output_widths)
//!     == eval_ir(ir, inputs)`

use proptest::prelude::*;
use volar_ir_passes::lower_ir_to_boolar;

use crate::generators::ir::gen_ir_and_inputs;
use crate::interpreter::biir::eval_biir;
use crate::interpreter::ir::{bit_flatten, bit_unflatten, bit_width, eval_ir};

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
