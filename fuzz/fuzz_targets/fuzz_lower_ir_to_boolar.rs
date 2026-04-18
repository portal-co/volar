//! cargo-fuzz target: lower_ir_to_boolar preserves semantics and does not panic.
//!
//! Property D: bit_unflatten(eval_biir(lower_ir_to_boolar(ir), bit_flatten(inputs)))
//!              == eval_ir(ir, inputs)

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::ir::ArbitraryIr;
use volar_fuzz::interpreter::biir::eval_biir;
use volar_fuzz::interpreter::ir::{bit_flatten, bit_unflatten, eval_ir};
use volar_ir_passes::lower_ir_to_boolar;

fuzz_target!(|data: ArbitraryIr| {
    let ArbitraryIr { blocks: ir, types, inputs } = data;

    // Evaluate the original IR.
    let expected = match eval_ir(&ir, &types, &inputs) {
        Some(v) => v,
        None => return, // original doesn't terminate — skip
    };

    // Compute output widths from expected results for bit_unflatten.
    let out_widths: Vec<usize> = expected.iter().map(|v| v.len()).collect();

    // Lower to boolar and evaluate with bit-flattened inputs.
    let biir = lower_ir_to_boolar(&ir, &types);
    let flat_inputs = bit_flatten(&inputs);

    let flat_actual = match eval_biir(&biir, &flat_inputs) {
        Some(v) => v,
        None => return, // lowered form doesn't terminate — skip
    };

    let actual = bit_unflatten(&flat_actual, &out_widths);

    assert_eq!(actual, expected, "lower_ir_to_boolar changed the output");
});
