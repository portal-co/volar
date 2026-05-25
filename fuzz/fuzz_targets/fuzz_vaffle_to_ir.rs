//! cargo-fuzz target: lower_vaffle_to_ir preserves semantics and does not panic.
//!
//! Property J: eval_ir(lower_vaffle_to_ir(m)) == eval_vaffle(m)

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::vaffle::ArbitraryVaffle;
use volar_fuzz::interpreter::ir::eval_ir;
use volar_fuzz::interpreter::vaffle::eval_vaffle;
use volar_vaffle_target::lower_vaffle_to_ir;

fuzz_target!(|data: ArbitraryVaffle| {
    let ArbitraryVaffle { module, func_id, inputs } = data;

    // lower_vaffle_to_ir uses a CPS stack ABI where block 0 takes no params.
    // Semantics comparison is valid only for zero-param VAFFLE functions.
    if !inputs.is_empty() {
        return;
    }

    let vaffle_out = match eval_vaffle(&module, func_id, &inputs) {
        Some(v) => v,
        None => return,
    };

    let (ir, ir_types) = lower_vaffle_to_ir(&module);

    let ir_out = match eval_ir(&ir, &ir_types, &[]) {
        Some(v) => v,
        None => return,
    };

    let flat_vaffle: Vec<bool> = vaffle_out.into_iter().flatten().collect();
    let flat_ir: Vec<bool> = ir_out.into_iter().flatten().collect();

    assert_eq!(flat_ir, flat_vaffle, "lower_vaffle_to_ir changed the output");
});
