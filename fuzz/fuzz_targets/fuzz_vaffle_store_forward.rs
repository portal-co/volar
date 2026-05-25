//! cargo-fuzz target: store_forward_vaffle_module preserves semantics and does not panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::vaffle::ArbitraryVaffleExt;
use volar_fuzz::interpreter::vaffle::eval_vaffle;
use volar_ir_opt::store_forward::store_forward_vaffle_module;

fuzz_target!(|data: ArbitraryVaffleExt| {
    let ArbitraryVaffleExt { mut module, func_id, inputs } = data;

    let expected = match eval_vaffle(&module, func_id, &inputs) {
        Some(v) => v,
        None => return,
    };

    store_forward_vaffle_module(&mut module);

    let actual = match eval_vaffle(&module, func_id, &inputs) {
        Some(v) => v,
        None => return,
    };

    assert_eq!(actual, expected, "store_forward_vaffle_module changed the output");
});
