//! cargo-fuzz target: fold_vaffle_module preserves semantics and does not panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::vaffle::ArbitraryVaffle;
use volar_fuzz::interpreter::vaffle::eval_vaffle;
use volar_ir_opt::vaffle::fold_vaffle_module;

fuzz_target!(|data: ArbitraryVaffle| {
    let ArbitraryVaffle { mut module, func_id, inputs } = data;

    let expected = match eval_vaffle(&module, func_id, &inputs) {
        Some(v) => v,
        None => return,
    };

    fold_vaffle_module(&mut module);

    let actual = match eval_vaffle(&module, func_id, &inputs) {
        Some(v) => v,
        None => return,
    };

    assert_eq!(actual, expected, "fold_vaffle_module changed the output");
});
