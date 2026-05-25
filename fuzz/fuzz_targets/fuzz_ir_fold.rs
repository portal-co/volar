//! cargo-fuzz target: fold_ir_blocks preserves semantics and does not panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::ir::ArbitraryIr;
use volar_fuzz::interpreter::ir::eval_ir;
use volar_ir_opt::ir::fold_ir_blocks;

fuzz_target!(|data: ArbitraryIr| {
    let ArbitraryIr { mut blocks, types, inputs } = data;

    let expected = match eval_ir(&blocks, &types, &inputs) {
        Some(v) => v,
        None => return,
    };

    fold_ir_blocks(&mut blocks, &types);

    let actual = match eval_ir(&blocks, &types, &inputs) {
        Some(v) => v,
        None => return,
    };

    assert_eq!(actual, expected, "fold_ir_blocks changed the output");
});
