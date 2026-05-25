//! cargo-fuzz target: store_forward_ir_blocks preserves semantics and does not panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::ir::ArbitraryIrExtended;
use volar_fuzz::interpreter::ir::eval_ir;
use volar_ir_opt::store_forward::store_forward_ir_blocks;

fuzz_target!(|data: ArbitraryIrExtended| {
    let ArbitraryIrExtended { mut blocks, types, inputs } = data;

    let expected = match eval_ir(&blocks, &types, &inputs) {
        Some(v) => v,
        None => return,
    };

    store_forward_ir_blocks(&mut blocks, &types);

    let actual = match eval_ir(&blocks, &types, &inputs) {
        Some(v) => v,
        None => return,
    };

    assert_eq!(actual, expected, "store_forward_ir_blocks changed the output");
});
