//! cargo-fuzz target: fold_biir_blocks preserves semantics and does not panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::biir::ArbitraryBIir;
use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir_opt::biir::fold_biir_blocks;

fuzz_target!(|data: ArbitraryBIir| {
    let ArbitraryBIir { mut blocks, inputs } = data;

    let expected = match eval_biir(&blocks, &inputs) {
        Some(v) => v,
        None => return,
    };

    fold_biir_blocks(&mut blocks);

    let actual = match eval_biir(&blocks, &inputs) {
        Some(v) => v,
        None => return,
    };

    assert_eq!(actual, expected, "fold_biir_blocks changed the output");
});
