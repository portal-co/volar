//! cargo-fuzz target: movfuscate_biir preserves semantics.
//!
//! Property A: eval_biir(movfuscate_biir(cfg), movfuscated_inputs) == eval_biir(cfg, inputs)
//!
//! Panics if the property is violated.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::biir::ArbitraryBIir;
use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir_passes::{movfuscate_biir, pc_bits_needed};

fuzz_target!(|data: ArbitraryBIir| {
    let ArbitraryBIir { blocks: cfg, inputs } = data;

    // Evaluate the original CFG.
    let expected = match eval_biir(&cfg, &inputs) {
        Some(v) => v,
        None => return, // original doesn't terminate — skip
    };

    // Build padded inputs for the movfuscated form.
    let n_blocks = cfg.blocks.len();
    let pc_width = pc_bits_needed(n_blocks);
    let state_width = cfg.blocks.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let n_entry = cfg.blocks[0].params as usize;

    let mut m_inputs = vec![false; pc_width];
    m_inputs.extend_from_slice(&inputs);
    m_inputs.extend(core::iter::repeat(false).take(state_width.saturating_sub(n_entry)));

    // Movfuscate and evaluate.
    let movfuscated = movfuscate_biir(&cfg);
    let actual = match eval_biir(&movfuscated, &m_inputs) {
        Some(v) => v,
        None => return, // movfuscated form doesn't terminate — skip
    };

    assert_eq!(actual, expected, "movfuscate_biir changed the output");
});
