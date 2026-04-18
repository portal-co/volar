//! cargo-fuzz target: lower_to_circuit preserves semantics and does not panic.
//!
//! Property B: eval_biir(lower_to_circuit(movfuscate_biir(cfg), limit), movfuscated_inputs)
//!              == eval_biir(movfuscate_biir(cfg), movfuscated_inputs)
//! (skipped when the movfuscated circuit doesn't terminate within limit)
//!
//! Also verifies Property C (no panic) for lower_to_circuit.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::biir::ArbitraryBIir;
use volar_fuzz::interpreter::biir::{eval_biir, eval_biir_with_limit};
use volar_ir_passes::lower_to_circuit::lower_to_circuit;
use volar_ir_passes::{movfuscate_biir, pc_bits_needed, LoweringMode};

/// Unroll limit — ample for small generated CFGs.
const LOWER_LIMIT: u32 = 16;

fuzz_target!(|data: ArbitraryBIir| {
    let ArbitraryBIir { blocks: cfg, inputs } = data;

    // Build padded inputs for the movfuscated form.
    let n_blocks = cfg.0.len();
    let pc_width = pc_bits_needed(n_blocks);
    let state_width = cfg.0.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let n_entry = cfg.0[0].params as usize;

    let mut m_inputs = vec![false; pc_width];
    m_inputs.extend_from_slice(&inputs);
    m_inputs.extend(core::iter::repeat(false).take(state_width.saturating_sub(n_entry)));

    let movfuscated = movfuscate_biir(&cfg);

    // Property C: lower_to_circuit must not panic.
    let circuit_unc =
        lower_to_circuit(&movfuscated, LOWER_LIMIT, LoweringMode::Unconditional);
    let _circuit_flag =
        lower_to_circuit(&movfuscated, LOWER_LIMIT, LoweringMode::WithTerminationFlag);

    // Property B: semantics are preserved.
    let expected =
        match eval_biir_with_limit(&movfuscated, &m_inputs, LOWER_LIMIT as usize) {
            Some(v) => v,
            None => return, // doesn't terminate within limit — skip
        };

    let actual = eval_biir(&circuit_unc, &m_inputs)
        .expect("lowered circuit (DAG) must always terminate");

    assert_eq!(actual, expected, "lower_to_circuit changed the output");
});
