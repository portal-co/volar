//! Properties A, B, C — testing `movfuscate_biir` and `lower_to_circuit`.
//!
//! ## Property A — movfuscate preserves semantics
//! `eval_biir(movfuscate_biir(cfg), movfuscated_inputs) == eval_biir(cfg, inputs)`
//!
//! ## Property B — lower_to_circuit preserves semantics
//! `eval_biir(lower_to_circuit(movfuscate_biir(cfg), limit), movfuscated_inputs)
//!     == eval_biir(movfuscate_biir(cfg), movfuscated_inputs)`
//! (skipped when the movfuscated circuit doesn't terminate within `limit`)
//!
//! ## Property C — passes do not panic on valid inputs
//! `movfuscate_biir` and `lower_to_circuit` complete without panicking.

use proptest::prelude::*;
use volar_ir_passes::lower_to_circuit::lower_to_circuit;
use volar_ir_passes::{movfuscate_biir, pc_bits_needed, LoweringMode};

use crate::generators::biir::gen_biir_and_inputs;
use crate::interpreter::biir::{eval_biir, eval_biir_with_limit};

/// Unroll limit used for `lower_to_circuit` in property B.
/// A DAG with ≤ 3 blocks, movfuscated, traverses at most ~3 loop iterations,
/// so 16 gives ample headroom.
const LOWER_LIMIT: u32 = 16;

/// Build the input vector for the movfuscated form of a CFG.
///
/// The movfuscated block's params are:
/// ```text
/// [pc_bit_0 … pc_bit_{k-1}, state_0 … state_{w-1}]
/// ```
/// where `k = pc_bits_needed(n_blocks)` and `w = max(block.params)`.
///
/// Initial call → PC = 0 (all false), state = original entry inputs padded
/// with false on the right.
fn movfuscated_inputs(cfg: &volar_ir::boolar::BIrBlocks<()>, entry_inputs: &[bool]) -> Vec<bool> {
    let n_blocks = cfg.blocks.len();
    let pc_width = pc_bits_needed(n_blocks);
    let state_width = cfg.blocks.iter().map(|b| b.params as usize).max().unwrap_or(0);
    let n_entry = cfg.blocks[0].params as usize;

    let mut v = vec![false; pc_width];
    v.extend_from_slice(entry_inputs);
    v.extend(core::iter::repeat(false).take(state_width.saturating_sub(n_entry)));
    v
}

proptest! {
    // ── Property A ──────────────────────────────────────────────────────────

    #[test]
    fn prop_a_movfuscate_preserves_semantics(
        (cfg, inputs) in gen_biir_and_inputs()
    ) {
        // Evaluate the original CFG.
        let expected = match eval_biir(&cfg, &inputs) {
            Some(v) => v,
            None => return Ok(()), // original doesn't terminate — skip
        };

        // Movfuscate and evaluate with correctly padded inputs.
        let movfuscated = movfuscate_biir(&cfg);
        let m_inputs = movfuscated_inputs(&cfg, &inputs);
        let actual = match eval_biir(&movfuscated, &m_inputs) {
            Some(v) => v,
            None => return Ok(()), // movfuscated form doesn't terminate — skip
        };

        prop_assert_eq!(actual, expected,
            "movfuscate_biir changed the output");
    }

    // ── Property B ──────────────────────────────────────────────────────────

    #[test]
    fn prop_b_lower_to_circuit_preserves_semantics(
        (cfg, inputs) in gen_biir_and_inputs()
    ) {
        let movfuscated = movfuscate_biir(&cfg);
        let m_inputs = movfuscated_inputs(&cfg, &inputs);

        // Evaluate the movfuscated form with the same loop limit we'll use
        // for lower_to_circuit, so we only assert when we know the circuit
        // will pick the right MUX output.
        let expected =
            match eval_biir_with_limit(&movfuscated, &m_inputs, LOWER_LIMIT as usize) {
                Some(v) => v,
                None => return Ok(()), // doesn't terminate within limit — skip
            };

        let circuit =
            lower_to_circuit(&movfuscated, LOWER_LIMIT, LoweringMode::Unconditional);
        let actual = eval_biir(&circuit, &m_inputs)
            .expect("lowered circuit (DAG) must always terminate");

        prop_assert_eq!(actual, expected,
            "lower_to_circuit changed the output");
    }

    // ── Property C ──────────────────────────────────────────────────────────

    #[test]
    fn prop_c_movfuscate_does_not_panic(
        (cfg, _inputs) in gen_biir_and_inputs()
    ) {
        let _ = movfuscate_biir(&cfg);
    }

    #[test]
    fn prop_c_lower_to_circuit_does_not_panic(
        (cfg, _inputs) in gen_biir_and_inputs()
    ) {
        let movfuscated = movfuscate_biir(&cfg);
        let _ = lower_to_circuit(&movfuscated, LOWER_LIMIT, LoweringMode::Unconditional);
        let _ = lower_to_circuit(&movfuscated, LOWER_LIMIT, LoweringMode::WithTerminationFlag);
    }
}
