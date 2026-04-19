//! Properties E, F, G, H — optimization passes preserve semantics.
//!
//! | Property | Pass                          | IR layer   |
//! |----------|-------------------------------|------------|
//! | E        | `fold_ir_blocks`              | Volar IR   |
//! | F        | `fold_biir_blocks`            | Boolar IR  |
//! | G        | `fold_vaffle_module`          | VAFFLE     |
//! | H        | `store_forward_*`             | all layers |

use proptest::prelude::*;
use volar_ir_opt::biir::fold_biir_blocks;
use volar_ir_opt::ir::fold_ir_blocks;
use volar_ir_opt::vaffle::fold_vaffle_module;
use volar_ir_opt::store_forward::{
    store_forward_ir_blocks,
    store_forward_vaffle_module,
};

use crate::generators::biir::gen_biir_and_inputs;
use crate::generators::ir::{gen_ir_and_inputs, gen_ir_extended_and_inputs};
use crate::generators::vaffle::{gen_vaffle_and_inputs, gen_vaffle_extended_and_inputs, gen_vaffle_multiblock_and_inputs};
use crate::interpreter::biir::eval_biir;
use crate::interpreter::ir::eval_ir;
use crate::interpreter::vaffle::eval_vaffle;

// ============================================================================
// Property E — fold_ir_blocks preserves Volar IR semantics
// ============================================================================

proptest! {
    #[test]
    fn prop_e_fold_ir_blocks_preserves_semantics(
        (ir, types, inputs) in gen_ir_and_inputs()
    ) {
        let before = match eval_ir(&ir, &types, &inputs) {
            Some(v) => v,
            None => return Ok(()), // single-block shouldn't loop — skip defensively
        };

        let mut folded = ir.clone();
        fold_ir_blocks(&mut folded, &types);

        let after = match eval_ir(&folded, &types, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(false, "eval_ir on folded IR did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(before, after, "fold_ir_blocks changed the semantics");
    }

    #[test]
    fn prop_e_fold_ir_blocks_does_not_panic(
        (ir, types, _inputs) in gen_ir_and_inputs()
    ) {
        let mut folded = ir.clone();
        let _ = fold_ir_blocks(&mut folded, &types);
    }
}

// ============================================================================
// Property F — fold_biir_blocks preserves Boolar IR semantics
// ============================================================================

proptest! {
    #[test]
    fn prop_f_fold_biir_blocks_preserves_semantics(
        (blocks, inputs) in gen_biir_and_inputs()
    ) {
        let before = match eval_biir(&blocks, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        let mut folded = blocks.clone();
        fold_biir_blocks(&mut folded);

        let after = match eval_biir(&folded, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(false, "eval_biir on folded BIrBlocks did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(before, after, "fold_biir_blocks changed the semantics");
    }

    #[test]
    fn prop_f_fold_biir_blocks_does_not_panic(
        (blocks, _inputs) in gen_biir_and_inputs()
    ) {
        let mut folded = blocks.clone();
        let _ = fold_biir_blocks(&mut folded);
    }
}

// ============================================================================
// Property G — fold_vaffle_module preserves VAFFLE semantics
// ============================================================================

proptest! {
    #[test]
    fn prop_g_fold_vaffle_module_preserves_semantics(
        (module, func_id, inputs) in gen_vaffle_and_inputs()
    ) {
        // Evaluate BEFORE folding.
        let before = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        // Fold in place (Module doesn't implement Clone, so we evaluate
        // before and after using the same owned value).
        let mut module = module;
        fold_vaffle_module(&mut module);

        let after = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(false, "eval_vaffle on folded Module did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(before, after, "fold_vaffle_module changed the semantics");
    }

    #[test]
    fn prop_g_fold_vaffle_module_does_not_panic(
        (module, _func_id, _inputs) in gen_vaffle_and_inputs()
    ) {
        let mut module = module;
        let _ = fold_vaffle_module(&mut module);
    }
}

// ============================================================================
// Property H — store_forward passes preserve semantics
// ============================================================================

proptest! {
    #[test]
    fn prop_h_store_forward_ir_preserves_semantics(
        (ir, types, inputs) in gen_ir_extended_and_inputs()
    ) {
        let before = match eval_ir(&ir, &types, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        let mut forwarded = ir.clone();
        store_forward_ir_blocks(&mut forwarded);

        let after = match eval_ir(&forwarded, &types, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(false, "eval_ir on store-forwarded IR did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(before, after, "store_forward_ir_blocks changed the semantics");
    }

    #[test]
    fn prop_h_store_forward_ir_does_not_panic(
        (ir, _types, _inputs) in gen_ir_extended_and_inputs()
    ) {
        let mut forwarded = ir.clone();
        let _ = store_forward_ir_blocks(&mut forwarded);
    }

    #[test]
    fn prop_h_store_forward_vaffle_preserves_semantics(
        (module, func_id, inputs) in gen_vaffle_extended_and_inputs()
    ) {
        let before = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        let mut module = module;
        store_forward_vaffle_module(&mut module);

        let after = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(false, "eval_vaffle on store-forwarded module did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(before, after, "store_forward_vaffle_module changed the semantics");
    }

    #[test]
    fn prop_h_store_forward_vaffle_does_not_panic(
        (module, _func_id, _inputs) in gen_vaffle_extended_and_inputs()
    ) {
        let mut module = module;
        let _ = store_forward_vaffle_module(&mut module);
    }
}

// ============================================================================
// Property H (multi-block) — cross-block store forwarding preserves semantics
// ============================================================================

proptest! {
    #[test]
    fn prop_h_store_forward_vaffle_multiblock_preserves_semantics(
        (module, func_id, inputs) in gen_vaffle_multiblock_and_inputs()
    ) {
        let before = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => return Ok(()),
        };

        let mut module = module;
        store_forward_vaffle_module(&mut module);

        let after = match eval_vaffle(&module, func_id, &inputs) {
            Some(v) => v,
            None => {
                prop_assert!(false, "eval_vaffle on multi-block store-forwarded module did not terminate");
                return Ok(());
            }
        };

        prop_assert_eq!(before, after, "store_forward_vaffle_module changed multi-block semantics");
    }

    #[test]
    fn prop_h_store_forward_vaffle_multiblock_does_not_panic(
        (module, _func_id, _inputs) in gen_vaffle_multiblock_and_inputs()
    ) {
        let mut module = module;
        let _ = store_forward_vaffle_module(&mut module);
    }
}
