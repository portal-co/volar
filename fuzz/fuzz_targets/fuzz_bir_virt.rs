//! cargo-fuzz target: virtualize_bir (Public/InIr) via lower_ir_to_boolar preserves semantics.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::ir::ArbitraryIr;
use volar_fuzz::interpreter::biir::eval_biir;
use volar_fuzz::interpreter::ir::{bit_flatten, bit_unflatten, eval_ir};
use volar_ir_passes::lower_ir_to_boolar;
use volar_ir_virt::{virtualize_bir, BytecodeForm, DispatchMode, VirtualizeConfig};

fuzz_target!(|data: ArbitraryIr| {
    let ArbitraryIr { blocks: ir, types, inputs } = data;

    let ir_out = match eval_ir(&ir, &types, &inputs) {
        Some(v) => v,
        None => return,
    };

    let bir = lower_ir_to_boolar(&ir, &types);
    let cfg = VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    };
    let virt = virtualize_bir::<()>(&bir, &cfg);

    let flat_inputs = bit_flatten(&inputs);
    let flat_actual = match eval_biir(&virt.blocks, &flat_inputs) {
        Some(v) => v,
        None => return,
    };

    let out_widths: Vec<usize> = ir_out.iter().map(|v| v.len()).collect();
    let actual = bit_unflatten(&flat_actual, &out_widths);

    assert_eq!(actual, ir_out, "virtualize_bir changed the output");
});
