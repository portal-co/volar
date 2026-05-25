//! cargo-fuzz target: virtualize_ir (Public/InIr) preserves semantics.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_fuzz::arbitrary::ir::ArbitraryIr;
use volar_fuzz::interpreter::ir::eval_ir;
use volar_ir_virt::{virtualize_ir, BytecodeForm, DispatchMode, VirtualizeConfig};

fuzz_target!(|data: ArbitraryIr| {
    let ArbitraryIr { blocks: ir, types, inputs } = data;

    let expected = match eval_ir(&ir, &types, &inputs) {
        Some(v) => v,
        None => return,
    };

    let mut types_mut = types.clone();
    let cfg = VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    };
    let virt = virtualize_ir::<()>(&ir, &mut types_mut, &cfg);

    let actual = match eval_ir(&virt.blocks, &types_mut, &inputs) {
        Some(v) => v,
        None => return,
    };

    assert_eq!(actual, expected, "virtualize_ir changed the output");
});
