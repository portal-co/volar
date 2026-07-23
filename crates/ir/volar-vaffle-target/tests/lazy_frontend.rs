//! @ai: unreviewed
//! Demand-indexed WAFFLE frontend regression coverage.

use portal_pc_waffle_frontend::{from_wasm_bytes, FrontendOptions};
use portal_pc_waffle_ir::{ExportKind, FuncDecl};
use volar_vaffle_target::{lower_waffle_function_lazy, VaffleTarget, WaffleImportConfig};

#[test]
fn selected_lazy_export_lowers_without_materializing_its_sibling() {
    // `(module (func (export "root") (result i32) i32.const 7)
    //          (func (result i32) i32.const 9))`
    let bytes = [
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f,
        0x03, 0x03, 0x02, 0x00, 0x00, 0x07, 0x08, 0x01, 0x04, b'r', b'o', b'o', b't', 0x00, 0x00,
        0x0a, 0x0b, 0x02, 0x04, 0x00, 0x41, 0x07, 0x0b, 0x04, 0x00, 0x41, 0x09, 0x0b,
    ];
    let wasm = from_wasm_bytes(&bytes, &FrontendOptions::default())
        .expect("fixture must parse as a lazy WAFFLE module");
    let root = match &wasm.exports[0].kind {
        ExportKind::Func(function) => *function,
        other => panic!("root export was not a function: {other:?}"),
    };

    assert!(matches!(wasm.funcs[root], FuncDecl::Lazy(..)));
    let sibling = wasm
        .funcs
        .entries()
        .find_map(|(function, _)| (function != root).then_some(function))
        .expect("fixture must contain an unexported sibling");
    assert!(matches!(wasm.funcs[sibling], FuncDecl::Lazy(..)));

    let mut target = VaffleTarget::new();
    lower_waffle_function_lazy(&wasm, root, &mut target, &WaffleImportConfig::default())
        .expect("selected function must lower");

    assert_eq!(target.module.funcs.len(), 1);
    // `clone_and_expand_body` resolves only a clone of the selected body; the
    // indexed source module still proves no sibling body was materialized.
    assert!(matches!(wasm.funcs[sibling], FuncDecl::Lazy(..)));
}
