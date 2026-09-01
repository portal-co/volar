//! Smoke that the volar-build wrapper still composes like volar-ir-tester.

use std::fs;
use volar_build::Pipeline;

#[test]
fn from_wasm_lower_to_volar_ir() {
    let wat = r#"
    (module
      (func $id (export "id") (param i32) (result i32)
        local.get 0))
    "#;
    let bytes = wat::parse_str(wat).expect("valid wat");
    let path = std::env::temp_dir().join(format!(
        "volar-build-smoke-{}-{}.wasm",
        std::process::id(),
        "id"
    ));
    fs::write(&path, bytes).expect("write wasm");
    let (blocks, _types) = Pipeline::from_wasm(&path)
        .lower_to_volar_ir()
        .to_volar_ir()
        .expect("wrapper from_wasm → Volar IR");
    assert!(!blocks.blocks.is_empty());
    let _ = fs::remove_file(&path);
}
