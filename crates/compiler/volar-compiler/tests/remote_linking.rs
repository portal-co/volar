//! Tests for remote-linked spec output: `LinkageKind::Remote`, `remote_refs()`,
//! `print_module_with_remotes()`, and `print_module_ts_with_imports()`.

use volar_compiler::{
    LinkedSpec, LinkageKind, LinkageSystem, IrModule, IrFunction,
    print_module_with_remotes, printer_ts::print_module_ts_with_imports,
};

const SIMPLE_SPEC: &str = r#"
pub struct Bit(pub bool);
pub struct Galois(pub u8);
pub trait Field {
    fn zero() -> Self;
}
"#;

fn parse_simple_spec() -> IrModule<IrFunction> {
    volar_compiler::parse_source(SIMPLE_SPEC, "test_primitives").expect("parse simple spec")
}

fn empty_module() -> IrModule<IrFunction> {
    IrModule {
        name: "woven".into(),
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        functions: vec![],
        type_aliases: vec![],
        consts: vec![],
    }
}

// ── 1. Remote spec is NOT inlined into apply() output ───────────────────────

#[test]
fn test_remote_spec_not_inlined() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_remote(
        "primitives",
        spec_module,
        "test-primitives",
        None,
    ));

    let mut target = empty_module();
    ls.apply(&mut target);

    assert!(target.structs.is_empty(), "remote spec must not be inlined into apply() output");
    assert!(target.traits.is_empty(), "remote spec traits must not be inlined");
}

// ── 2. Inline spec IS still inlined ─────────────────────────────────────────

#[test]
fn test_inline_spec_is_inlined() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_inline("primitives", spec_module));

    let mut target = empty_module();
    ls.apply(&mut target);

    assert!(!target.structs.is_empty(), "inline spec structs must be inlined");
    assert!(!target.traits.is_empty(), "inline spec traits must be inlined");
}

// ── 3. remote_refs() returns type names from remote spec ────────────────────

#[test]
fn test_remote_refs_returns_type_names() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_remote(
        "primitives",
        spec_module,
        "test-primitives",
        None,
    ));

    let refs = ls.remote_refs();
    assert_eq!(refs.len(), 1, "one remote spec → one ref");
    assert_eq!(refs[0].rust_crate, "test-primitives");
    assert!(refs[0].type_names.contains(&"Bit".to_string()), "should include Bit");
    assert!(refs[0].type_names.contains(&"Galois".to_string()), "should include Galois");
    assert!(refs[0].type_names.contains(&"Field".to_string()), "should include Field trait");
}

// ── 4. remote_refs() is empty when no remote specs ──────────────────────────

#[test]
fn test_remote_refs_empty_for_inline_only() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_inline("primitives", spec_module));

    assert!(ls.remote_refs().is_empty(), "inline-only linkage has no remote refs");
}

// ── 5. Rust preamble contains `pub use` for remote spec ─────────────────────

#[test]
fn test_rust_preamble_pub_use_for_remote_spec() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_remote(
        "primitives",
        spec_module,
        "test-primitives",
        None,
    ));

    let remotes = ls.remote_refs();
    let out = print_module_with_remotes(&empty_module(), &[], &remotes);

    assert!(
        out.contains("pub use test_primitives::"),
        "output must contain `pub use test_primitives::` (crate name dash→underscore)\nactual:\n{}",
        &out[..out.len().min(500)]
    );
    assert!(out.contains("Bit"), "pub use must include Bit");
    assert!(out.contains("Galois"), "pub use must include Galois");
}

// ── 6. No `pub use` emitted without remote specs ────────────────────────────

#[test]
fn test_rust_preamble_no_pub_use_without_remotes() {
    let out = print_module_with_remotes(&empty_module(), &[], &[]);
    assert!(
        !out.contains("pub use test_primitives"),
        "no remotes → no test_primitives pub use"
    );
}

// ── 7. TypeScript import with npm_package ───────────────────────────────────

#[test]
fn test_ts_import_with_npm_package() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_remote(
        "primitives",
        spec_module,
        "test-primitives",
        Some("@portal/primitives".to_string()),
    ));

    let remotes = ls.remote_refs();
    let out = print_module_ts_with_imports(&empty_module(), &remotes);

    assert!(
        out.contains("import {") && out.contains("from '@portal/primitives'"),
        "TS output must import from npm package\nactual:\n{}",
        &out[..out.len().min(500)]
    );
    assert!(out.contains("Bit"), "import must include Bit");
    assert!(out.contains("Galois"), "import must include Galois");
}

// ── 8. TypeScript import falls back to crate name when no npm_package ───────

#[test]
fn test_ts_import_falls_back_to_crate_name() {
    let spec_module = parse_simple_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_remote(
        "primitives",
        spec_module,
        "test-primitives",
        None,
    ));

    let remotes = ls.remote_refs();
    let out = print_module_ts_with_imports(&empty_module(), &remotes);

    assert!(
        out.contains("from 'test-primitives'"),
        "TS must fall back to crate name when no npm_package\nactual:\n{}",
        &out[..out.len().min(500)]
    );
}

// ── 9. No package import emitted without remote specs ────────────────────────

#[test]
fn test_ts_no_import_without_remotes() {
    let out = print_module_ts_with_imports(&empty_module(), &[]);
    // The TS preamble itself uses `import {}` for standard libs; we check that
    // there's no import from a remote crate or package.
    assert!(!out.contains("from 'test-primitives'"), "no remotes → no test-primitives import");
    assert!(!out.contains("from '@portal"), "no remotes → no portal package import");
}
