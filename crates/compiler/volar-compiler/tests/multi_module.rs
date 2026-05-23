//! Tests for multi-module output: partitioning, topological ordering, and
//! cross-module import generation for both Rust and TypeScript.

use volar_compiler::{
    LinkedSpec, LinkageSystem, IrModule, IrFunction,
    multi_module::{MultiModuleOutput, PartitionConfig, PartitionGroup},
};

const PRIMITIVES_SRC: &str = r#"
pub struct Bit(pub bool);
pub struct Galois(pub u8);
"#;

// Spec references Bit and Galois (defined in primitives).
const SPEC_SRC: &str = r#"
pub struct Delta<T>(pub T);
pub struct Vope<T>(pub T, pub Bit);
"#;

fn parse(src: &str, name: &str) -> IrModule<IrFunction> {
    volar_compiler::parse_source(src, name, &[]).expect("parse")
}

fn build_ls() -> LinkageSystem {
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec::new_inline("primitives", parse(PRIMITIVES_SRC, "primitives")));
    ls.add(LinkedSpec::new_inline("spec", parse(SPEC_SRC, "spec")));
    ls
}

fn two_group_config() -> PartitionConfig {
    PartitionConfig {
        groups: vec![
            PartitionGroup {
                name: "primitives".into(),
                rust_crate_name: "test-primitives".into(),
                npm_package: None,
                spec_names: vec!["primitives".into()],
            },
            PartitionGroup {
                name: "spec".into(),
                rust_crate_name: "test-spec".into(),
                npm_package: Some("@portal/spec".into()),
                spec_names: vec!["spec".into()],
            },
        ],
    }
}

// ── 1. Partition produces the correct number of modules ─────────────────────

#[test]
fn test_partition_module_count() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());
    assert_eq!(multi.modules.len(), 2, "two groups → two output modules");
}

// ── 2. Each module contains the right declarations ───────────────────────────

#[test]
fn test_partition_module_contents() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());

    let prim = multi.modules.iter().find(|m| m.name == "primitives").expect("primitives module");
    let spec = multi.modules.iter().find(|m| m.name == "spec").expect("spec module");

    let prim_names: Vec<_> = prim.module.structs.iter().map(|s| s.kind.to_string()).collect();
    assert!(prim_names.contains(&"Bit".to_string()), "primitives must contain Bit");
    assert!(prim_names.contains(&"Galois".to_string()), "primitives must contain Galois");

    let spec_names: Vec<_> = spec.module.structs.iter().map(|s| s.kind.to_string()).collect();
    assert!(spec_names.contains(&"Delta".to_string()), "spec must contain Delta");
    assert!(spec_names.contains(&"Vope".to_string()), "spec must contain Vope");
}

// ── 3. Topological order: primitives before spec ─────────────────────────────

#[test]
fn test_topological_order() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());

    let pos_prim = multi.modules.iter().position(|m| m.name == "primitives").unwrap();
    let pos_spec = multi.modules.iter().position(|m| m.name == "spec").unwrap();

    assert!(
        pos_prim < pos_spec,
        "primitives (pos {}) must come before spec (pos {})",
        pos_prim, pos_spec
    );
}

// ── 4. render_rust() emits `pub use` for cross-module deps ──────────────────

#[test]
fn test_render_rust_cross_module_import() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());
    let files = multi.render_rust();

    let spec_src = files.iter().find(|(n, _)| n == "spec").map(|(_, s)| s.as_str())
        .expect("spec file");

    assert!(
        spec_src.contains("pub use test_primitives::"),
        "spec Rust output must import from test_primitives\nactual:\n{}",
        &spec_src[..spec_src.len().min(800)]
    );
    assert!(spec_src.contains("Bit"), "cross-module import must include Bit");
}

// ── 5. render_rust() does NOT emit cross-module import for independent module ─

#[test]
fn test_render_rust_no_spurious_import() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());
    let files = multi.render_rust();

    let prim_src = files.iter().find(|(n, _)| n == "primitives").map(|(_, s)| s.as_str())
        .expect("primitives file");

    assert!(
        !prim_src.contains("pub use test_spec::"),
        "primitives must not import from spec"
    );
}

// ── 6. render_ts() emits `import` with npm_package when set ──────────────────

#[test]
fn test_render_ts_npm_package_import() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());
    // spec has npm_package = "@portal/spec" but depends on primitives (no npm_package)
    // → primitives import uses relative path ./primitives
    let files = multi.render_ts();

    // We want to check the woven module that imports from spec — but in this
    // two-group test, "spec" imports from "primitives".
    let spec_src = files.iter().find(|(n, _)| n == "spec").map(|(_, s)| s.as_str())
        .expect("spec ts file");

    // primitives has no npm_package → falls back to ./primitives
    assert!(
        spec_src.contains("from './primitives'") || spec_src.contains("from 'test-primitives'"),
        "spec TS must import from primitives\nactual:\n{}",
        &spec_src[..spec_src.len().min(800)]
    );
}

// ── 7. render_ts() returns correct number of (name, source) pairs ────────────

#[test]
fn test_render_ts_file_count() {
    let ls = build_ls();
    let multi = ls.partition(&two_group_config());
    let files = multi.render_ts();
    assert_eq!(files.len(), 2, "two modules → two TS files");
}

// ── 8. Single-group partition works without cross-module deps ────────────────

#[test]
fn test_single_group_no_deps() {
    let ls = build_ls();
    let config = PartitionConfig {
        groups: vec![PartitionGroup {
            name: "all".into(),
            rust_crate_name: "test-all".into(),
            npm_package: None,
            spec_names: vec!["primitives".into(), "spec".into()],
        }],
    };
    let multi = ls.partition(&config);
    assert_eq!(multi.modules.len(), 1);
    assert!(multi.modules[0].deps.is_empty(), "single group has no cross-module deps");
}
