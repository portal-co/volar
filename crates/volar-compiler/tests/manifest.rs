//! Tests for the manifest system: emit, parse, round-trip, and cross-crate integration.

use std::fs;
use volar_compiler::{
    parse_source, emit_manifest, is_manifest, TypeManifest, MANIFEST_MARKER,
    TypeContext, IrType, PrimitiveType, StructKind,
    print_module_with_deps, DynPreambleWriter, DisplayRust, RustBackend,
};

#[cfg(feature = "parsing")]
use volar_compiler::parse_manifest;

// ============================================================================
// Phase 4: Manifest emit/parse/round-trip
// ============================================================================

#[test]
fn test_parse_volar_primitives_source() {
    let source = include_str!("../data/volar_primitives_expanded.rs");
    let module = parse_source(source, "volar_primitives")
        .expect("parse volar-primitives");

    // 6 structs: Bit, Galois, BitsInBytes, Galois64, BitsInBytes64, Tropical
    assert_eq!(module.structs.len(), 6, "expected 6 structs, got {}", module.structs.len());

    // 1 trait: Invert
    assert_eq!(module.traits.len(), 1);
    assert_eq!(module.traits[0].kind.to_string(), "Invert");

    // Many impls: 4 per field type (Add, Mul, Sub, BitXor<u8>) + 1 Invert + 2 Tropical
    // Bit: 4, Galois: 4 + Invert = 5, BitsInBytes: 4, Galois64: 4, BitsInBytes64: 4, Tropical: 2
    // Total: 4 + 5 + 4 + 4 + 4 + 2 = 23
    assert_eq!(module.impls.len(), 23, "expected 23 impls, got {}", module.impls.len());

    // Verify struct fields
    let bit = module.structs.iter().find(|s| s.kind.to_string() == "Bit").unwrap();
    assert!(bit.is_tuple);
    assert_eq!(bit.fields.len(), 1);
    assert_eq!(bit.fields[0].ty, IrType::Primitive(PrimitiveType::Bool));

    let galois = module.structs.iter().find(|s| s.kind.to_string() == "Galois").unwrap();
    assert!(galois.is_tuple);
    assert_eq!(galois.fields[0].ty, IrType::Primitive(PrimitiveType::U8));

    let galois64 = module.structs.iter().find(|s| s.kind.to_string() == "Galois64").unwrap();
    assert!(galois64.is_tuple);
    assert_eq!(galois64.fields[0].ty, IrType::Primitive(PrimitiveType::U64));

    let tropical = module.structs.iter().find(|s| s.kind.to_string() == "Tropical").unwrap();
    assert!(tropical.is_tuple);
    assert_eq!(tropical.generics.len(), 1);
    assert_eq!(tropical.generics[0].name, "T");
}

#[test]
fn test_emit_manifest_has_marker() {
    let source = "pub struct Foo { pub x: u8 }";
    let module = parse_source(source, "test").unwrap();
    let bytes = emit_manifest(&module, "test-crate", "0.1.0", &[]);

    assert_eq!(bytes[0], MANIFEST_MARKER);
    assert!(is_manifest(&bytes));
    assert!(!is_manifest(b"not a manifest"));
}

#[test]
fn test_emit_manifest_not_valid_utf8() {
    let source = "pub struct Foo { pub x: u8 }";
    let module = parse_source(source, "test").unwrap();
    let bytes = emit_manifest(&module, "test-crate", "0.1.0", &[]);

    // The whole thing should NOT be valid UTF-8 (because of 0xFF prefix)
    assert!(core::str::from_utf8(&bytes).is_err());
}

#[test]
fn test_manifest_round_trip_simple() {
    let source = r#"
        pub struct Foo { pub x: u8, pub y: bool }
        pub trait Bar {
            fn baz(&self) -> u32;
        }
        impl Bar for Foo {
            fn baz(&self) -> u32 { todo!() }
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let bytes = emit_manifest(&module, "my-crate", "1.2.3", &["dep-a".into(), "dep-b".into()]);

    let manifest = parse_manifest(&bytes).expect("parse manifest");
    assert_eq!(manifest.crate_name, "my-crate");
    assert_eq!(manifest.version, "1.2.3");
    assert_eq!(manifest.deps, vec!["dep-a", "dep-b"]);

    // Struct preserved
    assert_eq!(manifest.module.structs.len(), 1);
    assert_eq!(manifest.module.structs[0].kind.to_string(), "Foo");
    assert_eq!(manifest.module.structs[0].fields.len(), 2);

    // Trait preserved
    assert_eq!(manifest.module.traits.len(), 1);
    assert_eq!(manifest.module.traits[0].kind.to_string(), "Bar");

    // Impl preserved
    assert_eq!(manifest.module.impls.len(), 1);
    assert_eq!(manifest.module.impls[0].trait_.as_ref().unwrap().kind.to_string(), "Bar");
}

#[test]
fn test_manifest_round_trip_primitives() {
    let source = include_str!("../data/volar_primitives_expanded.rs");
    let module = parse_source(source, "volar_primitives").unwrap();

    let bytes = emit_manifest(&module, "volar-primitives", "0.1.0", &[]);
    let manifest = parse_manifest(&bytes).expect("parse primitives manifest");

    assert_eq!(manifest.crate_name, "volar-primitives");
    assert_eq!(manifest.version, "0.1.0");
    assert_eq!(manifest.module.structs.len(), module.structs.len());
    assert_eq!(manifest.module.traits.len(), module.traits.len());
    assert_eq!(manifest.module.impls.len(), module.impls.len());

    // Verify specific structs survived round-trip
    let names: Vec<String> = manifest.module.structs.iter()
        .map(|s| s.kind.to_string())
        .collect();
    assert!(names.contains(&"Bit".to_string()));
    assert!(names.contains(&"Galois".to_string()));
    assert!(names.contains(&"Galois64".to_string()));
    assert!(names.contains(&"BitsInBytes".to_string()));
    assert!(names.contains(&"BitsInBytes64".to_string()));
    assert!(names.contains(&"Tropical".to_string()));
}

#[test]
fn test_manifest_parse_rejects_non_manifest() {
    let result = parse_manifest(b"pub struct Foo {}");
    assert!(result.is_err());
}

#[test]
fn test_manifest_bodies_are_stripped() {
    let source = r#"
        pub fn complex_fn(x: u8) -> u8 {
            let y = x + 1;
            let z = y * 2;
            z
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let bytes = emit_manifest(&module, "test", "0.1.0", &[]);

    // The emitted text (after 0xFF) should contain "todo!()" 
    let text = core::str::from_utf8(&bytes[1..]).unwrap();
    assert!(text.contains("todo!()"), "manifest should have todo!() bodies, got:\n{}", text);
    // Should NOT contain the original body expressions
    assert!(!text.contains("y * 2"), "manifest should not have original body");
}

// ============================================================================
// Phase 5: TypeContext::from_module_with_deps
// ============================================================================

#[test]
fn test_type_context_with_primitives_dep() {
    let prim_source = include_str!("../data/volar_primitives_expanded.rs");
    let prim_module = parse_source(prim_source, "volar_primitives").unwrap();
    let prim_bytes = emit_manifest(&prim_module, "volar-primitives", "0.1.0", &[]);
    let prim_manifest = parse_manifest(&prim_bytes).unwrap();

    // A downstream module that uses Galois
    let downstream = r#"
        pub struct MyStruct {
            pub field: Galois,
        }
    "#;
    let module = parse_source(downstream, "downstream").unwrap();

    let ctx = TypeContext::from_module_with_deps(&module, &[prim_manifest]);

    // Galois should be known
    assert!(ctx.structs.contains_key("Galois"), "Galois should be in context");
    assert!(ctx.structs.contains_key("Bit"), "Bit should be in context");
    assert!(ctx.structs.contains_key("BitsInBytes64"), "BitsInBytes64 should be in context");

    // The downstream struct should also be known
    assert!(ctx.structs.contains_key("MyStruct"), "MyStruct should be in context");

    // Invert trait should be known
    assert!(ctx.traits.contains_key("Invert"), "Invert should be in context");

    // Built-in traits should also be present
    assert!(ctx.traits.contains_key("Add"), "Add should be in context");
}

#[test]
fn test_type_context_dep_impls_registered() {
    let prim_source = include_str!("../data/volar_primitives_expanded.rs");
    let prim_module = parse_source(prim_source, "volar_primitives").unwrap();
    let prim_bytes = emit_manifest(&prim_module, "volar-primitives", "0.1.0", &[]);
    let prim_manifest = parse_manifest(&prim_bytes).unwrap();

    let module = parse_source("pub struct Dummy {}", "test").unwrap();
    let ctx = TypeContext::from_module_with_deps(&module, &[prim_manifest]);

    // Should have trait impls from the dep
    let add_impls: Vec<_> = ctx.trait_impls.iter()
        .filter(|imp| imp.trait_.as_ref().map_or(false, |t| t.kind.to_string() == "Add"))
        .collect();
    // At least: Bit+Bit, Galois+Galois, BitsInBytes+BitsInBytes, Galois64+Galois64, BitsInBytes64+BitsInBytes64, Tropical
    assert!(add_impls.len() >= 6, "expected >=6 Add impls, got {}", add_impls.len());
}

#[test]
fn test_module_overrides_dep_struct() {
    let prim_source = include_str!("../data/volar_primitives_expanded.rs");
    let prim_module = parse_source(prim_source, "volar_primitives").unwrap();
    let prim_bytes = emit_manifest(&prim_module, "volar-primitives", "0.1.0", &[]);
    let prim_manifest = parse_manifest(&prim_bytes).unwrap();

    // A module that redefines Galois with different fields
    let source = r#"
        pub struct Galois { pub x: u32, pub y: u32 }
    "#;
    let module = parse_source(source, "override_test").unwrap();
    let ctx = TypeContext::from_module_with_deps(&module, &[prim_manifest]);

    // Module's definition should win
    let galois = ctx.structs.get("Galois").unwrap();
    assert_eq!(galois.fields.len(), 2, "module's Galois should have 2 fields");
    assert!(!galois.is_tuple, "module's Galois should not be tuple");
}

// ============================================================================
// Phase 7: Data-driven preamble & end-to-end
// ============================================================================

#[test]
fn test_preamble_no_deps_has_no_pub_use() {
    let preamble = format!("{}", DisplayRust(DynPreambleWriter { deps: &[] }));
    assert!(!preamble.contains("pub use"), "no deps = no pub use statements");
    // Should still have standard imports
    assert!(preamble.contains("use alloc::vec::Vec"));
    assert!(preamble.contains("fn ilog2"));
}

#[test]
fn test_preamble_with_primitives_dep() {
    let prim_source = include_str!("../data/volar_primitives_expanded.rs");
    let prim_module = parse_source(prim_source, "volar_primitives").unwrap();
    let prim_bytes = emit_manifest(&prim_module, "volar-primitives", "0.1.0", &[]);
    let prim_manifest = parse_manifest(&prim_bytes).unwrap();

    let preamble = format!("{}", DisplayRust(DynPreambleWriter { deps: &[prim_manifest] }));

    // Should have pub use for primitives types
    assert!(preamble.contains("pub use volar_primitives::"), "should import from volar_primitives");
    assert!(preamble.contains("Bit"), "should import Bit");
    assert!(preamble.contains("Galois"), "should import Galois");
    assert!(preamble.contains("Galois64"), "should import Galois64");
    assert!(preamble.contains("BitsInBytes"), "should import BitsInBytes");
    assert!(preamble.contains("BitsInBytes64"), "should import BitsInBytes64");
    assert!(preamble.contains("Tropical"), "should import Tropical");
    assert!(preamble.contains("Invert"), "should import Invert trait");
}

#[test]
fn test_print_module_with_deps_includes_preamble() {
    let prim_source = include_str!("../data/volar_primitives_expanded.rs");
    let prim_module = parse_source(prim_source, "volar_primitives").unwrap();
    let prim_bytes = emit_manifest(&prim_module, "volar-primitives", "0.1.0", &[]);
    let prim_manifest = parse_manifest(&prim_bytes).unwrap();

    let source = "pub struct Foo { pub x: u8 }";
    let module = parse_source(source, "test").unwrap();
    let output = print_module_with_deps(&module, &[prim_manifest]);

    // Should contain both preamble and module content
    assert!(output.contains("pub use volar_primitives::"));
    assert!(output.contains("pub struct Foo"));
    assert!(output.contains("fn ilog2"));
}

#[test]
fn test_end_to_end_volar_primitives_manifest_file() {
    // Parse volar-primitives
    let prim_source = include_str!("../data/volar_primitives_expanded.rs");
    let prim_module = parse_source(prim_source, "volar_primitives").unwrap();

    // Emit manifest
    let manifest_bytes = emit_manifest(&prim_module, "volar-primitives", "0.1.0", &[]);
    assert!(is_manifest(&manifest_bytes));

    // Parse manifest back
    let manifest = parse_manifest(&manifest_bytes).unwrap();
    assert_eq!(manifest.crate_name, "volar-primitives");
    assert_eq!(manifest.module.structs.len(), 6);
    assert_eq!(manifest.module.traits.len(), 1);
    assert_eq!(manifest.module.impls.len(), 23);

    // Use manifest as dependency for a downstream module
    let downstream_source = r#"
        pub struct MyField {
            pub value: Galois,
            pub bits: BitsInBytes,
        }
    "#;
    let downstream = parse_source(downstream_source, "downstream").unwrap();

    // TypeContext resolves dependency types
    let ctx = TypeContext::from_module_with_deps(&downstream, &[manifest.clone()]);
    assert!(ctx.structs.contains_key("Galois"));
    assert!(ctx.structs.contains_key("BitsInBytes"));
    assert!(ctx.structs.contains_key("MyField"));

    // Print with deps produces correct preamble
    let output = print_module_with_deps(&downstream, &[manifest]);
    assert!(output.contains("pub use volar_primitives::{Bit"));
    assert!(output.contains("pub struct MyField"));
}

#[test]
fn test_manifest_with_multiple_deps() {
    // Create two fake dependency manifests
    let dep_a_source = r#"
        pub struct AlphaType { pub a: u8 }
        pub trait AlphaTrait {
            fn alpha(&self) -> u32;
        }
    "#;
    let dep_b_source = r#"
        pub struct BetaType { pub b: u64 }
    "#;

    let dep_a_module = parse_source(dep_a_source, "dep_a").unwrap();
    let dep_b_module = parse_source(dep_b_source, "dep_b").unwrap();

    let dep_a_bytes = emit_manifest(&dep_a_module, "dep-a", "1.0.0", &[]);
    let dep_b_bytes = emit_manifest(&dep_b_module, "dep-b", "2.0.0", &["dep-a".into()]);

    let dep_a = parse_manifest(&dep_a_bytes).unwrap();
    let dep_b = parse_manifest(&dep_b_bytes).unwrap();
    assert_eq!(dep_b.deps, vec!["dep-a"]);

    let main_source = "pub struct Main { pub x: bool }";
    let main_module = parse_source(main_source, "main").unwrap();

    let ctx = TypeContext::from_module_with_deps(&main_module, &[dep_a.clone(), dep_b.clone()]);
    assert!(ctx.structs.contains_key("AlphaType"));
    assert!(ctx.structs.contains_key("BetaType"));
    assert!(ctx.structs.contains_key("Main"));
    assert!(ctx.traits.contains_key("AlphaTrait"));

    let output = print_module_with_deps(&main_module, &[dep_a, dep_b]);
    assert!(output.contains("pub use dep_a::{AlphaType, AlphaTrait}"));
    assert!(output.contains("pub use dep_b::{BetaType}"));
}
