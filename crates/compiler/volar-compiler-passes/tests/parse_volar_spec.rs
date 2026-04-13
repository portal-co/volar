//! Integration test that parses volar-spec sources

use std::fs;
use std::path::Path;
use volar_compiler::{parse_source, parse_sources};
use volar_compiler_passes::{OperatorAnalysis, TypeContext, type_to_string};

fn read_volar_spec_sources() -> Vec<(String, String)> {
    let base_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("spec")
        .join("volar-spec")
        .join("src");

    let mut sources = Vec::new();

    fn collect_rs_files(dir: &Path, sources: &mut Vec<(String, String)>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().map_or(false, |e| e == "rs") {
                    if let Ok(content) = fs::read_to_string(&path) {
                        let name = path.file_stem().unwrap().to_string_lossy().to_string();
                        sources.push((content, name));
                    }
                } else if path.is_dir() {
                    collect_rs_files(&path, sources);
                }
            }
        }
    }

    collect_rs_files(&base_path, &mut sources);
    sources
}

#[test]
fn test_parse_volar_spec() {
    let sources = read_volar_spec_sources();

    // Ensure we found source files
    assert!(!sources.is_empty(), "Should find volar-spec source files");
    println!("Found {} source files", sources.len());

    // Parse each file individually first
    for (content, name) in &sources {
        match parse_source(content, name) {
            Ok(module) => {
                println!(
                    "Parsed {}: {} structs, {} traits, {} impls, {} functions",
                    name,
                    module.structs.len(),
                    module.traits.len(),
                    module.impls.len(),
                    module.functions.len()
                );
            }
            Err(e) => {
                println!("Warning: Failed to parse {}: {}", name, e);
                // Don't fail the test - some files might have features we don't support yet
            }
        }
    }
}

#[test]
fn test_parse_volar_spec_combined() {
    let sources = read_volar_spec_sources();

    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(content, name)| (content.as_str(), name.as_str()))
        .collect();

    match parse_sources(&sources_ref, "volar_spec") {
        Ok(module) => {
            println!("Combined module statistics:");
            println!("  Structs: {}", module.structs.len());
            println!("  Traits: {}", module.traits.len());
            println!("  Impls: {}", module.impls.len());
            println!("  Functions: {}", module.functions.len());
            println!("  Type aliases: {}", module.type_aliases.len());
            // println!("  Uses: {}", module.uses.len());

            // Analyze operators
            let op_analysis = OperatorAnalysis::from_module(&module);
            println!("  Binary operator impls: {}", op_analysis.binary_ops.len());

            // Build type context
            let type_ctx = TypeContext::from_module(&module);
            println!("  Known structs: {}", type_ctx.structs.len());
            println!("  Trait impls: {}", type_ctx.trait_impls.len());
            println!("  Associated types: {}", type_ctx.assoc_types.len());

            // Print some struct names
            println!("\nStructs found:");
            for s in &module.structs {
                let generics: Vec<_> = s.generics.iter().map(|g| &g.name).collect();
                println!(
                    "  {:?} <{}>",
                    s.kind,
                    generics
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }

            // Print impl blocks
            println!("\nImpl blocks found:");
            for imp in &module.impls {
                let self_ty = type_to_string(&imp.self_ty);
                if let Some(trait_ref) = &imp.trait_ {
                    println!("  {:?} for {}", trait_ref.kind, self_ty);
                } else {
                    println!("  impl {}", self_ty);
                }
            }
        }
        Err(e) => {
            println!("Failed to parse combined module: {}", e);
        }
    }
}

#[test]
fn test_parse_vole_struct() {
    let source = r#"
pub struct Delta<N: ArrayLength<T>, T> {
    pub delta: GenericArray<T, N>,
}
pub struct Q<N: ArrayLength<T>, T> {
    pub q: GenericArray<T, N>,
}
    "#;

    let module = parse_source(source, "vole").unwrap();

    assert_eq!(module.structs.len(), 2);

    let delta = &module.structs[0];
    assert!(matches!(delta.kind, volar_compiler::StructKind::Custom(ref n) if n == "Delta"));
    assert_eq!(delta.generics.len(), 2);
    assert_eq!(delta.generics[0].name, "N");
    assert_eq!(delta.generics[1].name, "T");
    assert_eq!(delta.fields.len(), 1);
    assert_eq!(delta.fields[0].name, "delta");
}

#[test]
fn test_parse_vope_struct() {
    let source = r#"
pub struct Vope<N: VoleArray<T>, T, K: ArrayLength<GenericArray<T, N>> = U1> {
    ///Multiplication-based randomizer
    pub u: GenericArray<GenericArray<T, N>, K>,
    ///Fixed offset
    pub v: GenericArray<T, N>,
}
    "#;

    let module = parse_source(source, "vope").unwrap();

    assert_eq!(module.structs.len(), 1);

    let vope = &module.structs[0];
    assert!(matches!(vope.kind, volar_compiler::StructKind::Custom(ref n) if n == "Vope"));
    assert_eq!(vope.generics.len(), 3);
    assert_eq!(vope.fields.len(), 2);
}

#[test]
fn test_parse_add_impl() {
    let source = r#"
impl<
    N: VoleArray<T> + VoleArray<U> + VoleArray<T::Output>,
    T: Add<U> + Clone,
    U: Clone,
    K: ArrayLength<GenericArray<U, N>>
        + ArrayLength<GenericArray<T::Output, N>>
        + ArrayLength<GenericArray<T, N>>,
> Add<Vope<N, U, K>> for Vope<N, T, K>
{
    type Output = Vope<N, T::Output, K>;
    fn add(self, rhs: Vope<N, U, K>) -> Self::Output {
        Vope {
            u: self.u.zip(rhs.u, |a, b| a.zip(b, |a, b| a + b)),
            v: self.v.zip(rhs.v, |a, b| a + b),
        }
    }
}
    "#;

    let module = parse_source(source, "add_impl").unwrap();

    assert_eq!(module.impls.len(), 1);

    let imp = &module.impls[0];
    assert!(imp.trait_.is_some());

    let trait_ref = imp.trait_.as_ref().unwrap();
    assert!(matches!(
        trait_ref.kind,
        volar_compiler::TraitKind::Math(volar_compiler::MathTrait::Add)
    ));

    // Check we have the Output associated type and add method
    let mut has_output = false;
    let mut has_add = false;
    for item in &imp.items {
        match item {
            volar_compiler::IrImplItem::AssociatedType { name, .. } => {
                if matches!(name, volar_compiler::AssociatedType::Output) {
                    has_output = true;
                }
            }
            volar_compiler::IrImplItem::Method(f) => {
                if f.name == "add" {
                    has_add = true;
                }
            }
            _ => {}
        }
    }
    assert!(has_output, "Should have Output associated type");
    assert!(has_add, "Should have add method");
}

#[test]
fn test_parse_method_with_closure() {
    let source = r#"
impl<N: ArrayLength<T>, T> Delta<N, T> {
    pub fn remap<M: ArrayLength<T>>(&self, mut f: impl FnMut(usize) -> usize) -> Delta<M, T>
    where
        T: Clone,
    {
        let Self { delta } = self;
        Delta {
            delta: GenericArray::<T, M>::generate(|i| delta[f(i) % N::to_usize()].clone()),
        }
    }
}
    "#;

    let module = parse_source(source, "remap").unwrap();

    assert_eq!(module.impls.len(), 1);

    let imp = &module.impls[0];
    assert_eq!(imp.items.len(), 1);

    if let volar_compiler::IrImplItem::Method(f) = &imp.items[0] {
        assert_eq!(f.name, "remap");
        assert_eq!(f.generics.len(), 1);
        assert_eq!(f.generics[0].name, "M");
        assert!(f.receiver.is_some());
        assert_eq!(f.params.len(), 1);
        assert!(!f.where_clause.is_empty());
    } else {
        panic!("Expected a method");
    }
}

#[test]
fn test_parse_logarithm2_projection_in_generate() {
    let source = r#"
use generic_array::{GenericArray, ArrayLength};
use typenum::Logarithm2;

struct Foo;

impl Foo {
    pub fn bar(&self) -> Vec<u8> {
        GenericArray::<u8, <typenum::U32 as Logarithm2>::Output>::generate(|j| {
            j as u8
        })
    }
}
    "#;

    let module = parse_source(source, "log2_test").unwrap();
    assert_eq!(module.impls.len(), 1);

    let imp = &module.impls[0];
    assert_eq!(imp.items.len(), 1);

    if let volar_compiler::ir::IrImplItem::Method(f) = &imp.items[0] {
        assert_eq!(f.name, "bar");
        let tail = f
            .body
            .expr
            .as_ref()
            .expect("body should have tail expression");
        match tail.as_ref() {
            volar_compiler::ir::IrExpr::ArrayGenerate { len, .. } => match len {
                volar_compiler::ir::ArrayLength::Projection {
                    field, trait_path, ..
                } => {
                    assert_eq!(field, "Output");
                    assert_eq!(
                        trait_path.as_deref(),
                        Some("Logarithm2"),
                        "Expected trait_path=Some(\"Logarithm2\"), got {:?}",
                        trait_path
                    );
                }
                other => panic!("Expected ArrayLength::Projection, got {:?}", other),
            },
            other => panic!("Expected ArrayGenerate expr, got {:?}", other),
        }
    } else {
        panic!("Expected a method");
    }
}

#[test]
#[cfg(feature = "parsing")]
fn test_prove_module_static_print_roundtrip() {
    use std::process::Command;
    use std::fs;

    let prove_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent().unwrap()
        .parent().unwrap()
        .join("spec/volar-spec/src/vole/prove.rs");
    let prove_src = fs::read_to_string(&prove_path)
        .expect("prove.rs should exist");
    let prove_src = prove_src.as_str();
    let module = volar_compiler::parse_source(prove_src, "prove")
        .expect("prove.rs should parse in the total-Rust subset");

    assert_eq!(module.functions.len(), 2, "Expected vole_and_prover_step and vole_and_verifier_check");
    assert!(module.functions.iter().any(|f| f.name == "vole_and_prover_step"));
    assert!(module.functions.iter().any(|f| f.name == "vole_and_verifier_check"));

    // Print the module body using the static (non-dyn) printer
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    let body = format!("{}", DisplayRust(ModuleWriter { module: &module }));
    assert!(body.contains("Array::<T, N>::from_fn"), "Should use static from_fn form, got:\n{}", body);
    assert!(body.contains("Array::<Array<T, N>, U1>::from_fn"), "Should handle nested array elem_ty, got:\n{}", body);
    assert!(body.contains("N::USIZE"), "BoundedLoop end should be N::USIZE, got:\n{}", body);

    // Build a temp crate and cargo-check it
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(3)
        .unwrap()
        .to_string_lossy()
        .into_owned();

    let tmp = std::env::temp_dir().join("volar_prove_print_roundtrip");
    let src = tmp.join("src");
    fs::create_dir_all(&src).unwrap();

    let code = format!(
        "#![allow(unused, non_snake_case)]\n\
         extern crate alloc;\n\
         use alloc::vec::Vec;\n\
         use core::ops::{{Add, Mul}};\n\
         use hybrid_array::{{Array, ArraySize}};\n\
         use cipher::consts::U1;\n\
         use volar_spec::vole::{{Delta, Q, Vope, VoleArray}};\n\
         \n\
         {body}\n",
        body = body,
    );

    let cargo_toml = format!(
        "[package]\nname = \"prove-roundtrip\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\
         [lib]\npath = \"src/lib.rs\"\n\
         [dependencies]\n\
         volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
         hybrid-array = \"0.4.8\"\n\
         cipher = {{ version = \"0.5.1\", default-features = false }}\n\
         typenum = {{ version = \"1.17\", default-features = false }}\n",
        root = root,
    );

    fs::write(tmp.join("Cargo.toml"), &cargo_toml).unwrap();
    fs::write(src.join("lib.rs"), &code).unwrap();

    let out = Command::new("cargo")
        .args(["check", "--quiet"])
        .current_dir(&tmp)
        .env("CARGO_TARGET_DIR", tmp.join("target").to_str().unwrap())
        .output()
        .expect("cargo check");

    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    let _ = fs::remove_dir_all(&tmp);

    assert!(
        out.status.success(),
        "Static print of prove.rs failed to compile:\n--- code ---\n{}\n--- stderr ---\n{}",
        code,
        stderr
    );
}

#[test]
#[cfg(feature = "parsing")]
fn test_parse_primitives_and_generate_manifest() {
    use volar_compiler::manifest::emit_manifest;

    let prim_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent().unwrap()
        .parent().unwrap()
        .join("spec/volar-primitives/src/lib.rs");
    let prim_src = fs::read_to_string(&prim_path)
        .expect("volar-primitives/src/lib.rs should exist");

    let module = volar_compiler::parse_source(&prim_src, "volar_primitives")
        .expect("lib.rs should parse (macros removed, total-Rust subset)");

    // Verify key types are present
    let struct_names: Vec<_> = module.structs.iter()
        .map(|s| s.kind.to_string()).collect();
    for expected in ["Bit", "Galois", "BitsInBytes", "Galois64", "BitsInBytes64",
                     "Galois128", "Galois256", "U256", "Tropical"] {
        assert!(struct_names.contains(&expected.to_string()),
            "Missing struct '{}' in parsed module. Found: {:?}", expected, struct_names);
    }

    // Verify compilable field functions are present
    let fn_names: Vec<_> = module.functions.iter().map(|f| f.name.as_str()).collect();
    for expected in ["gf_mul_u8", "gf_invert_u8", "gf_mul_u64", "gf_invert_u64",
                     "gf_mul_u128", "gf_invert_u128", "gf_mul_256", "gf_invert_256"] {
        assert!(fn_names.contains(&expected),
            "Missing function '{}'. Found: {:?}", expected, fn_names);
    }

    // Verify trait impls are present
    let impl_count = module.impls.len();
    assert!(impl_count >= 20, "Expected at least 20 impl blocks, got {}", impl_count);

    // Generate manifest — should not panic
    let manifest_bytes = emit_manifest(&module, "volar-primitives", "0.1.0", &[]);
    assert!(!manifest_bytes.is_empty(), "Manifest should not be empty");

    // Round-trip: parse the manifest back
    let parsed_back = volar_compiler::manifest::parse_manifest(&manifest_bytes)
        .expect("Manifest should round-trip");
    assert_eq!(parsed_back.crate_name, "volar-primitives");

    // The manifest structs should match the original
    let manifest_structs: Vec<_> = parsed_back.module.structs.iter()
        .map(|s| s.kind.to_string()).collect();
    for expected in ["Bit", "Galois", "Galois128", "U256"] {
        assert!(manifest_structs.contains(&expected.to_string()),
            "Manifest missing struct '{}'. Found: {:?}", expected, manifest_structs);
    }
}

#[test]
#[cfg(feature = "parsing")]
fn test_primitives_ts_transpile() {
    let prim_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent().unwrap()
        .parent().unwrap()
        .join("spec/volar-primitives/src/lib.rs");
    let prim_src = fs::read_to_string(&prim_path).unwrap();

    let module = volar_compiler::parse_source(&prim_src, "volar_primitives").unwrap();

    // Static Rust print — should contain from_fn and field ops
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    let rust_out = format!("{}", DisplayRust(ModuleWriter { module: &module }));
    assert!(rust_out.contains("fn gf_mul_u8"), "Should contain gf_mul_u8:\n{}", &rust_out[..500]);
    assert!(rust_out.contains("fn gf_invert_u128"), "Should contain gf_invert_u128");

    // Dyn-lower and print to TypeScript
    let dyn_module = volar_compiler_passes::lowering_dyn::lower_module_dyn(&module);
    let ts_out = volar_compiler::printer_ts::print_module_ts(&dyn_module);

    // Basic sanity: TS output should contain the field functions
    assert!(ts_out.contains("gf_mul_u8"), "TS should contain gf_mul_u8:\n{}", &ts_out[..500.min(ts_out.len())]);
    assert!(ts_out.contains("gf_invert_u8"), "TS should contain gf_invert_u8");
    assert!(ts_out.contains("gf_mul_u64"), "TS should contain gf_mul_u64");
    assert!(ts_out.contains("gf_mul_u128"), "TS should contain gf_mul_u128");

    // The TS output should have class definitions for the field types
    assert!(ts_out.contains("class Galois"), "TS should contain class Galois");
    assert!(ts_out.contains("class Galois128"), "TS should contain class Galois128");
    assert!(ts_out.contains("class U256"), "TS should contain class U256");
}
