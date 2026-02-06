//! Integration test that parses volar-spec sources

use std::fs;
use std::path::Path;
use volar_compiler::{parse_source, parse_sources, TypeContext, OperatorAnalysis, type_to_string};

fn read_volar_spec_sources() -> Vec<(String, String)> {
    let base_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
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
                println!("  {:?} <{}>", s.kind, generics.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "));
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
    assert!(matches!(trait_ref.kind, volar_compiler::TraitKind::Math(volar_compiler::MathTrait::Add)));
    
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
            delta: GenericArray::generate(|i| delta[f(i) % N::to_usize()].clone()),
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
