//! Example demonstrating parsing volar-spec and printing the IR.

use std::fs;
use std::path::Path;
use volar_compiler::{
    parse_sources, print_module, specialize_module,
    TypeContext, OperatorAnalysis, type_to_string,
    StructKind, TraitKind, MathTrait, SpecType, ArrayKind,
};

fn main() {
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
                if path.is_file() && path.extension().is_some_and(|e| e == "rs") {
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

    println!("Found {} source files in volar-spec", sources.len());
    
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(content, name)| (content.as_str(), name.as_str()))
        .collect();

    match parse_sources(&sources_ref, "volar_spec") {
        Ok(module) => {
            println!("\n=== Generic IR Module Statistics ===");
            println!("Structs: {}", module.structs.len());
            println!("Traits: {}", module.traits.len());
            println!("Impls: {}", module.impls.len());
            println!("Functions: {}", module.functions.len());
            println!("Type aliases: {}", module.type_aliases.len());

            // Analyze operators
            let op_analysis = OperatorAnalysis::from_module(&module);
            println!("\n=== Operator Implementations ===");
            for ((trait_name, self_ty, rhs_ty), output_ty) in &op_analysis.binary_ops {
                println!(
                    "  {} {} {} = {}",
                    self_ty,
                    trait_name,
                    rhs_ty,
                    type_to_string(output_ty)
                );
            }

            // Build type context
            let type_ctx = TypeContext::from_module(&module);
            println!("\n=== Type Context ===");
            println!("Known structs: {}", type_ctx.structs.len());
            println!("Trait implementations: {}", type_ctx.trait_impls.len());
            println!("Associated types: {}", type_ctx.assoc_types.len());

            // Now specialize the IR
            println!("\n=== Specializing IR ===");
            let spec_module = specialize_module(&module);
            
            println!("\n=== Specialized Struct Classifications ===");
            for s in &spec_module.structs {
                let classification = match &s.kind {
                    StructKind::Delta | StructKind::Q | StructKind::Vope | StructKind::BitVole => "VOLE",
                    StructKind::ABO | StructKind::ABOOpening | StructKind::CommitmentCore => "Crypto",
                    StructKind::Poly | StructKind::PolyInputPool => "Polynomial",
                    _ => "Other",
                };
                println!("  {:?}: {} ({} fields)", s.kind, classification, s.fields.len());
                
                // Show field types
                for field in &s.fields {
                    let ty_desc = match &field.ty {
                        SpecType::Primitive(p) => format!("primitive {:?}", p),
                        SpecType::Array { kind: ArrayKind::GenericArray, .. } => "GenericArray".to_string(),
                        SpecType::Array { kind: ArrayKind::FixedArray, .. } => "fixed array".to_string(),
                        SpecType::TypeParam(name) => format!("type param {}", name),
                        SpecType::Struct { kind, .. } => format!("struct {:?}", kind),
                        _ => "other".to_string(),
                    };
                    println!("    {}: {}", field.name, ty_desc);
                }
            }
            
            println!("\n=== Specialized Trait Impl Classifications ===");
            let mut math_count = 0;
            let mut crypto_count = 0;
            let mut inherent_count = 0;
            
            for imp in &spec_module.impls {
                match &imp.trait_ {
                    Some(tr) => match &tr.kind {
                        TraitKind::Math(m) => {
                            math_count += 1;
                            if matches!(m, MathTrait::Add | MathTrait::Sub | MathTrait::Mul | MathTrait::BitXor) {
                                println!("  Math op: {:?} for {:?}", m, imp.self_ty.as_struct());
                            }
                        }
                        TraitKind::Crypto(c) => {
                            crypto_count += 1;
                            println!("  Crypto: {:?} for {:?}", c, imp.self_ty.as_struct());
                        }
                        _ => {}
                    },
                    None => {
                        inherent_count += 1;
                    }
                }
            }
            
            println!("\n  Math trait impls: {}", math_count);
            println!("  Crypto trait impls: {}", crypto_count);
            println!("  Inherent impls: {}", inherent_count);

            // Print the generic IR (truncated)
            println!("\n=== Generic IR Output (truncated) ===");
            let printed = print_module(&module);
            
            if printed.len() > 3000 {
                println!("{}...\n", &printed[..3000]);
            } else {
                println!("{}", printed);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse: {}", e);
            std::process::exit(1);
        }
    }
}
