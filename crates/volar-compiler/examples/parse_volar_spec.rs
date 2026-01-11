//! Example demonstrating parsing volar-spec and printing the IR.

use std::fs;
use std::path::Path;
use volar_compiler::{parse_sources, print_module, TypeContext, OperatorAnalysis, type_to_string};

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
            println!("\n=== Module Statistics ===");
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

            // Print the full IR
            println!("\n=== IR Output ===");
            let printed = print_module(&module);
            
            // Just print first 5000 chars to avoid overwhelming output
            if printed.len() > 5000 {
                println!("{}...\n\n(truncated, showing first 5000 chars)", &printed[..5000]);
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
