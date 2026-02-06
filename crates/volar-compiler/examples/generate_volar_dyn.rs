use std::fs;
use std::path::Path;
use volar_compiler::{parser::parse_source, print_module_rust_dyn};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let spec_dir = Path::new("../../crates/volar-spec/src");
    let mut files = Vec::new();
    collect_files(spec_dir, &mut files)?;

    println!("Found {} source files in volar-spec", files.len());

    let mut combined_ir = volar_compiler::IrModule {
        name: "volar_dyn".to_string(),
        ..Default::default()
    };

    for file in files {
        let content = fs::read_to_string(&file)?;
        let name = file.file_stem().unwrap().to_str().unwrap();
        match parse_source(&content, name) {
            Ok(module) => {
                combined_ir.structs.extend(module.structs);
                combined_ir.traits.extend(module.traits);
                combined_ir.impls.extend(module.impls);
                combined_ir.functions.extend(module.functions);
                combined_ir.type_aliases.extend(module.type_aliases);
                // combined_ir.uses.extend(module.uses);
            }
            Err(e) => {
                eprintln!("Error parsing {:?}: {}", file, e);
            }
        }
    }

    let dyn_code = print_module_rust_dyn(&combined_ir);
    
    // In a real scenario, we'd write to the actual files in volar-dyn.
    // Here we print a sample of the generated code for verification.
    println!("=== Generated volar-dyn code sample ===");
    println!("{}", &dyn_code[..2000]);
    
    // Optionally write to a temporary file
    fs::write("volar_dyn_generated.rs", dyn_code)?;
    println!("Full generated code written to volar_dyn_generated.rs");

    Ok(())
}

fn collect_files(dir: &Path, files: &mut Vec<std::path::PathBuf>) -> std::io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                collect_files(&path, files)?;
            } else if path.extension().map_or(false, |s| s == "rs") {
                files.push(path);
            }
        }
    }
    Ok(())
}
