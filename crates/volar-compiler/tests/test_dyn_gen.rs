use std::fs;
use std::path::Path;
use volar_compiler::{parser::parse_source, printer_dyn::print_module_rust_dyn};

/// Test that generates dynamic code and writes it to volar-spec-dyn for compilation
#[test]
fn test_generate_dyn_module() -> Result<(), Box<dyn std::error::Error>> {
    let spec_dir = Path::new("../volar-spec/src");
    let mut files = Vec::new();
    collect_files(spec_dir, &mut files)?;

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
                combined_ir.uses.extend(module.uses);
            }
            Err(e) => {
                eprintln!("Warning: Error parsing {:?}: {}", file, e);
            }
        }
    }

    let dyn_code = print_module_rust_dyn(&combined_ir);

    // Write to volar-spec-dyn/src/generated.rs
    let generated_path = Path::new("../volar-spec-dyn/src/generated.rs");
    fs::write(&generated_path, &dyn_code)?;

    println!("Generated {} bytes of dynamic code", dyn_code.len());
    println!("Written to: {:?}", generated_path);

    // Also write a standalone copy for debugging
    let debug_path = Path::new("volar_dyn_generated.rs");
    fs::write(&debug_path, &dyn_code)?;

    Ok(())
}

/// Test that the generated code compiles with the volar-spec-dyn crate
#[test]
fn test_generated_dyn_compiles() -> Result<(), Box<dyn std::error::Error>> {
    // First generate the code
    test_generate_dyn_module()?;

    // Now try to build volar-spec-dyn
    let output = std::process::Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("volar-spec-dyn")
        .arg("--features")
        .arg("generated")
        .current_dir("../..") // workspace root
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    eprintln!("Build output:\n{}", stdout);
    eprintln!("Build errors:\n{}", stderr);
    if !output.status.success() {
        return Err(format!("volar-spec-dyn build failed").into());
    }

    println!("volar-spec-dyn compiled successfully!");
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
