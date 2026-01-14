use std::fs;
use std::path::Path;
use volar_compiler::{parser::parse_source, printer_dyn::print_module_rust_dyn};

#[test]
fn test_generated_dyn_compiles() -> Result<(), Box<dyn std::error::Error>> {
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
                eprintln!("Error parsing {:?}: {}", file, e);
            }
        }
    }

    let dyn_code = print_module_rust_dyn(&combined_ir);
    
    // Write to a temporary file in the target directory for compilation check
    let out_dir = Path::new("../../target/debug");
    let test_file = out_dir.join("volar_dyn_test.rs");
    fs::create_dir_all(out_dir)?;
    fs::write(&test_file, dyn_code)?;

    // Use rustc to check if it compiles
    let output = std::process::Command::new("rustc")
        .arg("--crate-type")
        .arg("lib")
        .arg("--emit")
        .arg("dep-info,metadata")
        .arg("-o")
        .arg(out_dir.join("volar_dyn_test.rmeta"))
        .arg(&test_file)
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("Compilation failed:\n{}", stderr);
        return Err(format!("Compilation failed").into());
    }

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
