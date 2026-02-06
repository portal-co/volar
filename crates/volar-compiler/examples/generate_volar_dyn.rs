//! Standalone tool to generate dynamic (runtime-sized) Rust code from volar-spec.
//!
//! Usage:
//!   cargo run --example generate_volar_dyn --features parsing [-- [SPEC_DIR] [OUTPUT_PATH]]
//!
//! Defaults:
//!   SPEC_DIR   = ../../crates/volar-spec/src
//!   OUTPUT_PATH = ../../crates/volar-spec-dyn/src/generated.rs

use std::fs;
use std::path::{Path, PathBuf};
use volar_compiler::{parser::parse_source, print_module_rust_dyn};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();

    let spec_dir = args
        .get(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("../../crates/volar-spec/src"));
    let output_path = args
        .get(2)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("../../crates/volar-spec-dyn/src/generated.rs"));

    eprintln!("Reading volar-spec from: {:?}", spec_dir);
    eprintln!("Writing output to: {:?}", output_path);

    let mut files = Vec::new();
    collect_files(&spec_dir, &mut files)?;

    // NOTE: volar-common is excluded temporarily to avoid duplicate definitions
    // (commit, CommitmentCore) while we focus on the main volar-spec lowering logic.
    // let common_dir = spec_dir.parent().unwrap().parent().unwrap().join("volar-common/src");
    // if common_dir.is_dir() {
    //     collect_files(&common_dir, &mut files)?;
    //     eprintln!("Including volar-common from: {:?}", common_dir);
    // }
    eprintln!("Found {} source files", files.len());

    let mut combined_ir = volar_compiler::IrModule {
        name: "volar_dyn".to_string(),
        ..Default::default()
    };

    let mut parse_errors = 0;
    for file in &files {
        let content = fs::read_to_string(file)?;
        let name = file.file_stem().unwrap().to_str().unwrap();
        match parse_source(&content, name) {
            Ok(module) => {
                eprintln!("  Parsed {:?}: {} structs, {} impls, {} fns",
                    file.file_name().unwrap(),
                    module.structs.len(),
                    module.impls.len(),
                    module.functions.len(),
                );
                combined_ir.structs.extend(module.structs);
                combined_ir.traits.extend(module.traits);
                combined_ir.impls.extend(module.impls);
                combined_ir.functions.extend(module.functions);
                combined_ir.type_aliases.extend(module.type_aliases);
            }
            Err(e) => {
                eprintln!("  Error parsing {:?}: {}", file.file_name().unwrap(), e);
                parse_errors += 1;
            }
        }
    }

    eprintln!(
        "Combined IR: {} structs, {} traits, {} impls, {} fns ({} parse errors)",
        combined_ir.structs.len(),
        combined_ir.traits.len(),
        combined_ir.impls.len(),
        combined_ir.functions.len(),
        parse_errors,
    );

    let dyn_code = print_module_rust_dyn(&combined_ir);

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&output_path, &dyn_code)?;
    eprintln!("Generated {} bytes → {:?}", dyn_code.len(), output_path);

    // Also write a debug copy alongside the compiler crate
    let debug_path = Path::new("volar_dyn_generated.rs");
    fs::write(debug_path, &dyn_code)?;
    eprintln!("Debug copy → {:?}", debug_path);

    Ok(())
}

fn collect_files(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
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
