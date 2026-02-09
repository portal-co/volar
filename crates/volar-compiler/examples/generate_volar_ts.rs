//! Standalone tool to generate TypeScript code from volar-spec.
//!
//! Usage:
//!   cargo run --example generate_volar_ts --features parsing [-- [SPEC_DIR] [OUTPUT_PATH]]
//!
//! Defaults:
//!   SPEC_DIR   = ../../crates/volar-spec/src
//!   OUTPUT_PATH = ../../packages/volar-runtime/src/generated.ts

use std::fs;
use std::path::{Path, PathBuf};
use volar_compiler::{parser::parse_source, print_module_typescript};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();

    // Check for --dump-ir or --dump-ir-dyn flags
    let dump_ir = args.iter().any(|a| a == "--dump-ir");
    let dump_ir_dyn = args.iter().any(|a| a == "--dump-ir-dyn");
    let positional: Vec<&String> = args[1..].iter().filter(|a| !a.starts_with("--")).collect();

    let spec_dir = positional
        .first()
        .map(|s| PathBuf::from(s.as_str()))
        .unwrap_or_else(|| PathBuf::from("../../crates/volar-spec/src"));
    let output_path = positional
        .get(1)
        .map(|s| PathBuf::from(s.as_str()))
        .unwrap_or_else(|| PathBuf::from("../../packages/volar-runtime/src/generated.ts"));

    eprintln!("Reading volar-spec from: {:?}", spec_dir);
    eprintln!("Writing output to: {:?}", output_path);

    let mut files = Vec::new();
    collect_files(&spec_dir, &mut files)?;
    eprintln!("Found {} source files", files.len());

    let mut combined_ir = volar_compiler::IrModule {
        name: "volar_ts".to_string(),
        ..Default::default()
    };

    let mut parse_errors = 0;
    for file in &files {
        let content = fs::read_to_string(file)?;
        let name = file.file_stem().unwrap().to_str().unwrap();
        match parse_source(&content, name) {
            Ok(module) => {
                eprintln!(
                    "  Parsed {:?}: {} structs, {} impls, {} fns",
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

    // --dump-ir: dump the parsed (pre-lowering) IR
    if dump_ir {
        let dump = volar_compiler::dump_ir::dump_module(&combined_ir);
        let dump_path = Path::new("ir_dump.txt");
        fs::write(dump_path, &dump)?;
        eprintln!("IR dump ({} bytes) → {:?}", dump.len(), dump_path);
    }

    let ts_code = print_module_typescript(&combined_ir);

    // --dump-ir-dyn: dump the dyn-lowered IR (post-lowering, pre-TS-printing)
    if dump_ir_dyn {
        let lowered = volar_compiler::lowering_dyn::lower_module_dyn(&combined_ir);
        let dump = volar_compiler::dump_ir::dump_module(&lowered);
        let dump_path = Path::new("ir_dump_dyn.txt");
        fs::write(dump_path, &dump)?;
        eprintln!(
            "Dyn-lowered IR dump ({} bytes) → {:?}",
            dump.len(),
            dump_path
        );
    }

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&output_path, &ts_code)?;
    eprintln!("Generated {} bytes → {:?}", ts_code.len(), output_path);

    // Also write a debug copy alongside the compiler crate
    let debug_path = Path::new("volar_ts_generated.ts");
    fs::write(debug_path, &ts_code)?;
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
