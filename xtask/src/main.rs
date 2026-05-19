//! # volar xtask
//!
//! Build task runner invoked via `cargo xtask <subcommand>`.
//!
//! ## Subcommands
//!
//! - `gen-specs`   — regenerate all derived spec files and write them to disk
//! - `check-specs` — regenerate in memory, diff against checked-in files, exit
//!                   nonzero if any file is stale

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use volar_compiler::{SourceInput, parse_sources};
use volar_compiler_passes::{print_module_rust_dyn, print_module_typescript};

fn main() {
    let args: Vec<String> = env::args().collect();
    let subcommand = args.get(1).map(|s| s.as_str()).unwrap_or("help");

    match subcommand {
        "gen-specs" => gen_specs(false),
        "check-specs" => gen_specs(true),
        _ => {
            eprintln!("Usage: cargo xtask <subcommand>");
            eprintln!("Subcommands:");
            eprintln!("  gen-specs    Regenerate all derived spec files");
            eprintln!("  check-specs  Check that derived files match; exit 1 if stale");
            process::exit(1);
        }
    }
}

// ============================================================================
// GEN-SPECS / CHECK-SPECS
// ============================================================================

fn gen_specs(check_only: bool) {
    let workspace = workspace_root();

    // ── Parse spec sources ──────────────────────────────────────────────────
    let primitives_src_dir = workspace.join("crates/spec/volar-primitives/src");
    let spec_src_dir = workspace.join("crates/spec/volar-spec/src");

    let primitives_sources = collect_rs_files(&primitives_src_dir);
    let spec_sources = collect_rs_files(&spec_src_dir);

    // Parse primitives as a standalone module.
    let prim_inputs: Vec<SourceInput<'_>> = primitives_sources
        .iter()
        .map(|(content, name)| SourceInput { source: content.as_str(), name: name.as_str() })
        .collect();
    // Parse volar-spec (merged with primitives so it can reference primitive types).
    let mut all_inputs: Vec<SourceInput<'_>> = prim_inputs.clone();
    let spec_inputs: Vec<SourceInput<'_>> = spec_sources
        .iter()
        .map(|(content, name)| SourceInput { source: content.as_str(), name: name.as_str() })
        .collect();
    all_inputs.extend(spec_inputs.iter().cloned());
    let combined_module = parse_sources(&all_inputs, "volar_spec")
        .expect("parse volar-spec + volar-primitives");

    // ── Generate outputs ────────────────────────────────────────────────────
    let generated = [
        (
            workspace.join("crates/compiler/volar-compiler/volar_ts_generated.ts"),
            print_module_typescript(&combined_module),
        ),
        (
            workspace.join("crates/compiler/volar-compiler/volar_dyn_generated.rs"),
            print_module_rust_dyn(&combined_module),
        ),
        (
            workspace.join("crates/spec/volar-spec-dyn/src/generated.rs"),
            print_module_rust_dyn(&combined_module),
        ),
    ];

    let mut stale = false;
    for (path, new_content) in &generated {
        if check_only {
            match fs::read_to_string(path) {
                Ok(existing) if existing == *new_content => {
                    println!("  ok   {}", path.display());
                }
                Ok(_) => {
                    eprintln!("STALE {}", path.display());
                    stale = true;
                }
                Err(e) => {
                    eprintln!("ERROR reading {}: {}", path.display(), e);
                    stale = true;
                }
            }
        } else {
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("create parent dir");
            }
            fs::write(path, new_content)
                .unwrap_or_else(|e| panic!("write {}: {}", path.display(), e));
            println!("wrote {}", path.display());
        }
    }

    if check_only && stale {
        eprintln!("\nStale generated files detected. Run `cargo xtask gen-specs` to regenerate.");
        process::exit(1);
    }
}

// ============================================================================
// HELPERS
// ============================================================================

fn workspace_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is xtask/, one level below workspace root.
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest_dir)
        .parent()
        .expect("xtask must be inside workspace root")
        .to_path_buf()
}

fn collect_rs_files(dir: &Path) -> Vec<(String, String)> {
    let mut sources = Vec::new();
    collect_rs_files_inner(dir, &mut sources);
    sources
}

fn collect_rs_files_inner(dir: &Path, out: &mut Vec<(String, String)>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().map_or(false, |e| e == "rs") {
                if let Ok(content) = fs::read_to_string(&path) {
                    let name = path.file_stem().unwrap().to_string_lossy().to_string();
                    out.push((content, name));
                }
            } else if path.is_dir() {
                collect_rs_files_inner(&path, out);
            }
        }
    }
}
