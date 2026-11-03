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

use volar_compiler::{IrFunction, IrModule, SourceInput, parse_sources};
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

/// Render the garbled-circuit spec subset as native Lean (voof's "cleaner
/// representation"; proven equivalent to the Aeneas extraction there).
fn print_garble_lean(spec_module: &volar_compiler::IrModule<volar_compiler::IrFunction>) -> String {
    use volar_compiler::printer_lean::print_module_lean;
    let only: std::collections::BTreeSet<String> = [
        "Eval",
        "Garble",
        "GarbleTable",
        "GlobalSecret",
        "absorb_args_bases",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    print_module_lean(spec_module, &only).expect("Lean generation for garble subset")
}

/// Render the garbled-circuit spec subset's Aeneas↔native bridge file
/// (transports + commutation theorems; consumed by voof's
/// `VolarSpecBridge.Generated` via `prepare-native.sh`).
fn print_garble_lean_bridge(
    spec_module: &volar_compiler::IrModule<volar_compiler::IrFunction>,
) -> String {
    use volar_compiler::printer_lean_bridge::print_module_lean_bridge;
    let only: std::collections::BTreeSet<String> = [
        "Eval",
        "Garble",
        "GarbleTable",
        "GlobalSecret",
        "absorb_args_bases",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    print_module_lean_bridge(spec_module, &only, "garble")
        .expect("Lean bridge generation for garble subset")
}

fn gen_specs(check_only: bool) {
    let workspace = workspace_root();

    // ── Parse each spec crate with its own namespace path ──────────────────
    let primitives_src_dir = workspace.join("crates/spec/volar-primitives/src");
    let common_src_dir = workspace.join("crates/spec/volar-common/src");
    let spec_src_dir = workspace.join("crates/spec/volar-spec/src");

    let primitives_sources = collect_rs_files(&primitives_src_dir);
    let common_sources = collect_rs_files(&common_src_dir);
    let spec_sources = collect_rs_files(&spec_src_dir);

    let prim_path = vec!["volar_primitives".to_string()];
    let common_path = vec!["volar_common".to_string()];
    let spec_path = vec!["volar_spec".to_string()];

    let prim_inputs: Vec<SourceInput<'_>> = primitives_sources
        .iter()
        .map(|(c, n)| SourceInput {
            source: c.as_str(),
            name: n.as_str(),
        })
        .collect();
    let common_inputs: Vec<SourceInput<'_>> = common_sources
        .iter()
        .map(|(c, n)| SourceInput {
            source: c.as_str(),
            name: n.as_str(),
        })
        .collect();
    let spec_inputs: Vec<SourceInput<'_>> = spec_sources
        .iter()
        .map(|(c, n)| SourceInput {
            source: c.as_str(),
            name: n.as_str(),
        })
        .collect();

    let prim_module = parse_sources(&prim_inputs, "volar_primitives", &prim_path)
        .expect("parse volar-primitives");
    let common_module =
        parse_sources(&common_inputs, "volar_common", &common_path).expect("parse volar-common");
    let spec_module =
        parse_sources(&spec_inputs, "volar_spec", &spec_path).expect("parse volar-spec");

    let garble_lean = print_garble_lean(&spec_module);
    let garble_lean_bridge = print_garble_lean_bridge(&spec_module);
    let combined_module = merge_modules(vec![prim_module, common_module, spec_module]);

    // ── mxx-sourced spec elements (docs/mxx-cutover-plan.md Phase 1) ───────
    // The mxx_rt helpers and every registered element program are lowered
    // from validated frozen mxx graphs; both are freshness-gated here like
    // every other generated artifact.
    let mut generated: Vec<(PathBuf, String)> = vec![
        (
            workspace.join("packages/volar-spec-ts/generated.ts"),
            print_module_typescript(&combined_module),
        ),
        (
            workspace.join("crates/compiler/volar-compiler/volar_ts_generated.ts"),
            print_module_typescript(&combined_module),
        ),
        (
            workspace.join("crates/compiler/volar-compiler/volar_dyn_generated.rs"),
            print_module_rust_dyn(&combined_module),
        ),
        (
            workspace.join("crates/spec/volar-spec-lean/volar_lean_generated.lean"),
            garble_lean,
        ),
        (
            workspace.join("crates/spec/volar-spec-lean/volar_lean_bridge_generated.lean"),
            garble_lean_bridge,
        ),
        (
            workspace.join("crates/spec/volar-spec-dyn/src/generated.rs"),
            print_module_rust_dyn(&combined_module),
        ),
    ];
    generated.push((
        workspace.join("crates/spec/volar-spec/src/mxx_rt.rs"),
        volar_mxx_interop::generate::generate_rt_source(),
    ));
    for element in volar_mxx_interop::programs::elements() {
        let src = volar_mxx_interop::generate::generate_element_rust(&element)
            .unwrap_or_else(|e| panic!("mxx element `{}`: {e}", element.element));
        generated.push((
            workspace.join(format!("crates/spec/volar-spec/src/mxx_{}.rs", element.element)),
            src,
        ));
    }

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

/// Merge multiple `IrModule`s into one flat module by extending all item lists.
/// The merged module takes its name from the last module in the list.
fn merge_modules(modules: Vec<IrModule<IrFunction>>) -> IrModule<IrFunction> {
    let mut out = IrModule::default();
    for m in modules {
        if out.name.is_empty() {
            out.name = m.name.clone();
        } else {
            out.name = m.name.clone(); // last wins for module name
        }
        out.structs.extend(m.structs);
        out.enums.extend(m.enums);
        out.traits.extend(m.traits);
        out.impls.extend(m.impls);
        out.functions.extend(m.functions);
        out.type_aliases.extend(m.type_aliases);
        out.consts.extend(m.consts);
    }
    out
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

const CODEGEN_EXCLUDE_MARKER: &str = "//! @volar-codegen-exclude:";

fn is_codegen_excluded(source: &str) -> bool {
    source
        .lines()
        .any(|line| line.trim_start().starts_with(CODEGEN_EXCLUDE_MARKER))
}

fn collect_rs_files_inner(dir: &Path, out: &mut Vec<(String, String)>) {
    if let Ok(entries) = fs::read_dir(dir) {
        // Sort: read_dir order is filesystem-dependent (differs between
        // macOS APFS and Linux ext4), which would make generated output
        // platform-dependent.
        let mut entries: Vec<_> = entries.flatten().collect();
        entries.sort_by_key(|e| e.file_name());
        for entry in entries {
            let path = entry.path();
            if path.is_file() && path.extension().map_or(false, |e| e == "rs") {
                if let Ok(content) = fs::read_to_string(&path) {
                    if is_codegen_excluded(&content) {
                        continue;
                    }
                    let name = path.file_stem().unwrap().to_string_lossy().to_string();
                    out.push((content, name));
                }
            } else if path.is_dir() {
                collect_rs_files_inner(&path, out);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::is_codegen_excluded;

    #[test]
    fn codegen_exclusion_requires_an_explicit_module_doc_marker() {
        assert!(is_codegen_excluded(
            "//! Module documentation.\n//! @volar-codegen-exclude: target-specific implementation."
        ));
        assert!(!is_codegen_excluded("//! Ordinary generated spec module."));
        assert!(!is_codegen_excluded(
            "// @volar-codegen-exclude: not a module-level documentation marker"
        ));
    }
}
