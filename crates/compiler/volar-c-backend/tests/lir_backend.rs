//! Integration test: full LIR codegen pipeline for volar-spec.
//!
//! Reads volar-primitives, volar-common, and volar-spec sources, runs them
//! through `lower_module_with_opts` + `CBackend::finish()`, and verifies that
//! lowering completes without panicking and produces non-empty C output.
//!
//! Each function that fails to lower is logged so that gaps can be tracked.

use std::fs;
use std::path::{Path, PathBuf};

use volar_c_backend::CBackend;
use volar_compiler::{ir::IrFunction, ir::IrModule, parser::parse_source};
use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

// ---------------------------------------------------------------------------
// Source collection
// ---------------------------------------------------------------------------

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn collect_rs_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let p = entry.path();
                if p.is_dir() {
                    walk(&p, out);
                } else if p.extension().map_or(false, |e| e == "rs") {
                    out.push(p);
                }
            }
        }
    }
    walk(dir, &mut out);
    out.sort();
    out
}

fn parse_dir(dir: &Path, crate_name: &str, module: &mut IrModule<IrFunction>) {
    for file in collect_rs_files(dir) {
        let src = match fs::read_to_string(&file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("  warn: cannot read {}: {e}", file.display());
                continue;
            }
        };
        let stem = file.file_stem().unwrap().to_string_lossy().to_string();
        let module_path = vec![crate_name.to_string(), stem.clone()];
        match parse_source(&src, &stem, &module_path) {
            Ok(m) => {
                module.structs.extend(m.structs);
                module.enums.extend(m.enums);
                module.traits.extend(m.traits);
                module.impls.extend(m.impls);
                module.functions.extend(m.functions);
                module.type_aliases.extend(m.type_aliases);
                module.consts.extend(m.consts);
            }
            Err(e) => eprintln!("  warn: parse error in {}: {e}", file.file_name().unwrap().to_string_lossy()),
        }
    }
}

fn build_module() -> IrModule<IrFunction> {
    let root = workspace_root();
    let mut module = IrModule {
        name: "volar_lir_test".to_string(),
        ..Default::default()
    };

    parse_dir(&root.join("crates/spec/volar-primitives/src"), "volar_primitives", &mut module);
    parse_dir(&root.join("crates/spec/volar-common/src"), "volar_common", &mut module);
    parse_dir(&root.join("crates/spec/volar-spec/src"), "volar_spec", &mut module);

    // Dedup: within the same crate, keep first by bare name.
    {
        let mut seen = std::collections::HashSet::new();
        module.structs.retain(|s| {
            let cn = s.module_path.first().cloned().unwrap_or_default();
            seen.insert((cn, s.kind.to_string()))
        });
    }
    {
        let mut seen = std::collections::HashSet::new();
        module.functions.retain(|f| {
            let cn = f.module_path.first().cloned().unwrap_or_default();
            seen.insert((cn, f.name.clone()))
        });
    }
    {
        let mut seen = std::collections::HashSet::new();
        module.consts.retain(|c| {
            let cn = c.module_path.first().cloned().unwrap_or_default();
            seen.insert((cn, c.name.clone()))
        });
    }

    module
}

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

#[test]
fn test_lir_backend_no_errors() {
    let module = build_module();

    eprintln!(
        "\n=== LIR backend: lowering {} functions from {} structs ===\n",
        module.functions.len(),
        module.structs.len()
    );

    // Catch panics from lower_module_with_opts so we can report them all.
    let result = std::panic::catch_unwind(|| {
        let mut backend = CBackend::new();
        lower_module_with_opts(&module, &mut backend, &MonoEnv::new("volar_lir_test"));
        backend.finish()
    });

    match result {
        Ok(c_output) => {
            eprintln!("LIR lowering succeeded. C output: {} bytes", c_output.len());
            assert!(!c_output.is_empty(), "CBackend produced empty output");
        }
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "(non-string panic payload)".to_string()
            };
            panic!("LIR backend panicked: {}", msg);
        }
    }
}
