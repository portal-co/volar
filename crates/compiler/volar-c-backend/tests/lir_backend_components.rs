//! Component-level LIR codegen tests using seeded lazy emission.
//!
//! Each test lowers only the functions reachable from a small set of seeds,
//! so one failing component doesn't block others.  All tests are expected to
//! fail initially; they turn green as panics are fixed component by component.

use std::fs;
use std::path::{Path, PathBuf};

use volar_c_backend::CBackend;
use volar_compiler::{ir::IrFunction, ir::IrModule, parser::parse_source};
use volar_lir_codegen::{lower_module_seeded, mono::MonoEnv};

// ---------------------------------------------------------------------------
// Shared helpers (mirrors lir_backend.rs)
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
            Err(e) => eprintln!(
                "  warn: parse error in {}: {e}",
                file.file_name().unwrap().to_string_lossy()
            ),
        }
    }
}

fn build_module() -> IrModule<IrFunction> {
    let root = workspace_root();
    let mut module = IrModule {
        name: "volar_lir_components".to_string(),
        ..Default::default()
    };
    parse_dir(&root.join("crates/spec/volar-primitives/src"), "volar_primitives", &mut module);
    parse_dir(&root.join("crates/spec/volar-common/src"), "volar_common", &mut module);
    parse_dir(&root.join("crates/spec/volar-spec/src"), "volar_spec", &mut module);
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

/// Lower only the functions reachable from `seeds` and assert no panic.
fn run_lir_component(seeds: &[&str], label: &str) {
    let module = build_module();

    let seeds_owned: Vec<String> = seeds.iter().map(|s| s.to_string()).collect();
    let result = std::panic::catch_unwind(move || {
        let mut backend = CBackend::new();
        lower_module_seeded(&module, &mut backend, &MonoEnv::new("volar_lir_components"), &seeds_owned.iter().map(|s| s.as_str()).collect::<Vec<_>>());
        backend.finish()
    });

    match result {
        Ok(c_output) => {
            eprintln!("\n=== LIR component [{}]: OK ({} bytes) ===", label, c_output.len());
        }
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "(non-string panic payload)".to_string()
            };
            eprintln!("\n=== LIR component [{}]: FAILED ===\n  {}", label, msg);
            panic!("LIR backend panicked for component '{}': {}", label, msg);
        }
    }
}

// ---------------------------------------------------------------------------
// Component test table
// ---------------------------------------------------------------------------

macro_rules! lir_component {
    ($($name:ident: [$($seed:literal),+ $(,)?]),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                run_lir_component(&[$($seed),+], stringify!($name));
            }
        )*
    }
}

lir_component! {
    vole_prover: [
        "vole_and_prover_step",
        "vole_sbox_prover_step",
        "vole_mul3_prover_step",
    ],
    vole_verifier: [
        "vole_and_verifier_check",
        "vole_sbox_verifier_check",
        "vole_mul3_verifier_check",
    ],
    vole_setup: [
        "random_nonzero_delta",
        "vole_commit_bit",
        "derive_and_q",
    ],
    tfhe: [
        "tfhe_xor",
        "tfhe_not",
        "tfhe_gate_bootstrapping_and",
        "tfhe_cmux",
    ],
    faest_core: [
        "sign",
        "verify",
        "keygen",
    ],
}
