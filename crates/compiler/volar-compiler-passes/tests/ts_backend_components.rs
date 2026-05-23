//! Component-level TS codegen tests using seeded lazy emission.
//!
//! Each test compiles only the functions reachable from a small set of seeds,
//! so one failing component doesn't block others.  All tests are expected to
//! fail initially; they turn green as errors are fixed component by component.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use volar_compiler::{ir::IrFunction, ir::IrModule, parser::parse_source};
use volar_compiler_passes::print_module_typescript_seeded;

// ---------------------------------------------------------------------------
// Shared helpers (mirrors ts_backend.rs)
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
        name: "volar_ts_components".to_string(),
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

fn find_tsc() -> Option<PathBuf> {
    let root = workspace_root();
    let candidate = root.join("node_modules/.bin/tsc");
    if candidate.exists() {
        return Some(candidate);
    }
    let out = Command::new("which").arg("tsc").output().ok()?;
    if out.status.success() {
        let p = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if !p.is_empty() {
            return Some(PathBuf::from(p));
        }
    }
    None
}

/// Run tsc on the seeded TS output for `seeds`, label the output with `label`.
fn run_ts_component(seeds: &[&str], label: &str) {
    let module = build_module();
    let ts_code = print_module_typescript_seeded(&module, seeds);

    let stripped: String = ts_code
        .lines()
        .filter(|l| !l.trim_start().starts_with("// @ts-nocheck"))
        .collect::<Vec<_>>()
        .join("\n");

    let root = workspace_root();
    let runtime_src = root.join("packages/volar-runtime/src");
    let tmp_path = runtime_src.join(format!(".generated_component_{}.ts", label));
    fs::write(&tmp_path, &stripped).expect("write temp TS file");

    let tsc = match find_tsc() {
        Some(p) => p,
        None => {
            let _ = fs::remove_file(&tmp_path);
            panic!("tsc not found — install TypeScript or add node_modules/.bin to PATH");
        }
    };

    let output = Command::new(&tsc)
        .args([
            "--noEmit",
            "--strict",
            "--moduleResolution",
            "bundler",
            "--target",
            "esnext",
            "--module",
            "esnext",
            tmp_path.to_str().unwrap(),
        ])
        .current_dir(&root.join("packages/volar-runtime"))
        .output()
        .expect("tsc command failed to launch");

    let _ = fs::remove_file(&tmp_path);

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{stdout}{stderr}");

    let mut by_code: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut total = 0usize;
    for line in combined.lines() {
        if let Some(pos) = line.find("error TS") {
            let code_start = pos + "error ".len();
            let rest = &line[code_start..];
            let code: String = rest.chars().take_while(|c| !c.is_whitespace() && *c != ':').collect();
            by_code.entry(code).or_default().push(line.to_string());
            total += 1;
        }
    }

    if total > 0 {
        eprintln!("\n=== TS component [{}]: {} error(s) ===\n", label, total);
        for (code, lines) in &by_code {
            eprintln!("--- {} ({} occurrence(s)) ---", code, lines.len());
            for l in lines {
                eprintln!("  {}", l);
            }
        }
        eprintln!("\n=== Error code summary ===");
        for (code, lines) in &by_code {
            eprintln!("  {}: {}", code, lines.len());
        }
        eprintln!("  TOTAL: {}", total);
    } else {
        eprintln!("\n=== TS component [{}]: OK ===", label);
    }

    assert_eq!(
        total, 0,
        "tsc --strict reported {} error(s) for component '{}'; see above",
        total, label
    );
}

// ---------------------------------------------------------------------------
// Component test table
// ---------------------------------------------------------------------------

macro_rules! ts_component {
    ($($name:ident: [$($seed:literal),+ $(,)?]),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                run_ts_component(&[$($seed),+], stringify!($name));
            }
        )*
    }
}

ts_component! {
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
        "tfhe_gate_bootstrapping_or",
        "tfhe_cmux",
        "tfhe_programmable_bootstrap",
        "tfhe_lut_read",
    ],
    faest_core: [
        "sign",
        "verify",
        "keygen",
    ],
}
