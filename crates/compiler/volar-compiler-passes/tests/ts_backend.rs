//! Integration test: full TS codegen pipeline → tsc --noEmit.
//!
//! Reads volar-primitives, volar-common, and volar-spec sources (same as the
//! `volar-codegen ts` binary), runs `print_module_typescript`, writes output to a
//! temp file, and checks it with `tsc --strict --noEmit`.
//!
//! The test is expected to fail until all type errors are resolved.  It logs the
//! full error list and the count so progress can be tracked with `--nocapture`.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use volar_compiler::{ir::IrFunction, ir::IrModule, parser::parse_source};
use volar_compiler_passes::print_module_typescript;

// ---------------------------------------------------------------------------
// Source collection (mirrors volar_codegen.rs)
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
        name: "volar_ts".to_string(),
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
// tsc runner
// ---------------------------------------------------------------------------

fn find_tsc() -> Option<PathBuf> {
    let root = workspace_root();
    let candidate = root.join("node_modules/.bin/tsc");
    if candidate.exists() { return Some(candidate); }
    // Fall back to PATH
    which_tsc()
}

fn which_tsc() -> Option<PathBuf> {
    let out = Command::new("which").arg("tsc").output().ok()?;
    if out.status.success() {
        let p = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if !p.is_empty() { return Some(PathBuf::from(p)); }
    }
    None
}

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

#[test]
fn test_ts_backend_no_errors() {
    let module = build_module();
    let ts_code = print_module_typescript(&module);

    // Strip @ts-nocheck so tsc sees all errors.
    let stripped: String = ts_code
        .lines()
        .filter(|l| !l.trim_start().starts_with("// @ts-nocheck"))
        .collect::<Vec<_>>()
        .join("\n");

    // Write to a temp file next to generated.ts so relative imports resolve correctly.
    let root = workspace_root();
    let runtime_src = root.join("packages/volar-runtime/src");
    let tmp_path = runtime_src.join(".generated_test_check.ts");
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
            "--moduleResolution", "bundler",
            "--target", "esnext",
            "--module", "esnext",
            tmp_path.to_str().unwrap(),
        ])
        .current_dir(&root.join("packages/volar-runtime"))
        .output()
        .expect("tsc command failed to launch");

    let _ = fs::remove_file(&tmp_path);

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{stdout}{stderr}");

    // Count and group errors by code.
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
        eprintln!("\n=== TypeScript backend: {} error(s) ===\n", total);
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
    }

    assert_eq!(total, 0, "tsc --strict reported {} error(s); see above for details", total);
}
