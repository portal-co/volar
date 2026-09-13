// Shared with native_loop_equiv.rs (include! — keep in sync with
// lir_backend_components.rs).

use std::fs;
use std::path::{Path, PathBuf};
use volar_compiler::ir::{IrFunction, IrModule, IrType, PrimitiveType};
use volar_compiler::parser::parse_source;

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
    parse_dir(
        &root.join("crates/spec/volar-primitives/src"),
        "volar_primitives",
        &mut module,
    );
    parse_dir(
        &root.join("crates/spec/volar-common/src"),
        "volar_common",
        &mut module,
    );
    parse_dir(
        &root.join("crates/spec/volar-spec/src"),
        "volar_spec",
        &mut module,
    );
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

/// Env with common VOLE/TFHE const-generic bindings for seeded generic roots.
fn component_env() -> volar_lir_codegen::mono::MonoEnv {
    volar_lir_codegen::mono::MonoEnv::new("volar_lir_components")
        .with_len("N", 16)
        .with_len("U1", 1)
        .with_len("U2", 2)
        .with_len("U3", 3)
        .with_len("U0", 0)
        .with_len("K", 1)
        // TFHE ciphertext const params (root-level, harness-bound like N/K),
        // mirroring the spec's toy test profile (tfhe.rs T_* consts).
        .with_len("N_LWE", 8)
        .with_len("BIG_N", 64)
        .with_len("BS_ELL", 2)
        .with_len("KS_ELL", 2)
        .with_len("BS_BG_LOG", 16)
        .with_len("KS_BG_LOG", 16)
        .with_type("T", IrType::Primitive(PrimitiveType::U8))
}
