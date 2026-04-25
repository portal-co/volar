//! `volar-codegen` — unified code generation binary for volar-spec.
//!
//! Reads all `.rs` files from a spec source directory, parses them through the
//! volar-compiler IR, and emits either dynamic Rust or TypeScript.
//!
//! # Usage
//!
//! ```text
//! volar-codegen <target> [OPTIONS]
//!
//! TARGETS
//!   ts        Emit TypeScript (for packages/volar-runtime)
//!   dyn       Emit dynamic Rust (for crates/spec/volar-spec-dyn)
//!
//! OPTIONS
//!   --spec-dir <path>     Directory containing volar-spec .rs source files.
//!                         Default (ts):  crates/spec/volar-spec/src
//!                         Default (dyn): crates/spec/volar-spec/src
//!   --out <path>          Output file path.
//!                         Default (ts):  packages/volar-runtime/src/generated.ts
//!                         Default (dyn): crates/spec/volar-spec-dyn/src/generated.rs
//!   --dump-ir             Write pre-lowering IR dump to ir_dump.txt.
//!   --dump-ir-dyn         Write post-lowering IR dump to ir_dump_dyn.txt.
//! ```
//!
//! All paths are relative to the workspace root (the directory containing the
//! root `Cargo.toml`). The binary locates the workspace root by walking up from
//! `CARGO_MANIFEST_DIR` until it finds a `Cargo.toml` with a `[workspace]`
//! section. When run via `cargo run` this is always the workspace root.

use std::fs;
use std::path::{Path, PathBuf};

use volar_compiler::{
    ir::{IrFunction, IrModule},
    manifest::emit_manifest,
    parser::parse_source,
};
use volar_compiler_passes::{
    dump_ir::dump_module,
    lowering_dyn::lower_module_dyn,
    print_module_rust_dyn,
    print_module_typescript,
};

// ---------------------------------------------------------------------------
// Target
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Target {
    Ts,
    Dyn,
    Manifest,
}

impl Target {
    fn default_spec_dir(self) -> &'static str {
        match self {
            Target::Ts | Target::Dyn => "crates/spec/volar-spec/src",
            Target::Manifest => "crates/spec/volar-primitives/src",
        }
    }

    fn default_out(self) -> &'static str {
        match self {
            Target::Ts => "packages/volar-runtime/src/generated.ts",
            Target::Dyn => "crates/spec/volar-spec-dyn/src/generated.rs",
            Target::Manifest => "crates/spec/volar-primitives/volar-primitives.volar.d",
        }
    }

    fn module_name(self) -> &'static str {
        match self {
            Target::Ts => "volar_ts",
            Target::Dyn => "volar_dyn",
            Target::Manifest => "volar_primitives",
        }
    }
}

// ---------------------------------------------------------------------------
// Config
// ---------------------------------------------------------------------------

struct Config {
    target: Target,
    spec_dir: PathBuf,
    out: PathBuf,
    dump_ir: bool,
    dump_ir_dyn: bool,
}

fn parse_args() -> Result<Config, Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();

    // First positional argument is the target.
    let target_str = args
        .get(1)
        .ok_or("Missing target. Usage: volar-codegen <ts|dyn> [OPTIONS]")?;

    let target = match target_str.as_str() {
        "ts" => Target::Ts,
        "dyn" => Target::Dyn,
        "manifest" => Target::Manifest,
        other => return Err(format!("Unknown target {:?}. Expected 'ts', 'dyn', or 'manifest'.", other).into()),
    };

    // Parse remaining arguments.
    let rest = &args[2..];
    let dump_ir = rest.iter().any(|a| a == "--dump-ir");
    let dump_ir_dyn = rest.iter().any(|a| a == "--dump-ir-dyn");

    let spec_dir = flag_value(rest, "--spec-dir")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(target.default_spec_dir()));

    let out = flag_value(rest, "--out")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(target.default_out()));

    Ok(Config { target, spec_dir, out, dump_ir, dump_ir_dyn })
}

/// Return the value following `--flag value` in `args`, if present.
fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    let pos = args.iter().position(|a| a == flag)?;
    args.get(pos + 1).map(|s| s.as_str())
}

// ---------------------------------------------------------------------------
// Source collection
// ---------------------------------------------------------------------------

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    if !dir.is_dir() {
        return Ok(());
    }
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out)?;
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

fn parse_spec(spec_dir: &Path, module_name: &str) -> IrModule<IrFunction> {
    let mut files = Vec::new();
    collect_rs_files(spec_dir, &mut files).unwrap_or_else(|e| {
        eprintln!("warning: error reading spec dir {:?}: {}", spec_dir, e);
    });
    files.sort(); // deterministic order

    eprintln!("[volar-codegen] spec dir:  {:?}", spec_dir);
    eprintln!("[volar-codegen] {} source file(s) found", files.len());

    let mut module = IrModule {
        name: module_name.to_string(),
        ..Default::default()
    };
    let mut errors = 0usize;

    for file in &files {
        let content = match fs::read_to_string(file) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("  error reading {:?}: {}", file, e);
                errors += 1;
                continue;
            }
        };
        let stem = file
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        match parse_source(&content, stem) {
            Ok(m) => {
                eprintln!(
                    "  parsed {:?}: {} structs  {} impls  {} fns",
                    file.file_name().unwrap(),
                    m.structs.len(),
                    m.impls.len(),
                    m.functions.len(),
                );
                module.structs.extend(m.structs);
                module.traits.extend(m.traits);
                module.impls.extend(m.impls);
                module.functions.extend(m.functions);
                module.type_aliases.extend(m.type_aliases);
            }
            Err(e) => {
                eprintln!("  parse error in {:?}: {}", file.file_name().unwrap(), e);
                errors += 1;
            }
        }
    }

    eprintln!(
        "[volar-codegen] combined IR: {} structs  {} traits  {} impls  {} fns  ({} error(s))",
        module.structs.len(),
        module.traits.len(),
        module.impls.len(),
        module.functions.len(),
        errors,
    );

    module
}

// ---------------------------------------------------------------------------
// Output helpers
// ---------------------------------------------------------------------------

fn write_file(path: &Path, content: &str) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, content)?;
    eprintln!("[volar-codegen] wrote {} byte(s) → {:?}", content.len(), path);
    Ok(())
}

fn write_dump(path: &Path, content: &str) -> Result<(), Box<dyn std::error::Error>> {
    fs::write(path, content)?;
    eprintln!("[volar-codegen] IR dump ({} bytes) → {:?}", content.len(), path);
    Ok(())
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cfg = parse_args()?;

    let module = parse_spec(&cfg.spec_dir, cfg.target.module_name());

    // Optional pre-lowering IR dump.
    if cfg.dump_ir {
        write_dump(Path::new("ir_dump.txt"), &dump_module(&module))?;
    }

    match cfg.target {
        Target::Ts => {
            // Optional post-lowering IR dump (dyn-lowered, for TS inspection).
            if cfg.dump_ir_dyn {
                let lowered = lower_module_dyn(&module);
                write_dump(Path::new("ir_dump_dyn.txt"), &dump_module(&lowered))?;
            }
            let code = print_module_typescript(&module);
            write_file(&cfg.out, &code)?;
        }

        Target::Dyn => {
            let code = print_module_rust_dyn(&module);
            // Optional post-lowering IR dump.
            if cfg.dump_ir_dyn {
                let lowered = lower_module_dyn(&module);
                write_dump(Path::new("ir_dump_dyn.txt"), &dump_module(&lowered))?;
            }
            write_file(&cfg.out, &code)?;
        }

        Target::Manifest => {
            let manifest_bytes = emit_manifest(
                &module,
                cfg.target.module_name(),
                "0.1.0",
                &[],
            );
            if let Some(parent) = cfg.out.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&cfg.out, &manifest_bytes)?;
            eprintln!(
                "[volar-codegen] wrote {} byte manifest \u{2192} {:?}",
                manifest_bytes.len(),
                cfg.out,
            );
        }
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("[volar-codegen] error: {}", e);
        std::process::exit(1);
    }
}
