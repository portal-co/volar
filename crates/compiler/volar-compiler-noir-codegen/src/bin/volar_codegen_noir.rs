//! `volar-codegen-noir` — minimal CLI wiring for the Noir backend.
//!
//! This is a v1 stub (full CLI wiring is a later milestone in the approved
//! plan — see `docs/noir-backend.md`): reads every `.rs` file from
//! `--spec-dir`, parses it, merges the result into a single `IrModule`, and
//! prints Noir source to `--out` (or stdout). No dedup/reachability pruning
//! yet — mirrors the simple parts of `volar-codegen`'s (`ts`/`dyn` targets)
//! own file-walking, not its full dedup logic.
//!
//! ```text
//! volar-codegen-noir --spec-dir <path> [--out <path>]
//! ```

use std::fs;
use std::path::{Path, PathBuf};

use volar_compiler::{ir::IrModule, parser::parse_source};
use volar_compiler_noir_codegen::print_module_noir;

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
            out.push(path);
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut spec_dir: Option<PathBuf> = None;
    let mut out_path: Option<PathBuf> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--spec-dir" => {
                spec_dir = args.get(i + 1).map(PathBuf::from);
                i += 2;
            }
            "--out" => {
                out_path = args.get(i + 1).map(PathBuf::from);
                i += 2;
            }
            other => {
                eprintln!("unknown argument: {other}");
                std::process::exit(1);
            }
        }
    }

    let Some(spec_dir) = spec_dir else {
        eprintln!("usage: volar-codegen-noir --spec-dir <path> [--out <path>]");
        std::process::exit(1);
    };

    let mut files = Vec::new();
    collect_rs_files(&spec_dir, &mut files);
    files.sort();

    let mut module = IrModule {
        name: "noir_module".to_string(),
        ..Default::default()
    };
    let mut parse_errors = 0usize;

    for file in &files {
        let content = match fs::read_to_string(file) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("error reading {file:?}: {e}");
                parse_errors += 1;
                continue;
            }
        };
        let stem = file.file_stem().and_then(|s| s.to_str()).unwrap_or("unknown");
        match parse_source(&content, stem, &[stem.to_string()]) {
            Ok(m) => {
                module.structs.extend(m.structs);
                module.enums.extend(m.enums);
                module.traits.extend(m.traits);
                module.impls.extend(m.impls);
                module.functions.extend(m.functions);
                module.type_aliases.extend(m.type_aliases);
                module.consts.extend(m.consts);
            }
            Err(e) => {
                eprintln!("parse error in {file:?}: {e}");
                parse_errors += 1;
            }
        }
    }

    if parse_errors > 0 {
        eprintln!("{parse_errors} file(s) failed to parse");
        std::process::exit(1);
    }

    match print_module_noir(&module) {
        Ok(text) => match out_path {
            Some(path) => {
                fs::write(&path, text).expect("failed to write output");
            }
            None => println!("{text}"),
        },
        Err(errors) => {
            for e in errors {
                eprintln!("noir codegen error: {e}");
            }
            std::process::exit(1);
        }
    }
}
