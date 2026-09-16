// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Workspace gate for the IR-not-text rule (AGENTS.md Core Design Rule 13;
//! plan: `docs/ir-not-text-weaving-plan.md`).
//!
//! Walks `crates/compiler/` and `crates/examples/` Rust sources with
//! `volar_compiler_passes::weave_text_lint` and requires zero violations
//! outside documented `@volar-allow-rust-text:` exemptions. Test paths
//! (`/tests/`) and `#[cfg(test)]` modules are skipped by the lint itself.

use std::path::{Path, PathBuf};

use volar_compiler_passes::weave_text_lint::{WEAVER_TEXT_EMITTERS, lint_source};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(3)
        .expect("could not find workspace root")
        .to_path_buf()
}

fn collect_rs(dir: &Path, out: &mut Vec<PathBuf>) {
    if !dir.is_dir() {
        return;
    }
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            // Standalone test directories are test code, not production.
            if path.file_name().is_some_and(|n| n == "tests") {
                continue;
            }
            collect_rs(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

#[test]
fn no_rust_text_weaving_in_production_code() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs(&root.join("crates/compiler"), &mut files);
    collect_rs(&root.join("crates/examples"), &mut files);
    files.sort();

    let mut violations = Vec::new();
    for f in &files {
        let source = std::fs::read_to_string(f).unwrap();
        let rel = f.strip_prefix(&root).unwrap().to_string_lossy().into_owned();
        violations.extend(lint_source(&source, &rel));
    }

    if !violations.is_empty() {
        let mut report = String::new();
        for v in &violations {
            report.push_str(&format!(
                "{}: {} [{}] {}\n",
                v.file, v.item, v.rule, v.note
            ));
        }
        panic!(
            "weave-text violations ({}); exempt with /// @volar-allow-rust-text: <category>: <reason>\n{}",
            violations.len(),
            report
        );
    }
}

/// Guard against emitter-list drift: every exported `print_*` function in
/// volar-weaver that renders woven output to Rust text must be in the
/// lint's emitter list (or be a target backend / TS printer, which are not
/// rustc feeders and are deliberately out of scope).
#[test]
fn emitter_list_covers_weaver_print_surface() {
    let root = workspace_root();
    let mut missing = Vec::new();
    for f in [
        "crates/compiler/volar-weaver/src/fhe.rs",
        "crates/compiler/volar-weaver/src/garble.rs",
        "crates/compiler/volar-weaver/src/vole.rs",
        "crates/compiler/volar-weaver/src/faest.rs",
        "crates/compiler/volar-weaver/src/net.rs",
        "crates/compiler/volar-weaver/src/hybrid_net.rs",
        "crates/compiler/volar-weaver/src/glue.rs",
    ] {
        let source = std::fs::read_to_string(root.join(f)).unwrap();
        for m in source.match_indices("pub fn print_") {
            let rest = &source[m.0 + 7..];
            let name: String = rest
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            // Target printers (C99 via LIR, TS) are not rustc feeders.
            if name.ends_with("_c") || name.ends_with("_ts") {
                continue;
            }
            if !WEAVER_TEXT_EMITTERS.contains(&name.as_str()) {
                missing.push(format!("{f}: {name}"));
            }
        }
    }
    assert!(
        missing.is_empty(),
        "new weaver text emitters not in the lint list; add them to WEAVER_TEXT_EMITTERS:\n{}",
        missing.join("\n")
    );
}
