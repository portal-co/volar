//! End-to-end proof that emitted Noir source is real, compiling, executable
//! Noir — not just plausible-looking text. Parses a small hand-written spec
//! fixture, runs it through `print_module_noir`, writes the result into a
//! scratch Nargo project, and shells out to `nargo check`/`nargo execute`.
//!
//! Requires `nargo` on `PATH`. Mirrors the milestone-1 exit bar from the
//! approved plan: a non-generic, loop-free function round-tripped through
//! the whole pipeline and validated against a known input/output pair.

use std::fs;
use std::process::Command;

use volar_compiler::parser::parse_source;
use volar_compiler_noir_codegen::print_module_noir;

fn nargo_available() -> bool {
    Command::new("nargo")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn scratch_project(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("noir_codegen_smoke_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("src")).unwrap();
    fs::write(
        dir.join("Nargo.toml"),
        "[package]\nname = \"smoke\"\ntype = \"bin\"\nauthors = [\"\"]\n\n[dependencies]\n",
    )
    .unwrap();
    dir
}

fn run_nargo(dir: &std::path::Path, args: &[&str]) -> std::process::Output {
    Command::new("nargo")
        .args(args)
        .current_dir(dir)
        .output()
        .expect("failed to run nargo")
}

#[test]
fn straight_line_arithmetic_round_trips_through_nargo() {
    if !nargo_available() {
        eprintln!("skipping: nargo not on PATH");
        return;
    }

    let source = r#"
        fn main(a: u32, b: u32) -> u32 {
            a + b
        }
    "#;
    let module = parse_source(source, "smoke", &["smoke".to_string()]).expect("parse failed");
    let noir_source = print_module_noir(&module).expect("codegen failed");
    assert!(noir_source.contains("fn main"), "generated:\n{noir_source}");

    let dir = scratch_project("straight_line");
    fs::write(dir.join("src/main.nr"), &noir_source).unwrap();
    fs::write(dir.join("Prover.toml"), "a = \"3\"\nb = \"4\"\n").unwrap();

    let check = run_nargo(&dir, &["check"]);
    assert!(
        check.status.success(),
        "nargo check failed:\nstdout: {}\nstderr: {}\n---\n{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr),
        noir_source,
    );

    let execute = run_nargo(&dir, &["execute"]);
    assert!(
        execute.status.success(),
        "nargo execute failed:\nstdout: {}\nstderr: {}\n---\n{}",
        String::from_utf8_lossy(&execute.stdout),
        String::from_utf8_lossy(&execute.stderr),
        noir_source,
    );
}

#[test]
fn if_else_round_trips_through_nargo() {
    if !nargo_available() {
        eprintln!("skipping: nargo not on PATH");
        return;
    }

    let source = r#"
        fn main(a: u32, b: u32) -> u32 {
            if a > b {
                a
            } else {
                b
            }
        }
    "#;
    let module = parse_source(source, "smoke_if", &["smoke_if".to_string()]).expect("parse failed");
    let noir_source = print_module_noir(&module).expect("codegen failed");

    let dir = scratch_project("if_else");
    fs::write(dir.join("src/main.nr"), &noir_source).unwrap();
    fs::write(dir.join("Prover.toml"), "a = \"7\"\nb = \"2\"\n").unwrap();

    let execute = run_nargo(&dir, &["execute"]);
    assert!(
        execute.status.success(),
        "nargo execute failed:\nstdout: {}\nstderr: {}\n---\n{}",
        String::from_utf8_lossy(&execute.stdout),
        String::from_utf8_lossy(&execute.stderr),
        noir_source,
    );
}

#[test]
fn while_loop_is_rejected_before_any_file_is_written() {
    let source = r#"
        fn main(a: u32) -> u32 {
            let mut x = a;
            while x > 0 {
                x = x - 1;
            }
            x
        }
    "#;
    let module = parse_source(source, "smoke_while", &["smoke_while".to_string()])
        .expect("parse failed");

    let result = std::panic::catch_unwind(|| print_module_noir(&module));
    let result = result.expect("print_module_noir must not panic");
    let errors = result.expect_err("while-loop module must be rejected, not accepted");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, volar_compiler_noir_codegen::NoirCodegenError::UnsupportedWhileLoop { .. })),
        "expected UnsupportedWhileLoop, got: {errors:?}"
    );
}
