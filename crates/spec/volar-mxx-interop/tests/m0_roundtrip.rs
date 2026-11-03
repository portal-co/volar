// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! M0 evidence for Phase 0 of `docs/mxx-cutover-plan.md`: a small mxx DSL
//! program (matrix arithmetic core of an RLWE-shaped computation, ring
//! `Z_257[X]/(X^4 + 1)`) is built with `mxx-dsl`, validated, lowered to spec
//! IR by `volar-mxx-interop`, printed with `volar_compiler::printer`, and
//! then **compiled with rustc and run** against a naive reference computed
//! in this test (core design rule 2: the signal is that the generated code
//! compiles and runs correctly, not that the IR looks a certain way).
//!
//! The rustc compile+run is skippable only via `VOLAR_MXX_INTEROP_NO_RUSTC=1`
//! (environment blocker, recorded distinctly from a source failure).

use std::io::Write as _;
use std::path::{Path, PathBuf};

use mxx_dsl::{DslContext, Ring};
use mxx_ir_core::ParamEnv;
use volar_mxx_interop::{HelperMode, LowerOptions, lower_validated_to_module};

/// Naive negacyclic convolution mod `q`, the reference semantics.
fn poly_mul_ref(a: [u64; 4], b: [u64; 4], q: u64) -> [u64; 4] {
    let mut acc = [0u128; 4];
    for i in 0..4 {
        for j in 0..4 {
            let t = (a[i] as u128 * b[j] as u128) % q as u128;
            if i + j < 4 {
                acc[i + j] = (acc[i + j] + t) % q as u128;
            } else {
                acc[i + j - 4] = (acc[i + j - 4] + q as u128 - t) % q as u128;
            }
        }
    }
    acc.map(|x| x as u64)
}

fn poly_add_ref(a: [u64; 4], b: [u64; 4], q: u64) -> [u64; 4] {
    let mut out = [0u64; 4];
    for i in 0..4 {
        out[i] = ((a[i] as u128 + b[i] as u128) % q as u128) as u64;
    }
    out
}

fn poly_sub_ref(a: [u64; 4], b: [u64; 4], q: u64) -> [u64; 4] {
    let mut out = [0u64; 4];
    for i in 0..4 {
        out[i] = ((a[i] as u128 + q as u128 - b[i] as u128) % q as u128) as u64;
    }
    out
}

/// Deterministic toy inputs (LCG fill), as `[tower][coeff]` with one tower.
fn toy_poly(seed: u64, q: u64) -> [[u64; 4]; 1] {
    let mut x = seed;
    let mut p = [0u64; 4];
    for c in &mut p {
        x = x.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        *c = (x >> 33) % q;
    }
    [p]
}

fn rustc_available() -> bool {
    std::process::Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Compile `module_rs` plus a harness `main.rs` with rustc and run it.
fn compile_and_run(dir: &Path, module_rs: &str, harness: &str) {
    let module_path = dir.join("prog.rs");
    let main_path = dir.join("main.rs");
    std::fs::write(&module_path, module_rs).unwrap();
    std::fs::write(&main_path, harness).unwrap();
    let bin: PathBuf = dir.join("prog");
    let compile = std::process::Command::new("rustc")
        .arg("--edition")
        .arg("2024")
        // Unused inlined helpers are expected (the whole helper set is
        // inlined for closure safety), so no `--deny warnings` here.
        .arg("-o")
        .arg(&bin)
        .arg(&main_path)
        .output()
        .expect("spawn rustc");
    assert!(
        compile.status.success(),
        "rustc failed:\n{}\n--- module ---\n{}",
        String::from_utf8_lossy(&compile.stderr),
        module_rs
    );
    let run = std::process::Command::new(&bin).output().expect("run compiled program");
    assert!(
        run.status.success(),
        "compiled program failed:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn m0_mxx_graph_lowers_prints_compiles_and_runs() {
    // ── Build the mxx program: c = a*b + a, d = transpose(c) + (c - a) ──
    let ctx = DslContext::new("m0-arith");
    let ring = Ring::from_crt_moduli(vec![257.into()], 4);
    let a = ring.input("a", (2usize, 2usize));
    let b = ring.input("b", (2usize, 2usize));
    let c = a.clone() * b.clone() + a.clone();
    let d = c.clone().transpose() + (c - a);
    let built = ctx.output("d", d).unwrap().build().expect("build m0 graph");
    let validated = built.validate(&ParamEnv::default()).expect("validate m0 graph");

    // ── Lower to spec IR and print Rust ─────────────────────────────────
    let module = lower_validated_to_module(
        &built.graph,
        &validated,
        &LowerOptions {
            function_name: "m0_arith".into(),
            helpers: HelperMode::Inline,
            module_name: "m0_arith".into(),
        },
    )
    .expect("lower m0 graph");
    let printed = volar_mxx_interop::rt::print_standalone(&module);

    // The IR must round-trip through the spec parser too: a generated
    // element has to be parseable by the same pipeline that consumes
    // hand-written spec code.
    let reparsed = volar_compiler::parse_sources(
        &[volar_compiler::SourceInput {
            source: &printed,
            name: "m0_arith",
        }],
        "m0_arith",
        &[],
    )
    .expect("reparse printed module");
    assert!(
        reparsed.functions.iter().any(|f| f.name == "m0_arith"),
        "reparsed module must contain the lowered function"
    );

    if std::env::var_os("VOLAR_MXX_INTEROP_NO_RUSTC").is_some() || !rustc_available() {
        eprintln!("skipping rustc compile+run (VOLAR_MXX_INTEROP_NO_RUSTC or no rustc)");
        return;
    }

    // ── Reference computation ───────────────────────────────────────────
    let q = 257u64;
    let a = [
        [toy_poly(7, q), toy_poly(8, q)],
        [toy_poly(11, q), toy_poly(12, q)],
    ];
    let b = [
        [toy_poly(9, q), toy_poly(10, q)],
        [toy_poly(13, q), toy_poly(14, q)],
    ];
    let mut c = [[[[0u64; 4]; 1]; 2]; 2];
    for i in 0..2 {
        for j in 0..2 {
            let t0 = poly_mul_ref(a[i][0][0], b[0][j][0], q);
            let t1 = poly_mul_ref(a[i][1][0], b[1][j][0], q);
            c[i][j] = [poly_add_ref(poly_add_ref(t0, t1, q), a[i][j][0], q)];
        }
    }
    let mut d = [[[[0u64; 4]; 1]; 2]; 2];
    for i in 0..2 {
        for j in 0..2 {
            // d = transpose(c) + (c - a)
            d[i][j] = [poly_add_ref(c[j][i][0], poly_sub_ref(c[i][j][0], a[i][j][0], q), q)];
        }
    }

    // ── Compile the printed module with rustc and check the run ─────────
    let mut harness = String::new();
    harness.push_str("mod prog;\nuse prog::m0_arith;\nfn main() {\n");
    harness.push_str(&format!("    let a: [[[[u64; 4]; 1]; 2]; 2] = {a:?};\n"));
    harness.push_str(&format!("    let b: [[[[u64; 4]; 1]; 2]; 2] = {b:?};\n"));
    harness.push_str(&format!("    let expected: [[[[u64; 4]; 1]; 2]; 2] = {d:?};\n"));
    harness.push_str("    let got = m0_arith(a, b);\n");
    harness.push_str("    assert_eq!(got, expected, \"lowered m0 program disagrees with the naive reference\");\n");
    harness.push_str("}\n");

    let dir = std::env::temp_dir().join(format!("volar-mxx-m0-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    compile_and_run(&dir, &printed, &harness);
    // Keep artifacts for post-mortem on failure; clean up on success.
    let _ = std::fs::remove_dir_all(&dir);
    std::io::stderr().flush().ok();
}
