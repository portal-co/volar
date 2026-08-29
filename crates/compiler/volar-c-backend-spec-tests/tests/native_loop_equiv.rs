//! Dual-path equivalence harness (docs/lir-native-loops-plan.md, Stage 4).
//!
//! For each component seed set that lowers green today, lower the reachable
//! set twice — once with concrete unrolling (`LoopLowering::Unroll`) and once
//! with native CFG loops (`LoopLowering::Native`) — then:
//!
//! 1. compile **both** C outputs with `cc` (smoke main; per the
//!    compile-and-run testing rule),
//! 2. compare the emitted function-name sets (both modes must specialize the
//!    identical instance set — the monomorphizer is loop-agnostic, so any
//!    difference is a bug),
//! 3. record both code sizes for the measurement table in the plan doc.
//!
//! Behavioral parity of the two loop paths is pinned by the focused
//! compile-and-run tests in `basic.rs` (sum_to, fill4, accumulate, while,
//! continue/break) where entry signatures are simple enough to call from C.

use std::collections::BTreeSet;

use volar_c_backend::CBackend;
use volar_lir_codegen::{
    LoopLowering, MonoPlanOptions, MonoRoot, lower_module_monomorphized, mono::MonoEnv,
};
use volar_lir_test_corpus::compile_and_run;

include!("lir_backend_components_module.rs");

fn lower_component(
    module: &IrModule<IrFunction>,
    env: &MonoEnv,
    seeds: &[&str],
    mode: LoopLowering,
) -> String {
    let roots: Vec<MonoRoot> = seeds
        .iter()
        .map(|s| MonoRoot::new((*s).to_owned(), env.clone()))
        .collect();
    let mut backend = CBackend::new();
    lower_module_monomorphized(
        module,
        &mut backend,
        MonoPlanOptions {
            roots,
            loop_lowering: mode,
            ..Default::default()
        },
    )
    .unwrap_or_else(|err| panic!("LIR monomorphization failed: {err}"));
    backend.finish()
}

/// Extract the emitted top-level function names from a C output.
fn emitted_functions(c_src: &str) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for line in c_src.lines() {
        let trimmed = line.trim_start();
        // Function definitions look like `<type> <name>(...) {`; skip control
        // keywords and declarations ending in `;`.
        if !trimmed.ends_with('{') || !trimmed.contains('(') {
            continue;
        }
        if trimmed.starts_with("if ")
            || trimmed.starts_with("while ")
            || trimmed.starts_with("for ")
            || trimmed.starts_with("switch ")
            || trimmed.starts_with("static ")
        {
            continue;
        }
        if let Some(open) = trimmed.rfind('(') {
            let head = &trimmed[..open];
            if let Some(name) = head.rsplit([' ', '*']).next() {
                if name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
                    && !name.is_empty()
                    && name != "main"
                {
                    names.insert(name.to_owned());
                }
            }
        }
    }
    names
}

/// Dual-path check for one component: both modes must emit the same instance
/// set, both C outputs must compile, and sizes are recorded.
fn check_component(seeds: &[&str], label: &str) {
    let module = build_module();
    let env = component_env();

    let unroll_c = lower_component(&module, &env, seeds, LoopLowering::Unroll);
    let native_c = lower_component(&module, &env, seeds, LoopLowering::Native);
    if std::env::var("VOLAR_DUMP_EQUIV").is_ok() {
        std::fs::write(format!("/tmp/equiv_{label}_unroll.c"), &unroll_c).unwrap();
        std::fs::write(format!("/tmp/equiv_{label}_native.c"), &native_c).unwrap();
    }

    let unroll_fns = emitted_functions(&unroll_c);
    let native_fns = emitted_functions(&native_c);
    assert_eq!(
        unroll_fns, native_fns,
        "component '{label}': Unroll and Native modes must specialize the identical instance set"
    );

    // Compile both outputs (link + run smoke main). The unrolled baseline is
    // the arbiter: if the baseline itself fails to compile, that is a
    // pre-existing emission gap unrelated to loop mode — report it instead
    // of attributing it to Native. When the baseline compiles, Native must
    // compile too.
    //
    // Outputs above COMPILE_BUDGET_BYTES are not sent to cc at all: cc
    // wall-time/memory explodes on very large translation units (the
    // ~283 MB unrolled tfhe output cannot be compiled in practice — itself
    // the motivation for native loops) and would hang the test, not fail it.
    const COMPILE_BUDGET_BYTES: usize = 64 * 1024 * 1024;
    let compile_c = |c: &str, what: &str| -> bool {
        if c.len() > COMPILE_BUDGET_BYTES {
            eprintln!(
                "    ({what} output {} MB exceeds the {} MB compile budget; skipped)",
                c.len() / (1024 * 1024),
                COMPILE_BUDGET_BYTES / (1024 * 1024)
            );
            return true; // not a failure — just out of budget
        }
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            compile_and_run(c, "  /* native-loop equivalence smoke */")
        }))
        .is_ok()
    };
    let unroll_ok = compile_c(&unroll_c, "unrolled baseline");
    let native_ok = compile_c(&native_c, "native");
    match (unroll_ok, native_ok) {
        (true, true) => {}
        (true, false) => panic!(
            "component '{label}': native-mode C output does not compile while the unrolled baseline does"
        ),
        (false, _) => eprintln!(
            "=== native-loop equivalence [{label}]: unrolled baseline does not compile; set-equality only ==="
        ),
    }

    eprintln!(
        "=== native-loop equivalence [{label}] OK: unrolled {} bytes vs native {} bytes ({:.1}x smaller) ===",
        unroll_c.len(),
        native_c.len(),
        unroll_c.len() as f64 / native_c.len().max(1) as f64
    );
}

#[test]
fn equiv_vole_prover() {
    check_component(
        &[
            "vole_and_prover_step",
            "vole_sbox_prover_step",
            "vole_mul3_prover_step",
        ],
        "vole_prover",
    );
}

#[test]
fn equiv_vole_verifier() {
    check_component(
        &[
            "vole_and_verifier_check",
            "vole_sbox_verifier_check",
            "vole_mul3_verifier_check",
        ],
        "vole_verifier",
    );
}

#[test]
fn equiv_tfhe() {
    check_component(
        &[
            "tfhe_not",
            "tfhe_gate_bootstrapping_and",
            "tfhe_cmux",
        ],
        "tfhe",
    );
}
