// @reliability: experimental
//! End-to-end integration tests: VOLE weaver → IrModule → lower_module_with_opts → C → compile.
//!
//! These tests exercise the full pipeline from a boolean circuit through the VOLE
//! weaver, through the LIR lowering pass, to compiled C code.  They are intended
//! to surface lowering gaps and type-handling bugs; failures are expected initially
//! and should be fixed iteratively.
//!
//! # Pipeline
//!
//! ```text
//! BIrBlocks (circuit)
//!   └─ weave_vole_prover/verifier  (volar-weaver)
//!        └─ IrModule<IrFunction>   (woven + spec-linked)
//!             └─ lower_module_with_opts  (volar-lir-codegen)
//!                  └─ CBackend::finish()
//!                       └─ cc -O0 -std=c99  (system C compiler)
//! ```

use std::path::Path;

use volar_c_backend::CBackend;
use volar_compiler::{
    SourceInput, ir::IrType, ir::PrimitiveType,
    linkage::{LinkageSystem, LinkedSpec},
    parse_sources,
};
use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};
use volar_lir_test_corpus::{compile_and_run, make_biir_and, make_biir_xor, make_biir_half_adder};
use volar_weaver::{weave_vole_prover, weave_vole_verifier};

// ============================================================================
// Helpers
// ============================================================================

fn spec_src_dir() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent().unwrap()
        .parent().unwrap()
        .join("spec").join("volar-spec").join("src")
}

fn read_spec(name: &str) -> (String, String) {
    let path = spec_src_dir().join(name);
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("cannot read spec file {}: {e}", path.display()));
    let stem = Path::new(name).file_stem().unwrap().to_string_lossy().into_owned();
    (src, stem)
}

/// Parse the VOLE-relevant spec sources into a single `IrModule`.
///
/// We include:
/// - `lib.rs`         — top-level re-exports, SpecRng trait
/// - `vole.rs`        — Delta, Q, VoleArray trait, rotation/remap operations
/// - `vole/prove.rs`  — vole_and_prover_step, vole_and_verifier_check
/// - `vole/vope.rs`   — Vope struct definition
/// - `vole/impls.rs`  — Add/Mul/BitXor impls for Vope
fn parse_vole_spec() -> volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction> {
    let files = [
        "lib.rs",
        "vole.rs",
        "vole/prove.rs",
        "vole/vope.rs",
        "vole/impls.rs",
    ];
    let loaded: Vec<(String, String)> = files.iter().map(|&f| read_spec(f)).collect();
    let inputs: Vec<SourceInput> = loaded.iter()
        .map(|(src, name)| SourceInput { source: src.as_str(), name: name.as_str() })
        .collect();
    parse_sources(&inputs, "volar_spec")
        .unwrap_or_else(|e| panic!("parse_vole_spec failed: {e}"))
}

/// Build a `LinkageSystem` from the VOLE spec sources.
fn make_vole_linkage() -> LinkageSystem {
    let spec_module = parse_vole_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec { name: "volar_spec".into(), module: spec_module });
    ls
}

/// `MonoEnv` for VOLE with 16-lane GF(2^8) field elements.
///
/// - `N = 16`  : VOLE vector length (number of field elements per wire)
/// - `T = u8`  : field element type (GF(2^8))
/// - `U1 = 1`  : typenum U1, used as the degree-1 polynomial index in Vope
/// - `K = 1`   : Vope degree param (same as U1; appears as ArrayLength::TypeParam)
/// - `U0 = 0`  : typenum U0
fn vole_env() -> MonoEnv {
    MonoEnv::new("sha256")
        .with_len("N", 16)
        .with_len("U1", 1)
        .with_len("U0", 0)
        .with_len("K", 1)
        .with_type("T", IrType::Primitive(PrimitiveType::U8))
}

// ============================================================================
// Structural tests (no lowering)
// ============================================================================

/// Verify that the spec parses without error and contains the expected types.
#[test]
fn vole_spec_parses() {
    let module = parse_vole_spec();
    let struct_names: Vec<&str> = module.structs.iter()
        .filter_map(|s| {
            if let volar_compiler::ir::StructKind::Custom(n) = &s.kind { Some(n.as_str()) } else { None }
        })
        .collect();
    assert!(
        struct_names.iter().any(|&n| n == "Vope" || n == "Delta" || n == "Q"),
        "expected Vope/Delta/Q in parsed spec structs, got: {struct_names:?}"
    );
    let fn_names: Vec<&str> = module.functions.iter().map(|f| f.name.as_str()).collect();
    assert!(
        fn_names.iter().any(|&n| n == "vole_and_prover_step"),
        "expected vole_and_prover_step in functions, got: {fn_names:?}"
    );
    assert!(
        fn_names.iter().any(|&n| n == "vole_and_verifier_check"),
        "expected vole_and_verifier_check in functions, got: {fn_names:?}"
    );
}

/// Verify that weaving a circuit with linkage adds the spec structs to the module.
#[test]
fn vole_weaved_module_has_spec_structs() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage));

    let struct_names: Vec<&str> = module.structs.iter()
        .filter_map(|s| {
            if let volar_compiler::ir::StructKind::Custom(n) = &s.kind { Some(n.as_str()) } else { None }
        })
        .collect();
    assert!(
        !struct_names.is_empty(),
        "linkage should add struct defs from spec, got empty structs list"
    );
}

// ============================================================================
// Lowering smoke tests — no linkage (woven function body only)
// ============================================================================
//
// These test the woven prover/verifier function in isolation — without the
// spec helper functions linked in.  The struct registry will be empty, so
// types like Vope<N,T,U1> appear as opaque externals.  Once lenient mode is
// enabled, these should compile to C even if the semantics are degraded.

/// Weave a single AND gate VOLE prover without spec linkage, then lower.
///
/// Without linkage, `module.structs` is empty and the C backend will encounter
/// unregistered Vope/Q types.  This test documents the current behavior —
/// it panics until lenient/opaque type handling is added to `lower_module_with_opts`.
#[test]
#[ignore = "requires lenient registry mode — documents expected failure"]
fn vole_prover_and_no_linkage_lower() {
    let circuit = make_biir_and();
    let module = weave_vole_prover(&circuit, "and_prover", None);

    assert!(module.structs.is_empty(), "no structs expected without linkage");

    let env = vole_env();
    let mut b = CBackend::new();
    // Currently panics: struct Vope not in registry.
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();
    assert!(!c_src.is_empty());
}

// ============================================================================
// Full pipeline tests — with spec linkage
// ============================================================================

/// Weave a single AND gate VOLE prover with spec linkage, lower to C, compile.
///
/// This is the primary smoke test for the VOLE lowering pipeline.
/// Failures here indicate unimplemented IR constructs in the spec helper
/// functions (`vole_and_prover_step`, etc.).
#[test]
fn vole_prover_and_gate_to_c() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage));

    let env = vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty(), "C output should be non-empty");
    // Compile the generated C to catch syntax/type errors.
    compile_and_run(&c_src, "/* vole prover smoke */");
}

/// Weave an XOR gate VOLE prover with spec linkage.
///
/// XOR has no AND gates, so there are no `vole_and_prover_step` calls —
/// simpler than the AND test.
#[test]
fn vole_prover_xor_gate_to_c() {
    let circuit = make_biir_xor();
    let linkage = make_vole_linkage();
    let module = weave_vole_prover(&circuit, "xor_prover", Some(&linkage));

    let env = vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty());
    compile_and_run(&c_src, "/* vole xor prover smoke */");
}

/// Weave a half-adder VOLE prover (XOR + AND) with spec linkage.
///
/// This is more comprehensive — the half adder has both XOR (no call) and
/// AND (calls vole_and_prover_step), exercising the tuple-return destructuring.
#[test]
fn vole_prover_half_adder_to_c() {
    let circuit = make_biir_half_adder();
    let linkage = make_vole_linkage();
    let module = weave_vole_prover(&circuit, "half_adder_prover", Some(&linkage));

    let env = vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty());
    compile_and_run(&c_src, "/* half adder prover smoke */");
}

/// Weave an AND gate VOLE verifier with spec linkage.
///
/// The verifier uses `vole_and_verifier_check` which returns `(Q<N,T>, bool)` —
/// tests tuple destructuring + bool field handling.
#[test]
fn vole_verifier_and_gate_to_c() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    let module = weave_vole_verifier(&circuit, "and_verifier", Some(&linkage));

    let env = vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty());
    compile_and_run(&c_src, "/* vole verifier smoke */");
}

/// Weave XOR gate verifier.
#[test]
fn vole_verifier_xor_gate_to_c() {
    let circuit = make_biir_xor();
    let linkage = make_vole_linkage();
    let module = weave_vole_verifier(&circuit, "xor_verifier", Some(&linkage));

    let env = vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty());
    compile_and_run(&c_src, "/* vole xor verifier smoke */");
}

/// End-to-end correctness test: prover produces hat, verifier accepts it.
///
/// Fabricates honest VOLE inputs with masks=0, delta=1, and bit values a=1, b=0.
/// VOLE relation: K_w = v_w + u_w * delta.  With v=0 and delta=1:
///   K_a = 1, K_b = 0, K_and = 0, hat = v_a*v_b = 0.
/// Verifier equation: K_a*K_b + hat == K_and*delta  =>  0 == 0  =>  true.
///
/// Stronger models would implement proper VOLE setup (OT-based correlation
/// generation) so that delta is unknown to the prover.  See goals.md.
#[test]
fn vole_prover_verifier_correctness_e2e() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    // Prover C already contains vole_and_verifier_check via spec linkage.
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage));

    let env = vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    // Honest VOLE inputs: a=1, b=0 => AND=0; delta=1 on all 16 GF(2^8) lanes;
    // masks v=0 so K_w = u_w * delta directly.
    let main_body = r#"
  Vope vope_one; memset(&vope_one, 0, sizeof(vope_one));
  memset(vope_one.u.data[0].data, 1, 16);
  Vope vope_a; memset(&vope_a, 0, sizeof(vope_a));
  memset(vope_a.u.data[0].data, 1, 16);
  Vope vope_b; memset(&vope_b, 0, sizeof(vope_b));

  __Tuple_s2_aau8x16x1 prover_out = vole_prove_and_prover(vope_one, vope_a, vope_b);
  Arr_U8_16 hat = prover_out._1.data[0];

  Delta delta; memset(&delta, 0, sizeof(delta));
  memset(delta.delta.data, 1, 16);
  Q q_a; memset(&q_a, 0, sizeof(q_a));
  memset(q_a.q.data, 1, 16);
  Q q_b; memset(&q_b, 0, sizeof(q_b));
  Q q_and; memset(&q_and, 0, sizeof(q_and));

  __Tuple_s1_b result = vole_and_verifier_check(delta, q_a, q_b, q_and, hat);
  printf("%d\n", (int)result._1);
"#;
    let out = compile_and_run(&c_src, main_body);
    assert_eq!(out.trim(), "1", "VOLE verifier should accept honest prover output");
}

#[test]
#[ignore = "diagnostic: writes C to /tmp for manual inspection"]
fn dump_vole_and_c_to_tmp() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    let module_p = weave_vole_prover(&circuit, "and_prover", Some(&linkage));
    let module_v = weave_vole_verifier(&circuit, "and_verifier", Some(&linkage));
    let env = vole_env();
    let mut bp = CBackend::new();
    lower_module_with_opts(&module_p, &mut bp, &env);
    let mut bv = CBackend::new();
    lower_module_with_opts(&module_v, &mut bv, &env);
    std::fs::write("/tmp/vole_prover.c", bp.finish()).unwrap();
    std::fs::write("/tmp/vole_verifier.c", bv.finish()).unwrap();
}
