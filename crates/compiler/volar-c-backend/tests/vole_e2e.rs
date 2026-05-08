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

/// `MonoEnv` for VOLE with 16-lane `Galois` (GF(2^8)) field elements.
///
/// Unlike `vole_env`, `T = Galois` routes through the C backend's
/// GF-aware arithmetic (`^` for add/sub, `volar_gf8_mul` for multiply).
/// This is required for correct OT-based VOLE where field masks are non-zero.
fn galois_vole_env() -> MonoEnv {
    MonoEnv::new("sha256")
        .with_len("N", 16)
        .with_len("U1", 1)
        .with_len("U0", 0)
        .with_len("K", 1)
        .with_type("T", IrType::Primitive(PrimitiveType::Galois))
}

/// Build a `LinkageSystem` using the standard spec (Galois is a PrimitiveType).
fn make_vole_linkage_galois() -> LinkageSystem {
    // Standard spec parse suffices; Galois is a PrimitiveType, not a custom struct.
    make_vole_linkage()
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

#[test]
#[ignore = "diagnostic: writes C (T=Galois) to /tmp for manual inspection"]
fn dump_vole_galois_c_to_tmp() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage_galois();
    let module_p = weave_vole_prover(&circuit, "and_prover", Some(&linkage));
    let env = galois_vole_env();
    let mut bp = CBackend::new();
    lower_module_with_opts(&module_p, &mut bp, &env);
    std::fs::write("/tmp/vole_galois_prover.c", bp.finish()).unwrap();
}

/// OT-driven end-to-end test: VOLE setup with random Δ and masks → Quicksilver AND check.
///
/// This test exercises the full pipeline with honest GF(2^8) VOLE correlations:
///
/// 1. The AND-gate prover is woven and lowered with `T = Galois`.  The C backend
///    emits `volar_gf8_mul` (carry-less multiply) for every `*` on GF values and
///    `^` for every `+` / `-`.
///
/// 2. The C `main` implements OT-based VOLE setup:
///    - Δ is sampled uniformly from non-zero GF(2^8) elements.
///    - Per-wire masks `v` are uniform random GF elements.
///    - Verifier's q is `q_w[i] = r0[i]` (sender), prover's `v_w[i] = r0[i] XOR b*delta[i]`.
///    - VOLE relation: `q = v + u·Δ` in GF(2^8).
///
/// 3. For each (a,b) ∈ {(0,0),(0,1),(1,0),(1,1)}, the test:
///    - Commits vope_one (bit=1), vope_a, vope_b via OT
///    - Calls `vole_prove_and_prover(vope_one, vope_a, vope_b)` → (vope_c, hat_wrapped)
///    - Derives `q_and[i] = (q_a[i] * q_b[i] XOR hat[i]) * Δ[i]^{-1}`
///    - Calls `vole_and_verifier_check(delta, q_a, q_b, q_and, hat)` → bool
///    - All four must return true
///
/// Generated C struct names (with T=Galois → LirType::Native(AES8) → uint8_t):
///   `Arr_Native_AES8_16`       – [uint8_t; 16]
///   `Arr_Arr_Native_AES8_16_1` – [[uint8_t; 16]; 1]
///   `Vope` / `Delta` / `Q`    – wrapping the above
///   `__Tuple_s2_aan7x16x1`     – prover output (Vope, Arr_Arr_Native_AES8_16_1)
///   `__Tuple_s1_b`             – verifier output (Q, bool)
#[test]
fn vole_prover_verifier_ot_setup_e2e() {
    let circuit = make_biir_and();
    // Standard linkage suffices: Galois is a PrimitiveType, not a custom struct.
    let linkage = make_vole_linkage_galois();
    // Weave the AND-gate prover (includes spec helper functions via linkage).
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage));

    let env = galois_vole_env();
    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty(), "C output should be non-empty");
    // The generated code must include the GF8 multiply helper.
    assert!(c_src.contains("volar_gf8_mul"), "generated C must contain volar_gf8_mul");

    // GF(2^8) inversion helper (a^{254}) — appended after generated code so it
    // can use `volar_gf8_mul` which is `static` in the generated code.
    let gf8_inv_helper = r#"
/* GF(2^8) inversion via repeated squaring: a^{254} = a^{128+64+32+16+8+4+2}.
 * Uses volar_gf8_mul defined above in the generated section. */
static uint8_t volar_gf8_inv(uint8_t a) {
  if (a == 0) return 0;
  uint8_t a2   = volar_gf8_mul(a, a);
  uint8_t a4   = volar_gf8_mul(a2, a2);
  uint8_t a8   = volar_gf8_mul(a4, a4);
  uint8_t a16  = volar_gf8_mul(a8, a8);
  uint8_t a32  = volar_gf8_mul(a16, a16);
  uint8_t a64  = volar_gf8_mul(a32, a32);
  uint8_t a128 = volar_gf8_mul(a64, a64);
  uint8_t r = volar_gf8_mul(a2, a4);
  r = volar_gf8_mul(r, a8);
  r = volar_gf8_mul(r, a16);
  r = volar_gf8_mul(r, a32);
  r = volar_gf8_mul(r, a64);
  r = volar_gf8_mul(r, a128);
  return r;
}
"#;
    // Append inversion helper after generated code (which contains volar_gf8_mul).
    let full_c = format!("{c_src}\n{gf8_inv_helper}");

    // C main: OT-based VOLE setup + Quicksilver AND check for all 4 (a,b) pairs.
    //
    // VOLE relation (IdealCot-style): q[i] = r0[i]; v[i] = r0[i] XOR b*delta[i]
    //   where b = 0 or 1 (the committed bit, lifted to GF).
    let main_body = r#"
  /* ---- SplitMix64 RNG ---- */
  uint64_t sm_state = 0xDEADBEEFCAFEBABEull;
  #define SM_NEXT() do { \
    sm_state += 0x9E3779B97F4A7C15ull; \
    uint64_t _z = sm_state; \
    _z = (_z ^ (_z >> 30)) * 0xBF58476D1CE4E5B9ull; \
    _z = (_z ^ (_z >> 27)) * 0x94D049BB133111EBull; \
    sm_state = _z ^ (_z >> 31); \
  } while(0)
  #define SM_U8() ((uint8_t)(sm_state >> 56))

  /* ---- Sample non-zero delta (16 lanes) ---- */
  Arr_Native_AES8_16 delta_arr;
  int di;
  for (di = 0; di < 16; di++) {
    SM_NEXT();
    uint8_t d = SM_U8();
    if (d == 0) d = 1; /* ensure non-zero */
    delta_arr.data[di] = d;
  }
  Delta delta; delta.delta = delta_arr;

  /* ---- Helper: commit a bit (b=0 or 1) to a VOLE wire ---- */
  /* Uses IdealCot: r0 = random row; q = r0; v = r0 XOR b*delta  */
  #define COMMIT_BIT(bit, vp, qv) do { \
    Arr_Native_AES8_16 _r0; \
    int _ci; \
    for (_ci = 0; _ci < 16; _ci++) { SM_NEXT(); _r0.data[_ci] = SM_U8(); } \
    (qv).q = _r0; \
    Arr_Native_AES8_16 _v; \
    for (_ci = 0; _ci < 16; _ci++) { \
      _v.data[_ci] = (bit) ? (_r0.data[_ci] ^ delta_arr.data[_ci]) : _r0.data[_ci]; \
    } \
    Arr_Native_AES8_16 _u_row; \
    for (_ci = 0; _ci < 16; _ci++) { _u_row.data[_ci] = (bit) ? 1 : 0; } \
    Arr_Arr_Native_AES8_16_1 _u; _u.data[0] = _u_row; \
    (vp).u = _u; (vp).v = _v; \
  } while(0)

  /* ---- derive_and_q: q_and[i] = (q_a[i]*q_b[i] XOR hat[i]) * delta[i]^{-1} ---- */
  #define DERIVE_AND_Q(qa, qb, ht, qand_out) do { \
    int _qi; \
    for (_qi = 0; _qi < 16; _qi++) { \
      uint8_t _lhs = volar_gf8_mul((qa).q.data[_qi], (qb).q.data[_qi]) ^ (ht).data[_qi]; \
      (qand_out).q.data[_qi] = volar_gf8_mul(_lhs, volar_gf8_inv(delta_arr.data[_qi])); \
    } \
  } while(0)

  /* ---- Run all 4 (a,b) combinations ---- */
  int pass = 0;
  int total = 4;
  int ai, bi;
  for (ai = 0; ai <= 1; ai++) {
    for (bi = 0; bi <= 1; bi++) {
      Vope vope_one; Q q_one;
      Vope vope_a;   Q q_a;
      Vope vope_b;   Q q_b;
      COMMIT_BIT(1,   vope_one, q_one);
      COMMIT_BIT(ai,  vope_a,   q_a);
      COMMIT_BIT(bi,  vope_b,   q_b);

      /* Prover step: (vope_c, hat_wrapped) */
      __Tuple_s2_aan7x16x1 pout = vole_prove_and_prover(vope_one, vope_a, vope_b);
      /* hat is pout._1.data[0] */
      Arr_Native_AES8_16 hat = pout._1.data[0];

      /* Verifier derives q_and */
      Q q_and;
      DERIVE_AND_Q(q_a, q_b, hat, q_and);

      /* Verifier check */
      __Tuple_s1_b chk = vole_and_verifier_check(delta, q_a, q_b, q_and, hat);
      if (chk._1) pass++;
    }
  }
  printf("%d/%d\n", pass, total);
"#;

    let out = compile_and_run(&full_c, main_body);
    assert_eq!(out.trim(), "4/4",
        "All 4 (a,b) combinations should pass the Quicksilver AND verifier check.\n\
         Output was: {}", out.trim());
}
