// @reliability: experimental
//! End-to-end integration tests: VOLE weaver → IrModule → MonoPlan → C → compile.
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
//!             └─ lower_module_monomorphized  (volar-lir-codegen)
//!                  └─ CBackend::finish()
//!                       └─ cc -O0 -std=c99  (system C compiler)
//! ```

use std::path::Path;

use volar_c_backend::CBackend;
use volar_compiler::{
    SourceInput,
    ir::IrFunction,
    ir::IrModule,
    ir::IrType,
    ir::PrimitiveType,
    linkage::{LinkageKind, LinkageSystem, LinkedSpec},
    parse_sources,
};
use volar_lir_codegen::mono::MonoEnv;
use volar_lir_saved::{RecordingTarget, SavedLirModule};
use volar_vole_pipeline::{
    galois_vole_env, lower_vole_to_target, make_vole_linkage, make_vole_linkage_galois,
    parse_vole_spec, vole_env,
};
use volar_lir_test_corpus::{compile_and_run, make_biir_and, make_biir_half_adder, make_biir_xor};
use volar_wasm_backend::WasmBackend;
use volar_weaver::{weave_vole_prover, weave_vole_verifier};
use wasmtime::{Engine, Module, Store};


/// Plan-API lower for woven VOLE modules (delegates to the shared pipeline).
fn lower_vole_module(module: &IrModule<IrFunction>, backend: &mut CBackend, env: &MonoEnv) {
    lower_vole_to_target(module, backend, env);
}

// ============================================================================
// Structural tests (no lowering)
// ============================================================================

/// Verify that the spec parses without error and contains the expected types.
#[test]
fn vole_spec_parses() {
    let module = parse_vole_spec();
    let struct_names: Vec<&str> = module
        .structs
        .iter()
        .filter_map(|s| {
            if let volar_compiler::ir::StructKind::Custom(n) = &s.kind {
                Some(n.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        struct_names
            .iter()
            .any(|&n| n == "Vope" || n == "Delta" || n == "Q"),
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
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();

    let struct_names: Vec<&str> = module
        .structs
        .iter()
        .filter_map(|s| {
            if let volar_compiler::ir::StructKind::Custom(n) = &s.kind {
                Some(n.as_str())
            } else {
                None
            }
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
    let module = weave_vole_prover(&circuit, "and_prover", None).into_inner();

    assert!(
        module.structs.is_empty(),
        "no structs expected without linkage"
    );

    let env = vole_env();
    let mut b = CBackend::new();
    // Currently panics: struct Vope not in registry.
    lower_vole_module(&module, &mut b, &env);
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
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();

    let env = vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
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
    let module = weave_vole_prover(&circuit, "xor_prover", Some(&linkage)).into_inner();

    let env = vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
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
    let module = weave_vole_prover(&circuit, "half_adder_prover", Some(&linkage)).into_inner();

    let env = vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
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
    let module = weave_vole_verifier(&circuit, "and_verifier", Some(&linkage)).into_inner();

    let env = vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty());
    compile_and_run(&c_src, "/* vole verifier smoke */");
}

/// Weave XOR gate verifier.
#[test]
fn vole_verifier_xor_gate_to_c() {
    let circuit = make_biir_xor();
    let linkage = make_vole_linkage();
    let module = weave_vole_verifier(&circuit, "xor_verifier", Some(&linkage)).into_inner();

    let env = vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
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
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();

    let env = vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
    let c_src = b.finish();

    // Honest VOLE inputs: a=1, b=0 => AND=0; delta=1 on all 16 GF(2^8) lanes;
    // masks v=0 so K_w = u_w * delta directly.
    let main_body = r#"
  Vope vope_one; memset(&vope_one, 0, sizeof(vope_one));
  memset(vope_one.u.data[0].data, 1, 16);
  Vope vope_a; memset(&vope_a, 0, sizeof(vope_a));
  memset(vope_a.u.data[0].data, 1, 16);
  Vope vope_b; memset(&vope_b, 0, sizeof(vope_b));

  __Tuple_s0_aau8x16x1 prover_out = vole_prove_and_prover(vope_one, vope_a, vope_b);
  Arr_U8_16 hat = prover_out._1.data[0];

  Delta delta; memset(&delta, 0, sizeof(delta));
  memset(delta.delta.data, 1, 16);
  Q q_a; memset(&q_a, 0, sizeof(q_a));
  memset(q_a.q.data, 1, 16);
  Q q_b; memset(&q_b, 0, sizeof(q_b));
  Q q_and; memset(&q_and, 0, sizeof(q_and));

  __Tuple_s3_b result = vole_and_verifier_check(delta, q_a, q_b, q_and, hat);
  printf("%d\n", (int)result._1);
"#;
    let out = compile_and_run(&c_src, main_body);
    assert_eq!(
        out.trim(),
        "1",
        "VOLE verifier should accept honest prover output"
    );
}

/// Record-once / replay-many: woven AND → [`RecordingTarget`] → C + WASM.
///
/// C path re-runs the honest-prover correctness check. WASM path validates the
/// module with wasmtime and executes `vole_and_verifier_check` on the same
/// zero-mask inputs (flat `i32` params / results).
#[test]
fn vole_and_record_replay_c_and_wasm() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();
    let env = vole_env();

    let mut rec = RecordingTarget::new();
    lower_vole_to_target(&module, &mut rec, &env);
    let saved: SavedLirModule = rec.finish();

    let mut c = CBackend::new();
    let mut wasm = WasmBackend::new();
    saved.replay_pair(&mut c, &mut wasm);

    let c_src = c.finish();
    let main_body = r#"
  Vope vope_one; memset(&vope_one, 0, sizeof(vope_one));
  memset(vope_one.u.data[0].data, 1, 16);
  Vope vope_a; memset(&vope_a, 0, sizeof(vope_a));
  memset(vope_a.u.data[0].data, 1, 16);
  Vope vope_b; memset(&vope_b, 0, sizeof(vope_b));

  __Tuple_s0_aau8x16x1 prover_out = vole_prove_and_prover(vope_one, vope_a, vope_b);
  Arr_U8_16 hat = prover_out._1.data[0];

  Delta delta; memset(&delta, 0, sizeof(delta));
  memset(delta.delta.data, 1, 16);
  Q q_a; memset(&q_a, 0, sizeof(q_a));
  memset(q_a.q.data, 1, 16);
  Q q_b; memset(&q_b, 0, sizeof(q_b));
  Q q_and; memset(&q_and, 0, sizeof(q_and));

  __Tuple_s3_b result = vole_and_verifier_check(delta, q_a, q_b, q_and, hat);
  printf("%d\n", (int)result._1);
"#;
    assert_eq!(
        compile_and_run(&c_src, main_body).trim(),
        "1",
        "C replay of recorded LIR should accept honest prover output"
    );

    let bytes = wasm.finish();
    assert!(!bytes.is_empty(), "WASM replay should emit a module");
    let engine = Engine::default();
    let wasm_module = Module::new(&engine, &bytes).expect("WASM module should validate");
    assert!(
        wasm_module
            .exports()
            .any(|e| e.name() == "vole_and_verifier_check"),
        "expected vole_and_verifier_check export"
    );

    // Stub any imports (crypto externs unused on this zero-mask path).
    let mut linker = wasmtime::Linker::new(&engine);
    for import in wasm_module.imports() {
        let module_name = import.module().to_owned();
        let name = import.name().to_owned();
        match import.ty() {
            wasmtime::ExternType::Func(ft) => {
                linker
                    .func_new(&module_name, &name, ft, |_, _params, results| {
                        for r in results.iter_mut() {
                            *r = match *r {
                                wasmtime::Val::I32(_) => wasmtime::Val::I32(0),
                                wasmtime::Val::I64(_) => wasmtime::Val::I64(0),
                                wasmtime::Val::F32(_) => wasmtime::Val::F32(0),
                                wasmtime::Val::F64(_) => wasmtime::Val::F64(0),
                                _ => wasmtime::Val::I32(0),
                            };
                        }
                        Ok(())
                    })
                    .unwrap_or_else(|e| panic!("link import {module_name}::{name}: {e}"));
            }
            other => panic!("unsupported wasm import type: {other:?}"),
        }
    }

    // Flat ABI: Delta(16) + Q(16)*3 + hat(16) = 80 × i32 params; returns Q(16) + bool.
    let mut store = Store::new(&engine, ());
    let instance = linker
        .instantiate(&mut store, &wasm_module)
        .expect("instantiate wasm");
    let func = instance
        .get_func(&mut store, "vole_and_verifier_check")
        .expect("export vole_and_verifier_check");
    let mut params = vec![wasmtime::Val::I32(0); 80];
    for i in 0..16 {
        params[i] = wasmtime::Val::I32(1); // delta
        params[16 + i] = wasmtime::Val::I32(1); // q_a
    }
    let mut results = vec![wasmtime::Val::I32(0); 17];
    func.call(&mut store, &params, &mut results)
        .expect("call vole_and_verifier_check");
    let ok = match results[16] {
        wasmtime::Val::I32(v) => v != 0,
        other => panic!("expected i32 bool result, got {other:?}"),
    };
    assert!(
        ok,
        "WASM verifier should accept honest zero-mask AND inputs"
    );
}

#[test]
#[ignore = "diagnostic: writes C to /tmp for manual inspection"]
fn dump_vole_and_c_to_tmp() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage();
    let module_p = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();
    let module_v = weave_vole_verifier(&circuit, "and_verifier", Some(&linkage)).into_inner();
    let env = vole_env();
    let mut bp = CBackend::new();
    lower_vole_module(&module_p, &mut bp, &env);
    let mut bv = CBackend::new();
    lower_vole_module(&module_v, &mut bv, &env);
    std::fs::write("/tmp/vole_prover.c", bp.finish()).unwrap();
    std::fs::write("/tmp/vole_verifier.c", bv.finish()).unwrap();
}

#[test]
#[ignore = "diagnostic: writes C (T=Galois) to /tmp for manual inspection"]
fn dump_vole_galois_c_to_tmp() {
    let circuit = make_biir_and();
    let linkage = make_vole_linkage_galois();
    let module_p = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();
    let env = galois_vole_env();
    let mut bp = CBackend::new();
    lower_vole_module(&module_p, &mut bp, &env);
    std::fs::write("/tmp/vole_galois_prover.c", bp.finish()).unwrap();
}

/// OT-driven end-to-end test: VOLE setup with random Δ and masks → Quicksilver AND check.
///
/// This test exercises the full pipeline with honest GF(2^8) VOLE correlations:
///
/// 1. The AND-gate prover is woven and lowered with `T = Galois`. The C backend
///    keeps the canonical GF(2^8) extension-field value typed and emits its
///    descriptor-specific carry-less multiplication helper.
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
/// Generated C structs retain the canonical extension-field descriptor for
/// each GF(2^8) lane; the test's host-side OT harness uses those same wrappers.
#[test]
fn vole_prover_verifier_ot_setup_e2e() {
    let circuit = make_biir_and();
    // Standard linkage suffices: Galois is a PrimitiveType, not a custom struct.
    let linkage = make_vole_linkage_galois();
    // Weave the AND-gate prover (includes spec helper functions via linkage).
    let module = weave_vole_prover(&circuit, "and_prover", Some(&linkage)).into_inner();

    let env = galois_vole_env();
    let mut b = CBackend::new();
    lower_vole_module(&module, &mut b, &env);
    let c_src = b.finish();

    assert!(!c_src.is_empty(), "C output should be non-empty");
    // The generated code must contain the multiplication helper for its
    // canonical GF(2^8) descriptor (the name deliberately includes that type).
    assert!(
        c_src.contains("volar_mul_Ext_d8_Native_Bit_p11b"),
        "generated C must contain the GF(2^8) extension-field multiply helper"
    );

    // The generated C ABI represents a field scalar as a typed one-limb value.
    // These aliases keep the host-side OT test readable while retaining that
    // type at every field operation.
    let gf8_inv_helper = r#"
typedef Volar_Ext_d8_Native_Bit_p11b Gf8;
typedef Arr_Ext_d8_Native_Bit_p11b_16 Arr_Gf8_16;
typedef Arr_Arr_Ext_d8_Native_Bit_p11b_16_1 Arr_Arr_Gf8_16_1;
typedef __Tuple_s0_aaef8xn0p1_1_0_1_1_0_0_0_1x16x1 VoleProveAndOut;

static Gf8 gf8_from_u8(uint8_t x) {
  Gf8 value = {0};
  value.limb[0] = x;
  return value;
}

/* GF(2^8) inversion via repeated squaring: a^{254}. */
static Gf8 volar_gf8_inv(Gf8 a) {
  if (a.limb[0] == 0) return a;
  Gf8 a2 = volar_mul_Ext_d8_Native_Bit_p11b(a, a);
  Gf8 a4 = volar_mul_Ext_d8_Native_Bit_p11b(a2, a2);
  Gf8 a8 = volar_mul_Ext_d8_Native_Bit_p11b(a4, a4);
  Gf8 a16 = volar_mul_Ext_d8_Native_Bit_p11b(a8, a8);
  Gf8 a32 = volar_mul_Ext_d8_Native_Bit_p11b(a16, a16);
  Gf8 a64 = volar_mul_Ext_d8_Native_Bit_p11b(a32, a32);
  Gf8 a128 = volar_mul_Ext_d8_Native_Bit_p11b(a64, a64);
  Gf8 r = volar_mul_Ext_d8_Native_Bit_p11b(a2, a4);
  r = volar_mul_Ext_d8_Native_Bit_p11b(r, a8);
  r = volar_mul_Ext_d8_Native_Bit_p11b(r, a16);
  r = volar_mul_Ext_d8_Native_Bit_p11b(r, a32);
  r = volar_mul_Ext_d8_Native_Bit_p11b(r, a64);
  r = volar_mul_Ext_d8_Native_Bit_p11b(r, a128);
  return r;
}
"#;
    // Append the typed inversion helper after the generated helper definitions.
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
  Arr_Gf8_16 delta_arr;
  int di;
  for (di = 0; di < 16; di++) {
    SM_NEXT();
    uint8_t d = SM_U8();
    if (d == 0) d = 1; /* ensure non-zero */
    delta_arr.data[di] = gf8_from_u8(d);
  }
  Delta delta; delta.delta = delta_arr;

  /* ---- Helper: commit a bit (b=0 or 1) to a VOLE wire ---- */
  /* Uses IdealCot: r0 = random row; q = r0; v = r0 XOR b*delta  */
  #define COMMIT_BIT(bit, vp, qv) do { \
    Arr_Gf8_16 _r0; \
    int _ci; \
    for (_ci = 0; _ci < 16; _ci++) { SM_NEXT(); _r0.data[_ci] = gf8_from_u8(SM_U8()); } \
    (qv).q = _r0; \
    Arr_Gf8_16 _v; \
    for (_ci = 0; _ci < 16; _ci++) { \
      _v.data[_ci] = (bit) ? volar_add_Ext_d8_Native_Bit_p11b(_r0.data[_ci], delta_arr.data[_ci]) : _r0.data[_ci]; \
    } \
    Arr_Gf8_16 _u_row; \
    for (_ci = 0; _ci < 16; _ci++) { _u_row.data[_ci] = gf8_from_u8((bit) ? 1 : 0); } \
    Arr_Arr_Gf8_16_1 _u; _u.data[0] = _u_row; \
    (vp).u = _u; (vp).v = _v; \
  } while(0)

  /* ---- derive_and_q: q_and[i] = (q_a[i]*q_b[i] XOR hat[i]) * delta[i]^{-1} ---- */
  #define DERIVE_AND_Q(qa, qb, ht, qand_out) do { \
    int _qi; \
    for (_qi = 0; _qi < 16; _qi++) { \
      Gf8 _lhs = volar_add_Ext_d8_Native_Bit_p11b( \
        volar_mul_Ext_d8_Native_Bit_p11b((qa).q.data[_qi], (qb).q.data[_qi]), (ht).data[_qi]); \
      (qand_out).q.data[_qi] = volar_mul_Ext_d8_Native_Bit_p11b(_lhs, volar_gf8_inv(delta_arr.data[_qi])); \
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
      VoleProveAndOut pout = vole_prove_and_prover(vope_one, vope_a, vope_b);
      /* hat is pout._1.data[0] */
      Arr_Gf8_16 hat = pout._1.data[0];

      /* Verifier derives q_and */
      Q q_and;
      DERIVE_AND_Q(q_a, q_b, hat, q_and);

      /* Verifier check */
      __Tuple_s3_b chk = vole_and_verifier_check(delta, q_a, q_b, q_and, hat);
      if (chk._1) pass++;
    }
  }
  printf("%d/%d\n", pass, total);
"#;

    let out = compile_and_run(&full_c, main_body);
    assert_eq!(
        out.trim(),
        "4/4",
        "All 4 (a,b) combinations should pass the Quicksilver AND verifier check.\n\
         Output was: {}",
        out.trim()
    );
}
