// @reliability: normal
//! @ai: assisted
//! M4: compile the spec WARP module (`volar-spec/src/warp.rs`) via IR.
//!
//! Gate (plan §8): parse → lower → Volar IR/VAFFLE round-trip of the spec
//! module's verify/decide path, with native-vs-IR differential execution.
//!
//! The probes below are the incremental compile-through ladder:
//! field identity → reference-to-array params → array element access →
//! the dialect's const-bound doubling loop → `warp_mle_eval` end-to-end.

use volar_compiler::parser::{SourceInput, parse_sources};
use volar_fuzz::interpreter::vaffle::eval_vaffle;
use volar_primitives::Galois128;
use volar_vaffle_target::VaffleTarget;

fn spec_source(file: &str) -> String {
    std::fs::read_to_string(format!(
        "{}/../../spec/volar-spec/src/{}",
        env!("CARGO_MANIFEST_DIR"),
        file
    ))
    .expect("volar-spec source is readable")
}

fn parse_with_driver(driver: &str) -> volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction> {
    let warp = spec_source("warp.rs");
    let keccak = spec_source("keccak.rs");
    parse_sources(
        &[
            SourceInput {
                source: &warp,
                name: "warp",
            },
            SourceInput {
                source: &keccak,
                name: "keccak",
            },
            SourceInput {
                source: driver,
                name: "warp_driver",
            },
        ],
        "warp_compile_probe",
        &[],
    )
    .expect("spec WARP + driver parse")
}

fn lower_seeded(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    seeds: &[&str],
) -> VaffleTarget {
    let mut target = VaffleTarget::new();
    volar_lir_codegen::lower_module_seeded(
        module,
        &mut target,
        &volar_lir_codegen::mono::MonoEnv::new(""),
        seeds,
    );
    target
}

fn bits_of(value: u128, width: usize) -> Vec<bool> {
    (0..width).map(|i| (value >> i) & 1 == 1).collect()
}

fn bits_to_u128(bits: &[bool]) -> u128 {
    bits.iter().enumerate().fold(0u128, |acc, (i, b)| {
        if *b { acc | (1u128 << i) } else { acc }
    })
}

/// Isolate: identity over the field through the full pipeline.
#[test]
fn probe_field_identity() {
    let module = parse_with_driver("use cipher::consts::*;\npub fn probe_id(x: Galois128) -> Galois128 { x }\n");
    let target = lower_seeded(&module, &["probe_id"]);
    let fid = target.module.exports["probe_id"];
    let out = eval_vaffle(&target.module, fid, &[bits_of(0xDEADBEEF, 128)])
        .expect("identity evaluates");
    assert_eq!(bits_to_u128(&out[0]), 0xDEADBEEF, "field identity");
}

/// Isolate: indexing a reference-to-array parameter.
#[test]
fn probe_ref_array_index() {
    let module = parse_with_driver(
        "use cipher::consts::*;\npub fn probe_index(point: &Array<Galois128, U2>) -> Galois128 {\n    point[0] + point[1]\n}\n",
    );
    let target = lower_seeded(&module, &["probe_index"]);
    let fid = target.module.exports["probe_index"];
    let out = eval_vaffle(&target.module, fid, &[bits_of(5, 128), bits_of(9, 128)])
        .expect("index evaluates");
    assert_eq!(bits_to_u128(&out[0]), (Galois128(5) + Galois128(9)).0);
}

/// Isolate: array element access and field addition through codegen.
#[test]
fn probe_array_elem_sum() {
    let module = parse_with_driver(
        "use cipher::consts::*;\npub fn probe_sum2(a: Array<Galois128, U2>) -> Galois128 { a[0] + a[1] }\n",
    );
    let target = lower_seeded(&module, &["probe_sum2"]);
    let fid = target.module.exports["probe_sum2"];
    let out = eval_vaffle(&target.module, fid, &[bits_of(5, 128), bits_of(9, 128)])
        .expect("sum2 evaluates");
    assert_eq!(bits_to_u128(&out[0]), (Galois128(5) + Galois128(9)).0);
}

/// Isolate: the dialect's const-bound doubling loop (the eq_evals shape).
#[test]
fn probe_loop_mutated_array() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_loop(p0: Galois128, p1: Galois128) -> Galois128 {\n\
             let mut out: Array<Galois128, U4> = Array::<Galois128, U4>::default();\n\
             out[0] = Galois128(1);\n\
             for c in 0..2usize {\n\
                 let coordinate = if c == 0 { p0 } else { p1 };\n\
                 for index in 0..(1usize << c) {\n\
                     let product = out[index] * coordinate;\n\
                     out[index] = out[index] - product;\n\
                     out[index + (1usize << c)] = product;\n\
                 }\n\
             }\n\
             out[3]\n\
         }\n",
    );
    let target = lower_seeded(&module, &["probe_loop"]);
    let fid = target.module.exports["probe_loop"];
    // eq((p0,p1), (1,1)) = p0 * p1
    let out = eval_vaffle(&target.module, fid, &[bits_of(5, 128), bits_of(9, 128)])
        .expect("loop evaluates");
    assert_eq!(
        bits_to_u128(&out[0]),
        (Galois128(5) * Galois128(9)).0,
        "out[3] = p0*p1"
    );
}

/// The pure-algebra path: MLE evaluation through the full
/// parse → LIR → VAFFLE pipeline, evaluated differentially.
#[test]
fn warp_mle_eval_compiles_via_ir_and_matches_native() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_mle(\n\
             values: Array<Galois128, U4>,\n\
             point: Array<Galois128, U2>,\n\
         ) -> Galois128 {\n\
             warp_mle_eval::<U4, U2>(&values, &point)\n\
         }\n",
    );
    let target = lower_seeded(&module, &["probe_mle"]);
    let fid = target.module.exports["probe_mle"];
    for (values, point) in [
        ([1u128, 2, 3, 4], [5u128, 6]),
        ([0xDEAD, 0xBEEF, 0xCAFE, 0xF00D], [0x1234, 0xABCD]),
    ] {
        let inputs: Vec<Vec<bool>> = values
            .iter()
            .chain(point.iter())
            .map(|v| bits_of(*v, 128))
            .collect();
        let out = eval_vaffle(&target.module, fid, &inputs).expect("compiled MLE evaluates");
        // Native reference: MLE = sum_i values[i] * eq(point, i).
        let (p0, p1) = (Galois128(point[0]), Galois128(point[1]));
        let eqs = [
            (Galois128(1) - p0) * (Galois128(1) - p1),
            p0 * (Galois128(1) - p1),
            (Galois128(1) - p0) * p1,
            p0 * p1,
        ];
        let mut expected = Galois128(0);
        for (v, w) in values.iter().zip(eqs.iter()) {
            expected = expected + Galois128(*v) * *w;
        }
        assert_eq!(
            bits_to_u128(&out[0]),
            expected.0,
            "compiled MLE must match native field arithmetic"
        );
    }
}
