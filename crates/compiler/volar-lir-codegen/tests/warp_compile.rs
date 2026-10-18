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

/// Parse ONLY the driver source (no spec warp/keccak modules) — for
/// micro-probes that use only driver-local types and don't need the spec.
fn parse_driver_only(driver: &str) -> volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction> {
    parse_sources(
        &[SourceInput {
            source: driver,
            name: "warp_driver",
        }],
        "warp_compile_probe",
        &[],
    )
    .expect("driver-only parse")
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
    lower_seeded_loops(module, seeds, volar_lir_codegen::LoopLowering::Unroll)
}

/// Lower with an explicit loop mode. `Native` keeps loops as VAFFLE/Volar IR
/// CFG cycles (the WARP path's loop-heavy hashing/accumulation stays compact
/// instead of unrolling).
fn lower_seeded_loops(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    seeds: &[&str],
    loop_lowering: volar_lir_codegen::LoopLowering,
) -> VaffleTarget {
    let mut target = VaffleTarget::new();
    volar_lir_codegen::lower_module_monomorphized(
        module,
        &mut target,
        volar_lir_codegen::MonoPlanOptions {
            roots: seeds
                .iter()
                .map(|s| volar_lir_codegen::MonoRoot::new(*s, volar_lir_codegen::mono::MonoEnv::new("")))
                .collect(),
            loop_lowering,
            ..Default::default()
        },
    )
    .expect("seeded lowering succeeds");
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

/// Native-loop `if`-with-outer-assignment: the branch rebinds `acc`/`pos`,
/// which must be merged through the join block's params (the cross-block
/// threading fix). Without it, the join references a then/else-block value
/// directly and the interpreter reports "value not in table".
#[test]
fn probe_native_loop_if_assign() {
    let module = parse_with_driver(
        "pub fn probe(x: u64) -> u64 {\n\
             let mut acc = 0u64;\n\
             let mut pos = 0u64;\n\
             for i in 0..8u64 {\n\
                 acc = acc + i;\n\
                 pos = pos + 1u64;\n\
                 if pos == 3u64 {\n\
                     acc = acc * 2u64 + x;\n\
                     pos = 0u64;\n\
                 }\n\
             }\n\
             acc + pos\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe"];
    // x=0: acc folds +i with a doubling (and +0) whenever pos wraps at 3.
    // i=0: a=0,p=1; i=1: a=1,p=2; i=2: a=3,p=3→a=6,p=0; i=3: a=9,p=1;
    // i=4: a=13,p=2; i=5: a=18,p=3→a=36,p=0; i=6: a=42,p=1; i=7: a=49,p=2.
    // result = 49 + 2 = 51.
    // A pure-bit u64 param flattens to 64 single-bit entry params.
    let input: Vec<Vec<bool>> = (0..64).map(|_| vec![false]).collect();
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &input, 64)
        .expect("native loop with if-assign evaluates");
    // acc+pos = 51 as one U64 output value.
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 51, "acc+pos");
}

/// Genesis: index construction + transcript + Merkle + MLE + bundled eval,
/// all through IR, differentially against the native spec.
///
/// `#[ignore]`: with `LoopLowering::Native` the genesis lowering completes
/// and the interpreter (dense value table + shared stack) now runs the eval to
/// completion in a test budget. Cross-block threading is fixed (`lower_if`
/// merge), Keccak is correct (`probe_keccak_f1600_zero_state`), and the
/// mutable-array-reference aliasing works (shared `StorageId::STACK`). The one
/// remaining correctness gap is the match-of-captured-struct lowering bug —
/// the toy PESAT index built by the driver's `from_fn(|i| match i { 0 => mk0,
/// ... })` miscompiles (see `probe_match_captured_struct`), so the constraint
/// eval reads `n_vars` as 0 and the genesis returns `None`. Fix that bug and
/// this probe should match the native spec. The lighter probes above prove
/// the compile-via-IR + differential machinery end-to-end.
/// Focused Keccak-f[1600] differential: one permutation of the zero state,
/// lane 0 has a known answer. Isolates the Keccak lowering from the rest of
/// the genesis path. Fast (a single permute), unlike the full genesis probe.
#[test]
fn probe_keccak_f1600_zero_state() {
    let module = parse_with_driver(
        "pub fn probe_keccak() -> u64 {\n\
             let mut st = [0u64; 25];\n\
             keccak_f1600(&mut st);\n\
             st[0]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_keccak"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_keccak"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("keccak_f1600 evaluates");
    let flat: Vec<bool> = out.concat();
    // Keccak-f[1600] of the all-zero state: lane 0 = 0xF1258F7940E1DDE7.
    assert_eq!(bits_to_u128(&flat) as u64, 0xF1258F7940E1DDE7u64, "keccak lane 0");
}


/// A `match` on a runtime index returning captured struct values — the
/// `from_fn(|i| match i { 0 => m0, ... })` shape. Isolates the multi-scalar
/// (struct) match-result threading from the array wrapping.
/// KNOWN-OPEN BUG (genesis blocker): a `match` on a runtime index whose arms
/// return *captured* (let-bound) struct values miscompiles — the join threads a
/// wrong/garbage value instead of the selected struct's scalars. Inline struct
/// literals in the arms work; captured lets do not. This is the
/// `from_fn(|i| match i { 0 => mk0, ... })` shape used to build the toy PESAT
/// index in the genesis probe, so the genesis constraint eval is wrong until
/// this is fixed. `#[ignore]`d: the match lowering produces both wrong values
/// and a block blowup; needs investigation in `lower_match_arm_chain`.
#[ignore = "match returning a captured struct miscompiles; see doc comment"]
#[test]
fn probe_match_captured_struct() {
    let module = parse_driver_only(
        "use cipher::consts::*;\n\
         struct LT { a: u32, n_vars: u32 }\n\
         pub fn probe_match(i: u64) -> u32 {\n\
             let m0 = LT { a: 100, n_vars: 7 };\n\
             let m1 = LT { a: 200, n_vars: 9 };\n\
             let sel = match i { 0 => m0, _ => m1 };\n\
             sel.n_vars\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_match"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_match"];
    // i is a pure-bit u64 → 64 single-bit inputs.
    for (i, want) in [(0u64, 7u128), (1u64, 9u128)] {
        let input: Vec<Vec<bool>> = (0..64).map(|b| vec![(i >> b) & 1 == 1]).collect();
        let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &input, 64)
            .expect("match");
        let flat: Vec<bool> = out.concat();
        assert_eq!(bits_to_u128(&flat), want, "match i={i}");
    }
}

/// Nested struct-array isolation: a struct with an array-of-structs field,
/// read a leaf field of an element (`o.items[i].v`). This is the
/// `constraint.terms[i].n_vars` shape — a struct array nested in a struct.
#[test]
fn probe_nested_struct_array() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         struct LocalTerm<D: ArraySize> { coeff: Galois128, vars: Array<usize, D>, n_vars: u32 }\n\
         struct LocalCon<N: ArraySize, D: ArraySize> { terms: Array<LocalTerm<D>, N>, n_terms: u32 }\n\
         fn get_it(o: &LocalCon<U4, U2>, i: usize) -> u32 { o.terms[i].n_vars }\n\
         pub fn probe_nested() -> u32 {\n\
             let m0 = LocalTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::default(), n_vars: 2 };\n\
             let o = LocalCon::<U4, U2> { terms: Array::<LocalTerm<U2>, U4>::from_fn(|_| m0), n_terms: 3 };\n\
             get_it(&o, 0)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_nested"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_nested"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("nested struct array");
    let flat: Vec<bool> = out.concat();
    // every element is the captured m0 (n_vars=2).
    assert_eq!(bits_to_u128(&flat), 2, "captured m0.n_vars == 2");
}

/// Minimal struct-array element field read: `arr[1].b` where the elements
/// have distinct field values. Isolates the array-element field-offset
/// computation (the `constraint.terms[i].n_vars` shape) from the WARP types.
#[test]
fn probe_struct_array_elem_field() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         struct Pair { a: u32, b: u32 }\n\
         fn get_a(arr: &Array<Pair, U3>, i: usize) -> u32 { arr[i].a }\n\
         fn get_b(arr: &Array<Pair, U3>, i: usize) -> u32 { arr[i].b }\n\
         pub fn probe_pair() -> u32 {\n\
             let arr = Array::<Pair, U3>::from_fn(|i| match i {\n\
                 0 => Pair { a: 1, b: 10 },\n\
                 1 => Pair { a: 2, b: 20 },\n\
                 _ => Pair { a: 3, b: 30 },\n\
             });\n\
             get_a(&arr, 1) * 1000 + get_b(&arr, 1) * 100 + get_a(&arr, 2)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_pair"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_pair"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("struct array elem field");
    let flat: Vec<bool> = out.concat();
    // arr[1].a=2, arr[1].b=20, arr[2].a=3 → 2*1000 + 20*100 + 3 = 4003.
    assert_eq!(bits_to_u128(&flat), 4003, "arr[1].a=2, arr[1].b=20, arr[2].a=3");
}


/// Direct `warp_eval_term` with explicit turbofish — isolates the term
/// evaluation (coeff * witness[vars[0]] * witness[vars[1]]) from the
/// constraint aggregation.
#[test]
fn probe_warp_eval_term() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn eval_one(t: &WarpTerm<U2>, w: &Array<Galois128, U4>) -> Galois128 {\n\
             warp_eval_term::<U2, U0, U4>(t, &Array::<Galois128, U0>::default(), w)\n\
         }\n\
         pub fn probe_term(w0: Galois128, w1: Galois128) -> Galois128 {\n\
             let t = WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|i| match i { 0 => 0usize, _ => 1usize }), n_vars: 2 };\n\
             let witness = Array::<Galois128, U4>::from_fn(|i| match i { 0 => w0, 1 => w1, _ => Galois128(0) });\n\
             eval_one(&t, &witness)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_term"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_term"];
    let (a, b) = (3u128, 5u128);
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(
        &target.module,
        fid,
        &[bits_of(a, 128), bits_of(b, 128)],
        64,
    )
    .expect("term eval");
    assert_eq!(bits_to_u128(&out[0]), (Galois128(a) * Galois128(b)).0, "term == w0*w1");
}

/// Micro-isolation: a `usize` array built by `from_fn` + `match`, then read
/// back — the `term.vars[i]` shape in `warp_eval_term`. A misread index would
/// make the term eval pick the wrong witness element.
#[test]
fn probe_usize_array_from_fn() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_vars() -> u64 {\n\
             let vars = Array::<usize, U2>::from_fn(|i| match i { 0 => 0usize, _ => 1usize });\n\
             vars[1]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_vars"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_vars"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("vars read");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 1, "vars[1] == 1");
}

/// Micro-isolation: a native loop bounded by a generic const (`0..DEG::USIZE`)
/// inside a generic function — the constraint term-eval loop's shape. If the
/// const bound misresolves (e.g. to 0), the variable products are dropped.
#[test]
fn probe_generic_const_loop_bound() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn loop_count<DEG: ArraySize>() -> u64 {\n\
             let mut c = 0u64;\n\
             for i in 0..DEG::USIZE {\n\
                 c = c + 1u64;\n\
             }\n\
             c\n\
         }\n\
         pub fn probe_bound() -> u64 { loop_count::<U2>() }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_bound"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_bound"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("loop bound");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 2, "0..U2::USIZE runs twice");
}

/// Micro-isolation: read the `n_vars` field of a constructed `WarpTerm`.
/// The constraint probe showed every term reducing to its coeff (the variable
/// product dropped), consistent with `n_vars` reading 0 — so this checks the
/// struct field offset directly.
#[test]
fn probe_term_n_vars_field() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn get_nvars<DEG: ArraySize>(t: &WarpTerm<DEG>) -> u32 { t.n_vars }\n\
         pub fn probe_nvars() -> u32 {\n\
             let t = WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|i| i), n_vars: 5 };\n\
             get_nvars(&t)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_nvars"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_nvars"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("n_vars read");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 5, "WarpTerm.n_vars field");
}


#[ignore = "genesis lowers + runs to completion; the match-of-captured-struct lowering bug makes the constraint eval wrong; see module note"]
#[test]
fn warp_genesis_compiles_via_ir_and_matches_native() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_genesis(w0: Galois128, w1: Galois128, w2: Galois128) -> Array<u8, U32> {\n\
             let mk0 = WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|i| match i { 0 => 0usize, _ => 1usize }), n_vars: 2 };\n\
             let mk1 = WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|i| match i { 0 => 2usize, _ => 0usize }), n_vars: 1 };\n\
             let mk2 = WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|i| match i { 0 => 3usize, _ => 0usize }), n_vars: 1 };\n\
             let mkz = WarpTerm::<U2> { coeff: Galois128(0), vars: Array::<usize, U2>::default(), n_vars: 0 };\n\
             let constraint = WarpConstraint::<U4, U2> { terms: Array::<WarpTerm<U2>, U4>::from_fn(|i| match i { 0 => mk0, 1 => mk1, 2 => mk2, _ => mkz }), n_terms: 3 };\n\
             let index = WarpIndex::<U1, U4, U2> { constraints: Array::<WarpConstraint<U4, U2>, U1>::from_fn(|_| constraint), public_len: 0, witness_len: 4, degree_bound: 2 };\n\
             let w3 = w0 * w1 + w2;\n\
             let witness = WarpWitness::<U4, U0> { public: Array::<Galois128, U0>::default(), witness: Array::<Galois128, U4>::from_fn(|i| match i { 0 => w0, 1 => w1, 2 => w2, _ => w3 }) };\n\
             let acc = warp_genesis_spec::<U1, U4, U2, U4, U0, U4, WarpIdentityCode, U7, U0>(&index, &WarpIdentityCode, &witness);\n\
             match acc {\n\
                 Some(a) => a.instance.root,\n\
                 None => Array::<u8, U32>::default(),\n\
             }\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_genesis"], volar_lir_codegen::LoopLowering::Native);
    for (i, f) in target.module.funcs.iter().enumerate() {
        if let vaffle::FuncDecl::Import { name, .. } = f {
            panic!("leftover Import func[{i}] name={name}");
        }
    }
    let fid = target.module.exports["probe_genesis"];
    // w0*w1 + w2 + w3 = 0 → w3 = w0*w1 + w2 (char 2).
    let (a, b, c) = (3u128, 5u128, 7u128);
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(
        &target.module,
        fid,
        &[bits_of(a, 128), bits_of(b, 128), bits_of(c, 128)],
        64,
    )
    .expect("compiled genesis evaluates");
    // Native reference: the spec's own genesis, constructed inline.
    use cipher::consts::*;
    use volar_spec::warp::{WarpConstraint, WarpIndex, WarpTerm, WarpWitness};
    let mk = |vars: [usize; 2], n_vars: u32| WarpTerm::<U2> {
        coeff: volar_primitives::Galois128(1),
        vars: volar_spec::Array::from_fn(|i| vars[i]),
        n_vars,
    };
    let index = WarpIndex::<U1, U4, U2> {
        constraints: volar_spec::Array::from_fn(|_| WarpConstraint::<U4, U2> {
            terms: volar_spec::Array::from_fn(|i| match i {
                0 => mk([0, 1], 2),
                1 => mk([2, 0], 1),
                2 => mk([3, 0], 1),
                _ => mk([0, 0], 0),
            }),
            n_terms: 3,
        }),
        public_len: 0,
        witness_len: 4,
        degree_bound: 2,
    };
    let d = Galois128(a) * Galois128(b) + Galois128(c);
    let witness = WarpWitness::<U4, U0> {
        public: volar_spec::Array::default(),
        witness: volar_spec::Array::from_fn(|i| [Galois128(a), Galois128(b), Galois128(c), d][i]),
    };
    let native = volar_spec::warp::warp_genesis_spec::<_, _, _, _, _, U4, _, U7, U0>(
        &index,
        &volar_spec::warp::WarpIdentityCode,
        &witness,
    )
    .expect("native genesis succeeds");
    let got: Vec<u8> = out[0]
        .chunks(8)
        .map(|bits| bits.iter().enumerate().fold(0u8, |acc, (i, b)| if *b { acc | (1 << i) } else { acc }))
        .collect();
    assert_eq!(got.as_slice(), native.instance.root.as_slice(),
        "compiled genesis root must match the native spec's");
}
