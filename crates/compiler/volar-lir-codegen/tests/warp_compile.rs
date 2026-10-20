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
/// THE M4 GATE: this passes end-to-end (~240s). With `LoopLowering::Native`
/// the lowering completes, and the interpreter (dense value table, shared
/// cell-granular stack, disjoint spill/sp regions) evaluates the full
/// Keccak-sponge Merkle tree to exactly the native root. The toy PESAT index
/// inlines its `WarpTerm` struct literals into the `from_fn` match arms to
/// sidestep the still-open match-of-captured-struct bug
/// (`probe_match_captured_struct`, `#[ignore]`d). The lighter probes above
/// decompose the gate: Keccak permutation, sponge absorb/finalize/read,
/// leaf encoding, and the two-call absorb schedule each have their own
/// focused differential.
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


/// Minimal struct-pointer field-write test: a callee writes a field of a
/// `&mut Struct` and the caller reads it back. This is the sponge's
/// write-back pattern (`sponge.state = state`).
#[test]
fn probe_struct_ref_field_write() {
    let module = parse_driver_only(
        "struct S { x: u32 }\n\
         fn set_x(s: &mut S) { s.x = 42; }\n\
         pub fn probe_sref() -> u32 {\n\
             let mut s = S { x: 0 };\n\
             set_x(&mut s);\n\
             s.x\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_sref"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_sref"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("struct ref field write");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 42, "callee's field write must reach the caller");
}

/// Byte<->lane conversion round-trip: `u64::from_le_bytes(x.to_le_bytes())`
/// must be the identity. Isolates the conversions used by permute_bytes.
#[test]
fn probe_le_bytes_roundtrip() {
    let module = parse_driver_only(
        "pub fn probe_le(x: u64) -> u64 {\n\
             let b = x.to_le_bytes();\n\
             u64::from_le_bytes(b)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_le"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_le"];
    let input: Vec<Vec<bool>> = (0..64).map(|i| vec![(0x1234567890ABCDEFu64 >> i) & 1 == 1]).collect();
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &input, 64)
        .expect("le roundtrip");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0x1234567890ABCDEFu64, "from_le_bytes(to_le_bytes(x)) == x");
}

/// Isolate a range-index + copy_from_slice write to a `&mut` *parameter*
/// pointer (the `lanes_to_bytes` write-back shape: `b[i*8..i*8+8]`).
#[test]
fn probe_range_write_param_ptr() {
    let module = parse_driver_only(
        "fn write_into(s: &mut [u8; 16]) {\n\
             let src = [9u8, 8, 7, 6];\n\
             s[4..8].copy_from_slice(&src);\n\
         }\n\
         pub fn probe_rangewrite() -> u64 {\n\
             let mut b = [0u8; 16];\n\
             write_into(&mut b);\n\
             b[4] as u64\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_rangewrite"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_rangewrite"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("range write evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 9, "b[4] == 9 after the range write");
}

/// KNOWN-OPEN (genesis blocker): passing a `&mut` param pointer through TWO
/// function calls (caller -> outer -> helper) loses the callee's write — the
/// second callee's store doesn't reach the original caller's memory. This is
/// the `permute_bytes` shape (state_bytes -> bytes_to_lanes / keccak_f1600 /
/// lanes_to_bytes). Single-callee writes work (probe_read_write_param_ptr);
/// the double handoff is the open bug. `#[ignore]`d pending a fix.
#[test]
fn probe_readwrite_via_calls() {
    let module = parse_driver_only(
        "fn read_it(s: &[u64; 4]) -> u64 { s[0] }\n\
         fn write_it(s: &mut [u64; 4], v: u64) { s[0] = v; }\n\
         fn outer(s: &mut [u64; 4]) {\n\
             let x = read_it(s);\n\
             write_it(s, x + 1);\n\
         }\n\
         pub fn probe_rwc() -> u64 {\n\
             let mut b = [0u64; 4];\n\
             b[0] = 41;\n\
             outer(&mut b);\n\
             b[0]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_rwc"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_rwc"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("readwrite via calls");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 42, "b[0] == 42 after read+write via calls");
}

/// Read-then-write on a `&mut` param pointer (the `permute_bytes` shape:
/// bytes_to_lanes reads the state, lanes_to_bytes writes it back).
#[test]
fn probe_read_write_param_ptr() {
    let module = parse_driver_only(
        "fn read_write(s: &mut [u64; 16]) {\n\
             let x = s[0];\n\
             s[1] = x + 1u64;\n\
         }\n\
         pub fn probe_rw() -> u64 {\n\
             let mut b = [0u64; 16];\n\
             b[0] = 41u64;\n\
             read_write(&mut b);\n\
             b[1]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_rw"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_rw"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("read-write evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 42, "b[1] == b[0]+1 == 42");
}

/// The full permute chain with a LOCAL output array (vs the param-pointer
/// write-back in `permute_bytes`): isolates whether writing back to a
/// `&mut [u8;200]` *parameter* is the broken step.
#[test]
fn probe_full_chain_local_out() {
    let module = parse_with_driver(
        "pub fn probe_full() -> u64 {\n\
             let mut state = [0u8; 200];\n\
             let mut st = [0u64; 25];\n\
             bytes_to_lanes(&state, &mut st);\n\
             keccak_f1600(&mut st);\n\
             let mut out_state = [0u8; 200];\n\
             lanes_to_bytes(&st, &mut out_state);\n\
             let mut out = 0u64;\n\
             for i in 0..8 {\n\
                 out = out | ((out_state[i] as u64) << (i * 8));\n\
             }\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_full"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_full"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("full chain evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0xF1258F7940E1DDE7u64, "full chain lane 0");
}

/// Isolate the permute chain's shared local: `bytes_to_lanes` then
/// `keccak_f1600` both take `&mut st` — the second call must reuse the first's
/// promoted slot (not a fresh copy).
#[test]
fn probe_permute_chain() {
    let module = parse_with_driver(
        "pub fn probe_chain() -> u64 {\n\
             let mut state = [0u8; 200];\n\
             let mut st = [0u64; 25];\n\
             bytes_to_lanes(&state, &mut st);\n\
             keccak_f1600(&mut st);\n\
             st[0]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_chain"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_chain"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("chain evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0xF1258F7940E1DDE7u64, "chained permute lane 0");
}

/// Isolate `bytes_to_lanes` (the permute read): write a known byte pattern and
/// read lane 0 back as a u64.
#[test]
fn probe_bytes_to_lanes() {
    let module = parse_with_driver(
        "pub fn probe_b2l() -> u64 {\n\
             let mut b = [0u8; 200];\n\
             b[0] = 0xE7; b[1] = 0xDD; b[2] = 0xE1; b[3] = 0x40;\n\
             b[4] = 0x79; b[5] = 0x8F; b[6] = 0x25; b[7] = 0xF1;\n\
             let mut st = [0u64; 25];\n\
             bytes_to_lanes(&b, &mut st);\n\
             st[0]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_b2l"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_b2l"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("bytes_to_lanes evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0xF1258F7940E1DDE7u64, "lane 0 from LE bytes");
}

/// Isolate `lanes_to_bytes` (the permute write-back): write a known lane into
/// a zero byte state and read the first 8 bytes back as a u64.
#[test]
fn probe_lanes_to_bytes() {
    let module = parse_with_driver(
        "pub fn probe_l2b() -> u64 {\n\
             let mut st = [0u64; 25];\n\
             st[0] = 0xF1258F7940E1DDE7u64;\n\
             let mut b = [0u8; 200];\n\
             lanes_to_bytes(&st, &mut b);\n\
             let mut out = 0u64;\n\
             for i in 0..8 {\n\
                 out = out | ((b[i] as u64) << (i * 8));\n\
             }\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_l2b"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_l2b"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("lanes_to_bytes evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0xF1258F7940E1DDE7u64, "lane 0 LE bytes");
}

/// `le16` (the field→bytes conversion for the Merkle leaf encoding): the
/// field cell expands to its 128-bit view and returns 16 LE byte scalars.
#[test]
fn probe_le16() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_le16() -> u64 {\n\
             let b = le16(Galois128(0x0102));\n\
             let mut out = 0u64;\n\
             for i in 0..8 {\n\
                 out = out | ((b[i] as u64) << (i * 8));\n\
             }\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_le16"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_le16"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("le16 evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0x0102u64, "le16(Galois128(0x0102)) LE bytes");
}

/// Merkle leaf-hash differential: `warp_hash_leaf_field(Galois128(5))` vs the
/// native spec. Isolates the leaf encoding (`le16`) + absorb schedule from the
/// tree structure. The sponge absorb loops are memory-backed (cell-granular
/// storage); this runs in ~20s.
#[test]
fn probe_hash_leaf() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_leaf() -> Array<u8, U32> {\n\
             warp_hash_leaf_field(Galois128(5))\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_leaf"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_leaf"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("leaf hash evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    // Native: sha3_256 of [0x00] ++ le16(Galois128(5)).
    let mut input = vec![0x00u8];
    input.extend_from_slice(&5u128.to_le_bytes());
    let native = volar_spec::keccak::spec_sha3_256(&input);
    assert_eq!(got.as_slice(), native.as_slice(), "leaf hash of Galois128(5)");
}

/// KNOWN-OPEN (genesis blocker): `permute_bytes` of the zero state yields zeros
/// — the absorb path now works (struct refs), but the byte<->lane conversion
/// write-back (`lanes_to_bytes`' `b[i*8..i*8+8].copy_from_slice(..)` in nested
/// native loops) doesn't land. `keccak_f1600` itself is correct.
#[test]
fn probe_permute_bytes() {
    let module = parse_with_driver(
        "pub fn probe_permute() -> u64 {\n\
             let mut state = [0u8; 200];\n\
             permute_bytes(&mut state);\n\
             let mut out = 0u64;\n\
             for i in 0..8 {\n\
                 out = out | ((state[i] as u64) << (i * 8));\n\
             }\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_permute"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_permute"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("permute evaluates");
    let flat: Vec<bool> = out.concat();
    // Keccak-f[1600] zero-state lane 0 = 0xF1258F7940E1DDE7 (LE bytes).
    assert_eq!(bits_to_u128(&flat) as u64, 0xF1258F7940E1DDE7u64, "permute zero state lane 0");
}

/// KNOWN-OPEN BUG (genesis blocker): the spec sponge's absorb doesn't land —
/// the compiled `spec_sha3_256` of a fixed input diverges from native (the
/// output is the finalize suffix + zeros, i.e. the absorbed bytes never reach
/// the state). The Keccak permutation itself is correct
/// (`probe_keccak_f1600_zero_state`), so the divergence is in the sponge's
/// `&mut SpecSponge` / nested-reference chain. `#[ignore]`d pending a fix.
#[test]
fn probe_sponge_sha3() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_sponge() -> Array<u8, U32> {\n\
             let mut s = spec_sponge_new(SHA3_256_RATE);\n\
             let data = Array::<u8, U3>::from_fn(|i| match i { 0 => 0x61u8, 1 => 0x62u8, _ => 0x63u8 });\n\
             spec_sponge_absorb_array(&mut s, &data);\n\
             let mut r = spec_sponge_finalize_xof(&s, 0x06);\n\
             let mut out = Array::<u8, U32>::default();\n\
             spec_xof_read_array(&mut r, &mut out);\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_sponge"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_sponge"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("sponge evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat
        .chunks(8)
        .map(|bits| bits.iter().enumerate().fold(0u8, |acc, (i, b)| if *b { acc | (1 << i) } else { acc }))
        .collect();
    let native = volar_spec::keccak::spec_sha3_256(b"abc");
    assert_eq!(got.as_slice(), native.as_slice(), "compiled sponge == native sha3_256");
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
#[test]
#[ignore = "match-of-captured-struct mis-threading in lower_match_arm_chain (open)"]
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


#[test]
fn warp_genesis_compiles_via_ir_and_matches_native() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_genesis(w0: Galois128, w1: Galois128, w2: Galois128) -> Array<u8, U32> {\n\
             // Inline struct literals in the match arms (not captured lets) to
             // sidestep the open match-of-captured-struct lowering bug.
             let constraint = WarpConstraint::<U4, U2> { terms: Array::<WarpTerm<U2>, U4>::from_fn(|i| match i {\
                 0 => WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|j| match j { 0 => 0usize, _ => 1usize }), n_vars: 2 },\
                 1 => WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|j| match j { 0 => 2usize, _ => 0usize }), n_vars: 1 },\
                 2 => WarpTerm::<U2> { coeff: Galois128(1), vars: Array::<usize, U2>::from_fn(|j| match j { 0 => 3usize, _ => 0usize }), n_vars: 1 },\
                 _ => WarpTerm::<U2> { coeff: Galois128(0), vars: Array::<usize, U2>::default(), n_vars: 0 }\
             }), n_terms: 3 };\n\
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
    // The return is Array<u8, U32> flattened to 32 separate byte scalars (one
    // IrValue per scalar) — concatenate them all, then chunk into bytes.
    let flat_bits: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat_bits
        .chunks(8)
        .map(|bits| bits.iter().enumerate().fold(0u8, |acc, (i, b)| if *b { acc | (1 << i) } else { acc }))
        .collect();
    assert_eq!(got.as_slice(), native.instance.root.as_slice(),
        "compiled genesis root must match the native spec's");
}

/// Minimal repro for the absorb-loop blowup: a native loop writing
/// `state[pos] ^= data[i]` with a runtime `pos` over a local [u8; 200].
/// The promoted array should be memory-backed: ~1.6K stmts total, not ~5M.
#[test]
fn probe_runtime_index_write_size() {
    let module = parse_driver_only(
        "pub fn probe_absorb() -> u8 {\n\
             let mut state = [0u8; 200];\n\
             let mut pos = 0usize;\n\
             let data = [1u8, 2, 3, 4];\n\
             for i in 0..4 {\n\
                 state[pos] ^= data[i];\n\
                 pos += 1;\n\
             }\n\
             state[3]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_absorb"], volar_lir_codegen::LoopLowering::Native);
    // Size regression guard: the promoted [u8; 200] init + a 4-iteration
    // native loop with a runtime-indexed `^=` must stay memory-backed — the
    // cell-granular storage model keeps this in the tens of thousands of
    // values (the per-bit select-mux model produced ~4M).
    for f in target.module.funcs.iter() {
        if let vaffle::FuncDecl::Body(b) = f {
            assert!(
                b.values.len() < 500_000,
                "absorb loop module blew up: {} values (select-mux regression?)",
                b.values.len()
            );
        }
    }
    let fid = target.module.exports["probe_absorb"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 4u8 as u64, "state[3] == 4 after absorb");
}

/// Size regression guard: a `[u8; 200]` repeat + one read must stay in the
/// tens of thousands of values (the pre-cell-storage model produced ~3.7M).
#[test]
fn probe_repeat_alone_size() {
    let module = parse_driver_only(
        "pub fn probe_r() -> u8 {\n\
             let state = [0u8; 200];\n\
             state[3]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_r"], volar_lir_codegen::LoopLowering::Native);
    for f in target.module.funcs.iter() {
        if let vaffle::FuncDecl::Body(b) = f {
            assert!(
                b.values.len() < 500_000,
                "[0u8; 200] init blew up: {} values (per-bit storage regression?)",
                b.values.len()
            );
        }
    }
    let fid = target.module.exports["probe_r"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 0u64, "state[3] == 0");
}

/// Size + value guard for a mutable `[u8; 200]` with a constant-index write.
#[test]
fn probe_mut_repeat_alone_size() {
    let module = parse_driver_only(
        "pub fn probe_r() -> u8 {\n\
             let mut state = [0u8; 200];\n\
             state[0] = 7u8;\n\
             state[3] + state[0]\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_r"], volar_lir_codegen::LoopLowering::Native);
    for f in target.module.funcs.iter() {
        if let vaffle::FuncDecl::Body(b) = f {
            assert!(
                b.values.len() < 500_000,
                "mutable [u8; 200] blew up: {} values",
                b.values.len()
            );
        }
    }
    let fid = target.module.exports["probe_r"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat) as u64, 7u64, "state[3] + state[0] == 7");
}

/// Two sequential absorbs into one sponge — the hash_leaf absorb schedule.
#[test]
fn probe_two_absorbs() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_2a() -> Array<u8, U32> {\n\
             let mut h = spec_sponge_new(SHA3_256_RATE);\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x61u8));\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U2>::from_fn(|i| if i == 0 { 0x62u8 } else { 0x63u8 }));\n\
             let mut reader = spec_sponge_finalize_xof(&h, 0x06u8);\n\
             let mut out: Array<u8, U32> = Array::<u8, U32>::default();\n\
             spec_xof_read_array(&mut reader, &mut out);\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_2a"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_2a"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("two-absorb evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    let native = volar_spec::keccak::spec_sha3_256(b"abc");
    assert_eq!(got.as_slice(), native.as_slice(), "two-absorb sha3(abc)");
}

/// hash_leaf shape with CONSTANT 16 payload bytes (no le16 call).
#[test]
fn probe_leaf_const_bytes() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_lcb() -> Array<u8, U32> {\n\
             let mut h = spec_sponge_new(SHA3_256_RATE);\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x00u8));\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U16>::from_fn(|i| if i == 0 { 5u8 } else { 0u8 }));\n\
             let mut reader = spec_sponge_finalize_xof(&h, 0x06u8);\n\
             let mut out: Array<u8, U32> = Array::<u8, U32>::default();\n\
             spec_xof_read_array(&mut reader, &mut out);\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_lcb"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_lcb"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    let mut input = vec![0x00u8];
    input.extend_from_slice(&5u128.to_le_bytes());
    let native = volar_spec::keccak::spec_sha3_256(&input);
    assert_eq!(got.as_slice(), native.as_slice(), "const-bytes leaf hash");
}

/// hash_leaf shape with the real le16(Galois128(5)) payload.
#[test]
fn probe_leaf_le16_bytes() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_llb() -> Array<u8, U32> {\n\
             let mut h = spec_sponge_new(SHA3_256_RATE);\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x00u8));\n\
             let v_bytes: Array<u8, U16> = le16(Galois128(5));\n\
             spec_sponge_absorb_array(&mut h, &v_bytes);\n\
             let mut reader = spec_sponge_finalize_xof(&h, 0x06u8);\n\
             let mut out: Array<u8, U32> = Array::<u8, U32>::default();\n\
             spec_xof_read_array(&mut reader, &mut out);\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_llb"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_llb"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    let mut input = vec![0x00u8];
    input.extend_from_slice(&5u128.to_le_bytes());
    let native = volar_spec::keccak::spec_sha3_256(&input);
    assert_eq!(got.as_slice(), native.as_slice(), "le16-bytes leaf hash");
}

/// Dump all 16 bytes of le16(Galois128(5)).
#[test]
fn probe_le16_all_bytes() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_l16() -> Array<u8, U16> {\n\
             le16(Galois128(5))\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_l16"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_l16"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    let native = 5u128.to_le_bytes();
    assert_eq!(got.as_slice(), native.as_slice(), "le16 all bytes");
}

/// Const 16 bytes bound to a local then passed by reference.
#[test]
fn probe_leaf_letbound_bytes() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_lbb() -> Array<u8, U32> {\n\
             let mut h = spec_sponge_new(SHA3_256_RATE);\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x00u8));\n\
             let v_bytes: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 0 { 5u8 } else { 0u8 });\n\
             spec_sponge_absorb_array(&mut h, &v_bytes);\n\
             let mut reader = spec_sponge_finalize_xof(&h, 0x06u8);\n\
             let mut out: Array<u8, U32> = Array::<u8, U32>::default();\n\
             spec_xof_read_array(&mut reader, &mut out);\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_lbb"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_lbb"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    let mut input = vec![0x00u8];
    input.extend_from_slice(&5u128.to_le_bytes());
    let native = volar_spec::keccak::spec_sha3_256(&input);
    assert_eq!(got.as_slice(), native.as_slice(), "let-bound const-bytes leaf hash");
}

/// Mem-backed let-bound array: store then read back, no call involved.
#[test]
fn probe_letbound_array_read() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_lar() -> u64 {\n\
             let a: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 3 { 42u8 } else { 0u8 });\n\
             (a[0] as u64) + 10 * (a[3] as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_lar"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_lar"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 420, "a[0] + 10*a[3] == 420");
}

/// Reference-to-array call arg: `read1(&a)` where `a` is a mem-backed local.
#[test]
fn probe_letbound_array_ref_call() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn read1(a: &Array<u8, U16>) -> u64 {\n\
             (a[0] as u64) + 10 * (a[3] as u64)\n\
         }\n\
         pub fn probe_lrc() -> u64 {\n\
             let a: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 3 { 42u8 } else { 0u8 });\n\
             read1(&a)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_lrc"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_lrc"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 420, "read1(&a) == 420");
}

/// Two-reference call: &mut array + &array, non-generic callee.
#[test]
fn probe_two_refs_nongeneric() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn put(s: &mut Array<u8, U4>, d: &Array<u8, U16>) {\n\
             s[0] = d[3];\n\
         }\n\
         pub fn probe_2r() -> u64 {\n\
             let mut s: Array<u8, U4> = Array::<u8, U4>::default();\n\
             let d: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 3 { 42u8 } else { 0u8 });\n\
             put(&mut s, &d);\n\
             s[0] as u64\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_2r"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_2r"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 42, "s[0] == 42 after put(&mut s, &d)");
}

/// Generic two-reference call: &mut array + &array<u8, N>.
#[test]
fn probe_two_refs_generic() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn putg<N: ArraySize>(s: &mut Array<u8, U4>, d: &Array<u8, N>) {\n\
             s[0] = d[3];\n\
         }\n\
         pub fn probe_2g() -> u64 {\n\
             let mut s: Array<u8, U4> = Array::<u8, U4>::default();\n\
             let d: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 3 { 42u8 } else { 0u8 });\n\
             putg(&mut s, &d);\n\
             s[0] as u64\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_2g"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_2g"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 42, "s[0] == 42 after putg(&mut s, &d)");
}

/// Native-loop runtime-indexed copy from a reference param into a &mut array.
#[test]
fn probe_loop_copy_from_ref() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         fn abs_tiny(s: &mut Array<u8, U32>, d: &Array<u8, U16>) {\n\
             let mut pos = 0usize;\n\
             for i in 0..16 {\n\
                 s[pos] = d[i];\n\
                 pos += 1;\n\
             }\n\
         }\n\
         pub fn probe_lcr() -> u64 {\n\
             let mut s: Array<u8, U32> = Array::<u8, U32>::default();\n\
             let d: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| i as u8);\n\
             abs_tiny(&mut s, &d);\n\
             (s[3] as u64) + 100 * (s[15] as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_lcr"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_lcr"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 1503, "s[3] + 100*s[15] == 3 + 1500");
}

/// ONE absorb of 16 bytes (the spilled-param case), no tag absorb first.
#[test]
fn probe_absorb_16() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_a16() -> Array<u8, U32> {\n\
             let mut h = spec_sponge_new(SHA3_256_RATE);\n\
             spec_sponge_absorb_array(&mut h, &Array::<u8, U16>::from_fn(|i| if i == 0 { 5u8 } else { 0u8 }));\n\
             let mut reader = spec_sponge_finalize_xof(&h, 0x06u8);\n\
             let mut out: Array<u8, U32> = Array::<u8, U32>::default();\n\
             spec_xof_read_array(&mut reader, &mut out);\n\
             out\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_a16"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_a16"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got: Vec<u8> = flat.chunks(8).map(|bits| bits.iter().enumerate().fold(0u8, |acc,(i,b)| if *b {acc|(1<<i)} else {acc})).collect();
    let native = volar_spec::keccak::spec_sha3_256(&5u128.to_le_bytes());
    assert_eq!(got.as_slice(), native.as_slice(), "absorb-16 sha3");
}

/// Minimal two-call sponge replica with a driver-local struct.
#[test]
fn probe_two_absorb_struct() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         struct MySp { state: [u8; 32], rate: usize, pos: usize }\n\
         fn new_sp() -> MySp { MySp { state: [0u8; 32], rate: 32, pos: 0 } }\n\
         fn abs1(sp: &mut MySp) {\n\
             let mut st = sp.state;\n\
             st[sp.pos] ^= 0x61u8;\n\
             sp.state = st;\n\
             sp.pos = sp.pos + 1;\n\
         }\n\
         fn abs16(sp: &mut MySp, d: &Array<u8, U16>) {\n\
             let mut state = sp.state;\n\
             let mut pos = sp.pos;\n\
             for i in 0..16 {\n\
                 state[pos] ^= d[i];\n\
                 pos += 1;\n\
             }\n\
             sp.state = state;\n\
             sp.pos = pos;\n\
         }\n\
         pub fn probe_tas() -> u64 {\n\
             let mut sp = new_sp();\n\
             abs1(&mut sp);\n\
             let d: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 0 { 5u8 } else { 0u8 });\n\
             abs16(&mut sp, &d);\n\
             (sp.state[0] as u64) + 10 * (sp.state[1] as u64) + 1000 * (sp.pos as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_tas"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_tas"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    // state[0]=0x61, state[1]=5, pos=17 → 0x61 + 50 + 17000 = 97 + 50 + 17000 = 17147
    assert_eq!(bits_to_u128(&flat), 17147, "state[0]=0x61 state[1]=5 pos=17");
}

/// Same two-call shape but abs16 writes a CONSTANT (no d read).
#[test]
fn probe_two_absorb_struct_constwrite() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         struct MySp { state: [u8; 32], rate: usize, pos: usize }\n\
         fn new_sp() -> MySp { MySp { state: [0u8; 32], rate: 32, pos: 0 } }\n\
         fn abs1(sp: &mut MySp) {\n\
             let mut st = sp.state;\n\
             st[sp.pos] ^= 0x61u8;\n\
             sp.state = st;\n\
             sp.pos = sp.pos + 1;\n\
         }\n\
         fn abs16(sp: &mut MySp, d: &Array<u8, U16>) {\n\
             let mut state = sp.state;\n\
             let mut pos = sp.pos;\n\
             for i in 0..16 {\n\
                 state[pos] = d[i] + 9u8;\n\
                 pos += 1;\n\
             }\n\
             sp.state = state;\n\
             sp.pos = pos;\n\
         }\n\
         pub fn probe_tcw() -> u64 {\n\
             let mut sp = new_sp();\n\
             abs1(&mut sp);\n\
             let d: Array<u8, U16> = Array::<u8, U16>::from_fn(|i| if i == 0 { 5u8 } else { 0u8 });\n\
             abs16(&mut sp, &d);\n\
             (sp.state[0] as u64) + 10 * (sp.state[1] as u64) + 1000 * (sp.state[2] as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_tcw"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_tcw"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    // state[0]=0x61, state[1]=d[0]+9=14, state[2]=d[1]+9=9 → 97 + 140 + 9000 = 9237
    assert_eq!(bits_to_u128(&flat), 9237, "state[0]=0x61 state[1]=14 state[2]=9");
}

/// ^= on a promoted local in a native loop, constant rhs: single application?
#[test]
fn probe_assignop_promoted() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_aop() -> u64 {\n\
             let mut state = [0u8; 32];\n\
             let mut pos = 1usize;\n\
             for i in 0..4 {\n\
                 state[pos] ^= 5u8;\n\
                 pos += 1;\n\
             }\n\
             (state[1] as u64) + 10 * (state[2] as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_aop"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_aop"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 55, "state[1]=5 state[2]=5");
}

/// ^= on a struct-destructured local inside a callee (single call).
#[test]
fn probe_assignop_struct_destructure() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         struct MySp { state: [u8; 32], rate: usize, pos: usize }\n\
         fn new_sp() -> MySp { MySp { state: [0u8; 32], rate: 32, pos: 0 } }\n\
         fn abs16(sp: &mut MySp) {\n\
             let mut state = sp.state;\n\
             let mut pos = sp.pos;\n\
             for i in 0..4 {\n\
                 state[pos] ^= 5u8;\n\
                 pos += 1;\n\
             }\n\
             sp.state = state;\n\
             sp.pos = pos;\n\
         }\n\
         pub fn probe_asd() -> u64 {\n\
             let mut sp = new_sp();\n\
             abs16(&mut sp);\n\
             (sp.state[0] as u64) + 10 * (sp.state[1] as u64) + 100 * (sp.pos as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_asd"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_asd"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    assert_eq!(bits_to_u128(&flat), 455, "state[0]=5 state[1]=5 pos=4");
}

/// Two sequential calls; the second ^=s into the destructured state.
#[test]
fn probe_assignop_two_calls() {
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         struct MySp { state: [u8; 32], rate: usize, pos: usize }\n\
         fn new_sp() -> MySp { MySp { state: [0u8; 32], rate: 32, pos: 0 } }\n\
         fn abs1(sp: &mut MySp) {\n\
             let mut st = sp.state;\n\
             st[sp.pos] ^= 0x61u8;\n\
             sp.state = st;\n\
             sp.pos = sp.pos + 1;\n\
         }\n\
         fn abs16(sp: &mut MySp) {\n\
             let mut state = sp.state;\n\
             let mut pos = sp.pos;\n\
             for i in 0..4 {\n\
                 state[pos] ^= 5u8;\n\
                 pos += 1;\n\
             }\n\
             sp.state = state;\n\
             sp.pos = pos;\n\
         }\n\
         pub fn probe_a2c() -> u64 {\n\
             let mut sp = new_sp();\n\
             abs1(&mut sp);\n\
             abs16(&mut sp);\n\
             (sp.state[0] as u64) + 10 * (sp.state[1] as u64) + 1000 * (sp.pos as u64)\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_a2c"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_a2c"];
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &[], 64)
        .expect("evaluates");
    let flat: Vec<bool> = out.concat();
    // abs16 starts at pos=1 (abs1's write), so state[0] stays 0x61;
    // state[1..5] ^= 5; pos = 5 → 97 + 50 + 5000 = 5147.
    assert_eq!(bits_to_u128(&flat), 5147, "state[0]=0x61 state[1]=5 pos=5");
}

// ============================================================================
// E5: independent per-step proof — the satisfaction checker for an emitted
// step instance compiled via IR, run in-circuit (plan §6 E5).
// ============================================================================

/// The minimal self-loop step circuit's emitted instance, flattened for the
/// const-generic in-circuit driver. Returns (flattened, honest witness as
/// field elements [public.., witness..]).
fn e5_self_loop_instance() -> (volar_pesat_emit::FlattenedIndex, Vec<volar_primitives::Galois128>) {
    use volar_ir::ir::*;
    use volar_ir_common::{Constant, IrType, Node, PolyCoeffs, Type, TypeId, TypeTable};
    let types = TypeTable(vec![IrType::Primitive(Type::Bit)]);
    let mk = |kind: IRStmt| Node::new(kind, (), None);
    // block0 (param b: Bit): out = b XOR 1; if out==1 loop else return out.
    let block0 = IRBlock {
        params: vec![TypeId(0)],
        stmts: vec![mk(IRStmt::Poly {
            ty: TypeId(0),
            coeffs: PolyCoeffs::from_iter([(vec![IRVarId(0)], 1u8)]),
            constant: Constant { hi: 0, lo: 1 },
        })],
        terminator: IRTerminator::JumpCond {
            condition: IRVarId(1),
            then_target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(0)), vec![IRVarId(1)]),
            else_target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(1)]),
        },
    };
    let blocks = IRBlocks::new(vec![block0]);
    let bit_ty = IRTypeId(0);
    let circuit = volar_ir_passes::lower_to_circuit::lower_to_circuit_ir(
        &blocks,
        &bit_ty,
        1,
        volar_ir_passes::lower_to_circuit::LoweringMode::WithTerminationFlag,
    );
    let step_block = &circuit.blocks[0];
    let emission = volar_pesat_emit::emit_step_index(step_block, &types).expect("emit");
    let flat = volar_pesat_emit::flatten_index(&emission);

    // Honest trace for the step with input b = 0 (state bit 0): out = 1.
    let mut storage = volar_pesat_emit::StorageMap::default();
    let mut accesses = Vec::new();
    let wires = volar_pesat_emit::eval_step_wires(
        step_block,
        &types,
        &[vec![false]],
        &mut storage,
        &mut accesses,
    )
    .expect("eval step");
    let witness = volar_pesat_emit::build_witness(
        &emission,
        &wires,
        volar_primitives::Galois128(0),
        volar_primitives::Galois128(0),
        volar_primitives::Galois128(0),
        &[],
        volar_primitives::Galois128(0),
        volar_primitives::Galois128(0),
    );
    // Sanity: the native oracle check accepts the honest trace.
    emission
        .index
        .check_instance(&witness)
        .expect("native honest trace satisfies");
    // Flatten to [public..., witness...] field elements for the driver input.
    let mut fields = witness.public.clone();
    fields.extend_from_slice(&witness.witness);
    (flat, fields)
}

/// E5: the satisfaction checker compiles via IR and runs in-circuit.
///
/// NOTE: this drives the satisfaction RELATION (per-constraint `Σ terms ==
/// 0` over `public || witness`) directly over the flattened instance arrays
/// for speed (~110s). The canonical `warp_is_instance_satisfied` also works
/// (see `probe_e5_canonical_check`, #[ignore]d at ~14 min) — it requires
/// `parse_with_driver` so `warp.rs` (and thus the checker) is in-module. The
/// relation here is validated differentially against the native
/// `check_instance` on the same emitted instance (honest satisfies, tampered
/// does not), which is the semantics-preservation signal.
#[test]
fn e5_in_circuit_step_satisfaction() {
    let (flat, fields) = e5_self_loop_instance();
    // Sizes from the self-loop emission: M=16, TERMS=3, DEG=2, X=1, K=8.
    assert_eq!((flat.m, flat.terms, flat.deg), (16, 3, 2));
    assert_eq!((flat.public_len, flat.witness_len), (1, 8));

    let module = parse_driver_only(
        "use cipher::consts::*;\n\
         pub fn probe_check(\n\
             coeffs: Array<Galois128, U48>,\n\
             nvars: Array<usize, U48>,\n\
             vars: Array<usize, U96>,\n\
             nterms: Array<usize, U16>,\n\
             pub_in: Array<Galois128, U1>,\n\
             wit: Array<Galois128, U8>,\n\
         ) -> u8 {\n\
             // Per-constraint satisfaction (the check_instance relation):\n\
             // for each constraint, sum of terms over (public || witness) == 0.\n\
             let mut ok = 1u8;\n\
             for c in 0..16 {\n\
                 let mut sum = Galois128(0);\n\
                 for t in 0..3 {\n\
                     if (t as usize) < nterms[c] {\n\
                         let mut prod = coeffs[c * 3 + t];\n\
                         for j in 0..2 {\n\
                             if (j as usize) < nvars[c * 3 + t] {\n\
                                 let v = vars[(c * 3 + t) * 2 + j];\n\
                                 let x = if v < 1 { pub_in[v] } else { wit[v - 1] };\n\
                                 prod = prod * x;\n\
                             }\n\
                         }\n\
                         sum = sum + prod;\n\
                     }\n\
                 }\n\
                 if sum != Galois128(0) { ok = 0u8; }\n\
             }\n\
             ok\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_check"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_check"];

    // Assemble the eval inputs in param order, one IrValue per VAFFLE entry
    // slot: a Galois128 field param is one 128-bit value; a usize param is 64
    // one-bit values.
    // Param order: coeffs, nvars, vars, nterms, pub_in, wit. One IrValue per
    // VAFFLE entry slot: a field param = one 128-bit value; a usize param = 64
    // one-bit values.
    let mut inputs: Vec<Vec<bool>> = Vec::new();
    for c in &flat.coeffs {
        inputs.push(bits_of(c.0, 128));
    }
    for v in &flat.n_vars {
        for i in 0..64 {
            inputs.push(vec![((*v as usize) >> i) & 1 == 1]);
        }
    }
    for v in &flat.vars {
        for i in 0..64 {
            inputs.push(vec![(*v >> i) & 1 == 1]);
        }
    }
    for v in &flat.n_terms {
        for i in 0..64 {
            inputs.push(vec![((*v as usize) >> i) & 1 == 1]);
        }
    }
    inputs.push(bits_of(fields[0].0, 128)); // pub_in
    for w in &fields[1..] {
        inputs.push(bits_of(w.0, 128));
    }

    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &inputs, 64)
        .expect("in-circuit check evaluates");
    let flat_out: Vec<bool> = out.concat();
    assert_eq!(
        bits_to_u128(&flat_out),
        1,
        "honest step trace must satisfy the instance in-circuit"
    );

    // Tamper: flip a witness wire (the step's output bit, fields[1]) so the
    // relational constraint no longer holds.
    let mut bad_inputs = inputs.clone();
    // Slot index of the first witness input: 48 field (coeffs) + (48+96+16)
    // usize × 64 bits + 1 field (pub_in).
    let wit_start = 48 + (48 + 96 + 16) * 64 + 1;
    bad_inputs[wit_start] = bits_of(fields[1].0 ^ 1, 128);
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(&target.module, fid, &bad_inputs, 64)
        .expect("tampered check evaluates");
    let flat_out: Vec<bool> = out.concat();
    assert_eq!(
        bits_to_u128(&flat_out),
        0,
        "tampered step trace must be rejected in-circuit"
    );
    eprintln!("[E5] in-circuit satisfaction check: honest accepted, tampered rejected");
}

/// Evidence probe: the canonical `warp_is_instance_satisfied` (the actual spec
/// checker) compiles and accepts the honest step trace in-circuit when the
/// driver is parsed with `parse_with_driver` (warp.rs in-module). This proves
/// the earlier "struct not in registry" E5 failure was a *harness* issue —
/// `parse_driver_only` omits warp.rs so the callee is never planned and its
/// `WarpIndex` never registered — not a volar-lir-codegen registration bug.
/// Slow (~14 min: full warp.rs parse + native-loop lowering), so #[ignore].
#[test]
#[ignore = "correct-but-slow (~14 min): full warp.rs parse + lowering; run explicitly"]
fn probe_e5_canonical_check() {
    let (flat, fields) = e5_self_loop_instance();
    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_check(\n\
             coeffs: Array<Galois128, U48>,\n\
             nvars: Array<usize, U48>,\n\
             vars: Array<usize, U96>,\n\
             nterms: Array<usize, U16>,\n\
             pub_in: Array<Galois128, U1>,\n\
             wit: Array<Galois128, U8>,\n\
         ) -> u8 {\n\
             let index = WarpIndex::<U16, U3, U2> {\n\
                 constraints: Array::<WarpConstraint<U3, U2>, U16>::from_fn(|c| WarpConstraint::<U3, U2> {\n\
                     terms: Array::<WarpTerm<U2>, U3>::from_fn(|t| WarpTerm::<U2> {\n\
                         coeff: coeffs[c * 3 + t],\n\
                         vars: Array::<usize, U2>::from_fn(|j| vars[(c * 3 + t) * 2 + j]),\n\
                         n_vars: nvars[c * 3 + t],\n\
                     }),\n\
                     n_terms: nterms[c],\n\
                 }),\n\
                 public_len: 1,\n\
                 witness_len: 8,\n\
                 degree_bound: 2,\n\
             };\n\
             let witness = WarpWitness::<U8, U1> {\n\
                 public: Array::<Galois128, U1>::from_fn(|i| pub_in[i]),\n\
                 witness: Array::<Galois128, U8>::from_fn(|i| wit[i]),\n\
             };\n\
             if warp_is_instance_satisfied::<U16, U3, U2, U8, U1>(&index, &witness) { 1u8 } else { 0u8 }\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_check"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_check"];
    let mut inputs: Vec<Vec<bool>> = Vec::new();
    for c in &flat.coeffs {
        inputs.push(bits_of(c.0, 128));
    }
    for v in &flat.n_vars {
        for i in 0..64 {
            inputs.push(vec![((*v as usize) >> i) & 1 == 1]);
        }
    }
    for v in &flat.vars {
        for i in 0..64 {
            inputs.push(vec![(*v >> i) & 1 == 1]);
        }
    }
    for v in &flat.n_terms {
        for i in 0..64 {
            inputs.push(vec![((*v as usize) >> i) & 1 == 1]);
        }
    }
    inputs.push(bits_of(fields[0].0, 128));
    for w in &fields[1..] {
        inputs.push(bits_of(w.0, 128));
    }
    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(
        &target.module, fid, &inputs, 1 << 26,
    )
    .expect("canonical in-circuit satisfaction evaluates");
    let result: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    assert_eq!(bits_to_u128(&result), 1, "canonical checker accepts honest trace");
    println!("[E5-CANON] canonical warp_is_instance_satisfied works via parse_with_driver");
}

/// E6: in-circuit decision. A toy accumulator chain (genesis over the emitted
/// self-loop step instance) is decided in-circuit: `warp_decide_spec`'s loops
/// (index validity, Merkle-root recompute + codeword commit, MLE eval, bundled
/// eval) compile via the M4 path and evaluate to the native decider's verdict,
/// differentially. Required a fix: aggregate `==`/`!=` now reduce to a scalar
/// bool (AND of element-wise Eq / OR of element-wise Ne) in lower_expr — the
/// decider's `WarpDigest != WarpDigest` / `codeword != codeword` comparisons
/// previously lowered element-wise and failed the `||` width check.
///
/// Slow (~25 min: full warp.rs parse + native-loop lowering + in-circuit
/// Merkle/Keccak eval), so #[ignore]d — run explicitly to re-validate.
#[test]
#[ignore = "correct-but-slow (~25 min): in-circuit decider eval; run explicitly"]
fn e6_in_circuit_decision() {
    use cipher::consts::*;
    use volar_spec::warp::{
        WarpAcc, WarpAccInstance, WarpAccWitness, WarpConstraint, WarpIndex, WarpMerkleTree,
        WarpTerm, WarpWitness,
    };
    let (flat, fields) = e5_self_loop_instance();
    // Native const-generic index matching the emitted instance (M=16, TERMS=3,
    // DEG=2), rebuilt from the flattened constraints.
    let index = WarpIndex::<U16, U3, U2> {
        constraints: volar_spec::Array::from_fn(|c| WarpConstraint::<U3, U2> {
            terms: volar_spec::Array::from_fn(|t| WarpTerm::<U2> {
                coeff: flat.coeffs[c * 3 + t],
                vars: volar_spec::Array::from_fn(|j| flat.vars[(c * 3 + t) * 2 + j]),
                n_vars: flat.n_vars[c * 3 + t],
            }),
            n_terms: flat.n_terms[c],
        }),
        public_len: 1,
        witness_len: 8,
        degree_bound: 2,
    };
    let witness = WarpWitness::<U8, U1> {
        public: volar_spec::Array::from_fn(|i| fields[i]),
        witness: volar_spec::Array::from_fn(|i| fields[1 + i]),
    };
    // Native genesis over the toy instance → a toy accumulator. N=8 (identity
    // code: codeword == witness, len 8), LOGN=3, BETA=1+log2(16)=5, NODES=15.
    let acc = volar_spec::warp::warp_genesis_spec::<
        U16, U3, U2, U8, U1, U8, volar_spec::warp::WarpIdentityCode, U15, U5,
    >(&index, &volar_spec::warp::WarpIdentityCode, &witness)
    .expect("native genesis over the toy instance");
    let native_verdict = volar_spec::warp::warp_decide_spec::<
        U16, U3, U2, U8, U8, U3, U5, U15, volar_spec::warp::WarpIdentityCode,
    >(&index, &volar_spec::warp::WarpIdentityCode, &acc);
    assert!(native_verdict, "native decide accepts the genesis acc");

    let module = parse_with_driver(
        "use cipher::consts::*;\n\
         pub fn probe_decide(\n\
             coeffs: Array<Galois128, U48>,\n\
             nvars: Array<usize, U48>,\n\
             vars: Array<usize, U96>,\n\
             nterms: Array<usize, U16>,\n\
             root: Array<u8, U32>,\n\
             alpha: Array<Galois128, U3>,\n\
             mu: Galois128,\n\
             beta: Array<Galois128, U5>,\n\
             eta: Galois128,\n\
             leaves: usize,\n\
             nodes: Array<u8, U480>,\n\
             codeword: Array<Galois128, U8>,\n\
             witness: Array<Galois128, U8>,\n\
         ) -> u8 {\n\
             let index = WarpIndex::<U16, U3, U2> {\n\
                 constraints: Array::<WarpConstraint<U3, U2>, U16>::from_fn(|c| WarpConstraint::<U3, U2> {\n\
                     terms: Array::<WarpTerm<U2>, U3>::from_fn(|t| WarpTerm::<U2> {\n\
                         coeff: coeffs[c * 3 + t],\n\
                         vars: Array::<usize, U2>::from_fn(|j| vars[(c * 3 + t) * 2 + j]),\n\
                         n_vars: nvars[c * 3 + t],\n\
                     }),\n\
                     n_terms: nterms[c],\n\
                 }),\n\
                 public_len: 1,\n\
                 witness_len: 8,\n\
                 degree_bound: 2,\n\
             };\n\
             let acc = WarpAcc::<U8, U8, U3, U5, U15> {\n\
                 instance: WarpAccInstance::<U3, U5> {\n\
                     root: Array::<u8, U32>::from_fn(|j| root[j]),\n\
                     alpha: Array::<Galois128, U3>::from_fn(|i| alpha[i]),\n\
                     mu: mu,\n\
                     beta: Array::<Galois128, U5>::from_fn(|i| beta[i]),\n\
                     eta: eta,\n\
                 },\n\
                 witness: WarpAccWitness::<U8, U8, U15> {\n\
                     tree: WarpMerkleTree::<U15> {\n\
                         leaves: leaves,\n\
                         nodes: Array::<WarpDigest, U15>::from_fn(|i| Array::<u8, U32>::from_fn(|j| nodes[i * 32 + j])),\n\
                     },\n\
                     codeword: Array::<Galois128, U8>::from_fn(|i| codeword[i]),\n\
                     witness: Array::<Galois128, U8>::from_fn(|i| witness[i]),\n\
                 },\n\
             };\n\
             if warp_decide_spec::<U16, U3, U2, U8, U8, U3, U5, U15, WarpIdentityCode>(&index, &WarpIdentityCode, &acc) { 1u8 } else { 0u8 }\n\
         }\n",
    );
    let target = lower_seeded_loops(&module, &["probe_decide"], volar_lir_codegen::LoopLowering::Native);
    let fid = target.module.exports["probe_decide"];

    // Assemble inputs in param order. Field param = one 128-bit slot; usize =
    // 64 one-bit slots; u8 = 8 one-bit slots; Array<u8, U*> flattens per byte.
    let mut inputs: Vec<Vec<bool>> = Vec::new();
    let mut push_field = |inputs: &mut Vec<Vec<bool>>, v: u128| inputs.push(bits_of(v, 128));
    let mut push_usize = |inputs: &mut Vec<Vec<bool>>, v: usize| {
        for i in 0..64 {
            inputs.push(vec![(v >> i) & 1 == 1]);
        }
    };
    let mut push_u8 = |inputs: &mut Vec<Vec<bool>>, v: u8| {
        for i in 0..8 {
            inputs.push(vec![(v >> i) & 1 == 1]);
        }
    };
    for c in &flat.coeffs {
        push_field(&mut inputs, c.0);
    }
    for v in &flat.n_vars {
        push_usize(&mut inputs, *v as usize);
    }
    for v in &flat.vars {
        push_usize(&mut inputs, *v);
    }
    for v in &flat.n_terms {
        push_usize(&mut inputs, *v as usize);
    }
    for j in 0..32 {
        push_u8(&mut inputs, acc.instance.root[j]);
    }
    for i in 0..3 {
        push_field(&mut inputs, acc.instance.alpha[i].0);
    }
    push_field(&mut inputs, acc.instance.mu.0);
    for i in 0..5 {
        push_field(&mut inputs, acc.instance.beta[i].0);
    }
    push_field(&mut inputs, acc.instance.eta.0);
    push_usize(&mut inputs, acc.witness.tree.leaves);
    for i in 0..15 {
        for j in 0..32 {
            push_u8(&mut inputs, acc.witness.tree.nodes[i][j]);
        }
    }
    for i in 0..8 {
        push_field(&mut inputs, acc.witness.codeword[i].0);
    }
    for i in 0..8 {
        push_field(&mut inputs, acc.witness.witness[i].0);
    }

    let out = volar_fuzz::interpreter::vaffle::eval_vaffle_with_limits(
        &target.module,
        fid,
        &inputs,
        1 << 27,
    )
    .expect("in-circuit decision evaluates");
    let result: Vec<bool> = out.iter().flat_map(|v| v.iter().copied()).collect();
    let got = bits_to_u128(&result);
    assert_eq!(
        got, native_verdict as u128,
        "in-circuit decider verdict matches native ({native_verdict})"
    );
    println!("[E6] in-circuit decision: verdict {got} matches native {native_verdict}");
}
