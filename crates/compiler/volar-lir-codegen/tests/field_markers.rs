// @reliability: normal
//! @ai: assisted
//! M2 compile-through: field markers in the AST lower through LIR to VAFFLE
//! with **native field ops** (the acceleration invariance), and the compiled
//! arithmetic evaluates identically to native spec arithmetic
//! (`volar-ir`'s `docs/warp-vole-verifier-zksnark-plan.md`, milestone M2).

use volar_compiler::parser::{SourceInput, parse_sources};
use volar_fuzz::interpreter::vaffle::eval_vaffle;
use volar_ir_common::{IrType, Stmt, TypeId};
use volar_primitives::Galois128;
use volar_vaffle_target::VaffleTarget;

fn bits_of(value: u128, width: usize) -> Vec<bool> {
    (0..width).map(|i| (value >> i) & 1 == 1).collect()
}

fn bits_to_u128(bits: &[bool]) -> u128 {
    bits.iter().enumerate().fold(0u128, |acc, (i, b)| {
        if *b { acc | (1u128 << i) } else { acc }
    })
}

/// Parse `source` and lower it into a fresh VAFFLE target.
fn compile_to_vaffle(source: &str) -> VaffleTarget {
    let module = parse_sources(
        &[SourceInput {
            source,
            name: "field_marker_m2",
        }],
        "field_marker_m2",
        &[],
    )
    .expect("marker-typed source parses");
    let mut target = VaffleTarget::new();
    volar_lir_codegen::lower_module(&module, &mut target);
    target
}

/// Find the TypeId of the field type the function under test operates on.
fn field_tid(target: &VaffleTarget, predicate: impl Fn(&IrType) -> bool) -> TypeId {
    target
        .module
        .types
        .0
        .iter()
        .position(|ty| predicate(ty))
        .map(|i| TypeId(i as u32))
        .expect("the field type must be interned in the VAFFLE type table")
}

/// Count the field-typed `Stmt::Poly` values in the named function.
fn field_poly_count(target: &VaffleTarget, name: &str, tid: TypeId) -> usize {
    let fid = target.module.exports[name];
    let body = match &target.module.funcs[fid.0] {
        vaffle::FuncDecl::Body(b) => b,
        _ => panic!("expected function body"),
    };
    body.values
        .iter()
        .filter(|v| matches!(&v.kind, vaffle::Value::Op(Stmt::Poly { ty, .. }) if *ty == tid))
        .count()
}

#[test]
fn ext_field_marker_compiles_to_native_field_ops() {
    // a*b + a over the Gf2_128 marker (the VOLE field).
    let target = compile_to_vaffle("fn mul_add(a: Gf2_128, b: Gf2_128) -> Gf2_128 { a * b + a }");
    let tid = field_tid(&target, |ty| {
        matches!(ty, IrType::ExtField { degree: 128, .. })
    });
    // Acceleration invariance: both arithmetic ops are field-typed Polys.
    assert_eq!(
        field_poly_count(&target, "mul_add", tid),
        2,
        "mul and add must each compile to one native field Poly"
    );
}

#[test]
fn ext_field_marker_matches_native_vole_field_arithmetic() {
    let target = compile_to_vaffle("fn mul_add(a: Gf2_128, b: Gf2_128) -> Gf2_128 { a * b + a }");
    let fid = target.module.exports["mul_add"];
    for (a, b) in [(0u128, 0u128), (1, 1), (5, 9), (0xDEADBEEF, 0x1234567890ABCDEF)] {
        let out = eval_vaffle(&target.module, fid, &[bits_of(a, 128), bits_of(b, 128)])
            .expect("compiled field arithmetic evaluates");
        // Native oracle: volar_primitives::Galois128 IS the VOLE field.
        let expected = (Galois128(a) * Galois128(b) + Galois128(a)).0;
        assert_eq!(
            bits_to_u128(&out[0]),
            expected,
            "compiled a*b+a must match native Galois128 arithmetic"
        );
    }
}

#[test]
fn prime_field_marker_compiles_and_matches_native_fp3() {
    let target = compile_to_vaffle("fn mul_add(a: Fp3, b: Fp3) -> Fp3 { a * b + a }");
    let tid = field_tid(&target, |ty| matches!(ty, IrType::PrimeField { k: 2, .. }));
    assert_eq!(
        field_poly_count(&target, "mul_add", tid),
        2,
        "mul and add must each compile to one native field Poly"
    );
    let fid = target.module.exports["mul_add"];
    for a in 0..3u128 {
        for b in 0..3u128 {
            let out = eval_vaffle(&target.module, fid, &[bits_of(a, 2), bits_of(b, 2)])
                .expect("compiled prime-field arithmetic evaluates");
            assert_eq!(bits_to_u128(&out[0]), (a * b + a) % 3, "a*b+a over GF(3)");
        }
    }
}

#[test]
fn legacy_primitive_and_marker_compile_identically() {
    // Back-compat: the `Galois128` primitive is an alias of the `Gf2_128`
    // marker — both spellings must produce the same VAFFLE.
    let via_primitive =
        compile_to_vaffle("fn mul_add(a: Galois128, b: Galois128) -> Galois128 { a * b + a }");
    let via_marker = compile_to_vaffle("fn mul_add(a: Gf2_128, b: Gf2_128) -> Gf2_128 { a * b + a }");
    let fid_p = via_primitive.module.exports["mul_add"];
    let fid_m = via_marker.module.exports["mul_add"];
    for (a, b) in [(1u128, 1u128), (7, 13), (0xFEEDFACE, 0xCAFEBABE)] {
        let inputs = [bits_of(a, 128), bits_of(b, 128)];
        let out_p = eval_vaffle(&via_primitive.module, fid_p, &inputs).expect("primitive evaluates");
        let out_m = eval_vaffle(&via_marker.module, fid_m, &inputs).expect("marker evaluates");
        assert_eq!(out_p, out_m, "primitive and marker spellings must agree");
    }
}
