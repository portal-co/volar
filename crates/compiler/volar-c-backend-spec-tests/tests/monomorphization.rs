// @reliability: normal
// @ai: assisted
//! End-to-end tests for concrete LIR specialization planning.
//!
//! These fixtures parse a production spec helper, then add a typed synthetic
//! caller in the same way a weaver adds its generated entry point. The caller
//! uses the helper at more than one concrete const-generic size. The test
//! lowers the closed set through LIR to C, compiles it, and checks its result.

use std::path::Path;

use volar_c_backend::CBackend;
use volar_compiler::parser::parse_source;
use volar_lir_codegen::{MonoPlanOptions, lower_module_monomorphized};
use volar_lir_test_corpus::compile_and_run;

fn tfhe_spec_source() -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("compiler directory")
        .parent()
        .expect("crates directory")
        .join("spec/volar-spec/src/tfhe.rs");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read TFHE spec helper {}: {error}", path.display()))
}

/// A generated caller can ask for separate concrete instances of one linked
/// spec helper. `poly_add_neg` is the actual TFHE reference helper; choosing
/// sizes 2 and 3 makes an accidental module-wide `N` substitution observable.
#[test]
fn real_spec_helper_multiple_const_instances_to_c() {
    let parsed_spec =
        parse_source(&tfhe_spec_source(), "tfhe", &[]).expect("parse the TFHE spec source");
    let poly_add_neg = parsed_spec
        .functions
        .into_iter()
        .find(|function| function.name == "poly_add_neg")
        .expect("TFHE spec contains poly_add_neg");

    // Deliberately parsed rather than assembled from strings in IR: like a
    // weaver output, this is ordinary typed IR that invokes linked spec code.
    let generated = parse_source(
        r#"
        fn synthetic_poly_caller(
            a2: [u32; 2], b2: [u32; 2], a3: [u32; 3], b3: [u32; 3],
        ) -> u32 {
            let sum2 = poly_add_neg::<2>(&a2, &b2);
            let sum3 = poly_add_neg::<3>(&a3, &b3);
            sum2[0] + sum2[1] + sum3[0] + sum3[1] + sum3[2]
        }
        "#,
        "synthetic_weaver_output",
        &[],
    )
    .expect("parse synthetic generated caller");
    let caller = generated
        .functions
        .into_iter()
        .find(|function| function.name == "synthetic_poly_caller")
        .expect("synthetic module contains its caller");

    let module = volar_compiler::ir::IrModule {
        name: "multiple_const_specializations".to_owned(),
        functions: vec![caller, poly_add_neg],
        ..Default::default()
    };

    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("plan and lower both poly_add_neg specializations");
    let c_source = backend.finish();

    let output = compile_and_run(
        &c_source,
        r#"
        Arr_U32_2 a2 = { .data = { 1u, 2u } };
        Arr_U32_2 b2 = { .data = { 10u, 20u } };
        Arr_U32_3 a3 = { .data = { 3u, 4u, 5u } };
        Arr_U32_3 b3 = { .data = { 30u, 40u, 50u } };
        printf("%u\n", synthetic_poly_caller(a2, b2, a3, b3));
        "#,
    );
    assert_eq!(output.trim(), "165");
}

#[test]
fn hybrid_array_tuple_constructor_preserves_its_fixed_array_values() {
    let module = parse_source(
        r#"
        fn root() -> u64 {
            let values = Array::<u8, U4>([4u8, 7u8, 9u8, 11u8]);
            values.0[2] as u64
        }
        "#,
        "hybrid_array_constructor",
        &[],
    )
    .expect("parse hybrid-array tuple constructor fixture");
    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("lower hybrid-array constructor");
    let c_source = backend.finish();
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "9", "generated C:\n{c_source}");
}

#[test]
fn fixed_array_destructuring_preserves_each_element_layout() {
    let module = parse_source(
        r#"
        fn root() -> u64 {
            let [left, right] = [
                Array::<u8, U4>([4u8, 7u8, 9u8, 11u8]),
                Array::<u8, U4>([3u8, 5u8, 8u8, 12u8]),
            ];
            left[2] as u64 + right[3] as u64
        }
        "#,
        "fixed_array_destructuring",
        &[],
    )
    .expect("parse fixed-array destructuring fixture");
    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("lower fixed-array destructuring");
    let c_source = backend.finish();
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "21", "generated C:\n{c_source}");
}

/// Explicit generic arguments must distinguish calls whose parameters are the
/// same but whose return layouts differ.
#[test]
fn static_associated_call_on_bare_nominal_preserves_array_return_type() {
    let module = parse_source(
        r#"
        struct Pairer {}
        impl Pairer {
            fn pass(values: [u8; 2]) -> [u8; 2] { values }
        }
        fn root() -> u64 {
            let [left, right] = Pairer::pass([7u8, 11u8]);
            left as u64 + right as u64
        }
        "#,
        "bare_nominal_static_associated_call",
        &[],
    )
    .expect("parse bare-nominal static associated call fixture");
    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("plan and lower bare-nominal static associated call");
    let c_source = backend.finish();
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "18", "generated C:\n{c_source}");
}

#[test]
fn static_associated_self_return_resolves_to_impl_type() {
    let module = parse_source(
        r#"
        struct BoxedWord { value: u64 }
        impl BoxedWord {
            fn new(value: u64) -> Self { BoxedWord { value } }
        }
        fn root() -> u64 {
            let boxed = BoxedWord::new(29);
            boxed.value
        }
        "#,
        "static_associated_self_return",
        &[],
    )
    .expect("parse associated Self-return fixture");
    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("plan and lower associated Self-return");
    let c_source = backend.finish();
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "29", "generated C:\n{c_source}");
}

#[test]
fn module_array_lengths_resolve_referenced_constants() {
    let module = parse_source(
        r#"
        const BASE: usize = 2;
        const EXTENT: usize = BASE + 1;
        fn root() -> u64 {
            let values: [u8; EXTENT] = [4u8, 7u8, 9u8];
            values[2] as u64
        }
        "#,
        "referenced_module_array_length",
        &[],
    )
    .expect("parse referenced module const fixture");
    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("plan and lower referenced module const");
    let c_source = backend.finish();
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "9", "generated C:\n{c_source}");
}

#[test]
fn return_only_const_generic_instances_do_not_collide() {
    let module = parse_source(
        r#"
        fn make<const N: usize>() -> [u8; N] { [7u8; N] }
        fn root() -> u8 {
            let three = make::<3>();
            let two = make::<2>();
            three[2] + two[0]
        }
        "#,
        "return_only_generics",
        &[],
    )
    .expect("parse return-only const-generic fixture");

    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("plan both return-only const-generic instances");
    let c_source = backend.finish();
    let output = compile_and_run(&c_source, r#"printf("%u\n", (unsigned)root());"#);
    assert_eq!(output.trim(), "14", "generated C:\n{c_source}");
}

/// Two concrete nominal layouts (`Wrap<u8>` and `Wrap<u64>`) must coexist in one
/// lowering — a bare `StructKind` registry key would collide and corrupt fields.
#[test]
fn dual_nominal_wrap_layouts_to_c() {
    let module = parse_source(
        r#"
        struct Wrap<T> { value: T }

        fn wrap_u8(x: u8) -> Wrap<u8> {
            Wrap { value: x }
        }

        fn wrap_u64(x: u64) -> Wrap<u64> {
            Wrap { value: x }
        }

        fn dual_wrap_sum(a: u8, b: u64) -> u64 {
            let w8 = wrap_u8(a);
            let w64 = wrap_u64(b);
            (w8.value as u64) + w64.value
        }
        "#,
        "dual_wrap",
        &[],
    )
    .expect("parse dual Wrap module");

    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions::default())
        .expect("plan and lower dual Wrap specializations");
    let c_source = backend.finish();

    let output = compile_and_run(
        &c_source,
        r#"
        printf("%llu\n", (unsigned long long)dual_wrap_sum(3, 40));
        "#,
    );
    assert_eq!(output.trim(), "43");
}
