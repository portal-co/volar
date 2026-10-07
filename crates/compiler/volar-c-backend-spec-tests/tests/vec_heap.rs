//! Runtime `Vec` lowering through the LIR heap-allocation capability.

use volar_c_backend::CBackend;
use volar_compiler::parser::parse_source;
use volar_lir_codegen::{lower_module_seeded, mono::MonoEnv};
use volar_lir_test_corpus::compile_and_run;

fn lower_root(source: &str) -> String {
    let module = parse_source(source, "vec_heap", &[]).expect("parse Vec fixture");
    let mut backend = CBackend::new();
    lower_module_seeded(&module, &mut backend, &MonoEnv::new("vec_heap"), &["root"]);
    backend.finish()
}

#[test]
fn vec_repeat_uses_runtime_count_and_supports_indexed_reads_and_writes() {
    let c_source = lower_root(
        r#"
        fn root(count: usize) -> u64 {
            let mut values = vec![0u8; count];
            values[3] = 41;
            values[0] as u64 + values[3] as u64 + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root(5));"#,
    );
    assert_eq!(output.trim(), "46", "generated C:\n{c_source}");
}

#[test]
fn vec_repeat_handles_a_zero_runtime_count() {
    let c_source = lower_root(
        r#"
        fn root(count: usize) -> u64 {
            let values = vec![7u8; count];
            values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root(0));"#,
    );
    assert_eq!(output.trim(), "0", "generated C:\n{c_source}");
}

#[test]
fn vec_repeat_rejects_types_without_copy_or_clone_lowering() {
    let result = std::panic::catch_unwind(|| {
        let _ = lower_root(
            r#"
            struct Item { value: u8 }
            fn root(count: usize) -> u64 {
                let item = Item { value: 7 };
                let values = vec![item; count];
                values[0].value as u64
            }
            "#,
        );
    });
    let panic = result.expect_err("custom Vec repetition must fail closed");
    let message = panic
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| panic.downcast_ref::<&str>().copied())
        .unwrap_or_default();
    assert!(
        message.contains("Clone lowering is not implemented"),
        "{message}"
    );
}

#[test]
fn vec_literal_allocates_and_initializes_each_element() {
    let c_source = lower_root(
        r#"
        fn root() -> u64 {
            let values = vec![4u8, 7u8, 9u8];
            values[1] as u64 + values[2] as u64 + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "19", "generated C:\n{c_source}");
}

#[test]
fn vec_repeat_initializes_each_slot_from_the_repeated_value() {
    let c_source = lower_root(
        r#"
        fn root(count: usize, repeated: u8) -> u64 {
            let values = vec![repeated; count];
            values[0] as u64 + values[3] as u64 + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root(5, 7));"#,
    );
    assert_eq!(output.trim(), "19", "generated C:\n{c_source}");
}
