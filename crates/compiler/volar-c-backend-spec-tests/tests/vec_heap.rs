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
fn vec_with_capacity_allocates_for_push_and_tracks_length() {
    let c_source = lower_root(
        r#"
        fn root(capacity: usize) -> u64 {
            let mut values: Vec<u8> = Vec::with_capacity(capacity);
            values.push(4);
            values.push(7);
            values[0] as u64 + values[1] as u64 + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root(2));"#,
    );
    assert_eq!(output.trim(), "13", "generated C:\n{c_source}");
}

#[test]
fn vec_new_and_push_mutate_through_a_borrowed_vec_handle() {
    let c_source = lower_root(
        r#"
        fn push_one(values: &mut Vec<u8>, value: u8) {
            values.push(value);
        }
        fn root() -> u64 {
            let mut values: Vec<u8> = Vec::new();
            push_one(&mut values, 1);
            push_one(&mut values, 2);
            push_one(&mut values, 3);
            push_one(&mut values, 4);
            push_one(&mut values, 5);
            values[0] as u64 + values[4] as u64 + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "11", "generated C:\n{c_source}");
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
fn vec_for_loop_uses_runtime_length_and_preserves_loop_carried_values() {
    let c_source = lower_root(
        r#"
        fn root(count: usize) -> u64 {
            let values = vec![7u8; count];
            let mut sum = 0u64;
            for value in &values {
                sum += value as u64;
            }
            sum + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root(5)); printf("%llu\n", (unsigned long long)root(0));"#,
    );
    assert_eq!(output.trim(), "40\n0", "generated C:\n{c_source}");
}

#[test]
fn vec_option_repeat_and_mutation_preserve_some_none_semantics() {
    let c_source = lower_root(
        r#"
        fn root() -> u64 {
            let mut values: Vec<Option<u8>> = vec![None; 3];
            values[0] = Some(7u8);
            match values[0] {
                Some(value) => value as u64 + values[1].is_none() as u64 + values.len() as u64,
                None => 0,
            }
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "11", "generated C:\n{c_source}");
}

#[test]
fn vec_reference_coerces_to_a_slice_for_local_calls() {
    let c_source = lower_root(
        r#"
        fn second(values: &[u8]) -> u64 {
            values[1] as u64
        }
        fn root() -> u64 {
            let values = vec![4u8, 7u8, 9u8];
            second(&values) + values.len() as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    assert_eq!(output.trim(), "10", "generated C:\n{c_source}");
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

#[test]
fn copy_from_slice_into_fixed_array_from_offset_source() {
    let c_source = lower_root(
        r#"
        fn root() -> u64 {
            let digest = [3u8, 5, 7, 9, 11, 13, 17, 19];
            let mut sd = [0u8; 3];
            let mut com = [0u8; 2];
            sd.copy_from_slice(&digest[..3]);
            com.copy_from_slice(&digest[3..5]);
            sd[0] as u64 + sd[1] as u64 + sd[2] as u64 + com[0] as u64 + com[1] as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    // 3 + 5 + 7 + 9 + 11 = 35
    assert_eq!(output.trim(), "35", "generated C:\n{c_source}");
}

#[test]
fn copy_from_slice_into_vec_from_whole_slice() {
    let c_source = lower_root(
        r#"
        fn root() -> u64 {
            let src = [4u8, 8, 15, 16, 23, 42];
            let mut values: Vec<u8> = vec![0u8; 6];
            values.copy_from_slice(&src[..]);
            values[0] as u64 + values[4] as u64 + values[5] as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    // 4 + 23 + 42 = 69
    assert_eq!(output.trim(), "69", "generated C:\n{c_source}");
}

#[test]
fn from_le_bytes_and_to_le_bytes_round_trip() {
    let c_source = lower_root(
        r#"
        fn root() -> u64 {
            let bytes = [0x78u8, 0x56, 0x34, 0x12];
            let word = u32::from_le_bytes(bytes);
            let back = word.to_le_bytes();
            (back[0] as u64) + (back[3] as u64) + (word >> 16) as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    // 0x78 + 0x12 + 0x1234 = 120 + 18 + 4660 = 4798
    assert_eq!(output.trim(), "4798", "generated C:\n{c_source}");
}

#[test]
fn from_be_bytes_and_to_be_bytes_round_trip() {
    let c_source = lower_root(
        r#"
        fn root() -> u64 {
            let bytes = [0x12u8, 0x34, 0x56, 0x78];
            let word = u32::from_be_bytes(bytes);
            let back = word.to_be_bytes();
            (back[0] as u64) + (back[3] as u64) + (word >> 16) as u64
        }
        "#,
    );
    let output = compile_and_run(
        &c_source,
        r#"printf("%llu\n", (unsigned long long)root());"#,
    );
    // 0x12 + 0x78 + 0x1234 = 18 + 120 + 4660 = 4798
    assert_eq!(output.trim(), "4798", "generated C:\n{c_source}");
}
