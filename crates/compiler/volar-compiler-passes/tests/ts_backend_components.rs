//! Component-level TS codegen tests using seeded lazy emission.
//!
//! Each test compiles only the functions reachable from a small set of seeds,
//! so one failing component doesn't block others. A working `tsc` process is
//! required: executable/launcher failures fail the test rather than being
//! misread as a clean strict-mode run.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use volar_compiler::{ir::IrFunction, ir::IrModule, parser::parse_source};
use volar_compiler_passes::{print_module_typescript, print_module_typescript_seeded};

// ---------------------------------------------------------------------------
// Shared helpers (mirrors ts_backend.rs)
// ---------------------------------------------------------------------------

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn collect_rs_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let p = entry.path();
                if p.is_dir() {
                    walk(&p, out);
                } else if p.extension().map_or(false, |e| e == "rs") {
                    out.push(p);
                }
            }
        }
    }
    walk(dir, &mut out);
    out.sort();
    out
}

fn parse_dir(dir: &Path, crate_name: &str, module: &mut IrModule<IrFunction>) {
    for file in collect_rs_files(dir) {
        let src = match fs::read_to_string(&file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("  warn: cannot read {}: {e}", file.display());
                continue;
            }
        };
        if src
            .lines()
            .any(|line| line.trim_start().starts_with("//! @volar-codegen-exclude:"))
        {
            continue;
        }
        let stem = file.file_stem().unwrap().to_string_lossy().to_string();
        let module_path = vec![crate_name.to_string(), stem.clone()];
        match parse_source(&src, &stem, &module_path) {
            Ok(m) => {
                module.structs.extend(m.structs);
                module.enums.extend(m.enums);
                module.traits.extend(m.traits);
                module.impls.extend(m.impls);
                module.functions.extend(m.functions);
                module.type_aliases.extend(m.type_aliases);
                module.consts.extend(m.consts);
            }
            Err(e) => eprintln!(
                "  warn: parse error in {}: {e}",
                file.file_name().unwrap().to_string_lossy()
            ),
        }
    }
}

fn build_module() -> IrModule<IrFunction> {
    let root = workspace_root();
    let mut module = IrModule {
        name: "volar_ts_components".to_string(),
        ..Default::default()
    };
    parse_dir(
        &root.join("crates/spec/volar-primitives/src"),
        "volar_primitives",
        &mut module,
    );
    parse_dir(
        &root.join("crates/spec/volar-common/src"),
        "volar_common",
        &mut module,
    );
    parse_dir(
        &root.join("crates/spec/volar-spec/src"),
        "volar_spec",
        &mut module,
    );
    {
        let mut seen = std::collections::HashSet::new();
        module.structs.retain(|s| {
            let cn = s.module_path.first().cloned().unwrap_or_default();
            seen.insert((cn, s.kind.to_string()))
        });
    }
    {
        let mut seen = std::collections::HashSet::new();
        module.functions.retain(|f| {
            let cn = f.module_path.first().cloned().unwrap_or_default();
            seen.insert((cn, f.name.clone()))
        });
    }
    {
        let mut seen = std::collections::HashSet::new();
        module.consts.retain(|c| {
            let cn = c.module_path.first().cloned().unwrap_or_default();
            seen.insert((cn, c.name.clone()))
        });
    }
    module
}

fn find_tsc() -> Option<PathBuf> {
    let root = workspace_root();
    let candidate = root.join("node_modules/.bin/tsc");
    if candidate.exists() {
        return Some(candidate);
    }
    let out = Command::new("which").arg("tsc").output().ok()?;
    if out.status.success() {
        let p = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if !p.is_empty() {
            return Some(PathBuf::from(p));
        }
    }
    None
}

/// Run tsc on the seeded TS output for `seeds`, label the output with `label`.
fn run_ts_component(seeds: &[&str], label: &str) {
    let module = build_module();
    let ts_code = print_module_typescript_seeded(&module, seeds);
    run_ts_code(&ts_code, label);
}

fn run_ts_code(ts_code: &str, label: &str) {
    let stripped: String = ts_code
        .lines()
        .filter(|l| !l.trim_start().starts_with("// @ts-nocheck"))
        .collect::<Vec<_>>()
        .join("\n");

    let root = workspace_root();
    let runtime_src = root.join("packages/volar-runtime/src");
    let tmp_path = runtime_src.join(format!(".generated_component_{}.ts", label));
    fs::write(&tmp_path, &stripped).expect("write temp TS file");

    let tsc = match find_tsc() {
        Some(p) => p,
        None => {
            let _ = fs::remove_file(&tmp_path);
            panic!("tsc not found — install TypeScript or add node_modules/.bin to PATH");
        }
    };

    let output = Command::new(&tsc)
        .args([
            "--noEmit",
            "--strict",
            "--moduleResolution",
            "bundler",
            "--target",
            "esnext",
            "--module",
            "esnext",
            tmp_path.to_str().unwrap(),
        ])
        .current_dir(&root.join("packages/volar-runtime"))
        .output()
        .expect("tsc command failed to launch");

    let _ = fs::remove_file(&tmp_path);

    // Same fail-closed rule as ts_backend.rs: a broken tsc launcher (e.g. a
    // stale node_modules/.bin entry) must not pass as "0 errors" just
    // because its output contains no `error TS` lines.
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{stdout}{stderr}");

    if !output.status.success() && !combined.contains("error TS") {
        panic!(
            "tsc failed without TypeScript diagnostics (status {:?}):\n{}",
            output.status.code(),
            combined
        );
    }

    let mut by_code: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut total = 0usize;
    for line in combined.lines() {
        if let Some(pos) = line.find("error TS") {
            let code_start = pos + "error ".len();
            let rest = &line[code_start..];
            let code: String = rest
                .chars()
                .take_while(|c| !c.is_whitespace() && *c != ':')
                .collect();
            by_code.entry(code).or_default().push(line.to_string());
            total += 1;
        }
    }

    if total > 0 {
        eprintln!("\n=== TS component [{}]: {} error(s) ===\n", label, total);
        for (code, lines) in &by_code {
            eprintln!("--- {} ({} occurrence(s)) ---", code, lines.len());
            for l in lines {
                eprintln!("  {}", l);
            }
        }
        eprintln!("\n=== Error code summary ===");
        for (code, lines) in &by_code {
            eprintln!("  {}: {}", code, lines.len());
        }
        eprintln!("  TOTAL: {}", total);
    } else {
        eprintln!("\n=== TS component [{}]: OK ===", label);
    }

    assert_eq!(
        total, 0,
        "tsc --strict reported {} error(s) for component '{}'; see above",
        total, label
    );
}

#[test]
fn unqualified_function_calls_forward_witness_context() {
    let source = r#"
        fn needs_default<T: Default>(value: T) -> T {
            T::default()
        }

        fn forwards<T: Default>(value: T) -> T {
            needs_default(value)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "witness_forwarding", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "unqualified_witness_forwarding");
}

#[test]
fn option_try_propagates_none() {
    let source = r#"
        fn unwrap_option(value: Option<u32>) -> Option<u32> {
            let value = value?;
            Some(value)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "option_try", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "option_try_propagates_none");
}

#[test]
fn result_try_propagates_error_into_result_return_type() {
    let source = r#"
        enum DecodeError { Truncated, Invalid }

        fn propagate(value: Result<u32, DecodeError>) -> Result<u32, DecodeError> {
            let value = value?;
            value
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "result_try", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("type Result<T, E = unknown> = T | E | __VolarError;"),
        "Result must admit the error marker propagated by `?`:\n{ts_code}"
    );
    run_ts_code(&ts_code, "result_try_propagates_error");
}

#[test]
fn option_let_else_checks_and_binds_the_some_value() {
    let source = r#"
        fn increment_if_some(value: Option<u32>) -> Option<u32> {
            let Some(mut result) = value else {
                return None;
            };
            result = result + 1;
            Some(result)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "option_let_else", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "option_let_else");
}

#[test]
fn let_else_tail_return_is_emitted_as_statement() {
    let source = r#"
        fn fail_on_none(value: Option<u32>) -> bool {
            let Some(_value) = value else { return false };
            true
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "let_else_tail_return", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("return false;") && !ts_code.contains("return return false"),
        "a diverging let-else tail must be emitted as one return statement:\n{ts_code}"
    );
    run_ts_code(&ts_code, "let_else_tail_return");
}

#[test]
fn iterator_min_max_lower_empty_and_nonempty_results_to_typescript() {
    let source = r#"
        fn iterator_max(values: Vec<u32>) -> Option<u32> {
            values.into_iter().max()
        }

        fn iterator_min(values: Vec<u32>) -> Option<u32> {
            values.into_iter().min()
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "iterator_extrema", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "iterator_min_max");
}

#[test]
fn wrapping_arithmetic_preserves_width_through_local_bindings() {
    let source = r#"
        struct Accumulator {}

        impl Accumulator {
            fn add_u64(&self, value: u64) -> u64 {
                let method_local = value;
                method_local.wrapping_add(1)
            }
        }

        fn add_u64(value: u64) -> u64 {
            let intermediate = value;
            intermediate.wrapping_add(1)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "wrapping_width", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("wrappingAdd(intermediate, 1n, 64)"),
        "u64 width should be preserved through top-level local bindings:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(method_local, 1n, 64)"),
        "u64 width should be preserved through impl-method local bindings:\n{ts_code}"
    );
    run_ts_code(&ts_code, "wrapping_width_local_binding");
}

#[test]
fn wrapping_arithmetic_infers_widths_from_calls_fields_arrays_and_tuple_results() {
    let source = r#"
        const WIDE_LIMBS: [u64; 2] = [1, 2];
        type Wide = u64;

        struct Limbs {
            values: [u64; 2],
        }

        fn from_const_and_overflowing_sub(values: &[u64; 2]) -> u64 {
            let (limb, _) = values[0].overflowing_sub(WIDE_LIMBS[0]);
            let alias = limb;
            alias.wrapping_add(1)
        }

        impl Limbs {
            fn from_field(&self, index: usize) -> u64 {
                self.values[index].wrapping_add(1)
            }
        }

        fn from_associated_call(bytes: [u8; 4]) -> u32 {
            let value = u32::from_le_bytes(bytes);
            value.wrapping_add(1)
        }

        fn from_type_alias(value: Wide) -> Wide {
            value.wrapping_add(1)
        }

        fn from_annotated_array(values: &[u32; 2]) -> u32 {
            let mut output: [u32; 2] = [0; 2];
            output[0] = values[0].wrapping_add(1);
            output[0]
        }

        fn from_typed_closure(value: usize) -> usize {
            let add_one = |a: usize| a.wrapping_add(1);
            add_one(value)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "wrapping_width_inference", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("wrappingSub(values[Number(0n)], WIDE_LIMBS[Number(0n)], 64)"),
        "const-backed u64 array elements should preserve their width:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(alias, 1n, 64)"),
        "overflowing-sub tuple results should preserve their u64 width through aliases:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(this.$fvalues[Number(index)], 1n, 64)"),
        "struct field array elements should preserve their u64 width:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(value, 1n, 32)"),
        "primitive associated-call return types should preserve u32 width:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(value, 1n, 64)"),
        "primitive type aliases should preserve their target width:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(values[Number(0n)], 1n, 32)"),
        "annotated local arrays should preserve their u32 element width:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(a, 1n, 64)"),
        "explicit closure parameter types should inform width-aware codegen:\n{ts_code}"
    );
    run_ts_code(&ts_code, "wrapping_width_inference");
}

#[test]
fn vole_rotation_closures_preserve_usize_width() {
    let root = workspace_root();
    for relative_path in [
        "crates/spec/volar-spec/src/vole.rs",
        "crates/spec/volar-spec/src/vole/vope.rs",
    ] {
        let source = fs::read_to_string(root.join(relative_path)).unwrap();
        let stem = Path::new(relative_path)
            .file_stem()
            .unwrap()
            .to_string_lossy();
        let module = parse_source(&source, &stem, &["volar_spec".to_string()]).unwrap();
        let ts_code = print_module_typescript(&module);
        assert!(
            ts_code.contains("wrappingSub(a, n_param, 64)"),
            "usize rotation closure should wrap at 64 bits in {relative_path}:\n{ts_code}"
        );
        assert!(
            ts_code.contains("wrappingAdd(a, n_param, 64)"),
            "usize rotation closure should wrap at 64 bits in {relative_path}:\n{ts_code}"
        );
    }
}

#[test]
fn loop_tail_unit_match_does_not_return_from_function() {
    let source = r#"
        fn after_loop(values: &[Option<u32>]) -> u32 {
            for value in values.iter() {
                if let Some(_found) = value {}
            }
            7
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "loop_tail_unit_match", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "loop_tail_unit_match");
}

#[test]
fn seeded_ts_emits_methods_called_transitively() {
    let source = r#"
        struct Nested { value: u32 }

        impl Nested {
            fn inner(&self) -> u32 { self.value }
            fn outer(&self) -> u32 { self.inner() }
        }

        fn entry(value: &Nested) -> u32 {
            value.outer()
        }
    "#;
    let module = parse_source(source, "seeded_methods", &[]).unwrap();
    let ts_code = print_module_typescript_seeded(&module, &["entry"]);
    assert!(
        ts_code.contains("inner()"),
        "nested method should be emitted:\n{ts_code}"
    );
    assert!(
        ts_code.contains("outer()"),
        "seed method should be emitted:\n{ts_code}"
    );
    run_ts_code(&ts_code, "seeded_transitive_methods");
}

#[test]
fn seeded_ts_emits_associated_methods_called_by_qualified_path() {
    let source = r#"
        struct Factory;
        impl Factory {
            fn make() -> u32 { helper() }
        }

        fn helper() -> u32 { 42 }
        fn entry() -> u32 { Factory::make() }
    "#;
    let module = parse_source(source, "seeded_associated_method", &[]).unwrap();
    let ts_code = print_module_typescript_seeded(&module, &["entry"]);
    assert!(
        ts_code.contains("static make()") && ts_code.contains("export function helper()"),
        "associated function and its free-function dependency should be emitted:\n{ts_code}"
    );
    run_ts_code(&ts_code, "seeded_associated_method");
}

#[test]
fn seeded_ts_emits_functions_called_inside_iterator_pipelines() {
    let source = r#"
        struct Item { value: u32 }

        impl Item {
            fn increment(&self) -> u32 { self.value + 1 }
        }

        fn wrap(value: u32) -> u32 { value }

        fn entry(values: &[Item]) -> Vec<u32> {
            values.iter().map(|item| wrap(item.increment())).collect()
        }
    "#;
    let module = parse_source(source, "seeded_iter_pipeline", &[]).unwrap();
    let ts_code = print_module_typescript_seeded(&module, &["entry"]);
    run_ts_code(&ts_code, "seeded_iterator_pipeline_calls");
}

#[test]
fn slice_get_range_returns_an_optional_slice() {
    let source = r#"
        fn get_range(values: &[u8], start: usize, end: usize) -> Option<&[u8]> {
            values.get(start..end)
        }

        fn get_range_inclusive(values: &[u8], start: usize, end: usize) -> Option<&[u8]> {
            values.get(start..=end)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "slice_get_range", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains(".slice(Number(__get_start), Number(__get_end))")
            && ts_code.contains("__get_end <= BigInt(__get_array.length)"),
        "slice get(range) should bounds-check and return an optional slice:\n{ts_code}"
    );
    assert!(
        ts_code.contains("const __get_end = BigInt(end) + 1n"),
        "inclusive slice ranges should include the final element:\n{ts_code}"
    );
    run_ts_code(&ts_code, "slice_get_range");
}

#[test]
fn integer_try_from_and_vec_slice_helpers_lower_to_typescript() {
    let source = r#"
        fn checked_u32(value: u64) -> u32 {
            u32::try_from(value).expect("value must fit in u32")
        }

        fn checked_add_u8(left: u8, right: u8) -> Option<u8> {
            left.checked_add(right)
        }

        fn checked_sub_i128(left: i128, right: i128) -> Option<i128> {
            left.checked_sub(right)
        }

        fn saturating_add_u8(left: u8, right: u8) -> u8 {
            left.saturating_add(right)
        }

        fn first_value(values: &[u32]) -> Option<u32> {
            values.first()
        }

        fn slice_len(values: Vec<u32>) -> usize {
            Vec::as_slice(&values).len()
        }

        fn copy_slice(values: &[u8]) -> Vec<u8> {
            Vec::from(values)
        }

        fn mapped_slice_lengths(values: Vec<Vec<u32>>) -> Vec<usize> {
            values.iter().map(Vec::as_slice).map(|slice| slice.len()).collect()
        }

        fn enumerated_indices(values: &[u32]) -> Vec<usize> {
            values.iter().enumerate().map(|(index, _)| index).collect()
        }

        struct HashOutput { bytes: Vec<u8> }
        impl HashOutput {
            fn finalize(&self) -> Vec<u8> { self.bytes.clone() }
        }
        fn digest_bytes(digest: &HashOutput) -> Vec<u8> { digest.finalize() }

        fn digest_output_bytes() -> Vec<u8> {
            let hasher = Sha3_256::new();
            Digest::finalize(hasher).to_vec()
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "integer_and_vec_helpers", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("Array.from(values)"),
        "Vec::from(slice) must copy the slice into a TypeScript array:\n{ts_code}"
    );
    assert!(
        ts_code.contains("Array.from(hasher.finalize(), BigInt)"),
        "digest output bytes must be converted from JS numbers to Rust u8 bigints:\n{ts_code}"
    );
    assert!(
        ts_code.contains("__result <= 255n ? __result : undefined"),
        "u8 checked arithmetic must preserve overflow as None:\n{ts_code}"
    );
    assert!(
        ts_code.contains("__result >= -170141183460469231731687303715884105728n"),
        "i128 checked arithmetic must enforce the signed lower bound:\n{ts_code}"
    );
    assert!(
        ts_code.contains("__result > 255n ? 255n : __result"),
        "u8 saturating arithmetic must clamp rather than wrap:\n{ts_code}"
    );
    run_ts_code(&ts_code, "integer_try_from_and_vec_slice_helpers");
}

#[test]
fn wrapping_width_infers_length_and_custom_method_returns() {
    let source = r#"
        struct FerretParams { values: Vec<u8> }

        impl FerretParams {
            fn splen(&self) -> usize { self.values.len() }
        }

        fn from_slice_len(values: &[u8]) -> usize {
            values.len().wrapping_add(1)
        }

        fn from_custom_method(params: &FerretParams) -> usize {
            params.splen().wrapping_add(1)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "wrapping_method_returns", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("wrappingAdd(BigInt(values.length), 1n, 64)"),
        "slice length is usize and should retain 64-bit wrapping:\n{ts_code}"
    );
    assert!(
        ts_code.contains("wrappingAdd(params.splen(), 1n, 64)"),
        "custom usize method returns should retain 64-bit wrapping:\n{ts_code}"
    );
    run_ts_code(&ts_code, "wrapping_method_return_widths");
}

#[test]
fn wrapping_width_infers_checked_mul_and_then_chain() {
    let source = r#"
        struct Params { k: usize, t: usize }

        impl Params {
            fn count(&self) -> usize {
                let body = self.t.checked_mul(8).and_then(|noise| self.k.checked_add(noise)).expect("count");
                body.wrapping_add(1)
            }
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "checked_mul_width", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("wrappingAdd(body, 1n, 64)"),
        "checked_mul/and_then/expect must preserve the usize width of the bound value:\n{ts_code}"
    );
    run_ts_code(&ts_code, "checked_mul_width");
}

#[test]
fn wrapping_width_infers_checked_integer_methods_after_try() {
    let source = r#"
        fn checked(value: usize) -> Result<usize, u8> {
            let rounded = value.checked_next_power_of_two().ok_or(1u8)?;
            Ok(rounded.wrapping_add(1))
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "checked_method_width", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("wrappingAdd(rounded, 1n, 64)"),
        "try-unwrapped checked integer methods should retain their usize width:\n{ts_code}"
    );
}

#[test]
fn bit_count_methods_preserve_rust_integer_result_types() {
    let source = r#"
        fn trailing(value: u32) -> u32 { value.trailing_zeros() }
        fn leading(value: u32) -> u32 { value.leading_zeros() }
        fn scale(value: u32) -> u32 { 32u32.saturating_sub(value.trailing_zeros()) }
        fn trailing_wide(value: u64) -> u32 { value.trailing_zeros() }
        fn leading_wide(value: u64) -> u32 { value.leading_zeros() }
        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "bit_count_types", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("BigInt.asUintN(32, value)"),
        "Rust u32 bit-count methods must operate on 32-bit values:\n{ts_code}"
    );
    assert!(
        ts_code.contains("BigInt.asUintN(64, value)"),
        "Rust u64 bit-count methods must operate on 64-bit values:\n{ts_code}"
    );
    run_ts_code(&ts_code, "bit_count_result_types");
}

#[test]
fn tuple_struct_class_fields_follow_position_after_ir_field_renaming() {
    let mut module = parse_source(
        "struct Marker(u64); fn make_marker(value: u64) -> Marker { crate::Marker(value) } fn encrypt_block() {}",
        "tuple_marker",
        &[],
    )
    .unwrap();
    // Dynamic lowering can assign a semantic field name while retaining the
    // tuple layout; emitted storage must still use numeric tuple properties.
    module.structs[0].fields[0].name = "runtime_len".to_string();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("[0]!: bigint;"),
        "tuple layout should use a numeric TypeScript property:\n{ts_code}"
    );
    run_ts_code(&ts_code, "tuple_struct_positional_fields");
}

#[test]
fn option_combinators_and_vec_split_off_lower_to_typescript() {
    let source = r#"
        fn checked_round_up(value: usize) -> Option<usize> {
            value.checked_next_power_of_two()
        }

        fn continue_option(value: Option<u32>) -> Option<u32> {
            value.and_then(|item| Some(item + 1))
        }

        fn has_even_value(value: Option<u32>) -> bool {
            value.is_some_and(|item| item % 2 == 0)
        }

        fn split_values(values: &mut Vec<u32>) -> Vec<u32> {
            values.split_off(1)
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "option_combinators", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "option_combinators_and_split_off");
}

#[test]
fn mutable_iteration_writes_through_enumerate_and_slice_loops() {
    let source = r#"
        fn mutate_enumerated(values: &mut [u32]) {
            for (index, value) in values.iter_mut().enumerate() {
                *value = *value + index as u32;
            }
        }

        fn mutate_slice(values: &mut [u32]) {
            for value in values {
                *value = *value + 1;
            }
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "mutable_iteration", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    run_ts_code(&ts_code, "mutable_iteration_writes_through");
}

// ---------------------------------------------------------------------------
// Component test table
// ---------------------------------------------------------------------------

#[test]
fn let_match_return_arm_uses_statement_control_flow() {
    let source = r#"
        fn first_or_none(values: Option<Vec<u32>>) -> Option<u32> {
            let values = match values {
                Some(values) => values,
                None => return None,
            };
            values.first()
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "let_match_return", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("__volar_match_result_"),
        "a let-match with an early return should lower to statement control flow:\n{ts_code}"
    );
    run_ts_code(&ts_code, "let_match_return_arm");
}

#[test]
fn enum_impl_methods_are_emitted_on_variant_classes() {
    let source = r#"
        enum Status { Ready, Busy }

        impl Status {
            fn is_ready(&self) -> bool {
                match self {
                    Status::Ready => true,
                    Status::Busy => false,
                }
            }
        }

        fn status_is_ready(status: &Status) -> bool {
            status.is_ready()
        }

        fn encrypt_block() {}
    "#;
    let module = parse_source(source, "enum_impl_methods", &[]).unwrap();
    let ts_code = print_module_typescript(&module);
    assert!(
        ts_code.contains("is_ready()"),
        "enum impl methods must be attached to their variant classes:\n{ts_code}"
    );
    run_ts_code(&ts_code, "enum_impl_methods");
}

macro_rules! ts_component {
    ($($name:ident: [$($seed:literal),+ $(,)?]),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                run_ts_component(&[$($seed),+], stringify!($name));
            }
        )*
    }
}

ts_component! {
    vole_prover: [
        "vole_and_prover_step",
        "vole_sbox_prover_step",
        "vole_mul3_prover_step",
    ],
    vole_verifier: [
        "vole_and_verifier_check",
        "vole_sbox_verifier_check",
        "vole_mul3_verifier_check",
    ],
    vole_setup: [
        "random_nonzero_delta",
        "vole_commit_bit",
        "derive_and_q",
    ],
    tfhe: [
        "tfhe_xor",
        "tfhe_not",
        "tfhe_gate_bootstrapping_and",
        "tfhe_gate_bootstrapping_or",
        "tfhe_cmux",
        "tfhe_programmable_bootstrap",
        "tfhe_lut_read",
    ],
    faest_core: [
        "sign",
        "verify",
        "keygen",
    ],
}
