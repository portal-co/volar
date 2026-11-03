// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! Phase-2 scaffold test: the `lwe` element exports both Lean sides —
//! mxx's scope relation via `mxx_ir_core::lean::export` and the native
//! function via `printer_lean` — so the transport layer has its two ends.
//! Writes both to a temp dir for inspection and asserts generation succeeds.

use volar_mxx_interop::lower::{HelperMode, LowerOptions, lower_validated_to_module};
use volar_mxx_interop::programs::elements;

#[test]
fn lwe_element_exports_both_lean_sides() {
    let element = elements().remove(0);
    let (built, validated) = (element.build)().expect("build lwe program");

    // mxx side: the scope relation for the frozen graph.
    let artifact = mxx_ir_core::lean::export(
        &validated,
        &mxx_ir_core::lean::ExportOptions {
            module_name: format!("Stage_mxx_{}", element.element),
            namespace: format!("Stage_mxx_{}", element.element),
            ..Default::default()
        },
    )
    .expect("mxx lean export");

    // Native side: the lowered module through the spec's Lean backend.
    let module = lower_validated_to_module(
        &built.graph,
        &validated,
        &LowerOptions {
            function_name: element.function.into(),
            helpers: HelperMode::Inline,
            module_name: format!("mxx_{}", element.element),
        },
    )
    .expect("lower");
    let names: std::collections::BTreeSet<String> = module
        .functions
        .iter()
        .map(|f| f.name.clone())
        .chain(module.consts.iter().map(|c| c.name.clone()))
        .collect();
    let native = volar_compiler::printer_lean::print_module_lean_with(
        &module,
        &names,
        volar_compiler::printer_lean::LeanPrintOptions { exact_nat: true },
    )
    .expect("native lean print");

    let dir = std::env::temp_dir().join(format!("volar-mxx-lean-smoke-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let mxx_path = dir.join("Stage.lean");
    let native_path = dir.join("Native.lean");
    std::fs::write(&mxx_path, &artifact.source).unwrap();
    std::fs::write(&native_path, &native).unwrap();
    eprintln!("mxx stage relation: {}", mxx_path.display());
    eprintln!("native lean:        {}", native_path.display());
    assert!(artifact.source.contains("def generatedRoot"));
    assert!(native.contains("mxx_lwe_sample"));
}
