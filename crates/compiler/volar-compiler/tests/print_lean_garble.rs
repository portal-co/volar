//! Smoke test: the garble spec subset prints as native Lean.
#[test]
fn print_garble_lean() {
    let src = include_str!("../../../spec/volar-spec/src/garble.rs");
    let inputs = [volar_compiler::parser::SourceInput { source: src, name: "garble.rs" }];
    let m = volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()])
        .expect("parse garble.rs");
    let only: std::collections::BTreeSet<String> =
        ["Eval", "Garble", "GarbleTable", "GlobalSecret", "absorb_args_bases"]
            .into_iter()
            .map(String::from)
            .collect();
    let out = volar_compiler::printer_lean::print_module_lean(&m, &only).expect("print");
    for expected in [
        "structure Garble (n : Nat)",
        "namespace Garble",
        "def share",
        "def gen_and_table_pol",
        "def absorb_args_bases",
        "structure DigestSt",
    ] {
        assert!(out.contains(expected), "missing: {expected}");
    }
}
