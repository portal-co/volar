//! Smoke test: the garble spec subset prints to the Lean bridge file.
#[test]
fn print_garble_lean_bridge() {
    let src = include_str!("../../../spec/volar-spec/src/garble.rs");
    let inputs = [volar_compiler::parser::SourceInput { source: src, name: "garble.rs" }];
    let m = volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()]).expect("parse");
    let only: std::collections::BTreeSet<String> =
        ["Eval", "Garble", "GarbleTable", "GlobalSecret", "absorb_args_bases"]
            .into_iter().map(String::from).collect();
    match volar_compiler::printer_lean_bridge::print_module_lean_bridge(&m, &only, "garble") {
        Ok(s) => std::fs::write("/tmp/garble_bridge.lean", s).unwrap(),
        Err(e) => panic!("print failed: {e}"),
    }
}
