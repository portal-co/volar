#[test]
fn print_garble_lean() {
    let src = include_str!("../../../spec/volar-spec/src/garble.rs");
    let inputs = [volar_compiler::parser::SourceInput { source: src, name: "garble.rs" }];
    let m = volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()]).expect("parse");
    let only: std::collections::BTreeSet<String> =
        ["Eval", "Garble", "GarbleTable", "GlobalSecret", "absorb_args_bases"]
            .into_iter().map(String::from).collect();
    match volar_compiler::printer_lean::print_module_lean(&m, &only) {
        Ok(s) => std::fs::write("/tmp/garble.lean", s).unwrap(),
        Err(e) => panic!("print failed: {e}"),
    }
}
