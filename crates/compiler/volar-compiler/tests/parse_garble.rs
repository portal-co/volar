#[test]
fn parse_garble() {
    let src = include_str!("../../spec/volar-spec/src/garble.rs");
    let inputs = [volar_compiler::parser::SourceInput { source: src, name: "garble.rs" }];
    match volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()]) {
        Ok(m) => {
            eprintln!("structs: {:?}", m.structs.iter().map(|s| s.kind.to_string()).collect::<Vec<_>>());
            eprintln!("functions: {:?}", m.functions.iter().map(|f| f.name.clone()).collect::<Vec<_>>());
        }
        Err(e) => panic!("parse failed: {e:?}"),
    }
}
