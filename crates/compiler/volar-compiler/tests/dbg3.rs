#[test]
fn dbg3() {
    let src = include_str!("../../../spec/volar-spec/src/garble.rs");
    let inputs = [volar_compiler::parser::SourceInput { source: src, name: "garble.rs" }];
    let m = volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()]).expect("parse");
    for i in &m.impls {
        for it in &i.items {
            if let volar_compiler::ir::IrImplItem::Method(f) = it {
                if f.name == "encode" {
                    println!("params: {:?}", f.params.iter().map(|p| format!("{}: {:?}", p.name, p.ty)).collect::<Vec<_>>());
                }
            }
        }
    }
}
