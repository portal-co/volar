#[test]
fn dbg3() {
    let src = include_str!("../../../spec/volar-spec/src/garble.rs");
    let inputs = [volar_compiler::parser::SourceInput { source: src, name: "garble.rs" }];
    let m = volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()]).expect("parse");
    let mut out = String::new();
    for i in &m.impls {
        for it in &i.items {
            if let volar_compiler::ir::IrImplItem::Method(f) = it {
                if matches!(f.name.as_str(), "and_via_table") {
                    out.push_str(&format!("=== {:?}::{}\n{:.2500?}\n\n", i.self_ty, f.name, f.body));
                }
            }
        }
    }
    std::fs::write("/tmp/garble-ir5.txt", out).unwrap();
}
