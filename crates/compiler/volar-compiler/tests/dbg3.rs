#[test]
fn dbg3() {
    // mirror xtask's merged parse
    let mut sources = Vec::new();
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../spec/volar-spec/src");
    let mut files: Vec<_> = std::fs::read_dir(&dir).unwrap().flatten().collect();
    files.sort_by_key(|e| e.file_name());
    for e in files {
        let p = e.path();
        if p.extension().map_or(false, |e| e == "rs") {
            let content = std::fs::read_to_string(&p).unwrap();
            if content.lines().any(|l| l.trim_start().starts_with("//! @volar-codegen-exclude:")) { continue; }
            sources.push((content, p.file_stem().unwrap().to_string_lossy().to_string()));
        }
    }
    let inputs: Vec<_> = sources.iter().map(|(c, n)| volar_compiler::parser::SourceInput { source: c.as_str(), name: n.as_str() }).collect();
    let m = volar_compiler::parse_sources(&inputs, "volar_spec", &["volar_spec".to_string()]).expect("parse");
    let only: std::collections::BTreeSet<String> =
        ["Eval", "Garble", "GarbleTable", "GlobalSecret", "absorb_args_bases"]
            .into_iter().map(String::from).collect();
    let out = volar_compiler::printer_lean_bridge::print_module_lean_bridge(&m, &only, "garble").unwrap();
    for line in out.lines() {
        if line.contains("and_result") { println!("{line}"); }
    }
}
