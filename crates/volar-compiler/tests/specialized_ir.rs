//! Integration test demonstrating the specialized IR

use std::fs;
use std::path::Path;
use volar_compiler::{
    ArrayKind, AssociatedType, CryptoMethod, CryptoTrait, IrExpr, IrImplItem, IrStmt, IrType,
    MathTrait, MethodKind, PrimitiveType, StructKind, TraitKind, VoleMethod, parse_sources,
};

fn read_volar_spec_sources() -> Vec<(String, String)> {
    let base_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("volar-spec")
        .join("src");

    let mut sources = Vec::new();

    fn collect_rs_files(dir: &Path, sources: &mut Vec<(String, String)>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().map_or(false, |e| e == "rs") {
                    if let Ok(content) = fs::read_to_string(&path) {
                        let name = path.file_stem().unwrap().to_string_lossy().to_string();
                        sources.push((content, name));
                    }
                } else if path.is_dir() {
                    collect_rs_files(&path, sources);
                }
            }
        }
    }

    collect_rs_files(&base_path, &mut sources);
    sources
}

#[test]
fn test_specialize_volar_spec() {
    let sources = read_volar_spec_sources();
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(content, name)| (content.as_str(), name.as_str()))
        .collect();

    let spec = parse_sources(&sources_ref, "volar_spec").unwrap();

    println!("\n=== Specialized Module Statistics ===");
    println!("Structs: {}", spec.structs.len());
    println!("Traits: {}", spec.traits.len());
    println!("Impls: {}", spec.impls.len());
    println!("Functions: {}", spec.functions.len());

    // Count struct kinds
    let mut vole_structs = 0;
    let mut crypto_structs = 0;
    let mut custom_structs = 0;

    println!("\n=== Struct Classifications ===");
    for s in &spec.structs {
        match &s.kind {
            StructKind::Custom(name) => {
                match name.as_str() {
                    "Delta" | "Q" | "Vope" | "BitVole" => {
                        vole_structs += 1;
                        println!("  VOLE: {}", name);
                    }
                    "ABO" | "ABOOpening" | "CommitmentCore" => {
                        crypto_structs += 1;
                        println!("  Crypto: {}", name);
                    }
                    _ => {
                        custom_structs += 1;
                        println!("  Custom: {}", name);
                    }
                }
            }
            other => {
                println!("  Other: {:?}", other);
            }
        }
    }

    println!("\n  VOLE structs: {}", vole_structs);
    println!("  Crypto structs: {}", crypto_structs);
    println!("  Custom structs: {}", custom_structs);

    // Count trait classifications

    println!("\n=== Trait Classifications ===");
    for t in &spec.traits {
        match &t.kind {
            TraitKind::Math(m) => {
                println!("  Math: {:?}", m);
            }
            TraitKind::Crypto(c) => {
                println!("  Crypto: {:?}", c);
            }
            TraitKind::Custom(name) => {
                println!("  Custom: {}", name);
            }
            TraitKind::External { path } => {
                println!("  External: {}", path.join("::"));
            }
            TraitKind::Into(ty) => {
                println!("  Into: {:?}", ty);
            }
            TraitKind::AsRef(ty) => {
                println!("  AsRef: {:?}", ty);
            }
            TraitKind::Fn(inp, ty) => {
                println!("  Fn-like trait: {:?} -> {:?}", inp, ty);
            }
        }
    }

    // Count impl block trait kinds
    let mut math_impls = 0;
    let mut crypto_impls = 0;
    let mut std_impls = 0;
    let mut inherent_impls = 0;

    println!("\n=== Impl Block Classifications ===");
    for imp in &spec.impls {
        match &imp.trait_ {
            Some(tr) => match &tr.kind {
                TraitKind::Math(m) => {
                    math_impls += 1;
                    println!("  Math impl: {:?}", m);
                }
                TraitKind::Crypto(c) => {
                    crypto_impls += 1;
                    println!("  Crypto impl: {:?}", c);
                }
                TraitKind::Custom(name) => {
                    std_impls += 1;
                    println!("  Std impl: {}", name);
                }
                _ => {
                    std_impls += 1;
                }
            },
            None => {
                inherent_impls += 1;
            }
        }
    }

    println!("\n  Math trait impls: {}", math_impls);
    println!("  Crypto trait impls: {}", crypto_impls);
    println!("  Standard trait impls: {}", std_impls);
    println!("  Inherent impls: {}", inherent_impls);

    // Analyze field types
    println!("\n=== Field Type Analysis ===");
    let mut primitive_fields = 0;
    let mut array_fields = 0;
    let mut struct_fields = 0;

    for s in &spec.structs {
        for field in &s.fields {
            match &field.ty {
                IrType::Primitive(p) => {
                    primitive_fields += 1;
                    println!("  {} ({}): primitive {:?}", s.kind, field.name, p);
                }
                IrType::Array {
                    kind,
                    elem: _,
                    len: _,
                } => {
                    array_fields += 1;
                    println!("  {} ({}): {:?} array", s.kind, field.name, kind);
                }
                IrType::Struct { kind, .. } => {
                    struct_fields += 1;
                    println!("  {} ({}): struct {:?}", s.kind, field.name, kind);
                }
                IrType::TypeParam(name) => {
                    println!("  {} ({}): type param {}", s.kind, field.name, name);
                }
                _ => {}
            }
        }
    }

    println!("\n  Primitive fields: {}", primitive_fields);
    println!("  Array fields: {}", array_fields);
    println!("  Struct fields: {}", struct_fields);

    // Verify expected classifications
    assert!(
        vole_structs >= 4,
        "Should have at least 4 VOLE structs (Delta, Q, Vope, BitVole)"
    );
    assert!(
        crypto_structs >= 2,
        "Should have at least 2 crypto structs (ABO, ABOOpening)"
    );
    assert!(
        math_impls >= 3,
        "Should have at least 3 math trait impls (Add, Mul, BitXor)"
    );
}

#[test]
fn test_method_classification() {
    let source = r#"
        fn test() {
            // VOLE methods
            let a = delta.remap(|i| i);
            let b = vope.rotate_left(1);
            
            // Crypto methods
            let e = cipher.encrypt_block(&mut block);
            let f = hasher.update(&data);
            let g = hasher.finalize();
            
            // Standard methods
            let h = x.clone();
            let i = y.into();
        }
    "#;

    use volar_compiler::{IrExpr, IrStmt, parse_source};

    let spec = parse_source(source, "test").unwrap();

    let f = &spec.functions[0];

    let mut vole_methods = 0;
    let mut crypto_methods = 0;
    let mut std_methods = 0;
    let mut iter_methods = 0;

    for stmt in &f.body.stmts {
        if let IrStmt::Let {
            init: Some(expr), ..
        } = stmt
        {
            if let IrExpr::MethodCall { method, .. } = expr {
                match method {
                    MethodKind::Vole(v) => {
                        vole_methods += 1;
                        println!("VOLE method: {:?}", v);
                    }
                    MethodKind::Crypto(c) => {
                        crypto_methods += 1;
                        println!("Crypto method: {:?}", c);
                    }
                    MethodKind::Std(s) => {
                        std_methods += 1;
                        println!("Std method: {}", s);
                    }
                    MethodKind::Unknown(u) => {
                        println!("Unknown method: {}", u);
                    }
                }
            }
        }
    }

    assert!(
        vole_methods >= 2,
        "Should recognize VOLE methods (remap, rotate_left)"
    );
    assert!(crypto_methods >= 3, "Should recognize crypto methods");
    assert!(std_methods >= 2, "Should recognize standard methods");
}

#[test]
fn test_array_operations() {
    let source = r#"
        fn test() {
            // Array generate
            let a = GenericArray::generate(|i| i * 2);
            
            // Array map
            let b = arr.map(|x| x + 1);
            
            // Array zip
            let c = arr.zip(other, |a, b| a + b);
        }
    "#;

    use volar_compiler::{IrExpr, IrStmt, parse_source};

    let spec = parse_source(source, "test").unwrap();

    let f = &spec.functions[0];

    let mut generate_count = 0;
    let mut map_count = 0;
    let mut zip_count = 0;

    for stmt in &f.body.stmts {
        if let IrStmt::Let {
            init: Some(expr), ..
        } = stmt
        {
            match expr {
                IrExpr::ArrayGenerate { index_var, .. } => {
                    generate_count += 1;
                    println!("ArrayGenerate with index var: {}", index_var);
                }
                IrExpr::ArrayMap { elem_var, .. } => {
                    map_count += 1;
                    println!("ArrayMap with elem var: {}", elem_var);
                }
                IrExpr::ArrayZip {
                    left_var,
                    right_var,
                    ..
                } => {
                    zip_count += 1;
                    println!("ArrayZip with vars: {}, {}", left_var, right_var);
                }
                _ => {}
            }
        }
    }

    assert_eq!(generate_count, 1, "Should recognize ArrayGenerate");
    assert_eq!(map_count, 1, "Should recognize ArrayMap");
    assert_eq!(zip_count, 1, "Should recognize ArrayZip");
}

#[test]
fn test_bounded_loops() {
    let source = r#"
        fn test() {
            // Range loop (bounded) - with semicolon to make it a statement
            for i in 0..10 {
                x = i;
            };
            
            // Range loop with expression end
            for j in 0..N::to_usize() {
                y = j;
            };
            
            // Iteration over collection (bounded)
            for item in arr.iter() {
                z = item;
            };
        }
    "#;

    use volar_compiler::{IrExpr, IrStmt, parse_source};

    let spec = parse_source(source, "test").unwrap();

    let f = &spec.functions[0];

    let mut bounded_loops = 0;
    let mut iter_loops = 0;

    println!("Statements in function body: {}", f.body.stmts.len());
    for (i, stmt) in f.body.stmts.iter().enumerate() {
        match stmt {
            IrStmt::Semi(expr) | IrStmt::Expr(expr) => match expr {
                IrExpr::BoundedLoop { var, .. } => {
                    bounded_loops += 1;
                    println!("  Stmt {}: BoundedLoop with var: {}", i, var);
                }
                IrExpr::IterLoop { .. } => {
                    iter_loops += 1;
                    println!("  Stmt {}: IterLoop found", i);
                }
                _ => {
                    println!("  Stmt {}: Other expr", i);
                }
            },
            _ => {}
        }
    }

    assert!(
        bounded_loops >= 2,
        "Should recognize BoundedLoop (for i in 0..N), found {}",
        bounded_loops
    );
    assert!(
        iter_loops >= 1,
        "Should recognize IterLoop (for item in arr.iter()), found {}",
        iter_loops
    );
}

#[test]
fn test_primitive_type_classification() {
    use volar_compiler::parse_source;

    let source = r#"
        fn test(
            a: u8,
            b: u32,
            c: u64,
            d: usize,
            e: bool,
            f: Bit,
            g: Galois,
            h: Galois64,
            i: BitsInBytes,
            j: BitsInBytes64,
        ) {
        }
    "#;

    let spec = parse_source(source, "test").unwrap();

    let f = &spec.functions[0];

    let expected = [
        ("a", PrimitiveType::U8),
        ("b", PrimitiveType::U32),
        ("c", PrimitiveType::U64),
        ("d", PrimitiveType::Usize),
        ("e", PrimitiveType::Bool),
        ("f", PrimitiveType::Bit),
        ("g", PrimitiveType::Galois),
        ("h", PrimitiveType::Galois64),
        ("i", PrimitiveType::BitsInBytes),
        ("j", PrimitiveType::BitsInBytes64),
    ];

    for (i, (name, expected_ty)) in expected.iter().enumerate() {
        let param = &f.params[i];
        assert_eq!(param.name, *name);
        match &param.ty {
            IrType::Primitive(p) => {
                assert_eq!(p, expected_ty, "Type mismatch for {}", name);
            }
            other => {
                panic!("Expected Primitive for {}, got {:?}", name, other);
            }
        }
    }

    // Check field element classification
    for prim in [
        PrimitiveType::Bit,
        PrimitiveType::Galois,
        PrimitiveType::Galois64,
        PrimitiveType::BitsInBytes,
        PrimitiveType::BitsInBytes64,
    ] {
        assert!(
            prim.is_field_element(),
            "{:?} should be a field element",
            prim
        );
    }

    for prim in [PrimitiveType::U8, PrimitiveType::U32, PrimitiveType::Bool] {
        assert!(
            !prim.is_field_element(),
            "{:?} should not be a field element",
            prim
        );
    }
}

#[test]
fn test_array_type_classification() {
    use volar_compiler::parse_source;

    let source = r#"
        fn test(
            a: GenericArray<u8, N>,
            b: [u8; 32],
            c: &[u8],
        ) {
        }
    "#;

    let spec = parse_source(source, "test").unwrap();

    let f = &spec.functions[0];

    // Check GenericArray
    match &f.params[0].ty {
        IrType::Array {
            kind: ArrayKind::GenericArray,
            elem,
            ..
        } => {
            assert!(matches!(**elem, IrType::Primitive(PrimitiveType::U8)));
        }
        other => panic!("Expected GenericArray, got {:?}", other),
    }

    // Check fixed array
    match &f.params[1].ty {
        IrType::Array {
            kind: ArrayKind::FixedArray,
            elem,
            ..
        } => {
            assert!(matches!(**elem, IrType::Primitive(PrimitiveType::U8)));
        }
        other => panic!("Expected FixedArray, got {:?}", other),
    }

    // Check slice (inside reference)
    match &f.params[2].ty {
        IrType::Reference { elem, .. } => match &**elem {
            IrType::Array {
                kind: ArrayKind::Slice,
                elem: inner,
                ..
            } => {
                assert!(matches!(**inner, IrType::Primitive(PrimitiveType::U8)));
            }
            other => panic!("Expected Slice, got {:?}", other),
        },
        other => panic!("Expected Reference, got {:?}", other),
    }
}
