//! Integration test demonstrating the specialized IR

use std::fs;
use std::path::Path;
use volar_compiler::{
    ArrayKind, AssociatedType, IrExpr, IrImplItem, IrStmt, IrType,
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
            TraitKind::Custom(name) => {
                println!("  Custom/Domain: {}", name);
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
    let mut custom_impls = 0;
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
                TraitKind::Custom(name) => {
                    custom_impls += 1;
                    println!("  Custom impl: {}", name);
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
    println!("  Custom trait impls: {}", custom_impls);
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
    assert!(std_methods >= 2, "Should recognize standard methods");
}

#[test]
fn test_array_operations() {
    let source = r#"
        fn test() {
            // Array generate
            let a = GenericArray::<u32, U4>::generate(|i| i * 2);
            
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
    let mut method_map_count = 0;
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
                IrExpr::RawMap { elem_var, .. } => {
                    method_map_count += 1;
                    println!("RawMap with var: {}", elem_var);
                }
                IrExpr::RawZip { left_var, right_var, .. } => {
                    zip_count += 1;
                    println!("RawZip with vars: {}, {}", left_var, right_var);
                }
                _ => {}
            }
        }
    }

    assert_eq!(generate_count, 1, "Should recognize ArrayGenerate");
    assert_eq!(method_map_count, 1, "Should recognize non-iterator map as RawMap");
    assert_eq!(zip_count, 1, "Should recognize zip as RawZip");
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

// ============================================================================
// Iterator chain tests
// ============================================================================

#[test]
fn test_iter_chain_simple_fold() {
    use volar_compiler::{parse_source, IrIterChain, IterChainSource, IterStep, IterTerminal, IterMethod};

    let source = r#"
        fn test() {
            let x = arr.iter().fold(0, |acc, elem| acc + elem);
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let f = &module.functions[0];

    if let IrStmt::Let { init: Some(expr), .. } = &f.body.stmts[0] {
        if let IrExpr::IterPipeline(chain) = expr {
            // Source should be Method { collection: arr, method: Iter }
            match &chain.source {
                IterChainSource::Method { method, .. } => {
                    assert_eq!(*method, IterMethod::Iter);
                }
                other => panic!("Expected Method source, got {:?}", other),
            }
            assert!(chain.steps.is_empty(), "No intermediate steps");
            match &chain.terminal {
                IterTerminal::Fold { acc_var, elem_var, .. } => {
                    assert_eq!(acc_var, "acc");
                    assert_eq!(elem_var, "elem");
                }
                other => panic!("Expected Fold terminal, got {:?}", other),
            }
        } else {
            panic!("Expected IterPipeline, got {:?}", expr);
        }
    } else {
        panic!("Expected Let statement");
    }
}

#[test]
fn test_iter_chain_enumerate_filter_map_fold() {
    use volar_compiler::{parse_source, IterChainSource, IterStep, IterTerminal, IterMethod};

    let source = r#"
        fn test() {
            let x = s.iter().enumerate().filter_map(|a| compute(a)).fold(init, |a, b| a + b);
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let f = &module.functions[0];

    if let IrStmt::Let { init: Some(IrExpr::IterPipeline(chain)), .. } = &f.body.stmts[0] {
        // Source
        assert!(matches!(&chain.source, IterChainSource::Method { method: IterMethod::Iter, .. }));
        // Steps: enumerate, filter_map
        assert_eq!(chain.steps.len(), 2);
        assert!(matches!(&chain.steps[0], IterStep::Enumerate));
        match &chain.steps[1] {
            IterStep::FilterMap { var, .. } => assert_eq!(var, "a"),
            other => panic!("Expected FilterMap, got {:?}", other),
        }
        // Terminal: fold
        assert!(matches!(&chain.terminal, IterTerminal::Fold { acc_var, elem_var, .. } if acc_var == "a" && elem_var == "b"));
    } else {
        panic!("Expected IterPipeline");
    }
}

#[test]
fn test_iter_chain_map_collect() {
    use volar_compiler::{parse_source, IterChainSource, IterStep, IterTerminal, IterMethod};

    let source = r#"
        fn test() {
            let x = items.iter().map(|i| i * 2).collect();
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let f = &module.functions[0];

    if let IrStmt::Let { init: Some(IrExpr::IterPipeline(chain)), .. } = &f.body.stmts[0] {
        assert!(matches!(&chain.source, IterChainSource::Method { method: IterMethod::Iter, .. }));
        assert_eq!(chain.steps.len(), 1);
        match &chain.steps[0] {
            IterStep::Map { var, .. } => assert_eq!(var, "i"),
            other => panic!("Expected Map step, got {:?}", other),
        }
        assert!(matches!(&chain.terminal, IterTerminal::Collect));
    } else {
        panic!("Expected IterPipeline");
    }
}

#[test]
fn test_non_iterator_map_is_raw_map() {
    use volar_compiler::parse_source;

    let source = r#"
        fn test() {
            let x = arr.map(|a| a + 1);
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let f = &module.functions[0];

    if let IrStmt::Let { init: Some(expr), .. } = &f.body.stmts[0] {
        // No .iter() → not an iter chain → should be RawMap
        assert!(matches!(expr, IrExpr::RawMap { .. }),
            "arr.map() without .iter() should be RawMap, got {:?}", expr);
    } else {
        panic!("Expected Let statement");
    }
}

#[test]
fn test_iter_chain_range_fold() {
    use volar_compiler::{parse_source, IterChainSource, IterTerminal};

    let source = r#"
        fn test() {
            let x = (0..n).fold(init, |acc, i| acc + i);
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let f = &module.functions[0];

    if let IrStmt::Let { init: Some(IrExpr::IterPipeline(chain)), .. } = &f.body.stmts[0] {
        assert!(matches!(&chain.source, IterChainSource::Range { inclusive: false, .. }));
        assert!(chain.steps.is_empty());
        assert!(matches!(&chain.terminal, IterTerminal::Fold { .. }));
    } else {
        panic!("Expected IterPipeline with Range source");
    }
}

#[test]
fn test_iter_chain_for_loop_uses_lazy_chain() {
    use volar_compiler::{parse_source, IterChainSource, IterStep, IterTerminal, IterMethod};

    let source = r#"
        fn test() {
            for (i, x) in arr.iter().enumerate() {
                process(i, x);
            };
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let f = &module.functions[0];

    // Should be IterLoop with collection = IterPipeline(Lazy chain)
    if let IrStmt::Semi(IrExpr::IterLoop { collection, .. }) = &f.body.stmts[0] {
        if let IrExpr::IterPipeline(chain) = collection.as_ref() {
            assert!(matches!(&chain.source, IterChainSource::Method { method: IterMethod::Iter, .. }));
            assert_eq!(chain.steps.len(), 1);
            assert!(matches!(&chain.steps[0], IterStep::Enumerate));
            assert!(matches!(&chain.terminal, IterTerminal::Lazy));
        } else {
            panic!("Expected IterPipeline as for-loop collection, got {:?}", collection);
        }
    } else {
        panic!("Expected IterLoop statement");
    }
}

#[test]
fn test_iter_chain_printer_round_trip() {
    use volar_compiler::{parse_source, print_module};

    let source = r#"
        fn test() {
            let a = items.iter().map(|x| x + 1).collect();
            let b = arr.iter().enumerate().filter_map(|a| check(a)).fold(init, |acc, x| acc + x);
            let c = (0..n).fold(start, |a, i| a + i);
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let output = print_module(&module);

    // Verify the printed output contains expected method chains
    assert!(output.contains(".iter().map(|x|"), "Should print iter().map(): {}", output);
    assert!(output.contains(".collect::<Vec<_>>()"), "Should print collect: {}", output);
    assert!(output.contains(".enumerate().filter_map(|a|"), "Should print enumerate().filter_map(): {}", output);
    assert!(output.contains(".fold("), "Should print fold: {}", output);
}
