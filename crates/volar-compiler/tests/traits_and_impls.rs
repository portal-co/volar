//! Tests for custom traits, implementations, and the new const_analysis +
//! TypeContext infrastructure.

use std::fs;
use std::path::Path;
use volar_compiler::{
    AssociatedType, ConstAnalysis, DisplayRust, ExprWriter, GenericKind, IrGenericParamKind,
    IrImpl, IrImplItem, IrMethodSig, IrTrait, IrTraitItem, IrTraitRef, IrType, MathTrait,
    ModuleWriter, PrimitiveType, RustBackend, SpecBinOp, SpecUnaryOp, StructKind, TraitKind,
    TypeContext, TypeWriter, builtin_trait_defs, parse_source, parse_sources,
};

// ============================================================================
// Phase 1: const_analysis tests
// ============================================================================

#[test]
fn test_classify_struct_generics() {
    let source = r#"
        struct Foo<N: ArrayLength<u8>, T: Clone, B: BlockEncrypt> {
            data: GenericArray<T, N>,
            cipher: B,
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    let generics = analysis.struct_generics.get("Foo").unwrap();
    assert_eq!(generics.len(), 3);
    assert_eq!(generics[0], ("N".into(), GenericKind::Length, IrGenericParamKind::Type));
    assert_eq!(generics[1], ("T".into(), GenericKind::Type, IrGenericParamKind::Type));
    assert_eq!(generics[2], ("B".into(), GenericKind::Type, IrGenericParamKind::Type));
}

#[test]
fn test_classify_trait_assoc_type_as_length() {
    let source = r#"
        pub trait LengthDoubler {
            type OutputSize: ArrayLength<u8>;
            fn double(a: GenericArray<u8, Self::OutputSize>) -> [GenericArray<u8, Self::OutputSize>; 2];
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    // OutputSize is bounded by ArrayLength<u8>, so it's a type-level constant
    assert!(
        analysis.is_type_level_const("LengthDoubler", "OutputSize"),
        "OutputSize should be classified as a type-level constant"
    );
}

#[test]
fn test_classify_trait_assoc_type_as_type() {
    let source = r#"
        pub trait MyTrait {
            type Output;
            fn compute(self) -> Self::Output;
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    // Output has no length-related bounds, so it's a regular type
    assert!(
        !analysis.is_type_level_const("MyTrait", "Output"),
        "Output should NOT be classified as a type-level constant"
    );
}

#[test]
fn test_classify_impl_assoc_inherits_from_trait() {
    let source = r#"
        pub trait LengthDoubler {
            type OutputSize: ArrayLength<u8>;
        }
        pub struct MyDoubler {}
        impl LengthDoubler for MyDoubler {
            type OutputSize = U32;
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    // The impl's OutputSize should inherit Length classification from the trait
    assert!(
        analysis.is_type_level_const("LengthDoubler", "OutputSize"),
        "Trait-level OutputSize should be Length"
    );
    // The impl_assoc_kinds should also have it
    assert!(
        analysis.is_impl_type_level_const("MyDoubler", "OutputSize"),
        "Impl-level OutputSize should inherit Length classification"
    );
}

#[test]
fn test_classify_add_output_is_type() {
    // Add::Output has no length bounds — it's a regular associated type
    let source = r#"
        struct Foo {}
        impl Add<Foo> for Foo {
            type Output = Foo;
            fn add(self, rhs: Foo) -> Foo { Foo {} }
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    // Add is a built-in trait — its Output is a regular type, not a length
    assert!(
        !analysis.is_impl_type_level_const("Foo", "Output"),
        "Add::Output should NOT be a type-level constant"
    );
}

// ============================================================================
// Phase 2: Associated types Display + printer round-trip
// ============================================================================

#[test]
fn test_associated_type_display() {
    assert_eq!(format!("{}", AssociatedType::Output), "Output");
    assert_eq!(format!("{}", AssociatedType::Key), "Key");
    assert_eq!(format!("{}", AssociatedType::BlockSize), "BlockSize");
    assert_eq!(format!("{}", AssociatedType::OutputSize), "OutputSize");
    assert_eq!(
        format!("{}", AssociatedType::Other("Foo".into())),
        "Foo"
    );
}

#[test]
fn test_printer_trait_associated_types_with_bounds() {
    let source = r#"
        pub trait LengthDoubler {
            type OutputSize: ArrayLength<u8>;
            fn double(a: u8) -> u8;
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let printed = volar_compiler::print_module(&module);

    // Should contain proper associated type with bounds, not Debug format
    assert!(
        printed.contains("type OutputSize"),
        "Printer should emit `type OutputSize`, got:\n{}",
        printed
    );
    assert!(
        !printed.contains("Other("),
        "Printer should NOT emit Debug format for associated types"
    );
}

#[test]
fn test_printer_impl_associated_type() {
    let source = r#"
        struct Foo {}
        impl Add for Foo {
            type Output = Foo;
            fn add(self, rhs: Foo) -> Foo { Foo {} }
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let printed = volar_compiler::print_module(&module);

    // Should contain `type Output = Foo;`
    assert!(
        printed.contains("type Output = Foo"),
        "Printer should emit `type Output = Foo`, got:\n{}",
        printed
    );
}

// ============================================================================
// Phase 2: SpecBinOp ↔ MathTrait mappings
// ============================================================================

#[test]
fn test_binop_to_math_trait_roundtrip() {
    let ops = [
        (SpecBinOp::Add, MathTrait::Add),
        (SpecBinOp::Sub, MathTrait::Sub),
        (SpecBinOp::Mul, MathTrait::Mul),
        (SpecBinOp::Div, MathTrait::Div),
        (SpecBinOp::Rem, MathTrait::Rem),
        (SpecBinOp::BitAnd, MathTrait::BitAnd),
        (SpecBinOp::BitOr, MathTrait::BitOr),
        (SpecBinOp::BitXor, MathTrait::BitXor),
        (SpecBinOp::Shl, MathTrait::Shl),
        (SpecBinOp::Shr, MathTrait::Shr),
    ];
    for (op, math) in &ops {
        assert_eq!(
            op.to_math_trait(),
            Some(*math),
            "{:?} should map to {:?}",
            op,
            math
        );
        assert_eq!(
            math.to_bin_op(),
            Some(*op),
            "{:?} should map back to {:?}",
            math,
            op
        );
    }
}

#[test]
fn test_unary_op_to_math_trait() {
    assert_eq!(SpecUnaryOp::Neg.to_math_trait(), Some(MathTrait::Neg));
    assert_eq!(SpecUnaryOp::Not.to_math_trait(), Some(MathTrait::Not));
    assert_eq!(SpecUnaryOp::Deref.to_math_trait(), None);
}

#[test]
fn test_comparison_ops_map_to_traits() {
    assert_eq!(SpecBinOp::Eq.to_math_trait(), Some(MathTrait::PartialEq));
    assert_eq!(SpecBinOp::Ne.to_math_trait(), Some(MathTrait::PartialEq));
    assert_eq!(SpecBinOp::Lt.to_math_trait(), Some(MathTrait::PartialOrd));
    assert_eq!(SpecBinOp::Le.to_math_trait(), Some(MathTrait::PartialOrd));
    assert_eq!(SpecBinOp::Gt.to_math_trait(), Some(MathTrait::PartialOrd));
    assert_eq!(SpecBinOp::Ge.to_math_trait(), Some(MathTrait::PartialOrd));
}

#[test]
fn test_logical_ops_have_no_trait() {
    assert_eq!(SpecBinOp::And.to_math_trait(), None);
    assert_eq!(SpecBinOp::Or.to_math_trait(), None);
}

// ============================================================================
// Phase 2: Built-in trait definitions
// ============================================================================

#[test]
fn test_builtin_add_trait_structure() {
    let builtins = builtin_trait_defs();
    let add = builtins
        .iter()
        .find(|t| t.kind == TraitKind::Math(MathTrait::Add))
        .expect("Add trait should exist in builtins");

    // Should have one generic param: Rhs with default Self
    assert_eq!(add.generics.len(), 1);
    assert_eq!(add.generics[0].name, "Rhs");
    assert_eq!(
        add.generics[0].default,
        Some(IrType::TypeParam("Self".into()))
    );

    // Should have 2 items: Output associated type + add method
    assert_eq!(add.items.len(), 2);

    // First item: associated type Output
    match &add.items[0] {
        IrTraitItem::AssociatedType { name, .. } => {
            assert_eq!(*name, AssociatedType::Output);
        }
        other => panic!("Expected AssociatedType, got {:?}", other),
    }

    // Second item: method add(self, rhs: Rhs) -> Self::Output
    match &add.items[1] {
        IrTraitItem::Method(sig) => {
            assert_eq!(sig.name, "add");
            assert_eq!(sig.receiver, Some(volar_compiler::IrReceiver::Value));
            assert_eq!(sig.params.len(), 1);
            assert_eq!(sig.params[0].name, "rhs");
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

#[test]
fn test_builtin_clone_trait_structure() {
    let builtins = builtin_trait_defs();
    let clone = builtins
        .iter()
        .find(|t| t.kind == TraitKind::Math(MathTrait::Clone))
        .expect("Clone trait should exist in builtins");

    assert!(clone.generics.is_empty());
    assert_eq!(clone.items.len(), 1);

    match &clone.items[0] {
        IrTraitItem::Method(sig) => {
            assert_eq!(sig.name, "clone");
            assert_eq!(sig.receiver, Some(volar_compiler::IrReceiver::Ref));
            assert!(sig.params.is_empty());
            assert_eq!(
                sig.return_type,
                Some(IrType::TypeParam("Self".into()))
            );
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

#[test]
fn test_builtin_default_has_no_receiver() {
    let builtins = builtin_trait_defs();
    let default = builtins
        .iter()
        .find(|t| t.kind == TraitKind::Math(MathTrait::Default))
        .expect("Default trait should exist in builtins");

    match &default.items[0] {
        IrTraitItem::Method(sig) => {
            assert_eq!(sig.name, "default");
            assert_eq!(sig.receiver, None, "Default::default() is a static method");
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

#[test]
fn test_builtin_copy_is_marker_with_clone_supertrait() {
    let builtins = builtin_trait_defs();
    let copy = builtins
        .iter()
        .find(|t| t.kind == TraitKind::Math(MathTrait::Copy))
        .expect("Copy trait should exist in builtins");

    assert!(copy.items.is_empty(), "Copy is a marker trait");
    assert_eq!(copy.super_traits.len(), 1);
    assert_eq!(
        copy.super_traits[0].trait_kind,
        TraitKind::Math(MathTrait::Clone)
    );
}

#[test]
fn test_builtin_neg_is_unary() {
    let builtins = builtin_trait_defs();
    let neg = builtins
        .iter()
        .find(|t| t.kind == TraitKind::Math(MathTrait::Neg))
        .expect("Neg trait should exist in builtins");

    // Unary: no Rhs generic
    assert!(neg.generics.is_empty());
    assert_eq!(neg.items.len(), 2); // Output + neg method

    match &neg.items[1] {
        IrTraitItem::Method(sig) => {
            assert_eq!(sig.name, "neg");
            assert!(sig.params.is_empty(), "Neg::neg takes only self");
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

// ============================================================================
// Phase 3: TypeContext with trait registry
// ============================================================================

#[test]
fn test_type_context_has_builtins() {
    let module = volar_compiler::IrModule::default();
    let ctx = TypeContext::from_module(&module);

    // Should have all built-in traits registered
    assert!(
        ctx.lookup_trait(&TraitKind::Math(MathTrait::Add)).is_some(),
        "TypeContext should have Add"
    );
    assert!(
        ctx.lookup_trait(&TraitKind::Math(MathTrait::Clone)).is_some(),
        "TypeContext should have Clone"
    );
    assert!(
        ctx.lookup_trait(&TraitKind::Custom("Digest".into()))
            .is_some(),
        "TypeContext should have Digest"
    );
}

#[test]
fn test_type_context_custom_trait_overrides_builtin() {
    let source = r#"
        pub trait LengthDoubler {
            type OutputSize: ArrayLength<u8>;
            fn double(a: u8) -> u8;
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let ctx = TypeContext::from_module(&module);

    // Custom trait should be findable
    let ld = ctx
        .lookup_trait(&TraitKind::Custom("LengthDoubler".into()))
        .expect("LengthDoubler should be in TypeContext");

    assert_eq!(ld.items.len(), 2); // OutputSize + double
}

#[test]
fn test_type_context_lookup_trait_items() {
    let module = volar_compiler::IrModule::default();
    let ctx = TypeContext::from_module(&module);

    let items = ctx
        .trait_items(&TraitKind::Math(MathTrait::Add))
        .expect("Add should have items");

    assert_eq!(items.len(), 2); // Output + add method
}

#[test]
fn test_validate_impl_missing_associated_type() {
    let source = r#"
        pub trait MyTrait {
            type Output;
            fn compute(self) -> u8;
        }
        struct Foo {}
        impl MyTrait for Foo {
            fn compute(self) -> u8 { 42 }
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let ctx = TypeContext::from_module(&module);

    let errors = ctx.validate_impl(&module.impls[0]);
    assert!(
        !errors.is_empty(),
        "Should detect missing associated type Output"
    );
    assert!(
        errors[0].contains("Output"),
        "Error should mention Output, got: {}",
        errors[0]
    );
}

#[test]
fn test_validate_impl_missing_method() {
    let source = r#"
        pub trait MyTrait {
            type Output;
            fn compute(self) -> u8;
        }
        struct Foo {}
        impl MyTrait for Foo {
            type Output = u8;
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let ctx = TypeContext::from_module(&module);

    let errors = ctx.validate_impl(&module.impls[0]);
    assert!(
        !errors.is_empty(),
        "Should detect missing method compute"
    );
    assert!(
        errors[0].contains("compute"),
        "Error should mention compute, got: {}",
        errors[0]
    );
}

#[test]
fn test_validate_impl_complete() {
    let source = r#"
        pub trait MyTrait {
            type Output;
            fn compute(self) -> u8;
        }
        struct Foo {}
        impl MyTrait for Foo {
            type Output = u8;
            fn compute(self) -> u8 { 42 }
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let ctx = TypeContext::from_module(&module);

    let errors = ctx.validate_impl(&module.impls[0]);
    assert!(
        errors.is_empty(),
        "Complete impl should have no errors, got: {:?}",
        errors
    );
}

// ============================================================================
// Feature 3: Methods — generic, static, receiver variants
// ============================================================================

#[test]
fn test_trait_with_generic_method() {
    let source = r#"
        pub trait Remappable {
            fn remap<M, F: FnMut(usize) -> usize>(&self, f: F) -> Self;
        }
    "#;
    let module = parse_source(source, "test").unwrap();

    assert_eq!(module.traits.len(), 1);
    let t = &module.traits[0];

    match &t.items[0] {
        IrTraitItem::Method(sig) => {
            assert_eq!(sig.name, "remap");
            assert_eq!(
                sig.receiver,
                Some(volar_compiler::IrReceiver::Ref)
            );
            assert_eq!(sig.generics.len(), 2, "Should have M and F generics");
            assert_eq!(sig.generics[0].name, "M");
            assert_eq!(sig.generics[1].name, "F");
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

#[test]
fn test_trait_with_static_method() {
    let source = r#"
        pub trait LengthDoubler {
            fn double(a: u8) -> [u8; 2];
        }
    "#;
    let module = parse_source(source, "test").unwrap();

    let t = &module.traits[0];
    match &t.items[0] {
        IrTraitItem::Method(sig) => {
            assert_eq!(sig.name, "double");
            assert_eq!(sig.receiver, None, "double should be a static method");
            assert_eq!(sig.params.len(), 1);
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

#[test]
fn test_impl_method_with_where_clause() {
    let source = r#"
        struct Foo<T> { val: T }
        impl<T> Foo<T> {
            fn transform<U>(self, f: U) -> Foo<U>
            where
                U: Clone,
            {
                Foo { val: f }
            }
        }
    "#;
    let module = parse_source(source, "test").unwrap();

    let imp = &module.impls[0];
    match &imp.items[0] {
        IrImplItem::Method(f) => {
            assert_eq!(f.name, "transform");
            assert_eq!(f.generics.len(), 1);
            assert_eq!(f.generics[0].name, "U");
            assert!(!f.where_clause.is_empty(), "Should have where clause");
        }
        other => panic!("Expected Method, got {:?}", other),
    }
}

// ============================================================================
// Feature 4: Trait-level generics
// ============================================================================

#[test]
fn test_trait_with_generic_param_and_default() {
    let source = r#"
        pub trait Add<Rhs = Self> {
            type Output;
            fn add(self, rhs: Rhs) -> Self::Output;
        }
    "#;
    let module = parse_source(source, "test").unwrap();

    let t = &module.traits[0];
    assert_eq!(t.generics.len(), 1);
    assert_eq!(t.generics[0].name, "Rhs");
    assert_eq!(
        t.generics[0].default,
        Some(IrType::TypeParam("Self".into()))
    );
}

#[test]
fn test_trait_with_supertrait() {
    let source = r#"
        pub trait PuncturableLengthDoubler: LengthDoubler {}
    "#;
    let module = parse_source(source, "test").unwrap();

    let t = &module.traits[0];
    assert_eq!(t.super_traits.len(), 1);
    assert_eq!(
        t.super_traits[0].trait_kind,
        TraitKind::Custom("LengthDoubler".into())
    );
}

#[test]
fn test_trait_with_generic_supertrait() {
    let source = r#"
        pub trait VoleArray<T>: ArrayLength<T> {}
    "#;
    let module = parse_source(source, "test").unwrap();

    let t = &module.traits[0];
    assert_eq!(t.generics.len(), 1);
    assert_eq!(t.generics[0].name, "T");
    assert_eq!(t.super_traits.len(), 1);
    // ArrayLength is recognized as Custom trait
    assert_eq!(
        t.super_traits[0].trait_kind,
        TraitKind::Custom("ArrayLength".into())
    );
    // With type arg T
    assert_eq!(t.super_traits[0].type_args.len(), 1);
}

// ============================================================================
// Feature 5: Custom trait definitions via TypeContext
// ============================================================================

#[test]
fn test_custom_trait_full_definition() {
    let source = r#"
        pub trait LengthDoubler {
            type OutputSize: ArrayLength<u8>;
            fn double(a: GenericArray<u8, Self::OutputSize>) -> [GenericArray<u8, Self::OutputSize>; 2];
        }
        pub trait PuncturableLengthDoubler: LengthDoubler {}
    "#;
    let module = parse_source(source, "test").unwrap();
    let ctx = TypeContext::from_module(&module);

    // LengthDoubler
    let ld = ctx
        .lookup_trait(&TraitKind::Custom("LengthDoubler".into()))
        .expect("LengthDoubler should be registered");
    assert_eq!(ld.items.len(), 2); // OutputSize + double
    assert!(ld.generics.is_empty());

    // PuncturableLengthDoubler
    let pld = ctx
        .lookup_trait(&TraitKind::Custom("PuncturableLengthDoubler".into()))
        .expect("PuncturableLengthDoubler should be registered");
    assert!(pld.items.is_empty());
    assert_eq!(pld.super_traits.len(), 1);
}

// ============================================================================
// Feature 6: Built-in trait special treatment — integration
// ============================================================================

#[test]
fn test_add_impl_resolved_through_type_context() {
    let source = r#"
        struct Foo { val: u8 }
        impl Add for Foo {
            type Output = Foo;
            fn add(self, rhs: Foo) -> Foo {
                Foo { val: self.val + rhs.val }
            }
        }
    "#;
    let module = parse_source(source, "test").unwrap();
    let ctx = TypeContext::from_module(&module);

    // The builtin Add definition should exist
    let add_def = ctx
        .lookup_trait(&TraitKind::Math(MathTrait::Add))
        .expect("Add should exist");

    // It should have the Output associated type and add method
    assert_eq!(add_def.items.len(), 2);

    // The impl should validate successfully
    let errors = ctx.validate_impl(&module.impls[0]);
    assert!(
        errors.is_empty(),
        "Add impl should be complete, got: {:?}",
        errors
    );
}

#[test]
fn test_math_trait_method_names() {
    assert_eq!(MathTrait::Add.method_name(), Some("add"));
    assert_eq!(MathTrait::Sub.method_name(), Some("sub"));
    assert_eq!(MathTrait::Mul.method_name(), Some("mul"));
    assert_eq!(MathTrait::BitXor.method_name(), Some("bitxor"));
    assert_eq!(MathTrait::Neg.method_name(), Some("neg"));
    assert_eq!(MathTrait::Clone.method_name(), Some("clone"));
    assert_eq!(MathTrait::Default.method_name(), Some("default"));
    assert_eq!(MathTrait::PartialEq.method_name(), Some("eq"));
}

// ============================================================================
// Integration: parse volar-spec and verify trait infrastructure
// ============================================================================

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
fn test_volar_spec_length_doubler_is_custom_trait() {
    let sources = read_volar_spec_sources();
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(c, n)| (c.as_str(), n.as_str()))
        .collect();
    let module = parse_sources(&sources_ref, "volar_spec").unwrap();

    let ld = module
        .traits
        .iter()
        .find(|t| matches!(&t.kind, TraitKind::Custom(n) if n == "LengthDoubler"));
    assert!(
        ld.is_some(),
        "LengthDoubler should be parsed as a custom trait"
    );

    let ld = ld.unwrap();
    // Should have OutputSize associated type and double method
    let has_output_size = ld.items.iter().any(|item| {
        matches!(item, IrTraitItem::AssociatedType { name, .. } if *name == AssociatedType::OutputSize)
    });
    assert!(has_output_size, "LengthDoubler should have OutputSize");

    let has_double = ld.items.iter().any(|item| {
        matches!(item, IrTraitItem::Method(sig) if sig.name == "double")
    });
    assert!(has_double, "LengthDoubler should have double method");
}

#[test]
fn test_volar_spec_length_doubler_output_size_is_const() {
    let sources = read_volar_spec_sources();
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(c, n)| (c.as_str(), n.as_str()))
        .collect();
    let module = parse_sources(&sources_ref, "volar_spec").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    assert!(
        analysis.is_type_level_const("LengthDoubler", "OutputSize"),
        "LengthDoubler::OutputSize should be a type-level constant"
    );
}

#[test]
fn test_volar_spec_add_impls_have_output() {
    let sources = read_volar_spec_sources();
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(c, n)| (c.as_str(), n.as_str()))
        .collect();
    let module = parse_sources(&sources_ref, "volar_spec").unwrap();
    let ctx = TypeContext::from_module(&module);

    // Find all Add impls
    let add_impls: Vec<_> = module
        .impls
        .iter()
        .filter(|imp| {
            imp.trait_
                .as_ref()
                .map(|tr| matches!(&tr.kind, TraitKind::Math(MathTrait::Add)))
                .unwrap_or(false)
        })
        .collect();

    assert!(
        !add_impls.is_empty(),
        "Should have at least one Add impl in volar-spec"
    );

    // Each should have an Output associated type
    for imp in &add_impls {
        let has_output = imp.items.iter().any(|item| {
            matches!(item, IrImplItem::AssociatedType { name, .. } if *name == AssociatedType::Output)
        });
        assert!(
            has_output,
            "Add impl for {} should have Output",
            imp.self_ty
        );
    }
}

#[test]
fn test_volar_spec_type_context_validates_impls() {
    let sources = read_volar_spec_sources();
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(c, n)| (c.as_str(), n.as_str()))
        .collect();
    let module = parse_sources(&sources_ref, "volar_spec").unwrap();
    let ctx = TypeContext::from_module(&module);

    // Validate all custom trait impls (not math/crypto builtins since those
    // may have trait items that are provided by default in the real Rust trait
    // but not modeled in our minimal builtin defs)
    let mut total_validated = 0;
    let mut total_errors = 0;
    for imp in &module.impls {
        if let Some(tr) = &imp.trait_ {
            if matches!(&tr.kind, TraitKind::Custom(_)) {
                let errors = ctx.validate_impl(imp);
                total_validated += 1;
                if !errors.is_empty() {
                    println!("Validation errors for impl {} for {}:", tr.kind, imp.self_ty);
                    for e in &errors {
                        println!("  - {}", e);
                    }
                    total_errors += errors.len();
                }
            }
        }
    }
    println!(
        "Validated {} custom trait impls, {} errors",
        total_validated, total_errors
    );
}

// ============================================================================
// RustBackend / DisplayRust tests
// ============================================================================

#[test]
fn test_display_rust_type_writer() {
    let ty = IrType::Primitive(PrimitiveType::U8);
    let s = format!("{}", DisplayRust(TypeWriter { ty: &ty }));
    assert_eq!(s, "u8");
}

#[test]
fn test_display_rust_generic_type() {
    let ty = IrType::Struct {
        kind: StructKind::Custom("Vec".into()),
        type_args: vec![IrType::Primitive(PrimitiveType::U8)],
    };
    let s = format!("{}", DisplayRust(TypeWriter { ty: &ty }));
    assert_eq!(s, "Vec<u8>");
}

#[test]
fn test_display_rust_module_writer() {
    let source = r#"
        pub struct Foo { pub x: u8 }
    "#;
    let module = parse_source(source, "test").unwrap();
    let s = format!("{}", DisplayRust(ModuleWriter { module: &module }));
    assert!(s.contains("pub struct Foo"));
    assert!(s.contains("pub x: u8"));
}

#[test]
fn test_display_rust_expr_writer() {
    let expr = volar_compiler::IrExpr::Lit(volar_compiler::IrLit::Int(42));
    let s = format!("{}", DisplayRust(ExprWriter { expr: &expr }));
    assert_eq!(s, "42");
}

#[test]
fn test_print_module_backward_compat() {
    let source = r#"
        pub struct Bar { pub y: bool }
    "#;
    let module = parse_source(source, "test").unwrap();
    let printed = volar_compiler::print_module(&module);
    // Should contain the preamble
    assert!(printed.contains("Auto-generated"));
    assert!(printed.contains("extern crate alloc"));
    // And the struct
    assert!(printed.contains("pub struct Bar"));
}

#[test]
fn test_module_writer_no_preamble() {
    let source = r#"
        pub struct Baz { pub z: u32 }
    "#;
    let module = parse_source(source, "test").unwrap();
    let s = format!("{}", DisplayRust(ModuleWriter { module: &module }));
    // Should NOT contain the preamble
    assert!(!s.contains("Auto-generated"));
    assert!(!s.contains("extern crate"));
    // But should contain the struct
    assert!(s.contains("pub struct Baz"));
}

#[test]
fn test_length_alias_discovery() {
    let source = r#"
        pub trait VoleArray<T>: ArrayLength<T> {}
        pub struct Foo<N: VoleArray<u8>> { pub data: GenericArray<u8, N> }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    // VoleArray should be discovered as a length alias
    assert!(
        analysis.length_alias_traits.contains(&"VoleArray".to_string()),
        "VoleArray should be a length alias, got: {:?}",
        analysis.length_alias_traits
    );

    // N: VoleArray<u8> should be classified as Length
    let generics = analysis.struct_generics.get("Foo").unwrap();
    assert_eq!(generics.len(), 1);
    assert_eq!(
        generics[0],
        ("N".into(), GenericKind::Length, IrGenericParamKind::Type),
        "N bound by VoleArray should be Length"
    );
}

#[test]
fn test_transitive_length_alias() {
    let source = r#"
        pub trait BigArray<T>: ArrayLength<T> {}
        pub trait HugeArray<T>: BigArray<T> {}
        pub struct Bar<N: HugeArray<u8>> { pub data: GenericArray<u8, N> }
    "#;
    let module = parse_source(source, "test").unwrap();
    let analysis = ConstAnalysis::from_module(&module);

    assert!(analysis.length_alias_traits.contains(&"BigArray".to_string()));
    assert!(analysis.length_alias_traits.contains(&"HugeArray".to_string()));

    let generics = analysis.struct_generics.get("Bar").unwrap();
    assert_eq!(
        generics[0],
        ("N".into(), GenericKind::Length, IrGenericParamKind::Type),
    );
}

#[test]
fn test_preamble_no_byte_block_encrypt() {
    let module = volar_compiler::IrModule::default();
    let printed = volar_compiler::print_module(&module);
    // ByteBlockEncrypt should NOT appear in the preamble anymore
    assert!(
        !printed.contains("ByteBlockEncrypt"),
        "Preamble should not contain ByteBlockEncrypt"
    );
    // volar_primitives types are imported for now (Bit, BitsInBytes, etc.)
    assert!(
        printed.contains("volar_primitives"),
        "Preamble should contain volar_primitives import for primitive types"
    );
}
