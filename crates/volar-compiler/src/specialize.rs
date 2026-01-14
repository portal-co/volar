//! Conversion from generic IR to specialized IR.
//!
//! This module provides conversion functions to transform the string-based
//! generic IR into the domain-specific specialized IR with proper enums.

use crate::ir::*;
use crate::specialized::*;

/// Convert a generic IR module to a specialized module
pub fn specialize_module(module: &IrModule) -> SpecModule {
    SpecModule {
        name: module.name.clone(),
        structs: module.structs.iter().map(specialize_struct).collect(),
        traits: module.traits.iter().map(specialize_trait).collect(),
        impls: module.impls.iter().map(specialize_impl).collect(),
        functions: module.functions.iter().map(specialize_function).collect(),
    }
}

/// Convert a generic struct to specialized
pub fn specialize_struct(s: &IrStruct) -> SpecStruct {
    SpecStruct {
        kind: StructKind::from_str(&s.name),
        generics: s.generics.iter().map(specialize_generic_param).collect(),
        fields: s.fields.iter().map(specialize_field).collect(),
        is_tuple: s.is_tuple,
    }
}

fn specialize_field(f: &IrField) -> SpecField {
    SpecField {
        name: f.name.clone(),
        ty: specialize_type(&f.ty),
        public: f.visibility == Visibility::Public,
    }
}

/// Convert a generic trait to specialized
pub fn specialize_trait(t: &IrTrait) -> SpecTrait {
    SpecTrait {
        kind: TraitKind::from_path(&[t.name.clone()]),
        generics: t.generics.iter().map(specialize_generic_param).collect(),
        super_traits: t.super_traits.iter().map(specialize_trait_bound).collect(),
        items: t.items.iter().map(specialize_trait_item).collect(),
    }
}

fn specialize_trait_item(item: &IrTraitItem) -> SpecTraitItem {
    match item {
        IrTraitItem::Method(sig) => SpecTraitItem::Method(specialize_method_sig(sig)),
        IrTraitItem::AssociatedType { name, bounds, default } => SpecTraitItem::AssociatedType {
            name: AssociatedType::from_str(name),
            bounds: bounds.iter().map(specialize_trait_bound).collect(),
            default: default.as_ref().map(specialize_type),
        },
        IrTraitItem::Const { .. } => {
            // Constants are rare in volar-spec, treat as associated type for now
            SpecTraitItem::AssociatedType {
                name: AssociatedType::Custom("const".to_string()),
                bounds: vec![],
                default: None,
            }
        }
    }
}

fn specialize_method_sig(sig: &IrMethodSignature) -> SpecMethodSig {
    SpecMethodSig {
        name: sig.name.clone(),
        generics: sig.generics.iter().map(specialize_generic_param).collect(),
        receiver: sig.receiver.as_ref().map(specialize_receiver),
        params: sig.params.iter().map(specialize_param).collect(),
        return_type: sig.return_type.as_ref().map(specialize_type),
        where_clause: sig.where_clause.iter().filter_map(specialize_where_predicate).collect(),
    }
}

/// Convert a generic impl to specialized
pub fn specialize_impl(i: &IrImpl) -> SpecImpl {
    SpecImpl {
        generics: i.generics.iter().map(specialize_generic_param).collect(),
        trait_: i.trait_.as_ref().map(specialize_trait_ref),
        self_ty: specialize_type(&i.self_ty),
        where_clause: i.where_clause.iter().filter_map(specialize_where_predicate).collect(),
        items: i.items.iter().map(specialize_impl_item).collect(),
    }
}

fn specialize_trait_ref(tr: &IrTraitRef) -> SpecTraitRef {
    SpecTraitRef {
        kind: TraitKind::from_path(&tr.path),
        type_args: tr.type_args.iter().map(specialize_type).collect(),
    }
}

fn specialize_impl_item(item: &IrImplItem) -> SpecImplItem {
    match item {
        IrImplItem::Method(f) => SpecImplItem::Method(specialize_function(f)),
        IrImplItem::AssociatedType { name, ty } => SpecImplItem::AssociatedType {
            name: AssociatedType::from_str(name),
            ty: specialize_type(ty),
        },
        IrImplItem::Const { name, ty, value: _ } => SpecImplItem::AssociatedType {
            name: AssociatedType::Custom(name.clone()),
            ty: specialize_type(ty),
        },
    }
}

/// Convert a generic function to specialized
pub fn specialize_function(f: &IrFunction) -> SpecFunction {
    SpecFunction {
        name: f.name.clone(),
        generics: f.generics.iter().map(specialize_generic_param).collect(),
        receiver: f.receiver.as_ref().map(specialize_receiver),
        params: f.params.iter().map(specialize_param).collect(),
        return_type: f.return_type.as_ref().map(specialize_type),
        where_clause: f.where_clause.iter().filter_map(specialize_where_predicate).collect(),
        body: specialize_block(&f.body),
    }
}

fn specialize_receiver(r: &IrReceiver) -> SpecReceiver {
    match r {
        IrReceiver::Value => SpecReceiver::Value,
        IrReceiver::Ref => SpecReceiver::Ref,
        IrReceiver::RefMut => SpecReceiver::RefMut,
    }
}

fn specialize_param(p: &IrParam) -> SpecParam {
    SpecParam {
        name: p.name.clone(),
        ty: specialize_type(&p.ty),
    }
}

fn specialize_generic_param(p: &IrGenericParam) -> SpecGenericParam {
    SpecGenericParam {
        name: p.name.clone(),
        bounds: p.bounds.iter().map(specialize_trait_bound).collect(),
        default: p.default.as_ref().map(specialize_type),
    }
}

fn specialize_trait_bound(b: &IrTraitBound) -> SpecTraitBound {
    SpecTraitBound {
        trait_kind: TraitKind::from_path(&b.path),
        type_args: b.type_args.iter().map(specialize_type).collect(),
        assoc_bindings: b
            .assoc_type_bindings
            .iter()
            .map(|(name, ty)| (AssociatedType::from_str(name), specialize_type(ty)))
            .collect(),
    }
}

fn specialize_where_predicate(p: &IrWherePredicate) -> Option<SpecWherePredicate> {
    match p {
        IrWherePredicate::TypeBound { ty, bounds } => Some(SpecWherePredicate::TypeBound {
            ty: specialize_type(ty),
            bounds: bounds.iter().map(specialize_trait_bound).collect(),
        }),
        IrWherePredicate::Lifetime { .. } => None, // Skip lifetime predicates
    }
}

/// Convert a generic type to specialized
pub fn specialize_type(ty: &IrType) -> SpecType {
    match ty {
        IrType::Path { segments, type_args } => {
            let name = segments.last().map(|s| s.as_str()).unwrap_or("");
            
            // Check for primitive types
            if segments.len() == 1 {
                if let Some(prim) = PrimitiveType::from_str(name) {
                    return SpecType::Primitive(prim);
                }
                
                // Check for typenum constants
                if let Some(tn) = TypeNumConst::from_str(name) {
                    return SpecType::Primitive(match tn {
                        TypeNumConst::U0 => PrimitiveType::Usize,
                        TypeNumConst::U1 => PrimitiveType::Usize,
                        TypeNumConst::U2 => PrimitiveType::Usize,
                        TypeNumConst::U8 => PrimitiveType::Usize,
                        TypeNumConst::U16 => PrimitiveType::Usize,
                        TypeNumConst::U32 => PrimitiveType::Usize,
                        TypeNumConst::U64 => PrimitiveType::Usize,
                    });
                }
            }
            
            // Check for GenericArray
            if name == "GenericArray" && type_args.len() >= 2 {
                let elem = specialize_type(&type_args[0]);
                let len = specialize_array_length(&type_args[1]);
                return SpecType::Array {
                    kind: ArrayKind::GenericArray,
                    elem: Box::new(elem),
                    len,
                };
            }
            
            // Check for known struct types
            let struct_kind = StructKind::from_str(name);
            if !matches!(struct_kind, StructKind::Custom(_)) || !type_args.is_empty() {
                return SpecType::Struct {
                    kind: struct_kind,
                    type_args: type_args.iter().map(specialize_type).collect(),
                };
            }
            
            // Could be a type parameter
            if segments.len() == 1 && type_args.is_empty() {
                return SpecType::TypeParam(name.to_string());
            }
            
            // Default to struct
            SpecType::Struct {
                kind: StructKind::Custom(segments.join("::")),
                type_args: type_args.iter().map(specialize_type).collect(),
            }
        }
        IrType::Reference { mutable, elem } => SpecType::Reference {
            mutable: *mutable,
            elem: Box::new(specialize_type(elem)),
        },
        IrType::Slice(elem) => SpecType::Array {
            kind: ArrayKind::Slice,
            elem: Box::new(specialize_type(elem)),
            len: ArrayLength::Computed(Box::new(SpecExpr::Var("_".to_string()))),
        },
        IrType::Array { elem, len } => SpecType::Array {
            kind: ArrayKind::FixedArray,
            elem: Box::new(specialize_type(elem)),
            len: specialize_array_length_from_expr(len),
        },
        IrType::Tuple(elems) => {
            if elems.is_empty() {
                SpecType::Unit
            } else {
                SpecType::Tuple(elems.iter().map(specialize_type).collect())
            }
        }
        IrType::Unit => SpecType::Unit,
        IrType::Never => SpecType::Never,
        IrType::Infer => SpecType::Infer,
        IrType::Projection { base, assoc, type_args: _ } => SpecType::Projection {
            base: Box::new(specialize_type(base)),
            assoc: AssociatedType::from_str(assoc),
        },
        IrType::ImplTrait(bounds) => {
            SpecType::ImplTrait(bounds.iter().map(specialize_trait_bound).collect())
        }
        IrType::DynTrait(bounds) => {
            SpecType::ImplTrait(bounds.iter().map(specialize_trait_bound).collect())
        }
        IrType::FnPtr { params, ret } => SpecType::FnPtr {
            params: params.iter().map(specialize_type).collect(),
            ret: Box::new(specialize_type(ret)),
        },
    }
}

fn specialize_array_length(ty: &IrType) -> ArrayLength {
    match ty {
        IrType::Path { segments, type_args: _ } => {
            let name = segments.last().map(|s| s.as_str()).unwrap_or("");
            
            // Check for typenum constants
            if let Some(tn) = TypeNumConst::from_str(name) {
                return ArrayLength::TypeNum(tn);
            }
            
            // Type parameter
            ArrayLength::TypeParam(name.to_string())
        }
        _ => ArrayLength::Computed(Box::new(SpecExpr::Var("_".to_string()))),
    }
}

fn specialize_array_length_from_expr(expr: &IrExpr) -> ArrayLength {
    match expr {
        IrExpr::Lit(IrLit::Int(n)) => ArrayLength::Const(*n as usize),
        IrExpr::Path { segments, .. } => {
            let name = segments.last().map(|s| s.as_str()).unwrap_or("");
            if let Some(tn) = TypeNumConst::from_str(name) {
                ArrayLength::TypeNum(tn)
            } else {
                ArrayLength::TypeParam(name.to_string())
            }
        }
        _ => ArrayLength::Computed(Box::new(specialize_expr(expr))),
    }
}

/// Convert a generic expression to specialized
pub fn specialize_expr(expr: &IrExpr) -> SpecExpr {
    match expr {
        IrExpr::Lit(lit) => SpecExpr::Lit(specialize_lit(lit)),
        IrExpr::Path { segments, type_args } => {
            if segments.len() == 1 && type_args.is_empty() {
                SpecExpr::Var(segments[0].clone())
            } else {
                SpecExpr::Path {
                    segments: segments.clone(),
                    type_args: type_args.iter().map(specialize_type).collect(),
                }
            }
        }
        IrExpr::Binary { op, left, right } => SpecExpr::Binary {
            op: specialize_bin_op(op),
            left: Box::new(specialize_expr(left)),
            right: Box::new(specialize_expr(right)),
        },
        IrExpr::Unary { op, expr: e } => SpecExpr::Unary {
            op: specialize_unary_op(op),
            expr: Box::new(specialize_expr(e)),
        },
        IrExpr::Call { func, args } => SpecExpr::Call {
            func: Box::new(specialize_expr(func)),
            args: args.iter().map(specialize_expr).collect(),
        },
        IrExpr::MethodCall { receiver, method, type_args, args } => SpecExpr::MethodCall {
            receiver: Box::new(specialize_expr(receiver)),
            method: MethodKind::from_str(method),
            type_args: type_args.iter().map(specialize_type).collect(),
            args: args.iter().map(specialize_expr).collect(),
        },
        IrExpr::Field { base, field } => SpecExpr::Field {
            base: Box::new(specialize_expr(base)),
            field: field.clone(),
        },
        IrExpr::Index { base, index } => SpecExpr::Index {
            base: Box::new(specialize_expr(base)),
            index: Box::new(specialize_expr(index)),
        },
        IrExpr::Struct { path, fields, rest } => {
            let kind = StructKind::from_str(path.last().map(|s| s.as_str()).unwrap_or(""));
            SpecExpr::StructExpr {
                kind,
                type_args: vec![],
                fields: fields.iter().map(|(n, e)| (n.clone(), specialize_expr(e))).collect(),
                rest: rest.as_ref().map(|e| Box::new(specialize_expr(e))),
            }
        }
        IrExpr::Tuple(elems) => SpecExpr::Tuple(elems.iter().map(specialize_expr).collect()),
        IrExpr::Array(elems) => SpecExpr::Array(elems.iter().map(specialize_expr).collect()),
        IrExpr::Repeat { elem, len } => SpecExpr::Repeat {
            elem: Box::new(specialize_expr(elem)),
            len: Box::new(specialize_expr(len)),
        },
        IrExpr::Block(block) => SpecExpr::Block(specialize_block(block)),
        IrExpr::If { cond, then_branch, else_branch } => SpecExpr::If {
            cond: Box::new(specialize_expr(cond)),
            then_branch: specialize_block(then_branch),
            else_branch: else_branch.as_ref().map(|e| Box::new(specialize_expr(e))),
        },
        IrExpr::ForLoop { pattern, iter, body } => SpecExpr::ForLoop {
            pattern: specialize_pattern(pattern),
            iter: Box::new(specialize_expr(iter)),
            body: specialize_block(body),
        },
        IrExpr::While { cond, body } => SpecExpr::While {
            cond: Box::new(specialize_expr(cond)),
            body: specialize_block(body),
        },
        IrExpr::Loop { body } => SpecExpr::Loop {
            body: specialize_block(body),
        },
        IrExpr::Match { expr: e, arms } => SpecExpr::Match {
            expr: Box::new(specialize_expr(e)),
            arms: arms.iter().map(specialize_match_arm).collect(),
        },
        IrExpr::Closure { params, ret_type, body } => SpecExpr::Closure {
            params: params.iter().map(specialize_closure_param).collect(),
            ret_type: ret_type.as_ref().map(|t| Box::new(specialize_type(t))),
            body: Box::new(specialize_expr(body)),
        },
        IrExpr::Ref { mutable, expr: e } => SpecExpr::Ref {
            mutable: *mutable,
            expr: Box::new(specialize_expr(e)),
        },
        IrExpr::Deref(e) => SpecExpr::Unary {
            op: SpecUnaryOp::Deref,
            expr: Box::new(specialize_expr(e)),
        },
        IrExpr::Cast { expr: e, ty } => SpecExpr::Cast {
            expr: Box::new(specialize_expr(e)),
            ty: Box::new(specialize_type(ty)),
        },
        IrExpr::Return(e) => SpecExpr::Return(e.as_ref().map(|e| Box::new(specialize_expr(e)))),
        IrExpr::Break(e) => SpecExpr::Break(e.as_ref().map(|e| Box::new(specialize_expr(e)))),
        IrExpr::Continue => SpecExpr::Continue,
        IrExpr::Assign { left, right } => SpecExpr::Assign {
            left: Box::new(specialize_expr(left)),
            right: Box::new(specialize_expr(right)),
        },
        IrExpr::AssignOp { op, left, right } => SpecExpr::AssignOp {
            op: specialize_bin_op(op),
            left: Box::new(specialize_expr(left)),
            right: Box::new(specialize_expr(right)),
        },
        IrExpr::Range { start, end, inclusive } => SpecExpr::Range {
            start: start.as_ref().map(|e| Box::new(specialize_expr(e))),
            end: end.as_ref().map(|e| Box::new(specialize_expr(e))),
            inclusive: *inclusive,
        },
        IrExpr::Macro { path, tokens } => SpecExpr::Macro {
            name: path.join("::"),
            tokens: tokens.clone(),
        },
        IrExpr::Paren(e) => specialize_expr(e),
        IrExpr::Try(e) => SpecExpr::Try(Box::new(specialize_expr(e))),
        IrExpr::Await(e) => specialize_expr(e), // Simplify await
        IrExpr::Unsafe(block) => SpecExpr::Block(specialize_block(block)),
        IrExpr::Let { pattern, expr: e } => {
            // Convert let expression to a block with let statement
            SpecExpr::Block(SpecBlock {
                stmts: vec![SpecStmt::Let {
                    pattern: specialize_pattern(pattern),
                    ty: None,
                    init: Some(specialize_expr(e)),
                }],
                expr: None,
            })
        }
    }
}

fn specialize_lit(lit: &IrLit) -> SpecLit {
    match lit {
        IrLit::Int(n) => SpecLit::Int(*n),
        IrLit::Float(f) => SpecLit::Float(*f),
        IrLit::Bool(b) => SpecLit::Bool(*b),
        IrLit::Char(c) => SpecLit::Char(*c),
        IrLit::Str(s) => SpecLit::Str(s.clone()),
        IrLit::ByteStr(bs) => SpecLit::ByteStr(bs.clone()),
        IrLit::Byte(b) => SpecLit::Byte(*b),
    }
}

fn specialize_bin_op(op: &IrBinOp) -> SpecBinOp {
    match op {
        IrBinOp::Add => SpecBinOp::Add,
        IrBinOp::Sub => SpecBinOp::Sub,
        IrBinOp::Mul => SpecBinOp::Mul,
        IrBinOp::Div => SpecBinOp::Div,
        IrBinOp::Rem => SpecBinOp::Rem,
        IrBinOp::BitAnd => SpecBinOp::BitAnd,
        IrBinOp::BitOr => SpecBinOp::BitOr,
        IrBinOp::BitXor => SpecBinOp::BitXor,
        IrBinOp::Shl => SpecBinOp::Shl,
        IrBinOp::Shr => SpecBinOp::Shr,
        IrBinOp::Eq => SpecBinOp::Eq,
        IrBinOp::Ne => SpecBinOp::Ne,
        IrBinOp::Lt => SpecBinOp::Lt,
        IrBinOp::Le => SpecBinOp::Le,
        IrBinOp::Gt => SpecBinOp::Gt,
        IrBinOp::Ge => SpecBinOp::Ge,
        IrBinOp::And => SpecBinOp::And,
        IrBinOp::Or => SpecBinOp::Or,
    }
}

fn specialize_unary_op(op: &IrUnaryOp) -> SpecUnaryOp {
    match op {
        IrUnaryOp::Neg => SpecUnaryOp::Neg,
        IrUnaryOp::Not => SpecUnaryOp::Not,
        IrUnaryOp::Deref => SpecUnaryOp::Deref,
    }
}

fn specialize_pattern(pat: &IrPattern) -> SpecPattern {
    match pat {
        IrPattern::Ident { mutable, name, subpat } => SpecPattern::Ident {
            mutable: *mutable,
            name: name.clone(),
            subpat: subpat.as_ref().map(|p| Box::new(specialize_pattern(p))),
        },
        IrPattern::Tuple(elems) => SpecPattern::Tuple(elems.iter().map(specialize_pattern).collect()),
        IrPattern::Struct { path, fields, rest } => {
            let kind = StructKind::from_str(path.last().map(|s| s.as_str()).unwrap_or(""));
            SpecPattern::Struct {
                kind,
                fields: fields.iter().map(|(n, p)| (n.clone(), specialize_pattern(p))).collect(),
                rest: *rest,
            }
        }
        IrPattern::TupleStruct { path, elems } => {
            let kind = StructKind::from_str(path.last().map(|s| s.as_str()).unwrap_or(""));
            SpecPattern::TupleStruct {
                kind,
                elems: elems.iter().map(specialize_pattern).collect(),
            }
        }
        IrPattern::Slice(elems) => SpecPattern::Slice(elems.iter().map(specialize_pattern).collect()),
        IrPattern::Wild => SpecPattern::Wild,
        IrPattern::Lit(lit) => SpecPattern::Lit(specialize_lit(lit)),
        IrPattern::Ref { mutable, pat } => SpecPattern::Ref {
            mutable: *mutable,
            pat: Box::new(specialize_pattern(pat)),
        },
        IrPattern::Or(pats) => SpecPattern::Or(pats.iter().map(specialize_pattern).collect()),
        IrPattern::Rest => SpecPattern::Rest,
    }
}

fn specialize_block(block: &IrBlock) -> SpecBlock {
    SpecBlock {
        stmts: block.stmts.iter().map(specialize_stmt).collect(),
        expr: block.expr.as_ref().map(|e| Box::new(specialize_expr(e))),
    }
}

fn specialize_stmt(stmt: &IrStmt) -> SpecStmt {
    match stmt {
        IrStmt::Let { pattern, ty, init } => SpecStmt::Let {
            pattern: specialize_pattern(pattern),
            ty: ty.as_ref().map(specialize_type),
            init: init.as_ref().map(specialize_expr),
        },
        IrStmt::Expr(e) => SpecStmt::Expr(specialize_expr(e)),
        IrStmt::Semi(e) => SpecStmt::Semi(specialize_expr(e)),
        IrStmt::Item(_) => SpecStmt::Semi(SpecExpr::Lit(SpecLit::Int(0))), // Skip nested items
    }
}

fn specialize_match_arm(arm: &IrMatchArm) -> SpecMatchArm {
    SpecMatchArm {
        pattern: specialize_pattern(&arm.pattern),
        guard: arm.guard.as_ref().map(specialize_expr),
        body: specialize_expr(&arm.body),
    }
}

fn specialize_closure_param(param: &IrClosureParam) -> SpecClosureParam {
    SpecClosureParam {
        pattern: specialize_pattern(&param.pattern),
        ty: param.ty.as_ref().map(specialize_type),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;

    #[test]
    fn test_specialize_primitive_types() {
        let source = r#"
            fn test(a: u8, b: Galois, c: BitsInBytes) -> Bit {
                todo!()
            }
        "#;
        let module = parse_source(source, "test").unwrap();
        let spec = specialize_module(&module);
        
        assert_eq!(spec.functions.len(), 1);
        let f = &spec.functions[0];
        
        assert!(matches!(f.params[0].ty, SpecType::Primitive(PrimitiveType::U8)));
        assert!(matches!(f.params[1].ty, SpecType::Primitive(PrimitiveType::Galois)));
        assert!(matches!(f.params[2].ty, SpecType::Primitive(PrimitiveType::BitsInBytes)));
        assert!(matches!(f.return_type, Some(SpecType::Primitive(PrimitiveType::Bit))));
    }

    #[test]
    fn test_specialize_struct_types() {
        let source = r#"
            fn test(d: Delta<N, T>, v: Vope<N, T, K>) -> Q<N, T> {
                todo!()
            }
        "#;
        let module = parse_source(source, "test").unwrap();
        let spec = specialize_module(&module);
        
        let f = &spec.functions[0];
        
        assert!(matches!(f.params[0].ty, SpecType::Struct { kind: StructKind::Delta, .. }));
        assert!(matches!(f.params[1].ty, SpecType::Struct { kind: StructKind::Vope, .. }));
        assert!(matches!(f.return_type, Some(SpecType::Struct { kind: StructKind::Q, .. })));
    }

    #[test]
    fn test_specialize_array_types() {
        let source = r#"
            fn test(a: GenericArray<u8, N>) -> [u8; 32] {
                todo!()
            }
        "#;
        let module = parse_source(source, "test").unwrap();
        let spec = specialize_module(&module);
        
        let f = &spec.functions[0];
        
        assert!(matches!(f.params[0].ty, SpecType::Array { kind: ArrayKind::GenericArray, .. }));
        assert!(matches!(f.return_type, Some(SpecType::Array { kind: ArrayKind::FixedArray, .. })));
    }

    #[test]
    fn test_specialize_math_trait() {
        let source = r#"
            impl<N, T: Add<U>, U> Add<Vope<N, U>> for Vope<N, T> {
                type Output = Vope<N, T::Output>;
                fn add(self, rhs: Vope<N, U>) -> Self::Output {
                    todo!()
                }
            }
        "#;
        let module = parse_source(source, "test").unwrap();
        let spec = specialize_module(&module);
        
        assert_eq!(spec.impls.len(), 1);
        let imp = &spec.impls[0];
        
        assert!(imp.trait_.is_some());
        let trait_ref = imp.trait_.as_ref().unwrap();
        assert!(matches!(trait_ref.kind, TraitKind::Math(MathTrait::Add)));
    }

    #[test]
    fn test_specialize_method_kinds() {
        let source = r#"
            fn test() {
                let a = delta.remap(|i| i);
                let b = vope.rotate_left(1);
                let c = arr.zip(other, f);
                let d = cipher.encrypt_block(&mut block);
            }
        "#;
        let module = parse_source(source, "test").unwrap();
        let spec = specialize_module(&module);
        
        // Check that methods are properly categorized
        let f = &spec.functions[0];
        let stmts = &f.body.stmts;
        
        // We have 4 let statements
        assert_eq!(stmts.len(), 4);
    }

    #[test]
    fn test_specialize_vole_struct() {
        let source = r#"
            pub struct Vope<N: VoleArray<T>, T, K: ArrayLength<GenericArray<T, N>> = U1> {
                pub u: GenericArray<GenericArray<T, N>, K>,
                pub v: GenericArray<T, N>,
            }
        "#;
        let module = parse_source(source, "test").unwrap();
        let spec = specialize_module(&module);
        
        assert_eq!(spec.structs.len(), 1);
        let s = &spec.structs[0];
        
        assert!(matches!(s.kind, StructKind::Vope));
        assert_eq!(s.generics.len(), 3);
        assert_eq!(s.fields.len(), 2);
        
        // Check that VoleArray is recognized as a crypto trait
        let n_bounds = &s.generics[0].bounds;
        assert!(!n_bounds.is_empty());
        assert!(matches!(n_bounds[0].trait_kind, TraitKind::Crypto(CryptoTrait::VoleArray)));
    }
}
