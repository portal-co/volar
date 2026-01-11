//! Lowering and transformation utilities for the IR.
//!
//! This module provides utilities for:
//! - Resolving generic type parameters
//! - Monomorphizing generic code
//! - Analyzing operator trait implementations
//! - Preparing the IR for code generation

use crate::ir::*;
use std::collections::{HashMap, HashSet};

/// A resolved type after generic substitution
#[derive(Debug, Clone)]
pub struct ResolvedType {
    pub base_name: String,
    pub type_args: Vec<ResolvedType>,
    pub is_reference: bool,
    pub is_mutable: bool,
}

/// Context for type resolution
#[derive(Debug, Default)]
pub struct TypeContext {
    /// Map from type parameter names to their concrete types
    pub substitutions: HashMap<String, IrType>,
    /// Known struct definitions
    pub structs: HashMap<String, IrStruct>,
    /// Known trait implementations
    pub trait_impls: HashMap<(String, String), Vec<IrImpl>>, // (trait_name, self_type) -> impls
    /// Associated type resolutions
    pub assoc_types: HashMap<(String, String), IrType>, // (type, assoc_name) -> resolved_type
}

impl TypeContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a type context from an IR module
    pub fn from_module(module: &IrModule) -> Self {
        let mut ctx = Self::new();

        // Register all structs
        for s in &module.structs {
            ctx.structs.insert(s.name.clone(), s.clone());
        }

        // Register all trait implementations
        for imp in &module.impls {
            if let Some(trait_ref) = &imp.trait_ {
                let trait_name = trait_ref.path.join("::");
                let self_ty = type_to_string(&imp.self_ty);
                ctx.trait_impls
                    .entry((trait_name, self_ty))
                    .or_insert_with(Vec::new)
                    .push(imp.clone());

                // Register associated types
                for item in &imp.items {
                    if let IrImplItem::AssociatedType { name, ty } = item {
                        let self_ty = type_to_string(&imp.self_ty);
                        ctx.assoc_types.insert((self_ty, name.clone()), ty.clone());
                    }
                }
            }
        }

        ctx
    }

    /// Substitute type parameters in a type
    pub fn substitute(&self, ty: &IrType) -> IrType {
        match ty {
            IrType::Path { segments, type_args } => {
                // Check if this is a type parameter we can substitute
                if segments.len() == 1 {
                    if let Some(subst) = self.substitutions.get(&segments[0]) {
                        return subst.clone();
                    }
                }

                IrType::Path {
                    segments: segments.clone(),
                    type_args: type_args.iter().map(|t| self.substitute(t)).collect(),
                }
            }
            IrType::Reference { mutable, elem } => IrType::Reference {
                mutable: *mutable,
                elem: Box::new(self.substitute(elem)),
            },
            IrType::Slice(elem) => IrType::Slice(Box::new(self.substitute(elem))),
            IrType::Array { elem, len } => IrType::Array {
                elem: Box::new(self.substitute(elem)),
                len: len.clone(),
            },
            IrType::Tuple(elems) => IrType::Tuple(elems.iter().map(|t| self.substitute(t)).collect()),
            IrType::Projection { base, assoc, type_args } => {
                let base_subst = self.substitute(base);
                let base_str = type_to_string(&base_subst);

                // Try to resolve the associated type
                if let Some(resolved) = self.assoc_types.get(&(base_str, assoc.clone())) {
                    return self.substitute(resolved);
                }

                IrType::Projection {
                    base: Box::new(base_subst),
                    assoc: assoc.clone(),
                    type_args: type_args.iter().map(|t| self.substitute(t)).collect(),
                }
            }
            _ => ty.clone(),
        }
    }

    /// Add a type substitution
    pub fn with_substitution(&self, name: &str, ty: IrType) -> Self {
        let mut new_ctx = Self {
            substitutions: self.substitutions.clone(),
            structs: self.structs.clone(),
            trait_impls: self.trait_impls.clone(),
            assoc_types: self.assoc_types.clone(),
        };
        new_ctx.substitutions.insert(name.to_string(), ty);
        new_ctx
    }
}

/// Convert a type to a string representation
pub fn type_to_string(ty: &IrType) -> String {
    match ty {
        IrType::Path { segments, type_args } => {
            let base = segments.join("::");
            if type_args.is_empty() {
                base
            } else {
                format!(
                    "{}<{}>",
                    base,
                    type_args
                        .iter()
                        .map(type_to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        IrType::Reference { mutable, elem } => {
            if *mutable {
                format!("&mut {}", type_to_string(elem))
            } else {
                format!("&{}", type_to_string(elem))
            }
        }
        IrType::Slice(elem) => format!("[{}]", type_to_string(elem)),
        IrType::Array { elem, len: _ } => format!("[{}; _]", type_to_string(elem)),
        IrType::Tuple(elems) => {
            format!(
                "({})",
                elems.iter().map(type_to_string).collect::<Vec<_>>().join(", ")
            )
        }
        IrType::Unit => "()".to_string(),
        IrType::Never => "!".to_string(),
        IrType::Infer => "_".to_string(),
        IrType::Projection { base, assoc, .. } => {
            format!("<{} as _>::{}", type_to_string(base), assoc)
        }
        _ => "?".to_string(),
    }
}

/// Analyze an IR module to extract operator implementations
#[derive(Debug, Default)]
pub struct OperatorAnalysis {
    /// Binary operator implementations: (trait_name, self_type, rhs_type) -> output_type
    pub binary_ops: HashMap<(String, String, String), IrType>,
    /// Unary operator implementations: (trait_name, self_type) -> output_type
    pub unary_ops: HashMap<(String, String), IrType>,
}

impl OperatorAnalysis {
    /// Analyze a module for operator implementations
    pub fn from_module(module: &IrModule) -> Self {
        let mut analysis = Self::default();

        let binary_traits = [
            "Add", "Sub", "Mul", "Div", "Rem", "BitAnd", "BitOr", "BitXor", "Shl", "Shr",
        ];
        let unary_traits = ["Neg", "Not"];

        for imp in &module.impls {
            if let Some(trait_ref) = &imp.trait_ {
                let trait_name = trait_ref.path.last().cloned().unwrap_or_default();
                let self_ty = type_to_string(&imp.self_ty);

                if binary_traits.contains(&trait_name.as_str()) {
                    // Get the RHS type from trait type args
                    let rhs_ty = trait_ref
                        .type_args
                        .first()
                        .map(type_to_string)
                        .unwrap_or_else(|| self_ty.clone());

                    // Find the Output associated type
                    for item in &imp.items {
                        if let IrImplItem::AssociatedType { name, ty } = item {
                            if name == "Output" {
                                analysis
                                    .binary_ops
                                    .insert((trait_name.clone(), self_ty.clone(), rhs_ty.clone()), ty.clone());
                            }
                        }
                    }
                } else if unary_traits.contains(&trait_name.as_str()) {
                    for item in &imp.items {
                        if let IrImplItem::AssociatedType { name, ty } = item {
                            if name == "Output" {
                                analysis
                                    .unary_ops
                                    .insert((trait_name.clone(), self_ty.clone()), ty.clone());
                            }
                        }
                    }
                }
            }
        }

        analysis
    }
}

/// A visitor pattern for traversing the IR
pub trait IrVisitor {
    fn visit_module(&mut self, module: &IrModule) {
        for s in &module.structs {
            self.visit_struct(s);
        }
        for t in &module.traits {
            self.visit_trait(t);
        }
        for i in &module.impls {
            self.visit_impl(i);
        }
        for f in &module.functions {
            self.visit_function(f);
        }
    }

    fn visit_struct(&mut self, _s: &IrStruct) {}
    fn visit_trait(&mut self, _t: &IrTrait) {}
    fn visit_impl(&mut self, _i: &IrImpl) {}
    fn visit_function(&mut self, _f: &IrFunction) {}
    fn visit_expr(&mut self, _e: &IrExpr) {}
    fn visit_type(&mut self, _t: &IrType) {}
    fn visit_pattern(&mut self, _p: &IrPattern) {}
}

/// A mutable visitor pattern for transforming the IR
pub trait IrVisitorMut {
    fn visit_module_mut(&mut self, module: &mut IrModule) {
        for s in &mut module.structs {
            self.visit_struct_mut(s);
        }
        for t in &mut module.traits {
            self.visit_trait_mut(t);
        }
        for i in &mut module.impls {
            self.visit_impl_mut(i);
        }
        for f in &mut module.functions {
            self.visit_function_mut(f);
        }
    }

    fn visit_struct_mut(&mut self, _s: &mut IrStruct) {}
    fn visit_trait_mut(&mut self, _t: &mut IrTrait) {}
    fn visit_impl_mut(&mut self, _i: &mut IrImpl) {}
    fn visit_function_mut(&mut self, _f: &mut IrFunction) {}
    fn visit_expr_mut(&mut self, _e: &mut IrExpr) {}
    fn visit_type_mut(&mut self, _t: &mut IrType) {}
    fn visit_pattern_mut(&mut self, _p: &mut IrPattern) {}
}

/// Collect all type references in an expression
pub fn collect_type_refs(expr: &IrExpr) -> HashSet<String> {
    let mut refs = HashSet::new();
    collect_type_refs_inner(expr, &mut refs);
    refs
}

fn collect_type_refs_inner(expr: &IrExpr, refs: &mut HashSet<String>) {
    match expr {
        IrExpr::Path { segments, type_args } => {
            refs.insert(segments.join("::"));
            for arg in type_args {
                collect_type_refs_from_type(arg, refs);
            }
        }
        IrExpr::MethodCall { receiver, type_args, args, .. } => {
            collect_type_refs_inner(receiver, refs);
            for arg in type_args {
                collect_type_refs_from_type(arg, refs);
            }
            for arg in args {
                collect_type_refs_inner(arg, refs);
            }
        }
        IrExpr::Call { func, args } => {
            collect_type_refs_inner(func, refs);
            for arg in args {
                collect_type_refs_inner(arg, refs);
            }
        }
        IrExpr::Struct { path, fields, rest } => {
            refs.insert(path.join("::"));
            for (_, expr) in fields {
                collect_type_refs_inner(expr, refs);
            }
            if let Some(r) = rest {
                collect_type_refs_inner(r, refs);
            }
        }
        IrExpr::Binary { left, right, .. } => {
            collect_type_refs_inner(left, refs);
            collect_type_refs_inner(right, refs);
        }
        IrExpr::Unary { expr, .. } => {
            collect_type_refs_inner(expr, refs);
        }
        IrExpr::Block(block) => {
            for stmt in &block.stmts {
                if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                    collect_type_refs_inner(e, refs);
                }
            }
            if let Some(e) = &block.expr {
                collect_type_refs_inner(e, refs);
            }
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            collect_type_refs_inner(cond, refs);
            for stmt in &then_branch.stmts {
                if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                    collect_type_refs_inner(e, refs);
                }
            }
            if let Some(e) = else_branch {
                collect_type_refs_inner(e, refs);
            }
        }
        IrExpr::ForLoop { iter, body, .. } => {
            collect_type_refs_inner(iter, refs);
            for stmt in &body.stmts {
                if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                    collect_type_refs_inner(e, refs);
                }
            }
        }
        IrExpr::Closure { body, .. } => {
            collect_type_refs_inner(body, refs);
        }
        _ => {}
    }
}

fn collect_type_refs_from_type(ty: &IrType, refs: &mut HashSet<String>) {
    match ty {
        IrType::Path { segments, type_args } => {
            refs.insert(segments.join("::"));
            for arg in type_args {
                collect_type_refs_from_type(arg, refs);
            }
        }
        IrType::Reference { elem, .. } | IrType::Slice(elem) => {
            collect_type_refs_from_type(elem, refs);
        }
        IrType::Array { elem, .. } => {
            collect_type_refs_from_type(elem, refs);
        }
        IrType::Tuple(elems) => {
            for elem in elems {
                collect_type_refs_from_type(elem, refs);
            }
        }
        IrType::Projection { base, .. } => {
            collect_type_refs_from_type(base, refs);
        }
        _ => {}
    }
}

/// Represents a monomorphized (specialized) version of a generic function
#[derive(Debug, Clone)]
pub struct MonomorphizedFunction {
    /// The original function name
    pub base_name: String,
    /// The specialized name (with type parameters encoded)
    pub specialized_name: String,
    /// The type substitutions applied
    pub type_args: Vec<(String, IrType)>,
    /// The monomorphized function body
    pub function: IrFunction,
}

/// Monomorphization context
#[derive(Debug, Default)]
pub struct MonomorphizationContext {
    /// Already monomorphized functions
    pub monomorphized: HashMap<String, MonomorphizedFunction>,
    /// Functions waiting to be monomorphized
    pub worklist: Vec<(String, Vec<(String, IrType)>)>,
}

impl MonomorphizationContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Generate a specialized name for a function with type arguments
    pub fn specialized_name(base_name: &str, type_args: &[(String, IrType)]) -> String {
        if type_args.is_empty() {
            base_name.to_string()
        } else {
            let args_str: Vec<String> = type_args
                .iter()
                .map(|(_, ty)| type_to_string(ty).replace("::", "_").replace("<", "_").replace(">", "_").replace(", ", "_"))
                .collect();
            format!("{}_{}", base_name, args_str.join("_"))
        }
    }
}

/// Extracts all unique struct/type instantiations from the IR
pub fn extract_type_instantiations(module: &IrModule) -> HashSet<String> {
    let mut instantiations = HashSet::new();

    struct InstantiationCollector<'a> {
        instantiations: &'a mut HashSet<String>,
    }

    impl<'a> IrVisitor for InstantiationCollector<'a> {
        fn visit_type(&mut self, t: &IrType) {
            if let IrType::Path { segments: _, type_args } = t {
                if !type_args.is_empty() {
                    let full_type = type_to_string(t);
                    self.instantiations.insert(full_type);
                }
            }
        }
    }

    let mut collector = InstantiationCollector {
        instantiations: &mut instantiations,
    };

    // Walk through all structs, impls, and functions
    for s in &module.structs {
        for field in &s.fields {
            collector.visit_type(&field.ty);
        }
    }

    for imp in &module.impls {
        collector.visit_type(&imp.self_ty);
        for item in &imp.items {
            if let IrImplItem::Method(f) = item {
                if let Some(ret) = &f.return_type {
                    collector.visit_type(ret);
                }
                for param in &f.params {
                    collector.visit_type(&param.ty);
                }
            }
        }
    }

    instantiations
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;

    #[test]
    fn test_type_context_from_module() {
        let source = r#"
            pub struct Delta<N, T> {
                pub delta: GenericArray<T, N>,
            }
            
            impl<N, T: Clone> Clone for Delta<N, T> {
                fn clone(&self) -> Self {
                    todo!()
                }
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        let ctx = TypeContext::from_module(&module);

        assert!(ctx.structs.contains_key("Delta"));
    }

    #[test]
    fn test_operator_analysis() {
        let source = r#"
            impl<N, T: Add<U>, U> Add<Vope<N, U>> for Vope<N, T> {
                type Output = Vope<N, T::Output>;
                fn add(self, rhs: Vope<N, U>) -> Self::Output {
                    todo!()
                }
            }
        "#;

        let module = parse_source(source, "test").unwrap();
        let analysis = OperatorAnalysis::from_module(&module);

        // Should have one binary operator
        assert!(!analysis.binary_ops.is_empty());
    }

    #[test]
    fn test_specialized_name() {
        let type_args = vec![
            ("N".to_string(), IrType::simple("u8")),
            ("T".to_string(), IrType::simple("Galois")),
        ];

        let name = MonomorphizationContext::specialized_name("remap", &type_args);
        assert!(name.contains("remap"));
        assert!(name.contains("u8"));
        assert!(name.contains("Galois"));
    }
}
