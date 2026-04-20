// @reliability: normal
//! Monomorphization pass for `IrModule` and `IrCfgModule`.
//!
//! Substitutes concrete values for type parameters before LIR lowering.
//! Only handles length (const) parameters like `N: ArraySize`; trait parameters
//! like `D: Digest` are handled at method-dispatch time via `MonoEnv::hash_suffix`.

use std::collections::BTreeMap;
use volar_compiler::ir::{
    ArrayLength, ExternalKind, IrBlock, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump,
    IrCfgModule, IrCfgTerminator, IrEnum, IrEnumVariant, IrEnumVariantData, IrExpr, IrField,
    IrFunction, IrImpl, IrImplItem, IrModule, IrParam, IrStmt, IrStruct, IrType, IrTypeAlias,
};

// ============================================================================
// MonoEnv
// ============================================================================

/// Monomorphization environment: concrete values for type/const parameters.
pub struct MonoEnv {
    /// Substitutions for array-length type parameters.
    /// e.g., `"N" → 16` for `Array<u8, N>` → `Array<u8, 16>`.
    pub const_params: BTreeMap<String, usize>,
    /// Concrete name suffix for the `D: Digest` trait parameter.
    /// e.g., `"sha256"`. Appended to crypto extern function names.
    pub hash_suffix: String,
}

impl MonoEnv {
    pub fn new(hash_suffix: impl Into<String>) -> Self {
        MonoEnv {
            const_params: BTreeMap::new(),
            hash_suffix: hash_suffix.into(),
        }
    }

    pub fn with_len(mut self, param: impl Into<String>, len: usize) -> Self {
        self.const_params.insert(param.into(), len);
        self
    }
}

// ============================================================================
// Module entry point
// ============================================================================

/// Monomorphize an entire `IrModule`: substitutes type/length parameters
/// in all struct definitions and all function signatures/bodies.
pub fn monomorphize_module(module: &IrModule, env: &MonoEnv) -> IrModule {
    IrModule {
        name: module.name.clone(),
        structs: module.structs.iter().map(|s| monomorphize_struct(s, env)).collect(),
        enums: module.enums.clone(),
        traits: module.traits.clone(),
        impls: module.impls.clone(),
        functions: module.functions.iter().map(|f| monomorphize_function(f, env)).collect(),
        type_aliases: module.type_aliases.clone(),
    }
}

/// Monomorphize an `IrCfgModule`: substitutes type/length parameters
/// in all struct definitions, enums, type aliases, impls, and function
/// signatures/bodies (both CFG and auxiliary).
pub fn monomorphize_cfg_module(module: &IrCfgModule, env: &MonoEnv) -> IrCfgModule {
    IrCfgModule {
        name: module.name.clone(),
        structs: module.structs.iter().map(|s| monomorphize_struct(s, env)).collect(),
        enums: module.enums.iter().map(|e| monomorphize_enum(e, env)).collect(),
        traits: module.traits.clone(),
        impls: module.impls.iter().map(|i| monomorphize_impl(i, env)).collect(),
        functions: module.functions.iter().map(|f| monomorphize_cfg_function(f, env)).collect(),
        auxiliary_functions: module.auxiliary_functions.iter().map(|f| monomorphize_function(f, env)).collect(),
        type_aliases: module.type_aliases.iter().map(|a| monomorphize_type_alias(a, env)).collect(),
    }
}

// ============================================================================
// Struct monomorphization
// ============================================================================

fn monomorphize_struct(s: &IrStruct, env: &MonoEnv) -> IrStruct {
    IrStruct {
        kind: s.kind.clone(),
        // Remove generic params that are being substituted.
        generics: s
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        fields: s
            .fields
            .iter()
            .map(|f| IrField {
                name: f.name.clone(),
                ty: mono_type(&f.ty, env),
                public: f.public,
            })
            .collect(),
        is_tuple: s.is_tuple,
        native_volar_type: s.native_volar_type,
        derives: s.derives.clone(),
    }
}

// ============================================================================
// Function monomorphization
// ============================================================================

pub fn monomorphize_function(func: &IrFunction, env: &MonoEnv) -> IrFunction {
    IrFunction {
        name: func.name.clone(),
        generics: func
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        receiver: func.receiver,
        params: func
            .params
            .iter()
            .map(|p| IrParam { name: p.name.clone(), ty: mono_type(&p.ty, env) })
            .collect(),
        return_type: func.return_type.as_ref().map(|t| mono_type(t, env)),
        where_clause: func.where_clause.clone(),
        body: mono_block(&func.body, env),
        external_kind: func.external_kind,
    }
}

// ============================================================================
// Enum monomorphization
// ============================================================================

fn monomorphize_enum(e: &IrEnum, env: &MonoEnv) -> IrEnum {
    IrEnum {
        kind: e.kind.clone(),
        generics: e
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        variants: e
            .variants
            .iter()
            .map(|v| IrEnumVariant {
                name: v.name.clone(),
                fields: match &v.fields {
                    IrEnumVariantData::Unit => IrEnumVariantData::Unit,
                    IrEnumVariantData::Tuple(tys) => {
                        IrEnumVariantData::Tuple(tys.iter().map(|t| mono_type(t, env)).collect())
                    }
                    IrEnumVariantData::Struct(fields) => {
                        IrEnumVariantData::Struct(
                            fields
                                .iter()
                                .map(|f| IrField {
                                    name: f.name.clone(),
                                    ty: mono_type(&f.ty, env),
                                    public: f.public,
                                })
                                .collect(),
                        )
                    }
                },
            })
            .collect(),
        derives: e.derives.clone(),
    }
}

// ============================================================================
// Impl monomorphization
// ============================================================================

fn monomorphize_impl(imp: &IrImpl, env: &MonoEnv) -> IrImpl {
    IrImpl {
        generics: imp
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        trait_: imp.trait_.clone(),
        self_ty: mono_type(&imp.self_ty, env),
        where_clause: imp.where_clause.clone(),
        items: imp
            .items
            .iter()
            .map(|item| match item {
                IrImplItem::Method(f) => IrImplItem::Method(monomorphize_function(f, env)),
                IrImplItem::AssociatedType { name, ty } => {
                    IrImplItem::AssociatedType { name: name.clone(), ty: mono_type(ty, env) }
                }
            })
            .collect(),
    }
}

// ============================================================================
// Type alias monomorphization
// ============================================================================

fn monomorphize_type_alias(alias: &IrTypeAlias, env: &MonoEnv) -> IrTypeAlias {
    IrTypeAlias {
        name: alias.name.clone(),
        generics: alias
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        target: mono_type(&alias.target, env),
    }
}

// ============================================================================
// CFG function monomorphization
// ============================================================================

fn monomorphize_cfg_function(func: &IrCfgFunction, env: &MonoEnv) -> IrCfgFunction {
    IrCfgFunction {
        name: func.name.clone(),
        generics: func
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        receiver: func.receiver,
        params: func
            .params
            .iter()
            .map(|p| IrParam { name: p.name.clone(), ty: mono_type(&p.ty, env) })
            .collect(),
        return_type: func.return_type.as_ref().map(|t| mono_type(t, env)),
        where_clause: func.where_clause.clone(),
        external_kind: func.external_kind,
        body: mono_cfg_body(&func.body, env),
    }
}

fn mono_cfg_body(body: &IrCfgBody, env: &MonoEnv) -> IrCfgBody {
    IrCfgBody {
        blocks: body.blocks.iter().map(|b| mono_cfg_block(b, env)).collect(),
    }
}

fn mono_cfg_block(block: &IrCfgBlock, env: &MonoEnv) -> IrCfgBlock {
    IrCfgBlock {
        params: block
            .params
            .iter()
            .map(|p| IrParam { name: p.name.clone(), ty: mono_type(&p.ty, env) })
            .collect(),
        stmts: block.stmts.iter().map(|s| mono_stmt(s, env)).collect(),
        stmt_provs: Vec::new(),
        terminator: mono_cfg_terminator(&block.terminator, env),
    }
}

fn mono_cfg_terminator(term: &IrCfgTerminator, env: &MonoEnv) -> IrCfgTerminator {
    match term {
        IrCfgTerminator::Return(val) => {
            IrCfgTerminator::Return(val.as_ref().map(|e| mono_expr(e, env)))
        }
        IrCfgTerminator::Goto(jump) => IrCfgTerminator::Goto(mono_cfg_jump(jump, env)),
        IrCfgTerminator::CondGoto { cond, then_, else_ } => IrCfgTerminator::CondGoto {
            cond: mono_expr(cond, env),
            then_: mono_cfg_jump(then_, env),
            else_: mono_cfg_jump(else_, env),
        },
    }
}

fn mono_cfg_jump(jump: &IrCfgJump, env: &MonoEnv) -> IrCfgJump {
    IrCfgJump {
        target: jump.target,
        args: jump.args.iter().map(|a| mono_expr(a, env)).collect(),
    }
}

// ============================================================================
// Type monomorphization
// ============================================================================

pub fn mono_type(ty: &IrType, env: &MonoEnv) -> IrType {
    match ty {
        IrType::TypeParam(name) => {
            // If it's a known const param, we keep the TypeParam but with
            // the concrete length resolved into containing Array types.
            // Standalone TypeParam("N") in non-length context → leave as-is.
            ty.clone()
        }
        IrType::Array { kind, elem, len } => IrType::Array {
            kind: *kind,
            elem: Box::new(mono_type(elem, env)),
            len: mono_len(len, env),
        },
        IrType::Struct { kind, type_args } => IrType::Struct {
            kind: kind.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
        },
        IrType::Reference { mutable, elem } => IrType::Reference {
            mutable: *mutable,
            elem: Box::new(mono_type(elem, env)),
        },
        IrType::Tuple(elems) => IrType::Tuple(elems.iter().map(|e| mono_type(e, env)).collect()),
        IrType::Vector { elem } => {
            IrType::Vector { elem: Box::new(mono_type(elem, env)) }
        }
        // Primitive, Unit, Never, Infer, Existential, FnPtr, Projection, Param
        other => other.clone(),
    }
}

fn mono_len(len: &ArrayLength, env: &MonoEnv) -> ArrayLength {
    match len {
        ArrayLength::TypeParam(name) => {
            if let Some(&n) = env.const_params.get(name) {
                ArrayLength::Const(n)
            } else {
                len.clone()
            }
        }
        // Projection might reference a const param indirectly; leave for now.
        other => other.clone(),
    }
}

// ============================================================================
// Expression / block monomorphization
// ============================================================================

fn mono_block(block: &volar_compiler::ir::IrBlock, env: &MonoEnv) -> volar_compiler::ir::IrBlock {
    volar_compiler::ir::IrBlock {
        stmts: block.stmts.iter().map(|s| mono_stmt(s, env)).collect(),
        stmt_provs: Vec::new(),
        expr: block.expr.as_ref().map(|e| Box::new(mono_expr(e, env))),
    }
}

fn mono_stmt(stmt: &IrStmt, env: &MonoEnv) -> IrStmt {
    match stmt {
        IrStmt::Let { pattern, ty, init } => IrStmt::Let {
            pattern: pattern.clone(),
            ty: ty.as_ref().map(|t| mono_type(t, env)),
            init: init.as_ref().map(|e| mono_expr(e, env)),
        },
        IrStmt::Semi(e) => IrStmt::Semi(mono_expr(e, env)),
        IrStmt::Expr(e) => IrStmt::Expr(mono_expr(e, env)),
    }
}

fn mono_expr(expr: &IrExpr, env: &MonoEnv) -> IrExpr {
    use IrExpr::*;
    match expr {
        Lit(_) | Var(_) | Continue => expr.clone(),

        Path { segments, type_args } => Path {
            segments: segments.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
        },

        Binary { op, left, right } => Binary {
            op: *op,
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
        },

        Unary { op, expr: inner } => Unary { op: *op, expr: Box::new(mono_expr(inner, env)) },

        MethodCall { receiver, method, type_args, args } => MethodCall {
            receiver: Box::new(mono_expr(receiver, env)),
            method: method.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
            args: args.iter().map(|a| mono_expr(a, env)).collect(),
        },

        Call { func, args } => Call {
            func: Box::new(mono_expr(func, env)),
            args: args.iter().map(|a| mono_expr(a, env)).collect(),
        },

        Field { base, field } => {
            Field { base: Box::new(mono_expr(base, env)), field: field.clone() }
        }

        Index { base, index } => Index {
            base: Box::new(mono_expr(base, env)),
            index: Box::new(mono_expr(index, env)),
        },

        StructExpr { kind, type_args, fields, rest } => StructExpr {
            kind: kind.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
            fields: fields
                .iter()
                .map(|(name, e)| (name.clone(), mono_expr(e, env)))
                .collect(),
            rest: rest.as_ref().map(|r| Box::new(mono_expr(r, env))),
        },

        Tuple(elems) => Tuple(elems.iter().map(|e| mono_expr(e, env)).collect()),
        Array(elems) => Array(elems.iter().map(|e| mono_expr(e, env)).collect()),
        FixedArray(elems) => FixedArray(elems.iter().map(|e| mono_expr(e, env)).collect()),

        Repeat { elem, len } => {
            Repeat { elem: Box::new(mono_expr(elem, env)), len: Box::new(mono_expr(len, env)) }
        }

        ArrayGenerate { elem_ty, len, index_var, body } => ArrayGenerate {
            elem_ty: elem_ty.as_ref().map(|t| Box::new(mono_type(t, env))),
            len: mono_len(len, env),
            index_var: index_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        DefaultValue { ty } => {
            DefaultValue { ty: ty.as_ref().map(|t| Box::new(mono_type(t, env))) }
        }

        LengthOf(len) => LengthOf(mono_len(len, env)),

        BoundedLoop { var, start, end, inclusive, body } => BoundedLoop {
            var: var.clone(),
            start: Box::new(mono_expr(start, env)),
            end: Box::new(mono_expr(end, env)),
            inclusive: *inclusive,
            body: mono_block(body, env),
        },

        IterLoop { pattern, collection, body } => IterLoop {
            pattern: pattern.clone(),
            collection: Box::new(mono_expr(collection, env)),
            body: mono_block(body, env),
        },

        Block(b) => Block(mono_block(b, env)),

        If { cond, then_branch, else_branch } => If {
            cond: Box::new(mono_expr(cond, env)),
            then_branch: mono_block(then_branch, env),
            else_branch: else_branch.as_ref().map(|e| Box::new(mono_expr(e, env))),
        },

        Match { expr: inner, arms } => Match {
            expr: Box::new(mono_expr(inner, env)),
            arms: arms
                .iter()
                .map(|arm| volar_compiler::ir::IrMatchArm {
                    pattern: arm.pattern.clone(),
                    guard: arm.guard.as_ref().map(|g| mono_expr(g, env)),
                    body: mono_expr(&arm.body, env),
                })
                .collect(),
        },

        Closure { params, ret_type, body } => Closure {
            params: params
                .iter()
                .map(|p| volar_compiler::ir::IrClosureParam {
                    pattern: p.pattern.clone(),
                    ty: p.ty.as_ref().map(|t| mono_type(t, env)),
                })
                .collect(),
            ret_type: ret_type.as_ref().map(|t| Box::new(mono_type(t, env))),
            body: Box::new(mono_expr(body, env)),
        },

        Cast { expr: inner, ty } => {
            Cast { expr: Box::new(mono_expr(inner, env)), ty: Box::new(mono_type(ty, env)) }
        }

        Return(val) => Return(val.as_ref().map(|e| Box::new(mono_expr(e, env)))),
        Break(val) => Break(val.as_ref().map(|e| Box::new(mono_expr(e, env)))),

        Assign { left, right } => Assign {
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
        },

        AssignOp { op, left, right } => AssignOp {
            op: *op,
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
        },

        RawMap { receiver, elem_var, body } => RawMap {
            receiver: Box::new(mono_expr(receiver, env)),
            elem_var: elem_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        RawZip { left, right, left_var, right_var, body } => RawZip {
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
            left_var: left_var.clone(),
            right_var: right_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        RawFold { receiver, init, acc_var, elem_var, body } => RawFold {
            receiver: Box::new(mono_expr(receiver, env)),
            init: Box::new(mono_expr(init, env)),
            acc_var: acc_var.clone(),
            elem_var: elem_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        IterPipeline(_) | Range { .. } => expr.clone(),

        other => other.clone(),
    }
}
