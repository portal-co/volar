// @reliability: normal
// @ai: assisted
//! Lower `IrModule` (spec/compiler IR) to any `LirTarget`.
//!
//! Works best on monomorphized IR — run `monomorphize_module` first for any
//! module that uses generic type/length parameters.
//!
//! Handles: primitive types, arrays, structs, binary/unary ops, variables,
//! literals, if/else, let-bindings, field access, array indexing,
//! struct construction, BoundedLoop, ArrayGenerate, RawMap, RawZip,
//! `.clone()` (identity), crypto method calls (→ extern), call_extern.

pub mod mono;
pub mod structs;

use std::sync::LazyLock;

use std::collections::BTreeMap;
use volar_compiler::ir::{
    ArrayKind, ArrayLength, ExternalKind, IrAnyFunction, IrBlock, IrCfgBody, IrCfgFunction,
    IrCfgJump, IrCfgModule, IrCfgTerminator, IrClosureParam, IrExpr, IrFunction, IrLit, IrModule,
    IrPattern, IrStmt, IrType, MethodKind, PrimitiveType, SpecBinOp, SpecUnaryOp, StdMethod,
    StructKind,
};
use volar_lir::{IcmpPred, LirTarget, LirType, LirAbi};

use structs::{
    StructRegistry, flatten_count, flatten_scalar_types,
    struct_field_scalar_offset, struct_field_scalar_width, primitive_to_lir,
};

// Unwrap a single-element Vec into a scalar, panicking if the vec has != 1 element.
fn into_scalar<V: Clone>(vals: Vec<V>, context: &str) -> V {
    vals.into_iter().next().unwrap_or_else(|| panic!("expected scalar at {context}, got 0 values"))
}

// ============================================================================
// Type mapping
// ============================================================================

/// Convert an `IrType` to `LirType` without a struct registry (primitives only).
fn ir_type_to_lir(ty: &IrType) -> LirType {
    match ty {
        IrType::Primitive(p) => primitive_to_lir(*p),
        IrType::Unit => LirType::Bool,
        IrType::Reference { elem, .. } => ir_type_to_lir(elem),
        IrType::Array { elem, len, .. } => {
            let n = const_len(len);
            LirType::Arr(Box::new(ir_type_to_lir(elem)), n)
        }
        other => unimplemented!("ir_type_to_lir: unsupported type {:?}", other),
    }
}

fn const_len(len: &ArrayLength) -> usize {
    match len {
        ArrayLength::Const(n) => *n,
        ArrayLength::TypeNum(tn) => tn.to_usize(),
        ArrayLength::TypeParam(name) => {
            panic!("unsubstituted TypeParam length '{name}' — run monomorphize_module first")
        }
        ArrayLength::Projection { .. } => unimplemented!("Projection length in lowering"),
    }
}

// ============================================================================
// External-function dispatch info
// ============================================================================

/// Metadata for a function annotated `#[oracle]`, `#[action]`, or `#[rng]`.
/// Built once per module and consulted at each call site during lowering.
struct ExternalFnInfo {
    kind: ExternalKind,
    /// LIR types of the declared parameters (for arg-type passing to `LirTarget`).
    param_tys: Vec<LirType>,
    /// LIR return type (single element; multi-output uses multiple entries).
    return_type: Option<LirType>,
}

/// Signature metadata for any function in the module (normal or external).
/// Used to infer return types at call sites.
struct FuncSigInfo {
    /// LIR types of the declared parameters.
    param_tys: Vec<LirType>,
    /// LIR return type.
    return_type: Option<LirType>,
}

// ============================================================================
// Lowering context
// ============================================================================

struct LowerCtx<'t, T: LirTarget> {
    target: &'t mut T,
    /// Variable name → flat scalar SSA values.
    env: BTreeMap<String, Vec<T::Value>>,
    /// Variable name → IrType (for field/index type inference).
    env_types: BTreeMap<String, IrType>,
    /// The block we are currently emitting into.
    current_block: T::Block,
    /// Struct registry (IDs, field names, field types).
    registry: &'t StructRegistry,
    /// Hash suffix for crypto extern names (e.g., "sha256").
    hash_suffix: String,
    /// Original IrModule struct defs (for field type lookup).
    module_structs: &'t [volar_compiler::ir::IrStruct],
    /// Functions annotated `#[oracle]`, `#[action]`, or `#[rng]`.
    /// Populated from the module at `lower_module_with_opts` time.
    external_fns: &'t BTreeMap<String, ExternalFnInfo>,
    /// Signatures of all functions in the module (for return-type inference
    /// at call sites).  Populated by `lower_module_with_opts`.
    func_sigs: &'t BTreeMap<String, FuncSigInfo>,
    /// IR-level return types for functions, for type inference on Call exprs.
    ir_func_ret_types: &'t BTreeMap<String, IrType>,
}

impl<'t, T: LirTarget> LowerCtx<'t, T> {
    fn new(
        target: &'t mut T,
        entry: T::Block,
        params: Vec<(String, Vec<T::Value>, IrType)>,
        registry: &'t StructRegistry,
        hash_suffix: String,
        module_structs: &'t [volar_compiler::ir::IrStruct],
    ) -> Self {
        let mut env = BTreeMap::new();
        let mut env_types = BTreeMap::new();
        for (name, vals, ty) in params {
            env.insert(name.clone(), vals);
            env_types.insert(name, ty);
        }
        LowerCtx { target, env, env_types, current_block: entry, registry, hash_suffix, module_structs, external_fns: &EMPTY_EXTERNAL_FNS, func_sigs: &EMPTY_FUNC_SIGS, ir_func_ret_types: &EMPTY_IR_RET_TYPES }
    }

    /// Infer the IrType of an expression using env_types and module struct defs.
    /// Returns `None` for unsupported or uninferable expressions.
    fn infer_type(&self, expr: &IrExpr) -> Option<IrType> {
        match expr {
            IrExpr::Var(name) => self.env_types.get(name).cloned(),

            IrExpr::Field { base, field } => {
                let base_ty = self.infer_type(base)?;
                // Handle tuple field access (e.g., `.0`, `.1`).
                if let IrType::Tuple(elems) = &base_ty {
                    let idx: usize = field.parse().ok()?;
                    return elems.get(idx).cloned();
                }
                let struct_kind = extract_struct_kind(&base_ty)?;
                let ir_struct = self
                    .module_structs
                    .iter()
                    .find(|s| s.kind == *struct_kind)?;
                let field_def = ir_struct.fields.iter().find(|f| &f.name == field)?;
                Some(field_def.ty.clone())
            }

            IrExpr::Index { base, .. } => {
                let base_ty = self.infer_type(base)?;
                match &base_ty {
                    IrType::Array { elem, .. } => Some(*elem.clone()),
                    // Reference<Array<T>> (including slices): element type is T
                    IrType::Reference { elem, .. } => match elem.as_ref() {
                        IrType::Array { elem: inner, .. } => Some(*inner.clone()),
                        _ => None,
                    },
                    _ => None,
                }
            }

            IrExpr::MethodCall {
                receiver,
                method: MethodKind::Known(StdMethod::Clone | StdMethod::Deref),
                ..
            } => {
                self.infer_type(receiver)
            }

            IrExpr::Unary {
                op: SpecUnaryOp::Ref | SpecUnaryOp::RefMut,
                expr: inner,
            } => self.infer_type(inner),

            IrExpr::Unary { op: SpecUnaryOp::Deref, expr: inner } => {
                let ty = self.infer_type(inner)?;
                match ty {
                    IrType::Reference { elem, .. } => Some(*elem),
                    other => Some(other),
                }
            }

            // Call: look up the function name's return type.
            IrExpr::Call { func, .. } => {
                if let IrExpr::Path { segments, .. } = func.as_ref() {
                    let name = segments.last()?;
                    self.ir_func_ret_types.get(name).cloned()
                } else {
                    None
                }
            }

            // Literal: infer primitive type from the literal variant.
            IrExpr::Lit(lit) => match lit {
                IrLit::Bool(_) => Some(IrType::Primitive(PrimitiveType::Bool)),
                IrLit::Int(_) => Some(IrType::Primitive(PrimitiveType::U64)),
                IrLit::Unit => Some(IrType::Unit),
                _ => None,
            },

            // FixedArray: infer element type from the first element + count.
            IrExpr::FixedArray(elems) => {
                if elems.is_empty() {
                    return None;
                }
                let elem_ty = self.infer_type(&elems[0])?;
                Some(IrType::Array {
                    kind: ArrayKind::FixedArray,
                    elem: Box::new(elem_ty),
                    len: ArrayLength::Const(elems.len()),
                })
            }

            _ => None,
        }
    }
}

/// Extract the `StructKind` from a type (stripping references).
fn extract_struct_kind(ty: &IrType) -> Option<&StructKind> {
    match ty {
        IrType::Struct { kind, .. } => Some(kind),
        IrType::Reference { elem, .. } => extract_struct_kind(elem),
        _ => None,
    }
}

static EMPTY_EXTERNAL_FNS: LazyLock<BTreeMap<String, ExternalFnInfo>> =
    LazyLock::new(BTreeMap::new);
static EMPTY_FUNC_SIGS: LazyLock<BTreeMap<String, FuncSigInfo>> =
    LazyLock::new(BTreeMap::new);
static EMPTY_IR_RET_TYPES: LazyLock<BTreeMap<String, IrType>> =
    LazyLock::new(BTreeMap::new);

// ============================================================================
// Public API
// ============================================================================

/// Lower all functions in `module` to `target`.
///
/// The module should be monomorphized before calling this (via `mono::monomorphize_module`).
/// Struct types are registered via `structs::build_struct_registry` before lowering functions.
pub fn lower_module<T: LirTarget>(module: &IrModule<IrFunction>, target: &mut T) {
    lower_module_with_opts(module, target, "");
}

/// Like `lower_module` but with an explicit crypto hash suffix (for extern names).
pub fn lower_module_with_opts<T: LirTarget>(
    module: &IrModule<IrFunction>,
    target: &mut T,
    hash_suffix: &str,
) {
    let registry = structs::build_struct_registry(module, target);

    // Build a lookup table for oracle/action/rng functions.
    let external_fns: BTreeMap<String, ExternalFnInfo> = module
        .functions
        .iter()
        .filter(|f| matches!(f.external_kind, ExternalKind::Oracle | ExternalKind::Action | ExternalKind::Rng))
        .map(|f| {
            let param_tys = f.params.iter().map(|p| registry.ir_type_to_lir(&p.ty)).collect();
            let return_type = f.return_type.as_ref().map(|t| registry.ir_type_to_lir(t));
            (f.name.clone(), ExternalFnInfo {
                kind: f.external_kind,
                param_tys,
                return_type,
            })
        })
        .collect();

    // Build a signature table for ALL functions (for return-type inference at
    // call sites).  This enables `lower_call` to pass the correct `ret_ty`
    // to `call_extern` instead of `None`.
    let func_sigs: BTreeMap<String, FuncSigInfo> = module
        .functions
        .iter()
        .map(|f| {
            let param_tys = f.params.iter().map(|p| registry.ir_type_to_lir(&p.ty)).collect();
            let return_type = f.return_type.as_ref().map(|t| registry.ir_type_to_lir(t));
            (f.name.clone(), FuncSigInfo { param_tys, return_type })
        })
        .collect();

    // Lower only normal (non-external) function bodies.
    for func in &module.functions {
        if func.external_kind != ExternalKind::Normal {
            continue;
        }
        lower_function_in_module(
            func, target, &registry, hash_suffix, &module.structs, &external_fns, &func_sigs,
        );
    }
}

/// Internal: lower a function with full module context (external-fn dispatch).
fn lower_function_in_module<T: LirTarget>(
    func: &IrFunction,
    target: &mut T,
    registry: &StructRegistry,
    hash_suffix: &str,
    module_structs: &[volar_compiler::ir::IrStruct],
    external_fns: &BTreeMap<String, ExternalFnInfo>,
    func_sigs: &BTreeMap<String, FuncSigInfo>,
) {
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty))
        .collect();
    let ret_ty = func.return_type.as_ref().map(|t| registry.ir_type_to_lir(t));

    let (entry, param_val_groups) = target.begin_function(&func.name, &param_lir_tys, ret_ty);
    target.switch_to_block(entry.clone());

    let named_params: Vec<(String, Vec<T::Value>, IrType)> = func
        .params
        .iter()
        .zip(param_val_groups.into_iter())
        .map(|(p, vals)| (p.name.clone(), vals, p.ty.clone()))
        .collect();

    let mut ctx = LowerCtx::new(
        target,
        entry,
        named_params,
        registry,
        hash_suffix.to_owned(),
        module_structs,
    );
    ctx.external_fns = external_fns;
    ctx.func_sigs = func_sigs;

    let tail_vals = lower_block(&func.body, &mut ctx);
    ctx.target.ret(&tail_vals);
    ctx.target.end_function();
}

/// Lower a single `IrFunction` to `target` (no struct support).
///
/// Use `lower_module_with_opts` for modules that use structs or arrays.
pub fn lower_function<T: LirTarget>(func: &IrFunction, target: &mut T) {
    let empty_registry = StructRegistry::empty();
    lower_function_with_registry(func, target, &empty_registry, "", &[]);
}

pub fn lower_function_with_registry<T: LirTarget>(
    func: &IrFunction,
    target: &mut T,
    registry: &StructRegistry,
    hash_suffix: &str,
    module_structs: &[volar_compiler::ir::IrStruct],
) {
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty))
        .collect();
    let ret_ty = func.return_type.as_ref().map(|t| registry.ir_type_to_lir(t));

    let (entry, param_val_groups) = target.begin_function(&func.name, &param_lir_tys, ret_ty);
    target.switch_to_block(entry.clone());

    // Each group is the flat scalar values for that parameter.
    let named_params: Vec<(String, Vec<T::Value>, IrType)> = func
        .params
        .iter()
        .zip(param_val_groups.into_iter())
        .map(|(p, vals)| (p.name.clone(), vals, p.ty.clone()))
        .collect();

    let mut ctx = LowerCtx::new(
        target,
        entry,
        named_params,
        registry,
        hash_suffix.to_owned(),
        module_structs,
    );

    let tail_vals = lower_block(&func.body, &mut ctx);
    ctx.target.ret(&tail_vals);
    ctx.target.end_function();
}

// ============================================================================
// Block and statement lowering
// ============================================================================

/// Lower a block, returning the flat scalar list produced by its trailing
/// expression (or an empty vec for unit-typed blocks).
fn lower_block<T: LirTarget>(block: &IrBlock, ctx: &mut LowerCtx<T>) -> Vec<T::Value> {
    for stmt in &block.stmts {
        lower_stmt(stmt, ctx);
    }
    block.expr.as_deref().map(|e| lower_expr(e, ctx)).unwrap_or_default()
}

fn lower_stmt<T: LirTarget>(stmt: &IrStmt, ctx: &mut LowerCtx<T>) {
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            if let Some(init_expr) = init {
                // When the init expression is a literal and we have a type
                // annotation, pass the type hint so the literal gets the
                // correct narrow type (e.g. U8 instead of U64).
                let vals = match init_expr {
                    IrExpr::Lit(lit) => {
                        vec![lower_lit(lit, ctx, ty.as_ref())]
                    }
                    _ => lower_expr(init_expr, ctx),
                };
                if let IrPattern::Ident { name, .. } = pattern {
                    let ir_ty = ty.clone().or_else(|| ctx.infer_type(init_expr));
                    if let Some(ir_ty) = ir_ty {
                        ctx.env_types.insert(name.clone(), ir_ty);
                    }
                }
                bind_pattern(pattern, vals, ctx);
            }
        }
        IrStmt::Semi(expr) | IrStmt::Expr(expr) => {
            lower_expr(expr, ctx);
        }
    }
}

fn bind_pattern<T: LirTarget>(pattern: &IrPattern, vals: Vec<T::Value>, ctx: &mut LowerCtx<T>) {
    match pattern {
        IrPattern::Ident { name, .. } => {
            ctx.env.insert(name.clone(), vals);
        }
        IrPattern::Wild => {}
        other => unimplemented!("bind_pattern: unsupported pattern {:?}", other),
    }
}

// ============================================================================
// Expression lowering
// ============================================================================

/// Lower an expression, returning the flat scalar list for its value.
/// Scalar-typed expressions return a single-element vec.
/// Aggregate-typed expressions return multiple scalars (array = N elems, struct = all fields).
fn lower_expr<T: LirTarget>(expr: &IrExpr, ctx: &mut LowerCtx<T>) -> Vec<T::Value> {
    match expr {
        IrExpr::Lit(lit) => vec![lower_lit(lit, ctx, None)],

        IrExpr::Var(name) => ctx
            .env
            .get(name.as_str())
            .cloned()
            .unwrap_or_else(|| panic!("undefined variable: {name}")),

        IrExpr::Binary { op, left, right } => {
            let lv = into_scalar(lower_expr(left, ctx), "binary lhs");
            let rv = into_scalar(lower_expr(right, ctx), "binary rhs");
            vec![lower_binop(*op, lv, rv, ctx)]
        }

        IrExpr::Unary { op, expr: inner } => {
            // Ref / RefMut / Deref are transparent in value-semantics LIR and must
            // pass through all flat scalars of the inner expression (e.g. `&arr`
            // must keep 64 scalars, not squeeze to 1).
            match op {
                SpecUnaryOp::Ref | SpecUnaryOp::RefMut | SpecUnaryOp::Deref => {
                    lower_expr(inner, ctx)
                }
                _ => {
                    let v = into_scalar(lower_expr(inner, ctx), "unary operand");
                    vec![lower_unop(*op, v, ctx)]
                }
            }
        }

        IrExpr::Block(b) => lower_block(b, ctx),

        IrExpr::If { cond, then_branch, else_branch } => {
            lower_if(cond, then_branch, else_branch.as_deref(), ctx)
        }

        IrExpr::Cast { expr: inner, ty } => {
            let v = into_scalar(lower_expr(inner, ctx), "cast operand");
            let dst = ir_type_to_lir(ty);
            vec![ctx.target.zext(v, dst)]
        }

        IrExpr::Return(val) => {
            let ret_vals = val.as_deref().map(|e| lower_expr(e, ctx)).unwrap_or_default();
            ctx.target.ret(&ret_vals);
            vec![] // unreachable placeholder
        }

        // ---- Phase 2: field access ------------------------------------------

        IrExpr::Field { base, field } => lower_field(base, field, ctx),

        // ---- Phase 2: array index -------------------------------------------

        IrExpr::Index { base, index } => lower_index(base, index, ctx),

        // ---- Phase 2: struct construction -----------------------------------

        IrExpr::StructExpr { kind, fields, .. } => lower_struct_expr(kind, fields, ctx),

        // ---- Phase 2: fixed-size array literal ------------------------------

        IrExpr::FixedArray(elems) => lower_fixed_array(elems, ctx),
        IrExpr::Array(elems) => lower_fixed_array(elems, ctx),

        // ---- Phase 2: array generation via closure --------------------------

        IrExpr::ArrayGenerate { elem_ty, len, index_var, body } => {
            lower_array_generate(elem_ty.as_deref(), len, index_var, body, ctx)
        }

        // ---- Phase 2: element-wise map (RawMap) -----------------------------

        IrExpr::RawMap { receiver, elem_var, body } => {
            lower_raw_map(receiver, elem_var, body, ctx)
        }

        // ---- Phase 2: element-wise zip (RawZip) -----------------------------

        IrExpr::RawZip { left, right, left_var, right_var, body } => {
            lower_raw_zip(left, right, left_var, right_var, body, ctx)
        }

        // ---- Phase 2: bounded loop ------------------------------------------

        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            lower_bounded_loop(var, start, end, *inclusive, body, ctx);
            vec![] // loops are ()-typed
        }

        // ---- Phase 2: method calls ------------------------------------------

        IrExpr::MethodCall { receiver, method, type_args, args } => {
            lower_method_call(receiver, method, type_args, args, ctx)
        }

        // ---- Phase 2: free function calls -----------------------------------

        IrExpr::Call { func, args } => lower_call(func, args, ctx),

        // ---- Assignment (storage writes, etc.) ------------------------------

        IrExpr::Assign { left, right } => {
            lower_assign(left, right, ctx);
            vec![]
        }

        other => unimplemented!("lower_expr: unsupported expr {:?}", other),
    }
}

// ============================================================================
// Literal lowering
// ============================================================================

fn lower_lit<T: LirTarget>(lit: &IrLit, ctx: &mut LowerCtx<T>, hint: Option<&IrType>) -> T::Value {
    match lit {
        IrLit::Int(n) => {
            let lir_ty = hint
                .and_then(|ty| match ty {
                    IrType::Primitive(p) => Some(primitive_to_lir(*p)),
                    _ => None,
                })
                .unwrap_or(LirType::U64);
            ctx.target.iconst(lir_ty, *n as i64)
        }
        IrLit::Bool(b) => ctx.target.iconst(LirType::Bool, *b as i64),
        IrLit::Float(_) => unimplemented!("float literals not supported in LIR"),
        other => unimplemented!("lower_lit: unsupported literal {:?}", other),
    }
}

// ============================================================================
// Binary / unary ops
// ============================================================================

fn lower_binop<T: LirTarget>(
    op: SpecBinOp,
    lv: T::Value,
    rv: T::Value,
    ctx: &mut LowerCtx<T>,
) -> T::Value {
    match op {
        SpecBinOp::Add => ctx.target.add(lv, rv),
        SpecBinOp::Sub => ctx.target.sub(lv, rv),
        SpecBinOp::Mul => ctx.target.mul(lv, rv),
        SpecBinOp::Div => ctx.target.udiv(lv, rv),
        SpecBinOp::Rem => unimplemented!("Rem not in LirTarget"),
        SpecBinOp::BitAnd => ctx.target.and(lv, rv),
        SpecBinOp::BitOr => ctx.target.or(lv, rv),
        SpecBinOp::BitXor => ctx.target.xor(lv, rv),
        SpecBinOp::Shl => ctx.target.shl(lv, rv),
        SpecBinOp::Shr => ctx.target.lshr(lv, rv),
        SpecBinOp::Eq => ctx.target.icmp(IcmpPred::Eq, lv, rv),
        SpecBinOp::Ne => ctx.target.icmp(IcmpPred::Ne, lv, rv),
        SpecBinOp::Lt => ctx.target.icmp(IcmpPred::Ult, lv, rv),
        SpecBinOp::Le => ctx.target.icmp(IcmpPred::Ule, lv, rv),
        SpecBinOp::Gt => ctx.target.icmp(IcmpPred::Ugt, lv, rv),
        SpecBinOp::Ge => ctx.target.icmp(IcmpPred::Uge, lv, rv),
        SpecBinOp::And => ctx.target.and(lv, rv),
        SpecBinOp::Or => ctx.target.or(lv, rv),
    }
}

fn lower_unop<T: LirTarget>(op: SpecUnaryOp, v: T::Value, ctx: &mut LowerCtx<T>) -> T::Value {
    match op {
        SpecUnaryOp::Not => ctx.target.not(v),
        SpecUnaryOp::Neg => {
            let ty = ctx.target.value_scalar_type(&v);
            let zero = ctx.target.iconst(ty, 0);
            ctx.target.sub(zero, v)
        }
        // References / dereferences are transparent in value-semantics LIR.
        SpecUnaryOp::Deref | SpecUnaryOp::Ref | SpecUnaryOp::RefMut => v,
    }
}

// ============================================================================
// If/else lowering
// ============================================================================

fn lower_if<T: LirTarget>(
    cond_expr: &IrExpr,
    then_branch: &IrBlock,
    else_branch: Option<&IrExpr>,
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let cond_val = into_scalar(lower_expr(cond_expr, ctx), "if condition");

    let then_block = ctx.target.create_block();
    let else_block = ctx.target.create_block();

    // Branch to then/else — join block created lazily below.
    ctx.target.branch(cond_val, then_block.clone(), &[], else_block.clone(), &[]);

    // Lower the then-branch first so we discover N (the result scalar count).
    ctx.target.switch_to_block(then_block.clone());
    ctx.current_block = then_block;
    let then_vals = lower_block(then_branch, ctx);
    let n = then_vals.len();

    // Recover the scalar LirType of each then-value to use as join-block param types.
    let scalar_tys: Vec<LirType> = then_vals.iter()
        .map(|v| ctx.target.value_scalar_type(v))
        .collect();

    // Create the join block now that we know N.
    let join_block = ctx.target.create_block();
    let join_params: Vec<T::Value> = scalar_tys.iter()
        .map(|ty| ctx.target.add_block_param(join_block.clone(), ty.clone()))
        .collect();

    ctx.target.jump(join_block.clone(), &then_vals);

    // Lower the else-branch.
    ctx.target.switch_to_block(else_block.clone());
    ctx.current_block = else_block;
    let else_vals: Vec<T::Value> = if let Some(else_expr) = else_branch {
        lower_expr(else_expr, ctx)
    } else {
        // No else clause — produce N zeroed scalars matching the then-branch shape.
        scalar_tys.iter().map(|ty| ctx.target.iconst(ty.clone(), 0)).collect()
    };
    assert_eq!(
        else_vals.len(), n,
        "if/else branches produce different numbers of scalars ({n} vs {})", else_vals.len()
    );
    ctx.target.jump(join_block.clone(), &else_vals);

    ctx.target.switch_to_block(join_block.clone());
    ctx.current_block = join_block;
    join_params
}

// ============================================================================
// Phase 2: field access
// ============================================================================

fn lower_field<T: LirTarget>(base: &IrExpr, field: &str, ctx: &mut LowerCtx<T>) -> Vec<T::Value> {
    let base_ir_ty = ctx
        .infer_type(base)
        .unwrap_or_else(|| panic!("could not infer type for field access .{field}"));

    // Handle tuple field access (`.0`, `.1`, ...).
    if let IrType::Tuple(ref elems) = base_ir_ty {
        let idx: usize = field.parse().unwrap_or_else(|_| {
            panic!("tuple field access with non-numeric field `.{field}`")
        });
        assert!(idx < elems.len(), "tuple index {idx} out of bounds (tuple has {} elements)", elems.len());

        // Compute scalar offset = sum of flattened widths of elements 0..idx.
        let offset: usize = elems[..idx]
            .iter()
            .map(|ty| flatten_count(&ctx.registry.ir_type_to_lir(ty), ctx.registry))
            .sum();
        let width = flatten_count(&ctx.registry.ir_type_to_lir(&elems[idx]), ctx.registry);

        let base_vals = lower_expr(base, ctx);
        return base_vals[offset..offset + width].to_vec();
    }

    let struct_kind = extract_struct_kind(&base_ir_ty).unwrap_or_else(|| {
        panic!("field .{field} on non-struct type {:?}", base_ir_ty)
    });

    let struct_id = ctx.registry.id_for(struct_kind).unwrap_or_else(|| {
        panic!("struct {:?} not in registry for field .{field}", struct_kind)
    });

    let field_idx = ctx.registry.field_index(struct_id, field);
    let offset = struct_field_scalar_offset(ctx.registry, struct_id, field_idx);
    let width  = struct_field_scalar_width(ctx.registry, struct_id, field_idx);

    let base_vals = lower_expr(base, ctx);
    base_vals[offset .. offset + width].to_vec()
}

// ============================================================================
// Phase 2: struct construction
// ============================================================================

fn lower_struct_expr<T: LirTarget>(
    kind: &StructKind,
    fields: &[(String, IrExpr)],
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let struct_id = ctx.registry.id_for(kind).unwrap_or_else(|| {
        panic!("struct {:?} not in registry", kind)
    });

    let field_map: BTreeMap<&str, &IrExpr> =
        fields.iter().map(|(n, e)| (n.as_str(), e)).collect();

    let decl_names: Vec<String> = ctx.registry.field_names(struct_id).to_vec();
    // Concatenate flat scalar lists for all fields in declaration order.
    decl_names.iter().flat_map(|name| {
        let expr: &IrExpr = field_map.get(name.as_str()).copied()
            .or_else(|| fields.iter().find(|(n, _)| n == name).map(|(_, e)| e))
            .unwrap_or_else(|| panic!("StructExpr missing field '{name}'"));
        lower_expr(expr, ctx)
    }).collect()
}

// ============================================================================
// Phase 2: fixed-size array literal
// ============================================================================

fn lower_fixed_array<T: LirTarget>(elems: &[IrExpr], ctx: &mut LowerCtx<T>) -> Vec<T::Value> {
    // Concatenate the flat scalar lists of all elements directly — no arr_new.
    elems.iter().flat_map(|e| lower_expr(e, ctx)).collect()
}

// ============================================================================
// Phase 2: ArrayGenerate (from_fn / closure-based array fill)
// ============================================================================

fn lower_array_generate<T: LirTarget>(
    _elem_ty: Option<&IrType>,
    len: &ArrayLength,
    index_var: &str,
    body: &IrExpr,
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let n = const_len(len);

    // Unroll: evaluate body once per index, concatenate flat scalar lists.
    let result: Vec<T::Value> = (0..n)
        .flat_map(|k| {
            let idx_val = ctx.target.iconst(LirType::U64, k as i64);
            ctx.env.insert(index_var.to_owned(), vec![idx_val]);
            ctx.env_types.insert(
                index_var.to_owned(),
                IrType::Primitive(PrimitiveType::Usize),
            );
            lower_expr(body, ctx)
        })
        .collect();

    ctx.env.remove(index_var);
    ctx.env_types.remove(index_var);
    result
}

// ============================================================================
// Phase 2: RawMap (element-wise unary array op)
// ============================================================================

fn lower_raw_map<T: LirTarget>(
    receiver: &IrExpr,
    elem_var: &IrPattern,
    body: &IrExpr,
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let recv_ty = ctx
        .infer_type(receiver)
        .unwrap_or_else(|| panic!("RawMap: could not infer receiver type"));
    let (elem_ir_ty, n) = array_elem_and_len(&recv_ty);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let recv_vals = lower_expr(receiver, ctx); // n * elem_width scalars

    (0..n).flat_map(|k| {
        let elem_vals = recv_vals[k * elem_width .. (k + 1) * elem_width].to_vec();
        bind_map_pattern(elem_var, elem_vals, &elem_ir_ty, ctx);
        lower_expr(body, ctx)
    }).collect()
}

// ============================================================================
// Phase 2: RawZip (element-wise binary array op)
// ============================================================================

fn lower_raw_zip<T: LirTarget>(
    left: &IrExpr,
    right: &IrExpr,
    left_var: &IrPattern,
    right_var: &IrPattern,
    body: &IrExpr,
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let left_ty = ctx
        .infer_type(left)
        .unwrap_or_else(|| panic!("RawZip: could not infer left type"));
    let (elem_ir_ty, n) = array_elem_and_len(&left_ty);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let left_vals  = lower_expr(left, ctx);
    let right_vals = lower_expr(right, ctx);

    (0..n).flat_map(|k| {
        let lv = left_vals[k * elem_width .. (k + 1) * elem_width].to_vec();
        let rv = right_vals[k * elem_width .. (k + 1) * elem_width].to_vec();
        bind_map_pattern(left_var, lv, &elem_ir_ty, ctx);
        bind_map_pattern(right_var, rv, &elem_ir_ty, ctx);
        lower_expr(body, ctx)
    }).collect()
}

fn bind_map_pattern<T: LirTarget>(
    pattern: &IrPattern,
    vals: Vec<T::Value>,
    ir_ty: &IrType,
    ctx: &mut LowerCtx<T>,
) {
    match pattern {
        IrPattern::Ident { name, .. } => {
            ctx.env.insert(name.clone(), vals);
            ctx.env_types.insert(name.clone(), ir_ty.clone());
        }
        IrPattern::Wild => {}
        other => unimplemented!("bind_map_pattern: {:?}", other),
    }
}

fn array_elem_and_len(ty: &IrType) -> (IrType, usize) {
    match ty {
        IrType::Array { elem, len, .. } => (*elem.clone(), const_len(len)),
        IrType::Reference { elem, .. } => array_elem_and_len(elem),
        other => panic!("expected array type, got {:?}", other),
    }
}

// ============================================================================
// Slice-reference helpers
// ============================================================================

/// Check if an IrType is a reference to a slice (`&[T]` or `&mut [T]`).
fn is_slice_ref(ty: &IrType) -> bool {
    matches!(ty,
        IrType::Reference { elem, .. }
            if matches!(elem.as_ref(), IrType::Array { kind: ArrayKind::Slice, .. })
    )
}

/// Extract the element type from a `Reference<Slice<T>>`.
/// Panics if `ty` is not a slice reference.
fn slice_ref_elem(ty: &IrType) -> IrType {
    match ty {
        IrType::Reference { elem, .. } => match elem.as_ref() {
            IrType::Array { elem: inner, .. } => *inner.clone(),
            _ => panic!("slice_ref_elem: not a slice"),
        },
        _ => panic!("slice_ref_elem: not a reference"),
    }
}

// ============================================================================
// Phase 2: array index (runtime mux tree)
// ============================================================================

fn lower_index<T: LirTarget>(
    base: &IrExpr,
    index: &IrExpr,
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let base_ir_ty = ctx
        .infer_type(base)
        .unwrap_or_else(|| panic!("Index: could not infer base type"));

    // Pointer-based indexing: Reference<Slice<T>> → Ptr(T) in LIR.
    // Use ptr_index_load instead of the flat mux tree.
    if is_slice_ref(&base_ir_ty) {
        let elem_ir_ty = slice_ref_elem(&base_ir_ty);
        let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
        let ptr_vals = lower_expr(base, ctx);
        let ptr = ptr_vals.into_iter().next().expect("pointer should be a single scalar");
        let idx_val = into_scalar(lower_expr(index, ctx), "array index");
        return ctx.target.ptr_index_load(ptr, idx_val, &elem_lir_ty);
    }

    let (elem_ir_ty, n) = array_elem_and_len(&base_ir_ty);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let arr_vals = lower_expr(base, ctx);   // n * elem_width scalars
    let idx_val  = into_scalar(lower_expr(index, ctx), "array index");

    // Build a select mux tree for each scalar position within the element.
    // result[j] = select chain over arr_vals[0*ew+j], arr_vals[1*ew+j], ...
    (0..elem_width).map(|j| {
        let mut result = arr_vals[j].clone(); // element 0's j-th scalar
        for k in 1..n {
            let k_val = ctx.target.iconst(LirType::U64, k as i64);
            let cond = ctx.target.icmp(IcmpPred::Eq, idx_val.clone(), k_val);
            result = ctx.target.select(cond, arr_vals[k * elem_width + j].clone(), result);
        }
        result
    }).collect()
}

// ============================================================================
// Phase 2: assignment (storage writes, variable updates)
// ============================================================================

fn lower_assign<T: LirTarget>(
    left: &IrExpr,
    right: &IrExpr,
    ctx: &mut LowerCtx<T>,
) {
    match left {
        // Assignment to an indexed location: base[index] = rhs
        IrExpr::Index { base, index } => {
            let base_ir_ty = ctx.infer_type(base)
                .unwrap_or_else(|| panic!("Assign: could not infer base type"));

            if is_slice_ref(&base_ir_ty) {
                // Pointer-based store: ptr[idx] = pack(rhs_vals)
                let elem_ir_ty = slice_ref_elem(&base_ir_ty);
                let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
                let ptr_vals = lower_expr(base, ctx);
                let ptr = ptr_vals.into_iter().next().expect("pointer should be a single scalar");
                let idx_val = into_scalar(lower_expr(index, ctx), "assign index");
                let rhs_vals = lower_expr(right, ctx);
                ctx.target.ptr_index_store(ptr, idx_val, &rhs_vals, &elem_lir_ty);
            } else {
                // In-memory array: update env with new values.
                // For flat-scalar arrays this replaces the slice at the right index.
                let (elem_ir_ty, _n) = array_elem_and_len(&base_ir_ty);
                let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
                let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

                let idx_val = into_scalar(lower_expr(index, ctx), "assign index");
                let rhs_vals = lower_expr(right, ctx);

                // We need the variable name to update env.
                if let IrExpr::Var(name) = base.as_ref() {
                    let mut arr_vals = ctx.env.get(name).cloned()
                        .unwrap_or_else(|| panic!("undefined variable: {name}"));
                    // For each element position, conditionally update using select.
                    let n = arr_vals.len() / elem_width;
                    for k in 0..n {
                        let k_val = ctx.target.iconst(LirType::U64, k as i64);
                        let cond = ctx.target.icmp(IcmpPred::Eq, idx_val.clone(), k_val);
                        for j in 0..elem_width {
                            let old = arr_vals[k * elem_width + j].clone();
                            let new = rhs_vals[j].clone();
                            arr_vals[k * elem_width + j] = ctx.target.select(cond.clone(), new, old);
                        }
                    }
                    ctx.env.insert(name.clone(), arr_vals);
                } else {
                    unimplemented!("assign to non-variable indexed base: {:?}", base);
                }
            }
        }
        _ => unimplemented!("lower_assign: unsupported lhs {:?}", left),
    }
}

// ============================================================================
// Phase 2: BoundedLoop
// ============================================================================

fn lower_bounded_loop<T: LirTarget>(
    var: &str,
    start: &IrExpr,
    end: &IrExpr,
    inclusive: bool,
    body: &IrBlock,
    ctx: &mut LowerCtx<T>,
) {
    // Strategy: loop_header(counter: U64, limit: U64)
    //
    // before:
    //   start_val = lower(start)
    //   end_val   = lower(end)
    //   if inclusive: limit = end_val + 1, else limit = end_val
    //   jump loop_header(start_val, limit)
    //
    // loop_header(counter, limit):
    //   cmp = counter < limit
    //   branch cmp → body_block, done_block
    //
    // body_block:
    //   env[var] = counter
    //   lower(body)
    //   next = counter + 1
    //   jump loop_header(next, limit)
    //
    // done_block:
    //   (unit)

    let loop_header = ctx.target.create_block();
    let body_block = ctx.target.create_block();
    let done_block = ctx.target.create_block();

    let counter = ctx.target.add_block_param(loop_header.clone(), LirType::U64);
    let limit = ctx.target.add_block_param(loop_header.clone(), LirType::U64);

    // Before block: compute start and end.
    let start_val = into_scalar(lower_expr(start, ctx), "loop start");
    let end_val   = into_scalar(lower_expr(end, ctx), "loop end");
    let limit_val = if inclusive {
        let one = ctx.target.iconst(LirType::U64, 1);
        ctx.target.add(end_val, one)
    } else {
        end_val
    };
    // Ensure values are U64 (the loop uses U64 counters).
    let start_u64 = ctx.target.zext(start_val, LirType::U64);
    ctx.target.jump(loop_header.clone(), &[start_u64, limit_val.clone()]);

    // Loop header.
    ctx.target.switch_to_block(loop_header.clone());
    ctx.current_block = loop_header.clone();
    let cmp = ctx.target.icmp(IcmpPred::Ult, counter.clone(), limit.clone());
    ctx.target.branch(cmp, body_block.clone(), &[], done_block.clone(), &[]);

    // Body block.
    ctx.target.switch_to_block(body_block.clone());
    ctx.current_block = body_block;
    ctx.env.insert(var.to_owned(), vec![counter.clone()]);
    ctx.env_types.insert(var.to_owned(), IrType::Primitive(PrimitiveType::Usize));
    lower_block(body, ctx);
    let one = ctx.target.iconst(LirType::U64, 1);
    let next = ctx.target.add(counter, one);
    ctx.target.jump(loop_header, &[next, limit_val]);

    // Done block.
    ctx.target.switch_to_block(done_block.clone());
    ctx.current_block = done_block;

    // Clean up loop variable from env.
    ctx.env.remove(var);
    ctx.env_types.remove(var);
}

// ============================================================================
// Phase 2: method calls
// ============================================================================

fn lower_method_call<T: LirTarget>(
    receiver: &IrExpr,
    method: &MethodKind,
    type_args: &[IrType],
    args: &[IrExpr],
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    match method {
        // `.clone()` and `.deref()` are transparent in value semantics.
        MethodKind::Known(StdMethod::Clone | StdMethod::Deref) => lower_expr(receiver, ctx),

        // Reference methods — transparent.
        MethodKind::Known(StdMethod::AsRef | StdMethod::AsSlice) => lower_expr(receiver, ctx),

        // Other methods → extern call.
        MethodKind::Known(m) => {
            lower_method_extern(receiver, m.as_str(), type_args, args, ctx)
        }
        MethodKind::Other(name) => {
            // Guardrails: ident-char validity + misrouting check.
            debug_assert!(
                name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                "LIR lower: method name is not a valid identifier: {name:?}"
            );
            debug_assert!(
                StdMethod::try_from_str(name.as_str()).is_none(),
                "LIR lower: {name:?} is a well-known StdMethod but was constructed as \
                 MethodKind::Other; use MethodKind::from_str(\"{name}\") or \
                 MethodKind::Known(StdMethod::…) instead"
            );
            lower_method_extern(receiver, name, type_args, args, ctx)
        }

        other => unimplemented!("lower_method_call: {:?}", other),
    }
}

fn lower_method_extern<T: LirTarget>(
    receiver: &IrExpr,
    method_name: &str,
    _type_args: &[IrType],
    args: &[IrExpr],
    ctx: &mut LowerCtx<T>,
) -> Vec<T::Value> {
    let extern_name = if ctx.hash_suffix.is_empty() {
        method_name.to_owned()
    } else {
        format!("{}_{}", method_name, ctx.hash_suffix)
    };

    // Collect ABI types and flat scalar values for receiver + args.
    let recv_ir_ty = ctx.infer_type(receiver);
    let recv_lir_ty = recv_ir_ty.as_ref()
        .map(|t| ctx.registry.ir_type_to_lir(t))
        .unwrap_or(LirType::U64);

    let mut arg_tys: Vec<LirType> = vec![recv_lir_ty];
    let mut flat_args: Vec<T::Value> = lower_expr(receiver, ctx);

    for a in args {
        let a_ir_ty = ctx.infer_type(a);
        let a_lir_ty = a_ir_ty.as_ref()
            .map(|t| ctx.registry.ir_type_to_lir(t))
            .unwrap_or(LirType::U64);
        arg_tys.push(a_lir_ty);
        flat_args.extend(lower_expr(a, ctx));
    }

    let ret_lir_ty = recv_ir_ty.map(|t| ctx.registry.ir_type_to_lir(&t));
    ctx.target.call_extern(&extern_name, &arg_tys, &flat_args, ret_lir_ty)
}

// ============================================================================
// Phase 2: free function calls
// ============================================================================

fn lower_call<T: LirTarget>(func: &IrExpr, args: &[IrExpr], ctx: &mut LowerCtx<T>) -> Vec<T::Value> {
    if let IrExpr::Path { segments, .. } = func {
        if segments.len() >= 2
            && (segments[segments.len() - 2] == "Array"
                || segments[segments.len() - 2] == "GenericArray")
            && segments[segments.len() - 1] == "from_fn"
        {
            unimplemented!(
                "Array::from_fn call — use IrExpr::ArrayGenerate instead (run lowering_dyn)"
            );
        }
    }

    let func_name = match func {
        IrExpr::Path { segments, .. } => segments.join("_"),
        IrExpr::Var(name) => name.clone(),
        other => unimplemented!("lower_call: non-path func {:?}", other),
    };

    // ---- Dispatch oracle / action / rng ------------------------------------
    if let Some(info) = ctx.external_fns.get(&func_name) {
        return match info.kind {
            ExternalKind::Oracle => {
                // Oracle: all call-site args are the actual function args.
                // Use declared parameter types from the function signature.
                let arg_tys: Vec<LirType> = info.param_tys.clone();
                let mut flat_args: Vec<T::Value> = Vec::new();
                for a in args {
                    flat_args.extend(lower_expr(a, ctx));
                }
                let ret_tys: Vec<LirType> = info.return_type.iter().cloned().collect();
                ctx.target.oracle(&func_name, &arg_tys, &flat_args, &ret_tys)
            }
            ExternalKind::Action => {
                // Action ABI (encoded in the function declaration):
                //   params[0]                 = guard (Bool)
                //   params[1 .. 1+n_fb]       = fallback values (one per return element)
                //   params[1+n_fb ..]          = real arguments
                //
                // The call-site args match the declaration 1:1.
                let n_declared = info.param_tys.len();

                // Determine n_fb from the IR-level return type.
                let n_fb = match ctx.ir_func_ret_types.get(&func_name) {
                    Some(IrType::Tuple(elems)) => elems.len(),
                    Some(_) => 1,
                    None => 0,
                };

                assert!(
                    args.len() == n_declared,
                    "action '{func_name}': call has {} args but declaration has {n_declared} params",
                    args.len()
                );

                // Guard (first arg).
                let guard_vals = lower_expr(&args[0], ctx);
                let guard = guard_vals.into_iter().next().expect("action guard must be a scalar");

                // Fallbacks (next n_fb args).
                let mut flat_fallbacks: Vec<T::Value> = Vec::new();
                for a in &args[1..1 + n_fb] {
                    flat_fallbacks.extend(lower_expr(a, ctx));
                }

                // Real arguments (remaining args).
                // Use declared parameter types from the function signature
                // rather than inferring from expressions — infer_type cannot
                // handle complex expressions like RawMap.
                let arg_tys: Vec<LirType> = info.param_tys[1 + n_fb..].to_vec();
                let mut flat_args: Vec<T::Value> = Vec::new();
                for a in args[1 + n_fb..].iter() {
                    flat_args.extend(lower_expr(a, ctx));
                }

                let ret_tys: Vec<LirType> = info.return_type.iter().cloned().collect();
                ctx.target.action(&func_name, guard, &arg_tys, &flat_args, &flat_fallbacks, &ret_tys)
            }
            ExternalKind::Rng => {
                // Rng: no arguments, return type from declaration.
                let ret_ty = info.return_type.clone().unwrap_or(LirType::U64);
                vec![ctx.target.rng(ret_ty)]
            }
            _ => unreachable!(),
        };
    }

    // ---- Default: call_extern ----------------------------------------------
    // Look up the called function's signature for return-type inference.
    // Without this, intra-module calls would discard their return value.
    let ret_ty = ctx.func_sigs.get(&func_name)
        .and_then(|sig| sig.return_type.clone());

    let mut arg_tys: Vec<LirType> = Vec::new();
    let mut flat_args: Vec<T::Value> = Vec::new();
    for a in args {
        let a_ir_ty = ctx.infer_type(a);
        let a_lir_ty = a_ir_ty.as_ref()
            .map(|t| ctx.registry.ir_type_to_lir(t))
            .unwrap_or(LirType::U64);
        arg_tys.push(a_lir_ty);
        flat_args.extend(lower_expr(a, ctx));
    }

    ctx.target.call_extern(&func_name, &arg_tys, &flat_args, ret_ty)
}

// ============================================================================
// CFG Module lowering — IrCfgModule → LirTarget (direct block map)
// ============================================================================

/// Lower all functions in an `IrCfgModule` to `target`.
///
/// Auxiliary functions (flat bodies from linked specs) are lowered through the
/// normal `lower_function_in_module` path.  CFG-structured circuit functions
/// map each `IrCfgBlock` directly to a LIR block, using the target's native
/// `create_block`/`jump`/`branch`/`ret`.
pub fn lower_cfg_module<T: LirTarget>(module: &IrCfgModule, target: &mut T) {
    lower_cfg_module_with_opts(module, target, "", true);
}

/// Like `lower_cfg_module` but with control over hash suffix and auxiliary
/// function lowering.
///
/// When `include_auxiliary` is `false`, only CFG functions are lowered.
/// This is useful for backends (like C) that cannot handle the Rust-specific
/// constructs typically found in linked spec auxiliary functions.
pub fn lower_cfg_module_with_opts<T: LirTarget>(
    module: &IrCfgModule,
    target: &mut T,
    hash_suffix: &str,
    include_auxiliary: bool,
) {
    // Build struct registry from the CFG module's structs.
    // We need a temporary flat IrModule to reuse `build_struct_registry`.
    let flat: IrModule<IrFunction> = IrModule {
        name: module.name.clone(),
        structs: module.structs.clone(),
        enums: module.enums.clone(),
        traits: module.traits.clone(),
        impls: module.impls.clone(),
        functions: module.functions.iter().filter_map(|f| {
            if let IrAnyFunction::Flat(f) = f { Some(f.clone()) } else { None }
        }).collect(),
        type_aliases: module.type_aliases.clone(),
    };
    let mut registry = structs::build_struct_registry(&flat, target);
    // When skipping auxiliary functions, enable lenient mode so that external
    // types (e.g. TFHE scheme types not defined in the module) are treated
    // as opaque rather than causing a panic.
    if !include_auxiliary {
        registry.lenient = true;
    }

    // Pre-register synthetic structs for any tuple types appearing in function
    // signatures.  This must happen before `ir_type_to_lir` encounters tuples.
    for func in &module.functions {
        let (params, return_type) = match func {
            IrAnyFunction::Flat(f) => (&f.params, &f.return_type),
            IrAnyFunction::Cfg(f) => (&f.params, &f.return_type),
        };
        for p in params {
            structs::register_tuples_in_type(&p.ty, &mut registry, target);
        }
        if let Some(rt) = return_type {
            structs::register_tuples_in_type(rt, &mut registry, target);
        }
    }

    // Build external-fn and func-sigs tables from both flat and CFG functions.
    let mut external_fns: BTreeMap<String, ExternalFnInfo> = BTreeMap::new();
    let mut func_sigs: BTreeMap<String, FuncSigInfo> = BTreeMap::new();
    // IR-level return types for type inference at call sites.
    let mut ir_func_ret_types: BTreeMap<String, IrType> = BTreeMap::new();

    // Always register metadata (external_fns, func_sigs, ir_func_ret_types)
    // from all functions — even when `include_auxiliary` is false for flat bodies.
    // The CFG functions call action/oracle/rng functions defined as flat entries,
    // so we need their signatures and external-kind info for lowering.
    // `include_auxiliary` only controls whether we *lower flat function bodies*.
    for func in &module.functions {
        let (name, params, return_type, external_kind) = match func {
            IrAnyFunction::Flat(f) => (&f.name, &f.params, &f.return_type, f.external_kind),
            IrAnyFunction::Cfg(f) => (&f.name, &f.params, &f.return_type, f.external_kind),
        };
        let param_tys: Vec<LirType> = params.iter().map(|p| registry.ir_type_to_lir(&p.ty)).collect();
        let ret_lir = return_type.as_ref().map(|t| registry.ir_type_to_lir(t));
        func_sigs.insert(name.clone(), FuncSigInfo { param_tys: param_tys.clone(), return_type: ret_lir.clone() });
        if let Some(rt) = return_type {
            ir_func_ret_types.insert(name.clone(), rt.clone());
        }
        if matches!(external_kind, ExternalKind::Oracle | ExternalKind::Action | ExternalKind::Rng) {
            external_fns.insert(name.clone(), ExternalFnInfo {
                kind: external_kind,
                param_tys,
                return_type: ret_lir,
            });
        }
    }

    // Lower flat functions (auxiliary spec functions).
    if include_auxiliary {
        for func in &module.functions {
            if let IrAnyFunction::Flat(func) = func {
                if func.external_kind != ExternalKind::Normal {
                    continue;
                }
                lower_function_in_module(
                    func, target, &registry, hash_suffix, &module.structs, &external_fns, &func_sigs,
                );
            }
        }
    }

    // Lower CFG functions.
    for func in &module.functions {
        if let IrAnyFunction::Cfg(func) = func {
            if func.external_kind != ExternalKind::Normal {
                continue;
            }
            lower_cfg_function(
                func, target, &registry, hash_suffix, &module.structs, &external_fns, &func_sigs, &ir_func_ret_types,
            );
        }
    }
}

/// Lower a single `IrCfgFunction` by mapping each CFG block to a LIR block.
fn lower_cfg_function<T: LirTarget>(
    func: &IrCfgFunction,
    target: &mut T,
    registry: &StructRegistry,
    hash_suffix: &str,
    module_structs: &[volar_compiler::ir::IrStruct],
    external_fns: &BTreeMap<String, ExternalFnInfo>,
    func_sigs: &BTreeMap<String, FuncSigInfo>,
    ir_func_ret_types: &BTreeMap<String, IrType>,
) {
    let blocks = &func.body.blocks;

    // Compute LIR param types and return type.
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty))
        .collect();
    let ret_ty = func.return_type.as_ref().map(|t| registry.ir_type_to_lir(t));

    // Begin the function — gets the entry block (block 0) and its parameter values.
    let (entry, param_val_groups) = target.begin_function(&func.name, &param_lir_tys, ret_ty);

    // Create LIR blocks for each CFG block.  Block 0 = entry.
    let mut lir_blocks: Vec<T::Block> = Vec::new();
    lir_blocks.push(entry.clone());
    for _ in 1..blocks.len() {
        lir_blocks.push(target.create_block());
    }

    // Add block parameters for blocks 1.. (block 0 uses function params).
    // Collect the resulting LIR values for use when binding params at block start.
    let mut block_param_vals: Vec<Vec<Vec<T::Value>>> = Vec::new();
    block_param_vals.push(Vec::new()); // block 0 — no block params, uses fn params
    for (bidx, blk) in blocks.iter().enumerate().skip(1) {
        let mut param_groups = Vec::new();
        for param in &blk.params {
            let lir_ty = registry.ir_type_to_lir(&param.ty);
            // add_block_param returns a single LIR value; for aggregate types
            // we need to flatten (same as begin_function).
            let scalar_tys = structs::flatten_scalar_types(&lir_ty, registry);
            let mut vals = Vec::new();
            for sty in &scalar_tys {
                vals.push(target.add_block_param(lir_blocks[bidx].clone(), sty.clone()));
            }
            param_groups.push(vals);
        }
        block_param_vals.push(param_groups);
    }

    // Now emit each block.
    for (bidx, blk) in blocks.iter().enumerate() {
        target.switch_to_block(lir_blocks[bidx].clone());

        // Set up LowerCtx with the correct params in env.
        let named_params: Vec<(String, Vec<T::Value>, IrType)> = if bidx == 0 {
            // Block 0: use function params.
            func.params
                .iter()
                .zip(param_val_groups.iter())
                .map(|(p, vals)| (p.name.clone(), vals.clone(), p.ty.clone()))
                .collect()
        } else {
            // Blocks 1..: use block params.
            blk.params
                .iter()
                .zip(block_param_vals[bidx].iter())
                .map(|(p, vals)| (p.name.clone(), vals.clone(), p.ty.clone()))
                .collect()
        };

        let mut ctx = LowerCtx::new(
            target,
            lir_blocks[bidx].clone(),
            named_params,
            registry,
            hash_suffix.to_owned(),
            module_structs,
        );
        ctx.external_fns = external_fns;
        ctx.func_sigs = func_sigs;
        ctx.ir_func_ret_types = ir_func_ret_types;

        // Lower statements.
        for stmt in &blk.stmts {
            lower_stmt(stmt, &mut ctx);
        }

        // Emit terminator.
        lower_cfg_terminator(&blk.terminator, &lir_blocks, &mut ctx, registry);
    }

    target.end_function();
}

/// Emit LIR instructions for a CFG terminator.
fn lower_cfg_terminator<T: LirTarget>(
    term: &IrCfgTerminator,
    lir_blocks: &[T::Block],
    ctx: &mut LowerCtx<T>,
    registry: &StructRegistry,
) {
    match term {
        IrCfgTerminator::Return(None) => {
            ctx.target.ret(&[]);
        }
        IrCfgTerminator::Return(Some(expr)) => {
            let vals = lower_expr(expr, ctx);
            ctx.target.ret(&vals);
        }
        IrCfgTerminator::Goto(jump) => {
            let args = lower_jump_args(jump, ctx);
            ctx.target.jump(lir_blocks[jump.target].clone(), &args);
        }
        IrCfgTerminator::CondGoto { cond, then_, else_ } => {
            let cond_vals = lower_expr(cond, ctx);
            let cond_val = into_scalar(cond_vals, "CondGoto condition");
            let then_args = lower_jump_args(then_, ctx);
            let else_args = lower_jump_args(else_, ctx);
            ctx.target.branch(
                cond_val,
                lir_blocks[then_.target].clone(),
                &then_args,
                lir_blocks[else_.target].clone(),
                &else_args,
            );
        }
    }
}

/// Lower the argument expressions for a CFG jump, flattening to scalar values.
fn lower_jump_args<T: LirTarget>(jump: &IrCfgJump, ctx: &mut LowerCtx<T>) -> Vec<T::Value> {
    let mut all = Vec::new();
    for arg in &jump.args {
        all.extend(lower_expr(arg, ctx));
    }
    all
}
