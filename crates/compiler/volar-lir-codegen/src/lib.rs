// @reliability: normal
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

use std::collections::BTreeMap;
use volar_compiler::ir::{
    ArrayLength, IrBlock, IrClosureParam, IrExpr, IrFunction, IrLit, IrModule, IrPattern, IrStmt,
    IrType, MethodKind, PrimitiveType, SpecBinOp, SpecUnaryOp, StructKind,
};
use volar_lir::{IcmpPred, LirTarget, LirType};

use structs::{
    StructRegistry, flatten_count, flatten_scalar_types,
    struct_field_scalar_offset, struct_field_scalar_width, primitive_to_lir,
};

// Unwrap a single-element Vec into a scalar, panicking if the vec has != 1 element.
fn into_scalar<V: Copy>(vals: Vec<V>, context: &str) -> V {
    assert_eq!(vals.len(), 1, "expected scalar at {context}, got {} values", vals.len());
    vals[0]
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
        LowerCtx { target, env, env_types, current_block: entry, registry, hash_suffix, module_structs }
    }

    /// Infer the IrType of an expression using env_types and module struct defs.
    /// Returns `None` for unsupported or uninferable expressions.
    fn infer_type(&self, expr: &IrExpr) -> Option<IrType> {
        match expr {
            IrExpr::Var(name) => self.env_types.get(name).cloned(),

            IrExpr::Field { base, field } => {
                let base_ty = self.infer_type(base)?;
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
                match base_ty {
                    IrType::Array { elem, .. } => Some(*elem),
                    _ => None,
                }
            }

            IrExpr::MethodCall { receiver, method: MethodKind::Std(m), .. }
                if m == "clone" || m == "deref" =>
            {
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

// ============================================================================
// Public API
// ============================================================================

/// Lower all functions in `module` to `target`.
///
/// The module should be monomorphized before calling this (via `mono::monomorphize_module`).
/// Struct types are registered via `structs::build_struct_registry` before lowering functions.
pub fn lower_module<T: LirTarget>(module: &IrModule, target: &mut T) {
    lower_module_with_opts(module, target, "");
}

/// Like `lower_module` but with an explicit crypto hash suffix (for extern names).
pub fn lower_module_with_opts<T: LirTarget>(
    module: &IrModule,
    target: &mut T,
    hash_suffix: &str,
) {
    let registry = structs::build_struct_registry(module, target);
    for func in &module.functions {
        lower_function_with_registry(func, target, &registry, hash_suffix, &module.structs);
    }
}

/// Lower a single `IrFunction` to `target` (no struct support).
///
/// Use `lower_module_with_opts` for modules that use structs or arrays.
pub fn lower_function<T: LirTarget>(func: &IrFunction, target: &mut T) {
    let empty_registry = StructRegistry::empty();
    lower_function_with_registry(func, target, &empty_registry, "", &[]);
}

fn lower_function_with_registry<T: LirTarget>(
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
    target.switch_to_block(entry);

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
                let vals = lower_expr(init_expr, ctx);
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
        IrExpr::Lit(lit) => vec![lower_lit(lit, ctx)],

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
            let v = into_scalar(lower_expr(inner, ctx), "unary operand");
            vec![lower_unop(*op, v, ctx)]
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

        other => unimplemented!("lower_expr: unsupported expr {:?}", other),
    }
}

// ============================================================================
// Literal lowering
// ============================================================================

fn lower_lit<T: LirTarget>(lit: &IrLit, ctx: &mut LowerCtx<T>) -> T::Value {
    match lit {
        IrLit::Int(n) => ctx.target.iconst(LirType::U64, *n as i64),
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
            let zero = ctx.target.iconst(LirType::U64, 0);
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
    ctx.target.branch(cond_val, then_block, &[], else_block, &[]);

    // Lower the then-branch first so we discover N (the result scalar count).
    ctx.target.switch_to_block(then_block);
    ctx.current_block = then_block;
    let then_vals = lower_block(then_branch, ctx);
    let n = then_vals.len();

    // Recover the scalar LirType of each then-value to use as join-block param types.
    let scalar_tys: Vec<LirType> = then_vals.iter()
        .map(|&v| ctx.target.value_scalar_type(v))
        .collect();

    // Create the join block now that we know N.
    let join_block = ctx.target.create_block();
    let join_params: Vec<T::Value> = scalar_tys.iter()
        .map(|ty| ctx.target.add_block_param(join_block, ty.clone()))
        .collect();

    ctx.target.jump(join_block, &then_vals);

    // Lower the else-branch.
    ctx.target.switch_to_block(else_block);
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
    ctx.target.jump(join_block, &else_vals);

    ctx.target.switch_to_block(join_block);
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
    let (elem_ir_ty, n) = array_elem_and_len(&base_ir_ty);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let arr_vals = lower_expr(base, ctx);   // n * elem_width scalars
    let idx_val  = into_scalar(lower_expr(index, ctx), "array index");

    // Build a select mux tree for each scalar position within the element.
    // result[j] = select chain over arr_vals[0*ew+j], arr_vals[1*ew+j], ...
    (0..elem_width).map(|j| {
        let mut result = arr_vals[j]; // element 0's j-th scalar
        for k in 1..n {
            let k_val = ctx.target.iconst(LirType::U64, k as i64);
            let cond = ctx.target.icmp(IcmpPred::Eq, idx_val, k_val);
            result = ctx.target.select(cond, arr_vals[k * elem_width + j], result);
        }
        result
    }).collect()
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

    let counter = ctx.target.add_block_param(loop_header, LirType::U64);
    let limit = ctx.target.add_block_param(loop_header, LirType::U64);

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
    ctx.target.jump(loop_header, &[start_u64, limit_val]);

    // Loop header.
    ctx.target.switch_to_block(loop_header);
    ctx.current_block = loop_header;
    let cmp = ctx.target.icmp(IcmpPred::Ult, counter, limit);
    ctx.target.branch(cmp, body_block, &[], done_block, &[]);

    // Body block.
    ctx.target.switch_to_block(body_block);
    ctx.current_block = body_block;
    ctx.env.insert(var.to_owned(), vec![counter]);
    ctx.env_types.insert(var.to_owned(), IrType::Primitive(PrimitiveType::Usize));
    lower_block(body, ctx);
    let one = ctx.target.iconst(LirType::U64, 1);
    let next = ctx.target.add(counter, one);
    ctx.target.jump(loop_header, &[next, limit]);

    // Done block.
    ctx.target.switch_to_block(done_block);
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
        MethodKind::Std(m) if m == "clone" || m == "deref" => lower_expr(receiver, ctx),

        // Reference methods — transparent.
        MethodKind::Std(m) if m == "as_ref" || m == "as_slice" => lower_expr(receiver, ctx),

        // Unknown methods → extern call.
        MethodKind::Unknown(name) => {
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

    // Collect ABI types and flat scalars for all arguments.
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

    ctx.target.call_extern(&func_name, &arg_tys, &flat_args, None)
}
