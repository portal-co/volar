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

use structs::{StructRegistry, primitive_to_lir};

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
    /// Variable name → SSA value.
    env: BTreeMap<String, T::Value>,
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
        params: Vec<(String, T::Value, IrType)>,
        registry: &'t StructRegistry,
        hash_suffix: String,
        module_structs: &'t [volar_compiler::ir::IrStruct],
    ) -> Self {
        let mut env = BTreeMap::new();
        let mut env_types = BTreeMap::new();
        for (name, val, ty) in params {
            env.insert(name.clone(), val);
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
    // Map parameter types.
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty))
        .collect();
    let ret_ty = func.return_type.as_ref().map(|t| registry.ir_type_to_lir(t));

    let (entry, param_vals) = target.begin_function(&func.name, &param_lir_tys, ret_ty);
    target.switch_to_block(entry);

    let named_params: Vec<(String, T::Value, IrType)> = func
        .params
        .iter()
        .zip(param_vals.iter())
        .map(|(p, &v)| (p.name.clone(), v, p.ty.clone()))
        .collect();

    let mut ctx = LowerCtx::new(
        target,
        entry,
        named_params,
        registry,
        hash_suffix.to_owned(),
        module_structs,
    );

    let tail = lower_block(&func.body, &mut ctx);
    ctx.target.ret(tail);
    ctx.target.end_function();
}

// ============================================================================
// Block and statement lowering
// ============================================================================

fn lower_block<T: LirTarget>(block: &IrBlock, ctx: &mut LowerCtx<T>) -> Option<T::Value> {
    for stmt in &block.stmts {
        lower_stmt(stmt, ctx);
    }
    block.expr.as_deref().map(|e| lower_expr(e, ctx))
}

fn lower_stmt<T: LirTarget>(stmt: &IrStmt, ctx: &mut LowerCtx<T>) {
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            if let Some(init_expr) = init {
                let val = lower_expr(init_expr, ctx);
                // Record the type in env_types (use declared type if available,
                // otherwise try to infer from the expression).
                if let IrPattern::Ident { name, .. } = pattern {
                    let ir_ty = ty
                        .clone()
                        .or_else(|| ctx.infer_type(init_expr));
                    if let Some(ir_ty) = ir_ty {
                        ctx.env_types.insert(name.clone(), ir_ty);
                    }
                }
                bind_pattern(pattern, val, ctx);
            }
        }
        IrStmt::Semi(expr) | IrStmt::Expr(expr) => {
            lower_expr(expr, ctx);
        }
    }
}

fn bind_pattern<T: LirTarget>(pattern: &IrPattern, val: T::Value, ctx: &mut LowerCtx<T>) {
    match pattern {
        IrPattern::Ident { name, .. } => {
            ctx.env.insert(name.clone(), val);
        }
        IrPattern::Wild => {}
        other => unimplemented!("bind_pattern: unsupported pattern {:?}", other),
    }
}

// ============================================================================
// Expression lowering
// ============================================================================

fn lower_expr<T: LirTarget>(expr: &IrExpr, ctx: &mut LowerCtx<T>) -> T::Value {
    match expr {
        IrExpr::Lit(lit) => lower_lit(lit, ctx),

        IrExpr::Var(name) => *ctx
            .env
            .get(name.as_str())
            .unwrap_or_else(|| panic!("undefined variable: {name}")),

        IrExpr::Binary { op, left, right } => {
            let lv = lower_expr(left, ctx);
            let rv = lower_expr(right, ctx);
            lower_binop(*op, lv, rv, ctx)
        }

        IrExpr::Unary { op, expr: inner } => {
            let v = lower_expr(inner, ctx);
            lower_unop(*op, v, ctx)
        }

        IrExpr::Block(b) => {
            lower_block(b, ctx).unwrap_or_else(|| ctx.target.iconst(LirType::Bool, 0))
        }

        IrExpr::If { cond, then_branch, else_branch } => {
            lower_if(cond, then_branch, else_branch.as_deref(), ctx)
        }

        IrExpr::Cast { expr: inner, ty } => {
            let v = lower_expr(inner, ctx);
            let dst = ir_type_to_lir(ty);
            ctx.target.zext(v, dst)
        }

        IrExpr::Return(val) => {
            let rv = val.as_deref().map(|e| lower_expr(e, ctx));
            ctx.target.ret(rv);
            ctx.target.iconst(LirType::Bool, 0)
        }

        // ---- Phase 2: field access ------------------------------------------

        IrExpr::Field { base, field } => lower_field(base, field, ctx),

        // ---- Phase 2: array index -------------------------------------------

        IrExpr::Index { base, index } => {
            let arr = lower_expr(base, ctx);
            let idx = lower_expr(index, ctx);
            ctx.target.arr_get(arr, idx)
        }

        // ---- Phase 2: struct construction -----------------------------------

        IrExpr::StructExpr { kind, fields, .. } => lower_struct_expr(kind, fields, ctx),

        // ---- Phase 2: fixed-size array literal ------------------------------

        IrExpr::FixedArray(elems) => lower_fixed_array(elems, ctx),
        IrExpr::Array(elems) => lower_fixed_array(elems, ctx), // treat same as FixedArray

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
            ctx.target.iconst(LirType::Bool, 0) // loops are ()-typed
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
) -> T::Value {
    let cond_val = lower_expr(cond_expr, ctx);

    let then_block = ctx.target.create_block();
    let else_block = ctx.target.create_block();
    let join_block = ctx.target.create_block();

    // Placeholder result type — U64 covers most cases.
    // TODO: propagate type information properly.
    let result_ty = LirType::U64;
    let join_param = ctx.target.add_block_param(join_block, result_ty.clone());

    ctx.target.branch(cond_val, then_block, &[], else_block, &[]);

    ctx.target.switch_to_block(then_block);
    ctx.current_block = then_block;
    let then_val = lower_block(then_branch, ctx)
        .unwrap_or_else(|| ctx.target.iconst(result_ty.clone(), 0));
    ctx.target.jump(join_block, &[then_val]);

    ctx.target.switch_to_block(else_block);
    ctx.current_block = else_block;
    let else_val = if let Some(else_expr) = else_branch {
        lower_expr(else_expr, ctx)
    } else {
        ctx.target.iconst(result_ty, 0)
    };
    ctx.target.jump(join_block, &[else_val]);

    ctx.target.switch_to_block(join_block);
    ctx.current_block = join_block;

    join_param
}

// ============================================================================
// Phase 2: field access
// ============================================================================

fn lower_field<T: LirTarget>(base: &IrExpr, field: &str, ctx: &mut LowerCtx<T>) -> T::Value {
    // Infer struct type of base, then find field index.
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
    let base_val = lower_expr(base, ctx);
    ctx.target.struct_get(base_val, field_idx)
}

// ============================================================================
// Phase 2: struct construction
// ============================================================================

fn lower_struct_expr<T: LirTarget>(
    kind: &StructKind,
    fields: &[(String, IrExpr)],
    ctx: &mut LowerCtx<T>,
) -> T::Value {
    let struct_id = ctx.registry.id_for(kind).unwrap_or_else(|| {
        panic!("struct {:?} not in registry", kind)
    });

    // Build a name → expr map from the IrExpr fields.
    let field_map: BTreeMap<&str, &IrExpr> =
        fields.iter().map(|(n, e)| (n.as_str(), e)).collect();

    // Lower fields in the declaration order recorded in the registry.
    let decl_names: Vec<String> = ctx.registry.field_names(struct_id).to_vec();
    let field_vals: Vec<T::Value> = decl_names
        .iter()
        .map(|name| {
            let expr: &IrExpr = field_map.get(name.as_str()).copied()
                .or_else(|| fields.iter().find(|(n, _)| n == name).map(|(_, e)| e))
                .unwrap_or_else(|| panic!("StructExpr missing field '{name}'"));
            lower_expr(expr, ctx)
        })
        .collect();

    ctx.target.struct_new(struct_id, &field_vals)
}

// ============================================================================
// Phase 2: fixed-size array literal
// ============================================================================

fn lower_fixed_array<T: LirTarget>(elems: &[IrExpr], ctx: &mut LowerCtx<T>) -> T::Value {
    let elem_vals: Vec<T::Value> = elems.iter().map(|e| lower_expr(e, ctx)).collect();

    // Determine element type from the first element's type (default U8).
    let elem_lir_ty = if let Some(first) = elems.first() {
        ctx.infer_type(first)
            .map(|t| ctx.registry.ir_type_to_lir(&t))
            .unwrap_or(LirType::U8)
    } else {
        LirType::U8
    };

    ctx.target.arr_new(elem_lir_ty, &elem_vals)
}

// ============================================================================
// Phase 2: ArrayGenerate (from_fn / closure-based array fill)
// ============================================================================

fn lower_array_generate<T: LirTarget>(
    elem_ty: Option<&IrType>,
    len: &ArrayLength,
    index_var: &str,
    body: &IrExpr,
    ctx: &mut LowerCtx<T>,
) -> T::Value {
    let n = const_len(len);
    let elem_lir_ty = elem_ty
        .map(|t| ctx.registry.ir_type_to_lir(t))
        .unwrap_or(LirType::U8);

    // Unroll: evaluate body once per index value.
    let elem_vals: Vec<T::Value> = (0..n)
        .map(|k| {
            let idx_val = ctx.target.iconst(LirType::U64, k as i64);
            ctx.env.insert(index_var.to_owned(), idx_val);
            ctx.env_types.insert(
                index_var.to_owned(),
                IrType::Primitive(PrimitiveType::Usize),
            );
            lower_expr(body, ctx)
        })
        .collect();

    // Restore: remove the index variable.
    ctx.env.remove(index_var);
    ctx.env_types.remove(index_var);

    ctx.target.arr_new(elem_lir_ty, &elem_vals)
}

// ============================================================================
// Phase 2: RawMap (element-wise unary array op)
// ============================================================================

fn lower_raw_map<T: LirTarget>(
    receiver: &IrExpr,
    elem_var: &IrPattern,
    body: &IrExpr,
    ctx: &mut LowerCtx<T>,
) -> T::Value {
    // Determine array length from receiver's type.
    let recv_ty = ctx
        .infer_type(receiver)
        .unwrap_or_else(|| panic!("RawMap: could not infer receiver type"));
    let (elem_ir_ty, n) = array_elem_and_len(&recv_ty);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);

    let recv_val = lower_expr(receiver, ctx);

    let elem_vals: Vec<T::Value> = (0..n)
        .map(|k| {
            let idx = ctx.target.iconst(LirType::U64, k as i64);
            let elem = ctx.target.arr_get(recv_val, idx);
            bind_map_pattern(elem_var, elem, &elem_ir_ty, ctx);
            lower_expr(body, ctx)
        })
        .collect();

    ctx.target.arr_new(elem_lir_ty, &elem_vals)
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
) -> T::Value {
    let left_ty = ctx
        .infer_type(left)
        .unwrap_or_else(|| panic!("RawZip: could not infer left type"));
    let (elem_ir_ty, n) = array_elem_and_len(&left_ty);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty);

    let left_val = lower_expr(left, ctx);
    let right_val = lower_expr(right, ctx);

    let elem_vals: Vec<T::Value> = (0..n)
        .map(|k| {
            let idx = ctx.target.iconst(LirType::U64, k as i64);
            let lv = ctx.target.arr_get(left_val, idx);
            let rv = ctx.target.arr_get(right_val, idx);
            bind_map_pattern(left_var, lv, &elem_ir_ty, ctx);
            bind_map_pattern(right_var, rv, &elem_ir_ty, ctx);
            lower_expr(body, ctx)
        })
        .collect();

    ctx.target.arr_new(elem_lir_ty, &elem_vals)
}

fn bind_map_pattern<T: LirTarget>(
    pattern: &IrPattern,
    val: T::Value,
    ir_ty: &IrType,
    ctx: &mut LowerCtx<T>,
) {
    match pattern {
        IrPattern::Ident { name, .. } => {
            ctx.env.insert(name.clone(), val);
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
    let start_val = lower_expr(start, ctx);
    let end_val = lower_expr(end, ctx);
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
    ctx.env.insert(var.to_owned(), counter);
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
) -> T::Value {
    match method {
        // `.clone()` and `.deref()` are transparent in value semantics.
        MethodKind::Std(m) if m == "clone" || m == "deref" => lower_expr(receiver, ctx),

        // Reference method — transparent.
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
) -> T::Value {
    // Build the extern function name: "{method_name}_{hash_suffix}" if hash_suffix is set.
    let extern_name = if ctx.hash_suffix.is_empty() {
        method_name.to_owned()
    } else {
        format!("{}_{}", method_name, ctx.hash_suffix)
    };

    // Lower receiver + args.
    let recv_val = lower_expr(receiver, ctx);
    let arg_vals: Vec<T::Value> = args.iter().map(|a| lower_expr(a, ctx)).collect();

    let mut all_args = vec![recv_val];
    all_args.extend(arg_vals);

    // Infer return type from receiver type (the method returns the same struct type).
    let ret_ir_ty = ctx.infer_type(receiver);
    let ret_lir_ty = ret_ir_ty.as_ref().map(|ty| ctx.registry.ir_type_to_lir(ty));

    ctx.target
        .call_extern(&extern_name, ret_lir_ty, &all_args)
        .unwrap_or_else(|| ctx.target.iconst(LirType::Bool, 0))
}

// ============================================================================
// Phase 2: free function calls
// ============================================================================

fn lower_call<T: LirTarget>(func: &IrExpr, args: &[IrExpr], ctx: &mut LowerCtx<T>) -> T::Value {
    // Array::from_fn(|j| body) — not yet handled here.
    // The weaver should produce IrExpr::ArrayGenerate instead.
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

    // Generic extern call: lower func to a path name.
    let func_name = match func {
        IrExpr::Path { segments, .. } => segments.join("_"),
        IrExpr::Var(name) => name.clone(),
        other => unimplemented!("lower_call: non-path func {:?}", other),
    };

    let arg_vals: Vec<T::Value> = args.iter().map(|a| lower_expr(a, ctx)).collect();
    ctx.target
        .call_extern(&func_name, None, &arg_vals)
        .unwrap_or_else(|| ctx.target.iconst(LirType::Bool, 0))
}
