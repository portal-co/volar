// @reliability: normal
//! Lower `IrModule` (spec/compiler IR) to any `LirTarget`.
//!
//! Works on the output of `lower_module_dyn` — monomorphized, primitive types only.
//! Handles: primitive types, binary/unary ops, variables, literals, if/else,
//! sequential blocks with let-bindings.
//!
//! Unimplemented (see goals.md): arrays, structs, generics, method calls,
//! function calls, loops, match expressions.

use std::collections::BTreeMap;
use volar_compiler::ir::{
    IrBlock, IrExpr, IrFunction, IrLit, IrModule, IrPattern, IrStmt, IrType, PrimitiveType,
    SpecBinOp, SpecUnaryOp,
};
use volar_lir::{IcmpPred, LirTarget, LirType};

// ============================================================================
// Type mapping
// ============================================================================

fn primitive_to_lir(p: PrimitiveType) -> LirType {
    match p {
        PrimitiveType::Bool | PrimitiveType::Bit => LirType::Bool,
        PrimitiveType::U8 | PrimitiveType::Galois | PrimitiveType::BitsInBytes => LirType::U8,
        PrimitiveType::U32 => LirType::U32,
        PrimitiveType::U64
        | PrimitiveType::Galois64
        | PrimitiveType::BitsInBytes64
        | PrimitiveType::Usize => LirType::U64,
        PrimitiveType::I128 => LirType::I64, // truncate to i64 for now
    }
}

fn ir_type_to_lir(ty: &IrType) -> LirType {
    match ty {
        IrType::Primitive(p) => primitive_to_lir(*p),
        IrType::Unit => LirType::Bool, // unit → zero-bit; use Bool as placeholder
        other => unimplemented!("ir_type_to_lir: unsupported type {:?}", other),
    }
}

// ============================================================================
// Lowering context
// ============================================================================

struct LowerCtx<'t, T: LirTarget> {
    target: &'t mut T,
    /// Variable name → SSA value.
    env: BTreeMap<String, T::Value>,
    /// The block we are currently emitting into.
    current_block: T::Block,
}

impl<'t, T: LirTarget> LowerCtx<'t, T> {
    fn new(target: &'t mut T, entry: T::Block, params: Vec<(String, T::Value)>) -> Self {
        let mut env = BTreeMap::new();
        for (name, val) in params {
            env.insert(name, val);
        }
        LowerCtx { target, env, current_block: entry }
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Lower all functions in `module` to `target`.
///
/// Only free functions are lowered; structs, traits, and impls are skipped.
pub fn lower_module<T: LirTarget>(module: &IrModule, target: &mut T) {
    for func in &module.functions {
        lower_function(func, target);
    }
}

/// Lower a single `IrFunction` to `target`.
pub fn lower_function<T: LirTarget>(func: &IrFunction, target: &mut T) {
    // Map parameter types.
    let param_tys: Vec<LirType> = func.params.iter().map(|p| ir_type_to_lir(&p.ty)).collect();
    let ret_ty = func.return_type.as_ref().map(|t| ir_type_to_lir(t));

    let (entry, param_vals) = target.begin_function(&func.name, &param_tys, ret_ty);
    target.switch_to_block(entry);

    // Bind parameter names.
    let named_params: Vec<(String, T::Value)> = func
        .params
        .iter()
        .zip(param_vals.iter())
        .map(|(p, &v)| (p.name.clone(), v))
        .collect();

    let mut ctx = LowerCtx::new(target, entry, named_params);

    // Lower the body block.
    let tail = lower_block(&func.body, &mut ctx);

    // Emit return.
    ctx.target.ret(tail);
    ctx.target.end_function();
}

// ============================================================================
// Block and statement lowering
// ============================================================================

/// Lower an `IrBlock`, returning the tail expression value (if any).
fn lower_block<T: LirTarget>(block: &IrBlock, ctx: &mut LowerCtx<T>) -> Option<T::Value> {
    for stmt in &block.stmts {
        lower_stmt(stmt, ctx);
    }
    block.expr.as_deref().map(|e| lower_expr(e, ctx))
}

fn lower_stmt<T: LirTarget>(stmt: &IrStmt, ctx: &mut LowerCtx<T>) {
    match stmt {
        IrStmt::Let { pattern, init, .. } => {
            if let Some(init_expr) = init {
                let val = lower_expr(init_expr, ctx);
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

        IrExpr::If {
            cond,
            then_branch,
            else_branch,
        } => lower_if(cond, then_branch, else_branch.as_deref(), ctx),

        IrExpr::Cast { expr: inner, ty } => {
            let v = lower_expr(inner, ctx);
            let dst = ir_type_to_lir(ty);
            // Use zext as a safe default cast; sign/trunc semantics are approximate.
            ctx.target.zext(v, dst)
        }

        IrExpr::Return(val) => {
            let rv = val.as_deref().map(|e| lower_expr(e, ctx));
            ctx.target.ret(rv);
            // Return expressions diverge; emit a dummy value.
            ctx.target.iconst(LirType::Bool, 0)
        }

        other => unimplemented!("lower_expr: unsupported expr {:?}", other),
    }
}

fn lower_lit<T: LirTarget>(lit: &IrLit, ctx: &mut LowerCtx<T>) -> T::Value {
    match lit {
        IrLit::Int(n) => {
            // Default to U64 for untyped integer literals.
            ctx.target.iconst(LirType::U64, *n as i64)
        }
        IrLit::Bool(b) => ctx.target.iconst(LirType::Bool, *b as i64),
        IrLit::Float(_) => unimplemented!("float literals not supported in LIR"),
        other => unimplemented!("lower_lit: unsupported literal {:?}", other),
    }
}

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
        SpecBinOp::Rem => {
            // LirTarget has no rem; approximate as udiv-based subtraction.
            // This is a placeholder — users should add urem/srem to LirTarget.
            unimplemented!("Rem not yet in LirTarget")
        }
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
        SpecBinOp::And => {
            // Logical AND: both operands must be Bool.
            ctx.target.and(lv, rv)
        }
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
        SpecUnaryOp::Deref | SpecUnaryOp::Ref | SpecUnaryOp::RefMut => {
            // References are transparent in LIR — treat as identity.
            v
        }
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

    // We'll add a join param after we know the result type.
    // For now, assume both branches produce U64 (the common case).
    // TODO: propagate type information to pick the right LirType.
    let result_ty = LirType::U64;
    let join_param = ctx.target.add_block_param(join_block, result_ty);

    ctx.target.branch(cond_val, then_block, &[], else_block, &[]);

    // --- then branch ---
    ctx.target.switch_to_block(then_block);
    ctx.current_block = then_block;
    let then_val = lower_block(then_branch, ctx)
        .unwrap_or_else(|| ctx.target.iconst(result_ty, 0));
    ctx.target.jump(join_block, &[then_val]);

    // --- else branch ---
    ctx.target.switch_to_block(else_block);
    ctx.current_block = else_block;
    let else_val = if let Some(else_expr) = else_branch {
        lower_expr(else_expr, ctx)
    } else {
        ctx.target.iconst(result_ty, 0)
    };
    ctx.target.jump(join_block, &[else_val]);

    // --- join ---
    ctx.target.switch_to_block(join_block);
    ctx.current_block = join_block;

    join_param
}
