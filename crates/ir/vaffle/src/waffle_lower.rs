// @reliability: experimental
// @ai: assisted
//! WAFFLE `FunctionBody` → VAFFLE lowering.
//!
//! Handles the subset relevant to program-related cryptography:
//! - **Integer ops**: I32/I64 arithmetic, bitwise, comparisons, constants,
//!   and conversions.
//! - **Branches**: `Br`, `CondBr`, `Return`.
//! - **Direct calls**: `Operator::Call { function_index }`.
//!
//! All integer values are bit-decomposed via `BitCircuitBuilder`, sharing
//! the same GF(2) Poly circuits as `VolarIrTarget`.
//!
//! # Errors
//! `UnsupportedOp` is returned for floats, memory, tables, globals, indirect
//! calls, SIMD, and atomics.  The module-level helper skips those functions.

use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use portal_pc_waffle_ir::{
    entity::EntityRef, // for .index() on Func/Block/etc.
    FuncDecl, FunctionBody, Module as WModule, Operator, SignatureData, Terminator,
    Type as WType, Value as WValue, ValueDef,
};

use volar_lir::{BitCircuitBuilder, IcmpPred, LirTarget, LirType};

use super::target::{bits_for_lir_type, VaffleBlock, VaffleTarget, VaffleValue};
use super::ValueId;

// ============================================================================
// Error
// ============================================================================

#[derive(Debug, Clone)]
pub struct UnsupportedOp(pub String);

impl core::fmt::Display for UnsupportedOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "unsupported WAFFLE op: {}", self.0)
    }
}

// ============================================================================
// Type mapping
// ============================================================================

fn waffle_ty(ty: WType) -> Result<LirType, UnsupportedOp> {
    match ty {
        WType::I32 => Ok(LirType::U32),
        WType::I64 => Ok(LirType::U64),
        other => Err(UnsupportedOp(alloc::format!("{other:?}"))),
    }
}

// ============================================================================
// Public entry points
// ============================================================================

/// Lower all function bodies in a WAFFLE module into `target`, skipping
/// unsupported functions.  Returns a list of (name, error) for skipped functions.
pub fn lower_waffle_module(
    wasm: &WModule,
    target: &mut VaffleTarget,
) -> Vec<(String, UnsupportedOp)> {
    let mut errors = Vec::new();
    // EntityVec::entries() yields (Func, &FuncDecl) pairs.
    for (_func_ref, decl) in wasm.funcs.entries() {
        if let FuncDecl::Body(_, name, body) = decl {
            if let Err(e) = lower_waffle_function(body, name.as_str(), wasm, target) {
                errors.push((name.clone(), e));
            }
        }
    }
    errors
}

/// Lower a single WAFFLE `FunctionBody` into `target`.
///
/// Parameter types come from `body.locals` (first `body.n_params` locals);
/// return types from `body.rets`.
pub fn lower_waffle_function(
    body: &FunctionBody,
    name: &str,
    wasm: &WModule,
    target: &mut VaffleTarget,
) -> Result<(), UnsupportedOp> {
    // ---- Parameter / return types from FunctionBody directly ----------------
    // body.locals contains all locals; the first n_params are the parameters.
    let param_tys: Vec<WType> = body.locals.values().take(body.n_params).copied().collect();
    let ret_tys: &[WType] = &body.rets;

    let param_lir: Vec<LirType> = param_tys.iter().map(|&t| waffle_ty(t)).collect::<Result<_, _>>()?;
    let ret_lir: Option<LirType> = match ret_tys {
        [] => None,
        [t] => Some(waffle_ty(*t)?),
        _ => return Err(UnsupportedOp("multi-value return".into())),
    };

    let (entry_block, param_groups) = target.begin_function(name, &param_lir, ret_lir.clone());
    target.switch_to_block(entry_block);

    // ---- Map WAFFLE blocks → VAFFLE blocks ----------------------------------
    // EntityVec::entries() yields (Block, &BlockDef).
    let mut block_map: BTreeMap<portal_pc_waffle_ir::Block, VaffleBlock> = BTreeMap::new();
    block_map.insert(body.entry, entry_block);
    for (wblock, _) in body.blocks.entries() {
        if wblock == body.entry { continue; }
        block_map.insert(wblock, target.create_block());
    }

    // ---- Seed value map with entry-block params (= function parameters) -----
    let mut val_map: BTreeMap<WValue, VaffleValue> = BTreeMap::new();
    for ((_, entry_wval), group) in body.blocks[body.entry].params.iter().zip(param_groups.iter()) {
        if let Some(vv) = group.first() {
            val_map.insert(*entry_wval, vv.clone());
        }
    }

    // ---- Emit each WAFFLE block ---------------------------------------------
    for (wblock, block_def) in body.blocks.entries() {
        let vblock = block_map[&wblock];
        target.switch_to_block(vblock);

        // Non-entry block params become VAFFLE block params.
        if wblock != body.entry {
            for &(ty, wval) in &block_def.params {
                let lir_ty = waffle_ty(ty)?;
                let vv = target.add_block_param(vblock, lir_ty);
                val_map.insert(wval, vv);
            }
        }

        // Instructions: each ValueRecord wraps a Value index.
        for record in &block_def.insts {
            let wval = record.value;
            match &body.values[wval] {
                ValueDef::Operator(op, args_ref, tys_ref) => {
                    // ListPool[ListRef<T>] -> &[T] via the Index impl.
                    let args: Vec<WValue> = body.arg_pool[*args_ref].to_vec();
                    let result_tys: Vec<WType> = body.type_pool[*tys_ref].to_vec();
                    if let Some(vv) = lower_op(op, &args, &result_tys, &val_map, target, wasm)? {
                        val_map.insert(wval, vv);
                    }
                }
                ValueDef::PickOutput(from_val, idx, ty) => {
                    if let Some(call_vv) = val_map.get(from_val) {
                        let lir_ty = waffle_ty(*ty)?;
                        let n = bits_for_lir_type(&lir_ty, &[]);
                        let start = (*idx as usize) * n;
                        let end = (start + n).min(call_vv.bits.len());
                        val_map.insert(wval, VaffleValue {
                            bits: call_vv.bits[start..end].to_vec(),
                            ty: lir_ty,
                        });
                    }
                }
                ValueDef::Alias(target_val) => {
                    if let Some(vv) = val_map.get(target_val).cloned() {
                        val_map.insert(wval, vv);
                    }
                }
                // BlockParam and Placeholder are handled during block-param setup.
                _ => {}
            }
        }

        // Terminator.
        lower_term(
            &block_def.terminator.terminator,
            &val_map,
            &block_map,
            ret_lir.clone(),
            target,
        )?;
    }

    target.end_function();
    Ok(())
}

// ============================================================================
// Operator lowering
// ============================================================================

fn lower_op(
    op: &Operator,
    args: &[WValue],
    result_tys: &[WType],
    val_map: &BTreeMap<WValue, VaffleValue>,
    tgt: &mut VaffleTarget,
    wasm: &WModule,
) -> Result<Option<VaffleValue>, UnsupportedOp> {
    let get = |i: usize| -> Result<VaffleValue, UnsupportedOp> {
        val_map.get(&args[i]).cloned()
            .ok_or_else(|| UnsupportedOp(alloc::format!("undefined value {:?}", args[i])))
    };

    Ok(Some(match op {
        // ---- Constants -------------------------------------------------
        Operator::I32Const { value } => tgt.iconst(LirType::U32, *value as i32 as i64),
        Operator::I64Const { value } => tgt.iconst(LirType::U64, *value as i64),

        // ---- I32 arithmetic --------------------------------------------
        Operator::I32Add  => tgt.add(get(0)?, get(1)?),
        Operator::I32Sub  => tgt.sub(get(0)?, get(1)?),
        Operator::I32Mul  => tgt.mul(get(0)?, get(1)?),
        Operator::I32DivS => tgt.sdiv(get(0)?, get(1)?),
        Operator::I32DivU => tgt.udiv(get(0)?, get(1)?),
        Operator::I32And  => tgt.and(get(0)?, get(1)?),
        Operator::I32Or   => tgt.or(get(0)?, get(1)?),
        Operator::I32Xor  => tgt.xor(get(0)?, get(1)?),
        Operator::I32Shl  => tgt.shl(get(0)?, get(1)?),
        Operator::I32ShrS => tgt.ashr(get(0)?, get(1)?),
        Operator::I32ShrU => tgt.lshr(get(0)?, get(1)?),

        // ---- I32 comparisons (result is i32: 0 or 1) -------------------
        Operator::I32Eqz => { let z = tgt.iconst(LirType::U32, 0); let c = tgt.icmp(IcmpPred::Eq,  get(0)?, z); tgt.zext(c, LirType::U32) }
        Operator::I32Eq  => { let c = tgt.icmp(IcmpPred::Eq,  get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32Ne  => { let c = tgt.icmp(IcmpPred::Ne,  get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32LtS => { let c = tgt.icmp(IcmpPred::Slt, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32LtU => { let c = tgt.icmp(IcmpPred::Ult, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32GtS => { let c = tgt.icmp(IcmpPred::Sgt, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32GtU => { let c = tgt.icmp(IcmpPred::Ugt, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32LeS => { let c = tgt.icmp(IcmpPred::Sle, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32LeU => { let c = tgt.icmp(IcmpPred::Ule, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32GeS => { let c = tgt.icmp(IcmpPred::Sge, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }
        Operator::I32GeU => { let c = tgt.icmp(IcmpPred::Uge, get(0)?, get(1)?); tgt.zext(c, LirType::U32) }

        // ---- I64 arithmetic --------------------------------------------
        Operator::I64Add  => tgt.add(get(0)?, get(1)?),
        Operator::I64Sub  => tgt.sub(get(0)?, get(1)?),
        Operator::I64Mul  => tgt.mul(get(0)?, get(1)?),
        Operator::I64DivS => tgt.sdiv(get(0)?, get(1)?),
        Operator::I64DivU => tgt.udiv(get(0)?, get(1)?),
        Operator::I64And  => tgt.and(get(0)?, get(1)?),
        Operator::I64Or   => tgt.or(get(0)?, get(1)?),
        Operator::I64Xor  => tgt.xor(get(0)?, get(1)?),
        Operator::I64Shl  => tgt.shl(get(0)?, get(1)?),
        Operator::I64ShrS => tgt.ashr(get(0)?, get(1)?),
        Operator::I64ShrU => tgt.lshr(get(0)?, get(1)?),

        // ---- I64 comparisons -------------------------------------------
        Operator::I64Eqz => { let z = tgt.iconst(LirType::U64, 0); let c = tgt.icmp(IcmpPred::Eq,  get(0)?, z); tgt.zext(c, LirType::U64) }
        Operator::I64Eq  => { let c = tgt.icmp(IcmpPred::Eq,  get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64Ne  => { let c = tgt.icmp(IcmpPred::Ne,  get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64LtS => { let c = tgt.icmp(IcmpPred::Slt, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64LtU => { let c = tgt.icmp(IcmpPred::Ult, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64GtS => { let c = tgt.icmp(IcmpPred::Sgt, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64GtU => { let c = tgt.icmp(IcmpPred::Ugt, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64LeS => { let c = tgt.icmp(IcmpPred::Sle, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64LeU => { let c = tgt.icmp(IcmpPred::Ule, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64GeS => { let c = tgt.icmp(IcmpPred::Sge, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }
        Operator::I64GeU => { let c = tgt.icmp(IcmpPred::Uge, get(0)?, get(1)?); tgt.zext(c, LirType::U64) }

        // ---- Conversions -----------------------------------------------
        Operator::I32WrapI64    => tgt.trunc(get(0)?, LirType::U32),
        Operator::I64ExtendI32S => tgt.sext(get(0)?, LirType::U64),
        Operator::I64ExtendI32U => tgt.zext(get(0)?, LirType::U64),

        // ---- Select (WAFFLE: args = [val_true, val_false, cond]) --------
        Operator::Select | Operator::TypedSelect { .. } => {
            let if_t = get(0)?;
            let if_f = get(1)?;
            let cond = get(2)?;
            // cond is I32; treat as bool via OR-reduce (non-zero = true).
            let cond_bit = or_bits(tgt, &cond.bits);
            let cond_bool = VaffleValue { bits: vec![cond_bit], ty: LirType::Bool };
            tgt.select(cond_bool, if_t, if_f)
        }

        // ---- Direct call -----------------------------------------------
        Operator::Call { function_index } => {
            let fid = *function_index;
            let n_results = result_tys.len();
            let ret_ty = match n_results {
                0 => None,
                1 => Some(waffle_ty(result_tys[0])?),
                _ => return Err(UnsupportedOp("multi-result Call".into())),
            };
            let name = callee_name(wasm, fid);
            let arg_vals: Vec<VaffleValue> = args.iter()
                .map(|wv| val_map.get(wv).cloned()
                    .ok_or_else(|| UnsupportedOp(alloc::format!("undefined arg {:?}", wv))))
                .collect::<Result<_, _>>()?;
            return Ok(tgt.call_extern(&name, &[], &arg_vals, ret_ty).into_iter().next());
        }

        Operator::Nop => return Ok(None),

        other => return Err(UnsupportedOp(alloc::format!("{other:?}"))),
    }))
}

// ============================================================================
// Terminator lowering
// ============================================================================

fn lower_term(
    term: &Terminator,
    val_map: &BTreeMap<WValue, VaffleValue>,
    block_map: &BTreeMap<portal_pc_waffle_ir::Block, VaffleBlock>,
    ret_lir: Option<LirType>,
    tgt: &mut VaffleTarget,
) -> Result<(), UnsupportedOp> {
    let get = |wv: &WValue| -> Result<VaffleValue, UnsupportedOp> {
        val_map.get(wv).cloned()
            .ok_or_else(|| UnsupportedOp(alloc::format!("undefined {:?}", wv)))
    };
    let get_block = |wb: portal_pc_waffle_ir::Block| -> Result<VaffleBlock, UnsupportedOp> {
        block_map.get(&wb).copied()
            .ok_or_else(|| UnsupportedOp(alloc::format!("unknown block {:?}", wb)))
    };
    let get_args = |wargs: &[WValue]| -> Result<Vec<VaffleValue>, UnsupportedOp> {
        wargs.iter().map(|wv| get(wv)).collect()
    };

    match term {
        Terminator::Br { target: bt } => {
            let vb = get_block(bt.block)?;
            let args = get_args(&bt.args)?;
            tgt.jump(vb, &args);
        }

        Terminator::CondBr { cond, if_true, if_false } => {
            let cond_vv = get(cond)?;
            // WAFFLE cond is I32; use OR-reduce for non-zero test.
            let cond_bit = or_bits(tgt, &cond_vv.bits);
            let cond_bool = VaffleValue { bits: vec![cond_bit], ty: LirType::Bool };
            let then_b = get_block(if_true.block)?;
            let else_b = get_block(if_false.block)?;
            tgt.branch(cond_bool, then_b, &get_args(&if_true.args)?, else_b, &get_args(&if_false.args)?);
        }

        Terminator::Return { values } => {
            let vals: Vec<VaffleValue> = values.iter().map(|v| get(v)).collect::<Result<_, _>>()?;
            tgt.ret(&vals);
        }

        Terminator::Unreachable | Terminator::UB | Terminator::None => {
            let zero = ret_lir.map(|ty| tgt.iconst(ty, 0));
            tgt.ret(zero.as_ref().map_or(&[], core::slice::from_ref));
        }

        Terminator::ReturnCall { func, args } => {
            let name = alloc::format!("func_{}", func.index());
            let arg_vals: Vec<VaffleValue> = args.iter().map(|a| get(a)).collect::<Result<_, _>>()?;
            let results = tgt.call_extern(&name, &[], &arg_vals, ret_lir);
            tgt.ret(&results);
        }

        other => return Err(UnsupportedOp(alloc::format!("terminator {other:?}"))),
    }
    Ok(())
}

// ============================================================================
// Helpers
// ============================================================================

/// OR-reduce a bit vector: 1 iff any input bit is 1 (non-zero test).
fn or_bits(tgt: &mut VaffleTarget, bits: &[ValueId]) -> ValueId {
    if bits.is_empty() { return tgt.bc_const(false); }
    let mut acc = bits[0];
    for &b in &bits[1..] { acc = tgt.bc_or(acc, b); }
    acc
}

/// Derive a callee name from the WAFFLE module's function declaration.
fn callee_name(wasm: &WModule, fid: portal_pc_waffle_ir::Func) -> String {
    match &wasm.funcs[fid] {
        FuncDecl::Body(_, name, _) => name.clone(),
        FuncDecl::Import(_, name)  => name.clone(),
        _ => alloc::format!("func_{}", fid.index()),
    }
}
