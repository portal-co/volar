// @reliability: experimental
// @ai: assisted
//! WAFFLE `FunctionBody` → VAFFLE lowering.
//!
//! Handles the subset relevant to program-related cryptography:
//!
//! **Types**: `I32`, `I64` only.  Any function or block parameter of type
//! `F32`, `F64`, `V128`, or any reference/heap type yields `UnsupportedOp`.
//!
//! **Operators** (supported):
//! - Constants: `I32Const`, `I64Const`
//! - Arithmetic: `I32/I64` add, sub, mul, divS, divU, and, or, xor, shl, shrS, shrU
//! - Comparisons: `I32/I64` eqz, eq, ne, ltS/U, gtS/U, leS/U, geS/U
//! - Conversions: `I32WrapI64`, `I64ExtendI32S`, `I64ExtendI32U`
//! - Select: `Select`, `TypedSelect` (I32 non-zero test via OR-reduce)
//! - Direct calls: `Call { function_index }` — single return value only
//! - Nop: `Nop`
//! - Memory loads: `I32Load`, `I64Load`, `I32Load8U/S`, `I32Load16U/S`,
//!   `I64Load8U/S`, `I64Load16U/S`, `I64Load32U/S` (byte-addressed storage)
//! - Memory stores: `I32Store`, `I64Store`, `I32Store8/16`,
//!   `I64Store8/16/32`
//! - Memory stubs: `MemorySize` (always 0), `MemoryGrow` (always -1)
//!
//! **Operators** (not supported — returns `UnsupportedOp`):
//! - All F32/F64 ops and float memory ops
//! - All V128/SIMD ops
//! - All atomic/threads ops
//! - Integer ops not listed above: rem, clz, ctz, popcnt, rotl, rotr,
//!   extend-sign variants, trunc-sat conversions
//! - Multi-result `Call`
//! - `CallIndirect`, `CallRef`
//! - Globals: `GlobalGet`, `GlobalSet`
//! - Tables: `TableGet`, `TableSet`, `TableGrow`, `TableSize`
//! - Bulk memory: `MemoryCopy`, `MemoryFill`, `MemoryInit`, `DataDrop`
//! - Reference types: `RefNull`, `RefIsNull`, `RefFunc`
//! - GC proposal operators
//! - `Unreachable` operator (distinct from `Terminator::Unreachable`)
//!
//! **Terminators** (supported):
//! - `Br`, `CondBr`, `Return`, `ReturnCall`
//! - `Unreachable`, `UB`, `None` — stubbed as a return of zero
//!
//! **Terminators** (not supported — returns `UnsupportedOp`):
//! - `Select` (br_table)
//! - `ReturnCallIndirect`, `ReturnCallRef`
//!
//! All integer values are bit-decomposed via `BitCircuitBuilder`, sharing
//! the same GF(2) Poly circuits as `VolarIrTarget`.
//!
//! # Errors
//! `UnsupportedOp` is returned for any unhandled op, type, or terminator.
//! The module-level helper [`lower_waffle_module`] skips those functions and
//! collects errors rather than panicking.

use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use portal_pc_waffle_ir::{
    entity::EntityRef, // for .index() on Func/Block/etc.
    FuncDecl, FunctionBody, MemoryArg, Module as WModule, Operator, SignatureData, Terminator,
    Type as WType, Value as WValue, ValueDef,
};

use volar_ir_common::StorageId;
use volar_lir::{BitCircuitBuilder, IcmpPred, LirTarget, LirType};
use volar_lir::circuits::StorageEmitter;

use crate::target::{bits_for_lir_type, VaffleBlock, VaffleTarget, VaffleValue};
use vaffle::ValueId;

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

        // ---- Memory loads (byte-addressed storage) ---------------------
        Operator::I32Load { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 4, LirType::U32, false)
        }
        Operator::I64Load { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 8, LirType::U64, false)
        }
        Operator::I32Load8U { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 1, LirType::U32, false)
        }
        Operator::I32Load8S { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 1, LirType::U32, true)
        }
        Operator::I32Load16U { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 2, LirType::U32, false)
        }
        Operator::I32Load16S { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 2, LirType::U32, true)
        }
        Operator::I64Load8U { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 1, LirType::U64, false)
        }
        Operator::I64Load8S { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 1, LirType::U64, true)
        }
        Operator::I64Load16U { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 2, LirType::U64, false)
        }
        Operator::I64Load16S { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 2, LirType::U64, true)
        }
        Operator::I64Load32U { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 4, LirType::U64, false)
        }
        Operator::I64Load32S { memory } => {
            lower_mem_load(tgt, memory, &get(0)?, 4, LirType::U64, true)
        }

        // ---- Memory stores ---------------------------------------------
        Operator::I32Store { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 4);
            return Ok(None);
        }
        Operator::I64Store { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 8);
            return Ok(None);
        }
        Operator::I32Store8 { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 1);
            return Ok(None);
        }
        Operator::I32Store16 { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 2);
            return Ok(None);
        }
        Operator::I64Store8 { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 1);
            return Ok(None);
        }
        Operator::I64Store16 { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 2);
            return Ok(None);
        }
        Operator::I64Store32 { memory } => {
            lower_mem_store(tgt, memory, &get(0)?, &get(1)?, 4);
            return Ok(None);
        }

        // ---- Memory size/grow (stubs returning constant) ----------------
        Operator::MemorySize { .. } => {
            // In the ZK/MPC context, memory size is fixed at compile time.
            // Return 0 pages as a placeholder; real programs should not
            // depend on dynamic memory growth.
            tgt.iconst(LirType::U32, 0)
        }
        Operator::MemoryGrow { .. } => {
            // memory.grow always fails (returns -1) in the circuit model.
            tgt.iconst(LirType::U32, -1i32 as i64)
        }

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
            tgt.ret_call(&name, &arg_vals);
        }

        other => return Err(UnsupportedOp(alloc::format!("terminator {other:?}"))),
    }
    Ok(())
}

// ============================================================================
// Memory helpers
// ============================================================================

/// Address width for WASM memory byte addresses (i32 addresses = 32 bits).
#[allow(dead_code)]
const MEM_ADDR_BITS: usize = 32;

/// Compute effective byte address = base_addr + static_offset.
///
/// `base` is the i32 address from the WASM operand stack (32 bits).
/// `offset` is the static offset from the `MemoryArg`.
/// Returns the 32-bit effective address as a bit vector.
fn effective_addr(
    tgt: &mut VaffleTarget,
    base: &VaffleValue,
    offset: u64,
) -> VaffleValue {
    if offset == 0 {
        return base.clone();
    }
    let off = tgt.iconst(LirType::U32, offset as i64);
    tgt.add(base.clone(), off)
}

/// Read `n_bytes` consecutive bytes from memory storage starting at
/// `byte_addr`, returning a flat bit vector (LSB first, little-endian).
///
/// Each byte is a separate `StorageRead` from `StorageId::memory(mem_idx)`.
/// The returned `VaffleValue` has `n_bytes * 8` bits.
fn mem_load_bytes(
    tgt: &mut VaffleTarget,
    mem_idx: u32,
    byte_addr: &VaffleValue,
    n_bytes: usize,
) -> VaffleValue {
    let storage = StorageId::memory(mem_idx);
    let byte_tid = tgt.byte_tid();
    let bit_tid = tgt.bit_tid();
    let mut all_bits: Vec<ValueId> = Vec::with_capacity(n_bytes * 8);

    for byte_i in 0..n_bytes {
        // Compute address for this byte.
        let addr_val = if byte_i == 0 {
            byte_addr.clone()
        } else {
            let off = tgt.iconst(LirType::U32, byte_i as i64);
            tgt.add(byte_addr.clone(), off)
        };

        // StorageRead: reads one byte (Vec(8, Bit)) from memory.
        let byte_var = tgt.emit_read(storage, byte_tid, &addr_val.bits);

        // Decompose the byte into 8 individual bits via Shuffle.
        for bit_j in 0..8u8 {
            let bit_var = {
                let v = vaffle::Value::Op(volar_ir_common::Stmt::Shuffle {
                    result_bits: vec![(bit_j, byte_var)],
                    ty: bit_tid,
                });
                tgt.fb().emit_value(v)
            };
            all_bits.push(bit_var);
        }
    }

    let ty = match n_bytes {
        1 => LirType::U8,
        2 => LirType::U16,
        4 => LirType::U32,
        8 => LirType::U64,
        _ => LirType::U32,
    };
    VaffleValue { bits: all_bits, ty }
}

/// Write `n_bytes` bytes of `value` (little-endian, LSB first) to memory
/// storage starting at `byte_addr`.
fn mem_store_bytes(
    tgt: &mut VaffleTarget,
    mem_idx: u32,
    byte_addr: &VaffleValue,
    value: &VaffleValue,
    n_bytes: usize,
) {
    let storage = StorageId::memory(mem_idx);
    let byte_tid = tgt.byte_tid();

    for byte_i in 0..n_bytes {
        // Compute address for this byte.
        let addr_val = if byte_i == 0 {
            byte_addr.clone()
        } else {
            let off = tgt.iconst(LirType::U32, byte_i as i64);
            tgt.add(byte_addr.clone(), off)
        };

        // Extract 8 bits for this byte.
        let base = byte_i * 8;
        let bits: Vec<ValueId> = (0..8)
            .map(|j| {
                if base + j < value.bits.len() {
                    value.bits[base + j]
                } else {
                    tgt.bc_const(false) // zero-pad
                }
            })
            .collect();

        // Merge 8 bits into a byte-typed value.
        let byte_var = tgt.compose_address(&bits); // compose_address creates Merge → Vec(8, Bit)
        // Actually compose_address creates Vec(N, Bit) where N = bits.len().
        // For 8 bits this gives us Vec(8, Bit) = byte_tid. Perfect.

        tgt.emit_write(storage, byte_var, byte_tid, &addr_val.bits);
    }
}

/// Lower a WAFFLE memory load operator.
///
/// Returns the loaded value as a `VaffleValue` with the appropriate type.
fn lower_mem_load(
    tgt: &mut VaffleTarget,
    memory: &MemoryArg,
    base: &VaffleValue,
    load_bytes: usize,
    result_ty: LirType,
    sign_extend: bool,
) -> VaffleValue {
    let mem_idx = memory.memory.index() as u32;
    let addr = effective_addr(tgt, base, memory.offset);
    let loaded = mem_load_bytes(tgt, mem_idx, &addr, load_bytes);

    // Extend to the target width if needed.
    let target_bits = bits_for_lir_type(&result_ty, &[]);
    if loaded.bits.len() == target_bits {
        VaffleValue { bits: loaded.bits, ty: result_ty }
    } else if sign_extend {
        tgt.sext(loaded, result_ty)
    } else {
        tgt.zext(loaded, result_ty)
    }
}

/// Lower a WAFFLE memory store operator.
fn lower_mem_store(
    tgt: &mut VaffleTarget,
    memory: &MemoryArg,
    base: &VaffleValue,
    value: &VaffleValue,
    store_bytes: usize,
) {
    let mem_idx = memory.memory.index() as u32;
    let addr = effective_addr(tgt, base, memory.offset);
    // Truncate to the store width if needed.
    let store_bits = store_bytes * 8;
    let truncated = if value.bits.len() > store_bits {
        VaffleValue {
            bits: value.bits[..store_bits].to_vec(),
            ty: value.ty.clone(),
        }
    } else {
        value.clone()
    };
    mem_store_bytes(tgt, mem_idx, &addr, &truncated, store_bytes);
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::vec;

    use super::*;
    use portal_pc_waffle_ir::{
        BlockTarget, Memory, MemoryData,
        Module as WModule, Operator, Signature, SignatureData,
        Terminator as WTerminator, Type as WType,
    };
    use portal_pc_waffle_ir::entity::EntityVec;
    use volar_ir_common::Stmt;

    /// Build a minimal WAFFLE module with one memory and a function that
    /// does `i32.store(addr=param0, val=param1)` then `i32.load(addr=param0) → return`.
    fn build_store_load_module() -> WModule<'static> {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        let sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32, WType::I32],
            returns: vec![WType::I32],
            shared: false,
        });

        let mut memories: EntityVec<Memory, MemoryData> = EntityVec::default();
        memories.push(MemoryData {
            initial_pages: 1,
            maximum_pages: None,
            segments: vec![],
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories,
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };

        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, sig);
        let entry = body.entry;

        // param0 = address, param1 = value
        let param0 = body.blocks[entry].params[0].1;
        let param1 = body.blocks[entry].params[1].1;

        let mem_arg = MemoryArg {
            align: 2,
            offset: 0,
            memory: Memory::from(0u32),
        };

        // i32.store param0, param1
        body.add_op(
            entry,
            Operator::I32Store { memory: mem_arg.clone() },
            &[param0, param1],
            &[],
        );

        // loaded = i32.load param0
        let loaded = body.add_op(
            entry,
            Operator::I32Load { memory: mem_arg },
            &[param0],
            &[WType::I32],
        );

        // return loaded
        body.set_terminator(
            entry,
            WTerminator::Return { values: vec![loaded] },
        );

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig, "store_load".into(), body,
        ));

        module
    }

    #[test]
    fn test_memory_store_load_lowering() {
        let wasm = build_store_load_module();
        let mut target = VaffleTarget::new();

        let errors = lower_waffle_module(&wasm, &mut target);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        // Should have one function in the module.
        assert_eq!(target.module.funcs.len(), 1);

        // Check that StorageRead and StorageWrite ops are present in the VAFFLE.
        let func = &target.module.funcs[0];
        let body = match func {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };

        let mut has_read = false;
        let mut has_write = false;
        for val in &body.values {
            if let vaffle::Value::Op(stmt) = val {
                match stmt {
                    Stmt::StorageRead { storage, .. } => {
                        assert_eq!(storage.0, StorageId::MEMORY_BASE);
                        has_read = true;
                    }
                    Stmt::StorageWrite { storage, .. } => {
                        assert_eq!(storage.0, StorageId::MEMORY_BASE);
                        has_write = true;
                    }
                    _ => {}
                }
            }
        }
        assert!(has_write, "VAFFLE should contain StorageWrite for i32.store");
        assert!(has_read, "VAFFLE should contain StorageRead for i32.load");
    }

    /// Build a module with i32.store8 + i32.load8_u to test sub-word memory access.
    fn build_byte_store_load_module() -> WModule<'static> {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        let sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32, WType::I32],
            returns: vec![WType::I32],
            shared: false,
        });

        let mut memories: EntityVec<Memory, MemoryData> = EntityVec::default();
        memories.push(MemoryData {
            initial_pages: 1,
            maximum_pages: None,
            segments: vec![],
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories,
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };

        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, sig);
        let entry = body.entry;
        let param0 = body.blocks[entry].params[0].1;
        let param1 = body.blocks[entry].params[1].1;

        let mem_arg = MemoryArg {
            align: 0,
            offset: 0,
            memory: Memory::from(0u32),
        };

        // i32.store8 param0, param1
        body.add_op(
            entry,
            Operator::I32Store8 { memory: mem_arg.clone() },
            &[param0, param1],
            &[],
        );

        // loaded = i32.load8_u param0
        let loaded = body.add_op(
            entry,
            Operator::I32Load8U { memory: mem_arg },
            &[param0],
            &[WType::I32],
        );

        body.set_terminator(entry, WTerminator::Return { values: vec![loaded] });

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig, "byte_store_load".into(), body,
        ));

        module
    }

    #[test]
    fn test_byte_memory_access() {
        let wasm = build_byte_store_load_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        let func = &target.module.funcs[0];
        let body = match func {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };

        // i32.store8 writes 1 byte, i32.load8_u reads 1 byte.
        // Each should produce exactly 1 StorageWrite / 1 StorageRead.
        let writes: Vec<_> = body.values.iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageWrite { .. })))
            .collect();
        let reads: Vec<_> = body.values.iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageRead { .. })))
            .collect();
        assert_eq!(writes.len(), 1, "store8 should produce exactly 1 StorageWrite");
        assert_eq!(reads.len(), 1, "load8_u should produce exactly 1 StorageRead");
    }

    #[test]
    fn test_i32_store_produces_4_byte_writes() {
        let wasm = build_store_load_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target);
        assert!(errors.is_empty());

        let body = match &target.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!(),
        };

        // i32.store writes 4 bytes → 4 StorageWrite ops.
        let writes: Vec<_> = body.values.iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageWrite { .. })))
            .collect();
        assert_eq!(writes.len(), 4, "i32.store should produce 4 StorageWrite ops (one per byte)");

        // i32.load reads 4 bytes → 4 StorageRead ops.
        let reads: Vec<_> = body.values.iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageRead { .. })))
            .collect();
        assert_eq!(reads.len(), 4, "i32.load should produce 4 StorageRead ops (one per byte)");
    }

    /// Test that MemoryArg.offset is applied correctly.
    fn build_offset_load_module() -> WModule<'static> {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        let sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32],
            returns: vec![WType::I32],
            shared: false,
        });

        let mut memories: EntityVec<Memory, MemoryData> = EntityVec::default();
        memories.push(MemoryData {
            initial_pages: 1,
            maximum_pages: None,
            segments: vec![],
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories,
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };

        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, sig);
        let entry = body.entry;
        let param0 = body.blocks[entry].params[0].1;

        let mem_arg = MemoryArg {
            align: 2,
            offset: 16, // static offset of 16 bytes
            memory: Memory::from(0u32),
        };

        let loaded = body.add_op(
            entry,
            Operator::I32Load { memory: mem_arg },
            &[param0],
            &[WType::I32],
        );

        body.set_terminator(entry, WTerminator::Return { values: vec![loaded] });

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig, "offset_load".into(), body,
        ));

        module
    }

    #[test]
    fn test_offset_load_lowering() {
        let wasm = build_offset_load_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        // The function should have produced VAFFLE with storage reads.
        // The offset computation happens via add circuits, but the key
        // property is that it lowers without error.
        let body = match &target.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!(),
        };
        let reads: Vec<_> = body.values.iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageRead { .. })))
            .collect();
        assert_eq!(reads.len(), 4, "i32.load with offset should produce 4 reads");
    }
}
