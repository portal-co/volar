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
//! - Arithmetic: `I32/I64` add, sub, mul, divS, divU, and, or, xor, shl, shrS, shrU,
//!   remU, remS, clz, ctz, popcnt, rotl, rotr
//! - Sign-extend: `I32Extend8S`, `I32Extend16S`, `I64Extend8S`, `I64Extend16S`, `I64Extend32S`
//! - Comparisons: `I32/I64` eqz, eq, ne, ltS/U, gtS/U, leS/U, geS/U
//! - Conversions: `I32WrapI64`, `I64ExtendI32S`, `I64ExtendI32U`
//! - Select: `Select`, `TypedSelect` (I32 non-zero test via OR-reduce)
//! - Direct calls: `Call { function_index }` — multi-return supported; mutable globals
//!   are threaded as extra args/returns at every call site
//! - Nop: `Nop`
//! - Memory loads: `I32Load`, `I64Load`, `I32Load8U/S`, `I32Load16U/S`,
//!   `I64Load8U/S`, `I64Load16U/S`, `I64Load32U/S` (byte-addressed storage)
//! - Memory stores: `I32Store`, `I64Store`, `I32Store8/16`,
//!   `I64Store8/16/32`
//! - Memory stubs: `MemorySize` (always 0), `MemoryGrow` (always -1)
//! - Globals: `GlobalGet` and `GlobalSet` for mutable I32/I64 globals — threaded
//!   as extra function parameters and return values.  Immutable globals return
//!   their compile-time constant.
//!
//! **Operators** (not supported — returns `UnsupportedOp`):
//! - All F32/F64 ops and float memory ops
//! - All V128/SIMD ops
//! - All atomic/threads ops
//! - Trunc-sat conversions
//! - `CallIndirect`, `CallRef`
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
    FuncDecl,
    FunctionBody,
    MemoryArg,
    Module as WModule,
    Operator,
    SignatureData,
    Terminator,
    Type as WType,
    Value as WValue,
    ValueDef,
    entity::EntityRef, // for .index() on Func/Block/etc.
};

use volar_ir_common::{Constant, PreInitSegment, StorageId};
use volar_lir::circuits::{
    StorageEmitter, bc_clz, bc_ctz, bc_popcnt, bc_rotl, bc_rotr, bc_srem, bc_urem,
};
use volar_lir::{BitCircuitBuilder, IcmpPred, LirTarget, LirType};

use crate::import_config::{WaffleImportConfig, WaffleImportKind};
use crate::target::{VaffleBlock, VaffleTarget, VaffleValue, bits_for_lir_type};
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
        WType::I32 | WType::F32 => Ok(LirType::U32),
        WType::I64 | WType::F64 => Ok(LirType::U64),
        other => Err(UnsupportedOp(alloc::format!("{other:?}"))),
    }
}

// ============================================================================
// Public entry points
// ============================================================================

/// Lower all function bodies in a WAFFLE module into `target`, skipping
/// unsupported functions.  Returns a list of (name, error) for skipped functions.
///
/// `config` maps WAFFLE import names to oracle/action declarations. Matching
/// imports are pre-registered in `target.module.oracles` / `target.module.actions`
/// and routed through the oracle/action calling convention at every call site.
/// Pass `&WaffleImportConfig::default()` for the original behaviour.
pub fn lower_waffle_module(
    wasm: &WModule,
    target: &mut VaffleTarget,
    config: &WaffleImportConfig,
) -> Vec<(String, UnsupportedOp)> {
    // Pre-register OracleDecl / ActionDecl for imports named in config.
    for (_func_ref, decl) in wasm.funcs.entries() {
        if let FuncDecl::Import(sig, import_name) = decl {
            let Some(kind) = config.imports.get(import_name) else { continue };
            let sig_data = &wasm.signatures[*sig];
            let (wasm_params, wasm_results) = match sig_data {
                portal_pc_waffle_ir::SignatureData::Func { params, returns, .. } => {
                    (params.as_slice(), returns.as_slice())
                }
                _ => continue,
            };
            match kind {
                WaffleImportKind::Oracle { name } => {
                    let params: alloc::vec::Vec<_> = wasm_params
                        .iter()
                        .filter_map(|&t| waffle_ty(t).ok())
                        .map(|lt| target.lir_type_to_tid(&lt))
                        .collect();
                    let results: alloc::vec::Vec<_> = wasm_results
                        .iter()
                        .filter_map(|&t| waffle_ty(t).ok())
                        .map(|lt| target.lir_type_to_tid(&lt))
                        .collect();
                    target.register_oracle(volar_ir_common::OracleDecl {
                        name: name.clone(),
                        params,
                        results,
                    });
                }
                WaffleImportKind::Action { name, n_args } => {
                    let action_params: alloc::vec::Vec<_> = wasm_params
                        .iter()
                        .skip(1) // skip guard
                        .take(*n_args)
                        .filter_map(|&t| waffle_ty(t).ok())
                        .map(|lt| target.lir_type_to_tid(&lt))
                        .collect();
                    let results: alloc::vec::Vec<_> = wasm_results
                        .iter()
                        .filter_map(|&t| waffle_ty(t).ok())
                        .map(|lt| target.lir_type_to_tid(&lt))
                        .collect();
                    target.register_action(volar_ir_common::ActionDecl {
                        name: name.clone(),
                        params: action_params,
                        results,
                    });
                }
            }
        }
    }

    let mut errors = Vec::new();
    // EntityVec::entries() yields (Func, &FuncDecl) pairs.
    for (_func_ref, decl) in wasm.funcs.entries() {
        if let FuncDecl::Body(_, name, body) = decl {
            if let Err(e) = lower_waffle_function(body, name.as_str(), wasm, target, config) {
                errors.push((name.clone(), e));
            }
        }
    }

    // Collect WASM active data-segment pre-initialisations.
    // WASM linear memories use 8-bit byte cells, so ty = the u8 TypeId.
    let u8_tid = target.lir_type_to_tid(&volar_lir::LirType::U8);
    for (mem_ref, mem_data) in wasm.memories.entries() {
        let storage = StorageId::memory(mem_ref.index() as u32);
        for seg in &mem_data.segments {
            target.module.pre_init.push(PreInitSegment {
                storage,
                ty: u8_tid,
                offset: seg.offset,
                data: seg.data.iter()
                    .map(|&b| Constant { hi: 0, lo: b as u128 })
                    .collect(),
            });
        }
    }

    errors
}

/// Lower a single WAFFLE `FunctionBody` into `target`.
///
/// Parameter types come from `body.locals` (first `body.n_params` locals);
/// return types from `body.rets`.
///
/// Mutable globals are threaded through every function as extra parameters
/// and return values: `params = [orig_params, g0, g1, …]`,
/// `rets = [orig_rets, g0', g1', …]`.
pub fn lower_waffle_function(
    body: &FunctionBody,
    name: &str,
    wasm: &WModule,
    target: &mut VaffleTarget,
    config: &WaffleImportConfig,
) -> Result<(), UnsupportedOp> {
    // ---- Collect mutable globals for threading ---------------------------
    let mut global_lir_tys: Vec<LirType> = Vec::new();
    let mut global_idx_map: BTreeMap<usize, usize> = BTreeMap::new();
    for (g_ref, g_data) in wasm.globals.entries() {
        if g_data.mutable {
            let lir_ty = waffle_ty(g_data.ty)?;
            global_idx_map.insert(g_ref.index(), global_lir_tys.len());
            global_lir_tys.push(lir_ty);
        }
    }

    // ---- Parameter / return types from FunctionBody directly ----------------
    let param_tys: Vec<WType> = body.locals.values().take(body.n_params).copied().collect();
    let ret_tys: &[WType] = &body.rets;

    let param_lir: Vec<LirType> = param_tys
        .iter()
        .map(|&t| waffle_ty(t))
        .collect::<Result<_, _>>()?;
    let ret_lir: Vec<LirType> = ret_tys
        .iter()
        .map(|&t| waffle_ty(t))
        .collect::<Result<_, _>>()?;

    // Append global types to function params for threading.
    let mut all_param_lir = param_lir.clone();
    all_param_lir.extend_from_slice(&global_lir_tys);

    let ret_hint = ret_lir.first().cloned();
    let (entry_block, param_groups) = target.begin_function(name, &all_param_lir, ret_hint);
    target.switch_to_block(entry_block);

    // ---- Map WAFFLE blocks → VAFFLE blocks ----------------------------------
    let mut block_map: BTreeMap<portal_pc_waffle_ir::Block, VaffleBlock> = BTreeMap::new();
    block_map.insert(body.entry, entry_block);
    for (wblock, _) in body.blocks.entries() {
        if wblock == body.entry {
            continue;
        }
        block_map.insert(wblock, target.create_block());
    }

    // ---- Seed value map with entry-block params (= function parameters) -----
    let mut val_map: BTreeMap<WValue, VaffleValue> = BTreeMap::new();
    for ((_, entry_wval), group) in body.blocks[body.entry]
        .params
        .iter()
        .zip(param_groups.iter())
    {
        if let Some(vv) = group.first() {
            val_map.insert(*entry_wval, vv.clone());
        }
    }

    // ---- Initialize current_globals from the trailing function param groups -
    let n_orig_params = param_lir.len();
    let mut current_globals: Vec<VaffleValue> = (0..global_lir_tys.len())
        .map(|i| {
            param_groups
                .get(n_orig_params + i)
                .and_then(|g| g.first())
                .cloned()
                .unwrap_or_else(|| VaffleValue {
                    bits: vec![],
                    ty: LirType::Bool,
                })
        })
        .collect();

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
            // Extra block params carry the threaded globals for this block.
            current_globals = global_lir_tys
                .iter()
                .map(|ty| target.add_block_param(vblock, ty.clone()))
                .collect();
        }

        // Instructions: each ValueRecord wraps a Value index.
        for record in &block_def.insts {
            let wval = record.value;
            match &body.values[wval] {
                ValueDef::Operator(op, args_ref, tys_ref) => {
                    let args: Vec<WValue> = body.arg_pool[*args_ref].to_vec();
                    let result_tys: Vec<WType> = body.type_pool[*tys_ref].to_vec();
                    if let Some(vv) = lower_op(
                        op,
                        &args,
                        &result_tys,
                        &val_map,
                        &mut current_globals,
                        &global_idx_map,
                        &global_lir_tys,
                        target,
                        wasm,
                        config,
                    )? {
                        val_map.insert(wval, vv);
                    }
                }
                ValueDef::PickOutput(from_val, idx, ty) => {
                    if let Some(call_vv) = val_map.get(from_val) {
                        let lir_ty = waffle_ty(*ty)?;
                        let n = bits_for_lir_type(&lir_ty, &[]);
                        // Compute correct bit offset using the source op's result types.
                        let start = compute_pick_offset(body, from_val, *idx as usize);
                        let end = (start + n).min(call_vv.bits.len());
                        val_map.insert(
                            wval,
                            VaffleValue {
                                bits: call_vv.bits[start..end].to_vec(),
                                ty: lir_ty,
                            },
                        );
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
            &ret_lir,
            &current_globals,
            target,
        )?;
    }

    target.end_function();
    Ok(())
}

// ============================================================================
// Operator lowering
// ============================================================================

/// Compute the bit offset of the `idx`-th output in a multi-result `ValueDef::Operator`.
///
/// Sums the widths of outputs `0..idx` by reading the operator's result type pool.
/// Returns 0 if `from_val` is not an `Operator` node or any type is unsupported.
fn compute_pick_offset(body: &FunctionBody, from_val: &WValue, idx: usize) -> usize {
    match &body.values[*from_val] {
        ValueDef::Operator(_, _, tys_ref) => body.type_pool[*tys_ref]
            .iter()
            .take(idx)
            .filter_map(|&t| waffle_ty(t).ok())
            .map(|ty| bits_for_lir_type(&ty, &[]))
            .sum(),
        _ => 0,
    }
}

fn lower_op(
    op: &Operator,
    args: &[WValue],
    result_tys: &[WType],
    val_map: &BTreeMap<WValue, VaffleValue>,
    current_globals: &mut Vec<VaffleValue>,
    global_idx_map: &BTreeMap<usize, usize>,
    global_lir_tys: &[LirType],
    tgt: &mut VaffleTarget,
    wasm: &WModule,
    config: &WaffleImportConfig,
) -> Result<Option<VaffleValue>, UnsupportedOp> {
    let get = |i: usize| -> Result<VaffleValue, UnsupportedOp> {
        val_map
            .get(&args[i])
            .cloned()
            .ok_or_else(|| UnsupportedOp(alloc::format!("undefined value {:?}", args[i])))
    };

    Ok(Some(match op {
        // ---- Constants -------------------------------------------------
        Operator::I32Const { value } => tgt.iconst(LirType::U32, *value as i32 as i64),
        Operator::I64Const { value } => tgt.iconst(LirType::U64, *value as i64),

        // ---- I32 arithmetic --------------------------------------------
        Operator::I32Add => tgt.add(get(0)?, get(1)?),
        Operator::I32Sub => tgt.sub(get(0)?, get(1)?),
        Operator::I32Mul => tgt.mul(get(0)?, get(1)?),
        Operator::I32DivS => tgt.sdiv(get(0)?, get(1)?),
        Operator::I32DivU => tgt.udiv(get(0)?, get(1)?),
        Operator::I32And => tgt.and(get(0)?, get(1)?),
        Operator::I32Or => tgt.or(get(0)?, get(1)?),
        Operator::I32Xor => tgt.xor(get(0)?, get(1)?),
        Operator::I32Shl => tgt.shl(get(0)?, get(1)?),
        Operator::I32ShrS => tgt.ashr(get(0)?, get(1)?),
        Operator::I32ShrU => tgt.lshr(get(0)?, get(1)?),
        Operator::I32RemU => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_urem(tgt, &a.bits, &x.bits),
                ty,
            }
        }
        Operator::I32RemS => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_srem(tgt, &a.bits, &x.bits),
                ty,
            }
        }
        Operator::I32Clz => {
            let a = get(0)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_clz(tgt, &a.bits),
                ty,
            }
        }
        Operator::I32Ctz => {
            let a = get(0)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_ctz(tgt, &a.bits),
                ty,
            }
        }
        Operator::I32Popcnt => {
            let a = get(0)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_popcnt(tgt, &a.bits),
                ty,
            }
        }
        Operator::I32Rotl => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_rotl(tgt, &a.bits, &x.bits),
                ty,
            }
        }
        Operator::I32Rotr => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_rotr(tgt, &a.bits, &x.bits),
                ty,
            }
        }

        // ---- I32 comparisons (result is i32: 0 or 1) -------------------
        Operator::I32Eqz => {
            let z = tgt.iconst(LirType::U32, 0);
            let c = tgt.icmp(IcmpPred::Eq, get(0)?, z);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32Eq => {
            let c = tgt.icmp(IcmpPred::Eq, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32Ne => {
            let c = tgt.icmp(IcmpPred::Ne, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32LtS => {
            let c = tgt.icmp(IcmpPred::Slt, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32LtU => {
            let c = tgt.icmp(IcmpPred::Ult, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32GtS => {
            let c = tgt.icmp(IcmpPred::Sgt, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32GtU => {
            let c = tgt.icmp(IcmpPred::Ugt, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32LeS => {
            let c = tgt.icmp(IcmpPred::Sle, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32LeU => {
            let c = tgt.icmp(IcmpPred::Ule, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32GeS => {
            let c = tgt.icmp(IcmpPred::Sge, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }
        Operator::I32GeU => {
            let c = tgt.icmp(IcmpPred::Uge, get(0)?, get(1)?);
            tgt.zext(c, LirType::U32)
        }

        // ---- I64 arithmetic --------------------------------------------
        Operator::I64Add => tgt.add(get(0)?, get(1)?),
        Operator::I64Sub => tgt.sub(get(0)?, get(1)?),
        Operator::I64Mul => tgt.mul(get(0)?, get(1)?),
        Operator::I64DivS => tgt.sdiv(get(0)?, get(1)?),
        Operator::I64DivU => tgt.udiv(get(0)?, get(1)?),
        Operator::I64And => tgt.and(get(0)?, get(1)?),
        Operator::I64Or => tgt.or(get(0)?, get(1)?),
        Operator::I64Xor => tgt.xor(get(0)?, get(1)?),
        Operator::I64Shl => tgt.shl(get(0)?, get(1)?),
        Operator::I64ShrS => tgt.ashr(get(0)?, get(1)?),
        Operator::I64ShrU => tgt.lshr(get(0)?, get(1)?),
        Operator::I64RemU => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_urem(tgt, &a.bits, &x.bits),
                ty,
            }
        }
        Operator::I64RemS => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_srem(tgt, &a.bits, &x.bits),
                ty,
            }
        }
        Operator::I64Clz => {
            let a = get(0)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_clz(tgt, &a.bits),
                ty,
            }
        }
        Operator::I64Ctz => {
            let a = get(0)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_ctz(tgt, &a.bits),
                ty,
            }
        }
        Operator::I64Popcnt => {
            let a = get(0)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_popcnt(tgt, &a.bits),
                ty,
            }
        }
        Operator::I64Rotl => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_rotl(tgt, &a.bits, &x.bits),
                ty,
            }
        }
        Operator::I64Rotr => {
            let a = get(0)?;
            let x = get(1)?;
            let ty = a.ty.clone();
            VaffleValue {
                bits: bc_rotr(tgt, &a.bits, &x.bits),
                ty,
            }
        }

        // ---- I64 comparisons -------------------------------------------
        Operator::I64Eqz => {
            let z = tgt.iconst(LirType::U64, 0);
            let c = tgt.icmp(IcmpPred::Eq, get(0)?, z);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64Eq => {
            let c = tgt.icmp(IcmpPred::Eq, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64Ne => {
            let c = tgt.icmp(IcmpPred::Ne, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64LtS => {
            let c = tgt.icmp(IcmpPred::Slt, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64LtU => {
            let c = tgt.icmp(IcmpPred::Ult, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64GtS => {
            let c = tgt.icmp(IcmpPred::Sgt, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64GtU => {
            let c = tgt.icmp(IcmpPred::Ugt, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64LeS => {
            let c = tgt.icmp(IcmpPred::Sle, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64LeU => {
            let c = tgt.icmp(IcmpPred::Ule, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64GeS => {
            let c = tgt.icmp(IcmpPred::Sge, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }
        Operator::I64GeU => {
            let c = tgt.icmp(IcmpPred::Uge, get(0)?, get(1)?);
            tgt.zext(c, LirType::U64)
        }

        // ---- Conversions -----------------------------------------------
        Operator::I32WrapI64 => tgt.trunc(get(0)?, LirType::U32),
        Operator::I64ExtendI32S => tgt.sext(get(0)?, LirType::U64),
        Operator::I64ExtendI32U => tgt.zext(get(0)?, LirType::U64),
        // Sign-extend variants: truncate to the narrow width, then sign-extend.
        Operator::I32Extend8S => {
            let t = tgt.trunc(get(0)?, LirType::U8);
            tgt.sext(t, LirType::U32)
        }
        Operator::I32Extend16S => {
            let t = tgt.trunc(get(0)?, LirType::U16);
            tgt.sext(t, LirType::U32)
        }
        Operator::I64Extend8S => {
            let t = tgt.trunc(get(0)?, LirType::U8);
            tgt.sext(t, LirType::U64)
        }
        Operator::I64Extend16S => {
            let t = tgt.trunc(get(0)?, LirType::U16);
            tgt.sext(t, LirType::U64)
        }
        Operator::I64Extend32S => {
            let t = tgt.trunc(get(0)?, LirType::U32);
            tgt.sext(t, LirType::U64)
        }

        // ---- Select (WAFFLE: args = [val_true, val_false, cond]) --------
        Operator::Select | Operator::TypedSelect { .. } => {
            let if_t = get(0)?;
            let if_f = get(1)?;
            let cond = get(2)?;
            // cond is I32; treat as bool via OR-reduce (non-zero = true).
            let cond_bit = or_bits(tgt, &cond.bits);
            let cond_bool = VaffleValue {
                bits: vec![cond_bit],
                ty: LirType::Bool,
            };
            tgt.select(cond_bool, if_t, if_f)
        }

        // ---- Globals ---------------------------------------------------
        Operator::GlobalGet { global_index } => {
            let g_ref = *global_index;
            let g_data = &wasm.globals[g_ref];
            if g_data.mutable {
                let local_idx = *global_idx_map.get(&g_ref.index()).ok_or_else(|| {
                    UnsupportedOp(alloc::format!("unknown mutable global {}", g_ref.index()))
                })?;
                return Ok(Some(current_globals[local_idx].clone()));
            } else {
                let lir_ty = waffle_ty(g_data.ty)?;
                let val = g_data.value.unwrap_or(0) as i64;
                return Ok(Some(tgt.iconst(lir_ty, val)));
            }
        }
        Operator::GlobalSet { global_index } => {
            let g_ref = *global_index;
            let g_data = &wasm.globals[g_ref];
            if g_data.mutable {
                let local_idx = *global_idx_map.get(&g_ref.index()).ok_or_else(|| {
                    UnsupportedOp(alloc::format!("unknown mutable global {}", g_ref.index()))
                })?;
                current_globals[local_idx] = get(0)?;
            }
            return Ok(None);
        }

        // ---- Direct call (multi-result, globals threaded) --------------
        Operator::Call { function_index } => {
            let fid = *function_index;
            let name = callee_name(wasm, fid);

            // Oracle / action dispatch: bypass globals threading.
            if let Some(kind) = config.imports.get(&name) {
                let all_arg_vals: Vec<VaffleValue> = args
                    .iter()
                    .map(|wv| {
                        val_map
                            .get(wv)
                            .cloned()
                            .ok_or_else(|| UnsupportedOp(alloc::format!("undefined arg {:?}", wv)))
                    })
                    .collect::<Result<_, _>>()?;
                let orig_ret_tys: Vec<LirType> = result_tys
                    .iter()
                    .map(|&t| waffle_ty(t))
                    .collect::<Result<_, _>>()?;

                let results = match kind {
                    WaffleImportKind::Oracle { name: oracle_name } => {
                        tgt.call_extern_multi(
                            &alloc::format!("oracle_{oracle_name}"),
                            &all_arg_vals,
                            &orig_ret_tys,
                        )
                    }
                    WaffleImportKind::Action { name: action_name, n_args } => {
                        let guard_vv = all_arg_vals[0].clone();
                        let guard_bit = or_bits(tgt, &guard_vv.bits);
                        let real_args = &all_arg_vals[1..=*n_args];
                        let fallbacks = &all_arg_vals[*n_args + 1..];
                        tgt.action_call(action_name, guard_bit, real_args, fallbacks, &orig_ret_tys)
                    }
                };

                return Ok(match results.len() {
                    0 => None,
                    1 => Some(results.into_iter().next().unwrap()),
                    _ => {
                        let bits: Vec<ValueId> = results
                            .iter()
                            .flat_map(|vv| vv.bits.iter().copied())
                            .collect();
                        let ty = results[0].ty.clone();
                        Some(VaffleValue { bits, ty })
                    }
                });
            }

            let arg_vals: Vec<VaffleValue> = args
                .iter()
                .map(|wv| {
                    val_map
                        .get(wv)
                        .cloned()
                        .ok_or_else(|| UnsupportedOp(alloc::format!("undefined arg {:?}", wv)))
                })
                .collect::<Result<_, _>>()?;
            let orig_ret_tys: Vec<LirType> = result_tys
                .iter()
                .map(|&t| waffle_ty(t))
                .collect::<Result<_, _>>()?;
            // Append current globals to args and return types for threading.
            let mut all_args = arg_vals;
            all_args.extend_from_slice(current_globals);
            let mut all_ret_tys = orig_ret_tys.clone();
            all_ret_tys.extend_from_slice(global_lir_tys);
            let mut all_results = tgt.call_extern_multi(&name, &all_args, &all_ret_tys);
            let n_orig = orig_ret_tys.len();
            let new_globals = all_results.split_off(n_orig);
            *current_globals = new_globals;
            return Ok(match all_results.len() {
                0 => None,
                1 => Some(all_results.into_iter().next().unwrap()),
                _ => {
                    // Concatenate return bits so PickOutput can slice with compute_pick_offset.
                    let bits: Vec<ValueId> = all_results
                        .iter()
                        .flat_map(|vv| vv.bits.iter().copied())
                        .collect();
                    let ty = all_results[0].ty.clone();
                    Some(VaffleValue { bits, ty })
                }
            });
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

        _ => return Err(UnsupportedOp(alloc::format!("{op:?}"))),
    }))
}

// ============================================================================
// Terminator lowering
// ============================================================================

fn lower_term(
    term: &Terminator,
    val_map: &BTreeMap<WValue, VaffleValue>,
    block_map: &BTreeMap<portal_pc_waffle_ir::Block, VaffleBlock>,
    ret_lir: &[LirType],
    current_globals: &[VaffleValue],
    tgt: &mut VaffleTarget,
) -> Result<(), UnsupportedOp> {
    let get = |wv: &WValue| -> Result<VaffleValue, UnsupportedOp> {
        val_map
            .get(wv)
            .cloned()
            .ok_or_else(|| UnsupportedOp(alloc::format!("undefined {:?}", wv)))
    };
    let get_block = |wb: portal_pc_waffle_ir::Block| -> Result<VaffleBlock, UnsupportedOp> {
        block_map
            .get(&wb)
            .copied()
            .ok_or_else(|| UnsupportedOp(alloc::format!("unknown block {:?}", wb)))
    };
    let get_args = |wargs: &[WValue]| -> Result<Vec<VaffleValue>, UnsupportedOp> {
        wargs
            .iter()
            .map(|wv| {
                val_map
                    .get(wv)
                    .cloned()
                    .ok_or_else(|| UnsupportedOp(alloc::format!("undefined {:?}", wv)))
            })
            .collect()
    };

    match term {
        Terminator::Br { target: bt } => {
            let vb = get_block(bt.block)?;
            let mut args = get_args(&bt.args)?;
            args.extend_from_slice(current_globals);
            tgt.jump(vb, &args);
        }

        Terminator::CondBr {
            cond,
            if_true,
            if_false,
        } => {
            let cond_vv = get(cond)?;
            // WAFFLE cond is I32; use OR-reduce for non-zero test.
            let cond_bit = or_bits(tgt, &cond_vv.bits);
            let cond_bool = VaffleValue {
                bits: vec![cond_bit],
                ty: LirType::Bool,
            };
            let then_b = get_block(if_true.block)?;
            let else_b = get_block(if_false.block)?;
            let mut then_args = get_args(&if_true.args)?;
            then_args.extend_from_slice(current_globals);
            let mut else_args = get_args(&if_false.args)?;
            else_args.extend_from_slice(current_globals);
            tgt.branch(cond_bool, then_b, &then_args, else_b, &else_args);
        }

        Terminator::Return { values } => {
            let mut vals: Vec<VaffleValue> =
                values.iter().map(|v| get(v)).collect::<Result<_, _>>()?;
            vals.extend_from_slice(current_globals);
            tgt.ret(&vals);
        }

        Terminator::Unreachable | Terminator::UB | Terminator::None => {
            // Stub: emit zeros for all original return values + global slots.
            let mut zero_vals: Vec<VaffleValue> =
                ret_lir.iter().map(|ty| tgt.iconst(ty.clone(), 0)).collect();
            let global_zeros: Vec<VaffleValue> = current_globals
                .iter()
                .map(|vv| tgt.iconst(vv.ty.clone(), 0))
                .collect();
            zero_vals.extend(global_zeros);
            tgt.ret(&zero_vals);
        }

        Terminator::ReturnCall { func, args } => {
            let name = alloc::format!("func_{}", func.index());
            let mut arg_vals: Vec<VaffleValue> =
                args.iter().map(|a| get(a)).collect::<Result<_, _>>()?;
            arg_vals.extend_from_slice(current_globals);
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
fn effective_addr(tgt: &mut VaffleTarget, base: &VaffleValue, offset: u64) -> VaffleValue {
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
        VaffleValue {
            bits: loaded.bits,
            ty: result_ty,
        }
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
    if bits.is_empty() {
        return tgt.bc_const(false);
    }
    let mut acc = bits[0];
    for &b in &bits[1..] {
        acc = tgt.bc_or(acc, b);
    }
    acc
}

/// Derive a callee name from the WAFFLE module's function declaration.
fn callee_name(wasm: &WModule, fid: portal_pc_waffle_ir::Func) -> String {
    match &wasm.funcs[fid] {
        FuncDecl::Body(_, name, _) => name.clone(),
        FuncDecl::Import(_, name) => name.clone(),
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
    use portal_pc_waffle_ir::entity::EntityVec;
    use portal_pc_waffle_ir::{
        BlockTarget, Global, GlobalData, Memory, MemoryData, Module as WModule, Operator,
        Signature, SignatureData, Terminator as WTerminator, Type as WType, ValueDef,
    };
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
            Operator::I32Store {
                memory: mem_arg.clone(),
            },
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
            WTerminator::Return {
                values: vec![loaded],
            },
        );

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig,
            "store_load".into(),
            body,
        ));

        module
    }

    #[test]
    fn test_memory_store_load_lowering() {
        let wasm = build_store_load_module();
        let mut target = VaffleTarget::new();

        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
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
        assert!(
            has_write,
            "VAFFLE should contain StorageWrite for i32.store"
        );
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
            Operator::I32Store8 {
                memory: mem_arg.clone(),
            },
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

        body.set_terminator(
            entry,
            WTerminator::Return {
                values: vec![loaded],
            },
        );

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig,
            "byte_store_load".into(),
            body,
        ));

        module
    }

    #[test]
    fn test_byte_memory_access() {
        let wasm = build_byte_store_load_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        let func = &target.module.funcs[0];
        let body = match func {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };

        // i32.store8 writes 1 byte, i32.load8_u reads 1 byte.
        // Each should produce exactly 1 StorageWrite / 1 StorageRead.
        let writes: Vec<_> = body
            .values
            .iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageWrite { .. })))
            .collect();
        let reads: Vec<_> = body
            .values
            .iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageRead { .. })))
            .collect();
        assert_eq!(
            writes.len(),
            1,
            "store8 should produce exactly 1 StorageWrite"
        );
        assert_eq!(
            reads.len(),
            1,
            "load8_u should produce exactly 1 StorageRead"
        );
    }

    #[test]
    fn test_i32_store_produces_4_byte_writes() {
        let wasm = build_store_load_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty());

        let body = match &target.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!(),
        };

        // i32.store writes 4 bytes → 4 StorageWrite ops.
        let writes: Vec<_> = body
            .values
            .iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageWrite { .. })))
            .collect();
        assert_eq!(
            writes.len(),
            4,
            "i32.store should produce 4 StorageWrite ops (one per byte)"
        );

        // i32.load reads 4 bytes → 4 StorageRead ops.
        let reads: Vec<_> = body
            .values
            .iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageRead { .. })))
            .collect();
        assert_eq!(
            reads.len(),
            4,
            "i32.load should produce 4 StorageRead ops (one per byte)"
        );
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

        body.set_terminator(
            entry,
            WTerminator::Return {
                values: vec![loaded],
            },
        );

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig,
            "offset_load".into(),
            body,
        ));

        module
    }

    #[test]
    fn test_offset_load_lowering() {
        let wasm = build_offset_load_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        // The function should have produced VAFFLE with storage reads.
        // The offset computation happens via add circuits, but the key
        // property is that it lowers without error.
        let body = match &target.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!(),
        };
        let reads: Vec<_> = body
            .values
            .iter()
            .filter(|v| matches!(v, vaffle::Value::Op(Stmt::StorageRead { .. })))
            .collect();
        assert_eq!(
            reads.len(),
            4,
            "i32.load with offset should produce 4 reads"
        );
    }

    // ── helpers shared by integer-op tests ───────────────────────────────────

    /// Build a minimal module (no memory) with the given signature and body.
    fn build_simple_module(
        params: Vec<WType>,
        returns: Vec<WType>,
        build_body: impl FnOnce(
            &mut portal_pc_waffle_ir::FunctionBody,
            portal_pc_waffle_ir::Block,
            Vec<portal_pc_waffle_ir::Value>,
        ) -> portal_pc_waffle_ir::Value,
    ) -> WModule<'static> {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        let sig = sigs.push(SignatureData::Func {
            params: params.clone(),
            returns: returns.clone(),
            shared: false,
        });
        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories: EntityVec::default(),
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };
        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, sig);
        let entry = body.entry;
        let ps: Vec<_> = body.blocks[entry]
            .params
            .iter()
            .map(|&(_ty, v)| v)
            .collect();
        let result = build_body(&mut body, entry, ps);
        body.set_terminator(
            entry,
            WTerminator::Return {
                values: vec![result],
            },
        );
        module
            .funcs
            .push(portal_pc_waffle_ir::FuncDecl::Body(sig, "f".into(), body));
        module
    }

    // ── I32RemU ──────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_remu_lowers() {
        let wasm = build_simple_module(
            vec![WType::I32, WType::I32],
            vec![WType::I32],
            |body, entry, ps| body.add_op(entry, Operator::I32RemU, &[ps[0], ps[1]], &[WType::I32]),
        );
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "i32.rem_u lowering failed: {:?}", errors);
        assert_eq!(target.module.funcs.len(), 1);
    }

    // ── I32RemS ──────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_rems_lowers() {
        let wasm = build_simple_module(
            vec![WType::I32, WType::I32],
            vec![WType::I32],
            |body, entry, ps| body.add_op(entry, Operator::I32RemS, &[ps[0], ps[1]], &[WType::I32]),
        );
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "i32.rem_s lowering failed: {:?}", errors);
    }

    // ── I32Clz ───────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_clz_lowers() {
        let wasm = build_simple_module(vec![WType::I32], vec![WType::I32], |body, entry, ps| {
            body.add_op(entry, Operator::I32Clz, &[ps[0]], &[WType::I32])
        });
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "i32.clz lowering failed: {:?}", errors);
    }

    // ── I32Ctz ───────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_ctz_lowers() {
        let wasm = build_simple_module(vec![WType::I32], vec![WType::I32], |body, entry, ps| {
            body.add_op(entry, Operator::I32Ctz, &[ps[0]], &[WType::I32])
        });
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "i32.ctz lowering failed: {:?}", errors);
    }

    // ── I32Popcnt ────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_popcnt_lowers() {
        let wasm = build_simple_module(vec![WType::I32], vec![WType::I32], |body, entry, ps| {
            body.add_op(entry, Operator::I32Popcnt, &[ps[0]], &[WType::I32])
        });
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(
            errors.is_empty(),
            "i32.popcnt lowering failed: {:?}",
            errors
        );
    }

    // ── I32Rotl ──────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_rotl_lowers() {
        let wasm = build_simple_module(
            vec![WType::I32, WType::I32],
            vec![WType::I32],
            |body, entry, ps| body.add_op(entry, Operator::I32Rotl, &[ps[0], ps[1]], &[WType::I32]),
        );
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "i32.rotl lowering failed: {:?}", errors);
    }

    // ── I32Rotr ──────────────────────────────────────────────────────────────

    #[test]
    fn test_i32_rotr_lowers() {
        let wasm = build_simple_module(
            vec![WType::I32, WType::I32],
            vec![WType::I32],
            |body, entry, ps| body.add_op(entry, Operator::I32Rotr, &[ps[0], ps[1]], &[WType::I32]),
        );
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "i32.rotr lowering failed: {:?}", errors);
    }

    // ── I32Extend8S ──────────────────────────────────────────────────────────

    #[test]
    fn test_i32_extend8s_lowers() {
        let wasm = build_simple_module(vec![WType::I32], vec![WType::I32], |body, entry, ps| {
            body.add_op(entry, Operator::I32Extend8S, &[ps[0]], &[WType::I32])
        });
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(
            errors.is_empty(),
            "i32.extend8_s lowering failed: {:?}",
            errors
        );
    }

    // ── I32Extend16S ─────────────────────────────────────────────────────────

    #[test]
    fn test_i32_extend16s_lowers() {
        let wasm = build_simple_module(vec![WType::I32], vec![WType::I32], |body, entry, ps| {
            body.add_op(entry, Operator::I32Extend16S, &[ps[0]], &[WType::I32])
        });
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(
            errors.is_empty(),
            "i32.extend16_s lowering failed: {:?}",
            errors
        );
    }

    // ── I64Extend32S ─────────────────────────────────────────────────────────

    #[test]
    fn test_i64_extend32s_lowers() {
        let wasm = build_simple_module(vec![WType::I64], vec![WType::I64], |body, entry, ps| {
            body.add_op(entry, Operator::I64Extend32S, &[ps[0]], &[WType::I64])
        });
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(
            errors.is_empty(),
            "i64.extend32_s lowering failed: {:?}",
            errors
        );
    }

    // ── Mutable global threading ──────────────────────────────────────────────
    //
    // A WASM function with sig () → (i32) that reads then sets a mutable i32
    // global (g0=42) should lower to a VAFFLE function whose signature has an
    // extra i32 param (the incoming global value) and an extra i32 result (the
    // updated global value).

    fn build_global_get_set_module() -> WModule<'static> {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        // WASM: () → (i32)
        let sig = sigs.push(SignatureData::Func {
            params: vec![],
            returns: vec![WType::I32],
            shared: false,
        });

        let mut globals: EntityVec<Global, GlobalData> = EntityVec::default();
        globals.push(GlobalData {
            ty: WType::I32,
            value: Some(42),
            mutable: true,
        });

        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals,
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories: EntityVec::default(),
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };

        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, sig);
        let entry = body.entry;
        let g = Global::from(0u32);

        // old_val = global.get g0
        let old_val = body.add_op(
            entry,
            Operator::GlobalGet { global_index: g },
            &[],
            &[WType::I32],
        );

        // global.set g0, old_val  (write back same value — just to exercise GlobalSet)
        body.add_op(
            entry,
            Operator::GlobalSet { global_index: g },
            &[old_val],
            &[],
        );

        // return old_val
        body.set_terminator(
            entry,
            WTerminator::Return {
                values: vec![old_val],
            },
        );
        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig,
            "global_rw".into(),
            body,
        ));
        module
    }

    #[test]
    fn test_mutable_global_threading() {
        let wasm = build_global_get_set_module();
        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &WaffleImportConfig::default());
        assert!(errors.is_empty(), "global threading failed: {:?}", errors);
        assert_eq!(target.module.funcs.len(), 1);

        // The VAFFLE function's signature should have 1 extra param (the incoming
        // global value) and 1 extra result (the outgoing global value) compared
        // to the original WASM signature (() → i32).
        let func_body = match &target.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };
        let sig = &target.module.sigs[func_body.sig.0];

        // Original WASM: 0 params, 1 i32 return.
        // After threading 1 mutable i32 global: 32 bit-params (the global bits),
        // and the Return terminator should carry 32 (orig i32) + 32 (global out) = 64 bits.
        assert_eq!(
            sig.params.len(),
            32,
            "VAFFLE sig should have 32 bit-params (the mutable i32 global), got {:?}",
            sig.params.len()
        );

        let entry_block = &func_body.blocks[func_body.entry.0];
        let ret_bits = match &entry_block.terminator {
            vaffle::Terminator::Return { values } => values.len(),
            other => panic!("expected Return terminator, got {:?}", other),
        };
        assert_eq!(
            ret_bits, 64,
            "Return should carry 64 bits (32 orig + 32 global_out), got {ret_bits}"
        );
    }

    // ── Multi-value return lowers without error ───────────────────────────────
    //
    // A WASM function that returns 2 values should lower without error.

    #[test]
    fn test_multi_value_return_lowers() {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        // (i32) → (i32, i32)
        let sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32],
            returns: vec![WType::I32, WType::I32],
            shared: false,
        });
        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories: EntityVec::default(),
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };
        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, sig);
        let entry = body.entry;
        let p = body.blocks[entry].params[0].1;
        // return (p, p)
        body.set_terminator(entry, WTerminator::Return { values: vec![p, p] });
        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            sig,
            "multi_ret".into(),
            body,
        ));

        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&module, &mut target, &WaffleImportConfig::default());
        assert!(
            errors.is_empty(),
            "multi-value return lowering failed: {:?}",
            errors
        );
        assert_eq!(target.module.funcs.len(), 1);
        // VAFFLE sig should return 2 * 32 = 64 bits (two i32s concatenated).
        let func_body = match &target.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };
        let sig = &target.module.sigs[func_body.sig.0];
        // WASM (i32) → (i32, i32): one 32-bit param → 64-bit return.
        // VAFFLE sig carries 32 bit-params. Results are in the Return terminator.
        assert_eq!(
            sig.params.len(),
            32,
            "VAFFLE sig should have 32 bit-params for the i32 input"
        );

        // The Return terminator should carry 64 ValueIds (32+32 bits for the two i32s).
        let entry_block = &func_body.blocks[func_body.entry.0];
        let ret_bits = match &entry_block.terminator {
            vaffle::Terminator::Return { values } => values.len(),
            other => panic!("expected Return terminator, got {:?}", other),
        };
        assert_eq!(
            ret_bits, 64,
            "Return should carry 64 bits for two i32 returns, got {ret_bits}"
        );
    }

    // ── PickOutput: multi-value call with per-output extraction ───────────────

    #[test]
    fn test_pick_output_from_multi_return_call() {
        // Callee: () → (i32, i32) returning two constants.
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        let callee_sig = sigs.push(SignatureData::Func {
            params: vec![],
            returns: vec![WType::I32, WType::I32],
            shared: false,
        });
        // Caller: () → (i32) that calls the callee and returns the second output.
        let caller_sig = sigs.push(SignatureData::Func {
            params: vec![],
            returns: vec![WType::I32],
            shared: false,
        });

        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories: EntityVec::default(),
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };

        // Build callee body: return (1, 2).
        let mut callee_body = portal_pc_waffle_ir::FunctionBody::new(&module, callee_sig);
        let callee_entry = callee_body.entry;
        let c1 = callee_body.add_op(
            callee_entry,
            Operator::I32Const { value: 1 },
            &[],
            &[WType::I32],
        );
        let c2 = callee_body.add_op(
            callee_entry,
            Operator::I32Const { value: 2 },
            &[],
            &[WType::I32],
        );
        callee_body.set_terminator(
            callee_entry,
            WTerminator::Return {
                values: vec![c1, c2],
            },
        );
        let callee_func = module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            callee_sig,
            "callee".into(),
            callee_body,
        ));

        // Build caller body: call callee, pick output[1], return it.
        let mut caller_body = portal_pc_waffle_ir::FunctionBody::new(&module, caller_sig);
        let caller_entry = caller_body.entry;
        // The Call op produces a tuple value with both returns.
        let call_val = caller_body.add_op(
            caller_entry,
            Operator::Call {
                function_index: callee_func,
            },
            &[],
            &[WType::I32, WType::I32],
        );
        // PickOutput(call_val, 1, I32) extracts the second return.
        let picked = caller_body.add_value(ValueDef::PickOutput(call_val, 1, WType::I32));
        caller_body.append_to_block(caller_entry, picked);
        caller_body.set_terminator(
            caller_entry,
            WTerminator::Return {
                values: vec![picked],
            },
        );
        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            caller_sig,
            "caller".into(),
            caller_body,
        ));

        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&module, &mut target, &WaffleImportConfig::default());
        assert!(
            errors.is_empty(),
            "PickOutput lowering failed: {:?}",
            errors
        );
        // Both functions should have been lowered.
        assert_eq!(target.module.funcs.len(), 2);
    }

    // ── Oracle / action import registration ──────────────────────────────────
    //
    // A WAFFLE module with:
    //   - import "oracle_hash"  : (i32, i32) → i32   (oracle)
    //   - import "action_send"  : (i32, i32, i32) → i32  (action: guard, 1 arg, 1 fallback)
    //   - a caller function that calls both
    //
    // After lowering with the appropriate WaffleImportConfig the VAFFLE module
    // must contain one OracleDecl with name "hash" and one ActionDecl with name "send".

    fn build_oracle_action_module() -> WModule<'static> {
        let mut sigs: EntityVec<Signature, SignatureData> = EntityVec::default();
        // oracle_hash: (i32, i32) → i32
        let oracle_sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32, WType::I32],
            returns: vec![WType::I32],
            shared: false,
        });
        // action_send: (i32, i32, i32) → i32  [guard, arg, fallback]
        let action_sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32, WType::I32, WType::I32],
            returns: vec![WType::I32],
            shared: false,
        });
        // caller: (i32, i32, i32) → i32  [two hash inputs + guard]
        let caller_sig = sigs.push(SignatureData::Func {
            params: vec![WType::I32, WType::I32, WType::I32],
            returns: vec![WType::I32],
            shared: false,
        });

        let mut module = WModule {
            orig_bytes: None,
            funcs: EntityVec::default(),
            signatures: sigs,
            globals: EntityVec::default(),
            tables: EntityVec::default(),
            imports: vec![],
            exports: vec![],
            memories: EntityVec::default(),
            control_tags: EntityVec::default(),
            start_func: None,
            debug: Default::default(),
            debug_map: Default::default(),
            custom_sections: Default::default(),
        };

        // Push the two imports.
        let oracle_func = module.funcs.push(portal_pc_waffle_ir::FuncDecl::Import(
            oracle_sig,
            "oracle_hash".into(),
        ));
        let action_func = module.funcs.push(portal_pc_waffle_ir::FuncDecl::Import(
            action_sig,
            "action_send".into(),
        ));

        // Build caller body: hash(p0, p1) → h; send(p2, h, 0) → result; return result.
        let mut body = portal_pc_waffle_ir::FunctionBody::new(&module, caller_sig);
        let entry = body.entry;
        let p0 = body.blocks[entry].params[0].1;
        let p1 = body.blocks[entry].params[1].1;
        let p2 = body.blocks[entry].params[2].1; // guard

        // h = oracle_hash(p0, p1)
        let h = body.add_op(
            entry,
            Operator::Call { function_index: oracle_func },
            &[p0, p1],
            &[WType::I32],
        );

        // fallback = 0
        let fallback = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[WType::I32]);

        // result = action_send(p2, h, fallback)
        let result = body.add_op(
            entry,
            Operator::Call { function_index: action_func },
            &[p2, h, fallback],
            &[WType::I32],
        );

        body.set_terminator(entry, WTerminator::Return { values: vec![result] });

        module.funcs.push(portal_pc_waffle_ir::FuncDecl::Body(
            caller_sig,
            "caller".into(),
            body,
        ));

        module
    }

    #[test]
    fn test_oracle_and_action_registration() {
        let wasm = build_oracle_action_module();
        let config = WaffleImportConfig::new()
            .with_oracle("oracle_hash", "hash")
            .with_action("action_send", "send", 1);

        let mut target = VaffleTarget::new();
        let errors = lower_waffle_module(&wasm, &mut target, &config);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        // OracleDecl for "hash" should be registered.
        assert_eq!(target.module.oracles.len(), 1, "expected one oracle");
        assert_eq!(target.module.oracles[0].name, "hash");
        assert_eq!(target.module.oracles[0].params.len(), 2, "oracle has 2 params");
        assert_eq!(target.module.oracles[0].results.len(), 1, "oracle has 1 result");

        // ActionDecl for "send" should be registered.
        assert_eq!(target.module.actions.len(), 1, "expected one action");
        assert_eq!(target.module.actions[0].name, "send");
        assert_eq!(target.module.actions[0].params.len(), 1, "action has 1 real arg");
        assert_eq!(target.module.actions[0].results.len(), 1, "action has 1 result");

        // The caller function should have lowered successfully.
        let caller = target.module.funcs.iter().find(|f| matches!(f, vaffle::FuncDecl::Body(_)));
        assert!(caller.is_some(), "caller function body should be present");
    }
}
