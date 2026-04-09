// @reliability: normal
//! Lowering passes from the circuit IRs (`BIrBlocks`, `IRBlocks`) to `LirTarget`.

use alloc::{vec, vec::Vec};
use volar_lir::{LirTarget, LirType};

use crate::{
    boolar::{BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypes},
};

// ============================================================================
// BIrBlocks → LirTarget
// ============================================================================

/// Lower a boolean circuit (`BIrBlocks`) to any `LirTarget`.
///
/// All values are `LirType::Bool`. The entry block's parameter count determines
/// the function signature. Multiple outputs are packed into a `U64` via
/// shl/or/zext; a single output is returned directly.
pub fn lower_biir<T: LirTarget>(blocks: &BIrBlocks, name: &str, target: &mut T) {
    let entry_block = &blocks.0[0];
    let num_inputs = entry_block.params as usize;

    // Create all LIR block handles up front (forward declarations).
    let mut block_handles: Vec<T::Block> = Vec::with_capacity(blocks.0.len());
    // begin_function creates the entry block for us; create extra blocks here temporarily.
    // We'll call begin_function below, which gives us the true entry block.
    // For blocks beyond the first we pre-allocate after begin_function.

    let input_tys: Vec<LirType> = (0..num_inputs).map(|_| LirType::Bool).collect();
    let (entry_handle, entry_params) = target.begin_function(name, &input_tys, Some(LirType::U64));
    block_handles.push(entry_handle);

    // Pre-create handles for all blocks after the entry.
    for _ in 1..blocks.0.len() {
        block_handles.push(target.create_block());
    }

    // Add block params for blocks beyond entry (entry params are function params).
    // vals_per_block[b] stores the values for block b: [param0, param1, ..., stmt0, stmt1, ...]
    let mut vals_per_block: Vec<Vec<T::Value>> = Vec::with_capacity(blocks.0.len());
    vals_per_block.push(entry_params); // entry block's params = function params

    for (bi, block) in blocks.0.iter().enumerate().skip(1) {
        let mut block_vals: Vec<T::Value> = Vec::with_capacity(block.params as usize);
        for _ in 0..block.params {
            let v = target.add_block_param(block_handles[bi], LirType::Bool);
            block_vals.push(v);
        }
        vals_per_block.push(block_vals);
    }

    // Emit each block.
    for (bi, block) in blocks.0.iter().enumerate() {
        target.switch_to_block(block_handles[bi]);

        let vals = &mut vals_per_block[bi];
        // Reserve space for stmt results.
        for stmt in &block.stmts {
            let v = lower_biir_stmt(stmt, vals, target);
            vals.push(v);
        }

        // Emit terminator.
        // We need to look up vals immutably, but we already have &mut vals_per_block.
        // Clone the vals for this block to avoid borrow issues.
        let block_vals = vals_per_block[bi].clone();
        lower_biir_terminator(&block.terminator, &block_vals, &block_handles, target);
    }

    target.end_function();
}

fn lower_biir_stmt<T: LirTarget>(stmt: &BIrStmt, vals: &[T::Value], target: &mut T) -> T::Value {
    match stmt {
        BIrStmt::Zero => target.iconst(LirType::Bool, 0),
        BIrStmt::One => target.iconst(LirType::Bool, 1),
        BIrStmt::And(a, b) => {
            let va = vals[a.0 as usize];
            let vb = vals[b.0 as usize];
            target.and(va, vb)
        }
        BIrStmt::Or(a, b) => {
            let va = vals[a.0 as usize];
            let vb = vals[b.0 as usize];
            target.or(va, vb)
        }
        BIrStmt::Xor(a, b) => {
            let va = vals[a.0 as usize];
            let vb = vals[b.0 as usize];
            target.xor(va, vb)
        }
        BIrStmt::Not(a) => {
            let va = vals[a.0 as usize];
            target.not(va)
        }
    }
}

fn lower_biir_terminator<T: LirTarget>(
    term: &BIrTerminator,
    vals: &[T::Value],
    block_handles: &[T::Block],
    target: &mut T,
) {
    match term {
        BIrTerminator::Jmp(tgt) => {
            lower_biir_jump(tgt, vals, block_handles, target);
        }
        BIrTerminator::CondJmp {
            val,
            then_target,
            else_target,
        } => {
            let cond = vals[val.0 as usize];
            let (then_block, then_args) = resolve_biir_target::<T>(then_target, vals, block_handles);
            let (else_block, else_args) = resolve_biir_target::<T>(else_target, vals, block_handles);
            match (then_block, else_block) {
                (Some(tb), Some(eb)) => {
                    target.branch(cond, tb, &then_args, eb, &else_args);
                }
                _ => unimplemented!("CondJmp with Return/Dyn target"),
            }
        }
    }
}

fn lower_biir_jump<T: LirTarget>(
    tgt: &BIrTarget,
    vals: &[T::Value],
    block_handles: &[T::Block],
    target: &mut T,
) {
    match &tgt.block {
        IRBlockTargetId::Return => {
            let args: Vec<T::Value> = tgt.args.iter().map(|id| vals[id.0 as usize]).collect();
            let ret_val = pack_bits_to_u64(&args, target);
            target.ret(Some(ret_val));
        }
        IRBlockTargetId::Block(id) => {
            let args: Vec<T::Value> = tgt.args.iter().map(|id| vals[id.0 as usize]).collect();
            target.jump(block_handles[id.0 as usize], &args);
        }
        IRBlockTargetId::Dyn(_) => unimplemented!("dynamic jump target in BIrBlocks lowering"),
    }
}

fn resolve_biir_target<T: LirTarget>(
    tgt: &BIrTarget,
    vals: &[T::Value],
    block_handles: &[T::Block],
) -> (Option<T::Block>, Vec<T::Value>) {
    let args: Vec<T::Value> = tgt.args.iter().map(|id| vals[id.0 as usize]).collect();
    match &tgt.block {
        IRBlockTargetId::Block(id) => (Some(block_handles[id.0 as usize]), args),
        IRBlockTargetId::Return => (None, args),
        IRBlockTargetId::Dyn(_) => unimplemented!("dynamic jump target in BIrBlocks lowering"),
    }
}

/// Pack a slice of Bool values into a single U64 via shl/or/zext.
/// For a single value, just zext it.
fn pack_bits_to_u64<T: LirTarget>(bits: &[T::Value], target: &mut T) -> T::Value {
    if bits.is_empty() {
        return target.iconst(LirType::U64, 0);
    }
    if bits.len() == 1 {
        return target.zext(bits[0], LirType::U64);
    }
    let mut acc = target.zext(bits[0], LirType::U64);
    for (i, &bit) in bits.iter().enumerate().skip(1) {
        let ext = target.zext(bit, LirType::U64);
        let shift = target.iconst(LirType::U64, i as i64);
        let shifted = target.shl(ext, shift);
        acc = target.or(acc, shifted);
    }
    acc
}

// ============================================================================
// IRBlocks + IRTypes → LirTarget
// ============================================================================

/// Lower typed Volar IR (`IRBlocks`) to any `LirTarget`.
///
/// Only scalar types are supported: `Bit` → `Bool`, `Galois8AES` → `U8`,
/// `Galois64` → `U64`, `Vec(n≤64, Bit)` → smallest fitting unsigned integer.
/// Tuple, nested-vec, multi-element Galois vecs, StorageRead/Write, and
/// dynamic targets are `unimplemented!()`.
pub fn lower_ir<T: LirTarget>(
    blocks: &IRBlocks,
    types: &IRTypes,
    name: &str,
    target: &mut T,
) {
    let entry = &blocks.0[0];

    // Map entry block param types to LirType.
    let input_tys: Vec<LirType> = entry.params.iter().map(|tid| ir_type_to_lir(&types.0[tid.0 as usize])).collect();

    // Determine return type from the return terminator's args.
    // For simplicity, assume single-output; we'll pack multi-output below.
    let ret_ty = Some(LirType::U64);

    let (entry_handle, entry_params) = target.begin_function(name, &input_tys, ret_ty);

    let mut block_handles: Vec<T::Block> = vec![entry_handle];
    for _ in 1..blocks.0.len() {
        block_handles.push(target.create_block());
    }

    // vals_per_block: for each block, vec of (var_id → value) in order
    let mut vals_per_block: Vec<Vec<T::Value>> = Vec::with_capacity(blocks.0.len());
    vals_per_block.push(entry_params);

    for (bi, block) in blocks.0.iter().enumerate().skip(1) {
        let mut bv: Vec<T::Value> = Vec::new();
        for tid in &block.params {
            let lir_ty = ir_type_to_lir(&types.0[tid.0 as usize]);
            bv.push(target.add_block_param(block_handles[bi], lir_ty));
        }
        vals_per_block.push(bv);
    }

    for (bi, block) in blocks.0.iter().enumerate() {
        target.switch_to_block(block_handles[bi]);

        for stmt in &block.stmts {
            let v = lower_ir_stmt(stmt, &vals_per_block[bi], types, target);
            vals_per_block[bi].push(v);
        }

        let block_vals = vals_per_block[bi].clone();
        lower_ir_terminator(&block.terminator, &block_vals, &block_handles, target);
    }

    target.end_function();
}

fn ir_type_to_lir(ty: &IRType) -> LirType {
    match ty {
        IRType::Bit => LirType::Bool,
        IRType::Galois8AES => LirType::U8,
        IRType::Galois64 => LirType::U64,
        IRType::Vec(n, _elem) if *n <= 8 => LirType::U8,
        IRType::Vec(n, _elem) if *n <= 16 => LirType::U16,
        IRType::Vec(n, _elem) if *n <= 32 => LirType::U32,
        IRType::Vec(n, _elem) if *n <= 64 => LirType::U64,
        other => unimplemented!("ir_type_to_lir: unsupported type {:?}", other),
    }
}

fn lower_ir_stmt<T: LirTarget>(
    stmt: &IRStmt,
    vals: &[T::Value],
    types: &IRTypes,
    target: &mut T,
) -> T::Value {
    match stmt {
        IRStmt::Const(c, tid) => {
            let lir_ty = ir_type_to_lir(&types.0[tid.0 as usize]);
            target.iconst(lir_ty, c.lo as i64)
        }
        IRStmt::Transmute { src, src_ty, dst_ty } => {
            let sv = vals[src.0 as usize];
            let src_lir = ir_type_to_lir(&types.0[src_ty.0 as usize]);
            let dst_lir = ir_type_to_lir(&types.0[dst_ty.0 as usize]);
            if dst_lir.bit_width() > src_lir.bit_width() {
                target.zext(sv, dst_lir)
            } else if dst_lir.bit_width() < src_lir.bit_width() {
                target.trunc(sv, dst_lir)
            } else {
                sv
            }
        }
        IRStmt::Poly { coeffs, constant } => {
            // Only GF(2) (Bit) is supported: add=XOR, mul=AND.
            let mut acc = target.iconst(LirType::Bool, (constant.lo & 1) as i64);
            for (vars, &coeff) in coeffs {
                if coeff == 0 {
                    continue;
                }
                let product = vars
                    .iter()
                    .map(|id| vals[id.0 as usize])
                    .reduce(|a, b| target.and(a, b))
                    .unwrap_or_else(|| target.iconst(LirType::Bool, 1));
                acc = target.xor(acc, product);
            }
            acc
        }
        IRStmt::Rol { src, ty, n } => {
            let sv = vals[src.0 as usize];
            let lir_ty = ir_type_to_lir(&types.0[ty.0 as usize]);
            let width = lir_ty.bit_width();
            let n_mod = (*n as u32) % width;
            if n_mod == 0 {
                return sv;
            }
            let shift_l = target.iconst(lir_ty.clone(), n_mod as i64);
            let shift_r = target.iconst(lir_ty, (width - n_mod) as i64);
            let left = target.shl(sv, shift_l);
            let right = target.lshr(sv, shift_r);
            target.or(left, right)
        }
        IRStmt::Ror { src, ty, n } => {
            let sv = vals[src.0 as usize];
            let lir_ty = ir_type_to_lir(&types.0[ty.0 as usize]);
            let width = lir_ty.bit_width();
            let n_mod = (*n as u32) % width;
            if n_mod == 0 {
                return sv;
            }
            let shift_r = target.iconst(lir_ty.clone(), n_mod as i64);
            let shift_l = target.iconst(lir_ty, (width - n_mod) as i64);
            let right = target.lshr(sv, shift_r);
            let left = target.shl(sv, shift_l);
            target.or(right, left)
        }
        IRStmt::Merge { parts, ty } => {
            let dst_lir = ir_type_to_lir(&types.0[ty.0 as usize]);
            let elem_bits = dst_lir.bit_width() / parts.len() as u32;
            let mut acc = target.iconst(dst_lir.clone(), 0);
            for (i, part_id) in parts.iter().enumerate() {
                let part = vals[part_id.0 as usize];
                let ext = target.zext(part, dst_lir.clone());
                let shift = target.iconst(dst_lir.clone(), (i as u32 * elem_bits) as i64);
                let shifted = target.shl(ext, shift);
                acc = target.or(acc, shifted);
            }
            acc
        }
        IRStmt::Splat { src, ty } => {
            // Replicate src into all positions of the target type.
            let dst_lir = ir_type_to_lir(&types.0[ty.0 as usize]);
            let sv = vals[src.0 as usize];
            // Determine element width from src type (assume Bool = 1 bit).
            let src_bits = 1u32; // conservative: treat as 1-bit element
            let total_bits = dst_lir.bit_width();
            let ext = target.zext(sv, dst_lir.clone());
            let mut acc = ext;
            let mut pos = src_bits;
            while pos < total_bits {
                let shift = target.iconst(dst_lir.clone(), pos as i64);
                let shifted = target.shl(ext, shift);
                acc = target.or(acc, shifted);
                pos += src_bits;
            }
            acc
        }
        IRStmt::StorageRead { .. } | IRStmt::StorageWrite { .. } => {
            unimplemented!("StorageRead/Write require memory ops not yet in LirTarget")
        }
    }
}

fn lower_ir_terminator<T: LirTarget>(
    term: &IRTerminator,
    vals: &[T::Value],
    block_handles: &[T::Block],
    target: &mut T,
) {
    match term {
        IRTerminator::Jmp { func, args } => {
            let arg_vals: Vec<T::Value> = args.iter().map(|id| vals[id.0 as usize]).collect();
            match func {
                IRBlockTargetId::Return => {
                    let ret = pack_bits_to_u64(&arg_vals, target);
                    target.ret(Some(ret));
                }
                IRBlockTargetId::Block(id) => {
                    target.jump(block_handles[id.0 as usize], &arg_vals);
                }
                IRBlockTargetId::Dyn(_) => unimplemented!("dynamic jump target"),
            }
        }
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            let cond = vals[condition.0 as usize];
            let true_vals: Vec<T::Value> = true_args.iter().map(|id| vals[id.0 as usize]).collect();
            let false_vals: Vec<T::Value> = false_args.iter().map(|id| vals[id.0 as usize]).collect();
            match (true_block, false_block) {
                (IRBlockTargetId::Block(t), IRBlockTargetId::Block(f)) => {
                    target.branch(cond, block_handles[t.0 as usize], &true_vals, block_handles[f.0 as usize], &false_vals);
                }
                _ => unimplemented!("JumpCond with Return/Dyn target"),
            }
        }
        IRTerminator::JumpTable { .. } => {
            unimplemented!("JumpTable lowering not yet implemented")
        }
    }
}
