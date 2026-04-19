//! Concrete evaluator for VAFFLE `Module`.
//!
//! All values are represented as `Vec<bool>` (LSB-first bit vectors), the
//! same representation used by [`crate::interpreter::ir`].
//!
//! # Limitations
//! - `Call`, `Output`, `StackAlloc`, `PtrLoad`, `PtrStore`, `PtrOffset`
//!   values **panic** — the generator never produces them.
//! - `ReturnCall` and `Table` terminators **panic** — not emitted by the
//!   generator.
//! - Oracle, action, RNG, and storage statements **panic**.

use std::collections::BTreeMap;

use vaffle::{BlockId, FuncDecl, FuncId, Module, Terminator, Value, ValueId};
use volar_ir_common::{Constant, Stmt, StorageId, TypeId, TypeTable};

use crate::interpreter::ir::{IrValue, StorageMap, bit_width, bits_to_u64, const_to_bits, rotate_left, rotate_right, transmute_bits};

// ============================================================================
// Public API
// ============================================================================

/// Maximum number of times the entry block may be re-entered before the
/// interpreter gives up.
pub const MAX_ITERS: usize = 512;

/// Evaluate function `func_id` in `module` with the given inputs.
///
/// `inputs[i]` must have exactly `bit_width(entry_param_types[i])` bits.
///
/// Returns the values listed in the `Return` terminator, or `None` if the
/// iteration guard is exceeded.
pub fn eval_vaffle(
    module: &Module,
    func_id: FuncId,
    inputs: &[IrValue],
) -> Option<Vec<IrValue>> {
    let body = match &module.funcs[func_id.0] {
        FuncDecl::Body(b) => b,
        FuncDecl::Import { .. } => panic!("eval_vaffle: cannot evaluate an imported function"),
    };

    let mut value_table: BTreeMap<usize, IrValue> = BTreeMap::new();
    let mut storage: StorageMap = BTreeMap::new();

    // Seed the entry block's params from `inputs`.
    let entry_block = &body.blocks[body.entry.0];
    assert_eq!(
        inputs.len(),
        entry_block.params.len(),
        "eval_vaffle: expected {} inputs, got {}",
        entry_block.params.len(),
        inputs.len(),
    );
    for ((vid, _tid), val) in entry_block.params.iter().zip(inputs.iter()) {
        value_table.insert(vid.0, val.clone());
    }

    let mut current_block_id = body.entry;
    let mut loop_count = 0usize;

    loop {
        if current_block_id == body.entry {
            loop_count += 1;
            if loop_count > MAX_ITERS {
                return None;
            }
        }

        let block = &body.blocks[current_block_id.0];

        // Evaluate each stmt value in this block.
        for &vid in &block.stmts {
            let val = eval_vaffle_value(vid, &body.values, &value_table, &module.types, &mut storage);
            value_table.insert(vid.0, val);
        }

        // Dispatch the terminator.
        match &block.terminator {
            Terminator::Return { values } => {
                return Some(
                    values
                        .iter()
                        .map(|vid| get_val(&value_table, *vid))
                        .collect(),
                );
            }
            Terminator::Jump(target) => {
                let args: Vec<IrValue> = target
                    .args
                    .iter()
                    .map(|vid| get_val(&value_table, *vid))
                    .collect();
                let next_block = &body.blocks[target.block.0];
                for ((param_vid, _), val) in next_block.params.iter().zip(args.iter()) {
                    value_table.insert(param_vid.0, val.clone());
                }
                current_block_id = target.block;
            }
            Terminator::IfNonzero {
                cond,
                then_target,
                else_target,
            } => {
                let cond_val = get_val(&value_table, *cond);
                let target = if cond_val.first().copied().unwrap_or(false) {
                    then_target
                } else {
                    else_target
                };
                let args: Vec<IrValue> = target
                    .args
                    .iter()
                    .map(|vid| get_val(&value_table, *vid))
                    .collect();
                let next_block = &body.blocks[target.block.0];
                for ((param_vid, _), val) in next_block.params.iter().zip(args.iter()) {
                    value_table.insert(param_vid.0, val.clone());
                }
                current_block_id = target.block;
            }
            Terminator::ReturnCall { .. } => panic!("eval_vaffle: ReturnCall not supported"),
            Terminator::Table { index, targets, default_target } => {
                let index_val = get_val(&value_table, *index);
                let idx = bits_to_u64(&index_val) as usize;
                let target = if idx < targets.len() { &targets[idx] } else { default_target };
                let args: Vec<IrValue> = target
                    .args
                    .iter()
                    .map(|vid| get_val(&value_table, *vid))
                    .collect();
                let next_block = &body.blocks[target.block.0];
                for ((param_vid, _), val) in next_block.params.iter().zip(args.iter()) {
                    value_table.insert(param_vid.0, val.clone());
                }
                current_block_id = target.block;
            }
        }
    }
}

// ============================================================================
// Value evaluation
// ============================================================================

fn eval_vaffle_value(
    vid: ValueId,
    values: &[Value],
    value_table: &BTreeMap<usize, IrValue>,
    types: &TypeTable,
    storage: &mut StorageMap,
) -> IrValue {
    match &values[vid.0] {
        Value::Param { .. } => get_val(value_table, vid),
        Value::Op(stmt) => eval_vaffle_stmt(stmt, value_table, types, storage),
        Value::Call { .. } => panic!("eval_vaffle: Call not supported"),
        Value::Output { .. } => panic!("eval_vaffle: Output not supported"),
        Value::StackAlloc { .. } => panic!("eval_vaffle: StackAlloc not supported"),
        Value::PtrLoad { .. } => panic!("eval_vaffle: PtrLoad not supported"),
        Value::PtrStore { .. } => panic!("eval_vaffle: PtrStore not supported"),
        Value::PtrOffset { .. } => panic!("eval_vaffle: PtrOffset not supported"),
    }
}

fn eval_vaffle_stmt(
    stmt: &Stmt<ValueId>,
    value_table: &BTreeMap<usize, IrValue>,
    types: &TypeTable,
    storage: &mut StorageMap,
) -> IrValue {
    let get = |vid: &ValueId| -> IrValue { get_val(value_table, *vid) };

    match stmt {
        Stmt::Const(c, ty) => {
            let w = bit_width(*ty, types);
            const_to_bits(c, w)
        }
        Stmt::Transmute { src, dst_ty, .. } => {
            let src_val = get(src);
            let dst_w = bit_width(*dst_ty, types);
            transmute_bits(&src_val, dst_w)
        }
        Stmt::Poly { coeffs, constant, .. } => {
            let width = coeffs
                .iter()
                .next()
                .and_then(|(key, _)| key.first())
                .map(|first_var| get(first_var).len())
                .unwrap_or(1);
            eval_vaffle_poly(coeffs, constant, width, value_table)
        }
        Stmt::Rol { src, ty, n } => {
            let val = get(src);
            let w = bit_width(*ty, types);
            rotate_left(&val, w, *n)
        }
        Stmt::Ror { src, ty, n } => {
            let val = get(src);
            let w = bit_width(*ty, types);
            rotate_right(&val, w, *n)
        }
        Stmt::Merge { parts, ty } => {
            let w = bit_width(*ty, types);
            let mut result = Vec::with_capacity(w);
            for part in parts {
                result.extend_from_slice(&get(part));
            }
            result.truncate(w);
            while result.len() < w {
                result.push(false);
            }
            result
        }
        Stmt::Splat { src, ty } => {
            let bit = get(src).first().copied().unwrap_or(false);
            let w = bit_width(*ty, types);
            vec![bit; w]
        }
        Stmt::Shuffle { result_bits, ty } => {
            let w = bit_width(*ty, types);
            let mut result = vec![false; w];
            for (i, (bit_idx, var)) in result_bits.iter().enumerate() {
                if i < w {
                    let val = get(var);
                    result[i] = val.get(*bit_idx as usize).copied().unwrap_or(false);
                }
            }
            result
        }
        Stmt::OracleCall { .. } => panic!("eval_vaffle: OracleCall not supported"),
        Stmt::OracleOutput { .. } => panic!("eval_vaffle: OracleOutput not supported"),
        Stmt::ActionCall { .. } => panic!("eval_vaffle: ActionCall not supported"),
        Stmt::ActionOutput { .. } => panic!("eval_vaffle: ActionOutput not supported"),
        Stmt::Rng { .. } => panic!("eval_vaffle: Rng not supported"),
        Stmt::StorageRead { storage: sid, ty, addr } => {
            let w = bit_width(*ty, types);
            let addr_u64 = bits_to_u64(&get(addr));
            storage
                .get(&(*sid, *ty, addr_u64))
                .cloned()
                .unwrap_or_else(|| vec![false; w])
        }
        Stmt::StorageWrite { storage: sid, src, ty, addr } => {
            let src_val = get(src);
            let addr_u64 = bits_to_u64(&get(addr));
            let w = bit_width(*ty, types);
            let mut val = src_val;
            val.truncate(w);
            while val.len() < w {
                val.push(false);
            }
            storage.insert((*sid, *ty, addr_u64), val);
            vec![] // StorageWrite has no output
        }
    }
}

// ============================================================================
// Polynomial evaluation (VAFFLE variant with ValueId keys)
// ============================================================================

fn eval_vaffle_poly(
    coeffs: &std::collections::BTreeMap<Vec<ValueId>, u8>,
    constant: &Constant,
    width: usize,
    value_table: &BTreeMap<usize, IrValue>,
) -> IrValue {
    let mut result = vec![false; width];
    for k in 0..width {
        let const_bit = if k < 128 {
            ((constant.lo >> k) & 1) != 0
        } else {
            ((constant.hi >> (k - 128)) & 1) != 0
        };
        let mut acc = const_bit;
        for (monomial, coeff) in coeffs {
            if coeff & 1 == 0 {
                continue;
            }
            let product = monomial.iter().all(|var| {
                get_val(value_table, *var)
                    .get(k)
                    .copied()
                    .unwrap_or(false)
            });
            acc ^= product;
        }
        result[k] = acc;
    }
    result
}

// ============================================================================
// Utilities
// ============================================================================

fn get_val(value_table: &BTreeMap<usize, IrValue>, vid: ValueId) -> IrValue {
    value_table
        .get(&vid.0)
        .cloned()
        .unwrap_or_else(|| panic!("eval_vaffle: value {} not in table", vid.0))
}
