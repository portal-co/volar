// @reliability: experimental
// @ai: assisted
//! Bit-level step-circuit evaluator: the emitter's own forward evaluator for
//! exactly the op subset it emits constraints for. Sharing one semantics
//! between evaluation and emission is the C2 safeguard (the constraints are
//! the step's own arithmetic, never a re-encoding). Differentially
//! cross-checked against `volar-fuzz`'s `eval_ir_circuit_step` in tests.

use volar_ir::ir::{IRBlock, IRVarId};
use volar_ir_common::{Constant, Stmt, TypeTable as IRTypes};
use volar_ir_opt::common::stmt_output_type;

use crate::EmitError;

/// Bit width of a type, fail-closed on non-data types.
pub(crate) fn width_of(ty: volar_ir_common::TypeId, types: &IRTypes) -> Result<usize, EmitError> {
    types
        .value_bit_width(ty)
        .ok_or(EmitError::NonDataType(ty))
}

/// `Constant` → its low `width` bits, LSB-first.
pub(crate) fn const_bits(c: &Constant, width: usize) -> Vec<bool> {
    (0..width)
        .map(|i| {
            if i < 128 {
                (c.lo >> i) & 1 != 0
            } else {
                (c.hi >> (i - 128)) & 1 != 0
            }
        })
        .collect()
}

/// Evaluate a movfuscated step block on `inputs` (one bit-vector per block
/// param), returning every variable's bits — params first, then one entry per
/// statement in order. The witness for the step's PESAT instance is exactly
/// this trace's statement-output region.
pub fn eval_step_wires(
    block: &IRBlock<()>,
    types: &IRTypes,
    inputs: &[Vec<bool>],
) -> Result<Vec<Vec<bool>>, EmitError> {
    let n_params = block.params.len();
    if inputs.len() != n_params {
        return Err(EmitError::Arity {
            expected: n_params,
            got: inputs.len(),
        });
    }
    let mut wires: Vec<Vec<bool>> = Vec::with_capacity(n_params + block.stmts.len());
    for (param, input) in block.params.iter().zip(inputs.iter()) {
        let w = width_of(*param, types)?;
        if input.len() != w {
            return Err(EmitError::Arity {
                expected: w,
                got: input.len(),
            });
        }
        wires.push(input.clone());
    }
    for node in &block.stmts {
        let out = eval_stmt(&node.kind, &wires, types)?;
        wires.push(out);
    }
    Ok(wires)
}

/// Evaluate one statement to its output bits.
fn eval_stmt(
    stmt: &Stmt<IRVarId>,
    wires: &[Vec<bool>],
    types: &IRTypes,
) -> Result<Vec<bool>, EmitError> {
    let get = |v: IRVarId| -> Result<&[bool], EmitError> {
        wires
            .get(v.0 as usize)
            .map(Vec::as_slice)
            .ok_or(EmitError::UndefinedVar(v))
    };
    match stmt {
        Stmt::Const(c, ty) => {
            let w = width_of(*ty, types)?;
            Ok(const_bits(c, w))
        }
        Stmt::Poly {
            ty,
            coeffs,
            constant,
        } => {
            if volar_ir_common::contains_ext_field(*ty, types)
                || volar_ir_common::contains_prime_field(*ty, types)
            {
                // Field polys multiply in the field, not per-bit AND — a
                // different term shape (F-series). Not in this emitter's
                // bit-level scope.
                return Err(EmitError::Unsupported("field-typed Poly"));
            }
            let w = width_of(*ty, types)?;
            let const_b = const_bits(constant, w);
            let mut out = Vec::with_capacity(w);
            for lane in 0..w {
                let mut acc = const_b[lane];
                for (mono, coeff) in coeffs {
                    if coeff & 1 == 0 {
                        continue;
                    }
                    let mut prod = true;
                    for v in mono {
                        let bits = get(*v)?;
                        // lane_product: a width-1 factor broadcasts, a wider
                        // factor contributes its lane bit, a narrower factor
                        // contributes 0 (killing the product) past its width.
                        let bit = if bits.len() == 1 {
                            bits[0]
                        } else {
                            bits.get(lane).copied().unwrap_or(false)
                        };
                        prod &= bit;
                    }
                    acc ^= prod;
                }
                out.push(acc);
            }
            Ok(out)
        }
        Stmt::Merge { parts, ty } => {
            let w = width_of(*ty, types)?;
            let mut out = Vec::with_capacity(w);
            for part in parts {
                out.extend_from_slice(get(*part)?);
            }
            out.truncate(w);
            while out.len() < w {
                out.push(false);
            }
            Ok(out)
        }
        Stmt::Splat { src, ty } => {
            let w = width_of(*ty, types)?;
            let bit = get(*src)?.first().copied().unwrap_or(false);
            Ok(vec![bit; w])
        }
        Stmt::Shuffle { result_bits, ty } => {
            let w = width_of(*ty, types)?;
            let mut out = vec![false; w];
            for (i, (bit_idx, var)) in result_bits.iter().enumerate() {
                if i < w {
                    out[i] = get(*var)?
                        .get(*bit_idx as usize)
                        .copied()
                        .unwrap_or(false);
                }
            }
            Ok(out)
        }
        Stmt::Transmute { src, dst_ty, .. } => {
            let w = width_of(*dst_ty, types)?;
            let mut out = get(*src)?.to_vec();
            out.truncate(w);
            while out.len() < w {
                out.push(false);
            }
            Ok(out)
        }
        Stmt::Rol { src, ty, n } => {
            let bits = get(*src)?;
            let w = width_of(*ty, types)?;
            let mut out = vec![false; w];
            for j in 0..w {
                out[j] = bits.get((j + w - (n % w.max(1))) % w.max(1)).copied().unwrap_or(false);
            }
            Ok(out)
        }
        Stmt::Ror { src, ty, n } => {
            let bits = get(*src)?;
            let w = width_of(*ty, types)?;
            let mut out = vec![false; w];
            for j in 0..w {
                out[j] = bits.get((j + (n % w.max(1))) % w.max(1)).copied().unwrap_or(false);
            }
            Ok(out)
        }
        other => Err(EmitError::UnsupportedStmt {
            discriminant: core::mem::discriminant(other),
        }),
    }
}

/// The output width of one statement (0 for side-effect-only statements).
pub(crate) fn stmt_out_width(stmt: &Stmt<IRVarId>, types: &IRTypes) -> Result<usize, EmitError> {
    match stmt_output_type(stmt) {
        Some(ty) => width_of(ty, types),
        None => Ok(0),
    }
}
