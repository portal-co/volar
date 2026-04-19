//! Concrete evaluator for `IRBlocks<()>`.
//!
//! All values are represented as `Vec<bool>` (LSB-first bit vectors).  This
//! handles all widths uniformly, including `Vec`-typed compound values.
//!
//! # Limitations
//! - Oracle, action, RNG, and storage statements **panic** — the generator
//!   never produces them.
//! - `JumpTable` and `Dyn` jump targets **panic** — not emitted by the
//!   generator.
//! - `_256`, `AES8`, and `Galois64` types are supported by width only (no
//!   field-specific semantics); the generator avoids them.

use std::collections::BTreeMap;

use volar_ir::ir::{
    IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRTypes, IRVarId,
};
use volar_ir_common::{Constant, IrType, Stmt, StorageId, Type, TypeId};

// ============================================================================
// Public API
// ============================================================================

/// A concrete value: a `Vec<bool>` of bits, LSB at index 0.
pub type IrValue = Vec<bool>;

/// Storage map: keyed by `(StorageId, address_as_u64)`, value is a bit vector.
pub type StorageMap = BTreeMap<(StorageId, u64), Vec<bool>>;

/// Evaluate `blocks` from block 0 with `inputs` bound to the entry block's
/// typed params.
///
/// `inputs[i]` must have `bit_width(entry_param_types[i], types)` bits.
///
/// Returns the output values (the args of the first `Jmp(Return)` reached),
/// or `None` if the iteration guard is exceeded.
pub fn eval_ir(
    blocks: &IRBlocks<()>,
    types: &IRTypes,
    inputs: &[IrValue],
) -> Option<Vec<IrValue>> {
    let mut loop_count = 0usize;
    let mut current_block: usize = 0;
    let mut current_inputs: Vec<IrValue> = inputs.to_vec();
    let mut storage: StorageMap = BTreeMap::new();

    loop {
        if current_block == 0 {
            loop_count += 1;
            if loop_count > crate::interpreter::biir::MAX_ITERS {
                return None;
            }
        }

        let block = &blocks.blocks[current_block];
        let result = eval_ir_block(block, types, &current_inputs, &mut storage)?;

        match result {
            IrBlockResult::Return(vals) => return Some(vals),
            IrBlockResult::Jump { target, args } => {
                current_block = target;
                current_inputs = args;
            }
        }
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Result of executing one `IRBlock` to its terminator.
enum IrBlockResult {
    Return(Vec<IrValue>),
    Jump { target: usize, args: Vec<IrValue> },
}

/// Evaluate a single `IRBlock`, building a variable table and dispatching the
/// terminator.
fn eval_ir_block(
    block: &volar_ir::ir::IRBlock<()>,
    types: &IRTypes,
    params: &[IrValue],
    storage: &mut StorageMap,
) -> Option<IrBlockResult> {
    assert_eq!(
        params.len(),
        block.params.len(),
        "eval_ir_block: expected {} params, got {}",
        block.params.len(),
        params.len()
    );

    let mut vars: BTreeMap<u32, IrValue> = BTreeMap::new();
    for (i, v) in params.iter().enumerate() {
        vars.insert(i as u32, v.clone());
    }

    let base = block.params.len() as u32;
    for (i, stmt) in block.stmts.iter().enumerate() {
        let id = base + i as u32;
        let val = eval_ir_stmt(stmt, types, &vars, storage);
        vars.insert(id, val);
    }

    let result = match &block.terminator {
        IRTerminator::Jmp { func, args } => {
            let arg_vals: Vec<IrValue> = args.iter().map(|id| get_ir(&vars, id)).collect();
            match func {
                IRBlockTargetId::Return => IrBlockResult::Return(arg_vals),
                IRBlockTargetId::Block(b) => IrBlockResult::Jump {
                    target: b.0 as usize,
                    args: arg_vals,
                },
                IRBlockTargetId::Dyn(_) => panic!("eval_ir: Dyn jump not supported"),
            }
        }
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            let cond_val = get_ir(&vars, condition);
            let (target_block, target_args) = if cond_val[0] {
                (true_block, true_args)
            } else {
                (false_block, false_args)
            };
            let arg_vals: Vec<IrValue> =
                target_args.iter().map(|id| get_ir(&vars, id)).collect();
            match target_block {
                IRBlockTargetId::Return => IrBlockResult::Return(arg_vals),
                IRBlockTargetId::Block(b) => IrBlockResult::Jump {
                    target: b.0 as usize,
                    args: arg_vals,
                },
                IRBlockTargetId::Dyn(_) => panic!("eval_ir: Dyn jump in JumpCond not supported"),
            }
        }
        IRTerminator::JumpTable { .. } => panic!("eval_ir: JumpTable not supported"),
    };

    Some(result)
}

/// Evaluate a single IR statement.  Returns the result value as a bit vector.
fn eval_ir_stmt(stmt: &IRStmt, types: &IRTypes, vars: &BTreeMap<u32, IrValue>, storage: &mut StorageMap) -> IrValue {
    match stmt {
        Stmt::Const(c, ty) => {
            let w = bit_width(*ty, types);
            const_to_bits(c, w)
        }
        Stmt::Transmute { src, dst_ty, .. } => {
            let src_val = get_ir(vars, src);
            let dst_w = bit_width(*dst_ty, types);
            transmute_bits(&src_val, dst_w)
        }
        Stmt::Poly { coeffs, constant, .. } => {
            // Determine the output width from the first monomial's first var,
            // or from a constant-only poly (width = 1 bit as default).
            let width = if let Some((key, _)) = coeffs.iter().next() {
                if let Some(first_var) = key.first() {
                    get_ir(vars, first_var).len()
                } else {
                    1
                }
            } else {
                1
            };
            eval_poly(coeffs, constant, width, vars)
        }
        Stmt::Rol { src, ty, n } => {
            let val = get_ir(vars, src);
            let w = bit_width(*ty, types);
            rotate_left(&val, w, *n)
        }
        Stmt::Ror { src, ty, n } => {
            let val = get_ir(vars, src);
            let w = bit_width(*ty, types);
            rotate_right(&val, w, *n)
        }
        Stmt::Merge { parts, ty } => {
            let w = bit_width(*ty, types);
            let mut result = Vec::with_capacity(w);
            for part in parts {
                result.extend_from_slice(&get_ir(vars, part));
            }
            // Truncate or pad to expected width.
            result.truncate(w);
            while result.len() < w {
                result.push(false);
            }
            result
        }
        Stmt::Splat { src, ty } => {
            let bit = get_ir(vars, src)[0];
            let w = bit_width(*ty, types);
            vec![bit; w]
        }
        Stmt::Shuffle { result_bits, ty } => {
            let w = bit_width(*ty, types);
            let mut result = vec![false; w];
            for (i, (bit_idx, var)) in result_bits.iter().enumerate() {
                if i < w {
                    let val = get_ir(vars, var);
                    result[i] = val.get(*bit_idx as usize).copied().unwrap_or(false);
                }
            }
            result
        }
        Stmt::OracleCall { .. } => panic!("eval_ir: OracleCall not supported"),
        Stmt::OracleOutput { .. } => panic!("eval_ir: OracleOutput not supported"),
        Stmt::ActionCall { .. } => panic!("eval_ir: ActionCall not supported"),
        Stmt::ActionOutput { .. } => panic!("eval_ir: ActionOutput not supported"),
        Stmt::Rng { .. } => panic!("eval_ir: Rng not supported"),
        Stmt::StorageRead { storage: store_id, ty, addr } => {
            let w = bit_width(*ty, types);
            let addr_val = get_ir(vars, addr);
            let addr_u64 = bits_to_u64(&addr_val);
            storage
                .get(&(*store_id, addr_u64))
                .cloned()
                .unwrap_or_else(|| vec![false; w])
        }
        Stmt::StorageWrite { storage: store_id, src, ty, addr } => {
            let src_val = get_ir(vars, src);
            let addr_val = get_ir(vars, addr);
            let addr_u64 = bits_to_u64(&addr_val);
            let w = bit_width(*ty, types);
            let mut val = src_val;
            val.truncate(w);
            while val.len() < w {
                val.push(false);
            }
            storage.insert((*store_id, addr_u64), val);
            vec![] // StorageWrite has no output
        }
    }
}

// ============================================================================
// Type utilities
// ============================================================================

/// Number of bits used by the type identified by `ty_id`.
pub fn bit_width(ty_id: TypeId, types: &IRTypes) -> usize {
    match &types.0[ty_id.0 as usize] {
        IrType::Primitive(t) => primitive_width(*t),
        IrType::Vec(n, elem_ty) => n * bit_width(*elem_ty, types),
        IrType::Tuple(elems) => elems.iter().map(|&e| bit_width(e, types)).sum(),
        IrType::Block { .. } => panic!("bit_width: Block type has no concrete bit width"),
        IrType::Func { .. } => panic!("bit_width: Func type has no concrete bit width"),
    }
}

/// Bit width of a primitive `Type`.
pub fn primitive_width(ty: Type) -> usize {
    match ty {
        Type::Bit => 1,
        Type::_8 | Type::AES8 => 8,
        Type::_16 => 16,
        Type::_32 => 32,
        Type::_64 | Type::Galois64 => 64,
        Type::_128 => 128,
        Type::_256 => 256,
        _ => panic!("primitive_width: unknown Type variant"),
    }
}

// ============================================================================
// Bit-vector operations
// ============================================================================

/// Convert a `Constant` (256-bit) to a bit vector of `width` bits, LSB first.
pub fn const_to_bits(c: &Constant, width: usize) -> IrValue {
    let mut result = Vec::with_capacity(width);
    for k in 0..width {
        let bit = if k < 128 {
            ((c.lo >> k) & 1) != 0
        } else {
            ((c.hi >> (k - 128)) & 1) != 0
        };
        result.push(bit);
    }
    result
}

/// Reinterpret `src` bits into a value of `dst_width` bits (truncate or
/// zero-extend as needed).
pub fn transmute_bits(src: &[bool], dst_width: usize) -> IrValue {
    let mut result = src.to_vec();
    result.truncate(dst_width);
    while result.len() < dst_width {
        result.push(false);
    }
    result
}

/// Rotate a bit-vector of the first `width` bits left by `n` positions.
pub fn rotate_left(val: &[bool], width: usize, n: usize) -> IrValue {
    if width == 0 {
        return vec![];
    }
    let n = n % width;
    let mut result = vec![false; width];
    for i in 0..width {
        result[(i + n) % width] = val.get(i).copied().unwrap_or(false);
    }
    result
}

/// Rotate a bit-vector of the first `width` bits right by `n` positions.
pub fn rotate_right(val: &[bool], width: usize, n: usize) -> IrValue {
    if width == 0 {
        return vec![];
    }
    let n = n % width;
    rotate_left(val, width, width - n)
}

/// Evaluate a GF(2) polynomial pointwise across `width` bit positions.
///
/// For bit position `k`:
///   result[k] = (constant bit k) XOR (XOR of monomials where coeff is odd:
///               AND of var[k] for each var in the monomial key)
pub fn eval_poly(
    coeffs: &std::collections::BTreeMap<Vec<IRVarId>, u8>,
    constant: &Constant,
    width: usize,
    vars: &BTreeMap<u32, IrValue>,
) -> IrValue {
    let mut result = vec![false; width];
    for k in 0..width {
        // Degree-0 term: the constant bit k.
        let const_bit = if k < 128 {
            ((constant.lo >> k) & 1) != 0
        } else {
            ((constant.hi >> (k - 128)) & 1) != 0
        };
        let mut acc = const_bit;
        // Higher-degree terms.
        for (monomial, coeff) in coeffs {
            if coeff & 1 == 0 {
                continue;
            }
            // AND of bit k of every variable in the monomial.
            let product = monomial.iter().all(|var| {
                get_ir(vars, var).get(k).copied().unwrap_or(false)
            });
            acc ^= product;
        }
        result[k] = acc;
    }
    result
}

/// Look up a variable by ID, panicking with a clear message if missing.
fn get_ir(vars: &BTreeMap<u32, IrValue>, id: &IRVarId) -> IrValue {
    vars.get(&id.0)
        .cloned()
        .unwrap_or_else(|| panic!("eval_ir: var {} not found", id.0))
}

/// Convert the first 64 bits of a bit-vector to a `u64` (LSB at index 0).
pub fn bits_to_u64(bits: &[bool]) -> u64 {
    let mut result = 0u64;
    for (i, &b) in bits.iter().enumerate().take(64) {
        if b {
            result |= 1u64 << i;
        }
    }
    result
}

// ============================================================================
// Bit-flatten / unflatten (used by the lower_ir_to_boolar property test)
// ============================================================================

/// Flatten a list of typed values into a single `Vec<bool>` for feeding into
/// a `BIrBlocks` circuit.  The order is: all bits of value[0] (LSB first),
/// then all bits of value[1], etc.
pub fn bit_flatten(values: &[IrValue]) -> Vec<bool> {
    values.iter().flat_map(|v| v.iter().copied()).collect()
}

/// Reconstruct a list of typed values from a flat `Vec<bool>` produced by a
/// `BIrBlocks` circuit, given the expected widths of each output value.
pub fn bit_unflatten(bits: &[bool], widths: &[usize]) -> Vec<IrValue> {
    let mut result = Vec::with_capacity(widths.len());
    let mut offset = 0;
    for &w in widths {
        let val: Vec<bool> = bits[offset..offset + w].to_vec();
        result.push(val);
        offset += w;
    }
    result
}

// ============================================================================
// Unit tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use volar_ir::ir::{IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId};
    use volar_ir_common::{Constant, IrType, Stmt, Type, TypeId, TypeTable};

    fn zero_const() -> Constant {
        Constant { hi: 0, lo: 0 }
    }
    fn one_const() -> Constant {
        Constant { hi: 0, lo: 1 }
    }

    fn simple_block(
        params: Vec<TypeId>,
        stmts: Vec<IRStmt>,
        terminator: IRTerminator,
    ) -> IRBlock<()> {
        let n = stmts.len();
        IRBlock {
            params,
            stmts,
            stmt_provs: vec![(); n],
            terminator,
        }
    }

    fn single_block_return(
        params: Vec<TypeId>,
        stmts: Vec<IRStmt>,
        ret_args: Vec<IRVarId>,
    ) -> IRBlocks<()> {
        IRBlocks::new(vec![simple_block(
            params,
            stmts,
            IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: ret_args,
            },
        )])
    }

    #[test]
    fn const_zero_evaluates_to_false() {
        let mut types = TypeTable::new();
        let bit = types.bit();
        // Block: Const(0, Bit) → return it
        let blocks = single_block_return(
            vec![],
            vec![Stmt::Const(zero_const(), bit)],
            vec![IRVarId(0)],
        );
        let result = eval_ir(&blocks, &types, &[]).unwrap();
        assert_eq!(result, vec![vec![false]]);
    }

    #[test]
    fn const_one_evaluates_to_true() {
        let mut types = TypeTable::new();
        let bit = types.bit();
        let blocks = single_block_return(
            vec![],
            vec![Stmt::Const(one_const(), bit)],
            vec![IRVarId(0)],
        );
        let result = eval_ir(&blocks, &types, &[]).unwrap();
        assert_eq!(result, vec![vec![true]]);
    }

    #[test]
    fn identity_param_returned() {
        let mut types = TypeTable::new();
        let bit = types.bit();
        let blocks = single_block_return(vec![bit], vec![], vec![IRVarId(0)]);
        assert_eq!(
            eval_ir(&blocks, &types, &[vec![true]]).unwrap(),
            vec![vec![true]]
        );
        assert_eq!(
            eval_ir(&blocks, &types, &[vec![false]]).unwrap(),
            vec![vec![false]]
        );
    }

    #[test]
    fn poly_and_of_two_bits() {
        // Poly: {[v0, v1]: 1}, constant=0 → result = v0 AND v1
        let mut types = TypeTable::new();
        let bit = types.bit();
        let v0 = IRVarId(0);
        let v1 = IRVarId(1);
        let mut coeffs = std::collections::BTreeMap::new();
        coeffs.insert(vec![v0, v1], 1u8);
        let stmt = Stmt::Poly {
            ty: bit,
            coeffs,
            constant: zero_const(),
        };
        let blocks = single_block_return(vec![bit, bit], vec![stmt], vec![IRVarId(2)]);
        let tt: Vec<([bool; 2], bool)> = vec![
            ([false, false], false),
            ([false, true], false),
            ([true, false], false),
            ([true, true], true),
        ];
        for ([a, b], expected) in tt {
            let result = eval_ir(&blocks, &types, &[vec![a], vec![b]]).unwrap();
            assert_eq!(result, vec![vec![expected]], "AND({a},{b})");
        }
    }

    #[test]
    fn poly_xor_of_two_bits() {
        // Poly: {[v0]: 1, [v1]: 1}, constant=0 → result = v0 XOR v1
        let mut types = TypeTable::new();
        let bit = types.bit();
        let v0 = IRVarId(0);
        let v1 = IRVarId(1);
        let mut coeffs = std::collections::BTreeMap::new();
        coeffs.insert(vec![v0], 1u8);
        coeffs.insert(vec![v1], 1u8);
        let stmt = Stmt::Poly {
            ty: bit,
            coeffs,
            constant: zero_const(),
        };
        let blocks = single_block_return(vec![bit, bit], vec![stmt], vec![IRVarId(2)]);
        let tt: Vec<([bool; 2], bool)> = vec![
            ([false, false], false),
            ([false, true], true),
            ([true, false], true),
            ([true, true], false),
        ];
        for ([a, b], expected) in tt {
            let result = eval_ir(&blocks, &types, &[vec![a], vec![b]]).unwrap();
            assert_eq!(result, vec![vec![expected]], "XOR({a},{b})");
        }
    }

    #[test]
    fn rol_8bit() {
        let mut types = TypeTable::new();
        let u8_ty = types.primitive(Type::_8);
        let v0 = IRVarId(0);
        let stmt = Stmt::Rol {
            src: v0,
            ty: u8_ty,
            n: 1,
        };
        let blocks = single_block_return(vec![u8_ty], vec![stmt], vec![IRVarId(1)]);
        // 0b00000001 (= 1) rotated left 1 → 0b00000010 (= 2)
        let input: Vec<bool> = (0..8).map(|i| i == 0).collect(); // bit 0 = true
        let result = eval_ir(&blocks, &types, &[input]).unwrap();
        let result_byte: u8 = result[0]
            .iter()
            .enumerate()
            .map(|(i, &b)| (b as u8) << i)
            .sum();
        assert_eq!(result_byte, 2u8);
    }

    #[test]
    fn merge_two_bits_into_byte() {
        let mut types = TypeTable::new();
        let bit = types.bit();
        let u8_ty = types.primitive(Type::_8);
        // Params: 2 bits. Merge them + 6 zero-bits into a byte.
        // Actually just test Merge of bit-typed parts into a byte.
        let v0 = IRVarId(0);
        let v1 = IRVarId(1);
        // const six zero bits
        let c0 = Stmt::Const(zero_const(), bit);
        let c1 = Stmt::Const(zero_const(), bit);
        let c2 = Stmt::Const(zero_const(), bit);
        let c3 = Stmt::Const(zero_const(), bit);
        let c4 = Stmt::Const(zero_const(), bit);
        let c5 = Stmt::Const(zero_const(), bit);
        let v2 = IRVarId(2);
        let v3 = IRVarId(3);
        let v4 = IRVarId(4);
        let v5 = IRVarId(5);
        let v6 = IRVarId(6);
        let v7 = IRVarId(7);
        let merge = Stmt::Merge {
            parts: vec![v0, v1, v2, v3, v4, v5, v6, v7],
            ty: u8_ty,
        };
        let v8 = IRVarId(8);
        let blocks = single_block_return(
            vec![bit, bit],
            vec![c0, c1, c2, c3, c4, c5, merge],
            vec![v8],
        );
        // input (1, 0) → byte LSB=1, bit1=0, ... = 0b00000001 = 1
        let result = eval_ir(&blocks, &types, &[vec![true], vec![false]]).unwrap();
        let result_byte: u8 = result[0]
            .iter()
            .enumerate()
            .map(|(i, &b)| (b as u8) << i)
            .sum();
        assert_eq!(result_byte, 1u8);
    }

    #[test]
    fn rotate_operations() {
        assert_eq!(rotate_left(&[true, false, false, false], 4, 1), vec![false, true, false, false]);
        assert_eq!(rotate_right(&[true, false, false, false], 4, 1), vec![false, false, false, true]);
        assert_eq!(rotate_left(&[true, false, false, false], 4, 4), vec![true, false, false, false]);
    }

    #[test]
    fn const_to_bits_works() {
        let c = Constant { hi: 0, lo: 0b1010 };
        let bits = const_to_bits(&c, 4);
        assert_eq!(bits, vec![false, true, false, true]); // LSB first: bit0=0, bit1=1, bit2=0, bit3=1
    }

    #[test]
    fn bit_flatten_unflatten_roundtrip() {
        let vals = vec![vec![true, false], vec![false, false, true]];
        let flat = bit_flatten(&vals);
        let widths = vec![2, 3];
        let reconstructed = bit_unflatten(&flat, &widths);
        assert_eq!(reconstructed, vals);
    }
}
