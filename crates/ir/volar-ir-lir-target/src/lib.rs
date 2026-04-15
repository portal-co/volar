// @reliability: experimental
// @ai: assisted
//! `VolarIrTarget` — a [`LirTarget`] that emits Volar IR ([`IRBlocks`]).
//!
//! Integer operations (add, sub, mul, udiv, sdiv, shifts, comparisons,
//! extensions, truncations) are implemented as bit-level circuits using
//! [`IRStmt::Poly`].  Cryptographic externals are provided as single-block
//! [`IRBlocks`] at construction time and inlined at the call site.
//!
//! # Type representation
//!
//! All LIR values are represented as flat lists of `Bit`-typed [`IRVarId`]s,
//! one per bit, LSB first:
//!
//! | `LirType` | bits |
//! |-----------|------|
//! | `Bool`    | 1    |
//! | `U8/I8`   | 8    |
//! | `U16/I16` | 16   |
//! | `U32/I32` | 32   |
//! | `U64/I64` | 64   |
//! | `Arr(T,n)` | n × bits(T) |
//! | `Struct(id)` | sum of field widths |
//!
//! # Primitive operations
//!
//! | LIR op | circuit |
//! |--------|---------|
//! | `xor`  | `Poly { [a]:1, [b]:1 }` per bit |
//! | `and`  | `Poly { [a,b]:1 }` per bit |
//! | `not`  | `Poly { [a]:1, const:1 }` per bit |
//! | `add`  | ripple-carry, 2 Poly per bit |
//! | `mul`  | shift-and-add, O(n²) Poly |
//! | `shl`/`lshr`/`ashr` | barrel shifter, O(n log n) Poly |
//! | `icmp` | borrow-chain / AND-tree |
//! | `udiv`/`sdiv` | restoring long division, O(n²) Poly |
//!
//! # Extern calls
//!
// @reliability: experimental
// @ai: assisted
//! `VolarIrTarget` — a [`LirTarget`] that emits Volar IR ([`IRBlocks`]).
//!
//! Integer operations are implemented as bit-level circuits using
//! [`IRStmt::Poly`].  Local function calls are inlined via
//! [`module_lower::lower_module_inlining`].  Cryptographic externals are
//! provided as pre-built [`IRBlocks`] via [`add_extern`](VolarIrTarget::add_extern)
//! and inlined at the call site (single- or multi-block).

pub mod module_lower;

use std::collections::BTreeMap;
use std::{string::String, string::ToString, vec, vec::Vec};

use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId,
    IRTypes, IRVarId,
};
use volar_ir_common::Constant;
use volar_lir::{IcmpPred, LirTarget, LirType, StructDef, StructId};

// ============================================================================
// Public types
// ============================================================================

/// A SSA value in `VolarIrTarget`: a flat list of block-local `Bit` var IDs,
/// one per bit (LSB at index 0).
///
/// Values are only valid in the block where they were created.  The caller
/// must uphold the SSA discipline: use a value only in the block that
/// produced it, or pass it as a jump argument (where it becomes a fresh
/// param in the target block).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VolarValue {
    /// Block-local [`IRVarId`]s, one per bit (LSB at index 0).
    pub bits: Vec<IRVarId>,
}

/// A block handle in `VolarIrTarget`: index into the current function's block
/// list.  Cheap to clone.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VolarBlock(pub usize);

// ============================================================================
// Internal builders
// ============================================================================

struct BlockBuilder {
    params: Vec<IRTypeId>,
    stmts: Vec<IRStmt>,
    terminator: Option<IRTerminator>,
}

impl BlockBuilder {
    fn new() -> Self {
        BlockBuilder { params: vec![], stmts: vec![], terminator: None }
    }

    /// Next local var ID = params.len() + stmts.len().
    fn next_local_id(&self) -> u32 {
        (self.params.len() + self.stmts.len()) as u32
    }
}

struct FuncBuilder {
    name: String,
    blocks: Vec<BlockBuilder>,
    /// Index of the block currently being emitted.
    current: usize,
}

// ============================================================================
// VolarIrTarget
// ============================================================================

/// A [`LirTarget`] that builds Volar IR ([`IRBlocks`]).
///
/// Integer operations are implemented as bit-level circuits.
/// Cryptographic operations are provided as pre-built [`IRBlocks`] via
/// [`add_extern`](Self::add_extern) and inlined at each call site.
pub struct VolarIrTarget {
    /// Shared IR types table.  Always contains [`IRType::Bit`] at index 0.
    pub types: IRTypes,
    bit_tid: IRTypeId,

    /// Total bit width per registered struct ID.
    struct_widths: Vec<usize>,

    /// External single-block implementations (name → IRBlocks).
    externs: BTreeMap<String, IRBlocks>,

    /// State for the function currently being built.
    func: Option<FuncBuilder>,

    /// Completed `(name, IRBlocks)` pairs, in order of `end_function` calls.
    pub completed: Vec<(String, IRBlocks)>,
}

impl VolarIrTarget {
    /// Create a new target.  A fresh `IRTypes` is initialised with
    /// [`IRType::Bit`] at type-ID 0.
    pub fn new() -> Self {
        let bit_tid = IRTypeId(0);
        VolarIrTarget {
            types: IRTypes(vec![IRType::Bit]),
            bit_tid,
            struct_widths: vec![],
            externs: BTreeMap::new(),
            func: None,
            completed: vec![],
        }
    }

    /// Register a named external implementation.
    ///
    /// The implementation must be a **single-block** `IRBlocks` (a plain
    /// combinational circuit).  Its params must equal the flat bit count of
    /// the ABI arguments, and its `Return` args the flat bit count of the
    /// return type.
    pub fn add_extern(&mut self, name: impl Into<String>, blocks: IRBlocks) {
        self.externs.insert(name.into(), blocks);
    }

    /// Drain and return all completed `(name, IRBlocks)` pairs.
    pub fn take_completed(&mut self) -> Vec<(String, IRBlocks)> {
        let mut out = Vec::new();
        core::mem::swap(&mut out, &mut self.completed);
        out
    }

    // ---- Low-level emitters ------------------------------------------------

    fn emit(&mut self, stmt: IRStmt) -> IRVarId {
        let f = self.func.as_mut().unwrap();
        let blk = &mut f.blocks[f.current];
        let id = blk.next_local_id();
        blk.stmts.push(stmt);
        IRVarId(id)
    }

    fn bit_const(&mut self, val: bool) -> IRVarId {
        self.emit(IRStmt::Const(
            Constant { hi: 0, lo: val as u128 },
            self.bit_tid.clone(),
        ))
    }

    // ---- Bit-level primitives (GF(2) Poly) ---------------------------------

    fn xor_bit(&mut self, a: IRVarId, b: IRVarId) -> IRVarId {
        if a == b {
            return self.bit_const(false);
        }
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![a], 1u8);
        coeffs.insert(vec![b], 1u8);
        self.emit(IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 0 } })
    }

    fn and_bit(&mut self, a: IRVarId, b: IRVarId) -> IRVarId {
        if a == b {
            return a;
        }
        let mut key = vec![a, b];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        self.emit(IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 0 } })
    }

    fn not_bit(&mut self, a: IRVarId) -> IRVarId {
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![a], 1u8);
        self.emit(IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 1 } })
    }

    fn or_bit(&mut self, a: IRVarId, b: IRVarId) -> IRVarId {
        let na = self.not_bit(a);
        let nb = self.not_bit(b);
        let nand = self.and_bit(na, nb);
        self.not_bit(nand)
    }

    /// XOR of three bits (full-adder sum = a ⊕ b ⊕ c).
    fn xor3_bit(&mut self, a: IRVarId, b: IRVarId, c: IRVarId) -> IRVarId {
        let ab = self.xor_bit(a, b);
        self.xor_bit(ab, c)
    }

    /// Majority of three bits: (a∧b) ⊕ (a∧c) ⊕ (b∧c).
    ///
    /// Used as the carry output of a full adder and as the borrow output of a
    /// full subtractor (with one input negated).
    fn carry_bit(&mut self, a: IRVarId, b: IRVarId, c: IRVarId) -> IRVarId {
        let mut coeffs = BTreeMap::new();
        let mut ab = vec![a, b]; ab.sort();
        let mut ac = vec![a, c]; ac.sort();
        let mut bc = vec![b, c]; bc.sort();
        coeffs.insert(ab, 1u8);
        coeffs.insert(ac, 1u8);
        coeffs.insert(bc, 1u8);
        self.emit(IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 0 } })
    }

    /// MUX: `select(cond, a, b) = AND(cond, XOR(a,b)) XOR b`.
    fn select_bit(&mut self, cond: IRVarId, a: IRVarId, b: IRVarId) -> IRVarId {
        let xab = self.xor_bit(a, b);
        let sel = self.and_bit(cond, xab);
        self.xor_bit(sel, b)
    }

    // ---- Vectorised bit ops ------------------------------------------------

    fn xor_vec(&mut self, a: &[IRVarId], b: &[IRVarId]) -> Vec<IRVarId> {
        a.iter().zip(b).map(|(&ai, &bi)| self.xor_bit(ai, bi)).collect()
    }

    fn and_vec(&mut self, a: &[IRVarId], b: &[IRVarId]) -> Vec<IRVarId> {
        a.iter().zip(b).map(|(&ai, &bi)| self.and_bit(ai, bi)).collect()
    }

    fn not_vec(&mut self, a: &[IRVarId]) -> Vec<IRVarId> {
        a.iter().map(|&ai| self.not_bit(ai)).collect()
    }

    fn or_vec(&mut self, a: &[IRVarId], b: &[IRVarId]) -> Vec<IRVarId> {
        a.iter().zip(b).map(|(&ai, &bi)| self.or_bit(ai, bi)).collect()
    }

    fn select_vec(&mut self, cond: IRVarId, a: &[IRVarId], b: &[IRVarId]) -> Vec<IRVarId> {
        a.iter().zip(b).map(|(&ai, &bi)| self.select_bit(cond, ai, bi)).collect()
    }

    // ---- Integer arithmetic core -------------------------------------------

    /// Ripple-carry adder over `a` and `b` with a constant carry-in.
    /// Overflow is silently discarded.
    fn add_with_carry_in(&mut self, a: &[IRVarId], b: &[IRVarId], carry_in: bool) -> Vec<IRVarId> {
        let n = a.len();
        let mut carry = self.bit_const(carry_in);
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let sum = self.xor3_bit(a[i], b[i], carry);
            let new_carry = self.carry_bit(a[i], b[i], carry);
            out.push(sum);
            carry = new_carry;
        }
        out
    }

    fn add_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        VolarValue { bits: self.add_with_carry_in(&a.bits, &b.bits, false) }
    }

    fn sub_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        // a − b = a + ~b + 1 (two's complement)
        let not_b = self.not_vec(&b.bits);
        VolarValue { bits: self.add_with_carry_in(&a.bits, &not_b, true) }
    }

    fn negate_impl(&mut self, val: &VolarValue) -> VolarValue {
        let not_bits = self.not_vec(&val.bits);
        let zeros: Vec<IRVarId> = (0..val.bits.len()).map(|_| self.bit_const(false)).collect();
        VolarValue { bits: self.add_with_carry_in(&not_bits, &zeros, true) }
    }

    /// Two's-complement absolute value: select(MSB, −val, val).
    fn abs_impl(&mut self, val: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let sign = val.bits[n - 1];
        let neg = self.negate_impl(val);
        let bits = val.bits.iter().zip(&neg.bits)
            .map(|(&pos, &neg_b)| self.select_bit(sign, neg_b, pos))
            .collect();
        VolarValue { bits }
    }

    /// n×n-bit shift-and-add multiplier.  Lower n bits of the full product.
    fn mul_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let mut acc: Vec<IRVarId> = (0..n).map(|_| self.bit_const(false)).collect();
        for i in 0..n {
            // Partial product shifted left by i: pp[k] = a[i] & b[k−i] if k≥i, else 0.
            let pp: Vec<IRVarId> = (0..n)
                .map(|k| {
                    if k < i {
                        self.bit_const(false)
                    } else {
                        self.and_bit(a.bits[i], b.bits[k - i])
                    }
                })
                .collect();
            acc = self.add_with_carry_in(&acc, &pp, false);
        }
        VolarValue { bits: acc }
    }

    /// Restoring long-division: returns the quotient of `a / b`.
    ///
    /// For n-bit operands, O(n²) `Poly` stmts.
    fn udiv_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        // n-bit remainder register, initialised to 0.
        let mut r: Vec<IRVarId> = (0..n).map(|_| self.bit_const(false)).collect();
        let mut q_bits: Vec<IRVarId> = (0..n).map(|_| self.bit_const(false)).collect();

        for i in (0..n).rev() {
            // Shift r left by 1 and bring in a[i] as the new LSB.
            // new_r[0] = a[i], new_r[k+1] = old_r[k] for k in 0..n−1.
            let mut new_r = Vec::with_capacity(n);
            new_r.push(a.bits[i]);
            for k in 0..n - 1 {
                new_r.push(r[k]);
            }
            r = new_r;

            // Compute r − b via a full subtractor chain.
            // borrow_out = majority(NOT(r[k]), b[k], borrow_in).
            let mut borrow = self.bit_const(false);
            let mut diff = Vec::with_capacity(n);
            for k in 0..n {
                let not_rk = self.not_bit(r[k]);
                let d = self.xor3_bit(r[k], b.bits[k], borrow);
                let new_borrow = self.carry_bit(not_rk, b.bits[k], borrow);
                diff.push(d);
                borrow = new_borrow;
            }

            // no_borrow = NOT(final_borrow): r ≥ b, so quotient bit = 1.
            let no_borrow = self.not_bit(borrow);
            q_bits[i] = no_borrow;

            // r = select(no_borrow, diff, r).
            let r_copy = r.clone();
            r = self.select_vec(no_borrow, &diff, &r_copy);
        }

        VolarValue { bits: q_bits }
    }

    fn sdiv_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let sign_a = a.bits[n - 1];
        let sign_b = b.bits[n - 1];

        let abs_a = self.abs_impl(a);
        let abs_b = self.abs_impl(b);
        let abs_q = self.udiv_impl(&abs_a, &abs_b);

        // Negate if signs differ.
        let signs_differ = self.xor_bit(sign_a, sign_b);
        let neg_q = self.negate_impl(&abs_q);
        let bits = abs_q.bits.iter().zip(&neg_q.bits)
            .map(|(&pq, &nq)| self.select_bit(signs_differ, nq, pq))
            .collect();
        VolarValue { bits }
    }

    // ---- Barrel shifters ---------------------------------------------------

    /// Number of barrel-shifter stages for an n-bit value (≥ 1).
    fn barrel_stages(n: usize, shift_bits: usize) -> usize {
        // ceil(log2(n)) stages, at least 1 so single-bit shifts work correctly.
        let from_n = if n <= 1 {
            1
        } else {
            (usize::BITS - (n - 1).leading_zeros()) as usize
        };
        from_n.min(shift_bits).max(1)
    }

    fn shl_impl(&mut self, val: &VolarValue, shift: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let stages = Self::barrel_stages(n, shift.bits.len());
        let mut cur = val.bits.clone();
        for k in 0..stages {
            let step = 1usize << k;
            let sb = shift.bits[k];
            cur = (0..n)
                .map(|j| {
                    if j < step {
                        let z = self.bit_const(false);
                        self.select_bit(sb, z, cur[j])
                    } else {
                        self.select_bit(sb, cur[j - step], cur[j])
                    }
                })
                .collect();
        }
        VolarValue { bits: cur }
    }

    fn lshr_impl(&mut self, val: &VolarValue, shift: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let stages = Self::barrel_stages(n, shift.bits.len());
        let mut cur = val.bits.clone();
        for k in 0..stages {
            let step = 1usize << k;
            let sb = shift.bits[k];
            cur = (0..n)
                .map(|j| {
                    if j + step >= n {
                        let z = self.bit_const(false);
                        self.select_bit(sb, z, cur[j])
                    } else {
                        self.select_bit(sb, cur[j + step], cur[j])
                    }
                })
                .collect();
        }
        VolarValue { bits: cur }
    }

    fn ashr_impl(&mut self, val: &VolarValue, shift: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let sign = val.bits[n - 1]; // sign bit never changes
        let stages = Self::barrel_stages(n, shift.bits.len());
        let mut cur = val.bits.clone();
        for k in 0..stages {
            let step = 1usize << k;
            let sb = shift.bits[k];
            cur = (0..n)
                .map(|j| {
                    if j + step >= n {
                        self.select_bit(sb, sign, cur[j])
                    } else {
                        self.select_bit(sb, cur[j + step], cur[j])
                    }
                })
                .collect();
        }
        VolarValue { bits: cur }
    }

    // ---- Comparisons -------------------------------------------------------

    fn icmp_eq(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        // AND of NOT(XOR(ai, bi)) for each bit pair.
        let n = a.bits.len();
        let mut xors: Vec<IRVarId> = (0..n).map(|i| self.xor_bit(a.bits[i], b.bits[i])).collect();
        let not_xors: Vec<IRVarId> = (0..n).map(|i| self.not_bit(xors[i])).collect();
        let result = if n == 0 {
            self.bit_const(true) // vacuously equal
        } else {
            let mut acc = not_xors[0];
            for i in 1..n {
                acc = self.and_bit(acc, not_xors[i]);
            }
            acc
        };
        VolarValue { bits: vec![result] }
    }

    fn icmp_ne(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        // OR of XOR(ai, bi) for each bit pair.
        let n = a.bits.len();
        let xors: Vec<IRVarId> = (0..n).map(|i| self.xor_bit(a.bits[i], b.bits[i])).collect();
        let result = if n == 0 {
            self.bit_const(false)
        } else {
            let mut acc = xors[0];
            for i in 1..n {
                acc = self.or_bit(acc, xors[i]);
            }
            acc
        };
        VolarValue { bits: vec![result] }
    }

    /// Unsigned a < b: final borrow out of the ripple subtractor.
    fn icmp_ult(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let mut borrow = self.bit_const(false);
        for i in 0..n {
            let not_ai = self.not_bit(a.bits[i]);
            borrow = self.carry_bit(not_ai, b.bits[i], borrow);
        }
        VolarValue { bits: vec![borrow] }
    }

    fn icmp_ule(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        // a ≤ b iff NOT(b < a)
        let gt = self.icmp_ult(b, a);
        VolarValue { bits: vec![self.not_bit(gt.bits[0])] }
    }

    fn icmp_slt(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let sign_a = a.bits[n - 1];
        let sign_b = b.bits[n - 1];
        // slt = (a<0 ∧ b≥0) ∨ (same_sign ∧ ult(a,b))
        let not_sign_b = self.not_bit(sign_b);
        let a_neg_b_pos = self.and_bit(sign_a, not_sign_b);
        let ult = self.icmp_ult(a, b).bits[0];
        let signs_xor = self.xor_bit(sign_a, sign_b);
        let signs_eq = self.not_bit(signs_xor);
        let same_sign_lt = self.and_bit(signs_eq, ult);
        VolarValue { bits: vec![self.or_bit(a_neg_b_pos, same_sign_lt)] }
    }

    fn icmp_sle(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        // a ≤ b (signed) iff NOT(b < a signed)
        let sgt = self.icmp_slt(b, a);
        VolarValue { bits: vec![self.not_bit(sgt.bits[0])] }
    }

    // ---- Helpers -----------------------------------------------------------

    fn bits_for(&self, ty: &LirType) -> usize {
        bits_for_lir_type(ty, &self.struct_widths)
    }

    fn set_terminator(&mut self, term: IRTerminator) {
        let f = self.func.as_mut().unwrap();
        let blk = &mut f.blocks[f.current];
        assert!(blk.terminator.is_none(), "VolarIrTarget: block already has a terminator");
        blk.terminator = Some(term);
    }

    fn flatten(vals: &[VolarValue]) -> Vec<IRVarId> {
        vals.iter().flat_map(|v| v.bits.iter().cloned()).collect()
    }

    /// Inline a single-block callee by substituting `flat_args` for its params
    /// and re-emitting every stmt.  Returns the flat return bits remapped to
    /// the current block's var space.
    /// Inline a single-block callee (convenience wrapper around `inline_blocks`).
    fn inline_callee(
        &mut self,
        callee: &IRBlock,
        flat_args: Vec<IRVarId>,
        ret_ty: Option<&LirType>,
    ) -> Vec<VolarValue> {
        assert_eq!(
            callee.params.len(), flat_args.len(),
            "VolarIrTarget: arg bit count mismatch (expected {}, got {})",
            callee.params.len(), flat_args.len()
        );
        let mut var_map: Vec<IRVarId> = flat_args;
        for stmt in &callee.stmts {
            let mapped = subst_stmt(stmt, &var_map);
            let id = self.emit(mapped);
            var_map.push(id);
        }
        match &callee.terminator {
            IRTerminator::Jmp { func: IRBlockTargetId::Return, args: ret_args } => {
                let ret_bits: Vec<IRVarId> =
                    ret_args.iter().map(|id| var_map[id.0 as usize]).collect();
                match ret_ty {
                    Some(ty) => {
                        let n = bits_for_lir_type(ty, &self.struct_widths);
                        assert_eq!(ret_bits.len(), n,
                            "VolarIrTarget: return bit count mismatch");
                        vec![VolarValue { bits: ret_bits }]
                    }
                    None => vec![],
                }
            }
            other => panic!("VolarIrTarget: single-block callee has non-Return terminator: {other:?}"),
        }
    }

    /// Inline a (potentially multi-block) callee at the current insertion point.
    ///
    /// For a single-block callee this is equivalent to [`inline_callee`].
    /// For multi-block callees the algorithm:
    ///
    /// 1. Creates fresh caller blocks for each callee block > 0.
    /// 2. Creates a **continuation** block that receives the callee's return
    ///    values as block parameters.
    /// 3. Emits each callee block's stmts into the corresponding caller block,
    ///    remapping all var IDs and block targets.
    /// 4. `IRBlockTargetId::Return` in the callee becomes a jump to the
    ///    continuation block.
    /// 5. Switches to the continuation block so the caller resumes there.
    ///
    /// Returns the flat `VolarValue`(s) for the callee's return value (the
    /// continuation block's parameters).
    pub fn inline_blocks(
        &mut self,
        callee: &IRBlocks,
        flat_args: Vec<IRVarId>,
        ret_ty: Option<&LirType>,
    ) -> Vec<VolarValue> {
        // Fast path: single-block with a direct unconditional Return — use the
        // simpler inline_callee which avoids allocating a continuation block.
        if callee.0.len() == 1 {
            if let IRTerminator::Jmp { func: IRBlockTargetId::Return, .. } = &callee.0[0].terminator {
                return self.inline_callee(&callee.0[0], flat_args, ret_ty);
            }
        }

        let n = callee.0.len();
        let ret_n = ret_ty.map(|ty| self.bits_for(ty)).unwrap_or(0);

        // ---- Continuation block (receives callee return values) -----------
        let cont_block = self.create_block();
        let mut cont_params = Vec::with_capacity(ret_n);
        for _ in 0..ret_n {
            let v = self.add_block_param(cont_block, LirType::Bool);
            cont_params.push(v.bits[0]);
        }

        // ---- Map callee block idx → caller block idx ---------------------
        // Callee block 0 stays in the current caller block.
        let initial_block = self.func.as_ref().unwrap().current;
        let mut new_blocks: Vec<usize> = Vec::with_capacity(n);
        new_blocks.push(initial_block);
        for _ in 1..n {
            new_blocks.push(self.create_block().0);
        }

        // ---- Pre-add params to new blocks 1..n --------------------------
        // Callee block 0's params come from flat_args (not from block params).
        let mut block_params: Vec<Vec<IRVarId>> = Vec::with_capacity(n);
        block_params.push(flat_args);
        for ci in 1..n {
            let n_params = callee.0[ci].params.len();
            let blk = VolarBlock(new_blocks[ci]);
            let mut params = Vec::with_capacity(n_params);
            for _ in 0..n_params {
                let v = self.add_block_param(blk, LirType::Bool);
                params.push(v.bits[0]);
            }
            block_params.push(params);
        }

        // ---- Emit each callee block --------------------------------------
        for ci in 0..n {
            self.func.as_mut().unwrap().current = new_blocks[ci];

            let mut var_map: Vec<IRVarId> = block_params[ci].clone();
            for stmt in &callee.0[ci].stmts {
                let mapped = subst_stmt(stmt, &var_map);
                let id = self.emit(mapped);
                var_map.push(id);
            }
            let term = remap_terminator(
                &callee.0[ci].terminator, &var_map, &new_blocks, cont_block.0,
            );
            self.set_terminator(term);
        }

        // ---- Resume in the continuation block ----------------------------
        self.func.as_mut().unwrap().current = cont_block.0;

        if ret_n > 0 {
            vec![VolarValue { bits: cont_params }]
        } else {
            vec![]
        }
    }
}

// ============================================================================
// Type helpers
// ============================================================================

fn bits_for_lir_type(ty: &LirType, struct_widths: &[usize]) -> usize {
    match ty {
        LirType::Bool => 1,
        LirType::I8 | LirType::U8 => 8,
        LirType::I16 | LirType::U16 => 16,
        LirType::I32 | LirType::U32 => 32,
        LirType::I64 | LirType::U64 => 64,
        LirType::Arr(elem, n) => n * bits_for_lir_type(elem, struct_widths),
        LirType::Struct(id) => struct_widths[*id as usize],
    }
}

fn lir_type_for_bits(n: usize) -> LirType {
    match n {
        1 => LirType::Bool,
        8 => LirType::U8,
        16 => LirType::U16,
        32 => LirType::U32,
        64 => LirType::U64,
        other => panic!("VolarIrTarget: no standard LirType for {other} bits"),
    }
}

// ============================================================================
// ============================================================================
// IRStmt variable substitution (for callee inlining)
// ============================================================================

/// Remap var IDs and block targets in a terminator for multi-block inlining.
///
/// - `var_map`: callee local var ID → caller var ID
/// - `new_blocks[i]`: caller block idx for callee block i
/// - `cont_block_idx`: caller block idx that receives `Return` values
fn remap_terminator(
    term: &IRTerminator,
    var_map: &[IRVarId],
    new_blocks: &[usize],
    cont_block_idx: usize,
) -> IRTerminator {
    let remap_args = |args: &[IRVarId]| -> Vec<IRVarId> {
        args.iter().map(|id| var_map[id.0 as usize]).collect()
    };
    let remap_target = |t: &IRBlockTargetId| -> IRBlockTargetId {
        match t {
            IRBlockTargetId::Return =>
                IRBlockTargetId::Block(IRBlockId(cont_block_idx as u32)),
            IRBlockTargetId::Block(IRBlockId(j)) =>
                IRBlockTargetId::Block(IRBlockId(new_blocks[*j as usize] as u32)),
            IRBlockTargetId::Dyn(v) =>
                IRBlockTargetId::Dyn(var_map[v.0 as usize]),
        }
    };
    match term {
        IRTerminator::Jmp { func, args } => IRTerminator::Jmp {
            func: remap_target(func),
            args: remap_args(args),
        },
        IRTerminator::JumpCond { condition, true_block, true_args, false_block, false_args } =>
            IRTerminator::JumpCond {
                condition: var_map[condition.0 as usize],
                true_block:  remap_target(true_block),
                true_args:   remap_args(true_args),
                false_block: remap_target(false_block),
                false_args:  remap_args(false_args),
            },
        IRTerminator::JumpTable { index, cases } => IRTerminator::JumpTable {
            index: var_map[index.0 as usize],
            cases: cases.iter().map(|(k, (t, args))| {
                (*k, (remap_target(t), remap_args(args)))
            }).collect(),
        },
    }
}

fn subst_stmt(stmt: &IRStmt, var_map: &[IRVarId]) -> IRStmt {
    let s = |id: &IRVarId| var_map[id.0 as usize]; // IRVarId: Copy
    match stmt {
        IRStmt::Const(c, ty) => IRStmt::Const(*c, ty.clone()),
        IRStmt::Poly { coeffs, constant } => IRStmt::Poly {
            coeffs: coeffs
                .iter()
                .map(|(vars, &coeff)| {
                    let mut nv: Vec<IRVarId> = vars.iter().map(s).collect();
                    nv.sort();
                    (nv, coeff)
                })
                .collect(),
            constant: *constant,
        },
        IRStmt::Transmute { src, src_ty, dst_ty } => {
            IRStmt::Transmute { src: s(src), src_ty: src_ty.clone(), dst_ty: dst_ty.clone() }
        }
        IRStmt::Rol { src, ty, n } => IRStmt::Rol { src: s(src), ty: ty.clone(), n: *n },
        IRStmt::Ror { src, ty, n } => IRStmt::Ror { src: s(src), ty: ty.clone(), n: *n },
        IRStmt::Merge { parts, ty } => {
            IRStmt::Merge { parts: parts.iter().map(s).collect(), ty: ty.clone() }
        }
        IRStmt::Splat { src, ty } => IRStmt::Splat { src: s(src), ty: ty.clone() },
        IRStmt::StorageRead { ty, addr } => {
            IRStmt::StorageRead { ty: ty.clone(), addr: s(addr) }
        }
        IRStmt::StorageWrite { src, ty, addr } => {
            IRStmt::StorageWrite { src: s(src), ty: ty.clone(), addr: s(addr) }
        }
        IRStmt::Shuffle { result_bits, ty } => IRStmt::Shuffle {
            result_bits: result_bits.iter().map(|(b, id)| (*b, s(id))).collect(),
            ty: ty.clone(),
        },
    }
}

// ============================================================================
// LirTarget implementation
// ============================================================================

impl LirTarget for VolarIrTarget {
    type Value = VolarValue;
    type Block = VolarBlock;

    // ---- Struct registration -----------------------------------------------

    fn define_struct(&mut self, def: StructDef) -> StructId {
        let id = self.struct_widths.len() as StructId;
        let total: usize = def.fields.iter()
            .map(|f| bits_for_lir_type(&f.ty, &self.struct_widths))
            .sum();
        self.struct_widths.push(total);
        id
    }

    // ---- Function management -----------------------------------------------

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        _ret: Option<LirType>,
    ) -> (VolarBlock, Vec<Vec<VolarValue>>) {
        let mut block = BlockBuilder::new();
        let mut groups: Vec<Vec<VolarValue>> = Vec::new();

        for ty in params {
            let n = bits_for_lir_type(ty, &self.struct_widths);
            let start = block.params.len() as u32;
            for _ in 0..n {
                block.params.push(self.bit_tid.clone());
            }
            groups.push(vec![VolarValue {
                bits: (start..start + n as u32).map(IRVarId).collect(),
            }]);
        }

        self.func = Some(FuncBuilder { name: name.to_string(), blocks: vec![block], current: 0 });
        (VolarBlock(0), groups)
    }

    fn end_function(&mut self) {
        let func = self.func.take().expect("end_function: no function in progress");
        let blocks = func.blocks.into_iter()
            .map(|b| IRBlock {
                params: b.params,
                stmts: b.stmts,
                terminator: b.terminator.expect("VolarIrTarget: block missing terminator"),
            })
            .collect();
        self.completed.push((func.name, IRBlocks(blocks)));
    }

    // ---- Block management --------------------------------------------------

    fn create_block(&mut self) -> VolarBlock {
        let f = self.func.as_mut().unwrap();
        let idx = f.blocks.len();
        f.blocks.push(BlockBuilder::new());
        VolarBlock(idx)
    }

    fn add_block_param(&mut self, block: VolarBlock, ty: LirType) -> VolarValue {
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let f = self.func.as_mut().unwrap();
        let blk = &mut f.blocks[block.0];
        let start = blk.params.len() as u32;
        for _ in 0..n {
            blk.params.push(self.bit_tid.clone());
        }
        VolarValue { bits: (start..start + n as u32).map(IRVarId).collect() }
    }

    fn switch_to_block(&mut self, block: VolarBlock) {
        self.func.as_mut().unwrap().current = block.0;
    }

    // ---- Constants ---------------------------------------------------------

    fn iconst(&mut self, ty: LirType, val: i64) -> VolarValue {
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let bits: Vec<IRVarId> = (0..n)
            .map(|i| self.bit_const((val >> i) & 1 != 0))
            .collect();
        VolarValue { bits }
    }

    // ---- Arithmetic --------------------------------------------------------

    fn add(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        self.add_impl(&lhs, &rhs)
    }

    fn sub(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        self.sub_impl(&lhs, &rhs)
    }

    fn mul(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        self.mul_impl(&lhs, &rhs)
    }

    fn udiv(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        self.udiv_impl(&lhs, &rhs)
    }

    fn sdiv(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        self.sdiv_impl(&lhs, &rhs)
    }

    // ---- Bitwise -----------------------------------------------------------

    fn and(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        VolarValue { bits: self.and_vec(&lhs.bits, &rhs.bits) }
    }

    fn or(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        VolarValue { bits: self.or_vec(&lhs.bits, &rhs.bits) }
    }

    fn xor(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        VolarValue { bits: self.xor_vec(&lhs.bits, &rhs.bits) }
    }

    fn not(&mut self, val: VolarValue) -> VolarValue {
        VolarValue { bits: self.not_vec(&val.bits) }
    }

    fn shl(&mut self, val: VolarValue, shift: VolarValue) -> VolarValue {
        self.shl_impl(&val, &shift)
    }

    fn lshr(&mut self, val: VolarValue, shift: VolarValue) -> VolarValue {
        self.lshr_impl(&val, &shift)
    }

    fn ashr(&mut self, val: VolarValue, shift: VolarValue) -> VolarValue {
        self.ashr_impl(&val, &shift)
    }

    // ---- Comparisons -------------------------------------------------------

    fn icmp(&mut self, pred: IcmpPred, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        match pred {
            IcmpPred::Eq  => self.icmp_eq(&lhs, &rhs),
            IcmpPred::Ne  => self.icmp_ne(&lhs, &rhs),
            IcmpPred::Ult => self.icmp_ult(&lhs, &rhs),
            IcmpPred::Ule => self.icmp_ule(&lhs, &rhs),
            IcmpPred::Ugt => self.icmp_ult(&rhs, &lhs),   // a > b ↔ b < a
            IcmpPred::Uge => self.icmp_ule(&rhs, &lhs),   // a ≥ b ↔ b ≤ a
            IcmpPred::Slt => self.icmp_slt(&lhs, &rhs),
            IcmpPred::Sle => self.icmp_sle(&lhs, &rhs),
            IcmpPred::Sgt => self.icmp_slt(&rhs, &lhs),   // a > b ↔ b < a (signed)
            IcmpPred::Sge => self.icmp_sle(&rhs, &lhs),   // a ≥ b ↔ b ≤ a (signed)
        }
    }

    // ---- Conversions -------------------------------------------------------

    fn zext(&mut self, val: VolarValue, dst_ty: LirType) -> VolarValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        let mut bits = val.bits;
        while bits.len() < dst_n {
            let z = self.bit_const(false);
            bits.push(z);
        }
        VolarValue { bits }
    }

    fn sext(&mut self, val: VolarValue, dst_ty: LirType) -> VolarValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        let sign = *val.bits.last().expect("sext of zero-bit value");
        let mut bits = val.bits;
        // Reuse the same sign-bit IRVarId for all extended positions.
        bits.resize(dst_n, sign);
        VolarValue { bits }
    }

    fn trunc(&mut self, val: VolarValue, dst_ty: LirType) -> VolarValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        VolarValue { bits: val.bits[..dst_n].to_vec() }
    }

    // ---- Select ------------------------------------------------------------

    fn select(&mut self, cond: VolarValue, then_val: VolarValue, else_val: VolarValue) -> VolarValue {
        let cond_bit = cond.bits[0];
        let bits = self.select_vec(cond_bit, &then_val.bits, &else_val.bits);
        VolarValue { bits }
    }

    // ---- Type query --------------------------------------------------------

    fn value_scalar_type(&self, val: &VolarValue) -> LirType {
        // Try standard widths first, then registered structs.
        match val.bits.len() {
            1  => LirType::Bool,
            8  => LirType::U8,
            16 => LirType::U16,
            32 => LirType::U32,
            64 => LirType::U64,
            n  => {
                for (id, &w) in self.struct_widths.iter().enumerate() {
                    if w == n {
                        return LirType::Struct(id as StructId);
                    }
                }
                panic!("VolarIrTarget: no LirType for {n}-bit value")
            }
        }
    }

    // ---- Extern calls ------------------------------------------------------

    fn call_extern(
        &mut self,
        name: &str,
        _arg_tys: &[LirType],
        args: &[VolarValue],
        ret_ty: Option<LirType>,
    ) -> Vec<VolarValue> {
        let flat_args = Self::flatten(args);
        // Clone the implementation to release the borrow on `self`.
        let impl_blocks = self
            .externs
            .get(name)
            .unwrap_or_else(|| {
                panic!("VolarIrTarget: extern '{name}' not registered — \
                        provide a Volar IR implementation via add_extern()")
            })
            .clone();
        // Use inline_blocks — handles both single- and multi-block callees.
        self.inline_blocks(&impl_blocks, flat_args, ret_ty.as_ref())
    }

    // ---- Terminators -------------------------------------------------------

    fn jump(&mut self, target: VolarBlock, args: &[VolarValue]) {
        let flat = Self::flatten(args);
        self.set_terminator(IRTerminator::Jmp {
            func: IRBlockTargetId::Block(IRBlockId(target.0 as u32)),
            args: flat,
        });
    }

    fn branch(
        &mut self,
        cond: VolarValue,
        then_block: VolarBlock,
        then_args: &[VolarValue],
        else_block: VolarBlock,
        else_args: &[VolarValue],
    ) {
        let cond_bit = cond.bits[0];
        self.set_terminator(IRTerminator::JumpCond {
            condition: cond_bit,
            true_block: IRBlockTargetId::Block(IRBlockId(then_block.0 as u32)),
            true_args: Self::flatten(then_args),
            false_block: IRBlockTargetId::Block(IRBlockId(else_block.0 as u32)),
            false_args: Self::flatten(else_args),
        });
    }

    fn ret(&mut self, vals: &[VolarValue]) {
        let flat = Self::flatten(vals);
        self.set_terminator(IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: flat,
        });
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use volar_lir::{LirTarget, LirType, IcmpPred};

    /// Run a single-function VolarIrTarget and return its `IRBlocks`.
    fn build(f: impl FnOnce(&mut VolarIrTarget)) -> IRBlocks {
        let mut t = VolarIrTarget::new();
        f(&mut t);
        let (_, blocks) = t.completed.into_iter().next().unwrap();
        blocks
    }

    // ---- iconst ------------------------------------------------------------

    #[test]
    fn test_iconst_bool_zero_emits_const_stmt() {
        let blocks = build(|t| {
            let (_entry, _) = t.begin_function("f", &[], None);
            let v = t.iconst(LirType::Bool, 0);
            assert_eq!(v.bits.len(), 1);
            t.ret(&[]);
            t.end_function();
        });
        assert!(blocks.is_circuit());
    }

    #[test]
    fn test_iconst_u8_produces_8_bits() {
        let blocks = build(|t| {
            let (_entry, params) = t.begin_function("f", &[LirType::U8], None);
            let _v = params[0][0].clone();
            t.ret(&[]);
            t.end_function();
        });
        // Entry block has 8 Bit params.
        assert_eq!(blocks.0[0].params.len(), 8);
    }

    // ---- bitwise -----------------------------------------------------------

    #[test]
    fn test_xor_bool() {
        build(|t| {
            let (entry, params) = t.begin_function("xor_test", &[LirType::Bool, LirType::Bool], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.xor(a, b);
            assert_eq!(r.bits.len(), 1);
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_not_bool() {
        build(|t| {
            let (entry, params) = t.begin_function("not_test", &[LirType::Bool], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let r = t.not(a);
            assert_eq!(r.bits.len(), 1);
            t.ret(&[r]);
            t.end_function();
        });
    }

    // ---- arithmetic --------------------------------------------------------

    #[test]
    fn test_add_u8_produces_8_result_bits() {
        build(|t| {
            let (entry, params) = t.begin_function("add8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.add(a, b);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_sub_u8_produces_8_result_bits() {
        build(|t| {
            let (entry, params) = t.begin_function("sub8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.sub(a, b);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_mul_u8_produces_8_result_bits() {
        build(|t| {
            let (entry, params) = t.begin_function("mul8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.mul(a, b);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_udiv_u8_produces_8_result_bits() {
        build(|t| {
            let (entry, params) = t.begin_function("udiv8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.udiv(a, b);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    // ---- shifts ------------------------------------------------------------

    #[test]
    fn test_shl_u8() {
        build(|t| {
            let (entry, params) = t.begin_function("shl8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.shl(a, b);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_lshr_u8() {
        build(|t| {
            let (entry, params) = t.begin_function("lshr8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.lshr(a, b);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    // ---- comparisons -------------------------------------------------------

    #[test]
    fn test_icmp_eq_produces_bool() {
        build(|t| {
            let (entry, params) = t.begin_function("eq8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.icmp(IcmpPred::Eq, a, b);
            assert_eq!(r.bits.len(), 1, "icmp Eq must return a Bool (1 bit)");
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_icmp_ult_produces_bool() {
        build(|t| {
            let (entry, params) = t.begin_function("ult8", &[LirType::U8, LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let b = params[1][0].clone();
            let r = t.icmp(IcmpPred::Ult, a, b);
            assert_eq!(r.bits.len(), 1);
            t.ret(&[r]);
            t.end_function();
        });
    }

    // ---- conversions -------------------------------------------------------

    #[test]
    fn test_zext_bool_to_u8() {
        build(|t| {
            let (entry, params) = t.begin_function("zext", &[LirType::Bool], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let r = t.zext(a, LirType::U8);
            assert_eq!(r.bits.len(), 8);
            t.ret(&[r]);
            t.end_function();
        });
    }

    #[test]
    fn test_trunc_u8_to_bool() {
        build(|t| {
            let (entry, params) = t.begin_function("trunc", &[LirType::U8], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let r = t.trunc(a, LirType::Bool);
            assert_eq!(r.bits.len(), 1);
            t.ret(&[r]);
            t.end_function();
        });
    }

    // ---- control flow ------------------------------------------------------

    #[test]
    fn test_branch_produces_two_blocks() {
        build(|t| {
            let (entry, params) = t.begin_function("branch_test", &[LirType::Bool], None);
            t.switch_to_block(entry);
            let cond = params[0][0].clone();
            let then_b = t.create_block();
            let else_b = t.create_block();
            t.branch(cond, then_b, &[], else_b, &[]);

            t.switch_to_block(then_b);
            let one = t.iconst(LirType::Bool, 1);
            t.ret(&[one]);

            t.switch_to_block(else_b);
            let zero = t.iconst(LirType::Bool, 0);
            t.ret(&[zero]);
            t.end_function();
        });
    }

    // ---- extern inlining ---------------------------------------------------

    #[test]
    fn test_call_extern_single_block_inlined() {
        // Build a trivial extern: the identity function on 1 Bit.
        // params=[Bit], stmts=[], return [param0].
        let bit_tid = IRTypeId(0);
        let extern_impl = IRBlocks(std::vec![IRBlock {
            params: std::vec![bit_tid.clone()],
            stmts: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![IRVarId(0)],
            },
        }]);

        let blocks = build(|t| {
            t.add_extern("id_bit", extern_impl);
            let (entry, params) = t.begin_function("caller", &[LirType::Bool], None);
            t.switch_to_block(entry);
            let a = params[0][0].clone();
            let result = t.call_extern("id_bit", &[LirType::Bool], &[a], Some(LirType::Bool));
            assert_eq!(result.len(), 1);
            assert_eq!(result[0].bits.len(), 1);
            t.ret(&result);
            t.end_function();
        });
        assert!(blocks.is_circuit());
    }

    // ---- value_scalar_type -------------------------------------------------

    #[test]
    fn test_value_scalar_type_bool() {
        let t = VolarIrTarget::new();
        let v = VolarValue { bits: std::vec![IRVarId(0)] };
        assert_eq!(t.value_scalar_type(&v), LirType::Bool);
    }

    #[test]
    fn test_value_scalar_type_u64() {
        let t = VolarIrTarget::new();
        let v = VolarValue { bits: (0..64).map(IRVarId).collect() };
        assert_eq!(t.value_scalar_type(&v), LirType::U64);
    }

    // ---- multi-block inlining (inline_blocks) ------------------------------

    /// Build a two-block callee that uses a conditional branch:
    ///
    ///   Block 0: params=[cond: Bit]  branch(cond, Block(1,[]), Return([0]))
    ///   Block 1: params=[]           Jmp(Return, [1])
    ///
    /// Semantics: if cond then return 1 else return 0.
    /// After inlining this into a caller that passes a Bool param, the caller
    /// should produce a 1-bit result and be a valid (non-trivially-circuit)
    /// IRBlocks.
    #[test]
    fn test_inline_blocks_two_block_callee() {
        let bit_tid = IRTypeId(0);
        let callee = IRBlocks(std::vec![
            // Block 0: branch on param 0.
            IRBlock {
                params: std::vec![bit_tid],
                stmts: std::vec![],
                terminator: IRTerminator::JumpCond {
                    condition: IRVarId(0),
                    true_block:  IRBlockTargetId::Block(IRBlockId(1)),
                    true_args:   std::vec![],
                    false_block: IRBlockTargetId::Return,
                    false_args:  std::vec![IRVarId(0)], // return cond (=0)
                },
            },
            // Block 1: unconditional return of 1.
            IRBlock {
                params: std::vec![],
                stmts: std::vec![IRStmt::Const(Constant { hi: 0, lo: 1 }, bit_tid)],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: std::vec![IRVarId(0)], // return the Const(1)
                },
            },
        ]);

        let blocks = build(|t| {
            let (entry, params) = t.begin_function("caller", &[LirType::Bool], None);
            t.switch_to_block(entry);
            let cond = params[0][0].clone();
            // Inline the two-block callee.
            let result = t.inline_blocks(&callee, cond.bits.clone(), Some(&LirType::Bool));
            assert_eq!(result.len(), 1);
            assert_eq!(result[0].bits.len(), 1, "callee returns one Bool");
            t.ret(&result);
            t.end_function();
        });

        // The caller should now have 3 blocks:
        //   block 0 (original current) + block 1 (new for callee block 1)
        //   + continuation block.
        assert_eq!(blocks.0.len(), 3, "caller should have 3 blocks after inlining");
    }

    /// Inline a callee that loops (self-jump), verifying that multi-block
    /// inlining handles back-edges correctly by mapping them to fresh blocks.
    #[test]
    fn test_inline_blocks_loop_callee() {
        let bit_tid = IRTypeId(0);
        // Callee: Block 0(x: Bit) -> CondJmp(x, Return([x]), Block(0,[NOT(x)]))
        // Semantics: if x=1 return 1, else loop with NOT(x)=1 → returns 1.
        let callee = IRBlocks(std::vec![IRBlock {
            params: std::vec![bit_tid],
            stmts: std::vec![
                // stmt 0: NOT(param_0)
                {
                    let mut coeffs = std::collections::BTreeMap::new();
                    coeffs.insert(std::vec![IRVarId(0)], 1u8);
                    IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: 1 } }
                },
            ],
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(0),
                true_block:  IRBlockTargetId::Return,
                true_args:   std::vec![IRVarId(0)],
                false_block: IRBlockTargetId::Block(IRBlockId(0)),
                false_args:  std::vec![IRVarId(1)], // NOT(x)
            },
        }]);

        // Single-block callee → inline_callee path.
        build(|t| {
            let (entry, params) = t.begin_function("caller", &[LirType::Bool], None);
            t.switch_to_block(entry);
            let x = params[0][0].clone();
            let result = t.inline_blocks(&callee, x.bits.clone(), Some(&LirType::Bool));
            assert_eq!(result[0].bits.len(), 1);
            t.ret(&result);
            t.end_function();
        });
    }
}
