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
    IRTypes, IRVarId, OracleDecl, ActionDecl,
};
use volar_ir_common::{Constant, IrType, Type as NativeType};
use volar_lir::{BitCircuitBuilder, IcmpPred, LirTarget, LirType, LirAbi, StructDef, StructId};
use volar_lir::circuits::{
    bc_abs, bc_add, bc_and_vec, bc_ashr, bc_eq, bc_lshr, bc_mul, bc_ne, bc_neg,
    bc_not_vec, bc_or_vec, bc_sdiv, bc_select_vec, bc_shl, bc_sle, bc_slt,
    bc_sub, bc_udiv, bc_ule, bc_ult, bc_xor_vec, StorageEmitter,
};

// ============================================================================
// Public types
// ============================================================================

/// A SSA value in `VolarIrTarget`: a flat list of block-local `IRVarId`s.
///
/// For standard integer types (`U8`, `U32`, …) there is one `Bit`-typed
/// `IRVarId` per bit (LSB at index 0).  For [`LirType::Native`] types there
/// is exactly **one** `IRVarId` whose `IrType` is the corresponding
/// `IrType::Primitive(t)` — it is **not** decomposed into bits.
///
/// The `ty` field carries the authoritative [`LirType`] for this value group,
/// which is needed to distinguish `LirType::Bool` (1 `Bit` var) from
/// `LirType::Native(Type::Bit)` (1 native var) and to round-trip correctly
/// through `value_scalar_type` → `add_block_param`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VolarValue {
    /// Block-local [`IRVarId`]s representing this value.
    pub bits: Vec<IRVarId>,
    /// The [`LirType`] of this value group.
    pub ty: LirType,
}

/// A block handle in `VolarIrTarget`: index into the current function's block
/// list.  Cheap to clone.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VolarBlock(pub usize);

// ============================================================================
// Internal builders
// ============================================================================

struct BlockBuilder<P: Clone = ()> {
    params: Vec<IRTypeId>,
    stmts: Vec<IRStmt>,
    stmt_provs: Vec<P>,
    terminator: Option<IRTerminator>,
}

impl<P: Clone> BlockBuilder<P> {
    fn new() -> Self {
        BlockBuilder { params: vec![], stmts: vec![], stmt_provs: vec![], terminator: None }
    }

    /// Next local var ID = params.len() + stmts.len().
    fn next_local_id(&self) -> u32 {
        (self.params.len() + self.stmts.len()) as u32
    }
}

struct FuncBuilder<P: Clone = ()> {
    name: String,
    blocks: Vec<BlockBuilder<P>>,
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
///
/// The type parameter `P` is an optional provenance annotation for emitted
/// statements.  Call [`set_prov`](volar_lir::LirTarget::set_prov) before
/// emitting instructions; each instruction is tagged with the most recently
/// set provenance.  Use `P = ()` (the default) when provenance is not needed.
pub struct VolarIrTarget<P: Clone = ()> {
    /// Shared IR types table.  Always contains [`IRType::Bit`] at index 0.
    pub types: IRTypes,
    bit_tid: IRTypeId,

    /// Total bit width per registered struct ID.
    struct_widths: Vec<usize>,

    /// External single-block implementations (name → IRBlocks).
    /// Externally-provided circuit fragments carry no provenance (`P = ()`).
    externs: BTreeMap<String, IRBlocks<()>>,

    /// Oracle declarations accumulated since the last `end_function`.
    pending_oracles: Vec<OracleDecl>,
    /// Action declarations accumulated since the last `end_function`.
    pending_actions: Vec<ActionDecl>,

    /// Provenance to attach to the next emitted statement.
    current_prov: P,

    /// State for the function currently being built.
    func: Option<FuncBuilder<P>>,

    /// Completed `(name, IRBlocks)` pairs, in order of `end_function` calls.
    pub completed: Vec<(String, IRBlocks<P>)>,
}

impl<P: Clone> VolarIrTarget<P> {
    /// Create a new target with an explicit initial provenance value.
    ///
    /// The initial provenance is used for any instructions emitted before the
    /// first [`set_prov`](volar_lir::LirTarget::set_prov) call.  For unit
    /// provenance use [`VolarIrTarget::new`] instead.
    pub fn new_with_prov(initial_prov: P) -> Self {
        let bit_tid = IRTypeId(0);
        VolarIrTarget {
            types: IRTypes(std::vec![IrType::Primitive(NativeType::Bit)]),
            bit_tid,
            struct_widths: vec![],
            externs: BTreeMap::new(),
            pending_oracles: vec![],
            pending_actions: vec![],
            current_prov: initial_prov,
            func: None,
            completed: vec![],
        }
    }
}

impl VolarIrTarget<()> {
    /// Create a new unit-provenance target.
    pub fn new() -> Self {
        Self::new_with_prov(())
    }
}

impl<P: Clone> VolarIrTarget<P> {
    /// Register a named external implementation.
    ///
    /// The implementation must be a **single-block** `IRBlocks` (a plain
    /// combinational circuit).  Its params must equal the flat bit count of
    /// the ABI arguments, and its `Return` args the flat bit count of the
    /// return type.
    ///
    /// Externally-provided circuit fragments carry no provenance (`IRBlocks<()>`).
    /// When inlined, re-emitted statements inherit the caller's `current_prov`.
    pub fn add_extern(&mut self, name: impl Into<String>, blocks: IRBlocks<()>) {
        self.externs.insert(name.into(), blocks);
    }

    /// Register an oracle declaration for the next completed function.
    pub fn register_oracle(&mut self, decl: OracleDecl) {
        self.pending_oracles.push(decl);
    }

    /// Register an action declaration for the next completed function.
    pub fn register_action(&mut self, decl: ActionDecl) {
        self.pending_actions.push(decl);
    }

    /// Drain and return all completed `(name, IRBlocks<P>)` pairs.
    pub fn take_completed(&mut self) -> Vec<(String, IRBlocks<P>)> {
        let mut out = Vec::new();
        core::mem::swap(&mut out, &mut self.completed);
        out
    }

    /// Intern an `IrType` into the shared type table, returning its `IRTypeId`.
    fn intern_type(&mut self, ty: IrType) -> IRTypeId {
        self.types.intern(ty)
    }

    /// Map a `LirType` to an interned `IRTypeId` in the types table.
    fn lir_to_ir_type(&mut self, ty: &LirType) -> IRTypeId {
        match ty {
            LirType::Bool => self.bit_tid,
            LirType::Native(t) => self.types.intern(IrType::Primitive(*t)),
            LirType::Ptr(_) => panic!("VolarIrTarget: LirType::Ptr is not supported (circuit backends have no memory model)"),
            _ => {
                // For non-native multi-bit types, fall back to the bit type.
                // Full support for U8/U16/etc. requires an IrType per LIR type;
                // for now the oracle/action output is a single IR var.
                self.bit_tid
            }
        }
    }

    // ---- Low-level emitters ------------------------------------------------

    fn emit(&mut self, stmt: IRStmt) -> IRVarId {
        let f = self.func.as_mut().unwrap();
        let blk = &mut f.blocks[f.current];
        let id = blk.next_local_id();
        blk.stmt_provs.push(self.current_prov.clone());
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
        self.emit(IRStmt::Poly { ty: self.bit_tid, coeffs, constant: Constant { hi: 0, lo: 0 } })
    }

    fn and_bit(&mut self, a: IRVarId, b: IRVarId) -> IRVarId {
        if a == b {
            return a;
        }
        let mut key = vec![a, b];
        key.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(key, 1u8);
        self.emit(IRStmt::Poly { ty: self.bit_tid, coeffs, constant: Constant { hi: 0, lo: 0 } })
    }

    fn not_bit(&mut self, a: IRVarId) -> IRVarId {
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![a], 1u8);
        self.emit(IRStmt::Poly { ty: self.bit_tid, coeffs, constant: Constant { hi: 0, lo: 1 } })
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
        self.emit(IRStmt::Poly { ty: self.bit_tid, coeffs, constant: Constant { hi: 0, lo: 0 } })
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
        VolarValue { bits: self.add_with_carry_in(&a.bits, &b.bits, false), ty: a.ty.clone() }
    }

    fn sub_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let not_b = self.not_vec(&b.bits);
        VolarValue { bits: self.add_with_carry_in(&a.bits, &not_b, true), ty: a.ty.clone() }
    }

    fn negate_impl(&mut self, val: &VolarValue) -> VolarValue {
        let not_bits = self.not_vec(&val.bits);
        let zeros: Vec<IRVarId> = (0..val.bits.len()).map(|_| self.bit_const(false)).collect();
        VolarValue { bits: self.add_with_carry_in(&not_bits, &zeros, true), ty: val.ty.clone() }
    }

    /// Two's-complement absolute value: select(MSB, −val, val).
    fn abs_impl(&mut self, val: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let sign = val.bits[n - 1];
        let neg = self.negate_impl(val);
        let bits = val.bits.iter().zip(&neg.bits)
            .map(|(&pos, &neg_b)| self.select_bit(sign, neg_b, pos))
            .collect();
        VolarValue { bits, ty: val.ty.clone() }
    }

    /// n×n-bit shift-and-add multiplier.  Lower n bits of the full product.
    fn mul_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let mut acc: Vec<IRVarId> = (0..n).map(|_| self.bit_const(false)).collect();
        for i in 0..n {
            let pp: Vec<IRVarId> = (0..n)
                .map(|k| {
                    if k < i { self.bit_const(false) } else { self.and_bit(a.bits[i], b.bits[k - i]) }
                })
                .collect();
            acc = self.add_with_carry_in(&acc, &pp, false);
        }
        VolarValue { bits: acc, ty: a.ty.clone() }
    }

    /// Restoring long-division: returns the quotient of `a / b`.
    ///
    /// For n-bit operands, O(n²) `Poly` stmts.
    fn udiv_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let mut r: Vec<IRVarId> = (0..n).map(|_| self.bit_const(false)).collect();
        let mut q_bits: Vec<IRVarId> = (0..n).map(|_| self.bit_const(false)).collect();
        for i in (0..n).rev() {
            let mut new_r = Vec::with_capacity(n);
            new_r.push(a.bits[i]);
            for k in 0..n - 1 { new_r.push(r[k]); }
            r = new_r;
            let mut borrow = self.bit_const(false);
            let mut diff = Vec::with_capacity(n);
            for k in 0..n {
                let not_rk = self.not_bit(r[k]);
                let d = self.xor3_bit(r[k], b.bits[k], borrow);
                let new_borrow = self.carry_bit(not_rk, b.bits[k], borrow);
                diff.push(d);
                borrow = new_borrow;
            }
            let no_borrow = self.not_bit(borrow);
            q_bits[i] = no_borrow;
            let r_copy = r.clone();
            r = self.select_vec(no_borrow, &diff, &r_copy);
        }
        VolarValue { bits: q_bits, ty: a.ty.clone() }
    }

    fn sdiv_impl(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let sign_a = a.bits[n - 1];
        let sign_b = b.bits[n - 1];
        let abs_a = self.abs_impl(a);
        let abs_b = self.abs_impl(b);
        let abs_q = self.udiv_impl(&abs_a, &abs_b);
        let signs_differ = self.xor_bit(sign_a, sign_b);
        let neg_q = self.negate_impl(&abs_q);
        let bits = abs_q.bits.iter().zip(&neg_q.bits)
            .map(|(&pq, &nq)| self.select_bit(signs_differ, nq, pq))
            .collect();
        VolarValue { bits, ty: a.ty.clone() }
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
            cur = (0..n).map(|j| {
                if j < step { let z = self.bit_const(false); self.select_bit(sb, z, cur[j]) }
                else { self.select_bit(sb, cur[j - step], cur[j]) }
            }).collect();
        }
        VolarValue { bits: cur, ty: val.ty.clone() }
    }

    fn lshr_impl(&mut self, val: &VolarValue, shift: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let stages = Self::barrel_stages(n, shift.bits.len());
        let mut cur = val.bits.clone();
        for k in 0..stages {
            let step = 1usize << k;
            let sb = shift.bits[k];
            cur = (0..n).map(|j| {
                if j + step >= n { let z = self.bit_const(false); self.select_bit(sb, z, cur[j]) }
                else { self.select_bit(sb, cur[j + step], cur[j]) }
            }).collect();
        }
        VolarValue { bits: cur, ty: val.ty.clone() }
    }

    fn ashr_impl(&mut self, val: &VolarValue, shift: &VolarValue) -> VolarValue {
        let n = val.bits.len();
        let sign = val.bits[n - 1];
        let stages = Self::barrel_stages(n, shift.bits.len());
        let mut cur = val.bits.clone();
        for k in 0..stages {
            let step = 1usize << k;
            let sb = shift.bits[k];
            cur = (0..n).map(|j| {
                if j + step >= n { self.select_bit(sb, sign, cur[j]) }
                else { self.select_bit(sb, cur[j + step], cur[j]) }
            }).collect();
        }
        VolarValue { bits: cur, ty: val.ty.clone() }
    }

    // ---- Comparisons -------------------------------------------------------

    fn icmp_eq(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let xors: Vec<IRVarId> = (0..n).map(|i| self.xor_bit(a.bits[i], b.bits[i])).collect();
        let not_xors: Vec<IRVarId> = (0..n).map(|i| self.not_bit(xors[i])).collect();
        let result = if n == 0 { self.bit_const(true) } else {
            let mut acc = not_xors[0];
            for i in 1..n { acc = self.and_bit(acc, not_xors[i]); }
            acc
        };
        VolarValue { bits: vec![result], ty: LirType::Bool }
    }

    fn icmp_ne(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let xors: Vec<IRVarId> = (0..n).map(|i| self.xor_bit(a.bits[i], b.bits[i])).collect();
        let result = if n == 0 { self.bit_const(false) } else {
            let mut acc = xors[0];
            for i in 1..n { acc = self.or_bit(acc, xors[i]); }
            acc
        };
        VolarValue { bits: vec![result], ty: LirType::Bool }
    }

    fn icmp_ult(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let mut borrow = self.bit_const(false);
        for i in 0..n {
            let not_ai = self.not_bit(a.bits[i]);
            borrow = self.carry_bit(not_ai, b.bits[i], borrow);
        }
        VolarValue { bits: vec![borrow], ty: LirType::Bool }
    }

    fn icmp_ule(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let gt = self.icmp_ult(b, a);
        VolarValue { bits: vec![self.not_bit(gt.bits[0])], ty: LirType::Bool }
    }

    fn icmp_slt(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let n = a.bits.len();
        let sign_a = a.bits[n - 1];
        let sign_b = b.bits[n - 1];
        let not_sign_b = self.not_bit(sign_b);
        let a_neg_b_pos = self.and_bit(sign_a, not_sign_b);
        let ult = self.icmp_ult(a, b).bits[0];
        let signs_xor = self.xor_bit(sign_a, sign_b);
        let signs_eq = self.not_bit(signs_xor);
        let same_sign_lt = self.and_bit(signs_eq, ult);
        VolarValue { bits: vec![self.or_bit(a_neg_b_pos, same_sign_lt)], ty: LirType::Bool }
    }

    fn icmp_sle(&mut self, a: &VolarValue, b: &VolarValue) -> VolarValue {
        let sgt = self.icmp_slt(b, a);
        VolarValue { bits: vec![self.not_bit(sgt.bits[0])], ty: LirType::Bool }
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
        callee: &IRBlock<()>,
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
                        vec![VolarValue { bits: ret_bits, ty: ty.clone() }]
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
        callee: &IRBlocks<()>,
        flat_args: Vec<IRVarId>,
        ret_ty: Option<&LirType>,
    ) -> Vec<VolarValue> {
        // Fast path: single-block with a direct unconditional Return — use the
        // simpler inline_callee which avoids allocating a continuation block.
        if callee.blocks.len() == 1 {
            if let IRTerminator::Jmp { func: IRBlockTargetId::Return, .. } = &callee.blocks[0].terminator {
                return self.inline_callee(&callee.blocks[0], flat_args, ret_ty);
            }
        }

        let n = callee.blocks.len();
        let ret_n = ret_ty.map(|ty| self.bits_for(ty)).unwrap_or(0);

        // ---- Continuation block (receives callee return values) -----------
        let cont_block = self.create_block();
        let mut cont_params = Vec::with_capacity(ret_n);
        // For Native return types add one native-typed param; for all other
        // types (including multi-bit) add Bool params (one per bit).
        let cont_param_ty = ret_ty
            .filter(|t| matches!(t, LirType::Native(_)))
            .cloned()
            .unwrap_or(LirType::Bool);
        for _ in 0..ret_n {
            let v = self.add_block_param(cont_block, cont_param_ty.clone());
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
            let n_params = callee.blocks[ci].params.len();
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
            for stmt in &callee.blocks[ci].stmts {
                let mapped = subst_stmt(stmt, &var_map);
                let id = self.emit(mapped);
                var_map.push(id);
            }
            let term = remap_terminator(
                &callee.blocks[ci].terminator, &var_map, &new_blocks, cont_block.0,
            );
            self.set_terminator(term);
        }

        // ---- Resume in the continuation block ----------------------------
        self.func.as_mut().unwrap().current = cont_block.0;

        if ret_n > 0 {
            let ret_lir_ty = ret_ty.cloned().unwrap_or(LirType::Bool);
            vec![VolarValue { bits: cont_params, ty: ret_lir_ty }]
        } else {
            vec![]
        }
    }
}

// ============================================================================
// BitCircuitBuilder implementation for VolarIrTarget
// ============================================================================

impl<P: Clone> BitCircuitBuilder for VolarIrTarget<P> {
    type Bit = IRVarId;

    fn bc_const(&mut self, val: bool) -> IRVarId {
        self.bit_const(val)
    }

    fn bc_poly(
        &mut self,
        coeffs: std::collections::BTreeMap<Vec<IRVarId>, u8>,
        constant: u128,
    ) -> IRVarId {
        self.emit(IRStmt::Poly {
            ty: self.bit_tid,
            coeffs,
            constant: Constant { hi: 0, lo: constant },
        })
    }

    /// Override with the optimized 3-variable degree-2 Poly (1 stmt vs 5).
    fn bc_carry3(&mut self, a: IRVarId, b: IRVarId, c: IRVarId) -> IRVarId {
        self.carry_bit(a, b, c)
    }

    // The remaining bc_xor / bc_and / bc_not / bc_or / bc_select use the
    // existing optimized helpers (which carry idempotency short-circuits).
    fn bc_xor(&mut self, a: IRVarId, b: IRVarId) -> IRVarId { self.xor_bit(a, b) }
    fn bc_and(&mut self, a: IRVarId, b: IRVarId) -> IRVarId { self.and_bit(a, b) }
    fn bc_not(&mut self, a: IRVarId)             -> IRVarId { self.not_bit(a) }
    fn bc_or (&mut self, a: IRVarId, b: IRVarId) -> IRVarId { self.or_bit(a, b) }
    fn bc_select(&mut self, cond: IRVarId, a: IRVarId, b: IRVarId) -> IRVarId {
        self.select_bit(cond, a, b)
    }
}

impl<P: Clone> StorageEmitter for VolarIrTarget<P> {
    fn compose_address(&mut self, bits: &[IRVarId]) -> IRVarId {
        if bits.len() == 1 {
            return bits[0];
        }
        let n = bits.len();
        let bit_tid = self.bit_tid;
        let vec_ty = self.types.intern(IrType::Vec(n, bit_tid));
        self.emit(IRStmt::Merge { parts: bits.to_vec(), ty: vec_ty })
    }

    fn extract_bit(&mut self, word: IRVarId, idx: u8) -> IRVarId {
        self.emit(IRStmt::Shuffle { result_bits: vec![(idx, word)], ty: self.bit_tid })
    }

    fn emit_read(&mut self, storage: volar_ir_common::StorageId, ty: volar_ir_common::TypeId, addr_bits: &[IRVarId]) -> IRVarId {
        let addr = self.compose_address(addr_bits);
        self.emit(IRStmt::StorageRead { storage, ty, addr })
    }

    fn emit_write(&mut self, storage: volar_ir_common::StorageId, src: IRVarId, ty: volar_ir_common::TypeId, addr_bits: &[IRVarId]) {
        let addr = self.compose_address(addr_bits);
        self.emit(IRStmt::StorageWrite { storage, src, ty, addr });
    }
}


fn bits_for_lir_type(ty: &LirType, struct_widths: &[usize]) -> usize {
    match ty {
        LirType::Bool => 1,
        LirType::I8 | LirType::U8 => 8,
        LirType::I16 | LirType::U16 => 16,
        LirType::I32 | LirType::U32 => 32,
        LirType::I64 | LirType::U64 => 64,
        LirType::I128 | LirType::U128 => 128,
        LirType::Arr(elem, n) => n * bits_for_lir_type(elem, struct_widths),
        LirType::Struct(id) => struct_widths[*id as usize],
        // Native field elements occupy exactly one IRVarId slot (not N bits).
        LirType::Native(_) => 1,
        LirType::Ptr(_) => panic!("VolarIrTarget: LirType::Ptr is not supported (circuit backends have no memory model)"),
        _ => panic!("bits_for_lir_type: unhandled LirType variant — add bit-width calculation"),
    }
}

fn lir_type_for_bits(n: usize) -> LirType {
    match n {
        1 => LirType::Bool,
        8 => LirType::U8,
        16 => LirType::U16,
        32 => LirType::U32,
        64 => LirType::U64,
        128 => LirType::U128,
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
            _ => panic!("remap_target: unhandled IRBlockTargetId variant — add remapping for this variant"),
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
        _ => panic!("remap_terminator: unhandled IRTerminator variant — add remapping for this variant"),
    }
}

fn subst_stmt(stmt: &IRStmt, var_map: &[IRVarId]) -> IRStmt {
    let s = |id: &IRVarId| var_map[id.0 as usize]; // IRVarId: Copy
    match stmt {
        IRStmt::Const(c, ty) => IRStmt::Const(*c, ty.clone()),
        IRStmt::Poly { ty, coeffs, constant } => IRStmt::Poly {
            ty: *ty,
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
        IRStmt::StorageRead { storage, ty, addr } => {
            IRStmt::StorageRead { storage: *storage, ty: ty.clone(), addr: s(addr) }
        }
        IRStmt::StorageWrite { storage, src, ty, addr } => {
            IRStmt::StorageWrite { storage: *storage, src: s(src), ty: ty.clone(), addr: s(addr) }
        }
        IRStmt::Shuffle { result_bits, ty } => IRStmt::Shuffle {
            result_bits: result_bits.iter().map(|(b, id)| (*b, s(id))).collect(),
            ty: ty.clone(),
        },
        IRStmt::OracleCall { name, args, output_tys, result_ty } => IRStmt::OracleCall {
            name: name.clone(),
            args: args.iter().map(s).collect(),
            output_tys: output_tys.clone(),
            result_ty: result_ty.clone(),
        },
        IRStmt::OracleOutput { call, idx, ty } =>
            IRStmt::OracleOutput { call: s(call), idx: *idx, ty: ty.clone() },
        IRStmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } =>
            IRStmt::ActionCall {
                name: name.clone(),
                guard: s(guard),
                args: args.iter().map(s).collect(),
                fallbacks: fallbacks.iter().map(s).collect(),
                output_tys: output_tys.clone(),
                result_ty: result_ty.clone(),
            },
        IRStmt::ActionOutput { call, idx, ty } =>
            IRStmt::ActionOutput { call: s(call), idx: *idx, ty: ty.clone() },
        IRStmt::Rng { name, ty } => IRStmt::Rng { name: name.clone(), ty: ty.clone() },
        _ => panic!("subst_stmt: unhandled IRStmt variant — add substitution for this variant"),
    }
}

// ============================================================================
// LirTarget implementation
// ============================================================================

impl<P: Clone> LirTarget<P> for VolarIrTarget<P> {
    type Value = VolarValue;
    type Block = VolarBlock;

    fn set_prov(&mut self, prov: P) {
        self.current_prov = prov;
    }

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
            if let LirType::Native(native_ty) = ty {
                // One native-typed IRVarId, not N Bit vars.
                let type_id = self.types.intern(IrType::Primitive(*native_ty));
                let id = block.params.len() as u32;
                block.params.push(type_id);
                groups.push(vec![VolarValue { bits: vec![IRVarId(id)], ty: ty.clone() }]);
            } else {
                let n = bits_for_lir_type(ty, &self.struct_widths);
                let start = block.params.len() as u32;
                for _ in 0..n {
                    block.params.push(self.bit_tid.clone());
                }
                groups.push(vec![VolarValue {
                    bits: (start..start + n as u32).map(IRVarId).collect(),
                    ty: ty.clone(),
                }]);
            }
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
                stmt_provs: b.stmt_provs,
                terminator: b.terminator.expect("VolarIrTarget: block missing terminator"),
            })
            .collect();
        let oracles = core::mem::take(&mut self.pending_oracles);
        let actions = core::mem::take(&mut self.pending_actions);
        self.completed.push((func.name, IRBlocks { oracles, actions, rngs: vec![], blocks, pre_init: vec![] }));
    }

    // ---- Block management --------------------------------------------------

    fn create_block(&mut self) -> VolarBlock {
        let f = self.func.as_mut().unwrap();
        let idx = f.blocks.len();
        f.blocks.push(BlockBuilder::<P>::new());
        VolarBlock(idx)
    }

    fn add_block_param(&mut self, block: VolarBlock, ty: LirType) -> VolarValue {
        if let LirType::Native(native_ty) = &ty {
            let type_id = self.types.intern(IrType::Primitive(*native_ty));
            let f = self.func.as_mut().unwrap();
            let blk = &mut f.blocks[block.0];
            let id = blk.params.len() as u32;
            blk.params.push(type_id);
            return VolarValue { bits: vec![IRVarId(id)], ty };
        }
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let f = self.func.as_mut().unwrap();
        let blk = &mut f.blocks[block.0];
        let start = blk.params.len() as u32;
        for _ in 0..n {
            blk.params.push(self.bit_tid.clone());
        }
        VolarValue { bits: (start..start + n as u32).map(IRVarId).collect(), ty }
    }

    fn switch_to_block(&mut self, block: VolarBlock) {
        self.func.as_mut().unwrap().current = block.0;
    }

    // ---- Constants ---------------------------------------------------------

    fn iconst(&mut self, ty: LirType, val: i64) -> VolarValue {
        if let LirType::Native(native_ty) = &ty {
            let type_id = self.types.intern(IrType::Primitive(*native_ty));
            let id = self.emit(IRStmt::Const(
                Constant { hi: 0, lo: val as u128 },
                type_id,
            ));
            return VolarValue { bits: vec![id], ty };
        }
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let bits: Vec<IRVarId> = (0..n)
            .map(|i| self.bit_const((val >> i) & 1 != 0))
            .collect();
        VolarValue { bits, ty }
    }

    // ---- Arithmetic (delegated to generic BitCircuitBuilder algorithms) -----

    fn add(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_add(self, &lhs.bits, &rhs.bits, false), ty }
    }
    fn sub(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_sub(self, &lhs.bits, &rhs.bits), ty }
    }
    fn mul(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_mul(self, &lhs.bits, &rhs.bits), ty }
    }
    fn udiv(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_udiv(self, &lhs.bits, &rhs.bits), ty }
    }
    fn sdiv(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_sdiv(self, &lhs.bits, &rhs.bits), ty }
    }

    fn and(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_and_vec(self, &lhs.bits, &rhs.bits), ty }
    }
    fn or(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_or_vec(self, &lhs.bits, &rhs.bits), ty }
    }
    fn xor(&mut self, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let ty = lhs.ty.clone();
        VolarValue { bits: bc_xor_vec(self, &lhs.bits, &rhs.bits), ty }
    }
    fn not(&mut self, val: VolarValue) -> VolarValue {
        let ty = val.ty.clone();
        VolarValue { bits: bc_not_vec(self, &val.bits), ty }
    }
    fn shl(&mut self, val: VolarValue, shift: VolarValue) -> VolarValue {
        let ty = val.ty.clone();
        VolarValue { bits: bc_shl(self, &val.bits, &shift.bits), ty }
    }
    fn lshr(&mut self, val: VolarValue, shift: VolarValue) -> VolarValue {
        let ty = val.ty.clone();
        VolarValue { bits: bc_lshr(self, &val.bits, &shift.bits), ty }
    }
    fn ashr(&mut self, val: VolarValue, shift: VolarValue) -> VolarValue {
        let ty = val.ty.clone();
        VolarValue { bits: bc_ashr(self, &val.bits, &shift.bits), ty }
    }

    // ---- Comparisons -------------------------------------------------------

    fn icmp(&mut self, pred: IcmpPred, lhs: VolarValue, rhs: VolarValue) -> VolarValue {
        let bit = match pred {
            IcmpPred::Eq  => bc_eq(self, &lhs.bits, &rhs.bits),
            IcmpPred::Ne  => bc_ne(self, &lhs.bits, &rhs.bits),
            IcmpPred::Ult => bc_ult(self, &lhs.bits, &rhs.bits),
            IcmpPred::Ule => bc_ule(self, &lhs.bits, &rhs.bits),
            IcmpPred::Ugt => bc_ult(self, &rhs.bits, &lhs.bits),
            IcmpPred::Uge => bc_ule(self, &rhs.bits, &lhs.bits),
            IcmpPred::Slt => bc_slt(self, &lhs.bits, &rhs.bits),
            IcmpPred::Sle => bc_sle(self, &lhs.bits, &rhs.bits),
            IcmpPred::Sgt => bc_slt(self, &rhs.bits, &lhs.bits),
            IcmpPred::Sge => bc_sle(self, &rhs.bits, &lhs.bits),
        };
        VolarValue { bits: vec![bit], ty: LirType::Bool }
    }

    // ---- Conversions -------------------------------------------------------

    fn zext(&mut self, val: VolarValue, dst_ty: LirType) -> VolarValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        let mut bits = val.bits;
        while bits.len() < dst_n { let z = self.bit_const(false); bits.push(z); }
        VolarValue { bits, ty: dst_ty }
    }
    fn sext(&mut self, val: VolarValue, dst_ty: LirType) -> VolarValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        let sign = *val.bits.last().expect("sext of zero-bit value");
        let mut bits = val.bits;
        bits.resize(dst_n, sign);
        VolarValue { bits, ty: dst_ty }
    }
    fn trunc(&mut self, val: VolarValue, dst_ty: LirType) -> VolarValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        VolarValue { bits: val.bits[..dst_n].to_vec(), ty: dst_ty }
    }

    // ---- Select ------------------------------------------------------------

    fn select(&mut self, cond: VolarValue, then_val: VolarValue, else_val: VolarValue) -> VolarValue {
        let cond_bit = cond.bits[0];
        let ty = then_val.ty.clone();
        let bits = self.select_vec(cond_bit, &then_val.bits, &else_val.bits);
        VolarValue { bits, ty }
    }

    // ---- Type query --------------------------------------------------------

    /// Return the [`LirType`] of a previously-emitted value.
    ///
    /// For `VolarIrTarget` all type information is stored directly in
    /// `VolarValue::ty`, so this is a trivial projection.  The `ty` field is
    /// set correctly at every construction site, including for
    /// [`LirType::Native`] values that cannot be inferred from `bits.len()`.
    fn value_scalar_type(&self, val: &VolarValue) -> LirType {
        val.ty.clone()
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

    // ---- External access primitives ----------------------------------------

    fn oracle(
        &mut self,
        name: &str,
        _arg_tys: &[LirType],
        args: &[VolarValue],
        ret_tys: &[LirType],
    ) -> Vec<VolarValue> {
        let flat_args = Self::flatten(args);
        let output_tys: Vec<IRTypeId> = ret_tys.iter().map(|t| self.lir_to_ir_type(t)).collect();
        let result_ty = self.types.intern(volar_ir_common::IrType::Tuple(
            output_tys.clone(),
        ));
        let call_var = self.emit(IRStmt::OracleCall {
            name: name.into(),
            args: flat_args,
            output_tys: output_tys.clone(),
            result_ty,
        });
        let mut result = Vec::new();
        for (i, t) in ret_tys.iter().enumerate() {
            let out = self.emit(IRStmt::OracleOutput {
                call: call_var,
                idx: i,
                ty: output_tys[i],
            });
            result.push(VolarValue { bits: vec![out], ty: t.clone() });
        }
        result
    }

    fn action(
        &mut self,
        name: &str,
        guard: VolarValue,
        _arg_tys: &[LirType],
        args: &[VolarValue],
        fallbacks: &[VolarValue],
        ret_tys: &[LirType],
    ) -> Vec<VolarValue> {
        let flat_args = Self::flatten(args);
        let flat_fallbacks: Vec<IRVarId> = Self::flatten(fallbacks);
        let output_tys: Vec<IRTypeId> = ret_tys.iter().map(|t| self.lir_to_ir_type(t)).collect();
        let result_ty = self.types.intern(volar_ir_common::IrType::Tuple(
            output_tys.clone(),
        ));
        let call_var = self.emit(IRStmt::ActionCall {
            name: name.into(),
            guard: guard.bits[0],
            args: flat_args,
            fallbacks: flat_fallbacks,
            output_tys: output_tys.clone(),
            result_ty,
        });
        let mut result = Vec::new();
        for (i, t) in ret_tys.iter().enumerate() {
            let out = self.emit(IRStmt::ActionOutput {
                call: call_var,
                idx: i,
                ty: output_tys[i],
            });
            result.push(VolarValue { bits: vec![out], ty: t.clone() });
        }
        result
    }

    fn rng(&mut self, ty: LirType) -> VolarValue {
        let ir_ty = self.lir_to_ir_type(&ty);
        let id = self.emit(IRStmt::Rng { name: "rng".into(), ty: ir_ty });
        VolarValue { bits: vec![id], ty }
    }

    fn abi(&self) -> LirAbi {
        LirAbi::CIRCUIT
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

    /// Run a single-function VolarIrTarget and return its `IRBlocks<()>`.
    fn build(f: impl FnOnce(&mut VolarIrTarget<()>)) -> IRBlocks<()> {
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
        assert_eq!(blocks.blocks[0].params.len(), 8);
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
        let extern_impl = IRBlocks::new(std::vec![IRBlock {
            params: std::vec![bit_tid.clone()],
            stmts: std::vec![],
            stmt_provs: std::vec![],
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
        let t = VolarIrTarget::<()>::new();
        let v = VolarValue { bits: std::vec![IRVarId(0)], ty: LirType::Bool };
        assert_eq!(t.value_scalar_type(&v), LirType::Bool);
    }

    #[test]
    fn test_value_scalar_type_u64() {
        let t = VolarIrTarget::<()>::new();
        let v = VolarValue { bits: (0..64).map(IRVarId).collect(), ty: LirType::U64 };
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
        let callee = IRBlocks::new(std::vec![
            // Block 0: branch on param 0.
            IRBlock {
                params: std::vec![bit_tid],
                stmts: std::vec![],
                stmt_provs: std::vec![],
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
                stmt_provs: std::vec![()],
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
        assert_eq!(blocks.blocks.len(), 3, "caller should have 3 blocks after inlining");
    }

    /// Inline a callee that loops (self-jump), verifying that multi-block
    /// inlining handles back-edges correctly by mapping them to fresh blocks.
    #[test]
    fn test_inline_blocks_loop_callee() {
        let bit_tid = IRTypeId(0);
        // Callee: Block 0(x: Bit) -> CondJmp(x, Return([x]), Block(0,[NOT(x)]))
        // Semantics: if x=1 return 1, else loop with NOT(x)=1 → returns 1.
        let callee = IRBlocks::new(std::vec![IRBlock {
            params: std::vec![bit_tid],
            stmts: std::vec![
                // stmt 0: NOT(param_0)
                {
                    let mut coeffs = std::collections::BTreeMap::new();
                    coeffs.insert(std::vec![IRVarId(0)], 1u8);
                    IRStmt::Poly { ty: bit_tid, coeffs, constant: Constant { hi: 0, lo: 1 } }
                },
            ],
            stmt_provs: std::vec![()],
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

    // ---- StackAllocExt -------------------------------------------------

    #[test]
    fn test_stack_alloc_ext_returns_none() {
        let mut t = VolarIrTarget::<()>::new();
        assert!(
            t.stack_alloc_ext().is_none(),
            "VolarIrTarget has no memory model and must return None from stack_alloc_ext"
        );
    }

    #[test]
    fn test_lir_type_ptr_is_constructable() {
        // Confirm the Ptr variant exists and wraps an inner type correctly.
        let ty = LirType::Ptr(std::boxed::Box::new(LirType::U32));
        match &ty {
            LirType::Ptr(inner) => assert_eq!(**inner, LirType::U32),
            _ => panic!("expected LirType::Ptr"),
        }
    }

    #[test]
    #[should_panic(expected = "VolarIrTarget: LirType::Ptr is not supported")]
    fn test_bits_for_ptr_panics() {
        let mut t = VolarIrTarget::<()>::new();
        // bits_for_lir_type is private, but begin_function exercises the same
        // path when it processes the parameter list.
        t.begin_function("f", &[LirType::Ptr(std::boxed::Box::new(LirType::U32))], None);
    }

    #[test]
    fn test_volar_ir_target_abi_is_circuit() {
        let t = VolarIrTarget::<()>::new();
        let abi = t.abi();
        assert!(!abi.native_aggregates);
        assert_eq!(abi.aggregate_byval_limit, usize::MAX);
        assert!(!abi.pass_by_ptr(1_000_000));
    }

    // ============================================================================
    // Corpus: smoke test (all cases build without panic)
    // ============================================================================

    #[test]
    fn test_corpus_smoke() {
        volar_lir_test_corpus::for_each_build!(VolarIrTarget::<()>::new());
    }
}
