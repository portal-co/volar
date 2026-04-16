// @reliability: experimental
// @ai: assisted
//! `VaffleTarget` — a [`LirTarget`] + [`BitCircuitBuilder`] that emits VAFFLE IR.
//!
//! Shares every arithmetic circuit with `VolarIrTarget` through the generic
//! `bc_*` functions in [`volar_lir::circuits`].  Both emit `Stmt::Poly` for
//! GF(2) bit operations; the only difference is the variable type
//! (`IRVarId` vs `ValueId`).

use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use volar_ir_common::{
    ActionDecl, Constant, IrType, OracleDecl, Stmt, Type, TypeId, TypeTable,
};
use volar_lir::{
    circuits::{
        bc_abs, bc_add, bc_and_vec, bc_ashr, bc_eq, bc_lshr, bc_mul, bc_ne, bc_neg,
        bc_not_vec, bc_or_vec, bc_sdiv, bc_select_vec, bc_shl, bc_sle, bc_slt,
        bc_sub, bc_udiv, bc_ule, bc_ult, bc_xor_vec, StorageEmitter,
    },
    BitCircuitBuilder, IcmpPred, LirTarget, LirType, StructDef, StructId,
};

use vaffle::{
    Block, BlockId, FuncBody, FuncDecl, FuncId, Module, SigDecl, SigId,
    Target, Terminator, Value, ValueId,
};

// ============================================================================
// Public types
// ============================================================================

/// A VAFFLE SSA value: a flat bit decomposition of one LIR integer.
///
/// Mirrors `VolarValue` from `volar-ir-lir-target`: each bit of an N-bit
/// integer is a separate `ValueId` pointing to a `Value::Op(Stmt::Poly{…})`
/// or `Value::Op(Stmt::Const{…})`, LSB first.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VaffleValue {
    /// One `ValueId` per bit, LSB first.
    pub bits: Vec<ValueId>,
    /// LIR type of this value group.
    pub ty: LirType,
}

/// Handle to a VAFFLE basic block.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VaffleBlock(pub usize);

// ============================================================================
// Internal builders
// ============================================================================

struct BlockBuilder {
    params: Vec<(ValueId, TypeId)>,
    stmts: Vec<ValueId>,
    terminator: Option<Terminator>,
}

struct FuncBuilder {
    name: String,
    sig_id: SigId,
    blocks: Vec<BlockBuilder>,
    current: usize,
    all_values: Vec<Value>,
    bit_tid: TypeId,
}

impl FuncBuilder {
    fn new(name: String, sig_id: SigId, bit_tid: TypeId) -> Self {
        FuncBuilder {
            name,
            sig_id,
            blocks: vec![BlockBuilder { params: vec![], stmts: vec![], terminator: None }],
            current: 0,
            all_values: vec![],
            bit_tid,
        }
    }

    fn next_value_id(&self) -> ValueId {
        ValueId(self.all_values.len())
    }

    fn emit_value(&mut self, val: Value) -> ValueId {
        let id = self.next_value_id();
        self.all_values.push(val);
        self.blocks[self.current].stmts.push(id);
        id
    }

    fn emit_block_param(&mut self, block_idx: usize, ty: TypeId) -> ValueId {
        let idx = self.blocks[block_idx].params.len();
        let id = self.next_value_id();
        self.all_values.push(Value::Param { block: BlockId(block_idx), ty, idx });
        self.blocks[block_idx].params.push((id, ty));
        id
    }
}

// ============================================================================
// VaffleTarget
// ============================================================================

/// A [`LirTarget`] that assembles a VAFFLE [`Module`].
pub struct VaffleTarget {
    pub module: Module,
    func: Option<FuncBuilder>,
    struct_widths: Vec<usize>,
}

impl VaffleTarget {
    pub fn new() -> Self {
        let mut types = TypeTable::new();
        types.intern(IrType::Primitive(Type::Bit)); // bit_tid always at index 0
        VaffleTarget {
            module: Module {
                types,
                oracles: vec![],
                actions: vec![],
                funcs: vec![],
                sigs: vec![],
                exports: BTreeMap::new(),
            },
            func: None,
            struct_widths: vec![],
        }
    }

    fn bit_tid(&mut self) -> TypeId { self.module.types.bit() }

    fn intern_type(&mut self, ty: IrType) -> TypeId { self.module.types.intern(ty) }

    fn bits_for(&self, ty: &LirType) -> usize {
        bits_for_lir_type(ty, &self.struct_widths)
    }

    fn fb(&mut self) -> &mut FuncBuilder {
        self.func.as_mut().expect("VaffleTarget: no function in progress")
    }

    fn emit_const(&mut self, val: u128, ty: TypeId) -> ValueId {
        let v = Value::Op(Stmt::Const(Constant { hi: 0, lo: val }, ty));
        self.fb().emit_value(v)
    }

    pub fn register_oracle(&mut self, decl: OracleDecl) { self.module.oracles.push(decl); }
    pub fn register_action(&mut self, decl: ActionDecl) { self.module.actions.push(decl); }
}

impl Default for VaffleTarget {
    fn default() -> Self { Self::new() }
}

// ============================================================================
// BitCircuitBuilder
// ============================================================================

impl BitCircuitBuilder for VaffleTarget {
    type Bit = ValueId;

    fn bc_const(&mut self, val: bool) -> ValueId {
        let ty = self.bit_tid();
        self.emit_const(val as u128, ty)
    }

    fn bc_poly(&mut self, coeffs: BTreeMap<Vec<ValueId>, u8>, constant: u128) -> ValueId {
        let v = Value::Op(Stmt::Poly {
            coeffs,
            constant: Constant { hi: 0, lo: constant },
        });
        self.fb().emit_value(v)
    }

    // Optimized single-bit ops matching VolarIrTarget's helpers.
    fn bc_xor(&mut self, a: ValueId, b: ValueId) -> ValueId {
        if a == b { return self.bc_const(false); }
        let mut c = BTreeMap::new();
        c.insert(vec![a], 1u8);
        c.insert(vec![b], 1u8);
        self.bc_poly(c, 0)
    }
    fn bc_and(&mut self, a: ValueId, b: ValueId) -> ValueId {
        if a == b { return a; }
        let mut key = vec![a, b]; key.sort();
        let mut c = BTreeMap::new(); c.insert(key, 1u8);
        self.bc_poly(c, 0)
    }
    fn bc_not(&mut self, a: ValueId) -> ValueId {
        let mut c = BTreeMap::new(); c.insert(vec![a], 1u8);
        self.bc_poly(c, 1)
    }
    fn bc_or(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let na = self.bc_not(a); let nb = self.bc_not(b);
        let nand = self.bc_and(na, nb); self.bc_not(nand)
    }
    fn bc_select(&mut self, cond: ValueId, a: ValueId, b: ValueId) -> ValueId {
        let xab = self.bc_xor(a, b); let sel = self.bc_and(cond, xab); self.bc_xor(sel, b)
    }
    /// Majority via single degree-2 Poly (identical to VolarIrTarget::carry_bit).
    fn bc_carry3(&mut self, a: ValueId, b: ValueId, c: ValueId) -> ValueId {
        let mut ab = vec![a, b]; ab.sort();
        let mut ac = vec![a, c]; ac.sort();
        let mut bc_ = vec![b, c]; bc_.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(ab, 1u8); coeffs.insert(ac, 1u8); coeffs.insert(bc_, 1u8);
        self.bc_poly(coeffs, 0)
    }
}

impl StorageEmitter for VaffleTarget {
    fn compose_address(&mut self, bits: &[ValueId]) -> ValueId {
        if bits.len() == 1 {
            return bits[0];
        }
        let n = bits.len();
        let bit_tid = self.bit_tid();
        let vec_ty = self.intern_type(IrType::Vec(n, bit_tid));
        let v = Value::Op(Stmt::Merge { parts: bits.to_vec(), ty: vec_ty });
        self.fb().emit_value(v)
    }

    fn emit_read(&mut self, storage: volar_ir_common::StorageId, ty: volar_ir_common::TypeId, addr_bits: &[ValueId]) -> ValueId {
        let addr = self.compose_address(addr_bits);
        let v = Value::Op(Stmt::StorageRead { storage, ty, addr });
        self.fb().emit_value(v)
    }

    fn emit_write(&mut self, storage: volar_ir_common::StorageId, src: ValueId, ty: volar_ir_common::TypeId, addr_bits: &[ValueId]) {
        let addr = self.compose_address(addr_bits);
        let v = Value::Op(Stmt::StorageWrite { storage, src, ty, addr });
        self.fb().emit_value(v);
    }
}

// ============================================================================
// LirTarget
// ============================================================================

impl LirTarget for VaffleTarget {
    type Value = VaffleValue;
    type Block = VaffleBlock;

    fn define_struct(&mut self, def: StructDef) -> StructId {
        let id = self.struct_widths.len() as StructId;
        let total: usize = def.fields.iter()
            .map(|f| bits_for_lir_type(&f.ty, &self.struct_widths)).sum();
        self.struct_widths.push(total);
        id
    }

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        _ret: Option<LirType>,
    ) -> (VaffleBlock, Vec<Vec<VaffleValue>>) {
        let bit_tid = self.bit_tid();
        let sig_params: Vec<TypeId> = params.iter()
            .flat_map(|ty| {
                let n = bits_for_lir_type(ty, &self.struct_widths);
                (0..n).map(|_| bit_tid)
            })
            .collect();
        let sig_id = SigId(self.module.sigs.len());
        self.module.sigs.push(SigDecl { params: sig_params, results: vec![] });

        let mut fb = FuncBuilder::new(name.to_string(), sig_id, bit_tid);
        let mut groups: Vec<Vec<VaffleValue>> = Vec::new();
        for ty in params {
            let n = bits_for_lir_type(ty, &self.struct_widths);
            let bits: Vec<ValueId> = (0..n).map(|_| fb.emit_block_param(0, bit_tid)).collect();
            groups.push(vec![VaffleValue { bits, ty: ty.clone() }]);
        }
        self.func = Some(fb);
        (VaffleBlock(0), groups)
    }

    fn end_function(&mut self) {
        let fb = self.func.take().expect("VaffleTarget: no function in progress");
        let blocks: Vec<Block> = fb.blocks.into_iter().map(|bb| Block {
            params: bb.params,
            stmts: bb.stmts,
            terminator: bb.terminator.unwrap_or(Terminator::Return { values: vec![] }),
        }).collect();
        let body = FuncBody { sig: fb.sig_id, blocks, values: fb.all_values, entry: BlockId(0) };
        let func_id = FuncId(self.module.funcs.len());
        self.module.funcs.push(FuncDecl::Body(body));
        self.module.exports.insert(fb.name, func_id);
    }

    fn create_block(&mut self) -> VaffleBlock {
        let fb = self.fb();
        let idx = fb.blocks.len();
        fb.blocks.push(BlockBuilder { params: vec![], stmts: vec![], terminator: None });
        VaffleBlock(idx)
    }

    fn add_block_param(&mut self, block: VaffleBlock, ty: LirType) -> VaffleValue {
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let bit_tid = self.bit_tid();
        let bits: Vec<ValueId> = (0..n).map(|_| self.fb().emit_block_param(block.0, bit_tid)).collect();
        VaffleValue { bits, ty }
    }

    fn switch_to_block(&mut self, block: VaffleBlock) { self.fb().current = block.0; }

    fn iconst(&mut self, ty: LirType, val: i64) -> VaffleValue {
        if let LirType::Native(t) = &ty {
            let type_id = self.intern_type(IrType::Primitive(*t));
            let id = self.emit_const(val as u128, type_id);
            return VaffleValue { bits: vec![id], ty };
        }
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let bit_tid = self.bit_tid();
        let bits: Vec<ValueId> = (0..n)
            .map(|i| self.emit_const(((val >> i) & 1) as u128, bit_tid))
            .collect();
        VaffleValue { bits, ty }
    }

    // ---- Arithmetic --------------------------------------------------------
    fn add(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_add(self, &lhs.bits, &rhs.bits, false), ty }
    }
    fn sub(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_sub(self, &lhs.bits, &rhs.bits), ty }
    }
    fn mul(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_mul(self, &lhs.bits, &rhs.bits), ty }
    }
    fn udiv(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_udiv(self, &lhs.bits, &rhs.bits), ty }
    }
    fn sdiv(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_sdiv(self, &lhs.bits, &rhs.bits), ty }
    }
    fn and(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_and_vec(self, &lhs.bits, &rhs.bits), ty }
    }
    fn or(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_or_vec(self, &lhs.bits, &rhs.bits), ty }
    }
    fn xor(&mut self, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let ty = lhs.ty.clone();
        VaffleValue { bits: bc_xor_vec(self, &lhs.bits, &rhs.bits), ty }
    }
    fn not(&mut self, val: VaffleValue) -> VaffleValue {
        let ty = val.ty.clone();
        VaffleValue { bits: bc_not_vec(self, &val.bits), ty }
    }
    fn shl(&mut self, val: VaffleValue, shift: VaffleValue) -> VaffleValue {
        let ty = val.ty.clone();
        VaffleValue { bits: bc_shl(self, &val.bits, &shift.bits), ty }
    }
    fn lshr(&mut self, val: VaffleValue, shift: VaffleValue) -> VaffleValue {
        let ty = val.ty.clone();
        VaffleValue { bits: bc_lshr(self, &val.bits, &shift.bits), ty }
    }
    fn ashr(&mut self, val: VaffleValue, shift: VaffleValue) -> VaffleValue {
        let ty = val.ty.clone();
        VaffleValue { bits: bc_ashr(self, &val.bits, &shift.bits), ty }
    }

    // ---- Comparisons -------------------------------------------------------
    fn icmp(&mut self, pred: IcmpPred, lhs: VaffleValue, rhs: VaffleValue) -> VaffleValue {
        let bit = match pred {
            IcmpPred::Eq  => bc_eq(self,  &lhs.bits, &rhs.bits),
            IcmpPred::Ne  => bc_ne(self,  &lhs.bits, &rhs.bits),
            IcmpPred::Ult => bc_ult(self, &lhs.bits, &rhs.bits),
            IcmpPred::Ule => bc_ule(self, &lhs.bits, &rhs.bits),
            IcmpPred::Ugt => bc_ult(self, &rhs.bits, &lhs.bits),
            IcmpPred::Uge => bc_ule(self, &rhs.bits, &lhs.bits),
            IcmpPred::Slt => bc_slt(self, &lhs.bits, &rhs.bits),
            IcmpPred::Sle => bc_sle(self, &lhs.bits, &rhs.bits),
            IcmpPred::Sgt => bc_slt(self, &rhs.bits, &lhs.bits),
            IcmpPred::Sge => bc_sle(self, &rhs.bits, &lhs.bits),
        };
        VaffleValue { bits: vec![bit], ty: LirType::Bool }
    }

    // ---- Conversions -------------------------------------------------------
    fn zext(&mut self, val: VaffleValue, dst_ty: LirType) -> VaffleValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        let mut bits = val.bits;
        while bits.len() < dst_n { bits.push(self.bc_const(false)); }
        VaffleValue { bits, ty: dst_ty }
    }
    fn sext(&mut self, val: VaffleValue, dst_ty: LirType) -> VaffleValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        let sign = *val.bits.last().expect("sext of empty value");
        let mut bits = val.bits;
        bits.resize(dst_n, sign);
        VaffleValue { bits, ty: dst_ty }
    }
    fn trunc(&mut self, val: VaffleValue, dst_ty: LirType) -> VaffleValue {
        let dst_n = bits_for_lir_type(&dst_ty, &self.struct_widths);
        VaffleValue { bits: val.bits[..dst_n].to_vec(), ty: dst_ty }
    }

    fn select(&mut self, cond: VaffleValue, then_val: VaffleValue, else_val: VaffleValue) -> VaffleValue {
        let ty = then_val.ty.clone();
        let cond_bit = cond.bits[0];
        let bits = bc_select_vec(self, cond_bit, &then_val.bits, &else_val.bits);
        VaffleValue { bits, ty }
    }

    fn value_scalar_type(&self, val: &VaffleValue) -> LirType { val.ty.clone() }

    // ---- Extern calls ------------------------------------------------------
    fn call_extern(
        &mut self,
        name: &str,
        _arg_tys: &[LirType],
        args: &[VaffleValue],
        ret_ty: Option<LirType>,
    ) -> Vec<VaffleValue> {
        let func_id = if let Some(&fid) = self.module.exports.get(name) {
            fid
        } else {
            let fid = FuncId(self.module.funcs.len());
            let sig_id = SigId(self.module.sigs.len());
            self.module.sigs.push(SigDecl { params: vec![], results: vec![] });
            self.module.funcs.push(FuncDecl::Import {
                module: "env".to_string(),
                name: name.to_string(),
                sig: sig_id,
            });
            fid
        };
        let flat_args: Vec<ValueId> = args.iter().flat_map(|v| v.bits.iter().copied()).collect();
        let call_id = self.fb().emit_value(Value::Call { func: func_id, args: flat_args });
        match ret_ty {
            None => vec![],
            Some(ty) => {
                let n = bits_for_lir_type(&ty, &self.struct_widths);
                let bits: Vec<ValueId> = (0..n)
                    .map(|i| self.fb().emit_value(Value::Output { value: call_id, idx: i }))
                    .collect();
                vec![VaffleValue { bits, ty }]
            }
        }
    }

    // ---- External access primitives ----------------------------------------
    fn oracle(&mut self, name: &str, arg_tys: &[LirType], args: &[VaffleValue], ret_tys: &[LirType]) -> Vec<VaffleValue> {
        self.call_extern(&alloc::format!("oracle_{name}"), arg_tys, args, ret_tys.first().cloned())
    }

    fn action(&mut self, name: &str, guard: VaffleValue, arg_tys: &[LirType], args: &[VaffleValue], fallbacks: &[VaffleValue], ret_tys: &[LirType]) -> Vec<VaffleValue> {
        let ret_ty = ret_tys.first().cloned();
        let results = self.call_extern(&alloc::format!("action_{name}"), arg_tys, args, ret_ty.clone());
        let guard_bit = guard.bits[0];
        results.into_iter().zip(fallbacks.iter()).map(|(r, fb)| {
            let ty = r.ty.clone();
            let bits = bc_select_vec(self, guard_bit, &r.bits, &fb.bits);
            VaffleValue { bits, ty }
        }).collect()
    }

    fn rng(&mut self, ty: LirType) -> VaffleValue {
        self.call_extern("volar_rng", &[], &[], Some(ty))
            .into_iter().next()
            .unwrap_or(VaffleValue { bits: vec![], ty: LirType::Bool })
    }

    // ---- Terminators -------------------------------------------------------
    fn jump(&mut self, target: VaffleBlock, args: &[VaffleValue]) {
        let flat: Vec<ValueId> = args.iter().flat_map(|v| v.bits.iter().copied()).collect();
        let fb = self.fb();
        let cur = fb.current;
        fb.blocks[cur].terminator = Some(Terminator::Jump(Target { block: BlockId(target.0), args: flat }));
    }

    fn branch(&mut self, cond: VaffleValue, then_block: VaffleBlock, then_args: &[VaffleValue], else_block: VaffleBlock, else_args: &[VaffleValue]) {
        let cond_bit = cond.bits[0];
        let flat_then: Vec<ValueId> = then_args.iter().flat_map(|v| v.bits.iter().copied()).collect();
        let flat_else: Vec<ValueId> = else_args.iter().flat_map(|v| v.bits.iter().copied()).collect();
        let fb = self.fb();
        let cur = fb.current;
        fb.blocks[cur].terminator = Some(Terminator::IfNonzero {
            cond: cond_bit,
            then_target: Target { block: BlockId(then_block.0), args: flat_then },
            else_target: Target { block: BlockId(else_block.0), args: flat_else },
        });
    }

    fn ret(&mut self, vals: &[VaffleValue]) {
        let flat: Vec<ValueId> = vals.iter().flat_map(|v| v.bits.iter().copied()).collect();
        let fb = self.fb();
        let cur = fb.current;
        fb.blocks[cur].terminator = Some(Terminator::Return { values: flat });
    }
}

// ============================================================================
// Helpers
// ============================================================================

pub(crate) fn bits_for_lir_type(ty: &LirType, struct_widths: &[usize]) -> usize {
    match ty {
        LirType::Bool => 1,
        LirType::I8 | LirType::U8 => 8,
        LirType::I16 | LirType::U16 => 16,
        LirType::I32 | LirType::U32 => 32,
        LirType::I64 | LirType::U64 => 64,
        LirType::Arr(elem, n) => n * bits_for_lir_type(elem, struct_widths),
        LirType::Struct(id) => struct_widths[*id as usize],
        LirType::Native(_) => 1,
    }
}
