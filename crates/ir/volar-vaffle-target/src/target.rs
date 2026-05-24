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
    ActionDecl, Constant, IrType, OracleDecl, Stmt, StorageId, Type, TypeId, TypeTable,
};
use volar_lir::{
    circuits::{
        bc_abs, bc_add, bc_and_vec, bc_ashr, bc_eq, bc_lshr, bc_mul, bc_ne, bc_neg,
        bc_not_vec, bc_or_vec, bc_sdiv, bc_select_vec, bc_shl, bc_sle, bc_slt,
        bc_sub, bc_udiv, bc_ule, bc_ult, bc_xor_vec, StorageEmitter,
    },
    BitCircuitBuilder, IcmpPred, LirTarget, LirType, LirAbi, StackAllocExt, StructDef, StructId,
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

pub(crate) struct FuncBuilder {
    name: String,
    sig_id: SigId,
    blocks: Vec<BlockBuilder>,
    current: usize,
    all_values: Vec<Value>,
    bit_tid: TypeId,
    /// Next free stack-storage slot for `StackAlloc` within this function.
    next_stack_slot: u64,
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
            next_stack_slot: 0,
        }
    }

    fn next_value_id(&self) -> ValueId {
        ValueId(self.all_values.len())
    }

    pub(crate) fn emit_value(&mut self, val: Value) -> ValueId {
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
///
/// # ABI modes
///
/// By default, all parameters are passed as flat bit vectors (`LirAbi::CIRCUIT`).
/// Call [`with_optimized_abi`](Self::with_optimized_abi) to enable stack-based
/// passing for parameters whose bit-count exceeds 64.  This reduces
/// block-parameter counts and spill/reload overhead in `lower_to_ir`, at the
/// cost of extra `StorageRead`/`StorageWrite` operations.
pub struct VaffleTarget {
    pub module: Module,
    func: Option<FuncBuilder>,
    struct_widths: Vec<usize>,
    /// When `true`, parameters wider than `LirAbi::VAFFLE_OPTIMIZED.aggregate_byval_limit`
    /// are passed via `StorageId::STACK` (caller writes bits, passes address;
    /// callee reads bits from address).  Default: `false`.
    optimized_abi: bool,
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
            optimized_abi: false,
        }
    }

    /// Enable the optimized stack-based ABI for large parameters.
    ///
    /// When enabled, parameters whose bit-decomposition exceeds 64 bits are
    /// passed via `StorageId::STACK` instead of as individual block parameters.
    /// This reduces the block-parameter count (and therefore spill/reload cost
    /// in `lower_to_ir`) for functions that accept AES-sized or larger values.
    ///
    /// # Compatibility
    ///
    /// Callers and callees **must** use the same ABI setting.  Mixing
    /// `optimized_abi = true` callers with `optimized_abi = false` callees
    /// (or vice versa) produces incorrect code.
    pub fn with_optimized_abi(mut self) -> Self {
        self.optimized_abi = true;
        self
    }

    /// Query whether the optimized ABI is enabled.
    pub fn optimized_abi(&self) -> bool {
        self.optimized_abi
    }

    /// Set the ABI mode at runtime.
    pub fn set_optimized_abi(&mut self, enabled: bool) {
        self.optimized_abi = enabled;
    }

    pub(crate) fn bit_tid(&mut self) -> TypeId { self.module.types.bit() }

    /// Intern (or retrieve) the byte type `Vec(8, Bit)` used for memory storage cells.
    pub(crate) fn byte_tid(&mut self) -> TypeId {
        let bit = self.bit_tid();
        self.intern_type(IrType::Vec(8, bit))
    }

    fn intern_type(&mut self, ty: IrType) -> TypeId { self.module.types.intern(ty) }

    fn bits_for(&self, ty: &LirType) -> usize {
        bits_for_lir_type(ty, &self.struct_widths)
    }

    pub(crate) fn fb(&mut self) -> &mut FuncBuilder {
        self.func.as_mut().expect("VaffleTarget: no function in progress")
    }

    fn emit_const(&mut self, val: u128, ty: TypeId) -> ValueId {
        let v = Value::Op(Stmt::Const(Constant { hi: 0, lo: val }, ty));
        self.fb().emit_value(v)
    }

    pub fn register_oracle(&mut self, decl: OracleDecl) { self.module.oracles.push(decl); }
    pub fn register_action(&mut self, decl: ActionDecl) { self.module.actions.push(decl); }

    /// Emit an action call: calls `action_{name}`, then muxes each result with
    /// its fallback — `guard_bit=1` uses the result, `guard_bit=0` uses the fallback.
    pub fn action_call(
        &mut self,
        name: &str,
        guard_bit: ValueId,
        args: &[VaffleValue],
        fallbacks: &[VaffleValue],
        ret_tys: &[LirType],
    ) -> Vec<VaffleValue> {
        let results = self.call_extern_multi(
            &alloc::format!("action_{name}"),
            args,
            ret_tys,
        );
        results
            .into_iter()
            .zip(fallbacks.iter())
            .map(|(r, fb)| {
                let ty = r.ty.clone();
                let bits = bc_select_vec(self, guard_bit, &r.bits, &fb.bits);
                VaffleValue { bits, ty }
            })
            .collect()
    }

    /// Convert a `LirType` to a `TypeId` in the module's type table.
    pub(crate) fn lir_type_to_tid(&mut self, ty: &LirType) -> TypeId {
        match ty {
            LirType::Bool => self.bit_tid(),
            LirType::I8 | LirType::U8  => self.intern_type(IrType::Primitive(Type::_8)),
            LirType::I16 | LirType::U16 => self.intern_type(IrType::Primitive(Type::_16)),
            LirType::I32 | LirType::U32 => self.intern_type(IrType::Primitive(Type::_32)),
            LirType::I64 | LirType::U64 => self.intern_type(IrType::Primitive(Type::_64)),
            LirType::Native(t) => self.intern_type(IrType::Primitive(*t)),
            LirType::Arr(elem, n) => {
                let elem_tid = self.lir_type_to_tid(elem);
                self.intern_type(IrType::Vec(*n, elem_tid))
            }
            _ => self.bit_tid(), // fallback for Struct/Ptr
        }
    }

    /// Pad or truncate a `VaffleValue` to exactly `PTR_BITS` bits.
    pub(crate) fn pad_to_ptr_bits(&mut self, val: VaffleValue) -> Vec<ValueId> {
        let mut bits = val.bits;
        while bits.len() < PTR_BITS {
            bits.push(self.bc_const(false));
        }
        bits.truncate(PTR_BITS);
        bits
    }
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
        let bit_tid = self.bit_tid();
        let v = Value::Op(Stmt::Poly {
            ty: bit_tid,
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

    fn extract_bit(&mut self, word: ValueId, idx: u8) -> ValueId {
        let bit_tid = self.bit_tid();
        let v = Value::Op(Stmt::Shuffle { result_bits: vec![(idx, word)], ty: bit_tid });
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
        let threshold = self.abi().aggregate_byval_limit;

        // Decide per-param: direct (N block params) or ptr (PTR_BITS block params).
        let param_infos: Vec<(usize, bool)> = params.iter()
            .map(|ty| {
                let n = bits_for_lir_type(ty, &self.struct_widths);
                (n, self.optimized_abi && n > threshold)
            })
            .collect();

        // Build signature: direct params contribute N Bit slots,
        // ptr params contribute PTR_BITS Bit slots.
        let sig_params: Vec<TypeId> = param_infos.iter()
            .map(|&(n, is_ptr)| if is_ptr { PTR_BITS } else { n })
            .flat_map(|count| (0..count).map(|_| bit_tid))
            .collect();
        let sig_id = SigId(self.module.sigs.len());
        self.module.sigs.push(SigDecl { params: sig_params, results: vec![] });

        let mut fb = FuncBuilder::new(name.to_string(), sig_id, bit_tid);
        let mut groups: Vec<Vec<VaffleValue>> = Vec::new();
        // Collect (group_idx, addr_bits, orig_type, orig_bits) for deferred loads.
        let mut deferred: Vec<(usize, Vec<ValueId>, LirType, usize)> = Vec::new();

        for (pi, ty) in params.iter().enumerate() {
            let (n, is_ptr) = param_infos[pi];
            if is_ptr {
                let bits: Vec<ValueId> = (0..PTR_BITS)
                    .map(|_| fb.emit_block_param(0, bit_tid))
                    .collect();
                deferred.push((pi, bits, ty.clone(), n));
                groups.push(vec![]); // placeholder — filled after loads
            } else {
                let bits: Vec<ValueId> = (0..n)
                    .map(|_| fb.emit_block_param(0, bit_tid))
                    .collect();
                groups.push(vec![VaffleValue { bits, ty: ty.clone() }]);
            }
        }
        self.func = Some(fb);

        // Emit StorageReads for ptr-passed params (now self.func is set).
        for (pi, addr_bits, ty, n) in deferred {
            let storage = StorageId::STACK;
            let bit_tid = self.bit_tid();
            let mut loaded = Vec::with_capacity(n);
            for i in 0..n {
                if i == 0 {
                    let v = self.emit_read(storage, bit_tid, &addr_bits);
                    loaded.push(v);
                } else {
                    let off = self.iconst(LirType::U32, i as i64);
                    let off_bits = self.pad_to_ptr_bits(off);
                    let addr = bc_add(self, &addr_bits, &off_bits, false);
                    let v = self.emit_read(storage, bit_tid, &addr);
                    loaded.push(v);
                }
            }
            groups[pi] = vec![VaffleValue { bits: loaded, ty }];
        }
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

        let threshold = self.abi().aggregate_byval_limit;

        // Build the flat argument list, writing large args to stack storage.
        let mut flat_args: Vec<ValueId> = Vec::new();
        for arg in args {
            if self.optimized_abi && arg.bits.len() > threshold {
                // Allocate a stack slot for this arg.
                let base_slot = self.fb().next_stack_slot;
                self.fb().next_stack_slot += arg.bits.len() as u64;

                // Emit address constant.
                let addr_const: Vec<ValueId> = (0..PTR_BITS)
                    .map(|i| self.bc_const((base_slot >> i) & 1 != 0))
                    .collect();

                // Write each bit to StorageId::STACK.
                let bit_tid = self.bit_tid();
                let storage = StorageId::STACK;
                for (i, &bit) in arg.bits.iter().enumerate() {
                    if i == 0 {
                        self.emit_write(storage, bit, bit_tid, &addr_const);
                    } else {
                        let off = self.iconst(LirType::U32, i as i64);
                        let off_bits = self.pad_to_ptr_bits(off);
                        let addr = bc_add(self, &addr_const, &off_bits, false);
                        self.emit_write(storage, bit, bit_tid, &addr);
                    }
                }
                // Pass the address instead of the raw bits.
                flat_args.extend(addr_const);
            } else {
                flat_args.extend(arg.bits.iter().copied());
            }
        }

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

    fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = Self::Value>> {
        Some(self)
    }

    fn abi(&self) -> LirAbi {
        if self.optimized_abi {
            LirAbi::VAFFLE_OPTIMIZED
        } else {
            LirAbi::CIRCUIT
        }
    }
}

// ============================================================================
// Tail-call support
// ============================================================================

impl VaffleTarget {
    /// Emit a tail-call terminator: `Terminator::ReturnCall { func, args }`.
    ///
    /// Looks up `name` in the module's export table (same as `call_extern`); if
    /// not found, registers an Import stub.  The current block's terminator is
    /// set to `ReturnCall` — no `Value::Call` or `Value::Output` nodes are
    /// emitted.  The caller is responsible for ensuring no further stmts follow.
    pub fn ret_call(&mut self, name: &str, args: &[VaffleValue]) {
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
        let flat: Vec<ValueId> = args.iter().flat_map(|v| v.bits.iter().copied()).collect();
        let fb = self.fb();
        let cur = fb.current;
        fb.blocks[cur].terminator = Some(Terminator::ReturnCall { func: func_id, args: flat });
    }

    /// Emit a call with multiple return types and return all results as separate
    /// [`VaffleValue`]s.
    ///
    /// Unlike [`LirTarget::call_extern`] (which accepts at most one return type),
    /// `call_extern_multi` handles an arbitrary number of return values by
    /// assigning each its own slice of `Value::Output { idx }` nodes.
    /// This is used by the WAFFLE lowering for multi-result calls and for
    /// globals-tunnelling (where globals are appended to both args and rets).
    pub fn call_extern_multi(
        &mut self,
        name: &str,
        args: &[VaffleValue],
        ret_tys: &[LirType],
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

        let mut results = Vec::with_capacity(ret_tys.len());
        let mut bit_offset = 0usize;
        for ty in ret_tys {
            let n = bits_for_lir_type(ty, &self.struct_widths);
            let bits: Vec<ValueId> = (bit_offset..bit_offset + n)
                .map(|i| self.fb().emit_value(Value::Output { value: call_id, idx: i }))
                .collect();
            results.push(VaffleValue { bits, ty: ty.clone() });
            bit_offset += n;
        }
        results
    }
}

// ============================================================================
// StackAllocExt — stack allocation via StorageId::STACK
// ============================================================================

impl StackAllocExt for VaffleTarget {
    type Value = VaffleValue;

    fn alloca(&mut self, elem_ty: LirType, count: usize) -> VaffleValue {
        let elem_bits = bits_for_lir_type(&elem_ty, &self.struct_widths);
        let total_slots = (elem_bits * count) as u64;
        let base_slot = self.fb().next_stack_slot;
        self.fb().next_stack_slot += total_slots;

        // Intern the pointee type so we can record it in the VAFFLE value.
        let elem_tid = self.lir_type_to_tid(&elem_ty);

        // Emit the StackAlloc value node.
        let alloc_val = Value::StackAlloc {
            elem_ty: elem_tid,
            count,
            base_slot,
        };
        let _alloc_vid = self.fb().emit_value(alloc_val);

        // The pointer is the constant address `base_slot`, bit-decomposed.
        let addr_bits: Vec<ValueId> = (0..PTR_BITS)
            .map(|i| self.bc_const((base_slot >> i) & 1 != 0))
            .collect();
        VaffleValue {
            bits: addr_bits,
            ty: LirType::Ptr(alloc::boxed::Box::new(elem_ty)),
        }
    }

    fn ptr_load(&mut self, ptr: VaffleValue, ty: LirType) -> VaffleValue {
        let n = bits_for_lir_type(&ty, &self.struct_widths);
        let bit_tid = self.bit_tid();
        let storage = StorageId::STACK;

        // Read `n` individual bit-slots from STACK storage.
        let mut bits = Vec::with_capacity(n);
        for i in 0..n {
            let mut addr_bits = ptr.bits.clone();
            if i > 0 {
                // offset = ptr + i
                let off = self.iconst(LirType::U32, i as i64);
                let padded = self.pad_to_ptr_bits(off);
                addr_bits = bc_add(self, &addr_bits, &padded, false);
            }
            let v = self.emit_read(storage, bit_tid, &addr_bits);
            bits.push(v);
        }

        // Record the PtrLoad in the VAFFLE value stream for the lowering pass.
        let pointee_tid = self.lir_type_to_tid(&ty);
        let load_val = Value::PtrLoad {
            ptr: ptr.bits[0], // representative (the actual addr is in the storage reads above)
            pointee_ty: pointee_tid,
        };
        let _load_vid = self.fb().emit_value(load_val);

        VaffleValue { bits, ty }
    }

    fn ptr_store(&mut self, ptr: VaffleValue, val: VaffleValue) {
        let n = val.bits.len();
        let bit_tid = self.bit_tid();
        let storage = StorageId::STACK;

        for i in 0..n {
            let mut addr_bits = ptr.bits.clone();
            if i > 0 {
                let off = self.iconst(LirType::U32, i as i64);
                let padded = self.pad_to_ptr_bits(off);
                addr_bits = bc_add(self, &addr_bits, &padded, false);
            }
            self.emit_write(storage, val.bits[i], bit_tid, &addr_bits);
        }

        // Record the PtrStore for the VAFFLE lowering.
        let store_val = Value::PtrStore {
            ptr: ptr.bits[0],
            val: val.bits[0],
        };
        let _store_vid = self.fb().emit_value(store_val);
    }

    fn ptr_offset(&mut self, ptr: VaffleValue, idx: VaffleValue) -> VaffleValue {
        let pointee_ty = match &ptr.ty {
            LirType::Ptr(inner) => inner.as_ref().clone(),
            other => other.clone(),
        };
        let elem_bits = bits_for_lir_type(&pointee_ty, &self.struct_widths);

        // offset_slots = idx * elem_bits
        let scale = self.iconst(LirType::U32, elem_bits as i64);
        let scaled_idx = self.mul(idx, scale);

        // Pad to pointer width and add.
        let padded = self.pad_to_ptr_bits(scaled_idx);
        let new_bits = bc_add(self, &ptr.bits, &padded, false);

        // Record the PtrOffset for the VAFFLE lowering.
        let offset_val = Value::PtrOffset {
            ptr: ptr.bits[0],
            idx: new_bits[0], // representative
            elem_bits,
        };
        let _offset_vid = self.fb().emit_value(offset_val);

        VaffleValue {
            bits: new_bits,
            ty: ptr.ty.clone(),
        }
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Width of a stack pointer / alloca address in bits.
///
/// This determines how many `ValueId` bits a `LirType::Ptr(_)` value carries.
/// 32 bits allows addressing up to 4 billion storage slots per function frame,
/// which is more than enough for any realistic circuit.
pub const PTR_BITS: usize = 32;

pub(crate) fn bits_for_lir_type(ty: &LirType, struct_widths: &[usize]) -> usize {
    match ty {
        LirType::Bool => 1,
        LirType::I8 | LirType::U8 => 8,
        LirType::I16 | LirType::U16 => 16,
        LirType::I32 | LirType::U32 => 32,
        LirType::I64 | LirType::U64 => 64,
        LirType::I128 | LirType::U128 => 128,
        LirType::Arr(elem, n) => n * bits_for_lir_type(elem, struct_widths),
        LirType::Struct(id) => struct_widths[*id as usize],
        LirType::Native(_) => 1,
        LirType::Ptr(_) => PTR_BITS,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use volar_ir_common::Stmt;

    #[test]
    fn test_stack_alloc_ext_available() {
        let mut t = VaffleTarget::new();
        let (_entry, _params) = t.begin_function("test", &[LirType::U32], None);
        assert!(
            t.stack_alloc_ext().is_some(),
            "VaffleTarget must return Some from stack_alloc_ext"
        );
        t.ret(&[]);
        t.end_function();
    }

    #[test]
    fn test_alloca_produces_stack_alloc_value() {
        let mut t = VaffleTarget::new();
        let (entry, _params) = t.begin_function("test", &[], None);
        t.switch_to_block(entry);

        let ptr = t.alloca(LirType::U32, 4);

        // The returned value should be a Ptr type with PTR_BITS bits.
        assert_eq!(ptr.bits.len(), PTR_BITS);
        assert!(matches!(ptr.ty, LirType::Ptr(_)));

        t.ret(&[]);
        t.end_function();

        // The function body should contain a StackAlloc value.
        let body = match &t.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };
        let has_stack_alloc = body.values.iter().any(|v| matches!(v, Value::StackAlloc { .. }));
        assert!(has_stack_alloc, "VAFFLE should contain a StackAlloc value");
    }

    #[test]
    fn test_ptr_store_and_load_round_trip() {
        let mut t = VaffleTarget::new();
        let (entry, _params) = t.begin_function("test", &[], Some(LirType::U32));
        t.switch_to_block(entry);

        // Allocate 1 element of U32.
        let ptr = t.alloca(LirType::U32, 1);

        // Store a constant.
        let val = t.iconst(LirType::U32, 42);
        t.ptr_store(ptr.clone(), val);

        // Load it back.
        let loaded = t.ptr_load(ptr, LirType::U32);
        assert_eq!(loaded.bits.len(), 32);
        assert_eq!(loaded.ty, LirType::U32);

        t.ret(&[loaded]);
        t.end_function();

        // Should have StorageRead and StorageWrite with STACK storage.
        let body = match &t.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };
        let has_stack_write = body.values.iter().any(|v| matches!(
            v, Value::Op(Stmt::StorageWrite { storage, .. }) if *storage == StorageId::STACK
        ));
        let has_stack_read = body.values.iter().any(|v| matches!(
            v, Value::Op(Stmt::StorageRead { storage, .. }) if *storage == StorageId::STACK
        ));
        assert!(has_stack_write, "ptr_store should emit StorageWrite to STACK");
        assert!(has_stack_read, "ptr_load should emit StorageRead from STACK");
    }

    #[test]
    fn test_ptr_offset_element_scaling() {
        let mut t = VaffleTarget::new();
        let (entry, _params) = t.begin_function("test", &[], None);
        t.switch_to_block(entry);

        // Allocate 4 elements of U32 (32 bits each).
        let ptr = t.alloca(LirType::U32, 4);
        let idx = t.iconst(LirType::U32, 2);
        let offset_ptr = t.ptr_offset(ptr.clone(), idx);

        // The offset pointer should still be a Ptr type.
        assert!(matches!(offset_ptr.ty, LirType::Ptr(_)));
        assert_eq!(offset_ptr.bits.len(), PTR_BITS);

        // There should be a PtrOffset value in the body.
        t.ret(&[]);
        t.end_function();

        let body = match &t.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };
        let has_ptr_offset = body.values.iter().any(|v| matches!(v, Value::PtrOffset { elem_bits: 32, .. }));
        assert!(has_ptr_offset, "VAFFLE should contain a PtrOffset with elem_bits=32");
    }

    #[test]
    fn test_multiple_allocas_non_overlapping() {
        let mut t = VaffleTarget::new();
        let (entry, _params) = t.begin_function("test", &[], None);
        t.switch_to_block(entry);

        let ptr1 = t.alloca(LirType::U32, 2); // 64 slots
        let ptr2 = t.alloca(LirType::U8, 4);  // 32 slots

        t.ret(&[]);
        t.end_function();

        // Extract the base_slot from the StackAlloc values.
        let body = match &t.module.funcs[0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected function body"),
        };
        let allocs: std::vec::Vec<_> = body.values.iter().filter_map(|v| match v {
            Value::StackAlloc { base_slot, count, .. } => Some((*base_slot, *count)),
            _ => None,
        }).collect();
        assert_eq!(allocs.len(), 2);
        // First alloc at slot 0, size = 32*2 = 64 slots.
        assert_eq!(allocs[0].0, 0);
        // Second alloc at slot 64.
        assert_eq!(allocs[1].0, 64);
    }

    #[test]
    fn test_bits_for_lir_type_ptr() {
        assert_eq!(bits_for_lir_type(&LirType::Ptr(alloc::boxed::Box::new(LirType::U32)), &[]), PTR_BITS);
    }

    #[test]
    fn test_vaffle_target_abi_is_circuit() {
        let t = VaffleTarget::new();
        let abi = t.abi();
        assert!(!abi.native_aggregates);
        assert_eq!(abi.aggregate_byval_limit, usize::MAX);
        assert!(!abi.pass_by_ptr(1_000_000));
    }

    #[test]
    fn test_vaffle_optimized_abi_flag() {
        let t = VaffleTarget::new().with_optimized_abi();
        assert!(t.optimized_abi());
        let abi = t.abi();
        assert_eq!(abi.aggregate_byval_limit, 64);
        assert!(abi.pass_by_ptr(65));
        assert!(!abi.pass_by_ptr(64));
    }

    #[test]
    fn test_vaffle_set_optimized_abi_runtime() {
        let mut t = VaffleTarget::new();
        assert!(!t.optimized_abi());
        t.set_optimized_abi(true);
        assert!(t.optimized_abi());
        t.set_optimized_abi(false);
        assert!(!t.optimized_abi());
    }

    /// With `optimized_abi = false`, a 128-bit param produces 128 block params.
    #[test]
    fn test_begin_function_flat_abi_wide_param() {
        let mut t = VaffleTarget::new();
        let arr_ty = LirType::Arr(alloc::boxed::Box::new(LirType::U8), 16); // 128 bits
        let (entry, groups) = t.begin_function("f", &[arr_ty.clone()], None);
        t.switch_to_block(entry);

        // Flat ABI: 128 block params, 128 bits in the VaffleValue.
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0][0].bits.len(), 128);

        // Count the block params.
        let body = t.func.as_ref().unwrap();
        assert_eq!(body.blocks[0].params.len(), 128);

        t.ret(&[]);
        t.end_function();
    }

    /// With `optimized_abi = true`, a 128-bit param produces PTR_BITS block
    /// params (the address) and StorageRead stmts to load the actual bits.
    #[test]
    fn test_begin_function_optimized_abi_wide_param() {
        let mut t = VaffleTarget::new().with_optimized_abi();
        let arr_ty = LirType::Arr(alloc::boxed::Box::new(LirType::U8), 16); // 128 bits
        let (entry, groups) = t.begin_function("f", &[arr_ty.clone()], None);
        t.switch_to_block(entry);

        // Optimized ABI: callee still gets 128 bits (loaded from stack),
        // but block params are only PTR_BITS wide.
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0][0].bits.len(), 128);

        // Block params should be PTR_BITS, not 128.
        let body = t.func.as_ref().unwrap();
        assert_eq!(body.blocks[0].params.len(), PTR_BITS);

        // There should be StorageRead ops for loading the bits.
        let has_stack_read = body.all_values.iter().any(|v| matches!(
            v, Value::Op(Stmt::StorageRead { storage, .. }) if *storage == StorageId::STACK
        ));
        assert!(has_stack_read, "optimized ABI should emit StorageReads from STACK");

        t.ret(&[]);
        t.end_function();
    }

    /// A small param (e.g. U32 = 32 bits) is passed directly even with
    /// optimized ABI enabled.
    #[test]
    fn test_optimized_abi_small_param_direct() {
        let mut t = VaffleTarget::new().with_optimized_abi();
        let (entry, groups) = t.begin_function("f", &[LirType::U32], None);
        t.switch_to_block(entry);

        assert_eq!(groups[0][0].bits.len(), 32);
        let body = t.func.as_ref().unwrap();
        assert_eq!(body.blocks[0].params.len(), 32);

        t.ret(&[]);
        t.end_function();
    }

    /// call_extern with optimized ABI writes large args to stack and passes
    /// the address (PTR_BITS bits) instead.
    #[test]
    fn test_call_extern_optimized_abi_writes_stack() {
        let mut t = VaffleTarget::new().with_optimized_abi();
        let arr_ty = LirType::Arr(alloc::boxed::Box::new(LirType::U8), 16);
        let (entry, _) = t.begin_function("caller", &[], None);
        t.switch_to_block(entry);

        // Create a 128-bit value.
        let mut bits = std::vec::Vec::new();
        for i in 0..128 {
            bits.push(t.bc_const(i % 2 != 0));
        }
        let arg = VaffleValue { bits, ty: arr_ty.clone() };

        // Call with the large arg.
        t.call_extern("callee", &[arr_ty], &[arg], None);

        // There should be StorageWrite ops to STACK.
        let body = t.func.as_ref().unwrap();
        let has_stack_write = body.all_values.iter().any(|v| matches!(
            v, Value::Op(Stmt::StorageWrite { storage, .. }) if *storage == StorageId::STACK
        ));
        assert!(has_stack_write, "optimized call_extern should write large args to STACK");

        // The Call node's arg count should be PTR_BITS (the address),
        // NOT 128 (the raw bits).
        let call_arg_count = body.all_values.iter().find_map(|v| match v {
            Value::Call { args, .. } => Some(args.len()),
            _ => None,
        });
        assert_eq!(call_arg_count, Some(PTR_BITS), "call should pass PTR_BITS address bits");

        t.ret(&[]);
        t.end_function();
    }

    /// call_extern with optimized ABI passes small args directly.
    #[test]
    fn test_call_extern_optimized_abi_small_arg_direct() {
        let mut t = VaffleTarget::new().with_optimized_abi();
        let (entry, _) = t.begin_function("caller", &[], None);
        t.switch_to_block(entry);

        let val = t.iconst(LirType::U32, 42);
        t.call_extern("callee", &[LirType::U32], &[val], None);

        let body = t.func.as_ref().unwrap();
        let call_arg_count = body.all_values.iter().find_map(|v| match v {
            Value::Call { args, .. } => Some(args.len()),
            _ => None,
        });
        // 32 bits passed directly.
        assert_eq!(call_arg_count, Some(32));

        t.ret(&[]);
        t.end_function();
    }

    /// `ret_call` emits `Terminator::ReturnCall` and no `Value::Call` node.
    #[test]
    fn test_ret_call_emits_return_call_terminator() {
        let mut t = VaffleTarget::new();
        let (entry, params) = t.begin_function("caller", &[LirType::Bool], Some(LirType::Bool));
        t.switch_to_block(entry);

        let arg = params[0][0].clone();
        t.ret_call("callee", &[arg]);
        t.end_function();

        // Look up "caller" via the exports map — ret_call may have registered
        // an Import for "callee" at index 0 before end_function pushed the Body.
        let caller_fid = t.module.exports["caller"];
        let body = match &t.module.funcs[caller_fid.0] {
            vaffle::FuncDecl::Body(b) => b,
            _ => panic!("expected body"),
        };

        // Terminator must be ReturnCall.
        assert!(
            matches!(body.blocks[0].terminator, vaffle::Terminator::ReturnCall { .. }),
            "ret_call must produce ReturnCall terminator"
        );

        // No Value::Call node should be present (tail call, not a regular call).
        let has_call_node = body.values.iter().any(|v| matches!(v, Value::Call { .. }));
        assert!(!has_call_node, "ret_call must not emit a Value::Call node");
    }

    // ============================================================================
    // Corpus: smoke test (all cases build without panic)
    // ============================================================================

    #[test]
    fn test_corpus_smoke() {
        volar_lir_test_corpus::for_each_build!(VaffleTarget::new());
    }
}
