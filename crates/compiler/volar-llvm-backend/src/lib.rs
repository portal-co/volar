// @reliability: normal
// @ai: assisted
//! LLVM backend for `LirTarget` (via `inkwell` / LLVM 20).
//!
//! Emits a native LLVM `Module` that can be compiled to object code, LLVM IR
//! text, or bitcode.  Each LIR function becomes an LLVM function; SSA values
//! map 1-to-1 to LLVM `BasicValueEnum` handles.
//!
//! # ABI
//!
//! Returns [`LirAbi::DEFAULT`], so the `volar-lir-codegen` layer decomposes
//! all `Arr`/`Struct` types to flat scalars before calling any backend method.
//! The backend therefore only receives scalar `LirType` variants (`Bool`,
//! `I8`–`U64`, `Native`, `Ptr`) from the codegen layer.  `define_struct` is
//! still called for bookkeeping, but the resulting LLVM struct types are only
//! used internally (e.g. as alloca element types).
//!
//! # Block-parameter / PHI strategy
//!
//! LIR uses block-parameter SSA (Cranelift-style).  LLVM uses PHI nodes.
//! The mapping:
//!
//! 1. `add_block_param(block, ty)` — temporarily positions the builder at the
//!    end of `block` (which is empty at this point), emits the PHI node, then
//!    restores the previous insertion position.  Returns the PHI's
//!    `BasicValueEnum` as an [`LlvmValue`].
//!
//! 2. `switch_to_block(block)` — positions the builder at the end of `block`.
//!    PHI nodes are already present from step 1.
//!
//! 3. `jump(target, args)` / `branch(...)` — adds incoming edges to the
//!    target block's PHI nodes, then emits an unconditional/conditional branch.
//!
//! # Pointer types (opaque pointers)
//!
//! LLVM 20 uses untyped `ptr` for all pointer values.  `ptr_load` and GEP
//! operations receive the pointee type explicitly (stored in `LlvmValue::ty`).

use std::collections::HashMap;
use std::vec::Vec;

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PhiValue},
};
use volar_ir_common::Type as NativeType;
use volar_lir::{IcmpPred, LirAbi, LirTarget, LirType, StackAllocExt, StructDef, StructId};

pub use volar_lir::NameConfig;

// ============================================================================
// Value and Block handle types
// ============================================================================

/// An SSA value produced by the LLVM backend.
///
/// Wraps an inkwell `BasicValueEnum` together with its `LirType` so that
/// [`LirTarget::value_scalar_type`] can be answered without a separate map.
#[derive(Clone, Debug)]
pub struct LlvmValue<'ctx> {
    pub(crate) inner: BasicValueEnum<'ctx>,
    pub(crate) ty: LirType,
}

impl PartialEq for LlvmValue<'_> {
    fn eq(&self, other: &Self) -> bool {
        // LLVM values are pointer-unique; compare by the underlying raw pointer.
        self.inner == other.inner
    }
}

impl Eq for LlvmValue<'_> {}

/// A basic block handle used by the LLVM backend.
#[derive(Clone, Copy, Debug)]
pub struct LlvmBlock<'ctx> {
    pub(crate) inner: inkwell::basic_block::BasicBlock<'ctx>,
    /// Index into `FunctionState::blocks`.
    pub(crate) id: u32,
}

impl PartialEq for LlvmBlock<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.inner == other.inner
    }
}
impl Eq for LlvmBlock<'_> {}

// ============================================================================
// Per-block state
// ============================================================================

struct BlockState<'ctx> {
    /// PHI nodes created by `add_block_param`, in declaration order.
    /// Each entry is `(phi_value, lir_type)`.
    phi_values: Vec<(PhiValue<'ctx>, LirType)>,
}

// ============================================================================
// Per-function state
// ============================================================================

struct FunctionState<'ctx> {
    func: FunctionValue<'ctx>,
    ret_ty: Option<LirType>,
    /// One entry per block created via `begin_function` / `create_block`.
    blocks: Vec<BlockState<'ctx>>,
}

// ============================================================================
// The backend
// ============================================================================

/// LLVM backend implementing [`LirTarget`].
///
/// # Usage
///
/// ```ignore
/// let ctx = inkwell::context::Context::create();
/// let mut backend = LlvmBackend::new(&ctx, "my_module");
/// // drive via `lower_biir` / `lower_module` / etc.
/// let module = backend.finish();
/// module.print_to_file("out.ll").unwrap();
/// ```
pub struct LlvmBackend<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current: Option<FunctionState<'ctx>>,

    // ---- Struct registry ----
    struct_defs: Vec<StructDef>,
    struct_names: Vec<String>,
    struct_llvm_types: Vec<StructType<'ctx>>,
    next_struct_id: StructId,

    // ---- Extern function cache ----
    /// Maps declared function names to their `FunctionValue` to avoid
    /// re-declaring the same extern multiple times.
    extern_cache: HashMap<String, FunctionValue<'ctx>>,

    /// Name configuration: prefix and per-name remaps applied to all defined
    /// and called function names.  See [`NameConfig`].
    pub name_config: NameConfig,

    /// Name of the C function called for `rng` stmts.
    /// Expected signature: `void volar_rng(void*, uint64_t)`.
    /// Default: `"volar_rng"`.
    pub rng_fn: String,
}

impl<'ctx> LlvmBackend<'ctx> {
    /// Create a new backend bound to `context`, producing a module named
    /// `module_name`.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        LlvmBackend {
            context,
            module: context.create_module(module_name),
            builder: context.create_builder(),
            current: None,
            struct_defs: Vec::new(),
            struct_names: Vec::new(),
            struct_llvm_types: Vec::new(),
            next_struct_id: 0,
            extern_cache: HashMap::new(),
            name_config: NameConfig::default(),
            rng_fn: "volar_rng".to_string(),
        }
    }

    /// Set the name configuration (prefix + per-name remaps).
    pub fn with_name_config(mut self, config: NameConfig) -> Self {
        self.name_config = config;
        self
    }

    /// Convenience: set a prefix applied to all emitted function names.
    pub fn with_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.name_config.prefix = prefix.into();
        self
    }

    /// Set the RNG helper function name.
    pub fn with_rng_fn(mut self, name: impl Into<String>) -> Self {
        self.rng_fn = name.into();
        self
    }

    /// Consume the backend and return the finished LLVM module.
    pub fn finish(self) -> Module<'ctx> {
        self.module
    }

    /// Borrow the LLVM module (e.g. to verify or print before consuming).
    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }

    // ---- Internal helpers ---------------------------------------------------

    fn state(&mut self) -> &mut FunctionState<'ctx> {
        self.current.as_mut().expect("LlvmBackend: not inside a function")
    }

    /// Map a scalar `LirType` to its LLVM `BasicTypeEnum`.
    ///
    /// Panics on `Arr`/`Struct` — `LirAbi::DEFAULT` ensures the codegen layer
    /// flattens these before they reach the backend.
    fn lir_type_to_llvm(&self, ty: &LirType) -> BasicTypeEnum<'ctx> {
        match ty {
            LirType::Bool => self.context.bool_type().into(),
            LirType::I8 | LirType::U8 => self.context.i8_type().into(),
            LirType::I16 | LirType::U16 => self.context.i16_type().into(),
            LirType::I32 | LirType::U32 => self.context.i32_type().into(),
            LirType::I64 | LirType::U64 => self.context.i64_type().into(),
            LirType::I128 | LirType::U128 => self.context.i128_type().into(),
            LirType::Native(t) => self.native_type_to_llvm(*t).into(),
            // LLVM 21 opaque pointer.
            LirType::Ptr(_) => self.context.ptr_type(AddressSpace::default()).into(),
            LirType::Arr(_, _) | LirType::Struct(_) => {
                panic!(
                    "LlvmBackend: aggregate LirType {:?} passed to lir_type_to_llvm — \
                     backend uses LirAbi::DEFAULT; codegen should have flattened this",
                    ty
                )
            }
        }
    }

    /// Map a `NativeType` (GF-field element) to the closest LLVM integer type.
    fn native_type_to_llvm(&self, t: NativeType) -> inkwell::types::IntType<'ctx> {
        match t {
            NativeType::Bit => self.context.bool_type(),
            NativeType::_8 | NativeType::AES8 => self.context.i8_type(),
            NativeType::_16 => self.context.i16_type(),
            NativeType::_32 => self.context.i32_type(),
            NativeType::_64 | NativeType::Galois64 => self.context.i64_type(),
            // 128-bit: use i128
            NativeType::_128 => self.context.i128_type(),
            // 256-bit and beyond: conservative fallback to i64
            _ => self.context.i64_type(),
        }
    }

    /// Declare an external function in the module (idempotent).
    fn declare_extern(
        &mut self,
        name: &str,
        fn_type: FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        if let Some(&fv) = self.extern_cache.get(name) {
            return fv;
        }
        let fv = self.module.add_function(name, fn_type, Some(Linkage::External));
        self.extern_cache.insert(name.to_string(), fv);
        fv
    }

    /// Build a binary integer operation, returning a fresh value.
    fn int_binop(
        &mut self,
        lhs: LlvmValue<'ctx>,
        rhs: LlvmValue<'ctx>,
        op: impl FnOnce(
            &Builder<'ctx>,
            inkwell::values::IntValue<'ctx>,
            inkwell::values::IntValue<'ctx>,
        ) -> inkwell::values::IntValue<'ctx>,
    ) -> LlvmValue<'ctx> {
        let ty = lhs.ty.clone();
        let l = lhs.inner.into_int_value();
        let r = rhs.inner.into_int_value();
        let result = op(&self.builder, l, r);
        LlvmValue { inner: result.into(), ty }
    }

    /// Return the current insertion block (panics if builder is not positioned).
    fn current_block(&self) -> inkwell::basic_block::BasicBlock<'ctx> {
        self.builder
            .get_insert_block()
            .expect("LlvmBackend: builder has no current block")
    }
}

// ============================================================================
// LirTarget implementation
// ============================================================================

impl<'ctx> LirTarget for LlvmBackend<'ctx> {
    type Value = LlvmValue<'ctx>;
    type Block = LlvmBlock<'ctx>;

    // ---- ABI ----------------------------------------------------------------

    fn abi(&self) -> LirAbi {
        LirAbi::DEFAULT
    }

    // ---- Type registration --------------------------------------------------

    fn define_struct(&mut self, def: StructDef) -> StructId {
        let id = self.next_struct_id;
        self.next_struct_id += 1;

        // Build LLVM struct type from field types.
        // With LirAbi::DEFAULT, fields should all be scalars (no nested Arr/Struct
        // in practice), but we handle them gracefully anyway.
        let field_types: Vec<BasicTypeEnum<'ctx>> = def
            .fields
            .iter()
            .map(|f| self.lir_type_to_llvm(&f.ty))
            .collect();
        // Use opaque_struct_type + set_body to get a named struct in the IR.
        let struct_ty = self.context.opaque_struct_type(&def.name);
        struct_ty.set_body(&field_types, false);

        self.struct_defs.push(def.clone());
        self.struct_names.push(def.name.clone());
        self.struct_llvm_types.push(struct_ty);
        id
    }

    // ---- Value type query ---------------------------------------------------

    fn value_scalar_type(&self, val: &LlvmValue<'ctx>) -> LirType {
        val.ty.clone()
    }

    // ---- Function management ------------------------------------------------

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (LlvmBlock<'ctx>, Vec<Vec<LlvmValue<'ctx>>>) {
        assert!(
            self.current.is_none(),
            "begin_function called while already inside a function"
        );

        // Build LLVM function type.
        let param_llvm_tys: Vec<BasicMetadataTypeEnum<'ctx>> = params
            .iter()
            .map(|ty| self.lir_type_to_llvm(ty).into())
            .collect();

        let fn_type = match ret.as_ref().map(|ty| self.lir_type_to_llvm(ty)) {
            Some(ret_ty) => ret_ty.fn_type(&param_llvm_tys, false),
            None => self.context.void_type().fn_type(&param_llvm_tys, false),
        };

        let func = self.module.add_function(&self.name_config.apply(name), fn_type, None);

        // Create the entry block.
        let entry_llvm = self.context.append_basic_block(func, "block0");

        self.current = Some(FunctionState {
            func,
            ret_ty: ret,
            blocks: vec![BlockState { phi_values: Vec::new() }],
        });

        // Wrap each LLVM function parameter as LlvmValue.
        let param_vals: Vec<Vec<LlvmValue<'ctx>>> = func
            .get_params()
            .iter()
            .zip(params.iter())
            .map(|(param, ty)| vec![LlvmValue { inner: *param, ty: ty.clone() }])
            .collect();

        (LlvmBlock { inner: entry_llvm, id: 0 }, param_vals)
    }

    fn end_function(&mut self) {
        let _ = self.current.take().expect("end_function called outside a function");
        // Builder is left wherever the last instruction placed it; the caller
        // must have emitted a terminator in every block.
    }

    // ---- Block management ---------------------------------------------------

    fn create_block(&mut self) -> LlvmBlock<'ctx> {
        let (id, func) = {
            let state = self.state();
            let id = state.blocks.len() as u32;
            (id, state.func)
        };
        let name = format!("block{id}");
        let llvm_block = self.context.append_basic_block(func, &name);
        self.state().blocks.push(BlockState { phi_values: Vec::new() });
        LlvmBlock { inner: llvm_block, id }
    }

    /// Create a PHI node at the *start* of `block` and return its value.
    ///
    /// The builder is temporarily repositioned to the end of `block`
    /// (which is empty at this point — params are declared before any
    /// instructions are emitted into the block) to emit the PHI, then
    /// restored to the previous insertion point.
    fn add_block_param(&mut self, block: LlvmBlock<'ctx>, ty: LirType) -> LlvmValue<'ctx> {
        // Save current insertion position.
        let saved = self.builder.get_insert_block();

        // Move to the target block to emit the PHI at its head.
        self.builder.position_at_end(block.inner);

        let llvm_ty = self.lir_type_to_llvm(&ty);
        let phi = self.builder.build_phi(llvm_ty, "").unwrap();

        // Restore insertion position.
        if let Some(prev) = saved {
            self.builder.position_at_end(prev);
        }

        // Record the PHI for incoming-edge wiring in jump/branch.
        let state = self.state();
        state.blocks[block.id as usize]
            .phi_values
            .push((phi, ty.clone()));

        LlvmValue { inner: phi.as_basic_value(), ty }
    }

    fn switch_to_block(&mut self, block: LlvmBlock<'ctx>) {
        self.builder.position_at_end(block.inner);
    }

    // ---- Constants ----------------------------------------------------------

    fn iconst(&mut self, ty: LirType, val: i64) -> LlvmValue<'ctx> {
        let llvm_ty = self.lir_type_to_llvm(&ty);
        let int_ty = llvm_ty.into_int_type();
        // sign_extend=true so negative i64 values are represented correctly
        // in narrower types (the truncation to the correct bit width is
        // implicit in `const_int`'s bit pattern).
        let int_val = int_ty.const_int(val as u64, true);
        LlvmValue { inner: int_val.into(), ty }
    }

    // ---- Arithmetic ---------------------------------------------------------

    fn add(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_int_add(l, r, "").unwrap())
    }

    fn sub(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_int_sub(l, r, "").unwrap())
    }

    fn mul(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_int_mul(l, r, "").unwrap())
    }

    fn udiv(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_int_unsigned_div(l, r, "").unwrap())
    }

    fn sdiv(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_int_signed_div(l, r, "").unwrap())
    }

    // ---- Bitwise ------------------------------------------------------------

    fn and(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_and(l, r, "").unwrap())
    }

    fn or(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_or(l, r, "").unwrap())
    }

    fn xor(&mut self, lhs: LlvmValue<'ctx>, rhs: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(lhs, rhs, |b, l, r| b.build_xor(l, r, "").unwrap())
    }

    fn not(&mut self, val: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        let ty = val.ty.clone();
        let v = val.inner.into_int_value();
        let result = if ty == LirType::Bool {
            // LLVM has no logical NOT; XOR with true (1) is correct for i1.
            let true_val = self.context.bool_type().const_int(1, false);
            self.builder.build_xor(v, true_val, "").unwrap()
        } else {
            self.builder.build_not(v, "").unwrap()
        };
        LlvmValue { inner: result.into(), ty }
    }

    fn shl(&mut self, val: LlvmValue<'ctx>, shift: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        self.int_binop(val, shift, |b, l, r| b.build_left_shift(l, r, "").unwrap())
    }

    fn lshr(&mut self, val: LlvmValue<'ctx>, shift: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        // sign_extend = false → logical (zero-fill) right shift
        self.int_binop(val, shift, |b, l, r| b.build_right_shift(l, r, false, "").unwrap())
    }

    fn ashr(&mut self, val: LlvmValue<'ctx>, shift: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        // sign_extend = true → arithmetic (sign-fill) right shift
        self.int_binop(val, shift, |b, l, r| b.build_right_shift(l, r, true, "").unwrap())
    }

    // ---- Comparison ---------------------------------------------------------

    fn icmp(
        &mut self,
        pred: IcmpPred,
        lhs: LlvmValue<'ctx>,
        rhs: LlvmValue<'ctx>,
    ) -> LlvmValue<'ctx> {
        use inkwell::IntPredicate;
        let llvm_pred = match pred {
            IcmpPred::Eq => IntPredicate::EQ,
            IcmpPred::Ne => IntPredicate::NE,
            IcmpPred::Ult => IntPredicate::ULT,
            IcmpPred::Ule => IntPredicate::ULE,
            IcmpPred::Ugt => IntPredicate::UGT,
            IcmpPred::Uge => IntPredicate::UGE,
            IcmpPred::Slt => IntPredicate::SLT,
            IcmpPred::Sle => IntPredicate::SLE,
            IcmpPred::Sgt => IntPredicate::SGT,
            IcmpPred::Sge => IntPredicate::SGE,
        };
        let l = lhs.inner.into_int_value();
        let r = rhs.inner.into_int_value();
        let result = self.builder.build_int_compare(llvm_pred, l, r, "").unwrap();
        LlvmValue { inner: result.into(), ty: LirType::Bool }
    }

    // ---- Conversions --------------------------------------------------------

    fn zext(&mut self, val: LlvmValue<'ctx>, dst_ty: LirType) -> LlvmValue<'ctx> {
        let llvm_dst = self.lir_type_to_llvm(&dst_ty).into_int_type();
        let src = val.inner.into_int_value();
        let result = self.builder.build_int_z_extend(src, llvm_dst, "").unwrap();
        LlvmValue { inner: result.into(), ty: dst_ty }
    }

    fn sext(&mut self, val: LlvmValue<'ctx>, dst_ty: LirType) -> LlvmValue<'ctx> {
        let llvm_dst = self.lir_type_to_llvm(&dst_ty).into_int_type();
        let src = val.inner.into_int_value();
        let result = self.builder.build_int_s_extend(src, llvm_dst, "").unwrap();
        LlvmValue { inner: result.into(), ty: dst_ty }
    }

    fn trunc(&mut self, val: LlvmValue<'ctx>, dst_ty: LirType) -> LlvmValue<'ctx> {
        let llvm_dst = self.lir_type_to_llvm(&dst_ty).into_int_type();
        let src = val.inner.into_int_value();
        let result = self.builder.build_int_truncate(src, llvm_dst, "").unwrap();
        LlvmValue { inner: result.into(), ty: dst_ty }
    }

    // ---- Select -------------------------------------------------------------

    fn select(
        &mut self,
        cond: LlvmValue<'ctx>,
        then_val: LlvmValue<'ctx>,
        else_val: LlvmValue<'ctx>,
    ) -> LlvmValue<'ctx> {
        let ty = then_val.ty.clone();
        let c = cond.inner.into_int_value();
        let result = self
            .builder
            .build_select(c, then_val.inner, else_val.inner, "")
            .unwrap();
        LlvmValue { inner: result, ty }
    }

    // ---- Terminators --------------------------------------------------------

    fn jump(&mut self, target: LlvmBlock<'ctx>, args: &[LlvmValue<'ctx>]) {
        let pred_block = self.current_block();

        // Wire incoming edges into the target block's PHI nodes.
        // Safety: we hold a shared reference to `self.current.blocks` via the
        // block id, so we read phi values first before calling the builder.
        let phi_count = self
            .current
            .as_ref()
            .map(|s| s.blocks[target.id as usize].phi_values.len())
            .unwrap_or(0);

        assert_eq!(
            args.len(),
            phi_count,
            "jump: arg count ({}) != block param count ({})",
            args.len(),
            phi_count
        );

        for i in 0..phi_count {
            let phi = self
                .current
                .as_ref()
                .unwrap()
                .blocks[target.id as usize]
                .phi_values[i]
                .0;
            phi.add_incoming(&[(&args[i].inner, pred_block)]);
        }

        self.builder.build_unconditional_branch(target.inner).unwrap();
    }

    fn branch(
        &mut self,
        cond: LlvmValue<'ctx>,
        then_block: LlvmBlock<'ctx>,
        then_args: &[LlvmValue<'ctx>],
        else_block: LlvmBlock<'ctx>,
        else_args: &[LlvmValue<'ctx>],
    ) {
        // When both targets are the same block, emitting a conditional branch
        // with identical predecessors would produce a PHI node with two entries
        // from the same predecessor, which LLVM forbids.  Convert to `select`
        // per argument + unconditional branch instead.
        if then_block.id == else_block.id {
            let selected: Vec<LlvmValue<'ctx>> = then_args
                .iter()
                .zip(else_args.iter())
                .map(|(t, e)| self.select(cond.clone(), t.clone(), e.clone()))
                .collect();
            self.jump(then_block, &selected);
            return;
        }

        let pred_block = self.current_block();

        // Wire then-block PHIs.
        let then_phi_count = self
            .current
            .as_ref()
            .map(|s| s.blocks[then_block.id as usize].phi_values.len())
            .unwrap_or(0);
        assert_eq!(then_args.len(), then_phi_count);
        for i in 0..then_phi_count {
            let phi = self
                .current
                .as_ref()
                .unwrap()
                .blocks[then_block.id as usize]
                .phi_values[i]
                .0;
            phi.add_incoming(&[(&then_args[i].inner, pred_block)]);
        }

        // Wire else-block PHIs.
        let else_phi_count = self
            .current
            .as_ref()
            .map(|s| s.blocks[else_block.id as usize].phi_values.len())
            .unwrap_or(0);
        assert_eq!(else_args.len(), else_phi_count);
        for i in 0..else_phi_count {
            let phi = self
                .current
                .as_ref()
                .unwrap()
                .blocks[else_block.id as usize]
                .phi_values[i]
                .0;
            phi.add_incoming(&[(&else_args[i].inner, pred_block)]);
        }

        let cond_val = cond.inner.into_int_value();
        self.builder
            .build_conditional_branch(cond_val, then_block.inner, else_block.inner)
            .unwrap();
    }

    fn ret(&mut self, vals: &[LlvmValue<'ctx>]) {
        match vals {
            [] => {
                self.builder.build_return(None).unwrap();
            }
            [single] => {
                self.builder.build_return(Some(&single.inner)).unwrap();
            }
            _ => {
                // Multiple return values should not appear with LirAbi::DEFAULT
                // (aggregates are flattened to single scalars). Panic defensively.
                panic!(
                    "LlvmBackend::ret: {} values — LirAbi::DEFAULT \
                     should produce at most one return scalar",
                    vals.len()
                );
            }
        }
    }

    // ---- Extern calls -------------------------------------------------------

    fn call_extern(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[LlvmValue<'ctx>],
        ret_ty: Option<LirType>,
    ) -> Vec<LlvmValue<'ctx>> {
        // Build the LLVM function type for the extern declaration.
        let param_llvm_tys: Vec<BasicMetadataTypeEnum<'ctx>> = arg_tys
            .iter()
            .map(|ty| self.lir_type_to_llvm(ty).into())
            .collect();

        let fn_type = match ret_ty.as_ref().map(|ty| self.lir_type_to_llvm(ty)) {
            Some(r) => r.fn_type(&param_llvm_tys, false),
            None => self.context.void_type().fn_type(&param_llvm_tys, false),
        };

        let func = self.declare_extern(&self.name_config.apply(name), fn_type);

        // Build the call arguments.
        let call_args: Vec<BasicMetadataValueEnum<'ctx>> =
            args.iter().map(|v| v.inner.into()).collect();

        let call = self
            .builder
            .build_call(func, &call_args, "")
            .unwrap();

        match ret_ty {
            None => vec![],
            Some(ty) => {
                let ret_val = call
                    .try_as_basic_value()
                    .unwrap_basic();
                vec![LlvmValue { inner: ret_val, ty }]
            }
        }
    }

    // ---- External access primitives ----------------------------------------

    fn oracle(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[LlvmValue<'ctx>],
        ret_tys: &[LirType],
    ) -> Vec<LlvmValue<'ctx>> {
        // Treat oracle as a plain extern call (single output, like the C backend).
        let ret_ty = ret_tys.first().cloned();
        self.call_extern(&format!("oracle_{name}"), arg_tys, args, ret_ty)
    }

    fn action(
        &mut self,
        name: &str,
        guard: LlvmValue<'ctx>,
        arg_tys: &[LirType],
        args: &[LlvmValue<'ctx>],
        fallbacks: &[LlvmValue<'ctx>],
        ret_tys: &[LirType],
    ) -> Vec<LlvmValue<'ctx>> {
        // Call the action unconditionally, then select between result and fallback.
        let ret_ty = ret_tys.first().cloned();
        let action_result =
            self.call_extern(&format!("action_{name}"), arg_tys, args, ret_ty);
        action_result
            .iter()
            .zip(fallbacks.iter())
            .map(|(r, f)| self.select(guard.clone(), r.clone(), f.clone()))
            .collect()
    }

    fn rng(&mut self, ty: LirType) -> LlvmValue<'ctx> {
        // Strategy: alloca a slot of the target type, call volar_rng(ptr, size),
        // load and return.
        //
        // Expected C signature: void volar_rng(void* out, uint64_t len);
        // We approximate: declare as (ptr, i64) -> void.

        let llvm_ty = self.lir_type_to_llvm(&ty);
        let size_bytes = (llvm_ty.size_of().unwrap().get_zero_extended_constant().unwrap_or(8)) as u64;

        // Alloca the slot.
        let slot = self
            .builder
            .build_alloca(llvm_ty, "rng_slot")
            .unwrap();

        // Build the volar_rng call.
        let i64_ty = self.context.i64_type();
        let ptr_ty: BasicTypeEnum<'ctx> =
            self.context.ptr_type(AddressSpace::default()).into();
        let rng_fn_type = self
            .context
            .void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into()], false);
        let rng_fn_name = self.rng_fn.clone();
        let rng_func = self.declare_extern(&rng_fn_name, rng_fn_type);

        let size_val = i64_ty.const_int(size_bytes, false);
        self.builder
            .build_call(
                rng_func,
                &[slot.into(), size_val.into()],
                "",
            )
            .unwrap();

        // Load and return.
        let loaded = self.builder.build_load(llvm_ty, slot, "").unwrap();
        LlvmValue { inner: loaded, ty }
    }

    fn stack_alloc_ext(
        &mut self,
    ) -> Option<&mut dyn StackAllocExt<Value = LlvmValue<'ctx>>> {
        Some(self)
    }

    fn ptr_index_load(
        &mut self,
        ptr: LlvmValue<'ctx>,
        idx: LlvmValue<'ctx>,
        pointee_ty: &LirType,
    ) -> Vec<LlvmValue<'ctx>> {
        let ext = self.stack_alloc_ext().unwrap();
        let offset_ptr = ext.ptr_offset(ptr, idx);
        let loaded = ext.ptr_load(offset_ptr, pointee_ty.clone());
        vec![loaded]
    }

    fn ptr_index_store(
        &mut self,
        ptr: LlvmValue<'ctx>,
        idx: LlvmValue<'ctx>,
        vals: &[LlvmValue<'ctx>],
        _pointee_ty: &LirType,
    ) {
        assert_eq!(vals.len(), 1, "ptr_index_store: expected 1 scalar (DEFAULT ABI)");
        let val = vals[0].clone();
        let ext = self.stack_alloc_ext().unwrap();
        let offset_ptr = ext.ptr_offset(ptr, idx);
        ext.ptr_store(offset_ptr, val);
    }
}

// ============================================================================
// StackAllocExt implementation
// ============================================================================

impl<'ctx> StackAllocExt for LlvmBackend<'ctx> {
    type Value = LlvmValue<'ctx>;

    /// Allocate a stack region for `count` elements of `elem_ty`.
    ///
    /// Emits `alloca elem_ty, count` at the current insertion point.
    /// Returns a `LirType::Ptr(elem_ty)` value.
    ///
    /// Note: For canonical SSA form, callers should ensure alloca is emitted
    /// in the function entry block.  `volar-lir-codegen` does this by default
    /// when using the `C_NATIVE`-style ABI; with `DEFAULT` ABI alloca is
    /// emitted wherever the builder currently points.
    fn alloca(&mut self, elem_ty: LirType, count: usize) -> LlvmValue<'ctx> {
        let llvm_elem_ty = self.lir_type_to_llvm(&elem_ty);
        let count_val = self.context.i64_type().const_int(count as u64, false);
        let ptr = self
            .builder
            .build_array_alloca(llvm_elem_ty, count_val, "")
            .unwrap();
        LlvmValue {
            inner: ptr.into(),
            ty: LirType::Ptr(Box::new(elem_ty)),
        }
    }

    /// Load through a typed pointer.
    ///
    /// LLVM 21 uses opaque pointers; the pointee type must be provided
    /// explicitly via `ty`.
    fn ptr_load(&mut self, ptr: LlvmValue<'ctx>, ty: LirType) -> LlvmValue<'ctx> {
        let llvm_ty = self.lir_type_to_llvm(&ty);
        let ptr_val = ptr.inner.into_pointer_value();
        let loaded = self.builder.build_load(llvm_ty, ptr_val, "").unwrap();
        LlvmValue { inner: loaded, ty }
    }

    /// Store `val` through `ptr`.
    fn ptr_store(&mut self, ptr: LlvmValue<'ctx>, val: LlvmValue<'ctx>) {
        let ptr_val = ptr.inner.into_pointer_value();
        self.builder.build_store(ptr_val, val.inner).unwrap();
    }

    /// Element-wise pointer offset: `ptr + idx` elements.
    ///
    /// Uses GEP with the pointee type inferred from `ptr.ty`.
    fn ptr_offset(&mut self, ptr: LlvmValue<'ctx>, idx: LlvmValue<'ctx>) -> LlvmValue<'ctx> {
        let pointee_ty = match &ptr.ty {
            LirType::Ptr(inner) => self.lir_type_to_llvm(inner),
            other => panic!("ptr_offset: expected Ptr type, got {:?}", other),
        };
        let ptr_val = ptr.inner.into_pointer_value();
        let idx_val = idx.inner.into_int_value();
        let gep = unsafe {
            self.builder
                .build_gep(pointee_ty, ptr_val, &[idx_val], "")
                .unwrap()
        };
        LlvmValue { inner: gep.into(), ty: ptr.ty }
    }
}
