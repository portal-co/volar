//! `WasmFuncEncoder` -- the background-thread half of the WASM backend.
//!
//! Replays one function's recorded [`SavedLirModule`] (produced cheaply on
//! the driver thread by a per-function `RecordingTarget`, see
//! `WasmBackend::end_function` in `lib.rs`) into real `wasm_encoder` bytes.
//! This is where the actual per-instruction encoding cost lives, which is
//! exactly the work `WasmExecutor` lets run off the driver's thread.
//!
//! # Value model
//!
//! Every SSA value (including block parameters) gets its own dedicated
//! mutable WASM local -- no stack-slot reuse. Simplest possible correct
//! mapping; not optimized.
//!
//! # Control flow
//!
//! LIR is block-parameter SSA over an arbitrary CFG; WASM only has
//! structured, nested `block`/`loop`/`br`/`br_if`. This uses a generic,
//! always-correct **dispatch-loop relooper**: every LIR block gets an
//! integer label, the whole function body is one outer `loop` containing one
//! nested `block` per label (innermost = label 0), and a `br_table` on a
//! dedicated `$label` local redispatches to any label (forward or backward)
//! by setting `$label` and branching back to the loop. See `finish` for the
//! assembly and the module doc for why this particular shape validates.
use std::borrow::Cow;
use std::sync::Arc;

use volar_lir::{BranchTarget, IcmpPred, LirAbi, LirTarget, LirType, StackAllocExt, StructDef, StructId};
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

use crate::registry::{FuncRegistry, is_64, lir_scalar_to_valtype};

pub(crate) const STACK_PTR_GLOBAL: u32 = 0;

/// A LIR SSA value, represented as a dedicated WASM local.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WasmVal {
    pub(crate) local: u32,
    pub(crate) ty: LirType,
}

/// A single already-final `wasm_encoder` instruction, or a deferred
/// "transfer control to another LIR block via the dispatch loop" marker
/// whose exact `br` depth can only be computed once the function's total
/// block count is known (see module doc).
enum WOp {
    Real(Instruction<'static>),
    BrToLoop { from_block: u32, extra_depth: u32 },
}

pub(crate) struct WasmFuncEncoder {
    registry: Arc<FuncRegistry>,
    next_local: u32,
    /// Extra locals beyond the function's own parameters, in allocation
    /// order (WASM numbers parameters as locals `0..num_params`, then
    /// declared locals continue from there).
    locals: Vec<ValType>,
    per_block_code: std::collections::BTreeMap<u32, Vec<WOp>>,
    /// label -> ordered param-local indices, for parallel-assignment on jump/branch.
    block_params: std::collections::BTreeMap<u32, Vec<u32>>,
    current_block: Option<u32>,
    next_block_label: u32,
    label_local: u32,
    entry_sp_local: u32,
    /// The function's own result arity, as a `block`/`loop` type. Must be
    /// used for the outer dispatch loop (not `BlockType::Empty`): every path
    /// through the loop exits via an inner `Return` (which is validated
    /// against the *function's* result types, independent of enclosing
    /// nesting), but WASM validation still requires the loop's own declared
    /// result type to match what the function needs, because closing a
    /// block/loop resets to normal (non-polymorphic) validation in the
    /// enclosing frame regardless of what happened inside -- it does not
    /// inherit the "unreachable after return" state from within.
    result_block_type: BlockType,
}

impl WasmFuncEncoder {
    pub(crate) fn new(registry: Arc<FuncRegistry>) -> Self {
        WasmFuncEncoder {
            registry,
            next_local: 0,
            locals: Vec::new(),
            per_block_code: std::collections::BTreeMap::new(),
            block_params: std::collections::BTreeMap::new(),
            current_block: None,
            next_block_label: 0,
            label_local: 0,
            entry_sp_local: 0,
            result_block_type: BlockType::Empty,
        }
    }

    fn alloc_val(&mut self, ty: LirType) -> WasmVal {
        let vt = lir_scalar_to_valtype(&ty);
        let local = self.next_local;
        self.next_local += 1;
        self.locals.push(vt);
        WasmVal { local, ty }
    }

    /// Allocate a bookkeeping local (dispatch label, saved stack pointer)
    /// that has no corresponding LIR value.
    fn alloc_raw_local(&mut self, vt: ValType) -> u32 {
        let local = self.next_local;
        self.next_local += 1;
        self.locals.push(vt);
        local
    }

    fn emit_real(&mut self, insn: Instruction<'static>) {
        let block = self.current_block.expect("volar-wasm-backend: no current block (call switch_to_block first)");
        self.per_block_code.entry(block).or_default().push(WOp::Real(insn));
    }

    fn emit_transfer(&mut self, extra_depth: u32) {
        let block = self.current_block.expect("volar-wasm-backend: no current block");
        self.per_block_code
            .entry(block)
            .or_default()
            .push(WOp::BrToLoop { from_block: block, extra_depth });
    }

    /// Push `val` onto the stack, coercing its representation to match
    /// `want_64` if it doesn't already (defensive -- well-formed IR should
    /// already match widths, but e.g. shift-amount operands are not
    /// guaranteed to be the same width as the shifted value).
    fn push_coerced(&mut self, val: &WasmVal, want_64: bool) {
        self.emit_real(Instruction::LocalGet(val.local));
        let v_is_64 = is_64(&val.ty);
        if want_64 && !v_is_64 {
            if val.ty.is_signed() {
                self.emit_real(Instruction::I64ExtendI32S);
            } else {
                self.emit_real(Instruction::I64ExtendI32U);
            }
        } else if !want_64 && v_is_64 {
            self.emit_real(Instruction::I32WrapI64);
        }
    }

    /// Normalize the value currently on top of the WASM operand stack to
    /// `ty`'s canonical bit pattern (mask/sign-extend narrow scalars).
    /// I32/U32/I64/U64 are already native width -- no-op.
    fn narrow_mask_stack_top(&mut self, ty: &LirType) {
        match ty {
            LirType::Bool => {
                self.emit_real(Instruction::I32Const(1));
                self.emit_real(Instruction::I32And);
            }
            LirType::I8 => self.emit_real(Instruction::I32Extend8S),
            LirType::U8 => {
                self.emit_real(Instruction::I32Const(0xFF));
                self.emit_real(Instruction::I32And);
            }
            LirType::I16 => self.emit_real(Instruction::I32Extend16S),
            LirType::U16 => {
                self.emit_real(Instruction::I32Const(0xFFFF));
                self.emit_real(Instruction::I32And);
            }
            LirType::I32 | LirType::U32 | LirType::I64 | LirType::U64 | LirType::Ptr(_) => {}
            _ => panic!("volar-wasm-backend: narrow_mask_stack_top called on unsupported type {ty:?}"),
        }
    }

    fn binop(
        &mut self,
        lhs: WasmVal,
        rhs: WasmVal,
        out_ty: LirType,
        op32: Instruction<'static>,
        op64: Instruction<'static>,
    ) -> WasmVal {
        let want_64 = is_64(&out_ty);
        self.push_coerced(&lhs, want_64);
        self.push_coerced(&rhs, want_64);
        self.emit_real(if want_64 { op64 } else { op32 });
        self.narrow_mask_stack_top(&out_ty);
        let out = self.alloc_val(out_ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn emit_call(&mut self, idx: u32, args: &[WasmVal], ret_ty: Option<LirType>) -> Vec<WasmVal> {
        for a in args {
            self.emit_real(Instruction::LocalGet(a.local));
        }
        self.emit_real(Instruction::Call(idx));
        match ret_ty {
            Some(ty) => {
                let out = self.alloc_val(ty);
                self.emit_real(Instruction::LocalSet(out.local));
                vec![out]
            }
            None => vec![],
        }
    }

    fn emit_jump_to(&mut self, target: u32, args: &[WasmVal], extra_depth: u32) {
        let params = self.block_params.get(&target).cloned().unwrap_or_default();
        assert_eq!(
            params.len(),
            args.len(),
            "volar-wasm-backend: block {target} expects {} args, got {}",
            params.len(),
            args.len()
        );
        // Parallel assignment via the operand stack: push every source
        // first (a snapshot -- reads never alias a not-yet-written target),
        // then pop into the targets in reverse order. Avoids read-after-write
        // hazards without needing extra temporaries.
        for a in args {
            self.emit_real(Instruction::LocalGet(a.local));
        }
        for &p in params.iter().rev() {
            self.emit_real(Instruction::LocalSet(p));
        }
        self.emit_real(Instruction::I32Const(target as i32));
        self.emit_real(Instruction::LocalSet(self.label_local));
        self.emit_transfer(extra_depth);
    }

    /// Assemble the finished function body: prologue, dispatch loop, and
    /// every LIR block's code nested at the right nesting depth.
    pub(crate) fn finish(mut self) -> Function {
        let block_count = self.next_block_label;
        assert!(block_count > 0, "volar-wasm-backend: function has no blocks");

        let mut body: Vec<Instruction<'static>> = Vec::new();
        body.push(Instruction::GlobalGet(STACK_PTR_GLOBAL));
        body.push(Instruction::LocalSet(self.entry_sp_local));
        body.push(Instruction::I32Const(0));
        body.push(Instruction::LocalSet(self.label_local));

        body.push(Instruction::Loop(self.result_block_type));
        for _ in 0..block_count {
            body.push(Instruction::Block(BlockType::Empty));
        }

        // Dispatch, sitting inside the innermost block (label 0's own).
        // Blocks are opened label (block_count-1)..=0 outer-to-inner, so
        // label 0's own block is the innermost one (depth 0 from here),
        // label 1's is one level out (depth 1), etc: targets[L] = L.
        let targets: Vec<u32> = (0..block_count).collect();
        body.push(Instruction::LocalGet(self.label_local));
        body.push(Instruction::BrTable(Cow::Owned(targets.clone()), targets[0]));

        for label in 0..block_count {
            body.push(Instruction::End); // closes block `label`
            let ops = self.per_block_code.remove(&label).unwrap_or_default();
            for op in ops {
                body.push(match op {
                    WOp::Real(insn) => insn,
                    WOp::BrToLoop { from_block, extra_depth } => {
                        Instruction::Br(block_count - 1 - from_block + extra_depth)
                    }
                });
            }
        }
        body.push(Instruction::End); // closes the loop

        let mut func = Function::new_with_locals_types(self.locals);
        for insn in &body {
            func.instruction(insn);
        }
        func.instruction(&Instruction::End); // closes the function body itself
        func
    }
}

fn icmp_instr(want_64: bool, pred: IcmpPred) -> Instruction<'static> {
    use IcmpPred::*;
    match (want_64, pred) {
        (false, Eq) => Instruction::I32Eq,
        (false, Ne) => Instruction::I32Ne,
        (false, Ult) => Instruction::I32LtU,
        (false, Ule) => Instruction::I32LeU,
        (false, Ugt) => Instruction::I32GtU,
        (false, Uge) => Instruction::I32GeU,
        (false, Slt) => Instruction::I32LtS,
        (false, Sle) => Instruction::I32LeS,
        (false, Sgt) => Instruction::I32GtS,
        (false, Sge) => Instruction::I32GeS,
        (true, Eq) => Instruction::I64Eq,
        (true, Ne) => Instruction::I64Ne,
        (true, Ult) => Instruction::I64LtU,
        (true, Ule) => Instruction::I64LeU,
        (true, Ugt) => Instruction::I64GtU,
        (true, Uge) => Instruction::I64GeU,
        (true, Slt) => Instruction::I64LtS,
        (true, Sle) => Instruction::I64LeS,
        (true, Sgt) => Instruction::I64GtS,
        (true, Sge) => Instruction::I64GeS,
    }
}

fn load_instr(ty: &LirType, m: MemArg) -> Instruction<'static> {
    match ty {
        LirType::Bool | LirType::U8 => Instruction::I32Load8U(m),
        LirType::I8 => Instruction::I32Load8S(m),
        LirType::U16 => Instruction::I32Load16U(m),
        LirType::I16 => Instruction::I32Load16S(m),
        LirType::I32 | LirType::U32 => Instruction::I32Load(m),
        LirType::I64 | LirType::U64 => Instruction::I64Load(m),
        LirType::Ptr(_) => Instruction::I32Load(m),
        _ => panic!("volar-wasm-backend: unsupported pointee type for load: {ty:?}"),
    }
}

fn store_instr(ty: &LirType, m: MemArg) -> Instruction<'static> {
    match ty {
        LirType::Bool | LirType::U8 | LirType::I8 => Instruction::I32Store8(m),
        LirType::U16 | LirType::I16 => Instruction::I32Store16(m),
        LirType::I32 | LirType::U32 => Instruction::I32Store(m),
        LirType::I64 | LirType::U64 => Instruction::I64Store(m),
        LirType::Ptr(_) => Instruction::I32Store(m),
        _ => panic!("volar-wasm-backend: unsupported pointee type for store: {ty:?}"),
    }
}

fn elem_byte_size(ty: &LirType) -> i32 {
    ((ty.bit_width() + 7) / 8).max(1) as i32
}

impl LirTarget for WasmFuncEncoder {
    type Value = WasmVal;
    type Block = u32;

    fn abi(&self) -> LirAbi {
        LirAbi::WASM
    }

    fn define_struct(&mut self, _def: StructDef) -> StructId {
        panic!("volar-wasm-backend: struct types are not supported in v1")
    }

    fn declare_import(&mut self, _name: &str, _params: &[LirType], _ret: Option<LirType>) {
        // No-op: WasmBackend already resolved every import to a final index
        // before this function's body was ever begun.
    }

    fn declare_function(&mut self, _name: &str, _params: &[LirType], _ret: Option<LirType>) {
        // No-op: same reasoning as `declare_import`.
    }

    fn begin_function(&mut self, _name: &str, params: &[LirType], ret: Option<LirType>) -> (u32, Vec<Vec<WasmVal>>) {
        let num_params = params.len() as u32;
        self.next_local = num_params;
        self.result_block_type = match ret {
            Some(ty) => BlockType::Result(lir_scalar_to_valtype(&ty)),
            None => BlockType::Empty,
        };

        let param_vals: Vec<Vec<WasmVal>> = params
            .iter()
            .enumerate()
            .map(|(i, ty)| vec![WasmVal { local: i as u32, ty: ty.clone() }])
            .collect();

        self.label_local = self.alloc_raw_local(ValType::I32);
        self.entry_sp_local = self.alloc_raw_local(ValType::I32);

        self.block_params.insert(0, (0..num_params).collect());
        self.per_block_code.insert(0, Vec::new());
        self.current_block = Some(0);
        self.next_block_label = 1;

        (0, param_vals)
    }

    fn end_function(&mut self) {
        // Actual assembly happens in `finish`, once every block is known.
    }

    fn create_block(&mut self) -> u32 {
        let label = self.next_block_label;
        self.next_block_label += 1;
        self.per_block_code.insert(label, Vec::new());
        label
    }

    fn add_block_param(&mut self, block: u32, ty: LirType) -> WasmVal {
        let val = self.alloc_val(ty);
        self.block_params.entry(block).or_default().push(val.local);
        val
    }

    fn switch_to_block(&mut self, block: u32) {
        self.current_block = Some(block);
    }

    fn iconst(&mut self, ty: LirType, val: i64) -> WasmVal {
        let want_64 = is_64(&ty);
        if want_64 {
            self.emit_real(Instruction::I64Const(val));
        } else {
            self.emit_real(Instruction::I32Const(val as i32));
        }
        self.narrow_mask_stack_top(&ty);
        let out = self.alloc_val(ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn add(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32Add, Instruction::I64Add)
    }
    fn sub(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32Sub, Instruction::I64Sub)
    }
    fn mul(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32Mul, Instruction::I64Mul)
    }
    fn udiv(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32DivU, Instruction::I64DivU)
    }
    fn sdiv(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32DivS, Instruction::I64DivS)
    }

    fn and(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32And, Instruction::I64And)
    }
    fn or(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32Or, Instruction::I64Or)
    }
    fn xor(&mut self, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let ty = lhs.ty.clone();
        self.binop(lhs, rhs, ty, Instruction::I32Xor, Instruction::I64Xor)
    }
    fn not(&mut self, val: WasmVal) -> WasmVal {
        let ty = val.ty.clone();
        let want_64 = is_64(&ty);
        self.push_coerced(&val, want_64);
        if want_64 {
            self.emit_real(Instruction::I64Const(-1));
            self.emit_real(Instruction::I64Xor);
        } else {
            self.emit_real(Instruction::I32Const(-1));
            self.emit_real(Instruction::I32Xor);
        }
        self.narrow_mask_stack_top(&ty);
        let out = self.alloc_val(ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }
    fn shl(&mut self, val: WasmVal, shift: WasmVal) -> WasmVal {
        let ty = val.ty.clone();
        self.binop(val, shift, ty, Instruction::I32Shl, Instruction::I64Shl)
    }
    fn lshr(&mut self, val: WasmVal, shift: WasmVal) -> WasmVal {
        let ty = val.ty.clone();
        self.binop(val, shift, ty, Instruction::I32ShrU, Instruction::I64ShrU)
    }
    fn ashr(&mut self, val: WasmVal, shift: WasmVal) -> WasmVal {
        let ty = val.ty.clone();
        self.binop(val, shift, ty, Instruction::I32ShrS, Instruction::I64ShrS)
    }

    fn icmp(&mut self, pred: IcmpPred, lhs: WasmVal, rhs: WasmVal) -> WasmVal {
        let want_64 = is_64(&lhs.ty);
        self.push_coerced(&lhs, want_64);
        self.push_coerced(&rhs, want_64);
        self.emit_real(icmp_instr(want_64, pred));
        let out = self.alloc_val(LirType::Bool);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn zext(&mut self, val: WasmVal, dst_ty: LirType) -> WasmVal {
        assert!(
            !is_64(&val.ty) || is_64(&dst_ty),
            "volar-wasm-backend: zext source wider than destination"
        );
        self.emit_real(Instruction::LocalGet(val.local));
        if !is_64(&val.ty) && is_64(&dst_ty) {
            self.emit_real(Instruction::I64ExtendI32U);
        }
        self.narrow_mask_stack_top(&dst_ty);
        let out = self.alloc_val(dst_ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn sext(&mut self, val: WasmVal, dst_ty: LirType) -> WasmVal {
        if matches!(val.ty, LirType::Bool) {
            // 0/1 -> 0/-1 (all bits set), so the sign bit lands correctly at
            // any destination width.
            self.emit_real(Instruction::I32Const(0));
            self.emit_real(Instruction::LocalGet(val.local));
            self.emit_real(Instruction::I32Sub);
            if is_64(&dst_ty) {
                self.emit_real(Instruction::I64ExtendI32S);
            }
        } else {
            self.emit_real(Instruction::LocalGet(val.local));
            if !is_64(&val.ty) && is_64(&dst_ty) {
                self.emit_real(Instruction::I64ExtendI32S);
            }
        }
        self.narrow_mask_stack_top(&dst_ty);
        let out = self.alloc_val(dst_ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn trunc(&mut self, val: WasmVal, dst_ty: LirType) -> WasmVal {
        self.emit_real(Instruction::LocalGet(val.local));
        if is_64(&val.ty) && !is_64(&dst_ty) {
            self.emit_real(Instruction::I32WrapI64);
        }
        self.narrow_mask_stack_top(&dst_ty);
        let out = self.alloc_val(dst_ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn select(&mut self, cond: WasmVal, then_val: WasmVal, else_val: WasmVal) -> WasmVal {
        let out = self.alloc_val(then_val.ty.clone());
        self.emit_real(Instruction::LocalGet(cond.local));
        self.emit_real(Instruction::If(BlockType::Empty));
        self.emit_real(Instruction::LocalGet(then_val.local));
        self.emit_real(Instruction::LocalSet(out.local));
        self.emit_real(Instruction::Else);
        self.emit_real(Instruction::LocalGet(else_val.local));
        self.emit_real(Instruction::LocalSet(out.local));
        self.emit_real(Instruction::End);
        out
    }

    fn value_scalar_type(&self, val: &WasmVal) -> LirType {
        val.ty.clone()
    }

    fn call_extern(&mut self, name: &str, _arg_tys: &[LirType], args: &[WasmVal], ret_ty: Option<LirType>) -> Vec<WasmVal> {
        let idx = self.registry.index_of(name);
        self.emit_call(idx, args, ret_ty)
    }

    fn jump(&mut self, target: u32, branch: BranchTarget<WasmVal>) {
        self.emit_jump_to(target, &branch.args, 0);
    }

    fn branch(
        &mut self,
        cond: WasmVal,
        then_block: u32,
        then_branch: BranchTarget<WasmVal>,
        else_block: u32,
        else_branch: BranchTarget<WasmVal>,
    ) {
        self.emit_real(Instruction::LocalGet(cond.local));
        self.emit_real(Instruction::If(BlockType::Empty));
        self.emit_jump_to(then_block, &then_branch.args, 1);
        self.emit_real(Instruction::Else);
        self.emit_jump_to(else_block, &else_branch.args, 1);
        self.emit_real(Instruction::End);
    }

    fn ret(&mut self, vals: &[WasmVal]) {
        self.emit_real(Instruction::LocalGet(self.entry_sp_local));
        self.emit_real(Instruction::GlobalSet(STACK_PTR_GLOBAL));
        for v in vals {
            self.emit_real(Instruction::LocalGet(v.local));
        }
        self.emit_real(Instruction::Return);
    }

    fn oracle(&mut self, name: &str, _arg_tys: &[LirType], args: &[WasmVal], ret_tys: &[LirType]) -> Vec<WasmVal> {
        assert!(ret_tys.len() <= 1, "volar-wasm-backend: multi-output oracle not supported in v1");
        let idx = self.registry.index_of(name);
        self.emit_call(idx, args, ret_tys.first().cloned())
    }

    fn action(
        &mut self,
        name: &str,
        guard: WasmVal,
        _arg_tys: &[LirType],
        args: &[WasmVal],
        fallbacks: &[WasmVal],
        ret_tys: &[LirType],
    ) -> Vec<WasmVal> {
        assert!(ret_tys.len() <= 1, "volar-wasm-backend: multi-output action not supported in v1");
        let idx = self.registry.index_of(name);
        let out = ret_tys.first().cloned().map(|ty| self.alloc_val(ty));

        self.emit_real(Instruction::LocalGet(guard.local));
        self.emit_real(Instruction::If(BlockType::Empty));
        for a in args {
            self.emit_real(Instruction::LocalGet(a.local));
        }
        self.emit_real(Instruction::Call(idx));
        if let Some(o) = &out {
            self.emit_real(Instruction::LocalSet(o.local));
        }
        self.emit_real(Instruction::Else);
        if let (Some(o), Some(fb)) = (&out, fallbacks.first()) {
            self.emit_real(Instruction::LocalGet(fb.local));
            self.emit_real(Instruction::LocalSet(o.local));
        }
        self.emit_real(Instruction::End);

        out.into_iter().collect()
    }

    fn rng(&mut self, ty: LirType) -> WasmVal {
        let idx = self.registry.index_of(crate::RNG_IMPORT_NAME);
        self.emit_call(idx, &[], Some(ty)).pop().expect("rng: import declared with no return type")
    }

    fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = WasmVal>> {
        Some(self)
    }

    fn ptr_index_load(&mut self, ptr: WasmVal, idx: WasmVal, pointee_ty: &LirType) -> Vec<WasmVal> {
        let addr = StackAllocExt::ptr_offset(self, ptr, idx);
        vec![StackAllocExt::ptr_load(self, addr, pointee_ty.clone())]
    }

    fn ptr_index_store(&mut self, ptr: WasmVal, idx: WasmVal, vals: &[WasmVal], _pointee_ty: &LirType) {
        assert_eq!(vals.len(), 1, "volar-wasm-backend: ptr_index_store only supports a scalar pointee in v1");
        let addr = StackAllocExt::ptr_offset(self, ptr, idx);
        StackAllocExt::ptr_store(self, addr, vals[0].clone());
    }
}

impl StackAllocExt for WasmFuncEncoder {
    type Value = WasmVal;

    fn alloca(&mut self, elem_ty: LirType, count: usize) -> WasmVal {
        let total_bytes = elem_byte_size(&elem_ty) * (count as i32);
        let ptr = self.alloc_val(LirType::Ptr(Box::new(elem_ty)));
        self.emit_real(Instruction::GlobalGet(STACK_PTR_GLOBAL));
        self.emit_real(Instruction::LocalSet(ptr.local));
        self.emit_real(Instruction::GlobalGet(STACK_PTR_GLOBAL));
        self.emit_real(Instruction::I32Const(total_bytes));
        self.emit_real(Instruction::I32Add);
        self.emit_real(Instruction::GlobalSet(STACK_PTR_GLOBAL));
        ptr
    }

    fn ptr_load(&mut self, ptr: WasmVal, ty: LirType) -> WasmVal {
        let memarg = MemArg { offset: 0, align: 0, memory_index: 0 };
        let load = load_instr(&ty, memarg);
        self.emit_real(Instruction::LocalGet(ptr.local));
        self.emit_real(load);
        let out = self.alloc_val(ty);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }

    fn ptr_store(&mut self, ptr: WasmVal, val: WasmVal) {
        let memarg = MemArg { offset: 0, align: 0, memory_index: 0 };
        let store = store_instr(&val.ty, memarg);
        self.emit_real(Instruction::LocalGet(ptr.local));
        self.emit_real(Instruction::LocalGet(val.local));
        self.emit_real(store);
    }

    fn ptr_offset(&mut self, ptr: WasmVal, idx: WasmVal) -> WasmVal {
        let elem_ty = match &ptr.ty {
            LirType::Ptr(e) => (**e).clone(),
            other => panic!("volar-wasm-backend: ptr_offset called on non-Ptr value {other:?}"),
        };
        let elem_bytes = elem_byte_size(&elem_ty);
        let out = self.alloc_val(LirType::Ptr(Box::new(elem_ty)));
        self.emit_real(Instruction::LocalGet(ptr.local));
        self.push_coerced(&idx, false);
        self.emit_real(Instruction::I32Const(elem_bytes));
        self.emit_real(Instruction::I32Mul);
        self.emit_real(Instruction::I32Add);
        self.emit_real(Instruction::LocalSet(out.local));
        out
    }
}
