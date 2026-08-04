// @reliability: experimental
// @ai: assisted
//! WASM backend for [`LirTarget`].
//!
//! Emits a complete WASM module (`Vec<u8>`) via [`wasm_encoder`] and
//! [`wax_core::build::InstructionSink`].  SSA values and block parameters are
//! mapped to WASM locals; multi-block control flow uses a `br_table`
//! dispatcher (`locals + br`).
//!
//! # ABI
//!
//! Returns [`LirAbi::DEFAULT`]: aggregates are flattened to scalar locals /
//! parameters.  [`define_struct`](LirTarget::define_struct) records field
//! layouts for flattening.  [`StackAllocExt`] is not supported yet
//! (`stack_alloc_ext` returns `None`).
//!
//! # Arrays
//!
//! Small fixed arrays are represented as flat scalar locals.  Helper methods
//! [`arr_new`](WasmBackend::arr_new) / [`arr_get`](WasmBackend::arr_get) /
//! [`arr_set`](WasmBackend::arr_set) operate on those flat lists.

#![cfg_attr(not(test), no_std)]
extern crate alloc;

use alloc::{
    borrow::Cow,
    collections::BTreeMap,
    string::String,
    vec,
    vec::Vec,
};
use volar_ir_common::Type as NativeType;
use volar_lir::{
    BranchTarget, IcmpPred, LirAbi, LirTarget, LirType, NameConfig, StructDef, StructId,
};
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, Module, TypeSection, ValType,
};
use wax_core::build::InstructionSink;

// ============================================================================
// Handles
// ============================================================================

/// An SSA value: index into the current function's local/value table.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct WasmValue(pub u32);

/// A basic-block handle.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct WasmBlock(pub u32);

// ============================================================================
// Buffered ops (lowered at `end_function`)
// ============================================================================

#[derive(Clone, Debug)]
enum BinOpKind {
    Add,
    Sub,
    Mul,
    Udiv,
    Sdiv,
    And,
    Or,
    Xor,
    Shl,
    Lshr,
    Ashr,
}

#[derive(Clone, Debug)]
enum Op {
    Iconst {
        dest: u32,
        ty: LirType,
        val: i64,
    },
    Binop {
        dest: u32,
        kind: BinOpKind,
        lhs: u32,
        rhs: u32,
    },
    Not {
        dest: u32,
        val: u32,
    },
    Icmp {
        dest: u32,
        pred: IcmpPred,
        lhs: u32,
        rhs: u32,
    },
    Zext {
        dest: u32,
        val: u32,
        dst_ty: LirType,
    },
    Sext {
        dest: u32,
        val: u32,
        dst_ty: LirType,
    },
    Trunc {
        dest: u32,
        val: u32,
        dst_ty: LirType,
    },
    Select {
        dest: u32,
        cond: u32,
        then_val: u32,
        else_val: u32,
    },
    /// Copy `src` into local `dest` (used by arr helpers / parallel assign).
    Copy {
        dest: u32,
        src: u32,
    },
    Call {
        /// Destination locals for results (may be empty).
        dests: Vec<u32>,
        func_idx: u32,
        args: Vec<u32>,
    },
    Jump {
        target: u32,
        args: Vec<u32>,
    },
    Branch {
        cond: u32,
        then_block: u32,
        then_args: Vec<u32>,
        else_block: u32,
        else_args: Vec<u32>,
    },
    Ret {
        vals: Vec<u32>,
    },
}

struct BlockState {
    /// Local indices for this block's parameters, in order.
    param_locals: Vec<u32>,
    ops: Vec<Op>,
    /// True once a terminator has been recorded.
    terminated: bool,
}

struct ValueInfo {
    /// WASM local index holding this value.
    local: u32,
    ty: LirType,
}

struct FunctionState {
    name: String,
    /// Flattened WASM parameter types (scalars only).
    wasm_param_tys: Vec<ValType>,
    /// Flattened WASM result types (scalars only).
    wasm_result_tys: Vec<ValType>,
    values: Vec<ValueInfo>,
    /// Next WASM local index to allocate (params occupy `0..wasm_param_tys.len()`).
    next_local: u32,
    blocks: Vec<BlockState>,
    current_block: Option<u32>,
    /// Local index reserved for the PC dispatcher (`None` until first multi-block use).
    pc_local: Option<u32>,
}

impl FunctionState {
    fn alloc_value(&mut self, ty: LirType) -> WasmValue {
        let local = self.next_local;
        self.next_local += 1;
        let id = self.values.len() as u32;
        self.values.push(ValueInfo {
            local,
            ty,
        });
        WasmValue(id)
    }

    fn local_of(&self, v: WasmValue) -> u32 {
        self.values[v.0 as usize].local
    }

    fn ty_of(&self, v: WasmValue) -> &LirType {
        &self.values[v.0 as usize].ty
    }

    fn push_op(&mut self, op: Op) {
        let bid = self.current_block.expect("WasmBackend: no current block");
        let block = &mut self.blocks[bid as usize];
        assert!(
            !block.terminated,
            "WasmBackend: emitting after terminator in block {bid}"
        );
        let is_term = matches!(op, Op::Jump { .. } | Op::Branch { .. } | Op::Ret { .. });
        block.ops.push(op);
        if is_term {
            block.terminated = true;
        }
    }

    fn ensure_pc_local(&mut self) -> u32 {
        if let Some(pc) = self.pc_local {
            return pc;
        }
        let pc = self.next_local;
        self.next_local += 1;
        self.pc_local = Some(pc);
        pc
    }
}

// ============================================================================
// Completed function record
// ============================================================================

struct CompletedFunction {
    name: String,
    type_idx: u32,
    /// Encoded function body (locals + instructions, including final `end`).
    body: Function,
}

// ============================================================================
// The backend
// ============================================================================

/// WASM backend implementing [`LirTarget`].
///
/// Drive via `begin_function` / emit / `end_function`, then call [`finish`](Self::finish)
/// to obtain module bytes.
pub struct WasmBackend {
    current: Option<FunctionState>,
    completed: Vec<CompletedFunction>,

    struct_defs: Vec<StructDef>,
    next_struct_id: StructId,

    /// Deduplicated function types: key → type index.
    type_map: BTreeMap<(Vec<ValType>, Vec<ValType>), u32>,
    types: TypeSection,

    /// Import name → function index (imports occupy the low indices).
    import_map: BTreeMap<String, u32>,
    /// Parallel to import order for emission.
    imports: Vec<(String, u32)>, // (name, type_idx)
    next_func_idx: u32,

    pub name_config: NameConfig,
    /// Import module name for `call_extern` (default `"env"`).
    pub import_module: String,
    /// Import name used for `rng` (default `"volar_rng"`).
    pub rng_fn: String,
}

impl Default for WasmBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl WasmBackend {
    pub fn new() -> Self {
        WasmBackend {
            current: None,
            completed: Vec::new(),
            struct_defs: Vec::new(),
            next_struct_id: 0,
            type_map: BTreeMap::new(),
            types: TypeSection::new(),
            import_map: BTreeMap::new(),
            imports: Vec::new(),
            next_func_idx: 0,
            name_config: NameConfig::default(),
            import_module: String::from("env"),
            rng_fn: String::from("volar_rng"),
        }
    }

    pub fn with_name_config(mut self, config: NameConfig) -> Self {
        self.name_config = config;
        self
    }

    pub fn with_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.name_config.prefix = prefix.into();
        self
    }

    /// Consume the backend and return the finished WASM module bytes.
    pub fn finish(self) -> Vec<u8> {
        let mut module = Module::new();

        // Type section.
        module.section(&self.types);

        // Import section (functions from `import_module`).
        if !self.imports.is_empty() {
            let mut imports = ImportSection::new();
            for (name, type_idx) in &self.imports {
                imports.import(
                    &self.import_module,
                    name,
                    wasm_encoder::EntityType::Function(*type_idx),
                );
            }
            module.section(&imports);
        }

        // Function section (type indices for defined functions).
        if !self.completed.is_empty() {
            let mut functions = FunctionSection::new();
            for f in &self.completed {
                functions.function(f.type_idx);
            }
            module.section(&functions);
        }

        // Export section — export every defined function by name.
        if !self.completed.is_empty() {
            let mut exports = ExportSection::new();
            let import_count = self.imports.len() as u32;
            for (i, f) in self.completed.iter().enumerate() {
                exports.export(&f.name, ExportKind::Func, import_count + i as u32);
            }
            module.section(&exports);
        }

        // Code section.
        if !self.completed.is_empty() {
            let mut codes = CodeSection::new();
            for f in &self.completed {
                codes.function(&f.body);
            }
            module.section(&codes);
        }

        module.finish()
    }

    // ---- Array helpers (flat scalar representation) -------------------------

    /// Copy `elems` into fresh locals — the "array" is the returned flat list.
    pub fn arr_new(&mut self, elems: &[WasmValue]) -> Vec<WasmValue> {
        elems
            .iter()
            .map(|&e| {
                let ty = self.state().ty_of(e).clone();
                let dest = self.state().alloc_value(ty);
                let src_local = self.state().local_of(e);
                let dest_local = self.state().local_of(dest);
                self.state().push_op(Op::Copy {
                    dest: dest_local,
                    src: src_local,
                });
                dest
            })
            .collect()
    }

    /// Read one element of width `elem_width` at constant `idx`.
    pub fn arr_get(&mut self, arr: &[WasmValue], elem_width: usize, idx: usize) -> Vec<WasmValue> {
        let n = arr.len() / elem_width;
        assert!(idx < n, "arr_get: index {idx} out of bounds (len {n})");
        let start = idx * elem_width;
        self.arr_new(&arr[start..start + elem_width])
    }

    /// Functional update: replace element `idx` (width `elem_width`) with `vals`.
    pub fn arr_set(
        &mut self,
        arr: &[WasmValue],
        elem_width: usize,
        idx: usize,
        vals: &[WasmValue],
    ) -> Vec<WasmValue> {
        assert_eq!(vals.len(), elem_width, "arr_set: vals width mismatch");
        let n = arr.len() / elem_width;
        assert!(idx < n, "arr_set: index {idx} out of bounds (len {n})");
        let mut out = self.arr_new(arr);
        let start = idx * elem_width;
        for (i, &v) in vals.iter().enumerate() {
            let ty = self.state().ty_of(v).clone();
            let dest = self.state().alloc_value(ty);
            let src_local = self.state().local_of(v);
            let dest_local = self.state().local_of(dest);
            self.state().push_op(Op::Copy {
                dest: dest_local,
                src: src_local,
            });
            out[start + i] = dest;
        }
        out
    }

    // ---- Internal helpers ---------------------------------------------------

    fn state(&mut self) -> &mut FunctionState {
        self.current
            .as_mut()
            .expect("WasmBackend: not inside a function")
    }

    fn intern_type(&mut self, params: &[ValType], results: &[ValType]) -> u32 {
        let key = (params.to_vec(), results.to_vec());
        if let Some(&idx) = self.type_map.get(&key) {
            return idx;
        }
        let idx = self.type_map.len() as u32;
        self.types
            .ty()
            .function(params.iter().copied(), results.iter().copied());
        self.type_map.insert(key, idx);
        idx
    }

    fn ensure_import(&mut self, name: &str, params: &[ValType], results: &[ValType]) -> u32 {
        if let Some(&idx) = self.import_map.get(name) {
            return idx;
        }
        let type_idx = self.intern_type(params, results);
        let func_idx = self.next_func_idx;
        self.next_func_idx += 1;
        self.import_map.insert(String::from(name), func_idx);
        self.imports.push((String::from(name), type_idx));
        func_idx
    }

    fn flatten_scalar_tys(&self, ty: &LirType) -> Vec<LirType> {
        match ty {
            LirType::Arr(elem, n) => {
                let mut out = Vec::new();
                for _ in 0..*n {
                    out.extend(self.flatten_scalar_tys(elem));
                }
                out
            }
            LirType::Struct(id) => {
                let mut out = Vec::new();
                for field in &self.struct_defs[*id as usize].fields {
                    out.extend(self.flatten_scalar_tys(&field.ty));
                }
                out
            }
            _ => vec![ty.clone()],
        }
    }

    fn lir_to_val_ty(ty: &LirType) -> ValType {
        match ty {
            LirType::Bool
            | LirType::I8
            | LirType::U8
            | LirType::I16
            | LirType::U16
            | LirType::I32
            | LirType::U32 => ValType::I32,
            LirType::I64 | LirType::U64 => ValType::I64,
            LirType::I128 | LirType::U128 => {
                panic!("WasmBackend: i128/u128 not supported yet")
            }
            LirType::Native(t) => match t {
                NativeType::Bit
                | NativeType::_8
                | NativeType::AES8
                | NativeType::_16
                | NativeType::_32 => ValType::I32,
                NativeType::_64 | NativeType::Galois64 => ValType::I64,
                NativeType::_128 => panic!("WasmBackend: Native::_128 not supported yet"),
                _ => ValType::I64,
            },
            LirType::Arr(_, _) | LirType::Struct(_) => {
                panic!("WasmBackend: aggregate passed to lir_to_val_ty; flatten first")
            }
            LirType::Ptr(_) => {
                panic!("WasmBackend: Ptr not supported (no StackAllocExt)")
            }
            _ => panic!("WasmBackend: unsupported LirType {ty:?}"),
        }
    }

    fn is_i64(ty: &LirType) -> bool {
        matches!(Self::lir_to_val_ty(ty), ValType::I64)
    }

    fn binop(&mut self, lhs: WasmValue, rhs: WasmValue, kind: BinOpKind) -> WasmValue {
        let ty = self.state().ty_of(lhs).clone();
        let dest = self.state().alloc_value(ty);
        let dest_l = self.state().local_of(dest);
        let lhs_l = self.state().local_of(lhs);
        let rhs_l = self.state().local_of(rhs);
        self.state().push_op(Op::Binop {
            dest: dest_l,
            kind,
            lhs: lhs_l,
            rhs: rhs_l,
        });
        dest
    }
}

// ============================================================================
// Lowering buffered ops → wasm_encoder::Function
// ============================================================================

/// Declare each extra local individually (groups of size 1) in allocation
/// order so WASM local indices match our monotonic allocator.
fn local_decls_in_alloc_order(state: &FunctionState) -> Vec<(u32, ValType)> {
    let param_count = state.wasm_param_tys.len() as u32;
    let mut decls = Vec::new();
    // Build a map local_idx → ValType for every non-param local.
    let mut local_ty: BTreeMap<u32, ValType> = BTreeMap::new();
    for v in &state.values {
        if v.local >= param_count {
            local_ty.insert(v.local, WasmBackend::lir_to_val_ty(&v.ty));
        }
    }
    if let Some(pc) = state.pc_local {
        if pc >= param_count {
            local_ty.insert(pc, ValType::I32);
        }
    }
    for (_idx, vt) in local_ty {
        decls.push((1, vt));
    }
    decls
}

/// Emit via [`InstructionSink`] so we go through wax-core (not the inherent
/// `Function::instruction` method, which has a different signature).
fn emit(func: &mut Function, instr: Instruction<'_>) {
    InstructionSink::<(), ()>::instruction(func, &mut (), &instr).unwrap();
}

fn emit_const(func: &mut Function, ty: &LirType, val: i64) {
    match WasmBackend::lir_to_val_ty(ty) {
        ValType::I32 => emit(func, Instruction::I32Const(val as i32)),
        ValType::I64 => emit(func, Instruction::I64Const(val)),
        _ => panic!("WasmBackend: unsupported const type"),
    }
}

fn emit_binop_instr(func: &mut Function, kind: &BinOpKind, is_i64: bool) {
    let instr = match (kind, is_i64) {
        (BinOpKind::Add, false) => Instruction::I32Add,
        (BinOpKind::Add, true) => Instruction::I64Add,
        (BinOpKind::Sub, false) => Instruction::I32Sub,
        (BinOpKind::Sub, true) => Instruction::I64Sub,
        (BinOpKind::Mul, false) => Instruction::I32Mul,
        (BinOpKind::Mul, true) => Instruction::I64Mul,
        (BinOpKind::Udiv, false) => Instruction::I32DivU,
        (BinOpKind::Udiv, true) => Instruction::I64DivU,
        (BinOpKind::Sdiv, false) => Instruction::I32DivS,
        (BinOpKind::Sdiv, true) => Instruction::I64DivS,
        (BinOpKind::And, false) => Instruction::I32And,
        (BinOpKind::And, true) => Instruction::I64And,
        (BinOpKind::Or, false) => Instruction::I32Or,
        (BinOpKind::Or, true) => Instruction::I64Or,
        (BinOpKind::Xor, false) => Instruction::I32Xor,
        (BinOpKind::Xor, true) => Instruction::I64Xor,
        (BinOpKind::Shl, false) => Instruction::I32Shl,
        (BinOpKind::Shl, true) => Instruction::I64Shl,
        (BinOpKind::Lshr, false) => Instruction::I32ShrU,
        (BinOpKind::Lshr, true) => Instruction::I64ShrU,
        (BinOpKind::Ashr, false) => Instruction::I32ShrS,
        (BinOpKind::Ashr, true) => Instruction::I64ShrS,
    };
    emit(func, instr);
}

fn emit_icmp_instr(func: &mut Function, pred: IcmpPred, is_i64: bool) {
    let instr = match (pred, is_i64) {
        (IcmpPred::Eq, false) => Instruction::I32Eq,
        (IcmpPred::Eq, true) => Instruction::I64Eq,
        (IcmpPred::Ne, false) => Instruction::I32Ne,
        (IcmpPred::Ne, true) => Instruction::I64Ne,
        (IcmpPred::Ult, false) => Instruction::I32LtU,
        (IcmpPred::Ult, true) => Instruction::I64LtU,
        (IcmpPred::Ule, false) => Instruction::I32LeU,
        (IcmpPred::Ule, true) => Instruction::I64LeU,
        (IcmpPred::Ugt, false) => Instruction::I32GtU,
        (IcmpPred::Ugt, true) => Instruction::I64GtU,
        (IcmpPred::Uge, false) => Instruction::I32GeU,
        (IcmpPred::Uge, true) => Instruction::I64GeU,
        (IcmpPred::Slt, false) => Instruction::I32LtS,
        (IcmpPred::Slt, true) => Instruction::I64LtS,
        (IcmpPred::Sle, false) => Instruction::I32LeS,
        (IcmpPred::Sle, true) => Instruction::I64LeS,
        (IcmpPred::Sgt, false) => Instruction::I32GtS,
        (IcmpPred::Sgt, true) => Instruction::I64GtS,
        (IcmpPred::Sge, false) => Instruction::I32GeS,
        (IcmpPred::Sge, true) => Instruction::I64GeS,
    };
    emit(func, instr);
}

fn local_ty_lookup(state: &FunctionState, local: u32) -> LirType {
    for v in &state.values {
        if v.local == local {
            return v.ty.clone();
        }
    }
    if state.pc_local == Some(local) {
        return LirType::I32;
    }
    panic!("WasmBackend: unknown local {local}");
}

fn emit_op(func: &mut Function, state: &FunctionState, op: &Op) {
    match op {
        Op::Iconst { dest, ty, val } => {
            emit_const(func, ty, *val);
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Binop {
            dest,
            kind,
            lhs,
            rhs,
        } => {
            let ty = local_ty_lookup(state, *lhs);
            let i64 = WasmBackend::is_i64(&ty);
            emit(func, Instruction::LocalGet(*lhs));
            emit(func, Instruction::LocalGet(*rhs));
            emit_binop_instr(func, kind, i64);
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Not { dest, val } => {
            let ty = local_ty_lookup(state, *val);
            emit(func, Instruction::LocalGet(*val));
            if ty == LirType::Bool {
                emit(func, Instruction::I32Const(1));
                emit(func, Instruction::I32Xor);
            } else if WasmBackend::is_i64(&ty) {
                emit(func, Instruction::I64Const(-1));
                emit(func, Instruction::I64Xor);
            } else {
                emit(func, Instruction::I32Const(-1));
                emit(func, Instruction::I32Xor);
            }
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Icmp {
            dest,
            pred,
            lhs,
            rhs,
        } => {
            let ty = local_ty_lookup(state, *lhs);
            let i64 = WasmBackend::is_i64(&ty);
            emit(func, Instruction::LocalGet(*lhs));
            emit(func, Instruction::LocalGet(*rhs));
            emit_icmp_instr(func, *pred, i64);
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Zext { dest, val, dst_ty } => {
            let src_ty = local_ty_lookup(state, *val);
            emit(func, Instruction::LocalGet(*val));
            match (
                WasmBackend::lir_to_val_ty(&src_ty),
                WasmBackend::lir_to_val_ty(dst_ty),
            ) {
                (ValType::I32, ValType::I64) => {
                    emit(func, Instruction::I64ExtendI32U);
                }
                (ValType::I32, ValType::I32) => {
                    let bits = src_ty.bit_width().min(32);
                    if bits < 32 && src_ty != LirType::Bool {
                        let mask = (1i32 << bits) - 1;
                        emit(func, Instruction::I32Const(mask));
                        emit(func, Instruction::I32And);
                    }
                }
                (ValType::I64, ValType::I64) => {}
                _ => {}
            }
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Sext { dest, val, dst_ty } => {
            let src_ty = local_ty_lookup(state, *val);
            emit(func, Instruction::LocalGet(*val));
            match (
                WasmBackend::lir_to_val_ty(&src_ty),
                WasmBackend::lir_to_val_ty(dst_ty),
            ) {
                (ValType::I32, ValType::I64) => {
                    emit(func, Instruction::I64ExtendI32S);
                }
                _ => {
                    let bits = src_ty.bit_width().min(32);
                    if bits < 32 {
                        let sh = 32 - bits;
                        emit(func, Instruction::I32Const(sh as i32));
                        emit(func, Instruction::I32Shl);
                        emit(func, Instruction::I32Const(sh as i32));
                        emit(func, Instruction::I32ShrS);
                    }
                }
            }
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Trunc { dest, val, dst_ty } => {
            let src_ty = local_ty_lookup(state, *val);
            emit(func, Instruction::LocalGet(*val));
            match (
                WasmBackend::lir_to_val_ty(&src_ty),
                WasmBackend::lir_to_val_ty(dst_ty),
            ) {
                (ValType::I64, ValType::I32) => {
                    emit(func, Instruction::I32WrapI64);
                }
                (ValType::I32, ValType::I32) => {
                    let bits = dst_ty.bit_width().min(32);
                    if bits < 32 {
                        let mask = (1i32 << bits) - 1;
                        emit(func, Instruction::I32Const(mask));
                        emit(func, Instruction::I32And);
                    }
                }
                _ => {}
            }
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Select {
            dest,
            cond,
            then_val,
            else_val,
        } => {
            emit(func, Instruction::LocalGet(*then_val));
            emit(func, Instruction::LocalGet(*else_val));
            emit(func, Instruction::LocalGet(*cond));
            emit(func, Instruction::Select);
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Copy { dest, src } => {
            emit(func, Instruction::LocalGet(*src));
            emit(func, Instruction::LocalSet(*dest));
        }
        Op::Call {
            dests,
            func_idx,
            args,
        } => {
            for &a in args {
                emit(func, Instruction::LocalGet(a));
            }
            emit(func, Instruction::Call(*func_idx));
            for &d in dests.iter().rev() {
                emit(func, Instruction::LocalSet(d));
            }
        }
        Op::Jump { .. } | Op::Branch { .. } | Op::Ret { .. } => {
            panic!("emit_op: terminators must be lowered by emit_terminator");
        }
    }
}

fn emit_assign_block_params(
    func: &mut Function,
    state: &FunctionState,
    target: u32,
    args: &[u32],
) {
    let params = &state.blocks[target as usize].param_locals;
    assert_eq!(
        params.len(),
        args.len(),
        "jump arg count mismatch for block {target}"
    );
    for &a in args {
        emit(func, Instruction::LocalGet(a));
    }
    for &p in params.iter().rev() {
        emit(func, Instruction::LocalSet(p));
    }
}

fn emit_terminator(
    func: &mut Function,
    state: &FunctionState,
    op: &Op,
    dispatch_depth: u32,
) {
    match op {
        Op::Ret { vals } => {
            for &v in vals {
                emit(func, Instruction::LocalGet(v));
            }
            emit(func, Instruction::Return);
        }
        Op::Jump { target, args } => {
            emit_assign_block_params(func, state, *target, args);
            let pc = state.pc_local.expect("pc local");
            emit(func, Instruction::I32Const(*target as i32));
            emit(func, Instruction::LocalSet(pc));
            emit(func, Instruction::Br(dispatch_depth));
        }
        Op::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
        } => {
            let pc = state.pc_local.expect("pc local");
            emit(func, Instruction::LocalGet(*cond));
            emit(func, Instruction::If(wasm_encoder::BlockType::Empty));
            emit_assign_block_params(func, state, *then_block, then_args);
            emit(func, Instruction::I32Const(*then_block as i32));
            emit(func, Instruction::LocalSet(pc));
            emit(func, Instruction::Else);
            emit_assign_block_params(func, state, *else_block, else_args);
            emit(func, Instruction::I32Const(*else_block as i32));
            emit(func, Instruction::LocalSet(pc));
            emit(func, Instruction::End);
            emit(func, Instruction::Br(dispatch_depth));
        }
        _ => panic!("not a terminator"),
    }
}

fn lower_function(state: &FunctionState) -> Function {
    let decls = local_decls_in_alloc_order(state);
    let mut func = Function::new(decls);
    let nblocks = state.blocks.len() as u32;
    let multi = nblocks > 1 || state.pc_local.is_some();

    if !multi {
        let block = &state.blocks[0];
        for op in &block.ops {
            if matches!(op, Op::Jump { .. } | Op::Branch { .. } | Op::Ret { .. }) {
                match op {
                    Op::Ret { vals } => {
                        for &v in vals {
                            emit(&mut func, Instruction::LocalGet(v));
                        }
                        emit(&mut func, Instruction::Return);
                    }
                    _ => panic!("WasmBackend: multi-block terminator in single-block function"),
                }
            } else {
                emit_op(&mut func, state, op);
            }
        }
        InstructionSink::<(), ()>::finish(&mut func).unwrap();
        return func;
    }

    let pc = state.ensure_pc_local_immut();
    emit(&mut func, Instruction::I32Const(0));
    emit(&mut func, Instruction::LocalSet(pc));

    // loop $dispatch
    //   block $default
    //     block $b_{n-1} ... block $b_0
    //       br_table ...
    //     end ;; fall into b0 body
    //     ...
    //   end ;; default → unreachable
    // end ;; dispatch
    emit(
        &mut func,
        Instruction::Loop(wasm_encoder::BlockType::Empty),
    ); // $dispatch
    emit(
        &mut func,
        Instruction::Block(wasm_encoder::BlockType::Empty),
    ); // $default
    for _ in 0..nblocks {
        emit(
            &mut func,
            Instruction::Block(wasm_encoder::BlockType::Empty),
        );
    }

    let targets: Vec<u32> = (0..nblocks).collect();
    let default_target = nblocks;
    emit(&mut func, Instruction::LocalGet(pc));
    emit(
        &mut func,
        Instruction::BrTable(Cow::Owned(targets), default_target),
    );

    for bid in 0..nblocks {
        emit(&mut func, Instruction::End);
        let block = &state.blocks[bid as usize];
        for op in &block.ops {
            if matches!(op, Op::Jump { .. } | Op::Branch { .. } | Op::Ret { .. }) {
                // Depth to $dispatch: remaining b_* wrappers + $default.
                let dispatch_depth = nblocks - bid;
                emit_terminator(&mut func, state, op, dispatch_depth);
            } else {
                emit_op(&mut func, state, op);
            }
        }
        if !block.terminated {
            emit(&mut func, Instruction::Unreachable);
        }
    }

    emit(&mut func, Instruction::End); // $default
    emit(&mut func, Instruction::Unreachable);
    emit(&mut func, Instruction::End); // $dispatch
    // All real paths `return`; mark fallthrough unreachable so the function
    // result type validates.
    emit(&mut func, Instruction::Unreachable);

    InstructionSink::<(), ()>::finish(&mut func).unwrap();
    func
}

impl FunctionState {
    /// Read pc local; panics if missing (multi-block path must have allocated it).
    fn ensure_pc_local_immut(&self) -> u32 {
        self.pc_local
            .expect("WasmBackend: pc local missing for multi-block function")
    }
}

// ============================================================================
// LirTarget impl
// ============================================================================

impl LirTarget for WasmBackend {
    type Value = WasmValue;
    type Block = WasmBlock;

    fn abi(&self) -> LirAbi {
        LirAbi::DEFAULT
    }

    fn define_struct(&mut self, def: StructDef) -> StructId {
        let id = self.next_struct_id;
        self.next_struct_id += 1;
        self.struct_defs.push(def);
        id
    }

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (WasmBlock, Vec<Vec<WasmValue>>) {
        assert!(
            self.current.is_none(),
            "begin_function called while already inside a function"
        );

        let flat_param_groups: Vec<Vec<LirType>> = params
            .iter()
            .map(|ty| self.flatten_scalar_tys(ty))
            .collect();
        let flat_params: Vec<LirType> = flat_param_groups.iter().flatten().cloned().collect();
        let wasm_param_tys: Vec<ValType> = flat_params.iter().map(Self::lir_to_val_ty).collect();

        let flat_rets = ret
            .as_ref()
            .map(|ty| self.flatten_scalar_tys(ty))
            .unwrap_or_default();
        let wasm_result_tys: Vec<ValType> = flat_rets.iter().map(Self::lir_to_val_ty).collect();

        let mut state = FunctionState {
            name: self.name_config.apply(name),
            wasm_param_tys: wasm_param_tys.clone(),
            wasm_result_tys,
            values: Vec::new(),
            next_local: 0,
            blocks: vec![BlockState {
                param_locals: Vec::new(),
                ops: Vec::new(),
                terminated: false,
            }],
            current_block: None,
            pc_local: None,
        };

        // Allocate parameter values as locals 0..n-1.
        let mut param_vals: Vec<Vec<WasmValue>> = Vec::new();
        for group in &flat_param_groups {
            let mut g = Vec::new();
            for ty in group {
                let local = state.next_local;
                state.next_local += 1;
                let id = state.values.len() as u32;
                state.values.push(ValueInfo {
                    local,
                    ty: ty.clone(),
                });
                g.push(WasmValue(id));
            }
            param_vals.push(g);
        }

        self.current = Some(state);
        (WasmBlock(0), param_vals)
    }

    fn end_function(&mut self) {
        let mut state = self
            .current
            .take()
            .expect("end_function called outside a function");

        // Multi-block functions need a PC local for the dispatcher.
        if state.blocks.len() > 1 {
            state.ensure_pc_local();
        }

        let type_idx = self.intern_type(&state.wasm_param_tys, &state.wasm_result_tys);
        // Defined functions follow imports in the function index space.
        // next_func_idx tracks imports only until we start completing functions;
        // we don't need the defined func index until export time.
        let body = lower_function(&state);
        self.completed.push(CompletedFunction {
            name: state.name,
            type_idx,
            body,
        });
    }

    fn create_block(&mut self) -> WasmBlock {
        let state = self.state();
        let id = state.blocks.len() as u32;
        state.blocks.push(BlockState {
            param_locals: Vec::new(),
            ops: Vec::new(),
            terminated: false,
        });
        // Ensure PC exists once we have multiple blocks.
        if state.blocks.len() > 1 {
            state.ensure_pc_local();
        }
        WasmBlock(id)
    }

    fn add_block_param(&mut self, block: WasmBlock, ty: LirType) -> WasmValue {
        let v = self.state().alloc_value(ty);
        let local = self.state().local_of(v);
        self.state().blocks[block.0 as usize]
            .param_locals
            .push(local);
        v
    }

    fn switch_to_block(&mut self, block: WasmBlock) {
        self.state().current_block = Some(block.0);
    }

    fn iconst(&mut self, ty: LirType, val: i64) -> WasmValue {
        let dest = self.state().alloc_value(ty.clone());
        let dest_l = self.state().local_of(dest);
        self.state().push_op(Op::Iconst {
            dest: dest_l,
            ty,
            val,
        });
        dest
    }

    fn add(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Add)
    }
    fn sub(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Sub)
    }
    fn mul(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Mul)
    }
    fn udiv(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Udiv)
    }
    fn sdiv(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Sdiv)
    }

    fn and(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::And)
    }
    fn or(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Or)
    }
    fn xor(&mut self, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        self.binop(lhs, rhs, BinOpKind::Xor)
    }
    fn not(&mut self, val: WasmValue) -> WasmValue {
        let ty = self.state().ty_of(val).clone();
        let dest = self.state().alloc_value(ty);
        let dest_l = self.state().local_of(dest);
        let val_l = self.state().local_of(val);
        self.state().push_op(Op::Not {
            dest: dest_l,
            val: val_l,
        });
        dest
    }
    fn shl(&mut self, val: WasmValue, shift: WasmValue) -> WasmValue {
        self.binop(val, shift, BinOpKind::Shl)
    }
    fn lshr(&mut self, val: WasmValue, shift: WasmValue) -> WasmValue {
        self.binop(val, shift, BinOpKind::Lshr)
    }
    fn ashr(&mut self, val: WasmValue, shift: WasmValue) -> WasmValue {
        self.binop(val, shift, BinOpKind::Ashr)
    }

    fn icmp(&mut self, pred: IcmpPred, lhs: WasmValue, rhs: WasmValue) -> WasmValue {
        let dest = self.state().alloc_value(LirType::Bool);
        let dest_l = self.state().local_of(dest);
        let lhs_l = self.state().local_of(lhs);
        let rhs_l = self.state().local_of(rhs);
        self.state().push_op(Op::Icmp {
            dest: dest_l,
            pred,
            lhs: lhs_l,
            rhs: rhs_l,
        });
        dest
    }

    fn zext(&mut self, val: WasmValue, dst_ty: LirType) -> WasmValue {
        let dest = self.state().alloc_value(dst_ty.clone());
        let dest_l = self.state().local_of(dest);
        let val_l = self.state().local_of(val);
        self.state().push_op(Op::Zext {
            dest: dest_l,
            val: val_l,
            dst_ty,
        });
        dest
    }
    fn sext(&mut self, val: WasmValue, dst_ty: LirType) -> WasmValue {
        let dest = self.state().alloc_value(dst_ty.clone());
        let dest_l = self.state().local_of(dest);
        let val_l = self.state().local_of(val);
        self.state().push_op(Op::Sext {
            dest: dest_l,
            val: val_l,
            dst_ty,
        });
        dest
    }
    fn trunc(&mut self, val: WasmValue, dst_ty: LirType) -> WasmValue {
        let dest = self.state().alloc_value(dst_ty.clone());
        let dest_l = self.state().local_of(dest);
        let val_l = self.state().local_of(val);
        self.state().push_op(Op::Trunc {
            dest: dest_l,
            val: val_l,
            dst_ty,
        });
        dest
    }

    fn select(
        &mut self,
        cond: WasmValue,
        then_val: WasmValue,
        else_val: WasmValue,
    ) -> WasmValue {
        let ty = self.state().ty_of(then_val).clone();
        let dest = self.state().alloc_value(ty);
        let dest_l = self.state().local_of(dest);
        let cond_l = self.state().local_of(cond);
        let then_l = self.state().local_of(then_val);
        let else_l = self.state().local_of(else_val);
        self.state().push_op(Op::Select {
            dest: dest_l,
            cond: cond_l,
            then_val: then_l,
            else_val: else_l,
        });
        dest
    }

    fn value_scalar_type(&self, val: &WasmValue) -> LirType {
        self.current
            .as_ref()
            .expect("value_scalar_type: not inside a function")
            .values[val.0 as usize]
            .ty
            .clone()
    }

    fn call_extern(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[WasmValue],
        ret_ty: Option<LirType>,
    ) -> Vec<WasmValue> {
        let flat_arg_tys: Vec<LirType> = arg_tys
            .iter()
            .flat_map(|ty| self.flatten_scalar_tys(ty))
            .collect();
        assert_eq!(
            flat_arg_tys.len(),
            args.len(),
            "call_extern: flat arg count mismatch"
        );
        let param_vts: Vec<ValType> = flat_arg_tys.iter().map(Self::lir_to_val_ty).collect();
        let flat_rets = ret_ty
            .as_ref()
            .map(|ty| self.flatten_scalar_tys(ty))
            .unwrap_or_default();
        let result_vts: Vec<ValType> = flat_rets.iter().map(Self::lir_to_val_ty).collect();

        let resolved = self.name_config.apply(name);
        let func_idx = self.ensure_import(&resolved, &param_vts, &result_vts);

        let mut dests = Vec::new();
        let mut dest_locals = Vec::new();
        for ty in &flat_rets {
            let v = self.state().alloc_value(ty.clone());
            dest_locals.push(self.state().local_of(v));
            dests.push(v);
        }
        let arg_locals: Vec<u32> = args.iter().map(|a| self.state().local_of(*a)).collect();
        self.state().push_op(Op::Call {
            dests: dest_locals,
            func_idx,
            args: arg_locals,
        });
        dests
    }

    fn jump(&mut self, target: WasmBlock, branch: BranchTarget<WasmValue>) {
        let args: Vec<u32> = branch
            .args
            .iter()
            .map(|a| self.state().local_of(*a))
            .collect();
        self.state().push_op(Op::Jump {
            target: target.0,
            args,
        });
    }

    fn branch(
        &mut self,
        cond: WasmValue,
        then_block: WasmBlock,
        then_branch: BranchTarget<WasmValue>,
        else_block: WasmBlock,
        else_branch: BranchTarget<WasmValue>,
    ) {
        let cond_l = self.state().local_of(cond);
        let then_args: Vec<u32> = then_branch
            .args
            .iter()
            .map(|a| self.state().local_of(*a))
            .collect();
        let else_args: Vec<u32> = else_branch
            .args
            .iter()
            .map(|a| self.state().local_of(*a))
            .collect();
        self.state().push_op(Op::Branch {
            cond: cond_l,
            then_block: then_block.0,
            then_args,
            else_block: else_block.0,
            else_args,
        });
    }

    fn ret(&mut self, vals: &[WasmValue]) {
        let locals: Vec<u32> = vals.iter().map(|v| self.state().local_of(*v)).collect();
        self.state().push_op(Op::Ret { vals: locals });
    }

    fn oracle(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[WasmValue],
        ret_tys: &[LirType],
    ) -> Vec<WasmValue> {
        let ret_ty = ret_tys.first().cloned();
        self.call_extern(&alloc::format!("oracle_{name}"), arg_tys, args, ret_ty)
    }

    fn action(
        &mut self,
        name: &str,
        guard: WasmValue,
        arg_tys: &[LirType],
        args: &[WasmValue],
        fallbacks: &[WasmValue],
        ret_tys: &[LirType],
    ) -> Vec<WasmValue> {
        let ret_ty = ret_tys.first().cloned();
        let results = self.call_extern(&alloc::format!("action_{name}"), arg_tys, args, ret_ty);
        results
            .iter()
            .zip(fallbacks.iter())
            .map(|(&r, &fb)| self.select(guard, r, fb))
            .collect()
    }

    fn rng(&mut self, ty: LirType) -> WasmValue {
        // Import: `rng_fn(out_ptr: i32, len: i32)` is awkward without memory.
        // MVP: import a function that returns the scalar directly.
        let name = self.rng_fn.clone();
        let vt = Self::lir_to_val_ty(&ty);
        let func_idx = self.ensure_import(&name, &[], &[vt]);
        let dest = self.state().alloc_value(ty);
        let dest_l = self.state().local_of(dest);
        self.state().push_op(Op::Call {
            dests: vec![dest_l],
            func_idx,
            args: vec![],
        });
        dest
    }

    // StackAllocExt intentionally unsupported for now.
}
