//! WASM `LirTarget` backend, built on `wasm_encoder`.
//!
//! Symmetric to `volar-c-backend`/`volar-llvm-backend`, with one difference
//! neither of those needs to handle: WASM's binary format is index-based (a
//! `call` instruction names its target by numeric `funcidx`, and the
//! function index space is fixed by declaration order -- imports first, then
//! module-defined functions). `WasmBackend` resolves every callee -- sibling
//! function, oracle, action, or RNG import -- to a final index *before* any
//! function body is lowered (via the `declare_import`/`declare_function`
//! hooks on `LirTarget`, driven automatically by `volar-lir-codegen`'s module
//! driver), so a function can call another one that hasn't been lowered yet.
//!
//! That eager index allocation is also what makes background encoding safe:
//! each function's actual `wasm_encoder` byte-level encoding (the expensive
//! part for large functions) can run on a worker thread -- see
//! [`WasmExecutor`] -- while the driver keeps lowering the next function,
//! because every `call` target a background job might need is already a
//! plain, stable number by the time that job starts.
//!
//! See `docs/wasm-backend.md` for the full design and current scope.

mod encoder;
mod executor;
mod registry;

use std::sync::{Arc, Condvar, Mutex};

use volar_lir::{BranchTarget, IcmpPred, LirAbi, LirTarget, LirType, StackAllocExt, StructDef, StructId};
use volar_lir_saved::RecordingTarget;
use wasm_encoder::{
    CodeSection, ConstExpr, EntityType, ExportKind, ExportSection, Function, FunctionSection, GlobalSection,
    GlobalType, ImportSection, MemorySection, MemoryType, Module, TypeSection, ValType,
};

pub use executor::{InlineExecutor, ThreadPerJobExecutor, WasmExecutor};

use encoder::WasmFuncEncoder;
use registry::FuncRegistry;

/// Fixed registry key for the RNG import -- see [`WasmBackend::with_rng_fn`].
pub(crate) const RNG_IMPORT_NAME: &str = "volar_rng";

const DEFAULT_INITIAL_PAGES: u64 = 1;
const DEFAULT_STACK_BASE: i32 = 16;

#[derive(Clone, Copy)]
struct MemoryConfig {
    initial_pages: u64,
    max_pages: Option<u64>,
    stack_base: i32,
}

impl Default for MemoryConfig {
    fn default() -> Self {
        MemoryConfig { initial_pages: DEFAULT_INITIAL_PAGES, max_pages: None, stack_base: DEFAULT_STACK_BASE }
    }
}

/// A `LirTarget` implementation that emits a WASM binary module.
///
/// Function bodies are recorded cheaply on the calling thread (via a
/// per-function [`RecordingTarget`]) and handed to a [`WasmExecutor`] at
/// `end_function`, so the actual instruction-encoding work can happen in the
/// background. The default executor ([`InlineExecutor`]) runs inline --
/// background encoding is strictly opt-in via [`WasmBackend::with_executor`].
pub struct WasmBackend {
    registry: Arc<FuncRegistry>,
    executor: Arc<dyn WasmExecutor>,
    slots: Arc<Mutex<Vec<Option<Function>>>>,
    pending: Arc<(Mutex<usize>, Condvar)>,
    current: Option<(u32, RecordingTarget)>,
    memory: MemoryConfig,
    exports: Vec<(String, u32)>,
    import_module: String,
}

impl Default for WasmBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl WasmBackend {
    pub fn new() -> Self {
        WasmBackend {
            registry: Arc::new(FuncRegistry::default()),
            executor: Arc::new(InlineExecutor),
            slots: Arc::new(Mutex::new(Vec::new())),
            pending: Arc::new((Mutex::new(0), Condvar::new())),
            current: None,
            memory: MemoryConfig::default(),
            exports: Vec::new(),
            import_module: "env".to_string(),
        }
    }

    /// Run function-body encoding through `executor` instead of inline.
    pub fn with_executor(mut self, executor: impl WasmExecutor + 'static) -> Self {
        self.executor = Arc::new(executor);
        self
    }

    /// Override the linear memory's initial/maximum page counts (default: 1
    /// initial page, unbounded growth). Only meaningful if the module uses
    /// `StackAllocExt` (`alloca`/`ptr_load`/`ptr_store`/`ptr_offset`).
    pub fn with_memory_pages(mut self, initial: u64, max: Option<u64>) -> Self {
        self.memory.initial_pages = initial;
        self.memory.max_pages = max;
        self
    }

    /// Set the WASM import module name used for every import (default `"env"`).
    pub fn with_import_module(mut self, name: impl Into<String>) -> Self {
        self.import_module = name.into();
        self
    }

    /// Export a declared function under `name`, using its own LIR name as
    /// the export name too (call after the function has been declared, e.g.
    /// via the automatic `declare_function` driver hook).
    pub fn with_export(mut self, name: impl Into<String>) -> Self {
        let name = name.into();
        let idx = self.registry.index_of(&name);
        self.exports.push((name, idx));
        self
    }

    /// Declare an imported function (an oracle, action, or other genuine
    /// host-provided function) before lowering starts. Must be called before
    /// any function body is declared -- imports always occupy the lowest
    /// indices in WASM's function index space.
    ///
    /// Not needed for oracle/action declarations that came from an `IrModule`
    /// -- `volar-lir-codegen`'s module driver already calls the equivalent
    /// `LirTarget::declare_import` hook automatically. This is for manual/
    /// test harnesses that drive `WasmBackend` without going through that
    /// driver.
    pub fn with_import(mut self, name: &str, params: &[LirType], ret: Option<LirType>) -> Self {
        LirTarget::declare_import(&mut self, name, params, ret);
        self
    }

    /// Configure the imported RNG function (registry key fixed to
    /// `"volar_rng"`, matching `CBackend`/`LlvmBackend`'s default `rng_fn`).
    /// Must be called before any function body is declared. Required only if
    /// the module actually calls `LirTarget::rng`.
    pub fn with_rng_fn(mut self, ret_ty: LirType) -> Self {
        LirTarget::declare_import(&mut self, RNG_IMPORT_NAME, &[], Some(ret_ty));
        self
    }

    fn current_mut(&mut self) -> &mut RecordingTarget {
        &mut self.current.as_mut().expect("WasmBackend: not inside a function (call begin_function first)").1
    }

    /// Wait for all in-flight background encoding jobs, then assemble and
    /// return the finished `.wasm` binary.
    pub fn finish(self) -> Vec<u8> {
        {
            let (lock, cvar) = &*self.pending;
            let mut count = lock.lock().unwrap();
            while *count > 0 {
                count = cvar.wait(count).unwrap();
            }
        }

        let registry = &*self.registry;
        let slots = self.slots.lock().unwrap();

        let mut types = TypeSection::new();
        for (params, results) in registry.type_table() {
            types.ty().function(params.iter().cloned(), results.iter().cloned());
        }

        let mut imports = ImportSection::new();
        for (name, type_idx) in &registry.imports {
            imports.import(&self.import_module, name, EntityType::Function(*type_idx));
        }

        let mut functions = FunctionSection::new();
        for (_, type_idx) in &registry.defined {
            functions.function(*type_idx);
        }

        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: self.memory.initial_pages,
            maximum: self.memory.max_pages,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType { val_type: ValType::I32, mutable: true, shared: false },
            &ConstExpr::i32_const(self.memory.stack_base),
        );

        let mut exports = ExportSection::new();
        for (name, idx) in &self.exports {
            exports.export(name, ExportKind::Func, *idx);
        }
        exports.export("memory", ExportKind::Memory, 0);

        let mut code = CodeSection::new();
        for (i, slot) in slots.iter().enumerate() {
            let func = slot.as_ref().unwrap_or_else(|| {
                panic!("volar-wasm-backend: function at slot {i} was declared but never given a body")
            });
            code.function(func);
        }

        let mut module = Module::new();
        module
            .section(&types)
            .section(&imports)
            .section(&functions)
            .section(&memories)
            .section(&globals)
            .section(&exports)
            .section(&code);
        module.finish()
    }
}

impl LirTarget for WasmBackend {
    type Value = u32;
    type Block = u32;

    fn abi(&self) -> LirAbi {
        LirAbi::WASM
    }

    fn define_struct(&mut self, def: StructDef) -> StructId {
        self.current_mut().define_struct(def)
    }

    fn declare_import(&mut self, name: &str, params: &[LirType], ret: Option<LirType>) {
        Arc::make_mut(&mut self.registry).declare_import(name, params, ret);
    }

    fn declare_function(&mut self, name: &str, params: &[LirType], ret: Option<LirType>) {
        Arc::make_mut(&mut self.registry).declare_function(name, params, ret);
        self.slots.lock().unwrap().push(None);
    }

    fn begin_function(&mut self, name: &str, params: &[LirType], ret: Option<LirType>) -> (u32, Vec<Vec<u32>>) {
        assert!(self.current.is_none(), "begin_function called while already inside a function");
        // Callers that drive `begin_function` directly without going through
        // `volar-lir-codegen`'s module driver (e.g. `lower_ir`'s single-circuit
        // entry point, which has no sibling functions to forward-reference)
        // never call `declare_function` up front. Fall back to declaring it
        // here -- safe precisely because a name seen for the first time at
        // `begin_function` can't have been referenced by an earlier caller.
        if !self.registry.contains(name) {
            LirTarget::declare_function(self, name, params, ret.clone());
        }
        let slot_idx = self.registry.slot_index_of(name);
        let mut rec = RecordingTarget::new();
        let result = rec.begin_function(name, params, ret);
        self.current = Some((slot_idx, rec));
        result
    }

    fn end_function(&mut self) {
        self.current_mut().end_function();
        let (slot_idx, rec) = self.current.take().expect("end_function called outside a function");
        let saved = rec.finish();
        let registry = self.registry.clone();
        let slots = self.slots.clone();
        let pending = self.pending.clone();
        {
            let mut count = pending.0.lock().unwrap();
            *count += 1;
        }
        self.executor.spawn(Box::new(move || {
            let mut enc = WasmFuncEncoder::new(registry);
            saved.replay(&mut enc);
            let func = enc.finish();
            {
                let mut guard = slots.lock().unwrap();
                guard[slot_idx as usize] = Some(func);
            }
            let mut count = pending.0.lock().unwrap();
            *count -= 1;
            pending.1.notify_all();
        }));
    }

    fn create_block(&mut self) -> u32 {
        self.current_mut().create_block()
    }
    fn add_block_param(&mut self, block: u32, ty: LirType) -> u32 {
        self.current_mut().add_block_param(block, ty)
    }
    fn switch_to_block(&mut self, block: u32) {
        self.current_mut().switch_to_block(block)
    }

    fn iconst(&mut self, ty: LirType, val: i64) -> u32 {
        self.current_mut().iconst(ty, val)
    }

    fn add(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().add(lhs, rhs)
    }
    fn sub(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().sub(lhs, rhs)
    }
    fn mul(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().mul(lhs, rhs)
    }
    fn udiv(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().udiv(lhs, rhs)
    }
    fn sdiv(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().sdiv(lhs, rhs)
    }

    fn and(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().and(lhs, rhs)
    }
    fn or(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().or(lhs, rhs)
    }
    fn xor(&mut self, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().xor(lhs, rhs)
    }
    fn not(&mut self, val: u32) -> u32 {
        self.current_mut().not(val)
    }
    fn shl(&mut self, val: u32, shift: u32) -> u32 {
        self.current_mut().shl(val, shift)
    }
    fn lshr(&mut self, val: u32, shift: u32) -> u32 {
        self.current_mut().lshr(val, shift)
    }
    fn ashr(&mut self, val: u32, shift: u32) -> u32 {
        self.current_mut().ashr(val, shift)
    }

    fn icmp(&mut self, pred: IcmpPred, lhs: u32, rhs: u32) -> u32 {
        self.current_mut().icmp(pred, lhs, rhs)
    }

    fn zext(&mut self, val: u32, dst_ty: LirType) -> u32 {
        self.current_mut().zext(val, dst_ty)
    }
    fn sext(&mut self, val: u32, dst_ty: LirType) -> u32 {
        self.current_mut().sext(val, dst_ty)
    }
    fn trunc(&mut self, val: u32, dst_ty: LirType) -> u32 {
        self.current_mut().trunc(val, dst_ty)
    }

    fn select(&mut self, cond: u32, then_val: u32, else_val: u32) -> u32 {
        self.current_mut().select(cond, then_val, else_val)
    }

    fn value_scalar_type(&self, val: &u32) -> LirType {
        self.current.as_ref().expect("WasmBackend: not inside a function").1.value_scalar_type(val)
    }

    fn call_extern(&mut self, name: &str, arg_tys: &[LirType], args: &[u32], ret_ty: Option<LirType>) -> Vec<u32> {
        self.current_mut().call_extern(name, arg_tys, args, ret_ty)
    }

    fn jump(&mut self, target: u32, branch: BranchTarget<u32>) {
        self.current_mut().jump(target, branch)
    }

    fn branch(
        &mut self,
        cond: u32,
        then_block: u32,
        then_branch: BranchTarget<u32>,
        else_block: u32,
        else_branch: BranchTarget<u32>,
    ) {
        self.current_mut().branch(cond, then_block, then_branch, else_block, else_branch)
    }

    fn ret(&mut self, vals: &[u32]) {
        self.current_mut().ret(vals)
    }

    fn oracle(&mut self, name: &str, arg_tys: &[LirType], args: &[u32], ret_tys: &[LirType]) -> Vec<u32> {
        self.current_mut().oracle(name, arg_tys, args, ret_tys)
    }

    fn action(
        &mut self,
        name: &str,
        guard: u32,
        arg_tys: &[LirType],
        args: &[u32],
        fallbacks: &[u32],
        ret_tys: &[LirType],
    ) -> Vec<u32> {
        self.current_mut().action(name, guard, arg_tys, args, fallbacks, ret_tys)
    }

    fn rng(&mut self, ty: LirType) -> u32 {
        self.current_mut().rng(ty)
    }

    fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = u32>> {
        self.current.as_mut().map(|(_, rec)| rec as &mut dyn StackAllocExt<Value = u32>)
    }

    fn ptr_index_load(&mut self, ptr: u32, idx: u32, pointee_ty: &LirType) -> Vec<u32> {
        self.current_mut().ptr_index_load(ptr, idx, pointee_ty)
    }

    fn ptr_index_store(&mut self, ptr: u32, idx: u32, vals: &[u32], pointee_ty: &LirType) {
        self.current_mut().ptr_index_store(ptr, idx, vals, pointee_ty)
    }
}
