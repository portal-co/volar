//! Name -> WASM `funcidx`/type-index registry.
//!
//! Populated eagerly (imports first, then module-defined functions) before
//! any function body is lowered, so a call to any name -- including one whose
//! body hasn't been lowered yet -- always resolves to a stable, final index.

use std::collections::HashMap;
use volar_lir::LirType;
use wasm_encoder::ValType;

/// Map a scalar `LirType` to its WASM value type.
///
/// Panics on aggregates (`Arr`/`Struct`), `Native` field elements, and
/// 128-bit integers -- unsupported in this backend's first version. See
/// `docs/wasm-backend.md` for the full scope.
pub(crate) fn lir_scalar_to_valtype(ty: &LirType) -> ValType {
    match ty {
        LirType::Bool
        | LirType::I8
        | LirType::U8
        | LirType::I16
        | LirType::U16
        | LirType::I32
        | LirType::U32 => ValType::I32,
        LirType::I64 | LirType::U64 => ValType::I64,
        LirType::Ptr(_) => ValType::I32,
        _ => {
            panic!(
                "volar-wasm-backend: unsupported LirType {ty:?} -- v1 supports only \
                 scalar integers up to 64 bits, Bool, and Ptr; large aggregates must be \
                 passed via StackAllocExt (LirAbi::WASM's byval limit already routes \
                 them through alloca + pointer before they reach the backend)"
            )
        }
    }
}

pub(crate) fn is_64(ty: &LirType) -> bool {
    matches!(ty, LirType::I64 | LirType::U64)
}

#[derive(Clone)]
struct FuncEntry {
    func_idx: u32,
    /// Position within the defined-function list (`None` for imports, which
    /// never get a `Code` section body).
    slot_idx: Option<u32>,
}

/// Name -> `funcidx` table, plus the interned WASM type table.
///
/// Imports always occupy the lowest indices (WASM requires this), so
/// [`declare_import`](FuncRegistry::declare_import) must not be called after
/// [`declare_function`](FuncRegistry::declare_function).
#[derive(Clone, Default)]
pub(crate) struct FuncRegistry {
    type_table: Vec<(Vec<ValType>, Vec<ValType>)>,
    type_index: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    by_name: HashMap<String, FuncEntry>,
    /// `(name, type_idx)`, in declaration order; `func_idx` = position.
    pub(crate) imports: Vec<(String, u32)>,
    /// `(name, type_idx)`, in declaration order; `func_idx` = `imports.len() + position`.
    pub(crate) defined: Vec<(String, u32)>,
}

impl FuncRegistry {
    pub(crate) fn type_table(&self) -> &[(Vec<ValType>, Vec<ValType>)] {
        &self.type_table
    }

    fn intern_type(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&idx) = self.type_index.get(&key) {
            return idx;
        }
        let idx = self.type_table.len() as u32;
        self.type_index.insert(key.clone(), idx);
        self.type_table.push(key);
        idx
    }

    pub(crate) fn declare_import(&mut self, name: &str, param_tys: &[LirType], ret_ty: Option<LirType>) -> u32 {
        assert!(
            self.defined.is_empty(),
            "volar-wasm-backend: import '{name}' declared after a function body was \
             already declared -- imports must occupy the lowest indices in WASM's \
             function index space, so every declare_import call must happen before \
             the first declare_function call"
        );
        let params: Vec<ValType> = param_tys.iter().map(lir_scalar_to_valtype).collect();
        let results: Vec<ValType> = ret_ty.iter().map(lir_scalar_to_valtype).collect();
        let type_idx = self.intern_type(params, results);
        let func_idx = self.imports.len() as u32;
        self.imports.push((name.to_string(), type_idx));
        self.by_name.insert(name.to_string(), FuncEntry { func_idx, slot_idx: None });
        func_idx
    }

    pub(crate) fn declare_function(&mut self, name: &str, param_tys: &[LirType], ret_ty: Option<LirType>) -> u32 {
        let params: Vec<ValType> = param_tys.iter().map(lir_scalar_to_valtype).collect();
        let results: Vec<ValType> = ret_ty.iter().map(lir_scalar_to_valtype).collect();
        let type_idx = self.intern_type(params, results);
        let func_idx = self.imports.len() as u32 + self.defined.len() as u32;
        let slot_idx = self.defined.len() as u32;
        self.defined.push((name.to_string(), type_idx));
        self.by_name.insert(name.to_string(), FuncEntry { func_idx, slot_idx: Some(slot_idx) });
        func_idx
    }

    pub(crate) fn contains(&self, name: &str) -> bool {
        self.by_name.contains_key(name)
    }

    /// Resolve a name (import or defined function) to its final `funcidx`.
    ///
    /// Panics if `name` was never declared -- every callee must be
    /// pre-declared (via `declare_import`/`declare_function`, both driven
    /// automatically by `volar-lir-codegen`'s module driver for compiler-fed
    /// modules) before any function body is lowered.
    pub(crate) fn index_of(&self, name: &str) -> u32 {
        self.by_name
            .get(name)
            .unwrap_or_else(|| {
                panic!(
                    "volar-wasm-backend: call to undeclared function/import '{name}' -- \
                     every callee must be pre-declared before any body is lowered"
                )
            })
            .func_idx
    }

    /// Resolve a defined function's name to its position in the `Code`
    /// section (i.e. `slots` index). Panics if `name` is an import.
    pub(crate) fn slot_index_of(&self, name: &str) -> u32 {
        self.by_name
            .get(name)
            .unwrap_or_else(|| panic!("volar-wasm-backend: undeclared function '{name}'"))
            .slot_idx
            .unwrap_or_else(|| panic!("volar-wasm-backend: '{name}' is an import, not a defined function"))
    }
}
