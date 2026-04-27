// @reliability: normal
// @ai: assisted
//! C99 backend for `LirTarget`.
//!
//! Emits a complete `.c` file suitable for testing with `cc -O0 -std=c99`.
//! Each function becomes a C function. SSA values are `vN` locals; block
//! parameters are pre-declared `blockM_pK` variables at the function top.
//!
//! # Parallel-assignment safety
//!
//! When jumping to a block, all parameter assignments go through globally-unique
//! temporaries (`_tN`) before being stored, avoiding read-after-write hazards.
//!
//! # Phase 2 additions
//!
//! Supports `LirType::Arr` and `LirType::Struct`:
//! - Arrays are typedef'd as `typedef struct { T data[N]; } Arr_T_N;`
//! - Structs are typedef'd from their `StructDef` fields.
//! - `arr_set` emits a copy-and-mutate pattern (functional update).
//! - `call_extern` emits `extern` declarations in `finish()`.

use std::{
    collections::BTreeSet,
    fmt::Write as FmtWrite,
    string::String,
    vec::Vec,
};
use volar_lir::{IcmpPred, LirTarget, LirType, LirAbi, StackAllocExt, StructDef, StructId};

pub use volar_lir::NameConfig;

/// Sanitize a field name for C: if it starts with a digit (e.g. tuple fields
/// "0", "1", …), prefix with `_` to make it a valid C identifier.
fn c_field_name(name: &str) -> String {
    if name.starts_with(|c: char| c.is_ascii_digit()) {
        format!("_{name}")
    } else {
        name.to_string()
    }
}

// ============================================================================
// Handles
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CValue(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CBlock(pub u32);

// ============================================================================
// Per-block metadata
// ============================================================================

struct BlockMeta {
    /// How many parameters this block has.
    param_count: u32,
    /// The value-ID of the first param; params are IDs [base, base+count).
    param_base: u32,
}

// ============================================================================
// Per-function state
// ============================================================================

struct FunctionState {
    name: String,
    ret_ty: Option<LirType>,

    /// Number of C function parameters (= entry block's function-level params).
    func_param_count: usize,

    /// Block-param pre-declarations, emitted before any label.
    preamble: String,
    /// Instruction stream, labels, and jump code.
    body: String,

    /// One entry per block.
    blocks: Vec<BlockMeta>,
    /// Maps value ID → C name (`blockM_pK` or `vN`).
    value_name: Vec<String>,
    /// Maps value ID → pre-computed C type string (e.g. `"uint8_t"`, `"Arr_U8_16"`).
    value_ctype: Vec<String>,
    /// Maps value ID → LirType (for type queries in the backend).
    value_type: Vec<LirType>,

    /// Next fresh value ID.
    next_value: u32,
    /// Counter for unique jump-temporary variable names (`_tN`).
    next_tmp: u32,
    /// Which block is currently being emitted.
    current_block: Option<u32>,
}

impl FunctionState {
    fn alloc_value(&mut self, ty: LirType, c_type: String, name: String) -> CValue {
        let id = self.next_value;
        self.next_value += 1;
        self.value_name.push(name);
        self.value_ctype.push(c_type);
        self.value_type.push(ty);
        CValue(id)
    }

    /// Emit `<c_type> vN = <expr>;` and return the fresh value.
    fn emit_instr(&mut self, ty: LirType, c_type: String, expr: &str) -> CValue {
        let id = self.next_value;
        writeln!(self.body, "  {c_type} v{id} = {expr};").unwrap();
        self.alloc_value(ty, c_type, format!("v{id}"))
    }

    fn name_of(&self, v: CValue) -> &str {
        &self.value_name[v.0 as usize]
    }

    fn type_of(&self, v: CValue) -> &LirType {
        &self.value_type[v.0 as usize]
    }

    fn ctype_of(&self, v: CValue) -> &str {
        &self.value_ctype[v.0 as usize]
    }

    /// Two-phase parallel assignment to `target` block params, then `goto`.
    fn emit_jump(&mut self, target: CBlock, args: &[CValue]) {
        let count = self.blocks[target.0 as usize].param_count as usize;
        assert_eq!(args.len(), count, "jump arg count mismatch");

        let base = self.next_tmp;
        self.next_tmp += count as u32;

        // Phase 1: snapshot into globally-unique temporaries.
        for (i, &arg) in args.iter().enumerate() {
            let c_type = self.value_ctype[arg.0 as usize].clone();
            let src = self.value_name[arg.0 as usize].clone();
            let t = base + i as u32;
            writeln!(self.body, "  {c_type} _t{t} = {src};").unwrap();
        }
        // Phase 2: assign temporaries to block-param variables.
        let bid = target.0;
        for i in 0..count {
            let t = base + i as u32;
            writeln!(self.body, "  block{bid}_p{i} = _t{t};").unwrap();
        }
        writeln!(self.body, "  goto block{bid};").unwrap();
    }
}

// ============================================================================
// The backend
// ============================================================================

/// C99 backend implementing `LirTarget`.
///
/// Call `begin_function` / emit instructions / `end_function` one or more
/// times, then call `finish()` to obtain the complete C source file.
pub struct CBackend {
    completed_functions: Vec<String>,
    current: Option<FunctionState>,

    // --- Phase 2: aggregate type support ---

    /// Registered struct definitions in `define_struct` call order.
    struct_defs: Vec<StructDef>,
    /// Struct names indexed by `StructId`.
    struct_names: Vec<String>,
    /// Unified list of all type definitions (array and struct typedefs) in
    /// dependency order.  Emitted as-is in `finish()`.
    all_typedefs: Vec<String>,
    /// Set of already-registered array typedef names for deduplication.
    array_typedef_set: BTreeSet<String>,
    /// Rendered `extern RetType name(ArgTypes...);` declarations.
    extern_decls: Vec<String>,
    /// Next StructId to assign.
    next_struct_id: StructId,
    /// Name configuration: prefix and per-name remaps applied to all defined
    /// and called function names.  See [`NameConfig`].
    pub name_config: NameConfig,
    /// Name of the C function to call for `Rng` stmts.
    /// Expected signature: `void rng_fn(void *out, size_t len);`
    /// Default: `"volar_rng"`.
    pub rng_fn: String,
}

impl CBackend {
    pub fn new() -> Self {
        CBackend {
            completed_functions: Vec::new(),
            current: None,
            struct_defs: Vec::new(),
            struct_names: Vec::new(),
            all_typedefs: Vec::new(),
            array_typedef_set: BTreeSet::new(),
            extern_decls: Vec::new(),
            next_struct_id: 0,
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

    /// Set the C function name used for `Rng` stmts.
    pub fn with_rng_fn(mut self, name: impl Into<String>) -> Self {
        self.rng_fn = name.into();
        self
    }

    /// Finalize and return the complete C source.
    ///
    /// Emits (in order):
    /// 1. `#include` headers
    /// 2. Type definitions (array + struct typedefs in dependency order)
    /// 3. Extern declarations
    /// 4. Function definitions
    pub fn finish(self) -> String {
        let mut out = String::new();
        out.push_str("#include <stdint.h>\n");
        out.push_str("#include <stdbool.h>\n\n");

        // All type definitions in dependency order.
        for td in &self.all_typedefs {
            out.push_str(td);
        }
        if !self.all_typedefs.is_empty() {
            out.push('\n');
        }

        // Extern declarations.
        for decl in &self.extern_decls {
            out.push_str(decl);
        }
        if !self.extern_decls.is_empty() {
            out.push('\n');
        }

        // Function definitions.
        for func in self.completed_functions {
            out.push_str(&func);
            out.push('\n');
        }
        out
    }

    // ---- Internal helpers ---------------------------------------------------

    fn state(&mut self) -> &mut FunctionState {
        self.current.as_mut().expect("CBackend: not inside a function")
    }

    /// Convert a `LirType` to its C type name string.
    fn type_to_c(&self, ty: &LirType) -> String {
        lir_type_to_c_free(ty, &self.struct_names)
    }

    /// Register an array typedef (and any nested array typedefs) in DFS order.
    /// No-ops if already registered.
    fn register_array_typedef(&mut self, ty: &LirType) {
        if let LirType::Arr(elem, len) = ty {
            // Register the element type first (handles nesting).
            let elem_clone = *elem.clone();
            self.register_array_typedef(&elem_clone);
            let name = arr_typedef_name(elem, *len);
            if self.array_typedef_set.insert(name.clone()) {
                let elem_c = lir_type_to_c_free(&elem_clone, &self.struct_names);
                let td = format!("typedef struct {{ {elem_c} data[{len}]; }} {name};\n");
                self.all_typedefs.push(td);
            }
        }
    }

    fn binop(&mut self, lhs: CValue, op: &str, rhs: CValue) -> CValue {
        let ty = self.state().type_of(lhs).clone();
        let c_type = self.type_to_c(&ty);
        let l = self.state().name_of(lhs).to_owned();
        let r = self.state().name_of(rhs).to_owned();
        let expr = format!("{l} {op} {r}");
        self.state().emit_instr(ty, c_type, &expr)
    }

    fn unop(&mut self, op: &str, val: CValue) -> CValue {
        let ty = self.state().type_of(val).clone();
        let c_type = self.type_to_c(&ty);
        let v = self.state().name_of(val).to_owned();
        let expr = format!("{op}{v}");
        self.state().emit_instr(ty, c_type, &expr)
    }
}

impl Default for CBackend {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Aggregate pack / unpack helpers
// ============================================================================

impl CBackend {
    /// Recursively unpack an aggregate `CValue` into leaf scalar `CValue`s.
    ///
    /// If `to_preamble` is true, emits declarations to the function preamble
    /// (used when unpacking function parameters at function entry).  Otherwise
    /// emits to the body (used when unpacking extern call return values).
    fn unpack_to_scalars(&mut self, agg: CValue, ty: &LirType, to_preamble: bool) -> Vec<CValue> {
        match ty.clone() {
            LirType::Arr(elem, n) => {
                let elem_c = self.type_to_c(&elem);
                let agg_name = self.state().name_of(agg).to_owned();
                let mut result = Vec::new();
                for i in 0..n {
                    let expr = format!("{agg_name}.data[{i}]");
                    let id = self.state().next_value;
                    if to_preamble {
                        writeln!(self.state().preamble, "  {elem_c} v{id} = {expr};").unwrap();
                    } else {
                        writeln!(self.state().body, "  {elem_c} v{id} = {expr};").unwrap();
                    }
                    let child = self.state().alloc_value(*elem.clone(), elem_c.clone(), format!("v{id}"));
                    let mut children = self.unpack_to_scalars(child, &elem, to_preamble);
                    result.append(&mut children);
                }
                result
            }
            LirType::Struct(id) => {
                let field_tys: Vec<LirType> = self.struct_defs[id as usize].fields.iter().map(|f| f.ty.clone()).collect();
                let field_names: Vec<String> = self.struct_defs[id as usize].fields.iter().map(|f| f.name.clone()).collect();
                let agg_name = self.state().name_of(agg).to_owned();
                let mut result = Vec::new();
                for (fname, fty) in field_names.iter().zip(field_tys.iter()) {
                    let fc = self.type_to_c(fty);
                    let cfname = c_field_name(fname);
                    let expr = format!("{agg_name}.{cfname}");
                    let vid = self.state().next_value;
                    if to_preamble {
                        writeln!(self.state().preamble, "  {fc} v{vid} = {expr};").unwrap();
                    } else {
                        writeln!(self.state().body, "  {fc} v{vid} = {expr};").unwrap();
                    }
                    let child = self.state().alloc_value(fty.clone(), fc.clone(), format!("v{vid}"));
                    let mut children = self.unpack_to_scalars(child, fty, to_preamble);
                    result.append(&mut children);
                }
                result
            }
            // Already a scalar — return as-is.
            _ => vec![agg],
        }
    }

    /// Recursively pack a flat slice of scalars into the given aggregate `LirType`.
    ///
    /// `offset` is advanced by the number of scalars consumed.  Emits
    /// construction instructions to the body.
    fn pack_scalars(&mut self, ty: &LirType, scalars: &[CValue], offset: &mut usize) -> CValue {
        match ty.clone() {
            LirType::Arr(elem, n) => {
                let packed_elems: Vec<CValue> = (0..n)
                    .map(|_| self.pack_scalars(&elem, scalars, offset))
                    .collect();
                let arr_ty = LirType::Arr(elem.clone(), n);
                self.register_array_typedef(&arr_ty);
                let type_name = arr_typedef_name(&elem, n);
                let elem_names: Vec<String> = packed_elems.iter()
                    .map(|&v| self.state().name_of(v).to_owned())
                    .collect();
                let data_init = elem_names.join(", ");
                let expr = format!("({type_name}){{ .data = {{ {data_init} }} }}");
                self.state().emit_instr(arr_ty, type_name, &expr)
            }
            LirType::Struct(id) => {
                let field_tys: Vec<LirType> = self.struct_defs[id as usize].fields.iter().map(|f| f.ty.clone()).collect();
                let field_names: Vec<String> = self.struct_defs[id as usize].fields.iter().map(|f| f.name.clone()).collect();
                let struct_name = self.struct_names[id as usize].clone();
                let packed_fields: Vec<CValue> = field_tys.iter()
                    .map(|fty| self.pack_scalars(fty, scalars, offset))
                    .collect();
                let val_names: Vec<String> = packed_fields.iter()
                    .map(|&v| self.state().name_of(v).to_owned())
                    .collect();
                let init = field_names.iter().zip(val_names.iter())
                    .map(|(fname, vname)| {
                        let cfname = c_field_name(fname);
                        format!(".{cfname} = {vname}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let expr = format!("({struct_name}){{ {init} }}");
                self.state().emit_instr(LirType::Struct(id), struct_name, &expr)
            }
            // Scalar — consume one value from the flat list.
            _ => {
                let val = scalars[*offset];
                *offset += 1;
                val
            }
        }
    }
}

// ============================================================================
// LirTarget impl
// ============================================================================

impl LirTarget for CBackend {
    type Value = CValue;
    type Block = CBlock;

    // ---- Type registration --------------------------------------------------

    fn define_struct(&mut self, def: StructDef) -> StructId {
        let id = self.next_struct_id;
        self.next_struct_id += 1;

        // Register array typedefs for all field types (pre-pass before rendering).
        let field_tys: Vec<LirType> = def.fields.iter().map(|f| f.ty.clone()).collect();
        for ty in &field_tys {
            self.register_array_typedef(ty);
        }

        // Render the typedef with C-safe field names.
        let mut s = "typedef struct {\n".to_string();
        for field in &def.fields {
            let c_type = self.type_to_c(&field.ty);
            let fname = c_field_name(&field.name);
            writeln!(s, "  {c_type} {fname};").unwrap();
        }
        writeln!(s, "}} {};", def.name).unwrap();

        self.struct_names.push(def.name.clone());
        self.all_typedefs.push(s);
        self.struct_defs.push(def);
        id
    }

    // ---- Value type query ---------------------------------------------------

    fn value_scalar_type(&self, val: &CValue) -> LirType {
        self.current.as_ref().expect("value_scalar_type: not inside a function")
            .value_type[val.0 as usize].clone()
    }

    // ---- Function management ------------------------------------------------

    /// Begin a new C function.
    ///
    /// `params` may contain aggregate types (`Arr`/`Struct`).  Each aggregate
    /// parameter becomes one C function parameter (for ABI compatibility) and
    /// is then immediately unpacked into scalar locals in the preamble.
    /// Returns one `Vec<CValue>` per parameter containing the flat scalars.
    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (CBlock, Vec<Vec<CValue>>) {
        assert!(self.current.is_none(), "begin_function called while inside a function");

        // Register array typedefs for param and return types.
        for ty in params {
            self.register_array_typedef(ty);
        }
        if let Some(ref ty) = ret {
            self.register_array_typedef(ty);
        }

        let param_ctypes: Vec<String> = params.iter().map(|ty| self.type_to_c(ty)).collect();

        let mut state = FunctionState {
            name: self.name_config.apply(name),
            ret_ty: ret,
            func_param_count: params.len(),
            preamble: String::new(),
            body: String::new(),
            blocks: vec![BlockMeta { param_count: 0, param_base: 0 }],
            value_name: Vec::new(),
            value_ctype: Vec::new(),
            value_type: Vec::new(),
            next_value: 0,
            next_tmp: 0,
            current_block: None,
        };

        // Allocate one value per parameter for the C function signature.
        // These are the aggregate values as seen by the C ABI.
        let agg_params: Vec<CValue> = params
            .iter()
            .zip(param_ctypes.iter())
            .enumerate()
            .map(|(i, (ty, c_type))| {
                state.alloc_value(ty.clone(), c_type.clone(), format!("v{i}"))
            })
            .collect();

        self.current = Some(state);

        // Unpack each aggregate param to scalars, emitting into the preamble.
        let param_val_groups: Vec<Vec<CValue>> = agg_params
            .iter()
            .zip(params.iter())
            .map(|(&agg, ty)| self.unpack_to_scalars(agg, ty, true))
            .collect();

        (CBlock(0), param_val_groups)
    }

    fn end_function(&mut self) {
        let state = self.current.take().expect("end_function called outside a function");

        let ret_cty = state
            .ret_ty
            .as_ref()
            .map(|ty| self.type_to_c(ty))
            .unwrap_or_else(|| "void".to_string());

        let mut param_list = String::new();
        for i in 0..state.func_param_count {
            if i > 0 {
                param_list.push_str(", ");
            }
            let c_type = &state.value_ctype[i];
            let name = &state.value_name[i];
            param_list.push_str(&format!("{c_type} {name}"));
        }

        let mut func = String::new();
        writeln!(func, "{ret_cty} {}({param_list}) {{", state.name).unwrap();
        func.push_str(&state.preamble);
        func.push_str(&state.body);
        writeln!(func, "}}").unwrap();

        self.completed_functions.push(func);
    }

    // ---- Block management ---------------------------------------------------

    fn create_block(&mut self) -> CBlock {
        let state = self.state();
        let id = state.blocks.len() as u32;
        state.blocks.push(BlockMeta { param_count: 0, param_base: state.next_value });
        CBlock(id)
    }

    fn add_block_param(&mut self, block: CBlock, ty: LirType) -> CValue {
        let c_type = self.type_to_c(&ty);
        let state = self.state();
        let block_id = block.0;
        let param_idx = state.blocks[block_id as usize].param_count;
        state.blocks[block_id as usize].param_count += 1;

        let c_name = format!("block{block_id}_p{param_idx}");
        writeln!(state.preamble, "  {c_type} {c_name};").unwrap();
        state.alloc_value(ty, c_type, c_name)
    }

    fn switch_to_block(&mut self, block: CBlock) {
        let state = self.state();
        state.current_block = Some(block.0);
        writeln!(state.body, "block{}:;", block.0).unwrap();
    }

    // ---- Constants ----------------------------------------------------------

    fn iconst(&mut self, ty: LirType, val: i64) -> CValue {
        let c_type = self.type_to_c(&ty);
        self.state().emit_instr(ty, c_type, &format!("{val}"))
    }

    // ---- Arithmetic ---------------------------------------------------------

    fn add(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "+", rhs) }
    fn sub(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "-", rhs) }
    fn mul(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "*", rhs) }
    fn udiv(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "/", rhs) }
    fn sdiv(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "/", rhs) }

    // ---- Bitwise ------------------------------------------------------------

    fn and(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "&", rhs) }
    fn or(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "|", rhs) }
    fn xor(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "^", rhs) }
    fn not(&mut self, val: CValue) -> CValue {
        let ty = self.state().type_of(val).clone();
        // For bools, use logical `!`; for integers, use bitwise `~`.
        let op = if ty == LirType::Bool { "!" } else { "~" };
        self.unop(op, val)
    }
    fn shl(&mut self, val: CValue, shift: CValue) -> CValue { self.binop(val, "<<", shift) }
    fn lshr(&mut self, val: CValue, shift: CValue) -> CValue { self.binop(val, ">>", shift) }

    fn ashr(&mut self, val: CValue, shift: CValue) -> CValue {
        let ty = self.state().type_of(val).clone();
        let c_type = self.type_to_c(&ty);
        let signed_ty = signed_variant(&ty);
        let v = self.state().name_of(val).to_owned();
        let s = self.state().name_of(shift).to_owned();
        let expr = format!("(({signed_ty}){v}) >> {s}");
        self.state().emit_instr(ty, c_type, &expr)
    }

    // ---- Comparison ---------------------------------------------------------

    fn icmp(&mut self, pred: IcmpPred, lhs: CValue, rhs: CValue) -> CValue {
        let op = match pred {
            IcmpPred::Eq => "==",
            IcmpPred::Ne => "!=",
            IcmpPred::Ult | IcmpPred::Slt => "<",
            IcmpPred::Ule | IcmpPred::Sle => "<=",
            IcmpPred::Ugt | IcmpPred::Sgt => ">",
            IcmpPred::Uge | IcmpPred::Sge => ">=",
        };
        let l = self.state().name_of(lhs).to_owned();
        let r = self.state().name_of(rhs).to_owned();
        let expr = match pred {
            IcmpPred::Slt | IcmpPred::Sle | IcmpPred::Sgt | IcmpPred::Sge => {
                let ty = self.state().type_of(lhs).clone();
                let sc = signed_variant(&ty);
                format!("(({sc}){l}) {op} (({sc}){r})")
            }
            _ => format!("{l} {op} {r}"),
        };
        self.state().emit_instr(LirType::Bool, "bool".to_string(), &expr)
    }

    // ---- Conversions --------------------------------------------------------

    fn zext(&mut self, val: CValue, dst_ty: LirType) -> CValue {
        let src_name = self.state().name_of(val).to_owned();
        let c_type = self.type_to_c(&dst_ty);
        let expr = format!("({c_type}){src_name}");
        self.state().emit_instr(dst_ty, c_type, &expr)
    }

    fn sext(&mut self, val: CValue, dst_ty: LirType) -> CValue {
        let src_ty = self.state().type_of(val).clone();
        let src_name = self.state().name_of(val).to_owned();
        let signed_src = signed_variant(&src_ty);
        let c_type = self.type_to_c(&dst_ty);
        let expr = format!("({c_type})(({signed_src}){src_name})");
        self.state().emit_instr(dst_ty, c_type, &expr)
    }

    fn trunc(&mut self, val: CValue, dst_ty: LirType) -> CValue {
        let src_name = self.state().name_of(val).to_owned();
        let c_type = self.type_to_c(&dst_ty);
        let expr = format!("({c_type}){src_name}");
        self.state().emit_instr(dst_ty, c_type, &expr)
    }

    // ---- Select -------------------------------------------------------------

    fn select(&mut self, cond: CValue, then_val: CValue, else_val: CValue) -> CValue {
        let ty = self.state().type_of(then_val).clone();
        let c_type = self.type_to_c(&ty);
        let c = self.state().name_of(cond).to_owned();
        let t = self.state().name_of(then_val).to_owned();
        let e = self.state().name_of(else_val).to_owned();
        let expr = format!("{c} ? {t} : {e}");
        self.state().emit_instr(ty, c_type, &expr)
    }

    // ---- Extern calls -------------------------------------------------------

    /// Call an external C function.
    ///
    /// `arg_tys` gives the ABI (possibly aggregate) type for each logical arg.
    /// `args` is the flat scalar list.  The backend packs scalars into C
    /// aggregates before the call and unpacks the return value afterwards.
    fn call_extern(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[CValue],
        ret_ty: Option<LirType>,
    ) -> Vec<CValue> {
        let name = self.name_config.apply(name);
        let name = name.as_str();
        // Pack flat scalars into C aggregate arguments.
        let mut offset = 0usize;
        let packed_args: Vec<CValue> = arg_tys.iter()
            .map(|ty| self.pack_scalars(ty, args, &mut offset))
            .collect();

        // Build the extern declaration using the aggregate C types.
        let arg_c_tys: Vec<String> = arg_tys.iter().map(|ty| self.type_to_c(ty)).collect();
        let ret_c_ty = ret_ty.as_ref().map(|ty| self.type_to_c(ty)).unwrap_or_else(|| "void".to_string());
        let params_str = arg_c_tys.join(", ");
        let extern_decl = format!("extern {ret_c_ty} {name}({params_str});\n");
        if !self.extern_decls.contains(&extern_decl) {
            self.extern_decls.push(extern_decl);
        }

        let packed_names: Vec<String> = packed_args.iter()
            .map(|&v| self.state().name_of(v).to_owned())
            .collect();
        let args_str = packed_names.join(", ");

        match ret_ty {
            Some(ret) => {
                // Emit the call, capturing the return value as an aggregate.
                let c_type = self.type_to_c(&ret);
                let expr = format!("{name}({args_str})");
                let agg_result = self.state().emit_instr(ret.clone(), c_type, &expr);
                // Unpack the aggregate return into flat scalars.
                self.unpack_to_scalars(agg_result, &ret, false)
            }
            None => {
                writeln!(self.state().body, "  {name}({args_str});").unwrap();
                vec![]
            }
        }
    }

    // ---- Terminators --------------------------------------------------------

    fn jump(&mut self, target: CBlock, args: &[CValue]) {
        let args = args.to_vec();
        self.state().emit_jump(target, &args);
    }

    fn branch(
        &mut self,
        cond: CValue,
        then_block: CBlock,
        then_args: &[CValue],
        else_block: CBlock,
        else_args: &[CValue],
    ) {
        let cond_name = self.state().name_of(cond).to_owned();
        let then_args = then_args.to_vec();
        let else_args = else_args.to_vec();

        writeln!(self.state().body, "  if ({cond_name}) {{").unwrap();
        {
            let state = self.state();
            let count = state.blocks[then_block.0 as usize].param_count as usize;
            let base = state.next_tmp;
            state.next_tmp += count as u32;
            for (i, &arg) in then_args.iter().enumerate() {
                let c_type = state.value_ctype[arg.0 as usize].clone();
                let src = state.value_name[arg.0 as usize].clone();
                let t = base + i as u32;
                writeln!(state.body, "    {c_type} _t{t} = {src};").unwrap();
            }
            let bid = then_block.0;
            for i in 0..count {
                let t = base + i as u32;
                writeln!(state.body, "    block{bid}_p{i} = _t{t};").unwrap();
            }
            writeln!(state.body, "    goto block{bid};").unwrap();
        }
        writeln!(self.state().body, "  }} else {{").unwrap();
        {
            let state = self.state();
            let count = state.blocks[else_block.0 as usize].param_count as usize;
            let base = state.next_tmp;
            state.next_tmp += count as u32;
            for (i, &arg) in else_args.iter().enumerate() {
                let c_type = state.value_ctype[arg.0 as usize].clone();
                let src = state.value_name[arg.0 as usize].clone();
                let t = base + i as u32;
                writeln!(state.body, "    {c_type} _t{t} = {src};").unwrap();
            }
            let bid = else_block.0;
            for i in 0..count {
                let t = base + i as u32;
                writeln!(state.body, "    block{bid}_p{i} = _t{t};").unwrap();
            }
            writeln!(state.body, "    goto block{bid};").unwrap();
        }
        writeln!(self.state().body, "  }}").unwrap();
    }

    /// Emit a return.  `vals` is the flat scalar list.
    ///
    /// If the function's declared return type is an aggregate, the scalars are
    /// packed back into the C struct/array before `return`.
    fn ret(&mut self, vals: &[CValue]) {
        if vals.is_empty() {
            writeln!(self.state().body, "  return;").unwrap();
            return;
        }
        let ret_ty = self.state().ret_ty.clone();
        match ret_ty {
            None => {
                writeln!(self.state().body, "  return;").unwrap();
            }
            Some(ty) if ty.is_scalar() => {
                // Single scalar return — emit directly.
                assert_eq!(vals.len(), 1, "ret: scalar return type but {} values", vals.len());
                let name = self.state().name_of(vals[0]).to_owned();
                writeln!(self.state().body, "  return {name};").unwrap();
            }
            Some(ty) => {
                // Aggregate return — pack scalars back into the C type.
                let mut offset = 0usize;
                let packed = self.pack_scalars(&ty.clone(), vals, &mut offset);
                let name = self.state().name_of(packed).to_owned();
                writeln!(self.state().body, "  return {name};").unwrap();
            }
        }
    }

    // ---- External access primitives ----------------------------------------

    fn oracle(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[CValue],
        ret_tys: &[LirType],
    ) -> Vec<CValue> {
        // Treat oracle as a plain extern call. Single-output for now.
        let ret_ty = ret_tys.first().cloned();
        self.call_extern(&format!("oracle_{name}"), arg_tys, args, ret_ty)
    }

    fn action(
        &mut self,
        name: &str,
        guard: CValue,
        arg_tys: &[LirType],
        args: &[CValue],
        fallbacks: &[CValue],
        ret_tys: &[LirType],
    ) -> Vec<CValue> {
        // Call the action unconditionally, then select between result and fallback.
        let ret_ty = ret_tys.first().cloned();
        let action_result = self.call_extern(&format!("action_{name}"), arg_tys, args, ret_ty);
        // For each result scalar: output = guard ? action_result : fallback
        action_result
            .iter()
            .zip(fallbacks.iter())
            .map(|(r, f)| self.select(guard.clone(), r.clone(), f.clone()))
            .collect()
    }

    fn rng(&mut self, ty: LirType) -> CValue {
        let c_ty = lir_type_to_c_free(&ty, &self.struct_names);
        let rng_fn = self.rng_fn.clone();
        // Emit: `<c_ty> vN; <rng_fn>(&vN, sizeof(<c_ty>));`
        let state = self.current.as_mut().unwrap();
        let id = state.next_value;
        let var_name = format!("v{id}");
        writeln!(state.body, "  {c_ty} {var_name};").unwrap();
        writeln!(state.body, "  {rng_fn}(&{var_name}, sizeof({c_ty}));").unwrap();
        state.alloc_value(ty, c_ty, var_name)
    }

    fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = CValue>> {
        Some(self)
    }

    fn abi(&self) -> LirAbi {
        LirAbi::C_NATIVE
    }

    fn ptr_index_load(
        &mut self,
        ptr: CValue,
        idx: CValue,
        pointee_ty: &LirType,
    ) -> Vec<CValue> {
        // Emit: pointee_ty vN = ptr[idx];
        let ptr_name = self.state().name_of(ptr).to_owned();
        let idx_name = self.state().name_of(idx).to_owned();
        let c_type = self.type_to_c(pointee_ty);
        let expr = format!("{ptr_name}[{idx_name}]");
        let loaded = self.state().emit_instr(pointee_ty.clone(), c_type, &expr);
        self.unpack_to_scalars(loaded, pointee_ty, false)
    }

    fn ptr_index_store(
        &mut self,
        ptr: CValue,
        idx: CValue,
        vals: &[CValue],
        pointee_ty: &LirType,
    ) {
        // Pack flat scalars into the aggregate, then emit: ptr[idx] = packed;
        let mut offset = 0usize;
        let packed = self.pack_scalars(pointee_ty, vals, &mut offset);
        let ptr_name = self.state().name_of(ptr).to_owned();
        let idx_name = self.state().name_of(idx).to_owned();
        let val_name = self.state().name_of(packed).to_owned();
        writeln!(self.state().body, "  {ptr_name}[{idx_name}] = {val_name};").unwrap();
    }
}

// ============================================================================
// StackAllocExt impl
// ============================================================================

impl StackAllocExt for CBackend {
    type Value = CValue;

    /// Allocate a stack region for `count` elements of `elem_ty`.
    ///
    /// Emits into the function preamble (so the array has function scope):
    /// ```c
    /// T slot_vN[count];
    /// T* vN = slot_vN;
    /// ```
    /// Returns the pointer value `vN` of type `LirType::Ptr(elem_ty)`.
    fn alloca(&mut self, elem_ty: LirType, count: usize) -> CValue {
        let elem_c = lir_type_to_c_free(&elem_ty, &self.struct_names);
        let ptr_c = format!("{elem_c}*");
        let ptr_ty = LirType::Ptr(Box::new(elem_ty));

        let state = self.current.as_mut().expect("CBackend::alloca: not inside a function");
        let id = state.next_value;
        let slot_name = format!("slot_v{id}");
        let ptr_name = format!("v{id}");

        // Emit array declaration and pointer initialisation into the preamble
        // so the array lives for the entire function (C99 VLA-free version).
        writeln!(state.preamble, "  {elem_c} {slot_name}[{count}];").unwrap();
        writeln!(state.preamble, "  {ptr_c} {ptr_name} = {slot_name};").unwrap();

        state.alloc_value(ptr_ty, ptr_c, ptr_name)
    }

    /// Load through a typed pointer.
    ///
    /// Emits: `ty vN = *ptr;`
    fn ptr_load(&mut self, ptr: CValue, ty: LirType) -> CValue {
        let ptr_name = self.state().name_of(ptr).to_owned();
        let c_type = self.type_to_c(&ty);
        let expr = format!("*{ptr_name}");
        self.state().emit_instr(ty, c_type, &expr)
    }

    /// Store `val` through `ptr`.
    ///
    /// Emits: `*ptr = val;`
    fn ptr_store(&mut self, ptr: CValue, val: CValue) {
        let ptr_name = self.state().name_of(ptr).to_owned();
        let val_name = self.state().name_of(val).to_owned();
        writeln!(self.state().body, "  *{ptr_name} = {val_name};").unwrap();
    }

    /// Element-wise pointer offset.
    ///
    /// Emits: `T* vN = ptr + idx;`
    fn ptr_offset(&mut self, ptr: CValue, idx: CValue) -> CValue {
        let ty = self.state().type_of(ptr).clone();
        let c_type = self.type_to_c(&ty);
        let ptr_name = self.state().name_of(ptr).to_owned();
        let idx_name = self.state().name_of(idx).to_owned();
        let expr = format!("{ptr_name} + {idx_name}");
        self.state().emit_instr(ty, c_type, &expr)
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Convert a `LirType` to its C type string, resolving struct names from the registry.
fn lir_type_to_c_free(ty: &LirType, struct_names: &[String]) -> String {
    match ty {
        LirType::Bool => "bool".to_string(),
        LirType::I8 => "int8_t".to_string(),
        LirType::U8 => "uint8_t".to_string(),
        LirType::I16 => "int16_t".to_string(),
        LirType::U16 => "uint16_t".to_string(),
        LirType::I32 => "int32_t".to_string(),
        LirType::U32 => "uint32_t".to_string(),
        LirType::I64 => "int64_t".to_string(),
        LirType::U64 => "uint64_t".to_string(),
        LirType::Arr(elem, len) => arr_typedef_name(elem, *len),
        LirType::Struct(id) => struct_names[*id as usize].clone(),
        // Native field elements are exposed as their closest C integer type.
        LirType::Native(t) => native_type_to_c(*t).to_string(),
        // Pointer: emit as `inner_type*`.
        LirType::Ptr(inner) => format!("{}*", lir_type_to_c_free(inner, struct_names)),
    }
}

/// Unique suffix for a `LirType`, used in typedef names.
fn lir_type_suffix(ty: &LirType) -> String {
    match ty {
        LirType::Bool => "Bool".to_string(),
        LirType::I8 => "I8".to_string(),
        LirType::U8 => "U8".to_string(),
        LirType::I16 => "I16".to_string(),
        LirType::U16 => "U16".to_string(),
        LirType::I32 => "I32".to_string(),
        LirType::U32 => "U32".to_string(),
        LirType::I64 => "I64".to_string(),
        LirType::U64 => "U64".to_string(),
        LirType::Arr(elem, len) => format!("Arr_{}_{}", lir_type_suffix(elem), len),
        LirType::Struct(id) => format!("S{id}"),
        LirType::Native(t) => format!("Native_{t:?}"),
        LirType::Ptr(inner) => format!("Ptr_{}", lir_type_suffix(inner)),
    }
}

/// C typedef name for `Arr(elem, len)`, e.g. `Arr_U8_16`.
fn arr_typedef_name(elem: &LirType, len: usize) -> String {
    format!("Arr_{}_{}", lir_type_suffix(elem), len)
}

/// Signed C type for arithmetic-shift or signed-comparison casts.
/// Panics on aggregate types (should only be called for scalars).
fn signed_variant(ty: &LirType) -> &'static str {
    match ty {
        LirType::Bool | LirType::I8 | LirType::U8 => "int8_t",
        LirType::I16 | LirType::U16 => "int16_t",
        LirType::I32 | LirType::U32 => "int32_t",
        LirType::I64 | LirType::U64 => "int64_t",
        LirType::Native(t) => native_type_signed(*t),
        LirType::Arr(_, _) | LirType::Struct(_) => panic!("signed_variant: aggregate type"),
        LirType::Ptr(_) => panic!("signed_variant: Ptr has no signed variant"),
    }
}

/// Map a [`NativeType`] to its unsigned C integer type string.
fn native_type_to_c(t: volar_ir_common::Type) -> &'static str {
    use volar_ir_common::Type;
    match t {
        Type::Bit => "bool",
        Type::_8 | Type::AES8 => "uint8_t",
        Type::_16 => "uint16_t",
        Type::_32 => "uint32_t",
        Type::_64 | Type::Galois64 => "uint64_t",
        Type::_128 => "__uint128_t",
        Type::_256 => "uint64_t", // no native 256-bit C integer; use u64 placeholder
        _ => "uint64_t",           // future primitive types: conservative fallback
    }
}

/// Signed C integer for a native type (used in arithmetic-shift casts).
fn native_type_signed(t: volar_ir_common::Type) -> &'static str {
    use volar_ir_common::Type;
    match t {
        Type::Bit => "int8_t",
        Type::_8 | Type::AES8 => "int8_t",
        Type::_16 => "int16_t",
        Type::_32 => "int32_t",
        Type::_64 | Type::Galois64 => "int64_t",
        Type::_128 => "__int128_t",
        Type::_256 => "int64_t",
        _ => "int64_t",
    }
}
