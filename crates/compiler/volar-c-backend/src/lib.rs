// @reliability: normal
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
use volar_lir::{IcmpPred, LirTarget, LirType, StructDef, StructId};

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
    /// Pre-rendered `typedef struct { ... } Name;` strings.
    struct_typedefs: Vec<String>,
    /// Struct names indexed by `StructId`.
    struct_names: Vec<String>,
    /// Ordered list of array typedefs needed: (typedef_name, elem_ty, len).
    /// Inner arrays appear before outer arrays (insertion order via DFS).
    array_typedefs: Vec<(String, LirType, usize)>,
    /// Set of already-registered array typedef names for deduplication.
    array_typedef_set: BTreeSet<String>,
    /// Rendered `extern RetType name(ArgTypes...);` declarations.
    extern_decls: Vec<String>,
    /// Next StructId to assign.
    next_struct_id: StructId,
}

impl CBackend {
    pub fn new() -> Self {
        CBackend {
            completed_functions: Vec::new(),
            current: None,
            struct_defs: Vec::new(),
            struct_typedefs: Vec::new(),
            struct_names: Vec::new(),
            array_typedefs: Vec::new(),
            array_typedef_set: BTreeSet::new(),
            extern_decls: Vec::new(),
            next_struct_id: 0,
        }
    }

    /// Finalize and return the complete C source.
    ///
    /// Emits (in order):
    /// 1. `#include` headers
    /// 2. Array typedefs (inner-first so nested arrays are valid C)
    /// 3. Struct typedefs
    /// 4. Extern declarations
    /// 5. Function definitions
    pub fn finish(self) -> String {
        let mut out = String::new();
        out.push_str("#include <stdint.h>\n");
        out.push_str("#include <stdbool.h>\n\n");

        // Array typedefs.
        for (name, elem_ty, len) in &self.array_typedefs {
            let elem_c = lir_type_to_c_free(elem_ty, &self.struct_names);
            writeln!(out, "typedef struct {{ {elem_c} data[{len}]; }} {name};").unwrap();
        }
        if !self.array_typedefs.is_empty() {
            out.push('\n');
        }

        // Struct typedefs.
        for s in &self.struct_typedefs {
            out.push_str(s);
        }
        if !self.struct_typedefs.is_empty() {
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
                self.array_typedefs.push((name, elem_clone, *len));
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

        // Render the typedef.
        let mut s = format!("typedef struct {{\n");
        for field in &def.fields {
            let c_type = self.type_to_c(&field.ty);
            writeln!(s, "  {c_type} {};", field.name).unwrap();
        }
        writeln!(s, "}} {};", def.name).unwrap();

        self.struct_names.push(def.name.clone());
        self.struct_typedefs.push(s);
        self.struct_defs.push(def);
        id
    }

    // ---- Function management ------------------------------------------------

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (CBlock, Vec<CValue>) {
        assert!(self.current.is_none(), "begin_function called while inside a function");

        // Register array typedefs for param and return types.
        for ty in params {
            self.register_array_typedef(ty);
        }
        if let Some(ref ty) = ret {
            self.register_array_typedef(ty);
        }

        // Pre-compute C type strings for parameters.
        let param_ctypes: Vec<String> = params.iter().map(|ty| self.type_to_c(ty)).collect();

        let mut state = FunctionState {
            name: name.to_owned(),
            ret_ty: ret,
            func_param_count: params.len(),
            preamble: String::new(),
            body: String::new(),
            // Entry block (block 0) has no block-param pre-declarations.
            blocks: vec![BlockMeta { param_count: 0, param_base: 0 }],
            value_name: Vec::new(),
            value_ctype: Vec::new(),
            value_type: Vec::new(),
            next_value: 0,
            next_tmp: 0,
            current_block: None,
        };

        // Allocate values for function parameters: v0, v1, ...
        let param_vals: Vec<CValue> = params
            .iter()
            .zip(param_ctypes.iter())
            .enumerate()
            .map(|(i, (ty, c_type))| {
                state.alloc_value(ty.clone(), c_type.clone(), format!("v{i}"))
            })
            .collect();

        self.current = Some(state);
        (CBlock(0), param_vals)
    }

    fn end_function(&mut self) {
        let state = self.current.take().expect("end_function called outside a function");

        // Compute return type C string after taking state (self.current is None again).
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
    fn not(&mut self, val: CValue) -> CValue { self.unop("~", val) }
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

    // ---- Array operations ---------------------------------------------------

    fn arr_new(&mut self, elem_ty: LirType, elems: &[CValue]) -> CValue {
        let len = elems.len();
        let arr_ty = LirType::Arr(Box::new(elem_ty.clone()), len);
        self.register_array_typedef(&arr_ty);
        let type_name = arr_typedef_name(&elem_ty, len);

        let elem_names: Vec<String> =
            elems.iter().map(|&v| self.state().name_of(v).to_owned()).collect();
        let data_init = elem_names.join(", ");
        let expr = format!("({type_name}){{ .data = {{ {data_init} }} }}");
        self.state().emit_instr(arr_ty, type_name, &expr)
    }

    fn arr_get(&mut self, arr: CValue, idx: CValue) -> CValue {
        let arr_ty = self.state().type_of(arr).clone();
        let (elem_ty, _len) = match arr_ty {
            LirType::Arr(ref elem, len) => (*elem.clone(), len),
            _ => panic!("arr_get: expected Arr type, got {:?}", arr_ty),
        };
        let elem_c = self.type_to_c(&elem_ty);
        let arr_name = self.state().name_of(arr).to_owned();
        let idx_name = self.state().name_of(idx).to_owned();
        let expr = format!("{arr_name}.data[{idx_name}]");
        self.state().emit_instr(elem_ty, elem_c, &expr)
    }

    fn arr_set(&mut self, arr: CValue, idx: CValue, val: CValue) -> CValue {
        let arr_ty = self.state().type_of(arr).clone();
        let type_name = self.type_to_c(&arr_ty);
        let arr_name = self.state().name_of(arr).to_owned();
        let idx_name = self.state().name_of(idx).to_owned();
        let val_name = self.state().name_of(val).to_owned();

        // Functional update: copy + mutate.
        let id = self.state().next_value;
        let result_name = format!("v{id}");
        writeln!(self.state().body, "  {type_name} {result_name} = {arr_name};").unwrap();
        writeln!(self.state().body, "  {result_name}.data[{idx_name}] = {val_name};").unwrap();
        self.state().alloc_value(arr_ty, type_name, result_name)
    }

    // ---- Struct operations --------------------------------------------------

    fn struct_new(&mut self, id: StructId, fields: &[CValue]) -> CValue {
        let struct_name = self.struct_names[id as usize].clone();
        // Collect field names before borrowing state.
        let field_names: Vec<String> =
            self.struct_defs[id as usize].fields.iter().map(|f| f.name.clone()).collect();
        assert_eq!(fields.len(), field_names.len(), "struct_new: field count mismatch");

        let val_names: Vec<String> =
            fields.iter().map(|&v| self.state().name_of(v).to_owned()).collect();

        let init = field_names
            .iter()
            .zip(val_names.iter())
            .map(|(fname, vname)| format!(".{fname} = {vname}"))
            .collect::<Vec<_>>()
            .join(", ");
        let expr = format!("({struct_name}){{ {init} }}");
        let ty = LirType::Struct(id);
        self.state().emit_instr(ty, struct_name, &expr)
    }

    fn struct_get(&mut self, val: CValue, field_idx: usize) -> CValue {
        let struct_ty = self.state().type_of(val).clone();
        let struct_id = match struct_ty {
            LirType::Struct(id) => id,
            _ => panic!("struct_get: expected Struct type, got {:?}", struct_ty),
        };
        let field = &self.struct_defs[struct_id as usize].fields[field_idx];
        let field_name = field.name.clone();
        let field_ty = field.ty.clone();
        let field_ctype = self.type_to_c(&field_ty);
        let val_name = self.state().name_of(val).to_owned();
        let expr = format!("{val_name}.{field_name}");
        self.state().emit_instr(field_ty, field_ctype, &expr)
    }

    // ---- Extern calls -------------------------------------------------------

    fn call_extern(
        &mut self,
        name: &str,
        ret_ty: Option<LirType>,
        args: &[CValue],
    ) -> Option<CValue> {
        // Collect argument info before any mutable borrows.
        let arg_names: Vec<String> =
            args.iter().map(|&v| self.state().name_of(v).to_owned()).collect();
        let arg_tys: Vec<LirType> =
            args.iter().map(|&v| self.state().type_of(v).clone()).collect();

        let arg_c_tys: Vec<String> = arg_tys.iter().map(|ty| self.type_to_c(ty)).collect();
        let ret_c_ty = ret_ty
            .as_ref()
            .map(|ty| self.type_to_c(ty))
            .unwrap_or_else(|| "void".to_string());

        // Accumulate extern declaration (deduplicate by string).
        let params_str = arg_c_tys.join(", ");
        let extern_decl = format!("extern {ret_c_ty} {name}({params_str});\n");
        if !self.extern_decls.contains(&extern_decl) {
            self.extern_decls.push(extern_decl);
        }

        let args_str = arg_names.join(", ");
        if let Some(ret_ty) = ret_ty {
            let c_type = self.type_to_c(&ret_ty);
            let expr = format!("{name}({args_str})");
            Some(self.state().emit_instr(ret_ty, c_type, &expr))
        } else {
            writeln!(self.state().body, "  {name}({args_str});").unwrap();
            None
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

    fn ret(&mut self, val: Option<CValue>) {
        match val {
            Some(v) => {
                let name = self.state().name_of(v).to_owned();
                writeln!(self.state().body, "  return {name};").unwrap();
            }
            None => {
                writeln!(self.state().body, "  return;").unwrap();
            }
        }
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
        LirType::Arr(_, _) | LirType::Struct(_) => panic!("signed_variant: aggregate type"),
    }
}
