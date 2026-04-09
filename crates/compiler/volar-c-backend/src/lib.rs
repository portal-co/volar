// @reliability: normal
//! C99 backend for `LirTarget`.
//!
//! Emits a complete `.c` file suitable for testing with `cc -O0 -std=c99`.
//! Each function becomes a C function. SSA values are `vN` locals; block
//! parameters are pre-declared `blockM_pK` variables at the function top.
//!
//! # Parallel-assignment safety
//!
//! When jumping to a block, all parameter assignments go through temporaries
//! (`_tmpN`) before being stored, avoiding read-after-write hazards regardless
//! of argument ordering.

use std::{
    fmt::Write as FmtWrite,
    string::String,
    vec::Vec,
};
use volar_lir::{IcmpPred, LirTarget, LirType};

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
    /// Maps value ID → LirType (needed for typed temporaries).
    value_type: Vec<LirType>,

    /// Next fresh value ID.
    next_value: u32,
    /// Counter for unique jump-temporary variable names.
    next_tmp: u32,
    /// Which block is currently being emitted (index into `blocks`).
    current_block: Option<u32>,
}

impl FunctionState {
    fn alloc_value(&mut self, ty: LirType, name: String) -> CValue {
        let id = self.next_value;
        self.next_value += 1;
        self.value_name.push(name);
        self.value_type.push(ty);
        CValue(id)
    }

    /// Emit an instruction that produces a fresh `vN` value.
    fn emit_instr(&mut self, ty: LirType, expr: &str) -> CValue {
        let id = self.next_value;
        let name = format!("v{id}");
        let cty = lir_type_to_c(ty);
        writeln!(self.body, "  {cty} v{id} = {expr};").unwrap();
        self.alloc_value(ty, name)
    }

    fn name_of(&self, v: CValue) -> &str {
        &self.value_name[v.0 as usize]
    }

    fn type_of(&self, v: CValue) -> LirType {
        self.value_type[v.0 as usize]
    }

    /// Emit two-phase parallel assignment to `target` block params, then `goto`.
    fn emit_jump(&mut self, target: CBlock, args: &[CValue]) {
        let meta = &self.blocks[target.0 as usize];
        let count = meta.param_count as usize;
        assert_eq!(args.len(), count, "jump arg count mismatch");

        let base_tmp = self.next_tmp;
        self.next_tmp += count as u32;

        // Phase 1: snapshot into globally-unique temporaries.
        for (i, &arg) in args.iter().enumerate() {
            let ty = lir_type_to_c(self.value_type[arg.0 as usize]);
            let src = self.value_name[arg.0 as usize].clone();
            let t = base_tmp + i as u32;
            writeln!(self.body, "  {ty} _t{t} = {src};").unwrap();
        }
        // Phase 2: assign temporaries to block-param variables.
        let block_id = target.0;
        for i in 0..count {
            let t = base_tmp + i as u32;
            writeln!(self.body, "  block{block_id}_p{i} = _t{t};").unwrap();
        }
        writeln!(self.body, "  goto block{block_id};").unwrap();
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
}

impl CBackend {
    pub fn new() -> Self {
        CBackend {
            completed_functions: Vec::new(),
            current: None,
        }
    }

    /// Finalize and return the complete C source.
    pub fn finish(self) -> String {
        let mut out = String::new();
        out.push_str("#include <stdint.h>\n");
        out.push_str("#include <stdbool.h>\n\n");
        for func in self.completed_functions {
            out.push_str(&func);
            out.push('\n');
        }
        out
    }

    fn state(&mut self) -> &mut FunctionState {
        self.current.as_mut().expect("CBackend: not inside a function")
    }

    /// Emit a two-operand instruction.
    fn binop(&mut self, lhs: CValue, op: &str, rhs: CValue) -> CValue {
        let ty = self.state().type_of(lhs);
        let l = self.state().name_of(lhs).to_owned();
        let r = self.state().name_of(rhs).to_owned();
        let expr = format!("{l} {op} {r}");
        self.state().emit_instr(ty, &expr)
    }

    fn unop(&mut self, op: &str, val: CValue) -> CValue {
        let ty = self.state().type_of(val);
        let v = self.state().name_of(val).to_owned();
        let expr = format!("{op}{v}");
        self.state().emit_instr(ty, &expr)
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

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (CBlock, Vec<CValue>) {
        assert!(self.current.is_none(), "begin_function called while inside a function");

        let mut state = FunctionState {
            name: name.to_owned(),
            ret_ty: ret,
            func_param_count: params.len(),
            preamble: String::new(),
            body: String::new(),
            blocks: Vec::new(),
            value_name: Vec::new(),
            value_type: Vec::new(),
            next_value: 0,
            next_tmp: 0,
            current_block: None,
        };

        // Create the entry block (block 0) with no block-param pre-declarations
        // — its "params" are the C function parameters.
        state.blocks.push(BlockMeta { param_count: 0, param_base: 0 });

        // Allocate values for function parameters with names v0, v1, ...
        let param_vals: Vec<CValue> = params
            .iter()
            .enumerate()
            .map(|(i, &ty)| state.alloc_value(ty, format!("v{i}")))
            .collect();

        self.current = Some(state);

        let entry = CBlock(0);
        (entry, param_vals)
    }

    fn end_function(&mut self) {
        let state = self.current.take().expect("end_function called outside a function");

        let ret_cty = state
            .ret_ty
            .map(lir_type_to_c)
            .unwrap_or("void");

        let mut param_list = String::new();
        for i in 0..state.func_param_count {
            if i > 0 {
                param_list.push_str(", ");
            }
            let ty = lir_type_to_c(state.value_type[i]);
            let name = &state.value_name[i];
            param_list.push_str(&format!("{ty} {name}"));
        }

        let mut func = String::new();
        writeln!(func, "{ret_cty} {}({param_list}) {{", state.name).unwrap();
        func.push_str(&state.preamble);
        func.push_str(&state.body);
        writeln!(func, "}}").unwrap();

        self.completed_functions.push(func);
    }

    fn create_block(&mut self) -> CBlock {
        let state = self.state();
        let id = state.blocks.len() as u32;
        state.blocks.push(BlockMeta { param_count: 0, param_base: state.next_value });
        CBlock(id)
    }

    fn add_block_param(&mut self, block: CBlock, ty: LirType) -> CValue {
        let state = self.state();
        let block_id = block.0;
        let param_idx = state.blocks[block_id as usize].param_count;
        state.blocks[block_id as usize].param_count += 1;

        let c_name = format!("block{block_id}_p{param_idx}");
        let cty = lir_type_to_c(ty);
        writeln!(state.preamble, "  {cty} block{block_id}_p{param_idx};").unwrap();

        state.alloc_value(ty, c_name)
    }

    fn switch_to_block(&mut self, block: CBlock) {
        let state = self.state();
        state.current_block = Some(block.0);
        writeln!(state.body, "block{}:;", block.0).unwrap();
    }

    fn iconst(&mut self, ty: LirType, val: i64) -> CValue {
        let expr = format!("{val}");
        self.state().emit_instr(ty, &expr)
    }

    fn add(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "+", rhs) }
    fn sub(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "-", rhs) }
    fn mul(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "*", rhs) }
    fn udiv(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "/", rhs) }
    fn sdiv(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "/", rhs) }

    fn and(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "&", rhs) }
    fn or(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "|", rhs) }
    fn xor(&mut self, lhs: CValue, rhs: CValue) -> CValue { self.binop(lhs, "^", rhs) }
    fn not(&mut self, val: CValue) -> CValue { self.unop("~", val) }
    fn shl(&mut self, val: CValue, shift: CValue) -> CValue { self.binop(val, "<<", shift) }
    fn lshr(&mut self, val: CValue, shift: CValue) -> CValue { self.binop(val, ">>", shift) }
    fn ashr(&mut self, val: CValue, shift: CValue) -> CValue {
        // Arithmetic shift right: cast to signed first.
        let ty = self.state().type_of(val);
        let v = self.state().name_of(val).to_owned();
        let s = self.state().name_of(shift).to_owned();
        let signed_cty = signed_variant(ty);
        let expr = format!("(({signed_cty}){v}) >> {s}");
        self.state().emit_instr(ty, &expr)
    }

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
        // For signed predicates, cast operands to their signed variant.
        let expr = match pred {
            IcmpPred::Slt | IcmpPred::Sle | IcmpPred::Sgt | IcmpPred::Sge => {
                let ty = self.state().type_of(lhs);
                let sc = signed_variant(ty);
                format!("(({sc}){l}) {op} (({sc}){r})")
            }
            _ => format!("{l} {op} {r}"),
        };
        self.state().emit_instr(LirType::Bool, &expr)
    }

    fn zext(&mut self, val: CValue, dst_ty: LirType) -> CValue {
        let src_name = self.state().name_of(val).to_owned();
        let dst_c = lir_type_to_c(dst_ty);
        let expr = format!("({dst_c}){src_name}");
        self.state().emit_instr(dst_ty, &expr)
    }

    fn sext(&mut self, val: CValue, dst_ty: LirType) -> CValue {
        let ty = self.state().type_of(val);
        let src_name = self.state().name_of(val).to_owned();
        let signed_src = signed_variant(ty);
        let dst_c = lir_type_to_c(dst_ty);
        let expr = format!("({dst_c})(({signed_src}){src_name})");
        self.state().emit_instr(dst_ty, &expr)
    }

    fn trunc(&mut self, val: CValue, dst_ty: LirType) -> CValue {
        let src_name = self.state().name_of(val).to_owned();
        let dst_c = lir_type_to_c(dst_ty);
        let expr = format!("({dst_c}){src_name}");
        self.state().emit_instr(dst_ty, &expr)
    }

    fn select(&mut self, cond: CValue, then_val: CValue, else_val: CValue) -> CValue {
        let ty = self.state().type_of(then_val);
        let c = self.state().name_of(cond).to_owned();
        let t = self.state().name_of(then_val).to_owned();
        let e = self.state().name_of(else_val).to_owned();
        let expr = format!("{c} ? {t} : {e}");
        self.state().emit_instr(ty, &expr)
    }

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
        // Then branch: emit param assignments inside the if block (scoped, so no naming conflict).
        {
            let state = self.state();
            let count = state.blocks[then_block.0 as usize].param_count as usize;
            let base = state.next_tmp;
            state.next_tmp += count as u32;
            for (i, &arg) in then_args.iter().enumerate() {
                let ty = lir_type_to_c(state.value_type[arg.0 as usize]);
                let src = state.value_name[arg.0 as usize].clone();
                let t = base + i as u32;
                writeln!(state.body, "    {ty} _t{t} = {src};").unwrap();
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
                let ty = lir_type_to_c(state.value_type[arg.0 as usize]);
                let src = state.value_name[arg.0 as usize].clone();
                let t = base + i as u32;
                writeln!(state.body, "    {ty} _t{t} = {src};").unwrap();
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

fn lir_type_to_c(ty: LirType) -> &'static str {
    match ty {
        LirType::Bool => "bool",
        LirType::I8 => "int8_t",
        LirType::U8 => "uint8_t",
        LirType::I16 => "int16_t",
        LirType::U16 => "uint16_t",
        LirType::I32 => "int32_t",
        LirType::U32 => "uint32_t",
        LirType::I64 => "int64_t",
        LirType::U64 => "uint64_t",
    }
}

fn signed_variant(ty: LirType) -> &'static str {
    match ty {
        LirType::Bool | LirType::I8 | LirType::U8 => "int8_t",
        LirType::I16 | LirType::U16 => "int16_t",
        LirType::I32 | LirType::U32 => "int32_t",
        LirType::I64 | LirType::U64 => "int64_t",
    }
}
