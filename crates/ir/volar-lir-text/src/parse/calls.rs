// @reliability: experimental
// @ai: assisted
//! Parser for [`LirCall`] and [`SavedLirModule`].

use alloc::string::ToString;
use alloc::vec::Vec;
use volar_lir::{FieldDef, IcmpPred, LirType, StructDef};
use volar_lir_saved::{LirCall, SavedLirModule};

use super::{
    error::ParseError,
    lexer::Lexer,
    types::{parse_lir_type, parse_lir_type_list, parse_opt_lir_type},
};
use crate::calls::FORMAT_HEADER;

// ============================================================================
// Entry point
// ============================================================================

pub fn parse_saved_lir_module(s: &str) -> Result<SavedLirModule, ParseError> {
    let mut lex = Lexer::new(s);

    // Version line
    lex.skip();
    if lex.is_eof() {
        return Err(ParseError::MissingVersionLine);
    }
    let version_line = lex.read_to_newline().trim().to_string();
    if version_line != FORMAT_HEADER {
        // If it looks like a version line but wrong version, distinguish the error.
        if version_line.starts_with("volar-lir-saved ") {
            return Err(ParseError::UnsupportedVersion(version_line));
        }
        return Err(ParseError::MissingVersionLine);
    }

    let mut calls: Vec<LirCall> = Vec::new();
    loop {
        lex.skip();
        if lex.is_eof() {
            break;
        }
        let call = parse_lir_call(&mut lex)?;
        calls.push(call);
    }

    Ok(SavedLirModule { calls })
}

// ============================================================================
// Single LirCall
// ============================================================================

fn parse_lir_call(lex: &mut Lexer<'_>) -> Result<LirCall, ParseError> {
    let directive = lex.read_ident()?;
    match directive {
        "define_struct" => {
            lex.expect_key("id")?;
            let id = lex.read_u32()?;
            lex.expect_key("name")?;
            let name = lex.read_string()?;
            lex.expect_key("fields")?;
            let fields = parse_field_def_list(lex)?;
            Ok(LirCall::DefineStruct {
                def: StructDef { name, fields },
                id,
            })
        }

        "begin_function" => {
            lex.expect_key("name")?;
            let name = lex.read_string()?;
            lex.expect_key("params")?;
            let params = parse_lir_type_list(lex)?;
            lex.expect_key("ret")?;
            let ret = parse_opt_lir_type(lex)?;
            lex.expect_key("entry")?;
            let entry_block = lex.read_u32()?;
            lex.expect_key("pvals")?;
            let param_vals = lex.read_u32_list_list()?;
            Ok(LirCall::BeginFunction { name, params, ret, entry_block, param_vals })
        }

        "end_function" => Ok(LirCall::EndFunction),

        "create_block" => {
            lex.expect_key("block")?;
            let block = lex.read_u32()?;
            Ok(LirCall::CreateBlock { block })
        }

        "add_block_param" => {
            lex.expect_key("block")?;
            let block = lex.read_u32()?;
            lex.expect_key("ty")?;
            let ty = parse_lir_type(lex)?;
            lex.expect_key("val")?;
            let val = lex.read_u32()?;
            Ok(LirCall::AddBlockParam { block, ty, val })
        }

        "switch_to_block" => {
            lex.expect_key("block")?;
            let block = lex.read_u32()?;
            Ok(LirCall::SwitchToBlock { block })
        }

        "iconst" => {
            lex.expect_key("ty")?;
            let ty = parse_lir_type(lex)?;
            lex.expect_key("val")?;
            let val = lex.read_i64()?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::Iconst { ty, val, out })
        }

        // ---- Arithmetic -------------------------------------------------------
        "add"  => parse_binop(lex, |lhs, rhs, out| LirCall::Add  { lhs, rhs, out }),
        "sub"  => parse_binop(lex, |lhs, rhs, out| LirCall::Sub  { lhs, rhs, out }),
        "mul"  => parse_binop(lex, |lhs, rhs, out| LirCall::Mul  { lhs, rhs, out }),
        "udiv" => parse_binop(lex, |lhs, rhs, out| LirCall::Udiv { lhs, rhs, out }),
        "sdiv" => parse_binop(lex, |lhs, rhs, out| LirCall::Sdiv { lhs, rhs, out }),

        // ---- Bitwise ----------------------------------------------------------
        "and"  => parse_binop(lex, |lhs, rhs, out| LirCall::And  { lhs, rhs, out }),
        "or"   => parse_binop(lex, |lhs, rhs, out| LirCall::Or   { lhs, rhs, out }),
        "xor"  => parse_binop(lex, |lhs, rhs, out| LirCall::Xor  { lhs, rhs, out }),
        "not"  => {
            lex.expect_key("val")?;
            let val = lex.read_u32()?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::Not { val, out })
        }
        "shl"  => parse_shift(lex, |val, shift, out| LirCall::Shl  { val, shift, out }),
        "lshr" => parse_shift(lex, |val, shift, out| LirCall::Lshr { val, shift, out }),
        "ashr" => parse_shift(lex, |val, shift, out| LirCall::Ashr { val, shift, out }),

        // ---- Comparison -------------------------------------------------------
        "icmp" => {
            lex.expect_key("pred")?;
            let pred = parse_icmp_pred(lex)?;
            lex.expect_key("lhs")?;
            let lhs = lex.read_u32()?;
            lex.expect_key("rhs")?;
            let rhs = lex.read_u32()?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::Icmp { pred, lhs, rhs, out })
        }

        // ---- Conversions ------------------------------------------------------
        "zext"  => parse_conv(lex, |val, dst_ty, out| LirCall::Zext  { val, dst_ty, out }),
        "sext"  => parse_conv(lex, |val, dst_ty, out| LirCall::Sext  { val, dst_ty, out }),
        "trunc" => parse_conv(lex, |val, dst_ty, out| LirCall::Trunc { val, dst_ty, out }),

        // ---- Select -----------------------------------------------------------
        "select" => {
            lex.expect_key("cond")?;
            let cond = lex.read_u32()?;
            lex.expect_key("then")?;
            let then_val = lex.read_u32()?;
            lex.expect_key("else")?;
            let else_val = lex.read_u32()?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::Select { cond, then_val, else_val, out })
        }

        // ---- Terminators ------------------------------------------------------
        "jump" => {
            lex.expect_key("target")?;
            let target = lex.read_u32()?;
            lex.expect_key("args")?;
            let args = lex.read_u32_list()?;
            Ok(LirCall::Jump { target, args })
        }
        "branch" => {
            lex.expect_key("cond")?;
            let cond = lex.read_u32()?;
            lex.expect_key("then_block")?;
            let then_block = lex.read_u32()?;
            lex.expect_key("then_args")?;
            let then_args = lex.read_u32_list()?;
            lex.expect_key("else_block")?;
            let else_block = lex.read_u32()?;
            lex.expect_key("else_args")?;
            let else_args = lex.read_u32_list()?;
            Ok(LirCall::Branch { cond, then_block, then_args, else_block, else_args })
        }
        "ret" => {
            lex.expect_key("vals")?;
            let vals = lex.read_u32_list()?;
            Ok(LirCall::Ret { vals })
        }

        // ---- Extern calls -----------------------------------------------------
        "call_extern" => {
            lex.expect_key("name")?;
            let name = lex.read_string()?;
            lex.expect_key("arg_tys")?;
            let arg_tys = parse_lir_type_list(lex)?;
            lex.expect_key("args")?;
            let args = lex.read_u32_list()?;
            lex.expect_key("ret")?;
            let ret_ty = parse_opt_lir_type(lex)?;
            lex.expect_key("outs")?;
            let outs = lex.read_u32_list()?;
            Ok(LirCall::CallExtern { name, arg_tys, args, ret_ty, outs })
        }

        // ---- Crypto primitives ------------------------------------------------
        "oracle" => {
            lex.expect_key("name")?;
            let name = lex.read_string()?;
            lex.expect_key("arg_tys")?;
            let arg_tys = parse_lir_type_list(lex)?;
            lex.expect_key("args")?;
            let args = lex.read_u32_list()?;
            lex.expect_key("ret_tys")?;
            let ret_tys = parse_lir_type_list(lex)?;
            lex.expect_key("outs")?;
            let outs = lex.read_u32_list()?;
            Ok(LirCall::Oracle { name, arg_tys, args, ret_tys, outs })
        }
        "action" => {
            lex.expect_key("name")?;
            let name = lex.read_string()?;
            lex.expect_key("guard")?;
            let guard = lex.read_u32()?;
            lex.expect_key("arg_tys")?;
            let arg_tys = parse_lir_type_list(lex)?;
            lex.expect_key("args")?;
            let args = lex.read_u32_list()?;
            lex.expect_key("fallbacks")?;
            let fallbacks = lex.read_u32_list()?;
            lex.expect_key("ret_tys")?;
            let ret_tys = parse_lir_type_list(lex)?;
            lex.expect_key("outs")?;
            let outs = lex.read_u32_list()?;
            Ok(LirCall::Action { name, guard, arg_tys, args, fallbacks, ret_tys, outs })
        }
        "rng" => {
            lex.expect_key("ty")?;
            let ty = parse_lir_type(lex)?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::Rng { ty, out })
        }

        // ---- StackAllocExt ----------------------------------------------------
        "alloca" => {
            lex.expect_key("elem")?;
            let elem_ty = parse_lir_type(lex)?;
            lex.expect_key("count")?;
            let count = lex.read_usize()?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::Alloca { elem_ty, count, out })
        }
        "ptr_load" => {
            lex.expect_key("ptr")?;
            let ptr = lex.read_u32()?;
            lex.expect_key("ty")?;
            let ty = parse_lir_type(lex)?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::PtrLoad { ptr, ty, out })
        }
        "ptr_store" => {
            lex.expect_key("ptr")?;
            let ptr = lex.read_u32()?;
            lex.expect_key("val")?;
            let val = lex.read_u32()?;
            Ok(LirCall::PtrStore { ptr, val })
        }
        "ptr_offset" => {
            lex.expect_key("ptr")?;
            let ptr = lex.read_u32()?;
            lex.expect_key("idx")?;
            let idx = lex.read_u32()?;
            lex.expect_key("out")?;
            let out = lex.read_u32()?;
            Ok(LirCall::PtrOffset { ptr, idx, out })
        }
        "ptr_index_load" => {
            lex.expect_key("ptr")?;
            let ptr = lex.read_u32()?;
            lex.expect_key("idx")?;
            let idx = lex.read_u32()?;
            lex.expect_key("pointee")?;
            let pointee_ty = parse_lir_type(lex)?;
            lex.expect_key("outs")?;
            let outs = lex.read_u32_list()?;
            Ok(LirCall::PtrIndexLoad { ptr, idx, pointee_ty, outs })
        }
        "ptr_index_store" => {
            lex.expect_key("ptr")?;
            let ptr = lex.read_u32()?;
            lex.expect_key("idx")?;
            let idx = lex.read_u32()?;
            lex.expect_key("vals")?;
            let vals = lex.read_u32_list()?;
            lex.expect_key("pointee")?;
            let pointee_ty = parse_lir_type(lex)?;
            Ok(LirCall::PtrIndexStore { ptr, idx, vals, pointee_ty })
        }

        // ---- Metadata ---------------------------------------------------------
        "value_type" => {
            lex.expect_key("val")?;
            let val = lex.read_u32()?;
            lex.expect_key("ty")?;
            let ty = parse_lir_type(lex)?;
            Ok(LirCall::ValueType { val, ty })
        }
        "set_prov" => Ok(LirCall::SetProv),

        other => Err(ParseError::UnknownDirective(other.into())),
    }
}

// ============================================================================
// Sub-parsers
// ============================================================================

fn parse_binop<F>(lex: &mut Lexer<'_>, f: F) -> Result<LirCall, ParseError>
where
    F: FnOnce(u32, u32, u32) -> LirCall,
{
    lex.expect_key("lhs")?;
    let lhs = lex.read_u32()?;
    lex.expect_key("rhs")?;
    let rhs = lex.read_u32()?;
    lex.expect_key("out")?;
    let out = lex.read_u32()?;
    Ok(f(lhs, rhs, out))
}

fn parse_shift<F>(lex: &mut Lexer<'_>, f: F) -> Result<LirCall, ParseError>
where
    F: FnOnce(u32, u32, u32) -> LirCall,
{
    lex.expect_key("val")?;
    let val = lex.read_u32()?;
    lex.expect_key("shift")?;
    let shift = lex.read_u32()?;
    lex.expect_key("out")?;
    let out = lex.read_u32()?;
    Ok(f(val, shift, out))
}

fn parse_conv<F>(lex: &mut Lexer<'_>, f: F) -> Result<LirCall, ParseError>
where
    F: FnOnce(u32, LirType, u32) -> LirCall,
{
    lex.expect_key("val")?;
    let val = lex.read_u32()?;
    lex.expect_key("dst")?;
    let dst_ty = parse_lir_type(lex)?;
    lex.expect_key("out")?;
    let out = lex.read_u32()?;
    Ok(f(val, dst_ty, out))
}

fn parse_icmp_pred(lex: &mut Lexer<'_>) -> Result<IcmpPred, ParseError> {
    let tok = lex.read_ident()?;
    match tok {
        "eq"  => Ok(IcmpPred::Eq),
        "ne"  => Ok(IcmpPred::Ne),
        "ult" => Ok(IcmpPred::Ult),
        "ule" => Ok(IcmpPred::Ule),
        "ugt" => Ok(IcmpPred::Ugt),
        "uge" => Ok(IcmpPred::Uge),
        "slt" => Ok(IcmpPred::Slt),
        "sle" => Ok(IcmpPred::Sle),
        "sgt" => Ok(IcmpPred::Sgt),
        "sge" => Ok(IcmpPred::Sge),
        other => Err(ParseError::UnexpectedToken {
            line: lex.pos().line, col: lex.pos().col,
            got: alloc::format!("unknown icmp predicate: {:?}", other),
        }),
    }
}

fn parse_field_def_list(lex: &mut Lexer<'_>) -> Result<Vec<FieldDef>, ParseError> {
    lex.expect_byte(b'[')?;
    let mut out = Vec::new();
    loop {
        lex.skip();
        if lex.try_byte(b']') { break; }
        // Each field: `(<lir_type>, <str>)`
        lex.expect_byte(b'(')?;
        let ty = parse_lir_type(lex)?;
        lex.expect_byte(b',')?;
        let name = lex.read_string()?;
        lex.expect_byte(b')')?;
        out.push(FieldDef { name, ty });
        lex.skip();
        if lex.try_byte(b',') { continue; }
        lex.expect_byte(b']')?;
        break;
    }
    Ok(out)
}
