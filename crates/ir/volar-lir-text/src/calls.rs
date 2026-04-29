// @reliability: experimental
// @ai: assisted
//! [`WriteText`] implementations for [`LirCall`] and [`SavedLirModule`].

use crate::{
    types::{
        write_lir_type_list, write_opt_lir_type, write_quoted_str, write_u32_list,
        write_u32_list_list,
    },
    WriteText,
};
use core::fmt;
use volar_lir_saved::{LirCall, SavedLirModule};

// ============================================================================
// Header constant
// ============================================================================

pub const FORMAT_HEADER: &str = "volar-lir-saved v1";

// ============================================================================
// SavedLirModule
// ============================================================================

impl WriteText for SavedLirModule {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        w.write_str(FORMAT_HEADER)?;
        w.write_char('\n')?;
        for call in &self.calls {
            call.write_text(w)?;
            w.write_char('\n')?;
        }
        Ok(())
    }
}

// ============================================================================
// LirCall
// ============================================================================

impl WriteText for LirCall {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        match self {
            // ---- Type registration -------------------------------------------
            LirCall::DefineStruct { def, id } => {
                write!(w, "define_struct id={} ", id)?;
                def.write_text(w)
            }

            // ---- Function management -----------------------------------------
            LirCall::BeginFunction { name, params, ret, entry_block, param_vals } => {
                w.write_str("begin_function name=")?;
                write_quoted_str(name, w)?;
                w.write_str(" params=")?;
                write_lir_type_list(params, w)?;
                w.write_str(" ret=")?;
                write_opt_lir_type(ret, w)?;
                write!(w, " entry={} pvals=", entry_block)?;
                write_u32_list_list(param_vals, w)
            }
            LirCall::EndFunction => w.write_str("end_function"),

            // ---- Block management --------------------------------------------
            LirCall::CreateBlock { block } => {
                write!(w, "create_block block={}", block)
            }
            LirCall::AddBlockParam { block, ty, val } => {
                write!(w, "add_block_param block={} ty=", block)?;
                ty.write_text(w)?;
                write!(w, " val={}", val)
            }
            LirCall::SwitchToBlock { block } => {
                write!(w, "switch_to_block block={}", block)
            }

            // ---- Constants --------------------------------------------------
            LirCall::Iconst { ty, val, out } => {
                w.write_str("iconst ty=")?;
                ty.write_text(w)?;
                write!(w, " val={} out={}", val, out)
            }

            // ---- Arithmetic -------------------------------------------------
            LirCall::Add { lhs, rhs, out } => write!(w, "add lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Sub { lhs, rhs, out } => write!(w, "sub lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Mul { lhs, rhs, out } => write!(w, "mul lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Udiv { lhs, rhs, out } => write!(w, "udiv lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Sdiv { lhs, rhs, out } => write!(w, "sdiv lhs={} rhs={} out={}", lhs, rhs, out),

            // ---- Bitwise ----------------------------------------------------
            LirCall::And  { lhs, rhs, out } => write!(w, "and lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Or   { lhs, rhs, out } => write!(w, "or lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Xor  { lhs, rhs, out } => write!(w, "xor lhs={} rhs={} out={}", lhs, rhs, out),
            LirCall::Not  { val, out }       => write!(w, "not val={} out={}", val, out),
            LirCall::Shl  { val, shift, out } => write!(w, "shl val={} shift={} out={}", val, shift, out),
            LirCall::Lshr { val, shift, out } => write!(w, "lshr val={} shift={} out={}", val, shift, out),
            LirCall::Ashr { val, shift, out } => write!(w, "ashr val={} shift={} out={}", val, shift, out),

            // ---- Comparison -------------------------------------------------
            LirCall::Icmp { pred, lhs, rhs, out } => {
                w.write_str("icmp pred=")?;
                pred.write_text(w)?;
                write!(w, " lhs={} rhs={} out={}", lhs, rhs, out)
            }

            // ---- Conversions ------------------------------------------------
            LirCall::Zext  { val, dst_ty, out } => { w.write_str("zext val=")?;  write!(w, "{} dst=", val)?; dst_ty.write_text(w)?; write!(w, " out={}", out) }
            LirCall::Sext  { val, dst_ty, out } => { w.write_str("sext val=")?;  write!(w, "{} dst=", val)?; dst_ty.write_text(w)?; write!(w, " out={}", out) }
            LirCall::Trunc { val, dst_ty, out } => { w.write_str("trunc val=")?; write!(w, "{} dst=", val)?; dst_ty.write_text(w)?; write!(w, " out={}", out) }

            // ---- Select -----------------------------------------------------
            LirCall::Select { cond, then_val, else_val, out } => {
                write!(w, "select cond={} then={} else={} out={}", cond, then_val, else_val, out)
            }

            // ---- Terminators ------------------------------------------------
            LirCall::Jump { target, args } => {
                write!(w, "jump target={} args=", target)?;
                write_u32_list(args, w)
            }
            LirCall::Branch { cond, then_block, then_args, else_block, else_args } => {
                write!(w, "branch cond={} then_block={} then_args=", cond, then_block)?;
                write_u32_list(then_args, w)?;
                write!(w, " else_block={} else_args=", else_block)?;
                write_u32_list(else_args, w)
            }
            LirCall::Ret { vals } => {
                w.write_str("ret vals=")?;
                write_u32_list(vals, w)
            }

            // ---- Extern calls -----------------------------------------------
            LirCall::CallExtern { name, arg_tys, args, ret_ty, outs } => {
                w.write_str("call_extern name=")?;
                write_quoted_str(name, w)?;
                w.write_str(" arg_tys=")?;
                write_lir_type_list(arg_tys, w)?;
                w.write_str(" args=")?;
                write_u32_list(args, w)?;
                w.write_str(" ret=")?;
                write_opt_lir_type(ret_ty, w)?;
                w.write_str(" outs=")?;
                write_u32_list(outs, w)
            }

            // ---- Crypto primitives ------------------------------------------
            LirCall::Oracle { name, arg_tys, args, ret_tys, outs } => {
                w.write_str("oracle name=")?;
                write_quoted_str(name, w)?;
                w.write_str(" arg_tys=")?;
                write_lir_type_list(arg_tys, w)?;
                w.write_str(" args=")?;
                write_u32_list(args, w)?;
                w.write_str(" ret_tys=")?;
                write_lir_type_list(ret_tys, w)?;
                w.write_str(" outs=")?;
                write_u32_list(outs, w)
            }
            LirCall::Action { name, guard, arg_tys, args, fallbacks, ret_tys, outs } => {
                w.write_str("action name=")?;
                write_quoted_str(name, w)?;
                write!(w, " guard={} arg_tys=", guard)?;
                write_lir_type_list(arg_tys, w)?;
                w.write_str(" args=")?;
                write_u32_list(args, w)?;
                w.write_str(" fallbacks=")?;
                write_u32_list(fallbacks, w)?;
                w.write_str(" ret_tys=")?;
                write_lir_type_list(ret_tys, w)?;
                w.write_str(" outs=")?;
                write_u32_list(outs, w)
            }
            LirCall::Rng { ty, out } => {
                w.write_str("rng ty=")?;
                ty.write_text(w)?;
                write!(w, " out={}", out)
            }

            // ---- StackAllocExt ----------------------------------------------
            LirCall::Alloca { elem_ty, count, out } => {
                w.write_str("alloca elem=")?;
                elem_ty.write_text(w)?;
                write!(w, " count={} out={}", count, out)
            }
            LirCall::PtrLoad { ptr, ty, out } => {
                write!(w, "ptr_load ptr={} ty=", ptr)?;
                ty.write_text(w)?;
                write!(w, " out={}", out)
            }
            LirCall::PtrStore { ptr, val } => {
                write!(w, "ptr_store ptr={} val={}", ptr, val)
            }
            LirCall::PtrOffset { ptr, idx, out } => {
                write!(w, "ptr_offset ptr={} idx={} out={}", ptr, idx, out)
            }
            LirCall::PtrIndexLoad { ptr, idx, pointee_ty, outs } => {
                write!(w, "ptr_index_load ptr={} idx={} pointee=", ptr, idx)?;
                pointee_ty.write_text(w)?;
                w.write_str(" outs=")?;
                write_u32_list(outs, w)
            }
            LirCall::PtrIndexStore { ptr, idx, vals, pointee_ty } => {
                write!(w, "ptr_index_store ptr={} idx={} vals=", ptr, idx)?;
                write_u32_list(vals, w)?;
                w.write_str(" pointee=")?;
                pointee_ty.write_text(w)
            }

            // ---- Metadata ---------------------------------------------------
            LirCall::ValueType { val, ty } => {
                write!(w, "value_type val={} ty=", val)?;
                ty.write_text(w)
            }
            LirCall::SetProv => w.write_str("set_prov"),
        }
    }
}
