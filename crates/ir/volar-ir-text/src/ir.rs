// @reliability: experimental
// @ai: assisted
//! [`WriteText`] + [`ParseText`] for [`SavedIrBlocks`] (the `.vir` format).
//!
//! Format summary:
//! ```text
//! volar-ir v1
//! type 0 prim bit
//! type 1 prim u8
//! type 2 vec 4 1
//! type 3 tuple [0,1]
//! type 4 block [0,1]
//! type 5 func [0,1]->[2]
//! oracle "foo" params=[0] results=[1]
//! action "bar" params=[0] results=[1]
//! rng "baz" ty=0
//! begin_block 0
//! params [0,1]
//! v2 = const 0x0:0xff ty=1
//! v3 = storage_read storage=0 ty=1 addr=v2
//! v4 = storage_write storage=0 src=v2 ty=1 addr=v3
//! jmp return args=[]
//! end_block
//! ```

use core::fmt;
use volar_ir_common::{
    ActionDecl, Constant, IrType, OracleDecl, RngDecl, StorageId, Stmt, Type, TypeId, TypeTable,
};
use volar_ir::ir::{IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId};
use crate::WriteText;

pub(crate) const FORMAT_HEADER: &str = "volar-ir v1";

// ============================================================================
// SavedIrBlocks — public struct
// ============================================================================

/// A complete, serialisable snapshot of an IR module (type table + blocks).
pub struct SavedIrBlocks {
    pub types:  TypeTable,
    pub blocks: IRBlocks<()>,
}

// ============================================================================
// Helper: write_quoted_str
// ============================================================================

pub(crate) fn write_quoted_str(s: &str, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('"')?;
    for ch in s.chars() {
        match ch {
            '\\' => w.write_str("\\\\")?,
            '"'  => w.write_str("\\\"")?,
            '\n' => w.write_str("\\n")?,
            '\t' => w.write_str("\\t")?,
            '\r' => w.write_str("\\r")?,
            c    => w.write_char(c)?,
        }
    }
    w.write_char('"')
}

// ============================================================================
// Helper: write_var / write_var_list / write_type_id_list / write_constant
// ============================================================================

pub(crate) fn write_var(v: IRVarId, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('v')?;
    write!(w, "{}", v.0)
}

pub(crate) fn write_var_list(vars: &[IRVarId], w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('[')?;
    for (i, v) in vars.iter().enumerate() {
        if i > 0 { w.write_char(',')?; }
        write_var(*v, w)?;
    }
    w.write_char(']')
}

pub(crate) fn write_type_id_list(ids: &[TypeId], w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('[')?;
    for (i, id) in ids.iter().enumerate() {
        if i > 0 { w.write_char(',')?; }
        write!(w, "{}", id.0)?;
    }
    w.write_char(']')
}

pub(crate) fn write_constant(c: &Constant, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "0x{:x}:0x{:x}", c.hi, c.lo)
}

pub(crate) fn write_storage(s: StorageId, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", s.0)
}

// ============================================================================
// Helper: write_block_target
// ============================================================================

pub(crate) fn write_block_target(t: &IRBlockTargetId, w: &mut dyn fmt::Write) -> fmt::Result {
    match t {
        IRBlockTargetId::Return      => w.write_str("return"),
        IRBlockTargetId::Block(id)   => write!(w, "block:{}", id.0),
        IRBlockTargetId::Dyn(var)    => { w.write_str("dyn:")?; write_var(*var, w) }
    }
}

// ============================================================================
// Helper: write_prim_type
// ============================================================================

fn write_prim_type(ty: Type, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_str(match ty {
        Type::Bit      => "bit",
        Type::_8       => "u8",
        Type::_16      => "u16",
        Type::_32      => "u32",
        Type::_64      => "u64",
        Type::_128     => "u128",
        Type::_256     => "u256",
        Type::AES8     => "aes8",
        Type::Galois64 => "galois64",
        _              => "unknown",
    })
}

// ============================================================================
// WriteText for TypeTable
// ============================================================================

impl WriteText for TypeTable {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        for (i, ty) in self.0.iter().enumerate() {
            write!(w, "type {} ", i)?;
            match ty {
                IrType::Primitive(p) => {
                    w.write_str("prim ")?;
                    write_prim_type(*p, w)?;
                }
                IrType::Vec(n, elem) => {
                    write!(w, "vec {} {}", n, elem.0)?;
                }
                IrType::Tuple(tys) => {
                    w.write_str("tuple ")?;
                    write_type_id_list(tys, w)?;
                }
                IrType::Block { params } => {
                    w.write_str("block ")?;
                    write_type_id_list(params, w)?;
                }
                IrType::Func { params, results } => {
                    w.write_str("func ")?;
                    write_type_id_list(params, w)?;
                    w.write_str("->")?;
                    write_type_id_list(results, w)?;
                }
            }
            w.write_char('\n')?;
        }
        Ok(())
    }
}

// ============================================================================
// WriteText for OracleDecl / ActionDecl / RngDecl
// ============================================================================

fn write_oracle_decl(d: &OracleDecl, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_str("oracle ")?;
    write_quoted_str(&d.name, w)?;
    w.write_str(" params=")?;
    write_type_id_list(&d.params, w)?;
    w.write_str(" results=")?;
    write_type_id_list(&d.results, w)?;
    w.write_char('\n')
}

fn write_action_decl(d: &ActionDecl, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_str("action ")?;
    write_quoted_str(&d.name, w)?;
    w.write_str(" params=")?;
    write_type_id_list(&d.params, w)?;
    w.write_str(" results=")?;
    write_type_id_list(&d.results, w)?;
    w.write_char('\n')
}

fn write_rng_decl(d: &RngDecl, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_str("rng ")?;
    write_quoted_str(&d.name, w)?;
    write!(w, " ty={}\n", d.ty.0)
}

// ============================================================================
// WriteText for IRStmt  (Stmt<IRVarId>)
// ============================================================================

fn write_ir_stmt(
    result: IRVarId,
    stmt:   &Stmt<IRVarId>,
    w:      &mut dyn fmt::Write,
) -> fmt::Result {
    write_var(result, w)?;
    w.write_str(" = ")?;
    match stmt {
        Stmt::Const(c, ty) => {
            w.write_str("const ")?;
            write_constant(c, w)?;
            write!(w, " ty={}", ty.0)?;
        }
        Stmt::StorageRead { storage, ty, addr } => {
            w.write_str("storage_read storage=")?;
            write_storage(*storage, w)?;
            write!(w, " ty={} addr=", ty.0)?;
            write_var(*addr, w)?;
        }
        Stmt::StorageWrite { storage, src, ty, addr } => {
            w.write_str("storage_write storage=")?;
            write_storage(*storage, w)?;
            w.write_str(" src=")?;
            write_var(*src, w)?;
            write!(w, " ty={} addr=", ty.0)?;
            write_var(*addr, w)?;
        }
        Stmt::Transmute { src, src_ty, dst_ty } => {
            w.write_str("transmute src=")?;
            write_var(*src, w)?;
            write!(w, " src_ty={} dst_ty={}", src_ty.0, dst_ty.0)?;
        }
        Stmt::Poly { ty, coeffs, constant } => {
            write!(w, "poly ty={} const=", ty.0)?;
            write_constant(constant, w)?;
            for (vars, coeff) in coeffs {
                w.write_str(" coeff=")?;
                write_var_list(vars, w)?;
                write!(w, ":{}", coeff)?;
            }
        }
        Stmt::Rol { src, ty, n } => {
            w.write_str("rol src=")?;
            write_var(*src, w)?;
            write!(w, " ty={} n={}", ty.0, n)?;
        }
        Stmt::Ror { src, ty, n } => {
            w.write_str("ror src=")?;
            write_var(*src, w)?;
            write!(w, " ty={} n={}", ty.0, n)?;
        }
        Stmt::Merge { parts, ty } => {
            write!(w, "merge ty={} parts=", ty.0)?;
            write_var_list(parts, w)?;
        }
        Stmt::Splat { src, ty } => {
            w.write_str("splat src=")?;
            write_var(*src, w)?;
            write!(w, " ty={}", ty.0)?;
        }
        Stmt::Shuffle { result_bits, ty } => {
            write!(w, "shuffle ty={} bits=[", ty.0)?;
            for (i, (bit, var)) in result_bits.iter().enumerate() {
                if i > 0 { w.write_char(',')?; }
                write!(w, "({},", bit)?;
                write_var(*var, w)?;
                w.write_char(')')?;
            }
            w.write_char(']')?;
        }
        Stmt::OracleCall { name, args, output_tys, result_ty } => {
            w.write_str("oracle_call ")?;
            write_quoted_str(name, w)?;
            w.write_str(" args=")?;
            write_var_list(args, w)?;
            w.write_str(" out_tys=")?;
            write_type_id_list(output_tys, w)?;
            write!(w, " result_ty={}", result_ty.0)?;
        }
        Stmt::OracleOutput { call, idx, ty } => {
            w.write_str("oracle_output call=")?;
            write_var(*call, w)?;
            write!(w, " idx={} ty={}", idx, ty.0)?;
        }
        Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } => {
            w.write_str("action_call ")?;
            write_quoted_str(name, w)?;
            w.write_str(" guard=")?;
            write_var(*guard, w)?;
            w.write_str(" args=")?;
            write_var_list(args, w)?;
            w.write_str(" fallbacks=")?;
            write_var_list(fallbacks, w)?;
            w.write_str(" out_tys=")?;
            write_type_id_list(output_tys, w)?;
            write!(w, " result_ty={}", result_ty.0)?;
        }
        Stmt::ActionOutput { call, idx, ty } => {
            w.write_str("action_output call=")?;
            write_var(*call, w)?;
            write!(w, " idx={} ty={}", idx, ty.0)?;
        }
        Stmt::Rng { name, ty } => {
            w.write_str("rng ")?;
            write_quoted_str(name, w)?;
            write!(w, " ty={}", ty.0)?;
        }
    }
    w.write_char('\n')
}

// ============================================================================
// WriteText for IRTerminator
// ============================================================================

fn write_ir_terminator(term: &IRTerminator, w: &mut dyn fmt::Write) -> fmt::Result {
    match term {
        IRTerminator::Jmp { func, args } => {
            w.write_str("jmp ")?;
            write_block_target(func, w)?;
            w.write_str(" args=")?;
            write_var_list(args, w)?;
        }
        IRTerminator::JumpCond { condition, true_block, true_args, false_block, false_args } => {
            w.write_str("jmp_cond cond=")?;
            write_var(*condition, w)?;
            w.write_str(" then=")?;
            write_block_target(true_block, w)?;
            w.write_str(" then_args=")?;
            write_var_list(true_args, w)?;
            w.write_str(" else=")?;
            write_block_target(false_block, w)?;
            w.write_str(" else_args=")?;
            write_var_list(false_args, w)?;
        }
        IRTerminator::JumpTable { index, cases } => {
            w.write_str("jmp_table index=")?;
            write_var(*index, w)?;
            for (constant, (target, args)) in cases {
                w.write_str(" case ")?;
                write_constant(constant, w)?;
                w.write_str(" -> ")?;
                write_block_target(target, w)?;
                w.write_str(" args=")?;
                write_var_list(args, w)?;
            }
        }
    }
    w.write_char('\n')
}

// ============================================================================
// Write a single IRBlock
// ============================================================================

fn write_ir_block(id: usize, block: &IRBlock<()>, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "begin_block {}\n", id)?;
    w.write_str("params ")?;
    write_type_id_list(&block.params, w)?;
    w.write_char('\n')?;
    let base = block.params.len() as u32;
    for (i, stmt) in block.stmts.iter().enumerate() {
        write_ir_stmt(IRVarId(base + i as u32), stmt, w)?;
    }
    write_ir_terminator(&block.terminator, w)?;
    w.write_str("end_block\n")
}

// ============================================================================
// WriteText for SavedIrBlocks
// ============================================================================

impl WriteText for SavedIrBlocks {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        w.write_str(FORMAT_HEADER)?;
        w.write_char('\n')?;
        self.types.write_text(w)?;
        for d in &self.blocks.oracles { write_oracle_decl(d, w)?; }
        for d in &self.blocks.actions { write_action_decl(d, w)?; }
        for d in &self.blocks.rngs    { write_rng_decl(d, w)?;    }
        for (i, block) in self.blocks.blocks.iter().enumerate() {
            write_ir_block(i, block, w)?;
        }
        Ok(())
    }
}
