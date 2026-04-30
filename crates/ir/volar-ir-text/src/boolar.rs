// @reliability: experimental
// @ai: assisted
//! [`WriteText`] + [`ParseText`] for [`SavedBIrBlocks`] (the `.vbir` format).
//!
//! Format summary:
//! ```text
//! volar-bir v1
//! begin_block 0
//! params 4
//! v4 = zero
//! v5 = one
//! v6 = and v4 v5
//! v7 = xor v4 v5
//! jmp return args=[v6]
//! end_block
//! ```

use core::fmt;
use alloc::string::ToString;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use crate::WriteText;
use crate::ir::{write_block_target, write_quoted_str, write_var, write_var_list};

pub(crate) const FORMAT_HEADER: &str = "volar-bir v1";

// ============================================================================
// SavedBIrBlocks — public struct
// ============================================================================

/// A complete, serialisable snapshot of a boolean-IR module.
pub struct SavedBIrBlocks {
    pub blocks: BIrBlocks<()>,
}

// ============================================================================
// WriteText for BIrStmt
// ============================================================================

fn write_bir_stmt(result: IRVarId, stmt: &BIrStmt, w: &mut dyn fmt::Write) -> fmt::Result {
    write_var(result, w)?;
    w.write_str(" = ")?;
    match stmt {
        BIrStmt::Zero => { w.write_str("zero")?; }
        BIrStmt::One  => { w.write_str("one")?;  }
        BIrStmt::And(a, b) => {
            w.write_str("and ")?;
            write_var(*a, w)?;
            w.write_char(' ')?;
            write_var(*b, w)?;
        }
        BIrStmt::Or(a, b) => {
            w.write_str("or ")?;
            write_var(*a, w)?;
            w.write_char(' ')?;
            write_var(*b, w)?;
        }
        BIrStmt::Xor(a, b) => {
            w.write_str("xor ")?;
            write_var(*a, w)?;
            w.write_char(' ')?;
            write_var(*b, w)?;
        }
        BIrStmt::Not(a) => {
            w.write_str("not ")?;
            write_var(*a, w)?;
        }
        BIrStmt::OracleCall { name, args, num_bits } => {
            w.write_str("oracle_call ")?;
            write_quoted_str(name, w)?;
            w.write_str(" args=")?;
            write_var_list(args, w)?;
            write!(w, " num_bits={}", num_bits)?;
        }
        BIrStmt::OracleBit { call, bit } => {
            w.write_str("oracle_bit call=")?;
            write_var(*call, w)?;
            write!(w, " bit={}", bit)?;
        }
        BIrStmt::ActionCall { name, guard, args, fallback, num_bits } => {
            w.write_str("action_call ")?;
            write_quoted_str(name, w)?;
            w.write_str(" guard=")?;
            write_var(*guard, w)?;
            w.write_str(" args=")?;
            write_var_list(args, w)?;
            w.write_str(" fallback=")?;
            write_var_list(fallback, w)?;
            write!(w, " num_bits={}", num_bits)?;
        }
        BIrStmt::ActionBit { call, bit } => {
            w.write_str("action_bit call=")?;
            write_var(*call, w)?;
            write!(w, " bit={}", bit)?;
        }
        BIrStmt::Rng { name } => {
            w.write_str("rng ")?;
            write_quoted_str(name, w)?;
        }
        BIrStmt::StorageRead { storage, bit_width, addr } => {
            write!(w, "storage_read storage={} bit_width={} addr=", storage.0, bit_width)?;
            write_var_list(addr, w)?;
        }
        BIrStmt::StorageWrite { storage, src, bit_width, addr } => {
            write!(w, "storage_write storage={} src=", storage.0)?;
            write_var(*src, w)?;
            write!(w, " bit_width={} addr=", bit_width)?;
            write_var_list(addr, w)?;
        }
    }
    w.write_char('\n')
}

// ============================================================================
// WriteText for BIrTerminator
// ============================================================================

fn write_bir_terminator(term: &BIrTerminator, w: &mut dyn fmt::Write) -> fmt::Result {
    match term {
        BIrTerminator::Jmp(target) => {
            w.write_str("jmp ")?;
            write_block_target(&target.block, w)?;
            w.write_str(" args=")?;
            write_var_list(&target.args, w)?;
        }
        BIrTerminator::CondJmp { val, then_target, else_target } => {
            w.write_str("cond_jmp val=")?;
            write_var(*val, w)?;
            w.write_str(" then=")?;
            write_block_target(&then_target.block, w)?;
            w.write_str(" then_args=")?;
            write_var_list(&then_target.args, w)?;
            w.write_str(" else=")?;
            write_block_target(&else_target.block, w)?;
            w.write_str(" else_args=")?;
            write_var_list(&else_target.args, w)?;
        }
    }
    w.write_char('\n')
}

// ============================================================================
// Write a single BIrBlock
// ============================================================================

fn write_bir_block(id: usize, block: &BIrBlock<()>, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "begin_block {}\n", id)?;
    write!(w, "params {}\n", block.params)?;
    let base = block.params;
    for (i, stmt) in block.stmts.iter().enumerate() {
        write_bir_stmt(IRVarId(base + i as u32), stmt, w)?;
    }
    write_bir_terminator(&block.terminator, w)?;
    w.write_str("end_block\n")
}

// ============================================================================
// WriteText for SavedBIrBlocks
// ============================================================================

impl WriteText for SavedBIrBlocks {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        w.write_str(FORMAT_HEADER)?;
        w.write_char('\n')?;
        for (i, block) in self.blocks.0.iter().enumerate() {
            write_bir_block(i, block, w)?;
        }
        Ok(())
    }
}
