// @reliability: experimental
// @ai: assisted
//! Parser for [`SavedIrBlocks`] and [`SavedBIrBlocks`].

use alloc::collections::BTreeMap;
use alloc::string::ToString;
use alloc::vec::Vec;

use volar_ir_common::{
    ActionDecl, Constant, IrType, OracleDecl, RngDecl, StorageId, Stmt, Type, TypeId, TypeTable,
};
use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId,
};
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};

use super::{error::ParseError, lexer::Lexer};
use crate::{ir::{FORMAT_HEADER as IR_HEADER, SavedIrBlocks}, boolar::{FORMAT_HEADER as BIR_HEADER, SavedBIrBlocks}};

// ============================================================================
// Helpers
// ============================================================================

fn mk_var(raw: u32) -> IRVarId { IRVarId(raw) }
fn mk_type(raw: u32) -> TypeId { TypeId(raw) }
fn mk_storage(raw: u32) -> StorageId { StorageId(raw) }
fn mk_block_id(raw: u32) -> IRBlockId { IRBlockId(raw) }

fn read_constant(lex: &mut Lexer) -> Result<Constant, ParseError> {
    let hi = lex.read_u128()?;
    lex.expect_byte(b':')?;
    let lo = lex.read_u128()?;
    Ok(Constant { hi, lo })
}

fn read_block_target(lex: &mut Lexer) -> Result<IRBlockTargetId, ParseError> {
    let kw = lex.read_ident()?;
    match kw {
        "return" => Ok(IRBlockTargetId::Return),
        "block"  => { lex.expect_byte(b':')?; let n = lex.read_u32()?; Ok(IRBlockTargetId::Block(mk_block_id(n))) }
        "dyn"    => { lex.expect_byte(b':')?; let v = lex.read_var()?; Ok(IRBlockTargetId::Dyn(mk_var(v))) }
        other    => Err(ParseError::UnknownDirective(other.into())),
    }
}

fn read_var_id_list(lex: &mut Lexer) -> Result<Vec<IRVarId>, ParseError> {
    Ok(lex.read_var_list()?.into_iter().map(mk_var).collect())
}

fn read_type_id_list(lex: &mut Lexer) -> Result<Vec<TypeId>, ParseError> {
    Ok(lex.read_u32_list()?.into_iter().map(mk_type).collect())
}

// ============================================================================
// Parse TypeTable
// ============================================================================

fn parse_type_table(lex: &mut Lexer) -> Result<TypeTable, ParseError> {
    let mut types: Vec<IrType> = Vec::new();
    loop {
        // peek at next directive: stop if not "type"
        let saved_pos = {
            // We need a multi-token lookahead — easiest is to try read_ident and
            // put back if it's not "type".  We clone the lexer state (only need pos+line+col).
            lex.skip();
            lex.pos()
        };
        // We can't "unread" so we'll check bytes manually.
        let remaining = lex.read_to_newline(); // tentative – need to not consume
        // Actually, instead of lookahead: parse_type_table is called first;
        // non-"type" lines are handled by the outer loop returning TypeTable.
        // We use a peek approach via read_ident, but we need to not consume if wrong.
        // Solution: restart lex from scratch is impossible. Use the approach of
        // parsing in a loop and stopping on the first non-"type" token.
        // We already consumed a line above. This won't work.
        // REVISED DESIGN: parse the whole file with a single dispatch loop.
        let _ = remaining;
        let _ = saved_pos;
        break;
    }
    // This function is not used directly; see parse_saved_ir_blocks.
    Ok(TypeTable(types))
}

fn parse_ir_type(kw: &str, lex: &mut Lexer) -> Result<IrType, ParseError> {
    match kw {
        "prim" => {
            let prim_kw = lex.read_ident()?;
            let ty = match prim_kw {
                "bit"      => Type::Bit,
                "u8"       => Type::_8,
                "u16"      => Type::_16,
                "u32"      => Type::_32,
                "u64"      => Type::_64,
                "u128"     => Type::_128,
                "u256"     => Type::_256,
                "aes8"     => Type::AES8,
                "galois64" => Type::Galois64,
                other      => return Err(ParseError::UnknownPrimType(other.into())),
            };
            Ok(IrType::Primitive(ty))
        }
        "vec" => {
            let n = lex.read_usize()?;
            let elem = mk_type(lex.read_u32()?);
            Ok(IrType::Vec(n, elem))
        }
        "tuple" => {
            let ids = read_type_id_list(lex)?;
            Ok(IrType::Tuple(ids))
        }
        "block" => {
            let params = read_type_id_list(lex)?;
            Ok(IrType::Block { params })
        }
        "func" => {
            let params = read_type_id_list(lex)?;
            lex.expect_str("->")?;
            let results = read_type_id_list(lex)?;
            Ok(IrType::Func { params, results })
        }
        other => Err(ParseError::UnknownDirective(other.into())),
    }
}

// ============================================================================
// Parse IRStmt
// ============================================================================

fn parse_ir_stmt(kw: &str, lex: &mut Lexer) -> Result<Stmt<IRVarId>, ParseError> {
    match kw {
        "const" => {
            let c = read_constant(lex)?;
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            Ok(Stmt::Const(c, ty))
        }
        "storage_read" => {
            lex.expect_key("storage")?;
            let storage = mk_storage(lex.read_u32()?);
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("addr")?;
            let addr = mk_var(lex.read_var()?);
            Ok(Stmt::StorageRead { storage, ty, addr })
        }
        "storage_write" => {
            lex.expect_key("storage")?;
            let storage = mk_storage(lex.read_u32()?);
            lex.expect_key("src")?;
            let src = mk_var(lex.read_var()?);
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("addr")?;
            let addr = mk_var(lex.read_var()?);
            Ok(Stmt::StorageWrite { storage, src, ty, addr })
        }
        "transmute" => {
            lex.expect_key("src")?;
            let src = mk_var(lex.read_var()?);
            lex.expect_key("src_ty")?;
            let src_ty = mk_type(lex.read_u32()?);
            lex.expect_key("dst_ty")?;
            let dst_ty = mk_type(lex.read_u32()?);
            Ok(Stmt::Transmute { src, src_ty, dst_ty })
        }
        "poly" => {
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("const")?;
            let constant = read_constant(lex)?;
            let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
            while lex.peek_byte() == Some(b'c') {
                let kw2 = lex.read_ident()?;
                if kw2 != "coeff" { break; }
                lex.expect_byte(b'=')?;
                let vars: Vec<IRVarId> = lex.read_var_list()?.into_iter().map(mk_var).collect();
                lex.expect_byte(b':')?;
                let coeff = lex.read_u32()? as u8;
                coeffs.insert(vars, coeff);
            }
            Ok(Stmt::Poly { ty, coeffs, constant })
        }
        "rol" => {
            lex.expect_key("src")?;
            let src = mk_var(lex.read_var()?);
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("n")?;
            let n = lex.read_usize()?;
            Ok(Stmt::Rol { src, ty, n })
        }
        "ror" => {
            lex.expect_key("src")?;
            let src = mk_var(lex.read_var()?);
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("n")?;
            let n = lex.read_usize()?;
            Ok(Stmt::Ror { src, ty, n })
        }
        "merge" => {
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("parts")?;
            let parts = read_var_id_list(lex)?;
            Ok(Stmt::Merge { parts, ty })
        }
        "splat" => {
            lex.expect_key("src")?;
            let src = mk_var(lex.read_var()?);
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            Ok(Stmt::Splat { src, ty })
        }
        "shuffle" => {
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            lex.expect_key("bits")?;
            lex.expect_byte(b'[')?;
            let mut result_bits: Vec<(u8, IRVarId)> = Vec::new();
            loop {
                lex.skip();
                if lex.try_byte(b']') { break; }
                lex.expect_byte(b'(')?;
                let bit = lex.read_u32()? as u8;
                lex.expect_byte(b',')?;
                let v = mk_var(lex.read_var()?);
                lex.expect_byte(b')')?;
                result_bits.push((bit, v));
                lex.skip();
                if lex.try_byte(b',') { continue; }
                lex.expect_byte(b']')?;
                break;
            }
            Ok(Stmt::Shuffle { result_bits, ty })
        }
        "oracle_call" => {
            let name = lex.read_string()?;
            lex.expect_key("args")?;
            let args = read_var_id_list(lex)?;
            lex.expect_key("out_tys")?;
            let output_tys = read_type_id_list(lex)?;
            lex.expect_key("result_ty")?;
            let result_ty = mk_type(lex.read_u32()?);
            Ok(Stmt::OracleCall { name, args, output_tys, result_ty })
        }
        "oracle_output" => {
            lex.expect_key("call")?;
            let call = mk_var(lex.read_var()?);
            lex.expect_key("idx")?;
            let idx = lex.read_usize()?;
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            Ok(Stmt::OracleOutput { call, idx, ty })
        }
        "action_call" => {
            let name = lex.read_string()?;
            lex.expect_key("guard")?;
            let guard = mk_var(lex.read_var()?);
            lex.expect_key("args")?;
            let args = read_var_id_list(lex)?;
            lex.expect_key("fallbacks")?;
            let fallbacks = read_var_id_list(lex)?;
            lex.expect_key("out_tys")?;
            let output_tys = read_type_id_list(lex)?;
            lex.expect_key("result_ty")?;
            let result_ty = mk_type(lex.read_u32()?);
            Ok(Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty })
        }
        "action_output" => {
            lex.expect_key("call")?;
            let call = mk_var(lex.read_var()?);
            lex.expect_key("idx")?;
            let idx = lex.read_usize()?;
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            Ok(Stmt::ActionOutput { call, idx, ty })
        }
        "rng" => {
            let name = lex.read_string()?;
            lex.expect_key("ty")?;
            let ty = mk_type(lex.read_u32()?);
            Ok(Stmt::Rng { name, ty })
        }
        other => {
            let p = lex.pos();
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("unknown IR stmt '{}'", other),
            })
        }
    }
}

// ============================================================================
// Parse IRTerminator
// ============================================================================

fn parse_ir_terminator(kw: &str, lex: &mut Lexer) -> Result<IRTerminator, ParseError> {
    match kw {
        "jmp" => {
            let func = read_block_target(lex)?;
            lex.expect_key("args")?;
            let args = read_var_id_list(lex)?;
            Ok(IRTerminator::Jmp { func, args })
        }
        "jmp_cond" => {
            lex.expect_key("cond")?;
            let condition = mk_var(lex.read_var()?);
            lex.expect_key("then")?;
            let true_block = read_block_target(lex)?;
            lex.expect_key("then_args")?;
            let true_args = read_var_id_list(lex)?;
            lex.expect_key("else")?;
            let false_block = read_block_target(lex)?;
            lex.expect_key("else_args")?;
            let false_args = read_var_id_list(lex)?;
            Ok(IRTerminator::JumpCond { condition, true_block, true_args, false_block, false_args })
        }
        "jmp_table" => {
            lex.expect_key("index")?;
            let index = mk_var(lex.read_var()?);
            let mut cases = BTreeMap::new();
            while lex.peek_byte() == Some(b'c') {
                let kw2 = lex.read_ident()?;
                if kw2 != "case" { break; }
                let constant = read_constant(lex)?;
                lex.expect_str("->")?;
                let target = read_block_target(lex)?;
                lex.expect_key("args")?;
                let args = read_var_id_list(lex)?;
                cases.insert(constant, (target, args));
            }
            Ok(IRTerminator::JumpTable { index, cases })
        }
        other => {
            let p = lex.pos();
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("unknown terminator '{}'", other),
            })
        }
    }
}

// ============================================================================
// Parse a single IRBlock
// ============================================================================

fn parse_ir_block(lex: &mut Lexer) -> Result<IRBlock<()>, ParseError> {
    // parse `params [...]`
    let kw = lex.read_ident()?;
    if kw != "params" {
        return Err(ParseError::MissingField("params".into()));
    }
    let param_ids: Vec<TypeId> = read_type_id_list(lex)?;

    let mut stmts: Vec<Stmt<IRVarId>> = Vec::new();
    let mut stmt_provs: Vec<()> = Vec::new();
    let mut terminator: Option<IRTerminator> = None;

    loop {
        lex.skip();
        if lex.is_eof() { break; }

        // Check for `end_block`
        // Check if next bytes look like a var reference `vN` or a keyword.
        let is_var = lex.peek_byte() == Some(b'v');

        if is_var {
            // `vN = <stmt_kw> ...`
            let _var_idx = lex.read_var()?; // consume, ignore (sequential)
            lex.expect_byte(b'=')?;
            let stmt_kw = lex.read_ident()?;
            let stmt = parse_ir_stmt(stmt_kw, lex)?;
            stmts.push(stmt);
            stmt_provs.push(());
        } else {
            // keyword: either terminator or `end_block`
            let kw = lex.read_ident()?;
            if kw == "end_block" { break; }
            let term = parse_ir_terminator(kw, lex)?;
            terminator = Some(term);
            // expect end_block
            let end_kw = lex.read_ident()?;
            if end_kw != "end_block" {
                return Err(ParseError::UnexpectedToken {
                    line: lex.pos().line, col: lex.pos().col,
                    got: alloc::format!("expected end_block, got '{}'", end_kw),
                });
            }
            break;
        }
    }

    let terminator = terminator.ok_or(ParseError::MissingField("terminator".into()))?;

    Ok(IRBlock {
        params: param_ids,
        stmts,
        stmt_provs,
        terminator,
    })
}

// ============================================================================
// ParseText for SavedIrBlocks
// ============================================================================

pub(crate) fn parse_saved_ir_blocks(s: &str) -> Result<SavedIrBlocks, ParseError> {
    let mut lex = Lexer::new(s);

    // Check version header
    let header = lex.read_to_newline().trim().to_string();
    if header.is_empty() {
        return Err(ParseError::MissingVersionLine);
    }
    if header != IR_HEADER {
        if header.starts_with("volar-ir v") {
            return Err(ParseError::UnsupportedVersion(header));
        }
        return Err(ParseError::MissingVersionLine);
    }

    let mut types:   Vec<IrType>    = Vec::new();
    let mut oracles: Vec<OracleDecl> = Vec::new();
    let mut actions: Vec<ActionDecl> = Vec::new();
    let mut rngs:    Vec<RngDecl>    = Vec::new();
    let mut blocks:  Vec<IRBlock<()>> = Vec::new();

    loop {
        lex.skip();
        if lex.is_eof() { break; }

        let directive = lex.read_ident()?;

        match directive {
            "type" => {
                let _idx = lex.read_u32()?;  // ignored; must be sequential
                let ty_kw = lex.read_ident()?;
                let ty = parse_ir_type(ty_kw, &mut lex)?;
                types.push(ty);
            }
            "oracle" => {
                let name = lex.read_string()?;
                lex.expect_key("params")?;
                let params = read_type_id_list(&mut lex)?;
                lex.expect_key("results")?;
                let results = read_type_id_list(&mut lex)?;
                oracles.push(OracleDecl { name, params, results });
            }
            "action" => {
                let name = lex.read_string()?;
                lex.expect_key("params")?;
                let params = read_type_id_list(&mut lex)?;
                lex.expect_key("results")?;
                let results = read_type_id_list(&mut lex)?;
                actions.push(ActionDecl { name, params, results });
            }
            "rng" => {
                let name = lex.read_string()?;
                lex.expect_key("ty")?;
                let ty = mk_type(lex.read_u32()?);
                rngs.push(RngDecl { name, ty });
            }
            "begin_block" => {
                let _block_id = lex.read_u32()?; // ignored; sequential
                let block = parse_ir_block(&mut lex)?;
                blocks.push(block);
            }
            other => return Err(ParseError::UnknownDirective(other.into())),
        }
    }

    Ok(SavedIrBlocks {
        types: TypeTable(types),
        blocks: IRBlocks { oracles, actions, rngs, blocks, pre_init: vec![] },
    })
}

// ============================================================================
// Parse BIrStmt
// ============================================================================

fn parse_bir_stmt(kw: &str, lex: &mut Lexer) -> Result<BIrStmt, ParseError> {
    match kw {
        "zero" => Ok(BIrStmt::Zero),
        "one"  => Ok(BIrStmt::One),
        "and"  => { let a = mk_var(lex.read_var()?); let b = mk_var(lex.read_var()?); Ok(BIrStmt::And(a, b)) }
        "or"   => { let a = mk_var(lex.read_var()?); let b = mk_var(lex.read_var()?); Ok(BIrStmt::Or(a, b)) }
        "xor"  => { let a = mk_var(lex.read_var()?); let b = mk_var(lex.read_var()?); Ok(BIrStmt::Xor(a, b)) }
        "not"  => { let a = mk_var(lex.read_var()?); Ok(BIrStmt::Not(a)) }
        "oracle_call" => {
            let name = lex.read_string()?;
            lex.expect_key("args")?;
            let args = read_var_id_list(lex)?;
            lex.expect_key("num_bits")?;
            let num_bits = lex.read_usize()?;
            Ok(BIrStmt::OracleCall { name, args, num_bits })
        }
        "oracle_bit" => {
            lex.expect_key("call")?;
            let call = mk_var(lex.read_var()?);
            lex.expect_key("bit")?;
            let bit = lex.read_usize()?;
            Ok(BIrStmt::OracleBit { call, bit })
        }
        "action_call" => {
            let name = lex.read_string()?;
            lex.expect_key("guard")?;
            let guard = mk_var(lex.read_var()?);
            lex.expect_key("args")?;
            let args = read_var_id_list(lex)?;
            lex.expect_key("fallback")?;
            let fallback = read_var_id_list(lex)?;
            lex.expect_key("num_bits")?;
            let num_bits = lex.read_usize()?;
            Ok(BIrStmt::ActionCall { name, guard, args, fallback, num_bits })
        }
        "action_bit" => {
            lex.expect_key("call")?;
            let call = mk_var(lex.read_var()?);
            lex.expect_key("bit")?;
            let bit = lex.read_usize()?;
            Ok(BIrStmt::ActionBit { call, bit })
        }
        "rng" => {
            let name = lex.read_string()?;
            Ok(BIrStmt::Rng { name })
        }
        "storage_read" => {
            lex.expect_key("storage")?;
            let storage = mk_storage(lex.read_u32()?);
            lex.expect_key("bit_width")?;
            let bit_width = lex.read_usize()?;
            lex.expect_key("addr")?;
            let addr = read_var_id_list(lex)?;
            Ok(BIrStmt::StorageRead { storage, bit_width, addr })
        }
        "storage_write" => {
            lex.expect_key("storage")?;
            let storage = mk_storage(lex.read_u32()?);
            lex.expect_key("src")?;
            let src = mk_var(lex.read_var()?);
            lex.expect_key("bit_width")?;
            let bit_width = lex.read_usize()?;
            lex.expect_key("addr")?;
            let addr = read_var_id_list(lex)?;
            Ok(BIrStmt::StorageWrite { storage, src, bit_width, addr })
        }
        other => {
            let p = lex.pos();
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("unknown BIR stmt '{}'", other),
            })
        }
    }
}

// ============================================================================
// Parse BIrTerminator
// ============================================================================

fn parse_bir_terminator(kw: &str, lex: &mut Lexer) -> Result<BIrTerminator, ParseError> {
    match kw {
        "jmp" => {
            let block = read_block_target(lex)?;
            lex.expect_key("args")?;
            let args = read_var_id_list(lex)?;
            Ok(BIrTerminator::Jmp(BIrTarget { block, args }))
        }
        "cond_jmp" => {
            lex.expect_key("val")?;
            let val = mk_var(lex.read_var()?);
            lex.expect_key("then")?;
            let then_block = read_block_target(lex)?;
            lex.expect_key("then_args")?;
            let then_args = read_var_id_list(lex)?;
            lex.expect_key("else")?;
            let else_block = read_block_target(lex)?;
            lex.expect_key("else_args")?;
            let else_args = read_var_id_list(lex)?;
            Ok(BIrTerminator::CondJmp {
                val,
                then_target: BIrTarget { block: then_block, args: then_args },
                else_target: BIrTarget { block: else_block, args: else_args },
            })
        }
        other => {
            let p = lex.pos();
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("unknown BIR terminator '{}'", other),
            })
        }
    }
}

// ============================================================================
// Parse a single BIrBlock
// ============================================================================

fn parse_bir_block(lex: &mut Lexer) -> Result<BIrBlock<()>, ParseError> {
    let kw = lex.read_ident()?;
    if kw != "params" { return Err(ParseError::MissingField("params".into())); }
    let param_count = lex.read_u32()?;

    let mut stmts:      Vec<BIrStmt> = Vec::new();
    let mut stmt_provs: Vec<()>      = Vec::new();
    let mut terminator: Option<BIrTerminator> = None;

    loop {
        lex.skip();
        if lex.is_eof() { break; }

        let is_var = lex.peek_byte() == Some(b'v');
        if is_var {
            let _var_idx = lex.read_var()?;
            lex.expect_byte(b'=')?;
            let stmt_kw = lex.read_ident()?;
            let stmt = parse_bir_stmt(stmt_kw, lex)?;
            stmts.push(stmt);
            stmt_provs.push(());
        } else {
            let kw = lex.read_ident()?;
            if kw == "end_block" { break; }
            let term = parse_bir_terminator(kw, lex)?;
            terminator = Some(term);
            let end_kw = lex.read_ident()?;
            if end_kw != "end_block" {
                return Err(ParseError::UnexpectedToken {
                    line: lex.pos().line, col: lex.pos().col,
                    got: alloc::format!("expected end_block, got '{}'", end_kw),
                });
            }
            break;
        }
    }

    let terminator = terminator.ok_or(ParseError::MissingField("terminator".into()))?;

    Ok(BIrBlock { params: param_count, stmts, stmt_provs, terminator })
}

// ============================================================================
// ParseText for SavedBIrBlocks
// ============================================================================

pub(crate) fn parse_saved_bir_blocks(s: &str) -> Result<SavedBIrBlocks, ParseError> {
    let mut lex = Lexer::new(s);

    let header = lex.read_to_newline().trim().to_string();
    if header.is_empty() {
        return Err(ParseError::MissingVersionLine);
    }
    if header != BIR_HEADER {
        if header.starts_with("volar-bir v") {
            return Err(ParseError::UnsupportedVersion(header));
        }
        return Err(ParseError::MissingVersionLine);
    }

    let mut blocks: Vec<BIrBlock<()>> = Vec::new();

    loop {
        lex.skip();
        if lex.is_eof() { break; }

        let directive = lex.read_ident()?;
        match directive {
            "begin_block" => {
                let _block_id = lex.read_u32()?;
                let block = parse_bir_block(&mut lex)?;
                blocks.push(block);
            }
            other => return Err(ParseError::UnknownDirective(other.into())),
        }
    }

    Ok(SavedBIrBlocks { blocks: BIrBlocks { blocks, pre_init: vec![] } })
}
