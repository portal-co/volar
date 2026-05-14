// @reliability: experimental
// @ai: assisted
//! Parser for [`LirType`] and related sub-types.

use alloc::boxed::Box;
use volar_lir::LirType;
use volar_ir_common::Type as NativeType;
use super::{error::ParseError, lexer::Lexer};

// ============================================================================
// Public entry point: parse a full LirType from a bare string
// ============================================================================

pub fn parse_lir_type_full(s: &str) -> Result<LirType, ParseError> {
    let mut lex = Lexer::new(s);
    let ty = parse_lir_type(&mut lex)?;
    // Ensure no trailing garbage (ignoring whitespace/comments)
    if !lex.is_eof() {
        let p = lex.pos();
        return Err(ParseError::UnexpectedToken {
            line: p.line, col: p.col,
            got: "trailing input after type".into(),
        });
    }
    Ok(ty)
}

// ============================================================================
// Core recursive parser
// ============================================================================

/// Parse a `LirType` from the current position of `lex`.
pub(crate) fn parse_lir_type(lex: &mut Lexer<'_>) -> Result<LirType, ParseError> {
    let tok = lex.read_type_token()?;
    match tok {
        "bool" => Ok(LirType::Bool),
        "i8"   => Ok(LirType::I8),
        "u8"   => Ok(LirType::U8),
        "i16"  => Ok(LirType::I16),
        "u16"  => Ok(LirType::U16),
        "i32"  => Ok(LirType::I32),
        "u32"  => Ok(LirType::U32),
        "i64"  => Ok(LirType::I64),
        "u64"  => Ok(LirType::U64),
        "i128" => Ok(LirType::I128),
        "u128" => Ok(LirType::U128),
        "arr"  => {
            lex.expect_byte(b'[')?;
            let elem = parse_lir_type(lex)?;
            lex.expect_byte(b',')?;
            let len = lex.read_usize()?;
            lex.expect_byte(b']')?;
            Ok(LirType::Arr(Box::new(elem), len))
        }
        "ptr" => {
            lex.expect_byte(b'[')?;
            let inner = parse_lir_type(lex)?;
            lex.expect_byte(b']')?;
            Ok(LirType::Ptr(Box::new(inner)))
        }
        other if other.starts_with("struct:") => {
            let id_str = &other["struct:".len()..];
            let id: u32 = id_str.parse().map_err(|_| ParseError::InvalidInt(id_str.into()))?;
            Ok(LirType::Struct(id))
        }
        other if other.starts_with("native:") => {
            let suffix = &other["native:".len()..];
            let nt = parse_native_type(suffix)?;
            Ok(LirType::Native(nt))
        }
        unknown => Err(ParseError::UnexpectedToken {
            line: lex.pos().line, col: lex.pos().col,
            got: alloc::format!("unknown LirType token: {:?}", unknown),
        }),
    }
}

fn parse_native_type(s: &str) -> Result<NativeType, ParseError> {
    match s {
        "bit"      => Ok(NativeType::Bit),
        "u8"       => Ok(NativeType::_8),
        "u16"      => Ok(NativeType::_16),
        "u32"      => Ok(NativeType::_32),
        "u64"      => Ok(NativeType::_64),
        "u128"     => Ok(NativeType::_128),
        "u256"     => Ok(NativeType::_256),
        "aes8"     => Ok(NativeType::AES8),
        "galois64" => Ok(NativeType::Galois64),
        other      => Err(ParseError::UnknownNativeType(other.into())),
    }
}

// ============================================================================
// LirType list: `[ty, ty, ...]`
// ============================================================================

pub(crate) fn parse_lir_type_list(lex: &mut Lexer<'_>) -> Result<alloc::vec::Vec<LirType>, ParseError> {
    lex.expect_byte(b'[')?;
    let mut out = alloc::vec::Vec::new();
    loop {
        lex.skip();
        if lex.try_byte(b']') { break; }
        out.push(parse_lir_type(lex)?);
        lex.skip();
        if lex.try_byte(b',') { continue; }
        lex.expect_byte(b']')?;
        break;
    }
    Ok(out)
}

// ============================================================================
// Optional LirType: `none` | lir_type
// ============================================================================

pub(crate) fn parse_opt_lir_type(lex: &mut Lexer<'_>) -> Result<Option<LirType>, ParseError> {
    lex.skip();
    // Peek: is it 'n' for 'none'?
    if lex.remaining().starts_with("none") {
        // consume 'none'
        lex.expect_str("none")?;
        Ok(None)
    } else {
        Ok(Some(parse_lir_type(lex)?))
    }
}
