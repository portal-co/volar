// @reliability: experimental
// @ai: assisted
//! Lexer for the volar-ir / volar-bir text formats.
//!
//! Reuses the same design as `volar-lir-text`: line-oriented, comment-aware,
//! token-by-token without full pre-scan.

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use super::error::ParseError;

#[derive(Clone, Copy, Debug)]
pub struct Pos { pub line: u32, pub col: u32 }

pub struct Lexer<'a> {
    src:  &'a str,
    pos:  usize,
    line: u32,
    col:  u32,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self { Lexer { src, pos: 0, line: 1, col: 1 } }
    pub fn pos(&self)       -> Pos   { Pos { line: self.line, col: self.col } }
    pub fn is_eof(&mut self) -> bool { self.skip(); self.pos >= self.src.len() }

    // ------------------------------------------------------------------
    // Internal advance helpers
    // ------------------------------------------------------------------

    fn advance_byte(&mut self) {
        if self.pos < self.src.len() {
            let b = self.src.as_bytes()[self.pos];
            self.pos += 1;
            if b == b'\n' { self.line += 1; self.col = 1; } else { self.col += 1; }
        }
    }

    pub fn skip(&mut self) {
        loop {
            let start = self.pos;
            while self.pos < self.src.len() {
                let b = self.src.as_bytes()[self.pos];
                if b == b' ' || b == b'\t' || b == b'\r' || b == b'\n' {
                    self.advance_byte();
                } else {
                    break;
                }
            }
            if self.pos < self.src.len() && self.src.as_bytes()[self.pos] == b';' {
                while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != b'\n' {
                    self.pos += 1; self.col += 1;
                }
                continue;
            }
            if self.pos == start { break; }
        }
    }

    // ------------------------------------------------------------------
    // Peeking
    // ------------------------------------------------------------------

    pub fn peek_byte(&mut self) -> Option<u8> {
        self.skip();
        self.src.as_bytes().get(self.pos).copied()
    }

    pub fn try_byte(&mut self, b: u8) -> bool {
        self.skip();
        if self.pos < self.src.len() && self.src.as_bytes()[self.pos] == b {
            self.advance_byte(); true
        } else { false }
    }

    // ------------------------------------------------------------------
    // Identifiers / keywords
    // ------------------------------------------------------------------

    /// Read `[a-zA-Z_][a-zA-Z0-9_-]*`  (used for directives and keys).
    pub fn read_ident(&mut self) -> Result<&'a str, ParseError> {
        self.skip();
        let start = self.pos;
        if self.pos >= self.src.len() { return Err(ParseError::UnexpectedEof); }
        let b = self.src.as_bytes()[self.pos];
        if !b.is_ascii_alphabetic() && b != b'_' {
            let p = self.pos();
            return Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("'{}'", b as char),
            });
        }
        while self.pos < self.src.len() {
            let b = self.src.as_bytes()[self.pos];
            if b.is_ascii_alphanumeric() || b == b'_' || b == b'-' {
                self.pos += 1; self.col += 1;
            } else { break; }
        }
        Ok(&self.src[start..self.pos])
    }

    // ------------------------------------------------------------------
    // Numbers
    // ------------------------------------------------------------------

    pub fn read_u32(&mut self) -> Result<u32, ParseError> {
        self.skip();
        let raw = self.read_unsigned_int_str()?;
        if raw.starts_with("0x") || raw.starts_with("0X") {
            u32::from_str_radix(&raw[2..], 16).map_err(|_| ParseError::InvalidInt(raw.to_string()))
        } else {
            raw.parse::<u32>().map_err(|_| ParseError::InvalidInt(raw.to_string()))
        }
    }

    pub fn read_usize(&mut self) -> Result<usize, ParseError> { self.read_u32().map(|v| v as usize) }

    /// Read a `u128` (decimal or `0x`-prefixed hex).
    pub fn read_u128(&mut self) -> Result<u128, ParseError> {
        self.skip();
        let raw = self.read_unsigned_int_str()?;
        if raw.starts_with("0x") || raw.starts_with("0X") {
            u128::from_str_radix(&raw[2..], 16).map_err(|_| ParseError::InvalidInt(raw.to_string()))
        } else {
            raw.parse::<u128>().map_err(|_| ParseError::InvalidInt(raw.to_string()))
        }
    }

    fn read_unsigned_int_str(&mut self) -> Result<&'a str, ParseError> {
        let start = self.pos;
        if self.pos + 1 < self.src.len()
            && self.src.as_bytes()[self.pos] == b'0'
            && (self.src.as_bytes()[self.pos + 1] == b'x' || self.src.as_bytes()[self.pos + 1] == b'X')
        {
            self.pos += 2; self.col += 2;
        }
        let digit_start = self.pos;
        while self.pos < self.src.len() && self.src.as_bytes()[self.pos].is_ascii_hexdigit() {
            self.pos += 1; self.col += 1;
        }
        if self.pos == digit_start { return Err(ParseError::UnexpectedEof); }
        Ok(&self.src[start..self.pos])
    }

    // ------------------------------------------------------------------
    // Specific tokens
    // ------------------------------------------------------------------

    pub fn expect_byte(&mut self, expected: u8) -> Result<(), ParseError> {
        self.skip();
        if self.pos < self.src.len() && self.src.as_bytes()[self.pos] == expected {
            self.advance_byte(); Ok(())
        } else {
            let p = self.pos();
            let got = self.src.as_bytes().get(self.pos).copied().unwrap_or(0);
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("expected '{}', got '{}'", expected as char, got as char),
            })
        }
    }

    pub fn expect_str(&mut self, expected: &str) -> Result<(), ParseError> {
        self.skip();
        if self.src[self.pos..].starts_with(expected) {
            for _ in expected.bytes() { self.advance_byte(); }
            Ok(())
        } else {
            let p = self.pos();
            let got: String = self.src[self.pos..].chars().take(16).collect();
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("expected {:?}, got {:?}", expected, got),
            })
        }
    }

    pub fn expect_key(&mut self, expected: &str) -> Result<(), ParseError> {
        let k = self.read_ident()?;
        if k != expected { return Err(ParseError::MissingField(expected.into())); }
        self.expect_byte(b'=')?;
        Ok(())
    }

    // ------------------------------------------------------------------
    // Quoted strings
    // ------------------------------------------------------------------

    pub fn read_string(&mut self) -> Result<String, ParseError> {
        self.skip();
        self.expect_byte(b'"')?;
        let mut s = String::new();
        loop {
            if self.pos >= self.src.len() { return Err(ParseError::UnexpectedEof); }
            let b = self.src.as_bytes()[self.pos];
            if b == b'"' { self.advance_byte(); return Ok(s); }
            if b == b'\\' {
                self.advance_byte();
                if self.pos >= self.src.len() { return Err(ParseError::UnexpectedEof); }
                let esc = self.src.as_bytes()[self.pos];
                self.advance_byte();
                match esc {
                    b'\\' => s.push('\\'),
                    b'"'  => s.push('"'),
                    b'n'  => s.push('\n'),
                    b't'  => s.push('\t'),
                    b'r'  => s.push('\r'),
                    other => s.push(other as char),
                }
            } else {
                let ch = self.src[self.pos..].chars().next().unwrap();
                self.pos += ch.len_utf8(); self.col += 1;
                s.push(ch);
            }
        }
    }

    // ------------------------------------------------------------------
    // Lists
    // ------------------------------------------------------------------

    /// `[u32, ...]`
    pub fn read_u32_list(&mut self) -> Result<Vec<u32>, ParseError> {
        self.expect_byte(b'[')?;
        let mut out = Vec::new();
        loop {
            self.skip();
            if self.try_byte(b']') { break; }
            out.push(self.read_u32()?);
            self.skip();
            if self.try_byte(b',') { continue; }
            self.expect_byte(b']')?;
            break;
        }
        Ok(out)
    }

    // ------------------------------------------------------------------
    // Variable references: `vN`
    // ------------------------------------------------------------------

    /// Read `vN` and return the raw u32 index.
    pub fn read_var(&mut self) -> Result<u32, ParseError> {
        self.skip();
        if self.pos >= self.src.len() { return Err(ParseError::UnexpectedEof); }
        if self.src.as_bytes()[self.pos] != b'v' {
            let p = self.pos();
            return Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("expected 'v<N>', got '{}'", self.src.as_bytes()[self.pos] as char),
            });
        }
        self.advance_byte();
        self.read_u32()
    }

    /// Read `[vN, ...]`
    pub fn read_var_list(&mut self) -> Result<Vec<u32>, ParseError> {
        self.expect_byte(b'[')?;
        let mut out = Vec::new();
        loop {
            self.skip();
            if self.try_byte(b']') { break; }
            out.push(self.read_var()?);
            self.skip();
            if self.try_byte(b',') { continue; }
            self.expect_byte(b']')?;
            break;
        }
        Ok(out)
    }

    // ------------------------------------------------------------------
    // Rest-of-line (for version header)
    // ------------------------------------------------------------------

    pub fn read_to_newline(&mut self) -> &'a str {
        let start = self.pos;
        while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != b'\n' {
            self.pos += 1; self.col += 1;
        }
        &self.src[start..self.pos]
    }
}
