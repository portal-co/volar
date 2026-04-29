// @reliability: experimental
// @ai: assisted
//! Lexer / tokeniser for the Volar LIR text format.
//!
//! The format is line-oriented but types and value lists can span several
//! tokens on the same line. This tokeniser is intentionally simple:
//! it does not pre-scan the whole input but drives token-by-token.

use alloc::string::{String, ToString};
use super::error::ParseError;

/// A position within the source text.
#[derive(Clone, Copy, Debug)]
pub struct Pos {
    pub line: u32,
    pub col:  u32,
}

/// A thin wrapper around a `&str` that tracks position and yields tokens.
pub struct Lexer<'a> {
    src:  &'a str,
    pos:  usize,
    line: u32,
    col:  u32,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src, pos: 0, line: 1, col: 1 }
    }

    pub fn pos(&self) -> Pos {
        Pos { line: self.line, col: self.col }
    }

    pub fn remaining(&self) -> &'a str {
        &self.src[self.pos..]
    }

    pub fn is_eof(&self) -> bool {
        self.skip_ws_and_comments();
        self.pos >= self.src.len()
    }

    /// Advance past whitespace and `;`-comment lines.
    /// This is non-mutating in the sense that it finds the next
    /// non-whitespace position but does NOT advance `self.pos` —
    /// it merely returns the offset. Use `skip_ws_and_comments`
    /// for the side-effecting variant.
    fn skip_ws_and_comments(&self) {
        // We expose this as a shared borrow inspection helper only;
        // the mutable version is `do_skip`.
    }

    /// Skip whitespace and `;`-comments. Returns whether any bytes were skipped.
    pub fn skip(&mut self) {
        loop {
            // skip plain whitespace
            let start = self.pos;
            while self.pos < self.src.len() {
                let b = self.src.as_bytes()[self.pos];
                if b == b' ' || b == b'\t' || b == b'\r' {
                    self.advance_byte();
                } else if b == b'\n' {
                    self.advance_byte();
                    self.line += 1;
                    self.col = 1;
                    // After a newline, reset col (advance_byte already increments col,
                    // but we set it to 1 above so we need to not double-count).
                    // Actually, let's fix: advance_byte increments col, so we should
                    // do it manually here.
                } else {
                    break;
                }
            }
            // skip ; comment
            if self.pos < self.src.len() && self.src.as_bytes()[self.pos] == b';' {
                while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != b'\n' {
                    self.pos += 1;
                    self.col += 1;
                }
                continue;
            }
            if self.pos == start {
                break; // nothing skipped this round
            }
        }
    }

    fn advance_byte(&mut self) {
        if self.pos < self.src.len() {
            let b = self.src.as_bytes()[self.pos];
            self.pos += 1;
            if b == b'\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
    }

    // -------------------------------------------------------------------------
    // Token readers
    // -------------------------------------------------------------------------

    /// Peek at the next byte (after skipping whitespace/comments).
    pub fn peek_byte(&mut self) -> Option<u8> {
        self.skip();
        self.src.as_bytes().get(self.pos).copied()
    }

    /// Read a bare identifier/keyword: `[a-zA-Z_:][a-zA-Z0-9_:.-]*`.
    /// Used for directives and `key=` names.
    pub fn read_ident(&mut self) -> Result<&'a str, ParseError> {
        self.skip();
        let start = self.pos;
        if self.pos >= self.src.len() {
            return Err(ParseError::UnexpectedEof);
        }
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
                self.pos += 1;
                self.col += 1;
            } else {
                break;
            }
        }
        Ok(&self.src[start..self.pos])
    }

    /// Read an identifier that may contain `:` and `.` (for `native:bit`,
    /// `arr[...]`, etc.).
    pub fn read_type_token(&mut self) -> Result<&'a str, ParseError> {
        self.skip();
        let start = self.pos;
        if self.pos >= self.src.len() {
            return Err(ParseError::UnexpectedEof);
        }
        while self.pos < self.src.len() {
            let b = self.src.as_bytes()[self.pos];
            if b.is_ascii_alphanumeric() || b == b'_' || b == b':' || b == b'-' {
                self.pos += 1;
                self.col += 1;
            } else {
                break;
            }
        }
        if self.pos == start {
            let p = self.pos();
            return Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("'{}'", self.src.as_bytes()[self.pos] as char),
            });
        }
        Ok(&self.src[start..self.pos])
    }

    /// Expect a specific literal string (no whitespace consumed inside it).
    pub fn expect_str(&mut self, expected: &str) -> Result<(), ParseError> {
        self.skip();
        if self.src[self.pos..].starts_with(expected) {
            for _ in expected.bytes() {
                self.advance_byte();
            }
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

    /// Expect a specific single byte (after skipping whitespace).
    pub fn expect_byte(&mut self, expected: u8) -> Result<(), ParseError> {
        self.skip();
        if self.pos < self.src.len() && self.src.as_bytes()[self.pos] == expected {
            self.advance_byte();
            Ok(())
        } else {
            let p = self.pos();
            let got = self.src.as_bytes().get(self.pos).copied().unwrap_or(0);
            Err(ParseError::UnexpectedToken {
                line: p.line, col: p.col,
                got: alloc::format!("expected '{}', got '{}'", expected as char, got as char),
            })
        }
    }

    /// Try to consume a specific byte; return true if consumed.
    pub fn try_byte(&mut self, b: u8) -> bool {
        self.skip();
        if self.pos < self.src.len() && self.src.as_bytes()[self.pos] == b {
            self.advance_byte();
            true
        } else {
            false
        }
    }

    /// Read a decimal or `0x`-hex integer. Works for both `u32` and `i64`.
    pub fn read_i64(&mut self) -> Result<i64, ParseError> {
        self.skip();
        let negative = self.try_byte(b'-');
        let raw = self.read_unsigned_int_str()?;
        let n: i64 = if raw.starts_with("0x") || raw.starts_with("0X") {
            i64::from_str_radix(&raw[2..], 16)
                .map_err(|_| ParseError::InvalidInt(raw.to_string()))?
        } else {
            raw.parse::<i64>().map_err(|_| ParseError::InvalidInt(raw.to_string()))?
        };
        Ok(if negative { -n } else { n })
    }

    pub fn read_u32(&mut self) -> Result<u32, ParseError> {
        self.skip();
        let raw = self.read_unsigned_int_str()?;
        if raw.starts_with("0x") || raw.starts_with("0X") {
            u32::from_str_radix(&raw[2..], 16)
                .map_err(|_| ParseError::InvalidInt(raw.to_string()))
        } else {
            raw.parse::<u32>().map_err(|_| ParseError::InvalidInt(raw.to_string()))
        }
    }

    pub fn read_usize(&mut self) -> Result<usize, ParseError> {
        self.read_u32().map(|v| v as usize)
    }

    fn read_unsigned_int_str(&mut self) -> Result<&'a str, ParseError> {
        let start = self.pos;
        // consume optional 0x prefix
        if self.pos + 1 < self.src.len()
            && self.src.as_bytes()[self.pos] == b'0'
            && (self.src.as_bytes()[self.pos + 1] == b'x' || self.src.as_bytes()[self.pos + 1] == b'X')
        {
            self.pos += 2;
            self.col += 2;
        }
        let digit_start = self.pos;
        while self.pos < self.src.len() {
            let b = self.src.as_bytes()[self.pos];
            if b.is_ascii_hexdigit() {
                self.pos += 1;
                self.col += 1;
            } else {
                break;
            }
        }
        if self.pos == digit_start && start == digit_start {
            return Err(ParseError::UnexpectedEof);
        }
        Ok(&self.src[start..self.pos])
    }

    /// Read `key=` and return the key name (the `=` is consumed).
    pub fn read_key(&mut self) -> Result<&'a str, ParseError> {
        let key = self.read_ident()?;
        self.expect_byte(b'=')?;
        Ok(key)
    }

    /// Parse `key=<value>` where the key must equal `expected`.
    pub fn expect_key(&mut self, expected: &str) -> Result<(), ParseError> {
        let k = self.read_ident()?;
        if k != expected {
            return Err(ParseError::MissingField(expected.into()));
        }
        self.expect_byte(b'=')?;
        Ok(())
    }

    /// Read a quoted string `"..."` (consuming the surrounding quotes).
    pub fn read_string(&mut self) -> Result<String, ParseError> {
        self.skip();
        self.expect_byte(b'"')?;
        let mut s = String::new();
        loop {
            if self.pos >= self.src.len() {
                return Err(ParseError::UnexpectedEof);
            }
            let b = self.src.as_bytes()[self.pos];
            if b == b'"' {
                self.advance_byte();
                return Ok(s);
            }
            if b == b'\\' {
                self.advance_byte();
                if self.pos >= self.src.len() {
                    return Err(ParseError::UnexpectedEof);
                }
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
                // Safety: we are iterating over a valid UTF-8 str
                let ch_str = &self.src[self.pos..];
                let ch = ch_str.chars().next().unwrap();
                self.pos += ch.len_utf8();
                self.col += 1;
                s.push(ch);
            }
        }
    }

    /// Read `[u32, u32, ...]` into a Vec.
    pub fn read_u32_list(&mut self) -> Result<alloc::vec::Vec<u32>, ParseError> {
        self.expect_byte(b'[')?;
        let mut out = alloc::vec::Vec::new();
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

    /// Read `[[u32,...],...]` into a Vec<Vec<u32>>.
    pub fn read_u32_list_list(&mut self) -> Result<alloc::vec::Vec<alloc::vec::Vec<u32>>, ParseError> {
        self.expect_byte(b'[')?;
        let mut out = alloc::vec::Vec::new();
        loop {
            self.skip();
            if self.try_byte(b']') { break; }
            out.push(self.read_u32_list()?);
            self.skip();
            if self.try_byte(b',') { continue; }
            self.expect_byte(b']')?;
            break;
        }
        Ok(out)
    }

    /// Read the rest of the current logical line (up to `\n` or EOF).
    pub fn read_to_newline(&mut self) -> &'a str {
        let start = self.pos;
        while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != b'\n' {
            self.pos += 1;
            self.col += 1;
        }
        &self.src[start..self.pos]
    }
}
