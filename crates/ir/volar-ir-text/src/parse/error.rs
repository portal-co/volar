// @reliability: experimental
// @ai: assisted
//! Parse errors for `volar-ir-text`.

use alloc::string::String;

#[derive(Debug)]
pub enum ParseError {
    /// No `volar-ir v1` / `volar-bir v1` header found.
    MissingVersionLine,
    /// Header found but the version number is not supported.
    UnsupportedVersion(String),
    /// A directive keyword was not recognised.
    UnknownDirective(String),
    /// Unexpected end of input.
    UnexpectedEof,
    /// A token was present but had an unexpected value or form.
    UnexpectedToken { line: u32, col: u32, got: String },
    /// A required `key=value` field was absent.
    MissingField(String),
    /// An integer literal could not be parsed.
    InvalidInt(String),
    /// A `prim` keyword was not recognised.
    UnknownPrimType(String),
    /// A type index was out of range for the current type table.
    TypeIdOutOfRange(u32),
    /// A variable reference `vN` was out of range for the current block.
    VarOutOfRange(u32),
}
