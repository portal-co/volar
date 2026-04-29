// @reliability: experimental
// @ai: assisted
//! Parse error type.

use alloc::string::String;

/// Error produced by the `volar-lir-text` parser.
#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    /// The version line was present but names an unsupported version.
    UnsupportedVersion(String),
    /// A directive keyword was not recognised.
    UnknownDirective(String),
    /// The input ended earlier than expected.
    UnexpectedEof,
    /// A token was found that does not fit the grammar at this position.
    UnexpectedToken {
        /// 1-based line number.
        line: u32,
        /// 1-based column number.
        col: u32,
        /// Textual description of what was found.
        got: String,
    },
    /// A required `key=value` field was missing from a directive.
    MissingField(String),
    /// An integer literal could not be parsed.
    InvalidInt(String),
    /// An unknown / unparseable `native:X` suffix was encountered.
    UnknownNativeType(String),
    /// A `struct:N` id referred to a struct that has not been defined yet.
    UnknownStructId(u32),
    /// The version line was missing entirely.
    MissingVersionLine,
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ParseError::UnsupportedVersion(v) => write!(f, "unsupported version: {}", v),
            ParseError::UnknownDirective(d)   => write!(f, "unknown directive: {}", d),
            ParseError::UnexpectedEof         => write!(f, "unexpected end of input"),
            ParseError::UnexpectedToken { line, col, got } =>
                write!(f, "unexpected token at {}:{}: {}", line, col, got),
            ParseError::MissingField(k)       => write!(f, "missing required field: {}", k),
            ParseError::InvalidInt(s)         => write!(f, "invalid integer: {}", s),
            ParseError::UnknownNativeType(s)  => write!(f, "unknown native type: {}", s),
            ParseError::UnknownStructId(id)   => write!(f, "undefined struct id: {}", id),
            ParseError::MissingVersionLine    => write!(f, "missing version line"),
        }
    }
}
