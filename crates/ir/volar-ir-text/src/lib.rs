// @reliability: experimental
// @ai: assisted
//! Text-format serialisation and parsing for `volar-ir` types.
//!
//! # Types serialised
//!
//! - [`SavedIrBlocks`] — a `(TypeTable, IRBlocks<()>)` pair in `.vir` format.
//! - [`SavedBIrBlocks`] — a `BIrBlocks<()>` in `.vbir` format.
//!
//! # Traits
//!
//! - [`WriteText`] — serialise into any [`core::fmt::Write`] sink.
//! - [`ParseText`] — parse from a `&str` (feature `"parse"`).

#![no_std]
extern crate alloc;

pub mod ir;
pub mod boolar;

#[cfg(feature = "parse")]
pub mod parse;

#[cfg(test)]
mod tests;

use alloc::string::String;
use core::fmt;

pub use ir::SavedIrBlocks;
pub use boolar::SavedBIrBlocks;

// ============================================================================
// WriteText
// ============================================================================

/// Serialise a value into any [`core::fmt::Write`] sink.
pub trait WriteText {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result;

    fn to_text_string(&self) -> String {
        let mut s = String::new();
        self.write_text(&mut s).expect("String write is infallible");
        s
    }
}

// ============================================================================
// ParseText (feature = "parse")
// ============================================================================

#[cfg(feature = "parse")]
pub use parse::error::ParseError;

#[cfg(feature = "parse")]
pub trait ParseText: Sized {
    fn parse_text(s: &str) -> Result<Self, ParseError>;
}
