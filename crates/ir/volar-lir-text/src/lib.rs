// @reliability: experimental
// @ai: assisted
//! Text-format serialisation and parsing for `volar-lir` types and
//! [`SavedLirModule`](volar_lir_saved::SavedLirModule).
//!
//! # Traits
//!
//! - [`WriteText`] — serialise a value to any [`core::fmt::Write`] sink.
//!   Implemented for `LirType`, `StructDef`, `IcmpPred`, `LirCall`,
//!   `SavedLirModule`.
//!
//! - [`ParseText`] — parse a value from a `&str` slice.
//!   Available with `feature = "parse"`.
//!   Implemented for the same types.
//!
//! # Format
//!
//! See `docs/text-format-spec.md` §2–3 for the full grammar. A quick summary:
//!
//! - `LirType` is written inline: `bool`, `u8`, `arr[u8, 4]`, `ptr[u8]`, …
//! - `SavedLirModule` is a line-oriented file starting with `volar-lir-saved v1`.
//! - Comments begin with `;` and extend to end of line.
//! - Unknown `key=value` fields on known directives are silently skipped on parse.

#![no_std]
extern crate alloc;

pub mod calls;
pub mod types;

#[cfg(feature = "parse")]
pub mod parse;

#[cfg(test)]
mod tests;

use alloc::string::String;
use core::fmt;

// ============================================================================
// WriteText trait
// ============================================================================

/// Serialise a value into any [`core::fmt::Write`] sink using the Volar text
/// format.
pub trait WriteText {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result;

    /// Convenience: serialise to a heap-allocated [`String`].
    fn to_text_string(&self) -> String {
        let mut s = String::new();
        self.write_text(&mut s).expect("String write is infallible");
        s
    }
}

// ============================================================================
// ParseText trait (feature = "parse")
// ============================================================================

#[cfg(feature = "parse")]
pub use parse::error::ParseError;

/// Parse a value from a text representation produced by [`WriteText`].
///
/// Only available with `feature = "parse"`.
#[cfg(feature = "parse")]
pub trait ParseText: Sized {
    fn parse_text(s: &str) -> Result<Self, ParseError>;
}
