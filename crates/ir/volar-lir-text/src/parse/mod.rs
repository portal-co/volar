// @reliability: experimental
// @ai: assisted
//! Parser for the `volar-lir-saved` text format.
//!
//! Entry points:
//! - [`parse_lir_type`] — parse a single [`LirType`] from a `&str`.
//! - [`parse_saved_lir_module`] — parse a complete [`SavedLirModule`] from a `&str`.
//!
//! [`ParseText`](crate::ParseText) is also implemented for those types when
//! `feature = "parse"` is active.

pub mod error;
pub mod lexer;
pub mod types;
pub mod calls;

pub use error::ParseError;

use volar_lir::LirType;
use volar_lir_saved::SavedLirModule;

use crate::ParseText;

impl ParseText for LirType {
    fn parse_text(s: &str) -> Result<Self, ParseError> {
        types::parse_lir_type_full(s)
    }
}

impl ParseText for SavedLirModule {
    fn parse_text(s: &str) -> Result<Self, ParseError> {
        calls::parse_saved_lir_module(s)
    }
}
