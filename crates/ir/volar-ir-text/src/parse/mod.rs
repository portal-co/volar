// @reliability: experimental
// @ai: assisted
//! Parse module for `volar-ir-text`.

pub mod error;
pub mod lexer;
pub mod ir;

pub use error::ParseError;

use crate::{ParseText, SavedIrBlocks, SavedBIrBlocks};

impl ParseText for SavedIrBlocks {
    fn parse_text(s: &str) -> Result<Self, ParseError> {
        ir::parse_saved_ir_blocks(s)
    }
}

impl ParseText for SavedBIrBlocks {
    fn parse_text(s: &str) -> Result<Self, ParseError> {
        ir::parse_saved_bir_blocks(s)
    }
}
