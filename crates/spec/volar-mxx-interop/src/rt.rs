// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! Access to the bundled `mxx_rt.rs` runtime helpers, as IR.
//!
//! The helpers are plain spec-subset Rust (see `rust_rt/mxx_rt.rs`). They
//! are parsed into spec IR through the same `volar_compiler::parse_sources`
//! pipeline as hand-written spec code, so the IR a lowered program calls is
//! exactly the IR the normal spec backends see — no parallel construction.

use core::fmt::Write as _;
use volar_compiler::{IrFunction, IrModule, SourceInput, parse_sources};

use crate::InteropError;

/// Render a lowered module as standalone Rust, without the dyn preamble's
/// spec-crate imports (`use typenum::...`, `use volar_common::...`).
///
/// The interop lowerer's output is self-contained (fixed-size arrays and
/// `u64`/`u128` arithmetic only), so the spec-crate preamble does not
/// apply. This uses the printer's structured `ModuleWriter` API — no text
/// manipulation of printer output.
///
/// /// @volar-allow-rust-text: test-fixture: standalone rendering exists so
/// interop tests can compile lowered modules with `rustc` directly (core
/// design rule 2); the Phase-1 spec integration consumes the IR through
/// the normal gen-specs pipeline, not this text.
pub fn print_standalone(module: &IrModule<IrFunction>) -> String {
    let mut out = String::new();
    let _ = write!(
        out,
        "{}",
        volar_compiler::printer::DisplayRust(volar_compiler::printer::ModuleWriter {
            module,
            emit_async: false,
        })
    );
    out
}

/// The bundled helper source, verbatim.
pub const MXX_RT_SOURCE: &str = include_str!("../rust_rt/mxx_rt.rs");

/// Parse the bundled helpers into spec IR functions.
///
/// The returned functions are in source order; callers typically keep only
/// the ones a lowered program actually references.
pub fn helper_functions() -> Result<Vec<IrFunction>, InteropError> {
    let sources = [SourceInput {
        source: MXX_RT_SOURCE,
        name: "mxx_rt",
    }];
    let module = parse_sources(&sources, "mxx_rt", &["mxx_rt".to_string()])
        .map_err(|e| InteropError::HelperParse(format!("{e:?}")))?;
    if !module.structs.is_empty()
        || !module.enums.is_empty()
        || !module.traits.is_empty()
        || !module.impls.is_empty()
        || !module.consts.is_empty()
    {
        return Err(InteropError::HelperParse(
            "mxx_rt.rs must contain only free functions".into(),
        ));
    }
    Ok(module.functions)
}
