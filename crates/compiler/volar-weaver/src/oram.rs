// @reliability: experimental
//! @ai: assisted
//!
//! ORAM weaver: compiles `volar-oram-core` server-side ORAM operations
//! through the volar-compiler pipeline so they can be lowered to circuits.
//!
//! The ORAM server code (tree operations: `read_path`, `write_path`,
//! `server_step`) is written in "total Rust" — the subset that the
//! compiler can parse. This module provides:
//!
//! - [`oram_linked_spec`]: parses the `volar-oram-core` source into a
//!   [`LinkedSpec`] that can be merged into any target `IrModule`.
//!
//! The client-side logic (stash, position map, eviction, Protocol
//! impls) stays in normal Rust in `volar-oram`.

use volar_compiler::linkage::LinkedSpec;

/// Parse the `volar-oram-core` server-side source and return a [`LinkedSpec`].
///
/// This uses `include_str!` to embed the source at compile time, then
/// parses it with the volar-compiler parser. The resulting `LinkedSpec`
/// can be merged into any target `IrModule` via the linkage system.
///
/// Inner attributes (`#![...]`) are stripped before parsing since the
/// compiler parser handles outer attributes only.
///
/// Requires the `linking` feature.
#[cfg(feature = "linking")]
pub fn oram_linked_spec() -> LinkedSpec {
    use volar_compiler::parser::{SourceInput, parse_sources};
    let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
    let filtered = strip_inner_attributes(source);
    let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
        .expect("oram_linked_spec: failed to parse volar-oram-core/src/lib.rs");
    LinkedSpec {
        name: "oram_core".into(),
        module,
    }
}

/// Strip inner attributes (`#![...]`) and inner doc comments (`//!`)
/// from source code.
///
/// The volar-compiler parser handles outer attributes (`#[derive(...)]`)
/// but not inner attributes. This function removes them line-by-line
/// so the source can be parsed.
fn strip_inner_attributes(source: &str) -> alloc::string::String {
    use alloc::string::String;
    let mut result = String::new();
    for line in source.split('\n') {
        let trimmed = line.trim_start();
        if trimmed.starts_with("#![") || trimmed.starts_with("//!") {
            // Skip inner attribute and inner doc comment lines
            continue;
        }
        result.push_str(line);
        result.push('\n');
    }
    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(all(test, feature = "linking"))]
mod tests {
    extern crate std;

    use volar_compiler::parser::{SourceInput, parse_sources};

    #[test]
    fn oram_core_parses_from_include() {
        let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
        let filtered = super::strip_inner_attributes(source);
        let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
            .expect("failed to parse oram-core source");

        // Verify expected structure
        assert!(module.structs.len() >= 2, "expected at least OramEntry, Bucket structs");
        assert!(module.enums.len() >= 2, "expected at least ServerRequest, ServerResponse enums");
        assert!(module.functions.len() >= 4, "expected at least path_indices, read_path, write_path, server_step");
        assert!(module.impls.len() >= 2, "expected at least OramEntry, Bucket impls");
    }

    #[test]
    fn oram_core_round_trips_through_printer() {
        use volar_compiler::printer::{DisplayRust, ModuleWriter};

        let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
        let filtered = super::strip_inner_attributes(source);
        let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
            .expect("failed to parse oram-core source");

        let out = std::format!("{}", DisplayRust(ModuleWriter { module: &module }));

        // Key types present
        assert!(out.contains("struct OramEntry"), "missing OramEntry in output");
        assert!(out.contains("struct Bucket"), "missing Bucket in output");
        assert!(out.contains("enum ServerRequest"), "missing ServerRequest in output");
        assert!(out.contains("enum ServerResponse"), "missing ServerResponse in output");

        // Key functions present
        assert!(out.contains("fn path_indices"), "missing path_indices in output");
        assert!(out.contains("fn read_path"), "missing read_path in output");
        assert!(out.contains("fn write_path"), "missing write_path in output");
        assert!(out.contains("fn server_step"), "missing server_step in output");

        // Derives preserved
        assert!(out.contains("Clone"), "missing Clone derive in output");
        assert!(out.contains("Copy"), "missing Copy derive in output");
    }

    #[test]
    fn oram_core_compile_check() {
        use crate::tests_common::run_compile_check;
        use volar_compiler::printer::{DisplayRust, ModuleWriter};

        let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
        let filtered = super::strip_inner_attributes(source);
        let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
            .expect("failed to parse oram-core source");

        let code = std::format!("{}", DisplayRust(ModuleWriter { module: &module }));
        run_compile_check(&code, "oram_core_roundtrip");
    }
}
