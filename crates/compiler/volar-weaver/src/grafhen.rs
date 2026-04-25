// @reliability: experimental
// @experimental-status: design
// @experimental-since: (not yet reviewed)
// @ai: unreviewed
//! GRAFHEN weaving pass.
//!
//! Lowers a boolean circuit (`BIrBlocks`) into an `IrModule` that evaluates
//! the circuit **homomorphically** over GRAFHEN ciphertexts.
//!
//! **WARNING: IND-CPA BROKEN.** The generated code relies on GRAFHEN, which
//! is broken by ePrint 2026/700. Use only inside a ZK proof for correctness,
//! never as a confidentiality primitive. See `docs/grafhen.md`.
//!
//! ## Generated function signature
//!
//! ```text
//! fn <name>_grafhen<R: WordReducer<WBOUND>>(
//!     pk: &GrafhenPublic<R, WBOUND>,
//!     input_0: &GrafhenWord<WBOUND>,
//!     input_1: &GrafhenWord<WBOUND>,
//!     ...
//! ) -> GrafhenWord<WBOUND>
//! ```
//!
//! `WBOUND` is a concrete literal baked in at weave time via the `word_bound`
//! parameter (e.g. `42`). The same literal appears in all type positions, so
//! the caller instantiates by choosing a concrete `R`.
//!
//! ## Gate lowering
//!
//! | Gate | Generated call |
//! |------|---------------|
//! | Zero | `GrafhenWord::identity()` |
//! | One  | `pk.enc_one.clone()` |
//! | XOR  | `grafhen_xor(&a, &b)` |
//! | NOT  | `grafhen_not(&a, pk)` |
//! | AND  | `grafhen_and(&a, &b, pk)` |
//! | OR   | expanded via De Morgan: `NOT(AND(NOT(a), NOT(b)))` |

use alloc::string::String;

use volar_compiler::{
    ir::{IrFunction, IrModule},
    linkage::LinkageSystem,
};
use volar_ir::boolar::BIrBlocks;

use crate::{
    fhe::{weave_fhe_flat_bir, GrafhenScheme},
    NoProvenance, ProvenanceHandler,
};

// ============================================================================
// Weaver
// ============================================================================

/// Weave a boolean circuit into a GRAFHEN homomorphic evaluator.
///
/// `word_bound` is baked into all `GrafhenWord<WBOUND>` and `GrafhenPublic<R,
/// WBOUND>` type positions as a concrete literal.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_grafhen<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    word_bound: usize,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    weave_grafhen_with_handler(circuit, name, word_bound, linkage, &NoProvenance)
}

/// Weave a boolean circuit into a GRAFHEN homomorphic evaluator,
/// using `handler` to map input provenance into the output IR.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_grafhen_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    word_bound: usize,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_grafhen: circuit must satisfy is_circuit() (single block with Return terminator)"
    );
    let scheme = GrafhenScheme::new(word_bound);
    weave_fhe_flat_bir(circuit, &scheme, name, linkage, handler, None)
}

// ============================================================================
// Printer
// ============================================================================

/// Render a weaved GRAFHEN `IrModule` to Rust source.
pub fn print_grafhen_module(module: &IrModule<IrFunction>, self_contained: bool) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));

    let preamble: &str = if self_contained {
        concat!(
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
            "use volar_spec::grafhen::{GrafhenWord, GrafhenPublic, WordReducer, NoReduction,\n",
            "    grafhen_zero, grafhen_xor, grafhen_not, grafhen_and};\n",
            "\n",
        )
    } else {
        concat!(
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
            "use volar_spec::grafhen::{GrafhenWord, GrafhenPublic, WordReducer,\n",
            "    grafhen_zero, grafhen_xor, grafhen_not, grafhen_and};\n",
            "\n",
        )
    };

    let mut full = String::with_capacity(preamble.len() + out.len());
    full.push_str(preamble);
    full.push_str(&out);
    full
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::string::String;

    use super::*;
    use crate::tests_common::{build_xor_and_circuit, run_compile_check, workspace_root};

    #[test]
    fn test_weave_grafhen_xor_and_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_grafhen(&circuit, "test_circuit", 64, None);
        let code = print_grafhen_module(&module, false);
        run_compile_check_grafhen(&code, "grafhen_xor_and");
    }

    /// Like `run_compile_check` but with the GRAFHEN spec dependency.
    fn run_compile_check_grafhen(code: &str, test_name: &str) {
        use std::{fs, process::Command};
        let root = workspace_root();
        let tmpdir = std::env::temp_dir().join(std::format!("volar_weaver_{}", test_name));
        let srcdir = tmpdir.join("src");
        fs::create_dir_all(&srcdir).unwrap();

        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-check-{name}\"\n\
             version = \"0.1.0\"\n\
             edition = \"2024\"\n\
             \n\
             [lib]\n\
             path = \"src/lib.rs\"\n\
             \n\
             [dependencies]\n\
             volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
             volar-primitives = {{ path = \"{root}/crates/spec/volar-primitives\" }}\n\
             volar-common = {{ path = \"{root}/crates/spec/volar-common\" }}\n\
             hybrid-array = \"0.4.8\"\n\
             digest = {{ version = \"0.11.2\", default-features = false }}\n\
             cipher = {{ version = \"0.5.1\", default-features = false }}\n\
             rand = {{ version = \"0.9.2\", default-features = false }}\n\
             typenum = {{ version = \"1.17\", default-features = false }}\n\
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n",
            name = test_name,
            root = root,
        );

        fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();
        fs::write(srcdir.join("lib.rs"), code).unwrap();

        let output = Command::new("cargo")
            .args(["check", "--quiet"])
            .current_dir(&tmpdir)
            .env(
                "CARGO_TARGET_DIR",
                String::from(tmpdir.join("target").to_str().unwrap()),
            )
            .output()
            .expect("failed to run cargo check");

        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let _ = fs::remove_dir_all(&tmpdir);

        if !output.status.success() {
            panic!(
                "Generated GRAFHEN code failed to compile (test: {})\n--- code ---\n{}\n--- stderr ---\n{}",
                test_name, code, stderr
            );
        }
    }
}
