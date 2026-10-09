// @reliability: normal
//! @ai: assisted
//! Shared VOLE spec-source plumbing: parse the VOLE spec module, build its
//! [`LinkageSystem`], produce the standard [`MonoEnv`] bindings, and lower a
//! woven VOLE module into an arbitrary [`volar_lir::LirTarget`].
//!
//! Extracted from `volar-c-backend-spec-tests/tests/vole_e2e.rs` so the same
//! linkage/env definitions are single-sourced for in-repo tests and external
//! consumers (e.g. the volar-cloud weaver binding) rather than duplicated.
//!
//! # Pipeline
//!
//! ```text
//! BIrBlocks (circuit)
//!   └─ weave_vole_prover/verifier  (volar-weaver)
//!        └─ IrModule<IrFunction>   (woven + spec-linked)
//!             └─ lower_vole_to_target / lower_vole_module  (this crate)
//!                  └─ CBackend / WasmBackend / RecordingTarget → SavedLirModule
//! ```

use std::path::Path;

use volar_compiler::{
    SourceInput,
    ir::IrFunction,
    ir::IrModule,
    ir::IrType,
    ir::PrimitiveType,
    linkage::{LinkageKind, LinkageSystem, LinkedSpec},
    parse_sources,
};
use volar_lir_codegen::{MonoPlanOptions, lower_module_monomorphized, mono::MonoEnv};

/// Directory containing the VOLE spec sources, derived from the workspace
/// layout (`crates/compiler/volar-vole-pipeline` → `crates/spec/volar-spec`).
fn spec_src_dir() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("spec")
        .join("volar-spec")
        .join("src")
}

/// Read one spec file, returning `(source, file-stem)`.
fn read_spec(name: &str) -> (String, String) {
    let path = spec_src_dir().join(name);
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("cannot read spec file {}: {e}", path.display()));
    let stem = Path::new(name)
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    (src, stem)
}

/// Parse the VOLE-relevant spec sources into a single `IrModule`.
///
/// We include:
/// - `lib.rs`         — top-level re-exports, SpecRng trait
/// - `vole.rs`        — Delta, Q, VoleArray trait, rotation/remap operations
/// - `vole/prove.rs`  — vole_and_prover_step, vole_and_verifier_check
/// - `vole/vope.rs`   — Vope struct definition
/// - `vole/impls.rs`  — Add/Mul/BitXor impls for Vope
pub fn parse_vole_spec() -> volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction> {
    let files = [
        "lib.rs",
        "vole.rs",
        "vole/prove.rs",
        "vole/vope.rs",
        "vole/impls.rs",
    ];
    let loaded: Vec<(String, String)> = files.iter().map(|&f| read_spec(f)).collect();
    let inputs: Vec<SourceInput> = loaded
        .iter()
        .map(|(src, name)| SourceInput {
            source: src.as_str(),
            name: name.as_str(),
        })
        .collect();
    parse_sources(&inputs, "volar_spec", &[])
        .unwrap_or_else(|e| panic!("parse_vole_spec failed: {e}"))
}

/// Build a `LinkageSystem` from the VOLE spec sources.
pub fn make_vole_linkage() -> LinkageSystem {
    let spec_module = parse_vole_spec();
    let mut ls = LinkageSystem::new();
    ls.add(LinkedSpec {
        name: "volar_spec".into(),
        module: spec_module,
        kind: LinkageKind::Inline,
    });
    ls
}

/// `MonoEnv` for VOLE with 16-lane GF(2^8) field elements.
///
/// - `N = 16`  : VOLE vector length (number of field elements per wire)
/// - `T = u8`  : field element type (GF(2^8))
/// - `U1 = 1`  : typenum U1, used as the degree-1 polynomial index in Vope
/// - `K = 1`   : Vope degree param (same as U1; appears as ArrayLength::TypeParam)
/// - `U0 = 0`  : typenum U0
pub fn vole_env() -> MonoEnv {
    MonoEnv::new("sha256")
        .with_len("N", 16)
        .with_len("U1", 1)
        .with_len("U0", 0)
        .with_len("K", 1)
        .with_type("T", IrType::Primitive(PrimitiveType::U8))
}

/// `MonoEnv` for VOLE with 16-lane `Galois` (GF(2^8)) field elements.
///
/// Unlike [`vole_env`], `T = Galois` routes through the C backend's
/// GF-aware arithmetic (`^` for add/sub, `volar_gf8_mul` for multiply).
/// This is required for correct OT-based VOLE where field masks are non-zero.
pub fn galois_vole_env() -> MonoEnv {
    MonoEnv::new("sha256")
        .with_len("N", 16)
        .with_len("U1", 1)
        .with_len("U0", 0)
        .with_len("K", 1)
        .with_type("T", IrType::Primitive(PrimitiveType::Galois))
}

/// Build a `LinkageSystem` using the standard spec (Galois is a PrimitiveType).
pub fn make_vole_linkage_galois() -> LinkageSystem {
    // Standard spec parse suffices; Galois is a PrimitiveType, not a custom struct.
    make_vole_linkage()
}

/// Root selection for woven VOLE modules.
///
/// Covers only `vole_prove_`/`vole_verify_` since the driving circuits
/// (AND/XOR/half-adder) never weave a QSim role (`vole_qsim_`); a caller
/// rooting a QSim-inclusive module passes a broader list.
pub const VOLE_ROOT_PREFIXES: &[&str] = &["vole_prove_", "vole_verify_"];

/// Harness helper roots the e2e `main` calls directly.
pub const VOLE_ROOT_EXACT: &[&str] = &["vole_and_verifier_check", "vole_and_prover_step"];

/// Lower a woven VOLE module into an arbitrary [`volar_lir::LirTarget`],
/// rooting only woven entries plus harness helpers.
///
/// Rooting every linked Normal function would force unused
/// sbox/`mul_generalized` specializations that small AND/XOR circuits never
/// call.
pub fn lower_vole_to_target<T: volar_lir::LirTarget>(
    module: &IrModule<IrFunction>,
    target: &mut T,
    env: &MonoEnv,
) {
    let roots = volar_lir_codegen::roots_by_name_prefix(
        module,
        VOLE_ROOT_PREFIXES,
        VOLE_ROOT_EXACT,
        env.clone(),
    );
    lower_module_monomorphized(
        module,
        target,
        MonoPlanOptions {
            roots,
            ..Default::default()
        },
    )
    .unwrap_or_else(|err| panic!("LIR monomorphization failed: {err}"));
}
