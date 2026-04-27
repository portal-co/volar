// @reliability: experimental
// @ai: assisted
//! Build script helper for compiling [`SavedLirModule`] to native object code.
//!
//! Intended for use in `build.rs` scripts.  A typical usage:
//!
//! ```rust,ignore
//! // build.rs
//! fn main() {
//!     let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
//!     volar_build::compile_lir_to_object(
//!         std::path::Path::new("src/my_program.lir"),
//!         &out.join("my_program.o"),
//!         "my_program",
//!         volar_build::opt_level_from_env(),
//!     ).unwrap();
//!     println!("cargo:rustc-link-search={}", out.display());
//! }
//! ```

use std::path::Path;

use inkwell::{
    OptimizationLevel,
    context::Context,
    passes::PassBuilderOptions,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
    },
};
use volar_lir_saved::SavedLirModule;
use volar_llvm_backend::LlvmBackend;

/// Read the `OPT_LEVEL` environment variable (set by Cargo for build scripts)
/// and return the corresponding [`OptimizationLevel`].
///
/// | `OPT_LEVEL` | [`OptimizationLevel`]          |
/// |-------------|-------------------------------|
/// | `"0"`       | `None`                        |
/// | `"1"`       | `Less`                        |
/// | `"2"`, `"s"`, `"z"` | `Default`             |
/// | `"3"` or anything else | `Aggressive`       |
pub fn opt_level_from_env() -> OptimizationLevel {
    match std::env::var("OPT_LEVEL").as_deref() {
        Ok("0") => OptimizationLevel::None,
        Ok("1") => OptimizationLevel::Less,
        Ok("2") | Ok("s") | Ok("z") => OptimizationLevel::Default,
        _ => OptimizationLevel::Aggressive,
    }
}

/// Compile a [`SavedLirModule`] (loaded from `saved_path`) to a native object
/// file at `out_path`.
///
/// Also emits `cargo:rerun-if-changed=<saved_path>` so Cargo re-runs the
/// build script when the saved LIR file changes.
///
/// # Arguments
///
/// - `saved_path` — path to an rkyv-serialized [`SavedLirModule`] file.
/// - `out_path` — destination for the emitted object (`.o`) file.
/// - `module_name` — name embedded in the LLVM module (used as the module ID).
/// - `opt_level` — LLVM optimization level; use [`opt_level_from_env`] to
///   match the Cargo profile's `opt-level`.
///
/// # Errors
///
/// Returns a `Box<dyn std::error::Error>` on I/O or LLVM errors.
pub fn compile_lir_to_object(
    saved_path: &Path,
    out_path: &Path,
    module_name: &str,
    opt_level: OptimizationLevel,
) -> Result<(), Box<dyn std::error::Error>> {
    // Tell Cargo to re-run this build script if the saved LIR changes.
    println!("cargo:rerun-if-changed={}", saved_path.display());

    // ---- Deserialize --------------------------------------------------------
    let bytes = std::fs::read(saved_path)?;
    let saved: SavedLirModule = rkyv::from_bytes::<SavedLirModule, rkyv::rancor::Error>(&bytes)?;

    // ---- Replay into LlvmBackend --------------------------------------------
    let context = Context::create();
    let mut backend = LlvmBackend::new(&context, module_name);
    saved.replay(&mut backend);
    let module = backend.finish();

    // ---- Optimize -----------------------------------------------------------
    // Map OptimizationLevel to an LLVM new-pass-manager pipeline string.
    let pass_pipeline = match opt_level {
        OptimizationLevel::None => None,
        OptimizationLevel::Less => Some("default<O1>"),
        OptimizationLevel::Default => Some("default<O2>"),
        OptimizationLevel::Aggressive => Some("default<O3>"),
    };

    // ---- Emit object file ---------------------------------------------------
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| format!("LLVM target initialization failed: {e}"))?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .map_err(|e| format!("LLVM target from triple failed: {e}"))?;

    let cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();

    let target_machine = target
        .create_target_machine(
            &triple,
            cpu.to_str().unwrap_or(""),
            features.to_str().unwrap_or(""),
            opt_level,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or("Failed to create LLVM TargetMachine")?;

    // Run new-pass-manager optimization pipeline.
    if let Some(pipeline) = pass_pipeline {
        module
            .run_passes(pipeline, &target_machine, PassBuilderOptions::create())
            .map_err(|e| format!("LLVM run_passes failed: {e}"))?;
    }

    target_machine
        .write_to_file(&module, FileType::Object, out_path)
        .map_err(|e| format!("LLVM write_to_file failed: {e}"))?;

    Ok(())
}

/// Serialize a [`SavedLirModule`] to bytes using rkyv.
///
/// Convenience function for the recording side: after recording into a
/// [`volar_lir_saved::RecordingTarget`], call `finish()` then this function
/// to write the bytes to a file.
pub fn serialize_saved_module(
    module: &SavedLirModule,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(module)?;
    Ok(bytes.into_vec())
}
