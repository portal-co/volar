// @reliability: experimental
// @ai: assisted
//! Build script helper for compiling [`SavedLirModule`] to object code.
//!
//! Intended for use in `build.rs` scripts.  [`compile_lir_to_object`]
//! automatically reads Cargo's `TARGET`, `HOST`, and `OPT_LEVEL` environment
//! variables, so `CompileOptions::default()` works for both native and
//! cross-compilation builds without any explicit configuration:
//!
//! ```rust,ignore
//! // build.rs
//! fn main() {
//!     let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
//!     volar_build::compile_lir_to_object(
//!         std::path::Path::new("src/my_program.lir"),
//!         &out.join("my_program.o"),
//!         &volar_build::CompileOptions::default(),
//!     ).unwrap();
//!     println!("cargo:rustc-link-search={}", out.display());
//! }
//! ```
//!
//! To add name remapping (cross-compilation is still auto-detected):
//!
//! ```rust,ignore
//! // build.rs
//! fn main() {
//!     let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
//!     let opts = volar_build::CompileOptions::default()
//!         .with_prefix("mylib_");
//!     volar_build::compile_lir_to_object(
//!         std::path::Path::new("src/my_program.lir"),
//!         &out.join("my_program.o"),
//!         &opts,
//!     ).unwrap();
//!     println!("cargo:rustc-link-search={}", out.display());
//! }
//! ```
//!
//! To force a specific target triple (overrides Cargo env):
//!
//! ```rust,ignore
//! let opts = volar_build::CompileOptions::default()
//!     .for_target("aarch64-unknown-linux-gnu")
//!     .with_cpu("cortex-a55");
//! ```

#[cfg(feature = "weave-rust")]
mod weave;
#[cfg(feature = "weave-rust")]
pub use weave::{
    Weaver, SavedCircuit, emit_woven_rust,
    serialize_boolar_circuit, serialize_volar_circuit,
};
#[cfg(feature = "weave-rust")]
pub use volar_weaver::StorageSizes;

use std::path::Path;

use inkwell::{
    OptimizationLevel,
    context::Context,
    passes::PassBuilderOptions,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
};
use volar_lir_saved::SavedLirModule;
use volar_llvm_backend::LlvmBackend;

pub use volar_lir::NameConfig;

// ============================================================================
// CompileOptions
// ============================================================================

/// Options controlling how a [`SavedLirModule`] is compiled to object code.
///
/// Build with the provided builder methods; unset fields fall back to
/// sensible defaults.
///
/// # Defaults
///
/// | Field          | Default                                             |
/// |----------------|-----------------------------------------------------|
/// | `module_name`  | `None` (derived from the `out_path` stem at call time) |
/// | `opt_level`    | [`opt_level_from_env`] at call time                 |
/// | `name_config`  | Identity (no prefix, no remaps)                     |
/// | `target_triple`| `None` (native target)                              |
/// | `cpu`          | `None` (host CPU for native; `"generic"` for cross) |
/// | `features`     | `None` (host features for native; `""` for cross)   |
#[derive(Clone, Debug, Default)]
pub struct CompileOptions {
    /// LLVM module name (the module ID embedded in IR).
    ///
    /// When `None`, the stem of `out_path` is used at compile time.
    pub module_name: Option<String>,

    /// LLVM optimization level.
    ///
    /// When `None`, [`opt_level_from_env`] is called at compile time.
    pub opt_level: Option<OptimizationLevel>,

    /// Name remapping applied to all emitted and called function names.
    pub name_config: NameConfig,

    /// Target triple override, e.g. `"aarch64-unknown-linux-gnu"`.
    ///
    /// When `None` (the default), [`compile_lir_to_object`] reads the `TARGET`
    /// and `HOST` environment variables that Cargo sets in every `build.rs`.
    /// If `TARGET != HOST` the `TARGET` triple is used automatically, so
    /// cross-compilation works without any explicit configuration.
    ///
    /// Set this field only when you need to override the Cargo-detected target
    /// or compile outside of a `build.rs` context.
    pub target_triple: Option<String>,

    /// CPU name passed to the LLVM `TargetMachine`.
    ///
    /// `None` uses the host CPU name for native builds, or `"generic"` for
    /// cross-compilation builds.
    pub cpu: Option<String>,

    /// CPU feature string passed to the LLVM `TargetMachine`
    /// (e.g. `"+neon,+fp-armv8"`).
    ///
    /// `None` uses the host feature string for native builds, or `""` for
    /// cross-compilation builds.
    pub features: Option<String>,
}

impl CompileOptions {
    /// Set the LLVM module name.
    pub fn with_module_name(mut self, name: impl Into<String>) -> Self {
        self.module_name = Some(name.into());
        self
    }

    /// Set the optimization level.
    pub fn with_opt_level(mut self, level: OptimizationLevel) -> Self {
        self.opt_level = Some(level);
        self
    }

    /// Replace the full name configuration.
    pub fn with_name_config(mut self, config: NameConfig) -> Self {
        self.name_config = config;
        self
    }

    /// Convenience: set a prefix prepended to all emitted function names.
    pub fn with_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.name_config.prefix = prefix.into();
        self
    }

    /// Set the target triple for cross-compilation.
    ///
    /// Setting this causes `Target::initialize_all` to be called instead of
    /// `Target::initialize_native`, and disables automatic use of the host
    /// CPU / feature string.
    pub fn for_target(mut self, triple: impl Into<String>) -> Self {
        self.target_triple = Some(triple.into());
        self
    }

    /// Set the CPU name for the target machine.
    pub fn with_cpu(mut self, cpu: impl Into<String>) -> Self {
        self.cpu = Some(cpu.into());
        self
    }

    /// Set the CPU feature string for the target machine.
    pub fn with_features(mut self, features: impl Into<String>) -> Self {
        self.features = Some(features.into());
        self
    }
}

// ============================================================================
// Public helpers
// ============================================================================

/// Read the `OPT_LEVEL` environment variable (set by Cargo for build scripts)
/// and return the corresponding [`OptimizationLevel`].
///
/// | `OPT_LEVEL`             | [`OptimizationLevel`]  |
/// |-------------------------|------------------------|
/// | `"0"`                   | `None`                 |
/// | `"1"`                   | `Less`                 |
/// | `"2"`, `"s"`, `"z"`    | `Default`              |
/// | `"3"` or anything else  | `Aggressive`           |
pub fn opt_level_from_env() -> OptimizationLevel {
    match std::env::var("OPT_LEVEL").as_deref() {
        Ok("0") => OptimizationLevel::None,
        Ok("1") => OptimizationLevel::Less,
        Ok("2") | Ok("s") | Ok("z") => OptimizationLevel::Default,
        _ => OptimizationLevel::Aggressive,
    }
}

// ============================================================================
// compile_lir_to_object
// ============================================================================

/// Compile a [`SavedLirModule`] (loaded from `saved_path`) to an object file
/// at `out_path`.
///
/// Also emits `cargo:rerun-if-changed=<saved_path>` so Cargo re-runs the
/// build script when the saved LIR file changes.
///
/// # Arguments
///
/// - `saved_path` — path to an rkyv-serialized [`SavedLirModule`] file.
/// - `out_path` — destination for the emitted object (`.o`) file.
/// - `options` — compilation options; see [`CompileOptions`].
///
/// # Errors
///
/// Returns a `Box<dyn std::error::Error>` on I/O or LLVM errors.
pub fn compile_lir_to_object(
    saved_path: &Path,
    out_path: &Path,
    options: &CompileOptions,
) -> Result<(), Box<dyn std::error::Error>> {
    // Tell Cargo to re-run this build script if the saved LIR changes.
    println!("cargo:rerun-if-changed={}", saved_path.display());

    let opt_level = options.opt_level.unwrap_or_else(opt_level_from_env);

    let module_name = options
        .module_name
        .as_deref()
        .or_else(|| out_path.file_stem().and_then(|s| s.to_str()))
        .unwrap_or("volar_module");

    // ---- Deserialize --------------------------------------------------------
    let bytes = std::fs::read(saved_path)?;
    let saved: SavedLirModule = rkyv::from_bytes::<SavedLirModule, rkyv::rancor::Error>(&bytes)?;

    // ---- Replay into LlvmBackend --------------------------------------------
    let context = Context::create();
    let mut backend = LlvmBackend::new(&context, module_name)
        .with_name_config(options.name_config.clone());
    saved.replay(&mut backend);
    let module = backend.finish();

    // ---- Resolve target machine ---------------------------------------------
    // Prefer an explicit override, then fall back to Cargo's TARGET env var.
    // Cargo always sets TARGET (and HOST) in build scripts; comparing them
    // tells us whether this is a cross-compilation.
    let cargo_target = std::env::var("TARGET").ok();
    let cargo_host  = std::env::var("HOST").ok();
    let explicit_triple = options.target_triple.as_deref();

    // Resolved triple string (owned).
    let resolved_triple_str: Option<String> = explicit_triple
        .map(str::to_owned)
        .or_else(|| {
            // Use Cargo's TARGET only when it differs from HOST (cross build),
            // or HOST is unavailable (not running inside a build script).
            match (&cargo_target, &cargo_host) {
                (Some(t), Some(h)) if t != h => Some(t.clone()),
                (Some(_), Some(_)) => None, // native — let LLVM auto-detect
                (Some(t), None)    => Some(t.clone()), // unknown host, use TARGET
                _                  => None,
            }
        });

    let (triple, cpu_str, features_str) = match resolved_triple_str {
        None => {
            // Native target: initialize just the host backend.
            Target::initialize_native(&InitializationConfig::default())
                .map_err(|e| format!("LLVM native target initialization failed: {e}"))?;

            let triple = TargetMachine::get_default_triple();
            let cpu = options
                .cpu
                .as_deref()
                .map(str::to_owned)
                .unwrap_or_else(|| TargetMachine::get_host_cpu_name().to_string_lossy().into_owned());
            let features = options
                .features
                .as_deref()
                .map(str::to_owned)
                .unwrap_or_else(|| {
                    TargetMachine::get_host_cpu_features()
                        .to_string_lossy()
                        .into_owned()
                });
            (triple, cpu, features)
        }
        Some(triple_str) => {
            // Cross-compilation: initialize all LLVM backends.
            Target::initialize_all(&InitializationConfig::default());

            let triple = TargetTriple::create(&triple_str);
            let cpu = options.cpu.as_deref().unwrap_or("generic").to_owned();
            let features = options.features.as_deref().unwrap_or("").to_owned();
            (triple, cpu, features)
        }
    };

    let target = Target::from_triple(&triple)
        .map_err(|e| format!("LLVM target from triple failed: {e}"))?;

    let target_machine = target
        .create_target_machine(
            &triple,
            &cpu_str,
            &features_str,
            opt_level,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or("Failed to create LLVM TargetMachine")?;

    // ---- Optimize -----------------------------------------------------------
    let pass_pipeline = match opt_level {
        OptimizationLevel::None => None,
        OptimizationLevel::Less => Some("default<O1>"),
        OptimizationLevel::Default => Some("default<O2>"),
        OptimizationLevel::Aggressive => Some("default<O3>"),
    };
    if let Some(pipeline) = pass_pipeline {
        module
            .run_passes(pipeline, &target_machine, PassBuilderOptions::create())
            .map_err(|e| format!("LLVM run_passes failed: {e}"))?;
    }

    // ---- Emit object file ---------------------------------------------------
    target_machine
        .write_to_file(&module, FileType::Object, out_path)
        .map_err(|e| format!("LLVM write_to_file failed: {e}"))?;

    Ok(())
}

// ============================================================================
// serialize_saved_module
// ============================================================================

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
