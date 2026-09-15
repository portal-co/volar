// @reliability: experimental
// @ai: assisted
//! Builder-style pipeline for compiling from any IR level to object code or
//! woven Rust. IR transforms live in `volar-ir-build`; this crate adds object
//! emit, weaving, and cargo-directives.
//!
//! Mirrors `volar_ir_build::Pipeline<S>`'s typestate shape: `Pipeline<S>`
//! wraps a `volar_ir_build::Pipeline<S>` plus this crate's own
//! `cargo:rerun-if-changed` bookkeeping, so a terminal like
//! [`Pipeline::<VolarIrStage>::compile_to_object`] only exists for the stage
//! it actually needs — the "weaver X requires Boolar IR"-style runtime
//! checks this crate used to need are narrowed to real per-stage impl
//! blocks wherever the stage alone determines applicability.

use std::path::{Path, PathBuf};

use volar_ir::ir::{IRBlocks, IRTypes};
use volar_ir_build::{LirStage, PipelineStage, VolarIrStage};
use volar_lir_saved::SavedLirModule;

use crate::{CompileOptions, SavedCircuit};

pub use volar_ir_build::{
    BoolarCircuitStage, BoolarStage, FoldIr, FromReversible, FuseBoolar, LowerToBoolar, LowerToLir,
    Movfuscate, PipelinePass, RCircuitStage, StorageToMuxBoolar, StorageToMuxIr, ToReversible,
    UnrollIrEverything, VaffleStage,
};

type BoxError = Box<dyn std::error::Error>;

/// A composable lowering pipeline for `build.rs` scripts, generic over its
/// current [`PipelineStage`] — see the module docs.
pub struct Pipeline<S: PipelineStage> {
    inner: volar_ir_build::Pipeline<S>,
    rerun: Vec<PathBuf>,
}

impl<S: PipelineStage> Pipeline<S> {
    fn wrap(inner: volar_ir_build::Pipeline<S>, rerun: impl IntoIterator<Item = PathBuf>) -> Self {
        Pipeline {
            inner,
            rerun: rerun.into_iter().collect(),
        }
    }

    fn emit_rerun(&self) {
        #[cfg(feature = "cargo-directives")]
        for p in &self.rerun {
            println!("cargo:rerun-if-changed={}", p.display());
        }
        #[cfg(not(feature = "cargo-directives"))]
        let _ = &self.rerun;
    }

    /// Wrap already-in-hand stage data, carrying no rerun-if-changed paths.
    pub fn from_data(data: S::Data) -> Self {
        Self::wrap(volar_ir_build::Pipeline::from_data(data), [])
    }

    /// Apply any [`PipelinePass`] whose input stage is `S`.
    pub fn apply<P: PipelinePass<S>>(self, pass: P) -> Result<Pipeline<P::Output>, BoxError> {
        Ok(Pipeline {
            inner: self.inner.apply(pass)?,
            rerun: self.rerun,
        })
    }

    fn map_inner<T: PipelineStage>(
        self,
        f: impl FnOnce(volar_ir_build::Pipeline<S>) -> Result<volar_ir_build::Pipeline<T>, BoxError>,
    ) -> Result<Pipeline<T>, BoxError> {
        Ok(Pipeline {
            inner: f(self.inner)?,
            rerun: self.rerun,
        })
    }
}

impl Pipeline<LirStage> {
    /// Start from a pre-recorded `.lir` file.
    pub fn from_saved_lir(path: impl Into<PathBuf>) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_saved_lir(&path)?,
            [path],
        ))
    }

    /// Terminal: the saved LIR module.
    pub fn to_lir(self) -> SavedLirModule {
        self.emit_rerun();
        self.inner.to_lir()
    }

    /// Execute all passes and compile the result to a native object file.
    pub fn compile_to_object(
        self,
        out_path: &Path,
        options: &CompileOptions,
    ) -> Result<(), BoxError> {
        self.emit_rerun();
        lir_to_object(&self.inner.to_lir(), out_path, options)
    }
}

impl Pipeline<VolarIrStage> {
    /// Start from a `.circuit` file (rkyv-serialized `SavedCircuit`), falling
    /// back to a bare `(IRBlocks, IRTypes)` blob.
    pub fn from_volar_ir(path: impl Into<PathBuf>) -> Result<Self, BoxError> {
        let path = path.into();
        let inner = match load_saved_circuit(&path) {
            Ok((blocks, types)) => volar_ir_build::Pipeline::from_volar_ir_blocks(blocks, types),
            Err(_) => volar_ir_build::Pipeline::from_volar_ir_file(&path)?,
        };
        Ok(Self::wrap(inner, [path]))
    }

    /// Wrap in-memory Volar IR.
    pub fn from_volar_ir_blocks(blocks: IRBlocks, types: IRTypes) -> Self {
        Self::from_data((blocks, types))
    }

    /// Constant-fold Volar IR until stable.
    pub fn fold_ir(self) -> Result<Self, BoxError> {
        self.map_inner(|p| p.fold_ir())
    }

    /// Movfuscate Volar IR into a single self-looping block.
    pub fn movfuscate(self) -> Result<Self, BoxError> {
        self.map_inner(|p| p.movfuscate())
    }

    /// Unroll Volar IR into a combinational circuit (concrete CF required).
    pub fn unroll_ir(self) -> Result<Self, BoxError> {
        self.map_inner(|p| p.unroll_ir())
    }

    /// Eliminate `StorageRead`/`StorageWrite` for `cfg.storage` via an
    /// explicit MUX/demux register file.
    pub fn storage_to_mux(
        self,
        cfg: volar_ir_build::volar_ir_passes::StorageToMuxConfig,
    ) -> Result<Self, BoxError> {
        self.map_inner(|p| p.storage_to_mux(cfg))
    }

    /// Lower Volar IR → Boolar IR.
    pub fn lower_to_boolar(self) -> Result<Pipeline<BoolarStage>, BoxError> {
        self.map_inner(|p| p.lower_to_boolar())
    }

    /// Lower Volar IR → saved LIR.
    pub fn lower_to_lir(self) -> Result<Pipeline<LirStage>, BoxError> {
        self.map_inner(|p| p.lower_to_lir())
    }

    /// Terminal: the resulting Volar IR.
    pub fn to_volar_ir(self) -> (IRBlocks, IRTypes) {
        self.emit_rerun();
        self.inner.to_volar_ir()
    }

    /// Execute all passes and compile the result to a native object file
    /// (lowering to LIR first).
    pub fn compile_to_object(
        self,
        out_path: &Path,
        options: &CompileOptions,
    ) -> Result<(), BoxError> {
        self.emit_rerun();
        let saved = self.inner.lower_to_lir()?.into_data();
        lir_to_object(&saved, out_path, options)
    }

    /// Execute all passes and emit woven Rust source.
    #[cfg(feature = "weave-rust")]
    pub fn emit_woven_rust(self, out_path: &Path, weaver: &crate::Weaver) -> Result<(), BoxError> {
        self.emit_rerun();
        let (blocks, types) = self.inner.to_volar_ir();
        weave_volar_ir_in_memory(&blocks, &types, out_path, weaver)
    }

    /// Execute all passes and emit chunked Rust source files into `out_dir/`.
    #[cfg(feature = "weave-chunked")]
    pub fn emit_woven_rust_chunked(
        self,
        out_dir: &Path,
        weaver: &crate::Weaver,
        options: &volar_compiler::chunk_module::ChunkOptions,
    ) -> Result<Vec<std::path::PathBuf>, BoxError> {
        self.emit_rerun();
        let (blocks, types) = self.inner.to_volar_ir();
        weave_volar_ir_chunked(&blocks, &types, out_dir, weaver, options)
    }

    /// Execute all passes and emit chunked TypeScript source files into `out_dir/`.
    #[cfg(feature = "weave-ts")]
    pub fn emit_woven_typescript_chunked(
        self,
        out_dir: &Path,
        weaver: &crate::Weaver,
        options: &volar_compiler::chunk_module::ChunkOptions,
    ) -> Result<Vec<std::path::PathBuf>, BoxError> {
        self.emit_rerun();
        let (blocks, types) = self.inner.to_volar_ir();
        let module = weave_volar_ir_to_ir_module(&blocks, &types, weaver)?;
        volar_compiler_passes::emit_woven_ts_chunked(&module, out_dir, options)
    }
}

impl Pipeline<BoolarStage> {
    /// Eliminate `StorageRead`/`StorageWrite` for `cfg.storage`/`cfg.lane`
    /// via an explicit MUX/demux bit register file.
    pub fn storage_to_mux(
        self,
        cfg: volar_ir_build::volar_ir_passes::StorageToMuxBoolarConfig,
    ) -> Result<Self, BoxError> {
        self.map_inner(|p| p.storage_to_mux(cfg))
    }

    /// Fuse to the single-block circuit form.
    pub fn fuse(
        self,
        limit: u32,
        mode: volar_ir_build::volar_ir_passes::LoweringMode,
    ) -> Result<Pipeline<BoolarCircuitStage>, BoxError> {
        self.map_inner(|p| p.fuse(limit, mode))
    }
}

impl Pipeline<BoolarCircuitStage> {
    /// Convert to a reversible gate circuit.
    pub fn to_reversible(self) -> Result<Pipeline<RCircuitStage>, BoxError> {
        self.map_inner(|p| p.to_reversible())
    }
}

impl Pipeline<RCircuitStage> {
    /// Lower back to circuit-fused Boolar IR.
    pub fn from_reversible(self) -> Result<Pipeline<BoolarCircuitStage>, BoxError> {
        self.map_inner(|p| p.from_reversible())
    }
}

#[cfg(feature = "pipeline-vaffle")]
impl Pipeline<VaffleStage> {
    /// Start from a `.vaffle` file.
    pub fn from_vaffle(path: impl Into<PathBuf>) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_vaffle(&path)?,
            [path],
        ))
    }

    /// Start from a `.wasm` file.
    #[cfg(feature = "pipeline-wasm")]
    pub fn from_wasm(path: impl Into<PathBuf>) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_wasm(&path)?,
            [path],
        ))
    }

    /// Start from a `.wasm` file with an explicit oracle/action import config.
    #[cfg(feature = "pipeline-wasm")]
    pub fn from_wasm_with_config(
        path: impl Into<PathBuf>,
        config: volar_ir_build::WaffleImportConfig,
    ) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_wasm_with_config(&path, config)?,
            [path],
        ))
    }

    /// Fully-inlined WASM frontend (WAFFLE → VAFFLE → inline-everything).
    #[cfg(feature = "pipeline-wasm")]
    pub fn from_wasm_inlined(path: impl Into<PathBuf>) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_wasm_inlined(&path)?,
            [path],
        ))
    }

    /// Structural LLVM import from `.ll`, `.bc`, or a clang full-LTO
    /// static library (`.a` / `.lib`). Calls are preserved until a later pass.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_llvm(path: impl Into<PathBuf>, entries: &[&str]) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_llvm(&path, entries)?,
            [path],
        ))
    }

    /// Structural LLVM import plus VAFFLE inline-everything.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_llvm_inlined(path: impl Into<PathBuf>, entries: &[&str]) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_llvm_inlined(&path, entries)?,
            [path],
        ))
    }

    /// Compile `build` to a clang full-LTO static library, then import
    /// structurally. Emits `cargo:rerun-if-changed` for each source on
    /// `build`. GCC LTO is rejected.
    #[cfg(feature = "pipeline-cc")]
    pub fn from_cc(build: cc::Build, lib_name: &str, entries: &[&str]) -> Result<Self, BoxError> {
        let rerun: Vec<PathBuf> = build.get_files().map(Path::to_path_buf).collect();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_cc(build, lib_name, entries)?,
            rerun,
        ))
    }

    /// [`Pipeline::from_cc`] plus VAFFLE inline-everything.
    #[cfg(feature = "pipeline-cc")]
    pub fn from_cc_inlined(
        build: cc::Build,
        lib_name: &str,
        entries: &[&str],
    ) -> Result<Self, BoxError> {
        let rerun: Vec<PathBuf> = build.get_files().map(Path::to_path_buf).collect();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_cc_inlined(build, lib_name, entries)?,
            rerun,
        ))
    }

    /// Run `cmd` (no shell) to produce an LTO static library, then import
    /// structurally. The command's inputs are not known here — emit
    /// `cargo:rerun-if-changed` in the calling `build.rs` if needed.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_command(
        cmd: volar_ir_build::CommandBuild,
        entries: &[&str],
    ) -> Result<Self, BoxError> {
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_command(cmd, entries)?,
            [],
        ))
    }

    /// [`Pipeline::from_command`] plus VAFFLE inline-everything.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_command_inlined(
        cmd: volar_ir_build::CommandBuild,
        entries: &[&str],
    ) -> Result<Self, BoxError> {
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_command_inlined(cmd, entries)?,
            [],
        ))
    }

    /// Construct the deterministic baseline Rust-to-LLVM command for a future
    /// deferred-compute provider, then import it structurally through the
    /// ordinary LLVM/VAFFLE pipeline. `entries` are the provider's declared
    /// public ABI names; protocol code must still validate and bind that ABI.
    ///
    /// This is intentionally generic: it accepts a reviewed future FHE
    /// provider or a practical heavy-garbling implementation, but selects
    /// neither and does not use the historical `FheScheme` integration.
    // TODO(provider-ledger: FHE-PLUMB-TOOLCHAIN-02): exercise this with a
    // reviewed deterministic fixture after the target toolchain prerequisite.
    #[cfg(feature = "provider-llvm-toolchain")]
    pub fn from_provider_artifact(
        spec: crate::fhe_provider::ProviderArtifactSpec,
        rustc: impl AsRef<Path>,
    ) -> Result<Self, BoxError> {
        let entries = spec.entry_points.clone();
        let entry_refs: Vec<&str> = entries.iter().map(String::as_str).collect();
        let source = spec.source.clone();
        let command = spec.command_build(rustc)?;
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_command(command, &entry_refs)?,
            [source],
        ))
    }

    /// [`Self::from_provider_artifact`] plus VAFFLE inline-everything over the
    /// provider's declared public entry ABI.
    #[cfg(feature = "provider-llvm-toolchain")]
    pub fn from_provider_artifact_inlined(
        spec: crate::fhe_provider::ProviderArtifactSpec,
        rustc: impl AsRef<Path>,
    ) -> Result<Self, BoxError> {
        let entries = spec.entry_points.clone();
        let entry_refs: Vec<&str> = entries.iter().map(String::as_str).collect();
        Self::from_provider_artifact(spec, rustc)?.inline_vaffle_everything_over(&entry_refs)
    }

    /// Inline every non-recursive intra-module VAFFLE call, using every
    /// export (or every function body) as the root set.
    pub fn inline_vaffle_everything(self) -> Result<Self, BoxError> {
        self.map_inner(|p| p.inline_vaffle_everything())
    }

    /// Inline every non-recursive intra-module VAFFLE call reachable from
    /// `entries`.
    pub fn inline_vaffle_everything_over(self, entries: &[&str]) -> Result<Self, BoxError> {
        self.map_inner(|p| p.inline_vaffle_everything_over(entries))
    }

    /// Lower VAFFLE → Volar IR.
    pub fn lower_to_volar_ir(self) -> Result<Pipeline<VolarIrStage>, BoxError> {
        self.map_inner(|p| p.lower_to_volar_ir())
    }
}

/// Execution-mode LLVM-direct import (already `is_circuit()` when it
/// succeeds) lands on [`VolarIrStage`] directly rather than [`VaffleStage`].
#[cfg(feature = "pipeline-llvm")]
impl Pipeline<VolarIrStage> {
    /// Execution-mode LLVM-direct import. Accepts `.ll`, `.bc`, or an LTO
    /// static library.
    pub fn from_llvm_direct(path: impl Into<PathBuf>, entry: &str) -> Result<Self, BoxError> {
        let path = path.into();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_llvm_direct(&path, entry)?,
            [path],
        ))
    }

    /// [`Pipeline::<VaffleStage>::from_cc`] then the execution-mode importer.
    #[cfg(feature = "pipeline-cc")]
    pub fn from_cc_direct(build: cc::Build, lib_name: &str, entry: &str) -> Result<Self, BoxError> {
        let rerun: Vec<PathBuf> = build.get_files().map(Path::to_path_buf).collect();
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_cc_direct(build, lib_name, entry)?,
            rerun,
        ))
    }

    /// [`Pipeline::<VaffleStage>::from_command`] then the execution-mode
    /// importer.
    pub fn from_command_direct(
        cmd: volar_ir_build::CommandBuild,
        entry: &str,
    ) -> Result<Self, BoxError> {
        Ok(Self::wrap(
            volar_ir_build::Pipeline::from_command_direct(cmd, entry)?,
            [],
        ))
    }
}

fn load_saved_circuit(path: &Path) -> Result<(IRBlocks, IRTypes), BoxError> {
    let bytes = std::fs::read(path)?;
    let circuit = rkyv::from_bytes::<SavedCircuit, rkyv::rancor::Error>(&bytes)
        .map_err(|e| format!("failed to deserialize .circuit file: {e}"))?;
    match circuit {
        SavedCircuit::Volar(blocks, types) => Ok((blocks, types)),
        _ => Err("expected a Volar circuit file, got Boolar".into()),
    }
}

fn lir_to_object(
    saved: &SavedLirModule,
    out_path: &Path,
    options: &CompileOptions,
) -> Result<(), BoxError> {
    use inkwell::{
        context::Context,
        passes::PassBuilderOptions,
        targets::{
            CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
            TargetTriple,
        },
    };
    use volar_llvm_backend::LlvmBackend;

    let opt_level = options.opt_level.unwrap_or_else(crate::opt_level_from_env);
    let module_name = options
        .module_name
        .as_deref()
        .or_else(|| out_path.file_stem().and_then(|s| s.to_str()))
        .unwrap_or("volar_module");

    let context = Context::create();
    let mut backend =
        LlvmBackend::new(&context, module_name).with_name_config(options.name_config.clone());
    saved.replay(&mut backend);
    let module = backend.finish();

    let cargo_target = std::env::var("TARGET").ok();
    let cargo_host = std::env::var("HOST").ok();
    let explicit_triple = options.target_triple.as_deref();

    let resolved_triple_str: Option<String> =
        explicit_triple
            .map(str::to_owned)
            .or_else(|| match (&cargo_target, &cargo_host) {
                (Some(t), Some(h)) if t != h => Some(t.clone()),
                (Some(_), Some(_)) => None,
                (Some(t), None) => Some(t.clone()),
                _ => None,
            });

    let (triple, cpu_str, features_str) = match resolved_triple_str {
        None => {
            Target::initialize_native(&InitializationConfig::default())
                .map_err(|e| format!("LLVM native target init failed: {e}"))?;
            let triple = TargetMachine::get_default_triple();
            let cpu = options
                .cpu
                .as_deref()
                .map(str::to_owned)
                .unwrap_or_else(|| {
                    TargetMachine::get_host_cpu_name()
                        .to_string_lossy()
                        .into_owned()
                });
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
            Target::initialize_all(&InitializationConfig::default());
            let triple = TargetTriple::create(&triple_str);
            let cpu = options.cpu.as_deref().unwrap_or("generic").to_owned();
            let features = options.features.as_deref().unwrap_or("").to_owned();
            (triple, cpu, features)
        }
    };

    let target =
        Target::from_triple(&triple).map_err(|e| format!("LLVM target from triple: {e}"))?;
    let target_machine = target
        .create_target_machine(
            &triple,
            &cpu_str,
            &features_str,
            opt_level,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or("failed to create LLVM TargetMachine")?;

    let pass_pipeline = match opt_level {
        inkwell::OptimizationLevel::None => None,
        inkwell::OptimizationLevel::Less => Some("default<O1>"),
        inkwell::OptimizationLevel::Default => Some("default<O2>"),
        inkwell::OptimizationLevel::Aggressive => Some("default<O3>"),
    };
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

#[cfg(feature = "weave-rust")]
fn weave_volar_ir_in_memory(
    blocks: &IRBlocks,
    types: &IRTypes,
    out_path: &Path,
    weaver: &crate::Weaver,
) -> Result<(), BoxError> {
    use crate::Weaver;

    let rust_source: String = match weaver {
        Weaver::VoleProverIr { name, storage_sizes } => {
            let module = volar_weaver::weave_vole_prover_ir(blocks, types, name, storage_sizes, None).into_inner();
            volar_weaver::print_weaved_vole_module(&module)
        }
        Weaver::VoleVerifierIr { name, storage_sizes } => {
            let module = volar_weaver::weave_vole_verifier_ir(blocks, types, name, storage_sizes, None).into_inner();
            volar_weaver::print_weaved_vole_module(&module)
        }
        w => return Err(format!(
            "weaver {:?} requires Boolar IR; pass a Boolar circuit or use emit_woven_rust with a .circuit file",
            w
        ).into()),
    };

    std::fs::write(out_path, rust_source)?;
    Ok(())
}

#[cfg(feature = "weave-chunked")]
fn weave_volar_ir_chunked(
    blocks: &IRBlocks,
    types: &IRTypes,
    out_dir: &Path,
    weaver: &crate::Weaver,
    options: &volar_compiler::chunk_module::ChunkOptions,
) -> Result<Vec<std::path::PathBuf>, BoxError> {
    use volar_compiler::chunk_module::{ChunkConfig, chunk_module_rust};
    use volar_compiler_passes::chunk_function_bodies;

    let module = match weaver {
        crate::Weaver::VoleProverIr { name, storage_sizes } => {
            volar_weaver::weave_vole_prover_ir(blocks, types, name, storage_sizes, None).into_inner()
        }
        crate::Weaver::VoleVerifierIr { name, storage_sizes } => {
            volar_weaver::weave_vole_verifier_ir(blocks, types, name, storage_sizes, None).into_inner()
        }
        w => return Err(format!(
            "Pipeline::emit_woven_rust_chunked: weaver {:?} requires Boolar IR; use emit_woven_rust_chunked (standalone) with a .circuit file instead",
            w
        ).into()),
    };

    let chunked_module;
    let effective = if let Some(threshold) = options.fn_chunk_max_stmts {
        chunked_module = chunk_function_bodies(&module, threshold);
        &chunked_module
    } else {
        &module
    };

    let items_per_chunk = options.module_items_per_chunk.unwrap_or(usize::MAX);
    let output = chunk_module_rust(effective, &ChunkConfig { items_per_chunk }, &[]);

    std::fs::create_dir_all(out_dir)?;

    let mut written = Vec::new();
    let wrapper_path = out_dir.join("mod.rs");
    std::fs::write(&wrapper_path, &output.wrapper)?;
    written.push(wrapper_path);
    for (i, src) in output.chunks.iter().enumerate() {
        let chunk_path = out_dir.join(format!("chunk_{i}.rs"));
        std::fs::write(&chunk_path, src)?;
        written.push(chunk_path);
    }
    Ok(written)
}

#[cfg(feature = "weave-ts")]
fn weave_volar_ir_to_ir_module(
    blocks: &IRBlocks,
    types: &IRTypes,
    weaver: &crate::Weaver,
) -> Result<volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>, BoxError> {
    use crate::Weaver;
    let module = match weaver {
        Weaver::VoleProverIr { name, storage_sizes } => {
            volar_weaver::weave_vole_prover_ir(blocks, types, name, storage_sizes, None).into_inner()
        }
        Weaver::VoleVerifierIr { name, storage_sizes } => {
            volar_weaver::weave_vole_verifier_ir(blocks, types, name, storage_sizes, None).into_inner()
        }
        w => return Err(format!(
            "Pipeline TS emit: weaver {:?} requires Boolar IR; use emit_woven_typescript_chunked (standalone) with a .circuit file instead",
            w
        ).into()),
    };
    Ok(module)
}
