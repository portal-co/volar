// @reliability: experimental
// @ai: assisted
//! Builder-style pipeline for compiling from any IR level to object code or
//! woven Rust. IR transforms live in `volar-ir-build`; this crate adds object
//! emit, weaving, and cargo-directives.

use std::path::{Path, PathBuf};

use volar_ir::ir::{IRBlocks, IRTypes};
use volar_lir_saved::SavedLirModule;

use crate::{CompileOptions, SavedCircuit};

pub use volar_ir_build::PipelinePass;

/// A composable lowering pipeline for `build.rs` scripts.
///
/// Wraps [`volar_ir_build::Pipeline`] and adds object-file / weave terminals.
pub struct Pipeline {
    inner: volar_ir_build::Pipeline,
    rerun: Vec<PathBuf>,
}

impl Pipeline {
    fn wrap(inner: volar_ir_build::Pipeline, rerun: impl IntoIterator<Item = PathBuf>) -> Self {
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

    /// Start from a pre-recorded `.lir` file.
    pub fn from_saved_lir(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        Self::wrap(volar_ir_build::Pipeline::from_saved_lir(&path), [path])
    }

    /// Start from a `.circuit` file (rkyv-serialized `SavedCircuit`).
    pub fn from_volar_ir(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        match load_saved_circuit(&path) {
            Ok((blocks, types)) => Self::wrap(
                volar_ir_build::Pipeline::from_volar_ir_blocks(blocks, types),
                [path],
            ),
            Err(_) => {
                // Fall back to a bare `(IRBlocks, IRTypes)` blob.
                Self::wrap(volar_ir_build::Pipeline::from_volar_ir(&path), [path])
            }
        }
    }

    /// Start from a `.vaffle` file.
    #[cfg(feature = "pipeline-vaffle")]
    pub fn from_vaffle(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        Self::wrap(volar_ir_build::Pipeline::from_vaffle(&path), [path])
    }

    /// Start from a `.wasm` file.
    #[cfg(feature = "pipeline-wasm")]
    pub fn from_wasm(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        Self::wrap(volar_ir_build::Pipeline::from_wasm(&path), [path])
    }

    /// Fully-inlined WASM frontend (WAFFLE → VAFFLE → inline-everything).
    #[cfg(feature = "pipeline-wasm")]
    pub fn from_wasm_inlined(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        Self::wrap(volar_ir_build::Pipeline::from_wasm_inlined(&path), [path])
    }

    /// Structural LLVM import from `.ll`, `.bc`, or a clang full-LTO
    /// static library (`.a` / `.lib`). Calls are preserved until a later pass.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_llvm(path: impl Into<PathBuf>, entries: &[&str]) -> Self {
        let path = path.into();
        Self::wrap(volar_ir_build::Pipeline::from_llvm(&path, entries), [path])
    }

    /// Structural LLVM import plus VAFFLE inline-everything.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_llvm_inlined(path: impl Into<PathBuf>, entries: &[&str]) -> Self {
        let path = path.into();
        Self::wrap(
            volar_ir_build::Pipeline::from_llvm_inlined(&path, entries),
            [path],
        )
    }

    /// Execution-mode LLVM-direct import (already `is_circuit()` when it succeeds).
    /// Accepts `.ll`, `.bc`, or an LTO static library.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_llvm_direct(path: impl Into<PathBuf>, entry: &str) -> Self {
        let path = path.into();
        Self::wrap(
            volar_ir_build::Pipeline::from_llvm_direct(&path, entry),
            [path],
        )
    }

    /// Compile `build` to a clang full-LTO static library, then import
    /// structurally. Emits `cargo:rerun-if-changed` for each source on
    /// `build`. GCC LTO is rejected.
    #[cfg(feature = "pipeline-cc")]
    pub fn from_cc(build: cc::Build, lib_name: &str, entries: &[&str]) -> Self {
        let rerun: Vec<PathBuf> = build.get_files().map(Path::to_path_buf).collect();
        Self::wrap(
            volar_ir_build::Pipeline::from_cc(build, lib_name, entries),
            rerun,
        )
    }

    /// [`Pipeline::from_cc`] plus VAFFLE inline-everything.
    #[cfg(feature = "pipeline-cc")]
    pub fn from_cc_inlined(build: cc::Build, lib_name: &str, entries: &[&str]) -> Self {
        let rerun: Vec<PathBuf> = build.get_files().map(Path::to_path_buf).collect();
        Self::wrap(
            volar_ir_build::Pipeline::from_cc_inlined(build, lib_name, entries),
            rerun,
        )
    }

    /// [`Pipeline::from_cc`] then the execution-mode importer.
    #[cfg(feature = "pipeline-cc")]
    pub fn from_cc_direct(build: cc::Build, lib_name: &str, entry: &str) -> Self {
        let rerun: Vec<PathBuf> = build.get_files().map(Path::to_path_buf).collect();
        Self::wrap(
            volar_ir_build::Pipeline::from_cc_direct(build, lib_name, entry),
            rerun,
        )
    }

    /// Run `cmd` (no shell) to produce an LTO static library, then import
    /// structurally. The command's inputs are not known here — emit
    /// `cargo:rerun-if-changed` in the calling `build.rs` if needed.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_command(cmd: volar_ir_build::CommandBuild, entries: &[&str]) -> Self {
        Self::wrap(volar_ir_build::Pipeline::from_command(cmd, entries), [])
    }

    /// [`Pipeline::from_command`] plus VAFFLE inline-everything.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_command_inlined(cmd: volar_ir_build::CommandBuild, entries: &[&str]) -> Self {
        Self::wrap(
            volar_ir_build::Pipeline::from_command_inlined(cmd, entries),
            [],
        )
    }

    /// [`Pipeline::from_command`] then the execution-mode importer.
    #[cfg(feature = "pipeline-llvm")]
    pub fn from_command_direct(cmd: volar_ir_build::CommandBuild, entry: &str) -> Self {
        Self::wrap(
            volar_ir_build::Pipeline::from_command_direct(cmd, entry),
            [],
        )
    }

    /// Configure oracle/action import mappings for WASM pipelines.
    #[cfg(feature = "pipeline-wasm")]
    pub fn with_import_config(mut self, config: volar_ir_build::WaffleImportConfig) -> Self {
        self.inner = self.inner.with_import_config(config);
        self
    }

    /// Names used as roots for VAFFLE inline-everything.
    pub fn with_inline_entries(mut self, entries: &[&str]) -> Self {
        self.inner = self.inner.with_inline_entries(entries);
        self
    }

    /// Inline every non-recursive intra-module VAFFLE call.
    #[cfg(feature = "pipeline-vaffle")]
    pub fn inline_vaffle_everything(mut self) -> Self {
        self.inner = self.inner.inline_vaffle_everything();
        self
    }

    /// Lower VAFFLE → Volar IR.
    #[cfg(feature = "pipeline-vaffle")]
    pub fn lower_to_volar_ir(mut self) -> Self {
        self.inner = self.inner.lower_to_volar_ir();
        self
    }

    /// Constant-fold Volar IR until stable.
    pub fn fold_ir(mut self) -> Self {
        self.inner = self.inner.fold_ir();
        self
    }

    /// Movfuscate Volar IR into a single self-looping block.
    pub fn movfuscate(mut self) -> Self {
        self.inner = self.inner.movfuscate();
        self
    }

    /// Unroll Volar IR into a combinational circuit (concrete CF required).
    pub fn unroll_ir(mut self) -> Self {
        self.inner = self.inner.unroll_ir();
        self
    }

    /// Execute all passes and return the resulting Volar IR.
    #[cfg(feature = "pipeline")]
    pub fn to_volar_ir(self) -> Result<(IRBlocks, IRTypes), Box<dyn std::error::Error>> {
        self.emit_rerun();
        self.inner.to_volar_ir()
    }

    /// Execute all passes and return a saved LIR module.
    #[cfg(feature = "pipeline")]
    pub fn to_lir(self) -> Result<SavedLirModule, Box<dyn std::error::Error>> {
        self.emit_rerun();
        self.inner.to_lir()
    }

    /// Execute all passes and compile the result to a native object file.
    pub fn compile_to_object(
        self,
        out_path: &Path,
        options: &CompileOptions,
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.emit_rerun();
        let saved = self.inner.to_lir()?;
        lir_to_object(&saved, out_path, options)
    }

    /// Execute all passes and emit woven Rust source.
    #[cfg(feature = "weave-rust")]
    pub fn emit_woven_rust(
        self,
        out_path: &Path,
        weaver: &crate::Weaver,
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.emit_rerun();
        let (blocks, types) = self.inner.to_volar_ir()?;
        weave_volar_ir_in_memory(&blocks, &types, out_path, weaver)
    }

    /// Execute all passes and emit chunked Rust source files into `out_dir/`.
    #[cfg(feature = "weave-chunked")]
    pub fn emit_woven_rust_chunked(
        self,
        out_dir: &Path,
        weaver: &crate::Weaver,
        options: &volar_compiler::chunk_module::ChunkOptions,
    ) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
        self.emit_rerun();
        let (blocks, types) = self.inner.to_volar_ir()?;
        weave_volar_ir_chunked(&blocks, &types, out_dir, weaver, options)
    }

    /// Execute all passes and emit chunked TypeScript source files into `out_dir/`.
    #[cfg(feature = "weave-ts")]
    pub fn emit_woven_typescript_chunked(
        self,
        out_dir: &Path,
        weaver: &crate::Weaver,
        options: &volar_compiler::chunk_module::ChunkOptions,
    ) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
        self.emit_rerun();
        let (blocks, types) = self.inner.to_volar_ir()?;
        let module = weave_volar_ir_to_ir_module(&blocks, &types, weaver)?;
        volar_compiler_passes::emit_woven_ts_chunked(&module, out_dir, options)
    }
}

fn load_saved_circuit(path: &Path) -> Result<(IRBlocks, IRTypes), Box<dyn std::error::Error>> {
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
) -> Result<(), Box<dyn std::error::Error>> {
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
) -> Result<(), Box<dyn std::error::Error>> {
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
) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
    use volar_compiler::chunk_module::{chunk_module_rust, ChunkConfig};
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
) -> Result<volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>, Box<dyn std::error::Error>>
{
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
