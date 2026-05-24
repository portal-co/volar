// @reliability: experimental
// @ai: assisted
//! Builder-style pipeline for compiling from any IR level to object code or
//! woven Rust.

use std::path::{Path, PathBuf};

use volar_ir::ir::{IRBlocks, IRTypes};
use volar_lir_saved::{RecordingTarget, SavedLirModule};

use crate::{CompileOptions, SavedCircuit};

// ============================================================================
// Internal source-stage discriminant (paths only — data loaded in execute())
// ============================================================================

enum SourceStage {
    Lir(PathBuf),
    VolarIr(PathBuf),
    #[cfg(feature = "pipeline-vaffle")]
    Vaffle(PathBuf),
    #[cfg(feature = "pipeline-wasm")]
    Wasm(PathBuf),
}

// ============================================================================
// PipelinePass
// ============================================================================

/// A pass applied to the IR during [`Pipeline`] execution.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum PipelinePass {
    /// Lower a VAFFLE module to Volar IR.  Source must be Vaffle or Wasm.
    #[cfg(feature = "pipeline-vaffle")]
    LowerToVolarIr,
    /// Constant-fold Volar IR until stable.  Source must be VolarIr.
    FoldIr,
    /// Movfuscate Volar IR into a single self-looping block.  Source must be VolarIr.
    Movfuscate,
}

// ============================================================================
// Pipeline
// ============================================================================

/// A composable lowering pipeline for `build.rs` scripts.
///
/// Start from a file on disk, layer optional IR passes, then terminate into
/// an object file or woven Rust source.
///
/// # Example
/// ```rust,ignore
/// Pipeline::from_volar_ir("src/my.circuit")
///     .fold_ir()
///     .movfuscate()
///     .compile_to_object(&out.join("my.o"), &CompileOptions::default())?;
/// ```
pub struct Pipeline {
    source: SourceStage,
    passes: Vec<PipelinePass>,
}

// ---- Constructors -----------------------------------------------------------

impl Pipeline {
    /// Start from a pre-recorded `.lir` file.
    pub fn from_saved_lir(path: impl Into<PathBuf>) -> Self {
        Pipeline { source: SourceStage::Lir(path.into()), passes: vec![] }
    }

    /// Start from a `.circuit` file (rkyv-serialized `IRBlocks + IRTypes`).
    pub fn from_volar_ir(path: impl Into<PathBuf>) -> Self {
        Pipeline { source: SourceStage::VolarIr(path.into()), passes: vec![] }
    }

    /// Start from a `.vaffle` file (rkyv-serialized VAFFLE `Module`).
    #[cfg(feature = "pipeline-vaffle")]
    pub fn from_vaffle(path: impl Into<PathBuf>) -> Self {
        Pipeline { source: SourceStage::Vaffle(path.into()), passes: vec![] }
    }

    /// Start from a `.wasm` file; WAFFLE parsing happens in-memory at execution time.
    #[cfg(feature = "pipeline-wasm")]
    pub fn from_wasm(path: impl Into<PathBuf>) -> Self {
        Pipeline { source: SourceStage::Wasm(path.into()), passes: vec![] }
    }
}

// ---- Pass builder methods ---------------------------------------------------

impl Pipeline {
    /// Lower VAFFLE → Volar IR.
    #[cfg(feature = "pipeline-vaffle")]
    pub fn lower_to_volar_ir(mut self) -> Self {
        self.passes.push(PipelinePass::LowerToVolarIr);
        self
    }

    /// Constant-fold Volar IR until stable.
    pub fn fold_ir(mut self) -> Self {
        self.passes.push(PipelinePass::FoldIr);
        self
    }

    /// Movfuscate Volar IR into a single self-looping block.
    pub fn movfuscate(mut self) -> Self {
        self.passes.push(PipelinePass::Movfuscate);
        self
    }
}

// ---- Terminal methods -------------------------------------------------------

impl Pipeline {
    /// Execute all passes and compile the result to a native object file.
    pub fn compile_to_object(
        self,
        out_path: &Path,
        options: &CompileOptions,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let executed = self.execute()?;
        match executed {
            ExecutedPipeline::Lir(saved) => lir_to_object(&saved, out_path, options),
            ExecutedPipeline::VolarIr(blocks, types) => {
                let saved = lower_volar_ir_to_lir(&blocks, &types);
                lir_to_object(&saved, out_path, options)
            }
        }
    }

    /// Execute all passes and emit woven Rust source.
    #[cfg(feature = "weave-rust")]
    pub fn emit_woven_rust(
        self,
        out_path: &Path,
        weaver: &crate::Weaver,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let executed = self.execute()?;
        match executed {
            ExecutedPipeline::VolarIr(blocks, types) => {
                weave_volar_ir_in_memory(&blocks, &types, out_path, weaver)
            }
            ExecutedPipeline::Lir(_) => Err(
                "emit_woven_rust requires VolarIr stage; got Lir (add passes or start from volar IR)".into()
            ),
        }
    }

    /// Execute all passes and emit chunked Rust source files into `out_dir/`.
    ///
    /// Only supports VolarIr-stage weavers (`VoleProverIr`, `VoleVerifierIr`).
    /// Returns a list of written file paths.
    #[cfg(feature = "weave-chunked")]
    pub fn emit_woven_rust_chunked(
        self,
        out_dir: &Path,
        weaver: &crate::Weaver,
        options: &volar_compiler::chunk_module::ChunkOptions,
    ) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
        let executed = self.execute()?;
        match executed {
            ExecutedPipeline::VolarIr(blocks, types) => {
                weave_volar_ir_chunked(&blocks, &types, out_dir, weaver, options)
            }
            ExecutedPipeline::Lir(_) => Err(
                "emit_woven_rust_chunked requires VolarIr stage; got Lir".into()
            ),
        }
    }
}

// ============================================================================
// Executed pipeline result
// ============================================================================

enum ExecutedPipeline {
    Lir(SavedLirModule),
    VolarIr(IRBlocks, IRTypes),
}

// ============================================================================
// execute() — load source + run passes
// ============================================================================

impl Pipeline {
    fn execute(self) -> Result<ExecutedPipeline, Box<dyn std::error::Error>> {
        let source_path: Option<&Path> = match &self.source {
            SourceStage::Lir(p) => Some(p),
            SourceStage::VolarIr(p) => Some(p),
            #[cfg(feature = "pipeline-vaffle")]
            SourceStage::Vaffle(p) => Some(p),
            #[cfg(feature = "pipeline-wasm")]
            SourceStage::Wasm(p) => Some(p),
        };
        if let Some(p) = source_path {
            println!("cargo:rerun-if-changed={}", p.display());
        }

        // Load into the internal runtime stage.
        let mut stage = load_source(self.source)?;

        // Run passes.
        for pass in self.passes {
            stage = apply_pass(pass, stage)?;
        }

        // Convert to ExecutedPipeline.
        match stage {
            RuntimeStage::Lir(saved) => Ok(ExecutedPipeline::Lir(saved)),
            RuntimeStage::VolarIr(blocks, types) => Ok(ExecutedPipeline::VolarIr(blocks, types)),
            #[cfg(feature = "pipeline-vaffle")]
            RuntimeStage::Vaffle(_) => Err(
                "pipeline terminated at Vaffle stage — add .lower_to_volar_ir() before the terminal method".into()
            ),
        }
    }
}

// ============================================================================
// RuntimeStage — in-memory IR during pass execution
// ============================================================================

enum RuntimeStage {
    Lir(SavedLirModule),
    VolarIr(IRBlocks, IRTypes),
    #[cfg(feature = "pipeline-vaffle")]
    Vaffle(vaffle::Module),
}

// ============================================================================
// load_source
// ============================================================================

fn load_source(source: SourceStage) -> Result<RuntimeStage, Box<dyn std::error::Error>> {
    match source {
        SourceStage::Lir(path) => {
            let bytes = std::fs::read(&path)?;
            let saved = rkyv::from_bytes::<SavedLirModule, rkyv::rancor::Error>(&bytes)?;
            Ok(RuntimeStage::Lir(saved))
        }
        SourceStage::VolarIr(path) => {
            let bytes = std::fs::read(&path)?;
            let circuit = rkyv::from_bytes::<SavedCircuit, rkyv::rancor::Error>(&bytes)
                .map_err(|e| format!("failed to deserialize .circuit file: {e}"))?;
            match circuit {
                SavedCircuit::Volar(blocks, types) => Ok(RuntimeStage::VolarIr(blocks, types)),
                _ => Err("expected a Volar circuit file, got Boolar".into()),
            }
        }
        #[cfg(feature = "pipeline-vaffle")]
        SourceStage::Vaffle(path) => {
            let bytes = std::fs::read(&path)?;
            let module = rkyv::from_bytes::<vaffle::Module, rkyv::rancor::Error>(&bytes)
                .map_err(|e| format!("failed to deserialize .vaffle file: {e}"))?;
            Ok(RuntimeStage::Vaffle(module))
        }
        #[cfg(feature = "pipeline-wasm")]
        SourceStage::Wasm(path) => {
            let bytes = std::fs::read(&path)?;
            let waffle_module =
                portal_pc_waffle_frontend::from_wasm_bytes(&bytes, &portal_pc_waffle_frontend::FrontendOptions::default())
                    .map_err(|e| format!("WAFFLE parse failed: {e}"))?;
            let mut target = volar_vaffle_target::VaffleTarget::new();
            volar_vaffle_target::lower_waffle_module(&waffle_module, &mut target);
            Ok(RuntimeStage::Vaffle(target.module))
        }
    }
}

// ============================================================================
// apply_pass
// ============================================================================

fn apply_pass(
    pass: PipelinePass,
    stage: RuntimeStage,
) -> Result<RuntimeStage, Box<dyn std::error::Error>> {
    match pass {
        #[cfg(feature = "pipeline-vaffle")]
        PipelinePass::LowerToVolarIr => match stage {
            RuntimeStage::Vaffle(module) => {
                let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir(&module);
                Ok(RuntimeStage::VolarIr(blocks, types))
            }
            _ => Err("LowerToVolarIr pass requires Vaffle stage".into()),
        },
        PipelinePass::FoldIr => match stage {
            RuntimeStage::VolarIr(mut blocks, types) => {
                while volar_ir_opt::ir::fold_ir_blocks(&mut blocks, &types) {}
                Ok(RuntimeStage::VolarIr(blocks, types))
            }
            _ => Err("FoldIr pass requires VolarIr stage".into()),
        },
        PipelinePass::Movfuscate => match stage {
            RuntimeStage::VolarIr(blocks, mut types) => {
                let blocks = volar_ir_passes::movfuscate_ir(&blocks, &mut types);
                Ok(RuntimeStage::VolarIr(blocks, types))
            }
            _ => Err("Movfuscate pass requires VolarIr stage".into()),
        },
    }
}

// ============================================================================
// Private helpers
// ============================================================================

fn lower_volar_ir_to_lir(blocks: &IRBlocks, types: &IRTypes) -> SavedLirModule {
    let mut rec = RecordingTarget::new();
    volar_ir_passes::lower_lir::lower_ir(blocks, types, "volar_module", &mut rec);
    rec.finish()
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
            CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
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
    let mut backend = LlvmBackend::new(&context, module_name)
        .with_name_config(options.name_config.clone());
    saved.replay(&mut backend);
    let module = backend.finish();

    let cargo_target = std::env::var("TARGET").ok();
    let cargo_host = std::env::var("HOST").ok();
    let explicit_triple = options.target_triple.as_deref();

    let resolved_triple_str: Option<String> = explicit_triple
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
                .unwrap_or_else(|| TargetMachine::get_host_cpu_name().to_string_lossy().into_owned());
            let features = options
                .features
                .as_deref()
                .map(str::to_owned)
                .unwrap_or_else(|| {
                    TargetMachine::get_host_cpu_features().to_string_lossy().into_owned()
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

    let target = Target::from_triple(&triple)
        .map_err(|e| format!("LLVM target from triple: {e}"))?;
    let target_machine = target
        .create_target_machine(&triple, &cpu_str, &features_str, opt_level, RelocMode::Default, CodeModel::Default)
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
            let module = volar_weaver::weave_vole_prover_ir(blocks, types, name, storage_sizes, None);
            volar_weaver::print_weaved_vole_module(&module)
        }
        Weaver::VoleVerifierIr { name, storage_sizes } => {
            let module = volar_weaver::weave_vole_verifier_ir(blocks, types, name, storage_sizes, None);
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

// ============================================================================
// weave_volar_ir_chunked
// ============================================================================

#[cfg(feature = "weave-chunked")]
fn weave_volar_ir_chunked(
    blocks: &IRBlocks,
    types: &IRTypes,
    out_dir: &Path,
    weaver: &crate::Weaver,
    options: &volar_compiler::chunk_module::ChunkOptions,
) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
    use volar_compiler::chunk_module::{ChunkConfig, chunk_module_rust};
    use volar_compiler_passes::chunk_function_bodies;

    let module = match weaver {
        crate::Weaver::VoleProverIr { name, storage_sizes } => {
            volar_weaver::weave_vole_prover_ir(blocks, types, name, storage_sizes, None)
        }
        crate::Weaver::VoleVerifierIr { name, storage_sizes } => {
            volar_weaver::weave_vole_verifier_ir(blocks, types, name, storage_sizes, None)
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

// ============================================================================
// serialize_vaffle_module
// ============================================================================

/// Serialize a VAFFLE [`Module`](vaffle::Module) to bytes for use as a `.vaffle`
/// file with [`Pipeline::from_vaffle`].
#[cfg(feature = "pipeline-vaffle")]
pub fn serialize_vaffle_module(
    module: &vaffle::Module,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    Ok(rkyv::to_bytes::<rkyv::rancor::Error>(module)?.into_vec())
}
