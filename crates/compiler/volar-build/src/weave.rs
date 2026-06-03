// @reliability: experimental
// @ai: assisted
//! Woven Rust source emission for `build.rs` scripts.
//!
//! Enabled by the `weave-rust` Cargo feature.  Provides:
//!
//! - [`SavedCircuit`] — on-disk format for circuit IR (Boolar or Volar).
//! - [`Weaver`] — `#[non_exhaustive]` enum selecting the weaving protocol.
//! - [`emit_woven_rust`] — weave a saved circuit and write Rust source to disk.
//! - [`serialize_boolar_circuit`] / [`serialize_volar_circuit`] — produce
//!   `.circuit` files for use with [`emit_woven_rust`].

use std::path::Path;

use volar_ir::boolar::BIrBlocks;
use volar_ir::ir::{IRBlocks, IRTypes};

pub use crate::SavedCircuit;

/// Serialize a [`BIrBlocks`] circuit for later use with [`emit_woven_rust`].
pub fn serialize_boolar_circuit(
    circuit: &BIrBlocks,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let saved = SavedCircuit::Boolar(circuit.clone());
    let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(&saved)?;
    Ok(bytes.into_vec())
}

/// Serialize an [`IRBlocks`] + [`IRTypes`] circuit for later use with [`emit_woven_rust`].
pub fn serialize_volar_circuit(
    circuit: &IRBlocks,
    types: &IRTypes,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let saved = SavedCircuit::Volar(circuit.clone(), types.clone());
    let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(&saved)?;
    Ok(bytes.into_vec())
}

// ============================================================================
// Weaver
// ============================================================================

/// Selects the weaving protocol applied by [`emit_woven_rust`].
///
/// The enum is `#[non_exhaustive]` — new variants may be added in future
/// versions or unlocked by Cargo feature flags (e.g. `weave-net`).
///
/// All Boolar-IR variants require the circuit to satisfy
/// [`BIrBlocks::is_circuit()`]; Volar-IR variants require
/// [`IRBlocks::is_circuit()`].
#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Weaver {
    /// Garbled-circuit **evaluator** (Boolar IR).
    GarbleEvaluator { name: String },
    /// Garbled-circuit **garbler** (Boolar IR).
    GarbleGarbler { name: String },
    /// VOLE ZK **prover** (Boolar IR).
    VoleProver { name: String },
    /// VOLE ZK **verifier** (Boolar IR).
    VoleVerifier { name: String },
    /// VOLE ZK **prover** using field-level Volar IR.
    VoleProverIr { name: String, storage_sizes: volar_weaver::StorageSizes },
    /// VOLE ZK **verifier** using field-level Volar IR.
    VoleVerifierIr { name: String, storage_sizes: volar_weaver::StorageSizes },
    /// Cleartext (non-ZK) **no-op** evaluator (Boolar IR).
    NoOp { name: String },
    /// Cleartext (non-ZK) **no-op** evaluator using field-level Volar IR.
    NoOpIr { name: String },
    /// Network VOLE **prover** (Boolar IR).  Requires the `weave-net` feature.
    #[cfg(feature = "weave-net")]
    NetVoleProver { name: String },
    /// Network VOLE **verifier** (Boolar IR).  Requires the `weave-net` feature.
    #[cfg(feature = "weave-net")]
    NetVoleVerifier { name: String },
    /// Hybrid network-resilient VOLE **prover** (Boolar loop circuit → CFG).
    /// Reacts to the network cutting off; requires the `weave-net` feature.
    #[cfg(feature = "weave-net")]
    HybridNetVoleProver { name: String },
    /// Hybrid network-resilient VOLE **verifier** (Boolar loop circuit → CFG).
    /// Requires the `weave-net` feature.
    #[cfg(feature = "weave-net")]
    HybridNetVoleVerifier { name: String },
}

// ============================================================================
// emit_woven_rust
// ============================================================================

/// Weave a saved circuit and write the resulting Rust source to `out_path`.
///
/// Also emits `cargo:rerun-if-changed=<circuit_path>` so Cargo re-runs the
/// build script when the circuit file changes.
///
/// # Arguments
///
/// - `circuit_path` — path to a `.circuit` file produced by
///   [`serialize_boolar_circuit`] or [`serialize_volar_circuit`].
/// - `out_path` — destination for the emitted Rust source (`.rs`) file.
///   Typically inside `$OUT_DIR`.
/// - `weaver` — protocol to apply; must match the circuit variant
///   (`Boolar*` weavers require a [`SavedCircuit::Boolar`] file, etc.).
///
/// # Errors
///
/// Returns an error on I/O failure, deserialization failure, or
/// weaver/circuit variant mismatch.
pub fn emit_woven_rust(
    circuit_path: &Path,
    out_path: &Path,
    weaver: &Weaver,
) -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "cargo-directives")]
    println!("cargo:rerun-if-changed={}", circuit_path.display());

    let bytes = std::fs::read(circuit_path)?;
    let circuit = rkyv::from_bytes::<SavedCircuit, rkyv::rancor::Error>(&bytes)?;

    let rust_source: String = match (weaver, circuit) {
        (Weaver::GarbleEvaluator { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_evaluator(&bir, name, None);
            volar_weaver::print_weaved_module(&module, false)
        }
        (Weaver::GarbleGarbler { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_garbler(&bir, name, None);
            volar_weaver::print_weaved_module(&module, false)
        }
        (Weaver::VoleProver { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_vole_prover(&bir, name, None);
            volar_weaver::print_weaved_vole_module(&module)
        }
        (Weaver::VoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_vole_verifier(&bir, name, None);
            volar_weaver::print_weaved_vole_module(&module)
        }
        (Weaver::VoleProverIr { name, storage_sizes }, SavedCircuit::Volar(ir, types)) => {
            let module = volar_weaver::weave_vole_prover_ir(&ir, &types, name, storage_sizes, None);
            volar_weaver::print_weaved_vole_module(&module)
        }
        (Weaver::VoleVerifierIr { name, storage_sizes }, SavedCircuit::Volar(ir, types)) => {
            let module = volar_weaver::weave_vole_verifier_ir(&ir, &types, name, storage_sizes, None);
            volar_weaver::print_weaved_vole_module(&module)
        }
        (Weaver::NoOp { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_noop(&bir, name, None);
            volar_weaver::print_noop_module(&module)
        }
        (Weaver::NoOpIr { name }, SavedCircuit::Volar(ir, types)) => {
            let module = volar_weaver::weave_noop_ir(&ir, &types, name, None);
            volar_weaver::print_noop_module(&module)
        }
        #[cfg(feature = "weave-net")]
        (Weaver::NetVoleProver { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_net_vole_prover(&bir, name, None);
            volar_weaver::print_weaved_module(&module, false)
        }
        #[cfg(feature = "weave-net")]
        (Weaver::NetVoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_net_vole_verifier(&bir, name, None);
            volar_weaver::print_weaved_module(&module, false)
        }
        #[cfg(feature = "weave-net")]
        (Weaver::HybridNetVoleProver { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_hybrid_net_vole_prover(&bir, name, None);
            volar_weaver::print_hybrid_net_cfg_module(&module)
        }
        #[cfg(feature = "weave-net")]
        (Weaver::HybridNetVoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
            let module = volar_weaver::weave_hybrid_net_vole_verifier(&bir, name, None);
            volar_weaver::print_hybrid_net_cfg_module(&module)
        }
        (w, c) => {
            let circuit_kind = match &c {
                SavedCircuit::Boolar(_) => "Boolar",
                SavedCircuit::Volar(..) => "Volar",
                _ => "unknown",
            };
            return Err(format!(
                "weaver/circuit mismatch: {:?} cannot process {} circuit",
                w, circuit_kind
            )
            .into());
        }
    };

    std::fs::write(out_path, rust_source)?;
    Ok(())
}

// ============================================================================
// emit_woven_typescript
// ============================================================================

/// Weave a saved circuit and write the resulting TypeScript source to `out_path`.
///
/// Emits `cargo:rerun-if-changed=<circuit_path>` when the `cargo-directives`
/// feature is enabled (the default).
///
/// # Arguments
///
/// - `circuit_path` — path to a `.circuit` file (Boolar or Volar).
/// - `out_path` — destination for the emitted TypeScript (`.ts`) file.
/// - `weaver` — protocol to apply; must match the circuit variant.
#[cfg(feature = "weave-ts")]
pub fn emit_woven_typescript(
    circuit_path: &Path,
    out_path: &Path,
    weaver: &Weaver,
) -> Result<(), Box<dyn std::error::Error>> {
    use volar_compiler::chunk_module::{ChunkConfig, chunk_module_ts};

    #[cfg(feature = "cargo-directives")]
    println!("cargo:rerun-if-changed={}", circuit_path.display());

    let bytes = std::fs::read(circuit_path)?;
    let circuit = rkyv::from_bytes::<SavedCircuit, rkyv::rancor::Error>(&bytes)?;

    let module = weave_to_ir_module(weaver, circuit)?;

    let output = chunk_module_ts(&module, &ChunkConfig { items_per_chunk: usize::MAX }, &[]);
    let ts_source = output.chunks.into_iter().next().unwrap_or_default();
    std::fs::write(out_path, ts_source)?;
    Ok(())
}

// ============================================================================
// emit_woven_typescript_chunked
// ============================================================================

/// Weave a saved circuit and write chunked TypeScript source files into `out_dir/`.
///
/// Creates `out_dir/index.ts` (wrapper) and `out_dir/chunk_0.ts`, ... chunk files.
///
/// Emits `cargo:rerun-if-changed=<circuit_path>` when the `cargo-directives`
/// feature is enabled (the default).
///
/// # Returns
///
/// A list of written file paths in emission order (index.ts first, then chunks).
#[cfg(feature = "weave-ts")]
pub fn emit_woven_typescript_chunked(
    circuit_path: &Path,
    out_dir: &Path,
    weaver: &Weaver,
    options: &volar_compiler::chunk_module::ChunkOptions,
) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
    #[cfg(feature = "cargo-directives")]
    println!("cargo:rerun-if-changed={}", circuit_path.display());

    let bytes = std::fs::read(circuit_path)?;
    let circuit = rkyv::from_bytes::<SavedCircuit, rkyv::rancor::Error>(&bytes)?;

    let module = weave_to_ir_module(weaver, circuit)?;
    volar_compiler_passes::emit_woven_ts_chunked(&module, out_dir, options)
}

// ============================================================================
// weave_to_ir_module — shared helper for TS emit functions
// ============================================================================

#[cfg(feature = "weave-ts")]
fn weave_to_ir_module(
    weaver: &Weaver,
    circuit: SavedCircuit,
) -> Result<volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>, Box<dyn std::error::Error>> {
    let module = match (weaver, circuit) {
        (Weaver::GarbleEvaluator { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_evaluator(&bir, name, None)
        }
        (Weaver::GarbleGarbler { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_garbler(&bir, name, None)
        }
        (Weaver::VoleProver { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_vole_prover(&bir, name, None)
        }
        (Weaver::VoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_vole_verifier(&bir, name, None)
        }
        (Weaver::VoleProverIr { name, storage_sizes }, SavedCircuit::Volar(ir, types)) => {
            volar_weaver::weave_vole_prover_ir(&ir, &types, name, storage_sizes, None)
        }
        (Weaver::VoleVerifierIr { name, storage_sizes }, SavedCircuit::Volar(ir, types)) => {
            volar_weaver::weave_vole_verifier_ir(&ir, &types, name, storage_sizes, None)
        }
        (Weaver::NoOp { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_noop(&bir, name, None)
        }
        (Weaver::NoOpIr { name }, SavedCircuit::Volar(ir, types)) => {
            volar_weaver::weave_noop_ir(&ir, &types, name, None)
        }
        #[cfg(feature = "weave-net")]
        (Weaver::NetVoleProver { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_net_vole_prover(&bir, name, None)
        }
        #[cfg(feature = "weave-net")]
        (Weaver::NetVoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
            volar_weaver::weave_net_vole_verifier(&bir, name, None)
        }
        (w, c) => {
            let circuit_kind = match &c {
                SavedCircuit::Boolar(_) => "Boolar",
                SavedCircuit::Volar(..) => "Volar",
                _ => "unknown",
            };
            return Err(format!(
                "weaver/circuit mismatch: {:?} cannot process {} circuit",
                w, circuit_kind
            ).into());
        }
    };
    Ok(module)
}

// ============================================================================
// emit_woven_rust_chunked
// ============================================================================

/// Weave a saved circuit and write chunked Rust source files into `out_dir/`.
///
/// Creates `out_dir/mod.rs` (wrapper) and `out_dir/chunk_0.rs`, ... chunk files.
///
/// Also emits `cargo:rerun-if-changed=<circuit_path>`.
///
/// # Returns
///
/// A list of written file paths in emission order (mod.rs first, then chunks).
#[cfg(feature = "weave-chunked")]
pub fn emit_woven_rust_chunked(
    circuit_path: &Path,
    out_dir: &Path,
    weaver: &Weaver,
    options: &volar_compiler::chunk_module::ChunkOptions,
) -> Result<Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
    use volar_compiler::chunk_module::{ChunkConfig, chunk_module_rust};
    use volar_compiler_passes::chunk_function_bodies;

    #[cfg(feature = "cargo-directives")]
    println!("cargo:rerun-if-changed={}", circuit_path.display());

    let bytes = std::fs::read(circuit_path)?;
    let circuit = rkyv::from_bytes::<SavedCircuit, rkyv::rancor::Error>(&bytes)?;

    let module: volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction> =
        match (weaver, circuit) {
            (Weaver::GarbleEvaluator { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_evaluator(&bir, name, None)
            }
            (Weaver::GarbleGarbler { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_garbler(&bir, name, None)
            }
            (Weaver::VoleProver { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_vole_prover(&bir, name, None)
            }
            (Weaver::VoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_vole_verifier(&bir, name, None)
            }
            (Weaver::VoleProverIr { name, storage_sizes }, SavedCircuit::Volar(ir, types)) => {
                volar_weaver::weave_vole_prover_ir(&ir, &types, name, storage_sizes, None)
            }
            (Weaver::VoleVerifierIr { name, storage_sizes }, SavedCircuit::Volar(ir, types)) => {
                volar_weaver::weave_vole_verifier_ir(&ir, &types, name, storage_sizes, None)
            }
            (Weaver::NoOp { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_noop(&bir, name, None)
            }
            (Weaver::NoOpIr { name }, SavedCircuit::Volar(ir, types)) => {
                volar_weaver::weave_noop_ir(&ir, &types, name, None)
            }
            #[cfg(feature = "weave-net")]
            (Weaver::NetVoleProver { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_net_vole_prover(&bir, name, None)
            }
            #[cfg(feature = "weave-net")]
            (Weaver::NetVoleVerifier { name }, SavedCircuit::Boolar(bir)) => {
                volar_weaver::weave_net_vole_verifier(&bir, name, None)
            }
            (w, c) => {
                let circuit_kind = match &c {
                    SavedCircuit::Boolar(_) => "Boolar",
                    SavedCircuit::Volar(..) => "Volar",
                    _ => "unknown",
                };
                return Err(format!(
                    "weaver/circuit mismatch: {:?} cannot process {} circuit",
                    w, circuit_kind
                ).into());
            }
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

    let mut written: Vec<std::path::PathBuf> = Vec::new();

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
