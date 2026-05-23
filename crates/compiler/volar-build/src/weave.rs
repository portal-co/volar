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
    /// Network VOLE **prover** (Boolar IR).  Requires the `weave-net` feature.
    #[cfg(feature = "weave-net")]
    NetVoleProver { name: String },
    /// Network VOLE **verifier** (Boolar IR).  Requires the `weave-net` feature.
    #[cfg(feature = "weave-net")]
    NetVoleVerifier { name: String },
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
