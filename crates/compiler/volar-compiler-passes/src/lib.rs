//! # Volar Compiler Passes
//!
//! Analysis and transformation passes over the `volar-compiler` IR.

#[cfg(feature = "std")]
extern crate std;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(feature = "std")]
use std::string::String;

pub mod chunk_fns;
pub mod const_analysis;
pub mod dump_ir;
pub mod lowering;
pub mod lowering_dyn;
pub mod unpack_packed_bits;

pub use chunk_fns::chunk_function_bodies;
pub use const_analysis::*;
pub use lowering::*;
pub use unpack_packed_bits::unpack_bits_in_type;
pub use volar_compiler::chunk_module::ChunkOptions;

/// Generate dynamic Rust code by lowering type-level lengths to runtime witnesses.
pub fn print_module_rust_dyn(module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer::print_module(&lowered)
}

/// Generate dynamic Rust code with dependency manifests providing context.
pub fn print_module_rust_dyn_with_deps(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    deps: &[volar_compiler::manifest::TypeManifest],
) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer::print_module_with_deps(&lowered, deps)
}

/// Generate TypeScript code by lowering type-level lengths to runtime witnesses.
pub fn print_module_typescript(module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer_ts::print_module_ts(&lowered)
}

/// Generate TypeScript code for only the functions transitively reachable from
/// `seeds` (and their dependencies).  Types are always emitted in full.
pub fn print_module_typescript_seeded(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    seeds: &[&str],
) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer_ts::print_module_ts_seeded(&lowered, seeds)
}

/// Generate TypeScript code with dependency manifests providing context.
pub fn print_module_typescript_with_deps(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    deps: &[volar_compiler::manifest::TypeManifest],
) -> String {
    let _ = deps;
    let lowered = lowering_dyn::lower_module_dyn(module);
    volar_compiler::printer_ts::print_module_ts(&lowered)
}

/// Weave a circuit and emit chunked TypeScript files to `out_dir/`.
///
/// Produces `out_dir/index.ts` + `out_dir/chunk_0.ts`, etc.
/// Applies fn-body chunking first if `options.fn_chunk_max_stmts` is set.
#[cfg(feature = "std")]
pub fn emit_woven_ts_chunked(
    module: &volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>,
    out_dir: &std::path::Path,
    options: &ChunkOptions,
) -> Result<std::vec::Vec<std::path::PathBuf>, Box<dyn std::error::Error>> {
    use volar_compiler::chunk_module::{ChunkConfig, chunk_module_ts};

    let chunked_module;
    let effective = if let Some(threshold) = options.fn_chunk_max_stmts {
        chunked_module = chunk_function_bodies(module, threshold);
        &chunked_module
    } else {
        module
    };

    let items_per_chunk = options.module_items_per_chunk.unwrap_or(usize::MAX);
    let output = chunk_module_ts(effective, &ChunkConfig { items_per_chunk }, &[]);

    std::fs::create_dir_all(out_dir)?;

    let mut written: std::vec::Vec<std::path::PathBuf> = std::vec::Vec::new();

    let index_path = out_dir.join("index.ts");
    std::fs::write(&index_path, &output.wrapper)?;
    written.push(index_path);

    for (i, src) in output.chunks.iter().enumerate() {
        let chunk_path = out_dir.join(format!("chunk_{i}.ts"));
        std::fs::write(&chunk_path, src)?;
        written.push(chunk_path);
    }

    Ok(written)
}
