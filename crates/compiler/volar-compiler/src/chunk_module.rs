// @reliability: normal
//! Split an [`IrModule`] into multiple file-sized chunks plus a wrapper re-export module.
//!
//! # Usage
//!
//! ```rust,ignore
//! let output = chunk_module_rust(&module, &ChunkConfig { items_per_chunk: 200 }, &[]);
//! // write output.wrapper to mod.rs
//! for (i, src) in output.chunks.iter().enumerate() {
//!     // write src to chunk_{i}.rs
//! }
//! ```

#[cfg(feature = "std")]
use std::collections::BTreeSet;
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeSet;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::ir::{IrFunction, IrModule};
use crate::linkage::RemoteSpecRef;
use crate::multi_module::{collect_type_refs, topo_sort};
use crate::printer::print_module_with_remotes;
use crate::printer_ts::print_module_ts_with_imports;

// ============================================================================
// Public types
// ============================================================================

/// Backend-agnostic chunking options, shared by Rust and TypeScript emit paths.
#[derive(Debug, Clone, Default)]
pub struct ChunkOptions {
    /// Apply fn-body chunking via `chunk_function_bodies`; threshold = max stmts per function.
    /// `None` = skip fn-chunking pass.
    pub fn_chunk_max_stmts: Option<usize>,
    /// Max functions per chunk file.
    /// `None` = place all functions in a single chunk (folder layout still produced).
    pub module_items_per_chunk: Option<usize>,
}

/// Configuration for module-level file chunking.
pub struct ChunkConfig {
    /// Maximum number of functions per chunk file.
    /// Types (structs, enums, traits, type_aliases, consts) always go in chunk_0.
    pub items_per_chunk: usize,
}

/// Output of a chunked module split: a wrapper re-export file plus N chunk files.
pub struct ChunkedModuleOutput {
    /// Wrapper file content: `mod.rs` (Rust) or `index.ts` (TS).
    /// Contains only re-export directives — no item definitions.
    pub wrapper: String,
    /// Source strings for `chunk_0`, `chunk_1`, ... in order.
    pub chunks: Vec<String>,
}

// ============================================================================
// Rust chunking
// ============================================================================

/// Split `module` into file chunks and render each to Rust source.
///
/// Returns a [`ChunkedModuleOutput`] where:
/// - `wrapper` is the `mod.rs` content with `pub mod chunk_N; pub use chunk_N::*;` lines
/// - `chunks[i]` is the content of `chunk_i.rs`
pub fn chunk_module_rust(
    module: &IrModule<IrFunction>,
    config: &ChunkConfig,
    remotes: &[RemoteSpecRef<'_>],
) -> ChunkedModuleOutput {
    let chunk_modules = split_module(module, config.items_per_chunk);
    let n = chunk_modules.len();

    // Build per-chunk exported type-name sets.
    let exported: Vec<BTreeSet<String>> = chunk_modules.iter().map(|m| {
        let mut names = BTreeSet::new();
        for s in &m.structs { names.insert(s.kind.to_string()); }
        for t in &m.traits  { names.insert(t.kind.to_string()); }
        names
    }).collect();

    // Render each chunk with intra-chunk dep imports + external remotes.
    let chunks: Vec<String> = (0..n).map(|i| {
        // Collect intra-chunk deps: which prior chunks does chunk i reference?
        let refs = collect_type_refs(&chunk_modules[i]);
        let mut intra_remotes: Vec<OwnedRemote> = Vec::new();
        for j in 0..i {
            let names: Vec<String> = exported[j].iter()
                .filter(|name| refs.contains(*name))
                .cloned()
                .collect();
            if !names.is_empty() {
                intra_remotes.push(OwnedRemote {
                    rust_crate: format!("super::chunk_{j}"),
                    npm_package: None,
                    type_names: names,
                });
            }
        }

        let combined_remotes: Vec<RemoteSpecRef<'_>> = remotes.iter().map(|r| RemoteSpecRef {
            rust_crate: r.rust_crate,
            npm_package: r.npm_package,
            type_names: r.type_names.clone(),
        }).chain(intra_remotes.iter().map(|r| RemoteSpecRef {
            rust_crate: r.rust_crate.as_str(),
            npm_package: r.npm_package.as_deref(),
            type_names: r.type_names.clone(),
        })).collect();

        print_module_with_remotes(&chunk_modules[i], &[], &combined_remotes)
    }).collect();

    // Build wrapper (mod.rs).
    let wrapper = build_rust_wrapper(n);

    ChunkedModuleOutput { wrapper, chunks }
}

fn build_rust_wrapper(n: usize) -> String {
    let mut out = String::new();
    for i in 0..n {
        out.push_str(&format!("pub mod chunk_{i};\n"));
        out.push_str(&format!("pub use chunk_{i}::*;\n"));
    }
    out
}

// ============================================================================
// TypeScript chunking
// ============================================================================

/// Split `module` into file chunks and render each to TypeScript source.
///
/// Returns a [`ChunkedModuleOutput`] where:
/// - `wrapper` is the `index.ts` content with `export * from './chunk_N';` lines
/// - `chunks[i]` is the content of `chunk_i.ts`
pub fn chunk_module_ts(
    module: &IrModule<IrFunction>,
    config: &ChunkConfig,
    remotes: &[RemoteSpecRef<'_>],
) -> ChunkedModuleOutput {
    let chunk_modules = split_module(module, config.items_per_chunk);
    let n = chunk_modules.len();

    let exported: Vec<BTreeSet<String>> = chunk_modules.iter().map(|m| {
        let mut names = BTreeSet::new();
        for s in &m.structs { names.insert(s.kind.to_string()); }
        for t in &m.traits  { names.insert(t.kind.to_string()); }
        names
    }).collect();

    let chunks: Vec<String> = (0..n).map(|i| {
        let refs = collect_type_refs(&chunk_modules[i]);
        let mut intra_remotes: Vec<OwnedRemote> = Vec::new();
        for j in 0..i {
            let names: Vec<String> = exported[j].iter()
                .filter(|name| refs.contains(*name))
                .cloned()
                .collect();
            if !names.is_empty() {
                intra_remotes.push(OwnedRemote {
                    rust_crate: format!("./chunk_{j}"),
                    npm_package: None,
                    type_names: names,
                });
            }
        }

        let combined_remotes: Vec<RemoteSpecRef<'_>> = remotes.iter().map(|r| RemoteSpecRef {
            rust_crate: r.rust_crate,
            npm_package: r.npm_package,
            type_names: r.type_names.clone(),
        }).chain(intra_remotes.iter().map(|r| RemoteSpecRef {
            rust_crate: r.rust_crate.as_str(),
            npm_package: r.npm_package.as_deref(),
            type_names: r.type_names.clone(),
        })).collect();

        print_module_ts_with_imports(&chunk_modules[i], &combined_remotes)
    }).collect();

    let wrapper = build_ts_wrapper(n);

    ChunkedModuleOutput { wrapper, chunks }
}

fn build_ts_wrapper(n: usize) -> String {
    let mut out = String::new();
    for i in 0..n {
        out.push_str(&format!("export * from './chunk_{i}';\n"));
    }
    out
}

// ============================================================================
// Module splitting
// ============================================================================

/// Partition `module` items into chunk sub-modules.
///
/// chunk_0 gets all types (structs, enums, traits, type_aliases, consts) plus
/// the first `items_per_chunk` functions.  Subsequent chunks get pure function
/// slices.  Always returns at least one chunk.
fn split_module(module: &IrModule<IrFunction>, items_per_chunk: usize) -> Vec<IrModule<IrFunction>> {
    let per_chunk = items_per_chunk.max(1);
    let fns = &module.functions;
    let num_chunks = if fns.is_empty() { 1 } else { (fns.len() + per_chunk - 1) / per_chunk };

    let mut chunks: Vec<IrModule<IrFunction>> = Vec::with_capacity(num_chunks);

    for i in 0..num_chunks {
        let start = i * per_chunk;
        let end = (start + per_chunk).min(fns.len());
        let mut chunk: IrModule<IrFunction> = IrModule {
            name: module.name.clone(),
            structs: Vec::new(),
            enums: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            functions: fns[start..end].to_vec(),
            type_aliases: Vec::new(),
            consts: Vec::new(),
        };
        if i == 0 {
            // Types always go in the first chunk.
            chunk.structs = module.structs.clone();
            chunk.enums = module.enums.clone();
            chunk.traits = module.traits.clone();
            chunk.type_aliases = module.type_aliases.clone();
            chunk.consts = module.consts.clone();
            chunk.impls = module.impls.clone();
        }
        chunks.push(chunk);
    }

    // Topological sort across chunks based on type-name deps (in practice,
    // chunks are already ordered since types are in chunk_0, but run the
    // sort for correctness when items_per_chunk is very large or types
    // reference each other across chunks).
    let n = chunks.len();
    let exported: Vec<BTreeSet<String>> = chunks.iter().map(|m| {
        let mut names = BTreeSet::new();
        for s in &m.structs { names.insert(s.kind.to_string()); }
        for t in &m.traits  { names.insert(t.kind.to_string()); }
        names
    }).collect();

    let deps: Vec<Vec<usize>> = (0..n).map(|i| {
        let refs = collect_type_refs(&chunks[i]);
        (0..n).filter(|&j| j != i && exported[j].iter().any(|name| refs.contains(name))).collect()
    }).collect();

    let order = topo_sort(n, &deps);
    order.into_iter().map(|i| chunks[i].clone()).collect()
}

// ============================================================================
// Helpers
// ============================================================================

struct OwnedRemote {
    rust_crate: String,
    npm_package: Option<String>,
    type_names: Vec<String>,
}
