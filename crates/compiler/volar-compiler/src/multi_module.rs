// @reliability: normal
//! Multi-module output for partitioned spec codegen.
//!
//! [`MultiModuleOutput`] holds a set of named output units (Rust crates or ESM
//! files) in topological order.  Each unit carries its own `IrModule` and a
//! list of the other units it depends on.
//!
//! # Usage
//!
//! 1. Build a [`LinkageSystem`] with inline specs for each group.
//! 2. Call [`LinkageSystem::partition`] with a [`PartitionConfig`] to split
//!    specs into named modules.
//! 3. Call [`MultiModuleOutput::render_rust`] or [`MultiModuleOutput::render_ts`]
//!    to obtain `(name, source_text)` pairs ready to write to disk.

#[cfg(feature = "std")]
use std::collections::{BTreeMap, BTreeSet};
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(not(feature = "std"))]
use alloc::collections::{BTreeMap, BTreeSet};
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::ir::{IrFunction, IrImpl, IrModule};
use crate::linkage::{LinkageKind, LinkageSystem, RemoteSpecRef};
use crate::printer::print_module_with_remotes;
use crate::printer_ts::print_module_ts_with_imports;

// ============================================================================
// PARTITION CONFIG
// ============================================================================

/// One named group of specs to be emitted as a single output module.
pub struct PartitionGroup {
    /// Short module name used in dep lists and import paths (e.g. `"primitives"`).
    pub name: String,
    /// Cargo crate name for Rust `pub use` statements (e.g. `"volar-primitives"`).
    pub rust_crate_name: String,
    /// NPM package for TypeScript `import` statements.
    /// Falls back to `rust_crate_name` when `None`.
    pub npm_package: Option<String>,
    /// Names of [`LinkedSpec`](crate::linkage::LinkedSpec) entries to include in this group.
    pub spec_names: Vec<String>,
}

/// Maps spec names to named output modules.
pub struct PartitionConfig {
    pub groups: Vec<PartitionGroup>,
}

// ============================================================================
// OUTPUT MODULE
// ============================================================================

/// A single named output unit (one Rust source file / one ESM file).
pub struct OutputModule {
    /// Short module name (matches [`PartitionGroup::name`]).
    pub name: String,
    /// Cargo crate name used in `pub use` statements.
    pub rust_crate_name: String,
    /// NPM package for TypeScript imports.
    pub npm_package: Option<String>,
    /// The IR for this module's declarations.
    pub module: IrModule<IrFunction>,
    /// Short names of other `OutputModule`s this one imports from,
    /// in topological order (deps before dependents).
    pub deps: Vec<String>,
}

impl OutputModule {
    pub(crate) fn type_names(&self) -> Vec<String> {
        let mut names: Vec<String> = Vec::new();
        for s in &self.module.structs {
            names.push(s.kind.to_string());
        }
        for t in &self.module.traits {
            names.push(t.kind.to_string());
        }
        names
    }
}

// ============================================================================
// MULTI-MODULE OUTPUT
// ============================================================================

/// A set of output modules in topological dependency order.
pub struct MultiModuleOutput {
    /// Ordered so that every module's `deps` appear before it in this vec.
    pub modules: Vec<OutputModule>,
}

impl MultiModuleOutput {
    /// Render each module to Rust source, with `pub use` imports for deps.
    ///
    /// Returns `(module_name, rust_source)` pairs in topological order.
    pub fn render_rust(&self) -> Vec<(String, String)> {
        // Build a map: module name → type names (for dep ref construction)
        let type_name_map: BTreeMap<&str, (&OutputModule, Vec<String>)> = self
            .modules
            .iter()
            .map(|m| (m.name.as_str(), (m, m.type_names())))
            .collect();

        self.modules
            .iter()
            .map(|m| {
                let remotes: Vec<RemoteSpecRef<'_>> = m
                    .deps
                    .iter()
                    .filter_map(|dep_name| {
                        type_name_map.get(dep_name.as_str()).map(|(dep_mod, names)| {
                            RemoteSpecRef {
                                rust_crate: dep_mod.rust_crate_name.as_str(),
                                npm_package: dep_mod.npm_package.as_deref(),
                                type_names: names.clone(),
                            }
                        })
                    })
                    .collect();
                let src = print_module_with_remotes(&m.module, &[], &remotes);
                (m.name.clone(), src)
            })
            .collect()
    }

    /// Render each module to TypeScript source, with ESM `import` statements for deps.
    ///
    /// Local deps use relative paths (`./dep-name`); deps with `npm_package`
    /// set use the npm package name.
    ///
    /// Returns `(module_name, ts_source)` pairs in topological order.
    pub fn render_ts(&self) -> Vec<(String, String)> {
        let type_name_map: BTreeMap<&str, (&OutputModule, Vec<String>)> = self
            .modules
            .iter()
            .map(|m| (m.name.as_str(), (m, m.type_names())))
            .collect();

        self.modules
            .iter()
            .map(|m| {
                let src = build_ts_with_local_deps(m, &type_name_map);
                (m.name.clone(), src)
            })
            .collect()
    }
}

/// Build TypeScript source for `m`, resolving dep import paths correctly.
fn build_ts_with_local_deps<'a>(
    m: &'a OutputModule,
    type_name_map: &BTreeMap<&str, (&'a OutputModule, Vec<String>)>,
) -> String {
    // Collect owned data first so we can take references from it.
    struct OwnedRemote {
        pkg: String,
        type_names: Vec<String>,
    }

    let owned: Vec<OwnedRemote> = m
        .deps
        .iter()
        .filter_map(|dep_name| {
            type_name_map.get(dep_name.as_str()).map(|(dep_mod, names)| {
                let pkg = dep_mod
                    .npm_package
                    .clone()
                    .unwrap_or_else(|| format!("./{}", dep_mod.name));
                OwnedRemote { pkg, type_names: names.clone() }
            })
        })
        .collect();

    // Build RemoteSpecRef from owned data (rust_crate re-used as pkg for TS).
    let remotes: Vec<RemoteSpecRef<'_>> = owned
        .iter()
        .map(|o| RemoteSpecRef {
            rust_crate: o.pkg.as_str(),
            npm_package: None, // ts_package() returns rust_crate when this is None
            type_names: o.type_names.clone(),
        })
        .collect();

    print_module_ts_with_imports(&m.module, &remotes)
}

// ============================================================================
// PARTITION IMPL ON LINKAGESYSTEM
// ============================================================================

impl LinkageSystem {
    /// Partition **inline** specs into named output modules according to `config`.
    ///
    /// Specs whose name does not match any group are silently ignored.
    /// Remote specs are always ignored (they are external by definition).
    ///
    /// Inter-module dependencies are inferred by scanning each group's type
    /// names against the other groups' exported type names.  The result is
    /// returned in topological order (dependencies first).
    pub fn partition(&self, config: &PartitionConfig) -> MultiModuleOutput {
        let n = config.groups.len();

        // ── 1. Collect specs per group ──────────────────────────────────────
        let mut group_modules: Vec<IrModule<IrFunction>> = (0..n)
            .map(|i| IrModule {
                name: config.groups[i].name.clone(),
                structs: Vec::new(),
                enums: Vec::new(),
                traits: Vec::new(),
                impls: Vec::new(),
                functions: Vec::new(),
                type_aliases: Vec::new(),
                consts: Vec::new(),
            })
            .collect();

        for spec in &self.specs {
            if matches!(spec.kind, LinkageKind::Remote { .. }) {
                continue;
            }
            for (gi, group) in config.groups.iter().enumerate() {
                if group.spec_names.iter().any(|sn| sn == &spec.name) {
                    let m = &mut group_modules[gi];
                    m.structs.extend(spec.module.structs.iter().cloned());
                    m.enums.extend(spec.module.enums.iter().cloned());
                    m.traits.extend(spec.module.traits.iter().cloned());
                    m.impls.extend(spec.module.impls.iter().cloned());
                    m.functions.extend(spec.module.functions.iter().cloned());
                    m.type_aliases.extend(spec.module.type_aliases.iter().cloned());
                }
            }
        }

        // ── 2. Build type-name set per group for dep inference ──────────────
        let exported: Vec<BTreeSet<String>> = group_modules
            .iter()
            .map(|m| {
                let mut names = BTreeSet::new();
                for s in &m.structs { names.insert(s.kind.to_string()); }
                for t in &m.traits { names.insert(t.kind.to_string()); }
                names
            })
            .collect();

        // ── 3. Infer deps: does group i reference any name exported by group j? ─
        let mut dep_indices: Vec<Vec<usize>> = vec![Vec::new(); n];
        for i in 0..n {
            let refs = collect_type_refs(&group_modules[i]);
            for j in 0..n {
                if i == j { continue; }
                if exported[j].iter().any(|name| refs.contains(name)) {
                    dep_indices[i].push(j);
                }
            }
        }

        // ── 4. Topological sort (Kahn's algorithm) ───────────────────────────
        let order = topo_sort(n, &dep_indices);

        // ── 5. Build OutputModule vec in topo order ──────────────────────────
        // Move modules out of the vec by taking them via Option.
        let mut module_slots: Vec<Option<IrModule<IrFunction>>> =
            group_modules.into_iter().map(Some).collect();

        let modules: Vec<OutputModule> = order
            .iter()
            .map(|&i| {
                let group = &config.groups[i];
                let module = module_slots[i].take().expect("each index visited once");
                let deps: Vec<String> = dep_indices[i]
                    .iter()
                    .map(|&j| config.groups[j].name.clone())
                    .collect();
                OutputModule {
                    name: group.name.clone(),
                    rust_crate_name: group.rust_crate_name.clone(),
                    npm_package: group.npm_package.clone(),
                    module,
                    deps,
                }
            })
            .collect();

        MultiModuleOutput { modules }
    }
}

// ============================================================================
// HELPERS
// ============================================================================

/// Collect all struct-kind type-name strings referenced within an `IrModule`'s
/// field types, impl self-types, and function signatures.  Used for dependency
/// inference between partition groups.
pub(crate) fn collect_type_refs(module: &IrModule<IrFunction>) -> BTreeSet<String> {
    use crate::ir::IrType;

    fn walk(ty: &IrType, out: &mut BTreeSet<String>) {
        match ty {
            IrType::Struct { kind, type_args } => {
                out.insert(kind.to_string());
                for a in type_args { walk(a, out); }
            }
            // Primitive types like Bit, Galois, Z3 may be exported as named structs
            // by a primitives spec group, so include their display names in refs.
            IrType::Primitive(p) => { out.insert(p.to_string()); }
            IrType::Array { elem, .. } => walk(elem, out),
            IrType::Vector { elem } => walk(elem, out),
            IrType::Reference { elem, .. } => walk(elem, out),
            IrType::Tuple(ts) => { for t in ts { walk(t, out); } }
            IrType::Projection { base, trait_args, .. } => {
                walk(base, out);
                for a in trait_args { walk(a, out); }
            }
            IrType::FnPtr { params, ret } => {
                for p in params { walk(p, out); }
                walk(ret, out);
            }
            IrType::Existential { bounds } => {
                for b in bounds {
                    for a in &b.type_args { walk(a, out); }
                }
            }
            IrType::TypeParam(_) | IrType::Unit
            | IrType::Never | IrType::Infer | IrType::Param { .. } => {}
        }
    }

    let mut refs = BTreeSet::new();

    for s in &module.structs {
        for field in &s.fields {
            walk(&field.ty, &mut refs);
        }
    }
    for imp in &module.impls {
        walk(&imp.self_ty, &mut refs);
    }
    for func in &module.functions {
        for param in &func.params {
            walk(&param.ty, &mut refs);
        }
        if let Some(ret) = &func.return_type {
            walk(ret, &mut refs);
        }
    }

    refs
}

/// Topological sort (Kahn's algorithm).
///
/// `deps[i]` = list of indices that `i` depends on (must come before `i`).
/// Returns indices in dependency-first order.
pub(crate) fn topo_sort(n: usize, deps: &[Vec<usize>]) -> Vec<usize> {
    // Build reverse edges: dependents[j] = nodes that depend on j.
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut in_deg = vec![0usize; n];
    for i in 0..n {
        for &j in &deps[i] {
            dependents[j].push(i);
            in_deg[i] += 1;
        }
    }
    let mut queue: Vec<usize> = (0..n).filter(|&i| in_deg[i] == 0).collect();
    let mut order: Vec<usize> = Vec::with_capacity(n);
    while !queue.is_empty() {
        let node = queue.remove(0);
        order.push(node);
        for &dep in &dependents[node] {
            in_deg[dep] -= 1;
            if in_deg[dep] == 0 {
                queue.push(dep);
            }
        }
    }
    // Append any remaining nodes (disconnected or cycle members) in index order.
    for i in 0..n {
        if !order.contains(&i) {
            order.push(i);
        }
    }
    order
}
