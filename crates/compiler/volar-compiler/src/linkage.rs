// @reliability: normal
//! Linkage system for merging spec `IrModule`s into generated output.
//!
//! A [`LinkageSystem`] accumulates one or more [`LinkedSpec`] modules and can
//! merge their declarations (structs, traits, impls, functions, type aliases)
//! into any target `IrModule`, making the output self-contained.
//!
//! Specs marked [`LinkageKind::Remote`] are never inlined — instead their type
//! names are surfaced via [`LinkageSystem::remote_refs`] so preamble writers
//! can emit `pub use` (Rust) or `import` (TypeScript) statements.

#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};

#[cfg(feature = "std")]
use std::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::ir::{IrAnyFunction, IrCfgModule, IrFunction, IrImpl, IrModule, MapProv};

// ============================================================================
// LINKAGE KIND
// ============================================================================

/// How a spec's declarations are incorporated into generated output.
pub enum LinkageKind {
    /// Inline all declarations into the output module (existing behaviour).
    Inline,
    /// Reference types from an external Rust crate / npm package.
    ///
    /// [`LinkageSystem::apply`] skips body inlining for remote specs.
    /// Call [`LinkageSystem::remote_refs`] to obtain type-name lists for
    /// preamble writers to emit `pub use` or `import` statements.
    Remote {
        /// Cargo crate name (e.g. `"volar-primitives"`).
        rust_crate: String,
        /// NPM package name for TypeScript output (e.g. `"@portal/primitives"`).
        /// Falls back to `rust_crate` when absent.
        npm_package: Option<String>,
    },
}

// ============================================================================
// LINKED SPEC
// ============================================================================

/// A named spec module to be linked into generated output.
pub struct LinkedSpec {
    /// Human-readable name for this spec (e.g. `"garble"`).
    pub name: String,
    /// The parsed `IrModule` containing the spec's declarations.
    pub module: IrModule<IrFunction>,
    /// Whether to inline declarations or reference them externally.
    pub kind: LinkageKind,
}

impl LinkedSpec {
    /// Convenience constructor for an inline spec (backward-compatible with
    /// the old two-field struct literal).
    pub fn new_inline(name: impl Into<String>, module: IrModule<IrFunction>) -> Self {
        Self { name: name.into(), module, kind: LinkageKind::Inline }
    }

    /// Convenience constructor for a remote spec.
    pub fn new_remote(
        name: impl Into<String>,
        module: IrModule<IrFunction>,
        rust_crate: impl Into<String>,
        npm_package: Option<String>,
    ) -> Self {
        Self {
            name: name.into(),
            module,
            kind: LinkageKind::Remote { rust_crate: rust_crate.into(), npm_package },
        }
    }
}

// ============================================================================
// REMOTE SPEC REF
// ============================================================================

/// Borrowed view of a remote spec's import information, used by preamble writers.
pub struct RemoteSpecRef<'a> {
    /// Cargo crate name (e.g. `"volar-primitives"`).
    pub rust_crate: &'a str,
    /// NPM package name. Falls back to `rust_crate` when `None`.
    pub npm_package: Option<&'a str>,
    /// Struct and trait names exported from this remote spec.
    pub type_names: Vec<String>,
}

impl<'a> RemoteSpecRef<'a> {
    /// The import path to use in TypeScript (`npm_package` if set, else `rust_crate`).
    pub fn ts_package(&self) -> &str {
        self.npm_package.unwrap_or(self.rust_crate)
    }
}

// ============================================================================
// LINKAGE SYSTEM
// ============================================================================

/// Accumulates [`LinkedSpec`] modules and merges them into target `IrModule`s.
///
/// Inline specs are merged via [`apply`](LinkageSystem::apply).
/// Remote specs are surfaced via [`remote_refs`](LinkageSystem::remote_refs)
/// for preamble/import generation.
#[derive(Default)]
pub struct LinkageSystem {
    pub specs: Vec<LinkedSpec>,
}

impl LinkageSystem {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a spec to the linkage set.
    pub fn add(&mut self, spec: LinkedSpec) {
        self.specs.push(spec);
    }

    /// Merge all **inline** spec declarations into `target`.
    ///
    /// Remote specs are silently skipped — call [`remote_refs`](Self::remote_refs)
    /// to obtain their type names for import generation.
    ///
    /// Items are appended in the order they were added; no deduplication is
    /// performed. Call this once before rendering the target module.
    pub fn apply<Q: Clone + Default>(&self, target: &mut IrModule<IrFunction<Q>, Q>) {
        for spec in &self.specs {
            if matches!(spec.kind, LinkageKind::Remote { .. }) {
                continue;
            }
            target.structs.extend(spec.module.structs.iter().cloned());
            target.enums.extend(spec.module.enums.iter().cloned());
            target.traits.extend(spec.module.traits.iter().cloned());
            target.impls.extend(spec.module.impls.iter().cloned().map(|i: IrImpl<()>| i.map_prov(&|_| Q::default())));
            target.functions.extend(spec.module.functions.iter().cloned().map(|f: IrFunction<()>| f.map_prov(&|_| Q::default())));
            target.type_aliases.extend(spec.module.type_aliases.iter().cloned());
        }
    }

    /// Merge all **inline** spec declarations into a CFG module.
    ///
    /// Remote specs are skipped. Flat spec functions are wrapped as
    /// [`IrAnyFunction::Flat`].
    pub fn apply_cfg<Q: Clone + Default>(&self, target: &mut IrCfgModule<Q>) {
        for spec in &self.specs {
            if matches!(spec.kind, LinkageKind::Remote { .. }) {
                continue;
            }
            target.structs.extend(spec.module.structs.iter().cloned());
            target.enums.extend(spec.module.enums.iter().cloned());
            target.traits.extend(spec.module.traits.iter().cloned());
            target.impls.extend(spec.module.impls.iter().cloned().map(|i: IrImpl<()>| i.map_prov(&|_| Q::default())));
            target.functions.extend(
                spec.module.functions.iter().cloned()
                    .map(|f: IrFunction<()>| IrAnyFunction::Flat(f.map_prov(&|_| Q::default())))
            );
            target.type_aliases.extend(spec.module.type_aliases.iter().cloned());
        }
    }

    /// Return import metadata for all remote specs.
    ///
    /// Each entry contains the crate/package name and the struct+trait names
    /// that preamble writers should emit as `pub use` (Rust) or `import` (TS).
    pub fn remote_refs(&self) -> Vec<RemoteSpecRef<'_>> {
        self.specs.iter().filter_map(|spec| {
            match &spec.kind {
                LinkageKind::Remote { rust_crate, npm_package } => {
                    let mut type_names: Vec<String> = Vec::new();
                    for s in &spec.module.structs {
                        type_names.push(s.kind.to_string());
                    }
                    for t in &spec.module.traits {
                        type_names.push(t.kind.to_string());
                    }
                    Some(RemoteSpecRef {
                        rust_crate: rust_crate.as_str(),
                        npm_package: npm_package.as_deref(),
                        type_names,
                    })
                }
                LinkageKind::Inline => None,
            }
        }).collect()
    }
}
