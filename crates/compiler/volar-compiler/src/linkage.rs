// @reliability: normal
//! Linkage system for merging spec `IrModule`s into generated output.
//!
//! A [`LinkageSystem`] accumulates one or more [`LinkedSpec`] modules and can
//! merge their declarations (structs, traits, impls, functions, type aliases)
//! into any target `IrModule`, making the output self-contained.

#[cfg(feature = "std")]
use std::string::String;
#[cfg(not(feature = "std"))]
use alloc::string::String;

#[cfg(feature = "std")]
use std::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::ir::{IrModule, IrCfgModule};

/// A named spec module to be linked into generated output.
pub struct LinkedSpec {
    /// Human-readable name for this spec (e.g. `"garble"`).
    pub name: String,
    /// The parsed `IrModule` containing the spec's declarations.
    pub module: IrModule,
}

/// Accumulates [`LinkedSpec`] modules and merges them into target `IrModule`s.
///
/// After [`apply`](LinkageSystem::apply), the target module contains all
/// declarations from every linked spec, making the output self-contained.
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

    /// Merge all linked spec declarations into `target`.
    ///
    /// Items are appended in the order they were added; no deduplication is
    /// performed. Call this once before rendering the target module.
    ///
    /// The linked spec declarations carry `()` provenance; they are assigned
    /// `Q::default()` in the output module.
    pub fn apply<Q: Clone + Default>(&self, target: &mut IrModule<Q>) {
        for spec in &self.specs {
            target.structs.extend(spec.module.structs.iter().cloned());
            target.enums.extend(spec.module.enums.iter().cloned());
            target.traits.extend(spec.module.traits.iter().cloned());
            target.impls.extend(spec.module.impls.iter().cloned().map(|i| i.map_prov(&|_| Q::default())));
            target.functions.extend(spec.module.functions.iter().cloned().map(|f| f.map_prov(&|_| Q::default())));
            target.type_aliases.extend(spec.module.type_aliases.iter().cloned());
        }
    }

    /// Merge all linked spec declarations into a CFG module.
    ///
    /// Structs, enums, traits, impls, and type aliases are appended directly.
    /// Linked spec functions (which have linear bodies) go into
    /// `auxiliary_functions` rather than the CFG `functions` list.
    pub fn apply_cfg<Q: Clone + Default>(&self, target: &mut IrCfgModule<Q>) {
        for spec in &self.specs {
            target.structs.extend(spec.module.structs.iter().cloned());
            target.enums.extend(spec.module.enums.iter().cloned());
            target.traits.extend(spec.module.traits.iter().cloned());
            target.impls.extend(spec.module.impls.iter().cloned().map(|i| i.map_prov(&|_| Q::default())));
            target.auxiliary_functions.extend(spec.module.functions.iter().cloned().map(|f| f.map_prov(&|_| Q::default())));
            target.type_aliases.extend(spec.module.type_aliases.iter().cloned());
        }
    }
}
