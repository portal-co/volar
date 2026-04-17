#![no_std]

use alloc::{collections::btree_map::BTreeMap, vec::Vec};
extern crate alloc;
pub mod boolar;
pub mod ir;

/// Re-export provenance handler types for convenience.
pub use volar_provenance::{self, ProvenanceHandler, NoProvenance, KeepProvenance, MapProvenance};
