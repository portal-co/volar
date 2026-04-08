#![no_std]

use alloc::{collections::btree_map::BTreeMap, vec::Vec};
extern crate alloc;
pub mod boolar;
pub mod ir;
pub mod lower_to_circuit;
pub use lower_to_circuit::LoweringMode;
