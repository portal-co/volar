#![no_std]

use alloc::{collections::btree_map::BTreeMap, vec::Vec};
extern crate alloc;
pub mod boolar;
pub mod ir;
pub mod lower_to_circuit;
pub mod lower_lir;
pub use lower_to_circuit::LoweringMode;
