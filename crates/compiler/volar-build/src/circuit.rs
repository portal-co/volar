// @reliability: experimental
// @ai: assisted
//! On-disk circuit format shared by the `weave-rust` and `pipeline` features.

use volar_ir::boolar::BIrBlocks;
use volar_ir::ir::{IRBlocks, IRTypes};

/// On-disk representation of a circuit IR.
///
/// Serialized with rkyv; the conventional file extension is `.circuit`.
#[non_exhaustive]
#[derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub enum SavedCircuit {
    /// A Boolar IR circuit (boolean SSA gates: AND / XOR / NOT).
    Boolar(BIrBlocks),
    /// A Volar IR circuit (field-level SSA) together with its type table.
    Volar(IRBlocks, IRTypes),
}
