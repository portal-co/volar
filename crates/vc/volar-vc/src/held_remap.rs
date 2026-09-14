//! Deferred opaque-label remapping protocol design.
//!
//! A strict held output already *is* a valid next input label when the next
//! circuit adopts its garbler false-label base. That zero-cost rebase is the
//! preferred path. Durable restoration is different: a material AES opening
//! yields bytes, not a label under a newly garbled circuit's wire base. This
//! module records the safe deferred alternative: remap one output label to a
//! fresh input base without decoding the Boolean first.

use alloc::vec;
use alloc::vec::Vec;

use volar_mpc::strict_split::{SplitInput, SplitOutput};

/// Public script for an opaque output-to-input label remapping circuit.
///
/// `source` is held material, `target` is a fresh garbler base paired with an
/// evaluator-private selector through OT. The output remains opaque, so no
/// party learns the logical bit. This is a protocol contract only: it makes
/// explicit why a simple host-side byte copy is insufficient after durability.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeferredLabelRemapPlan {
    pub inputs: Vec<SplitInput>,
    pub outputs: Vec<SplitOutput>,
}

impl DeferredLabelRemapPlan {
    /// One-bit opaque rebase. It consumes the source label/base as `Held`,
    /// contributes a fresh target encoding through the normal role partition,
    /// and retains the mapped output in role-local state.
    pub fn one_bit() -> Self {
        Self {
            inputs: vec![SplitInput::Held, SplitInput::Garbler, SplitInput::Evaluator],
            outputs: vec![SplitOutput::Opaque],
        }
    }

    /// Fixed-width opaque remapping is just the public repetition of the
    /// one-bit script. It is suitable for deferred batches after material
    /// opening; no Boolean decoding/re-encoding occurs at the host seam.
    pub fn width(bits: usize) -> Self {
        let one = Self::one_bit();
        Self {
            inputs: one.inputs.repeat(bits),
            outputs: one.outputs.repeat(bits),
        }
    }
}
