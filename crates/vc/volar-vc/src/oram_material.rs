//! Public partition scripts for directionally-owned durable material blocks.
//!
//! The boolar circuit is the same AES-XOR operation in every direction. This
//! module is the protocol seam: callers select a direction and receive the
//! exact `SplitInput`/`SplitOutput` script required to keep a garbler base,
//! evaluator label, and evaluator-hosted ciphertext in their proper roles.

use alloc::vec;
use alloc::vec::Vec;

use volar_mpc::strict_split::{SplitInput, SplitOutput};

use crate::oram_gadget::MaterialBlockDirection;

/// Bits in a fixed material AES block.
pub const MATERIAL_BLOCK_BITS: usize = 128;

/// Public split-runner partition for one material block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MaterialBlockProtocol {
    /// Input owners, in `[key, tweak, material]` circuit order.
    pub inputs: Vec<SplitInput>,
    /// Role-private output disposition for all 128 ciphertext/material bits.
    pub outputs: Vec<SplitOutput>,
}

impl MaterialBlockProtocol {
    /// Construct the ownership script for one explicitly named direction.
    pub fn for_direction(direction: MaterialBlockDirection) -> Self {
        let material_owner = match direction {
            MaterialBlockDirection::SealGarbler => SplitInput::Garbler,
            MaterialBlockDirection::OpenGarbler
            | MaterialBlockDirection::SealEvaluator
            | MaterialBlockDirection::OpenEvaluator => SplitInput::Evaluator,
        };
        let output = match direction {
            MaterialBlockDirection::OpenGarbler => SplitOutput::GarblerReveal,
            MaterialBlockDirection::SealGarbler
            | MaterialBlockDirection::SealEvaluator
            | MaterialBlockDirection::OpenEvaluator => SplitOutput::EvaluatorReveal,
        };
        Self {
            inputs: [
                vec![SplitInput::Garbler; 64],
                vec![SplitInput::Evaluator; 64],
                vec![SplitInput::Public; MATERIAL_BLOCK_BITS],
                vec![material_owner; MATERIAL_BLOCK_BITS],
            ]
            .concat(),
            outputs: vec![output; MATERIAL_BLOCK_BITS],
        }
    }
}
