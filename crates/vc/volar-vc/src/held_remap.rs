//! Deferred opaque-label remapping.
//!
//! A strict held output already is a valid next input label when the next
//! circuit adopts its false-label base. That zero-cost rebase remains the
//! preferred path. Durable restoration is different: after a material opening
//! a caller may need to bind an opaque restored label to a fresh circuit-wire
//! base without decoding its Boolean value.
//!
//! [`deferred_remap_schedule`] performs that binding with a free-XOR gate:
//! `new = held XOR public_zero`. The garbler supplies a fresh false-label base
//! for every `public_zero` wire and sends only its zero label; the evaluator
//! combines it with its held active label. Thus the output has a fresh base,
//! retains the same hidden Boolean, and remains [`SplitOutput::Opaque`].

use alloc::vec;
use alloc::vec::Vec;

use volar_mpc::strict_split::{SplitInput, SplitOutput};
use volar_mpc::{Gate, GateSchedule};

/// Build `bits` independent, opaque output-to-input rebases.
///
/// Inputs are `[held_0, public_zero_0, held_1, public_zero_1, ...]`; each
/// public wire must be supplied as `false`. The output for bit `i` is
/// `held_i XOR public_zero_i`. Although XOR is free, its output false base is
/// the XOR of the held base and a fresh public-wire base, so it is distinct
/// from the restored source base without revealing the underlying Boolean.
pub fn deferred_remap_schedule(bits: usize) -> GateSchedule {
    assert!(bits > 0, "opaque remap needs at least one bit");
    let num_inputs = bits * 2;
    let gates = (0..bits)
        .map(|bit| Gate::Xor(bit * 2, bit * 2 + 1))
        .collect::<Vec<_>>();
    let outputs = (0..bits).map(|bit| num_inputs + bit).collect::<Vec<_>>();
    GateSchedule {
        num_inputs,
        gates,
        output: outputs[0],
        outputs: Some(outputs),
        storages: vec![],
        actions: vec![],
    }
}

/// Public script for [`deferred_remap_schedule`].
///
/// The fresh target encoding is represented by a public zero wire, not by a
/// host-side copy or decode/re-encode. The garbler chooses its fresh base;
/// the evaluator receives only the corresponding zero label. Both outputs
/// stay role-local and opaque.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeferredLabelRemapPlan {
    pub inputs: Vec<SplitInput>,
    pub public_bits: Vec<bool>,
    pub outputs: Vec<SplitOutput>,
}

impl DeferredLabelRemapPlan {
    /// One-bit opaque rebase.
    pub fn one_bit() -> Self {
        Self {
            inputs: vec![SplitInput::Held, SplitInput::Public],
            public_bits: vec![false],
            outputs: vec![SplitOutput::Opaque],
        }
    }

    /// Fixed-width opaque remapping. `bits` must match the restored held
    /// material width and [`deferred_remap_schedule`]'s width.
    pub fn width(bits: usize) -> Self {
        assert!(bits > 0, "opaque remap needs at least one bit");
        let one = Self::one_bit();
        Self {
            inputs: one.inputs.repeat(bits),
            public_bits: one.public_bits.repeat(bits),
            outputs: one.outputs.repeat(bits),
        }
    }
}
