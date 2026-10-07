//! Public partition scripts for directionally-owned durable material blocks.
//!
//! The boolar circuit is the same AES-XOR operation in every direction. This
//! module is the protocol seam: callers select a direction and receive the
//! exact `SplitInput`/`SplitOutput` script required to keep a garbler base,
//! evaluator label, and evaluator-hosted ciphertext in their proper roles.

use alloc::collections::BTreeSet;
use alloc::vec;
use alloc::vec::Vec;

use volar_mpc::strict_chain::MaterialRole;
use volar_mpc::strict_split::{SplitInput, SplitOutput};

use crate::oram_gadget::MaterialBlockDirection;

/// Bits in a fixed material AES block.
pub const MATERIAL_BLOCK_BITS: usize = 128;
/// AES material-cipher block width in bytes.
pub const MATERIAL_BLOCK_BYTES: usize = MATERIAL_BLOCK_BITS / 8;

/// Fixed public packing layout for opaque role-local label bytes.
///
/// The layout never splits one label across encrypted blocks. A label of
/// `label_bytes < 16` shares a block with a fixed number of neighbouring slots;
/// a 16-byte garbling label uses exactly one block. The final partial block is
/// zero padded, so the number of encrypted blocks depends only on public slot
/// extent, never on label values.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MaterialBlockLayout {
    label_bytes: usize,
    labels_per_block: usize,
}

impl MaterialBlockLayout {
    pub fn new(label_bytes: usize) -> Option<Self> {
        if label_bytes == 0 || label_bytes > MATERIAL_BLOCK_BYTES {
            return None;
        }
        Some(Self {
            label_bytes,
            labels_per_block: MATERIAL_BLOCK_BYTES / label_bytes,
        })
    }
    pub const fn label_bytes(self) -> usize {
        self.label_bytes
    }
    pub const fn labels_per_block(self) -> usize {
        self.labels_per_block
    }
    pub const fn block_for(self, slot: usize) -> usize {
        slot / self.labels_per_block
    }
    pub const fn offset_in_block(self, slot: usize) -> usize {
        (slot % self.labels_per_block) * self.label_bytes
    }

    /// Pack role-local fixed-width values from one public block. Missing slots
    /// receive deterministic zero padding; callers must reject an attempt to
    /// put a value in a different block or of a different width.
    pub fn pack_block<'a>(
        self,
        block: usize,
        values: impl IntoIterator<Item = (usize, &'a [u8])>,
    ) -> Option<[u8; MATERIAL_BLOCK_BYTES]> {
        let mut packed = [0u8; MATERIAL_BLOCK_BYTES];
        for (slot, value) in values {
            if self.block_for(slot) != block || value.len() != self.label_bytes {
                return None;
            }
            let offset = self.offset_in_block(slot);
            packed[offset..offset + self.label_bytes].copy_from_slice(value);
        }
        Some(packed)
    }

    /// Borrow the fixed-width value for `slot` from an opened packed block.
    pub fn value_from_block<'a>(
        self,
        slot: usize,
        block: &'a [u8; MATERIAL_BLOCK_BYTES],
    ) -> &'a [u8] {
        let offset = self.offset_in_block(slot);
        &block[offset..offset + self.label_bytes]
    }
}

/// Public cache/flush planner for packed durable material.
///
/// It is deliberately label-agnostic: adapters keep label bytes role-local,
/// while this planner tells them which packed ciphertext blocks need opening
/// or sealing. Thus repeated reads of resident labels produce no AES work and
/// multiple dirty labels in the same block produce one encryption operation.
#[derive(Clone, Debug)]
pub struct MaterialBlockCachePlan {
    layout: MaterialBlockLayout,
    resident: BTreeSet<usize>,
    dirty_blocks: BTreeSet<usize>,
}

impl MaterialBlockCachePlan {
    pub fn new(layout: MaterialBlockLayout) -> Self {
        Self {
            layout,
            resident: BTreeSet::new(),
            dirty_blocks: BTreeSet::new(),
        }
    }
    pub const fn layout(&self) -> MaterialBlockLayout {
        self.layout
    }
    pub fn needs_open(&self, slot: usize) -> bool {
        !self.resident.contains(&slot)
    }
    pub fn mark_open(&mut self, slot: usize) {
        self.resident.insert(slot);
    }
    pub fn mark_write(&mut self, slot: usize) {
        self.resident.insert(slot);
        self.dirty_blocks.insert(self.layout.block_for(slot));
    }
    /// Forget a resident value at an explicit persistence boundary. A later
    /// read will require opening its public packed block again.
    pub fn evict(&mut self, slot: usize) {
        self.resident.remove(&slot);
    }

    /// Return each dirty packed block once, in public block-index order.
    pub fn take_flush_blocks(&mut self) -> Vec<usize> {
        core::mem::take(&mut self.dirty_blocks)
            .into_iter()
            .collect()
    }
    pub fn resident_slots(&self) -> usize {
        self.resident.len()
    }
}

/// Public split-runner partition for one material block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MaterialBlockProtocol {
    /// Input owners, in `[key, tweak, material]` circuit order.
    pub inputs: Vec<SplitInput>,
    /// Role-private output disposition for all 128 ciphertext/material bits.
    pub outputs: Vec<SplitOutput>,
}

impl MaterialBlockProtocol {
    /// Select the one valid direction for a role-local store operation.
    /// `Both` is intentionally rejected: each half must persist in a separate
    /// transaction, so the physical ciphertext stream never combines them.
    pub fn for_store_owner(owner: MaterialRole) -> Option<MaterialBlockDirection> {
        match owner {
            MaterialRole::Garbler => Some(MaterialBlockDirection::SealGarbler),
            MaterialRole::Evaluator => Some(MaterialBlockDirection::SealEvaluator),
            MaterialRole::Both => None,
        }
    }

    /// Select the one valid opening direction for one role-local stream.
    pub fn for_load_owner(owner: MaterialRole) -> Option<MaterialBlockDirection> {
        match owner {
            MaterialRole::Garbler => Some(MaterialBlockDirection::OpenGarbler),
            MaterialRole::Evaluator => Some(MaterialBlockDirection::OpenEvaluator),
            MaterialRole::Both => None,
        }
    }

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
