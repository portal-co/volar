// @reliability: experimental
// @ai: assisted
//! Shared glue for the virtualisation pass — the [`VirtOutput`] wrapper
//! and the dedup-index table used by both the IR and BIR impls.

use alloc::{collections::BTreeMap, vec::Vec};

use crate::bytecode::{BytecodeEntry, HandlerImmSchema, VirtBytecode};
use crate::canon::{BlockImmediates, HandlerKey};

/// Output of [`crate::virtualize_ir`] / [`crate::virtualize_bir`].
#[derive(Clone, Debug)]
pub struct VirtOutput<B> {
    /// The rewritten IR (or BIR) module.
    pub blocks: B,
    /// The bytecode artefact, present iff the config asked for
    /// [`crate::BytecodeForm::wants_external`].
    pub bytecode: Option<VirtBytecode>,
    /// Number of unique handlers after deduplication.  Primary size
    /// metric of the pass.
    pub n_handlers: usize,
    /// Number of original blocks in the input module.  `blocks_in ==
    /// n_handlers` means the pass was a no-op (no duplicates existed).
    pub blocks_in: usize,
}

/// Intermediate table built by canonicalising every block.
///
/// `per_block[b]` holds `(handler_idx, immediates)` for the original
/// block at index `b`.  `handler_keys[h]` is the canonical block key
/// whose body is emitted as handler `h`.
#[derive(Clone, Debug)]
pub struct DedupTable<K: HandlerKey> {
    pub per_block: Vec<(u32, BlockImmediates)>,
    pub handler_keys: Vec<K>,
}

impl<K: HandlerKey> DedupTable<K> {
    /// Build a dedup table from a list of canonicalised per-block results.
    pub fn build(per_block_canon: Vec<(K, BlockImmediates)>) -> Self {
        let mut keys_to_idx: BTreeMap<K, u32> = BTreeMap::new();
        let mut handler_keys: Vec<K> = Vec::new();
        let mut per_block = Vec::with_capacity(per_block_canon.len());

        for (key, imm) in per_block_canon {
            let idx = if let Some(&i) = keys_to_idx.get(&key) {
                i
            } else {
                let i = handler_keys.len() as u32;
                keys_to_idx.insert(key.clone(), i);
                handler_keys.push(key);
                i
            };
            per_block.push((idx, imm));
        }

        Self {
            per_block,
            handler_keys,
        }
    }

    /// Number of unique handlers.
    pub fn n_handlers(&self) -> usize {
        self.handler_keys.len()
    }

    /// Assemble the external bytecode artefact from this table.
    pub fn to_bytecode(&self) -> VirtBytecode {
        let handler_schemas: Vec<HandlerImmSchema> = self
            .handler_keys
            .iter()
            .map(|k| HandlerImmSchema {
                kinds: k.immediate_schema(),
            })
            .collect();
        let entries: Vec<BytecodeEntry> = self
            .per_block
            .iter()
            .map(|(h, imm)| BytecodeEntry {
                handler_idx: *h,
                consts: imm.consts.clone(),
                targets: imm.targets.clone(),
            })
            .collect();
        VirtBytecode {
            n_handlers: self.handler_keys.len(),
            handler_schemas,
            entries,
        }
    }
}
