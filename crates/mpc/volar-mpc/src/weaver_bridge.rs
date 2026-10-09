// @reliability: experimental
// @experimental-status: design
// @ai: assisted
//! Bridge from [`GateSchedule`] (volar-mpc's circuit executor schedule) to
//! volar-ir's [`BIrBlocks`] boolean IR, so a schedule that the garbler /
//! interpreter runs can be **woven** into the compiler backends (LIR /
//! WASM / native) for a four-way differential.
//!
//! Only **pure-boolean** schedules bridge: `Zero`/`One`/`Xor`/`And`/`Not`
//! gates, empty `storages` and `actions`. A schedule with storage or action
//! gates returns [`BridgeError`] (those need a GRAM/action host that the
//! pure compiler path cannot express).
//!
//! # Shape
//!
//! `BIrBlocks` is a single-block circuit: `params = num_inputs`, one
//! [`BIrStmt`] per gate, and a `Jmp(Return, output_wires)` terminator.
//! `GateSchedule` gate `k` defines wire `num_inputs + k`, which maps
//! directly to `IRVarId(num_inputs + k)`. A `Not` gate is emitted as
//! `Xor(a, one)` with a shared constant-1 wire (the weaver's convention —
//! free NOT is XOR with the constant-1 wire).

use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};

use crate::{Gate, GateSchedule};

/// A schedule could not be bridged to a pure-boolean `BIrBlocks`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BridgeError {
    /// The schedule uses a GRAM storage op (`StorageRead`/`StorageWrite`).
    HasStorage,
    /// The schedule uses an action call (`ActionBit`).
    HasAction,
}

impl GateSchedule {
    /// Bridge this schedule to a single-block [`BIrBlocks`].
    ///
    /// Only pure-boolean schedules (empty `storages`/`actions`) convert;
    /// see the module docs. The output is `BIrBlocks<()>` (no provenance
    /// annotations). `Not` gates become `Xor(a, one)` with a shared
    /// constant-1 wire (the weaver's free-NOT convention).
    pub fn to_bir_blocks(&self) -> Result<BIrBlocks<()>, BridgeError> {
        if !self.storages.is_empty() {
            return Err(BridgeError::HasStorage);
        }
        if !self.actions.is_empty() {
            return Err(BridgeError::HasAction);
        }

        let num_inputs = self.num_inputs;
        // `BIrStmt`'s var namespace is the block's var space: params occupy
        // `0..num_inputs`, then each statement produces the next var.
        // `IRVarId(num_inputs + k)` is the output of the `k`-th statement,
        // matching `GateSchedule` gate `k` → wire `num_inputs + k`.
        let var = |wire: usize| IRVarId(wire as u32);

        // Lazily materialize the constant-0 / constant-1 wires shared by
        // all `Not` gates (NOT = XOR with the one-wire).
        let mut stmts: Vec<BIrStmt> = Vec::with_capacity(self.gates.len() + 2);
        let mut one_var: Option<u32> = None;
        let mut stmt_index = num_inputs;

        for gate in &self.gates {
            let stmt = match *gate {
                Gate::Zero => {
                    stmt_index += 1;
                    BIrStmt::Zero
                }
                Gate::One => {
                    stmt_index += 1;
                    BIrStmt::One
                }
                Gate::Xor(a, b) => {
                    stmt_index += 1;
                    BIrStmt::Xor(var(a), var(b))
                }
                Gate::And(a, b) => {
                    stmt_index += 1;
                    BIrStmt::And(var(a), var(b))
                }
                Gate::Not(a) => {
                    // NOT = XOR with the constant-1 wire (weaver
                    // convention). Materialize `one` on first use.
                    let one = match one_var {
                        Some(v) => v,
                        None => {
                            stmts.push(BIrStmt::One);
                            let v = stmt_index as u32;
                            stmt_index += 1;
                            one_var = Some(v);
                            v
                        }
                    };
                    stmt_index += 1;
                    BIrStmt::Xor(var(a), IRVarId(one))
                }
                Gate::StorageRead { .. } | Gate::StorageWrite { .. } => {
                    return Err(BridgeError::HasStorage);
                }
                Gate::ActionBit { .. } => return Err(BridgeError::HasAction),
                // Non-exhaustive `Gate` may grow; surface as a storage/
                // action-shaped rejection rather than silently dropping.
                _ => return Err(BridgeError::HasAction),
            };
            stmts.push(stmt);
        }

        // Output wires: the schedule's `outputs` (or `[output]`).
        let outputs: Vec<usize> = match &self.outputs {
            Some(outs) => outs.clone(),
            None => alloc::vec![self.output],
        };
        let args: Vec<IRVarId> = outputs.iter().map(|&w| var(w)).collect();

        Ok(BIrBlocks {
            blocks: alloc::vec![BIrBlock {
                params: num_inputs as u32,
                stmts: stmts
                    .into_iter()
                    .map(|s| volar_ir_common::Node::new(s, (), None))
                    .collect(),
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args,
                }),
            }],
            pre_init: alloc::vec![],
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bool_schedule(gates: Vec<Gate>, inputs: usize, output: usize) -> GateSchedule {
        GateSchedule {
            num_inputs: inputs,
            gates,
            output,
            outputs: None,
            storages: Vec::new(),
            actions: Vec::new(),
        }
    }

    #[test]
    fn pure_boolean_bridges() {
        // (x0 ∧ x1) ⊕ x2, with a NOT: out = (x0 ∧ x1) ⊕ ¬x2.
        let sched = bool_schedule(
            alloc::vec![
                Gate::And(0, 1),  // wire 3
                Gate::Not(2),     // wire 4 = ¬x2 (as Xor(2, one))
                Gate::Xor(3, 4),  // wire 5
            ],
            3,
            5,
        );
        let blocks = sched.to_bir_blocks().expect("pure boolean bridges");
        assert_eq!(blocks.blocks.len(), 1);
        let block = &blocks.blocks[0];
        assert_eq!(block.params, 3);
        // 3 gates + 1 shared constant-1 wire for the NOT.
        assert_eq!(block.stmts.len(), 4);
        assert!(matches!(
            block.terminator,
            BIrTerminator::Jmp(_)
        ));
    }

    #[test]
    fn storage_and_action_rejected() {
        let mut sched = bool_schedule(alloc::vec![Gate::Xor(0, 1)], 2, 2);
        // Force a storage gate.
        sched.gates.push(Gate::StorageRead {
            storage: 0,
            cell: 0,
            access: 1,
        });
        assert_eq!(sched.to_bir_blocks(), Err(BridgeError::HasStorage));
    }
}
