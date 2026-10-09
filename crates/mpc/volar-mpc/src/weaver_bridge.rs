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
//! [`BIrStmt`] per scheduled gate (plus one shared constant-1 statement
//! when any `Not` appears), and a `Jmp(Return, output_wires)` terminator.
//! Inputs keep their wire IDs. If a shared constant-1 statement is needed,
//! computed schedule wires are remapped by one so the inserted statement
//! does not shift or alias any gate output. A `Not` gate is emitted as
//! `Xor(a, one)` per the weaver convention.

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
        // `BIrStmt` results are numbered by statement position after the
        // params. Prefix the shared one before all gates, then remap every
        // computed schedule wire by that prefix length. Inputs are params
        // and retain their IDs.
        let has_not = self.gates.iter().any(|gate| matches!(gate, Gate::Not(_)));
        let prefix = usize::from(has_not);
        let var = |wire: usize| {
            let remapped = if wire < num_inputs { wire } else { wire + prefix };
            IRVarId(remapped as u32)
        };

        let mut stmts: Vec<BIrStmt> = Vec::with_capacity(self.gates.len() + prefix);
        if has_not {
            // This is IRVarId(num_inputs), the shared XOR-with-one operand.
            stmts.push(BIrStmt::One);
        }

        for gate in &self.gates {
            let stmt = match *gate {
                Gate::Zero => BIrStmt::Zero,
                Gate::One => BIrStmt::One,
                Gate::Xor(a, b) => BIrStmt::Xor(var(a), var(b)),
                Gate::And(a, b) => BIrStmt::And(var(a), var(b)),
                Gate::Not(a) => BIrStmt::Xor(var(a), IRVarId(num_inputs as u32)),
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
                Gate::And(0, 1), // wire 3
                Gate::Not(2),    // wire 4 = ¬x2 (as Xor(2, one))
                Gate::Xor(3, 4), // wire 5
            ],
            3,
            5,
        );
        let blocks = sched.to_bir_blocks().expect("pure boolean bridges");
        assert_eq!(blocks.blocks.len(), 1);
        let block = &blocks.blocks[0];
        assert_eq!(block.params, 3);
        // Prefix constant 1 at var 3; schedule wires 3, 4, 5 map to
        // vars 4, 5, 6 respectively. The NOT's XOR uses input 2 and var 3.
        assert_eq!(block.stmts.len(), 4);
        assert_eq!(block.stmts[0].kind, BIrStmt::One);
        assert_eq!(block.stmts[1].kind, BIrStmt::And(IRVarId(0), IRVarId(1)));
        assert_eq!(block.stmts[2].kind, BIrStmt::Xor(IRVarId(2), IRVarId(3)));
        assert_eq!(block.stmts[3].kind, BIrStmt::Xor(IRVarId(4), IRVarId(5)));
        let BIrTerminator::Jmp(target) = &block.terminator else {
            panic!("single-block circuit must return");
        };
        assert_eq!(target.args, [IRVarId(6)]);
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
