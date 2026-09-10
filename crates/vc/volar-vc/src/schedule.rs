// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Compile a circuit-fused [`BIrBlocks`] into a [`GateSchedule`].
//!
//! The MPC session layer (`volar-mpc`) executes a [`GateSchedule`]: inputs on
//! wires `0 .. num_inputs-1`, gate `k` defining wire `num_inputs + k`, one
//! output wire. This module re-expresses a weaver-produced boolean circuit in
//! that shape so *any* circuit that satisfies `is_circuit()` (a single block
//! with a `Return` terminator) can run through the two-party session — not
//! just schedules written by hand.
//!
//! Wire numbering matches the weaver's convention: the block's `params` are
//! the circuit inputs in order, statement `i` produces wire `params + i`, and
//! the `Return` terminator's single argument is the output wire. `Or` gates
//! are expanded to `Not`/`And` by De Morgan (`a | b = !(!a & !b)`), matching
//! `volar_weaver`'s `expand_ors`.

use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::IRBlockTargetId;
use volar_mpc::{Gate, GateSchedule};

/// Why a [`BIrBlocks`] could not be compiled to a [`GateSchedule`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScheduleError {
    /// The circuit is not in fused single-block form (`is_circuit()` false).
    NotACircuit,
    /// The `Return` terminator carries zero output wires (a circuit must
    /// return at least one bit to reveal).
    NoOutput,
    /// A statement the schedule shape cannot express (oracle/action call,
    /// RNG, storage read/write, or a projected-bit form). These are not pure
    /// boolean gates and must be lowered away before scheduling.
    UnsupportedStmt,
    /// A gate references a wire that has not been defined yet (dangling).
    DanglingWire,
}

impl core::fmt::Display for ScheduleError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ScheduleError::NotACircuit => write!(f, "circuit is not fused single-block form"),
            ScheduleError::NoOutput => {
                write!(f, "circuit return carries no output wires")
            }
            ScheduleError::UnsupportedStmt => {
                write!(f, "circuit contains a non-boolean statement (oracle/action/rng/storage)")
            }
            ScheduleError::DanglingWire => write!(f, "gate references a not-yet-defined wire"),
        }
    }
}

/// Compile `circuit` to a [`GateSchedule`].
///
/// `circuit` must satisfy `is_circuit()`: exactly one block whose terminator
/// is `Jmp` to [`IRBlockTargetId::Return`]. The block's `params` become
/// `num_inputs`; each boolean statement becomes one gate; the `Return`
/// arguments become the output wires, in order (`output` is the first,
/// `outputs` the full list). A multi-bit guest result (e.g. an `i32`)
/// therefore schedules as one output wire per result bit. `Or` is expanded
/// by De Morgan into four gates (two `Not` + one `And` + one `Not`), so the
/// resulting schedule's AND count is the number of `And` statements plus the
/// number of `Or` statements.
pub fn compile_schedule<P: Clone>(circuit: &BIrBlocks<P>) -> Result<GateSchedule, ScheduleError> {
    if !circuit.is_circuit() || circuit.blocks.len() != 1 {
        return Err(ScheduleError::NotACircuit);
    }
    let block = &circuit.blocks[0];
    let num_inputs = block.params as usize;

    // Output wires: the Return target's arguments, in order.
    let outputs: Vec<usize> = match &block.terminator {
        BIrTerminator::Jmp(target) if target.block == IRBlockTargetId::Return => {
            if target.args.is_empty() {
                return Err(ScheduleError::NoOutput);
            }
            target.args.iter().map(|a| a.0 as usize).collect()
        }
        _ => return Err(ScheduleError::NotACircuit),
    };
    let output = outputs[0];

    // Each statement defines wire num_inputs + (schedule position). Because
    // Or expands to multiple gates, statement i does *not* map to gate i; we
    // track the wire index each statement's result landed on so later
    // references resolve correctly.
    let mut gates: Vec<Gate> = Vec::new();
    // stmt ordinal -> wire index holding its result.
    let mut stmt_wire: Vec<usize> = Vec::with_capacity(block.stmts.len());

    // Resolve a source var (input or earlier stmt result) to a wire index.
    let wire_of = |v: volar_ir::ir::IRVarId, stmt_wire: &Vec<usize>| -> Result<usize, ScheduleError> {
        let raw = v.0 as usize;
        if raw < num_inputs {
            Ok(raw)
        } else {
            let ord = raw - num_inputs;
            stmt_wire.get(ord).copied().ok_or(ScheduleError::DanglingWire)
        }
    };

    for stmt in &block.stmts {
        let next_wire = num_inputs + gates.len();
        match &stmt.kind {
            BIrStmt::Zero => {
                gates.push(Gate::Zero);
                stmt_wire.push(next_wire);
            }
            BIrStmt::One => {
                gates.push(Gate::One);
                stmt_wire.push(next_wire);
            }
            BIrStmt::Xor(a, b) => {
                let wa = wire_of(*a, &stmt_wire)?;
                let wb = wire_of(*b, &stmt_wire)?;
                gates.push(Gate::Xor(wa, wb));
                stmt_wire.push(next_wire);
            }
            BIrStmt::And(a, b) => {
                let wa = wire_of(*a, &stmt_wire)?;
                let wb = wire_of(*b, &stmt_wire)?;
                gates.push(Gate::And(wa, wb));
                stmt_wire.push(next_wire);
            }
            BIrStmt::Not(a) => {
                let wa = wire_of(*a, &stmt_wire)?;
                gates.push(Gate::Not(wa));
                stmt_wire.push(next_wire);
            }
            BIrStmt::Or(a, b) => {
                // a | b  ==  !(!a & !b). Reserve the result wire for the final
                // Not; intermediates are schedule-internal.
                let wa = wire_of(*a, &stmt_wire)?;
                let wb = wire_of(*b, &stmt_wire)?;
                let base = num_inputs + gates.len();
                gates.push(Gate::Not(wa)); // base + 0
                gates.push(Gate::Not(wb)); // base + 1
                gates.push(Gate::And(base, base + 1)); // base + 2
                gates.push(Gate::Not(base + 2)); // base + 3 == result
                stmt_wire.push(base + 3);
            }
            // Non-boolean statements cannot be scheduled into a pure GC.
            _ => return Err(ScheduleError::UnsupportedStmt),
        }
    }

    Ok(GateSchedule {
        num_inputs,
        gates,
        output,
        outputs: Some(outputs),
    })
}
