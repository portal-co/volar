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
use alloc::vec;

use volar_ir::boolar::{BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};
use volar_mpc::{Gate, GateSchedule, GramStorageSpec};

/// Why a [`BIrBlocks`] could not be compiled to a [`GateSchedule`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScheduleError {
    /// The circuit is not in fused single-block form (`is_circuit()` false).
    NotACircuit,
    /// The `Return` terminator carries zero output wires (a circuit must
    /// return at least one bit to reveal).
    NoOutput,
    /// A statement the schedule shape cannot express (oracle/action call,
    /// RNG, or a projected-bit form). These are not pure boolean gates and
    /// must be lowered away before scheduling. (`StorageRead`/`StorageWrite`
    /// ARE expressible: they schedule to GRAM storage gates.)
    UnsupportedStmt,
    /// A storage read/write had a symbolic (non-constant) address wire: the
    /// GRAM driver runs a concrete ORAM access, so every address bit must be
    /// a compile-time-known constant. Symbolic addressing needs the MUX/GRAM
    /// symbolic-address layer instead.
    SymbolicStorageAddress,
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
                write!(f, "circuit contains a non-boolean statement (oracle/action/rng)")
            }
            ScheduleError::SymbolicStorageAddress => {
                write!(f, "storage address is symbolic; GRAM storage needs concrete address bits")
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

    // Output wires: the Return target's arguments, in order. Keep the raw
    // var ids here; they are resolved to *wires* (through `stmt_wire`) only
    // after the statement loop, because multi-gate statements (e.g. `Or`)
    // make a statement's result wire differ from its raw var id.
    let output_args: Vec<volar_ir::ir::IRVarId> = match &block.terminator {
        BIrTerminator::Jmp(target) if target.block == IRBlockTargetId::Return => {
            if target.args.is_empty() {
                return Err(ScheduleError::NoOutput);
            }
            target.args.clone()
        }
        _ => return Err(ScheduleError::NotACircuit),
    };

    // Each statement defines wire num_inputs + (schedule position). Because
    // Or expands to multiple gates, statement i does *not* map to gate i; we
    // track the wire index each statement's result landed on so later
    // references resolve correctly.
    let mut gates: Vec<Gate> = Vec::new();
    // stmt ordinal -> wire index holding its result.
    let mut stmt_wire: Vec<usize> = Vec::with_capacity(block.stmts.len());
    // Per-wire constant value, for wires that are a compile-time-known
    // constant (`Zero`/`One`, and `Not` of a constant). Storage addresses are
    // resolved against this: a storage op's address bits must be constant so
    // the cell is concrete (the GRAM driver runs a concrete ORAM access).
    let mut wire_const: Vec<Option<bool>> = vec![None; num_inputs];
    // GRAM storage spaces, in first-use `StorageId` order (the gate's
    // `storage` field indexes this). Per space, a `cell_map` folds each
    // distinct (possibly huge, sparsely-touched) memory address to a compact
    // consecutive ORAM block index.
    let mut storage_ids: Vec<StorageId> = Vec::new();
    let mut storage_cell_maps: Vec<alloc::collections::BTreeMap<u64, u64>> = Vec::new();
    let mut access_count: u64 = 0;
    // Resolve (or register) a StorageId to its storage index.
    let mut storage_index =
        |sid: StorageId,
         ids: &mut Vec<StorageId>,
         maps: &mut Vec<alloc::collections::BTreeMap<u64, u64>>| -> usize {
            match ids.iter().position(|&s| s == sid) {
                Some(i) => i,
                None => {
                    ids.push(sid);
                    maps.push(alloc::collections::BTreeMap::new());
                    ids.len() - 1
                }
            }
        };
    // Compress a memory address to its compact ORAM block, assigning the next
    // free block on first touch.
    let compress = |mem_addr: u64, map: &mut alloc::collections::BTreeMap<u64, u64>| -> u64 {
        let next = map.len() as u64;
        *map.entry(mem_addr).or_insert(next)
    };
    // Resolve a source var (input or earlier stmt result) to a wire index,
    // then fold an address-bit wire vector to a concrete cell index, failing
    // if any bit is non-constant.
    let concrete_cell = |addr: &[IRVarId],
                         wire_const: &Vec<Option<bool>>,
                         stmt_wire: &Vec<usize>| -> Result<u64, ScheduleError> {
        let mut cell = 0u64;
        for (bit, v) in addr.iter().enumerate() {
            let raw = v.0 as usize;
            let w = if raw < num_inputs {
                raw
            } else {
                stmt_wire
                    .get(raw - num_inputs)
                    .copied()
                    .ok_or(ScheduleError::DanglingWire)?
            };
            match wire_const.get(w).copied().flatten() {
                Some(true) => cell |= 1u64 << bit,
                Some(false) => {}
                None => return Err(ScheduleError::SymbolicStorageAddress),
            }
        }
        Ok(cell)
    };

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
        // Default: the wire this statement defines is not a known constant.
        let mut result_const: Option<bool> = None;
        match &stmt.kind {
            BIrStmt::Zero => {
                gates.push(Gate::Zero);
                stmt_wire.push(next_wire);
                result_const = Some(false);
            }
            BIrStmt::One => {
                gates.push(Gate::One);
                stmt_wire.push(next_wire);
                result_const = Some(true);
            }
            BIrStmt::Xor(a, b) => {
                let wa = wire_of(*a, &stmt_wire)?;
                let wb = wire_of(*b, &stmt_wire)?;
                gates.push(Gate::Xor(wa, wb));
                stmt_wire.push(next_wire);
                result_const = match (wire_const[wa], wire_const[wb]) {
                    (Some(x), Some(y)) => Some(x ^ y),
                    _ => None,
                };
            }
            BIrStmt::And(a, b) => {
                let wa = wire_of(*a, &stmt_wire)?;
                let wb = wire_of(*b, &stmt_wire)?;
                gates.push(Gate::And(wa, wb));
                stmt_wire.push(next_wire);
                result_const = match (wire_const[wa], wire_const[wb]) {
                    (Some(x), Some(y)) => Some(x & y),
                    _ => None,
                };
            }
            BIrStmt::Not(a) => {
                let wa = wire_of(*a, &stmt_wire)?;
                gates.push(Gate::Not(wa));
                stmt_wire.push(next_wire);
                result_const = wire_const[wa].map(|x| !x);
            }
            BIrStmt::Or(a, b) => {
                // a | b  ==  !(!a & !b). Reserve the result wire for the final
                // Not; intermediates are schedule-internal.
                let wa = wire_of(*a, &stmt_wire)?;
                let wb = wire_of(*b, &stmt_wire)?;
                result_const = match (wire_const[wa], wire_const[wb]) {
                    (Some(x), Some(y)) => Some(x | y),
                    _ => None,
                };
                let base = num_inputs + gates.len();
                gates.push(Gate::Not(wa)); // base + 0
                gates.push(Gate::Not(wb)); // base + 1
                gates.push(Gate::And(base, base + 1)); // base + 2
                gates.push(Gate::Not(base + 2)); // base + 3 == result
                stmt_wire.push(base + 3);
            }
            BIrStmt::StorageRead { storage, addr, .. } => {
                let mem_addr = concrete_cell(addr, &wire_const, &stmt_wire)?;
                let si = storage_index(*storage, &mut storage_ids, &mut storage_cell_maps);
                let cell = compress(mem_addr, &mut storage_cell_maps[si]);
                access_count += 1;
                gates.push(Gate::StorageRead {
                    storage: si,
                    cell,
                    access: access_count,
                });
                stmt_wire.push(next_wire);
            }
            BIrStmt::StorageWrite { storage, src, addr, .. } => {
                let mem_addr = concrete_cell(addr, &wire_const, &stmt_wire)?;
                let si = storage_index(*storage, &mut storage_ids, &mut storage_cell_maps);
                let cell = compress(mem_addr, &mut storage_cell_maps[si]);
                let wsrc = wire_of(*src, &stmt_wire)?;
                access_count += 1;
                gates.push(Gate::StorageWrite {
                    storage: si,
                    cell,
                    src: wsrc,
                    access: access_count,
                });
                stmt_wire.push(next_wire);
            }
            // Non-boolean statements cannot be scheduled into a pure GC.
            _ => return Err(ScheduleError::UnsupportedStmt),
        }
        // `wire_const` is indexed by *wire index*, but multi-gate statements
        // (e.g. `Or`, which expands to four gates) create several wires per
        // statement. Grow the table to cover every wire created so far, then
        // record this statement's result wire's const-ness — otherwise a later
        // gate referencing a multi-gate statement's output indexes out of
        // bounds. Intermediate expansion wires are left `None` (they are
        // schedule-internal and never looked up).
        wire_const.resize(num_inputs + gates.len(), None);
        let result_wire = *stmt_wire.last().expect("each stmt defines a wire");
        wire_const[result_wire] = result_const;
    }

    // Resolve the Return arguments to wires now that `stmt_wire` is complete.
    let outputs: Vec<usize> = output_args
        .iter()
        .map(|a| wire_of(*a, &stmt_wire))
        .collect::<Result<_, _>>()?;
    let output = outputs[0];

    // Size each storage space's ORAM by the *number of distinct cells* (the
    // compressed address space); the gates already carry the compact block.
    let storages: Vec<GramStorageSpec> = storage_cell_maps
        .iter()
        .map(|map| {
            let num_cells = (map.len() as u64).max(1);
            // Smallest levels with 2^(levels-1) >= num_cells (levels >= 1).
            let mut levels = 1usize;
            while (1u64 << (levels - 1)) < num_cells {
                levels += 1;
            }
            GramStorageSpec { num_cells, levels }
        })
        .collect();

    Ok(GateSchedule {
        num_inputs,
        gates,
        output,
        outputs: Some(outputs),
        storages,
    })
}
