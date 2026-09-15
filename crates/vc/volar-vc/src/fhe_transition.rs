//! Stateless garbled-circuit transition seam around an imported FHE module.
//!
//! An imported no-std BinFHE artifact may use bounded `alloc` internally, but
//! its observable circuit boundary is much narrower: Boolean input wires,
//! Boolean output wires, and no retained Volar storage. After the host obtains
//! ciphertext/plaintext bits from the FHE module, this module rebuilds the
//! next fused GC circuit from *only* those output wires. It therefore drops
//! every module-local global/storage representation instead of accidentally
//! carrying it into the next garbled invocation.
//!
//! ```text
//! fused GC prefix -> imported FHE module boundary -> captured output wires
//!   -> reset module-local state -> fresh fused GC suffix
//! ```
//!
//! This is compiler plumbing, not a BinFHE encryption/decryption circuit.
//! `binfhe::boundary::PlanBoundary` owns typed host conversion; an imported
//! LLVM/WASM module must first lower to an actual `BIrBlocks` circuit before
//! it can be supplied here. The legacy `FheScheme`/TFHE surface is never used.
//! The next provider shape is documented in `docs/fhe/circuit-provider-abi.md`:
//! it will inline complete split-seed key-derivation and randomized conversion
//! circuits before calling this state-eliding transition seam.
//!
//! # Ledger
//!
//! See `docs/fhe/future-provider-integration-ledger.md`, especially
//! `FHE-PLUMB-GC-TRANSITION-01`.

use core::convert::Infallible;

use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::circuit::{BCircuit, CircuitFusionError};
use volar_ir::ir::IRVarId;
use volar_ir_common::Node;

/// Why an FHE-module circuit cannot be isolated/fused at a GC transition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FheTransitionError {
    /// The source is not a single fused circuit with a return terminator.
    NotFused(CircuitFusionError),
    /// Static initialization is module-local state. It must be consumed by the
    /// imported module and absent from its post-exchange transition circuit.
    HasPreinitializedStorage,
    /// A dynamic storage instruction remains in the imported module. The
    /// caller must split at this point rather than claim it has been reset.
    HasStorageOperation,
    /// A prefix does not expose exactly the parameter count expected by the
    /// FHE module boundary.
    BoundaryArity { expected: usize, actual: usize },
}

/// Validate and isolate an imported FHE boundary circuit.
///
/// On success the returned fused circuit carries only input parameters,
/// statement dataflow, and returned output wires. It has no `pre_init`; any
/// module-local globals/storage must already have been consumed before this
/// point. The caller can then append it to a strict GC transition with
/// [`fuse_after`].
// TODO(provider-ledger: FHE-PLUMB-GC-TRANSITION-01): connect this to a real
// imported BinFHE LLVM/WASM boundary after its ABI and static-memory profile
// are fixed. Current tests cover only generic Boolar state-elision semantics.
pub fn isolate_stateless_module<P: Clone>(
    blocks: &BIrBlocks<P>,
) -> Result<BCircuit<P>, FheTransitionError> {
    let circuit = BCircuit::try_from_ir(blocks).map_err(FheTransitionError::NotFused)?;
    if !circuit.pre_init.is_empty() {
        return Err(FheTransitionError::HasPreinitializedStorage);
    }
    if circuit.stmts.iter().any(|node| {
        matches!(
            node.kind,
            BIrStmt::StorageRead { .. }
                | BIrStmt::StorageWrite { .. }
                | BIrStmt::ActionStoreBit { .. }
        )
    }) {
        return Err(FheTransitionError::HasStorageOperation);
    }
    Ok(circuit)
}

/// Append an isolated FHE-module boundary after an already fused GC prefix.
///
/// The prefix's output wires become the module's parameters in order. The
/// resulting `BCircuit` is one circuit: its schedule/garbling observes the
/// transition as ordinary gate dataflow rather than a fresh independently
/// initialized storage/global context. The output list is replaced by the
/// module's remapped output list, which is the reset point for the next phase.
pub fn fuse_after<P: Clone>(
    mut prefix: BCircuit<P>,
    module: &BCircuit<P>,
) -> Result<BCircuit<P>, FheTransitionError> {
    if !module.pre_init.is_empty() {
        return Err(FheTransitionError::HasPreinitializedStorage);
    }
    if module.stmts.iter().any(|node| {
        matches!(
            node.kind,
            BIrStmt::StorageRead { .. }
                | BIrStmt::StorageWrite { .. }
                | BIrStmt::ActionStoreBit { .. }
        )
    }) {
        return Err(FheTransitionError::HasStorageOperation);
    }
    if prefix.outputs.len() != module.params as usize {
        return Err(FheTransitionError::BoundaryArity {
            expected: module.params as usize,
            actual: prefix.outputs.len(),
        });
    }

    let prefix_outputs = prefix.outputs.clone();
    let statement_base = prefix.var_space();
    for node in &module.stmts {
        let remapped = node
            .kind
            .clone()
            .map(
                &mut (),
                |_, source| -> Result<IRVarId, Infallible> {
                    let source = source.0 as usize;
                    Ok(if source < module.params as usize {
                        prefix_outputs[source]
                    } else {
                        IRVarId(statement_base + (source - module.params as usize) as u32)
                    })
                },
                |_, storage| -> Result<_, Infallible> { Ok(storage) },
            )
            .expect("infallible transition remap");
        prefix
            .stmts
            .push(Node::new(remapped, node.prov.clone(), node.side));
    }
    prefix.outputs = module
        .outputs
        .iter()
        .map(|source| {
            let source = source.0 as usize;
            if source < module.params as usize {
                prefix_outputs[source]
            } else {
                IRVarId(statement_base + (source - module.params as usize) as u32)
            }
        })
        .collect();
    Ok(prefix)
}

#[cfg(test)]
mod tests {
    use alloc::vec;
    use alloc::vec::Vec;

    use super::*;
    use volar_ir::boolar::{BIrBlock, BIrTarget, BIrTerminator};
    use volar_ir::ir::IRBlockTargetId;

    fn circuit(params: u32, stmts: Vec<BIrStmt>, outputs: Vec<u32>) -> BIrBlocks {
        BIrBlocks {
            blocks: vec![BIrBlock {
                params,
                stmts: stmts
                    .into_iter()
                    .map(|stmt| Node::new(stmt, (), None))
                    .collect(),
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: outputs.into_iter().map(IRVarId).collect(),
                }),
            }],
            pre_init: vec![],
        }
    }

    #[test]
    fn fuses_stateless_module_without_retaining_module_storage() {
        let prefix = isolate_stateless_module(&circuit(
            2,
            vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
            vec![2],
        ))
        .unwrap();
        // The module receives the prefix output as its one input and computes
        // NOT. Its sole output is remapped to the joined circuit's final wire.
        let module =
            isolate_stateless_module(&circuit(1, vec![BIrStmt::Not(IRVarId(0))], vec![1])).unwrap();
        let fused = fuse_after(prefix, &module).unwrap();
        assert_eq!(fused.params, 2);
        assert_eq!(fused.stmts.len(), 2);
        assert!(fused.pre_init.is_empty());
        assert_eq!(fused.outputs, vec![IRVarId(3)]);
    }

    #[test]
    fn rejects_module_state_instead_of_silently_carrying_it() {
        let mut stateful = circuit(1, vec![], vec![0]);
        stateful.pre_init.push(volar_ir::boolar::BIrPreInitSegment {
            storage: volar_ir_common::StorageId(0),
            lane: volar_ir::boolar::LaneId(0),
            addr: vec![],
            data: vec![true],
        });
        assert!(matches!(
            isolate_stateless_module(&stateful),
            Err(FheTransitionError::HasPreinitializedStorage),
        ));
    }
}
