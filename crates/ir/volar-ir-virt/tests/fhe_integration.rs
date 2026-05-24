// @reliability: experimental
// @ai: assisted
//! Integration test: virtualise + movfuscate composes cleanly for BIR,
//! producing a shape the FHE flat weaver can consume.
//!
//! # Scope and limitations (v1)
//!
//! The plan asks for a round-trip through `weave_fhe_flat_bir` + the
//! Rust/TS/C printers.  Two upstream limitations restrict what we can
//! test here without further changes:
//!
//! 1. `weave_fhe_flat_bir` requires `circuit.is_circuit()` — that is,
//!    a single block ending in `Jmp(Return)`.  `movfuscate_biir`
//!    produces a single self-looping block terminating with
//!    `CondJmp(done, Return, Block(0))`, which is **not** an
//!    `is_circuit()` shape.  As a result, `weave_fhe_flat` already
//!    only accepts inputs that are purely combinational single-block
//!    circuits.
//! 2. `movfuscate_ir` does not (yet) support `IRTerminator::JumpTable`,
//!    so `virtualize_ir` + Oblivious cannot complete for multi-handler
//!    inputs until movfuscate_ir grows that support.
//!
//! We therefore restrict this integration test to the BIR Oblivious
//! path and verify composition at the shape level: the output is a
//! well-formed movfuscated BIR module.  The full compile-check pipeline
//! (FHE weave → Rust / TS / C print → compile) is tracked as a
//! follow-up.

use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{IRBlockId, IRBlockTargetId, IRVarId},
};
use volar_ir_virt::{virtualize_bir, BytecodeForm, DispatchMode, VirtualizeConfig};
use volar_weaver::fhe::derive_storage_config;

fn cfg_oblivious() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Oblivious,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

/// Mini BIR: `block 0` XORs its two params, `block 1` NOTs the first
/// param and returns.  Both blocks use `params = 2` so the virtualiser
/// accepts them.
fn mini_bir() -> BIrBlocks {
    BIrBlocks { blocks: vec![
        BIrBlock {
            params: 2,
            stmts: vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(2), IRVarId(1)],
            }),
        },
        BIrBlock {
            params: 2,
            stmts: vec![BIrStmt::Not(IRVarId(0))],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        },
    ], pre_init: vec![] }
}

#[test]
fn virtualize_bir_oblivious_is_movfuscated_and_storage_derivable() {
    let blocks = mini_bir();
    let virt = virtualize_bir(&blocks, &cfg_oblivious());

    // Composition shape: Oblivious dispatch routes through
    // `movfuscate_biir`, so the output is a single self-looping block.
    assert!(
        virt.blocks.is_movfuscated(),
        "expected a single movfuscated block after Oblivious dispatch; \
         got {} blocks",
        virt.blocks.blocks.len()
    );

    // The rendered form must still be a valid input to the FHE
    // storage-config deriver.  This is the hand-off point between
    // `volar-ir-virt` and the weaver.
    let storage = derive_storage_config(&virt.blocks);
    // The virtualisation pass writes bytecode rows during setup, so at
    // least one (StorageId, bit_width) key must have been derived.
    assert!(
        !storage.sizes.is_empty(),
        "derive_storage_config must detect at least the bytecode \
         storage writes emitted by the InIr form"
    );
}
