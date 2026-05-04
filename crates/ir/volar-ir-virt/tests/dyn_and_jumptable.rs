// @reliability: experimental
// @ai: assisted
//! Tests for two newly-unlocked features:
//!
//! 1. `IRBlockTargetId::Dyn` in `virtualize_ir` (was panicking before).
//! 2. `IRTerminator::JumpTable` in `movfuscate_ir` (was `unimplemented!`).
//! 3. The two passes applied together: virtualize a JumpTable IR then
//!    movfuscate the result.

use std::collections::BTreeMap;

use volar_fuzz::interpreter::ir::{bit_width, const_to_bits, eval_ir};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypes, IRVarId,
    PrimType,
};
use volar_ir_common::{Constant, Stmt};
use volar_ir_passes::movfuscate_ir;
use volar_ir_virt::{virtualize_ir, BytecodeForm, DispatchMode, VirtualizeConfig};

// ============================================================================
// Helpers
// ============================================================================

fn cfg_public() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

fn cfg_oblivious() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Oblivious,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

fn bits_to_u32(bits: &[bool]) -> u32 {
    bits.iter()
        .take(32)
        .enumerate()
        .fold(0u32, |acc, (i, &b)| if b { acc | (1 << i) } else { acc })
}

/// A 3-block module with a `JumpTable` dispatch in the entry block.
///
/// ```text
/// block 0  (params=[idx: _32]):
///   JumpTable { index: idx,
///     cases: { 0 → (Block(1),[]), 1 → (Block(2),[]) } }
/// block 1  (params=[]):
///   stmt: k = Const(10, _32)
///   Jmp(Return, [k])
/// block 2  (params=[]):
///   stmt: k = Const(20, _32)
///   Jmp(Return, [k])
/// ```
///
/// Inputs: one _32 value (0 → return 10, 1 → return 20).
fn jumptable_three_block() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let mut cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> = BTreeMap::new();
    cases.insert(
        Constant { hi: 0, lo: 0 },
        (IRBlockTargetId::Block(IRBlockId(1)), vec![]),
    );
    cases.insert(
        Constant { hi: 0, lo: 1 },
        (IRBlockTargetId::Block(IRBlockId(2)), vec![]),
    );

    let blocks = IRBlocks::new(vec![
        // block 0: dispatch on idx
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::JumpTable {
                index: IRVarId(0),
                cases,
            },
        },
        // block 1: return 10
        IRBlock {
            params: vec![],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 10 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        },
        // block 2: return 20
        IRBlock {
            params: vec![],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 20 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        },
    ]);
    (blocks, types)
}

/// A 2-block module where block 0 uses a `Dyn` jump target.
///
/// ```text
/// block 0  (params=[x: _32]):
///   stmt: target = Const(1, Block{[_32]})   — static ref to block 1
///   Jmp(Dyn(IRVarId(1)), [IRVarId(0)])
/// block 1  (params=[x: _32]):
///   Jmp(Return, [x])
/// ```
///
/// NOTE: `eval_ir` panics on Block-typed values, so this fixture is only used
/// for structural / no-panic testing of `virtualize_ir`.
fn dyn_two_block(types: &mut IRTypes) -> IRBlocks {
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));
    let block_ty = types.intern(IRType::Block { params: vec![u32_ty] });

    IRBlocks::new(vec![
        // block 0: Dyn jump to block 1 (encoded as Const(1, block_ty)).
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 1 }, block_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Dyn(IRVarId(1)),
                args: vec![IRVarId(0)],
            },
        },
        // block 1: return x
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        },
    ])
}

// ============================================================================
// Test A: movfuscate_ir handles JumpTable (no panic + structural)
// ============================================================================

#[test]
fn movfuscate_jumptable_produces_single_block() {
    let (blocks, mut types) = jumptable_three_block();

    let movf = movfuscate_ir(&blocks, &mut types);

    // Movfuscated output must be a single self-looping block.
    assert_eq!(movf.blocks.len(), 1, "movfuscation must produce exactly one block");
    assert!(movf.is_movfuscated(), "is_movfuscated() sanity check");
}

/// Structural check for movfuscate_ir on a JumpTable input.
/// The `eval_ir` interpreter does not fully support movfuscated IR (it evaluates
/// Poly bit-by-bit and treats Bit-typed selectors as 1-bit, which breaks the
/// GF scalar-multiplication semantics used in `emit_gate`).  This is a
/// pre-existing interpreter limitation shared by all IR movfuscation tests.
#[test]
fn movfuscate_jumptable_combined_block_shape() {
    let (blocks, mut types) = jumptable_three_block();

    let movf = movfuscate_ir(&blocks, &mut types);

    // 3 blocks → pc_width=2; state = [_32 for idx]; combined params = [Bit,Bit,_32].
    assert_eq!(movf.blocks[0].params.len(), 3, "pc(2) + state(1) params");
    // Return value: one _32 slot (the dispatch result is a _32 constant).
    // The combined block terminates with JumpCond → Return | self-loop.
    assert!(
        matches!(
            &movf.blocks[0].terminator,
            volar_ir::ir::IRTerminator::JumpCond {
                true_block: volar_ir::ir::IRBlockTargetId::Return,
                ..
            }
        ),
        "combined block terminator must be JumpCond(done, Return, self-loop)"
    );
}

// ============================================================================
// Test B: virtualize_ir handles Dyn jump (structural / no-panic)
// ============================================================================

#[test]
fn virtualize_dyn_target_no_panic() {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let blocks = dyn_two_block(&mut types);

    // Must not panic.
    let out = virtualize_ir(&blocks, &mut types, &cfg_public());

    assert_eq!(out.blocks_in, 2, "input had 2 blocks");
    assert!(out.n_handlers >= 1, "at least one handler must be emitted");
    assert!(
        out.blocks.blocks.len() > 2,
        "virtualized output should have more blocks than the input"
    );
}

// ============================================================================
// Test C: virtualize_ir (Public) + movfuscate_ir chained
// ============================================================================

/// Virtualize a JumpTable module, verify semantic equivalence on the virtualized
/// output, then movfuscate and verify the movfuscated output is well-formed.
#[test]
fn virtualize_then_movfuscate_jumptable() {
    let (blocks, mut types) = jumptable_three_block();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    // Reference semantics (original input).
    let idx0 = const_to_bits(&Constant { hi: 0, lo: 0 }, bit_width(u32_ty, &types));
    let idx1 = const_to_bits(&Constant { hi: 0, lo: 1 }, bit_width(u32_ty, &types));
    let ref0 = eval_ir(&blocks, &types, &[idx0.clone()]).expect("ref idx=0");
    let ref1 = eval_ir(&blocks, &types, &[idx1.clone()]).expect("ref idx=1");
    assert_eq!(bits_to_u32(&ref0[0]), 10);
    assert_eq!(bits_to_u32(&ref1[0]), 20);

    // Step 1: virtualize (Public dispatch emits JumpTable in DISPATCH block).
    let virt = virtualize_ir(&blocks, &mut types, &cfg_public());

    // Verify semantic equivalence on the virtualized output (entry params unchanged).
    let virt0 = eval_ir(&virt.blocks, &types, &[idx0.clone()]).expect("virt idx=0");
    let virt1 = eval_ir(&virt.blocks, &types, &[idx1.clone()]).expect("virt idx=1");
    assert_eq!(ref0, virt0, "virt idx=0 semantic mismatch");
    assert_eq!(ref1, virt1, "virt idx=1 semantic mismatch");

    // Step 2: movfuscate the virtualized output (exercises JumpTable in movfuscation).
    let movf = movfuscate_ir(&virt.blocks, &mut types);
    assert_eq!(movf.blocks.len(), 1, "movfuscation must produce exactly one block");
    assert!(movf.is_movfuscated(), "is_movfuscated() after virt+movf");
}

// ============================================================================
// Test D: DispatchMode::Oblivious (the combined path in one call)
// ============================================================================

/// `DispatchMode::Oblivious` internally chains `virtualize_ir` + `movfuscate_ir`.
/// This test verifies it now works end-to-end on a JumpTable module.
#[test]
fn virtualize_oblivious_jumptable_no_panic() {
    let (blocks, mut types) = jumptable_three_block();

    // Must not panic; output must be movfuscated (single self-looping block).
    let out = virtualize_ir(&blocks, &mut types, &cfg_oblivious());
    assert_eq!(out.blocks.blocks.len(), 1, "Oblivious dispatch must produce one combined block");
    assert!(out.blocks.is_movfuscated(), "is_movfuscated() in Oblivious mode");
}
