// @reliability: experimental
// @ai: assisted
//! The real WAT loop guest lowered to a movfuscated step circuit, and the
//! precise blocker for running it through the symbolic ORAM.
//!
//! The counting-loop guest (`f(n) = n + (n-1) + ... + 1`) lowers cleanly
//! through waffle -> vc-vaffle -> IRBlocks -> movfuscate -> `lower_to_circuit_ir(
//! WithTerminationFlag)` to a single `is_circuit` step circuit, and to boolar
//! with 492 storage ops at 34-bit addresses. **But those ops span THREE
//! read-write storage spaces** (the linear-memory stack, the movfuscated
//! bytecode stream, and a small ABI space), and `storage_to_oram` currently
//! lowers a SINGLE space (`OramLowerConfig::storage`), returning
//! `MixedStorage` for the step circuit.
//!
//! This is the **storage-ownership management** the plan flags as not-done:
//! running the WAT loop guest needs a multi-space `storage_to_oram` (one ORAM
//! per space, or read-only spaces as constant tables) plus per-space narrowing
//! and a shared-ORAM loop driver. The narrowing mechanism (`narrow_bits`) and
//! the single-space symbolic ORAM are proven; the multi-space composition is
//! the remaining integration.

use volar_ir::boolar::BIrStmt;
use volar_ir::ir::{IRType, IRTypeId};
use volar_ir_common::Type;
use volar_vc::oram_lower::{OramLowerConfig, OramLowerError, storage_to_oram};

const LOOP_WAT: &str = r#"(module
  (func $f (export "f") (param $n i32) (result i32)
    (local $acc i32)
    (local $i i32)
    (local.set $i (local.get $n))
    (block $exit
      (loop $l
        (br_if $exit (i32.eqz (local.get $i)))
        (local.set $acc (i32.add (local.get $acc) (local.get $i)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $l)))
    (local.get $acc)))"#;

#[test]
fn wat_loop_guest_lowers_to_step_circuit_multi_space() {
    let bytes: &'static [u8] = Box::leak(wat::parse_str(LOOP_WAT).unwrap().into_boxed_slice());
    let module = portal_pc_waffle_frontend::from_wasm_bytes(
        bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("waffle parse");
    let vc = volar_vaffle_target::VcConfig::new()
        .with_call("f", vec![volar_vaffle_target::VcArg::Private]);
    let mut target =
        volar_vaffle_target::VaffleTarget::with_pointer_width(vaffle::PointerWidth::Bits32);
    let (errors, _artifact) = volar_vaffle_target::lower_waffle_module_with_vc(
        &module,
        &mut target,
        &volar_vaffle_target::WaffleImportConfig::default(),
        &vc,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);

    // Optimize to fixpoint, movfuscate to one self-loop, lower to a step circuit.
    let mut ib = blocks.clone();
    loop {
        let c1 = volar_ir_opt::ir::fold_ir_blocks(&mut ib, &types);
        let c2 = volar_ir_opt::store_forward::store_forward_ir_blocks(&mut ib, &types);
        if !c1 && !c2 {
            break;
        }
    }
    let (movf, _b, _acc) = volar_ir_passes::movfuscate_ir_with_boundary(&ib, &mut types);
    let bit_ty: IRTypeId = types.intern(IRType::Primitive(Type::Bit));
    let step = volar_ir_passes::lower_to_circuit_ir(
        &movf,
        &bit_ty,
        1,
        volar_ir_passes::LoweringMode::WithTerminationFlag,
    );
    assert!(step.is_circuit(), "loop guest movfuscates to a step circuit");

    // To boolar: a single block with storage ops across multiple spaces.
    let boolar = volar_ir_passes::lower_ir_to_boolar(&step, &types);
    let b0 = &boolar.blocks[0];
    let storage_ops = b0
        .stmts
        .iter()
        .filter(|s| matches!(s.kind, BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. }))
        .count();
    assert!(storage_ops > 0, "step circuit has storage (spill) ops");

    // The storage ops span multiple read-write spaces (stack, bytecode, ABI).
    let mut spaces: Vec<u32> = b0
        .stmts
        .iter()
        .filter_map(|s| match &s.kind {
            BIrStmt::StorageRead { storage, .. } => Some(storage.0),
            BIrStmt::StorageWrite { storage, .. } => Some(storage.0),
            _ => None,
        })
        .collect();
    spaces.sort();
    spaces.dedup();
    assert!(spaces.len() > 1, "the WAT loop guest uses multiple storage spaces: {spaces:?}");

    // The single-space symbolic-ORAM lowering rejects the mixed-storage step
    // circuit — the documented storage-ownership blocker.
    let res = storage_to_oram(
        &boolar,
        &OramLowerConfig {
            storage: volar_ir::ir::StorageId(spaces[0]),
            levels: 13,
            bucket_size: 2,
            max_stash: 2 * 13 + 2 + 16,
            secure: false,
            narrow_bits: Some(12),
        },
    );
    assert!(
        matches!(res, Err(OramLowerError::MixedStorage)),
        "single-space lowering must reject the multi-space step circuit"
    );
}
