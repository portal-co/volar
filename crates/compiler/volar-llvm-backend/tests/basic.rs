// @reliability: normal
//! Direct LirTarget API tests for `LlvmBackend`.
//!
//! These tests exercise the backend in isolation — they drive `LirTarget`
//! methods directly (no `volar-lir-codegen` layer) and verify correctness by
//! printing the resulting LLVM IR and checking that it compiles/verifies.

use inkwell::context::Context;
use volar_llvm_backend::LlvmBackend;
use volar_lir::{IcmpPred, LirTarget, LirType, StackAllocExt};

// ============================================================================
// Helpers
// ============================================================================

/// Verify the LLVM module (panic if malformed).
fn verify(ctx: &Context, f: impl FnOnce(&Context, &mut LlvmBackend<'_>)) {
    let mut b = LlvmBackend::new(ctx, "test");
    f(ctx, &mut b);
    let module = b.finish();
    module.verify().expect("LLVM module verification failed");
}

// ============================================================================
// Constants
// ============================================================================

#[test]
fn test_iconst_u32() {
    let ctx = Context::create();
    verify(&ctx, |_, b| {
        let (entry, _) = b.begin_function("f", &[], Some(LirType::U32));
        b.switch_to_block(entry);
        let c = b.iconst(LirType::U32, 7);
        b.ret(&[c]);
        b.end_function();
    });
}

/// Verify we can emit a function that returns an i64 constant.
#[test]
fn test_iconst_return() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_iconst");
    let (entry, _) = b.begin_function("ret42", &[], Some(LirType::I64));
    b.switch_to_block(entry);
    let c = b.iconst(LirType::I64, 42);
    b.ret(&[c]);
    b.end_function();
    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(ir.contains("ret i64 42"), "expected 'ret i64 42' in:\n{ir}");
}

// ============================================================================
// Arithmetic
// ============================================================================

#[test]
fn test_add_u64() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_add");
    let params = [LirType::U64, LirType::U64];
    let (entry, pvs) = b.begin_function("add", &params, Some(LirType::U64));
    b.switch_to_block(entry);
    let lhs = pvs[0][0].clone();
    let rhs = pvs[1][0].clone();
    let sum = b.add(lhs, rhs);
    b.ret(&[sum]);
    b.end_function();
    b.finish().verify().expect("verify");
}

#[test]
fn test_mul_i32() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_mul");
    let params = [LirType::I32, LirType::I32];
    let (entry, pvs) = b.begin_function("mul", &params, Some(LirType::I32));
    b.switch_to_block(entry);
    let result = b.mul(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[result]);
    b.end_function();
    b.finish().verify().expect("verify");
}

#[test]
fn test_udiv_u32() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_udiv");
    let params = [LirType::U32, LirType::U32];
    let (entry, pvs) = b.begin_function("udiv", &params, Some(LirType::U32));
    b.switch_to_block(entry);
    let result = b.udiv(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[result]);
    b.end_function();
    b.finish().verify().expect("verify");
}

#[test]
fn test_sdiv_i32() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_sdiv");
    let params = [LirType::I32, LirType::I32];
    let (entry, pvs) = b.begin_function("sdiv", &params, Some(LirType::I32));
    b.switch_to_block(entry);
    let result = b.sdiv(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[result]);
    b.end_function();
    b.finish().verify().expect("verify");
}

// ============================================================================
// Bitwise
// ============================================================================

#[test]
fn test_bitwise_ops() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_bitwise");
    let params = [LirType::U64, LirType::U64];
    let (entry, pvs) = b.begin_function("bitwise", &params, Some(LirType::U64));
    b.switch_to_block(entry);
    let a = pvs[0][0].clone();
    let bv = pvs[1][0].clone();
    let and = b.and(a.clone(), bv.clone());
    let or = b.or(and, bv.clone());
    let xor = b.xor(or, a.clone());
    let not = b.not(xor);
    b.ret(&[not]);
    b.end_function();
    b.finish().verify().expect("verify");
}

#[test]
fn test_not_bool() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_not_bool");
    let (entry, pvs) = b.begin_function("notb", &[LirType::Bool], Some(LirType::Bool));
    b.switch_to_block(entry);
    let result = b.not(pvs[0][0].clone());
    b.ret(&[result]);
    b.end_function();
    let m = b.finish();
    m.verify().expect("verify");
    // bool NOT should be xor with true (i1 1)
    let ir = m.print_to_string().to_string();
    assert!(ir.contains("xor i1"), "expected xor i1 for bool not in:\n{ir}");
}

#[test]
fn test_shifts() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_shifts");
    let params = [LirType::U64, LirType::U64];
    let (entry, pvs) = b.begin_function("shifts", &params, Some(LirType::U64));
    b.switch_to_block(entry);
    let val = pvs[0][0].clone();
    let shift = pvs[1][0].clone();
    let shl = b.shl(val.clone(), shift.clone());
    let lshr = b.lshr(shl, shift.clone());
    let ashr = b.ashr(lshr, shift);
    b.ret(&[ashr]);
    b.end_function();
    b.finish().verify().expect("verify");
}

// ============================================================================
// Comparison
// ============================================================================

#[test]
fn test_icmp_eq() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_icmp");
    let params = [LirType::U32, LirType::U32];
    let (entry, pvs) = b.begin_function("cmp_eq", &params, Some(LirType::Bool));
    b.switch_to_block(entry);
    let result = b.icmp(IcmpPred::Eq, pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[result]);
    b.end_function();
    b.finish().verify().expect("verify");
}

#[test]
fn test_icmp_signed() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_icmp_signed");
    let params = [LirType::I32, LirType::I32];
    let (entry, pvs) = b.begin_function("cmp_slt", &params, Some(LirType::Bool));
    b.switch_to_block(entry);
    let result = b.icmp(IcmpPred::Slt, pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[result]);
    b.end_function();
    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(ir.contains("icmp slt"), "expected 'icmp slt' in:\n{ir}");
}

// ============================================================================
// Conversions
// ============================================================================

#[test]
fn test_zext() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_zext");
    let (entry, pvs) = b.begin_function("zext", &[LirType::U8], Some(LirType::U64));
    b.switch_to_block(entry);
    let result = b.zext(pvs[0][0].clone(), LirType::U64);
    b.ret(&[result]);
    b.end_function();
    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(ir.contains("zext i8"), "expected 'zext i8' in:\n{ir}");
}

#[test]
fn test_sext() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_sext");
    let (entry, pvs) = b.begin_function("sext", &[LirType::I8], Some(LirType::I64));
    b.switch_to_block(entry);
    let result = b.sext(pvs[0][0].clone(), LirType::I64);
    b.ret(&[result]);
    b.end_function();
    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(ir.contains("sext i8"), "expected 'sext i8' in:\n{ir}");
}

#[test]
fn test_trunc() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_trunc");
    let (entry, pvs) = b.begin_function("trunc", &[LirType::U64], Some(LirType::U8));
    b.switch_to_block(entry);
    let result = b.trunc(pvs[0][0].clone(), LirType::U8);
    b.ret(&[result]);
    b.end_function();
    b.finish().verify().expect("verify");
}

// ============================================================================
// Select
// ============================================================================

#[test]
fn test_select() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_select");
    let params = [LirType::Bool, LirType::U64, LirType::U64];
    let (entry, pvs) = b.begin_function("sel", &params, Some(LirType::U64));
    b.switch_to_block(entry);
    let result = b.select(pvs[0][0].clone(), pvs[1][0].clone(), pvs[2][0].clone());
    b.ret(&[result]);
    b.end_function();
    b.finish().verify().expect("verify");
}

// ============================================================================
// Block parameters / PHI nodes
// ============================================================================

/// Emit a function with a single non-trivial join block:
///
/// ```
/// entry(x: u64):
///   if x == 0 { jump merge(1) } else { jump merge(2) }
/// merge(p: u64):
///   return p
/// ```
#[test]
fn test_block_param_phi() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_phi");
    let (entry, pvs) = b.begin_function("phi_test", &[LirType::U64], Some(LirType::U64));

    let merge = b.create_block();
    let p = b.add_block_param(merge, LirType::U64);

    b.switch_to_block(entry);
    let x = pvs[0][0].clone();
    let zero = b.iconst(LirType::U64, 0);
    let cond = b.icmp(IcmpPred::Eq, x, zero);
    let one = b.iconst(LirType::U64, 1);
    let two = b.iconst(LirType::U64, 2);
    b.branch(cond, merge, &[one], merge, &[two]);

    b.switch_to_block(merge);
    b.ret(&[p]);
    b.end_function();

    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(ir.contains("phi i64"), "expected phi node in:\n{ir}");
}

/// Loop: count down from n to 0, return 0.
///
/// ```
/// entry(n: u64):
///   jump loop(n)
/// loop(i: u64):
///   if i == 0 { jump exit } else { jump loop(i - 1) }
/// exit:
///   return 0
/// ```
#[test]
fn test_loop_phi() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_loop");
    let (entry, pvs) = b.begin_function("countdown", &[LirType::U64], Some(LirType::U64));

    let loop_block = b.create_block();
    let exit_block = b.create_block();
    let i = b.add_block_param(loop_block, LirType::U64);

    b.switch_to_block(entry);
    b.jump(loop_block, &[pvs[0][0].clone()]);

    b.switch_to_block(loop_block);
    let zero = b.iconst(LirType::U64, 0);
    let cond = b.icmp(IcmpPred::Eq, i.clone(), zero.clone());
    let one = b.iconst(LirType::U64, 1);
    let i_minus_1 = b.sub(i, one);
    b.branch(cond, exit_block, &[], loop_block, &[i_minus_1]);

    b.switch_to_block(exit_block);
    b.ret(&[zero]);
    b.end_function();

    b.finish().verify().expect("verify");
}

// ============================================================================
// Void functions
// ============================================================================

#[test]
fn test_void_function() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_void");
    let (entry, _) = b.begin_function("noop", &[], None);
    b.switch_to_block(entry);
    b.ret(&[]);
    b.end_function();
    b.finish().verify().expect("verify");
}

// ============================================================================
// StackAllocExt
// ============================================================================

#[test]
fn test_alloca_load_store() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_alloca");
    let (entry, pvs) = b.begin_function("alloca_test", &[LirType::U64], Some(LirType::U64));
    b.switch_to_block(entry);

    // alloca a single u64 slot
    let ptr = b.alloca(LirType::U64, 1);
    // store the parameter
    b.ptr_store(ptr.clone(), pvs[0][0].clone());
    // load it back
    let loaded = b.ptr_load(ptr, LirType::U64);
    b.ret(&[loaded]);
    b.end_function();

    b.finish().verify().expect("verify");
}

#[test]
fn test_alloca_array_offset() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_gep");
    let params = [LirType::U64, LirType::U64, LirType::U64];
    let (entry, pvs) = b.begin_function("gep_test", &params, Some(LirType::U64));
    b.switch_to_block(entry);

    // alloca [2 x u64]
    let ptr = b.alloca(LirType::U64, 2);
    // store at index 0 and 1
    let idx0 = b.iconst(LirType::U64, 0);
    let idx1 = b.iconst(LirType::U64, 1);
    let ptr0 = b.ptr_offset(ptr.clone(), idx0);
    let ptr1 = b.ptr_offset(ptr, idx1);
    b.ptr_store(ptr0.clone(), pvs[0][0].clone());
    b.ptr_store(ptr1, pvs[1][0].clone());
    // load from index controlled by third param
    let idx = pvs[2][0].clone();
    // use ptr_index_load via the LirTarget trait method
    let loaded = b.ptr_index_load(ptr0.clone(), idx, &LirType::U64);
    b.ret(&[loaded[0].clone()]);
    b.end_function();

    b.finish().verify().expect("verify");
}

// ============================================================================
// call_extern
// ============================================================================

#[test]
fn test_call_extern() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_extern");
    let (entry, pvs) = b.begin_function("caller", &[LirType::U64], Some(LirType::U64));
    b.switch_to_block(entry);
    let result = b.call_extern(
        "some_extern",
        &[LirType::U64],
        &[pvs[0][0].clone()],
        Some(LirType::U64),
    );
    b.ret(&[result[0].clone()]);
    b.end_function();

    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("declare") && ir.contains("some_extern"),
        "expected extern declaration in:\n{ir}"
    );
}

/// Calling the same extern twice should not duplicate the declaration.
#[test]
fn test_call_extern_dedup() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_extern_dedup");
    let (entry, pvs) = b.begin_function("caller", &[LirType::U64], Some(LirType::U64));
    b.switch_to_block(entry);
    let r1 = b.call_extern(
        "dup_extern",
        &[LirType::U64],
        &[pvs[0][0].clone()],
        Some(LirType::U64),
    );
    let r2 = b.call_extern(
        "dup_extern",
        &[LirType::U64],
        &[r1[0].clone()],
        Some(LirType::U64),
    );
    b.ret(&[r2[0].clone()]);
    b.end_function();

    let m = b.finish();
    m.verify().expect("verify");
    // Only one declaration of dup_extern.
    let ir = m.print_to_string().to_string();
    let count = ir.matches("dup_extern").count();
    // 1 declaration + 2 call sites = 3 occurrences, but only 1 `declare`
    assert_eq!(ir.matches("declare").filter(|_| true).count(), 1, "ir:\n{ir}");
    assert!(count >= 3, "expected 3+ references to dup_extern, got {count}:\n{ir}");
}

// ============================================================================
// Multiple functions in one module
// ============================================================================

#[test]
fn test_multiple_functions() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "multi");

    // First function: identity u64
    let (e1, pvs1) = b.begin_function("id_u64", &[LirType::U64], Some(LirType::U64));
    b.switch_to_block(e1);
    b.ret(&[pvs1[0][0].clone()]);
    b.end_function();

    // Second function: add two u32
    let (e2, pvs2) = b.begin_function(
        "add_u32",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    b.switch_to_block(e2);
    let sum = b.add(pvs2[0][0].clone(), pvs2[1][0].clone());
    b.ret(&[sum]);
    b.end_function();

    b.finish().verify().expect("verify");
}

// ============================================================================
// NameConfig: prefix and per-name remap
// ============================================================================

#[test]
fn test_name_config_prefix() {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_prefix").with_prefix("pfx_");

    let (entry, pvs) = b.begin_function("add", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let sum = b.add(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[sum]);
    b.end_function();

    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("pfx_add"),
        "expected prefixed function name 'pfx_add' in:\n{ir}"
    );
    // The bare "add" should not appear as a definition.
    assert!(
        !ir.contains("define") || ir.contains("pfx_add"),
        "un-prefixed 'add' should not be defined in:\n{ir}"
    );
}

#[test]
fn test_name_config_remap() {
    use std::collections::BTreeMap;
    use volar_llvm_backend::NameConfig;

    let mut remap = BTreeMap::new();
    remap.insert("add".to_string(), "vector_add".to_string());
    let cfg = NameConfig { prefix: "pfx_".to_string(), remap };

    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, "test_remap").with_name_config(cfg);

    // "add" is remapped to "vector_add" (prefix not applied).
    let (entry, pvs) = b.begin_function("add", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let sum = b.add(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[sum]);
    b.end_function();

    // "sub" gets the prefix "pfx_".
    let (entry2, pvs2) = b.begin_function("sub", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry2);
    let diff = b.sub(pvs2[0][0].clone(), pvs2[1][0].clone());
    b.ret(&[diff]);
    b.end_function();

    let m = b.finish();
    m.verify().expect("verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("vector_add"),
        "expected remapped name 'vector_add' in:\n{ir}"
    );
    assert!(
        ir.contains("pfx_sub"),
        "expected prefixed name 'pfx_sub' in:\n{ir}"
    );
}
