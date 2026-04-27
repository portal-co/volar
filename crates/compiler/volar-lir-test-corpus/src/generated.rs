// @reliability: normal
// @ai: assisted
// !!! THIS FILE IS AUTO-GENERATED !!!
// Run `python3 scripts/gen-lir-corpus.py` to regenerate.
// Do NOT edit by hand.

use volar_lir::{IcmpPred, LirTarget, LirType};
use crate::{CorpusCase, CorpusIo};

/// Build `const_u32` into backend `b`: Return a compile-time constant u32.
pub fn build_const_u32<B: LirTarget>(b: &mut B) {
    let (entry, _) = b.begin_function("const_u32", &[], Some(LirType::U32));
    b.switch_to_block(entry);
    let c = b.iconst(LirType::U32, 42);
    b.ret(&[c]);
    b.end_function();
}

/// Build `add_u32` into backend `b`: Add two u32 values.
pub fn build_add_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("add_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let sum = b.add(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[sum]);
    b.end_function();
}

/// Build `sub_u32` into backend `b`: Subtract two u32 values.
pub fn build_sub_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("sub_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let diff = b.sub(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[diff]);
    b.end_function();
}

/// Build `mul_u32` into backend `b`: Multiply two u32 values.
pub fn build_mul_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("mul_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let prod = b.mul(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[prod]);
    b.end_function();
}

/// Build `udiv_u32` into backend `b`: Unsigned-divide two u32 values.
pub fn build_udiv_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("udiv_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let quot = b.udiv(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[quot]);
    b.end_function();
}

/// Build `and_u64` into backend `b`: Bitwise AND two u64 values.
pub fn build_and_u64<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("and_u64", &[LirType::U64, LirType::U64], Some(LirType::U64));
    b.switch_to_block(entry);
    let r = b.and(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `or_u64` into backend `b`: Bitwise OR two u64 values.
pub fn build_or_u64<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("or_u64", &[LirType::U64, LirType::U64], Some(LirType::U64));
    b.switch_to_block(entry);
    let r = b.or(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `xor_u64` into backend `b`: Bitwise XOR two u64 values.
pub fn build_xor_u64<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("xor_u64", &[LirType::U64, LirType::U64], Some(LirType::U64));
    b.switch_to_block(entry);
    let r = b.xor(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `not_bool` into backend `b`: Boolean NOT.
pub fn build_not_bool<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("not_bool", &[LirType::Bool], Some(LirType::Bool));
    b.switch_to_block(entry);
    let r = b.not(pvs[0][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `shl_u32` into backend `b`: Left-shift a u32.
pub fn build_shl_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("shl_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let r = b.shl(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `lshr_u32` into backend `b`: Logical right-shift a u32.
pub fn build_lshr_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("lshr_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let r = b.lshr(pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `icmp_eq_u32` into backend `b`: Compare two u32 values for equality, returning a bool.
pub fn build_icmp_eq_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("icmp_eq_u32", &[LirType::U32, LirType::U32], Some(LirType::Bool));
    b.switch_to_block(entry);
    let r = b.icmp(IcmpPred::Eq, pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `icmp_ult_u32` into backend `b`: Unsigned less-than comparison of two u32 values, returning a bool.
pub fn build_icmp_ult_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("icmp_ult_u32", &[LirType::U32, LirType::U32], Some(LirType::Bool));
    b.switch_to_block(entry);
    let r = b.icmp(IcmpPred::Ult, pvs[0][0].clone(), pvs[1][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `zext_u8_to_u32` into backend `b`: Zero-extend a u8 to u32.
pub fn build_zext_u8_to_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("zext_u8_to_u32", &[LirType::U8], Some(LirType::U32));
    b.switch_to_block(entry);
    let r = b.zext(pvs[0][0].clone(), LirType::U32);
    b.ret(&[r]);
    b.end_function();
}

/// Build `trunc_u32_to_u8` into backend `b`: Truncate a u32 to u8.
pub fn build_trunc_u32_to_u8<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("trunc_u32_to_u8", &[LirType::U32], Some(LirType::U8));
    b.switch_to_block(entry);
    let r = b.trunc(pvs[0][0].clone(), LirType::U8);
    b.ret(&[r]);
    b.end_function();
}

/// Build `select_u32` into backend `b`: Select one of two u32 values based on a bool condition.
pub fn build_select_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function(
        "select_u32",
        &[LirType::Bool, LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    b.switch_to_block(entry);
    let r = b.select(pvs[0][0].clone(), pvs[1][0].clone(), pvs[2][0].clone());
    b.ret(&[r]);
    b.end_function();
}

/// Build `branch_merge_u32` into backend `b`: Branch on a bool and merge two u32 paths: if cond { x } else { y }.
pub fn build_branch_merge_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function(
        "branch_merge_u32",
        &[LirType::Bool, LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    let merge = b.create_block();
    let result = b.add_block_param(merge.clone(), LirType::U32);
    b.switch_to_block(entry);
    let cond = pvs[0][0].clone();
    let x    = pvs[1][0].clone();
    let y    = pvs[2][0].clone();
    b.branch(cond, merge.clone(), &[x], merge.clone(), &[y]);
    b.switch_to_block(merge);
    b.ret(&[result]);
    b.end_function();
}

/// Build `loop_sum_u32` into backend `b`: Sum 1..=n using a loop: countdown(n, acc=0) accumulates n + n-1 + ... + 1.
pub fn build_loop_sum_u32<B: LirTarget>(b: &mut B) {
    let (entry, pvs) = b.begin_function("loop_sum_u32", &[LirType::U32], Some(LirType::U32));
    let loop_block = b.create_block();
    let counter    = b.add_block_param(loop_block.clone(), LirType::U32);
    let accum      = b.add_block_param(loop_block.clone(), LirType::U32);
    let done_block  = b.create_block();
    let done_result = b.add_block_param(done_block.clone(), LirType::U32);
    b.switch_to_block(entry);
    let zero_init = b.iconst(LirType::U32, 0);
    b.jump(loop_block.clone(), &[pvs[0][0].clone(), zero_init]);
    b.switch_to_block(loop_block.clone());
    let zero = b.iconst(LirType::U32, 0);
    let cond = b.icmp(IcmpPred::Eq, counter.clone(), zero);
    let new_acc = b.add(accum.clone(), counter.clone());
    let one = b.iconst(LirType::U32, 1);
    let new_ctr = b.sub(counter, one);
    b.branch(cond, done_block.clone(), &[accum], loop_block, &[new_ctr, new_acc]);
    b.switch_to_block(done_block);
    b.ret(&[done_result]);
    b.end_function();
}

/// Metadata for all corpus cases: name + I/O pairs for verification.
pub static ALL_CASES: &[CorpusCase] = &[
    CorpusCase {
        name: "const_u32",
        ios: &[
        CorpusIo { inputs: &[], expected: 42 },
        ],
        c_arg_types: &[],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "const_u32()",
    },
    CorpusCase {
        name: "add_u32",
        ios: &[
        CorpusIo { inputs: &[3, 4], expected: 7 },
        CorpusIo { inputs: &[0, 0], expected: 0 },
        CorpusIo { inputs: &[100, 200], expected: 300 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "add_u32(a0, a1)",
    },
    CorpusCase {
        name: "sub_u32",
        ios: &[
        CorpusIo { inputs: &[10, 3], expected: 7 },
        CorpusIo { inputs: &[0, 0], expected: 0 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "sub_u32(a0, a1)",
    },
    CorpusCase {
        name: "mul_u32",
        ios: &[
        CorpusIo { inputs: &[3, 4], expected: 12 },
        CorpusIo { inputs: &[0, 7], expected: 0 },
        CorpusIo { inputs: &[6, 7], expected: 42 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "mul_u32(a0, a1)",
    },
    CorpusCase {
        name: "udiv_u32",
        ios: &[
        CorpusIo { inputs: &[12, 4], expected: 3 },
        CorpusIo { inputs: &[7, 2], expected: 3 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "udiv_u32(a0, a1)",
    },
    CorpusCase {
        name: "and_u64",
        ios: &[
        CorpusIo { inputs: &[255, 15], expected: 15 },
        CorpusIo { inputs: &[0, 18446744073709551615], expected: 0 },
        ],
        c_arg_types: &["uint64_t", "uint64_t"],
        c_ret_fmt: "%llu",
        c_ret_cast: "(unsigned long long)",
        c_call_template: "and_u64(a0, a1)",
    },
    CorpusCase {
        name: "or_u64",
        ios: &[
        CorpusIo { inputs: &[240, 15], expected: 255 },
        CorpusIo { inputs: &[0, 0], expected: 0 },
        ],
        c_arg_types: &["uint64_t", "uint64_t"],
        c_ret_fmt: "%llu",
        c_ret_cast: "(unsigned long long)",
        c_call_template: "or_u64(a0, a1)",
    },
    CorpusCase {
        name: "xor_u64",
        ios: &[
        CorpusIo { inputs: &[255, 15], expected: 240 },
        CorpusIo { inputs: &[170, 170], expected: 0 },
        ],
        c_arg_types: &["uint64_t", "uint64_t"],
        c_ret_fmt: "%llu",
        c_ret_cast: "(unsigned long long)",
        c_call_template: "xor_u64(a0, a1)",
    },
    CorpusCase {
        name: "not_bool",
        ios: &[
        CorpusIo { inputs: &[0], expected: 1 },
        CorpusIo { inputs: &[1], expected: 0 },
        ],
        c_arg_types: &["uint8_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "not_bool(a0)",
    },
    CorpusCase {
        name: "shl_u32",
        ios: &[
        CorpusIo { inputs: &[1, 4], expected: 16 },
        CorpusIo { inputs: &[3, 2], expected: 12 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "shl_u32(a0, a1)",
    },
    CorpusCase {
        name: "lshr_u32",
        ios: &[
        CorpusIo { inputs: &[16, 4], expected: 1 },
        CorpusIo { inputs: &[12, 2], expected: 3 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "lshr_u32(a0, a1)",
    },
    CorpusCase {
        name: "icmp_eq_u32",
        ios: &[
        CorpusIo { inputs: &[5, 5], expected: 1 },
        CorpusIo { inputs: &[5, 6], expected: 0 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "icmp_eq_u32(a0, a1)",
    },
    CorpusCase {
        name: "icmp_ult_u32",
        ios: &[
        CorpusIo { inputs: &[3, 5], expected: 1 },
        CorpusIo { inputs: &[5, 3], expected: 0 },
        CorpusIo { inputs: &[4, 4], expected: 0 },
        ],
        c_arg_types: &["uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "icmp_ult_u32(a0, a1)",
    },
    CorpusCase {
        name: "zext_u8_to_u32",
        ios: &[
        CorpusIo { inputs: &[255], expected: 255 },
        CorpusIo { inputs: &[0], expected: 0 },
        CorpusIo { inputs: &[42], expected: 42 },
        ],
        c_arg_types: &["uint8_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "zext_u8_to_u32(a0)",
    },
    CorpusCase {
        name: "trunc_u32_to_u8",
        ios: &[
        CorpusIo { inputs: &[256], expected: 0 },
        CorpusIo { inputs: &[257], expected: 1 },
        CorpusIo { inputs: &[42], expected: 42 },
        ],
        c_arg_types: &["uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "trunc_u32_to_u8(a0)",
    },
    CorpusCase {
        name: "select_u32",
        ios: &[
        CorpusIo { inputs: &[1, 10, 20], expected: 10 },
        CorpusIo { inputs: &[0, 10, 20], expected: 20 },
        ],
        c_arg_types: &["uint8_t", "uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "select_u32(a0, a1, a2)",
    },
    CorpusCase {
        name: "branch_merge_u32",
        ios: &[
        CorpusIo { inputs: &[1, 10, 20], expected: 10 },
        CorpusIo { inputs: &[0, 10, 20], expected: 20 },
        ],
        c_arg_types: &["uint8_t", "uint32_t", "uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "branch_merge_u32(a0, a1, a2)",
    },
    CorpusCase {
        name: "loop_sum_u32",
        ios: &[
        CorpusIo { inputs: &[0], expected: 0 },
        CorpusIo { inputs: &[1], expected: 1 },
        CorpusIo { inputs: &[5], expected: 15 },
        CorpusIo { inputs: &[10], expected: 55 },
        ],
        c_arg_types: &["uint32_t"],
        c_ret_fmt: "%u",
        c_ret_cast: "(unsigned)",
        c_call_template: "loop_sum_u32(a0)",
    },
];

/// Invoke `$factory` once per corpus case (creates a fresh backend per case).
/// Primarily for smoke tests on circuit backends.
#[macro_export]
macro_rules! for_each_build {
    ($factory:expr) => {{
        { let mut b = $factory; $crate::generated::build_const_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_add_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_sub_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_mul_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_udiv_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_and_u64(&mut b); }
        { let mut b = $factory; $crate::generated::build_or_u64(&mut b); }
        { let mut b = $factory; $crate::generated::build_xor_u64(&mut b); }
        { let mut b = $factory; $crate::generated::build_not_bool(&mut b); }
        { let mut b = $factory; $crate::generated::build_shl_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_lshr_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_icmp_eq_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_icmp_ult_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_zext_u8_to_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_trunc_u32_to_u8(&mut b); }
        { let mut b = $factory; $crate::generated::build_select_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_branch_merge_u32(&mut b); }
        { let mut b = $factory; $crate::generated::build_loop_sum_u32(&mut b); }
    }};
}
