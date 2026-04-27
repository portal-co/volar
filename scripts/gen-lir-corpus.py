#!/usr/bin/env python3
# @reliability: normal
# @ai: assisted
"""
Generator for volar-lir-test-corpus/src/generated.rs.

Run from the workspace root:
    python3 scripts/gen-lir-corpus.py

Writes crates/compiler/volar-lir-test-corpus/src/generated.rs.
"""

import os
import textwrap

# ---------------------------------------------------------------------------
# Corpus case definitions
# Each entry:
#   name        – snake_case function name (also used as corpus case name)
#   build_fn    – Rust code lines that build the function (list of strings)
#   param_types – LirTypes for the function parameters (as Rust exprs)
#   ret_type    – LirType for the return value (Rust expr, or None)
#   ios         – list of (inputs: list[int], expected: int)
#   c_printf    – printf format + cast/expression for calling the function in C
#                 The call expression uses arg names a0, a1, ... as u32/u64 values.
#   c_arg_type  – C type string for arguments (e.g. "uint32_t")
#   c_ret_fmt   – printf format specifier (e.g. "%u" or "%llu")
# ---------------------------------------------------------------------------

CASES = [
    {
        "name": "const_u32",
        "doc": "Return a compile-time constant u32.",
        "param_types": [],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, _) = b.begin_function(\"const_u32\", &[], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let c = b.iconst(LirType::U32, 42);",
            "b.ret(&[c]);",
            "b.end_function();",
        ],
        "ios": [([], 42)],
        "c_call": "const_u32()",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "add_u32",
        "doc": "Add two u32 values.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"add_u32\", &[LirType::U32, LirType::U32], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let sum = b.add(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[sum]);",
            "b.end_function();",
        ],
        "ios": [([3, 4], 7), ([0, 0], 0), ([100, 200], 300)],
        "c_call": "add_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "sub_u32",
        "doc": "Subtract two u32 values.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"sub_u32\", &[LirType::U32, LirType::U32], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let diff = b.sub(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[diff]);",
            "b.end_function();",
        ],
        "ios": [([10, 3], 7), ([0, 0], 0)],
        "c_call": "sub_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "mul_u32",
        "doc": "Multiply two u32 values.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"mul_u32\", &[LirType::U32, LirType::U32], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let prod = b.mul(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[prod]);",
            "b.end_function();",
        ],
        "ios": [([3, 4], 12), ([0, 7], 0), ([6, 7], 42)],
        "c_call": "mul_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "udiv_u32",
        "doc": "Unsigned-divide two u32 values.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"udiv_u32\", &[LirType::U32, LirType::U32], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let quot = b.udiv(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[quot]);",
            "b.end_function();",
        ],
        "ios": [([12, 4], 3), ([7, 2], 3)],
        "c_call": "udiv_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "and_u64",
        "doc": "Bitwise AND two u64 values.",
        "param_types": ["LirType::U64", "LirType::U64"],
        "ret_type": "LirType::U64",
        "build": [
            "let (entry, pvs) = b.begin_function(\"and_u64\", &[LirType::U64, LirType::U64], Some(LirType::U64));",
            "b.switch_to_block(entry);",
            "let r = b.and(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([0xFF, 0x0F], 0x0F), ([0, 0xFFFFFFFFFFFFFFFF], 0)],
        "c_call": "and_u64(a0, a1)",
        "c_arg_type": "uint64_t",
        "c_ret_fmt": "%llu",
        "c_ret_cast": "(unsigned long long)",
    },
    {
        "name": "or_u64",
        "doc": "Bitwise OR two u64 values.",
        "param_types": ["LirType::U64", "LirType::U64"],
        "ret_type": "LirType::U64",
        "build": [
            "let (entry, pvs) = b.begin_function(\"or_u64\", &[LirType::U64, LirType::U64], Some(LirType::U64));",
            "b.switch_to_block(entry);",
            "let r = b.or(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([0xF0, 0x0F], 0xFF), ([0, 0], 0)],
        "c_call": "or_u64(a0, a1)",
        "c_arg_type": "uint64_t",
        "c_ret_fmt": "%llu",
        "c_ret_cast": "(unsigned long long)",
    },
    {
        "name": "xor_u64",
        "doc": "Bitwise XOR two u64 values.",
        "param_types": ["LirType::U64", "LirType::U64"],
        "ret_type": "LirType::U64",
        "build": [
            "let (entry, pvs) = b.begin_function(\"xor_u64\", &[LirType::U64, LirType::U64], Some(LirType::U64));",
            "b.switch_to_block(entry);",
            "let r = b.xor(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([0xFF, 0x0F], 0xF0), ([0xAA, 0xAA], 0)],
        "c_call": "xor_u64(a0, a1)",
        "c_arg_type": "uint64_t",
        "c_ret_fmt": "%llu",
        "c_ret_cast": "(unsigned long long)",
    },
    {
        "name": "not_bool",
        "doc": "Boolean NOT.",
        "param_types": ["LirType::Bool"],
        "ret_type": "LirType::Bool",
        "build": [
            "let (entry, pvs) = b.begin_function(\"not_bool\", &[LirType::Bool], Some(LirType::Bool));",
            "b.switch_to_block(entry);",
            "let r = b.not(pvs[0][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([0], 1), ([1], 0)],
        "c_call": "not_bool(a0)",
        "c_arg_type": "uint8_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "shl_u32",
        "doc": "Left-shift a u32.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"shl_u32\", &[LirType::U32, LirType::U32], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let r = b.shl(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([1, 4], 16), ([3, 2], 12)],
        "c_call": "shl_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "lshr_u32",
        "doc": "Logical right-shift a u32.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"lshr_u32\", &[LirType::U32, LirType::U32], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let r = b.lshr(pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([16, 4], 1), ([12, 2], 3)],
        "c_call": "lshr_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "icmp_eq_u32",
        "doc": "Compare two u32 values for equality, returning a bool.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::Bool",
        "build": [
            "let (entry, pvs) = b.begin_function(\"icmp_eq_u32\", &[LirType::U32, LirType::U32], Some(LirType::Bool));",
            "b.switch_to_block(entry);",
            "let r = b.icmp(IcmpPred::Eq, pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([5, 5], 1), ([5, 6], 0)],
        "c_call": "icmp_eq_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "icmp_ult_u32",
        "doc": "Unsigned less-than comparison of two u32 values, returning a bool.",
        "param_types": ["LirType::U32", "LirType::U32"],
        "ret_type": "LirType::Bool",
        "build": [
            "let (entry, pvs) = b.begin_function(\"icmp_ult_u32\", &[LirType::U32, LirType::U32], Some(LirType::Bool));",
            "b.switch_to_block(entry);",
            "let r = b.icmp(IcmpPred::Ult, pvs[0][0].clone(), pvs[1][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([3, 5], 1), ([5, 3], 0), ([4, 4], 0)],
        "c_call": "icmp_ult_u32(a0, a1)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "zext_u8_to_u32",
        "doc": "Zero-extend a u8 to u32.",
        "param_types": ["LirType::U8"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"zext_u8_to_u32\", &[LirType::U8], Some(LirType::U32));",
            "b.switch_to_block(entry);",
            "let r = b.zext(pvs[0][0].clone(), LirType::U32);",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([255], 255), ([0], 0), ([42], 42)],
        "c_call": "zext_u8_to_u32(a0)",
        "c_arg_type": "uint8_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "trunc_u32_to_u8",
        "doc": "Truncate a u32 to u8.",
        "param_types": ["LirType::U32"],
        "ret_type": "LirType::U8",
        "build": [
            "let (entry, pvs) = b.begin_function(\"trunc_u32_to_u8\", &[LirType::U32], Some(LirType::U8));",
            "b.switch_to_block(entry);",
            "let r = b.trunc(pvs[0][0].clone(), LirType::U8);",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([256], 0), ([257], 1), ([42], 42)],
        "c_call": "trunc_u32_to_u8(a0)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "select_u32",
        "doc": "Select one of two u32 values based on a bool condition.",
        "param_types": ["LirType::Bool", "LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(",
            "    \"select_u32\",",
            "    &[LirType::Bool, LirType::U32, LirType::U32],",
            "    Some(LirType::U32),",
            ");",
            "b.switch_to_block(entry);",
            "let r = b.select(pvs[0][0].clone(), pvs[1][0].clone(), pvs[2][0].clone());",
            "b.ret(&[r]);",
            "b.end_function();",
        ],
        "ios": [([1, 10, 20], 10), ([0, 10, 20], 20)],
        "c_call": "select_u32(a0, a1, a2)",
        "c_arg_types": ["uint8_t", "uint32_t", "uint32_t"],
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "branch_merge_u32",
        "doc": "Branch on a bool and merge two u32 paths: if cond { x } else { y }.",
        "param_types": ["LirType::Bool", "LirType::U32", "LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(",
            "    \"branch_merge_u32\",",
            "    &[LirType::Bool, LirType::U32, LirType::U32],",
            "    Some(LirType::U32),",
            ");",
            "let merge = b.create_block();",
            "let result = b.add_block_param(merge.clone(), LirType::U32);",
            "b.switch_to_block(entry);",
            "let cond = pvs[0][0].clone();",
            "let x    = pvs[1][0].clone();",
            "let y    = pvs[2][0].clone();",
            "b.branch(cond, merge.clone(), &[x], merge.clone(), &[y]);",
            "b.switch_to_block(merge);",
            "b.ret(&[result]);",
            "b.end_function();",
        ],
        "ios": [([1, 10, 20], 10), ([0, 10, 20], 20)],
        "c_call": "branch_merge_u32(a0, a1, a2)",
        "c_arg_types": ["uint8_t", "uint32_t", "uint32_t"],
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
    {
        "name": "loop_sum_u32",
        "doc": "Sum 1..=n using a loop: countdown(n, acc=0) accumulates n + n-1 + ... + 1.",
        "param_types": ["LirType::U32"],
        "ret_type": "LirType::U32",
        "build": [
            "let (entry, pvs) = b.begin_function(\"loop_sum_u32\", &[LirType::U32], Some(LirType::U32));",
            "let loop_block = b.create_block();",
            "let counter    = b.add_block_param(loop_block.clone(), LirType::U32);",
            "let accum      = b.add_block_param(loop_block.clone(), LirType::U32);",
            "let done_block  = b.create_block();",
            "let done_result = b.add_block_param(done_block.clone(), LirType::U32);",
            "b.switch_to_block(entry);",
            "let zero_init = b.iconst(LirType::U32, 0);",
            "b.jump(loop_block.clone(), &[pvs[0][0].clone(), zero_init]);",
            "b.switch_to_block(loop_block.clone());",
            "let zero = b.iconst(LirType::U32, 0);",
            "let cond = b.icmp(IcmpPred::Eq, counter.clone(), zero);",
            "let new_acc = b.add(accum.clone(), counter.clone());",
            "let one = b.iconst(LirType::U32, 1);",
            "let new_ctr = b.sub(counter, one);",
            "b.branch(cond, done_block.clone(), &[accum], loop_block, &[new_ctr, new_acc]);",
            "b.switch_to_block(done_block);",
            "b.ret(&[done_result]);",
            "b.end_function();",
        ],
        "ios": [([0], 0), ([1], 1), ([5], 15), ([10], 55)],
        "c_call": "loop_sum_u32(a0)",
        "c_arg_type": "uint32_t",
        "c_ret_fmt": "%u",
        "c_ret_cast": "(unsigned)",
    },
]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def indent(lines, n=4):
    prefix = " " * n
    return "\n".join(prefix + line for line in lines)


def get_arg_types(case):
    """Return the list of C argument types for this case."""
    if "c_arg_types" in case:
        return case["c_arg_types"]
    if "c_arg_type" in case:
        return [case["c_arg_type"]] * len(case["param_types"])
    return []


def ios_rust(case):
    """Render the ios as a Rust array literal."""
    ios = case["ios"]
    lines = []
    for (inputs, expected) in ios:
        input_str = "&[" + ", ".join(str(i) for i in inputs) + "]"
        lines.append(f"    CorpusIo {{ inputs: {input_str}, expected: {expected} }},")
    return "[\n" + "\n".join(lines) + "\n]"


def c_arg_names(case):
    n = len(case["param_types"])
    return [f"a{i}" for i in range(n)]


# ---------------------------------------------------------------------------
# Emit build_* functions
# ---------------------------------------------------------------------------

def emit_build_fn(case):
    name = case["name"]
    doc = case["doc"]
    lines = []
    lines.append(f"/// Build `{name}` into backend `b`: {doc}")
    lines.append(f"pub fn build_{name}<B: LirTarget>(b: &mut B) {{")
    for line in case["build"]:
        lines.append(f"    {line}")
    lines.append("}")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Emit ALL_CASES static
# ---------------------------------------------------------------------------

def emit_all_cases(cases):
    lines = []
    lines.append("/// Metadata for all corpus cases: name + I/O pairs for verification.")
    lines.append("pub static ALL_CASES: &[CorpusCase] = &[")
    for case in cases:
        name = case["name"]
        ios = case["ios"]
        ios_items = []
        for (inputs, expected) in ios:
            in_str = "&[" + ", ".join(str(i) for i in inputs) + "]"
            ios_items.append(f"        CorpusIo {{ inputs: {in_str}, expected: {expected} }},")
        ios_str = "\n".join(ios_items)

        arg_types = get_arg_types(case)
        ret_fmt = case["c_ret_fmt"]
        ret_cast = case["c_ret_cast"]
        c_call = case["c_call"]

        arg_types_strs = ", ".join(f'"{t}"' for t in arg_types)

        lines.append(f"    CorpusCase {{")
        lines.append(f'        name: "{name}",')
        lines.append(f"        ios: &[")
        lines.append(ios_str)
        lines.append(f"        ],")
        lines.append(f'        c_arg_types: &[{arg_types_strs}],')
        lines.append(f'        c_ret_fmt: "{ret_fmt}",')
        lines.append(f'        c_ret_cast: "{ret_cast}",')
        lines.append(f'        c_call_template: "{c_call}",')
        lines.append(f"    }},")
    lines.append("];")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Emit for_each_build! macro
# ---------------------------------------------------------------------------

def emit_macro(cases):
    lines = []
    lines.append("/// Invoke `$factory` once per corpus case (creates a fresh backend per case).")
    lines.append("/// Primarily for smoke tests on circuit backends.")
    lines.append("#[macro_export]")
    lines.append("macro_rules! for_each_build {")
    lines.append("    ($factory:expr) => {{")
    for case in cases:
        name = case["name"]
        lines.append(f"        {{ let mut b = $factory; $crate::generated::build_{name}(&mut b); }}")
    lines.append("    }};")
    lines.append("}")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Top-level emit
# ---------------------------------------------------------------------------

HEADER = """\
// @reliability: normal
// @ai: assisted
// !!! THIS FILE IS AUTO-GENERATED !!!
// Run `python3 scripts/gen-lir-corpus.py` to regenerate.
// Do NOT edit by hand.

use volar_lir::{IcmpPred, LirTarget, LirType};
use crate::{CorpusCase, CorpusIo};
"""

def emit_generated(cases):
    parts = [HEADER]
    # Build functions
    for case in cases:
        parts.append(emit_build_fn(case))
        parts.append("")
    # ALL_CASES
    parts.append(emit_all_cases(cases))
    parts.append("")
    # for_each_build! macro
    parts.append(emit_macro(cases))
    parts.append("")
    return "\n".join(parts)


if __name__ == "__main__":
    output_path = os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
        "crates", "compiler", "volar-lir-test-corpus", "src", "generated.rs",
    )
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    content = emit_generated(CASES)
    with open(output_path, "w") as f:
        f.write(content)
    print(f"Written: {output_path}")
    print(f"Cases: {len(CASES)}")
