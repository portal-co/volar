// @reliability: experimental
// @ai: assisted
//! Round-trip and golden tests for the volar-lir-text format.

#![cfg(all(test, feature = "parse"))]
extern crate std;

use std::boxed::Box;
use volar_lir::{FieldDef, IcmpPred, LirType, StructDef};
use volar_lir_saved::{LirCall, SavedLirModule};
use volar_ir_common::Type as NativeType;
use crate::{ParseText, WriteText};
use crate::parse::ParseError;

// ============================================================================
// LirType round-trips
// ============================================================================

fn rt_lir_type(ty: LirType) {
    let text = ty.to_text_string();
    let parsed = LirType::parse_text(&text).expect("parse failed");
    assert_eq!(ty, parsed, "round-trip failed for: {}", text);
}

#[test]
fn lir_type_scalars() {
    for ty in [
        LirType::Bool,
        LirType::I8, LirType::U8,
        LirType::I16, LirType::U16,
        LirType::I32, LirType::U32,
        LirType::I64, LirType::U64,
    ] {
        rt_lir_type(ty);
    }
}

#[test]
fn lir_type_arr() {
    rt_lir_type(LirType::Arr(Box::new(LirType::U8), 32));
    rt_lir_type(LirType::Arr(Box::new(LirType::U32), 4));
    // nested
    rt_lir_type(LirType::Arr(Box::new(LirType::Arr(Box::new(LirType::U8), 16)), 8));
}

#[test]
fn lir_type_struct() {
    rt_lir_type(LirType::Struct(0));
    rt_lir_type(LirType::Struct(42));
}

#[test]
fn lir_type_native() {
    for nt in [
        NativeType::Bit,
        NativeType::_8,
        NativeType::_16,
        NativeType::_32,
        NativeType::_64,
        NativeType::_128,
        NativeType::_256,
        NativeType::AES8,
        NativeType::Galois64,
    ] {
        rt_lir_type(LirType::Native(nt));
    }
}

#[test]
fn lir_type_ptr() {
    rt_lir_type(LirType::Ptr(Box::new(LirType::U8)));
    rt_lir_type(LirType::Ptr(Box::new(LirType::Arr(Box::new(LirType::U32), 4))));
}

// ============================================================================
// LirCall round-trips
// ============================================================================

fn rt_call(call: LirCall) {
    let module = SavedLirModule { calls: std::vec![call.clone()] };
    let text = module.to_text_string();
    let parsed = SavedLirModule::parse_text(&text).expect("parse failed");
    assert_eq!(
        module.calls, parsed.calls,
        "round-trip failed.\nSerialized:\n{}", text
    );
}

#[test]
fn call_define_struct() {
    rt_call(LirCall::DefineStruct {
        def: StructDef {
            name: "Foo".into(),
            fields: std::vec![
                FieldDef { name: "a".into(), ty: LirType::U8 },
                FieldDef { name: "b".into(), ty: LirType::U32 },
            ],
        },
        id: 0,
    });
}

#[test]
fn call_begin_end_function() {
    rt_call(LirCall::BeginFunction {
        name: "my_fn".into(),
        params: std::vec![LirType::U8, LirType::U32],
        ret: Some(LirType::U64),
        entry_block: 0,
        param_vals: std::vec![std::vec![0], std::vec![1]],
    });
    rt_call(LirCall::BeginFunction {
        name: "void_fn".into(),
        params: std::vec![],
        ret: None,
        entry_block: 0,
        param_vals: std::vec![],
    });
    rt_call(LirCall::EndFunction);
}

#[test]
fn call_blocks() {
    rt_call(LirCall::CreateBlock { block: 3 });
    rt_call(LirCall::AddBlockParam { block: 1, ty: LirType::U32, val: 5 });
    rt_call(LirCall::SwitchToBlock { block: 2 });
}

#[test]
fn call_iconst() {
    rt_call(LirCall::Iconst { ty: LirType::U8,  val: 42,  out: 2 });
    rt_call(LirCall::Iconst { ty: LirType::I64, val: -1,  out: 3 });
    rt_call(LirCall::Iconst { ty: LirType::U32, val: 0,   out: 4 });
}

#[test]
fn call_arithmetic() {
    rt_call(LirCall::Add  { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Sub  { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Mul  { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Udiv { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Sdiv { lhs: 0, rhs: 1, out: 2 });
}

#[test]
fn call_bitwise() {
    rt_call(LirCall::And  { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Or   { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Xor  { lhs: 0, rhs: 1, out: 2 });
    rt_call(LirCall::Not  { val: 0, out: 1 });
    rt_call(LirCall::Shl  { val: 0, shift: 1, out: 2 });
    rt_call(LirCall::Lshr { val: 0, shift: 1, out: 2 });
    rt_call(LirCall::Ashr { val: 0, shift: 1, out: 2 });
}

#[test]
fn call_icmp_all_preds() {
    for pred in [
        IcmpPred::Eq,  IcmpPred::Ne,
        IcmpPred::Ult, IcmpPred::Ule, IcmpPred::Ugt, IcmpPred::Uge,
        IcmpPred::Slt, IcmpPred::Sle, IcmpPred::Sgt, IcmpPred::Sge,
    ] {
        rt_call(LirCall::Icmp { pred, lhs: 0, rhs: 1, out: 2 });
    }
}

#[test]
fn call_conversions() {
    rt_call(LirCall::Zext  { val: 0, dst_ty: LirType::U32, out: 1 });
    rt_call(LirCall::Sext  { val: 0, dst_ty: LirType::I64, out: 1 });
    rt_call(LirCall::Trunc { val: 0, dst_ty: LirType::U8,  out: 1 });
}

#[test]
fn call_select() {
    rt_call(LirCall::Select { cond: 0, then_val: 1, else_val: 2, out: 3 });
}

#[test]
fn call_terminators() {
    rt_call(LirCall::Jump { target: 2, args: std::vec![0, 1] });
    rt_call(LirCall::Jump { target: 0, args: std::vec![] });
    rt_call(LirCall::Branch {
        cond: 0,
        then_block: 1, then_args: std::vec![2],
        else_block: 3, else_args: std::vec![4, 5],
    });
    rt_call(LirCall::Ret { vals: std::vec![0] });
    rt_call(LirCall::Ret { vals: std::vec![] });
}

#[test]
fn call_extern() {
    rt_call(LirCall::CallExtern {
        name: "printf".into(),
        arg_tys: std::vec![LirType::Ptr(Box::new(LirType::U8)), LirType::U32],
        args: std::vec![0, 1],
        ret_ty: Some(LirType::I32),
        outs: std::vec![2],
    });
    rt_call(LirCall::CallExtern {
        name: "void_call".into(),
        arg_tys: std::vec![],
        args: std::vec![],
        ret_ty: None,
        outs: std::vec![],
    });
}

#[test]
fn call_oracle() {
    rt_call(LirCall::Oracle {
        name: "sha256".into(),
        arg_tys: std::vec![LirType::Arr(Box::new(LirType::U8), 64)],
        args: std::vec![0],
        ret_tys: std::vec![LirType::Arr(Box::new(LirType::U8), 32)],
        outs: std::vec![1],
    });
}

#[test]
fn call_action() {
    rt_call(LirCall::Action {
        name: "verify_sig".into(),
        guard: 0,
        arg_tys: std::vec![LirType::U32],
        args: std::vec![1],
        fallbacks: std::vec![2],
        ret_tys: std::vec![LirType::Bool],
        outs: std::vec![3],
    });
}

#[test]
fn call_rng() {
    rt_call(LirCall::Rng { ty: LirType::U64, out: 0 });
    rt_call(LirCall::Rng { ty: LirType::Native(NativeType::Galois64), out: 1 });
}

#[test]
fn call_stack_alloc() {
    rt_call(LirCall::Alloca { elem_ty: LirType::U8, count: 16, out: 0 });
    rt_call(LirCall::PtrLoad { ptr: 0, ty: LirType::U8, out: 1 });
    rt_call(LirCall::PtrStore { ptr: 0, val: 1 });
    rt_call(LirCall::PtrOffset { ptr: 0, idx: 1, out: 2 });
    rt_call(LirCall::PtrIndexLoad {
        ptr: 0, idx: 1,
        pointee_ty: LirType::U32,
        outs: std::vec![2],
    });
    rt_call(LirCall::PtrIndexStore {
        ptr: 0, idx: 1,
        vals: std::vec![2],
        pointee_ty: LirType::U32,
    });
}

#[test]
fn call_metadata() {
    rt_call(LirCall::ValueType { val: 0, ty: LirType::U8 });
    rt_call(LirCall::SetProv);
}

#[test]
fn call_string_escaping() {
    // Names containing characters that need escaping
    rt_call(LirCall::BeginFunction {
        name: r#"fn_with_"quotes""#.into(),
        params: std::vec![],
        ret: None,
        entry_block: 0,
        param_vals: std::vec![],
    });
    rt_call(LirCall::BeginFunction {
        name: "fn_with_back\\slash".into(),
        params: std::vec![],
        ret: None,
        entry_block: 0,
        param_vals: std::vec![],
    });
}

// ============================================================================
// Full module round-trip
// ============================================================================

#[test]
fn full_module_round_trip() {
    // Build a realistic two-function module using RecordingTarget
    use volar_lir::LirTarget;
    use volar_lir_saved::RecordingTarget;

    let mut rec = RecordingTarget::new();

    // Function 1: add_bytes(u8, u8) -> u8
    let (_entry, pvs) = rec.begin_function("add_bytes", &[LirType::U8, LirType::U8], Some(LirType::U8));
    let a = pvs[0][0].clone();
    let b = pvs[1][0].clone();
    let sum = rec.add(a, b);
    rec.ret(&[sum]);
    rec.end_function();

    // Function 2: iconst_u32() -> u32
    let (_entry2, _) = rec.begin_function("const_fn", &[], Some(LirType::U32));
    let c = rec.iconst(LirType::U32, 255);
    rec.ret(&[c]);
    rec.end_function();

    let original = rec.finish();

    // Round-trip through text
    let text = original.to_text_string();
    let parsed = SavedLirModule::parse_text(&text).expect("parse failed");

    assert_eq!(original.calls, parsed.calls,
        "full module round-trip failed.\nText:\n{}", text);
}

// ============================================================================
// Replay equivalence
// ============================================================================

/// Build a module, round-trip through text, replay both into fresh
/// RecordingTargets, and assert the re-recorded module call sequences are equal.
#[test]
fn round_trip_replay_equivalence() {
    use volar_lir::LirTarget;
    use volar_lir_saved::RecordingTarget;

    let mut rec = RecordingTarget::new();
    let (_entry, pvs) = rec.begin_function("f", &[LirType::U32, LirType::U32], Some(LirType::U32));
    let x = pvs[0][0].clone();
    let y = pvs[1][0].clone();
    let s = rec.add(x.clone(), y);
    let z = rec.iconst(LirType::U32, 0);
    let r = rec.icmp(IcmpPred::Eq, s, z);
    let v = rec.zext(r, LirType::U32);
    rec.ret(&[v]);
    rec.end_function();
    let original = rec.finish();

    // Text round-trip
    let text = original.to_text_string();
    let parsed = SavedLirModule::parse_text(&text).expect("parse");

    // Replay original into a fresh recorder
    let mut rec2 = RecordingTarget::new();
    original.replay(&mut rec2);
    let replayed_orig = rec2.finish();

    // Replay parsed into a fresh recorder
    let mut rec3 = RecordingTarget::new();
    parsed.replay(&mut rec3);
    let replayed_parsed = rec3.finish();

    assert_eq!(
        replayed_orig.calls, replayed_parsed.calls,
        "replay outputs differ after text round-trip"
    );
}

// ============================================================================
// Parser error cases
// ============================================================================

#[test]
fn error_missing_version_line() {
    let err = SavedLirModule::parse_text("end_function\n").unwrap_err();
    assert!(matches!(err, ParseError::MissingVersionLine),
        "expected MissingVersionLine, got {:?}", err);
}

#[test]
fn error_wrong_version() {
    let err = SavedLirModule::parse_text("volar-lir-saved v99\n").unwrap_err();
    assert!(matches!(err, ParseError::UnsupportedVersion(_)),
        "expected UnsupportedVersion, got {:?}", err);
}

#[test]
fn error_unknown_directive() {
    let err = SavedLirModule::parse_text("volar-lir-saved v1\nfrobulate x=1\n").unwrap_err();
    assert!(matches!(err, ParseError::UnknownDirective(_)),
        "expected UnknownDirective, got {:?}", err);
}

#[test]
fn error_unknown_lir_type() {
    let err = LirType::parse_text("quadfloat").unwrap_err();
    assert!(matches!(err, ParseError::UnexpectedToken { .. }),
        "expected UnexpectedToken for unknown type, got {:?}", err);
}

#[test]
fn comments_and_blank_lines() {
    let text = r#"volar-lir-saved v1
; this is a comment
; another comment

end_function
; trailing comment
"#;
    let parsed = SavedLirModule::parse_text(text).expect("parse with comments should work");
    assert_eq!(parsed.calls, std::vec![LirCall::EndFunction]);
}
