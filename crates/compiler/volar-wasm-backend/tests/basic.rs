// @reliability: experimental
// @ai: assisted
//! Direct `LirTarget` API tests for [`WasmBackend`].
//!
//! Builds tiny modules and executes them with wasmtime.

use volar_lir::{BranchTarget, IcmpPred, LirTarget, LirType};
use volar_lir_saved::{RecordingTarget, SavedLirModule};
use volar_wasm_backend::WasmBackend;
use wasmtime::{Engine, Instance, Module, Store};

fn run_i32_binop(bytes: &[u8], name: &str, a: i32, b: i32) -> i32 {
    let engine = Engine::default();
    let module = Module::new(&engine, bytes).expect("invalid wasm module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("instantiate");
    let func = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, name)
        .unwrap_or_else(|e| panic!("get func `{name}`: {e}"));
    func.call(&mut store, (a, b))
        .unwrap_or_else(|e| panic!("call `{name}`: {e}"))
}

#[test]
fn add_u32_via_lir_target() {
    let mut b = WasmBackend::new();
    let (entry, params) = b.begin_function(
        "add",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    b.switch_to_block(entry);
    let sum = b.add(params[0][0], params[1][0]);
    b.ret(&[sum]);
    b.end_function();
    let bytes = b.finish();
    assert!(!bytes.is_empty());
    assert_eq!(run_i32_binop(&bytes, "add", 2, 3), 5);
    assert_eq!(run_i32_binop(&bytes, "add", 100, 7), 107);
}

#[test]
fn max_u32_with_block_params() {
    let mut be = WasmBackend::new();
    let (entry, params) = be.begin_function(
        "max",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    let a = params[0][0];
    let b = params[1][0];

    let then_block = be.create_block();
    let else_block = be.create_block();
    let merge = be.create_block();
    let result = be.add_block_param(merge, LirType::U32);

    be.switch_to_block(entry);
    let cond = be.icmp(IcmpPred::Uge, a, b);
    be.branch(
        cond,
        then_block,
        BranchTarget::args(vec![]),
        else_block,
        BranchTarget::args(vec![]),
    );

    be.switch_to_block(then_block);
    be.jump(merge, BranchTarget::args(vec![a]));

    be.switch_to_block(else_block);
    be.jump(merge, BranchTarget::args(vec![b]));

    be.switch_to_block(merge);
    be.ret(&[result]);
    be.end_function();

    let bytes = be.finish();
    assert_eq!(run_i32_binop(&bytes, "max", 3, 9), 9);
    assert_eq!(run_i32_binop(&bytes, "max", 11, 4), 11);
}

#[test]
fn replay_into_many_targets() {
    let mut rec = RecordingTarget::new();
    let (entry, params) = rec.begin_function(
        "add",
        &[LirType::I32, LirType::I32],
        Some(LirType::I32),
    );
    rec.switch_to_block(entry);
    let sum = rec.add(params[0][0], params[1][0]);
    rec.ret(&[sum]);
    rec.end_function();
    let saved: SavedLirModule = rec.finish();

    let mut backends = [WasmBackend::new(), WasmBackend::new()];
    saved.replay_into_many(&mut backends);
    let [b0, b1] = backends;
    let bytes0 = b0.finish();
    let bytes1 = b1.finish();
    assert_eq!(run_i32_binop(&bytes0, "add", 10, 32), 42);
    assert_eq!(run_i32_binop(&bytes1, "add", 1, 2), 3);
}

#[test]
fn arr_helpers_round_trip() {
    let mut b = WasmBackend::new();
    let (entry, _) = b.begin_function("arr_get1", &[], Some(LirType::I32));
    b.switch_to_block(entry);
    let e0 = b.iconst(LirType::I32, 1);
    let e1 = b.iconst(LirType::I32, 2);
    let e2 = b.iconst(LirType::I32, 3);
    let arr = b.arr_new(&[e0, e1, e2]);
    let twenty = b.iconst(LirType::I32, 20);
    let arr2 = b.arr_set(&arr, 1, 1, &[twenty]);
    let v = b.arr_get(&arr2, 1, 1);
    b.ret(&v);
    b.end_function();
    let bytes = b.finish();

    let engine = Engine::default();
    let module = Module::new(&engine, bytes).expect("wasm");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).unwrap();
    let func = instance
        .get_typed_func::<(), i32>(&mut store, "arr_get1")
        .unwrap();
    assert_eq!(func.call(&mut store, ()).unwrap(), 20);
}
