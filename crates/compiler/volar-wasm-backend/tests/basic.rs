// @reliability: normal
//! Integration tests: build a module directly against `WasmBackend`'s
//! `LirTarget` impl (no `IrModule`/`volar-lir-codegen` driver involved --
//! that's covered by other backends' own tests already), then run the
//! resulting bytes with `wasmtime` and check the result.

use volar_lir::{BranchTarget, IcmpPred, LirTarget, LirType};
use volar_wasm_backend::{ThreadPerJobExecutor, WasmBackend};

fn run_u32(wasm: &[u8], name: &str, args: (u32,)) -> u32 {
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, wasm).expect("module should validate");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiate");
    let f = instance
        .get_typed_func::<(u32,), u32>(&mut store, name)
        .unwrap_or_else(|_| panic!("{name} should be exported with signature (u32) -> u32"));
    f.call(&mut store, (args.0,)).expect("should not trap")
}

fn run_u32_u32(wasm: &[u8], name: &str, args: (u32, u32)) -> u32 {
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, wasm).expect("module should validate");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiate");
    let f = instance
        .get_typed_func::<(u32, u32), u32>(&mut store, name)
        .unwrap_or_else(|_| panic!("{name} should be exported with signature (u32, u32) -> u32"));
    f.call(&mut store, args).expect("should not trap")
}

fn run_u64_pair(wasm: &[u8], name: &str, args: (u64, u64)) -> u64 {
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, wasm).expect("module should validate");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiate");
    let f = instance
        .get_typed_func::<(u64, u64), u64>(&mut store, name)
        .unwrap_or_else(|_| panic!("{name} should be exported with signature (u64, u64) -> u64"));
    f.call(&mut store, args).expect("should not trap")
}

fn run_no_args_u32(wasm: &[u8], name: &str) -> u32 {
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, wasm).expect("module should validate");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiate");
    let f = instance
        .get_typed_func::<(), u32>(&mut store, name)
        .unwrap_or_else(|_| panic!("{name} should be exported with signature () -> u32"));
    f.call(&mut store, ()).expect("should not trap")
}

// ============================================================================
// Scalar arithmetic
// ============================================================================

#[test]
fn test_add_two() {
    let mut b = WasmBackend::new();
    LirTarget::declare_function(&mut b, "add_two", &[LirType::U32, LirType::U32], Some(LirType::U32));

    let (entry, params) = b.begin_function("add_two", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let sum = b.add(params[0][0], params[1][0]);
    b.ret(&[sum]);
    b.end_function();

    let b = b.with_export("add_two");
    let wasm = b.finish();
    assert_eq!(run_u32_u32(&wasm, "add_two", (3, 4)), 7);
}

// ============================================================================
// Countdown loop (sum 1..=10 = 55) -- exercises the dispatch relooper's
// backward edge (loop_block jumps to itself).
// ============================================================================

#[test]
fn test_countdown_loop() {
    let mut b = WasmBackend::new();
    LirTarget::declare_function(&mut b, "countdown", &[LirType::U64, LirType::U64], Some(LirType::U64));

    let (entry, entry_params) = b.begin_function("countdown", &[LirType::U64, LirType::U64], Some(LirType::U64));
    let n_init = entry_params[0][0];
    let acc_init = entry_params[1][0];

    let loop_block = b.create_block();
    let counter = b.add_block_param(loop_block, LirType::U64);
    let accum = b.add_block_param(loop_block, LirType::U64);

    let done_block = b.create_block();
    let done_result = b.add_block_param(done_block, LirType::U64);

    b.switch_to_block(entry);
    b.jump(loop_block, BranchTarget::args([n_init, acc_init]));

    b.switch_to_block(loop_block);
    let zero = b.iconst(LirType::U64, 0);
    let cond = b.icmp(IcmpPred::Eq, counter, zero);
    let new_acc = b.add(accum, counter);
    let one = b.iconst(LirType::U64, 1);
    let new_ctr = b.sub(counter, one);
    b.branch(
        cond,
        done_block,
        BranchTarget::args([accum]),
        loop_block,
        BranchTarget::args([new_ctr, new_acc]),
    );

    b.switch_to_block(done_block);
    b.ret(&[done_result]);
    b.end_function();

    let b = b.with_export("countdown");
    let wasm = b.finish();
    assert_eq!(run_u64_pair(&wasm, "countdown", (10, 0)), 55);
}

// ============================================================================
// if/join pattern (both arms target the same join block with different args)
// ============================================================================

#[test]
fn test_if_max() {
    let mut b = WasmBackend::new();
    LirTarget::declare_function(&mut b, "max_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));

    let (entry, params) = b.begin_function("max_u32", &[LirType::U32, LirType::U32], Some(LirType::U32));
    let a = params[0][0];
    let bv = params[1][0];

    let join_block = b.create_block();
    let result_param = b.add_block_param(join_block, LirType::U32);

    b.switch_to_block(entry);
    let cond = b.icmp(IcmpPred::Ugt, a, bv);
    b.branch(cond, join_block, BranchTarget::args([a]), join_block, BranchTarget::args([bv]));

    b.switch_to_block(join_block);
    b.ret(&[result_param]);
    b.end_function();

    let b = b.with_export("max_u32");
    let wasm = b.finish();
    assert_eq!(run_u32_u32(&wasm, "max_u32", (3, 9)), 9);
    assert_eq!(run_u32_u32(&wasm, "max_u32", (9, 3)), 9);
}

// ============================================================================
// Forward call: `caller`'s body (lowered first) calls `callee`, whose body
// is only lowered afterward -- the scenario eager index allocation exists
// for. caller(x) = (x + 1) * 2.
// ============================================================================

#[test]
fn test_forward_call() {
    let mut b = WasmBackend::new();
    LirTarget::declare_function(&mut b, "caller", &[LirType::U32], Some(LirType::U32));
    LirTarget::declare_function(&mut b, "callee", &[LirType::U32], Some(LirType::U32));

    let (entry, params) = b.begin_function("caller", &[LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let one = b.iconst(LirType::U32, 1);
    let arg = b.add(params[0][0], one);
    let result = b.call_extern("callee", &[LirType::U32], &[arg], Some(LirType::U32));
    b.ret(&result);
    b.end_function();

    // `callee`'s body is lowered *after* `caller`'s, which already referenced
    // it by (already final) index.
    let (entry2, params2) = b.begin_function("callee", &[LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry2);
    let two = b.iconst(LirType::U32, 2);
    let doubled = b.mul(params2[0][0], two);
    b.ret(&[doubled]);
    b.end_function();

    let b = b.with_export("caller");
    let wasm = b.finish();
    assert_eq!(run_u32(&wasm, "caller", (5,)), 12);
}

// ============================================================================
// StackAllocExt: alloca + ptr_offset + ptr_store + ptr_load over linear memory.
// ============================================================================

#[test]
fn test_alloca_sum() {
    let mut b = WasmBackend::new();
    LirTarget::declare_function(&mut b, "sum4", &[], Some(LirType::U32));

    let (entry, _params) = b.begin_function("sum4", &[], Some(LirType::U32));
    b.switch_to_block(entry);

    let ptr = b.stack_alloc_ext().expect("linear memory available").alloca(LirType::U32, 4);

    for (i, v) in [10i64, 20, 30, 40].into_iter().enumerate() {
        let idx = b.iconst(LirType::U32, i as i64);
        let val = b.iconst(LirType::U32, v);
        let sae = b.stack_alloc_ext().unwrap();
        let addr = sae.ptr_offset(ptr.clone(), idx);
        sae.ptr_store(addr, val);
    }

    let mut sum = b.iconst(LirType::U32, 0);
    for i in 0..4i64 {
        let idx = b.iconst(LirType::U32, i);
        let loaded = {
            let sae = b.stack_alloc_ext().unwrap();
            let addr = sae.ptr_offset(ptr.clone(), idx);
            sae.ptr_load(addr, LirType::U32)
        };
        sum = b.add(sum, loaded);
    }
    b.ret(&[sum]);
    b.end_function();

    let b = b.with_export("sum4");
    let wasm = b.finish();
    assert_eq!(run_no_args_u32(&wasm, "sum4"), 100);
}

// ============================================================================
// Background executor: same countdown computation, but forcing every
// function's body through a real background thread (`ThreadPerJobExecutor`)
// instead of `InlineExecutor`, to confirm correctness isn't specific to
// synchronous encoding.
// ============================================================================

#[test]
fn test_background_executor_matches_inline() {
    let wasm = {
        let mut b = WasmBackend::new().with_executor(ThreadPerJobExecutor);
        LirTarget::declare_function(&mut b, "countdown", &[LirType::U64, LirType::U64], Some(LirType::U64));
        let (entry, entry_params) = b.begin_function("countdown", &[LirType::U64, LirType::U64], Some(LirType::U64));
        let n_init = entry_params[0][0];
        let acc_init = entry_params[1][0];
        let loop_block = b.create_block();
        let counter = b.add_block_param(loop_block, LirType::U64);
        let accum = b.add_block_param(loop_block, LirType::U64);
        let done_block = b.create_block();
        let done_result = b.add_block_param(done_block, LirType::U64);
        b.switch_to_block(entry);
        b.jump(loop_block, BranchTarget::args([n_init, acc_init]));
        b.switch_to_block(loop_block);
        let zero = b.iconst(LirType::U64, 0);
        let cond = b.icmp(IcmpPred::Eq, counter, zero);
        let new_acc = b.add(accum, counter);
        let one = b.iconst(LirType::U64, 1);
        let new_ctr = b.sub(counter, one);
        b.branch(cond, done_block, BranchTarget::args([accum]), loop_block, BranchTarget::args([new_ctr, new_acc]));
        b.switch_to_block(done_block);
        b.ret(&[done_result]);
        b.end_function();
        let b = b.with_export("countdown");
        b.finish()
    };
    assert_eq!(run_u64_pair(&wasm, "countdown", (10, 0)), 55);
}
