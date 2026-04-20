# Volar Progress Tracker

> Load at the start of a session to see what's done and what's next.

## Current Goal

Fix TFHE cryptographic implementation bugs in `volar-spec` so all tests pass (Phase 2b), add FHE property-based fuzzing, port XOR composability fix to the weaver, then implement **Recursive Path ORAM with a unified packet-based client-server communication system**. After ORAM core: add ORAM fuzzing, docs, reliability tags, then proceed with **weaver integration** — compiling the ORAM runtime through the volar-compiler pipeline and stitching it into the circuit (NOT by adding special modes to the existing FHE weaver).

The current immediate task is **adding multi-backend compatibility tests** for the weaver.

## Completed

### FHE Weaver Fixes
- Fixed shift overflow in `Const` emission for types > 256 bits (`fhe.rs`)
- Converted `rewritten_ir_weaves_through_cfg` from `#[should_panic]` to passing test
- Added `enums` and `auxiliary_functions` fields to `IrCfgModule` (`ir.rs`)
- Added `apply_cfg` method to `LinkageSystem` (`linkage.rs`)
- Updated `CfgModuleWriter` to print enums and auxiliary functions (`printer.rs`)
- Wired linkage into `weave_fhe_cfg` — replaced no-op with actual `apply_cfg` call (`fhe.rs`)
- Wrote `rewrite_link_weave_integration` test — structural assertions pass
- All workspace tests pass (484+ tests)

### Action System
- Created `volar-macros` proc macro crate with `#[volar_action]` attribute
- Updated `FunctionWriter` in `printer.rs` to emit `#[volar_action]` for `ExternalKind::Action`
- Added action function stub generation to `weave_fhe_cfg`
- Moved `run_compile_check_tfhe_cfg` to shared `tests_common` module
- Fixed action stub return type: always wrap in tuple
- Fixed action stub generics: includes `scheme.generics()`
- Added `volar-macros` dependency to compile check template

### ORAM Integration (Rust backend)
- Fixed StorageWrite semicolon bug: `Semi` instead of `Expr`
- Added `derive_ir_storage_config` function
- Fixed `derive_ir_storage_config` type mismatch
- Wired `derive_ir_storage_config` into the ORAM integration test
- Fixed CFG storage parameter element type
- Added `bools_to_usize` address conversion for public addresses
- Fixed single-element tuple type printing: `(T,)` not `(T)` in `printer.rs`
- Fixed public action arg promotion using `IrExpr::RawMap` (portable `.map()`)
- Added turbofish generics to ActionCall emission
- Added `N_LWE` turbofish to `tfhe_trivial_zero`/`one`/`encrypt`
- ORAM integration test passes (`rewrite_link_weave_integration`)
- All 76 weaver tests (with linking) pass, full workspace passes

### Multi-Backend Support
- Added TS CFG printer (`TsCfgModuleWriter`, `TsCfgFunctionWriter`, `ts_write_enum`, `ts_write_type_alias`, state machine emission) in `printer_ts.rs`
- Added `print_cfg_module_ts` public API function with proper deshadowing and witness analysis
- Added LIR CFG lowering (`lower_cfg_module`, `lower_cfg_function`, `lower_cfg_terminator`, `lower_jump_args`) in `volar-lir-codegen/src/lib.rs`
- Added `print_fhe_cfg_module_ts` and `print_fhe_cfg_module_c` in `fhe.rs`
- Added `volar-lir`, `volar-lir-codegen`, `volar-c-backend` dependencies to `volar-weaver/Cargo.toml`
- Added `run_compile_check_ts` and `run_compile_check_c` helpers in `tests_common`
- Refactored `rewrite_link_weave_integration` to use shared `build_oram_cfg_module` helper
- Added `rewrite_link_weave_integration_ts` and `rewrite_link_weave_integration_c` tests
- All code compiles cleanly (cargo check passes for compiler, lir-codegen, c-backend, weaver)

## In Progress

- **Run the full test suite** to verify no regressions and that the new TS/C tests pass (or gracefully skip if tsc/cc not available)

## Next Steps (in order)

1. **Run and fix the full test suite** — `cargo test --workspace` or at least `cargo test -p volar-weaver --features linking`
2. **Fix any test failures** — the TS and C tests may have issues (TS printer may not handle all IR expression variants used by woven ORAM code, LIR lowering may hit unimplemented types)
3. **After multi-backend tests pass**: move on to ORAM fuzzing, docs, and reliability tags

## Key Files Modified Recently

| File | What changed |
|---|---|
| `crates/compiler/volar-compiler/src/printer_ts.rs` | TS CFG printer (TsCfgModuleWriter, state machine, enums, type aliases) |
| `crates/compiler/volar-lir-codegen/src/lib.rs` | LIR CFG lowering (lower_cfg_module, lower_cfg_function) |
| `crates/compiler/volar-weaver/Cargo.toml` | Added lir, lir-codegen, c-backend deps |
| `crates/compiler/volar-weaver/src/fhe.rs` | print_fhe_cfg_module_ts, print_fhe_cfg_module_c |
| `crates/compiler/volar-weaver/src/lib.rs` | run_compile_check_ts, run_compile_check_c in tests_common |
| `crates/compiler/volar-weaver/src/oram.rs` | build_oram_cfg_module helper, TS/C integration tests |
