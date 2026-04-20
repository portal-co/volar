# Volar Progress Tracker

> Load at the start of a session to see what's done and what's next.

## Current Goal

Implement **weaver integration** — compiling the ORAM runtime through the volar-compiler pipeline and stitching it into the circuit (NOT by adding special modes to the existing FHE weaver).

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

### Runtime Linking & Action Fallback Fixes
- Created `runtime/helpers.rs` — total Rust `bools_to_usize` implementation, parsed through volar-compiler pipeline
- Added `runtime_linked_spec()` in `oram.rs` — `include_str!` + parser pattern for runtime helpers
- Updated `weave_fhe_cfg` `bools_to_usize` stub to check `already_linked` flag
- Fixed action stub fallback types: fallback params now match return element types (wire-typed when `!is_output_public`)
- Fixed call-site fallback promotion: public fallbacks promoted to wire type via `tfhe_trivial_encrypt` / `RawMap` (same pattern as arg promotion)
- Fixed Rust printer 1-element tuple expressions: `(x,)` not `(x)`
- Replaced `unreachable!()` in action stub bodies with fallback return bodies
- 4 new linking tests: `runtime_helpers_parses_from_include`, `runtime_helpers_round_trips_through_printer`, `runtime_helpers_compile_check`, `woven_module_uses_linked_bools_to_usize`

### E2E Execution Test
- First test that compiles AND runs generated FHE code (not just compile-check)
- `test_tfhe_cfg_and_executes_correctly`: weaves AND circuit through TFHE, generates keys, encrypts inputs, runs the circuit, decrypts, verifies truth table
- Uses noiseless (noise_bits=0) parameters for deterministic correctness
- Follows `test_multi_eval_and` temp-crate pattern from garble.rs

### Multi-Backend Support
- Added TS CFG printer (`TsCfgModuleWriter`, `TsCfgFunctionWriter`, `ts_write_enum`, `ts_write_type_alias`, state machine emission) in `printer_ts.rs`
- Added `print_cfg_module_ts` public API function with proper deshadowing and witness analysis
- Added LIR CFG lowering (`lower_cfg_module`, `lower_cfg_function`, `lower_cfg_terminator`, `lower_jump_args`) in `volar-lir-codegen/src/lib.rs`
- Added `print_fhe_cfg_module_ts` and `print_fhe_cfg_module_c` in `fhe.rs`
- Added `volar-lir`, `volar-lir-codegen`, `volar-c-backend` dependencies to `volar-weaver/Cargo.toml`
- Added `run_compile_check_ts` and `run_compile_check_c` helpers in `tests_common`
- Refactored `rewrite_link_weave_integration` to use shared `build_oram_cfg_module` helper
- Added `rewrite_link_weave_integration_ts` and `rewrite_link_weave_integration_c` tests
- All 6 linking integration tests pass, all 72 base tests pass

### ExternalKind::TypeStub Fix
- Added `ExternalKind::TypeStub` variant to `volar-compiler/src/ir.rs`
- Added `helper_type_stubs()` method to `FheScheme` trait (default: empty vec)
- Implemented `helper_type_stubs()` for `TfheScheme` — returns 4 helper stubs
- Added `scheme.helper_type_stubs()` call in `weave_fhe_cfg` to inject stubs into all modules
- Rust printer and TS printer skip `TypeStub` functions
- LIR codegen registers `TypeStub` in `func_sigs` for return type inference, excludes from `external_fns`
- Removed old `add_fhe_helper_stubs()` function and clone+mutate in `print_fhe_cfg_module_c()`

### LIR / C Backend Infrastructure
- Added `monomorphize_cfg_module` to `mono.rs`
- Added `lenient` field to `StructRegistry` in `structs.rs`
- Extended `infer_type` for tuple field access, Call return types, Index on Reference<Array>, Lit, FixedArray
- Fixed action ABI parsing in `lower_call`
- Added tuple registration infrastructure (synthetic structs, 1-element unwrapping)
- Fixed `IrExpr::Unary` for Ref/RefMut/Deref — passes through all flat values
- Fixed `ir_type_to_lir_inner` for Reference<Slice<T>> → Ptr(T)
- Added `ptr_index_load` / `ptr_index_store` to `LirTarget` trait and C backend
- Added `lower_assign` function for Index-based assignments
- Changed action/oracle `lower_call` to use declared param types
- Added `c_field_name()` helper for C-safe numeric field names
- Merged array/struct typedefs into unified `all_typedefs: Vec<String>` in C backend

### Reliability Tags & Docs
- Added `@reliability:` and `@ai: assisted` tags to all modified compiler files
- Files tagged: `ir.rs`, `printer.rs`, `printer_ts.rs`, `lir-codegen/lib.rs`, `mono.rs`, `structs.rs`, `c-backend/lib.rs`, `volar-lir/lib.rs`
- ORAM/channel files already tagged `experimental` + `assisted`: `oram.rs`, `volar-oram/lib.rs`, `volar-oram-core/lib.rs`, `volar-channel/lib.rs`
- Created `AGENTS.md`, `PROGRESS.md`, and `docs/agent-context/` topic files

### OramConfig Flow Automation
- Added `tree_cell_count()` method to `OramConfig` — returns `Some(num_nodes)` for matching tree storage
- Added `oram_cell_count_fn()` — builds the closure for `derive_ir_storage_config` from a slice of configs
- Replaced manual closures in `rewrite_link_weave_read_write_e2e` and `build_oram_cfg_module` with `oram_cell_count_fn`
- Added `apply_mono()` method to `OramConfig` — feeds Z, B, L, N into a `MonoEnv` for monomorphization
- Replaced manual `.with_len("Z", ...)` chain in `rewrite_link_weave_integration_c` with `c.apply_mono(env)`
- Added 7 new config tests: `tree_cell_count_matches_own_storage`, `tree_cell_count_returns_none_for_other_storage`, `oram_cell_count_fn_returns_nodes_for_tree_storage`, `oram_cell_count_fn_handles_multiple_configs`, `apply_mono_adds_z_b_l_n`, `apply_mono_preserves_existing_params`, and one multi-config test

### ORAM Three-Phase Handler Decomposition
- Added `ActionBeginState` struct and three handler methods on `OramClient`:
  - `handle_begin()` — position map lookup, new leaf assignment
  - `handle_process()` — absorb path, read/write, pack stash, compute eviction targets
  - `handle_evict()` — absorb eviction path, pack stash back
- Added 5 handler unit tests (including `test_three_phase_matches_local_oracle`)
- Added 2 property-based tests (`prop_o_three_phase_equivalence`, `prop_p_three_phase_no_duplicates`) — 200 cases each

## In Progress

- **Weaver integration** — compiling the ORAM runtime through the volar-compiler pipeline

### Tail Call Support (completed this session)
- Added `VaffleTarget::ret_call(name, args)` in its own `impl VaffleTarget` block (`target.rs`) — emits `Terminator::ReturnCall` directly, no `Value::Call` node
- Updated `waffle_lower.rs` `Terminator::ReturnCall` arm to call `tgt.ret_call` instead of `call_extern + ret`
- Added `Terminator::ReturnCall` arm in `lower_to_ir.rs` `translate_terminator` — retreats SP by `own_layout.spill_base`, writes callee args, advances SP by callee size, jumps to callee entry
- Added 3 new tests: `test_ret_call_emits_return_call_terminator` (target), `test_return_call_lowers_to_direct_jump` and `test_return_call_no_spill_no_cont_write` (lower_to_ir)
- Fixed `stack_alloc_ext` and `abi` methods accidentally dropped from `impl LirTarget` during prior session
- All 26 `volar-vaffle-target` tests pass; full workspace clean

## Deferred

- **Action dispatch** — connecting `#[volar_action]` stubs to `OramClient` handlers at runtime. Deferred because it would require giving the proc-macro attribute extra meaning beyond its current identity-transform role, which is out of scope outside of tests.

## Next Steps (in order)

1. **Reliability tags** — tag newly modified files
2. **Further E2E tests** — more complex circuits (multi-block, ORAM storage) with execution verification

## Key Files

| File | Tags | Role |
|---|---|---|
| `crates/compiler/volar-compiler/src/ir.rs` | normal, assisted | Core IR types, `ExternalKind::TypeStub` |
| `crates/compiler/volar-compiler/src/printer.rs` | normal, assisted | Rust code emitter, CFG + flat |
| `crates/compiler/volar-compiler/src/printer_ts.rs` | normal, assisted | TypeScript code emitter, CFG + flat |
| `crates/compiler/volar-lir-codegen/src/lib.rs` | normal, assisted | LIR lowering from IR |
| `crates/compiler/volar-lir-codegen/src/mono.rs` | normal, assisted | Monomorphization pass |
| `crates/compiler/volar-lir-codegen/src/structs.rs` | normal, assisted | Struct registry for LIR |
| `crates/compiler/volar-c-backend/src/lib.rs` | normal, assisted | C99 backend |
| `crates/ir/volar-lir/src/lib.rs` | normal, assisted | LirTarget trait |
| `crates/compiler/volar-weaver/src/fhe.rs` | experimental, assisted | FHE weaver + scheme trait |
| `crates/compiler/volar-weaver/src/oram.rs` | experimental, assisted | ORAM rewriting + weaving |
| `crates/oram/volar-oram/src/lib.rs` | experimental, assisted | Recursive Path ORAM |
| `crates/oram/volar-oram-core/src/lib.rs` | experimental, assisted | ORAM core (no_std, zero deps) |
| `crates/channel/volar-channel/src/lib.rs` | experimental, assisted | Packet-based channel |
