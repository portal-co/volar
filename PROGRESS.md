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

### Virtualisation Pass (new crate)
- New crate `crates/ir/volar-ir-virt/` (Tier 2, `experimental` + `assisted`)
  with `virtualize_ir` and `virtualize_bir`: deduplicates blocks into unique
  *instruction handlers*, rewrites the module into dispatcher + handlers,
  and emits a bytecode table (in-IR `StorageWrite`s at `StorageId::VIRT_BYTECODE`
  and/or an external `VirtBytecode` artefact).  Primary size win for the
  Rust / TS / C backends (`N_blocks` → `N_handlers`)
- Public dispatch: `IRTerminator::JumpTable` for IR, balanced `CondJmp`
  cascade for BIR
- Oblivious dispatch: composes `virtualize_*` output with `movfuscate_*`,
  producing a flat self-looping block using the shared slot-accumulator
  helpers
- Added `StorageId::VIRT_BYTECODE` constant to `volar-ir-common`
- Extracted `dispatch_accumulator` helper module in `volar-ir-passes` —
  `emit_is_block`, `encode_pc_bits`, `emit_select_bit`, `emit_select_slot`
  moved to free generics over `DispatchBitPrimitives` /
  `DispatchSlotPrimitives` so `movfuscate` and `volar-ir-virt` share one
  implementation.  `MovfuscCtx` public surface unchanged (default
  methods now delegate via a local `MovfuscShim`)
- Added `JumpTable` case to the `eval_ir` interpreter in
  `volar-fuzz::interpreter::ir` (previously panicked — needed to
  evaluate the Public-dispatch output)
- Tests: `crates/ir/volar-ir-virt/tests/equiv_ir.rs`,
  `equiv_bir.rs`, `dedup_invariant.rs`, `fhe_integration.rs`
  (virtualise + movfuscate composition check) — all 15 passing
- Fuzz: `crates/fuzz/volar-fuzz/src/properties/virt.rs` —
  `prop_virt_ir_preserves_semantics`,
  `prop_virt_bir_preserves_semantics`, and two `does_not_panic`
  properties, all passing

### Virtualisation v2 — register-based IR calling convention (this session)
- `virtualize_ir` rewritten to route cross-block values through a
  per-type *register file* stored in dedicated `StorageId`s (one
  `StorageId` per IR type used as a param / return arg).  Block
  parameters are no longer required to share the same arity or
  signature; each block reads its inputs from register indices encoded
  in the bytecode and writes its terminator args into the target
  block's register indices.
- `Jmp(Return, args)` is now a *special branch* — each arm carries a
  `done: Bit` immediate alongside `next_pc: _32`; the dispatcher
  branches on `done` to a Return sub-block that reads the return
  registers and exits via `Jmp(Return, [vals…])`.
- Conditional branches (`JumpCond` / `JumpTable`) materialise one
  arm-sub-block per target; each sub-block performs its own arg
  forwarding + jump to the dispatcher with its own `(next_pc, done)`.
  Conditions remain structural canonical-var references (register
  reads happen at handler entry).
- Added `StorageId::VIRT_REGISTERS_BASE` to `volar-ir-common`; the
  register file is placed *above* the bytecode range at pass time
  (`bytecode_storage + 1 + total_bytecode_slots`) to avoid collisions.
- Extended per-handler bytecode schema with five slot kinds:
  `ParamSrcReg` (where to read each block param from),
  `ConstValue` (lifted `Stmt::Const`), and per arm `TargetPc` +
  `DoneFlag` + `ArgDstReg` (one dst-register-index per arg).  All
  address-typed slots are `_32`; the done flag is `Bit`.
- Return-shape extraction scans every `Jmp(Return, args)` (including
  conditional arms and jump-table cases) and panics if they disagree,
  making the function's return shape a well-defined property of the
  input module.
- New equivalence tests in `tests/equiv_ir.rs`:
  `varied_param_blocks_semantics`, `varied_param_blocks_dedup_count`,
  `multi_type_return_semantics` (mixed `_32` + `Bit` return arity),
  and `jumpcond_with_return_arm_semantics` (`JumpCond` with a Return
  arm + a Block arm).  All 11 IR tests + 6 BIR tests + 2 dedup-invariant
  tests + 1 FHE-integration test pass.
- Fuzz properties unchanged and still pass (4 properties, 256+ cases each).

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

- **TS primitives completeness** — `printer_ts.rs` currently handles `Z3` in preamble, `is_primitive_class`, `runtime_type_check`, `ts_primitive`, and `ts_default_value`. Future schemes adding new `PrimitiveType` variants will need corresponding entries in all five locations. Consider a table-driven or trait-based dispatch to enforce completeness at compile time.
- **Action dispatch** — connecting `#[volar_action]` stubs to `OramClient` handlers at runtime. Deferred because it would require giving the proc-macro attribute extra meaning beyond its current identity-transform role, which is out of scope outside of tests.
- **`volar-ir-virt` `BytecodeForm::External` printer integration** — Rust / TS / C backends currently ignore the returned `VirtBytecode` artefact.  A follow-up task should teach the printers to emit it as a `const BYTECODE: &[u8]` (or equivalent) plus a small dispatch shim.  Out of scope for v1 because the in-IR form already gives the full correctness story.
- **`virtualize_ir` + Oblivious full pipeline** — blocked on `movfuscate_ir` learning to handle `IRTerminator::JumpTable`.  Until that lands, IR Oblivious dispatch panics when the input has more than one handler.  BIR Oblivious dispatch works today because BIR's Public dispatch uses a `CondJmp` cascade rather than a `JumpTable`.
- **`volar-ir-virt` `DedupPolicy::Maximal`** — lifting scalar-in-stmt fields (`Rol.n`, `Shuffle.result_bits`, BIR `storage`, RNG/oracle names) as immediates.  Enum variant is reserved; currently panics.
- **`virtualize_bir` varied block-param counts** — the BIR path still requires all blocks to share the same `params: u32` (its v1 calling convention threads bit state through block params rather than a register file).  The IR register-file approach can be mirrored to BIR (single `Bit` register file), but is deferred; typical BIR post-lowering modules already have uniform param counts so this is not blocking.

## Next Steps (in order)

1. **Reliability tags** — tag newly modified files
2. **Further E2E tests** — more complex circuits (multi-block, ORAM storage) with execution verification
3. **Virtualisation External bytecode printer hookup** — emit the `VirtBytecode` artefact as a `const` + runtime shim in the Rust / TS / C backends

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
| `crates/ir/volar-ir-virt/src/lib.rs` | experimental, assisted | Virtualisation pass public API + `VirtualizeConfig` |
| `crates/ir/volar-ir-virt/src/canon.rs` | experimental, assisted | `IrHandlerKey` / `BirHandlerKey` canonicalisation |
| `crates/ir/volar-ir-virt/src/ctx.rs` | experimental, assisted | Shared `DedupTable`, `VirtOutput` |
| `crates/ir/volar-ir-virt/src/ir.rs` | experimental, assisted | `virtualize_ir` (JumpTable dispatch) |
| `crates/ir/volar-ir-virt/src/bir.rs` | experimental, assisted | `virtualize_bir` (CondJmp-cascade dispatch) |
| `crates/ir/volar-ir-virt/src/bytecode.rs` | experimental, assisted | `VirtBytecode` external artefact |
| `crates/ir/volar-ir-passes/src/dispatch_accumulator.rs` | normal, assisted | Shared `emit_is_block`, `encode_pc_bits`, `emit_select_bit`, `emit_select_slot` |
