# Weaving & Multi-Backend Architecture

> Load when working on FHE/garbled-circuit weaving, compiler printers (Rust/TS/C), action system, or CFG emission.

## Garbler + Evaluator Pairing

Any weaving pass targeting a garbled circuit scheme must generate **both** variants:
- **Evaluator side**: takes precomputed `GarbleTable<N>` inputs and evaluates the circuit using `and_via_table`, free-XOR, etc.
- **Garbler side**: takes `GlobalSecret<N>` and input `Garble<N>` wire labels, produces `GarbleTable<N>` for each AND gate.

These variants must be co-designed so their protocols are compatible (same table order, same wire convention).

## Multi-Backend Module Types

- **`IrCfgModule`**: `structs`, `enums`, `traits`, `impls`, `functions: Vec<IrCfgFunction>`, `auxiliary_functions: Vec<IrFunction>`, `type_aliases`
- **`IrModule`**: same but `functions: Vec<IrFunction>` (flat bodies), no `auxiliary_functions`

## Rust CFG Printer (`printer.rs`)

`CfgModuleWriter` implements `RustBackend` trait. `CfgFunctionWriter` emits a state machine:
```rust
let mut __state: usize = 0;
loop {
    match __state {
        0 => { ... }
        ...
        _ => unreachable!()
    }
}
```
Block params use `Option<T>` slots (`__bN_pK`). Terminators: `Return` -> `return`, `Goto` -> assign slots + `continue`, `CondGoto` -> `if/else` + assign + `continue`.

## TypeScript CFG Printer (`printer_ts.rs`)

`TsCfgModuleWriter` + `TsCfgFunctionWriter`. State machine:
```typescript
while (true) {
    switch (__state) {
        case N: ...
    }
}
```
Block params use `T | undefined` slots with `!` non-null assertion for binding. Enums -> tagged union types with factory functions. Type aliases supported. Auxiliary functions go through `TsFunctionWriter`.

**Design decisions:**
- `print_cfg_module_ts` builds a temporary flat `IrModule` from the CFG module's auxiliary functions/structs/impls, runs `deshadow_module` and `build_module_witness_map`/`collect_erased_type_params` on it, then reassembles a deshadowed `IrCfgModule` for printing. This ensures spec functions get proper witness analysis and deshadowing.
- TS doesn't have const generics — `TsCfgFunctionWriter` filters to type-only generics.

## TypeScript Flat Printer (`printer_ts.rs`)

`TsBackend` trait + `TsFmt` wrapper (like `RustBackend`/`DisplayRust`). `TsModuleWriter` handles flat `IrModule`. `TsFunctionWriter` for flat functions. `TsClassWriter` for structs -> classes. Uses `TsContext` with witness maps and erased type params.

## LIR Codegen (`volar-lir-codegen`)

- **Flat**: `lower_module<T: LirTarget>(module: &IrModule, target: &mut T)` — flat only.
- **CFG**: `lower_cfg_module<T: LirTarget>(module: &IrCfgModule, target: &mut T)` — direct block map. Each CFG block maps to a LIR block. Block params become `add_block_param` calls. Terminators use native `jump`/`branch`/`ret`. Auxiliary functions lowered through existing `lower_function_in_module`.

## C Backend (`volar-c-backend`)

`CBackend` implements `LirTarget`. Usage: `CBackend::new()` -> `lower_cfg_module(&module, &mut backend)` -> `backend.finish()` -> C string.

## Action System Design

1. **Proc macro** (`#[volar_action]`) in `volar-macros` — identity transform that passes through the function unchanged.
2. **Actions emit as unbound attributes** wrapping function declarations in generated Rust code.
3. **The attribute wraps a function declaration** — the function is what gets called; the attribute provides context-dependent behavior.
4. **Slice parameters for storages** — storage params should be `&mut [T]` slices.
5. **Action registry**: `blocks.actions: Vec<ActionDecl>` has signatures (params/results type IDs). `FheActionConfig` on the scheme has `output_public` flags. Together these provide enough info to emit typed action function declarations.

## CFG Emission Details

### How the CFG weaver emits Constants and ActionCalls

- **Constants are always public**: `Const(0, u64)` emits `[false; 64]` (type `[bool; 64]`), `Const(1, Bit)` emits `true` (type `bool`). Promotion to `LweCiphertext` happens at use sites where needed via `tfhe_trivial_encrypt`.
- **ActionCall emission (public guard)**: `oram_begin_0(guard_bool, fallbacks..., args...)` — first arg is `bool`, then fallback values (from Const, so always public-typed), then actual args (encrypted-typed).
- **ActionOutput**: `IrExpr::Field { base: call_var, field: "0" }` — tuple index access `.0`, `.1`, etc. — so **action functions must always return a tuple**, even for single results.
- **Variable types use `ty: None`** in `IrStmt::Let` — Rust's type inference determines the type from the expression.

### Action function stubs

For compile checks, action function bodies use `IrExpr::Unreachable` which prints as `unreachable!()` — this returns `!` which coerces to any return type. Parameters: guard `bool`, fallbacks as public types, args as wire (encrypted) types. Generics include `scheme.generics()`. Return type: always `IrType::Tuple(ret_elems)`.

### Storage emission in CFG path

- **StorageRead/StorageWrite** use `format!("storage_{}_{}", storage.0, ty.0)` where `ty` is `IRTypeId`.
- Storage elements are compound types (e.g., `[LweCiphertext<N_LWE>; 4096]` for ORAM path).
- Public addresses use `bools_to_usize(&addr)` wrapper.

### Turbofish generics

- **Action function calls** need `::<N_LWE, BIG_N, BS_ELL, KS_ELL>` turbofish because Rust can't infer const generics from return position alone.
- **`tfhe_trivial_zero`/`tfhe_trivial_one`/`tfhe_trivial_encrypt`** all need `::<N_LWE>` turbofish.

### IR Type Structure Notes

- `IrEnum` uses `kind: StructKind` for its name (not `name`), and `IrEnumVariant.fields` is `IrEnumVariantData` enum (Unit/Tuple(Vec<IrType>)/Struct(Vec<IrField>)), not a simple `Vec`.
- `IrTypeAlias` uses `target: IrType` (not `ty`).
- `StructKind` is `GenericArray` or `Custom(String)`, implements `Display`.
- `flatten_scalar_types` takes `(&LirType, &StructRegistry)` — needs the registry parameter.

## Compile Check Infrastructure

- `run_compile_check_tfhe_cfg` — writes generated Rust to temp dir alongside spec sources, runs `rustc --edition 2021`.
- `run_compile_check_ts` — writes generated TS, runs `tsc --noEmit` (skips if `tsc` not found).
- `run_compile_check_c` — writes generated C, runs `cc -fsyntax-only` (skips if `cc` not found).
- All in `tests_common` module within `volar-weaver`.

## Relevant Files

- `crates/compiler/volar-compiler/src/printer.rs` — Rust `CfgModuleWriter`/`CfgFunctionWriter`
- `crates/compiler/volar-compiler/src/printer_ts.rs` — TS flat + CFG printers
- `crates/compiler/volar-compiler/src/ir.rs` — `IrCfgModule`, `IrCfgFunction`, `IrCfgBlock`, `IrCfgTerminator`, `IrEnum`, `IrTypeAlias`
- `crates/ir/volar-lir/src/lib.rs` — `LirTarget` trait
- `crates/compiler/volar-lir-codegen/src/lib.rs` — flat + CFG lowering
- `crates/compiler/volar-c-backend/src/lib.rs` — `CBackend`
- `crates/compiler/volar-weaver/src/fhe.rs` — weaving + print functions
- `crates/compiler/volar-weaver/src/oram.rs` — ORAM rewrite + integration tests
- `crates/macros/volar-macros/src/lib.rs` — `#[volar_action]` proc macro
