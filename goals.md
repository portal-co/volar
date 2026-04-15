# Volar Goals

Volar's overall goal is to increase adoption of program-related cryptography by
implementing it, compiling it to other targets, and developing it publicly.
The task list below tracks progress toward that goal within the current
implementation focus (VOLE-based ZK, garbled circuits, compiler toolchain).

## Completed

- [x] LIR (low-level IR target) — compiler IR → native-like code
  - [x] `LirTarget` trait (SSA with block parameters, integer types, arithmetic, control flow)
  - [x] `CBackend` (C99 emitter for testing)
  - [x] `BIrBlocks` / `IRBlocks` → `LirTarget` lowering (scalar types)
  - [x] `IrModule` → `LirTarget` lowering (primitive types, binary/unary ops, if/else)
  - [x] array types (`LirType::Arr`) + `arr_new`/`arr_get`/`arr_set`
  - [x] struct types (`LirType::Struct`, `StructDef`) + `define_struct`/`struct_new`/`struct_get`
  - [x] extern calls (`call_extern`)
  - [x] generic monomorphization (`MonoEnv`, const-param substitution)
  - [x] struct registry (IrModule.structs → define_struct)
  - [x] `Field`, `Index`, `StructExpr`, `FixedArray` lowering
  - [x] `ArrayGenerate`, `RawMap`, `RawZip` unrolling
  - [x] `BoundedLoop` lowering (loop-header block params: counter + limit)
  - [x] `.clone()` method call (identity), crypto method calls (→ `call_extern`)
- [x] implement encryption
  - [x] garbled circuits (evaluator, garbler, GarbledCircuit, EvalSetup)
  - [x] garble weaver: `BIrBlocks` → evaluator/garbler `IrModule`
  - [x] garbler returns fixed-size `[GarbleTable<N>; A]` (no heap allocation)
- [x] implement proving
  - [x] VOLE ZK proving: Quicksilver-style AND check (`volar-spec/src/vole/prove.rs`)
  - [x] VOLE weaver: `BIrBlocks` → prover/verifier `IrModule` (returns fixed-size hat array)
  - [x] prove helpers in total-Rust subset, parseable by compiler
- [x] primitives compile
  - [x] macros removed, lib.rs parseable by volar-compiler
  - [x] manifest generation (`volar-codegen manifest`)
  - [x] compilable field ops (gf_mul/gf_invert for u8/u64/u128/U256)
  - [x] TS transpilation of field ops
  - [x] GF(2^128) and GF(2^256) with Itoh–Tsujii inversion
  - [x] Galois inversion fixed (was computing wrong exponent)
- [x] static printer fixes
  - [x] `ArrayGenerate` → `Array::<T, N>::from_fn(|i| body)` (was `(0..n).map(...).collect`)
  - [x] `IrType::Array { GenericArray }` → `Array<T, N>` (was `Vec<T>`)
  - [x] `LengthOf(TypeParam)` → `N::USIZE` (was lowercase `n`)
  - [x] `IrType::Array { FixedArray }` → `[T; n]` (was `[T; typenum::Un]`)
- [x] compiler: compound assignment parsing (`^=`, `<<=`, `>>=` → `AssignOp`)
- [x] compiler: `u128` as primitive type

## In Progress

- [ ] full compile and link
  - [x] primitives can link to spec (manifest system)
  - [ ] registry-driven primitive discovery (Phase 2 of primitives plan — deferred)

## Planned

- [ ] implement waffle → vaffle lowering
- [ ] implement vaffle → volar ir lowering
- [ ] interactive proofs (multi-round protocol)
- [ ] consistency checks
- [ ] succinct proofs
- [ ] MPC protocol (beyond type stubs)
- [ ] VOLE setup (OT-based correlation generation)
- [ ] batched Quicksilver verification (random-challenge version)
- [ ] additional ZK proof schemes (non-VOLE-based)
- [ ] additional garbled circuit schemes
- [ ] services / integrations that embed these protocols (e.g. threshold signing, private queries)

## LIR Remaining

- [ ] memory operations (load/store) in `LirTarget`
- [ ] `StorageRead` / `StorageWrite` in `lower_ir`
- [ ] multi-element vector types in `IRBlocks` lowering
- [ ] `IterLoop` over non-array collections

Prefer dynamically creating subgoals to handling entire goals at a time; AI agents, add this to files and memory.
