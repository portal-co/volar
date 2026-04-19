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
- [x] IR lowering config (`volar-ir-config` crate) — `IrLoweringConfig` struct with word/pointer width, configurable per lowering target; `lower_ir` keeps backward-compatible default
- [x] `Poly` statement generalized: `ty: TypeId` field carries the output type (Bit for GF(2) gate, or any bitvector/field element); all construction and match sites updated
- [x] FHE CFG weaver: typed block parameters — `wire_type_for_ir` / `public_type_for_ir` trait methods replace hardcoded `wire_ty`; return type derived from circuit structure; `promote_to_wire` extended with `width` argument for multi-bit public→encrypted promotion

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
- [ ] GRAFHEN: replace/fix current stub with full scheme design (currently experimental placeholder)
- [ ] CKKS: approximate homomorphic encryption for real-number computation
- [ ] BFV/BGV: exact integer homomorphic encryption schemes
- [ ] TFHE: fully homomorphic encryption over binary circuits (bootstrapping-based)
  - [ ] FheScheme trait + generic weaver infrastructure (prerequisite; enables TFHE, CKKS, BFV/BGV)
  - [ ] TfheScheme reference implementation (LWE ciphertexts, gate bootstrapping, bootstrapping keys)
- [ ] services / integrations that embed these protocols (e.g. threshold signing, private queries)

## LIR Remaining

- [ ] memory operations (load/store) in `LirTarget`
  - [ ] `LirType::Ptr` + `StackAllocExt` trait (`alloca`, `ptr_load`, `ptr_store`, `ptr_offset`) — opt-in via `LirTarget::stack_alloc_ext()`
  - [ ] ABI optimization in `lower_lir.rs`: when `stack_alloc_ext()` is `Some`, pass large struct arguments by pointer instead of flat scalar expansion
- [ ] `StorageRead` / `StorageWrite` in `lower_ir`
- [ ] multi-element vector types in `IRBlocks` lowering
  - [x] `IrType::Vec(N, T)` bit-width computation via `ir_type_bits` helper (fixes Splat and Poly lowering in LIR)
  - [ ] `PrimType::_128` / `PrimType::_256` multi-word primitive support in LIR (currently `unimplemented!`)
  - [ ] field element types (`PrimType::AES8`, `PrimType::Galois64`) in FHE CFG weaving (deferred)
  - Note: `AES8` and `Galois64` constants will be integrated into constant folding/polynomial
    simplification in a separate pass, since both types can be emulated (inefficiently) with
    plain `Bit` values. The initial `volar-ir-opt` folding pass conservatively skips monomials
    whose variables have these types rather than risk incorrect field arithmetic.
- [ ] `AES8`/`Galois64` constant folding in `volar-ir-opt` (separate pass after emulation support)
- [ ] `IterLoop` over non-array collections

## Misc

- [ ] Harden
  - [ ] Formal security proofs (Lean)
  - [ ] Use Mythos, if this gets popular

Prefer dynamically creating subgoals to handling entire goals at a time; AI agents, add this to files and memory.
