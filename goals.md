# Volar Goals

Volar's overall goal is to increase adoption of program-related cryptography by
implementing it, compiling it to other targets, and developing it publicly.
The task list below tracks progress toward that goal within the current
implementation focus (VOLE-based ZK, garbled circuits, ORAM, compiler toolchain).

## Completed

- [x] LIR (low-level IR target) — compiler IR -> native-like code
  - [x] `LirTarget` trait (SSA with block parameters, integer types, arithmetic, control flow)
  - [x] `CBackend` (C99 emitter for testing)
  - [x] `BIrBlocks` / `IRBlocks` -> `LirTarget` lowering (scalar types)
  - [x] `IrModule` -> `LirTarget` lowering (primitive types, binary/unary ops, if/else)
  - [x] array types (`LirType::Arr`) + `arr_new`/`arr_get`/`arr_set`
  - [x] struct types (`LirType::Struct`, `StructDef`) + `define_struct`/`struct_new`/`struct_get`
  - [x] extern calls (`call_extern`)
  - [x] generic monomorphization (`MonoEnv`, const-param substitution)
  - [x] struct registry (IrModule.structs -> define_struct)
  - [x] `Field`, `Index`, `StructExpr`, `FixedArray` lowering
  - [x] `ArrayGenerate`, `RawMap`, `RawZip` unrolling
  - [x] `BoundedLoop` lowering (loop-header block params: counter + limit)
  - [x] `.clone()` method call (identity), crypto method calls (-> `call_extern`)
- [x] implement encryption
  - [x] garbled circuits (evaluator, garbler, GarbledCircuit, EvalSetup)
  - [x] garble weaver: `BIrBlocks` -> evaluator/garbler `IrModule`
  - [x] garbler returns fixed-size `[GarbleTable<N>; A]` (no heap allocation)
- [x] implement proving
  - [x] VOLE ZK proving: Quicksilver-style AND check (`volar-spec/src/vole/prove.rs`)
  - [x] VOLE weaver: `BIrBlocks` -> prover/verifier `IrModule` (returns fixed-size hat array)
  - [x] prove helpers in total-Rust subset, parseable by compiler
- [x] primitives compile
  - [x] macros removed, lib.rs parseable by volar-compiler
  - [x] manifest generation (`volar-codegen manifest`)
  - [x] compilable field ops (gf_mul/gf_invert for u8/u64/u128/U256)
  - [x] TS transpilation of field ops
  - [x] GF(2^128) and GF(2^256) with Itoh-Tsujii inversion
  - [x] Galois inversion fixed (was computing wrong exponent)
- [x] static printer fixes
  - [x] `ArrayGenerate` -> `Array::<T, N>::from_fn(|i| body)` (was `(0..n).map(...).collect`)
  - [x] `IrType::Array { GenericArray }` -> `Array<T, N>` (was `Vec<T>`)
  - [x] `LengthOf(TypeParam)` -> `N::USIZE` (was lowercase `n`)
  - [x] `IrType::Array { FixedArray }` -> `[T; n]` (was `[T; typenum::Un]`)
- [x] compiler: compound assignment parsing (`^=`, `<<=`, `>>=` -> `AssignOp`)
- [x] compiler: `u128` as primitive type
- [x] IR lowering config (`volar-ir-config` crate) — `IrLoweringConfig` struct with word/pointer width, configurable per lowering target
- [x] `Poly` statement generalized: `ty: TypeId` field carries output type (Bit for GF(2) gate, or any bitvector/field element)
- [x] FHE CFG weaver: typed block parameters — `wire_type_for_ir` / `public_type_for_ir` trait methods replace hardcoded `wire_ty`
- [x] compiler: enum support (Rust backend)
  - [x] `IrEnum` / `IrEnumVariant` / `IrEnumVariantData` IR types
  - [x] parser: `convert_enum()` + `convert_enum_variant()` — unit, tuple, struct variants + generics
  - [x] printer: `EnumWriter` — Rust backend (`#[derive(Debug)] pub enum ... { ... }`)
  - [x] round-trip tests (parse -> print, 4 tests passing)
- [x] TFHE property tests (11 proptest properties in `volar-spec`)
- [x] weaver XOR port — all 6 MUX sites use `emit_cmux`, `xor2` decomposed
- [x] `volar-channel` crate — transport-agnostic protocol abstraction
  - [x] `Yield<Done, Msg>` enum, `Protocol` trait, `run_protocol` driver
  - [x] 3 tests passing, `#![no_std] + alloc`
- [x] `volar-oram` crate — Recursive Path ORAM runtime (client-side)
  - [x] `OramTree`, `OramClient`, `PosMap`, `RecursiveOram` types
  - [x] Deterministic reverse-lexicographic eviction (no RNG for eviction target selection)
  - [x] `OramAccessProtocol` + `RecursiveOramProtocol` implementing `Protocol` trait
  - [x] 20 unit tests + 6 property tests (I-N), all passing
- [x] `volar-oram-core` crate — server-side ORAM in total Rust (compiler-parseable)
  - [x] `OramEntry<B>`, `Bucket<Z, B>` (fixed-size arrays, Copy, const generics)
  - [x] `path_indices::<L>()`, `read_path`, `write_path`, `server_step`
  - [x] Helpers: `leaf_in_subtree`, `eviction_target`, `bit_reverse`, `bits_needed`, `log2_floor`
  - [x] 14 unit tests passing, zero dependencies, `#![no_std]`
  - [x] Compiler parse test (`oram_core_parses_successfully`) passing

## In Progress

- [ ] ORAM weaver — compile `volar-oram-core` through `volar-compiler` pipeline
  - [x] Wire `volar-oram` to depend on `volar-oram-core` (deduplicate shared types/functions)
  - [x] Build ORAM weaver: parse `volar-oram-core` -> AST, combine with synthetic module from user code
  - [x] ActionCall-based ORAM interaction (BIR `ActionCall` mechanism for client-server communication)
  - [x] `OramConfig` with IR type construction, action decl/config helpers, `configure_scheme()`
  - [x] `oram_begin_circuit()` — minimal ActionCall IR, weaves through CFG path
  - [ ] Storage-to-ORAM IR transformation: rewrite `StorageRead`/`StorageWrite` on ORAM-backed regions into ActionCall sequences
  - [ ] Integration tests: end-to-end ORAM through compiler pipeline
  - [ ] Server circuit compilation: compile `server_step` into sub-circuit via FHE weaver

## Planned

### Compiler backends (enum support)
- [ ] TypeScript enum backend (discriminated unions)
- [ ] C enum backend (tagged struct + union)

### Linking and discovery
- [ ] full compile and link
  - [x] primitives can link to spec (manifest system)
  - [ ] registry-driven primitive discovery (Phase 2 of primitives plan)

### LIR serialization (deferred)
- [ ] `rkyv` derives on `IRBlocks<P>`, `IRBlock<P>`, `IRTerminator` in `volar-ir` (`ir.rs`) — needed to serialize IR before lowering
- [ ] `rkyv` dep + derives on `vaffle` crate types (`Module`, `FuncBody`, `Block`, `Terminator`) — needed for serialized waffle IR

### LIR remaining
- [ ] memory operations (load/store) in `LirTarget`
  - [ ] `LirType::Ptr` + `StackAllocExt` trait (`alloca`, `ptr_load`, `ptr_store`, `ptr_offset`)
  - [ ] ABI optimization in `lower_lir.rs`: large struct arguments by pointer
- [ ] `StorageRead` / `StorageWrite` in `lower_ir`
- [ ] multi-element vector types in `IRBlocks` lowering
  - [x] `IrType::Vec(N, T)` bit-width computation via `ir_type_bits` helper
  - [ ] `PrimType::_128` / `PrimType::_256` multi-word primitive support in LIR
  - [ ] field element types (`PrimType::AES8`, `PrimType::Galois64`) in FHE CFG weaving
- [ ] `AES8`/`Galois64` constant folding in `volar-ir-opt` (separate pass after emulation support)
- [ ] `IterLoop` over non-array collections

### IR lowering
- [ ] implement waffle -> vaffle lowering
- [ ] implement vaffle -> volar ir lowering

### Cryptographic protocols
- [ ] interactive proofs (multi-round protocol)
- [ ] consistency checks
- [ ] succinct proofs
- [ ] MPC protocol (beyond type stubs)
- [ ] VOLE setup (OT-based correlation generation)
- [ ] batched Quicksilver verification (random-challenge version)
- [ ] additional ZK proof schemes (non-VOLE-based)
- [ ] additional garbled circuit schemes

### Encryption schemes
- [ ] GRAFHEN: replace/fix current stub with full scheme design (currently experimental placeholder)
- [ ] CKKS: approximate homomorphic encryption for real-number computation
- [ ] BFV/BGV: exact integer homomorphic encryption schemes
- [ ] TFHE: fully homomorphic encryption over binary circuits (bootstrapping-based)
  - [ ] FheScheme trait + generic weaver infrastructure (prerequisite; enables TFHE, CKKS, BFV/BGV)
  - [ ] TfheScheme reference implementation (LWE ciphertexts, gate bootstrapping, bootstrapping keys)

### ORAM future work
- [ ] VAFFLE ORAM support — apply storage-to-ORAM transformation at the VAFFLE level for performance
  - [ ] VAFFLE has the same storage system (`Stmt<ValueId>` shares `StorageRead`/`StorageWrite` with IR) and functions — transformation can operate on `vaffle::Module` directly
  - [ ] Benefits: VAFFLE-level rewriting preserves SSA structure and enables VAFFLE-specific optimizations (constant folding, store forwarding) on the ORAM ActionCall sequences before lowering to IR/BIR
  - [ ] Prerequisite: IR-level storage-to-ORAM transformation (validates the design)
- [ ] Port client-side ORAM logic to total Rust (enable full ORAM compilation to circuits)
- [ ] ZK nesting: compile ORAM step function to VOLE-ZK circuit (see `docs/archive/oram-channel-plan.md` Part 4)
- [ ] ORAM threshold tuning (Linear MUX for small storage, ORAM for large)

### Applications
- [ ] services / integrations that embed these protocols (e.g. threshold signing, private queries)

## Misc

- [ ] Harden
  - [ ] Formal security proofs (Lean)
  - [ ] Use Mythos, if this gets popular

Prefer dynamically creating subgoals to handling entire goals at a time; AI agents, add this to files and memory.
