# volar

A Rust implementation of VOLE-in-the-head (VOLEitH) zero-knowledge proofs. The project is early-stage and incomplete.

## What it is

VOLE-in-the-head is a technique for building zero-knowledge proofs from Vector Oblivious Linear Evaluation (VOLE), a two-party cryptographic primitive. The prover holds vectors `u` and `v`; the verifier holds a global secret `Δ`; and the relation `u·Δ + v = q` is maintained. The "in-the-head" construction extends this to proofs about arbitrary computations without requiring an actual OT/VOLE protocol between two live parties.

This repo is an attempt to build a usable VOLEitH library in Rust, with a compiler that can transpile the core spec to other targets (dynamic Rust and TypeScript).

## Repository structure

```
crates/
  spec/
    volar-primitives/   Field element types: Bit, Galois (GF(2^8)), Galois64, BitsInBytes, etc.
    volar-common/       Shared primitives: hash commitments, length-doubling PRG
    volar-spec/         Core VOLE types and protocols (static, typenum-length-parameterized)
    volar-dyn/          Dynamic (heap-allocated, runtime-sized) mirror of volar-spec
    volar-spec-dyn/     Auto-generated dynamic types from volar-compiler; also a manual reference
  ir/
    volar-ir/           Two IRs: Volar IR (SSA, field-aware) and Boolar IR (AND/XOR/NOT boolean circuits)
    vaffle/             Stub crate; placeholder for WAFFLE→VAFFLE translation (not implemented)
  compiler/
    volar-compiler/     Parses volar-spec Rust source via syn, emits dynamic Rust or TypeScript
```

## What is implemented

**volar-primitives**: Field element types with arithmetic. GF(2^8) (`Galois`) uses the AES reduction polynomial `x^8 + x^4 + x^3 + x + 1`, including field inversion. `BitsInBytes` and `BitsInBytes64` are packed bit representations for SIMD-style operations. A `Tropical` semiring is also present.

**volar-common**: `hash_commitment` (commit-open via `H(message || nonce)`). `length_doubling` (a digest-based PRG that doubles its output at each step, used as the PRF backbone for ABO generation).

**volar-spec**: The main cryptographic kernel.
- `Delta<N, T>` and `Q<N, T>`: verifier-side VOLE types. `Delta` is the verifier's global secret; `Q` is the verifier's share.
- `Vope<N, T, K>`: a degree-K polynomial in Δ over a vector of length N. This is the prover's side of the commitment. Degree-K multiplication (`ai_hazmat::mul_generalized`) is included but marked hazmat — it is only safe inside a Quicksilver-style constraint check.
- `BitVole<N, T>`: a VOLE where `u` is a bit vector and `v` is an extension-field vector, representing the subfield VOLE over GF(2).
- `field_rotate`: bit-level rotation and extraction operations on `BitsInBytes`/`BitsInBytes64` arrays. Used for the Galois extension lifting technique (mapping bit-commitments into GF(2^k) for AES-like operations).
- `byte_gen` (ABO protocol): prover and verifier sides of an "almost bit OT" generation protocol, built on the length-doubling PRG and hash commitments. The `gen_abo` function generates the material; prover and verifier each have methods to convert it to VOLE material (`to_vole_material*`) or bit-split material (`split_bit_typenum`). Both 2-party and N-party (`multi_party` feature) variants are implemented, but the N-party protocol has a note that it was revised after a discovered soundness bug and has not been independently verified.
- `garble`: a half-gate garbled circuit scheme over VOLE types (Zahur-Rosulek-Evans 2015). Marked experimental; the VOLE-specific binding has not been reviewed.
- `mpc`: stub only — `AllParties` and `OtherParties` type skeletons with no protocol semantics.

**volar-dyn**: A dynamic (heap-allocated) re-implementation of `volar-spec` types, replacing `GenericArray<T, N>` with `Vec<T>` plus a runtime length witness. Manually written; intended as a reference.

**volar-spec-dyn**: A crate that can contain auto-generated dynamic types (produced by `volar-compiler`), gated behind the `generated` feature. Also contains manually written `core_types` as a reference.

**volar-ir**: Two intermediate representations.
- `volar_ir::ir`: Volar IR — an SSA block-based IR for VOLE computations. Statements include polynomial operations (`Poly`), rotations, merges, splatting, storage reads/writes, and transmutes. Types: `Bit`, `Vec`, `Tuple`, `Galois8AES`, `Galois64`, and continuation blocks.
- `volar_ir::boolar`: Boolar IR — a boolean circuit IR with `AND`, `OR`, `XOR`, `NOT` gates.
- Both IRs have an `is_movfuscated()` and `is_circuit()` predicate. "Movfuscation" in this context means reduction to a single block (the term refers to the mov-obfuscation technique of collapsing control flow).

**vaffle**: Empty `no_std` crate; placeholder for a planned WAFFLE-to-VAFFLE translation layer.

**volar-compiler**: A compiler that reads Rust source files from `volar-spec` using `syn`, builds an IR (`IrModule`), and can emit:
- Dynamic Rust (replacing typenum-length arrays with `Vec` + runtime witnesses) via `print_module_rust_dyn`
- TypeScript via `print_module_typescript`
- Type manifest files (`.volar.d`) for cross-crate compilation, stored with a `0xFF` prefix to prevent them being parsed as normal Rust.

The `volar-codegen` binary drives this pipeline. The manifest system captures struct/trait/impl signatures (with `todo!()` bodies) to allow type-checking dependents without re-parsing implementation details.

## What is not implemented

- Construction of commitments from witness/secret data
- Consistency checks (alternative/supplement to the Hypercube technique)
- IR proving (single round or multiple rounds)
- WAFFLE-to-VAFFLE translation
- Volar IR movfuscation pass
- Succinct proofs
- MPC protocol (only type stubs exist)
- Nested proving/verification

## Technical notes

- The VOLE relation is `u·Δ + v = q` over binary extension fields. Addition is XOR; multiplication is carry-less.
- The Hypercube technique is used to derive separate commitments from a single VOLE commitment.
- All `volar-spec` crates are `#![no_std]` and depend only on `cipher`, `rand`, `digest`, and `typenum`/`generic-array` from the RustCrypto ecosystem.
- The `multi_party` feature on `volar-spec` extends 2-party protocols to N-party; it is present but the N-party ABO protocol has a noted soundness caveat.
- Code is annotated with `@reliability` tags (`normal`, `experimental`, `hazmat`) and `@ai` tags indicating AI involvement in authoring.

## License

CC0-1.0 AND MIT AND Apache-2.0

*AI assisted*
