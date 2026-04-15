# volar

A Rust library and compiler toolchain for **program-related cryptography** — zero-knowledge proofs, garbled circuits, multi-party computation, and the field-arithmetic primitives that underpin them. The project is early-stage and actively developed in the open.

## What it is

**Program-related cryptography** is the family of techniques that let programs prove things about their own execution without revealing private inputs: zero-knowledge proofs, garbled circuits, multi-party computation, oblivious RAM, witness encryption, and similar constructions. These techniques exist in the research literature, but adoption is low because production-quality implementations are rare, trust is hard to establish, and cross-language deployment is painful.

Volar's goals are:

1. **Implement** program-related cryptography in auditable, `no_std` Rust, starting with the constructions the team understands well and expanding outward.
2. **Compile** those implementations to other targets (dynamic Rust, TypeScript, C) so the same cryptographic kernel can run in browsers, servers, and embedded systems without re-implementation drift.
3. **Develop and refine publicly** — new constructions enter at the Experimental reliability tier, earn review, and graduate to Normal or Hazmat. Broken constructions are preserved as Insecure with analysis explaining why.

The current implementation focus is **VOLE-based zero-knowledge proofs** (specifically the Quicksilver-style VOLEitH construction) and **garbled circuits**, because these share a clean common substrate (VOLE correlations, boolean circuits, field arithmetic) that the compiler and IR were designed around. Other ZK schemes, MPC protocols, and related constructions will follow as the infrastructure matures.

This repo is an attempt to build that foundation: a usable cryptographic library in Rust, with a compiler that can transpile the core spec to other targets (dynamic Rust and TypeScript).

## Repository structure

```
crates/
  spec/
    volar-primitives/   Field element types: Bit, Galois (GF(2^8)), Galois64, Galois128, Galois256, etc.
    volar-common/       Shared primitives: hash commitments, length-doubling PRG
    volar-spec/         Core protocol specifications (VOLE ZK, garbled circuits, MPC types)
    volar-dyn/          Dynamic (heap-allocated, runtime-sized) mirror of volar-spec
    volar-spec-dyn/     Auto-generated dynamic types from volar-compiler; also a manual reference
  ir/
    volar-ir/           Two IRs: Volar IR (SSA, field-aware) and Boolar IR (AND/XOR/NOT boolean circuits)
    vaffle/             Stub crate; placeholder for WAFFLE→VAFFLE translation (not implemented)
  compiler/
    volar-compiler/     Parses volar-spec Rust source via syn, emits dynamic Rust or TypeScript
    volar-weaver/       BIrBlocks → garbled-circuit and VOLE ZK proof code generation
```

## What is implemented

**volar-primitives**: Field element types with arithmetic. GF(2^8) (`Galois`) uses the AES reduction polynomial `x^8 + x^4 + x^3 + x + 1`. GF(2^64) (`Galois64`), GF(2^128) (`Galois128`, GCM polynomial), and GF(2^256) (`Galois256`) are also provided. All Galois types support field inversion via Itoh–Tsujii addition-chain exponentiation. `BitsInBytes` and `BitsInBytes64` are packed bit representations for SIMD-style operations. A `Tropical` semiring is also present. The field operations (`gf_mul_*`, `gf_invert_*`) are written in the total-Rust subset and are compilable by `volar-compiler` for TypeScript transpilation.

**volar-common**: `hash_commitment` (commit-open via `H(message || nonce)`). `length_doubling` (a digest-based PRG that doubles its output at each step, used as the PRF backbone for ABO generation).

**volar-spec**: The main cryptographic kernel, implementing two constructions today.

*VOLE-based zero-knowledge proofs (Quicksilver-style):*
- `Delta<N, T>` and `Q<N, T>`: verifier-side VOLE types. `Delta` is the verifier's global secret; `Q` is the verifier's share.
- `Vope<N, T, K>`: a degree-K polynomial in Δ over a vector of length N. This is the prover's side of the commitment.
- `BitVole<N, T>`: subfield VOLE over GF(2), where `u` is a bit vector and `v` is an extension-field vector.
- `field_rotate`: bit-level rotation and extraction operations on `BitsInBytes`/`BitsInBytes64` arrays, used for the Galois extension lifting technique (mapping bit-commitments into GF(2^k) for AES-like operations).
- `byte_gen` (ABO protocol): prover and verifier sides of an "almost bit OT" generation protocol. Both 2-party and N-party (`multi_party` feature) variants are implemented, though the N-party protocol has a noted soundness caveat.
- `prove`: Quicksilver-style VOLE AND gate primitives (`vole_and_prover_step`, `vole_and_verifier_check`). Written in the total-Rust subset, parseable by the compiler. The weaver generates prover and verifier circuit functions from these primitives.

*Garbled circuits:*
- `garble`: a half-gate garbled circuit scheme over VOLE types (Zahur-Rosulek-Evans 2015). The weaver generates evaluator, garbler, GarbledCircuit, and EvalSetup code from boolean circuits.

*MPC:*
- `mpc`: stub only — `AllParties` and `OtherParties` type skeletons with no protocol semantics.

**volar-dyn**: A dynamic (heap-allocated) re-implementation of `volar-spec` types, replacing `GenericArray<T, N>` with `Vec<T>` plus a runtime length witness. Manually written; intended as a reference.

**volar-spec-dyn**: A crate that can contain auto-generated dynamic types (produced by `volar-compiler`), gated behind the `generated` feature. Also contains manually written `core_types` as a reference.

**volar-ir**: Two intermediate representations.
- `volar_ir::ir`: Volar IR — an SSA block-based IR for field-level computations. Statements include polynomial operations (`Poly`), rotations, merges, splatting, storage reads/writes, and transmutes.
- `volar_ir::boolar`: Boolar IR — a boolean circuit IR with `AND`, `OR`, `XOR`, `NOT` gates.
- Both IRs have an `is_movfuscated()` and `is_circuit()` predicate.

**vaffle**: Empty `no_std` crate; placeholder for a planned WAFFLE-to-VAFFLE translation layer.

**volar-compiler**: A compiler that reads Rust source files from `volar-spec` using `syn`, builds an IR (`IrModule`), and can emit:
- Dynamic Rust (replacing typenum-length arrays with `Vec` + runtime witnesses) via `print_module_rust_dyn`
- TypeScript via `print_module_typescript`
- Type manifest files (`.volar.d`) for cross-crate compilation, stored with a `0xFF` prefix to prevent them being parsed as normal Rust.

The `volar-codegen` binary drives this pipeline. The manifest system captures struct/trait/impl signatures (with `todo!()` bodies) to allow type-checking dependents without re-parsing implementation details.

## What is not implemented

- Construction of VOLE commitments from witness/secret data
- Consistency checks (alternative/supplement to the Hypercube technique)
- Multi-round interactive proofs
- WAFFLE-to-VAFFLE translation
- Volar IR movfuscation pass
- Succinct proofs
- MPC protocol (only type stubs exist)
- Nested proving/verification
- VOLE setup (OT-based correlation generation)
- Batched Quicksilver verification (random-challenge version)
- Other ZK proof systems (STARKs, SNARKs, sigma protocols)
- Oblivious RAM
- Witness encryption (a broken attempt is preserved in `xsat.rs.insecure`)

## Technical notes

- The VOLE relation is `u·Δ + v = q` over binary extension fields. Addition is XOR; multiplication is carry-less.
- The Hypercube technique is used to derive separate VOLE commitments from a single ABO generation.
- All `volar-spec` crates are `#![no_std]` and depend only on `cipher`, `rand`, `digest`, and `typenum`/`generic-array` from the RustCrypto ecosystem.
- The `multi_party` feature on `volar-spec` extends 2-party protocols to N-party; the N-party ABO protocol has a noted soundness caveat.
- Code is annotated with `@reliability` tags (`normal`, `experimental`, `hazmat`) and `@ai` tags indicating AI involvement in authoring.

## License

CC0-1.0 AND MIT AND Apache-2.0

*AI assisted*
