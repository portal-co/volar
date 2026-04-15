# Volar Architecture Overview

## Project Goal

Volar's goal is to increase adoption of **program-related cryptography** —
zero-knowledge proofs, garbled circuits, multi-party computation, and the
field-arithmetic primitives that underpin them — by implementing these
techniques in auditable Rust, compiling them to other targets (TypeScript, C),
and developing and refining them publicly with a rigorous reliability system.

The current implementation focus is **VOLE-based zero-knowledge proofs**
(Quicksilver-style VOLEitH) and **garbled circuits** (half-gate scheme).
These were chosen because they share a clean common substrate — VOLE
correlations, boolean circuits, binary extension-field arithmetic — that the
compiler and IR were designed around. Other ZK schemes, MPC protocols, and
related constructions will follow as the infrastructure matures.

---

## Workspace Layout

```
volar/
├── crates/
│   ├── spec/
│   │   ├── volar-primitives/   Field element types (GF(2), GF(2^8/64/128/256)) and arithmetic
│   │   ├── volar-common/       Hash commitments, length-doubling utilities
│   │   ├── volar-spec/         Cryptographic protocol specifications (ZK, MPC, garbled)
│   │   ├── volar-spec-dyn/     Auto-generated dynamic version of volar-spec
│   │   └── volar-dyn/          Runtime dynamic bindings (generated from volar-spec)
│   ├── ir/
│   │   ├── volar-ir/           Low-level circuit IR (Volar IR, Boolar IR)
│   │   ├── volar-lir/          LirTarget trait + LirType (no_std)
│   │   └── vaffle/             WAFFLE-to-VAFFLE intermediate layer (stub)
│   └── compiler/
│       ├── volar-compiler/     Compiler: parser → IR → lowering → printers
│       ├── volar-weaver/       BIrBlocks → IrModule (garbler/evaluator + VOLE prover/verifier)
│       ├── volar-lir-codegen/  IrModule → LirTarget lowering (mono, structs, loops)
│       └── volar-c-backend/    LirTarget → C99 source (for testing)
├── docs/                       This documentation
└── packages/                   npm packages (volar-runtime TypeScript)
```

---

## Crate Dependency Graph

```
volar-primitives     (field elements: Bit, Galois, Galois64, Galois128, Galois256, …)
      │
volar-common         (hash_commitment, length_doubling::LengthDoubler)
      │
volar-spec           (specifications: ZK/VOLE, MPC, garbled circuits)
      │
volar-spec-dyn       (auto-generated dynamic runtime, compiled by volar-compiler)
      │
volar-dyn            (additional dynamic wrappers)

volar-compiler       (stand-alone: parser → IR → printer; reads volar-spec source)
volar-ir             (low-level IR: Volar IR, Boolar IR; used downstream by prover)
vaffle               (WAFFLE integration stub)
```

---

## Compilation Pipeline

```
volar-spec Rust source (total subset of Rust)
        │
        │  volar-compiler / parser.rs
        ▼
    IrModule   (generic, type-level lengths encoded in generics)
        │
        │  volar-compiler / lowering_dyn.rs
        ▼
    IrModule   (dynamic: lengths → runtime usize witnesses)
        │
        ├──────────────────────────┐
        │  printer.rs              │  printer_ts.rs
        ▼                          ▼
  Rust source               TypeScript source
  (volar-spec-dyn)           (packages/volar-runtime)
```

The compiler also reads `.volar.d` manifests — signature-only descriptions of
compiled crates (see [compiler.md](compiler.md)) — so that code referencing
`volar-primitives` types can be lowered without access to their source.

---

## Low-Level Circuit Pipeline (volar-ir)

```
WAFFLE IR
    │  (lowering to VOLE-compatible ops)
    ▼
VAFFLE   (optimizations, inlining)
    │  (replacing function calls with dynamic block jumps)
    ▼
Volar IR   (constant folding, SSA opts)
    │  (replacing blocks with real-vs-fake flags)
    ▼
Movfuscated Volar IR   (constant folding, SSA)
    │                           │
    │  (ZK proof conversion)    │  (booleanize)
    ▼                           ▼
ZK Proof              Boolar IR   (peephole)
```

See [ir-lowering.md](ir-lowering.md) for details.

---

## Key Cryptographic Relationships

### VOLE (current primary substrate)

VOLE satisfies the relation:

```
u · Δ + v = q
```

where Δ is the verifier's global secret, and (u, v) / q are the prover's and
verifier's shares respectively. The current ZK proof construction encodes all
proof operations as algebraic manipulations over VOLE correlations.

The `Vope<N, T, K>` type in `volar-spec` represents a polynomial of degree K
in Δ whose coefficients are vectors of length N with element type T. This
generalizes scalar VOLE to polynomial VOLE, enabling Quicksilver-style AND
gate checks.

### Garbled circuits (current secondary construction)

The garbled circuit layer uses the same VOLE wire-label types (`Garble<N>`,
`Eval<N>`, `GarbleTable<N>`) and shares the boolean circuit IR (`BIrBlocks`)
with the ZK proof path. XOR gates are free; AND gates require one pre-computed
`GarbleTable<N>`.

See [spec.md](spec.md) for the full specification-layer documentation.
