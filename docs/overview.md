# Volar Architecture Overview

## Project Goal

Volar implements **VOLE-in-the-head** (VOLEitH) — a zero-knowledge proof
technique based on Vector Oblivious Linear Evaluation over GF(2). The system
allows a prover to convince a verifier that they know a secret satisfying some
predicate, without revealing the secret, using cheap VOLE correlations as the
cryptographic backbone.

The project also supports **multi-party computation (MPC)** and
**garbled circuits** through the same VOLE-based primitives.

---

## Workspace Layout

```
volar/
├── crates/
│   ├── spec/
│   │   ├── volar-primitives/   Field element types and arithmetic
│   │   ├── volar-common/       Hash commitments, length-doubling utilities
│   │   ├── volar-spec/         Cryptographic protocol specifications (ZK, MPC, garbled)
│   │   ├── volar-spec-dyn/     Auto-generated dynamic version of volar-spec
│   │   └── volar-dyn/          Runtime dynamic bindings (generated from volar-spec)
│   ├── ir/
│   │   ├── volar-ir/           Low-level circuit IR (Volar IR, Boolar IR)
│   │   └── vaffle/             WAFFLE-to-VAFFLE intermediate layer (stub)
│   └── compiler/
│       └── volar-compiler/     Compiler: parser → IR → lowering → printers
├── docs/                       This documentation
└── packages/                   npm packages (volar-runtime TypeScript)
```

---

## Crate Dependency Graph

```
volar-primitives     (field elements: Bit, Galois, BitsInBytes, …)
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

VOLE satisfies the relation:

```
u · Δ + v = q
```

where Δ is the verifier's global secret, and (u, v) / q are the prover's and
verifier's shares respectively. Volar encodes all protocol operations as
algebraic manipulations over VOLE correlations.

The `Vope<N, T, K>` type in `volar-spec` represents a polynomial of degree K
in Δ whose coefficients are vectors of length N with element type T. This
generalizes scalar VOLE to polynomial VOLE.

See [spec.md](spec.md) for the full specification-layer documentation.
