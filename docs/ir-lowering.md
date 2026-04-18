# IR Lowering Pipeline

This document describes the low-level circuit IR defined in `volar-ir`
(`crates/volar-ir`) and its place in the full compilation pipeline. The IR
is designed to be protocol-agnostic: the same `Volar IR` and `Boolar IR`
structures serve both the ZK proof path and the garbled circuit path, and
are intended to support additional protocols (other ZK schemes, MPC, etc.)
as they are added.

---

## Position in the Pipeline

```
High-level Rust (volar-spec)
        │
        │  volar-compiler
        ▼
Dynamic Rust / TypeScript (volar-spec-dyn / npm package)
        │
        │  WAFFLE (WebAssembly-like IR from the portal-pc ecosystem)
        ▼
    VAFFLE    ← volar-aware WAFFLE; optimizations, inlining
        │
        │  replace function calls with dynamic block jumps
        ▼
  Volar IR   ← defined in crates/volar-ir/src/ir.rs
        │  constant folding, SSA optimizations
        │
        │  replace conditional blocks with real-vs-fake flags
        ▼
Movfuscated Volar IR
        │  constant folding, SSA
        │
        ├──────────────────────────────┐
        │  convert to ZK proof scheme  │  booleanize
        ▼                              ▼
   ZK Proof                       Boolar IR   ← crates/volar-ir/src/boolar.rs
                                       │  peephole optimizations
                                       ▼
                                  Boolean circuit
```

The `volar-ir` crate defines the two lowest-level IRs: **Volar IR** (for
field-level computations — currently VOLE-based, but designed to be
protocol-agnostic) and **Boolar IR** (for boolean circuits, consumed by both
the garbling pipeline and the ZK proof path). The WAFFLE → VAFFLE → Volar IR
lowering is the responsibility of the `vaffle` crate and future work; `volar-ir`
focuses on the structures these lowerings produce.

---

## Volar IR (`src/ir.rs`)

Volar IR is a **Static Single Assignment (SSA)** block-based IR with explicit
control flow, typed variables, and a small set of domain-specific instructions.

### Top-Level Structures

```rust
pub struct IRBlocks(pub Vec<IRBlock>);
pub struct IRBlock {
    pub params: Vec<IRTypeId>,   // block parameters (phi nodes)
    pub stmts:  Vec<IRStmt>,     // instructions
    pub terminator: IRTerminator,
}
```

An `IRBlocks` is either:
- **Movfuscated** — exactly one block (the real/fake branching has been collapsed).
- **A circuit** — a single block whose terminator is `Jmp { func: Return, .. }`.

### Type System (`IRType`)

```rust
pub enum IRType {
    Primitive(PrimType),            // scalar or packed bitvector
    Vec(usize, IRTypeId),           // fixed-length vector of another type
    Tuple(Vec<IRTypeId>),           // product type
    Block { params: Vec<IRTypeId> }, // first-class block (closure)
}

pub enum PrimType {
    Bit,     // GF(2) — 1 wire
    _8,      // packed 8-bit bitvector — 8 wires
    _16,     // packed 16-bit bitvector — 16 wires
    _32,     // packed 32-bit bitvector — 32 wires
    _64,     // packed 64-bit bitvector — 64 wires
    _128,    // packed 128-bit bitvector — 128 wires (LIR: unimplemented)
    _256,    // packed 256-bit bitvector — 256 wires (LIR: unimplemented)
    AES8,    // GF(2⁸) with AES polynomial — 8 wires (FHE CFG: deferred)
    Galois64, // GF(2⁶⁴) — 64 wires (FHE CFG: deferred)
}
```

**Taxonomy for backends:**

| Variant | Kind | Wire count | FHE CFG weaving |
|---|---|---|---|
| `Primitive(Bit)` | 1-bit GF(2) | 1 | Full |
| `Vec(N, Bit)` | packed bitvector | N | Full (`[wire; N]`) |
| `Primitive(_8/_16/_32/_64)` | packed bitvector | 8/16/32/64 | Full (`[wire; W]`) |
| `Primitive(_128/_256)` | packed bitvector | 128/256 | LIR: `unimplemented!`; FHE: `[wire; W]` |
| `Primitive(AES8)` | GF(256) field element | 8 | Deferred |
| `Primitive(Galois64)` | GF(2^64) field element | 64 | Deferred |

`ir_type_bit_width(ty_id, types)` computes the total wire count for any supported type.

Types are interned via `IRTypeId` (a `u32` index into an `IRTypes` table).
Variables are similarly interned via `IRVarId`.

### Instructions (`IRStmt`)

| Variant | Description |
|---|---|
| `StorageRead { ty, addr }` | Read a typed value from a storage cell |
| `StorageWrite { src, ty, addr }` | Write a typed value to a storage cell |
| `Const(bytes, ty)` | Inject a compile-time constant |
| `Transmute { src, src_ty, dst_ty }` | Reinterpret bits between compatible types |
| `Poly { ty, coeffs, constant }` | Evaluate a multilinear polynomial over variables; output type is `ty` |
| `Rol { src, ty, n }` | Rotate-left by n bits |
| `Ror { src, ty, n }` | Rotate-right by n bits |
| `Merge { parts, ty }` | Concatenate multiple values into one typed value |
| `Splat { src, ty }` | Broadcast a scalar to a vector type |

`Poly` is the workhorse instruction — any VOLE-based gate compiles to a
polynomial evaluation over the wire variables. The `coeffs` map is a sparse
monomial encoding: `BTreeMap<Vec<Var>, u8>` maps each monomial (product of
variable IDs) to its GF(2⁸) coefficient.

**`Poly` output-type semantics (`ty` field):**

- **`ty = Bit`**: standard GF(2) gate — all coefficient variables are `Bit`-typed. This is the original and most common case.
- **`ty = T` (bitvector or field element)**: at most one `T`-typed variable per monomial; all other variables in that monomial are `Bit`-typed selectors. The polynomial result has type `T`.

Always supply `ty` explicitly when constructing `Poly` nodes; `ir_stmt_output_ty` now returns `Some(*ty)` for `Poly`.

### Terminators

```rust
pub enum IRTerminator {
    Jmp { func: IRBlockTargetId, args: Vec<IRVarId> },
    JumpCond {
        condition: IRVarId,
        true_block: IRBlockTargetId,  true_args:  Vec<IRVarId>,
        false_block: IRBlockTargetId, false_args: Vec<IRVarId>,
    },
}

pub enum IRBlockTargetId {
    Block(IRBlockId),   // jump to another block
    Return,             // return from the function
    Dyn(IRVarId),       // dynamic dispatch via a first-class Block value
}
```

`Dyn` is used after movfuscation where the "next block" is determined at runtime
by a flag variable that is either the real-path or fake-path label.

---

## Movfuscation

Movfuscation is the transformation that replaces explicit conditional jumps with
a uniform execution pattern driven by "real vs. fake" flags, making the control
flow opaque to an observer of the transcript.

Before movfuscation: `JumpCond` determines which branch is taken.
After movfuscation: both branches are unconditionally executed; the result is
selected by multiplying outputs by the corresponding flag. The result is a
single basic block (`IRBlocks::is_movfuscated() == true`).

This is a key step for VOLE-in-the-head proofs: the verifier never learns which
branch was "real" because all branches produce valid-looking transcripts.

---

## Boolar IR (`src/boolar.rs`)

Boolar IR is the **boolean circuit** IR used after booleanization. It is a
direct analogue of Volar IR at the bit level.

### Top-Level Structures

```rust
pub struct BIrBlocks(pub Vec<BIrBlock>);
pub struct BIrBlock {
    pub params: u32,           // number of block parameters (all bits)
    pub stmts:  Vec<BIrStmt>,
    pub terminator: BIrTerminator,
}
```

`BIrBlocks::is_movfuscated()` and `is_circuit()` mirror the Volar IR predicates.

### Instructions (`BIrStmt`)

```rust
pub enum BIrStmt {
    Zero,              // constant 0
    One,               // constant 1
    And(IRVarId, IRVarId),
    Or(IRVarId, IRVarId),
    Xor(IRVarId, IRVarId),
    Not(IRVarId),
}
```

This is an AND-XOR-NOT basis (complete for boolean computation). `Or` is
included for convenience and may be lowered to `And`+`Not` if needed by
downstream backends.

### Terminators

```rust
pub enum BIrTerminator {
    Jmp(BIrTarget),
    CondJmp { val: IRVarId, then_target: BIrTarget, else_target: BIrTarget },
}

pub struct BIrTarget {
    pub block: IRBlockTargetId,
    pub args: Vec<IRVarId>,
}
```

---

## Status and Next Steps

The `volar-ir` crate defines the *data structures* for the IR but does not yet
include:

- A lowering pass from VAFFLE to Volar IR.
- An SSA construction / normalization pass.
- The movfuscation transformation itself.
- A booleanization pass (Volar IR → Boolar IR).
- A ZK proof generator (Movfuscated Volar IR → proof transcript).
- Peephole optimizations for Boolar IR.
- Protocol-specific backends beyond ZK (e.g. MPC, threshold protocols).

These are planned in the roadmap; the `vaffle` crate stub will handle the
WAFFLE → VAFFLE → Volar IR path. The IR node set (`IRStmt`, `Poly`, etc.) is
currently shaped around VOLE semantics; generalizing it to support other
protocol backends cleanly is a known future design task.
