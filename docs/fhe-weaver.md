# FHE Weaver — Generic FHE Scheme Abstraction

> **Referenced by**: `volar-weaver/src/fhe.rs`, `volar-spec/src/tfhe.rs`
>
> **Status**: @reliability: experimental

---

## Overview

The FHE weaver provides a single entry point — `weave_fhe` — that lowers a
boolean circuit (`IRBlocks`) into an `IrModule` (or `IrCfgModule`) implementing
evaluation under a Fully Homomorphic Encryption (or similarly structured)
scheme.

Scheme-specific gate emission is handled by the `FheScheme` trait, which every
scheme implements.  This decouples the shared weaving infrastructure (movfuscation,
storage lowering, provenance, CFG traversal) from scheme-specific details (wire
types, bootstrapping keys, bootstrap costs).

Currently implemented schemes:

| Scheme | Kind | Crate | Path |
|--------|------|-------|------|
| **GRAFHEN** | Garbled-circuit hybrid | `volar-weaver` | `crates/compiler/volar-weaver/src/grafhen.rs` |
| **TFHE** | Torus FHE gate-bootstrapping | `volar-spec` | `crates/spec/volar-spec/src/tfhe.rs` |

---

## The `FheScheme` Trait

```rust
pub trait FheScheme {
    // ── Control flow ──────────────────────────────────────────────────────────

    /// Whether this scheme supports CFG-based weaving (`IRBlocks` directly).
    /// `false` (default): lower to BIrBlocks first, then call binary gate methods.
    fn cfg_capable(&self) -> bool { false }

    // ── Signature / type ──────────────────────────────────────────────────────

    /// The Rust type of an owned gate-output wire.
    fn wire_type(&self) -> IrType;

    /// The Rust type of a circuit input parameter (default: `wire_type`).
    fn input_type(&self) -> IrType { self.wire_type() }

    /// Additional function parameters prepended before the circuit inputs.
    fn extra_params(&self) -> Vec<IrParam>;

    /// Generic type parameters on the emitted function.
    fn generics(&self) -> Vec<IrGenericParam> { vec![] }

    /// Suffix appended to the circuit name → `{name}_{suffix}`. Default: `"fhe"`.
    fn fn_name_suffix(&self) -> &str { "fhe" }

    // ── Binary gate methods ───────────────────────────────────────────────────

    fn emit_zero<Q: Clone + Default>(&self) -> IrExpr<Q>;
    fn emit_one<Q: Clone + Default>(&self) -> IrExpr<Q>;
    fn emit_xor<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q>;
    fn emit_not<Q: Clone + Default>(&self, a: IrExpr<Q>) -> IrExpr<Q>;
    /// `gate_idx` is the 0-based count of AND gates emitted so far.
    fn emit_and<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>, gate_idx: usize) -> IrExpr<Q>;

    // ── External primitives (default: panic) ──────────────────────────────────

    fn emit_oracle_call<Q: Clone + Default>(&self, name: &str, args: Vec<IrExpr<Q>>, bits: usize) -> IrExpr<Q>;
    fn emit_oracle_bit<Q: Clone + Default>(&self, call_var: &str, bit: usize) -> IrExpr<Q>;
    fn emit_action_call<Q: Clone + Default>(&self, name: &str, guard: IrExpr<Q>, args: Vec<IrExpr<Q>>, fallbacks: Vec<IrExpr<Q>>, bits: usize) -> IrExpr<Q>;
    fn emit_action_bit<Q: Clone + Default>(&self, call_var: &str, bit: usize) -> IrExpr<Q>;
    fn emit_rng<Q: Clone + Default>(&self, rng_name: &str) -> IrExpr<Q>;

    // ── CFG path (only when cfg_capable = true) ───────────────────────────────

    fn emit_ir_stmt<Q: Clone + Default>(&self, stmt: &IRStmt, ...) -> Option<IrExpr<Q>>;
}
```

---

## Two Weaving Paths

### Flat path (`cfg_capable = false`, default)

```
IRBlocks
  ↓  lower_ir_to_boolar
BIrBlocks
  ↓  movfuscate_biir
BIrBlocks (single-block DAG)
  ↓  expand_ors (De Morgan)
BIrBlocks (XOR / AND / NOT only)
  ↓  iterate gates → emit_zero/one/xor/not/and
IrModule (flat sequential body)
```

Use `weave_fhe` with a non-`cfg_capable` scheme; returns `FheOutput::Flat`.

Use `weave_fhe_flat_bir` directly when you already hold `BIrBlocks` and want
provenance threading:

```rust
let output: IrModule<H::Output> = weave_fhe_flat_bir(
    &bir_circuit,
    &scheme,
    "my_fn",
    linkage,
    &my_handler,
    storage,
);
```

### CFG path (`cfg_capable = true`)

```
IRBlocks
  ↓  iterate block-by-block, calling emit_ir_stmt per stmt
IrCfgModule (structured CFG with IrCfgBlock / IrCfgJump)
```

Returns `FheOutput::Cfg`.  Schemes that return `None` from `emit_ir_stmt`
panic the weaver — CFG-capable schemes must handle every `IRStmt` variant
that might appear in their inputs.

---

## Entry Point

```rust
pub fn weave_fhe<S: FheScheme>(
    blocks: &IRBlocks,
    types: &IRTypes,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
    storage: Option<&FheStorageConfig>,
) -> FheOutput;

pub enum FheOutput {
    Flat(IrModule),
    Cfg(IrCfgModule),
}
```

---

## Oblivious Storage (`FheStorageConfig`)

When `storage` is `Some`, `StorageRead` and `StorageWrite` statements in the
flat path are lowered to **oblivious MUX-tree** reads and writes using the
scheme's binary gate primitives.

```rust
pub type FheStorageSizes = BTreeMap<(u32, usize), usize>;
// Key: (StorageId.0, bit_width_per_cell)
// Value: cell count

pub struct FheStorageConfig {
    pub sizes: FheStorageSizes,
}
```

Each storage space `(sid, bw)` with `count` cells becomes a **`&mut [WireType; count]`**
parameter on the emitted function.  The weaver tracks the current wire name for
each cell and updates it on writes.

**Read** (MUX tree): select the correct cell using the address bits as a binary
selector, emitting `count − 1` AND gates per bit layer.

**Write** (demux+MUX): for each cell `i`, emit `(addr == i) ? new_value : old_value`
using AND / XOR gates.

**AND gate cost**: `O(count × addr_bits)` per access.

For zero-AND-gate storage, use the VOLE Commitment mode instead (see
`docs/memory-checking.md` and `docs/vole-weaving.md`).

---

## GRAFHEN Scheme

GRAFHEN is a garbled-circuit hybrid where gate outputs are encrypted word-level
values rather than individual bits.  See `docs/grafhen.md` for the full protocol.

The `GrafhenScheme` struct implements `FheScheme` with:
- `wire_type()` → `GrafhenWord<WBOUND>`
- `input_type()` → `&GrafhenWord<WBOUND>` (pass by reference)
- `extra_params()` → `bk: &GrafhenBootstrappingKey<…>` (bootstrapping key)
- `fn_name_suffix()` → `"grafhen"`
- `emit_and` → emits a bootstrapping call via `grafhen_and(bk, a, b)`

GRAFHEN is not a standard FHE scheme; it uses garbled circuits internally and
has no IND-CPA security claim.  See `docs/insecure.md` for the security caveats.

---

## TFHE Scheme

> **File**: `crates/spec/volar-spec/src/tfhe.rs`
> **Status**: @reliability: experimental, @experimental-status: unreviewed

TFHE (Chillotti et al., *J. Cryptology* 2020; Micciancio & Polyakov, ePrint
2020/086) implements boolean gate evaluation via GINX blind-rotation bootstrapping
over the LWE and RLWE problem.

### Parameters

```rust
// LWE ciphertext dimension (secret key length)
const N_LWE: usize = 630;    // typical 128-bit security value

// RLWE ring dimension (X^BIG_N + 1)
const BIG_N: usize = 1024;   // must be a power of two

// Bootstrapping gadget decomposition levels
const BS_ELL: usize = 2;

// Key-switching gadget decomposition levels
const KS_ELL: usize = 5;
```

### Gate costs

| Gate | Cost |
|------|------|
| XOR | Free (LWE addition) |
| NOT | Free (LWE negation + constant shift) |
| AND | 1 gate bootstrapping (blind rotation + RLWE × LWE + key-switch) |

### Key types

```rust
pub struct LweCiphertext<const N_LWE: usize> { pub a: [u32; N_LWE], pub b: u32 }
pub struct RlweCiphertext<const BIG_N: usize> { pub a: [u32; BIG_N], pub b: [u32; BIG_N] }
pub struct RgswCiphertext<const BIG_N: usize, const BS_ELL: usize> { /* gadget decomposed */ }
pub struct BootstrappingKey<const N_LWE: usize, const BIG_N: usize, const BS_ELL: usize> {
    pub bsk: [RgswCiphertext<BIG_N, BS_ELL>; N_LWE],
}
pub struct KeySwitchingKey<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize> {
    pub ksk: [[LweCiphertext<N_LWE>; KS_ELL]; BIG_N],
}
```

### Gate bootstrapping protocol

1. **Homomorphic AND** of LWE ciphertexts `ct_a`, `ct_b`:
   - Add them: `ct_sum = ct_a + ct_b + constant_shift`
   - **Blind rotation**: multiply the test polynomial by `X^{-ct_sum.b}` and
     each `X^{-ct_sum.a[i]}` using RGSW multiplication against the BSK.
   - **Extract**: convert the RLWE result back to an LWE ciphertext.
   - **Key-switch**: translate from RLWE-dimension key to LWE-dimension key via
     KSK.

All arithmetic is over `u32` modular (torus ℝ/ℤ represented as `mod 2^32`).
Polynomial multiplication is schoolbook O(N²) negacyclic convolution.

### Torus encoding

```
bit 0 → 0
bit 1 → Q4 = 2^30   (quarter turn)
```

An LWE ciphertext `(a, b)` encodes bit `x` as:
`b = ⟨a, s⟩ + e + x·Q4` where `e` is small noise and `s` is the secret key.

### Security

IND-CPA under the LWE/RLWE hardness assumption. Reference parameters
`(N_LWE=630, BIG_N=1024, BS_ELL=2, KS_ELL=5)` are suggested for 128-bit
security but have **not been validated** by this codebase. Do not use in
production without independent expert review.

---

## Printing CFG modules

```rust
/// Emit a self-contained Rust source for an IrCfgModule produced by the CFG path.
pub fn print_fhe_cfg_module(module: &IrCfgModule, self_contained: bool) -> String;
```

`self_contained = true` adds the standard `#![allow(...)]`, `extern crate alloc`,
and scheme imports.  `self_contained = false` emits only the function bodies.

---

## See Also

- [GRAFHEN scheme](grafhen.md) — word-level garbled circuit hybrid
- [VOLE storage modes](vole-weaving.md) — zero-AND-gate memory checking
- [Memory checking protocol](memory-checking.md) — multiset hash specification
- [Reliability](reliability.md) — experimental status of TFHE and GRAFHEN
