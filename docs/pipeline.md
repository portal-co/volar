# Volar Compilation Pipeline

> This document describes the full compilation pipeline, including recursive
> flows, weaving feedback, and IR interconnections. Read it before touching
> any lowering pass, codegen backend, weaver, or IR transform. The linear
> sketch in `overview.md` is a useful introduction, not an accurate map.

---

## Mental Model: A Graph of IRs, Not a Pipeline

The compilation "pipeline" is actually a **graph of IR representations** where
any IR can be an input to any transform that accepts it. There is no required
ordering beyond what data dependencies impose:

- A **high-level `IrModule`** (generic, type-level lengths) can be woven into
  a new `IrModule`, lowered to a dynamic `IrModule`, or printed directly.
- A **dynamic `IrModule`** (lengths as runtime `usize`) can be printed to Rust
  or TypeScript, lowered to `LirTarget`, or fed into further weaving.
- An **`IrCfgModule`** (CFG variant) can be lowered to `LirTarget` or printed.
- **VAFFLE / Volar IR / Boolar IR** are a family of low-level SSA IRs.
  Transforms between them are bidirectional in the sense that a LIR codegen
  step can produce VAFFLE or Volar IR values that then re-enter the
  movfuscation → weaving chain.
- **Movfuscated Volar IR** can be woven (ZK proof path) or booleanized into
  Boolar IR (garbling path), and from either of those the resulting IR can
  feed back into another weaving or transform step.

Complex multi-party or hybrid protocols can be expressed as a sequence of
in-memory IR transforms **without ever materializing an intermediate file or
invoking an external tool**. Every stage is a Rust function call that
produces a new IR value from existing IR values.

---

## IR Representations and Their Relationships

```
 ┌─────────────────────────────────────────────────────────────────────┐
 │                  High-Level IRs (volar-compiler)                    │
 │                                                                     │
 │   IrModule (generic)  ←──weave──→  IrModule (woven)                │
 │         │                               │                           │
 │     lower_module_dyn()            lower_module_dyn()               │
 │         │                               │                           │
 │   IrModule (dynamic)  ←──weave──→  IrModule (dynamic, woven)       │
 │         │                               │                           │
 │   ┌─────┴──────┐                ┌───────┴───────┐                  │
 │   │printer.rs  │                │IrCfgModule    │                  │
 │   │(Rust dyn)  │                │(CFG variant)  │                  │
 │   └─────┬──────┘                └───────┬───────┘                  │
 │         │                               │                           │
 │  printer_ts.rs ──→  TypeScript     lower_cfg_module()              │
 └─────────────────────────────────────────┼───────────────────────────┘
                                           │
 ┌─────────────────────────────────────────▼───────────────────────────┐
 │               Mid-Level IR (volar-lir / LirTarget)                  │
 │                                                                     │
 │   LirTarget (builder trait; in-memory or emitting)                 │
 │         │                         │                                 │
 │     CBackend                  VaffleTarget (→ VAFFLE)              │
 │     (→ C99)                       │                                 │
 │                                   │                                 │
 └───────────────────────────────────┼─────────────────────────────────┘
                                     │
 ┌───────────────────────────────────▼─────────────────────────────────┐
 │               Low-Level Circuit IRs (volar-ir)                      │
 │                                                                     │
 │   VAFFLE (volar-aware WAFFLE; optimizations, inlining)             │
 │       │                                                             │
 │       │  replace function calls with block jumps                   │
 │       ▼                                                             │
 │   Volar IR   (SSA; field-level; protocol-agnostic)                 │
 │       │  constant folding, SSA optimizations                       │
 │       │                                                             │
 │       │  replace conditional blocks with real-vs-fake flags        │
 │       ▼                                                             │
 │   Movfuscated Volar IR                                              │
 │       │  constant folding, SSA                  │                  │
 │       │                                         │                  │
 │       │  ZK proof weaving              booleanize                  │
 │       ▼                                         ▼                  │
 │   ZK Proof IR  ──→  (back to IrModule    Boolar IR  (boolean SSA) │
 │                       or IrCfgModule)        │  peephole opts      │
 │                                              │                     │
 │                                         garble weaving             │
 │                                              │                     │
 │                                              ▼                     │
 │                                         IrCfgModule (garbler +    │
 │                                          evaluator) ──→ LirTarget  │
 └─────────────────────────────────────────────────────────────────────┘
```

The dotted feedback arrows are real: Boolar IR and Volar IR outputs feed into
weaving passes that produce new high-level `IrModule`/`IrCfgModule` values,
which can then be lowered and printed without leaving the process.

---

## Stage Descriptions

### Parsing (`parser.rs`)

`parse_source(src: &str, stem: &str) -> IrModule`

Converts a `volar-spec` Rust file into a generic `IrModule`. All const-generic
parameters (N_LWE, BIG_N, …) are preserved as `IrGenericParam` entries; no
lengths are materialized yet.

Multiple source files are parsed separately and merged. The primitives
(`volar-primitives/src`) are parsed alongside the spec for the TypeScript
target so that field-element types get full class definitions.

### Weaving (`volar-weaver`)

Weaving passes are IR-to-IR transforms that generate new protocol code not
present in any source file. They are pure Rust functions: they accept one or
more IR values and return new IR values. They may be applied at any point in
the pipeline and may be composed:

| Weaver | Input | Output | Purpose |
|---|---|---|---|
| VOLE weaver | `BIrBlocks` (boolean circuit) | `IrModule` (prover + verifier fns) | Quicksilver-style VOLE ZK |
| Garbling weaver | `BIrBlocks` | `IrCfgModule` (garbler + evaluator) | Half-gate garbled circuits |
| FHE weaver | `IrModule` + scheme config | `IrCfgModule` | Homomorphic evaluation stubs |
| Network weaver | `IrModule` | `IrModule` | Transport/receive stubs from protocol sigs |

Because woven output is ordinary `IrModule` / `IrCfgModule`, it passes through
all subsequent transforms (lowering, further weaving, printing) without
modification to those transforms.

### Dynamic Lowering (`lowering_dyn.rs`)

`lower_module_dyn(module: &IrModule) -> IrModule`

Erases length-kind generic parameters by converting them into explicit runtime
`usize` parameters. **Call-site propagation is the lowering's responsibility.**
When `f<N>` calls `g<N>` and both are lowered, the call `g(args)` in `f`'s
body becomes `g(n, args)` — the lowering injects the forwarded length args.

This must happen in the lowering, not in individual backends, because:
- The Rust dyn target (`volar-spec-dyn`) needs explicit runtime params for
  `alloc`-friendly, monomorphization-free code.
- The TypeScript backend derives call-site args from the same lowered IR.
- The C backend via LIR codegen also derives them from the same IR.
- A printer-side workaround duplicated across backends would create drift
  between targets.

**Current limitation — call-site length param forwarding**: The lowering matches
callee length-param names against the caller's in-scope names. When the caller
names a length generic `BIG_N` but the callee names the same concept `N`, the
names differ and injection misses. The short-term workaround is explicit
turbofish in the spec (e.g., `poly_mul_neg::<BIG_N>(a, b)`), which the parser
captures as type args and the lowering uses to determine which in-scope variable
to forward.

**Future goal — weak type inference pass**: Implement a separate lowering sub-pass
that, for each `Call` site without explicit type args, infers which length
variables to forward by walking the argument types and matching array sizes
against the callee's parameter types. This would eliminate the need for turbofish
annotations in the spec for this purpose, and is the right long-term design.
Until then, callers that use different generic names than their callees must
annotate with turbofish.

### LIR Codegen (`volar-lir-codegen`) and Backends

`lower_module` / `lower_cfg_module` lowers a (possibly dynamic) `IrModule` or
`IrCfgModule` to `LirTarget`. `LirTarget` is a **builder trait**: callers issue
a sequence of method calls and the implementation emits whatever it targets —
C99 text, in-memory VAFFLE IR, machine code, etc.

Crucially, a `LirTarget` implementation can produce **another IR** (e.g.,
`VaffleTarget` produces VAFFLE) which then re-enters the low-level pipeline.
This is how LIR connects back to VAFFLE and Volar IR.

### Low-Level Circuit IRs (`volar-ir`)

**Volar IR** and **Boolar IR** are SSA block-based IRs for field-level and
boolean-level computations respectively. Transforms between them:

- `Volar IR → movfuscation → Movfuscated Volar IR` — collapses real/fake
  conditional branching into oblivious evaluation.
- `Movfuscated Volar IR → ZK weaving → IrModule/IrCfgModule` — produces
  a checkable proof of circuit evaluation (feeds back to high-level IR).
- `Movfuscated Volar IR → booleanize → Boolar IR` — converts field
  operations to boolean gates.
- `Boolar IR → garbling weaver → IrCfgModule` — generates garbler and
  evaluator (feeds back to high-level IR).

These feedback paths mean that a multi-layer protocol (e.g., ZK proof over a
garbled circuit) can be composed entirely in-memory:

```
parse_spec
  → garble_weave(circuit)      → IrCfgModule (garbler/evaluator)
  → lower_cfg_module           → LirTarget / VAFFLE
  → volar_ir_lower             → Movfuscated Volar IR
  → zk_weave                   → IrModule (prover/verifier for the garbling)
  → lower_module_dyn           → IrModule (dynamic)
  → printer.rs / printer_ts.rs → Rust / TypeScript
```

No intermediate files, no external tools.

---

## Key Invariants

1. **`IrModule` is the lingua franca for high-level IR.** Every stage that
   reads or writes program structure uses `IrModule` (or `IrCfgModule` for
   CFG). Never bypass this with raw strings or pre-rendered code fragments.

2. **Lowering is backend-neutral.** `lowering_dyn.rs` knows nothing about
   Rust or TypeScript syntax. Its output is consumed identically by all
   backends. Backend-specific quirks (bigint, class witnesses) live only in
   their printers.

3. **Call-site injection belongs in the lowering.** When a function gains
   runtime length params, every call site to that function is updated in the
   same lowering pass. Individual backends must not compensate for a missing
   lowering step.

4. **All IR representations are interconnected.** Any IR can be an input to
   any transform that accepts it. The pipeline graph has cycles (via weaving
   feedback) and any edge can be traversed in a pipeline run.

5. **Woven modules inherit all lowering and printing fixes for free.** Because
   weaving produces standard `IrModule` / `IrCfgModule` values, a fix to
   `lower_module_dyn`, `printer.rs`, or `printer_ts.rs` automatically applies
   to VOLE-woven, garble-woven, FHE-woven, and net-woven code.

6. **Complex pipelines need no external tools or files.** Any multi-stage
   protocol — parse, weave, booleanize, movfuscate, re-weave, lower, print —
   is a sequence of in-memory Rust function calls over IR values. Intermediate
   files are optional (for debugging or caching) but never required for
   correctness.

---

## Related Documents

| Topic | File |
|---|---|
| IR types and storage in depth | [`agent-context/ir-types-storage.md`](agent-context/ir-types-storage.md) |
| Weaving passes (garbling, VOLE) | [`agent-context/weaving.md`](agent-context/weaving.md) |
| TypeScript class witnesses | [`agent-context/ts-class-witnesses.md`](agent-context/ts-class-witnesses.md) |
| Low-level circuit IRs (Volar IR, Boolar IR) | [`ir-lowering.md`](ir-lowering.md) |
| LIR target trait and C backend | [`lir.md`](lir.md), [`lir-abi.md`](lir-abi.md) |
| Garbling pipeline | [`garbling-pipeline.md`](garbling-pipeline.md) |
| VOLE weaving | [`vole-weaving.md`](vole-weaving.md) |
| Architecture overview (sketch) | [`overview.md`](overview.md) |
