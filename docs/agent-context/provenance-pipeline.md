# Provenance Pipeline

> Load when adding provenance to a pass, writing a `ProvenanceHandler` /
> `DualProvenanceHandler` impl, adding a new IR container, or wondering why
> there's no `Default` bound or `synthetic()` escape hatch.

## Rationale

**Debuggability.** Every statement in compiled output — garbled-circuit
gates, VOLE wire allocations, generated Rust/TS functions, LIR instructions —
can be traced back to the source WASM instruction or spec expression that
caused it. Unexpected gate counts, miscompiles, and "why does this function
exist" questions become attributable to a specific input statement instead of
"somewhere in the pipeline".

**Integrability with source tracking.** Dependents that use Volar as a
library can attach their own source-location type as `P` (e.g. a `Span` from
their own parser, or a WASM byte offset) and have it flow, untouched, all the
way through to the `IrModule`/`IrCfgModule` they receive. That enables IDE
overlays, error messages with source locations, and per-source-statement
proof-size or gate-count annotations — all without Volar knowing anything
about the dependent's source format.

## Methodology

### `P` is attached at every IR level

| Level | Container | Per-statement provenance field |
|---|---|---|
| VAFFLE | `Module<P>` / `FuncBody<P>` / `Block<P>` (`vaffle`) | `Block::stmt_provs: Vec<P>` |
| Volar field-level IR | `IRBlocks<P>` / `IRBlock<P>` (`volar-ir`) | `IRBlock::stmt_provs: Vec<P>` |
| Boolar (boolean circuit) IR | `BIrBlocks<P>` / `BIrBlock<P>` (`volar-ir`) | `BIrBlock::stmt_provs: Vec<P>` |
| Compiler IR (flat) | `IrModule<IrFunction<P>, P>` (`volar-compiler`) | `IrBlock::stmt_provs: Vec<P>` |
| Compiler IR (CFG) | `IrCfgModule<P> = IrModule<IrAnyFunction<P>, P>` (`volar-compiler`) | `IrCfgBlock::stmt_provs: Vec<P>` |

`stmt_provs` is always parallel to `stmts`: `stmt_provs[i]` is the provenance
of `stmts[i]`. Every one of these containers has `P: Clone = ()`, so existing
call sites that don't care about provenance compile unchanged with `P = ()`.

### Design invariant: provenance is never invented

There is **no `Default` bound on `P`** anywhere in this pipeline, and **no
`synthetic()` method** on any handler trait. The type system enforces that
every output statement's provenance is either:

- propagated unchanged from an input statement (`P` flows straight through), or
- derived by cloning **some specific source statement's** provenance through a
  [`ProvenanceHandler::map`](#provenancehandlerp).

When a pass expands one source statement into N derived statements (e.g.
`movfuscate` decomposing a `Poly` into several boolean gates, or
`weave_fhe_flat_bir` emitting several wires for one circuit statement), **all
N derived statements clone the source statement's provenance**. When a pass
emits genuinely infrastructural statements with no single corresponding source
statement (storage init, CMUX overhead, control-flow gates for a whole CFG
block), it derives a `ctrl_prov` / fallback provenance from **the first
available source statement in the relevant scope** — see
[`weave_fhe_flat_bir`'s `ctrl_prov`](../../crates/compiler/volar-weaver/src/fhe.rs)
and [`weave_fhe_cfg_with_handler`'s `block_ctrl_provs`/`fallback`](../../crates/compiler/volar-weaver/src/fhe.rs)
for the canonical examples.

If a circuit has *no* statements at all, there is nothing to derive
infrastructure provenance from — these functions `panic!` rather than
inventing a value. This is a deliberate "fail loudly on a degenerate input"
choice, not an oversight.

### `ProvenanceHandler<P>`

Defined in `volar-provenance` (`#![no_std]`, zero dependencies):

```rust
pub trait ProvenanceHandler<P: Clone> {
    type Output: Clone;
    fn map(&self, prov: &P) -> Self::Output;
    fn gate_degree(&self, _prov: &P) -> usize { 1 }  // QuickSilver AND-gate degree hint
}
```

Built-in implementations:

| Handler | `Output` | Behaviour |
|---|---|---|
| `NoProvenance` | `()` | Discards all annotations — used by every pre-existing `P = ()` wrapper |
| `KeepProvenance` | `P` | Clones input provenance unchanged (identity) |
| `MapProvenance(f)` | `Q` | Applies a user closure `Fn(&P) -> Q` |

### `DualProvenanceHandler<P1, P2>`

For passes that combine two independently-provenanced inputs — e.g.
`substitute_vaffle_with_handler`, which inlines a replacement `Module<Q>`
(oracle/action body) into a host `Module<P>`:

```rust
pub trait DualProvenanceHandler<P1: Clone, P2: Clone> {
    type Output: Clone;
    fn map_left(&self, p1: &P1) -> Self::Output;   // host-only statement
    fn map_right(&self, p2: &P2) -> Self::Output;  // replacement-only statement
    fn merge(&self, p1: &P1, p2: &P2) -> Self::Output; // jointly-attributed statement
}
```

Built-ins: `KeepLeft` (`Output = P1`, `map_right`/`merge` panic — only valid
when every statement has host provenance), `KeepRight` (`Output = P2`,
mirror), `MergePair(fl, fr, fm)` (three closures, one per case).

### `map_prov` vs `map_prov_with_handler`

Two distinct mechanisms exist and are easy to confuse:

- **`MapProv<P, Q>`** (`volar-compiler::ir`) — a structural trait implemented
  for every compiler-IR node (`IrExpr<P>`, `IrStmt<P>`, `IrBlock<P>`,
  `IrCfgBlock<P>`, `IrModule<F, P>`, …). `map_prov(self, f: &impl Fn(P) -> Q)`
  recursively rewrites a value's `P` to `Q` using a plain closure. Only
  `IrBlock`/`IrCfgBlock`'s `stmt_provs` actually invoke `f` — leaf expressions
  like `Var`/`Lit` never carry provenance and pass through untouched.
- **`map_prov_with_handler<H: ProvenanceHandler<P>>`** — inherent methods on
  `IRBlocks<P>`/`IRBlock<P>` (and the VAFFLE equivalents) that apply a
  *handler* (not a bare closure) to every `stmt_prov`. This is the
  weaving-boundary entry point: e.g.
  `blocks.clone().map_prov_with_handler(&NoProvenance)` to erase to `()`
  before calling a `P = ()`-only helper, then converting the result back with
  `MapProv::map_prov` plus per-block derived provenance.

`weave_fhe_cfg_with_handler` uses exactly this combination: it can't thread
`H` through the 450-line `weave_fhe_cfg`, so it erases input provenance with
`map_prov_with_handler(&NoProvenance)`, runs the existing `P = ()` weaver, then
uses `MapProv::map_prov` to rewrite each output block's `stmt_provs` to that
block's derived `H::Output` provenance.

### LIR: `LirTarget<Prov>` + `set_prov`

`volar-lir`'s `LirTarget` trait takes `Prov: Clone = ()` as a **trait-level**
generic parameter (not per-method):

```rust
pub trait LirTarget<Prov: Clone = ()> {
    type Value: Clone + Eq + core::fmt::Debug;
    type Block: Clone + Eq + core::fmt::Debug;

    /// Instructions emitted after this call (until the next `set_prov`)
    /// are tagged with `prov`. No-op default for backends that don't track it.
    fn set_prov(&mut self, _prov: Prov) {}

    fn iconst(&mut self, ty: LirType, val: i64) -> Self::Value;
    // ... all other instruction-creating methods, unchanged signatures ...
}
```

Rather than threading a `prov` argument through every one of the ~30
instruction-creating methods, callers call `set_prov(p)` once before emitting
the instructions attributable to source statement `p`; the backend records it
as "current provenance" and stamps it onto subsequent instructions.
`lir-codegen`'s `lower_*` functions call `set_prov(stmt_prov.clone())` once per
source `IrStmt`/`IRStmt` before lowering it.

| Backend | `Prov` | Behaviour |
|---|---|---|
| `VolarIrTarget<P>` | `P` | `set_prov` updates `current_prov: P`; each emitted IR statement is tagged with the current value in `stmt_provs` |
| `CBackend`, other `Prov = ()` backends | `()` | Default no-op `set_prov` |

## Usage example

Attach a `u32` (e.g. a WASM byte offset) as provenance and weave an FHE
circuit while keeping it:

```rust
use volar_weaver::{weave_fhe_with_handler, KeepProvenance, FheOutput};

// `blocks: IRBlocks<u32>` — produced by a frontend that stamped each
// statement with the WASM byte offset of the instruction it came from.
let output = weave_fhe_with_handler(
    &blocks, &types, &scheme, "my_circuit",
    /* linkage */ None, /* storage */ None,
    &KeepProvenance,
);

match output {
    FheOutput::Flat(module) => {
        // module: IrModule<IrFunction<u32>, u32>
        // module.functions[0].body.stmt_provs[i] == the WASM byte offset
        // that produced (or, for infrastructure gates, is nearest to)
        // stmts[i].
    }
    FheOutput::Cfg(module) => {
        // module: IrCfgModule<u32>; same per-block stmt_provs guarantee.
    }
}
```

To discard provenance entirely (the pre-existing behaviour), pass
`&NoProvenance` instead — `H::Output = ()` and every `_with_handler` function
is a strict generalization of its `P = ()` counterpart.

## `_with_handler` functions by area

| Area | `P = ()` entry point | Generic entry point |
|---|---|---|
| FHE flat weaving | `weave_fhe_flat` (private) | `weave_fhe_flat_ir_with_handler` |
| FHE CFG weaving | `weave_fhe_cfg` (private) | `weave_fhe_cfg_with_handler` |
| FHE dispatch | `weave_fhe` | `weave_fhe_with_handler` |
| Noop (cleartext) weaving | `weave_noop_ir` | `weave_noop_ir_with_handler` |
| VAFFLE → Volar IR lowering | `lower_vaffle_to_ir::<()>` | `lower_vaffle_to_ir::<P>` (always generic) |
| VAFFLE oracle/action substitution | `substitute_vaffle` | `substitute_vaffle_with_handler` |

All of these live in `crates/compiler/volar-weaver/` (FHE/noop) and
`crates/ir/volar-ir-opt/` + `crates/ir/volar-vaffle-target/` (VAFFLE).
