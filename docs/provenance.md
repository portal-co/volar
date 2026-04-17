# Provenance Tracking

Provenance is a per-statement annotation system that tracks where each IR
statement originated.  It flows from the input circuit through lowering,
movfuscation, and weaving into the final generated code.  Its primary uses
are **app integration** (connecting generated code back to source-level
constructs) and **debugging** (understanding which circuit gate produced a
particular spec call).

## How it works

Every block-based IR in Volar carries a type parameter `P` for provenance:

| IR type | Provenance field |
|---------|-----------------|
| `IRBlock<P>` | `stmt_provs: Vec<P>` |
| `BIrBlock<P>` | `stmt_provs: Vec<P>` |
| `IrBlock<P>` (compiler) | `stmt_provs: Vec<P>` |

The default is `P = ()`, which erases provenance and incurs no overhead.
When `P` is a meaningful type (e.g. a source location, gate ID, or
user-defined tag), each statement in every block carries a `P` value
recording its origin.

## Provenance through the pipeline

```
Source code
  ↓  parser
IrModule<()>
  ↓  lower to LIR
LirTarget::set_prov(prov)   // emitted instructions inherit current prov
  ↓  lower to Volar IR
IRBlocks<P>  /  BIrBlocks<P>
  ↓  movfuscate / lower_to_circuit
BIrBlocks<P>                // provenance preserved through transforms
  ↓  weave
IrModule<Q>                 // P → Q via Into<Q>
  ↓  print
Rust source                 // provenance discarded (metadata only)
```

### Weaving

All weaving functions accept an input circuit `BIrBlocks<P>` and produce
an output `IrModule<Q>` where `P: Into<Q>`.  This allows flexible
provenance mapping:

```rust
// Identity: keep the same provenance type
let module: IrModule<MyProv> = weave_evaluator(&circuit, "name", None);

// Erase: discard provenance (default when P = ())
let module: IrModule<()> = weave_evaluator(&circuit, "name", None);

// Convert: map to a different type
// (requires MyProv: Into<AppProv>)
let module: IrModule<AppProv> = weave_evaluator(&circuit, "name", None);
```

When a single source gate expands into multiple output statements (e.g.
OR → NOT+AND+NOT via De Morgan, or an AND gate producing both a table
computation and a wire result), all generated statements inherit the
provenance of the original gate.

### `map_prov`

After weaving, you can re-map provenance on any IR node:

```rust
let module: IrModule<CircuitProv> = weave_garbler(&circuit, "c", None);
let mapped: IrModule<AppProv> = module.map_prov(|cp| AppProv::from(cp));
```

`map_prov` is available on `IrModule`, `IrFunction`, `IrBlock`, `IrImpl`,
`IrStmt`, `IrExpr`, and all nested IR types.

## App integration example

A circuit debugger or profiler can attach provenance to track which
high-level operation each gate belongs to:

```rust
#[derive(Clone, Default)]
struct GateProv {
    /// Which high-level operation (e.g. "AES S-Box", "SHA round 3")
    operation: Option<String>,
    /// Original source line
    source_line: Option<u32>,
}

// Build circuit with provenance
let mut block = BIrBlock::<GateProv> { params: 2, stmts: vec![], stmt_provs: vec![], .. };
block.push_stmt(BIrStmt::And(IRVarId(0), IRVarId(1)), GateProv {
    operation: Some("AES S-Box".into()),
    source_line: Some(42),
});

// Weave — provenance carries through
let circuit = BIrBlocks(vec![block]);
let module: IrModule<GateProv> = weave_evaluator(&circuit, "aes", None);

// Inspect: module.functions[0].body.stmt_provs[i] tells you
// which high-level operation produced statement i
```

## Debugging example

When a generated proof fails verification, provenance lets you trace
which circuit gate produced the failing AND check:

```rust
let module: IrModule<u32> = weave_vole_verifier(&circuit, "c", None);
// stmt_provs[i] = original gate index in the BIrBlock
// If the verifier's ok_3 fails, look up stmt_provs to find the
// AND gate that produced it, then trace back to the source.
```

## LIR-level provenance

The `LirTarget` trait supports provenance via `set_prov`:

```rust
impl<Prov: Clone + Default> LirTarget<Prov> {
    fn set_prov(&mut self, prov: Prov);
}
```

Call `set_prov` before emitting instructions.  Each instruction inherits
the most recently set provenance.  The `VolarIrTarget<P>` backend stores
provenance in `IRBlock<P>::stmt_provs`, preserving it through to the
circuit IR.

## Design notes

- **Zero-cost default**: when `P = ()`, `Vec<()>` has zero heap allocation
  (the vec stores nothing meaningful but maintains the length invariant).
- **No runtime effect**: provenance is compile-time / tooling metadata.
  The Rust printer ignores `stmt_provs` entirely.
- **`Into`-based mapping**: the `P: Into<Q>` constraint is the standard
  Rust conversion trait.  It enables both identity mappings (`P = Q`)
  and lossy projections (`P → ()`).  The `(): Into<Q>` bound on weaving
  functions ensures that synthetic statements (setup code, return
  expressions) can always produce a default `Q`.
