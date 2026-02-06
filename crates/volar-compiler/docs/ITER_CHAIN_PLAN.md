# Iterator Chain Refactor & printer_dyn Removal Plan

**Status**: Draft → Revised per feedback

## Problem Statement

### 1. Nested IrExpr iterator variants are hard to work with
The current IR represents iterator chains as deeply nested `IrExpr` variants:
```
ArrayFold {
    array: IterEnumerate {
        iter: IterSource {
            collection: Var("inputs"),
            method: Iter
        }
    },
    init: ...,
    acc_var: "a",
    elem_var: "b",
    body: ...
}
```

This encoding:
- **Is Rust-specific**: The nesting mirrors Rust's method-chaining syntax. Other backends (C, WASM, etc.) need to destructure this nesting to emit `for` loops with conditionals.
- **Is hard to analyze**: To find the source collection, you must recurse through arbitrary nesting depth. To find the terminal operation, you must look at the outermost node.
- **Duplicates logic**: `ArrayMap`/`ArrayFold` vs `IterSource + map/fold` are parallel representations of the same concept—the "Array" prefix ones came first and the "Iter" ones were added for chains, but they overlap.
- **Causes the `test_dyn_gen` failure**: `lower_expr_dyn` doesn't handle the `Iter*` variants, hitting `todo!()`.

### 2. `printer_dyn.rs` is trivial
It's just `lower_module_dyn` + `print_module`. It should be inlined/deleted.

## Design: `IrIterChain`

Replace all `Iter*` and `Array{Map,Zip,Fold}` expression variants with a single **flat** iterator chain structure. `IterLoop` (for-in loops) stays separate — it's a statement, not an expression pipeline.

```rust
/// A complete iterator pipeline from source to terminal operation.
#[derive(Debug, Clone, PartialEq)]
pub struct IrIterChain {
    /// Where the data comes from
    pub source: IterChainSource,
    /// Zero or more intermediate transformations, in order
    pub steps: Vec<IterStep>,
    /// How the pipeline terminates
    pub terminal: IterTerminal,
}

/// The data source for an iterator chain.
#[derive(Debug, Clone, PartialEq)]
pub enum IterChainSource {
    /// `expr.iter()`, `expr.into_iter()`, `expr.chars()`, etc.
    Method { collection: Box<IrExpr>, method: IterMethod },
    /// A range: `start..end` or `start..=end`
    Range { start: Box<IrExpr>, end: Box<IrExpr>, inclusive: bool },
    /// Zipping two sources: `a.iter().zip(b.iter())`
    Zip { left: Box<IrIterChain>, right: Box<IrIterChain> },
}

/// An intermediate transformation step.
#[derive(Debug, Clone, PartialEq)]
pub enum IterStep {
    Map { var: String, body: Box<IrExpr> },
    Filter { var: String, body: Box<IrExpr> },
    FilterMap { var: String, body: Box<IrExpr> },
    FlatMap { var: String, body: Box<IrExpr> },
    Enumerate,
    Take { count: Box<IrExpr> },
    Skip { count: Box<IrExpr> },
    /// Appends another iterator. The `other` is itself a chain (with a `Lazy`
    /// terminal if it hasn't been terminated yet). If parsing the chain argument
    /// fails (e.g. it's just a variable implementing `IntoIterator`), we use a
    /// default source wrapping the expression.
    Chain { other: Box<IrIterChain> },
}

/// How the pipeline terminates.
#[derive(Debug, Clone, PartialEq)]
pub enum IterTerminal {
    /// `.collect()` into a Vec (or other container)
    Collect,
    /// `.fold(init, |acc, elem| body)`
    Fold { init: Box<IrExpr>, acc_var: String, elem_var: String, body: Box<IrExpr> },
    /// No terminal yet — chain is still lazy / passed to another consumer.
    Lazy,
}
```

### `IterLoop` stays separate

`IterLoop` represents `for pattern in expr { body }` — it's a *statement*, not an
expression pipeline that yields a value. It stays as `IrExpr::IterLoop { pattern,
collection, body }`. Its `collection` field may contain an `IrExpr::IterChain(...)` if
the for-loop iterates over a chain (e.g. `for (i, x) in arr.iter().enumerate() { ... }`).
In that case the chain's terminal will be `Lazy`.

### `ArrayMap` vs `IterStep::Map`

The distinction: **`ArrayMap`** was used for both GenericArray's `.map()` (a non-iterator,
element-wise transform that returns another array of the same length) and for iterator
`.map()`. These are different operations:

- **GenericArray `.map(|x| ...)`**: Not an iterator chain. Produces another GenericArray.
  Should become `IrExpr::MethodCall { method: Map, ... }` — a plain method call on the
  array type. After dyn lowering (where GenericArray → Vec), the lowering pass can
  rewrite it as an `IrIterChain` with `source → Map → Collect`.
- **Iterator `.map(|x| ...)`**: Part of an iterator chain. Becomes `IterStep::Map`.

How to distinguish at parse time: With the chain model, the parser builds chains
inside-out. If the receiver of `.map()` already resolved to an `IrIterChain` (or an
`IterSource`), then this `.map()` is an iterator step. If the receiver is an ordinary
expression (struct, variable, etc.), it's a method call. The chain-building code in
the parser makes this distinction naturally: when peeling back the syn method chain,
if we never find an iterator source (`.iter()`, `.into_iter()`, range, etc.), the
chain doesn't exist and we fall back to `MethodCall`.

### What this replaces

| Old `IrExpr` variant | New representation |
|---|---|
| `IterSource { collection, method }` | `IrIterChain { source: Method { .. }, steps: [], terminal: Lazy }` |
| `IterEnumerate { iter }` | step: `IterStep::Enumerate` |
| `IterFilter { iter, elem_var, body }` | step: `IterStep::Filter { var, body }` |
| `IterTake { iter, count }` | step: `IterStep::Take { count }` |
| `IterSkip { iter, count }` | step: `IterStep::Skip { count }` |
| `IterChain { left, right }` | step: `IterStep::Chain { other: IrIterChain }` |
| `IterFlatMap { iter, elem_var, body }` | step: `IterStep::FlatMap { var, body }` |
| `IterFilterMap { iter, elem_var, body }` | step: `IterStep::FilterMap { var, body }` |
| `IterFold { iter, init, acc_var, elem_var, body }` | terminal: `IterTerminal::Fold { .. }` |
| `ArrayFold { array, init, acc_var, elem_var, body }` | source + terminal Fold |
| `ArrayMap { array, elem_var, body }` (on iterator) | source + Map step + Collect terminal |
| `ArrayMap { array, elem_var, body }` (on GenericArray) | stays as `MethodCall` |
| `ArrayZip { left, right, ..., body }` | source: Zip + Map step + Collect |

### What stays as-is

| `IrExpr` variant | Why |
|---|---|
| `IterLoop { pattern, collection, body }` | Statement (for-in loop), not an expression pipeline. |
| `BoundedLoop { var, start, end, inclusive, body }` | Counted `for i in 0..n` — different concept. |
| `ArrayGenerate { elem_ty, len, index_var, body }` | Array initialization by index — not an iterator chain. |
| `Repeat { elem, len }` | Array fill — not an iterator chain. |

### The new `IrExpr` variant

```rust
pub enum IrExpr {
    // ... existing variants ...

    /// A flat iterator pipeline (replaces all Iter* and Array{Map,Zip,Fold})
    IterChain(IrIterChain),

    /// For-in loop (stays separate — it's a statement)
    IterLoop { pattern: IrPattern, collection: Box<IrExpr>, body: IrBlock },

    // Removed: IterSource, IterEnumerate, IterFilter, IterTake, IterSkip,
    //          IterChain (expr), IterFlatMap, IterFilterMap, IterFold,
    //          ArrayMap, ArrayZip, ArrayFold
}
```

## Parser Strategy: Direct Chain Building

Rather than a normalization pass, the parser directly builds `IrIterChain` during
parsing. The key mechanism: **peel the syn method chain before converting expressions**.

### How syn represents method chains

For `x.iter().enumerate().filter_map(|a, c| ...).fold(init, |a, b| body)`, syn gives:
```
ExprMethodCall {
    receiver: ExprMethodCall {           // filter_map()
        receiver: ExprMethodCall {       // enumerate()
            receiver: ExprMethodCall {   // iter()
                receiver: Path("x"),
                method: "iter",
                args: []
            },
            method: "enumerate",
            args: []
        },
        method: "filter_map",
        args: [Closure(|a, c| ...)]
    },
    method: "fold",
    args: [init, Closure(|a, b| body)]
}
```

The outermost call (`fold`) enters `convert_method_call` first. Currently the code
calls `convert_expr(receiver)` which recursively converts, producing nested `IrExpr`
nodes. Instead:

### New approach: `try_build_iter_chain`

When `convert_method_call` sees a method that could be part of an iterator chain, it
calls `try_build_iter_chain(receiver_syn, method, args)` which:

1. **Peels the syn AST** from outside in, collecting steps and looking for a source:
   ```
   fn peel_chain(syn_expr: &Expr) -> (Option<IterChainSource>, Vec<IterStep>)
   ```
   Walk into `Expr::MethodCall` receivers, matching method names:
   - `iter`/`into_iter`/`chars`/`bytes` → found the source, stop
   - `enumerate`/`filter`/`map`/`take`/`skip`/`flat_map`/`filter_map` → push a step, continue into receiver
   - `zip` → source is `Zip { left: peel(receiver), right: peel(arg) }`
   - `chain` → push `IterStep::Chain { other: peel(arg) }`, continue into receiver
   - `fold` → this is a terminal, record it, continue peeling
   - `collect` → terminal Collect, continue peeling
   - anything else → not part of the chain, this expression is the deepest non-chain receiver

2. **If a source was found**: Build `IrIterChain { source, steps, terminal }`.
3. **If no source found**: The chain doesn't have an explicit `.iter()` call.
   This means it's either a method call on a non-iterator (fall back to `MethodCall`)
   or an implicit `IntoIterator`. For now, fall back to `MethodCall` for `.map()`
   (keeps GenericArray `.map()` as `MethodCall`). For `.fold()`, wrap the receiver
   as `IterChainSource::Method { collection, method: IntoIter }`.

### Handling `convert_method_call` dispatch

The current `convert_method_call(receiver: &Expr, method: &str, args: &[&Expr])` is
called from `convert_expr` when it sees `Expr::MethodCall`. The new flow:

```rust
fn convert_method_call(receiver: &Expr, method: &str, args: &[&Expr]) -> Result<IrExpr> {
    // First, try to build an iterator chain
    if is_iter_terminal(method) || is_iter_step(method) || is_iter_source(method) {
        if let Some(chain) = try_build_iter_chain(receiver, method, args)? {
            return Ok(IrExpr::IterChain(chain));
        }
    }

    // Fall through to existing MethodCall handling
    Ok(IrExpr::MethodCall {
        receiver: Box::new(convert_expr(receiver)?),
        method: MethodKind::from_str(method),
        type_args: Vec::new(),
        args: args.iter().map(|a| convert_expr(a)).collect::<Result<Vec<_>>>()?,
    })
}
```

Where `try_build_iter_chain` does the full peel-and-build, returning `None` if the
chain has no iterator source (making `.map()` on GenericArray fall through to `MethodCall`).

### Edge cases

- **`.map()` on non-iterator**: Returns `None` from `try_build_iter_chain` → becomes `MethodCall`.
- **`.fold()` on non-iterator** (e.g. `(0..n).fold(...)`): The range `0..n` is parsed as
  `Expr::Range` not `Expr::MethodCall`, so the peel stops and the range becomes the source:
  `IterChainSource::Range { ... }`.
- **`chain()` with non-chain arg**: The arg (implementing `IntoIterator`) is wrapped as
  `IterChain { source: Method { collection: arg, method: IntoIter }, steps: [], terminal: Lazy }`.
- **Bare `.iter()` not in a chain**: Produces `IrIterChain { source: Method { .. }, steps: [], terminal: Lazy }`.
  This is then used as `collection` in an `IterLoop`, or consumed by the enclosing expression.

### `for` loop integration

`convert_for_loop` already handles `Expr::Range` → `BoundedLoop`. For non-range cases:
```rust
fn convert_for_loop(pat: &Pat, iter: &Expr, body: &syn::Block) -> Result<IrExpr> {
    if let Expr::Range(r) = iter { ... BoundedLoop ... }

    // Try to build the collection as an iter chain
    let collection = convert_expr(iter)?;
    Ok(IrExpr::IterLoop {
        pattern: convert_pattern(pat),
        collection: Box::new(collection),
        body: convert_block(body)?,
    })
}
```

If `iter` is e.g. `arr.iter().enumerate()`, `convert_expr` will trigger the chain
builder and produce `IrExpr::IterChain(IrIterChain { source, steps: [Enumerate], terminal: Lazy })`.
The `IterLoop` then wraps this chain as its collection. This is correct — the loop
consumes a lazy chain.

## Migration Plan

### Phase 1: Add `IrIterChain` types to `ir.rs`
- Add `IrIterChain`, `IterChainSource`, `IterStep`, `IterTerminal` structs
- Add `IrExpr::IterChain(IrIterChain)` variant
- Keep old variants temporarily (both exist during migration)
- Move `IterMethod` enum near the new chain types
- **Tests**: unit tests for IrIterChain construction and equality

### Phase 2: Parser produces `IrIterChain` directly
- Add `try_build_iter_chain()` and `peel_chain()` to `parser.rs`
- Rewrite `convert_method_call` to try chain building first
- `try_build_iter_chain` walks the raw syn `Expr::MethodCall` chain without
  calling `convert_expr` on the receiver (avoids producing old `Iter*` variants)
- Returns `None` if no iterator source found (`.map()` on GenericArray → `MethodCall`)
- Returns `Some(IrIterChain)` with correct source, steps, terminal
- Old `Iter*` construction code is removed from `convert_method_call`
- **Tests**: parsing `x.iter().map(|a| a + 1).collect()` → `IrIterChain`,
  parsing `arr.map(|x| f(x))` → `MethodCall`, round-trip through printer

### Phase 3: Printer handles `IrIterChain`
- New `IterChainWriter` implementing `RustBackend`
  - Emits source: `collection.iter()` / `start..end` / `left.zip(right)`
  - Emits each step: `.enumerate()`, `.map(|x| ...)`, etc.
  - Emits terminal: `.collect::<Vec<_>>()` / `.fold(init, |a, b| body)` / nothing for Lazy
- Add `IrExpr::IterChain` arm to `ExprWriter`
- Remove old `Iter*` arms from `ExprWriter` and `ExprChainWriter`
- `ExprChainWriter` is simplified or removed (its only remaining purpose would be
  non-chain expressions that appear in a chaining context)
- **Tests**: printer round-trips with new chain IR

### Phase 4: Lowering handles `IrIterChain`
- `lower_expr_dyn` handles `IrExpr::IterChain` by lowering each component:
  - Source: lower the collection expression + method stays the same
  - Steps: lower expressions inside each step's body/count
  - Terminal: lower init/body in Fold, nothing for Collect/Lazy
- **This fixes `test_dyn_gen`** — no more `todo!()` for iterator expressions
- **Tests**: verify lowered chain has types transformed correctly

### Phase 5: Remove old variants
- Remove from `IrExpr`: `IterSource`, `IterEnumerate`, `IterFilter`, `IterTake`,
  `IterSkip`, `IterChain` (the old expr variant), `IterFlatMap`, `IterFilterMap`,
  `IterFold`, `ArrayMap`, `ArrayZip`, `ArrayFold`
- Remove stale `IterMethod` variants that are now only steps (Enumerate, Filter, etc.)
  — keep only `Iter`, `IntoIter`, `Chars`, `Bytes` as source methods
- Clean up all dead match arms in printer, lowering_dyn, lowering
- Update `specialized_ir.rs` tests to match on `IrExpr::IterChain` instead of old variants
- **Tests**: all existing tests still pass

### Phase 6: Remove `printer_dyn.rs`
- Inline its two functions into callers or `lib.rs` as thin wrappers
- `print_module_rust_dyn` → `print_module(&lower_module_dyn(module))`
- `print_module_rust_dyn_with_deps` → `print_module_with_deps(&lower_module_dyn(module), deps)`
- Remove `pub mod printer_dyn` from `lib.rs`
- Update `test_dyn_gen.rs` to import from `lowering_dyn` + `printer` directly

### Phase 7: Convert `test_dyn_gen.rs` to standalone tool
- Move the test logic into `examples/gen_dyn.rs` or `src/bin/gen_dyn.rs`
- Accept volar-spec path as CLI argument (default: `../volar-spec/src`)
- Accept output path as CLI argument (default: `../volar-spec-dyn/src/generated.rs`)
- Remove the test file
- Optionally: also accept manifest deps as arguments

## Affected Files

| File | Changes |
|------|---------|
| `src/ir.rs` | Add `IrIterChain`, `IterChainSource`, `IterStep`, `IterTerminal`; add `IrExpr::IterChain`; later remove 12 old variants |
| `src/parser.rs` | Add `try_build_iter_chain`, `peel_chain`; rewrite `convert_method_call`; remove old `Iter*` construction |
| `src/printer.rs` | Add `IterChainWriter`; add `IrExpr::IterChain` to `ExprWriter`; remove old `Iter*`/`Array*` arms; simplify `ExprChainWriter` |
| `src/lowering_dyn.rs` | Add `IrExpr::IterChain` handling in `lower_expr_dyn`; remove old `ArrayMap`/`ArrayZip`/`ArrayFold`/`IterLoop` arms |
| `src/lowering.rs` | Minor: if TypeContext needs to understand chain types |
| `src/printer_dyn.rs` | Delete entirely |
| `src/lib.rs` | Remove `pub mod printer_dyn`; re-export chain types |
| `tests/specialized_ir.rs` | Update `ArrayMap`/`ArrayZip`/`IterLoop` assertions to new IR shape |
| `tests/test_dyn_gen.rs` | Update imports; later convert to standalone tool |

## Estimated Effort

| Phase | Description | Size |
|-------|-------------|------|
| 1 | Add chain types to `ir.rs` | S |
| 2 | Parser direct chain building | M-L (core of the refactor) |
| 3 | Printer for chains | M |
| 4 | Lowering for chains | S |
| 5 | Remove old variants | M (mechanical but many files) |
| 6 | Remove `printer_dyn.rs` | S |
| 7 | Standalone tool | S |

Phases 2-5 can be done atomically (parser + printer + lowering + cleanup in one pass)
or incrementally (keep old + new in parallel, then cut over). The plan above takes the
incremental approach for safety, but since the parser is the only producer of these
IR nodes, switching the parser in Phase 2 means the old variants are immediately dead
code — Phase 5 is just cleanup.
