# Iterator Chain Refactor & printer_dyn Removal Plan

## Problem Statement

### 1. Nested IrExpr iterator variants are hard to work with
The current IR represents iterator chains as deeply nested `IrExpr` variants:
```
ArrayMap {
    array: IterFilter {
        iter: IterEnumerate {
            iter: IterSource {
                collection: Var("inputs"),
                method: Iter
            }
        },
        elem_var: "x",
        body: ...
    },
    elem_var: "y",
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

Replace all `Iter*` and `Array*` expression variants with a single **flat** iterator chain structure:

```rust
/// A complete iterator pipeline from source to terminal operation.
#[derive(Debug, Clone, PartialEq)]
pub struct IrIterChain {
    /// Where the data comes from
    pub source: IterSource,
    /// Zero or more intermediate transformations, in order
    pub steps: Vec<IterStep>,
    /// What to do with the result
    pub terminal: IterTerminal,
}

/// The data source for an iterator chain
#[derive(Debug, Clone, PartialEq)]
pub enum IterSource {
    /// `expr.iter()`, `expr.into_iter()`, `expr.chars()`, etc.
    Method { collection: Box<IrExpr>, method: IterMethod },
    /// A range: `start..end` or `start..=end`
    Range { start: Box<IrExpr>, end: Box<IrExpr>, inclusive: bool },
    /// Zipping two sources: `a.iter().zip(b.iter())`
    Zip { left: Box<IrExpr>, right: Box<IrExpr> },
}

/// An intermediate transformation step
#[derive(Debug, Clone, PartialEq)]
pub enum IterStep {
    Map { var: String, body: Box<IrExpr> },
    Filter { var: String, body: Box<IrExpr> },
    FilterMap { var: String, body: Box<IrExpr> },
    FlatMap { var: String, body: Box<IrExpr> },
    Enumerate,
    Take { count: Box<IrExpr> },
    Skip { count: Box<IrExpr> },
    Chain { other: Box<IrExpr> },  // chain is odd — appends another iterator
}

/// How the pipeline terminates
#[derive(Debug, Clone, PartialEq)]
pub enum IterTerminal {
    /// `for pattern in chain { body }` — side-effecting loop
    ForEach { pattern: IrPattern, body: IrBlock },
    /// `.collect()` into a Vec
    Collect,
    /// `.fold(init, |acc, elem| body)`
    Fold { init: Box<IrExpr>, acc_var: String, elem_var: String, body: Box<IrExpr> },
    /// No terminal — the chain itself is an expression (e.g., passed to another function)
    /// This shouldn't normally happen but covers edge cases.
    Lazy,
}
```

### Key properties
- **Flat**: All steps are in a `Vec`, not nested. Easy to iterate, analyze, transform.
- **Backend-neutral**: Any backend can walk `source → steps → terminal` to emit a `for` loop, a WASM loop, a C `for`, etc. No Rust-specific chaining assumed.
- **Single representation**: No more `ArrayMap` vs `IterSource+Map` duality.
- **Analyzable**: The source collection, all transforms, and the terminal are immediately accessible without recursion.

### What this replaces

| Old `IrExpr` variant | New representation |
|---|---|
| `IterSource { collection, method }` | `IrIterChain { source: IterSource::Method { .. }, .. }` |
| `IterEnumerate { iter }` | step: `IterStep::Enumerate` |
| `IterFilter { iter, elem_var, body }` | step: `IterStep::Filter { var, body }` |
| `IterTake { iter, count }` | step: `IterStep::Take { count }` |
| `IterSkip { iter, count }` | step: `IterStep::Skip { count }` |
| `IterChain { left, right }` | step: `IterStep::Chain { other }` |
| `IterFlatMap { iter, elem_var, body }` | step: `IterStep::FlatMap { var, body }` |
| `IterFilterMap { iter, elem_var, body }` | step: `IterStep::FilterMap { var, body }` |
| `IterFold { iter, init, acc_var, elem_var, body }` | terminal: `IterTerminal::Fold { .. }` |
| `ArrayFold { array, init, acc_var, elem_var, body }` | source + terminal Fold |
| `ArrayMap { array, elem_var, body }` | source + Map step + Collect terminal |
| `ArrayZip { left, right, left_var, right_var, body }` | source: Zip + Map step + Collect |
| `IterLoop { pattern, collection, body }` | terminal: ForEach (collection becomes source) |

### What stays as-is

| `IrExpr` variant | Why |
|---|---|
| `BoundedLoop { var, start, end, inclusive, body }` | This is a `for i in 0..n` loop, not an iterator chain. It's a counted loop — a different concept. |
| `ArrayGenerate { elem_ty, len, index_var, body }` | Array initialization by index — not an iterator chain. |
| `Repeat { elem, len }` | Array fill — not an iterator chain. |

### The new `IrExpr` variant

```rust
pub enum IrExpr {
    // ... existing variants ...
    
    /// A complete iterator pipeline (replaces Iter* and Array{Map,Zip,Fold})
    IterChain(IrIterChain),
    
    // Remove: IterSource, IterEnumerate, IterFilter, IterTake, IterSkip,
    //         IterChain, IterFlatMap, IterFilterMap, IterFold,
    //         ArrayMap, ArrayZip, ArrayFold, IterLoop
}
```

`IterLoop` merges into `IterChain` with `ForEach` terminal. `BoundedLoop` stays separate.

## Migration Plan

### Phase 1: Add `IrIterChain` types to `ir.rs`
- Add `IrIterChain`, `IterSource` (renamed to avoid conflict with existing variant),
  `IterStep`, `IterTerminal`
- Add `IrExpr::IterChain(IrIterChain)` variant
- **Don't remove old variants yet** — both exist temporarily

### Phase 2: Parser produces `IrIterChain`
- Rewrite `convert_method_call` to build flat chain structures
- The tricky part: the parser sees method calls one at a time from the outermost
  call inward (syn gives us `fold(enumerate(iter(x)))` as `x.iter().enumerate().fold()`
  parsed outside-in). We need to:
  1. Recognize the terminal (outermost: `collect`, `fold`, `for_each`, or "used as expression")
  2. Peel off intermediate steps
  3. Find the source at the innermost level
  4. Build the chain flat

  Strategy: After parsing the full expression, add a **normalization pass** that
  walks nested old-style `Iter*`/`Array*` nodes and collapses them into `IrIterChain`.
  This way the parser can keep producing the old nesting (which matches syn's structure)
  and a post-pass flattens it.

  Alternative: Parse directly into chains. This requires recognizing the chain as a
  whole in `convert_method_call`. Since syn gives us the outermost call first
  (`fold` is the `method` in the `MethodCall`), we'd need to look at the receiver
  recursively to find the source. This is actually feasible:
  
  ```
  x.iter().enumerate().filter_map(|a,c| ...).fold(init, |a,b| ...)
  ```
  
  syn gives us: MethodCall { receiver: MethodCall { receiver: MethodCall { receiver: MethodCall { ... "iter" }, "enumerate" }, "filter_map" }, "fold" }
  
  We can write a recursive "peel_chain" function that walks inward collecting steps,
  then finds the source. But this means convert_method_call needs to not eagerly
  convert the receiver — it needs to peek.
  
  **Decision: normalization pass.** Parser keeps producing old variants. A new
  `fn normalize_iter_chains(module: &mut IrModule)` walks all expressions and
  collapses nested Iter*/Array* into IrIterChain. Called after parsing, before
  lowering. This is simpler and less error-prone.

### Phase 3: Printer handles `IrIterChain`  
- Add `IterChainWriter` implementing `RustBackend`
- For Rust output: reconstruct the method chain syntax from the flat structure
- Remove old `ExprChainWriter` and the old variant handling in `ExprWriter`

### Phase 4: Lowering handles `IrIterChain`
- `lower_expr_dyn` handles `IrExpr::IterChain` by lowering each component:
  - Lower types in the source expression
  - Lower types/exprs in each step's body
  - Lower types/exprs in the terminal
- This fixes the `test_dyn_gen` failure

### Phase 5: Remove old variants
- Remove from `IrExpr`: `IterSource`, `IterEnumerate`, `IterFilter`, `IterTake`,
  `IterSkip`, `IterChain` (the expr variant), `IterFlatMap`, `IterFilterMap`,
  `IterFold`, `ArrayMap`, `ArrayZip`, `ArrayFold`, `IterLoop`
- Clean up all match arms
- Move `IterMethod` enum to sit near the new chain types

### Phase 6: Remove `printer_dyn.rs`
- Inline its two functions into callers or into `printer.rs`
- `print_module_rust_dyn` = `print_module(lower_module_dyn(module))`
- `print_module_rust_dyn_with_deps` = `print_module_with_deps(lower_module_dyn(module), deps)`
- Remove `pub mod printer_dyn` from `lib.rs`
- Update `test_dyn_gen.rs` to call `lower_module_dyn` + `print_module` directly

### Phase 7: Convert `test_dyn_gen.rs` to standalone tool
- Move the test logic into `examples/gen_dyn.rs` or `src/bin/gen_dyn.rs`
- Accept volar-spec path as argument
- Accept output path as argument
- Remove the test that writes to `volar-spec-dyn/src/generated.rs`

## Open Questions

1. **`IterStep::Chain`**: `chain()` is unusual — it appends another iterator.
   In the flat model, should it be a step that references another chain?  Or
   should we keep it simple (another `Box<IrExpr>` that is the second iterator)?
   → Keep simple: `Chain { other: Box<IrExpr> }`. The `other` might itself be
   an `IrIterChain` or a simple expression.

2. **GenericArray `.map()`**: In volar-spec, `GenericArray::map(|x| ...)` is
   NOT an iterator — it's a built-in method that produces another GenericArray.
   After lowering to dyn (where GenericArray → Vec), it becomes `vec.iter().map().collect()`.
   The parser currently produces `ArrayMap` for this. In the new model, should this
   become an `IrIterChain` with Collect terminal? → **Yes**, after the dyn lowering
   pass.  Before lowering (in the static IR), it should stay as a `MethodCall` on
   GenericArray. The normalization pass should only convert things that are *already*
   iterator chains.
   
   Actually, re-examining the parser: `ArrayMap` is produced for ANY `.map()` call
   regardless of receiver type. So `foo.iter().map(|x| ...).collect()` becomes
   `ArrayMap { array: IterSource { .. }, .. }`.  This means the parser doesn't
   distinguish "GenericArray.map" from "iterator.map". The new `IrIterChain` will
   absorb both cases, which is fine — the terminal determines the semantics.

3. **Collect type inference**: Currently `ArrayMap` always emits `.collect::<Vec<_>>()`.
   With the new chain model and `Collect` terminal, we can add an optional type hint
   to `Collect` later if needed.

## Estimated Effort

| Phase | Description | Size |
|-------|-------------|------|
| 1 | Add chain types to `ir.rs` | S |
| 2 | Normalization pass | M |
| 3 | Printer for chains | M |
| 4 | Lowering for chains | S |
| 5 | Remove old variants | M (mechanical) |
| 6 | Remove `printer_dyn.rs` | S |
| 7 | Standalone tool | S |
