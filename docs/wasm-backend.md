# WASM Backend — `volar-wasm-backend`

`WasmBackend` implements `LirTarget` (see [`docs/lir.md`](lir.md)) and emits a
real WASM binary module via [`wasm-encoder`](https://docs.rs/wasm-encoder).
It is the output-side counterpart to `volar-c-backend`/`volar-llvm-backend`;
it goes the opposite direction from the existing WASM code in this repo
(`volar-vaffle-target`'s `lower_waffle_module`, which *parses* `.wasm` bytes
into IR — see [`docs/wasm-feature-support.md`](wasm-feature-support.md)).

WASM's binary format is index-based: a `call` instruction names its target by
numeric `funcidx`, and the function index space is fixed by declaration order
(imports first, then module-defined functions). That's the one problem
neither `CBackend` (C `extern` declarations) nor `LlvmBackend` (`add_function`
forward declarations) has to solve. `WasmBackend` solves it by resolving
every callee to a final index *before* any function body is lowered, which
also happens to be exactly what makes background encoding safe (see below).

---

## Eager function declaration

`LirTarget` has two hooks, both defaulting to a no-op, that only an
index-based backend needs:

```rust
fn declare_import(&mut self, name: &str, params: &[LirType], ret: Option<LirType>) {}
fn declare_function(&mut self, name: &str, params: &[LirType], ret: Option<LirType>) {}
```

`volar-lir-codegen`'s module driver (`lower_planned_module` /
`lower_cfg_module_with_opts`) calls `declare_import` for every
`ExternalKind::Oracle`/`ExternalKind::Action` declaration, then
`declare_function` for every module function — both *before* lowering any
function body. `WasmBackend` overrides these to assign a final `funcidx`
immediately (imports first, since WASM requires them to occupy the lowest
indices). By the time any function's body is actually lowered, every name it
could possibly call — a sibling function, an oracle, an action — already has
a stable index, regardless of module order or mutual recursion.

`ExternalKind::Rng` is not routed through `declare_import`: `LirTarget::rng`
takes no name (see [`docs/lir.md`](lir.md)), so `WasmBackend` uses a single
fixed, separately-configured import instead — see
[`WasmBackend::with_rng_fn`](#imports).

Callers that drive `begin_function` directly without going through that
driver (a hand-written test, or `volar_ir_passes::lower_lir::lower_ir`'s
single-circuit entry point, which has no sibling functions to
forward-reference in the first place) don't need to call `declare_function`
themselves — `WasmBackend::begin_function` lazily self-declares a name it
hasn't seen before. Eager declaration is still the default/recommended path
for anything with more than one function; the lazy fallback exists only so
`WasmBackend` doesn't uniquely require a step no other backend needs for the
single-function case.

---

## Background encoding

No executor/thread-pool abstraction existed anywhere in this workspace
before this backend; `volar-wasm-backend` adds a minimal one:

```rust
pub trait WasmExecutor: Send + Sync {
    fn spawn(&self, job: Box<dyn FnOnce() + Send + 'static>);
}
```

`InlineExecutor` (the default — zero setup, same ergonomics as
`CBackend::new()`) just calls `job()` immediately. `ThreadPerJobExecutor`
spawns a real `std::thread` per job; plug in anything else (e.g. a
`rayon::ThreadPool`) by implementing the one method.

The reason this is safe and actually useful:

- `WasmBackend` (the front end the driver talks to) doesn't touch
  `wasm_encoder` at all. Each `begin_function`/`end_function` pair opens a
  fresh `RecordingTarget` (from `volar-lir-saved`) and forwards every call
  into it — cheap, since it's just pushing enum variants, not encoding bytes.
- At `end_function`, the finished per-function `SavedLirModule` is handed to
  the executor, along with the (already-known, from the eager declaration
  step above) `Arc<FuncRegistry>` snapshot. The driver's thread returns
  immediately and moves on to the next function.
- The background job replays that one function's call log into a
  `WasmFuncEncoder` — a second, private `LirTarget` implementation that does
  the actual `wasm_encoder` instruction encoding — and stores the resulting
  `wasm_encoder::Function` into a shared slot.
- `WasmBackend::finish` blocks until every slot is filled, then assembles the
  module's sections in the now-fixed index order.

So a large function's actual byte-level encoding overlaps with the driver
already lowering the *next* function's (cheap) call log, and — with a real
executor — with other functions' background encoding too.

---

## Control flow: the dispatch-loop relooper

LIR is block-parameter SSA over an arbitrary CFG (`jump`/`branch` to any
block); WASM only has structured, nested `block`/`loop`/`br`/`br_if`.
`CBackend` sidesteps this with `goto`; WASM has none. `WasmFuncEncoder` uses a
generic, always-correct construction:

- Every LIR block gets an integer label (`0..N`, in `create_block` order;
  the entry block is always label `0`).
- The whole function body is one outer `loop`, containing one nested `block`
  per label, opened outermost-to-innermost as label `N-1` down to label `0`
  (so label `0`'s own block is the innermost).
- A `br_table` on a dedicated `$label` local, sitting inside the innermost
  block, dispatches to any label by branching to the right nesting depth —
  landing exactly after that label's own `end`, where its code sits.
- `jump`/`branch` to another block: set `$label` to the target, then `br` back
  out to the loop (the depth depends on which label you're branching *from*).
  This handles forward and backward edges, and reducible or irreducible
  CFGs, uniformly.
- The outer `loop`'s own declared block type must match the function's actual
  result arity (not always empty) — closing a `block`/`loop` resets to
  normal, non-polymorphic validation in the *enclosing* frame regardless of
  what happened inside (an inner `return` does not make the outer loop's
  declared type irrelevant).

Block parameters become dedicated locals; a `jump`/`branch`'s argument
transfer pushes every source value first (a snapshot, taken before any
writes) and then pops them into the target block's param locals in reverse
order — parallel assignment via the operand stack itself, with no risk of one
argument's write clobbering a value another argument still needs to read.

This is a correctness-first, always-works construction, not an optimized
one — every inter-block transition goes through the dispatch, even for the
simple `if`/join and counted-loop shapes `volar-lir-codegen` actually emits
most of the time. A structured relooper that recognizes those specific shapes
(cheaper, tighter code) is a natural follow-up once this is proven correct;
it is not implemented here.

---

## Value model and type mapping

Every SSA value (including block parameters) gets its own dedicated mutable
WASM local — no stack-slot reuse. `LirType` maps to WASM value types:

| `LirType` | WASM |
|---|---|
| `Bool`, `I8`, `U8`, `I16`, `U16`, `I32`, `U32` | `i32` |
| `I64`, `U64` | `i64` |
| `Ptr(_)` | `i32` (a linear-memory address) |
| `Arr`/`Struct`/`Native`/`I128`/`U128` | not supported — panics |

Narrower-than-32-bit types are stored in their canonical bit pattern in
every local that holds one (masked/sign-extended immediately after any op
that produces one, using `i32.extend8_s`/`extend16_s`/`and` as appropriate) —
not left as raw, possibly-out-of-range `i32` bit patterns — so e.g. `U8`
arithmetic wraps correctly and `I8` comparisons see the right sign bit.

---

## Linear memory / `StackAllocExt`

`WasmBackend` always declares one WASM memory (default: 1 initial page,
unbounded growth — override via `WasmBackend::with_memory_pages`) and one
mutable `i32` global (index `0`) used as a bump-allocated stack pointer
(default base address `16`, to keep `0` distinguishable from a real
allocation).

- `begin_function` snapshots the global into a per-function entry local.
- `alloca(ty, count)` bumps the global by `count * byte_size(ty)` and returns
  the pre-bump address.
- Every `ret` restores the global from the entry snapshot first, so
  allocations are reclaimed LIFO on function exit — matching the "addresses
  into the backend's stack frame" semantics `docs/lir.md` documents for
  `Ptr`: pointers must not escape the function that allocated them.
- `ptr_load`/`ptr_store`/`ptr_offset`/`ptr_index_load`/`ptr_index_store` map
  directly onto WASM's `i32.load`/`i64.load`/`i32.store`/`i64.store` family
  and address arithmetic.

`LirAbi::WASM` (`aggregate_byval_limit: 64, native_aggregates: false`) governs
when the codegen layer routes an aggregate through this instead of inline
scalar params — the same mechanism `LirAbi::C_NATIVE`/`VAFFLE_OPTIMIZED`
already use for their own thresholds.

---

## Imports

Oracle/action declarations from a compiler-fed `IrModule` are wired up
automatically (see [Eager function declaration](#eager-function-declaration)
above). Two things are still caller-configured, since `WasmBackend` has no
other way to learn about them:

```rust
let backend = WasmBackend::new()
    .with_import("host_fn", &[LirType::U32], Some(LirType::U32)) // manual/test harnesses only
    .with_rng_fn(LirType::U64); // configures LirTarget::rng's target
```

`with_import` is only needed for hand-written test harnesses that drive
`WasmBackend` without going through `volar-lir-codegen`'s driver (which
already calls the equivalent hook automatically for oracle/action
declarations it finds in the `IrModule`). Both must be called before any
function body is declared — imports always occupy the lowest indices in
WASM's function index space.

---

## `volar-build` pipeline integration

Mirrors `compile_to_object`/`compile_lir_to_object` (LLVM), minus all the
target-triple/CPU-feature resolution — WASM is architecture-independent:

```rust
// From a Pipeline:
Pipeline::from_saved_lir("out.lir")
    .compile_to_wasm(&out.join("out.wasm"), &WasmCompileOptions::default())?;

// From a persisted .lir file (e.g. in a build.rs):
volar_build::compile_lir_to_wasm(
    Path::new("src/my_program.lir"),
    &out.join("my_program.wasm"),
    &WasmCompileOptions::default(),
)?;
```

Both are behind the `backend-wasm` Cargo feature (deliberately named
differently from the existing `pipeline-wasm` feature, which is the *input*
side — parsing `.wasm` via `portal-pc-waffle-*` — an unrelated pipeline
stage).

---

## Testing

`crates/compiler/volar-wasm-backend/tests/basic.rs` builds modules directly
against `WasmBackend`'s `LirTarget` impl (mirroring
`volar-c-backend/tests/basic.rs`'s style), then runs the result with
`wasmtime` (already a dependency of `crates/examples/volar-riscv-e2e` for the
same purpose) and checks the result:

| Test | What it exercises |
|---|---|
| `test_add_two` | Scalar arithmetic, single block |
| `test_countdown_loop` | The dispatch relooper's backward edge (a loop) |
| `test_if_max` | `branch` with both arms targeting the same join block |
| `test_forward_call` | A function calling another whose body is lowered *after* it — the scenario eager index allocation exists for |
| `test_alloca_sum` | `StackAllocExt`: `alloca`/`ptr_offset`/`ptr_store`/`ptr_load` |
| `test_background_executor_matches_inline` | Same computation via `ThreadPerJobExecutor` instead of the default inline executor |

Run via `cargo test -p volar-wasm-backend`.

---

## Still TODO

| Feature | Status |
|---|---|
| 128-bit ints, SIMD, GC types, exception handling | Not supported — panics, matching the "Still TODO" style in `docs/lir.md` |
| Finer-than-per-function background encoding (splitting one huge function's own op list into chunks) | Not implemented; per-function granularity already gets the overlap the executor is for |
| Structured (non-dispatch-loop) relooper | Not implemented; the generic dispatch-loop version is correctness-first |
| Per-name remapping (`NameConfig`, like `CBackend`/`LlvmBackend` have) | Not implemented |
| Deallocating individual `alloca`s mid-function | Only whole-frame reclaim on `ret` — matches how `Ptr`/`alloca` semantics work elsewhere in this codebase |

---

## See Also

- [LIR](lir.md) — the `LirTarget` trait, block-parameter SSA, and the other backends.
- [LIR ABI](lir-abi.md) — `LirAbi`, aggregate passing thresholds.
- [WASM feature support](wasm-feature-support.md) — the *input*-side pipeline
  (`.wasm` → IR), covering a different (and currently larger) WASM feature
  surface than this backend's *output* side.
