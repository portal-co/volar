# TS Emitter Hardening, Length-Parameterized Arrays, and a Solidity Backend — Plan

**Status:** Draft, updated after review — the open questions in the original
draft (§8) have been answered and folded into the plan below. Still no
implementation changes have been made; this document is the output of a
research pass over the current compiler, its tests, and its generated-artifact
pipeline, plus one round of direction decisions from review.
**@ai:** assisted
**Base evidence:** local `volar` tree @ `5feba83` (working tree clean at time
of writing). Commands below were run against this commit; see §1 for the
exact invocations and output used as evidence.

**Decisions from review (folded in below; see each section for detail):**

1. The semantic-equivalence harness (Part 1) targets **full spec-tree
   coverage**, not a narrow permanently-scoped subset — §2.3, §2.5, §6.
2. `ring_lwe.rs`'s bare `loop {}` gets **first-class parser support**, not a
   spec-level rewrite — §1.3.4, §2.4.
3. The monomorphize-vs-dyn-lower boundary (Parts 2 and 3) is a
   **size/instantiation-count budget**, explicitly to bound generated code
   size — §3.3, §4.4, §5.
4. The Solidity backend (Part 3) is confirmed as an **on-chain verifier**
   target, and must give **first-class, tested support to dyn-lowered IR**
   (`Vec<T>` → dynamic Solidity arrays) alongside the monomorphized/fixed-array
   path — not as a rarely-exercised fallback — §4.1, §4.2, §4.4.
5. The high-level-printer-over-`LirTarget` architecture call (§4.3) is
   confirmed. The underlying principle has been promoted to a numbered Core
   Design Rule in [`AGENTS.md`](../AGENTS.md) (rule 15).
6. Parallelism across Parts 2 and 3 is allowed where the work doesn't share a
   dependency — §6.
7. `docs/archive/compiler/TS_BACKEND_PROGRESS.md` is retired/archived once
   Part 1 lands — §2.6.

---

## 0. Scope recap

Three ordered asks:

1. Test the compiler's IR/AST → TypeScript emitter (`printer_ts.rs`) and make
   sure it actually works, starting from the known-gaps inventory that
   already exists in the repo's history.
2. Teach the TS emitter to represent statically-known array lengths as
   `[Element] & { length: Length }`-style intersection types, so fixed-size
   arrays don't have to be erased to `number[]` via dynamic lowering.
3. Build a Solidity backend that reuses the same core passes as the
   Rust/TS backends, with dynamic (runtime-length) lowering as one of
   several *optional* strategies rather than a mandatory step.

This document lays out what's actually true about the current system (§1),
then a plan for each of the three asks (§§2–4), how they interact (§5),
a suggested sequencing (§6), risks (§7), and an index of the review
decisions folded into the plan (§8).

---

## 1. Current-state findings

### 1.1 Architecture recap

- `crates/compiler/volar-compiler/src/parser.rs` — `syn`-based Rust → `IrModule`.
- `crates/compiler/volar-compiler/src/printer_ts.rs` (5873 lines) — `IrModule` → TypeScript. Consumes an already dyn-lowered module.
- `crates/compiler/volar-compiler-passes/src/lowering_dyn.rs` (3579 lines) — erases type-level lengths (`N: Unsigned`, `GenericArray<T, N>`) into runtime `usize`/`number` witnesses. Both the Rust-dyn backend (`printer.rs`) and the TS backend go through this unconditionally today:

  ```rust
  // crates/compiler/volar-compiler-passes/src/lib.rs
  pub fn print_module_typescript(module: &IrModule<IrFunction>) -> String {
      let lowered = lowering_dyn::lower_module_dyn(module);
      volar_compiler::printer_ts::print_module_ts(&lowered)
  }
  ```

- `crates/compiler/volar-lir-codegen/src/{lib.rs,mono.rs,structs.rs}` — a **separate**, lower-level pipeline: lowers `IrModule` directly to `LirTarget` (the C/WASM/LLVM backends, which live in the sibling `volar-ir` repo). Crucially, this path does **not** go through `lower_module_dyn` at all — it consumes the generic module directly and substitutes concrete const/type values per call site via `mono::MonoEnv` ("monomorphized IR", see `lib.rs` doc comment: *"Works best on monomorphized IR — run `monomorphize_module` first"*). This is the existing precedent for "optional dyn lowering" the ask wants for Solidity — it already exists for C/WASM/LLVM, just not documented as a reusable pattern and not exposed to a high-level source-text backend.
- Codegen entry points: `cargo xtask gen-specs`/`check-specs` (writes `packages/volar-spec-ts/generated.ts`, `crates/compiler/volar-compiler/volar_ts_generated.ts`, `volar_dyn_generated.rs`, `volar-spec-dyn/src/generated.rs`) and the separate `.cargo/config.toml` aliases `cargo generate-ts` / `cargo generate-spec`, which invoke a *different* binary (`volar-codegen`, in `volar-compiler-passes`) with different output paths (`packages/volar-runtime/src/generated.ts` per the alias comment, `packages/volar-spec-ts/generated.ts` per the binary's actual current default). These two generation paths have drifted from each other (see §1.3.6).

### 1.2 What's already tested — and the false-green discovery

> **Corrected during Part 1 implementation.** The original draft of this
> section asserted the tsc-strict suites "currently pass with 0 errors." That
> was wrong — those greens were **false positives** produced by a broken local
> toolchain and two masking bugs in the harness, not by a clean emitter. The
> true strict-mode baseline, measured after fixing the harness, is
> **~848 errors on the full module** (and 14–53 per seeded component). The
> emitter is *not* strict-clean; closing that is now explicit Part 1 scope.

The infrastructure, as it existed at research time:

- `crates/compiler/volar-compiler-passes/tests/ts_backend.rs` — parses all of
  `volar-primitives` + `volar-common` + `volar-spec`, runs
  `print_module_typescript`, strips `// @ts-nocheck`, and shells out to
  `tsc --strict --noEmit`.
- `crates/compiler/volar-compiler-passes/tests/ts_backend_components.rs` — the
  same idea, seeded per-component (`vole_prover`, `vole_verifier`,
  `vole_setup`, `tfhe`, `faest_core`) so one broken component doesn't block
  the others.
- `packages/volar-spec-ts/scripts/typecheck-strict.sh` (+ `npm run
  typecheck:count`) — same tsc-strict check against the checked-in
  `generated.ts`.

**Why the greens were false (three stacked masking bugs, all fixed in Part 1):**

1. **Broken local `tsc`/`zshy` shims fail open.** `node_modules/.bin/tsc` was a
   stale *copied* wrapper (from an old install) containing
   `require('../lib/tsc.js')`, which resolves relative to `.bin/` and crashes
   with `MODULE_NOT_FOUND`. Neither the Rust tests nor the shell script checked
   the process's exit status when the output contained no `error TS` lines —
   they counted 0 TypeScript diagnostics and passed. So a *non-functional* tsc
   reported "0 errors."
2. **`typecheck-strict.sh` passed an unsupported flag.** It invoked `npx tsc
   --ignoreConfig`, which tsc 5.x rejects with `error TS5023: Unknown compiler
   option` — which the script then *counted* as the strict-mode error total
   ("1 error") or, with `|| true` swallowing the status, misread as success.
   `--ignoreConfig` is a TypeScript-7-native-only flag; it needs a capability
   probe (the Rust tests already had one; the shell script didn't).
3. **An early syntax error aborts semantic checking.** The generated output
   contained an unbalanced-paren bug in the `count_ones` emission
   (`printer_ts.rs`'s `StdMethod::CountOnes` printed `((() => {...})()` — one
   `(` short). `tsc` reports `error TS1005: ')' expected` and then **abandons
   semantic analysis of the file**, so the hundreds of real type errors
   downstream were never reported. Fixing the paren unmasked them.

With all three fixed, `ts_backend.rs` reports the real number. This is the
*true* starting point for "make the emitter work": the existing typecheck
signal was measuring nothing. `TS_BACKEND_PROGRESS.md`'s resolved-error
inventory should be re-read in that light — those errors may be *resolved*,
but the suite that would've shown it wasn't actually running.

### 1.3 Known-gaps inventory (evidenced)

#### 1.3.1 No semantic/runtime equivalence testing — only typechecking

Grepping the test suite and `packages/*/scripts/*.mjs` turns up no test that
*executes* generated TypeScript and compares its output against the Rust
reference for the same input. `ts_backend.rs`'s own doc comment even
frames the goal narrowly: *"full TS codegen pipeline → tsc --noEmit"* — type
soundness, not value soundness. `TS_BACKEND_PROGRESS.md` item 10, "Dual-backend
round-trip testing," was listed as a **future refactor in Feb 2026 and is
still unimplemented** in the current tree. This matters because `tsc --strict`
passing is compatible with `printer_ts.rs` silently emitting the wrong
*value* — see 1.3.2 and 1.3.3 for two concrete ways that already happens.

This is the single highest-priority gap for "ensure it works": nothing today
proves the emitted TypeScript computes the same field elements as the Rust
spec for the same inputs.

#### 1.3.2 Width-blind wrapping arithmetic (real correctness bug)

`packages/volar-runtime/src/helpers.ts`:

```ts
export function wrappingAdd(a: bigint | number, b: bigint | number): bigint {
  const x = typeof a === "bigint" ? a : BigInt(a);
  const y = typeof b === "bigint" ? b : BigInt(b);
  return BigInt((Number(x + y)) >>> 0);   // always wraps mod 2^32
}
```

`wrappingAdd`/`wrappingSub` always wrap at 32 bits regardless of the Rust
operand's actual width. `printer_ts.rs`'s call site (`emit_known_method_call`,
`StdMethod::WrappingAdd`/`WrappingSub`) passes no width information at all.
Any spec code calling `.wrapping_add()`/`.wrapping_sub()` on a `u64` or `u128`
(both exist in the primitive-type table) will silently compute a wrong,
truncated result in TS while typechecking cleanly and while the Rust output is
correct. This is a **pre-existing, silent, latent bug**, independent of the
three asks, and per this machine's standing policy it should be fixed rather
than deferred — it belongs in Part 1, ideally as the first thing the new
semantic-equivalence harness (1.3.1) would have caught.

#### 1.3.3 Silent catch-all for unhandled `IrExprKind` variants

`printer_ts.rs`'s `TsExprWriter::ts_fmt` match ends with:

```rust
_ => write!(f, "undefined /* unsupported: {:?} */", std::any::type_name::<IrExpr>())?,
```

Today this is dead code — cross-checking `IrExprKind`'s full variant list
(`ir.rs`) against the match arms in `printer_ts.rs` shows every current
variant is handled explicitly. But `IrExprKind` is `#[non_exhaustive]`, and
this wildcard means a **future variant added anywhere in the IR** (by any
contributor, for any backend) will compile silently for the TS backend and
emit `undefined` at every use site — a value that satisfies `any`-typed
TypeScript and produces wrong-but-plausible runtime behavior with no compiler
signal. Recommend replacing the silent fallback with an explicit,
enumerated match (accepting the `non_exhaustive` warning as the forcing
function) or a `debug_assert!`/panic-in-test guard, so a new variant becomes a
loud build/test failure for the TS backend instead of a silent codegen gap.

#### 1.3.4 Totality gaps and one doc/implementation mismatch

- `crates/spec/volar-spec/src/tinylabels/ring_lwe.rs` fails to parse today
  with `Unbounded loop detected: only total (bounded) loops are allowed`
  (confirmed by running the `volar-codegen ts` binary and by the `ts_backend*`
  tests' own stderr). The offending construct is a bare `loop { … }` with a
  `break` (line ~1085) — `parser.rs` hard-rejects `syn::Expr::Loop` regardless
  of whether the loop is actually bounded. This means **every function in
  that file is silently dropped from the generated TS/Rust-dyn output** (the
  parser logs a warning and continues; nothing downstream fails loudly). This
  is a real, currently-uncovered gap in "the emitter works": an entire spec
  file's functions are missing from the generated artifacts with only a
  build-log warning as evidence.
- `docs/compiler.md`'s Totality section says *"`while` loops (unbounded)"* are
  rejected. That's inaccurate: `parser.rs` parses `syn::Expr::While` into a
  first-class `IrExprKind::WhileLoop` node, and both `printer.rs` and
  `printer_ts.rs` emit it. Only the bare `loop {}` form (`syn::Expr::Loop`) is
  rejected. **Decision: add first-class `loop` support to the parser** (not a
  spec-level rewrite to `while`) — see §2.4 for the concrete mechanism. This
  also means the doc fix isn't just "say `loop` is rejected too"; it's
  "describe what `loop` *and* `while` both actually guarantee," since neither
  form has any real static boundedness proof today (see next paragraph) —
  the totality checker's current claim is doubly inaccurate, not just
  under-scoped.
- **A sharper finding surfaced while designing the `loop` fix**: the existing
  `while` support has *no* real boundedness analysis behind it either —
  `parser.rs` accepts any `syn::Expr::While` unconditionally (e.g.
  `while r != 0 { r -= k }` parses fine today whether or not it actually
  terminates for all inputs; `ring_lwe.rs`'s own GCD-style `while` loops at
  lines 1040/1130/1148 already rely on exactly this). So `loop { .. }`
  is not meaningfully *less* total than `while` as currently accepted —
  it's missing purely because `Expr::Loop` is hard-rejected while
  `Expr::While` isn't, not because of any real difference in provable
  termination. This makes the fix simple and non-regressive (see §2.4), but
  it also means "total (bounded) loops are allowed" (the compiler's own
  totality error message, and `docs/compiler.md`'s framing) currently
  overstates what's actually checked for either loop form — worth flagging
  as a separate, smaller doc-accuracy fix alongside the `loop` support work,
  independent of whether a real termination-boundedness prover is ever built.

#### 1.3.5 CI likely cannot exercise the tsc-based tests today

`.github/workflows/ci.yml`'s `test` job runs `cargo test --workspace --exclude
xtask` with **no `actions/setup-node`, `npm ci`, or `npm install` step
anywhere in the workflow**. `ts_backend.rs`/`ts_backend_components.rs` locate
`tsc` via `<repo>/node_modules/.bin/tsc` first, falling back to `which tsc`
on `PATH`. GitHub's `ubuntu-latest` runner has a Node.js runtime preinstalled
but not a global `tsc`, and without an `npm install` step `node_modules/`
won't exist in a fresh checkout. If this reasoning is right, these tests
currently **panic** in CI (`"tsc not found — install TypeScript or add
node_modules/.bin to PATH"`) rather than passing or being skipped — i.e. the
one meaningful correctness gate on the TS backend may not actually be running
in CI. This needs to be confirmed against actual CI run logs (I don't have
repo CI history access) and fixed as the first, cheapest step of Part 1
regardless of the other findings.

#### 1.3.6 Generated-artifact / build-alias drift

- `.cargo/config.toml`'s `generate-ts` alias comment says it writes to
  `packages/volar-runtime/src/generated.ts`; the actual `volar-codegen` binary
  (per a live run today) writes to `packages/volar-spec-ts/generated.ts`. The
  alias and its own comment disagree with current behavior.
- `xtask gen-specs`/`check-specs` writes/checks four files, including both
  `packages/volar-spec-ts/generated.ts` **and** a second, separate copy at
  `crates/compiler/volar-compiler/volar_ts_generated.ts` — but `ci.yml`'s
  `check-generated` job only diffs the compiler-crate copy and the two Rust
  outputs; it never diffs `packages/volar-spec-ts/generated.ts` against a
  fresh `xtask gen-specs` run, so that file (the one actually consumed by
  `npm test`/`typecheck-strict.sh`/the published npm package) has no
  freshness check in CI at all.
- `xtask`'s own `gen-specs` and the `volar-codegen` binary duplicate almost
  identical source-collection/merge logic (`collect_rs_files`,
  `merge_modules`/dedup-by-name) independently, and can silently diverge (e.g.
  one gracefully skips a parse error, the other's behavior would need to be
  checked) since they're two separate `fn main`s.

None of this blocks correctness of a single `print_module_typescript` call,
but it means "regenerate and check" is not a single, trustworthy source of
truth today, which matters for Part 1's testing goal and for standing up a
Solidity generator the same way later.

#### 1.3.7 Minor: non-executable test script

`packages/volar-spec-ts/scripts/typecheck-strict.sh` is checked in without
the executable bit, so `npm run typecheck:count` fails with `Permission
denied` until `chmod +x` is run once locally. Small, but it's the literal
entry point for measuring TS-backend strictness, so it should be fixed in
Part 1. **(Fixed in Part 1a — executable bit set.)**

#### 1.3.8 The strict-mode error surface is real and large (found during Part 1)

Once the three masking bugs in §1.2 were fixed, the true strict-mode baseline
surfaced. Measured on the freshly-generated `packages/volar-spec-ts/generated.ts`
with a working tsc 5.9.3, `--strict --noEmit --moduleResolution bundler
--target esnext --module esnext`:

- **Full module: 848 errors.** Top categories: TS2304 (cannot find name, 206),
  TS2554 (arity mismatch, 127), TS2339 (property does not exist, 126), TS2345
  (argument not assignable, 87), TS2693 (value used as type, 79), TS2322 (72),
  TS2365 (52), TS2588 (24), plus smaller tails.
- **Seeded components: vole_prover 14, vole_verifier 14, tfhe 35, faest_core
  49, vole_setup 53.**

None of these are at the wrapping-arithmetic call sites — the width fix's
emitted `wrappingAdd(a, b, 32)` calls type-check cleanly. The errors are
pre-existing emitter bugs now visible for the first time, e.g. method calls
on `self.field` receivers that drop the receiver (`Cannot find name 'torus'`
/ `'sampler'`), witness variables referenced but never declared (`Cannot find
name 'n'` / `'table'` / `'value'`), and generic types referenced without their
dyn-suffixed emitted name (`LweBaseOt` vs `LweBaseOtDyn`).

**Implication for scope:** "the emitter typechecks cleanly" is not the
starting state. Closing this strict-error surface is now part of Part 1 (it
must precede meaningful semantic-equivalence testing — you can't trust a
value comparison against output that doesn't typecheck). The Part 1 exit
criterion is updated accordingly in §2.6.

#### 1.3.9 Progress on the strict-error surface (during Part 1 implementation)

Full-module `tsc --strict` count, cumulative: **848 → 780 → 776 → 718 → 691 → 650 → 579 → 565 → 537 → 524 → 509 → … → 469** (and seeded
components improved correspondingly, e.g. vole_prover/verifier 14 → 4). All
*syntax* errors are fixed; the remainder are semantic. Bug classes fixed:

- **Module-path qualifiers not stripped** (`torus::reduce` → `torus.reduce`,
  never defined): `TsContext::is_type_head` + leading-module-segment stripping
  in the Path writer. (Largest single win.)
- **Associated-fn type head not dyn-renamed** (`Garble::zero()` referencing the
  un-suffixed name): fixed in `lowering_dyn.rs` to rename `segments[0]`.
- **`VecDeque<T>`** type/`::new()` unmapped: now `T[]` / `[] as any[]`.
- **`X::default()`** emitted as undefined `Default.default()`/`default()`: now
  resolved to a concrete zero via the struct-field registry (or `undefined as
  any` for untracked stdlib aliases).
- **Crypto-stub types** (`Sha3_256`/`Shake128`/`Shake256`) were `type X = any`
  aliases: now `__StubDigest` subclasses so `X::new()`/`.update()`/`.finalize()`
  type-check; `X::new()` on stubs + alias resolution (`DigestImpl::new()` →
  `new Sha3_256()`) handled.
- **Rust primitives with no `PrimitiveType` variant** (`i64`, `str`, …) leaked
  as bare names: now map to `bigint`/`string`.
- **Struct-pattern `match` bindings never declared** (`PlanOp::Not { input, ..
  }`): `ts_emit_pattern_bindings` gained an `IrPattern::Struct` arm.
- **Function-local `const` items dropped** by `convert_block`'s catch-all:
  now parsed as `let` bindings.

**Remaining error classes** (each a deeper emitter/lowering feature, not a
one-line fix):

- **TS2554 arity / overload merging (largest, ~184)**: distinct functions
  sharing a bare name (e.g. free `commit<D>(msg, rand)` vs trait
  `LeafCommit::commit(r, iv, tweak)` vs `Bavc::commit(r, iv, tau, n)`)
  collapse to one emission; and length-witness params aren't threaded
  consistently between definitions and call sites. The latter is the
  *documented* `lowering_dyn` call-site length-forwarding limitation
  (pipeline.md) whose named fix is the deferred "weak type inference pass".
- **Trait-method resolution in dyn-lowering**: `L::commit(...)` (`L:
  LeafCommit`) is lowered to a bare `commit::<D>(...)`, losing the trait/impl
  identity and resolving to the wrong `commit`.
- **Static-method length witnesses**: `Array.from({length: Number(n)})` inside
  a `static` method references `n`, which is never in scope there.
- **Function-local `fn` items** (recursive helpers like bavc's `walk`):
  dropped by the parser; representing them needs a new `IrStmtKind` variant.
- **`from_fn` length placeholders without turbofish** (the parser emits a bare
  `N` → `n` when `core::array::from_fn(|i| ..)` has no explicit length): the
  lowering must infer the intended length. **Partially fixed** (commit
  bed080b, 3444997): a bounded use-site inference pass resolves the placeholder
  from a struct-field assignment use-site (the dominant `let rows = from_fn(..);
  ..; Struct { rows }` pattern) and from return-position array literals against
  the original return type (e.g. `double` → `ctx.D_OutputSize`). This took the
  `n`-placeholder errors from 12 → 4. The remaining 4 are *nested* producers
  (an inner `from_fn` inside an outer `from_fn`'s closure, e.g. the KSK's
  `Vec<[LweCiphertext; KS_ELL]>`), which need multi-level element-of-element
  length inference — a further extension of the same pass. This whole area is
  the concrete, now-partially-built instance of the pipeline's deferred "weak
  type inference pass".
- **Associated consts on impls** (`impl Fe25519 { pub const ONE: Self = ... }`,
  referenced as `Fe25519::ONE`): the parser's `convert_impl_item` drops
  `ImplItem::Const`, so `Type::CONST` references emit as the undefined
  `Type.CONST`. A proper fix needs an `IrImplItem::AssociatedConst` variant
  (touches ~15 files) OR lifting to module consts — the latter was prototyped
  and reverted because the const *value* can reference `Self` and other lifted
  consts, requiring Self-resolution during the lift. ~10 errors.

#### 1.3.10 The Rust-dyn reference backend is also broken (found during Part 1)

The semantic-equivalence harness (§2.3) compares generated TS against the
Rust-dyn reference (`volar-spec-dyn`). But that reference is itself broken:
`volar-spec-dyn/src/generated.rs` is gated behind `#[cfg(feature =
"generated")]` (off by default), so `cargo build -p volar-spec-dyn` never
compiles it. Building with `--features generated` surfaces **1474 errors** —
the same dyn-lowering witness/length-threading gaps as the TS surface (unbound
length witness `n`, `_` used as an identifier, etc.), plus more. So the
dyn-lowering is broken for *both* high-level backends, not just TS; the TS
surface was simply the only one with a (previously-masking) test wired up.

**Implication for §2.3:** the dual-backend harness cannot assume the Rust-dyn
reference is correct. Two options: (a) fix the shared `lowering_dyn`
witness/length threading first so both backends are sound (this is the real
dependency — it is the same root cause as most of the remaining TS2554/TS2339
errors), or (b) have the harness compare TS against the *non-dyn* Rust spec
(`volar-spec`, which compiles and is the actual reference) rather than
Rust-dyn. Option (b) is more faithful (volar-spec is the source of truth) and
sidesteps the broken middle layer; it requires the TS output to be driven by
the same inputs as a native `volar-spec` test harness. Recommend (b) as the
harness reference and treating the `lowering_dyn` soundness fix as its own
tracked work item (it gates both backends and Part 3's Solidity path).

### 1.4 Current array/length representation (baseline for Part 2)

`TsTypeWriter` (`printer_ts.rs`) always renders `IrType::Array { elem, .. }`
and `IrType::Vector { elem }` as plain `elem[]`, irrespective of `len`
(`ArrayLength::Const`, `TypeParam`, `TypeNum`, or `Projection`) and
irrespective of `kind` (`GenericArray` / `FixedArray` / `Slice`). Length
information exists in the IR (`ArrayLength`) but is discarded at the type
level entirely — it only survives as a *value* (a `number`/`bigint` witness
variable, post-dyn-lowering) used at construction sites (`Array.from({length:
n}, …)`). There is currently no length-carrying TS type anywhere in the
emitted output. This is exactly the gap Part 2 targets, and
`TS_BACKEND_PROGRESS.md` already named the target encoding as a "future
refactor" (§Future Refactors, item 8) without designing it — this plan
designs it.

### 1.5 No existing Solidity-related code or docs

No file in `docs/`, `PROGRESS.md`, or `goals.md` mentions Solidity, EVM, or
`solc`. This is greenfield; Part 3's architecture recommendation leans
heavily on precedent from the *existing* multi-backend design (Rust-dyn, TS,
C/WASM/LLVM via LIR) rather than any in-repo prior art.

---

## 2. Part 1 — Prove the TS emitter works, then close the gaps

### 2.1 Objective

Move from "typechecks cleanly" to "provably computes the same values as the
Rust reference," fix the concrete bugs found in §1.3, and make the existing
test suite trustworthy (i.e. actually run in CI).

### 2.2 Phase 1a — Fix the testing infrastructure itself (cheap, do first)

1. Add `actions/setup-node` + `npm ci` (root workspace, which pulls in `zshy`
   → `typescript`) to `.github/workflows/ci.yml`'s `test` job, *before*
   `cargo test`, so `node_modules/.bin/tsc` exists when
   `ts_backend*.rs` run. Confirm by checking actual CI run history for these
   tests (I couldn't access CI logs during research) — if they're already
   green somehow (e.g. a runner image ships `tsc` globally, or another job
   coincidentally populates `node_modules` first), document why instead.
2. `chmod +x packages/volar-spec-ts/scripts/typecheck-strict.sh`.
3. Reconcile `.cargo/config.toml`'s `generate-ts`/`generate-spec` alias
   comments and output paths with what `volar-codegen` actually does today,
   and decide whether `xtask` and `volar-codegen` should be merged into one
   generator (they duplicate source-collection logic) or clearly documented
   as intentionally separate (e.g. one for CI-checked freshness, one for local
   iteration) — either is fine, but the current silent drift isn't.
4. Add `packages/volar-spec-ts/generated.ts` to the `check-generated` CI job's
   diff so the npm-published artifact actually has a freshness gate.
5. Fix `docs/compiler.md`'s totality section to accurately describe `while`
   vs bare `loop` handling (§1.3.4).

Exit criteria: `cargo test --workspace` (as CI runs it) actually exercises
`ts_backend.rs`/`ts_backend_components.rs` against a real `tsc`, from a clean
checkout, with no manual setup.

### 2.3 Phase 1b — Semantic equivalence harness (the real "make sure it works")

This is the core deliverable of Part 1. **Decision: this harness targets full
spec-tree coverage, not a permanently-scoped narrow subset** — the exit
criterion for Part 1 is that every function reachable from the TS backend's
emission (i.e. everything `print_module_typescript`/`print_module_typescript_seeded`
can currently produce, once 1.3.4's `ring_lwe.rs` gap is closed) has semantic
equivalence coverage against the Rust-dyn reference, not just a representative
sample. Proposed shape, modeled on the existing `ts_backend.rs` scaffolding,
built incrementally toward that full-coverage target rather than stopping once
a first slice is green:

1. Start from the full reachable function set, not a hand-picked subset —
   walk every `IrFunction` in the combined `volar-primitives` +
   `volar-common` + `volar-spec` module (the same set `ts_backend.rs`'s
   `build_module()` already assembles) as the seed universe, and track
   coverage against it explicitly (e.g. a generated checklist/report of
   which functions have an equivalence test and which don't), rather than
   leaving "how much is covered" implicit in which entries happen to exist
   in `ts_component!`.
2. For each covered function, generate **both** `print_module_rust_dyn` and
   `print_module_typescript_seeded` output from the same source `IrModule`.
3. Compile the Rust-dyn output (`rustc` or `cargo run` against a tiny
   generated harness crate) and the TS output (via `tsc` + `node`) with a
   fixed table of concrete inputs (small integers/field elements covering 0,
   1, max value, and a couple of "random" fixed vectors for reproducibility —
   no live RNG, per the deterministic-spec rule already in force for
   `volar-spec`, which is exactly what makes full-protocol round-tripping
   tractable here: `volar-spec` already forbids the `rand` crate and defines
   a deterministic `SpecRng` trait, so a full VOLE prover/verifier or FAEST
   sign/verify round trip is not blocked by cross-backend RNG divergence the
   way it would be for an ordinary nondeterministic protocol implementation).
4. Assert byte-for-byte/value-for-value equality between the two outputs for
   every input tuple.
5. Wire this as a new integration test (e.g.
   `crates/compiler/volar-compiler-passes/tests/dual_backend_equivalence.rs`),
   structured the same way `ts_backend_components.rs` is (one test per
   component/seed group) so partial progress toward full coverage is visible
   in `cargo test` output rather than being one giant all-or-nothing test,
   gated behind `--features parsing` like its siblings, with a documented
   `node`/`rustc` toolchain requirement matching what `ts_backend.rs` already
   assumes is available.

This directly implements `TS_BACKEND_PROGRESS.md`'s long-deferred item 10 and
would have caught 1.3.2 (wrapping-arithmetic width bug) immediately.

**Build order (not a scope limit):** start with pure, RNG-free field/array
arithmetic functions (the natural home of the wrapping-arithmetic bug and of
Part 2's array-length work, and the cheapest to get right first), then expand
component-by-component through the full VOLE prover/verifier, FAEST
sign/verify, TFHE bootstrapping, garbling, and ORAM/channel-adjacent spec code
— mirroring how `ts_backend_components.rs` already scopes typechecking, but
with "reach full coverage" as the actual finish line rather than an optional
stretch goal. Full-protocol round trips (prover+verifier together, or a full
sign/verify) should land once their constituent pure functions already have
coverage, since they compose out of already-tested pieces.

### 2.4 Phase 1c — Fix the concrete known-gap bugs found in §1.3

In roughly this order (independent of each other, can be parallelized):

1. **Wrapping arithmetic width bug (1.3.2). — DONE.** Threaded the operand's
   bit width through to the emitted call sites as an explicit third argument
   (`wrappingAdd(a, b, 32)`), inferred via a new function-scoped
   primitive-width inference (`infer_wrapping_bit_width` in `printer_ts.rs`,
   resolving receivers from parameter types, struct-field types via a new
   module-level struct registry, array element types, casts, and chained
   wrapping ops). Rewrote `wrappingAdd`/`wrappingSub` in `helpers.ts` to do
   pure `bigint` masking (`(x op y) & ((1n << bits) - 1n)`) and added a new
   `wrappingNeg`. The previously hardcoded-32-bit `WrappingNeg` site was fixed
   in the same pass; the `OverflowingAdd`/`OverflowingSub` always-`false`
   overflow flag was deliberately left out of scope. On unresolved width it
   warns and defaults to 32. Runtime semantics pinned by
   `packages/volar-runtime/scripts/wrapping-smoke.mjs` (u8/u32/u64/u128
   wraparound cases).
2. **`count_ones` unbalanced-paren syntax bug (found in §1.2). — DONE.**
   `StdMethod::CountOnes` emitted `((() => {...})()` (one `(` short), which
   both broke the output and—worse—made tsc abort semantic checking, masking
   the ~848 pre-existing strict errors (§1.2). Fixed the paren; the real error
   surface is now visible and is Part 1 scope (§2.6).
3. **Harness fail-open bugs (§1.2). — DONE.** `ts_backend.rs`,
   `ts_backend_components.rs`, and `typecheck-strict.sh` now fail closed when
   tsc exits non-zero without producing `error TS` diagnostics (broken
   launcher, bad flags), and `typecheck-strict.sh` probes for `--ignoreConfig`
   support instead of unconditionally passing it. Added a `wrapping-smoke`
   npm test wired into CI and the `volar-spec-ts` test script. Stale
   `.bin/tsc`/`zshy` copied-shim problem repaired locally via reinstall.
4. **Silent unsupported-expr fallback (1.3.3). — TODO.** Replace the wildcard arm
   with an explicit, fully-enumerated match (or a loud `unreachable!`/test-time
   exhaustiveness check) so a new `IrExprKind` variant becomes a build or test
   failure for the TS backend, not silent `undefined` output.
5. **Strict-error surface closure (§1.3.8). — TODO, now explicit scope.**
   Drive the full-module strict count from 848 → 0 and the five seeded
   components to 0, fixing the underlying emitter bugs (receiver-less method
   calls, undeclared witness vars, dyn-name resolution, arity mismatches).
6. **`ring_lwe.rs` totality gap (1.3.4) — add `loop` support to the parser. — DONE.**
   Since (per 1.3.4's sharper finding) `while` is already accepted with no
   real boundedness proof beyond "it parsed," the minimal, non-regressive fix
   is to give `loop { .. }` exactly the same treatment: parse
   `syn::Expr::Loop(body)` into `IrExprKind::WhileLoop { cond: Lit(Bool(true)),
   body }` instead of hard-rejecting it with `CompilerError::UnboundedLoop`.
   This is a small, mechanical `parser.rs` change (remove the `Expr::Loop(_)
   => Err(...)` arm, add the mapping above), immediately unblocks
   `ring_lwe.rs` with no spec-source changes, and both `printer.rs` and
   `printer_ts.rs` need no changes at all since they already handle
   `WhileLoop`. A real termination-boundedness prover (distinguishing
   genuinely-bounded `loop`/`while` uses from genuinely-unbounded ones) is out
   of scope here — flagged as a possible, separate future project if false
   totality claims ever become a practical problem, not a prerequisite for
   this fix. Either way, the parser should also stop *silently* dropping a
   whole file's functions on any parse error — surface it as a hard error in
   `volar-codegen`/`xtask` (with an explicit
   `--allow-partial` escape hatch if partial output is sometimes wanted).

### 2.5 Phase 1d — Coverage expansion to the full spec tree

Once 1b's harness exists, extend seeded coverage (both the existing
typecheck-only components and the new semantic harness) to **every** part of
the spec tree, not just the parts already in the `ts_component!` table —
concretely, at minimum: `byte_gen` (prover/verifier), `garble`, `memory`
(memory-checking), the full VOLE/OT/COT suite (`base`, `base_ot`, `cot`,
`lpn`, `mpcot_reg`, `mpcot_uni`, `softspoken`, `spcot`, `iknp`, `two_party`,
`wire`, `stack`, `pool`, `group`, `ideal_cot`), the TFHE ring-LWE/RGSW/RLWE/LUT
/PBS stack (`lwe`, `rlwe`, `rgsw`, `lut`, `pbs`, `keys`, `plan`,
`plan_codec`, `gadget`, `torus`, `modswitch`, `sampler`), `curve`/`aes`, and
`oram`/`channel`-adjacent spec code if any of it is TS-targeted — i.e. close
the gap to the full, un-seeded `test_ts_backend_no_errors` test's reach
entirely, with per-component semantic (not just typecheck) coverage. Track
this as an explicit checklist (per 2.3, item 1) so "full coverage" has a
verifiable definition of done rather than being asserted informally.

### 2.6 Deliverables

- Green CI that actually runs `tsc` from a clean checkout — **and that now
  fails closed** when tsc is missing/broken rather than reporting a false
  green (§1.2).
- **A strict-clean emitter**: full-module `tsc --strict` at 0 errors (from the
  §1.3.8 baseline of 848) and all seeded components at 0. This is a
  prerequisite for the semantic-equivalence harness to be meaningful.
- A semantic-equivalence test harness with **full spec-tree coverage**, not
  an initial/partial component set — see §2.3, §2.5.
- Fixes for the wrapping-arithmetic bug (**done**), the `count_ones`
  syntax bug (**done**), the harness fail-open bugs (**done**), the
  silent-fallback risk, and the `ring_lwe.rs` gap (via first-class `loop`
  parser support, §2.4 item 6 — **done**, not left as an open gap).
- Updated `docs/compiler.md` totality section (both the `loop`/`while`
  inaccuracy and the "only total (bounded) loops are allowed" overstatement
  from 1.3.4).
- **`docs/archive/compiler/TS_BACKEND_PROGRESS.md` retired/archived** once
  Part 1 lands (move it under `docs/archive/` if it isn't already effectively
  there, and add a pointer from it to this document) — confirmed rather than
  left as an open question; nearly all of its open items are already closed,
  and this document supersedes it as the live plan for what remains.

---

## 3. Part 2 — Length parameters via `Element[] & { length: Length }`

### 3.1 Motivation

Today, `[T; N]` and `GenericArray<T, N>` both erase to plain `T[]` in the
emitted TypeScript (§1.4) via `lower_module_dyn`. Two costs:

- **Type safety**: nothing stops passing a wrong-length array to a function
  that assumes a fixed size; every length invariant that `rustc` enforces at
  compile time via `[T; N]` is silently dropped for TS consumers.
- **Necessity of dyn lowering**: because the TS printer only ever sees
  dyn-lowered IR, *every* TS function pays the runtime-witness-parameter
  tax (`ctx`/length args threaded everywhere) even when every call site in
  practice uses one concrete, compile-time-known `N`. This is the
  "avoid dyn lowering" half of the ask, and it's also what Part 3 needs
  (Solidity has no generics at all, so a length-erasure-free, per-instantiation
  path is close to mandatory there — see §4).

### 3.2 Design options considered

**Option A — Keep dyn lowering, annotate results with intersection types.**
Post-hoc, tag `number[]` results that a witness/const-analysis pass can prove
came from a fixed-length source as `number[] & { length: N }`, where `N` is
whatever numeric expression is already in scope (a runtime `number` local,
not a literal type). This is cheap (no change to `lowering_dyn.rs` or the
overall pipeline) but weak: TypeScript's structural typing doesn't verify
`{ length: N }` against actual array length at any point unless every
construction site is routed through a length-checked helper, and `N` being a
*value* (not a `number`-literal *type*) means it can't participate in
generic-length static reasoning across function boundaries (e.g. TS can't
statically catch "this function expects length 32, you passed length 16"
unless `N` is a literal numeric type parameter, not a runtime variable). This
matches `TS_BACKEND_PROGRESS.md`'s original "future goal 8" framing but,
worked through, doesn't fully deliver on "avoid dyn lowering" — it's still
dyn-lowered underneath.

**Option B — Print directly from the pre-dyn-lowering generic `IrModule`,
keeping length generics as TS generic type parameters.** Add a new TS
emission path that does *not* call `lower_module_dyn` first. Type-level
length parameters (`N: Unsigned`, classified `GenericKind::Length` by
`const_analysis.rs`) become TS generic parameters bound `N extends number`;
`[T; N]` becomes `T[] & { length: N }` in signatures; concrete call sites
(where `N` is statically known, e.g. `commit::<U32>(...)`) can either
instantiate the TS generic explicitly or (better, see Option C) be
monomorphized so the emitted signature has a numeric *literal* type
(`T[] & { length: 32 }`) instead of a symbolic `N`.

**Option C — Monomorphize per concrete instantiation, emit fixed-literal
lengths.** Reuse (or port to the `IrModule` level) the substitution machinery
`volar-lir-codegen::mono::MonoEnv` already implements for the C/WASM/LLVM
path: walk the call graph from a set of seeds, and for each distinct
`(function, concrete const/length args)` pair, emit one specialized function
whose array types carry the literal length (`Element[] & { length: 32 }`,
or even `[E,E,E,...]` fixed-arity tuple types for small N, which TypeScript
*does* structurally enforce). This eliminates ctx/witness plumbing for length
entirely at those call sites — genuinely "avoiding dyn lowering," not just
disguising it — at the cost of code-size growth proportional to the number of
distinct instantiations (same tradeoff the C backend already accepts).

### 3.3 Recommendation

**B, with C as the default strategy for known-finite instantiation sets, and
dyn lowering kept as an explicit fallback for genuinely runtime-variable
lengths** (e.g. variable-length signature/witness data that can't be
monomorphized). Concretely:

1. Add a monomorphization pass operating on the *high-level* `IrModule`
   (not LIR) — either by porting `MonoEnv`'s type/length substitution logic
   out of `volar-lir-codegen::mono` into a shared, backend-agnostic crate
   (candidate home: `volar-compiler-passes`, alongside `lowering_dyn.rs`, as
   a sibling "lowering strategy"), or by writing a new, smaller pass scoped
   to just `IrType`/`ArrayLength`/`IrExpr::TypenumUsize`/`LengthOf`
   substitution (the parts of `MonoEnv` actually needed at this level,
   without the LIR-specific struct-flattening machinery).
2. Extend `TsTypeWriter` to emit `Element[] & { length: N }` for
   `IrType::Array` when a length is staticaly known in the current emission
   context (either a literal `ArrayLength::Const`/`TypeNum` post-monomorphization,
   or a symbolic `N extends number` generic pre-monomorphization for the
   still-generic path).
3. Extend the witness/`ctx` system (`compute_function_witnesses`,
   `write_ctx_param`) to recognize when a length witness is now a *type-level*
   parameter instead of a runtime `ctx` field, and stop injecting it into
   `ctx` in that case.
4. Extend value-construction sites (`ts_default_value`, `Array.from({length:
   …})`, `RawMap`/`RawZip`/`ArrayGenerate` emission, `Repeat`) to preserve the
   `& { length: N }` type through the operation, which likely needs small
   typed runtime helpers in `packages/volar-runtime` (e.g. a `fixedArray<N
   extends number, T>(n: N, f: (i: number) => T): T[] & { length: N }`
   wrapper) rather than relying on TypeScript's control-flow inference to
   carry it through raw `Array.from`.
5. **Boundary rule (decided): a size/instantiation-count budget, chosen
   explicitly to bound generated code size.** Monomorphize a function for a
   given concrete instantiation only while the running total of distinct
   `(function, concrete const/length args)` instantiations reachable from the
   configured entry points stays under a configurable budget; once a
   function's (or the module's) instantiation count would exceed it, fall
   back to dyn lowering for the remaining/over-budget instantiations instead
   of continuing to monomorphize unboundedly. This is the same shared rule
   Part 3 needs for Solidity's contract-size constraints (§4.4) and the same
   shape as `chunk_fns.rs`'s existing threshold-based chunking — recommend
   literally sharing the budgeting mechanism (a count and/or an estimated
   emitted-size accumulator) between the two call sites rather than
   reinventing it per backend. See §5 for where this rule lives in the
   shared pass.

### 3.4 Important TypeScript caveats to design around

- `{ length: N }` where `N` is a numeric *literal type* is a real, enforced
  static shape — TS checks it structurally at assignment sites. But nothing
  about `T[]` at runtime *guarantees* its `.length` matches; the type is only
  as sound as every construction site that claims it. This means Part 2's
  value is contingent on routing every array-producing operation through
  length-preserving helpers (item 4 above), not just annotating types after
  the fact.
- Very large `N` (say, thousands, plausible for some LWE/RLWE parameter sets)
  makes literal-type array encodings unwieldy if ever represented as tuple
  types (`[E, E, E, ...]`, which TS can express but which are impractical
  past a few dozen elements); `T[] & { length: N }` (an intersection with the
  regular array type, not a tuple) is the right choice specifically because
  it avoids that blowup — worth calling out explicitly since it's easy to
  reach for tuple types instead and regret it.
- Numeric literal type parameters interact awkwardly with arithmetic
  (`N extends number` doesn't let TS compute `N + 1` as a type); any spec
  function whose output length is a function of its input length(s) (e.g.
  concatenation, `LengthDoubler`) will need either monomorphization to
  resolve the arithmetic to a literal before emission, or an `any`/erased
  fallback for the output length specifically. This is likely to be the
  single biggest source of "can't cleanly express this in the type system"
  cases — worth prototyping against `LengthDoubler`/`Output<D>` (the
  associated-type-driven length case already special-cased in
  `TsTypeWriter`) early, since it's the hardest existing case.

### 3.5 Testing plan

- Extend Part 1's semantic-equivalence harness (§2.3) to also assert on the
  *type-level* shape of generated signatures where feasible (e.g. a
  `tsd`/`expect-type`-style compile-only assertion that a known-fixed-length
  function's return type is `T[] & { length: 32 }`, not just `T[]`).
  the reachable instantiation set from
- Add negative tests: a hand-written TS call site passing a wrong-length
  array should fail `tsc --strict`.
- Re-run the full `ts_backend*` suite after the change to confirm no
  regression in the currently-clean 0-error baseline.

---

## 4. Part 3 — Solidity backend

### 4.1 Confirmed use case: on-chain verification

**Confirmed.** The Solidity backend targets **on-chain verification**: pure,
deterministic, RNG-free *verification* logic (the `verifier.rs`/`sign.rs`
"verify" half, transcript/Fiat–Shamir recomputation, field arithmetic) for a
proof/signature produced off-chain by the existing Rust/TS backends — not
proving (computationally unsuited to the EVM's gas model). This drives the
scope in §4.6.

**Also confirmed, and important**: this is *not* solely a "monomorphize
everything to fixed-size arrays" backend. Handling **dyn-lowered IR directly**
— the same `IrModule` shape `lower_module_dyn` already produces for the
Rust-dyn and TS backends, with `Vec<T>`-typed fields/values and runtime
`usize` length witnesses — is explicitly **in scope as a first-class,
tested path**, alongside the monomorphized/fixed-array path from §3.2 Option
C. See §4.2 and §4.4 for what that means concretely; this is a change from
the original draft, which treated dyn-lowered input as an edge-case fallback
rather than an equally-real target shape.

### 4.2 Solidity/EVM constraints that shape the design

Researched against current Solidity language semantics:

- **No generics.** Solidity has never supported generic functions or types.
  Every function must have concrete parameter/return types. A *generic*
  `IrModule` function (unresolved type-level length params, `impl Trait`
  dispatch) is not representable in Solidity at all — it must first become
  either a monomorphized instantiation (§3.2 Option C — fixed, literal
  lengths) or a dyn-lowered function (runtime `usize` length parameter plus
  `T[] memory`/`T[] calldata`, exactly what `lower_module_dyn` already
  produces for Rust-dyn/TS today). **Both are first-class, required targets
  for this backend**, not a primary path plus a rarely-exercised fallback
  (§4.1): the size/instantiation-count budget from §3.3/§5 decides which
  strategy a given function/instantiation set uses, and `printer_sol.rs`
  must correctly emit both shapes.
- **Fixed-size vs dynamic arrays.** Solidity has real fixed-size array types
  (`uint8[32]`), unlike TypeScript — so a monomorphized `[T; 32]` maps
  directly and natively, with no `{length: N}` trick needed; the trick is
  TS-specific and Solidity doesn't need it. Solidity also has real dynamic
  arrays (`T[] memory`/`T[] calldata`/`T[] storage`), which is exactly what
  dyn-lowered `Vec<T>` maps to — unlike TypeScript's `number[]`, this is a
  *native, first-class* Solidity type, not an erasure-of-something-better.
  Dynamic arrays cost more gas (length-prefixed layout, dynamic bounds
  checks) than fixed arrays, which is the practical reason to prefer
  monomorphization within the size budget and reserve dyn-lowered dynamic
  arrays for cases that are genuinely runtime-variable-length (e.g. signature
  byte strings) or that would blow the code-size budget if monomorphized.
- **Checked-by-default arithmetic (Solidity ≥0.8.0).** `+`/`-`/`*` revert on
  overflow/underflow by default; Rust's `wrapping_add`/`wrapping_sub`
  semantics require explicit `unchecked { ... }` blocks. This is the same
  bug class as §1.3.2 in a different backend — worth fixing generically
  (width-aware wrapping helpers) once, and reusing the fix's *design* (not
  its TS-specific code) here.
- **Integer widths.** Solidity has native `uint8`/`uint32`/`uint64`/`uint128`/
  `uint256` etc., so (unlike the TS backend's uniform `bigint` choice)
  primitive types can map to their *exact* width, which actually makes the
  width-aware wrapping-arithmetic fix easier to get right here than in TS —
  worth doing Solidity's version with the correct width from day one rather
  than inheriting TS's original mistake.
- **No native GF(2^8)/GF(2^64) field arithmetic.** `Bit`, `Galois`,
  `Galois64`, `BitsInBytes`, `BitsInBytes64` (the primitive types in
  `ir.rs`) all need library implementations in Solidity — direct analogs of
  `volar-runtime/src/primitives.ts`'s `gf256Mul`/`gf64Mul` shift-and-XOR
  loops, implementable as a pure Solidity `library` (e.g.
  `FieldOps.sol`) with `pure` functions operating on `uint8`/`uint64`. These
  loops are small, bounded (8 or 64 iterations), and gas-cheap — no design
  risk there, just a direct, mechanical port.
- **No mutable references / value semantics.** Solidity's `memory` vs
  `storage` vs `calldata` data-location annotations have no Rust/TS
  equivalent in the current IR at all; every emitted function will need
  explicit location annotations. For a `pure`/`view` verifier-style backend
  (§4.1), this is simpler than the general case — everything can plausibly be
  `memory` — but it's still a new axis the printer needs to reason about that
  neither `printer.rs` nor `printer_ts.rs` has any concept of.
- **Gas / block-gas-limit bound on loops.** All the totality guarantees
  already enforced by the compiler (bounded loops, no recursion — see
  `docs/compiler.md`) are a good match for Solidity, which also strongly
  prefers/requires bounded loops for gas-cost predictability. This is a
  point in favor of targeting Solidity at all — the existing totality
  discipline is exactly what a gas-bounded target needs, and needs no new
  compiler-level work.
- **No floating point** — not a concern here since the spec has none either.

### 4.3 Architecture recommendation: high-level printer, not `LirTarget`

**Confirmed.** This is the single biggest architectural fork in the plan
(originally flagged for explicit sign-off) and the call is now made: a
high-level `printer_sol.rs`, not a `LirTarget` implementation. The underlying
principle — pick a high-level `IrModule` printer over a `LirTarget`/SSA-CFG
backend whenever the target language is itself structured/AST-like, and
reserve `LirTarget` for genuinely SSA/CFG-shaped targets — has been promoted
to **Core Design Rule 15 in [`AGENTS.md`](../AGENTS.md)**, so future backend
decisions in this repo don't have to re-derive this reasoning from scratch.

Two real options exist, given the current pipeline (§1.1):

- **(i) A new `printer_sol.rs`**, structurally mirroring `printer_ts.rs`:
  consumes `IrModule` (monomorphized per §3.2/§4.2, with dyn lowering as a
  fallback for the dynamic-array case) directly, emits structured Solidity
  source (functions, `if`, `for`, structs) 1:1 from the already-structured
  `IrExpr`/`IrStmt` tree.
- **(ii) A new `LirTarget` implementation** (`SolidityBackend`), living
  alongside `volar-c-backend` in the sibling `volar-ir` repo, consuming
  the low-level SSA/CFG `LirTarget` calls the way `CBackend` does (record-then-render, `vN` locals, block-parameter based control flow).

**Recommendation: (i).** Reasoning:

- Solidity is a structured, high-level language (functions, `if`, `for`,
  no `goto`/labels) much closer to Rust/TS than to C's ability to target
  arbitrary control flow via labels/`goto`. `CBackend`'s SSA/CFG →
  goto-based-C lowering exists because C *can* represent arbitrary block
  jumps; Solidity can't, so a CFG-based backend would need to reconstruct
  structured control flow from a CFG — real, solvable, but pure incidental
  complexity that the high-level `IrModule` path (which never lost the
  original `If`/`BoundedLoop`/`Match` structure in the first place) avoids
  entirely.
- On-chain contract code has a much higher bar for human/auditor
  readability than C output aimed at `cc`. Preserving 1:1 structure from the
  spec (the same reason `printer.rs`/`printer_ts.rs` exist as high-level
  printers rather than routing Rust/TS through LIR) matters more here, not
  less.
- The pieces this needs already exist at the *right* level for option (i):
  `const_analysis.rs`'s length/type classification, `deshadow.rs`, and a
  monomorphization pass (new, per §3.2/§4.2) all operate on `IrModule`/`IrExpr`
  already; none of them are LIR-specific.

`volar-lir-codegen::mono::MonoEnv` remains valuable groundwork either way —
even choosing (i), its substitution *logic* (not its LIR-emission calls) is
the direct model for the `IrModule`-level monomorphizer both Part 2 and
Part 3 need. Recommend factoring the substitution core
(`type_args_to_len`/`mono_type`/`mono_len`/`MonoEnv` itself) so it can be
shared between the LIR path and this new `IrModule`-level path instead of
duplicated, rather than writing Solidity's monomorphizer from scratch.

### 4.4 Core-passes reuse plan

Confirms and elaborates the "reuse the same core passes, with optional dyn
lowering" half of the ask:

| Pass | Reused as-is? | Notes |
|---|---|---|
| `parser.rs` | Yes, unchanged | Same `IrModule` for all three backends. |
| `const_analysis.rs` (Length vs Type classification) | Yes, unchanged | Solidity needs this even more, to decide what *must* monomorphize vs. dyn-lower. |
| `deshadow.rs` | Yes, unchanged | Self-shadowing binding rename is language-agnostic. |
| New: `IrModule`-level monomorphizer | New, shared with Part 2 | Ported/extracted from `volar-lir-codegen::mono::MonoEnv`; applies the shared size/instantiation-count budget (§3.3, §5) to decide, per function, whether to monomorphize or hand off to `lower_module_dyn`. |
| `lowering_dyn.rs` | **Yes, first-class — not merely a fallback.** | Selected by the budget rule (§3.3/§5) for functions/instantiation sets that are over-budget to monomorphize, or whose length is genuinely runtime-variable. `printer_sol.rs` must consume this output directly and correctly, the same way `printer_ts.rs` already does — see the dedicated breakdown below. |
| Witness/`ctx` system (`printer_ts.rs`) | New Solidity-specific analog needed | Solidity has no closures/first-class functions in the same sense; type-parameter witnesses likely become either monomorphized-away entirely (preferred, since Solidity can't express `impl Trait`/generic dispatch at all) or fixed library addresses, not a `ctx` object — needs its own design pass, not a direct port. Dyn-lowered length witnesses (the `let n = self.n;` pattern `lowering_dyn.rs` already injects) map straightforwardly to a Solidity `uint256 n` local, no new design needed there. |
| `chunk_fns.rs`/module chunking | Possibly reused | Solidity has a 24KB contract code-size limit (EIP-170); large generated modules may need the same chunking discipline `emit_woven_ts_chunked` already applies, likely via separate library contracts rather than separate files. The same budget mechanism that gates monomorphization (§5) is also the first line of defense against hitting this limit. |

**What "first-class dyn-lowered support" concretely requires in
`printer_sol.rs`**, mirroring how `printer_ts.rs` already handles
`lower_module_dyn`'s output (`docs/compiler.md`'s Dynamic Lowering section):

- `IrType::Vector { elem }` (dyn-lowered `Vec<T>`) → Solidity dynamic array
  type, with the correct data-location annotation per call site (`memory`
  for most verifier-style pure/view functions per §4.2's data-location
  point; `calldata` where a function parameter is never mutated, for gas
  savings — a Solidity-specific optimization with no Rust/TS analog).
- The `usize` length-witness fields `lower_module_dyn` prepends to dyn-lowered
  structs (e.g. `Vope<N, T, K>` → `VopeDyn<T>` with `n`, `k: usize` fields)
  → Solidity `uint256` struct fields, and the `let n = self.n;` unpacking
  statement injected at each method's top → a plain Solidity local variable
  declaration, same idea as `printer_ts.rs`'s handling.
- Dyn-lowered static-method length parameters (type-level lengths erased to
  explicit `usize` arguments) → explicit `uint256` function parameters, the
  direct Solidity analog of what `lower_module_dyn` already does for
  Rust-dyn's `alloc`-friendly, monomorphization-free functions.
- `GenericArray::default()` → `vec![]` (dyn-lowered) → Solidity `new
  T[](0)` (dynamic array of length 0), and nested `DefaultValue { Array }`
  → `IterPipeline(0..n).map(...)` → a Solidity `for` loop populating a
  freshly-allocated `T[](n)`.
- This path should get its **own** seeded test-component tier (§4.7),
  separate from the monomorphized-fixed-array tier, since they exercise
  different printer code paths and different Solidity type shapes.

### 4.5 Runtime library plan

Mirror `packages/volar-runtime` with a `packages/volar-runtime-sol` (or
`contracts/`) Solidity library:

- `FieldOps.sol` — `Bit`/`Galois`/`Galois64`/`BitsInBytes`/`BitsInBytes64`
  arithmetic, direct ports of `gf256Mul`/`gf64Mul` and the `fieldAdd`/`fieldSub`/
  `fieldMul`/`fieldBitxor`/etc. dispatch, but *not* needing the TS runtime's
  `any`-typed multi-type dispatch (`isFieldElement` runtime checks) since
  Solidity's static typing and lack of generics means each call site already
  knows its concrete field type at compile (monomorphization) time — this
  should end up *simpler* than the TS runtime, not a straight port. This
  holds for both the monomorphized and dyn-lowered emission paths equally,
  since dyn lowering erases *lengths*, not field-element *types* — a
  dyn-lowered function still knows its concrete field type statically.
- Width-correct `unchecked`-block wrapping-arithmetic helpers (§4.2), fixed
  from day one rather than inheriting TS's original bug.
- Byte/array conversion helpers analogous to `asRefU8`/`u32_from_le_bytes`/etc.
- Dynamic-array helpers for the dyn-lowered path specifically (§4.4) — e.g.
  a small library of `memory`/`calldata` array-construction and
  bounds-checked-indexing helpers mirroring what `Array.from({length: n},
  ...)` does in the TS runtime, since Solidity's dynamic-array construction
  syntax (`new T[](n)`) is more restrictive than TS's.

### 4.6 Scope recommendation for v1

Confirmed as on-chain verification (§4.1); within that:

1. Target `pure`/`view` functions reachable from an explicit, configured
   seed list (mirroring `print_module_typescript_seeded`), not the whole spec
   tree — verification/field-arithmetic functions first, the same natural
   starting point as Part 1's semantic-equivalence harness. **Both the
   monomorphized and the dyn-lowered emission paths (§4.1, §4.4) are in
   scope for v1** — this isn't deferred to a later version; a verifier
   backend that only handled fixed-size arrays would fail on any
   variable-length signature/witness data, which is common in this domain.
2. No contract *state* (storage variables) in v1 — everything is
   function-parameter-in, return-value-out, matching how the spec functions
   are already written (they're not stateful objects; `struct` instances are
   values, not accounts). Dyn-lowered `Vec<T>` fields still use `memory`
   data location in this scope, never `storage`.
3. No proving-side code — proving is computationally unsuited to on-chain
   execution and there's no evident use case for it.
4. Explicit Solidity version pin (recommend the latest audited-stable 0.8.x
   at implementation time) and an SPDX license header matching this repo's
   `CC0-1.0 AND MIT AND Apache-2.0` workspace license, consistent with how
   `printer_ts.rs`'s preamble already emits fixed boilerplate.

### 4.7 Testing plan

Mirror Part 1's two-tier approach exactly, and **run both tiers against both
emission paths** (monomorphized/fixed-array and dyn-lowered/dynamic-array,
§4.1/§4.4) as separate, explicitly labeled test groups — mirroring how
`ts_backend_components.rs` already keeps components independent so one
failing path doesn't block the other:

- **Typecheck tier**: `solc --strict-mode` (or Foundry's `forge build`) in
  place of `tsc --strict --noEmit`, same shape as `ts_backend.rs`. Run once
  against monomorphized output, once against dyn-lowered output, for the same
  underlying seed functions where both shapes are meaningful (i.e. functions
  whose length parameters are small enough to be monomorphized *and* that
  can be forced through the dyn-lowered path for comparison).
- **Semantic tier**: reuse Part 1's dual-backend equivalence harness design
  (§2.3), adding Solidity as a third leg — same fixed input tables, executed
  via `forge test`/Foundry cheatcodes or `solc` + a minimal EVM
  runner (e.g. `revm` or `anvil`) rather than `node`, asserting equality
  against both the Rust-dyn and TS outputs for the same inputs, again for
  both the monomorphized and dyn-lowered Solidity outputs independently.
- Gas-cost regression tracking is out of scope for v1 correctness testing but
  worth a lightweight `forge snapshot`-style baseline once the backend
  exists, so later changes don't accidentally blow up gas costs unnoticed —
  and a natural place to observe the monomorphized-vs-dyn-lowered gas
  tradeoff directly (higher deploy cost / cheaper calls vs. the reverse),
  which could later inform tuning the size/instantiation budget (§3.3/§5).

### 4.8 Phased rollout

1. Extract/port the `IrModule`-level monomorphizer (shared with Part 2),
   including the size/instantiation-count budget (§3.3/§5).
2. `FieldOps.sol` runtime library + typecheck-tier test harness (`solc`),
   proven against a hand-written trivial function first (not yet
   generator-produced) to validate the toolchain wiring.
3. `printer_sol.rs` skeleton, monomorphized path only: primitives, structs,
   straight-line arithmetic functions only (no control flow) — smallest
   possible vertical slice.
4. Add `if`/`BoundedLoop`/array indexing to the monomorphized path.
5. **Add the dyn-lowered path** (§4.4): `IrType::Vector` → dynamic array,
   `uint256` length-witness fields/parameters, dyn-lowered `DefaultValue`
   expansion. This is not deferred past the monomorphized path's first
   working slice, but it is sequenced after it, since the monomorphized path
   is the simpler printer surface to validate the toolchain (`solc`, gas
   accounting, data-location annotations) against first.
6. Wire both paths into the seeded-component test pattern from
   `ts_backend_components.rs`, starting with the same small field-arithmetic
   seed set Part 1's semantic harness starts with, run through both emission
   paths per §4.7.
7. Extend to a real verifier component (needs Fiat–Shamir/transcript
   recomputation — the first genuinely protocol-shaped target, and likely
   the first case that actually exercises variable-length data and therefore
   forces the dyn-lowered path in practice) once the above is solid.

---

## 5. Shared architecture across Parts 2 and 3

Both parts need the same new capability: an `IrModule`-level monomorphizer
that substitutes concrete const/length/type arguments per reachable
instantiation, as an alternative to `lowering_dyn.rs`'s blanket erasure.
Recommend building this **once**, as a new pass in `volar-compiler-passes`
(sibling to `lowering_dyn.rs`, likely `mono_dyn.rs` or similar), with:

- A shared substitution core ported from `volar-lir-codegen::mono::MonoEnv`
  (or refactored so both crates depend on one shared substitution crate —
  worth deciding based on how much LIR-specific baggage `MonoEnv` actually
  carries; from the code read during research, `type_args_to_len`/`mono_type`/
  `mono_len` look substantially LIR-agnostic already).
- A pluggable "length strategy" concept per backend/per-call-graph: dyn-lower
  (existing default, still used by Rust-dyn and available to TS/Solidity for
  runtime-length cases), or monomorphize (new, becomes the TS/Solidity
  default for statically-bounded instantiation sets).
- **A shared size/instantiation-count budget (decided, §3.3 point 5).** The
  monomorphizer tracks, per configured entry-point set, the running count of
  distinct `(function, concrete args)` instantiations (and/or an estimated
  emitted-size accumulator, same idea as `chunk_fns.rs`'s existing
  threshold). While under budget, monomorphize; once a function/instantiation
  set would push the total over budget, route it through `lowering_dyn.rs`
  instead. This one rule is the entire answer to "avoid dyn lowering" (TS,
  Part 2) *and* "optional dyn lowering" (Solidity, Part 3) — both backends
  call the same budgeted-monomorphize-or-dyn-lower decision function; they
  differ only in what they do with each resulting shape (TS: `T[] & {length:
  N}` vs. `T[]`; Solidity: `T[N]` vs. `T[] memory`/`calldata`, per §4.1/§4.4
  — Solidity's dyn-lowered path is explicitly first-class, not merely
  available, so this budget is a real load-bearing decision there, not just
  an edge-case knob). The concrete threshold value(s) are an implementation
  detail to tune empirically once the pass exists (start conservative, watch
  generated-code size in CI, adjust) — not something to pin down in this
  plan.
- This directly satisfies "optional dyn lowering" for Solidity and "avoid dyn
  lowering" for TS with one implementation, not two.

---

## 6. Suggested sequencing

**Parallelism across Parts 2 and 3 is allowed** where the work doesn't share
a dependency (revised from the original draft's strict Part 1 → 2 → 3
recommendation). Concretely:

1. **Part 1 first**, entirely, before any Part 2/3 implementation work
   lands. It's the cheapest, most independently valuable piece (CI fix
   alone is high-value/low-risk), and both Parts 2 and 3 need a trustworthy
   semantic-equivalence harness to validate themselves against — building
   that harness twice would be wasted work if Part 1 builds it properly
   once. This part doesn't parallelize away; everything downstream depends
   on it existing and being trustworthy.
2. **Once Part 1 lands, Parts 2 and 3 may proceed in parallel for the work
   that doesn't share a dependency**:
   - Part 3's `FieldOps.sol` runtime library (§4.5) and the `solc`/Foundry
     toolchain-wiring/typecheck-tier harness (§4.7, §4.8 step 2) have no
     dependency on Part 2 or on the shared monomorphizer (§5) — these can
     start as soon as Part 1's toolchain-availability fix (§2.2) establishes
     the pattern for wiring an external compiler into CI, in parallel with
     Part 2.
   - The **shared monomorphizer (§5) is a hard serialization point**: both
     Part 2's `TsTypeWriter`/witness-system changes and Part 3's
     `printer_sol.rs` monomorphized-path work (§4.8 steps 3–4) depend on it
     existing. Whichever of Part 2 or Part 3 needs it first should build it
     (as the shared pass described in §5, not a Part-2-only or
     Part-3-only artifact), and the other part consumes it rather than
     duplicating it.
   - Part 3's dyn-lowered path (§4.4, §4.8 step 5) depends only on
     `lowering_dyn.rs` (already exists, unchanged) and `printer_sol.rs`'s
     own skeleton (§4.8 step 3) — not on Part 2 at all — so it can proceed
     in parallel with Part 2's work once `printer_sol.rs`'s skeleton exists.

Rough sizing (for planning only, not a commitment): Part 1 is the largest
single piece of work given the full-coverage decision (§2.3, §2.5) — it's
open-ended by nature of "cover everything," not bounded by a fixed subset
anymore; Part 2 is medium, gated mostly on the `LengthDoubler`/
associated-type-length edge case (§3.4) and on the shared monomorphizer
landing; Part 3 is the largest single piece of *new* code (a whole new
printer + runtime library, now explicitly covering two emission paths
instead of one) but lowest-risk architecturally once Part 1 establishes the
pattern and the shared monomorphizer exists for its monomorphized path — its
dyn-lowered path and runtime-library groundwork can start earlier than that.

---

## 7. Risks and non-goals

- **Non-goal for this plan**: fixing the `volar-compiler`/`volar-compiler-passes`
  git-vs-path dependency setup noticed during research (several crates in
  this workspace declare `[workspace.dependencies]`/`Cargo.toml` entries as
  `git = "https://github.com/portal-co/volar.git"` for what are also local
  path members; it resolves correctly today per `Cargo.lock`, so it's not
  blocking, but it's a fragile pattern worth a separate look if it ever
  breaks a build). Flagging it here only so it's not mistaken for something
  this plan already accounted for.
- **Risk**: the semantic-equivalence harness (Part 1) needs a toolchain
  (`node`, `rustc`, later `solc`/Foundry) available in both local dev and CI;
  Part 1 phase 1a already has to solve this for `tsc` and should solve it
  generally rather than per-backend.
- **Risk, sharpened by the full-coverage decision (§2.3/§2.5)**: covering the
  *entire* spec tree, including full VOLE prover/verifier and FAEST
  sign/verify round trips, is meaningfully more work than a representative
  subset would have been, and touches code paths (transcript/Fiat–Shamir
  recomputation, multi-function protocol composition) that have never been
  exercised end-to-end through the TS backend before. `volar-spec`'s
  deterministic-RNG discipline (`SpecRng`, no `rand` crate — see §2.3 item 3)
  removes the *cross-backend nondeterminism* risk, but doesn't remove the
  plain size-of-effort risk; expect this to be the single largest time sink
  in the whole plan and to surface additional known-gap-shaped findings
  (in the spirit of §1.3) as coverage expands into less-exercised corners of
  the spec tree.
- **Risk**: `LengthDoubler`/associated-type-driven lengths (§3.4) may turn
  out to need real design work, not just plumbing — recommend prototyping
  that specific case early in Part 2 rather than discovering it late.
- **Risk**: contract code-size limits (EIP-170, 24KB) could bite for
  anything beyond small verifier functions; `chunk_fns.rs`'s existence for
  the TS case suggests this is a known-shaped problem, and the shared
  size/instantiation-count budget (§5) is designed as the primary mitigation
  — but the Solidity-specific answer (separate library contracts vs.
  internal function splitting once under EIP-170) still needs its own
  design, not a direct port from `chunk_fns.rs`.
- **Risk**: building both the monomorphized and dyn-lowered Solidity emission
  paths as equally first-class (§4.1, §4.4) roughly doubles `printer_sol.rs`'s
  surface area relative to a monomorphize-only design — worth watching for
  scope creep during implementation; the phased rollout (§4.8) deliberately
  sequences the simpler monomorphized path first specifically to contain
  this.

---

## 8. Decisions record

The original draft's open questions have all been answered during review;
this section is kept as a short index back to where each decision now lives
in the plan, rather than as a live open-questions list.

| # | Question | Decision | Where it's reflected |
|---|---|---|---|
| 1 | Part 1 semantic-harness scope | Full spec-tree coverage, not a narrow permanent subset | §2.3, §2.5, §2.6, §6, §7 |
| 2 | `ring_lwe.rs`'s bare `loop {}` | First-class parser support (`loop` → `WhileLoop{cond: true, ..}`), not a spec rewrite | §1.3.4, §2.4 item 3 |
| 3 | Monomorphize-vs-dyn-lower boundary | Size/instantiation-count budget, explicitly to bound generated code size | §3.3 item 5, §4.4, §5 |
| 4 | Solidity use case | On-chain verification, confirmed — **with dyn-lowered IR (`Vec`-using) handling as an explicit, first-class, tested scope item**, not just a fallback | §4.1, §4.2, §4.4, §4.6, §4.7, §4.8 |
| 5 | Solidity architecture: high-level printer vs. `LirTarget` | Confirmed: high-level printer. Principle promoted to Core Design Rule 15 in `AGENTS.md` | §4.3, [`AGENTS.md`](../AGENTS.md) rule 15 |
| 6 | Sequencing | Part 1 fully first; Parts 2 and 3 may run in parallel afterward where they don't share a dependency (the shared monomorphizer, §5, is the one hard serialization point) | §6 |
| 7 | `TS_BACKEND_PROGRESS.md` disposition | Retire/mark superseded once Part 1 lands | §2.6 |
