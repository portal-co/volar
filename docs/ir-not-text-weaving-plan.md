# Plan: weavers emit IR, not text — rule, Rust-side lint, and migration

**Status:** plan + research record. It authorizes no reclassification; CI
enforcement defaults are flagged as human decisions in §8. **@ai:** assisted.

**Scope:** a third (rule + lint + migration) triple in the family of
[`fhe/vec-elimination-and-linter-plan.md`](fhe/vec-elimination-and-linter-plan.md):
(1) an AGENTS.md rule that **weavers emit the entire woven program as IR**
(`IrModule` / `IrCfgModule`) and never as Rust source text destined for
`rustc`; (2) a Rust-side lint enforcing it on fully-featured Rust code;
(3) a migration of the remaining text-path production callers to
IR→LIR compilation.

**Related records:** [direct-to-LIR weaver fast-path plan](direct-to-lir-weaver-fast-path-plan.md) (the enabling compilation track — this plan is its policy/enforcement arm) · [pipeline.md](pipeline.md) · [lir-native-loops plan](lir-native-loops-plan.md) · [Vec linter plan](fhe/vec-elimination-and-linter-plan.md) (sibling triple; exemption-marker precedent) · [reliability.md](reliability.md)

---

## 1. Why

The weavers already *produce* IR; the failure is in the **consumption
contract**: production pipelines print the woven module to Rust **text** and
compile that text with `rustc`. `rustc` is the measured wall at every scale
this project has hit:

| Evidence | Source |
|---|---|
| `chunk_size=8` prints a **44.6 MB** accumulator-chunk function; `rustc` OOMs after **~28 min** | `direct-to-lir-weaver-fast-path-plan.md` |
| A `[StepMemOp; ~68000]×N` literal (single ~52 MB line) hung `rustc`'s own `UnusedDelimLint` for **45+ min** — a distinct bottleneck class from memory blowup | `volar-riscv-e2e/src/wat_gen.rs:1600` |
| `print_weaved_vole_module` reached **13 GB+ RSS** merely *printing* the module | `volar-weaver/src/vole.rs:2445` |
| Unrolled TFHE spec slice: **283 MB** of generated Rust → **15 MB** through the LIR-native loop path (**18.9×**) | `lir-native-loops-plan.md` |

The IR→LIR path (`lower_module` / `lower_cfg_module` → `LirTarget` → C99 /
WASM / object via `volar-build`) emits text whose consumers are `cc`,
`wasmtime`, or LLVM — none of which exhibit `rustc`'s source-size
pathologies. Routing woven programs through it removes this entire blocker
class **and** skips `rustc`'s full frontend→MIR→LLVM pipeline for generated
code, which is the speed win stated in the rule.

The rule is also an *architecture* invariant, per `pipeline.md`: `IrModule`
is the lingua franca; every stage reads and writes program structure as IR.
Text is a *rendering of* IR for a backend's consumer, never the woven
program itself.

## 2. Research: where text is produced today

### 2.1 Inventory (as of this plan)

Weaver text emitters (`volar-weaver/src/`):

| Function | Output | Role today |
|---|---|---|
| `print_fhe_flat_module`, `print_fhe_cfg_module` (fhe.rs) | Rust text | Test compile-checks only (in-tree) |
| `print_weaved_module` (garble.rs) | Rust text | Test compile-checks |
| `print_weaved_vole_module` (vole.rs) | Rust text | **Production**: `volar-riscv-e2e/src/wat_gen.rs` (~10 call sites) prints prover/verifier/qsim to Rust text for `rustc` |
| `print_weaved_faest_module` (faest.rs) | Rust text | Test compile-checks |
| `print_net_vole_module`, `print_net_vole_cfg_module`, `print_hybrid_net_cfg_module`, `print_glue_module` | Rust text | Test compile-checks |
| `print_fhe_cfg_module_ts` | TS text | TS target (deferred; out of scope, §6.3) |
| `print_fhe_cfg_module_c` | C99 text | Already LIR-derived (`volar-lir-codegen` mono env) — **compliant** |

Production offenders (rule violations under §3): the `wat_gen.rs` sites
above. Everything else in-tree is test-only today, which is exactly why a
**lint** is needed now: the rule's value is preventing *new* text-path
production code, not just migrating the known one.

### 2.2 What "emit IR for the entire woven program" means

A weaver's deliverable for a host-language (Rust) consumer is the complete
`IrModule`/`IrCfgModule` — the whole woven program as typed IR, including
every function it wove — so the caller can `lower_module` /
`lower_cfg_module` directly to any `LirTarget` without a text round-trip.
"Entire" matters: partial-program text emission (print prover here, verifier
there) splits the module into fragments that each need separate `rustc`
invocations and lose cross-function inlining/monomorphization context the
LIR lowering uses. `volar-build::compile_lir_to_object` (SavedLirModule →
object code in `build.rs`) is the native endpoint that replaces
"print Rust → temp crate → `cargo build`".

### 2.3 What is *not* a violation

- **Target text from a backend**: C99 / WASM text / object code emitted by
  an `LirTarget`, and the TypeScript printer (a target language, not a
  `rustc` feeder). These render IR for a *non-rustc* consumer.
- **Diagnostics**: `dump_ir`, debug prints, error messages, logging.
- **Test fixtures**: compile-check harnesses that generate a temp crate to
  prove emitted code compiles — they run `rustc` on small fixtures as a
  *test oracle*, which is legitimate and already the repo's established
  pattern (`run_compile_check`). They carry the test-fixture exemption.
- **`volar-spec-dyn` / `volar-runtime` generation**: `volar-codegen`
  printing the *spec* (not a woven program) to Rust/TS text as a checked-in
  artifact. Different artifact class; explicitly out of this rule's scope
  but noted so the lint does not flag it.

## 3. The rule (AGENTS.md text — §8 lands it)

> **Weavers emit IR, not text.** A weaver's deliverable for a Rust consumer
> is the complete woven program as typed IR (`IrModule`/`IrCfgModule`).
> Producing Rust *source text* of a woven program to be compiled by `rustc`
> is forbidden in production code: route the IR through
> `lower_module`/`lower_cfg_module` to an `LirTarget` (C99, WASM, or object
> code via `volar-build`) instead. Rust text printing of woven modules is
> allowed only with a `/// @volar-allow-rust-text: <category>: <reason>`
> doc comment, categories: `test-fixture`, `diagnostic`, `ts-target`,
> `migration-in-progress`. Backend text for non-`rustc` consumers (C99,
> WASM, TypeScript) is not a violation. The `weave_text_lint` source lint
> errors on violations in the compiler workspace.

## 4. Research: enforcing custom lints on fully-featured Rust

Evaluated against: full-Rust-code reachability (must see call paths, not
just imports), exemption mechanism, toolchain/dependency cost, and repo
precedent (`vec_lint` — in-tree `syn` walk, no new infra).

| Mechanism | How it would enforce the rule | Verdict |
|---|---|---|
| **`syn`-based source lint** (test + codegen gate) | Walk `syn::File` per compiler-workspace `.rs`: flag (a) calls to the enumerated weaver text emitters outside `#[cfg(test)]`/exempt files, (b) `weave_*` fns returning `String`, (c) `Command::new("cargo"/"rustc")` adjacent to printed-output writes. Exemptions via `/// @volar-allow-rust-text:` doc attrs — the exact `vec_lint`/`@volar-allow-vec` mechanism. | **Primary.** In-repo precedent, no new deps, sees the whole file incl. return types and attributes; deterministic `path:line: rule: message` output. |
| **Clippy deny-lists** (`clippy.toml`) | `disallowed-methods = [{ path = "volar_weaver::fhe::print_fhe_flat_module", reason = "..." }, ...]` — resolution-aware, catches method/function paths clippy can resolve; `disallowed_macros` for any text-macro forms. Set `deny` at workspace `[lints.clippy]`; CI `cargo clippy --all-targets -- -D warnings`. | **Secondary layer.** Cheap, native, but cannot express "except in tests" per-file, cannot see `-> String` fn shapes, and `#[allow]` is an easy escape — it backs up, never replaces, the `syn` lint. (`forbid` rejected: blocks the legitimate test-fixture `#[allow]` sites.) |
| **Dylint** (Trail of Bits) | A custom lint crate could do everything, with full type resolution. | **Rejected for now.** Lints build against a pinned toolchain and must be rebuilt on rustc changes; the repo already pays one toolchain-coupling cost (`llvm-sys`). Revisit only if enforcement must span multiple repos. |
| **`rustc_private` driver** | Custom `rustc_driver::Callbacks` + `declare_lint!`. | **Rejected.** No API stability, nightly + `rustc-dev`/`llvm-tools` components, highest maintenance; buys nothing over the `syn` lint for this rule. |
| **Semgrep** | YAML pattern rules (`languages: [rust]`) for the banned call shapes. | **Rejected.** New external tool dependency; weaker doc-attr exemption story; the `syn` lint covers the same patterns in-repo. |
| **`arch-lint`** | TOML/Rust-trait rules, `check!()` in tests. | **Rejected.** New dependency (the Vec plan's constraint — no unmodeled deps — applies); its value is marginal over the in-tree `syn` walk. |
| **cargo-deny** | — | **Not applicable** (dependency policy only; no arbitrary source rules). |

**Selection:** the `syn` source lint as the real gate, run as a
`volar-weaver` test (scans the compiler workspace) and re-runnable from CI;
clippy deny-lists as a zero-maintenance CI backstop for the enumerated
emitter paths. Both reuse existing in-repo infrastructure and precedent.

## 5. The lint design (`weave_text_lint`)

### 5.1 Detection rules

| ID | Pattern | Where flagged |
|---|---|---|
| W1 | Call (path or method) to an enumerated weaver text emitter (`print_fhe_flat_module`, `print_fhe_cfg_module`, `print_weaved_module`, `print_weaved_vole_module`, `print_weaved_faest_module`, `print_net_vole_*`, `print_hybrid_net_cfg_module`, `print_glue_module`) in non-test production code | call site |
| W2 | A function named `weave_*` / `weave_*_split*` whose return type is `String` (a weaver *producing* text) | fn signature |
| W3 | `std::process::Command::new("cargo")` / `("rustc")` in non-test code whose enclosing function also writes a `.rs` file (the print→rustc pipeline shape) | call site (best-effort heuristic; W1 is the primary signal) |
| W4 | A `let _code = print_*…` (or similarly named binding) whose value flows to a `Command` spawn — same shape as W3, data-flow form (optional, may fold into W3) | — |

W1's emitter list is maintained as a constant in the lint with a test that
asserts it stays in sync with the weaver's exported `print_*` surface
(guard against drift).

### 5.2 Exemptions

Doc-attr scan identical to `vec_lint`'s:

```text
/// @volar-allow-rust-text: <test-fixture | diagnostic | ts-target | migration-in-progress>: <one-line reason>
```

Scopes: item-level (fn/struct) and file-level `//!` form (for the
compile-check harness module and `wat_gen` during migration). Unrecognized
categories fail closed. `#[cfg(test)]` modules are skipped entirely.

### 5.3 Run points

1. **`volar-weaver` test** (`tests/weave_text_lint.rs`): walks
   `crates/compiler/**` + `crates/examples/**` `.rs` files; empty violation
   list required. Deterministic `path:line: id: message` output; the failure
   message names the exemption marker.
2. **CI**: the same test is in the default `cargo test` set; clippy layer
   via workspace `[lints.clippy]` + existing `ci.yml` clippy invocation.

### 5.4 Non-goals

- No linting of `volar-spec-dyn`/`volar-runtime` generation paths (§2.3).
- No data-flow tracking beyond W3's heuristic (W1 catches the causal site).
- No enforcement in `volar-ir` (sibling repo; the rule is this repo's).

## 6. Migration

Ordered; each step keeps `cargo test -p volar-weaver` green.

### 6.1 M-A — inventory, marker, and exemptions

- Land the AGENTS.md rule (§3) and the lint (§5).
- Exempt current legitimate uses: weaver-internal compile-check tests
  (`test-fixture` — they are `#[cfg(test)]`, auto-skipped);
  `tests_common::run_compile_check*` harness (`test-fixture` file-level);
  TS printer (`ts-target`); `wat_gen.rs` temporarily
  (`migration-in-progress`, with a tracking note to §6.2).
- Lint gate: violations == 0 with exemptions present; a seeded violation
  fails (mirror the Vec plan's seeded test).

### 6.2 M-B — `wat_gen.rs` to IR→LIR (the only production offender)

Replace `print_weaved_vole_module` + rustc with LIR compilation of the
woven `IrModule`/`IrCfgModule`, role by role (prover, verifier, qsim), via
`lower_cfg_module*` + `LirTarget` → object/`volar-build`. **Recorded
dependency:** the direct-to-LIR plan's Phase-2 spec-to-LIR gap inventory
must cover the VOLE module set (its component matrix tracks greenness;
`lir-native-loops-plan.md` already shows the 18.9× size win on the loops
path). Migrate each role as its components go green in that plan; a role
that cannot lower yet keeps `migration-in-progress` with a dated note —
never a silent permanent exemption. Acceptance per role: the LIR-compiled
artifact passes the same e2e test the rustc-compiled one did, and the
`print_weaved_vole_module` call is deleted.

### 6.3 M-C — emitter relegation

Once no production caller remains: mark the `print_*` emitters
`#[doc(hidden)]` with a doc note ("diagnostic/test rendering only;
production output is IR — see AGENTS.md rule"), or rename to
`print_*_debug`. TS printer unchanged (separate target; TS backend work is
deferred per the fast-path plan's scope note). Remove the emitters from
`lib.rs`'s prelude re-exports.

### 6.4 M-D — enforcement on

- Remove `migration-in-progress` exemptions as M-B completes; the lint is
  then the standing gate.
- CI: workspace `[lints.clippy]` deny-lists for the enumerated emitter
  paths; the `syn` lint test already runs in `cargo test`.

## 7. Milestones

| # | Milestone | Gate |
|---|---|---|
| T1 | AGENTS.md rule + `weave_text_lint` (`syn` walk, exemptions, tests) + this doc | lint tests green; seeded violation fails; current tree clean with exemptions |
| T2 | clippy deny-list layer + CI wiring | `cargo clippy -- -D warnings` flags a seeded call; CI green on the tree |
| T3 | M-A exemptions landed (test-fixture/ts-target/migration) | violations == 0; every exemption has a category + reason |
| T4 | M-B `wat_gen.rs` prover+verifier+qsim on LIR (sequenced by the direct-to-LIR plan's component matrix) | per-role: same e2e green on LIR artifact; print calls deleted |
| T5 | M-C emitter relegation + M-D enforcement-on | no production caller of a text emitter remains; lint gate is the standing check |

T4 is the only milestone with an external dependency (Phase-2 spec-to-LIR
coverage); T1–T3 and T5's lint arm are independently landable.

## 8. Open decisions (human)

1. **CI default-on** for the lint (T2/T6): making a violation fail CI is a
   build-behavior decision, same gating as the Vec linter's codegen gate.
2. **Emitter relegation severity** (M-C): `#[doc(hidden)]` vs rename vs
   delete-after-migration. Recommendation: `#[doc(hidden)]` + doc note
   (keeps the test oracle; deletion is allowed only after every consumer
   is migrated, including out-of-tree ones the maintainer knows about).
3. **`volar-spec-dyn` scope** (§2.3): confirming spec-artifact generation
   stays outside the rule is a policy confirmation, not a code question.

## 9. Risks

- **W3/W4 heuristics false-negative** a print→rustc pipeline that hides
  the spawn — mitigated by W1 being the primary signal (the pipeline must
  call an emitter to exist) and by review of exemptions.
- **Exemption abuse** — mitigated by required categories, the
  `migration-in-progress` dated-note convention, and M-D's removal sweep.
- **Premature migration pressure** while spec-to-LIR has gaps — the
  `migration-in-progress` exemption exists precisely so T4 can sequence
  behind the direct-to-LIR plan instead of blocking on it.
