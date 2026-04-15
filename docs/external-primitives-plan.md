# External Access Primitives: Oracles, Actions, and Native RNG

**Status**: Planning — awaiting review before implementation.  
**Reliability tier for new code**: Experimental.  
**@ai: assisted**

---

## 1. Motivation

The Volar IR family currently has one mechanism for reaching outside a circuit: `StorageRead`/`StorageWrite` (key-value persistent store, not yet lowered). Three additional access patterns appear throughout program-related cryptography and are not expressible today:

| Primitive | Purity | Side-effects | Parties that evaluate |
|-----------|--------|-------------|----------------------|
| **Oracle** | Pure | None | All parties (deterministic) |
| **Action** | Impure | Yes, conditional | One party (prover / evaluator) |
| **Native RNG** | Effectful | Implicit | One party (fresh randomness) |

These three primitives cover the vast majority of "reach outside the circuit" patterns in ZK, garbled circuits, and MPC without requiring full general-purpose I/O.

---

## 2. Semantic contracts

### 2.1 Oracle

An **oracle** is a named pure external function. All parties evaluate it and agree on its output — identical inputs always produce identical outputs regardless of which party evaluates it or when.

Cryptographic uses:
- **ZK**: the hash function H in a Merkle proof; an AES S-box lookup; any computation whose table is public.
- **Garbled circuits**: inlined as garbled gates or a pre-computed garbled lookup table shared with the evaluator.
- **MPC**: each party locally evaluates (for publicly-agreed functions) or contributes to a shared protocol.

**Key invariant**: the oracle's outputs are fully determined by its arguments. The compiler may memoize, reorder, or CSE oracle calls freely. An oracle may produce **multiple independent outputs** (e.g., a hash-and-derive function that simultaneously produces a key and a tag).

### 2.2 Action

An **action** is a named external function with side effects that is **conditionally executed** based on a boolean guard.

```
(out_0, out_1, …) = if guard {
    action(args…)
} else {
    (fallback_0, fallback_1, …)
}
```

Side-effect semantics:
- `guard = 0` → action is **not invoked**; fallback values are used; no side effect occurs.
- `guard = 1` → action **is invoked** with `args`; its real outputs are used; the side effect occurs.

The circuit always produces well-defined outputs regardless of `guard`. Multi-output is first-class: a single `ActionCall` carries all outputs and their fallbacks atomically.

Cryptographic uses:
- **ZK**: the prover performs the action when `guard = 1` and provides the real outputs as witness values; the verifier checks consistency. When `guard = 0`, the fallbacks are authenticated instead and no action occurs.
- **Garbled circuits**: the evaluator decrypts the guard wire. If it is 1, the action is performed on the decrypted inputs; if 0, the fallbacks are used.
- **MPC**: each party participates in a conditional OT-based evaluation.

**Key invariant**: the action's side effect is observable in the real world. The IR records *what would be called*, *under what guard*, *with what fallbacks*, but does not specify the implementation. A single `ActionCall` represents one logical invocation — every `ActionOutput` that projects from it is part of that same invocation.

### 2.3 Native RNG

**Native RNG** generates a fresh random value of a specified type. Unlike `Const`, the value is not known at compile time. Unlike an oracle, it has no inputs.

- **ZK**: part of the prover's private randomness tape; VOLE-authenticated.
- **Garbled circuits**: the garbler picks fresh random wire labels at garbling time.
- **MPC**: execution-environment-defined; the IR records the intent.

**Key invariant**: each `Rng` stmt occurrence is an independent sample. Compilers must not deduplicate, CSE, or reorder `Rng` stmts. No domain tag is included — entropy pool routing would complicate execution environments beyond what is needed.

---

## 3. Concrete data-structure changes

### 3.1 Five new `Stmt` variants in `volar-ir-common`

Multi-output is handled with the **call + projection** pattern, the standard SSA approach for multi-result operations. Each `*Call` produces a typed aggregate result (a `TypeId` pointing to `IrType::Tuple(output_tys)`); each `*Output` projects one field from it.

```rust
pub enum Stmt<Var, Addr = Var> {
    // ... existing variants unchanged ...

    // ---- Oracle ----

    /// Invoke a named pure oracle (deterministic, no side effects).
    ///
    /// Produces an aggregate result of type `IrType::Tuple(output_tys)`.
    /// Project individual outputs with `OracleOutput`.
    ///
    /// The oracle must appear in the enclosing module's oracle table.
    /// All parties evaluate it; its outputs are fully determined by `args`.
    OracleCall {
        name: alloc::string::String,
        args: alloc::vec::Vec<Var>,
        /// Return types, one per output, in declaration order. Non-empty.
        output_tys: alloc::vec::Vec<TypeId>,
        /// Pre-interned TypeId of IrType::Tuple(output_tys).
        /// Stored at construction time so type inference never mutates the table.
        result_ty: TypeId,
    },

    /// Project output `idx` from an `OracleCall` result.
    ///
    /// `call` must be the SSA var of an `OracleCall` in the same block.
    /// `ty` must equal `oracle_call.output_tys[idx]`.
    OracleOutput {
        call: Var,
        idx: usize,
        ty: TypeId,
    },

    // ---- Action ----

    /// Conditionally invoke a named impure action.
    ///
    /// Produces an aggregate result of type `IrType::Tuple(output_tys)`.
    /// Output `i` equals `action(args)[i]` when `guard = 1`, `fallbacks[i]`
    /// when `guard = 0`. Project with `ActionOutput`.
    ///
    /// `fallbacks[i]` must have type `output_tys[i]`.
    /// One `ActionCall` stmt = one logical invocation.
    ActionCall {
        name: alloc::string::String,
        guard: Var,
        args: alloc::vec::Vec<Var>,
        /// Fallback values — one var per output (typed as `output_tys[i]`).
        fallbacks: alloc::vec::Vec<Var>,
        /// Return types, one per output, in declaration order. Non-empty.
        output_tys: alloc::vec::Vec<TypeId>,
        /// Pre-interned TypeId of IrType::Tuple(output_tys).
        result_ty: TypeId,
    },

    /// Project output `idx` from an `ActionCall` result.
    ///
    /// `call` must be the SSA var of an `ActionCall` in the same block.
    /// `ty` must equal `action_call.output_tys[idx]`.
    ActionOutput {
        call: Var,
        idx: usize,
        ty: TypeId,
    },

    // ---- RNG ----

    /// Produce a fresh random value drawn uniformly from the type's domain.
    ///
    /// Each `Rng` occurrence is an independent sample.
    /// Optimisers must NOT deduplicate, CSE, or reorder `Rng` stmts.
    Rng {
        ty: TypeId,
    },
}
```

**Why call + projection?**  
The previous draft correlated multi-output action stmts by name within a block. This breaks after any pass that reorders, renames, or splits stmts — including movfuscation, CSE, and the planned lowering passes. The `call: Var` field is a stable SSA variable ID that survives all passes.

**Why `result_ty` stored in `*Call` stmts?**  
`infer_stmt_result_type` takes a shared `&[IRType]` and must not mutate the type table. Interning `IrType::Tuple(output_tys)` lazily would require `&mut IRTypes`. Storing the pre-interned `TypeId` at construction time keeps inference read-only. The cost is one extra `TypeId` field per `*Call` stmt, which is acceptable.

### 3.2 Oracle and action tables in Volar IR

Volar IR (`IRBlocks`) is currently a tuple-struct `IRBlocks(pub Vec<IRBlock>)`. To support oracle/action declarations, it is expanded to a named-field struct:

```rust
pub struct IRBlocks {
    /// Declared oracles available in this circuit.
    pub oracles: Vec<OracleDecl>,
    /// Declared actions available in this circuit.
    pub actions: Vec<ActionDecl>,
    /// The blocks of the circuit, in order (block 0 is the entry).
    pub blocks: Vec<IRBlock>,
}
```

`OracleDecl` and `ActionDecl` are defined in `volar-ir-common` (alongside `Stmt`) so they can be shared:

```rust
/// Declaration of a named pure oracle.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct OracleDecl {
    pub name: alloc::string::String,
    pub params: alloc::vec::Vec<TypeId>,
    /// Return types, one per output (length ≥ 1).
    pub results: alloc::vec::Vec<TypeId>,
}

/// Declaration of a named conditional action.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ActionDecl {
    pub name: alloc::string::String,
    pub params: alloc::vec::Vec<TypeId>,
    /// Return types, one per output (length ≥ 1).
    pub results: alloc::vec::Vec<TypeId>,
}
```

**`IRBlocks` is a breaking change**: all construction sites (`IRBlocks(vec![...])`) and field accesses (`.0`) must be updated. The `is_movfuscated()` / `is_circuit()` helpers move to methods on the new struct and reference `self.blocks` instead of `self.0`.

**`VolarIrTarget::completed`**: currently `Vec<(String, IRBlocks)>`. After the change, the individual `IRBlocks` values carry their own oracle/action tables. `VolarIrTarget` accumulates oracle/action declarations (registered via new methods `register_oracle` / `register_action`) and writes them into each completed `IRBlocks`.

### 3.3 Oracle and action tables in VAFFLE

`vaffle::Module` adds the same declaration tables, sharing `OracleDecl`/`ActionDecl` from `volar-ir-common`:

```rust
pub struct Module {
    pub types: TypeTable,
    pub oracles: Vec<OracleDecl>,   // NEW
    pub actions: Vec<ActionDecl>,   // NEW
    pub funcs: Vec<FuncDecl>,
    pub sigs: Vec<SigDecl>,
    pub exports: BTreeMap<String, FuncId>,
}
```

Sharing the declaration types from `volar-ir-common` ensures VAFFLE and Volar IR cannot describe an oracle differently.

---

## 4. Type-inference rules (for movfuscation / `infer_stmt_result_type`)

| Stmt variant | Result `TypeId` |
|---|---|
| `OracleCall { result_ty, .. }` | `result_ty` (pre-interned; no table mutation) |
| `OracleOutput { ty, .. }` | `ty` |
| `ActionCall { result_ty, .. }` | `result_ty` |
| `ActionOutput { ty, .. }` | `ty` |
| `Rng { ty }` | `ty` |

For `subst_ir` (variable substitution):

| Stmt variant | Variables to remap |
|---|---|
| `OracleCall { args, .. }` | Each arg. Name, `output_tys`, `result_ty` invariant. |
| `OracleOutput { call, .. }` | `call`. `idx` and `ty` invariant. |
| `ActionCall { guard, args, fallbacks, .. }` | `guard`, each arg, each fallback. |
| `ActionOutput { call, .. }` | `call`. `idx` and `ty` invariant. |
| `Rng { .. }` | Nothing. |

**Movfuscation note**: `OracleCall`/`ActionCall` produce tuple-typed vars. The movfuscation dispatch loop (gate/field-add accumulation) must skip tuple-typed vars — only scalar-typed `*Output` results participate in dispatch.

---

## 5. `LirTarget` trait additions

Three new builder methods. `oracle` and `action` accept a slice of return types and return a flat concatenation of all outputs' scalar representations — extending the `call_extern` convention to multiple outputs.

```rust
pub trait LirTarget {
    // ... existing methods unchanged ...

    /// Invoke a named pure oracle, returning all outputs as a flat scalar list.
    ///
    /// `ret_tys` holds the LirType of each output (length ≥ 1).
    /// The return is the concatenation of `flatten(output_i)` for each `i`,
    /// in order. Callers split using `flatten_count(&ret_tys[i])`.
    ///
    /// The oracle must be declared in the module's oracle table before any
    /// block that contains an `OracleCall` for this name.
    fn oracle(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[Self::Value],
        ret_tys: &[LirType],
    ) -> Vec<Self::Value>;

    /// Invoke a named conditional action, returning all outputs as a flat
    /// scalar list.
    ///
    /// `guard` must be `LirType::Bool`.
    /// `fallbacks` is the flat concatenation of all outputs' fallback scalars,
    /// in the same order and layout as the return value.
    /// `ret_tys` holds the LirType of each output (length ≥ 1).
    ///
    /// The action must be declared in the module's action table.
    fn action(
        &mut self,
        name: &str,
        guard: Self::Value,
        arg_tys: &[LirType],
        args: &[Self::Value],
        fallbacks: &[Self::Value],
        ret_tys: &[LirType],
    ) -> Vec<Self::Value>;

    /// Generate a fresh random value of `ty`.
    ///
    /// Each call is an independent sample; implementations must not alias results.
    fn rng(&mut self, ty: LirType) -> Self::Value;
}
```

**Default implementations are not provided** — backends must make a deliberate choice.

---

## 6. `VolarIrTarget` implementation

### 6.1 `oracle`

```
oracle(name, arg_tys, args, ret_tys)
  → emit OracleCall {
        name, args: flat_vars,
        output_tys: [intern(t) for t in ret_tys],
        result_ty: intern(IrType::Tuple(output_tys)),
    } → call_var

  → for each i in 0..ret_tys.len():
        emit OracleOutput { call: call_var, idx: i, ty: intern(ret_tys[i]) }
          → out_i
        append VolarValue { bits: [out_i], ty: ret_tys[i] } to result

  → return flat concat
```

### 6.2 `action`

```
action(name, guard, arg_tys, args, fallbacks, ret_tys)
  → split fallbacks into per-output groups by flatten_count(ret_tys[i])
  → each single-scalar group is used directly as fallback_var_i;
    multi-scalar groups must be pre-assembled by the caller as a single SSA var

  → emit ActionCall {
        name, guard: guard.bits[0],
        args: flat_arg_vars,
        fallbacks: [fallback_var_0, …],
        output_tys: [intern(t) for t in ret_tys],
        result_ty: intern(IrType::Tuple(output_tys)),
    } → call_var

  → for each i in 0..ret_tys.len():
        emit ActionOutput { call: call_var, idx: i, ty: intern(ret_tys[i]) }
        append VolarValue { bits: [out_i], ty: ret_tys[i] } to result

  → return flat concat
```

### 6.3 `rng`

```
rng(ty)
  → emit Rng { ty: intern(ty) }
  → return VolarValue { bits: [new_var], ty }
```

### 6.4 Declaration registration

`VolarIrTarget` gains two new methods:

```rust
impl VolarIrTarget {
    /// Register an oracle declaration. Must be called before any block
    /// that contains an OracleCall for this name.
    pub fn register_oracle(&mut self, decl: OracleDecl) { … }

    /// Register an action declaration. Must be called before any block
    /// that contains an ActionCall for this name.
    pub fn register_action(&mut self, decl: ActionDecl) { … }
}
```

Registered declarations are carried into the `IRBlocks::oracles` / `IRBlocks::actions` fields of every completed function.

### 6.5 `subst_stmt` in `volar-ir-lir-target`

Five new match arms, each remapping all `Var` fields, leaving names, indices, and type IDs unchanged.

---

## 7. Lowering passes

### 7.1 `movfuscate.rs`

**`subst_ir`**: five new arms per the §4 table.

**`infer_stmt_result_type`**: five new arms per the §4 table. `*Call` arms return `result_ty` directly (no type-table mutation needed).

**`emit_block_stmts`**: tuple-typed `*Call` result vars must not enter the gate/field-add dispatch accumulator. Add a guard: skip vars whose type is `IrType::Tuple(_)`. Only scalar-typed `*Output` vars participate.

### 7.2 `lower_lir.rs` — `lower_ir_stmt`

Five `unimplemented!` stubs. Full lowering requires a **deferred-value strategy**: when a `lower_ir_stmt` call encounters an `OracleCall`, it emits the LirTarget `oracle(…)` call immediately and stashes the flat output list in a side-table keyed by the `OracleCall`'s var ID. Subsequent `OracleOutput { call, idx }` stmts look up the stash and return the appropriate scalar slice — they do not emit any LirTarget call. `ActionCall`/`ActionOutput` follow the same pattern.

This two-pass structure is documented as a comment on the stubs so implementors have the full context.

### 7.3 `volar-lir-codegen` — compiler-level lowering

Oracle, action, and RNG calls are recognised via **`#[oracle]`, `#[action]`, and `#[rng]` attributes** on function declarations, parseable by `volar-compiler`'s existing `syn`-based parser. These attributes are part of the volar-compiler infrastructure and may also be attached by external proc-macros, allowing users to annotate library functions without modifying Volar's source.

Name-convention recognition (Phase 1 in the previous draft) is **dropped entirely**. It cannot handle the attribute-based representation natively: a function named `oracle_sha256` has no attribute and should not be treated as an oracle; a function named `compute_key` with `#[oracle]` should be. Convention-only recognition would produce both false positives and false negatives. The attribute is the unambiguous source of truth.

The parser changes needed:
- Recognise `#[oracle]`, `#[action]`, `#[rng]` on `ItemFn` nodes during `convert_function`.
- Add `is_oracle: bool`, `is_action: bool`, `is_rng: bool` fields to `IrFunction` (or a new `ExternalKind` enum).
- In `lower_function_with_registry`, check the kind and dispatch to `ctx.target.oracle(…)` / `.action(…)` / `.rng(…)` instead of the default body lowering.

---

## 8. Backend handling

### 8.1 C backend

The C backend carries a configurable RNG function name rather than a fixed convention. This allows embedders to plug in their own CSPRNG without patching generated code:

```rust
pub struct CBackend {
    // ... existing fields ...
    /// Name of the C function to call for `Rng` stmts.
    /// Signature expected: `void rng_fn(void *out, size_t len)`.
    /// Default: `"volar_rng"`.
    pub rng_fn: String,
}

impl CBackend {
    pub fn with_rng_fn(mut self, name: impl Into<String>) -> Self {
        self.rng_fn = name.into();
        self
    }
}
```

Emission for each primitive:

**Oracle** (multi-output): declare a C struct for the oracle's result type (once per oracle name), then emit a call:
```c
/* oracle: pure */
struct OracleName_Result r = oracle_<name>(args…);
```
Each `OracleOutput { idx }` becomes `r.field_<idx>`.

**Action** (multi-output):
```c
struct ActionName_Result r = { fallback_0, fallback_1, … };
if (guard) { r = action_<name>(args…); }
```
Each `ActionOutput { idx }` becomes `r.field_<idx>`.

**Rng**:
```c
T out;
<rng_fn>(&out, sizeof(T));
```

### 8.2 Future ZK and GC backends

The IR design deliberately preserves the information each backend needs:

- **ZK prover**: oracle outputs are evaluated locally (all parties agree); action outputs are witness inputs when `guard = 1`, fallbacks when `guard = 0`; rng outputs come from the randomness tape.
- **ZK verifier**: oracle outputs are verified for circuit consistency; action outputs are VOLE-authenticated; rng outputs are VOLE-authenticated witness values.
- **Garbler**: oracles become shared lookup tables or inlined gates; action calls generate conditional garbled gates using the guard wire; rng produces fresh wire labels.
- **Evaluator**: oracles are evaluated locally; actions are conditionally called based on the decrypted guard wire; rng is not needed (the garbler provides wire labels).

---

## 9. Relationship to existing `StorageRead`/`StorageWrite`

| Primitive | State | Conditionality | Multi-output |
|-----------|-------|----------------|--------------|
| `StorageRead` | Shared persistent | Unconditional | No |
| `StorageWrite` | Shared persistent | Unconditional | No (unit result) |
| `OracleCall` | None (pure) | N/A | Yes |
| `ActionCall` | External (opaque) | Guard-conditional | Yes |
| `Rng` | Entropy source | N/A | No |

---

## 10. Sequencing and ordering constraints

| Stmt | Reorderable? | CSE-able? | DCE-able? |
|------|-------------|-----------|-----------|
| `OracleCall` | Yes (pure) | Yes (same name+args) | Only if all `OracleOutput`s are also DCE'd |
| `OracleOutput` | With its `OracleCall` | N/A | Yes, if result unused |
| `ActionCall` | No (side effect) | No | No (guard may be 1) |
| `ActionOutput` | With its `ActionCall` | N/A | No (drives `ActionCall` liveness) |
| `Rng` | No (independence) | No | Yes, if output unused |

---

## 11. Open questions (all resolved)

**Q2 — Oracle/action declarations in Volar IR** → **Use tables.** `IRBlocks` is expanded from a tuple-struct to a named-field struct carrying `oracles: Vec<OracleDecl>` and `actions: Vec<ActionDecl>` alongside `blocks: Vec<IRBlock>`. Declaration types (`OracleDecl`, `ActionDecl`) live in `volar-ir-common` so they are shared between Volar IR and VAFFLE.

**Q3 — RNG domain tag** → **Single variant, no tag.** `Rng { ty: TypeId }` stays as-is. Entropy pool routing would complicate execution environments beyond what is needed at this stage.

**Q5 — Compiler syntax** → **`#[oracle]` / `#[action]` / `#[rng]` attributes.** These are standard attributes recognised by `volar-compiler`'s parser and may also be provided by external proc-macros, allowing users to annotate library functions. Name-convention recognition (Phase 1 from the previous draft) is dropped — it cannot correctly handle the attribute-based representation and would produce false positives and false negatives.

**Q6 — RNG function name in C backend** → **Configurable field.** `CBackend::rng_fn: String` defaults to `"volar_rng"` and is overridden via `CBackend::with_rng_fn(name)`. The expected C signature is `void rng_fn(void *out, size_t len)`, giving embedders full control of entropy sourcing.

**Q7 — `result_ty` field trade-off** → **Accepted.** Storing the pre-interned tuple `TypeId` directly in `*Call` stmts is the right trade-off: inference stays read-only, the field is small, and the alternative (threading `&mut IRTypes` through inference) imposes a larger structural cost on every pass.

---

## 12. Proposed implementation order

1. **`OracleDecl` / `ActionDecl` in `volar-ir-common`** — Add the two declaration structs. They are depended on by both `volar-ir` and `vaffle`, so they must land first.

2. **`Stmt` variants** — Add `OracleCall`, `OracleOutput`, `ActionCall`, `ActionOutput`, `Rng` to `volar-ir-common`. Update all match sites (`subst_ir`, `infer_stmt_result_type`, `subst_stmt`) with correct handling or documented `unimplemented!` stubs. All existing tests must pass.

3. **`IRBlocks` structural change** — Expand `IRBlocks` from a tuple-struct to a named-field struct with `oracles`, `actions`, `blocks`. Update all construction sites and field accesses. Update `is_movfuscated()`, `is_circuit()`. Update `VolarIrTarget` to populate the tables from registered declarations.

4. **`LirTarget` trait** — Add `oracle`, `action`, `rng` to `volar-lir`. No default implementations.

5. **`VolarIrTarget`** — Add `register_oracle`/`register_action` methods. Implement `oracle`, `action`, `rng` in `volar-ir-lir-target`. Test: a two-output oracle produces two distinct `OracleOutput` vars; an `ActionCall` with `guard = 0` uses fallbacks; `Rng` vars are all distinct.

6. **VAFFLE declarations** — Add `oracles` and `actions` to `vaffle::Module`.

7. **C backend** — Add `rng_fn` field to `CBackend`. Implement `oracle`, `action`, `rng`. Generate per-call C structs for multi-output results. Add a compile-and-link test with trivial stub implementations.

8. **Lowering stubs** — Add `unimplemented!` stubs in `lower_ir_stmt` with comments on the deferred-value two-pass strategy.

9. **Parser / compiler attributes** — Recognise `#[oracle]`, `#[action]`, `#[rng]` in `convert_function`; add `ExternalKind` field to `IrFunction`; dispatch to `ctx.target.oracle(…)` / `.action(…)` / `.rng(…)` in `lower_function_with_registry`.
