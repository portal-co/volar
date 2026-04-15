# Volar Agent Context

## Project Mission

Volar's goal is to increase adoption of **program-related cryptography** — zero-knowledge proofs, garbled circuits, multi-party computation, and the primitives that underpin them — by implementing it in auditable Rust, compiling it to other targets (TypeScript, C), and developing it publicly with rigorous reliability tracking.

The current implementation focus is VOLE-based ZK proofs and garbled circuits. These were chosen because they share a clean common substrate (VOLE correlations, boolean circuits, binary extension-field arithmetic) that the compiler and IR were designed around. Other schemes and protocols will follow as the infrastructure matures.

When deciding what to implement or how to design a component, prefer choices that:
- Are useful to the broadest set of program-related cryptography applications (not just the current VOLEitH construction).
- Keep the IR, compiler, and spec layer general enough to support future protocols.
- Follow the reliability system: new cryptographic constructions start at Experimental, not Normal.

## Current Implementation Architecture

The live cryptographic kernel (`volar-spec`) implements two constructions:

**VOLE-based ZK (Quicksilver-style VOLEitH):**
- Subfield VOLE over GF(2) for bit-level operations, authenticated by an extension field GF(2^128) with a global secret Δ.
- Commitment structure: `Vope<N, T, K>` — a vector of N authenticated values as polynomials of degree K in Δ.
- Bit-slicing via `BitsInBytes`/`BitsInBytes64` for SIMD-style parallelization over GF(2).
- Galois Extension Lifting: mapping bit-commitments into GF(2^k) (like AES's GF(256)) via linear basis transformations to perform field-specific operations (e.g., S-Box inversions).
- Quicksilver-style algebraic AND checks: the product of two VOLE polynomials is verified against a claimed result by ensuring the resulting high-degree polynomial vanishes.

**Garbled circuits:**
- Half-gate scheme (Zahur-Rosulek-Evans 2015) over VOLE wire labels.
- `GlobalSecret<N>`, `Garble<N>`, `Eval<N>`, `GarbleTable<N>` types.
- Weaver generates both garbler and evaluator variants from boolean circuits.

## Compiler IR Genericity Invariant

All code that programmatically constructs `IrModule` (in `volar-compiler`) must use typed `IrExpr`/`IrStmt` nodes — never embed raw Rust strings as expression text. Use:
- `IrExpr::MethodCall` for method calls
- `IrExpr::Binary` for binary operators
- `IrExpr::StructExpr` for struct literals
- `IrExpr::Var` for variable references
- `IrExpr::Call` + `IrExpr::Path` for free function calls with turbofish
- `IrType::Struct { kind: Custom("..."), .. }` for named types

If the printer does not handle a node correctly, **fix the printer** rather than working around it with raw strings. Guardrails in the printer (debug_assert on ident characters, etc.) exist to catch injection; respect them.

## Tests for Generated IR

Tests of generated compiler IR must **lower and compile the output** (real backend: `print_module` → `rustc`), not perform syntactic IR analysis. Do not assert on variable names, statement counts, or IR structure unless the test is specifically verifying a hard-to-change structural invariant. The correctness signal is: the generated code compiles and (where possible) runs correctly.

## Garbler + Evaluator Pairing

Any weaving pass targeting a garbled circuit scheme must generate **both** variants:
- **Evaluator side**: takes precomputed `GarbleTable<N>` inputs and evaluates the circuit using `and_via_table`, free-XOR, etc.
- **Garbler side**: takes `GlobalSecret<N>` and input `Garble<N>` wire labels, produces `GarbleTable<N>` for each AND gate.

These variants must be co-designed so their protocols are compatible (same table order, same wire convention).

## Reliability Tags

Files tagged `// @reliability: experimental` contain unreviewed cryptographic code. New code depending on these must not be deployed without separate review. The `@ai: none` or `@ai: assisted` tags indicate the level of AI involvement in that file.
