# Volar Agent Context

## Core Architecture

A VOLE-based ZK system utilizing Subfield VOLE over GF(2) for bit-level operations, authenticated by an extension field GF(2^128) with a global secret Δ.

**Commitment Structure:** `Vope<N, T, K>` representing a vector of N authenticated values as polynomials of degree K in Δ.

**Methodology:**
1. Bit-slicing: Extensive use of bitwise shuffling and SIMD parallelization over GF(2).
2. Galois Extension Lifting: Mapping bit-commitments into GF(2^k) (like AES's GF(256)) via linear basis transformations to perform field-specific operations (e.g., S-Box inversions).
3. Constraint Logic: Using Quicksilver-style algebraic checks where the product of two polynomials is verified against a claimed result by ensuring the resulting high-degree polynomial vanishes.

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
