# Volar Compiler IR Documentation

This document describes the intermediate representation (IR) used by the `volar-compiler`. The IR is designed to be **total** (provably terminating) and specialized for VOLE (Vector OLE) and cryptographic protocol specifications.

## Current Status & Features

- [x] **Unified Specialized IR**: Single-pass conversion from Rust/`syn` AST to specialized constructs.
- [x] **Totality Enforcement**: Explicitly rejects `while` and infinite `loop` constructs.
- [x] **Bounded Loops**: First-class support for `for` loops over ranges and array operations.
- [x] **Specialized Array Ops**: High-level representations for `map`, `zip`, `fold`, and `generate`.
- [x] **Type Safety**: Rich enum-based classification for traits, methods, and structures.
- [x] **Rust Desugaring**: Support for `impl Trait` (existential types) and Lifetimes.
- [x] **Robust Error Handling**: Precise error propagation using `thiserror`.

## Intermediate Representation (IR)

The IR is defined in `crates/volar-compiler/src/ir.rs`.

### Total Control Flow
To ensure totality, the IR only supports bounded iteration:

```rust
// crates/volar-compiler/src/ir.rs
pub enum IrExpr {
    // Bounded range-based loops
    BoundedLoop {
        var: String,
        start: Box<IrExpr>,
        end: Box<IrExpr>,
        inclusive: bool,
        body: IrBlock,
    },
    // Collection-based iteration
    IterLoop {
        pattern: IrPattern,
        collection: Box<IrExpr>,
        body: IrBlock,
    },
    // ...
}
```

### Specialized Array Operations
Common array patterns are lifted into specialized IR nodes to facilitate hardware acceleration and optimization:

- `ArrayGenerate`: desugared from `GenericArray::generate`.
- `ArrayMap`: desugared from `.map(|x| ...)`.
- `ArrayZip`: desugared from `.zip(other).map(|(a, b)| ...)`.
- `ArrayFold`: desugared from `.fold(init, |acc, x| ...)`.

### Specialized Type System
The IR distinguishes between standard Rust types and domain-specific cryptographic types:

```rust
// crates/volar-compiler/src/ir.rs
pub enum PrimitiveType {
    Bool, U8, U32, U64, Usize, I128,
    Bit, Galois, Galois64, BitsInBytes, BitsInBytes64,
}

pub enum ArrayKind {
    GenericArray, // Fixed-size from GenericArray crate
    FixedArray,   // Standard [T; N]
    Slice,        // [T]
}
```

### Existential Types & Generics
`impl Trait` is desugared into `IrType::Existential`, and generic parameters are classified by kind:

```rust
// crates/volar-compiler/src/ir.rs
pub enum IrType {
    // ...
    Existential { bounds: Vec<IrTraitBound> },
}

pub enum IrGenericParamKind {
    Type,
    Const,
    Lifetime,
}
```

## Parsing & Lowering

- **Parser** (`src/parser.rs`): Maps `syn` AST to `IrModule`. It performs "on-the-fly" specialization by identifying known trait paths (e.g., `Add`, `BlockEncrypt`) and method names.
- **Lowering/Monomorphization** (`src/lowering.rs`): (In Progress) Handles the substitution of type parameters and resolution of associated types (e.g., `<T as Trait>::Output`).
- **Printer** (`src/printer.rs`): Provides a human-readable representation of the IR for debugging.

## Error Handling

The compiler uses `CompilerError` for exhaustive error reporting:

- `UnboundedLoop`: Returned when `while` or `loop` is encountered.
- `Unsupported(String)`: Returned for Rust features outside the supported VOLE subset.
- `InvalidType(String)`: Returned for malformed or unrecognized type signatures.

## Future Roadmap

1. **Complete Monomorphization**: Fully specialize all generic functions and structs.
2. **Backend Code Generation**: Implement backends for C and hardware description languages.
3. **Formal Verification**: Leverage the total nature of the IR to provide termination proofs for generated code.
