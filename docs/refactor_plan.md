# Refactoring Plan: Intermediate Dynamic IR Transformation

Currently, `printer_dyn.rs` performs a direct transformation from the specialized `volar-spec` IR to dynamic Rust code. This makes it difficult to target other languages or perform further optimizations. The goal is to refactor this into a two-stage process:
1.  **Lowering**: Transform the `volar-spec` IR into a "Dynamic IR" (let's call it `IrModule` but with dynamic semantics).
2.  **Printing**: Print this Dynamic IR to Rust or other languages.

Actually, the current `IrModule` is already quite general. The "transformation" mostly involves:
-   Renaming structs (adding `Dyn` suffix).
-   Converting type-level lengths to runtime values.
-   Unpacking witnesses in methods.
-   Adding `_phantom` fields.
-   Injecting specific helper calls (like `cipher::Block::from_mut_slice`).

## Step 1: Define the Lowering Logic
Create a new module `lowering_dyn.rs` that takes an `IrModule` and produces a *new* `IrModule` representing the dynamic version of the specification.

### Data Transformations:
-   **Structs**:
    -   Append `Dyn` to `StructKind::Custom`.
    -   Add `usize` fields for every generic parameter classified as `GenericKind::Length`.
    -   Add `_phantom: PhantomData<...>` if crypto generics are present.
-   **Functions/Methods**:
    -   Transform signatures to include `usize` parameters for length generics.
    -   In methods, add `let n = self.n;` statements at the beginning of the body to "unpack" witnesses.
    -   Transform types in signatures (e.g., `GenericArray<T, N>` -> `Vec<T>`).
-   **Expressions**:
    -   `N::to_usize()` -> `n` (variable access).
    -   `GenericArray::default()` -> `Vec::new()`.
    -   Handle the `encrypt_block` special case by transforming the call to a specialized IR construct or a specific function call.

## Step 2: Update `IrModule` and `IrExpr` if necessary
We might need a few more variants in `IrExpr` to represent dynamic-specific operations, or just use existing ones creatively.

## Step 3: Implement the Printer
The existing `printer.rs` is very basic. We can improve it to handle the dynamic IR produced in Step 1. `printer_dyn.rs` can be phased out or repurposed as the "Lowering + Printing" coordinator.

## Step 4: Verification
Verify that `cargo test -p volar-compiler test_generated_dyn_compiles` still passes after the refactor.

---

### Task List:
- [ ] Create `crates/volar-compiler/src/lowering_dyn.rs`.
- [ ] Move logic for `classify_generic` and `StructInfo` to `lowering_dyn.rs`.
- [ ] Implement `lower_module_dyn(module: &IrModule) -> IrModule`.
- [ ] Update `printer.rs` to support the full range of IR constructs (it's currently missing many `IrExpr` and `IrPattern` variants).
- [ ] Connect them in `printer_dyn.rs` (which will now be a wrapper).
