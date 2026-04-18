# LIR — Low-Level IR Target

The LIR subsystem bridges the compiler's high-level `IrModule` and the
low-level integer code needed for correctness testing. It lives in three crates:

| Crate | Role |
|---|---|
| `crates/ir/volar-lir` | `LirTarget` trait + type definitions (no_std) |
| `crates/compiler/volar-lir-codegen` | `IrModule` → `LirTarget` lowering + monomorphization |
| `crates/compiler/volar-c-backend` | `LirTarget` implementation that emits C99 |

---

## `volar-lir` — The Trait

`LirTarget` is a **builder** trait: the caller issues a sequence of method calls
and the implementation produces whatever output it targets (C text, machine code,
in-memory IR, etc.).

```rust
pub trait LirTarget {
    type Value: Copy + Eq + Debug;
    type Block: Copy + Eq + Debug;

    // Struct registration (must precede use of StructId)
    fn define_struct(&mut self, def: StructDef) -> StructId;

    // Function scope
    fn begin_function(&mut self, name: &str, params: &[LirType], ret: Option<LirType>)
        -> (Self::Block, Vec<Self::Value>);
    fn end_function(&mut self);

    // Block management
    fn create_block(&mut self) -> Self::Block;
    fn add_block_param(&mut self, block: Self::Block, ty: LirType) -> Self::Value;
    fn switch_to_block(&mut self, block: Self::Block);

    // Constants / arithmetic / bitwise / comparison / conversions / select …
    fn iconst(&mut self, ty: LirType, val: i64) -> Self::Value;
    fn add(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    // … (see lib.rs for the full list)

    // Array operations
    fn arr_new(&mut self, elem_ty: LirType, elems: &[Self::Value]) -> Self::Value;
    fn arr_get(&mut self, arr: Self::Value, idx: Self::Value) -> Self::Value;
    fn arr_set(&mut self, arr: Self::Value, idx: Self::Value, val: Self::Value) -> Self::Value;

    // Struct operations
    fn struct_new(&mut self, id: StructId, fields: &[Self::Value]) -> Self::Value;
    fn struct_get(&mut self, val: Self::Value, field_idx: usize) -> Self::Value;

    // Extern calls
    fn call_extern(&mut self, name: &str, ret_ty: Option<LirType>, args: &[Self::Value])
        -> Option<Self::Value>;

    // Terminators
    fn jump(&mut self, target: Self::Block, args: &[Self::Value]);
    fn branch(&mut self, cond: Self::Value,
               then_block: Self::Block, then_args: &[Self::Value],
               else_block: Self::Block, else_args: &[Self::Value]);
    fn ret(&mut self, val: Option<Self::Value>);
}
```

### Block-parameter SSA

Control-flow joins use block parameters (Cranelift / MLIR style) rather than
phi nodes. A phi:

```
%v = phi [%a, bb0], [%b, bb1]
```

becomes a block `bb_join(p0)` with:
- `jump bb_join(%a)` from `bb0`
- `jump bb_join(%b)` from `bb1`

### `LirType`

```rust
pub enum LirType {
    Bool,                         // 1-bit boolean
    I8, U8, I16, U16, I32, U32, I64, U64,
    Arr(Box<LirType>, usize),     // fixed-size homogeneous array
    Struct(StructId),             // named struct (registered via define_struct)
}
```

`LirType` is `Clone` (not `Copy`) because `Arr` boxes its element type.

### `StructDef`

```rust
pub struct FieldDef { pub name: String, pub ty: LirType }
pub struct StructDef { pub name: String, pub fields: Vec<FieldDef> }
```

Structs must be registered with `define_struct` before any instruction references
them. Callers are responsible for dependency ordering.

---

## `volar-c-backend` — C99 Backend

`CBackend` implements `LirTarget` and emits a self-contained C99 source file.

### Usage

```rust
let mut b = CBackend::new();

// (Optional) register struct types before begin_function
let eval_id = b.define_struct(StructDef {
    name: "Eval".into(),
    fields: vec![FieldDef { name: "base".into(), ty: LirType::Arr(Box::new(LirType::U8), 16) }],
});

let (entry, params) = b.begin_function("my_fn", &[LirType::U32], Some(LirType::U32));
b.switch_to_block(entry);
let doubled = b.add(params[0], params[0]);
b.ret(Some(doubled));
b.end_function();

let c_source: String = b.finish();
// c_source is a complete #include'd C file, compile with: cc -O0 -std=c99
```

### Generated C structure

```c
#include <stdint.h>
#include <stdbool.h>

// 1. Array typedefs (innermost first)
typedef struct { uint8_t data[16]; } Arr_U8_16;

// 2. Struct typedefs (in define_struct call order)
typedef struct {
  Arr_U8_16 base;
} Eval;

// 3. Extern declarations (from call_extern)
extern Eval and_via_table_sha256(Eval recv, Eval other, Eval table);

// 4. Function definitions
uint32_t my_fn(uint32_t v0) {
  uint32_t v1 = v0 + v0;
  return v1;
}
```

### Block parameters in C

Block parameters become pre-declared local variables (`blockM_pN`) at the
function top. Jumps use **two-phase parallel assignment** to avoid read-after-write
hazards:

```c
uint64_t countdown(uint64_t v0, uint64_t v1) {
  uint64_t block1_p0;   // pre-declared block params
  uint64_t block1_p1;
block0:;
  uint64_t _t0 = v0; uint64_t _t1 = v1;   // phase 1: snapshot
  block1_p0 = _t0; block1_p1 = _t1;        // phase 2: assign
  goto block1;
block1:;
  // ...
}
```

Temporary names (`_t0`, `_t1`, …) are globally unique within the function to
avoid C's flat scoping rules conflicting across multiple branches.

### Array representation

`LirType::Arr(T, N)` → `typedef struct { T data[N]; } Arr_T_N;`

This allows arrays to be passed and returned **by value** in C, enabling the
value-copy model used throughout LIR. `arr_set` emits a copy-and-mutate:

```c
Arr_U8_16 vN = src;      /* copy */
vN.data[idx] = val;       /* mutate copy */
```

---

## `volar-lir-codegen` — IrModule Lowering

### Entry points

```rust
// Lower all functions in a module (builds struct registry automatically).
pub fn lower_module<T: LirTarget>(module: &IrModule, target: &mut T);

// Same, but with an explicit crypto hash suffix for extern name generation.
pub fn lower_module_with_opts<T: LirTarget>(module: &IrModule, target: &mut T, hash_suffix: &str);

// Lower a single function (no struct support).
pub fn lower_function<T: LirTarget>(func: &IrFunction, target: &mut T);
```

### Monomorphization (`mono.rs`)

Generic modules (with `TypeParam` lengths like `N: ArraySize`) must be
monomorphized before lowering. The `MonoEnv` specifies concrete values:

```rust
let env = MonoEnv::new("sha256")   // hash suffix for D: Digest
    .with_len("N", 16);            // N → 16

let mono_module = monomorphize_module(&module, &env);
```

`monomorphize_module` traverses the entire `IrModule` — struct definitions,
function signatures, and every expression — substituting:
- `ArrayLength::TypeParam("N")` → `ArrayLength::Const(16)` in all array lengths
- Struct type_args containing `TypeParam` are recursively substituted

The hash-algorithm parameter `D` is not a concrete type; it becomes an extern
name suffix (e.g., `and_via_table_sha256`) at method-dispatch time.

### Struct registry (`structs.rs`)

`build_struct_registry` registers each non-`GenericArray` struct from
`module.structs` with the `LirTarget`:

```rust
let registry = build_struct_registry(&mono_module, &mut target);
```

The registry records `StructKind` → `(StructId, field_names)` and provides:
- `id_for(kind)` — look up a struct's `StructId`
- `field_index(id, name)` — field name → 0-based declaration-order index
- `ir_type_to_lir(ty)` — convert an `IrType` to `LirType` using the registry

### Type mapping

| `IrType` | `LirType` |
|---|---|
| `Primitive(Bool \| Bit)` | `Bool` |
| `Primitive(U8 \| Galois)` | `U8` |
| `Primitive(U32)` | `U32` |
| `Primitive(U64 \| Galois64 \| Usize)` | `U64` |
| `Array { elem: T, len: Const(N) }` | `Arr(lir(T), N)` |
| `Struct { kind, .. }` | `Struct(registry.id_for(kind))` |
| `Reference { elem, .. }` | `lir(elem)` (transparent) |

### Expression lowering

| `IrExpr` variant | LIR emission |
|---|---|
| `Var(name)` | `env[name]` |
| `Lit(Int(n))` | `iconst(U64, n)` |
| `Binary { BitXor, l, r }` | `xor(lower(l), lower(r))` |
| `Field { base, field }` | `struct_get(lower(base), field_idx)` |
| `Index { base, idx }` | `arr_get(lower(base), lower(idx))` |
| `StructExpr { kind, fields }` | `struct_new(id, &fields_in_decl_order)` |
| `FixedArray(elems)` | `arr_new(elem_ty, &lower_each(elems))` |
| `ArrayGenerate { len, index_var, body }` | unrolled (see §Loop unrolling) |
| `RawMap { receiver, elem_var, body }` | unrolled element-wise map |
| `RawZip { left, right, vars, body }` | unrolled element-wise binary op |
| `BoundedLoop { var, start, end, body }` | LIR loop via block params |
| `MethodCall { .clone() }` | identity (value semantics) |
| `MethodCall { Unknown(name) }` | `call_extern("{name}_{hash_suffix}", ...)` |
| `If { cond, then, else }` | branch + join block with block param |
| `Cast { .. }` | `zext` |

Field access (`Field`) requires type inference on the base expression. The
lowering context tracks `env_types: BTreeMap<String, IrType>` for each bound
variable. The `infer_type` function recurses through `Field`, `Index`, and
`.clone()` chains to find the originating struct kind.

---

## Loop Unrolling

Arrays in the garbling pipeline have **statically known sizes** (the concrete N
from monomorphization). Three `IrExpr` variants that represent loops over
fixed-size collections are handled by **compile-time unrolling** rather than
emitting runtime loop blocks:

### `ArrayGenerate { len: Const(N), index_var: "j", body }`

Semantic: `Array::from_fn(|j| body)` — build an N-element array by evaluating
`body` once per index.

Lowering: evaluate `body` N times with `j` bound to each concrete index value,
then call `arr_new(elem_ty, &[v0, v1, ..., vN-1])`.

```
// Source: Array::<u8, 4>::from_fn(|j| a.base[j] ^ b.base[j])
// Emitted C (after unrolling with N=4):
Arr_U8_4 vR = (Arr_U8_4){ .data = {
    a.data[0] ^ b.data[0],
    a.data[1] ^ b.data[1],
    a.data[2] ^ b.data[2],
    a.data[3] ^ b.data[3]
}};
```

### `RawMap { receiver, elem_var, body }`

Semantic: element-wise unary array transform — `receiver.map(|x| body)`.

Lowering: for each index `k` in `0..N` (N from the receiver's `LirType::Arr`),
emit `arr_get(recv, k)`, bind `elem_var`, evaluate `body`, collect results,
emit `arr_new`.

### `RawZip { left, right, left_var, right_var, body }`

Semantic: element-wise binary array combinator — `left.zip(right, |a, b| body)`.

Lowering: for each index `k`, emit `arr_get(left, k)` and `arr_get(right, k)`,
bind both variables, evaluate `body`, collect, emit `arr_new`.

### `BoundedLoop { var, start, end, inclusive, body }`

Semantic: imperative counter loop — `for var in start..end { body }`.

Lowering: two-block LIR loop with counter and limit as block parameters:

```
loop_header(counter: U64, limit: U64):
  cmp = icmp(Ult, counter, limit)
  branch cmp → body_block, done_block

body_block:
  env[var] = counter
  lower(body)
  next = add(counter, iconst(1))
  jump loop_header(next, limit)

done_block:
  (unit result)
```

The limit is computed in the entry block and passed as a block parameter to avoid
re-evaluation on each iteration.

---

## Full Pipeline: Weaver Output → C

```
BIrBlocks (boolean circuit)
        │  volar-weaver::weave_evaluator / weave_garbler
        ▼
IrModule (generic: N: ArraySize, D: Digest)
        │  mono::monomorphize_module(env with N=16, hash_suffix="sha256")
        ▼
IrModule (concrete: all ArrayLength::Const, no TypeParam)
        │  lower_module_with_opts(&mono_module, &mut CBackend::new(), "sha256")
        │    ├─ structs::build_struct_registry (calls define_struct for each IrStruct)
        │    └─ lower_function_with_registry (for each IrFunction)
        ▼
C99 source (complete: typedefs + extern decls + functions)
        │  cc -O0 -std=c99
        ▼
Compiled test binary
```

---

## Testing

Integration tests are in `crates/compiler/volar-c-backend/tests/basic.rs`.
Each test emits C, appends a `main()`, compiles with `cc -O0 -std=c99`, runs,
and asserts stdout:

```
cargo test -p volar-c-backend
```

| Test | What it exercises |
|---|---|
| `test_add_two` | `U32` arithmetic, `ret` |
| `test_countdown` | Block parameters, `icmp`, `branch`, parallel assignment |
| `test_if_max_via_codegen` | `IrModule` if/else via `lower_function` |
| `test_arrays` | `arr_new`, `arr_get`, `arr_set`, array typedef emission |
| `test_structs` | `define_struct`, `struct_new`, `struct_get`, struct typedef |
| `test_phase2_codegen_struct_array` | `lower_module_with_opts` + `MonoEnv` |

---

## Still TODO

| Feature | Status |
|---|---|
| `Vec<T>` (dynamic arrays) | Requires heap allocation; out of scope for LIR |
| Garbler lowering (table accumulation) | Blocked by `Vec<T>` |
| Memory ops (`StorageRead`/`StorageWrite`) | No `load`/`store` in `LirTarget` yet |
| Multi-element `Vec(n, Galois)` in `lower_ir` | Needs aggregate IR type support |
| `Array::from_fn` in raw `IrExpr` (non-`ArrayGenerate`) | Use `lowering_dyn` first |
| Signed-compare predicates on aggregate types | Panics; scalars only |

---

## See Also

- [LIR ABI](lir-abi.md) — calling conventions, type passing, per-target
  policies, and the `LirAbi` configuration struct.
| Signed-compare predicates on aggregate types | Panics; scalars only |
