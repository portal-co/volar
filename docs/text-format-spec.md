# Volar IR Text Format Specification

> Version: 1  
> Status: Stable for `volar-lir-text` (v1). `volar-ir-text` and `volar-compiler-text` are draft.

## Motivation

`rkyv` produces opaque binary blobs with no versioning guarantee, requires
complex bound annotations for recursive types (see `LirType::Arr`/`LirType::Ptr`),
and cannot be inspected or diffed. This specification defines stable, human-readable
text formats for all three IR layers so that serialised IR artefacts can be:

- Version-controlled and diffed
- Inspected and debugged without tooling
- Replayed across rkyv version changes
- Round-tripped without information loss (provenance `P` is always erased to `()`)

## Crate map

| Crate | Covers |
|---|---|
| `volar-lir-text` | `LirType`, `StructDef`, `IcmpPred`, `LirCall`, `SavedLirModule` |
| `volar-ir-text` | `volar-ir-common` primitives, `IRBlocks`, `BIrBlocks` |
| `volar-compiler-text` | `IrModule`, `IrExpr`, `IrStmt`, CFG variants |

---

## 1. Lexical conventions (all formats)

```
; this is a comment — ignored to end of line
```

- Comments begin with `;` and extend to the end of the line.
- Whitespace (space, tab, `\r`, `\n`) is ignored between tokens except inside
  quoted strings.
- **Integers**: decimal by default; `0x` prefix for hexadecimal (case-insensitive).
  Negative integers use a leading `-`.
- **Quoted strings**: `"..."` with the following escape sequences:
  - `\\` → `\`
  - `\"` → `"`
  - `\n` → newline
  - `\t` → tab
  - `\r` → carriage return
- **Identifiers**: `[a-zA-Z_][a-zA-Z0-9_]*` — used for keywords and variant names.
- **Lists**: `[item, item, ...]` — whitespace around `,` is optional.
- **Nested lists**: `[[a, b], [c]]` — full recursive nesting.
- **Structs**: `(field_value, ...)` — positional tuples.
- **Key=value pairs**: `key=value` — no spaces around `=`.

---

## 2. `LirType` inline notation

`LirType` appears inline wherever a type is required (not on its own line).

```ebnf
lir_type ::=
    "bool"
  | "i8" | "u8"
  | "i16" | "u16"
  | "i32" | "u32"
  | "i64" | "u64"
  | "arr[" lir_type ", " nat "]"
  | "struct:" nat
  | "native:" native_prim
  | "ptr[" lir_type "]"

native_prim ::= "bit" | "u8" | "u16" | "u32" | "u64" | "u128" | "u256" | "aes8" | "galois64"
nat         ::= [0-9]+
```

Examples:
```
bool
u8
arr[u8, 32]
arr[arr[u32, 4], 8]
struct:0
native:bit
native:galois64
ptr[u8]
ptr[arr[u32, 4]]
```

`struct:N` refers to the `StructId` assigned by the `define_struct` directive
earlier in the same file.

---

## 3. `SavedLirModule` / `.vlm` format

File extension: `.vlm` ("Volar LIR Module").

### 3.1 Overall structure

```
volar-lir-saved v1
<directive>*
```

The **version line** must be the first non-comment, non-blank line. An unknown
version string produces `ParseError::UnsupportedVersion`.

Each subsequent line is a single **directive** (one `LirCall` variant).
Blank lines and comment lines are ignored between directives.

### 3.2 Directive reference

#### Type registration

```
define_struct id=<nat> name=<str> fields=[(<lir_type>, <str>), ...]
```

`id` is the `StructId` returned by the original `define_struct` call. The parser
assigns struct IDs in encounter order; the recorded `id` must equal this counter
(parser error otherwise).

#### Function management

```
begin_function name=<str> params=[<lir_type>, ...] ret=<lir_type|"none"> entry=<nat> pvals=[[<nat>, ...], ...]
end_function
```

`ret=none` means `Option<LirType>::None`.  
`pvals` contains one inner list per parameter; each inner list is the flat scalar
value indices assigned for that parameter.

#### Block management

```
create_block block=<nat>
add_block_param block=<nat> ty=<lir_type> val=<nat>
switch_to_block block=<nat>
```

#### Constants

```
iconst ty=<lir_type> val=<int> out=<nat>
```

#### Arithmetic

```
add lhs=<nat> rhs=<nat> out=<nat>
sub lhs=<nat> rhs=<nat> out=<nat>
mul lhs=<nat> rhs=<nat> out=<nat>
udiv lhs=<nat> rhs=<nat> out=<nat>
sdiv lhs=<nat> rhs=<nat> out=<nat>
```

#### Bitwise

```
and  lhs=<nat> rhs=<nat> out=<nat>
or   lhs=<nat> rhs=<nat> out=<nat>
xor  lhs=<nat> rhs=<nat> out=<nat>
not  val=<nat> out=<nat>
shl  val=<nat> shift=<nat> out=<nat>
lshr val=<nat> shift=<nat> out=<nat>
ashr val=<nat> shift=<nat> out=<nat>
```

#### Comparison

```
icmp pred=<icmp_pred> lhs=<nat> rhs=<nat> out=<nat>

icmp_pred ::= "eq" | "ne" | "ult" | "ule" | "ugt" | "uge" | "slt" | "sle" | "sgt" | "sge"
```

#### Conversions

```
zext  val=<nat> dst=<lir_type> out=<nat>
sext  val=<nat> dst=<lir_type> out=<nat>
trunc val=<nat> dst=<lir_type> out=<nat>
```

#### Select

```
select cond=<nat> then=<nat> else=<nat> out=<nat>
```

#### Terminators

```
jump target=<nat> args=[<nat>, ...]
branch cond=<nat> then_block=<nat> then_args=[<nat>, ...] else_block=<nat> else_args=[<nat>, ...]
ret vals=[<nat>, ...]
```

#### External calls

```
call_extern name=<str> arg_tys=[<lir_type>, ...] args=[<nat>, ...] ret=<lir_type|"none"> outs=[<nat>, ...]
```

#### Crypto primitives

```
oracle name=<str> arg_tys=[<lir_type>, ...] args=[<nat>, ...] ret_tys=[<lir_type>, ...] outs=[<nat>, ...]
action name=<str> guard=<nat> arg_tys=[<lir_type>, ...] args=[<nat>, ...] fallbacks=[<nat>, ...] ret_tys=[<lir_type>, ...] outs=[<nat>, ...]
rng ty=<lir_type> out=<nat>
```

#### Stack allocation

```
alloca elem=<lir_type> count=<nat> out=<nat>
ptr_load ptr=<nat> ty=<lir_type> out=<nat>
ptr_store ptr=<nat> val=<nat>
ptr_offset ptr=<nat> idx=<nat> out=<nat>
ptr_index_load ptr=<nat> idx=<nat> pointee=<lir_type> outs=[<nat>, ...]
ptr_index_store ptr=<nat> idx=<nat> vals=[<nat>, ...] pointee=<lir_type>
```

#### Metadata

```
value_type val=<nat> ty=<lir_type>
set_prov
```

These carry no downstream target calls (see `SavedLirModule::replay`). They are
serialised and deserialised as-is.

### 3.3 Full example

```
volar-lir-saved v1
; one-function module: adds two u8 args and returns the result
define_struct id=0 name="Pair" fields=[(u8, "a"), (u8, "b")]
begin_function name="add_bytes" params=[u8, u8] ret=u8 entry=0 pvals=[[0], [1]]
value_type val=0 ty=u8
value_type val=1 ty=u8
switch_to_block block=0
add lhs=0 rhs=1 out=2
value_type val=2 ty=u8
ret vals=[2]
end_function
```

---

## 4. `volar-ir` circuit IR / `.vir` format

File extension: `.vir` ("Volar IR").

### 4.1 `IrType` (low-level) inline notation

`IrType` from `volar-ir-common` is written inline. `TypeId` references use `#N`.

```ebnf
vir_type ::=
    "bit" | "u8" | "u16" | "u32" | "u64" | "u128" | "u256" | "aes8" | "galois64"
  | "vec[" "#" nat ", " nat "]"
  | "tuple[" ("#" nat ",")* "]"
  | "block[" ("#" nat ",")* "]"
  | "func[(" ("#" nat ",")* ") -> (" ("#" nat ",")* ")]"
```

`Constant` (256-bit): `0x` followed by 64 hex digits, zero-padded on the left.

### 4.2 Overall structure

```
volar-ir v1

types:
  <nat>: <vir_type>
  ...

oracles: [ { name=<str> params=[#<nat>, ...] results=[#<nat>, ...] }, ... ]
actions: [ { name=<str> params=[#<nat>, ...] results=[#<nat>, ...] }, ... ]
rngs:    [ { name=<str> ty=#<nat> }, ... ]

block <nat> (<param_list>):
  <stmt>*
  <terminator>

...
```

### 4.3 Statements (IRStmt / Stmt<IRVarId>)

```
v<nat> = storage_read  storage=<storage_id> ty=#<nat> addr=v<nat>
         storage_write storage=<storage_id> src=v<nat> ty=#<nat> addr=v<nat>
v<nat> = const <constant> ty=#<nat>
v<nat> = transmute src=v<nat> src_ty=#<nat> dst_ty=#<nat>
v<nat> = poly ty=#<nat> constant=<constant> coeffs={<coeff>, ...}
v<nat> = rol src=v<nat> ty=#<nat> n=<nat>
v<nat> = ror src=v<nat> ty=#<nat> n=<nat>
v<nat> = merge parts=[v<nat>, ...] ty=#<nat>
v<nat> = splat src=v<nat> ty=#<nat>
v<nat> = shuffle result_bits=[(u8, v<nat>), ...] ty=#<nat>
v<nat> = oracle_call name=<str> args=[v<nat>, ...] output_tys=[#<nat>, ...] result_ty=#<nat>
v<nat> = oracle_output call=v<nat> idx=<nat> ty=#<nat>
v<nat> = action_call name=<str> guard=v<nat> args=[v<nat>, ...] fallbacks=[v<nat>, ...] output_tys=[#<nat>, ...] result_ty=#<nat>
v<nat> = action_output call=v<nat> idx=<nat> ty=#<nat>
v<nat> = rng name=<str> ty=#<nat>

storage_id ::= "default" | "stack" | "mem:" <nat>
coeff       ::= "[" ("v" <nat> ",")* "]" ":" <nat>
```

### 4.4 Terminators (IRTerminator)

```
jmp <block_target> [v<nat>, ...]
jmp_cond v<nat> then=<block_target> then_args=[v<nat>, ...] else=<block_target> else_args=[v<nat>, ...]
jmp_table v<nat> cases={ <constant>=(<block_target>, [v<nat>, ...]), ... }

block_target ::= "block:" <nat> | "return" | "dyn:" "v" <nat>
```

### 4.5 BIrBlocks (Boolar IR)

Boolar IR uses the same file header (`volar-ir v1`) with a `boolar:` section prefix.

---

## 5. `volar-compiler` high-level IR / `.vca` format

File extension: `.vca` ("Volar Compiler AST").

This format mirrors the `IrModule` AST tree. It is **not** the Rust printer
output — it serialises the IR nodes directly so that round-tripping is lossless.

> Specification deferred to `volar-compiler-text` implementation (Phase 3).
> The format will be defined in this document when the crate is implemented.

---

## 6. Versioning policy

- The **version line** (`volar-lir-saved v1`, `volar-ir v1`, etc.) is mandatory and
  must be the first non-comment, non-blank line.
- **Minor additions** (new optional `key=value` fields on existing directives) are
  backward-compatible. Parsers skip unknown keys.
- **New directive types** are not backward-compatible; they bump the version.
- **Removed directives** cause a `ParseError::UnknownDirective` in older parsers.
- Version `v1` is the initial stable version. Future incompatible changes use `v2`, etc.
- Cross-version compatibility is not guaranteed. Convert with the tool that produced
  the file.

---

## 7. Reliability tags

All crates in this format family are tagged:

```rust
// @reliability: experimental
// @ai: assisted
```

New cryptographic infrastructure depending on these crates requires separate review
before deployment.
