# volar-spec — Cryptographic Protocol Specifications

`volar-spec` (`crates/volar-spec`) is the **specification layer** of Volar.
It defines cryptographic protocols in a total, generic subset of Rust that
can be read by both the Rust compiler (for correctness checking and testing)
and by `volar-compiler` (for transpilation to dynamic Rust or TypeScript).

All code in this crate uses `#![no_std]` and must stay within the
[total fragment](compiler.md#totality) accepted by the compiler: no unbounded
loops, no recursion, no `while` or bare `loop`.

---

## Module Map

```
volar-spec/src/
├── lib.rs          Re-exports; feature flags
├── vole.rs         VOLE core types: Delta, Q, Vope, VoleArray trait
│   ├── vope.rs         Vope<N,T,K> polynomial VOLE structure
│   │   └── ai_hazmat.rs  Degree-K multiplication (hazardous; see below)
│   ├── field_rotate.rs  Field-rotation wrappers
│   │   └── vope.rs      Rotating Vope
│   ├── bitvole.rs      Bit-level VOLE utilities
│   ├── poly.rs         Polynomial helpers
│   ├── garble.rs       VOLE-derived garbled circuit types (re-exported)
│   └── impls.rs        Blanket trait implementations
├── garble.rs       Garbled circuit specification
├── mpc.rs          Multi-party computation types
├── byte_gen.rs     Byte generation: ABO, BSplit, gen_abo, VOLE-from-material
└── xsat.rs.insecure  ⚠ Witness-encryption attempt (see insecure.md)
```

---

## The VOLE Core (`vole.rs`)

### VOLE Relation

A VOLE correlation satisfies:

```
u · Δ + v = q
```

- **Δ** (`Delta<N, T>`) — the verifier's global secret vector of length N.
- **u** — the prover's "multiplier".
- **v** — the prover's "offset".
- **q** (`Q<N, T>`) — the verifier's share.

`VoleArray<T>` is a marker trait alias for `ArrayLength<T>` — it bounds array
length type parameters to express that an array is sized at the type level.

### Delta and Q Operations

Both `Delta<N, T>` and `Q<N, T>` support:

- **`remap(f)`** — permute elements by an index function (bounded by N).
- **`rotate_left(n)`** / **`rotate_right(n)`** — cyclic rotation over the vector.

`Delta` additionally supports:

- **`static(val)`** — compute `val[i] * delta[i]` to produce a `Q`, representing
  a public constant in the VOLE relation.

These operations respect the VOLE relation:
- Adding two pairs: `(u₁+u₂)·Δ + (v₁+v₂) = q₁+q₂`.
- XOR-ing a public value p into u: `(u+p)·Δ + v = q + p·Δ`.
- XOR-ing p into v: `u·Δ + (v+p) = (q+p)`, sharing u with derivatives.

### Vope — Polynomial VOLE (`vole/vope.rs`)

```rust
pub struct Vope<N: VoleArray<T>, T, K: ArrayLength<GenericArray<T, N>> = U1> {
    pub u: GenericArray<GenericArray<T, N>, K>,  // K coefficient vectors of length N
    pub v: GenericArray<T, N>,                    // constant term (degree 0)
}
```

`Vope<N, T, K>` is a **polynomial of degree K in Δ** whose evaluation at Δ
produces a `Q<N, T>`. Concretely, `Vope * Delta` yields:

```
q[i] = v[i] + u[0][i]·Δ[i] + u[1][i]·Δ[i]² + … + u[K-1][i]·Δ[i]^K
```

Key operations:

| Operation | Description |
|---|---|
| `Vope::constant(v)` | Create a degree-0 Vope (no randomizer, just a public value) |
| `vope1 + vope2` | Add two Vopes element-wise; degree unchanged |
| `vope ^ public_array` | XOR a public value into the constant term |
| `vope * delta` | Evaluate the polynomial; produces `Q<N, O>` |
| `vope.remap(f)` | Permute underlying vectors by index function |
| `vope.expand()` | Zero-extend to higher degree |
| `vope.scale(f)` | Map `Bit` elements to another type via a boolean function |

#### Degree-K Multiplication (`ai_hazmat.rs`)

The `ai_hazmat` submodule contains `mul_generalized`, which multiplies two
`Vope`s of degrees K and K₂ to produce a `Vope` of degree K+K₂ via SIMD
convolution over the coefficient arrays.

> **Warning**: This is marked `ai_hazmat` because multiplying VOLE polynomials
> only remains sound in specific contexts. The caller is responsible for ensuring
> the algebraic product is used within a valid Quicksilver-style constraint check.
> Using multiplication outside this context can break the zero-knowledge property.

---

## Garbled Circuits (`garble.rs`)

The garbled circuit specification implements a standard half-gate garbling
scheme built on top of VOLE types.

### Types

| Type | Description |
|---|---|
| `Garble<N>` | A garbler's share: a uniformly random `base` byte-array of length N |
| `Eval<N>` | An evaluator's share: a `target = base ⊕ encoded_value` |
| `GarbleTable<N>` | A 4-entry AND gate table (indexed by input wire bits) |
| `GlobalSecret<N>` | The garbler's global secret Δ (LSB forced to 1 for free-XOR) |

### Protocol

**Encode a wire value:**
```
GlobalSecret::encode(garble, value)
  → Eval { target: if value { Δ ⊕ base } else { base } }
```

**Evaluate XOR gate:** Free via `Eval ^ Eval` (XOR of targets).

**Generate AND table:**
`GlobalSecret::gen_and_table<D>(a, b)` produces a `GarbleTable<N>` using a
hash function `D: Digest<OutputSize = N>`. The table is indexed by the free-XOR
bits of the input wire labels. Each entry is:

```
H(encode(a, av) || encode(b, bv)) ⊕ result_wire_label
```

**Evaluate AND gate:** `Eval::and_via_table(other, table)` hashes the input
labels and XORs with the appropriate table entry.

**Bit packing:** Both `Garble` and `Eval` provide `to_share()` which bit-packs
a length-N byte array into a length-N/8 byte array (mapping `base[8i+j] & 1`
into bit j of `output[i]`).

---

## Multi-Party Computation (`mpc.rs`)

A minimal type-level description of the MPC party model:

```rust
pub struct AllParties<N, T> {
    pub other_parties: OtherParties<N, T>,  // shares held by the N other parties
    pub self_party: T,                       // this party's own share
}

pub struct OtherParties<N, T> {
    pub other_parties: GenericArray<T, N>,
}
```

These types serve as the generic skeleton for N+1 party protocols. They are
a placeholder for a full MPC framework; the semantics of share combination,
consistency checks, and communication rounds are specified elsewhere.

---

## Byte Generation (`byte_gen.rs`)

`byte_gen` provides utilities for generating VOLE material from block ciphers
and hash functions.

### ABO — Authenticated Bit-OT

`ABO<B, D, K, N>` (Authenticated Bit Oblivious Transfer) stores:
- `commit` — a hash commitment binding the entire structure.
- `per_byte` — an N×K×B::OutputSize table of pre-generated byte arrays.

`gen_abo` generates an ABO from a seed `a` and randomness, using a
`LengthDoubler` `B` (typically an AES-based PRG) and digest `D`. Internally,
for each of the K choices, it uses the binary decomposition of the choice index
to walk a tree built by repeated calls to `B::double`, which doubles the output
length at each step. This is the "hypercube" technique that avoids N separate
VOLE instances.

### VOLE from Material

`create_vole_from_material<B, X>(s)` converts a slice of byte arrays `s` into a
`Vope<B::OutputSize, u8>` by XOR-accumulation:

```
u = ⊕ s[i]                   (XOR of all inputs)
v = ⊕ (s[i] ⊕ i)              (XOR of all inputs masked by their index)
```

`create_vole_from_material_expanded` adds a pre-processing step `f` applied
to each input slice before accumulation.

### BSplit

`BSplit<B, D>` stores a `Logarithm2(D::OutputSize)`-length array of pairs
`[GenericArray<u8, B::OutputSize>; 2]`, representing a tree of AES double
evaluations for length-doubling. Used internally for splitting VOLE randomness.

### Supporting Types

- `ABOOpening<B, D, T, U, N>` — opening proofs for ABO positions, used in
  consistency checking protocols.

---

## volar-common

`volar-common` provides two shared utilities:

### `hash_commitment`

`commit<D: Digest>(data, nonce) -> GenericArray<u8, D::OutputSize>`

A simple hash commitment: `H(data || nonce)`. Used in `gen_abo` to bind
the ABO structure to a specific key.

### `length_doubling`

`LengthDoubler` is the core trait for AES-based PRGs:

```rust
pub trait LengthDoubler {
    type OutputSize: ArrayLength<u8>;
    fn double(input: GenericArray<u8, Self::OutputSize>)
        -> [GenericArray<u8, Self::OutputSize>; 2];
}
```

`double` takes an output-size block and returns two output-size blocks —
effectively doubling the generated randomness. The canonical implementation
uses AES in a counter-like mode.

`PuncturableLengthDoubler` (not yet fully specified) extends `LengthDoubler`
with puncturing: the ability to reveal one branch of the tree while hiding
the other, which is the basis for punctured PRF-based VOLE.

---

## volar-primitives

`volar-primitives` provides the concrete field element types used throughout
`volar-spec`:

| Type | Underlying | Field | Notes |
|---|---|---|---|
| `Bit` | `bool` | GF(2) | XOR = add, AND = mul |
| `Galois` | `u8` | GF(2⁸) = GF(256) | AES field; irreducible poly x⁸+x⁴+x³+x+1 |
| `BitsInBytes` | `u8` | GF(2)⁸ packed | SIMD 8-lane bit field; add=XOR, mul=AND |
| `Galois64` | `u64` | GF(2⁶⁴) | 64-bit extension field |
| `BitsInBytes64` | `u64` | GF(2)⁶⁴ packed | SIMD 64-lane bit field |
| `Tropical<T>` | any `T` | Tropical semiring | add=min, mul=add |

All types implement `Add`, `Mul`, `Sub`, `Clone`, `Copy`, `Default`,
`PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`.

`Galois` additionally implements `Invert` (multiplicative inverse via
Fermat's little theorem: a⁻¹ = a^(2⁸-2)).

Arithmetic is done via the `backend::field_mul` generic function, which
implements carry-less multiplication modulo an irreducible polynomial using
the standard shift-and-XOR method.

---

## Status

| Feature | Status |
|---|---|
| VOLE primitives (Delta, Q, Vope) | ✅ Complete |
| Garbled circuits | ✅ Complete |
| MPC types (AllParties, OtherParties) | 🔲 Stub — semantics TBD |
| Byte generation (ABO, LengthDoubler) | ✅ Complete |
| Witness encryption | ⚠ See [insecure.md](insecure.md) |
| IR proving (one round) | 🔲 Planned |
| Consistency checks | 🔲 Planned |
| Succinct proofs | 🔲 Planned |
