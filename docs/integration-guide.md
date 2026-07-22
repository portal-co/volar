# Volar Integration Guide

> How to use Volar in your application. For internal architecture, see
> [overview.md](overview.md). For the agent-facing companion, see
> [agents-guide.md](agents-guide.md).

This guide is for **integrators** — people writing applications that need
zero-knowledge proofs, garbled circuits, oblivious RAM, or
program-related cryptography in general, and who want to use Volar to do
it. It is opinionated about which APIs to depend on, in part because the
[pinnedness/stability policy](reliability.md) records what evidence exists and
what dependents may expect.

> **Volar is early-stage.** Most code is currently Unpinned, and no
> project-wide stability reclassification has been completed. Read
> [reliability.md](reliability.md) before depending on any crate. A legacy
> `@reliability:` marker is not a deployment, paper-binding, review, proof, or
> stability claim.

---

## 1. What Volar Provides

Volar is a Rust workspace plus generated TypeScript that exposes four
broad capabilities:

| Capability | Crate(s) | Current migration status | Deployment position |
|---|---|---|---|
| Field arithmetic over GF(2), GF(2⁸), GF(2⁶⁴), GF(2¹²⁸), GF(2²⁵⁶) | `volar-primitives` | Legacy marker; no explicit two-axis classification | Assess the exact pinned commit and evidence before deployment. |
| Hash commitments and length-doubling PRG | `volar-common` | Legacy marker; no explicit two-axis classification | Assess the exact pinned commit and evidence before deployment. |
| VOLE-based zero-knowledge proofs (Quicksilver-style VOLEitH) | `volar-spec` | Mostly legacy `experimental`; treat as Unpinned and Very unstable | **No.** Prototyping only. |
| Garbled circuits (half-gate scheme) | `volar-spec` (`garble`) | Legacy `experimental`; treat as Unpinned and Very unstable | **No.** Prototyping only. |
| Multi-party computation type skeletons | `volar-spec` (`mpc`) | Legacy `experimental`; treat as Unpinned and Very unstable | **No.** Stubs only. |
| Recursive Path ORAM (client + server) | `volar-oram`, `volar-oram-core` | Legacy `experimental`; treat as Unpinned and Very unstable | **No.** Prototyping only. |
| Generic protocol abstraction (transport-agnostic) | `volar-channel` | Legacy `experimental`; treat as Unpinned and Very unstable | **No.** Prototyping only. |
| Compiler from `volar-spec` Rust to dynamic Rust, TypeScript, or C | `volar-compiler`, `volar-weaver`, `volar-c-backend`, `volar-lir-codegen` | Legacy markers; no explicit two-axis classification | Generated output inherits the evidence and stability position of its input. |

The compiler is more mature than the protocols it compiles. If you need
something that runs cross-target (browser + server) but is not security
critical — for example, a deterministic protocol simulator or an
educational demo — the compiler path is a reasonable target today.

---

## 2. Choosing a Target

Volar has three output targets. Pick based on where the code needs to run
and how much you need to trust the output.

### 2.1 Native Rust (`volar-spec` directly)

Use when: your application is a Rust binary or library and runs on a
machine with `std`.

```toml
# Cargo.toml
[dependencies]
volar-primitives = { git = "https://github.com/portal-co/volar.git" }
volar-common     = { git = "https://github.com/portal-co/volar.git" }
volar-spec       = { git = "https://github.com/portal-co/volar.git" }
```

You import the primitive types and call the Rust APIs directly.
Pinnedness and stability records on the source files apply. During migration,
treat legacy `experimental` modules as Unpinned and Very unstable; do not
deploy them without the required independent evidence and human decision.

### 2.2 Dynamic Rust (`volar-spec-dyn`)

Use when: you need runtime-sized arrays (length determined at runtime,
not at the type level). The dynamic crate replaces every
`GenericArray<T, N>` with `Vec<T>` plus a runtime length witness, at the
cost of slightly worse codegen.

```toml
[dependencies]
volar-spec-dyn = { git = "https://github.com/portal-co/volar.git", features = ["generated"] }
```

The `generated` feature pulls in the auto-generated dynamic mirror of
`volar-spec`. There is also a hand-written reference (`core_types`) for
debugging.

### 2.3 TypeScript (`@portal-solutions/volar-runtime`)

Use when: your application runs in a browser, in Node.js, or anywhere a
TypeScript runtime is more practical than a Rust toolchain.

```bash
# from the repo root, generate the TS package
cargo generate-ts

# in your app
npm install @portal-solutions/volar-runtime
```

```typescript
import {
  Bit,
  Galois,
  Galois128,
  fieldAdd,
  fieldMul,
} from "@portal-solutions/volar-runtime";
```

The TS package exposes:

- **Primitive types**: `Bit`, `Galois`, `Galois64`, `BitsInBytes`,
  `BitsInBytes64`. Higher field types (`Galois128`, `Galois256`) are
  available via the generated bindings.
- **Generic field operations**: `fieldAdd`, `fieldSub`, `fieldMul`,
  `fieldBitxor`, `fieldBitor`, `fieldBitand`, `fieldShl`, `fieldShr`,
  `fieldEq`, `fieldNe`. These work on any field-element type the runtime
  knows about.
- **Hash commitments**: `commit`.
- **Length-doubling PRG**: `doubleVec`.
- **Generated dynamic protocol code** (under `generated.ts`).

The TS runtime is generated from the same `volar-spec` source as the
Rust dynamic crate; the cryptographic kernel is identical, just compiled
through `volar-compiler`'s TypeScript backend.

### 2.4 C (via `volar-c-backend`)

Use when: you need to embed a compiled circuit in a non-Rust, non-JS
environment — for example, an embedded system, a hardware harness, or a
foreign-language wrapper.

```rust
use volar_c_backend::CBackend;
use volar_lir_codegen::lower_cfg_module;

let mut backend = CBackend::new();
lower_cfg_module(&module, &mut backend);
let c_source = backend.finish();
```

The C backend currently targets correctness testing rather than
deployment. It does not implement large-aggregate ABI optimisations and
emits `unimplemented!` for `_128` / `_256` primitive types. See
[lir-abi.md](lir-abi.md).

---

## 3. Three Worked Integrations

### 3.1 Computing in GF(2¹²⁸)

`volar-primitives` has not yet received an explicit two-axis classification.
Pin and assess the exact revision before any deployment decision.

```rust
use volar_primitives::{Galois128, gf_invert_128};

let a = Galois128::from(0xdeadbeef_u128);
let b = Galois128::from(0xcafef00d_u128);

let sum     = a + b;             // XOR
let product = a * b;             // GCM-polynomial multiplication
let inv_a   = gf_invert_128(a);  // Itoh-Tsujii inversion

assert_eq!(a * inv_a, Galois128::from(1));
```

The same operation in TypeScript:

```typescript
import { Galois128, fieldAdd, fieldMul } from "@portal-solutions/volar-runtime";

const a = new Galois128(0xdeadbeefn);
const b = new Galois128(0xcafef00dn);

const sum     = fieldAdd(a, b);
const product = fieldMul(a, b);
```

### 3.2 Hash commitments

Pin and assess the exact revision before any deployment decision.

```rust
use volar_common::hash_commitment::commit;
use sha2::Sha256;

let nonce = [0u8; 32]; // ALWAYS generate freshly per commitment
let c     = commit::<Sha256>(b"my message", &nonce);
```

The TypeScript equivalent is `commit(message, nonce)` from
`@portal-solutions/volar-runtime`.

> **Operational note:** A hash commitment is binding only as long as
> `nonce` is unpredictable and unique per commitment. Generate it from a
> CSPRNG; never reuse it across commitments to different messages.

### 3.3 Building a VOLE ZK proof of a circuit (Unpinned, Very unstable)

This legacy-Experimental construction is treated as Unpinned and Very unstable.
The integration shape is given so you can prototype, but **do not deploy**
without the required external review and documented reclassification.

The end-to-end flow is:

1. Express your statement as a boolean circuit (`BIrBlocks`).
2. Run `weave_vole_prover` to generate a prover circuit (`IrModule`).
3. Run `weave_vole_verifier` to generate the verifier.
4. Compile both with `volar-compiler` to your target (Rust dyn or TS).
5. At runtime, the prover runs the prover circuit on its witness and
   sends the proof messages over a transport. The verifier runs the
   verifier circuit and checks the result.

```rust
use volar_weaver::vole::{weave_vole_prover, weave_vole_verifier};

let prover_module   = weave_vole_prover(&boolean_circuit, "my_proof", None);
let verifier_module = weave_vole_verifier(&boolean_circuit, "my_proof", None);

// emit Rust dyn for both, or TS, or C — same as any IrModule
let prover_rust   = volar_compiler::print_module_rust_dyn(&prover_module);
let verifier_rust = volar_compiler::print_module_rust_dyn(&verifier_module);
```

For the cryptographic background see [vole-weaving.md](vole-weaving.md).

### 3.4 Wrapping a side-effecting interaction with `volar-channel`

The `volar-channel` crate is the right abstraction whenever you have a
two-party interactive protocol and want to keep the transport pluggable.
It is `no_std + alloc` and has no dependencies of its own.

```rust
use volar_channel::{Protocol, Yield};

struct MyClient;
struct MyServer;

impl Protocol for MyClient {
    type State    = ClientState;
    type Incoming = ServerMsg;
    type Outgoing = ClientMsg;
    type Done     = Result;

    fn step(state: ClientState, incoming: ServerMsg)
        -> (ClientState, Yield<Result, ClientMsg>) { ... }
}

// drive both sides locally for testing:
let result = volar_channel::run_protocol::<MyClient, MyServer>(
    initial_client_state,
    initial_server_state,
    initial_client_msg,
);
```

`run_protocol` is the simplest possible driver. In production you supply
your own driver (a TCP loop, a WebSocket loop, a request/response RPC
handler, …). Because `step` is pure, the same `Protocol` implementation
runs over any transport without modification.

### 3.5 Using ORAM (Unpinned, Very unstable)

The `volar-oram` crate provides Recursive Path ORAM with deterministic
eviction.

```rust
use volar_oram::{OramClient, OramTree};

const Z: usize = 4;       // bucket size
const B: usize = 32;      // block byte count
const L: usize = 16;      // tree height

let mut tree   = OramTree::<Z, B>::new(L);
let mut client = OramClient::<Z, B>::new(L, &mut rng);

let value = client.access(addr, ReadOrWrite::Read,  None,        &mut tree, &mut rng);
client.access(addr, ReadOrWrite::Write, Some([1u8; B]), &mut tree, &mut rng);
```

For interactive use over `volar-channel`, see `OramAccessProtocol` and
`RecursiveOramProtocol` in `volar-oram/src/lib.rs`. For circuit
integration via the FHE weaver, see [agent-context/oram.md](agent-context/oram.md).

---

## 4. Pinnedness and Stability Hygiene for Integrators

Integrators inherit the evidence and dependent contract of every Volar module
they use. Practical rules:

1. **Pin the commit**, not the version. Volar is pre-1.0; pinnedness and
   stability may change between commits. Pin to a specific revision and review
   the diff before bumping it.
2. **Read both `// @pinnedness` and `// @stability` markers and their evidence
   records.** An Unpinned component has no complete external binding; a Very
   unstable component is not a general deployment dependency.
3. **Treat legacy `@reliability: experimental` as Unpinned and Very unstable.**
   A legacy `normal` or `hazmat` marker is not an automatic paper, review,
   proof, or stability claim.
4. **Read the `# Safety` section of every Hazmat module you call.** The misuse
   condition is the security property you must enforce at every call site.
5. **Never link an `.insecure` file.** Its extension keeps it out of Rust
   module discovery; do not move or rename it into use.

For applications that can defer cryptographic deployment, the safest posture
today is to use Volar for prototyping, vendor and audit the exact subset you
need, and make an evidence-led downstream assessment. Do not infer deployment
readiness from a legacy marker, an AI marker, passing tests alone, or generated
output.

---

## 5. Versioning and Stability Promises

The explicit `@stability` marker, not a legacy reliability label, defines a
component's contract:

| Stability | Integrator expectation |
|---|---|
| `forever` | Intended to remain usable indefinitely; it must also be Proven. |
| `stable` | Changes are exceptional and include a migration path. |
| `semver` | Compatibility follows semantic versioning; breaking changes require a major version. |
| `unstable` | Expect evolution and plan for adaptation. |
| `very-unstable` | Expect replacement or substantial adaptation; do not use as a general deployment dependency. |

Generated TypeScript and C track their pinned input revision. Generated C is
currently for testing. Until a component carries an explicit stability marker,
treat it as Unstable under the migration policy.

---

## 6. Common Integration Mistakes

### Mistake: Using `Galois128` for AES-related field arithmetic

`Galois128` uses the **GCM** reduction polynomial, not the AES one. AES
operates over `Galois` (GF(2⁸)). If your code mixes these up the maths is
silently wrong.

### Mistake: Reusing nonces in `commit`

Hash commitments are binding only with a uniformly random per-commitment
nonce. Reuse leaks plaintext relationships. Generate a fresh nonce per
commitment from a CSPRNG — `OsRng::default().fill_bytes(&mut nonce)`
is the canonical Rust pattern.

### Mistake: Importing from `volar-spec-dyn` in Rust code that has access to `volar-spec`

`volar-spec-dyn` exists for the case where lengths are not known at
compile time. If your application can use type-level lengths, use
`volar-spec` directly: it has stronger compile-time correctness and
better codegen.

### Mistake: Calling spec functions in tight TS loops without batching

The TS runtime mirrors the spec faithfully but does not auto-batch.
Field operations on small types are cheap; field operations on
`Galois256` are not. Batch where you can; profile before optimising.

### Mistake: Using `run_protocol` in production

`run_protocol` is a local driver intended for testing both sides of a
`volar-channel` protocol in the same process. Production code should
implement its own driver that moves messages across the actual
transport.

### Mistake: Reclassifying a dependency in your own fork without evidence

If you fork Volar and change an Unpinned/Very unstable or legacy marker to make
it appear deployable, you now own the evidence and dependent-contract argument.
Keep Volar's markers and evidence records, add your own review artifact, and do
not erase the migration signal.

---

## 7. Telling Volar About Your Application

If you are building something on Volar and want it tracked in the
project:

1. File an issue describing the use case.
2. Note which crates and which pinnedness/stability markers your application
   depends on.
3. If the application needs a less volatile contract or stronger evidence for a
   dependency, that is a signal to prioritise its review or proof work.

This feeds the project's classification planning: the constructions that real
applications depend on get evidence and stability work first.

---

## 8. Related Documents

- [overview.md](overview.md) — workspace layout and pipeline.
- [reliability.md](reliability.md) — pinnedness, stability, AI markers,
  legacy migration, and reclassification protocol.
- [spec.md](spec.md) — full `volar-spec` reference.
- [compiler.md](compiler.md) — `volar-compiler` reference.
- [vole-weaving.md](vole-weaving.md) — how the VOLE ZK weaver works.
- [garbling-pipeline.md](garbling-pipeline.md) — how the garbling weaver
  works.
- [insecure.md](insecure.md) — what the `.insecure` extension means.
- [text-format-spec.md](text-format-spec.md) — IR text formats for
  serialised artefacts.
