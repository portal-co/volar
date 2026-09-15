# TinyLabels as an opt-in input-delivery mode: Volar and Cirrus plan

**Status:** design and research plan, 2026-09-14.  This document does not
change the default transport, label width, or embedded deployment profile.

## Decision

TinyLabels is worth pursuing as a **separate, opt-in input-label-delivery
module** for large, highly reused server-class workloads. It must not become:

- a replacement for 128-bit garbling labels;
- a durable-material encoding or an AES-block packing technique;
- the default `OtChannel` path;
- a dependency of `no_std`/embedded Cirrus ERT deployments; or
- an implicit selection made by a circuit compiler.

The baseline remains direct selected-label delivery through the existing OT
seam (including the Ferret-backed channel). TinyLabels has a much larger
Ring-LWE preprocessing, memory, storage, framing, and review cost. It is
selected only by an explicit session profile negotiated before a transcript
starts.

## What TinyLabels changes

TinyLabels compresses **delivery of selected external input labels** in an
offline/online setting. It does not compress:

- AND-table streams;
- labels retained as durable held state;
- evaluator-derived intermediate labels; or
- the cryptographic width of a wire label.

For a fixed garbling and common free-XOR offset, its reusable `enc1` material
covers the label-difference vector; `enc2` covers a per-use zero-label vector;
a later selection/key stage produces the selected labels. This creates a
resource-heavy alternate transport for input labels, not a new `Garble<U16>`
format. [DLL24, Construction 3]

## The deep module and its seam

Introduce one deep module, conceptually named **`InputLabelDelivery`**, at the
point where a session needs a public ordered batch of externally supplied
selected labels.

Its interface must be intentionally small:

```text
prepare(profile, ordered wire manifest) -> reusable handle
begin_use(reusable handle, transcript binding) -> use handle
receive_selected(use handle, role-owned choices) -> ordered active labels
metrics() -> public transfer and resource counters
```

The interface's invariant is stronger than its types: output position `i` is
exactly the active label for the same schedule/input-manifest wire `i`, with no
reordering, omission, Boolean decode, or label conversion visible to the
caller. A direct-OT adapter and a TinyLabels adapter satisfy the same seam.
The TinyLabels implementation owns field encoding, staged messages, bounded
frame reassembly, parameter fingerprints, replay protection, and its failure
policy. Callers should not learn any of those details.

This seam must be batch-oriented. Retrofitting the Ring-LWE protocol behind
one `OtChannel::receive(bit)` call would make a deep implementation look like a
shallow per-bit adapter and would conceal unacceptable setup/latency costs.

## Volar work plan

### V1 — manifest and batched delivery seam

1. Add a public, canonical `InputLabelManifest` to the strict split/chain
   runner. It contains a session identifier, circuit/schedule digest, role,
   ordered input positions, label byte width, and expected count. It contains
   no labels or choices.
2. Refactor evaluator-owned external inputs so one runner call can request a
   contiguous manifest batch through `InputLabelDelivery`; retain the current
   per-bit `OtChannel` code as the default adapter.
3. Bind every delivery transcript to the strict session transcript plus
   manifest digest, direction, batch number, and a monotonic use counter.
   Reject a mismatched profile, stale counter, duplicate manifest, or label
   length before evaluation begins.
4. Add differential tests: direct OT and a deterministic test delivery adapter
   must yield identical active-label sequences and identical revealed circuit
   results for the same schedule/choices. This tests the seam without making a
   cryptographic TinyLabels claim.

### V2 — server-only TinyLabels adapter

1. Reuse the separately reviewed Cirrus arithmetic implementation only after
   its field encoding, framing, sampler, and interoperability gates below are
   complete; do not copy unreviewed Ring-LWE logic into `volar-mpc`.
2. Provide an adapter behind a `std` and explicit `tinylabels` feature. Its
   constructors must demand an explicit profile and resource budget. There is
   no default constructor and no embedded feature path.
3. Define canonical frames for public parameters, reusable ciphertext,
   per-use ciphertext, selection/key material, and completion/error. Every
   frame has a version, parameter fingerprint, stage, exact element count,
   session/manifest binding, and authenticated length. Raw SEAL NTT dumps are
   prohibited.
4. Add host-only interoperability tests against the pinned author artifact at
   a small or streamed profile, then a separately gated reference-profile
   test. Do not put the artifact's multi-gigabyte reference ciphertext in the
   repository or CI.

### V3 — measurement and admission policy

For both direct `NetOtChannel`/Ferret and TinyLabels, publish the same public
measurements: setup bytes, reusable-storage bytes, per-use bytes, online
rounds, peak heap, CPU time, selected-label count, and table bytes. Require
an explicit break-even calculation by workload reuse and bandwidth; never
activate TinyLabels based only on label count.

## Cirrus work plan and port map

Cirrus already has the correct ownership direction: the
`cirrus-garbled-circuit-tinylabels` crate owns input-label batching separately
from the four-row and first-row-fixed table formats, and the ERT/LLVM
interpreters stream tables through `Pusher` and consume them through ordered
iterators. The port should deepen those existing modules rather than import
Volar's strict-session types.

| Recent Volar work | Cirrus equivalent / action |
| --- | --- |
| Shared AES material batches and `MaterialBlockCachePlan` | No direct port. Cirrus does not currently have split-key AES durable held-material storage. Add nothing until a Cirrus role-local persistence protocol exists; TinyLabels must not be repurposed as material storage. |
| Deferred opaque rebase | Add an explicit helper at `cirrus-volar-garble`'s label seam: garbler false base XOR fresh public-zero base, evaluator active label XOR its matching zero label. It must be allocation-free, free-XOR-only, and have a streaming/interpreter round-trip test. It is a base rebind, never compression. |
| Ferret `OtChannel` and metrics | Cirrus already exercises the upstream LWE → SoftSpoken → Ferret stack in `cirrus-volar-vole/tests/ferret_stack.rs`. Extract its tagged framed `StackIo` adapter and public byte/frame counters into one host-side transport module, then use it for both VOLE and future direct input-label delivery. Keep `miniot`'s ML-KEM code separate; it is a different OT construction. |
| Material/chain metrics | Define a Cirrus `StreamingMetrics` value at the `Pusher`/iterator seam: table records/bytes, input-label records/bytes, Ferret frames/bytes, and peak buffered records. Do not make interpreter contexts own network metrics. |
| Strict-chain storage boundaries | ERT/LLVM fixed interpreter traces already provide an explicit topology-preprocessing boundary. A future persistence adapter must live outside the interpreter, consume opaque role-local labels, and preserve its streaming table contract. |

### C1 — finish the existing TinyLabels crate safely

The present crate has typed `setup`/`enc1`/`enc2`/`keygen`/`dec` arithmetic but
correctly has no deployment protocol. Complete these gates in order:

1. Specify and test a canonical, injective `[u8; 16] <-> Z_p^3` encoding with
   explicit endianness, field bounds, and exact inverse. Prove/test that it
   reconstructs labels byte-for-byte; it does not shorten them or expose the
   free-XOR offset.
2. Add versioned frame codecs and bounded streaming reader/writer adapters for
   each TinyLabels stage. Frames are independent of `GarbleTable` records and
   work with the coroutine `Pusher`/`Puller` backpressure model.
3. Integrate a reviewed CSPRNG and exact clipped discrete-Gaussian sampler;
   document the decryption-failure budget, secret-side constant-time
   requirements, and zeroization. `ZeroNoise` stays test-only.
4. Add a semantic interoperability harness against the author artifact,
   preserving the local correction that samples Construction 1's `r` values
   instead of reproducing the artifact's apparent omission.
5. Run a resource-admission study. The published reference profile's raw
   public parameters are about 34 MB and reusable `ct1` about 2.55 GB, far
   beyond Cirrus's 256 KiB RAM/2 MiB flash target. A full profile may run only
   at a server/offline coordinator; an embedded participant needs a separately
   proven bounded streaming role or is rejected.

### C2 — interpreter and streaming integration

1. Complete the planned private `MachineHandler` host-operation seam so ERT
   and LLVM interpreters identify a stable ordered external-input manifest.
   TinyLabels cannot repair symbolic branches or unknown topology.
2. Use the existing fixed ERT workload to preprocess and stream tables once.
   Only true external inputs enter the TinyLabels manifest; intermediate
   labels remain evaluator-derived.
3. Add one coroutine-backed test that streams table records and input-label
   frames concurrently with a bounded buffer. It must demonstrate backpressure
   and exact order without collecting the multi-megabyte table stream in a
   `Vec`.
4. Preserve the baseline and first-row-fixed evaluator tests. TinyLabels is an
   input-delivery adapter, not a third garbling-record format.

## Security and deployment gates

TinyLabels remains experimental until all of the following are complete:

- an independent cryptographic review of the label encoding, Ring-LWE
  parameterization, sampler, failure probability, and complete transcript;
- authenticated framing and session/circuit/manifest binding;
- explicit choice ownership and malicious-party threat analysis;
- reproducible interoperability and malformed-frame tests;
- server and target-device resource measurements; and
- an explicit product opt-in, with a fallback to direct OT when admission
  fails.

No current embedded Cirrus profile may select TinyLabels. No current Volar
session should expose it by default.

## Sources

- **[DLL24]** Marian Dietz, Hanjun Li, and Huijia Lin, *TinyLabels: How to
  Compress Garbled Circuit Input Labels, Efficiently*, IACR ePrint 2024/2048,
  especially Construction 3 and the parameter/evaluation discussion.
  https://eprint.iacr.org/2024/2048
- Cirrus's existing primary-source implementation review and artifact pin:
  [`../../../cirrus/crates/garbled-circuit/cirrus-garbled-circuit-tinylabels/RESEARCH.md`](../../../cirrus/crates/garbled-circuit/cirrus-garbled-circuit-tinylabels/RESEARCH.md)
  (path relative to this checkout).