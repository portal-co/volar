# Plan: Secret-shared Volar IR for multi-party computation

**Status:** revised plan with user decisions locked; protocol implementation remains unadmitted.<br>
**Pinnedness:** unpinned. **Stability:** very unstable.<br>
**Scope:** secret-shared Volar IR values, beginning with semi-honest `n=2, t=1`. Not a weaver and not Boolar IR.

## Executive summary

Use a new **secret-shared Volar IR value model** with a compiler transform whose emitted program is still ordinary Volar IR: values are normalized to one binary extension field `GF(2^k)`, local share operations remain local, and only genuinely interactive protocol steps become explicit Volar IR actions. Do not bit-blast through Boolar or generate a per-party weaver. Source external actions and RNG remain explicit exceptions; storage does not.

The first security profile is computational semi-honest two-party computation with `n=2, t=1`, using two-out-of-two secret shares and threshold key participation `q=t+1=2`. Every party, including the designated storage party, is an ordinary MPC participant and may collude within the declared corruption threshold. Malicious security with abort is a possible later profile, not a property or implementation goal of this phase; stronger models are out of scope.

The field-lowering feature pulled from `main` is the starting seam: `lower_to_native` can emit Volar IR over an `ExtField` rather than Boolar. The selected native field must be a nontrivial binary extension `GF(2^k)`, not `Bit`/`GF(2)`, because the latter is the Boolar sink. The current pass is not yet sufficient: it panics on storage, oracle/action, and RNG statements, so this plan requires an exhaustive field-only lowering/rewrite for every in-scope Volar IR operation.

For storage, use **MPC over one encrypted server-held ORAM tree**: the storage party owns the ciphertext store and also participates in the same MPC; it may see the oblivious physical path and ciphertext, but not logical addresses, plaintext blocks, or the combined key. Reuse the existing symbolic Path-ORAM + in-MPC encryption architecture as the correctness baseline, then benchmark Circuit ORAM as the best fit for reducing MPC circuit/nonlinear work. DORAM schemes such as Duoram are highly efficient recent alternatives when memory is secret-shared across multiple servers, but that placement is not the requested single-store model. Fast schemes that deliberately reveal logical access patterns to one party are excluded.

Protocol context is hidden from output Volar IR. Synchronous action execution binds to a session context carried by native thread/coroutine state and hidden ABI parameters/returns; the emitted Volar IR contains only field values and action calls. Sessions bind all frames, preprocessing, storage epochs, and replay counters to a fresh salt. Any offline party causes immediate abort. Resume/recovery is disabled by default and permitted only for the semi-honest profile after explicit external confirmation and enough threshold shares; otherwise restart. With `n=2,t=1`, one remaining party cannot reconstruct a two-party threshold state, so a single offline party cannot be recovered from without the missing party or an authorized replacement share.

## Goals and non-goals

### Goals

- Start with the locked computational semi-honest profile `n=2,t=1`; make the share/key threshold `q=t+1=2` and permit later party-count profiles without changing the Volar IR output format.
- Normalize every in-scope data operation to one selected binary extension field `GF(2^k)` using Volar IR → Volar IR lowering. Preserve Volar IR control flow and ordinary Volar IR output.
- Keep local share operations local; add explicit Volar IR protocol actions only for interaction, preprocessing, ORAM access, key setup, or authorized openings.
- Treat every storage access as oblivious and encrypted, with one predetermined data-store party that is also an MPC participant and has no privileged protocol role.
- Carry protocol state in native thread/coroutine state and hidden ABI parameters/returns, not in output Volar IR values or types.
- Use synchronous actions and separate-process IPC tests; bind sessions and transcripts to a fresh salt.
- Specify immediate abort and the narrow, explicitly confirmed semi-honest recovery path; require all proof, review, and test gates before a security claim.

### Non-goals

- Do not repurpose Boolar, Yao/garbled circuits, or generated weavers as this IR model. They may remain independent baselines, but the new lowering must not route through them.
- Do not implement or claim malicious security in this phase. Malicious security with abort may be proposed later; stronger security is not a target.
- Source external actions and source RNG are exempt from the all-operations-to-field rule. No other operation—including storage, comparisons, selects, or control-dependent data operations—is silently exempted.
- Do not claim ciphertext alone hides access patterns, that semi-honest tests prove malicious security, or that a field-only type table proves protocol correctness.
- Do not claim recovery from fewer than `q=t+1` available shares, guaranteed output delivery, fairness, or a production network transport.
- Do not expose a general plaintext opening as an optimizer convenience. Openings are explicit protocol operations authorized by the output/leakage policy.

## Current repository state and the gap

The intended seam is informed by these local components; they are useful inputs, not proof that the new shared-field path exists:

- `MPC_PLAN.md` and `volar-mpc` contain the existing semi-honest two-party garbling/OT execution path. It is not the new secret-sharing model.
- `volar/docs/mpc-external-actions-oracles-plan.md` and its ledger define source external-action policy/identity/order. Preserve those declarations and policies; do not conflate a source action with a protocol multiplication or ORAM action.
- `GRAM_SYMBOLIC_ORAM.md`, `volar/docs/agent-context/oram.md`, and `volar/crates/vc/volar-vc/src/oram_2pc.rs` show that **single-store ORAM plus in-MPC encryption is viable in the current two-party GC implementation**: the host holds an encrypted tree; ORAM position-map/stash state stays secret; the physical path is revealed and its ciphertext path is read/written. This is a valuable algorithmic/conformance baseline, not the new IR implementation.
- The current symbolic ORAM uses Path ORAM and an AES-128 per-node pad. The checked-in driver is a single-process/loopback harness; `SharedOramKey` is 64/64 two-role material, not a general `t+1` threshold key. The encrypted-valid/versioned-pad modes are not supported by `new_with_shared_key`, and the keyed-leaf/encryption defaults still need to be wired into real role-local execution. Those are explicit porting gaps.
- `volar-ir/crates/ir/volar-ir-passes/src/lower_to_native.rs` is the `main`-branch Volar IR → Volar IR binary/extension-field seam. For an `ExtField` native sink it emits IR; for `Bit`, it emits Boolar. Its present `lower_stmt` handles `Const`, `Transmute`, `Poly`, rotations, `Merge`, `Splat`, and `Shuffle`, and its terminator support is limited; it panics on storage, oracles, actions, and RNG. The new pipeline must close these gaps for all in-scope operations and explicitly preserve the two user-approved exceptions.
- The current native field and extension-field implementation in `volar-ir/docs/primefield-plan.md`, `extfield-plan.md`, and `ir-lowering.md` can supply the field semantics. The native field width/polynomial still needs a profile decision and benchmark.
- Current `LirTarget::action` is a stateless compiler-builder hook. Do not put context values into output Volar IR to repair this; inject a hidden native runtime context/ABI and thread it across native functions/coroutines.

The architecture is therefore: **field-normalize Volar IR → add local share operations and protocol/ORAM actions while retaining Volar IR → execute through a runtime with hidden session state.** It is neither an extension of `GateSchedule` nor a new Boolar/weaver path.

## Proposed pipeline

```text
source / imported Volar IR
        │
        ├── classify only source ActionCall and RNG as explicit exceptions
        ▼
field-only Volar IR normalization (native ExtField = GF(2^k))
        │  all in-scope data operations, including storage addresses/values,
        │  are represented using the one selected binary extension field
        ▼
secret-share analysis + local-operation rewrite
        │  compile-time side tables bind IR values to share domains/party roles
        ├── local field/share operations stay ordinary Volar IR
        ├── multiplication/interactive primitives become protocol ActionCalls
        └── storage is rewritten to ORAM actions plus external encrypted path I/O
        ▼
ordinary Volar IR output
        │  native values are GF(2^k); explicit action declarations/calls;
        │  no SharedContext type/value, no Boolar, no weaver
        ▼
existing Volar IR → LIR/native codegen
        │  hidden native ABI parameter carries task/session context;
        │  sync protocol runtime, IPC/network transport, designated data store
        ▼
per-party execution and policy-authorized output reconstruction
```

The new model is a compiler analysis/transform over Volar IR values, not a second serialized IR schema. Its emitted artifact remains Volar IR with the added protocol actions. Public compile-time manifests may carry party/share/storage policy, while runtime secrets and context remain outside Volar IR.

## Domain model and invariants

### Parties and protocol profiles

The initial `MpcProfile` is locked to computational semi-honest two-party MPC with `n=2,t=1`, an authenticated synchronous channel, and abort on any missing party/message. Its secret-share and storage-key reconstruction quorum is `q=t+1=2`. Each profile manifest binds party IDs/order, designated store party, binary extension field/polynomial, protocol and preprocessing versions, source external-action policies, storage geometry, session salt, and public leakage. It makes no fairness, guaranteed-output, or malicious-security claim.

For later `n>2` profiles, keep `n`, `t`, and `q=t+1` explicit; do not assume the two-party protocol or its key setup automatically generalizes. Any unsupported `(n,t,field,operation)` combination fails validation before execution. No pass infers a party, threshold, or executor from source symbols, secret values, or addresses.

### Shared values

The compiler's nonserialized `SharedValuePlan` should bind each logical `IRVarId` to a local field share and its profile. The output IR's *data* types are exclusively the selected `ExtField(Bit, k, irreducible)` plus structural/control types; it does not add `SecretShared`, `PartyShare`, or runtime-context `IrType` variants.

Threshold sharing supplies a distribution mechanism, not secure evaluation by itself; the selected scheme and access structure determine which operations are valid [Shamir79] [CDM00]. For the initial `n=2,t=1` profile, each logical field element is additively/XOR-shared as `x = x_0 + x_1` in `GF(2^k)`. Either one share alone is statistically independent of `x`; reconstructing requires both shares. Local field addition is share-local. The current multiplication candidate is fresh field Beaver triples from the common semi-honest OT/COT/VOLE provider fixed in [ADR 0002](adr/0002-semi-honest-pq-cot-ferret.md). The exact field-triple equations and provider composition still need a paper-bound security argument; some characteristic-two OT-triple constructions add active MAC/checking and must not be adopted unchanged. A direct semi-honest OT multiplication remains a correctness oracle, not a per-protocol provider alternative.

The internal plan records the source type, normalized field wires, owner/party binding, storage classification, and action dependencies so the verifier can reject accidental reconstruction, unapproved openings, cross-profile mixing, and a party program receiving another party's local share. Public constants are injected using the exact additive-sharing convention (for two parties, inject into one designated share), not copied into both shares.

### Sharing representation and operation capabilities

The initial algebra is `GF(2^k)`: field addition is XOR; field multiplication is the binary-field product. Use the `ExtField` native sink (not `Bit`) so `lower_to_native` returns Volar IR rather than Boolar. The exact degree and irreducible polynomial remain a benchmark/security-profile parameter: compare at least 64- and 128-bit fields, bind one canonical polynomial, and do not describe a 64-bit field as providing 128-bit statistical security.

The field normalizer must cover every in-scope value statement, CFG/block argument, branch condition, return, and storage operation. It must preserve exactly two source-operation exceptions—external `ActionCall` and RNG—without attempting to implement their bodies as field arithmetic. This is an operation exception, not a way for old-width clear values to leak into the shared computation: arguments/results crossing either boundary must use explicit typed field conversion, input-sharing, or authorized-opening actions, and every result consumed by shared operations must enter as a valid share. Storage is not an exception: logical address, write data, read result, and ORAM state are normalized to field values; raw indexed `StorageRead`/`StorageWrite` are replaced by the oblivious storage protocol. A pure `OracleCall` is not an exception: inline or field-lower its implementation; if there is no field-compatible implementation, reject the module rather than silently leaving the oracle outside the shared-field computation.

For the initial additive sharing, field additions, XOR/linear combinations, and public scalar operations are local. Field multiplication, input/output sharing, authorized opening, and ORAM's secret-dependent comparisons/selects that lower to multiplication use protocol actions as required by the selected implementation. The capability table is exhaustive over Volar IR variants; unhandled statements fail at compile time, not by falling back to clear evaluation.

“Homomorphic operation on shares” means evaluating the field operation on the distributed representation without revealing the logical operands. It does not mean homomorphic encryption.

### Shared IR operations and action insertion

The transformed program remains Volar IR. Compile-time analysis tracks which field values are local shares; ordinary `Poly`, `Merge`, `Shuffle`, branches, and other supported statements compute over those shares. Add versioned, typed `ActionDecl`/`ActionCall` protocol operations for field multiplication/triple consumption, input sharing, output reconstruction, and ORAM protocol boundaries. Each protocol action binds operation/profile ID, session salt/digest, participating party IDs, exact field input/output widths, dependencies, sequence number, and allowed masked-value openings.

Do not confuse these with source `ActionCall`s, whose declared executor/reveal policy and guarded fallback semantics are preserved as source behavior. Source RNG remains independently sampled and is never CSE'd; when its result enters the shared computation, a declared protocol action must convert it into a proper shared field value. The compiler does not silently treat a single party's clear RNG output as a valid secret share.

The analysis sidecar retains source-provenance mappings and operation classifications, but no sidecar or context token is serialized as an IR value. Every action's effect/order and public transcript is validated against the manifest.

The field normalizer and share/action transform operate on Volar IR and produce Volar IR:

1. Validate the source module, enumerate every statement/terminator variant, and classify only source external actions and RNG as exceptions.
2. Normalize every in-scope data value and storage interface to the selected native `ExtField`; prove no legacy integer/bit data type remains in the output type graph.
3. Build the `SharedValuePlan` for each local party and attach protocol/profile/action manifest bindings outside the IR value graph.
4. Fold and simplify field arithmetic while preserving source action effects, independent RNG occurrences, protocol action sequence, ORAM physical transcript, and storage epochs.
5. Keep local additions/linear operations as plain Volar IR; insert protocol actions only for operations that actually need interaction/preprocessing. A shared field addition must add zero interactive actions.
6. Convert each logical storage op to a fixed-shape ORAM operation: reveal only an oblivious physical path, read encrypted path ciphertext from the designated data store, process/evict/decrypt/re-encrypt inside MPC, and write ciphertext back.
7. Preserve explicit source-action policies and boundaries. No protocol optimizer may CSE source actions or RNG. Pure oracles must be implemented/inlined into the field computation or rejected; they are not an additional exception.
8. Verify every output is a valid field-only Volar IR module with no raw direct storage op, unapproved clear opening, missing protocol action, or unsupported statement variant.

Use a clear Volar IR evaluator plus a small-field share reference model and independent protocol oracle. Test semantic equality and action minimality, and check the emitted artifact is ordinary Volar IR, not Boolar, a circuit, or a per-party generated weaver.

## Hidden native protocol context

The user requirement is to carry opaque protocol context through action-to-LIR lowering while keeping it hidden from output Volar IR. **Do not serialize the context as a Volar IR value/type or expose it in source signatures.** The LIR/native backend may carry it as a backend-private opaque ABI parameter/return or task-frame field; generated Volar IR itself remains field values plus explicit action calls.

The synchronous native runtime owns a per-session `ProtocolContext` containing the local party identity, session salt/manifest, IPC/transport endpoint, ordered transcript counter, preprocessed triple cursor, threshold-key share handle, ORAM host/store handle, and recovery/checkpoint state. Native function lowering preferably introduces a hidden ABI context parameter and threads it through native function calls/returns; a coroutine stores it in its hidden task frame, and native threads receive their own context/task handle. A hidden global model is acceptable only if it resolves the correct per-session/per-thread-or-task context; do not use one unkeyed process-global singleton that can cross-wire concurrent sessions. Context remains absent from Volar IR types, SSA values, source signatures, and serialized IR.

`LirTarget::action` can remain the ordinary action-emission hook; its backend implementation emits a synchronous runtime call that obtains the hidden context from the native ABI/task frame. Any extra context parameters are backend-private ABI, not IR. The runtime must enforce action order and bind each generated action call to its public operation/profile ID and to the active session salt/transcript counter. A context-free native target rejects MPC action declarations rather than executing them as ordinary host externs.

Tests must cover direct and nested function calls, returns, loops, worker threads, coroutine suspension/resumption, two concurrent sessions, abort unwinding, and serialization/replay of IR without serializing credentials or runtime context. Output IR should round-trip unchanged except for explicit field-normalization and action statements.

## Mandatory oblivious encrypted storage

Every storage declaration processed by this lowering must bind an immutable public policy before transformation:

```text
StoragePolicy {
  storage_id,
  designated_storage_party,
  public_geometry_and_epoch,
  oblivious_access_protocol,
  encrypted_block_profile,
  multi_party_key_policy,
  integrity_and_rollback_policy,
  public_leakage_profile,
}
```

### Designated storage party

- Select one party as the physical owner of the data store before execution and bind it into the public manifest. This is only data placement: that party participates in every relevant MPC action as an ordinary party, has the same share threshold and corruption status as peers, and receives no helper/oracle/trusted-client privilege.
- It stores the encrypted ORAM tree and serves the revealed physical path reads/writes. The physical access trace and ciphertext may be visible to that party (and may be public to all participants); the logical address, read/write data, position map, stash, and key remain secret-shared. Public access count/geometry/timing are declared leakage.
- The storage party may collude with other MPC parties, subject only to the initial `t=1` corruption bound. At `n=2`, collusion of both parties is outside the privacy guarantee; no “non-colluding storage server” assumption is added.
- Every `StorageRead`/`StorageWrite` must be rewritten into this ORAM path protocol. There is no direct logical indexed storage access, plaintext initialization shortcut, or separate storage server protocol role.
- Encryption hides contents; ORAM hides logical access patterns. Both are required. The physical path is intentionally revealed only because it is the ORAM's oblivious trace.

### Threshold key and ciphertext formats

The storage key is a `q=t+1`-of-`n` secret, where `t` is the declared number of semi-honest colluding parties. For the first profile `n=2,t=1,q=2`: both key shares are required; neither party alone, including the storage party, can reconstruct the key. The normal protocol evaluates AES/PRF and any authentication inside MPC with key shares; it does not materialize the combined key at the store.

Use the existing encrypted Path-ORAM data path as the porting baseline: ciphertext is an AES-128 keyed pad over the whole physical entry, including `valid`, tag/address, leaf, and payload. Its current two-party `SharedOramKey` split is only 64/64, and its shared-key adapter rejects `encrypt_valid` and `versioned_pads`; it is not yet the requested threshold-key implementation. The new path must put formatting, whole-entry encryption, key generation/derivation, and any persistent-version handling inside the shared-field/MPC path. The session salt, storage ID, epoch, physical slot, and version are domain-separated in pad derivation; physical-slot writes must not reuse a pad. Semi-honest profile does not claim malicious ciphertext integrity, but the format must still fail closed on malformed geometry and support replay-safe session binding.

The initial q-of-n key setup is a protocol decision, not just `XOR`ing key strings. Compare a threshold symmetric-encryption/distributed-PRF construction with MPC evaluation of AES under additive field shares [DiSE18] [ParaDiSE22]. DKG/resharing is only useful if its exact key type and assumptions match; it does not by itself provide symmetric encryption or ORAM. No party reconstructs a reusable key in ordinary execution. Any recovery that reconstructs state/key shares is an explicit externally authorized exception, audit event, and re-sharing/key-rotation boundary.

### ORAM scheme research and choice

The requested deployment is accurately described as **single-store MPC-over-ORAM with an MPC-executed ORAM client and threshold-encrypted server tree**, not necessarily DORAM. The storage owner holds the physical tree but is one of the computation parties; the ORAM client state and cryptographic operations are shared. This is the same broad architecture already prototyped locally with two-party garbling, now to be expressed as field-only Volar IR actions.

| Scheme | Evidence / efficiency signal | Fit to this plan |
|---|---|---|
| Existing local Path ORAM + in-MPC encryption | Correctness and two-party GC execution already exist in `oram_gadget.rs` / `oram_2pc.rs`; the current local client state is hidden and only the oblivious physical path/ciphertext crosses the store boundary. | **Best porting baseline**, because it matches one encrypted store and already has a local reference. It is not the expected final performance point: Path ORAM was optimized for bandwidth/client storage, not MPC circuit cost. |
| Circuit ORAM | Designed explicitly to minimize circuit complexity for secure computation. Its reported 58.4× reduction is in non-free gates against a particular straightforward Path-ORAM implementation; variants/parameters change the ratio [CircuitORAM]. | **First single-store performance challenger.** Its client computation can run over field shares while the designated store serves ciphertext paths. Validate encrypted-entry and threshold-key compatibility; its published Boolean-circuit cost is not a benchmark of our `GF(2^k)` action runtime. |
| SCORAM | Studies ORAM circuit size specifically for secure computation and reports substantial practical improvements over earlier circuit constructions [SCORAM]. | Include as a second 2PC-oriented baseline. It is not automatically compatible with our one-store ciphertext format or native shared-field operations. |
| Duoram (USENIX Security 2023) | Practical 2- and 3-party semi-honest DORAM with one passive corruption; uses preprocessing and low online communication. The database is additively shared between computation parties [Duoram]. | Strong DORAM comparison if relaxing the one physical-store requirement. The share-distributed database is not a direct replacement for one party owning an encrypted tree, and its protocol/preprocessing does not automatically generalize to arbitrary `n`. |
| GigaDORAM (USENIX Security 2023) | Three-server DORAM for large memories; reports over 700 queries/sec at `2^31` addresses [GigaDORAM]. | Large-scale multi-server comparison, but requires three storage/compute servers; not the selected two-party single-store topology. |
| PRAC / 3-Party RAM-MPC (2023–24) | Recent systems work builds dynamic data structures over DORAM; a separate 3PC RAM construction reports large concrete speedups over previous DORAMs [PRAC] [ThreePartyRAM]. | Best systems comparison for a future multi-store `n=3` profile. Both change the storage topology/security assumptions and cannot be used as single encrypted-store adapters. |
| MetaDORAM / Efficient Mixed-Mode ORAM (ITC 2025 / 2025) | Recent honest-majority 3PC work improves DORAM communication; mixed-mode ORAM targets a combination of public and secret accesses [MetaDORAM] [MixedModeORAM]. | Relevant future research, not a fit for `n=2,t=1` plus one designated encrypted store. The public/secret access split must not be mistaken for hiding all accesses. |
| SOCS-ORAM / server-aided fast ORAM (2023) | Reports 13 bytes/access for a `2^30`-entry, 4-byte array [SOCS-ORAM]. | **Reject for this requirement:** its privacy/efficiency tradeoff reveals logical access information to a participant and relies on server-aided preprocessing, violating the no-privileged-storage-role requirement. |

**Conclusion and recommendation:** the local Path-ORAM + in-MPC encryption path proves the broad architecture is implementable with the existing two-party GC, but does not prove it works with the new semi-honest field-sharing protocol. The research review found no recent construction that simultaneously matches general MPC RAM, `n=2,t=1`, one designated encrypted store that is an ordinary colluding MPC party, and this field-only Volar IR contract. Port Path ORAM as the correctness baseline; compare Circuit ORAM and SCORAM under the actual shared-field backend before selecting the single-store design. If measurements or the security argument fail, revisit the storage construction rather than weakening the store-party threat model. Duoram, GigaDORAM, PRAC, Three-Party RAM, MetaDORAM, and Mixed-Mode ORAM are strong alternatives only if the system changes to distributed storage / an honest-majority profile. Do not adopt SOCS-ORAM's access-pattern leakage.

## Session, IPC, abort, and recovery semantics

- Each session receives a fresh high-entropy salt and a session/manifest digest. Bind the salt and monotonic action index to every IPC protocol frame, preprocessing batch, key derivation, ORAM storage epoch, and ciphertext tweak. The salt is one replay-binding component, not a substitute for authenticated transport, sequence checks, unique nonces, or persistent storage versioning. A retry/recovery uses a new salt and new transcript/key epoch; old frames are rejected.
- Protocol execution is synchronous. A missing party, dropped IPC frame, invalid sequence, or expired deadline aborts the active action and session immediately. Do not let remaining parties continue with a partial multiplication, ORAM update, or write-back. The storage operation must have a commit boundary so restart does not treat a half-written path as a valid ORAM state.
- Tests run every party in a separate child process and exchange bytes over OS IPC (e.g. inherited pipes or a Unix `socketpair`), not `LoopbackOt`, shared in-process role state, or TCP listeners. This tests share/key separation and framing without requiring a local network stack. Production network transport is a separate adapter.
- Recovery is disabled by default and only available when the profile is semi-honest, recovery is explicitly enabled, and an external operator/quorum confirms the recovery event. After confirmation, reconstruct only the checkpointed material needed to retry; audit the disclosure; form a new party set; re-share the checkpoint and threshold key under the new set; rotate the salt/key epoch; and resume only from the last committed action/storage boundary. Otherwise discard the session and restart from the application checkpoint.
- Recovery requires at least `q=t+1` valid shares of every secret needed. In the initial `n=2,t=1,q=2` profile, one remaining party has only one share and cannot reconstruct; one offline party therefore means abort and restart unless it returns or an independently authorized replacement share exists. External confirmation authorizes the recovery disclosure; it does not create missing shares or change the threshold.

## Initial protocol and share profile

The user has selected the initial model: computational semi-honest two-party MPC, `n=2,t=1`, binary extension field, and `q=t+1=2` required key/share parties. For the first protocol, use two-party additive sharing over `GF(2^k)`; additions are local. The current candidate uses a fresh triple `[a],[b],[c]` with `c=ab`: open only `d=x-a` and `e=y-b`, then derive shares of `xy=c+db+ea+de`, assigning the public `de` term to one party. This is correct only with fresh, correctly generated triples and semi-honest parties; the chosen OT/COT/VOLE generation protocol must be separately specified and reviewed. A direct semi-honest OT multiplication is a reference point, not a commitment to abandon triples. Choose and bind a field degree/polynomial after benchmarking 64 vs. 128 bits and validating security margins. A 64-bit field must not be presented as 128-bit statistical security.

All protocol consumers use the session's one correlation provider through the common `CotSource`/VOLE seam; they do not independently select OT/COT/VOLE constructions. The selected baseline is KEM-based base OT → IKNP/SoftSpoken seed extension → Ferret-Reg refill pool → Bea95 chosen-bit COT conversion. If that common provider fails its security or performance gate, replace it once for the profile rather than adding protocol-local alternatives. Binary Boolean triples and `GF(2^k)` field triples are not interchangeable; each triple consumer still documents and tests its exact field equation.

Do not start with BGW/Shamir honest-majority sharing: it cannot give private `n=2,t=1` security. Do not adopt SPDZ/MAC machinery in this semi-honest phase; that would implement a stronger different model and add complexity. A later `n>2` expansion needs its own defined semi-honest sharing/preprocessing profile and exact threshold; it must preserve the `q=t+1` key invariant rather than assuming the two-party triple protocol scales unchanged.

## Milestones and exit criteria

Milestones validate the chosen semi-honest profile in order. The final artifact at every compiler stage is Volar IR (before the existing LIR backend); no milestone routes through Boolar or a weaver.

### M0 — Correlation-provider decision and performance exploration

The initial profile is normative: computational semi-honest; `n=2,t=1`; `q=t+1=2`; one binary extension field; all operations except source external actions and RNG are in scope; one fixed storage party as an ordinary MPC participant; synchronous execution; IPC test transport; salt-bound sessions; immediate abort on offline party; only confirmed, enabled semi-honest recovery. Malicious-with-abort is a possible later profile; stronger security is out of scope.

**Protocol-layer decision:** every OT/COT/VOLE-using protocol consumes one session-scoped provider: an experimental ML-KEM-seeded 1-of-2 base-OT candidate → semi-honest IKNP/SoftSpoken seed extension → Ferret-Reg iterative COT pool → Bea95 chosen-bit conversion → common `CotSource`/VOLE API. Protocols do not select these constructions independently. ML-KEM-512/768/1024 have been compared for performance, but no parameter set or OT construction is approved: the exact OT proof, quantum-adversary game, security-level requirement, Ferret/LPN profile, and complete composition remain unpinned. The candidate adapter is currently in-process only; the networked `FerretOtChannel` still defaults to LWE base OT. FIPS 203 standardizes ML-KEM, not this OT composition; a KEM choice or passing COT equation test establishes no post-quantum security claim.

M0 is exploratory and performance-first. Reproduce the measurements and blockers in [ADR 0002](adr/0002-semi-honest-pq-cot-ferret.md). The initial recorded run measured 128-base-OT medians of 7.454/12.790/20.311 ms for ML-KEM-512/768/1024 and main-profile setup+drain totals of 31.742–31.840 s. A same-host replay on 2026-10-06 (Apple M3 Ultra, Rust 1.98.1) measured base-OT medians of 7.837/12.720/20.062 ms, nominal candidate traffic of 405,504/585,728/806,912 B, and full main-profile setup+drain totals of 31.875–31.917 s at 0.654–0.657M rows/s, with 2.027–2.034 GB maximum RSS. The benchmark-only plugins use the same `OtStack`/Ferret code. Across both single-run comparisons, Ferret/LPN dominates and end-to-end timing differences remain sub-percent. This is a performance-only ordering, not a security ranking or parameter selection; use the security level required by a reviewed exact OT construction.

The earlier fused-LPN main-profile run measured 16.24 s setup and 10.20M chosen-COT→VOLE rows at about 0.660M rows/s (15.46 s drain), with ~2.03 GB peak resident memory. The measurements use deterministic RNG, no IPC/network, and the Ferret table's legacy classical parameters; they are not controlled deployment benchmarks. The documented setup profile is not usable with current regular-MPCOT shape requirements, so the benchmark bypasses it and generates main-iteration seed COTs directly. This is evidence for the prototype only—not a post-quantum security claim.

Still open: benchmark a different PKE/KEM only after its OT-composition assumptions and a correct implementation are established; repair the setup/main parameter binding; paper-bind/replace the still-unpinned SHA3-based LPN expansion; reduce memory; and run through the eventual two-process IPC provider. Select the binary-field degree/polynomial, threshold-key construction, encrypted-block format, and Path-ORAM vs Circuit-ORAM candidate with their own evidence; do not conflate those choices with the shared correlation-provider decision.

**Exit:** the user-decided profile and common provider layering are recorded; the exploratory benchmark is reproducible; all known cryptographic/performance blockers are explicit. This exit is not a security approval or a PQ claim.

### M1 — Exhaustive binary-field Volar IR normalization

Extend/build a pass based on `lower_to_native` that normalizes every in-scope source operation to `ExtField(Bit,k,p)` and returns Volar IR. Retain source external `ActionCall` and RNG as explicitly typed exceptions. Rewrite storage to field-valued ORAM protocol inputs/outputs. Ensure every data SSA value and relevant storage address/value is field-typed; prune unused source data types from the output type graph. Add a verifier that enumerates all statement/terminator kinds and rejects any missing case.

**Tests:** differential evaluation for every supported IR operation and CFG/control form; canonical field arithmetic; conversion/embedding round trips; verify only the selected field and structural control types remain; assert `NativeLowering::Ir`, never the `Bit`/Boolar sink; source actions/RNG survive with metadata; storage does not remain as direct clear indexed I/O.

**Exit:** source IR → field-only Volar IR is semantics-preserving for the declared subset, with no Boolar dependency.

### M2 — Two-party semi-honest shared-field operations

Implement `n=2,t=1` additive shares over the selected `GF(2^k)`. Keep additions/local linear maps as ordinary local Volar IR. Add OT/COT/VOLE-preprocessed field Beaver triples and protocol ActionCalls for multiplications and required input/opening conversions. Treat source actions and RNG under their explicit exception rules; do not convert their semantics silently.

**Tests:** triple correctness plus a proved provider-composition boundary; exhaustive small-field multiplication/reference semantics; masked-difference opening; private-input sharing; authorized output reconstruction; local additions produce no network actions; offline triple exhaustion/malformed actions abort. Use two separate processes in IPC tests. COT equation tests establish correctness only, not privacy or post-quantum security.

**Exit:** reproducible semi-honest 2PC field computation against a clear reference. No malicious security claim.

### M3 — Hidden native session context and IPC runtime

Inject a backend-private session/context parameter into native function and coroutine ABIs while keeping it absent from Volar IR and user signatures. Implement synchronous action dispatch, per-thread/task context ownership, sequence/salt binding, and an IPC channel for child-process tests.

**Tests:** calls/returns/loops, separate threads/coroutines, two concurrent sessions, salt mismatch/replay rejection, action order, no cross-session context leakage, abrupt process loss causing immediate abort. Verify saved/serialized Volar IR has no runtime context values or secrets.

**Exit:** existing Volar IR actions run through the hidden context in distinct processes; no `LoopbackOt`-only proof.

### M4 — One-store ORAM plus encryption in shared-field MPC

Port the current symbolic ORAM algorithm to the field-only Volar IR/action path. Preserve secret-shared posmap/stash and reveal only oblivious physical paths; the designated store host serves encrypted paths and is an ordinary participating party. Encrypt the entire block, including validity/tag fields, and evaluate encryption/decryption under threshold key shares inside MPC. First establish tiny-geometry Path-ORAM equivalence; in parallel compile/benchmark Circuit ORAM as a candidate replacement.

**Tests:** initialize/write/read/repeated logical addresses; compare with `oram_access_local` and an independent map; only physical paths and ciphertext visible to store; access traces do not reveal equal logical addresses; storage-party process is one of the MPC participants; shared key never reconstructed in ordinary runs; one-key-share corruption reveals no plaintext; no role-specific helper exception. Exercise whole-entry encryption, fresh unpredictable leaf generation, fixed geometry/padding, salt/epoch binding, and abort-safe path commit.

**Exit:** a real two-process semi-honest ORAM+encryption run with field-only Volar IR and one designated store. This is the key feasibility gate; passing current GC tests alone does not satisfy it.

### M5 — Threshold key setup, sessions, and controlled recovery

Implement the `q=t+1` key-share setup and refresh for the selected party profile. Add fresh session salts, monotonic action/transcript/storage counters, restart/retry handling, and the externally confirmed recovery path. Any party offline first aborts; normal execution does not degrade to a smaller quorum.

**Tests:** `n=2,q=2` requires both contributions; missing party immediately aborts; stale-session frames rejected; unconfirmed/disabled recovery rejected; confirmed recovery with q shares reconstructs only the checkpointed state, forms a new party set/salt/key epoch, and resumes from the last committed action; fewer than q shares cannot recover. Make the expected unrecoverable one-party-left case explicit.

**Exit:** no silent continuation, stale transcript reuse, or undeclared reconstruction.

### M6 — ORAM performance and multi-party scale-up

Benchmark Path ORAM vs Circuit ORAM with the actual shared-field multiplication/action backend, not paper gate counts alone. Report total/online actions, field triples, rounds, bytes, encryption work, wall time, tree size, stash/posmap size, and sensitivity to `N`, block width, and field degree. The compiler-cost harness must construct circuits through LLVM → VAFFLE → Volar IR → circuit-shaped Volar IR → (optionally) circuit-shaped Boolar IR; keep plain Volar IR execution separate from circuit construction/evaluation. Then instantiate an explicitly specified `n>2` semi-honest profile with `q=t+1`; preserve the single physical store and same field-only Volar IR contract. Treat Duoram/GigaDORAM/MetaDORAM as alternatives only if changing to distributed storage.

**Partial field measurement:** `volar/crates/vc/volar-vc/examples/field_circuit_bench.rs` compares one LLVM-imported field multiplication per candidate, unrolls to circuit-shaped Volar IR, and lowers to Boolar. On an Apple M3 Ultra with Rust 1.98.1, three process runs each evaluated 1,000 plain Volar IR multiplications cycling through 16 deterministic input pairs; the table gives median interpreter time and median per-process peak RSS (`/usr/bin/time -l`). All candidates were admitted by the field type checker, and the plain IR and Boolar outputs matched an independent polynomial-multiplication reference.

| Field | Volar IR ops | Boolar ops | ANDs | Plain IR eval / multiply | Peak RSS |
|---|---:|---:|---:|---:|---:|
| `GF(2^64)`, `x^64 + 0x1b` | 1,425 | 21,117 | 6,670 | 0.807 ms | 16,744,448 B |
| `GF(2^64)`, `x^64 + 0x807` | 1,425 | 22,147 | 7,014 | 0.805 ms | 16,793,600 B |
| `GF(2^128)`, `x^128 + 0x87` | 1,941 | 79,037 | 25,999 | 2.778 ms | 23,805,952 B |
| `GF(2^128)`, `x^128 + 0x10000000000000000b` | 1,941 | 88,784 | 29,248 | 2.792 ms | 25,034,752 B |

This is a cost-only result: among the sampled polynomials, `0x1b` and `0x87` have the lowest Boolar AND counts at degrees 64 and 128, respectively. The degree-128 product costs about 3.9× the ANDs of degree 64 in this lowering. These AND counts are not Beaver-triple counts or measurements of the future shared-field protocol; a field multiplication would consume one field triple, with communication width dependent on `k`. The small plain-interpreter timing differences are uncontrolled and do not rank the polynomials. Interpreter timings are not native or deployment timings, and RSS includes LLVM import and circuit construction. No degree/security profile or polynomial has been selected.

**ORAM comparison blocker:** the repository has a Path-ORAM runtime/gadget and an LLVM→VAFFLE→Volar IR→Boolar→Path-ORAM fixture, but no Circuit-ORAM implementation. The existing `llvm_storage_oram` smoke fixture reports 144 accesses, `tape_bits=1164`, 64 cells, 4 levels, stash 96, and 90,516 access-schedule ANDs (13,034,304 for the guest's 144 accesses); this is Path-ORAM-only data, not a Path-vs-Circuit comparison. The test named `s1_circuit_oram_matches_reference` also exercises Path ORAM. Implement and validate a distinct Circuit-ORAM candidate before comparing lowered operations, ANDs, memory, or plain execution time; do not substitute the paper's gate counts or mislabel the Path baseline.

**Tests:** multiple `n>2` parties through IPC; threshold boundary and `q` availability; store party collusion within `t`; same logical program semantics; reproducible benchmark harness. Reject any profile with no defined multiplication/triple/key protocol.

**Exit:** measured ORAM choice and an actual multi-party execution beyond two parties.

### M7 — Independent evidence and frontend integration

Carry the selected field/profile/storage metadata through one real frontend, then LLVM/WASM as required. Complete compiler/runtime diagnostics, manifest versioning, trace redaction, operation inventory, and reproducible deployment/test instructions.

**Exit gates before *any* profile security claim:** (1) a proof/security argument bound to the exact protocol, field, implementation, and leakage; (2) independent cryptographic/protocol implementation review; (3) a malicious/fault-injection test harness covering malformed frames, reordered/replayed messages, aborts, and recovery boundaries—without claiming malicious security; and (4) separate-process end-to-end tests/benchmarks. If any gate is missing, report implementation evidence only, not a security claim.

## Validation and evidence rules

- **Field-normalization completeness:** a machine-checked inventory covers every `IRStmt`, terminator, and value type. Only source external actions and RNG are exceptions. Output is ordinary Volar IR, data values are all the selected `GF(2^k)`, and storage is ORAM-rewritten.
- **Semantic oracle:** clear Volar IR evaluator vs field-normalized IR vs shared reference vs real protocol result.
- **Share invariant:** reconstructing all `q=t+1` authorized shares returns the logical value; any coalition of at most `t` parties has no more than the declared semi-honest view. Do not infer privacy because one share “looks random.”
- **Action minimality/order:** local field additions generate no protocol action; each field multiply consumes exactly the declared correlation; all openings are masked and manifest-bound; actions cannot be deleted, duplicated, reordered, or CSE'd unsafely.
- **ORAM view:** test that the store sees ciphertext and the declared physical path, while logical addresses, read/write data, posmap/stash, and key remain secret. Include repeated equal logical accesses, distinct traces of equal public length, initialization, retries, and abort paths.
- **Process separation:** run role binaries as separate child processes over IPC; no shared in-memory secret, OT object, or combined test harness context.
- **Session/key:** test q-of-n setup, fresh salts, action counters, nonce/pad separation, epoch changes, unauthorized reconstruction rejection, and exact recovery gate behavior.
- **Fault harness:** fuzz malformed/replayed/reordered IPC frames and crash points. The required behavior is fail-closed abort; this testing is not a malicious-security proof.
- **Resource evidence:** publish field degree/polynomial, AND-equivalent multiplication count, triple preprocessing volume, rounds, bytes, ORAM physical traffic, latency, memory, and geometry. Compare ORAM schemes under the exact same semi-honest field backend.
- **Security language:** every claim names semi-honest `t`, key quorum, party/store collusion, setup, field, channel, leakage, abort and recovery assumptions. Active security is not claimed in this phase.

## Decisions locked by the user

1. Start semi-honest; malicious security with abort is a possible later profile; stronger security is out of scope.
2. The first profile must support private two-party `n=2,t=1` computation.
3. Use a binary extension field; choose `k` and the canonical irreducible polynomial by security/cost evidence. Use `ExtField`, not the `Bit` Boolar sink.
4. Every operation through the field lowering is in scope except source external actions and RNG. Storage is included and becomes ORAM actions.
5. Key threshold is `q=t+1`; with `n=2,t=1`, both parties' key shares are needed. Any offline party aborts. Recovery is only semi-honest, explicitly enabled, externally confirmed, and requires enough shares to reconstruct/re-share the retry state.
6. The designated store owner may collude within `t`, is an MPC party, and should participate as one. It sees the physical oblivious accesses and encrypted tree only.
7. No privileged storage role or helper assumption. Reuse and scale the existing “run ORAM + encryption in MPC” architecture; compare single-store ORAM algorithms. DORAM is for a changed multi-store placement.
8. Output remains Volar IR with field values and added actions. Runtime context is hidden from that IR and carried by native thread/coroutine state plus hidden ABI parameters/returns.
9. Synchronous protocol; separate-process IPC for tests, without requiring full network sockets. Session salt is bound to frames and derivations. Unconfirmed/disabled recovery aborts/restarts.
10. Before any security claim, require all: exact proof/security argument, independent review, malicious/fault-injection harness, and end-to-end evidence. Harness results do not upgrade the semi-honest claim.

## Remaining design work (not user decisions)

- Select `k`/irreducible polynomial and benchmark it against ORAM/AES operation counts.
- Paper-bind and independently review the common KEM-OT → IKNP/SoftSpoken → Ferret-Reg provider for the exact semi-honest and quantum-adversary game, then specify the `GF(2^k)` triple consumer's exact correlations.
- Decide whether the first store implementation ports Path ORAM only or includes Circuit ORAM immediately; M6 requires a measured comparison before a performance claim.
- Specify how each pure source oracle implementation is linked/inlined and field-lowered; missing field-compatible implementations must fail closed. Specify exact source-action/RNG boundary-conversion semantics.
- Specify storage initialization, ciphertext persistence across sessions, counter/version source, checkpoint commit point, and authorized recovery coordinator/quorum.

## References

### Primary cryptographic and ORAM sources

- **[Shamir79]** Adi Shamir, “How to Share a Secret,” *Communications of the ACM* 22(11), 612–613 (1979). [DOI 10.1145/359168.359176](https://doi.org/10.1145/359168.359176). Threshold sharing is a primitive; it is not by itself a secure computation protocol.
- **[Gilboa99]** Niv Gilboa, “Two Party RSA Key Generation,” CRYPTO 1999, LNCS 1666, pp. 116–129. [DOI 10.1007/3-540-48405-1_8](https://doi.org/10.1007/3-540-48405-1_8). Contains the semi-honest OT-based additive-share multiplication often called Gilboa multiplication; it is a useful direct-multiplication reference, not itself the selected field-triple/preprocessing protocol.
- **[FKS15]** Tore Kasper Frederiksen, Marcel Keller, Emmanuela Orsini, and Peter Scholl, “A Unified Approach to MPC with Preprocessing Using OT,” ASIACRYPT 2015. [IACR ePrint 2015/901](https://eprint.iacr.org/2015/901). Covers OT-based triple generation in characteristic-two fields; its concrete SPDZ/TinyOT/MiniMAC constructions include authentication machinery, so extract/adapt no semi-honest variant without a separate proof.
- **[SilentOT]** Elette Boyle, Geoffroy Couteau, Niv Gilboa, Yuval Ishai, Lisa Kohl, and Peter Scholl, “Efficient Pseudorandom Correlation Generators: Silent OT Extension and More,” CRYPTO 2019. [IACR ePrint 2019/448](https://eprint.iacr.org/2019/448). Candidate low-communication correlation/OT extension layer; not a complete MPC multiplication protocol by itself.
- **[BGW88]** Michael Ben-Or, Shafi Goldwasser, and Avi Wigderson, “Completeness Theorems for Non-Cryptographic Fault-Tolerant Distributed Computation,” STOC 1988. [DOI 10.1145/62212.62213](https://doi.org/10.1145/62212.62213); [full proof and protocol exposition](https://eprint.iacr.org/2011/136.pdf). Supports the honest-majority threshold distinctions recorded above.
- **[CDM00]** Ronald Cramer, Ivan Damgård, and Ueli Maurer, “General Secure Multi-Party Computation from any Linear Secret-Sharing Scheme,” EUROCRYPT 2000. [IACR paper](https://www.iacr.org/archive/eurocrypt2000/1807/18070321-new.pdf). Useful background for multiplication/re-sharing and why the LSSS access structure matters.
- **[DPSZ12]** Ivan Damgård, Valerio Pastro, Nigel Smart, and Sarah Zakarias, “Multiparty Computation from Somewhat Homomorphic Encryption,” CRYPTO 2012. [IACR ePrint 2011/535](https://eprint.iacr.org/2011/535). Dishonest-majority active SPDZ-style authenticated shares and preprocessing; contrast only, not the chosen semi-honest implementation.
- **[ABY3]** Payman Mohassel and Peter Rindal, “ABY³: A Mixed Protocol Framework for Machine Learning,” ACM CCS 2018. [IACR ePrint 2018/403](https://eprint.iacr.org/2018/403). A three-party, honest-majority example; do not treat it as arbitrary-`n` or two-party coverage.
- **[RSS23]** Alessandro Baccarini, Marina Blanton, and Chen Yuan, “Multi-Party Replicated Secret Sharing over a Ring with Applications to Privacy-Preserving Machine Learning,” *Proceedings on Privacy Enhancing Technologies* 2023(1), 608–626. [PoPETs paper](https://petsymposium.org/2023/files/papers/issue1/popets-2023-0035.pdf), [DOI 10.56553/popets-2023-0035](https://doi.org/10.56553/popets-2023-0035). Generalizes replicated sharing beyond three parties but documents the combinatorial share representation/cost as thresholds and `n` grow.
- **[PathORAM]** Emil Stefanov et al., “Path ORAM: An Extremely Simple Oblivious RAM Protocol,” CCS 2013; full journal version in *JACM* 65(4) (2018). [Original/full paper](https://pages.cs.wisc.edu/~yxy/pubs/PathORam.pdf), [JACM version](https://people.csail.mit.edu/devadas/pubs/PathORAMJACM.pdf). A client-server ORAM reference; it does not alone supply multi-party secret-shared client state or threshold encryption.
- **[CircuitORAM]** Xiao Wang, T.-H. Hubert Chan, and Elaine Shi, “Circuit ORAM: On Tightness of the Goldreich–Ostrovsky Lower Bound,” ACM CCS 2015. [IACR ePrint 2014/672](https://eprint.iacr.org/2014/672). Its gate-count results are implementation/parameter-specific and must be remeasured with the target protocol.
- **[SCORAM]** Xiao Wang, T.-H. Hubert Chan, and Elaine Shi, “SCORAM: Oblivious RAM for Secure Computation,” ACM CCS 2014. [Paper](https://i.cs.hku.hk/~hubert/ccs14a.pdf). Directly optimizes circuit cost; compare against field-operation/action costs rather than assuming circuit-size wins transfer.
- **[Duoram]** Adithya Vadapalli, Ryan Henry, and Ian Goldberg, “Duoram: A Bandwidth-Efficient Distributed ORAM for 2- and 3-Party Computation,” USENIX Security 2023. [USENIX paper](https://www.usenix.org/system/files/usenixsecurity23-vadapalli.pdf), [IACR ePrint 2022/1747](https://eprint.iacr.org/2022/1747). Semi-honest 2P/3P constructions tolerating one passive corruption; shares memory across parties.
- **[GigaDORAM]** Brett Falk, Rafail Ostrovsky, Matan Shtepel, and Jacob Zhang, “GigaDORAM: Breaking the Billion Address Barrier,” USENIX Security 2023. [IACR ePrint 2023/1950](https://eprint.iacr.org/2023/1950). Three-server DORAM; the reported 700+ queries/sec is at an address-space size of `2^31`, not `2^31` queries/sec.
- **[PRAC]** Mahak Sasy, Adithya Vadapalli, and Ian Goldberg, “PRAC: Round-Efficient 3-Party MPC for Dynamic Data Structures,” *Proceedings on Privacy Enhancing Technologies* 2024(1), 174–197. [PoPETs paper](https://petsymposium.org/popets/2024/popets-2024-0100.pdf). Semi-honest three-party DORAM-based systems work.
- **[ThreePartyRAM]** “3-Party Secure Computation for RAMs: Optimal and Concretely Efficient,” IACR ePrint 2023/516. [Paper](https://eprint.iacr.org/2023/516.pdf). Recent three-party DORAM/RAM-MPC comparison; its distributed memory placement differs from this plan.
- **[MetaDORAM]** Brett Hemenway Falk, Daniel Noble, and Rafail Ostrovsky, “MetaDORAM: Info-Theoretic Distributed ORAM with Less Communication,” ITC 2025. [LIPIcs paper](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ITC.2025.6), [IACR ePrint 2024/011](https://eprint.iacr.org/2024/011). Semi-honest honest-majority three-party DORAM; variants have distinct statistical/perfect-security and setup costs.
- **[MixedModeORAM]** “Efficient Mixed-Mode Oblivious RAMs,” IACR ePrint 2025/1057. [Paper](https://eprint.iacr.org/2025/1057). Studies mixed public/secret access in MPC; public accesses are an explicit leakage class, not a substitute for fully oblivious storage.
- **[LO13]** Steve Lu and Rafail Ostrovsky, “Distributed Oblivious RAM for Secure Two-Party Computation.” [IACR ePrint 2011/384](https://eprint.iacr.org/2011/384.pdf). Relevant two-party/MPC ORAM design point; compare its exact server/trust assumptions before reuse.
- **[SOCS-ORAM]** Vladimir Kolesnikov, Stanislav Peceny, Ni Trieu, and Xiao Wang, “Fast ORAM with Server-Aided Preprocessing and Pragmatic Privacy-Efficiency Trade-Off,” IACR ePrint 2023/991. [Paper](https://eprint.iacr.org/2023/991). Reports very low communication under a deliberate privacy/efficiency tradeoff; reject where the server/helper may learn logical access information.
- **[DORAM23]** Brett Falk, Daniel Noble, Rafail Ostrovsky, Matan Shtepel, and Jacob Zhang, “DORAM Revisited: Maliciously Secure RAM-MPC with Logarithmic Overhead,” TCC 2023. [IACR ePrint 2023/578](https://eprint.iacr.org/2023/578.pdf). A three-party, one-static-malicious-corruption DORAM reference; not a single-host arbitrary-`n` drop-in.
- **[DiSE18]** Shashank Agrawal, Payman Mohassel, Pratyay Mukherjee, and Peter Rindal, “DiSE: Distributed Symmetric-key Encryption,” ACM CCS 2018. [IACR ePrint 2018/727](https://eprint.iacr.org/2018/727.pdf). Formalizes distributed symmetric encryption with privacy and authenticity. Important warning: threshold symmetric encryption needs a dedicated protocol definition; ordinary key sharing followed by key reconstruction loses the main key-separation property.
- **[ParaDiSE22]** Shashank Agrawal, Wei Dai, Atul Luykx, Pratyay Mukherjee, and Peter Rindal, “ParaDiSE: Efficient Threshold Authenticated Encryption in Fully Malicious Model,” INDOCRYPT 2022. [IACR ePrint 2022/1449](https://eprint.iacr.org/2022/1449.pdf). Candidate primary source for comparing threshold AE constructions and malicious-model definitions.
- **[DKG]** Rosario Gennaro, Stanisław Jarecki, Hugo Krawczyk, and Tal Rabin, “Secure Distributed Key Generation for Discrete-Log Based Cryptosystems,” EUROCRYPT 1999. [DOI 10.1007/3-540-48910-X_21](https://doi.org/10.1007/3-540-48910-X_21). Key-generation reference; DKG alone does not provide symmetric AEAD or storage obliviousness.

### Local implementation/design anchors

- [`MPC_PLAN.md`](../../MPC_PLAN.md) — existing workspace MPC architecture and milestones.
- [`GRAM_SYMBOLIC_ORAM.md`](../../GRAM_SYMBOLIC_ORAM.md) — current symbolic ORAM design/evidence and its two-party/Boolar boundaries.
- [`mpc-external-actions-oracles-plan.md`](mpc-external-actions-oracles-plan.md) and [`mpc-external-actions-oracles-ledger.md`](mpc-external-actions-oracles-ledger.md) — external actions, order, identity, explicit policy, and current implementation gaps.
- [`agent-context/oram.md`](agent-context/oram.md) — local ORAM/channel architecture.
- [`direct-to-lir-weaver-fast-path-plan.md`](direct-to-lir-weaver-fast-path-plan.md) — direct LIR lowering precedent; its weaver path is explicitly not this design.
- [`../../volar-ir/docs/ir-lowering.md`](../../volar-ir/docs/ir-lowering.md), [`../../volar-ir/docs/lir.md`](../../volar-ir/docs/lir.md), and [`../../volar-ir/docs/lir-abi.md`](../../volar-ir/docs/lir-abi.md) — current Volar IR/LIR semantics and ABI. The current LIR action hook is in `volar-ir/crates/ir/volar-lir/src/lib.rs`; action lowering is in `volar-ir/crates/ir/volar-ir-passes/src/lower_lir.rs`.
