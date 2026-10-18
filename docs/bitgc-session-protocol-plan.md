# BitGC session protocol plan

**Status:** Phases 0–3 landed; Phase 4 added canonical bounded evaluator-material codecs (`RoleMaterialCodec`, `encode_role_tables`/`decode_role_tables`), a transcript-bound MATERIAL Data-frame delivery phase, garbler-input direct delivery frames, and per-role networked entry points (`run_garbler_over`/`run_evaluator_over`) — exercised over threaded pipes with authenticated sessions for both variants, plus the in-process runner, Cirrus adapter, and cross-process frame/OT tests. Phase 5 remains an independent review gate that cannot be self-satisfied. No session protocol or protected-data profile is deployable.
**Scope:** shared Volar/Cirrus session infrastructure. Aero is not part of this work.
**Ledger:** `BITGC-LEDGER-07` in `docs/fhe/future-provider-integration-ledger.md`.

## 1. Requirements

- **R-1 Paper equations replace the reference equation** — `BITGC-LEDGER-04`, the archived 2025/1810 and 2024/1988 papers, and the authors' artifact.
- **R-2 Garbler secrets stay role-separated** — the request from t-9bba and the pinned construction's key ownership.
- **R-3 Evaluator inputs use OT** — the existing `volar-mpc` OT seam and BitGC input-label equations.
- **R-4 Sessions bind peers and circuits** — the request for versioned, transcript-bound frames.
- **R-5 Frames are bounded and sessions cancel safely** — the request for bounded APIs, cancellation, and errors.
- **R-6 Output labels are opened by hash match** — the paper's output-label pair path; the reference `pi` opening is test-only. Hash matching does not authenticate an unauthenticated transport.
- **R-7 Cirrus uses the shared protocol** — the request for a shared Volar/Cirrus implementation and differential tests.
- **R-8 Security claims remain gated** — Volar reliability policy and `BITGC-LEDGER-03`.

## 2. Approach by requirement

### R-1 Paper equations replace the reference equation

- **Approach:** Keep equation and label math in `volar-spec::bitgc::pinned`; the role-separated Volar core is landed, while Cirrus's existing reference adapter remains behind its explicit opt-in until Phase 4. Quarantine `ReferenceEquation` to tests or an explicit reference-only feature.
- **Touches:** `volar-spec::bitgc`, `volar-mpc`, and `cirrus-volar-garble`.
- **Test seam:** Pinned role-separated garble/evaluate drivers and artifact-backed circuit vectors.
- **Acceptance:** [x] Small and fast equations map to cited figures. [x] Cirrus cannot select the reference equation by default. [x] Truth tables and record counts match the independent oracle on the current fixtures; exact per-wire HSS differential remains open.
- **Hard part:** Replace the opt-in Cirrus reference adapter with the shared role API and session path in Phase 4.

### R-2 Garbler secrets stay role-separated

- **Approach:** Split garbler setup from evaluator-safe evaluation material. The evaluator receives neither `Delta` nor the SWHE secret key. The evaluator-facing DD interface exposes only the public/evaluation material required by the pinned operations.
- **Touches:** `volar-spec::bitgc::pinned` and the session setup in `volar-mpc`.
- **Test seam:** Construct distinct evaluator material and exercise evaluation without passing the garbler object, `Delta`, or PRF key; serialization and session ownership are deferred to Phases 2–3.
- **Acceptance:** [x] No evaluator entry point accepts `SecretKey`, `Delta`, or the PRF key. [x] Both variants evaluate using a distinct evaluator-only material object. [ ] Session construction and serialization enforce the role boundary across processes.
- **Hard part:** The paper's distributed-decryption and CP-RLWE key material must be mapped exactly, not hidden behind a renamed shared key; the current RNS/noise profile still needs independent review.

### R-3 Evaluator inputs use OT

- **Approach:** Route evaluator-owned labels through the fallible `BitGcInputOt` seam. The experimental `ChouOrlandiBitGcOt` uses a dedicated bounded OT message transport; the infallible `OtChannel` adapter is explicitly local-only. Input descriptors bind index and chunk order to session frames, and `AuthenticatedBitGcOtTransport` adds keyed envelopes with one ordered transcript sequence across both OT directions.
- **Touches:** `volar-mpc` input ownership/OT and the BitGC session.
- **Test seam:** Loopback tests plus two-thread Chou–Orlandi OT tests, both input choices, malformed descriptors, and injected OT failures.
- **Acceptance:** [x] The tested OT implementation returns only its selected label. [x] Input index/chunk order is checked before the corresponding OT read. [x] Authenticated OT messages are bound to the descriptor context and one ordered keyed OT transcript. [x] Session-driven input transfer merges hello/data-frame digests, frame tags, and OT messages into one common transcript.
- **Hard part:** The pinned reference driver currently returns masked bits alongside labels; the session must not send that helper value as clear protocol data. The PSK mode has no forward secrecy, so deployments may still need an external authenticated key exchange.

### R-4 Sessions bind peers and circuits

- **Approach:** Add a transport-neutral session state machine. Carry role, protocol version, variant, parameter fingerprint, circuit digest, session ID, and monotonic sequence numbers in every frame and validate them against local session context. `new_authenticated` and `new_psk_authenticated` authenticate frame tags from the common frame/OT transcript; the PSK constructor derives a binding-separated session key. External key exchange remains available where forward secrecy is required.
- **Touches:** New `volar-mpc::bitgc_session` module and the Cirrus adapter.
- **Test seam:** Role-pair loopback tests plus wrong-version, wrong-profile, wrong-circuit, replay, and reordering tests.
- **Acceptance:** [x] A frame from another session, variant, parameter profile, or circuit is rejected before cryptographic evaluation. [x] Sequences advance exactly once; replay and reordering are rejected.
- **Hard part:** Bind the canonical digest helpers to the compiler/provider's reviewed profile registry and define the external key-establishment seam without adding a serialization dependency to `volar-spec`.

### R-5 Frames are bounded and sessions cancel safely

- **Approach:** Define a versioned, canonical frame codec with checked lengths and bounded chunks. Return transport and protocol errors; cancellation moves both state machines to a terminal state.
- **Touches:** `volar-mpc::bitgc_session` and transport adapters.
- **Test seam:** Codec property tests, truncated/oversized frame tests, injected I/O failures, and cancel-at-each-state tests.
- **Acceptance:** [x] The borrowed decoder checks fixed limits before exposing payloads; the transport seam requires bounds before peer-sized allocation. [x] Malformed, duplicate, out-of-order, or over-budget input aborts. [x] Abort/cancel cannot resume the session.
- **Hard part:** Bound ciphertext and RNS payload sizes before deserialization and avoid partial-state reuse after errors.

### R-6 Output label hash-match opening

- **Approach:** Send domain-separated hashes of both output labels bound to the canonical session/circuit/profile context; the evaluator recognizes its active label by hash match. Never reveal `pi` as the production opening. Unauthenticated sessions provide label-pair consistency only; authenticated sessions also require an externally established key.
- **Touches:** Pinned output handling and session frames.
- **Test seam:** Output-label tampering and output/wire-order checks. Transport-authentication tests remain open.
- **Acceptance:** [x] The evaluator rejects an active label outside the committed pair. [x] The garbler's SWHE secret is not needed to open evaluator output. [ ] The commitments are authenticated to the peer and transcript.
- **Hard part:** This does not provide active-secure garbling or authenticate the sender; keep protected use disabled and document the semi-honest model.

### R-7 Cirrus uses the shared protocol

- **Approach:** Keep `cirrus-volar-garble::bitgc` as a thin adapter over `volar-mpc::bitgc_session` and the pinned spec types. Remove duplicated equation behavior.
- **Touches:** Cirrus BitGC backend and its `cirrus_core` adapters.
- **Test seam:** Existing BitGC roundtrip plus shared-schedule differential tests against Volar and the artifact.
- **Acceptance:** [ ] Both backends consume the same versioned schedule/profile. [ ] No default Cirrus path reaches `ReferenceEquation` or accepts evaluator-held garbler secrets.
- **Hard part:** Preserve Cirrus streaming while the offline expansion and record stream become framed and bounded.

### R-8 Security claims remain gated

- **Approach:** Keep the module unpinned/very unstable and refuse protected-data admission until independent parameter/noise review and protocol review pass.
- **Touches:** Ledger, profile admission, module docs, and tests.
- **Test seam:** Admission tests and the external review record.
- **Acceptance:** [ ] No production profile is enabled by test evidence alone. [ ] `BITGC-LEDGER-03` remains a release gate.
- **Hard part:** The current lattice gadget uses simplified noise and has an unresolved KDM/RLWE security review.

## 3. Phases

### Phase 0 — Fail-closed quarantine

**Covers:** R-8 and the safety preconditions for R-1/R-2.

**Goal:** Prevent the current reference adapters from being selected by default or admitted as protected sessions while the role-safe core is unfinished.

**Landed:** Volar rejects every BitGC profile at admission; Cirrus exports its existing reference adapter only behind the explicit `bitgc-reference` feature. Half-gate admission remains on its separate existing path.

**Proof:** Default Cirrus build/tests pass without exporting the reference adapter; the explicit reference-feature round-trip remains available for correctness testing. This is containment only, not role separation or a security review.

### Phase 1 — Role-safe paper-pinned core

**Covers:** R-1, R-2, R-8.

**Goal:** The smallest garble/evaluate path runs from distinct garbler and evaluator material, with no evaluator reference to `Delta` or the garbler's secret key.

**Tasks — landed**

- [x] Trace the small/fast equations against the archived papers and artifact; see `docs/research/bitgc-paper-pinning.md`.
- [x] Split DD setup from evaluator-safe DD operations. `LatticeRnsEvaluator` carries no Δ, and `RoleDdGarbler` does not implement `RoleDdEvaluator`.
- [x] Gate the legacy shared/reference equation and drivers behind the non-default `bitgc-reference` feature (or unit tests).
- [x] Add a compile-fail API check and runtime role-separated round-trips.
- [x] Exercise small and fast with distinct garbler/evaluator material; explicitly reject tiny.

**Validation (2026-10-09):** `volar-spec` default-feature `cargo check` passed; feature-enabled tests passed (330 unit tests, 5 integration tests, and the compile-fail doctest; 2 unit tests and 1 doctest ignored). Cirrus default-feature `cargo check` passed and its opt-in reference round-trip passed (2 tests). These are API/correctness checks only—not independent review, cryptographic security evidence, or deployability evidence.
**Not here:** Session transport, output-opening protocol, Cirrus integration, or production parameter claims.

### Phase 2 — Bounded, bound session frames

**Covers:** R-4, R-5.

**Goal:** Two local roles reject malformed, replayed, mismatched, and over-budget frames before evaluation.

**Tasks — landed**

- [x] Define the v1 header and sequence rules: role, variant, 32-byte parameter fingerprint, circuit digest, session ID, and per-direction monotonic sequence.
- [x] Implement canonical encode/decode with a 64 KiB payload cap and borrowed payload parsing.
- [x] Add fallible `BitGcFrameTransport` send/receive/cancel seams and terminal abort/cancel states.
- [x] Test malformed, truncated, oversized, replayed, reordered, wrong-role, and wrong-binding frames, plus injected transport failure and cancellation during handshake/active states.

**Validation (2026-10-09):** `volar-mpc` BitGC session unit tests passed (10/10) with an offline workspace-patched Cargo run; `git diff --check` and targeted rustfmt passed. The codec rejects noncanonical/truncated/oversized frames, and the state machine rejects mismatches before returning payloads. At this phase boundary the envelope was unauthenticated and OT was not yet implemented; Phase 3 adds only an experimental unbound OT adapter. Peer/transcript authentication, canonical digest construction, and protected-session admission remain open.
**Not here:** Live OT or any deployment transport.

### Phase 3 — Input OT and output-label hash-match foundation

**Covers:** R-2, R-3, R-6.

#### Landed prototype tasks

- [x] Add a fallible 1-of-2 OT interface and experimental Chou–Orlandi adapter over a dedicated bounded message channel; keep the infallible `OtChannel` wrapper explicitly local-only.
- [x] Transfer serialized evaluator-input labels in bounded OT chunks, with session-bound ordered descriptors; keep the choice bit out of frames and enforce the profile's exact ring degree.
- [x] Add domain-separated output-label pair hashes and evaluator hash-match opening without revealing `pi` or the garbler's secret.
- [x] Make malformed descriptors, OT/frame failures, output mismatches, and failed cancellations terminal; test both input choices, OT failure paths, profile shape rejection, and output-label tampering.
- [x] Add canonical circuit/profile digest helpers and an opt-in keyed frame authenticator; bind the dedicated OT channel with keyed ordered envelopes.
- [x] Add PSK-authenticated nonce hellos and binding-separated session-key derivation; retain an external-key constructor for deployments requiring forward secrecy.
- [x] Chain authenticated OT messages with one ordered transcript sequence across both directions and construct OT wrappers from the authenticated session binding/key.
- [x] Maintain one session context transcript across both hello frames, data frames, frame authentication tags, and authenticated OT messages; use its checkpoint to seed each OT invocation.
- [x] Bind each OT invocation to the exact encoded garbler/evaluator hello digests and descriptor-frame digest, including frame authentication tags.
- [x] Add an explicit Cirrus `bitgc-session` adapter over bounded Pusher/iterator frame and OT channels; keep `bitgc-reference` separate and opt-in.
- [x] Exercise authenticated frame and OT round trips across independent test processes over bounded loopback TCP transports, with each OT chunk bound to its descriptor-frame context.

**Validation (2026-10-09):** `volar-mpc` session tests passed (15/15); the full `volar-mpc` library suite passed (45 tests), and `volar-spec` passed (330 passed, 2 ignored). Two-thread and independent-process tests exercised the Chou–Orlandi adapter over bounded message pipes/TCP, including the shared context transcript across hello/data frames and OT messages; canonical circuit/profile digest, keyed-frame tamper, context-mismatch, and PSK-handshake tests also pass. This is implementation/correctness evidence only.

**Still open:** The thin Cirrus adapter is transport/session plumbing only; it does not replace the legacy reference backend's live `Context` integration. The in-process runner drives both roles synchronously over loopback; cross-process role separation is exercised by the independent-process frame/OT test. The PSK handshake is authenticated but not forward-secret, and external key exchange remains supported. The default unauthenticated API remains label-pair consistency only, and no active-security claim follows.

**Proof:** Not yet met. The independent-process test exercises bounded loopback TCP and keyed checks, but does not establish a deployable transport, key-establishment protocol, or cryptographic security review.
**Not here:** Malicious security or protected-data admission.

### Phase 4 — Cirrus adapter and differential closeout

**Covers:** R-1, R-3, R-4, R-5, R-6, R-7.

**Goal:** Cirrus drives the same shared protocol and agrees with Volar and the independent artifact on bounded circuits.

**Tasks**

- [x] Add the explicit Cirrus `bitgc-session` adapter over bounded Pusher/iterator frame and OT channels (Cirrus `18b48e8`), kept separate from `bitgc-reference`.
- [x] Add `run_role_separated_circuit_local` plus `transfer_evaluator_input`/`open_committed_outputs` seams over the common frame/OT transcript (Volar).
- [x] Add canonical bounded codecs: `RoleMaterialCodec` for evaluator setup material and rows (implemented for `LatticeRnsEvaluator`), `encode_role_tables`/`decode_role_tables` for `RoleEvaluatorTables`, with parameter-validated decode and fail-closed truncation/tamper/variant-mismatch tests.
- [x] Add the MATERIAL Data-frame delivery phase (`send_material_over`/`receive_material_over`): bounded chunked delivery of the setup material + tables package after hello and before input OTs, advancing the unified transcript; wrong-order/reordered/oversized chunks abort.
- [x] Add garbler-input direct delivery frames (`send_garbler_input_over`/`receive_garbler_input_over`): garbler-owned inputs need no OT; the active label is sent in one bound Data frame.
- [x] Add per-role networked entry points `run_garbler_over`/`run_evaluator_over` over caller-owned frame and OT transports, with caller-constructed sessions (structural, keyed, or PSK) and post-decode profile verification against the session fingerprint.
- [x] Exercise small and fast role-separated circuits across all 8 input choices (in-process), and a mixed-ownership circuit (garbler + evaluator inputs) across threads with authenticated sessions and real Chou–Orlandi OT; plus Tiny/bad-input/nontopological rejection (51 total `volar-mpc` tests).
- [x] sub64 accounting and correctness across both variants (landed in Phase 1's conformance test; unchanged).
- [x] Record remaining parameters and review blockers in the ledger.
- [ ] Full Cirrus streaming `Context` replacement: the legacy reference backend remains available only behind `bitgc-reference`; wiring a live Cirrus interpreter over the session runner is deferred.
- [ ] Artifact-exact per-wire HSS differential vectors remain open (BITGC-LEDGER-01/02).

**Validation:** `volar-mpc` library suite passes (51 tests, including the threaded authenticated per-role runner); `volar-spec` passes (331 lib tests including the material codec round-trip); Cirrus `bitgc-session` adapter tests pass (3). All are correctness evidence only. Property-based topology and frame mutation are covered by focused topology/mutation tests rather than a randomized harness.
**Not here:** Active security, BitGC-tiny, or a production admission profile.

### Phase 5 — Independent review and release gate

**Covers:** R-2, R-6, R-8.

**Goal:** No protected-data claim is made unless independent protocol and parameter reviews pass.

**Tasks**

- Review RLWE/KDM assumptions, noise distributions, and per-gate failure bounds.
- Review transcript, OT, output-opening, and cancellation security.
- Record reviewer, reviewed commit, assumptions, and findings.
- Keep profile admission disabled until the maintainers accept the review.

**Proof:** `BITGC-LEDGER-03` and `BITGC-LEDGER-07` have explicit accepted evidence, or the feature remains unavailable for protected data.
**Not here:** Changes to Aero or its provider registry.
