# BitGC session protocol plan

**Status:** Phases 0–2 safety quarantine, role-separated algebraic core, and bounded frame/state-machine foundation landed and locally validated. No session protocol or protected-data profile is deployable.
**Scope:** shared Volar/Cirrus session infrastructure. Aero is not part of this work.
**Ledger:** `BITGC-LEDGER-07` in `docs/fhe/future-provider-integration-ledger.md`.

## 1. Requirements

- **R-1 Paper equations replace the reference equation** — `BITGC-LEDGER-04`, the archived 2025/1810 and 2024/1988 papers, and the authors' artifact.
- **R-2 Garbler secrets stay role-separated** — the request from t-9bba and the pinned construction's key ownership.
- **R-3 Evaluator inputs use OT** — the existing `volar-mpc` OT seam and BitGC input-label equations.
- **R-4 Sessions bind peers and circuits** — the request for versioned, transcript-bound frames.
- **R-5 Frames are bounded and sessions cancel safely** — the request for bounded APIs, cancellation, and errors.
- **R-6 Outputs are authenticated** — the paper's output-label hash-match path; the reference `pi` opening is test-only.
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

- **Approach:** Route evaluator-owned input labels through the existing `OtChannel` seam; garbler-owned and public inputs use explicit ownership rules. Bind each transfer to the circuit input index and session.
- **Touches:** `volar-mpc` input ownership/OT and the BitGC session.
- **Test seam:** Deterministic loopback OT tests with both input owners and mismatched-choice tests.
- **Acceptance:** [ ] Evaluator receives only its selected label. [ ] Input order and ownership are checked before any gate record is consumed.
- **Hard part:** The pinned reference driver currently returns masked bits alongside labels; the session must not send that helper value as clear protocol data.

### R-4 Sessions bind peers and circuits

- **Approach:** Add a transport-neutral session state machine. Carry role, protocol version, variant, parameter fingerprint, circuit digest, session ID, and monotonic sequence numbers in every frame and validate them against local session context. Cryptographic transcript authentication remains deferred.
- **Touches:** New `volar-mpc::bitgc_session` module and the Cirrus adapter.
- **Test seam:** Role-pair loopback tests plus wrong-version, wrong-profile, wrong-circuit, replay, and reordering tests.
- **Acceptance:** [x] A frame from another session, variant, parameter profile, or circuit is rejected before cryptographic evaluation. [x] Sequences advance exactly once; replay and reordering are rejected.
- **Hard part:** Define canonical parameter and circuit digests without adding a serialization dependency to `volar-spec`.

### R-5 Frames are bounded and sessions cancel safely

- **Approach:** Define a versioned, canonical frame codec with checked lengths and bounded chunks. Return transport and protocol errors; cancellation moves both state machines to a terminal state.
- **Touches:** `volar-mpc::bitgc_session` and transport adapters.
- **Test seam:** Codec property tests, truncated/oversized frame tests, injected I/O failures, and cancel-at-each-state tests.
- **Acceptance:** [x] The borrowed decoder checks fixed limits before exposing payloads; the transport seam requires bounds before peer-sized allocation. [x] Malformed, duplicate, out-of-order, or over-budget input aborts. [x] Abort/cancel cannot resume the session.
- **Hard part:** Bound ciphertext and RNS payload sizes before deserialization and avoid partial-state reuse after errors.

### R-6 Outputs are authenticated

- **Approach:** Commit to both output labels with domain-separated hashes bound to the transcript; the evaluator recognizes its active label by hash match. Never reveal `pi` as the production opening.
- **Touches:** Pinned output handling and session frames.
- **Test seam:** Output-label tampering, wrong-wire, wrong-bit, and transcript-replay tests.
- **Acceptance:** [ ] Evaluator cannot accept a label outside the committed pair. [ ] The garbler's SWHE secret is never needed to open evaluator output.
- **Hard part:** This authenticates output consistency but does not provide active-secure garbling; document the semi-honest model.

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
**Not here:** Wire transport, OT frames, authenticated output opening, or production parameter claims.

### Phase 2 — Bounded, bound session frames

**Covers:** R-4, R-5.

**Goal:** Two local roles reject malformed, replayed, mismatched, and over-budget frames before evaluation.

**Tasks — landed**

- [x] Define the v1 header and sequence rules: role, variant, 32-byte parameter fingerprint, circuit digest, session ID, and per-direction monotonic sequence.
- [x] Implement canonical encode/decode with a 64 KiB payload cap and borrowed payload parsing.
- [x] Add fallible `BitGcFrameTransport` send/receive/cancel seams and terminal abort/cancel states.
- [x] Test malformed, truncated, oversized, replayed, reordered, wrong-role, and wrong-binding frames, plus injected transport failure and cancellation during handshake/active states.

**Validation (2026-10-09):** `volar-mpc` BitGC session unit tests passed (10/10) with an offline workspace-patched Cargo run; `git diff --check` and targeted rustfmt passed. The codec rejects noncanonical/truncated/oversized frames, and the state machine rejects mismatches before returning payloads. The envelope is not authenticated: peer/transcript authentication, canonical digest construction, OT, and protected-session admission remain open.
**Not here:** Live OT or any deployment transport.

### Phase 3 — Input OT and authenticated opening

**Covers:** R-2, R-3, R-6.

**Goal:** A loopback session transfers only selected evaluator labels and opens outputs by hash match.

**Tasks**

- Bind input ownership and OT choices to the session transcript.
- Stream bounded offline evaluator material and gate records.
- Commit output-label hashes and implement evaluator hash-match opening.
- Propagate cancel and transport errors through both role machines.
- Test wrong choices, altered records, bad output labels, and abort at every state.

**Proof:** Two distinct role processes compute correct outputs; neither holds the other's private inputs or secret key.
**Not here:** Malicious security or external network deployment.

### Phase 4 — Cirrus adapter and differential closeout

**Covers:** R-1, R-3, R-4, R-5, R-6, R-7.

**Goal:** Cirrus drives the same shared protocol and agrees with Volar and the independent artifact on bounded circuits.

**Tasks**

- Replace the current Cirrus reference-equation path with the shared role API.
- Adapt Cirrus record push/pull to bounded session chunks.
- Run circuit truth tables and sub64 accounting across both variants.
- Add property-based topology and frame mutation tests.
- Record remaining parameters and review blockers in the ledger.

**Proof:** Volar/Cirrus/artifact outputs and record counts agree on the conformance fixtures.
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
