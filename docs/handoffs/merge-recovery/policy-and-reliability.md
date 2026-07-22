# Policy and reliability handoff

- **Current merged base:** `08d1d33`; policy stream `6596629` → `d783cb1`.
- **Evidence:** merge diffs, `AGENTS.md` changelog (2026-07-22), and the
  recovery plan. Historical sessions are indexed in [sources.md](sources.md).
- **Implemented and verified:** the merged policy direction is model-neutral:
  enforcement-style capability tiers and mandatory model identification were
  removed. The current `AGENTS.md` changelog is the controlling migration note.
- **Phase 3 reconciliation:** `AGENTS.md`,
  `docs/agents-guide.md`, `docs/reliability.md`, and `docs/README.md` now use
  the model-neutral, evidence-based policy and link this handoff. The active
  plan documents no longer direct contributors to capability tiers or the
  removed Experimental Cargo feature.
- **Current failures / blockers:** no policy-document blocker remains. The
  unresolved LIR const parameter `L` and missing LLVM environment are separate
  source and environment blockers recorded in the sibling handoffs.
- **Invariants and non-goals:** retain reliability labels, Experimental/Hazmat
  review evidence, paper binding/review plans, deterministic-spec rules, typed
  IR, provenance non-invention, generated-code execution tests, and the
  ZK/non-ZK `Tagged<Z, _>` / `NonZk` boundary. Relaxed policy does not permit a
  semantic merge resolution merely because one branch compiles.
- **Collision changes since the original plan:** policy relaxation removed the
  enforcement mechanism, not cryptographic review obligations. Phase 3 removed
  the resulting documentation mix-up; retain that distinction in later edits.
- **Next smallest safe action:** before changing reliability policy, ask the
  owner and update this handoff plus `AGENTS.md`, `docs/agents-guide.md`,
  `docs/reliability.md`, and `docs/README.md` together. Otherwise proceed to
  the source blocker in the static-shapes handoff.
- **Completion evidence:** the listed documents agree with `AGENTS.md`, link
  this handoff or the recovery index, and do not direct a new agent to removed
  tier or model-identification steps.
- **Documents to update before coding:** only the policy documents above when
  an owner-approved reliability-policy change is in scope; otherwise update the
  handoff for the subsystem being changed.
