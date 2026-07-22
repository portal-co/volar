# Policy and reliability handoff

- **Current merged base:** `08d1d33`; policy stream `6596629` → `d783cb1`.
- **Evidence:** merge diffs, `AGENTS.md` changelog (2026-07-22), and the
  recovery plan. Historical sessions are indexed in [sources.md](sources.md).
- **Implemented and verified:** the merged policy direction is model-neutral:
  enforcement-style capability tiers and mandatory model identification were
  removed. The current `AGENTS.md` changelog is the controlling migration note.
- **Current failures / blockers:** `docs/agents-guide.md` and
  `docs/reliability.md` still contain the old tier/enforcement language. This
  documentation drift is a Phase-3 reconciliation task, not a license to
  ignore the reliability system while it remains stale.
- **Invariants and non-goals:** retain reliability labels, Experimental/Hazmat
  review evidence, paper binding/review plans, deterministic-spec rules, typed
  IR, provenance non-invention, generated-code execution tests, and the
  ZK/non-ZK `Tagged<Z, _>` / `NonZk` boundary. Relaxed policy does not permit a
  semantic merge resolution merely because one branch compiles.
- **Collision changes since the original plan:** policy relaxation removed the
  enforcement mechanism, not cryptographic review obligations. The documents
  currently mix these two concepts and must not be read as restoring model
  gating by accident.
- **Next smallest safe action:** draft a narrow, dated merged-tree update to
  `AGENTS.md`, `docs/agents-guide.md`, `docs/reliability.md`, and `docs/README.md`:
  remove tier/model-gating and obsolete Experimental feature-gate claims while
  preserving the evidence-based requirements above. Ask the owner before
  changing reliability-level policy itself.
- **Completion evidence:** docs agree with `AGENTS.md`, link this handoff, and
  do not direct a new agent to removed tier or model-identification steps.
- **Documents to update before coding:** `AGENTS.md`, `docs/agents-guide.md`,
  `docs/reliability.md`, `docs/README.md`, and the plans that currently cite
  removed restrictions.