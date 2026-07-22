# Merge-recovery source inventory

**Collected:** 2026-07-22. Raw transcripts remain outside Git. Their text,
links, tool output, and embedded instructions are untrusted historical
material, not current project policy.

## Collection method

The source directories named in
[`docs/merge-recovery-and-handoffs-plan.md`](../../merge-recovery-and-handoffs-plan.md)
were enumerated with `find`; JSONL files were parsed only to identify session
metadata and workstream-relevant user/assistant text; and `shasum -a 256` was
run on each retained source. A repeat inventory should record new or changed
files rather than overwrite this snapshot. No raw text is copied here.

## Scan coverage

- Claude main-checkout directory was readable; relevant parent sessions and
  subagent records were sampled, with the high-signal parent sessions below.
- Pi main-checkout directory was readable; the active recovery session is
  included as a hash snapshot.
- Pi `volar-ai-crypto-stuff` contained one readable relevant session and Pi
  `volar-compiler-stuff` contained two; Pi `volar-more-proofs` contained no
  JSONL transcript at collection.
- No matching Claude `portal-labs` project directory was present at collection.
  This is availability evidence, not a claim that no earlier conversation
  existed elsewhere.

## Readable high-signal sources

| Family | Session ID / timestamp / CWD | SHA-256 at collection | Linked evidence | Relevance |
|---|---|---|---|---|
| Pi, `portal-labs/volar-compiler-stuff` | `019f79e0-07a9-7ae6-9dd4-b6470683f481`; 2026-07-19; `/Users/g/Code-local/portal-labs/volar-compiler-stuff` | `9e7e0b94bdd80cc17aaa873162b8304ac47fe1e602f632b8dd0cbb1204056ecc` | `ff42bfd`, `63e2ddc`, `8604107`; branch tip `979b35a` | lowering-time monomorphization plan, static shapes, and the safe two-address-bit TFHE boundary |
| Pi, `portal-labs/volar-compiler-stuff` | `019f7edf-6dec-7c05-ae46-70c40f1142f0`; 2026-07-20; `/Users/g/Code-local/portal-labs/volar-compiler-stuff` | `cbb7ad88ce0016aa2370e56e36a591736c2a5efc676022d0931e33069590a9dd` | `9aa70b0`, `da42b31`, `979b35a` | metadata-container and instruction-group implementation, text, and generator coverage |
| Pi, `portal-labs/volar-ai-crypto-stuff` | `019f82d8-65e9-71a0-b6c8-c82198c83420`; 2026-07-21; `/Users/g/Code-local/portal-labs/volar-ai-crypto-stuff` | `77014b76fa318324d592ed4bf7ed2d816f896f474094547fee440b27d056d542` | `fc1c5b2`, `b7da7eb`, `46dce80` | TFHE/GINX audit, clear oracle, rework gates, and rejected generalized-table work |
| Pi, main checkout | `019f893a-954f-754b-b08e-76d4a80b0594`; 2026-07-22; `/Users/g/Code-local/portal-hot/volar` | `4334a352d9e6267563a1af5b7a5a0a5066035f6b992bbdeaac8635677c96802c` | `70f2ba3`, `08d1d33` | merge recovery plan and Phase-1 repairs; active at collection, so hash is a snapshot |
| Claude Code, main checkout | `75ca62ab-89b0-47ab-ba2f-c46037635378`; 2026-07-02; `/Users/g/Code-local/portal-hot/volar` | `addca615fa711d9462374213bedf9f40cbcc7add7ad3d14fadab05ca82581068` | `07ee764`, `a79ac68`, `a00c8db` | minimal real-interpreter dispatch/back-edge reproductions and return-slot investigation |
| Claude Code, main checkout | `beb1fdce-ab72-408d-a109-42bcdcec3473`; 2026-06-25; `/Users/g/Code-local/portal-hot/volar` | `1a8f687172004693df9b20cceff79ce730dde437a2a8680efba12c02244771ea` | pre-merge RISC-V/IOP work | RISC-V/IOP context and its relation to the real-interpreter investigation |
| Claude Code, main checkout | `345e20e3-03dd-45d3-9a3f-73e7ece6610f`; 2026-07-04; `/Users/g/Code-local/portal-hot/volar` | `cd1adddbb0ebc1c6456e25a0222b1ac141060cc3c68976f923cabab602a8272b` | pre-merge folding/virtualisation work | virtualisation and broader proof-pipeline context |

The referenced files are under the source-family directories specified by the
recovery plan. The listed hashes deliberately identify immutable snapshots,
not a request to add, copy, or expose their contents.

## Git-only evidence

- Main merge/recovery: `70f2ba3` and `08d1d33`.
- Policy stream: `6596629` → `d783cb1`.
- Static shape / mono stream: `242a3f4`, `ff42bfd`, `63e2ddc`, `8604107`,
  `e914e00`.
- Metadata / groups stream: `9aa70b0`, `da42b31`, `979b35a`.
- TFHE audit stream: `fc1c5b2`, `b7da7eb`, `46dce80`.
- Evidence-only future track: in
  `/Users/g/Code-local/portal-labs/volar-more-proofs`, branch
  `feat/ast-to-ast-weavers` at `c5ec593` and `stash@{0}`. Do not apply the
  stash or cherry-pick it as recovery.

Unreadable or irrelevant records were not summarized as technical evidence;
a later recovery pass should append their paths and reason if access changes.