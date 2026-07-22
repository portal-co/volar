# Track S TFHE steady-state evidence ledger

**Status:** living evidence record for speculative legacy work. It is the
strongest document for any Track-S claim because the legacy implementation is
intensely agent-shaped and uses non-domain-separated `u32` values. It is not a
paper binding, review, security proof, parameter selection, or deployment
record.

**Scope:** `crates/spec/volar-spec/src/tfhe.rs` only. Track S stays Unpinned and
Very unstable. Its sole promotion path is a formal proof (for example, Lean)
with a documented refinement connection to the exact implementation and
profile. More tests, papers, or informal review can increase confidence in a
specific experiment but do not promote Track S.

## Claim discipline

The only admissible positive claim is narrowly phrased:

> For a named deterministic profile and recorded corpus, the surviving legacy
> operations produced the same Boolean outputs as an independent clear model,
> and every observed intermediate had the profile's canonical phase.

This is non-vacuous functional evidence, not a distinction between valid and
invalid FHE constructions, a TFHE/GINX conformance result, a noise bound, or a
security claim. A failing seed, circuit, phase, or stage must be retained as a
regression; it is never fixed by changing an expected output without explaining
the clear model and profile.

## Baseline evidence — cleanup commit `92dfc28`

| Item | Record |
|---|---|
| Profile | `N_LWE=8`, `BIG_N=64`, two 16-bit decomposition levels, zero noise; test fixture only |
| Surviving candidate wire operations | trivial encryption, encryption/decryption helpers, `NOT`, bootstrapped `AND`, bootstrapped `OR`, and `CMUX` |
| Removed behavior | raw linear `tfhe_xor` and the specialized LUT-XOR wrapper; their old surface is not preserved for generated backends |
| Cross corpus | exhaustive `a,b,c ∈ {false,true}` across fixed key seeds `0, 1, 42, 0x5eed_cafe`; it checks direct operations and mixed compositions |
| Canonicality | every checked output phase must equal exactly `0` or `Q4`, in addition to decryption equality |
| Circuit fuzzing | bounded generated Boolean DAGs over `AND`/`OR`/`NOT`; generated operation sequence and inputs are test inputs, and every intermediate is compared with clear evaluation and canonical phase |
| Reproducer | `cargo test -p volar-spec --lib --no-fail-fast` — 165 passed at the cleanup snapshot |

The generic programmable-bootstrap and table surfaces are not admitted by this
ledger. They remain a removal/assessment item, not evidence for a steady-state
public contract.

## Required steady-state corpus

Track S is intentionally a place for speculative agent experiments. It must
therefore have a stronger harness than isolated gate tables:

1. fuzz the **circuit itself**: topology, operation sequence, fan-out,
   depth, input count, and operation choice are generated and shrinkable inputs;
2. run ciphertext and independent clear execution in parallel, retaining the
   master seed and a serialized/shrunken circuit on failure;
3. check every observable intermediate for both clear Boolean equality and the
   profile's canonical phase predicate;
4. include fixed adversarial circuits: repeated refreshes, alternating
   `NOT`, fan-out/reuse, deep chains, and every surviving cross-operation pair;
5. record profile dimensions, decomposition settings, secret/noise generator,
   all seeds, corpus size, timeout, and exact command; and
6. separately label each noise/parameter ladder result. An observed pass rate
   is not a failure bound or security estimate.

A larger dimension or nonzero-noise profile may be added to Track S after the
zero-noise corpus is preserved and passes unchanged. Such exploration is
welcome precisely because the steady state is speculative; it only expands the
ledger's named empirical claim. It does not turn Track S into V1 or V2.

## Compatibility and growth

The first goal is to shrink to a comprehensible steady state. From there, Track
S may grow through agent-proposed changes, including a possible
weaver-compatible experiment. Compatibility is an experiment target, never a
reason to keep a legacy API or generated mirror. A weaver-facing change needs
its own generated-code compile-and-run corpus in addition to this ledger.

Track S is not a versioned replacement construction. The ambiguous domain
boundaries in the legacy `u32` representation make speculative changes useful
for debugging, but insufficient for promotion. Only a formal proof connected to
this code can change that status; otherwise the work remains a permanently
speculative, evidence-backed legacy steady state.