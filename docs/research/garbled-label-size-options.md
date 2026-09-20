# Garbled-label size and construction options

**Status:** research note, 2026-09-14. This note deliberately does **not**
change Volar's `U16` (128-bit) garbling labels. Label width is a security
parameter and construction choice, not a material-cache optimization.

## Decision

Keep the current 128-bit labels for the durable held-material path. In
particular, do not introduce an 8-byte representation merely to make several
values fit in one AES material block. Current `MaterialBlockLayout` packing is
therefore a future capability, not an authorization to truncate labels.

The existing garbling module is explicitly experimental and its VOLE binding is
not reviewed. Any change to label size or garbling construction must first
supply a construction-specific security proof/parameter analysis, update the
wire/table format as one coherent scheme, and receive a cryptographic review.
A cut-and-choose layer is an active-security mechanism; by itself it does not
turn arbitrary shortened semi-honest labels into a sound 128-bit-label
replacement.

## What short labels do and do not buy

A conventional free-XOR label is a security-parameter-sized secret offset plus
a false base. Reducing its bit length reduces brute-force/collision margins and
may change concrete attacks that scale with the total number of garbled AND
gates or executions. It also changes every stored label, OT payload, AND table,
and strict output check. It cannot be safely implemented as a post-garbling
byte truncation.

Bellare, Hoang, Keelveedhi, and Rogaway give a different fixed-key-permutation
construction with 80-bit *tokens* (not AES-80): AES is still a 128-bit public
fixed-key permutation. Their A3 concrete analysis depends on its
construction-specific injectivity property and their A4 tradeoff supports
GaX/GaXR at greater cost. That is a candidate to evaluate as a wholesale
alternative scheme, not evidence that Volar's current free-XOR/half-gate-like
format may use 80-bit labels. [BHKR13]

More importantly, Choi et al. show concrete attacks on common fixed-key-AES
half-gate instantiations whose cost is roughly `2^k / C`, with `C` the number
of non-free gates across executions. They report a practical concern for
80-bit labels around `C ≈ 10^9`. Their mitigations are construction-specific
hash/tweak changes, including multi-instance handling; this is direct evidence
against treating 80 or 96 bits as an unqualified storage optimization. [CKPR19]

## Better directions than raw truncation

1. **Improve garbled-table communication while retaining κ-bit labels.**
   Rosulek and Roy's Three-Halves construction keeps free XOR and reduces AND
   communication to `1.5κ + O(1)` bits by slicing/dicing labels. It is a
   potential replacement for the AND-gate backend, not a shorter durable-label
   encoding; it needs its generalized correlation-robustness assumption and a
   full implementation/proof review. [RR21]

   Sliced-garbling follow-up work reports `4κ/3 + O(1)` AND communication for
   a particular construction. This is likewise table compression, not a
   drop-in reduction of stored label entropy. [KRS24]

2. **Compress input-label delivery rather than labels at rest.**
   *TinyLabels* targets communication of input labels with an offline/online
   construction and Ring-LWE machinery. It may be relevant to bulk OT/input
   transfer, but it does not provide a 16-byte durable opaque label format and
   has materially different assumptions/complexity. The explicit, server-only
   cross-repository plan is in
   [`tinylabels-cross-repository-plan.md`](tinylabels-cross-repository-plan.md).
   [HLL24]

3. **Evaluate a different complete garbling scheme.**
   BHKR fixed-key-AES token constructions are the most concrete short-token
   candidate found. A proper evaluation must compare: privacy and authenticity
   notions, free-XOR compatibility, GRR compatibility, exact multi-instance
   bound, table layout, OT interaction, and the effect on the strict-chain
   base/label relation. No partial adapter conversion is valid.

4. **Use authenticated garbling / active security for its own goal.**
   Authenticated garbling and cut-and-choose can constrain a malicious garbler
   and provide integrity/correctness guarantees. They do not provide a generic
   compensating proof that an arbitrary 64- or 96-bit label truncation retains
   the privacy/authenticity security of a 128-bit label scheme. If an audited
   evaluator-only execution mode is desired, it must be specified as a distinct
   privacy-free or verifiable-computation protocol with its own threat model,
   not silently selected by the durable store. [WRK17] [KOS16]

## Required gate before any smaller durable format

A future proposal must name:

- the complete garbling construction and its standard/modelled assumptions;
- exact label width and target work factor, including all sessions and AND
  tables under one correlation scope;
- privacy, authenticity, evaluator compromise, and active-adversary goals;
- whether an auditor observes execution, what it verifies, and what remains
  hidden from it;
- any cut-and-choose/authenticated-garbling protocol, statistical parameter,
  and how it binds labels, circuit identity, inputs, and outputs;
- versioned migration and rejection behavior for durable records; and
- independent cryptographic review plus parameter test vectors.

Until then, `U16` stays fixed. The new deferred remap circuit only changes a
restored label's false base with a free XOR; it never shortens, decodes, or
re-encodes labels.

## Sources

- **[BHKR13]** Mihir Bellare, Viet Tung Hoang, Sriram Keelveedhi, and Phillip
  Rogaway, *Efficient Garbling from a Fixed-Key Blockcipher*, IACR ePrint
  2013/426. https://eprint.iacr.org/2013/426
- **[CKPR19]** *Better Concrete Security for Half-Gates Garbling (in the
  Multi-Instance Setting)*, IACR ePrint 2019/1168.
  https://eprint.iacr.org/2019/1168
- **[RR21]** Mike Rosulek and Lawrence Roy, *Three Halves Make a Whole?
  Beating the Half-Gates Lower Bound for Garbled Circuits*, IACR ePrint
  2021/749. https://eprint.iacr.org/2021/749
- **[KRS24]** *On the Feasibility of Sliced Garbling*, IACR ePrint 2024/389.
  https://eprint.iacr.org/2024/389
- **[HLL24]** *TinyLabels: How to Compress Garbled Circuit Input Labels,
  Efficiently*, IACR ePrint 2024/2048.
  https://eprint.iacr.org/2024/2048
- **[WRK17]** Xiao Wang, Samuel Ranellucci, and Jonathan Katz,
  *Authenticated Garbling and Efficient Maliciously Secure Two-Party
  Computation*, IACR ePrint 2017/030.
  https://eprint.iacr.org/2017/030
- **[KOS16]** Marcel Keller, Emmanuela Orsini, and Peter Scholl,
  *Constant-Round Maliciously Secure 2PC*, IACR ePrint 2016/805.
  https://eprint.iacr.org/2016/805
