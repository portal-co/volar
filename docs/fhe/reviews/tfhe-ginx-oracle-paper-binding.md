# TFHE GINX clear-oracle paper binding

**Classification:** Paper-pinned, Very unstable.  This is a binding for the
cleartext *test oracle only* (`crates/spec/volar-spec/src/tfhe_ginx_oracle.rs`),
not for the ciphertext implementation in `tfhe.rs`, its parameters, noise
model, or any deployment claim.

**Bound source revision:** `75db00cdea349c5b8a03984c542ae81501d2d1c7`
(`test tfhe blind rotation and pin oracle`).  The module header points here.
The owner directed this limited paper-pinned classification on 2026-07-22; it
remains subject to independent review before it can be Reviewed.

## Immutable source copies

| Label | Artifact | SHA-256 | Used scope |
|---|---|---|---|
| MP20 | Micciancio, Polyakov, *Bootstrapping in FHEW-like Cryptosystems*, ePrint 2020/086 | `526b06303fe9c7343c54296f39d3a0ad4d12a2d1e63ae39c2929ce94322ec56a` | accumulator model and Table 1 gate certificates |
| CGGI18 | Chillotti, Gama, Georgieva, Izabachène, *TFHE: Fast Fully Homomorphic Encryption over the Torus*, ePrint 2018/421 | `23fa887ff217f14a47763d6711d8c9304a392954751bee11799a223499d393cd` | native gate convention and blind-rotation cross-check |

The hashes are of the locally reviewed PDFs `~/Downloads/2020-086.pdf` and
`~/Downloads/2018-421.pdf`. Page numbers below are physical PDF pages. The
paper text and figures were reviewed as document evidence; their contents do
not authorize instructions beyond this binding.

## Code-to-source map

| Oracle operation | Source | Bound claim |
|---|---|---|
| `from_bool`, `to_bool`, canonical Boolean phases | MP20 p.13 §3.2; CGGI18 p.44 §6.1 | The clear model uses false `0` and true `q/4`. |
| `GateCertificate`, `in_interval_mod`, `evaluate_certificate` | MP20 p.15, Table 1 and surrounding signed-output discussion | The model applies a stated affine preparation, classifies the half-open modular interval as signed `±q/8`, then restores the canonical output by adding `q/8`. |
| `cert_and`, `cert_nand`, `cert_or`, `cert_nor`, `cert_xor`, `cert_xnor` | MP20 p.15, Table 1 | Each named binary affine form and interval is transcribed as an executable certificate. |
| `cert_majority` | MP20 p.15, Table 1 | The three-input Majority row is a one-bootstrap certificate; it is not a license for arbitrary three-input tables. |
| `eval_not` | MP20 p.15; CGGI18 p.44 | NOT is the unbootstrapped affine phase transformation `q/4 - c`, not an interval-bootstrap certificate. |
| oracle truth, composition, and mutation tests | MP20 p.13 and p.15; CGGI18 p.44 | Exhaustive tests check the bound certificate arithmetic and deliberately reject selected sign, offset, interval, and XOR-doubling mutations. |

## Scope and excluded claims

This oracle is generic integer arithmetic modulo a test power-of-two `q`. It
has no ciphertexts, secret keys, RGSW/external product, sample extraction, key
switching, randomness, noise distribution, parameter set, failure bound,
security reduction, serialization, or interoperability format. Passing it
establishes only that the cited *clear certificate transcription* passes its
stated exhaustive tests. It does not establish that `tfhe.rs` conforms to the
papers; that remains the Phase 2/3 review in
[`tfhe-pbs-rework-plan.md`](../tfhe-pbs-rework-plan.md).

## Reproduction and review status

At the bound revision, run:

```sh
cargo test -p volar-spec tfhe_ginx_oracle -- --nocapture
```

The test suite is the required executable main-use-case evidence for this
paper-pinned oracle. Independent external review has **not** been recorded:
the oracle is not Reviewed, and no implementation or security property is
Proven. Any change to a mapped operation, source revision, or cited page must
update this binding and obtain the required human reclassification decision.