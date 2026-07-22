# Phase 3 external-reference reconnaissance: `tfhe-go`

**Status:** reconnaissance and reproducibility record only; **not a Gate C
acceptance**. This records the selected independent implementation and the
first compatibility result without claiming ciphertext interoperability,
parameter equivalence, a noise result, or a security assessment.

## Pinned external reference and reproducible evidence

- Repository: <https://github.com/sp301415/tfhe-go>
- Revision: `a5cdbf4c57d30909ee8a04c22db762ffec094d42` (`master` when cloned)
- Local evidence checkout: `/tmp/volar-tfhe-go`
- Command executed on 2026-07-22: `cd /tmp/volar-tfhe-go && go test ./tfhe`
- Result: pass (`ok github.com/sp301415/tfhe-go/tfhe`, 1.512 s)
- Go toolchain: `go1.26.4 darwin/arm64`

The reference suite exercises encrypted binary AND/NAND/OR/NOR/XOR/XNOR and
its configured parameter tests. That confirms the selected revision is
executable; it does **not** compare a Volar ciphertext to a `tfhe-go`
ciphertext.

## Convention comparison completed

| Concern | `tfhe-go` evidence | Volar evidence | Result |
|---|---|---|---|
| Binary-gate affine preparation | `tfhe/binary_evaluator.go` at the pinned revision: AND/NAND/OR/NOR prepare a sum/difference with `±2^(logQ-3)`; XOR/XNOR double an affine form with `±2^(logQ-2)`, then bootstrap the sign LUT. | `tfhe_ginx_oracle.rs`, bound by [the paper binding](tfhe-ginx-oracle-paper-binding.md), uses MP20 Table 1 certificates. | Formula family is a useful independent cross-check; it is not byte-level compatibility. |
| Phase quantization | `tfhe/bootstrap.go`: `ModSwitch(x) = round(x * 2*LUTSize / Q) mod 2*LUTSize`. | `tfhe.rs`: `torus_to_exp` rounds a `u32` torus value to `Z_(2N)` by a power-of-two shift. | Algebraically comparable only when `Q=2^32` and `LUTSize=N`; rounding ties and accepted domains still need a test-vector comparison. |
| Blind-rotation sign | `bootstrap.go` initializes with `-ModSwitch(b)` and applies monomial/sub-one updates using `-ModSwitch(a_i)` in its original path. | `blind_rotate_with_poly` initializes with `X^(-b_exp)` and conditionally applies `X^(a_exp)` under the encrypted selector. | The clear final exponent convention is plausibly aligned with the secret phase, but RGSW/key layout has not been mapped. |
| Accumulator/LUT completion | `tfhe/bootstrap_lut.go` fills logical bins, rotates by its offset, then negates the antipodal tail. | `tfhe.rs` has a hand-built AND polynomial and a restricted `TfheBootstrapTable` encoding. | Both use a negacyclic signed completion, but their exact layouts must be compared via shared vectors. |
| Bootstrap order | `ParamsBinary` selects blind-rotate then key-switch. | `tfhe.rs` does blind rotation, extraction, then key switch. | Structurally comparable; no equivalence conclusion follows. |
| Test parameters | `ParamsBinary`: LWE dimension 687, GLWE rank 2, polynomial rank 512, nontrivial configured noise/decomposition. | Current conformance fixture: LWE dimension 8, ring dimension 64, rank-one RLWE representation, exact 32-bit decomposition, zero noise. | **Not compatible.** Current Volar vectors cannot be serialized into the reference or used for an end-to-end differential test. |

## What Phase 2 evidence now covers

Volar's focused `tfhe.rs` conformance suite independently checks polynomial
rotation, exact-grid blind rotation of an arbitrary accumulator, CMUX on
arbitrary polynomials, sample extraction, and key switching. The blind-rotation
case was added at source revision
`75db00cdea349c5b8a03984c542ae81501d2d1c7` and is intentionally independent
of `blind_rotate_with_poly`, `poly_rotate`, and `sample_extract` for its
expected result. This is useful Stage-2 evidence, but it is not a
reviewer-approved proof that all source-to-paper mappings are complete.

## Blocking work before a real differential test

1. A human cryptographic reviewer must choose a parameter model and approve a
   complete mapping of torus width, LWE/GLWE rank and dimensions, secret/noise
   distribution, gadget decomposition, key-switch direction, and bootstrap
   order. Do not import `ParamsBinary` by matching a few field names.
2. Define an explicit shared vector format for plaintexts, keys, ciphertexts,
   LUT coefficients, and intermediate stages. It must state coefficient order,
   torus endianness, canonical representative, and RNG/noise source. Neither
   implementation's in-memory serialization is an acceptable implicit format.
3. Compare at least modulus switch, accumulator formation, blind rotation,
   extraction, key switch, and the selected gate certificates at the agreed
   parameters. For each divergence, either fix the mapping or record the
   mathematical reason in `tfhe-ginx-core-spec.md`; never adjust an expected
   output to conceal it.
4. Only after that review, run a separately approved nonzero-noise experiment
   and report observed failures independently from any proven failure bound.

Until these blockers are cleared, Gate C remains open and the legacy `tfhe.rs`
classification remains Unpinned and Very unstable. This record is deliberately
not a promotion artifact.