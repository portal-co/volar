# TinyLabels Ring-LWE implementation research

Status: implementation design input, not a security claim. This note reviews
the primary paper and the authors' artifact/reference implementation so that a
Rust backend does not accidentally promote the current local label selector to
a cryptographic protocol.

## Sources pinned for implementation work

- Dietz, Li, and Lin, *TinyLabels: How to Compress Garbled Circuit Input
  Labels, Efficiently*, [IACR ePrint 2024/2048](https://eprint.iacr.org/2024/2048.pdf),
  especially Construction 3 (pp. 25--26) and the evaluated parameters
  (pp. 33--36). The user-provided local copy is
  `/Users/g/Downloads/2024-2048.pdf`.
- The [IACR EUROCRYPT 2025 artifact](https://artifacts.iacr.org/eurocrypt/2025/a14/)
  links the authors' evaluation code and records a “Results Reproduced” badge.
- The implementation target is the artifact's pinned
  [`46107cce5e1c10f3906da07c6c3666d3f40eb734`](https://github.com/MarianDietz/tinylabels/tree/46107cce5e1c10f3906da07c6c3666d3f40eb734)
  revision. Its [README](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/README.md)
  expressly says not to use it in production and makes no security guarantee.
  The operative algorithms and file layout are in
  [`batchselect.h`](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/native/tinylabels/batchselect.h)
  and
  [`batchselect.cpp`](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/native/tinylabels/batchselect.cpp).

The paper calls the construction *batch-select*: for message vectors
`l1`, `l2` and a binary selector `y`, the recipient learns only
`l1 * y + l2`, coordinate-wise. It is input-label delivery; it neither
reduces garbled tables nor authenticates a garbler. [Paper, pp. 8--10.](https://eprint.iacr.org/2024/2048.pdf)

## Exact construction to implement

Let `Encode` pack plaintext slots into `R_p = Z_p[X]/(X^n + 1)`, preserving
component-wise addition and multiplication. Let `q = p * Delta`, and treat
the packed values as elements of `R_q` after multiplying message data by
`Delta`.

The two internal components are as follows.

1. `LEnc` encrypts a vector `s`. It returns a random vector `r` and an
   `ell = log2(w')`-level binary-tree ciphertext. For a supplied packed
   vector `a`, `Digest(a)` constructs the tree root `d_a`; `Eval(ct, a)`
   returns `r * d_a - s ⊙ a` plus bounded noise. Its public values are two
   `m`-element Ring-LWE vectors, and each tree level has `w' * 2m` ring
   elements. [Paper, Construction 1, pp. 21--22.](https://eprint.iacr.org/2024/2048.pdf)
2. `LHE` linearly combines one reusable encryption and one per-use
   encryption. Given public `a`, `Enc1(m1)` creates
   `ct1 = a * s1^T + m1 * g^T + E`; `Enc2(m2)` creates
   `ct2 = a * s2 + m2 + e`. `KeyGen` digit-decomposes the selected digest
   and returns the single ring element `s1^T * g^-1(d_y) + s2`. Decryption
   combines `ct1`, `ct2`, and that key, then subtracts `a * sk`.
   [Paper, Construction 2, pp. 23--24.](https://eprint.iacr.org/2024/2048.pdf)

The public `Sel` construction is therefore four phases, which must remain
separate in the Rust API:

| Phase | Inputs held by the input-label sender | Output / reusable state |
| --- | --- | --- |
| `setup` | Parameters and RNG | `pp = (LHE.pp, LEnc.pp)` |
| `enc1` (reusable) | `l1` | `LEnc.Enc(Encode(l1) * Delta) = (r, LEnc.ct)`, then `LHE.Enc1(r)`; output `ct1 = (LEnc.ct, LHE.ct1)` and retain `s1` |
| `enc2` (per use) | `l2` | Sample `e_LEnc`, run `LHE.Enc2(Encode(l2) * Delta + e_LEnc)`; output `ct2` and retain `s2` |
| `keygen` / `dec` (after choices) | selector `y` | Pack `y`, calculate `d_y = LEnc.Digest(Encode(y))`, make `sk_y = LHE.KeyGen(s1, s2, d_y)`; the recipient recomputes the digest, evaluates LHE and LEnc, subtracts, divides/rounds by `Delta`, and decodes `Encode^-1` |

This is exactly the ordering in Construction 3. In particular, `ct1` is the
reusable item and `ct2` is not; `LEnc`'s evaluation-noise distribution is
required inside `Enc2`, not an optional post-processing step. [Paper,
pp. 25--26.](https://eprint.iacr.org/2024/2048.pdf)

### Mapping labels is still a design item

The paper's generic batch-select message space is `Z_p^ell` where
`p^ell >= 2^lambda`. The evaluated profile uses three 50-bit field elements
per 128-bit label. The paper explicitly notes that ordinary garbling keys
need a translation layer when the batch-select message space and key space
differ. [Paper, pp. 26 and 33.](https://eprint.iacr.org/2024/2048.pdf)

For this repository, do **not** treat a `[u8; 16]` free-XOR label as an
implicitly specified field encoding. The implementation needs a versioned,
canonical byte-to-`Z_p^3` injection and its inverse, or the paper's key
translation ciphertexts. Given an agreed encoding, each Boolean input wire
would supply `l1 = K1 - K0` and `l2 = K0`; batch-select then reconstructs
`K_y`. This choice must be reviewed alongside the eventual `MachineHandler`
and symbolic-hash protocol, because it fixes both a wire format and who may
hold the selection bits.

## Author-reference profile and serialization

The profile below is a compatibility target for the authors' benchmark, not a
parameter set suitable for the microcontroller deployment:

| Item | Pinned reference value |
| --- | --- |
| Ring | `R_q = Z_q[X]/(X^4096 + 1)` |
| Packing / `w'` | `n = 4096`, `w' = 512`, `ell = 9`; 2^21 scalar slots, or 699,050 128-bit values at three slots per value |
| Moduli | `p = 1_125_899_906_826_241` (50 bits), `Delta = 576_460_752_303_415_297` (59 bits), both 8192-NTT-friendly; `q = p * Delta` |
| Gadget | `m = 4`, `g = 2^28` |
| Reference noises | small normal: standard deviation `4`, max `512`; large normal: standard deviation `1000`, max `128000` |
| Reuse benchmark in paper | `T = 2^15 = 32,768` |

The paper describes the same ring degree and 50/59-bit split, selects
`m = 4`, `g = 2^28`, and evaluates a 699,050-message batch at 128-bit
security. [Paper, pp. 33--35.](https://eprint.iacr.org/2024/2048.pdf) The
literal primes and the reference's truncated-normal parameters are from the
pinned code: `PlainModulus::Batching(4096, 50)` and
`CoeffModulus::Create(4096, {59})`, together with the constants in
[`batchselect.h`](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/native/tinylabels/batchselect.h).

The artifact's `*.bin` files are **not a protocol serialization**. They are
unversioned host-endian `fwrite` streams of SEAL's two-RNS-limb, NTT-form
`uint64_t` polynomial representation. There is no header, parameter ID,
length, endianness marker, checksum, or read-length validation. At the pinned
profile each polynomial is `2 * 4096 * 8 = 65,536` bytes. The order and exact
byte counts are:

| File | Raw order | Size |
| --- | --- | ---: |
| `pp.bin` | LHE `a[w']`, then LEnc `b[2m]` | 34,078,720 B |
| `st1.bin` | LHE `s1[m]` | 262,144 B |
| `ct1.bin` | LHE `ct1[w' * m]`, then LEnc `ct[ell * w' * 2m]` | 2,550,136,832 B |
| `st2.bin` | LHE `s2` | 65,536 B |
| `ct2.bin` | LHE `ct2[w']` | 33,554,432 B |
| `sk.bin` | LHE `sk_y` | 65,536 B |

Those layouts follow the `save_*` / `read_*` methods in the
[pinned header](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/native/tinylabels/batchselect.h).
The authors likewise warn that their storage is not optimized: 128 bits are
used to store a 109-bit coefficient. [Artifact README.](https://artifacts.iacr.org/eurocrypt/2025/a14/readme.html)

Consequently, Rust must define its own canonical wire frames; it must not
claim cross-platform interoperability by reading those raw files. A test-only
reader may support them on a matching little-endian, pinned-NTT representation
to aid diagnosis, but it must be feature-gated and reject all unrecognized
parameter fingerprints.

## Reference build and compatibility test

The authors' tree is a patch of Microsoft SEAL 4.1.1 with an added `onoff`
scheme and a change permitting a plaintext modulus that divides the ring
modulus. It accepts only `q = p * Delta` with two NTT-friendly primes, where
`p` is also the plaintext modulus; it does not implement the paper's random-
oracle `ct2` compression. [Artifact README.](https://artifacts.iacr.org/eurocrypt/2025/a14/readme.html)

Build it exactly as the artifact directs:

```text
cmake -S . -B build -DSEAL_BUILD_TINYLABELS=ON
cmake --build build
```

`-DSEAL_USE_ZLIB=OFF` is an artifact-documented fallback; Intel HEXL is
optional and aimed at server AVX-512 acceleration, not Cortex-M. The build
produces `setup`, `enc1`, `enc2`, `keygen`, `dec`, `gen_samples`, and
`benchmark`. [Artifact README.](https://artifacts.iacr.org/eurocrypt/2025/a14/readme.html)

The first compatibility oracle is semantic, not byte-for-byte:

```text
gen_samples -> setup -> enc1 -> enc2 -> keygen -> dec
diff expected.txt output.txt
```

It operates on exactly `2^21` scalar decimal values in each of `l1.txt`,
`l2.txt`, and `y.txt`, and checks coordinate-wise `(l1 * y + l2) mod p`.
The authors also explicitly support retaining `ct1` and repeating `enc2` to
measure amortization. [Artifact README.](https://artifacts.iacr.org/eurocrypt/2025/a14/readme.html)

The reference's random sampling is not seeded as a published deterministic
test vector, so it cannot yield a byte KAT without a test-only deterministic
RNG patch or captured transcript. The required test ladder is:

1. deterministic toy-ring unit tests for modular arithmetic, NTT/inverse
   NTT, CRT split/join, gadget decomposition/recomposition, packing, and
   bounded-noise rounding;
2. fixed-profile tests pinning the primes, ordering, dimensions, and the
   semantic output of a checked-in small transcript;
3. an opt-in host interoperability test which invokes the pinned C++ artifact
   on deterministic `l1`, `l2`, and `y`, then compares the resulting
   `output.txt` to Rust's decoded selection;
4. only after canonical frames exist, cross-language frame tests at both
   directions and malformed-frame tests.

No test should generate or store the reference's 2.55-GB default `ct1.bin` in
the Rust repository or in embedded CI.

### Source audit finding: do not copy the reference verbatim

Construction 1 requires `LEnc.Enc` to sample every layer's `r_i` from the
Ring-LWE distribution. The pinned C++ `Lenc::enc` allocates `data_r_` and uses
it in the outer products, but does not call `sample_poly_uniform` (or any
other initializer) for it. Compare [the allocation and use at lines
342--373](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/native/tinylabels/batchselect.cpp#L342-L373)
with the paper's explicit sampling step in Construction 1. [Paper,
pp. 21--22.](https://eprint.iacr.org/2024/2048.pdf)

That means the source is not a cryptographic implementation specification:
depending on the allocator, `r` may be zero or retain pool contents. It may
still satisfy the sample program's functional output check, which is not a
security test. A Rust implementation must follow the paper and sample each
`r_i` from a reviewed CSPRNG; it must not reproduce this apparent omission for
byte compatibility. This finding reinforces the artifact's own “no security
guarantees” warning.

## Current Rust implementation and required follow-up

`src/ring_lwe.rs` now keeps the work in this separate experimental crate and
implements the reference profile and a scaled test profile with canonical
two-limb arithmetic, negacyclic NTT/inverse NTT, CRT gadget decomposition,
`LEnc`, `LHE`, and composed typed `setup`, `enc1`, `enc2`, `keygen`, and `dec`
stages. Its deterministic zero-noise tests validate the complete field-element
equation and the reference-degree NTT. It samples every `r_i` through a caller
provided `RandomSource`, fixing the source-audit issue above rather than
copying it.

This is deliberately not a 128-bit-security implementation: it has no
reviewed CSPRNG integration, exact clipped discrete-Gaussian sampler,
constant-time field kernel, canonical wire format, or label encoding. Its
in-memory holders also intentionally show why the reference layout is not a
microcontroller deployment format. The next work is therefore:

1. Define versioned, size-checked canonical frames and bounded streaming
   readers/writers for each stage; use a paired iterator-driven recipient test
   without adopting a garbling-table record format.
2. Add reviewed CSPRNG and exact noise implementations, quantify the error
   budget/failure probability, and audit parameter derivation and secret-side
   operations before any 128-bit-security statement.
3. Introduce a canonical input-label encoding/translation only after its
   ownership and symbolic-hash semantics are fixed. Its tests must reconstruct
   selected `[u8; 16]` labels exactly and preserve the global free-XOR relation
   where the garbling scheme requires it.
4. Measure a scaled profile on Thumb/Cortex-M33 before attempting the paper
   profile. The pinned reference alone exceeds the focused deployment's
   256-KiB RAM and 2-MiB flash limits by orders of magnitude (`pp` is 34 MB
   and `ct1` is 2.55 GB in its raw layout). Its published timing is a
   server-SEAL baseline, not microcontroller evidence. The optional RO
   compression of `ct2` is not in the source, so it must be separately
   designed and validated rather than assumed. [Paper,
   pp. 34--36.](https://eprint.iacr.org/2024/2048.pdf)

The implementation does not substitute a different LWE scheme, change the
current garbling format, or make a deployment-security claim. Those are
separate from reproducing the cited batch-select arithmetic.
