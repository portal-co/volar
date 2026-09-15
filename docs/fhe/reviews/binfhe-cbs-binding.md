# binfhe circuit-bootstrapping paper binding

**Classification target:** Paper-pinned (pending paper-PDF hashing), Very
unstable. This binds the *construction* of
`crates/spec/volar-spec/src/binfhe/circuit_bs.rs` (and the RGSW row
invariant it relies on in `rgsw.rs`). It is not a parameter, noise,
security, interoperability, or deployment claim; those remain gated by the
[plan](../binfhe-v2-implementation-plan.md) §9.

## Sources

| Label | Artifact | Reference | Used scope |
|---|---|---|---|
| CGGI17 | Chillotti, Gama, Georgieva, Izabachène, *Faster Packed Homomorphic Operations and Efficient Circuit Bootstrapping for TFHE*, ASIACRYPT 2017 | ePrint 2017/430, §4 | Circuit bootstrap = per-gadget-level bootstrap producing an LWE of `m * g_j` under the ring key, followed by private key switching to RGSW rows. **PDF hash pending** (ePrint rate-limited at review time); the construction was cross-checked against TFHEpp below. |
| CGGI16 | Chillotti, Gama, Georgieva, Izabachène, *Faster fully homomorphic encryption: Bootstrapping in less than 0.1 seconds*, ASIACRYPT 2016 | ePrint 2016/870 | Private (functional) key switching: per-coefficient gadget decomposition against a key of RLWE-encrypted key-affiliated messages. **PDF hash pending** (same). |
| TFHEpp | `virtualsecureplatform/TFHEpp` @ `3982f4e1ab2620fdc878c5631cd8ca45cb0bd578` (Apache 2.0) | `src/circuitbootstrapping.cpp` SHA-256 `287f28bbce9057544477b144a7480c778fcd445e1727d1e6a8a3094b593164d6`; `include/tfhe/keyswitch.hpp` SHA-256 `eb9b976e0c7ac09b9626023f0fbb537e679d3fdc71431fa1e120da8633df4281` | Executable cross-check of the construction shape: CB test vector packs gadget factors, per-level extraction plus offset, then `PrivKeySwitch` into the two RGSW column families. Read as a convention reference; no code was copied. |

## Code-to-construction map

| `binfhe` item | Bound claim |
|---|---|
| `circuit_bs::level_test_poly` | Level-`j` extraction test polynomial: constant `g_j` on bin 1, zero on bin 0 (the constant-per-bin specialization of the `lut.rs` half-circle selector). Input wires are centered by `Delta/2` before rotation. |
| `circuit_bootstrap` steps | Per level: blind rotate → sample extract (phase `m * g_j` under the ring key) → two private key switches. No LWE key switch or modulus switch inside circuit bootstrap. |
| `PrivateKeySwitchingKey` layout | Per source coefficient `i in [0, N)` and level `l`: RLWE of `f(s'_i) * g_l`; body index holds `f(-1) * g_l`. a-column `f(x) = x * s'(X)`; b-column `f(x) = -x`. |
| `priv_ks` accumulation | `out = sum_{i,l} d_l(a_i) * col[i][l] + sum_l d_l(b) * body[l]`; with the phase convention `b - <a, s'>` this yields phase `phi` (b-column) and `-phi * s'(X)` (a-column). |
| RGSW row invariant | `phase(rlwe0_j) = -m * g_j * s'(X)`, `phase(rlwe1_j) = m * g_j` — the same invariant `rgsw::external_product` assumes of directly encrypted RGSW ciphertexts. |

## Test evidence (crate tests, `toy` exact profile)

- `circuit_bootstrap_rows_satisfy_the_rgsw_invariant`: row phases checked
  coefficient-by-coefficient against an independently written negacyclic
  convolution, for both plaintext bits.
- `circuit_bootstrapped_rgsw_drives_external_products`: CB output in
  `external_product` reproduces `m * phase(content)` on arbitrary content.
- `circuit_bootstrapped_cmux_tree_selects`: two-level oblivious select over
  four RLWE contents matches plaintext selection.
- `keygen_is_deterministic`: seeded regeneration reproduces the key.

## Scope and excluded claims

- The TFHEpp pin is a *convention* cross-check (like the existing `tfhe-go`
  reconnaissance record), not ciphertext interoperability.
- No noise bound is claimed here. CB output noise feeds external products
  multiplicatively (digits × noise); the plan's `BootstrapPlan` failure
  budget and the M8 noise-budget measurements own that analysis.
- The WWL⁺24 ring-only circuit-bootstrap redesign (ePrint 2024/323) and its
  mandatory error-analysis correction (ePrint 2024/1318) are explicitly out
  of scope for this binding; they are a separate, later optimization with
  its own binding.
