// @pinnedness: paper-pinned
// @paper: Micciancio & Polyakov, “Bootstrapping in FHEW-like Cryptosystems”,
//   ePrint 2020/086, pp. 9–15, Figures 1–4 and Table 1
// @paper: Chillotti, Gama, Georgieva & Izabachène, “TFHE: Fast Fully
//   Homomorphic Encryption over the Torus”, ePrint 2018/421, pp. 23, 41–44
// @paper-binding: docs/reviews/tfhe-ginx-oracle-paper-binding.md
// @stability: very-unstable
// @ai: assisted
//! Independent cleartext oracle for the paper-derived GINX/FHEW Boolean-gate
//! certificates documented in `docs/tfhe-ginx-core-spec.md` (§4). The exact
//! source-to-operation bindings, source hashes, and scope are recorded in
//! `docs/reviews/tfhe-ginx-oracle-paper-binding.md`.
//!
//! # Purpose (Phase 1 of `docs/tfhe-pbs-rework-plan.md`)
//!
//! This module contains **no ciphertexts, no secret keys, no RGSW/RLWE
//! arithmetic, and no `SpecRng` dependency**. It models each Boolean gate as
//! plain modular-integer arithmetic over `Z_q` for a generic power-of-two
//! `q`, exactly mirroring the affine preparation and threshold intervals in
//! the cited table. Its only purpose is to serve as an independent,
//! from-the-paper reference that a later, ciphertext-level conformance suite
//! (Phase 2 of the rework plan) can be checked against — it must never be
//! used as a substitute proof of the ciphertext-level implementation's
//! correctness, and it deliberately does not import anything from `tfhe.rs`.
//!
//! # Compilation
//!
//! Test-only (`#[cfg(test)]` at the `mod` declaration in `lib.rs`). This is
//! not a public API and ships no runtime behavior.
//!
//! # Encoding
//!
//! Canonical Boolean encoding per the core spec §1.2: `false = 0`,
//! `true = q/4`, all arithmetic mod `q`. Gate outputs are restored to this
//! same canonical encoding (§4, "Output restoration"), so gate outputs may be
//! fed as inputs to subsequent gates in this oracle, mirroring the
//! composability argument in the core spec §4.3.

#![allow(dead_code)]

/// A gate certificate: an affine preparation (as a function of the input
/// phases) plus the half-open interval `[lo, hi)` (mod `q`) that decides the
/// `+q/8`-vs-`-q/8` signed bootstrap outcome.
///
/// Direct source: [MP20, p.15, Table 1]. The signed output and its canonical
/// restoration are derived immediately above that table; see
/// `evaluate_certificate`.
///
/// `interval_true` is stored as `(lo, hi)` such that `hi` may be `< lo` to
/// represent a wraparound interval mod `q` (matching the paper's use of
/// negative representatives such as `[-q/8, 3q/8)`).
struct GateCertificate {
    name: &'static str,
    /// Arity: number of Boolean ciphertext inputs.
    arity: usize,
    /// Affine combination of the `arity` input phases, returned mod `q`.
    prepare: fn(inputs: &[u64], q: u64) -> u64,
    /// Interval (mod `q`) that maps to the *signed* `+q/8` bootstrap output.
    /// Anything outside this interval maps to `-q/8`.
    interval_true: (u64, u64),
}

/// Returns true if `x mod q` lies in the half-open interval `[lo, hi)` mod
/// `q`, where the interval may wrap around zero (`hi < lo`).
fn in_interval_mod(x: u64, lo: u64, hi: u64, q: u64) -> bool {
    let x = x % q;
    let lo = lo % q;
    let hi = hi % q;
    if lo <= hi {
        x >= lo && x < hi
    } else {
        // Wraps around 0.
        x >= lo || x < hi
    }
}

/// Evaluate a certificate against ciphertext-phase inputs (each in canonical
/// `{0, q/4}` encoding), returning the canonical `{0, q/4}` output.
///
/// This directly implements the signed-output restoration in [MP20, p.15]:
/// the signed bootstrap output in `{-q/8, +q/8}` has `q/8` added to land back
/// in `{0, q/4}`. This is an integer model of the cited arithmetic, not a
/// ciphertext bootstrap or a noise claim.
fn evaluate_certificate(cert: &GateCertificate, inputs: &[u64], q: u64) -> u64 {
    assert_eq!(
        inputs.len(),
        cert.arity,
        "certificate {} arity mismatch",
        cert.name
    );
    let prepared = (cert.prepare)(inputs, q) % q;
    let (lo, hi) = cert.interval_true;
    let signed_eighth: i64 = if in_interval_mod(prepared, lo, hi, q) {
        (q / 8) as i64
    } else {
        -((q / 8) as i64)
    };
    // Restore canonical {0, q/4} encoding: add q/8 to the signed {-q/8,+q/8}.
    let restored = signed_eighth + (q / 8) as i64;
    restored.rem_euclid(q as i64) as u64
}

const FALSE: u64 = 0;
fn true_val(q: u64) -> u64 {
    q / 4
}

// Intervals in the table are naturally expressed in units of q/8. To avoid
// repeating `* (q/8)` at every call site, certificates store interval bounds
// in units of q/8 and `evaluate_certificate` is called through this wrapper
// that rescales them for the chosen q.
fn evaluate_gate(cert_eighths: &GateCertificate, inputs: &[u64], q: u64) -> u64 {
    let scale = q / 8;
    let scaled = GateCertificate {
        name: cert_eighths.name,
        arity: cert_eighths.arity,
        prepare: cert_eighths.prepare,
        interval_true: (
            cert_eighths.interval_true.0.wrapping_mul(scale),
            cert_eighths.interval_true.1.wrapping_mul(scale),
        ),
    };
    evaluate_certificate(&scaled, inputs, q)
}

/// AND: `c1+c2`, true region `[3q/8, 7q/8)` ([MP20, p.15, Table 1]).
fn cert_and() -> GateCertificate {
    GateCertificate {
        name: "AND",
        arity: 2,
        prepare: |c, q| (c[0] + c[1]) % q,
        interval_true: (3, 7),
    }
}

/// NAND: `c1+c2`, true region `[-q/8, 3q/8)` ([MP20, p.15, Table 1]);
/// represented as `(7, 3)` wrapping in eighths.
fn cert_nand() -> GateCertificate {
    GateCertificate {
        name: "NAND",
        arity: 2,
        prepare: |c, q| (c[0] + c[1]) % q,
        interval_true: (7, 3),
    }
}

/// OR: `c1+c2`, true region `[q/8, 5q/8)` ([MP20, p.15, Table 1]).
fn cert_or() -> GateCertificate {
    GateCertificate {
        name: "OR",
        arity: 2,
        prepare: |c, q| (c[0] + c[1]) % q,
        interval_true: (1, 5),
    }
}

/// NOR: `c1+c2`, true region `[-3q/8, q/8)` ([MP20, p.15, Table 1]);
/// represented as `(5, 1)` wrapping in eighths.
fn cert_nor() -> GateCertificate {
    GateCertificate {
        name: "NOR",
        arity: 2,
        prepare: |c, q| (c[0] + c[1]) % q,
        interval_true: (5, 1),
    }
}

/// XOR: `2*(c1-c2)`, true region `[q/8, 5q/8)` ([MP20, p.15, Table 1]).
fn cert_xor() -> GateCertificate {
    GateCertificate {
        name: "XOR",
        arity: 2,
        prepare: |c, q| {
            let diff = (c[0] as i64 - c[1] as i64).rem_euclid(q as i64) as u64;
            (2 * diff) % q
        },
        interval_true: (1, 5),
    }
}

/// XNOR: `2*(c1-c2)`, true region `[-3q/8, q/8)` ([MP20, p.15, Table 1]);
/// represented as `(5, 1)` wrapping in eighths.
fn cert_xnor() -> GateCertificate {
    GateCertificate {
        name: "XNOR",
        arity: 2,
        prepare: |c, q| {
            let diff = (c[0] as i64 - c[1] as i64).rem_euclid(q as i64) as u64;
            (2 * diff) % q
        },
        interval_true: (5, 1),
    }
}

/// Majority: `c1+c2+c3`, true region `[3q/8, 7q/8)` ([MP20, p.15,
/// Table 1]) — the table's one-bootstrap 3-input gate. This does not justify
/// arbitrary 3-input tables or circuit bootstrapping.
fn cert_majority() -> GateCertificate {
    GateCertificate {
        name: "Majority",
        arity: 3,
        prepare: |c, q| (c[0] + c[1] + c[2]) % q,
        interval_true: (3, 7),
    }
}

/// NOT is not bootstrapped: `(-a, -b + q/4)` ([MP20, p.15] and [CGGI18,
/// p.44]). At the phase level (ignoring mask `a`) this is negation plus `q/4`,
/// so it bypasses the interval and signed-output-restoration model entirely.
fn eval_not(c: u64, q: u64) -> u64 {
    let true_v = q / 4;
    (true_v as i64 - c as i64).rem_euclid(q as i64) as u64
}

fn to_bool(phase: u64, q: u64) -> bool {
    phase == q / 4
}

fn from_bool(b: bool, q: u64) -> u64 {
    if b { true_val(q) } else { FALSE }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test moduli: q must be a power of two divisible by 8 so that q/8 is
    /// exact, per the core spec's units-of-q/8 table. We exercise several
    /// values to make sure the certificates are not accidentally tied to one
    /// specific q (which would indicate a hardcoded-constant bug rather than
    /// a genuine affine/interval model).
    const TEST_MODULI: [u64; 3] = [1 << 8, 1 << 16, 1 << 32];

    fn exhaustive_binary(q: u64, cert: &GateCertificate, expected: fn(bool, bool) -> bool) {
        for a in [false, true] {
            for b in [false, true] {
                let inputs = [from_bool(a, q), from_bool(b, q)];
                let got_phase = evaluate_gate(cert, &inputs, q);
                let got = to_bool(got_phase, q);
                assert_eq!(
                    got,
                    expected(a, b),
                    "{} q={} a={} b={}: got {}, want {}",
                    cert.name,
                    q,
                    a,
                    b,
                    got,
                    expected(a, b)
                );
            }
        }
    }

    #[test]
    fn and_matches_truth_table() {
        for &q in &TEST_MODULI {
            exhaustive_binary(q, &cert_and(), |a, b| a && b);
        }
    }

    #[test]
    fn nand_matches_truth_table() {
        for &q in &TEST_MODULI {
            exhaustive_binary(q, &cert_nand(), |a, b| !(a && b));
        }
    }

    #[test]
    fn or_matches_truth_table() {
        for &q in &TEST_MODULI {
            exhaustive_binary(q, &cert_or(), |a, b| a || b);
        }
    }

    #[test]
    fn nor_matches_truth_table() {
        for &q in &TEST_MODULI {
            exhaustive_binary(q, &cert_nor(), |a, b| !(a || b));
        }
    }

    #[test]
    fn xor_matches_truth_table() {
        for &q in &TEST_MODULI {
            exhaustive_binary(q, &cert_xor(), |a, b| a != b);
        }
    }

    #[test]
    fn xnor_matches_truth_table() {
        for &q in &TEST_MODULI {
            exhaustive_binary(q, &cert_xnor(), |a, b| a == b);
        }
    }

    #[test]
    fn not_matches_truth_table() {
        for &q in &TEST_MODULI {
            for a in [false, true] {
                let got = to_bool(eval_not(from_bool(a, q), q), q);
                assert_eq!(got, !a, "NOT q={} a={}: got {}", q, a, got);
            }
        }
    }

    #[test]
    fn majority_matches_truth_table_exhaustive() {
        // The 3-input counterexample to "arity >= 3 needs circuit
        // bootstrapping" (core spec §4.1): exhaustively check all 8
        // input combinations against the plaintext majority function.
        for &q in &TEST_MODULI {
            let cert = cert_majority();
            for a in [false, true] {
                for b in [false, true] {
                    for c in [false, true] {
                        let inputs = [from_bool(a, q), from_bool(b, q), from_bool(c, q)];
                        let got_phase = evaluate_gate(&cert, &inputs, q);
                        let got = to_bool(got_phase, q);
                        let count = [a, b, c].iter().filter(|&&x| x).count();
                        let expected = count >= 2;
                        assert_eq!(
                            got, expected,
                            "Majority q={} a={} b={} c={}: got {}, want {}",
                            q, a, b, c, got, expected
                        );
                    }
                }
            }
        }
    }

    /// Composition: feed a certified XOR output into a certified AND, over
    /// every input assignment, checking against the plaintext oracle. This
    /// exercises the "output restoration keeps composability" claim in core
    /// spec §4.3 in the *cleartext* model — it does not by itself say
    /// anything about the ciphertext-level implementation's composability.
    #[test]
    fn xor_composes_into_and() {
        for &q in &TEST_MODULI {
            let xor = cert_xor();
            let and = cert_and();
            for a in [false, true] {
                for b in [false, true] {
                    for c in [false, true] {
                        let xor_out = evaluate_gate(&xor, &[from_bool(a, q), from_bool(b, q)], q);
                        let and_out = evaluate_gate(&and, &[xor_out, from_bool(c, q)], q);
                        let got = to_bool(and_out, q);
                        let expected = (a != b) && c;
                        assert_eq!(
                            got, expected,
                            "(a xor b) and c: q={} a={} b={} c={}: got {}, want {}",
                            q, a, b, c, got, expected
                        );
                    }
                }
            }
        }
    }

    // ── Mutation tests (Gate A of the rework plan) ──────────────────────
    //
    // Each test intentionally introduces exactly one of the convention
    // mistakes the reverted prototype made, and asserts the resulting
    // certificate *fails* the exhaustive check for at least one input. This
    // demonstrates the oracle actually discriminates a correct certificate
    // from nearby incorrect ones, rather than trivially passing everything.

    #[test]
    fn mutation_and_wrong_sign_offset_fails() {
        // Correct AND true-region is [3q/8, 7q/8). The only achievable
        // affine sums for two canonical Boolean inputs are {0, 2, 2, 4}
        // (in eighths), so a shift must cross one of {2, 4} to actually
        // change a classification. Shifting to [2, 6) crosses the sum=2
        // boundary (an off-by-one-eighth error, as would result from mixing
        // the "folded into pre-bootstrap value" and "folded into output"
        // conventions noted in core spec §3.5) and must break at least one
        // input assignment.
        let q = 1u64 << 16;
        let mutated = GateCertificate {
            name: "AND-mutated-offset",
            arity: 2,
            prepare: |c, q| (c[0] + c[1]) % q,
            interval_true: (2, 6), // shifted by one eighth from the correct (3,7)
        };
        let mut any_mismatch = false;
        for a in [false, true] {
            for b in [false, true] {
                let inputs = [from_bool(a, q), from_bool(b, q)];
                let got = to_bool(evaluate_gate(&mutated, &inputs, q), q);
                if got != (a && b) {
                    any_mismatch = true;
                }
            }
        }
        assert!(
            any_mismatch,
            "mutated AND offset should disagree with truth table on some input"
        );
    }

    #[test]
    fn mutation_xor_missing_doubling_fails() {
        // XOR's affine preparation is `2*(c1-c2)`, not `c1-c2`. Omitting the
        // doubling (an easy transcription error since AND/OR use unscaled
        // sums) must fail the exhaustive check.
        let q = 1u64 << 16;
        let mutated = GateCertificate {
            name: "XOR-no-doubling",
            arity: 2,
            prepare: |c, q| {
                let diff = (c[0] as i64 - c[1] as i64).rem_euclid(q as i64) as u64;
                diff % q // missing the *2
            },
            interval_true: (1, 5),
        };
        let mut any_mismatch = false;
        for a in [false, true] {
            for b in [false, true] {
                let inputs = [from_bool(a, q), from_bool(b, q)];
                let got = to_bool(evaluate_gate(&mutated, &inputs, q), q);
                if got != (a != b) {
                    any_mismatch = true;
                }
            }
        }
        assert!(
            any_mismatch,
            "XOR without doubling should disagree with truth table on some input"
        );
    }

    #[test]
    fn mutation_majority_wrong_threshold_fails() {
        // Using AND's arity but OR's interval for majority (a plausible
        // "reuse the wrong row" copy-paste mistake) must fail.
        let q = 1u64 << 16;
        let mutated = GateCertificate {
            name: "Majority-wrong-interval",
            arity: 3,
            prepare: |c, q| (c[0] + c[1] + c[2]) % q,
            interval_true: (1, 5), // OR's interval, not Majority's (3,7)
        };
        let mut any_mismatch = false;
        for a in [false, true] {
            for b in [false, true] {
                for c in [false, true] {
                    let inputs = [from_bool(a, q), from_bool(b, q), from_bool(c, q)];
                    let got = to_bool(evaluate_gate(&mutated, &inputs, q), q);
                    let count = [a, b, c].iter().filter(|&&x| x).count();
                    if got != (count >= 2) {
                        any_mismatch = true;
                    }
                }
            }
        }
        assert!(
            any_mismatch,
            "majority with OR's interval should disagree with truth table on some input"
        );
    }

    #[test]
    fn mutation_missing_output_restoration_fails() {
        // If the +q/8 output restoration (core spec §4, "Output
        // restoration") is skipped, the result stays in signed {-q/8,+q/8}
        // rather than canonical {0, q/4} and will not compare equal to the
        // canonical `true_val`/`FALSE` constants for at least one input.
        let q = 1u64 << 16;
        let cert = cert_and();
        let scale = q / 8;
        for a in [false, true] {
            for b in [false, true] {
                let inputs = [from_bool(a, q), from_bool(b, q)];
                let prepared = (cert.prepare)(&inputs, q) % q;
                let (lo, hi) = (cert.interval_true.0 * scale, cert.interval_true.1 * scale);
                let signed_no_restore: i64 = if in_interval_mod(prepared, lo, hi, q) {
                    (q / 8) as i64
                } else {
                    -((q / 8) as i64)
                };
                let unrestored = signed_no_restore.rem_euclid(q as i64) as u64;
                // Canonical false=0, true=q/4; q/8 and -q/8 (mod q) are
                // neither, so equality with the canonical encoding must fail
                // for both branches once restoration is skipped.
                assert_ne!(
                    unrestored,
                    from_bool(a && b, q),
                    "unrestored output should not equal canonical encoding by coincidence"
                );
            }
        }
    }
}
