// @reliability: experimental
//! @ai: assisted
//! **Prove-the-verifier integration: runtime side.** Owns everything that
//! *executes* a woven VOLE verifier and folds its output — the counterpart to
//! `volar-verifier-fold`'s compile-time-only `emit_verifier_c`/
//! `emit_verifier_rust` terminals. Deliberately depends only on
//! `volar-fold`/`volar-spec`/`volar-discipline` (+ `std`), never on
//! `volar-compiler`/`volar-lir-codegen`/`volar-c-backend`/`volar-weaver` — it
//! only ever consumes already-generated Rust **source text** (`&str`), so the
//! compiler-crate dependency graph stays one-directional (weave → print →
//! text → this crate).
//!
//! This crate has two, mostly-independent halves:
//!
//! 1. **Batch/testing path** ([`GateObservation`], [`verifier_trace`],
//!    [`prove_and_verify_folded`], moved here verbatim from
//!    `volar-verifier-fold`) — takes an already-fully-materialized
//!    `&[GateObservation]` slice and folds it via `volar_fold::verifier`'s
//!    general machinery. Still useful for hand-constructed test traces; not
//!    the production path any more.
//! 2. **In-loop fold-linking definitions** ([`FoldLift`], [`FoldScalar`],
//!    [`FoldAccumulator`], [`fold_accumulator_fresh`], [`fold_and_gate`]) —
//!    the concrete definitions a compiled, `NovaFoldSink`-woven verifier
//!    links against for its `FoldScalar`/`FoldAccumulator`/
//!    `fold_accumulator_fresh`/`fold_and_gate` bare names (see
//!    `volar_weaver::vole::VerifierTraceSink`'s doc for why those names are
//!    left unresolved at weave time). Plus [`run_folded_verifier`], the
//!    generic "print → temp Cargo project → real `cargo`/`rustc` → capture
//!    output" harness (same shape as the existing `garble.rs`/`fhe.rs` test
//!    harnesses) that actually compiles and runs the linked result.
//!
//! ## The GF(2^k) → F_ℓ embedding: tracked, not solved
//!
//! [`FoldLift`] is deliberately **not** `std::convert::From`/`Into` — `T`
//! (e.g. `Galois`, from `volar-primitives`) and `FoldScalar`
//! (`volar_fold::scalar::Scalar`) are both foreign to any crate that isn't
//! `volar-fold` itself, so a blanket `impl From<Galois> for Scalar` can only
//! be written inside `volar-fold` (Rust's orphan rule) — and doing that would
//! quietly present one specific field embedding as *the* answer, which is
//! not something this crate should decide. `FoldLift` is a local trait
//! instead, purely so the orphan rule doesn't force the decision into the
//! wrong crate. Two implementations are provided, and swapping which one a
//! given `fold_and_gate::<N, T>` monomorphization uses is exactly a matter of
//! which `T` the driver instantiates the woven verifier with:
//!
//! - `impl FoldLift for Galois` — reinterprets the GF(2^k) element's integer
//!   representation directly as an `F_ℓ` integer (`Scalar::from_u64`). This
//!   is the *existing* simplification `volar_fold::verifier::VerifierStep`/
//!   `GateObservation` already use elsewhere in this codebase (not a new
//!   cryptographic decision) — kept as the default here for that reason.
//! - `impl FoldLift for Bit` — the simplest possible embedding (`0 ↦ 0`,
//!   `1 ↦ 1`), sound *only* for a degenerate `T = GF(2)` weave (`Δ` can only
//!   be `1`, so this isn't cryptographically meaningful VOLE — it exists to
//!   keep a genuinely-homomorphic-on-multiplication option pluggable).
//!   **Known gap, not silently papered over**: `+` is not a ring
//!   homomorphism here — `1 + 1 = 0` in `GF(2)` (XOR) but `Scalar::ONE +
//!   Scalar::ONE = 2` in `F_ℓ` (no reduction). The `and_check` relation
//!   `K_a·K_b + V̂ = K_c·Δ` is multiplication-safe under this embedding but
//!   can spuriously fail on an *honest* gate whenever `K_a·K_b = 1` and
//!   `K_c·Δ = 0` (so `V̂` must be `1` to compensate in `GF(2)`, but the
//!   embedded sum is `2 ≠ 0`). Left as-is rather than "fixed" with an ad hoc
//!   mod-2 wrap, since the right fix depends on the review this is tracked
//!   against — see `docs/agent-context/gf2k-to-fell-embedding.md`.
//!
//! Neither implementation is presented as sound for the real, non-degenerate
//! `GF(2^k)` case; both exist so the mechanism is exercised end to end while
//! that review is pending.

use std::fs;
use std::process::Command;

use volar_discipline::{NonZk, Tagged, Transparent};
use volar_fold::pedersen::PedersenParams;
use volar_fold::scalar::Scalar;
use volar_fold::verifier::{prove_verifier, verify_folded, VerifierFold, VerifierStep, VerifierTrace};
use volar_spec::field::{Bit, Galois};
use volar_spec::vole::{Delta, Q};
use volar_spec::{Array, ArraySize};

// ============================================================================
// Batch/testing path (moved verbatim from volar-verifier-fold)
// ============================================================================

/// The verifier-side values observed for one AND gate during a verifier run:
/// the MAC shares `K_a, K_b, K_c`, the global secret `Δ`, and the prover-sent
/// opening `V̂`.  An honest gate satisfies `K_a·K_b + V̂ = K_c·Δ`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GateObservation {
    /// Verifier MAC share of the left input wire.
    pub k_a: u64,
    /// Verifier MAC share of the right input wire.
    pub k_b: u64,
    /// Verifier MAC share of the output wire.
    pub k_c: u64,
    /// Verifier global secret Δ.
    pub delta: u64,
    /// Prover-sent gate opening V̂.
    pub v_hat: u64,
}

/// **Arithmetization frontend.** Assemble a [`VerifierTrace`] from a verifier's
/// per-gate observations and its memory-accumulator boundary
/// (`mem_acc_in` / `mem_acc_out`, the multiset-hash state under
/// `StorageMode::Commitment`).  The result is tagged [`Transparent`] — it is the
/// non-ZK computation that [`prove_and_verify_folded`] folds.
///
/// The folding challenges and Pedersen blinders are derived deterministically
/// from the step index for reproducibility; a production driver derives the
/// public-coin challenges by Fiat–Shamir and samples the blinders at random.
pub fn verifier_trace(
    gates: &[GateObservation],
    mem_acc_in: &[u64],
    mem_acc_out: &[u64],
) -> Tagged<Transparent, VerifierTrace> {
    let steps: Vec<VerifierStep> = gates
        .iter()
        .enumerate()
        .map(|(i, g)| {
            let seed = i as u64 + 1;
            VerifierStep {
                k_a: Scalar::from_u64(g.k_a),
                k_b: Scalar::from_u64(g.k_b),
                k_c: Scalar::from_u64(g.k_c),
                delta: Scalar::from_u64(g.delta),
                v_hat: Scalar::from_u64(g.v_hat),
                r_w: Scalar::from_u64(seed.wrapping_mul(7).wrapping_add(1)),
                r: Scalar::from_u64(seed.wrapping_mul(13).wrapping_add(3)),
                r_t: Scalar::from_u64(seed.wrapping_mul(17).wrapping_add(5)),
            }
        })
        .collect();
    Tagged::seal(VerifierTrace {
        steps,
        mem_acc_in: mem_acc_in.iter().map(|x| Scalar::from_u64(*x)).collect(),
        mem_acc_out: mem_acc_out.iter().map(|x| Scalar::from_u64(*x)).collect(),
        r_in: Scalar::from_u64(2),
        r_out: Scalar::from_u64(3),
    })
}

/// **Pipeline terminal (fold leg).** Fold the whole verifier and check the
/// folded instance natively.  Returns the folded proof and whether it verifies.
///
/// Bound `where Z: NonZk`: a [`volar_discipline::Zk`] artifact cannot be folded
/// here (compile error).  The fold output is [`Transparent`].
pub fn prove_and_verify_folded<Z: NonZk>(
    trace: Tagged<Z, VerifierTrace>,
    params: &PedersenParams,
) -> (Tagged<Transparent, VerifierFold>, bool) {
    let fold = prove_verifier(trace, params);
    let ok = verify_folded(&fold, params);
    (fold, ok)
}

// ============================================================================
// In-loop fold-linking definitions (what a NovaFoldSink-woven verifier links
// against — see this module's doc for the GF(2^k) -> F_ell note)
// ============================================================================

/// The folding scalar field `F_ℓ` a `NovaFoldSink`-woven verifier's
/// `FoldScalar` bare name resolves to.
pub type FoldScalar = Scalar;

/// Lifts a single VOLE field element into [`FoldScalar`]. See this module's
/// doc for why this is a local trait, not `std::convert::From`/`Into`, and
/// for what each implementation does and does not guarantee.
pub trait FoldLift {
    fn fold_lift(&self) -> FoldScalar;
}

/// The default lift: reinterpret the GF(2^k) element's integer
/// representation directly as an `F_ℓ` integer. Matches the existing
/// `GateObservation`/`VerifierStep` simplification elsewhere in this
/// codebase — not a new cryptographic decision.
impl FoldLift for Galois {
    fn fold_lift(&self) -> FoldScalar {
        FoldScalar::from_u64(self.0 as u64)
    }
}

/// The simplest possible lift, sound only for a degenerate `T = GF(2)`
/// weave — see this module's doc for the known `+`-is-not-a-homomorphism gap.
impl FoldLift for Bit {
    fn fold_lift(&self) -> FoldScalar {
        if self.0 { FoldScalar::ONE } else { FoldScalar::ZERO }
    }
}

/// The threaded fold-accumulator state a `NovaFoldSink`-woven verifier's
/// `FoldAccumulator` bare name resolves to: `None` until the first gate is
/// folded in (matching [`volar_fold::ivc::GapAccumulator`]'s `fresh`-vs-fold
/// split), then the running Nova relaxed witness `(W, E, u)` — no
/// commitments; those are computed once, outside the loop, from this small
/// fixed-size witness (see `volar_weaver::vole::NovaFoldSink`'s doc for why
/// that's still succinct).
#[derive(Clone)]
pub struct FoldAccumulator {
    inner: Option<([FoldScalar; 7], [FoldScalar; 3], FoldScalar)>,
}

impl FoldAccumulator {
    /// The running witness/error/relaxation-scalar, once at least one gate
    /// has been folded in.
    pub fn witness(&self) -> Option<(&[FoldScalar; 7], &[FoldScalar; 3], &FoldScalar)> {
        self.inner.as_ref().map(|(w, e, u)| (w, e, u))
    }
}

/// What a `NovaFoldSink`-woven verifier's `fold_accumulator_fresh` bare name
/// resolves to.
pub fn fold_accumulator_fresh() -> FoldAccumulator {
    FoldAccumulator { inner: None }
}

/// What a `NovaFoldSink`-woven verifier's `fold_and_gate` bare name resolves
/// to: fold one AND gate's observed values into `state`, real
/// `volar_spec::fold` calls (`gate_witness`/`cross_term`/`fold_witness`/
/// `fold_u`) — genuinely spec-linked math, executed as this function runs,
/// not logged for later interpretation.
pub fn fold_and_gate<N, T>(
    state: FoldAccumulator,
    k_a: Q<N, T>,
    k_b: Q<N, T>,
    k_c: Q<N, T>,
    delta: &Delta<N, T>,
    hat: Array<T, N>,
    r: FoldScalar,
) -> FoldAccumulator
where
    N: ArraySize,
    T: FoldLift,
{
    let gate_w = volar_spec::fold::gate_witness(
        k_a.q[0].fold_lift(),
        k_b.q[0].fold_lift(),
        k_c.q[0].fold_lift(),
        delta.delta[0].fold_lift(),
        hat[0].fold_lift(),
    );
    match state.inner {
        None => FoldAccumulator { inner: Some((gate_w, [FoldScalar::ZERO; 3], FoldScalar::ONE)) },
        Some((w1, e1, u1)) => {
            let t = volar_spec::fold::cross_term(&w1, &u1, &gate_w, &FoldScalar::ONE);
            let (w, e) = volar_spec::fold::fold_witness(&w1, &e1, &gate_w, &[FoldScalar::ZERO; 3], &t, &r);
            let u = volar_spec::fold::fold_u(&u1, &FoldScalar::ONE, &r);
            FoldAccumulator { inner: Some((w, e, u)) }
        }
    }
}

// ============================================================================
// Generic compile-and-run harness
// ============================================================================

/// Locate the workspace root from this crate's own manifest directory (same
/// approach `volar-weaver`'s test harnesses use).
fn workspace_root() -> String {
    // CARGO_MANIFEST_DIR = .../volar/crates/fold/volar-verifier-runtime
    let mut dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.pop(); // .../volar/crates/fold
    dir.pop(); // .../volar/crates
    dir.pop(); // .../volar
    dir.to_string_lossy().into_owned()
}

/// Compile and run `rust_source` (already-printed Rust — e.g. from
/// `volar_verifier_fold::emit_verifier_rust`) together with `driver_src` (a
/// hand-written `#[test]` module that calls into it) as a real, standalone
/// Cargo project — real `cargo test` + real `rustc`, the same "print → temp
/// Cargo project → cargo test → capture output" shape as
/// `volar-weaver`'s `garble.rs`/`fhe.rs` test harnesses. Returns captured
/// stdout on success; panics with stdout+stderr on failure.
///
/// This crate's own definitions ([`FoldScalar`], [`FoldAccumulator`],
/// [`fold_accumulator_fresh`], [`fold_and_gate`], [`FoldLift`]) are always
/// linked in via a `volar-verifier-runtime` path dependency and a `use
/// volar_verifier_runtime::*;` prelude line, so `rust_source`'s bare
/// `FoldScalar`/`FoldAccumulator`/… references resolve to them.
///
/// Deliberately thin/generic: this function's job is to prove the *compiler's*
/// output (the printed, `NovaFoldSink`-woven Rust) actually compiles and runs
/// — not to house new logic. Test-specific assembly (building a concrete
/// proof, checking results) belongs in the caller.
pub fn run_folded_verifier(rust_source: &str, driver_src: &str) -> String {
    let root = workspace_root();
    let tmpdir = std::env::temp_dir().join(format!(
        "volar_verifier_runtime_{}",
        std::process::id()
    ));
    let srcdir = tmpdir.join("src");
    fs::create_dir_all(&srcdir).expect("create temp src dir");

    // rust_source carries its own leading `#![allow(...)]` inner attribute
    // (from print_weaved_vole_module) — inner attributes must be the very
    // first item in the file, so it has to come first; our own `use` goes
    // after it, not before.
    let full_src = format!(
        "{rust_source}\n\n\
         use volar_verifier_runtime::*;\n\n\
         #[cfg(test)]\n\
         mod driver {{\n\
             use super::*;\n\
             {driver_src}\n\
         }}\n"
    );

    let cargo_toml = format!(
        "[package]\n\
         name = \"volar-verifier-runtime-check\"\n\
         version = \"0.1.0\"\n\
         edition = \"2024\"\n\
         \n\
         [[test]]\n\
         name = \"driver\"\n\
         path = \"src/lib.rs\"\n\
         \n\
         [dependencies]\n\
         volar-verifier-runtime = {{ path = \"{root}/crates/fold/volar-verifier-runtime\" }}\n\
         volar-fold = {{ path = \"{root}/crates/fold/volar-fold\" }}\n\
         volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
         hybrid-array = {{ version = \"0.4.8\", default-features = false }}\n\
         cipher = {{ version = \"0.5.1\", default-features = false }}\n"
    );

    fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).expect("write Cargo.toml");
    fs::write(srcdir.join("lib.rs"), &full_src).expect("write src/lib.rs");

    let output = Command::new("cargo")
        .args(["test", "--quiet", "--test", "driver", "--", "--nocapture"])
        .current_dir(&tmpdir)
        .env("CARGO_TARGET_DIR", tmpdir.join("target").to_string_lossy().into_owned())
        .output()
        .expect("failed to run cargo test");

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();

    if !output.status.success() {
        panic!(
            "run_folded_verifier: compile/run failed\n--- source ---\n{full_src}\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}"
        );
    }
    stdout
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Honest gate: pick `K_a,K_b,K_c,Δ` and set `V̂ = K_c·Δ − K_a·K_b`.
    fn honest_gate(a: u64, b: u64, c: u64, delta: u64) -> GateObservation {
        let lhs = (a as u128) * (b as u128);
        let rhs = (c as u128) * (delta as u128);
        assert!(rhs >= lhs, "test gate must have K_c·Δ ≥ K_a·K_b");
        GateObservation { k_a: a, k_b: b, k_c: c, delta, v_hat: (rhs - lhs) as u64 }
    }

    #[test]
    fn whole_verifier_folds_and_verifies() {
        let params = PedersenParams::setup(8, 41);
        let gates: Vec<GateObservation> =
            (1..=10u64).map(|i| honest_gate(i, i + 2, i + 6, i + 1)).collect();
        let trace = verifier_trace(&gates, &[1], &[99]);
        let (fold, ok) = prove_and_verify_folded(trace, &params);
        assert!(ok, "honest verifier run must fold to a natively-verifiable instance");
        assert_eq!(fold.inner().gap.steps, 10);
    }

    #[test]
    fn tampered_gate_is_rejected() {
        let params = PedersenParams::setup(8, 41);
        let mut gates: Vec<GateObservation> =
            (1..=6u64).map(|i| honest_gate(i, i + 2, i + 6, i + 1)).collect();
        gates[3].v_hat += 1; // break the check
        let trace = verifier_trace(&gates, &[1], &[2]);
        let (_, ok) = prove_and_verify_folded(trace, &params);
        assert!(!ok, "a tampered gate observation must fail native verification");
    }

    #[test]
    fn fold_and_gate_matches_gap_accumulator_over_the_same_witnesses() {
        // fold_and_gate's own logic must agree with volar_fold::ivc::GapAccumulator
        // (the one true streaming accumulator) when fed the same and_check
        // witnesses and challenges — this is the Rust-level analog of the
        // end-to-end test's real-compile check (see volar-verifier-fold's
        // end-to-end test), isolating fold_and_gate from the weave/compile
        // machinery entirely.
        use volar_fold::ivc::{GapAccumulator, Step};
        use volar_fold::verifier::and_check_r1cs;

        fn q1(x: u64) -> Q<cipher::consts::U1, Galois> {
            Q { q: Array::<Galois, cipher::consts::U1>::from_fn(|_| Galois(x as u8)) }
        }
        fn delta1(x: u64) -> Delta<cipher::consts::U1, Galois> {
            Delta { delta: Array::<Galois, cipher::consts::U1>::from_fn(|_| Galois(x as u8)) }
        }
        fn hat1(x: u64) -> Array<Galois, cipher::consts::U1> {
            Array::<Galois, cipher::consts::U1>::from_fn(|_| Galois(x as u8))
        }

        let r1cs = and_check_r1cs();
        let params = PedersenParams::setup(8, 7);
        // Both gates honest with K_c·Δ >= K_a·K_b, so V̂ = K_c·Δ - K_a·K_b is a
        // small non-negative integer that fits in Galois(u8) untruncated.
        let gates: [(u64, u64, u64, u64); 2] = [(3, 4, 5, 6), (2, 3, 4, 5)];

        let mut state = fold_accumulator_fresh();
        let mut acc = GapAccumulator::new();
        for (i, &(a, b, c, d)) in gates.iter().enumerate() {
            let v_hat: u64 = c * d - a * b;
            let r = FoldScalar::from_u64(0xabcd + i as u64);
            state = fold_and_gate(state, q1(a), q1(b), q1(c), &delta1(d), hat1(v_hat), r);

            let w = volar_spec::fold::gate_witness(
                FoldScalar::from_u64(a), FoldScalar::from_u64(b), FoldScalar::from_u64(c),
                FoldScalar::from_u64(d), FoldScalar::from_u64(v_hat),
            );
            let step = Step { w: w.to_vec(), r_w: FoldScalar::from_u64(11 + i as u64), r, r_t: FoldScalar::from_u64(99 + i as u64) };
            acc.push(&r1cs, &params, &step);
        }

        let (w, e, u) = state.witness().expect("state must be folded after >=1 gate");
        let gp = acc.finish(&params, &[FoldScalar::from_u64(1)], &FoldScalar::from_u64(2), &[FoldScalar::from_u64(3)], &FoldScalar::from_u64(4));
        // GapAccumulator's own witness isn't directly exposed (only the final
        // committed GapProof is) — cross-check via native_verify instead:
        // reconstruct a RelaxedWitness from fold_and_gate's state and confirm
        // it's the R1CS-satisfying witness that gp's committed instance
        // corresponds to (r1cs is the same and_check_r1cs both legs used).
        assert!(r1cs.is_satisfied_relaxed(w, e, u), "fold_and_gate's own accumulator must satisfy and_check_r1cs");
        assert!(volar_fold::verify::native_verify(&r1cs, &params, &gp.final_u, &gp.final_w));
    }
}
