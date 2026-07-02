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
//! ## The GF(2^k) → F_ℓ embedding: constraint expansion
//!
//! Per-gate folding no longer lifts each GF(2^k) element to a *single*
//! `F_ℓ` scalar (the old one-scalar `FoldLift`, whose unsoundness
//! `docs/agent-context/gf2k-to-fell-embedding.md` documents). Instead
//! [`fold_and_gate`] expands each gate into the **spaced-packing bit
//! relation** of `docs/fold-lift-expansion.md`: every GF(2^k) value enters
//! the R1CS as its GF(2) coefficient bits ([`FoldLift::lift_bits`]), and
//! `volar_fold::gf2k::and_check_gf2k` emits constraints that hold **iff**
//! the Quicksilver check `K_a·K_b + V̂ + K_c·Δ = 0` holds *in `GF(2^k)`* —
//! sound by construction per that doc's §5 argument (booleanity + spaced
//! packing + unique binary decomposition + evenness of the reduced columns).
//! Deterministic soundness tests live in `volar_fold::gf2k`; the seam is
//! still **flagged for cryptographic review** before being called closed
//! (see the spec's §7 for exactly what is and is not claimed — e.g. lane-0
//! projection and per-gate independent Δ bits remain documented
//! simplifications).
//!
//! [`FoldLift`] is deliberately **not** `std::convert::From`/`Into` — `T`
//! (e.g. `Galois`, from `volar-primitives`) and `FoldScalar`
//! (`volar_fold::scalar::Scalar`) are both foreign to any crate that isn't
//! `volar-fold` itself, so a blanket impl could only be written inside
//! `volar-fold` (Rust's orphan rule) — and doing that would hard-wire one
//! specific field's expansion parameters into the folding crate. `FoldLift`
//! is a local trait instead, purely so the orphan rule doesn't force the
//! decision into the wrong crate: each `T` carries its own degree
//! ([`FoldLift::BITS`]) and irreducible polynomial ([`FoldLift::POLY`]),
//! and swapping fields is exactly a matter of which `T` the driver
//! instantiates the woven verifier with.

use std::fs;
use std::process::Command;

use volar_discipline::{NonZk, Tagged, Transparent};
use volar_fold::gf2k::{and_check_gf2k, Gf2kParams};
use volar_fold::nifs::cross_term_z;
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

/// Expands a single VOLE field element into its LSB-first GF(2) coefficient
/// bits, plus the field's compile-time expansion parameters — the inputs
/// [`fold_and_gate`] feeds to `volar_fold::gf2k::and_check_gf2k`. See this
/// module's doc for why this is a local trait, not
/// `std::convert::From`/`Into`.
pub trait FoldLift {
    /// GF(2)-degree k of T.
    const BITS: usize;
    /// Irreducible polynomial low bits (x^BITS implicit), LSB = x^0.
    const POLY: u128;
    /// LSB-first GF(2) coefficients of self; len == BITS.
    fn lift_bits(&self) -> Vec<bool>;
}

/// `GF(2^8)` with the AES polynomial `x^8 + x^4 + x^3 + x + 1`
/// (`volar_primitives::GF8_POLY`). Sound by construction: the bits feed the
/// spec §3 constraint expansion, which encodes genuine `GF(2^8)` arithmetic
/// — not the old integer-reinterpretation lift.
impl FoldLift for Galois {
    const BITS: usize = 8;
    const POLY: u128 = 0x1b;
    fn lift_bits(&self) -> Vec<bool> {
        (0..8).map(|i| (self.0 >> i) & 1 == 1).collect()
    }
}

/// `GF(2)` (`P(x) = x`, no reduction terms). Sound by construction: the
/// expanded relation keeps addition inside `{0,1}` (the old
/// `+`-is-not-a-homomorphism gap of the one-scalar lift does not arise —
/// see `docs/fold-lift-expansion.md` §4's "kept contained" note).
impl FoldLift for Bit {
    const BITS: usize = 1;
    const POLY: u128 = 0;
    fn lift_bits(&self) -> Vec<bool> {
        vec![self.0]
    }
}

/// The threaded fold-accumulator state a `NovaFoldSink`-woven verifier's
/// `FoldAccumulator` bare name resolves to: `None` until the first gate is
/// folded in (matching [`volar_fold::ivc::GapAccumulator`]'s `fresh`-vs-fold
/// split), then the running Nova relaxed witness `(W, E, u)` over the
/// expanded gf2k relation — larger than the old 7-slot witness (165
/// variables / 174 error slots for `GF(2^8)`) but still **constant-size per
/// accumulator**, independent of the gate count, so the
/// commitments-once-outside-the-loop succinctness story is unchanged (see
/// `volar_weaver::vole::NovaFoldSink`'s doc).
#[derive(Clone)]
pub struct FoldAccumulator {
    inner: Option<(Vec<FoldScalar>, Vec<FoldScalar>, FoldScalar)>,
}

impl FoldAccumulator {
    /// The running witness/error/relaxation-scalar, once at least one gate
    /// has been folded in.
    pub fn witness(&self) -> Option<(&[FoldScalar], &[FoldScalar], &FoldScalar)> {
        self.inner.as_ref().map(|(w, e, u)| (w.as_slice(), e.as_slice(), u))
    }
}

/// What a `NovaFoldSink`-woven verifier's `fold_accumulator_fresh` bare name
/// resolves to.
pub fn fold_accumulator_fresh() -> FoldAccumulator {
    FoldAccumulator { inner: None }
}

/// What a `NovaFoldSink`-woven verifier's `fold_and_gate` bare name resolves
/// to: fold one AND gate's observed values into `state` — the gate expands
/// into the spaced-packing gf2k relation of `docs/fold-lift-expansion.md` §3
/// (`volar_fold::gf2k::and_check_gf2k`, R1CS + honest witness built
/// together), then Nova-folds witness-only via
/// [`volar_fold::nifs::cross_term_z`]: `W' = W₁ + r·W₂`, `E' = E₁ + r·T`
/// (the incoming gate is fresh, so its `E`/`r²` term vanishes),
/// `u' = u₁ + r`. Genuinely executed math, not logged for later
/// interpretation. The legacy 7-slot batch path (`volar_spec::fold`,
/// `and_check_r1cs`, [`GateObservation`]/`VerifierStep`) is untouched and
/// remains available; this in-loop path no longer uses it.
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
    // Lane-0 projection of the N VOLE lanes — unchanged, still a documented
    // separate simplification (spec §7).
    let a_bits = k_a.q[0].lift_bits();
    let b_bits = k_b.q[0].lift_bits();
    let c_bits = k_c.q[0].lift_bits();
    let d_bits = delta.delta[0].lift_bits();
    let v_bits = hat[0].lift_bits();

    let params = Gf2kParams::new(T::BITS, T::POLY);
    let (r1cs, gate_w) = and_check_gf2k(&params, &a_bits, &b_bits, &c_bits, &d_bits, &v_bits);

    match state.inner {
        None => FoldAccumulator {
            inner: Some((gate_w, vec![FoldScalar::ZERO; r1cs.num_cons], FoldScalar::ONE)),
        },
        Some((w1, e1, u1)) => {
            let t = cross_term_z(&r1cs, &w1, &u1, &gate_w, &FoldScalar::ONE);
            let w: Vec<FoldScalar> =
                w1.iter().zip(gate_w.iter()).map(|(x, y)| x.add(&r.mul(y))).collect();
            let e: Vec<FoldScalar> =
                e1.iter().zip(t.iter()).map(|(x, ti)| x.add(&r.mul(ti))).collect();
            let u = u1.add(&r);
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

    // ── fold_and_gate over the expanded gf2k relation ────────────────────────

    /// `GF(2^8)` multiply with the AES polynomial (test-local mirror of
    /// `volar_primitives::gf_mul_u8`, kept here to avoid a new dependency).
    fn gf_mul(a: u8, b: u8) -> u8 {
        let mut p = 0u8;
        let (mut a, mut b) = (a, b);
        for _ in 0..8 {
            if b & 1 != 0 {
                p ^= a;
            }
            let hi = a & 0x80;
            a <<= 1;
            if hi != 0 {
                a ^= 0x1b;
            }
            b >>= 1;
        }
        p
    }

    /// Fermat inverse `a^(2^8 − 2)` in `GF(2^8)`.
    fn gf_invert(a: u8) -> u8 {
        let mut result = 1u8;
        let mut base = a;
        let mut e = 254u8;
        while e > 0 {
            if e & 1 == 1 {
                result = gf_mul(result, base);
            }
            base = gf_mul(base, base);
            e >>= 1;
        }
        result
    }

    fn q1(x: u8) -> Q<cipher::consts::U1, Galois> {
        Q { q: Array::<Galois, cipher::consts::U1>::from_fn(|_| Galois(x)) }
    }
    fn delta1(x: u8) -> Delta<cipher::consts::U1, Galois> {
        Delta { delta: Array::<Galois, cipher::consts::U1>::from_fn(|_| Galois(x)) }
    }
    fn hat1(x: u8) -> Array<Galois, cipher::consts::U1> {
        Array::<Galois, cipher::consts::U1>::from_fn(|_| Galois(x))
    }

    /// The gf2k R1CS for `GF(2^8)` — shape depends only on the params, so any
    /// input bits produce the same constraint system.
    fn gf2k_r1cs_k8() -> volar_fold::r1cs::R1CS {
        let params = Gf2kParams::new(8, 0x1b);
        let zeros = [false; 8];
        let (r1cs, _) = and_check_gf2k(&params, &zeros, &zeros, &zeros, &zeros, &zeros);
        r1cs
    }

    #[test]
    fn galois_lift_bits_is_lsb_first() {
        // 0x53 = 0b0101_0011 → LSB-first.
        assert_eq!(
            Galois(0x53).lift_bits(),
            vec![true, true, false, false, true, false, true, false]
        );
        assert_eq!(Bit(true).lift_bits(), vec![true]);
        assert_eq!(Bit(false).lift_bits(), vec![false]);
    }

    #[test]
    fn fresh_single_gate_accumulator_is_plain_satisfied() {
        // One honest GF(2^8) gate: K_c = (K_a·K_b + V̂)·Δ⁻¹.
        let (ka, kb, vv, delta) = (0x37u8, 0x82u8, 0x5au8, 0xc3u8);
        let kc = gf_mul(gf_mul(ka, kb) ^ vv, gf_invert(delta));

        let state = fold_and_gate(
            fold_accumulator_fresh(),
            q1(ka),
            q1(kb),
            q1(kc),
            &delta1(delta),
            hat1(vv),
            FoldScalar::from_u64(0xabcd),
        );
        let (w, e, u) = state.witness().expect("folded after one gate");
        assert_eq!(*u, FoldScalar::ONE, "fresh instance has u = 1");
        assert_eq!(w.len(), 165, "expanded GF(2^8) witness size");
        assert_eq!(e.len(), 174, "E has one slot per constraint");
        assert!(e.iter().all(|x| *x == FoldScalar::ZERO), "fresh instance has E = 0");
        assert!(gf2k_r1cs_k8().is_satisfied_relaxed(w, e, u));
    }

    #[test]
    fn two_gate_fold_satisfies_relaxed_gf2k_relation() {
        // Two honest GF(2^8) tuples folded through fold_and_gate must leave a
        // relaxed-satisfying accumulator against and_check_gf2k's R1CS.
        let tuples: [(u8, u8, u8, u8); 2] = [(0x37, 0x82, 0x5a, 0xc3), (0x01, 0xff, 0x00, 0x1d)];

        let mut state = fold_accumulator_fresh();
        for (i, &(ka, kb, vv, delta)) in tuples.iter().enumerate() {
            let kc = gf_mul(gf_mul(ka, kb) ^ vv, gf_invert(delta));
            let r = FoldScalar::from_u64(0xabcd + i as u64);
            state = fold_and_gate(state, q1(ka), q1(kb), q1(kc), &delta1(delta), hat1(vv), r);
        }

        let (w, e, u) = state.witness().expect("folded after two gates");
        assert!(
            gf2k_r1cs_k8().is_satisfied_relaxed(w, e, u),
            "two-gate fold must satisfy the relaxed gf2k relation"
        );
        // u = 1 + r₂ after the second fold, no longer 1.
        assert_eq!(*u, FoldScalar::ONE.add(&FoldScalar::from_u64(0xabce)));
    }

    #[test]
    fn fold_and_gate_matches_gap_accumulator_over_gf2k() {
        // fold_and_gate's own witness-only fold must agree, elementwise, with
        // volar_fold::ivc::GapAccumulator (the one true streaming accumulator)
        // when fed the same and_check_gf2k gate witnesses and the same r
        // challenges — the gf2k-relation revival of the parity test this file
        // carried over the legacy 7-slot relation before the expansion.
        // GapAccumulator's challenges are caller-supplied via Step { r, r_t }
        // (not internally derived), so both legs genuinely fold with identical
        // r; r_w/r_t only affect the Pedersen commitments, which the
        // witness-only leg doesn't carry — (W, E, u) are honestly comparable.
        use volar_fold::ivc::{GapAccumulator, Step};

        let tuples: [(u8, u8, u8, u8); 3] =
            [(0x37, 0x82, 0x5a, 0xc3), (0x01, 0xff, 0x00, 0x1d), (0xaa, 0x55, 0x99, 0x02)];
        let gf2k_params = Gf2kParams::new(8, 0x1b);
        let r1cs = gf2k_r1cs_k8();
        // Generators must cover the longest committed vector (E: 174 slots).
        let pedersen = PedersenParams::setup(r1cs.num_cons, 7);

        let mut state = fold_accumulator_fresh();
        let mut acc = GapAccumulator::new();
        for (i, &(ka, kb, vv, delta)) in tuples.iter().enumerate() {
            let kc = gf_mul(gf_mul(ka, kb) ^ vv, gf_invert(delta));
            let r = FoldScalar::from_u64(0xabcd + i as u64);

            state = fold_and_gate(state, q1(ka), q1(kb), q1(kc), &delta1(delta), hat1(vv), r);

            let (_, gate_w) = and_check_gf2k(
                &gf2k_params,
                &Galois(ka).lift_bits(),
                &Galois(kb).lift_bits(),
                &Galois(kc).lift_bits(),
                &Galois(delta).lift_bits(),
                &Galois(vv).lift_bits(),
            );
            let step = Step {
                w: gate_w,
                r_w: FoldScalar::from_u64(11 + i as u64),
                r,
                r_t: FoldScalar::from_u64(99 + i as u64),
            };
            acc.push(&r1cs, &pedersen, &step);
        }

        let (w, e, u) = state.witness().expect("state must be folded after >=1 gate");
        let gp = acc.finish(
            &pedersen,
            &[FoldScalar::from_u64(1)],
            &FoldScalar::from_u64(2),
            &[FoldScalar::from_u64(3)],
            &FoldScalar::from_u64(4),
        );
        assert_eq!(w, gp.final_w.w.as_slice(), "folded W must agree elementwise");
        assert_eq!(e, gp.final_w.e.as_slice(), "folded E must agree elementwise");
        assert_eq!(*u, gp.final_u.u, "folded u must agree");
        assert!(r1cs.is_satisfied_relaxed(w, e, u));
    }
}
