// @reliability: experimental
//! @ai: assisted
//! **Prove-the-verifier (IOP path): runtime side.** Owns everything that
//! *executes* an `IopSink`-woven VOLE verifier and produces/checks its
//! finalization proof — the counterpart to `volar-verifier-fold`'s
//! compile-time-only `emit_verifier_c`/`emit_verifier_rust` terminals
//! (backend-agnostic — see `docs/prove-the-verifier-iop.md`). Deliberately
//! depends only on `volar-iop`/`volar-spec`/`volar-discipline` (+ `std`),
//! never on `volar-compiler`/`volar-lir-codegen`/`volar-c-backend`/
//! `volar-weaver`, so the compiler-crate dependency graph stays
//! one-directional (weave → print → text → this crate).
//!
//! Two halves:
//!
//! 1. **In-loop fold-linking definitions** ([`IopLift`], [`IopChallenge`],
//!    [`IopAccumulator`], [`iop_accumulator_fresh`], [`iop_fold_gate`]) —
//!    the concrete definitions an `IopSink`-woven verifier links against for
//!    its `IopAccumulator`/`IopChallenge`/`iop_accumulator_fresh`/
//!    `iop_fold_gate` bare names (see
//!    `volar_weaver::vole::VerifierTraceSink`'s doc, and `IopSink`'s own
//!    doc, for why those names are left unresolved at weave time).
//! 2. [`prove_and_verify_iop`] — the pipeline terminal (Phase 2) — and
//!    [`run_iop_verifier`], the generic "print → temp Cargo project → real
//!    `cargo`/`rustc`" harness (`AGENTS.md` rule 2).
//!
//! ## Why `IopLift` is a sound embedding
//!
//! `T` (the VOLE's own field, e.g. `Galois` for `GF(2^8)`) is a different
//! Rust type from [`Gf128`] (this crate's fold/challenge field), even though
//! `Gf128` is built as a tower *extension* of (an encoding of) `Galois` —
//! Rust still needs an explicit conversion. [`IopLift::iop_embed`] is a
//! **canonical ring embedding**: it places `T`'s value at the tower's base
//! level with every higher limb zero. This is sound *by construction* (it's
//! the literal definition of a field extension containing its base field)
//! — ordinary type plumbing, not a cryptographic design decision.

use volar_discipline::{NonZk, Tagged, Transparent};
use volar_iop::field::{Field as IopField, Gf128};
use volar_iop::fold::fold_gate;
use volar_iop::ligero::LigeroProof;
use volar_spec::field::{Bit, Galois};
use volar_spec::vole::{Delta, Q};
use volar_spec::{Array, ArraySize};

// ============================================================================
// In-loop fold-linking definitions (what an IopSink-woven verifier links
// against — see this module's doc for the IopLift soundness note)
// ============================================================================

/// The fold/challenge field an `IopSink`-woven verifier's `IopChallenge`
/// bare name resolves to.
pub type IopChallenge = Gf128;

/// What an `IopSink`-woven verifier's `IopAccumulator` bare name resolves
/// to — the fixed-size accumulator, native to [`Gf128`].
pub type IopAccumulator = volar_iop::fold::IopAccumulator<Gf128>;

/// Canonical, characteristic-preserving embedding of a VOLE field element
/// into [`Gf128`] — see this module's doc for why this is sound by
/// construction (a plain ring embedding, not a cross-field cast).
pub trait IopLift {
    fn iop_embed(&self) -> Gf128;
}

/// `GF(2^8)`: embed at the tower's base level, every higher limb zero.
impl IopLift for Galois {
    fn iop_embed(&self) -> Gf128 {
        use volar_iop::field::Ext;
        let g16 = Ext::new(*self, Galois::ZERO);
        let g32 = Ext::new(g16, volar_iop::field::Gf16::ZERO);
        let g64 = Ext::new(g32, volar_iop::field::Gf32::ZERO);
        Ext::new(g64, volar_iop::field::Gf64::ZERO)
    }
}

/// `GF(2)`: embed the bit as `Galois(0|1)`, then as above.
impl IopLift for Bit {
    fn iop_embed(&self) -> Gf128 {
        Galois(self.0 as u8).iop_embed()
    }
}

/// What an `IopSink`-woven verifier's `iop_accumulator_fresh` bare name
/// resolves to.
pub fn iop_accumulator_fresh() -> IopAccumulator {
    IopAccumulator::fresh()
}

/// What an `IopSink`-woven verifier's `iop_fold_gate` bare name resolves
/// to: fold one AND gate's observed values into `state`, natively in
/// `Gf128` — no bit-expansion, no R1CS-embedding gadget needed. Lane-0
/// projection of the `N` parallel VOLE lanes — a documented simplification
/// (per-gate Δ-independence across lanes is a separate, already-flagged
/// concern; see `docs/prove-the-verifier-iop.md`'s honest scope).
pub fn iop_fold_gate<N, T>(
    state: IopAccumulator,
    k_a: Q<N, T>,
    k_b: Q<N, T>,
    k_c: Q<N, T>,
    delta: &Delta<N, T>,
    hat: Array<T, N>,
    r: IopChallenge,
) -> IopAccumulator
where
    N: ArraySize,
    T: IopLift,
{
    let ka = k_a.q[0].iop_embed();
    let kb = k_b.q[0].iop_embed();
    let kc = k_c.q[0].iop_embed();
    let d = delta.delta[0].iop_embed();
    let v = hat[0].iop_embed();
    fold_gate(state, ka, kb, kc, d, v, r)
}

// ============================================================================
// Pipeline terminal (Phase 2: finalization)
// ============================================================================

/// **Pipeline terminal (finalization leg).** Fold the whole verifier
/// (Phase 1, already folded into `acc` by the woven verifier calling
/// [`iop_fold_gate`] once per gate) and produce + check the finalization
/// proof (Phase 2), including the memory-accumulator boundary
/// `(mem_acc_in, mem_acc_out)` — pass empty slices for a circuit with no
/// committed storage. `expected_mem_acc`, if given, additionally checks the
/// proof's recovered memory boundary against a caller-supplied expectation.
/// Returns the proof and whether it verifies.
///
/// Bound `where Z: NonZk`: a [`volar_discipline::Zk`] artifact cannot reach
/// here (compile error).
pub fn prove_and_verify_iop<Z: NonZk>(
    acc: Tagged<Z, IopAccumulator>,
    mem_acc_in: &[Gf128],
    mem_acc_out: &[Gf128],
    expected_mem_acc: Option<(&[Gf128], &[Gf128])>,
) -> (Tagged<Transparent, LigeroProof<Gf128>>, bool) {
    let proof = volar_iop::prove_verifier_iop(acc, mem_acc_in, mem_acc_out);
    let ok = volar_iop::verify_iop(&proof, expected_mem_acc);
    (proof, ok)
}

// ============================================================================
// Generic compile-and-run harness
// ============================================================================

fn workspace_root() -> std::string::String {
    let mut dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.pop(); // .../volar/crates/iop
    dir.pop(); // .../volar/crates
    dir.pop(); // .../volar
    dir.to_string_lossy().into_owned()
}

/// Compile and run `rust_source` (already-printed Rust — e.g. from
/// `volar_verifier_fold::emit_verifier_rust`) together with `driver_src` (a
/// hand-written `#[test]` module that calls into it) as a real, standalone
/// Cargo project — same "print → temp Cargo project → cargo test → capture
/// output" shape as this repo's other such harnesses (`AGENTS.md` rule 2).
/// Returns captured stdout on success; panics with stdout+stderr on failure.
pub fn run_iop_verifier(rust_source: &str, driver_src: &str) -> std::string::String {
    let root = workspace_root();
    let tmpdir = std::env::temp_dir().join(std::format!(
        "volar_verifier_iop_runtime_{}",
        std::process::id()
    ));
    let srcdir = tmpdir.join("src");
    std::fs::create_dir_all(&srcdir).expect("create temp src dir");

    let full_src = std::format!(
        "{rust_source}\n\n\
         use volar_verifier_iop_runtime::*;\n\n\
         #[cfg(test)]\n\
         mod driver {{\n\
             use super::*;\n\
             {driver_src}\n\
         }}\n"
    );

    let cargo_toml = std::format!(
        "[package]\n\
         name = \"volar-verifier-iop-runtime-check\"\n\
         version = \"0.1.0\"\n\
         edition = \"2024\"\n\
         \n\
         [[test]]\n\
         name = \"driver\"\n\
         path = \"src/lib.rs\"\n\
         \n\
         [dependencies]\n\
         volar-verifier-iop-runtime = {{ path = \"{root}/crates/iop/volar-verifier-iop-runtime\" }}\n\
         volar-iop = {{ path = \"{root}/crates/iop/volar-iop\" }}\n\
         volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
         volar-discipline = {{ path = \"{root}/crates/ir/volar-discipline\" }}\n\
         hybrid-array = {{ version = \"0.4.8\", default-features = false }}\n\
         cipher = {{ version = \"0.5.1\", default-features = false }}\n"
    );

    std::fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).expect("write Cargo.toml");
    std::fs::write(srcdir.join("lib.rs"), &full_src).expect("write src/lib.rs");

    let output = std::process::Command::new("cargo")
        .args(["test", "--quiet", "--test", "driver", "--", "--nocapture"])
        .current_dir(&tmpdir)
        .env("CARGO_TARGET_DIR", tmpdir.join("target").to_string_lossy().into_owned())
        .output()
        .expect("failed to run cargo test");

    let stdout = std::string::String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = std::string::String::from_utf8_lossy(&output.stderr).into_owned();

    if !output.status.success() {
        panic!(
            "run_iop_verifier: compile/run failed\n--- source ---\n{full_src}\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}"
        );
    }
    stdout
}

#[cfg(test)]
mod tests {
    use super::*;
    use hybrid_array::Array as HArray;
    use volar_iop::transcript::FromBytes as _;

    fn honest_gate(a: u8, b: u8, d: u8) -> (Galois, Galois, Galois, Galois, Galois) {
        // GF(2^8) multiply/inverse implemented locally to avoid a new
        // dependency, and to pick k_c so v_hat = 0.
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
        let c = gf_mul(gf_mul(a, b), gf_invert(d));
        (Galois(a), Galois(b), Galois(c), Galois(d), Galois(0))
    }

    fn q1(x: Galois) -> Q<cipher::consts::U1, Galois> {
        Q { q: HArray::<Galois, cipher::consts::U1>::from_fn(|_| x) }
    }
    fn delta1(x: Galois) -> Delta<cipher::consts::U1, Galois> {
        Delta { delta: HArray::<Galois, cipher::consts::U1>::from_fn(|_| x) }
    }
    fn hat1(x: Galois) -> Array<Galois, cipher::consts::U1> {
        HArray::<Galois, cipher::consts::U1>::from_fn(|_| x)
    }

    #[test]
    fn iop_lift_embeds_at_base_level() {
        let embedded = Galois(0x53).iop_embed();
        assert_eq!(embedded.to_bytes()[0], 0x53);
        assert!(embedded.to_bytes()[1..].iter().all(|b| *b == 0));
    }

    #[test]
    fn fresh_single_gate_accumulator_is_plain_satisfied() {
        let (ka, kb, kc, delta, vv) = honest_gate(0x37, 0x82, 0xc3);
        let state = iop_fold_gate(
            iop_accumulator_fresh(),
            q1(ka),
            q1(kb),
            q1(kc),
            &delta1(delta),
            hat1(vv),
            IopChallenge::from_u64(0xabcd),
        );
        let (w, e, u) = state.witness().expect("folded after one gate");
        assert_eq!(*u, IopChallenge::ONE, "fresh instance has u = 1");
        assert!(volar_iop::fold::and_check_r1cs::<Gf128>().is_satisfied_relaxed(w, e, u));
    }

    #[test]
    fn whole_verifier_folds_and_finalization_proof_verifies() {
        let mut state = iop_accumulator_fresh();
        for (i, &(a, b, d)) in [(0x37u8, 0x82u8, 0xc3u8), (0x01, 0xff, 0x1d), (0xaa, 0x55, 0x02)].iter().enumerate() {
            let (ka, kb, kc, delta, vv) = honest_gate(a, b, d);
            let r = IopChallenge::from_u64(0xabcd + i as u64);
            state = iop_fold_gate(state, q1(ka), q1(kb), q1(kc), &delta1(delta), hat1(vv), r);
        }
        let tagged: Tagged<Transparent, _> = Tagged::seal(state);
        let (proof, ok) = prove_and_verify_iop(tagged, &[], &[], None);
        assert!(ok, "honest chain of gates must produce a verifying finalization proof");
        assert_eq!(proof.discipline(), volar_discipline::Discipline::Transparent);
    }

    #[test]
    fn whole_verifier_with_memory_boundary_verifies() {
        let mut state = iop_accumulator_fresh();
        for (i, &(a, b, d)) in [(0x37u8, 0x82u8, 0xc3u8), (0x01, 0xff, 0x1d)].iter().enumerate() {
            let (ka, kb, kc, delta, vv) = honest_gate(a, b, d);
            let r = IopChallenge::from_u64(0xabcd + i as u64);
            state = iop_fold_gate(state, q1(ka), q1(kb), q1(kc), &delta1(delta), hat1(vv), r);
        }
        let tagged: Tagged<Transparent, _> = Tagged::seal(state);
        let mem_in = [IopChallenge::from_u64(1)];
        let mem_out = [IopChallenge::from_u64(2)];
        let (proof, ok) = prove_and_verify_iop(tagged, &mem_in, &mem_out, Some((&mem_in, &mem_out)));
        assert!(ok, "honest chain + matching memory-boundary expectation must verify");
        let _ = proof;
    }

    #[test]
    fn memory_boundary_mismatch_is_rejected() {
        let mut state = iop_accumulator_fresh();
        let (ka, kb, kc, delta, vv) = honest_gate(0x37, 0x82, 0xc3);
        state = iop_fold_gate(state, q1(ka), q1(kb), q1(kc), &delta1(delta), hat1(vv), IopChallenge::from_u64(1));
        let tagged: Tagged<Transparent, _> = Tagged::seal(state);
        let mem_in = [IopChallenge::from_u64(1)];
        let mem_out = [IopChallenge::from_u64(2)];
        let wrong_out = [IopChallenge::from_u64(999)];
        let (_, ok) = prove_and_verify_iop(tagged, &mem_in, &mem_out, Some((&mem_in, &wrong_out)));
        assert!(!ok, "a caller-expected memory boundary that doesn't match must be rejected");
    }

    #[test]
    fn tampered_gate_is_rejected() {
        let mut state = iop_accumulator_fresh();
        let tuples = [(0x37u8, 0x82u8, 0xc3u8), (0x01, 0xff, 0x1d), (0xaa, 0x55, 0x02)];
        for (i, &(a, b, d)) in tuples.iter().enumerate() {
            let (ka, kb, kc, delta, mut vv) = honest_gate(a, b, d);
            if i == 1 {
                vv = Galois(vv.0 ^ 1); // tamper the middle gate
            }
            let r = IopChallenge::from_u64(0xabcd + i as u64);
            state = iop_fold_gate(state, q1(ka), q1(kb), q1(kc), &delta1(delta), hat1(vv), r);
        }
        let tagged: Tagged<Transparent, _> = Tagged::seal(state);
        let (_, ok) = prove_and_verify_iop(tagged, &[], &[], None);
        assert!(!ok, "a tampered gate observation must fail the finalization proof");
    }
}
