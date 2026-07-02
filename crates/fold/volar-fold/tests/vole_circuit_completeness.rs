// @reliability: experimental
//! Completeness (fuzzing-style) check: for many randomly generated boolean
//! circuits and many random inputs, running the real VOLE prover+verifier
//! primitives (`volar_spec::vole::prove::{vole_and_prover_step,
//! vole_and_verifier_check}`, `volar_spec::vole::setup::derive_and_q`) over
//! every gate of an *honest* run must always pass. This is the "assert honest
//! proofs pass, over many circuits/programs with many inputs" completeness
//! test the plan calls for, exercised via `proptest` — the established
//! property-testing idiom in this repo (`volar-oram`, `volar-spec`).
//!
//! ## Circuit generator: local, not `volar_fuzz::generators::biir`
//!
//! `volar_fuzz::generators::biir::gen_biir_and_inputs` (and its variants) are
//! gated `#[cfg(test)]` inside `volar-fuzz` — that's crate-local to
//! `volar-fuzz`'s own test builds and is **not** visible to an external
//! dev-dependent crate's tests (a `#[cfg(test)]` item is absent entirely when
//! `volar-fuzz` is compiled as a normal, non-test dependency here). Rather
//! than changing `volar-fuzz`'s compilation gating for this, this file uses
//! its own small, self-contained circuit generator restricted to `And`/`Xor`/
//! `Not` gates over a flat operand list — everything the VOLE walker below
//! needs, with no `volar-ir`/`volar-fuzz` dependency. The dormant cargo-fuzz
//! target (`fuzz/fuzz_targets/fuzz_vole_circuit_completeness.rs`) mirrors this
//! generator/walker independently for the same reason.

use proptest::prelude::*;
use volar_spec::field::Galois;
use volar_spec::ot::IdealCot;
use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};
use volar_spec::vole::setup::{derive_and_q, random_nonzero_delta, vole_commit_bit};
use volar_spec::vole::{Delta, Q, Vope};
use volar_spec::{Array, SpecRng};

type N = cipher::consts::U16;
type U1 = cipher::consts::U1;

/// SplitMix64-based deterministic test RNG (mirrors the pattern in
/// `volar_spec::vole::setup`'s own `#[cfg(test)]` module — that struct is
/// crate-local for the same `#[cfg(test)]` reason as the generators above).
struct TestRng(u64);
impl SpecRng for TestRng {
    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        (z ^ (z >> 31)) as u32
    }
}

fn sample_g<R: SpecRng>(r: &mut R) -> Galois {
    Galois(r.next_u8())
}
fn lift_bit_g(b: bool) -> Galois {
    Galois(if b { 1 } else { 0 })
}
fn is_zero_g(g: &Galois) -> bool {
    g.0 == 0
}

/// A public-constant Vope for bit `1`: `u = 1` (lifted), `v = 0`, matching
/// what the real weaver emits for `Not` (`crate::weaver`'s `vope_one` —
/// `emit_prover_and_gate`/`Not` arm in `volar-weaver/src/vole.rs`).
fn one_vope() -> Vope<N, Galois, U1> {
    Vope {
        u: Array::<Array<Galois, N>, U1>::from_fn(|_| Array::<Galois, N>::from_fn(|_| Galois(1))),
        v: Array::<Galois, N>::from_fn(|_| Galois(0)),
    }
}

/// One gate in the local circuit representation. Operand indices are into the
/// flat `inputs ++ gate outputs` wire list, in evaluation order.
#[derive(Clone, Copy, Debug)]
enum Gate {
    And(usize, usize),
    Xor(usize, usize),
    Not(usize),
}

/// Run an honest VOLE prover+verifier over every gate of `gates`, given
/// `inputs` as the circuit's honest input bits and `seed` driving the VOLE
/// setup randomness (Δ, per-wire masks). Returns whether *every* gate's
/// Quicksilver check passed.
///
/// - `And`: fresh VOLE wire via [`vole_and_prover_step`]/[`derive_and_q`]/
///   [`vole_and_verifier_check`] — the real, checked primitive.
/// - `Xor`/`Not`: free (linear) combinations, matching exactly what
///   `weave_vole_prover_inner`/`weave_vole_verifier_inner` emit
///   (`volar-weaver/src/vole.rs`'s `Xor`/`Not` arms): `Xor` adds the two
///   wires' `Vope`/`Q` directly; `Not` adds the public-constant [`one_vope`]
///   (prover) / `Δ` (verifier).
fn run_honest_circuit(inputs: &[bool], gates: &[Gate], seed: u64) -> bool {
    let mut rng = TestRng(seed);
    let delta: Delta<N, Galois> = random_nonzero_delta(&mut rng, sample_g, is_zero_g);
    let cot = IdealCot::new(delta.clone());

    let mut prover_wires: Vec<Vope<N, Galois, U1>> = Vec::with_capacity(inputs.len() + gates.len());
    let mut verifier_wires: Vec<Q<N, Galois>> = Vec::with_capacity(inputs.len() + gates.len());

    for &bit in inputs {
        let (vope, q) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, bit);
        prover_wires.push(vope);
        verifier_wires.push(q);
    }

    let mut all_ok = true;
    for gate in gates {
        match *gate {
            Gate::And(a, b) => {
                let (vope_c, hat) =
                    vole_and_prover_step(prover_wires[a].clone(), prover_wires[b].clone());
                let (q_a, q_b) = (&verifier_wires[a], &verifier_wires[b]);
                let q_and = derive_and_q(&delta, q_a, q_b, &hat);
                let (q_out, ok) = vole_and_verifier_check(&delta, q_a, q_b, &q_and, &hat);
                all_ok = all_ok && ok;
                prover_wires.push(vope_c);
                verifier_wires.push(q_out);
            }
            Gate::Xor(a, b) => {
                let vope_c = prover_wires[a].clone() + prover_wires[b].clone();
                let q_c = Q {
                    q: Array::<Galois, N>::from_fn(|i| {
                        verifier_wires[a].q[i].clone() + verifier_wires[b].q[i].clone()
                    }),
                };
                prover_wires.push(vope_c);
                verifier_wires.push(q_c);
            }
            Gate::Not(a) => {
                let vope_c = prover_wires[a].clone() + one_vope();
                let q_c = Q {
                    q: Array::<Galois, N>::from_fn(|i| {
                        verifier_wires[a].q[i].clone() + delta.delta[i].clone()
                    }),
                };
                prover_wires.push(vope_c);
                verifier_wires.push(q_c);
            }
        }
    }
    all_ok
}

/// Generate `(inputs, gates)`: 1-4 input wires, 1-10 gates each referencing
/// only already-defined wires (inputs or earlier gate outputs) — always a
/// valid, acyclic, straight-line circuit by construction.
fn gen_circuit_and_inputs() -> impl Strategy<Value = (Vec<bool>, Vec<Gate>)> {
    (1usize..=4).prop_flat_map(|n_inputs| {
        let inputs = proptest::collection::vec(any::<bool>(), n_inputs);
        let raw_gates = proptest::collection::vec((0u8..3, any::<u32>(), any::<u32>()), 1..=10);
        (inputs, raw_gates).prop_map(move |(inputs, raw_gates)| {
            let mut gates = Vec::with_capacity(raw_gates.len());
            let mut n_avail = n_inputs;
            for (kind, a, b) in raw_gates {
                let av = (a as usize) % n_avail;
                let bv = (b as usize) % n_avail;
                gates.push(match kind % 3 {
                    0 => Gate::And(av, bv),
                    1 => Gate::Xor(av, bv),
                    _ => Gate::Not(av),
                });
                n_avail += 1;
            }
            (inputs, gates)
        })
    })
}

#[test]
fn honest_examples() {
    // a AND b, both true.
    assert!(run_honest_circuit(&[true, true], &[Gate::And(0, 1)], 1));
    // a AND b, one false.
    assert!(run_honest_circuit(&[true, false], &[Gate::And(0, 1)], 2));
    // a XOR b, then NOT the result, then AND with a — a small mixed chain.
    let gates = [Gate::Xor(0, 1), Gate::Not(2), Gate::And(0, 3)];
    assert!(run_honest_circuit(&[true, false], &gates, 3));
    assert!(run_honest_circuit(&[false, false], &gates, 4));
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(48))]

    /// Completeness: for any generated straight-line And/Xor/Not circuit and
    /// any honest inputs, every gate's honest VOLE check passes.
    #[test]
    fn prop_completeness_honest_vole_proof_passes(
        (inputs, gates) in gen_circuit_and_inputs(),
        seed in any::<u64>(),
    ) {
        prop_assert!(run_honest_circuit(&inputs, &gates, seed));
    }
}
