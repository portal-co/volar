//! cargo-fuzz target (dormant scaffold): for a fuzzer-generated straight-line
//! And/Xor/Not circuit and honest inputs, running the real VOLE
//! prover+verifier primitives over every gate of an honest run must always
//! pass — the same completeness property as
//! `crates/fold/volar-fold/tests/vole_circuit_completeness.rs`'s
//! `prop_completeness_honest_vole_proof_passes`.
//!
//! This mirrors that file's generator/walker independently (small,
//! deliberate duplication — see that file's module doc for why
//! `volar_fuzz::generators::biir` can't be reused directly across a crate
//! boundary) rather than sharing code, since this target is genuinely
//! **dormant**: real coverage-guided fuzzing benefits from LLVM sanitizer/
//! coverage instrumentation, and this repo's LLVM backend (AST-to-LLVM)
//! isn't mature/available enough yet for that to be worthwhile (see
//! `docs/agent-context/lir-u128-support.md` and the LLVM-backend gating
//! noted throughout `docs/`). This target exists so the scaffolding is in
//! place and exercised at least once (build-checked), not to run routinely
//! or to be wired into CI.

#![no_main]

use libfuzzer_sys::fuzz_target;
use volar_spec::field::Galois;
use volar_spec::ot::IdealCot;
use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};
use volar_spec::vole::setup::{derive_and_q, random_nonzero_delta, vole_commit_bit};
use volar_spec::vole::{Delta, Q, Vope};
use volar_spec::{Array, SpecRng};

type N = cipher::consts::U16;
type U1 = cipher::consts::U1;

struct FuzzRng(u64);
impl SpecRng for FuzzRng {
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

fn one_vope() -> Vope<N, Galois, U1> {
    Vope {
        u: Array::<Array<Galois, N>, U1>::from_fn(|_| Array::<Galois, N>::from_fn(|_| Galois(1))),
        v: Array::<Galois, N>::from_fn(|_| Galois(0)),
    }
}

#[derive(Clone, Copy)]
enum Gate {
    And(usize, usize),
    Xor(usize, usize),
    Not(usize),
}

/// Decode fuzzer bytes into `(seed, inputs, gates)`. Every operand index is
/// clamped modulo the number of wires defined so far, so any byte string
/// decodes to a valid, acyclic, straight-line circuit — no rejection needed.
fn decode(data: &[u8]) -> Option<(u64, Vec<bool>, Vec<Gate>)> {
    if data.len() < 9 {
        return None;
    }
    let seed = u64::from_le_bytes(data[0..8].try_into().unwrap());
    let mut pos = 8usize;
    let mut next_byte = || {
        let b = data[pos % data.len()];
        pos = pos.wrapping_add(1);
        b
    };

    let n_inputs = 1 + (next_byte() as usize % 4);
    let inputs: Vec<bool> = (0..n_inputs).map(|_| next_byte() & 1 == 1).collect();

    let n_gates = 1 + (next_byte() as usize % 10);
    let mut gates = Vec::with_capacity(n_gates);
    let mut n_avail = n_inputs;
    for _ in 0..n_gates {
        let kind = next_byte();
        let a = next_byte() as usize % n_avail;
        let b = next_byte() as usize % n_avail;
        gates.push(match kind % 3 {
            0 => Gate::And(a, b),
            1 => Gate::Xor(a, b),
            _ => Gate::Not(a),
        });
        n_avail += 1;
    }
    Some((seed, inputs, gates))
}

fn run_honest_circuit(inputs: &[bool], gates: &[Gate], seed: u64) -> bool {
    let mut rng = FuzzRng(seed);
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

fuzz_target!(|data: &[u8]| {
    let Some((seed, inputs, gates)) = decode(data) else { return };
    assert!(
        run_honest_circuit(&inputs, &gates, seed),
        "honest VOLE circuit completeness failed for seed={seed}"
    );
});
