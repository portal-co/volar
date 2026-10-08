// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-phase-2
//! @ai: assisted
//! BitGC-small and BitGC-fast construction drivers.
//!
//! These drivers run a [`Circuit`] over the offline-expanded material
//! ([`super::seed`]) using a [`GateEquation`] for the per-gate semantics.
//! With [`ReferenceEquation::SMALL`] this is the **BitGC-small** shape
//! ([ZLL+26] §4: 1 bit per AND, 1 bit per XOR); with
//! [`ReferenceEquation::FAST`] it is the **BitGC-fast** shape
//! ([ZLL+26] §5.1: 5 bits per AND, free XOR).
//!
//! # Pinning status
//!
//! The drivers implement the communication shape, the offline/online
//! split, the SIMD SWHE machinery, and the free-XOR/permutation-bit
//! structure of the paper — everything except the exact per-gate
//! stitching equations, which sit behind the `GateEquation` seam pending
//! paper pinning (BITGC-LEDGER-04). Do not use for protected data.
//!
//! # Output opening
//!
//! The evaluator recovers output bits by decrypting its active output
//! labels (semi-honest structure; a real 2PC protocol binds the opening
//! to a session transcript — out of scope here, see the plan §3).

use alloc::vec::Vec;

use super::common::{
    Circuit, ExpandedGate, Gate, GateEquation, GlobalDelta, Stitch, StitchError, WireState,
};
use super::seed::{OfflineEvaluator, OfflineGarbler};
use super::swhe::{Ciphertext, Context, KeySwitchKeySet};

/// Garble a circuit, producing the online stitching record stream.
///
/// `equation` selects the variant semantics. The returned records are in
/// circuit order (charged gates only — free XOR gates emit nothing).
pub fn garble(
    ctx: &Context,
    equation: &impl GateEquation,
    circuit: &Circuit,
    offline: &OfflineGarbler,
    delta: &GlobalDelta,
) -> Result<Vec<Stitch>, StitchError> {
    circuit.validate()?;
    let p = ctx.parameters().plaintext_modulus;
    if delta.value() >= p {
        return Err(StitchError::NonCanonicalBit);
    }

    // Per-wire garbler state from the expanded material.
    let mut wires: Vec<WireState> = Vec::with_capacity(circuit.wires());
    for w in 0..circuit.wires() {
        let (a0, pi) = offline.wire_bits(ctx, w)?;
        wires.push(WireState { a0: a0 % p, pi });
    }

    let mut records = Vec::new();
    for (index, gate) in circuit.gates.iter().enumerate() {
        let out_wire = circuit.inputs + index;
        let (ai, bi) = gate.operands();
        let (a, b) = (wires[ai], wires[bi]);
        match gate {
            Gate::And { .. } => {
                let out = wires[out_wire];
                records.push(equation.garble_and(index, &a, &b, &out));
            }
            Gate::Xor { .. } => {
                let mut out = wires[out_wire];
                if !equation.xor_is_charged() {
                    // Free-XOR structural rule ([ZLL+26] free-XOR
                    // contribution): the output wire's state is derived
                    // from the inputs so `L_a + L_b` is the output's
                    // active label with zero communication. The expanded
                    // candidate for this wire stays unused on the
                    // garbler side; evaluation never consults it either
                    // (payload-add path).
                    out = WireState {
                        a0: (a.a0 + b.a0) % p,
                        pi: a.pi ^ b.pi,
                    };
                    wires[out_wire] = out;
                }
                if let Some(stitch) = equation.garble_xor(p, &a, &b, &out) {
                    records.push(stitch);
                }
            }
        }
    }
    Ok(records)
}

/// Evaluate a garbled circuit.
///
/// `input_labels` encrypts each input wire's active label
/// (`A0 + (v ⊕ π)·Δ`); the garbler produces it from [`OfflineGarbler`]
/// wire bits and the clear input values (or it arrives via the session's
/// input-label delivery path, unchanged by this construction).
///
/// Returns the decrypted output bits (evaluator-side opening; see module
/// docs).
pub fn evaluate(
    ctx: &Context,
    equation: &impl GateEquation,
    circuit: &Circuit,
    offline: &OfflineEvaluator,
    delta: &GlobalDelta,
    ksk: &KeySwitchKeySet,
    input_labels: &[Ciphertext],
    records: &[Stitch],
    output_key: &super::swhe::SecretKey,
) -> Result<Vec<bool>, StitchError> {
    circuit.validate()?;
    if input_labels.len() != circuit.inputs {
        return Err(StitchError::Exhausted);
    }

    // Gate evaluation happens at the online base level: the level the
    // expanded material landed at after expansion consumed its depth.
    // `Context::mul` walks the chain down as multiplications accumulate,
    // and equation internals align mixed levels as needed.
    let top = offline
        .wires
        .first()
        .ok_or(StitchError::Exhausted)?
        .ct_a0
        .level();
    let mut wires: Vec<Ciphertext> = Vec::with_capacity(circuit.wires());
    for label in input_labels {
        wires.push(ctx.switch_to(label, top)?);
    }

    let mut record_iter = records.iter();
    for (index, gate) in circuit.gates.iter().enumerate() {
        let out_wire = circuit.inputs + index;
        let (ai, bi) = gate.operands();
        let expanded = ExpandedGate {
            a: align_wire(ctx, offline.wire(ai).ok_or(StitchError::Exhausted)?, top)?,
            b: align_wire(ctx, offline.wire(bi).ok_or(StitchError::Exhausted)?, top)?,
            // The output wire's material is consumed at the *resulting*
            // label's level (wrapping happens after multiplications),
            // which `GateEquation` aligns internally; align to top here
            // and let the equation switch as needed.
            out: align_wire(
                ctx,
                offline.wire(out_wire).ok_or(StitchError::Exhausted)?,
                top,
            )?,
        };
        let la = &wires[ai];
        let lb = &wires[bi];
        let out = match gate {
            Gate::And { .. } => {
                let stitch = record_iter.next().ok_or(StitchError::Exhausted)?;
                equation.eval_and(ctx, la, lb, &expanded, delta, ksk, stitch)?
            }
            Gate::Xor { .. } => {
                let stitch = if equation.xor_is_charged() {
                    Some(record_iter.next().ok_or(StitchError::Exhausted)?)
                } else {
                    None
                };
                equation.eval_xor(ctx, la, lb, &expanded, delta, ksk, stitch)?
            }
        };
        wires.push(out);
    }
    if record_iter.next().is_some() {
        return Err(StitchError::NonCanonicalBit);
    }

    // Open the outputs: unwrap to Enc(masked payload), reduce to parity,
    // decrypt, and unmask with the expanded π candidates.
    //
    // The evaluator holds no clear π values; the reference opening
    // decrypts the payload parity and the expanded π ciphertext and
    // combines them. Distributed decryption is a session-level seam.
    let p = ctx.parameters().plaintext_modulus;
    let inv_delta = delta.inverse(p).ok_or(StitchError::NonCanonicalBit)?;
    let mut outputs = Vec::with_capacity(circuit.outputs.len());
    for &wire in &circuit.outputs {
        let expanded = offline.wire(wire).ok_or(StitchError::Exhausted)?;
        let label = &wires[wire];
        // payload = (L − A0)·Δ⁻¹, parity = masked bit
        let diff = ctx.sub(label, &expanded.ct_a0)?;
        let payload = ctx.mul_plain(&diff, &ctx.constant_slots(inv_delta))?;
        let payload_bits = ctx.decrypt(output_key, &payload)?;
        let ct_pi = ctx.switch_to(&expanded.ct_pi, payload.level())?;
        let pi_bits = ctx.decrypt(output_key, &ct_pi)?;
        let masked = payload_bits[expanded.slot_a0] % 2;
        let pi = pi_bits[expanded.slot_pi] % 2;
        outputs.push((masked ^ pi) == 1);
    }
    Ok(outputs)
}

/// Switch both expanded ciphertexts of a wire to `level`.
fn align_wire(
    ctx: &Context,
    wire: &super::common::ExpandedWire,
    level: usize,
) -> Result<super::common::ExpandedWire, StitchError> {
    Ok(super::common::ExpandedWire {
        ct_a0: ctx.switch_to(&wire.ct_a0, level)?,
        ct_pi: ctx.switch_to(&wire.ct_pi, level)?,
        slot_a0: wire.slot_a0,
        slot_pi: wire.slot_pi,
    })
}

#[cfg(test)]
mod tests {
    use super::super::common::ReferenceEquation;
    use super::super::prg::ReferencePrg;
    use super::super::seed::offline_phase;
    use super::super::swhe::{Error as SwheError, Parameters, RandomSource, ZeroNoise};
    use super::*;

    /// Deterministic byte source for tests (LCG counter).
    struct TestRandom(u64);
    impl RandomSource for TestRandom {
        fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), SwheError> {
            for byte in output.iter_mut() {
                self.0 = self
                    .0
                    .wrapping_mul(6364136223846793005)
                    .wrapping_add(1442695040888963407);
                *byte = (self.0 >> 33) as u8;
            }
            Ok(())
        }
    }

    /// Test profile: degree 8, plaintext modulus 97, and an eight-level
    /// chain of primes `≡ 1 mod 1552` (`lcm(2·degree, p)`) so the BGV
    /// modulus switch preserves messages. Depth budget: expansion 1 +
    /// online 2, with headroom for the reference adapter's transient
    /// level arithmetic. Correctness-only; no security claim.
    fn test_context() -> Context {
        Context::new(Parameters {
            degree: 8,
            plaintext_modulus: 97, // prime, ≡ 1 mod 16
            chain: alloc::vec![
                1_099_511_645_201,
                1_099_511_673_137,
                1_099_511_677_793,
                1_099_511_704_177,
                1_099_511_705_729,
                1_099_511_755_393,
                1_099_511_798_849,
                1_099_511_801_953,
            ],
            noise_bound: 2,
        })
        .unwrap()
    }

    /// `(x0 ∧ x1) ⊕ x2` — one AND, one XOR.
    fn small_circuit() -> Circuit {
        Circuit {
            inputs: 3,
            gates: alloc::vec![Gate::And { a: 0, b: 1 }, Gate::Xor { a: 3, b: 2 }],
            outputs: alloc::vec![4],
        }
    }

    fn run_roundtrip(equation: ReferenceEquation, inputs: [bool; 3]) -> bool {
        let ctx = test_context();
        let circuit = small_circuit();
        let delta = GlobalDelta::new(5, ctx.parameters().plaintext_modulus).unwrap();
        let prg = ReferencePrg::new(1); // slot-0 discipline: one bit per seed

        let mut random = TestRandom(0x1234_5678_9abc_def0);
        // Chain has 4 primes: expansion depth 1 + online budget 2.
        let (garbler, evaluator) = offline_phase(
            &ctx,
            circuit.wires(),
            &prg,
            1 << 10,
            7,
            2,
            &mut random,
            &mut ZeroNoise,
        )
        .unwrap();

        let records = garble(&ctx, &equation, &circuit, &garbler, &delta).unwrap();

        // Input labels: encrypt active labels under the offline secret at
        // the online level.
        let mut input_labels = Vec::new();
        for (w, &bit) in inputs.iter().enumerate() {
            let (a0, pi) = garbler.wire_bits(&ctx, w).unwrap();
            let masked = bit ^ pi;
            let label =
                (a0 + if masked { delta.value() } else { 0 }) % ctx.parameters().plaintext_modulus;
            let mut slots = alloc::vec![0_u64; ctx.parameters().degree];
            slots[evaluator.wire(w).unwrap().slot_a0] = label;
            // Online base = top - expansion depth (6 for the test chain).
            input_labels.push(
                ctx.encrypt(&garbler.secret, 6, &slots, &mut random, &mut ZeroNoise)
                    .unwrap(),
            );
        }

        let outputs = evaluate(
            &ctx,
            &equation,
            &circuit,
            &evaluator,
            &delta,
            &garbler.ksks,
            &input_labels,
            &records,
            &garbler.secret,
        )
        .unwrap();
        outputs[0]
    }

    #[test]
    fn small_variant_truth_table() {
        for bits in 0..8u8 {
            let inputs = [bits & 1 == 1, bits & 2 == 2, bits & 4 == 4];
            let expect = (inputs[0] & inputs[1]) ^ inputs[2];
            let got = run_roundtrip(ReferenceEquation::SMALL, inputs);
            assert_eq!(got, expect, "inputs {inputs:?}");
        }
    }

    #[test]
    fn fast_variant_truth_table() {
        for bits in 0..8u8 {
            let inputs = [bits & 1 == 1, bits & 2 == 2, bits & 4 == 4];
            let expect = (inputs[0] & inputs[1]) ^ inputs[2];
            let got = run_roundtrip(ReferenceEquation::FAST, inputs);
            assert_eq!(got, expect, "inputs {inputs:?}");
        }
    }
}

