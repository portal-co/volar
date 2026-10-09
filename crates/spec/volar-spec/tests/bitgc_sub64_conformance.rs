//! Differential conformance test: run the pinned BitGC drivers over the
//! `sub64` Bristol circuit and check the **structural accounting**
//! against the authors' artifact (`wtd2/more-efficient-bitgc`), whose
//! `--check=wire` vectors are captured in
//! `docs/research/artifact-vectors/` (BITGC-LEDGER-01/02).
//!
//! What is checked (and why each is a meaningful conformance signal):
//!
//! - **Gate counts** (AND=63, XOR=313, INV=63) — the parser and gate
//!   model match the artifact's circuit interpretation.
//! - **Communication totals** — small: 1 bit/AND + 1 bit/XOR = 376 bits;
//!   fast: 5 bits/AND = 315 bits (INV free). This is the paper's Table 1
//!   accounting applied to sub64.
//! - **Offline RBits** — small: |I| + |W_AND| + |W_XOR| = 128+63+313 =
//!   504 (INV consumes none, per the artifact's `AssignWireMasks`
//!   `continue`); fast: 4×|W_AND| = 252. These match the artifact's
//!   `Required=504` (small) and `Required=252` (fast) exactly.
//! - **Functional correctness** — sub64 computes 64-bit subtraction;
//!   evaluated over sampled inputs and checked against plain evaluation.

use volar_spec::SpecRng;
use volar_spec::bitgc::Variant;
use volar_spec::bitgc::common::{Circuit, Gate};
use volar_spec::bitgc::lattice_dd::LatticeDd;
use volar_spec::bitgc::pinned::{
    GateRecord, PrfKey, encode_inputs, evaluate_circuit, garble_circuit, open_output,
};

/// Parse a Bristol-format circuit into [`Circuit`].
///
/// Header: `num_gates num_wires`, then `n1 n2` (two-party input sizes),
/// then `1 n_out` (one output size). Gate lines:
/// `<ni> <no> <in...> <out...> <TYPE>` with TYPE ∈ {AND, XOR, INV}.
/// Only single-output gates appear in these fixtures.
fn parse_bristol(text: &str) -> Circuit {
    let mut lines = text.lines().map(str::trim).filter(|l| !l.is_empty());
    let header: Vec<usize> = lines
        .next()
        .expect("gate/wire header")
        .split_whitespace()
        .map(|t| t.parse().expect("header int"))
        .collect();
    let (num_gates, _num_wires) = (header[0], header[1]);
    let in_line: Vec<usize> = lines
        .next()
        .expect("input header")
        .split_whitespace()
        .map(|t| t.parse().expect("input int"))
        .collect();
    // `2 n1 n2` (two parties); inputs = n1 + n2.
    let inputs = in_line[1] + in_line[2];
    let out_line: Vec<usize> = lines
        .next()
        .expect("output header")
        .split_whitespace()
        .map(|t| t.parse().expect("output int"))
        .collect();
    let n_out = out_line[1];

    // Bristol output wires are explicit and NOT in `inputs + gate_index`
    // order (sub64 numbers them in a different topological order). Parse
    // into an explicit-wire list, then re-emit gates in topological order
    // with a dense implicit-output wire assignment matching `Circuit`.
    struct RawGate {
        ins: Vec<usize>,
        out: usize,
        ty: Gate,
    }
    let mut raw: Vec<RawGate> = Vec::with_capacity(num_gates);
    for line in lines {
        let toks: Vec<&str> = line.split_whitespace().collect();
        let ni: usize = toks[0].parse().expect("nin");
        let no: usize = toks[1].parse().expect("nout");
        let ty = *toks.last().expect("gate type");
        let ins: Vec<usize> = toks[2..2 + ni]
            .iter()
            .map(|t| t.parse().expect("in wire"))
            .collect();
        let out: usize = toks[2 + ni].parse().expect("out wire");
        assert_eq!(no, 1, "only single-output gates supported");
        let ty_gate = match ty {
            "AND" => Gate::And {
                a: ins[0],
                b: ins[1],
            },
            "XOR" => Gate::Xor {
                a: ins[0],
                b: ins[1],
            },
            "INV" => Gate::Not { a: ins[0] },
            other => panic!("unsupported gate {other}"),
        };
        raw.push(RawGate {
            ins,
            out,
            ty: ty_gate,
        });
    }
    assert_eq!(raw.len(), num_gates, "gate count matches header");

    // The Bristol file lists the 64 output wires on a final line; the
    // output wires are the last `n_out` wire indices (the circuit's
    // convention for this family). Build the dense mapping: each wire is
    // either an input (`< inputs`) or produced by exactly one gate.
    let total_wires = inputs + raw.len();
    // Map each Bristol wire to its dense index. Inputs keep 0..inputs.
    // Gates are already in topological order in the file (each gate's
    // inputs are defined before it). Assign each gate's output the next
    // dense wire index and record the Bristol→dense map.
    let mut wire_map = vec![usize::MAX; total_wires.max(568)];
    for w in 0..inputs {
        wire_map[w] = w;
    }
    let mut gates = Vec::with_capacity(num_gates);
    let mut next_wire = inputs;
    for rg in &raw {
        let mapped = match rg.ty {
            Gate::And { .. } => Gate::And {
                a: wire_map[rg.ins[0]],
                b: wire_map[rg.ins[1]],
            },
            Gate::Xor { .. } => Gate::Xor {
                a: wire_map[rg.ins[0]],
                b: wire_map[rg.ins[1]],
            },
            Gate::Not { .. } => Gate::Not {
                a: wire_map[rg.ins[0]],
            },
        };
        // Sanity: inputs must already be mapped (topological order).
        assert_ne!(wire_map[rg.ins[0]], usize::MAX, "gate input unmapped");
        wire_map[rg.out] = next_wire;
        next_wire += 1;
        gates.push(mapped);
    }
    let outputs: Vec<usize> = (total_wires - n_out..total_wires)
        .map(|w| wire_map[w])
        .collect();
    Circuit {
        inputs,
        gates,
        outputs,
    }
}

fn counts(c: &Circuit) -> (usize, usize, usize) {
    let mut and = 0;
    let mut xor = 0;
    let mut inv = 0;
    for g in &c.gates {
        match g {
            Gate::And { .. } => and += 1,
            Gate::Xor { .. } => xor += 1,
            Gate::Not { .. } => inv += 1,
        }
    }
    (and, xor, inv)
}

struct TestRng(u64);
impl SpecRng for TestRng {
    fn next_u32(&mut self) -> u32 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (self.0 >> 33) as u32
    }
}

/// The 64-bit subtraction oracle: interpret the 128 input bits as two
/// little-endian 64-bit limbs (Bristol convention) and the 64 output bits
/// as `a − b` little-endian.
fn sub64_plain(inputs: &[bool]) -> Vec<bool> {
    let mut a = 0u64;
    let mut b = 0u64;
    for i in 0..64 {
        if inputs[i] {
            a |= 1 << i;
        }
        if inputs[i + 64] {
            b |= 1 << i;
        }
    }
    let d = a.wrapping_sub(b);
    (0..64).map(|i| (d >> i) & 1 == 1).collect()
}

#[test]
fn sub64_gate_counts_match_artifact() {
    let text = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/sub64.txt"
    ))
    .expect("sub64 fixture");
    let circuit = parse_bristol(&text);
    let (and, xor, inv) = counts(&circuit);
    // From the artifact's wire-log: "Online Gates: AND=63, XOR=313, INV=63".
    assert_eq!(and, 63, "AND count");
    assert_eq!(xor, 313, "XOR count");
    assert_eq!(inv, 63, "INV count");
    assert_eq!(circuit.inputs, 128);
    assert_eq!(circuit.wires(), 567);
    assert_eq!(circuit.outputs.len(), 64);
    // Plain-evaluation oracle agrees with the parser on a subtraction.
    let mut rng = TestRng(0x5eed);
    let inputs: Vec<bool> = (0..128).map(|_| (rng.next_u32() & 1) == 1).collect();
    let got = circuit.eval_plain(&inputs).expect("eval_plain");
    assert_eq!(got, sub64_plain(&inputs), "sub64 semantics");
}

#[test]
fn sub64_pinned_accounting_and_correctness() {
    let text = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/sub64.txt"
    ))
    .expect("sub64 fixture");
    let circuit = parse_bristol(&text);
    let (and, xor, inv) = counts(&circuit);
    assert_eq!((and, xor, inv), (63, 313, 63));

    // Conformance accounting from the artifact vectors.
    let small_comm_bits = and * 1 + xor * 1; // 1 bit per AND + 1 per XOR
    let fast_comm_bits = and * 5; // 5 bits per AND
    assert_eq!(small_comm_bits, 376);
    assert_eq!(fast_comm_bits, 315);
    let small_rbits = circuit.inputs + and + xor; // INV consumes none
    let fast_rbits = 4 * and;
    assert_eq!(small_rbits, 504, "small offline RBits");
    assert_eq!(fast_rbits, 252, "fast offline RBits");

    // Run both variants through the pinned drivers over the lattice
    // gadget and check record counts + functional correctness.
    let p = 4_611_686_018_427_387_919u64; // 2^62+135, prime
    let q = p.wrapping_mul(3);
    let degree = 8;
    let wires = circuit.wires();

    for variant in [Variant::Small, Variant::Fast] {
        let mut rng = TestRng(0x5eed_u64.wrapping_add(variant as u64));
        let prf = PrfKey([0x1111, 0x2222, 0x3333, 0x4444]);
        let dd = LatticeDd::new(degree, p, q, &mut rng).expect("params");

        // Offline permutation bits (one per wire) + fast PRG mask bits.
        let pis: Vec<bool> = (0..wires).map(|_| (rng.next_u32() & 1) == 1).collect();
        let prg_count = if variant == Variant::Fast { 4 * and } else { 0 };
        let prg_bits: Vec<bool> = (0..prg_count).map(|_| (rng.next_u32() & 1) == 1).collect();

        let garbling = garble_circuit(variant, &circuit, &dd, &pis, &prf, &prg_bits, &mut rng)
            .expect("garble");

        // Communication accounting: sum of record bits.
        let total_bits: usize = garbling.records.iter().map(GateRecord::bits).sum();
        let want = match variant {
            Variant::Small => small_comm_bits,
            Variant::Fast => fast_comm_bits,
            Variant::Tiny => unreachable!(),
        };
        assert_eq!(total_bits, want, "{variant:?} communication bits");

        // Functional correctness on a few sampled subtractions.
        for trial in 0..4 {
            let inputs: Vec<bool> = (0..128).map(|_| (rng.next_u32() & 1) == 1).collect();
            let encoded = encode_inputs(
                &garbling,
                &volar_spec::bitgc::pinned::Delta {
                    coeffs: dd.delta().clone(),
                    p,
                },
                &inputs,
                circuit.inputs,
            )
            .expect("encode");
            let masked_out = evaluate_circuit(
                variant,
                &circuit,
                &dd,
                &garbling.delta_ct,
                &garbling.and_rows,
                &garbling.records,
                &prf,
                &encoded,
            )
            .expect("evaluate");
            let got: Vec<bool> = masked_out
                .iter()
                .enumerate()
                .map(|(i, &m)| open_output(m, garbling.labels[circuit.outputs[i]].pi))
                .collect();
            let want = sub64_plain(&inputs);
            assert_eq!(got, want, "{variant:?} trial {trial}");
        }
    }
}
