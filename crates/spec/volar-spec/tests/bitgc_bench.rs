//! BitGC per-gate timing benchmark (deterministic, small circuits).
//! Reports garble/eval wall-time per gate for sub64 under each variant,
//! and the exact communication bytes. This is the local benchmark; the
//! QEMU Linux run (BITGC-LEDGER-05) repeats it for the locked Thumb
//! SHA-256 fixture's communication envelope.
use std::time::Instant;
use volar_spec::bitgc::common::{Circuit, Gate};
use volar_spec::bitgc::lattice_dd::LatticeDd;
use volar_spec::bitgc::pinned::*;
use volar_spec::bitgc::Variant;
use volar_spec::SpecRng;

struct TestRng(u64);
impl SpecRng for TestRng {
    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (self.0 >> 33) as u32
    }
}

fn parse_bristol(text: &str) -> Circuit {
    let mut lines = text.lines().map(str::trim).filter(|l| !l.is_empty());
    let h: Vec<usize> = lines.next().unwrap().split_whitespace().map(|t| t.parse().unwrap()).collect();
    let (num_gates, _) = (h[0], h[1]);
    let il: Vec<usize> = lines.next().unwrap().split_whitespace().map(|t| t.parse().unwrap()).collect();
    let inputs = il[1] + il[2];
    let ol: Vec<usize> = lines.next().unwrap().split_whitespace().map(|t| t.parse().unwrap()).collect();
    let n_out = ol[1];
    struct RG { ins: Vec<usize>, out: usize, ty: Gate }
    let mut raw = Vec::new();
    for line in lines {
        let t: Vec<&str> = line.split_whitespace().collect();
        let ni: usize = t[0].parse().unwrap();
        let ins: Vec<usize> = t[2..2+ni].iter().map(|x| x.parse().unwrap()).collect();
        let out: usize = t[2+ni].parse().unwrap();
        let ty = match *t.last().unwrap() {
            "AND" => Gate::And{a:ins[0],b:ins[1]},
            "XOR" => Gate::Xor{a:ins[0],b:ins[1]},
            "INV" => Gate::Not{a:ins[0]},
            o => panic!("{o}"),
        };
        raw.push(RG { ins, out, ty });
    }
    let total = inputs + raw.len();
    let mut wm = vec![usize::MAX; total.max(568)];
    for w in 0..inputs { wm[w] = w; }
    let mut gates = Vec::new();
    let mut nw = inputs;
    for rg in &raw {
        let g = match rg.ty {
            Gate::And{..} => Gate::And{a: wm[rg.ins[0]], b: wm[rg.ins[1]]},
            Gate::Xor{..} => Gate::Xor{a: wm[rg.ins[0]], b: wm[rg.ins[1]]},
            Gate::Not{..} => Gate::Not{a: wm[rg.ins[0]]},
        };
        wm[rg.out] = nw; nw += 1; gates.push(g);
    }
    let outputs: Vec<usize> = (total - n_out..total).map(|w| wm[w]).collect();
    Circuit { inputs, gates, outputs }
}

#[test]
fn sub64_bench() {
    let text = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/sub64.txt")).unwrap();
    let circuit = parse_bristol(&text);
    let wires = circuit.wires();
    let and = circuit.gates.iter().filter(|g| matches!(g, Gate::And{..})).count();
    let xor = circuit.gates.iter().filter(|g| matches!(g, Gate::Xor{..})).count();
    let inv = circuit.gates.iter().filter(|g| matches!(g, Gate::Not{..})).count();
    let p = 4_611_686_018_427_387_919u64;
    let q = p.wrapping_mul(3);

    eprintln!("=== sub64: {} wires, {} AND, {} XOR, {} INV ===", wires, and, xor, inv);
    for variant in [Variant::Small, Variant::Fast] {
        let mut rng = TestRng(0xbeef);
        let prf = PrfKey([1,2,3,4]);
        let dd = LatticeDd::new(8, p, q, &mut rng).unwrap();
        let pis: Vec<bool> = (0..wires).map(|_| (rng.next_u32()&1)==1).collect();
        let prg_bits: Vec<bool> = (0..4*and).map(|_| (rng.next_u32()&1)==1).collect();

        let t0 = Instant::now();
        let g = garble_circuit(variant, &circuit, &dd, &pis, &prf, &prg_bits, &mut rng).unwrap();
        let garble_ms = t0.elapsed().as_secs_f64() * 1000.0;

        let inputs: Vec<bool> = (0..128).map(|_| (rng.next_u32()&1)==1).collect();
        let encoded = encode_inputs(&g, &Delta{coeffs: dd.delta().clone(), p}, &inputs, circuit.inputs).unwrap();
        let t1 = Instant::now();
        let masked = evaluate_circuit(variant, &circuit, &dd, &g.delta_ct, &g.and_rows, &g.records, &prf, &encoded).unwrap();
        let eval_ms = t1.elapsed().as_secs_f64() * 1000.0;

        let bits: usize = g.records.iter().map(GateRecord::bits).sum();
        // Records are one per charged gate: small = AND+XOR, fast = AND only.
        let charged = match variant {
            Variant::Small => and + xor,
            Variant::Fast => and,
            Variant::Tiny => and,
        };
        eprintln!(
            "{:?}: garble {:.2}ms, eval {:.2}ms, comm {} bits ({} B), charged-gates {}",
            variant, garble_ms, eval_ms, bits, bits/8, charged
        );
        let _ = masked;
    }
}
