// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Exploratory CPU/working-set probe for ML-KEM base OT → IKNP/SoftSpoken → Ferret.
//! Deterministic `SeedRng` is for repeatable benchmarking only, never deployment.

use std::hint::black_box;
use std::time::Instant;

use sha2::Sha256;
use volar_mpc::ot::SeedRng;
use volar_mpc::ot_mlkem::{CT_BYTES, EK_BYTES, MlKem1024BaseOt};
use volar_spec::ot::base_ot::BaseOt;
use volar_spec::ot::ferret::{FERRET_REG_MAIN, FERRET_REG_TOY, FerretParams};
use volar_spec::ot::stack::OtStack;

const BASE_OT_COUNT: usize = 128;
const LABEL_BYTES: usize = 16;

fn benchmark_base_ots(rounds: usize) {
    let m0 = [0x35; LABEL_BYTES];
    let m1 = [0xCA; LABEL_BYTES];
    let mut times_ms = Vec::with_capacity(rounds);
    for run in 0..rounds {
        let mut rng = SeedRng::new(0xAABB_CCDD + run as u64);
        let start = Instant::now();
        for i in 0..BASE_OT_COUNT {
            let choice = i % 2 == 1;
            let (sender_state, setup) = MlKem1024BaseOt::sender_setup(&mut rng);
            let (receiver_state, recv_msg) = MlKem1024BaseOt::recv_start(&mut rng, &setup, choice);
            let payload =
                MlKem1024BaseOt::sender_payload(&mut rng, &sender_state, &recv_msg, &m0, &m1);
            let got = MlKem1024BaseOt::recv_finish(&receiver_state, &payload);
            assert_eq!(got, if choice { m1 } else { m0 });
            black_box(got);
        }
        times_ms.push(start.elapsed().as_secs_f64() * 1_000.0);
    }
    println!("128-base-OT-only_ms={times_ms:?}");
}

fn probe(params: FerretParams, rounds: usize, total: usize, chunk: usize) {
    println!(
        "profile n={} k={} t={} seed_cots={} output_cots/refill={} requested_cots={}",
        params.n,
        params.k,
        params.t,
        params.seed_cot_count(false),
        params.output_cot_count(false),
        total,
    );
    println!(
        "ML-KEM-1024 base-OT payload traffic (no framing): receiver→sender={} B, sender→receiver={} B, total={} B",
        BASE_OT_COUNT * 2 * EK_BYTES,
        BASE_OT_COUNT * 2 * (CT_BYTES + LABEL_BYTES),
        BASE_OT_COUNT * (2 * EK_BYTES + 2 * (CT_BYTES + LABEL_BYTES)),
    );

    benchmark_base_ots(rounds);
    let mut setup_ms = Vec::with_capacity(rounds);
    let mut drain_ms = Vec::with_capacity(rounds);
    for run in 0..rounds {
        let mut rng_s = SeedRng::new(0x1122_3344 + run as u64);
        let mut rng_r = SeedRng::new(0x5566_7788 + run as u64);
        let start = Instant::now();
        let mut stack =
            OtStack::setup_with_base::<Sha256, _, MlKem1024BaseOt>(&mut rng_s, &mut rng_r, params);
        setup_ms.push(start.elapsed().as_secs_f64() * 1_000.0);

        let start = Instant::now();
        let mut completed = 0;
        while completed < total {
            let count = chunk.min(total - completed);
            let choices: Vec<bool> = (0..count).map(|i| (i + completed) % 2 == 1).collect();
            black_box(stack.commit_bits(&mut rng_s, &choices));
            completed += count;
        }
        drain_ms.push(start.elapsed().as_secs_f64() * 1_000.0);
    }
    println!("setup_ms={setup_ms:?}");
    println!("chosen-COT→VOLE drain/refill_ms={drain_ms:?}");
    println!(
        "Note: in-process only; excludes transport/IPC and uses deterministic RNG. Toy params are correctness-only; main params are legacy classical Ferret sizing, not a post-quantum security claim."
    );
}

fn main() {
    let full = std::env::args().skip(1).any(|arg| arg == "--main");
    if full {
        let params = FERRET_REG_MAIN;
        probe(params, 1, params.output_cot_count(false), 100_000);
    } else {
        let params = FERRET_REG_TOY;
        probe(params, 5, params.output_cot_count(false) * 5, 100);
    }
}
