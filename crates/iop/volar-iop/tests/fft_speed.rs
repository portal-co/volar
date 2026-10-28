//! S4 prover-speedup measurement: the naive Horner RS encode vs the additive
//! FFT. This is a std integration test (volar-iop is `no_std` even in unit
//! tests, so the timing/`eprintln` lives here).

use volar_iop::additive_fft::additive_fft_evaluate;
use volar_iop::field::Field;
use volar_primitives::Galois128;

fn naive_evaluate<F: Field>(p: &[F], n: usize) -> Vec<F> {
    (0..n)
        .map(|j| {
            let point = F::from_u128(j as u128);
            let mut acc = F::ZERO;
            for i in (0..p.len()).rev() {
                acc = acc.mul(&point).add(&p[i]);
            }
            acc
        })
        .collect()
}

#[test]
fn additive_fft_speedup() {
    let now = std::time::Instant::now;
    for log_n in [8usize, 10, 12] {
        let n = 1usize << log_n;
        let msg: Vec<Galois128> = (0..n / 4)
            .map(|i| Galois128(0x9e3779b9u128.wrapping_mul(i as u128 + 1)))
            .collect();
        let t0 = now();
        let naive = naive_evaluate(&msg, n);
        let naive_t = t0.elapsed();
        let t0 = now();
        let fft = additive_fft_evaluate(&msg, n);
        let fft_t = t0.elapsed();
        assert_eq!(fft, naive);
        println!(
            "S4: n=2^{log_n} k=2^{} naive={naive_t:?} additive_fft={fft_t:?} speedup={:.1}x",
            log_n - 2,
            naive_t.as_secs_f64() / fft_t.as_secs_f64().max(1e-9)
        );
    }
}
