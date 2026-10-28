//! H7: native hash throughput (the executor-side cost) for the per-target cost
//! table in `docs/hash-cipher-generic-plan.md`. This is a std integration test
//! (the spec lib is no_std).

use volar_spec::{ascon::ascon_hash256, blake2s::blake2s_256, keccak::spec_sha3_256};

#[test]
fn native_hash_speed() {
    let input = [0xabu8; 64];
    let n = 20_000;
    let t0 = std::time::Instant::now();
    for _ in 0..n {
        core::hint::black_box(ascon_hash256(&input));
    }
    let ascon_ns = t0.elapsed().as_nanos() as f64 / n as f64;
    let t0 = std::time::Instant::now();
    for _ in 0..n {
        core::hint::black_box(blake2s_256(&input));
    }
    let blake2s_ns = t0.elapsed().as_nanos() as f64 / n as f64;
    let t0 = std::time::Instant::now();
    for _ in 0..n {
        core::hint::black_box(spec_sha3_256(&input));
    }
    let keccak_ns = t0.elapsed().as_nanos() as f64 / n as f64;
    // NOTE: these are the *spec-dialect* (portable, unoptimized) forms, not the
    // optimized RustCrypto/asm implementations — the relative number is the
    // in-executor signal; the absolute is pessimistic.
    eprintln!(
        "[NATIVE] per-64B-hash (spec-dialect, unoptimized): ascon={ascon_ns:.0}ns blake2s={blake2s_ns:.0}ns keccak={keccak_ns:.0}ns"
    );
}
