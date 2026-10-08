//! Focused SWHE probe: encrypt/decrypt/add/mul/switch at each level.
#![cfg(test)]

use volar_spec::bitgc::swhe::{Context, NoiseSource, Parameters, RandomSource, ZeroNoise};

struct R(u64);
impl RandomSource for R {
    fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), volar_spec::bitgc::swhe::Error> {
        for b in output.iter_mut() {
            self.0 = self.0.wrapping_mul(6364136223846793005).wrapping_add(1);
            *b = (self.0 >> 33) as u8;
        }
        Ok(())
    }
}

struct B(u64);
impl NoiseSource for B {
    fn sample(&mut self, bound: u64, output: &mut [i64]) -> Result<(), volar_spec::bitgc::swhe::Error> {
        for v in output.iter_mut() {
            self.0 = self.0.wrapping_mul(2862933555777941757).wrapping_add(3037000493);
            let span = (2 * bound + 1) as i64;
            *v = ((self.0 >> 33) % span as u64) as i64 - bound as i64;
        }
        Ok(())
    }
}

fn ctx() -> Context {
    Context::new(Parameters {
        degree: 8,
        plaintext_modulus: 97,
        chain: vec![
            1_099_511_645_201,
            1_099_511_673_137,
            1_099_511_677_793,
            1_099_511_704_177,
        ],
        noise_bound: 2,
    })
    .unwrap()
}

#[test]
fn probe_encrypt_decrypt_all_levels() {
    let ctx = ctx();
    let mut r = R(42);
    let sk = ctx.keygen(&mut r).unwrap();
    let msg = [3u64, 0, 1, 0, 2, 0, 0, 0];
    for level in 0..=ctx.max_level() {
        let ct = ctx.encrypt(&sk, level, &msg, &mut r, &mut B(9)).unwrap();
        let got = ctx.decrypt(&sk, &ct).unwrap();
        assert_eq!(&got[..3], &msg[..3], "level {level} roundtrip");
    }
}

#[test]
fn probe_add_mul_switch() {
    let ctx = ctx();
    let mut r = R(7);
    let sk = ctx.keygen(&mut r).unwrap();
    let ksk = ctx.key_switch_keyset(&sk, 1 << 10, 7, &mut r, &mut ZeroNoise).unwrap();
    let top = ctx.max_level();
    let a = [2u64; 8];
    let b = [5u64; 8];
    let ca = ctx.encrypt(&sk, top, &a, &mut r, &mut B(1)).unwrap();
    let cb = ctx.encrypt(&sk, top, &b, &mut r, &mut B(2)).unwrap();

    let sum = ctx.add(&ca, &cb).unwrap();
    assert_eq!(ctx.decrypt(&sk, &sum).unwrap()[0], 7, "add");

    let prod = ctx.mul(&ca, &cb, &ksk).unwrap();
    assert_eq!(prod.level(), top - 1);
    assert_eq!(ctx.decrypt(&sk, &prod).unwrap()[0], 10, "mul");

    let down = ctx.mod_switch_down(&ca).unwrap();
    assert_eq!(ctx.decrypt(&sk, &down).unwrap()[0], 2, "switch preserves message");

    let cb2 = ctx.switch_to(&cb, prod.level()).unwrap();
    let prod2 = ctx.mul(&prod, &cb2, &ksk).unwrap();
    assert_eq!(ctx.decrypt(&sk, &prod2).unwrap()[0], 50, "chained mul");
}
