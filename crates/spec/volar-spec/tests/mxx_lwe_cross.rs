// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! Cross-test for Phase 1 of `docs/mxx-cutover-plan.md`: the mxx-generated
//! spec element `mxx_lwe::mxx_lwe_sample` must agree with the hand-written
//! generic `lwe::LweSample::sample` on shared deterministic vectors
//! (plan §4 criterion 3: semantics match on a deterministic cross-test).

use core::ops::{Add, Mul};

use hybrid_array::typenum::{U3, U4};
use volar_spec::lwe::LweSample;
use volar_spec::{Array, mxx_lwe};

/// The toy ring modulus of the generated element (`Z_257`, ring dim 1).
const Q: u64 = 257;

/// Wrapper making `u64 mod Q` usable as the generic `LweSample` element type.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct ModQ(u64);

impl Add for ModQ {
    type Output = ModQ;

    fn add(self, other: ModQ) -> ModQ {
        ModQ((self.0 + other.0) % Q)
    }
}

impl Mul for ModQ {
    type Output = ModQ;

    fn mul(self, other: ModQ) -> ModQ {
        ModQ((self.0 * other.0) % Q)
    }
}

/// Deterministic toy value stream (LCG).
fn toy(i: u64) -> u64 {
    let mut x = i.wrapping_add(0x9e37_79b9_7f4a_7c15);
    x = x.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
    (x >> 33) % Q
}

#[test]
fn mxx_lwe_matches_generic_lwe_sample() {
    let matrix: Array<Array<ModQ, U3>, U4> =
        Array::from_fn(|i| Array::<ModQ, U3>::from_fn(|j| ModQ(toy((i * 3 + j) as u64))));
    let secret: Array<ModQ, U3> = Array::from_fn(|j| ModQ(toy(100 + j as u64)));
    let noise: Array<ModQ, U4> = Array::from_fn(|i| ModQ(toy(200 + i as u64)));

    // Hand-written generic reference.
    let generic = LweSample::<ModQ, ModQ, U3, U4>::sample(matrix.clone(), secret.clone(), noise.clone());

    // mxx-generated concrete element. Entry shape is `[[x]]`: the innermost
    // `[u64; 1]` is the (dimension-1) polynomial, the next level the single
    // CRT tower.
    let a_arr: [[[[u64; 1]; 1]; 3]; 4] =
        core::array::from_fn(|i| core::array::from_fn(|j| [[matrix[i][j].0]]));
    let s_arr: [[[[u64; 1]; 1]; 1]; 3] = core::array::from_fn(|j| [[[secret[j].0]]]);
    let e_arr: [[[[u64; 1]; 1]; 1]; 4] = core::array::from_fn(|i| [[[noise[i].0]]]);
    let got = mxx_lwe::mxx_lwe_sample(a_arr, s_arr, e_arr);

    for i in 0..4 {
        assert_eq!(
            got[i][0][0][0],
            generic.b[i].0,
            "row {i}: mxx-generated element disagrees with the generic LWE sample"
        );
    }
}
