//! Phase-one key-bearing driver coverage. The test proves the two driver
//! types can be constructed independently and expose matching public epochs
//! without sharing key material or state.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_spec::garble::GlobalSecret;
use volar_vc::oram_split::{
    EvaluatorEncryptedOramDriver, EvaluatorOramState, EvaluatorTreeKeyHalf,
    GarblerEncryptedOramDriver, GarblerOramState, GarblerTreeKeyHalf, SplitOramEvaluator,
    SplitOramGarbler,
};

type N = U16;
type D = Sha256;

#[test]
fn key_bearing_drivers_have_separate_role_surfaces() {
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[7; 16]));
    let garbler = SplitOramGarbler::new(secret, GarblerOramState::new(vec![], vec![], vec![]));
    let mut garbler = GarblerEncryptedOramDriver::new::<D>(
        garbler,
        GarblerTreeKeyHalf::new([true; 64]),
        b"garbler-local-seed",
    )
    .expect("garbler driver");
    let evaluator = SplitOramEvaluator::<N>::new(EvaluatorOramState::new(vec![], vec![], vec![]));
    let mut evaluator =
        EvaluatorEncryptedOramDriver::new(evaluator, EvaluatorTreeKeyHalf::new([false; 64]));

    assert_eq!(garbler.epoch(), 0);
    assert_eq!(evaluator.epoch(), 0);
    garbler.complete_access(0).expect("garbler epoch");
    evaluator.complete_access(0).expect("evaluator epoch");
    assert_eq!(garbler.epoch(), evaluator.epoch());
    // The public interface exposes only epoch progression. Key halves and
    // garbler input bases are intentionally not inspectable by this peer.
}

#[test]
fn epoch_cannot_be_advanced_twice() {
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[9; 16]));
    let driver = SplitOramGarbler::new(secret, GarblerOramState::new(vec![], vec![], vec![]));
    let mut driver =
        GarblerEncryptedOramDriver::new::<D>(driver, GarblerTreeKeyHalf::new([false; 64]), b"seed")
            .expect("driver");
    driver.complete_access(0).expect("first access");
    assert!(driver.complete_access(0).is_err());
}
