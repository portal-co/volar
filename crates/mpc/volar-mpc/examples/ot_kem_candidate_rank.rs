// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Compare the three ML-KEM parameter sets at the same experimental KEM-OT
//! candidate seam. Deterministic RNG is for benchmarking only.

use core::marker::PhantomData;
use std::hint::black_box;
use std::time::Instant;

use digest::Digest;
use ml_kem::{
    Decapsulate, DecapsulationKey512, DecapsulationKey768, DecapsulationKey1024, Encapsulate,
    EncapsulationKey512, EncapsulationKey768, EncapsulationKey1024, Generate, KeyExport,
    KeySizeUser, SharedKey,
};
use sha2::Sha256;
use volar_mpc::ot::SeedRng;
use volar_mpc::ot_mlkem::SpecRngAsCrypto;
use volar_spec::SpecRng;
use volar_spec::ot::base_ot::BaseOt;
use volar_spec::ot::ferret::{FERRET_REG_MAIN, FERRET_REG_TOY, FerretParams};
use volar_spec::ot::stack::OtStack;

const LABEL_BYTES: usize = 16;
const BASE_OT_COUNT: usize = 128;
const ROUNDS: usize = 5;

trait KemProfile {
    type DecapsulationKey;
    type EncapsulationKey;

    const NAME: &'static str;
    const K: usize;
    const EK_BYTES: usize;
    const CT_BYTES: usize;

    fn generate(rng: &mut dyn SpecRng) -> (Self::DecapsulationKey, Self::EncapsulationKey);
    fn parse_ek(bytes: &[u8]) -> Option<Self::EncapsulationKey>;
    fn ek_bytes(key: &Self::EncapsulationKey) -> Vec<u8>;
    fn encapsulate(rng: &mut dyn SpecRng, key: &Self::EncapsulationKey) -> (Vec<u8>, [u8; 32]);
    fn decapsulate(key: &Self::DecapsulationKey, ciphertext: &[u8]) -> Option<[u8; 32]>;
}

struct MlKem512;
struct MlKem768;
struct MlKem1024;

macro_rules! impl_kem_profile {
    ($profile:ty, $name:literal, $k:expr, $ek_bytes:expr, $ct_bytes:expr,
        $dk:ty, $ek:ty, $ct:ty) => {
        impl KemProfile for $profile {
            type DecapsulationKey = $dk;
            type EncapsulationKey = $ek;

            const NAME: &'static str = $name;
            const K: usize = $k;
            const EK_BYTES: usize = $ek_bytes;
            const CT_BYTES: usize = $ct_bytes;

            fn generate(rng: &mut dyn SpecRng) -> (Self::DecapsulationKey, Self::EncapsulationKey) {
                let mut crypto_rng = SpecRngAsCrypto(rng);
                let decapsulation_key = <$dk>::generate_from_rng(&mut crypto_rng);
                let encapsulation_key = decapsulation_key.encapsulation_key().clone();
                (decapsulation_key, encapsulation_key)
            }

            fn parse_ek(bytes: &[u8]) -> Option<Self::EncapsulationKey> {
                if bytes.len() != Self::EK_BYTES {
                    return None;
                }
                let encoded: &ml_kem::array::Array<u8, <$ek as KeySizeUser>::KeySize> =
                    bytes.try_into().ok()?;
                <$ek>::new(encoded).ok()
            }

            fn ek_bytes(key: &Self::EncapsulationKey) -> Vec<u8> {
                key.to_bytes().as_slice().to_vec()
            }

            fn encapsulate(
                rng: &mut dyn SpecRng,
                key: &Self::EncapsulationKey,
            ) -> (Vec<u8>, [u8; 32]) {
                let mut crypto_rng = SpecRngAsCrypto(rng);
                let (ciphertext, shared_key) = key.encapsulate_with_rng(&mut crypto_rng);
                (ciphertext.as_slice().to_vec(), shared_key_bytes(shared_key))
            }

            fn decapsulate(key: &Self::DecapsulationKey, ciphertext: &[u8]) -> Option<[u8; 32]> {
                if ciphertext.len() != Self::CT_BYTES {
                    return None;
                }
                let ciphertext: $ct = ciphertext.try_into().ok()?;
                Some(shared_key_bytes(key.decapsulate(&ciphertext)))
            }
        }
    };
}

impl_kem_profile!(
    MlKem512,
    "ML-KEM-512",
    2,
    800,
    768,
    DecapsulationKey512,
    EncapsulationKey512,
    ml_kem::ml_kem_512::Ciphertext
);
impl_kem_profile!(
    MlKem768,
    "ML-KEM-768",
    3,
    1184,
    1088,
    DecapsulationKey768,
    EncapsulationKey768,
    ml_kem::ml_kem_768::Ciphertext
);
impl_kem_profile!(
    MlKem1024,
    "ML-KEM-1024",
    4,
    1568,
    1568,
    DecapsulationKey1024,
    EncapsulationKey1024,
    ml_kem::ml_kem_1024::Ciphertext
);

fn shared_key_bytes(key: SharedKey) -> [u8; 32] {
    core::array::from_fn(|i| key[i])
}

fn sample_fake_ek<P: KemProfile>(rng: &mut dyn SpecRng) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(P::EK_BYTES);
    // t_hat contains k polynomials, each 256 canonical 12-bit coefficients;
    // rho is an independent 32-byte seed.
    for _ in 0..(P::K * 128) {
        let mut coefficient = || loop {
            let value = (rng.next_u32() & 0x0fff) as u16;
            if value < 3329 {
                break value;
            }
        };
        let a = coefficient();
        let b = coefficient();
        bytes.push(a as u8);
        bytes.push(((a >> 8) as u8 & 0x0f) | ((b as u8 & 0x0f) << 4));
        bytes.push((b >> 4) as u8);
    }
    for _ in 0..32 {
        bytes.push(rng.next_u32() as u8);
    }
    assert_eq!(bytes.len(), P::EK_BYTES);
    bytes
}

fn kdf_expand(key: &[u8; 32], n: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(n);
    let mut counter = 0u32;
    while out.len() < n {
        let mut hash = Sha256::new();
        hash.update(key);
        hash.update(counter.to_le_bytes());
        out.extend_from_slice(&hash.finalize());
        counter += 1;
    }
    out.truncate(n);
    out
}

struct ProfileReceiver<P: KemProfile> {
    decapsulation_key: P::DecapsulationKey,
    choice: bool,
}

fn receiver_start<P: KemProfile>(
    rng: &mut dyn SpecRng,
    choice: bool,
) -> (ProfileReceiver<P>, Vec<u8>) {
    let (decapsulation_key, real_key) = P::generate(rng);
    let fake_key = P::parse_ek(&sample_fake_ek::<P>(rng)).expect("fake key encoding is valid");
    let (key0, key1) = if choice {
        (P::ek_bytes(&fake_key), P::ek_bytes(&real_key))
    } else {
        (P::ek_bytes(&real_key), P::ek_bytes(&fake_key))
    };
    let mut message = Vec::with_capacity(2 * P::EK_BYTES);
    message.extend_from_slice(&key0);
    message.extend_from_slice(&key1);
    (
        ProfileReceiver {
            decapsulation_key,
            choice,
        },
        message,
    )
}

fn sender_payload<P: KemProfile>(
    rng: &mut dyn SpecRng,
    receiver_message: &[u8],
    m0: &[u8; LABEL_BYTES],
    m1: &[u8; LABEL_BYTES],
) -> Vec<u8> {
    assert_eq!(receiver_message.len(), 2 * P::EK_BYTES);
    let labels = [m0, m1];
    let mut payload = Vec::with_capacity(2 * (P::CT_BYTES + LABEL_BYTES));
    for i in 0..2 {
        let start = i * P::EK_BYTES;
        let key = P::parse_ek(&receiver_message[start..start + P::EK_BYTES])
            .expect("receiver sent a valid public key");
        let (ciphertext, shared_key) = P::encapsulate(rng, &key);
        assert_eq!(ciphertext.len(), P::CT_BYTES);
        let mask = kdf_expand(&shared_key, LABEL_BYTES);
        payload.extend_from_slice(&ciphertext);
        payload.extend(labels[i].iter().zip(mask).map(|(label, mask)| label ^ mask));
    }
    payload
}

fn receiver_finish<P: KemProfile>(
    receiver: &ProfileReceiver<P>,
    payload: &[u8],
) -> [u8; LABEL_BYTES] {
    let half = P::CT_BYTES + LABEL_BYTES;
    assert_eq!(payload.len(), 2 * half);
    let offset = usize::from(receiver.choice) * half;
    let shared_key = P::decapsulate(
        &receiver.decapsulation_key,
        &payload[offset..offset + P::CT_BYTES],
    )
    .expect("ciphertext size matches profile");
    let mask = kdf_expand(&shared_key, LABEL_BYTES);
    core::array::from_fn(|i| payload[offset + P::CT_BYTES + i] ^ mask[i])
}

/// Benchmark-only `BaseOt` plugin: each parameter set uses the same existing
/// OtStack/CotSource boundary. This does not promote the experimental OT to a
/// reviewed or deployment-ready construction.
struct CandidateBaseOt<P>(PhantomData<P>);

impl<P: KemProfile> BaseOt<16> for CandidateBaseOt<P> {
    type SenderState = ();
    type ReceiverState = ProfileReceiver<P>;
    type SetupMsg = ();
    type RecvMsg = Vec<u8>;
    type PayloadMsg = Vec<u8>;

    fn sender_setup<R: SpecRng>(_rng: &mut R) -> (Self::SenderState, Self::SetupMsg) {
        ((), ())
    }

    fn recv_start<R: SpecRng>(
        rng: &mut R,
        _setup: &Self::SetupMsg,
        choice: bool,
    ) -> (Self::ReceiverState, Self::RecvMsg) {
        let (receiver, message) = receiver_start::<P>(rng, choice);
        (receiver, message)
    }

    fn sender_payload<R: SpecRng>(
        rng: &mut R,
        _state: &Self::SenderState,
        recv_msg: &Self::RecvMsg,
        m0: &[u8; 16],
        m1: &[u8; 16],
    ) -> Self::PayloadMsg {
        sender_payload::<P>(rng, recv_msg, m0, m1)
    }

    fn recv_finish(state: &Self::ReceiverState, payload: &Self::PayloadMsg) -> [u8; 16] {
        receiver_finish::<P>(state, payload)
    }
}

fn benchmark_base_ots<P: KemProfile>(rounds: usize) -> Vec<f64> {
    let m0 = [0x35; LABEL_BYTES];
    let m1 = [0xCA; LABEL_BYTES];
    let mut times_ms = Vec::with_capacity(rounds);
    for run in 0..rounds {
        let mut rng = SeedRng::new(0xAABB_CCDD + run as u64);
        let start = Instant::now();
        for i in 0..BASE_OT_COUNT {
            let choice = i % 2 == 1;
            let (sender_state, setup) = CandidateBaseOt::<P>::sender_setup(&mut rng);
            let (receiver_state, recv_msg) =
                CandidateBaseOt::<P>::recv_start(&mut rng, &setup, choice);
            let payload =
                CandidateBaseOt::<P>::sender_payload(&mut rng, &sender_state, &recv_msg, &m0, &m1);
            let got = CandidateBaseOt::<P>::recv_finish(&receiver_state, &payload);
            assert_eq!(got, if choice { m1 } else { m0 });
            black_box(got);
        }
        times_ms.push(start.elapsed().as_secs_f64() * 1000.0);
    }
    times_ms
}

fn benchmark_toy_stack<P: KemProfile>() -> (f64, f64) {
    let mut rng_s = SeedRng::new(0x1122_3344);
    let mut rng_r = SeedRng::new(0x5566_7788);
    let start = Instant::now();
    let mut stack = OtStack::setup_with_base::<Sha256, _, CandidateBaseOt<P>>(
        &mut rng_s,
        &mut rng_r,
        FERRET_REG_TOY,
    );
    let setup_ms = start.elapsed().as_secs_f64() * 1000.0;
    let bits: Vec<bool> = (0..1000).map(|i| i % 2 == 1).collect();
    let start = Instant::now();
    black_box(stack.commit_bits(&mut rng_s, &bits));
    let drain_ms = start.elapsed().as_secs_f64() * 1000.0;
    (setup_ms, drain_ms)
}

fn benchmark_main_stack<P: KemProfile>() -> (f64, f64) {
    let params: FerretParams = FERRET_REG_MAIN;
    let mut rng_s = SeedRng::new(0x1122_3344);
    let mut rng_r = SeedRng::new(0x5566_7788);
    let start = Instant::now();
    let mut stack =
        OtStack::setup_with_base::<Sha256, _, CandidateBaseOt<P>>(&mut rng_s, &mut rng_r, params);
    let setup_ms = start.elapsed().as_secs_f64() * 1000.0;
    let total = params.output_cot_count(false);
    let start = Instant::now();
    let mut completed = 0;
    while completed < total {
        let count = 100_000.min(total - completed);
        let choices: Vec<bool> = (0..count).map(|i| (i + completed) % 2 == 1).collect();
        black_box(stack.commit_bits(&mut rng_s, &choices));
        completed += count;
    }
    let drain_ms = start.elapsed().as_secs_f64() * 1000.0;
    (setup_ms, drain_ms)
}

fn report<P: KemProfile>(main_stack: bool) {
    let samples = benchmark_base_ots::<P>(ROUNDS);
    let mut sorted_samples = samples.clone();
    sorted_samples.sort_by(f64::total_cmp);
    let median_ms = sorted_samples[sorted_samples.len() / 2];
    let receiver_bytes = BASE_OT_COUNT * 2 * P::EK_BYTES;
    let sender_bytes = BASE_OT_COUNT * 2 * (P::CT_BYTES + LABEL_BYTES);
    println!(
        "{}: base-OT-128_ms={samples:?} median={median_ms:.3}; traffic={receiver_bytes}+{sender_bytes}={} B (receiver→sender + sender→receiver, excludes framing)",
        P::NAME,
        receiver_bytes + sender_bytes,
    );
    if main_stack {
        let (setup_ms, drain_ms) = benchmark_main_stack::<P>();
        let rows = FERRET_REG_MAIN.output_cot_count(false);
        println!(
            "{}: Ferret-Reg-main setup={setup_ms:.3} ms drain={drain_ms:.3} ms rows={rows} rows_per_s={:.0}",
            P::NAME,
            rows as f64 / (drain_ms / 1000.0),
        );
    } else {
        let (setup_ms, drain_ms) = benchmark_toy_stack::<P>();
        println!(
            "{}: toy-stack setup={setup_ms:.3} ms 1000-chosen-COT→VOLE={drain_ms:.3} ms",
            P::NAME,
        );
    }
}

fn report_selected(name: &str, main_stack: bool) {
    match name {
        "512" => report::<MlKem512>(main_stack),
        "768" => report::<MlKem768>(main_stack),
        "1024" => report::<MlKem1024>(main_stack),
        other => panic!("unknown ML-KEM profile {other}; choose 512, 768, or 1024"),
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let main_stack = args.iter().any(|arg| arg == "--main");
    let selected = args.iter().find_map(|arg| arg.strip_prefix("--only="));
    println!(
        "candidate benchmark: {} base OTs/run, {} in-process rounds, deterministic RNG; no security inference",
        BASE_OT_COUNT, ROUNDS
    );
    if let Some(selected) = selected {
        report_selected(selected, main_stack);
    } else {
        report::<MlKem512>(main_stack);
        report::<MlKem768>(main_stack);
        report::<MlKem1024>(main_stack);
    }
}
