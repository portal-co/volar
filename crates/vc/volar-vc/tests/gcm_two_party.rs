//! P4c-i two-party check: an AES-128-GCM record encryption runs through the
//! garbled session with the **key garbler-private** and the IV + plaintext
//! **evaluator-private** (OT-delivered) — neither party learns the other's
//! input, yet the evaluator comes away holding the ciphertext + tag. This is
//! the core MPC-TLS record-layer mechanism (key shares + 2PC AEAD) that the
//! rustls `CryptoProvider` seam (P4c-ii) will delegate to.

use hybrid_array::Array;
use volar_ir::boolar::BIrBlocks;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::aes_gadget::build_aes128_gcm;
use volar_vc::{VcEmbedder, VcOutcome};

type N = typenum::U16;
type D = sha2::Sha256;

const I: usize = 352; // key 128 ++ iv 96 ++ pt 128
const A: usize = 80768; // probed via compile_schedule

fn det_bytes(seed: u8) -> Array<u8, N> {
    Array::clone_from_slice(&[seed; 16])
}

fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: Array::clone_from_slice(&[seed.wrapping_mul(31); 16]),
    }
}

fn bits_of(bytes: &[u8]) -> Vec<bool> {
    bytes
        .iter()
        .flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1))
        .collect()
}

fn bytes_of(bits: &[bool]) -> Vec<u8> {
    bits.chunks(8)
        .map(|c| {
            c.iter()
                .enumerate()
                .fold(0u8, |a, (j, &b)| a | ((b as u8) << j))
        })
        .collect()
}

fn two_party_gcm(key: &[u8; 16], iv: &[u8; 12], pt: &[u8; 16]) -> ([u8; 16], [u8; 16]) {
    let circuit: BIrBlocks = build_aes128_gcm(0, 1);
    // Partition: key bits garbler-owned (server share), iv + plaintext
    // evaluator-owned (client side, OT-delivered).
    let mut partition = vec![InputOwner::Garbler; 128];
    partition.extend(std::iter::repeat(InputOwner::Evaluator).take(96 + 128));
    let inputs: Vec<bool> = [bits_of(key), bits_of(iv), bits_of(pt)].concat();

    let secret = GlobalSecret::<N>::new(det_bytes(7));
    let labels: [Garble<N>; I] = core::array::from_fn(|i| det_label(i as u8));
    let embedder = VcEmbedder::<N, I, A>::with_secret(secret, labels);
    let schedule = VcEmbedder::<N, I, A>::compile(&circuit).expect("gcm circuit schedules");

    let mut public = Vec::new();
    let mut garbler = Vec::new();
    let mut evaluator = Vec::new();
    for (i, &bit) in inputs.iter().enumerate() {
        match partition[i] {
            InputOwner::Public => public.push(bit),
            InputOwner::Garbler => garbler.push(bit),
            InputOwner::Evaluator => evaluator.push(bit),
        }
    }
    let mut ot = LoopbackOt::<N>::new();
    let out = match embedder.invoke_schedule::<D>(
        &schedule, &partition, &public, &garbler, &evaluator, &mut ot,
    ) {
        VcOutcome::Value(bits) => bits,
        other => panic!("two-party gcm aborted: {other:?}"),
    };
    assert_eq!(out.len(), 256);
    let mut ct = [0u8; 16];
    ct.copy_from_slice(&bytes_of(&out[..128]));
    let mut tag = [0u8; 16];
    tag.copy_from_slice(&bytes_of(&out[128..]));
    (ct, tag)
}

/// Scalar GCM (the anchored reference from the gadget test), 1-block geometry.
fn scalar_gcm_1block(key: &[u8; 16], iv: &[u8; 12], pt: &[u8; 16]) -> ([u8; 16], [u8; 16]) {
    use volar_spec::faest::aes;
    fn gcm_mul(x: &[u8; 16], y: &[u8; 16]) -> [u8; 16] {
        let mut z = [0u8; 16];
        let mut v = *x;
        for i in 0..128 {
            if (y[i / 8] >> (7 - (i % 8))) & 1 == 1 {
                for j in 0..16 {
                    z[j] ^= v[j];
                }
            }
            let carry = v[15] & 1;
            for j in (1..16).rev() {
                v[j] = (v[j] >> 1) | ((v[j - 1] & 1) << 7);
            }
            v[0] >>= 1;
            if carry == 1 {
                v[0] ^= 0xe1;
            }
        }
        z
    }
    let h = aes::encrypt_block(key, &[0u8; 16]);
    let mut j0 = [0u8; 16];
    j0[..12].copy_from_slice(iv);
    j0[15] = 1;
    let mut ctr = j0;
    ctr[12..16].copy_from_slice(&2u32.to_be_bytes());
    let ks = aes::encrypt_block(key, &ctr);
    let mut ct = [0u8; 16];
    for j in 0..16 {
        ct[j] = pt[j] ^ ks[j];
    }
    let mut x = gcm_mul(&ct, &h);
    let mut len_block = [0u8; 16];
    len_block[8..].copy_from_slice(&128u64.to_be_bytes());
    for j in 0..16 {
        x[j] ^= len_block[j];
    }
    x = gcm_mul(&x, &h);
    let s = aes::encrypt_block(key, &j0);
    let mut tag = [0u8; 16];
    for j in 0..16 {
        tag[j] = x[j] ^ s[j];
    }
    (ct, tag)
}

fn body() {
    // NIST case 2 through the two-party session.
    let (ct, tag) = two_party_gcm(&[0; 16], &[0; 12], &[0; 16]);
    assert_eq!(
        ct.to_vec(),
        (0..16)
            .map(|i| [
                3u8, 136, 218, 206, 96, 182, 163, 146, 243, 40, 194, 185, 113, 178, 254, 120
            ][i])
            .collect::<Vec<_>>(),
        "2pc NIST case 2 ct"
    );
    assert_eq!(
        tag.to_vec(),
        vec![
            171, 110, 71, 212, 44, 236, 19, 189, 245, 58, 103, 178, 18, 87, 189, 223
        ],
        "2pc NIST case 2 tag"
    );

    // Non-trivial key/plaintext: cross-check against the scalar reference.
    let mut key = [0u8; 16];
    key.copy_from_slice(&[
        0xfe, 0xff, 0xe9, 0x92, 0x86, 0x65, 0x73, 0x1c, 0x6d, 0x6a, 0x8f, 0x94, 0x67, 0x30, 0x83,
        0x08,
    ]);
    let mut iv = [0u8; 12];
    iv.copy_from_slice(&[
        0xca, 0xfe, 0xba, 0xbe, 0xfa, 0xce, 0xdb, 0xad, 0xde, 0xca, 0xf8, 0x88,
    ]);
    let mut pt = [0u8; 16];
    pt.copy_from_slice(&[
        0xd9, 0x31, 0x32, 0x25, 0xf8, 0x84, 0x06, 0xe5, 0xa5, 0x59, 0x09, 0xc5, 0xaf, 0xf5, 0x26,
        0x9a,
    ]);
    let (ct2, tag2) = two_party_gcm(&key, &iv, &pt);
    let (ect2, etag2) = scalar_gcm_1block(&key, &iv, &pt);
    assert_eq!(ct2, ect2, "2pc vs scalar ct");
    assert_eq!(tag2, etag2, "2pc vs scalar tag");
}

#[test]
fn p4c_two_party_gcm_record() {
    // ~80k garbled tables: far past the default 2 MiB test-thread stack.
    std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(body)
        .unwrap()
        .join()
        .unwrap();
}
