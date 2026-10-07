//! Validate the AES-128-GCM boolar gadget (`build_aes128_gcm`, P4c-i) against
//! the NIST SP 800-38D test vectors, with a scalar reference (built from
//! `volar_spec::faest::aes::encrypt_block`) anchoring the field conventions.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir::boolar::BIrBlocks;
use volar_spec::faest::aes;
use volar_vc::aes_gadget::build_aes128_gcm;

/// Scalar GCM multiply (SP 800-38D Algorithm 1), bytes MSB-first per the spec.
fn gcm_mul(x: &[u8; 16], y: &[u8; 16]) -> [u8; 16] {
    let mut z = [0u8; 16];
    let mut v = *x;
    for i in 0..128 {
        let bit = (y[i / 8] >> (7 - (i % 8))) & 1;
        if bit == 1 {
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

/// Scalar AES-128-GCM encrypt+tag for a 96-bit IV (the TLS form).
fn gcm_encrypt(key: &[u8; 16], iv: &[u8; 12], aad: &[u8], pt: &[u8]) -> (Vec<u8>, [u8; 16]) {
    assert_eq!(pt.len() % 16, 0, "gadget geometry is block-aligned");
    assert_eq!(aad.len() % 16, 0);
    let h = aes::encrypt_block(key, &[0u8; 16]);
    let mut j0 = [0u8; 16];
    j0[..12].copy_from_slice(iv);
    j0[15] = 1;
    let mut ct = Vec::with_capacity(pt.len());
    for (i, block) in pt.chunks(16).enumerate() {
        let mut ctr = j0;
        ctr[12..16].copy_from_slice(&((i as u32 + 2).to_be_bytes()));
        let ks = aes::encrypt_block(key, &ctr);
        for j in 0..16 {
            ct.push(block[j] ^ ks[j]);
        }
    }
    let mut x = [0u8; 16];
    let mut absorb = |x: &mut [u8; 16], block: &[u8]| {
        let mut xb = *x;
        for j in 0..16 {
            xb[j] ^= block[j];
        }
        *x = gcm_mul(&xb, &h);
    };
    for block in aad.chunks(16) {
        absorb(&mut x, block);
    }
    for block in ct.chunks(16) {
        absorb(&mut x, block);
    }
    let mut len_block = [0u8; 16];
    len_block[..8].copy_from_slice(&((aad.len() * 8) as u64).to_be_bytes());
    len_block[8..].copy_from_slice(&((pt.len() * 8) as u64).to_be_bytes());
    absorb(&mut x, &len_block);
    let s = aes::encrypt_block(key, &j0);
    let mut tag = [0u8; 16];
    for j in 0..16 {
        tag[j] = x[j] ^ s[j];
    }
    (ct, tag)
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
        .collect()
}

/// LSB-first-per-byte bit packing, matching the gadget's param layout.
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

fn circuit_gcm(
    circuit: &BIrBlocks,
    key: &[u8; 16],
    iv: &[u8; 12],
    aad: &[u8],
    pt: &[u8],
) -> (Vec<u8>, [u8; 16]) {
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(iv));
    inputs.extend(bits_of(aad));
    inputs.extend(bits_of(pt));
    let out = eval_biir(circuit, &inputs).expect("circuit evaluates");
    let ct_bits = pt.len() * 8;
    let ct = bytes_of(&out[..ct_bits]);
    let mut tag = [0u8; 16];
    tag.copy_from_slice(&bytes_of(&out[ct_bits..ct_bits + 128]));
    (ct, tag)
}

#[test]
fn scalar_gcm_matches_nist_vectors() {
    // SP 800-38D F.5.1 / CAVP case 1: K=0, IV=0, empty P/A.
    let (ct, tag) = gcm_encrypt(&[0; 16], &[0; 12], &[], &[]);
    assert!(ct.is_empty());
    assert_eq!(tag.to_vec(), hex("58e2fccefa7e3061367f1d57a4e7455a"));
    // Case 2: K=0, IV=0, one zero block.
    let (ct, tag) = gcm_encrypt(&[0; 16], &[0; 12], &[], &[0; 16]);
    assert_eq!(ct, hex("0388dace60b6a392f328c2b971b2fe78"));
    assert_eq!(tag.to_vec(), hex("ab6e47d42cec13bdf53a67b21257bddf"));
}

#[test]
fn gcm_gadget_matches_nist_and_scalar() {
    // Geometry (0 AAD blocks, 0 PT blocks): tag-only, NIST case 1.
    let c10 = build_aes128_gcm(0, 0);
    let (ct, tag) = circuit_gcm(&c10, &[0; 16], &[0; 12], &[], &[]);
    assert!(ct.is_empty());
    assert_eq!(
        tag.to_vec(),
        hex("58e2fccefa7e3061367f1d57a4e7455a"),
        "circuit case 1"
    );

    // Geometry (0, 1): NIST case 2.
    let c01 = build_aes128_gcm(0, 1);
    let (ct, tag) = circuit_gcm(&c01, &[0; 16], &[0; 12], &[], &[0; 16]);
    assert_eq!(
        ct,
        hex("0388dace60b6a392f328c2b971b2fe78"),
        "circuit case 2 ct"
    );
    assert_eq!(
        tag.to_vec(),
        hex("ab6e47d42cec13bdf53a67b21257bddf"),
        "circuit case 2 tag"
    );

    // Multi-block + AAD: cross-check the circuit against the anchored scalar
    // reference on non-trivial inputs (including the classic feffe992 key).
    let c24 = build_aes128_gcm(2, 4);
    let key: [u8; 16] = hex("feffe9928665731c6d6a8f9467308308").try_into().unwrap();
    let iv: [u8; 12] = hex("cafebabefacedbaddecaf888").try_into().unwrap();
    let aad = hex("feedfacedeadbeeffeedfacedeadbeefabaddad2000000000000000000000000");
    // The classic 60-byte plaintext, zero-padded to the gadget's 4-block
    // block-aligned geometry (the published ct/tag don't apply; the
    // circuit is cross-checked against the anchored scalar reference).
    let mut pt = hex(
        "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a72\
         1c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39",
    );
    pt.resize(64, 0);
    let (ect, etag) = gcm_encrypt(&key, &iv, &aad, &pt);
    let (cct, ctag) = circuit_gcm(&c24, &key, &iv, &aad, &pt);
    assert_eq!(cct, ect, "circuit vs scalar ct");
    assert_eq!(ctag, etag, "circuit vs scalar tag");
}
