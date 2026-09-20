//! Byte-exact (non-block-multiple) AES-128-GCM circuits: NIST case 4
//! (60-byte pt + 20-byte aad) through the encrypt shape, and the decrypt
//! shape's defining property — GHASH covers the received ciphertext, not
//! the recovered plaintext (the encrypt shape cannot AEAD-open).

use volar_fuzz::interpreter::biir::eval_biir;
use volar_vc::aes_gadget::{build_aes128_gcm_decrypt_var, build_aes128_gcm_var};

fn hex(s: &str) -> Vec<u8> {
    let s: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
        .collect()
}

fn bits_of(b: &[u8]) -> Vec<bool> {
    b.iter()
        .flat_map(|x| (0..8).map(move |i| (x >> i) & 1 == 1))
        .collect()
}

fn bytes_of(b: &[bool]) -> Vec<u8> {
    b.chunks(8)
        .map(|c| {
            c.iter()
                .enumerate()
                .fold(0u8, |a, (i, &x)| a | ((x as u8) << i))
        })
        .collect()
}

fn gcm_var(key: &[u8; 16], iv: &[u8; 12], aad: &[u8], pt: &[u8]) -> Vec<u8> {
    let c = build_aes128_gcm_var(aad.len(), pt.len());
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(iv));
    inputs.extend(bits_of(aad));
    inputs.extend(bits_of(pt));
    bytes_of(&eval_biir(&c, &inputs).expect("eval"))
}

#[test]
fn gcm_var_nist_case_4() {
    let key: [u8; 16] = hex("feffe9928665731c6d6a8f9467308308").try_into().unwrap();
    let iv: [u8; 12] = hex("cafebabefacedbaddecaf888").try_into().unwrap();
    let aad = hex("feedfacedeadbeeffeedfacedeadbeefabaddad2");
    let pt = hex(
        "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a72\
         1c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39",
    );
    let out = gcm_var(&key, &iv, &aad, &pt);
    assert_eq!(
        out[..pt.len()],
        hex(
            "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e\
             21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091"
        )[..],
        "NIST case 4 ct"
    );
    assert_eq!(
        out[pt.len()..],
        hex("5bc94fbc3221a5db94fae95ae7121a47")[..],
        "NIST case 4 tag"
    );

    // The decrypt shape recovers the plaintext and recomputes the same
    // tag from the ciphertext input.
    let d = build_aes128_gcm_decrypt_var(aad.len(), pt.len());
    let mut inputs = bits_of(&key);
    inputs.extend(bits_of(&iv));
    inputs.extend(bits_of(&aad));
    inputs.extend(bits_of(&out[..pt.len()]));
    let back = bytes_of(&eval_biir(&d, &inputs).expect("eval"));
    assert_eq!(back[..pt.len()], pt[..], "decrypt recovers pt");
    assert_eq!(
        back[pt.len()..],
        hex("5bc94fbc3221a5db94fae95ae7121a47")[..],
        "decrypt recomputes the tag over the ciphertext"
    );
}
