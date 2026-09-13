//! P4c-ii: the SHA-256 / HMAC / HKDF boolar gadgets, checked concretely
//! (`eval_biir`) against the standard vectors — FIPS 180-4 examples, RFC
//! 4231 (HMAC), RFC 5869 (HKDF). These are the TLS 1.3 transcript-hash and
//! key-schedule primitives the MPC-TLS client needs in-circuit.

use volar_vc::sha_gadget::{build_hkdf_sha256, build_hmac_sha256, build_sha256};

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

fn eval(circuit: &volar_ir::boolar::BIrBlocks, inputs: &[bool]) -> Vec<u8> {
    bytes_of(&volar_fuzz::interpreter::biir::eval_biir(circuit, inputs).expect("concrete eval"))
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len() / 2)
        .map(|i| u8::from_str_radix(&s[2 * i..2 * i + 2], 16).unwrap())
        .collect()
}

#[test]
fn sha256_fips1804_vectors() {
    // "abc" (FIPS 180-4 example 1).
    let c = build_sha256(3);
    let d = eval(&c, &bits_of(b"abc"));
    assert_eq!(
        d,
        hex("BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"),
        "sha256(abc)"
    );
    // Empty string.
    let c = build_sha256(0);
    let d = eval(&c, &[]);
    assert_eq!(
        d,
        hex("E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"),
        "sha256()"
    );
    // The 56-byte two-block example (FIPS 180-4 example 2).
    let msg = b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";
    assert_eq!(msg.len(), 56);
    let c = build_sha256(56);
    let d = eval(&c, &bits_of(msg));
    assert_eq!(
        d,
        hex("248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1"),
        "sha256(abcdbc...)"
    );
}

#[test]
fn hmac_sha256_rfc4231() {
    // RFC 4231 test case 1: key = 0x0b * 20, data = "Hi There".
    let c = build_hmac_sha256(20, 8);
    let mut inputs = bits_of(&[0x0b; 20]);
    inputs.extend(bits_of(b"Hi There"));
    let d = eval(&c, &inputs);
    assert_eq!(
        d,
        hex("b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"),
        "RFC4231 case 1"
    );
    // RFC 4231 test case 6 (key = 0xaa * 131 > block size? no: 131 > 64 —
    // out of gadget scope). Use case 2: key = "Jefe" (4 bytes),
    // data = "what do ya want for nothing?" (28 bytes).
    let c = build_hmac_sha256(4, 28);
    let mut inputs = bits_of(b"Jefe");
    inputs.extend(bits_of(b"what do ya want for nothing?"));
    let d = eval(&c, &inputs);
    assert_eq!(
        d,
        hex("5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"),
        "RFC4231 case 2"
    );
}

#[test]
fn hkdf_sha256_rfc5869_case1() {
    // RFC 5869 Appendix A.1 (SHA-256):
    //   IKM  = 0x0b * 22
    //   salt = 0x000102030405060708090a0b0c
    //   info = 0xf0f1f2f3f4f5f6f7f8f9
    //   L    = 42
    let c = build_hkdf_sha256(13, 22, 10, 42);
    let mut inputs = bits_of(&hex("000102030405060708090a0b0c"));
    inputs.extend(bits_of(&[0x0b; 22]));
    inputs.extend(bits_of(&hex("f0f1f2f3f4f5f6f7f8f9")));
    let okm = eval(&c, &inputs);
    assert_eq!(
        okm,
        hex("3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"),
        "RFC5869 A.1 OKM"
    );
}
