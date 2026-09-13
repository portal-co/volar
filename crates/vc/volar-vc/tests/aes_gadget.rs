// @reliability: experimental
// @ai: assisted
//! Validate the boolar AES-128 gadget against the FIPS-197 known-answer vectors
//! (the same KATs the `volar_spec::faest::aes` reference is checked against),
//! evaluated concretely via `volar_fuzz::interpreter::biir::eval_biir`.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_vc::aes_gadget::build_aes128;

fn bits_of_bytes(bytes: &[u8; 16]) -> Vec<bool> {
    let mut v = Vec::with_capacity(128);
    for b in bytes {
        for j in 0..8 {
            v.push((b >> j) & 1 == 1);
        }
    }
    v
}

fn bytes_of_bits(bits: &[bool]) -> [u8; 16] {
    let mut out = [0u8; 16];
    for i in 0..16 {
        let mut b = 0u8;
        for j in 0..8 {
            if bits[i * 8 + j] {
                b |= 1 << j;
            }
        }
        out[i] = b;
    }
    out
}

fn run_aes(key: [u8; 16], plain: [u8; 16]) -> [u8; 16] {
    let circ = build_aes128();
    let mut inp = bits_of_bytes(&key);
    inp.extend_from_slice(&bits_of_bytes(&plain));
    let out = eval_biir(&circ, &inp).expect("aes circuit evaluates");
    assert_eq!(out.len(), 128);
    bytes_of_bits(&out)
}

#[test]
fn aes_gadget_fips_197_appendix_b() {
    let key: [u8; 16] = [
        0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f,
        0x3c,
    ];
    let plain: [u8; 16] = [
        0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37, 0x07,
        0x34,
    ];
    let want: [u8; 16] = [
        0x39, 0x25, 0x84, 0x1d, 0x02, 0xdc, 0x09, 0xfb, 0xdc, 0x11, 0x85, 0x97, 0x19, 0x6a, 0x0b,
        0x32,
    ];
    assert_eq!(run_aes(key, plain), want);
}

#[test]
fn aes_gadget_fips_197_appendix_c1() {
    let key = [0x00u8; 16];
    let plain = [0x00u8; 16];
    // AES-128(0, 0) = 66e94bd4ef8a2c3b884cfa59ca342b2e.
    let want: [u8; 16] = [
        0x66, 0xe9, 0x4b, 0xd4, 0xef, 0x8a, 0x2c, 0x3b, 0x88, 0x4c, 0xfa, 0x59, 0xca, 0x34, 0x2b,
        0x2e,
    ];
    assert_eq!(run_aes(key, plain), want);
}
