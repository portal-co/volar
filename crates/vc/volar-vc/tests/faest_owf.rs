// @reliability: experimental
// @ai: assisted
//! AX5: the FAEST OWF consistency check — the first consumer of the
//! `aes128_encrypt_block` extern — running identically natively
//! (volar-spec's byte-level FAEST AES) and in-circuit (extern realized by
//! `expand_aes_oracles`). Also drives the extern end-to-end through the WAT
//! frontend: a WASM guest importing `portal_crypto.aes128_enc` lowers through
//! the vc pipeline with `WaffleImportConfig::with_portal_crypto_aes()`.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir_common::aes_extern;
use volar_vc::aes_extern::expand_aes_oracles;
use volar_vc::faest_owf::{OWF_INPUT_BITS, build_faest_owf_check, faest_owf_check_native};

const KEY: [u8; 16] = [
    0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c,
];
const PT: [u8; 16] = [
    0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37, 0x07, 0x34,
];
const CT: [u8; 16] = [
    0x39, 0x25, 0x84, 0x1d, 0x02, 0xdc, 0x09, 0xfb, 0xdc, 0x11, 0x85, 0x97, 0x19, 0x6a, 0x0b, 0x32,
];

fn bits_of(bytes: &[u8]) -> Vec<bool> {
    let mut v = Vec::with_capacity(bytes.len() * 8);
    for b in bytes {
        for j in 0..8 {
            v.push((b >> j) & 1 == 1);
        }
    }
    v
}

fn owf_inputs(key: &[u8; 16], pt: &[u8; 16], ct: &[u8; 16]) -> Vec<bool> {
    let mut v = bits_of(key);
    v.extend(bits_of(pt));
    v.extend(bits_of(ct));
    v
}

#[test]
fn owf_check_circuit_matches_native() {
    let circuit = expand_aes_oracles(&build_faest_owf_check());
    assert!(
        circuit.blocks[0]
            .stmts
            .iter()
            .all(|s| !matches!(&s.kind, volar_ir::boolar::BIrStmt::OracleBit { .. })),
        "extern realized: no oracle bits remain"
    );

    // Honest statement: ok, both ways.
    assert!(faest_owf_check_native(&KEY, &PT, &CT));
    let out = eval_biir(&circuit, &owf_inputs(&KEY, &PT, &CT)).expect("eval");
    assert_eq!(out, vec![true], "valid OWF statement verifies in-circuit");

    // Tampered ciphertext: rejected, both ways.
    let mut bad_ct = CT;
    bad_ct[0] ^= 1;
    assert!(!faest_owf_check_native(&KEY, &PT, &bad_ct));
    let out = eval_biir(&circuit, &owf_inputs(&KEY, &PT, &bad_ct)).expect("eval");
    assert_eq!(out, vec![false], "tampered OWF statement rejected in-circuit");

    // A wrong key equally fails (binding to the statement).
    let mut bad_key = KEY;
    bad_key[15] ^= 0x80;
    let out = eval_biir(&circuit, &owf_inputs(&bad_key, &PT, &CT)).expect("eval");
    assert_eq!(out, vec![false]);
    assert!(!faest_owf_check_native(&bad_key, &PT, &CT));
}

/// The WAT guest: `ok = (aes128_enc(k) == ct)` over i64-pair-packed words,
/// importing the extern exactly as guest code would.
const OWF_WAT: &str = r#"(module
  (import "portal_crypto" "aes128_enc" (func $aes (param i64 i64 i64 i64) (result i64 i64)))
  (func $f (export "f")
      (param $klo i64) (param $khi i64) (param $plo i64) (param $phi i64)
      (param $clo i64) (param $chi i64) (result i32)
    (local $rlo i64) (local $rhi i64)
    (call $aes (local.get $klo) (local.get $khi) (local.get $plo) (local.get $phi))
    (local.set $rhi)
    (local.set $rlo)
    (i32.and
      (i64.eq (local.get $rlo) (local.get $clo))
      (i64.eq (local.get $rhi) (local.get $chi)))))"#;

#[test]
fn owf_check_wat_guest_end_to_end() {
    let bytes: &'static [u8] = Box::leak(wat::parse_str(OWF_WAT).unwrap().into_boxed_slice());
    let module = portal_pc_waffle_frontend::from_wasm_bytes(
        bytes,
        &portal_pc_waffle_frontend::FrontendOptions::default(),
    )
    .expect("waffle parse");
    let mut target = volar_vaffle_target::VaffleTarget::new();
    let config = volar_vaffle_target::WaffleImportConfig::new().with_portal_crypto_aes();
    let errors = volar_vaffle_target::lower_waffle_module(&module, &mut target, &config);
    assert!(errors.is_empty(), "{errors:?}");
    let (blocks, types) = volar_vaffle_target::lower_vaffle_to_ir_owned(target.module);
    let ir_circuit = volar_ir_passes::unroll_ir_everything(&blocks, &types).expect("unroll");
    assert!(
        ir_circuit.blocks[0]
            .stmts
            .iter()
            .any(|s| matches!(&s.kind, volar_ir::ir::IRStmt::OracleCall { name, .. } if name == aes_extern::ORACLE_NAME)),
        "the guest's extern call survives to the IR as an OracleCall"
    );
    let boolar = volar_ir_passes::lower_ir_to_boolar(&ir_circuit, &types);
    let realized = expand_aes_oracles(&boolar);
    assert!(
        realized.blocks[0]
            .stmts
            .iter()
            .all(|s| !matches!(&s.kind, volar_ir::boolar::BIrStmt::OracleBit { .. })),
        "extern realized: no oracle bits remain"
    );

    // Entry inputs: the six i64 params (LE byte-packed) plus any scaffold
    // words the call ABI appends (zeroed here).
    let n_params = realized.blocks[0].params as usize;
    assert!(n_params >= OWF_INPUT_BITS, "params cover the OWF inputs");
    let mut inputs = owf_inputs(&KEY, &PT, &CT);
    inputs.resize(n_params, false);
    let out = eval_biir(&realized, &inputs).expect("eval");
    assert!(out[0], "WAT guest verifies the honest OWF statement");

    let mut bad = owf_inputs(&KEY, &PT, &CT);
    bad[aes_extern::ARG_BITS] = !bad[aes_extern::ARG_BITS]; // flip ct bit 0
    bad.resize(n_params, false);
    let out = eval_biir(&realized, &bad).expect("eval");
    assert!(!out[0], "WAT guest rejects a tampered ciphertext");
}
