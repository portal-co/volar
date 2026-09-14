//! Durable held-material block format: AES pads are evaluated in the circuit
//! and bind the public role/slot/version/block transaction name.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_spec::faest::aes::encrypt_block;
use volar_vc::oram_gadget::{build_material_block_cipher, material_block_tweak};

fn bits(bytes: &[u8; 16]) -> Vec<bool> {
    bytes
        .iter()
        .flat_map(|byte| (0..8).map(move |bit| (byte >> bit) & 1 != 0))
        .collect()
}

fn bytes(bits: &[bool]) -> [u8; 16] {
    core::array::from_fn(|index| {
        (0..8).fold(0u8, |value, bit| {
            value | ((bits[index * 8 + bit] as u8) << bit)
        })
    })
}

#[test]
fn material_block_circuit_matches_aes_xor_pad_and_is_involutory() {
    let key = [0x51; 16];
    let plaintext = [0xA7; 16];
    let tweak = material_block_tweak(0, 37, 9, 0);
    let circuit = build_material_block_cipher();

    let mut input = bits(&key);
    input.extend(bits(&tweak));
    input.extend(bits(&plaintext));
    let ciphertext = bytes(&eval_biir(&circuit, &input).expect("seal circuit"));
    let pad = encrypt_block(&key, &tweak);
    assert_eq!(
        ciphertext,
        core::array::from_fn(|index| plaintext[index] ^ pad[index])
    );

    let mut reopen = bits(&key);
    reopen.extend(bits(&tweak));
    reopen.extend(bits(&ciphertext));
    assert_eq!(
        bytes(&eval_biir(&circuit, &reopen).expect("open circuit")),
        plaintext
    );
}

#[test]
fn material_tweaks_are_role_slot_version_and_block_bound() {
    let base = material_block_tweak(0, 7, 11, 0);
    assert_ne!(base, material_block_tweak(1, 7, 11, 0));
    assert_ne!(base, material_block_tweak(0, 8, 11, 0));
    assert_ne!(base, material_block_tweak(0, 7, 12, 0));
    assert_ne!(base, material_block_tweak(0, 7, 11, 1));
}
