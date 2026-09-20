//! Directional ownership scripts for durable held-material AES blocks.

use volar_mpc::strict_chain::MaterialRole;
use volar_mpc::strict_split::{SplitInput, SplitOutput};
use volar_vc::compile_schedule;
use volar_vc::oram_gadget::{
    MaterialBlockDirection, build_material_open_evaluator_block, build_material_open_garbler_block,
    build_material_seal_evaluator_block, build_material_seal_garbler_block,
    material_block_tweak_checked,
};
use volar_vc::oram_material::{MATERIAL_BLOCK_BITS, MaterialBlockProtocol};

#[test]
fn every_direction_has_a_64_64_key_partition_and_fixed_block_shape() {
    for direction in [
        MaterialBlockDirection::SealGarbler,
        MaterialBlockDirection::OpenGarbler,
        MaterialBlockDirection::SealEvaluator,
        MaterialBlockDirection::OpenEvaluator,
    ] {
        let protocol = MaterialBlockProtocol::for_direction(direction);
        assert_eq!(protocol.inputs.len(), 384);
        assert_eq!(protocol.outputs.len(), MATERIAL_BLOCK_BITS);
        assert!(
            protocol.inputs[..64]
                .iter()
                .all(|input| *input == SplitInput::Garbler)
        );
        assert!(
            protocol.inputs[64..128]
                .iter()
                .all(|input| *input == SplitInput::Evaluator)
        );
        assert!(
            protocol.inputs[128..256]
                .iter()
                .all(|input| *input == SplitInput::Public)
        );
    }
}

#[test]
fn material_directions_never_cross_the_role_seam() {
    let seal_garbler = MaterialBlockProtocol::for_direction(MaterialBlockDirection::SealGarbler);
    assert!(
        seal_garbler.inputs[256..]
            .iter()
            .all(|input| *input == SplitInput::Garbler)
    );
    assert!(
        seal_garbler
            .outputs
            .iter()
            .all(|output| *output == SplitOutput::EvaluatorReveal)
    );

    let open_garbler = MaterialBlockProtocol::for_direction(MaterialBlockDirection::OpenGarbler);
    assert!(
        open_garbler.inputs[256..]
            .iter()
            .all(|input| *input == SplitInput::Evaluator)
    );
    assert!(
        open_garbler
            .outputs
            .iter()
            .all(|output| *output == SplitOutput::GarblerReveal)
    );

    for direction in [
        MaterialBlockDirection::SealEvaluator,
        MaterialBlockDirection::OpenEvaluator,
    ] {
        let protocol = MaterialBlockProtocol::for_direction(direction);
        assert!(
            protocol.inputs[256..]
                .iter()
                .all(|input| *input == SplitInput::Evaluator)
        );
        assert!(
            protocol
                .outputs
                .iter()
                .all(|output| *output == SplitOutput::EvaluatorReveal)
        );
    }
}

#[test]
fn all_directional_circuits_are_fixed_384_to_128_aes_xor_shapes() {
    for circuit in [
        build_material_seal_garbler_block(),
        build_material_open_garbler_block(),
        build_material_seal_evaluator_block(),
        build_material_open_evaluator_block(),
    ] {
        let schedule = compile_schedule(&circuit).expect("material circuit compiles");
        assert_eq!(schedule.num_inputs, 384);
        assert_eq!(schedule.output_wires().len(), MATERIAL_BLOCK_BITS);
    }
}

#[test]
fn both_material_halves_require_separate_durable_transactions() {
    assert_eq!(
        MaterialBlockProtocol::for_store_owner(MaterialRole::Garbler),
        Some(MaterialBlockDirection::SealGarbler)
    );
    assert_eq!(
        MaterialBlockProtocol::for_load_owner(MaterialRole::Garbler),
        Some(MaterialBlockDirection::OpenGarbler)
    );
    assert_eq!(
        MaterialBlockProtocol::for_store_owner(MaterialRole::Evaluator),
        Some(MaterialBlockDirection::SealEvaluator)
    );
    assert_eq!(
        MaterialBlockProtocol::for_load_owner(MaterialRole::Evaluator),
        Some(MaterialBlockDirection::OpenEvaluator)
    );
    assert_eq!(
        MaterialBlockProtocol::for_store_owner(MaterialRole::Both),
        None
    );
    assert_eq!(
        MaterialBlockProtocol::for_load_owner(MaterialRole::Both),
        None
    );
}

#[test]
fn fixed_tweak_format_rejects_values_that_would_alias() {
    assert!(
        material_block_tweak_checked(0, (1 << 48) - 1, (1 << 48) - 1, u16::MAX as u32).is_some()
    );
    assert!(material_block_tweak_checked(0, 1 << 48, 0, 0).is_none());
    assert!(material_block_tweak_checked(0, 0, 1 << 48, 0).is_none());
    assert!(material_block_tweak_checked(0, 0, 0, (u16::MAX as u32) + 1).is_none());
}
