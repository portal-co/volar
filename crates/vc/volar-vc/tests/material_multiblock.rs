use volar_fuzz::interpreter::biir::eval_biir;
use volar_mpc::strict_split::{SplitInput, SplitOutput};
use volar_vc::compile_schedule;
use volar_vc::held_remap::DeferredLabelRemapPlan;
use volar_vc::oram_gadget::{build_material_block_cipher_n, build_material_block_loop_step};

#[test]
fn multi_block_material_gadget_has_one_key_and_linear_block_io() {
    let one = compile_schedule(&build_material_block_cipher_n(1)).expect("one block");
    let four = compile_schedule(&build_material_block_cipher_n(4)).expect("four blocks");
    assert_eq!(one.num_inputs, 384);
    assert_eq!(one.output_wires().len(), 128);
    assert_eq!(four.num_inputs, 128 + 4 * 256);
    assert_eq!(four.output_wires().len(), 4 * 128);
    // The current prototype inlines independent AES blocks. This asserts the
    // honest baseline that motivates the loop/key-schedule sharing follow-up.
    assert_eq!(four.and_count(), one.and_count() * 4);
}

#[test]
fn loop_step_exposes_revealed_termination_and_held_counter_state_shape() {
    let counter_bits = 8;
    let circuit = build_material_block_loop_step(counter_bits);
    let schedule = compile_schedule(&circuit).expect("loop step");
    assert_eq!(schedule.num_inputs, 384 + counter_bits);
    assert_eq!(schedule.output_wires().len(), 128 + counter_bits + 1);

    let mut input = vec![false; 384 + counter_bits];
    // `remaining = 2`: emit next=1 and `done = false`.
    input[384 + 1] = true;
    let output = eval_biir(&circuit, &input).expect("concrete loop step");
    assert!(output[128], "low bit of decremented counter");
    assert!(
        !output[128 + counter_bits],
        "do not reveal termination early"
    );

    input[384 + 1] = false;
    input[384] = true;
    let output = eval_biir(&circuit, &input).expect("concrete final loop step");
    assert!(
        output[128 + counter_bits],
        "remaining=1 reveals termination"
    );
}

#[test]
fn deferred_remap_stays_opaque_and_never_host_decodes() {
    let plan = DeferredLabelRemapPlan::width(16);
    assert_eq!(plan.inputs.len(), 16 * 3);
    assert!(
        plan.inputs.chunks(3).all(|chunk| {
            chunk == [SplitInput::Held, SplitInput::Garbler, SplitInput::Evaluator]
        })
    );
    assert!(
        plan.outputs
            .iter()
            .all(|output| *output == SplitOutput::Opaque)
    );
}
