// Known-answer: Keccak-f[1600] over the all-zero state.
// Reference (Keccak Team's Keccak-reference, "ShortInputKAT" style):
// lane[0] after one permutation = 0xF1258F7940E1DDE7.
#[test]
fn keccak_f1600_zero_state_known_answer() {
    let mut st = [0u64; 25];
    volar_spec::keccak::keccak_f1600(&mut st);
    assert_eq!(st[0], 0xF1258F7940E1DDE7u64, "lane 0");
    // The full-state pin is the sha3-crate differential in keccak::tests
    // (covers all 1600 bits through the sponge); this KAT catches gross
    // permutation regressions cheaply.
}
