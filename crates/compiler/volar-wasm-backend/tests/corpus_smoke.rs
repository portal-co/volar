// @reliability: experimental
// @ai: assisted
//! Corpus smoke: every `build_*` case lowers through [`WasmBackend`] without panic.

use volar_lir_test_corpus::for_each_build;
use volar_wasm_backend::WasmBackend;

#[test]
fn for_each_build_wasm_backend() {
    for_each_build!(WasmBackend::new());
}
