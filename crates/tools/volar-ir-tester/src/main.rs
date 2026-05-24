// @reliability: experimental
// @ai: assisted
//! Compile a `.wasm` file to Volar IR, report size metrics, and optionally
//! write the text representation to a `.vir` file.
//!
//! Usage: volar-ir-tester <input.wasm> [output.vir]

use std::{env, fs, path::Path, process};

use volar_build::Pipeline;
use volar_ir_text::{SavedIrBlocks, WriteText};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: volar-ir-tester <input.wasm> [output.vir]");
        process::exit(1);
    }

    let wasm_path = Path::new(&args[1]);
    let out_path: Option<&Path> = args.get(2).map(|s| Path::new(s.as_str()));

    let (blocks, types) = Pipeline::from_wasm(wasm_path)
        .lower_to_volar_ir()
        .to_volar_ir()
        .unwrap_or_else(|e| {
            eprintln!("error: {e}");
            process::exit(1);
        });

    let saved = SavedIrBlocks { types, blocks };
    let text = saved.to_text_string();

    let block_count = saved.blocks.blocks.len();
    let stmt_count: usize = saved.blocks.blocks.iter().map(|b| b.stmts.len()).sum();
    let text_bytes = text.len();

    println!("blocks:     {block_count}");
    println!("stmts:      {stmt_count}");
    println!("text bytes: {text_bytes}");

    if let Some(p) = out_path {
        fs::write(p, &text).unwrap_or_else(|e| eprintln!("warn: write failed: {e}"));
        println!("wrote:      {}", p.display());
    }
}
