//! First-ever exercise of `volar-build`'s real object-emission code path
//! (`pipeline_smoke.rs` only ever runs the pipeline as far as Volar IR).
//!
//! Two tests:
//! - `compile_to_object_plain_smoke`: the existing, non-chunked
//!   `Pipeline::<VolarIrStage>::compile_to_object` path, linked with a tiny C
//!   harness and run, to close the pre-existing "never actually exercised"
//!   gap.
//! - `compile_chunks_to_objects_smoke`: the new multi-translation-unit path
//!   (`Pipeline::<VaffleStage>::compile_chunks_to_objects`) — a 3-function
//!   VAFFLE call chain split into 2 independent object files, linked
//!   together, and run, proving a call crossing a chunk boundary resolves
//!   correctly via ordinary object-level symbol resolution.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use vaffle::{
    Block, BlockId, FuncBody, FuncDecl, FuncId, Module, SigDecl, SigId, Terminator, Value, ValueId,
};
use volar_build::{CompileOptions, Pipeline};
use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRType, IRTypeId,
    IRTypes, IRVarId,
};
use volar_ir_common::{Constant, Node, Stmt, Type, TypeTable};
use tempfile::TempDir;

fn node(kind: Value) -> Node<Value, ()> {
    Node::new(kind, (), None)
}

/// Link `obj_paths` plus a small generated C `main` (`main_body`) with `cc`
/// and return the child process's captured stdout.
fn link_and_run(obj_paths: &[PathBuf], main_body: &str, work_dir: &Path) -> String {
    let c_path = work_dir.join("main.c");
    let exe_path = work_dir.join("test");
    let c_src = format!(
        "#include <stdio.h>\n#include <stdint.h>\nint main(void) {{\n{main_body}\n  return 0;\n}}\n"
    );
    std::fs::write(&c_path, &c_src).expect("write C main");

    let status = Command::new("cc")
        .arg("-o")
        .arg(&exe_path)
        .args(obj_paths)
        .arg(&c_path)
        .status()
        .expect("cc not found — install a C compiler");
    assert!(status.success(), "linking failed for:\n{c_src}");

    let output = Command::new(&exe_path)
        .output()
        .expect("failed to run compiled program");
    String::from_utf8(output.stdout).expect("non-UTF8 output")
}

/// `fn answer() -> u8 { return 42; }`, built directly as Volar IR (mirroring
/// `volar-ir-build`'s own `Pipeline<LirStage>` test fixture) — Volar IR has
/// no cross-function call construct, so this is already circuit-shaped with
/// no VAFFLE lowering or unrolling/movfuscation needed.
fn build_answer_ir() -> (IRBlocks, IRTypes) {
    let types = IRTypes(vec![IRType::Primitive(Type::_8)]);
    let blocks = IRBlocks::new(vec![IRBlock {
        params: vec![],
        stmts: vec![Node::new(
            IRStmt::Const(Constant { hi: 0, lo: 42 }, IRTypeId(0)),
            (),
            None,
        )],
        terminator: IRTerminator::Jmp {
            target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(0)]),
        },
    }]);
    (blocks, types)
}

/// `f0(x) -> f1(x) -> f2(x)`, where `f2` ignores `x` and returns 7 —
/// mirrors `volar-ir-build`'s own chunked pipeline test fixture.
fn build_chain_module() -> Module<()> {
    let mut types = TypeTable::new();
    let u8_ty = types.primitive(Type::_8);

    const N: usize = 3;
    let sigs = vec![
        SigDecl {
            params: vec![u8_ty],
            results: vec![u8_ty],
        };
        N
    ];

    let mut funcs = Vec::with_capacity(N);
    for i in 0..N - 1 {
        funcs.push(FuncDecl::Body(FuncBody {
            sig: SigId(i),
            entry: BlockId(0),
            blocks: vec![Block {
                params: vec![(ValueId(0), u8_ty)],
                stmts: vec![ValueId(1), ValueId(2)],
                terminator: Terminator::Return {
                    values: vec![ValueId(2)],
                },
            }],
            values: vec![
                node(Value::Param {
                    block: BlockId(0),
                    ty: u8_ty,
                    idx: 0,
                }),
                node(Value::Call {
                    func: FuncId(i + 1),
                    args: vec![ValueId(0)],
                }),
                node(Value::Output {
                    value: ValueId(1),
                    idx: 0,
                }),
            ],
        }));
    }
    funcs.push(FuncDecl::Body(FuncBody {
        sig: SigId(N - 1),
        entry: BlockId(0),
        blocks: vec![Block {
            params: vec![(ValueId(0), u8_ty)],
            stmts: vec![ValueId(1)],
            terminator: Terminator::Return {
                values: vec![ValueId(1)],
            },
        }],
        values: vec![
            node(Value::Param {
                block: BlockId(0),
                ty: u8_ty,
                idx: 0,
            }),
            node(Value::Op(Stmt::Const(Constant { hi: 0, lo: 7 }, u8_ty))),
        ],
    }));

    let mut exports = BTreeMap::new();
    exports.insert("vmain".to_string(), FuncId(0));

    Module {
        pointer_width: vaffle::PointerWidth::Bits64,
        types,
        oracles: vec![],
        actions: vec![],
        funcs,
        sigs,
        exports,
        pre_init: vec![],
    }
}

#[test]
fn compile_to_object_plain_smoke() {
    let dir = TempDir::new().expect("tempdir");
    let obj_path = dir.path().join("answer.o");

    let (blocks, types) = build_answer_ir();
    Pipeline::from_volar_ir_blocks(blocks, types)
        .compile_to_object(&obj_path, &CompileOptions::default())
        .expect("compile_to_object");

    assert!(obj_path.exists(), "object file was not written");

    // `Pipeline::<VolarIrStage>::compile_to_object` lowers to LIR via the
    // default (unnamed) `lower_to_lir()`, which always names the compiled
    // function `volar_module` (see `volar_ir_build::Pipeline::lower_to_lir`).
    let out = link_and_run(
        &[obj_path],
        "  extern unsigned char volar_module(void);\n  printf(\"%d\\n\", volar_module());",
        dir.path(),
    );
    assert_eq!(out.trim(), "42");
}

#[test]
fn compile_chunks_to_objects_smoke() {
    let dir = TempDir::new().expect("tempdir");

    let obj_paths = Pipeline::from_data(build_chain_module())
        .compile_chunks_to_objects(2, dir.path(), &CompileOptions::default())
        .expect("compile_chunks_to_objects");
    assert_eq!(obj_paths.len(), 2, "expected exactly 2 chunk object files");
    for p in &obj_paths {
        assert!(p.exists(), "chunk object file {p:?} was not written");
    }

    let out = link_and_run(
        &obj_paths,
        "  extern unsigned char vmain(unsigned char);\n  printf(\"%d\\n\", vmain(3));",
        dir.path(),
    );
    assert_eq!(out.trim(), "7");
}
