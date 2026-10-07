//! Exploratory binary-field multiplication benchmark through the LLVM import pipeline.
//!
//! Build once, then run one candidate per process under `/usr/bin/time -l` for
//! candidate-specific peak RSS:
//! `cargo build --release --manifest-path volar/Cargo.toml -p volar-vc --example field_circuit_bench`
//! `/usr/bin/time -l volar/target/release/examples/field_circuit_bench 64-p1b 1000`
//!
//! This measures compiler/circuit costs and plain Volar IR interpreter time. It
//! is not a protocol benchmark or a security parameter selection.

use std::hint::black_box;
use std::time::Instant;

use inkwell::{context::Context, memory_buffer::MemoryBuffer};
use volar_fuzz::interpreter::{biir::eval_biir, ir::eval_ir};
use volar_ir::boolar::BIrStmt;
use volar_ir_passes::{lower_ir_to_boolar::lower_ir_to_boolar, unroll_ir::unroll_ir_everything};
use volar_llvm_vaffle_import::import_module;
use volar_vaffle_target::lower_vaffle_to_ir_owned;

#[derive(Clone, Copy)]
struct Candidate {
    name: &'static str,
    degree: u32,
    polynomial: &'static str,
}

const CANDIDATES: &[Candidate] = &[
    Candidate {
        name: "64-p1b",
        degree: 64,
        polynomial: "1b",
    },
    Candidate {
        name: "64-p807",
        degree: 64,
        polynomial: "807",
    },
    Candidate {
        name: "128-p87",
        degree: 128,
        polynomial: "87",
    },
    Candidate {
        name: "128-p10000000000000000b",
        degree: 128,
        polynomial: "10000000000000000b",
    },
];

fn output_integer(values: &[Vec<bool>]) -> Vec<bool> {
    values
        .iter()
        .flat_map(|value| value.iter().copied())
        .collect()
}

fn deterministic_bits(width: usize, mut state: u64) -> Vec<bool> {
    (0..width)
        .map(|_| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state & 1 != 0
        })
        .collect()
}

fn pack_inputs(lhs: &[bool], rhs: &[bool]) -> Vec<Vec<bool>> {
    [lhs, rhs]
        .into_iter()
        .flat_map(|value| value.chunks(64).map(|chunk| chunk.to_vec()))
        .collect()
}

fn multiply_reference(lhs: &[bool], rhs: &[bool], low_polynomial: u128) -> Vec<bool> {
    let degree = lhs.len();
    assert_eq!(rhs.len(), degree);
    let mut product = vec![false; degree * 2];
    for (i, &left) in lhs.iter().enumerate() {
        if left {
            for (j, &right) in rhs.iter().enumerate() {
                product[i + j] ^= right;
            }
        }
    }

    for exponent in (degree..product.len()).rev() {
        if !product[exponent] {
            continue;
        }
        product[exponent] = false;
        let offset = exponent - degree;
        for tap in 0..degree {
            if (low_polynomial >> tap) & 1 != 0 {
                product[offset + tap] ^= true;
            }
        }
    }
    product.truncate(degree);
    product
}

fn find_candidate(name: &str) -> Candidate {
    CANDIDATES
        .iter()
        .copied()
        .find(|candidate| candidate.name == name)
        .unwrap_or_else(|| {
            eprintln!(
                "candidate must be one of: {}",
                CANDIDATES
                    .iter()
                    .map(|c| c.name)
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            std::process::exit(2);
        })
}

fn main() {
    let mut args = std::env::args().skip(1);
    let candidate_name = args.next().unwrap_or_else(|| {
        eprintln!("usage: field_circuit_bench <candidate> [plain-eval-iterations]");
        std::process::exit(2);
    });
    let candidate = find_candidate(&candidate_name);
    let iterations = args
        .next()
        .map(|value| value.parse::<usize>().expect("integer iteration count"))
        .unwrap_or(100);

    let symbol = format!(
        "volar.field.mul.d{}.bit.p{}",
        candidate.degree, candidate.polynomial
    );
    let llvm_ir = format!(
        "declare i{width} @{symbol}(i{width}, i{width})\n\
         define i{width} @mul(i{width} %a, i{width} %b) {{\n\
         entry:\n\
         %r = call i{width} @{symbol}(i{width} %a, i{width} %b)\n\
         ret i{width} %r\n\
         }}\n",
        width = candidate.degree,
    );

    let context = Context::create();
    let llvm = context
        .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
            llvm_ir.as_bytes(),
            "field-circuit-bench.ll",
        ))
        .expect("parse LLVM IR");
    let vaffle = import_module(&llvm, &["mul"]).expect("LLVM -> VAFFLE import");
    let (plain_ir, types) = lower_vaffle_to_ir_owned(vaffle);
    let circuit_ir = unroll_ir_everything(&plain_ir, &types)
        .expect("LLVM-imported straight-line field operation unrolls to a circuit");
    assert!(circuit_ir.is_circuit(), "expected circuit-shaped Volar IR");
    let boolar = lower_ir_to_boolar(&circuit_ir, &types);
    assert_eq!(boolar.blocks.len(), 1);

    let low_polynomial =
        u128::from_str_radix(candidate.polynomial, 16).expect("candidate polynomial fits in u128");
    let sample_inputs = (0..16)
        .map(|sample| {
            let width = candidate.degree as usize;
            let lhs = deterministic_bits(width, 0x9e37_79b9_7f4a_7c15 ^ sample as u64);
            let rhs = deterministic_bits(width, 0xd1b5_4a32_d192_ed03 ^ ((sample as u64) << 1));
            let inputs = pack_inputs(&lhs, &rhs);
            let expected = multiply_reference(&lhs, &rhs, low_polynomial);
            let plain_output =
                output_integer(&eval_ir(&plain_ir, &types, &inputs).expect("plain IR eval"));
            assert_eq!(plain_output, expected, "field reference mismatch");
            let boolar_output = eval_biir(
                &boolar,
                &inputs.iter().flatten().copied().collect::<Vec<_>>(),
            )
            .expect("Boolar eval");
            assert_eq!(
                boolar_output, expected,
                "Boolar result differs from plain IR"
            );
            inputs
        })
        .collect::<Vec<_>>();

    let mut ands = 0usize;
    let mut boolar_ops = 0usize;
    for stmt in boolar.blocks.iter().flat_map(|block| &block.stmts) {
        boolar_ops += 1;
        if matches!(stmt.kind, BIrStmt::And(_, _)) {
            ands += 1;
        }
    }
    let ir_ops = plain_ir
        .blocks
        .iter()
        .map(|block| block.stmts.len())
        .sum::<usize>();
    let circuit_ir_ops = circuit_ir
        .blocks
        .iter()
        .map(|block| block.stmts.len())
        .sum::<usize>();

    let start = Instant::now();
    for iteration in 0..iterations {
        let inputs = &sample_inputs[iteration % sample_inputs.len()];
        black_box(eval_ir(&plain_ir, &types, black_box(inputs)).expect("plain IR eval"));
    }
    let elapsed = start.elapsed();

    println!(
        "candidate={} degree={} polynomial=x^{}+0x{} iterations={} llvm_instructions={} ir_ops={} circuit_ir_ops={} boolar_ops={} ands={} plain_total_ns={} plain_ns_per_mul={:.1} peak_rss=external_time_-l",
        candidate.name,
        candidate.degree,
        candidate.degree,
        candidate.polynomial,
        iterations,
        2, // the function body has a call and a return
        ir_ops,
        circuit_ir_ops,
        boolar_ops,
        ands,
        elapsed.as_nanos(),
        elapsed.as_nanos() as f64 / iterations as f64,
    );
}
