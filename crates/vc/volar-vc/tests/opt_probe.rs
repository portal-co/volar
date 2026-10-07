//! Optimization probe: measure boolar gate counts of the session gadgets
//! before/after the existing fold pass and the new CSE/DCE passes.

#![cfg(feature = "std")]

use volar_ir::boolar::{BIrBlocks, BIrStmt};

fn counts(c: &BIrBlocks<()>) -> (usize, usize, usize, usize, usize, usize) {
    let (mut total, mut and, mut or, mut xor, mut not, mut konst) = (0, 0, 0, 0, 0, 0);
    for b in &c.blocks {
        for s in &b.stmts {
            total += 1;
            match s.kind {
                BIrStmt::And(_, _) => and += 1,
                BIrStmt::Or(_, _) => or += 1,
                BIrStmt::Xor(_, _) => xor += 1,
                BIrStmt::Not(_) => not += 1,
                BIrStmt::Zero | BIrStmt::One => konst += 1,
                _ => {}
            }
        }
    }
    (total, and, or, xor, not, konst)
}

fn report(name: &str, mut c: BIrBlocks<()>) {
    let before = counts(&c);
    let sched_before = volar_vc::compile_schedule(&c)
        .expect("schedules before")
        .and_count();
    volar_ir_opt::biir::fold_biir_blocks(&mut c);
    let after_fold = counts(&c);
    volar_ir_opt::biir::cse_biir_blocks(&mut c);
    let after_cse = counts(&c);
    volar_ir_opt::biir::dce_biir_blocks(&mut c);
    let after_dce = counts(&c);
    let sched = volar_vc::compile_schedule(&c).expect("schedules after opt");
    eprintln!(
        "{name}: total {} -> {} -> {} -> {} | and {} -> {} -> {} -> {} | schedule_and {} -> {}",
        before.0,
        after_fold.0,
        after_cse.0,
        after_dce.0,
        before.1,
        after_fold.1,
        after_cse.1,
        after_dce.1,
        sched_before,
        sched.and_count(),
    );
}

#[test]
fn gadget_gate_counts() {
    report("x25519_step", volar_vc::x25519_gadget::build_x25519_step());
    report("fe_square", volar_vc::x25519_gadget::build_fe_square());
    report("fe_mul", volar_vc::x25519_gadget::build_fe_mul());
    report("final_cswap", volar_vc::x25519_gadget::build_final_cswap());
    report("aes128", volar_vc::aes_gadget::build_aes128());
    report(
        "gcm_record_open_512",
        volar_vc::tls13::record_open_circuit(512),
    );
    report(
        "gcm_record_seal_512",
        volar_vc::tls13::record_seal_circuit(512),
    );
    report("sha256_1block", volar_vc::sha_gadget::build_sha256(55));
    report("hmac_short", volar_vc::sha_gadget::build_hmac_sha256(32, 5));
    report("transcript_200", volar_vc::tls13::transcript_circuit(200));
    report("extract", volar_vc::tls13::extract_circuit(32, 32));
}
