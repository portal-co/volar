#[test]
fn opt_bisect_field_gadgets() {
    use volar_fuzz::interpreter::biir::eval_biir;
    use volar_vc::x25519_gadget::scalar_ref::*;
    let z: Fp = fp_from_bytes(&[0x2Bu8; 32]);
    let x: Fp = fp_from_bytes(&[0x4Du8; 32]);
    let fe_in = |w: &Fp| volar_vc::tls13_2pc::bits_of(&fp_to_bytes(w))[..255].to_vec();

    for (name, c, inp, want) in [
        {
            let c = volar_vc::x25519_gadget::build_fe_square();
            let want = eval_biir(&c, &fe_in(&z)).unwrap();
            ("fe_square", c, fe_in(&z), want)
        },
        {
            let c = volar_vc::x25519_gadget::build_fe_mul();
            let mut i = fe_in(&z); i.extend(fe_in(&x));
            let want = eval_biir(&c, &i).unwrap();
            ("fe_mul", c, i, want)
        },
        {
            let c = volar_vc::x25519_gadget::build_final_cswap();
            let mut i = vec![true]; i.extend(fe_in(&z)); i.extend(fe_in(&x)); i.extend(fe_in(&x)); i.extend(fe_in(&z));
            let want = eval_biir(&c, &i).unwrap();
            ("final_cswap", c, i, want)
        },
    ] {
        for pass in ["fold", "cse", "dce"] {
            let mut cc = want.clone(); let _ = &mut cc; // silence
            let mut cc = match pass {
                "fold" => { let mut c2 = c.clone(); volar_ir_opt::biir::fold_biir_blocks(&mut c2); c2 }
                "cse" => { let mut c2 = c.clone(); volar_ir_opt::biir::cse_biir_blocks(&mut c2); c2 }
                _ => { let mut c2 = c.clone(); volar_ir_opt::biir::dce_biir_blocks(&mut c2); c2 }
            };
            let got = eval_biir(&cc, &inp).unwrap();
            assert_eq!(got, want, "{pass} changed {name} semantics ({} -> {} stmts)", c.blocks[0].stmts.len(), cc.blocks[0].stmts.len());
            eprintln!("{name} {pass}: OK ({} -> {} stmts)", c.blocks[0].stmts.len(), cc.blocks[0].stmts.len());
        }
    }
}

#[test]
fn opt_bisect_step_circuit() {
    use volar_fuzz::interpreter::biir::eval_biir;
    use volar_vc::x25519_gadget::scalar_ref::*;
    let u: Fp = fp_from_bytes(&{ let mut b=[0u8;32]; b[0]=9; b });
    // One step from init state, kt=1, swap=0.
    let mut k = [0u8;32]; k[0]=0x35; k[0]&=248; k[31]&=127; k[31]|=64;
    let kt = (k[254/8] >> (254%8)) & 1 == 1;
    let c = volar_vc::x25519_gadget::build_x25519_step();
    let fe_in = |w: &Fp| volar_vc::tls13_2pc::bits_of(&fp_to_bytes(w))[..255].to_vec();
    let mut inp = Vec::new();
    let mut one = [0u8;32]; one[0]=1;
    let onef = fp_from_bytes(&one);
    let zerof = fp_from_bytes(&[0u8;32]);
    inp.extend(fe_in(&onef)); inp.extend(fe_in(&zerof)); inp.extend(fe_in(&u)); inp.extend(fe_in(&onef)); inp.extend(fe_in(&u));
    inp.push(false); inp.push(kt);
    let want = eval_biir(&c, &inp).unwrap();
    for pass in ["fold", "cse", "dce"] {
        let mut cc = match pass {
            "fold" => { let mut c2 = c.clone(); volar_ir_opt::biir::fold_biir_blocks(&mut c2); c2 }
            "cse" => { let mut c2 = c.clone(); volar_ir_opt::biir::cse_biir_blocks(&mut c2); c2 }
            _ => { let mut c2 = c.clone(); volar_ir_opt::biir::dce_biir_blocks(&mut c2); c2 }
        };
        let got = eval_biir(&cc, &inp).unwrap();
        assert_eq!(got, want, "{pass} changed step semantics ({} -> {} stmts)", c.blocks[0].stmts.len(), cc.blocks[0].stmts.len());
        eprintln!("step {pass}: OK ({} -> {} stmts)", c.blocks[0].stmts.len(), cc.blocks[0].stmts.len());
    }
}


/// The COMBINED fold+cse+dce pipeline on each KX circuit, concretely —
/// mirrors exactly what compile_schedule_optimized runs in the session.
#[test]
fn opt_combined_pipeline_preserves_kx_circuits() {
    use volar_fuzz::interpreter::biir::eval_biir;
    use volar_vc::x25519_gadget::scalar_ref::*;
    let z: Fp = fp_from_bytes(&[0x2Bu8; 32]);
    let x: Fp = fp_from_bytes(&[0x4Du8; 32]);
    let u: Fp = fp_from_bytes(&{ let mut b=[0u8;32]; b[0]=9; b });
    let fe_in = |w: &Fp| volar_vc::tls13_2pc::bits_of(&fp_to_bytes(w))[..255].to_vec();
    let opt = |mut c: volar_ir::boolar::BIrBlocks<()>| {
        volar_ir_opt::biir::fold_biir_blocks(&mut c);
        volar_ir_opt::biir::cse_biir_blocks(&mut c);
        volar_ir_opt::biir::dce_biir_blocks(&mut c);
        c
    };
    // fe_square
    {
        let c = volar_vc::x25519_gadget::build_fe_square();
        let inp = fe_in(&z);
        let want = eval_biir(&c, &inp).unwrap();
        let got = eval_biir(&opt(c.clone()), &inp).unwrap();
        assert_eq!(got, want, "combined broke fe_square");
    }
    // fe_mul
    {
        let c = volar_vc::x25519_gadget::build_fe_mul();
        let mut inp = fe_in(&z); inp.extend(fe_in(&x));
        let want = eval_biir(&c, &inp).unwrap();
        let got = eval_biir(&opt(c.clone()), &inp).unwrap();
        assert_eq!(got, want, "combined broke fe_mul");
    }
    // final_cswap
    {
        let c = volar_vc::x25519_gadget::build_final_cswap();
        let mut inp = vec![true]; inp.extend(fe_in(&z)); inp.extend(fe_in(&x)); inp.extend(fe_in(&x)); inp.extend(fe_in(&z));
        let want = eval_biir(&c, &inp).unwrap();
        let got = eval_biir(&opt(c.clone()), &inp).unwrap();
        assert_eq!(got, want, "combined broke final_cswap");
    }
    // ladder step (init state, kt=1)
    {
        let c = volar_vc::x25519_gadget::build_x25519_step();
        let one: Fp = [1,0,0,0];
        let zer: Fp = [0;4];
        let mut inp = Vec::new();
        inp.extend(fe_in(&one)); inp.extend(fe_in(&zer)); inp.extend(fe_in(&u)); inp.extend(fe_in(&one)); inp.extend(fe_in(&u));
        inp.push(false); inp.push(true);
        let want = eval_biir(&c, &inp).unwrap();
        let got = eval_biir(&opt(c.clone()), &inp).unwrap();
        assert_eq!(got, want, "combined broke x25519_step");
    }
}

/// The full stepped ladder + inversion through the OPTIMIZED circuits,
/// against the RFC 7748 base-point vector — the same value the session's
/// KX must produce.
#[test]
#[ignore = "heavyweight: 255 optimized ladder steps + inversion"]
fn optimized_stepped_ladder_rfc7748() {
    use volar_fuzz::interpreter::biir::eval_biir;
    use volar_vc::x25519_gadget::scalar_ref::*;
    let opt = |mut c: volar_ir::boolar::BIrBlocks<()>| {
        volar_ir_opt::biir::fold_biir_blocks(&mut c);
        volar_ir_opt::biir::cse_biir_blocks(&mut c);
        volar_ir_opt::biir::dce_biir_blocks(&mut c);
        c
    };
    let step = opt(volar_vc::x25519_gadget::build_x25519_step());
    let sq = opt(volar_vc::x25519_gadget::build_fe_square());
    let mul = opt(volar_vc::x25519_gadget::build_fe_mul());
    let cswap = opt(volar_vc::x25519_gadget::build_final_cswap());

    let mut k9 = [0u8; 32]; k9[0] = 9;
    let mut k = k9; k[0] &= 248; k[31] &= 127; k[31] |= 64;
    let u: Fp = fp_from_bytes(&k9);
    let fe_bits = |w: &Fp| volar_vc::tls13_2pc::bits_of(&fp_to_bytes(w))[..255].to_vec();
    let fe_out = |out: &[bool]| -> Fp {
        let mut bytes = [0u8; 32];
        for (i, &b) in out.iter().take(255).enumerate() { if b { bytes[i/8] |= 1 << (i%8); } }
        bytes[31] &= 0x7f;
        fp_from_bytes(&bytes)
    };
    let mut x2: Fp = [1,0,0,0]; let mut z2: Fp = [0;4]; let mut x3: Fp = u; let mut z3: Fp = [1,0,0,0];
    let mut swap = false;
    for i in 0..255 {
        let t = 254 - i;
        let kt = (k[t/8] >> (t%8)) & 1 == 1;
        let mut inp = Vec::with_capacity(1277);
        inp.extend(fe_bits(&x2)); inp.extend(fe_bits(&z2)); inp.extend(fe_bits(&x3)); inp.extend(fe_bits(&z3)); inp.extend(fe_bits(&u));
        inp.push(swap); inp.push(kt);
        let out = eval_biir(&step, &inp).unwrap();
        x2 = fe_out(&out[0..255]); z2 = fe_out(&out[255..510]); x3 = fe_out(&out[510..765]); z3 = fe_out(&out[765..1020]);
        swap = out[1020];
    }
    // final cswap through the circuit
    let mut inp = vec![swap];
    inp.extend(fe_bits(&x2)); inp.extend(fe_bits(&z2)); inp.extend(fe_bits(&x3)); inp.extend(fe_bits(&z3));
    let out = eval_biir(&cswap, &inp).unwrap();
    x2 = fe_out(&out[0..255]); z2 = fe_out(&out[255..510]);
    // inversion chain via sq/mul (same ops as the session driver)
    let fev = |c: &volar_ir::boolar::BIrBlocks<()>, args: &[&Fp]| -> Fp {
        let mut inputs = Vec::new();
        for a in args { inputs.extend(fe_bits(a)); }
        fe_out(&eval_biir(c, &inputs).unwrap())
    };
    let square = |a: &Fp| fev(&sq, &[a]);
    let mult = |a: &Fp, b: &Fp| fev(&mul, &[a, b]);
    let t0 = square(&z2);
    let mut t1 = square(&t0);
    t1 = square(&t1);
    t1 = mult(&z2, &t1);
    let t0 = mult(&t0, &t1);
    let mut t2 = square(&t0);
    t2 = mult(&t1, &t2);
    let mut t1 = t2;
    for _ in 0..5 { t1 = square(&t1); }
    let t1 = mult(&t1, &t2);
    let mut t2 = t1;
    for _ in 0..10 { t2 = square(&t2); }
    let t2 = mult(&t2, &t1);
    let mut t3 = t2;
    for _ in 0..20 { t3 = square(&t3); }
    let t2 = mult(&t3, &t2);
    let mut t3 = t2;
    for _ in 0..10 { t3 = square(&t3); }
    let t1 = mult(&t3, &t1);
    let mut t3 = t1;
    for _ in 0..50 { t3 = square(&t3); }
    let t2 = mult(&t3, &t1);
    let mut t3 = t2;
    for _ in 0..100 { t3 = square(&t3); }
    let t2 = mult(&t3, &t2);
    let mut t2 = t2;
    for _ in 0..50 { t2 = square(&t2); }
    let t1 = mult(&t2, &t1);
    let mut t1 = t1;
    for _ in 0..5 { t1 = square(&t1); }
    let zinv = mult(&t1, &t0);
    let res = mult(&x2, &zinv);
    assert_eq!(
        fp_to_bytes(&res).to_vec(),
        volar_vc::x25519_gadget::scalar_ref::x25519_scalar(&k9, &k9).to_vec(),
        "optimized stepped X25519 base point"
    );
}
