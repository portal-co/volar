use cipher::consts::*;
pub fn mle_eval(values: Array<Galois128, U8>, point: Array<Galois128, U4>) -> Galois128 {
    let mut eq = Array::<Galois128, U8>::default();
    eq[0] = Galois128(1);
    for c in 0..3 {
        let coordinate = point[c];
        for index in 0..(1usize << c) {
            let product = eq[index] * coordinate;
            eq[index] = eq[index] - product;
            eq[index + (1usize << c)] = product;
        }
    }
    let mut out = Galois128(0);
    for i in 0..8 { out = out + values[i] * eq[i]; }
    out
}
pub fn bundled_eval(
    coeffs: Array<Galois128, U48>,
    nvars: Array<usize, U48>,
    tvars: Array<usize, U96>,
    nterms: Array<usize, U16>,
    beta: Array<Galois128, U8>,
    witness: Array<Galois128, U8>,
) -> Galois128 {
    let mut eq = Array::<Galois128, U16>::default();
    eq[0] = Galois128(1);
    for j in 0..4 {
        let coordinate = beta[1 + j];
        for index in 0..(1usize << j) {
            let product = eq[index] * coordinate;
            eq[index] = eq[index] - product;
            eq[index + (1usize << j)] = product;
        }
    }
    let mut out = Galois128(0);
    for c in 0..16 {
        let mut value = Galois128(0);
        for t in 0..nterms[c] {
            let mut product = coeffs[c * 3 + t];
            for v in 0..nvars[c * 3 + t] {
                let var = tvars[(c * 3 + t) * 2 + v];
                let x = if var < 1 { beta[var] } else { witness[var - 1] };
                product = product * x;
            }
            value = value + product;
        }
        out = out + eq[c] * value;
    }
    out
}
pub fn decide_toy(
    coeffs: Array<Galois128, U48>,
    nvars: Array<usize, U48>,
    tvars: Array<usize, U96>,
    nterms: Array<usize, U16>,
    root: Galois128,
    alpha: Array<Galois128, U4>,
    mu: Galois128,
    beta: Array<Galois128, U8>,
    eta: Galois128,
    codeword: Array<Galois128, U8>,
    witness: Array<Galois128, U8>,
) -> bool {
    let a = keccak_pair(codeword[0], codeword[1]);
    let b = keccak_pair(codeword[2], codeword[3]);
    let c = keccak_pair(codeword[4], codeword[5]);
    let d = keccak_pair(codeword[6], codeword[7]);
    let e = keccak_pair(a, b);
    let f = keccak_pair(c, d);
    let computed_root = keccak_pair(e, f);
    if computed_root != root { return false; }
    for i in 0..8 {
        if witness[i] != codeword[i] { return false; }
    }
    let mle = mle_eval(codeword, alpha);
    if mle != mu { return false; }
    let bundled = bundled_eval(coeffs, nvars, tvars, nterms, beta, witness);
    if bundled != eta { return false; }
    true
}
