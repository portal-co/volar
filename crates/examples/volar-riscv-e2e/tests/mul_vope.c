/* Vope polynomial multiplication over GF(2^8): the GF(2^8)-mul analogue of
 * mul_generalized (volar-spec vope/ai_hazmat.rs). a: Vope<N=16, K=ka>
 * (u: [16] rows of 16, v: 16), b: K=kb; result K=ka+kb — u rows [ka+kb],
 * v row. Coefficients are Galois bytes; every multiply is a GF(2^8) mul,
 * every add an XOR. This is the GF-field-accurate form (the lowered C's
 * `mul__Vope_mem_probe` extern exists because the weave's own lowering
 * decided the Rust `*` operator on Vope needed the host; here we supply
 * the same polynomial in the field the harness commits bits in). */
#include <stdint.h>
#include <string.h>
#include "header.h"

extern uint8_t volar_gf8_mul(uint8_t a, uint8_t b); /* in header.c of the TU */

static uint8_t gf8_mul_local(uint8_t a, uint8_t b) {
    /* same carry-less multiply as the generated header's volar_gf8_mul */
    uint8_t p = 0;
    for (int i = 0; i < 8; i++) {
        if (b & 1) p ^= a;
        uint8_t hi = a & 0x80;
        a = (uint8_t)(a << 1);
        if (hi) a ^= 0x1b;
        b >>= 1;
    }
    return p;
}

/* a = mul__Vope_mem_probe(x, y): x is K=1 (16 u-rows? no — K=1 means
 * u has 1 row), y is K=1 → result K=2. The generic K=1×K=1 case is the
 * only one the weave planned instances for here (both operands K=1). */
Vope mul__Vope_mem_probe(Vope x, Vope y) {
    Vope r;
    memset(&r, 0, sizeof r);
    const int N = 16;
    /* deg(x) = 1 (u[0] + v), deg(y) = 1 → result deg 2: u rows 0..1, v */
    for (int i = 0; i <= 1; i++) {
        for (int j = 0; j <= 1; j++) {
            int k = i + j;
            const uint8_t *a = (i == 0) ? x.v.data : x.u.data[i - 1].data;
            const uint8_t *b = (j == 0) ? y.v.data : y.u.data[j - 1].data;
            uint8_t *dst = (k == 0) ? r.v.data : r.u.data[k - 1].data;
            for (int lane = 0; lane < N; lane++) {
                dst[lane] ^= gf8_mul_local(a[lane], b[lane]);
            }
        }
    }
    return r;
}
