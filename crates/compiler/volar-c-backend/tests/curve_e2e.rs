// @reliability: experimental
//! End-to-end test: Chou-Orlandi base OT using Ed25519 arithmetic in C.
//!
//! # Strategy
//!
//! The curve module (`curve.rs`) uses `u128` arithmetic and generic `Digest`
//! bounds that cannot be lowered through the current IR pipeline:
//!
//! - `u128` multiplications in `mul_4x4` / `reduce_wide` would be silently
//!   truncated to `i64` by `primitive_to_lir` (PrimitiveType::U128 → LirType::I64),
//!   producing incorrect field arithmetic.
//! - Generic `D: Digest` constraints on `write_element`, `ot_send_finish`,
//!   `ot_recv_finish` are not representable in the IR type system.
//!
//! Blocker: `u128` → `I64` truncation in `primitive_to_lir` makes field
//! multiplication wrong; fixing it requires adding `LirType::U128` and
//! `__uint128_t` emission to the C backend (future work).
//!
//! # Workaround
//!
//! Implement the Ed25519 arithmetic and Chou-Orlandi OT protocol directly in C,
//! matching the spec exactly:
//!
//! - `Fe25519` as `uint64_t[4]` with `__uint128_t` intermediate products.
//! - Extended Edwards operations (`ep_add`, `ep_double`, `ep_scalar_mul`).
//! - A deterministic hash of a curve point to 32 bytes (SplitMix64-based;
//!   not SHA-256 but sufficient for OT correctness testing).
//! - The full Chou-Orlandi `SimplestOT` exchange for c=0 and c=1.
//!
//! The main test asserts that for c=0 the receiver's k_c equals sender's k_0,
//! and for c=1 the receiver's k_c equals sender's k_1.
//! This mirrors `curve::tests::ed25519_chou_orlandi_round_trip` in the spec.

use volar_lir_test_corpus::compile_and_run;

// ============================================================================
// C source: Ed25519 arithmetic + Chou-Orlandi OT
// ============================================================================

/// Complete C99+extensions implementation of Ed25519 arithmetic and OT.
/// Uses `__uint128_t` (GCC/clang) for 128-bit intermediate products.
const C_ED25519_SRC: &str = r##"
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* =========================================================================
 * Fe25519: field element mod p = 2^255 - 19.
 * 4 little-endian uint64_t limbs, canonical form (< p).
 * ========================================================================= */

typedef uint64_t Fe25519[4];

static const uint64_t FE_P[4] = {
    0xFFFFFFFFFFFFFFEDull,
    0xFFFFFFFFFFFFFFFFull,
    0xFFFFFFFFFFFFFFFFull,
    0x7FFFFFFFFFFFFFFFull,
};

/* Conditionally subtract p if a >= p. Inputs in [0, 2p). */
static void fe_cond_sub_p(uint64_t r[4]) {
    uint64_t tmp[4];
    uint64_t borrow = 0;
    int i;
    for (i = 0; i < 4; i++) {
        uint64_t r1 = r[i] - FE_P[i];
        uint64_t b1 = (r1 > r[i]) ? 1u : 0u;
        uint64_t r2 = r1 - borrow;
        uint64_t b2 = (r2 > r1) ? 1u : 0u;
        tmp[i] = r2;
        borrow = b1 | b2;
    }
    if (!borrow) {
        r[0]=tmp[0]; r[1]=tmp[1]; r[2]=tmp[2]; r[3]=tmp[3];
    }
}

/* Bring into canonical [0,p) form — call up to twice after add/reduce. */
static void fe_canonicalize(uint64_t r[4]) {
    fe_cond_sub_p(r);
    fe_cond_sub_p(r);
}

static void fe_copy(Fe25519 dst, const Fe25519 src) {
    dst[0]=src[0]; dst[1]=src[1]; dst[2]=src[2]; dst[3]=src[3];
}
static void fe_zero(Fe25519 a) { a[0]=a[1]=a[2]=a[3]=0; }
static void fe_one(Fe25519 a)  { a[0]=1; a[1]=a[2]=a[3]=0; }
static int  fe_is_zero(const Fe25519 a) { return (a[0]|a[1]|a[2]|a[3])==0; }
static int  fe_eq(const Fe25519 a, const Fe25519 b) {
    return (a[0]==b[0])&(a[1]==b[1])&(a[2]==b[2])&(a[3]==b[3]);
}

static void fe_add(Fe25519 r, const Fe25519 a, const Fe25519 b) {
    __uint128_t carry = 0;
    int i;
    for (i = 0; i < 4; i++) {
        __uint128_t v = (__uint128_t)a[i] + b[i] + carry;
        r[i] = (uint64_t)v;
        carry = v >> 64;
    }
    /* carry in {0,1}: fold via 2^256 ≡ 38 mod p */
    if (carry) {
        __uint128_t c = carry * 38;
        for (i = 0; i < 4; i++) {
            __uint128_t v = (__uint128_t)r[i] + c;
            r[i] = (uint64_t)v;
            c = v >> 64;
        }
    }
    fe_canonicalize(r);
}

static void fe_sub(Fe25519 r, const Fe25519 a, const Fe25519 b) {
    /* a - b = a + (p - b) mod p */
    uint64_t neg_b[4];
    uint64_t borrow = 0;
    int i;
    for (i = 0; i < 4; i++) {
        uint64_t r1 = FE_P[i] - b[i];
        uint64_t borrow1 = (r1 > FE_P[i]) ? 1u : 0u;
        uint64_t r2 = r1 - borrow;
        uint64_t borrow2 = (r2 > r1) ? 1u : 0u;
        neg_b[i] = r2;
        borrow = borrow1 | borrow2;
    }
    fe_add(r, a, neg_b);
}

static void fe_neg(Fe25519 r, const Fe25519 a) {
    if (fe_is_zero(a)) { fe_zero(r); return; }
    uint64_t borrow = 0;
    int i;
    for (i = 0; i < 4; i++) {
        uint64_t r1 = FE_P[i] - a[i];
        uint64_t borrow1 = (r1 > FE_P[i]) ? 1u : 0u;
        uint64_t r2 = r1 - borrow;
        uint64_t borrow2 = (r2 > r1) ? 1u : 0u;
        r[i] = r2;
        borrow = borrow1 | borrow2;
    }
}

/* Schoolbook 4x4 → 8-limb, then reduce mod p via 2^256 ≡ 38. */
static void fe_mul(Fe25519 r, const Fe25519 a, const Fe25519 b) {
    uint64_t wide[8];
    memset(wide, 0, sizeof(wide));
    int i, j;
    for (i = 0; i < 4; i++) {
        uint64_t carry = 0;
        for (j = 0; j < 4; j++) {
            __uint128_t v = (__uint128_t)a[i] * b[j] + wide[i+j] + carry;
            wide[i+j] = (uint64_t)v;
            carry = (uint64_t)(v >> 64);
        }
        wide[i+4] += carry;
    }
    /* Step 1: acc[0..4] = wide[0..4] + 38*wide[4..8] */
    uint64_t acc[5];
    __uint128_t c = 0;
    for (i = 0; i < 4; i++) {
        __uint128_t v = (__uint128_t)wide[i] + (__uint128_t)wide[4+i]*38 + c;
        acc[i] = (uint64_t)v;
        c = v >> 64;
    }
    acc[4] = (uint64_t)c;
    /* Step 2: fold acc[4]*38 back in */
    c = (__uint128_t)acc[4] * 38;
    for (i = 0; i < 4; i++) {
        __uint128_t v = (__uint128_t)acc[i] + c;
        r[i] = (uint64_t)v;
        c = v >> 64;
    }
    if (c) {
        __uint128_t c2 = c * 38;
        for (i = 0; i < 4; i++) {
            __uint128_t v = (__uint128_t)r[i] + c2;
            r[i] = (uint64_t)v;
            c2 = v >> 64;
        }
    }
    fe_canonicalize(r);
}

static void fe_sq(Fe25519 r, const Fe25519 a) { fe_mul(r, a, a); }

/* a^(p-2) mod p via binary square-and-multiply. */
static void fe_invert(Fe25519 r, const Fe25519 a) {
    static const uint64_t EXP[4] = {
        0xFFFFFFFFFFFFFFEBull,
        0xFFFFFFFFFFFFFFFFull,
        0xFFFFFFFFFFFFFFFFull,
        0x7FFFFFFFFFFFFFFFull,
    };
    Fe25519 acc;
    fe_one(acc);
    int li, bit;
    for (li = 3; li >= 0; li--) {
        for (bit = 63; bit >= 0; bit--) {
            fe_sq(acc, acc);
            if ((EXP[li] >> bit) & 1) {
                Fe25519 tmp;
                fe_mul(tmp, acc, a);
                fe_copy(acc, tmp);
            }
        }
    }
    fe_copy(r, acc);
}

/* =========================================================================
 * EdPoint: extended twisted-Edwards coordinates (X:Y:Z:T).
 * Identity = (0:1:1:0).
 * ========================================================================= */

typedef struct { Fe25519 x, y, z, t; } EdPoint;

/* Twisted-Edwards d = -121665/121666 mod p */
static const uint64_t ED_D[4] = {
    0x75EB4DCA135978A3ull,
    0x00700A4D4141D8ABull,
    0x8CC740797779E898ull,
    0x52036CEE2B6FFE73ull,
};
/* 2*d mod p */
static const uint64_t ED_D2[4] = {
    0xEBD69B9426B2F159ull,
    0x00E0149A8283B156ull,
    0x198E80F2EEF3D130ull,
    0x2406D9DC56DFFCE7ull,
};

/* Ed25519 base point (X, Y), Z=1. From RFC 8032 / curve.rs. */
static const uint64_t BASE_X[4] = {
    0xC9562D608F25D51Aull,
    0x692CC7609525A7B2ull,
    0xC0A4E231FDD6DC5Cull,
    0x216936D3CD6E53FEull,
};
static const uint64_t BASE_Y[4] = {
    0x6666666666666658ull,
    0x6666666666666666ull,
    0x6666666666666666ull,
    0x6666666666666666ull,
};

static void ep_identity(EdPoint *p) {
    fe_zero(p->x); fe_one(p->y); fe_one(p->z); fe_zero(p->t);
}
static void ep_base(EdPoint *p) {
    fe_copy(p->x, BASE_X);
    fe_copy(p->y, BASE_Y);
    fe_one(p->z);
    fe_mul(p->t, p->x, p->y);
}
static int ep_eq(const EdPoint *a, const EdPoint *b) {
    Fe25519 lx, rx, ly, ry;
    fe_mul(lx, a->x, b->z);
    fe_mul(rx, b->x, a->z);
    fe_mul(ly, a->y, b->z);
    fe_mul(ry, b->y, a->z);
    return fe_eq(lx, rx) & fe_eq(ly, ry);
}

/* Hisil-Wong-Carter-Dawson unified addition, a = -1. */
static void ep_add(EdPoint *r, const EdPoint *p1, const EdPoint *p2) {
    Fe25519 A, B, C, Dv, E, F, G, H, tmp1, tmp2;
    /* A = (Y1-X1)*(Y2-X2) */
    fe_sub(tmp1, p1->y, p1->x);
    fe_sub(tmp2, p2->y, p2->x);
    fe_mul(A, tmp1, tmp2);
    /* B = (Y1+X1)*(Y2+X2) */
    fe_add(tmp1, p1->y, p1->x);
    fe_add(tmp2, p2->y, p2->x);
    fe_mul(B, tmp1, tmp2);
    /* C = T1 * 2d * T2 */
    fe_mul(tmp1, p1->t, ED_D2);
    fe_mul(C, tmp1, p2->t);
    /* D = 2*Z1*Z2 */
    fe_add(tmp1, p1->z, p1->z);   /* 2*Z1 */
    fe_mul(Dv, tmp1, p2->z);       /* 2*Z1*Z2 */
    /* E=B-A, F=D-C, G=D+C, H=B+A */
    fe_sub(E, B, A);
    fe_sub(F, Dv, C);
    fe_add(G, Dv, C);
    fe_add(H, B, A);
    fe_mul(r->x, E, F);
    fe_mul(r->y, G, H);
    fe_mul(r->t, E, H);
    fe_mul(r->z, F, G);
}

/* Point doubling for a = -1. */
static void ep_double(EdPoint *r, const EdPoint *p) {
    Fe25519 A, B, C, Dv, E, F, G, H, xy_sum, tmp;
    fe_sq(A, p->x);            /* A = X^2 */
    fe_sq(B, p->y);            /* B = Y^2 */
    fe_sq(tmp, p->z);
    fe_add(C, tmp, tmp);       /* C = 2*Z^2 */
    fe_neg(Dv, A);             /* D = -A  (a=-1) */
    /* E = (X+Y)^2 - A - B */
    fe_add(xy_sum, p->x, p->y);
    fe_sq(tmp, xy_sum);
    fe_sub(tmp, tmp, A);
    fe_sub(E, tmp, B);
    /* G = D + B */
    fe_add(G, Dv, B);
    /* F = G - C */
    fe_sub(F, G, C);
    /* H = D - B */
    fe_sub(H, Dv, B);
    fe_mul(r->x, E, F);
    fe_mul(r->y, G, H);
    fe_mul(r->t, E, H);
    fe_mul(r->z, F, G);
}

static void ep_neg(EdPoint *r, const EdPoint *p) {
    fe_neg(r->x, p->x);
    fe_copy(r->y, p->y);
    fe_copy(r->z, p->z);
    fe_neg(r->t, p->t);
}

/* Double-and-add scalar multiplication. k = 32 LE bytes. */
static void ep_scalar_mul(EdPoint *r, const EdPoint *p, const uint8_t k[32]) {
    ep_identity(r);
    int bi, bit;
    for (bi = 31; bi >= 0; bi--) {
        for (bit = 7; bit >= 0; bit--) {
            EdPoint tmp;
            ep_double(&tmp, r);
            *r = tmp;
            if ((k[bi] >> bit) & 1) {
                ep_add(&tmp, r, p);
                *r = tmp;
            }
        }
    }
}

/* =========================================================================
 * Hash an EdPoint to 32 bytes.
 *
 * We use a custom mixing function instead of SHA-256 to keep the C
 * self-contained.  The hash just needs to be collision-resistant for
 * testing — same point → same bytes, distinct points → distinct bytes
 * with overwhelming probability.
 *
 * Construction: hash affine (x||y) via a SplitMix64-like mixing pass.
 * ========================================================================= */

static void fe_to_bytes(uint8_t out[32], const Fe25519 a) {
    int i, j;
    for (i = 0; i < 4; i++) {
        uint64_t v = a[i];
        for (j = 0; j < 8; j++) {
            out[i*8+j] = (uint8_t)(v & 0xff);
            v >>= 8;
        }
    }
}

static uint64_t splitmix64_mix(uint64_t z) {
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
    return z ^ (z >> 31);
}

static void hash_point(uint8_t out[32], const EdPoint *p) {
    /* Affine coords */
    Fe25519 zinv, ax, ay;
    fe_invert(zinv, p->z);
    fe_mul(ax, p->x, zinv);
    fe_mul(ay, p->y, zinv);
    uint8_t xb[32], yb[32];
    fe_to_bytes(xb, ax);
    fe_to_bytes(yb, ay);

    /* Mix each pair of u64 limbs from x and y together. */
    uint64_t h[4];
    int i;
    for (i = 0; i < 4; i++) {
        uint64_t xi, yi;
        memcpy(&xi, xb + i*8, 8);
        memcpy(&yi, yb + i*8, 8);
        /* Domain-separate each limb position with a constant derived from pi. */
        uint64_t domain = 0x3141592653589793ull ^ ((uint64_t)i * 0x9E3779B97F4A7C15ull);
        h[i] = splitmix64_mix(xi ^ domain) ^ splitmix64_mix(yi ^ ~domain);
        /* Cross-mix with previous limb for avalanche. */
        if (i > 0) h[i] ^= splitmix64_mix(h[i-1]);
    }
    /* Final avalanche pass. */
    for (i = 0; i < 4; i++) {
        h[i] = splitmix64_mix(h[i] ^ h[(i+3)%4]);
    }
    memcpy(out, h, 32);
}

/* =========================================================================
 * SplitMix64 RNG — matches the deterministic TestRng in spec tests.
 * ========================================================================= */
static uint64_t sm_state_g;
static void sm_seed(uint64_t seed) { sm_state_g = seed; }
static uint64_t sm_next64(void) {
    sm_state_g += 0x9E3779B97F4A7C15ull;
    uint64_t z = sm_state_g;
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
    return z ^ (z >> 31);
}
static void random_scalar(uint8_t k[32]) {
    int i, j;
    for (i = 0; i < 4; i++) {
        uint64_t w = sm_next64();
        for (j = 0; j < 8; j++) {
            k[i*8+j] = (uint8_t)(w & 0xff);
            w >>= 8;
        }
    }
    k[31] &= 0x3F; /* clamp top 2 bits */
}

/* =========================================================================
 * 32-byte key comparison.
 * ========================================================================= */
typedef struct { uint8_t bytes[32]; } OtKey;
static int otkey_eq(const OtKey *a, const OtKey *b) {
    return memcmp(a->bytes, b->bytes, 32) == 0;
}

/* =========================================================================
 * Chou-Orlandi "SimplestOT" base OT.
 *
 * 1. Sender:  y ← $,  S = g^y,  T = S^y;  → send S.
 * 2. Receiver: x ← $, R = (c==0) ? g^x : S + g^x;  k_c = H(S^x); → send R.
 * 3. Sender:  k_0 = H(R^y);  k_1 = H((R + (-S))^y).
 *
 * Returns 1 if receiver's k_c == sender's k_c.
 * ========================================================================= */
static int run_ot(int c) {
    EdPoint G;
    ep_base(&G);

    /* Sender setup */
    uint8_t y[32];
    random_scalar(y);
    EdPoint S;
    ep_scalar_mul(&S, &G, y);
    /* T = S^y — not needed for correctness check but mirrors protocol */

    /* Receiver */
    uint8_t x[32];
    random_scalar(x);
    EdPoint Gx, R;
    ep_scalar_mul(&Gx, &G, x);
    if (c) {
        ep_add(&R, &S, &Gx);
    } else {
        R = Gx;
    }
    /* Receiver key: k_c = H(S^x) */
    EdPoint Sx;
    ep_scalar_mul(&Sx, &S, x);
    OtKey k_c;
    hash_point(k_c.bytes, &Sx);

    /* Sender finish */
    EdPoint Ry;
    ep_scalar_mul(&Ry, &R, y);
    OtKey k_0;
    hash_point(k_0.bytes, &Ry);

    EdPoint neg_S, R_minus_S, RmS_y;
    ep_neg(&neg_S, &S);
    ep_add(&R_minus_S, &R, &neg_S);
    ep_scalar_mul(&RmS_y, &R_minus_S, y);
    OtKey k_1;
    hash_point(k_1.bytes, &RmS_y);

    if (c == 0) return otkey_eq(&k_c, &k_0);
    else        return otkey_eq(&k_c, &k_1);
}
"##;

/// C main: seed RNG, run OT for c=0 and c=1, print pass count.
const C_OT_MAIN: &str = r#"
  sm_seed(0xABADCAFEDEADBEEFull);
  int pass = 0;
  if (run_ot(0)) pass++;
  if (run_ot(1)) pass++;
  printf("%d/2\n", pass);
"#;

// ============================================================================
// Tests
// ============================================================================

/// Chou-Orlandi base OT over Ed25519, implemented directly in C.
///
/// Verifies that receiver's k_c equals sender's k_0 (c=0) and k_1 (c=1).
/// Uses a deterministic RNG so the test is reproducible.
///
/// Lowering blocker: `u128` → `LirType::I64` truncation in `primitive_to_lir`
/// makes `fe_mul` incorrect when generated from IR. The C implementation here
/// is the workaround, manually porting the spec's arithmetic.
#[test]
fn ed25519_chou_orlandi_e2e() {
    let out = compile_and_run(C_ED25519_SRC, C_OT_MAIN);
    assert_eq!(
        out.trim(),
        "2/2",
        "Chou-Orlandi OT should pass for both c=0 and c=1. Got: {}",
        out.trim()
    );
}

/// Sanity: base-point satisfies −x² + y² = 1 + d·x²·y².
#[test]
fn ed25519_base_point_on_curve_c() {
    let verify_main = r##"
  EdPoint g; ep_base(&g);
  Fe25519 zinv, ax, ay;
  fe_invert(zinv, g.z);
  fe_mul(ax, g.x, zinv);
  fe_mul(ay, g.y, zinv);

  Fe25519 xx, yy, lhs;
  fe_sq(xx, ax);
  fe_sq(yy, ay);
  fe_sub(lhs, yy, xx); /* y^2 - x^2 */

  Fe25519 one; fe_one(one);
  Fe25519 d;
  d[0]=0x75EB4DCA135978A3ull; d[1]=0x00700A4D4141D8ABull;
  d[2]=0x8CC740797779E898ull; d[3]=0x52036CEE2B6FFE73ull;
  Fe25519 xxyy, dxxyy, rhs;
  fe_mul(xxyy, xx, yy);
  fe_mul(dxxyy, d, xxyy);
  fe_add(rhs, one, dxxyy);

  printf("%d\n", fe_eq(lhs, rhs));
"##;
    let out = compile_and_run(C_ED25519_SRC, verify_main);
    assert_eq!(out.trim(), "1", "base point must satisfy the Ed25519 curve equation");
}

/// Sanity: [1]G == G.
#[test]
fn ed25519_scalar_mul_one_c() {
    let verify_main = r#"
  EdPoint g, r; ep_base(&g);
  uint8_t k[32]; memset(k, 0, 32); k[0] = 1;
  ep_scalar_mul(&r, &g, k);
  printf("%d\n", ep_eq(&r, &g));
"#;
    let out = compile_and_run(C_ED25519_SRC, verify_main);
    assert_eq!(out.trim(), "1", "[1]G must equal G");
}

/// Sanity: ep_double(G) == ep_add(G, G).
#[test]
fn ed25519_double_eq_add_self_c() {
    let verify_main = r#"
  EdPoint g, d, a; ep_base(&g);
  ep_double(&d, &g);
  ep_add(&a, &g, &g);
  printf("%d\n", ep_eq(&d, &a));
"#;
    let out = compile_and_run(C_ED25519_SRC, verify_main);
    assert_eq!(out.trim(), "1", "ep_double(G) must equal ep_add(G,G)");
}

/// Sanity: G + (−G) == identity.
#[test]
fn ed25519_neg_inverse_c() {
    let verify_main = r#"
  EdPoint g, ng, s; ep_base(&g);
  ep_neg(&ng, &g);
  ep_add(&s, &g, &ng);
  EdPoint id; ep_identity(&id);
  printf("%d\n", ep_eq(&s, &id));
"#;
    let out = compile_and_run(C_ED25519_SRC, verify_main);
    assert_eq!(out.trim(), "1", "G + (-G) must equal identity");
}

/// Sanity: (G^x)^y == (G^y)^x (DH correctness).
#[test]
fn ed25519_dh_correctness_c() {
    let verify_main = r#"
  EdPoint g; ep_base(&g);
  uint8_t x[32], y[32];
  memset(x, 0, 32); memset(y, 0, 32);
  x[0]=0x12; x[1]=0x34;
  y[0]=0x56; y[1]=0x78;
  EdPoint gx, gy, gxy_a, gxy_b;
  ep_scalar_mul(&gx, &g, x);
  ep_scalar_mul(&gy, &g, y);
  ep_scalar_mul(&gxy_a, &gx, y);
  ep_scalar_mul(&gxy_b, &gy, x);
  printf("%d\n", ep_eq(&gxy_a, &gxy_b));
"#;
    let out = compile_and_run(C_ED25519_SRC, verify_main);
    assert_eq!(out.trim(), "1", "(G^x)^y must equal (G^y)^x");
}
