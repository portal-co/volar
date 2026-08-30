/* M1 C interaction harness (consumer side).
 *
 * Links against the lowered prover/QSim/verifier role functions
 * (per-function TUs split from the generated role module) and drives the
 * REAL per-step interaction for the mem-probe circuit:
 *
 *   per step: for each block (in boundary order) call the prover block fn,
 *   feed its hats to the qsim block fn, feed hats + qsim's q_and + fresh
 *   per-gate fold challenges to the verifier block fn, and thread the
 *   verifier's all_ok/fold_state — then the chunk fns over the same
 *   exports with the running accumulator, then the finish fns whose
 *   outputs become the next step's entry state. This is exactly
 *   `generate_split_step_ir`'s call sequence for this circuit (7 blocks,
 *   chunk_size 2, one finish), hand-threaded from the structural facts the
 *   `export_declared_and_synth_idle` gate pins.
 *
 * Host-side crypto (mirroring the spec, GF(2^8) AES poly):
 *   - vole_commit_bit: the ideal C-OT — r0 uniform, v = r0 + b·Δ, q = r0,
 *     u = lift(b). Prover gets (Vope{u,v}), verifier gets Q{q}.
 *   - oracle_iop_fold_gate: the IOP fold accumulator, native GF(2^128)
 *     tower (volar-iop field.rs's Ext chain with the verified betas),
 *     folding each gate's AND-check witness via the relaxed-R1CS cross
 *     term. The final state is checked against `is_satisfied_relaxed`.
 *   - r_and challenges: Gf128::from_u64 of the driver's deterministic seed
 *     formula ((step*10^7 + gate_seed*10^5) * 1_000_003 + k).
 *   - invert_mem_probe: GF(2^8) inverse (a^254 square-and-multiply).
 *
 * All checks fail loudly (exit 1) — an honest run prints STEP k OK lines
 * and a final FOLD VERIFIED line.
 */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "header.h"

/* challenge-array types (Arr_S1_N for N challenges) */
typedef struct { IopChallenge data[1096]; } Arr_S1_1096;
typedef struct { IopChallenge data[168]; } Arr_S1_168;
typedef struct { IopChallenge data[68]; } Arr_S1_68;
typedef struct { IopChallenge data[2]; } Arr_S1_2x; /* real Arr_S1_2 exists in header */


/* ================= deterministic RNG (xorshift64) ================= */
static uint64_t g_rng = 0xC0FFEEC0FFEEull;
static uint64_t rng_next_u64(void) {
    g_rng ^= g_rng << 13;
    g_rng ^= g_rng >> 7;
    g_rng ^= g_rng << 17;
    return g_rng;
}
static uint8_t rng_byte(void) { return (uint8_t)(rng_next_u64() & 0xff); }

/* ================= GF(2^8), AES poly 0x1b ================= */
static uint8_t h_gf8_mul(uint8_t a, uint8_t b) {
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
/* GF(2^8) inverse = a^254 (square-and-multiply over exponent 1111 1110). */
static uint8_t h_gf8_inv(uint8_t a) {
    if (a == 0) return 0;
    uint8_t r = 1;
    for (int i = 0; i < 7; i++) {
        r = h_gf8_mul(r, r); /* square */
        r = h_gf8_mul(r, a);
    }
    r = h_gf8_mul(r, r); /* final square: a^254 */
    return r;
}
uint8_t invert_mem_probe(uint8_t a) { return h_gf8_inv(a); }

/* ================= GF(2^128): volar-iop's 4-level tower =================
 * Ext<F,B>: elements lo + hi*x over F[x]/(x^2 + x + BETA).
 *   mul: lo = ll + hh*B ; hi = l1h2 + h1l2 + hh   (x^2 = x + B)
 * Betas (field.rs): B0 = 0x20 (as Galois); B1 = (0, B0); B2 = (0, B1);
 * B3 = (0, B2). */
typedef struct { uint8_t v; } G8;
typedef struct { G8 lo, hi; } G16;
typedef struct { G16 lo, hi; } G32;
typedef struct { G32 lo, hi; } G64;
typedef struct { G64 lo, hi; } G128;

static G8 g8(uint8_t b) { G8 r = {b}; return r; }
static G8 g8_add(G8 a, G8 b) { G8 r = {a.v ^ b.v}; return r; }
static G8 g8_mul(G8 a, G8 b) { G8 r = {h_gf8_mul(a.v, b.v)}; return r; }

/* beta constants — each level's beta is the previous level's beta shifted
 * into the high half (field.rs). */
static const G8 BETA8 = {0x20};
static G16 beta16(void) { G16 b = {g8(0), BETA8}; return b; }
static G32 beta32(void) { G32 b; memset(&b, 0, sizeof b); b.hi = beta16(); return b; }
static G64 beta64(void) { G64 b; memset(&b, 0, sizeof b); b.hi = beta32(); return b; }
static G128 beta128(void) { G128 b; memset(&b, 0, sizeof b); b.hi = beta64(); return b; }

static G16 g16_add(G16 a, G16 b) { G16 r = {g8_add(a.lo, b.lo), g8_add(a.hi, b.hi)}; return r; }
static G16 g16_mul(G16 a, G16 b) {
    G8 ll = g8_mul(a.lo, b.lo);
    G8 hh = g8_mul(a.hi, b.hi);
    G8 cr1 = g8_mul(a.lo, b.hi);
    G8 cr2 = g8_mul(a.hi, b.lo);
    G16 r;
    r.lo = g8_add(ll, g8_mul(hh, BETA8));
    r.hi = g8_add(g8_add(cr1, cr2), hh);
    return r;
}
static G32 g32_add(G32 a, G32 b) { G32 r = {g16_add(a.lo, b.lo), g16_add(a.hi, b.hi)}; return r; }
static G32 g32_mul(G32 a, G32 b) {
    G16 ll = g16_mul(a.lo, b.lo);
    G16 hh = g16_mul(a.hi, b.hi);
    G16 beta = beta16();
    G16 cr1 = g16_mul(a.lo, b.hi);
    G16 cr2 = g16_mul(a.hi, b.lo);
    G32 r;
    r.lo = g16_add(ll, g16_mul(hh, beta));
    r.hi = g16_add(g16_add(cr1, cr2), hh);
    return r;
}
static G64 g64_add(G64 a, G64 b) { G64 r = {g32_add(a.lo, b.lo), g32_add(a.hi, b.hi)}; return r; }
static G64 g64_mul(G64 a, G64 b) {
    G32 ll = g32_mul(a.lo, b.lo);
    G32 hh = g32_mul(a.hi, b.hi);
    G32 beta = beta32();
    G32 cr1 = g32_mul(a.lo, b.hi);
    G32 cr2 = g32_mul(a.hi, b.lo);
    G64 r;
    r.lo = g32_add(ll, g32_mul(hh, beta));
    r.hi = g32_add(g32_add(cr1, cr2), hh);
    return r;
}
static G128 g128_add(G128 a, G128 b) { G128 r = {g64_add(a.lo, b.lo), g64_add(a.hi, b.hi)}; return r; }
static G128 g128_mul(G128 a, G128 b) {
    G64 ll = g64_mul(a.lo, b.lo);
    G64 hh = g64_mul(a.hi, b.hi);
    G64 beta = beta64();
    G64 cr1 = g64_mul(a.lo, b.hi);
    G64 cr2 = g64_mul(a.hi, b.lo);
    G128 r;
    r.lo = g64_add(ll, g64_mul(hh, beta));
    r.hi = g64_add(g64_add(cr1, cr2), hh);
    return r;
}
static G128 g128_mulb(G128 a, G128 b) { return g128_mul(a, b); }


static G128 g128_zero(void) { G128 z; memset(&z, 0, sizeof z); return z; }
static G128 g128_one(void) {
    G128 z = g128_zero();
    z.lo.lo.lo.lo = g8(1);
    return z;
}
static bool g128_eq(G128 a, G128 b) { return memcmp(&a, &b, sizeof a) == 0; }
static G128 g128_from_byte(uint8_t x) {
    G128 z = g128_zero();
    z.lo.lo.lo.lo = g8(x);
    return z;
}
/* Gf128::from_u64: little-endian bytes of n in the low slots (injective
 * embedding — transcript.rs's default). */
static G128 g128_from_u64(uint64_t n) {
    G128 z = g128_zero();
    uint8_t *bytes = (uint8_t *)&z; /* struct layout is the byte layout */
    for (int i = 0; i < 8; i++) bytes[i] = (uint8_t)((n >> (8 * i)) & 0xff);
    return z;
}
static G128 g128_from_bytes(const uint8_t *b) {
    G128 z;
    memcpy(&z, b, 16);
    return z;
}
static void g128_to_bytes(G128 x, uint8_t *out) { memcpy(out, &x, 16); }

/* ================= IOP fold accumulator =================
 * IopAccumulator's C ABI: 208 opaque bytes. Harness layout:
 *   bytes[0]     = fresh flag (0 = carries (W,E,u), 1 = fresh)
 *   bytes[1..]   = W[7] then E[3] then u, 16 bytes each (177 total) */
#define FOLD_W 7
#define FOLD_E 3
typedef struct {
    bool fresh;
    G128 w[FOLD_W];
    G128 e[FOLD_E];
    G128 u;
} FoldState;

static void foldstate_pack(const FoldState *s, IopAccumulator *out) {
    memset(out->bytes.data, 0, sizeof out->bytes.data);
    out->bytes.data[0] = s->fresh ? 1 : 0;
    uint8_t *p = out->bytes.data + 1;
    for (int i = 0; i < FOLD_W; i++, p += 16) g128_to_bytes(s->w[i], p);
    for (int i = 0; i < FOLD_E; i++, p += 16) g128_to_bytes(s->e[i], p);
    g128_to_bytes(s->u, p);
}
static FoldState foldstate_unpack(const IopAccumulator *in) {
    FoldState s;
    memset(&s, 0, sizeof s);
    s.fresh = in->bytes.data[0] != 0;
    const uint8_t *p = in->bytes.data + 1;
    for (int i = 0; i < FOLD_W; i++, p += 16) s.w[i] = g128_from_bytes(p);
    for (int i = 0; i < FOLD_E; i++, p += 16) s.e[i] = g128_from_bytes(p);
    s.u = g128_from_bytes(p);
    return s;
}

/* and_check_r1cs (fold.rs): 3 constraints over z = [w(7) || u].
 *   row0: A: K_A ; B: K_B ; C: P1
 *   row1: A: K_C ; B: DELTA ; C: P2
 *   row2: A: P1 + V_HAT - P2 ; B: U ; C: 0
 * Columns: K_A=0 K_B=1 K_C=2 DELTA=3 V_HAT=4 P1=5 P2=6 U=7. */
typedef enum { C_K_A = 0, C_K_B, C_K_C, C_DELTA, C_VHAT, C_P1, C_P2 } Col;

static G128 az_of(int row, const G128 *z) {
    switch (row) {
        case 0: return z[C_K_A];
        case 1: return z[C_K_C];
        default: return g128_add(g128_add(z[C_P1], z[C_VHAT]), z[C_P2]);
    }
}
static G128 bz_of(int row, const G128 *z) {
    switch (row) {
        case 0: return z[C_K_B];
        case 1: return z[C_DELTA];
        default: return z[7]; /* U */
    }
}
static G128 cz_of(int row, const G128 *z) {
    switch (row) {
        case 0: return z[C_P1];
        case 1: return z[C_P2];
        default: return g128_zero();
    }
}
/* cross_term_z(w1, u1, w2, u2=1): T = Az1∘Bz2 + Az2∘Bz1 - (u1·Cz2 + 1·Cz1).
 * char-2 subtraction = addition. */
static void cross_term_z(const G128 *w1, G128 u1, const G128 *w2, G128 *t) {
    G128 z1[8], z2[8];
    for (int i = 0; i < 7; i++) { z1[i] = w1[i]; z2[i] = w2[i]; }
    z1[7] = u1;
    z2[7] = g128_one();
    for (int r = 0; r < 3; r++) {
        G128 a1 = az_of(r, z1), b1 = bz_of(r, z1), c1 = cz_of(r, z1);
        G128 a2 = az_of(r, z2), b2 = bz_of(r, z2), c2 = cz_of(r, z2);
        t[r] = g128_add(
            g128_add(g128_mulb(a1, b2), g128_mulb(a2, b1)),
            g128_add(g128_mulb(u1, c2), c1));
    }
}

/* iop_fold_gate: fold one AND gate into the threaded accumulator. */
IopAccumulator oracle_iop_fold_gate(IopAccumulator state, Q k_a, Q k_b, Q k_c,
                                    Delta delta, Arr_Native_AES8_16 hat,
                                    IopChallenge r) {
    FoldState s = foldstate_unpack(&state);
    /* lane-0 projection (iop_fold_gate's [0] indexing + IopLift embed) */
    G128 ka = g128_from_byte(k_a.q.data[0]);
    G128 kb = g128_from_byte(k_b.q.data[0]);
    G128 kc = g128_from_byte(k_c.q.data[0]);
    G128 d = g128_from_byte(delta.delta.data[0]);
    G128 v = g128_from_byte(hat.data[0]);
    G128 rc = g128_from_bytes(r.bytes.data);

    G128 gw[FOLD_W];
    gw[0] = ka;
    gw[1] = kb;
    gw[2] = kc;
    gw[3] = d;
    gw[4] = v;
    gw[5] = g128_mulb(ka, kb);    /* P1 = K_a·K_b */
    gw[6] = g128_mulb(kc, d);     /* P2 = K_c·Δ */

    FoldState out;
    if (s.fresh) {
        out.fresh = false;
        for (int i = 0; i < FOLD_W; i++) out.w[i] = gw[i];
        for (int i = 0; i < FOLD_E; i++) out.e[i] = g128_zero();
        out.u = g128_one();
    } else {
        G128 t[FOLD_E];
        cross_term_z(s.w, s.u, gw, t);
        for (int i = 0; i < FOLD_W; i++) out.w[i] = g128_add(s.w[i], g128_mulb(rc, gw[i]));
        for (int i = 0; i < FOLD_E; i++) out.e[i] = g128_add(s.e[i], g128_mulb(rc, t[i]));
        out.u = g128_add(s.u, rc);
        out.fresh = false;
    }
    IopAccumulator packed;
    foldstate_pack(&out, &packed);
    return packed;
}

/* Final relaxed-satisfaction check over the accumulated state. */
static bool fold_verified(const FoldState *s) {
    if (s->fresh) return false; /* no gates folded — nothing to verify */
    G128 z[8];
    for (int i = 0; i < 7; i++) z[i] = s->w[i];
    z[7] = s->u;
    for (int r = 0; r < 3; r++) {
        G128 lhs = g128_mulb(az_of(r, z), bz_of(r, z));
        G128 rhs = g128_add(g128_mulb(s->u, cz_of(r, z)), s->e[r]);
        if (!g128_eq(lhs, rhs)) return false;
    }
    return true;
}

/* ================= ideal C-OT commit (vole_commit_bit) ================= */
static void commit_bit(const Delta *delta, bool b, Vope *out_v, Q *out_q) {
    Q q;
    for (int i = 0; i < 16; i++) {
        q.q.data[i] = rng_byte(); /* r0 = uniform row */
    }
    Vope v;
    for (int i = 0; i < 16; i++) {
        uint8_t di = delta->delta.data[i];
        /* v = r0 + b·Δ (XOR in char 2) */
        v.v.data[i] = q.q.data[i] ^ (b ? di : 0);
    }
    /* u = lift(bit): N=16 lanes of Galois(b) across K=1 rows */
    for (int i = 0; i < 16; i++) v.u.data[0].data[i] = b ? 1 : 0;
    *out_v = v;
    *out_q = q;
}

/* ================= driver-side zero/one constructors ================= */
static Vope vope_zero(void) { Vope v; memset(&v, 0, sizeof v); return v; }
static Q q_zero(void) { Q q; memset(&q, 0, sizeof q); return q; }
static Vope vope_one(const Delta *d) {
    (void)d;
    Vope v = vope_zero();
    for (int i = 0; i < 16; i++) v.u.data[0].data[i] = 1;
    return v;
}
static Q q_one(const Delta *d) {
    Q q;
    for (int i = 0; i < 16; i++) q.q.data[i] = d->delta.data[i];
    return q;
}

/* ================= the witness (public ground truth) ================= */
typedef struct {
    bool s2_bits[3];
    bool s33_bits[8];
    uint8_t byte_before;
    uint8_t byte_after;
} StepWitness;
static StepWitness witness[3];
static void witness_init(void) {
    for (int step = 0; step < 3; step++) {
        witness[step].s2_bits[0] = true;
        witness[step].s2_bits[1] = false;
        witness[step].s2_bits[2] = false;
        uint8_t byte = (uint8_t)step;
#ifdef M1_DISHONEST
        byte ^= 0x5A; /* lie about the read value */
#endif
        for (int i = 0; i < 8; i++)
            witness[step].s33_bits[i] = ((byte >> i) & 1) == 1;
        witness[step].byte_before = byte;
        witness[step].byte_after = (uint8_t)(byte + 1);
    }
}

/* ================= driver state ================= */
#define TOTAL_VARS 1650u
#define NW 5 /* circuit params: 3 pooled 1-bit + w_3[64] + w_4[32] */

static Delta delta;
static Vope _w_pool_vope[NW];
static bool _w_pool_vope_written[NW];
static Q _w_pool_q[NW];
static bool _w_pool_q_written[NW];
static Vope _synth_pool_vope[TOTAL_VARS];
static bool _synth_pool_vope_written[TOTAL_VARS];
static Q _synth_pool_q[TOTAL_VARS];
static bool _synth_pool_q_written[TOTAL_VARS];

/* entry state: params 0,1,2 pooled (w<=1); param 3 = [Vope;64]/[Q;64];
 * param 4 = [Vope;32]/[Q;32] */
static Vope w3_vope[64], w4_vope[32];
static Q w3_q[64], w4_q[32];

static int fails = 0;
static void check(bool ok, const char *what) {
    if (!ok) {
        printf("FAIL: %s\n", what);
        fails++;
    }
}

/* r_and challenges for one region: from_u64((step*10^7 + seed*10^5)*1_000_003 + k) */
static void r_ands_make(IopChallenge *out, int and_count, uint64_t step,
                        uint64_t gate_seed) {
    uint64_t base = (step * 10000000ull + gate_seed * 100000ull) * 1000003ull;
    for (int k = 0; k < and_count; k++) {
        g128_to_bytes(g128_from_u64(base + (uint64_t)k), out[k].bytes.data);
    }
}

/* ================= per-step driver (generate_split_step's sequence) ==== */
static void run_step(uint64_t step, uint64_t *gate_seed, FoldState *fold,
                     bool *all_ok) {
    IopAccumulator fold_abi;
    foldstate_pack(fold, &fold_abi);
    IopAccumulator fold_next = fold_abi;

    /* step-level oracle commits: event 0/1 = the two storage-2 reads
     * (3 bits each); event 2 = storage 33's double read (16 bits). The
     * committed (vope,q) pairs are consumed by whichever block/chunk
     * declares oracle_rd_ params — block 0 (event 0) and block 3 +
     * block 4 (events 1..15+ reuse the same 3-bit/16-bit patterns per
     * the region's own reads; see mem_probe.rs's oracle_bit_exprs). */
    Vope orv[16];
    Q orq[16];
    for (int j = 0; j < 3; j++)
        commit_bit(&delta, witness[step].s2_bits[j], &orv[j], &orq[j]);
    for (int j = 3; j < 16; j++)
        commit_bit(&delta, witness[step].s33_bits[j % 8], &orv[j], &orq[j]);

    /* ---------- blocks 0..6 ---------- */
    /* block 0: prover(Vope vope_one, Vope* w_4, Vope or0..2, pools) — the
     * w_4 param is the [Vope;32] entry slot; qsim(Delta, [hat;5], Q q_one,
     * Q* w_4, Q or0..2, pools); verify(Delta, [qand;5], [hat;5], [r;5],
     * Q q_one, Q* w_4, Q or0..2, all_ok, fold, pools). */
    {
        __Tuple_as4x64_as4x32_aan7x16x5 p = vole_prove_ir_mp_block_0(
            vope_one(&delta), w4_vope, orv[0], orv[1], orv[2],
            _synth_pool_vope, _synth_pool_vope_written, _w_pool_vope,
            _w_pool_vope_written);
        __Tuple_as3x64_as3x32_as3x5 q = vole_qsim_ir_mp_block_0(
            delta, p._2, q_one(&delta), w4_q, orq[0], orq[1], orq[2],
            _synth_pool_q, _synth_pool_q_written, _w_pool_q,
            _w_pool_q_written);
        IopChallenge rands[5];
        r_ands_make(rands, 5, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_0(
            delta, q._2, p._2, *(Arr_S1_5 *)rands, q_one(&delta), w4_q,
            orq[0], orq[1], orq[2], *all_ok, fold_next, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written);
        check(v._2, "block 0 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }

    /* block 1: no oracle reads, 2 AND gates. */
    {
        __Tuple_as4x64_as4x32_aan7x16x2 p = vole_prove_ir_mp_block_1(
            vope_one(&delta), _synth_pool_vope, _synth_pool_vope_written,
            _w_pool_vope, _w_pool_vope_written);
        __Tuple_as3x64_as3x32_as3x2 q = vole_qsim_ir_mp_block_1(
            delta, p._2, q_one(&delta), _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written);
        IopChallenge rands[2];
        r_ands_make(rands, 2, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_1(
            delta, q._2, p._2, *(Arr_S1_2 *)rands, q_one(&delta),
            *all_ok, fold_next, _synth_pool_q, _synth_pool_q_written,
            _w_pool_q, _w_pool_q_written);
        check(v._2, "block 1 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }

    /* block 2: 2 AND gates. */
    {
        __Tuple_as4x64_as4x32_aan7x16x2 p = vole_prove_ir_mp_block_2(
            vope_one(&delta), _synth_pool_vope, _synth_pool_vope_written,
            _w_pool_vope, _w_pool_vope_written, _synth_pool_vope);
        __Tuple_as3x64_as3x32_as3x2 q = vole_qsim_ir_mp_block_2(
            delta, p._2, q_one(&delta), _synth_pool_q, _w_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written);
        IopChallenge rands[2];
        r_ands_make(rands, 2, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_2(
            delta, q._2, p._2, *(Arr_S1_2 *)rands, q_one(&delta), _w_pool_q,
            *all_ok, fold_next, _synth_pool_q, _synth_pool_q_written,
            _w_pool_q, _w_pool_q_written);
        check(v._2, "block 2 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }

    /* block 3: the 1096-gate range. */
    {
        __Tuple_as4x64_as4x32_aan7x16x1096 p = vole_prove_ir_mp_block_3(
            vope_one(&delta), w3_vope, w4_vope, orv[0], orv[1], orv[2],
            _synth_pool_vope, _synth_pool_vope_written, _w_pool_vope,
            _w_pool_vope_written);
        __Tuple_as3x64_as3x32_as3x1096 q = vole_qsim_ir_mp_block_3(
            delta, p._2.data, q_one(&delta), w3_q, w4_q, orq[0],
            orq[1], orq[2], _synth_pool_q, _synth_pool_q_written,
            _w_pool_q, _w_pool_q_written);
        IopChallenge rands[1096];
        r_ands_make(rands, 1096, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_3(
            delta, q._2.data, p._2.data, rands, q_one(&delta), w3_q,
            w4_q, orq[0], orq[1], orq[2], *all_ok, fold_next,
            _synth_pool_q, _synth_pool_q_written, _w_pool_q,
            _w_pool_q_written);
        check(v._2, "block 3 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }

    /* block 4: the 347-gate range (2 pieces internally). Prover:
     * (vope_one, w_3*, w_4*, or0..15, synth, synth_w, w_pool, w_pool_w);
     * qsim: (delta, hat* = p._2.data, q_one, w_3*, w_4*, or0..15 Q, pools);
     * verify: (delta, qand* = q._2.data, hat*, r*, q_one, w_3*, w_4*,
     * or0..15, all_ok, fold, pools). */
    {
        __Tuple_as4x64_as4x32_aan7x16x347 p = vole_prove_ir_mp_block_4(
            vope_one(&delta), w3_vope, w4_vope,
            orv[0], orv[1], orv[2], orv[3], orv[4], orv[5], orv[6],
            orv[7], orv[8], orv[9], orv[10], orv[11], orv[12], orv[13],
            orv[14], orv[15],
            _synth_pool_vope, _synth_pool_vope_written, _w_pool_vope,
            _w_pool_vope_written);
        __Tuple_as3x64_as3x32_as3x347 q = vole_qsim_ir_mp_block_4(
            delta, p._2.data, q_one(&delta), w3_q, w4_q,
            orq[0], orq[1], orq[2], orq[3], orq[4], orq[5], orq[6],
            orq[7], orq[8], orq[9], orq[10], orq[11], orq[12], orq[13],
            orq[14], orq[15],
            _synth_pool_q, _synth_pool_q_written, _w_pool_q,
            _w_pool_q_written);
        IopChallenge rands[5];
        r_ands_make(rands, 5, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_4(
            delta, q._2.data, p._2.data, rands, q_one(&delta), w3_q, w4_q,
            orq[0], orq[1], orq[2], orq[3], orq[4], orq[5], orq[6],
            orq[7], orq[8], orq[9], orq[10], orq[11], orq[12], orq[13],
            orq[14], orq[15], *all_ok, fold_next, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written);
        check(v._2, "block 4 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }

    /* blocks 5 and 6: 2 AND gates each (leaf ranges, no oracle). */
    {
        __Tuple_as4x64_as4x32_aan7x16x2 p = vole_prove_ir_mp_block_5(
            vope_one(&delta), _synth_pool_vope, _w_pool_vope,
            _synth_pool_vope, _w_pool_vope_written, _synth_pool_vope,
            _synth_pool_vope_written);
        __Tuple_as3x64_as3x32_as3x2 q = vole_qsim_ir_mp_block_5(
            delta, p._2, q_one(&delta), _synth_pool_q, _w_pool_q,
            _synth_pool_q, _w_pool_q_written, _w_pool_q,
            _synth_pool_q_written);
        IopChallenge rands[2];
        r_ands_make(rands, 2, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_5(
            delta, q._2, p._2, *(Arr_S1_2 *)rands, q_one(&delta),
            _synth_pool_q, _w_pool_q, *all_ok, fold_next, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written);
        check(v._2, "block 5 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }
    {
        __Tuple_as4x64_as4x32_aan7x16x2 p = vole_prove_ir_mp_block_6(
            vope_one(&delta), _synth_pool_vope, _w_pool_vope,
            _synth_pool_vope, _w_pool_vope_written, _synth_pool_vope,
            _synth_pool_vope_written);
        __Tuple_as3x64_as3x32_as3x2 q = vole_qsim_ir_mp_block_6(
            delta, p._2, q_one(&delta), _synth_pool_q, _w_pool_q,
            _synth_pool_q, _w_pool_q_written, _w_pool_q,
            _synth_pool_q_written);
        IopChallenge rands[2];
        r_ands_make(rands, 2, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_6(
            delta, q._2, p._2, *(Arr_S1_2 *)rands, q_one(&delta),
            _synth_pool_q, _w_pool_q, *all_ok, fold_next, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written);
        check(v._2, "block 6 verifier all_ok");
        *all_ok = *all_ok && v._2;
        fold_next = v._3;
    }

    /* ---------- chunks 0..3: running accumulator over the same exports --
     * (threaded exactly like the blocks; hats from each chunk's own prover
     * call — the chunk fns recompute their ranges over the block outputs
     * carried in the synth pool) */
    {
        __Tuple_as4x64_as4x32_aan7x16x168 p0 = vole_prove_ir_mp_accum_chunk_0(
            vope_one(&delta), w3_vope, w4_vope, _synth_pool_vope,
            _synth_pool_vope_written, _w_pool_vope, _w_pool_vope_written,
            w3_vope, w4_vope, w3_vope, w4_vope, w3_vope, w4_vope);
        __Tuple_as3x64_as3x32_as3x168 q0 = vole_qsim_ir_mp_accum_chunk_0(
            delta, p0._2.data, q_one(&delta), w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            w3_q, w4_q, w3_q, w4_q, w3_q, w4_q);
        IopChallenge r0[168];
        r_ands_make(r0, 168, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v0 = vole_verify_ir_mp_accum_chunk_0(
            delta, q0._2.data, p0._2.data, r0, q_one(&delta), w3_q, w4_q,
            w3_q, w4_q, w3_q, w4_q, w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            *all_ok, fold_next);
        check(v0._2, "chunk 0 verifier all_ok");
        *all_ok = *all_ok && v0._2;
        fold_next = v0._3;
    }
    {
        __Tuple_as4x64_as4x32_aan7x16x168 p1 = vole_prove_ir_mp_accum_chunk_1(
            vope_one(&delta), w3_vope, w4_vope, _synth_pool_vope,
            _synth_pool_vope_written, _w_pool_vope, _w_pool_vope_written,
            w3_vope, w4_vope, w3_vope, w4_vope, w3_vope, w4_vope);
        __Tuple_as3x64_as3x32_as3x168 q1 = vole_qsim_ir_mp_accum_chunk_1(
            delta, p1._2.data, q_one(&delta), w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            w3_q, w4_q, w3_q, w4_q, w3_q, w4_q);
        IopChallenge r1[168];
        r_ands_make(r1, 168, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v1 = vole_verify_ir_mp_accum_chunk_1(
            delta, q1._2.data, p1._2.data, r1, q_one(&delta), w3_q, w4_q,
            w3_q, w4_q, w3_q, w4_q, w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            *all_ok, fold_next);
        check(v1._2, "chunk 1 verifier all_ok");
        *all_ok = *all_ok && v1._2;
        fold_next = v1._3;
    }
    {
        __Tuple_as4x64_as4x32_aan7x16x168 p2 = vole_prove_ir_mp_accum_chunk_2(
            vope_one(&delta), w3_vope, w4_vope, _synth_pool_vope,
            _synth_pool_vope_written, _w_pool_vope, _w_pool_vope_written,
            w3_vope, w4_vope, w3_vope, w4_vope, w3_vope, w4_vope);
        __Tuple_as3x64_as3x32_as3x168 q2 = vole_qsim_ir_mp_accum_chunk_2(
            delta, p2._2.data, q_one(&delta), w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            w3_q, w4_q, w3_q, w4_q, w3_q, w4_q);
        IopChallenge r2[168];
        r_ands_make(r2, 168, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v2 = vole_verify_ir_mp_accum_chunk_2(
            delta, q2._2.data, p2._2.data, r2, q_one(&delta), w3_q, w4_q,
            w3_q, w4_q, w3_q, w4_q, w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            *all_ok, fold_next);
        check(v2._2, "chunk 2 verifier all_ok");
        *all_ok = *all_ok && v2._2;
        fold_next = v2._3;
    }
    {
        __Tuple_as4x64_as4x32_aan7x16x68 p3 = vole_prove_ir_mp_accum_chunk_3(
            vope_one(&delta), w3_vope, w4_vope, _synth_pool_vope,
            _synth_pool_vope_written, _w_pool_vope, _w_pool_vope_written,
            w3_vope, w4_vope, w3_vope, w4_vope);
        __Tuple_as3x64_as3x32_as3x68 q3 = vole_qsim_ir_mp_accum_chunk_3(
            delta, p3._2.data, q_one(&delta), w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            w3_q, w4_q, w3_q, w4_q);
        IopChallenge r3[68];
        r_ands_make(r3, 68, step, ++*gate_seed);
        __Tuple_as3x64_as3x32_b_s0 v3 = vole_verify_ir_mp_accum_chunk_3(
            delta, q3._2.data, p3._2.data, r3, q_one(&delta), w3_q, w4_q,
            w3_q, w4_q, w3_q, w4_q, w3_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, *all_ok, fold_next);
        check(v3._2, "chunk 3 verifier all_ok");
        *all_ok = *all_ok && v3._2;
        fold_next = v3._3;
    }

    /* ---------- finish: next step's entry state ---------- */
    {
        __Tuple_s16_aan7x16x0 pf = vole_prove_ir_mp_finish(
            vope_one(&delta), w3_vope, w4_vope, _synth_pool_vope,
            _synth_pool_vope_written, _w_pool_vope, _w_pool_vope_written,
            w3_vope, w4_vope);
        __Tuple_s28_as3x0 qf = vole_qsim_ir_mp_finish(
            delta, q_one(&delta), w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            w3_q, w4_q);
        __Tuple_s28_b_s0 vf = vole_verify_ir_mp_finish(
            delta, q_one(&delta), w3_q, w4_q, _synth_pool_q,
            _synth_pool_q_written, _w_pool_q, _w_pool_q_written,
            w3_q, w4_q, *all_ok, fold_next);
        check(vf._1, "finish verifier all_ok");
        *all_ok = *all_ok && vf._1;
        fold_next = vf._2;
        /* Next step's entry state: the finish output tuples' Q/Vope
         * entries — this circuit's params are w_3/w_4 slots carried in the
         * finish outputs' flattened export list; the honest run's
         * interpreter state is step-looped through the finish outputs
         * (done flag consumed; state slots copied back). */
        (void)pf;
        (void)qf;
    }

    *fold = foldstate_unpack(&fold_next);
}

static int m1_main_inner(void) {
    witness_init();

    /* delta: 16 nonzero bytes (bounded retries, per random_nonzero_delta) */
    for (int i = 0; i < 16; i++) {
        uint8_t x = 0;
        int tries = 0;
        while (x == 0 && tries < 64) {
            x = rng_byte();
            tries++;
        }
        delta.delta.data[i] = x;
    }

    /* entry state: all params zero (the honest driver's own zero_stmts) */
    for (int i = 0; i < NW; i++) {
        _w_pool_vope[i] = vope_zero();
        _w_pool_q[i] = q_zero();
        _w_pool_vope_written[i] = true;
        _w_pool_q_written[i] = true;
    }
    for (int i = 0; i < 64; i++) {
        w3_vope[i] = vope_zero();
        w3_q[i] = q_zero();
    }
    for (int i = 0; i < 32; i++) {
        w4_vope[i] = vope_zero();
        w4_q[i] = q_zero();
    }

    FoldState fold;
    memset(&fold, 0, sizeof fold);
    fold.fresh = true;
    bool all_ok = true;

    for (uint64_t step = 0; step < 3; step++) {
        uint64_t gate_seed = 0;
        run_step(step, &gate_seed, &fold, &all_ok);
        printf("STEP %llu OK (all_ok=%d)\n",
               (unsigned long long)step, (int)all_ok);
        if (fails > 0) break;
    }

    check(all_ok, "honest 3-step run must keep all_ok true");
    check(fold_verified(&fold),
          "the IOP fold accumulator must end relaxed-satisfied");
    check(!fold.fresh, "gates must actually have been folded in");

    if (fails == 0) {
        printf("ALL CHECKS PASSED: 3-step honest interaction over the C-lowered "
               "roles; fold accumulator relaxed-satisfied\n");
        return 0;
    }
    printf("%d CHECK(S) FAILED\n", fails);
    return 1;
}

#include <pthread.h>
static void *m1_thread(void *arg) {
    (void)arg;
    return (void *)(long)m1_main_inner();
}
int main(void) {
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 1024ull * 1024 * 1024);
    pthread_t th;
    pthread_create(&th, &attr, m1_thread, NULL);
    void *ret = NULL;
    pthread_join(th, &ret);
    return (int)(long)ret;
}
