// Auto-generated TypeScript from volar-spec
// Type-level lengths have been converted to runtime number witnesses

import {
  type Cloneable,
  type FieldElement,
  type BlockEncrypt,
  type Digest,
  type LengthDoubler,
  Bit,
  Galois,
  Galois64,
  BitsInBytes,
  BitsInBytes64,
  Z3,
  fieldAdd,
  fieldSub,
  fieldMul,
  fieldBitxor,
  fieldBitor,
  fieldBitand,
  fieldShl,
  fieldShr,
  fieldEq,
  fieldNe,
  ilog2,
  commit as hashCommit,
  doubleVec,
  wrappingAdd,
  wrappingSub,
  asRefU8,
} from "./index";

class Some<T> { constructor(public _0: T) {} }
class Ok<T> { constructor(public _0: T) {} }
class Err<E = unknown> { constructor(public _0: E) {} }
type Vec<T> = T[];
type Option<T> = T | undefined;
type Result<T, E = unknown> = T;
function __clone<T>(x: T): T {
  if (Array.isArray(x)) return ([...x] as unknown) as T;
  if (x !== null && typeof x === 'object') return Object.assign(Object.create(Object.getPrototypeOf(x)), x) as T;
  return x;
}

export const P_LIMBS = [BigInt("18446744073709551597"), BigInt("18446744073709551615"), BigInt("18446744073709551615"), BigInt("9223372036854775807")];
export const D_LIMBS = [BigInt("8496970652267935907"), BigInt("31536524315187371"), BigInt("10144147576115030168"), BigInt("5909686906226998899")];
export const D2_LIMBS = [BigInt("16993941304535871833"), BigInt("63073048630374742"), BigInt("1841551078520508720"), BigInt("2596001775599221991")];
export const D = fe_const(D_LIMBS);
export const D2 = fe_const(D2_LIMBS);
export const BASE_X_LIMBS = [BigInt("14507833142362363162"), BigInt("7578651490590762930"), BigInt("13881468655802702940"), BigInt("2407515759118799870")];
export const BASE_Y_LIMBS = [BigInt("7378697629483820632"), BigInt("7378697629483820646"), BigInt("7378697629483820646"), BigInt("7378697629483820646")];
export const SBOX = [99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118, 202, 130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192, 183, 253, 147, 38, 54, 63, 247, 204, 52, 165, 229, 241, 113, 216, 49, 21, 4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117, 9, 131, 44, 26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132, 83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207, 208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168, 81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210, 205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93, 25, 115, 96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219, 224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98, 145, 149, 228, 121, 231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8, 186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138, 112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158, 225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223, 140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22];
export const RCON = [0, 1, 2, 4, 8, 16, 32, 64, 128, 27, 54];
export const NR = 10;
export const BLOCK = 16;
export const NK_ROUND_KEYS = fieldAdd(NR, 1);
export const GF8_AES_POLY = 27;
export const LAMBDA_BYTES = 16;
export const TAU = 4;
export const SUB_VOLE_N = 8;
export const SUB_VOLE_K = 3;
export const L_HAT_BYTES = 16;
export const W_GRIND = 4;
export const COM_BYTES = 32;
export const TOY_P = 2147483647;
export const TOY_G = 7;
export const IKNP_KAPPA = 128;
export const IKNP_KAPPA_BYTES = (IKNP_KAPPA / 8);
export const LWE_N = 16;
export const LWE_Q_BITS = 16;
export const LWE_Q = fieldShl(1, LWE_Q_BITS);
export const LWE_Q_MASK = fieldSub(LWE_Q, 1);
export const LWE_NOISE_BOUND = 1;
export const TAG_DOMAIN = new Uint8Array([/* byte string */]);
export const Q4 = fieldShl(1, 30);

export class ABODyn<B, D> {
  k!: number;
  n!: number;
  commit!: number[];
  per_byte!: number[][][];

  constructor(init: { 
    k: number,
    n: number,
    commit: number[],
    per_byte: number[][][]
  }) {
    Object.assign(this, init);
  }

  open(t: number, u: number, m: number, bad: bigint[], rand: R): ABOOpeningDyn<B, D>
  {
    const k: number = this.k;
    const n: number = this.n;
    return new ABOOpeningDyn({ bad: __clone(bad), openings: Array.from({length: n - 0}, (_, __i) => __i + 0).map((ni: any) => (() => {
  const bad_1 = __clone(bad);
  return Array.from({length: t - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const bad_2 = __clone(bad_1);
  return Array.from({length: u - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  const i2 = fieldBitor(i, fieldShl(Number(j), ilog2(t)));
  return (() => { if (bad_2.includes(BigInt(i2))) {
  const h = hashCommit(this.per_byte[ni][i2], rand);
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((j: any) => (asRefU8(h)?.[j] ?? 0));
} else {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  return (this.per_byte[ni][i2]?.[j] ?? 0);
})());
} })();
})());
})());
})()), t: 0, u: 0, n: 0 });
  }

  split_bit_typenum(ctx: { B_OutputSize: number, D_OutputSize: number }, m: number, target: number): BSplitDyn<B, D>[]
  {
    const k: number = this.k;
    const n: number = this.n;
    return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[N.party_index(target)].slice(fieldMul(i, m)).slice(0, m);
  return new BSplitDyn({ split: Array.from({length: ilog2(ctx.D_OutputSize) - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b: any) => (() => {
  return s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, c]: any) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return __clone(c);
} else {
  return undefined;
} })();
})()).filter((__x: any) => __x !== undefined).reduce((a: any, b: any) => (() => {
  return Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(a[i], b[i]));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
})());
})()) });
})());
  }

  to_vole_material(ctx: { B_OutputSize: number }, m: number, target: number): VopeDyn<number>[]
  {
    const k: number = this.k;
    const n: number = this.n;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[N.party_index(target)].slice(fieldMul(i, m)).slice(0, m);
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_expanded(ctx: { B_OutputSize: number }, m: number, target: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const k: number = this.k;
    const n: number = this.n;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[N.party_index(target)].slice(fieldMul(i, m)).slice(0, m);
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  to_vole_material_typenum(ctx: { B_OutputSize: number }, m: number, target: number): VopeDyn<number>[]
  {
    const k: number = this.k;
    const n: number = this.n;
    return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[N.party_index(target)].slice(fieldMul(i, m)).slice(0, m);
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_typenum_expanded(ctx: { B_OutputSize: number }, m: number, target: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const k: number = this.k;
    const n: number = this.n;
    return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[N.party_index(target)].slice(fieldMul(i, m)).slice(0, m);
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }
}

export class ABOOpeningDyn<B, D> {
  t!: number;
  u!: number;
  n!: number;
  bad!: bigint[];
  openings!: number[][][][];

  constructor(init: { 
    t: number,
    u: number,
    n: number,
    bad: bigint[],
    openings: number[][][][]
  }) {
    Object.assign(this, init);
  }

  split_bit_typenum(ctx: { B_OutputSize: number, D_OutputSize: number }, m: number, party: number): BSplitDyn<B, D>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    const n: number = this.n;
    return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.openings[N.party_index(party)][i];
  return new BSplitDyn({ split: Array.from({length: ilog2(ctx.D_OutputSize) - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b: any) => (() => {
  return s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, c]: any) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return __clone(c);
} else {
  return undefined;
} })();
})()).filter((__x: any) => __x !== undefined).reduce((a: any, b: any) => (() => {
  return Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(a[i], b[i]));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
})());
})()) });
})());
  }

  to_vole_material(ctx: { B_OutputSize: number }, m: number, party: number): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    const n: number = this.n;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.openings[N.party_index(party)][i];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_expanded(ctx: { B_OutputSize: number }, m: number, party: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    const n: number = this.n;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.openings[N.party_index(party)][i];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  to_vole_material_typenum(ctx: { B_OutputSize: number }, m: number, party: number): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    const n: number = this.n;
    return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.openings[N.party_index(party)][i];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_typenum_expanded(ctx: { B_OutputSize: number }, m: number, party: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    const n: number = this.n;
    return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.openings[N.party_index(party)][i];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  validate(ctx: { B_OutputSize: number, D_OutputSize: number, newD: () => any }, commit_: number[], rand: R): boolean
  {
    const t: number = this.t;
    const u: number = this.u;
    const n: number = this.n;
    let h = ctx.newD();
    for (let i = 0; i < t; i++)     {
      for (let b = 0; b < u; b++)       {
        const i2 = fieldBitor(i, fieldShl(Number(b), ilog2(t)));
        if (this.bad.includes(BigInt(i2)))         {
          h.update(this.openings[0][i][b].slice(0, ctx.D_OutputSize));
        } else         {
          h.update(hashCommit(this.openings[0][i][b].slice(0, ctx.B_OutputSize), rand));
        }
      }
    }
    return ([...h.finalize()] === commit_);
  }
}

export class BSplitDyn<B, D> {
  split!: number[][][];

  constructor(init: { 
    split: number[][][]
  }) {
    Object.assign(this, init);
  }
}

export class Fe25519 {
  _0!: bigint[];

  constructor(_0: bigint[]) {
    this._0 = _0;
  }

  add(rhs: Fe25519): Fe25519
  {
    return fe_add(this, rhs);
  }

  is_zero(): boolean
  {
    return (this._0 === [0, 0, 0, 0]);
  }

  mul(rhs: Fe25519): Fe25519
  {
    return fe_mul(this, rhs);
  }

  neg(): Fe25519
  {
    return fe_neg(this);
  }

  sub(rhs: Fe25519): Fe25519
  {
    return fe_sub(this, rhs);
  }

  to_bytes(): number[]
  {
    let out = Array.from({length: 32}, () => 0);
    for (let i = 0; i < 4; i++)     {
      (out.slice(fieldMul(i, 8), fieldAdd(fieldMul(i, 8), 8))).splice(0, ([(this._0[i]) & 0xFF, ((this._0[i]) >> 8) & 0xFF, ((this._0[i]) >> 16) & 0xFF, ((this._0[i]) >> 24) & 0xFF]).length, ...([(this._0[i]) & 0xFF, ((this._0[i]) >> 8) & 0xFF, ((this._0[i]) >> 16) & 0xFF, ((this._0[i]) >> 24) & 0xFF]));
    }
    return out;
  }
}

export class EdPoint {
  x!: Fe25519;
  y!: Fe25519;
  z!: Fe25519;
  t!: Fe25519;

  constructor(init: { 
    x: Fe25519,
    y: Fe25519,
    z: Fe25519,
    t: Fe25519
  }) {
    Object.assign(this, init);
  }

  static base(): EdPoint
  {
    const x = new Fe25519(BASE_X_LIMBS);
    const y = new Fe25519(BASE_Y_LIMBS);
    return new EdPoint({ x: x, y: y, z: Fe25519.ONE, t: fe_mul(x, y) });
  }

  eq(other: EdPoint): boolean
  {
    const lhs_x = fe_mul(this.x, other.z);
    const rhs_x = fe_mul(other.x, this.z);
    const lhs_y = fe_mul(this.y, other.z);
    const rhs_y = fe_mul(other.y, this.z);
    return ((lhs_x === rhs_x) && (lhs_y === rhs_y));
  }

  to_affine(): [Fe25519, Fe25519]
  {
    const zinv = fe_invert(this.z);
    return [fe_mul(this.x, zinv), fe_mul(this.y, zinv)];
  }
}

export class Ed25519 {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  static add(a: EdPoint, b: EdPoint): EdPoint
  {
    return ed_add(a, b);
  }

  static generator(): EdPoint
  {
    return EdPoint.base();
  }

  static neg(a: EdPoint): EdPoint
  {
    return ed_neg(a);
  }

  static random_scalar<R>(rng: R): number[]
  {
    let k = Array.from({length: 32}, () => 0);
    for (const byte of k.iter_mut())     {
      byte = rng.next_u8();
    }
    k[31] &= 63;
    return k;
  }

  static scalar_mul(elt: EdPoint, k: number[]): EdPoint
  {
    return ed_scalar_mul(elt, k);
  }

  static write_element<D_>(elt: EdPoint, h: D_)
  {
    const [x, y] = elt.to_affine();
    h.update(x.to_bytes());
    h.update(y.to_bytes());
  }
}

export class BavcCommitmentDyn {
  com_bytes!: number;
  root!: Vec<number>;
  vec_hashes!: Vec<Vec<number>>;
  seeds!: Vec<number[]>;
  commitments!: Vec<number[]>;

  constructor(init: { 
    com_bytes: number,
    root: Vec<number>,
    vec_hashes: Vec<Vec<number>>,
    seeds: Vec<number[]>,
    commitments: Vec<number[]>
  }) {
    Object.assign(this, init);
  }
}

export class BavcOpeningDyn {
  com_bytes!: number;
  hidden_commits!: Vec<number[]>;
  nodes!: Vec<[number, number[]]>;

  constructor(init: { 
    com_bytes: number,
    hidden_commits: Vec<number[]>,
    nodes: Vec<[number, number[]]>
  }) {
    Object.assign(this, init);
  }
}

export class BavcDyn<L> {
  com_bytes!: number;

  constructor(init: { 
    com_bytes: number
  }) {
    Object.assign(this, init);
  }

  static collect_open_nodes(com_bytes: number, deltas: readonly number[], tree: readonly number[][], tau: number, n: number): Vec<[number, number[]]>
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2, leaf_count), 1);
    let hidden = [];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1), leaf_k);
      hidden[tree_pos] = true;
    }
    for (const node of (Array.from({length: fieldSub(leaf_count, 1) - 0}, (_, i) => i + 0)).slice().reverse())     {
      hidden[node] = (hidden[fieldAdd(fieldMul(2, node), 1)] || hidden[fieldAdd(fieldMul(2, node), 2)]);
    }
    let out: Vec<[number, number[]]> = []();
    walk(0, hidden, tree, leaf_count, out);
    return out;
  }

  static commit(ctx: { newD: () => any }, com_bytes: number, r: number[], iv: number[], tau: number, n: number): BavcCommitmentDyn
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2, leaf_count), 1);
    let tree: Vec<number[]> = [];
    tree[0] = r;
    for (let node = 0; node < fieldSub(leaf_count, 1); node++)     {
      const parent = Vec(tree[node]);
      const [left, right] = doubleVec(parent);
      tree[fieldAdd(fieldMul(2, node), 1)] = left._0;
      tree[fieldAdd(fieldMul(2, node), 2)] = right._0;
    }
    let seeds = /* Vec::with_capacity */ Array(leaf_count);
    let commitments = /* Vec::with_capacity */ Array(leaf_count);
    for (let i = 0; i < tau; i++)     {
      for (let j = 0; j < n; j++)       {
        const leaf_k = fieldAdd(fieldMul(i, n), j);
        const tree_pos = fieldAdd(fieldSub(leaf_count, 1), leaf_k);
        const r_leaf = tree[tree_pos];
        const tweak = Number(leaf_k);
        const [sd, com] = hashCommit(r_leaf, iv, tweak);
        (seeds).push(sd);
        (commitments).push(com);
      }
    }
    let vec_hashes: Vec<Vec<number>> = /* Vec::with_capacity */ Array(tau);
    for (let i = 0; i < tau; i++)     {
      let h = ctx.newD();
      for (let j = 0; j < n; j++)       {
        h.update(commitments[fieldAdd(fieldMul(i, n), j)]);
      }
      (vec_hashes).push([...[...h.finalize()]]);
    }
    let h = ctx.newD();
    for (const hi of vec_hashes)     {
      h.update(hi);
    }
    const root = [...[...h.finalize()]];
    return new BavcCommitmentDyn({ root: root, vec_hashes: vec_hashes, seeds: seeds, commitments: commitments, com_bytes: 0 });
  }

  static open(com_bytes: number, commitment: BavcCommitmentDyn, deltas: readonly number[], tau: number, n: number): BavcOpeningDyn
  {
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
    }
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2, leaf_count), 1);
    let hidden = [];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1), leaf_k);
      hidden[tree_pos] = true;
    }
    for (const node of (Array.from({length: fieldSub(leaf_count, 1) - 0}, (_, i) => i + 0)).slice().reverse())     {
      const left = fieldAdd(fieldMul(2, node), 1);
      const right = fieldAdd(fieldMul(2, node), 2);
      hidden[node] = (hidden[left] || hidden[right]);
    }
    let tree: Vec<number[]> = [];
    const _ = tree;
    return new BavcOpeningDyn({ hidden_commits: deltas.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([i, d]: any) => commitment.commitments[fieldAdd(fieldMul(i, n), d)]), nodes: [](), com_bytes: 0 });
  }

  static reconstruct(ctx: { newD: () => any }, com_bytes: number, nodes: readonly [number, number[]][], hidden_commits: readonly number[][], deltas: readonly number[], iv: number[], expected_root: readonly number[], tau: number, n: number): (Vec<number[]> | undefined)
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2, leaf_count), 1);
    let hidden = [];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1), leaf_k);
      hidden[tree_pos] = true;
    }
    for (const node of (Array.from({length: fieldSub(leaf_count, 1) - 0}, (_, i) => i + 0)).slice().reverse())     {
      hidden[node] = (hidden[fieldAdd(fieldMul(2, node), 1)] || hidden[fieldAdd(fieldMul(2, node), 2)]);
    }
    let tree: Vec<(number[] | undefined)> = [];
    for (const [idx, seed] of nodes)     {
      tree[idx] = seed;
    }
    for (let node = 0; node < fieldSub(leaf_count, 1); node++)     {
      return (() => { const __match = tree[node]; if (__match !== null && __match !== undefined) { const parent_seed = __match;
return (() => {
  const parent = Vec(parent_seed);
  const [left, right] = doubleVec(parent);
  if ((tree[fieldAdd(fieldMul(2, node), 1)]) == null)   {
    tree[fieldAdd(fieldMul(2, node), 1)] = left._0;
  }
  if ((tree[fieldAdd(fieldMul(2, node), 2)]) == null)   {
    tree[fieldAdd(fieldMul(2, node), 2)] = right._0;
  }
})(); } else { return (() => {
})(); } })();
    }
    let leaf_seeds: Vec<number[]> = /* Vec::with_capacity */ Array(leaf_count);
    let leaf_coms: Vec<number[]> = /* Vec::with_capacity */ Array(leaf_count);
    for (let i = 0; i < tau; i++)     {
      for (let j = 0; j < n; j++)       {
        const leaf_k = fieldAdd(fieldMul(i, n), j);
        const tree_pos = fieldAdd(fieldSub(leaf_count, 1), leaf_k);
        if ((j === deltas[i]))         {
          (leaf_seeds).push(Array.from({length: LAMBDA_BYTES}, () => 0));
          (leaf_coms).push(hidden_commits[i]);
        } else         {
          const r_leaf = tree[tree_pos];
          const tweak = Number(leaf_k);
          const [sd, com] = hashCommit(r_leaf, iv, tweak);
          (leaf_seeds).push(sd);
          (leaf_coms).push(com);
        }
      }
    }
    let vec_hashes: Vec<Vec<number>> = /* Vec::with_capacity */ Array(tau);
    for (let i = 0; i < tau; i++)     {
      let h = ctx.newD();
      for (let j = 0; j < n; j++)       {
        h.update(leaf_coms[fieldAdd(fieldMul(i, n), j)]);
      }
      (vec_hashes).push([...[...h.finalize()]]);
    }
    let h = ctx.newD();
    for (const hi of vec_hashes)     {
      h.update(hi);
    }
    const root = [...[...h.finalize()]];
    return (() => { if ((root === expected_root)) {
  return leaf_seeds;
} else {
  return undefined;
} })();
  }
}

export class ConvertOutput {
  u!: Vec<number>;
  v!: Vec<Vec<number>>;

  constructor(init: { 
    u: Vec<number>,
    v: Vec<Vec<number>>
  }) {
    Object.assign(this, init);
  }
}

export class BigVoleProver {
  u!: Vec<number>;
  c!: Vec<Vec<number>>;
  v_columns!: Vec<Vec<number>>;

  constructor(init: { 
    u: Vec<number>,
    c: Vec<Vec<number>>,
    v_columns: Vec<Vec<number>>
  }) {
    Object.assign(this, init);
  }
}

export class BigVoleVerifier {
  q_columns!: Vec<Vec<number>>;

  constructor(init: { 
    q_columns: Vec<Vec<number>>
  }) {
    Object.assign(this, init);
  }
}

export class RoLeafCommitDyn<D> {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  static commit(ctx: { newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, r: number[], iv: number[], tweak: number): [number[], number[]]
  {
    const out_size = ctx.DClass.output_size();
    let h = ctx.newD();
    h.update(r);
    h.update(iv);
    h.update([(tweak) & 0xFF, ((tweak) >> 8) & 0xFF, ((tweak) >> 16) & 0xFF, ((tweak) >> 24) & 0xFF]);
    const digest = [...h.finalize()];
    let sd = Array.from({length: sd_bytes}, () => 0);
    let com = Array.from({length: com_bytes}, () => 0);
    (sd).splice(0, (digest.slice(0, sd_bytes)).length, ...(digest.slice(0, sd_bytes)));
    (com).splice(0, (digest.slice(sd_bytes, fieldAdd(sd_bytes, com_bytes))).length, ...(digest.slice(sd_bytes, fieldAdd(sd_bytes, com_bytes))));
    return [sd, com];
  }
}

export class EmLeafCommit {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  static commit(r: number[], iv: number[], tweak: number): [number[], number[]]
  {
    let seed_buf = Array.from({length: 16}, () => 0);
    (seed_buf).splice(0, (r.slice(0)).length, ...(r.slice(0)));
    const com_vec = aes_ctr_prg(seed_buf, iv, tweak, com_bytes);
    let sd = Array.from({length: sd_bytes}, () => 0);
    (sd).splice(0, (r).length, ...(r));
    let com = Array.from({length: com_bytes}, () => 0);
    (com).splice(0, (com_vec.slice(0, com_bytes)).length, ...(com_vec.slice(0, com_bytes)));
    return [sd, com];
  }
}

export class AesCtrLengthDoubler {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  static double(a: number[]): number[][]
  {
    let block0 = Array.from({length: BLOCK}, () => 0);
    let block1 = Array.from({length: BLOCK}, () => 0);
    block1[0] = 1;
    const key: number[] = a._0;
    const c0 = encrypt_block(key, block0);
    const c1 = encrypt_block(key, block1);
    (block0).fill(0);
    (block1).fill(0);
    return [Vec(c0), Vec(c1)];
  }
}

export class FaestSecretKey {
  _0!: number[];

  constructor(_0: number[]) {
    this._0 = _0;
  }
}

export class FaestPublicKey {
  _0!: number[];

  constructor(_0: number[]) {
    this._0 = _0;
  }
}

export class FaestSignature {
  iv!: number[];
  bavc_root!: Vec<number>;
  hidden_commits!: Vec<number[]>;
  nodes!: Vec<[number, number[]]>;
  corrections!: Vec<Vec<number>>;
  vole_u!: Vec<number>;
  qs_proof!: QuickSilverProof;
  c_hat_with_counter!: Vec<number>;
  chall_3!: Vec<number>;
  counter!: number;

  constructor(init: { 
    iv: number[],
    bavc_root: Vec<number>,
    hidden_commits: Vec<number[]>,
    nodes: Vec<[number, number[]]>,
    corrections: Vec<Vec<number>>,
    vole_u: Vec<number>,
    qs_proof: QuickSilverProof,
    c_hat_with_counter: Vec<number>,
    chall_3: Vec<number>,
    counter: number
  }) {
    Object.assign(this, init);
  }
}

export class QuickSilverProof {
  a_hat!: Vec<number>;
  b_hat!: Vec<number>;
  c_hat_base!: Vec<number>;

  constructor(init: { 
    a_hat: Vec<number>,
    b_hat: Vec<number>,
    c_hat_base: Vec<number>
  }) {
    Object.assign(this, init);
  }
}

export class StubFaestAesProver {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  prove_aes_witness(big_vole: BigVoleProver, hash_key: UniversalHashKey): QuickSilverProof
  {
    const a_hat_out: UniversalHashOutput = vole_hash(hash_key, big_vole.u);
    const a_hat: Vec<number> = [(a_hat_out.h0._0) & 0xFF, ((a_hat_out.h0._0) >> 8) & 0xFF, ((a_hat_out.h0._0) >> 16) & 0xFF, ((a_hat_out.h0._0) >> 24) & 0xFF].concat([(a_hat_out.h1._0) & 0xFF, ((a_hat_out.h1._0) >> 8) & 0xFF, ((a_hat_out.h1._0) >> 16) & 0xFF, ((a_hat_out.h1._0) >> 24) & 0xFF]).collect();
    const len = a_hat.length;
    return new QuickSilverProof({ a_hat: a_hat, b_hat: [], c_hat_base: [] });
  }
}

export class FaestTranscript {
  sponge!: Sponge;

  constructor(init: { 
    sponge: Sponge
  }) {
    Object.assign(this, init);
  }

  absorb(data: readonly number[])
  {
    this.sponge.absorb(data);
  }

  static new_shake128(): FaestTranscript
  {
    return new FaestTranscript({ sponge: Sponge.Shake128(Shake128.default()) });
  }

  static new_shake256(): FaestTranscript
  {
    return new FaestTranscript({ sponge: Sponge.Shake256(Shake256.default()) });
  }

  squeeze(n: number): Vec<number>
  {
    return this.sponge.squeeze(n);
  }
}

export class UniversalHashKey {
  r0!: Galois128;
  r1!: Galois64;

  constructor(init: { 
    r0: Galois128,
    r1: Galois64
  }) {
    Object.assign(this, init);
  }
}

export class UniversalHashOutput {
  h0!: Galois128;
  h1!: Galois64;

  constructor(init: { 
    h0: Galois128,
    h1: Galois64
  }) {
    Object.assign(this, init);
  }
}

export class EvalDyn {
  n!: number;
  target!: number[];

  constructor(init: { 
    n: number,
    target: number[]
  }) {
    Object.assign(this, init);
  }

  and_via_table(ctx: { newD: () => any }, other: EvalDyn, table: GarbleTableDyn): EvalDyn
  {
    const n: number = this.n;
    const index = fieldBitor((() => { if ((fieldBitand(this.target[0], 1) === 1)) {
  return 1;
} else {
  return 0;
} })(), (() => { if ((fieldBitand(other.target[0], 1) === 1)) {
  return 2;
} else {
  return 0;
} })());
    const hash = (() => {
  let d = ctx.newD();
  d.update(this.target);
  d.update(other.target);
  return [...d.finalize()];
})();
    return new EvalDyn({ target: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(hash[i], table.table[index][i])), n: 0 });
  }

  bitxor(rhs: EvalDyn): number /* EvalDyn::Output */
  {
    const n: number = this.n;
    return new EvalDyn({ target: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(this.target[i], rhs.target[i])), n: 0 });
  }

  open(garble: GarbleDyn): number[]
  {
    const n: number = this.n;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(this.target[i], garble.base[i]));
  }

  to_share(o: number): EvalDyn
  {
    const n: number = this.n;
    return new EvalDyn({ target: Array.from({length: o - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let v = 0;
  for (let j = 0; j < 8; j++)   {
    const bit = fieldBitand(this.target[fieldAdd(fieldMul(i, 8), j)], 1);
    v |= fieldShl(bit, j);
  }
  return v;
})()), n: 0 });
  }

  static zero(n: number): EvalDyn
  {
    return new EvalDyn({ target: Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => 0), n: 0 });
  }
}

export class GarbleDyn {
  n!: number;
  base!: number[];

  constructor(init: { 
    n: number,
    base: number[]
  }) {
    Object.assign(this, init);
  }

  and_result(ctx: { newD: () => any }, b: GarbleDyn): GarbleDyn
  {
    const n: number = this.n;
    let d = ctx.newD();
    d.update(this.base);
    d.update(b.base);
    const hash = [...d.finalize()];
    return new GarbleDyn({ base: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => hash[i]), n: 0 });
  }

  share(target: number[]): EvalDyn
  {
    const n: number = this.n;
    return new EvalDyn({ target: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(this.base[i], target[i])), n: 0 });
  }

  to_share(o: number): GarbleDyn
  {
    const n: number = this.n;
    return new GarbleDyn({ base: Array.from({length: o - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let v = 0;
  for (let j = 0; j < 8; j++)   {
    const bit = fieldBitand(this.base[fieldAdd(fieldMul(i, 8), j)], 1);
    v |= fieldShl(bit, j);
  }
  return v;
})()), n: 0 });
  }

  static zero(n: number): GarbleDyn
  {
    return new GarbleDyn({ base: Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => 0), n: 0 });
  }
}

export class GarbleTableDyn {
  n!: number;
  table!: number[][];

  constructor(init: { 
    n: number,
    table: number[][]
  }) {
    Object.assign(this, init);
  }
}

export class GlobalSecretDyn {
  n!: number;
  secret!: number[];

  constructor(init: { 
    n: number,
    secret: number[]
  }) {
    Object.assign(this, init);
  }

  encode(garble: GarbleDyn, value: boolean): EvalDyn
  {
    const n: number = this.n;
    return new EvalDyn({ target: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return (() => { if (value) {
  return fieldBitxor(this.secret[i], garble.base[i]);
} else {
  return garble.base[i];
} })();
})()), n: 0 });
  }

  gen_and_table(ctx: { newD: () => any }, a: GarbleDyn, b: GarbleDyn): GarbleTableDyn
  {
    const n: number = this.n;
    const result_base = a.and_result(b);
    let table = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => 0));
    for (let i = 0; i < 4; i++)     {
      const av = (fieldBitand(i, 1) !== 0);
      const bv = (fieldBitand(i, 2) !== 0);
      const ea = this.encode(a, av);
      const eb = this.encode(b, bv);
      const row = fieldBitor(Number(fieldBitand(ea.target[0], 1)), fieldShl(Number(fieldBitand(eb.target[0], 1)), 1));
      const result_label = this.encode(result_base, fieldBitand(av, bv));
      let d = ctx.newD();
      d.update(ea.target);
      d.update(eb.target);
      const hash = [...d.finalize()];
      table[row] = Array.from({length: n - 0}, (_, __i) => __i + 0).map((j: any) => fieldBitxor(hash[j], result_label.target[j]));
    }
    return new GarbleTableDyn({ table: table, n: 0 });
  }

  static new(n: number, secret: number[]): GlobalSecretDyn
  {
    secret[0] |= 1;
    return new GlobalSecretDyn({ secret: secret });
  }

  not_garble(a: GarbleDyn): GarbleDyn
  {
    const n: number = this.n;
    return new GarbleDyn({ base: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(a.base[i], this.secret[i])), n: 0 });
  }

  one_wire_eval(): EvalDyn
  {
    const n: number = this.n;
    return this.encode(Garble.zero(), true);
  }

  secret(): number[]
  {
    const n: number = this.n;
    return __clone(this.secret);
  }
}

export class GarbledCircuitDyn {
  n!: number;
  i!: number;
  a!: number;
  secret!: GlobalSecretDyn;
  input_labels!: GarbleDyn[];
  tables!: GarbleTableDyn[];
  output_label!: GarbleDyn;

  constructor(init: { 
    n: number,
    i: number,
    a: number,
    secret: GlobalSecretDyn,
    input_labels: GarbleDyn[],
    tables: GarbleTableDyn[],
    output_label: GarbleDyn
  }) {
    Object.assign(this, init);
  }

  encode_inputs(bits: boolean[]): EvalDyn[]
  {
    const n: number = this.n;
    const i: number = this.i;
    const a: number = this.a;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => this.secret.encode(this.input_labels[i], bits[i]));
  }

  eval_setup(): EvalSetupDyn
  {
    const n: number = this.n;
    const i: number = this.i;
    const a: number = this.a;
    return new EvalSetupDyn({ one_wire: this.secret.one_wire_eval(), tables: __clone(this.tables), output_label: __clone(this.output_label), n: 0, a: 0 });
  }
}

export class EvalSetupDyn {
  n!: number;
  a!: number;
  one_wire!: EvalDyn;
  tables!: GarbleTableDyn[];
  output_label!: GarbleDyn;

  constructor(init: { 
    n: number,
    a: number,
    one_wire: EvalDyn,
    tables: GarbleTableDyn[],
    output_label: GarbleDyn
  }) {
    Object.assign(this, init);
  }

  recover_output(result: EvalDyn): boolean
  {
    const n: number = this.n;
    const a: number = this.a;
    return (fieldBitand(result.open(this.output_label)[0], 1) !== 0);
  }
}

export class NoReduction {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  reduce(_word: GrafhenWordDyn)
  {
  }
}

export class GrafhenWordDyn {
  wbound!: number;
  data!: number[];
  len!: number;

  constructor(init: { 
    wbound: number,
    data: number[],
    len: number
  }) {
    Object.assign(this, init);
  }

  static identity(wbound: number): GrafhenWordDyn
  {
    return new GrafhenWordDyn({ data: Array.from({length: wbound}, () => 0), len: 0, wbound: 0 });
  }
}

export class GrafhenKeyDyn {
  n!: number;
  d!: number;
  gens!: number[][];
  inv_gens!: number[][];

  constructor(init: { 
    n: number,
    d: number,
    gens: number[][],
    inv_gens: number[][]
  }) {
    Object.assign(this, init);
  }
}

export class GrafhenPublicDyn<R> {
  wbound!: number;
  enc_one!: GrafhenWordDyn;
  and_w1!: GrafhenWordDyn;
  and_w2!: GrafhenWordDyn;
  reducer!: R;

  constructor(init: { 
    wbound: number,
    enc_one: GrafhenWordDyn,
    and_w1: GrafhenWordDyn,
    and_w2: GrafhenWordDyn,
    reducer: R
  }) {
    Object.assign(this, init);
  }
}

export class LweSampleDyn<T, U> {
  n!: number;
  m!: number;
  matrix!: any[][];
  b!: any[];

  constructor(init: { 
    n: number,
    m: number,
    matrix: any[][],
    b: any[]
  }) {
    Object.assign(this, init);
  }

  static new(n: number, m: number, matrix: any[][], b: any[]): LweSampleDyn
  {
    return new LweSampleDyn({ matrix: matrix, b: b });
  }

  static sample<S, P>(ctx: { defaultA: () => any }, n: number, m: number, matrix: any[][], s: any[], e: any[]): LweSampleDyn
  {
    return new LweSampleDyn({ b: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return fieldAdd(s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, b]: any) => fieldMul(__clone(b), __clone(matrix[i][a]))).reduce((a: any, b: any) => fieldAdd(a, b), ctx.defaultA()), __clone(e[i]));
})()), matrix: matrix });
  }
}

export class AllPartiesDyn<T> {
  n!: number;
  other_parties!: OtherPartiesDyn<any>;
  self_party!: any;

  constructor(init: { 
    n: number,
    other_parties: OtherPartiesDyn<any>,
    self_party: any
  }) {
    Object.assign(this, init);
  }
}

export class OtherPartiesDyn<T> {
  n!: number;
  other_parties!: any[];

  constructor(init: { 
    n: number,
    other_parties: any[]
  }) {
    Object.assign(this, init);
  }
}

export class BaseOtSenderDyn<G, D> {
  y!: number /* G::Scalar */;
  s!: number /* G::Element */;
  t!: number /* G::Element */;

  constructor(init: { 
    y: number /* G::Scalar */,
    s: number /* G::Element */,
    t: number /* G::Element */
  }) {
    Object.assign(this, init);
  }
}

export class BaseOtReceiverDyn<G, D> {
  x!: number /* G::Scalar */;
  s!: number /* G::Element */;
  c!: boolean;

  constructor(init: { 
    x: number /* G::Scalar */,
    s: number /* G::Element */,
    c: boolean
  }) {
    Object.assign(this, init);
  }
}

export class OtReceiverMsgDyn<G> {
  r!: number /* G::Element */;

  constructor(init: { 
    r: number /* G::Element */
  }) {
    Object.assign(this, init);
  }
}

export class ToyElement {
  _0!: bigint;

  constructor(_0: bigint) {
    this._0 = _0;
  }
}

export class ToyGroup {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  static add(a: ToyElement, b: ToyElement): ToyElement
  {
    return new ToyElement(toy_mul(a._0, b._0));
  }

  static generator(): ToyElement
  {
    return new ToyElement(TOY_G);
  }

  static neg(a: ToyElement): ToyElement
  {
    return new ToyElement(toy_pow(a._0, fieldSub(TOY_P, 2)));
  }

  static random_scalar<R>(rng: R): bigint
  {
    const lo = BigInt(rng.next_u32());
    const hi = BigInt(rng.next_u32());
    return (fieldBitor(fieldShl(hi, 32), lo) % fieldSub(TOY_P, 1));
  }

  static scalar_mul(elt: ToyElement, k: bigint): ToyElement
  {
    return new ToyElement(toy_pow(elt._0, k));
  }

  static write_element<D>(elt: ToyElement, h: D)
  {
    h.update([(elt._0) & 0xFF, ((elt._0) >> 8) & 0xFF, ((elt._0) >> 16) & 0xFF, ((elt._0) >> 24) & 0xFF]);
  }
}

export class IdealCotDyn<T> {
  n!: number;
  delta!: DeltaDyn<any>;

  constructor(init: { 
    n: number,
    delta: DeltaDyn<any>
  }) {
    Object.assign(this, init);
  }

  cot<R>(rng: R, sample_t: unknown /* impl Fn */, b: boolean): [any[], any[]]
  {
    const n: number = this.n;
    const r0 = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => sample_t(rng));
    const v = (() => { if (b) {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldAdd(__clone(r0[i]), __clone(this.delta.delta[i])));
} else {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(r0[i]));
} })();
    return [r0, v];
  }

  static new(n: number, delta: DeltaDyn<any>): IdealCotDyn
  {
    return new IdealCotDyn({ delta: delta });
  }
}

export class LweOtCrs {
  a!: Zq[][];
  h!: Zq[];

  constructor(init: { 
    a: Zq[][],
    h: Zq[]
  }) {
    Object.assign(this, init);
  }

  static sample<R>(rng: R): LweOtCrs
  {
    let a = Array.from({length: LWE_N}, () => Array.from({length: LWE_N}, () => 0));
    for (let i = 0; i < LWE_N; i++)     {
      for (let j = 0; j < LWE_N; j++)       {
        a[i][j] = sample_zq(rng);
      }
    }
    let h = Array.from({length: LWE_N}, () => 0);
    for (let i = 0; i < LWE_N; i++)     {
      h[i] = sample_zq(rng);
    }
    return new LweOtCrs({ a: a, h: h });
  }
}

export class LweOtReceiver {
  s!: Zq[];
  c!: boolean;

  constructor(init: { 
    s: Zq[],
    c: boolean
  }) {
    Object.assign(this, init);
  }
}

export class LweOtRecvMsg {
  pk0!: Zq[];

  constructor(init: { 
    pk0: Zq[]
  }) {
    Object.assign(this, init);
  }
}

export class LweOtSenderMsgDyn {
  l!: number;
  u0!: Zq[];
  v0!: Zq[];
  u1!: Zq[];
  v1!: Zq[];

  constructor(init: { 
    l: number,
    u0: Zq[],
    v0: Zq[],
    u1: Zq[],
    v1: Zq[]
  }) {
    Object.assign(this, init);
  }
}

export class SoftSpokenOutDyn<D> {
  m!: number;
  l!: number;
  sender_r0!: number[][];
  receiver_v!: number[][];
  sender_tag!: Output<D>;
  receiver_tag!: Output<D>;

  constructor(init: { 
    m: number,
    l: number,
    sender_r0: number[][],
    receiver_v: number[][],
    sender_tag: Output<D>,
    receiver_tag: Output<D>
  }) {
    Object.assign(this, init);
  }

  check(): boolean
  {
    const m: number = this.m;
    const l: number = this.l;
    return (this.sender_tag === this.receiver_tag);
  }
}

export class LweCiphertextDyn {
  n_lwe!: number;
  a!: number[];
  b!: number;

  constructor(init: { 
    n_lwe: number,
    a: number[],
    b: number
  }) {
    Object.assign(this, init);
  }
}

export class RlweCiphertextDyn {
  big_n!: number;
  a!: number[];
  b!: number[];

  constructor(init: { 
    big_n: number,
    a: number[],
    b: number[]
  }) {
    Object.assign(this, init);
  }
}

export class RgswRowDyn {
  big_n!: number;
  rlwe0!: RlweCiphertextDyn;
  rlwe1!: RlweCiphertextDyn;

  constructor(init: { 
    big_n: number,
    rlwe0: RlweCiphertextDyn,
    rlwe1: RlweCiphertextDyn
  }) {
    Object.assign(this, init);
  }
}

export class RgswCiphertextDyn {
  big_n!: number;
  bs_ell!: number;
  rows!: RgswRowDyn[];

  constructor(init: { 
    big_n: number,
    bs_ell: number,
    rows: RgswRowDyn[]
  }) {
    Object.assign(this, init);
  }
}

export class KeySwitchingKeyDyn {
  n_lwe!: number;
  big_n!: number;
  ks_ell!: number;
  ksk!: LweCiphertextDyn[][];
  ks_bg_log!: number;

  constructor(init: { 
    n_lwe: number,
    big_n: number,
    ks_ell: number,
    ksk: LweCiphertextDyn[][],
    ks_bg_log: number
  }) {
    Object.assign(this, init);
  }
}

export class BootstrappingKeyDyn {
  n_lwe!: number;
  big_n!: number;
  bs_ell!: number;
  ks_ell!: number;
  bsk!: RgswCiphertextDyn[];
  ksk!: KeySwitchingKeyDyn;
  bs_bg_log!: number;

  constructor(init: { 
    n_lwe: number,
    big_n: number,
    bs_ell: number,
    ks_ell: number,
    bsk: RgswCiphertextDyn[],
    ksk: KeySwitchingKeyDyn,
    bs_bg_log: number
  }) {
    Object.assign(this, init);
  }
}

export class LweSecretKeyDyn {
  n_lwe!: number;
  key!: number[];

  constructor(init: { 
    n_lwe: number,
    key: number[]
  }) {
    Object.assign(this, init);
  }
}

export class RlweSecretKeyDyn {
  big_n!: number;
  key!: number[];

  constructor(init: { 
    big_n: number,
    key: number[]
  }) {
    Object.assign(this, init);
  }
}

export class BitVoleDyn<T> {
  n!: number;
  u!: Bit[];
  v!: any[];

  constructor(init: { 
    n: number,
    u: Bit[],
    v: any[]
  }) {
    Object.assign(this, init);
  }
}

export class AdditiveHasher {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }

  static absorb(state: any, encoded: any)
  {
    const old = core.mem.take(state);
    state = fieldAdd(old, encoded);
  }

  static finalize_eq(produce: any, consume: any): boolean
  {
    return (produce === consume);
  }

  static new_state(ctx: { defaultT: () => any }): any
  {
    return ctx.defaultT();
  }
}

export class ChallengeKeyDyn<T> {
  r1!: any;
  r2!: any;
  r3!: any;

  constructor(init: { 
    r1: any,
    r2: any,
    r3: any
  }) {
    Object.assign(this, init);
  }

  static from_challenge(r: any): ChallengeKeyDyn
  {
    const r2 = fieldMul(__clone(r), __clone(r));
    const r3 = fieldMul(__clone(r2), __clone(r));
    return new ChallengeKeyDyn({ r1: r, r2: r2, r3: r3 });
  }
}

export class MemoryCheckStateDyn<T, H> {
  key!: ChallengeKeyDyn<any>;
  produce!: number /* H::State */;
  consume!: number /* H::State */;

  constructor(init: { 
    key: ChallengeKeyDyn<any>,
    produce: number /* H::State */,
    consume: number /* H::State */
  }) {
    Object.assign(this, init);
  }

  drain(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, final_value: any, final_timestamp: bigint)
  {
    const enc = this.encode(addr, final_value, final_timestamp);
    ctx.HClass.absorb(this.consume, enc);
  }

  encode(addr: any, value: any, timestamp: bigint): any
  {
    const a = fieldMul(addr, __clone(this.key.r1));
    const v = fieldMul(value, __clone(this.key.r2));
    const t = this.scale_by_u64(__clone(this.key.r3), timestamp);
    return fieldAdd(fieldAdd(a, v), t);
  }

  init(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, zero_value: any)
  {
    const enc = this.encode(addr, zero_value, 0);
    ctx.HClass.absorb(this.produce, enc);
  }

  static new(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, key: ChallengeKeyDyn<any>): MemoryCheckStateDyn
  {
    return new MemoryCheckStateDyn({ key: key, produce: ctx.HClass.new_state(), consume: ctx.HClass.new_state() });
  }

  read(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, value: any, timestamp: bigint, write_timestamp: bigint)
  {
    const enc_produce = this.encode(__clone(addr), __clone(value), timestamp);
    ctx.HClass.absorb(this.produce, enc_produce);
    const enc_consume = this.encode(addr, value, write_timestamp);
    ctx.HClass.absorb(this.consume, enc_consume);
  }

  scale_by_u64(ctx: { defaultT: () => any }, x: any, n: bigint): any
  {
    if ((n === 0))     {
      return ctx.defaultT();
    }
    if ((n === 1))     {
      return x;
    }
    let acc = ctx.defaultT();
    let base = x;
    for (let bit = 0; bit < 64; bit++)     {
      if ((fieldBitand(fieldShr(n, bit), 1) === 1))       {
        acc = fieldAdd(acc, __clone(base));
      }
      base = fieldAdd(__clone(base), base);
    }
    return acc;
  }

  verify(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }): boolean
  {
    return ctx.HClass.finalize_eq(this.produce, this.consume);
  }

  write(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, new_value: any, timestamp: bigint, old_value: any, old_timestamp: bigint)
  {
    const enc_new = this.encode(__clone(addr), new_value, timestamp);
    ctx.HClass.absorb(this.produce, enc_new);
    const enc_old = this.encode(addr, old_value, old_timestamp);
    ctx.HClass.absorb(this.consume, enc_old);
  }
}

export class PolyDyn<T> {
  n!: number;
  c0!: any;
  c1!: any[];

  constructor(init: { 
    n: number,
    c0: any,
    c1: any[]
  }) {
    Object.assign(this, init);
  }

  apply<O>(ctx: { defaultO: () => any }, m: number, x: number, x2: number, xs: number, s: number, voles: VopeDyn<any>[][]): VopeDyn<any>
  {
    const n: number = this.n;
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    let b: any = __clone(this.c1[k]);
    for (const v of voles)     {
      b = fieldMul(b, __clone(v[k].v[i]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = __clone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    for (let n = 0; n < x; n++)     {
      let b: any = __clone(this.c1[k]);
      for (let m = 0; m < s; m++)       {
        const l_1 = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if ((idx === n)) {
  return __clone(v[k].u[l_1][i]);
} else {
  return __clone(v[k].v[i]);
} })());
        }
      }
      sum = fieldAdd(sum, b);
    }
  }
  return sum;
})());
})());
    return new VopeDyn({ u: u, v: v, n: 0, k: 1 });
  }

  apply_pool<O>(ctx: { defaultO: () => any }, m: number, x: number, x2: number, xs: number, s: number, voles: PolyInputPoolDyn<VopeDyn<any>>): VopeDyn<any>
  {
    const n: number = this.n;
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    let b: any = __clone(this.c1[k]);
    for (const v of voles.indices)     {
      b = fieldMul(b, __clone(voles.inputs[v[k]].v[i]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = __clone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    for (let n = 0; n < x; n++)     {
      let b: any = __clone(this.c1[k]);
      for (let m = 0; m < s; m++)       {
        const l_1 = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.indices.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if ((idx === n)) {
  return __clone(voles.inputs[v[k]].u[l_1][i]);
} else {
  return __clone(voles.inputs[v[k]].v[i]);
} })());
        }
      }
      sum = fieldAdd(sum, b);
    }
  }
  return sum;
})());
})());
    return new VopeDyn({ u: u, v: v, n: 0, k: 1 });
  }

  get_qs<Q, A>(m: number, x: number, root: DeltaDyn<any>, inputs: QDyn<any>[][], reduction: number): QDyn<any>
  {
    const n: number = this.n;
    return new Q({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum: any = __clone(this.c0);
  for (let _ = 0; _ < n; _++)   {
    sum = fieldMul(__clone(root.delta[i]), sum);
  }
  for (let j = 0; j < n; j++)   {
    let b: any = __clone(this.c1[j]);
    for (const i2 of inputs)     {
      for (let _ = 0; _ < reduction; _++)       {
        b = fieldMul(__clone(i2[j].q[i]), b);
      }
    }
    sum = fieldAdd(sum, b);
  }
  return sum;
})()) });
  }

  get_qs_pool<Q, A>(m: number, x: number, root: DeltaDyn<any>, inputs: PolyInputPoolDyn<QDyn<any>>, reduction: number): QDyn<any>
  {
    const n: number = this.n;
    return new Q({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum: any = __clone(this.c0);
  for (let _ = 0; _ < n; _++)   {
    sum = fieldMul(__clone(root.delta[i]), sum);
  }
  for (let j = 0; j < n; j++)   {
    let b: any = __clone(this.c1[j]);
    for (const i2 of inputs.indices)     {
      for (let _ = 0; _ < reduction; _++)       {
        b = fieldMul(__clone(inputs.inputs[i2[j]].q[i]), b);
      }
    }
    sum = fieldAdd(sum, b);
  }
  return sum;
})()) });
  }
}

export class PolyInputPoolDyn<T> {
  n!: number;
  x!: number;
  inputs!: any[];
  indices!: number[][];

  constructor(init: { 
    n: number,
    x: number,
    inputs: any[],
    indices: number[][]
  }) {
    Object.assign(this, init);
  }
}

export class VopeDyn<T> {
  n!: number;
  k!: number;
  u!: any[][];
  v!: any[];

  constructor(init: { 
    n: number,
    k: number,
    u: any[][],
    v: any[]
  }) {
    Object.assign(this, init);
  }

  add(rhs: VopeDyn<any>): number /* VopeDyn<T>::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldAdd(__clone(this.u[l][i]), __clone(rhs.u[l][i])));
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldAdd(__clone(this.v[i]), __clone(rhs.v[i]))), n: 0, k: 1 });
  }

  bitxor(rhs: any[]): number /* VopeDyn<T>::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  const o: any = fieldBitxor(__clone(this.u[i][j]), __clone(rhs[fieldAdd(fieldMul(i, k), j)]));
  return o;
})());
})()), v: this.v.map((a: any) => a), n: 0, k: 1 });
  }

  clone(): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(u[l][i]))), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(v[i])), n: 0, k: 1 });
  }

  static constant(n: number, v: any[]): VopeDyn
  {
    const k: number = 0;
    return new VopeDyn({ u: Array.from({length: 0 - 0}, (_, __i) => __i + 0).map((_: any) => (() => { throw new Error("unreachable"); })()), v: v, n: 0, k: 1 });
  }

  eq(other: VopeDyn<T>): boolean
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u: u1, v: v1 } = this;
    const { u: u2, v: v2 } = other;
    for (let l = 0; l < k; l++)     {
      for (let i = 0; i < n; i++)       {
        if ((u1[l][i] !== u2[l][i]))         {
          return false;
        }
      }
    }
    for (let i = 0; i < n; i++)     {
      if ((v1[i] !== v2[i]))       {
        return false;
      }
    }
    return true;
  }

  expand(ctx: { defaultT: () => any }, l: number): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: l - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => ((u?.[l]) != null ? ((a) => __clone(a[i]))(u?.[l]) : (ctx.defaultT())));
})()), v: __clone(v), n: 0, k: 1 });
  }

  mul(rhs: DeltaDyn<any>): number /* VopeDyn<T>::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new QDyn({ q: this.u.map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  let x = __clone(rhs.delta[i]);
  for (let _ = 0; _ < i; _++)   {
    x = fieldMul(x, __clone(rhs.delta[i]));
  }
  const m: any = fieldMul(__clone(b[j]), x);
  return fieldAdd(m, __clone(a[j]));
})());
})(), this.v.map((a: any) => a)), n: 0 });
  }

  mul_generalized(ctx: { defaultT: () => any }, k2: number, other: VopeDyn<any>): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    let res_u = Array.from({length: fieldAdd(k2, k) - 0}, (_, __i) => __i + 0).map((_: any) => []);
    let res_v = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => ctx.defaultT());
    for (let i = 0; i <= k; i++)     {
      for (let j = 0; j <= k2; j++)       {
        const k_1 = fieldAdd(i, j);
        const a_coeff = (() => { if ((i === 0)) {
  return this.v;
} else {
  return this.u[fieldSub(i, 1)];
} })();
        const b_coeff = (() => { if ((j === 0)) {
  return other.v;
} else {
  return other.u[fieldSub(j, 1)];
} })();
        if ((k_1 === 0))         {
          for (let lane = 0; lane < n; lane++)           {
            res_v[lane] = fieldAdd(__clone(res_v[lane]), fieldMul(__clone(a_coeff[lane]), __clone(b_coeff[lane])));
          }
        } else         {
          for (let lane = 0; lane < n; lane++)           {
            res_u[fieldSub(k_1, 1)][lane] = fieldAdd(__clone(res_u[fieldSub(k_1, 1)][lane]), fieldMul(__clone(a_coeff[lane]), __clone(b_coeff[lane])));
          }
        }
      }
    }
    return new VopeDyn({ u: res_u, v: res_v, n: 0, k: 1 });
  }

  remap(m: number, f: (arg: number) => number): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => __clone(u[l][(f(i) % n)]));
})()), v: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => __clone(v[(f(i) % n)])), n: 0, k: 1 });
  }

  rotate_left(n_param: number): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(this.n, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: number): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(this.n, (a) => wrappingAdd(a, n_param));
  }

  scale<T>(f: (arg: boolean) => any): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(u[l][i]);
  return f(b);
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(v[i]);
  return f(b);
})()), n: 0, k: 1 });
  }

  bit(n_param: number): VopeDyn<Bit>
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(u[l][i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(v[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(u[l][i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(v[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0, k: 1 });
    }
  }

  rotate_left_bits(n_param: number): VopeDyn<T>
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(u[l][i]);
  const next = __clone(u[l][(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(v[i]);
  const next = __clone(v[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(u[l][i]);
  const next = __clone(u[l][(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(v[i]);
  const next = __clone(v[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})()), n: 0, k: 1 });
    }
  }

  rotate_right_bits(n_param: number): VopeDyn<T>
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(u[l][(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(u[l][i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(v[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(v[i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(u[l][(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(u[l][i]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n_param))), fieldShr(b, Number(n_param))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(v[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(v[i]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0, k: 1 });
    }
  }
}

export class DeltaDyn<T> {
  n!: number;
  delta!: any[];

  constructor(init: { 
    n: number,
    delta: any[]
  }) {
    Object.assign(this, init);
  }

  clone(): DeltaDyn<T>
  {
    const n: number = this.n;
    const { delta } = this;
    return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(delta[i])), n: 0 });
  }

  eq(other: DeltaDyn<T>): boolean
  {
    const n: number = this.n;
    const { delta: d1 } = this;
    const { delta: d2 } = other;
    for (let i = 0; i < n; i++)     {
      if ((d1[i] !== d2[i]))       {
        return false;
      }
    }
    return true;
  }

  remap(m: number, f: (arg: number) => number): DeltaDyn<any>
  {
    const n: number = this.n;
    const { delta } = this;
    return new DeltaDyn({ delta: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => __clone(delta[(f(i) % n)])), n: 0 });
  }

  rotate_left(n_param: number): DeltaDyn<T>
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: number): DeltaDyn<T>
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingAdd(a, n_param));
  }

  static_<U, O>(val: any[]): QDyn<any>
  {
    const n: number = this.n;
    return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldMul(__clone(val[i]), __clone(this.delta[i]))), n: 0 });
  }

  bit(n_param: number): DeltaDyn<Bit>
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(delta[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(delta[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    }
  }

  rotate_left_bits(n_param: number): DeltaDyn<T>
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(delta[i]);
  const next = __clone(delta[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(delta[i]);
  const next = __clone(delta[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})()), n: 0 });
    }
  }

  rotate_right_bits(n_param: number): DeltaDyn<T>
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(delta[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(delta[i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(delta[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(delta[i]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    }
  }
}

export class QDyn<T> {
  n!: number;
  q!: any[];

  constructor(init: { 
    n: number,
    q: any[]
  }) {
    Object.assign(this, init);
  }

  clone(): QDyn<T>
  {
    const n: number = this.n;
    const { q } = this;
    return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(q[i])), n: 0 });
  }

  eq(other: QDyn<T>): boolean
  {
    const n: number = this.n;
    const { q: q1 } = this;
    const { q: q2 } = other;
    for (let i = 0; i < n; i++)     {
      if ((q1[i] !== q2[i]))       {
        return false;
      }
    }
    return true;
  }

  remap(m: number, f: (arg: number) => number): QDyn<any>
  {
    const n: number = this.n;
    const { q } = this;
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => __clone(q[(f(i) % n)])), n: 0 });
  }

  rotate_left(n_param: number): QDyn<T>
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: number): QDyn<T>
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingAdd(a, n_param));
  }

  bit(n_param: number): QDyn<Bit>
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(q[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(q[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    }
  }

  rotate_left_bits(n_param: number): QDyn<T>
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(q[i]);
  const next = __clone(q[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = __clone(q[i]);
  const next = __clone(q[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})()), n: 0 });
    }
  }

  rotate_right_bits(n_param: number): QDyn<T>
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(q[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(q[i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = __clone(q[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = __clone(q[i]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    }
  }
}

export function add_round_key(state: number[], round_key: number[])
{
  for (let i = 0; i < BLOCK; i++)   {
    state[i] ^= round_key[i];
  }
}

export function add_to_lower_word(iv: number[], counter: number): number[]
{
  let out = iv;
  const lower = u32.from_le_bytes([out[0], out[1], out[2], out[3]]);
  const new_ = wrappingAdd(lower, counter);
  const bytes = [(new_) & 0xFF, ((new_) >> 8) & 0xFF, ((new_) >> 16) & 0xFF, ((new_) >> 24) & 0xFF];
  out[0] = bytes[0];
  out[1] = bytes[1];
  out[2] = bytes[2];
  out[3] = bytes[3];
  return out;
}

export function add_to_upper_word(iv: number[], tweak: number)
{
  const upper = u32.from_le_bytes([iv[12], iv[13], iv[14], iv[15]]);
  const new_ = wrappingAdd(upper, tweak);
  const bytes = [(new_) & 0xFF, ((new_) >> 8) & 0xFF, ((new_) >> 16) & 0xFF, ((new_) >> 24) & 0xFF];
  iv[12] = bytes[0];
  iv[13] = bytes[1];
  iv[14] = bytes[2];
  iv[15] = bytes[3];
}

export function aes_ctr_prg(seed: number[], iv: number[], tweak: number, out_bytes: number): Vec<number>
{
  let iv_tweaked = iv;
  add_to_upper_word(iv_tweaked, tweak);
  const n_full = (out_bytes / BLOCK);
  const rem = (out_bytes % BLOCK);
  let out = alloc.vec.Vec.with_capacity(out_bytes);
  for (let i = 0; i < n_full; i++)   {
    const block_in = add_to_lower_word(iv_tweaked, Number(i));
    const ct = encrypt_block(seed, block_in);
    out.extend_from_slice(ct);
  }
  if ((rem > 0))   {
    const block_in = add_to_lower_word(iv_tweaked, Number(n_full));
    const ct = encrypt_block(seed, block_in);
    out.extend_from_slice(ct.slice(0, rem));
  }
  return out;
}

export function and_test_poly(big_n: number): number[]
{
  let v = Array.from({length: big_n}, () => 0);
  const half_q4 = fieldShr(Q4, 1);
  for (let k = 0; k < (big_n / 2); k++)   {
    v[k] = (-((half_q4)) >>> 0);
  }
  for (let k = (big_n / 2); k < big_n; k++)   {
    v[k] = half_q4;
  }
  return v;
}

export function blind_rotate(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, ct: LweCiphertextDyn, bk: BootstrappingKeyDyn): RlweCiphertextDyn
{
  return blind_rotate_with_poly(ct, and_test_poly(), bk);
}

export function blind_rotate_with_poly(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, ct: LweCiphertextDyn, test_poly: number[], bk: BootstrappingKeyDyn): RlweCiphertextDyn
{
  let acc = new RlweCiphertextDyn({ a: Array.from({length: big_n}, () => 0), b: test_poly, big_n: 0 });
  const two_n = fieldMul(2, big_n);
  const log2_two_n = Math.clz32((two_n) & -((two_n) | 0));
  const scale_shift = (32 - (log2_two_n));
  const b_exp = torus_to_exp(ct.b, scale_shift, two_n);
  if ((b_exp !== 0))   {
    acc = rlwe_rotate(acc, fieldSub(two_n, b_exp));
  }
  for (let i = 0; i < n_lwe; i++)   {
    const a_exp = torus_to_exp(ct.a[i], scale_shift, two_n);
    if ((a_exp !== 0))     {
      const acc_rotated = rlwe_rotate(acc, a_exp);
      acc = cmux(bk.bsk[i], acc_rotated, acc, bk.bs_bg_log);
    }
  }
  return acc;
}

export function chall1(mu: readonly number[], iv: number[], com_bytes: readonly number[], lambda_plus_b: number, use_shake256: boolean): Vec<number>
{
  let t = (() => { if (use_shake256) {
  return FaestTranscript.new_shake256();
} else {
  return FaestTranscript.new_shake128();
} })();
  t.absorb(mu);
  t.absorb(asRefU8(iv));
  t.absorb(com_bytes);
  return t.squeeze(lambda_plus_b);
}

export function chall2(chall_1: readonly number[], u_hat: readonly number[], d: readonly number[], lambda_plus_b: number, use_shake256: boolean): Vec<number>
{
  let t = (() => { if (use_shake256) {
  return FaestTranscript.new_shake256();
} else {
  return FaestTranscript.new_shake128();
} })();
  t.absorb(chall_1);
  t.absorb(u_hat);
  t.absorb(d);
  return t.squeeze(lambda_plus_b);
}

export function chall3(chall_2: readonly number[], a_hat: readonly number[], b_hat: readonly number[], c_hat: readonly number[], lambda: number, use_shake256: boolean): Vec<number>
{
  let t = (() => { if (use_shake256) {
  return FaestTranscript.new_shake256();
} else {
  return FaestTranscript.new_shake128();
} })();
  t.absorb(chall_2);
  t.absorb(a_hat);
  t.absorb(b_hat);
  t.absorb(c_hat);
  return t.squeeze(lambda);
}

export function cmux(big_n: number, bs_ell: number, c: RgswCiphertextDyn, d1: RlweCiphertextDyn, d0: RlweCiphertextDyn, bs_bg_log: number): RlweCiphertextDyn
{
  const diff = rlwe_sub(d1, d0);
  const prod = external_product(c, diff, bs_bg_log);
  return rlwe_add(d0, prod);
}

export function concat_small_voles(outs: Vec<ConvertOutput>): BigVoleProver
{
  const l_hat = outs[0].u.length;
  for (const o of outs)   {
    for (const vj of o.v)     {
    }
  }
  const u = __clone(outs[0].u);
  let c: Vec<Vec<number>> = /* Vec::with_capacity */ Array(fieldSub(outs.length, 1));
  for (const o of outs.slice(1))   {
    let ci = __clone(o.u);
    xor_in_place(ci, u);
    (c).push(ci);
  }
  let v_columns: Vec<Vec<number>> = []();
  for (const o of outs)   {
    for (const vj of o.v)     {
      (v_columns).push(vj);
    }
  }
  return new BigVoleProver({ u: u, c: c, v_columns: v_columns });
}

export function concat_small_voles_verifier(outs: Vec<ConvertOutput>, deltas: readonly number[], corrections: readonly Vec<number>[]): BigVoleVerifier
{
  let q_columns: Vec<Vec<number>> = []();
  for (const [i, o] of outs.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const k = o.v.length;
    const delta_i = deltas[i];
    for (const [bit, vj_raw] of o.v.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      let q = vj_raw;
      if ((i >= 1))       {
        const delta_bit = (fieldBitand(fieldShr(delta_i, bit), 1) === 1);
        if (delta_bit)         {
          xor_in_place(q, corrections[fieldSub(i, 1)]);
        }
      }
      (q_columns).push(q);
    }
    const _ = k;
  }
  return new BigVoleVerifier({ q_columns: q_columns });
}

export function concat_words(wbound: number, a: GrafhenWordDyn, b: GrafhenWordDyn): (GrafhenWordDyn | undefined)
{
  const new_len = (() => { const __match = (a.len + (b.len)); if (__match !== null && __match !== undefined) { const n = __match;
return n; } else { return undefined; } })();
  let result = GrafhenWord.identity();
  (result.data.slice(0, a.len)).splice(0, (a.data.slice(0, a.len)).length, ...(a.data.slice(0, a.len)));
  (result.data.slice(a.len, new_len)).splice(0, (b.data.slice(0, b.len)).length, ...(b.data.slice(0, b.len)));
  result.len = new_len;
  return result;
}

export function convert_to_vole(seeds: readonly (number[] | undefined)[], iv: number[], tweak: number, l_hat_bytes: number): ConvertOutput
{
  const n = seeds.length;
  const d = Number(Math.clz32((n) & -((n) | 0)));
  const zero_block = [];
  let r: Vec<Vec<number>> = /* Vec::with_capacity */ Array(n);
  for (const s of seeds)   {
    return (() => { const __match = s; if (__match !== null && __match !== undefined) { const seed = __match;
return (r).push(aes_ctr_prg(seed, iv, tweak, l_hat_bytes)); } else { return (r).push(__clone(zero_block)); } })();
  }
  let v: Vec<Vec<number>> = Array.from({length: d - 0}, (_, __i) => __i + 0).map((_: any) => []);
  let level: Vec<Vec<number>> = r;
  for (let j = 0; j < d; j++)   {
    const half = (level.length / 2);
    let next: Vec<Vec<number>> = /* Vec::with_capacity */ Array(half);
    for (let i = 0; i < half; i++)     {
      xor_in_place(v[j], level[fieldAdd(fieldMul(2, i), 1)]);
      let new_entry = __clone(level[fieldMul(2, i)]);
      xor_in_place(new_entry, level[fieldAdd(fieldMul(2, i), 1)]);
      (next).push(new_entry);
    }
    level = next;
  }
  const u = (level.next())!;
  return new ConvertOutput({ u: u, v: v });
}

export function create_vole_from_material(ctx: { B_OutputSize: number }, s: readonly any[]): VopeDyn<number>
{
  const u: number[] = s.reduce((a: any, b: any) => (() => {
  return Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(a[i], asRefU8(b)[i]));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
  const v: number[] = s.map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((j: any) => fieldBitxor(fieldBitxor(a[j], asRefU8(b)[j]), ((i) & 0xFF)));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
  return new VopeDyn({ u: Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_: any) => __clone(u)), v: v, n: 0, k: 1 });
}

export function create_vole_from_material_expanded(ctx: { B_OutputSize: number }, s: readonly any[], f: (arg: Uint8Array) => any): VopeDyn<number>
{
  const u: number[] = s.map((b: any) => f(asRefU8(b).slice(0, ctx.B_OutputSize))).reduce((a: any, b: any) => (() => {
  return Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i: any) => fieldBitxor(a[i], asRefU8(b)[i]));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
  const v: number[] = s.map((b: any) => f(asRefU8(b).slice(0, ctx.B_OutputSize))).map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((j: any) => fieldBitxor(fieldBitxor(a[j], asRefU8(b)[j]), ((i) & 0xFF)));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
  return new VopeDyn({ u: Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_: any) => __clone(u)), v: v, n: 0, k: 1 });
}

export function derive_and_q<T>(n: number, delta: DeltaDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, hat: any[]): QDyn<any>
{
  return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const lhs = fieldAdd(fieldMul(__clone(q_a.q[i]), __clone(q_b.q[i])), __clone(hat[i]));
  return fieldMul(lhs, delta.delta[i].invert());
})()), n: 0 });
}

export function ed_add(p1: EdPoint, p2: EdPoint): EdPoint
{
  const a = fe_mul(fe_sub(p1.y, p1.x), fe_sub(p2.y, p2.x));
  const b = fe_mul(fe_add(p1.y, p1.x), fe_add(p2.y, p2.x));
  const c = fe_mul(fe_mul(p1.t, D2), p2.t);
  const d_ = fe_add(fe_mul(p1.z, p2.z), fe_mul(p1.z, p2.z));
  const e = fe_sub(b, a);
  const f = fe_sub(d_, c);
  const g = fe_add(d_, c);
  const h = fe_add(b, a);
  return new EdPoint({ x: fe_mul(e, f), y: fe_mul(g, h), t: fe_mul(e, h), z: fe_mul(f, g) });
}

export function ed_double(p: EdPoint): EdPoint
{
  const a = fe_sq(p.x);
  const b = fe_sq(p.y);
  const c = fe_add(fe_sq(p.z), fe_sq(p.z));
  const d_ = fe_neg(a);
  const xy_sum = fe_add(p.x, p.y);
  const e = fe_sub(fe_sub(fe_sq(xy_sum), a), b);
  const g = fe_add(d_, b);
  const f = fe_sub(g, c);
  const h = fe_sub(d_, b);
  return new EdPoint({ x: fe_mul(e, f), y: fe_mul(g, h), t: fe_mul(e, h), z: fe_mul(f, g) });
}

export function ed_neg(p: EdPoint): EdPoint
{
  return new EdPoint({ x: fe_neg(p.x), y: p.y, z: p.z, t: fe_neg(p.t) });
}

export function ed_scalar_mul(p: EdPoint, k: number[]): EdPoint
{
  let acc = EdPoint.IDENTITY;
  for (const byte_idx of (Array.from({length: 32 - 0}, (_, i) => i + 0)).slice().reverse())   {
    for (const bit of (Array.from({length: 8 - 0}, (_, i) => i + 0)).slice().reverse())     {
      acc = ed_double(acc);
      const b = fieldBitand(fieldShr(k[byte_idx], bit), 1);
      if ((b === 1))       {
        acc = ed_add(acc, p);
      }
    }
  }
  return acc;
}

export function encrypt_block(key: number[], plain: number[]): number[]
{
  const round_keys = key_expansion(key);
  let state = plain;
  add_round_key(state, round_keys[0]);
  for (let r = 1; r < NR; r++)   {
    sub_bytes(state);
    shift_rows(state);
    mix_columns(state);
    add_round_key(state, round_keys[r]);
  }
  sub_bytes(state);
  shift_rows(state);
  add_round_key(state, round_keys[NR]);
  return state;
}

export function encrypt_branch<R>(l: number, rng: R, crs: LweOtCrs, pk: Zq[], msg: number[]): [Zq[], Zq[]]
{
  let r = Array.from({length: LWE_N}, () => 0);
  for (let i = 0; i < LWE_N; i++)   {
    r[i] = sample_noise(rng);
  }
  let u = Array.from({length: LWE_N}, () => 0);
  for (let j = 0; j < LWE_N; j++)   {
    let acc: Zq = 0;
    for (let i = 0; i < LWE_N; i++)     {
      acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
    }
    acc = zq_add(acc, sample_noise(rng));
    u[j] = acc;
  }
  let base: Zq = 0;
  for (let i = 0; i < LWE_N; i++)   {
    base = zq_add(base, zq_mul(pk[i], r[i]));
  }
  const half_q = (LWE_Q / 2);
  let v = Array.from({length: l}, () => 0);
  for (let k = 0; k < l; k++)   {
    const plain = (() => { if ((fieldBitand(msg[k], 1) === 1)) {
  return half_q;
} else {
  return 0;
} })();
    v[k] = zq_add(zq_add(base, sample_noise(rng)), plain);
  }
  return [u, v];
}

export function eval_word_to_perm(n: number, d: number, wbound: number, key: GrafhenKeyDyn, word: GrafhenWordDyn): number[]
{
  let perm: number[] = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => ((i) & 0xFF));
  for (const g of word.data.slice(0, word.len))   {
    const g_1 = Number(g);
    const generator: number[] = (() => { if ((g_1 < d)) {
  return key.gens[g_1];
} else {
  return key.inv_gens[fieldSub(g_1, d)];
} })();
    perm = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => generator[Number(perm[i])]);
  }
  return perm;
}

export function expand_challenge_to_deltas(chall_1: readonly number[], tau: number, n: number): Vec<number>
{
  return Array.from({length: tau - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const byte = Number(chall_1[(i % chall_1.length)]);
  return (byte % n);
})());
}

export function external_product(big_n: number, bs_ell: number, rgsw: RgswCiphertextDyn, rlwe: RlweCiphertextDyn, bs_bg_log: number): RlweCiphertextDyn
{
  const a_decomp = poly_decompose(rlwe.a, bs_bg_log);
  const b_decomp = poly_decompose(rlwe.b, bs_bg_log);
  let out_a = Array.from({length: big_n}, () => 0);
  let out_b = Array.from({length: big_n}, () => 0);
  for (let j = 0; j < bs_ell; j++)   {
    const row = rgsw.rows[j];
    const prod_a0 = poly_mul_neg(a_decomp[j], row.rlwe0.a);
    const prod_a1 = poly_mul_neg(a_decomp[j], row.rlwe0.b);
    const prod_b0 = poly_mul_neg(b_decomp[j], row.rlwe1.a);
    const prod_b1 = poly_mul_neg(b_decomp[j], row.rlwe1.b);
    for (let k = 0; k < big_n; k++)     {
      out_a[k] = wrappingAdd(wrappingAdd(out_a[k], prod_a0[k]), prod_b0[k]);
      out_b[k] = wrappingAdd(wrappingAdd(out_b[k], prod_a1[k]), prod_b1[k]);
    }
  }
  return new RlweCiphertextDyn({ a: out_a, b: out_b, big_n: 0 });
}

export function fe_add(a: Fe25519, b: Fe25519): Fe25519
{
  let r = Array.from({length: 4}, () => 0);
  let c: bigint = 0;
  for (let i = 0; i < 4; i++)   {
    const v = fieldAdd(fieldAdd((a._0[i] as unknown as bigint), (b._0[i] as unknown as bigint)), (c as unknown as bigint));
    r[i] = BigInt(v);
    c = BigInt(fieldShr(v, 64));
  }
  if ((c !== 0))   {
    let c2: bigint = fieldMul((c as unknown as bigint), 38);
    for (let i = 0; i < 4; i++)     {
      const v = fieldAdd((r[i] as unknown as bigint), c2);
      r[i] = BigInt(v);
      c2 = fieldShr(v, 64);
    }
  }
  return fe_canonicalize(r);
}

export function fe_canonicalize(a: bigint[]): Fe25519
{
  let x = a;
  for (let _ = 0; _ < 2; _++)   {
    let tmp = Array.from({length: 4}, () => 0);
    let borrow: bigint = 0;
    for (let i = 0; i < 4; i++)     {
      const [r1, b1] = x[i].overflowing_sub(P_LIMBS[i]);
      const [r2, b2] = r1.overflowing_sub(borrow);
      tmp[i] = r2;
      borrow = fieldBitor(BigInt(b1), BigInt(b2));
    }
    if ((borrow === 0))     {
      x = tmp;
    }
  }
  return new Fe25519(x);
}

export function fe_const(limbs: bigint[]): Fe25519
{
  return new Fe25519(limbs);
}

export function fe_invert(a: Fe25519): Fe25519
{
  const exp_limbs: bigint[] = [BigInt("18446744073709551595"), BigInt("18446744073709551615"), BigInt("18446744073709551615"), BigInt("9223372036854775807")];
  let acc = Fe25519.ONE;
  for (const limb_idx of (Array.from({length: 4 - 0}, (_, i) => i + 0)).slice().reverse())   {
    for (const bit of (Array.from({length: 64 - 0}, (_, i) => i + 0)).slice().reverse())     {
      acc = fe_sq(acc);
      const b = fieldBitand(fieldShr(exp_limbs[limb_idx], bit), 1);
      if ((b === 1))       {
        acc = fe_mul(acc, a);
      }
    }
  }
  return acc;
}

export function fe_mul(a: Fe25519, b: Fe25519): Fe25519
{
  const wide = mul_4x4(a._0, b._0);
  return reduce_wide(wide);
}

export function fe_neg(a: Fe25519): Fe25519
{
  return (() => { if (a.is_zero()) {
  return Fe25519.ZERO;
} else {
  let neg = Array.from({length: 4}, () => 0);
  let borrow: bigint = 0;
  for (let i = 0; i < 4; i++)   {
    const [r1, br1] = P_LIMBS[i].overflowing_sub(a._0[i]);
    const [r2, br2] = r1.overflowing_sub(borrow);
    neg[i] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return new Fe25519(neg);
} })();
}

export function fe_sq(a: Fe25519): Fe25519
{
  return fe_mul(a, a);
}

export function fe_sub(a: Fe25519, b: Fe25519): Fe25519
{
  let neg_b = Array.from({length: 4}, () => 0);
  let borrow: bigint = 0;
  for (let i = 0; i < 4; i++)   {
    const [r1, br1] = P_LIMBS[i].overflowing_sub(b._0[i]);
    const [r2, br2] = r1.overflowing_sub(borrow);
    neg_b[i] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return fe_add(a, new Fe25519(neg_b));
}

export function gen_abo<B, D>(ctx: { newD: () => any }, k: number, n: number, a: number[], rand: readonly number[]): ABODyn<B, D>
{
  let h = ctx.newD();
  const per_byte = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_ni: any) => (() => {
  let per_byte = Array.from({length: k - 0}, (_, __i) => __i + 0).map((_: any) => []);
  for (let i = 0; i < k; i++)   {
    const core = Array.from({length: ilog2(k) - 0}, (_, __i) => __i + 0).reduce((acc: any, b: any) => (() => {
  if ((fieldBitand(fieldShr(i, b), 1) !== 0))   {
    const doubled = doubleVec(acc);
    acc = __clone(doubled[1]);
  } else   {
    const doubled = doubleVec(acc);
    acc = __clone(doubled[0]);
  }
  return acc;
})(), __clone(a));
    h.update(hashCommit(core, rand));
    per_byte[i] = core;
  }
  return per_byte;
})());
  return new ABODyn({ commit: [...h.finalize()], per_byte: per_byte, k: 0, n: 0 });
}

export function gen_bootstrapping_key<R>(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, lwe_sk: LweSecretKeyDyn, rlwe_sk: RlweSecretKeyDyn, bs_bg_log: number, ks_bg_log: number, bs_noise_bits: number, ks_noise_bits: number, rng: R): BootstrappingKeyDyn
{
  const bsk = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const bit = (lwe_sk.key[i] !== 0);
  return rgsw_encrypt(bit, rlwe_sk, bs_bg_log, bs_noise_bits, rng);
})());
  const rlwe_as_lwe = new LweSecretKeyDyn({ key: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => ((rlwe_sk.key[i]) & 0xFF)), n_lwe: 0 });
  const ksk_array: LweCiphertextDyn[][] = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s_bit = rlwe_sk.key[i];
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  const shift = (32 - (Math.imul(ks_bg_log, fieldAdd(Number(j), 1))));
  const msg_val = ((s_bit) << (shift)) >>> 0;
  return lwe_encrypt_raw(msg_val, lwe_sk, ks_noise_bits, rng);
})());
})());
  const _ = rlwe_as_lwe;
  const ksk = new KeySwitchingKeyDyn({ ksk: ksk_array, ks_bg_log: ks_bg_log, n_lwe: 0, big_n: 0, ks_ell: 0 });
  return new BootstrappingKeyDyn({ bsk: bsk, ksk: ksk, bs_bg_log: bs_bg_log, n_lwe: 0, big_n: 0, bs_ell: 0, ks_ell: 0 });
}

export function gen_lwe_secret_key<R>(n_lwe: number, rng: R): LweSecretKeyDyn
{
  let key = Array.from({length: n_lwe}, () => 0);
  for (const k of key.iter_mut())   {
    k = ((fieldBitand(rng.next_u8(), 1)) & 0xFF);
  }
  return new LweSecretKeyDyn({ key: key, n_lwe: 0 });
}

export function gen_rlwe_secret_key<R>(big_n: number, rng: R): RlweSecretKeyDyn
{
  let key = Array.from({length: big_n}, () => 0);
  for (const k of key.iter_mut())   {
    k = Number(fieldBitand(rng.next_u8(), 1));
  }
  return new RlweSecretKeyDyn({ key: key, big_n: 0 });
}

export function gf_mul(a: number, b: number): number
{
  return volar_primitives.gf_mul_u8(a, b, GF8_AES_POLY);
}

export function grafhen_and<R>(wbound: number, enc_a: GrafhenWordDyn, enc_b: GrafhenWordDyn, pk: GrafhenPublicDyn<R>): GrafhenWordDyn
{
  const w1 = enc_a;
  const w2 = enc_b;
  const a = pk.and_w1;
  const b = pk.and_w2;
  const segs: GrafhenWordDyn[] = [w1, a, w1, w2, b, w2, w1, a, w1, w2, b, w2];
  const total_len: number = segs.map((s: any) => s.len).sum();
  let result = GrafhenWord.identity();
  let pos = 0;
  for (const seg of segs)   {
    (result.data.slice(pos, fieldAdd(pos, seg.len))).splice(0, (seg.data.slice(0, seg.len)).length, ...(seg.data.slice(0, seg.len)));
    pos += seg.len;
  }
  result.len = total_len;
  pk.reducer.reduce(result);
  return result;
}

export function grafhen_decrypt(n: number, d: number, wbound: number, key: GrafhenKeyDyn, word: GrafhenWordDyn): (boolean | undefined)
{
  const perm = eval_word_to_perm(key, word);
  return (() => { const __match = perm[0]; if (__match === 0) { return false; } else if (__match === 4) { return true; } else { return undefined; } })();
}

export function grafhen_encrypt<R>(wbound: number, bit: boolean, zero_cipher: GrafhenWordDyn, pk: GrafhenPublicDyn<R>): GrafhenWordDyn
{
  return (() => { if (bit) {
  return grafhen_xor(zero_cipher, pk.enc_one);
} else {
  return zero_cipher;
} })();
}

export function grafhen_not<R>(wbound: number, a: GrafhenWordDyn, pk: GrafhenPublicDyn<R>): GrafhenWordDyn
{
  return grafhen_xor(a, pk.enc_one);
}

export function grafhen_xor(wbound: number, a: GrafhenWordDyn, b: GrafhenWordDyn): GrafhenWordDyn
{
  return (concat_words(a, b))!;
}

export function grafhen_zero(wbound: number): GrafhenWordDyn
{
  return GrafhenWord.identity();
}

export function grind_chall3(chall_2: readonly number[], a_hat: readonly number[], b_hat: readonly number[], c_hat_base: readonly number[], lambda: number, w_grind: number, use_shake256: boolean, max_iters: number): ([Vec<number>, number] | undefined)
{
  for (let counter = 0; counter < max_iters; counter++)   {
    const counter_bytes = [(counter) & 0xFF, ((counter) >> 8) & 0xFF, ((counter) >> 16) & 0xFF, ((counter) >> 24) & 0xFF];
    let c_hat_grind = alloc.vec.Vec.from(c_hat_base);
    c_hat_grind.extend_from_slice(counter_bytes);
    const candidate = chall3(chall_2, a_hat, b_hat, c_hat_grind, lambda, use_shake256);
    if (has_trailing_zero_bits(candidate, w_grind))     {
      return [candidate, counter];
    }
  }
  return undefined;
}

export function has_trailing_zero_bits(bytes: readonly number[], n: number): boolean
{
  if ((n === 0))   {
    return true;
  }
  const n_1 = Number(n);
  const full_bytes = (n_1 / 8);
  const rem = (n_1 % 8);
  if ((bytes.length < fieldAdd(full_bytes, (() => { if ((rem > 0)) {
  return 1;
} else {
  return 0;
} })())))   {
    return false;
  }
  for (let i = fieldSub(bytes.length, full_bytes); i < bytes.length; i++)   {
    if ((bytes[i] !== 0))     {
      return false;
    }
  }
  if ((rem > 0))   {
    const mask = fieldSub(fieldShl(1, rem), 1);
    const idx = fieldSub(fieldSub(bytes.length, full_bytes), 1);
    if ((fieldBitand(bytes[idx], mask) !== 0))     {
      return false;
    }
  }
  return true;
}

export function hash_key_from_chall(chall: readonly number[]): UniversalHashKey
{
  let r0_bytes = Array.from({length: 16}, () => 0);
  const n = Math.min(chall.length, 16);
  (r0_bytes.slice(0, n)).splice(0, (chall.slice(0, n)).length, ...(chall.slice(0, n)));
  let r1_bytes = Array.from({length: 8}, () => 0);
  const off = n;
  const m = Math.min(fieldSub(chall.length, off), 8);
  (r1_bytes.slice(0, m)).splice(0, (chall.slice(off, fieldAdd(off, m))).length, ...(chall.slice(off, fieldAdd(off, m))));
  return new UniversalHashKey({ r0: Galois128(u128.from_le_bytes(r0_bytes)), r1: new Galois64(u64.from_le_bytes(r1_bytes)) });
}

export function iknp_cot_extend<R>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: number, l: number, rng_s: R, rng_r: R, receiver_bits: boolean[], delta_msg: number[]): [number[][], number[][]]
{
  let delta_ot = Array.from({length: IKNP_KAPPA}, () => false);
  for (let i = 0; i < IKNP_KAPPA; i++)   {
    delta_ot[i] = (fieldBitand(rng_s.next_u32(), 1) === 1);
  }
  const delta_ot_bytes = pack_kappa(delta_ot);
  let seeds_0 = Array.from({length: IKNP_KAPPA}, () => Array.from({length: IKNP_KAPPA_BYTES}, () => 0));
  let seeds_1 = Array.from({length: IKNP_KAPPA}, () => Array.from({length: IKNP_KAPPA_BYTES}, () => 0));
  for (let i = 0; i < IKNP_KAPPA; i++)   {
    for (let b = 0; b < IKNP_KAPPA_BYTES; b++)     {
      seeds_0[i][b] = ((fieldBitand(rng_r.next_u32(), 255)) & 0xFF);
      seeds_1[i][b] = ((fieldBitand(rng_r.next_u32(), 255)) & 0xFF);
    }
  }
  let chosen_seeds = Array.from({length: IKNP_KAPPA}, () => Array.from({length: IKNP_KAPPA_BYTES}, () => 0));
  for (let i = 0; i < IKNP_KAPPA; i++)   {
    const [s_state, s_msg] = ot_send_setup(ctx, rng_r);
    const [r_state, r_msg] = ot_recv(ctx, rng_s, s_msg, delta_ot[i]);
    const [key_0, key_1] = ot_send_finish(ctx, s_state, r_msg);
    const kc = ot_recv_finish(ctx, r_state);
    let e0 = Array.from({length: IKNP_KAPPA_BYTES}, () => 0);
    let e1 = Array.from({length: IKNP_KAPPA_BYTES}, () => 0);
    ot_send_payload(key_0, key_1, seeds_0[i], seeds_1[i], e0, e1);
    const chosen_e: readonly number[] = (() => { if (delta_ot[i]) {
  return e1;
} else {
  return e0;
} })();
    ot_recv_payload(kc, chosen_e, chosen_seeds[i]);
  }
  let t_cols = Array.from({length: IKNP_KAPPA}, () => Array.from({length: m}, () => false));
  let q_cols = Array.from({length: IKNP_KAPPA}, () => Array.from({length: m}, () => false));
    {
    let prg1 = Array.from({length: m}, () => false);
    for (let i = 0; i < IKNP_KAPPA; i++)     {
      prg_to_bools(ctx, seeds_0[i], t_cols[i]);
      prg_to_bools(ctx, seeds_1[i], prg1);
      let u_col = Array.from({length: m}, () => false);
      for (let j = 0; j < m; j++)       {
        u_col[j] = fieldBitxor(fieldBitxor(t_cols[i][j], prg1[j]), receiver_bits[j]);
      }
      let prg_chosen = Array.from({length: m}, () => false);
      prg_to_bools(ctx, chosen_seeds[i], prg_chosen);
      for (let j = 0; j < m; j++)       {
        if (delta_ot[i])         {
          q_cols[i][j] = fieldBitxor(prg_chosen[j], u_col[j]);
        } else         {
          q_cols[i][j] = prg_chosen[j];
        }
      }
    }
  }
  let sender_r0 = Array.from({length: m}, () => Array.from({length: l}, () => 0));
  let receiver_v = Array.from({length: m}, () => Array.from({length: l}, () => 0));
  let q_row = Array.from({length: IKNP_KAPPA}, () => false);
  let t_row = Array.from({length: IKNP_KAPPA}, () => false);
  for (let j = 0; j < m; j++)   {
    for (let i = 0; i < IKNP_KAPPA; i++)     {
      q_row[i] = q_cols[i][j];
      t_row[i] = t_cols[i][j];
    }
    const q_bytes = pack_kappa(q_row);
    const t_bytes = pack_kappa(t_row);
    let r0 = Array.from({length: l}, () => 0);
    prg_with_index(ctx, q_bytes, Number(j), r0);
    let q_xor_delta = q_bytes;
    for (let b = 0; b < IKNP_KAPPA_BYTES; b++)     {
      q_xor_delta[b] ^= delta_ot_bytes[b];
    }
    let r1 = Array.from({length: l}, () => 0);
    prg_with_index(ctx, q_xor_delta, Number(j), r1);
    let v_pre = Array.from({length: l}, () => 0);
    prg_with_index(ctx, t_bytes, Number(j), v_pre);
    let correction = Array.from({length: l}, () => 0);
    for (let b = 0; b < l; b++)     {
      correction[b] = fieldBitxor(fieldBitxor(r0[b], r1[b]), delta_msg[b]);
    }
    sender_r0[j] = r0;
    if (receiver_bits[j])     {
      for (let b = 0; b < l; b++)       {
        receiver_v[j][b] = fieldBitxor(v_pre[b], correction[b]);
      }
    } else     {
      receiver_v[j] = v_pre;
    }
  }
  return [sender_r0, receiver_v];
}

export function key_expansion(key: number[]): number[][]
{
  let words = Array.from({length: fieldMul(4, NK_ROUND_KEYS)}, () => Array.from({length: 4}, () => 0));
  for (let i = 0; i < 4; i++)   {
    words[i] = [key[fieldMul(4, i)], key[fieldAdd(fieldMul(4, i), 1)], key[fieldAdd(fieldMul(4, i), 2)], key[fieldAdd(fieldMul(4, i), 3)]];
  }
  for (let i = 4; i < fieldMul(4, NK_ROUND_KEYS); i++)   {
    let temp = words[fieldSub(i, 1)];
    if (((i % 4) === 0))     {
      const t0 = temp[0];
      temp[0] = temp[1];
      temp[1] = temp[2];
      temp[2] = temp[3];
      temp[3] = t0;
      for (let b = 0; b < 4; b++)       {
        temp[b] = SBOX[Number(temp[b])];
      }
      temp[0] ^= RCON[(i / 4)];
    }
    for (let b = 0; b < 4; b++)     {
      words[i][b] = fieldBitxor(words[fieldSub(i, 4)][b], temp[b]);
    }
  }
  let round_keys = Array.from({length: NK_ROUND_KEYS}, () => Array.from({length: BLOCK}, () => 0));
  for (let r = 0; r < NK_ROUND_KEYS; r++)   {
    for (let c = 0; c < 4; c++)     {
      const w = words[fieldAdd(fieldMul(4, r), c)];
      round_keys[r][fieldMul(4, c)] = w[0];
      round_keys[r][fieldAdd(fieldMul(4, c), 1)] = w[1];
      round_keys[r][fieldAdd(fieldMul(4, c), 2)] = w[2];
      round_keys[r][fieldAdd(fieldMul(4, c), 3)] = w[3];
    }
  }
  return round_keys;
}

export function key_switch(n_lwe: number, big_n: number, ks_ell: number, ct_big: LweCiphertextDyn, ksk: KeySwitchingKeyDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: n_lwe}, () => 0);
  let out_b = ct_big.b;
  for (let i = 0; i < big_n; i++)   {
    const digits = ks_decompose(ct_big.a[i], ksk.ks_bg_log);
    for (let j = 0; j < ks_ell; j++)     {
      const d = Number(digits[j]);
      if ((d === 0))       {
        continue;
      }
      const ksk_ct = ksk.ksk[i][j];
      for (let k = 0; k < n_lwe; k++)       {
        out_a[k] = wrappingSub(out_a[k], Math.imul(d, ksk_ct.a[k]));
      }
      out_b = wrappingSub(out_b, Math.imul(d, ksk_ct.b));
    }
  }
  return new LweCiphertextDyn({ a: out_a, b: out_b, n_lwe: 0 });
}

export function keygen(rng: unknown /* impl SpecRng */): [FaestSecretKey, FaestPublicKey]
{
  let sk = Array.from({length: LAMBDA_BYTES}, () => 0);
  for (const b of sk.iter_mut())   {
    b = rng.next_u8();
  }
  const pk = aes128_encrypt(sk, Array.from({length: LAMBDA_BYTES}, () => 0));
  return [new FaestSecretKey(sk), new FaestPublicKey(pk)];
}

export function ks_decompose(ks_ell: number, x: number, bg_log: number): number[]
{
  const bg = fieldShl(1, bg_log);
  const mask = fieldSub(bg, 1);
  let rem = BigInt(x);
  const tail_shift = (32 - (Math.imul(bg_log, Number(ks_ell))));
  if (((tail_shift > 0) && (tail_shift < 32)))   {
    const half_tail = fieldShl(1, fieldSub(tail_shift, 1));
    rem = wrappingAdd(rem, half_tail);
  }
  let digits = Array.from({length: ks_ell}, () => 0);
  for (const j of (Array.from({length: ks_ell - 0}, (_, i) => i + 0)).slice().reverse())   {
    const shift = (32 - (Math.imul(bg_log, fieldAdd(Number(j), 1))));
    if ((shift < 32))     {
      digits[j] = Number(fieldBitand(fieldShr(rem, shift), mask));
    }
  }
  return digits;
}

export function lift_bit<T>(n: number, bit_t: any): any[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => __clone(bit_t));
}

export function lwe_add(n_lwe: number, a: LweCiphertextDyn, b: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: n_lwe}, () => 0);
  for (let i = 0; i < n_lwe; i++)   {
    out_a[i] = wrappingAdd(a.a[i], b.a[i]);
  }
  return new LweCiphertextDyn({ a: out_a, b: wrappingAdd(a.b, b.b), n_lwe: 0 });
}

export function lwe_decrypt(n_lwe: number, ct: LweCiphertextDyn, sk: LweSecretKeyDyn): boolean
{
  let dot: number = 0;
  for (let i = 0; i < n_lwe; i++)   {
    dot = wrappingAdd(dot, Math.imul(ct.a[i], Number(sk.key[i])));
  }
  const phase = wrappingSub(ct.b, dot);
  const half = fieldShr(Q4, 1);
  const shifted = wrappingSub(phase, half);
  return (shifted < Q4);
}

export function lwe_encrypt<R>(n_lwe: number, m: boolean, sk: LweSecretKeyDyn, noise_bits: number, rng: R): LweCiphertextDyn
{
  let a = Array.from({length: n_lwe}, () => 0);
  for (const ai of a.iter_mut())   {
    ai = rng.next_u32();
  }
  let dot: number = 0;
  for (let i = 0; i < n_lwe; i++)   {
    dot = wrappingAdd(dot, Math.imul(a[i], Number(sk.key[i])));
  }
  const e: number = small_noise(noise_bits, rng);
  const msg = (() => { if (m) {
  return Q4;
} else {
  return 0;
} })();
  const b = wrappingAdd(wrappingAdd(dot, e), msg);
  return new LweCiphertextDyn({ a: a, b: b, n_lwe: 0 });
}

export function lwe_encrypt_raw<R>(n_lwe: number, msg: number, sk: LweSecretKeyDyn, noise_bits: number, rng: R): LweCiphertextDyn
{
  let a = Array.from({length: n_lwe}, () => 0);
  for (const ai of a.iter_mut())   {
    ai = rng.next_u32();
  }
  let dot: number = 0;
  for (let i = 0; i < n_lwe; i++)   {
    dot = wrappingAdd(dot, Math.imul(a[i], Number(sk.key[i])));
  }
  const e = small_noise(noise_bits, rng);
  const b = wrappingAdd(wrappingAdd(dot, e), msg);
  return new LweCiphertextDyn({ a: a, b: b, n_lwe: 0 });
}

export function lwe_ot_recv<R>(rng: R, crs: LweOtCrs, c: boolean): [LweOtReceiver, LweOtRecvMsg]
{
  let s = Array.from({length: LWE_N}, () => 0);
  for (let i = 0; i < LWE_N; i++)   {
    s[i] = sample_noise(rng);
  }
  let pk_real = Array.from({length: LWE_N}, () => 0);
  for (let i = 0; i < LWE_N; i++)   {
    let acc: Zq = 0;
    for (let j = 0; j < LWE_N; j++)     {
      acc = zq_add(acc, zq_mul(crs.a[i][j], s[j]));
    }
    acc = zq_add(acc, sample_noise(rng));
    pk_real[i] = acc;
  }
  const pk0 = (() => { if (c) {
  let pk0 = Array.from({length: LWE_N}, () => 0);
  for (let i = 0; i < LWE_N; i++)   {
    pk0[i] = zq_sub(crs.h[i], pk_real[i]);
  }
  return pk0;
} else {
  return pk_real;
} })();
  return [new LweOtReceiver({ s: s, c: c }), new LweOtRecvMsg({ pk0: pk0 })];
}

export function lwe_ot_recv_decrypt(l: number, receiver: LweOtReceiver, sender_msg: LweOtSenderMsgDyn): number[]
{
  const [u, v] = (() => { if (receiver.c) {
  return [sender_msg.u1, sender_msg.v1];
} else {
  return [sender_msg.u0, sender_msg.v0];
} })();
  let s_dot_u: Zq = 0;
  for (let i = 0; i < LWE_N; i++)   {
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], u[i]));
  }
  const quarter = (LWE_Q / 4);
  const three_quarter = fieldMul(3, quarter);
  let out = Array.from({length: l}, () => 0);
  for (let k = 0; k < l; k++)   {
    const raw = zq_sub(v[k], s_dot_u);
    out[k] = (() => { if (((raw > quarter) && (raw <= three_quarter))) {
  return 1;
} else {
  return 0;
} })();
  }
  return out;
}

export function lwe_ot_send<R>(l: number, rng: R, crs: LweOtCrs, recv_msg: LweOtRecvMsg, m0: number[], m1: number[]): LweOtSenderMsgDyn
{
  const pk0 = recv_msg.pk0;
  let pk1 = Array.from({length: LWE_N}, () => 0);
  for (let i = 0; i < LWE_N; i++)   {
    pk1[i] = zq_sub(crs.h[i], pk0[i]);
  }
  const [u0, v0] = encrypt_branch(rng, crs, pk0, m0);
  const [u1, v1] = encrypt_branch(rng, crs, pk1, m1);
  return new LweOtSenderMsgDyn({ u0: u0, v0: v0, u1: u1, v1: v1, l: 0 });
}

export function memory_check_per_lane<T>(n: number, challenges: any[]): MemoryCheckStateDyn<any, AdditiveHasher>[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const key = ChallengeKey.from_challenge(__clone(challenges[i]));
  return MemoryCheckState.new(key);
})());
}

export function mix_columns(state: number[])
{
  for (let c = 0; c < 4; c++)   {
    const i = fieldMul(4, c);
    const s0 = state[i];
    const s1 = state[fieldAdd(i, 1)];
    const s2 = state[fieldAdd(i, 2)];
    const s3 = state[fieldAdd(i, 3)];
    state[i] = fieldBitxor(fieldBitxor(fieldBitxor(gf_mul(s0, 2), gf_mul(s1, 3)), s2), s3);
    state[fieldAdd(i, 1)] = fieldBitxor(fieldBitxor(fieldBitxor(s0, gf_mul(s1, 2)), gf_mul(s2, 3)), s3);
    state[fieldAdd(i, 2)] = fieldBitxor(fieldBitxor(fieldBitxor(s0, s1), gf_mul(s2, 2)), gf_mul(s3, 3));
    state[fieldAdd(i, 3)] = fieldBitxor(fieldBitxor(fieldBitxor(gf_mul(s0, 3), s1), s2), gf_mul(s3, 2));
  }
}

export function mul_4x4(a: bigint[], b: bigint[]): bigint[]
{
  let r = Array.from({length: 8}, () => 0);
  for (let i = 0; i < 4; i++)   {
    let carry: bigint = 0;
    for (let j = 0; j < 4; j++)     {
      const v = fieldAdd(fieldAdd((r[fieldAdd(i, j)] as unknown as bigint), fieldMul((a[i] as unknown as bigint), (b[j] as unknown as bigint))), (carry as unknown as bigint));
      r[fieldAdd(i, j)] = BigInt(v);
      carry = BigInt(fieldShr(v, 64));
    }
    r[fieldAdd(i, 4)] = carry;
  }
  return r;
}

export function ot_recv<G, D, R>(ctx: { GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng: R, s: number /* G::Element */, c: boolean): [BaseOtReceiverDyn<G, D>, OtReceiverMsgDyn<G>]
{
  const x = ctx.GClass.random_scalar(rng);
  const g = ctx.GClass.generator();
  const gx = ctx.GClass.scalar_mul(g, x);
  const r = (() => { if (c) {
  return ctx.GClass.add(s, gx);
} else {
  return gx;
} })();
  return [new BaseOtReceiverDyn({ x: x, s: s, c: c, _d: PhantomData }), new OtReceiverMsgDyn({ r: r })];
}

export function ot_recv_choice<G, D>(state: BaseOtReceiverDyn<G, D>): boolean
{
  return state.c;
}

export function ot_recv_finish<G, D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, state: BaseOtReceiverDyn<G, D>): Output<D>
{
  const sx = ctx.GClass.scalar_mul(state.s, state.x);
  let h = ctx.newD();
  ctx.GClass.write_element(sx, h);
  return [...h.finalize()];
}

export function ot_recv_payload<D>(kc: Output<D>, ec: readonly number[], mc: readonly number[])
{
  for (let i = 0; i < ec.length; i++)   {
    mc[i] = fieldBitxor(ec[i], kc[i]);
  }
}

export function ot_send_finish<G, D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, state: BaseOtSenderDyn<G, D>, msg: OtReceiverMsgDyn<G>): [Output<D>, Output<D>]
{
  const ry = ctx.GClass.scalar_mul(msg.r, state.y);
  const s_inv = ctx.GClass.neg(state.s);
  const r_minus_s = ctx.GClass.add(msg.r, s_inv);
  const r_minus_s_y = ctx.GClass.scalar_mul(r_minus_s, state.y);
  let h0 = ctx.newD();
  ctx.GClass.write_element(ry, h0);
  let h1 = ctx.newD();
  ctx.GClass.write_element(r_minus_s_y, h1);
  return [[...h0.finalize()], [...h1.finalize()]];
}

export function ot_send_payload<D>(k0: Output<D>, k1: Output<D>, m0: readonly number[], m1: readonly number[], e0: readonly number[], e1: readonly number[])
{
  for (let i = 0; i < m0.length; i++)   {
    e0[i] = fieldBitxor(m0[i], k0[i]);
  }
  for (let i = 0; i < m1.length; i++)   {
    e1[i] = fieldBitxor(m1[i], k1[i]);
  }
}

export function ot_send_setup<G, D, R>(ctx: { GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng: R): [BaseOtSenderDyn<G, D>, number /* G::Element */]
{
  const y = ctx.GClass.random_scalar(rng);
  const g = ctx.GClass.generator();
  const s = ctx.GClass.scalar_mul(g, y);
  const t = ctx.GClass.scalar_mul(s, y);
  return [new BaseOtSenderDyn({ y: y, s: __clone(s), t: t, _d: PhantomData }), s];
}

export function pack_kappa(bits: boolean[]): number[]
{
  let out = Array.from({length: IKNP_KAPPA_BYTES}, () => 0);
  for (let i = 0; i < IKNP_KAPPA; i++)   {
    if (bits[i])     {
      out[(i / 8)] |= fieldShl(1, (i % 8));
    }
  }
  return out;
}

export function poly_add_neg(n: number, a: number[], b: number[]): number[]
{
  let result = Array.from({length: n}, () => 0);
  for (let i = 0; i < n; i++)   {
    result[i] = wrappingAdd(a[i], b[i]);
  }
  return result;
}

export function poly_decompose(big_n: number, bs_ell: number, p: number[], bg_log: number): number[][]
{
  const bg = fieldShl(1, bg_log);
  const mask = Number(fieldSub(bg, 1));
  let result = Array.from({length: bs_ell}, () => Array.from({length: big_n}, () => 0));
  for (let i = 0; i < big_n; i++)   {
    const x = p[i];
    const tail_bits = (32 - (Math.imul(bg_log, Number(bs_ell))));
    const rounded = (() => { if (((tail_bits > 0) && (tail_bits < 32))) {
  return wrappingAdd(x, fieldShl(1, fieldSub(tail_bits, 1)));
} else {
  return x;
} })();
    for (let j = 0; j < bs_ell; j++)     {
      const shift = (32 - (Math.imul(bg_log, fieldAdd(Number(j), 1))));
      result[j][i] = (() => { if ((shift < 32)) {
  return fieldBitand(fieldShr(rounded, shift), mask);
} else {
  return 0;
} })();
    }
  }
  return result;
}

export function poly_mul_neg(n: number, a: number[], b: number[]): number[]
{
  let result = Array.from({length: n}, () => 0);
  for (let i = 0; i < n; i++)   {
    for (let j = 0; j < n; j++)     {
      const deg = fieldAdd(i, j);
      if ((deg < n))       {
        result[deg] = wrappingAdd(result[deg], Math.imul(a[i], b[j]));
      } else       {
        result[fieldSub(deg, n)] = wrappingSub(result[fieldSub(deg, n)], Math.imul(a[i], b[j]));
      }
    }
  }
  return result;
}

export function poly_rotate(n: number, p: number[], exp: number): number[]
{
  const exp_1 = (exp % fieldMul(2, n));
  if ((exp_1 === 0))   {
    return p;
  }
  let result = Array.from({length: n}, () => 0);
  for (let i = 0; i < n; i++)   {
    const new_pos = fieldAdd(i, exp_1);
    if ((new_pos < n))     {
      result[new_pos] = wrappingAdd(result[new_pos], p[i]);
    } else if ((new_pos < fieldMul(2, n)))     {
      result[fieldSub(new_pos, n)] = wrappingSub(result[fieldSub(new_pos, n)], p[i]);
    } else     {
      result[fieldSub(new_pos, fieldMul(2, n))] = wrappingAdd(result[fieldSub(new_pos, fieldMul(2, n))], p[i]);
    }
  }
  return result;
}

export function poly_sub_neg(n: number, a: number[], b: number[]): number[]
{
  let result = Array.from({length: n}, () => 0);
  for (let i = 0; i < n; i++)   {
    result[i] = wrappingSub(a[i], b[i]);
  }
  return result;
}

export function prg_to_bools(ctx: { newD: () => any }, seed: readonly number[], out: readonly boolean[])
{
  let counter: number = 0;
  let pos = 0;
  while ((pos < out.length))   {
    let h = ctx.newD();
    h.update(seed);
    h.update([(counter) & 0xFF, ((counter) >> 8) & 0xFF, ((counter) >> 16) & 0xFF, ((counter) >> 24) & 0xFF]);
    const block = [...h.finalize()];
    const block_bytes: readonly number[] = asRefU8(block);
    for (const byte of block_bytes)     {
      for (let bit = 0; bit < 8; bit++)       {
        if ((pos >= out.length))         {
          return;
        }
        out[pos] = (fieldBitand(fieldShr(byte, bit), 1) === 1);
        pos += 1;
      }
    }
    counter += 1;
  }
}

export function prg_with_index(ctx: { newD: () => any }, seed: readonly number[], idx: number, out: readonly number[])
{
  let counter: number = 0;
  let pos = 0;
  while ((pos < out.length))   {
    let h = ctx.newD();
    h.update(seed);
    h.update([(idx) & 0xFF, ((idx) >> 8) & 0xFF, ((idx) >> 16) & 0xFF, ((idx) >> 24) & 0xFF]);
    h.update([(counter) & 0xFF, ((counter) >> 8) & 0xFF, ((counter) >> 16) & 0xFF, ((counter) >> 24) & 0xFF]);
    const block = [...h.finalize()];
    const block_bytes: readonly number[] = asRefU8(block);
    const take = Math.min(fieldSub(out.length, pos), block_bytes.length);
    (out.slice(pos, fieldAdd(pos, take))).splice(0, (block_bytes.slice(0, take)).length, ...(block_bytes.slice(0, take)));
    pos += take;
    counter += 1;
  }
}

export function random_nonzero_delta<T, R>(n: number, rng: R, sample_t: unknown /* impl Fn */, is_zero: unknown /* impl Fn */): DeltaDyn<any>
{
  return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => (() => {
  let x = sample_t(rng);
  let tries = 0;
  while ((is_zero(x) && (tries < 64)))   {
    x = sample_t(rng);
    tries += 1;
  }
  return x;
})()), n: 0 });
}

export function recompute_tree(r: number[], total_leaves: number): Vec<number[]>
{
  const total_nodes = fieldSub(fieldMul(2, total_leaves), 1);
  let tree = [];
  tree[0] = r;
  for (let node = 0; node < fieldSub(total_leaves, 1); node++)   {
    const parent = Vec(tree[node]);
    const [left, right] = doubleVec(parent);
    tree[fieldAdd(fieldMul(2, node), 1)] = left._0;
    tree[fieldAdd(fieldMul(2, node), 2)] = right._0;
  }
  return tree;
}

export function reduce_wide(t: bigint[]): Fe25519
{
  let acc = Array.from({length: 5}, () => 0);
  let c: bigint = 0;
  for (let i = 0; i < 4; i++)   {
    const v = fieldAdd(fieldAdd((t[i] as unknown as bigint), fieldMul((t[fieldAdd(4, i)] as unknown as bigint), 38)), c);
    acc[i] = BigInt(v);
    c = fieldShr(v, 64);
  }
  acc[4] = BigInt(c);
  let out = Array.from({length: 4}, () => 0);
  let c_1: bigint = fieldMul((acc[4] as unknown as bigint), 38);
  for (let i = 0; i < 4; i++)   {
    const v = fieldAdd((acc[i] as unknown as bigint), c_1);
    out[i] = BigInt(v);
    c_1 = fieldShr(v, 64);
  }
  if ((c_1 !== 0))   {
    let c2: bigint = fieldMul(c_1, 38);
    for (let i = 0; i < 4; i++)     {
      const v = fieldAdd((out[i] as unknown as bigint), c2);
      out[i] = BigInt(v);
      c2 = fieldShr(v, 64);
    }
  }
  return fe_canonicalize(out);
}

export function rgsw_encrypt<R>(big_n: number, bs_ell: number, m: boolean, sk: RlweSecretKeyDyn, bs_bg_log: number, noise_bits: number, rng: R): RgswCiphertextDyn
{
  const msg_bit = (() => { if (m) {
  return 1;
} else {
  return 0;
} })();
  const rows = Array.from({length: n - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  const shift = (32 - (Math.imul(bs_bg_log, fieldAdd(Number(j), 1))));
  const g_factor = ((1) << (shift)) >>> 0;
  const contrib = Math.imul(msg_bit, g_factor);
  let rlwe0 = rlwe_encrypt_scalar(0, sk, noise_bits, rng);
  rlwe0.a[0] = wrappingAdd(rlwe0.a[0], contrib);
  const rlwe1 = rlwe_encrypt_scalar(contrib, sk, noise_bits, rng);
  return new RgswRowDyn({ rlwe0: rlwe0, rlwe1: rlwe1, big_n: 0 });
})());
  return new RgswCiphertextDyn({ rows: rows, big_n: 0, bs_ell: 0 });
}

export function rlwe_add(big_n: number, a: RlweCiphertextDyn, b: RlweCiphertextDyn): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ a: poly_add_neg(a.a, b.a), b: poly_add_neg(a.b, b.b), big_n: 0 });
}

export function rlwe_encrypt_poly<R>(big_n: number, msg_poly: number[], sk: RlweSecretKeyDyn, noise_bits: number, rng: R): RlweCiphertextDyn
{
  const a: number[] = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(a, sk.key);
  for (let i = 0; i < big_n; i++)   {
    b[i] = wrappingAdd(wrappingAdd(b[i], small_noise(noise_bits, rng)), msg_poly[i]);
  }
  return new RlweCiphertextDyn({ a: a, b: b, big_n: 0 });
}

export function rlwe_encrypt_scalar<R>(big_n: number, m: number, sk: RlweSecretKeyDyn, noise_bits: number, rng: R): RlweCiphertextDyn
{
  const a: number[] = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(a, sk.key);
  b[0] = wrappingAdd(wrappingAdd(b[0], small_noise(noise_bits, rng)), m);
  return new RlweCiphertextDyn({ a: a, b: b, big_n: 0 });
}

export function rlwe_rotate(big_n: number, ct: RlweCiphertextDyn, exp: number): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ a: poly_rotate(ct.a, exp), b: poly_rotate(ct.b, exp), big_n: 0 });
}

export function rlwe_sub(big_n: number, a: RlweCiphertextDyn, b: RlweCiphertextDyn): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ a: poly_sub_neg(a.a, b.a), b: poly_sub_neg(a.b, b.b), big_n: 0 });
}

export function sample_extract(big_n: number, rlwe: RlweCiphertextDyn): LweCiphertextDyn
{
  let a_lwe = Array.from({length: big_n}, () => 0);
  a_lwe[0] = rlwe.a[0];
  for (let i = 1; i < big_n; i++)   {
    a_lwe[i] = (-((rlwe.a[fieldSub(big_n, i)])) >>> 0);
  }
  return new LweCiphertextDyn({ a: a_lwe, b: rlwe.b[0], n_lwe: 0 });
}

export function sample_noise<R>(rng: R): Zq
{
  const span = fieldAdd(fieldMul(2, LWE_NOISE_BOUND), 1);
  const raw = (rng.next_u32() % span);
  return (() => { if ((raw <= LWE_NOISE_BOUND)) {
  return raw;
} else {
  return zq_neg(fieldSub(raw, LWE_NOISE_BOUND));
} })();
}

export function sample_zq<R>(rng: R): Zq
{
  return fieldBitand(rng.next_u32(), LWE_Q_MASK);
}

export function shift_rows(state: number[])
{
  const t = state[1];
  state[1] = state[5];
  state[5] = state[9];
  state[9] = state[13];
  state[13] = t;
  const t_1 = state[2];
  state[2] = state[10];
  state[10] = t_1;
  const t_2 = state[6];
  state[6] = state[14];
  state[14] = t_2;
  const t_3 = state[15];
  state[15] = state[11];
  state[11] = state[7];
  state[7] = state[3];
  state[3] = t_3;
}

export function sign(sk: FaestSecretKey, pk: FaestPublicKey, message: readonly number[], iv_seed: number[], prover: unknown /* impl FaestAesProver */): FaestSignature
{
  const iv: number[] = aes128_encrypt(iv_seed, Array.from({length: LAMBDA_BYTES}, () => 0));
  const r: number[] = aes128_encrypt(sk._0, iv);
  const commitment: BavcCommitmentDyn = Bavc.commit(r, iv, TAU, SUB_VOLE_N);
  const mu: Vec<number> = (() => {
  let h = Sha3_256.new();
  DigestUpdate.update(h, pk._0);
  DigestUpdate.update(h, message);
  return [...Digest.finalize(h)];
})();
  const chall_1 = chall1(mu, iv, commitment.root, fieldAdd(LAMBDA_BYTES, 8), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const nodes = Bavc.collect_open_nodes(deltas, recompute_tree(r, fieldMul(TAU, SUB_VOLE_N)), TAU, SUB_VOLE_N);
  const hidden_commits: Vec<number[]> = deltas.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([i, d]: any) => commitment.commitments[fieldAdd(fieldMul(i, SUB_VOLE_N), d)]);
  const opening = new BavcOpeningDyn({ hidden_commits: __clone(hidden_commits), nodes: __clone(nodes), com_bytes: 0 });
  const _ = opening;
  let sub_voles = /* Vec::with_capacity */ Array(TAU);
  for (let i = 0; i < TAU; i++)   {
    const seeds_i: Vec<(number[] | undefined)> = Array.from({length: SUB_VOLE_N - 0}, (_, __i) => __i + 0).map((j: any) => commitment.seeds[fieldAdd(fieldMul(i, SUB_VOLE_N), j)]);
    (sub_voles).push(convert_to_vole(seeds_i, iv, Number(i), L_HAT_BYTES));
  }
  const big_vole: BigVoleProver = concat_small_voles(sub_voles);
  const corrections_flat: Vec<number> = big_vole.c.flatten().collect();
  const chall_2 = chall2(chall_1, big_vole.u, corrections_flat, fieldAdd(LAMBDA_BYTES, 8), false);
  const hash_key = hash_key_from_chall(chall_2);
  const qs_proof = prover.prove_aes_witness(big_vole, hash_key);
  const [chall_3, counter] = (grind_chall3(chall_2, qs_proof.a_hat, qs_proof.b_hat, qs_proof.c_hat_base, LAMBDA_BYTES, W_GRIND, false, 1000000))!;
  let c_hat_with_counter = __clone(qs_proof.c_hat_base);
  c_hat_with_counter.extend_from_slice([(counter) & 0xFF, ((counter) >> 8) & 0xFF, ((counter) >> 16) & 0xFF, ((counter) >> 24) & 0xFF]);
  return new FaestSignature({ iv: iv, bavc_root: __clone(commitment.root), hidden_commits: hidden_commits, nodes: nodes, corrections: __clone(big_vole.c), vole_u: __clone(big_vole.u), qs_proof: qs_proof, c_hat_with_counter: c_hat_with_counter, chall_3: chall_3, counter: counter });
}

export function small_noise<R>(noise_bits: number, rng: R): number
{
  if ((noise_bits >= 32))   {
    return rng.next_u32();
  }
  const raw: number = rng.next_u32();
  const mask = wrappingSub(fieldShl(1, noise_bits), 1);
  const small = fieldBitand(raw, mask);
  return (() => { if (((noise_bits > 0) && (fieldShr(small, fieldSub(noise_bits, 1)) !== 0))) {
  return fieldBitor(small, !mask);
} else {
  return small;
} })();
}

export function softspoken_cot_extend<D, R>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, k: number, m: number, l: number, rng_s: R, rng_r: R, receiver_bits: boolean[], delta_msg: number[]): SoftSpokenOutDyn<D>
{
  const [sender_r0, receiver_v] = iknp_cot_extend(ctx, rng_s, rng_r, receiver_bits, delta_msg);
  let hs = ctx.newD();
  hs.update(TAG_DOMAIN);
  hs.update(delta_msg);
  for (const row of sender_r0)   {
    hs.update(row);
  }
  const sender_tag = [...hs.finalize()];
  let hr = ctx.newD();
  hr.update(TAG_DOMAIN);
  hr.update(delta_msg);
  for (let j = 0; j < m; j++)   {
    let r0_reconstructed = Array.from({length: l}, () => 0);
    if (receiver_bits[j])     {
      for (let b = 0; b < l; b++)       {
        r0_reconstructed[b] = fieldBitxor(receiver_v[j][b], delta_msg[b]);
      }
    } else     {
      r0_reconstructed = receiver_v[j];
    }
    hr.update(r0_reconstructed);
  }
  const receiver_tag = [...hr.finalize()];
  return new SoftSpokenOutDyn({ sender_r0: sender_r0, receiver_v: receiver_v, sender_tag: sender_tag, receiver_tag: receiver_tag, m: 0, l: 0 });
}

export function sub_bytes(state: number[])
{
  for (let i = 0; i < BLOCK; i++)   {
    state[i] = SBOX[Number(state[i])];
  }
}

export function tfhe_cmux(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, sel: LweCiphertextDyn, a: LweCiphertextDyn, b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const not_sel = tfhe_not(sel);
  const sel_and_a = tfhe_gate_bootstrapping_and(sel, a, bk);
  const nsel_and_b = tfhe_gate_bootstrapping_and(not_sel, b, bk);
  return tfhe_gate_bootstrapping_or(sel_and_a, nsel_and_b, bk);
}

export function tfhe_gate_bootstrapping_and(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, ct_a: LweCiphertextDyn, ct_b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  let ct = lwe_add(ct_a, ct_b);
  ct.b = wrappingSub(ct.b, fieldShr(Q4, 1));
  const acc = blind_rotate(ct, bk);
  const lwe_big = sample_extract(acc);
  let ct_out = key_switch(lwe_big, bk.ksk);
  ct_out.b = wrappingAdd(ct_out.b, fieldShr(Q4, 1));
  return ct_out;
}

export function tfhe_gate_bootstrapping_or(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, ct_a: LweCiphertextDyn, ct_b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  let ct = lwe_add(ct_a, ct_b);
  ct.b = wrappingAdd(ct.b, fieldShr(Q4, 1));
  const acc = blind_rotate(ct, bk);
  const lwe_big = sample_extract(acc);
  let ct_out = key_switch(lwe_big, bk.ksk);
  ct_out.b = wrappingAdd(ct_out.b, fieldShr(Q4, 1));
  return ct_out;
}

export function tfhe_lut_read(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, addr_bits: readonly LweCiphertextDyn[], lut: readonly boolean[], bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const two_n = fieldMul(2, big_n);
  const k = Math.max(lut.length, 1).next_power_of_two();
  if ((!(lut.length === 0) && lut.all((v) => (v === lut[0]))))   {
    const msg = (() => { if (lut[0]) {
  return Q4;
} else {
  return 0;
} })();
    return new LweCiphertextDyn({ a: Array.from({length: n_lwe}, () => 0), b: msg, n_lwe: 0 });
  }
  const half_q4 = fieldShr(Q4, 1);
  const step = (two_n / k);
  const half_k = (k / 2);
  const poly_step = (big_n / half_k);
  let test_poly = Array.from({length: big_n}, () => 0);
  for (let j = 0; j < big_n; j++)   {
    const entry_idx = (j / poly_step);
    const val = (() => { if (((entry_idx < lut.length) && lut[entry_idx])) {
  return half_q4;
} else {
  return (-((half_q4)) >>> 0);
} })();
    test_poly[j] = val;
  }
  const delta = (fieldShl(1, 32) / BigInt(k));
  let combined = new LweCiphertextDyn({ a: Array.from({length: n_lwe}, () => 0), b: 0, n_lwe: 0 });
  for (const [j, addr_ct] of addr_bits.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const target = fieldMul(fieldShl(1, j), delta);
    for (let i = 0; i < n_lwe; i++)     {
      const scaled = (Math.imul(BigInt(addr_ct.a[i]), target) / BigInt(Q4));
      combined.a[i] = wrappingAdd(combined.a[i], Number(scaled));
    }
    const scaled_b = (Math.imul(BigInt(addr_ct.b), target) / BigInt(Q4));
    combined.b = wrappingAdd(combined.b, Number(scaled_b));
  }
  const centering = Number((delta / 2));
  combined.b = wrappingAdd(combined.b, centering);
  let ct_out = tfhe_programmable_bootstrap(combined, test_poly, bk);
  ct_out.b = wrappingAdd(ct_out.b, half_q4);
  return ct_out;
}

export function tfhe_not(n_lwe: number, a: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: n_lwe}, () => 0);
  for (let i = 0; i < n_lwe; i++)   {
    out_a[i] = (-((a.a[i])) >>> 0);
  }
  return new LweCiphertextDyn({ a: out_a, b: wrappingSub(Q4, a.b), n_lwe: 0 });
}

export function tfhe_programmable_bootstrap(n_lwe: number, big_n: number, bs_ell: number, ks_ell: number, ct: LweCiphertextDyn, test_poly: number[], bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const acc = blind_rotate_with_poly(ct, test_poly, bk);
  const lwe_big = sample_extract(acc);
  return key_switch(lwe_big, bk.ksk);
}

export function tfhe_trivial_encrypt(n_lwe: number, b: boolean): LweCiphertextDyn
{
  return (() => { if (b) {
  return tfhe_trivial_one();
} else {
  return tfhe_trivial_zero();
} })();
}

export function tfhe_trivial_one(n_lwe: number): LweCiphertextDyn
{
  return new LweCiphertextDyn({ a: Array.from({length: n_lwe}, () => 0), b: Q4, n_lwe: 0 });
}

export function tfhe_trivial_zero(n_lwe: number): LweCiphertextDyn
{
  return new LweCiphertextDyn({ a: Array.from({length: n_lwe}, () => 0), b: 0, n_lwe: 0 });
}

export function tfhe_xor(n_lwe: number, a: LweCiphertextDyn, b: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: n_lwe}, () => 0);
  for (let i = 0; i < n_lwe; i++)   {
    out_a[i] = wrappingAdd(a.a[i], b.a[i]);
  }
  return new LweCiphertextDyn({ a: out_a, b: wrappingAdd(a.b, b.b), n_lwe: 0 });
}

export function torus_to_exp(x: number, scale_shift: number, two_n: number): number
{
  const half = (() => { if ((scale_shift > 0)) {
  return fieldShl(1, fieldSub(scale_shift, 1));
} else {
  return 0;
} })();
  const exp = Number(fieldShr(wrappingAdd(x, half), scale_shift));
  return fieldBitand(exp, fieldSub(two_n, 1));
}

export function toy_mul(a: bigint, b: bigint): bigint
{
  return (fieldMul(a, b) % TOY_P);
}

export function toy_pow(base: bigint, exp: bigint): bigint
{
  let acc: bigint = 1;
  let b = (base % TOY_P);
  while ((exp > 0))   {
    if ((fieldBitand(exp, 1) === 1))     {
      acc = toy_mul(acc, b);
    }
    b = toy_mul(b, b);
    exp >>= 1;
  }
  return acc;
}

export function verify(pk: FaestPublicKey, message: readonly number[], sig: FaestSignature): boolean
{
  const iv = sig.iv;
  const mu: Vec<number> = (() => {
  let h = Sha3_256.new();
  DigestUpdate.update(h, pk._0);
  DigestUpdate.update(h, message);
  return [...Digest.finalize(h)];
})();
  const chall_1 = chall1(mu, iv, sig.bavc_root, fieldAdd(LAMBDA_BYTES, 8), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const reconstructed_seeds_opt = Bavc.reconstruct(sig.nodes, sig.hidden_commits, deltas, iv, sig.bavc_root, TAU, SUB_VOLE_N);
  const reconstructed_seeds = (() => { const __match = reconstructed_seeds_opt; if (__match !== null && __match !== undefined) { const s = __match;
return s; } else { return false; } })();
  let sub_voles_v = /* Vec::with_capacity */ Array(TAU);
  for (let i = 0; i < TAU; i++)   {
    const d = deltas[i];
    const verifier_seeds: Vec<(number[] | undefined)> = Array.from({length: SUB_VOLE_N - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  return (() => { if ((j === 0)) {
  return undefined;
} else {
  return reconstructed_seeds[fieldAdd(fieldMul(i, SUB_VOLE_N), fieldBitxor(j, d))];
} })();
})());
    (sub_voles_v).push(convert_to_vole(verifier_seeds, iv, Number(i), L_HAT_BYTES));
  }
  const corrections = sig.corrections;
  if ((corrections.length !== fieldSub(TAU, 1)))   {
    return false;
  }
  const big_q: Vec<number> = (() => {
  const q_out = concat_small_voles_verifier(sub_voles_v, deltas, corrections);
  return q_out.q_columns.flatten().collect();
})();
  const corrections_flat: Vec<number> = sig.corrections.flatten().collect();
  const chall_2 = chall2(chall_1, sig.vole_u, corrections_flat, fieldAdd(LAMBDA_BYTES, 8), false);
  const hash_key = hash_key_from_chall(chall_2);
  const derived_chall_3 = chall3(chall_2, sig.qs_proof.a_hat, sig.qs_proof.b_hat, sig.c_hat_with_counter, LAMBDA_BYTES, false);
  if ((derived_chall_3 !== sig.chall_3))   {
    return false;
  }
  if (!has_trailing_zero_bits(sig.chall_3, W_GRIND))   {
    return false;
  }
  const _ = hash_key;
  return true;
}

export function vole_and_prover_step<T>(n: number, vope_a: VopeDyn<any>, vope_b: VopeDyn<any>): [VopeDyn<any>, any[]]
{
  const u_c_inner = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return fieldMul(__clone(vope_a.u[0][i]), __clone(vope_b.u[0][i]));
})());
  const u_c = Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_: any) => __clone(u_c_inner));
  const v_c = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return fieldAdd(fieldMul(__clone(vope_a.v[i]), __clone(vope_b.u[0][i])), fieldMul(__clone(vope_b.v[i]), __clone(vope_a.u[0][i])));
})());
  const hat = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return fieldMul(__clone(vope_a.v[i]), __clone(vope_b.v[i]));
})());
  return [new VopeDyn({ u: u_c, v: v_c, n: 0, k: 1 }), hat];
}

export function vole_and_verifier_check<T>(n: number, delta: DeltaDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, q_and: QDyn<any>, hat: any[]): [QDyn<any>, boolean]
{
  let ok = true;
  for (let i = 0; i < n; i++)   {
    const lhs = fieldAdd(fieldMul(__clone(q_a.q[i]), __clone(q_b.q[i])), __clone(hat[i]));
    const rhs = fieldMul(__clone(q_and.q[i]), __clone(delta.delta[i]));
    ok = (ok && (lhs === rhs));
  }
  return [new QDyn({ q: __clone(q_and.q), n: 0 }), ok];
}

export function vole_commit_bit<T, R>(n: number, cot: IdealCotDyn<any>, rng: R, sample_t: unknown /* impl Fn */, bit_to_t: (arg: boolean) => any, bit: boolean): [VopeDyn<any>, QDyn<any>]
{
  const [r0, v] = cot.cot(rng, sample_t, bit);
  const u_t = bit_to_t(bit);
  const u_row: any[] = lift_bit(u_t);
  const u: any[][] = Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(u_row[i]));
})());
  const q = Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => __clone(r0[i]));
  return [new VopeDyn({ u: u, v: v, n: 0, k: 1 }), new QDyn({ q: q, n: 0 })];
}

export function vole_hash(key: UniversalHashKey, input: readonly number[]): UniversalHashOutput
{
  const n_full = (input.length / 16);
  const tail = input.slice(fieldMul(n_full, 16));
  let h0 = Galois128(0);
  let h1 = new Galois64(0);
  let pow0 = key.r0;
  let pow1 = key.r1;
  for (let i = 0; i < n_full; i++)   {
    const block = input.slice(fieldMul(i, 16), fieldMul(fieldAdd(i, 1), 16));
    let bytes = Array.from({length: 16}, () => 0);
    (bytes).splice(0, (block).length, ...(block));
    const s = Galois128(u128.from_le_bytes(bytes));
    h0 = fieldAdd(h0, fieldMul(s, pow0));
    const s64 = new Galois64(BigInt(s._0));
    h1 = fieldAdd(h1, fieldMul(s64, pow1));
    pow0 = fieldMul(pow0, key.r0);
    pow1 = fieldMul(pow1, key.r1);
  }
  if (!(tail.length === 0))   {
    let bytes = Array.from({length: 8}, () => 0);
    const n = Math.min(tail.length, 8);
    (bytes.slice(0, n)).splice(0, (tail.slice(0, n)).length, ...(tail.slice(0, n)));
    const t = new Galois64(u64.from_le_bytes(bytes));
    h1 = fieldAdd(h1, fieldMul(t, pow1));
  }
  return new UniversalHashOutput({ h0: h0, h1: h1 });
}

export function vole_hash_consistency_check(key: UniversalHashKey, hu: UniversalHashOutput, hq: UniversalHashOutput, hv: UniversalHashOutput, hc: UniversalHashOutput, delta: Galois128): boolean
{
  const lhs0 = hq.h0;
  const rhs0 = fieldAdd(hv.h0, fieldMul(delta, fieldAdd(hu.h0, hc.h0)));
  const delta64 = new Galois64(BigInt(delta._0));
  const lhs1 = hq.h1;
  const rhs1 = fieldAdd(hv.h1, fieldMul(delta64, fieldAdd(hu.h1, hc.h1)));
  return ((lhs0 === rhs0) && (lhs1 === rhs1));
}

export function vole_mul3_prover_step<T>(n: number, vope_a: VopeDyn<any>, vope_b: VopeDyn<any>, vope_d: VopeDyn<any>): VopeDyn<any>
{
  const ab: VopeDyn<any> = vope_a.mul_generalized(vope_b);
  return ab.mul_generalized(vope_d);
}

export function vole_mul3_verifier_check<T>(n: number, delta: DeltaDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, q_d: QDyn<any>, vope_abd: VopeDyn<any>): [QDyn<any>, boolean]
{
  const q_abd = fieldMul(vope_abd, __clone(delta));
  let ok = true;
  for (let i = 0; i < n; i++)   {
    const lhs = fieldMul(fieldMul(__clone(q_a.q[i]), __clone(q_b.q[i])), __clone(q_d.q[i]));
    ok = (ok && (lhs === q_abd.q[i]));
  }
  return [q_abd, ok];
}

export function vole_sbox_prover_step<T>(n: number, vope_a: VopeDyn<any>, vope_b: VopeDyn<any>): [VopeDyn<any>, VopeDyn<any>]
{
  const k2: VopeDyn<any> = vope_a.mul_generalized(vope_b);
  const k1 = new VopeDyn({ u: Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_: any) => __clone(k2.u[1])), v: __clone(k2.u[0]), n: 0, k: 1 });
  return [k1, k2];
}

export function vole_sbox_verifier_check<T>(n: number, delta: DeltaDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, vope_k2: VopeDyn<any>): [QDyn<any>, boolean]
{
  const q_c = fieldMul(vope_k2, __clone(delta));
  let ok = true;
  for (let i = 0; i < n; i++)   {
    ok = (ok && (fieldMul(__clone(q_a.q[i]), __clone(q_b.q[i])) === q_c.q[i]));
  }
  return [q_c, ok];
}

export function xor_in_place(a: readonly number[], b: readonly number[])
{
  for (let i = 0; i < a.length; i++)   {
    a[i] ^= b[i];
  }
}

export function zk_hash(key: UniversalHashKey, elements: readonly Galois128[]): UniversalHashOutput
{
  let h0 = Galois128(0);
  let h1 = new Galois64(0);
  let pow0 = key.r0;
  let pow1 = key.r1;
  for (const x of elements)   {
    h0 = fieldAdd(h0, fieldMul(x, pow0));
    const x64 = new Galois64(BigInt(x._0));
    h1 = fieldAdd(h1, fieldMul(x64, pow1));
    pow0 = fieldMul(pow0, key.r0);
    pow1 = fieldMul(pow1, key.r1);
  }
  return new UniversalHashOutput({ h0: h0, h1: h1 });
}

export function zq_add(a: Zq, b: Zq): Zq
{
  return fieldBitand(wrappingAdd(a, b), LWE_Q_MASK);
}

export function zq_mul(a: Zq, b: Zq): Zq
{
  return fieldBitand(Math.imul(a, b), LWE_Q_MASK);
}

export function zq_neg(a: Zq): Zq
{
  return fieldBitand(wrappingSub(LWE_Q, a), LWE_Q_MASK);
}

export function zq_sub(a: Zq, b: Zq): Zq
{
  return fieldBitand(wrappingSub(a, b), LWE_Q_MASK);
}

export function absorb(data: readonly number[])
{
  return (() => { const __match = this; if (true /* Sponge::Shake128 */) { const h = __match._0;
return h.update(data); } else { const h = __match._0;
return h.update(data); } })();
}

export function party_index(...__args: any[]): any {
  if (__args.length === 1) {
    const requested = __args[0];
    return (() => {
  return requested;
})();
  } else
  if (__args.length === 1) {
    const _ = __args[0];
    return (() => {
  return 0;
})();
  }
  throw new Error("party_index(): no matching variant for " + __args.length + " args");
}

export function squeeze(n: number): Vec<number>
{
  let out = [];
  (() => { const __match = this; if (true /* Sponge::Shake128 */) { const h = __match._0;
return (() => {
  let r = __clone(h).finalize_xof();
  r.read(out);
})(); } else { const h = __match._0;
return (() => {
  let r = __clone(h).finalize_xof();
  r.read(out);
})(); } })();
  return out;
}

