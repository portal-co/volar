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
  commit,
  doubleVec,
  wrappingAdd,
  wrappingSub,
  asRefU8,
} from "volar-runtime";

export class DeltaDyn<T> {
  constructor(
    public n: number,
    public delta: T[],
  ) {}

  remap(m: number, f: (arg: number) => number): DeltaDyn<T>
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(delta[(f(i) % n)])), 0);
  }

  rotate_left(n: number): this
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingSub(a, n));
  }

  rotate_right(n: number): this
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingAdd(a, n));
  }

  static_<U, O>(val: U[]): QDyn<O>
  {
    const n: number = this.n;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => fieldMul(structuredClone(val[i]), structuredClone(this.delta[i]))), 0);
  }

  clone(): this
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(delta[i])), 0);
  }

  eq(other: this): boolean
  {
    const n: number = this.n;
    const { delta: d1, ...__rest } = this;
    const { delta: d2, ...__rest } = other;
    for (let i = 0; i < n; i++) {
  return (() => { if ((d1[i] !== d2[i])) {
  return false;
} })();
};
    return true;
  }

  rotate_left_bits(n: number): this
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  const next = structuredClone(delta[(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(8, Number(n)))));
})()), 0);
  }

  rotate_right_bits(n: number): this
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(delta[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(delta[i]);
  return BitsInBytes(fieldBitor(prev.shl(fieldSub(8, Number(n))), b.shr(Number(n))));
})()), 0);
  }

  bit(n: number): DeltaDyn<Bit>
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), 0);
  }

  rotate_left_bits(n: number): this
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  const next = structuredClone(delta[(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(64, Number(n)))));
})()), 0);
  }

  rotate_right_bits(n: number): this
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(delta[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(delta[i]);
  return BitsInBytes64(fieldBitor(prev.shl(fieldSub(64, Number(n))), b.shr(Number(n))));
})()), 0);
  }

  bit(n: number): DeltaDyn<Bit>
  {
    const n: number = this.n;
    const { delta, ...__rest } = this;
    return new DeltaDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), 0);
  }
}

export class QDyn<T> {
  constructor(
    public n: number,
    public q: T[],
  ) {}

  remap(m: number, f: (arg: number) => number): QDyn<T>
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(q[(f(i) % n)])), 0);
  }

  rotate_left(n: number): this
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingSub(a, n));
  }

  rotate_right(n: number): this
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingAdd(a, n));
  }

  clone(): this
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(q[i])), 0);
  }

  eq(other: this): boolean
  {
    const n: number = this.n;
    const { q: q1, ...__rest } = this;
    const { q: q2, ...__rest } = other;
    for (let i = 0; i < n; i++) {
  return (() => { if ((q1[i] !== q2[i])) {
  return false;
} })();
};
    return true;
  }

  rotate_left_bits(n: number): this
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  const next = structuredClone(q[(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(8, Number(n)))));
})()), 0);
  }

  rotate_right_bits(n: number): this
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(q[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(q[i]);
  return BitsInBytes(fieldBitor(prev.shl(fieldSub(8, Number(n))), b.shr(Number(n))));
})()), 0);
  }

  bit(n: number): QDyn<Bit>
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), 0);
  }

  rotate_left_bits(n: number): this
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  const next = structuredClone(q[(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(64, Number(n)))));
})()), 0);
  }

  rotate_right_bits(n: number): this
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(q[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(q[i]);
  return BitsInBytes64(fieldBitor(prev.shl(fieldSub(64, Number(n))), b.shr(Number(n))));
})()), 0);
  }

  bit(n: number): QDyn<Bit>
  {
    const n: number = this.n;
    const { q, ...__rest } = this;
    return new QDyn(Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), 0);
  }
}

export class ABODyn<B, D> {
  constructor(
    public k: number,
    public commit: number[],
    public per_byte: number[][],
  ) {}

  open(t: number, u: number, m: number, bad: bigint[], rand: R): ABOOpeningDyn<B, D>
  {
    const k: number = this.k;
    return new ABOOpeningDyn(structuredClone(bad), Array.from({length: t - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const bad = structuredClone(bad);
  return Array.from({length: u - 0}, (_, __i) => __i + 0).map((j) => (() => {
  const i2 = fieldBitor(i, fieldShl(Number(j), ilog2(t)));
  return (() => { if (bad.includes(BigInt(i2))) {
  const h = commit(this.per_byte[i2], rand);
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return asRefU8(h).at(j).valueOf();
})());
} else {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return this.per_byte[i2].at(j).valueOf();
})());
} })();
})());
})()), 0, 0);
  }

  to_vole_material(n: number): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material(s);
})());
  }

  to_vole_material_typenum(n: number): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material(s);
})());
  }

  to_vole_material_expanded(n: number, f: (arg: Uint8Array) => X): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material_expanded(s, f);
})());
  }

  to_vole_material_typenum_expanded(n: number, f: (arg: Uint8Array) => X): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material_expanded(s, f);
})());
  }

  split_bit_typenum(n: number): BSplitDyn<B, D>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return new BSplitDyn(Array.from({length: ilog2(/* D::OutputSize */ 0) - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b) => (() => {
  return s.map((val, i) => [i, val] as [number, typeof val]).map(([a, c]) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return Some(structuredClone(c));
} else {
  return None;
} })();
})()).filter((__x) => __x !== undefined).reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((_) => 0));
})());
})()));
})());
  }
}

export class ABOOpeningDyn<B, D> {
  constructor(
    public t: number,
    public u: number,
    public bad: bigint[],
    public openings: number[][][],
  ) {}

  validate(commit_: number[], rand: R): boolean
  {
    const t: number = this.t;
    const u: number = this.u;
    let h = new D();
    for (let i = 0; i < t; i++) {
  return for (let b = 0; b < u; b++) {
  const i2 = fieldBitor(i, fieldShl(Number(b), ilog2(t)));
  return (() => { if (this.bad.includes(BigInt(i2))) {
  h.update(this.openings[i][b][Array.from({length: /* D::OutputSize */ 0}, (_, i) => i)]);
} else {
  h.update(commit(this.openings[i][b][Array.from({length: /* B::OutputSize */ 0}, (_, i) => i)], rand));
} })();
};
};
    return ([...h.finalize()].slice() === commit_.slice());
  }

  to_vole_material(n: number): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material(s);
})());
  }

  to_vole_material_typenum(n: number): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material(s);
})());
  }

  to_vole_material_expanded(n: number, f: (arg: Uint8Array) => X): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material_expanded(s, f);
})());
  }

  to_vole_material_typenum_expanded(n: number, f: (arg: Uint8Array) => X): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material_expanded(s, f);
})());
  }

  split_bit_typenum(n: number): BSplitDyn<B, D>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return new BSplitDyn(Array.from({length: ilog2(/* D::OutputSize */ 0) - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b) => (() => {
  return s.map((val, i) => [i, val] as [number, typeof val]).map(([a, c]) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return Some(structuredClone(c));
} else {
  return None;
} })();
})()).filter((__x) => __x !== undefined).reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((_) => 0));
})());
})()));
})());
  }
}

export class BSplitDyn<B, D> {
  constructor(
    public split: number[][][],
  ) {}
}

export class PolyDyn<T> {
  constructor(
    public n: number,
    public c0: T,
    public c1: T[],
  ) {}

  get_qs_pool<Q, A>(m: number, x: number, root: DeltaDyn<Q>, inputs: PolyInputPoolDyn<QDyn<Q>>, reduction: number): QDyn<A>
  {
    const n: number = this.n;
    return new QDyn(Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum: A = structuredClone(this.c0);
  for (let _ = 0; _ < n; _++) {
  sum = fieldMul(structuredClone(root.delta[i]), sum);
};
  for (let j = 0; j < n; j++) {
  let b: A = structuredClone(this.c1[j]);
  for (const i2 of inputs.indices) {
  return for (let _ = 0; _ < reduction; _++) {
  b = fieldMul(structuredClone(inputs.inputs[i2[j]].q[i]), b);
};
};
  sum = fieldAdd(sum, b);
};
  return sum;
})()), 0);
  }

  get_qs<Q, A>(m: number, x: number, root: DeltaDyn<Q>, inputs: QDyn<Q>[][], reduction: number): QDyn<A>
  {
    const n: number = this.n;
    return new QDyn(Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum: A = structuredClone(this.c0);
  for (let _ = 0; _ < n; _++) {
  sum = fieldMul(structuredClone(root.delta[i]), sum);
};
  for (let j = 0; j < n; j++) {
  let b: A = structuredClone(this.c1[j]);
  for (const i2 of inputs) {
  return for (let _ = 0; _ < reduction; _++) {
  b = fieldMul(structuredClone(i2[j].q[i]), b);
};
};
  sum = fieldAdd(sum, b);
};
  return sum;
})()), 0);
  }

  apply_pool<M, O>(m: number, x: number, x2: number, xs: number, s: number, voles: PolyInputPoolDyn<VopeDyn<T>>): VopeDyn<O>
  {
    const n: number = this.n;
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = O.default();
  for (let k = 0; k < n; k++) {
  let b: O = structuredClone(this.c1[k]);
  for (const v of voles.indices) {
  b = fieldMul(b, structuredClone(voles.inputs[v[k]].v[i]));
};
  sum = fieldAdd(sum, b);
};
  const c0: O = structuredClone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = O.default();
  for (let k = 0; k < n; k++) {
  return for (let n = 0; n < x; n++) {
  let b: O = structuredClone(this.c1[k]);
  for (let m = 0; m < s; m++) {
  const l = fieldAdd(fieldMul(l, s), m);
  return for (const [idx, v] of voles.indices.map((val, i) => [i, val] as [number, typeof val])) {
  b = fieldMul(b, (() => { if ((idx === n)) {
  return structuredClone(voles.inputs[v[k]].u[l][i]);
} else {
  return structuredClone(voles.inputs[v[k]].v[i]);
} })());
};
};
  sum = fieldAdd(sum, b);
};
};
  return sum;
})());
})());
    return new VopeDyn(u, v, 0, 1);
  }

  apply<M, O>(m: number, x: number, x2: number, xs: number, s: number, voles: VopeDyn<T>[][]): VopeDyn<O>
  {
    const n: number = this.n;
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = O.default();
  for (let k = 0; k < n; k++) {
  let b: O = structuredClone(this.c1[k]);
  for (const v of voles) {
  b = fieldMul(b, structuredClone(v[k].v[i]));
};
  sum = fieldAdd(sum, b);
};
  const c0: O = structuredClone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = O.default();
  for (let k = 0; k < n; k++) {
  return for (let n = 0; n < x; n++) {
  let b: O = structuredClone(this.c1[k]);
  for (let m = 0; m < s; m++) {
  const l = fieldAdd(fieldMul(l, s), m);
  return for (const [idx, v] of voles.map((val, i) => [i, val] as [number, typeof val])) {
  b = fieldMul(b, (() => { if ((idx === n)) {
  return structuredClone(v[k].u[l][i]);
} else {
  return structuredClone(v[k].v[i]);
} })());
};
};
  sum = fieldAdd(sum, b);
};
};
  return sum;
})());
})());
    return new VopeDyn(u, v, 0, 1);
  }
}

export class PolyInputPoolDyn<T> {
  constructor(
    public n: number,
    public x: number,
    public inputs: T[],
    public indices: number[][],
  ) {}
}

export class BitVoleDyn<T> {
  constructor(
    public n: number,
    public u: Bit[],
    public v: T[],
  ) {}
}

export class VopeDyn<T> {
  constructor(
    public n: number,
    public k: number,
    public u: T[][],
    public v: T[],
  ) {}

  mul_generalized(k2: number, other: VopeDyn<T>): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    let res_u = Array.from({length: fieldAdd(k2, k) - 0}, (_, __i) => __i + 0).map((_) => []);
    let res_v = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_) => undefined /* default for TypeParam("T") */);
    for (let i = 0; i <= k; i++) {
  return for (let j = 0; j <= k2; j++) {
  const k = fieldAdd(i, j);
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
  return (() => { if ((k === 0)) {
  return for (let lane = 0; lane < n; lane++) {
  res_v[lane] = fieldAdd(structuredClone(res_v[lane]), fieldMul(structuredClone(a_coeff[lane]), structuredClone(b_coeff[lane])));
};
} else {
  return for (let lane = 0; lane < n; lane++) {
  res_u[fieldSub(k, 1)][lane] = fieldAdd(structuredClone(res_u[fieldSub(k, 1)][lane]), fieldMul(structuredClone(a_coeff[lane]), structuredClone(b_coeff[lane])));
};
} })();
};
};
    return new VopeDyn(res_u, res_v, 0, 1);
  }

  rotate_left_bits(n: number): this
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  const next = structuredClone(u[l][(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(8, Number(n)))));
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  const next = structuredClone(v[(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(8, Number(n)))));
})()), 0, 1);
  }

  rotate_right_bits(n: number): this
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(u[l][(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(u[l][i]);
  return BitsInBytes(fieldBitor(prev.shl(fieldSub(8, Number(n))), b.shr(Number(n))));
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(v[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(v[i]);
  return BitsInBytes(fieldBitor(prev.shl(fieldSub(8, Number(n))), b.shr(Number(n))));
})()), 0, 1);
  }

  bit(n: number): VopeDyn<Bit>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), 0, 1);
  }

  rotate_left_bits(n: number): this
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  const next = structuredClone(u[l][(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(64, Number(n)))));
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  const next = structuredClone(v[(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(b.shl(Number(n)), next.shr(fieldSub(64, Number(n)))));
})()), 0, 1);
  }

  rotate_right_bits(n: number): this
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(u[l][(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(u[l][i]);
  return BitsInBytes64(fieldBitor(prev.shl(fieldSub(64, Number(n))), b.shr(Number(n))));
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(v[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(v[i]);
  return BitsInBytes64(fieldBitor(prev.shl(fieldSub(64, Number(n))), b.shr(Number(n))));
})()), 0, 1);
  }

  bit(n: number): VopeDyn<Bit>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), 0, 1);
  }

  clone(): this
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(u[l][i]));
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(v[i])), 0, 1);
  }

  eq(other: this): boolean
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u: u1, v: v1, ...__rest } = this;
    const { u: u2, v: v2, ...__rest } = other;
    for (let l = 0; l < k; l++) {
  return for (let i = 0; i < n; i++) {
  return (() => { if ((u1[l][i] !== u2[l][i])) {
  return false;
} })();
};
};
    for (let i = 0; i < n; i++) {
  return (() => { if ((v1[i] !== v2[i])) {
  return false;
} })();
};
    return true;
  }

  static constant(n: number, v: T[]): this
  {
    const k: number = 0;
    return new VopeDyn(Array.from({length: 0 - 0}, (_, __i) => __i + 0).map((_) => (() => { throw new Error("unreachable"); })()), v, 0, 1);
  }

  add(rhs: VopeDyn<U>): number /* this::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn(this.u.map((__a, __i) => [__a, rhs.u[__i]] as [typeof __a, unknown]).map(([a, b]) => a.map((__a, __i) => [__a, b[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldAdd(a, b))), this.v.map((__a, __i) => [__a, rhs.v[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldAdd(a, b)), 0, 1);
  }

  bitxor(rhs: U[]): number /* this::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((i) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((j) => (() => {
  const o: O = fieldBitxor(structuredClone(this.u[i][j]), structuredClone(rhs[fieldAdd(fieldMul(i, k), j)]));
  return o;
})());
})()), this.v.map((a) => a), 0, 1);
  }

  mul(rhs: DeltaDyn<U>): number /* this::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new QDyn(this.u.map((val, i) => [i, val] as [number, typeof val]).reduce((a, [i, b]) => (() => {
  return a.map((__a, __i) => [__a, b[__i]] as [typeof __a, unknown]).map(([a, b]) => (() => {
  let x = structuredClone(rhs.delta[i]);
  for (let _ = 0; _ < i; _++) {
  x = fieldMul(x, structuredClone(rhs.delta[i]));
};
  const m: O = fieldMul(structuredClone(b), x);
  return fieldAdd(m, a);
})());
})(), structuredClone(this.v).map((a) => a)), 0);
  }

  expand(l: number): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: l - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => u.at(l).map_or(T.default(), (a) => structuredClone(a[i])));
})()), structuredClone(v), 0, 1);
  }

  rotate_left(n: number): this
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(n, (a) => wrappingSub(a, n));
  }

  rotate_right(n: number): this
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(n, (a) => wrappingAdd(a, n));
  }

  remap(m: number, f: (arg: number) => number): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(u[l][(f(i) % n)]));
})()), Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(v[(f(i) % n)])), 0, 1);
  }

  scale<T>(f: (arg: boolean) => T): VopeDyn<T>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v, ...__rest } = this;
    return new VopeDyn(Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  return f(b);
})());
})()), Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  return f(b);
})()), 0, 1);
  }
}

export function gen_abo<B, D>(k: number, a: number[], rand: readonly number[]): ABODyn<B, D>
{
  let h = new D();
  let per_byte = Array.from({length: k - 0}, (_, __i) => __i + 0).map((_) => undefined);
  for (let i = 0; i < k; i++) {
  const core = Array.from({length: ilog2(k) - 0}, (_, __i) => __i + 0).reduce((acc, b) => (() => {
  (() => { if ((fieldBitand(fieldShr(i, b), 1) !== 0)) {
  const doubled = doubleVec(acc);
  acc = structuredClone(doubled[1]);
} else {
  const doubled = doubleVec(acc);
  acc = structuredClone(doubled[0]);
} })();
  return acc;
})(), structuredClone(a));
  h.update(commit(core, rand));
  per_byte[i] = core;
};
  return new ABODyn([...h.finalize()], per_byte, 0);
}

export function create_vole_from_material<B>(s: readonly X[]): VopeDyn<number>
{
  const u: number[] = s.reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((_) => 0));
  const v: number[] = s.map((val, i) => [i, val] as [number, typeof val]).reduce((a, [i, b]) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(fieldBitxor(a, b), ((i) & 0xFF)));
})(), Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((_) => 0));
  return new VopeDyn(Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_) => structuredClone(u)), v, 0, 1);
}

export function create_vole_from_material_expanded<B>(s: readonly Y[], f: (arg: Uint8Array) => X): VopeDyn<number>
{
  const u: number[] = s.map((b) => f(asRefU8(b)[Array.from({length: /* B::OutputSize */ 0}, (_, i) => i)])).reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((_) => 0));
  const v: number[] = s.map((b) => f(asRefU8(b)[Array.from({length: /* B::OutputSize */ 0}, (_, i) => i)])).map((val, i) => [i, val] as [number, typeof val]).reduce((a, [i, b]) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(fieldBitxor(a, b), ((i) & 0xFF)));
})(), Array.from({length: /* B::OutputSize */ 0 - 0}, (_, __i) => __i + 0).map((_) => 0));
  return new VopeDyn(Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_) => structuredClone(u)), v, 0, 1);
}

