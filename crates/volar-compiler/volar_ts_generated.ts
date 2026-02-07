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
  n!: number;
  delta!: any[];

  constructor(init: { 
    n: number,
    delta: any[]
  }) {
    Object.assign(this, init);
  }

  clone(): any
  {
    const n: number = this.n;
    const { delta } = this;
    return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(delta[i])), n: 0 });
  }

  eq(other: any): boolean
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
    return new DeltaDyn({ delta: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(delta[(f(i) % n)])), n: 0 });
  }

  rotate_left(n: number): any
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingSub(a, n));
  }

  rotate_right(n: number): any
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingAdd(a, n));
  }

  static_<U, O>(val: any[]): QDyn<any>
  {
    const n: number = this.n;
    return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => fieldMul(structuredClone(val[i]), structuredClone(this.delta[i]))), n: 0 });
  }

  bit(n: number): DeltaDyn<Bit>
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), n: 0 });
    }
  }

  rotate_left_bits(n: number): any
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  const next = structuredClone(delta[(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(8, Number(n)))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(delta[i]);
  const next = structuredClone(delta[(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(64, Number(n)))));
})()), n: 0 });
    }
  }

  rotate_right_bits(n: number): any
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(delta[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(delta[i]);
  return BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n))), fieldShr(b, Number(n))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(delta[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(delta[i]);
  return BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n))), fieldShr(b, Number(n))));
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

  clone(): any
  {
    const n: number = this.n;
    const { q } = this;
    return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(q[i])), n: 0 });
  }

  eq(other: any): boolean
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
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(q[(f(i) % n)])), n: 0 });
  }

  rotate_left(n: number): any
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingSub(a, n));
  }

  rotate_right(n: number): any
  {
    const n: number = this.n;
    return this.remap(n, (a) => wrappingAdd(a, n));
  }

  bit(n: number): QDyn<Bit>
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), n: 0 });
    }
  }

  rotate_left_bits(n: number): any
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  const next = structuredClone(q[(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(8, Number(n)))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(q[i]);
  const next = structuredClone(q[(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(64, Number(n)))));
})()), n: 0 });
    }
  }

  rotate_right_bits(n: number): any
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(q[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(q[i]);
  return BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n))), fieldShr(b, Number(n))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(q[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(q[i]);
  return BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n))), fieldShr(b, Number(n))));
})()), n: 0 });
    }
  }
}

export class ABODyn<B, D> {
  k!: number;
  commit!: number[];
  per_byte!: number[][];

  constructor(init: { 
    k: number,
    commit: number[],
    per_byte: number[][]
  }) {
    Object.assign(this, init);
  }

  open(t: number, u: number, m: number, bad: bigint[], rand: any): ABOOpeningDyn<B, D>
  {
    const k: number = this.k;
    return new ABOOpeningDyn({ bad: structuredClone(bad), openings: Array.from({length: t - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const bad = structuredClone(bad);
  return Array.from({length: u - 0}, (_, __i) => __i + 0).map((j) => (() => {
  const i2 = fieldBitor(i, fieldShl(Number(j), ilog2(t)));
  return (() => { if (bad.includes(BigInt(i2))) {
  const h = commit(this.per_byte[i2], rand);
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return (asRefU8(h)?.[j] ?? 0);
})());
} else {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return (this.per_byte[i2]?.[j] ?? 0);
})());
} })();
})());
})()), t: 0, u: 0 });
  }

  split_bit_typenum(ctx: { B_OutputSize: number, D_OutputSize: number }, n: number): BSplitDyn<B, D>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return new BSplitDyn({ split: Array.from({length: ilog2(ctx.D_OutputSize) - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b) => (() => {
  return s.map((val, i) => [i, val] as [number, typeof val]).map(([a, c]) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return structuredClone(c);
} else {
  return undefined;
} })();
})()).filter((__x) => __x !== undefined).reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_) => 0));
})());
})()) });
})());
  }

  to_vole_material(ctx: { B_OutputSize: number }, n: number): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_expanded(ctx: { B_OutputSize: number }, n: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  to_vole_material_typenum(ctx: { B_OutputSize: number }, n: number): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_typenum_expanded(ctx: { B_OutputSize: number }, n: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const k: number = this.k;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.per_byte[Array.from({length: 0 - fieldMul(i, n)}, (_, i) => i + fieldMul(i, n))][Array.from({length: n}, (_, i) => i)];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }
}

export class ABOOpeningDyn<B, D> {
  t!: number;
  u!: number;
  bad!: bigint[];
  openings!: number[][][];

  constructor(init: { 
    t: number,
    u: number,
    bad: bigint[],
    openings: number[][][]
  }) {
    Object.assign(this, init);
  }

  split_bit_typenum(ctx: { B_OutputSize: number, D_OutputSize: number }, n: number): BSplitDyn<B, D>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return new BSplitDyn({ split: Array.from({length: ilog2(ctx.D_OutputSize) - 0}, (_, __i) => __i + 0).map((j) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b) => (() => {
  return s.map((val, i) => [i, val] as [number, typeof val]).map(([a, c]) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return structuredClone(c);
} else {
  return undefined;
} })();
})()).filter((__x) => __x !== undefined).reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_) => 0));
})());
})()) });
})());
  }

  to_vole_material(ctx: { B_OutputSize: number }, n: number): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_expanded(ctx: { B_OutputSize: number }, n: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  to_vole_material_typenum(ctx: { B_OutputSize: number }, n: number): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_typenum_expanded(ctx: { B_OutputSize: number }, n: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
  {
    const t: number = this.t;
    const u: number = this.u;
    return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const s = this.openings[i];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  validate(ctx: { B_OutputSize: number, D_OutputSize: number, newD: () => any }, commit_: number[], rand: any): boolean
  {
    const t: number = this.t;
    const u: number = this.u;
    let h = ctx.newD();
    for (let i = 0; i < t; i++)     {
      for (let b = 0; b < u; b++)       {
        const i2 = fieldBitor(i, fieldShl(Number(b), ilog2(t)));
        if (this.bad.includes(BigInt(i2)))         {
          h.update(this.openings[i][b][Array.from({length: ctx.D_OutputSize}, (_, i) => i)]);
        } else         {
          h.update(commit(this.openings[i][b][Array.from({length: ctx.B_OutputSize}, (_, i) => i)], rand));
        }
      }
    }
    return ([...h.finalize()].slice() === commit_.slice());
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
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    let b: any = structuredClone(this.c1[k]);
    for (const v of voles)     {
      b = fieldMul(b, structuredClone(v[k].v[i]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = structuredClone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    for (let n = 0; n < x; n++)     {
      let b: any = structuredClone(this.c1[k]);
      for (let m = 0; m < s; m++)       {
        const l = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.map((val, i) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if ((idx === n)) {
  return structuredClone(v[k].u[l][i]);
} else {
  return structuredClone(v[k].v[i]);
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
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    let b: any = structuredClone(this.c1[k]);
    for (const v of voles.indices)     {
      b = fieldMul(b, structuredClone(voles.inputs[v[k]].v[i]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = structuredClone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    for (let n = 0; n < x; n++)     {
      let b: any = structuredClone(this.c1[k]);
      for (let m = 0; m < s; m++)       {
        const l = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.indices.map((val, i) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if ((idx === n)) {
  return structuredClone(voles.inputs[v[k]].u[l][i]);
} else {
  return structuredClone(voles.inputs[v[k]].v[i]);
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
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum: any = structuredClone(this.c0);
  for (let _ = 0; _ < n; _++)   {
    sum = fieldMul(structuredClone(root.delta[i]), sum);
  }
  for (let j = 0; j < n; j++)   {
    let b: any = structuredClone(this.c1[j]);
    for (const i2 of inputs)     {
      for (let _ = 0; _ < reduction; _++)       {
        b = fieldMul(structuredClone(i2[j].q[i]), b);
      }
    }
    sum = fieldAdd(sum, b);
  }
  return sum;
})()), n: 0 });
  }

  get_qs_pool<Q, A>(m: number, x: number, root: DeltaDyn<any>, inputs: PolyInputPoolDyn<QDyn<any>>, reduction: number): QDyn<any>
  {
    const n: number = this.n;
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => (() => {
  let sum: any = structuredClone(this.c0);
  for (let _ = 0; _ < n; _++)   {
    sum = fieldMul(structuredClone(root.delta[i]), sum);
  }
  for (let j = 0; j < n; j++)   {
    let b: any = structuredClone(this.c1[j]);
    for (const i2 of inputs.indices)     {
      for (let _ = 0; _ < reduction; _++)       {
        b = fieldMul(structuredClone(inputs.inputs[i2[j]].q[i]), b);
      }
    }
    sum = fieldAdd(sum, b);
  }
  return sum;
})()), n: 0 });
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

  add(rhs: VopeDyn<any>): number /* any::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn({ u: this.u.map((__a, __i) => [__a, rhs.u[__i]] as [typeof __a, unknown]).map(([a, b]) => a.map((__a, __i) => [__a, b[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldAdd(a, b))), v: this.v.map((__a, __i) => [__a, rhs.v[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldAdd(a, b)), n: 0, k: 1 });
  }

  bitxor(rhs: any[]): number /* any::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((i) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((j) => (() => {
  const o: any = fieldBitxor(structuredClone(this.u[i][j]), structuredClone(rhs[fieldAdd(fieldMul(i, k), j)]));
  return o;
})());
})()), v: this.v.map((a) => a), n: 0, k: 1 });
  }

  clone(): any
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(u[l][i]));
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => structuredClone(v[i])), n: 0, k: 1 });
  }

  static constant(n: number, v: any[]): any
  {
    const k: number = 0;
    return new VopeDyn({ u: Array.from({length: 0 - 0}, (_, __i) => __i + 0).map((_) => (() => { throw new Error("unreachable"); })()), v: v, n: 0, k: 1 });
  }

  eq(other: any): boolean
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
    return new VopeDyn({ u: Array.from({length: l - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => ((u?.[l]) != null ? ((a) => structuredClone(a[i]))(u?.[l]) : (ctx.defaultT())));
})()), v: structuredClone(v), n: 0, k: 1 });
  }

  mul(rhs: DeltaDyn<any>): number /* any::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new QDyn({ q: this.u.map((val, i) => [i, val] as [number, typeof val]).reduce((a, [i, b]) => (() => {
  return a.map((__a, __i) => [__a, b[__i]] as [typeof __a, unknown]).map(([a, b]) => (() => {
  let x = structuredClone(rhs.delta[i]);
  for (let _ = 0; _ < i; _++)   {
    x = fieldMul(x, structuredClone(rhs.delta[i]));
  }
  const m: any = fieldMul(structuredClone(b), x);
  return fieldAdd(m, a);
})());
})(), structuredClone(this.v).map((a) => a)), n: 0 });
  }

  mul_generalized(ctx: { defaultT: () => any }, k2: number, other: VopeDyn<any>): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    let res_u = Array.from({length: fieldAdd(k2, k) - 0}, (_, __i) => __i + 0).map((_) => []);
    let res_v = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_) => ctx.defaultT());
    for (let i = 0; i <= k; i++)     {
      for (let j = 0; j <= k2; j++)       {
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
        if ((k === 0))         {
          for (let lane = 0; lane < n; lane++)           {
            res_v[lane] = fieldAdd(structuredClone(res_v[lane]), fieldMul(structuredClone(a_coeff[lane]), structuredClone(b_coeff[lane])));
          }
        } else         {
          for (let lane = 0; lane < n; lane++)           {
            res_u[fieldSub(k, 1)][lane] = fieldAdd(structuredClone(res_u[fieldSub(k, 1)][lane]), fieldMul(structuredClone(a_coeff[lane]), structuredClone(b_coeff[lane])));
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
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(u[l][(f(i) % n)]));
})()), v: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i) => structuredClone(v[(f(i) % n)])), n: 0, k: 1 });
  }

  rotate_left(n: number): any
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(n, (a) => wrappingSub(a, n));
  }

  rotate_right(n: number): any
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(n, (a) => wrappingAdd(a, n));
  }

  scale<T>(f: (arg: boolean) => any): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  return f(b);
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  return f(b);
})()), n: 0, k: 1 });
  }

  bit(n: number): VopeDyn<Bit>
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  return Bit((fieldBitand(fieldShr(b, n), 1) !== 0));
})()), n: 0, k: 1 });
    }
  }

  rotate_left_bits(n: number): any
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  const next = structuredClone(u[l][(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(8, Number(n)))));
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  const next = structuredClone(v[(fieldAdd(i, 1) % n)]);
  return BitsInBytes(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(8, Number(n)))));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(u[l][i]);
  const next = structuredClone(u[l][(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(64, Number(n)))));
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const b = structuredClone(v[i]);
  const next = structuredClone(v[(fieldAdd(i, 1) % n)]);
  return BitsInBytes64(fieldBitor(fieldShl(b, Number(n)), fieldShr(next, fieldSub(64, Number(n)))));
})()), n: 0, k: 1 });
    }
  }

  rotate_right_bits(n: number): any
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(u[l][(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(u[l][i]);
  return BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n))), fieldShr(b, Number(n))));
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(v[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(v[i]);
  return BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n))), fieldShr(b, Number(n))));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(u[l][(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(u[l][i]);
  return BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n))), fieldShr(b, Number(n))));
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i) => (() => {
  const prev = structuredClone(v[(fieldSub(fieldAdd(i, n), 1) % n)]);
  const b = structuredClone(v[i]);
  return BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n))), fieldShr(b, Number(n))));
})()), n: 0, k: 1 });
    }
  }
}

export function gen_abo<B, D>(ctx: { newD: () => any }, k: number, a: number[], rand: readonly number[]): ABODyn<B, D>
{
  let h = ctx.newD();
  let per_byte = Array.from({length: k - 0}, (_, __i) => __i + 0).map((_) => undefined);
  for (let i = 0; i < k; i++)   {
    const core = Array.from({length: ilog2(k) - 0}, (_, __i) => __i + 0).reduce((acc, b) => (() => {
  if ((fieldBitand(fieldShr(i, b), 1) !== 0))   {
    const doubled = doubleVec(acc);
    acc = structuredClone(doubled[1]);
  } else   {
    const doubled = doubleVec(acc);
    acc = structuredClone(doubled[0]);
  }
  return acc;
})(), structuredClone(a));
    h.update(commit(core, rand));
    per_byte[i] = core;
  }
  return new ABODyn({ commit: [...h.finalize()], per_byte: per_byte, k: 0 });
}

export function create_vole_from_material(ctx: { B_OutputSize: number }, s: readonly any[]): VopeDyn<number>
{
  const u: number[] = s.reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_) => 0));
  const v: number[] = s.map((val, i) => [i, val] as [number, typeof val]).reduce((a, [i, b]) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(fieldBitxor(a, b), ((i) & 0xFF)));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_) => 0));
  return new VopeDyn({ u: Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_) => structuredClone(u)), v: v, n: 0, k: 1 });
}

export function create_vole_from_material_expanded(ctx: { B_OutputSize: number }, s: readonly any[], f: (arg: Uint8Array) => any): VopeDyn<number>
{
  const u: number[] = s.map((b) => f(asRefU8(b)[Array.from({length: ctx.B_OutputSize}, (_, i) => i)])).reduce((a, b) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(a, b));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_) => 0));
  const v: number[] = s.map((b) => f(asRefU8(b)[Array.from({length: ctx.B_OutputSize}, (_, i) => i)])).map((val, i) => [i, val] as [number, typeof val]).reduce((a, [i, b]) => (() => {
  return a.map((__a, __i) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i) => asRefU8(b)[i])[__i]] as [typeof __a, unknown]).map(([a, b]) => fieldBitxor(fieldBitxor(a, b), ((i) & 0xFF)));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_) => 0));
  return new VopeDyn({ u: Array.from({length: 1 - 0}, (_, __i) => __i + 0).map((_) => structuredClone(u)), v: v, n: 0, k: 1 });
}

