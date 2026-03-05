// Auto-generated TypeScript from volar-spec
// Type-level lengths have been converted to runtime number witnesses
// @ts-nocheck — generated code uses dynamic patterns that need runtime dispatch

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
} from "./index";

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
    return new EvalDyn({ target: (() => {
  let d = ctx.newD();
  d.update(this.target);
  d.update(other.target);
  return [...d.finalize()];
})().map((__a: any, __i: number) => [__a, structuredClone(table.table[index])[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldBitxor(a, b)), n: 0 });
  }

  bitxor(rhs: EvalDyn): number /* any::Output */
  {
    const n: number = this.n;
    return new EvalDyn({ target: this.target.map((__a: any, __i: number) => [__a, rhs.target[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldBitxor(a, b)), n: 0 });
  }

  open(garble: GarbleDyn): number[]
  {
    const n: number = this.n;
    return structuredClone(this.target).map((__a: any, __i: number) => [__a, structuredClone(garble.base)[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldBitxor(a, b));
  }

  to_share(o: number): EvalDyn
  {
    const n: number = this.n;
    return new EvalDyn({ target: Array.from({length: o - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let v = 0;
  for (let j = 0; j < 8; j++)   {
    const bit = fieldBitand(this.target[fieldAdd(fieldMul(i, 8), j)], 1);
    fieldAdd(v, fieldShl(bit, j));
  }
  return v;
})()), n: 0 });
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

  share(target: number[]): EvalDyn
  {
    const n: number = this.n;
    return new EvalDyn({ target: structuredClone(this.base).map((__a: any, __i: number) => [__a, structuredClone(target)[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldBitxor(a, b)), n: 0 });
  }

  to_share(o: number): GarbleDyn
  {
    const n: number = this.n;
    return new GarbleDyn({ base: Array.from({length: o - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let v = 0;
  for (let j = 0; j < 8; j++)   {
    const bit = fieldBitand(this.base[fieldAdd(fieldMul(i, 8), j)], 1);
    fieldAdd(v, fieldShl(bit, j));
  }
  return v;
})()), n: 0 });
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
    return new EvalDyn({ target: structuredClone(this.secret).map((__a: any, __i: number) => [__a, structuredClone(garble.base)[__i]] as [typeof __a, any]).map(([a, b]: any) => (() => { if (value) {
  return fieldBitxor(a, b);
} else {
  return b;
} })()), n: 0 });
  }

  gen_and_table(ctx: { newD: () => any }, a: GarbleDyn, b: GarbleDyn): GarbleTableDyn
  {
    const n: number = this.n;
    let table = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => 0));
    for (let i = 0; i < 4; i++)     {
      const av = (fieldBitand(i, 1) !== 0);
      const bv = (fieldBitand(i, 2) !== 0);
      let d = ctx.newD();
      d.update(this.encode(a, av).target);
      d.update(this.encode(b, bv).target);
      const target = [...d.finalize()];
      const index = fieldBitor((() => { if ((fieldBitand(a.base[0], 1) !== 1)) {
  return 1;
} else {
  return 0;
} })(), (() => { if ((fieldBitand(b.base[0], 1) !== 1)) {
  return 2;
} else {
  return 0;
} })());
      table[index] = structuredClone(table[index]).map((__a: any, __i: number) => [__a, target[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldBitxor(a, b));
    }
    return new GarbleTableDyn({ table: table, n: 0 });
  }

  static new(n: number, secret: number[]): any
  {
    fieldAdd(secret[0], 1);
    return new Self({ secret: secret });
  }

  secret(): number[]
  {
    const n: number = this.n;
    return structuredClone(this.secret);
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
    let b: any = structuredClone(this.c1[k]);
    for (const v of voles)     {
      b = fieldMul(b, structuredClone(v[k].v[i]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = structuredClone(this.c0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    for (let n = 0; n < x; n++)     {
      let b: any = structuredClone(this.c1[k]);
      for (let m = 0; m < s; m++)       {
        const l_1 = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if ((idx === n)) {
  return structuredClone(v[k].u[l_1][i]);
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
    const v = Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
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
    const u = Array.from({length: xs - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0; k < n; k++)   {
    for (let n = 0; n < x; n++)     {
      let b: any = structuredClone(this.c1[k]);
      for (let m = 0; m < s; m++)       {
        const l_1 = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.indices.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if ((idx === n)) {
  return structuredClone(voles.inputs[v[k]].u[l_1][i]);
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
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
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
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
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
    return new VopeDyn({ u: this.u.map((__a: any, __i: number) => [__a, rhs.u[__i]] as [typeof __a, any]).map(([a, b]: any) => a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldAdd(a, b))), v: this.v.map((__a: any, __i: number) => [__a, rhs.v[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldAdd(a, b)), n: 0, k: 1 });
  }

  bitxor(rhs: any[]): number /* any::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  const o: any = fieldBitxor(structuredClone(this.u[i][j]), structuredClone(rhs[fieldAdd(fieldMul(i, k), j)]));
  return o;
})());
})()), v: this.v.map((a: any) => a), n: 0, k: 1 });
  }

  clone(): any
  {
    const n: number = this.n;
    const k: number = this.k;
    const { u, v } = this;
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(u[l][i]));
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(v[i])), n: 0, k: 1 });
  }

  static constant(n: number, v: any[]): any
  {
    const k: number = 0;
    return new VopeDyn({ u: Array.from({length: 0 - 0}, (_, __i) => __i + 0).map((_: any) => (() => { throw new Error("unreachable"); })()), v: v, n: 0, k: 1 });
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
    return new VopeDyn({ u: Array.from({length: l - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => ((u?.[l]) != null ? ((a) => structuredClone(a[i]))(u?.[l]) : (ctx.defaultT())));
})()), v: structuredClone(v), n: 0, k: 1 });
  }

  mul(rhs: DeltaDyn<any>): number /* any::Output */
  {
    const n: number = this.n;
    const k: number = this.k;
    return new QDyn({ q: this.u.map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([a, b]: any) => (() => {
  let x = structuredClone(rhs.delta[i]);
  for (let _ = 0; _ < i; _++)   {
    x = fieldMul(x, structuredClone(rhs.delta[i]));
  }
  const m: any = fieldMul(structuredClone(b), x);
  return fieldAdd(m, a);
})());
})(), structuredClone(this.v).map((a: any) => a)), n: 0 });
  }

  mul_generalized(ctx: { defaultT: () => any }, k2: number, other: VopeDyn<any>): VopeDyn<any>
  {
    const n: number = this.n;
    const k: number = this.k;
    let res_u = Array.from({length: fieldAdd(k2, k) - 0}, (_, __i) => __i + 0).map((_: any) => []);
    let res_v = Array.from({length: n - 0}, (_, __i) => __i + 0).map((_: any) => ctx.defaultT());
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
    return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(u[l][(f(i) % n)]));
})()), v: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(v[(f(i) % n)])), n: 0, k: 1 });
  }

  rotate_left(n_param: number): any
  {
    const n: number = this.n;
    const k: number = this.k;
    return this.remap(this.n, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: number): any
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
  const b = structuredClone(u[l][i]);
  return f(b);
})());
})()), v: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(v[i]);
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
  const b = structuredClone(u[l][i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(v[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(u[l][i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(v[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0, k: 1 });
    }
  }

  rotate_left_bits(n_param: number): any
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(u[l][i]);
  const next = structuredClone(u[l][(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(v[i]);
  const next = structuredClone(v[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(u[l][i]);
  const next = structuredClone(u[l][(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(v[i]);
  const next = structuredClone(v[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})()), n: 0, k: 1 });
    }
  }

  rotate_right_bits(n_param: number): any
  {
    if (this.u[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(u[l][(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(u[l][i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(v[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(v[i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0, k: 1 });
    } else {
      const n: number = this.n;
      const k: number = this.k;
      const { u, v } = this;
      return new VopeDyn({ u: Array.from({length: k - 0}, (_, __i) => __i + 0).map((l: any) => (() => {
  return Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(u[l][(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(u[l][i]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n_param))), fieldShr(b, Number(n_param))));
})());
})()), v: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(v[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(v[i]);
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

  clone(): any
  {
    const n: number = this.n;
    const { delta } = this;
    return new DeltaDyn({ delta: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(delta[i])), n: 0 });
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
    return new DeltaDyn({ delta: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(delta[(f(i) % n)])), n: 0 });
  }

  rotate_left(n_param: number): any
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: number): any
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingAdd(a, n_param));
  }

  static_<U, O>(val: any[]): QDyn<any>
  {
    const n: number = this.n;
    return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => fieldMul(structuredClone(val[i]), structuredClone(this.delta[i]))), n: 0 });
  }

  bit(n_param: number): DeltaDyn<Bit>
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(delta[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(delta[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    }
  }

  rotate_left_bits(n_param: number): any
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(delta[i]);
  const next = structuredClone(delta[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(delta[i]);
  const next = structuredClone(delta[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})()), n: 0 });
    }
  }

  rotate_right_bits(n_param: number): any
  {
    if (this.delta[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(delta[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(delta[i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { delta } = this;
      return new DeltaDyn({ delta: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(delta[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(delta[i]);
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

  clone(): any
  {
    const n: number = this.n;
    const { q } = this;
    return new QDyn({ q: Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(q[i])), n: 0 });
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
    return new QDyn({ q: Array.from({length: m - 0}, (_, __i) => __i + 0).map((i: any) => structuredClone(q[(f(i) % n)])), n: 0 });
  }

  rotate_left(n_param: number): any
  {
    const n: number = this.n;
    return this.remap(this.n, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: number): any
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
  const b = structuredClone(q[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(q[i]);
  return new Bit((fieldBitand(fieldShr(b, n_param), 1) !== 0));
})()), n: 0 });
    }
  }

  rotate_left_bits(n_param: number): any
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(q[i]);
  const next = structuredClone(q[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8, Number(n_param)))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const b = structuredClone(q[i]);
  const next = structuredClone(q[(fieldAdd(i, 1) % n_param)]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64, Number(n_param)))));
})()), n: 0 });
    }
  }

  rotate_right_bits(n_param: number): any
  {
    if (this.q[0] instanceof BitsInBytes) {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(q[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(q[i]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    } else {
      const n: number = this.n;
      const { q } = this;
      return new QDyn({ q: Array.from({length: n_param - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const prev = structuredClone(q[(fieldSub(fieldAdd(i, n_param), 1) % n_param)]);
  const b = structuredClone(q[i]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64, Number(n_param))), fieldShr(b, Number(n_param))));
})()), n: 0 });
    }
  }
}

// Orphan impls for ABODyn
export function to_vole_material(n: number, target: number): VopeDyn<number>[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[target].slice(fieldMul(i, n)).slice(0, n);
  return create_vole_from_material(s);
})());
}

export function to_vole_material_typenum(n: number, target: number): VopeDyn<number>[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[target].slice(fieldMul(i, n)).slice(0, n);
  return create_vole_from_material(s);
})());
}

export function to_vole_material_expanded(n: number, target: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[target].slice(fieldMul(i, n)).slice(0, n);
  return create_vole_from_material_expanded(s, f);
})());
}

export function to_vole_material_typenum_expanded(n: number, target: number, f: (arg: Uint8Array) => any): VopeDyn<number>[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[target].slice(fieldMul(i, n)).slice(0, n);
  return create_vole_from_material_expanded(s, f);
})());
}

export function split_bit_typenum(ctx: { B_OutputSize: number, D_OutputSize: number }, n: number, target: number): BSplitDyn<B, D>[]
{
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((i: any) => (() => {
  const s = this.per_byte[target].slice(fieldMul(i, n)).slice(0, n);
  return new BSplit({ split: Array.from({length: ilog2(ctx.D_OutputSize) - 0}, (_, __i) => __i + 0).map((j: any) => (() => {
  return Array.from({length: n - 0}, (_, __i) => __i + 0).map((b: any) => (() => {
  return s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, c]: any) => (() => {
  return (() => { if ((fieldBitand(fieldShr(a, j), 1) === b)) {
  return structuredClone(c);
} else {
  return undefined;
} })();
})()).filter((__x: any) => __x !== undefined).reduce((a: any, b: any) => (() => {
  return a.map((__a: any, __i: number) => [__a, Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((i: any) => asRefU8(b)[i])[__i]] as [typeof __a, any]).map(([a, b]: any) => fieldBitxor(a, b));
})(), Array.from({length: ctx.B_OutputSize - 0}, (_, __i) => __i + 0).map((_: any) => 0));
})());
})()) });
})());
}

