// @ts-nocheck
// Auto-generated TypeScript from volar-spec
// Type-level lengths have been converted to runtime number witnesses

import {
  type Cloneable,
  type FieldElement,
  type BlockEncrypt,
  type Digest,
  type LengthDoubler,
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
  wrappingAdd,
  wrappingSub,
  wrappingNeg,
  __chunks,
  asRefU8,
  u32_from_le_bytes,
  u64_from_le_bytes,
  u128_from_le_bytes,
} from "./index";

class __StubDigest { readonly outputSize = 32; update(_data: Uint8Array | readonly number[]): void {} finalize(): Uint8Array { return new Uint8Array(this.outputSize); } }
class Shake128 extends __StubDigest {} class Shake256 extends __StubDigest {} class Sha3_256 extends __StubDigest {}
type DigestUpdate = any;
declare const aes128_encrypt: typeof encrypt_block;

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
function __zeroValue<T>(val: T): T {
  if (typeof val === 'bigint') return 0n as any;
  if (Array.isArray(val)) return [] as any;
  if (val !== null && typeof val === 'object' && typeof (val as any).__zero === 'function') return (val as any).__zero();
  return val;
}
function __take<T>(val: T, setter: (v: T) => void): T { setter(__zeroValue(val)); return val; }
function __equals(a: any, b: any): boolean { return fieldEq(a, b); }

export type WireId = bigint;
export type RgswId = bigint;
export type CellId = bigint;
export type LutId = bigint;
export type DigestImpl = Sha3_256;
export type Base = LweBaseOt<LWE_N>;
export type Zq = bigint;
export type Block = bigint[];

export class U256 {
  [0]!: bigint[];

  constructor(_0: bigint[]) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  bit(n: bigint): boolean
  {
    const word = Number((n / 64n));
    const bit = (n % 64n);
    return (() => { if ((word < 4n)) {
  return !__equals(fieldBitand(fieldShr(this[0][Number(word)], bit), 1n), 0n);
} else {
  return false;
} })();
  }

  high_bit(): boolean
  {
    return !__equals(fieldShr(this[0][Number(3n)], 63n), 0n);
  }

  is_zero(): boolean
  {
    return (((__equals(this[0][Number(0n)], 0n) && __equals(this[0][Number(1n)], 0n)) && __equals(this[0][Number(2n)], 0n)) && __equals(this[0][Number(3n)], 0n));
  }

  shl1(): U256
  {
    let out = Array.from({length: Number(4n)}, () => 0n);
    out[Number(0n)] = fieldShl(this[0][Number(0n)], 1n);
    out[Number(1n)] = fieldBitor(fieldShl(this[0][Number(1n)], 1n), fieldShr(this[0][Number(0n)], 63n));
    out[Number(2n)] = fieldBitor(fieldShl(this[0][Number(2n)], 1n), fieldShr(this[0][Number(1n)], 63n));
    out[Number(3n)] = fieldBitor(fieldShl(this[0][Number(3n)], 1n), fieldShr(this[0][Number(2n)], 63n));
    return new U256(out);
  }

  shr1(): U256
  {
    let out = Array.from({length: Number(4n)}, () => 0n);
    out[Number(0n)] = fieldBitor(fieldShr(this[0][Number(0n)], 1n), fieldShl(this[0][Number(1n)], 63n));
    out[Number(1n)] = fieldBitor(fieldShr(this[0][Number(1n)], 1n), fieldShl(this[0][Number(2n)], 63n));
    out[Number(2n)] = fieldBitor(fieldShr(this[0][Number(2n)], 1n), fieldShl(this[0][Number(3n)], 63n));
    out[Number(3n)] = fieldShr(this[0][Number(3n)], 1n);
    return new U256(out);
  }

  xor(other: any): U256
  {
    return new U256([fieldBitxor(this[0][Number(0n)], other[0][Number(0n)]), fieldBitxor(this[0][Number(1n)], other[0][Number(1n)]), fieldBitxor(this[0][Number(2n)], other[0][Number(2n)]), fieldBitxor(this[0][Number(3n)], other[0][Number(3n)])]);
  }
}

export class Bit {
  [0]!: boolean;

  constructor(_0: boolean) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  bitxor(rhs: bigint): Bit
  {
    return new Bit(fieldBitxor(this[0], !__equals(fieldBitand(rhs, 1n), 0n)));
  }
}

export class Galois {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: Galois): Galois
  {
    return new Galois(fieldBitxor(this[0], rhs[0]));
  }

  bitxor(rhs: bigint): Galois
  {
    return new Galois(fieldBitxor(this[0], rhs));
  }

  invert(): Galois
  {
    return new Galois(gf_invert_u8(this[0], GF8_POLY));
  }

  mul(rhs: Galois): Galois
  {
    return new Galois(gf_mul_u8(this[0], rhs[0], GF8_POLY));
  }

  sub(rhs: Galois): Galois
  {
    return new Galois(fieldBitxor(this[0], rhs[0]));
  }
}

export class BitsInBytes {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: BitsInBytes): BitsInBytes
  {
    return new BitsInBytes(fieldBitxor(this[0], rhs[0]));
  }

  bitxor(rhs: bigint): BitsInBytes
  {
    return new BitsInBytes(fieldBitxor(this[0], rhs));
  }

  mul(rhs: BitsInBytes): BitsInBytes
  {
    return new BitsInBytes(fieldBitand(this[0], rhs[0]));
  }

  sub(rhs: BitsInBytes): BitsInBytes
  {
    return new BitsInBytes(fieldBitxor(this[0], rhs[0]));
  }
}

export class Galois64 {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: Galois64): Galois64
  {
    return new Galois64(fieldBitxor(this[0], rhs[0]));
  }

  bitxor(rhs: bigint): Galois64
  {
    return new Galois64(fieldBitxor(this[0], fieldMul(BigInt(rhs), 72340172838076673n)));
  }

  invert(): Galois64
  {
    return new Galois64(gf_invert_u64(this[0], GF64_POLY));
  }

  mul(rhs: Galois64): Galois64
  {
    return new Galois64(gf_mul_u64(this[0], rhs[0], GF64_POLY));
  }

  sub(rhs: Galois64): Galois64
  {
    return new Galois64(fieldBitxor(this[0], rhs[0]));
  }
}

export class BitsInBytes64 {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: BitsInBytes64): BitsInBytes64
  {
    return new BitsInBytes64(fieldBitxor(this[0], rhs[0]));
  }

  bitxor(rhs: bigint): BitsInBytes64
  {
    return new BitsInBytes64(fieldBitxor(this[0], fieldMul(BigInt(rhs), 72340172838076673n)));
  }

  mul(rhs: BitsInBytes64): BitsInBytes64
  {
    return new BitsInBytes64(fieldBitand(this[0], rhs[0]));
  }

  sub(rhs: BitsInBytes64): BitsInBytes64
  {
    return new BitsInBytes64(fieldBitxor(this[0], rhs[0]));
  }
}

export class Galois128 {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: Galois128): Galois128
  {
    return new Galois128(fieldBitxor(this[0], rhs[0]));
  }

  bitxor(rhs: bigint): Galois128
  {
    return new Galois128(fieldBitxor(this[0], (rhs as unknown as bigint)));
  }

  invert(): Galois128
  {
    return new Galois128(gf_invert_u128(this[0], GF128_POLY));
  }

  mul(rhs: Galois128): Galois128
  {
    return new Galois128(gf_mul_u128(this[0], rhs[0], GF128_POLY));
  }

  sub(rhs: Galois128): Galois128
  {
    return new Galois128(fieldBitxor(this[0], rhs[0]));
  }
}

export class Galois256 {
  [0]!: U256;

  constructor(_0: U256) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: Galois256): Galois256
  {
    return new Galois256(this[0].xor(rhs[0]));
  }

  invert(): Galois256
  {
    return new Galois256(gf_invert_256(this[0], GF256_POLY));
  }

  mul(rhs: Galois256): Galois256
  {
    return new Galois256(gf_mul_256(this[0], rhs[0], GF256_POLY));
  }

  sub(rhs: Galois256): Galois256
  {
    return new Galois256(this[0].xor(rhs[0]));
  }
}

export class Z3 {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: Z3): Z3
  {
    return new Z3(Z3.add3(this[0], rhs[0]));
  }

  static add3(a: bigint, b: bigint): bigint
  {
    const s = fieldAdd(a, b);
    return (() => { if ((s >= 3n)) {
  return fieldSub(s, 3n);
} else {
  return s;
} })();
  }

  mul(rhs: Z3): Z3
  {
    return new Z3(Z3.mul3(this[0], rhs[0]));
  }

  static mul3(a: bigint, b: bigint): bigint
  {
    const p = fieldMul(a, b);
    return (() => { if ((p >= 3n)) {
  return fieldSub(p, 3n);
} else {
  return p;
} })();
  }

  static neg3(a: bigint): bigint
  {
    return (() => { if (__equals(a, 0n)) {
  return 0n;
} else {
  return fieldSub(3n, a);
} })();
  }

  sub(rhs: Z3): Z3
  {
    return new Z3(Z3.add3(this[0], Z3.neg3(rhs[0])));
  }
}

export class TropicalDyn<T> {
  [0]!: T;

  constructor(_0: T) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: TropicalDyn<T>)
  {
    return new TropicalDyn(BigInt(Math.min(Number(this[0]), Number(rhs[0]))));
  }

  mul(rhs: TropicalDyn<any>)
  {
    return new TropicalDyn(fieldAdd(this[0], rhs[0]));
  }
}

export class ViaDigestPuncturableRandomizerDyn<D> {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static double(ctx: { DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, a: bigint[]): bigint[][]
  {
    const v = ctx.DClass.digest(a);
    return [__clone(v), Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(v[Number(i)], a[Number(i)]))];
  }
}

export class CommitmentCoreDyn<D> {
  [0]!: bigint[];

  constructor(_0: bigint[]) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  as_ref(): bigint[]
  {
    return this[0];
  }

  clone()
  {
    return new CommitmentCoreDyn(__clone(this[0]));
  }

  static default<D>(ctx: { D_OutputSize: bigint }): CommitmentCoreDyn<D>
  {
    return new CommitmentCoreDyn(Array.from({length: Number(ctx.D_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  }

  eq(other: any): boolean
  {
    return __equals(this[0], other[0]);
  }

  validate(ctx: { newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, opened_message: bigint[], opened_rand: bigint[]): boolean
  {
    const recomputed: CommitmentCoreDyn<D> = commit(ctx, opened_message, opened_rand);
    return __equals(recomputed[0], this[0]);
  }
}

export class DeltaDyn<T> {
  $fn!: bigint;
  $fdelta!: T[];

  constructor(init: {
    $fn: bigint,
    $fdelta: T[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fdelta: __zeroValue(this.$fdelta) }) as this;
  }

  clone()
  {
    const n: bigint = this.$fn;
    const { $fdelta: delta } = this;
    return new DeltaDyn({ $fdelta: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(delta[Number(i)])), $fn: 0n });
  }

  eq(other: any): boolean
  {
    const n: bigint = this.$fn;
    const { $fdelta: d1 } = this;
    const { $fdelta: d2 } = other;
    for (let i = 0n; i < n; i += 1n)     {
      if (!__equals(d1[Number(i)], d2[Number(i)]))       {
        return false;
      }
    }
    return true;
  }

  remap(m: bigint, f: (arg: number) => bigint)
  {
    const n: bigint = this.$fn;
    const { $fdelta: delta } = this;
    return new DeltaDyn({ $fdelta: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(delta[Number((f(i) % n))])), $fn: 0n });
  }

  rotate_left(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a) => wrappingSub(a, n_param, 32));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a) => wrappingAdd(a, n_param, 32));
  }

  static_<U, O>(val: any[]): QDyn<any>
  {
    const n: bigint = this.$fn;
    return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(val[Number(i)]), __clone(this.$fdelta[Number(i)]))), $fn: 0n });
  }

  bit(n_param: bigint): DeltaDyn<Bit>
  {
    if (this.$fdelta[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n });
    }
  }

  rotate_left_bits(n_param: bigint)
  {
    if (this.$fdelta[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  const next = __clone(delta[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8n, Number(n_param)))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  const next = __clone(delta[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64n, Number(n_param)))));
})()), $fn: 0n });
    }
  }

  rotate_right_bits(n_param: bigint)
  {
    if (this.$fdelta[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(delta[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(delta[Number(i)]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, Number(n_param))), fieldShr(b, Number(n_param))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(delta[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(delta[Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, Number(n_param))), fieldShr(b, Number(n_param))));
})()), $fn: 0n });
    }
  }
}

export class QDyn<T> {
  $fn!: bigint;
  $fq!: T[];

  constructor(init: {
    $fn: bigint,
    $fq: T[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fq: __zeroValue(this.$fq) }) as this;
  }

  clone()
  {
    const n: bigint = this.$fn;
    const { $fq: q } = this;
    return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(q[Number(i)])), $fn: 0n });
  }

  static default<T>(n: bigint): QDyn<T>
  {
    return new QDyn({ $fq: [] as any[], $fn: 0n });
  }

  eq(other: any): boolean
  {
    const n: bigint = this.$fn;
    const { $fq: q1 } = this;
    const { $fq: q2 } = other;
    for (let i = 0n; i < n; i += 1n)     {
      if (!__equals(q1[Number(i)], q2[Number(i)]))       {
        return false;
      }
    }
    return true;
  }

  remap(m: bigint, f: (arg: number) => bigint)
  {
    const n: bigint = this.$fn;
    const { $fq: q } = this;
    return new QDyn({ $fq: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(q[Number((f(i) % n))])), $fn: 0n });
  }

  rotate_left(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a) => wrappingSub(a, n_param, 32));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a) => wrappingAdd(a, n_param, 32));
  }

  bit(n_param: bigint): QDyn<Bit>
  {
    if (this.$fq[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(q[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(q[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n });
    }
  }

  rotate_left_bits(n_param: bigint)
  {
    if (this.$fq[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(q[Number(i)]);
  const next = __clone(q[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8n, Number(n_param)))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(q[Number(i)]);
  const next = __clone(q[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64n, Number(n_param)))));
})()), $fn: 0n });
    }
  }

  rotate_right_bits(n_param: bigint)
  {
    if (this.$fq[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(q[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(q[Number(i)]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, Number(n_param))), fieldShr(b, Number(n_param))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(q[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(q[Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, Number(n_param))), fieldShr(b, Number(n_param))));
})()), $fn: 0n });
    }
  }
}

export class UniversalHashKey {
  $fr0!: Galois128;
  $fr1!: Galois64;

  constructor(init: {
    $fr0: Galois128,
    $fr1: Galois64
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fr0: __zeroValue(this.$fr0), $fr1: __zeroValue(this.$fr1) }) as this;
  }
}

export class UniversalHashOutput {
  $fh0!: Galois128;
  $fh1!: Galois64;

  constructor(init: {
    $fh0: Galois128,
    $fh1: Galois64
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fh0: __zeroValue(this.$fh0), $fh1: __zeroValue(this.$fh1) }) as this;
  }
}

export class AesCtrLengthDoubler {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static double(a: bigint[]): bigint[][]
  {
    let block0 = Array.from({length: Number(BLOCK)}, () => 0n);
    let block1 = Array.from({length: Number(BLOCK)}, () => 0n);
    block1[Number(0n)] = 1n;
    const key: bigint[] = a;
    const c0 = encrypt_block(key, block0);
    const c1 = encrypt_block(key, block1);
    (block0).fill(0n);
    (block1).fill(0n);
    return [c0, c1];
  }
}

export class RoLeafCommitDyn<D> {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static commit(ctx: { newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, sd_bytes: bigint, com_bytes: bigint, r: bigint[], iv: bigint[], tweak: bigint): [bigint[], bigint[]]
  {
    const out_size = ctx.DClass.output_size();
    let h = ctx.newD();
    h.update(r);
    h.update(iv);
    h.update([(tweak) & 0xFFn, ((tweak) >> 8n) & 0xFFn, ((tweak) >> 16n) & 0xFFn, ((tweak) >> 24n) & 0xFFn]);
    const digest = [...h.finalize()];
    let sd = Array.from({length: Number(sd_bytes)}, () => 0n);
    let com = Array.from({length: Number(com_bytes)}, () => 0n);
    (sd).splice(0, (digest.slice(0, Number(sd_bytes))).length, ...(digest.slice(0, Number(sd_bytes))));
    (com).splice(0, (digest.slice(Number(sd_bytes), Number(fieldAdd(sd_bytes, com_bytes)))).length, ...(digest.slice(Number(sd_bytes), Number(fieldAdd(sd_bytes, com_bytes)))));
    return [sd, com];
  }
}

export class EmLeafCommit {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static commit(sd_bytes: bigint, com_bytes: bigint, r: bigint[], iv: bigint[], tweak: bigint): [bigint[], bigint[]]
  {
    let seed_buf = Array.from({length: Number(16n)}, () => 0n);
    (seed_buf).splice(0, (r.slice(0)).length, ...(r.slice(0)));
    const com_vec = aes_ctr_prg(seed_buf, iv, tweak, com_bytes);
    let sd = Array.from({length: Number(sd_bytes)}, () => 0n);
    (sd).splice(0, (r).length, ...(r));
    let com = Array.from({length: Number(com_bytes)}, () => 0n);
    (com).splice(0, (com_vec.slice(0, Number(com_bytes))).length, ...(com_vec.slice(0, Number(com_bytes))));
    return [sd, com];
  }
}

export class FaestTranscript {
  $fsponge!: Sponge;

  constructor(init: {
    $fsponge: Sponge
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsponge: __zeroValue(this.$fsponge) }) as this;
  }

  absorb(data: bigint[])
  {
    this.$fsponge.absorb(data);
  }

  static new_shake128(): FaestTranscript
  {
    return new FaestTranscript({ $fsponge: new Sponge_Shake128(undefined as any /* Shake128::default() */) });
  }

  static new_shake256(): FaestTranscript
  {
    return new FaestTranscript({ $fsponge: new Sponge_Shake256(undefined as any /* Shake256::default() */) });
  }

  squeeze(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n: bigint): Vec<bigint>
  {
    return this.$fsponge.squeeze(n);
  }
}

export class ConvertOutput {
  $fu!: Vec<bigint>;
  $fv!: Vec<Vec<bigint>>;

  constructor(init: {
    $fu: Vec<bigint>,
    $fv: Vec<Vec<bigint>>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu: __zeroValue(this.$fu), $fv: __zeroValue(this.$fv) }) as this;
  }
}

export class BigVoleProver {
  $fu!: Vec<bigint>;
  $fc!: Vec<Vec<bigint>>;
  $fv_columns!: Vec<Vec<bigint>>;

  constructor(init: {
    $fu: Vec<bigint>,
    $fc: Vec<Vec<bigint>>,
    $fv_columns: Vec<Vec<bigint>>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu: __zeroValue(this.$fu), $fc: __zeroValue(this.$fc), $fv_columns: __zeroValue(this.$fv_columns) }) as this;
  }
}

export class BigVoleVerifier {
  $fq_columns!: Vec<Vec<bigint>>;

  constructor(init: {
    $fq_columns: Vec<Vec<bigint>>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fq_columns: __zeroValue(this.$fq_columns) }) as this;
  }
}

export class BavcCommitmentDyn {
  $fcom_bytes!: bigint;
  $froot!: Vec<bigint>;
  $fvec_hashes!: Vec<Vec<bigint>>;
  $fseeds!: Vec<bigint[]>;
  $fcommitments!: Vec<bigint[]>;

  constructor(init: {
    $fcom_bytes: bigint,
    $froot: Vec<bigint>,
    $fvec_hashes: Vec<Vec<bigint>>,
    $fseeds: Vec<bigint[]>,
    $fcommitments: Vec<bigint[]>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fcom_bytes: __zeroValue(this.$fcom_bytes), $froot: __zeroValue(this.$froot), $fvec_hashes: __zeroValue(this.$fvec_hashes), $fseeds: __zeroValue(this.$fseeds), $fcommitments: __zeroValue(this.$fcommitments) }) as this;
  }
}

export class BavcOpeningDyn {
  $fcom_bytes!: bigint;
  $fhidden_commits!: Vec<bigint[]>;
  $fnodes!: Vec<[bigint, bigint[]]>;

  constructor(init: {
    $fcom_bytes: bigint,
    $fhidden_commits: Vec<bigint[]>,
    $fnodes: Vec<[bigint, bigint[]]>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fcom_bytes: __zeroValue(this.$fcom_bytes), $fhidden_commits: __zeroValue(this.$fhidden_commits), $fnodes: __zeroValue(this.$fnodes) }) as this;
  }
}

export class BavcDyn<L> {
  $fcom_bytes!: bigint;

  constructor(init: {
    $fcom_bytes: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fcom_bytes: __zeroValue(this.$fcom_bytes) }) as this;
  }

  static collect_open_nodes(com_bytes: bigint, deltas: bigint[], tree: bigint[][], tau: bigint, n: bigint): Vec<[bigint, bigint[]]>
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let hidden = [] as any[];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    for (const node of (Array.from({length: Number(fieldSub(leaf_count, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      hidden[Number(node)] = (hidden[Number(fieldAdd(fieldMul(2n, node), 1n))] || hidden[Number(fieldAdd(fieldMul(2n, node), 2n))]);
    }
    let out: Vec<[bigint, bigint[]]> = [] as any[];
    walk(0n, hidden, tree, leaf_count, out);
    return out;
  }

  static commit(ctx: { newD: () => any, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, com_bytes: bigint, r: bigint[], iv: bigint[], tau: bigint, n: bigint): BavcCommitmentDyn
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let tree: Vec<bigint[]> = [] as any[];
    tree[Number(0n)] = r;
    for (let node = 0n; node < fieldSub(leaf_count, 1n); node += 1n)     {
      const parent = tree[Number(node)];
      const [left, right] = AesCtrLengthDoubler.double(parent);
      tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left[0];
      tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right[0];
    }
    let seeds = ([] as any[]);
    let commitments = ([] as any[]);
    for (let i = 0n; i < tau; i += 1n)     {
      for (let j = 0n; j < n; j += 1n)       {
        const leaf_k = fieldAdd(fieldMul(i, n), j);
        const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
        const r_leaf = tree[Number(tree_pos)];
        const tweak = Number(leaf_k);
        const [sd, com] = ctx.LClass.commit(r_leaf, iv, tweak);
        (seeds).push(sd);
        (commitments).push(com);
      }
    }
    let vec_hashes: Vec<Vec<bigint>> = ([] as any[]);
    for (let i = 0n; i < tau; i += 1n)     {
      let h = ctx.newD();
      for (let j = 0n; j < n; j += 1n)       {
        h.update(commitments[Number(fieldAdd(fieldMul(i, n), j))]);
      }
      (vec_hashes).push([...[...h.finalize()]]);
    }
    let h = ctx.newD();
    for (const hi of vec_hashes)     {
      h.update(hi);
    }
    const root = [...[...h.finalize()]];
    return new BavcCommitmentDyn({ $froot: root, $fvec_hashes: vec_hashes, $fseeds: seeds, $fcommitments: commitments, $fcom_bytes: 0n });
  }

  static open(com_bytes: bigint, commitment: BavcCommitmentDyn, deltas: bigint[], tau: bigint, n: bigint): BavcOpeningDyn
  {
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
    }
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let hidden = [] as any[];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    for (const node of (Array.from({length: Number(fieldSub(leaf_count, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      const left = fieldAdd(fieldMul(2n, node), 1n);
      const right = fieldAdd(fieldMul(2n, node), 2n);
      hidden[Number(node)] = (hidden[Number(left)] || hidden[Number(right)]);
    }
    let tree: Vec<bigint[]> = [] as any[];
    const _ = tree;
    return new BavcOpeningDyn({ $fhidden_commits: deltas.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([i, d]: any) => commitment.$fcommitments[Number(fieldAdd(fieldMul(i, n), d))]), $fnodes: [] as any[], $fcom_bytes: 0n });
  }

  static reconstruct(ctx: { newD: () => any, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, com_bytes: bigint, nodes: [bigint, bigint[]][], hidden_commits: bigint[][], deltas: bigint[], iv: bigint[], expected_root: bigint[], tau: bigint, n: bigint): (Vec<bigint[]> | undefined)
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let hidden = [] as any[];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    for (const node of (Array.from({length: Number(fieldSub(leaf_count, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      hidden[Number(node)] = (hidden[Number(fieldAdd(fieldMul(2n, node), 1n))] || hidden[Number(fieldAdd(fieldMul(2n, node), 2n))]);
    }
    let tree: Vec<(bigint[] | undefined)> = [] as any[];
    for (const [idx, seed] of nodes)     {
      tree[Number(idx)] = seed;
    }
    for (let node = 0n; node < fieldSub(leaf_count, 1n); node += 1n)     {
      return (() => { const __match = tree[Number(node)]; if (__match !== null && __match !== undefined) { const parent_seed = __match;
return (() => {
  const parent = parent_seed;
  const [left, right] = AesCtrLengthDoubler.double(parent);
  if ((tree[Number(fieldAdd(fieldMul(2n, node), 1n))]) == null)   {
    tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left[0];
  }
  if ((tree[Number(fieldAdd(fieldMul(2n, node), 2n))]) == null)   {
    tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right[0];
  }
})(); } else { return (() => {
})(); } })();
    }
    let leaf_seeds: Vec<bigint[]> = ([] as any[]);
    let leaf_coms: Vec<bigint[]> = ([] as any[]);
    for (let i = 0n; i < tau; i += 1n)     {
      for (let j = 0n; j < n; j += 1n)       {
        const leaf_k = fieldAdd(fieldMul(i, n), j);
        const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
        if (__equals(j, deltas[Number(i)]))         {
          (leaf_seeds).push(Array.from({length: Number(LAMBDA_BYTES)}, () => 0n));
          (leaf_coms).push(hidden_commits[Number(i)]);
        } else         {
          const r_leaf = tree[Number(tree_pos)];
          const tweak = Number(leaf_k);
          const [sd, com] = ctx.LClass.commit(r_leaf, iv, tweak);
          (leaf_seeds).push(sd);
          (leaf_coms).push(com);
        }
      }
    }
    let vec_hashes: Vec<Vec<bigint>> = ([] as any[]);
    for (let i = 0n; i < tau; i += 1n)     {
      let h = ctx.newD();
      for (let j = 0n; j < n; j += 1n)       {
        h.update(leaf_coms[Number(fieldAdd(fieldMul(i, n), j))]);
      }
      (vec_hashes).push([...[...h.finalize()]]);
    }
    let h = ctx.newD();
    for (const hi of vec_hashes)     {
      h.update(hi);
    }
    const root = [...[...h.finalize()]];
    return (() => { if (__equals(root, expected_root)) {
  return leaf_seeds;
} else {
  return undefined;
} })();
  }
}

export class FaestSecretKey {
  [0]!: bigint[];

  constructor(_0: bigint[]) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }
}

export class FaestPublicKey {
  [0]!: bigint[];

  constructor(_0: bigint[]) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }
}

export class FaestSignature {
  $fiv!: bigint[];
  $fbavc_root!: Vec<bigint>;
  $fhidden_commits!: Vec<bigint[]>;
  $fnodes!: Vec<[bigint, bigint[]]>;
  $fcorrections!: Vec<Vec<bigint>>;
  $fvole_u!: Vec<bigint>;
  $fqs_proof!: QuickSilverProof;
  $fc_hat_with_counter!: Vec<bigint>;
  $fchall_3!: Vec<bigint>;
  $fcounter!: bigint;

  constructor(init: {
    $fiv: bigint[],
    $fbavc_root: Vec<bigint>,
    $fhidden_commits: Vec<bigint[]>,
    $fnodes: Vec<[bigint, bigint[]]>,
    $fcorrections: Vec<Vec<bigint>>,
    $fvole_u: Vec<bigint>,
    $fqs_proof: QuickSilverProof,
    $fc_hat_with_counter: Vec<bigint>,
    $fchall_3: Vec<bigint>,
    $fcounter: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fiv: __zeroValue(this.$fiv), $fbavc_root: __zeroValue(this.$fbavc_root), $fhidden_commits: __zeroValue(this.$fhidden_commits), $fnodes: __zeroValue(this.$fnodes), $fcorrections: __zeroValue(this.$fcorrections), $fvole_u: __zeroValue(this.$fvole_u), $fqs_proof: __zeroValue(this.$fqs_proof), $fc_hat_with_counter: __zeroValue(this.$fc_hat_with_counter), $fchall_3: __zeroValue(this.$fchall_3), $fcounter: __zeroValue(this.$fcounter) }) as this;
  }
}

export class QuickSilverProof {
  $fa_hat!: Vec<bigint>;
  $fb_hat!: Vec<bigint>;
  $fc_hat_base!: Vec<bigint>;

  constructor(init: {
    $fa_hat: Vec<bigint>,
    $fb_hat: Vec<bigint>,
    $fc_hat_base: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fa_hat: __zeroValue(this.$fa_hat), $fb_hat: __zeroValue(this.$fb_hat), $fc_hat_base: __zeroValue(this.$fc_hat_base) }) as this;
  }
}

export class StubFaestAesProver {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  prove_aes_witness(big_vole: any, hash_key: any): QuickSilverProof
  {
    const a_hat_out: UniversalHashOutput = vole_hash(hash_key, big_vole.$fu);
    const a_hat: Vec<bigint> = [(a_hat_out.$fh0[0]) & 0xFFn, ((a_hat_out.$fh0[0]) >> 8n) & 0xFFn, ((a_hat_out.$fh0[0]) >> 16n) & 0xFFn, ((a_hat_out.$fh0[0]) >> 24n) & 0xFFn].concat([(a_hat_out.$fh1[0]) & 0xFFn, ((a_hat_out.$fh1[0]) >> 8n) & 0xFFn, ((a_hat_out.$fh1[0]) >> 16n) & 0xFFn, ((a_hat_out.$fh1[0]) >> 24n) & 0xFFn]);
    const len = BigInt(a_hat.length);
    return new QuickSilverProof({ $fa_hat: a_hat, $fb_hat: [] as any[], $fc_hat_base: [] as any[] });
  }
}

export class ABODyn<B, D> {
  $fk!: bigint;
  $fn!: bigint;
  $fcommit!: bigint[];
  $fper_byte!: bigint[][][];

  constructor(init: {
    $fk: bigint,
    $fn: bigint,
    $fcommit: bigint[],
    $fper_byte: bigint[][][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fk: __zeroValue(this.$fk), $fn: __zeroValue(this.$fn), $fcommit: __zeroValue(this.$fcommit), $fper_byte: __zeroValue(this.$fper_byte) }) as this;
  }

  open(ctx: { newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, t: bigint, u: bigint, m: bigint, bad: bigint[], rand: bigint[])
  {
    const k: bigint = this.$fk;
    const n: bigint = this.$fn;
    return new ABOOpeningDyn({ $fbad: __clone(bad), $fopenings: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((ni: any) => (() => {
  const bad_1 = __clone(bad);
  return Array.from({length: Number(t - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const bad_2 = __clone(bad_1);
  return Array.from({length: Number(u - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const i2 = fieldBitor(i, fieldShl(Number(j), ilog2(t)));
  return (() => { if (bad_2.includes(BigInt(i2))) {
  const h = commit(ctx, this.$fper_byte[Number(ni)][Number(i2)], rand);
  return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return (asRefU8(h)?.[j] ?? 0);
})());
} else {
  return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return (this.$fper_byte[Number(ni)][Number(i2)]?.[j] ?? 0);
})());
} })();
})());
})());
})()), $ft: 0n, $fu: 0n, $fn: 0n });
  }

  split_bit_typenum(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, target: bigint)
  {
    const k: bigint = this.$fk;
    const n: bigint = this.$fn;
    return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fper_byte[Number(ctx.NClass.party_index(target))].slice(Number(fieldMul(i, m))).slice(0, Number(m));
  return new BSplitDyn({ $fsplit: Array.from({length: Number(ilog2(ctx.D_OutputSize) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((b: any) => (() => {
  return s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, c]: any) => (() => {
  return (() => { if (__equals(fieldBitand(fieldShr(a, j), 1n), b)) {
  return __clone(c);
} else {
  return undefined;
} })();
})()).filter((__x: any) => __x !== undefined).reduce((a: any, b: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a[Number(i)], b[Number(i)]));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
})());
})()) });
})());
  }

  to_vole_material(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, target: bigint): VopeDyn<bigint>[]
  {
    const k: bigint = this.$fk;
    const n: bigint = this.$fn;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fper_byte[Number(ctx.NClass.party_index(target))].slice(Number(fieldMul(i, m))).slice(0, Number(m));
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_expanded(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, target: bigint, f: (arg: Uint8Array) => any): VopeDyn<bigint>[]
  {
    const k: bigint = this.$fk;
    const n: bigint = this.$fn;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fper_byte[Number(ctx.NClass.party_index(target))].slice(Number(fieldMul(i, m))).slice(0, Number(m));
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  to_vole_material_typenum(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, target: bigint): VopeDyn<bigint>[]
  {
    const k: bigint = this.$fk;
    const n: bigint = this.$fn;
    return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fper_byte[Number(ctx.NClass.party_index(target))].slice(Number(fieldMul(i, m))).slice(0, Number(m));
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_typenum_expanded(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, target: bigint, f: (arg: Uint8Array) => any): VopeDyn<bigint>[]
  {
    const k: bigint = this.$fk;
    const n: bigint = this.$fn;
    return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fper_byte[Number(ctx.NClass.party_index(target))].slice(Number(fieldMul(i, m))).slice(0, Number(m));
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }
}

export class ABOOpeningDyn<B, D> {
  $ft!: bigint;
  $fu!: bigint;
  $fn!: bigint;
  $fbad!: bigint[];
  $fopenings!: bigint[][][][];

  constructor(init: {
    $ft: bigint,
    $fu: bigint,
    $fn: bigint,
    $fbad: bigint[],
    $fopenings: bigint[][][][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $ft: __zeroValue(this.$ft), $fu: __zeroValue(this.$fu), $fn: __zeroValue(this.$fn), $fbad: __zeroValue(this.$fbad), $fopenings: __zeroValue(this.$fopenings) }) as this;
  }

  split_bit_typenum(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, party: bigint)
  {
    const t: bigint = this.$ft;
    const u: bigint = this.$fu;
    const n: bigint = this.$fn;
    return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fopenings[Number(ctx.NClass.party_index(party))][Number(i)];
  return new BSplitDyn({ $fsplit: Array.from({length: Number(ilog2(ctx.D_OutputSize) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((b: any) => (() => {
  return s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, c]: any) => (() => {
  return (() => { if (__equals(fieldBitand(fieldShr(a, j), 1n), b)) {
  return __clone(c);
} else {
  return undefined;
} })();
})()).filter((__x: any) => __x !== undefined).reduce((a: any, b: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a[Number(i)], b[Number(i)]));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
})());
})()) });
})());
  }

  to_vole_material(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, party: bigint): VopeDyn<bigint>[]
  {
    const t: bigint = this.$ft;
    const u: bigint = this.$fu;
    const n: bigint = this.$fn;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fopenings[Number(ctx.NClass.party_index(party))][Number(i)];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_expanded(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, party: bigint, f: (arg: Uint8Array) => any): VopeDyn<bigint>[]
  {
    const t: bigint = this.$ft;
    const u: bigint = this.$fu;
    const n: bigint = this.$fn;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fopenings[Number(ctx.NClass.party_index(party))][Number(i)];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  to_vole_material_typenum(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, party: bigint): VopeDyn<bigint>[]
  {
    const t: bigint = this.$ft;
    const u: bigint = this.$fu;
    const n: bigint = this.$fn;
    return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fopenings[Number(ctx.NClass.party_index(party))][Number(i)];
  return create_vole_from_material(ctx, s);
})());
  }

  to_vole_material_typenum_expanded(ctx: { B_OutputSize: bigint, NClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, party: bigint, f: (arg: Uint8Array) => any): VopeDyn<bigint>[]
  {
    const t: bigint = this.$ft;
    const u: bigint = this.$fu;
    const n: bigint = this.$fn;
    return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s = this.$fopenings[Number(ctx.NClass.party_index(party))][Number(i)];
  return create_vole_from_material_expanded(ctx, s, f);
})());
  }

  validate(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, commit_: bigint[], rand: bigint[]): boolean
  {
    const t: bigint = this.$ft;
    const u: bigint = this.$fu;
    const n: bigint = this.$fn;
    let h = ctx.newD();
    for (let i = 0n; i < t; i += 1n)     {
      for (let b = 0n; b < u; b += 1n)       {
        const i2 = fieldBitor(i, fieldShl(Number(b), ilog2(t)));
        if (this.$fbad.includes(BigInt(i2)))         {
          h.update(this.$fopenings[Number(0n)][Number(i)][Number(b)].slice(0, Number(ctx.D_OutputSize)));
        } else         {
          h.update(commit(ctx, this.$fopenings[Number(0n)][Number(i)][Number(b)].slice(0, Number(ctx.B_OutputSize)), rand));
        }
      }
    }
    return __equals([...h.finalize()], commit_);
  }
}

export class BSplitDyn<B, D> {
  $fsplit!: bigint[][][];

  constructor(init: {
    $fsplit: bigint[][][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsplit: __zeroValue(this.$fsplit) }) as this;
  }
}

export class PolyDyn<T> {
  $fn!: bigint;
  $fc0!: T;
  $fc1!: T[];

  constructor(init: {
    $fn: bigint,
    $fc0: T,
    $fc1: T[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fc0: __zeroValue(this.$fc0), $fc1: __zeroValue(this.$fc1) }) as this;
  }

  apply<O>(ctx: { defaultO: () => any }, m: bigint, x: bigint, x2: bigint, xs: bigint, s: bigint, voles: VopeDyn<T>[][]): VopeDyn<any>
  {
    const n: bigint = this.$fn;
    const v = Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0n; k < n; k += 1n)   {
    let b: any = __clone(this.$fc1[Number(k)]);
    for (const v of voles)     {
      b = fieldMul(b, __clone(v[Number(k)].$fv[Number(i)]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = __clone(this.$fc0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: Number(xs - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0n; k < n; k += 1n)   {
    for (let n = 0n; n < x; n += 1n)     {
      let b: any = __clone(this.$fc1[Number(k)]);
      for (let m = 0n; m < s; m += 1n)       {
        const l_1 = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if (__equals(idx, n)) {
  return __clone(v[Number(k)].$fu[Number(l_1)][Number(i)]);
} else {
  return __clone(v[Number(k)].$fv[Number(i)]);
} })());
        }
      }
      sum = fieldAdd(sum, b);
    }
  }
  return sum;
})());
})());
    return new VopeDyn({ $fu: u, $fv: v, $fn: 0n, $fk: 1n });
  }

  apply_pool<O>(ctx: { defaultO: () => any }, m: bigint, x: bigint, x2: bigint, xs: bigint, s: bigint, voles: PolyInputPoolDyn<VopeDyn<T>>): VopeDyn<any>
  {
    const n: bigint = this.$fn;
    const v = Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0n; k < n; k += 1n)   {
    let b: any = __clone(this.$fc1[Number(k)]);
    for (const v of voles.$findices)     {
      b = fieldMul(b, __clone(voles.$finputs[Number(v[Number(k)])].$fv[Number(i)]));
    }
    sum = fieldAdd(sum, b);
  }
  const c0: any = __clone(this.$fc0);
  return fieldAdd(sum, c0);
})());
    const u = Array.from({length: Number(xs - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let sum = ctx.defaultO();
  for (let k = 0n; k < n; k += 1n)   {
    for (let n = 0n; n < x; n += 1n)     {
      let b: any = __clone(this.$fc1[Number(k)]);
      for (let m = 0n; m < s; m += 1n)       {
        const l_1 = fieldAdd(fieldMul(l, s), m);
        for (const [idx, v] of voles.$findices.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          b = fieldMul(b, (() => { if (__equals(idx, n)) {
  return __clone(voles.$finputs[Number(v[Number(k)])].$fu[Number(l_1)][Number(i)]);
} else {
  return __clone(voles.$finputs[Number(v[Number(k)])].$fv[Number(i)]);
} })());
        }
      }
      sum = fieldAdd(sum, b);
    }
  }
  return sum;
})());
})());
    return new VopeDyn({ $fu: u, $fv: v, $fn: 0n, $fk: 1n });
  }

  get_qs<Q, A>(m: bigint, x: bigint, root: DeltaDyn<any>, inputs: QDyn<any>[][], reduction: bigint): QDyn<any>
  {
    const n: bigint = this.$fn;
    return new Q({ $fq: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let sum: any = __clone(this.$fc0);
  for (let _ = 0n; _ < n; _ += 1n)   {
    sum = fieldMul(__clone(root.$fdelta[Number(i)]), sum);
  }
  for (let j = 0n; j < n; j += 1n)   {
    let b: any = __clone(this.$fc1[Number(j)]);
    for (const i2 of inputs)     {
      for (let _ = 0n; _ < reduction; _ += 1n)       {
        b = fieldMul(__clone(i2[Number(j)].$fq[Number(i)]), b);
      }
    }
    sum = fieldAdd(sum, b);
  }
  return sum;
})()) });
  }

  get_qs_pool<Q, A>(m: bigint, x: bigint, root: DeltaDyn<any>, inputs: PolyInputPoolDyn<QDyn<any>>, reduction: bigint): QDyn<any>
  {
    const n: bigint = this.$fn;
    return new Q({ $fq: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let sum: any = __clone(this.$fc0);
  for (let _ = 0n; _ < n; _ += 1n)   {
    sum = fieldMul(__clone(root.$fdelta[Number(i)]), sum);
  }
  for (let j = 0n; j < n; j += 1n)   {
    let b: any = __clone(this.$fc1[Number(j)]);
    for (const i2 of inputs.$findices)     {
      for (let _ = 0n; _ < reduction; _ += 1n)       {
        b = fieldMul(__clone(inputs.$finputs[Number(i2[Number(j)])].$fq[Number(i)]), b);
      }
    }
    sum = fieldAdd(sum, b);
  }
  return sum;
})()) });
  }
}

export class PolyInputPoolDyn<T> {
  $fn!: bigint;
  $fx!: bigint;
  $finputs!: T[];
  $findices!: bigint[][];

  constructor(init: {
    $fn: bigint,
    $fx: bigint,
    $finputs: T[],
    $findices: bigint[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fx: __zeroValue(this.$fx), $finputs: __zeroValue(this.$finputs), $findices: __zeroValue(this.$findices) }) as this;
  }
}

export class AdditiveHasher {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static absorb<T>(state: any, encoded: any)
  {
    const old = __take(state, y => state = y);
    state = fieldAdd(old, encoded);
  }

  static finalize_eq<T>(produce: any, consume: any): boolean
  {
    return __equals(produce, consume);
  }

  static new_state<T>(ctx: { defaultT: () => any }): T
  {
    return ctx.defaultT();
  }
}

export class ChallengeKeyDyn<T> {
  $fr1!: T;
  $fr2!: T;
  $fr3!: T;

  constructor(init: {
    $fr1: T,
    $fr2: T,
    $fr3: T
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fr1: __zeroValue(this.$fr1), $fr2: __zeroValue(this.$fr2), $fr3: __zeroValue(this.$fr3) }) as this;
  }

  static from_challenge<T>(r: any): ChallengeKeyDyn<T>
  {
    const r2 = fieldMul(__clone(r), __clone(r));
    const r3 = fieldMul(__clone(r2), __clone(r));
    return new ChallengeKeyDyn({ $fr1: r, $fr2: r2, $fr3: r3 });
  }
}

export class MemoryCheckStateDyn<T, H> {
  $fkey!: ChallengeKeyDyn<T>;
  $fproduce!: number /* H::State */;
  $fconsume!: number /* H::State */;

  constructor(init: {
    $fkey: ChallengeKeyDyn<T>,
    $fproduce: number /* H::State */,
    $fconsume: number /* H::State */
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fkey: __zeroValue(this.$fkey), $fproduce: __zeroValue(this.$fproduce), $fconsume: __zeroValue(this.$fconsume) }) as this;
  }

  consume()
  {
    return this.$fconsume;
  }

  drain(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, final_value: any, final_timestamp: bigint)
  {
    const enc = this.encode(ctx, addr, final_value, final_timestamp);
    ctx.HClass.absorb(this.$fconsume, enc);
  }

  encode(ctx: { defaultT: () => any }, addr: any, value: any, timestamp: bigint)
  {
    const a = fieldMul(addr, __clone(this.$fkey.$fr1));
    const v = fieldMul(value, __clone(this.$fkey.$fr2));
    const t = this.scale_by_u64(ctx, __clone(this.$fkey.$fr3), timestamp);
    return fieldAdd(fieldAdd(a, v), t);
  }

  init(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, zero_value: any)
  {
    const enc = this.encode(ctx, addr, zero_value, 0n);
    ctx.HClass.absorb(this.$fproduce, enc);
  }

  static new<T>(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, key: ChallengeKeyDyn<T>): MemoryCheckStateDyn<T, H>
  {
    return new MemoryCheckStateDyn({ $fkey: key, $fproduce: ctx.HClass.new_state(), $fconsume: ctx.HClass.new_state() });
  }

  produce()
  {
    return this.$fproduce;
  }

  read(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, value: any, timestamp: bigint, write_timestamp: bigint)
  {
    const enc_produce = this.encode(ctx, __clone(addr), __clone(value), timestamp);
    ctx.HClass.absorb(this.$fproduce, enc_produce);
    const enc_consume = this.encode(ctx, addr, value, write_timestamp);
    ctx.HClass.absorb(this.$fconsume, enc_consume);
  }

  scale_by_u64(ctx: { defaultT: () => any }, x: any, n: bigint)
  {
    if (__equals(n, 0n))     {
      return ctx.defaultT();
    }
    if (__equals(n, 1n))     {
      return x;
    }
    let acc = ctx.defaultT();
    let base = x;
    let remaining = n;
    while (!__equals(remaining, 0n))     {
      if (__equals(fieldBitand(remaining, 1n), 1n))       {
        acc = fieldAdd(acc, __clone(base));
      }
      remaining = fieldShr(remaining, 1n);
      if (!__equals(remaining, 0n))       {
        base = fieldAdd(__clone(base), base);
      }
    }
    return acc;
  }

  verify(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }): boolean
  {
    return ctx.HClass.finalize_eq(this.$fproduce, this.$fconsume);
  }

  write(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, new_value: any, timestamp: bigint, old_value: any, old_timestamp: bigint)
  {
    const enc_new = this.encode(ctx, __clone(addr), new_value, timestamp);
    ctx.HClass.absorb(this.$fproduce, enc_new);
    const enc_old = this.encode(ctx, addr, old_value, old_timestamp);
    ctx.HClass.absorb(this.$fconsume, enc_old);
  }
}

export class BitVoleDyn<T> {
  $fn!: bigint;
  $fu!: Bit[];
  $fv!: T[];

  constructor(init: {
    $fn: bigint,
    $fu: Bit[],
    $fv: T[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fu: __zeroValue(this.$fu), $fv: __zeroValue(this.$fv) }) as this;
  }
}

export class VopeDyn<T> {
  $fn!: bigint;
  $fk!: bigint;
  $fu!: T[][];
  $fv!: T[];

  constructor(init: {
    $fn: bigint,
    $fk: bigint,
    $fu: T[][],
    $fv: T[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fk: __zeroValue(this.$fk), $fu: __zeroValue(this.$fu), $fv: __zeroValue(this.$fv) }) as this;
  }

  add(rhs: VopeDyn<any>)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(this.$fu[Number(l)][Number(i)]), __clone(rhs.$fu[Number(l)][Number(i)])));
})()), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(this.$fv[Number(i)]), __clone(rhs.$fv[Number(i)]))), $fn: 0n, $fk: 1n });
  }

  bitxor(rhs: any[])
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const o: any = fieldBitxor(__clone(this.$fu[Number(i)][Number(j)]), __clone(rhs[Number(fieldAdd(fieldMul(i, k), j))]));
  return o;
})());
})()), $fv: this.$fv.map((a: any) => a), $fn: 0n, $fk: 1n });
  }

  clone()
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    const { $fu: u, $fv: v } = this;
    return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(u[Number(l)][Number(i)]))), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(v[Number(i)])), $fn: 0n, $fk: 1n });
  }

  static constant<T>(n: bigint, v: T[]): VopeDyn<T>
  {
    const k: bigint = 0n;
    return new VopeDyn({ $fu: Array.from({length: Number(0n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => { throw new Error("unreachable"); })()), $fv: v, $fn: 0n, $fk: 1n });
  }

  static default<T>(n: bigint, k: bigint): VopeDyn<T>
  {
    return new VopeDyn({ $fu: [] as any[], $fv: [] as any[], $fn: 0n, $fk: 1n });
  }

  eq(other: any): boolean
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    const { $fu: u1, $fv: v1 } = this;
    const { $fu: u2, $fv: v2 } = other;
    for (let l = 0n; l < k; l += 1n)     {
      for (let i = 0n; i < n; i += 1n)       {
        if (!__equals(u1[Number(l)][Number(i)], u2[Number(l)][Number(i)]))         {
          return false;
        }
      }
    }
    for (let i = 0n; i < n; i += 1n)     {
      if (!__equals(v1[Number(i)], v2[Number(i)]))       {
        return false;
      }
    }
    return true;
  }

  expand(ctx: { defaultT: () => any }, l: bigint)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    const { $fu: u, $fv: v } = this;
    return new VopeDyn({ $fu: Array.from({length: Number(l - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => ((u?.[l]) != null ? ((a) => __clone(a[Number(i)]))(u?.[l]) : (ctx.defaultT())));
})()), $fv: __clone(v), $fn: 0n, $fk: 1n });
  }

  mul(rhs: DeltaDyn<any>)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return new QDyn({ $fq: this.$fu.map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  let x = __clone(rhs.$fdelta[Number(i)]);
  for (let _ = 0n; _ < i; _ += 1n)   {
    x = fieldMul(x, __clone(rhs.$fdelta[Number(i)]));
  }
  const m: any = fieldMul(__clone(b[Number(j)]), x);
  return fieldAdd(m, __clone(a[Number(j)]));
})());
})(), this.$fv.map((a: any) => a)), $fn: 0n });
  }

  mul_generalized(ctx: { defaultT: () => any }, k2: bigint, other: VopeDyn<T>)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    let res_u = Array.from({length: Number(fieldAdd(k2, k) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
    let res_v = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT());
    for (let i = 0n; i <= k; i += 1n)     {
      for (let j = 0n; j <= k2; j += 1n)       {
        const k_1 = fieldAdd(i, j);
        const a_coeff = (() => { if (__equals(i, 0n)) {
  return this.$fv;
} else {
  return this.$fu[Number(fieldSub(i, 1n))];
} })();
        const b_coeff = (() => { if (__equals(j, 0n)) {
  return other.$fv;
} else {
  return other.$fu[Number(fieldSub(j, 1n))];
} })();
        if (__equals(k_1, 0n))         {
          for (let lane = 0n; lane < n; lane += 1n)           {
            res_v[Number(lane)] = fieldAdd(__clone(res_v[Number(lane)]), fieldMul(__clone(a_coeff[Number(lane)]), __clone(b_coeff[Number(lane)])));
          }
        } else         {
          for (let lane = 0n; lane < n; lane += 1n)           {
            res_u[Number(fieldSub(k_1, 1n))][Number(lane)] = fieldAdd(__clone(res_u[Number(fieldSub(k_1, 1n))][Number(lane)]), fieldMul(__clone(a_coeff[Number(lane)]), __clone(b_coeff[Number(lane)])));
          }
        }
      }
    }
    return new VopeDyn({ $fu: res_u, $fv: res_v, $fn: 0n, $fk: 1n });
  }

  remap(m: bigint, f: (arg: number) => bigint)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    const { $fu: u, $fv: v } = this;
    return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(u[Number(l)][Number((f(i) % n))]));
})()), $fv: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(v[Number((f(i) % n))])), $fn: 0n, $fk: 1n });
  }

  rotate_left(n_param: bigint)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return this.remap(this.$fn, (a) => wrappingSub(a, n_param, 32));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return this.remap(this.$fn, (a) => wrappingAdd(a, n_param, 32));
  }

  scale<T>(f: (arg: boolean) => T): VopeDyn<T>
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    const { $fu: u, $fv: v } = this;
    return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(u[Number(l)][Number(i)]);
  return f(b);
})());
})()), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  return f(b);
})()), $fn: 0n, $fk: 1n });
  }

  bit(n_param: bigint): VopeDyn<Bit>
  {
    if (this.$fu[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(u[Number(l)][Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n, $fk: 1n });
    } else {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(u[Number(l)][Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n, $fk: 1n });
    }
  }

  rotate_left_bits(n_param: bigint)
  {
    if (this.$fu[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(u[Number(l)][Number(i)]);
  const next = __clone(u[Number(l)][Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8n, Number(n_param)))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  const next = __clone(v[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(8n, Number(n_param)))));
})()), $fn: 0n, $fk: 1n });
    } else {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(u[Number(l)][Number(i)]);
  const next = __clone(u[Number(l)][Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64n, Number(n_param)))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  const next = __clone(v[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, Number(n_param)), fieldShr(next, fieldSub(64n, Number(n_param)))));
})()), $fn: 0n, $fk: 1n });
    }
  }

  rotate_right_bits(n_param: bigint)
  {
    if (this.$fu[0] instanceof BitsInBytes) {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(u[Number(l)][Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(u[Number(l)][Number(i)]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, Number(n_param))), fieldShr(b, Number(n_param))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(v[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(v[Number(i)]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, Number(n_param))), fieldShr(b, Number(n_param))));
})()), $fn: 0n, $fk: 1n });
    } else {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(u[Number(l)][Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(u[Number(l)][Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, Number(n_param))), fieldShr(b, Number(n_param))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(v[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(v[Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, Number(n_param))), fieldShr(b, Number(n_param))));
})()), $fn: 0n, $fk: 1n });
    }
  }
}

export class BinfheKeySwitchingKeyRefDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fks_ell!: bigint;
  $fksk!: BinfheLweCiphertextDyn[][];

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fks_ell: bigint,
    $fksk: BinfheLweCiphertextDyn[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fks_ell: __zeroValue(this.$fks_ell), $fksk: __zeroValue(this.$fksk) }) as this;
  }

  ksk_rows(): BinfheLweCiphertextDyn[][]
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const ks_ell: bigint = this.$fks_ell;
    return this.$fksk;
  }
}

export class BinfheBootstrappingKeyRefDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $fks_ell!: bigint;
  $fbsk!: BinfheRgswCiphertextDyn[];
  $fksk!: BinfheKeySwitchingKeyRefDyn;

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $fks_ell: bigint,
    $fbsk: BinfheRgswCiphertextDyn[],
    $fksk: BinfheKeySwitchingKeyRefDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $fks_ell: __zeroValue(this.$fks_ell), $fbsk: __zeroValue(this.$fbsk), $fksk: __zeroValue(this.$fksk) }) as this;
  }

  bsk_rows(): BinfheRgswCiphertextDyn[]
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    const ks_ell: bigint = this.$fks_ell;
    return this.$fbsk;
  }

  ksk_ref(): BinfheKeySwitchingKeyRefDyn
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    const ks_ell: bigint = this.$fks_ell;
    return this.$fksk;
  }
}

export class BinfheKeySwitchingKeyDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fks_ell!: bigint;
  $fksk!: Vec<BinfheLweCiphertextDyn[]>;

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fks_ell: bigint,
    $fksk: Vec<BinfheLweCiphertextDyn[]>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fks_ell: __zeroValue(this.$fks_ell), $fksk: __zeroValue(this.$fksk) }) as this;
  }

  ksk_rows(): BinfheLweCiphertextDyn[][]
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const ks_ell: bigint = this.$fks_ell;
    return this.$fksk;
  }
}

export class BinfheBootstrappingKeyDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $fks_ell!: bigint;
  $fbsk!: Vec<BinfheRgswCiphertextDyn>;
  $fksk!: BinfheKeySwitchingKeyDyn;

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $fks_ell: bigint,
    $fbsk: Vec<BinfheRgswCiphertextDyn>,
    $fksk: BinfheKeySwitchingKeyDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $fks_ell: __zeroValue(this.$fks_ell), $fbsk: __zeroValue(this.$fbsk), $fksk: __zeroValue(this.$fksk) }) as this;
  }

  as_ref(): BinfheBootstrappingKeyRefDyn
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    const ks_ell: bigint = this.$fks_ell;
    return new BinfheBootstrappingKeyRefDyn({ $fbsk: this.$fbsk, $fksk: new BinfheKeySwitchingKeyRefDyn({ $fksk: this.$fksk.$fksk, $fn_lwe: 0n, $fbig_n: 0n, $fks_ell: 0n }), $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n });
  }

  bsk_rows(): BinfheRgswCiphertextDyn[]
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    const ks_ell: bigint = this.$fks_ell;
    return this.$fbsk;
  }

  ksk_ref(): BinfheKeySwitchingKeyRefDyn
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    const ks_ell: bigint = this.$fks_ell;
    return new BinfheKeySwitchingKeyRefDyn({ $fksk: this.$fksk.$fksk, $fn_lwe: 0n, $fbig_n: 0n, $fks_ell: 0n });
  }
}

export class BinfheRgswRowDyn {
  $fn!: bigint;
  $frlwe0!: BinfheRlweCiphertextDyn;
  $frlwe1!: BinfheRlweCiphertextDyn;

  constructor(init: {
    $fn: bigint,
    $frlwe0: BinfheRlweCiphertextDyn,
    $frlwe1: BinfheRlweCiphertextDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $frlwe0: __zeroValue(this.$frlwe0), $frlwe1: __zeroValue(this.$frlwe1) }) as this;
  }
}

export class BinfheRgswCiphertextDyn {
  $fn!: bigint;
  $fell!: bigint;
  $frows!: BinfheRgswRowDyn[];

  constructor(init: {
    $fn: bigint,
    $fell: bigint,
    $frows: BinfheRgswRowDyn[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fell: __zeroValue(this.$fell), $frows: __zeroValue(this.$frows) }) as this;
  }
}

export class BinfheRlweSecretKeyDyn {
  $fn!: bigint;
  $fkey!: bigint[];

  constructor(init: {
    $fn: bigint,
    $fkey: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fkey: __zeroValue(this.$fkey) }) as this;
  }
}

export class BinfheRlweCiphertextDyn {
  $fn!: bigint;
  $fa!: bigint[];
  $fb!: bigint[];

  constructor(init: {
    $fn: bigint,
    $fa: bigint[],
    $fb: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fa: __zeroValue(this.$fa), $fb: __zeroValue(this.$fb) }) as this;
  }
}

export class BinfheLweSecretKeyDyn {
  $fn!: bigint;
  $fkey!: bigint[];

  constructor(init: {
    $fn: bigint,
    $fkey: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fkey: __zeroValue(this.$fkey) }) as this;
  }
}

export class BinfheLweCiphertextDyn {
  $fn!: bigint;
  $fa!: bigint[];
  $fb!: bigint;

  constructor(init: {
    $fn: bigint,
    $fa: bigint[],
    $fb: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fa: __zeroValue(this.$fa), $fb: __zeroValue(this.$fb) }) as this;
  }
}

export class LutInputs {
  $fids!: bigint[];
  $flen!: bigint;

  constructor(init: {
    $fids: bigint[],
    $flen: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fids: __zeroValue(this.$fids), $flen: __zeroValue(this.$flen) }) as this;
  }

  as_ref(): bigint[]
  {
    return this;
  }

  as_slice(): bigint[]
  {
    return this.$fids.slice(0, Number(Number(this.$flen)));
  }

  static default(): LutInputs
  {
    return LutInputs.new();
  }

  static from(ids: bigint[]): LutInputs
  {
    return LutInputs.from_slice(ids);
  }

  static from_slice(ids: bigint[]): LutInputs
  {
    let out = LutInputs.new();
    const take = BigInt(Math.min(Number(BigInt(ids.length)), Number(MAX_LUT_ARITY)));
    (out.$fids.slice(0, Number(take))).splice(0, (ids.slice(0, Number(take))).length, ...(ids.slice(0, Number(take))));
    out.$flen = ((take) & 0xFFn);
    return out;
  }

  is_empty(): boolean
  {
    return __equals(this.$flen, 0n);
  }

  len(): bigint
  {
    return Number(this.$flen);
  }

  static new(): LutInputs
  {
    return new LutInputs({ $fids: Array.from({length: Number(MAX_LUT_ARITY)}, () => 0n), $flen: 0n });
  }
}

export class LutSpec {
  $fentries!: Vec<boolean>;

  constructor(init: {
    $fentries: Vec<boolean>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fentries: __zeroValue(this.$fentries) }) as this;
  }
}

export class FailureBudget {
  $fper_bootstrap_log2!: bigint;
  $ftotal_log2!: bigint;

  constructor(init: {
    $fper_bootstrap_log2: bigint,
    $ftotal_log2: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fper_bootstrap_log2: __zeroValue(this.$fper_bootstrap_log2), $ftotal_log2: __zeroValue(this.$ftotal_log2) }) as this;
  }
}

export class BootstrapPlan {
  $fprofile!: ProfileId;
  $fk_max!: bigint;
  $fluts!: Vec<LutSpec>;
  $flayers!: Vec<Vec<PlanOp>>;
  $fnum_inputs!: bigint;
  $fnum_cells!: bigint;
  $foutputs!: Vec<bigint>;
  $fcell_outputs!: Vec<bigint>;
  $fbudget!: FailureBudget;

  constructor(init: {
    $fprofile: ProfileId,
    $fk_max: bigint,
    $fluts: Vec<LutSpec>,
    $flayers: Vec<Vec<PlanOp>>,
    $fnum_inputs: bigint,
    $fnum_cells: bigint,
    $foutputs: Vec<bigint>,
    $fcell_outputs: Vec<bigint>,
    $fbudget: FailureBudget
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fprofile: __zeroValue(this.$fprofile), $fk_max: __zeroValue(this.$fk_max), $fluts: __zeroValue(this.$fluts), $flayers: __zeroValue(this.$flayers), $fnum_inputs: __zeroValue(this.$fnum_inputs), $fnum_cells: __zeroValue(this.$fnum_cells), $foutputs: __zeroValue(this.$foutputs), $fcell_outputs: __zeroValue(this.$fcell_outputs), $fbudget: __zeroValue(this.$fbudget) }) as this;
  }

  bootstrap_op_count(): bigint
  {
    let count = 0n;
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        return (() => { const __match = op; if (true /* pattern Struct { kind: Custom("PlanOp::Lut"), fields: [("table", Ident { mutable: false, name: "table", subpat: None })], rest: true } */) { const table = __match.$ftable;
return (() => {
  if (!table_is_constant(this.$fluts[Number(Number(table))].$fentries))   {
    count = fieldAdd(count, 1n);
  }
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::CircuitBootstrap"), fields: [], rest: true } */) { return count = fieldAdd(count, 1n); } else { return (() => {
})(); } })();
      }
    }
    return count;
  }

  execute_clear(inputs: boolean[], cells: boolean[]): [Vec<boolean>, Vec<boolean>]
  {
    let wires = [...inputs];
    let rgsws = [] as any[];
    let cell_arena = [...cells];
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        return (() => { const __match = op; if (true /* pattern Struct { kind: Custom("PlanOp::Const"), fields: [("out", Ident { mutable: false, name: "out", subpat: None }), ("value", Ident { mutable: false, name: "value", subpat: None })], rest: false } */) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
  (wires).push(value);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Not"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (wires).push(!wires[Number(Number(input))]);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Lut"), fields: [("inputs", Ident { mutable: false, name: "inputs", subpat: None }), ("table", Ident { mutable: false, name: "table", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  let address = 0n;
  for (const [bit, input] of inputs.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    address = fieldBitor(address, fieldShl(Number(wires[Number(Number(input))]), bit));
  }
  (wires).push(this.$fluts[Number(Number(table))].$fentries[Number(address)]);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::CircuitBootstrap"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (rgsws).push(wires[Number(Number(input))]);
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
  (cell_arena).push((() => { if (rgsws[Number(Number(sel))]) {
  return cell_arena[Number(Number(then_cell))];
} else {
  return cell_arena[Number(Number(else_cell))];
} })());
})(); } })();
      }
    }
    return [wires, cell_arena];
  }

  plan_hash(): bigint
  {
    let h = 14695981039346656037n;
    for (const spec of this.$fluts)     {
      for (const [i, chunk] of __chunks(spec.$fentries, Number(8n)).enumerate())       {
        let byte = 0n;
        for (const [j, e] of chunk.map((val: any, i: number) => [i, val] as [number, typeof val]))         {
          byte = fieldBitor(byte, fieldShl(((e) & 0xFFn), j));
        }
        const _ = i;
      }
    }
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        return (() => { const __match = op; if (true /* pattern Struct { kind: Custom("PlanOp::Const"), fields: [("out", Ident { mutable: false, name: "out", subpat: None }), ("value", Ident { mutable: false, name: "value", subpat: None })], rest: false } */) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Not"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Lut"), fields: [("inputs", Ident { mutable: false, name: "inputs", subpat: None }), ("table", Ident { mutable: false, name: "table", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  for (const w of inputs)   {
  }
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::CircuitBootstrap"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
})(); } })();
      }
    }
    for (const w of this.$foutputs)     {
    }
    for (const c of this.$fcell_outputs)     {
    }
    return h;
  }

  validate(): Result<void, PlanError>
  {
    for (const [i, spec] of this.$fluts.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const len = BigInt(spec.$fentries.length);
      if ((__equals(len, 0n) || !((len) > 0n && ((len) & ((len) - 1n)) === 0n)))       {
        return new BadTableShape({ $ftable: Number(i) });
      }
      const arity = Number(Math.clz32((len) & -((len) | 0)));
      if ((arity > Number(this.$fk_max)))       {
        return new ArityExceedsKMax({ $ftable: Number(i) });
      }
    }
    let wires = this.$fnum_inputs;
    let rgsws = 0n;
    let cells = this.$fnum_cells;
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        return (() => { const __match = op; if (true /* pattern Struct { kind: Custom("PlanOp::Const"), fields: [("out", Ident { mutable: false, name: "out", subpat: None })], rest: true } */) { const out = __match.$fout;
return (() => {
  if (!__equals(out, wires))   {
    return new PlanError_BadReference();
  }
  wires = fieldAdd(wires, 1n);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Not"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  if (((input >= wires) || !__equals(out, wires)))   {
    return new PlanError_BadReference();
  }
  wires = fieldAdd(wires, 1n);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Lut"), fields: [("inputs", Ident { mutable: false, name: "inputs", subpat: None }), ("table", Ident { mutable: false, name: "table", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  if ((Number(table) >= BigInt(this.$fluts.length)))   {
    return new PlanError_BadReference();
  }
  const arity = Math.clz32((BigInt(this.$fluts[Number(Number(table))].$fentries.length)) & -((BigInt(this.$fluts[Number(Number(table))].$fentries.length)) | 0));
  if (((!__equals(BigInt(inputs.length), Number(arity)) || inputs.any((w) => (w >= wires))) || !__equals(out, wires)))   {
    return new PlanError_BadReference();
  }
  wires = fieldAdd(wires, 1n);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::CircuitBootstrap"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  if (((input >= wires) || !__equals(out, rgsws)))   {
    return new PlanError_BadReference();
  }
  rgsws = fieldAdd(rgsws, 1n);
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
  if (((((sel >= rgsws) || (then_cell >= cells)) || (else_cell >= cells)) || !__equals(out, cells)))   {
    return new PlanError_BadReference();
  }
  cells = fieldAdd(cells, 1n);
})(); } })();
      }
    }
    if ((this.$foutputs.any((w) => (w >= wires)) || this.$fcell_outputs.any((c) => (c >= cells))))     {
      return new PlanError_BadOutput();
    }
    const count = this.bootstrap_op_count();
    if ((count > 0n))     {
      const log2_count = fieldSub(64n, Math.clz32(count));
      if (((this.$fbudget.$ftotal_log2 < this.$fbudget.$fper_bootstrap_log2) || (fieldAdd(fieldSub(this.$fbudget.$ftotal_log2, this.$fbudget.$fper_bootstrap_log2), 1n) < log2_count)))       {
        return new PlanError_BudgetInconsistent();
      }
    }
    return [];
  }
}

export class PrivateKeySwitchingKeyDyn {
  $fbig_n!: bigint;
  $fpriv_ell!: bigint;
  $fa_col!: Vec<BinfheRlweCiphertextDyn[]>;
  $fb_col!: Vec<BinfheRlweCiphertextDyn[]>;
  $fa_body!: BinfheRlweCiphertextDyn[];
  $fb_body!: BinfheRlweCiphertextDyn[];

  constructor(init: {
    $fbig_n: bigint,
    $fpriv_ell: bigint,
    $fa_col: Vec<BinfheRlweCiphertextDyn[]>,
    $fb_col: Vec<BinfheRlweCiphertextDyn[]>,
    $fa_body: BinfheRlweCiphertextDyn[],
    $fb_body: BinfheRlweCiphertextDyn[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fbig_n: __zeroValue(this.$fbig_n), $fpriv_ell: __zeroValue(this.$fpriv_ell), $fa_col: __zeroValue(this.$fa_col), $fb_col: __zeroValue(this.$fb_col), $fa_body: __zeroValue(this.$fa_body), $fb_body: __zeroValue(this.$fb_body) }) as this;
  }
}

export class CircuitBootstrappingKeyDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $fks_ell!: bigint;
  $fpriv_ell!: bigint;
  $fbk!: BinfheBootstrappingKeyDyn;
  $fprivksk!: PrivateKeySwitchingKeyDyn;

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $fks_ell: bigint,
    $fpriv_ell: bigint,
    $fbk: BinfheBootstrappingKeyDyn,
    $fprivksk: PrivateKeySwitchingKeyDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $fks_ell: __zeroValue(this.$fks_ell), $fpriv_ell: __zeroValue(this.$fpriv_ell), $fbk: __zeroValue(this.$fbk), $fprivksk: __zeroValue(this.$fprivksk) }) as this;
  }
}

export class Reader {
  $fbytes!: bigint[];
  $foffset!: bigint;

  constructor(init: {
    $fbytes: bigint[],
    $foffset: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fbytes: __zeroValue(this.$fbytes), $foffset: __zeroValue(this.$foffset) }) as this;
  }

  byte(): Result<bigint, DecodeError>
  {
    return this.take(1n)[Number(0n)];
  }

  count(): Result<bigint, DecodeError>
  {
    const count = Number(this.u32());
    if ((count > MAX_ITEMS))     {
      return new DecodeError_TooLarge();
    }
    return count;
  }

  ids(): Result<Vec<bigint>, DecodeError>
  {
    const count = this.count();
    let ids = ([] as any[]);
    for (let _ = 0n; _ < count; _ += 1n)     {
      (ids).push(this.u32());
    }
    return ids;
  }

  take(count: bigint): Result<bigint[], DecodeError>
  {
    const end = (((this.$foffset + (count))) ?? (new DecodeError_Truncated()));
    const bytes = ((this.$fbytes?.[Array.from({length: Number(end - this.$foffset)}, (_, __i) => BigInt(__i) + this.$foffset)]) ?? (new DecodeError_Truncated()));
    this.$foffset = end;
    return bytes;
  }

  u32(): Result<bigint, DecodeError>
  {
    const bytes: bigint[] = this.take(4n);
    return u32_from_le_bytes(bytes);
  }
}

export class LutDyn {
  $faddr_bits!: bigint;
  $ftable_len!: bigint;
  $fbig_n!: bigint;
  $flog_q!: bigint;
  $flog_q_lwe!: bigint;
  $fk_max!: bigint;
  $flogical!: boolean[];
  $ftest_poly!: bigint[];
  $fis_constant!: boolean;

  constructor(init: {
    $faddr_bits: bigint,
    $ftable_len: bigint,
    $fbig_n: bigint,
    $flog_q: bigint,
    $flog_q_lwe: bigint,
    $fk_max: bigint,
    $flogical: boolean[],
    $ftest_poly: bigint[],
    $fis_constant: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $faddr_bits: __zeroValue(this.$faddr_bits), $ftable_len: __zeroValue(this.$ftable_len), $fbig_n: __zeroValue(this.$fbig_n), $flog_q: __zeroValue(this.$flog_q), $flog_q_lwe: __zeroValue(this.$flog_q_lwe), $fk_max: __zeroValue(this.$fk_max), $flogical: __zeroValue(this.$flogical), $ftest_poly: __zeroValue(this.$ftest_poly), $fis_constant: __zeroValue(this.$fis_constant) }) as this;
  }

  constant_value(): boolean
  {
    const addr_bits: bigint = this.$faddr_bits;
    const table_len: bigint = this.$ftable_len;
    const big_n: bigint = this.$fbig_n;
    const log_q: bigint = this.$flog_q;
    const log_q_lwe: bigint = this.$flog_q_lwe;
    const k_max: bigint = this.$fk_max;
    return this.$flogical[Number(0n)];
  }

  entries(): boolean[]
  {
    const addr_bits: bigint = this.$faddr_bits;
    const table_len: bigint = this.$ftable_len;
    const big_n: bigint = this.$fbig_n;
    const log_q: bigint = this.$flog_q;
    const log_q_lwe: bigint = this.$flog_q_lwe;
    const k_max: bigint = this.$fk_max;
    return this.$flogical;
  }

  is_constant(): boolean
  {
    const addr_bits: bigint = this.$faddr_bits;
    const table_len: bigint = this.$ftable_len;
    const big_n: bigint = this.$fbig_n;
    const log_q: bigint = this.$flog_q;
    const log_q_lwe: bigint = this.$flog_q_lwe;
    const k_max: bigint = this.$fk_max;
    return this.$fis_constant;
  }

  static new(addr_bits: bigint, table_len: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, k_max: bigint, logical: boolean[]): Result<LutDyn, LutError>
  {
    return (() => { const __match = check_lut_shape(addr_bits, table_len, big_n, log_q, log_q_lwe, k_max); if (__match === null || __match === undefined) { const e = __match;
return e; } else { return new LutDyn({ $flogical: logical, $ftest_poly: fill_test_poly(big_n, logical, addr_bits, k_max, log_q, log_q_lwe), $fis_constant: table_is_constant(logical) }); } })();
  }

  output_delta(): bigint
  {
    const addr_bits: bigint = this.$faddr_bits;
    const table_len: bigint = this.$ftable_len;
    const big_n: bigint = this.$fbig_n;
    const log_q: bigint = this.$flog_q;
    const log_q_lwe: bigint = this.$flog_q_lwe;
    const k_max: bigint = this.$fk_max;
    return fieldShl(1n, fieldSub(fieldSub(log_q_lwe, 1n), Number(k_max)));
  }

  test_polynomial(): bigint[]
  {
    const addr_bits: bigint = this.$faddr_bits;
    const table_len: bigint = this.$ftable_len;
    const big_n: bigint = this.$fbig_n;
    const log_q: bigint = this.$flog_q;
    const log_q_lwe: bigint = this.$flog_q_lwe;
    const k_max: bigint = this.$fk_max;
    return this.$ftest_poly;
  }
}

export class LweSampleDyn<T, U> {
  $fn!: bigint;
  $fm!: bigint;
  $fmatrix!: T[][];
  $fb!: any[];

  constructor(init: {
    $fn: bigint,
    $fm: bigint,
    $fmatrix: T[][],
    $fb: any[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fm: __zeroValue(this.$fm), $fmatrix: __zeroValue(this.$fmatrix), $fb: __zeroValue(this.$fb) }) as this;
  }

  static new<T, U>(n: bigint, m: bigint, matrix: T[][], b: any[]): LweSampleDyn<T, U>
  {
    return new LweSampleDyn<T, U>({ $fmatrix: matrix, $fb: b });
  }

  static sample<S, P, T, U>(ctx: { defaultA: () => any }, n: bigint, m: bigint, matrix: T[][], s: S[], e: any[]): LweSampleDyn<T, U>
  {
    return new LweSampleDyn<T, U>({ $fb: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, b]: any) => fieldMul(__clone(b), __clone(matrix[Number(i)][Number(a)]))).reduce((a: any, b: any) => fieldAdd(a, b), ctx.defaultA()), __clone(e[Number(i)]));
})()), $fmatrix: matrix });
  }
}

export class GateCertificate {
  $fname!: string;
  $farity!: bigint;
  $fprepare!: (arg0: bigint[], arg1: bigint) => bigint;
  $finterval_true!: [bigint, bigint];

  constructor(init: {
    $fname: string,
    $farity: bigint,
    $fprepare: (arg0: bigint[], arg1: bigint) => bigint,
    $finterval_true: [bigint, bigint]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fname: __zeroValue(this.$fname), $farity: __zeroValue(this.$farity), $fprepare: __zeroValue(this.$fprepare), $finterval_true: __zeroValue(this.$finterval_true) }) as this;
  }
}

export class PaperProfile {
  $fsecurity_bits!: bigint;
  $fring_degree!: bigint;
  $fmodulus_bits!: bigint;
  $fbatch_messages!: bigint;

  constructor(init: {
    $fsecurity_bits: bigint,
    $fring_degree: bigint,
    $fmodulus_bits: bigint,
    $fbatch_messages: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsecurity_bits: __zeroValue(this.$fsecurity_bits), $fring_degree: __zeroValue(this.$fring_degree), $fmodulus_bits: __zeroValue(this.$fmodulus_bits), $fbatch_messages: __zeroValue(this.$fbatch_messages) }) as this;
  }
}

export class EncodedLabelBatch {
  $fdifferences!: Vec<bigint>;
  $fzeroes!: Vec<bigint>;

  constructor(init: {
    $fdifferences: Vec<bigint>,
    $fzeroes: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdifferences: __zeroValue(this.$fdifferences), $fzeroes: __zeroValue(this.$fzeroes) }) as this;
  }

  static decode_selected(selected: bigint[]): Result<Vec<bigint[]>, LabelBatchDecodeError>
  {
    if (!__equals((BigInt(selected.length) % 3n), 0n))     {
      return new LabelBatchDecodeError_LengthMismatch();
    }
    return selected.chunks_exact(3n).map((chunk: any) => (() => {
  return decode_label_16([chunk[Number(0n)], chunk[Number(1n)], chunk[Number(2n)]]);
})());
  }

  differences(): bigint[]
  {
    return this.$fdifferences;
  }

  static expanded_choices(choices: boolean[]): Vec<boolean>
  {
    let out = ([] as any[]);
    for (const choice of choices)     {
      out.push(...(Array.from({length: Number(3n)}, () => choice)));
    }
    return out;
  }

  static from_pairs(pairs: LabelPairDyn[], offset: bigint[]): Result<EncodedLabelBatch, BatchError>
  {
    const batch = LabelBatchDyn.new(pairs, offset);
    let differences = ([] as any[]);
    let zeroes = ([] as any[]);
    const modulus = REFERENCE_PLAINTEXT_MODULUS;
    for (const pair of batch.$fpairs)     {
      const zero = encode_label_16(pair.$fzero);
      const one = encode_label_16(pair.$fone);
      for (const [one, zero] of one.map((__a: any, __i: number) => [__a, zero[__i]] as [typeof __a, any]))       {
        (differences).push((() => { if ((one >= zero)) {
  return fieldSub(one, zero);
} else {
  return fieldSub(modulus, fieldSub(zero, one));
} })());
        (zeroes).push(zero);
      }
    }
    return new EncodedLabelBatch({ $fdifferences: differences, $fzeroes: zeroes });
  }

  label_count(): bigint
  {
    return (BigInt(this.$fzeroes.length) / 3n);
  }

  pad_to_slots(slots: bigint): Result<PaddedLabelBatch, LabelBatchPaddingError>
  {
    const used = BigInt(this.$fzeroes.length);
    if ((slots < used))     {
      return new TooFewSlots({ $fslots: slots, $fused: used });
    }
    let differences = __clone(this.$fdifferences);
    let zeroes = __clone(this.$fzeroes);
    differences.resize(slots, 0n);
    zeroes.resize(slots, 0n);
    return new PaddedLabelBatch({ $flabel_count: this.label_count(), $fdifferences: differences, $fzeroes: zeroes });
  }

  zeroes(): bigint[]
  {
    return this.$fzeroes;
  }
}

export class PaddedLabelBatch {
  $flabel_count!: bigint;
  $fdifferences!: Vec<bigint>;
  $fzeroes!: Vec<bigint>;

  constructor(init: {
    $flabel_count: bigint,
    $fdifferences: Vec<bigint>,
    $fzeroes: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flabel_count: __zeroValue(this.$flabel_count), $fdifferences: __zeroValue(this.$fdifferences), $fzeroes: __zeroValue(this.$fzeroes) }) as this;
  }

  decode_selected(selected: bigint[]): Result<Vec<bigint[]>, LabelBatchDecodeError>
  {
    if (!__equals(BigInt(selected.length), this.slots()))     {
      return new LabelBatchDecodeError_LengthMismatch();
    }
    return EncodedLabelBatch.decode_selected(selected.slice(0, Number(fieldMul(this.$flabel_count, 3n))));
  }

  differences(): bigint[]
  {
    return this.$fdifferences;
  }

  expanded_choices(choices: boolean[]): Result<Vec<boolean>, LabelBatchPaddingError>
  {
    if (!__equals(BigInt(choices.length), this.$flabel_count))     {
      return new ChoiceLengthMismatch({ $fexpected: this.$flabel_count, $factual: BigInt(choices.length) });
    }
    let out = EncodedLabelBatch.expanded_choices(choices);
    out.resize(this.slots(), false);
    return out;
  }

  label_count(): bigint
  {
    return this.$flabel_count;
  }

  slots(): bigint
  {
    return BigInt(this.$fzeroes.length);
  }

  zeroes(): bigint[]
  {
    return this.$fzeroes;
  }
}

export class LabelPairDyn {
  $fn!: bigint;
  $fzero!: bigint[];
  $fone!: bigint[];

  constructor(init: {
    $fn: bigint,
    $fzero: bigint[],
    $fone: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fzero: __zeroValue(this.$fzero), $fone: __zeroValue(this.$fone) }) as this;
  }
}

export class LabelBatchDyn {
  $fn!: bigint;
  $fpairs!: LabelPairDyn[];

  constructor(init: {
    $fn: bigint,
    $fpairs: LabelPairDyn[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fpairs: __zeroValue(this.$fpairs) }) as this;
  }

  is_empty(): boolean
  {
    const n: bigint = this.$fn;
    return (this.$fpairs.length === 0);
  }

  len(): bigint
  {
    const n: bigint = this.$fn;
    return BigInt(this.$fpairs.length);
  }

  static new(n: bigint, pairs: LabelPairDyn[], offset: bigint[]): Result<LabelBatchDyn, BatchError>
  {
    if ((__equals(n, 0n) || __equals(fieldBitand(offset[Number(0n)], 1n), 0n)))     {
      return new BatchError_EvenOffset();
    }
    for (const [index, pair] of pairs.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      if (!__equals(pair.$fone, Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((byte: any) => fieldBitxor(pair.$fzero[Number(byte)], offset[Number(byte)]))))       {
        return new MismatchedPair({ $findex: index });
      }
    }
    return new LabelBatchDyn({ $fpairs: pairs });
  }

  select(choices: boolean[], output: bigint[][]): Result<void, BatchError>
  {
    const n: bigint = this.$fn;
    if ((!__equals(BigInt(choices.length), BigInt(this.$fpairs.length)) || !__equals(BigInt(output.length), BigInt(this.$fpairs.length))))     {
      return new BatchError_LengthMismatch();
    }
    for (const [[pair, choice], selected] of this.$fpairs.map((__a: any, __i: number) => [__a, choices[__i]] as [typeof __a, any]).map((__a: any, __i: number) => [__a, output[__i]] as [typeof __a, any]))     {
      selected = (() => { if (choice) {
  return pair.$fone;
} else {
  return pair.$fzero;
} })();
    }
    return [];
  }
}

export class Parameters {
  $fdegree!: bigint;
  $fwidth!: bigint;
  $fplaintext_modulus!: bigint;
  $fdelta!: bigint;
  $fgadget_base!: bigint;
  $fgadget_digits!: bigint;

  constructor(init: {
    $fdegree: bigint,
    $fwidth: bigint,
    $fplaintext_modulus: bigint,
    $fdelta: bigint,
    $fgadget_base: bigint,
    $fgadget_digits: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdegree: __zeroValue(this.$fdegree), $fwidth: __zeroValue(this.$fwidth), $fplaintext_modulus: __zeroValue(this.$fplaintext_modulus), $fdelta: __zeroValue(this.$fdelta), $fgadget_base: __zeroValue(this.$fgadget_base), $fgadget_digits: __zeroValue(this.$fgadget_digits) }) as this;
  }

  levels(): Result<bigint, Error>
  {
    if (((this.$fwidth < 2n) || !((this.$fwidth) > 0n && ((this.$fwidth) & ((this.$fwidth) - 1n)) === 0n)))     {
      return new Error_InvalidParameters();
    }
    return Number(ilog2(this.$fwidth));
  }

  static scaled_reference(degree: bigint, width: bigint): Parameters
  {
    return new Parameters({ $fdegree: degree, $fwidth: width });
  }

  slots(): Result<bigint, Error>
  {
    return ((((this.$fdegree) * (this.$fwidth))) ?? (new Error_InvalidParameters()));
  }

  validate(): Result<void, Error>
  {
    if (((((((((((this.$fdegree < 2n) || !((this.$fdegree) > 0n && ((this.$fdegree) & ((this.$fdegree) - 1n)) === 0n)) || (this.$fwidth < 2n)) || !((this.$fwidth) > 0n && ((this.$fwidth) & ((this.$fwidth) - 1n)) === 0n)) || (this.$fplaintext_modulus < 3n)) || (this.$fdelta < 3n)) || (this.$fgadget_base < 2n)) || __equals(this.$fgadget_digits, 0n)) || !__equals((fieldSub(this.$fplaintext_modulus, 1n) % fieldMul(2n, BigInt(this.$fdegree))), 0n)) || !__equals((fieldSub(this.$fdelta, 1n) % fieldMul(2n, BigInt(this.$fdegree))), 0n)))     {
      return new Error_InvalidParameters();
    }
    this.slots();
    const modulus = fieldMul((this.$fplaintext_modulus as unknown as bigint), (this.$fdelta as unknown as bigint));
    let capacity = 1n;
    for (let _ = 0n; _ < this.$fgadget_digits; _ += 1n)     {
      capacity = ((((capacity) * ((this.$fgadget_base as unknown as bigint)))) ?? (new Error_InvalidParameters()));
    }
    if ((capacity <= modulus))     {
      return new Error_InvalidParameters();
    }
    return [];
  }
}

export class ZeroNoise {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  sample(_standard_deviation: bigint, _bound: bigint, output: bigint[]): Result<void, Error>
  {
    (output).fill(0n);
    return [];
  }
}

export class Polynomial {
  $ffirst!: Vec<bigint>;
  $fsecond!: Vec<bigint>;

  constructor(init: {
    $ffirst: Vec<bigint>,
    $fsecond: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $ffirst: __zeroValue(this.$ffirst), $fsecond: __zeroValue(this.$fsecond) }) as this;
  }

  add_assign(ring: any, other: any)
  {
    for (const [left, right] of this.$ffirst.zip(other.$ffirst))     {
      left = add_mod(left, right, ring.$fparameters.$fplaintext_modulus);
    }
    for (const [left, right] of this.$fsecond.zip(other.$fsecond))     {
      left = add_mod(left, right, ring.$fparameters.$fdelta);
    }
  }

  degree(): bigint
  {
    return BigInt(this.$ffirst.length);
  }

  negate_assign(ring: any)
  {
    for (const value of this.$ffirst)     {
      if (!__equals(value, 0n))       {
        value = fieldSub(ring.$fparameters.$fplaintext_modulus, value);
      }
    }
    for (const value of this.$fsecond)     {
      if (!__equals(value, 0n))       {
        value = fieldSub(ring.$fparameters.$fdelta, value);
      }
    }
  }

  product(ring: any, other: any): Polynomial
  {
    let output = ring.zero();
    for (const [[output, left], right] of output.$ffirst.zip(this.$ffirst).zip(other.$ffirst))     {
      output = mul_mod(left, right, ring.$fparameters.$fplaintext_modulus);
    }
    for (const [[output, left], right] of output.$fsecond.zip(this.$fsecond).zip(other.$fsecond))     {
      output = mul_mod(left, right, ring.$fparameters.$fdelta);
    }
    return output;
  }

  rns_limbs(): [bigint[], bigint[]]
  {
    return [this.$ffirst, this.$fsecond];
  }

  scaled(ring: any, base: bigint, power: bigint): Polynomial
  {
    const first = pow_mod((base % ring.$fparameters.$fplaintext_modulus), power, ring.$fparameters.$fplaintext_modulus);
    const second = pow_mod((base % ring.$fparameters.$fdelta), power, ring.$fparameters.$fdelta);
    let output = __clone(this);
    for (const value of output.$ffirst)     {
      value = mul_mod(value, first, ring.$fparameters.$fplaintext_modulus);
    }
    for (const value of output.$fsecond)     {
      value = mul_mod(value, second, ring.$fparameters.$fdelta);
    }
    return output;
  }

  sub_assign(ring: any, other: any)
  {
    for (const [left, right] of this.$ffirst.zip(other.$ffirst))     {
      left = sub_mod(left, right, ring.$fparameters.$fplaintext_modulus);
    }
    for (const [left, right] of this.$fsecond.zip(other.$fsecond))     {
      left = sub_mod(left, right, ring.$fparameters.$fdelta);
    }
  }
}

export class PublicParameters {
  $fparameters!: Parameters;
  $flhe_a!: Vec<Polynomial>;
  $flenc_b!: Vec<Polynomial>;

  constructor(init: {
    $fparameters: Parameters,
    $flhe_a: Vec<Polynomial>,
    $flenc_b: Vec<Polynomial>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparameters: __zeroValue(this.$fparameters), $flhe_a: __zeroValue(this.$flhe_a), $flenc_b: __zeroValue(this.$flenc_b) }) as this;
  }

  lenc_polynomials(): unknown /* impl ExactSizeIterator<Item = &Polynomial> */
  {
    return this.$flenc_b;
  }

  lhe_polynomials(): unknown /* impl ExactSizeIterator<Item = &Polynomial> */
  {
    return this.$flhe_a;
  }

  parameters(): Parameters
  {
    return this.$fparameters;
  }
}

export class FirstCiphertext {
  $flhe_state!: Vec<Polynomial>;
  $flhe_ciphertext!: Vec<Polynomial>;
  $flenc_ciphertext!: Vec<Polynomial>;

  constructor(init: {
    $flhe_state: Vec<Polynomial>,
    $flhe_ciphertext: Vec<Polynomial>,
    $flenc_ciphertext: Vec<Polynomial>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flhe_state: __zeroValue(this.$flhe_state), $flhe_ciphertext: __zeroValue(this.$flhe_ciphertext), $flenc_ciphertext: __zeroValue(this.$flenc_ciphertext) }) as this;
  }

  lenc_polynomials(): unknown /* impl ExactSizeIterator<Item = &Polynomial> */
  {
    return this.$flenc_ciphertext;
  }

  lhe_polynomials(): unknown /* impl ExactSizeIterator<Item = &Polynomial> */
  {
    return this.$flhe_ciphertext;
  }
}

export class SecondCiphertext {
  $flhe_state!: Polynomial;
  $flhe_ciphertext!: Vec<Polynomial>;

  constructor(init: {
    $flhe_state: Polynomial,
    $flhe_ciphertext: Vec<Polynomial>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flhe_state: __zeroValue(this.$flhe_state), $flhe_ciphertext: __zeroValue(this.$flhe_ciphertext) }) as this;
  }

  polynomials(): unknown /* impl ExactSizeIterator<Item = &Polynomial> */
  {
    return this.$flhe_ciphertext;
  }
}

export class SelectionKey {
  $fkey!: Polynomial;

  constructor(init: {
    $fkey: Polynomial
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fkey: __zeroValue(this.$fkey) }) as this;
  }

  polynomial(): Polynomial
  {
    return this.$fkey;
  }
}

export class BatchSelect {
  $fring!: Ring;
  $fpublic!: PublicParameters;

  constructor(init: {
    $fring: Ring,
    $fpublic: PublicParameters
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fring: __zeroValue(this.$fring), $fpublic: __zeroValue(this.$fpublic) }) as this;
  }

  check_first(first: any): Result<void, Error>
  {
    const p = this.$fring.$fparameters;
    if ((((!__equals(BigInt(first.$flhe_state.length), p.$fgadget_digits) || !__equals(BigInt(first.$flhe_ciphertext.length), fieldMul(p.$fwidth, p.$fgadget_digits))) || !__equals(BigInt(first.$flenc_ciphertext.length), fieldMul(fieldMul(fieldMul(p.levels(), p.$fwidth), 2n), p.$fgadget_digits))) || first.$flhe_state.concat(first.$flhe_ciphertext).concat(first.$flenc_ciphertext).any((poly) => !__equals(poly.degree(), p.$fdegree))))     {
      return new Error_LengthMismatch();
    }
    return [];
  }

  check_second(second: any): Result<void, Error>
  {
    const p = this.$fring.$fparameters;
    if (((!__equals(BigInt(second.$flhe_ciphertext.length), p.$fwidth) || !__equals(second.$flhe_state.degree(), p.$fdegree)) || second.$flhe_ciphertext.any((poly) => !__equals(poly.degree(), p.$fdegree))))     {
      return new Error_LengthMismatch();
    }
    return [];
  }

  dec(first: any, second: any, key: any, choices: boolean[]): Result<Vec<bigint>, Error>
  {
    this.check_first(first);
    this.check_second(second);
    if (!__equals(key.$fkey.degree(), this.$fring.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    const choice = this.$fring.encode_choices(choices);
    const tree = this.lenc_digest(choice);
    const digits = this.$fring.decompose(tree.$fdigest);
    let result = ([] as any[]);
    for (let index = 0n; index < this.$fring.$fparameters.$fwidth; index += 1n)     {
      const row = first.$flhe_ciphertext.slice(Number(fieldMul(index, this.$fring.$fparameters.$fgadget_digits)), Number(fieldMul(fieldAdd(index, 1n), this.$fring.$fparameters.$fgadget_digits)));
      let value = inner_product(this.$fring, row, digits);
      value.add_assign(this.$fring, second.$flhe_ciphertext[Number(index)]);
      value.sub_assign(this.$fring, this.$fpublic.$flhe_a[Number(index)].product(this.$fring, key.$fkey));
      (result).push(value);
    }
    const correction = this.lenc_eval(first.$flenc_ciphertext, tree);
    for (const [value, delta] of result.zip(correction))     {
      value.sub_assign(this.$fring, delta);
    }
    return this.$fring.decode_messages(result);
  }

  enc1(ctx: { defaultA: () => any }, l1: bigint[], random: any, noise: any): Result<FirstCiphertext, Error>
  {
    const messages = this.$fring.encode_messages(l1);
    const [random_vector, lenc_ciphertext] = this.lenc_enc(ctx, messages, random, noise);
    const [lhe_state, lhe_ciphertext] = this.lhe_enc1(ctx, random_vector, random, noise);
    return new FirstCiphertext({ $flhe_state: lhe_state, $flhe_ciphertext: lhe_ciphertext, $flenc_ciphertext: lenc_ciphertext });
  }

  enc2(ctx: { defaultA: () => any }, l2: bigint[], random: any, noise: any): Result<SecondCiphertext, Error>
  {
    let messages = this.$fring.encode_messages(l2);
    for (const message of messages)     {
      this.$fring.add_noise(ctx, message, noise, LARGE_NOISE_STANDARD_DEVIATION, LARGE_NOISE_MAX_DEVIATION);
    }
    const [lhe_state, lhe_ciphertext] = this.lhe_enc2(ctx, messages, random, noise);
    return new SecondCiphertext({ $flhe_state: lhe_state, $flhe_ciphertext: lhe_ciphertext });
  }

  keygen(first: any, second: any, choices: boolean[]): Result<SelectionKey, Error>
  {
    this.check_first(first);
    this.check_second(second);
    const choice = this.$fring.encode_choices(choices);
    const tree = this.lenc_digest(choice);
    const digits = this.$fring.decompose(tree.$fdigest);
    let key = __clone(second.$flhe_state);
    for (const [state, digit] of first.$flhe_state.map((__a: any, __i: number) => [__a, digits[__i]] as [typeof __a, any]))     {
      key.add_assign(this.$fring, state.product(this.$fring, digit));
    }
    return new SelectionKey({ $fkey: key });
  }

  lenc_digest(choice: Polynomial[]): Result<Tree, Error>
  {
    const p = this.$fring.$fparameters;
    if (!__equals(BigInt(choice.length), p.$fwidth))     {
      return new Error_LengthMismatch();
    }
    let tree = [] as any[];
    for (const [index, value] of choice.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const digits = this.$fring.decompose(value);
      const base = fieldMul(fieldAdd(fieldSub(p.$fwidth, 1n), index), p.$fgadget_digits);
      tree.slice(Number(base), Number(fieldAdd(base, p.$fgadget_digits))).clone_from_slice(digits);
    }
    let digest = this.$fring.zero();
    for (const node of (Array.from({length: Number(fieldSub(p.$fwidth, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      const children = fieldMul(fieldAdd(fieldMul(2n, node), 1n), p.$fgadget_digits);
      let parent = inner_product(this.$fring, this.$fpublic.$flenc_b, tree.slice(Number(children), Number(fieldAdd(children, fieldMul(2n, p.$fgadget_digits)))));
      parent.negate_assign(this.$fring);
      if (__equals(node, 0n))       {
        digest = parent;
      } else       {
        const digits = this.$fring.decompose(parent);
        const base = fieldMul(node, p.$fgadget_digits);
        tree.slice(Number(base), Number(fieldAdd(base, p.$fgadget_digits))).clone_from_slice(digits);
      }
    }
    return new Tree({ $ftree: tree, $fdigest: digest });
  }

  lenc_enc(ctx: { defaultA: () => any }, message: Polynomial[], random: any, noise: any): Result<[Vec<Polynomial>, Vec<Polynomial>], Error>
  {
    const p = this.$fring.$fparameters;
    if (!__equals(BigInt(message.length), p.$fwidth))     {
      return new Error_LengthMismatch();
    }
    const levels = p.levels();
    let random_vector = ([] as any[]);
    for (let _ = 0n; _ < fieldMul(levels, p.$fwidth); _ += 1n)     {
      (random_vector).push(this.$fring.uniform(random));
    }
    let ciphertext = ([] as any[]);
    for (let level = 0n; level < levels; level += 1n)     {
      for (let row = 0n; row < p.$fwidth; row += 1n)       {
        for (const public_ of this.$fpublic.$flenc_b)         {
          (ciphertext).push(random_vector[Number(fieldAdd(fieldMul(level, p.$fwidth), row))].product(this.$fring, public_));
        }
        const half = (() => { if (__equals(fieldBitand(row, fieldShl(1n, fieldSub(fieldSub(levels, level), 1n))), 0n)) {
  return 0n;
} else {
  return p.$fgadget_digits;
} })();
        const next = (() => { if (__equals(fieldAdd(level, 1n), levels)) {
  return message[Number(row)];
} else {
  return random_vector[Number(fieldAdd(fieldMul(fieldAdd(level, 1n), p.$fwidth), row))];
} })();
        const base = fieldAdd(fieldMul(fieldMul(fieldAdd(fieldMul(level, p.$fwidth), row), 2n), p.$fgadget_digits), half);
        for (let digit = 0n; digit < p.$fgadget_digits; digit += 1n)         {
          const extra = next.scaled(this.$fring, p.$fgadget_base, digit);
          ciphertext[Number(fieldAdd(base, digit))].add_assign(this.$fring, extra);
        }
      }
    }
    for (const value of ciphertext)     {
      this.$fring.add_noise(ctx, value, noise, SMALL_NOISE_STANDARD_DEVIATION, SMALL_NOISE_MAX_DEVIATION);
    }
    return [random_vector, ciphertext];
  }

  lenc_eval(ciphertext: Polynomial[], tree: any): Result<Vec<Polynomial>, Error>
  {
    const p = this.$fring.$fparameters;
    const levels = p.levels();
    if ((!__equals(BigInt(ciphertext.length), fieldMul(fieldMul(fieldMul(levels, p.$fwidth), 2n), p.$fgadget_digits)) || !__equals(BigInt(tree.$ftree.length), fieldMul(fieldSub(fieldMul(2n, p.$fwidth), 1n), p.$fgadget_digits))))     {
      return new Error_LengthMismatch();
    }
    let delta = ([] as any[]);
    for (let row = 0n; row < p.$fwidth; row += 1n)     {
      let value = inner_product(this.$fring, ciphertext.slice(Number(fieldMul(fieldMul(row, 2n), p.$fgadget_digits)), Number(fieldMul(fieldMul(fieldAdd(row, 1n), 2n), p.$fgadget_digits))), tree.$ftree.slice(Number(p.$fgadget_digits), Number(fieldMul(3n, p.$fgadget_digits))));
      for (let level = 1n; level < levels; level += 1n)       {
        const ciphertext_base = fieldMul(fieldMul(fieldAdd(fieldMul(level, p.$fwidth), row), 2n), p.$fgadget_digits);
        const tree_base = fieldMul(fieldAdd(fieldMul(fieldSub(fieldAdd(fieldShr(row, fieldSub(levels, level)), fieldShl(1n, level)), 1n), 2n), 1n), p.$fgadget_digits);
        const term = inner_product(this.$fring, ciphertext.slice(Number(ciphertext_base), Number(fieldAdd(ciphertext_base, fieldMul(2n, p.$fgadget_digits)))), tree.$ftree.slice(Number(tree_base), Number(fieldAdd(tree_base, fieldMul(2n, p.$fgadget_digits)))));
        value.add_assign(this.$fring, term);
      }
      value.negate_assign(this.$fring);
      (delta).push(value);
    }
    return delta;
  }

  lhe_enc1(ctx: { defaultA: () => any }, messages: Polynomial[], random: any, noise: any): Result<[Vec<Polynomial>, Vec<Polynomial>], Error>
  {
    if (!__equals(BigInt(messages.length), this.$fring.$fparameters.$fwidth))     {
      return new Error_LengthMismatch();
    }
    const p = this.$fring.$fparameters;
    let state = ([] as any[]);
    for (let _ = 0n; _ < p.$fgadget_digits; _ += 1n)     {
      (state).push(this.$fring.uniform(random));
    }
    let ciphertext = ([] as any[]);
    for (const [index, message] of messages.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      for (let digit = 0n; digit < p.$fgadget_digits; digit += 1n)       {
        let value = this.$fpublic.$flhe_a[Number(index)].product(this.$fring, state[Number(digit)]);
        value.add_assign(this.$fring, message.scaled(this.$fring, p.$fgadget_base, digit));
        this.$fring.add_noise(ctx, value, noise, SMALL_NOISE_STANDARD_DEVIATION, SMALL_NOISE_MAX_DEVIATION);
        (ciphertext).push(value);
      }
    }
    return [state, ciphertext];
  }

  lhe_enc2(ctx: { defaultA: () => any }, messages: Polynomial[], random: any, noise: any): Result<[Polynomial, Vec<Polynomial>], Error>
  {
    if (!__equals(BigInt(messages.length), this.$fring.$fparameters.$fwidth))     {
      return new Error_LengthMismatch();
    }
    const state = this.$fring.uniform(random);
    let ciphertext = ([] as any[]);
    for (const [a, message] of this.$fpublic.$flhe_a.map((__a: any, __i: number) => [__a, messages[__i]] as [typeof __a, any]))     {
      let value = a.product(this.$fring, state);
      value.add_assign(this.$fring, message);
      this.$fring.add_noise(ctx, value, noise, LARGE_NOISE_STANDARD_DEVIATION, LARGE_NOISE_MAX_DEVIATION);
      (ciphertext).push(value);
    }
    return [state, ciphertext];
  }

  public_parameters(): PublicParameters
  {
    return this.$fpublic;
  }

  static setup(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, parameters: any, random: any): Result<BatchSelect, Error>
  {
    const ring = Ring.new(ctx, parameters);
    let lhe_a = ([] as any[]);
    for (let _ = 0n; _ < parameters.$fwidth; _ += 1n)     {
      (lhe_a).push(ring.uniform(random));
    }
    let lenc_b = ([] as any[]);
    for (let _ = 0n; _ < fieldMul(2n, parameters.$fgadget_digits); _ += 1n)     {
      (lenc_b).push(ring.uniform(random));
    }
    return new BatchSelect({ $fring: ring, $fpublic: new PublicParameters({ $fparameters: parameters, $flhe_a: lhe_a, $flenc_b: lenc_b }) });
  }
}

export class Tree {
  $ftree!: Vec<Polynomial>;
  $fdigest!: Polynomial;

  constructor(init: {
    $ftree: Vec<Polynomial>,
    $fdigest: Polynomial
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $ftree: __zeroValue(this.$ftree), $fdigest: __zeroValue(this.$fdigest) }) as this;
  }
}

export class Ring {
  $fparameters!: Parameters;
  $ffirst_ntt!: Ntt;
  $fsecond_ntt!: Ntt;
  $finverse_plaintext_mod_delta!: bigint;
  $finverse_delta_mod_plaintext!: bigint;

  constructor(init: {
    $fparameters: Parameters,
    $ffirst_ntt: Ntt,
    $fsecond_ntt: Ntt,
    $finverse_plaintext_mod_delta: bigint,
    $finverse_delta_mod_plaintext: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparameters: __zeroValue(this.$fparameters), $ffirst_ntt: __zeroValue(this.$ffirst_ntt), $fsecond_ntt: __zeroValue(this.$fsecond_ntt), $finverse_plaintext_mod_delta: __zeroValue(this.$finverse_plaintext_mod_delta), $finverse_delta_mod_plaintext: __zeroValue(this.$finverse_delta_mod_plaintext) }) as this;
  }

  add_noise(ctx: { defaultA: () => any }, value: any, noise: any, standard_deviation: bigint, bound: bigint): Result<void, Error>
  {
    let coefficients = [] as any[];
    noise.sample(coefficients.$fn, standard_deviation, bound, coefficients);
    if (coefficients.any((sample) => (sample.unsigned_abs() > bound)))     {
      return new Error_Noise();
    }
    let first = ([] as any[]);
    let second = ([] as any[]);
    for (const sample of coefficients)     {
      (first).push(signed_to_mod(sample, this.$fparameters.$fplaintext_modulus));
      (second).push(signed_to_mod(sample, this.$fparameters.$fdelta));
    }
    this.$ffirst_ntt.forward(first);
    this.$fsecond_ntt.forward(second);
    for (const [destination, error] of value.$ffirst.zip(first))     {
      destination = add_mod(destination, error, this.$fparameters.$fplaintext_modulus);
    }
    for (const [destination, error] of value.$fsecond.zip(second))     {
      destination = add_mod(destination, error, this.$fparameters.$fdelta);
    }
    return [];
  }

  combine(first: bigint, second: bigint): bigint
  {
    const first_mod_delta = (first % this.$fparameters.$fdelta);
    const offset = mul_mod(sub_mod(second, first_mod_delta, this.$fparameters.$fdelta), this.$finverse_plaintext_mod_delta, this.$fparameters.$fdelta);
    return fieldAdd((first as unknown as bigint), fieldMul((this.$fparameters.$fplaintext_modulus as unknown as bigint), (offset as unknown as bigint)));
  }

  decode_messages(values: Polynomial[]): Result<Vec<bigint>, Error>
  {
    if ((!__equals(BigInt(values.length), this.$fparameters.$fwidth) || values.any((value) => !__equals(value.degree(), this.$fparameters.$fdegree))))     {
      return new Error_LengthMismatch();
    }
    let output = ([] as any[]);
    for (const value of values)     {
      let noise = __clone(value.$fsecond);
      this.$fsecond_ntt.inverse(noise);
      let correction = ([] as any[]);
      for (const error of noise)       {
        const signed = (() => { if ((error > (this.$fparameters.$fdelta / 2n))) {
  const magnitude = fieldSub(this.$fparameters.$fdelta, error);
  if ((magnitude >= this.$fparameters.$fplaintext_modulus))   {
    return new Error_Noise();
  }
  return (() => { if (__equals(magnitude, 0n)) {
  return 0n;
} else {
  return fieldSub(this.$fparameters.$fplaintext_modulus, magnitude);
} })();
} else {
  if ((error >= this.$fparameters.$fplaintext_modulus))   {
    return new Error_Noise();
  }
  return error;
} })();
        (correction).push(signed);
      }
      this.$ffirst_ntt.forward(correction);
      for (const [message, correction] of value.$ffirst.map((__a: any, __i: number) => [__a, correction[__i]] as [typeof __a, any]))       {
        const no_error = sub_mod(message, correction, this.$fparameters.$fplaintext_modulus);
        (output).push(mul_mod(no_error, this.$finverse_delta_mod_plaintext, this.$fparameters.$fplaintext_modulus));
      }
    }
    return output;
  }

  decompose(value: any): Result<Vec<Polynomial>, Error>
  {
    if (!__equals(value.degree(), this.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    let first = __clone(value.$ffirst);
    let second = __clone(value.$fsecond);
    this.$ffirst_ntt.inverse(first);
    this.$fsecond_ntt.inverse(second);
    let digits: Vec<Polynomial> = Array.from({length: Number(this.$fparameters.$fgadget_digits - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => this.zero());
    for (let index = 0n; index < this.$fparameters.$fdegree; index += 1n)     {
      let combined = this.combine(first[Number(index)], second[Number(index)]);
      for (const digit of digits)       {
        const part = BigInt((combined % (this.$fparameters.$fgadget_base as unknown as bigint)));
        digit.$ffirst[Number(index)] = (part % this.$fparameters.$fplaintext_modulus);
        digit.$fsecond[Number(index)] = (part % this.$fparameters.$fdelta);
        combined /= (this.$fparameters.$fgadget_base as unknown as bigint);
      }
      if (!__equals(combined, 0n))       {
        return new Error_Arithmetic();
      }
    }
    for (const digit of digits)     {
      this.$ffirst_ntt.forward(digit.$ffirst);
      this.$fsecond_ntt.forward(digit.$fsecond);
    }
    return digits;
  }

  encode_choices(input: boolean[]): Result<Vec<Polynomial>, Error>
  {
    if (!__equals(BigInt(input.length), this.$fparameters.slots()))     {
      return new Error_LengthMismatch();
    }
    let output = ([] as any[]);
    for (const chunk of input.chunks_exact(this.$fparameters.$fdegree))     {
      let value = this.zero();
      for (const [[first, second], choice] of value.$ffirst.zip(value.$fsecond).zip(chunk))       {
        const choice_1 = u64.from(choice);
        first = choice_1;
        second = choice_1;
      }
      this.$ffirst_ntt.inverse(value.$fsecond);
      this.$fsecond_ntt.forward(value.$fsecond);
      (output).push(value);
    }
    return output;
  }

  encode_messages(input: bigint[]): Result<Vec<Polynomial>, Error>
  {
    if (!__equals(BigInt(input.length), this.$fparameters.slots()))     {
      return new Error_LengthMismatch();
    }
    if (input.any((value) => (value >= this.$fparameters.$fplaintext_modulus)))     {
      return new Error_NonCanonicalPlaintext();
    }
    let output = ([] as any[]);
    for (const chunk of input.chunks_exact(this.$fparameters.$fdegree))     {
      let value = this.zero();
      for (const [slot, message] of value.$ffirst.zip(chunk))       {
        slot = mul_mod(message, (this.$fparameters.$fdelta % this.$fparameters.$fplaintext_modulus), this.$fparameters.$fplaintext_modulus);
      }
      (output).push(value);
    }
    return output;
  }

  static new(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, parameters: any): Result<Ring, Error>
  {
    parameters.validate();
    return new Ring({ $ffirst_ntt: Ntt.new(parameters.$fdegree, parameters.$fplaintext_modulus), $fsecond_ntt: Ntt.new(parameters.$fdegree, parameters.$fdelta), $finverse_plaintext_mod_delta: ((inverse_mod((parameters.$fplaintext_modulus % parameters.$fdelta), parameters.$fdelta)) ?? (new Error_Arithmetic())), $finverse_delta_mod_plaintext: ((inverse_mod((parameters.$fdelta % parameters.$fplaintext_modulus), parameters.$fplaintext_modulus)) ?? (new Error_Arithmetic())), $fparameters: parameters });
  }

  uniform(random: any): Result<Polynomial, Error>
  {
    let value = this.zero();
    sample_uniform(random, this.$fparameters.$fplaintext_modulus, value.$ffirst);
    sample_uniform(random, this.$fparameters.$fdelta, value.$fsecond);
    return value;
  }

  zero(): Polynomial
  {
    return new Polynomial({ $ffirst: [] as any[], $fsecond: [] as any[] });
  }
}

export class Ntt {
  $fmodulus!: bigint;
  $fdegree!: bigint;
  $fpsi!: bigint;
  $fomega!: bigint;
  $finverse_psi!: bigint;
  $finverse_omega!: bigint;
  $finverse_degree!: bigint;

  constructor(init: {
    $fmodulus: bigint,
    $fdegree: bigint,
    $fpsi: bigint,
    $fomega: bigint,
    $finverse_psi: bigint,
    $finverse_omega: bigint,
    $finverse_degree: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fmodulus: __zeroValue(this.$fmodulus), $fdegree: __zeroValue(this.$fdegree), $fpsi: __zeroValue(this.$fpsi), $fomega: __zeroValue(this.$fomega), $finverse_psi: __zeroValue(this.$finverse_psi), $finverse_omega: __zeroValue(this.$finverse_omega), $finverse_degree: __zeroValue(this.$finverse_degree) }) as this;
  }

  cyclic(values: bigint[], root: bigint)
  {
    bit_reverse(values);
    let length = 2n;
    while ((length <= this.$fdegree))     {
      const step = pow_mod(root, (this.$fdegree / length), this.$fmodulus);
      for (const start of Array.from({length: Number(this.$fdegree - 0n)}, (_, __i) => BigInt(__i) + 0n).step_by(length))       {
        let twiddle = 1n;
        for (let offset = 0n; offset < (length / 2n); offset += 1n)         {
          const left = values[Number(fieldAdd(start, offset))];
          const right = mul_mod(values[Number(fieldAdd(fieldAdd(start, offset), (length / 2n)))], twiddle, this.$fmodulus);
          values[Number(fieldAdd(start, offset))] = add_mod(left, right, this.$fmodulus);
          values[Number(fieldAdd(fieldAdd(start, offset), (length / 2n)))] = sub_mod(left, right, this.$fmodulus);
          twiddle = mul_mod(twiddle, step, this.$fmodulus);
        }
      }
      length = fieldMul(length, 2n);
    }
  }

  forward(values: bigint[])
  {
    for (const [index, value] of values.enumerate())     {
      value = mul_mod(value, pow_mod(this.$fpsi, index, this.$fmodulus), this.$fmodulus);
    }
    this.cyclic(values, this.$fomega);
  }

  inverse(values: bigint[])
  {
    this.cyclic(values, this.$finverse_omega);
    for (const [index, value] of values.enumerate())     {
      value = mul_mod(value, this.$finverse_degree, this.$fmodulus);
      value = mul_mod(value, pow_mod(this.$finverse_psi, index, this.$fmodulus), this.$fmodulus);
    }
  }

  static new(degree: bigint, modulus: bigint): Result<Ntt, Error>
  {
    const psi = ((find_negacyclic_root(degree, modulus)) ?? (new Error_Arithmetic()));
    const omega = mul_mod(psi, psi, modulus);
    return new Ntt({ $fmodulus: modulus, $fdegree: degree, $fpsi: psi, $fomega: omega, $finverse_psi: ((inverse_mod(psi, modulus)) ?? (new Error_Arithmetic())), $finverse_omega: ((inverse_mod(omega, modulus)) ?? (new Error_Arithmetic())), $finverse_degree: ((inverse_mod(BigInt(degree), modulus)) ?? (new Error_Arithmetic())) });
  }
}

export class FrameBinding {
  $fparameter_fingerprint!: bigint[];
  $fsession_id!: bigint[];
  $fmanifest_digest!: bigint[];
  $fuse_counter!: bigint;

  constructor(init: {
    $fparameter_fingerprint: bigint[],
    $fsession_id: bigint[],
    $fmanifest_digest: bigint[],
    $fuse_counter: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparameter_fingerprint: __zeroValue(this.$fparameter_fingerprint), $fsession_id: __zeroValue(this.$fsession_id), $fmanifest_digest: __zeroValue(this.$fmanifest_digest), $fuse_counter: __zeroValue(this.$fuse_counter) }) as this;
  }
}

export class Frame {
  $fstage!: Stage;
  $fbinding!: FrameBinding;
  $fpayload!: Vec<bigint>;

  constructor(init: {
    $fstage: Stage,
    $fbinding: FrameBinding,
    $fpayload: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fstage: __zeroValue(this.$fstage), $fbinding: __zeroValue(this.$fbinding), $fpayload: __zeroValue(this.$fpayload) }) as this;
  }

  static decode(input: bigint[], max_payload: bigint): Result<Frame, FrameError>
  {
    if ((BigInt(input.length) < HEADER_BYTES))     {
      return new FrameError_Truncated();
    }
    if ((!__equals(input.slice(0, Number(8n)), MAGIC) || !__equals(u16.from_le_bytes([input[Number(8n)], input[Number(9n)]]), VERSION)))     {
      return new FrameError_UnsupportedFormat();
    }
    const stage = new Stage_try_from(input[Number(10n)]);
    let offset = 11n;
    let take_32 = () => (() => {
  let value = Array.from({length: Number(32n)}, () => 0n);
  (value).splice(0, (input.slice(Number(offset), Number(fieldAdd(offset, 32n)))).length, ...(input.slice(Number(offset), Number(fieldAdd(offset, 32n)))));
  offset = fieldAdd(offset, 32n);
  return value;
})();
    const parameter_fingerprint = take_32();
    const session_id = take_32();
    const manifest_digest = take_32();
    const use_counter = u64_from_le_bytes((input.slice(Number(offset), Number(fieldAdd(offset, 8n))))!);
    offset = fieldAdd(offset, 8n);
    const payload_len = Number(u32_from_le_bytes((input.slice(Number(offset), Number(fieldAdd(offset, 4n))))!));
    offset = fieldAdd(offset, 4n);
    if ((payload_len > max_payload))     {
      return new FrameError_PayloadTooLarge();
    }
    if (!__equals(BigInt(input.length), (offset + (payload_len))))     {
      return new FrameError_LengthMismatch();
    }
    return new Frame({ $fstage: stage, $fbinding: new FrameBinding({ $fparameter_fingerprint: parameter_fingerprint, $fsession_id: session_id, $fmanifest_digest: manifest_digest, $fuse_counter: use_counter }), $fpayload: [...input.slice(Number(offset))] });
  }

  encode(): Vec<bigint>
  {
    let out = ([] as any[]);
    out.push(...(MAGIC));
    out.push(...([(VERSION) & 0xFFn, ((VERSION) >> 8n) & 0xFFn, ((VERSION) >> 16n) & 0xFFn, ((VERSION) >> 24n) & 0xFFn]));
    (out).push(((this.$fstage) & 0xFFn));
    out.push(...(this.$fbinding.$fparameter_fingerprint));
    out.push(...(this.$fbinding.$fsession_id));
    out.push(...(this.$fbinding.$fmanifest_digest));
    out.push(...([(this.$fbinding.$fuse_counter) & 0xFFn, ((this.$fbinding.$fuse_counter) >> 8n) & 0xFFn, ((this.$fbinding.$fuse_counter) >> 16n) & 0xFFn, ((this.$fbinding.$fuse_counter) >> 24n) & 0xFFn]));
    out.push(...([(Number(BigInt(this.$fpayload.length))) & 0xFFn, ((Number(BigInt(this.$fpayload.length))) >> 8n) & 0xFFn, ((Number(BigInt(this.$fpayload.length))) >> 16n) & 0xFFn, ((Number(BigInt(this.$fpayload.length))) >> 24n) & 0xFFn]));
    out.push(...(this.$fpayload));
    return out;
  }
}

export class EvalDyn {
  $fn!: bigint;
  $ftarget!: bigint[];

  constructor(init: {
    $fn: bigint,
    $ftarget: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $ftarget: __zeroValue(this.$ftarget) }) as this;
  }

  and_via_table(ctx: { newD: () => any }, other: EvalDyn, table: GarbleTableDyn): EvalDyn
  {
    const n: bigint = this.$fn;
    const index = fieldBitor((() => { if (__equals(fieldBitand(this.$ftarget[Number(0n)], 1n), 1n)) {
  return 1n;
} else {
  return 0n;
} })(), (() => { if (__equals(fieldBitand(other.$ftarget[Number(0n)], 1n), 1n)) {
  return 2n;
} else {
  return 0n;
} })());
    const hash = (() => {
  let d = ctx.newD();
  d.update(this.$ftarget);
  d.update(other.$ftarget);
  return [...d.finalize()];
})();
    return new EvalDyn({ $ftarget: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(hash[Number(i)], table.$ftable[Number(index)][Number(i)])), $fn: 0n });
  }

  bitxor(rhs: EvalDyn): EvalDyn
  {
    const n: bigint = this.$fn;
    return new EvalDyn({ $ftarget: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(this.$ftarget[Number(i)], rhs.$ftarget[Number(i)])), $fn: 0n });
  }

  open(garble: GarbleDyn): bigint[]
  {
    const n: bigint = this.$fn;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(this.$ftarget[Number(i)], garble.$fbase[Number(i)]));
  }

  to_share(o: bigint): EvalDyn
  {
    const n: bigint = this.$fn;
    return new EvalDyn({ $ftarget: Array.from({length: Number(o - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let v = 0n;
  for (let j = 0n; j < 8n; j += 1n)   {
    const bit = fieldBitand(this.$ftarget[Number(fieldAdd(fieldMul(i, 8n), j))], 1n);
    v = fieldBitor(v, fieldShl(bit, j));
  }
  return v;
})()), $fn: 0n });
  }

  static zero(n: bigint): EvalDyn
  {
    return new EvalDyn({ $ftarget: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n), $fn: 0n });
  }
}

export class GarbleDyn {
  $fn!: bigint;
  $fbase!: bigint[];

  constructor(init: {
    $fn: bigint,
    $fbase: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fbase: __zeroValue(this.$fbase) }) as this;
  }

  and_result(ctx: { newD: () => any }, b: GarbleDyn): GarbleDyn
  {
    const n: bigint = this.$fn;
    let d = ctx.newD();
    d.update(this.$fbase);
    d.update(b.$fbase);
    const hash = [...d.finalize()];
    return new GarbleDyn({ $fbase: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => hash[Number(i)]), $fn: 0n });
  }

  share(target: bigint[]): EvalDyn
  {
    const n: bigint = this.$fn;
    return new EvalDyn({ $ftarget: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(this.$fbase[Number(i)], target[Number(i)])), $fn: 0n });
  }

  to_share(o: bigint): GarbleDyn
  {
    const n: bigint = this.$fn;
    return new GarbleDyn({ $fbase: Array.from({length: Number(o - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let v = 0n;
  for (let j = 0n; j < 8n; j += 1n)   {
    const bit = fieldBitand(this.$fbase[Number(fieldAdd(fieldMul(i, 8n), j))], 1n);
    v = fieldBitor(v, fieldShl(bit, j));
  }
  return v;
})()), $fn: 0n });
  }

  static zero(n: bigint): GarbleDyn
  {
    return new GarbleDyn({ $fbase: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n), $fn: 0n });
  }
}

export class GarbleTableDyn {
  $fn!: bigint;
  $ftable!: bigint[][];

  constructor(init: {
    $fn: bigint,
    $ftable: bigint[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $ftable: __zeroValue(this.$ftable) }) as this;
  }
}

export class GlobalSecretDyn {
  $fn!: bigint;
  $fsecret!: bigint[];

  constructor(init: {
    $fn: bigint,
    $fsecret: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fsecret: __zeroValue(this.$fsecret) }) as this;
  }

  encode(garble: GarbleDyn, value: boolean): EvalDyn
  {
    const n: bigint = this.$fn;
    return new EvalDyn({ $ftarget: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return (() => { if (value) {
  return fieldBitxor(this.$fsecret[Number(i)], garble.$fbase[Number(i)]);
} else {
  return garble.$fbase[Number(i)];
} })();
})()), $fn: 0n });
  }

  gen_and_table(ctx: { newD: () => any, defaultT: () => any }, a: GarbleDyn, b: GarbleDyn): GarbleTableDyn
  {
    const n: bigint = this.$fn;
    const result_base = a.and_result(ctx, b);
    let table = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
    for (let i = 0n; i < 4n; i += 1n)     {
      const av = !__equals(fieldBitand(i, 1n), 0n);
      const bv = !__equals(fieldBitand(i, 2n), 0n);
      const ea = this.encode(a, av);
      const eb = this.encode(b, bv);
      const row = fieldBitor(Number(fieldBitand(ea.$ftarget[Number(0n)], 1n)), fieldShl(Number(fieldBitand(eb.$ftarget[Number(0n)], 1n)), 1n));
      const result_label = this.encode(result_base, fieldBitand(av, bv));
      let d = ctx.newD();
      d.update(ea.$ftarget);
      d.update(eb.$ftarget);
      const hash = [...d.finalize()];
      table[Number(row)] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => fieldBitxor(hash[Number(j)], result_label.$ftarget[Number(j)]));
    }
    return new GarbleTableDyn({ $ftable: table, $fn: 0n });
  }

  static new(n: bigint, secret: bigint[]): GlobalSecretDyn
  {
    secret[Number(0n)] = fieldBitor(secret[Number(0n)], 1n);
    return new GlobalSecretDyn({ $fsecret: secret });
  }

  not_garble(a: GarbleDyn): GarbleDyn
  {
    const n: bigint = this.$fn;
    return new GarbleDyn({ $fbase: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a.$fbase[Number(i)], this.$fsecret[Number(i)])), $fn: 0n });
  }

  one_wire_eval(ctx: { defaultT: () => any }): EvalDyn
  {
    const n: bigint = this.$fn;
    return this.encode(GarbleDyn.zero(), true);
  }

  secret(): bigint[]
  {
    const n: bigint = this.$fn;
    return __clone(this.$fsecret);
  }
}

export class GarbledCircuitDyn {
  $fn!: bigint;
  $fi!: bigint;
  $fa!: bigint;
  $fsecret!: GlobalSecretDyn;
  $finput_labels!: GarbleDyn[];
  $ftables!: GarbleTableDyn[];
  $foutput_label!: GarbleDyn;

  constructor(init: {
    $fn: bigint,
    $fi: bigint,
    $fa: bigint,
    $fsecret: GlobalSecretDyn,
    $finput_labels: GarbleDyn[],
    $ftables: GarbleTableDyn[],
    $foutput_label: GarbleDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fi: __zeroValue(this.$fi), $fa: __zeroValue(this.$fa), $fsecret: __zeroValue(this.$fsecret), $finput_labels: __zeroValue(this.$finput_labels), $ftables: __zeroValue(this.$ftables), $foutput_label: __zeroValue(this.$foutput_label) }) as this;
  }

  encode_inputs(ctx: { defaultT: () => any }, bits: boolean[]): EvalDyn[]
  {
    const n: bigint = this.$fn;
    const i: bigint = this.$fi;
    const a: bigint = this.$fa;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => this.$fsecret.encode(this.$finput_labels[Number(i)], bits[Number(i)]));
  }

  eval_setup(ctx: { defaultT: () => any }): EvalSetupDyn
  {
    const n: bigint = this.$fn;
    const i: bigint = this.$fi;
    const a: bigint = this.$fa;
    return new EvalSetupDyn({ $fone_wire: this.$fsecret.one_wire_eval(ctx, ), $ftables: __clone(this.$ftables), $foutput_label: __clone(this.$foutput_label), $fn: 0n, $fa: 0n });
  }
}

export class EvalSetupDyn {
  $fn!: bigint;
  $fa!: bigint;
  $fone_wire!: EvalDyn;
  $ftables!: GarbleTableDyn[];
  $foutput_label!: GarbleDyn;

  constructor(init: {
    $fn: bigint,
    $fa: bigint,
    $fone_wire: EvalDyn,
    $ftables: GarbleTableDyn[],
    $foutput_label: GarbleDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fa: __zeroValue(this.$fa), $fone_wire: __zeroValue(this.$fone_wire), $ftables: __zeroValue(this.$ftables), $foutput_label: __zeroValue(this.$foutput_label) }) as this;
  }

  recover_output(ctx: { newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, result: EvalDyn): boolean
  {
    const n: bigint = this.$fn;
    const a: bigint = this.$fa;
    return !__equals(fieldBitand(result.open(this.$foutput_label.$ft, this.$foutput_label.$fu, this.$foutput_label.$fm, this.$foutput_label)[Number(0n)], 1n), 0n);
  }
}

export class LweCiphertextDyn {
  $fn_lwe!: bigint;
  $fa!: bigint[];
  $fb!: bigint;

  constructor(init: {
    $fn_lwe: bigint,
    $fa: bigint[],
    $fb: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fa: __zeroValue(this.$fa), $fb: __zeroValue(this.$fb) }) as this;
  }
}

export class RlweCiphertextDyn {
  $fbig_n!: bigint;
  $fa!: bigint[];
  $fb!: bigint[];

  constructor(init: {
    $fbig_n: bigint,
    $fa: bigint[],
    $fb: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fbig_n: __zeroValue(this.$fbig_n), $fa: __zeroValue(this.$fa), $fb: __zeroValue(this.$fb) }) as this;
  }
}

export class RgswRowDyn {
  $fbig_n!: bigint;
  $frlwe0!: RlweCiphertextDyn;
  $frlwe1!: RlweCiphertextDyn;

  constructor(init: {
    $fbig_n: bigint,
    $frlwe0: RlweCiphertextDyn,
    $frlwe1: RlweCiphertextDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fbig_n: __zeroValue(this.$fbig_n), $frlwe0: __zeroValue(this.$frlwe0), $frlwe1: __zeroValue(this.$frlwe1) }) as this;
  }
}

export class RgswCiphertextDyn {
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $frows!: RgswRowDyn[];

  constructor(init: {
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $frows: RgswRowDyn[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $frows: __zeroValue(this.$frows) }) as this;
  }
}

export class KeySwitchingKeyDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fks_ell!: bigint;
  $fks_bg_log!: bigint;
  $fksk!: LweCiphertextDyn[][];

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fks_ell: bigint,
    $fks_bg_log: bigint,
    $fksk: LweCiphertextDyn[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fks_ell: __zeroValue(this.$fks_ell), $fks_bg_log: __zeroValue(this.$fks_bg_log), $fksk: __zeroValue(this.$fksk) }) as this;
  }
}

export class BootstrappingKeyDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $fks_ell!: bigint;
  $fbs_bg_log!: bigint;
  $fks_bg_log!: bigint;
  $fbsk!: RgswCiphertextDyn[];
  $fksk!: KeySwitchingKeyDyn;

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $fks_ell: bigint,
    $fbs_bg_log: bigint,
    $fks_bg_log: bigint,
    $fbsk: RgswCiphertextDyn[],
    $fksk: KeySwitchingKeyDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $fks_ell: __zeroValue(this.$fks_ell), $fbs_bg_log: __zeroValue(this.$fbs_bg_log), $fks_bg_log: __zeroValue(this.$fks_bg_log), $fbsk: __zeroValue(this.$fbsk), $fksk: __zeroValue(this.$fksk) }) as this;
  }
}

export class LweSecretKeyDyn {
  $fn_lwe!: bigint;
  $fkey!: bigint[];

  constructor(init: {
    $fn_lwe: bigint,
    $fkey: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fkey: __zeroValue(this.$fkey) }) as this;
  }
}

export class RlweSecretKeyDyn {
  $fbig_n!: bigint;
  $fkey!: bigint[];

  constructor(init: {
    $fbig_n: bigint,
    $fkey: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fbig_n: __zeroValue(this.$fbig_n), $fkey: __zeroValue(this.$fkey) }) as this;
  }
}

export class TfheBootstrapTableDyn {
  $faddr_bits!: bigint;
  $ftable_len!: bigint;
  $fbig_n!: bigint;
  $flogical!: boolean[];
  $ftest_poly!: bigint[];
  $fis_constant!: boolean;

  constructor(init: {
    $faddr_bits: bigint,
    $ftable_len: bigint,
    $fbig_n: bigint,
    $flogical: boolean[],
    $ftest_poly: bigint[],
    $fis_constant: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $faddr_bits: __zeroValue(this.$faddr_bits), $ftable_len: __zeroValue(this.$ftable_len), $fbig_n: __zeroValue(this.$fbig_n), $flogical: __zeroValue(this.$flogical), $ftest_poly: __zeroValue(this.$ftest_poly), $fis_constant: __zeroValue(this.$fis_constant) }) as this;
  }

  entries(): boolean[]
  {
    const addr_bits: bigint = this.$faddr_bits;
    const table_len: bigint = this.$ftable_len;
    const big_n: bigint = this.$fbig_n;
    return this.$flogical;
  }

  static new(addr_bits: bigint, table_len: bigint, big_n: bigint, logical: boolean[]): Result<TfheBootstrapTableDyn, TfheBootstrapTableError>
  {
    const max_addr_bits = fieldSub(Number(usize.BITS), 1n);
    if ((__equals(addr_bits, 0n) || (addr_bits > max_addr_bits)))     {
      return new TfheBootstrapTableError_AddressWidthOutOfRange();
    }
    if ((addr_bits > 2n))     {
      return new TfheBootstrapTableError_InputEncodingUnsupported();
    }
    const domain = fieldShl(1n, addr_bits);
    if (!__equals(table_len, domain))     {
      return new TfheBootstrapTableError_TableLengthMismatch();
    }
    const capacity = (() => { const __match = ((big_n) * (2n)); if (__match !== null && __match !== undefined) { const capacity = __match;
return capacity; } else { return new TfheBootstrapTableError_RingCapacityExceeded(); } })();
    if ((((table_len > capacity) || __equals(big_n, 0n)) || !((big_n) > 0n && ((big_n) & ((big_n) - 1n)) === 0n)))     {
      return new TfheBootstrapTableError_RingCapacityExceeded();
    }
    let is_constant = true;
    let index = 1n;
    while ((index < table_len))     {
      if (!__equals(logical[Number(index)], logical[Number(0n)]))       {
        is_constant = false;
        break;
      }
      index = fieldAdd(index, 1n);
    }
    if (!is_constant)     {
      const half = (table_len / 2n);
      index = 0n;
      while ((index < half))       {
        if (__equals(logical[Number(index)], logical[Number(fieldAdd(index, half))]))         {
          return new TfheBootstrapTableError_NegacyclicIncompatible();
        }
        index = fieldAdd(index, 1n);
      }
    }
    const half_q4 = fieldShr(Q4, 1n);
    const poly_step = (big_n / (table_len / 2n));
    let test_poly = Array.from({length: Number(big_n)}, () => 0n);
    index = 0n;
    while ((index < big_n))     {
      const entry = (index / poly_step);
      test_poly[Number(index)] = (() => { if (logical[Number(entry)]) {
  return half_q4;
} else {
  return wrappingNeg(half_q4, 32);
} })();
      index = fieldAdd(index, 1n);
    }
    return new TfheBootstrapTableDyn({ $flogical: logical, $ftest_poly: test_poly, $fis_constant: is_constant });
  }
}

export class AllPartiesDyn<T> {
  $fn!: bigint;
  $fother_parties!: OtherPartiesDyn<T>;
  $fself_party!: T;

  constructor(init: {
    $fn: bigint,
    $fother_parties: OtherPartiesDyn<T>,
    $fself_party: T
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fother_parties: __zeroValue(this.$fother_parties), $fself_party: __zeroValue(this.$fself_party) }) as this;
  }
}

export class OtherPartiesDyn<T> {
  $fn!: bigint;
  $fother_parties!: T[];

  constructor(init: {
    $fn: bigint,
    $fother_parties: T[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fother_parties: __zeroValue(this.$fother_parties) }) as this;
  }
}

export class IknpUMsg {
  $fu_cols!: Vec<Vec<boolean>>;

  constructor(init: {
    $fu_cols: Vec<Vec<boolean>>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu_cols: __zeroValue(this.$fu_cols) }) as this;
  }
}

export class SenderLpn {
  $fseed_q!: Vec<bigint[]>;
  $femit!: bigint[][];

  constructor(init: {
    $fseed_q: Vec<bigint[]>,
    $femit: bigint[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fseed_q: __zeroValue(this.$fseed_q), $femit: __zeroValue(this.$femit) }) as this;
  }
}

export class RecvLpn {
  $fseed!: FerretReceiverSeed;
  $fx!: Vec<boolean>;
  $fz!: Vec<bigint[]>;

  constructor(init: {
    $fseed: FerretReceiverSeed,
    $fx: Vec<boolean>,
    $fz: Vec<bigint[]>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fseed: __zeroValue(this.$fseed), $fx: __zeroValue(this.$fx), $fz: __zeroValue(this.$fz) }) as this;
  }
}

export class SoftSpokenOutLoweredDyn<D> {
  $fm!: bigint;
  $fl!: bigint;
  $fsender_r0!: bigint[][];
  $freceiver_v!: bigint[][];
  $fsender_tag!: bigint[];
  $freceiver_tag!: bigint[];

  constructor(init: {
    $fm: bigint,
    $fl: bigint,
    $fsender_r0: bigint[][],
    $freceiver_v: bigint[][],
    $fsender_tag: bigint[],
    $freceiver_tag: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fm: __zeroValue(this.$fm), $fl: __zeroValue(this.$fl), $fsender_r0: __zeroValue(this.$fsender_r0), $freceiver_v: __zeroValue(this.$freceiver_v), $fsender_tag: __zeroValue(this.$fsender_tag), $freceiver_tag: __zeroValue(this.$freceiver_tag) }) as this;
  }

  check(): boolean
  {
    const m: bigint = this.$fm;
    const l: bigint = this.$fl;
    return __equals(this.$fsender_tag, this.$freceiver_tag);
  }
}

export class SoftSpokenOutDynDyn<D> {
  $fl!: bigint;
  $fsender_r0!: Vec<bigint[]>;
  $freceiver_v!: Vec<bigint[]>;
  $fsender_tag!: bigint[];
  $freceiver_tag!: bigint[];

  constructor(init: {
    $fl: bigint,
    $fsender_r0: Vec<bigint[]>,
    $freceiver_v: Vec<bigint[]>,
    $fsender_tag: bigint[],
    $freceiver_tag: bigint[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fl: __zeroValue(this.$fl), $fsender_r0: __zeroValue(this.$fsender_r0), $freceiver_v: __zeroValue(this.$freceiver_v), $fsender_tag: __zeroValue(this.$fsender_tag), $freceiver_tag: __zeroValue(this.$freceiver_tag) }) as this;
  }

  check(): boolean
  {
    const l: bigint = this.$fl;
    return __equals(this.$fsender_tag, this.$freceiver_tag);
  }
}

export class ChouOrlandiDyn<G, D> {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static recv_finish<D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, state: number /* ChouOrlandiDyn<G, D>::ReceiverState */, payload: number /* ChouOrlandiDyn<G, D>::PayloadMsg */): bigint[]
  {
    const kc = ot_recv_finish(ctx, state.$finner);
    const chosen = (() => { if (state.inner_choice()) {
  return payload[1];
} else {
  return payload[0];
} })();
    let mc = Array.from({length: Number(l)}, () => 0n);
    ot_recv_payload(kc, chosen, mc);
    return mc;
  }

  static recv_start<R, D>(ctx: { GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, rng: any, setup: number /* ChouOrlandiDyn<G, D>::SetupMsg */, c: boolean): [number /* ChouOrlandiDyn<G, D>::ReceiverState */, number /* ChouOrlandiDyn<G, D>::RecvMsg */]
  {
    const [inner, msg] = ot_recv(ctx, rng, __clone(setup), c);
    return [new ChouOrlandiRecvDyn({ $finner: inner }), msg];
  }

  static sender_payload<R, D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, _rng: any, state: number /* ChouOrlandiDyn<G, D>::SenderState */, recv_msg: number /* ChouOrlandiDyn<G, D>::RecvMsg */, m0: bigint[], m1: bigint[]): number /* ChouOrlandiDyn<G, D>::PayloadMsg */
  {
    const [k0, k1] = ot_send_finish(ctx, state, recv_msg);
    let e0 = Array.from({length: Number(l)}, () => 0n);
    let e1 = Array.from({length: Number(l)}, () => 0n);
    ot_send_payload(k0, k1, m0, m1, e0, e1);
    return [e0, e1];
  }

  static sender_setup<R, D>(ctx: { GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, rng: any): [number /* ChouOrlandiDyn<G, D>::SenderState */, number /* ChouOrlandiDyn<G, D>::SetupMsg */]
  {
    return ot_send_setup(ctx, rng);
  }
}

export class ChouOrlandiRecvDyn<G, D> {
  $finner!: BaseOtReceiverDyn<G, D>;

  constructor(init: {
    $finner: BaseOtReceiverDyn<G, D>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $finner: __zeroValue(this.$finner) }) as this;
  }

  inner_choice(): boolean
  {
    return ot_recv_choice(this.$finner);
  }
}

export class LweOtCrsDyn {
  $fn!: bigint;
  $fa!: Zq[][];
  $fh!: Zq[];

  constructor(init: {
    $fn: bigint,
    $fa: Zq[][],
    $fh: Zq[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fa: __zeroValue(this.$fa), $fh: __zeroValue(this.$fh) }) as this;
  }

  static sample<R>(n: bigint, rng: any): LweOtCrsDyn
  {
    let a = Array.from({length: Number(n)}, () => Array.from({length: Number(n)}, () => 0n));
    for (let i = 0n; i < n; i += 1n)     {
      for (let j = 0n; j < n; j += 1n)       {
        a[Number(i)][Number(j)] = sample_zq(rng);
      }
    }
    let h = Array.from({length: Number(n)}, () => 0n);
    for (let i = 0n; i < n; i += 1n)     {
      h[Number(i)] = sample_zq(rng);
    }
    return new LweOtCrsDyn({ $fa: a, $fh: h });
  }
}

export class LweOtReceiverDyn {
  $fn!: bigint;
  $fs!: Zq[];
  $fc!: boolean;

  constructor(init: {
    $fn: bigint,
    $fs: Zq[],
    $fc: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fs: __zeroValue(this.$fs), $fc: __zeroValue(this.$fc) }) as this;
  }
}

export class LweOtRecvMsgDyn {
  $fn!: bigint;
  $fpk0!: Zq[];

  constructor(init: {
    $fn: bigint,
    $fpk0: Zq[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fpk0: __zeroValue(this.$fpk0) }) as this;
  }
}

export class LweOtSenderMsgLoweredDyn {
  $fn!: bigint;
  $fl!: bigint;
  $fu0!: Zq[];
  $fv0!: Zq[];
  $fu1!: Zq[];
  $fv1!: Zq[];

  constructor(init: {
    $fn: bigint,
    $fl: bigint,
    $fu0: Zq[],
    $fv0: Zq[],
    $fu1: Zq[],
    $fv1: Zq[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fl: __zeroValue(this.$fl), $fu0: __zeroValue(this.$fu0), $fv0: __zeroValue(this.$fv0), $fu1: __zeroValue(this.$fu1), $fv1: __zeroValue(this.$fv1) }) as this;
  }
}

export class LweOtSenderMsgDyn {
  $fu0!: Vec<Zq>;
  $fv0!: Vec<Zq>;
  $fu1!: Vec<Zq>;
  $fv1!: Vec<Zq>;

  constructor(init: {
    $fu0: Vec<Zq>,
    $fv0: Vec<Zq>,
    $fu1: Vec<Zq>,
    $fv1: Vec<Zq>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu0: __zeroValue(this.$fu0), $fv0: __zeroValue(this.$fv0), $fu1: __zeroValue(this.$fu1), $fv1: __zeroValue(this.$fv1) }) as this;
  }
}

export class LweBaseOtDyn {
  $fn!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  static recv_finish(n: bigint, l: bigint, state: number /* LweBaseOtDyn::ReceiverState */, payload: number /* LweBaseOtDyn::PayloadMsg */): bigint[]
  {
    const bytes = lwe_ot_recv_decrypt_bytes(n, state, payload, l);
    let out = Array.from({length: Number(l)}, () => 0n);
    (out).splice(0, (bytes).length, ...(bytes));
    return out;
  }

  static recv_start<R>(n: bigint, l: bigint, rng: any, setup: number /* LweBaseOtDyn::SetupMsg */, c: boolean): [number /* LweBaseOtDyn::ReceiverState */, number /* LweBaseOtDyn::RecvMsg */]
  {
    return lwe_ot_recv(n, rng, setup, c);
  }

  static sender_payload<R>(n: bigint, l: bigint, rng: any, state: number /* LweBaseOtDyn::SenderState */, recv_msg: number /* LweBaseOtDyn::RecvMsg */, m0: bigint[], m1: bigint[]): number /* LweBaseOtDyn::PayloadMsg */
  {
    return lwe_ot_send_bytes(n, rng, state, recv_msg, m0, m1);
  }

  static sender_setup<R>(n: bigint, l: bigint, rng: any): [number /* LweBaseOtDyn::SenderState */, number /* LweBaseOtDyn::SetupMsg */]
  {
    const crs = LweOtCrsDyn.sample(rng);
    return [__clone(crs), crs];
  }
}

export class SpcotSenderMsg {
  $fms!: Vec<Block[]>;
  $fc!: Block;
  $fhash_v!: Vec<bigint>;

  constructor(init: {
    $fms: Vec<Block[]>,
    $fc: Block,
    $fhash_v: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fms: __zeroValue(this.$fms), $fc: __zeroValue(this.$fc), $fhash_v: __zeroValue(this.$fhash_v) }) as this;
  }
}

export class MpcotUniSenderMsg {
  $fhash_seed!: bigint[];
  $fblocks!: Vec<SpcotSenderMsg>;

  constructor(init: {
    $fhash_seed: bigint[],
    $fblocks: Vec<SpcotSenderMsg>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fhash_seed: __zeroValue(this.$fhash_seed), $fblocks: __zeroValue(this.$fblocks) }) as this;
  }
}

export class FerretParams {
  $fn!: bigint;
  $fk!: bigint;
  $ft!: bigint;

  constructor(init: {
    $fn: bigint,
    $fk: bigint,
    $ft: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fk: __zeroValue(this.$fk), $ft: __zeroValue(this.$ft) }) as this;
  }

  log_splen(): bigint
  {
    const s = this.splen();
    return Number(Math.clz32((s) & -((s) | 0)));
  }

  output_cot_count(malicious: boolean): bigint
  {
    return (this.$fn - (this.seed_cot_count(malicious)));
  }

  seed_cot_count(malicious: boolean): bigint
  {
    const body = fieldAdd(this.$fk, fieldMul(this.$ft, this.log_splen()));
    return (() => { if (malicious) {
  return fieldAdd(body, KAPPA_BITS);
} else {
  return body;
} })();
  }

  splen(): bigint
  {
    return (this.$fn / this.$ft);
  }
}

export class CotPoolSender {
  $fparams!: FerretParams;
  $fseed!: FerretSenderSeed;
  $fout!: Block[];
  $fraise_n!: (bigint | undefined);

  constructor(init: {
    $fparams: FerretParams,
    $fseed: FerretSenderSeed,
    $fout: Block[],
    $fraise_n: (bigint | undefined)
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparams: __zeroValue(this.$fparams), $fseed: __zeroValue(this.$fseed), $fout: __zeroValue(this.$fout), $fraise_n: __zeroValue(this.$fraise_n) }) as this;
  }

  remaining(): bigint
  {
    return BigInt(this.$fout.length);
  }
}

export class CotPoolReceiver {
  $fparams!: FerretParams;
  $fseed!: FerretReceiverSeed;
  $fout_x!: boolean[];
  $fout_z!: Block[];

  constructor(init: {
    $fparams: FerretParams,
    $fseed: FerretReceiverSeed,
    $fout_x: boolean[],
    $fout_z: Block[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparams: __zeroValue(this.$fparams), $fseed: __zeroValue(this.$fseed), $fout_x: __zeroValue(this.$fout_x), $fout_z: __zeroValue(this.$fout_z) }) as this;
  }

  remaining(): bigint
  {
    return BigInt(this.$fout_x.length);
  }
}

export class MpcotRegSenderMsg {
  $fblocks!: Vec<SpcotSenderMsg>;

  constructor(init: {
    $fblocks: Vec<SpcotSenderMsg>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fblocks: __zeroValue(this.$fblocks) }) as this;
  }
}

export class FerretIterMsg {
  $flpn_seed!: bigint[];
  $fchoices!: Vec<boolean>;
  $fmpcot!: MpcotRegSenderMsg;

  constructor(init: {
    $flpn_seed: bigint[],
    $fchoices: Vec<boolean>,
    $fmpcot: MpcotRegSenderMsg
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flpn_seed: __zeroValue(this.$flpn_seed), $fchoices: __zeroValue(this.$fchoices), $fmpcot: __zeroValue(this.$fmpcot) }) as this;
  }
}

export class FerretSenderSeed {
  $fdelta!: Block;
  $fq!: Vec<Block>;

  constructor(init: {
    $fdelta: Block,
    $fq: Vec<Block>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdelta: __zeroValue(this.$fdelta), $fq: __zeroValue(this.$fq) }) as this;
  }
}

export class FerretReceiverSeed {
  $fu!: Vec<boolean>;
  $fw!: Vec<Block>;

  constructor(init: {
    $fu: Vec<boolean>,
    $fw: Vec<Block>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu: __zeroValue(this.$fu), $fw: __zeroValue(this.$fw) }) as this;
  }
}

export class FerretExtendOut {
  $fsender_out!: Vec<Block>;
  $frecv_x!: Vec<boolean>;
  $frecv_z!: Vec<Block>;
  $fsender_seed!: FerretSenderSeed;
  $freceiver_seed!: FerretReceiverSeed;

  constructor(init: {
    $fsender_out: Vec<Block>,
    $frecv_x: Vec<boolean>,
    $frecv_z: Vec<Block>,
    $fsender_seed: FerretSenderSeed,
    $freceiver_seed: FerretReceiverSeed
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsender_out: __zeroValue(this.$fsender_out), $frecv_x: __zeroValue(this.$frecv_x), $frecv_z: __zeroValue(this.$frecv_z), $fsender_seed: __zeroValue(this.$fsender_seed), $freceiver_seed: __zeroValue(this.$freceiver_seed) }) as this;
  }
}

export class FerretPrep {
  $falphas!: Vec<bigint>;
  $fe!: Vec<boolean>;
  $flpn_seed!: bigint[];
  $fchoices!: Vec<boolean>;

  constructor(init: {
    $falphas: Vec<bigint>,
    $fe: Vec<boolean>,
    $flpn_seed: bigint[],
    $fchoices: Vec<boolean>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $falphas: __zeroValue(this.$falphas), $fe: __zeroValue(this.$fe), $flpn_seed: __zeroValue(this.$flpn_seed), $fchoices: __zeroValue(this.$fchoices) }) as this;
  }
}

export class BaseOtSenderDyn<G, D> {
  $fy!: number /* G::Scalar */;
  $fs!: number /* G::Element */;
  $ft!: number /* G::Element */;

  constructor(init: {
    $fy: number /* G::Scalar */,
    $fs: number /* G::Element */,
    $ft: number /* G::Element */
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fy: __zeroValue(this.$fy), $fs: __zeroValue(this.$fs), $ft: __zeroValue(this.$ft) }) as this;
  }
}

export class BaseOtReceiverDyn<G, D> {
  $fx!: number /* G::Scalar */;
  $fs!: number /* G::Element */;
  $fc!: boolean;

  constructor(init: {
    $fx: number /* G::Scalar */,
    $fs: number /* G::Element */,
    $fc: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fx: __zeroValue(this.$fx), $fs: __zeroValue(this.$fs), $fc: __zeroValue(this.$fc) }) as this;
  }
}

export class OtReceiverMsgDyn<G> {
  $fr!: number /* G::Element */;

  constructor(init: {
    $fr: number /* G::Element */
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fr: __zeroValue(this.$fr) }) as this;
  }
}

export class IdealCotDyn<T> {
  $fn!: bigint;
  $fdelta!: DeltaDyn<T>;

  constructor(init: {
    $fn: bigint,
    $fdelta: DeltaDyn<T>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fdelta: __zeroValue(this.$fdelta) }) as this;
  }

  cot<R>(rng: any, sample_t: any, bit: boolean)
  {
    const n: bigint = this.$fn;
    return IdealCotDyn.cot(this, rng, sample_t, bit);
  }

  static new<T>(n: bigint, delta: DeltaDyn<T>): IdealCotDyn<T>
  {
    return new IdealCotDyn<T>({ $fdelta: delta });
  }
}

export class OtStack {
  $fsender!: CotPoolSender;
  $freceiver!: CotPoolReceiver;

  constructor(init: {
    $fsender: CotPoolSender,
    $freceiver: CotPoolReceiver
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsender: __zeroValue(this.$fsender), $freceiver: __zeroValue(this.$freceiver) }) as this;
  }

  commit_bits<R>(rng: any, bits: boolean[]): Vec<[VopeDyn<Galois128>, QDyn<Galois128>]>
  {
    const [r0s, xs, zs] = take_random(rng, this.$fsender, this.$freceiver, BigInt(bits.length));
    const delta = this.$fsender.$fseed.$fdelta;
    let out = ([] as any[]);
    for (let j = 0n; j < BigInt(bits.length); j += 1n)     {
      const [r0, z, _d] = bea95_chosen_bit(delta, r0s[Number(j)], xs[Number(j)], zs[Number(j)], bits[Number(j)]);
      const r0_t = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => new Galois128(u128_from_le_bytes(r0)));
      const v_t = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => new Galois128(u128_from_le_bytes(z)));
      (out).push(vole_commit_bit_shares(r0_t, v_t, bit_to_g128, bits[Number(j)]));
    }
    return out;
  }

  cot<R>(rng: any, _sample_t: any, bit: boolean): [Galois128[], Galois128[]]
  {
    const [r0s, xs, zs] = take_random(rng, this.$fsender, this.$freceiver, 1n);
    const [r0, z, _d] = bea95_chosen_bit(this.$fsender.$fseed.$fdelta, r0s[Number(0n)], xs[Number(0n)], zs[Number(0n)], bit);
    const r0_t = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => new Galois128(u128_from_le_bytes(r0)));
    const v_t = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => new Galois128(u128_from_le_bytes(z)));
    return [r0_t, v_t];
  }

  static from_ideal_seed<R>(rng: any, params: any): OtStack
  {
    const [sender, receiver] = new_pool(rng, params);
    return new OtStack({ $fsender: sender, $freceiver: receiver });
  }

  static setup<R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng_s: any, rng_r: any, params: any): OtStack
  {
    const m = params.seed_cot_count(false);
    let bits = [] as any[];
    for (const b of bits)     {
      b = __equals(fieldBitand(rng_r.next_u32(), 1n), 1n);
    }
    let delta_msg = Array.from({length: Number(16n)}, () => 0n);
    for (const chunk of __chunks(delta_msg, Number(4n)))     {
      (chunk).splice(0, ([(rng_s.next_u32()) & 0xFFn, ((rng_s.next_u32()) >> 8n) & 0xFFn, ((rng_s.next_u32()) >> 16n) & 0xFFn, ((rng_s.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng_s.next_u32()) & 0xFFn, ((rng_s.next_u32()) >> 8n) & 0xFFn, ((rng_s.next_u32()) >> 16n) & 0xFFn, ((rng_s.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
    }
    const out = softspoken_cot_extend_base(ctx, rng_s, rng_r, bits, delta_msg);
    let q = ([] as any[]);
    let w = ([] as any[]);
    for (let j = 0n; j < m; j += 1n)     {
      (q).push(out.$fsender_r0[Number(j)]);
      (w).push(out.$freceiver_v[Number(j)]);
    }
    let stack = new OtStack({ $fsender: new CotPoolSender({ $fparams: params, $fseed: new FerretSenderSeed({ $fdelta: delta_msg, $fq: q }), $fout: [] as any[], $fraise_n: undefined }), $freceiver: new CotPoolReceiver({ $fparams: params, $fseed: new FerretReceiverSeed({ $fu: bits, $fw: w }), $fout_x: [] as any[], $fout_z: [] as any[] }) });
    refill(rng_s, stack.$fsender, stack.$freceiver);
    const _ = rng_r;
    return stack;
  }
}

export class ToyElement {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }
}

export class ToyGroup {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static add(a: any, b: any): ToyElement
  {
    return new ToyElement(toy_mul(a[0], b[0]));
  }

  static generator(): ToyElement
  {
    return new ToyElement(TOY_G);
  }

  static neg(a: any): ToyElement
  {
    return new ToyElement(toy_pow(a[0], fieldSub(TOY_P, 2n)));
  }

  static random_scalar<R>(rng: any): bigint
  {
    const lo = BigInt(rng.next_u32());
    const hi = BigInt(rng.next_u32());
    return (fieldBitor(fieldShl(hi, 32n), lo) % fieldSub(TOY_P, 1n));
  }

  static scalar_mul(elt: any, k: bigint): ToyElement
  {
    return new ToyElement(toy_pow(elt[0], k));
  }

  static write_element<D>(elt: any, h: any)
  {
    h.update([(elt[0]) & 0xFFn, ((elt[0]) >> 8n) & 0xFFn, ((elt[0]) >> 16n) & 0xFFn, ((elt[0]) >> 24n) & 0xFFn]);
  }
}

export class Fe25519 {
  [0]!: bigint[];

  constructor(_0: bigint[]) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: any): Fe25519
  {
    return fe_add(this, rhs);
  }

  is_zero(): boolean
  {
    return __equals(this[0], [0n, 0n, 0n, 0n]);
  }

  mul(rhs: any): Fe25519
  {
    return fe_mul(this, rhs);
  }

  neg(): Fe25519
  {
    return fe_neg(this);
  }

  sub(rhs: any): Fe25519
  {
    return fe_sub(this, rhs);
  }

  to_bytes(): bigint[]
  {
    let out = Array.from({length: Number(32n)}, () => 0n);
    for (let i = 0n; i < 4n; i += 1n)     {
      (out.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).splice(0, ([(this[0][Number(i)]) & 0xFFn, ((this[0][Number(i)]) >> 8n) & 0xFFn, ((this[0][Number(i)]) >> 16n) & 0xFFn, ((this[0][Number(i)]) >> 24n) & 0xFFn]).length, ...([(this[0][Number(i)]) & 0xFFn, ((this[0][Number(i)]) >> 8n) & 0xFFn, ((this[0][Number(i)]) >> 16n) & 0xFFn, ((this[0][Number(i)]) >> 24n) & 0xFFn]));
    }
    return out;
  }
}

export class EdPoint {
  $fx!: Fe25519;
  $fy!: Fe25519;
  $fz!: Fe25519;
  $ft!: Fe25519;

  constructor(init: {
    $fx: Fe25519,
    $fy: Fe25519,
    $fz: Fe25519,
    $ft: Fe25519
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fx: __zeroValue(this.$fx), $fy: __zeroValue(this.$fy), $fz: __zeroValue(this.$fz), $ft: __zeroValue(this.$ft) }) as this;
  }

  static base(): EdPoint
  {
    const x = new Fe25519(BASE_X_LIMBS);
    const y = new Fe25519(BASE_Y_LIMBS);
    return new EdPoint({ $fx: x, $fy: y, $fz: Fe25519.ONE, $ft: fe_mul(x, y) });
  }

  eq(other: any): boolean
  {
    const lhs_x = fe_mul(this.$fx, other.$fz);
    const rhs_x = fe_mul(other.$fx, this.$fz);
    const lhs_y = fe_mul(this.$fy, other.$fz);
    const rhs_y = fe_mul(other.$fy, this.$fz);
    return (__equals(lhs_x, rhs_x) && __equals(lhs_y, rhs_y));
  }

  to_affine(): [Fe25519, Fe25519]
  {
    const zinv = fe_invert(this.$fz);
    return [fe_mul(this.$fx, zinv), fe_mul(this.$fy, zinv)];
  }
}

export class Ed25519 {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static add(a: any, b: any): EdPoint
  {
    return ed_add(a, b);
  }

  static generator(): EdPoint
  {
    return EdPoint.base();
  }

  static neg(a: any): EdPoint
  {
    return ed_neg(a);
  }

  static random_scalar<R>(rng: any): bigint[]
  {
    let k = Array.from({length: Number(32n)}, () => 0n);
    for (let __mut_2 = 0n; __mut_2 < BigInt(k.length); __mut_2 += 1n) {
    {
      k[Number(__mut_2)] = rng.next_u8();
    }}
    k[Number(31n)] = fieldBitand(k[Number(31n)], 63n);
    return k;
  }

  static scalar_mul(elt: any, k: bigint[]): EdPoint
  {
    return ed_scalar_mul(elt, k);
  }

  static write_element<D_>(elt: any, h: any)
  {
    const [x, y] = elt.to_affine();
    h.update(x.to_bytes());
    h.update(y.to_bytes());
  }
}

export class Sponge_Shake128 { constructor(public _0: Shake128) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class Sponge_Shake256 { constructor(public _0: Shake256) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type Sponge = Sponge_Shake128 | Sponge_Shake256;

export class PlanOp_Const { constructor(public $fout: bigint, public $fvalue: boolean) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fout), __zeroValue(this.$fvalue)) as this; }
}
export class PlanOp_Not { constructor(public $finput: bigint, public $fout: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$finput), __zeroValue(this.$fout)) as this; }
}
export class PlanOp_Lut { constructor(public $finputs: LutInputs, public $ftable: bigint, public $fout: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$finputs), __zeroValue(this.$ftable), __zeroValue(this.$fout)) as this; }
}
export class PlanOp_CircuitBootstrap { constructor(public $finput: bigint, public $fout: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$finput), __zeroValue(this.$fout)) as this; }
}
export class PlanOp_RgswMux { constructor(public $fsel: bigint, public $fthen_cell: bigint, public $felse_cell: bigint, public $fout: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fsel), __zeroValue(this.$fthen_cell), __zeroValue(this.$felse_cell), __zeroValue(this.$fout)) as this; }
}
export type PlanOp = PlanOp_Const | PlanOp_Not | PlanOp_Lut | PlanOp_CircuitBootstrap | PlanOp_RgswMux;

export class ProfileId_Toy {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class ProfileId_ToyNoisy {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class ProfileId_Std128 {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class ProfileId_Custom {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type ProfileId = ProfileId_Toy | ProfileId_ToyNoisy | ProfileId_Std128 | ProfileId_Custom;

export class PlanError_BadTableShape { constructor(public $ftable: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$ftable)) as this; }
}
export class PlanError_ArityExceedsKMax { constructor(public $ftable: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$ftable)) as this; }
}
export class PlanError_BadReference {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class PlanError_BadOutput {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class PlanError_BudgetInconsistent {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type PlanError = PlanError_BadTableShape | PlanError_ArityExceedsKMax | PlanError_BadReference | PlanError_BadOutput | PlanError_BudgetInconsistent;

export class EncodeError_InvalidPlan { constructor(public _0: PlanError) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class EncodeError_TooLarge {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type EncodeError = EncodeError_InvalidPlan | EncodeError_TooLarge;

export class DecodeError_BadMagic {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_UnsupportedVersion {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_UnknownTag {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_Truncated {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_TooLarge {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_TrailingBytes {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_InvalidPlan { constructor(public _0: PlanError) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type DecodeError = DecodeError_BadMagic | DecodeError_UnsupportedVersion | DecodeError_UnknownTag | DecodeError_Truncated | DecodeError_TooLarge | DecodeError_TrailingBytes | DecodeError_InvalidPlan;

export class LutError_AddressShapeInvalid {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LutError_ArityExceedsCircuitMax {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LutError_ShapeUnsupported {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type LutError = LutError_AddressShapeInvalid | LutError_ArityExceedsCircuitMax | LutError_ShapeUnsupported;

export class LabelEncodingError_FieldElementOutOfRange { constructor(public $findex: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$findex)) as this; }
}
export class LabelEncodingError_NonCanonicalElement { constructor(public $findex: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$findex)) as this; }
}
export type LabelEncodingError = LabelEncodingError_FieldElementOutOfRange | LabelEncodingError_NonCanonicalElement;

export class LabelBatchPaddingError_TooFewSlots { constructor(public $fslots: bigint, public $fused: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fslots), __zeroValue(this.$fused)) as this; }
}
export class LabelBatchPaddingError_ChoiceLengthMismatch { constructor(public $fexpected: bigint, public $factual: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export type LabelBatchPaddingError = LabelBatchPaddingError_TooFewSlots | LabelBatchPaddingError_ChoiceLengthMismatch;

export class LabelBatchDecodeError_LengthMismatch {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LabelBatchDecodeError_NonCanonicalLabel { constructor(public _0: LabelEncodingError) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type LabelBatchDecodeError = LabelBatchDecodeError_LengthMismatch | LabelBatchDecodeError_NonCanonicalLabel;

export class BatchError_EvenOffset {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class BatchError_MismatchedPair { constructor(public $findex: bigint) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$findex)) as this; }
}
export class BatchError_LengthMismatch {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type BatchError = BatchError_EvenOffset | BatchError_MismatchedPair | BatchError_LengthMismatch;

export class Error_InvalidParameters {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_LengthMismatch {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_NonCanonicalPlaintext {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Randomness {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Noise {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Arithmetic {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type Error = Error_InvalidParameters | Error_LengthMismatch | Error_NonCanonicalPlaintext | Error_Randomness | Error_Noise | Error_Arithmetic;

export class Stage_PublicParameters {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Stage_ReusableCiphertext {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Stage_PerUseCiphertext {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Stage_SelectionKey {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Stage_Complete {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Stage_Error {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type Stage = Stage_PublicParameters | Stage_ReusableCiphertext | Stage_PerUseCiphertext | Stage_SelectionKey | Stage_Complete | Stage_Error;

export class FrameError_Truncated {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_UnsupportedFormat {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_UnknownStage {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_PayloadTooLarge {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_LengthMismatch {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type FrameError = FrameError_Truncated | FrameError_UnsupportedFormat | FrameError_UnknownStage | FrameError_PayloadTooLarge | FrameError_LengthMismatch;

export class TfheBootstrapTableError_AddressWidthOutOfRange {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_TableLengthMismatch {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_RingCapacityExceeded {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_NegacyclicIncompatible {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_InputEncodingUnsupported {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type TfheBootstrapTableError = TfheBootstrapTableError_AddressWidthOutOfRange | TfheBootstrapTableError_TableLengthMismatch | TfheBootstrapTableError_RingCapacityExceeded | TfheBootstrapTableError_NegacyclicIncompatible | TfheBootstrapTableError_InputEncodingUnsupported;

export const GF8_POLY = 27n;
export const GF64_POLY = 27n;
export const GF128_POLY = 135n;
export const GF256_POLY = new U256([1061n, 0n, 0n, 0n]);
export const SBOX = [99n, 124n, 119n, 123n, 242n, 107n, 111n, 197n, 48n, 1n, 103n, 43n, 254n, 215n, 171n, 118n, 202n, 130n, 201n, 125n, 250n, 89n, 71n, 240n, 173n, 212n, 162n, 175n, 156n, 164n, 114n, 192n, 183n, 253n, 147n, 38n, 54n, 63n, 247n, 204n, 52n, 165n, 229n, 241n, 113n, 216n, 49n, 21n, 4n, 199n, 35n, 195n, 24n, 150n, 5n, 154n, 7n, 18n, 128n, 226n, 235n, 39n, 178n, 117n, 9n, 131n, 44n, 26n, 27n, 110n, 90n, 160n, 82n, 59n, 214n, 179n, 41n, 227n, 47n, 132n, 83n, 209n, 0n, 237n, 32n, 252n, 177n, 91n, 106n, 203n, 190n, 57n, 74n, 76n, 88n, 207n, 208n, 239n, 170n, 251n, 67n, 77n, 51n, 133n, 69n, 249n, 2n, 127n, 80n, 60n, 159n, 168n, 81n, 163n, 64n, 143n, 146n, 157n, 56n, 245n, 188n, 182n, 218n, 33n, 16n, 255n, 243n, 210n, 205n, 12n, 19n, 236n, 95n, 151n, 68n, 23n, 196n, 167n, 126n, 61n, 100n, 93n, 25n, 115n, 96n, 129n, 79n, 220n, 34n, 42n, 144n, 136n, 70n, 238n, 184n, 20n, 222n, 94n, 11n, 219n, 224n, 50n, 58n, 10n, 73n, 6n, 36n, 92n, 194n, 211n, 172n, 98n, 145n, 149n, 228n, 121n, 231n, 200n, 55n, 109n, 141n, 213n, 78n, 169n, 108n, 86n, 244n, 234n, 101n, 122n, 174n, 8n, 186n, 120n, 37n, 46n, 28n, 166n, 180n, 198n, 232n, 221n, 116n, 31n, 75n, 189n, 139n, 138n, 112n, 62n, 181n, 102n, 72n, 3n, 246n, 14n, 97n, 53n, 87n, 185n, 134n, 193n, 29n, 158n, 225n, 248n, 152n, 17n, 105n, 217n, 142n, 148n, 155n, 30n, 135n, 233n, 206n, 85n, 40n, 223n, 140n, 161n, 137n, 13n, 191n, 230n, 66n, 104n, 65n, 153n, 45n, 15n, 176n, 84n, 187n, 22n];
export const RCON = [0n, 1n, 2n, 4n, 8n, 16n, 32n, 64n, 128n, 27n, 54n];
export const NR = 10n;
export const BLOCK = 16n;
export const NK_ROUND_KEYS = fieldAdd(NR, 1n);
export const GF8_AES_POLY = 27n;
export const LAMBDA_BYTES = 16n;
export const TAU = 4n;
export const SUB_VOLE_N = 8n;
export const SUB_VOLE_K = 3n;
export const L_HAT_BYTES = 16n;
export const W_GRIND = 4n;
export const COM_BYTES = 32n;
export const MAX_LUT_ARITY = 32n;
export const MAGIC = new Uint8Array([/* byte string */]);
export const VERSION = 1n;
export const MAX_ITEMS = fieldShl(1n, 20n);
export const FALSE = 0n;
export const SECURITY_BITS = 128n;
export const PAPER_PROFILE = new PaperProfile({ $fsecurity_bits: SECURITY_BITS, $fring_degree: 4096n, $fmodulus_bits: 109n, $fbatch_messages: 699050n });
export const REFERENCE_PLAINTEXT_MODULUS = 1125899906826241n;
export const REFERENCE_DELTA = 576460752303415297n;
export const SMALL_NOISE_STANDARD_DEVIATION = 4n;
export const SMALL_NOISE_MAX_DEVIATION = 512n;
export const LARGE_NOISE_STANDARD_DEVIATION = 1000n;
export const LARGE_NOISE_MAX_DEVIATION = 128000n;
export const REFERENCE_PARAMETERS = new Parameters({ $fdegree: 4096n, $fwidth: 512n, $fplaintext_modulus: REFERENCE_PLAINTEXT_MODULUS, $fdelta: REFERENCE_DELTA, $fgadget_base: fieldShl(1n, 28n), $fgadget_digits: 4n });
export const HEADER_BYTES = fieldAdd(fieldAdd(fieldAdd(fieldAdd(fieldAdd(fieldAdd(fieldAdd(8n, 2n), 1n), 32n), 32n), 32n), 8n), 4n);
export const Q4 = fieldShl(1n, 30n);
export const AND_VARS = 7n;
export const AND_CONS = 3n;
export const K_A = 0n;
export const K_B = 1n;
export const K_C = 2n;
export const DELTA = 3n;
export const V_HAT = 4n;
export const P1 = 5n;
export const P2 = 6n;
export const U = 7n;
export const IKNP_KAPPA = 128n;
export const IKNP_KAPPA_BYTES = (IKNP_KAPPA / 8n);
export const TAG_DOMAIN = new Uint8Array([/* byte string */]);
export const LWE_N = 16n;
export const LWE_Q_BITS = 16n;
export const LWE_Q = fieldShl(1n, LWE_Q_BITS);
export const LWE_Q_MASK = fieldSub(LWE_Q, 1n);
export const LWE_NOISE_BOUND = 1n;
export const KAPPA_BITS = 128n;
export const KAPPA_BYTES = 16n;
export const LOCALITY = 10n;
export const FERRET_REG_TOY = new FerretParams({ $fn: 256n, $fk: 32n, $ft: 4n });
export const FERRET_REG_SETUP = new FerretParams({ $fn: 609728n, $fk: 36288n, $ft: 1269n });
export const FERRET_REG_MAIN = new FerretParams({ $fn: 10805248n, $fk: 589760n, $ft: 1319n });
export const FERRET_UNI_TOY = new FerretParams({ $fn: 256n, $fk: 32n, $ft: 4n });
export const FERRET_UNI_SETUP = new FerretParams({ $fn: 616092n, $fk: 37248n, $ft: 1254n });
export const FERRET_UNI_MAIN = new FerretParams({ $fn: 10616092n, $fk: 588160n, $ft: 1324n });
export const STACK_SEED_BYTES = IKNP_KAPPA_BYTES;
export const TOY_P = 2147483647n;
export const TOY_G = 7n;
export const TAG_DELTA = 1n;
export const TAG_LWE_SETUP = 2n;
export const TAG_LWE_RECV = 3n;
export const TAG_LWE_PAYLOAD = 4n;
export const TAG_IKNP_U = 5n;
export const TAG_IKNP_CORR = 6n;
export const TAG_SSP_S = 7n;
export const TAG_SSP_R = 8n;
export const TAG_FERRET_OPEN = 9n;
export const TAG_FERRET_MPCOT = 10n;
export const TAG_BEA95 = 11n;
export const TAG_HAT = 12n;
export const P_LIMBS = [18446744073709551597n, 18446744073709551615n, 18446744073709551615n, 9223372036854775807n];
export const D_LIMBS = [8496970652267935907n, 31536524315187371n, 10144147576115030168n, 5909686906226998899n];
export const D2_LIMBS = [16993941304535871833n, 63073048630374742n, 1841551078520508720n, 2596001775599221991n];
export const D = fe_const(D_LIMBS);
export const D2 = fe_const(D2_LIMBS);
export const BASE_X_LIMBS = [14507833142362363162n, 7578651490590762930n, 13881468655802702940n, 2407515759118799870n];
export const BASE_Y_LIMBS = [7378697629483820632n, 7378697629483820646n, 7378697629483820646n, 7378697629483820646n];

export function add_mod(left: bigint, right: bigint, modulus: bigint): bigint
{
  const sum = fieldAdd(left, right);
  return (() => { if ((sum >= modulus)) {
  return fieldSub(sum, modulus);
} else {
  return sum;
} })();
}

export function add_round_key(state: bigint[], round_key: bigint[])
{
  for (let i = 0n; i < BLOCK; i += 1n)   {
    state[Number(i)] = fieldBitxor(state[Number(i)], round_key[Number(i)]);
  }
}

export function add_to_lower_word(iv: bigint[], counter: bigint): bigint[]
{
  let out = iv;
  const lower = u32_from_le_bytes([out[Number(0n)], out[Number(1n)], out[Number(2n)], out[Number(3n)]]);
  const new_ = wrappingAdd(lower, counter, 32);
  const bytes = [(new_) & 0xFFn, ((new_) >> 8n) & 0xFFn, ((new_) >> 16n) & 0xFFn, ((new_) >> 24n) & 0xFFn];
  out[Number(0n)] = bytes[Number(0n)];
  out[Number(1n)] = bytes[Number(1n)];
  out[Number(2n)] = bytes[Number(2n)];
  out[Number(3n)] = bytes[Number(3n)];
  return out;
}

export function add_to_upper_word(iv: bigint[], tweak: bigint)
{
  const upper = u32_from_le_bytes([iv[Number(12n)], iv[Number(13n)], iv[Number(14n)], iv[Number(15n)]]);
  const new_ = wrappingAdd(upper, tweak, 32);
  const bytes = [(new_) & 0xFFn, ((new_) >> 8n) & 0xFFn, ((new_) >> 16n) & 0xFFn, ((new_) >> 24n) & 0xFFn];
  iv[Number(12n)] = bytes[Number(0n)];
  iv[Number(13n)] = bytes[Number(1n)];
  iv[Number(14n)] = bytes[Number(2n)];
  iv[Number(15n)] = bytes[Number(3n)];
}

export function aes_ctr_prg(seed: bigint[], iv: bigint[], tweak: bigint, out_bytes: bigint): Vec<bigint>
{
  let iv_tweaked = iv;
  add_to_upper_word(iv_tweaked, tweak);
  const n_full = (out_bytes / BLOCK);
  const rem = (out_bytes % BLOCK);
  let out = ([] as any[]);
  for (let i = 0n; i < n_full; i += 1n)   {
    const block_in = add_to_lower_word(iv_tweaked, Number(i));
    const ct = encrypt_block(seed, block_in);
    out.push(...(ct));
  }
  if ((rem > 0n))   {
    const block_in = add_to_lower_word(iv_tweaked, Number(n_full));
    const ct = encrypt_block(seed, block_in);
    out.push(...(ct.slice(0, Number(rem))));
  }
  return out;
}

export function and_test_poly(big_n: bigint): bigint[]
{
  let v = Array.from({length: Number(big_n)}, () => 0n);
  const half_q4 = fieldShr(Q4, 1n);
  for (let k = 0n; k < (big_n / 2n); k += 1n)   {
    v[Number(k)] = wrappingNeg(half_q4, 32);
  }
  for (let k = (big_n / 2n); k < big_n; k += 1n)   {
    v[Number(k)] = half_q4;
  }
  return v;
}

export function assert_one_check<T>(n: bigint, q: QDyn<T>, opening: T[], delta: DeltaDyn<T>): boolean
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    ok = (ok && __equals(fieldAdd(__clone(q.$fq[Number(i)]), __clone(opening[Number(i)])), __clone(delta.$fdelta[Number(i)])));
  }
  return ok;
}

export function bea95_chosen_bit(delta: any, r0: any, x: boolean, z: any, b: boolean): [Block, Block, boolean]
{
  const d = fieldBitxor(b, x);
  const r0_chosen = (() => { if (d) {
  return xor_block(r0, delta);
} else {
  return r0;
} })();
  return [r0_chosen, z, d];
}

export function binfhe_blind_rotate(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, bs_ell: bigint, bs_base_log: bigint, ct: BinfheLweCiphertextDyn, test_poly: bigint[], bsk: BinfheRgswCiphertextDyn[]): BinfheRlweCiphertextDyn
{
  const two_n = fieldMul(2n, big_n);
  const b_exp = exponent(log_q_lwe, big_n, ct.$fb);
  let acc = binfhe_rlwe_trivial(test_poly);
  if (!__equals(b_exp, 0n))   {
    acc = binfhe_rlwe_rotate(acc, fieldSub(two_n, b_exp));
  }
  for (const [i, row] of bsk.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const a_exp = exponent(log_q_lwe, big_n, ct.$fa[Number(i)]);
    if (!__equals(a_exp, 0n))     {
      const rotated = binfhe_rlwe_rotate(acc, a_exp);
      acc = binfhe_rgsw_cmux(row, rotated, acc);
    }
  }
  return acc;
}

export function binfhe_cmux(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, k_max: bigint, sel: BinfheLweCiphertextDyn, a: BinfheLweCiphertextDyn, b: BinfheLweCiphertextDyn, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  const TABLE: boolean[] = [false, false, false, true, true, false, true, true];
  return binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, [sel, a, b], TABLE, k_max, bk);
}

export function binfhe_external_product(n: bigint, log: bigint, ell: bigint, base_log: bigint, c: BinfheRgswCiphertextDyn, ct: BinfheRlweCiphertextDyn): BinfheRlweCiphertextDyn
{
  const a_dec = gadget_poly_decompose(ct.$fa);
  const b_dec = gadget_poly_decompose(ct.$fb);
  let out_a = Array.from({length: Number(n)}, () => 0n);
  let out_b = Array.from({length: Number(n)}, () => 0n);
  for (let j = 0n; j < ell; j += 1n)   {
    const row = c.$frows[Number(j)];
    const a0 = binfhe_poly_mul_neg(n, log, a_dec[Number(j)], row.$frlwe0.$fa);
    const a1 = binfhe_poly_mul_neg(n, log, a_dec[Number(j)], row.$frlwe0.$fb);
    const b0 = binfhe_poly_mul_neg(n, log, b_dec[Number(j)], row.$frlwe1.$fa);
    const b1 = binfhe_poly_mul_neg(n, log, b_dec[Number(j)], row.$frlwe1.$fb);
    for (let k = 0n; k < n; k += 1n)     {
      out_a[Number(k)] = wrappingAdd(wrappingAdd(out_a[Number(k)], a0[Number(k)], 32), b0[Number(k)], 32);
      out_b[Number(k)] = wrappingAdd(wrappingAdd(out_b[Number(k)], a1[Number(k)], 32), b1[Number(k)], 32);
    }
  }
  for (let k = 0n; k < n; k += 1n)   {
    out_a[Number(k)] = reduce(out_a[Number(k)]);
    out_b[Number(k)] = reduce(out_b[Number(k)]);
  }
  return new BinfheRlweCiphertextDyn({ $fa: out_a, $fb: out_b, $fn: 0n });
}

export function binfhe_gate_and(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, k_max: bigint, a: BinfheLweCiphertextDyn, b: BinfheLweCiphertextDyn, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  return binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, [a, b], [false, false, false, true], k_max, bk);
}

export function binfhe_gate_or(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, k_max: bigint, a: BinfheLweCiphertextDyn, b: BinfheLweCiphertextDyn, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  return binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, [a, b], [false, true, true, true], k_max, bk);
}

export function binfhe_gate_xor(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, k_max: bigint, a: BinfheLweCiphertextDyn, b: BinfheLweCiphertextDyn, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  return binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, [a, b], [false, true, true, false], k_max, bk);
}

export function binfhe_gen_bootstrapping_key<R>(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, eta: bigint, lwe_sk: BinfheLweSecretKeyDyn, rlwe_sk: BinfheRlweSecretKeyDyn, rng: any): BinfheBootstrappingKeyDyn
{
  const bsk = Array.from({length: Number(n_lwe - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return binfhe_rgsw_encrypt(eta, !__equals(lwe_sk.$fkey[Number(i)], 0n), rlwe_sk, rng);
})());
  const ksk = new BinfheKeySwitchingKeyDyn({ $fksk: Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const msg = BigInt(Math.imul(Number(rlwe_sk.$fkey[Number(i)]), Number(level_factor(ks_base_log, j))));
  return binfhe_lwe_encrypt_raw(eta, reduce(msg), lwe_sk, rng);
})());
})()), $fn_lwe: 0n, $fbig_n: 0n, $fks_ell: 0n });
  return new BinfheBootstrappingKeyDyn({ $fbsk: bsk, $fksk: ksk, $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n });
}

export function binfhe_gen_lwe_secret_key<R>(n: bigint, rng: any): BinfheLweSecretKeyDyn
{
  let key = Array.from({length: Number(n)}, () => 0n);
  let i = 0n;
  while ((i < n))   {
    let word = rng.next_u32();
    const take = BigInt(Math.min(Number(fieldSub(n, i)), Number(32n)));
    for (let _ = 0n; _ < take; _ += 1n)     {
      key[Number(i)] = ((fieldBitand(word, 1n)) & 0xFFn);
      word = fieldShr(word, 1n);
      i = fieldAdd(i, 1n);
    }
  }
  return new BinfheLweSecretKeyDyn({ $fkey: key, $fn: 0n });
}

export function binfhe_gen_rlwe_secret_key<R>(n: bigint, rng: any): BinfheRlweSecretKeyDyn
{
  let key = Array.from({length: Number(n)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(key.length); __mut_1 += 1n) {
  {
    key[Number(__mut_1)] = Number(fieldBitand(rng.next_u32(), 1n));
  }}
  return new BinfheRlweSecretKeyDyn({ $fkey: key, $fn: 0n });
}

export function binfhe_key_switch<K>(n_lwe: bigint, big_n: bigint, log_mod_ks: bigint, ks_ell: bigint, ks_base_log: bigint, ct: BinfheLweCiphertextDyn, ksk: any): BinfheLweCiphertextDyn
{
  const ksk_rows = ksk.ksk_rows();
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  let out_b = ct.$fb;
  for (let i = 0n; i < big_n; i += 1n)   {
    const digits = gadget_decompose(ct.$fa[Number(i)]);
    for (let j = 0n; j < ks_ell; j += 1n)     {
      const d = digits[Number(j)];
      if (__equals(d, 0n))       {
        continue;
      }
      const entry = ksk_rows[Number(i)][Number(j)];
      for (let k = 0n; k < n_lwe; k += 1n)       {
        out_a[Number(k)] = wrappingSub(out_a[Number(k)], BigInt(Math.imul(Number(d), Number(entry.$fa[Number(k)]))), 32);
      }
      out_b = wrappingSub(out_b, BigInt(Math.imul(Number(d), Number(entry.$fb))), 32);
    }
  }
  for (let k = 0n; k < n_lwe; k += 1n)   {
    out_a[Number(k)] = reduce(out_a[Number(k)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: out_a, $fb: reduce(out_b), $fn: 0n });
}

export function binfhe_lut_read(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, addr_bits: bigint, table_len: bigint, k_max: bigint, addr: BinfheLweCiphertextDyn[], lut: LutDyn, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  const delta = wire_delta(log_q_lwe, k_max);
  if (lut.is_constant())   {
    return binfhe_trivial(lut.constant_value(), delta);
  }
  let combined = binfhe_trivial(false, 0n);
  for (const [j, bit] of addr.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const scaled = binfhe_lwe_scale(bit, fieldShl(1n, j));
    combined = binfhe_lwe_add(combined, scaled);
  }
  combined = binfhe_lwe_add_const(combined, (delta / 2n));
  return binfhe_pbs_core(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, combined, lut.test_polynomial(), bk);
}

export function binfhe_lut_read_dyn(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, inputs: BinfheLweCiphertextDyn[], table: boolean[], k_max: bigint, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  const delta = wire_delta(log_q_lwe, Number(k_max));
  if (table_is_constant(table))   {
    return binfhe_trivial(table[Number(0n)], delta);
  }
  const arity = Number(Math.clz32((BigInt(table.length)) & -((BigInt(table.length)) | 0)));
  const test_poly = fill_test_poly(big_n, table, arity, Number(k_max), log_q, log_q_lwe);
  let combined = binfhe_trivial(false, 0n);
  for (const [j, bit] of inputs.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const scaled = binfhe_lwe_scale(bit, fieldShl(1n, j));
    combined = binfhe_lwe_add(combined, scaled);
  }
  combined = binfhe_lwe_add_const(combined, (delta / 2n));
  return binfhe_pbs_core(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, combined, test_poly, bk);
}

export function binfhe_lwe_add(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, y: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = torus_add(x.$fa[Number(i)], y.$fa[Number(i)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: torus_add(x.$fb, y.$fb), $fn: 0n });
}

export function binfhe_lwe_add_const(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, c: bigint): BinfheLweCiphertextDyn
{
  return new BinfheLweCiphertextDyn({ $fa: x.$fa, $fb: torus_add(x.$fb, c), $fn: 0n });
}

export function binfhe_lwe_decrypt(n: bigint, log_m: bigint, ct: BinfheLweCiphertextDyn, sk: BinfheLweSecretKeyDyn, delta: bigint): boolean
{
  return lwe_decode(log_m, lwe_phase(n, log_m, ct, sk), delta);
}

export function binfhe_lwe_encrypt<R>(n: bigint, log_m: bigint, eta: bigint, m: boolean, delta: bigint, sk: BinfheLweSecretKeyDyn, rng: any): BinfheLweCiphertextDyn
{
  const msg = (() => { if (m) {
  return delta;
} else {
  return 0n;
} })();
  return binfhe_lwe_encrypt_raw(n, log_m, eta, msg, sk, rng);
}

export function binfhe_lwe_encrypt_raw<R>(n: bigint, log_m: bigint, eta: bigint, msg: bigint, sk: BinfheLweSecretKeyDyn, rng: any): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(a.length); __mut_1 += 1n) {
  {
    a[Number(__mut_1)] = reduce(rng.next_u32());
  }}
  let dot = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(Number(sk.$fkey[Number(i)])))), 32);
  }
  const e = sample_error(rng);
  const b = reduce(wrappingAdd(wrappingAdd(dot, e, 32), msg, 32));
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: b, $fn: 0n });
}

export function binfhe_lwe_neg(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = torus_neg(x.$fa[Number(i)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: torus_neg(x.$fb), $fn: 0n });
}

export function binfhe_lwe_scale(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, c: bigint): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = mul_exact(x.$fa[Number(i)], c);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: mul_exact(x.$fb, c), $fn: 0n });
}

export function binfhe_lwe_sub(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, y: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = torus_sub(x.$fa[Number(i)], y.$fa[Number(i)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: torus_sub(x.$fb, y.$fb), $fn: 0n });
}

export function binfhe_not(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, delta: bigint): BinfheLweCiphertextDyn
{
  let out = binfhe_lwe_neg(n, log_m, x);
  out.$fb = torus_add(out.$fb, delta);
  return out;
}

export function binfhe_pbs_core<BK>(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, ct: BinfheLweCiphertextDyn, test_poly: bigint[], bk: any): BinfheLweCiphertextDyn
{
  const acc = binfhe_blind_rotate(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, ct, test_poly, bk.bsk_rows());
  const extracted = binfhe_sample_extract(acc);
  const at_ks = mod_switch_lwe(extracted);
  const switched = binfhe_key_switch(n_lwe, big_n, log_mod_ks, ks_ell, ks_base_log, at_ks, bk.ksk_ref());
  return mod_switch_lwe(switched);
}

export function binfhe_poly_mul_neg(n: bigint, log: bigint, a: bigint[], b: bigint[]): bigint[]
{
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    for (let j = 0n; j < n; j += 1n)     {
      const deg = fieldAdd(i, j);
      const term = BigInt(Math.imul(Number(a[Number(i)]), Number(b[Number(j)])));
      if ((deg < n))       {
        result[Number(deg)] = wrappingAdd(result[Number(deg)], term, 32);
      } else       {
        result[Number(fieldSub(deg, n))] = wrappingSub(result[Number(fieldSub(deg, n))], term, 32);
      }
    }
  }
  for (let __mut_1 = 0n; __mut_1 < BigInt(result.length); __mut_1 += 1n) {
  {
    result[Number(__mut_1)] = reduce(result[Number(__mut_1)]);
  }}
  return result;
}

export function binfhe_poly_rotate(n: bigint, log: bigint, p: bigint[], exp: bigint): bigint[]
{
  const exp_1 = (exp % fieldMul(2n, n));
  let result = Array.from({length: Number(n)}, () => 0n);
  for (const [i, coeff] of p.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const dest = fieldAdd(i, exp_1);
    if ((dest < n))     {
      result[Number(dest)] = wrappingAdd(result[Number(dest)], coeff, 32);
    } else if ((dest < fieldMul(2n, n)))     {
      result[Number(fieldSub(dest, n))] = wrappingSub(result[Number(fieldSub(dest, n))], coeff, 32);
    } else     {
      result[Number(fieldSub(dest, fieldMul(2n, n)))] = wrappingAdd(result[Number(fieldSub(dest, fieldMul(2n, n)))], coeff, 32);
    }
  }
  for (let __mut_1 = 0n; __mut_1 < BigInt(result.length); __mut_1 += 1n) {
  {
    result[Number(__mut_1)] = reduce(result[Number(__mut_1)]);
  }}
  return result;
}

export function binfhe_rgsw_cmux(n: bigint, log: bigint, ell: bigint, base_log: bigint, c: BinfheRgswCiphertextDyn, d1: BinfheRlweCiphertextDyn, d0: BinfheRlweCiphertextDyn): BinfheRlweCiphertextDyn
{
  const diff = binfhe_rlwe_sub(n, log, d1, d0);
  const prod = binfhe_external_product(n, log, ell, base_log, c, diff);
  return binfhe_rlwe_add(n, log, d0, prod);
}

export function binfhe_rgsw_encrypt<R>(n: bigint, log: bigint, ell: bigint, base_log: bigint, eta: bigint, m: boolean, sk: BinfheRlweSecretKeyDyn, rng: any): BinfheRgswCiphertextDyn
{
  const rows = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const g = level_factor(base_log, j);
  const contrib = (() => { if (m) {
  return g;
} else {
  return 0n;
} })();
  let rlwe0 = binfhe_rlwe_encrypt_scalar(n, log, eta, 0n, sk, rng);
  rlwe0.$fa[Number(0n)] = torus_add(rlwe0.$fa[Number(0n)], contrib);
  const rlwe1 = binfhe_rlwe_encrypt_scalar(n, log, eta, contrib, sk, rng);
  return new BinfheRgswRowDyn({ $frlwe0: rlwe0, $frlwe1: rlwe1, $fn: 0n });
})());
  return new BinfheRgswCiphertextDyn({ $frows: rows, $fn: 0n, $fell: 0n });
}

export function binfhe_rlwe_add(n: bigint, log: bigint, x: BinfheRlweCiphertextDyn, y: BinfheRlweCiphertextDyn): BinfheRlweCiphertextDyn
{
  let out = x;
  for (let i = 0n; i < n; i += 1n)   {
    out.$fa[Number(i)] = torus_add(out.$fa[Number(i)], y.$fa[Number(i)]);
    out.$fb[Number(i)] = torus_add(out.$fb[Number(i)], y.$fb[Number(i)]);
  }
  return out;
}

export function binfhe_rlwe_encrypt_poly<R>(n: bigint, log: bigint, eta: bigint, msg: bigint[], sk: BinfheRlweSecretKeyDyn, rng: any): BinfheRlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => reduce(rng.next_u32()));
  let b = binfhe_poly_mul_neg(n, log, a, sk.$fkey);
  for (let i = 0n; i < n; i += 1n)   {
    b[Number(i)] = reduce(wrappingAdd(wrappingAdd(b[Number(i)], sample_error(rng), 32), msg[Number(i)], 32));
  }
  return new BinfheRlweCiphertextDyn({ $fa: a, $fb: b, $fn: 0n });
}

export function binfhe_rlwe_encrypt_scalar<R>(n: bigint, log: bigint, eta: bigint, m: bigint, sk: BinfheRlweSecretKeyDyn, rng: any): BinfheRlweCiphertextDyn
{
  let msg = Array.from({length: Number(n)}, () => 0n);
  msg[Number(0n)] = m;
  return binfhe_rlwe_encrypt_poly(n, log, eta, msg, sk, rng);
}

export function binfhe_rlwe_phase(n: bigint, log: bigint, ct: BinfheRlweCiphertextDyn, sk: BinfheRlweSecretKeyDyn): bigint[]
{
  const product = binfhe_poly_mul_neg(n, log, ct.$fa, sk.$fkey);
  let phase = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    phase[Number(i)] = torus_sub(ct.$fb[Number(i)], product[Number(i)]);
  }
  return phase;
}

export function binfhe_rlwe_rotate(n: bigint, log: bigint, ct: BinfheRlweCiphertextDyn, exp: bigint): BinfheRlweCiphertextDyn
{
  return new BinfheRlweCiphertextDyn({ $fa: binfhe_poly_rotate(n, log, ct.$fa, exp), $fb: binfhe_poly_rotate(n, log, ct.$fb, exp), $fn: 0n });
}

export function binfhe_rlwe_sub(n: bigint, log: bigint, x: BinfheRlweCiphertextDyn, y: BinfheRlweCiphertextDyn): BinfheRlweCiphertextDyn
{
  let out = x;
  for (let i = 0n; i < n; i += 1n)   {
    out.$fa[Number(i)] = torus_sub(out.$fa[Number(i)], y.$fa[Number(i)]);
    out.$fb[Number(i)] = torus_sub(out.$fb[Number(i)], y.$fb[Number(i)]);
  }
  return out;
}

export function binfhe_rlwe_trivial(n: bigint, log: bigint, msg: bigint[]): BinfheRlweCiphertextDyn
{
  return new BinfheRlweCiphertextDyn({ $fa: Array.from({length: Number(n)}, () => 0n), $fb: msg, $fn: 0n });
}

export function binfhe_sample_extract(n: bigint, log: bigint, ct: BinfheRlweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a_lwe = Array.from({length: Number(n)}, () => 0n);
  a_lwe[Number(0n)] = ct.$fa[Number(0n)];
  for (let i = 1n; i < n; i += 1n)   {
    a_lwe[Number(i)] = torus_neg(ct.$fa[Number(fieldSub(n, i))]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a_lwe, $fb: ct.$fb[Number(0n)], $fn: 0n });
}

export function binfhe_trivial(n: bigint, log_m: bigint, m: boolean, delta: bigint): BinfheLweCiphertextDyn
{
  return new BinfheLweCiphertextDyn({ $fa: Array.from({length: Number(n)}, () => 0n), $fb: (() => { if (m) {
  return delta;
} else {
  return 0n;
} })(), $fn: 0n });
}

export function bit_msb(alpha: bigint, h: bigint, i: bigint): boolean
{
  return __equals(fieldBitand(fieldShr(alpha, fieldSub(fieldSub(h, 1n), i)), 1n), 1n);
}

export function bit_reverse(values: bigint[])
{
  const bits = ilog2(BigInt(values.length));
  for (let index = 0n; index < BigInt(values.length); index += 1n)   {
    const reversed = fieldShr(index.reverse_bits(), fieldSub(usize.BITS, bits));
    if ((index < reversed))     {
      values.swap(index, reversed);
    }
  }
}

export function bit_to_g128(b: boolean): Galois128
{
  return new Galois128((b as unknown as bigint));
}

export function bits_to_bytes(bits: bigint[], nbytes: bigint): Vec<bigint>
{
  let out = [] as any[];
  for (let i = 0n; i < nbytes; i += 1n)   {
    let acc = 0n;
    for (let bit = 0n; bit < 8n; bit += 1n)     {
      const idx = fieldAdd(fieldMul(i, 8n), bit);
      if (((idx < BigInt(bits.length)) && !__equals(bits[Number(idx)], 0n)))       {
        acc = fieldBitor(acc, fieldShl(1n, bit));
      }
    }
    out[Number(i)] = acc;
  }
  return out;
}

export function blind_rotate(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, ct: LweCiphertextDyn, bk: BootstrappingKeyDyn): RlweCiphertextDyn
{
  return blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, ct, and_test_poly(big_n), bk);
}

export function blind_rotate_with_poly(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, ct: LweCiphertextDyn, test_poly: bigint[], bk: BootstrappingKeyDyn): RlweCiphertextDyn
{
  let acc = new RlweCiphertextDyn({ $fa: Array.from({length: Number(big_n)}, () => 0n), $fb: test_poly, $fbig_n: 0n });
  const two_n = fieldMul(2n, big_n);
  const log2_two_n = Math.clz32((two_n) & -((two_n) | 0));
  const scale_shift = (32n - (log2_two_n));
  const b_exp = torus_to_exp(ct.$fb, scale_shift, two_n);
  if (!__equals(b_exp, 0n))   {
    acc = rlwe_rotate(big_n, acc, fieldSub(two_n, b_exp));
  }
  for (let i = 0n; i < n_lwe; i += 1n)   {
    const a_exp = torus_to_exp(ct.$fa[Number(i)], scale_shift, two_n);
    if (!__equals(a_exp, 0n))     {
      const acc_rotated = rlwe_rotate(big_n, acc, a_exp);
      acc = cmux(big_n, bs_ell, bs_bg_log, bk.$fbsk[Number(i)], acc_rotated, acc);
    }
  }
  return acc;
}

export function block_from_field(g: Galois128): Block
{
  return [(g[0]) & 0xFFn, ((g[0]) >> 8n) & 0xFFn, ((g[0]) >> 16n) & 0xFFn, ((g[0]) >> 24n) & 0xFFn];
}

export function build_buckets(seed: bigint[], n: bigint, m: bigint): Vec<Vec<bigint>>
{
  let buckets = [] as any[];
  for (let x = 0n; x < n; x += 1n)   {
    for (let i = 0n; i < TAU; i += 1n)     {
      const j = hash_i(seed, i, x, m);
      (buckets[Number(j)]).push(x);
    }
  }
  for (const b of buckets)   {
    b.sort_unstable();
    b.dedup();
  }
  return buckets;
}

export function bytes_to_bits(bytes: bigint[]): Vec<bigint>
{
  let bits = ([] as any[]);
  for (const b of bytes)   {
    for (let bit = 0n; bit < 8n; bit += 1n)     {
      (bits).push(fieldBitand(fieldShr(b, bit), 1n));
    }
  }
  return bits;
}

export function cbd<R>(eta: bigint, rng: any): bigint
{
  if (__equals(eta, 0n))   {
    return 0n;
  }
  const mask = (() => { if ((eta >= 32n)) {
  return u32.MAX;
} else {
  return fieldSub(fieldShl(1n, eta), 1n);
} })();
  const a = (/* count_ones */ ((() => { let _n = fieldBitand(rng.next_u32(), mask), _c = 0; while (_n) { _c += _n & 1; _n >>>= 1; } return _c; })()) as unknown as bigint);
  const b = (/* count_ones */ ((() => { let _n = fieldBitand(rng.next_u32(), mask), _c = 0; while (_n) { _c += _n & 1; _n >>>= 1; } return _c; })()) as unknown as bigint);
  return fieldSub(a, b);
}

export function cert_and(): GateCertificate
{
  return new GateCertificate({ $fname: "AND", $farity: 2n, $fprepare: (c, q) => (fieldAdd(c[Number(0n)], c[Number(1n)]) % q), $finterval_true: [3n, 7n] });
}

export function cert_majority(): GateCertificate
{
  return new GateCertificate({ $fname: "Majority", $farity: 3n, $fprepare: (c, q) => (fieldAdd(fieldAdd(c[Number(0n)], c[Number(1n)]), c[Number(2n)]) % q), $finterval_true: [3n, 7n] });
}

export function cert_nand(): GateCertificate
{
  return new GateCertificate({ $fname: "NAND", $farity: 2n, $fprepare: (c, q) => (fieldAdd(c[Number(0n)], c[Number(1n)]) % q), $finterval_true: [7n, 3n] });
}

export function cert_nor(): GateCertificate
{
  return new GateCertificate({ $fname: "NOR", $farity: 2n, $fprepare: (c, q) => (fieldAdd(c[Number(0n)], c[Number(1n)]) % q), $finterval_true: [5n, 1n] });
}

export function cert_or(): GateCertificate
{
  return new GateCertificate({ $fname: "OR", $farity: 2n, $fprepare: (c, q) => (fieldAdd(c[Number(0n)], c[Number(1n)]) % q), $finterval_true: [1n, 5n] });
}

export function cert_xnor(): GateCertificate
{
  return new GateCertificate({ $fname: "XNOR", $farity: 2n, $fprepare: (c, q) => (() => {
  const diff = BigInt(fieldSub((c[Number(0n)] as unknown as bigint), (c[Number(1n)] as unknown as bigint)).rem_euclid((q as unknown as bigint)));
  return (fieldMul(2n, diff) % q);
})(), $finterval_true: [5n, 1n] });
}

export function cert_xor(): GateCertificate
{
  return new GateCertificate({ $fname: "XOR", $farity: 2n, $fprepare: (c, q) => (() => {
  const diff = BigInt(fieldSub((c[Number(0n)] as unknown as bigint), (c[Number(1n)] as unknown as bigint)).rem_euclid((q as unknown as bigint)));
  return (fieldMul(2n, diff) % q);
})(), $finterval_true: [1n, 5n] });
}

export function chall1(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, mu: bigint[], iv: bigint[], com_bytes: bigint[], lambda_plus_b: bigint, use_shake256: boolean): Vec<bigint>
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

export function chall2(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, chall_1: bigint[], u_hat: bigint[], d: bigint[], lambda_plus_b: bigint, use_shake256: boolean): Vec<bigint>
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

export function chall3(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, chall_2: bigint[], a_hat: bigint[], b_hat: bigint[], c_hat: bigint[], lambda: bigint, use_shake256: boolean): Vec<bigint>
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

export function check_lut_shape(addr_bits: bigint, table_len: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, k_max: bigint): Result<bigint, LutError>
{
  if ((__equals(addr_bits, 0n) || (addr_bits >= Number(usize.BITS))))   {
    return new LutError_AddressShapeInvalid();
  }
  if (!__equals(table_len, fieldShl(1n, addr_bits)))   {
    return new LutError_AddressShapeInvalid();
  }
  if ((addr_bits > k_max))   {
    return new LutError_ArityExceedsCircuitMax();
  }
  if (((((((fieldAdd(Number(k_max), 2n) > log_q_lwe) || !((big_n) > 0n && ((big_n) & ((big_n) - 1n)) === 0n)) || (fieldShl(1n, k_max) > big_n)) || !__equals(fieldShl(1n, log_q_lwe), fieldMul(2n, big_n))) || (log_q_lwe > log_q)) || (log_q > 32n)))   {
    return new LutError_ShapeUnsupported();
  }
  return fieldShr(big_n, k_max);
}

export function check_profile(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_base_log: bigint, bs_ell: bigint, ks_base_log: bigint, ks_ell: bigint, priv_base_log: bigint, priv_ell: bigint)
{
}

export function circuit_bootstrap(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, priv_ell: bigint, priv_base_log: bigint, ct: BinfheLweCiphertextDyn, cbk: CircuitBootstrappingKeyDyn, k_max: bigint): BinfheRgswCiphertextDyn
{
  const delta = wire_delta(log_q_lwe, Number(k_max));
  const centered = binfhe_lwe_add_const(ct, (delta / 2n));
  const rows = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const test_poly = level_test_poly(big_n, log_q, j, bs_base_log, k_max);
  const acc = binfhe_blind_rotate(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, centered, test_poly, cbk.$fbk.$fbsk);
  const extracted = binfhe_sample_extract(acc);
  const rlwe0 = priv_ks(big_n, log_q, priv_ell, priv_base_log, extracted, cbk.$fprivksk.$fa_col, cbk.$fprivksk.$fa_body);
  const rlwe1 = priv_ks(big_n, log_q, priv_ell, priv_base_log, extracted, cbk.$fprivksk.$fb_col, cbk.$fprivksk.$fb_body);
  return new BinfheRgswRowDyn({ $frlwe0: rlwe0, $frlwe1: rlwe1, $fn: 0n });
})());
  return new BinfheRgswCiphertextDyn({ $frows: rows, $fn: 0n, $fell: 0n });
}

export function cmux(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, c: RgswCiphertextDyn, d1: RlweCiphertextDyn, d0: RlweCiphertextDyn): RlweCiphertextDyn
{
  const diff = rlwe_sub(big_n, d1, d0);
  const prod = external_product(big_n, bs_ell, bs_bg_log, c, diff);
  return rlwe_add(big_n, d0, prod);
}

export function column_rows(seed: bigint[], k: bigint, j: bigint): bigint[]
{
  let rows = Array.from({length: Number(LOCALITY)}, () => 0n);
  let fill = 0n;
  let counter = 0n;
  while ((fill < LOCALITY))   {
    let h = new Sha3_256();
    h.update(new Uint8Array([/* byte string */]));
    h.update(seed);
    h.update([(BigInt(j)) & 0xFFn, ((BigInt(j)) >> 8n) & 0xFFn, ((BigInt(j)) >> 16n) & 0xFFn, ((BigInt(j)) >> 24n) & 0xFFn]);
    h.update([(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn]);
    const out = [...h.finalize()];
    for (const chunk of out.chunks_exact(4n))     {
      if ((fill >= LOCALITY))       {
        break;
      }
      const raw = Number(u32_from_le_bytes((chunk)!));
      const row = (raw % k);
      if (!rows.slice(0, Number(fill)).includes(row))       {
        rows[Number(fill)] = row;
        fill = fieldAdd(fill, 1n);
      }
    }
    counter = fieldAdd(counter, 1n);
  }
  return rows;
}

export function commit<D>(ctx: { newD: () => any }, message: bigint[], rand: bigint[]): CommitmentCoreDyn<D>
{
  let hasher = ctx.newD();
  hasher.update(asRefU8(message));
  hasher.update(asRefU8(rand));
  return new CommitmentCoreDyn([...hasher.finalize()]);
}

export function concat_small_voles(outs: Vec<ConvertOutput>): BigVoleProver
{
  const l_hat = BigInt(outs[Number(0n)].$fu.length);
  for (const o of outs)   {
    for (const vj of o.$fv)     {
    }
  }
  const u = __clone(outs[Number(0n)].$fu);
  let c: Vec<Vec<bigint>> = ([] as any[]);
  for (const o of outs.slice(Number(1n)))   {
    let ci = __clone(o.$fu);
    xor_in_place(ci, u);
    (c).push(ci);
  }
  let v_columns: Vec<Vec<bigint>> = [] as any[];
  for (const o of outs)   {
    for (const vj of o.$fv)     {
      (v_columns).push(vj);
    }
  }
  return new BigVoleProver({ $fu: u, $fc: c, $fv_columns: v_columns });
}

export function concat_small_voles_verifier(outs: Vec<ConvertOutput>, deltas: bigint[], corrections: Vec<bigint>[]): BigVoleVerifier
{
  let q_columns: Vec<Vec<bigint>> = [] as any[];
  for (const [i, o] of outs.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const k = BigInt(o.$fv.length);
    const delta_i = deltas[Number(i)];
    for (const [bit, vj_raw] of o.$fv.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      let q = vj_raw;
      if ((i >= 1n))       {
        const delta_bit = __equals(fieldBitand(fieldShr(delta_i, bit), 1n), 1n);
        if (delta_bit)         {
          xor_in_place(q, corrections[Number(fieldSub(i, 1n))]);
        }
      }
      (q_columns).push(q);
    }
    const _ = k;
  }
  return new BigVoleVerifier({ $fq_columns: q_columns });
}

export function convert_to_vole(seeds: (bigint[] | undefined)[], iv: bigint[], tweak: bigint, l_hat_bytes: bigint): ConvertOutput
{
  const n = BigInt(seeds.length);
  const d = Number(Math.clz32((n) & -((n) | 0)));
  const zero_block = [] as any[];
  let r: Vec<Vec<bigint>> = ([] as any[]);
  for (const s of seeds)   {
    return (() => { const __match = s; if (__match !== null && __match !== undefined) { const seed = __match;
return (r).push(aes_ctr_prg(seed, iv, tweak, l_hat_bytes)); } else { return (r).push(__clone(zero_block)); } })();
  }
  let v: Vec<Vec<bigint>> = Array.from({length: Number(d - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let level: Vec<Vec<bigint>> = r;
  for (let j = 0n; j < d; j += 1n)   {
    const half = (BigInt(level.length) / 2n);
    let next: Vec<Vec<bigint>> = ([] as any[]);
    for (let i = 0n; i < half; i += 1n)     {
      xor_in_place(v[Number(j)], level[Number(fieldAdd(fieldMul(2n, i), 1n))]);
      let new_entry = __clone(level[Number(fieldMul(2n, i))]);
      xor_in_place(new_entry, level[Number(fieldAdd(fieldMul(2n, i), 1n))]);
      (next).push(new_entry);
    }
    level = next;
  }
  const u = (level.next())!;
  return new ConvertOutput({ $fu: u, $fv: v });
}

export function create_vole_from_material(ctx: { B_OutputSize: bigint }, s: any[]): VopeDyn<bigint>
{
  const u: bigint[] = s.reduce((a: any, b: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a[Number(i)], asRefU8(b)[Number(i)]));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  const v: bigint[] = s.map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => fieldBitxor(fieldBitxor(a[Number(j)], asRefU8(b)[Number(j)]), ((i) & 0xFFn)));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  return new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u)), $fv: v, $fn: 0n, $fk: 1n });
}

export function create_vole_from_material_expanded(ctx: { B_OutputSize: bigint }, s: any[], f: (arg: Uint8Array) => any): VopeDyn<bigint>
{
  const u: bigint[] = s.map((b: any) => f(asRefU8(b).slice(0, Number(ctx.B_OutputSize)))).reduce((a: any, b: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a[Number(i)], asRefU8(b)[Number(i)]));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  const v: bigint[] = s.map((b: any) => f(asRefU8(b).slice(0, Number(ctx.B_OutputSize)))).map((val: any, i: number) => [i, val] as [number, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => fieldBitxor(fieldBitxor(a[Number(j)], asRefU8(b)[Number(j)]), ((i) & 0xFFn)));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  return new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u)), $fv: v, $fn: 0n, $fk: 1n });
}

export function crhf(input: any, tweak: bigint): Block
{
  let h = new Sha3_256();
  h.update(new Uint8Array([/* byte string */]));
  h.update(input);
  h.update([(tweak) & 0xFFn, ((tweak) >> 8n) & 0xFFn, ((tweak) >> 16n) & 0xFFn, ((tweak) >> 24n) & 0xFFn]);
  const out = [...h.finalize()];
  let b = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  (b).splice(0, (out.slice(0, Number(KAPPA_BYTES))).length, ...(out.slice(0, Number(KAPPA_BYTES))));
  return b;
}

export function cross_term<S>(ctx: { defaultS: () => any }, w1: S[], u1: any, w2: S[], u2: any): S[]
{
  const z1 = full_z(w1, u1);
  const z2 = full_z(w2, u2);
  const [az1, bz1, cz1] = eval_abc(z1);
  const [az2, bz2, cz2] = eval_abc(z2);
  let t = [undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */];
  for (let i = 0n; i < AND_CONS; i += 1n)   {
    const cross = fieldAdd(fieldMul(__clone(az1[Number(i)]), __clone(bz2[Number(i)])), fieldMul(__clone(az2[Number(i)]), __clone(bz1[Number(i)])));
    const sub = fieldAdd(fieldMul(__clone(u1), __clone(cz2[Number(i)])), fieldMul(__clone(u2), __clone(cz1[Number(i)])));
    t[Number(i)] = fieldSub(cross, sub);
  }
  return t;
}

export function cuckoo_insert(seed: bigint[], n: bigint, t: bigint, points: bigint[]): Vec<(bigint | undefined)>
{
  const m = cuckoo_table_size(t);
  let table = [] as any[];
  const max_kicks = fieldMul(8n, BigInt(Math.max(Number(m), Number(16n))));
  for (const item of points)   {
    let x = item;
    for (let kick = 0n; kick < max_kicks; kick += 1n)     {
      let placed = false;
      for (let i = 0n; i < TAU; i += 1n)       {
        const j = hash_i(seed, i, x, m);
        if ((table[Number(j)]) == null)         {
          table[Number(j)] = x;
          placed = true;
          break;
        }
      }
      if (placed)       {
        break;
      }
      const i = (kick % TAU);
      const j = hash_i(seed, i, x, m);
      x = (table[Number(j)].replace(x))!;
    }
  }
  return table;
}

export function cuckoo_table_size(t: bigint): bigint
{
  return BigInt(Math.max(Number(BigInt(Math.imul(Number(t), Number(3n))).div_ceil(2n)), Number(fieldAdd(t, 1n))));
}

export function debug_check_pool_written(written: boolean, slot: bigint)
{
  if (!written)   {
  }
}

export function decode_bools(bytes: bigint[]): [Vec<boolean>, bigint]
{
  let off = 0n;
  const n = Number(take_u32(bytes, off));
  const bits = bytes.slice(Number(off), Number(fieldAdd(off, n))).map((b: any) => !__equals(b, 0n));
  off = fieldAdd(off, n);
  return [bits, off];
}

export function decode_ferret_open(bytes: bigint[]): [bigint[], Vec<boolean>]
{
  let seed = Array.from({length: Number(16n)}, () => 0n);
  (seed).splice(0, (bytes.slice(0, Number(16n))).length, ...(bytes.slice(0, Number(16n))));
  const [choices, _] = decode_bools(bytes.slice(Number(16n)));
  return [seed, choices];
}

export function decode_iknp_corr(bytes: bigint[]): Vec<bigint[]>
{
  let off = 0n;
  const n = Number(take_u32(bytes, off));
  let rows = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    let r = Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n);
    (r).splice(0, (bytes.slice(Number(off), Number(fieldAdd(off, IKNP_KAPPA_BYTES)))).length, ...(bytes.slice(Number(off), Number(fieldAdd(off, IKNP_KAPPA_BYTES)))));
    off = fieldAdd(off, IKNP_KAPPA_BYTES);
    (rows).push(r);
  }
  return rows;
}

export function decode_iknp_u(bytes: bigint[]): IknpUMsg
{
  let off = 0n;
  const n = Number(take_u32(bytes, off));
  let u_cols = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    const [col, used] = decode_bools(bytes.slice(Number(off)));
    off = fieldAdd(off, used);
    (u_cols).push(col);
  }
  return new IknpUMsg({ $fu_cols: u_cols });
}

export function decode_label_16(elements: bigint[]): Result<bigint[], LabelEncodingError>
{
  const WIDTHS: bigint[] = [48n, 48n, 32n];
  const modulus = REFERENCE_PLAINTEXT_MODULUS;
  let label = Array.from({length: Number(16n)}, () => 0n);
  let offset = 0n;
  for (const [index, [element, width]] of elements.map((__a: any, __i: number) => [__a, WIDTHS[__i]] as [typeof __a, any]).map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    if ((element >= modulus))     {
      return new FieldElementOutOfRange({ $findex: index });
    }
    if ((element >= fieldShl(1n, width)))     {
      return new NonCanonicalElement({ $findex: index });
    }
    const bytes = [(element) & 0xFFn, ((element) >> 8n) & 0xFFn, ((element) >> 16n) & 0xFFn, ((element) >> 24n) & 0xFFn];
    const count = Number((width / 8n));
    (label.slice(Number(offset), Number(fieldAdd(offset, count)))).splice(0, (bytes.slice(0, Number(count))).length, ...(bytes.slice(0, Number(count))));
    offset = fieldAdd(offset, count);
  }
  return label;
}

export function decode_lwe_crs(n: bigint, bytes: bigint[]): LweOtCrsDyn
{
  let off = 0n;
  const n_1 = Number(take_u32(bytes, off));
  let a = Array.from({length: Number(n_1)}, () => Array.from({length: Number(n_1)}, () => 0n));
  for (let i = 0n; i < n_1; i += 1n)   {
    for (let j = 0n; j < n_1; j += 1n)     {
      a[Number(i)][Number(j)] = u32_from_le_bytes((bytes.slice(Number(off), Number(fieldAdd(off, 4n))))!);
      off = fieldAdd(off, 4n);
    }
  }
  let h = Array.from({length: Number(n_1)}, () => 0n);
  for (let i = 0n; i < n_1; i += 1n)   {
    h[Number(i)] = u32_from_le_bytes((bytes.slice(Number(off), Number(fieldAdd(off, 4n))))!);
    off = fieldAdd(off, 4n);
  }
  return new LweOtCrsDyn({ $fa: a, $fh: h, $fn: 0n });
}

export function decode_lwe_payload(bytes: bigint[]): LweOtSenderMsgDyn
{
  let off = 0n;
  const u0 = decode_zq_vec(bytes, off);
  const v0 = decode_zq_vec(bytes, off);
  const u1 = decode_zq_vec(bytes, off);
  const v1 = decode_zq_vec(bytes, off);
  return new LweOtSenderMsgDyn({ $fu0: u0, $fv0: v0, $fu1: u1, $fv1: v1 });
}

export function decode_lwe_recv(n: bigint, bytes: bigint[]): LweOtRecvMsgDyn
{
  let pk0 = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    pk0[Number(i)] = u32_from_le_bytes((bytes.slice(Number(fieldMul(i, 4n)), Number(fieldAdd(fieldMul(i, 4n), 4n))))!);
  }
  return new LweOtRecvMsgDyn({ $fpk0: pk0, $fn: 0n });
}

export function decode_mpcot_reg(bytes: bigint[]): MpcotRegSenderMsg
{
  let off = 0n;
  const n = Number(take_u32(bytes, off));
  let blocks = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    const len = Number(take_u32(bytes, off));
    const [msg, used] = decode_spcot(bytes.slice(Number(off), Number(fieldAdd(off, len))));
    off = fieldAdd(off, len);
    (blocks).push(msg);
  }
  return new MpcotRegSenderMsg({ $fblocks: blocks });
}

export function decode_plan(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, bytes: bigint[]): Result<BootstrapPlan, DecodeError>
{
  let reader = new Reader({ $fbytes: bytes, $foffset: 0n });
  if (!__equals(reader.take(4n), MAGIC))   {
    return new DecodeError_BadMagic();
  }
  if (!__equals(reader.byte(), VERSION))   {
    return new DecodeError_UnsupportedVersion();
  }
  const profile = parse_profile(reader.byte());
  const k_max = reader.u32();
  const num_inputs = reader.u32();
  const num_cells = reader.u32();
  const budget = new FailureBudget({ $fper_bootstrap_log2: reader.u32(), $ftotal_log2: reader.u32() });
  const lut_count = reader.count();
  let luts = ([] as any[]);
  for (let _ = 0n; _ < lut_count; _ += 1n)   {
    const bit_count = reader.count();
    const packed_len = bit_count.div_ceil(8n);
    const packed = reader.take(packed_len);
    if ((!__equals((bit_count % 8n), 0n) && packed.last().is_some_and((byte) => !__equals(fieldShr(byte, (bit_count % 8n)), 0n))))     {
      return new DecodeError_UnknownTag();
    }
    let entries = ([] as any[]);
    for (let bit = 0n; bit < bit_count; bit += 1n)     {
      (entries).push(!__equals(fieldBitand(fieldShr(packed[Number((bit / 8n))], (bit % 8n)), 1n), 0n));
    }
    (luts).push(new LutSpec({ $fentries: entries }));
  }
  const layer_count = reader.count();
  let layers = ([] as any[]);
  for (let _ = 0n; _ < layer_count; _ += 1n)   {
    const op_count = reader.count();
    let layer = ([] as any[]);
    for (let _ = 0n; _ < op_count; _ += 1n)     {
      (layer).push(read_op(reader));
    }
    (layers).push(layer);
  }
  const outputs = reader.ids();
  const cell_outputs = reader.ids();
  if (!__equals(reader.$foffset, BigInt(bytes.length)))   {
    return new DecodeError_TrailingBytes();
  }
  const plan = new BootstrapPlan({ $fprofile: profile, $fk_max: k_max, $fluts: luts, $flayers: layers, $fnum_inputs: num_inputs, $fnum_cells: num_cells, $foutputs: outputs, $fcell_outputs: cell_outputs, $fbudget: budget });
  plan.validate();
  return plan;
}

export function decode_spcot(bytes: bigint[]): [SpcotSenderMsg, bigint]
{
  let off = 0n;
  const h = Number(take_u32(bytes, off));
  let ms = ([] as any[]);
  for (let _ = 0n; _ < h; _ += 1n)   {
    const a = take_block(bytes, off);
    const b = take_block(bytes, off);
    (ms).push([a, b]);
  }
  const c = take_block(bytes, off);
  const hv_len = Number(take_u32(bytes, off));
  const hash_v = [...bytes.slice(Number(off), Number(fieldAdd(off, hv_len)))];
  off = fieldAdd(off, hv_len);
  return [new SpcotSenderMsg({ $fms: ms, $fc: c, $fhash_v: hash_v }), off];
}

export function decode_zq_vec(bytes: bigint[], off: bigint): Vec<Zq>
{
  const n = Number(take_u32(bytes, off));
  let v = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    (v).push(u32_from_le_bytes((bytes.slice(Number(off), Number(fieldAdd(off, 4n))))!));
    off = fieldAdd(off, 4n);
  }
  return v;
}

export function decrypt_coords(n: bigint, l: bigint, receiver: LweOtReceiverDyn, u: Zq[], v: Zq[]): bigint[]
{
  let s_dot_u: Zq = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.$fs[Number(i)], u[Number(i)]));
  }
  const quarter = (LWE_Q / 4n);
  const three_quarter = fieldMul(3n, quarter);
  let out = Array.from({length: Number(l)}, () => 0n);
  for (let k = 0n; k < l; k += 1n)   {
    const raw = zq_sub(v[Number(k)], s_dot_u);
    out[Number(k)] = (() => { if (((raw > quarter) && (raw <= three_quarter))) {
  return 1n;
} else {
  return 0n;
} })();
  }
  return out;
}

export function delta_out_full_width(delta_shift: bigint, upscale: bigint): bigint
{
  return fieldShl(1n, fieldAdd(delta_shift, upscale));
}

export function derive_and_q<T>(n: bigint, delta: DeltaDyn<T>, q_a: QDyn<T>, q_b: QDyn<T>, hat: T[]): QDyn<T>
{
  return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const lhs = fieldAdd(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), __clone(hat[Number(i)]));
  return fieldMul(lhs, delta.$fdelta[Number(i)].invert());
})()), $fn: 0n });
}

export function ed_add(p1: any, p2: any): EdPoint
{
  const a = fe_mul(fe_sub(p1.$fy, p1.$fx), fe_sub(p2.$fy, p2.$fx));
  const b = fe_mul(fe_add(p1.$fy, p1.$fx), fe_add(p2.$fy, p2.$fx));
  const c = fe_mul(fe_mul(p1.$ft, D2), p2.$ft);
  const d_ = fe_add(fe_mul(p1.$fz, p2.$fz), fe_mul(p1.$fz, p2.$fz));
  const e = fe_sub(b, a);
  const f = fe_sub(d_, c);
  const g = fe_add(d_, c);
  const h = fe_add(b, a);
  return new EdPoint({ $fx: fe_mul(e, f), $fy: fe_mul(g, h), $ft: fe_mul(e, h), $fz: fe_mul(f, g) });
}

export function ed_double(p: any): EdPoint
{
  const a = fe_sq(p.$fx);
  const b = fe_sq(p.$fy);
  const c = fe_add(fe_sq(p.$fz), fe_sq(p.$fz));
  const d_ = fe_neg(a);
  const xy_sum = fe_add(p.$fx, p.$fy);
  const e = fe_sub(fe_sub(fe_sq(xy_sum), a), b);
  const g = fe_add(d_, b);
  const f = fe_sub(g, c);
  const h = fe_sub(d_, b);
  return new EdPoint({ $fx: fe_mul(e, f), $fy: fe_mul(g, h), $ft: fe_mul(e, h), $fz: fe_mul(f, g) });
}

export function ed_mul_cofactor(p: any): EdPoint
{
  return ed_double(ed_double(ed_double(p)));
}

export function ed_neg(p: any): EdPoint
{
  return new EdPoint({ $fx: fe_neg(p.$fx), $fy: p.$fy, $fz: p.$fz, $ft: fe_neg(p.$ft) });
}

export function ed_scalar_mul(p: any, k: bigint[]): EdPoint
{
  let acc = EdPoint.IDENTITY;
  for (const byte_idx of (Array.from({length: Number(32n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    for (const bit of (Array.from({length: Number(8n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      acc = ed_double(acc);
      const b = fieldBitand(fieldShr(k[Number(byte_idx)], bit), 1n);
      if (__equals(b, 1n))       {
        acc = ed_add(acc, p);
      }
    }
  }
  return acc;
}

export function embed_up(from: bigint, to: bigint, x: bigint): bigint
{
  return reduce(fieldShl(x, fieldSub(to, from_)));
}

export function encode_bits(seed: bigint[], k: bigint, n: bigint, u: boolean[]): Vec<boolean>
{
  let x = [] as any[];
  for (let j = 0n; j < n; j += 1n)   {
    const rows = column_rows(seed, k, j);
    let acc = false;
    for (const row of rows)     {
      acc = fieldBitxor(acc, u[Number(row)]);
    }
    x[Number(j)] = acc;
  }
  return x;
}

export function encode_blocks(seed: bigint[], k: bigint, n: bigint, v: Block[]): Vec<Block>
{
  let y = [] as any[];
  for (let j = 0n; j < n; j += 1n)   {
    const rows = column_rows(seed, k, j);
    let acc = Array.from({length: Number(16n)}, () => 0n);
    for (const row of rows)     {
      for (let b = 0n; b < 16n; b += 1n)       {
        acc[Number(b)] = fieldBitxor(acc[Number(b)], v[Number(row)][Number(b)]);
      }
    }
    y[Number(j)] = acc;
  }
  return y;
}

export function encode_bools(bits: boolean[]): Vec<bigint>
{
  let buf = ([] as any[]);
  push_u32(buf, Number(BigInt(bits.length)));
  buf.push(...(bits.map((b: any) => ((b) & 0xFFn))));
  return buf;
}

export function encode_ferret_open(lpn_seed: bigint[], choices: boolean[]): Vec<bigint>
{
  let buf = [] as any[];
  buf.push(...(lpn_seed));
  buf.push(...(encode_bools(choices)));
  return buf;
}

export function encode_iknp_corr(rows: bigint[][]): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, Number(BigInt(rows.length)));
  for (const r of rows)   {
    buf.push(...(r));
  }
  return buf;
}

export function encode_iknp_u(msg: any): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, Number(BigInt(msg.$fu_cols.length)));
  for (const col of msg.$fu_cols)   {
    buf.push(...(encode_bools(col)));
  }
  return buf;
}

export function encode_label_16(label: bigint[]): bigint[]
{
  const first = u64_from_le_bytes([label[Number(0n)], label[Number(1n)], label[Number(2n)], label[Number(3n)], label[Number(4n)], label[Number(5n)], 0n, 0n]);
  const second = u64_from_le_bytes([label[Number(6n)], label[Number(7n)], label[Number(8n)], label[Number(9n)], label[Number(10n)], label[Number(11n)], 0n, 0n]);
  const third = BigInt(u32_from_le_bytes([label[Number(12n)], label[Number(13n)], label[Number(14n)], label[Number(15n)]]));
  return [first, second, third];
}

export function encode_lwe_crs(n: bigint, crs: LweOtCrsDyn): Vec<bigint>
{
  let buf = ([] as any[]);
  push_u32(buf, Number(n));
  for (let i = 0n; i < n; i += 1n)   {
    for (let j = 0n; j < n; j += 1n)     {
      buf.push(...([(crs.$fa[Number(i)][Number(j)]) & 0xFFn, ((crs.$fa[Number(i)][Number(j)]) >> 8n) & 0xFFn, ((crs.$fa[Number(i)][Number(j)]) >> 16n) & 0xFFn, ((crs.$fa[Number(i)][Number(j)]) >> 24n) & 0xFFn]));
    }
  }
  for (let i = 0n; i < n; i += 1n)   {
    buf.push(...([(crs.$fh[Number(i)]) & 0xFFn, ((crs.$fh[Number(i)]) >> 8n) & 0xFFn, ((crs.$fh[Number(i)]) >> 16n) & 0xFFn, ((crs.$fh[Number(i)]) >> 24n) & 0xFFn]));
  }
  return buf;
}

export function encode_lwe_payload(msg: any): Vec<bigint>
{
  let buf = [] as any[];
  buf.push(...(encode_zq_vec(msg.$fu0)));
  buf.push(...(encode_zq_vec(msg.$fv0)));
  buf.push(...(encode_zq_vec(msg.$fu1)));
  buf.push(...(encode_zq_vec(msg.$fv1)));
  return buf;
}

export function encode_lwe_recv(n: bigint, msg: LweOtRecvMsgDyn): Vec<bigint>
{
  let buf = ([] as any[]);
  for (let i = 0n; i < n; i += 1n)   {
    buf.push(...([(msg.$fpk0[Number(i)]) & 0xFFn, ((msg.$fpk0[Number(i)]) >> 8n) & 0xFFn, ((msg.$fpk0[Number(i)]) >> 16n) & 0xFFn, ((msg.$fpk0[Number(i)]) >> 24n) & 0xFFn]));
  }
  return buf;
}

export function encode_mpcot_reg(msg: any): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, Number(BigInt(msg.$fblocks.length)));
  for (const b of msg.$fblocks)   {
    const inner = encode_spcot(b);
    push_u32(buf, Number(BigInt(inner.length)));
    buf.push(...(inner));
  }
  return buf;
}

export function encode_plan(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, plan: any): Result<Vec<bigint>, EncodeError>
{
  plan.validate();
  if (((((((BigInt(plan.$fluts.length) > MAX_ITEMS) || (BigInt(plan.$flayers.length) > MAX_ITEMS)) || (BigInt(plan.$foutputs.length) > MAX_ITEMS)) || (BigInt(plan.$fcell_outputs.length) > MAX_ITEMS)) || plan.$flayers.any((layer) => (BigInt(layer.length) > MAX_ITEMS))) || plan.$fluts.any((lut) => (BigInt(lut.$fentries.length) > MAX_ITEMS))))   {
    return new EncodeError_TooLarge();
  }
  let bytes = [] as any[];
  bytes.push(...(MAGIC));
  (bytes).push(VERSION);
  (bytes).push(profile_tag(plan.$fprofile));
  put_u32(bytes, plan.$fk_max);
  put_u32(bytes, plan.$fnum_inputs);
  put_u32(bytes, plan.$fnum_cells);
  put_u32(bytes, plan.$fbudget.$fper_bootstrap_log2);
  put_u32(bytes, plan.$fbudget.$ftotal_log2);
  put_u32(bytes, Number(BigInt(plan.$fluts.length)));
  for (const lut of plan.$fluts)   {
    put_u32(bytes, Number(BigInt(lut.$fentries.length)));
    for (const chunk of __chunks(lut.$fentries, Number(8n)))     {
      let packed = 0n;
      for (const [bit, entry] of chunk.map((val: any, i: number) => [i, val] as [number, typeof val]))       {
        packed = fieldBitor(packed, fieldShl(((entry) & 0xFFn), bit));
      }
      (bytes).push(packed);
    }
  }
  put_u32(bytes, Number(BigInt(plan.$flayers.length)));
  for (const layer of plan.$flayers)   {
    put_u32(bytes, Number(BigInt(layer.length)));
    for (const op of layer)     {
      return (() => { const __match = op; if (true /* pattern Struct { kind: Custom("PlanOp::Const"), fields: [("out", Ident { mutable: false, name: "out", subpat: None }), ("value", Ident { mutable: false, name: "value", subpat: None })], rest: false } */) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
  (bytes).push(0n);
  put_u32(bytes, out);
  (bytes).push(((value) & 0xFFn));
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Not"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (bytes).push(1n);
  put_u32(bytes, input);
  put_u32(bytes, out);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Lut"), fields: [("inputs", Ident { mutable: false, name: "inputs", subpat: None }), ("table", Ident { mutable: false, name: "table", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  (bytes).push(2n);
  put_u32(bytes, Number(BigInt(inputs.length)));
  for (const input of inputs)   {
    put_u32(bytes, input);
  }
  put_u32(bytes, table);
  put_u32(bytes, out);
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::CircuitBootstrap"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (bytes).push(3n);
  put_u32(bytes, input);
  put_u32(bytes, out);
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
  (bytes).push(4n);
  put_u32(bytes, sel);
  put_u32(bytes, then_cell);
  put_u32(bytes, else_cell);
  put_u32(bytes, out);
})(); } })();
    }
  }
  put_ids(bytes, plan.$foutputs);
  put_ids(bytes, plan.$fcell_outputs);
  return bytes;
}

export function encode_receiver_only(receiver: any, prep: any, r: bigint[][]): RecvLpn
{
  const k = receiver.$fparams.$fk;
  const n = receiver.$fparams.$fn;
  const m = receiver.$fparams.seed_cot_count(false);
  const x_bits = encode_bits(prep.$flpn_seed, k, n, receiver.$fseed.$fu.slice(0, Number(k)));
  const z_lpn = encode_blocks(prep.$flpn_seed, k, n, receiver.$fseed.$fw.slice(0, Number(k)));
  let x = ([] as any[]);
  let z = ([] as any[]);
  for (let j = 0n; j < n; j += 1n)   {
    (x).push(fieldBitxor(x_bits[Number(j)], prep.$fe[Number(j)]));
    (z).push(xor_block(z_lpn[Number(j)], r[Number(j)]));
  }
  return new RecvLpn({ $fseed: new FerretReceiverSeed({ $fu: [...x.slice(0, Number(m))], $fw: [...z.slice(0, Number(m))] }), $fx: [...x.slice(Number(m))], $fz: [...z.slice(Number(m))] });
}

export function encode_sender_only(sender: any, lpn_seed: bigint[], s: bigint[][]): SenderLpn
{
  const k = sender.$fparams.$fk;
  const n = sender.$fparams.$fn;
  const m = sender.$fparams.seed_cot_count(false);
  const y_lpn = encode_blocks(lpn_seed, k, n, sender.$fseed.$fq.slice(0, Number(k)));
  let y = ([] as any[]);
  for (let j = 0n; j < n; j += 1n)   {
    (y).push(xor_block(y_lpn[Number(j)], s[Number(j)]));
  }
  return new SenderLpn({ $fseed_q: [...y.slice(0, Number(m))], $femit: y.slice(Number(m)) });
}

export function encode_spcot(msg: any): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, Number(BigInt(msg.$fms.length)));
  for (const pair of msg.$fms)   {
    push_block(buf, pair[Number(0n)]);
    push_block(buf, pair[Number(1n)]);
  }
  push_block(buf, msg.$fc);
  push_u32(buf, Number(BigInt(msg.$fhash_v.length)));
  buf.push(...(msg.$fhash_v));
  return buf;
}

export function encode_zq_vec(v: Zq[]): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, Number(BigInt(v.length)));
  for (const x of v)   {
    buf.push(...([(x) & 0xFFn, ((x) >> 8n) & 0xFFn, ((x) >> 16n) & 0xFFn, ((x) >> 24n) & 0xFFn]));
  }
  return buf;
}

export function encrypt_block(key: bigint[], plain: bigint[]): bigint[]
{
  const round_keys = key_expansion(key);
  let state = plain;
  add_round_key(state, round_keys[Number(0n)]);
  for (let r = 1n; r < NR; r += 1n)   {
    sub_bytes(state);
    shift_rows(state);
    mix_columns(state);
    add_round_key(state, round_keys[Number(r)]);
  }
  sub_bytes(state);
  shift_rows(state);
  add_round_key(state, round_keys[Number(NR)]);
  return state;
}

export function encrypt_branch<R>(n: bigint, l: bigint, rng: any, crs: LweOtCrsDyn, pk: Zq[], msg: bigint[]): [Zq[], Zq[]]
{
  let r = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    r[Number(i)] = sample_noise(rng);
  }
  let u = Array.from({length: Number(n)}, () => 0n);
  for (let j = 0n; j < n; j += 1n)   {
    let acc: Zq = 0n;
    for (let i = 0n; i < n; i += 1n)     {
      acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], r[Number(i)]));
    }
    acc = zq_add(acc, sample_noise(rng));
    u[Number(j)] = acc;
  }
  let base: Zq = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    base = zq_add(base, zq_mul(pk[Number(i)], r[Number(i)]));
  }
  const half_q = (LWE_Q / 2n);
  let v = Array.from({length: Number(l)}, () => 0n);
  for (let k = 0n; k < l; k += 1n)   {
    const plain = (() => { if (__equals(fieldBitand(msg[Number(k)], 1n), 1n)) {
  return half_q;
} else {
  return 0n;
} })();
    v[Number(k)] = zq_add(zq_add(base, sample_noise(rng)), plain);
  }
  return [u, v];
}

export function encrypt_branch_dyn<R>(n: bigint, rng: any, crs: LweOtCrsDyn, pk: Zq[], msg_bits: bigint[]): [Vec<Zq>, Vec<Zq>]
{
  let r = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    r[Number(i)] = sample_noise(rng);
  }
  let u = [] as any[];
  for (let j = 0n; j < n; j += 1n)   {
    let acc: Zq = 0n;
    for (let i = 0n; i < n; i += 1n)     {
      acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], r[Number(i)]));
    }
    acc = zq_add(acc, sample_noise(rng));
    u[Number(j)] = acc;
  }
  let base: Zq = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    base = zq_add(base, zq_mul(pk[Number(i)], r[Number(i)]));
  }
  const half_q = (LWE_Q / 2n);
  let v = [] as any[];
  for (const [k, bit] of msg_bits.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const plain = (() => { if (__equals(fieldBitand(bit, 1n), 1n)) {
  return half_q;
} else {
  return 0n;
} })();
    v[Number(k)] = zq_add(zq_add(base, sample_noise(rng)), plain);
  }
  return [u, v];
}

export function encrypt_scaled_poly<R>(big_n: bigint, log_q: bigint, eta: bigint, msg: bigint[], level: bigint, base_log: bigint, sk: BinfheRlweSecretKeyDyn, rng: any): BinfheRlweCiphertextDyn
{
  const g = level_factor(base_log, level);
  let scaled = Array.from({length: Number(big_n)}, () => 0n);
  for (let i = 0n; i < big_n; i += 1n)   {
    scaled[Number(i)] = mul_exact(msg[Number(i)], g);
  }
  return binfhe_rlwe_encrypt_poly(eta, scaled, sk, rng);
}

export function ensure<R>(rng: any, sender: any, receiver: any, need: bigint)
{
  const watermark = sender.$fparams.seed_cot_count(false);
  while (((sender.remaining() < need) || ((sender.remaining() - (need)) < watermark)))   {
    const before = sender.remaining();
    refill(rng, sender, receiver);
  }
}

export function ensure_receiver<R, Io>(rng: any, receiver: any, io: any, need: bigint)
{
  const watermark = receiver.$fparams.seed_cot_count(false);
  while (((receiver.remaining() < need) || ((receiver.remaining() - (need)) < watermark)))   {
    stack_refill_receiver(rng, receiver, io);
  }
}

export function ensure_sender<R, Io>(rng: any, sender: any, io: any, need: bigint)
{
  const watermark = sender.$fparams.seed_cot_count(false);
  while (((sender.remaining() < need) || ((sender.remaining() - (need)) < watermark)))   {
    stack_refill_sender(rng, sender, io);
  }
}

export function eval_abc<S>(ctx: { defaultS: () => any, UClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, z: S[]): [S[], S[], S[]]
{
  const az = [__clone(z[Number(K_A)]), __clone(z[Number(K_C)]), fieldSub(fieldAdd(__clone(z[Number(P1)]), __clone(z[Number(V_HAT)])), __clone(z[Number(P2)]))];
  const bz = [__clone(z[Number(K_B)]), __clone(z[Number(DELTA)]), __clone(z[Number(ctx.UClass)])];
  const cz = [__clone(z[Number(P1)]), __clone(z[Number(P2)]), undefined as any /* S::default() */];
  return [az, bz, cz];
}

export function eval_not(c: bigint, q: bigint): bigint
{
  const true_v = (q / 4n);
  return BigInt(fieldSub((true_v as unknown as bigint), (c as unknown as bigint)).rem_euclid((q as unknown as bigint)));
}

export function evaluate_certificate(cert: any, inputs: bigint[], q: bigint): bigint
{
  const prepared = (cert.$fprepare(inputs, q) % q);
  const [lo, hi] = cert.$finterval_true;
  const signed_eighth: bigint = (() => { if (in_interval_mod(prepared, lo, hi, q)) {
  return ((q / 8n) as unknown as bigint);
} else {
  return -((q / 8n) as unknown as bigint);
} })();
  const restored = fieldAdd(signed_eighth, ((q / 8n) as unknown as bigint));
  return BigInt(restored.rem_euclid((q as unknown as bigint)));
}

export function evaluate_gate(cert_eighths: any, inputs: bigint[], q: bigint): bigint
{
  const scale = (q / 8n);
  const scaled = new GateCertificate({ $fname: cert_eighths.$fname, $farity: cert_eighths.$farity, $fprepare: cert_eighths.$fprepare, $finterval_true: [BigInt(Math.imul(Number(cert_eighths.$finterval_true[0]), Number(scale))), BigInt(Math.imul(Number(cert_eighths.$finterval_true[1]), Number(scale)))] });
  return evaluate_certificate(scaled, inputs, q);
}

export function execute_plan(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, priv_ell: bigint, priv_base_log: bigint, plan: any, inputs: BinfheLweCiphertextDyn[], cells: BinfheRlweCiphertextDyn[], bk: BinfheBootstrappingKeyDyn, cbk: CircuitBootstrappingKeyDyn): [Vec<BinfheLweCiphertextDyn>, Vec<BinfheRlweCiphertextDyn>]
{
  const delta = wire_delta(log_q_lwe, Number(plan.$fk_max));
  let wires: Vec<BinfheLweCiphertextDyn> = [...inputs];
  let rgsws: Vec<BinfheRgswCiphertextDyn> = [] as any[];
  let cell_arena: Vec<BinfheRlweCiphertextDyn> = [...cells];
  for (const layer of plan.$flayers)   {
    for (const op of layer)     {
      return (() => { const __match = op; if (true /* pattern Struct { kind: Custom("PlanOp::Const"), fields: [("out", Ident { mutable: false, name: "out", subpat: None }), ("value", Ident { mutable: false, name: "value", subpat: None })], rest: false } */) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
  (wires).push(binfhe_trivial(value, delta));
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Not"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (wires).push(binfhe_not(wires[Number(Number(input))], delta));
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::Lut"), fields: [("inputs", Ident { mutable: false, name: "inputs", subpat: None }), ("table", Ident { mutable: false, name: "table", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  const spec = plan.$fluts[Number(Number(table))];
  const arity = Number(Math.clz32((BigInt(spec.$fentries.length)) & -((BigInt(spec.$fentries.length)) | 0)));
  let cts: BinfheLweCiphertextDyn[] = Array.from({length: Number(MAX_LUT_ARITY)}, () => binfhe_trivial(false, 0n));
  for (const [j, w] of inputs.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    cts[Number(j)] = wires[Number(Number(w))];
  }
  (wires).push(binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, cts.slice(0, Number(arity)), spec.$fentries, Number(plan.$fk_max), bk));
})(); } else if (true /* pattern Struct { kind: Custom("PlanOp::CircuitBootstrap"), fields: [("input", Ident { mutable: false, name: "input", subpat: None }), ("out", Ident { mutable: false, name: "out", subpat: None })], rest: false } */) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (rgsws).push(circuit_bootstrap(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, ks_ell, priv_ell, priv_base_log, wires[Number(Number(input))], cbk, Number(plan.$fk_max)));
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
  const out_cell = binfhe_rgsw_cmux(rgsws[Number(Number(sel))], cell_arena[Number(Number(then_cell))], cell_arena[Number(Number(else_cell))]);
  (cell_arena).push(out_cell);
})(); } })();
    }
  }
  return [wires, cell_arena];
}

export function expand_challenge_to_deltas(chall_1: bigint[], tau: bigint, n: bigint): Vec<bigint>
{
  return Array.from({length: Number(tau - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const byte = Number(chall_1[Number((i % BigInt(chall_1.length)))]);
  return (byte % n);
})());
}

export function expand_full(depth: bigint, seed: any): [Vec<Block>, Vec<Block[]>]
{
  let level: Vec<Block> = [seed];
  let sums = ([] as any[]);
  for (let _d = 0n; _d < depth; _d += 1n)   {
    let next = ([] as any[]);
    let k0 = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
    let k1 = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
    for (const node of level)     {
      const [l, r] = g_double(node);
      for (let i = 0n; i < KAPPA_BYTES; i += 1n)       {
        k0[Number(i)] = fieldBitxor(k0[Number(i)], l[Number(i)]);
        k1[Number(i)] = fieldBitxor(k1[Number(i)], r[Number(i)]);
      }
      (next).push(l);
      (next).push(r);
    }
    (sums).push([k0, k1]);
    level = next;
  }
  return [level, sums];
}

export function expand_partial(depth: bigint, sums: Block[], alpha: bigint): Vec<Block>
{
  const n = fieldShl(1n, depth);
  let level: Vec<Block> = [] as any[];
  let offset = 0n;
  for (let d = 0n; d < depth; d += 1n)   {
    const select = bit_msb(alpha, depth, d);
    recover_sibling(level, sums[Number(d)], offset, select);
    if (__equals(fieldAdd(d, 1n), depth))     {
      break;
    }
    let next = [] as any[];
    for (const [j, node] of level.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const [l, r] = g_double(node);
      next[Number(fieldMul(2n, j))] = l;
      next[Number(fieldAdd(fieldMul(2n, j), 1n))] = r;
    }
    offset = fieldShl(fieldAdd(offset, Number(select)), 1n);
    level = next;
  }
  return level;
}

export function exponent(log_q_lwe: bigint, big_n: bigint, x: bigint): bigint
{
  return fieldBitand(Number(x), fieldSub(fieldMul(2n, big_n), 1n));
}

export function external_product(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, rgsw: RgswCiphertextDyn, rlwe: RlweCiphertextDyn): RlweCiphertextDyn
{
  const a_decomp = poly_decompose(big_n, bs_ell, bs_bg_log, rlwe.$fa);
  const b_decomp = poly_decompose(big_n, bs_ell, bs_bg_log, rlwe.$fb);
  let out_a = Array.from({length: Number(big_n)}, () => 0n);
  let out_b = Array.from({length: Number(big_n)}, () => 0n);
  for (let j = 0n; j < bs_ell; j += 1n)   {
    const row = rgsw.$frows[Number(j)];
    const prod_a0 = poly_mul_neg(a_decomp[Number(j)], row.$frlwe0.$fa);
    const prod_a1 = poly_mul_neg(a_decomp[Number(j)], row.$frlwe0.$fb);
    const prod_b0 = poly_mul_neg(b_decomp[Number(j)], row.$frlwe1.$fa);
    const prod_b1 = poly_mul_neg(b_decomp[Number(j)], row.$frlwe1.$fb);
    for (let k = 0n; k < big_n; k += 1n)     {
      out_a[Number(k)] = wrappingAdd(wrappingAdd(out_a[Number(k)], prod_a0[Number(k)], 32), prod_b0[Number(k)], 32);
      out_b[Number(k)] = wrappingAdd(wrappingAdd(out_b[Number(k)], prod_a1[Number(k)], 32), prod_b1[Number(k)], 32);
    }
  }
  return new RlweCiphertextDyn({ $fa: out_a, $fb: out_b, $fbig_n: 0n });
}

export function fe_add(a: any, b: any): Fe25519
{
  let r = Array.from({length: Number(4n)}, () => 0n);
  let c: bigint = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const v = fieldAdd(fieldAdd((a[0][Number(i)] as unknown as bigint), (b[0][Number(i)] as unknown as bigint)), (c as unknown as bigint));
    r[Number(i)] = BigInt(v);
    c = BigInt(fieldShr(v, 64n));
  }
  if (!__equals(c, 0n))   {
    let c2: bigint = fieldMul((c as unknown as bigint), 38n);
    for (let i = 0n; i < 4n; i += 1n)     {
      const v = fieldAdd((r[Number(i)] as unknown as bigint), c2);
      r[Number(i)] = BigInt(v);
      c2 = fieldShr(v, 64n);
    }
  }
  return fe_canonicalize(r);
}

export function fe_canonicalize(a: bigint[]): Fe25519
{
  let x = a;
  for (let _ = 0n; _ < 2n; _ += 1n)   {
    let tmp = Array.from({length: Number(4n)}, () => 0n);
    let borrow: bigint = 0n;
    for (let i = 0n; i < 4n; i += 1n)     {
      const [r1, b1] = [wrappingSub(x[Number(i)], P_LIMBS[Number(i)], 32), false];
      const [r2, b2] = [wrappingSub(r1, borrow, 32), false];
      tmp[Number(i)] = r2;
      borrow = fieldBitor(BigInt(b1), BigInt(b2));
    }
    if (__equals(borrow, 0n))     {
      x = tmp;
    }
  }
  return new Fe25519(x);
}

export function fe_const(limbs: bigint[]): Fe25519
{
  return new Fe25519(limbs);
}

export function fe_from_bytes_le(b: bigint[]): Fe25519
{
  let limbs = Array.from({length: Number(4n)}, () => 0n);
  for (let i = 0n; i < 4n; i += 1n)   {
    let chunk = Array.from({length: Number(8n)}, () => 0n);
    (chunk).splice(0, (b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).length, ...(b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))));
    limbs[Number(i)] = u64_from_le_bytes(chunk);
  }
  return reduce_wide([limbs[Number(0n)], limbs[Number(1n)], limbs[Number(2n)], limbs[Number(3n)], 0n, 0n, 0n, 0n]);
}

export function fe_invert(a: any): Fe25519
{
  const exp_limbs: bigint[] = [18446744073709551595n, 18446744073709551615n, 18446744073709551615n, 9223372036854775807n];
  let acc = Fe25519.ONE;
  for (const limb_idx of (Array.from({length: Number(4n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    for (const bit of (Array.from({length: Number(64n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      acc = fe_sq(acc);
      const b = fieldBitand(fieldShr(exp_limbs[Number(limb_idx)], bit), 1n);
      if (__equals(b, 1n))       {
        acc = fe_mul(acc, a);
      }
    }
  }
  return acc;
}

export function fe_mul(a: any, b: any): Fe25519
{
  const wide = mul_4x4(a[0], b[0]);
  return reduce_wide(wide);
}

export function fe_neg(a: any): Fe25519
{
  return (() => { if (a.is_zero()) {
  return Fe25519.ZERO;
} else {
  let neg = Array.from({length: Number(4n)}, () => 0n);
  let borrow: bigint = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const [r1, br1] = [wrappingSub(P_LIMBS[Number(i)], a[0][Number(i)], 32), false];
    const [r2, br2] = [wrappingSub(r1, borrow, 32), false];
    neg[Number(i)] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return new Fe25519(neg);
} })();
}

export function fe_pow(base: any, exp: bigint[]): Fe25519
{
  let acc = Fe25519.ONE;
  for (const limb_idx of (Array.from({length: Number(4n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    for (const bit of (Array.from({length: Number(64n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      acc = fe_sq(acc);
      if (__equals(fieldBitand(fieldShr(exp[Number(limb_idx)], bit), 1n), 1n))       {
        acc = fe_mul(acc, base);
      }
    }
  }
  return acc;
}

export function fe_sq(a: any): Fe25519
{
  return fe_mul(a, a);
}

export function fe_sqrt(w: any): (Fe25519 | undefined)
{
  const E: bigint[] = [18446744073709551614n, 18446744073709551615n, 18446744073709551615n, 1152921504606846975n];
  const c = fe_pow(w, E);
  const c2 = fe_sq(c);
  return (() => { if (__equals(c2, w)) {
  return c;
} else if (__equals(c2, fe_neg(w))) {
  return fe_mul(c, sqrt_m1());
} else {
  return undefined;
} })();
}

export function fe_sub(a: any, b: any): Fe25519
{
  let neg_b = Array.from({length: Number(4n)}, () => 0n);
  let borrow: bigint = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const [r1, br1] = [wrappingSub(P_LIMBS[Number(i)], b[0][Number(i)], 32), false];
    const [r2, br2] = [wrappingSub(r1, borrow, 32), false];
    neg_b[Number(i)] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return fe_add(a, new Fe25519(neg_b));
}

export function ferret_extend<R>(rng: any, params: any, sender_seed: any, receiver_seed: any): FerretExtendOut
{
  const m = params.seed_cot_count(false);
  const prep = ferret_prepare_receiver(rng, params, receiver_seed);
  const [s, mpcot] = ferret_sender_mpcot(rng, params, sender_seed, prep.$fchoices);
  const r = ferret_receiver_mpcot(params, prep, receiver_seed, mpcot);
  return ferret_finish(params, sender_seed, receiver_seed, prep, s, r);
}

export function ferret_extend_uni<R>(rng: any, params: any, hash_seed: bigint[], sender_seed: any, receiver_seed: any): FerretExtendOut
{
  const m_seed = uni_seed_cot_count(hash_seed, params);
  const k = params.$fk;
  const heights = uni_spcot_heights(hash_seed, params.$fn, params.$ft);
  const points = sample_uniform_points(rng, params.$fn, params.$ft);
  const table = cuckoo_insert(hash_seed, params.$fn, params.$ft, points);
  let e = [] as any[];
  for (const slot of table)   {
    return (() => { const __match = slot; if (__match !== null && __match !== undefined) { const x = __match;
return (() => {
  e[Number(x)] = true;
})(); } else { return (() => {
})(); } })();
  }
  const cot_r = split_cot_chunks(receiver_seed.$fu.slice(Number(k)), heights);
  const choices = mpcot_uni_choice_bits(params, hash_seed, table, cot_r);
  const cot_q = split_cot_chunks(sender_seed.$fq.slice(Number(k)), heights);
  const [s, mpcot] = mpcot_uni_sender(rng, sender_seed.$fdelta, params, hash_seed, cot_q, choices);
  const cot_t = split_cot_chunks(receiver_seed.$fw.slice(Number(k)), heights);
  const r = mpcot_uni_receiver(params, table, cot_t, mpcot);
  const prep = new FerretPrep({ $falphas: points, $fe: e, $flpn_seed: sample_seed(rng), $fchoices: [] as any[] });
  const n = params.$fn;
  const v_lpn = sender_seed.$fq.slice(0, Number(k));
  const u_lpn = receiver_seed.$fu.slice(0, Number(k));
  const w_lpn = receiver_seed.$fw.slice(0, Number(k));
  const y_lpn = encode_blocks(prep.$flpn_seed, k, n, v_lpn);
  const x_bits = encode_bits(prep.$flpn_seed, k, n, u_lpn);
  const z_lpn = encode_blocks(prep.$flpn_seed, k, n, w_lpn);
  let y = ([] as any[]);
  let x = ([] as any[]);
  let z = ([] as any[]);
  for (let j = 0n; j < n; j += 1n)   {
    (y).push(xor_block(y_lpn[Number(j)], s[Number(j)]));
    (x).push(fieldBitxor(x_bits[Number(j)], prep.$fe[Number(j)]));
    (z).push(xor_block(z_lpn[Number(j)], r[Number(j)]));
  }
  return new FerretExtendOut({ $fsender_out: [...y.slice(Number(m_seed))], $frecv_x: [...x.slice(Number(m_seed))], $frecv_z: [...z.slice(Number(m_seed))], $fsender_seed: new FerretSenderSeed({ $fdelta: sender_seed.$fdelta, $fq: [...y.slice(0, Number(m_seed))] }), $freceiver_seed: new FerretReceiverSeed({ $fu: [...x.slice(0, Number(m_seed))], $fw: [...z.slice(0, Number(m_seed))] }) });
}

export function ferret_finish(params: any, sender_seed: any, receiver_seed: any, prep: any, s: Block[], r: Block[]): FerretExtendOut
{
  const n = params.$fn;
  const k = params.$fk;
  const m = params.seed_cot_count(false);
  const v_lpn = sender_seed.$fq.slice(0, Number(k));
  const u_lpn = receiver_seed.$fu.slice(0, Number(k));
  const w_lpn = receiver_seed.$fw.slice(0, Number(k));
  const y_lpn = encode_blocks(prep.$flpn_seed, k, n, v_lpn);
  const x_bits = encode_bits(prep.$flpn_seed, k, n, u_lpn);
  const z_lpn = encode_blocks(prep.$flpn_seed, k, n, w_lpn);
  let y = ([] as any[]);
  let x = ([] as any[]);
  let z = ([] as any[]);
  for (let j = 0n; j < n; j += 1n)   {
    (y).push(xor_block(y_lpn[Number(j)], s[Number(j)]));
    (x).push(fieldBitxor(x_bits[Number(j)], prep.$fe[Number(j)]));
    (z).push(xor_block(z_lpn[Number(j)], r[Number(j)]));
  }
  const sender_seed_1 = new FerretSenderSeed({ $fdelta: sender_seed.$fdelta, $fq: [...y.slice(0, Number(m))] });
  const receiver_seed_1 = new FerretReceiverSeed({ $fu: [...x.slice(0, Number(m))], $fw: [...z.slice(0, Number(m))] });
  return new FerretExtendOut({ $fsender_out: [...y.slice(Number(m))], $frecv_x: [...x.slice(Number(m))], $frecv_z: [...z.slice(Number(m))], $fsender_seed: sender_seed_1, $freceiver_seed: receiver_seed_1 });
}

export function ferret_prepare_receiver<R>(rng: any, params: any, receiver_seed: any): FerretPrep
{
  const n = params.$fn;
  const t = params.$ft;
  const k = params.$fk;
  const alphas = sample_regular_noise(rng, n, t);
  let e = [] as any[];
  for (const [i, a] of alphas.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    e[Number(fieldAdd(fieldMul(i, params.splen()), a))] = true;
  }
  const lpn_seed = sample_seed(rng);
  const cot_r = receiver_seed.$fu.slice(Number(k));
  const choices = mpcot_reg_choice_bits(n, t, alphas, cot_r);
  return new FerretPrep({ $falphas: alphas, $fe: e, $flpn_seed: lpn_seed, $fchoices: choices });
}

export function ferret_receiver_mpcot(params: any, prep: any, receiver_seed: any, mpcot: any): Vec<Block>
{
  const cot_t = receiver_seed.$fw.slice(Number(params.$fk));
  return mpcot_reg_receiver(params.$fn, params.$ft, prep.$falphas, cot_t, mpcot);
}

export function ferret_sender_mpcot<R>(rng: any, params: any, sender_seed: any, choices: boolean[]): [Vec<Block>, MpcotRegSenderMsg]
{
  const cot_q = sender_seed.$fq.slice(Number(params.$fk));
  return mpcot_reg_sender(rng, sender_seed.$fdelta, params.$fn, params.$ft, cot_q, choices);
}

export function field_from_block(b: any): Galois128
{
  return Galois128(u128_from_le_bytes(b));
}

export function field_invert<T>(ctx: { defaultT: () => any }, a: any, c: any, w: bigint): T
{
  if (__equals(a, ctx.defaultT()))   {
    return ctx.defaultT();
  }
  const e = fieldSub(w, 1n);
  let r = __clone(a);
  let k: bigint = 1n;
  const msb = fieldSub(31n, Math.clz32(e));
  for (const bit_pos of (Array.from({length: Number(msb - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    let tmp = __clone(r);
    for (let _ = 0n; _ < k; _ += 1n)     {
      tmp = field_square(tmp, __clone(c));
    }
    r = field_mul(tmp, r, __clone(c));
    k = fieldMul(k, 2n);
    if (__equals(fieldBitand(fieldShr(e, bit_pos), 1n), 1n))     {
      r = field_mul(field_square(r, __clone(c)), __clone(a), __clone(c));
      k = fieldAdd(k, 1n);
    }
  }
  return field_square(r, c);
}

export function field_mul(...__args: any[]): any {
  if (__args.length === 4) {
    const ctx = __args[0];
    const a = __args[1];
    const b = __args[2];
    const c = __args[3];
    return (() => {
  let p: T = ctx.defaultT();
  let a_1 = a;
  let b_1 = b;
  const h = fieldShl(ctx.TClass.from(1n), Number(fieldSub(fieldShl(ctx.sizeOfT, 3n), 1n)));
  for (let _ = 0n; _ < fieldShl(ctx.sizeOfT, 3n); _ += 1n)   {
    if (!__equals(fieldBitand(__clone(b_1), ctx.TClass.from(1n)), ctx.defaultT()))     {
      p = fieldBitxor(p, __clone(a_1));
    }
    const high_bit = fieldBitand(__clone(a_1), __clone(h));
    a_1 = fieldShl(a_1, 1n);
    if (!__equals(high_bit, ctx.defaultT()))     {
      a_1 = fieldBitxor(a_1, __clone(c));
    }
    b_1 = fieldShr(b_1, 1n);
  }
  return p;
})();
  } else
  if (__args.length === 3) {
    const ctx = __args[0];
    const a = __args[1];
    const b = __args[2];
    return (() => {
  return fieldMul(a, b);
})();
  }
  throw new Error("field_mul(): no matching variant for " + __args.length + " args");
}

export function field_mul_block(a: any, b: any): Block
{
  return block_from_field(field_mul(field_from_block(a), field_from_block(b)));
}

export function field_square<T>(a: any, c: any): T
{
  return field_mul(__clone(a), a, c);
}

export function fill_test_poly(big_n: bigint, logical: boolean[], addr_bits: bigint, k_max: bigint, log_q: bigint, log_q_lwe: bigint): bigint[]
{
  const table_len = fieldShl(1n, addr_bits);
  const is_constant = table_is_constant(logical);
  const delta_shift = fieldSub(fieldSub(log_q_lwe, 1n), Number(k_max));
  const value = (() => { if ((log_q >= 32n)) {
  return delta_out_full_width(delta_shift, fieldSub(log_q, log_q_lwe));
} else {
  return fieldBitand(fieldShl(1n, fieldAdd(delta_shift, fieldSub(log_q, log_q_lwe))), fieldSub(fieldShl(1n, log_q), 1n));
} })();
  const width = fieldShr(big_n, k_max);
  const used = (() => { if (is_constant) {
  return 0n;
} else {
  return fieldMul(table_len, width);
} })();
  let test_poly = Array.from({length: Number(big_n)}, () => 0n);
  let p = 0n;
  while ((p < used))   {
    test_poly[Number(p)] = (() => { if (logical[Number((p / width))]) {
  return value;
} else {
  return 0n;
} })();
    p = fieldAdd(p, 1n);
  }
  return test_poly;
}

export function find_negacyclic_root(degree: bigint, modulus: bigint): (bigint | undefined)
{
  const exponent = (fieldSub(modulus, 1n) / fieldMul(2n, BigInt(degree)));
  for (let candidate = 2n; candidate < modulus; candidate += 1n)   {
    const root = pow_mod(candidate, Number(exponent), modulus);
    if (__equals(pow_mod(root, degree, modulus), fieldSub(modulus, 1n)))     {
      return root;
    }
  }
  return undefined;
}

export function fold_blinder<S>(rho1: any, rho2: any, r: any): S
{
  return fieldAdd(__clone(rho1), fieldMul(__clone(r), __clone(rho2)));
}

export function fold_commit_e(comm_e1: any, comm_t: any, comm_e2: any, r: bigint[], r2: bigint[]): EdPoint
{
  return ed_add(ed_add(comm_e1, ed_scalar_mul(comm_t, r)), ed_scalar_mul(comm_e2, r2));
}

export function fold_commit_w(comm_w1: any, comm_w2: any, r: bigint[]): EdPoint
{
  return ed_add(comm_w1, ed_scalar_mul(comm_w2, r));
}

export function fold_error_blinder<S>(re1: any, rt: any, re2: any, r: any): S
{
  const r2 = fieldMul(__clone(r), __clone(r));
  return fieldAdd(fieldAdd(__clone(re1), fieldMul(__clone(r), __clone(rt))), fieldMul(r2, __clone(re2)));
}

export function fold_u<S>(u1: any, u2: any, r: any): S
{
  return fieldAdd(__clone(u1), fieldMul(__clone(r), __clone(u2)));
}

export function fold_witness<S>(ctx: { defaultS: () => any }, w1: S[], e1: S[], w2: S[], e2: S[], t: S[], r: any): [S[], S[]]
{
  const r2 = fieldMul(__clone(r), __clone(r));
  let w = [undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */];
  for (let i = 0n; i < AND_VARS; i += 1n)   {
    w[Number(i)] = fieldAdd(__clone(w1[Number(i)]), fieldMul(__clone(r), __clone(w2[Number(i)])));
  }
  let e = [undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */];
  for (let i = 0n; i < AND_CONS; i += 1n)   {
    e[Number(i)] = fieldAdd(fieldAdd(__clone(e1[Number(i)]), fieldMul(__clone(r), __clone(t[Number(i)]))), fieldMul(__clone(r2), __clone(e2[Number(i)])));
  }
  return [w, e];
}

export function from_bool(b: boolean, q: bigint): bigint
{
  return (() => { if (b) {
  return true_val(q);
} else {
  return FALSE;
} })();
}

export function full_z<S>(w: S[], u: any): S[]
{
  return [__clone(w[Number(0n)]), __clone(w[Number(1n)]), __clone(w[Number(2n)]), __clone(w[Number(3n)]), __clone(w[Number(4n)]), __clone(w[Number(5n)]), __clone(w[Number(6n)]), __clone(u)];
}

export function g_double(seed: any): [Block, Block]
{
  const [left, right] = AesCtrLengthDoubler.double(seed);
  return [left[0], right[0]];
}

export function gadget_decompose(log: bigint, ell: bigint, base_log: bigint, x: bigint): bigint[]
{
  let digits = Array.from({length: Number(ell)}, () => 0n);
  for (const [j, d] of digits.enumerate())   {
    const shift = level_shift(log, base_log, j);
    const bits = level_bits(log, base_log, j);
    const m = (() => { if ((bits >= 32n)) {
  return u32.MAX;
} else {
  return fieldSub(fieldShl(1n, bits), 1n);
} })();
    d = fieldBitand(fieldShr(x, shift), m);
  }
  return digits;
}

export function gadget_poly_decompose(n: bigint, log: bigint, ell: bigint, base_log: bigint, p: bigint[]): bigint[][]
{
  let out = Array.from({length: Number(ell)}, () => Array.from({length: Number(n)}, () => 0n));
  for (let i = 0n; i < n; i += 1n)   {
    const digits = gadget_decompose(log, ell, base_log, p[Number(i)]);
    for (let j = 0n; j < ell; j += 1n)     {
      out[Number(j)][Number(i)] = digits[Number(j)];
    }
  }
  return out;
}

export function gate_witness<S>(k_a: any, k_b: any, k_c: any, delta: any, v_hat: any): S[]
{
  const p1 = fieldMul(__clone(k_a), __clone(k_b));
  const p2 = fieldMul(__clone(k_c), __clone(delta));
  return [k_a, k_b, k_c, delta, v_hat, p1, p2];
}

export function gen_abo<B, D>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, k: bigint, n: bigint, a: bigint[], rand: bigint[]): ABODyn<B, D>
{
  let h = ctx.newD();
  const per_byte = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_ni: any) => (() => {
  let per_byte = Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  for (let i = 0n; i < k; i += 1n)   {
    const core = Array.from({length: Number(ilog2(k) - 0n)}, (_, __i) => BigInt(__i) + 0n).reduce((acc: any, b: any) => (() => {
  if (!__equals(fieldBitand(fieldShr(i, b), 1n), 0n))   {
    const doubled = ctx.BClass.double(acc);
    acc = __clone(doubled[Number(1n)]);
  } else   {
    const doubled = ctx.BClass.double(acc);
    acc = __clone(doubled[Number(0n)]);
  }
  return acc;
})(), __clone(a));
    h.update(commit(ctx, core, rand));
    per_byte[Number(i)] = core;
  }
  return per_byte;
})());
  return new ABODyn({ $fcommit: [...h.finalize()], $fper_byte: per_byte, $fk: 0n, $fn: 0n });
}

export function gen_bootstrapping_key<R>(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, lwe_sk: LweSecretKeyDyn, rlwe_sk: RlweSecretKeyDyn, bs_noise_bits: bigint, ks_noise_bits: bigint, rng: any): BootstrappingKeyDyn
{
  const bsk = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const bit = !__equals(lwe_sk.$fkey[Number(i)], 0n);
  return rgsw_encrypt(big_n, bs_ell, bs_bg_log, bit, rlwe_sk, bs_noise_bits, rng);
})());
  const ksk_array: LweCiphertextDyn[][] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s_bit = rlwe_sk.$fkey[Number(i)];
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const shift = (32n - (Number(fieldMul(ks_bg_log, fieldAdd(j, 1n)))));
  const msg_val = (((s_bit) << (shift)) & 0xFFFFFFFFn);
  return lwe_encrypt_raw(n_lwe, msg_val, lwe_sk, ks_noise_bits, rng);
})());
})());
  const ksk = new KeySwitchingKeyDyn({ $fksk: ksk_array, $fn_lwe: 0n, $fbig_n: 0n, $fks_ell: 0n, $fks_bg_log: 0n });
  return new BootstrappingKeyDyn({ $fbsk: bsk, $fksk: ksk, $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n, $fbs_bg_log: 0n, $fks_bg_log: 0n });
}

export function gen_circuit_bootstrapping_key<R>(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, priv_ell: bigint, priv_base_log: bigint, eta: bigint, lwe_sk: BinfheLweSecretKeyDyn, rlwe_sk: BinfheRlweSecretKeyDyn, rng: any): CircuitBootstrappingKeyDyn
{
  const bk = binfhe_gen_bootstrapping_key(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, eta, lwe_sk, rlwe_sk, rng);
  const zero = Array.from({length: Number(big_n)}, () => 0n);
  const neg_one_const: bigint[] = (() => {
  let p = Array.from({length: Number(big_n)}, () => 0n);
  p[Number(0n)] = torus_neg(1n);
  return p;
})();
  const neg_sk: bigint[] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => torus_neg(rlwe_sk.$fkey[Number(i)]));
  const one_const: bigint[] = (() => {
  let p = Array.from({length: Number(big_n)}, () => 0n);
  p[Number(0n)] = 1n;
  return p;
})();
  let a_col = ([] as any[]);
  for (let i = 0n; i < big_n; i += 1n)   {
    const msg = (() => { if (__equals(rlwe_sk.$fkey[Number(i)], 1n)) {
  return rlwe_sk.$fkey;
} else {
  return zero;
} })();
    (a_col).push(Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, msg, l, priv_base_log, rlwe_sk, rng);
})()));
  }
  let b_col = ([] as any[]);
  for (let i = 0n; i < big_n; i += 1n)   {
    const msg = (() => { if (__equals(rlwe_sk.$fkey[Number(i)], 1n)) {
  return neg_one_const;
} else {
  return zero;
} })();
    (b_col).push(Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, msg, l, priv_base_log, rlwe_sk, rng);
})()));
  }
  const a_body = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, neg_sk, l, priv_base_log, rlwe_sk, rng);
})());
  const b_body = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, one_const, l, priv_base_log, rlwe_sk, rng);
})());
  return new CircuitBootstrappingKeyDyn({ $fbk: bk, $fprivksk: new PrivateKeySwitchingKeyDyn({ $fa_col: a_col, $fb_col: b_col, $fa_body: a_body, $fb_body: b_body, $fbig_n: 0n, $fpriv_ell: 0n }), $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n, $fpriv_ell: 0n });
}

export function gen_lwe_secret_key<R>(n_lwe: bigint, rng: any): LweSecretKeyDyn
{
  let key = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(key.length); __mut_1 += 1n) {
  {
    key[Number(__mut_1)] = ((fieldBitand(rng.next_u8(), 1n)) & 0xFFn);
  }}
  return new LweSecretKeyDyn({ $fkey: key, $fn_lwe: 0n });
}

export function gen_rlwe_secret_key<R>(big_n: bigint, rng: any): RlweSecretKeyDyn
{
  let key = Array.from({length: Number(big_n)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(key.length); __mut_1 += 1n) {
  {
    key[Number(__mut_1)] = Number(fieldBitand(rng.next_u8(), 1n));
  }}
  return new RlweSecretKeyDyn({ $fkey: key, $fbig_n: 0n });
}

export function gf_invert_256(a: any, c: any): U256
{
  if (a.is_zero())   {
    return U256.ZERO;
  }
  const e: bigint = 255n;
  const msb: bigint = 7n;
  let r = a;
  let k: bigint = 1n;
  for (let bit_pos_rev = 0n; bit_pos_rev < msb; bit_pos_rev += 1n)   {
    const bit_pos = fieldSub(fieldSub(msb, 1n), bit_pos_rev);
    let tmp = r;
    for (let _ = 0n; _ < k; _ += 1n)     {
      tmp = gf_mul_256(tmp, tmp, c);
    }
    r = gf_mul_256(tmp, r, c);
    k = fieldMul(k, 2n);
    if (__equals(fieldBitand(fieldShr(e, bit_pos), 1n), 1n))     {
      r = gf_mul_256(gf_mul_256(r, r, c), a, c);
      k = fieldAdd(k, 1n);
    }
  }
  return gf_mul_256(r, r, c);
}

export function gf_invert_u128(a: bigint, c: bigint): bigint
{
  if (__equals(a, 0n))   {
    return 0n;
  }
  const e: bigint = 127n;
  const msb: bigint = 6n;
  let r = a;
  let k: bigint = 1n;
  for (let bit_pos_rev = 0n; bit_pos_rev < msb; bit_pos_rev += 1n)   {
    const bit_pos = fieldSub(fieldSub(msb, 1n), bit_pos_rev);
    let tmp = r;
    for (let _ = 0n; _ < k; _ += 1n)     {
      tmp = gf_mul_u128(tmp, tmp, c);
    }
    r = gf_mul_u128(tmp, r, c);
    k = fieldMul(k, 2n);
    if (__equals(fieldBitand(fieldShr(e, bit_pos), 1n), 1n))     {
      r = gf_mul_u128(gf_mul_u128(r, r, c), a, c);
      k = fieldAdd(k, 1n);
    }
  }
  return gf_mul_u128(r, r, c);
}

export function gf_invert_u64(a: bigint, c: bigint): bigint
{
  if (__equals(a, 0n))   {
    return 0n;
  }
  const e: bigint = 63n;
  const msb: bigint = 5n;
  let r = a;
  let k: bigint = 1n;
  for (let bit_pos_rev = 0n; bit_pos_rev < msb; bit_pos_rev += 1n)   {
    const bit_pos = fieldSub(fieldSub(msb, 1n), bit_pos_rev);
    let tmp = r;
    for (let _ = 0n; _ < k; _ += 1n)     {
      tmp = gf_mul_u64(tmp, tmp, c);
    }
    r = gf_mul_u64(tmp, r, c);
    k = fieldMul(k, 2n);
    if (__equals(fieldBitand(fieldShr(e, bit_pos), 1n), 1n))     {
      r = gf_mul_u64(gf_mul_u64(r, r, c), a, c);
      k = fieldAdd(k, 1n);
    }
  }
  return gf_mul_u64(r, r, c);
}

export function gf_invert_u8(a: bigint, c: bigint): bigint
{
  if (__equals(a, 0n))   {
    return 0n;
  }
  const e: bigint = 7n;
  const msb: bigint = 2n;
  let r = a;
  let k: bigint = 1n;
  for (let bit_pos_rev = 0n; bit_pos_rev < msb; bit_pos_rev += 1n)   {
    const bit_pos = fieldSub(fieldSub(msb, 1n), bit_pos_rev);
    let tmp = r;
    for (let _ = 0n; _ < k; _ += 1n)     {
      tmp = gf_mul_u8(tmp, tmp, c);
    }
    r = gf_mul_u8(tmp, r, c);
    k = fieldMul(k, 2n);
    if (__equals(fieldBitand(fieldShr(e, bit_pos), 1n), 1n))     {
      r = gf_mul_u8(gf_mul_u8(r, r, c), a, c);
      k = fieldAdd(k, 1n);
    }
  }
  return gf_mul_u8(r, r, c);
}

export function gf_mul(a: bigint, b: bigint): bigint
{
  return gf_mul_u8(a, b, GF8_AES_POLY);
}

export function gf_mul_256(a: any, b: any, c: any): U256
{
  let p = U256.ZERO;
  let a_1 = a;
  let b_1 = b;
  for (let _ = 0n; _ < 256n; _ += 1n)   {
    if (b_1.bit(0n))     {
      p = p.xor(a_1);
    }
    const high = a_1.high_bit();
    a_1 = a_1.shl1();
    if (high)     {
      a_1 = a_1.xor(c);
    }
    b_1 = b_1.shr1();
  }
  return p;
}

export function gf_mul_u128(a: bigint, b: bigint, c: bigint): bigint
{
  let p: bigint = 0n;
  let a_1 = a;
  let b_1 = b;
  const h: bigint = fieldShl(1n, 127n);
  for (let _ = 0n; _ < 128n; _ += 1n)   {
    if (!__equals(fieldBitand(b_1, 1n), 0n))     {
      p = fieldBitxor(p, a_1);
    }
    const high = fieldBitand(a_1, h);
    a_1 = fieldShl(a_1, 1n);
    if (!__equals(high, 0n))     {
      a_1 = fieldBitxor(a_1, c);
    }
    b_1 = fieldShr(b_1, 1n);
  }
  return p;
}

export function gf_mul_u64(a: bigint, b: bigint, c: bigint): bigint
{
  let p: bigint = 0n;
  let a_1 = a;
  let b_1 = b;
  const h: bigint = fieldShl(1n, 63n);
  for (let _ = 0n; _ < 64n; _ += 1n)   {
    if (!__equals(fieldBitand(b_1, 1n), 0n))     {
      p = fieldBitxor(p, a_1);
    }
    const high = fieldBitand(a_1, h);
    a_1 = fieldShl(a_1, 1n);
    if (!__equals(high, 0n))     {
      a_1 = fieldBitxor(a_1, c);
    }
    b_1 = fieldShr(b_1, 1n);
  }
  return p;
}

export function gf_mul_u8(a: bigint, b: bigint, c: bigint): bigint
{
  let p: bigint = 0n;
  let a_1 = a;
  let b_1 = b;
  for (let _ = 0n; _ < 8n; _ += 1n)   {
    if (!__equals(fieldBitand(b_1, 1n), 0n))     {
      p = fieldBitxor(p, a_1);
    }
    const high = fieldBitand(a_1, 128n);
    a_1 = fieldShl(a_1, 1n);
    if (!__equals(high, 0n))     {
      a_1 = fieldBitxor(a_1, c);
    }
    b_1 = fieldShr(b_1, 1n);
  }
  return p;
}

export function grind_chall3(chall_2: bigint[], a_hat: bigint[], b_hat: bigint[], c_hat_base: bigint[], lambda: bigint, w_grind: bigint, use_shake256: boolean, max_iters: bigint): ([Vec<bigint>, bigint] | undefined)
{
  for (let counter = 0n; counter < max_iters; counter += 1n)   {
    const counter_bytes = [(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn];
    let c_hat_grind = Vec.from(c_hat_base);
    c_hat_grind.push(...(counter_bytes));
    const candidate = chall3(chall_2, a_hat, b_hat, c_hat_grind, lambda, use_shake256);
    if (has_trailing_zero_bits(candidate, w_grind))     {
      return [candidate, counter];
    }
  }
  return undefined;
}

export function has_trailing_zero_bits(...__args: any[]): any {
  if (__args.length === 2) {
    const bytes = __args[0];
    const n = __args[1];
    return (() => {
  if (__equals(n, 0n))   {
    return true;
  }
  const n_1 = Number(n);
  const full_bytes = (n_1 / 8n);
  const rem_bits = (n_1 % 8n);
  if ((BigInt(bytes.length) < fieldAdd(full_bytes, (() => { if ((rem_bits > 0n)) {
  return 1n;
} else {
  return 0n;
} })())))   {
    return false;
  }
  for (let i = fieldSub(BigInt(bytes.length), full_bytes); i < BigInt(bytes.length); i += 1n)   {
    if (!__equals(bytes[Number(i)], 0n))     {
      return false;
    }
  }
  if ((rem_bits > 0n))   {
    const mask = fieldSub(fieldShl(1n, rem_bits), 1n);
    const byte_idx = fieldSub(fieldSub(BigInt(bytes.length), full_bytes), 1n);
    if (!__equals(fieldBitand(bytes[Number(byte_idx)], mask), 0n))     {
      return false;
    }
  }
  return true;
})();
  } else
  if (__args.length === 2) {
    const bytes = __args[0];
    const n = __args[1];
    return (() => {
  if (__equals(n, 0n))   {
    return true;
  }
  const n_1 = Number(n);
  const full_bytes = (n_1 / 8n);
  const rem = (n_1 % 8n);
  if ((BigInt(bytes.length) < fieldAdd(full_bytes, (() => { if ((rem > 0n)) {
  return 1n;
} else {
  return 0n;
} })())))   {
    return false;
  }
  for (let i = fieldSub(BigInt(bytes.length), full_bytes); i < BigInt(bytes.length); i += 1n)   {
    if (!__equals(bytes[Number(i)], 0n))     {
      return false;
    }
  }
  if ((rem > 0n))   {
    const mask = fieldSub(fieldShl(1n, rem), 1n);
    const idx = fieldSub(fieldSub(BigInt(bytes.length), full_bytes), 1n);
    if (!__equals(fieldBitand(bytes[Number(idx)], mask), 0n))     {
      return false;
    }
  }
  return true;
})();
  }
  throw new Error("has_trailing_zero_bits(): no matching variant for " + __args.length + " args");
}

export function hash_i(seed: bigint[], i: bigint, x: bigint, m: bigint): bigint
{
  let h = new Sha3_256();
  h.update(new Uint8Array([/* byte string */]));
  h.update(seed);
  h.update([(BigInt(i)) & 0xFFn, ((BigInt(i)) >> 8n) & 0xFFn, ((BigInt(i)) >> 16n) & 0xFFn, ((BigInt(i)) >> 24n) & 0xFFn]);
  h.update([(BigInt(x)) & 0xFFn, ((BigInt(x)) >> 8n) & 0xFFn, ((BigInt(x)) >> 16n) & 0xFFn, ((BigInt(x)) >> 24n) & 0xFFn]);
  const out = [...h.finalize()];
  const raw = u64_from_le_bytes((out.slice(0, Number(8n)))!);
  return (Number(raw) % m);
}

export function hash_key_from_chall(chall: bigint[]): UniversalHashKey
{
  let r0_bytes = Array.from({length: Number(16n)}, () => 0n);
  const n = BigInt(Math.min(Number(BigInt(chall.length)), Number(16n)));
  (r0_bytes.slice(0, Number(n))).splice(0, (chall.slice(0, Number(n))).length, ...(chall.slice(0, Number(n))));
  let r1_bytes = Array.from({length: Number(8n)}, () => 0n);
  const off = n;
  const m = BigInt(Math.min(Number(fieldSub(BigInt(chall.length), off)), Number(8n)));
  (r1_bytes.slice(0, Number(m))).splice(0, (chall.slice(Number(off), Number(fieldAdd(off, m)))).length, ...(chall.slice(Number(off), Number(fieldAdd(off, m)))));
  return new UniversalHashKey({ $fr0: new Galois128(u128_from_le_bytes(r0_bytes)), $fr1: new Galois64(u64_from_le_bytes(r1_bytes)) });
}

export function hash_prime(v: any): bigint[]
{
  let h = new Sha3_256();
  h.update(new Uint8Array([/* byte string */]));
  h.update(v);
  const out = [...h.finalize()];
  let b = Array.from({length: Number(32n)}, () => 0n);
  (b).splice(0, (out).length, ...(out));
  return b;
}

export function hash_to_curve(ctx: { DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, domain: bigint[], index: bigint): EdPoint
{
  for (let ctr = 0n; ctr < 0n; ctr += 1n)   {
    let h = new Sha3_256();
    h.update(domain);
    h.update([(index) & 0xFFn, ((index) >> 8n) & 0xFFn, ((index) >> 16n) & 0xFFn, ((index) >> 24n) & 0xFFn]);
    h.update([(ctr) & 0xFFn, ((ctr) >> 8n) & 0xFFn, ((ctr) >> 16n) & 0xFFn, ((ctr) >> 24n) & 0xFFn]);
    const out = [...h.finalize()];
    let xb = Array.from({length: Number(32n)}, () => 0n);
    (xb).splice(0, (out).length, ...(out));
    const sign = fieldBitand(fieldShr(xb[Number(31n)], 7n), 1n);
    xb[Number(31n)] = fieldBitand(xb[Number(31n)], 127n);
    const x = fe_from_bytes_le(xb);
    const xx = fe_sq(x);
    const num = fe_add(Fe25519.ONE, xx);
    const den = fe_sub(Fe25519.ONE, fe_mul(ctx.DClass, xx));
    if (den.is_zero())     {
      continue;
    }
    const yy = fe_mul(num, fe_invert(den));
    const y = fe_sqrt(yy);
    if (!__equals(fieldBitand(y.to_bytes()[Number(0n)], 1n), sign))     {
      y = fe_neg(y);
    }
    const point = new EdPoint({ $fx: x, $fy: y, $fz: Fe25519.ONE, $ft: fe_mul(x, y) });
    const p8 = ed_mul_cofactor(point);
    if (__equals(p8, EdPoint.IDENTITY))     {
      continue;
    }
    return p8;
  }
  return (() => { throw new Error("unreachable"); })();
}

export function iknp_cot_extend<R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): [bigint[][], bigint[][]]
{
  const [r0, v] = iknp_cot_extend_base(ctx, l, rng_s, rng_r, receiver_bits, delta_msg);
  let sender_r0 = Array.from({length: Number(m)}, () => Array.from({length: Number(l)}, () => 0n));
  let receiver_v = Array.from({length: Number(m)}, () => Array.from({length: Number(l)}, () => 0n));
  for (let j = 0n; j < m; j += 1n)   {
    sender_r0[Number(j)] = r0[Number(j)];
    receiver_v[Number(j)] = v[Number(j)];
  }
  return [sender_r0, receiver_v];
}

export function iknp_cot_extend_base<R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): [Vec<bigint[]>, Vec<bigint[]>]
{
  const m = BigInt(receiver_bits.length);
  let delta_ot = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    delta_ot[Number(i)] = __equals(fieldBitand(rng_s.next_u32(), 1n), 1n);
  }
  const delta_ot_bytes = pack_kappa(delta_ot);
  let seeds_0 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n));
  let seeds_1 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    for (let b = 0n; b < IKNP_KAPPA_BYTES; b += 1n)     {
      seeds_0[Number(i)][Number(b)] = ((fieldBitand(rng_r.next_u32(), 255n)) & 0xFFn);
      seeds_1[Number(i)][Number(b)] = ((fieldBitand(rng_r.next_u32(), 255n)) & 0xFFn);
    }
  }
  let chosen_seeds = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    const [s_state, setup] = ctx.BClass.sender_setup(rng_r);
    const [r_state, recv_msg] = ctx.BClass.recv_start(rng_s, setup, delta_ot[Number(i)]);
    const payload = ctx.BClass.sender_payload(rng_r, s_state, recv_msg, seeds_0[Number(i)], seeds_1[Number(i)]);
    chosen_seeds[Number(i)] = ctx.BClass.recv_finish(r_state, payload);
  }
  const [t_cols, u_msg] = iknp_receiver_u_cols(ctx, m, receiver_bits, seeds_0, seeds_1);
  const [sender_r0, corrections] = iknp_sender_from_u(ctx, l, m, delta_msg, delta_ot, delta_ot_bytes, chosen_seeds, u_msg);
  const receiver_v = iknp_receiver_finish(ctx, l, receiver_bits, t_cols, corrections);
  return [sender_r0, receiver_v];
}

export function iknp_receiver_finish(ctx: { newD: () => any }, l: bigint, receiver_bits: boolean[], t_cols: Vec<boolean>[], corrections: bigint[][]): Vec<bigint[]>
{
  const m = BigInt(receiver_bits.length);
  let receiver_v = ([] as any[]);
  let t_row = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  for (let j = 0n; j < m; j += 1n)   {
    for (let i = 0n; i < IKNP_KAPPA; i += 1n)     {
      t_row[Number(i)] = t_cols[Number(i)][Number(j)];
    }
    const t_bytes = pack_kappa(t_row);
    let v_pre = Array.from({length: Number(l)}, () => 0n);
    prg_with_index(ctx, t_bytes, Number(j), v_pre);
    let vj = Array.from({length: Number(l)}, () => 0n);
    if (receiver_bits[Number(j)])     {
      for (let b = 0n; b < l; b += 1n)       {
        vj[Number(b)] = fieldBitxor(v_pre[Number(b)], corrections[Number(j)][Number(b)]);
      }
    } else     {
      vj = v_pre;
    }
    (receiver_v).push(vj);
  }
  return receiver_v;
}

export function iknp_receiver_u_cols(ctx: { newD: () => any }, m: bigint, receiver_bits: boolean[], seeds_0: bigint[][], seeds_1: bigint[][]): [Vec<Vec<boolean>>, IknpUMsg]
{
  let t_cols = ([] as any[]);
  let u_cols = ([] as any[]);
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    let t_col = [] as any[];
    prg_to_bools(ctx, seeds_0[Number(i)], t_col);
    let prg1 = [] as any[];
    prg_to_bools(ctx, seeds_1[Number(i)], prg1);
    let u_col = [] as any[];
    for (let j = 0n; j < m; j += 1n)     {
      u_col[Number(j)] = fieldBitxor(fieldBitxor(t_col[Number(j)], prg1[Number(j)]), receiver_bits[Number(j)]);
    }
    (t_cols).push(t_col);
    (u_cols).push(u_col);
  }
  return [t_cols, new IknpUMsg({ $fu_cols: u_cols })];
}

export function iknp_sender_from_u(ctx: { newD: () => any }, l: bigint, m: bigint, delta_msg: bigint[], delta_ot: boolean[], delta_ot_bytes: bigint[], chosen_seeds: bigint[][], u_msg: any): [Vec<bigint[]>, Vec<bigint[]>]
{
  let q_cols = ([] as any[]);
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    let prg_chosen = [] as any[];
    prg_to_bools(ctx, chosen_seeds[Number(i)], prg_chosen);
    let q_col = [] as any[];
    for (let j = 0n; j < m; j += 1n)     {
      if (delta_ot[Number(i)])       {
        q_col[Number(j)] = fieldBitxor(prg_chosen[Number(j)], u_msg.$fu_cols[Number(i)][Number(j)]);
      } else       {
        q_col[Number(j)] = prg_chosen[Number(j)];
      }
    }
    (q_cols).push(q_col);
  }
  let sender_r0 = ([] as any[]);
  let corrections = ([] as any[]);
  let q_row = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  for (let j = 0n; j < m; j += 1n)   {
    for (let i = 0n; i < IKNP_KAPPA; i += 1n)     {
      q_row[Number(i)] = q_cols[Number(i)][Number(j)];
    }
    const q_bytes = pack_kappa(q_row);
    let r0 = Array.from({length: Number(l)}, () => 0n);
    prg_with_index(ctx, q_bytes, Number(j), r0);
    let q_xor_delta = q_bytes;
    for (let b = 0n; b < IKNP_KAPPA_BYTES; b += 1n)     {
      q_xor_delta[Number(b)] = fieldBitxor(q_xor_delta[Number(b)], delta_ot_bytes[Number(b)]);
    }
    let r1 = Array.from({length: Number(l)}, () => 0n);
    prg_with_index(ctx, q_xor_delta, Number(j), r1);
    let correction = Array.from({length: Number(l)}, () => 0n);
    for (let b = 0n; b < l; b += 1n)     {
      correction[Number(b)] = fieldBitxor(fieldBitxor(r0[Number(b)], r1[Number(b)]), delta_msg[Number(b)]);
    }
    (sender_r0).push(r0);
    (corrections).push(correction);
  }
  return [sender_r0, corrections];
}

export function in_interval_mod(x: bigint, lo: bigint, hi: bigint, q: bigint): boolean
{
  const x_1 = (x % q);
  const lo_1 = (lo % q);
  const hi_1 = (hi % q);
  return (() => { if ((lo_1 <= hi_1)) {
  return ((x_1 >= lo_1) && (x_1 < hi_1));
} else {
  return ((x_1 >= lo_1) || (x_1 < hi_1));
} })();
}

export function inner_product(ring: any, left: Polynomial[], right: Polynomial[]): Result<Polynomial, Error>
{
  if ((!__equals(BigInt(left.length), BigInt(right.length)) || left.concat(right).any((value) => !__equals(value.degree(), ring.$fparameters.$fdegree))))   {
    return new Error_LengthMismatch();
  }
  let output = ring.zero();
  for (const [left, right] of left.map((__a: any, __i: number) => [__a, right[__i]] as [typeof __a, any]))   {
    output.add_assign(ring, left.product(ring, right));
  }
  return output;
}

export function inverse_mod(value: bigint, modulus: bigint): (bigint | undefined)
{
  if ((__equals(value, 0n) || (modulus < 2n)))   {
    return undefined;
  }
  let old_r = (value as unknown as bigint);
  let r = (modulus as unknown as bigint);
  let old_s = 1n;
  let s = 0n;
  while (!__equals(r, 0n))   {
    const quotient = (old_r / r);
    [old_r, r] = [r, fieldSub(old_r, fieldMul(quotient, r))];
    [old_s, s] = [s, fieldSub(old_s, fieldMul(quotient, s))];
  }
  if (!__equals(old_r, 1n))   {
    return undefined;
  }
  return BigInt(old_s.rem_euclid((modulus as unknown as bigint)));
}

export function is_satisfied_relaxed<S>(w: S[], e: S[], u: any): boolean
{
  const z = full_z(w, u);
  const [az, bz, cz] = eval_abc(z);
  let ok = true;
  for (let i = 0n; i < AND_CONS; i += 1n)   {
    const lhs = fieldMul(__clone(az[Number(i)]), __clone(bz[Number(i)]));
    const rhs = fieldAdd(fieldMul(__clone(u), __clone(cz[Number(i)])), __clone(e[Number(i)]));
    ok = (ok && __equals(lhs, rhs));
  }
  return ok;
}

export function is_square(w: any): boolean
{
  return (fe_sqrt(w)) != null;
}

export function key_expansion(key: bigint[]): bigint[][]
{
  let words = Array.from({length: Number(fieldMul(4n, NK_ROUND_KEYS))}, () => Array.from({length: Number(4n)}, () => 0n));
  for (let i = 0n; i < 4n; i += 1n)   {
    words[Number(i)] = [key[Number(fieldMul(4n, i))], key[Number(fieldAdd(fieldMul(4n, i), 1n))], key[Number(fieldAdd(fieldMul(4n, i), 2n))], key[Number(fieldAdd(fieldMul(4n, i), 3n))]];
  }
  for (let i = 4n; i < fieldMul(4n, NK_ROUND_KEYS); i += 1n)   {
    let temp = words[Number(fieldSub(i, 1n))];
    if (__equals((i % 4n), 0n))     {
      const t0 = temp[Number(0n)];
      temp[Number(0n)] = temp[Number(1n)];
      temp[Number(1n)] = temp[Number(2n)];
      temp[Number(2n)] = temp[Number(3n)];
      temp[Number(3n)] = t0;
      for (let b = 0n; b < 4n; b += 1n)       {
        temp[Number(b)] = SBOX[Number(Number(temp[Number(b)]))];
      }
      temp[Number(0n)] = fieldBitxor(temp[Number(0n)], RCON[Number((i / 4n))]);
    }
    for (let b = 0n; b < 4n; b += 1n)     {
      words[Number(i)][Number(b)] = fieldBitxor(words[Number(fieldSub(i, 4n))][Number(b)], temp[Number(b)]);
    }
  }
  let round_keys = Array.from({length: Number(NK_ROUND_KEYS)}, () => Array.from({length: Number(BLOCK)}, () => 0n));
  for (let r = 0n; r < NK_ROUND_KEYS; r += 1n)   {
    for (let c = 0n; c < 4n; c += 1n)     {
      const w = words[Number(fieldAdd(fieldMul(4n, r), c))];
      round_keys[Number(r)][Number(fieldMul(4n, c))] = w[Number(0n)];
      round_keys[Number(r)][Number(fieldAdd(fieldMul(4n, c), 1n))] = w[Number(1n)];
      round_keys[Number(r)][Number(fieldAdd(fieldMul(4n, c), 2n))] = w[Number(2n)];
      round_keys[Number(r)][Number(fieldAdd(fieldMul(4n, c), 3n))] = w[Number(3n)];
    }
  }
  return round_keys;
}

export function key_switch(n_lwe: bigint, big_n: bigint, ks_ell: bigint, ks_bg_log: bigint, ct_big: LweCiphertextDyn, ksk: KeySwitchingKeyDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  let out_b = ct_big.$fb;
  for (let i = 0n; i < big_n; i += 1n)   {
    const digits = ks_decompose(ks_ell, ks_bg_log, ct_big.$fa[Number(i)]);
    for (let j = 0n; j < ks_ell; j += 1n)     {
      const d = Number(digits[Number(j)]);
      if (__equals(d, 0n))       {
        continue;
      }
      const ksk_ct = ksk.$fksk[Number(i)][Number(j)];
      for (let k = 0n; k < n_lwe; k += 1n)       {
        out_a[Number(k)] = wrappingSub(out_a[Number(k)], BigInt(Math.imul(Number(d), Number(ksk_ct.$fa[Number(k)]))), 32);
      }
      out_b = wrappingSub(out_b, BigInt(Math.imul(Number(d), Number(ksk_ct.$fb))), 32);
    }
  }
  return new LweCiphertextDyn({ $fa: out_a, $fb: out_b, $fn_lwe: 0n });
}

export function keygen(rng: any): [FaestSecretKey, FaestPublicKey]
{
  let sk = Array.from({length: Number(LAMBDA_BYTES)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(sk.length); __mut_1 += 1n) {
  {
    sk[Number(__mut_1)] = rng.next_u8();
  }}
  const pk = aes128_encrypt(sk, Array.from({length: Number(LAMBDA_BYTES)}, () => 0n));
  return [new FaestSecretKey(sk), new FaestPublicKey(pk)];
}

export function ks_decompose(ks_ell: bigint, ks_bg_log: bigint, x: bigint): bigint[]
{
  const bg = fieldShl(1n, ks_bg_log);
  const mask = fieldSub(bg, 1n);
  let rem = BigInt(x);
  const tail_shift = (32n - (Number(fieldMul(ks_bg_log, ks_ell))));
  if (((tail_shift > 0n) && (tail_shift < 32n)))   {
    const half_tail = fieldShl(1n, fieldSub(tail_shift, 1n));
    rem = wrappingAdd(rem, half_tail, 32);
  }
  let digits = Array.from({length: Number(ks_ell)}, () => 0n);
  for (const j of (Array.from({length: Number(ks_ell - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    const shift = (32n - (Number(fieldMul(ks_bg_log, fieldAdd(j, 1n)))));
    if ((shift < 32n))     {
      digits[Number(j)] = Number(fieldBitand(fieldShr(rem, shift), mask));
    }
  }
  return digits;
}

export function level_bits(log: bigint, base_log: bigint, j: bigint): bigint
{
  const remaining = (log - (fieldMul(base_log, Number(j))));
  return (() => { if ((remaining < base_log)) {
  return remaining;
} else {
  return base_log;
} })();
}

export function level_factor(log: bigint, base_log: bigint, j: bigint): bigint
{
  return reduce(fieldShl(1n, level_shift(log, base_log, j)));
}

export function level_shift(log: bigint, base_log: bigint, j: bigint): bigint
{
  return (log - (fieldMul(base_log, fieldAdd(Number(j), 1n))));
}

export function level_test_poly(big_n: bigint, log_q: bigint, level: bigint, bs_base_log: bigint, k_max: bigint): bigint[]
{
  const width = fieldShr(big_n, k_max);
  const g = level_factor(bs_base_log, level);
  let poly = Array.from({length: Number(big_n)}, () => 0n);
  for (let p = width; p < fieldMul(2n, width); p += 1n)   {
    poly[Number(p)] = g;
  }
  return poly;
}

export function lift_bit<T>(n: bigint, bit_t: any): T[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(bit_t));
}

export function lwe_add(n_lwe: bigint, a: LweCiphertextDyn, b: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let i = 0n; i < n_lwe; i += 1n)   {
    out_a[Number(i)] = wrappingAdd(a.$fa[Number(i)], b.$fa[Number(i)], 32);
  }
  return new LweCiphertextDyn({ $fa: out_a, $fb: wrappingAdd(a.$fb, b.$fb, 32), $fn_lwe: 0n });
}

export function lwe_decode(log_m: bigint, phase: bigint, delta: bigint): boolean
{
  return (reduce(wrappingSub(phase, (delta / 2n), 32)) < delta);
}

export function lwe_decrypt(n_lwe: bigint, ct: LweCiphertextDyn, sk: LweSecretKeyDyn): boolean
{
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(ct.$fa[Number(i)]), Number(Number(sk.$fkey[Number(i)])))), 32);
  }
  const phase = wrappingSub(ct.$fb, dot, 32);
  const half = fieldShr(Q4, 1n);
  const shifted = wrappingSub(phase, half, 32);
  return (shifted < Q4);
}

export function lwe_encrypt<R>(n_lwe: bigint, m: boolean, sk: LweSecretKeyDyn, noise_bits: bigint, rng: any): LweCiphertextDyn
{
  let a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(a.length); __mut_1 += 1n) {
  {
    a[Number(__mut_1)] = rng.next_u32();
  }}
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(Number(sk.$fkey[Number(i)])))), 32);
  }
  const e: bigint = small_noise(noise_bits, rng);
  const msg = (() => { if (m) {
  return Q4;
} else {
  return 0n;
} })();
  const b = wrappingAdd(wrappingAdd(dot, e, 32), msg, 32);
  return new LweCiphertextDyn({ $fa: a, $fb: b, $fn_lwe: 0n });
}

export function lwe_encrypt_raw<R>(n_lwe: bigint, msg: bigint, sk: LweSecretKeyDyn, noise_bits: bigint, rng: any): LweCiphertextDyn
{
  let a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(a.length); __mut_1 += 1n) {
  {
    a[Number(__mut_1)] = rng.next_u32();
  }}
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(Number(sk.$fkey[Number(i)])))), 32);
  }
  const e = small_noise(noise_bits, rng);
  const b = wrappingAdd(wrappingAdd(dot, e, 32), msg, 32);
  return new LweCiphertextDyn({ $fa: a, $fb: b, $fn_lwe: 0n });
}

export function lwe_ot_recv<R>(n: bigint, rng: any, crs: LweOtCrsDyn, c: boolean): [LweOtReceiverDyn, LweOtRecvMsgDyn]
{
  let s = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    s[Number(i)] = sample_noise(rng);
  }
  let pk_real = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    let acc: Zq = 0n;
    for (let j = 0n; j < n; j += 1n)     {
      acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], s[Number(j)]));
    }
    acc = zq_add(acc, sample_noise(rng));
    pk_real[Number(i)] = acc;
  }
  const pk0 = (() => { if (c) {
  let pk0 = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    pk0[Number(i)] = zq_sub(crs.$fh[Number(i)], pk_real[Number(i)]);
  }
  return pk0;
} else {
  return pk_real;
} })();
  return [new LweOtReceiverDyn({ $fs: s, $fc: c, $fn: 0n }), new LweOtRecvMsgDyn({ $fpk0: pk0, $fn: 0n })];
}

export function lwe_ot_recv_decrypt(n: bigint, l: bigint, receiver: LweOtReceiverDyn, sender_msg: LweOtSenderMsgLoweredDyn): bigint[]
{
  const [u, v] = (() => { if (receiver.$fc) {
  return [sender_msg.$fu1.slice(0), sender_msg.$fv1.slice(0)];
} else {
  return [sender_msg.$fu0.slice(0), sender_msg.$fv0.slice(0)];
} })();
  return decrypt_coords(n, l, receiver, u, v);
}

export function lwe_ot_recv_decrypt_bytes(n: bigint, receiver: LweOtReceiverDyn, sender_msg: any, nbytes: bigint): Vec<bigint>
{
  const [u, v] = (() => { if (receiver.$fc) {
  return [sender_msg.$fu1.slice(0), sender_msg.$fv1.slice(0)];
} else {
  return [sender_msg.$fu0.slice(0), sender_msg.$fv0.slice(0)];
} })();
  let s_dot_u: Zq = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.$fs[Number(i)], u[Number(i)]));
  }
  const quarter = (LWE_Q / 4n);
  const three_quarter = fieldMul(3n, quarter);
  let bits = [] as any[];
  for (let k = 0n; k < BigInt(v.length); k += 1n)   {
    const raw = zq_sub(v[Number(k)], s_dot_u);
    bits[Number(k)] = (() => { if (((raw > quarter) && (raw <= three_quarter))) {
  return 1n;
} else {
  return 0n;
} })();
  }
  return bits_to_bytes(bits, nbytes);
}

export function lwe_ot_send<R>(n: bigint, l: bigint, rng: any, crs: LweOtCrsDyn, recv_msg: LweOtRecvMsgDyn, m0: bigint[], m1: bigint[]): LweOtSenderMsgLoweredDyn
{
  const pk0 = recv_msg.$fpk0;
  let pk1 = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    pk1[Number(i)] = zq_sub(crs.$fh[Number(i)], pk0[Number(i)]);
  }
  const [u0, v0] = encrypt_branch(n, l, rng, crs, pk0, m0);
  const [u1, v1] = encrypt_branch(n, l, rng, crs, pk1, m1);
  return new LweOtSenderMsgLoweredDyn({ $fu0: u0, $fv0: v0, $fu1: u1, $fv1: v1, $fn: 0n, $fl: 0n });
}

export function lwe_ot_send_bytes<R>(n: bigint, rng: any, crs: LweOtCrsDyn, recv_msg: LweOtRecvMsgDyn, m0: bigint[], m1: bigint[]): LweOtSenderMsgDyn
{
  const bits0 = bytes_to_bits(m0);
  const bits1 = bytes_to_bits(m1);
  const pk0 = recv_msg.$fpk0;
  let pk1 = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    pk1[Number(i)] = zq_sub(crs.$fh[Number(i)], pk0[Number(i)]);
  }
  const [u0, v0] = encrypt_branch_dyn(n, rng, crs, pk0, bits0);
  const [u1, v1] = encrypt_branch_dyn(n, rng, crs, pk1, bits1);
  return new LweOtSenderMsgDyn({ $fu0: u0, $fv0: v0, $fu1: u1, $fv1: v1 });
}

export function lwe_phase(n: bigint, log_m: bigint, ct: BinfheLweCiphertextDyn, sk: BinfheLweSecretKeyDyn): bigint
{
  let dot = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(ct.$fa[Number(i)]), Number(Number(sk.$fkey[Number(i)])))), 32);
  }
  return reduce(wrappingSub(ct.$fb, dot, 32));
}

export function mask(log: bigint): bigint
{
  return (() => { if ((log >= 32n)) {
  return u32.MAX;
} else {
  return fieldSub(fieldShl(1n, log), 1n);
} })();
}

export function max_lut_arity(log_q_lwe: bigint): bigint
{
  return (log_q_lwe - (2n));
}

export function mem_acc_absorb<T>(acc: any, r0: any, r1: any, r2: any, r3: any, addr: any, value: any, ts: any): T
{
  return fieldAdd(fieldAdd(fieldAdd(fieldAdd(acc, r0), fieldMul(addr, r1)), fieldMul(value, r2)), fieldMul(ts, r3));
}

export function mem_acc_absorb_q<T>(n: bigint, acc: QDyn<T>, one: QDyn<T>, addr: QDyn<T>, value: QDyn<T>, ts: QDyn<T>, r0: any, r1: any, r2: any, r3: any): QDyn<T>
{
  const c = q_scale_const(n, one, r0);
  const a = q_scale_const(n, addr, r1);
  const v = q_scale_const(n, value, r2);
  const t = q_scale_const(n, ts, r3);
  return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(fieldAdd(fieldAdd(fieldAdd(__clone(acc.$fq[Number(i)]), __clone(c.$fq[Number(i)])), __clone(a.$fq[Number(i)])), __clone(v.$fq[Number(i)])), __clone(t.$fq[Number(i)]));
})()), $fn: 0n });
}

export function mem_acc_absorb_vope<T>(n: bigint, acc: VopeDyn<T>, one: VopeDyn<T>, addr: VopeDyn<T>, value: VopeDyn<T>, ts: VopeDyn<T>, r0: any, r1: any, r2: any, r3: any): VopeDyn<T>
{
  return fieldAdd(fieldAdd(fieldAdd(fieldAdd(acc, vope_scale_const(n, one, r0)), vope_scale_const(n, addr, r1)), vope_scale_const(n, value, r2)), vope_scale_const(n, ts, r3));
}

export function mem_drain_check<T>(n: bigint, prod_q: QDyn<T>, cons_q: QDyn<T>, opening: T[]): boolean
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const k_diff = fieldAdd(__clone(prod_q.$fq[Number(i)]), __clone(cons_q.$fq[Number(i)]));
    ok = (ok && __equals(k_diff, __clone(opening[Number(i)])));
  }
  return ok;
}

export function mem_drain_open<T>(n: bigint, prod: VopeDyn<T>, cons: VopeDyn<T>): T[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(prod.$fv[Number(i)]), __clone(cons.$fv[Number(i)])));
}

export function memory_check_per_lane<T>(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n: bigint, challenges: T[]): MemoryCheckStateDyn<T, AdditiveHasher>[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const key = ChallengeKeyDyn.from_challenge(__clone(challenges[Number(i)]));
  return MemoryCheckStateDyn.new(ctx, key);
})());
}

export function mix_columns(state: bigint[])
{
  for (let c = 0n; c < 4n; c += 1n)   {
    const i = fieldMul(4n, c);
    const s0 = state[Number(i)];
    const s1 = state[Number(fieldAdd(i, 1n))];
    const s2 = state[Number(fieldAdd(i, 2n))];
    const s3 = state[Number(fieldAdd(i, 3n))];
    state[Number(i)] = fieldBitxor(fieldBitxor(fieldBitxor(gf_mul(s0, 2n), gf_mul(s1, 3n)), s2), s3);
    state[Number(fieldAdd(i, 1n))] = fieldBitxor(fieldBitxor(fieldBitxor(s0, gf_mul(s1, 2n)), gf_mul(s2, 3n)), s3);
    state[Number(fieldAdd(i, 2n))] = fieldBitxor(fieldBitxor(fieldBitxor(s0, s1), gf_mul(s2, 2n)), gf_mul(s3, 3n));
    state[Number(fieldAdd(i, 3n))] = fieldBitxor(fieldBitxor(fieldBitxor(gf_mul(s0, 3n), s1), s2), gf_mul(s3, 2n));
  }
}

export function mod_switch(from: bigint, to: bigint, x: bigint): bigint
{
  return (() => { if ((to >= from_)) {
  return embed_up(x);
} else {
  const shift = fieldSub(from_, to);
  const half = fieldShl(1n, fieldSub(shift, 1n));
  const rounded = Number(fieldShr(fieldAdd(BigInt(x), BigInt(half)), shift));
  return reduce(rounded);
} })();
}

export function mod_switch_lwe(n: bigint, from: bigint, to: bigint, ct: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = mod_switch(from_, to, ct.$fa[Number(i)]);
  }
  return new BinfheLweCiphertext({ $fa: a, $fb: mod_switch(from_, to, ct.$fb) });
}

export function mpcot_reg_choice_bits(n: bigint, t: bigint, alphas: bigint[], cot_r: boolean[]): Vec<boolean>
{
  const splen = (n / t);
  const h = Number(Math.clz32((splen) & -((splen) | 0)));
  let out = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const r = cot_r.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    out.push(...(spcot_choice_bits(alphas[Number(i)], h, r)));
  }
  return out;
}

export function mpcot_reg_receiver(n: bigint, t: bigint, alphas: bigint[], cot_t: Block[], msg: any): Vec<Block>
{
  const splen = (n / t);
  const h = Number(Math.clz32((splen) & -((splen) | 0)));
  let r = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const t_rows = cot_t.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const w = spcot_receiver_extend(alphas[Number(i)], splen, t_rows, msg.$fblocks[Number(i)]);
    r.push(...(w));
  }
  return r;
}

export function mpcot_reg_sender<R>(rng: any, delta: any, n: bigint, t: bigint, cot_q: Block[], choices: boolean[]): [Vec<Block>, MpcotRegSenderMsg]
{
  const splen = (n / t);
  const h = Number(Math.clz32((splen) & -((splen) | 0)));
  let s = ([] as any[]);
  let blocks = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const q = cot_q.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const ch = choices.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const [v, msg] = spcot_sender_extend(rng, delta, splen, q, ch);
    s.push(...(v));
    (blocks).push(msg);
  }
  return [s, new MpcotRegSenderMsg({ $fblocks: blocks })];
}

export function mpcot_uni_choice_bits(params: any, hash_seed: bigint[], table: (bigint | undefined)[], cot_r_chunks: Vec<boolean>[]): Vec<Vec<boolean>>
{
  const n = params.$fn;
  const m = cuckoo_table_size(params.$ft);
  const buckets = build_buckets(hash_seed, n, m);
  let out = ([] as any[]);
  for (let j = 0n; j < m; j += 1n)   {
    const need = fieldAdd(BigInt(buckets[Number(j)].length), 1n);
    const splen = next_pow2(need);
    const h = Number(Math.clz32((splen) & -((splen) | 0)));
    const p = (() => { const __match = table[Number(j)]; if (true) { return BigInt(buckets[Number(j)].length); } else { const val = __match;
return (buckets[Number(j)].findIndex((y) => __equals(y, val)))!; } })();
    (out).push(spcot_choice_bits(p, h, cot_r_chunks[Number(j)]));
  }
  return out;
}

export function mpcot_uni_receiver(params: any, table: (bigint | undefined)[], cot_t_chunks: Vec<Block>[], msg: any): Vec<Block>
{
  const n = params.$fn;
  const m = cuckoo_table_size(params.$ft);
  const buckets = build_buckets(msg.$fhash_seed, n, m);
  let r_bins = ([] as any[]);
  for (let j = 0n; j < m; j += 1n)   {
    const need = fieldAdd(BigInt(buckets[Number(j)].length), 1n);
    const splen = next_pow2(need);
    const p = (() => { const __match = table[Number(j)]; if (true) { return BigInt(buckets[Number(j)].length); } else { const val = __match;
return (buckets[Number(j)].findIndex((y) => __equals(y, val)))!; } })();
    const w = spcot_receiver_extend(p, splen, cot_t_chunks[Number(j)], msg.$fblocks[Number(j)]);
    (r_bins).push(w);
  }
  let r = [] as any[];
  for (let x = 0n; x < n; x += 1n)   {
    let acc = Array.from({length: Number(16n)}, () => 0n);
    for (const j of unique_bins(msg.$fhash_seed, x, m))     {
      const pos = (buckets[Number(j)].findIndex((y) => __equals(y, x)))!;
      for (let b = 0n; b < 16n; b += 1n)       {
        acc[Number(b)] = fieldBitxor(acc[Number(b)], r_bins[Number(j)][Number(pos)][Number(b)]);
      }
    }
    r[Number(x)] = acc;
  }
  return r;
}

export function mpcot_uni_sender<R>(rng: any, delta: any, params: any, hash_seed: bigint[], cot_q_chunks: Vec<Block>[], choices_chunks: Vec<boolean>[]): [Vec<Block>, MpcotUniSenderMsg]
{
  const n = params.$fn;
  const m = cuckoo_table_size(params.$ft);
  const buckets = build_buckets(hash_seed, n, m);
  let s_bins = ([] as any[]);
  let blocks = ([] as any[]);
  for (let j = 0n; j < m; j += 1n)   {
    const need = fieldAdd(BigInt(buckets[Number(j)].length), 1n);
    const splen = next_pow2(need);
    const [v, msg] = spcot_sender_extend(rng, delta, splen, cot_q_chunks[Number(j)], choices_chunks[Number(j)]);
    (s_bins).push(v);
    (blocks).push(msg);
  }
  let s = [] as any[];
  for (let x = 0n; x < n; x += 1n)   {
    let acc = Array.from({length: Number(16n)}, () => 0n);
    for (const j of unique_bins(hash_seed, x, m))     {
      const pos = (buckets[Number(j)].findIndex((y) => __equals(y, x)))!;
      for (let b = 0n; b < 16n; b += 1n)       {
        acc[Number(b)] = fieldBitxor(acc[Number(b)], s_bins[Number(j)][Number(pos)][Number(b)]);
      }
    }
    s[Number(x)] = acc;
  }
  return [s, new MpcotUniSenderMsg({ $fhash_seed: hash_seed, $fblocks: blocks })];
}

export function mul_4x4(a: bigint[], b: bigint[]): bigint[]
{
  let r = Array.from({length: Number(8n)}, () => 0n);
  for (let i = 0n; i < 4n; i += 1n)   {
    let carry: bigint = 0n;
    for (let j = 0n; j < 4n; j += 1n)     {
      const v = fieldAdd(fieldAdd((r[Number(fieldAdd(i, j))] as unknown as bigint), fieldMul((a[Number(i)] as unknown as bigint), (b[Number(j)] as unknown as bigint))), (carry as unknown as bigint));
      r[Number(fieldAdd(i, j))] = BigInt(v);
      carry = BigInt(fieldShr(v, 64n));
    }
    r[Number(fieldAdd(i, 4n))] = carry;
  }
  return r;
}

export function mul_exact(log: bigint, a: bigint, c: bigint): bigint
{
  return reduce(log, BigInt(Math.imul(Number(a), Number(c))));
}

export function mul_mod(left: bigint, right: bigint, modulus: bigint): bigint
{
  return BigInt((fieldMul((left as unknown as bigint), (right as unknown as bigint)) % (modulus as unknown as bigint)));
}

export function new_pool<R>(rng: any, params: any): [CotPoolSender, CotPoolReceiver]
{
  const m = params.seed_cot_count(false);
  const [seed_s, seed_r] = sample_seed_cots(rng, m);
  return [new CotPoolSender({ $fparams: params, $fseed: seed_s, $fout: [] as any[], $fraise_n: undefined }), new CotPoolReceiver({ $fparams: params, $fseed: seed_r, $fout_x: [] as any[], $fout_z: [] as any[] })];
}

export function next_pow2(x: bigint): bigint
{
  return BigInt(Math.max(Number(BigInt(1 << Math.ceil(Math.log2(Number(x))))), Number(2n)));
}

export function ot_recv<G, D, R>(ctx: { GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng: any, s: number /* G::Element */, c: boolean): [BaseOtReceiverDyn<G, D>, OtReceiverMsgDyn<G>]
{
  const x = ctx.GClass.random_scalar(rng);
  const g = ctx.GClass.generator();
  const gx = ctx.GClass.scalar_mul(g, x);
  const r = (() => { if (c) {
  return ctx.GClass.add(s, gx);
} else {
  return gx;
} })();
  return [new BaseOtReceiverDyn({ $fx: x, $fs: s, $fc: c, $f_d: PhantomData }), new OtReceiverMsgDyn({ $fr: r })];
}

export function ot_recv_choice<G, D>(state: BaseOtReceiverDyn<G, D>): boolean
{
  return state.$fc;
}

export function ot_recv_finish<G, D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, state: BaseOtReceiverDyn<G, D>): bigint[]
{
  const sx = ctx.GClass.scalar_mul(state.$fs, state.$fx);
  let h = ctx.newD();
  ctx.GClass.write_element(sx, h);
  return [...h.finalize()];
}

export function ot_recv_payload<D>(kc: bigint[], ec: bigint[], mc: bigint[])
{
  for (let i = 0n; i < BigInt(ec.length); i += 1n)   {
    mc[Number(i)] = fieldBitxor(ec[Number(i)], kc[Number(i)]);
  }
}

export function ot_send_finish<G, D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, state: BaseOtSenderDyn<G, D>, msg: OtReceiverMsgDyn<G>): [bigint[], bigint[]]
{
  const ry = ctx.GClass.scalar_mul(msg.$fr, state.$fy);
  const s_inv = ctx.GClass.neg(state.$fs);
  const r_minus_s = ctx.GClass.add(msg.$fr, s_inv);
  const r_minus_s_y = ctx.GClass.scalar_mul(r_minus_s, state.$fy);
  let h0 = ctx.newD();
  ctx.GClass.write_element(ry, h0);
  let h1 = ctx.newD();
  ctx.GClass.write_element(r_minus_s_y, h1);
  return [[...h0.finalize()], [...h1.finalize()]];
}

export function ot_send_payload<D>(k0: bigint[], k1: bigint[], m0: bigint[], m1: bigint[], e0: bigint[], e1: bigint[])
{
  for (let i = 0n; i < BigInt(m0.length); i += 1n)   {
    e0[Number(i)] = fieldBitxor(m0[Number(i)], k0[Number(i)]);
  }
  for (let i = 0n; i < BigInt(m1.length); i += 1n)   {
    e1[Number(i)] = fieldBitxor(m1[Number(i)], k1[Number(i)]);
  }
}

export function ot_send_setup<G, D, R>(ctx: { GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng: any): [BaseOtSenderDyn<G, D>, number /* G::Element */]
{
  const y = ctx.GClass.random_scalar(rng);
  const g = ctx.GClass.generator();
  const s = ctx.GClass.scalar_mul(g, y);
  const t = ctx.GClass.scalar_mul(s, y);
  return [new BaseOtSenderDyn({ $fy: y, $fs: __clone(s), $ft: t, $f_d: PhantomData }), s];
}

export function pack_kappa(bits: boolean[]): bigint[]
{
  let out = Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n);
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    if (bits[Number(i)])     {
      out[Number((i / 8n))] = fieldBitor(out[Number((i / 8n))], fieldShl(1n, (i % 8n)));
    }
  }
  return out;
}

export function parse_profile(tag: bigint): Result<ProfileId, DecodeError>
{
  return (() => { const __match = tag; if (__match === 0n) { return new ProfileId_Toy(); } else if (__match === 1n) { return new ProfileId_ToyNoisy(); } else if (__match === 2n) { return new ProfileId_Std128(); } else if (__match === 3n) { return new ProfileId_Custom(); } else { return new DecodeError_UnknownTag(); } })();
}

export function pedersen_commit(gens: EdPoint[], h: any, x: bigint[][], blind: bigint[]): EdPoint
{
  let acc = ed_scalar_mul(h, blind);
  let i = 0n;
  while ((i < BigInt(x.length)))   {
    acc = ed_add(acc, ed_scalar_mul(gens[Number(i)], x[Number(i)]));
    i = fieldAdd(i, 1n);
  }
  return acc;
}

export function poly_add_neg(n: bigint, a: bigint[], b: bigint[]): bigint[]
{
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    result[Number(i)] = wrappingAdd(a[Number(i)], b[Number(i)], 32);
  }
  return result;
}

export function poly_decompose(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, p: bigint[]): bigint[][]
{
  const bg = fieldShl(1n, bs_bg_log);
  const mask = Number(fieldSub(bg, 1n));
  let result = Array.from({length: Number(bs_ell)}, () => Array.from({length: Number(big_n)}, () => 0n));
  for (let i = 0n; i < big_n; i += 1n)   {
    const x = p[Number(i)];
    const tail_bits = (32n - (Number(fieldMul(bs_bg_log, bs_ell))));
    const rounded = (() => { if (((tail_bits > 0n) && (tail_bits < 32n))) {
  return wrappingAdd(x, fieldShl(1n, fieldSub(tail_bits, 1n)), 32);
} else {
  return x;
} })();
    for (let j = 0n; j < bs_ell; j += 1n)     {
      const shift = (32n - (Number(fieldMul(bs_bg_log, fieldAdd(j, 1n)))));
      result[Number(j)][Number(i)] = (() => { if ((shift < 32n)) {
  return fieldBitand(fieldShr(rounded, shift), mask);
} else {
  return 0n;
} })();
    }
  }
  return result;
}

export function poly_mul_neg(n: bigint, a: bigint[], b: bigint[]): bigint[]
{
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    for (let j = 0n; j < n; j += 1n)     {
      const deg = fieldAdd(i, j);
      if ((deg < n))       {
        result[Number(deg)] = wrappingAdd(result[Number(deg)], BigInt(Math.imul(Number(a[Number(i)]), Number(b[Number(j)]))), 32);
      } else       {
        result[Number(fieldSub(deg, n))] = wrappingSub(result[Number(fieldSub(deg, n))], BigInt(Math.imul(Number(a[Number(i)]), Number(b[Number(j)]))), 32);
      }
    }
  }
  return result;
}

export function poly_rotate(n: bigint, p: bigint[], exp: bigint): bigint[]
{
  const exp_1 = (exp % fieldMul(2n, n));
  if (__equals(exp_1, 0n))   {
    return p;
  }
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    const new_pos = fieldAdd(i, exp_1);
    if ((new_pos < n))     {
      result[Number(new_pos)] = wrappingAdd(result[Number(new_pos)], p[Number(i)], 32);
    } else if ((new_pos < fieldMul(2n, n)))     {
      result[Number(fieldSub(new_pos, n))] = wrappingSub(result[Number(fieldSub(new_pos, n))], p[Number(i)], 32);
    } else     {
      result[Number(fieldSub(new_pos, fieldMul(2n, n)))] = wrappingAdd(result[Number(fieldSub(new_pos, fieldMul(2n, n)))], p[Number(i)], 32);
    }
  }
  return result;
}

export function poly_sub_neg(n: bigint, a: bigint[], b: bigint[]): bigint[]
{
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    result[Number(i)] = wrappingSub(a[Number(i)], b[Number(i)], 32);
  }
  return result;
}

export function pow_mod(value: bigint, exponent: bigint, modulus: bigint): bigint
{
  let result = 1n;
  while (!__equals(exponent, 0n))   {
    if (!__equals(fieldBitand(exponent, 1n), 0n))     {
      result = mul_mod(result, value, modulus);
    }
    value = mul_mod(value, value, modulus);
    exponent = fieldShr(exponent, 1n);
  }
  return result;
}

export function prg_to_bools(ctx: { newD: () => any }, seed: bigint[], out: boolean[])
{
  let counter: bigint = 0n;
  let pos = 0n;
  while ((pos < BigInt(out.length)))   {
    let h = ctx.newD();
    h.update(seed);
    h.update([(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn]);
    const block = [...h.finalize()];
    const block_bytes: bigint[] = asRefU8(block);
    for (const byte of block_bytes)     {
      for (let bit = 0n; bit < 8n; bit += 1n)       {
        if ((pos >= BigInt(out.length)))         {
          return;
        }
        out[Number(pos)] = __equals(fieldBitand(fieldShr(byte, bit), 1n), 1n);
        pos = fieldAdd(pos, 1n);
      }
    }
    counter = fieldAdd(counter, 1n);
  }
}

export function prg_with_index(ctx: { newD: () => any }, seed: bigint[], idx: bigint, out: bigint[])
{
  let counter: bigint = 0n;
  let pos = 0n;
  while ((pos < BigInt(out.length)))   {
    let h = ctx.newD();
    h.update(seed);
    h.update([(idx) & 0xFFn, ((idx) >> 8n) & 0xFFn, ((idx) >> 16n) & 0xFFn, ((idx) >> 24n) & 0xFFn]);
    h.update([(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn]);
    const block = [...h.finalize()];
    const block_bytes: bigint[] = asRefU8(block);
    const take = BigInt(Math.min(Number(fieldSub(BigInt(out.length), pos)), Number(BigInt(block_bytes.length))));
    (out.slice(Number(pos), Number(fieldAdd(pos, take)))).splice(0, (block_bytes.slice(0, Number(take))).length, ...(block_bytes.slice(0, Number(take))));
    pos = fieldAdd(pos, take);
    counter = fieldAdd(counter, 1n);
  }
}

export function priv_ks(big_n: bigint, log_q: bigint, priv_ell: bigint, priv_base_log: bigint, src: BinfheLweCiphertextDyn, col: BinfheRlweCiphertextDyn[][], body: BinfheRlweCiphertextDyn[]): BinfheRlweCiphertextDyn
{
  let out = new BinfheRlweCiphertextDyn({ $fa: Array.from({length: Number(big_n)}, () => 0n), $fb: Array.from({length: Number(big_n)}, () => 0n), $fn: 0n });
  for (let i = 0n; i < big_n; i += 1n)   {
    const digits = gadget_decompose(src.$fa[Number(i)]);
    for (const [l, d] of digits.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      if (__equals(d, 0n))       {
        continue;
      }
      const entry = col[Number(i)][Number(l)];
      for (let k = 0n; k < big_n; k += 1n)       {
        out.$fa[Number(k)] = wrappingAdd(out.$fa[Number(k)], BigInt(Math.imul(Number(d), Number(entry.$fa[Number(k)]))), 32);
        out.$fb[Number(k)] = wrappingAdd(out.$fb[Number(k)], BigInt(Math.imul(Number(d), Number(entry.$fb[Number(k)]))), 32);
      }
    }
  }
  const digits = gadget_decompose(src.$fb);
  for (const [l, d] of digits.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    if (__equals(d, 0n))     {
      continue;
    }
    const entry = body[Number(l)];
    for (let k = 0n; k < big_n; k += 1n)     {
      out.$fa[Number(k)] = wrappingAdd(out.$fa[Number(k)], BigInt(Math.imul(Number(d), Number(entry.$fa[Number(k)]))), 32);
      out.$fb[Number(k)] = wrappingAdd(out.$fb[Number(k)], BigInt(Math.imul(Number(d), Number(entry.$fb[Number(k)]))), 32);
    }
  }
  for (let k = 0n; k < big_n; k += 1n)   {
    out.$fa[Number(k)] = reduce(out.$fa[Number(k)]);
    out.$fb[Number(k)] = reduce(out.$fb[Number(k)]);
  }
  return out;
}

export function profile_tag(profile: any): bigint
{
  return (() => { const __match = profile; if (true) { return 0n; } else if (true) { return 1n; } else if (true) { return 2n; } else { return 3n; } })();
}

export function push_block(buf: Vec<bigint>, b: any)
{
  buf.push(...(b));
}

export function push_u32(buf: Vec<bigint>, x: bigint)
{
  buf.push(...([(x) & 0xFFn, ((x) >> 8n) & 0xFFn, ((x) >> 16n) & 0xFFn, ((x) >> 24n) & 0xFFn]));
}

export function put_ids(bytes: Vec<bigint>, ids: bigint[])
{
  put_u32(bytes, Number(BigInt(ids.length)));
  for (const id of ids)   {
    put_u32(bytes, id);
  }
}

export function put_u32(bytes: Vec<bigint>, value: bigint)
{
  bytes.push(...([(value) & 0xFFn, ((value) >> 8n) & 0xFFn, ((value) >> 16n) & 0xFFn, ((value) >> 24n) & 0xFFn]));
}

export function q_bitpack<T>(ctx: { defaultT: () => any }, bits: bigint, n: bigint, bit_values: QDyn<T>[], pow2: T[]): QDyn<T>
{
  let acc = new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT()), $fn: 0n });
  for (let index = 0n; index < bits; index += 1n)   {
    const scaled = q_scale_const(n, bit_values[Number(index)], pow2[Number(index)]);
    acc = new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(acc.$fq[Number(i)]), __clone(scaled.$fq[Number(i)]))), $fn: 0n });
  }
  return acc;
}

export function q_scale_const<T>(n: bigint, q: QDyn<T>, c: any): QDyn<T>
{
  return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(q.$fq[Number(i)]), __clone(c))), $fn: 0n });
}

export function random_nonzero_delta<T, R>(n: bigint, rng: any, sample_t: any, is_zero: any): DeltaDyn<T>
{
  return new DeltaDyn({ $fdelta: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  let x = sample_t(rng);
  let tries = 0n;
  while ((is_zero(x) && (tries < 64n)))   {
    x = sample_t(rng);
    tries = fieldAdd(tries, 1n);
  }
  return x;
})()), $fn: 0n });
}

export function read_op(reader: any): Result<PlanOp, DecodeError>
{
  return (() => { const __match = reader.byte(); if (__match === 0n) { return (() => {
  const out = reader.u32();
  const value = (() => { const __match = reader.byte(); if (__match === 0n) { return false; } else if (__match === 1n) { return true; } else { return new DecodeError_UnknownTag(); } })();
  return new Const({ $fout: out, $fvalue: value });
})(); } else if (__match === 1n) { return new Not({ $finput: reader.u32(), $fout: reader.u32() }); } else if (__match === 2n) { return (() => {
  const count = reader.count();
  let ids = ([] as any[]);
  for (let _ = 0n; _ < count; _ += 1n)   {
    (ids).push(reader.u32());
  }
  const inputs = LutInputs.from_slice(ids);
  const table: LutId = reader.u32();
  const out: WireId = reader.u32();
  return new Lut({ $finputs: inputs, $ftable: table, $fout: out });
})(); } else if (__match === 3n) { return new CircuitBootstrap({ $finput: reader.u32(), $fout: reader.u32() }); } else if (__match === 4n) { return (() => {
  const sel: RgswId = reader.u32();
  const then_cell = reader.u32();
  const else_cell = reader.u32();
  const out = reader.u32();
  return new RgswMux({ $fsel: sel, $fthen_cell: then_cell, $felse_cell: else_cell, $fout: out });
})(); } else { return new DecodeError_UnknownTag(); } })();
}

export function recompute_tree(r: bigint[], total_leaves: bigint): Vec<bigint[]>
{
  const total_nodes = fieldSub(fieldMul(2n, total_leaves), 1n);
  let tree = [] as any[];
  tree[Number(0n)] = r;
  for (let node = 0n; node < fieldSub(total_leaves, 1n); node += 1n)   {
    const parent = tree[Number(node)];
    const [left, right] = AesCtrLengthDoubler.double(parent);
    tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left[0];
    tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right[0];
  }
  return tree;
}

export function recover_sibling(layer: Block[], sum: any, offset: bigint, select: boolean)
{
  const on = fieldAdd(offset, Number(select));
  const off = fieldAdd(offset, Number(!select));
  if ((on < BigInt(layer.length)))   {
    layer[Number(on)] = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  }
  if ((off < BigInt(layer.length)))   {
    layer[Number(off)] = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  }
  let value = sum;
  let i = Number(!select);
  while ((i < BigInt(layer.length)))   {
    value = xor_block(value, layer[Number(i)]);
    i = fieldAdd(i, 2n);
  }
  layer[Number(off)] = value;
}

export function reduce(log: bigint, x: bigint): bigint
{
  return fieldBitand(x, mask(log));
}

export function reduce_wide(t: bigint[]): Fe25519
{
  let acc = Array.from({length: Number(5n)}, () => 0n);
  let c: bigint = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const v = fieldAdd(fieldAdd((t[Number(i)] as unknown as bigint), fieldMul((t[Number(fieldAdd(4n, i))] as unknown as bigint), 38n)), c);
    acc[Number(i)] = BigInt(v);
    c = fieldShr(v, 64n);
  }
  acc[Number(4n)] = BigInt(c);
  let out = Array.from({length: Number(4n)}, () => 0n);
  let c_1: bigint = fieldMul((acc[Number(4n)] as unknown as bigint), 38n);
  for (let i = 0n; i < 4n; i += 1n)   {
    const v = fieldAdd((acc[Number(i)] as unknown as bigint), c_1);
    out[Number(i)] = BigInt(v);
    c_1 = fieldShr(v, 64n);
  }
  if (!__equals(c_1, 0n))   {
    let c2: bigint = fieldMul(c_1, 38n);
    for (let i = 0n; i < 4n; i += 1n)     {
      const v = fieldAdd((out[Number(i)] as unknown as bigint), c2);
      out[Number(i)] = BigInt(v);
      c2 = fieldShr(v, 64n);
    }
  }
  return fe_canonicalize(out);
}

export function refill<R>(rng: any, sender: any, receiver: any)
{
  let params = sender.$fparams;
  (() => { const __match = sender.$fraise_n.take(); if (__match !== null && __match !== undefined) { const n = __match;
return (() => {
  let raised = params;
  raised.$fn = n;
  params.$fn = n;
  sender.$fparams = params;
  receiver.$fparams = params;
})(); } else { return (() => {
})(); } })();
  const out = ferret_extend(rng, params, sender.$fseed, receiver.$fseed);
  sender.$fseed = out.$fsender_seed;
  receiver.$fseed = out.$freceiver_seed;
  sender.$fout.push(...(out.$fsender_out));
  receiver.$fout_x.push(...(out.$frecv_x));
  receiver.$fout_z.push(...(out.$frecv_z));
}

export function rgsw_encrypt<R>(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, m: boolean, sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RgswCiphertextDyn
{
  const msg_bit = (() => { if (m) {
  return 1n;
} else {
  return 0n;
} })();
  const rows = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const shift = (32n - (Number(fieldMul(bs_bg_log, fieldAdd(j, 1n)))));
  const g_factor = (((1n) << (shift)) & 0xFFFFFFFFn);
  const contrib = BigInt(Math.imul(Number(msg_bit), Number(g_factor)));
  let rlwe0 = rlwe_encrypt_scalar(big_n, 0n, sk, noise_bits, rng);
  rlwe0.$fa[Number(0n)] = wrappingAdd(rlwe0.$fa[Number(0n)], contrib, 32);
  const rlwe1 = rlwe_encrypt_scalar(big_n, contrib, sk, noise_bits, rng);
  return new RgswRowDyn({ $frlwe0: rlwe0, $frlwe1: rlwe1, $fbig_n: 0n });
})());
  return new RgswCiphertextDyn({ $frows: rows, $fbig_n: 0n, $fbs_ell: 0n });
}

export function rlwe_add(big_n: bigint, a: RlweCiphertextDyn, b: RlweCiphertextDyn): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ $fa: poly_add_neg(a.$fa, b.$fa), $fb: poly_add_neg(a.$fb, b.$fb), $fbig_n: 0n });
}

export function rlwe_encrypt_poly<R>(big_n: bigint, msg_poly: bigint[], sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(a, sk.$fkey);
  for (let i = 0n; i < big_n; i += 1n)   {
    b[Number(i)] = wrappingAdd(wrappingAdd(b[Number(i)], small_noise(noise_bits, rng), 32), msg_poly[Number(i)], 32);
  }
  return new RlweCiphertextDyn({ $fa: a, $fb: b, $fbig_n: 0n });
}

export function rlwe_encrypt_scalar<R>(big_n: bigint, m: bigint, sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(a, sk.$fkey);
  b[Number(0n)] = wrappingAdd(wrappingAdd(b[Number(0n)], small_noise(noise_bits, rng), 32), m, 32);
  return new RlweCiphertextDyn({ $fa: a, $fb: b, $fbig_n: 0n });
}

export function rlwe_rotate(big_n: bigint, ct: RlweCiphertextDyn, exp: bigint): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ $fa: poly_rotate(ct.$fa, exp), $fb: poly_rotate(ct.$fb, exp), $fbig_n: 0n });
}

export function rlwe_sub(big_n: bigint, a: RlweCiphertextDyn, b: RlweCiphertextDyn): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ $fa: poly_sub_neg(a.$fa, b.$fa), $fb: poly_sub_neg(a.$fb, b.$fb), $fbig_n: 0n });
}

export function sample_block<R>(rng: any): Block
{
  let b = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  for (const chunk of __chunks(b, Number(4n)))   {
    const x = [(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn];
    (chunk).splice(0, (x.slice(0, Number(BigInt(chunk.length)))).length, ...(x.slice(0, Number(BigInt(chunk.length)))));
  }
  return b;
}

export function sample_bytes<R>(l: bigint, rng: any): bigint[]
{
  let b = Array.from({length: Number(l)}, () => 0n);
  for (const chunk of __chunks(b, Number(4n)))   {
    (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
  }
  return b;
}

export function sample_error<R>(log: bigint, eta: bigint, rng: any): bigint
{
  return reduce(Number(cbd(eta, rng)));
}

export function sample_extract(big_n: bigint, rlwe: RlweCiphertextDyn): LweCiphertextDyn
{
  let a_lwe = Array.from({length: Number(big_n)}, () => 0n);
  a_lwe[Number(0n)] = rlwe.$fa[Number(0n)];
  for (let i = 1n; i < big_n; i += 1n)   {
    a_lwe[Number(i)] = wrappingNeg(rlwe.$fa[Number(fieldSub(big_n, i))], 32);
  }
  return new LweCiphertextDyn({ $fa: a_lwe, $fb: rlwe.$fb[Number(0n)], $fn_lwe: 0n });
}

export function sample_noise<R>(rng: any): Zq
{
  const span = fieldAdd(fieldMul(2n, LWE_NOISE_BOUND), 1n);
  const raw = (rng.next_u32() % span);
  return (() => { if ((raw <= LWE_NOISE_BOUND)) {
  return raw;
} else {
  return zq_neg(fieldSub(raw, LWE_NOISE_BOUND));
} })();
}

export function sample_regular_noise<R>(rng: any, n: bigint, t: bigint): Vec<bigint>
{
  const splen = (n / t);
  return Array.from({length: Number(t - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (Number(rng.next_u32()) % splen));
}

export function sample_seed<R>(rng: any): bigint[]
{
  let s = Array.from({length: Number(16n)}, () => 0n);
  for (const chunk of __chunks(s, Number(4n)))   {
    (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
  }
  return s;
}

export function sample_seed_cots<R>(rng: any, m: bigint): [FerretSenderSeed, FerretReceiverSeed]
{
  let delta = Array.from({length: Number(16n)}, () => 0n);
  for (const chunk of __chunks(delta, Number(4n)))   {
    (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
  }
  let q = ([] as any[]);
  let u = ([] as any[]);
  let w = ([] as any[]);
  for (let _ = 0n; _ < m; _ += 1n)   {
    let row = Array.from({length: Number(16n)}, () => 0n);
    for (const chunk of __chunks(row, Number(4n)))     {
      (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
    }
    const bit = __equals(fieldBitand(rng.next_u32(), 1n), 1n);
    const t = (() => { if (bit) {
  return xor_block(row, delta);
} else {
  return row;
} })();
    (q).push(row);
    (u).push(bit);
    (w).push(t);
  }
  return [new FerretSenderSeed({ $fdelta: delta, $fq: q }), new FerretReceiverSeed({ $fu: u, $fw: w })];
}

export function sample_uniform(random: any, modulus: bigint, output: bigint[]): Result<void, Error>
{
  const threshold = fieldSub(u64.MAX, (u64.MAX % modulus));
  for (const value of output)   {
    while (true)     {
      let bytes = Array.from({length: Number(8n)}, () => 0n);
      random.fill_bytes(bytes);
      const candidate = u64_from_le_bytes(bytes);
      if ((candidate < threshold))       {
        value = (candidate % modulus);
        break;
      }
    }
  }
  return [];
}

export function sample_uniform_points<R>(rng: any, n: bigint, t: bigint): Vec<bigint>
{
  let pts = ([] as any[]);
  while ((BigInt(pts.length) < t))   {
    const x = (Number(rng.next_u32()) % n);
    if (!pts.includes(x))     {
      (pts).push(x);
    }
  }
  pts.sort_unstable();
  return pts;
}

export function sample_zq<R>(rng: any): Zq
{
  return fieldBitand(rng.next_u32(), LWE_Q_MASK);
}

export function selector_margin(log_q_lwe: bigint, k: bigint, input_noise_bound: bigint): boolean
{
  if ((fieldAdd(Number(k), 2n) >= log_q_lwe))   {
    return false;
  }
  const margin = fieldShl(1n, fieldSub(fieldSub(log_q_lwe, Number(k)), 2n));
  const weight = Number(fieldSub(fieldShl(1n, k), 1n));
  return (BigInt(Math.imul(Number(weight), Number(input_noise_bound))) < margin);
}

export function shift_rows(state: bigint[])
{
  const t = state[Number(1n)];
  state[Number(1n)] = state[Number(5n)];
  state[Number(5n)] = state[Number(9n)];
  state[Number(9n)] = state[Number(13n)];
  state[Number(13n)] = t;
  const t_1 = state[Number(2n)];
  state[Number(2n)] = state[Number(10n)];
  state[Number(10n)] = t_1;
  const t_2 = state[Number(6n)];
  state[Number(6n)] = state[Number(14n)];
  state[Number(14n)] = t_2;
  const t_3 = state[Number(15n)];
  state[Number(15n)] = state[Number(11n)];
  state[Number(11n)] = state[Number(7n)];
  state[Number(7n)] = state[Number(3n)];
  state[Number(3n)] = t_3;
}

export function sign(ctx: { newD: () => any, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, sk: any, pk: any, message: bigint[], iv_seed: bigint[], prover: any): FaestSignature
{
  const iv: bigint[] = aes128_encrypt(iv_seed, Array.from({length: Number(LAMBDA_BYTES)}, () => 0n));
  const r: bigint[] = aes128_encrypt(sk[0], iv);
  const commitment: BavcCommitmentDyn = BavcDyn.commit(ctx, r, iv, TAU, SUB_VOLE_N);
  const mu: Vec<bigint> = (() => {
  let h = new Sha3_256();
  h.update(pk[0]);
  h.update(message);
  return [...h.finalize()];
})();
  const chall_1 = chall1(mu, iv, commitment.$froot, fieldAdd(LAMBDA_BYTES, 8n), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const nodes = BavcDyn.collect_open_nodes(deltas, recompute_tree(r, fieldMul(TAU, SUB_VOLE_N)), TAU, SUB_VOLE_N);
  const hidden_commits: Vec<bigint[]> = deltas.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([i, d]: any) => commitment.$fcommitments[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), d))]);
  const opening = new BavcOpeningDyn({ $fhidden_commits: __clone(hidden_commits), $fnodes: __clone(nodes), $fcom_bytes: 0n });
  const _ = opening;
  let sub_voles = ([] as any[]);
  for (let i = 0n; i < TAU; i += 1n)   {
    const seeds_i: Vec<(bigint[] | undefined)> = Array.from({length: Number(SUB_VOLE_N - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => commitment.$fseeds[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), j))]);
    (sub_voles).push(convert_to_vole(seeds_i, iv, Number(i), L_HAT_BYTES));
  }
  const big_vole: BigVoleProver = concat_small_voles(sub_voles);
  const corrections_flat: Vec<bigint> = big_vole.$fc.flat();
  const chall_2 = chall2(chall_1, big_vole.$fu, corrections_flat, fieldAdd(LAMBDA_BYTES, 8n), false);
  const hash_key = hash_key_from_chall(chall_2);
  const qs_proof = prover.prove_aes_witness(big_vole, hash_key);
  const [chall_3, counter] = (grind_chall3(chall_2, qs_proof.$fa_hat, qs_proof.$fb_hat, qs_proof.$fc_hat_base, LAMBDA_BYTES, W_GRIND, false, 1000000n))!;
  let c_hat_with_counter = __clone(qs_proof.$fc_hat_base);
  c_hat_with_counter.push(...([(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn]));
  return new FaestSignature({ $fiv: iv, $fbavc_root: __clone(commitment.$froot), $fhidden_commits: hidden_commits, $fnodes: nodes, $fcorrections: __clone(big_vole.$fc), $fvole_u: __clone(big_vole.$fu), $fqs_proof: qs_proof, $fc_hat_with_counter: c_hat_with_counter, $fchall_3: chall_3, $fcounter: counter });
}

export function signed_to_mod(value: bigint, modulus: bigint): bigint
{
  return (() => { if ((value < 0n)) {
  const magnitude = (value.unsigned_abs() % modulus);
  return (() => { if (__equals(magnitude, 0n)) {
  return 0n;
} else {
  return fieldSub(modulus, magnitude);
} })();
} else {
  return (BigInt(value) % modulus);
} })();
}

export function small_noise<R>(noise_bits: bigint, rng: any): bigint
{
  if ((noise_bits >= 32n))   {
    return rng.next_u32();
  }
  const raw: bigint = rng.next_u32();
  const mask = wrappingSub(fieldShl(1n, noise_bits), 1n, 32);
  const small = fieldBitand(raw, mask);
  return (() => { if (((noise_bits > 0n) && !__equals(fieldShr(small, fieldSub(noise_bits, 1n)), 0n))) {
  return fieldBitor(small, !mask);
} else {
  return small;
} })();
}

export function softspoken_cot_extend<D, R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, k: bigint, m: bigint, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): SoftSpokenOutLoweredDyn<D>
{
  const [sender_r0, receiver_v] = iknp_cot_extend(ctx, m, l, rng_s, rng_r, receiver_bits, delta_msg);
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
  for (let j = 0n; j < m; j += 1n)   {
    let r0_reconstructed = Array.from({length: Number(l)}, () => 0n);
    if (receiver_bits[Number(j)])     {
      for (let b = 0n; b < l; b += 1n)       {
        r0_reconstructed[Number(b)] = fieldBitxor(receiver_v[Number(j)][Number(b)], delta_msg[Number(b)]);
      }
    } else     {
      r0_reconstructed = receiver_v[Number(j)];
    }
    hr.update(r0_reconstructed);
  }
  const receiver_tag = [...hr.finalize()];
  return new SoftSpokenOutLoweredDyn({ $fsender_r0: sender_r0, $freceiver_v: receiver_v, $fsender_tag: sender_tag, $freceiver_tag: receiver_tag, $fm: 0n, $fl: 0n });
}

export function softspoken_cot_extend_base<D, R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): SoftSpokenOutDynDyn<D>
{
  const [sender_r0, receiver_v] = iknp_cot_extend_base(ctx, l, rng_s, rng_r, receiver_bits, delta_msg);
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
  for (let j = 0n; j < BigInt(receiver_bits.length); j += 1n)   {
    let r0_reconstructed = Array.from({length: Number(l)}, () => 0n);
    if (receiver_bits[Number(j)])     {
      for (let b = 0n; b < l; b += 1n)       {
        r0_reconstructed[Number(b)] = fieldBitxor(receiver_v[Number(j)][Number(b)], delta_msg[Number(b)]);
      }
    } else     {
      r0_reconstructed = receiver_v[Number(j)];
    }
    hr.update(r0_reconstructed);
  }
  const receiver_tag = [...hr.finalize()];
  return new SoftSpokenOutDynDyn({ $fsender_r0: sender_r0, $freceiver_v: receiver_v, $fsender_tag: sender_tag, $freceiver_tag: receiver_tag, $fl: 0n });
}

export function softspoken_cot_extend_dyn<D, R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): SoftSpokenOutDynDyn<D>
{
  return softspoken_cot_extend_base(ctx, l, rng_s, rng_r, receiver_bits, delta_msg);
}

export function spcot_choice_bits(alpha: bigint, h: bigint, cot_r: boolean[]): Vec<boolean>
{
  return Array.from({length: Number(h - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(fieldBitxor(cot_r[Number(i)], bit_msb(alpha, h, i)), true));
}

export function spcot_consistency_check(delta: any, v: Block[], w: Block[], extra_q: Block[], extra_r: boolean[], extra_t: Block[], transcript: bigint[]): boolean
{
  const _ = [extra_q, extra_r, extra_t];
  const n = BigInt(v.length);
  let h = new Sha3_256();
  h.update(new Uint8Array([/* byte string */]));
  h.update(transcript);
  const seed = [...h.finalize()];
  let chi = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  (chi).splice(0, (seed.slice(0, Number(KAPPA_BYTES))).length, ...(seed.slice(0, Number(KAPPA_BYTES))));
  let chi_pow = field_from_block(chi);
  let chis = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    (chis).push(block_from_field(chi_pow));
    chi_pow = field_mul(chi_pow, field_from_block(chi));
  }
  let alpha = undefined;
  for (let i = 0n; i < n; i += 1n)   {
    if (!__equals(v[Number(i)], w[Number(i)]))     {
      if ((alpha) != null)       {
        return false;
      }
      alpha = i;
    }
  }
  const a = alpha;
  if (!__equals(xor_block(v[Number(a)], w[Number(a)]), delta))   {
    return false;
  }
  let ip_v = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  let ip_w = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    ip_v = xor_block(ip_v, field_mul_block(chis[Number(i)], v[Number(i)]));
    ip_w = xor_block(ip_w, field_mul_block(chis[Number(i)], w[Number(i)]));
  }
  const hv = hash_prime(ip_v);
  const hw = hash_prime(xor_block(ip_w, field_mul_block(chis[Number(a)], delta)));
  return __equals(hv, hw);
}

export function spcot_in_process<R>(rng: any, delta: any, n: bigint, alpha: bigint): [Vec<Block>, Vec<Block>]
{
  const h = Number(Math.clz32((n) & -((n) | 0)));
  let cot_q = ([] as any[]);
  let cot_r = ([] as any[]);
  let cot_t = ([] as any[]);
  for (let _ = 0n; _ < h; _ += 1n)   {
    const q = sample_block(rng);
    const r = __equals(fieldBitand(rng.next_u32(), 1n), 1n);
    const t = (() => { if (r) {
  return xor_block(q, delta);
} else {
  return q;
} })();
    (cot_q).push(q);
    (cot_r).push(r);
    (cot_t).push(t);
  }
  const choices = spcot_choice_bits(alpha, h, cot_r);
  const [v, msg] = spcot_sender_extend(rng, delta, n, cot_q, choices);
  const w = spcot_receiver_extend(alpha, n, cot_t, msg);
  return [v, w];
}

export function spcot_receiver_extend(alpha: bigint, n: bigint, cot_t: Block[], msg: any): Vec<Block>
{
  const h = Number(Math.clz32((n) & -((n) | 0)));
  let off_sums = ([] as any[]);
  for (let i = 0n; i < h; i += 1n)   {
    const select = bit_msb(alpha, h, i);
    const t = cot_t[Number(i)];
    const tweak = BigInt(i);
    const ht = crhf(t, tweak);
    const m = (() => { if (select) {
  return msg.$fms[Number(i)][Number(0n)];
} else {
  return msg.$fms[Number(i)][Number(1n)];
} })();
    (off_sums).push(xor_block(m, ht));
  }
  let w = expand_partial(h, off_sums, alpha);
  let acc = msg.$fc;
  for (const [i, wi] of w.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    if (!__equals(i, alpha))     {
      acc = xor_block(acc, wi);
    }
  }
  w[Number(alpha)] = acc;
  return w;
}

export function spcot_sender_extend<R>(rng: any, delta: any, n: bigint, cot_q: Block[], choices: boolean[]): [Vec<Block>, SpcotSenderMsg]
{
  const h = Number(Math.clz32((n) & -((n) | 0)));
  const seed = sample_block(rng);
  const [leaves, layer_sums] = expand_full(h, seed);
  let ms = ([] as any[]);
  for (let i = 0n; i < h; i += 1n)   {
    const b = choices[Number(i)];
    const q = cot_q[Number(i)];
    const q_xor_delta = xor_block(q, delta);
    const [k0, k1] = (() => { if (b) {
  return [q_xor_delta, q];
} else {
  return [q, q_xor_delta];
} })();
    const tweak = BigInt(i);
    (ms).push([xor_block(layer_sums[Number(i)][Number(0n)], crhf(k0, tweak)), xor_block(layer_sums[Number(i)][Number(1n)], crhf(k1, tweak))]);
  }
  let c = delta;
  for (const leaf of leaves)   {
    c = xor_block(c, leaf);
  }
  return [leaves, new SpcotSenderMsg({ $fms: ms, $fc: c, $fhash_v: [] as any[] })];
}

export function split_cot_chunks<T>(flat: T[], heights: bigint[]): Vec<Vec<T>>
{
  let out = ([] as any[]);
  let off = 0n;
  for (const h of heights)   {
    (out).push([...flat.slice(Number(off), Number(fieldAdd(off, h)))]);
    off = fieldAdd(off, h);
  }
  return out;
}

export function sqrt_m1(): Fe25519
{
  const E: bigint[] = [18446744073709551611n, 18446744073709551615n, 18446744073709551615n, 2305843009213693951n];
  return fe_pow(new Fe25519([2n, 0n, 0n, 0n]), E);
}

export function ssp_receiver_tag(delta_msg: bigint[], bits: boolean[], v: bigint[][]): bigint[]
{
  let hr = new Sha3_256();
  hr.update(TAG_DOMAIN);
  hr.update(delta_msg);
  for (let j = 0n; j < BigInt(bits.length); j += 1n)   {
    let r0r = Array.from({length: Number(16n)}, () => 0n);
    if (bits[Number(j)])     {
      for (let b = 0n; b < 16n; b += 1n)       {
        r0r[Number(b)] = fieldBitxor(v[Number(j)][Number(b)], delta_msg[Number(b)]);
      }
    } else     {
      r0r = v[Number(j)];
    }
    hr.update(r0r);
  }
  return [...hr.finalize()];
}

export function ssp_sender_tag(delta_msg: bigint[], r0: bigint[][]): bigint[]
{
  let hs = new Sha3_256();
  hs.update(TAG_DOMAIN);
  hs.update(delta_msg);
  for (const row of r0)   {
    hs.update(row);
  }
  return [...hs.finalize()];
}

export function stack_bea95_receiver<R, Io>(rng: any, receiver: any, io: any, bit: boolean): bigint[]
{
  ensure_receiver(rng, receiver, io, 1n);
  const x = (receiver.$fout_x.pop_front())!;
  const z = (receiver.$fout_z.pop_front())!;
  const [_r0, zc, d] = bea95_chosen_bit(Array.from({length: Number(16n)}, () => 0n), Array.from({length: Number(16n)}, () => 0n), x, z, bit);
  const _ = _r0;
  io.send(TAG_BEA95, [((d) & 0xFFn)]);
  return zc;
}

export function stack_bea95_sender<R, Io>(rng: any, sender: any, io: any): bigint[]
{
  ensure_sender(rng, sender, io, 1n);
  const r0 = (sender.$fout.pop_front())!;
  const d = !__equals(io.recv(TAG_BEA95)[Number(0n)], 0n);
  return (() => { if (d) {
  return xor_block(r0, sender.$fseed.$fdelta);
} else {
  return r0;
} })();
}

export function stack_refill_receiver<R, Io>(rng: any, receiver: any, io: any)
{
  const prep = ferret_prepare_receiver(rng, receiver.$fparams, receiver.$fseed);
  io.send(TAG_FERRET_OPEN, encode_ferret_open(prep.$flpn_seed, prep.$fchoices));
  const mpcot = decode_mpcot_reg(io.recv(TAG_FERRET_MPCOT));
  const r = ferret_receiver_mpcot(receiver.$fparams, prep, receiver.$fseed, mpcot);
  const rec = encode_receiver_only(receiver, prep, r);
  receiver.$fseed = rec.$fseed;
  receiver.$fout_x.push(...(rec.$fx));
  receiver.$fout_z.push(...(rec.$fz));
}

export function stack_refill_sender<R, Io>(rng: any, sender: any, io: any)
{
  const raw = io.recv(TAG_FERRET_OPEN);
  const [lpn_seed, choices] = decode_ferret_open(raw);
  const [s, mpcot] = ferret_sender_mpcot(rng, sender.$fparams, sender.$fseed, choices);
  io.send(TAG_FERRET_MPCOT, encode_mpcot_reg(mpcot));
  const out_full = encode_sender_only(sender, lpn_seed, s);
  sender.$fseed.$fq = out_full.$fseed_q;
  sender.$fout.push(...(out_full.$femit));
}

export function stack_setup_receiver<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any): CotPoolReceiver
{
  const m = params.seed_cot_count(false);
  const delta_raw = io.recv(TAG_DELTA);
  let delta_msg = Array.from({length: Number(16n)}, () => 0n);
  (delta_msg).splice(0, (delta_raw).length, ...(delta_raw));
  let bits = [] as any[];
  for (const b of bits)   {
    b = __equals(fieldBitand(rng.next_u32(), 1n), 1n);
  }
  let seeds_0 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n));
  let seeds_1 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    seeds_0[Number(i)] = sample_bytes(rng);
    seeds_1[Number(i)] = sample_bytes(rng);
  }
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    const [s_state, setup] = sender_setup(rng);
    io.send(TAG_LWE_SETUP, encode_lwe_crs(setup));
    const recv_msg = decode_lwe_recv(io.recv(TAG_LWE_RECV));
    const payload = sender_payload(rng, s_state, recv_msg, seeds_0[Number(i)], seeds_1[Number(i)]);
    io.send(TAG_LWE_PAYLOAD, encode_lwe_payload(payload));
  }
  const [t_cols, u_msg] = iknp_receiver_u_cols(ctx, m, bits, seeds_0, seeds_1);
  io.send(TAG_IKNP_U, encode_iknp_u(u_msg));
  const corrections = decode_iknp_corr(io.recv(TAG_IKNP_CORR));
  const receiver_v = iknp_receiver_finish(ctx, bits, t_cols, corrections);
  const tag_s = io.recv(TAG_SSP_S);
  const tag_r = ssp_receiver_tag(delta_msg, bits, receiver_v);
  io.send(TAG_SSP_R, tag_r);
  let w = ([] as any[]);
  for (const row of receiver_v)   {
    (w).push(row);
  }
  let receiver = new CotPoolReceiver({ $fparams: params, $fseed: new FerretReceiverSeed({ $fu: bits, $fw: w }), $fout_x: [] as any[], $fout_z: [] as any[] });
  stack_refill_receiver(rng, receiver, io);
  return receiver;
}

export function stack_setup_sender<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any): CotPoolSender
{
  const m = params.seed_cot_count(false);
  const delta_msg = sample_bytes(rng);
  io.send(TAG_DELTA, delta_msg);
  let delta_ot = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    delta_ot[Number(i)] = __equals(fieldBitand(rng.next_u32(), 1n), 1n);
  }
  const delta_ot_bytes = pack_kappa(delta_ot);
  let chosen_seeds = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    const setup = decode_lwe_crs(io.recv(TAG_LWE_SETUP));
    const [r_state, recv_msg] = recv_start(rng, setup, delta_ot[Number(i)]);
    io.send(TAG_LWE_RECV, encode_lwe_recv(recv_msg));
    const payload = decode_lwe_payload(io.recv(TAG_LWE_PAYLOAD));
    chosen_seeds[Number(i)] = recv_finish(r_state, payload);
  }
  const u_msg = decode_iknp_u(io.recv(TAG_IKNP_U));
  const [sender_r0, corrections] = iknp_sender_from_u(ctx, m, delta_msg, delta_ot, delta_ot_bytes, chosen_seeds, u_msg);
  io.send(TAG_IKNP_CORR, encode_iknp_corr(corrections));
  const tag_s = ssp_sender_tag(delta_msg, sender_r0);
  io.send(TAG_SSP_S, tag_s);
  const tag_r = io.recv(TAG_SSP_R);
  let q = ([] as any[]);
  for (const row of sender_r0)   {
    (q).push(row);
  }
  let sender = new CotPoolSender({ $fparams: params, $fseed: new FerretSenderSeed({ $fdelta: delta_msg, $fq: q }), $fout: [] as any[], $fraise_n: undefined });
  stack_refill_sender(rng, sender, io);
  return sender;
}

export function stack_uses_lwe_base(n: bigint, l: bigint): boolean
{
  return (ctx.sizeOf_unknown > 0n);
}

export function sub_bytes(state: bigint[])
{
  for (let i = 0n; i < BLOCK; i += 1n)   {
    state[Number(i)] = SBOX[Number(Number(state[Number(i)]))];
  }
}

export function sub_mod(left: bigint, right: bigint, modulus: bigint): bigint
{
  return (() => { if ((left >= right)) {
  return fieldSub(left, right);
} else {
  return fieldSub(modulus, fieldSub(right, left));
} })();
}

export function table_is_constant(logical: boolean[]): boolean
{
  let i = 1n;
  while ((i < BigInt(logical.length)))   {
    if (!__equals(logical[Number(i)], logical[Number(0n)]))     {
      return false;
    }
    i = fieldAdd(i, 1n);
  }
  return true;
}

export function take_block(bytes: bigint[], off: bigint): Block
{
  let b = Array.from({length: Number(16n)}, () => 0n);
  (b).splice(0, (bytes.slice(Number(off), Number(fieldAdd(off, 16n)))).length, ...(bytes.slice(Number(off), Number(fieldAdd(off, 16n)))));
  off = fieldAdd(off, 16n);
  return b;
}

export function take_random<R>(rng: any, sender: any, receiver: any, need: bigint): [Vec<Block>, Vec<boolean>, Vec<Block>]
{
  ensure(rng, sender, receiver, need);
  let r0 = ([] as any[]);
  let x = ([] as any[]);
  let z = ([] as any[]);
  for (let _ = 0n; _ < need; _ += 1n)   {
    (r0).push((sender.$fout.pop_front())!);
    (x).push((receiver.$fout_x.pop_front())!);
    (z).push((receiver.$fout_z.pop_front())!);
  }
  return [r0, x, z];
}

export function take_u32(bytes: bigint[], off: bigint): bigint
{
  const x = u32_from_le_bytes((bytes.slice(Number(off), Number(fieldAdd(off, 4n))))!);
  off = fieldAdd(off, 4n);
  return x;
}

export function tfhe_cmux(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, sel: LweCiphertextDyn, a: LweCiphertextDyn, b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const not_sel = tfhe_not(n_lwe, sel);
  const sel_and_a = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, sel, a, bk);
  const nsel_and_b = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, not_sel, b, bk);
  return tfhe_gate_bootstrapping_or(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, sel_and_a, nsel_and_b, bk);
}

export function tfhe_gate_bootstrapping_and(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, ct_a: LweCiphertextDyn, ct_b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  let ct = lwe_add(n_lwe, ct_a, ct_b);
  ct.$fb = wrappingSub(ct.$fb, fieldShr(Q4, 1n), 32);
  const acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, ct, bk);
  const lwe_big = sample_extract(big_n, acc);
  let ct_out = key_switch(n_lwe, big_n, ks_ell, ks_bg_log, lwe_big, bk.$fksk);
  ct_out.$fb = wrappingAdd(ct_out.$fb, fieldShr(Q4, 1n), 32);
  return ct_out;
}

export function tfhe_gate_bootstrapping_or(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, ct_a: LweCiphertextDyn, ct_b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  let ct = lwe_add(n_lwe, ct_a, ct_b);
  ct.$fb = wrappingAdd(ct.$fb, fieldShr(Q4, 1n), 32);
  const acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, ct, bk);
  const lwe_big = sample_extract(big_n, acc);
  let ct_out = key_switch(n_lwe, big_n, ks_ell, ks_bg_log, lwe_big, bk.$fksk);
  ct_out.$fb = wrappingAdd(ct_out.$fb, fieldShr(Q4, 1n), 32);
  return ct_out;
}

export function tfhe_lut_read(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, addr_bits: bigint, table_len: bigint, encrypted_addr_bits: LweCiphertextDyn[], table: TfheBootstrapTableDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  if (table.$fis_constant)   {
    return tfhe_trivial_encrypt(n_lwe, table.$flogical[Number(0n)]);
  }
  const delta = (fieldShl(1n, 32n) / BigInt(table_len));
  let combined = new LweCiphertextDyn({ $fa: Array.from({length: Number(n_lwe)}, () => 0n), $fb: 0n, $fn_lwe: 0n });
  for (let j = 0n; j < addr_bits; j += 1n)   {
    const addr_ct = encrypted_addr_bits[Number(j)];
    const target = fieldMul(fieldShl(1n, j), delta);
    for (let i = 0n; i < n_lwe; i += 1n)     {
      const scaled = (BigInt(Math.imul(Number(BigInt(addr_ct.$fa[Number(i)])), Number(target))) / BigInt(Q4));
      combined.$fa[Number(i)] = wrappingAdd(combined.$fa[Number(i)], Number(scaled), 32);
    }
    const scaled_b = (BigInt(Math.imul(Number(BigInt(addr_ct.$fb)), Number(target))) / BigInt(Q4));
    combined.$fb = wrappingAdd(combined.$fb, Number(scaled_b), 32);
  }
  combined.$fb = wrappingAdd(combined.$fb, Number((delta / 2n)), 32);
  let ct_out = tfhe_programmable_bootstrap(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, combined, table.$ftest_poly, bk);
  ct_out.$fb = wrappingAdd(ct_out.$fb, fieldShr(Q4, 1n), 32);
  return ct_out;
}

export function tfhe_not(n_lwe: bigint, a: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let i = 0n; i < n_lwe; i += 1n)   {
    out_a[Number(i)] = wrappingNeg(a.$fa[Number(i)], 32);
  }
  return new LweCiphertextDyn({ $fa: out_a, $fb: wrappingSub(Q4, a.$fb, 32), $fn_lwe: 0n });
}

export function tfhe_programmable_bootstrap(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, ct: LweCiphertextDyn, test_poly: bigint[], bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const acc = blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, ct, test_poly, bk);
  const lwe_big = sample_extract(big_n, acc);
  return key_switch(n_lwe, big_n, ks_ell, ks_bg_log, lwe_big, bk.$fksk);
}

export function tfhe_trivial_encrypt(n_lwe: bigint, b: boolean): LweCiphertextDyn
{
  return (() => { if (b) {
  return tfhe_trivial_one(n_lwe);
} else {
  return tfhe_trivial_zero(n_lwe);
} })();
}

export function tfhe_trivial_one(n_lwe: bigint): LweCiphertextDyn
{
  return new LweCiphertextDyn({ $fa: Array.from({length: Number(n_lwe)}, () => 0n), $fb: Q4, $fn_lwe: 0n });
}

export function tfhe_trivial_zero(n_lwe: bigint): LweCiphertextDyn
{
  return new LweCiphertextDyn({ $fa: Array.from({length: Number(n_lwe)}, () => 0n), $fb: 0n, $fn_lwe: 0n });
}

export function to_bool(phase: bigint, q: bigint): boolean
{
  return __equals(phase, (q / 4n));
}

export function torus_add(log: bigint, a: bigint, b: bigint): bigint
{
  return reduce(log, wrappingAdd(a, b, 32));
}

export function torus_neg(log: bigint, a: bigint): bigint
{
  return reduce(log, wrappingNeg(a, 32));
}

export function torus_sub(log: bigint, a: bigint, b: bigint): bigint
{
  return reduce(log, wrappingSub(a, b, 32));
}

export function torus_to_exp(x: bigint, scale_shift: bigint, two_n: bigint): bigint
{
  const half = (() => { if ((scale_shift > 0n)) {
  return fieldShl(1n, fieldSub(scale_shift, 1n));
} else {
  return 0n;
} })();
  const exp = Number(fieldShr(wrappingAdd(x, half, 32), scale_shift));
  return fieldBitand(exp, fieldSub(two_n, 1n));
}

export function toy_mul(a: bigint, b: bigint): bigint
{
  return (fieldMul(a, b) % TOY_P);
}

export function toy_pow(base: bigint, exp: bigint): bigint
{
  let acc: bigint = 1n;
  let b = (base % TOY_P);
  while ((exp > 0n))   {
    if (__equals(fieldBitand(exp, 1n), 1n))     {
      acc = toy_mul(acc, b);
    }
    b = toy_mul(b, b);
    exp = fieldShr(exp, 1n);
  }
  return acc;
}

export function true_val(q: bigint): bigint
{
  return (q / 4n);
}

export function uni_seed_cot_count(hash_seed: bigint[], params: any): bigint
{
  return fieldAdd(params.$fk, uni_spcot_heights(hash_seed, params.$fn, params.$ft).sum());
}

export function uni_spcot_heights(hash_seed: bigint[], n: bigint, t: bigint): Vec<bigint>
{
  const m = cuckoo_table_size(t);
  const buckets = build_buckets(hash_seed, n, m);
  return buckets.map((b: any) => Number(Math.clz32((next_pow2(fieldAdd(BigInt(b.length), 1n))) & -((next_pow2(fieldAdd(BigInt(b.length), 1n))) | 0))));
}

export function unique_bins(seed: bigint[], x: bigint, m: bigint): Vec<bigint>
{
  let js = ([] as any[]);
  for (let i = 0n; i < TAU; i += 1n)   {
    const j = hash_i(seed, i, x, m);
    if (!js.includes(j))     {
      (js).push(j);
    }
  }
  return js;
}

export function verify(ctx: { newD: () => any, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, pk: any, message: bigint[], sig: any): boolean
{
  const iv = sig.$fiv;
  const mu: Vec<bigint> = (() => {
  let h = new Sha3_256();
  h.update(pk[0]);
  h.update(message);
  return [...h.finalize()];
})();
  const chall_1 = chall1(mu, iv, sig.$fbavc_root, fieldAdd(LAMBDA_BYTES, 8n), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const reconstructed_seeds_opt = BavcDyn.reconstruct(ctx, sig.$fnodes, sig.$fhidden_commits, deltas, iv, sig.$fbavc_root, TAU, SUB_VOLE_N);
  const reconstructed_seeds = (() => { const __match = reconstructed_seeds_opt; if (__match !== null && __match !== undefined) { const s = __match;
return s; } else { return false; } })();
  let sub_voles_v = ([] as any[]);
  for (let i = 0n; i < TAU; i += 1n)   {
    const d = deltas[Number(i)];
    const verifier_seeds: Vec<(bigint[] | undefined)> = Array.from({length: Number(SUB_VOLE_N - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return (() => { if (__equals(j, 0n)) {
  return undefined;
} else {
  return reconstructed_seeds[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), fieldBitxor(j, d)))];
} })();
})());
    (sub_voles_v).push(convert_to_vole(verifier_seeds, iv, Number(i), L_HAT_BYTES));
  }
  const corrections = sig.$fcorrections;
  if (!__equals(BigInt(corrections.length), fieldSub(TAU, 1n)))   {
    return false;
  }
  const big_q: Vec<bigint> = (() => {
  const q_out = concat_small_voles_verifier(sub_voles_v, deltas, corrections);
  return q_out.$fq_columns.flat();
})();
  const corrections_flat: Vec<bigint> = sig.$fcorrections.flat();
  const chall_2 = chall2(chall_1, sig.$fvole_u, corrections_flat, fieldAdd(LAMBDA_BYTES, 8n), false);
  const hash_key = hash_key_from_chall(chall_2);
  const derived_chall_3 = chall3(chall_2, sig.$fqs_proof.$fa_hat, sig.$fqs_proof.$fb_hat, sig.$fc_hat_with_counter, LAMBDA_BYTES, false);
  if (!__equals(derived_chall_3, sig.$fchall_3))   {
    return false;
  }
  if (!has_trailing_zero_bits(sig.$fchall_3, W_GRIND))   {
    return false;
  }
  const _ = hash_key;
  return true;
}

export function vole_and_prover_step<T>(n: bigint, vope_a: VopeDyn<T>, vope_b: VopeDyn<T>): [VopeDyn<T>, T[]]
{
  const u_c_inner = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(vope_a.$fu[Number(0n)][Number(i)]), __clone(vope_b.$fu[Number(0n)][Number(i)])));
  const u_c = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u_c_inner));
  const v_c = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(fieldMul(__clone(vope_a.$fv[Number(i)]), __clone(vope_b.$fu[Number(0n)][Number(i)])), fieldMul(__clone(vope_b.$fv[Number(i)]), __clone(vope_a.$fu[Number(0n)][Number(i)])));
})());
  const hat = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(vope_a.$fv[Number(i)]), __clone(vope_b.$fv[Number(i)])));
  return [new VopeDyn({ $fu: u_c, $fv: v_c, $fn: 0n, $fk: 1n }), hat];
}

export function vole_and_verifier_check<T>(n: bigint, delta: DeltaDyn<T>, q_a: QDyn<T>, q_b: QDyn<T>, q_and: QDyn<T>, hat: T[]): [QDyn<T>, boolean]
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const lhs = fieldAdd(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), __clone(hat[Number(i)]));
    const rhs = fieldMul(__clone(q_and.$fq[Number(i)]), __clone(delta.$fdelta[Number(i)]));
    ok = (ok && __equals(lhs, rhs));
  }
  return [new QDyn({ $fq: __clone(q_and.$fq), $fn: 0n }), ok];
}

export function vole_commit_bit<T, R>(n: bigint, cot: IdealCotDyn<T>, rng: any, sample_t: any, bit_to_t: (arg: boolean) => T, bit: boolean): [VopeDyn<T>, QDyn<T>]
{
  const [r0, v] = cot.cot(rng, sample_t, bit);
  const u_t = bit_to_t(bit);
  const u_row: T[] = lift_bit(n, u_t);
  const u: T[][] = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(u_row[Number(i)])));
  const q = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(r0[Number(i)]));
  return [new VopeDyn({ $fu: u, $fv: v, $fn: 0n, $fk: 1n }), new QDyn({ $fq: q, $fn: 0n })];
}

export function vole_commit_bit_from<T, R, C>(n: bigint, cot: any, rng: any, sample_t: any, bit_to_t: (arg: boolean) => T, bit: boolean): [VopeDyn<T>, QDyn<T>]
{
  const [r0, v] = cot.cot(rng, sample_t, bit);
  return vole_commit_bit_shares(n, r0, v, bit_to_t, bit);
}

export function vole_commit_bit_shares<T>(n: bigint, r0: T[], v: T[], bit_to_t: (arg: boolean) => T, bit: boolean): [VopeDyn<T>, QDyn<T>]
{
  const u_t = bit_to_t(bit);
  const u_row: T[] = lift_bit(n, u_t);
  const u: T[][] = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(u_row[Number(i)])));
  const q = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(r0[Number(i)]));
  return [new VopeDyn({ $fu: u, $fv: v, $fn: 0n, $fk: 1n }), new QDyn({ $fq: q, $fn: 0n })];
}

export function vole_hash(key: any, input: bigint[]): UniversalHashOutput
{
  const n_full = (BigInt(input.length) / 16n);
  const tail = input.slice(Number(fieldMul(n_full, 16n)));
  let h0 = new Galois128(0n);
  let h1 = new Galois64(0n);
  let pow0 = key.$fr0;
  let pow1 = key.$fr1;
  for (let i = 0n; i < n_full; i += 1n)   {
    const block = input.slice(Number(fieldMul(i, 16n)), Number(fieldMul(fieldAdd(i, 1n), 16n)));
    let bytes = Array.from({length: Number(16n)}, () => 0n);
    (bytes).splice(0, (block).length, ...(block));
    const s = new Galois128(u128_from_le_bytes(bytes));
    h0 = fieldAdd(h0, fieldMul(s, pow0));
    const s64 = new Galois64(BigInt(s[0]));
    h1 = fieldAdd(h1, fieldMul(s64, pow1));
    pow0 = fieldMul(pow0, key.$fr0);
    pow1 = fieldMul(pow1, key.$fr1);
  }
  if (!(tail.length === 0))   {
    let bytes = Array.from({length: Number(8n)}, () => 0n);
    const n = BigInt(Math.min(Number(BigInt(tail.length)), Number(8n)));
    (bytes.slice(0, Number(n))).splice(0, (tail.slice(0, Number(n))).length, ...(tail.slice(0, Number(n))));
    const t = new Galois64(u64_from_le_bytes(bytes));
    h1 = fieldAdd(h1, fieldMul(t, pow1));
  }
  return new UniversalHashOutput({ $fh0: h0, $fh1: h1 });
}

export function vole_hash_consistency_check(key: any, hu: any, hq: any, hv: any, hc: any, delta: Galois128): boolean
{
  const lhs0 = hq.$fh0;
  const rhs0 = fieldAdd(hv.$fh0, fieldMul(delta, fieldAdd(hu.$fh0, hc.$fh0)));
  const delta64 = new Galois64(BigInt(delta[0]));
  const lhs1 = hq.$fh1;
  const rhs1 = fieldAdd(hv.$fh1, fieldMul(delta64, fieldAdd(hu.$fh1, hc.$fh1)));
  return (__equals(lhs0, rhs0) && __equals(lhs1, rhs1));
}

export function vole_mul3_prover_step<T>(ctx: { defaultT: () => any }, n: bigint, vope_a: VopeDyn<T>, vope_b: VopeDyn<T>, vope_d: VopeDyn<T>): VopeDyn<T>
{
  const ab: VopeDyn<T> = vope_a.mul_generalized(ctx, vope_b.$fk, vope_b);
  return ab.mul_generalized({ defaultT: () => __zeroValue(((recv: any) => recv.$fu?.[0] ?? 0n)(ab)) }, vope_d.$fk, vope_d);
}

export function vole_mul3_verifier_check<T>(n: bigint, delta: DeltaDyn<T>, q_a: QDyn<T>, q_b: QDyn<T>, q_d: QDyn<T>, vope_abd: VopeDyn<T>): [QDyn<T>, boolean]
{
  const q_abd = fieldMul(vope_abd, __clone(delta));
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const lhs = fieldMul(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), __clone(q_d.$fq[Number(i)]));
    ok = (ok && __equals(lhs, q_abd.$fq[Number(i)]));
  }
  return [q_abd, ok];
}

export function vole_rekey_prover<T>(n: bigint, wire: VopeDyn<T>, key: VopeDyn<T>): VopeDyn<T>
{
  return fieldAdd(wire, key);
}

export function vole_rekey_verifier_check<T>(n: bigint, q_wire: QDyn<T>, q_key: QDyn<T>, q_rekeyed: QDyn<T>): boolean
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const expect = fieldAdd(__clone(q_wire.$fq[Number(i)]), __clone(q_key.$fq[Number(i)]));
    ok = (ok && __equals(__clone(q_rekeyed.$fq[Number(i)]), expect));
  }
  return ok;
}

export function vole_sbox_prover_step<T>(ctx: { defaultT: () => any }, n: bigint, vope_a: VopeDyn<T>, vope_b: VopeDyn<T>): [VopeDyn<T>, VopeDyn<T>]
{
  const k2: VopeDyn<T> = vope_a.mul_generalized(ctx, vope_b.$fk, vope_b);
  const k1 = new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(k2.$fu[Number(1n)])), $fv: __clone(k2.$fu[Number(0n)]), $fn: 0n, $fk: 1n });
  return [k1, k2];
}

export function vole_sbox_verifier_check<T>(n: bigint, delta: DeltaDyn<T>, q_a: QDyn<T>, q_b: QDyn<T>, vope_k2: VopeDyn<T>): [QDyn<T>, boolean]
{
  const q_c = fieldMul(vope_k2, __clone(delta));
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    ok = (ok && __equals(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), q_c.$fq[Number(i)]));
  }
  return [q_c, ok];
}

export function vope_bitpack<T>(ctx: { defaultT: () => any }, bits: bigint, n: bigint, bit_values: VopeDyn<T>[], pow2: T[]): VopeDyn<T>
{
  let acc = new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT())), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT()), $fn: 0n, $fk: 1n });
  for (let index = 0n; index < bits; index += 1n)   {
    acc = fieldAdd(acc, vope_scale_const(n, bit_values[Number(index)], pow2[Number(index)]));
  }
  return acc;
}

export function vope_open_mask<T>(n: bigint, w: VopeDyn<T>): T[]
{
  return __clone(w.$fv);
}

export function vope_scale_const<T>(n: bigint, w: VopeDyn<T>, c: any): VopeDyn<T>
{
  return new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(w.$fu[Number(0n)][Number(i)]), __clone(c)));
})()), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(w.$fv[Number(i)]), __clone(c))), $fn: 0n, $fk: 1n });
}

export function wire_delta(log_q_lwe: bigint, k_max: bigint): bigint
{
  return fieldShl(1n, fieldSub(fieldSub(log_q_lwe, 1n), Number(k_max)));
}

export function xor_block(...__args: any[]): any {
  if (__args.length === 2) {
    const a = __args[0];
    const b = __args[1];
    return (() => {
  let o = Array.from({length: Number(16n)}, () => 0n);
  for (let i = 0n; i < 16n; i += 1n)   {
    o[Number(i)] = fieldBitxor(a[Number(i)], b[Number(i)]);
  }
  return o;
})();
  } else
  if (__args.length === 2) {
    const a = __args[0];
    const b = __args[1];
    return (() => {
  let o = Array.from({length: Number(KAPPA_BYTES)}, () => 0n);
  for (let i = 0n; i < KAPPA_BYTES; i += 1n)   {
    o[Number(i)] = fieldBitxor(a[Number(i)], b[Number(i)]);
  }
  return o;
})();
  } else
  if (__args.length === 2) {
    const a = __args[0];
    const b = __args[1];
    return (() => {
  let o = Array.from({length: Number(16n)}, () => 0n);
  for (let i = 0n; i < 16n; i += 1n)   {
    o[Number(i)] = fieldBitxor(a[Number(i)], b[Number(i)]);
  }
  return o;
})();
  } else
  if (__args.length === 2) {
    const a = __args[0];
    const b = __args[1];
    return (() => {
  let o = Array.from({length: Number(16n)}, () => 0n);
  for (let i = 0n; i < 16n; i += 1n)   {
    o[Number(i)] = fieldBitxor(a[Number(i)], b[Number(i)]);
  }
  return o;
})();
  }
  throw new Error("xor_block(): no matching variant for " + __args.length + " args");
}

export function xor_in_place(a: bigint[], b: bigint[])
{
  for (let i = 0n; i < BigInt(a.length); i += 1n)   {
    a[Number(i)] = fieldBitxor(a[Number(i)], b[Number(i)]);
  }
}

export function zk_hash(key: any, elements: Galois128[]): UniversalHashOutput
{
  let h0 = new Galois128(0n);
  let h1 = new Galois64(0n);
  let pow0 = key.$fr0;
  let pow1 = key.$fr1;
  for (const x of elements)   {
    h0 = fieldAdd(h0, fieldMul(x, pow0));
    const x64 = new Galois64(BigInt(x[0]));
    h1 = fieldAdd(h1, fieldMul(x64, pow1));
    pow0 = fieldMul(pow0, key.$fr0);
    pow1 = fieldMul(pow1, key.$fr1);
  }
  return new UniversalHashOutput({ $fh0: h0, $fh1: h1 });
}

export function zq_add(a: any, b: any): Zq
{
  return fieldBitand(wrappingAdd(a, b, 32), LWE_Q_MASK);
}

export function zq_mul(a: any, b: any): Zq
{
  return fieldBitand(BigInt(Math.imul(Number(a), Number(b))), LWE_Q_MASK);
}

export function zq_neg(a: any): Zq
{
  return fieldBitand(wrappingSub(LWE_Q, a, 32), LWE_Q_MASK);
}

export function zq_sub(a: any, b: any): Zq
{
  return fieldBitand(wrappingSub(a, b, 32), LWE_Q_MASK);
}

export function absorb(data: bigint[])
{
  return (() => { const __match = this; if (__match instanceof Sponge_Shake128) { const h = __match._0;
return h.update(data); } else { const h = __match._0;
return h.update(data); } })();
}

export function party_index(...__args: any[]): any {
  if (__args.length === 2) {
    const n = __args[0];
    const requested = __args[1];
    return (() => {
  return requested;
})();
  } else
  if (__args.length === 1) {
    const _ = __args[0];
    return (() => {
  return 0n;
})();
  }
  throw new Error("party_index(): no matching variant for " + __args.length + " args");
}

export function squeeze(n: bigint): Vec<bigint>
{
  let out = [] as any[];
  (() => { const __match = this; if (__match instanceof Sponge_Shake128) { const h = __match._0;
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

export function try_from(value: bigint): Result<any, FrameError>
{
  return (() => { const __match = value; if (__match === 1n) { return PublicParameters; } else if (__match === 2n) { return ReusableCiphertext; } else if (__match === 3n) { return PerUseCiphertext; } else if (__match === 4n) { return SelectionKey; } else if (__match === 5n) { return Complete; } else if (__match === 6n) { return Error; } else { return new FrameError_UnknownStage(); } })();
}

