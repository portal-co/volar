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
  asRefU8,
  u32_from_le_bytes,
  u64_from_le_bytes,
  u128_from_le_bytes,
} from "./index";

type Shake128 = any; type Shake256 = any; type Sha3_256 = any;
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

export type Zq = bigint;

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

  validate(ctx: { newD: () => any }, opened_message: bigint[], opened_rand: bigint[]): boolean
  {
    const recomputed: CommitmentCoreDyn<D> = commit(ctx, opened_message, opened_rand);
    return __equals(recomputed[0], this[0]);
  }
}

export class NoReduction {

  constructor(init: { 
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  reduce(_word: GrafhenWordDyn)
  {
  }
}

export class GrafhenWordDyn {
  $fwbound!: bigint;
  $fdata!: bigint[];
  $flen!: bigint;

  constructor(init: { 
    $fwbound: bigint,
    $fdata: bigint[],
    $flen: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fwbound: __zeroValue(this.$fwbound), $fdata: __zeroValue(this.$fdata), $flen: __zeroValue(this.$flen) }) as this;
  }

  static identity(wbound: bigint): GrafhenWordDyn
  {
    return new GrafhenWordDyn({ $fdata: Array.from({length: Number(wbound)}, () => 0n), $flen: 0n, $fwbound: 0n });
  }
}

export class GrafhenKeyDyn {
  $fn!: bigint;
  $fd!: bigint;
  $fgens!: bigint[][];
  $finv_gens!: bigint[][];

  constructor(init: { 
    $fn: bigint,
    $fd: bigint,
    $fgens: bigint[][],
    $finv_gens: bigint[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fd: __zeroValue(this.$fd), $fgens: __zeroValue(this.$fgens), $finv_gens: __zeroValue(this.$finv_gens) }) as this;
  }
}

export class GrafhenPublicDyn<R> {
  $fwbound!: bigint;
  $fenc_one!: GrafhenWordDyn;
  $fand_w1!: GrafhenWordDyn;
  $fand_w2!: GrafhenWordDyn;
  $freducer!: R;

  constructor(init: { 
    $fwbound: bigint,
    $fenc_one: GrafhenWordDyn,
    $fand_w1: GrafhenWordDyn,
    $fand_w2: GrafhenWordDyn,
    $freducer: R
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fwbound: __zeroValue(this.$fwbound), $fenc_one: __zeroValue(this.$fenc_one), $fand_w1: __zeroValue(this.$fand_w1), $fand_w2: __zeroValue(this.$fand_w2), $freducer: __zeroValue(this.$freducer) }) as this;
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
    return this.remap(this.$fn, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a) => wrappingAdd(a, n_param));
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
    return this.remap(this.$fn, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a) => wrappingAdd(a, n_param));
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
    return new FaestTranscript({ $fsponge: Sponge_Shake128(Shake128.default()) });
  }

  static new_shake256(): FaestTranscript
  {
    return new FaestTranscript({ $fsponge: Sponge_Shake256(Shake256.default()) });
  }

  squeeze(n: bigint): Vec<bigint>
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
    let hidden = [];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    for (const node of (Array.from({length: Number(fieldSub(leaf_count, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      hidden[Number(node)] = (hidden[Number(fieldAdd(fieldMul(2n, node), 1n))] || hidden[Number(fieldAdd(fieldMul(2n, node), 2n))]);
    }
    let out: Vec<[bigint, bigint[]]> = [];
    walk(0n, hidden, tree, leaf_count, out);
    return out;
  }

  static commit(ctx: { newD: () => any }, com_bytes: bigint, r: bigint[], iv: bigint[], tau: bigint, n: bigint): BavcCommitmentDyn
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let tree: Vec<bigint[]> = [];
    tree[Number(0n)] = r;
    for (let node = 0n; node < fieldSub(leaf_count, 1n); node += 1n)     {
      const parent = tree[Number(node)];
      const [left, right] = AesCtrLengthDoubler.double(parent);
      tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left[0];
      tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right[0];
    }
    let seeds = /* Vec::with_capacity */ Array(leaf_count);
    let commitments = /* Vec::with_capacity */ Array(leaf_count);
    for (let i = 0n; i < tau; i += 1n)     {
      for (let j = 0n; j < n; j += 1n)       {
        const leaf_k = fieldAdd(fieldMul(i, n), j);
        const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
        const r_leaf = tree[Number(tree_pos)];
        const tweak = Number(leaf_k);
        const [sd, com] = commit(ctx, r_leaf, iv, tweak);
        (seeds).push(sd);
        (commitments).push(com);
      }
    }
    let vec_hashes: Vec<Vec<bigint>> = /* Vec::with_capacity */ Array(tau);
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
    let hidden = [];
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
    let tree: Vec<bigint[]> = [];
    const _ = tree;
    return new BavcOpeningDyn({ $fhidden_commits: deltas.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([i, d]: any) => commitment.$fcommitments[Number(fieldAdd(fieldMul(i, n), d))]), $fnodes: [], $fcom_bytes: 0n });
  }

  static reconstruct(ctx: { newD: () => any }, com_bytes: bigint, nodes: [bigint, bigint[]][], hidden_commits: bigint[][], deltas: bigint[], iv: bigint[], expected_root: bigint[], tau: bigint, n: bigint): (Vec<bigint[]> | undefined)
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let hidden = [];
    for (const [i, d] of deltas.map((val: any, i: number) => [i, val] as [number, typeof val]))     {
      const leaf_k = fieldAdd(fieldMul(i, n), d);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    for (const node of (Array.from({length: Number(fieldSub(leaf_count, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      hidden[Number(node)] = (hidden[Number(fieldAdd(fieldMul(2n, node), 1n))] || hidden[Number(fieldAdd(fieldMul(2n, node), 2n))]);
    }
    let tree: Vec<(bigint[] | undefined)> = [];
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
    let leaf_seeds: Vec<bigint[]> = /* Vec::with_capacity */ Array(leaf_count);
    let leaf_coms: Vec<bigint[]> = /* Vec::with_capacity */ Array(leaf_count);
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
          const [sd, com] = commit(ctx, r_leaf, iv, tweak);
          (leaf_seeds).push(sd);
          (leaf_coms).push(com);
        }
      }
    }
    let vec_hashes: Vec<Vec<bigint>> = /* Vec::with_capacity */ Array(tau);
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
    return new QuickSilverProof({ $fa_hat: a_hat, $fb_hat: [], $fc_hat_base: [] });
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

  open(ctx: { newD: () => any }, t: bigint, u: bigint, m: bigint, bad: bigint[], rand: bigint[])
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
  return Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (asRefU8(h)?.[j] ?? 0));
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

  validate(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any }, commit_: bigint[], rand: bigint[]): boolean
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

  drain(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, final_value: any, final_timestamp: bigint)
  {
    const enc = this.encode(addr, final_value, final_timestamp);
    ctx.HClass.absorb(this.$fconsume, enc);
  }

  encode(addr: any, value: any, timestamp: bigint)
  {
    const a = fieldMul(addr, __clone(this.$fkey.$fr1));
    const v = fieldMul(value, __clone(this.$fkey.$fr2));
    const t = this.scale_by_u64(__clone(this.$fkey.$fr3), timestamp);
    return fieldAdd(fieldAdd(a, v), t);
  }

  init(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, zero_value: any)
  {
    const enc = this.encode(addr, zero_value, 0n);
    ctx.HClass.absorb(this.$fproduce, enc);
  }

  static new<T>(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, key: ChallengeKeyDyn<T>): MemoryCheckStateDyn<T, H>
  {
    return new MemoryCheckStateDyn({ $fkey: key, $fproduce: ctx.HClass.new_state(), $fconsume: ctx.HClass.new_state() });
  }

  read(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, value: any, timestamp: bigint, write_timestamp: bigint)
  {
    const enc_produce = this.encode(__clone(addr), __clone(value), timestamp);
    ctx.HClass.absorb(this.$fproduce, enc_produce);
    const enc_consume = this.encode(addr, value, write_timestamp);
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
    for (let bit = 0n; bit < 64n; bit += 1n)     {
      if (__equals(fieldBitand(fieldShr(n, bit), 1n), 1n))       {
        acc = fieldAdd(acc, __clone(base));
      }
      base = fieldAdd(__clone(base), base);
    }
    return acc;
  }

  verify(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }): boolean
  {
    return ctx.HClass.finalize_eq(this.$fproduce, this.$fconsume);
  }

  write(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, addr: any, new_value: any, timestamp: bigint, old_value: any, old_timestamp: bigint)
  {
    const enc_new = this.encode(__clone(addr), new_value, timestamp);
    ctx.HClass.absorb(this.$fproduce, enc_new);
    const enc_old = this.encode(addr, old_value, old_timestamp);
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
    let res_u = Array.from({length: Number(fieldAdd(k2, k) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => []);
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
    return this.remap(this.$fn, (a) => wrappingSub(a, n_param));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return this.remap(this.$fn, (a) => wrappingAdd(a, n_param));
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

  static sample<S, P, T, U>(ctx: { defaultA: () => any }, n: bigint, m: bigint, matrix: T[][], s: any[], e: any[]): LweSampleDyn<T, U>
  {
    return new LweSampleDyn<T, U>({ $fb: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(s.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([a, b]: any) => fieldMul(__clone(b), __clone(matrix[Number(i)][Number(a)]))).reduce((a: any, b: any) => fieldAdd(a, b), ctx.defaultA()), __clone(e[Number(i)]));
})()), $fmatrix: matrix });
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

  gen_and_table(ctx: { newD: () => any }, a: GarbleDyn, b: GarbleDyn): GarbleTableDyn
  {
    const n: bigint = this.$fn;
    const result_base = a.and_result(b);
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

  one_wire_eval(): EvalDyn
  {
    const n: bigint = this.$fn;
    return this.encode(Garble.zero(), true);
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

  encode_inputs(bits: boolean[]): EvalDyn[]
  {
    const n: bigint = this.$fn;
    const i: bigint = this.$fi;
    const a: bigint = this.$fa;
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => this.$fsecret.encode(this.$finput_labels[Number(i)], bits[Number(i)]));
  }

  eval_setup(): EvalSetupDyn
  {
    const n: bigint = this.$fn;
    const i: bigint = this.$fi;
    const a: bigint = this.$fa;
    return new EvalSetupDyn({ $fone_wire: this.$fsecret.one_wire_eval(), $ftables: __clone(this.$ftables), $foutput_label: __clone(this.$foutput_label), $fn: 0n, $fa: 0n });
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

  recover_output(result: EvalDyn): boolean
  {
    const n: bigint = this.$fn;
    const a: bigint = this.$fa;
    return !__equals(fieldBitand(result.open(this.$foutput_label)[Number(0n)], 1n), 0n);
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
  $fksk!: LweCiphertextDyn[][];
  $fks_bg_log!: bigint;

  constructor(init: { 
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fks_ell: bigint,
    $fksk: LweCiphertextDyn[][],
    $fks_bg_log: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fks_ell: __zeroValue(this.$fks_ell), $fksk: __zeroValue(this.$fksk), $fks_bg_log: __zeroValue(this.$fks_bg_log) }) as this;
  }
}

export class BootstrappingKeyDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $fks_ell!: bigint;
  $fbsk!: RgswCiphertextDyn[];
  $fksk!: KeySwitchingKeyDyn;
  $fbs_bg_log!: bigint;

  constructor(init: { 
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $fks_ell: bigint,
    $fbsk: RgswCiphertextDyn[],
    $fksk: KeySwitchingKeyDyn,
    $fbs_bg_log: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $fks_ell: __zeroValue(this.$fks_ell), $fbsk: __zeroValue(this.$fbsk), $fksk: __zeroValue(this.$fksk), $fbs_bg_log: __zeroValue(this.$fbs_bg_log) }) as this;
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

export class SoftSpokenOutDyn<D> {
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

export class LweOtCrs {
  $fa!: Zq[][];
  $fh!: Zq[];

  constructor(init: { 
    $fa: Zq[][],
    $fh: Zq[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fa: __zeroValue(this.$fa), $fh: __zeroValue(this.$fh) }) as this;
  }

  static sample<R>(rng: any): LweOtCrs
  {
    let a = Array.from({length: Number(LWE_N)}, () => Array.from({length: Number(LWE_N)}, () => 0n));
    for (let i = 0n; i < LWE_N; i += 1n)     {
      for (let j = 0n; j < LWE_N; j += 1n)       {
        a[Number(i)][Number(j)] = sample_zq(rng);
      }
    }
    let h = Array.from({length: Number(LWE_N)}, () => 0n);
    for (let i = 0n; i < LWE_N; i += 1n)     {
      h[Number(i)] = sample_zq(rng);
    }
    return new LweOtCrs({ $fa: a, $fh: h });
  }
}

export class LweOtReceiver {
  $fs!: Zq[];
  $fc!: boolean;

  constructor(init: { 
    $fs: Zq[],
    $fc: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fs: __zeroValue(this.$fs), $fc: __zeroValue(this.$fc) }) as this;
  }
}

export class LweOtRecvMsg {
  $fpk0!: Zq[];

  constructor(init: { 
    $fpk0: Zq[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fpk0: __zeroValue(this.$fpk0) }) as this;
  }
}

export class LweOtSenderMsgDyn {
  $fl!: bigint;
  $fu0!: Zq[];
  $fv0!: Zq[];
  $fu1!: Zq[];
  $fv1!: Zq[];

  constructor(init: { 
    $fl: bigint,
    $fu0: Zq[],
    $fv0: Zq[],
    $fu1: Zq[],
    $fv1: Zq[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fl: __zeroValue(this.$fl), $fu0: __zeroValue(this.$fu0), $fv0: __zeroValue(this.$fv0), $fu1: __zeroValue(this.$fu1), $fv1: __zeroValue(this.$fv1) }) as this;
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

  cot<R>(rng: any, sample_t: any, b: boolean)
  {
    const n: bigint = this.$fn;
    const r0 = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => sample_t(rng));
    const v = (() => { if (b) {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(r0[Number(i)]), __clone(this.$fdelta.$fdelta[Number(i)])));
} else {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(r0[Number(i)]));
} })();
    return [r0, v];
  }

  static new<T>(n: bigint, delta: DeltaDyn<T>): IdealCotDyn<T>
  {
    return new IdealCotDyn<T>({ $fdelta: delta });
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

export type Sponge =
  | { tag: "Shake128", _0: Shake128 }
  | { tag: "Shake256", _0: Shake256 };
export function Sponge_Shake128(_0: Shake128): Sponge { return { tag: "Shake128", _0 }; }
export function Sponge_Shake256(_0: Shake256): Sponge { return { tag: "Shake256", _0 }; }

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
export const Q4 = fieldShl(1n, 30n);
export const IKNP_KAPPA = 128n;
export const IKNP_KAPPA_BYTES = (IKNP_KAPPA / 8n);
export const TAG_DOMAIN = new Uint8Array([/* byte string */]);
export const LWE_N = 16n;
export const LWE_Q_BITS = 16n;
export const LWE_Q = fieldShl(1n, LWE_Q_BITS);
export const LWE_Q_MASK = fieldSub(LWE_Q, 1n);
export const LWE_NOISE_BOUND = 1n;
export const TOY_P = 2147483647n;
export const TOY_G = 7n;
export const P_LIMBS = [18446744073709551597n, 18446744073709551615n, 18446744073709551615n, 9223372036854775807n];
export const D_LIMBS = [8496970652267935907n, 31536524315187371n, 10144147576115030168n, 5909686906226998899n];
export const D2_LIMBS = [16993941304535871833n, 63073048630374742n, 1841551078520508720n, 2596001775599221991n];
export const D = fe_const(D_LIMBS);
export const D2 = fe_const(D2_LIMBS);
export const BASE_X_LIMBS = [14507833142362363162n, 7578651490590762930n, 13881468655802702940n, 2407515759118799870n];
export const BASE_Y_LIMBS = [7378697629483820632n, 7378697629483820646n, 7378697629483820646n, 7378697629483820646n];

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
  const new_ = wrappingAdd(lower, counter);
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
  const new_ = wrappingAdd(upper, tweak);
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
  let out = /* Vec::with_capacity */ Array(out_bytes);
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
    v[Number(k)] = ((-((half_q4)) & 0xFFFFFFFFn));
  }
  for (let k = (big_n / 2n); k < big_n; k += 1n)   {
    v[Number(k)] = half_q4;
  }
  return v;
}

export function blind_rotate(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, ct: LweCiphertextDyn, bk: BootstrappingKeyDyn): RlweCiphertextDyn
{
  return blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, ct, and_test_poly(big_n), bk);
}

export function blind_rotate_with_poly(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, ct: LweCiphertextDyn, test_poly: bigint[], bk: BootstrappingKeyDyn): RlweCiphertextDyn
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
      acc = cmux(big_n, bs_ell, bk.$fbsk[Number(i)], acc_rotated, acc, bk.$fbs_bg_log);
    }
  }
  return acc;
}

export function chall1(mu: bigint[], iv: bigint[], com_bytes: bigint[], lambda_plus_b: bigint, use_shake256: boolean): Vec<bigint>
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

export function chall2(chall_1: bigint[], u_hat: bigint[], d: bigint[], lambda_plus_b: bigint, use_shake256: boolean): Vec<bigint>
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

export function chall3(chall_2: bigint[], a_hat: bigint[], b_hat: bigint[], c_hat: bigint[], lambda: bigint, use_shake256: boolean): Vec<bigint>
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

export function cmux(big_n: bigint, bs_ell: bigint, c: RgswCiphertextDyn, d1: RlweCiphertextDyn, d0: RlweCiphertextDyn, bs_bg_log: bigint): RlweCiphertextDyn
{
  const diff = rlwe_sub(big_n, d1, d0);
  const prod = external_product(big_n, bs_ell, c, diff, bs_bg_log);
  return rlwe_add(big_n, d0, prod);
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
  let c: Vec<Vec<bigint>> = /* Vec::with_capacity */ Array(fieldSub(BigInt(outs.length), 1n));
  for (const o of outs.slice(Number(1n)))   {
    let ci = __clone(o.$fu);
    xor_in_place(ci, u);
    (c).push(ci);
  }
  let v_columns: Vec<Vec<bigint>> = [];
  for (const o of outs)   {
    for (const vj of o.$fv)     {
      (v_columns).push(vj);
    }
  }
  return new BigVoleProver({ $fu: u, $fc: c, $fv_columns: v_columns });
}

export function concat_small_voles_verifier(outs: Vec<ConvertOutput>, deltas: bigint[], corrections: Vec<bigint>[]): BigVoleVerifier
{
  let q_columns: Vec<Vec<bigint>> = [];
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

export function concat_words(wbound: bigint, a: GrafhenWordDyn, b: GrafhenWordDyn): (GrafhenWordDyn | undefined)
{
  const new_len = (() => { const __match = (a.$flen + (b.$flen)); if (__match !== null && __match !== undefined) { const n = __match;
return n; } else { return undefined; } })();
  let result = GrafhenWord.identity();
  (result.$fdata.slice(0, Number(a.$flen))).splice(0, (a.$fdata.slice(0, Number(a.$flen))).length, ...(a.$fdata.slice(0, Number(a.$flen))));
  (result.$fdata.slice(Number(a.$flen), Number(new_len))).splice(0, (b.$fdata.slice(0, Number(b.$flen))).length, ...(b.$fdata.slice(0, Number(b.$flen))));
  result.$flen = new_len;
  return result;
}

export function convert_to_vole(seeds: (bigint[] | undefined)[], iv: bigint[], tweak: bigint, l_hat_bytes: bigint): ConvertOutput
{
  const n = BigInt(seeds.length);
  const d = Number(Math.clz32((n) & -((n) | 0)));
  const zero_block = [];
  let r: Vec<Vec<bigint>> = /* Vec::with_capacity */ Array(n);
  for (const s of seeds)   {
    return (() => { const __match = s; if (__match !== null && __match !== undefined) { const seed = __match;
return (r).push(aes_ctr_prg(seed, iv, tweak, l_hat_bytes)); } else { return (r).push(__clone(zero_block)); } })();
  }
  let v: Vec<Vec<bigint>> = Array.from({length: Number(d - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => []);
  let level: Vec<Vec<bigint>> = r;
  for (let j = 0n; j < d; j += 1n)   {
    const half = (BigInt(level.length) / 2n);
    let next: Vec<Vec<bigint>> = /* Vec::with_capacity */ Array(half);
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

export function encrypt_branch<R>(l: bigint, rng: any, crs: any, pk: Zq[], msg: bigint[]): [Zq[], Zq[]]
{
  let r = Array.from({length: Number(LWE_N)}, () => 0n);
  for (let i = 0n; i < LWE_N; i += 1n)   {
    r[Number(i)] = sample_noise(rng);
  }
  let u = Array.from({length: Number(LWE_N)}, () => 0n);
  for (let j = 0n; j < LWE_N; j += 1n)   {
    let acc: Zq = 0n;
    for (let i = 0n; i < LWE_N; i += 1n)     {
      acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], r[Number(i)]));
    }
    acc = zq_add(acc, sample_noise(rng));
    u[Number(j)] = acc;
  }
  let base: Zq = 0n;
  for (let i = 0n; i < LWE_N; i += 1n)   {
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

export function eval_word_to_perm(n: bigint, d: bigint, wbound: bigint, key: GrafhenKeyDyn, word: GrafhenWordDyn): bigint[]
{
  let perm: bigint[] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => ((i) & 0xFFn));
  for (const g of word.$fdata.slice(0, Number(word.$flen)))   {
    const g_1 = Number(g);
    const generator: bigint[] = (() => { if ((g_1 < d)) {
  return key.$fgens[Number(g_1)];
} else {
  return key.$finv_gens[Number(fieldSub(g_1, d))];
} })();
    perm = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => generator[Number(Number(perm[Number(i)]))]);
  }
  return perm;
}

export function expand_challenge_to_deltas(chall_1: bigint[], tau: bigint, n: bigint): Vec<bigint>
{
  return Array.from({length: Number(tau - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const byte = Number(chall_1[Number((i % BigInt(chall_1.length)))]);
  return (byte % n);
})());
}

export function external_product(big_n: bigint, bs_ell: bigint, rgsw: RgswCiphertextDyn, rlwe: RlweCiphertextDyn, bs_bg_log: bigint): RlweCiphertextDyn
{
  const a_decomp = poly_decompose(rlwe.$fa, bs_bg_log);
  const b_decomp = poly_decompose(rlwe.$fb, bs_bg_log);
  let out_a = Array.from({length: Number(big_n)}, () => 0n);
  let out_b = Array.from({length: Number(big_n)}, () => 0n);
  for (let j = 0n; j < bs_ell; j += 1n)   {
    const row = rgsw.$frows[Number(j)];
    const prod_a0 = poly_mul_neg(a_decomp[Number(j)], row.$frlwe0.$fa);
    const prod_a1 = poly_mul_neg(a_decomp[Number(j)], row.$frlwe0.$fb);
    const prod_b0 = poly_mul_neg(b_decomp[Number(j)], row.$frlwe1.$fa);
    const prod_b1 = poly_mul_neg(b_decomp[Number(j)], row.$frlwe1.$fb);
    for (let k = 0n; k < big_n; k += 1n)     {
      out_a[Number(k)] = wrappingAdd(wrappingAdd(out_a[Number(k)], prod_a0[Number(k)]), prod_b0[Number(k)]);
      out_b[Number(k)] = wrappingAdd(wrappingAdd(out_b[Number(k)], prod_a1[Number(k)]), prod_b1[Number(k)]);
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
      const [r1, b1] = [wrappingSub(x[Number(i)], P_LIMBS[Number(i)]), false];
      const [r2, b2] = [wrappingSub(r1, borrow), false];
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
    const [r1, br1] = [wrappingSub(P_LIMBS[Number(i)], a[0][Number(i)]), false];
    const [r2, br2] = [wrappingSub(r1, borrow), false];
    neg[Number(i)] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return new Fe25519(neg);
} })();
}

export function fe_sq(a: any): Fe25519
{
  return fe_mul(a, a);
}

export function fe_sub(a: any, b: any): Fe25519
{
  let neg_b = Array.from({length: Number(4n)}, () => 0n);
  let borrow: bigint = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const [r1, br1] = [wrappingSub(P_LIMBS[Number(i)], b[0][Number(i)]), false];
    const [r2, br2] = [wrappingSub(r1, borrow), false];
    neg_b[Number(i)] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return fe_add(a, new Fe25519(neg_b));
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

export function field_mul<T>(ctx: { defaultT: () => any, TClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, sizeOfT: bigint }, a: any, b: any, c: any): T
{
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
}

export function field_square<T>(a: any, c: any): T
{
  return field_mul(__clone(a), a, c);
}

export function gen_abo<B, D>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, k: bigint, n: bigint, a: bigint[], rand: bigint[]): ABODyn<B, D>
{
  let h = ctx.newD();
  const per_byte = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_ni: any) => (() => {
  let per_byte = Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => []);
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

export function gen_bootstrapping_key<R>(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, lwe_sk: LweSecretKeyDyn, rlwe_sk: RlweSecretKeyDyn, bs_bg_log: bigint, ks_bg_log: bigint, bs_noise_bits: bigint, ks_noise_bits: bigint, rng: any): BootstrappingKeyDyn
{
  const bsk = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const bit = !__equals(lwe_sk.$fkey[Number(i)], 0n);
  return rgsw_encrypt(big_n, bs_ell, bit, rlwe_sk, bs_bg_log, bs_noise_bits, rng);
})());
  const rlwe_as_lwe = new LweSecretKeyDyn({ $fkey: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => ((rlwe_sk.$fkey[Number(i)]) & 0xFFn)), $fn_lwe: 0n });
  const ksk_array: LweCiphertextDyn[][] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s_bit = rlwe_sk.$fkey[Number(i)];
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const shift = (32n - (BigInt(Math.imul(Number(ks_bg_log), Number(fieldAdd(Number(j), 1n))))));
  const msg_val = (((s_bit) << (shift)) & 0xFFFFFFFFn);
  return lwe_encrypt_raw(n_lwe, msg_val, lwe_sk, ks_noise_bits, rng);
})());
})());
  const _ = rlwe_as_lwe;
  const ksk = new KeySwitchingKeyDyn({ $fksk: ksk_array, $fks_bg_log: ks_bg_log, $fn_lwe: 0n, $fbig_n: 0n, $fks_ell: 0n });
  return new BootstrappingKeyDyn({ $fbsk: bsk, $fksk: ksk, $fbs_bg_log: bs_bg_log, $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n });
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

export function grafhen_and<R>(wbound: bigint, enc_a: GrafhenWordDyn, enc_b: GrafhenWordDyn, pk: GrafhenPublicDyn<R>): GrafhenWordDyn
{
  const w1 = enc_a;
  const w2 = enc_b;
  const a = pk.$fand_w1;
  const b = pk.$fand_w2;
  const segs: GrafhenWordDyn[] = [w1, a, w1, w2, b, w2, w1, a, w1, w2, b, w2];
  const total_len: bigint = segs.map((s: any) => s.$flen).sum();
  let result = GrafhenWord.identity();
  let pos = 0n;
  for (const seg of segs)   {
    (result.$fdata.slice(Number(pos), Number(fieldAdd(pos, seg.$flen)))).splice(0, (seg.$fdata.slice(0, Number(seg.$flen))).length, ...(seg.$fdata.slice(0, Number(seg.$flen))));
    pos = fieldAdd(pos, seg.$flen);
  }
  result.$flen = total_len;
  pk.$freducer.reduce(result);
  return result;
}

export function grafhen_decrypt(n: bigint, d: bigint, wbound: bigint, key: GrafhenKeyDyn, word: GrafhenWordDyn): (boolean | undefined)
{
  const perm = eval_word_to_perm(n, d, wbound, key, word);
  return (() => { const __match = perm[Number(0n)]; if (__match === 0n) { return false; } else if (__match === 4n) { return true; } else { return undefined; } })();
}

export function grafhen_encrypt<R>(wbound: bigint, bit: boolean, zero_cipher: GrafhenWordDyn, pk: GrafhenPublicDyn<R>): GrafhenWordDyn
{
  return (() => { if (bit) {
  return grafhen_xor(wbound, zero_cipher, pk.$fenc_one);
} else {
  return zero_cipher;
} })();
}

export function grafhen_not<R>(wbound: bigint, a: GrafhenWordDyn, pk: GrafhenPublicDyn<R>): GrafhenWordDyn
{
  return grafhen_xor(wbound, a, pk.$fenc_one);
}

export function grafhen_xor(wbound: bigint, a: GrafhenWordDyn, b: GrafhenWordDyn): GrafhenWordDyn
{
  return (concat_words(wbound, a, b))!;
}

export function grafhen_zero(wbound: bigint): GrafhenWordDyn
{
  return GrafhenWord.identity();
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

export function iknp_cot_extend<R>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): [bigint[][], bigint[][]]
{
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
    const [s_state, s_msg] = ot_send_setup(ctx, rng_r);
    const [r_state, r_msg] = ot_recv(ctx, rng_s, s_msg, delta_ot[Number(i)]);
    const [key_0, key_1] = ot_send_finish(ctx, s_state, r_msg);
    const kc = ot_recv_finish(ctx, r_state);
    let e0 = Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n);
    let e1 = Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => 0n);
    ot_send_payload(key_0, key_1, seeds_0[Number(i)], seeds_1[Number(i)], e0, e1);
    const chosen_e: bigint[] = (() => { if (delta_ot[Number(i)]) {
  return e1;
} else {
  return e0;
} })();
    ot_recv_payload(kc, chosen_e, chosen_seeds[Number(i)]);
  }
  let t_cols = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(m)}, () => false));
  let q_cols = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(m)}, () => false));
    {
    let prg1 = Array.from({length: Number(m)}, () => false);
    for (let i = 0n; i < IKNP_KAPPA; i += 1n)     {
      prg_to_bools(ctx, seeds_0[Number(i)], t_cols[Number(i)]);
      prg_to_bools(ctx, seeds_1[Number(i)], prg1);
      let u_col = Array.from({length: Number(m)}, () => false);
      for (let j = 0n; j < m; j += 1n)       {
        u_col[Number(j)] = fieldBitxor(fieldBitxor(t_cols[Number(i)][Number(j)], prg1[Number(j)]), receiver_bits[Number(j)]);
      }
      let prg_chosen = Array.from({length: Number(m)}, () => false);
      prg_to_bools(ctx, chosen_seeds[Number(i)], prg_chosen);
      for (let j = 0n; j < m; j += 1n)       {
        if (delta_ot[Number(i)])         {
          q_cols[Number(i)][Number(j)] = fieldBitxor(prg_chosen[Number(j)], u_col[Number(j)]);
        } else         {
          q_cols[Number(i)][Number(j)] = prg_chosen[Number(j)];
        }
      }
    }
  }
  let sender_r0 = Array.from({length: Number(m)}, () => Array.from({length: Number(l)}, () => 0n));
  let receiver_v = Array.from({length: Number(m)}, () => Array.from({length: Number(l)}, () => 0n));
  let q_row = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  let t_row = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  for (let j = 0n; j < m; j += 1n)   {
    for (let i = 0n; i < IKNP_KAPPA; i += 1n)     {
      q_row[Number(i)] = q_cols[Number(i)][Number(j)];
      t_row[Number(i)] = t_cols[Number(i)][Number(j)];
    }
    const q_bytes = pack_kappa(q_row);
    const t_bytes = pack_kappa(t_row);
    let r0 = Array.from({length: Number(l)}, () => 0n);
    prg_with_index(ctx, q_bytes, Number(j), r0);
    let q_xor_delta = q_bytes;
    for (let b = 0n; b < IKNP_KAPPA_BYTES; b += 1n)     {
      q_xor_delta[Number(b)] = fieldBitxor(q_xor_delta[Number(b)], delta_ot_bytes[Number(b)]);
    }
    let r1 = Array.from({length: Number(l)}, () => 0n);
    prg_with_index(ctx, q_xor_delta, Number(j), r1);
    let v_pre = Array.from({length: Number(l)}, () => 0n);
    prg_with_index(ctx, t_bytes, Number(j), v_pre);
    let correction = Array.from({length: Number(l)}, () => 0n);
    for (let b = 0n; b < l; b += 1n)     {
      correction[Number(b)] = fieldBitxor(fieldBitxor(r0[Number(b)], r1[Number(b)]), delta_msg[Number(b)]);
    }
    sender_r0[Number(j)] = r0;
    if (receiver_bits[Number(j)])     {
      for (let b = 0n; b < l; b += 1n)       {
        receiver_v[Number(j)][Number(b)] = fieldBitxor(v_pre[Number(b)], correction[Number(b)]);
      }
    } else     {
      receiver_v[Number(j)] = v_pre;
    }
  }
  return [sender_r0, receiver_v];
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

export function key_switch(n_lwe: bigint, big_n: bigint, ks_ell: bigint, ct_big: LweCiphertextDyn, ksk: KeySwitchingKeyDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  let out_b = ct_big.$fb;
  for (let i = 0n; i < big_n; i += 1n)   {
    const digits = ks_decompose(ct_big.$fa[Number(i)], ksk.$fks_bg_log);
    for (let j = 0n; j < ks_ell; j += 1n)     {
      const d = Number(digits[Number(j)]);
      if (__equals(d, 0n))       {
        continue;
      }
      const ksk_ct = ksk.$fksk[Number(i)][Number(j)];
      for (let k = 0n; k < n_lwe; k += 1n)       {
        out_a[Number(k)] = wrappingSub(out_a[Number(k)], BigInt(Math.imul(Number(d), Number(ksk_ct.$fa[Number(k)]))));
      }
      out_b = wrappingSub(out_b, BigInt(Math.imul(Number(d), Number(ksk_ct.$fb))));
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

export function ks_decompose(ks_ell: bigint, x: bigint, bg_log: bigint): bigint[]
{
  const bg = fieldShl(1n, bg_log);
  const mask = fieldSub(bg, 1n);
  let rem = BigInt(x);
  const tail_shift = (32n - (BigInt(Math.imul(Number(bg_log), Number(Number(ks_ell))))));
  if (((tail_shift > 0n) && (tail_shift < 32n)))   {
    const half_tail = fieldShl(1n, fieldSub(tail_shift, 1n));
    rem = wrappingAdd(rem, half_tail);
  }
  let digits = Array.from({length: Number(ks_ell)}, () => 0n);
  for (const j of (Array.from({length: Number(ks_ell - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    const shift = (32n - (BigInt(Math.imul(Number(bg_log), Number(fieldAdd(Number(j), 1n))))));
    if ((shift < 32n))     {
      digits[Number(j)] = Number(fieldBitand(fieldShr(rem, shift), mask));
    }
  }
  return digits;
}

export function lift_bit<T>(n: bigint, bit_t: any): T[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(bit_t));
}

export function lwe_add(n_lwe: bigint, a: LweCiphertextDyn, b: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let i = 0n; i < n_lwe; i += 1n)   {
    out_a[Number(i)] = wrappingAdd(a.$fa[Number(i)], b.$fa[Number(i)]);
  }
  return new LweCiphertextDyn({ $fa: out_a, $fb: wrappingAdd(a.$fb, b.$fb), $fn_lwe: 0n });
}

export function lwe_decrypt(n_lwe: bigint, ct: LweCiphertextDyn, sk: LweSecretKeyDyn): boolean
{
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(ct.$fa[Number(i)]), Number(Number(sk.$fkey[Number(i)])))));
  }
  const phase = wrappingSub(ct.$fb, dot);
  const half = fieldShr(Q4, 1n);
  const shifted = wrappingSub(phase, half);
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
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(Number(sk.$fkey[Number(i)])))));
  }
  const e: bigint = small_noise(noise_bits, rng);
  const msg = (() => { if (m) {
  return Q4;
} else {
  return 0n;
} })();
  const b = wrappingAdd(wrappingAdd(dot, e), msg);
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
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(Number(sk.$fkey[Number(i)])))));
  }
  const e = small_noise(noise_bits, rng);
  const b = wrappingAdd(wrappingAdd(dot, e), msg);
  return new LweCiphertextDyn({ $fa: a, $fb: b, $fn_lwe: 0n });
}

export function lwe_ot_recv<R>(rng: any, crs: any, c: boolean): [LweOtReceiver, LweOtRecvMsg]
{
  let s = Array.from({length: Number(LWE_N)}, () => 0n);
  for (let i = 0n; i < LWE_N; i += 1n)   {
    s[Number(i)] = sample_noise(rng);
  }
  let pk_real = Array.from({length: Number(LWE_N)}, () => 0n);
  for (let i = 0n; i < LWE_N; i += 1n)   {
    let acc: Zq = 0n;
    for (let j = 0n; j < LWE_N; j += 1n)     {
      acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], s[Number(j)]));
    }
    acc = zq_add(acc, sample_noise(rng));
    pk_real[Number(i)] = acc;
  }
  const pk0 = (() => { if (c) {
  let pk0 = Array.from({length: Number(LWE_N)}, () => 0n);
  for (let i = 0n; i < LWE_N; i += 1n)   {
    pk0[Number(i)] = zq_sub(crs.$fh[Number(i)], pk_real[Number(i)]);
  }
  return pk0;
} else {
  return pk_real;
} })();
  return [new LweOtReceiver({ $fs: s, $fc: c }), new LweOtRecvMsg({ $fpk0: pk0 })];
}

export function lwe_ot_recv_decrypt(l: bigint, receiver: any, sender_msg: LweOtSenderMsgDyn): bigint[]
{
  const [u, v] = (() => { if (receiver.$fc) {
  return [sender_msg.$fu1, sender_msg.$fv1];
} else {
  return [sender_msg.$fu0, sender_msg.$fv0];
} })();
  let s_dot_u: Zq = 0n;
  for (let i = 0n; i < LWE_N; i += 1n)   {
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

export function lwe_ot_send<R>(l: bigint, rng: any, crs: any, recv_msg: any, m0: bigint[], m1: bigint[]): LweOtSenderMsgDyn
{
  const pk0 = recv_msg.$fpk0;
  let pk1 = Array.from({length: Number(LWE_N)}, () => 0n);
  for (let i = 0n; i < LWE_N; i += 1n)   {
    pk1[Number(i)] = zq_sub(crs.$fh[Number(i)], pk0[Number(i)]);
  }
  const [u0, v0] = encrypt_branch(rng, crs, pk0, m0);
  const [u1, v1] = encrypt_branch(rng, crs, pk1, m1);
  return new LweOtSenderMsgDyn({ $fu0: u0, $fv0: v0, $fu1: u1, $fv1: v1, $fl: 0n });
}

export function memory_check_per_lane<T>(n: bigint, challenges: T[]): MemoryCheckStateDyn<T, AdditiveHasher>[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const key = ChallengeKey.from_challenge(__clone(challenges[Number(i)]));
  return MemoryCheckState.new(key);
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

export function poly_add_neg(n: bigint, a: bigint[], b: bigint[]): bigint[]
{
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    result[Number(i)] = wrappingAdd(a[Number(i)], b[Number(i)]);
  }
  return result;
}

export function poly_decompose(big_n: bigint, bs_ell: bigint, p: bigint[], bg_log: bigint): bigint[][]
{
  const bg = fieldShl(1n, bg_log);
  const mask = Number(fieldSub(bg, 1n));
  let result = Array.from({length: Number(bs_ell)}, () => Array.from({length: Number(big_n)}, () => 0n));
  for (let i = 0n; i < big_n; i += 1n)   {
    const x = p[Number(i)];
    const tail_bits = (32n - (BigInt(Math.imul(Number(bg_log), Number(Number(bs_ell))))));
    const rounded = (() => { if (((tail_bits > 0n) && (tail_bits < 32n))) {
  return wrappingAdd(x, fieldShl(1n, fieldSub(tail_bits, 1n)));
} else {
  return x;
} })();
    for (let j = 0n; j < bs_ell; j += 1n)     {
      const shift = (32n - (BigInt(Math.imul(Number(bg_log), Number(fieldAdd(Number(j), 1n))))));
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
        result[Number(deg)] = wrappingAdd(result[Number(deg)], BigInt(Math.imul(Number(a[Number(i)]), Number(b[Number(j)]))));
      } else       {
        result[Number(fieldSub(deg, n))] = wrappingSub(result[Number(fieldSub(deg, n))], BigInt(Math.imul(Number(a[Number(i)]), Number(b[Number(j)]))));
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
      result[Number(new_pos)] = wrappingAdd(result[Number(new_pos)], p[Number(i)]);
    } else if ((new_pos < fieldMul(2n, n)))     {
      result[Number(fieldSub(new_pos, n))] = wrappingSub(result[Number(fieldSub(new_pos, n))], p[Number(i)]);
    } else     {
      result[Number(fieldSub(new_pos, fieldMul(2n, n)))] = wrappingAdd(result[Number(fieldSub(new_pos, fieldMul(2n, n)))], p[Number(i)]);
    }
  }
  return result;
}

export function poly_sub_neg(n: bigint, a: bigint[], b: bigint[]): bigint[]
{
  let result = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    result[Number(i)] = wrappingSub(a[Number(i)], b[Number(i)]);
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

export function recompute_tree(r: bigint[], total_leaves: bigint): Vec<bigint[]>
{
  const total_nodes = fieldSub(fieldMul(2n, total_leaves), 1n);
  let tree = [];
  tree[Number(0n)] = r;
  for (let node = 0n; node < fieldSub(total_leaves, 1n); node += 1n)   {
    const parent = tree[Number(node)];
    const [left, right] = AesCtrLengthDoubler.double(parent);
    tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left[0];
    tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right[0];
  }
  return tree;
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

export function rgsw_encrypt<R>(big_n: bigint, bs_ell: bigint, m: boolean, sk: RlweSecretKeyDyn, bs_bg_log: bigint, noise_bits: bigint, rng: any): RgswCiphertextDyn
{
  const msg_bit = (() => { if (m) {
  return 1n;
} else {
  return 0n;
} })();
  const rows = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const shift = (32n - (BigInt(Math.imul(Number(bs_bg_log), Number(fieldAdd(Number(j), 1n))))));
  const g_factor = (((1n) << (shift)) & 0xFFFFFFFFn);
  const contrib = BigInt(Math.imul(Number(msg_bit), Number(g_factor)));
  let rlwe0 = rlwe_encrypt_scalar(big_n, 0n, sk, noise_bits, rng);
  rlwe0.$fa[Number(0n)] = wrappingAdd(rlwe0.$fa[Number(0n)], contrib);
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
    b[Number(i)] = wrappingAdd(wrappingAdd(b[Number(i)], small_noise(noise_bits, rng)), msg_poly[Number(i)]);
  }
  return new RlweCiphertextDyn({ $fa: a, $fb: b, $fbig_n: 0n });
}

export function rlwe_encrypt_scalar<R>(big_n: bigint, m: bigint, sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(a, sk.$fkey);
  b[Number(0n)] = wrappingAdd(wrappingAdd(b[Number(0n)], small_noise(noise_bits, rng)), m);
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

export function sample_extract(big_n: bigint, rlwe: RlweCiphertextDyn): LweCiphertextDyn
{
  let a_lwe = Array.from({length: Number(big_n)}, () => 0n);
  a_lwe[Number(0n)] = rlwe.$fa[Number(0n)];
  for (let i = 1n; i < big_n; i += 1n)   {
    a_lwe[Number(i)] = ((-((rlwe.$fa[Number(fieldSub(big_n, i))])) & 0xFFFFFFFFn));
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

export function sample_zq<R>(rng: any): Zq
{
  return fieldBitand(rng.next_u32(), LWE_Q_MASK);
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

export function sign(sk: any, pk: any, message: bigint[], iv_seed: bigint[], prover: any): FaestSignature
{
  const iv: bigint[] = aes128_encrypt(iv_seed, Array.from({length: Number(LAMBDA_BYTES)}, () => 0n));
  const r: bigint[] = aes128_encrypt(sk[0], iv);
  const commitment: BavcCommitmentDyn = Bavc.commit(r, iv, TAU, SUB_VOLE_N);
  const mu: Vec<bigint> = (() => {
  let h = Sha3_256.new();
  h.update(pk[0]);
  h.update(message);
  return [...h.finalize()];
})();
  const chall_1 = chall1(mu, iv, commitment.$froot, fieldAdd(LAMBDA_BYTES, 8n), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const nodes = Bavc.collect_open_nodes(deltas, recompute_tree(r, fieldMul(TAU, SUB_VOLE_N)), TAU, SUB_VOLE_N);
  const hidden_commits: Vec<bigint[]> = deltas.map((val: any, i: number) => [i, val] as [number, typeof val]).map(([i, d]: any) => commitment.$fcommitments[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), d))]);
  const opening = new BavcOpeningDyn({ $fhidden_commits: __clone(hidden_commits), $fnodes: __clone(nodes), $fcom_bytes: 0n });
  const _ = opening;
  let sub_voles = /* Vec::with_capacity */ Array(TAU);
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

export function small_noise<R>(noise_bits: bigint, rng: any): bigint
{
  if ((noise_bits >= 32n))   {
    return rng.next_u32();
  }
  const raw: bigint = rng.next_u32();
  const mask = wrappingSub(fieldShl(1n, noise_bits), 1n);
  const small = fieldBitand(raw, mask);
  return (() => { if (((noise_bits > 0n) && !__equals(fieldShr(small, fieldSub(noise_bits, 1n)), 0n))) {
  return fieldBitor(small, !mask);
} else {
  return small;
} })();
}

export function softspoken_cot_extend<D, R>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, k: bigint, m: bigint, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): SoftSpokenOutDyn<D>
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
  return new SoftSpokenOutDyn({ $fsender_r0: sender_r0, $freceiver_v: receiver_v, $fsender_tag: sender_tag, $freceiver_tag: receiver_tag, $fm: 0n, $fl: 0n });
}

export function sub_bytes(state: bigint[])
{
  for (let i = 0n; i < BLOCK; i += 1n)   {
    state[Number(i)] = SBOX[Number(Number(state[Number(i)]))];
  }
}

export function tfhe_cmux(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, sel: LweCiphertextDyn, a: LweCiphertextDyn, b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const not_sel = tfhe_not(n_lwe, sel);
  const sel_and_a = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, sel, a, bk);
  const nsel_and_b = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, not_sel, b, bk);
  return tfhe_gate_bootstrapping_or(n_lwe, big_n, bs_ell, ks_ell, sel_and_a, nsel_and_b, bk);
}

export function tfhe_gate_bootstrapping_and(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, ct_a: LweCiphertextDyn, ct_b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  let ct = lwe_add(n_lwe, ct_a, ct_b);
  ct.$fb = wrappingSub(ct.$fb, fieldShr(Q4, 1n));
  const acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, ct, bk);
  const lwe_big = sample_extract(big_n, acc);
  let ct_out = key_switch(n_lwe, big_n, ks_ell, lwe_big, bk.$fksk);
  ct_out.$fb = wrappingAdd(ct_out.$fb, fieldShr(Q4, 1n));
  return ct_out;
}

export function tfhe_gate_bootstrapping_or(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, ct_a: LweCiphertextDyn, ct_b: LweCiphertextDyn, bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  let ct = lwe_add(n_lwe, ct_a, ct_b);
  ct.$fb = wrappingAdd(ct.$fb, fieldShr(Q4, 1n));
  const acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, ct, bk);
  const lwe_big = sample_extract(big_n, acc);
  let ct_out = key_switch(n_lwe, big_n, ks_ell, lwe_big, bk.$fksk);
  ct_out.$fb = wrappingAdd(ct_out.$fb, fieldShr(Q4, 1n));
  return ct_out;
}

export function tfhe_lut_read(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, addr_bits: LweCiphertextDyn[], lut: boolean[], bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const two_n = fieldMul(2n, big_n);
  const k = BigInt(1 << Math.ceil(Math.log2(Number(BigInt(Math.max(Number(BigInt(lut.length)), Number(1n)))))));
  if ((!(lut.length === 0) && lut.every((v) => __equals(v, lut[Number(0n)]))))   {
    const msg = (() => { if (lut[Number(0n)]) {
  return Q4;
} else {
  return 0n;
} })();
    return new LweCiphertextDyn({ $fa: Array.from({length: Number(n_lwe)}, () => 0n), $fb: msg, $fn_lwe: 0n });
  }
  const half_q4 = fieldShr(Q4, 1n);
  const step = (two_n / k);
  const half_k = (k / 2n);
  const poly_step = (big_n / half_k);
  let test_poly = Array.from({length: Number(big_n)}, () => 0n);
  for (let j = 0n; j < big_n; j += 1n)   {
    const entry_idx = (j / poly_step);
    const val = (() => { if (((entry_idx < BigInt(lut.length)) && lut[Number(entry_idx)])) {
  return half_q4;
} else {
  return ((-((half_q4)) & 0xFFFFFFFFn));
} })();
    test_poly[Number(j)] = val;
  }
  const delta = (fieldShl(1n, 32n) / BigInt(k));
  let combined = new LweCiphertextDyn({ $fa: Array.from({length: Number(n_lwe)}, () => 0n), $fb: 0n, $fn_lwe: 0n });
  for (const [j, addr_ct] of addr_bits.map((val: any, i: number) => [i, val] as [number, typeof val]))   {
    const target = fieldMul(fieldShl(1n, j), delta);
    for (let i = 0n; i < n_lwe; i += 1n)     {
      const scaled = (BigInt(Math.imul(Number(BigInt(addr_ct.$fa[Number(i)])), Number(target))) / BigInt(Q4));
      combined.$fa[Number(i)] = wrappingAdd(combined.$fa[Number(i)], Number(scaled));
    }
    const scaled_b = (BigInt(Math.imul(Number(BigInt(addr_ct.$fb)), Number(target))) / BigInt(Q4));
    combined.$fb = wrappingAdd(combined.$fb, Number(scaled_b));
  }
  const centering = Number((delta / 2n));
  combined.$fb = wrappingAdd(combined.$fb, centering);
  let ct_out = tfhe_programmable_bootstrap(n_lwe, big_n, bs_ell, ks_ell, combined, test_poly, bk);
  ct_out.$fb = wrappingAdd(ct_out.$fb, half_q4);
  return ct_out;
}

export function tfhe_not(n_lwe: bigint, a: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let i = 0n; i < n_lwe; i += 1n)   {
    out_a[Number(i)] = ((-((a.$fa[Number(i)])) & 0xFFFFFFFFn));
  }
  return new LweCiphertextDyn({ $fa: out_a, $fb: wrappingSub(Q4, a.$fb), $fn_lwe: 0n });
}

export function tfhe_programmable_bootstrap(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, ct: LweCiphertextDyn, test_poly: bigint[], bk: BootstrappingKeyDyn): LweCiphertextDyn
{
  const acc = blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, ct, test_poly, bk);
  const lwe_big = sample_extract(big_n, acc);
  return key_switch(n_lwe, big_n, ks_ell, lwe_big, bk.$fksk);
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

export function tfhe_xor(n_lwe: bigint, a: LweCiphertextDyn, b: LweCiphertextDyn): LweCiphertextDyn
{
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let i = 0n; i < n_lwe; i += 1n)   {
    out_a[Number(i)] = wrappingAdd(a.$fa[Number(i)], b.$fa[Number(i)]);
  }
  return new LweCiphertextDyn({ $fa: out_a, $fb: wrappingAdd(a.$fb, b.$fb), $fn_lwe: 0n });
}

export function torus_to_exp(x: bigint, scale_shift: bigint, two_n: bigint): bigint
{
  const half = (() => { if ((scale_shift > 0n)) {
  return fieldShl(1n, fieldSub(scale_shift, 1n));
} else {
  return 0n;
} })();
  const exp = Number(fieldShr(wrappingAdd(x, half), scale_shift));
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

export function verify(pk: any, message: bigint[], sig: any): boolean
{
  const iv = sig.$fiv;
  const mu: Vec<bigint> = (() => {
  let h = Sha3_256.new();
  h.update(pk[0]);
  h.update(message);
  return [...h.finalize()];
})();
  const chall_1 = chall1(mu, iv, sig.$fbavc_root, fieldAdd(LAMBDA_BYTES, 8n), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const reconstructed_seeds_opt = Bavc.reconstruct(sig.$fnodes, sig.$fhidden_commits, deltas, iv, sig.$fbavc_root, TAU, SUB_VOLE_N);
  const reconstructed_seeds = (() => { const __match = reconstructed_seeds_opt; if (__match !== null && __match !== undefined) { const s = __match;
return s; } else { return false; } })();
  let sub_voles_v = /* Vec::with_capacity */ Array(TAU);
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
  const u_c_inner = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldMul(__clone(vope_a.$fu[Number(0n)][Number(i)]), __clone(vope_b.$fu[Number(0n)][Number(i)]));
})());
  const u_c = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u_c_inner));
  const v_c = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(fieldMul(__clone(vope_a.$fv[Number(i)]), __clone(vope_b.$fu[Number(0n)][Number(i)])), fieldMul(__clone(vope_b.$fv[Number(i)]), __clone(vope_a.$fu[Number(0n)][Number(i)])));
})());
  const hat = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldMul(__clone(vope_a.$fv[Number(i)]), __clone(vope_b.$fv[Number(i)]));
})());
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
  const u: T[][] = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(u_row[Number(i)]));
})());
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

export function vole_mul3_prover_step<T>(n: bigint, vope_a: VopeDyn<T>, vope_b: VopeDyn<T>, vope_d: VopeDyn<T>): VopeDyn<T>
{
  const ab: VopeDyn<T> = vope_a.mul_generalized(vope_b);
  return ab.mul_generalized(vope_d);
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

export function vole_sbox_prover_step<T>(n: bigint, vope_a: VopeDyn<T>, vope_b: VopeDyn<T>): [VopeDyn<T>, VopeDyn<T>]
{
  const k2: VopeDyn<T> = vope_a.mul_generalized(vope_b);
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
  return fieldBitand(wrappingAdd(a, b), LWE_Q_MASK);
}

export function zq_mul(a: any, b: any): Zq
{
  return fieldBitand(BigInt(Math.imul(Number(a), Number(b))), LWE_Q_MASK);
}

export function zq_neg(a: any): Zq
{
  return fieldBitand(wrappingSub(LWE_Q, a), LWE_Q_MASK);
}

export function zq_sub(a: any, b: any): Zq
{
  return fieldBitand(wrappingSub(a, b), LWE_Q_MASK);
}

export function absorb(data: bigint[])
{
  return (() => { const __match = this; if (true /* Sponge::Shake128 */) { const h = __match[0];
return h.update(data); } else { const h = __match[0];
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
  let out = [];
  (() => { const __match = this; if (true /* Sponge::Shake128 */) { const h = __match[0];
return (() => {
  let r = __clone(h).finalize_xof();
  r.read(out);
})(); } else { const h = __match[0];
return (() => {
  let r = __clone(h).finalize_xof();
  r.read(out);
})(); } })();
  return out;
}

