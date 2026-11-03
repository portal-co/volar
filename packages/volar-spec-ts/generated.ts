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
  u16_from_le_bytes,
  u32_from_le_bytes,
  u64_from_le_bytes,
  u128_from_le_bytes,
} from "./index";

class __StubXofReader { read(_target: bigint[]): void {} }
class __StubDigest { readonly outputSize = 32; update(_data: Uint8Array | readonly (number | bigint)[]): void {} finalize(): Uint8Array { return new Uint8Array(this.outputSize); } finalize_xof(): __StubXofReader { return new __StubXofReader(); } }
class Shake128 extends __StubDigest {} class Shake256 extends __StubDigest {} class Sha3_256 extends __StubDigest {}
type DigestUpdate = any;
declare const aes128_encrypt: typeof encrypt_block;

class Some<T> { constructor(public _0: T) {} }
class Ok<T> { constructor(public _0: T) {} }
class Err<E = unknown> { constructor(public _0: E) {} }
type Vec<T> = T[];
type Option<T> = T | undefined;
type Result<T, E = unknown> = T | E | __VolarError;
export abstract class __VolarError {}
function __clone<T>(x: T): T {
  if (Array.isArray(x)) return ([...x] as unknown) as T;
  if (x !== null && typeof x === 'object') return Object.assign(Object.create(Object.getPrototypeOf(x)), x) as T;
  return x;
}
function __u32_try_from(value: bigint | number): bigint {
  const converted = BigInt(value);
  if (converted < 0n || converted > 0xffff_ffffn) throw new RangeError('u32 conversion out of range');
  return converted;
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
export type RqPoly = Vec<bigint>;
export type RpPoly = Vec<bigint>;
export type Limb = Vec<bigint>;
export type LabelPoly = Vec<bigint>;
export type WireStates = Vec<WireState>;
export type Block = bigint[];
export type Zq = bigint;
export type DigestImpl = Sha3_256;
export type Base = LweBaseOtDyn;
export type Log2<N> = number /* any::Output */;
export type WarpDigest = bigint[];

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
    const word = BigInt((n / 64n));
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
  [0]!: any;

  constructor(_0: any) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  add(rhs: TropicalDyn<any>)
  {
    return new TropicalDyn(BigInt(Math.min(Number(this[0]), Number(rhs[0]))));
  }

  mul(rhs: TropicalDyn<any>)
  {
    return new TropicalDyn(fieldAdd(this[0], rhs[0]));
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

export class ViaDigestPuncturableRandomizerDyn<D> {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  static double(ctx: { D_OutputSize: bigint, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, a: bigint[]): bigint[][]
  {
    const v = ctx.DClass.digest(a);
    return [__clone(v), Array.from({length: Number(ctx.D_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(v[Number(i)], a[Number(i)]))];
  }
}

export class RoleKeys {
  $fenc!: bigint[][];
  $fmac!: bigint[][];

  constructor(init: {
    $fenc: bigint[][],
    $fmac: bigint[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fenc: __zeroValue(this.$fenc), $fmac: __zeroValue(this.$fmac) }) as this;
  }
}

export class PlanBoundary {
  $fplan!: BootstrapPlan;
  $fdelta!: bigint;

  constructor(init: {
    $fplan: BootstrapPlan,
    $fdelta: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fplan: __zeroValue(this.$fplan), $fdelta: __zeroValue(this.$fdelta) }) as this;
  }

  decrypt_outputs(n_lwe: bigint, log_q_lwe: bigint, wire_arena: BinfheLweCiphertextDyn[], sk: BinfheLweSecretKeyDyn): Result<Vec<boolean>, BoundaryError>
  {
    const required = (((__values: any[]) => __values.length === 0 ? undefined : __values.reduce((__a: any, __b: any) => __a > __b ? __a : __b))(this.$fplan.$foutputs).map((wire: any) => fieldAdd(BigInt(wire), 1n)) ?? (0n));
    if ((BigInt(wire_arena.length) < required))     {
      return new BoundaryError_WireArenaTooShort(required, BigInt(wire_arena.length));
    }
    return this.$fplan.$foutputs.map((wire: any) => (() => {
  return binfhe_lwe_decrypt(n_lwe, log_q_lwe, wire_arena[Number(BigInt(wire))], sk, this.$fdelta);
})());
  }

  delta(): bigint
  {
    return this.$fdelta;
  }

  encrypt_inputs<R>(n_lwe: bigint, log_q_lwe: bigint, eta: bigint, inputs: boolean[], sk: BinfheLweSecretKeyDyn, rng: any): Result<Vec<BinfheLweCiphertextDyn>, BoundaryError>
  {
    let output = this.input_buffer(n_lwe);
    const __volar_try_1 = this.encrypt_inputs_into(n_lwe, log_q_lwe, eta, inputs, sk, rng, output);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    return output.$fciphertexts;
  }

  encrypt_inputs_into<R>(n_lwe: bigint, log_q_lwe: bigint, eta: bigint, inputs: boolean[], sk: BinfheLweSecretKeyDyn, rng: any, output: CiphertextInputBufferDyn): Result<void, BoundaryError>
  {
    const __volar_try_0 = this.require_input_count(BigInt(inputs.length));
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    if ((output.$fciphertexts.capacity() < BigInt(inputs.length)))     {
      return new BoundaryError_InputBufferCapacity(BigInt(inputs.length), output.$fciphertexts.capacity());
    }
    output.$fciphertexts.clear();
    output.$fciphertexts.push(...(inputs.map((bit: any) => (() => {
  return binfhe_lwe_encrypt(n_lwe, log_q_lwe, eta, bit, this.$fdelta, sk, rng);
})())));
    return undefined;
  }

  execute(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, priv_ell: bigint, priv_base_log: bigint, inputs: BinfheLweCiphertextDyn[], cells: BinfheRlweCiphertextDyn[], bk: BinfheBootstrappingKeyDyn, cbk: CircuitBootstrappingKeyDyn): Result<[Vec<BinfheLweCiphertextDyn>, Vec<BinfheRlweCiphertextDyn>], BoundaryError>
  {
    const __volar_try_0 = this.require_ciphertext_input_count(BigInt(inputs.length));
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const __volar_try_1 = this.require_cell_count(BigInt(cells.length));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    return execute_plan(ctx, n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, priv_ell, priv_base_log, this.$fplan, inputs, cells, bk, cbk);
  }

  execute_in_workspace(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, priv_ell: bigint, priv_base_log: bigint, inputs: BinfheLweCiphertextDyn[], cells: BinfheRlweCiphertextDyn[], bk: BinfheBootstrappingKeyDyn, cbk: CircuitBootstrappingKeyDyn, workspace: PlanWorkspaceDyn): Result<[BinfheLweCiphertextDyn[], BinfheRlweCiphertextDyn[]], BoundaryError>
  {
    const __volar_try_0 = this.require_ciphertext_input_count(BigInt(inputs.length));
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const __volar_try_1 = this.require_cell_count(BigInt(cells.length));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    return execute_plan_in_workspace(ctx, n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, priv_ell, priv_base_log, this.$fplan, inputs, cells, bk, cbk, workspace);
  }

  input_buffer(n_lwe: bigint): CiphertextInputBufferDyn
  {
    return new CiphertextInputBufferDyn({ $fciphertexts: ([] as any[]), $fn_lwe: 0n });
  }

  static new(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, log_q_lwe: bigint, plan: any): Result<PlanBoundary, BoundaryError>
  {
    const __volar_try_0 = plan.validate();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const delta = wire_delta(log_q_lwe, BigInt(plan.$fk_max));
    return new PlanBoundary({ $fplan: plan, $fdelta: delta });
  }

  plan(): BootstrapPlan
  {
    return this.$fplan;
  }

  require_cell_count(actual: bigint): Result<void, BoundaryError>
  {
    const expected = BigInt(this.$fplan.$fnum_cells);
    return (() => { if (__equals(actual, expected)) {
  return undefined;
} else {
  return new BoundaryError_CellCount(expected, actual);
} })();
  }

  require_ciphertext_input_count(actual: bigint): Result<void, BoundaryError>
  {
    const expected = BigInt(this.$fplan.$fnum_inputs);
    return (() => { if (__equals(actual, expected)) {
  return undefined;
} else {
  return new BoundaryError_CiphertextInputCount(expected, actual);
} })();
  }

  require_input_count(actual: bigint): Result<void, BoundaryError>
  {
    const expected = BigInt(this.$fplan.$fnum_inputs);
    return (() => { if (__equals(actual, expected)) {
  return undefined;
} else {
  return new BoundaryError_InputCount(expected, actual);
} })();
  }
}

export class CiphertextInputBufferDyn {
  $fn_lwe!: bigint;
  $fciphertexts!: Vec<BinfheLweCiphertextDyn>;

  constructor(init: {
    $fn_lwe: bigint,
    $fciphertexts: Vec<BinfheLweCiphertextDyn>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fciphertexts: __zeroValue(this.$fciphertexts) }) as this;
  }

  as_slice(): BinfheLweCiphertextDyn[]
  {
    const n_lwe: bigint = this.$fn_lwe;
    return this.$fciphertexts;
  }

  capacity(): bigint
  {
    const n_lwe: bigint = this.$fn_lwe;
    return this.$fciphertexts.capacity();
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
    return fieldShl(1n, fieldSub(fieldSub(log_q_lwe, 1n), BigInt(k_max)));
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

export class PlanWorkspaceRequirements {
  $fwires!: bigint;
  $frgsws!: bigint;
  $fcells!: bigint;
  $flut_inputs!: bigint;

  constructor(init: {
    $fwires: bigint,
    $frgsws: bigint,
    $fcells: bigint,
    $flut_inputs: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fwires: __zeroValue(this.$fwires), $frgsws: __zeroValue(this.$frgsws), $fcells: __zeroValue(this.$fcells), $flut_inputs: __zeroValue(this.$flut_inputs) }) as this;
  }
}

export class PlanWorkspaceDyn {
  $fn_lwe!: bigint;
  $fbig_n!: bigint;
  $fbs_ell!: bigint;
  $fwires!: Vec<BinfheLweCiphertextDyn>;
  $frgsws!: Vec<BinfheRgswCiphertextDyn>;
  $fcells!: Vec<BinfheRlweCiphertextDyn>;
  $flut_inputs!: Vec<BinfheLweCiphertextDyn>;

  constructor(init: {
    $fn_lwe: bigint,
    $fbig_n: bigint,
    $fbs_ell: bigint,
    $fwires: Vec<BinfheLweCiphertextDyn>,
    $frgsws: Vec<BinfheRgswCiphertextDyn>,
    $fcells: Vec<BinfheRlweCiphertextDyn>,
    $flut_inputs: Vec<BinfheLweCiphertextDyn>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn_lwe: __zeroValue(this.$fn_lwe), $fbig_n: __zeroValue(this.$fbig_n), $fbs_ell: __zeroValue(this.$fbs_ell), $fwires: __zeroValue(this.$fwires), $frgsws: __zeroValue(this.$frgsws), $fcells: __zeroValue(this.$fcells), $flut_inputs: __zeroValue(this.$flut_inputs) }) as this;
  }

  static new(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n_lwe: bigint, big_n: bigint, bs_ell: bigint, plan: any): Result<PlanWorkspaceDyn, PlanWorkspaceError>
  {
    const __volar_try_0 = plan.workspace_requirements(ctx, );
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const required = __volar_try_0;
    return new PlanWorkspaceDyn({ $fwires: ([] as any[]), $frgsws: ([] as any[]), $fcells: ([] as any[]), $flut_inputs: ([] as any[]) });
  }

  requirements(): PlanWorkspaceRequirements
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    return new PlanWorkspaceRequirements({ $fwires: this.$fwires.capacity(), $frgsws: this.$frgsws.capacity(), $fcells: this.$fcells.capacity(), $flut_inputs: this.$flut_inputs.capacity() });
  }

  reset()
  {
    const n_lwe: bigint = this.$fn_lwe;
    const big_n: bigint = this.$fbig_n;
    const bs_ell: bigint = this.$fbs_ell;
    this.$fwires.clear();
    this.$frgsws.clear();
    this.$fcells.clear();
    this.$flut_inputs.clear();
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
  $foutputs!: Vec<WireId>;
  $fcell_outputs!: Vec<CellId>;
  $fbudget!: FailureBudget;

  constructor(init: {
    $fprofile: ProfileId,
    $fk_max: bigint,
    $fluts: Vec<LutSpec>,
    $flayers: Vec<Vec<PlanOp>>,
    $fnum_inputs: bigint,
    $fnum_cells: bigint,
    $foutputs: Vec<WireId>,
    $fcell_outputs: Vec<CellId>,
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
        (() => { const __match = op; if (__match instanceof PlanOp_Lut) { const table = __match.$ftable;
return (() => {
  if (!table_is_constant(this.$fluts[Number(BigInt(table))].$fentries))   {
    count = fieldAdd(count, 1n);
  }
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { return count = fieldAdd(count, 1n); } else { return (() => {
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
        (() => { const __match = op; if (__match instanceof PlanOp_Const) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
  (wires).push(value);
})(); } else if (__match instanceof PlanOp_Not) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (wires).push(!wires[Number(BigInt(input))]);
})(); } else if (__match instanceof PlanOp_Lut) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  let address = 0n;
  for (const [bit, input] of inputs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    address = fieldBitor(address, fieldShl(BigInt(wires[Number(BigInt(input))]), bit));
  }
  (wires).push(this.$fluts[Number(BigInt(table))].$fentries[Number(address)]);
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (rgsws).push(wires[Number(BigInt(input))]);
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
  (cell_arena).push((() => { if (rgsws[Number(BigInt(sel))]) {
  return cell_arena[Number(BigInt(then_cell))];
} else {
  return cell_arena[Number(BigInt(else_cell))];
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
      for (const [i, chunk] of __chunks(spec.$fentries, Number(8n)).map((__enum_a: any, __enum_i: number) => [BigInt(__enum_i), __enum_a] as [bigint, typeof __enum_a]))       {
        let byte = ((0n) & 0xFFn);
        for (const [j, e] of chunk.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))         {
          byte = fieldBitor(byte, fieldShl(((e) & 0xFFn), j));
        }
        const _ = i;
      }
    }
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        (() => { const __match = op; if (__match instanceof PlanOp_Const) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
})(); } else if (__match instanceof PlanOp_Not) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
})(); } else if (__match instanceof PlanOp_Lut) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  for (const w of inputs)   {
  }
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { const input = __match.$finput;
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
    for (const [i, spec] of this.$fluts.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const len = BigInt(spec.$fentries.length);
      if ((__equals(len, 0n) || !((len) > 0n && ((len) & ((len) - 1n)) === 0n)))       {
        return new PlanError_BadTableShape((i as unknown as LutId));
      }
      const arity = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, len)));
      if ((arity > BigInt(this.$fk_max)))       {
        return new PlanError_ArityExceedsKMax((i as unknown as LutId));
      }
    }
    let wires = this.$fnum_inputs;
    let rgsws = 0n;
    let cells = this.$fnum_cells;
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        (() => { const __match = op; if (__match instanceof PlanOp_Const) { const out = __match.$fout;
return (() => {
  if (!__equals(out, wires))   {
    return new PlanError_BadReference();
  }
  wires = fieldAdd(wires, 1n);
})(); } else if (__match instanceof PlanOp_Not) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  if (((input >= wires) || !__equals(out, wires)))   {
    return new PlanError_BadReference();
  }
  wires = fieldAdd(wires, 1n);
})(); } else if (__match instanceof PlanOp_Lut) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  if ((BigInt(table) >= BigInt(this.$fluts.length)))   {
    return new PlanError_BadReference();
  }
  const arity = ((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, BigInt(this.$fluts[Number(BigInt(table))].$fentries.length)));
  if (((!__equals(BigInt(inputs.length), BigInt(arity)) || inputs.some((w) => (w >= wires))) || !__equals(out, wires)))   {
    return new PlanError_BadReference();
  }
  wires = fieldAdd(wires, 1n);
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { const input = __match.$finput;
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
    if ((this.$foutputs.some((w) => (w >= wires)) || this.$fcell_outputs.some((c) => (c >= cells))))     {
      return new PlanError_BadOutput();
    }
    const count = this.bootstrap_op_count();
    if ((count > 0n))     {
      const log2_count = fieldSub(64n, ((__value) => { let __bits = 0n; while (__value !== 0n) { __value >>= 1n; __bits += 1n; } return BigInt(64) - __bits; })(BigInt.asUintN(64, count)));
      if (((this.$fbudget.$ftotal_log2 < this.$fbudget.$fper_bootstrap_log2) || (fieldAdd(fieldSub(this.$fbudget.$ftotal_log2, this.$fbudget.$fper_bootstrap_log2), 1n) < log2_count)))       {
        return new PlanError_BudgetInconsistent();
      }
    }
    return undefined;
  }

  workspace_requirements(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }): Result<PlanWorkspaceRequirements, PlanWorkspaceError>
  {
    const __volar_try_0 = this.validate();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    let wires = BigInt(this.$fnum_inputs);
    let rgsws = 0n;
    let cells = BigInt(this.$fnum_cells);
    let lut_inputs = 0n;
    for (const layer of this.$flayers)     {
      for (const op of layer)       {
        (() => { const __match = op; if (true /* pattern Or([Struct { kind: Custom("PlanOp::Const"), fields: [], rest: true }, Struct { kind: Custom("PlanOp::Not"), fields: [], rest: true }, Struct { kind: Custom("PlanOp::Lut"), fields: [], rest: true }]) */) { return (() => {
  wires = (((() => { const __a = wires; const __b = 1n; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new PlanWorkspaceError_CapacityOverflow()));
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { return (() => {
  rgsws = (((() => { const __a = rgsws; const __b = 1n; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new PlanWorkspaceError_CapacityOverflow()));
})(); } else { return (() => {
  cells = (((() => { const __a = cells; const __b = 1n; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new PlanWorkspaceError_CapacityOverflow()));
})(); } })();
        (() => { const __match = op; if (__match instanceof PlanOp_Lut) { const inputs = __match.$finputs;
return (() => {
  lut_inputs = BigInt(Math.max(Number(lut_inputs), Number(BigInt(inputs.length))));
})(); } else { return (() => {
})(); } })();
      }
    }
    return new PlanWorkspaceRequirements({ $fwires: wires, $frgsws: rgsws, $fcells: cells, $flut_inputs: lut_inputs });
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
    const count = BigInt(this.u32());
    if ((count > MAX_ITEMS))     {
      return new DecodeError_TooLarge();
    }
    return count;
  }

  ids(): Result<Vec<bigint>, DecodeError>
  {
    const __volar_try_0 = this.count();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const count = __volar_try_0;
    let ids = ([] as any[]);
    for (let _ = 0n; _ < count; _ += 1n)     {
      (ids).push(this.u32());
    }
    return ids;
  }

  take(count: bigint): Result<bigint[], DecodeError>
  {
    const __volar_try_0 = (((() => { const __a = this.$foffset; const __b = count; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new DecodeError_Truncated()));
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const end = __volar_try_0;
    const __volar_try_1 = (((() => { const __get_array = this.$fbytes; const __get_start = BigInt(this.$foffset); const __get_end = BigInt(end); return __get_array != null && __get_start <= __get_end && __get_end <= BigInt(__get_array.length) ? __get_array.slice(Number(__get_start), Number(__get_end)) : undefined; })()) ?? (new DecodeError_Truncated()));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const bytes = __volar_try_1;
    this.$foffset = end;
    return bytes;
  }

  u32(): Result<bigint, DecodeError>
  {
    const __volar_try_0 = this.take(4n);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const bytes: bigint[] = __volar_try_0;
    return u32_from_le_bytes(bytes);
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

export class WireState {
  $fa0!: bigint;
  $fpi!: boolean;

  constructor(init: {
    $fa0: bigint,
    $fpi: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fa0: __zeroValue(this.$fa0), $fpi: __zeroValue(this.$fpi) }) as this;
  }

  active_label(masked_bit: boolean, delta: any, p: bigint): bigint
  {
    return (() => { if (masked_bit) {
  return (fieldAdd(this.$fa0, delta.value()) % p);
} else {
  return this.$fa0;
} })();
  }
}

export class GlobalDelta {
  $fdelta!: bigint;

  constructor(init: {
    $fdelta: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdelta: __zeroValue(this.$fdelta) }) as this;
  }

  inverse(plaintext_modulus: bigint): (bigint | undefined)
  {
    return inverse_mod(this.$fdelta, plaintext_modulus);
  }

  static new(delta: bigint, plaintext_modulus: bigint): (GlobalDelta | undefined)
  {
    if (((__equals(delta, 0n) || (delta >= plaintext_modulus)) || __equals(fieldBitand(delta, 1n), 0n)))     {
      return undefined;
    }
    return new GlobalDelta({ $fdelta: delta });
  }

  value(): bigint
  {
    return this.$fdelta;
  }
}

export class ExpandedWire {
  $fct_a0!: Ciphertext;
  $fct_pi!: Ciphertext;
  $fslot_a0!: bigint;
  $fslot_pi!: bigint;

  constructor(init: {
    $fct_a0: Ciphertext,
    $fct_pi: Ciphertext,
    $fslot_a0: bigint,
    $fslot_pi: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fct_a0: __zeroValue(this.$fct_a0), $fct_pi: __zeroValue(this.$fct_pi), $fslot_a0: __zeroValue(this.$fslot_a0), $fslot_pi: __zeroValue(this.$fslot_pi) }) as this;
  }
}

export class ExpandedGate {
  $fa!: ExpandedWire;
  $fb!: ExpandedWire;
  $fout!: ExpandedWire;

  constructor(init: {
    $fa: ExpandedWire,
    $fb: ExpandedWire,
    $fout: ExpandedWire
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fa: __zeroValue(this.$fa), $fb: __zeroValue(this.$fb), $fout: __zeroValue(this.$fout) }) as this;
  }
}

export class Circuit {
  $finputs!: bigint;
  $fgates!: Vec<Gate>;
  $foutputs!: Vec<bigint>;

  constructor(init: {
    $finputs: bigint,
    $fgates: Vec<Gate>,
    $foutputs: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $finputs: __zeroValue(this.$finputs), $fgates: __zeroValue(this.$fgates), $foutputs: __zeroValue(this.$foutputs) }) as this;
  }

  eval_plain(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, inputs: boolean[]): Result<Vec<boolean>, StitchError>
  {
    if (!__equals(BigInt(inputs.length), this.$finputs))     {
      return new StitchError_WrongVariant();
    }
    const __volar_try_1 = this.validate();
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    let values = Array.from({length: Number(this.wires())}, () => false);
    (values.slice(0, Number(this.$finputs))).splice(0, (inputs).length, ...(inputs));
    for (const [i, gate] of this.$fgates.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const [a, b] = gate.operands();
      values[Number(fieldAdd(this.$finputs, i))] = (() => { const __match = gate; if (__match instanceof Gate_And) { return fieldBitand(values[Number(a)], values[Number(b)]); } else if (__match instanceof Gate_Xor) { return fieldBitxor(values[Number(a)], values[Number(b)]); } else { return !values[Number(a)]; } })();
    }
    return this.$foutputs.map((o: any) => values[Number(o)]);
  }

  validate(): Result<void, StitchError>
  {
    const wires = this.wires();
    for (const [i, gate] of this.$fgates.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const out = fieldAdd(this.$finputs, i);
      const [a, b] = gate.operands();
      if ((((a >= out) || (b >= out)) || (out >= wires)))       {
        return new StitchError_WrongVariant();
      }
    }
    if (this.$foutputs.some((o) => (o >= wires)))     {
      return new StitchError_WrongVariant();
    }
    return undefined;
  }

  wires(): bigint
  {
    return fieldAdd(this.$finputs, BigInt(this.$fgates.length));
  }
}

export class ReferenceEquation {
  $fxor_charged!: boolean;

  constructor(init: {
    $fxor_charged: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fxor_charged: __zeroValue(this.$fxor_charged) }) as this;
  }

  complement(ctx: any, x: any): Result<Ciphertext, StitchError>
  {
    const p = ctx.parameters().$fplaintext_modulus;
    const __volar_try_1 = ctx.mul_plain(x, ctx.constant_slots(fieldSub(p, 1n)));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const neg = __volar_try_1;
    return ctx.add_plain(neg, ctx.constant_slots(1n));
  }

  eval_and(ctx: any, la: any, lb: any, expanded: any, delta: any, ksk: any, stitch: any): Result<Ciphertext, StitchError>
  {
    let __volar_match_result_0 ;
    const __volar_match_0 = [this.$fxor_charged, stitch];
    if (true /* pattern Tuple([Lit(Bool(true)), TupleStruct { kind: Custom("Stitch::OneBit"), elems: [Ident { mutable: false, name: "b", subpat: None }] }]) */) {
      const b = __volar_match_0[1]._0;
      __volar_match_result_0 = b;
    }
    if (true /* pattern Tuple([Lit(Bool(false)), Ident { mutable: false, name: "s", subpat: Some(TupleStruct { kind: Custom("Stitch::FiveBits"), elems: [Wild] }) }]) */) {
      const s = __volar_match_0[1];
      __volar_match_result_0 = s.check_five();
    }
    {
      return new StitchError_WrongVariant();
    }
    const correction = __volar_match_result_0;
    const k_max = (() => { if (this.$fxor_charged) {
  return 1n;
} else {
  return MAX_PAYLOAD;
} })();
    const __volar_try_2 = this.unwrap(ctx, la, expanded.$fa, delta);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const m_a = __volar_try_2;
    const __volar_try_3 = this.unwrap(ctx, lb, expanded.$fb, delta);
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const m_b = __volar_try_3;
    const __volar_try_4 = this.parity(ctx, m_a, k_max, ksk);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const b_a = __volar_try_4;
    const __volar_try_5 = this.parity(ctx, m_b, k_max, ksk);
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const b_b = __volar_try_5;
    const __volar_try_6 = this.xor_bits(ctx, b_a, expanded.$fa.$fct_pi, ksk);
    if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
    const x_a = __volar_try_6;
    const __volar_try_7 = this.xor_bits(ctx, b_b, expanded.$fb.$fct_pi, ksk);
    if (__volar_try_7 instanceof __VolarError) return __volar_try_7;
    const x_b = __volar_try_7;
    const top = ctx.max_level();
    const __volar_try_9 = ctx.switch_to(x_a, top);
    if (__volar_try_9 instanceof __VolarError) return __volar_try_9;
    const x_a_1 = __volar_try_9;
    const __volar_try_10 = ctx.switch_to(x_b, top);
    if (__volar_try_10 instanceof __VolarError) return __volar_try_10;
    const x_b_1 = __volar_try_10;
    const __volar_try_11 = ctx.switch_to(ctx.mul(x_a_1, x_b_1, ksk), top);
    if (__volar_try_11 instanceof __VolarError) return __volar_try_11;
    const v_ab = __volar_try_11;
    const __volar_try_12 = this.xor_bits(ctx, v_ab, expanded.$fout.$fct_pi, ksk);
    if (__volar_try_12 instanceof __VolarError) return __volar_try_12;
    let b_c = __volar_try_12;
    if (correction)     {
      b_c = this.complement(ctx, b_c);
    }
    return this.wrap(ctx, b_c, expanded.$fout, delta, ctx.max_level());
  }

  eval_xor(ctx: any, la: any, lb: any, expanded: any, delta: any, ksk: any, stitch: (Stitch | undefined)): Result<Ciphertext, StitchError>
  {
    return (() => { if (this.$fxor_charged) {
  let __volar_match_result_0 ;
  const __volar_match_0 = stitch;
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    const b = __volar_match_0._0;
    __volar_match_result_0 = b;
  }
  {
    return new StitchError_WrongVariant();
  }
  const bit = __volar_match_result_0;
  const __volar_try_1 = this.unwrap(ctx, la, expanded.$fa, delta);
  if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
  const b_a = __volar_try_1;
  const __volar_try_2 = this.unwrap(ctx, lb, expanded.$fb, delta);
  if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
  const b_b = __volar_try_2;
  const __volar_try_3 = this.xor_bits(ctx, b_a, b_b, ksk);
  if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
  let b_c = __volar_try_3;
  if (bit)   {
    b_c = this.complement(ctx, b_c);
  }
  return this.wrap(ctx, b_c, expanded.$fout, delta, ctx.max_level());
} else {
  if ((stitch) != null)   {
    return new StitchError_WrongVariant();
  }
  return ctx.add(la, lb);
} })();
  }

  garble_and(gate_index: bigint, a: any, b: any, out: any): Stitch
  {
    const bit = __equals(fieldBitand(fieldShr(wrappingAdd(wrappingAdd(wrappingAdd(BigInt(Math.imul(Number(BigInt(gate_index)), Number(2654435769n))), a.$fa0, 32), b.$fa0, 32), BigInt(out.$fpi), 32), 63n), 1n), 1n);
    return (() => { if (this.$fxor_charged) {
  return new Stitch_OneBit(bit);
} else {
  return five(bit);
} })();
  }

  garble_xor(p: bigint, a: any, b: any, out: any): (Stitch | undefined)
  {
    return (() => { if (this.$fxor_charged) {
  return new Stitch_OneBit(fieldBitxor(fieldBitxor(a.$fpi, b.$fpi), out.$fpi));
} else {
  return undefined;
} })();
  }

  parity(ctx: any, m: any, k_max: bigint, ksk: any): Result<Ciphertext, StitchError>
  {
    if (__equals(k_max, 1n))     {
      return __clone(m);
    }
    const p = ctx.parameters().$fplaintext_modulus;
    const __volar_try_2 = parity_coeffs(p, k_max);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const coeffs = __volar_try_2;
    const level = m.level();
    const __volar_try_4 = ctx.mul_plain(ctx.encrypt_one_like(m), ctx.constant_slots(coeffs[Number(k_max)]));
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    let acc = __volar_try_4;
    for (const i of (Array.from({length: Number(k_max - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      acc = ctx.switch_to(ctx.mul(acc, m, ksk), level);
      if (!__equals(coeffs[Number(i)], 0n))       {
        acc = ctx.add_plain(acc, ctx.constant_slots(coeffs[Number(i)]));
      }
    }
    return acc;
  }

  unwrap(ctx: any, label: any, expanded: any, delta: any): Result<Ciphertext, StitchError>
  {
    const p = ctx.parameters().$fplaintext_modulus;
    const __volar_try_1 = ((delta.inverse(p)) ?? (new StitchError_NonCanonicalBit()));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const inv = __volar_try_1;
    const __volar_try_2 = ctx.switch_to(expanded.$fct_a0, label.level());
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const ct_a0 = __volar_try_2;
    const __volar_try_3 = ctx.sub(label, ct_a0);
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const diff = __volar_try_3;
    return ctx.mul_plain(diff, ctx.constant_slots(inv));
  }

  wrap(ctx: any, masked: any, expanded: any, delta: any, online_base: bigint): Result<Ciphertext, StitchError>
  {
    const __volar_try_0 = ctx.mul_plain(masked, ctx.constant_slots(delta.value()));
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const scaled = __volar_try_0;
    const __volar_try_1 = ctx.switch_to(scaled, online_base);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const scaled_1 = __volar_try_1;
    const __volar_try_2 = ctx.switch_to(expanded.$fct_a0, online_base);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const ct_a0 = __volar_try_2;
    return ctx.add(scaled_1, ct_a0);
  }

  xor_bits(ctx: any, a: any, b: any, ksk: any): Result<Ciphertext, StitchError>
  {
    const top = ctx.max_level();
    const __volar_try_1 = ctx.switch_to(a, top);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const a_1 = __volar_try_1;
    const __volar_try_2 = ctx.switch_to(b, top);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const b_1 = __volar_try_2;
    const level = top;
    const __volar_try_4 = ctx.add(a_1, b_1);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const sum = __volar_try_4;
    const __volar_try_5 = ctx.mul(a_1, b_1, ksk);
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const product = __volar_try_5;
    const __volar_try_6 = ctx.mul_plain(product, ctx.constant_slots(2n));
    if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
    const doubled = __volar_try_6;
    const __volar_try_7 = ctx.sub(sum, doubled);
    if (__volar_try_7 instanceof __VolarError) return __volar_try_7;
    const out = __volar_try_7;
    return ctx.switch_to(out, level);
  }

  xor_is_charged(): boolean
  {
    return this.$fxor_charged;
  }
}

export class LatticeRowCt {
  $fu1!: RqPoly;
  $fu2!: RqPoly;

  constructor(init: {
    $fu1: RqPoly,
    $fu2: RqPoly
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu1: __zeroValue(this.$fu1), $fu2: __zeroValue(this.$fu2) }) as this;
  }
}

export class LatticeDd {
  $fn!: bigint;
  $fp!: bigint;
  $fq!: bigint;
  $fdelta!: RpPoly;
  $fdelta_pinned!: Delta;
  $ftriple!: [RqPoly, RqPoly, RqPoly];
  $ftriple_rp!: [RpPoly, RpPoly, RpPoly];
  $fntt_q!: (Ntt | undefined);
  $fntt_p!: (Ntt | undefined);

  constructor(init: {
    $fn: bigint,
    $fp: bigint,
    $fq: bigint,
    $fdelta: RpPoly,
    $fdelta_pinned: Delta,
    $ftriple: [RqPoly, RqPoly, RqPoly],
    $ftriple_rp: [RpPoly, RpPoly, RpPoly],
    $fntt_q: (Ntt | undefined),
    $fntt_p: (Ntt | undefined)
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fp: __zeroValue(this.$fp), $fq: __zeroValue(this.$fq), $fdelta: __zeroValue(this.$fdelta), $fdelta_pinned: __zeroValue(this.$fdelta_pinned), $ftriple: __zeroValue(this.$ftriple), $ftriple_rp: __zeroValue(this.$ftriple_rp), $fntt_q: __zeroValue(this.$fntt_q), $fntt_p: __zeroValue(this.$fntt_p) }) as this;
  }

  dd0(a: any, tau: any): LabelPoly
  {
    return this.mul_p(tau.$fc1, a);
  }

  dd0_raw(a: any, tau: any): RqPoly
  {
    const a_q: RqPoly = a.map((c: any) => (c % this.$fq));
    return this.mul_q(tau.$fu2, a_q);
  }

  dd_i(a: any, i: boolean, tau: any): LabelPoly
  {
    const base = dd0(this, a, tau);
    return (() => { if (i) {
  return add_coeffs(base, tau.$fc0, this.$fp);
} else {
  return base;
} })();
  }

  dd_ij(a: any, i: boolean, b: any, j: boolean, tau: any): LabelPoly
  {
    const share = this.hss_mul_rp(a, i, b, j);
    const base = this.mul_p(tau.$fc1, share);
    return (() => { if ((i && j)) {
  return add_coeffs(base, tau.$fc0, this.$fp);
} else {
  return base;
} })();
  }

  dec_row(tau: any): RpPoly
  {
    const q = this.$fq;
    const delta_q: RqPoly = this.$fdelta.map((c: any) => (() => {
  return (() => { if (__equals(c, 0n)) {
  return 0n;
} else if (__equals(c, 1n)) {
  return 1n;
} else {
  return fieldSub(q, 1n);
} })();
})());
    const u2d = this.mul_q(tau.$fu2, delta_q);
    const phase = add_rq(tau.$fu1, u2d, q);
    return round_to_rp(phase, q, this.$fp);
  }

  degree(): bigint
  {
    return this.$fn;
  }

  delta(): Delta
  {
    return this.$fdelta_pinned;
  }

  eval(a: any, i: boolean, b: any, j: boolean, taus: RowCt[]): LabelPoly
  {
    let out = dd_i(this, a, i, taus[Number(0n)]);
    out = add_coeffs(out, dd_i(this, b, j, taus[Number(1n)]), this.$fp);
    return add_coeffs(out, dd_ij(this, a, i, b, j, taus[Number(2n)]), this.$fp);
  }

  hss_mul(a: any, i: boolean, b: any, j: boolean): RpPoly
  {
    const q = this.$fq;
    const [u, v, w] = this.$ftriple;
    const a_q: RqPoly = a.map((c: any) => (c % q));
    const b_q: RqPoly = b.map((c: any) => (c % q));
    const ab = this.mul_q(a_q, b_q);
    let acc = this.mul_q(ab, u);
    if (i)     {
      const bv = this.mul_q(b_q, v);
      acc = add_rq(acc, bv, q);
    }
    if (j)     {
      const av = this.mul_q(a_q, v);
      acc = add_rq(acc, av, q);
    }
    if ((i && j))     {
      acc = sub_rq(acc, w, q);
    }
    return round_to_rp(acc, q, this.$fp);
  }

  hss_mul_raw(a: any, i: boolean, b: any, j: boolean): RqPoly
  {
    const q = this.$fq;
    const [u, v, w] = this.$ftriple;
    const a_q: RqPoly = a.map((c: any) => (c % q));
    const b_q: RqPoly = b.map((c: any) => (c % q));
    const ab = this.mul_q(a_q, b_q);
    let acc = this.mul_q(ab, u);
    if (i)     {
      const bv = this.mul_q(b_q, v);
      acc = add_rq(acc, bv, q);
    }
    if (j)     {
      const av = this.mul_q(a_q, v);
      acc = add_rq(acc, av, q);
    }
    if ((i && j))     {
      acc = sub_rq(acc, w, q);
    }
    return acc;
  }

  hss_mul_rp(a: any, i: boolean, b: any, j: boolean): RpPoly
  {
    const p = this.$fp;
    const [u, v, w] = this.$ftriple_rp;
    const ab = this.mul_p(a, b);
    let out = this.mul_p(ab, u);
    if (i)     {
      const bv = this.mul_p(b, v);
      out = add_coeffs(out, bv, p);
    }
    if (j)     {
      const av = this.mul_p(a, v);
      out = add_coeffs(out, av, p);
    }
    if ((i && j))     {
      out = sub_coeffs(out, w, p);
    }
    return out;
  }

  mul_p(a: bigint[], b: bigint[]): RpPoly
  {
    return (() => { const __match = this.$fntt_p; if (__match !== null && __match !== undefined) { const ntt = __match;
return negacyclic_mul_ntt(a, b, ntt); } else { return negacyclic_mul_coeffs(a, b, this.$fp); } })();
  }

  mul_q(a: bigint[], b: bigint[]): RqPoly
  {
    return (() => { const __match = this.$fntt_q; if (__match !== null && __match !== undefined) { const ntt = __match;
return negacyclic_mul_ntt(a, b, ntt); } else { return negacyclic_mul_rq(a, b, this.$fq); } })();
  }

  static new(n: bigint, p: bigint, q: bigint, random: any): Result<LatticeDd, LatticeError>
  {
    if (((((__equals(n, 0n) || (p < 3n)) || __equals((p % 2n), 0n)) || !__equals((q % p), 0n)) || __equals((q % 2n), 0n)))     {
      return new LatticeError_InvalidParameters();
    }
    let delta = Array.from({length: Number(n)}, () => 0n);
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(delta.length); __zip_mut_2 += 1n) {
      {
        const r = (random.next_u32() % 3n);
        delta[Number(__zip_mut_2)] = (() => { const __match = r; if (__match === 0n) { return 0n; } else if (__match === 1n) { return 1n; } else { return fieldSub(p, 1n); } })();
      }}
    delta[Number(0n)] = 1n;
    const delta_q: RqPoly = delta.map((c: any) => (() => {
  return (() => { if (__equals(c, 0n)) {
  return 0n;
} else if (__equals(c, 1n)) {
  return 1n;
} else {
  return fieldSub(q, 1n);
} })();
})());
    const u = sample_uniform_rq(n, q, random);
    const ud = negacyclic_mul_rq(u, delta_q, q);
    const e1 = sample_noise_rq(n, p, q, random);
    const v = add_rq(negate_rq(ud, q), e1, q);
    const d2_q = negacyclic_mul_rq(delta_q, delta_q, q);
    const ud2 = negacyclic_mul_rq(u, d2_q, q);
    const e2 = sample_noise_rq(n, p, q, random);
    const neg_delta_q = negate_rq(delta_q, q);
    const w = add_rq(sub_rq(neg_delta_q, ud2, q), e2, q);
    const red = (poly: RqPoly) => (() => {
  return poly.map((c: any) => (c % p));
})();
    const triple_rp = [red(u), red(v), red(w)];
    const delta_pinned = new DeltaLoweredDyn({ $fcoeffs: __clone(delta), $fp: p, $fn: 0n });
    const ntt_q = Ntt.new(n, q).ok();
    const ntt_p = Ntt.new(n, p).ok();
    return new LatticeDd({ $fn: n, $fp: p, $fq: q, $fdelta: delta, $fdelta_pinned: delta_pinned, $ftriple: [u, v, w], $ftriple_rp: triple_rp, $fntt_q: ntt_q, $fntt_p: ntt_p });
  }

  p(): bigint
  {
    return this.$fp;
  }

  q(): bigint
  {
    return this.$fq;
  }

  row(t: bigint, random: any): RowCt
  {
    const tau = LatticeDd.row(this, t, random);
    return rowct_from_lattice(tau, this.$fp);
  }
}

export class Rns2 {
  $fl0!: Limb;
  $fl1!: Limb;

  constructor(init: {
    $fl0: Limb,
    $fl1: Limb
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fl0: __zeroValue(this.$fl0), $fl1: __zeroValue(this.$fl1) }) as this;
  }
}

export class RnsRowCt {
  $fu1!: Rns2;
  $fu2!: Rns2;

  constructor(init: {
    $fu1: Rns2,
    $fu2: Rns2
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu1: __zeroValue(this.$fu1), $fu2: __zeroValue(this.$fu2) }) as this;
  }
}

export class LatticeRnsDd {
  $fn!: bigint;
  $fp!: bigint;
  $fq0!: bigint;
  $fq1!: bigint;
  $fdelta!: LabelPoly;
  $fdelta_pinned!: Delta;
  $ftriple!: [Rns2, Rns2, Rns2];
  $ftriple_rp!: [LabelPoly, LabelPoly, LabelPoly];
  $fntt0!: (Ntt | undefined);
  $fntt1!: (Ntt | undefined);
  $fntt_p!: (Ntt | undefined);

  constructor(init: {
    $fn: bigint,
    $fp: bigint,
    $fq0: bigint,
    $fq1: bigint,
    $fdelta: LabelPoly,
    $fdelta_pinned: Delta,
    $ftriple: [Rns2, Rns2, Rns2],
    $ftriple_rp: [LabelPoly, LabelPoly, LabelPoly],
    $fntt0: (Ntt | undefined),
    $fntt1: (Ntt | undefined),
    $fntt_p: (Ntt | undefined)
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fp: __zeroValue(this.$fp), $fq0: __zeroValue(this.$fq0), $fq1: __zeroValue(this.$fq1), $fdelta: __zeroValue(this.$fdelta), $fdelta_pinned: __zeroValue(this.$fdelta_pinned), $ftriple: __zeroValue(this.$ftriple), $ftriple_rp: __zeroValue(this.$ftriple_rp), $fntt0: __zeroValue(this.$fntt0), $fntt1: __zeroValue(this.$fntt1), $fntt_p: __zeroValue(this.$fntt_p) }) as this;
  }

  dd0(a: any, tau: any): LabelPoly
  {
    const [p, q0] = [this.$fp, this.$fq0];
    const a_q0: Limb = a.map((c: any) => (c % q0));
    const prod0 = mul_limb(tau.$fu2.$fl0, a_q0, q0, this.$fntt0);
    return round_limb0_to_rp(prod0, q0, p);
  }

  dd_i(a: any, i: boolean, tau: any): LabelPoly
  {
    const [p, q0] = [this.$fp, this.$fq0];
    const a_q0: Limb = a.map((c: any) => (c % q0));
    let acc0 = mul_limb(tau.$fu2.$fl0, a_q0, q0, this.$fntt0);
    if (i)     {
      acc0 = add_limb(acc0, tau.$fu1.$fl0, q0);
    }
    return round_limb0_to_rp(acc0, q0, p);
  }

  dd_ij(a: any, i: boolean, b: any, j: boolean, tau: any): LabelPoly
  {
    const [p, q0] = [this.$fp, this.$fq0];
    const share = this.hss_mul_rp(a, i, b, j);
    const share_q0: Limb = share.map((c: any) => (c % q0));
    let acc0 = mul_limb(tau.$fu2.$fl0, share_q0, q0, this.$fntt0);
    if ((i && j))     {
      acc0 = add_limb(acc0, tau.$fu1.$fl0, q0);
    }
    return round_limb0_to_rp(acc0, q0, p);
  }

  dec_row(tau: any): LabelPoly
  {
    const q0 = this.$fq0;
    const dq0 = to_limb(this.$fdelta, this.$fp, q0);
    const u2d0 = mul_limb(tau.$fu2.$fl0, dq0, q0, this.$fntt0);
    const phase0 = add_limb(tau.$fu1.$fl0, u2d0, q0);
    return round_limb0_to_rp(phase0, q0, this.$fp);
  }

  degree(): bigint
  {
    return this.$fn;
  }

  delta(): LabelPoly
  {
    return this.$fdelta;
  }

  delta_pinned_ref(): Delta
  {
    return this.$fdelta_pinned;
  }

  eval(a: any, i: boolean, b: any, j: boolean, taus: RnsRowCt[]): LabelPoly
  {
    const p = this.$fp;
    let out = this.dd_i(a, i, taus[Number(0n)]);
    out = add_coeffs(out, this.dd_i(b, j, taus[Number(1n)]), p);
    return add_coeffs(out, this.dd_ij(a, i, b, j, taus[Number(2n)]), p);
  }

  hss_mul_rp(a: any, i: boolean, b: any, j: boolean): LabelPoly
  {
    const p = this.$fp;
    const [u, v, w] = this.$ftriple_rp;
    const ab = this.mul_p(a, b);
    let out = this.mul_p(ab, u);
    if (i)     {
      const bv = this.mul_p(b, v);
      out = add_coeffs(out, bv, p);
    }
    if (j)     {
      const av = this.mul_p(a, v);
      out = add_coeffs(out, av, p);
    }
    if ((i && j))     {
      out = sub_coeffs(out, w, p);
    }
    return out;
  }

  modulus_bits(): bigint
  {
    return fieldSub(fieldAdd(fieldSub(64n, ((__value) => { let __bits = 0n; while (__value !== 0n) { __value >>= 1n; __bits += 1n; } return BigInt(64) - __bits; })(BigInt.asUintN(64, this.$fq0))), fieldSub(64n, ((__value) => { let __bits = 0n; while (__value !== 0n) { __value >>= 1n; __bits += 1n; } return BigInt(64) - __bits; })(BigInt.asUintN(64, this.$fq1)))), 2n);
  }

  mul_p(a: bigint[], b: bigint[]): LabelPoly
  {
    return (() => { const __match = this.$fntt_p; if (__match !== null && __match !== undefined) { const ntt = __match;
return negacyclic_mul_ntt(a, b, ntt); } else { return negacyclic_mul_coeffs(a, b, this.$fp); } })();
  }

  static new(n: bigint, p: bigint, q0: bigint, q1: bigint, random: any): Result<LatticeRnsDd, RnsError>
  {
    if (((((__equals(n, 0n) || (p < 3n)) || __equals((p % 2n), 0n)) || !__equals((q0 % p), 0n)) || __equals(q1, q0)))     {
      return new RnsError_InvalidParameters();
    }
    let delta = Array.from({length: Number(n)}, () => 0n);
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(delta.length); __zip_mut_2 += 1n) {
      {
        const r = (random.next_u32() % 3n);
        delta[Number(__zip_mut_2)] = (() => { const __match = r; if (__match === 0n) { return 0n; } else if (__match === 1n) { return 1n; } else { return fieldSub(p, 1n); } })();
      }}
    delta[Number(0n)] = 1n;
    const dq0 = to_limb(delta, p, q0);
    const dq1 = to_limb(delta, p, q1);
    const u0 = uniform_limb(n, q0, random);
    const u1 = uniform_limb(n, q1, random);
    const ud0 = mul_limb(u0, dq0, q0, ntt_ctx(n, q0));
    const ud1 = mul_limb(u1, dq1, q1, ntt_ctx(n, q1));
    const v0 = add_limb(negate_limb(ud0, q0), noise_limb(n, p, q0, random), q0);
    const v1 = add_limb(negate_limb(ud1, q1), noise_limb(n, p, q1, random), q1);
    const d2q0 = mul_limb(dq0, dq0, q0, ntt_ctx(n, q0));
    const d2q1 = mul_limb(dq1, dq1, q1, ntt_ctx(n, q1));
    const ud20 = mul_limb(u0, d2q0, q0, ntt_ctx(n, q0));
    const ud21 = mul_limb(u1, d2q1, q1, ntt_ctx(n, q1));
    const w0 = add_limb(sub_limb(negate_limb(dq0, q0), ud20, q0), noise_limb(n, p, q0, random), q0);
    const w1 = add_limb(sub_limb(negate_limb(dq1, q1), ud21, q1), noise_limb(n, p, q1, random), q1);
    const red = (limb: Limb) => (() => {
  return limb.map((c: any) => (c % p));
})();
    const triple_rp = [red(u0), red(v0), red(w0)];
    const delta_pinned = new DeltaLoweredDyn({ $fcoeffs: __clone(delta), $fp: p, $fn: 0n });
    return new LatticeRnsDd({ $fn: n, $fp: p, $fq0: q0, $fq1: q1, $fdelta: delta, $fdelta_pinned: delta_pinned, $ftriple: [new Rns2({ $fl0: u0, $fl1: u1 }), new Rns2({ $fl0: v0, $fl1: v1 }), new Rns2({ $fl0: w0, $fl1: w1 })], $ftriple_rp: triple_rp, $fntt0: Ntt.new(n, q0).ok(), $fntt1: Ntt.new(n, q1).ok(), $fntt_p: Ntt.new(n, p).ok() });
  }

  p(): bigint
  {
    return this.$fp;
  }

  row(t: bigint, random: any): RnsRowCt
  {
    const [p, q0, q1] = [this.$fp, this.$fq0, this.$fq1];
    const u2 = new Rns2({ $fl0: uniform_limb(this.$fn, q0, random), $fl1: uniform_limb(this.$fn, q1, random) });
    const dq0 = to_limb(this.$fdelta, p, q0);
    const dq1 = to_limb(this.$fdelta, p, q1);
    const u2d0 = mul_limb(u2.$fl0, dq0, q0, this.$fntt0);
    const u2d1 = mul_limb(u2.$fl1, dq1, q1, this.$fntt1);
    const td0 = scale_limb(dq0, t, q0);
    const td1 = scale_limb(dq1, t, q1);
    const e0 = noise_limb(this.$fn, p, q0, random);
    const e1 = noise_limb(this.$fn, p, q1, random);
    return new RnsRowCt({ $fu1: new Rns2({ $fl0: add_limb(sub_limb(td0, u2d0, q0), e0, q0), $fl1: add_limb(sub_limb(td1, u2d1, q1), e1, q1) }), $fu2: u2 });
  }
}

export class DeltaLoweredDyn {
  $fn!: bigint;
  $fcoeffs!: LabelPoly;
  $fp!: bigint;

  constructor(init: {
    $fn: bigint,
    $fcoeffs: LabelPoly,
    $fp: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fcoeffs: __zeroValue(this.$fcoeffs), $fp: __zeroValue(this.$fp) }) as this;
  }

  clone<T>(): DeltaLoweredDyn
  {
    const n: bigint = this.$fn;
    const { $fdelta: delta } = this;
    return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(delta[Number(i)])), $fn: 0n });
  }

  eq<T>(other: any): boolean
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
    return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(delta[Number((f(i) % n))])), $fn: 0n });
  }

  rotate_left(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a: bigint) => wrappingSub(a, n_param, 64));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a: bigint) => wrappingAdd(a, n_param, 64));
  }

  static_<U, O>(val: any[]): QDyn<any>
  {
    const n: bigint = this.$fn;
    return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(val[Number(i)]), __clone(this.$fdelta[Number(i)]))), $fn: 0n });
  }

  bit(n_param: bigint): DeltaLoweredDyn<Bit>
  {
    if (/* T is BitsInBytes */ true) {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  return new Bit(!__equals(fieldBitand(fieldShr(b, n_param), 1n), 0n));
})()), $fn: 0n });
    }
  }

  rotate_left_bits(n_param: bigint): DeltaLoweredDyn
  {
    if (/* T is BitsInBytes */ true) {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  const next = __clone(delta[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(8n, BigInt(n_param)))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(delta[Number(i)]);
  const next = __clone(delta[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(64n, BigInt(n_param)))));
})()), $fn: 0n });
    }
  }

  rotate_right_bits(n_param: bigint): DeltaLoweredDyn
  {
    if (/* T is BitsInBytes */ true) {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(delta[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(delta[Number(i)]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fdelta: delta } = this;
      return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(delta[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(delta[Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})()), $fn: 0n });
    }
  }
}

export class WireLabel {
  $fw0!: LabelPoly;
  $fpi!: boolean;

  constructor(init: {
    $fw0: LabelPoly,
    $fpi: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fw0: __zeroValue(this.$fw0), $fpi: __zeroValue(this.$fpi) }) as this;
  }

  active(masked: boolean, delta: any): LabelPoly
  {
    const base = this.base(delta);
    return (() => { if (masked) {
  return add_coeffs(base, delta.$fcoeffs, delta.$fp);
} else {
  return base;
} })();
  }

  base(delta: any): LabelPoly
  {
    return (() => { if (this.$fpi) {
  return add_coeffs(this.$fw0, delta.$fcoeffs, delta.$fp);
} else {
  return __clone(this.$fw0);
} })();
  }

  w0_lsb(): boolean
  {
    return __equals((this.$fw0[Number(0n)] % 2n), 1n);
  }
}

export class RowCt {
  $fc0!: LabelPoly;
  $fc1!: LabelPoly;

  constructor(init: {
    $fc0: LabelPoly,
    $fc1: LabelPoly
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fc0: __zeroValue(this.$fc0), $fc1: __zeroValue(this.$fc1) }) as this;
  }
}

export class ReferenceDd {
  $fp!: bigint;
  $fdelta!: Delta;
  $ftriple!: [LabelPoly, LabelPoly, LabelPoly];

  constructor(init: {
    $fp: bigint,
    $fdelta: Delta,
    $ftriple: [LabelPoly, LabelPoly, LabelPoly]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fp: __zeroValue(this.$fp), $fdelta: __zeroValue(this.$fdelta), $ftriple: __zeroValue(this.$ftriple) }) as this;
  }

  dd0(a: any, tau: any): LabelPoly
  {
    return negacyclic_mul_coeffs(tau.$fc1, a, this.$fp);
  }

  dd_i(a: any, i: boolean, tau: any): LabelPoly
  {
    const base = this.dd0(a, tau);
    return (() => { if (i) {
  return add_coeffs(base, tau.$fc0, this.$fp);
} else {
  return base;
} })();
  }

  dd_ij(a: any, i: boolean, b: any, j: boolean, tau: any): LabelPoly
  {
    const share = this.hss_mul(a, i, b, j);
    const base = negacyclic_mul_coeffs(tau.$fc1, share, this.$fp);
    return (() => { if ((i && j)) {
  return add_coeffs(base, tau.$fc0, this.$fp);
} else {
  return base;
} })();
  }

  dec_row(tau: any): LabelPoly
  {
    const c1d = negacyclic_mul_coeffs(tau.$fc1, this.$fdelta.$fcoeffs, this.$fp);
    return add_coeffs(tau.$fc0, c1d, this.$fp);
  }

  delta(): Delta
  {
    return this.$fdelta;
  }

  eval(a: any, i: boolean, b: any, j: boolean, taus: RowCt[]): LabelPoly
  {
    let out = this.dd_i(a, i, taus[Number(0n)]);
    out = add_coeffs(out, this.dd_i(b, j, taus[Number(1n)]), this.$fp);
    return add_coeffs(out, this.dd_ij(a, i, b, j, taus[Number(2n)]), this.$fp);
  }

  hss_mul(a: any, i: boolean, b: any, j: boolean): LabelPoly
  {
    const p = this.$fp;
    const [u, v, w] = this.$ftriple;
    const ab = negacyclic_mul_coeffs(a, b, p);
    let out = negacyclic_mul_coeffs(ab, u, p);
    if (i)     {
      const bv = negacyclic_mul_coeffs(b, v, p);
      out = add_coeffs(out, bv, p);
    }
    if (j)     {
      const av = negacyclic_mul_coeffs(a, v, p);
      out = add_coeffs(out, av, p);
    }
    if ((i && j))     {
      out = sub_coeffs(out, w, p);
    }
    return out;
  }

  static new(delta: any, degree: bigint, random: any): ReferenceDd
  {
    const p = delta.$fp;
    let uniform = () => (() => {
  let v = Array.from({length: Number(degree)}, () => 0n);
  random(v);
  v.for_each((x) => x %= p);
  return v;
})();
    const u = uniform();
    const du = negacyclic_mul_coeffs(delta.$fcoeffs, u, p);
    const v = du.map((x: any) => (() => { if (__equals(x, 0n)) {
  return 0n;
} else {
  return fieldSub(p, x);
} })());
    const d2 = negacyclic_mul_coeffs(delta.$fcoeffs, delta.$fcoeffs, p);
    const d2u = negacyclic_mul_coeffs(d2, u, p);
    const w = sub_coeffs(delta.neg(), d2u, p);
    return new ReferenceDd({ $fp: p, $fdelta: delta, $ftriple: [u, v, w] });
  }

  p(): bigint
  {
    return this.$fp;
  }

  row(t: bigint, random: any): RowCt
  {
    const p = this.$fp;
    const degree = BigInt(this.$fdelta.$fcoeffs.length);
    let c1 = Array.from({length: Number(degree)}, () => 0n);
    random(c1);
    c1.for_each((x) => x %= p);
    const td = (() => { if ((t >= 0n)) {
  return delta_scaled(this.$fdelta, BigInt(t), p);
} else {
  return delta_scaled(this.$fdelta, BigInt(-t), p).map((x: any) => (() => { if (__equals(x, 0n)) {
  return 0n;
} else {
  return fieldSub(p, x);
} })());
} })();
    const c1d = negacyclic_mul_coeffs(c1, this.$fdelta.$fcoeffs, p);
    const c0 = sub_coeffs(td, c1d, p);
    return new RowCt({ $fc0: c0, $fc1: c1 });
  }
}

export class PrfKey {
  [0]!: bigint[];

  constructor(_0: bigint[]) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  offset(wire: bigint, degree: bigint, p: bigint): LabelPoly
  {
    let out = Array.from({length: Number(degree)}, () => 0n);
    let s0 = fieldBitxor(this[0][Number(0n)], BigInt(Math.imul(Number(BigInt(wire)), Number(11400714819323198485n))));
    let s1 = wrappingAdd(this[0][Number(1n)], BigInt(wire), 64);
    let s2 = fieldBitxor(this[0][Number(2n)], this[0][Number(0n)]);
    let s3 = wrappingAdd(this[0][Number(3n)], this[0][Number(1n)], 64);
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(out.length); __zip_mut_2 += 1n) {
      {
        const t = fieldShl(s1, 17n);
        s2 = fieldBitxor(s2, s0);
        s3 = fieldBitxor(s3, s1);
        s1 = fieldBitxor(s1, s2);
        s0 = fieldBitxor(s0, s3);
        s2 = fieldBitxor(s2, t);
        s3 = s3.rotateleft(45n);
        out[Number(__zip_mut_2)] = (wrappingAdd(s3, s0, 64) % p);
      }}
    return out;
  }
}

export class PinnedGarbling {
  $flabels!: Vec<WireLabel>;
  $fdelta_ct!: RowCt;
  $fand_rows!: Vec<AndRows>;
  $frecords!: Vec<GateRecord>;

  constructor(init: {
    $flabels: Vec<WireLabel>,
    $fdelta_ct: RowCt,
    $fand_rows: Vec<AndRows>,
    $frecords: Vec<GateRecord>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flabels: __zeroValue(this.$flabels), $fdelta_ct: __zeroValue(this.$fdelta_ct), $fand_rows: __zeroValue(this.$fand_rows), $frecords: __zeroValue(this.$frecords) }) as this;
  }
}

export class ReferencePrg {
  $fslots!: bigint;

  constructor(init: {
    $fslots: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fslots: __zeroValue(this.$fslots) }) as this;
  }

  depth(): bigint
  {
    return 1n;
  }

  expand(ctx: any, seed: any, ksk: any): Result<Vec<Ciphertext>, Error>
  {
    const degree = ctx.parameters().$fdegree;
    if ((this.$fslots > degree))     {
      return new Error_InvalidParameters();
    }
    const __volar_try_2 = ctx.mul(seed, seed, ksk);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const s2 = __volar_try_2;
    let out = ([] as any[]);
    for (let i = 0n; i < this.$fslots; i += 1n)     {
      let mask = Array.from({length: Number(degree)}, () => 0n);
      mask[Number(i)] = 1n;
      (out).push(ctx.mul_plain(s2, mask));
    }
    return out;
  }

  static new(slots: bigint): ReferencePrg
  {
    return new ReferencePrg({ $fslots: slots });
  }

  slots(): bigint
  {
    return this.$fslots;
  }
}

export class ExpansionPlan {
  $fseed_ciphertexts!: bigint;
  $fslots_per_seed!: bigint;
  $fdepth!: bigint;
  $frequired_bits!: bigint;

  constructor(init: {
    $fseed_ciphertexts: bigint,
    $fslots_per_seed: bigint,
    $fdepth: bigint,
    $frequired_bits: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fseed_ciphertexts: __zeroValue(this.$fseed_ciphertexts), $fslots_per_seed: __zeroValue(this.$fslots_per_seed), $fdepth: __zeroValue(this.$fdepth), $frequired_bits: __zeroValue(this.$frequired_bits) }) as this;
  }

  static plan(required_bits: bigint, prg_depth: bigint, slots_per_seed: bigint, depth_budget: bigint): Result<ExpansionPlan, Error>
  {
    if ((__equals(slots_per_seed, 0n) || (prg_depth > depth_budget)))     {
      return new Error_InvalidParameters();
    }
    const seed_ciphertexts = ((required_bits) + (slots_per_seed) - 1n) / (slots_per_seed);
    return new ExpansionPlan({ $fseed_ciphertexts: seed_ciphertexts, $fslots_per_seed: slots_per_seed, $fdepth: prg_depth, $frequired_bits: required_bits });
  }

  total_slots(): bigint
  {
    return fieldMul(this.$fseed_ciphertexts, this.$fslots_per_seed);
  }
}

export class Parameters {
  $fdegree!: bigint;
  $fmodulus!: bigint;

  constructor(init: {
    $fdegree: bigint,
    $fmodulus: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdegree: __zeroValue(this.$fdegree), $fmodulus: __zeroValue(this.$fmodulus) }) as this;
  }

  levels(): Result<bigint, Error>
  {
    if (((this.$fwidth < 2n) || !((this.$fwidth) > 0n && ((this.$fwidth) & ((this.$fwidth) - 1n)) === 0n)))     {
      return new Error_InvalidParameters();
    }
    return BigInt(ilog2(this.$fwidth));
  }

  static scaled_reference(degree: bigint, width: bigint): Parameters
  {
    return new Parameters({ $fdegree: degree, $fwidth: width });
  }

  slots(): Result<bigint, Error>
  {
    return (((() => { const __a = this.$fdegree; const __b = this.$fwidth; const __result = __a * __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new Error_InvalidParameters()));
  }

  validate(): Result<void, Error>
  {
    if ((((this.$fdegree < 2n) || !((this.$fdegree) > 0n && ((this.$fdegree) & ((this.$fdegree) - 1n)) === 0n)) || (this.$fmodulus < 3n)))     {
      return new Error_InvalidParameters();
    }
    if (!__equals((fieldSub(this.$fmodulus, 1n) % fieldMul(2n, BigInt(this.$fdegree))), 0n))     {
      return new Error_InvalidParameters();
    }
    return undefined;
  }
}

export class PolyLoweredDyn {
  $fn!: bigint;
  $fslots!: Vec<bigint>;

  constructor(init: {
    $fn: bigint,
    $fslots: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fslots: __zeroValue(this.$fslots) }) as this;
  }

  apply<O>(ctx: { defaultO: () => any }, m: bigint, x: bigint, x2: bigint, xs: bigint, s: bigint, voles: VopeDyn<any>[][]): VopeDyn<any>
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
        for (const [idx, v] of voles.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))         {
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

  apply_pool<O>(ctx: { defaultO: () => any }, m: bigint, x: bigint, x2: bigint, xs: bigint, s: bigint, voles: PolyInputPoolDyn<VopeDyn<any>>): VopeDyn<any>
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
        for (const [idx, v] of voles.$findices.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))         {
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

  get_qs<Q, A>(m: bigint, x: bigint, root: DeltaLoweredDyn<any>, inputs: QDyn<any>[][], reduction: bigint): QDyn<any>
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

  get_qs_pool<Q, A>(m: bigint, x: bigint, root: DeltaLoweredDyn<any>, inputs: PolyInputPoolDyn<QDyn<any>>, reduction: bigint): QDyn<any>
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

export class Ring {
  $fparameters!: Parameters;
  $fntt!: Ntt;

  constructor(init: {
    $fparameters: Parameters,
    $fntt: Ntt
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparameters: __zeroValue(this.$fparameters), $fntt: __zeroValue(this.$fntt) }) as this;
  }

  add(left: any, right: any): Poly
  {
    let out = __clone(left);
    this.add_assign(out, right);
    return out;
  }

  add_assign(left: any, right: any)
  {
    const q = this.$fparameters.$fmodulus;
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(left.$fslots.length); __zip_mut_2 += 1n) {
      const r = right.$fslots[Number(__zip_mut_2)];
      {
        left.$fslots[Number(__zip_mut_2)] = add_mod(left.$fslots[Number(__zip_mut_2)], r, q);
      }}
  }

  add_noise(ctx: { defaultA: () => any }, value: any, noise: any, standard_deviation: bigint, bound: bigint): Result<void, Error>
  {
    let coefficients = Array.from({length: Number(this.$fparameters.$fdegree)}, () => 0n);
    const __volar_try_1 = noise.sample(ctx, coefficients.$fn, standard_deviation, bound, coefficients);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    if (coefficients.some((sample) => (((sample) < 0n ? -(sample) : (sample)) > bound)))     {
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
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(value.$ffirst.length); __zip_mut_2 += 1n) {
      const error = first[Number(__zip_mut_2)];
      {
        value.$ffirst[Number(__zip_mut_2)] = add_mod(value.$ffirst[Number(__zip_mut_2)], error, this.$fparameters.$fplaintext_modulus);
      }}
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(value.$fsecond.length); __zip_mut_2 += 1n) {
      const error = second[Number(__zip_mut_2)];
      {
        value.$fsecond[Number(__zip_mut_2)] = add_mod(value.$fsecond[Number(__zip_mut_2)], error, this.$fparameters.$fdelta);
      }}
    return undefined;
  }

  combine(first: bigint, second: bigint): bigint
  {
    const first_mod_delta = (first % this.$fparameters.$fdelta);
    const offset = mul_mod(sub_mod(second, first_mod_delta, this.$fparameters.$fdelta), this.$finverse_plaintext_mod_delta, this.$fparameters.$fdelta);
    return fieldAdd((first as unknown as bigint), fieldMul((this.$fparameters.$fplaintext_modulus as unknown as bigint), (offset as unknown as bigint)));
  }

  constant(value: bigint): Poly
  {
    return new PolyLoweredDyn({ $fslots: Array.from({length: Number(this.$fparameters.$fdegree)}, () => (value % this.$fparameters.$fmodulus)), $fn: 0n });
  }

  decode_messages(values: Polynomial[]): Result<Vec<bigint>, Error>
  {
    if ((!__equals(BigInt(values.length), this.$fparameters.$fwidth) || values.some((value) => !__equals(value.degree(), this.$fparameters.$fdegree))))     {
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
      for (let __mut_3 = 0n; __mut_3 < BigInt(digits.length); __mut_3 += 1n) {
      {
        const part = BigInt((combined % (this.$fparameters.$fgadget_base as unknown as bigint)));
        digits[Number(__mut_3)].$ffirst[Number(index)] = (part % this.$fparameters.$fplaintext_modulus);
        digits[Number(__mut_3)].$fsecond[Number(index)] = (part % this.$fparameters.$fdelta);
        combined /= (this.$fparameters.$fgadget_base as unknown as bigint);
      }}
      if (!__equals(combined, 0n))       {
        return new Error_Arithmetic();
      }
    }
    for (let __mut_2 = 0n; __mut_2 < BigInt(digits.length); __mut_2 += 1n) {
    {
      this.$ffirst_ntt.forward(digits[Number(__mut_2)].$ffirst);
      this.$fsecond_ntt.forward(digits[Number(__mut_2)].$fsecond);
    }}
    return digits;
  }

  encode_choices(input: boolean[]): Result<Vec<Polynomial>, Error>
  {
    if (!__equals(BigInt(input.length), this.$fparameters.slots()))     {
      return new Error_LengthMismatch();
    }
    let output = ([] as any[]);
    for (const chunk of __chunks(input, Number(this.$fparameters.$fdegree)).filter((__chunk: any) => __chunk.length === Number(this.$fparameters.$fdegree)))     {
      let value = this.zero();
      for (let __zip_mut_3 = 0n; __zip_mut_3 < BigInt(value.$ffirst.length); __zip_mut_3 += 1n) {
        const choice = chunk[Number(__zip_mut_3)];
        {
          const choice_1 = BigInt(choice);
          value.$ffirst[Number(__zip_mut_3)] = choice_1;
          second = choice_1;
        }}
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
    if (input.some((value) => (value >= this.$fparameters.$fplaintext_modulus)))     {
      return new Error_NonCanonicalPlaintext();
    }
    let output = ([] as any[]);
    for (const chunk of __chunks(input, Number(this.$fparameters.$fdegree)).filter((__chunk: any) => __chunk.length === Number(this.$fparameters.$fdegree)))     {
      let value = this.zero();
      for (let __zip_mut_3 = 0n; __zip_mut_3 < BigInt(value.$ffirst.length); __zip_mut_3 += 1n) {
        const message = chunk[Number(__zip_mut_3)];
        {
          value.$ffirst[Number(__zip_mut_3)] = mul_mod(message, (this.$fparameters.$fdelta % this.$fparameters.$fplaintext_modulus), this.$fparameters.$fplaintext_modulus);
        }}
      (output).push(value);
    }
    return output;
  }

  from_coeffs(coeffs: bigint[]): Result<Poly, Error>
  {
    if (!__equals(BigInt(coeffs.length), this.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    let slots = [...coeffs];
    this.$fntt.forward(slots);
    return new PolyLoweredDyn({ $fslots: slots, $fn: 0n });
  }

  from_slots(slots: bigint[]): Result<Poly, Error>
  {
    if (!__equals(BigInt(slots.length), this.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    const q = this.$fparameters.$fmodulus;
    return new PolyLoweredDyn({ $fslots: slots.map((v: any) => (v % q)), $fn: 0n });
  }

  gadget_decompose_coeffs(coeffs: bigint[], base: bigint, digits: bigint): Result<Vec<Vec<bigint>>, Error>
  {
    if ((!__equals(BigInt(coeffs.length), this.$fparameters.$fdegree) || !((base) > 0n && ((base) & ((base) - 1n)) === 0n)))     {
      return new Error_LengthMismatch();
    }
    let out = Array.from({length: Number(digits)}, () => Array.from({length: Number(this.$fparameters.$fdegree)}, () => 0n));
    const bits = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, base)));
    for (const [i, c] of coeffs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      for (let d = 0n; d < digits; d += 1n)       {
        out[Number(d)][Number(i)] = fieldBitand(fieldShr(c, fieldMul(bits, d)), fieldSub(base, 1n));
      }
    }
    return out;
  }

  mul(left: any, right: any): Poly
  {
    const q = this.$fparameters.$fmodulus;
    return new PolyLoweredDyn({ $fslots: left.$fslots.map((__a: any, __i: number) => [__a, right.$fslots[__i]] as [typeof __a, any]).map(([l, r]: any) => mul_mod(l, r, q)), $fn: 0n });
  }

  negate_assign(value: any)
  {
    const q = this.$fparameters.$fmodulus;
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(value.$fslots.length); __zip_mut_2 += 1n) {
      {
        if (!__equals(value.$fslots[Number(__zip_mut_2)], 0n))         {
          value.$fslots[Number(__zip_mut_2)] = fieldSub(q, value.$fslots[Number(__zip_mut_2)]);
        }
      }}
  }

  static new(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, parameters: any): Result<Ring, Error>
  {
    const __volar_try_0 = parameters.validate();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    return new Ring({ $fntt: Ntt.new(parameters.$fdegree, parameters.$fmodulus), $fparameters: parameters });
  }

  parameters(): Parameters
  {
    return this.$fparameters;
  }

  scalar_mul(value: any, scalar: bigint): Poly
  {
    const q = this.$fparameters.$fmodulus;
    const scalar_1 = (scalar % q);
    return new PolyLoweredDyn({ $fslots: value.$fslots.map((v: any) => mul_mod(v, scalar_1, q)), $fn: 0n });
  }

  sub(left: any, right: any): Poly
  {
    let out = __clone(left);
    this.sub_assign(out, right);
    return out;
  }

  sub_assign(left: any, right: any)
  {
    const q = this.$fparameters.$fmodulus;
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(left.$fslots.length); __zip_mut_2 += 1n) {
      const r = right.$fslots[Number(__zip_mut_2)];
      {
        left.$fslots[Number(__zip_mut_2)] = sub_mod(left.$fslots[Number(__zip_mut_2)], r, q);
      }}
  }

  to_coeffs(value: any): Vec<bigint>
  {
    let coeffs = __clone(value.$fslots);
    this.$fntt.inverse(coeffs);
    return coeffs;
  }

  uniform(random: (arg: Uint8Array) => Result<void, Error>): Result<Poly, Error>
  {
    let value = this.zero();
    const __volar_try_1 = sample_uniform(random, this.$fparameters.$fmodulus, value.$fslots);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    return value;
  }

  zero(): Poly
  {
    return new PolyLoweredDyn({ $fslots: Array.from({length: Number(this.$fparameters.$fdegree)}, () => 0n), $fn: 0n });
  }
}

export class Ntt {
  $fmodulus!: bigint;
  $fdegree!: bigint;
  $fpsi_powers!: Vec<bigint>;
  $finv_psi_powers_scaled!: Vec<bigint>;
  $ffwd_twiddles!: Vec<bigint>;
  $finv_twiddles!: Vec<bigint>;

  constructor(init: {
    $fmodulus: bigint,
    $fdegree: bigint,
    $fpsi_powers: Vec<bigint>,
    $finv_psi_powers_scaled: Vec<bigint>,
    $ffwd_twiddles: Vec<bigint>,
    $finv_twiddles: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fmodulus: __zeroValue(this.$fmodulus), $fdegree: __zeroValue(this.$fdegree), $fpsi_powers: __zeroValue(this.$fpsi_powers), $finv_psi_powers_scaled: __zeroValue(this.$finv_psi_powers_scaled), $ffwd_twiddles: __zeroValue(this.$ffwd_twiddles), $finv_twiddles: __zeroValue(this.$finv_twiddles) }) as this;
  }

  cyclic(values: bigint[], twiddles: bigint[])
  {
    bit_reverse(values);
    let length = 2n;
    let stage = 0n;
    while ((length <= this.$fdegree))     {
      const step = twiddles[Number(stage)];
      for (const start of Array.from({length: Number(this.$fdegree - 0n)}, (_, __i) => BigInt(__i) + 0n).filter((_: any, __step_i: number) => BigInt(__step_i) % (length) === 0n))       {
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
      stage = fieldAdd(stage, 1n);
    }
  }

  degree(): bigint
  {
    return this.$fdegree;
  }

  forward(values: bigint[])
  {
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(values.length); __zip_mut_2 += 1n) {
      const s = this.$fpsi_powers[Number(__zip_mut_2)];
      {
        values[Number(__zip_mut_2)] = mul_mod(values[Number(__zip_mut_2)], s, this.$fmodulus);
      }}
    this.cyclic(values, this.$ffwd_twiddles);
  }

  inverse(values: bigint[])
  {
    this.cyclic(values, this.$finv_twiddles);
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(values.length); __zip_mut_2 += 1n) {
      const s = this.$finv_psi_powers_scaled[Number(__zip_mut_2)];
      {
        values[Number(__zip_mut_2)] = mul_mod(values[Number(__zip_mut_2)], s, this.$fmodulus);
      }}
  }

  modulus(): bigint
  {
    return this.$fmodulus;
  }

  static new(degree: bigint, modulus: bigint): Result<Ntt, Error>
  {
    if (((__equals(degree, 0n) || !((degree) > 0n && ((degree) & ((degree) - 1n)) === 0n)) || !__equals((fieldSub(modulus, 1n) % fieldMul(2n, BigInt(degree))), 0n)))     {
      return new Error_Arithmetic();
    }
    const __volar_try_1 = ((find_negacyclic_root(degree, modulus)) ?? (new Error_Arithmetic()));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const psi = __volar_try_1;
    const omega = mul_mod(psi, psi, modulus);
    const __volar_try_3 = ((inverse_mod(psi, modulus)) ?? (new Error_Arithmetic()));
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const inverse_psi = __volar_try_3;
    const __volar_try_4 = ((inverse_mod(omega, modulus)) ?? (new Error_Arithmetic()));
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const inverse_omega = __volar_try_4;
    const __volar_try_5 = ((inverse_mod(BigInt(degree), modulus)) ?? (new Error_Arithmetic()));
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const inverse_degree = __volar_try_5;
    let psi_powers = ([] as any[]);
    let acc = 1n;
    for (let _ = 0n; _ < degree; _ += 1n)     {
      (psi_powers).push(acc);
      acc = mul_mod(acc, psi, modulus);
    }
    let inv_psi_powers_scaled = ([] as any[]);
    let acc_1 = 1n;
    for (let _ = 0n; _ < degree; _ += 1n)     {
      (inv_psi_powers_scaled).push(mul_mod(acc_1, inverse_degree, modulus));
      acc_1 = mul_mod(acc_1, inverse_psi, modulus);
    }
    let fwd_twiddles = [] as any[];
    let inv_twiddles = [] as any[];
    let length = 2n;
    while ((length <= degree))     {
      (fwd_twiddles).push(pow_mod(omega, (degree / length), modulus));
      (inv_twiddles).push(pow_mod(inverse_omega, (degree / length), modulus));
      length = fieldMul(length, 2n);
    }
    return new Ntt({ $fmodulus: modulus, $fdegree: degree, $fpsi_powers: psi_powers, $finv_psi_powers_scaled: inv_psi_powers_scaled, $ffwd_twiddles: fwd_twiddles, $finv_twiddles: inv_twiddles });
  }
}

export class OfflineGarbler {
  $fsecret!: SecretKey;
  $fksks!: KeySwitchKeySet;
  $fplan!: ExpansionPlan;

  constructor(init: {
    $fsecret: SecretKey,
    $fksks: KeySwitchKeySet,
    $fplan: ExpansionPlan
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsecret: __zeroValue(this.$fsecret), $fksks: __zeroValue(this.$fksks), $fplan: __zeroValue(this.$fplan) }) as this;
  }

  wire_state(ctx: any, plan: any, ct_a0: any, ct_pi: any, index: bigint): Result<WireState, Error>
  {
    const degree = ctx.parameters().$fdegree;
    const slot = (i: bigint) => ((i % plan.$fslots_per_seed) % degree);
    const a0 = (ctx.decrypt(this.$fsecret, ct_a0)[Number(slot(fieldMul(2n, index)))] % 2n);
    const pi = __equals((ctx.decrypt(this.$fsecret, ct_pi)[Number(slot(fieldAdd(fieldMul(2n, index), 1n)))] % 2n), 1n);
    const p = ctx.parameters().$fplaintext_modulus;
    return new WireState({ $fa0: (a0 % p), $fpi: pi });
  }
}

export class OfflineEvaluator {
  $fplan!: ExpansionPlan;
  $fwires!: Vec<ExpandedWire>;

  constructor(init: {
    $fplan: ExpansionPlan,
    $fwires: Vec<ExpandedWire>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fplan: __zeroValue(this.$fplan), $fwires: __zeroValue(this.$fwires) }) as this;
  }

  wire(wire: bigint): (ExpandedWire | undefined)
  {
    return this.$fwires?.[wire];
  }

  wires(): bigint
  {
    return BigInt(this.$fwires.length);
  }
}

export class ExpansionStep {
  $fwire_state!: WireState;
  $feval!: ExpandedWire;

  constructor(init: {
    $fwire_state: WireState,
    $feval: ExpandedWire
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fwire_state: __zeroValue(this.$fwire_state), $feval: __zeroValue(this.$feval) }) as this;
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

  sample(_bound: bigint, output: bigint[]): Result<void, Error>
  {
    (output).fill(0n);
    return undefined;
  }
}

export class Parameters {
  $fdegree!: bigint;
  $fplaintext_modulus!: bigint;
  $fchain!: Vec<bigint>;
  $fnoise_bound!: bigint;

  constructor(init: {
    $fdegree: bigint,
    $fplaintext_modulus: bigint,
    $fchain: Vec<bigint>,
    $fnoise_bound: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdegree: __zeroValue(this.$fdegree), $fplaintext_modulus: __zeroValue(this.$fplaintext_modulus), $fchain: __zeroValue(this.$fchain), $fnoise_bound: __zeroValue(this.$fnoise_bound) }) as this;
  }
}

export class RnsPoly {
  $flimbs!: Vec<Poly>;

  constructor(init: {
    $flimbs: Vec<Poly>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flimbs: __zeroValue(this.$flimbs) }) as this;
  }

  limbs(): Poly[]
  {
    return this.$flimbs;
  }
}

export class SecretKey {
  $fcoeffs!: Vec<bigint>;

  constructor(init: {
    $fcoeffs: Vec<bigint>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fcoeffs: __zeroValue(this.$fcoeffs) }) as this;
  }
}

export class KeySwitchKey {
  $flevel!: bigint;
  $fbase!: bigint;
  $fparts!: Vec<[RnsPoly, RnsPoly]>;

  constructor(init: {
    $flevel: bigint,
    $fbase: bigint,
    $fparts: Vec<[RnsPoly, RnsPoly]>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flevel: __zeroValue(this.$flevel), $fbase: __zeroValue(this.$fbase), $fparts: __zeroValue(this.$fparts) }) as this;
  }
}

export class KeySwitchKeySet {
  $fkeys!: Vec<KeySwitchKey>;
  $fbase!: bigint;

  constructor(init: {
    $fkeys: Vec<KeySwitchKey>,
    $fbase: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fkeys: __zeroValue(this.$fkeys), $fbase: __zeroValue(this.$fbase) }) as this;
  }

  at(level: bigint): (KeySwitchKey | undefined)
  {
    return this.$fkeys?.[level];
  }

  base(): bigint
  {
    return this.$fbase;
  }

  max_level(): bigint
  {
    return fieldSub(BigInt(this.$fkeys.length), 1n);
  }
}

export class Ciphertext {
  $flevel!: bigint;
  $fparts!: Vec<RnsPoly>;

  constructor(init: {
    $flevel: bigint,
    $fparts: Vec<RnsPoly>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flevel: __zeroValue(this.$flevel), $fparts: __zeroValue(this.$fparts) }) as this;
  }

  level(): bigint
  {
    return this.$flevel;
  }

  parts(): RnsPoly[]
  {
    return this.$fparts;
  }
}

export class Context {
  $fparameters!: Parameters;
  $flevel_rings!: Vec<Ring>;
  $fplaintext_ring!: Ring;

  constructor(init: {
    $fparameters: Parameters,
    $flevel_rings: Vec<Ring>,
    $fplaintext_ring: Ring
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparameters: __zeroValue(this.$fparameters), $flevel_rings: __zeroValue(this.$flevel_rings), $fplaintext_ring: __zeroValue(this.$fplaintext_ring) }) as this;
  }

  add(left: any, right: any): Result<Ciphertext, Error>
  {
    const [left_owned, right_owned];
    let l = left;
    let r = right;
    if ((l.$flevel > r.$flevel))     {
      left_owned = this.switch_to(l, r.$flevel);
      l = left_owned;
    } else if ((r.$flevel > l.$flevel))     {
      right_owned = this.switch_to(r, l.$flevel);
      r = right_owned;
    }
    const parts = l.$fparts.map((__a: any, __i: number) => [__a, r.$fparts[__i]] as [typeof __a, any]).map(([a, b]: any) => this.add_rns(a, b));
    return new Ciphertext({ $flevel: l.$flevel, $fparts: parts });
  }

  add_assign_rns(left: any, right: any)
  {
    for (const [i, [l, r]] of left.$flimbs.map((__zip_a: any, __zip_i: number) => [__zip_a, (right.$flimbs)[__zip_i]]).map((__enum_a: any, __enum_i: number) => [BigInt(__enum_i), __enum_a] as [bigint, typeof __enum_a]))     {
      this.$flevel_rings[Number(i)].add_assign(l, r);
    }
  }

  add_assign_rns_mut(left: any, right: any)
  {
    this.add_assign_rns(left, right);
  }

  add_plain(ct: any, slots: bigint[]): Result<Ciphertext, Error>
  {
    const __volar_try_0 = this.embed(ct.$flevel, slots);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const m = __volar_try_0;
    let parts = __clone(ct.$fparts);
    this.add_assign_rns(parts[Number(0n)], m);
    return new Ciphertext({ $flevel: ct.$flevel, $fparts: parts });
  }

  add_rns(left: any, right: any): RnsPoly
  {
    return new RnsPoly({ $flimbs: left.$flimbs.map((__a: any, __i: number) => [__a, right.$flimbs[__i]] as [typeof __a, any]).map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([i, [l, r]]: any) => this.$flevel_rings[Number(i)].add(l, r)) });
  }

  check_level(level: bigint): Result<void, Error>
  {
    if ((level > this.max_level()))     {
      return new Error_InvalidParameters();
    }
    return undefined;
  }

  constant_slots(value: bigint): Vec<bigint>
  {
    return Array.from({length: Number(this.$fparameters.$fdegree)}, () => (value % this.$fparameters.$fplaintext_modulus));
  }

  decrypt(sk: any, ct: any): Result<Vec<bigint>, Error>
  {
    if (!__equals(BigInt(sk.$fcoeffs.length), this.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    const p = this.$fparameters.$fplaintext_modulus;
    const s = this.secret_rns(ct.$flevel, sk);
    const coeffs = this.$flevel_rings[Number(0n)].to_coeffs((() => {
  const ring0 = this.$flevel_rings[Number(0n)];
  let limb = ring0.mul(ct.$fparts[Number(1n)].$flimbs[Number(0n)], s.$flimbs[Number(0n)]);
  ring0.add_assign(limb, ct.$fparts[Number(0n)].$flimbs[Number(0n)]);
  return limb;
})());
    const q0 = this.$fparameters.$fchain[Number(0n)];
    const m_coeffs: Vec<bigint> = coeffs.map((c: any) => BigInt((((center(c, q0)) % ((p as unknown as bigint)) + ((p as unknown as bigint))) % ((p as unknown as bigint)))));
    const __volar_try_6 = this.$fplaintext_ring.from_coeffs(m_coeffs);
    if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
    const packed = __volar_try_6;
    return [...packed.slots()];
  }

  embed(level: bigint, slots: bigint[]): Result<RnsPoly, Error>
  {
    if (!__equals(BigInt(slots.length), this.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    if ((__equals(level, 0n) && slots.every((v) => __equals(v, 0n))))     {
      return new Error_InvalidParameters();
    }
    const p = this.$fparameters.$fplaintext_modulus;
    if (slots.some((v) => (v >= p)))     {
      return new Error_InvalidParameters();
    }
    const __volar_try_4 = this.$fplaintext_ring.from_slots(slots);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const packed = __volar_try_4;
    const m_coeffs = this.$fplaintext_ring.to_coeffs(packed);
    let limbs = ([] as any[]);
    for (const ring of this.$flevel_rings.slice(0, Number(level + 1n)))     {
      const q = ring.parameters().$fmodulus;
      const coeffs: Vec<bigint> = m_coeffs.map((c: any) => (c % q));
      (limbs).push(ring.from_coeffs(coeffs));
    }
    return new RnsPoly({ $flimbs: limbs });
  }

  encrypt(ctx: { defaultA: () => any }, sk: any, level: bigint, slots: bigint[], random: any, noise: any): Result<Ciphertext, Error>
  {
    const __volar_try_0 = this.check_level(level);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    if (!__equals(BigInt(sk.$fcoeffs.length), this.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    const p = this.$fparameters.$fplaintext_modulus;
    const __volar_try_3 = this.embed(level, slots);
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const m = __volar_try_3;
    const __volar_try_4 = this.uniform_rns(level, random);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const a = __volar_try_4;
    const __volar_try_5 = this.noise_rns(ctx, level, noise);
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const e = __volar_try_5;
    const s = this.secret_rns(level, sk);
    let c0 = this.mul_rns(a, s);
    const pe = this.scalar_mul_rns(e, p);
    this.add_assign_rns(c0, m);
    this.add_assign_rns(c0, pe);
    let c1 = a;
    for (let __mut_2 = 0n; __mut_2 < BigInt(c1.$flimbs.length); __mut_2 += 1n) {
      const i = __mut_2;
      {
        this.$flevel_rings[Number(i)].negate_assign(c1.$flimbs[Number(__mut_2)]);
      }}
    return new Ciphertext({ $flevel: level, $fparts: [c0, c1] });
  }

  encrypt_one_like(like: any): Result<Ciphertext, Error>
  {
    let one = this.zero(like.$flevel.$fn, like.$flevel);
    for (let __mut_2 = 0n; __mut_2 < BigInt(one.$flimbs.length); __mut_2 += 1n) {
      const i = __mut_2;
      {
        one.$flimbs[Number(__mut_2)] = this.$flevel_rings[Number(i)].constant(1n.$fn, 1n);
      }}
    return new Ciphertext({ $flevel: like.$flevel, $fparts: [one, this.zero(like.$flevel.$fn, like.$flevel)] });
  }

  key_switch_keygen(ctx: { defaultA: () => any }, sk: any, level: bigint, base: bigint, digits: bigint, random: any, noise: any): Result<KeySwitchKey, Error>
  {
    const __volar_try_0 = this.check_level(level);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    if (((!((base) > 0n && ((base) & ((base) - 1n)) === 0n) || (base < 2n)) || __equals(digits, 0n)))     {
      return new Error_InvalidParameters();
    }
    if ((fieldMul(BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, base))), digits) < 64n))     {
      return new Error_InvalidParameters();
    }
    const p = this.$fparameters.$fplaintext_modulus;
    const s = this.secret_rns(level, sk);
    const s2 = this.mul_rns(s, s);
    let parts = ([] as any[]);
    for (let i = 0n; i < digits; i += 1n)     {
      const __volar_try_0 = this.uniform_rns(level, random);
      if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
      const a = __volar_try_0;
      const __volar_try_1 = this.noise_rns(ctx, level, noise);
      if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
      const e = __volar_try_1;
      let b = this.mul_rns(a, s);
      const pe = this.scalar_mul_rns(e, p);
      this.add_assign_rns(b, pe);
      let scaled = __clone(s2);
      for (let __mut_3 = 0n; __mut_3 < BigInt(scaled.$flimbs.length); __mut_3 += 1n) {
        const j = __mut_3;
        {
          const q = this.$flevel_rings[Number(j)].parameters().$fmodulus;
          const factor = pow_mod((base % q), i, q);
          scaled.$flimbs[Number(__mut_3)] = this.$flevel_rings[Number(j)].scalar_mul(scaled.$flimbs[Number(__mut_3)], factor);
        }}
      this.add_assign_rns(b, scaled);
      let neg_a = a;
      for (let __mut_3 = 0n; __mut_3 < BigInt(neg_a.$flimbs.length); __mut_3 += 1n) {
        const j = __mut_3;
        {
          this.$flevel_rings[Number(j)].negate_assign(neg_a.$flimbs[Number(__mut_3)]);
        }}
      (parts).push([neg_a, b]);
    }
    return new KeySwitchKey({ $flevel: level, $fbase: base, $fparts: parts });
  }

  key_switch_keyset(ctx: { defaultA: () => any }, sk: any, base: bigint, digits: bigint, random: any, noise: any): Result<KeySwitchKeySet, Error>
  {
    let keys = ([] as any[]);
    for (let level = 0n; level <= this.max_level(); level += 1n)     {
      (keys).push(this.key_switch_keygen(ctx, sk, level, base, digits, random, noise));
    }
    return new KeySwitchKeySet({ $fkeys: keys, $fbase: base });
  }

  keygen(random: any): Result<SecretKey, Error>
  {
    let coeffs = Array.from({length: Number(this.$fparameters.$fdegree)}, () => 0n);
    let byte = Array.from({length: Number(1n)}, () => ((0n) & 0xFFn));
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(coeffs.length); __zip_mut_2 += 1n) {
      {
        const __volar_try_0 = random.fill_bytes(byte.slice(0));
        if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
        coeffs[Number(__zip_mut_2)] = fieldSub(((byte[Number(0n)] % 3n) as unknown as bigint), 1n);
      }}
    return new SecretKey({ $fcoeffs: coeffs });
  }

  limbs(level: bigint): bigint
  {
    return fieldAdd(level, 1n);
  }

  max_level(): bigint
  {
    return fieldSub(BigInt(this.$fparameters.$fchain.length), 1n);
  }

  mod_switch_down(ct: any): Result<Ciphertext, Error>
  {
    if ((__equals(ct.$flevel, 0n) || (ct.$flevel > this.max_level())))     {
      return new Error_LevelMismatch();
    }
    const p = this.$fparameters.$fplaintext_modulus;
    const from_ = ct.$flevel;
    const q_top = this.$fparameters.$fchain[Number(from_)];
    const __volar_try_4 = ((inverse_mod((p % q_top), q_top)) ?? (new Error_Arithmetic()));
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const inv_p_top = __volar_try_4;
    const __volar_try_5 = ct.$fparts.map((part: any) => (() => {
  const top_coeffs = this.$flevel_rings[Number(from_)].to_coeffs(part.$flimbs[Number(from_)]);
  const deltas: Vec<bigint> = top_coeffs.map((c: any) => mul_mod(mul_mod(c, inv_p_top, q_top), 1n, q_top));
  let limbs = ([] as any[]);
  for (const [i, ring_i] of this.$flevel_rings.slice(0, Number(from_)).map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const q_i = ring_i.parameters().$fmodulus;
    const __volar_try_1 = ((inverse_mod((q_top % q_i), q_i)) ?? (new Error_Arithmetic()));
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const inv_q_top = __volar_try_1;
    const ci = ring_i.to_coeffs(part.$flimbs[Number(i)]);
    const p_mod_qi = (p % q_i);
    const out: Vec<bigint> = ci.map((__a: any, __i: number) => [__a, deltas[__i]] as [typeof __a, any]).map(([c, d]: any) => (() => {
  const pd = mul_mod(p_mod_qi, d, q_i);
  const diff = sub_mod(c, pd, q_i);
  return mul_mod(diff, inv_q_top, q_i);
})());
    (limbs).push(ring_i.from_coeffs(out));
  }
  return new RnsPoly({ $flimbs: limbs });
})());
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const parts = __volar_try_5;
    return new Ciphertext({ $flevel: fieldSub(from_, 1n), $fparts: parts });
  }

  mul(left: any, right: any, ksk: any): Result<Ciphertext, Error>
  {
    const [left_owned, right_owned];
    let l = left;
    let r = right;
    if ((l.$flevel > r.$flevel))     {
      left_owned = this.switch_to(l, r.$flevel);
      l = left_owned;
    } else if ((r.$flevel > l.$flevel))     {
      right_owned = this.switch_to(r, l.$flevel);
      r = right_owned;
    }
    const [left_1, right_1] = [l, r];
    const level = left_1.$flevel;
    const __volar_try_6 = ((ksk[(level)]) ?? (new Error_LevelMismatch()));
    if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
    const ksk_1 = __volar_try_6;
    if (!__equals(ksk_1.$flevel, level))     {
      return new Error_LevelMismatch();
    }
    const t0 = this.mul_rns(left_1.$fparts[Number(0n)], right_1.$fparts[Number(0n)]);
    let t1 = this.mul_rns(left_1.$fparts[Number(0n)], right_1.$fparts[Number(1n)]);
    const t1b = this.mul_rns(left_1.$fparts[Number(1n)], right_1.$fparts[Number(0n)]);
    this.add_assign_rns_mut(t1, t1b);
    const t2 = this.mul_rns(left_1.$fparts[Number(1n)], right_1.$fparts[Number(1n)]);
    let c0 = t0;
    let c1 = t1;
    const t2_coeffs: Vec<Vec<bigint>> = t2.$flimbs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([i, limb]: any) => this.$flevel_rings[Number(i)].to_coeffs(limb));
    const base_bits = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(32); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(32, ksk_1.$fbase)));
    if ((!((ksk_1.$fbase) > 0n && ((ksk_1.$fbase) & ((ksk_1.$fbase) - 1n)) === 0n) || (ksk_1.$fbase < 2n)))     {
      return new Error_InvalidParameters();
    }
    const _ = base_bits;
    for (const [digit_index, [neg_a, b]] of ksk_1.$fparts.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const shift = fieldMul(base_bits, digit_index);
      let digit = this.zero(level.$fn, level);
      if ((shift < 64n))       {
        for (const [i, ring_i] of this.$flevel_rings.slice(0, Number(level + 1n)).map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))         {
          const coeffs: Vec<bigint> = t2_coeffs[Number(i)].map((c: any) => fieldBitand(fieldShr(c, shift), fieldSub(ksk_1.$fbase, 1n)));
          digit.$flimbs[Number(i)] = ring_i.from_coeffs(coeffs);
        }
      }
      this.add_assign_rns_mut(c0, this.mul_rns(digit, b));
      this.add_assign_rns_mut(c1, this.mul_rns(digit, neg_a));
    }
    const product = new Ciphertext({ $flevel: level, $fparts: [c0, c1] });
    return (() => { if (__equals(level, 0n)) {
  return product;
} else {
  return this.mod_switch_down(product);
} })();
  }

  mul_plain(ct: any, slots: bigint[]): Result<Ciphertext, Error>
  {
    const __volar_try_0 = this.embed(ct.$flevel, slots);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const m = __volar_try_0;
    const parts = ct.$fparts.map((c: any) => this.mul_rns(c, m));
    return new Ciphertext({ $flevel: ct.$flevel, $fparts: parts });
  }

  mul_rns(left: any, right: any): RnsPoly
  {
    return new RnsPoly({ $flimbs: left.$flimbs.map((__a: any, __i: number) => [__a, right.$flimbs[__i]] as [typeof __a, any]).map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([i, [l, r]]: any) => this.$flevel_rings[Number(i)].mul(l, r)) });
  }

  static new(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, parameters: any): Result<Context, Error>
  {
    const __volar_try_0 = parameters.validate();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const __volar_try_1 = parameters.$fchain.map((q: any) => (() => {
  return Ring.new(ctx, new Parameters({ $fdegree: parameters.$fdegree, $fmodulus: q }));
})());
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const level_rings = __volar_try_1;
    const __volar_try_2 = Ring.new(ctx, new Parameters({ $fdegree: parameters.$fdegree, $fmodulus: parameters.$fplaintext_modulus }));
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const plaintext_ring = __volar_try_2;
    return new Context({ $fparameters: parameters, $flevel_rings: level_rings, $fplaintext_ring: plaintext_ring });
  }

  noise_rns(ctx: { defaultA: () => any }, level: bigint, noise: any): Result<RnsPoly, Error>
  {
    let coeffs_i = Array.from({length: Number(this.$fparameters.$fdegree)}, () => 0n);
    const __volar_try_1 = noise.sample(ctx, coeffs_i.$fn, this.$fparameters.$fnoise_bound, coeffs_i);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    if (coeffs_i.some((v) => (((v) < 0n ? -(v) : (v)) > this.$fparameters.$fnoise_bound)))     {
      return new Error_NoiseSource();
    }
    let limbs = ([] as any[]);
    for (const ring of this.$flevel_rings.slice(0, Number(level + 1n)))     {
      const q = ring.parameters().$fmodulus;
      const coeffs: Vec<bigint> = coeffs_i.map((v: any) => (() => {
  return (() => { if ((v < 0n)) {
  return fieldSub(q, (((v) < 0n ? -(v) : (v)) % q));
} else {
  return (BigInt(v) % q);
} })();
})());
      (limbs).push(ring.from_coeffs(coeffs));
    }
    return new RnsPoly({ $flimbs: limbs });
  }

  parameters(): Parameters
  {
    return this.$fparameters;
  }

  scalar_mul_rns(value: any, scalar: bigint): RnsPoly
  {
    return new RnsPoly({ $flimbs: value.$flimbs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([i, l]: any) => this.$flevel_rings[Number(i)].scalar_mul(l, scalar)) });
  }

  secret_rns(level: bigint, sk: any): RnsPoly
  {
    const limbs = this.$flevel_rings.slice(0, Number(level + 1n)).map((ring: any) => (() => {
  const q = ring.parameters().$fmodulus;
  const coeffs: Vec<bigint> = sk.$fcoeffs.map((v: any) => (() => {
  return (() => { if ((v < 0n)) {
  return fieldSub(q, (((v) < 0n ? -(v) : (v)) % q));
} else {
  return (BigInt(v) % q);
} })();
})());
  return (ring.from_coeffs(coeffs))!;
})());
    return new RnsPoly({ $flimbs: limbs });
  }

  sub(left: any, right: any): Result<Ciphertext, Error>
  {
    const [left_owned, right_owned];
    let l = left;
    let r = right;
    if ((l.$flevel > r.$flevel))     {
      left_owned = this.switch_to(l, r.$flevel);
      l = left_owned;
    } else if ((r.$flevel > l.$flevel))     {
      right_owned = this.switch_to(r, l.$flevel);
      r = right_owned;
    }
    let neg = __clone(r);
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(neg.$fparts.length); __zip_mut_2 += 1n) {
      {
        for (let __mut_4 = 0n; __mut_4 < BigInt(neg.$fparts[Number(__zip_mut_2)].$flimbs.length); __mut_4 += 1n) {
          const i = __mut_4;
          {
            this.$flevel_rings[Number(i)].negate_assign(neg.$fparts[Number(__zip_mut_2)].$flimbs[Number(__mut_4)]);
          }}
      }}
    return this.add(l, neg);
  }

  switch_to(ct: any, target: bigint): Result<Ciphertext, Error>
  {
    let out = __clone(ct);
    while ((out.$flevel > target))     {
      out = this.mod_switch_down(out);
    }
    return out;
  }

  uniform_rns(level: bigint, random: any): Result<RnsPoly, Error>
  {
    let limbs = ([] as any[]);
    for (const ring of this.$flevel_rings.slice(0, Number(level + 1n)))     {
      let slots = Array.from({length: Number(this.$fparameters.$fdegree)}, () => 0n);
      const modulus = ring.parameters().$fmodulus;
      let rng = (out: bigint[]) => (() => {
  return random.fill_bytes(out);
})();
      const __volar_try_3 = sample_uniform(rng, modulus, slots);
      if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
      (limbs).push(ring.from_slots(slots));
    }
    return new RnsPoly({ $flimbs: limbs });
  }

  zero(level: bigint): RnsPoly
  {
    return new RnsPoly({ $flimbs: this.$flevel_rings.slice(0, Number(level + 1n)).map(Ring.zero) });
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
  const i2 = fieldBitor(i, fieldShl(BigInt(j), ilog2(t)));
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
  return s.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([a, c]: any) => (() => {
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
  return s.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([a, c]: any) => (() => {
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
        const i2 = fieldBitor(i, fieldShl(BigInt(b), ilog2(t)));
        if (this.$fbad.includes(BigInt(i2)))         {
          h.update(this.$fopenings[Number(0n)][Number(i)][Number(b)].slice(0, Number(ctx.D_OutputSize)));
        } else         {
          h.update(commit(ctx, this.$fopenings[Number(0n)][Number(i)][Number(b)].slice(0, Number(ctx.B_OutputSize)), rand));
        }
      }
    }
    return __equals([...Array.from(h.finalize(), BigInt)], commit_);
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
    let out = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
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
    return new EdPoint({ $fx: x, $fy: y, $fz: new Fe25519([1n, 0n, 0n, 0n]), $ft: fe_mul(x, y) });
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

  static random_mod_order<R>(rng: any): bigint[]
  {
    let k = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(k.length); __zip_mut_2 += 1n) {
      {
        k[Number(__zip_mut_2)] = rng.next_u8();
      }}
    k[Number(31n)] = fieldBitand(k[Number(31n)], 15n);
    return k;
  }

  static random_scalar<R>(rng: any): bigint[]
  {
    let k = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(k.length); __zip_mut_2 += 1n) {
      {
        k[Number(__zip_mut_2)] = rng.next_u8();
      }}
    k[Number(31n)] = fieldBitand(k[Number(31n)], 63n);
    return k;
  }

  static scalar_add(a: bigint[], b: bigint[]): bigint[]
  {
    return limbs_to_bytes(add_mod_l(bytes_to_limbs(a), bytes_to_limbs(b)));
  }

  static scalar_eq(a: bigint[], b: bigint[]): boolean
  {
    const ra = reduce512([(() => {
  const l = bytes_to_limbs(a);
  return l[Number(0n)];
})(), bytes_to_limbs(a)[Number(1n)], bytes_to_limbs(a)[Number(2n)], bytes_to_limbs(a)[Number(3n)], 0n, 0n, 0n, 0n]);
    const rb = reduce512([(() => {
  const l = bytes_to_limbs(b);
  return l[Number(0n)];
})(), bytes_to_limbs(b)[Number(1n)], bytes_to_limbs(b)[Number(2n)], bytes_to_limbs(b)[Number(3n)], 0n, 0n, 0n, 0n]);
    return __equals(cmp4(ra, rb), Equal);
  }

  static scalar_from_hash<D>(h: any): bigint[]
  {
    const out = [...Array.from(h.finalize(), BigInt)];
    let wide = Array.from({length: Number(8n)}, () => 0n);
    const n = BigInt(Math.min(Number(BigInt(out.length)), Number(64n)));
    for (const [i, chunk] of __chunks(out.slice(0, Number(n)), Number(8n)).map((__enum_a: any, __enum_i: number) => [BigInt(__enum_i), __enum_a] as [bigint, typeof __enum_a]))     {
      let le = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
      (le.slice(0, Number(BigInt(chunk.length)))).splice(0, (chunk).length, ...(chunk));
      wide[Number(i)] = u64_from_le_bytes(le);
    }
    return limbs_to_bytes(reduce512(wide));
  }

  static scalar_mul(elt: any, k: bigint[]): EdPoint
  {
    return ed_scalar_mul(elt, k);
  }

  static scalar_mul_scalar(a: bigint[], b: bigint[]): bigint[]
  {
    const prod = mul4(bytes_to_limbs(a), bytes_to_limbs(b));
    return limbs_to_bytes(reduce512(prod));
  }

  static scalar_sub(a: bigint[], b: bigint[]): bigint[]
  {
    return limbs_to_bytes(sub_mod_l(bytes_to_limbs(a), bytes_to_limbs(b)));
  }

  static scalar_zero(): bigint[]
  {
    return Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  }

  static write_element<D_>(elt: any, h: any)
  {
    const [x, y] = elt.to_affine();
    h.update(x.to_bytes());
    h.update(y.to_bytes());
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
    let hidden = Array.from({length: Number(total_nodes)}, () => false);
    for (let i = 0n; i < tau; i += 1n)     {
      const leaf_k = fieldAdd(fieldMul(i, n), deltas[Number(i)]);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    let node = fieldSub(leaf_count, 1n);
    while ((node > 0n))     {
      node = fieldSub(node, 1n);
      hidden[Number(node)] = (hidden[Number(fieldAdd(fieldMul(2n, node), 1n))] || hidden[Number(fieldAdd(fieldMul(2n, node), 2n))]);
    }
    let out: Vec<[bigint, bigint[]]> = [] as any[];
    collect_open_nodes_walk(0n, hidden, tree, leaf_count, out);
    return out;
  }

  static commit(ctx: { newD: () => any, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, com_bytes: bigint, r: bigint[], iv: bigint[], tau: bigint, n: bigint): BavcCommitmentDyn
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let tree: Vec<bigint[]> = Array.from({length: Number(total_nodes)}, () => Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn)));
    tree[Number(0n)] = r;
    for (let node = 0n; node < fieldSub(leaf_count, 1n); node += 1n)     {
      const parent = tree[Number(node)];
      const [left, right] = AesCtrLengthDoubler.double(parent);
      tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left;
      tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right;
    }
    let seeds: Vec<bigint[]> = ([] as any[]);
    let commitments: Vec<bigint[]> = ([] as any[]);
    for (let i = 0n; i < tau; i += 1n)     {
      for (let j = 0n; j < n; j += 1n)       {
        const leaf_k = fieldAdd(fieldMul(i, n), j);
        const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
        const r_leaf = tree[Number(tree_pos)];
        const tweak = BigInt(leaf_k);
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
      (vec_hashes).push([...[...Array.from(h.finalize(), BigInt)]]);
    }
    let h = ctx.newD();
    for (const hi of vec_hashes)     {
      h.update(hi);
    }
    const root = [...[...Array.from(h.finalize(), BigInt)]];
    return new BavcCommitmentDyn({ $froot: root, $fvec_hashes: vec_hashes, $fseeds: seeds, $fcommitments: commitments, $fcom_bytes: 0n });
  }

  static open(com_bytes: bigint, commitment: BavcCommitmentDyn, deltas: bigint[], tau: bigint, n: bigint): BavcOpeningDyn
  {
    for (let i = 0n; i < tau; i += 1n)     {
      const d = deltas[Number(i)];
    }
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let hidden = Array.from({length: Number(total_nodes)}, () => false);
    for (let i = 0n; i < tau; i += 1n)     {
      const leaf_k = fieldAdd(fieldMul(i, n), deltas[Number(i)]);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    let node = fieldSub(leaf_count, 1n);
    while ((node > 0n))     {
      node = fieldSub(node, 1n);
      const left = fieldAdd(fieldMul(2n, node), 1n);
      const right = fieldAdd(fieldMul(2n, node), 2n);
      hidden[Number(node)] = (hidden[Number(left)] || hidden[Number(right)]);
    }
    let tree: Vec<bigint[]> = Array.from({length: Number(total_nodes)}, () => Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn)));
    const _ = tree;
    let hidden_commits: Vec<bigint[]> = ([] as any[]);
    for (let i = 0n; i < BigInt(deltas.length); i += 1n)     {
      const d = deltas[Number(i)];
      (hidden_commits).push(commitment.$fcommitments[Number(fieldAdd(fieldMul(i, n), d))]);
    }
    return new BavcOpeningDyn({ $fhidden_commits: hidden_commits, $fnodes: [] as any[], $fcom_bytes: 0n });
  }

  static reconstruct(ctx: { newD: () => any, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, com_bytes: bigint, nodes: Vec<[bigint, bigint[]]>, hidden_commits: bigint[][], deltas: bigint[], iv: bigint[], expected_root: bigint[], tau: bigint, n: bigint): (Vec<bigint[]> | undefined)
  {
    const leaf_count = fieldMul(tau, n);
    const total_nodes = fieldSub(fieldMul(2n, leaf_count), 1n);
    let hidden = Array.from({length: Number(total_nodes)}, () => false);
    for (let i = 0n; i < tau; i += 1n)     {
      const leaf_k = fieldAdd(fieldMul(i, n), deltas[Number(i)]);
      const tree_pos = fieldAdd(fieldSub(leaf_count, 1n), leaf_k);
      hidden[Number(tree_pos)] = true;
    }
    let node = fieldSub(leaf_count, 1n);
    while ((node > 0n))     {
      node = fieldSub(node, 1n);
      hidden[Number(node)] = (hidden[Number(fieldAdd(fieldMul(2n, node), 1n))] || hidden[Number(fieldAdd(fieldMul(2n, node), 2n))]);
    }
    let tree: Vec<(bigint[] | undefined)> = Array.from({length: Number(total_nodes)}, () => undefined);
    for (const [idx, seed] of nodes)     {
      tree[Number(idx)] = seed;
    }
    for (let node = 0n; node < fieldSub(leaf_count, 1n); node += 1n)     {
      (() => { const __match = tree[Number(node)]; if (__match !== null && __match !== undefined) { const parent_seed = __match;
return (() => {
  const parent = parent_seed;
  const [left, right] = AesCtrLengthDoubler.double(parent);
  if ((tree[Number(fieldAdd(fieldMul(2n, node), 1n))]) == null)   {
    tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left;
  }
  if ((tree[Number(fieldAdd(fieldMul(2n, node), 2n))]) == null)   {
    tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right;
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
          (leaf_seeds).push(Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn)));
          (leaf_coms).push(hidden_commits[Number(i)]);
        } else         {
          const __volar_try_0 = tree[Number(tree_pos)];
          if (__volar_try_0 == null) return undefined;
          const r_leaf = __volar_try_0;
          const tweak = BigInt(leaf_k);
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
      (vec_hashes).push([...[...Array.from(h.finalize(), BigInt)]]);
    }
    let h = ctx.newD();
    for (const hi of vec_hashes)     {
      h.update(hi);
    }
    const root = [...[...Array.from(h.finalize(), BigInt)]];
    let matches = __equals(BigInt(root.length), BigInt(expected_root.length));
    let k = 0n;
    while ((k < BigInt(root.length)))     {
      if (!__equals(root[Number(k)], expected_root[Number(k)]))       {
        matches = false;
      }
      k = fieldAdd(k, 1n);
    }
    return (() => { if (matches) {
  return leaf_seeds;
} else {
  return undefined;
} })();
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
    const digest = [...Array.from(h.finalize(), BigInt)];
    let sd = Array.from({length: Number(sd_bytes)}, () => ((0n) & 0xFFn));
    let com = Array.from({length: Number(com_bytes)}, () => ((0n) & 0xFFn));
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
    let seed_buf = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    (seed_buf).splice(0, (r.slice(0)).length, ...(r.slice(0)));
    const com_vec = aes_ctr_prg(seed_buf, iv, tweak, com_bytes);
    let sd = Array.from({length: Number(sd_bytes)}, () => ((0n) & 0xFFn));
    (sd).splice(0, (r).length, ...(r));
    let com = Array.from({length: Number(com_bytes)}, () => ((0n) & 0xFFn));
    (com).splice(0, (com_vec.slice(0, Number(com_bytes))).length, ...(com_vec.slice(0, Number(com_bytes))));
    return [sd, com];
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
    let block0 = Array.from({length: Number(BLOCK)}, () => ((0n) & 0xFFn));
    let block1 = Array.from({length: Number(BLOCK)}, () => ((0n) & 0xFFn));
    block1[Number(0n)] = 1n;
    const key: bigint[] = a;
    const c0 = encrypt_block(key, block0);
    const c1 = encrypt_block(key, block1);
    (block0).fill(0n);
    (block1).fill(0n);
    return [c0, c1];
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
    const h0_bytes = [(a_hat_out.$fh0[0]) & 0xFFn, ((a_hat_out.$fh0[0]) >> 8n) & 0xFFn, ((a_hat_out.$fh0[0]) >> 16n) & 0xFFn, ((a_hat_out.$fh0[0]) >> 24n) & 0xFFn];
    const h1_bytes = [(a_hat_out.$fh1[0]) & 0xFFn, ((a_hat_out.$fh1[0]) >> 8n) & 0xFFn, ((a_hat_out.$fh1[0]) >> 16n) & 0xFFn, ((a_hat_out.$fh1[0]) >> 24n) & 0xFFn];
    let a_hat: Vec<bigint> = ([] as any[]);
    for (let i = 0n; i < BigInt(h0_bytes.length); i += 1n)     {
      (a_hat).push(h0_bytes[Number(i)]);
    }
    for (let i = 0n; i < BigInt(h1_bytes.length); i += 1n)     {
      (a_hat).push(h1_bytes[Number(i)]);
    }
    const len = BigInt(a_hat.length);
    return new QuickSilverProof({ $fa_hat: a_hat, $fb_hat: Array.from({length: Number(len)}, () => ((0n) & 0xFFn)), $fc_hat_base: Array.from({length: Number(len)}, () => ((0n) & 0xFFn)) });
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
    return this.$fsponge.squeeze(ctx, n);
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

export class Gf2_8 {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }
}

export class Gf2_64 {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }
}

export class Gf2_128 {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }
}

export class Gf2_256 {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }
}

export class Fp3 {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
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
  return [...Array.from(d.finalize(), BigInt)];
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

  action_result_base(ctx: { newD: () => any }, args: GarbleDyn[], bit: bigint): GarbleDyn
  {
    const n: bigint = this.$fn;
    let d = ctx.newD();
    const domain_separator = [((172n) & 0xFFn)];
    d.update(domain_separator);
    d.update(this.$fbase);
    let d_1 = absorb_args_bases(n, d, args);
    const bit_bytes = [(BigInt(bit)) & 0xFFn, ((BigInt(bit)) >> 8n) & 0xFFn, ((BigInt(bit)) >> 16n) & 0xFFn, ((BigInt(bit)) >> 24n) & 0xFFn];
    d_1.update(bit_bytes);
    const hash = [...Array.from(d_1.finalize(), BigInt)];
    return new GarbleDyn({ $fbase: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => hash[Number(i)]), $fn: 0n });
  }

  and_result(ctx: { newD: () => any }, b: GarbleDyn): GarbleDyn
  {
    const n: bigint = this.$fn;
    let d = ctx.newD();
    d.update(this.$fbase);
    d.update(b.$fbase);
    const hash = [...Array.from(d.finalize(), BigInt)];
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
    return this.gen_and_table_pol(ctx, a, b, false, false);
  }

  gen_and_table_pol(ctx: { newD: () => any, defaultT: () => any }, a: GarbleDyn, b: GarbleDyn, pa: boolean, pb: boolean): GarbleTableDyn
  {
    const n: bigint = this.$fn;
    const result_base = a.and_result(ctx, b);
    let table = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
    for (let i = 0n; i < 4n; i += 1n)     {
      const av = !__equals(fieldBitand(i, 1n), 0n);
      const bv = !__equals(fieldBitand(i, 2n), 0n);
      const ea = this.encode(a, av);
      const eb = this.encode(b, bv);
      const row = fieldBitor(BigInt(fieldBitand(ea.$ftarget[Number(0n)], 1n)), fieldShl(BigInt(fieldBitand(eb.$ftarget[Number(0n)], 1n)), 1n));
      const result_label = this.encode(result_base, fieldBitand(fieldBitxor(av, pa), fieldBitxor(bv, pb)));
      let d = ctx.newD();
      d.update(ea.$ftarget);
      d.update(eb.$ftarget);
      const hash = [...Array.from(d.finalize(), BigInt)];
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
    return this.encode(GarbleDyn.zero(n), true);
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
    return !__equals(fieldBitand(result.open(this.$foutput_label.$fcom_bytes, this.$foutput_label)[Number(0n)], 1n), 0n);
  }
}

export class SpecSponge {
  $fstate!: bigint[];
  $frate!: bigint;
  $fpos!: bigint;

  constructor(init: {
    $fstate: bigint[],
    $frate: bigint,
    $fpos: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fstate: __zeroValue(this.$fstate), $frate: __zeroValue(this.$frate), $fpos: __zeroValue(this.$fpos) }) as this;
  }
}

export class SpecXofReader {
  $fstate!: bigint[];
  $frate!: bigint;
  $fpos!: bigint;

  constructor(init: {
    $fstate: bigint[],
    $frate: bigint,
    $fpos: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fstate: __zeroValue(this.$fstate), $frate: __zeroValue(this.$frate), $fpos: __zeroValue(this.$fpos) }) as this;
  }
}

export class LweSampleDyn<T, U> {
  $fn!: bigint;
  $fm!: bigint;
  $fmatrix!: any[][];
  $fb!: any[];

  constructor(init: {
    $fn: bigint,
    $fm: bigint,
    $fmatrix: any[][],
    $fb: any[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fm: __zeroValue(this.$fm), $fmatrix: __zeroValue(this.$fmatrix), $fb: __zeroValue(this.$fb) }) as this;
  }

  static new<T, U>(n: bigint, m: bigint, matrix: any[][], b: any[]): LweSampleDyn<T, U>
  {
    return new LweSampleDyn<T, U>({ $fmatrix: matrix, $fb: b });
  }

  static sample<S, P, T, U>(ctx: { defaultA: () => any }, n: bigint, m: bigint, matrix: any[][], s: S[], e: any[]): LweSampleDyn<T, U>
  {
    return new LweSampleDyn<T, U>({ $fb: Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(s.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([a, b]: any) => fieldMul(__clone(b), __clone(matrix[Number(i)][Number(a)]))).reduce((a: any, b: any) => fieldAdd(a, b), ctx.defaultA()), __clone(e[Number(i)]));
})()), $fmatrix: matrix });
  }
}

export class AllPartiesDyn<T> {
  $fn!: bigint;
  $fother_parties!: OtherPartiesDyn<any>;
  $fself_party!: any;

  constructor(init: {
    $fn: bigint,
    $fother_parties: OtherPartiesDyn<any>,
    $fself_party: any
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fother_parties: __zeroValue(this.$fother_parties), $fself_party: __zeroValue(this.$fself_party) }) as this;
  }
}

export class OtherPartiesDyn<T> {
  $fn!: bigint;
  $fother_parties!: any[];

  constructor(init: {
    $fn: bigint,
    $fother_parties: any[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fother_parties: __zeroValue(this.$fother_parties) }) as this;
  }
}

export class NttParams {
  $fdegree!: bigint;
  $fmodulus!: bigint;

  constructor(init: {
    $fdegree: bigint,
    $fmodulus: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdegree: __zeroValue(this.$fdegree), $fmodulus: __zeroValue(this.$fmodulus) }) as this;
  }

  modulus_bits(): bigint
  {
    return fieldSub(64n, ((__value) => { let __bits = 0n; while (__value !== 0n) { __value >>= 1n; __bits += 1n; } return BigInt(64) - __bits; })(BigInt.asUintN(64, this.$fmodulus)));
  }

  static new(degree: bigint, modulus: bigint): (NttParams | undefined)
  {
    if ((__equals(degree, 0n) || !__equals(fieldBitand(degree, fieldSub(degree, 1n)), 0n)))     {
      return undefined;
    }
    if (__equals((modulus % 2n), 0n))     {
      return undefined;
    }
    if (!__equals((fieldSub(modulus, 1n) % fieldMul(2n, BigInt(degree))), 0n))     {
      return undefined;
    }
    return new NttParams({ $fdegree: degree, $fmodulus: modulus });
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

export class OtConsistencyProofDyn<S> {
  $fe0!: S;
  $fe1!: S;
  $fz0!: S;
  $fz1!: S;

  constructor(init: {
    $fe0: S,
    $fe1: S,
    $fz0: S,
    $fz1: S
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fe0: __zeroValue(this.$fe0), $fe1: __zeroValue(this.$fe1), $fz0: __zeroValue(this.$fz0), $fz1: __zeroValue(this.$fz1) }) as this;
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
    let mc = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
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
    let e0 = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    let e1 = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
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

  interval_bounds(index: bigint): ([bigint, bigint] | undefined)
  {
    if ((__equals(this.$ft, 0n) || (index >= this.$ft)))     {
      return undefined;
    }
    const n = (this.$fn as unknown as bigint);
    const t = (this.$ft as unknown as bigint);
    const start = BigInt((fieldMul((index as unknown as bigint), n) / t));
    const end = BigInt((fieldMul(fieldAdd((index as unknown as bigint), 1n), n) / t));
    return [start, end];
  }

  log_splen(): bigint
  {
    return BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, this.splen())));
  }

  output_cot_count(malicious: boolean): bigint
  {
    return (() => { const __a = this.$fn; const __b = this.seed_cot_count(malicious); const __result = __a - __b; return __result < 0n ? 0n : (__result > 18446744073709551615n ? 18446744073709551615n : __result); })();
  }

  seed_cot_count(malicious: boolean): bigint
  {
    const body = (((__option: any, __continuation: (value: any) => any) => __option == null ? undefined : __continuation(__option))((() => { const __a = this.$ft; const __b = this.log_splen(); const __result = __a * __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })(), (noise) => (() => { const __a = this.$fk; const __b = noise; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()))!;
    const seed_count = (() => { if (malicious) {
  return ((() => { const __a = body; const __b = KAPPA_BITS; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })())!;
} else {
  return body;
} })();
    return seed_count;
  }

  splen(): bigint
  {
    return (((__value: bigint) => { let __power = 1n; while (__power < __value) __power <<= 1n; return __power <= 18446744073709551615n ? __power : undefined; }) (((this.$fn) + (this.$ft) - 1n) / (this.$ft)))!;
  }
}

export class CotPoolSender {
  $fparams!: FerretParams;
  $fseed!: FerretSenderSeed;
  $fout!: Block[];
  $fraise_n!: (bigint | undefined);
  $fmalicious!: boolean;
  $frefill_count!: bigint;

  constructor(init: {
    $fparams: FerretParams,
    $fseed: FerretSenderSeed,
    $fout: Block[],
    $fraise_n: (bigint | undefined),
    $fmalicious: boolean,
    $frefill_count: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparams: __zeroValue(this.$fparams), $fseed: __zeroValue(this.$fseed), $fout: __zeroValue(this.$fout), $fraise_n: __zeroValue(this.$fraise_n), $fmalicious: __zeroValue(this.$fmalicious), $frefill_count: __zeroValue(this.$frefill_count) }) as this;
  }

  static from_seed(params: any, seed: any): Result<CotPoolSender, PoolSeedError>
  {
    return CotPoolSender.from_seed_mode(params, seed, false);
  }

  static from_seed_malicious(params: any, seed: any): Result<CotPoolSender, PoolSeedError>
  {
    return CotPoolSender.from_seed_mode(params, seed, true);
  }

  static from_seed_mode(params: any, seed: any, malicious: boolean): Result<CotPoolSender, PoolSeedError>
  {
    const __volar_try_0 = checked_seed_cot_count_mode(params, malicious);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const expected = __volar_try_0;
    if (!__equals(BigInt(seed.$fq.length), expected))     {
      return new PoolSeedError_SenderSeedCount(expected, BigInt(seed.$fq.length));
    }
    return new CotPoolSender({ $fparams: params, $fseed: seed, $fout: [] as any[], $fraise_n: undefined, $fmalicious: malicious, $frefill_count: 0n });
  }

  remaining(): bigint
  {
    return BigInt(this.$fout.length);
  }

  reprofile(params: any): Result<CotPoolSender, PoolSeedError>
  {
    const __volar_try_0 = checked_seed_cot_count_mode(this.$fparams, this.$fmalicious);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const old_seed_count = __volar_try_0;
    if (!__equals(BigInt(this.$fseed.$fq.length), old_seed_count))     {
      return new PoolSeedError_SenderSeedCount(old_seed_count, BigInt(this.$fseed.$fq.length));
    }
    const __volar_try_2 = checked_seed_cot_count_mode(params, this.$fmalicious);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const required = __volar_try_2;
    const __volar_try_3 = (((() => { const __a = BigInt(this.$fseed.$fq.length); const __b = BigInt(this.$fout.length); const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new PoolSeedError_InvalidParameters()));
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const available = __volar_try_3;
    if ((available < required))     {
      return new PoolSeedError_InsufficientCotMaterial(required, available);
    }
    const delta = this.$fseed.$fdelta;
    let cots = this.$fseed.$fq;
    cots.push(...(this.$fout));
    const out = ((__vector: any[], __at: number) => __vector.splice(__at))(cots, Number(required));
    this.$fparams = params;
    this.$fseed = new FerretSenderSeed({ $fdelta: delta, $fq: cots });
    this.$fout = out;
    this.$fraise_n = undefined;
    return this;
  }
}

export class CotPoolReceiver {
  $fparams!: FerretParams;
  $fseed!: FerretReceiverSeed;
  $fout_x!: boolean[];
  $fout_z!: Block[];
  $fmalicious!: boolean;

  constructor(init: {
    $fparams: FerretParams,
    $fseed: FerretReceiverSeed,
    $fout_x: boolean[],
    $fout_z: Block[],
    $fmalicious: boolean
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fparams: __zeroValue(this.$fparams), $fseed: __zeroValue(this.$fseed), $fout_x: __zeroValue(this.$fout_x), $fout_z: __zeroValue(this.$fout_z), $fmalicious: __zeroValue(this.$fmalicious) }) as this;
  }

  static from_seed(params: any, seed: any): Result<CotPoolReceiver, PoolSeedError>
  {
    return CotPoolReceiver.from_seed_mode(params, seed, false);
  }

  static from_seed_malicious(params: any, seed: any): Result<CotPoolReceiver, PoolSeedError>
  {
    return CotPoolReceiver.from_seed_mode(params, seed, true);
  }

  static from_seed_mode(params: any, seed: any, malicious: boolean): Result<CotPoolReceiver, PoolSeedError>
  {
    const __volar_try_0 = checked_seed_cot_count_mode(params, malicious);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const expected = __volar_try_0;
    if (!__equals(BigInt(seed.$fu.length), expected))     {
      return new PoolSeedError_ReceiverChoiceCount(expected, BigInt(seed.$fu.length));
    }
    if (!__equals(BigInt(seed.$fw.length), expected))     {
      return new PoolSeedError_ReceiverSeedCount(expected, BigInt(seed.$fw.length));
    }
    return new CotPoolReceiver({ $fparams: params, $fseed: seed, $fout_x: [] as any[], $fout_z: [] as any[], $fmalicious: malicious });
  }

  remaining(): bigint
  {
    return BigInt(this.$fout_x.length);
  }

  reprofile(params: any): Result<CotPoolReceiver, PoolSeedError>
  {
    const __volar_try_0 = checked_seed_cot_count_mode(this.$fparams, this.$fmalicious);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const old_seed_count = __volar_try_0;
    if (!__equals(BigInt(this.$fseed.$fu.length), old_seed_count))     {
      return new PoolSeedError_ReceiverChoiceCount(old_seed_count, BigInt(this.$fseed.$fu.length));
    }
    if (!__equals(BigInt(this.$fseed.$fw.length), old_seed_count))     {
      return new PoolSeedError_ReceiverSeedCount(old_seed_count, BigInt(this.$fseed.$fw.length));
    }
    if (!__equals(BigInt(this.$fout_x.length), BigInt(this.$fout_z.length)))     {
      return new PoolSeedError_ReceiverOutputCountMismatch(BigInt(this.$fout_x.length), BigInt(this.$fout_z.length));
    }
    const __volar_try_4 = checked_seed_cot_count_mode(params, this.$fmalicious);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const required = __volar_try_4;
    const __volar_try_5 = (((() => { const __a = BigInt(this.$fseed.$fu.length); const __b = BigInt(this.$fout_x.length); const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new PoolSeedError_InvalidParameters()));
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const available = __volar_try_5;
    if ((available < required))     {
      return new PoolSeedError_InsufficientCotMaterial(required, available);
    }
    let choices = this.$fseed.$fu;
    choices.push(...(this.$fout_x));
    let values = this.$fseed.$fw;
    values.push(...(this.$fout_z));
    const output_choices = ((__vector: any[], __at: number) => __vector.splice(__at))(choices, Number(required));
    const output_values = ((__vector: any[], __at: number) => __vector.splice(__at))(values, Number(required));
    this.$fparams = params;
    this.$fseed = new FerretReceiverSeed({ $fu: choices, $fw: values });
    this.$fout_x = output_choices;
    this.$fout_z = output_values;
    return this;
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

  static random_mod_order<R>(rng: any): bigint
  {
    const v = fieldBitor(fieldShl(BigInt(rng.next_u32()), 32n), BigInt(rng.next_u32()));
    return (v % fieldSub(TOY_P, 1n));
  }

  static random_scalar<R>(rng: any): bigint
  {
    const lo = BigInt(rng.next_u32());
    const hi = BigInt(rng.next_u32());
    return (fieldBitor(fieldShl(hi, 32n), lo) % fieldSub(TOY_P, 1n));
  }

  static scalar_add(a: bigint, b: bigint): bigint
  {
    const q = fieldSub(TOY_P, 1n);
    return (fieldAdd((a % q), (b % q)) % q);
  }

  static scalar_eq(a: bigint, b: bigint): boolean
  {
    return __equals((a % fieldSub(TOY_P, 1n)), (b % fieldSub(TOY_P, 1n)));
  }

  static scalar_from_hash<D>(h: any): bigint
  {
    const out = [...Array.from(h.finalize(), BigInt)];
    let le = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
    (le).splice(0, (out.slice(0, Number(8n))).length, ...(out.slice(0, Number(8n))));
    return (u64_from_le_bytes(le) % fieldSub(TOY_P, 1n));
  }

  static scalar_mul(elt: any, k: bigint): ToyElement
  {
    return new ToyElement(toy_pow(elt[0], k));
  }

  static scalar_mul_scalar(a: bigint, b: bigint): bigint
  {
    const q = fieldSub(TOY_P, 1n);
    return (fieldMul((a % q), (b % q)) % q);
  }

  static scalar_sub(a: bigint, b: bigint): bigint
  {
    const q = fieldSub(TOY_P, 1n);
    return (fieldSub(fieldAdd((a % q), q), (b % q)) % q);
  }

  static scalar_zero(): bigint
  {
    return 0n;
  }

  static write_element<D>(elt: any, h: any)
  {
    h.update([(elt[0]) & 0xFFn, ((elt[0]) >> 8n) & 0xFFn, ((elt[0]) >> 16n) & 0xFFn, ((elt[0]) >> 24n) & 0xFFn]);
  }
}

export class IdealCotDyn<T> {
  $fn!: bigint;
  $fdelta!: DeltaLoweredDyn<any>;

  constructor(init: {
    $fn: bigint,
    $fdelta: DeltaLoweredDyn<any>
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

  static new<T>(n: bigint, delta: DeltaLoweredDyn<any>): IdealCotDyn<T>
  {
    return new IdealCotDyn<T>({ $fdelta: delta });
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

export class LweOtSenderMsgSecureLoweredDyn {
  $fn!: bigint;
  $fl!: bigint;
  $fu0!: Zq[][];
  $fv0!: Zq[];
  $fu1!: Zq[][];
  $fv1!: Zq[];

  constructor(init: {
    $fn: bigint,
    $fl: bigint,
    $fu0: Zq[][],
    $fv0: Zq[],
    $fu1: Zq[][],
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
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  static recv_finish(n: bigint, l: bigint, state: number /* LweBaseOtDyn::ReceiverState */, payload: number /* LweBaseOtDyn::PayloadMsg */): bigint[]
  {
    const bytes = lwe_ot_recv_decrypt_bytes(n, state, payload, l);
    let out = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
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
    const crs = LweOtCrsDyn.sample(n, rng);
    return [__clone(crs), crs];
  }
}

export class LweOtSenderMsgSecureDyn {
  $fu0!: Vec<Vec<Zq>>;
  $fv0!: Vec<Zq>;
  $fu1!: Vec<Vec<Zq>>;
  $fv1!: Vec<Zq>;

  constructor(init: {
    $fu0: Vec<Vec<Zq>>,
    $fv0: Vec<Zq>,
    $fu1: Vec<Vec<Zq>>,
    $fv1: Vec<Zq>
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fu0: __zeroValue(this.$fu0), $fv0: __zeroValue(this.$fv0), $fu1: __zeroValue(this.$fu1), $fv1: __zeroValue(this.$fv1) }) as this;
  }
}

export class LweBaseOtSecureDyn {
  [0]!: bigint;

  constructor(_0: bigint) {
    this[0] = _0;
  }
  __zero(): this {
    return new (this.constructor as any)(__zeroValue(this[0])) as this;
  }

  static recv_finish(n: bigint, l: bigint, state: number /* LweBaseOtSecureDyn::ReceiverState */, payload: number /* LweBaseOtSecureDyn::PayloadMsg */): bigint[]
  {
    const bytes = lwe_ot_recv_decrypt_bytes_secure(n, state, payload, l);
    let out = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    (out).splice(0, (bytes).length, ...(bytes));
    return out;
  }

  static recv_start<R>(n: bigint, l: bigint, rng: any, setup: number /* LweBaseOtSecureDyn::SetupMsg */, c: boolean): [number /* LweBaseOtSecureDyn::ReceiverState */, number /* LweBaseOtSecureDyn::RecvMsg */]
  {
    return lwe_ot_recv(n, rng, setup, c);
  }

  static sender_payload<R>(n: bigint, l: bigint, rng: any, state: number /* LweBaseOtSecureDyn::SenderState */, recv_msg: number /* LweBaseOtSecureDyn::RecvMsg */, m0: bigint[], m1: bigint[]): number /* LweBaseOtSecureDyn::PayloadMsg */
  {
    return lwe_ot_send_bytes_secure(n, rng, state, recv_msg, m0, m1);
  }

  static sender_setup<R>(n: bigint, l: bigint, rng: any): [number /* LweBaseOtSecureDyn::SenderState */, number /* LweBaseOtSecureDyn::SetupMsg */]
  {
    const crs = LweOtCrsDyn.sample(n, rng);
    return [__clone(crs), crs];
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
    const [r0s, xs, zs] = (take_random(rng, this.$fsender, this.$freceiver, BigInt(bits.length)))!;
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
    const [r0s, xs, zs] = (take_random(rng, this.$fsender, this.$freceiver, 1n))!;
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

  static setup<R>(rng_s: any, rng_r: any, params: any): OtStack
  {
    return OtStack.setup_impl(rng_s, rng_r, params, false);
  }

  static setup_impl<R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng_s: any, rng_r: any, params: any, malicious: boolean): OtStack
  {
    const m = params.seed_cot_count(malicious);
    let bits = Array.from({length: Number(m)}, () => false);
    for (let __mut_2 = 0n; __mut_2 < BigInt(bits.length); __mut_2 += 1n) {
    {
      bits[Number(__mut_2)] = __equals(fieldBitand(rng_r.next_u32(), 1n), 1n);
    }}
    let delta_msg = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    for (const chunk of __chunks(delta_msg, Number(4n)))     {
      (chunk).splice(0, ([(rng_s.next_u32()) & 0xFFn, ((rng_s.next_u32()) >> 8n) & 0xFFn, ((rng_s.next_u32()) >> 16n) & 0xFFn, ((rng_s.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng_s.next_u32()) & 0xFFn, ((rng_s.next_u32()) >> 8n) & 0xFFn, ((rng_s.next_u32()) >> 16n) & 0xFFn, ((rng_s.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
    }
    const out = softspoken_cot_extend_base(ctx, 16n, rng_s, rng_r, bits, delta_msg);
    let q = ([] as any[]);
    let w = ([] as any[]);
    for (let j = 0n; j < m; j += 1n)     {
      (q).push(out.$fsender_r0[Number(j)]);
      (w).push(out.$freceiver_v[Number(j)]);
    }
    let stack = new OtStack({ $fsender: new CotPoolSender({ $fparams: params, $fseed: new FerretSenderSeed({ $fdelta: delta_msg, $fq: q }), $fout: [] as any[], $fraise_n: undefined, $fmalicious: malicious, $frefill_count: 0n }), $freceiver: new CotPoolReceiver({ $fparams: params, $fseed: new FerretReceiverSeed({ $fu: bits, $fw: w }), $fout_x: [] as any[], $fout_z: [] as any[], $fmalicious: malicious }) });
    const _ = rng_r;
    return stack;
  }

  static setup_malicious<R>(rng_s: any, rng_r: any, params: any): OtStack
  {
    return OtStack.setup_impl(rng_s, rng_r, params, true);
  }

  static setup_with_base<R>(rng_s: any, rng_r: any, params: any): OtStack
  {
    return OtStack.setup_impl(rng_s, rng_r, params, false);
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
    const max_addr_bits = fieldSub(BigInt(64n), 1n);
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
    let __volar_match_result_5 ;
    const __volar_match_5 = (() => { const __a = big_n; const __b = 2n; const __result = __a * __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })();
    if (__volar_match_5 !== null && __volar_match_5 !== undefined) {
      const capacity = __volar_match_5;
      __volar_match_result_5 = capacity;
    }
    {
      return new TfheBootstrapTableError_RingCapacityExceeded();
    }
    const capacity = __volar_match_result_5;
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
    if ((!__equals(input.slice(0, Number(8n)), MAGIC) || !__equals(u16_from_le_bytes([input[Number(8n)], input[Number(9n)]]), VERSION)))     {
      return new FrameError_UnsupportedFormat();
    }
    const __volar_try_2 = try_from(input[Number(10n)]);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const stage = __volar_try_2;
    let offset: bigint = 11n;
    let take_32 = () => (() => {
  let value = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  (value).splice(0, (input.slice(Number(offset), Number(fieldAdd(offset, 32n)))).length, ...(input.slice(Number(offset), Number(fieldAdd(offset, 32n)))));
  offset = fieldAdd(offset, 32n);
  return value;
})();
    const parameter_fingerprint = take_32();
    const session_id = take_32();
    const manifest_digest = take_32();
    const use_counter = u64_from_le_bytes((input.slice(Number(offset), Number(fieldAdd(offset, 8n))))!);
    offset = fieldAdd(offset, 8n);
    const payload_len = BigInt(u32_from_le_bytes((input.slice(Number(offset), Number(fieldAdd(offset, 4n))))!));
    offset = fieldAdd(offset, 4n);
    if ((payload_len > max_payload))     {
      return new FrameError_PayloadTooLarge();
    }
    if (!__equals(BigInt(input.length), (() => { const __a = offset; const __b = payload_len; const __result = __a + __b; return __result < 0n ? 0n : (__result > 18446744073709551615n ? 18446744073709551615n : __result); })()))     {
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
    out.push(...([(BigInt(BigInt(this.$fpayload.length))) & 0xFFn, ((BigInt(BigInt(this.$fpayload.length))) >> 8n) & 0xFFn, ((BigInt(BigInt(this.$fpayload.length))) >> 16n) & 0xFFn, ((BigInt(BigInt(this.$fpayload.length))) >> 24n) & 0xFFn]));
    out.push(...(this.$fpayload));
    return out;
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
    return __chunks(selected, Number(3n)).filter((__chunk: any) => __chunk.length === Number(3n)).map((chunk: any) => (() => {
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
    const __volar_try_0 = LabelBatchDyn.new(16n, pairs, offset);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const batch = __volar_try_0;
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
      return new LabelBatchPaddingError_TooFewSlots(slots, used);
    }
    let differences = __clone(this.$fdifferences);
    let zeroes = __clone(this.$fzeroes);
    (() => { let __r = differences; for (let __i = __r.length; __i < Number(slots); __i++) __r.push(0n); return __r; })();
    (() => { let __r = zeroes; for (let __i = __r.length; __i < Number(slots); __i++) __r.push(0n); return __r; })();
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
      return new LabelBatchPaddingError_ChoiceLengthMismatch(this.$flabel_count, BigInt(choices.length));
    }
    let out = EncodedLabelBatch.expanded_choices(choices);
    (() => { let __r = out; for (let __i = __r.length; __i < Number(this.slots()); __i++) __r.push(false); return __r; })();
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
    for (const [index, pair] of pairs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      if (!__equals(pair.$fone, Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((byte: any) => fieldBitxor(pair.$fzero[Number(byte)], offset[Number(byte)]))))       {
        return new BatchError_MismatchedPair(index);
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
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(output.length); __zip_mut_2 += 1n) {
      const pair = this.$fpairs[Number(__zip_mut_2)];
      const choice = choices[Number(__zip_mut_2)];
      {
        output[Number(__zip_mut_2)] = (() => { if (choice) {
  return pair.$fone;
} else {
  return pair.$fzero;
} })();
      }}
    return undefined;
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
}

export class ZeroNoise {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
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
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(this.$ffirst.length); __zip_mut_2 += 1n) {
      const right = other.$ffirst[Number(__zip_mut_2)];
      {
        this.$ffirst[Number(__zip_mut_2)] = add_mod(this.$ffirst[Number(__zip_mut_2)], right, ring.$fparameters.$fplaintext_modulus);
      }}
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(this.$fsecond.length); __zip_mut_2 += 1n) {
      const right = other.$fsecond[Number(__zip_mut_2)];
      {
        this.$fsecond[Number(__zip_mut_2)] = add_mod(this.$fsecond[Number(__zip_mut_2)], right, ring.$fparameters.$fdelta);
      }}
  }

  degree(): bigint
  {
    return BigInt(this.$ffirst.length);
  }

  negate_assign(ring: any)
  {
    for (let __mut_2 = 0n; __mut_2 < BigInt(this.$ffirst.length); __mut_2 += 1n) {
    {
      if (!__equals(this.$ffirst[Number(__mut_2)], 0n))       {
        this.$ffirst[Number(__mut_2)] = fieldSub(ring.$fparameters.$fplaintext_modulus, this.$ffirst[Number(__mut_2)]);
      }
    }}
    for (let __mut_2 = 0n; __mut_2 < BigInt(this.$fsecond.length); __mut_2 += 1n) {
    {
      if (!__equals(this.$fsecond[Number(__mut_2)], 0n))       {
        this.$fsecond[Number(__mut_2)] = fieldSub(ring.$fparameters.$fdelta, this.$fsecond[Number(__mut_2)]);
      }
    }}
  }

  product(ring: any, other: any): Polynomial
  {
    let output = ring.zero();
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(output.$ffirst.length); __zip_mut_2 += 1n) {
      const left = this.$ffirst[Number(__zip_mut_2)];
      const right = other.$ffirst[Number(__zip_mut_2)];
      {
        output.$ffirst[Number(__zip_mut_2)] = mul_mod(left, right, ring.$fparameters.$fplaintext_modulus);
      }}
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(output.$fsecond.length); __zip_mut_2 += 1n) {
      const left = this.$fsecond[Number(__zip_mut_2)];
      const right = other.$fsecond[Number(__zip_mut_2)];
      {
        output.$fsecond[Number(__zip_mut_2)] = mul_mod(left, right, ring.$fparameters.$fdelta);
      }}
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
    for (let __mut_2 = 0n; __mut_2 < BigInt(output.$ffirst.length); __mut_2 += 1n) {
    {
      output.$ffirst[Number(__mut_2)] = mul_mod(output.$ffirst[Number(__mut_2)], first, ring.$fparameters.$fplaintext_modulus);
    }}
    for (let __mut_2 = 0n; __mut_2 < BigInt(output.$fsecond.length); __mut_2 += 1n) {
    {
      output.$fsecond[Number(__mut_2)] = mul_mod(output.$fsecond[Number(__mut_2)], second, ring.$fparameters.$fdelta);
    }}
    return output;
  }

  sub_assign(ring: any, other: any)
  {
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(this.$ffirst.length); __zip_mut_2 += 1n) {
      const right = other.$ffirst[Number(__zip_mut_2)];
      {
        this.$ffirst[Number(__zip_mut_2)] = sub_mod(this.$ffirst[Number(__zip_mut_2)], right, ring.$fparameters.$fplaintext_modulus);
      }}
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(this.$fsecond.length); __zip_mut_2 += 1n) {
      const right = other.$fsecond[Number(__zip_mut_2)];
      {
        this.$fsecond[Number(__zip_mut_2)] = sub_mod(this.$fsecond[Number(__zip_mut_2)], right, ring.$fparameters.$fdelta);
      }}
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
    if ((((!__equals(BigInt(first.$flhe_state.length), p.$fgadget_digits) || !__equals(BigInt(first.$flhe_ciphertext.length), fieldMul(p.$fwidth, p.$fgadget_digits))) || !__equals(BigInt(first.$flenc_ciphertext.length), fieldMul(fieldMul(fieldMul(p.levels(), p.$fwidth), 2n), p.$fgadget_digits))) || first.$flhe_state.concat(first.$flhe_ciphertext).concat(first.$flenc_ciphertext).some((poly) => !__equals(poly.degree(), p.$fdegree))))     {
      return new Error_LengthMismatch();
    }
    return undefined;
  }

  check_second(second: any): Result<void, Error>
  {
    const p = this.$fring.$fparameters;
    if (((!__equals(BigInt(second.$flhe_ciphertext.length), p.$fwidth) || !__equals(second.$flhe_state.degree(), p.$fdegree)) || second.$flhe_ciphertext.some((poly) => !__equals(poly.degree(), p.$fdegree))))     {
      return new Error_LengthMismatch();
    }
    return undefined;
  }

  dec(first: any, second: any, key: any, choices: boolean[]): Result<Vec<bigint>, Error>
  {
    const __volar_try_0 = this.check_first(first);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const __volar_try_1 = this.check_second(second);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    if (!__equals(key.$fkey.degree(), this.$fring.$fparameters.$fdegree))     {
      return new Error_LengthMismatch();
    }
    const __volar_try_3 = this.$fring.encode_choices(choices);
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const choice = __volar_try_3;
    const __volar_try_4 = this.lenc_digest(choice);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const tree = __volar_try_4;
    const __volar_try_5 = this.$fring.decompose(tree.$fdigest);
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const digits = __volar_try_5;
    let result = ([] as any[]);
    for (let index = 0n; index < this.$fring.$fparameters.$fwidth; index += 1n)     {
      const row = first.$flhe_ciphertext.slice(Number(fieldMul(index, this.$fring.$fparameters.$fgadget_digits)), Number(fieldMul(fieldAdd(index, 1n), this.$fring.$fparameters.$fgadget_digits)));
      const __volar_try_1 = inner_product(this.$fring, row, digits);
      if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
      let value = __volar_try_1;
      value.add_assign(this.$fring, second.$flhe_ciphertext[Number(index)]);
      value.sub_assign(this.$fring, this.$fpublic.$flhe_a[Number(index)].product(this.$fring, key.$fkey));
      (result).push(value);
    }
    const __volar_try_8 = this.lenc_eval(first.$flenc_ciphertext, tree);
    if (__volar_try_8 instanceof __VolarError) return __volar_try_8;
    const correction = __volar_try_8;
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(result.length); __zip_mut_2 += 1n) {
      const delta = correction[Number(__zip_mut_2)];
      {
        result[Number(__zip_mut_2)].sub_assign(this.$fring, delta);
      }}
    return this.$fring.decode_messages(result);
  }

  enc1(ctx: { defaultA: () => any }, l1: bigint[], random: any, noise: any): Result<FirstCiphertext, Error>
  {
    const __volar_try_0 = this.$fring.encode_messages(l1);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const messages = __volar_try_0;
    const __volar_try_1 = this.lenc_enc(ctx, messages, random, noise);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const [random_vector, lenc_ciphertext] = __volar_try_1;
    const __volar_try_2 = this.lhe_enc1(ctx, random_vector, random, noise);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const [lhe_state, lhe_ciphertext] = __volar_try_2;
    return new FirstCiphertext({ $flhe_state: lhe_state, $flhe_ciphertext: lhe_ciphertext, $flenc_ciphertext: lenc_ciphertext });
  }

  enc2(ctx: { defaultA: () => any }, l2: bigint[], random: any, noise: any): Result<SecondCiphertext, Error>
  {
    const __volar_try_0 = this.$fring.encode_messages(l2);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    let messages = __volar_try_0;
    for (let __mut_2 = 0n; __mut_2 < BigInt(messages.length); __mut_2 += 1n) {
    {
      const __volar_try_0 = this.$fring.add_noise(ctx, messages[Number(__mut_2)], noise, LARGE_NOISE_STANDARD_DEVIATION, LARGE_NOISE_MAX_DEVIATION);
      if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    }}
    const __volar_try_2 = this.lhe_enc2(ctx, messages, random, noise);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const [lhe_state, lhe_ciphertext] = __volar_try_2;
    return new SecondCiphertext({ $flhe_state: lhe_state, $flhe_ciphertext: lhe_ciphertext });
  }

  keygen(first: any, second: any, choices: boolean[]): Result<SelectionKey, Error>
  {
    const __volar_try_0 = this.check_first(first);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const __volar_try_1 = this.check_second(second);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const __volar_try_2 = this.$fring.encode_choices(choices);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const choice = __volar_try_2;
    const __volar_try_3 = this.lenc_digest(choice);
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const tree = __volar_try_3;
    const __volar_try_4 = this.$fring.decompose(tree.$fdigest);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const digits = __volar_try_4;
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
    let tree = Array.from({length: Number(fieldMul(fieldSub(fieldMul(2n, p.$fwidth), 1n), p.$fgadget_digits))}, () => this.$fring.zero());
    for (const [index, value] of choice.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const __volar_try_0 = this.$fring.decompose(value);
      if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
      const digits = __volar_try_0;
      const base = fieldMul(fieldAdd(fieldSub(p.$fwidth, 1n), index), p.$fgadget_digits);
      (() => { const __dst = tree.slice(Number(base), Number(fieldAdd(base, p.$fgadget_digits))); __dst.splice(0, __dst.length, ...(digits)); return __dst; })();
    }
    let digest = this.$fring.zero();
    for (const node of (Array.from({length: Number(fieldSub(p.$fwidth, 1n) - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())     {
      const children = fieldMul(fieldAdd(fieldMul(2n, node), 1n), p.$fgadget_digits);
      const __volar_try_1 = inner_product(this.$fring, this.$fpublic.$flenc_b, tree.slice(Number(children), Number(fieldAdd(children, fieldMul(2n, p.$fgadget_digits)))));
      if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
      let parent = __volar_try_1;
      parent.negate_assign(this.$fring);
      if (__equals(node, 0n))       {
        digest = parent;
      } else       {
        const __volar_try_0 = this.$fring.decompose(parent);
        if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
        const digits = __volar_try_0;
        const base = fieldMul(node, p.$fgadget_digits);
        (() => { const __dst = tree.slice(Number(base), Number(fieldAdd(base, p.$fgadget_digits))); __dst.splice(0, __dst.length, ...(digits)); return __dst; })();
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
    const __volar_try_2 = p.levels();
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const levels = __volar_try_2;
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
    for (let __mut_2 = 0n; __mut_2 < BigInt(ciphertext.length); __mut_2 += 1n) {
    {
      const __volar_try_0 = this.$fring.add_noise(ctx, ciphertext[Number(__mut_2)], noise, SMALL_NOISE_STANDARD_DEVIATION, SMALL_NOISE_MAX_DEVIATION);
      if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    }}
    return [random_vector, ciphertext];
  }

  lenc_eval(ciphertext: Polynomial[], tree: any): Result<Vec<Polynomial>, Error>
  {
    const p = this.$fring.$fparameters;
    const __volar_try_1 = p.levels();
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const levels = __volar_try_1;
    if ((!__equals(BigInt(ciphertext.length), fieldMul(fieldMul(fieldMul(levels, p.$fwidth), 2n), p.$fgadget_digits)) || !__equals(BigInt(tree.$ftree.length), fieldMul(fieldSub(fieldMul(2n, p.$fwidth), 1n), p.$fgadget_digits))))     {
      return new Error_LengthMismatch();
    }
    let delta = ([] as any[]);
    for (let row = 0n; row < p.$fwidth; row += 1n)     {
      const __volar_try_0 = inner_product(this.$fring, ciphertext.slice(Number(fieldMul(fieldMul(row, 2n), p.$fgadget_digits)), Number(fieldMul(fieldMul(fieldAdd(row, 1n), 2n), p.$fgadget_digits))), tree.$ftree.slice(Number(p.$fgadget_digits), Number(fieldMul(3n, p.$fgadget_digits))));
      if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
      let value = __volar_try_0;
      for (let level = 1n; level < levels; level += 1n)       {
        const ciphertext_base = fieldMul(fieldMul(fieldAdd(fieldMul(level, p.$fwidth), row), 2n), p.$fgadget_digits);
        const tree_base = fieldMul(fieldAdd(fieldMul(fieldSub(fieldAdd(fieldShr(row, fieldSub(levels, level)), fieldShl(1n, level)), 1n), 2n), 1n), p.$fgadget_digits);
        const __volar_try_2 = inner_product(this.$fring, ciphertext.slice(Number(ciphertext_base), Number(fieldAdd(ciphertext_base, fieldMul(2n, p.$fgadget_digits)))), tree.$ftree.slice(Number(tree_base), Number(fieldAdd(tree_base, fieldMul(2n, p.$fgadget_digits)))));
        if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
        const term = __volar_try_2;
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
    for (const [index, message] of messages.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      for (let digit = 0n; digit < p.$fgadget_digits; digit += 1n)       {
        let value = this.$fpublic.$flhe_a[Number(index)].product(this.$fring, state[Number(digit)]);
        value.add_assign(this.$fring, message.scaled(this.$fring, p.$fgadget_base, digit));
        const __volar_try_2 = this.$fring.add_noise(ctx, value, noise, SMALL_NOISE_STANDARD_DEVIATION, SMALL_NOISE_MAX_DEVIATION);
        if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
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
    const __volar_try_1 = this.$fring.uniform(random);
    if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
    const state = __volar_try_1;
    let ciphertext = ([] as any[]);
    for (const [a, message] of this.$fpublic.$flhe_a.map((__a: any, __i: number) => [__a, messages[__i]] as [typeof __a, any]))     {
      let value = a.product(this.$fring, state);
      value.add_assign(this.$fring, message);
      const __volar_try_2 = this.$fring.add_noise(ctx, value, noise, LARGE_NOISE_STANDARD_DEVIATION, LARGE_NOISE_MAX_DEVIATION);
      if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
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
    const __volar_try_0 = Ring.new(ctx, parameters);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const ring = __volar_try_0;
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
}

export class BitVoleDyn<T> {
  $fn!: bigint;
  $fu!: Bit[];
  $fv!: any[];

  constructor(init: {
    $fn: bigint,
    $fu: Bit[],
    $fv: any[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fu: __zeroValue(this.$fu), $fv: __zeroValue(this.$fv) }) as this;
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

  static new_state<T>(ctx: { defaultT: () => any }): any
  {
    return ctx.defaultT();
  }
}

export class ChallengeKeyDyn<T> {
  $fr1!: any;
  $fr2!: any;
  $fr3!: any;

  constructor(init: {
    $fr1: any,
    $fr2: any,
    $fr3: any
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
  $fkey!: ChallengeKeyDyn<any>;
  $fproduce!: number /* H::State */;
  $fconsume!: number /* H::State */;

  constructor(init: {
    $fkey: ChallengeKeyDyn<any>,
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

  static new<T>(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, key: ChallengeKeyDyn<any>): MemoryCheckStateDyn<T, H>
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

export class PolyLoweredDyn<T> {
  $fn!: bigint;
  $fc0!: any;
  $fc1!: any[];

  constructor(init: {
    $fn: bigint,
    $fc0: any,
    $fc1: any[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fc0: __zeroValue(this.$fc0), $fc1: __zeroValue(this.$fc1) }) as this;
  }
}

export class PolyInputPoolDyn<T> {
  $fn!: bigint;
  $fx!: bigint;
  $finputs!: any[];
  $findices!: bigint[][];

  constructor(init: {
    $fn: bigint,
    $fx: bigint,
    $finputs: any[],
    $findices: bigint[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fx: __zeroValue(this.$fx), $finputs: __zeroValue(this.$finputs), $findices: __zeroValue(this.$findices) }) as this;
  }
}

export class VopeDyn<T> {
  $fn!: bigint;
  $fk!: bigint;
  $fu!: any[][];
  $fv!: any[];

  constructor(init: {
    $fn: bigint,
    $fk: bigint,
    $fu: any[][],
    $fv: any[]
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

  static constant<T>(n: bigint, v: any[]): VopeDyn<T>
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

  mul(rhs: DeltaLoweredDyn<any>)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return new QDyn({ $fq: this.$fu.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).reduce((a: any, [i, b]: any) => (() => {
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

  mul_generalized(ctx: { defaultT: () => any }, k2: bigint, other: VopeDyn<any>)
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
    return this.remap(this.$fn, (a: bigint) => wrappingSub(a, n_param, 64));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    const k: bigint = this.$fk;
    return this.remap(this.$fn, (a: bigint) => wrappingAdd(a, n_param, 64));
  }

  scale<T>(f: (arg: boolean) => any): VopeDyn<any>
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
  return new BitsInBytes(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(8n, BigInt(n_param)))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  const next = __clone(v[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(8n, BigInt(n_param)))));
})()), $fn: 0n, $fk: 1n });
    } else {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(u[Number(l)][Number(i)]);
  const next = __clone(u[Number(l)][Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(64n, BigInt(n_param)))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(v[Number(i)]);
  const next = __clone(v[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(64n, BigInt(n_param)))));
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
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(v[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(v[Number(i)]);
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})()), $fn: 0n, $fk: 1n });
    } else {
      const n: bigint = this.$fn;
      const k: bigint = this.$fk;
      const { $fu: u, $fv: v } = this;
      return new VopeDyn({ $fu: Array.from({length: Number(k - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(u[Number(l)][Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(u[Number(l)][Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})());
})()), $fv: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(v[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(v[Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})()), $fn: 0n, $fk: 1n });
    }
  }
}

export class DeltaLoweredDyn<T> {
  $fn!: bigint;
  $fdelta!: any[];

  constructor(init: {
    $fn: bigint,
    $fdelta: any[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fdelta: __zeroValue(this.$fdelta) }) as this;
  }
}

export class QDyn<T> {
  $fn!: bigint;
  $fq!: any[];

  constructor(init: {
    $fn: bigint,
    $fq: any[]
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
    return this.remap(this.$fn, (a: bigint) => wrappingSub(a, n_param, 64));
  }

  rotate_right(n_param: bigint)
  {
    const n: bigint = this.$fn;
    return this.remap(this.$fn, (a: bigint) => wrappingAdd(a, n_param, 64));
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
  return new BitsInBytes(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(8n, BigInt(n_param)))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const b = __clone(q[Number(i)]);
  const next = __clone(q[Number((fieldAdd(i, 1n) % n_param))]);
  return new BitsInBytes64(fieldBitor(fieldShl(b, BigInt(n_param)), fieldShr(next, fieldSub(64n, BigInt(n_param)))));
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
  return new BitsInBytes(fieldBitor(fieldShl(prev, fieldSub(8n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})()), $fn: 0n });
    } else {
      const n: bigint = this.$fn;
      const { $fq: q } = this;
      return new QDyn({ $fq: Array.from({length: Number(n_param - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const prev = __clone(q[Number((fieldSub(fieldAdd(i, n_param), 1n) % n_param))]);
  const b = __clone(q[Number(i)]);
  return new BitsInBytes64(fieldBitor(fieldShl(prev, fieldSub(64n, BigInt(n_param))), fieldShr(b, BigInt(n_param))));
})()), $fn: 0n });
    }
  }
}

export class WarpTranscript {
  $fsponge!: SpecSponge;

  constructor(init: {
    $fsponge: SpecSponge
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fsponge: __zeroValue(this.$fsponge) }) as this;
  }

  absorb_array(n: bigint, data: bigint[])
  {
    let sponge = __clone(this.$fsponge);
    spec_sponge_absorb_array(n, sponge, data);
    this.$fsponge = sponge;
  }

  absorb_field(value: Galois128)
  {
    this.absorb_len_prefixed(le16(value).$fn, le16(value));
  }

  absorb_fields(n: bigint, values: Galois128[])
  {
    this.absorb_usize(n);
    for (let i = 0n; i < n; i += 1n)     {
      this.absorb_field(values[Number(i)]);
    }
  }

  absorb_len_prefixed(n: bigint, data: bigint[])
  {
    const len_bytes = [(BigInt(n)) & 0xFFn, ((BigInt(n)) >> 8n) & 0xFFn, ((BigInt(n)) >> 16n) & 0xFFn, ((BigInt(n)) >> 24n) & 0xFFn];
    this.absorb_array(n, Array.from({length: Number(8n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => len_bytes[Number(i)]));
    this.absorb_array(n, data);
  }

  absorb_poly(sc: bigint, cnt: bigint, poly: WarpUniDyn)
  {
    this.absorb_usize(cnt);
    for (let i = 0n; i < cnt; i += 1n)     {
      this.absorb_field(poly.$fevaluations[Number(i)]);
    }
  }

  absorb_usize(value: bigint)
  {
    const bytes = [(BigInt(value)) & 0xFFn, ((BigInt(value)) >> 8n) & 0xFFn, ((BigInt(value)) >> 16n) & 0xFFn, ((BigInt(value)) >> 24n) & 0xFFn];
    this.absorb_array(Array.from({length: Number(8n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => bytes[Number(i)]).$fn, Array.from({length: Number(8n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => bytes[Number(i)]));
  }

  static default(): WarpTranscript
  {
    return warp_transcript_new();
  }

  domain_sep(n: bigint, label: bigint[]): WarpTranscript
  {
    let sponge = __clone(this.$fsponge);
    spec_sponge_absorb_array(n, sponge, label);
    this.$fsponge = sponge;
    return this;
  }

  squeeze_field_grid(r: bigint, c: bigint): Galois128[][]
  {
    let reader = spec_sponge_finalize_xof(this.$fsponge, 31n);
    return Array.from({length: Number(r - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  return Array.from({length: Number(c - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  let bytes: bigint[] = Array.from({length: Number(16n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(reader, bytes);
  let b16 = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < 16n; i += 1n)   {
    b16[Number(i)] = bytes[Number(i)];
  }
  return new Galois128(u128_from_le_bytes(b16));
})());
})());
  }

  squeeze_fields(n: bigint): Galois128[]
  {
    let reader = spec_sponge_finalize_xof(this.$fsponge, 31n);
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  let bytes: bigint[] = Array.from({length: Number(16n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(n, reader, bytes);
  let b16 = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < 16n; i += 1n)   {
    b16[Number(i)] = bytes[Number(i)];
  }
  return new Galois128(u128_from_le_bytes(b16));
})());
  }

  squeeze_shift_and_xi(shifts: bigint, logr: bigint, logdom: bigint): [bigint[], Galois128[]]
  {
    let reader = spec_sponge_finalize_xof(this.$fsponge, 31n);
    const shifts_1 = Array.from({length: Number(shifts - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  let value = 0n;
  for (let position = 0n; position < (fieldAdd(logdom, 7n) / 8n); position += 1n)   {
    let byte: bigint[] = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
    spec_xof_read_array(reader, byte);
    value = fieldBitor(value, fieldShl(BigInt(byte[Number(0n)]), fieldMul(8n, position)));
  }
  return BigInt(fieldBitand(value, fieldSub(fieldShl(1n, logdom), 1n)));
})());
    const xi = Array.from({length: Number(logr - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  let bytes: bigint[] = Array.from({length: Number(16n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(reader, bytes);
  let b16 = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < 16n; i += 1n)   {
    b16[Number(i)] = bytes[Number(i)];
  }
  return new Galois128(u128_from_le_bytes(b16));
})());
    return [shifts_1, xi];
  }
}

export class WarpMerkleTreeDyn {
  $fnodes!: bigint;
  $fleaves!: bigint;
  $fnodes!: WarpDigest[];

  constructor(init: {
    $fnodes: bigint,
    $fleaves: bigint,
    $fnodes: WarpDigest[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fnodes: __zeroValue(this.$fnodes), $fleaves: __zeroValue(this.$fleaves), $fnodes: __zeroValue(this.$fnodes) }) as this;
  }
}

export class WarpAuthPathDyn {
  $flog2_leaves!: bigint;
  $fsiblings!: WarpDigest[];

  constructor(init: {
    $flog2_leaves: bigint,
    $fsiblings: WarpDigest[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flog2_leaves: __zeroValue(this.$flog2_leaves), $fsiblings: __zeroValue(this.$fsiblings) }) as this;
  }
}

export class WarpTermDyn {
  $fdeg!: bigint;
  $fcoeff!: Galois128;
  $fvars!: bigint[];
  $fn_vars!: bigint;

  constructor(init: {
    $fdeg: bigint,
    $fcoeff: Galois128,
    $fvars: bigint[],
    $fn_vars: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fdeg: __zeroValue(this.$fdeg), $fcoeff: __zeroValue(this.$fcoeff), $fvars: __zeroValue(this.$fvars), $fn_vars: __zeroValue(this.$fn_vars) }) as this;
  }
}

export class WarpConstraintDyn {
  $fterms!: bigint;
  $fdeg!: bigint;
  $fterms!: WarpTermDyn[];
  $fn_terms!: bigint;

  constructor(init: {
    $fterms: bigint,
    $fdeg: bigint,
    $fterms: WarpTermDyn[],
    $fn_terms: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fterms: __zeroValue(this.$fterms), $fdeg: __zeroValue(this.$fdeg), $fterms: __zeroValue(this.$fterms), $fn_terms: __zeroValue(this.$fn_terms) }) as this;
  }
}

export class WarpIndexDyn {
  $fm!: bigint;
  $fterms!: bigint;
  $fdeg!: bigint;
  $fconstraints!: WarpConstraintDyn[];
  $fpublic_len!: bigint;
  $fwitness_len!: bigint;
  $fdegree_bound!: bigint;

  constructor(init: {
    $fm: bigint,
    $fterms: bigint,
    $fdeg: bigint,
    $fconstraints: WarpConstraintDyn[],
    $fpublic_len: bigint,
    $fwitness_len: bigint,
    $fdegree_bound: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fm: __zeroValue(this.$fm), $fterms: __zeroValue(this.$fterms), $fdeg: __zeroValue(this.$fdeg), $fconstraints: __zeroValue(this.$fconstraints), $fpublic_len: __zeroValue(this.$fpublic_len), $fwitness_len: __zeroValue(this.$fwitness_len), $fdegree_bound: __zeroValue(this.$fdegree_bound) }) as this;
  }
}

export class WarpWitnessDyn {
  $fk!: bigint;
  $fx!: bigint;
  $fpublic!: Galois128[];
  $fwitness!: Galois128[];

  constructor(init: {
    $fk: bigint,
    $fx: bigint,
    $fpublic: Galois128[],
    $fwitness: Galois128[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fk: __zeroValue(this.$fk), $fx: __zeroValue(this.$fx), $fpublic: __zeroValue(this.$fpublic), $fwitness: __zeroValue(this.$fwitness) }) as this;
  }
}

export class WarpIdentityCode {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  encode(message: Galois128[]): Galois128[]
  {
    return __clone(message);
  }
}

export class WarpRepetitionCode {

  constructor(init: {
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({  }) as this;
  }

  encode(message: Galois128[]): Galois128[]
  {
    return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => message[Number((i % k))]);
  }
}

export class WarpUniDyn {
  $fcap!: bigint;
  $fevaluations!: Galois128[];
  $fcount!: bigint;

  constructor(init: {
    $fcap: bigint,
    $fevaluations: Galois128[],
    $fcount: bigint
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fcap: __zeroValue(this.$fcap), $fevaluations: __zeroValue(this.$fevaluations), $fcount: __zeroValue(this.$fcount) }) as this;
  }
}

export class WarpAccInstanceDyn {
  $flogn!: bigint;
  $fbeta!: bigint;
  $froot!: WarpDigest;
  $falpha!: Galois128[];
  $fmu!: Galois128;
  $fbeta!: Galois128[];
  $feta!: Galois128;

  constructor(init: {
    $flogn: bigint,
    $fbeta: bigint,
    $froot: WarpDigest,
    $falpha: Galois128[],
    $fmu: Galois128,
    $fbeta: Galois128[],
    $feta: Galois128
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $flogn: __zeroValue(this.$flogn), $fbeta: __zeroValue(this.$fbeta), $froot: __zeroValue(this.$froot), $falpha: __zeroValue(this.$falpha), $fmu: __zeroValue(this.$fmu), $fbeta: __zeroValue(this.$fbeta), $feta: __zeroValue(this.$feta) }) as this;
  }
}

export class WarpAccWitnessDyn {
  $fn!: bigint;
  $fk!: bigint;
  $fnodes!: bigint;
  $ftree!: WarpMerkleTreeDyn;
  $fcodeword!: Galois128[];
  $fwitness!: Galois128[];

  constructor(init: {
    $fn: bigint,
    $fk: bigint,
    $fnodes: bigint,
    $ftree: WarpMerkleTreeDyn,
    $fcodeword: Galois128[],
    $fwitness: Galois128[]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fk: __zeroValue(this.$fk), $fnodes: __zeroValue(this.$fnodes), $ftree: __zeroValue(this.$ftree), $fcodeword: __zeroValue(this.$fcodeword), $fwitness: __zeroValue(this.$fwitness) }) as this;
  }
}

export class WarpAccDyn {
  $fn!: bigint;
  $fk!: bigint;
  $flogn!: bigint;
  $fbeta!: bigint;
  $fnodes!: bigint;
  $finstance!: WarpAccInstanceDyn;
  $fwitness!: WarpAccWitnessDyn;

  constructor(init: {
    $fn: bigint,
    $fk: bigint,
    $flogn: bigint,
    $fbeta: bigint,
    $fnodes: bigint,
    $finstance: WarpAccInstanceDyn,
    $fwitness: WarpAccWitnessDyn
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $fn: __zeroValue(this.$fn), $fk: __zeroValue(this.$fk), $flogn: __zeroValue(this.$flogn), $fbeta: __zeroValue(this.$fbeta), $fnodes: __zeroValue(this.$fnodes), $finstance: __zeroValue(this.$finstance), $fwitness: __zeroValue(this.$fwitness) }) as this;
  }
}

export class WarpProofDyn {
  $ffresh!: bigint;
  $fell!: bigint;
  $fa!: bigint;
  $fs!: bigint;
  $fshifts!: bigint;
  $fsc!: bigint;
  $flogn!: bigint;
  $finitial_root!: WarpDigest;
  $ffresh_mus!: Galois128[];
  $ffirst_sumcheck!: WarpUniDyn[];
  $fnu0!: Galois128;
  $food_answers!: Galois128[];
  $fsecond_sumcheck!: WarpUniDyn[];
  $finitial_paths!: WarpAuthPathDyn[];
  $faccumulator_paths!: WarpAuthPathDyn[][];
  $fopenings!: Galois128[][];

  constructor(init: {
    $ffresh: bigint,
    $fell: bigint,
    $fa: bigint,
    $fs: bigint,
    $fshifts: bigint,
    $fsc: bigint,
    $flogn: bigint,
    $finitial_root: WarpDigest,
    $ffresh_mus: Galois128[],
    $ffirst_sumcheck: WarpUniDyn[],
    $fnu0: Galois128,
    $food_answers: Galois128[],
    $fsecond_sumcheck: WarpUniDyn[],
    $finitial_paths: WarpAuthPathDyn[],
    $faccumulator_paths: WarpAuthPathDyn[][],
    $fopenings: Galois128[][]
  }) {
    Object.assign(this, init);
  }
  __zero(): this {
    return new (this.constructor as any)({ $ffresh: __zeroValue(this.$ffresh), $fell: __zeroValue(this.$fell), $fa: __zeroValue(this.$fa), $fs: __zeroValue(this.$fs), $fshifts: __zeroValue(this.$fshifts), $fsc: __zeroValue(this.$fsc), $flogn: __zeroValue(this.$flogn), $finitial_root: __zeroValue(this.$finitial_root), $ffresh_mus: __zeroValue(this.$ffresh_mus), $ffirst_sumcheck: __zeroValue(this.$ffirst_sumcheck), $fnu0: __zeroValue(this.$fnu0), $food_answers: __zeroValue(this.$food_answers), $fsecond_sumcheck: __zeroValue(this.$fsecond_sumcheck), $finitial_paths: __zeroValue(this.$finitial_paths), $faccumulator_paths: __zeroValue(this.$faccumulator_paths), $fopenings: __zeroValue(this.$fopenings) }) as this;
  }
}

export class BoundaryError_InvalidPlan extends __VolarError { constructor(public _0: PlanError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class BoundaryError_InvalidWireEncoding extends __VolarError { constructor(public $fk_max: bigint, public $flog_q_lwe: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fk_max), __zeroValue(this.$flog_q_lwe)) as this; }
}
export class BoundaryError_InputCount extends __VolarError { constructor(public $fexpected: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export class BoundaryError_CiphertextInputCount extends __VolarError { constructor(public $fexpected: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export class BoundaryError_InputBufferCapacity extends __VolarError { constructor(public $frequired: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$frequired), __zeroValue(this.$factual)) as this; }
}
export class BoundaryError_WireArenaTooShort extends __VolarError { constructor(public $frequired: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$frequired), __zeroValue(this.$factual)) as this; }
}
export class BoundaryError_CellCount extends __VolarError { constructor(public $fexpected: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export class BoundaryError_Workspace extends __VolarError { constructor(public _0: PlanWorkspaceError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type BoundaryError = BoundaryError_InvalidPlan | BoundaryError_InvalidWireEncoding | BoundaryError_InputCount | BoundaryError_CiphertextInputCount | BoundaryError_InputBufferCapacity | BoundaryError_WireArenaTooShort | BoundaryError_CellCount | BoundaryError_Workspace;

export class LutError_AddressShapeInvalid extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LutError_ArityExceedsCircuitMax extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LutError_ShapeUnsupported extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type LutError = LutError_AddressShapeInvalid | LutError_ArityExceedsCircuitMax | LutError_ShapeUnsupported;

export class PlanWorkspaceError_InvalidPlan extends __VolarError { constructor(public _0: PlanError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class PlanWorkspaceError_CapacityOverflow extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class PlanWorkspaceError_InputCount extends __VolarError { constructor(public $fexpected: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export class PlanWorkspaceError_CellCount extends __VolarError { constructor(public $fexpected: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export type PlanWorkspaceError = PlanWorkspaceError_InvalidPlan | PlanWorkspaceError_CapacityOverflow | PlanWorkspaceError_InputCount | PlanWorkspaceError_CellCount;

export class PlanOp_Const { constructor(public $fout: WireId, public $fvalue: boolean) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fout), __zeroValue(this.$fvalue)) as this; }
}
export class PlanOp_Not { constructor(public $finput: WireId, public $fout: WireId) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$finput), __zeroValue(this.$fout)) as this; }
}
export class PlanOp_Lut { constructor(public $finputs: Vec<bigint>, public $ftable: LutId, public $fout: WireId) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$finputs), __zeroValue(this.$ftable), __zeroValue(this.$fout)) as this; }
}
export class PlanOp_CircuitBootstrap { constructor(public $finput: WireId, public $fout: RgswId) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$finput), __zeroValue(this.$fout)) as this; }
}
export class PlanOp_RgswMux { constructor(public $fsel: RgswId, public $fthen_cell: CellId, public $felse_cell: CellId, public $fout: CellId) {}
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

export class PlanError_BadTableShape extends __VolarError { constructor(public $ftable: LutId) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$ftable)) as this; }
}
export class PlanError_ArityExceedsKMax extends __VolarError { constructor(public $ftable: LutId) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$ftable)) as this; }
}
export class PlanError_BadReference extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class PlanError_BadOutput extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class PlanError_BudgetInconsistent extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type PlanError = PlanError_BadTableShape | PlanError_ArityExceedsKMax | PlanError_BadReference | PlanError_BadOutput | PlanError_BudgetInconsistent;

export class EncodeError_InvalidPlan extends __VolarError { constructor(public _0: PlanError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class EncodeError_TooLarge extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type EncodeError = EncodeError_InvalidPlan | EncodeError_TooLarge;

export class DecodeError_BadMagic extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_UnsupportedVersion extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_UnknownTag extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_Truncated extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_TooLarge extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_TrailingBytes extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DecodeError_InvalidPlan extends __VolarError { constructor(public _0: PlanError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type DecodeError = DecodeError_BadMagic | DecodeError_UnsupportedVersion | DecodeError_UnknownTag | DecodeError_Truncated | DecodeError_TooLarge | DecodeError_TrailingBytes | DecodeError_InvalidPlan;

class __VolarEnum_Stitch {

  bits(this: Stitch): bigint
  {
    return (() => { const __match = this; if (__match instanceof Self_OneBit) { return 1n; } else { return 5n; } })();
  }

  check_five(this: Stitch): Result<boolean, StitchError>
  {
    return (() => { const __match = this; if (__match instanceof Self_OneBit) { return new StitchError_WrongVariant(); } else { const b = __match._0;
return (() => {
  if (!__equals(fieldBitand(b, !31n), 0n))   {
    return new StitchError_NonCanonicalBit();
  }
  const s = fieldBitand(b, 1n);
  const expected = Stitch.five(__equals(s, 1n));
  if (!__equals(this, expected))   {
    return new StitchError_NonCanonicalBit();
  }
  return __equals(s, 1n);
})(); } })();
  }

  correction(this: Stitch): boolean
  {
    return (() => { const __match = this; if (__match instanceof Self_OneBit) { const b = __match._0;
return b; } else { const b = __match._0;
return __equals(fieldBitand(b, 1n), 1n); } })();
  }
}
export class Stitch_OneBit extends __VolarEnum_Stitch { constructor(public _0: boolean) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class Stitch_FiveBits extends __VolarEnum_Stitch { constructor(public _0: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type Stitch = Stitch_OneBit | Stitch_FiveBits;

class __VolarEnum_StitchError extends __VolarError {

  fmt(this: StitchError, f: Formatter): Result
  {
    return (() => { const __match = this; if (true) { return f.write_str("stitching record stream is exhausted"); } else if (true) { return f.write_str("stitching record variant mismatch"); } else if (true) { return f.write_str("stitching record is not canonical"); } else if (true) { return f.write_str("free-XOR payload exceeds reduction bound"); } else { return f.write_str("underlying SWHE error"); } })();
  }
}
export class StitchError_Exhausted extends __VolarEnum_StitchError { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class StitchError_WrongVariant extends __VolarEnum_StitchError { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class StitchError_NonCanonicalBit extends __VolarEnum_StitchError { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class StitchError_PayloadOverflow extends __VolarEnum_StitchError { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class StitchError_Crypto extends __VolarEnum_StitchError { constructor(public _0: SwheError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type StitchError = StitchError_Exhausted | StitchError_WrongVariant | StitchError_NonCanonicalBit | StitchError_PayloadOverflow | StitchError_Crypto;

class __VolarEnum_Gate {

  operands(this: Gate): [bigint, bigint]
  {
    return (() => { const __match = this; if (true /* pattern Or([Struct { kind: Custom("Self::And"), fields: [("a", Ident { mutable: false, name: "a", subpat: None }), ("b", Ident { mutable: false, name: "b", subpat: None })], rest: false }, Struct { kind: Custom("Self::Xor"), fields: [("a", Ident { mutable: false, name: "a", subpat: None }), ("b", Ident { mutable: false, name: "b", subpat: None })], rest: false }]) */) { return [a, b]; } else { const a = __match.$fa;
return [a, a]; } })();
  }

  out_at(this: Gate, inputs: bigint, index: bigint): bigint
  {
    return fieldAdd(inputs, index);
  }
}
export class Gate_And extends __VolarEnum_Gate { constructor(public $fa: bigint, public $fb: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fa), __zeroValue(this.$fb)) as this; }
}
export class Gate_Xor extends __VolarEnum_Gate { constructor(public $fa: bigint, public $fb: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fa), __zeroValue(this.$fb)) as this; }
}
export class Gate_Not extends __VolarEnum_Gate { constructor(public $fa: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fa)) as this; }
}
export type Gate = Gate_And | Gate_Xor | Gate_Not;

export class LatticeError_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LatticeError_InvalidParameters extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type LatticeError = LatticeError_LengthMismatch | LatticeError_InvalidParameters;

export class RnsError_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class RnsError_InvalidParameters extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type RnsError = RnsError_LengthMismatch | RnsError_InvalidParameters;

class __VolarEnum_Variant {

  bits_per_and(this: Variant): bigint
  {
    return (() => { const __match = this; if (true /* pattern Or([Wild, Wild]) */) { return 1n; } else { return 5n; } })();
  }

  bits_per_xor(this: Variant): bigint
  {
    return (() => { const __match = this; if (true) { return 1n; } else { return 0n; } })();
  }
}
export class Variant_Small extends __VolarEnum_Variant { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Variant_Fast extends __VolarEnum_Variant { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Variant_Tiny extends __VolarEnum_Variant { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type Variant = Variant_Small | Variant_Fast | Variant_Tiny;

class __VolarEnum_GateRecord {

  bits(this: GateRecord): bigint
  {
    return (() => { const __match = this; if (true /* pattern Or([TupleStruct { kind: Custom("Self::SmallAnd"), elems: [Wild] }, TupleStruct { kind: Custom("Self::SmallXor"), elems: [Wild] }]) */) { return 1n; } else if (__match instanceof Self_FastAnd) { return 5n; } else { return 0n; } })();
  }
}
export class GateRecord_SmallAnd extends __VolarEnum_GateRecord { constructor(public _0: boolean) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class GateRecord_SmallXor extends __VolarEnum_GateRecord { constructor(public _0: boolean) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class GateRecord_FastAnd extends __VolarEnum_GateRecord { constructor(public $fmasks: boolean[], public $fdet: boolean) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fmasks), __zeroValue(this.$fdet)) as this; }
}
export class GateRecord_FastXor extends __VolarEnum_GateRecord { constructor() { super(); }
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type GateRecord = GateRecord_SmallAnd | GateRecord_SmallXor | GateRecord_FastAnd | GateRecord_FastXor;

export class AndRows_Small { constructor(public _0: RowCt[]) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class AndRows_Fast { constructor(public _0: RowCt[]) {}
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type AndRows = AndRows_Small | AndRows_Fast;

export class DriverError_BadCircuit extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DriverError_BadInputCount extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DriverError_MissingRecord extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class DriverError_WrongRecord extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type DriverError = DriverError_BadCircuit | DriverError_BadInputCount | DriverError_MissingRecord | DriverError_WrongRecord;

export class Error_InvalidParameters extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Arithmetic extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Randomness extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type Error = Error_InvalidParameters | Error_LengthMismatch | Error_Arithmetic | Error_Randomness;

export class Error_InvalidParameters extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_LevelMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Noise extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_NoiseSource extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Randomness extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Arithmetic extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type Error = Error_InvalidParameters | Error_LengthMismatch | Error_LevelMismatch | Error_Noise | Error_NoiseSource | Error_Randomness | Error_Arithmetic;

export class CirrusHashBackend_Blake2s {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class CirrusHashBackend_Ascon {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class CirrusHashBackend_Keccak {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type CirrusHashBackend = CirrusHashBackend_Blake2s | CirrusHashBackend_Ascon | CirrusHashBackend_Keccak;

class __VolarEnum_Sponge {

  absorb(this: Sponge, data: bigint[])
  {
    return (() => { const __match = this; if (__match instanceof Sponge_Shake128) { const h = __match._0;
return h.update(data); } else { const h = __match._0;
return h.update(data); } })();
  }

  squeeze(this: Sponge, ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n: bigint): Vec<bigint>
  {
    let out = Array.from({length: Number(n)}, () => ((0n) & 0xFFn));
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
}
export class Sponge_Shake128 extends __VolarEnum_Sponge { constructor(public _0: Shake128) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export class Sponge_Shake256 extends __VolarEnum_Sponge { constructor(public _0: Shake256) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type Sponge = Sponge_Shake128 | Sponge_Shake256;

export class GramOutput_Cleartext {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class GramOutput_Regarble {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type GramOutput = GramOutput_Cleartext | GramOutput_Regarble;

export class PoolSeedError_InvalidParameters extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class PoolSeedError_SenderSeedCount extends __VolarError { constructor(public $fexpected: bigint, public $ffound: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$ffound)) as this; }
}
export class PoolSeedError_ReceiverChoiceCount extends __VolarError { constructor(public $fexpected: bigint, public $ffound: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$ffound)) as this; }
}
export class PoolSeedError_ReceiverSeedCount extends __VolarError { constructor(public $fexpected: bigint, public $ffound: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$ffound)) as this; }
}
export class PoolSeedError_InsufficientCotMaterial extends __VolarError { constructor(public $frequired: bigint, public $favailable: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$frequired), __zeroValue(this.$favailable)) as this; }
}
export class PoolSeedError_ReceiverOutputCountMismatch extends __VolarError { constructor(public $fchoices: bigint, public $fvalues: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fchoices), __zeroValue(this.$fvalues)) as this; }
}
export type PoolSeedError = PoolSeedError_InvalidParameters | PoolSeedError_SenderSeedCount | PoolSeedError_ReceiverChoiceCount | PoolSeedError_ReceiverSeedCount | PoolSeedError_InsufficientCotMaterial | PoolSeedError_ReceiverOutputCountMismatch;

export class TfheBootstrapTableError_AddressWidthOutOfRange extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_TableLengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_RingCapacityExceeded extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_NegacyclicIncompatible extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class TfheBootstrapTableError_InputEncodingUnsupported extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type TfheBootstrapTableError = TfheBootstrapTableError_AddressWidthOutOfRange | TfheBootstrapTableError_TableLengthMismatch | TfheBootstrapTableError_RingCapacityExceeded | TfheBootstrapTableError_NegacyclicIncompatible | TfheBootstrapTableError_InputEncodingUnsupported;

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

export class FrameError_Truncated extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_UnsupportedFormat extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_UnknownStage extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_PayloadTooLarge extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class FrameError_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type FrameError = FrameError_Truncated | FrameError_UnsupportedFormat | FrameError_UnknownStage | FrameError_PayloadTooLarge | FrameError_LengthMismatch;

export class LabelEncodingError_FieldElementOutOfRange extends __VolarError { constructor(public $findex: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$findex)) as this; }
}
export class LabelEncodingError_NonCanonicalElement extends __VolarError { constructor(public $findex: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$findex)) as this; }
}
export type LabelEncodingError = LabelEncodingError_FieldElementOutOfRange | LabelEncodingError_NonCanonicalElement;

export class LabelBatchPaddingError_TooFewSlots extends __VolarError { constructor(public $fslots: bigint, public $fused: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fslots), __zeroValue(this.$fused)) as this; }
}
export class LabelBatchPaddingError_ChoiceLengthMismatch extends __VolarError { constructor(public $fexpected: bigint, public $factual: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$fexpected), __zeroValue(this.$factual)) as this; }
}
export type LabelBatchPaddingError = LabelBatchPaddingError_TooFewSlots | LabelBatchPaddingError_ChoiceLengthMismatch;

export class LabelBatchDecodeError_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class LabelBatchDecodeError_NonCanonicalLabel extends __VolarError { constructor(public _0: LabelEncodingError) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this._0)) as this; }
}
export type LabelBatchDecodeError = LabelBatchDecodeError_LengthMismatch | LabelBatchDecodeError_NonCanonicalLabel;

export class BatchError_EvenOffset extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class BatchError_MismatchedPair extends __VolarError { constructor(public $findex: bigint) { super(); }
  __zero(): this { return new (this.constructor as any)(__zeroValue(this.$findex)) as this; }
}
export class BatchError_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type BatchError = BatchError_EvenOffset | BatchError_MismatchedPair | BatchError_LengthMismatch;

export class Error_InvalidParameters extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_LengthMismatch extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_NonCanonicalPlaintext extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Randomness extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Noise extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export class Error_Arithmetic extends __VolarError {
  __zero(): this { return new (this.constructor as any)() as this; }
}
export type Error = Error_InvalidParameters | Error_LengthMismatch | Error_NonCanonicalPlaintext | Error_Randomness | Error_Noise | Error_Arithmetic;

export const GF8_POLY = 27n;
export const GF64_POLY = 27n;
export const GF128_POLY = 135n;
export const GF256_POLY = new U256([1061n, 0n, 0n, 0n]);
export const BLOCK_LEN = BLOCK;
export const ASCON_HASH256_IV = [11177464323724596865n, 5459383224871899602n, 12566513473065105434n, 4359436768738168243n, 1899470422303676269n];
export const ASCON_XOF128_IV = [15745374255817443307n, 14734905824818166121n, 16667980376626976305n, 1072114354614917324n, 16164673763700313054n];
export const ASCON_RC = [240n, 225n, 210n, 195n, 180n, 165n, 150n, 135n, 120n, 105n, 90n, 75n];
export const ASCON_128A_IV = 9259414062373011456n;
export const MAGIC = [86n, 66n, 80n, 49n];
export const VERSION = 1n;
export const MAX_ITEMS = fieldShl(1n, 20n);
export const MAX_PAYLOAD = 4n;
export const BLAKE2S_BLOCK = 64n;
export const BLAKE2S_OUT = 32n;
export const BLAKE2S_IV = [1779033703n, 3144134277n, 1013904242n, 2773480762n, 1359893119n, 2600822924n, 528734635n, 1541459225n];
export const BLAKE2S_SIGMA = [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 11n, 12n, 13n, 14n, 15n, 14n, 10n, 4n, 8n, 9n, 15n, 13n, 6n, 1n, 12n, 0n, 2n, 11n, 7n, 5n, 3n, 11n, 8n, 12n, 0n, 5n, 2n, 15n, 13n, 10n, 14n, 3n, 6n, 7n, 1n, 9n, 4n, 7n, 9n, 3n, 1n, 13n, 12n, 11n, 14n, 2n, 6n, 5n, 10n, 4n, 0n, 15n, 8n, 9n, 0n, 5n, 7n, 2n, 4n, 10n, 15n, 14n, 1n, 11n, 12n, 6n, 8n, 3n, 13n, 2n, 12n, 6n, 10n, 0n, 11n, 8n, 3n, 4n, 13n, 7n, 5n, 15n, 14n, 1n, 9n, 12n, 5n, 1n, 15n, 14n, 13n, 4n, 10n, 0n, 7n, 6n, 3n, 9n, 2n, 8n, 11n, 13n, 11n, 7n, 14n, 12n, 1n, 3n, 9n, 5n, 0n, 15n, 4n, 8n, 6n, 2n, 10n, 6n, 15n, 14n, 9n, 11n, 3n, 0n, 8n, 12n, 2n, 13n, 7n, 1n, 4n, 10n, 5n, 10n, 2n, 8n, 4n, 7n, 6n, 1n, 5n, 15n, 11n, 9n, 14n, 3n, 12n, 13n, 0n];
export const CIRrus_DEFAULT_HASH = new CirrusHashBackend_Blake2s();
export const SPEC_HASH_SOURCES_DIGEST = [59n, 250n, 37n, 4n, 132n, 107n, 133n, 254n, 62n, 74n, 37n, 150n, 236n, 234n, 123n, 65n, 69n, 103n, 215n, 190n, 249n, 112n, 36n, 187n, 243n, 172n, 72n, 254n, 220n, 135n, 147n, 41n];
export const P_LIMBS = [18446744073709551597n, 18446744073709551615n, 18446744073709551615n, 9223372036854775807n];
export const D_LIMBS = [8496970652267935907n, 31536524315187371n, 10144147576115030168n, 5909686906226998899n];
export const D2_LIMBS = [16993941304535871833n, 63073048630374742n, 1841551078520508720n, 2596001775599221991n];
export const D = fe_const(D_LIMBS);
export const D2 = fe_const(D2_LIMBS);
export const BASE_X_LIMBS = [14507833142362363162n, 7578651490590762930n, 13881468655802702940n, 2407515759118799870n];
export const BASE_Y_LIMBS = [7378697629483820632n, 7378697629483820646n, 7378697629483820646n, 7378697629483820646n];
export const L_LIMBS = [6346243789798364141n, 1503914060200516822n, 0n, 1152921504606846976n];
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
export const SHAKE128_RATE = 168n;
export const SHA3_256_RATE = 136n;
export const KECCAK_RNDC = [1n, 32898n, 9223372036854808714n, 9223372039002292224n, 32907n, 2147483649n, 9223372039002292353n, 9223372036854808585n, 138n, 136n, 2147516425n, 2147483658n, 2147516555n, 9223372036854775947n, 9223372036854808713n, 9223372036854808579n, 9223372036854808578n, 9223372036854775936n, 32778n, 9223372039002259466n, 9223372039002292353n, 9223372036854808704n, 2147483649n, 9223372039002292232n];
export const KECCAK_ROTC = [1n, 3n, 6n, 10n, 15n, 21n, 28n, 36n, 45n, 55n, 2n, 14n, 27n, 41n, 56n, 8n, 25n, 43n, 62n, 18n, 39n, 61n, 20n, 44n];
export const KECCAK_PILN = [10n, 7n, 11n, 17n, 18n, 3n, 5n, 16n, 8n, 21n, 24n, 4n, 15n, 23n, 19n, 13n, 12n, 2n, 20n, 14n, 22n, 9n, 6n, 1n];
export const LOCALITY = 10n;
export const FERRET_REG_TOY = new FerretParams({ $fn: 256n, $fk: 32n, $ft: 4n });
export const FERRET_REG_SETUP = new FerretParams({ $fn: 609728n, $fk: 36288n, $ft: 1269n });
export const FERRET_REG_MAIN = new FerretParams({ $fn: 10805248n, $fk: 589760n, $ft: 1319n });
export const FERRET_UNI_TOY = new FerretParams({ $fn: 256n, $fk: 32n, $ft: 4n });
export const FERRET_UNI_SETUP = new FerretParams({ $fn: 616092n, $fk: 37248n, $ft: 1254n });
export const FERRET_UNI_MAIN = new FerretParams({ $fn: 10616092n, $fk: 588160n, $ft: 1324n });
export const KAPPA_BITS = 128n;
export const KAPPA_BYTES = 16n;
export const TOY_P = 2147483647n;
export const TOY_G = 7n;
export const IKNP_KAPPA = 128n;
export const IKNP_KAPPA_BYTES = (IKNP_KAPPA / 8n);
export const LWE_N = 16n;
export const LWE_Q_BITS = 16n;
export const LWE_Q = fieldShl(1n, LWE_Q_BITS);
export const LWE_Q_MASK = fieldSub(LWE_Q, 1n);
export const LWE_NOISE_BOUND = 1n;
export const TAG_DOMAIN = [115n, 111n, 102n, 116n, 115n, 112n, 111n, 107n, 101n, 110n, 45n, 99n, 111n, 116n, 45n, 116n, 97n, 103n, 45n, 118n, 49n];
export const STACK_SEED_BYTES = IKNP_KAPPA_BYTES;
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
export const TAG_FERRET_CHECK_MASK = 13n;
export const TAG_FERRET_CHECK_HV = 14n;
export const Q4 = fieldShl(1n, 30n);
export const FALSE = 0n;
export const HEADER_BYTES = fieldAdd(fieldAdd(fieldAdd(fieldAdd(fieldAdd(fieldAdd(fieldAdd(8n, 2n), 1n), 32n), 32n), 32n), 8n), 4n);
export const SECURITY_BITS = 128n;
export const PAPER_PROFILE = new PaperProfile({ $fsecurity_bits: SECURITY_BITS, $fring_degree: 4096n, $fmodulus_bits: 109n, $fbatch_messages: 699050n });
export const REFERENCE_PLAINTEXT_MODULUS = 1125899906826241n;
export const REFERENCE_DELTA = 576460752303415297n;
export const SMALL_NOISE_STANDARD_DEVIATION = 4n;
export const SMALL_NOISE_MAX_DEVIATION = 512n;
export const LARGE_NOISE_STANDARD_DEVIATION = 1000n;
export const LARGE_NOISE_MAX_DEVIATION = 128000n;
export const REFERENCE_PARAMETERS = new Parameters({ $fdegree: 4096n, $fwidth: 512n, $fplaintext_modulus: REFERENCE_PLAINTEXT_MODULUS, $fdelta: REFERENCE_DELTA, $fgadget_base: fieldShl(1n, 28n), $fgadget_digits: 4n });
export const WARP_TRANSCRIPT_DOMAIN = [118n, 111n, 108n, 97n, 114n, 45n, 105n, 111n, 112n, 47n, 119n, 97n, 114n, 112n, 47n, 101n, 112n, 114n, 105n, 110n, 116n, 45n, 50n, 48n, 50n, 53n, 45n, 55n, 53n, 51n, 47n, 99n, 111n, 110n, 115n, 116n, 114n, 117n, 99n, 116n, 105n, 111n, 110n, 45n, 49n, 48n, 46n, 52n, 47n, 118n, 49n];

export function absorb_args_bases<D>(n: bigint, d: any, args: GarbleDyn[]): D
{
  return (() => { const __match = args.split_first(); if (true) { return d; } else { const first = __match[0];
const rest = __match[1];
return (() => {
  d.update(first.$fbase);
  return absorb_args_bases(n, d, rest);
})(); } })();
}

export function add4(a: bigint[], b: bigint[]): bigint[]
{
  let out = Array.from({length: Number(4n)}, () => 0n);
  let carry = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const s = fieldAdd(fieldAdd((a[Number(i)] as unknown as bigint), (b[Number(i)] as unknown as bigint)), carry);
    out[Number(i)] = BigInt(s);
    carry = fieldShr(s, 64n);
  }
  return out;
}

export function add_coeffs(a: bigint[], b: bigint[], modulus: bigint): Vec<bigint>
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => add_mod(x, y, modulus));
}

export function add_limb(a: any, b: any, q: bigint): Limb
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => add_mod(x, y, q));
}

export function add_mod(...__args: any[]): any {
  if (__args.length === 3) {
    const left = __args[0];
    const right = __args[1];
    const modulus = __args[2];
    return (() => {
  return BigInt((fieldAdd((left as unknown as bigint), (right as unknown as bigint)) % (modulus as unknown as bigint)));
})();
  } else
  if (__args.length === 3) {
    const left = __args[0];
    const right = __args[1];
    const modulus = __args[2];
    return (() => {
  const sum = fieldAdd(left, right);
  return (() => { if ((sum >= modulus)) {
  return fieldSub(sum, modulus);
} else {
  return sum;
} })();
})();
  }
  throw new Error("add_mod(): no matching variant for " + __args.length + " args");
}

export function add_mod_l(a: bigint[], b: bigint[]): bigint[]
{
  let s = add4(a, b);
  if (!__equals(cmp4(s, L_LIMBS), Less))   {
    s = sub4(s, L_LIMBS);
  }
  return s;
}

export function add_round_key(state: bigint[], round_key: bigint[])
{
  for (let i = 0n; i < BLOCK; i += 1n)   {
    state[Number(i)] = fieldBitxor(state[Number(i)], round_key[Number(i)]);
  }
}

export function add_rp(a: bigint[], b: bigint[], p: bigint): RpPoly
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => add_mod(x, y, p));
}

export function add_rq(a: bigint[], b: bigint[], q: bigint): RqPoly
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => add_mod(x, y, q));
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

export function aead_pad(n: bigint): bigint
{
  return fieldShl(128n, fieldSub(56n, fieldMul(8n, n)));
}

export function aes128_etm_open(key: bigint[], nonce: bigint[], ct: bigint[], tag: bigint[]): (Vec<bigint> | undefined)
{
  const roles = derive_roles(key);
  let msg = ([] as any[]);
  msg.push(...(nonce));
  msg.push(...(ct));
  msg.push(...([((fieldMul(BigInt(BigInt(ct.length)), 8n)) >> 24n) & 0xFFn, ((fieldMul(BigInt(BigInt(ct.length)), 8n)) >> 16n) & 0xFFn, ((fieldMul(BigInt(BigInt(ct.length)), 8n)) >> 8n) & 0xFFn, (fieldMul(BigInt(BigInt(ct.length)), 8n)) & 0xFFn]));
  const computed = cmac(roles.$fmac, msg);
  if (!__equals(computed, tag))   {
    return undefined;
  }
  let pt = ([] as any[]);
  for (const [i, chunk] of __chunks(ct, Number(BLOCK_LEN)).map((__enum_a: any, __enum_i: number) => [BigInt(__enum_i), __enum_a] as [bigint, typeof __enum_a]))   {
    const ks = ctr_block(roles.$fenc, nonce, BigInt(i));
    pt.push(...(chunk.map((__a: any, __i: number) => [__a, ks[__i]] as [typeof __a, any]).map(([c, k]: any) => fieldBitxor(c, k))));
  }
  return pt;
}

export function aes128_etm_seal(key: bigint[], nonce: bigint[], pt: bigint[]): [Vec<bigint>, bigint[]]
{
  const roles = derive_roles(key);
  let ct = ([] as any[]);
  for (const [i, chunk] of __chunks(pt, Number(BLOCK_LEN)).map((__enum_a: any, __enum_i: number) => [BigInt(__enum_i), __enum_a] as [bigint, typeof __enum_a]))   {
    const ks = ctr_block(roles.$fenc, nonce, BigInt(i));
    ct.push(...(chunk.map((__a: any, __i: number) => [__a, ks[__i]] as [typeof __a, any]).map(([p, k]: any) => fieldBitxor(p, k))));
  }
  let msg = ([] as any[]);
  msg.push(...(nonce));
  msg.push(...(ct));
  msg.push(...([((fieldMul(BigInt(BigInt(pt.length)), 8n)) >> 24n) & 0xFFn, ((fieldMul(BigInt(BigInt(pt.length)), 8n)) >> 16n) & 0xFFn, ((fieldMul(BigInt(BigInt(pt.length)), 8n)) >> 8n) & 0xFFn, (fieldMul(BigInt(BigInt(pt.length)), 8n)) & 0xFFn]));
  const tag = cmac(roles.$fmac, msg);
  return [ct, tag];
}

export function aes_ctr_prg(seed: bigint[], iv: bigint[], tweak: bigint, out_bytes: bigint): Vec<bigint>
{
  let iv_tweaked = iv;
  add_to_upper_word(iv_tweaked, tweak);
  const n_full = (out_bytes / BLOCK);
  const rem = (out_bytes % BLOCK);
  let out: Vec<bigint> = ([] as any[]);
  for (let i = 0n; i < n_full; i += 1n)   {
    const block_in = add_to_lower_word(iv_tweaked, BigInt(i));
    const ct = encrypt_block(seed, block_in);
    for (let b = 0n; b < BLOCK; b += 1n)     {
      (out).push(ct[Number(b)]);
    }
  }
  if ((rem > 0n))   {
    const block_in = add_to_lower_word(iv_tweaked, BigInt(n_full));
    const ct = encrypt_block(seed, block_in);
    let b = 0n;
    while ((b < rem))     {
      (out).push(ct[Number(b)]);
      b = fieldAdd(b, 1n);
    }
  }
  return out;
}

export function align_wire(ctx: any, wire: ExpandedWire, level: bigint): Result<ExpandedWire, StitchError>
{
  return new ExpandedWire({ $fct_a0: ctx.switch_to(wire.$fct_a0, level), $fct_pi: ctx.switch_to(wire.$fct_pi, level), $fslot_a0: wire.$fslot_a0, $fslot_pi: wire.$fslot_pi });
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

export function ascon128a_decrypt(key: bigint[], nonce: bigint[], ad: bigint[], ciphertext: bigint[], tag: bigint[]): (Vec<bigint> | undefined)
{
  const k1 = be_u64(key.slice(0, Number(8n)));
  const k2 = be_u64(key.slice(Number(8n), Number(16n)));
  let state = [ASCON_128A_IV, k1, k2, be_u64(nonce.slice(0, Number(8n))), be_u64(nonce.slice(Number(8n), Number(16n)))];
  ascon_p12(state);
  state[Number(3n)] = fieldBitxor(state[Number(3n)], k1);
  state[Number(4n)] = fieldBitxor(state[Number(4n)], k2);
  if (!(ad.length === 0))   {
    let i = 0n;
    while ((fieldSub(BigInt(ad.length), i) >= 16n))     {
      state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64(ad.slice(Number(i), Number(fieldAdd(i, 8n)))));
      state[Number(1n)] = fieldBitxor(state[Number(1n)], be_u64(ad.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))));
      ascon_p8(state);
      i = fieldAdd(i, 16n);
    }
    const rem = fieldSub(BigInt(ad.length), i);
    const [sidx, lastlen] = (() => { if ((rem >= 8n)) {
  state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64(ad.slice(Number(i), Number(fieldAdd(i, 8n)))));
  return [1n, fieldSub(rem, 8n)];
} else {
  return [0n, rem];
} })();
    state[Number(sidx)] = fieldBitxor(state[Number(sidx)], aead_pad(lastlen));
    if ((lastlen > 0n))     {
      const off = fieldAdd(i, (() => { if (__equals(sidx, 1n)) {
  return 8n;
} else {
  return 0n;
} })());
      state[Number(sidx)] = fieldBitxor(state[Number(sidx)], be_u64_partial(ad.slice(Number(off), Number(fieldAdd(off, lastlen)))));
    }
    ascon_p8(state);
  }
  state[Number(4n)] = fieldBitxor(state[Number(4n)], 1n);
  let plaintext = ([] as any[]);
  let i = 0n;
  while ((fieldSub(BigInt(ciphertext.length), i) >= 16n))   {
    plaintext.push(...([((fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) >> 24n) & 0xFFn, ((fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) >> 16n) & 0xFFn, ((fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) >> 8n) & 0xFFn, (fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) & 0xFFn]));
    state[Number(0n)] = be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n))));
    plaintext.push(...([((fieldBitxor(state[Number(1n)], be_u64(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))))) >> 24n) & 0xFFn, ((fieldBitxor(state[Number(1n)], be_u64(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))))) >> 16n) & 0xFFn, ((fieldBitxor(state[Number(1n)], be_u64(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))))) >> 8n) & 0xFFn, (fieldBitxor(state[Number(1n)], be_u64(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))))) & 0xFFn]));
    state[Number(1n)] = be_u64(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n))));
    ascon_p8(state);
    i = fieldAdd(i, 16n);
  }
  const rem = fieldSub(BigInt(ciphertext.length), i);
  if ((rem >= 8n))   {
    plaintext.push(...([((fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) >> 24n) & 0xFFn, ((fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) >> 16n) & 0xFFn, ((fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) >> 8n) & 0xFFn, (fieldBitxor(state[Number(0n)], be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n)))))) & 0xFFn]));
    state[Number(0n)] = be_u64(ciphertext.slice(Number(i), Number(fieldAdd(i, 8n))));
    const rest = fieldSub(rem, 8n);
    state[Number(1n)] = fieldBitxor(state[Number(1n)], aead_pad(rest));
    if ((rest > 0n))     {
      const pt1 = fieldBitxor(state[Number(1n)], be_u64_partial(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(fieldAdd(i, 8n), rest)))));
      plaintext.push(...([((pt1) >> 24n) & 0xFFn, ((pt1) >> 16n) & 0xFFn, ((pt1) >> 8n) & 0xFFn, (pt1) & 0xFFn].slice(0, Number(rest))));
      let s1 = [((state[Number(1n)]) >> 24n) & 0xFFn, ((state[Number(1n)]) >> 16n) & 0xFFn, ((state[Number(1n)]) >> 8n) & 0xFFn, (state[Number(1n)]) & 0xFFn];
      (s1.slice(0, Number(rest))).splice(0, (ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(fieldAdd(i, 8n), rest)))).length, ...(ciphertext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(fieldAdd(i, 8n), rest)))));
      state[Number(1n)] = be_u64(s1);
    }
  } else   {
    state[Number(0n)] = fieldBitxor(state[Number(0n)], aead_pad(rem));
    if ((rem > 0n))     {
      const pt0 = fieldBitxor(state[Number(0n)], be_u64_partial(ciphertext.slice(Number(i), Number(fieldAdd(i, rem)))));
      plaintext.push(...([((pt0) >> 24n) & 0xFFn, ((pt0) >> 16n) & 0xFFn, ((pt0) >> 8n) & 0xFFn, (pt0) & 0xFFn].slice(0, Number(rem))));
      let s0 = [((state[Number(0n)]) >> 24n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, (state[Number(0n)]) & 0xFFn];
      (s0.slice(0, Number(rem))).splice(0, (ciphertext.slice(Number(i), Number(fieldAdd(i, rem)))).length, ...(ciphertext.slice(Number(i), Number(fieldAdd(i, rem)))));
      state[Number(0n)] = be_u64(s0);
    }
  }
  state[Number(2n)] = fieldBitxor(state[Number(2n)], k1);
  state[Number(3n)] = fieldBitxor(state[Number(3n)], k2);
  ascon_p12(state);
  state[Number(3n)] = fieldBitxor(state[Number(3n)], k1);
  state[Number(4n)] = fieldBitxor(state[Number(4n)], k2);
  let expected = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  (expected.slice(0, Number(8n))).splice(0, ([((state[Number(3n)]) >> 24n) & 0xFFn, ((state[Number(3n)]) >> 16n) & 0xFFn, ((state[Number(3n)]) >> 8n) & 0xFFn, (state[Number(3n)]) & 0xFFn]).length, ...([((state[Number(3n)]) >> 24n) & 0xFFn, ((state[Number(3n)]) >> 16n) & 0xFFn, ((state[Number(3n)]) >> 8n) & 0xFFn, (state[Number(3n)]) & 0xFFn]));
  (expected.slice(Number(8n))).splice(0, ([((state[Number(4n)]) >> 24n) & 0xFFn, ((state[Number(4n)]) >> 16n) & 0xFFn, ((state[Number(4n)]) >> 8n) & 0xFFn, (state[Number(4n)]) & 0xFFn]).length, ...([((state[Number(4n)]) >> 24n) & 0xFFn, ((state[Number(4n)]) >> 16n) & 0xFFn, ((state[Number(4n)]) >> 8n) & 0xFFn, (state[Number(4n)]) & 0xFFn]));
  return (() => { if (__equals(expected, tag)) {
  return plaintext;
} else {
  return undefined;
} })();
}

export function ascon128a_encrypt(key: bigint[], nonce: bigint[], ad: bigint[], plaintext: bigint[]): [Vec<bigint>, bigint[]]
{
  const k1 = be_u64(key.slice(0, Number(8n)));
  const k2 = be_u64(key.slice(Number(8n), Number(16n)));
  let state = [ASCON_128A_IV, k1, k2, be_u64(nonce.slice(0, Number(8n))), be_u64(nonce.slice(Number(8n), Number(16n)))];
  ascon_p12(state);
  state[Number(3n)] = fieldBitxor(state[Number(3n)], k1);
  state[Number(4n)] = fieldBitxor(state[Number(4n)], k2);
  if (!(ad.length === 0))   {
    let i = 0n;
    while ((fieldSub(BigInt(ad.length), i) >= 16n))     {
      state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64(ad.slice(Number(i), Number(fieldAdd(i, 8n)))));
      state[Number(1n)] = fieldBitxor(state[Number(1n)], be_u64(ad.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))));
      ascon_p8(state);
      i = fieldAdd(i, 16n);
    }
    const rem = fieldSub(BigInt(ad.length), i);
    const [sidx, lastlen] = (() => { if ((rem >= 8n)) {
  state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64(ad.slice(Number(i), Number(fieldAdd(i, 8n)))));
  return [1n, fieldSub(rem, 8n)];
} else {
  return [0n, rem];
} })();
    state[Number(sidx)] = fieldBitxor(state[Number(sidx)], aead_pad(lastlen));
    if ((lastlen > 0n))     {
      const off = fieldAdd(i, (() => { if (__equals(sidx, 1n)) {
  return 8n;
} else {
  return 0n;
} })());
      state[Number(sidx)] = fieldBitxor(state[Number(sidx)], be_u64_partial(ad.slice(Number(off), Number(fieldAdd(off, lastlen)))));
    }
    ascon_p8(state);
  }
  state[Number(4n)] = fieldBitxor(state[Number(4n)], 1n);
  let ciphertext = ([] as any[]);
  let i = 0n;
  while ((fieldSub(BigInt(plaintext.length), i) >= 16n))   {
    state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64(plaintext.slice(Number(i), Number(fieldAdd(i, 8n)))));
    ciphertext.push(...([((state[Number(0n)]) >> 24n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, (state[Number(0n)]) & 0xFFn]));
    state[Number(1n)] = fieldBitxor(state[Number(1n)], be_u64(plaintext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(i, 16n)))));
    ciphertext.push(...([((state[Number(1n)]) >> 24n) & 0xFFn, ((state[Number(1n)]) >> 16n) & 0xFFn, ((state[Number(1n)]) >> 8n) & 0xFFn, (state[Number(1n)]) & 0xFFn]));
    ascon_p8(state);
    i = fieldAdd(i, 16n);
  }
  const rem = fieldSub(BigInt(plaintext.length), i);
  if ((rem >= 8n))   {
    state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64(plaintext.slice(Number(i), Number(fieldAdd(i, 8n)))));
    ciphertext.push(...([((state[Number(0n)]) >> 24n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, (state[Number(0n)]) & 0xFFn]));
    const rest = fieldSub(rem, 8n);
    state[Number(1n)] = fieldBitxor(state[Number(1n)], aead_pad(rest));
    if ((rest > 0n))     {
      state[Number(1n)] = fieldBitxor(state[Number(1n)], be_u64_partial(plaintext.slice(Number(fieldAdd(i, 8n)), Number(fieldAdd(fieldAdd(i, 8n), rest)))));
      const keystream = [((state[Number(1n)]) >> 24n) & 0xFFn, ((state[Number(1n)]) >> 16n) & 0xFFn, ((state[Number(1n)]) >> 8n) & 0xFFn, (state[Number(1n)]) & 0xFFn];
      ciphertext.push(...(keystream.slice(0, Number(rest))));
    }
  } else   {
    state[Number(0n)] = fieldBitxor(state[Number(0n)], aead_pad(rem));
    if ((rem > 0n))     {
      state[Number(0n)] = fieldBitxor(state[Number(0n)], be_u64_partial(plaintext.slice(Number(i), Number(fieldAdd(i, rem)))));
      const keystream = [((state[Number(0n)]) >> 24n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, (state[Number(0n)]) & 0xFFn];
      ciphertext.push(...(keystream.slice(0, Number(rem))));
    }
  }
  state[Number(2n)] = fieldBitxor(state[Number(2n)], k1);
  state[Number(3n)] = fieldBitxor(state[Number(3n)], k2);
  ascon_p12(state);
  state[Number(3n)] = fieldBitxor(state[Number(3n)], k1);
  state[Number(4n)] = fieldBitxor(state[Number(4n)], k2);
  let tag = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  (tag.slice(0, Number(8n))).splice(0, ([((state[Number(3n)]) >> 24n) & 0xFFn, ((state[Number(3n)]) >> 16n) & 0xFFn, ((state[Number(3n)]) >> 8n) & 0xFFn, (state[Number(3n)]) & 0xFFn]).length, ...([((state[Number(3n)]) >> 24n) & 0xFFn, ((state[Number(3n)]) >> 16n) & 0xFFn, ((state[Number(3n)]) >> 8n) & 0xFFn, (state[Number(3n)]) & 0xFFn]));
  (tag.slice(Number(8n))).splice(0, ([((state[Number(4n)]) >> 24n) & 0xFFn, ((state[Number(4n)]) >> 16n) & 0xFFn, ((state[Number(4n)]) >> 8n) & 0xFFn, (state[Number(4n)]) & 0xFFn]).length, ...([((state[Number(4n)]) >> 24n) & 0xFFn, ((state[Number(4n)]) >> 16n) & 0xFFn, ((state[Number(4n)]) >> 8n) & 0xFFn, (state[Number(4n)]) & 0xFFn]));
  return [ciphertext, tag];
}

export function ascon_hash256(input: bigint[]): bigint[]
{
  let state = ASCON_HASH256_IV;
  const n = BigInt(input.length);
  let i = 0n;
  while ((fieldSub(n, i) >= 8n))   {
    let w = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
    (w).splice(0, (input.slice(Number(i), Number(fieldAdd(i, 8n)))).length, ...(input.slice(Number(i), Number(fieldAdd(i, 8n)))));
    state[Number(0n)] = fieldBitxor(state[Number(0n)], u64_from_le_bytes(w));
    ascon_p12(state);
    i = fieldAdd(i, 8n);
  }
  const rem = fieldSub(n, i);
  let last = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
  (last.slice(0, Number(rem))).splice(0, (input.slice(Number(i))).length, ...(input.slice(Number(i))));
  state[Number(0n)] = fieldBitxor(state[Number(0n)], u64_from_le_bytes(last));
  state[Number(0n)] = fieldBitxor(state[Number(0n)], fieldShl(1n, fieldMul(8n, rem)));
  ascon_p12(state);
  let out = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  for (let j = 0n; j < 4n; j += 1n)   {
    (out.slice(Number(fieldMul(j, 8n)), Number(fieldAdd(fieldMul(j, 8n), 8n)))).splice(0, ([(state[Number(0n)]) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 24n) & 0xFFn]).length, ...([(state[Number(0n)]) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 24n) & 0xFFn]));
    if ((j < 3n))     {
      ascon_p12(state);
    }
  }
  return out;
}

export function ascon_hash256_array(n: bigint, input: bigint[]): bigint[]
{
  let state = ASCON_HASH256_IV;
  const n_1 = n;
  const num_full = (n_1 / 8n);
  for (let i = 0n; i < num_full; i += 1n)   {
    let w = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
    for (let j = 0n; j < 8n; j += 1n)     {
      w[Number(j)] = input[Number(fieldAdd(fieldMul(i, 8n), j))];
    }
    state[Number(0n)] = fieldBitxor(state[Number(0n)], u64_from_le_bytes(w));
    ascon_p12(state);
  }
  const rem = fieldSub(n_1, fieldMul(num_full, 8n));
  let last = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
  for (let j = 0n; j < 8n; j += 1n)   {
    const idx = fieldAdd(fieldMul(num_full, 8n), j);
    if ((idx < n_1))     {
      last[Number(j)] = input[Number(idx)];
    }
  }
  state[Number(0n)] = fieldBitxor(state[Number(0n)], u64_from_le_bytes(last));
  state[Number(0n)] = fieldBitxor(state[Number(0n)], fieldShl(1n, fieldMul(8n, rem)));
  ascon_p12(state);
  let out = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  for (let j = 0n; j < 4n; j += 1n)   {
    (out.slice(Number(fieldMul(j, 8n)), Number(fieldAdd(fieldMul(j, 8n), 8n)))).splice(0, ([(state[Number(0n)]) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 24n) & 0xFFn]).length, ...([(state[Number(0n)]) & 0xFFn, ((state[Number(0n)]) >> 8n) & 0xFFn, ((state[Number(0n)]) >> 16n) & 0xFFn, ((state[Number(0n)]) >> 24n) & 0xFFn]));
    if ((j < 3n))     {
      ascon_p12(state);
    }
  }
  return out;
}

export function ascon_p12(state: bigint[])
{
  for (let round = 0n; round < 12n; round += 1n)   {
    const new_ = ascon_round(state, ASCON_RC[Number(round)]);
    for (let i = 0n; i < 5n; i += 1n)     {
      state[Number(i)] = new_[Number(i)];
    }
  }
}

export function ascon_p8(state: bigint[])
{
  for (let round = 4n; round < 12n; round += 1n)   {
    const new_ = ascon_round(state, ASCON_RC[Number(round)]);
    for (let i = 0n; i < 5n; i += 1n)     {
      state[Number(i)] = new_[Number(i)];
    }
  }
}

export function ascon_round(x: bigint[], c: bigint): bigint[]
{
  const x0 = fieldBitxor(x[Number(0n)], x[Number(4n)]);
  const x2 = fieldBitxor(fieldBitxor(x[Number(2n)], x[Number(1n)]), c);
  const x4 = fieldBitxor(x[Number(4n)], x[Number(3n)]);
  const tx0 = fieldBitxor(x0, fieldBitand(!x[Number(1n)], x2));
  const tx1 = fieldBitxor(x[Number(1n)], fieldBitand(!x2, x[Number(3n)]));
  const tx2 = fieldBitxor(x2, fieldBitand(!x[Number(3n)], x4));
  const tx3 = fieldBitxor(x[Number(3n)], fieldBitand(!x4, x0));
  const tx4 = fieldBitxor(x4, fieldBitand(!x0, x[Number(1n)]));
  const tx1_1 = fieldBitxor(tx1, tx0);
  const tx3_1 = fieldBitxor(tx3, tx2);
  const tx0_1 = fieldBitxor(tx0, tx4);
  const y0 = fieldBitxor(tx0_1, fieldBitor(fieldShr(tx0_1, 9n), fieldShl(tx0_1, 55n)));
  const y1 = fieldBitxor(tx1_1, fieldBitor(fieldShr(tx1_1, 22n), fieldShl(tx1_1, 42n)));
  const y2 = fieldBitxor(tx2, fieldBitor(fieldShr(tx2, 5n), fieldShl(tx2, 59n)));
  const y3 = fieldBitxor(tx3_1, fieldBitor(fieldShr(tx3_1, 7n), fieldShl(tx3_1, 57n)));
  const y4 = fieldBitxor(tx4, fieldBitor(fieldShr(tx4, 34n), fieldShl(tx4, 30n)));
  return [fieldBitxor(tx0_1, fieldBitor(fieldShr(y0, 19n), fieldShl(y0, 45n))), fieldBitxor(tx1_1, fieldBitor(fieldShr(y1, 39n), fieldShl(y1, 25n))), !fieldBitxor(tx2, fieldBitor(fieldShr(y2, 1n), fieldShl(y2, 63n))), fieldBitxor(tx3_1, fieldBitor(fieldShr(y3, 10n), fieldShl(y3, 54n))), fieldBitxor(tx4, fieldBitor(fieldShr(y4, 7n), fieldShl(y4, 57n)))];
}

export function assert_one_check<T>(n: bigint, q: QDyn<any>, opening: any[], delta: DeltaLoweredDyn<any>): boolean
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    ok = (ok && __equals(fieldAdd(__clone(q.$fq[Number(i)]), __clone(opening[Number(i)])), __clone(delta.$fdelta[Number(i)])));
  }
  return ok;
}

export function be_u64(input: bigint[]): bigint
{
  let b = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
  (b).splice(0, (input).length, ...(input));
  return u64.from_be_bytes(b);
}

export function be_u64_partial(input: bigint[]): bigint
{
  let b = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
  (b.slice(0, Number(BigInt(input.length)))).splice(0, (input).length, ...(input));
  return u64.from_be_bytes(b);
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
  let acc = binfhe_rlwe_trivial(big_n, log_q, test_poly);
  if (!__equals(b_exp, 0n))   {
    acc = binfhe_rlwe_rotate(big_n, log_q, acc, fieldSub(two_n, b_exp));
  }
  for (const [i, row] of bsk.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const a_exp = exponent(log_q_lwe, big_n, ct.$fa[Number(i)]);
    if (!__equals(a_exp, 0n))     {
      const rotated = binfhe_rlwe_rotate(big_n, log_q, acc, a_exp);
      acc = binfhe_rgsw_cmux(row, rotated, acc);
    }
  }
  return acc;
}

export function binfhe_external_product(n: bigint, log: bigint, ell: bigint, base_log: bigint, c: BinfheRgswCiphertextDyn, ct: BinfheRlweCiphertextDyn): BinfheRlweCiphertextDyn
{
  const a_dec = gadget_poly_decompose(n, log, ell, base_log, ct.$fa);
  const b_dec = gadget_poly_decompose(n, log, ell, base_log, ct.$fb);
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
    out_a[Number(k)] = reduce(log, out_a[Number(k)]);
    out_b[Number(k)] = reduce(log, out_b[Number(k)]);
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
  return binfhe_rgsw_encrypt(big_n, log_q, bs_ell, bs_base_log, eta, !__equals(lwe_sk.$fkey[Number(i)], 0n), rlwe_sk, rng);
})());
  const ksk = new BinfheKeySwitchingKeyDyn({ $fksk: Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return Array.from({length: Number(ks_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const msg = BigInt(Math.imul(Number(rlwe_sk.$fkey[Number(i)]), Number(level_factor(log_mod_ks, ks_base_log, j))));
  return binfhe_lwe_encrypt_raw(n_lwe, log_mod_ks, eta, reduce(log_mod_ks, msg), lwe_sk, rng);
})());
})()), $fn_lwe: 0n, $fbig_n: 0n, $fks_ell: 0n });
  return new BinfheBootstrappingKeyDyn({ $fbsk: bsk, $fksk: ksk, $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n });
}

export function binfhe_gen_lwe_secret_key<R>(n: bigint, rng: any): BinfheLweSecretKeyDyn
{
  let key = Array.from({length: Number(n)}, () => ((0n) & 0xFFn));
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
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(key.length); __zip_mut_1 += 1n) {
    {
      key[Number(__zip_mut_1)] = BigInt(fieldBitand(rng.next_u32(), 1n));
    }}
  return new BinfheRlweSecretKeyDyn({ $fkey: key, $fn: 0n });
}

export function binfhe_key_switch<K>(n_lwe: bigint, big_n: bigint, log_mod_ks: bigint, ks_ell: bigint, ks_base_log: bigint, ct: BinfheLweCiphertextDyn, ksk: any): BinfheLweCiphertextDyn
{
  const ksk_rows = ksk.ksk_rows();
  let out_a = Array.from({length: Number(n_lwe)}, () => 0n);
  let out_b = ct.$fb;
  for (let i = 0n; i < big_n; i += 1n)   {
    const digits = gadget_decompose(log_mod_ks, ks_ell, ks_base_log, ct.$fa[Number(i)]);
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
    out_a[Number(k)] = reduce(log_mod_ks, out_a[Number(k)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: out_a, $fb: reduce(log_mod_ks, out_b), $fn: 0n });
}

export function binfhe_lut_read(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, addr_bits: bigint, table_len: bigint, k_max: bigint, addr: BinfheLweCiphertextDyn[], lut: LutDyn, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  const delta = wire_delta(log_q_lwe, k_max);
  if (lut.is_constant())   {
    return binfhe_trivial(n_lwe, log_q_lwe, lut.constant_value(), delta);
  }
  let combined = binfhe_trivial(n_lwe, log_q_lwe, false, 0n);
  for (const [j, bit] of addr.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const scaled = binfhe_lwe_scale(n_lwe, log_q_lwe, bit, fieldShl(1n, j));
    combined = binfhe_lwe_add(n_lwe, log_q_lwe, combined, scaled);
  }
  combined = binfhe_lwe_add_const(n_lwe, log_q_lwe, combined, (delta / 2n));
  return binfhe_pbs_core(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, combined, lut.test_polynomial(), bk);
}

export function binfhe_lut_read_dyn(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, inputs: BinfheLweCiphertextDyn[], table: boolean[], k_max: bigint, bk: BinfheBootstrappingKeyDyn): BinfheLweCiphertextDyn
{
  const delta = wire_delta(log_q_lwe, BigInt(k_max));
  if (table_is_constant(table))   {
    return binfhe_trivial(n_lwe, log_q_lwe, table[Number(0n)], delta);
  }
  const arity = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, BigInt(table.length))));
  const test_poly = fill_test_poly(big_n, table, arity, BigInt(k_max), log_q, log_q_lwe);
  let combined = binfhe_trivial(n_lwe, log_q_lwe, false, 0n);
  for (const [j, bit] of inputs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const scaled = binfhe_lwe_scale(n_lwe, log_q_lwe, bit, fieldShl(1n, j));
    combined = binfhe_lwe_add(n_lwe, log_q_lwe, combined, scaled);
  }
  combined = binfhe_lwe_add_const(n_lwe, log_q_lwe, combined, (delta / 2n));
  return binfhe_pbs_core(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, combined, test_poly, bk);
}

export function binfhe_lwe_add(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, y: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = torus_add(log_m, x.$fa[Number(i)], y.$fa[Number(i)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: torus_add(log_m, x.$fb, y.$fb), $fn: 0n });
}

export function binfhe_lwe_add_const(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, c: bigint): BinfheLweCiphertextDyn
{
  return new BinfheLweCiphertextDyn({ $fa: x.$fa, $fb: torus_add(log_m, x.$fb, c), $fn: 0n });
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
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(a.length); __zip_mut_1 += 1n) {
    {
      a[Number(__zip_mut_1)] = reduce(log_m, rng.next_u32());
    }}
  let dot = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(BigInt(sk.$fkey[Number(i)])))), 32);
  }
  const e = sample_error(log_m, eta, rng);
  const b = reduce(log_m, wrappingAdd(wrappingAdd(dot, e, 32), msg, 32));
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: b, $fn: 0n });
}

export function binfhe_lwe_neg(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = torus_neg(log_m, x.$fa[Number(i)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: torus_neg(log_m, x.$fb), $fn: 0n });
}

export function binfhe_lwe_scale(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, c: bigint): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = mul_exact(log_m, x.$fa[Number(i)], c);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: mul_exact(log_m, x.$fb, c), $fn: 0n });
}

export function binfhe_lwe_sub(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, y: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = torus_sub(log_m, x.$fa[Number(i)], y.$fa[Number(i)]);
  }
  return new BinfheLweCiphertextDyn({ $fa: a, $fb: torus_sub(log_m, x.$fb, y.$fb), $fn: 0n });
}

export function binfhe_not(n: bigint, log_m: bigint, x: BinfheLweCiphertextDyn, delta: bigint): BinfheLweCiphertextDyn
{
  let out = binfhe_lwe_neg(n, log_m, x);
  out.$fb = torus_add(log_m, out.$fb, delta);
  return out;
}

export function binfhe_pbs_core<BK>(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, ct: BinfheLweCiphertextDyn, test_poly: bigint[], bk: any): BinfheLweCiphertextDyn
{
  const acc = binfhe_blind_rotate(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, ct, test_poly, bk.bsk_rows());
  const extracted = binfhe_sample_extract(big_n, log_q, acc);
  const at_ks = mod_switch_lwe(big_n, log_q, log_mod_ks, extracted);
  const switched = binfhe_key_switch(n_lwe, big_n, log_mod_ks, ks_ell, ks_base_log, at_ks, bk.ksk_ref());
  return mod_switch_lwe(n_lwe, log_mod_ks, log_q_lwe, switched);
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
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(result.length); __zip_mut_1 += 1n) {
    {
      result[Number(__zip_mut_1)] = reduce(log, result[Number(__zip_mut_1)]);
    }}
  return result;
}

export function binfhe_poly_rotate(n: bigint, log: bigint, p: bigint[], exp: bigint): bigint[]
{
  const exp_1 = (exp % fieldMul(2n, n));
  let result = Array.from({length: Number(n)}, () => 0n);
  for (const [i, coeff] of p.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const dest = fieldAdd(i, exp_1);
    if ((dest < n))     {
      result[Number(dest)] = wrappingAdd(result[Number(dest)], coeff, 32);
    } else if ((dest < fieldMul(2n, n)))     {
      result[Number(fieldSub(dest, n))] = wrappingSub(result[Number(fieldSub(dest, n))], coeff, 32);
    } else     {
      result[Number(fieldSub(dest, fieldMul(2n, n)))] = wrappingAdd(result[Number(fieldSub(dest, fieldMul(2n, n)))], coeff, 32);
    }
  }
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(result.length); __zip_mut_1 += 1n) {
    {
      result[Number(__zip_mut_1)] = reduce(log, result[Number(__zip_mut_1)]);
    }}
  return result;
}

export function binfhe_rgsw_cmux(...__args: any[]): any {
  if (__args.length === 14) {
    const n_lwe = __args[0];
    const big_n = __args[1];
    const log_q = __args[2];
    const log_q_lwe = __args[3];
    const log_mod_ks = __args[4];
    const bs_ell = __args[5];
    const bs_base_log = __args[6];
    const ks_ell = __args[7];
    const ks_base_log = __args[8];
    const k_max = __args[9];
    const sel = __args[10];
    const a = __args[11];
    const b = __args[12];
    const bk = __args[13];
    return (() => {
  const TABLE: boolean[] = [false, false, false, true, true, false, true, true];
  return binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, [sel, a, b], TABLE, k_max, bk);
})();
  } else
  if (__args.length === 7) {
    const n = __args[0];
    const log = __args[1];
    const ell = __args[2];
    const base_log = __args[3];
    const c = __args[4];
    const d1 = __args[5];
    const d0 = __args[6];
    return (() => {
  const diff = binfhe_rlwe_sub(n, log, d1, d0);
  const prod = binfhe_external_product(n, log, ell, base_log, c, diff);
  return binfhe_rlwe_add(n, log, d0, prod);
})();
  }
  throw new Error("binfhe_rgsw_cmux(): no matching variant for " + __args.length + " args");
}

export function binfhe_rgsw_encrypt<R>(n: bigint, log: bigint, ell: bigint, base_log: bigint, eta: bigint, m: boolean, sk: BinfheRlweSecretKeyDyn, rng: any): BinfheRgswCiphertextDyn
{
  const rows = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const g = level_factor(log, base_log, j);
  const contrib = (() => { if (m) {
  return g;
} else {
  return 0n;
} })();
  let rlwe0 = binfhe_rlwe_encrypt_scalar(n, log, eta, 0n, sk, rng);
  rlwe0.$fa[Number(0n)] = torus_add(log, rlwe0.$fa[Number(0n)], contrib);
  const rlwe1 = binfhe_rlwe_encrypt_scalar(n, log, eta, contrib, sk, rng);
  return new BinfheRgswRowDyn({ $frlwe0: rlwe0, $frlwe1: rlwe1, $fn: 0n });
})());
  return new BinfheRgswCiphertextDyn({ $frows: rows, $fn: 0n, $fell: 0n });
}

export function binfhe_rlwe_add(n: bigint, log: bigint, x: BinfheRlweCiphertextDyn, y: BinfheRlweCiphertextDyn): BinfheRlweCiphertextDyn
{
  let out = x;
  for (let i = 0n; i < n; i += 1n)   {
    out.$fa[Number(i)] = torus_add(log, out.$fa[Number(i)], y.$fa[Number(i)]);
    out.$fb[Number(i)] = torus_add(log, out.$fb[Number(i)], y.$fb[Number(i)]);
  }
  return out;
}

export function binfhe_rlwe_encrypt_poly<R>(n: bigint, log: bigint, eta: bigint, msg: bigint[], sk: BinfheRlweSecretKeyDyn, rng: any): BinfheRlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => reduce(log, rng.next_u32()));
  let b = binfhe_poly_mul_neg(n, log, a, sk.$fkey);
  for (let i = 0n; i < n; i += 1n)   {
    b[Number(i)] = reduce(log, wrappingAdd(wrappingAdd(b[Number(i)], sample_error(log, eta, rng), 32), msg[Number(i)], 32));
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
    phase[Number(i)] = torus_sub(log, ct.$fb[Number(i)], product[Number(i)]);
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
    out.$fa[Number(i)] = torus_sub(log, out.$fa[Number(i)], y.$fa[Number(i)]);
    out.$fb[Number(i)] = torus_sub(log, out.$fb[Number(i)], y.$fb[Number(i)]);
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
    a_lwe[Number(i)] = torus_neg(log, ct.$fa[Number(fieldSub(n, i))]);
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

export function bit_reverse(...__args: any[]): any {
  if (__args.length === 1) {
    const values = __args[0];
    return (() => {
  const bits = ilog2(BigInt(values.length));
  for (let index = 0n; index < BigInt(values.length); index += 1n)   {
    const reversed = fieldShr((() => { let __r = index; let __o = 0n; for (let __i = 0n; __i < 64n; __i += 1n) { __o = (__o << 1n) | (__r & 1n); __r >>= 1n; } return __o; })(), fieldSub(64n, bits));
    if ((index < reversed))     {
      (() => { const __a = values; const __i = Number(index); const __j = Number(reversed); const __tmp = __a[__i]; __a[__i] = __a[__j]; __a[__j] = __tmp; return __a; })();
    }
  }
})();
  } else
  if (__args.length === 1) {
    const values = __args[0];
    return (() => {
  const bits = ilog2(BigInt(values.length));
  for (let index = 0n; index < BigInt(values.length); index += 1n)   {
    const reversed = fieldShr((() => { let __r = index; let __o = 0n; for (let __i = 0n; __i < 64n; __i += 1n) { __o = (__o << 1n) | (__r & 1n); __r >>= 1n; } return __o; })(), fieldSub(64n, bits));
    if ((index < reversed))     {
      (() => { const __a = values; const __i = Number(index); const __j = Number(reversed); const __tmp = __a[__i]; __a[__i] = __a[__j]; __a[__j] = __tmp; return __a; })();
    }
  }
})();
  }
  throw new Error("bit_reverse(): no matching variant for " + __args.length + " args");
}

export function bit_to_g128(b: boolean): Galois128
{
  return new Galois128((b as unknown as bigint));
}

export function bits_to_bytes(bits: bigint[], nbytes: bigint): Vec<bigint>
{
  let out = Array.from({length: Number(nbytes)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < nbytes; i += 1n)   {
    let acc = ((0n) & 0xFFn);
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

export function blake2s_256(input: bigint[]): bigint[]
{
  let h = blake2s_init();
  const n = BigInt(input.length);
  const num_blocks = (() => { if (__equals(n, 0n)) {
  return 1n;
} else {
  return ((n) + (BLAKE2S_BLOCK) - 1n) / (BLAKE2S_BLOCK);
} })();
  let t = 0n;
  for (let i = 0n; i < num_blocks; i += 1n)   {
    let block = Array.from({length: Number(BLAKE2S_BLOCK)}, () => ((0n) & 0xFFn));
    const start = fieldMul(i, BLAKE2S_BLOCK);
    const take = BigInt(Math.min(Number(fieldSub(n, start)), Number(BLAKE2S_BLOCK)));
    (block.slice(0, Number(take))).splice(0, (input.slice(Number(start), Number(fieldAdd(start, take)))).length, ...(input.slice(Number(start), Number(fieldAdd(start, take)))));
    t = fieldAdd(t, BigInt(take));
    blake2s_compress(h, block, t, __equals(i, fieldSub(num_blocks, 1n)));
  }
  return blake2s_output(h);
}

export function blake2s_256_array(n: bigint, input: bigint[]): bigint[]
{
  let h = blake2s_init();
  const n_1 = n;
  const num_blocks = (() => { if (__equals(n_1, 0n)) {
  return 1n;
} else {
  return (fieldSub(fieldAdd(n_1, BLAKE2S_BLOCK), 1n) / BLAKE2S_BLOCK);
} })();
  let t = 0n;
  for (let i = 0n; i < num_blocks; i += 1n)   {
    let block = Array.from({length: Number(BLAKE2S_BLOCK)}, () => ((0n) & 0xFFn));
    const start = fieldMul(i, BLAKE2S_BLOCK);
    for (let j = 0n; j < BLAKE2S_BLOCK; j += 1n)     {
      const idx = fieldAdd(start, j);
      if ((idx < n_1))       {
        block[Number(j)] = input[Number(idx)];
      }
    }
    const take = (() => { if ((fieldAdd(start, BLAKE2S_BLOCK) <= n_1)) {
  return BLAKE2S_BLOCK;
} else {
  return fieldSub(n_1, start);
} })();
    t = fieldAdd(t, BigInt(take));
    blake2s_compress(h, block, t, __equals(i, fieldSub(num_blocks, 1n)));
  }
  return blake2s_output(h);
}

export function blake2s_compress(h: bigint[], block: bigint[], t: bigint, last: boolean)
{
  let m = Array.from({length: Number(16n)}, () => 0n);
  for (let i = 0n; i < 16n; i += 1n)   {
    let chunk = Array.from({length: Number(4n)}, () => ((0n) & 0xFFn));
    (chunk).splice(0, (block.slice(Number(fieldMul(i, 4n)), Number(fieldAdd(fieldMul(i, 4n), 4n)))).length, ...(block.slice(Number(fieldMul(i, 4n)), Number(fieldAdd(fieldMul(i, 4n), 4n)))));
    m[Number(i)] = u32_from_le_bytes(chunk);
  }
  let v = Array.from({length: Number(16n)}, () => 0n);
  for (let i = 0n; i < 8n; i += 1n)   {
    v[Number(i)] = h[Number(i)];
  }
  for (let i = 0n; i < 8n; i += 1n)   {
    v[Number(fieldAdd(i, 8n))] = BLAKE2S_IV[Number(i)];
  }
  v[Number(12n)] = fieldBitxor(v[Number(12n)], BigInt(t));
  v[Number(13n)] = fieldBitxor(v[Number(13n)], BigInt(fieldShr(t, 32n)));
  if (last)   {
    v[Number(14n)] = fieldBitxor(v[Number(14n)], 4294967295n);
  }
  for (let round = 0n; round < 10n; round += 1n)   {
    const base = fieldMul(round, 16n);
    blake2s_g(v, 0n, 4n, 8n, 12n, m[Number(BLAKE2S_SIGMA[Number(base)])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 1n))])]);
    blake2s_g(v, 1n, 5n, 9n, 13n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 2n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 3n))])]);
    blake2s_g(v, 2n, 6n, 10n, 14n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 4n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 5n))])]);
    blake2s_g(v, 3n, 7n, 11n, 15n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 6n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 7n))])]);
    blake2s_g(v, 0n, 5n, 10n, 15n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 8n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 9n))])]);
    blake2s_g(v, 1n, 6n, 11n, 12n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 10n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 11n))])]);
    blake2s_g(v, 2n, 7n, 8n, 13n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 12n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 13n))])]);
    blake2s_g(v, 3n, 4n, 9n, 14n, m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 14n))])], m[Number(BLAKE2S_SIGMA[Number(fieldAdd(base, 15n))])]);
  }
  for (let i = 0n; i < 8n; i += 1n)   {
    h[Number(i)] = fieldBitxor(h[Number(i)], fieldBitxor(v[Number(i)], v[Number(fieldAdd(i, 8n))]));
  }
}

export function blake2s_g(v: bigint[], a: bigint, b: bigint, c: bigint, d: bigint, mx: bigint, my: bigint)
{
  v[Number(a)] = wrappingAdd(wrappingAdd(v[Number(a)], v[Number(b)], 32), mx, 32);
  const t0 = fieldBitxor(v[Number(d)], v[Number(a)]);
  v[Number(d)] = fieldBitor(fieldShr(t0, 16n), fieldShl(t0, 16n));
  v[Number(c)] = wrappingAdd(v[Number(c)], v[Number(d)], 32);
  const t1 = fieldBitxor(v[Number(b)], v[Number(c)]);
  v[Number(b)] = fieldBitor(fieldShr(t1, 12n), fieldShl(t1, 20n));
  v[Number(a)] = wrappingAdd(wrappingAdd(v[Number(a)], v[Number(b)], 32), my, 32);
  const t2 = fieldBitxor(v[Number(d)], v[Number(a)]);
  v[Number(d)] = fieldBitor(fieldShr(t2, 8n), fieldShl(t2, 24n));
  v[Number(c)] = wrappingAdd(v[Number(c)], v[Number(d)], 32);
  const t3 = fieldBitxor(v[Number(b)], v[Number(c)]);
  v[Number(b)] = fieldBitor(fieldShr(t3, 7n), fieldShl(t3, 25n));
}

export function blake2s_init(): bigint[]
{
  let h = BLAKE2S_IV;
  h[Number(0n)] = fieldBitxor(h[Number(0n)], 16842784n);
  return h;
}

export function blake2s_output(h: bigint[]): bigint[]
{
  let out = Array.from({length: Number(BLAKE2S_OUT)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < 8n; i += 1n)   {
    (out.slice(Number(fieldMul(i, 4n)), Number(fieldAdd(fieldMul(i, 4n), 4n)))).splice(0, ([(h[Number(i)]) & 0xFFn, ((h[Number(i)]) >> 8n) & 0xFFn, ((h[Number(i)]) >> 16n) & 0xFFn, ((h[Number(i)]) >> 24n) & 0xFFn]).length, ...([(h[Number(i)]) & 0xFFn, ((h[Number(i)]) >> 8n) & 0xFFn, ((h[Number(i)]) >> 16n) & 0xFFn, ((h[Number(i)]) >> 24n) & 0xFFn]));
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
  const log2_two_n = ((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, two_n));
  const scale_shift = (() => { const __a = 32n; const __b = log2_two_n; const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
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
  let buckets = Array.from({length: Number(m)}, () => [] as any[]);
  for (let x = 0n; x < n; x += 1n)   {
    for (let i = 0n; i < TAU; i += 1n)     {
      const j = hash_i(seed, i, x, m);
      (buckets[Number(j)]).push(x);
    }
  }
  for (let __mut_1 = 0n; __mut_1 < BigInt(buckets.length); __mut_1 += 1n) {
  {
    buckets[Number(__mut_1)].sort((__a: any, __b: any) => (__a < __b ? -1 : (__a > __b ? 1 : 0)));
    buckets[Number(__mut_1)].dedup();
  }}
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

export function bytes_to_lanes(b: bigint[], st: bigint[])
{
  for (let i = 0n; i < 25n; i += 1n)   {
    let chunk = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
    (chunk).splice(0, (b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).length, ...(b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))));
    st[Number(i)] = u64_from_le_bytes(chunk);
  }
}

export function bytes_to_limbs(b: bigint[]): bigint[]
{
  let out = Array.from({length: Number(4n)}, () => 0n);
  for (let i = 0n; i < 4n; i += 1n)   {
    let le = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
    (le).splice(0, (b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).length, ...(b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))));
    out[Number(i)] = u64_from_le_bytes(le);
  }
  return out;
}

export function cbd<R>(eta: bigint, rng: any): bigint
{
  if (__equals(eta, 0n))   {
    return 0n;
  }
  const mask = (() => { if ((eta >= 32n)) {
  return 4294967295n;
} else {
  return fieldSub(fieldShl(1n, eta), 1n);
} })();
  const a = (/* count_ones */ ((() => { let _n = fieldBitand(rng.next_u32(), mask), _c = 0; while (_n) { _c += _n & 1; _n >>>= 1; } return _c; })()) as unknown as bigint);
  const b = (/* count_ones */ ((() => { let _n = fieldBitand(rng.next_u32(), mask), _c = 0; while (_n) { _c += _n & 1; _n >>>= 1; } return _c; })()) as unknown as bigint);
  return fieldSub(a, b);
}

export function center(value: bigint, modulus: bigint): bigint
{
  const value_1 = (value % modulus);
  return (() => { if ((value_1 > (modulus / 2n))) {
  return fieldSub((value_1 as unknown as bigint), (modulus as unknown as bigint));
} else {
  return (value_1 as unknown as bigint);
} })();
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
  const diff = BigInt((((fieldSub((c[Number(0n)] as unknown as bigint), (c[Number(1n)] as unknown as bigint))) % ((q as unknown as bigint)) + ((q as unknown as bigint))) % ((q as unknown as bigint))));
  return (fieldMul(2n, diff) % q);
})(), $finterval_true: [5n, 1n] });
}

export function cert_xor(): GateCertificate
{
  return new GateCertificate({ $fname: "XOR", $farity: 2n, $fprepare: (c, q) => (() => {
  const diff = BigInt((((fieldSub((c[Number(0n)] as unknown as bigint), (c[Number(1n)] as unknown as bigint))) % ((q as unknown as bigint)) + ((q as unknown as bigint))) % ((q as unknown as bigint))));
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
  t.absorb(iv);
  t.absorb(com_bytes);
  return t.squeeze(ctx, lambda_plus_b);
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
  return t.squeeze(ctx, lambda_plus_b);
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
  return t.squeeze(ctx, lambda);
}

export function check_lut_shape(addr_bits: bigint, table_len: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, k_max: bigint): Result<bigint, LutError>
{
  if ((__equals(addr_bits, 0n) || (addr_bits >= BigInt(64n))))   {
    return new LutError_AddressShapeInvalid();
  }
  if (!__equals(table_len, fieldShl(1n, addr_bits)))   {
    return new LutError_AddressShapeInvalid();
  }
  if ((addr_bits > k_max))   {
    return new LutError_ArityExceedsCircuitMax();
  }
  if (((((((fieldAdd(BigInt(k_max), 2n) > log_q_lwe) || !((big_n) > 0n && ((big_n) & ((big_n) - 1n)) === 0n)) || (fieldShl(1n, k_max) > big_n)) || !__equals(fieldShl(1n, log_q_lwe), fieldMul(2n, big_n))) || (log_q_lwe > log_q)) || (log_q > 32n)))   {
    return new LutError_ShapeUnsupported();
  }
  return fieldShr(big_n, k_max);
}

export function check_profile(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_base_log: bigint, bs_ell: bigint, ks_base_log: bigint, ks_ell: bigint, priv_base_log: bigint, priv_ell: bigint)
{
}

export function checked_seed_cot_count(params: any): Result<bigint, PoolSeedError>
{
  return checked_seed_cot_count_mode(params, false);
}

export function checked_seed_cot_count_malicious(params: any): Result<bigint, PoolSeedError>
{
  return checked_seed_cot_count_mode(params, true);
}

export function checked_seed_cot_count_mode(params: any, malicious: boolean): Result<bigint, PoolSeedError>
{
  if (((__equals(params.$ft, 0n) || (params.$fn < params.$ft)) || (params.$fk < LOCALITY)))   {
    return new PoolSeedError_InvalidParameters();
  }
  const max_interval_len = ((params.$fn) + (params.$ft) - 1n) / (params.$ft);
  const __volar_try_2 = ((((__value: bigint) => { let __power = 1n; while (__power < __value) __power <<= 1n; return __power <= 18446744073709551615n ? __power : undefined; }) (max_interval_len)) ?? (new PoolSeedError_InvalidParameters()));
  if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
  const splen = __volar_try_2;
  const __volar_try_3 = ((((__option: any, __continuation: (value: any) => any) => __option == null ? undefined : __continuation(__option))((() => { const __a = params.$ft; const __b = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen))); const __result = __a * __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })(), (extra) => (() => { const __a = params.$fk; const __b = extra; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })())) ?? (new PoolSeedError_InvalidParameters()));
  if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
  const body = __volar_try_3;
  const seed_count = (() => { if (malicious) {
  return (((() => { const __a = body; const __b = KAPPA_BITS; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new PoolSeedError_InvalidParameters()));
} else {
  return body;
} })();
  if ((seed_count >= params.$fn))   {
    return new PoolSeedError_InvalidParameters();
  }
  return seed_count;
}

export function circuit_bootstrap(n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, priv_ell: bigint, priv_base_log: bigint, ct: BinfheLweCiphertextDyn, cbk: CircuitBootstrappingKeyDyn, k_max: bigint): BinfheRgswCiphertextDyn
{
  const delta = wire_delta(log_q_lwe, BigInt(k_max));
  const centered = binfhe_lwe_add_const(n_lwe, log_q_lwe, ct, (delta / 2n));
  const rows = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const test_poly = level_test_poly(big_n, log_q, j, bs_base_log, k_max);
  const acc = binfhe_blind_rotate(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, centered, test_poly, cbk.$fbk.$fbsk);
  const extracted = binfhe_sample_extract(big_n, log_q, acc);
  const rlwe0 = priv_ks(big_n, log_q, priv_ell, priv_base_log, extracted, cbk.$fprivksk.$fa_col, cbk.$fprivksk.$fa_body);
  const rlwe1 = priv_ks(big_n, log_q, priv_ell, priv_base_log, extracted, cbk.$fprivksk.$fb_col, cbk.$fprivksk.$fb_body);
  return new BinfheRgswRowDyn({ $frlwe0: rlwe0, $frlwe1: rlwe1, $fn: 0n });
})());
  return new BinfheRgswCiphertextDyn({ $frows: rows, $fn: 0n, $fell: 0n });
}

export function cmac(mac_keys: bigint[][], msg: bigint[]): bigint[]
{
  const l = encrypt_block_with_keys(mac_keys, Array.from({length: Number(BLOCK_LEN)}, () => ((0n) & 0xFFn)));
  const k1 = dbl(l);
  const k2 = dbl(k1);
  const complete = (!(msg.length === 0) && __equals((BigInt(msg.length) % BLOCK_LEN), 0n));
  const n_blocks = (() => { if (complete) {
  return (BigInt(msg.length) / BLOCK_LEN);
} else {
  return fieldAdd((BigInt(msg.length) / BLOCK_LEN), 1n);
} })();
  let state = Array.from({length: Number(BLOCK_LEN)}, () => ((0n) & 0xFFn));
  for (let block_idx = 0n; block_idx < n_blocks; block_idx += 1n)   {
    const is_last = __equals(fieldAdd(block_idx, 1n), n_blocks);
    let block = Array.from({length: Number(BLOCK_LEN)}, () => ((0n) & 0xFFn));
    if (is_last)     {
      if (complete)       {
        (block).splice(0, (msg.slice(Number(fieldMul(block_idx, BLOCK_LEN)), Number(fieldMul(fieldAdd(block_idx, 1n), BLOCK_LEN)))).length, ...(msg.slice(Number(fieldMul(block_idx, BLOCK_LEN)), Number(fieldMul(fieldAdd(block_idx, 1n), BLOCK_LEN)))));
        for (let i = 0n; i < BLOCK_LEN; i += 1n)         {
          block[Number(i)] = fieldBitxor(block[Number(i)], k1[Number(i)]);
        }
      } else       {
        const rest = fieldSub(BigInt(msg.length), fieldMul(block_idx, BLOCK_LEN));
        (block.slice(0, Number(rest))).splice(0, (msg.slice(Number(fieldMul(block_idx, BLOCK_LEN)))).length, ...(msg.slice(Number(fieldMul(block_idx, BLOCK_LEN)))));
        block[Number(rest)] = 128n;
        for (let i = 0n; i < BLOCK_LEN; i += 1n)         {
          block[Number(i)] = fieldBitxor(block[Number(i)], k2[Number(i)]);
        }
      }
    } else     {
      (block).splice(0, (msg.slice(Number(fieldMul(block_idx, BLOCK_LEN)), Number(fieldMul(fieldAdd(block_idx, 1n), BLOCK_LEN)))).length, ...(msg.slice(Number(fieldMul(block_idx, BLOCK_LEN)), Number(fieldMul(fieldAdd(block_idx, 1n), BLOCK_LEN)))));
    }
    for (let i = 0n; i < BLOCK_LEN; i += 1n)     {
      state[Number(i)] = fieldBitxor(state[Number(i)], block[Number(i)]);
    }
    state = encrypt_block_with_keys(mac_keys, state);
  }
  return state;
}

export function cmp4(a: bigint[], b: bigint[]): Ordering
{
  for (const i of (Array.from({length: Number(4n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    if (!__equals(a[Number(i)], b[Number(i)]))     {
      return a[Number(i)].cmp(b[Number(i)]);
    }
  }
  return Equal;
}

export function cmux(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, c: RgswCiphertextDyn, d1: RlweCiphertextDyn, d0: RlweCiphertextDyn): RlweCiphertextDyn
{
  const diff = rlwe_sub(big_n, d1, d0);
  const prod = external_product(big_n, bs_ell, bs_bg_log, c, diff);
  return rlwe_add(big_n, d0, prod);
}

export function collect_open_nodes_walk(node: bigint, hidden: boolean[], tree: bigint[][], leaf_count: bigint, out: Vec<[bigint, bigint[]]>)
{
  if (!hidden[Number(node)])   {
    (out).push([node, tree[Number(node)]]);
    return;
  }
  if ((node >= fieldSub(leaf_count, 1n)))   {
    return;
  }
  collect_open_nodes_walk(fieldAdd(fieldMul(2n, node), 1n), hidden, tree, leaf_count, out);
  collect_open_nodes_walk(fieldAdd(fieldMul(2n, node), 2n), hidden, tree, leaf_count, out);
}

export function column_rows(seed: bigint[], k: bigint, j: bigint): bigint[]
{
  return column_rows_for_valid_k(seed, k, j);
}

export function column_rows_for_valid_k(seed: bigint[], k: bigint, j: bigint): bigint[]
{
  let rows = Array.from({length: Number(LOCALITY)}, () => 0n);
  let fill = 0n;
  let counter = 0n;
  while ((fill < LOCALITY))   {
    let h = new Sha3_256();
    h.update([102n, 101n, 114n, 114n, 101n, 116n, 45n, 108n, 112n, 110n, 45n, 49n, 48n, 108n, 111n, 99n, 97n, 108n, 45n, 118n, 49n]);
    h.update(seed);
    h.update([(BigInt(j)) & 0xFFn, ((BigInt(j)) >> 8n) & 0xFFn, ((BigInt(j)) >> 16n) & 0xFFn, ((BigInt(j)) >> 24n) & 0xFFn]);
    h.update([(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn]);
    const out = [...Array.from(h.finalize(), BigInt)];
    for (const chunk of __chunks(out, Number(4n)).filter((__chunk: any) => __chunk.length === Number(4n)))     {
      if ((fill >= LOCALITY))       {
        break;
      }
      const raw = BigInt(u32_from_le_bytes((chunk)!));
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
  return new CommitmentCoreDyn([...Array.from(hasher.finalize(), BigInt)]);
}

export function concat_small_voles(outs: Vec<ConvertOutput>): BigVoleProver
{
  const l_hat = BigInt(outs[Number(0n)].$fu.length);
  for (let oi = 0n; oi < BigInt(outs.length); oi += 1n)   {
    const o = outs[Number(oi)];
    for (let vi = 0n; vi < BigInt(o.$fv.length); vi += 1n)     {
    }
  }
  const u = __clone(outs[Number(0n)].$fu);
  let c: Vec<Vec<bigint>> = ([] as any[]);
  for (let oi = 1n; oi < BigInt(outs.length); oi += 1n)   {
    let ci = __clone(outs[Number(oi)].$fu);
    xor_in_place(ci, u);
    (c).push(ci);
  }
  let v_columns: Vec<Vec<bigint>> = [] as any[];
  for (let oi = 0n; oi < BigInt(outs.length); oi += 1n)   {
    for (let vi = 0n; vi < BigInt(outs[Number(oi)].$fv.length); vi += 1n)     {
      (v_columns).push(__clone(outs[Number(oi)].$fv[Number(vi)]));
    }
  }
  return new BigVoleProver({ $fu: u, $fc: c, $fv_columns: v_columns });
}

export function concat_small_voles_verifier(outs: Vec<ConvertOutput>, deltas: bigint[], corrections: Vec<bigint>[]): BigVoleVerifier
{
  let q_columns: Vec<Vec<bigint>> = [] as any[];
  for (let i = 0n; i < BigInt(outs.length); i += 1n)   {
    const delta_i = deltas[Number(i)];
    for (let bit = 0n; bit < BigInt(outs[Number(i)].$fv.length); bit += 1n)     {
      let q = __clone(outs[Number(i)].$fv[Number(bit)]);
      if ((i >= 1n))       {
        const delta_bit = __equals(fieldBitand(fieldShr(delta_i, bit), 1n), 1n);
        if (delta_bit)         {
          xor_in_place(q, corrections[Number(fieldSub(i, 1n))]);
        }
      }
      (q_columns).push(q);
    }
  }
  return new BigVoleVerifier({ $fq_columns: q_columns });
}

export function context(params: any): (Context | undefined)
{
  return Context.new(params.$fdegree, params.$fmodulus);
}

export function convert_to_vole(seeds: (bigint[] | undefined)[], iv: bigint[], tweak: bigint, l_hat_bytes: bigint): ConvertOutput
{
  const n = BigInt(seeds.length);
  const d = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, n)));
  const zero_block = Array.from({length: Number(l_hat_bytes)}, () => ((0n) & 0xFFn));
  let r: Vec<Vec<bigint>> = ([] as any[]);
  for (let i = 0n; i < BigInt(seeds.length); i += 1n)   {
    (() => { const __match = seeds[Number(i)]; if (__match !== null && __match !== undefined) { const seed = __match;
return (r).push(aes_ctr_prg(seed, iv, tweak, l_hat_bytes)); } else { return (r).push(__clone(zero_block)); } })();
  }
  let v: Vec<Vec<bigint>> = ([] as any[]);
  for (let _j = 0n; _j < d; _j += 1n)   {
    (v).push(Array.from({length: Number(l_hat_bytes)}, () => ((0n) & 0xFFn)));
  }
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
  const u = __clone(level[Number(0n)]);
  return new ConvertOutput({ $fu: u, $fv: v });
}

export function create_vole_from_material(ctx: { B_OutputSize: bigint }, s: any[]): VopeDyn<bigint>
{
  const u: bigint[] = s.reduce((a: any, b: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a[Number(i)], asRefU8(b)[Number(i)]));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  const v: bigint[] = s.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => fieldBitxor(fieldBitxor(a[Number(j)], asRefU8(b)[Number(j)]), ((i) & 0xFFn)));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  return new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u)), $fv: v, $fn: 0n, $fk: 1n });
}

export function create_vole_from_material_expanded(ctx: { B_OutputSize: bigint }, s: any[], f: (arg: Uint8Array) => any): VopeDyn<bigint>
{
  const u: bigint[] = s.map((b: any) => f(asRefU8(b).slice(0, Number(ctx.B_OutputSize)))).reduce((a: any, b: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(a[Number(i)], asRefU8(b)[Number(i)]));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  const v: bigint[] = s.map((b: any) => f(asRefU8(b).slice(0, Number(ctx.B_OutputSize)))).map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).reduce((a: any, [i, b]: any) => (() => {
  return Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => fieldBitxor(fieldBitxor(a[Number(j)], asRefU8(b)[Number(j)]), ((i) & 0xFFn)));
})(), Array.from({length: Number(ctx.B_OutputSize - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  return new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u)), $fv: v, $fn: 0n, $fk: 1n });
}

export function crhf(input: any, tweak: bigint): Block
{
  let h = new Sha3_256();
  h.update([102n, 101n, 114n, 114n, 101n, 116n, 45n, 115n, 112n, 99n, 111n, 116n, 45n, 104n, 45n, 118n, 49n]);
  h.update(input);
  h.update([(tweak) & 0xFFn, ((tweak) >> 8n) & 0xFFn, ((tweak) >> 16n) & 0xFFn, ((tweak) >> 24n) & 0xFFn]);
  const out = [...Array.from(h.finalize(), BigInt)];
  let b = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
  (b).splice(0, (out.slice(0, Number(KAPPA_BYTES))).length, ...(out.slice(0, Number(KAPPA_BYTES))));
  return b;
}

export function cross_term<S>(ctx: { defaultS: () => any, UClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, w1: S[], u1: any, w2: S[], u2: any): S[]
{
  const z1 = full_z(w1, u1);
  const z2 = full_z(w2, u2);
  const [az1, bz1, cz1] = eval_abc(ctx, z1);
  const [az2, bz2, cz2] = eval_abc(ctx, z2);
  let t = [undefined as any /* S::default() */, undefined as any /* S::default() */, undefined as any /* S::default() */];
  for (let i = 0n; i < AND_CONS; i += 1n)   {
    const cross = fieldAdd(fieldMul(__clone(az1[Number(i)]), __clone(bz2[Number(i)])), fieldMul(__clone(az2[Number(i)]), __clone(bz1[Number(i)])));
    const sub = fieldAdd(fieldMul(__clone(u1), __clone(cz2[Number(i)])), fieldMul(__clone(u2), __clone(cz1[Number(i)])));
    t[Number(i)] = fieldSub(cross, sub);
  }
  return t;
}

export function ctr_block(enc_keys: bigint[][], nonce: bigint[], i: bigint): bigint[]
{
  const counter = wrappingAdd(u128.from_be_bytes(nonce), (i as unknown as bigint), 128);
  return encrypt_block_with_keys(enc_keys, [((counter) >> 24n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 8n) & 0xFFn, (counter) & 0xFFn]);
}

export function cuckoo_insert(seed: bigint[], n: bigint, t: bigint, points: bigint[]): Vec<(bigint | undefined)>
{
  const m = cuckoo_table_size(t);
  let table = Array.from({length: Number(m)}, () => undefined);
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
  return BigInt(Math.max(Number((((() => { const __a = t; const __b = 3n; const __result = __a * __b; return __result < 0n ? 0n : (__result > 18446744073709551615n ? 18446744073709551615n : __result); })()) + (2n) - 1n) / (2n)), Number(fieldAdd(t, 1n))));
}

export function dbl(block: bigint[]): bigint[]
{
  let out = Array.from({length: Number(BLOCK_LEN)}, () => ((0n) & 0xFFn));
  let carry = ((0n) & 0xFFn);
  for (const i of (Array.from({length: Number(BLOCK_LEN - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    const next_carry = fieldShr(block[Number(i)], 7n);
    out[Number(i)] = fieldBitor(fieldShl(block[Number(i)], 1n), carry);
    carry = next_carry;
  }
  if (__equals(carry, 1n))   {
    out[Number(fieldSub(BLOCK_LEN, 1n))] = fieldBitxor(out[Number(fieldSub(BLOCK_LEN, 1n))], 135n);
  }
  return out;
}

export function debug_check_pool_written(written: boolean, slot: bigint)
{
  if (!written)   {
  }
}

export function decode_bools(bytes: bigint[]): [Vec<boolean>, bigint]
{
  let off = 0n;
  const n = BigInt(take_u32(bytes, off));
  const bits = bytes.slice(Number(off), Number(fieldAdd(off, n))).map((b: any) => !__equals(b, 0n));
  off = fieldAdd(off, n);
  return [bits, off];
}

export function decode_ferret_open(bytes: bigint[]): [bigint[], Vec<boolean>]
{
  let seed = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  (seed).splice(0, (bytes.slice(0, Number(16n))).length, ...(bytes.slice(0, Number(16n))));
  const [choices, _] = decode_bools(bytes.slice(Number(16n)));
  return [seed, choices];
}

export function decode_iknp_corr(bytes: bigint[]): Vec<bigint[]>
{
  let off = 0n;
  const n = BigInt(take_u32(bytes, off));
  let rows = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    let r = Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn));
    (r).splice(0, (bytes.slice(Number(off), Number(fieldAdd(off, IKNP_KAPPA_BYTES)))).length, ...(bytes.slice(Number(off), Number(fieldAdd(off, IKNP_KAPPA_BYTES)))));
    off = fieldAdd(off, IKNP_KAPPA_BYTES);
    (rows).push(r);
  }
  return rows;
}

export function decode_iknp_u(bytes: bigint[]): IknpUMsg
{
  let off = 0n;
  const n = BigInt(take_u32(bytes, off));
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
  let label = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  let offset = 0n;
  for (const [index, [element, width]] of elements.map((__a: any, __i: number) => [__a, WIDTHS[__i]] as [typeof __a, any]).map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    if ((element >= modulus))     {
      return new LabelEncodingError_FieldElementOutOfRange(index);
    }
    if ((element >= fieldShl(1n, width)))     {
      return new LabelEncodingError_NonCanonicalElement(index);
    }
    const bytes = [(element) & 0xFFn, ((element) >> 8n) & 0xFFn, ((element) >> 16n) & 0xFFn, ((element) >> 24n) & 0xFFn];
    const count = BigInt((width / 8n));
    (label.slice(Number(offset), Number(fieldAdd(offset, count)))).splice(0, (bytes.slice(0, Number(count))).length, ...(bytes.slice(0, Number(count))));
    offset = fieldAdd(offset, count);
  }
  return label;
}

export function decode_lwe_crs(n: bigint, bytes: bigint[]): LweOtCrsDyn
{
  let off = 0n;
  const n_1 = BigInt(take_u32(bytes, off));
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
  const n = BigInt(take_u32(bytes, off));
  let blocks = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    const len = BigInt(take_u32(bytes, off));
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
  const __volar_try_3 = parse_profile(reader.byte());
  if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
  const profile = __volar_try_3;
  const __volar_try_4 = reader.u32();
  if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
  const k_max = __volar_try_4;
  const __volar_try_5 = reader.u32();
  if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
  const num_inputs = __volar_try_5;
  const __volar_try_6 = reader.u32();
  if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
  const num_cells = __volar_try_6;
  const budget = new FailureBudget({ $fper_bootstrap_log2: reader.u32(), $ftotal_log2: reader.u32() });
  const __volar_try_8 = reader.count();
  if (__volar_try_8 instanceof __VolarError) return __volar_try_8;
  const lut_count = __volar_try_8;
  let luts = ([] as any[]);
  for (let _ = 0n; _ < lut_count; _ += 1n)   {
    const __volar_try_0 = reader.count();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const bit_count = __volar_try_0;
    const packed_len = ((bit_count) + (8n) - 1n) / (8n);
    const __volar_try_2 = reader.take(packed_len);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const packed = __volar_try_2;
    if ((!__equals((bit_count % 8n), 0n) && ((__option: any, __predicate: (value: any) => boolean) => __option != null && __predicate(__option))(packed.at(-1), (byte) => !__equals(fieldShr(byte, (bit_count % 8n)), 0n))))     {
      return new DecodeError_UnknownTag();
    }
    let entries = ([] as any[]);
    for (let bit = 0n; bit < bit_count; bit += 1n)     {
      (entries).push(!__equals(fieldBitand(fieldShr(packed[Number((bit / 8n))], (bit % 8n)), 1n), 0n));
    }
    (luts).push(new LutSpec({ $fentries: entries }));
  }
  const __volar_try_11 = reader.count();
  if (__volar_try_11 instanceof __VolarError) return __volar_try_11;
  const layer_count = __volar_try_11;
  let layers = ([] as any[]);
  for (let _ = 0n; _ < layer_count; _ += 1n)   {
    const __volar_try_0 = reader.count();
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const op_count = __volar_try_0;
    let layer = ([] as any[]);
    for (let _ = 0n; _ < op_count; _ += 1n)     {
      (layer).push(read_op(reader));
    }
    (layers).push(layer);
  }
  const __volar_try_14 = reader.ids();
  if (__volar_try_14 instanceof __VolarError) return __volar_try_14;
  const outputs = __volar_try_14;
  const __volar_try_15 = reader.ids();
  if (__volar_try_15 instanceof __VolarError) return __volar_try_15;
  const cell_outputs = __volar_try_15;
  if (!__equals(reader.$foffset, BigInt(bytes.length)))   {
    return new DecodeError_TrailingBytes();
  }
  const plan = new BootstrapPlan({ $fprofile: profile, $fk_max: k_max, $fluts: luts, $flayers: layers, $fnum_inputs: num_inputs, $fnum_cells: num_cells, $foutputs: outputs, $fcell_outputs: cell_outputs, $fbudget: budget });
  const __volar_try_18 = plan.validate();
  if (__volar_try_18 instanceof __VolarError) return __volar_try_18;
  return plan;
}

export function decode_spcot(bytes: bigint[]): [SpcotSenderMsg, bigint]
{
  let off = 0n;
  const h = BigInt(take_u32(bytes, off));
  let ms = ([] as any[]);
  for (let _ = 0n; _ < h; _ += 1n)   {
    const a = take_block(bytes, off);
    const b = take_block(bytes, off);
    (ms).push([a, b]);
  }
  const c = take_block(bytes, off);
  const hv_len = BigInt(take_u32(bytes, off));
  const hash_v = [...bytes.slice(Number(off), Number(fieldAdd(off, hv_len)))];
  off = fieldAdd(off, hv_len);
  return [new SpcotSenderMsg({ $fms: ms, $fc: c, $fhash_v: hash_v }), off];
}

export function decode_zq_vec(bytes: bigint[], off: bigint): Vec<Zq>
{
  const n = BigInt(take_u32(bytes, off));
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
  let out = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
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

export function delta_scaled(delta: any, k: bigint, p: bigint): LabelPoly
{
  return delta.$fcoeffs.map((c: any) => mul_mod(c, k, p));
}

export function derive_and_q<T>(n: bigint, delta: DeltaLoweredDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, hat: any[]): QDyn<any>
{
  return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const lhs = fieldAdd(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), __clone(hat[Number(i)]));
  return fieldMul(lhs, delta.$fdelta[Number(i)].invert());
})()), $fn: 0n });
}

export function derive_roles(key: bigint[]): RoleKeys
{
  const zero = Array.from({length: Number(BLOCK_LEN)}, () => ((0n) & 0xFFn));
  let one = Array.from({length: Number(BLOCK_LEN)}, () => ((0n) & 0xFFn));
  one[Number(fieldSub(BLOCK_LEN, 1n))] = 1n;
  const enc_key = encrypt_block_with_keys(key_expansion(key), zero);
  const mac_key = encrypt_block_with_keys(key_expansion(key), one);
  return new RoleKeys({ $fenc: key_expansion(enc_key), $fmac: key_expansion(mac_key) });
}

export function determination_bit(w0_lsb: boolean, pi: boolean): boolean
{
  return fieldBitxor(w0_lsb, pi);
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
  let acc = new EdPoint({ $fx: new Fe25519([0n, 0n, 0n, 0n]), $fy: new Fe25519([1n, 0n, 0n, 0n]), $fz: new Fe25519([1n, 0n, 0n, 0n]), $ft: new Fe25519([0n, 0n, 0n, 0n]) });
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

export function embed_up(from_: bigint, to: bigint, x: bigint): bigint
{
  return reduce(to, fieldShl(x, fieldSub(to, from_)));
}

export function encode_bits(seed: bigint[], k: bigint, n: bigint, u: boolean[]): Vec<boolean>
{
  let x = Array.from({length: Number(n)}, () => false);
  for (let j = 0n; j < n; j += 1n)   {
    const rows = column_rows_for_valid_k(seed, k, j);
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
  let y = Array.from({length: Number(n)}, () => Array.from({length: Number(16n)}, () => ((0n) & 0xFFn)));
  for (let j = 0n; j < n; j += 1n)   {
    const rows = column_rows_for_valid_k(seed, k, j);
    let acc = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
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
  push_u32(buf, BigInt(BigInt(bits.length)));
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

export function encode_ferret_outputs(seed: bigint[], k: bigint, n: bigint, v: Block[], u: boolean[], w: Block[], s: Block[], e: boolean[], r: Block[]): [Vec<Block>, Vec<boolean>, Vec<Block>]
{
  let y = ([] as any[]);
  let x = ([] as any[]);
  let z = ([] as any[]);
  for (let j = 0n; j < n; j += 1n)   {
    const rows = column_rows_for_valid_k(seed, k, j);
    let y_col = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    let x_bit = false;
    let z_col = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    for (const row of rows)     {
      x_bit = fieldBitxor(x_bit, u[Number(row)]);
      for (let b = 0n; b < 16n; b += 1n)       {
        y_col[Number(b)] = fieldBitxor(y_col[Number(b)], v[Number(row)][Number(b)]);
        z_col[Number(b)] = fieldBitxor(z_col[Number(b)], w[Number(row)][Number(b)]);
      }
    }
    for (let b = 0n; b < 16n; b += 1n)     {
      y_col[Number(b)] = fieldBitxor(y_col[Number(b)], s[Number(j)][Number(b)]);
      z_col[Number(b)] = fieldBitxor(z_col[Number(b)], r[Number(j)][Number(b)]);
    }
    (y).push(y_col);
    (x).push(fieldBitxor(x_bit, e[Number(j)]));
    (z).push(z_col);
  }
  return [y, x, z];
}

export function encode_iknp_corr(rows: bigint[][]): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, BigInt(BigInt(rows.length)));
  for (const r of rows)   {
    buf.push(...(r));
  }
  return buf;
}

export function encode_iknp_u(msg: any): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, BigInt(BigInt(msg.$fu_cols.length)));
  for (const col of msg.$fu_cols)   {
    buf.push(...(encode_bools(col)));
  }
  return buf;
}

export function encode_inputs(garbling: any, delta: any, inputs: boolean[], input_count: bigint): Result<Vec<[LabelPoly, boolean]>, DriverError>
{
  if (!__equals(BigInt(inputs.length), input_count))   {
    return new DriverError_BadInputCount();
  }
  let out = ([] as any[]);
  for (const [i, x] of inputs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const masked = fieldBitxor(x, garbling.$flabels[Number(i)].$fpi);
    (out).push([garbling.$flabels[Number(i)].active(masked, delta), masked]);
  }
  return out;
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
  push_u32(buf, BigInt(n));
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
  push_u32(buf, BigInt(BigInt(msg.$fblocks.length)));
  for (const b of msg.$fblocks)   {
    const inner = encode_spcot(b);
    push_u32(buf, BigInt(BigInt(inner.length)));
    buf.push(...(inner));
  }
  return buf;
}

export function encode_plan(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, plan: any): Result<Vec<bigint>, EncodeError>
{
  const __volar_try_0 = plan.validate();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  if (((((((BigInt(plan.$fluts.length) > MAX_ITEMS) || (BigInt(plan.$flayers.length) > MAX_ITEMS)) || (BigInt(plan.$foutputs.length) > MAX_ITEMS)) || (BigInt(plan.$fcell_outputs.length) > MAX_ITEMS)) || plan.$flayers.some((layer) => (BigInt(layer.length) > MAX_ITEMS))) || plan.$fluts.some((lut) => (BigInt(lut.$fentries.length) > MAX_ITEMS))))   {
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
  put_u32(bytes, BigInt(BigInt(plan.$fluts.length)));
  for (const lut of plan.$fluts)   {
    put_u32(bytes, BigInt(BigInt(lut.$fentries.length)));
    for (const chunk of __chunks(lut.$fentries, Number(8n)))     {
      let packed = ((0n) & 0xFFn);
      for (const [bit, entry] of chunk.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))       {
        packed = fieldBitor(packed, fieldShl(((entry) & 0xFFn), bit));
      }
      (bytes).push(packed);
    }
  }
  put_u32(bytes, BigInt(BigInt(plan.$flayers.length)));
  for (const layer of plan.$flayers)   {
    put_u32(bytes, BigInt(BigInt(layer.length)));
    for (const op of layer)     {
      (() => { const __match = op; if (__match instanceof PlanOp_Const) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
  (bytes).push(0n);
  put_u32(bytes, out);
  (bytes).push(((value) & 0xFFn));
})(); } else if (__match instanceof PlanOp_Not) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  (bytes).push(1n);
  put_u32(bytes, input);
  put_u32(bytes, out);
})(); } else if (__match instanceof PlanOp_Lut) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  (bytes).push(2n);
  put_u32(bytes, BigInt(BigInt(inputs.length)));
  for (const input of inputs)   {
    put_u32(bytes, input);
  }
  put_u32(bytes, table);
  put_u32(bytes, out);
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { const input = __match.$finput;
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

export function encode_receiver_only(receiver: any, prep: any, r: bigint[][], m: bigint): RecvLpn
{
  const k = receiver.$fparams.$fk;
  const n = receiver.$fparams.$fn;
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

export function encode_sender_only(sender: any, lpn_seed: bigint[], s: bigint[][], m: bigint): SenderLpn
{
  const k = sender.$fparams.$fk;
  const n = sender.$fparams.$fn;
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
  push_u32(buf, BigInt(BigInt(msg.$fms.length)));
  for (const pair of msg.$fms)   {
    push_block(buf, pair[Number(0n)]);
    push_block(buf, pair[Number(1n)]);
  }
  push_block(buf, msg.$fc);
  push_u32(buf, BigInt(BigInt(msg.$fhash_v.length)));
  buf.push(...(msg.$fhash_v));
  return buf;
}

export function encode_zq_vec(v: Zq[]): Vec<bigint>
{
  let buf = [] as any[];
  push_u32(buf, BigInt(BigInt(v.length)));
  for (const x of v)   {
    buf.push(...([(x) & 0xFFn, ((x) >> 8n) & 0xFFn, ((x) >> 16n) & 0xFFn, ((x) >> 24n) & 0xFFn]));
  }
  return buf;
}

export function encrypt_block(key: bigint[], plain: bigint[]): bigint[]
{
  const round_keys = key_expansion(key);
  return encrypt_block_with_keys(round_keys, plain);
}

export function encrypt_block_with_keys(round_keys: bigint[][], plain: bigint[]): bigint[]
{
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
  let v = Array.from({length: Number(BigInt(msg_bits.length))}, () => 0n);
  for (const [k, bit] of msg_bits.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const plain = (() => { if (__equals(fieldBitand(bit, 1n), 1n)) {
  return half_q;
} else {
  return 0n;
} })();
    v[Number(k)] = zq_add(zq_add(base, sample_noise(rng)), plain);
  }
  return [u, v];
}

export function encrypt_branch_secure<R>(n: bigint, l: bigint, rng: any, crs: LweOtCrsDyn, pk: Zq[], msg: bigint[]): [Zq[][], Zq[]]
{
  const half_q = (LWE_Q / 2n);
  let u = Array.from({length: Number(l)}, () => Array.from({length: Number(n)}, () => 0n));
  let v = Array.from({length: Number(l)}, () => 0n);
  for (let k = 0n; k < l; k += 1n)   {
    let r = Array.from({length: Number(n)}, () => 0n);
    for (let i = 0n; i < n; i += 1n)     {
      r[Number(i)] = sample_noise(rng);
    }
    for (let j = 0n; j < n; j += 1n)     {
      let acc: Zq = 0n;
      for (let i = 0n; i < n; i += 1n)       {
        acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], r[Number(i)]));
      }
      acc = zq_add(acc, sample_noise(rng));
      u[Number(k)][Number(j)] = acc;
    }
    let base: Zq = 0n;
    for (let i = 0n; i < n; i += 1n)     {
      base = zq_add(base, zq_mul(pk[Number(i)], r[Number(i)]));
    }
    const plain = (() => { if (__equals(fieldBitand(msg[Number(k)], 1n), 1n)) {
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
  const g = level_factor(log_q, base_log, level);
  let scaled = Array.from({length: Number(big_n)}, () => 0n);
  for (let i = 0n; i < big_n; i += 1n)   {
    scaled[Number(i)] = mul_exact(log_q, msg[Number(i)], g);
  }
  return binfhe_rlwe_encrypt_poly(big_n, log_q, eta, scaled, sk, rng);
}

export function ensure<R>(rng: any, sender: any, receiver: any, need: bigint): boolean
{
  const watermark = sender.$fparams.seed_cot_count(sender.$fmalicious);
  while (((sender.remaining() < need) || ((() => { const __a = sender.remaining(); const __b = need; const __result = __a - __b; return __result < 0n ? 0n : (__result > 18446744073709551615n ? 18446744073709551615n : __result); })() < watermark)))   {
    const before = sender.remaining();
    if (!refill(rng, sender, receiver))     {
      return false;
    }
  }
  return true;
}

export function ensure_receiver<R, Io>(rng: any, receiver: any, io: any, need: bigint)
{
  const watermark = receiver.$fparams.seed_cot_count(receiver.$fmalicious);
  while (((receiver.remaining() < need) || ((() => { const __a = receiver.remaining(); const __b = need; const __result = __a - __b; return __result < 0n ? 0n : (__result > 18446744073709551615n ? 18446744073709551615n : __result); })() < watermark)))   {
    if (receiver.$fmalicious)     {
    } else     {
      stack_refill_receiver(rng, receiver, io);
    }
  }
}

export function ensure_sender<R, Io>(rng: any, sender: any, io: any, need: bigint)
{
  const watermark = sender.$fparams.seed_cot_count(sender.$fmalicious);
  while (((sender.remaining() < need) || ((() => { const __a = sender.remaining(); const __b = need; const __result = __a - __b; return __result < 0n ? 0n : (__result > 18446744073709551615n ? 18446744073709551615n : __result); })() < watermark)))   {
    if (sender.$fmalicious)     {
      stack_refill_sender_malicious(rng, sender, io);
    } else     {
      stack_refill_sender(rng, sender, io);
    }
  }
}

export function eval_abc<S>(ctx: { defaultS: () => any, UClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, z: S[]): [S[], S[], S[]]
{
  const az = [__clone(z[Number(K_A)]), __clone(z[Number(K_C)]), fieldSub(fieldAdd(__clone(z[Number(P1)]), __clone(z[Number(V_HAT)])), __clone(z[Number(P2)]))];
  const bz = [__clone(z[Number(K_B)]), __clone(z[Number(DELTA)]), __clone(z[Number(ctx.UClass)])];
  const cz = [__clone(z[Number(P1)]), __clone(z[Number(P2)]), undefined as any /* S::default() */];
  return [az, bz, cz];
}

export function eval_gate<D>(dd: any, a: any, i: boolean, b: any, j: boolean, taus: RowCt[], prf: any, wire: bigint, degree: bigint, p: bigint): LabelPoly
{
  const out = dd.eval(a, i, b, j, taus);
  return add_coeffs(out, prf.offset(wire, degree, p), p);
}

export function eval_not(c: bigint, q: bigint): bigint
{
  const true_v = (q / 4n);
  return BigInt((((fieldSub((true_v as unknown as bigint), (c as unknown as bigint))) % ((q as unknown as bigint)) + ((q as unknown as bigint))) % ((q as unknown as bigint))));
}

export function evaluate(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, ctx: any, equation: any, circuit: any, offline: any, delta: any, ksk: any, input_labels: Ciphertext[], records: Stitch[], output_key: SecretKey): Result<Vec<boolean>, StitchError>
{
  const __volar_try_0 = circuit.validate();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  if (!__equals(BigInt(input_labels.length), circuit.$finputs))   {
    return new StitchError_Exhausted();
  }
  const top = ((offline.$fwires[0]) ?? (new StitchError_Exhausted())).$fct_a0.level();
  let wires: Vec<Ciphertext> = ([] as any[]);
  for (const label of input_labels)   {
    (wires).push(ctx.switch_to(label, top));
  }
  let record_iter = records;
  for (const [index, gate] of circuit.$fgates.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const out_wire = fieldAdd(circuit.$finputs, index);
    const [ai, bi] = gate.operands();
    const expanded = new ExpandedGate({ $fa: align_wire(ctx, ((offline.wire(ai)) ?? (new StitchError_Exhausted())), top), $fb: align_wire(ctx, ((offline.wire(bi)) ?? (new StitchError_Exhausted())), top), $fout: align_wire(ctx, ((offline.wire(out_wire)) ?? (new StitchError_Exhausted())), top) });
    const la = wires[Number(ai)];
    const lb = wires[Number(bi)];
    const out = (() => { const __match = gate; if (__match instanceof Gate_And) { return (() => {
  const __volar_try_0 = ((record_iter.shift()) ?? (new StitchError_Exhausted()));
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const stitch = __volar_try_0;
  return equation.eval_and(ctx, la, lb, expanded, delta, ksk, stitch);
})(); } else if (__match instanceof Gate_Xor) { return (() => {
  const stitch = (() => { if (equation.xor_is_charged()) {
  return ((record_iter.shift()) ?? (new StitchError_Exhausted()));
} else {
  return undefined;
} })();
  return equation.eval_xor(ctx, la, lb, expanded, delta, ksk, stitch);
})(); } else { return (() => {
  return __clone(la);
})(); } })();
    (wires).push(out);
  }
  if ((record_iter.shift()) != null)   {
    return new StitchError_NonCanonicalBit();
  }
  const p = ctx.parameters().$fplaintext_modulus;
  const __volar_try_9 = ((delta.inverse(p)) ?? (new StitchError_NonCanonicalBit()));
  if (__volar_try_9 instanceof __VolarError) return __volar_try_9;
  const inv_delta = __volar_try_9;
  let outputs = ([] as any[]);
  for (const wire of circuit.$foutputs)   {
    const __volar_try_0 = ((offline.wire(wire)) ?? (new StitchError_Exhausted()));
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const expanded = __volar_try_0;
    const label = wires[Number(wire)];
    const __volar_try_2 = ctx.sub(label, expanded.$fct_a0);
    if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
    const diff = __volar_try_2;
    const __volar_try_3 = ctx.mul_plain(diff, ctx.constant_slots(inv_delta));
    if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
    const payload = __volar_try_3;
    const __volar_try_4 = ctx.decrypt(output_key, payload);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const payload_bits = __volar_try_4;
    const __volar_try_5 = ctx.switch_to(expanded.$fct_pi, payload.level());
    if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
    const ct_pi = __volar_try_5;
    const __volar_try_6 = ctx.decrypt(output_key, ct_pi);
    if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
    const pi_bits = __volar_try_6;
    const masked = (payload_bits[Number(expanded.$fslot_a0)] % 2n);
    const pi = (pi_bits[Number(expanded.$fslot_pi)] % 2n);
    (outputs).push(__equals(fieldBitxor(masked, pi), 1n));
  }
  return outputs;
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
  return BigInt((((restored) % ((q as unknown as bigint)) + ((q as unknown as bigint))) % ((q as unknown as bigint))));
}

export function evaluate_circuit<D>(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, variant: any, circuit: any, dd: any, delta_ct: any, and_rows: AndRows[], records: GateRecord[], prf: any, inputs: [LabelPoly, boolean][]): Result<Vec<boolean>, DriverError>
{
  const __volar_try_0 = circuit.validate();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  if (!__equals(BigInt(inputs.length), circuit.$finputs))   {
    return new DriverError_BadInputCount();
  }
  const delta = dd.delta();
  const p = dd.p();
  const degree = BigInt(delta.$fcoeffs.length);
  let labels: Vec<LabelPoly> = ([] as any[]);
  let masked: Vec<boolean> = ([] as any[]);
  for (const [w, m] of inputs)   {
    (labels).push(__clone(w));
    (masked).push(m);
  }
  let rec = records;
  let and_iter = and_rows;
  for (const [gi, gate] of circuit.$fgates.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const w = fieldAdd(circuit.$finputs, gi);
    const [a, b] = gate.operands();
    (() => { const __match = [variant, gate]; if (true /* pattern Or([Tuple([Wild, Struct { kind: Custom("Gate::And"), fields: [], rest: true }]), Tuple([Wild, Struct { kind: Custom("Gate::And"), fields: [], rest: true }])]) */) { return (() => {
  let __volar_match_result_0 ;
  const __volar_match_0 = rec.shift();
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    const d = __volar_match_0._0;
    __volar_match_result_0 = d;
  }
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    return new DriverError_WrongRecord();
  }
  {
    return new DriverError_MissingRecord();
  }
  const d = __volar_match_result_0;
  let __volar_match_result_1 ;
  const __volar_match_1 = and_iter.shift();
  if (__volar_match_1 !== null && __volar_match_1 !== undefined) {
    const rows = __volar_match_1._0;
    __volar_match_result_1 = rows;
  }
  if (__volar_match_1 !== null && __volar_match_1 !== undefined) {
    return new DriverError_WrongRecord();
  }
  {
    return new DriverError_MissingRecord();
  }
  const taus = __volar_match_result_1;
  const out = eval_gate(dd, labels[Number(a)], masked[Number(a)], labels[Number(b)], masked[Number(b)], taus, prf, w, degree, p);
  const m = fieldBitxor(label_lsb(out), d);
  (labels).push(out);
  (masked).push(m);
})(); } else if (true /* pattern Or([Tuple([Wild, Struct { kind: Custom("Gate::Xor"), fields: [], rest: true }]), Tuple([Wild, Struct { kind: Custom("Gate::Xor"), fields: [], rest: true }])]) */) { return (() => {
  let __volar_match_result_0 ;
  const __volar_match_0 = rec.shift();
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    const d = __volar_match_0._0;
    __volar_match_result_0 = d;
  }
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    return new DriverError_WrongRecord();
  }
  {
    return new DriverError_MissingRecord();
  }
  const d = __volar_match_result_0;
  const taus = xor_rows(delta_ct, d, p);
  const out = eval_gate(dd, labels[Number(a)], masked[Number(a)], labels[Number(b)], masked[Number(b)], taus, prf, w, degree, p);
  (labels).push(out);
  (masked).push(fieldBitxor(fieldBitxor(masked[Number(a)], masked[Number(b)]), d));
})(); } else if (true /* pattern Tuple([Wild, Struct { kind: Custom("Gate::And"), fields: [], rest: true }]) */) { return (() => {
  let __volar_match_result_0 ;
  const __volar_match_0 = rec.shift();
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    const masks = __volar_match_0.$fmasks;
    const det = __volar_match_0.$fdet;
    __volar_match_result_0 = [masks, det];
  }
  if (__volar_match_0 !== null && __volar_match_0 !== undefined) {
    return new DriverError_WrongRecord();
  }
  {
    return new DriverError_MissingRecord();
  }
  const [masks, det] = __volar_match_result_0;
  let __volar_match_result_1 ;
  const __volar_match_1 = and_iter.shift();
  if (__volar_match_1 !== null && __volar_match_1 !== undefined) {
    const rows = __volar_match_1._0;
    __volar_match_result_1 = rows;
  }
  if (__volar_match_1 !== null && __volar_match_1 !== undefined) {
    return new DriverError_WrongRecord();
  }
  {
    return new DriverError_MissingRecord();
  }
  const r_rows = __volar_match_result_1;
  const taus = fast_and_rows(delta_ct, r_rows, masks, p);
  const out = eval_gate(dd, labels[Number(a)], masked[Number(a)], labels[Number(b)], masked[Number(b)], taus, prf, w, degree, p);
  const m = fieldBitxor(label_lsb(out), det);
  (labels).push(out);
  (masked).push(m);
})(); } else if (true /* pattern Tuple([Wild, Struct { kind: Custom("Gate::Xor"), fields: [], rest: true }]) */) { return (() => {
  (() => { const __match = rec.shift(); if (__match !== null && __match !== undefined) { return (() => {
})(); } else if (__match !== null && __match !== undefined) { return new DriverError_WrongRecord(); } else { return new DriverError_MissingRecord(); } })();
  const taus = xor_rows(delta_ct, false, p);
  const out = eval_gate(dd, labels[Number(a)], masked[Number(a)], labels[Number(b)], masked[Number(b)], taus, prf, w, degree, p);
  (labels).push(out);
  (masked).push(fieldBitxor(masked[Number(a)], masked[Number(b)]));
})(); } else { return (() => {
  (labels).push(__clone(labels[Number(a)]));
  (masked).push(masked[Number(a)]);
})(); } })();
  }
  return circuit.$foutputs.map((o: any) => masked[Number(o)]);
}

export function evaluate_gate(cert_eighths: any, inputs: bigint[], q: bigint): bigint
{
  const scale = (q / 8n);
  const scaled = new GateCertificate({ $fname: cert_eighths.$fname, $farity: cert_eighths.$farity, $fprepare: cert_eighths.$fprepare, $finterval_true: [BigInt(Math.imul(Number(cert_eighths.$finterval_true[0]), Number(scale))), BigInt(Math.imul(Number(cert_eighths.$finterval_true[1]), Number(scale)))] });
  return evaluate_certificate(scaled, inputs, q);
}

export function execute_plan(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, priv_ell: bigint, priv_base_log: bigint, plan: any, inputs: BinfheLweCiphertextDyn[], cells: BinfheRlweCiphertextDyn[], bk: BinfheBootstrappingKeyDyn, cbk: CircuitBootstrappingKeyDyn): [Vec<BinfheLweCiphertextDyn>, Vec<BinfheRlweCiphertextDyn>]
{
  let workspace = (PlanWorkspaceDyn.new(ctx, n_lwe, big_n, bs_ell, plan))!;
  return (execute_plan_in_workspace(ctx, n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, priv_ell, priv_base_log, plan, inputs, cells, bk, cbk, workspace).map(([wires, cells]: any) => [[...wires], [...cells]]))!;
}

export function execute_plan_in_workspace(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n_lwe: bigint, big_n: bigint, log_q: bigint, log_q_lwe: bigint, log_mod_ks: bigint, bs_ell: bigint, bs_base_log: bigint, ks_ell: bigint, ks_base_log: bigint, priv_ell: bigint, priv_base_log: bigint, plan: any, inputs: BinfheLweCiphertextDyn[], cells: BinfheRlweCiphertextDyn[], bk: BinfheBootstrappingKeyDyn, cbk: CircuitBootstrappingKeyDyn, workspace: PlanWorkspaceDyn): Result<[BinfheLweCiphertextDyn[], BinfheRlweCiphertextDyn[]], PlanWorkspaceError>
{
  const __volar_try_0 = plan.validate();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  if (!__equals(BigInt(inputs.length), BigInt(plan.$fnum_inputs)))   {
    return new PlanWorkspaceError_InputCount(BigInt(plan.$fnum_inputs), BigInt(inputs.length));
  }
  if (!__equals(BigInt(cells.length), BigInt(plan.$fnum_cells)))   {
    return new PlanWorkspaceError_CellCount(BigInt(plan.$fnum_cells), BigInt(cells.length));
  }
  const __volar_try_3 = plan.workspace_requirements(ctx, );
  if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
  const required = __volar_try_3;
  if (((((workspace.$fwires.capacity() < required.$fwires) || (workspace.$frgsws.capacity() < required.$frgsws)) || (workspace.$fcells.capacity() < required.$fcells)) || (workspace.$flut_inputs.capacity() < required.$flut_inputs)))   {
    return new PlanWorkspaceError_CapacityOverflow();
  }
  workspace.reset();
  workspace.$fwires.push(...(inputs));
  workspace.$fcells.push(...(cells));
  const delta = wire_delta(log_q_lwe, BigInt(plan.$fk_max));
  for (const layer of plan.$flayers)   {
    for (const op of layer)     {
      (() => { const __match = op; if (__match instanceof PlanOp_Const) { const out = __match.$fout;
const value = __match.$fvalue;
return (() => {
  (workspace.$fwires).push(binfhe_trivial(n_lwe, log_q_lwe, value, delta));
})(); } else if (__match instanceof PlanOp_Not) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  const input_1 = workspace.$fwires[Number(BigInt(input))];
  (workspace.$fwires).push(binfhe_not(n_lwe, log_q_lwe, input_1, delta));
})(); } else if (__match instanceof PlanOp_Lut) { const inputs = __match.$finputs;
const table = __match.$ftable;
const out = __match.$fout;
return (() => {
  const spec = plan.$fluts[Number(BigInt(table))];
  const arity = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, BigInt(spec.$fentries.length))));
  workspace.$flut_inputs.clear();
  workspace.$flut_inputs.push(...(inputs.map((w: any) => workspace.$fwires[Number(BigInt(w))])));
  (workspace.$fwires).push(binfhe_lut_read_dyn(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, workspace.$flut_inputs, spec.$fentries, BigInt(plan.$fk_max), bk));
})(); } else if (__match instanceof PlanOp_CircuitBootstrap) { const input = __match.$finput;
const out = __match.$fout;
return (() => {
  const input_1 = workspace.$fwires[Number(BigInt(input))];
  (workspace.$frgsws).push(circuit_bootstrap(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, ks_ell, priv_ell, priv_base_log, input_1, cbk, BigInt(plan.$fk_max)));
})(); } else { const sel = __match.$fsel;
const then_cell = __match.$fthen_cell;
const else_cell = __match.$felse_cell;
const out = __match.$fout;
return (() => {
  const out_cell = binfhe_rgsw_cmux(workspace.$frgsws[Number(BigInt(sel))], workspace.$fcells[Number(BigInt(then_cell))], workspace.$fcells[Number(BigInt(else_cell))]);
  (workspace.$fcells).push(out_cell);
})(); } })();
    }
  }
  return [workspace.$fwires, workspace.$fcells];
}

export function expand_challenge_to_deltas(chall_1: bigint[], tau: bigint, n: bigint): Vec<bigint>
{
  let out: Vec<bigint> = ([] as any[]);
  for (let i = 0n; i < tau; i += 1n)   {
    const byte = BigInt(chall_1[Number((i % BigInt(chall_1.length)))]);
    (out).push((byte % n));
  }
  return out;
}

export function expand_full(depth: bigint, seed: any): [Vec<Block>, Vec<Block[]>]
{
  let level: Vec<Block> = [seed];
  let sums = ([] as any[]);
  for (let _d = 0n; _d < depth; _d += 1n)   {
    let next = ([] as any[]);
    let k0 = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
    let k1 = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
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
  let level: Vec<Block> = Array.from({length: Number(2n)}, () => Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
  let offset = 0n;
  for (let d = 0n; d < depth; d += 1n)   {
    const select = bit_msb(alpha, depth, d);
    recover_sibling(level, sums[Number(d)], offset, select);
    if (__equals(fieldAdd(d, 1n), depth))     {
      break;
    }
    let next = Array.from({length: Number(fieldMul(BigInt(level.length), 2n))}, () => Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
    for (const [j, node] of level.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const [l, r] = g_double(node);
      next[Number(fieldMul(2n, j))] = l;
      next[Number(fieldAdd(fieldMul(2n, j), 1n))] = r;
    }
    offset = fieldShl(fieldAdd(offset, BigInt(select)), 1n);
    level = next;
  }
  return level;
}

export function exponent(log_q_lwe: bigint, big_n: bigint, x: bigint): bigint
{
  return fieldBitand(BigInt(x), fieldSub(fieldMul(2n, big_n), 1n));
}

export function external_product(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, rgsw: RgswCiphertextDyn, rlwe: RlweCiphertextDyn): RlweCiphertextDyn
{
  const a_decomp = poly_decompose(big_n, bs_ell, bs_bg_log, rlwe.$fa);
  const b_decomp = poly_decompose(big_n, bs_ell, bs_bg_log, rlwe.$fb);
  let out_a = Array.from({length: Number(big_n)}, () => 0n);
  let out_b = Array.from({length: Number(big_n)}, () => 0n);
  for (let j = 0n; j < bs_ell; j += 1n)   {
    const row = rgsw.$frows[Number(j)];
    const prod_a0 = poly_mul_neg(big_n, a_decomp[Number(j)], row.$frlwe0.$fa);
    const prod_a1 = poly_mul_neg(big_n, a_decomp[Number(j)], row.$frlwe0.$fb);
    const prod_b0 = poly_mul_neg(big_n, b_decomp[Number(j)], row.$frlwe1.$fa);
    const prod_b1 = poly_mul_neg(big_n, b_decomp[Number(j)], row.$frlwe1.$fb);
    for (let k = 0n; k < big_n; k += 1n)     {
      out_a[Number(k)] = wrappingAdd(wrappingAdd(out_a[Number(k)], prod_a0[Number(k)], 32), prod_b0[Number(k)], 32);
      out_b[Number(k)] = wrappingAdd(wrappingAdd(out_b[Number(k)], prod_a1[Number(k)], 32), prod_b1[Number(k)], 32);
    }
  }
  return new RlweCiphertextDyn({ $fa: out_a, $fb: out_b, $fbig_n: 0n });
}

export function fast_and_rows(delta_ct: any, r: RowCt[], d: boolean[], p: bigint): RowCt[]
{
  let z: Vec<RowCt> = ([] as any[]);
  for (let k = 0n; k < 4n; k += 1n)   {
    (z).push((() => { if (d[Number(k)]) {
  return sub_rows(delta_ct, r[Number(k)], p);
} else {
  return __clone(r[Number(k)]);
} })());
  }
  const t1 = sub_rows(z[Number(1n)], z[Number(0n)], p);
  const t2 = sub_rows(z[Number(2n)], z[Number(0n)], p);
  const t3_pre = sub_rows(z[Number(3n)], z[Number(0n)], p);
  const t3 = sub_rows(sub_rows(t3_pre, t1, p), t2, p);
  return [t1, t2, t3];
}

export function fast_mask_bits(z: boolean[], r: boolean[]): boolean[]
{
  let out = Array.from({length: Number(4n)}, () => false);
  for (let k = 0n; k < 4n; k += 1n)   {
    out[Number(k)] = fieldBitxor(z[Number(k)], r[Number(k)]);
  }
  return out;
}

export function fast_unmask_coeffs(d: boolean): [bigint, bigint]
{
  return (() => { if (d) {
  return [1n, -1n];
} else {
  return [0n, 1n];
} })();
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
      const [r1, b1] = [wrappingSub(x[Number(i)], P_LIMBS[Number(i)], 64), false];
      const [r2, b2] = [wrappingSub(r1, borrow, 64), false];
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
    let chunk = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
    (chunk).splice(0, (b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).length, ...(b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))));
    limbs[Number(i)] = u64_from_le_bytes(chunk);
  }
  return reduce_wide([limbs[Number(0n)], limbs[Number(1n)], limbs[Number(2n)], limbs[Number(3n)], 0n, 0n, 0n, 0n]);
}

export function fe_invert(a: any): Fe25519
{
  const exp_limbs: bigint[] = [18446744073709551595n, 18446744073709551615n, 18446744073709551615n, 9223372036854775807n];
  let acc = new Fe25519([1n, 0n, 0n, 0n]);
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
  return new Fe25519([0n, 0n, 0n, 0n]);
} else {
  let neg = Array.from({length: Number(4n)}, () => 0n);
  let borrow: bigint = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const [r1, br1] = [wrappingSub(P_LIMBS[Number(i)], a[0][Number(i)], 64), false];
    const [r2, br2] = [wrappingSub(r1, borrow, 64), false];
    neg[Number(i)] = r2;
    borrow = fieldBitor(BigInt(br1), BigInt(br2));
  }
  return new Fe25519(neg);
} })();
}

export function fe_pow(base: any, exp: bigint[]): Fe25519
{
  let acc = new Fe25519([1n, 0n, 0n, 0n]);
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
    const [r1, br1] = [wrappingSub(P_LIMBS[Number(i)], b[0][Number(i)], 64), false];
    const [r2, br2] = [wrappingSub(r1, borrow, 64), false];
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

export function ferret_extend_malicious<R>(rng: any, params: any, sender_seed: any, receiver_seed: any, transcript: bigint[]): [FerretExtendOut, boolean]
{
  const m = params.seed_cot_count(true);
  const k = params.$fk;
  const n = params.$fn;
  const t = params.$ft;
  const spcot_cots = fieldMul(t, params.log_splen());
  const extra_off = fieldAdd(k, spcot_cots);
  const alphas = sample_regular_noise(rng, n, t);
  let e = Array.from({length: Number(n)}, () => false);
  for (const [i, a] of alphas.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const [start, end] = (params.interval_bounds(i))!;
    e[Number(fieldAdd(start, a))] = true;
  }
  const lpn_seed = sample_seed(rng);
  const choices = mpcot_reg_choice_bits(n, t, alphas, receiver_seed.$fu.slice(Number(k), Number(extra_off)));
  const [s_padded, mpcot] = mpcot_reg_sender_padded(rng, sender_seed.$fdelta, n, t, sender_seed.$fq.slice(Number(k), Number(extra_off)), choices);
  const r_padded = mpcot_reg_receiver_padded(n, t, alphas, receiver_seed.$fw.slice(Number(k), Number(extra_off)), mpcot);
  const extra_q = sender_seed.$fq.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const extra_r = receiver_seed.$fu.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const extra_t = receiver_seed.$fw.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const check = mpcot_reg_consistency_check_padded(sender_seed.$fdelta, s_padded, r_padded, alphas, extra_q, extra_r, extra_t, transcript);
  const s = mpcot_reg_flatten_outputs(n, t, s_padded);
  const r = mpcot_reg_flatten_outputs(n, t, r_padded);
  const prep = new FerretPrep({ $falphas: alphas, $fe: e, $flpn_seed: lpn_seed, $fchoices: [] as any[] });
  const out = ferret_finish_m(params, sender_seed, receiver_seed, prep, s, r, m);
  return [out, check];
}

export function ferret_extend_uni<R>(rng: any, params: any, hash_seed: bigint[], sender_seed: any, receiver_seed: any): FerretExtendOut
{
  const m_seed = uni_seed_cot_count(hash_seed, params);
  const k = params.$fk;
  const heights = uni_spcot_heights(hash_seed, params.$fn, params.$ft);
  const points = sample_uniform_points(rng, params.$fn, params.$ft);
  const table = cuckoo_insert(hash_seed, params.$fn, params.$ft, points);
  let e = Array.from({length: Number(params.$fn)}, () => false);
  for (const slot of table)   {
    (() => { const __match = slot; if (__match !== null && __match !== undefined) { const x = __match;
return (() => {
  e[Number(x)] = true;
})(); } else { return (() => {
})(); } })();
  }
  const cot_r = split_cot_chunks(receiver_seed.$fu.slice(Number(k)), heights);
  const choices = mpcot_uni_choice_bits(params, hash_seed, table, cot_r);
  const cot_q = split_cot_chunks(sender_seed.$fq.slice(Number(k)), heights);
  const [s, _s_bins, mpcot] = mpcot_uni_sender(rng, sender_seed.$fdelta, params, hash_seed, cot_q, choices);
  const cot_t = split_cot_chunks(receiver_seed.$fw.slice(Number(k)), heights);
  const [r, _r_bins] = mpcot_uni_receiver(params, table, cot_t, mpcot);
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

export function ferret_extend_uni_malicious<R>(rng: any, params: any, hash_seed: bigint[], sender_seed: any, receiver_seed: any, transcript: bigint[]): [FerretExtendOut, boolean]
{
  const m_seed = uni_seed_cot_count_malicious(hash_seed, params);
  const k = params.$fk;
  const heights = uni_spcot_heights(hash_seed, params.$fn, params.$ft);
  const spcot_cots: bigint = heights.reduce((__sum: bigint, __x: bigint) => __sum + __x, 0n);
  const extra_off = fieldAdd(k, spcot_cots);
  const points = sample_uniform_points(rng, params.$fn, params.$ft);
  const table = cuckoo_insert(hash_seed, params.$fn, params.$ft, points);
  let e = Array.from({length: Number(params.$fn)}, () => false);
  for (const slot of table)   {
    (() => { const __match = slot; if (__match !== null && __match !== undefined) { const x = __match;
return (() => {
  e[Number(x)] = true;
})(); } else { return (() => {
})(); } })();
  }
  const cot_r = split_cot_chunks(receiver_seed.$fu.slice(Number(k), Number(extra_off)), heights);
  const choices = mpcot_uni_choice_bits(params, hash_seed, table, cot_r);
  const cot_q = split_cot_chunks(sender_seed.$fq.slice(Number(k), Number(extra_off)), heights);
  const [s, s_bins, mpcot] = mpcot_uni_sender(rng, sender_seed.$fdelta, params, hash_seed, cot_q, choices);
  const cot_t = split_cot_chunks(receiver_seed.$fw.slice(Number(k), Number(extra_off)), heights);
  const [r, r_bins] = mpcot_uni_receiver(params, table, cot_t, mpcot);
  const [alphas, lens] = mpcot_uni_bucket_params(params, hash_seed, table);
  const extra_q = sender_seed.$fq.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const extra_r = receiver_seed.$fu.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const extra_t = receiver_seed.$fw.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const check = mpcot_uni_consistency_check(sender_seed.$fdelta, s_bins, r_bins, alphas, lens, extra_q, extra_r, extra_t, transcript);
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
  const out = new FerretExtendOut({ $fsender_out: [...y.slice(Number(m_seed))], $frecv_x: [...x.slice(Number(m_seed))], $frecv_z: [...z.slice(Number(m_seed))], $fsender_seed: new FerretSenderSeed({ $fdelta: sender_seed.$fdelta, $fq: [...y.slice(0, Number(m_seed))] }), $freceiver_seed: new FerretReceiverSeed({ $fu: [...x.slice(0, Number(m_seed))], $fw: [...z.slice(0, Number(m_seed))] }) });
  return [out, check];
}

export function ferret_finish(params: any, sender_seed: any, receiver_seed: any, prep: any, s: Block[], r: Block[]): FerretExtendOut
{
  return ferret_finish_m(params, sender_seed, receiver_seed, prep, s, r, params.seed_cot_count(false));
}

export function ferret_finish_m(params: any, sender_seed: any, receiver_seed: any, prep: any, s: Block[], r: Block[], m: bigint): FerretExtendOut
{
  const n = params.$fn;
  const k = params.$fk;
  const v_lpn = sender_seed.$fq.slice(0, Number(k));
  const u_lpn = receiver_seed.$fu.slice(0, Number(k));
  const w_lpn = receiver_seed.$fw.slice(0, Number(k));
  let [y, x, z] = encode_ferret_outputs(prep.$flpn_seed, k, n, v_lpn, u_lpn, w_lpn, s, prep.$fe, r);
  const sender_out = ((__vector: any[], __at: number) => __vector.splice(__at))(y, Number(m));
  const recv_x = ((__vector: any[], __at: number) => __vector.splice(__at))(x, Number(m));
  const recv_z = ((__vector: any[], __at: number) => __vector.splice(__at))(z, Number(m));
  const sender_seed_1 = new FerretSenderSeed({ $fdelta: sender_seed.$fdelta, $fq: y });
  const receiver_seed_1 = new FerretReceiverSeed({ $fu: x, $fw: z });
  return new FerretExtendOut({ $fsender_out: sender_out, $frecv_x: recv_x, $frecv_z: recv_z, $fsender_seed: sender_seed_1, $freceiver_seed: receiver_seed_1 });
}

export function ferret_prepare_receiver<R>(rng: any, params: any, receiver_seed: any): FerretPrep
{
  const n = params.$fn;
  const t = params.$ft;
  const k = params.$fk;
  const alphas = sample_regular_noise(rng, n, t);
  let e = Array.from({length: Number(n)}, () => false);
  for (const [i, a] of alphas.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const [start, end] = (params.interval_bounds(i))!;
    e[Number(fieldAdd(start, a))] = true;
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
  return new Galois128(u128_from_le_bytes(b));
}

export function field_mul(a: Galois128, b: Galois128): Galois128
{
  return fieldMul(a, b);
}

export function field_mul_block(a: any, b: any): Block
{
  return block_from_field(field_mul(field_from_block(a), field_from_block(b)));
}

export function fill_test_poly(big_n: bigint, logical: boolean[], addr_bits: bigint, k_max: bigint, log_q: bigint, log_q_lwe: bigint): bigint[]
{
  const table_len = fieldShl(1n, addr_bits);
  const is_constant = table_is_constant(logical);
  const delta_shift = fieldSub(fieldSub(log_q_lwe, 1n), BigInt(k_max));
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

export function find_negacyclic_root(...__args: any[]): any {
  if (__args.length === 2) {
    const degree = __args[0];
    const modulus = __args[1];
    return (() => {
  const exponent = (fieldSub(modulus, 1n) / fieldMul(2n, BigInt(degree)));
  for (let candidate = 2n; candidate < modulus; candidate += 1n)   {
    const root = pow_mod(candidate, BigInt(exponent), modulus);
    if (__equals(pow_mod(root, degree, modulus), fieldSub(modulus, 1n)))     {
      return root;
    }
  }
  return undefined;
})();
  } else
  if (__args.length === 2) {
    const degree = __args[0];
    const modulus = __args[1];
    return (() => {
  const exponent = (fieldSub(modulus, 1n) / fieldMul(2n, BigInt(degree)));
  for (let candidate = 2n; candidate < modulus; candidate += 1n)   {
    const root = pow_mod(candidate, BigInt(exponent), modulus);
    if (__equals(pow_mod(root, degree, modulus), fieldSub(modulus, 1n)))     {
      return root;
    }
  }
  return undefined;
})();
  }
  throw new Error("find_negacyclic_root(): no matching variant for " + __args.length + " args");
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
  return [left, right];
}

export function gadget_decompose(log: bigint, ell: bigint, base_log: bigint, x: bigint): bigint[]
{
  let digits = Array.from({length: Number(ell)}, () => 0n);
  for (let __mut_1 = 0n; __mut_1 < BigInt(digits.length); __mut_1 += 1n) {
    const j = __mut_1;
    {
      const shift = level_shift(log, base_log, j);
      const bits = level_bits(log, base_log, j);
      const m = (() => { if ((bits >= 32n)) {
  return 4294967295n;
} else {
  return fieldSub(fieldShl(1n, bits), 1n);
} })();
      digits[Number(__mut_1)] = fieldBitand(fieldShr(x, shift), m);
    }}
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

export function garble(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, ctx: any, equation: any, circuit: any, states: WireState[], delta: any): Result<Vec<Stitch>, StitchError>
{
  const __volar_try_0 = circuit.validate();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const p = ctx.parameters().$fplaintext_modulus;
  if ((delta.value() >= p))   {
    return new StitchError_NonCanonicalBit();
  }
  if (!__equals(BigInt(states.length), circuit.wires()))   {
    return new StitchError_Exhausted();
  }
  let wires: Vec<WireState> = [...states];
  let records = [] as any[];
  for (const [index, gate] of circuit.$fgates.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const out_wire = fieldAdd(circuit.$finputs, index);
    const [ai, bi] = gate.operands();
    const [a, b] = [wires[Number(ai)], wires[Number(bi)]];
    (() => { const __match = gate; if (__match instanceof Gate_And) { return (() => {
  const out = wires[Number(out_wire)];
  (records).push(equation.garble_and(index, a, b, out));
})(); } else if (__match instanceof Gate_Xor) { return (() => {
  let out = wires[Number(out_wire)];
  if (!equation.xor_is_charged())   {
    out = new WireState({ $fa0: (fieldAdd(a.$fa0, b.$fa0) % p), $fpi: fieldBitxor(a.$fpi, b.$fpi) });
    wires[Number(out_wire)] = out;
  }
  return (() => { const __match = equation.garble_xor(p, a, b, out); if (__match !== null && __match !== undefined) { const stitch = __match;
return (() => {
  (records).push(stitch);
})(); } else { return (() => {
})(); } })();
})(); } else { return (() => {
  wires[Number(out_wire)] = new WireState({ $fa0: a.$fa0, $fpi: !a.$fpi });
})(); } })();
  }
  return records;
}

export function garble_circuit<D>(ctx: { B_OutputSize: bigint, D_OutputSize: bigint, newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, variant: any, circuit: any, dd: any, pis: boolean[], prf: any, prg_bits: boolean[], random: any): Result<PinnedGarbling, DriverError>
{
  const __volar_try_0 = circuit.validate();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const delta = dd.delta();
  const p = dd.p();
  const degree = BigInt(delta.$fcoeffs.length);
  if (!__equals(BigInt(pis.length), circuit.wires()))   {
    return new DriverError_BadInputCount();
  }
  const delta_ct = dd.row(1n, random);
  let pi: Vec<boolean> = [...pis];
  let labels: Vec<WireLabel> = ([] as any[]);
  let and_rows: Vec<AndRows> = [] as any[];
  let records = [] as any[];
  let fast_and = 0n;
  for (let w = 0n; w < circuit.wires(); w += 1n)   {
    if ((BigInt(labels.length) <= w))     {
      (labels).push(new WireLabel({ $fw0: sample_poly(degree, p, random), $fpi: pi[Number(w)] }));
    }
    if ((w < circuit.$finputs))     {
      continue;
    }
    const gate = circuit.$fgates[Number(fieldSub(w, circuit.$finputs))];
    const [a, b] = gate.operands();
    const wa = labels[Number(a)].base(delta);
    const wb = labels[Number(b)].base(delta);
    (() => { const __match = [variant, gate]; if (true /* pattern Or([Tuple([Wild, Struct { kind: Custom("Gate::And"), fields: [], rest: true }]), Tuple([Wild, Struct { kind: Custom("Gate::And"), fields: [], rest: true }])]) */) { return (() => {
  const z = masked_table(true, pi[Number(a)], pi[Number(b)], pi[Number(w)]);
  const t = row_multipliers(z);
  const taus = [dd.row(t[Number(0n)], random), dd.row(t[Number(1n)], random), dd.row(t[Number(2n)], random)];
  const out = gate_output_label(dd, wa, wb, taus, z[Number(0n)], prf, w, pi[Number(w)]);
  const det = determination_bit(out.w0_lsb(), pi[Number(w)]);
  labels[Number(w)] = out;
  (and_rows).push(new AndRows_Small(taus));
  (records).push(new GateRecord_SmallAnd(det));
})(); } else if (true /* pattern Or([Tuple([Wild, Struct { kind: Custom("Gate::Xor"), fields: [], rest: true }]), Tuple([Wild, Struct { kind: Custom("Gate::Xor"), fields: [], rest: true }])]) */) { return (() => {
  const d = fieldBitxor(fieldBitxor(pi[Number(a)], pi[Number(b)]), pi[Number(w)]);
  const taus = xor_rows(delta_ct, d, p);
  const out = gate_output_label(dd, wa, wb, taus, d, prf, w, pi[Number(w)]);
  labels[Number(w)] = out;
  (records).push(new GateRecord_SmallXor(d));
})(); } else if (true /* pattern Tuple([Wild, Struct { kind: Custom("Gate::And"), fields: [], rest: true }]) */) { return (() => {
  const out_pi = __equals(fieldBitand(random.next_u32(), 1n), 1n);
  pi[Number(w)] = out_pi;
  const z = masked_table(true, pi[Number(a)], pi[Number(b)], out_pi);
  const r: boolean[] = [(prg_bits?.[fieldMul(4n, fast_and)] ?? (false)), (prg_bits?.[fieldAdd(fieldMul(4n, fast_and), 1n)] ?? (false)), (prg_bits?.[fieldAdd(fieldMul(4n, fast_and), 2n)] ?? (false)), (prg_bits?.[fieldAdd(fieldMul(4n, fast_and), 3n)] ?? (false))];
  fast_and = fieldAdd(fast_and, 1n);
  const masks = fast_mask_bits(z, r);
  const r_rows = [dd.row(BigInt(r[Number(0n)]), random), dd.row(BigInt(r[Number(1n)]), random), dd.row(BigInt(r[Number(2n)]), random), dd.row(BigInt(r[Number(3n)]), random)];
  const taus = fast_and_rows(delta_ct, r_rows, masks, p);
  const out = gate_output_label(dd, wa, wb, taus, z[Number(0n)], prf, w, out_pi);
  const det = determination_bit(out.w0_lsb(), out_pi);
  labels[Number(w)] = out;
  (and_rows).push(new AndRows_Fast(r_rows));
  (records).push(new GateRecord_FastAnd(masks, det));
})(); } else if (true /* pattern Tuple([Wild, Struct { kind: Custom("Gate::Xor"), fields: [], rest: true }]) */) { return (() => {
  const out_pi = fieldBitxor(pi[Number(a)], pi[Number(b)]);
  pi[Number(w)] = out_pi;
  const taus = xor_rows(delta_ct, false, p);
  const out = gate_output_label(dd, wa, wb, taus, false, prf, w, out_pi);
  labels[Number(w)] = out;
  (records).push(new GateRecord_FastXor());
})(); } else { return (() => {
  const out_pi = !pi[Number(a)];
  pi[Number(w)] = out_pi;
  const base = labels[Number(a)].base(delta);
  const w0 = (() => { if (out_pi) {
  return sub_coeffs(base, delta.$fcoeffs, p);
} else {
  return base;
} })();
  labels[Number(w)] = new WireLabel({ $fw0: w0, $fpi: out_pi });
})(); } })();
  }
  return new PinnedGarbling({ $flabels: labels, $fdelta_ct: delta_ct, $fand_rows: and_rows, $frecords: records });
}

export function gate_output_label<D>(dd: any, wa_base: any, wb_base: any, taus: RowCt[], z00: boolean, prf: any, wire: bigint, out_pi: boolean): WireLabel
{
  const delta = dd.delta();
  const p = dd.p();
  const degree = BigInt(delta.$fcoeffs.length);
  let out = dd.eval(wa_base, false, wb_base, false, taus);
  if (z00)   {
    out = sub_coeffs(out, delta.$fcoeffs, p);
  }
  out = add_coeffs(out, prf.offset(wire, degree, p), p);
  const w0 = (() => { if (out_pi) {
  return sub_coeffs(out, delta.$fcoeffs, p);
} else {
  return out;
} })();
  return new WireLabel({ $fw0: w0, $fpi: out_pi });
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
  return new ABODyn({ $fcommit: [...Array.from(h.finalize(), BigInt)], $fper_byte: per_byte, $fk: 0n, $fn: 0n });
}

export function gen_bootstrapping_key<R>(n_lwe: bigint, big_n: bigint, bs_ell: bigint, ks_ell: bigint, bs_bg_log: bigint, ks_bg_log: bigint, lwe_sk: LweSecretKeyDyn, rlwe_sk: RlweSecretKeyDyn, bs_noise_bits: bigint, ks_noise_bits: bigint, rng: any): BootstrappingKeyDyn
{
  const bsk = Array.from({length: Number(n_lwe - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const bit = !__equals(lwe_sk.$fkey[Number(i)], 0n);
  return rgsw_encrypt(big_n, bs_ell, bs_bg_log, bit, rlwe_sk, bs_noise_bits, rng);
})());
  const ksk_array: LweCiphertextDyn[][] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const s_bit = rlwe_sk.$fkey[Number(i)];
  return Array.from({length: Number(ks_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const shift = (() => { const __a = 32n; const __b = BigInt(fieldMul(ks_bg_log, fieldAdd(j, 1n))); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
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
  p[Number(0n)] = torus_neg(log_q, 1n);
  return p;
})();
  const neg_sk: bigint[] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => torus_neg(log_q, rlwe_sk.$fkey[Number(i)]));
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
    (a_col).push(Array.from({length: Number(priv_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
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
    (b_col).push(Array.from({length: Number(priv_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, msg, l, priv_base_log, rlwe_sk, rng);
})()));
  }
  const a_body = Array.from({length: Number(priv_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, neg_sk, l, priv_base_log, rlwe_sk, rng);
})());
  const b_body = Array.from({length: Number(priv_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((l: any) => (() => {
  return encrypt_scaled_poly(big_n, log_q, eta, one_const, l, priv_base_log, rlwe_sk, rng);
})());
  return new CircuitBootstrappingKeyDyn({ $fbk: bk, $fprivksk: new PrivateKeySwitchingKeyDyn({ $fa_col: a_col, $fb_col: b_col, $fa_body: a_body, $fb_body: b_body, $fbig_n: 0n, $fpriv_ell: 0n }), $fn_lwe: 0n, $fbig_n: 0n, $fbs_ell: 0n, $fks_ell: 0n, $fpriv_ell: 0n });
}

export function gen_lwe_secret_key<R>(n_lwe: bigint, rng: any): LweSecretKeyDyn
{
  let key = Array.from({length: Number(n_lwe)}, () => ((0n) & 0xFFn));
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(key.length); __zip_mut_1 += 1n) {
    {
      key[Number(__zip_mut_1)] = ((fieldBitand(rng.next_u8(), 1n)) & 0xFFn);
    }}
  return new LweSecretKeyDyn({ $fkey: key, $fn_lwe: 0n });
}

export function gen_rlwe_secret_key<R>(big_n: bigint, rng: any): RlweSecretKeyDyn
{
  let key = Array.from({length: Number(big_n)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(key.length); __zip_mut_1 += 1n) {
    {
      key[Number(__zip_mut_1)] = BigInt(fieldBitand(rng.next_u8(), 1n));
    }}
  return new RlweSecretKeyDyn({ $fkey: key, $fbig_n: 0n });
}

export function gf_invert_256(a: any, c: any): U256
{
  if (a.is_zero())   {
    return new U256([0n, 0n, 0n, 0n]);
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
  let p = new U256([0n, 0n, 0n, 0n]);
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

export function gram_decode_label(ctx: { newD: () => any, DClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n: bigint, label: EvalDyn, base: GarbleDyn): boolean
{
  return !__equals(fieldBitand(label.open(base.$fcom_bytes, base)[Number(0n)], 1n), 0n);
}

export function gram_regarble(ctx: { defaultT: () => any }, n: bigint, secret: GlobalSecretDyn, base: GarbleDyn, bit: boolean): EvalDyn
{
  return secret.encode(base, bit);
}

export function grind_chall3(ctx: { defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, chall_2: bigint[], a_hat: bigint[], b_hat: bigint[], c_hat_base: bigint[], lambda: bigint, w_grind: bigint, use_shake256: boolean, max_iters: bigint): ([Vec<bigint>, bigint] | undefined)
{
  for (let counter = 0n; counter < max_iters; counter += 1n)   {
    const counter_bytes = [(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn];
    let c_hat_grind: Vec<bigint> = ([] as any[]);
    for (let i = 0n; i < BigInt(c_hat_base.length); i += 1n)     {
      (c_hat_grind).push(c_hat_base[Number(i)]);
    }
    for (let i = 0n; i < BigInt(counter_bytes.length); i += 1n)     {
      (c_hat_grind).push(counter_bytes[Number(i)]);
    }
    const candidate = chall3(ctx, chall_2, a_hat, b_hat, c_hat_grind, lambda, use_shake256);
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
  const n_1 = BigInt(n);
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
    const mask = fieldSub(fieldShl(((1n) & 0xFFn), rem), 1n);
    const idx = fieldSub(fieldSub(BigInt(bytes.length), full_bytes), 1n);
    if (!__equals(fieldBitand(bytes[Number(idx)], mask), 0n))     {
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
  const n_1 = BigInt(n);
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
    const mask = fieldSub(fieldShl(((1n) & 0xFFn), rem_bits), 1n);
    const byte_idx = fieldSub(fieldSub(BigInt(bytes.length), full_bytes), 1n);
    if (!__equals(fieldBitand(bytes[Number(byte_idx)], mask), 0n))     {
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
  h.update([102n, 101n, 114n, 114n, 101n, 116n, 45n, 99n, 117n, 99n, 107n, 111n, 111n, 45n, 104n, 45n, 118n, 49n]);
  h.update(seed);
  h.update([(BigInt(i)) & 0xFFn, ((BigInt(i)) >> 8n) & 0xFFn, ((BigInt(i)) >> 16n) & 0xFFn, ((BigInt(i)) >> 24n) & 0xFFn]);
  h.update([(BigInt(x)) & 0xFFn, ((BigInt(x)) >> 8n) & 0xFFn, ((BigInt(x)) >> 16n) & 0xFFn, ((BigInt(x)) >> 24n) & 0xFFn]);
  const out = [...Array.from(h.finalize(), BigInt)];
  const raw = u64_from_le_bytes((out.slice(0, Number(8n)))!);
  return (BigInt(raw) % m);
}

export function hash_key_from_chall(chall: bigint[]): UniversalHashKey
{
  let r0_bytes = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  const n = BigInt(Math.min(Number(BigInt(chall.length)), Number(16n)));
  (r0_bytes.slice(0, Number(n))).splice(0, (chall.slice(0, Number(n))).length, ...(chall.slice(0, Number(n))));
  let r1_bytes = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
  const off = n;
  const m = BigInt(Math.min(Number(fieldSub(BigInt(chall.length), off)), Number(8n)));
  (r1_bytes.slice(0, Number(m))).splice(0, (chall.slice(Number(off), Number(fieldAdd(off, m)))).length, ...(chall.slice(Number(off), Number(fieldAdd(off, m)))));
  return new UniversalHashKey({ $fr0: new Galois128(u128_from_le_bytes(r0_bytes)), $fr1: new Galois64(u64_from_le_bytes(r1_bytes)) });
}

export function hash_prime(v: any): bigint[]
{
  let h = new Sha3_256();
  h.update([102n, 101n, 114n, 114n, 101n, 116n, 45n, 115n, 112n, 99n, 111n, 116n, 45n, 104n, 112n, 45n, 118n, 49n]);
  h.update(v);
  const out = [...Array.from(h.finalize(), BigInt)];
  let b = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
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
    const out = [...Array.from(h.finalize(), BigInt)];
    let xb = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
    (xb).splice(0, (out).length, ...(out));
    const sign = fieldBitand(fieldShr(xb[Number(31n)], 7n), 1n);
    xb[Number(31n)] = fieldBitand(xb[Number(31n)], 127n);
    const x = fe_from_bytes_le(xb);
    const xx = fe_sq(x);
    const num = fe_add(new Fe25519([1n, 0n, 0n, 0n]), xx);
    const den = fe_sub(new Fe25519([1n, 0n, 0n, 0n]), fe_mul(ctx.DClass, xx));
    if (den.is_zero())     {
      continue;
    }
    const yy = fe_mul(num, fe_invert(den));
    const __volar_let_else_15 = fe_sqrt(yy);
    if (!(__volar_let_else_15 !== null && __volar_let_else_15 !== undefined)) {
            {
        continue;
      }
    }
    let y = __volar_let_else_15;
    if (!__equals(fieldBitand(y.to_bytes()[Number(0n)], 1n), sign))     {
      y = fe_neg(y);
    }
    const point = new EdPoint({ $fx: x, $fy: y, $fz: new Fe25519([1n, 0n, 0n, 0n]), $ft: fe_mul(x, y) });
    const p8 = ed_mul_cofactor(point);
    if (__equals(p8, new EdPoint({ $fx: new Fe25519([0n, 0n, 0n, 0n]), $fy: new Fe25519([1n, 0n, 0n, 0n]), $fz: new Fe25519([1n, 0n, 0n, 0n]), $ft: new Fe25519([0n, 0n, 0n, 0n]) })))     {
      continue;
    }
    return p8;
  }
  return (() => { throw new Error("unreachable"); })();
}

export function iknp_cot_extend<R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, m: bigint, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): [bigint[][], bigint[][]]
{
  const [r0, v] = iknp_cot_extend_base(ctx, l, rng_s, rng_r, receiver_bits, delta_msg);
  let sender_r0 = Array.from({length: Number(m)}, () => Array.from({length: Number(l)}, () => ((0n) & 0xFFn)));
  let receiver_v = Array.from({length: Number(m)}, () => Array.from({length: Number(l)}, () => ((0n) & 0xFFn)));
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
  let seeds_0 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
  let seeds_1 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    for (let b = 0n; b < IKNP_KAPPA_BYTES; b += 1n)     {
      seeds_0[Number(i)][Number(b)] = ((fieldBitand(rng_r.next_u32(), 255n)) & 0xFFn);
      seeds_1[Number(i)][Number(b)] = ((fieldBitand(rng_r.next_u32(), 255n)) & 0xFFn);
    }
  }
  let chosen_seeds = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
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
    let v_pre = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    prg_with_index(ctx, t_bytes, BigInt(j), v_pre);
    let vj = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
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
    let t_col = Array.from({length: Number(m)}, () => false);
    prg_to_bools(ctx, seeds_0[Number(i)], t_col);
    let prg1 = Array.from({length: Number(m)}, () => false);
    prg_to_bools(ctx, seeds_1[Number(i)], prg1);
    let u_col = Array.from({length: Number(m)}, () => false);
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
    let prg_chosen = Array.from({length: Number(m)}, () => false);
    prg_to_bools(ctx, chosen_seeds[Number(i)], prg_chosen);
    let q_col = Array.from({length: Number(m)}, () => false);
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
    let r0 = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    prg_with_index(ctx, q_bytes, BigInt(j), r0);
    let q_xor_delta = q_bytes;
    for (let b = 0n; b < IKNP_KAPPA_BYTES; b += 1n)     {
      q_xor_delta[Number(b)] = fieldBitxor(q_xor_delta[Number(b)], delta_ot_bytes[Number(b)]);
    }
    let r1 = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    prg_with_index(ctx, q_xor_delta, BigInt(j), r1);
    let correction = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
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
  if ((!__equals(BigInt(left.length), BigInt(right.length)) || left.concat(right).some((value) => !__equals(value.degree(), ring.$fparameters.$fdegree))))   {
    return new Error_LengthMismatch();
  }
  let output = ring.zero();
  for (const [left, right] of left.map((__a: any, __i: number) => [__a, right[__i]] as [typeof __a, any]))   {
    output.add_assign(ring, left.product(ring, right));
  }
  return output;
}

export function interval_bounds(n: bigint, t: bigint, index: bigint): [bigint, bigint]
{
  const n_1 = (n as unknown as bigint);
  const t_1 = (t as unknown as bigint);
  const index_1 = (index as unknown as bigint);
  return [BigInt((fieldMul(index_1, n_1) / t_1)), BigInt((fieldMul(fieldAdd(index_1, 1n), n_1) / t_1))];
}

export function inverse_mod(...__args: any[]): any {
  if (__args.length === 2) {
    const value = __args[0];
    const modulus = __args[1];
    return (() => {
  let [t, new_t] = [0n, 1n];
  let [r, new_r] = [(modulus as unknown as bigint), ((value % modulus) as unknown as bigint)];
  while (!__equals(new_r, 0n))   {
    const quotient = (r / new_r);
    [t, new_t] = [new_t, fieldSub(t, fieldMul(quotient, new_t))];
    [r, new_r] = [new_r, fieldSub(r, fieldMul(quotient, new_r))];
  }
  if (!__equals(r, 1n))   {
    return undefined;
  }
  let t_1 = (t % (modulus as unknown as bigint));
  if ((t_1 < 0n))   {
    t_1 = fieldAdd(t_1, (modulus as unknown as bigint));
  }
  return BigInt(t_1);
})();
  } else
  if (__args.length === 2) {
    const value = __args[0];
    const modulus = __args[1];
    return (() => {
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
  return BigInt((((old_s) % ((modulus as unknown as bigint)) + ((modulus as unknown as bigint))) % ((modulus as unknown as bigint))));
})();
  }
  throw new Error("inverse_mod(): no matching variant for " + __args.length + " args");
}

export function is_pow2(n: bigint): boolean
{
  return (!__equals(n, 0n) && ((n) > 0n && ((n) & ((n) - 1n)) === 0n));
}

export function is_satisfied_relaxed<S>(ctx: { defaultS: () => any, UClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, w: S[], e: S[], u: any): boolean
{
  const z = full_z(w, u);
  const [az, bz, cz] = eval_abc(ctx, z);
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

export function keccak_f1600(st: bigint[])
{
  for (let round = 0n; round < 24n; round += 1n)   {
    let bc = Array.from({length: Number(5n)}, () => 0n);
    for (let i = 0n; i < 5n; i += 1n)     {
      bc[Number(i)] = fieldBitxor(fieldBitxor(fieldBitxor(fieldBitxor(st[Number(i)], st[Number(fieldAdd(i, 5n))]), st[Number(fieldAdd(i, 10n))]), st[Number(fieldAdd(i, 15n))]), st[Number(fieldAdd(i, 20n))]);
    }
    for (let i = 0n; i < 5n; i += 1n)     {
      const t = fieldBitxor(bc[Number((fieldAdd(i, 4n) % 5n))], fieldBitor(fieldShl(bc[Number((fieldAdd(i, 1n) % 5n))], 1n), fieldShr(bc[Number((fieldAdd(i, 1n) % 5n))], 63n)));
      for (let jj = 0n; jj < 5n; jj += 1n)       {
        st[Number(fieldAdd(fieldMul(jj, 5n), i))] = fieldBitxor(st[Number(fieldAdd(fieldMul(jj, 5n), i))], t);
      }
    }
    let t = st[Number(1n)];
    for (let i = 0n; i < 24n; i += 1n)     {
      const j = KECCAK_PILN[Number(i)];
      const k = KECCAK_ROTC[Number(i)];
      const tmp = st[Number(j)];
      st[Number(j)] = fieldBitor(fieldShl(t, k), fieldShr(t, fieldSub(64n, k)));
      t = tmp;
    }
    for (let jj = 0n; jj < 5n; jj += 1n)     {
      const j = fieldMul(jj, 5n);
      let row = Array.from({length: Number(5n)}, () => 0n);
      for (let i = 0n; i < 5n; i += 1n)       {
        row[Number(i)] = st[Number(fieldAdd(j, i))];
      }
      for (let i = 0n; i < 5n; i += 1n)       {
        st[Number(fieldAdd(j, i))] = fieldBitxor(st[Number(fieldAdd(j, i))], fieldBitand(!row[Number((fieldAdd(i, 1n) % 5n))], row[Number((fieldAdd(i, 2n) % 5n))]));
      }
    }
    st[Number(0n)] = fieldBitxor(st[Number(0n)], KECCAK_RNDC[Number(round)]);
  }
}

export function key_expansion(key: bigint[]): bigint[][]
{
  let words = Array.from({length: Number(fieldMul(4n, NK_ROUND_KEYS))}, () => Array.from({length: Number(4n)}, () => ((0n) & 0xFFn)));
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
        temp[Number(b)] = SBOX[Number(BigInt(temp[Number(b)]))];
      }
      temp[Number(0n)] = fieldBitxor(temp[Number(0n)], RCON[Number((i / 4n))]);
    }
    for (let b = 0n; b < 4n; b += 1n)     {
      words[Number(i)][Number(b)] = fieldBitxor(words[Number(fieldSub(i, 4n))][Number(b)], temp[Number(b)]);
    }
  }
  let round_keys = Array.from({length: Number(NK_ROUND_KEYS)}, () => Array.from({length: Number(BLOCK)}, () => ((0n) & 0xFFn)));
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
      const d = BigInt(digits[Number(j)]);
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
  let sk = Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < LAMBDA_BYTES; i += 1n)   {
    sk[Number(i)] = rng.next_u8();
  }
  const pk = encrypt_block(sk, Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn)));
  return [new FaestSecretKey(sk), new FaestPublicKey(pk)];
}

export function ks_decompose(ks_ell: bigint, ks_bg_log: bigint, x: bigint): bigint[]
{
  const bg = fieldShl(1n, ks_bg_log);
  const mask = fieldSub(bg, 1n);
  let rem = BigInt(x);
  const tail_shift = (() => { const __a = 32n; const __b = BigInt(fieldMul(ks_bg_log, ks_ell)); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
  if (((tail_shift > 0n) && (tail_shift < 32n)))   {
    const half_tail = fieldShl(1n, fieldSub(tail_shift, 1n));
    rem = wrappingAdd(rem, half_tail, 64);
  }
  let digits = Array.from({length: Number(ks_ell)}, () => 0n);
  for (const j of (Array.from({length: Number(ks_ell - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    const shift = (() => { const __a = 32n; const __b = BigInt(fieldMul(ks_bg_log, fieldAdd(j, 1n))); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
    if ((shift < 32n))     {
      digits[Number(j)] = BigInt(fieldBitand(fieldShr(rem, shift), mask));
    }
  }
  return digits;
}

export function label_lsb(w: any): boolean
{
  return (!(w.length === 0) && __equals((w[Number(0n)] % 2n), 1n));
}

export function lanes_to_bytes(st: bigint[], b: bigint[])
{
  for (let i = 0n; i < 25n; i += 1n)   {
    (b.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).splice(0, ([(st[Number(i)]) & 0xFFn, ((st[Number(i)]) >> 8n) & 0xFFn, ((st[Number(i)]) >> 16n) & 0xFFn, ((st[Number(i)]) >> 24n) & 0xFFn]).length, ...([(st[Number(i)]) & 0xFFn, ((st[Number(i)]) >> 8n) & 0xFFn, ((st[Number(i)]) >> 16n) & 0xFFn, ((st[Number(i)]) >> 24n) & 0xFFn]));
  }
}

export function le16(v: Galois128): bigint[]
{
  const b = [(v[0]) & 0xFFn, ((v[0]) >> 8n) & 0xFFn, ((v[0]) >> 16n) & 0xFFn, ((v[0]) >> 24n) & 0xFFn];
  return Array.from({length: Number(16n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => b[Number(i)]);
}

export function level_bits(log: bigint, base_log: bigint, j: bigint): bigint
{
  const remaining = (() => { const __a = log; const __b = fieldMul(base_log, BigInt(j)); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
  return (() => { if ((remaining < base_log)) {
  return remaining;
} else {
  return base_log;
} })();
}

export function level_factor(log: bigint, base_log: bigint, j: bigint): bigint
{
  return reduce(log, fieldShl(1n, level_shift(log, base_log, j)));
}

export function level_shift(log: bigint, base_log: bigint, j: bigint): bigint
{
  return (() => { const __a = log; const __b = fieldMul(base_log, fieldAdd(BigInt(j), 1n)); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
}

export function level_test_poly(big_n: bigint, log_q: bigint, level: bigint, bs_base_log: bigint, k_max: bigint): bigint[]
{
  const width = fieldShr(big_n, k_max);
  const g = level_factor(log_q, bs_base_log, level);
  let poly = Array.from({length: Number(big_n)}, () => 0n);
  for (let p = width; p < fieldMul(2n, width); p += 1n)   {
    poly[Number(p)] = g;
  }
  return poly;
}

export function lift_bit<T>(n: bigint, bit_t: any): any[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(bit_t));
}

export function limbs_to_bytes(l: bigint[]): bigint[]
{
  let out = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < 4n; i += 1n)   {
    (out.slice(Number(fieldMul(i, 8n)), Number(fieldAdd(fieldMul(i, 8n), 8n)))).splice(0, ([(l[Number(i)]) & 0xFFn, ((l[Number(i)]) >> 8n) & 0xFFn, ((l[Number(i)]) >> 16n) & 0xFFn, ((l[Number(i)]) >> 24n) & 0xFFn]).length, ...([(l[Number(i)]) & 0xFFn, ((l[Number(i)]) >> 8n) & 0xFFn, ((l[Number(i)]) >> 16n) & 0xFFn, ((l[Number(i)]) >> 24n) & 0xFFn]));
  }
  return out;
}

export function log2_pow2(n: bigint): bigint
{
  return BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, n)));
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
  return (reduce(log_m, wrappingSub(phase, (delta / 2n), 32)) < delta);
}

export function lwe_decrypt(n_lwe: bigint, ct: LweCiphertextDyn, sk: LweSecretKeyDyn): boolean
{
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(ct.$fa[Number(i)]), Number(BigInt(sk.$fkey[Number(i)])))), 32);
  }
  const phase = wrappingSub(ct.$fb, dot, 32);
  const half = fieldShr(Q4, 1n);
  const shifted = wrappingSub(phase, half, 32);
  return (shifted < Q4);
}

export function lwe_encrypt<R>(n_lwe: bigint, m: boolean, sk: LweSecretKeyDyn, noise_bits: bigint, rng: any): LweCiphertextDyn
{
  let a = Array.from({length: Number(n_lwe)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(a.length); __zip_mut_1 += 1n) {
    {
      a[Number(__zip_mut_1)] = rng.next_u32();
    }}
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(BigInt(sk.$fkey[Number(i)])))), 32);
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
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(a.length); __zip_mut_1 += 1n) {
    {
      a[Number(__zip_mut_1)] = rng.next_u32();
    }}
  let dot: bigint = 0n;
  for (let i = 0n; i < n_lwe; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(a[Number(i)]), Number(BigInt(sk.$fkey[Number(i)])))), 32);
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
  let bits = Array.from({length: Number(BigInt(v.length))}, () => ((0n) & 0xFFn));
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

export function lwe_ot_recv_decrypt_bytes_secure(n: bigint, receiver: LweOtReceiverDyn, sender_msg: any, nbytes: bigint): Vec<bigint>
{
  const [us, vs] = (() => { if (receiver.$fc) {
  return [sender_msg.$fu1.slice(0), sender_msg.$fv1.slice(0)];
} else {
  return [sender_msg.$fu0.slice(0), sender_msg.$fv0.slice(0)];
} })();
  const quarter = (LWE_Q / 4n);
  const three_quarter = fieldMul(3n, quarter);
  let bits = Array.from({length: Number(BigInt(vs.length))}, () => ((0n) & 0xFFn));
  for (let k = 0n; k < BigInt(vs.length); k += 1n)   {
    let s_dot_u: Zq = 0n;
    for (let i = 0n; i < n; i += 1n)     {
      s_dot_u = zq_add(s_dot_u, zq_mul(receiver.$fs[Number(i)], us[Number(k)][Number(i)]));
    }
    const raw = zq_sub(vs[Number(k)], s_dot_u);
    bits[Number(k)] = (() => { if (((raw > quarter) && (raw <= three_quarter))) {
  return 1n;
} else {
  return 0n;
} })();
  }
  return bits_to_bytes(bits, nbytes);
}

export function lwe_ot_recv_decrypt_secure(n: bigint, l: bigint, receiver: LweOtReceiverDyn, msg: LweOtSenderMsgSecureLoweredDyn): bigint[]
{
  const [us, vs] = (() => { if (receiver.$fc) {
  return [msg.$fu1, msg.$fv1];
} else {
  return [msg.$fu0, msg.$fv0];
} })();
  const quarter = (LWE_Q / 4n);
  const three_quarter = fieldMul(3n, quarter);
  let out = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
  for (let k = 0n; k < l; k += 1n)   {
    let s_dot_u: Zq = 0n;
    for (let i = 0n; i < n; i += 1n)     {
      s_dot_u = zq_add(s_dot_u, zq_mul(receiver.$fs[Number(i)], us[Number(k)][Number(i)]));
    }
    const raw = zq_sub(vs[Number(k)], s_dot_u);
    out[Number(k)] = (() => { if (((raw > quarter) && (raw <= three_quarter))) {
  return 1n;
} else {
  return 0n;
} })();
  }
  return out;
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

export function lwe_ot_send_bytes_secure<R>(n: bigint, rng: any, crs: LweOtCrsDyn, recv_msg: LweOtRecvMsgDyn, m0: bigint[], m1: bigint[]): LweOtSenderMsgSecureDyn
{
  const bits0 = bytes_to_bits(m0);
  const bits1 = bytes_to_bits(m1);
  const pk0 = recv_msg.$fpk0;
  let pk1 = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    pk1[Number(i)] = zq_sub(crs.$fh[Number(i)], pk0[Number(i)]);
  }
  const half_q = (LWE_Q / 2n);
  const enc = (rng: R, pk: Zq[], bits: bigint[]) => (() => {
  let u = ([] as any[]);
  let v = ([] as any[]);
  for (const bit of bits)   {
    let r = Array.from({length: Number(n)}, () => 0n);
    for (let i = 0n; i < n; i += 1n)     {
      r[Number(i)] = sample_noise(rng);
    }
    let uk = Array.from({length: Number(n)}, () => 0n);
    for (let j = 0n; j < n; j += 1n)     {
      let acc: Zq = 0n;
      for (let i = 0n; i < n; i += 1n)       {
        acc = zq_add(acc, zq_mul(crs.$fa[Number(i)][Number(j)], r[Number(i)]));
      }
      acc = zq_add(acc, sample_noise(rng));
      uk[Number(j)] = acc;
    }
    let base: Zq = 0n;
    for (let i = 0n; i < n; i += 1n)     {
      base = zq_add(base, zq_mul(pk[Number(i)], r[Number(i)]));
    }
    const plain = (() => { if (__equals(fieldBitand(bit, 1n), 1n)) {
  return half_q;
} else {
  return 0n;
} })();
    (v).push(zq_add(zq_add(base, sample_noise(rng)), plain));
    (u).push(uk);
  }
  return [u, v];
})();
  const [u0, v0] = enc(rng, pk0, bits0);
  const [u1, v1] = enc(rng, pk1, bits1);
  return new LweOtSenderMsgSecureDyn({ $fu0: u0, $fv0: v0, $fu1: u1, $fv1: v1 });
}

export function lwe_ot_send_secure<R>(n: bigint, l: bigint, rng: any, crs: LweOtCrsDyn, recv_msg: LweOtRecvMsgDyn, m0: bigint[], m1: bigint[]): LweOtSenderMsgSecureLoweredDyn
{
  const pk0 = recv_msg.$fpk0;
  let pk1 = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    pk1[Number(i)] = zq_sub(crs.$fh[Number(i)], pk0[Number(i)]);
  }
  const [u0, v0] = encrypt_branch_secure(n, l, rng, crs, pk0, m0);
  const [u1, v1] = encrypt_branch_secure(n, l, rng, crs, pk1, m1);
  return new LweOtSenderMsgSecureLoweredDyn({ $fu0: u0, $fv0: v0, $fu1: u1, $fv1: v1, $fn: 0n, $fl: 0n });
}

export function lwe_phase(n: bigint, log_m: bigint, ct: BinfheLweCiphertextDyn, sk: BinfheLweSecretKeyDyn): bigint
{
  let dot = 0n;
  for (let i = 0n; i < n; i += 1n)   {
    dot = wrappingAdd(dot, BigInt(Math.imul(Number(ct.$fa[Number(i)]), Number(BigInt(sk.$fkey[Number(i)])))), 32);
  }
  return reduce(log_m, wrappingSub(ct.$fb, dot, 32));
}

export function mask(log: bigint): bigint
{
  return (() => { if ((log >= 32n)) {
  return 4294967295n;
} else {
  return fieldSub(fieldShl(1n, log), 1n);
} })();
}

export function masked_table(and: boolean, pi_a: boolean, pi_b: boolean, pi_c: boolean): boolean[]
{
  let out = Array.from({length: Number(4n)}, () => false);
  for (let j = 0n; j < 2n; j += 1n)   {
    for (let i = 0n; i < 2n; i += 1n)     {
      const [ii, jj] = [__equals(i, 1n), __equals(j, 1n)];
      out[Number(fieldAdd(i, fieldMul(2n, j)))] = (() => { if (and) {
  return fieldBitxor(fieldBitand(fieldBitxor(pi_a, ii), fieldBitxor(pi_b, jj)), pi_c);
} else {
  return fieldBitxor(fieldBitxor(fieldBitxor(pi_a, ii), fieldBitxor(pi_b, jj)), pi_c);
} })();
    }
  }
  return out;
}

export function max_lut_arity(log_q_lwe: bigint): bigint
{
  return (() => { const __a = log_q_lwe; const __b = 2n; const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
}

export function mem_acc_absorb<T>(acc: any, r0: any, r1: any, r2: any, r3: any, addr: any, value: any, ts: any): any
{
  return fieldAdd(fieldAdd(fieldAdd(fieldAdd(acc, r0), fieldMul(addr, r1)), fieldMul(value, r2)), fieldMul(ts, r3));
}

export function mem_acc_absorb_q<T>(n: bigint, acc: QDyn<any>, one: QDyn<any>, addr: QDyn<any>, value: QDyn<any>, ts: QDyn<any>, r0: any, r1: any, r2: any, r3: any): QDyn<any>
{
  const c = q_scale_const(n, one, r0);
  const a = q_scale_const(n, addr, r1);
  const v = q_scale_const(n, value, r2);
  const t = q_scale_const(n, ts, r3);
  return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(fieldAdd(fieldAdd(fieldAdd(__clone(acc.$fq[Number(i)]), __clone(c.$fq[Number(i)])), __clone(a.$fq[Number(i)])), __clone(v.$fq[Number(i)])), __clone(t.$fq[Number(i)]));
})()), $fn: 0n });
}

export function mem_acc_absorb_vope<T>(n: bigint, acc: VopeDyn<any>, one: VopeDyn<any>, addr: VopeDyn<any>, value: VopeDyn<any>, ts: VopeDyn<any>, r0: any, r1: any, r2: any, r3: any): VopeDyn<any>
{
  return fieldAdd(fieldAdd(fieldAdd(fieldAdd(acc, vope_scale_const(n, one, r0)), vope_scale_const(n, addr, r1)), vope_scale_const(n, value, r2)), vope_scale_const(n, ts, r3));
}

export function mem_drain_check<T>(n: bigint, prod_q: QDyn<any>, cons_q: QDyn<any>, opening: any[]): boolean
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const k_diff = fieldAdd(__clone(prod_q.$fq[Number(i)]), __clone(cons_q.$fq[Number(i)]));
    ok = (ok && __equals(k_diff, __clone(opening[Number(i)])));
  }
  return ok;
}

export function mem_drain_open<T>(n: bigint, prod: VopeDyn<any>, cons: VopeDyn<any>): any[]
{
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(prod.$fv[Number(i)]), __clone(cons.$fv[Number(i)])));
}

export function memory_check_per_lane<T>(ctx: { HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, n: bigint, challenges: any[]): MemoryCheckStateDyn<any, AdditiveHasher>[]
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

export function mod_switch(from_: bigint, to: bigint, x: bigint): bigint
{
  return (() => { if ((to >= from_)) {
  return embed_up(from_, to, x);
} else {
  const shift = fieldSub(from_, to);
  const half = fieldShl(1n, fieldSub(shift, 1n));
  const rounded = BigInt(fieldShr(fieldAdd(BigInt(x), BigInt(half)), shift));
  return reduce(to, rounded);
} })();
}

export function mod_switch_lwe(n: bigint, from_: bigint, to: bigint, ct: BinfheLweCiphertextDyn): BinfheLweCiphertextDyn
{
  let a = Array.from({length: Number(n)}, () => 0n);
  for (let i = 0n; i < n; i += 1n)   {
    a[Number(i)] = mod_switch(from_, to, ct.$fa[Number(i)]);
  }
  return new BinfheLweCiphertext({ $fa: a, $fb: mod_switch(from_, to, ct.$fb) });
}

export function mpcot_reg_choice_bits(n: bigint, t: bigint, alphas: bigint[], cot_r: boolean[]): Vec<boolean>
{
  const splen = padded_interval_len(n, t);
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen)));
  let out = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const r = cot_r.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    out.push(...(spcot_choice_bits(alphas[Number(i)], h, r)));
  }
  return out;
}

export function mpcot_reg_consistency_check(delta: any, s: Block[], r: Block[], alphas: bigint[], extra_q: Block[], extra_r: boolean[], extra_t: Block[], transcript: bigint[]): boolean
{
  const t = BigInt(alphas.length);
  if (((__equals(t, 0n) || !__equals(BigInt(s.length), BigInt(r.length))) || (BigInt(s.length) < t)))   {
    return false;
  }
  const n = BigInt(s.length);
  let vs = ([] as any[]);
  let ws = ([] as any[]);
  for (const [index, alpha] of alphas.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const [start, end] = interval_bounds(n, t, index);
    if ((alpha >= fieldSub(end, start)))     {
      return false;
    }
    (vs).push(s.slice(Number(start), Number(end)));
    (ws).push(r.slice(Number(start), Number(end)));
  }
  return mpcot_reg_consistency_check_slices(delta, vs, ws, alphas, extra_q, extra_r, extra_t, transcript);
}

export function mpcot_reg_consistency_check_padded(delta: any, s: Vec<Block>[], r: Vec<Block>[], alphas: bigint[], extra_q: Block[], extra_r: boolean[], extra_t: Block[], transcript: bigint[]): boolean
{
  const __volar_let_else_0 = s[0];
  if (!(__volar_let_else_0 !== null && __volar_let_else_0 !== undefined)) {
        {
      return false;
    }
  }
  const first = __volar_let_else_0;
  if (((!((BigInt(first.length)) > 0n && ((BigInt(first.length)) & ((BigInt(first.length)) - 1n)) === 0n) || s.some((bucket) => !__equals(BigInt(bucket.length), BigInt(first.length)))) || r.some((bucket) => !__equals(BigInt(bucket.length), BigInt(first.length)))))   {
    return false;
  }
  const vs: Vec<Block[]> = s.map((value: any) => value);
  const ws: Vec<Block[]> = r.map((value: any) => value);
  return mpcot_reg_consistency_check_slices(delta, vs, ws, alphas, extra_q, extra_r, extra_t, transcript);
}

export function mpcot_reg_consistency_check_slices(delta: any, vs: Block[][], ws: Block[][], alphas: bigint[], extra_q: Block[], extra_r: boolean[], extra_t: Block[], transcript: bigint[]): boolean
{
  const t = BigInt(alphas.length);
  if ((((((__equals(t, 0n) || !__equals(BigInt(vs.length), t)) || !__equals(BigInt(ws.length), t)) || !__equals(BigInt(extra_q.length), KAPPA_BITS)) || !__equals(BigInt(extra_r.length), KAPPA_BITS)) || !__equals(BigInt(extra_t.length), KAPPA_BITS)))   {
    return false;
  }
  let lens = ([] as any[]);
  for (const [alpha, [v, w]] of alphas.map((__a: any, __i: number) => [__a, vs.map((__a: any, __i: number) => [__a, ws[__i]] as [typeof __a, any])[__i]] as [typeof __a, any]))   {
    if ((!__equals(BigInt(v.length), BigInt(w.length)) || (alpha >= BigInt(v.length))))     {
      return false;
    }
    (lens).push(BigInt(v.length));
  }
  const chis = spcot_batched_fs_chis(lens, transcript);
  const x_star_prime = spcot_batched_masked_choice(alphas, extra_r, chis);
  const hv = spcot_batched_sender_hash_v(delta, vs, extra_q, x_star_prime, chis);
  const hw = spcot_batched_receiver_hash_w(ws, extra_t, chis);
  return __equals(hv, hw);
}

export function mpcot_reg_flatten_outputs(n: bigint, t: bigint, buckets: Vec<Block>[]): Vec<Block>
{
  let output = ([] as any[]);
  for (const [index, bucket] of buckets.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const [start, end] = interval_bounds(n, t, index);
    output.push(...(bucket.slice(0, Number(fieldSub(end, start)))));
  }
  return output;
}

export function mpcot_reg_receiver(n: bigint, t: bigint, alphas: bigint[], cot_t: Block[], msg: any): Vec<Block>
{
  const splen = padded_interval_len(n, t);
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen)));
  let r = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const [start, end] = interval_bounds(n, t, i);
    const t_rows = cot_t.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const w = spcot_receiver_extend(alphas[Number(i)], splen, t_rows, msg.$fblocks[Number(i)]);
    r.push(...(w.slice(0, Number(fieldSub(end, start)))));
  }
  return r;
}

export function mpcot_reg_receiver_padded(n: bigint, t: bigint, alphas: bigint[], cot_t: Block[], msg: any): Vec<Vec<Block>>
{
  const splen = padded_interval_len(n, t);
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen)));
  let outputs = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const t_rows = cot_t.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    (outputs).push(spcot_receiver_extend(alphas[Number(i)], splen, t_rows, msg.$fblocks[Number(i)]));
  }
  return outputs;
}

export function mpcot_reg_sender<R>(rng: any, delta: any, n: bigint, t: bigint, cot_q: Block[], choices: boolean[]): [Vec<Block>, MpcotRegSenderMsg]
{
  const splen = padded_interval_len(n, t);
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen)));
  let s = ([] as any[]);
  let blocks = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const q = cot_q.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const ch = choices.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const [start, end] = interval_bounds(n, t, i);
    const [v, msg] = spcot_sender_extend(rng, delta, splen, q, ch);
    s.push(...(v.slice(0, Number(fieldSub(end, start)))));
    (blocks).push(msg);
  }
  return [s, new MpcotRegSenderMsg({ $fblocks: blocks })];
}

export function mpcot_reg_sender_padded<R>(rng: any, delta: any, n: bigint, t: bigint, cot_q: Block[], choices: boolean[]): [Vec<Vec<Block>>, MpcotRegSenderMsg]
{
  const splen = padded_interval_len(n, t);
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen)));
  let outputs = ([] as any[]);
  let blocks = ([] as any[]);
  for (let i = 0n; i < t; i += 1n)   {
    const q = cot_q.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const ch = choices.slice(Number(fieldMul(i, h)), Number(fieldMul(fieldAdd(i, 1n), h)));
    const [v, msg] = spcot_sender_extend(rng, delta, splen, q, ch);
    (outputs).push(v);
    (blocks).push(msg);
  }
  return [outputs, new MpcotRegSenderMsg({ $fblocks: blocks })];
}

export function mpcot_uni_bucket_params(params: any, hash_seed: bigint[], table: (bigint | undefined)[]): [Vec<bigint>, Vec<bigint>]
{
  const n = params.$fn;
  const m = cuckoo_table_size(params.$ft);
  const buckets = build_buckets(hash_seed, n, m);
  let alphas = ([] as any[]);
  let lens = ([] as any[]);
  for (let j = 0n; j < m; j += 1n)   {
    const need = fieldAdd(BigInt(buckets[Number(j)].length), 1n);
    const splen = next_pow2(need);
    const p = (() => { const __match = table[Number(j)]; if (true) { return BigInt(buckets[Number(j)].length); } else { const val = __match;
return (buckets[Number(j)].findIndex((y) => __equals(y, val)))!; } })();
    (alphas).push(p);
    (lens).push(splen);
  }
  return [alphas, lens];
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
    const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, splen)));
    const p = (() => { const __match = table[Number(j)]; if (true) { return BigInt(buckets[Number(j)].length); } else { const val = __match;
return (buckets[Number(j)].findIndex((y) => __equals(y, val)))!; } })();
    (out).push(spcot_choice_bits(p, h, cot_r_chunks[Number(j)]));
  }
  return out;
}

export function mpcot_uni_consistency_check(delta: any, s_bins: Vec<Block>[], r_bins: Vec<Block>[], alphas: bigint[], lens: bigint[], extra_q: Block[], extra_r: boolean[], extra_t: Block[], transcript: bigint[]): boolean
{
  const chis = spcot_batched_fs_chis(lens, transcript);
  const x_star_prime = spcot_batched_masked_choice(alphas, extra_r, chis);
  const vs: Vec<Block[]> = s_bins.map((b: any) => b);
  const ws: Vec<Block[]> = r_bins.map((b: any) => b);
  const hv = spcot_batched_sender_hash_v(delta, vs, extra_q, x_star_prime, chis);
  const hw = spcot_batched_receiver_hash_w(ws, extra_t, chis);
  return __equals(hv, hw);
}

export function mpcot_uni_receiver(params: any, table: (bigint | undefined)[], cot_t_chunks: Vec<Block>[], msg: any): [Vec<Block>, Vec<Vec<Block>>]
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
  let r = Array.from({length: Number(n)}, () => Array.from({length: Number(16n)}, () => ((0n) & 0xFFn)));
  for (let x = 0n; x < n; x += 1n)   {
    let acc = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    for (const j of unique_bins(msg.$fhash_seed, x, m))     {
      const pos = (buckets[Number(j)].findIndex((y) => __equals(y, x)))!;
      for (let b = 0n; b < 16n; b += 1n)       {
        acc[Number(b)] = fieldBitxor(acc[Number(b)], r_bins[Number(j)][Number(pos)][Number(b)]);
      }
    }
    r[Number(x)] = acc;
  }
  return [r, r_bins];
}

export function mpcot_uni_sender<R>(rng: any, delta: any, params: any, hash_seed: bigint[], cot_q_chunks: Vec<Block>[], choices_chunks: Vec<boolean>[]): [Vec<Block>, Vec<Vec<Block>>, MpcotUniSenderMsg]
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
  let s = Array.from({length: Number(n)}, () => Array.from({length: Number(16n)}, () => ((0n) & 0xFFn)));
  for (let x = 0n; x < n; x += 1n)   {
    let acc = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    for (const j of unique_bins(hash_seed, x, m))     {
      const pos = (buckets[Number(j)].findIndex((y) => __equals(y, x)))!;
      for (let b = 0n; b < 16n; b += 1n)       {
        acc[Number(b)] = fieldBitxor(acc[Number(b)], s_bins[Number(j)][Number(pos)][Number(b)]);
      }
    }
    s[Number(x)] = acc;
  }
  return [s, s_bins, new MpcotUniSenderMsg({ $fhash_seed: hash_seed, $fblocks: blocks })];
}

export function mul4(a: bigint[], b: bigint[]): bigint[]
{
  let out = Array.from({length: Number(8n)}, () => 0n);
  for (let i = 0n; i < 4n; i += 1n)   {
    let carry = 0n;
    for (let j = 0n; j < 4n; j += 1n)     {
      const idx = fieldAdd(i, j);
      const cur = fieldAdd(fieldAdd((out[Number(idx)] as unknown as bigint), fieldMul((a[Number(i)] as unknown as bigint), (b[Number(j)] as unknown as bigint))), carry);
      out[Number(idx)] = BigInt(cur);
      carry = fieldShr(cur, 64n);
    }
    let k = fieldAdd(i, 4n);
    while (((carry > 0n) && (k < 8n)))     {
      const cur = fieldAdd((out[Number(k)] as unknown as bigint), carry);
      out[Number(k)] = BigInt(cur);
      carry = fieldShr(cur, 64n);
      k = fieldAdd(k, 1n);
    }
  }
  return out;
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

export function mul_limb(a: any, b: any, q: bigint, ntt: (Ntt | undefined)): Limb
{
  return (() => { const __match = ntt; if (__match !== null && __match !== undefined) { const nt = __match;
return negacyclic_mul_ntt(a, b, nt); } else { return negacyclic_mul_coeffs(a, b, q); } })();
}

export function mul_mod(...__args: any[]): any {
  if (__args.length === 3) {
    const left = __args[0];
    const right = __args[1];
    const modulus = __args[2];
    return (() => {
  return BigInt((fieldMul((left as unknown as bigint), (right as unknown as bigint)) % (modulus as unknown as bigint)));
})();
  } else
  if (__args.length === 3) {
    const left = __args[0];
    const right = __args[1];
    const modulus = __args[2];
    return (() => {
  return BigInt((fieldMul((left as unknown as bigint), (right as unknown as bigint)) % (modulus as unknown as bigint)));
})();
  }
  throw new Error("mul_mod(): no matching variant for " + __args.length + " args");
}

export function negacyclic_mul(params: any, a: bigint[], b: bigint[]): Vec<bigint>
{
  return negacyclic_mul(params.$fdegree, params.$fmodulus, a, b);
}

export function negacyclic_mul_coeffs(a: bigint[], b: bigint[], modulus: bigint): Vec<bigint>
{
  const n = BigInt(a.length);
  let out = Array.from({length: Number(n)}, () => 0n);
  for (const [i, ai] of a.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    for (const [j, bj] of b.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      const k = fieldAdd(i, j);
      if ((k < n))       {
        out[Number(k)] = add_mod(out[Number(k)], mul_mod(ai, bj, modulus), modulus);
      } else       {
        out[Number(fieldSub(k, n))] = sub_mod(out[Number(fieldSub(k, n))], mul_mod(ai, bj, modulus), modulus);
      }
    }
  }
  return out;
}

export function negacyclic_mul_ntt(a: bigint[], b: bigint[], ntt: any): Vec<bigint>
{
  let fa = [...a];
  let fb = [...b];
  ntt.forward(fa);
  ntt.forward(fb);
  const q = ntt.$fmodulus;
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(fa.length); __zip_mut_1 += 1n) {
    const y = fb[Number(__zip_mut_1)];
    {
      fa[Number(__zip_mut_1)] = mul_mod(fa[Number(__zip_mut_1)], y, q);
    }}
  ntt.inverse(fa);
  return fa;
}

export function negacyclic_mul_rq(a: bigint[], b: bigint[], q: bigint): RqPoly
{
  const n = BigInt(a.length);
  let out = Array.from({length: Number(n)}, () => 0n);
  for (const [i, ai] of a.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    if (__equals(ai, 0n))     {
      continue;
    }
    const ai_1 = center(ai, q);
    for (const [j, bj] of b.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      if (__equals(bj, 0n))       {
        continue;
      }
      const bj_1 = center(bj, q);
      const prod = fieldMul(ai_1, bj_1);
      if ((fieldAdd(i, j) < n))       {
        out[Number(fieldAdd(i, j))] = fieldAdd(out[Number(fieldAdd(i, j))], prod);
      } else       {
        out[Number(fieldSub(fieldAdd(i, j), n))] = fieldSub(out[Number(fieldSub(fieldAdd(i, j), n))], prod);
      }
    }
  }
  return out.map((c: any) => BigInt((((c) % ((q as unknown as bigint)) + ((q as unknown as bigint))) % ((q as unknown as bigint)))));
}

export function negate_limb(a: any, q: bigint): Limb
{
  return a.map((x: any) => (() => { if (__equals(x, 0n)) {
  return 0n;
} else {
  return fieldSub(q, x);
} })());
}

export function negate_rq(a: bigint[], q: bigint): RqPoly
{
  return a.map((x: any) => (() => { if (__equals(x, 0n)) {
  return 0n;
} else {
  return fieldSub(q, x);
} })());
}

export function new_pool<R>(rng: any, params: any): [CotPoolSender, CotPoolReceiver]
{
  return new_pool_mode(rng, params, false);
}

export function new_pool_malicious<R>(rng: any, params: any): [CotPoolSender, CotPoolReceiver]
{
  return new_pool_mode(rng, params, true);
}

export function new_pool_mode<R>(rng: any, params: any, malicious: boolean): [CotPoolSender, CotPoolReceiver]
{
  const m = params.seed_cot_count(malicious);
  const [seed_s, seed_r] = sample_seed_cots(rng, m);
  return [new CotPoolSender({ $fparams: params, $fseed: seed_s, $fout: [] as any[], $fraise_n: undefined, $fmalicious: malicious, $frefill_count: 0n }), new CotPoolReceiver({ $fparams: params, $fseed: seed_r, $fout_x: [] as any[], $fout_z: [] as any[], $fmalicious: malicious })];
}

export function next_pow2(x: bigint): bigint
{
  return BigInt(Math.max(Number(((__value: bigint) => { let __power = 1n; while (__power < __value) __power <<= 1n; return __power; }) (x)), Number(2n)));
}

export function noise_limb(n: bigint, p: bigint, q: bigint, random: any): Limb
{
  let out = Array.from({length: Number(n)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(out.length); __zip_mut_1 += 1n) {
    {
      const r = fieldSub(((random.next_u32() % 3n) as unknown as bigint), 1n);
      out[Number(__zip_mut_1)] = (() => { if ((r < 0n)) {
  return fieldSub(q, (p % q));
} else {
  return (fieldMul(BigInt(r), p) % q);
} })();
    }}
  return out;
}

export function ntt_ctx(n: bigint, q: bigint): (Ntt | undefined)
{
  return Ntt.new(n, q).ok();
}

export function offline_phase(ctx: { defaultA: () => any, defaultT: () => any }, ctx: any, wire_count: bigint, prg: any, ksk_base: bigint, ksk_digits: bigint, online_depth_budget: bigint, random: any, noise: any): Result<[OfflineGarbler, OfflineEvaluator, WireStates], Error>
{
  const __volar_try_0 = offline_setup(ctx, ctx, wire_count, prg, ksk_base, ksk_digits, online_depth_budget, random, noise);
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const garbler = __volar_try_0;
  const plan = garbler.$fplan;
  const __volar_try_2 = stream_expansion(ctx, ctx, wire_count, prg, garbler, random, noise);
  if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
  const steps = __volar_try_2;
  let states = ([] as any[]);
  let wires = ([] as any[]);
  for (const step of steps)   {
    (states).push(step.$fwire_state);
    (wires).push(step.$feval);
  }
  return [garbler, new OfflineEvaluator({ $fplan: plan, $fwires: wires }), states];
}

export function offline_setup(ctx: { defaultA: () => any }, ctx: any, wire_count: bigint, prg: any, ksk_base: bigint, ksk_digits: bigint, online_depth_budget: bigint, random: any, noise: any): Result<OfflineGarbler, Error>
{
  const __volar_try_0 = (((() => { const __a = 2n; const __b = wire_count; const __result = __a * __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })()) ?? (new Error_InvalidParameters()));
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const required_bits = __volar_try_0;
  const __volar_try_1 = ExpansionPlan.plan(required_bits, prg.depth(), prg.slots(), fieldAdd(ctx.max_level(), 1n));
  if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
  const plan = __volar_try_1;
  const top = ctx.max_level();
  if ((top < fieldAdd(prg.depth(), online_depth_budget)))   {
    return new Error_InvalidParameters();
  }
  const __volar_try_4 = ctx.keygen(random);
  if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
  const secret = __volar_try_4;
  const __volar_try_5 = ctx.key_switch_keyset(ctx, secret, ksk_base, ksk_digits, random, noise);
  if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
  const ksks = __volar_try_5;
  return new OfflineGarbler({ $fsecret: secret, $fksks: ksks, $fplan: plan });
}

export function open_output(masked: boolean, pi: boolean): boolean
{
  return fieldBitxor(masked, pi);
}

export function or_proof_challenge<G>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, s: number /* G::Element */, r: number /* G::Element */, a0: number /* G::Element */, a1: number /* G::Element */): number /* G::Scalar */
{
  const g = ctx.GClass.generator();
  let h = ctx.newD();
  h.update([118n, 111n, 108n, 97n, 114n, 45n, 99n, 104n, 111n, 117n, 45n, 111n, 114n, 108n, 97n, 110n, 100n, 105n, 45n, 111n, 114n, 45n, 112n, 114n, 111n, 111n, 102n, 45n, 118n, 49n]);
  ctx.GClass.write_element(g, h);
  ctx.GClass.write_element(s, h);
  ctx.GClass.write_element(r, h);
  ctx.GClass.write_element(a0, h);
  ctx.GClass.write_element(a1, h);
  return ctx.GClass.scalar_from_hash(h);
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
  return [new BaseOtReceiverDyn({ $fx: x, $fs: s, $fc: c }), new OtReceiverMsgDyn({ $fr: r })];
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
  return [...Array.from(h.finalize(), BigInt)];
}

export function ot_recv_malicious<G, D, R>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng: any, s: number /* G::Element */, c: boolean): [BaseOtReceiverDyn<G, D>, OtReceiverMsgDyn<G>, OtConsistencyProofDyn<number /* G::Scalar */>]
{
  const [state, msg] = ot_recv(ctx, rng, s, c);
  const proof = ot_recv_prove(ctx, rng, state.$fs, msg.$fr, state.$fx, state.$fc);
  return [state, msg, proof];
}

export function ot_recv_payload<D>(kc: bigint[], ec: bigint[], mc: bigint[])
{
  for (let i = 0n; i < BigInt(ec.length); i += 1n)   {
    mc[Number(i)] = fieldBitxor(ec[Number(i)], kc[Number(i)]);
  }
}

export function ot_recv_prove<G, R>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, rng: any, s: number /* G::Element */, r: number /* G::Element */, x: number /* G::Scalar */, c: boolean): OtConsistencyProofDyn<number /* G::Scalar */>
{
  const g = ctx.GClass.generator();
  const r1_stmt = ctx.GClass.add(r, ctx.GClass.neg(s));
  const rr = ctx.GClass.random_mod_order(rng);
  const e_sim = ctx.GClass.random_mod_order(rng);
  const z_sim = ctx.GClass.random_mod_order(rng);
  const a_real = ctx.GClass.scalar_mul(g, rr);
  const r_sim_stmt = (() => { if (c) {
  return r;
} else {
  return r1_stmt;
} })();
  const a_sim = ctx.GClass.add(ctx.GClass.scalar_mul(g, z_sim), ctx.GClass.neg(ctx.GClass.scalar_mul(r_sim_stmt, e_sim)));
  const [a0, a1] = (() => { if (c) {
  return [a_sim, a_real];
} else {
  return [a_real, a_sim];
} })();
  const e = or_proof_challenge(ctx, s, r, a0, a1);
  const e_real = ctx.GClass.scalar_sub(e, e_sim);
  const z_real = ctx.GClass.scalar_add(rr, ctx.GClass.scalar_mul_scalar(e_real, x));
  const [e0, z0, e1, z1] = (() => { if (c) {
  return [e_sim, z_sim, e_real, z_real];
} else {
  return [e_real, z_real, e_sim, z_sim];
} })();
  return new OtConsistencyProofDyn({ $fe0: e0, $fe1: e1, $fz0: z0, $fz1: z1 });
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
  return [[...Array.from(h0.finalize(), BigInt)], [...Array.from(h1.finalize(), BigInt)]];
}

export function ot_send_finish_malicious<G, D>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, state: BaseOtSenderDyn<G, D>, msg: OtReceiverMsgDyn<G>, proof: OtConsistencyProofDyn<number /* G::Scalar */>): ([bigint[], bigint[]] | undefined)
{
  if (!ot_send_verify(ctx, state.$fs, msg.$fr, proof))   {
    return undefined;
  }
  return ot_send_finish(ctx, state, msg);
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
  return [new BaseOtSenderDyn({ $fy: y, $fs: __clone(s), $ft: t }), s];
}

export function ot_send_verify<G>(ctx: { newD: () => any, GClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, s: number /* G::Element */, r: number /* G::Element */, proof: OtConsistencyProofDyn<number /* G::Scalar */>): boolean
{
  const g = ctx.GClass.generator();
  const r1_stmt = ctx.GClass.add(r, ctx.GClass.neg(s));
  const a0 = ctx.GClass.add(ctx.GClass.scalar_mul(g, proof.$fz0), ctx.GClass.neg(ctx.GClass.scalar_mul(r, proof.$fe0)));
  const a1 = ctx.GClass.add(ctx.GClass.scalar_mul(g, proof.$fz1), ctx.GClass.neg(ctx.GClass.scalar_mul(r1_stmt, proof.$fe1)));
  const e = or_proof_challenge(ctx, s, r, a0, a1);
  return ctx.GClass.scalar_eq(e, ctx.GClass.scalar_add(proof.$fe0, proof.$fe1));
}

export function pack_kappa(bits: boolean[]): bigint[]
{
  let out = Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    if (bits[Number(i)])     {
      out[Number((i / 8n))] = fieldBitor(out[Number((i / 8n))], fieldShl(1n, (i % 8n)));
    }
  }
  return out;
}

export function padded_interval_len(n: bigint, t: bigint): bigint
{
  return (((__value: bigint) => { let __power = 1n; while (__power < __value) __power <<= 1n; return __power <= 18446744073709551615n ? __power : undefined; }) (((n) + (t) - 1n) / (t)))!;
}

export function parity_coeffs(p: bigint, k_max: bigint): Result<Vec<bigint>, StitchError>
{
  if ((__equals(k_max, 0n) || (k_max > BigInt(fieldSub(p, 1n)))))   {
    return new StitchError_PayloadOverflow();
  }
  let coeffs = Array.from({length: Number(fieldAdd(k_max, 1n))}, () => 0n);
  const points: Vec<bigint> = Array.from({length: Number(BigInt(k_max) + 1n - 0n)}, (_, __i) => BigInt(__i) + 0n);
  for (const [j, x_j] of points.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    if (__equals((x_j % 2n), 0n))     {
      continue;
    }
    let numerator = [1n];
    let denominator = 1n;
    for (const [k, x_k] of points.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      if (__equals(k, j))       {
        continue;
      }
      let next = Array.from({length: Number(fieldAdd(BigInt(numerator.length), 1n))}, () => 0n);
      for (const [i, c] of numerator.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))       {
        next[Number(fieldAdd(i, 1n))] = add_mod(next[Number(fieldAdd(i, 1n))], c, p);
        next[Number(i)] = sub_mod(next[Number(i)], mul_mod(c, x_k, p), p);
      }
      numerator = next;
      const diff = (() => { if ((x_j > x_k)) {
  return fieldSub(x_j, x_k);
} else {
  return fieldSub(p, fieldSub(x_k, x_j));
} })();
      denominator = mul_mod(denominator, (diff % p), p);
    }
    const __volar_try_4 = ((inverse_mod(denominator, p)) ?? (new StitchError_PayloadOverflow()));
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const inv = __volar_try_4;
    for (const [i, c] of numerator.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
      coeffs[Number(i)] = add_mod(coeffs[Number(i)], mul_mod(c, inv, p), p);
    }
  }
  return coeffs;
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

export function permute_bytes(state_bytes: bigint[])
{
  let st = Array.from({length: Number(25n)}, () => 0n);
  bytes_to_lanes(state_bytes, st);
  keccak_f1600(st);
  lanes_to_bytes(st, state_bytes);
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
  const mask = BigInt(fieldSub(bg, 1n));
  let result = Array.from({length: Number(bs_ell)}, () => Array.from({length: Number(big_n)}, () => 0n));
  for (let i = 0n; i < big_n; i += 1n)   {
    const x = p[Number(i)];
    const tail_bits = (() => { const __a = 32n; const __b = BigInt(fieldMul(bs_bg_log, bs_ell)); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
    const rounded = (() => { if (((tail_bits > 0n) && (tail_bits < 32n))) {
  return wrappingAdd(x, fieldShl(1n, fieldSub(tail_bits, 1n)), 32);
} else {
  return x;
} })();
    for (let j = 0n; j < bs_ell; j += 1n)     {
      const shift = (() => { const __a = 32n; const __b = BigInt(fieldMul(bs_bg_log, fieldAdd(j, 1n))); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
      result[Number(j)][Number(i)] = (() => { if ((shift < 32n)) {
  return fieldBitand(fieldShr(rounded, shift), mask);
} else {
  return 0n;
} })();
    }
  }
  return result;
}

export function poly_eval_at_x(blocks: Block[]): Galois128
{
  const x_elem = new Galois128(2n);
  let acc = new Galois128(0n);
  let x_pow = new Galois128(1n);
  for (const b of blocks)   {
    acc = fieldAdd(acc, field_mul(field_from_block(b), x_pow));
    x_pow = field_mul(x_pow, x_elem);
  }
  return acc;
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

export function pow_mod(...__args: any[]): any {
  if (__args.length === 3) {
    const base = __args[0];
    const exponent = __args[1];
    const modulus = __args[2];
    return (() => {
  let result = 1n;
  let base_1 = (base % modulus);
  while ((exponent > 0n))   {
    if (__equals(fieldBitand(exponent, 1n), 1n))     {
      result = mul_mod(result, base_1, modulus);
    }
    base_1 = mul_mod(base_1, base_1, modulus);
    exponent = fieldShr(exponent, 1n);
  }
  return result;
})();
  } else
  if (__args.length === 3) {
    const value = __args[0];
    const exponent = __args[1];
    const modulus = __args[2];
    return (() => {
  let result = 1n;
  while (!__equals(exponent, 0n))   {
    if (!__equals(fieldBitand(exponent, 1n), 0n))     {
      result = mul_mod(result, value, modulus);
    }
    value = mul_mod(value, value, modulus);
    exponent = fieldShr(exponent, 1n);
  }
  return result;
})();
  }
  throw new Error("pow_mod(): no matching variant for " + __args.length + " args");
}

export function prg_to_bools(ctx: { newD: () => any }, seed: bigint[], out: boolean[])
{
  let counter: bigint = 0n;
  let pos = 0n;
  while ((pos < BigInt(out.length)))   {
    let h = ctx.newD();
    h.update(seed);
    h.update([(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn]);
    const block = [...Array.from(h.finalize(), BigInt)];
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
    const block = [...Array.from(h.finalize(), BigInt)];
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
    const digits = gadget_decompose(log_q, priv_ell, priv_base_log, src.$fa[Number(i)]);
    for (const [l, d] of digits.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))     {
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
  const digits = gadget_decompose(log_q, priv_ell, priv_base_log, src.$fb);
  for (const [l, d] of digits.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
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
    out.$fa[Number(k)] = reduce(log_q, out.$fa[Number(k)]);
    out.$fb[Number(k)] = reduce(log_q, out.$fb[Number(k)]);
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
  put_u32(bytes, BigInt(BigInt(ids.length)));
  for (const id of ids)   {
    put_u32(bytes, id);
  }
}

export function put_u32(bytes: Vec<bigint>, value: bigint)
{
  bytes.push(...([(value) & 0xFFn, ((value) >> 8n) & 0xFFn, ((value) >> 16n) & 0xFFn, ((value) >> 24n) & 0xFFn]));
}

export function q_bitpack<T>(ctx: { defaultT: () => any }, bits: bigint, n: bigint, bit_values: QDyn<any>[], pow2: any[]): QDyn<any>
{
  let acc = new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT()), $fn: 0n });
  for (let index = 0n; index < bits; index += 1n)   {
    const scaled = q_scale_const(n, bit_values[Number(index)], pow2[Number(index)]);
    acc = new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldAdd(__clone(acc.$fq[Number(i)]), __clone(scaled.$fq[Number(i)]))), $fn: 0n });
  }
  return acc;
}

export function q_scale_const<T>(n: bigint, q: QDyn<any>, c: any): QDyn<any>
{
  return new QDyn({ $fq: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(q.$fq[Number(i)]), __clone(c))), $fn: 0n });
}

export function random_nonzero_delta<T, R>(n: bigint, rng: any, sample_t: any, is_zero: any): DeltaLoweredDyn<any>
{
  return new DeltaLoweredDyn({ $fdelta: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
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
  const __volar_try_0 = reader.u32();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const out = __volar_try_0;
  let __volar_match_result_1 ;
  const __volar_match_1 = reader.byte();
  if (__volar_match_1 === 0n) {
    __volar_match_result_1 = false;
  }
  if (__volar_match_1 === 1n) {
    __volar_match_result_1 = true;
  }
  {
    return new DecodeError_UnknownTag();
  }
  const value = __volar_match_result_1;
  return new PlanOp_Const(out, value);
})(); } else if (__match === 1n) { return new PlanOp_Not(reader.u32(), reader.u32()); } else if (__match === 2n) { return (() => {
  const __volar_try_0 = reader.count();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const count = __volar_try_0;
  let ids = ([] as any[]);
  for (let _ = 0n; _ < count; _ += 1n)   {
    (ids).push(reader.u32());
  }
  let inputs = Array.from({length: Number(BigInt(ids.length))}, () => 0n);
  for (const [i, id] of ids.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    inputs[Number(i)] = id;
  }
  const __volar_try_5 = reader.u32();
  if (__volar_try_5 instanceof __VolarError) return __volar_try_5;
  const table: LutId = __volar_try_5;
  const __volar_try_6 = reader.u32();
  if (__volar_try_6 instanceof __VolarError) return __volar_try_6;
  const out: WireId = __volar_try_6;
  return new PlanOp_Lut(inputs, table, out);
})(); } else if (__match === 3n) { return new PlanOp_CircuitBootstrap(reader.u32(), reader.u32()); } else if (__match === 4n) { return (() => {
  const __volar_try_0 = reader.u32();
  if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
  const sel: RgswId = __volar_try_0;
  const __volar_try_1 = reader.u32();
  if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
  const then_cell = __volar_try_1;
  const __volar_try_2 = reader.u32();
  if (__volar_try_2 instanceof __VolarError) return __volar_try_2;
  const else_cell = __volar_try_2;
  const __volar_try_3 = reader.u32();
  if (__volar_try_3 instanceof __VolarError) return __volar_try_3;
  const out = __volar_try_3;
  return new PlanOp_RgswMux(sel, then_cell, else_cell, out);
})(); } else { return new DecodeError_UnknownTag(); } })();
}

export function recompute_tree(r: bigint[], total_leaves: bigint): Vec<bigint[]>
{
  const total_nodes = fieldSub(fieldMul(2n, total_leaves), 1n);
  let tree = Array.from({length: Number(total_nodes)}, () => Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn)));
  tree[Number(0n)] = r;
  for (let node = 0n; node < fieldSub(total_leaves, 1n); node += 1n)   {
    const parent = tree[Number(node)];
    const [left, right] = AesCtrLengthDoubler.double(parent);
    tree[Number(fieldAdd(fieldMul(2n, node), 1n))] = left;
    tree[Number(fieldAdd(fieldMul(2n, node), 2n))] = right;
  }
  return tree;
}

export function recover_sibling(layer: Block[], sum: any, offset: bigint, select: boolean)
{
  const on = fieldAdd(offset, BigInt(select));
  const off = fieldAdd(offset, BigInt(!select));
  if ((on < BigInt(layer.length)))   {
    layer[Number(on)] = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
  }
  if ((off < BigInt(layer.length)))   {
    layer[Number(off)] = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
  }
  let value = sum;
  let i = BigInt(!select);
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

export function reduce512(x: bigint[]): bigint[]
{
  let r = Array.from({length: Number(4n)}, () => 0n);
  for (const bit of (Array.from({length: Number(512n - 0n)}, (_, __i) => BigInt(__i) + 0n)).slice().reverse())   {
    const xbit = fieldBitand(fieldShr(x[Number((bit / 64n))], (bit % 64n)), 1n);
    let carry = xbit;
    for (let __zip_mut_2 = 0n; __zip_mut_2 < BigInt(r.length); __zip_mut_2 += 1n) {
      {
        const new_carry = fieldShr(r[Number(__zip_mut_2)], 63n);
        r[Number(__zip_mut_2)] = fieldBitor(fieldShl(r[Number(__zip_mut_2)], 1n), carry);
        carry = new_carry;
      }}
    if (!__equals(cmp4(r, L_LIMBS), Less))     {
      r = sub4(r, L_LIMBS);
    }
  }
  return r;
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

export function refill<R>(rng: any, sender: any, receiver: any): boolean
{
  const malicious = sender.$fmalicious;
  if ((!__equals(malicious, receiver.$fmalicious) || !__equals(sender.$fparams, receiver.$fparams)))   {
    return false;
  }
  let params = sender.$fparams;
  let __volar_match_result_3 ;
  const __volar_match_3 = checked_seed_cot_count_mode(params, malicious);
  if (__volar_match_3 !== null && __volar_match_3 !== undefined) {
    const count = __volar_match_3;
    __volar_match_result_3 = count;
  }
  {
    return false;
  }
  const current_seed_count = __volar_match_result_3;
  if (((((!__equals(BigInt(sender.$fseed.$fq.length), current_seed_count) || !__equals(BigInt(receiver.$fseed.$fu.length), current_seed_count)) || !__equals(BigInt(receiver.$fseed.$fw.length), current_seed_count)) || !__equals(BigInt(sender.$fout.length), BigInt(receiver.$fout_x.length))) || !__equals(BigInt(sender.$fout.length), BigInt(receiver.$fout_z.length))))   {
    return false;
  }
  (() => { const __match = sender.$fraise_n.take(); if (__match !== null && __match !== undefined) { const n = __match;
return (() => {
  let raised = params;
  raised.$fn = n;
  let __volar_match_result_2 ;
  const __volar_match_2 = checked_seed_cot_count_mode(raised, malicious);
  if (__volar_match_2 !== null && __volar_match_2 !== undefined) {
    const count = __volar_match_2;
    __volar_match_result_2 = count;
  }
  {
    return false;
  }
  const raised_seed_count = __volar_match_result_2;
  if ((((!__equals(raised_seed_count, current_seed_count) || !__equals(raised_seed_count, BigInt(sender.$fseed.$fq.length))) || !__equals(raised_seed_count, BigInt(receiver.$fseed.$fu.length))) || !__equals(raised_seed_count, BigInt(receiver.$fseed.$fw.length))))   {
    return false;
  }
  params = raised;
  sender.$fparams = params;
  receiver.$fparams = params;
})(); } else { return (() => {
})(); } })();
  return (() => { if (malicious) {
  const __volar_let_else_0 = (() => { const __a = sender.$frefill_count; const __b = 1n; const __result = __a + __b; return __result >= 0n && __result <= 18446744073709551615n ? __result : undefined; })();
  if (!(__volar_let_else_0 !== null && __volar_let_else_0 !== undefined)) {
        {
      return false;
    }
  }
  const next_refill_count = __volar_let_else_0;
  let transcript = [...[118n, 111n, 108n, 97n, 114n, 45n, 102n, 101n, 114n, 114n, 101n, 116n, 45n, 112n, 111n, 111n, 108n, 45n, 114n, 101n, 102n, 105n, 108n, 108n, 45n, 118n, 49n]];
  transcript.push(...([(sender.$frefill_count) & 0xFFn, ((sender.$frefill_count) >> 8n) & 0xFFn, ((sender.$frefill_count) >> 16n) & 0xFFn, ((sender.$frefill_count) >> 24n) & 0xFFn]));
  const [out, check] = ferret_extend_malicious(rng, params, sender.$fseed, receiver.$fseed, transcript);
  if (!check)   {
    return false;
  }
  sender.$frefill_count = next_refill_count;
  sender.$fseed = out.$fsender_seed;
  receiver.$fseed = out.$freceiver_seed;
  sender.$fout.push(...(out.$fsender_out));
  receiver.$fout_x.push(...(out.$frecv_x));
  receiver.$fout_z.push(...(out.$frecv_z));
  return true;
} else {
  const out = ferret_extend(rng, params, sender.$fseed, receiver.$fseed);
  sender.$fseed = out.$fsender_seed;
  receiver.$fseed = out.$freceiver_seed;
  sender.$fout.push(...(out.$fsender_out));
  receiver.$fout_x.push(...(out.$frecv_x));
  receiver.$fout_z.push(...(out.$frecv_z));
  return true;
} })();
}

export function rgsw_encrypt<R>(big_n: bigint, bs_ell: bigint, bs_bg_log: bigint, m: boolean, sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RgswCiphertextDyn
{
  const msg_bit = (() => { if (m) {
  return 1n;
} else {
  return 0n;
} })();
  const rows = Array.from({length: Number(bs_ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  const shift = (() => { const __a = 32n; const __b = BigInt(fieldMul(bs_bg_log, fieldAdd(j, 1n))); const __result = __a - __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })();
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
  return new RlweCiphertextDyn({ $fa: poly_add_neg(big_n, a.$fa, b.$fa), $fb: poly_add_neg(big_n, a.$fb, b.$fb), $fbig_n: 0n });
}

export function rlwe_encrypt_poly<R>(big_n: bigint, msg_poly: bigint[], sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(big_n, a, sk.$fkey);
  for (let i = 0n; i < big_n; i += 1n)   {
    b[Number(i)] = wrappingAdd(wrappingAdd(b[Number(i)], small_noise(noise_bits, rng), 32), msg_poly[Number(i)], 32);
  }
  return new RlweCiphertextDyn({ $fa: a, $fb: b, $fbig_n: 0n });
}

export function rlwe_encrypt_scalar<R>(big_n: bigint, m: bigint, sk: RlweSecretKeyDyn, noise_bits: bigint, rng: any): RlweCiphertextDyn
{
  const a: bigint[] = Array.from({length: Number(big_n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => rng.next_u32());
  let b = poly_mul_neg(big_n, a, sk.$fkey);
  b[Number(0n)] = wrappingAdd(wrappingAdd(b[Number(0n)], small_noise(noise_bits, rng), 32), m, 32);
  return new RlweCiphertextDyn({ $fa: a, $fb: b, $fbig_n: 0n });
}

export function rlwe_rotate(big_n: bigint, ct: RlweCiphertextDyn, exp: bigint): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ $fa: poly_rotate(big_n, ct.$fa, exp), $fb: poly_rotate(big_n, ct.$fb, exp), $fbig_n: 0n });
}

export function rlwe_sub(big_n: bigint, a: RlweCiphertextDyn, b: RlweCiphertextDyn): RlweCiphertextDyn
{
  return new RlweCiphertextDyn({ $fa: poly_sub_neg(big_n, a.$fa, b.$fa), $fb: poly_sub_neg(big_n, a.$fb, b.$fb), $fbig_n: 0n });
}

export function round_limb0_to_rp(x: any, q0: bigint, p: bigint): LabelPoly
{
  return x.map((c: any) => (() => {
  const centered = center(c, q0);
  return BigInt((((centered) % ((p as unknown as bigint)) + ((p as unknown as bigint))) % ((p as unknown as bigint))));
})());
}

export function round_to_rp(x: any, q: bigint, p: bigint): RpPoly
{
  return x.map((c: any) => (() => {
  const centered = center(c, q);
  return BigInt((((centered) % ((p as unknown as bigint)) + ((p as unknown as bigint))) % ((p as unknown as bigint))));
})());
}

export function row_multipliers(z: boolean[]): bigint[]
{
  const b = (x: boolean) => BigInt(x);
  const t1 = fieldSub(b(z[Number(1n)]), b(z[Number(0n)]));
  const t2 = fieldSub(b(z[Number(2n)]), b(z[Number(0n)]));
  const t3 = fieldSub(fieldSub(fieldSub(b(z[Number(3n)]), b(z[Number(0n)])), t1), t2);
  return [t1, t2, t3];
}

export function rowct_from_lattice(tau: any, p: bigint): RowCt
{
  const red = (poly: RqPoly) => (() => {
  return poly.map((c: any) => (c % p));
})();
  return new RowCt({ $fc0: red(tau.$fu1), $fc1: red(tau.$fu2) });
}

export function sample_below<R>(rng: any, upper: bigint): bigint
{
  const upper_1 = (__u32_try_from(upper))!;
  const zone = fieldSub(4294967295n, (4294967295n % upper_1));
  while (true)   {
    const value = rng.next_u32();
    if ((value < zone))     {
      return BigInt((value % upper_1));
    }
  }
}

export function sample_block<R>(rng: any): Block
{
  let b = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
  for (const chunk of __chunks(b, Number(4n)))   {
    const x = [(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn];
    (chunk).splice(0, (x.slice(0, Number(BigInt(chunk.length)))).length, ...(x.slice(0, Number(BigInt(chunk.length)))));
  }
  return b;
}

export function sample_bytes<R>(l: bigint, rng: any): bigint[]
{
  let b = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
  for (const chunk of __chunks(b, Number(4n)))   {
    (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
  }
  return b;
}

export function sample_error<R>(log: bigint, eta: bigint, rng: any): bigint
{
  return reduce(log, BigInt(cbd(eta, rng)));
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

export function sample_noise_rq(n: bigint, p: bigint, q: bigint, random: any): RqPoly
{
  let out = Array.from({length: Number(n)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(out.length); __zip_mut_1 += 1n) {
    {
      const r = fieldSub(((random.next_u32() % 3n) as unknown as bigint), 1n);
      const pe = (() => { if ((r < 0n)) {
  return fieldSub(q, (p % q));
} else {
  return (fieldMul(BigInt(r), p) % q);
} })();
      out[Number(__zip_mut_1)] = pe;
    }}
  return out;
}

export function sample_poly(degree: bigint, p: bigint, random: any): LabelPoly
{
  let out = Array.from({length: Number(degree)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(out.length); __zip_mut_1 += 1n) {
    {
      out[Number(__zip_mut_1)] = (BigInt(random.next_u32()) % p);
    }}
  return out;
}

export function sample_regular_noise<R>(rng: any, n: bigint, t: bigint): Vec<bigint>
{
  return Array.from({length: Number(t - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  const [start, end] = interval_bounds(n, t, i);
  return sample_below(rng, fieldSub(end, start));
})());
}

export function sample_seed<R>(rng: any): bigint[]
{
  let s = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  for (const chunk of __chunks(s, Number(4n)))   {
    (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
  }
  return s;
}

export function sample_seed_cots<R>(rng: any, m: bigint): [FerretSenderSeed, FerretReceiverSeed]
{
  let delta = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  for (const chunk of __chunks(delta, Number(4n)))   {
    (chunk).splice(0, ([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))).length, ...([(rng.next_u32()) & 0xFFn, ((rng.next_u32()) >> 8n) & 0xFFn, ((rng.next_u32()) >> 16n) & 0xFFn, ((rng.next_u32()) >> 24n) & 0xFFn].slice(0, Number(BigInt(chunk.length)))));
  }
  let q = ([] as any[]);
  let u = ([] as any[]);
  let w = ([] as any[]);
  for (let _ = 0n; _ < m; _ += 1n)   {
    let row = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
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

export function sample_uniform(...__args: any[]): any {
  if (__args.length === 3) {
    const random = __args[0];
    const modulus = __args[1];
    const output = __args[2];
    return (() => {
  const threshold = fieldSub(18446744073709551615n, (18446744073709551615n % modulus));
  for (let __mut_1 = 0n; __mut_1 < BigInt(output.length); __mut_1 += 1n) {
    {
      while (true)       {
        let bytes = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
        const __volar_try_1 = random(bytes);
        if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
        const candidate = u64_from_le_bytes(bytes);
        if ((candidate < threshold))         {
          output[Number(__mut_1)] = (candidate % modulus);
          break;
        }
      }
    }}
  return undefined;
})();
  } else
  if (__args.length === 3) {
    const random = __args[0];
    const modulus = __args[1];
    const output = __args[2];
    return (() => {
  const threshold = fieldSub(18446744073709551615n, (18446744073709551615n % modulus));
  for (let __mut_1 = 0n; __mut_1 < BigInt(output.length); __mut_1 += 1n) {
    {
      while (true)       {
        let bytes = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
        const __volar_try_1 = random.fill_bytes(bytes);
        if (__volar_try_1 instanceof __VolarError) return __volar_try_1;
        const candidate = u64_from_le_bytes(bytes);
        if ((candidate < threshold))         {
          output[Number(__mut_1)] = (candidate % modulus);
          break;
        }
      }
    }}
  return undefined;
})();
  }
  throw new Error("sample_uniform(): no matching variant for " + __args.length + " args");
}

export function sample_uniform_points<R>(rng: any, n: bigint, t: bigint): Vec<bigint>
{
  let pts = ([] as any[]);
  while ((BigInt(pts.length) < t))   {
    const x = (BigInt(rng.next_u32()) % n);
    if (!pts.includes(x))     {
      (pts).push(x);
    }
  }
  pts.sort((__a: any, __b: any) => (__a < __b ? -1 : (__a > __b ? 1 : 0)));
  return pts;
}

export function sample_uniform_rq(n: bigint, q: bigint, random: any): RqPoly
{
  let out = Array.from({length: Number(n)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(out.length); __zip_mut_1 += 1n) {
    {
      out[Number(__zip_mut_1)] = (BigInt(random.next_u32()) % q);
    }}
  return out;
}

export function sample_zq<R>(rng: any): Zq
{
  return fieldBitand(rng.next_u32(), LWE_Q_MASK);
}

export function scale_limb(delta_limb: any, t: bigint, q: bigint): Limb
{
  return delta_limb.map((c: any) => (() => {
  const c_1 = (c as unknown as bigint);
  return BigInt((((fieldMul(c_1, (t as unknown as bigint))) % ((q as unknown as bigint)) + ((q as unknown as bigint))) % ((q as unknown as bigint))));
})());
}

export function scale_row(row: any, k: bigint, p: bigint): RowCt
{
  const scale = (poly: LabelPoly, k: bigint) => (() => {
  return poly.map((c: any) => mul_mod(c, k, p));
})();
  const neg = (poly: LabelPoly) => (() => {
  return poly.map((c: any) => (() => { if (__equals(c, 0n)) {
  return 0n;
} else {
  return fieldSub(p, c);
} })());
})();
  return (() => { if ((k >= 0n)) {
  return new RowCt({ $fc0: scale(row.$fc0, BigInt(k)), $fc1: scale(row.$fc1, BigInt(k)) });
} else {
  return new RowCt({ $fc0: neg(scale(row.$fc0, BigInt(-k))), $fc1: neg(scale(row.$fc1, BigInt(-k))) });
} })();
}

export function selector_margin(log_q_lwe: bigint, k: bigint, input_noise_bound: bigint): boolean
{
  if ((fieldAdd(BigInt(k), 2n) >= log_q_lwe))   {
    return false;
  }
  const margin = fieldShl(1n, fieldSub(fieldSub(log_q_lwe, BigInt(k)), 2n));
  const weight = BigInt(fieldSub(fieldShl(1n, k), 1n));
  return ((() => { const __a = weight; const __b = input_noise_bound; const __result = __a * __b; return __result < 0n ? 0n : (__result > 4294967295n ? 4294967295n : __result); })() < margin);
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

export function sign(ctx: { newD: () => any, defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, sk: any, pk: any, message: bigint[], iv_seed: bigint[], prover: any): FaestSignature
{
  const iv: bigint[] = encrypt_block(iv_seed, Array.from({length: Number(LAMBDA_BYTES)}, () => ((0n) & 0xFFn)));
  const r: bigint[] = encrypt_block(sk[0], iv);
  const commitment: BavcCommitmentDyn = BavcDyn.commit(ctx, 32n, r, iv, TAU, SUB_VOLE_N);
  const mu: Vec<bigint> = (() => {
  let h = new Sha3_256();
  h.update(pk[0]);
  h.update(message);
  return Array.from(h.finalize(), BigInt);
})();
  const chall_1 = chall1(ctx, mu, iv, commitment.$froot, fieldAdd(LAMBDA_BYTES, 8n), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const nodes = BavcDyn.collect_open_nodes(32n, deltas, recompute_tree(r, fieldMul(TAU, SUB_VOLE_N)), TAU, SUB_VOLE_N);
  let hidden_commits: Vec<bigint[]> = ([] as any[]);
  for (let i = 0n; i < BigInt(deltas.length); i += 1n)   {
    const d = deltas[Number(i)];
    (hidden_commits).push(commitment.$fcommitments[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), d))]);
  }
  const opening = new BavcOpeningDyn({ $fhidden_commits: __clone(hidden_commits), $fnodes: __clone(nodes), $fcom_bytes: 0n });
  const _ = opening;
  let sub_voles: Vec<ConvertOutput> = ([] as any[]);
  for (let i = 0n; i < TAU; i += 1n)   {
    let seeds_i: Vec<(bigint[] | undefined)> = ([] as any[]);
    for (let j = 0n; j < SUB_VOLE_N; j += 1n)     {
      (seeds_i).push(commitment.$fseeds[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), j))]);
    }
    (sub_voles).push(convert_to_vole(seeds_i, iv, BigInt(i), L_HAT_BYTES));
  }
  const big_vole: BigVoleProver = concat_small_voles(sub_voles);
  let corrections_flat: Vec<bigint> = [] as any[];
  for (let i = 0n; i < BigInt(big_vole.$fc.length); i += 1n)   {
    for (let j = 0n; j < BigInt(big_vole.$fc[Number(i)].length); j += 1n)     {
      (corrections_flat).push(big_vole.$fc[Number(i)][Number(j)]);
    }
  }
  const chall_2 = chall2(ctx, chall_1, big_vole.$fu, corrections_flat, fieldAdd(LAMBDA_BYTES, 8n), false);
  const hash_key = hash_key_from_chall(chall_2);
  const qs_proof = prover.prove_aes_witness(big_vole, hash_key);
  const [chall_3, counter] = (grind_chall3(ctx, chall_2, qs_proof.$fa_hat, qs_proof.$fb_hat, qs_proof.$fc_hat_base, LAMBDA_BYTES, W_GRIND, false, 1000000n))!;
  let c_hat_with_counter = __clone(qs_proof.$fc_hat_base);
  const counter_bytes = [(counter) & 0xFFn, ((counter) >> 8n) & 0xFFn, ((counter) >> 16n) & 0xFFn, ((counter) >> 24n) & 0xFFn];
  for (let i = 0n; i < BigInt(counter_bytes.length); i += 1n)   {
    (c_hat_with_counter).push(counter_bytes[Number(i)]);
  }
  return new FaestSignature({ $fiv: iv, $fbavc_root: __clone(commitment.$froot), $fhidden_commits: hidden_commits, $fnodes: nodes, $fcorrections: __clone(big_vole.$fc), $fvole_u: __clone(big_vole.$fu), $fqs_proof: qs_proof, $fc_hat_with_counter: c_hat_with_counter, $fchall_3: chall_3, $fcounter: counter });
}

export function signed_to_mod(value: bigint, modulus: bigint): bigint
{
  return (() => { if ((value < 0n)) {
  const magnitude = (((value) < 0n ? -(value) : (value)) % modulus);
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
  const sender_tag = [...Array.from(hs.finalize(), BigInt)];
  let hr = ctx.newD();
  hr.update(TAG_DOMAIN);
  hr.update(delta_msg);
  for (let j = 0n; j < m; j += 1n)   {
    let r0_reconstructed = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    if (receiver_bits[Number(j)])     {
      for (let b = 0n; b < l; b += 1n)       {
        r0_reconstructed[Number(b)] = fieldBitxor(receiver_v[Number(j)][Number(b)], delta_msg[Number(b)]);
      }
    } else     {
      r0_reconstructed = receiver_v[Number(j)];
    }
    hr.update(r0_reconstructed);
  }
  const receiver_tag = [...Array.from(hr.finalize(), BigInt)];
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
  const sender_tag = [...Array.from(hs.finalize(), BigInt)];
  let hr = ctx.newD();
  hr.update(TAG_DOMAIN);
  hr.update(delta_msg);
  for (let j = 0n; j < BigInt(receiver_bits.length); j += 1n)   {
    let r0_reconstructed = Array.from({length: Number(l)}, () => ((0n) & 0xFFn));
    if (receiver_bits[Number(j)])     {
      for (let b = 0n; b < l; b += 1n)       {
        r0_reconstructed[Number(b)] = fieldBitxor(receiver_v[Number(j)][Number(b)], delta_msg[Number(b)]);
      }
    } else     {
      r0_reconstructed = receiver_v[Number(j)];
    }
    hr.update(r0_reconstructed);
  }
  const receiver_tag = [...Array.from(hr.finalize(), BigInt)];
  return new SoftSpokenOutDynDyn({ $fsender_r0: sender_r0, $freceiver_v: receiver_v, $fsender_tag: sender_tag, $freceiver_tag: receiver_tag, $fl: 0n });
}

export function softspoken_cot_extend_dyn<D, R>(ctx: { newD: () => any, BClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, l: bigint, rng_s: any, rng_r: any, receiver_bits: boolean[], delta_msg: bigint[]): SoftSpokenOutDynDyn<D>
{
  return softspoken_cot_extend_base(ctx, l, rng_s, rng_r, receiver_bits, delta_msg);
}

export function spcot_batched_fs_chis(lens: bigint[], transcript: bigint[]): Vec<Vec<Block>>
{
  return lens.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]).map(([l, n]: any) => (() => {
  let t = [...transcript];
  t.push(...([(BigInt(l)) & 0xFFn, ((BigInt(l)) >> 8n) & 0xFFn, ((BigInt(l)) >> 16n) & 0xFFn, ((BigInt(l)) >> 24n) & 0xFFn]));
  return spcot_fs_chis(n, t);
})());
}

export function spcot_batched_masked_choice(alphas: bigint[], extra_r: boolean[], chis: Vec<Block>[]): Vec<boolean>
{
  let phi = new Galois128(0n);
  for (const [l, a] of alphas.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    phi = fieldAdd(phi, field_from_block(chis[Number(l)][Number(a)]));
  }
  const phi_bits = phi[0];
  return Array.from({length: Number(KAPPA_BITS - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(extra_r[Number(i)], __equals(fieldBitand(fieldShr(phi_bits, i), 1n), 1n)));
}

export function spcot_batched_receiver_hash_w(ws: Block[][], extra_t: Block[], chis: Vec<Block>[]): bigint[]
{
  let acc = poly_eval_at_x(extra_t);
  for (const [l, w] of ws.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    for (let i = 0n; i < BigInt(w.length); i += 1n)     {
      acc = fieldAdd(acc, field_mul(field_from_block(chis[Number(l)][Number(i)]), field_from_block(w[Number(i)])));
    }
  }
  return hash_prime(block_from_field(acc));
}

export function spcot_batched_sender_hash_v(delta: any, vs: Block[][], extra_q: Block[], x_star_prime: boolean[], chis: Vec<Block>[]): bigint[]
{
  const yp: Vec<Block> = Array.from({length: Number(KAPPA_BITS - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return (() => { if (x_star_prime[Number(i)]) {
  return xor_block(extra_q[Number(i)], delta);
} else {
  return extra_q[Number(i)];
} })();
})());
  let acc = poly_eval_at_x(yp);
  for (const [l, v] of vs.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    for (let i = 0n; i < BigInt(v.length); i += 1n)     {
      acc = fieldAdd(acc, field_mul(field_from_block(chis[Number(l)][Number(i)]), field_from_block(v[Number(i)])));
    }
  }
  return hash_prime(block_from_field(acc));
}

export function spcot_choice_bits(alpha: bigint, h: bigint, cot_r: boolean[]): Vec<boolean>
{
  return Array.from({length: Number(h - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(fieldBitxor(cot_r[Number(i)], bit_msb(alpha, h, i)), true));
}

export function spcot_consistency_check(delta: any, v: Block[], w: Block[], extra_q: Block[], extra_r: boolean[], extra_t: Block[], transcript: bigint[]): boolean
{
  const n = BigInt(v.length);
  const chis = spcot_fs_chis(n, transcript);
  let alpha = undefined;
  for (let i = 0n; i < n; i += 1n)   {
    if (!__equals(v[Number(i)], w[Number(i)]))     {
      if ((alpha) != null)       {
        return false;
      }
      alpha = i;
    }
  }
  const __volar_let_else_4 = alpha;
  if (!(__volar_let_else_4 !== null && __volar_let_else_4 !== undefined)) {
        {
      return false;
    }
  }
  const a = __volar_let_else_4;
  if (!__equals(xor_block(v[Number(a)], w[Number(a)]), delta))   {
    return false;
  }
  const x_star_prime = spcot_masked_choice(a, extra_r, chis);
  const hv = spcot_sender_hash_v(delta, v, extra_q, x_star_prime, chis);
  const hw = spcot_receiver_hash_w(w, extra_t, chis);
  return __equals(hv, hw);
}

export function spcot_fs_chis(n: bigint, transcript: bigint[]): Vec<Block>
{
  let h = new Sha3_256();
  h.update([102n, 101n, 114n, 114n, 101n, 116n, 45n, 115n, 112n, 99n, 111n, 116n, 45n, 102n, 115n, 45n, 118n, 49n]);
  h.update(transcript);
  const seed = [...Array.from(h.finalize(), BigInt)];
  let chi = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
  (chi).splice(0, (seed.slice(0, Number(KAPPA_BYTES))).length, ...(seed.slice(0, Number(KAPPA_BYTES))));
  let chi_pow = field_from_block(chi);
  let chis = ([] as any[]);
  for (let _ = 0n; _ < n; _ += 1n)   {
    (chis).push(block_from_field(chi_pow));
    chi_pow = field_mul(chi_pow, field_from_block(chi));
  }
  return chis;
}

export function spcot_in_process<R>(rng: any, delta: any, n: bigint, alpha: bigint): [Vec<Block>, Vec<Block>]
{
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, n)));
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

export function spcot_masked_choice(alpha: bigint, extra_r: boolean[], chis: Block[]): Vec<boolean>
{
  const phi_bits = u128_from_le_bytes(chis[Number(alpha)]);
  return Array.from({length: Number(KAPPA_BITS - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldBitxor(extra_r[Number(i)], __equals(fieldBitand(fieldShr(phi_bits, i), 1n), 1n)));
}

export function spcot_receiver_extend(alpha: bigint, n: bigint, cot_t: Block[], msg: any): Vec<Block>
{
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, n)));
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
  for (const [i, wi] of w.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    if (!__equals(i, alpha))     {
      acc = xor_block(acc, wi);
    }
  }
  w[Number(alpha)] = acc;
  return w;
}

export function spcot_receiver_hash_w(w: Block[], extra_t: Block[], chis: Block[]): bigint[]
{
  const z = poly_eval_at_x(extra_t);
  let ip = new Galois128(0n);
  for (let i = 0n; i < BigInt(w.length); i += 1n)   {
    ip = fieldAdd(ip, field_mul(field_from_block(chis[Number(i)]), field_from_block(w[Number(i)])));
  }
  return hash_prime(block_from_field(fieldAdd(ip, z)));
}

export function spcot_sender_extend<R>(rng: any, delta: any, n: bigint, cot_q: Block[], choices: boolean[]): [Vec<Block>, SpcotSenderMsg]
{
  const h = BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, n)));
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

export function spcot_sender_hash_v(delta: any, v: Block[], extra_q: Block[], x_star_prime: boolean[], chis: Block[]): bigint[]
{
  const yp: Vec<Block> = Array.from({length: Number(KAPPA_BITS - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return (() => { if (x_star_prime[Number(i)]) {
  return xor_block(extra_q[Number(i)], delta);
} else {
  return extra_q[Number(i)];
} })();
})());
  const y = poly_eval_at_x(yp);
  let ip = new Galois128(0n);
  for (let i = 0n; i < BigInt(v.length); i += 1n)   {
    ip = fieldAdd(ip, field_mul(field_from_block(chis[Number(i)]), field_from_block(v[Number(i)])));
  }
  return hash_prime(block_from_field(fieldAdd(ip, y)));
}

export function spec_sha3_256(input: bigint[]): bigint[]
{
  let sponge = spec_sponge_new(SHA3_256_RATE);
  spec_sponge_absorb(sponge, input);
  let reader = spec_sponge_finalize_xof(sponge, 6n);
  let out = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  spec_xof_read(reader, out);
  return out;
}

export function spec_shake128(n: bigint, input: bigint[]): bigint[]
{
  let sponge = spec_sponge_new(SHAKE128_RATE);
  spec_sponge_absorb(sponge, input);
  let reader = spec_sponge_finalize_xof(sponge, 31n);
  let out = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read(reader, out);
  return out;
}

export function spec_sponge_absorb(sponge: any, data: bigint[])
{
  let state = sponge.$fstate;
  let pos = sponge.$fpos;
  const rate = sponge.$frate;
  for (let i = 0n; i < BigInt(data.length); i += 1n)   {
    state[Number(pos)] = fieldBitxor(state[Number(pos)], data[Number(i)]);
    pos = fieldAdd(pos, 1n);
    if (__equals(pos, rate))     {
      permute_bytes(state);
      pos = 0n;
    }
  }
  sponge.$fstate = state;
  sponge.$fpos = pos;
}

export function spec_sponge_absorb_array(n: bigint, sponge: any, data: bigint[])
{
  let state = sponge.$fstate;
  let pos = sponge.$fpos;
  const rate = sponge.$frate;
  for (let i = 0n; i < n; i += 1n)   {
    state[Number(pos)] = fieldBitxor(state[Number(pos)], data[Number(i)]);
    pos = fieldAdd(pos, 1n);
    if (__equals(pos, rate))     {
      permute_bytes(state);
      pos = 0n;
    }
  }
  sponge.$fstate = state;
  sponge.$fpos = pos;
}

export function spec_sponge_finalize_xof(sponge: any, suffix: bigint): SpecXofReader
{
  let state = sponge.$fstate;
  state[Number(sponge.$fpos)] = fieldBitxor(state[Number(sponge.$fpos)], suffix);
  state[Number(fieldSub(sponge.$frate, 1n))] = fieldBitxor(state[Number(fieldSub(sponge.$frate, 1n))], 128n);
  permute_bytes(state);
  return new SpecXofReader({ $fstate: state, $frate: sponge.$frate, $fpos: 0n });
}

export function spec_sponge_new(rate: bigint): SpecSponge
{
  return new SpecSponge({ $fstate: Array.from({length: Number(200n)}, () => ((0n) & 0xFFn)), $frate: rate, $fpos: 0n });
}

export function spec_xof_read(reader: any, out: bigint[])
{
  let state = reader.$fstate;
  let pos = reader.$fpos;
  const rate = reader.$frate;
  for (let i = 0n; i < BigInt(out.length); i += 1n)   {
    if (__equals(pos, rate))     {
      permute_bytes(state);
      pos = 0n;
    }
    out[Number(i)] = state[Number(pos)];
    pos = fieldAdd(pos, 1n);
  }
  reader.$fstate = state;
  reader.$fpos = pos;
}

export function spec_xof_read_array(n: bigint, reader: any, out: bigint[])
{
  let state = reader.$fstate;
  let pos = reader.$fpos;
  const rate = reader.$frate;
  for (let i = 0n; i < n; i += 1n)   {
    if (__equals(pos, rate))     {
      permute_bytes(state);
      pos = 0n;
    }
    out[Number(i)] = state[Number(pos)];
    pos = fieldAdd(pos, 1n);
  }
  reader.$fstate = state;
  reader.$fpos = pos;
}

export function split_cot_chunks<T>(flat: any[], heights: bigint[]): Vec<Vec<any>>
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
    let r0r = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    if (bits[Number(j)])     {
      for (let b = 0n; b < 16n; b += 1n)       {
        r0r[Number(b)] = fieldBitxor(v[Number(j)][Number(b)], delta_msg[Number(b)]);
      }
    } else     {
      r0r = v[Number(j)];
    }
    hr.update(r0r);
  }
  return [...Array.from(hr.finalize(), BigInt)];
}

export function ssp_sender_tag(delta_msg: bigint[], r0: bigint[][]): bigint[]
{
  let hs = new Sha3_256();
  hs.update(TAG_DOMAIN);
  hs.update(delta_msg);
  for (const row of r0)   {
    hs.update(row);
  }
  return [...Array.from(hs.finalize(), BigInt)];
}

export function stack_bea95_receiver<R, Io>(rng: any, receiver: any, io: any, bit: boolean): bigint[]
{
  ensure_receiver(rng, receiver, io, 1n);
  const x = (receiver.$fout_x.pop_front())!;
  const z = (receiver.$fout_z.pop_front())!;
  const [_r0, zc, d] = bea95_chosen_bit(Array.from({length: Number(16n)}, () => ((0n) & 0xFFn)), Array.from({length: Number(16n)}, () => ((0n) & 0xFFn)), x, z, bit);
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
  const rec = encode_receiver_only(receiver, prep, r, receiver.$fparams.seed_cot_count(false));
  receiver.$fseed = rec.$fseed;
  receiver.$fout_x.push(...(rec.$fx));
  receiver.$fout_z.push(...(rec.$fz));
}

export function stack_refill_receiver_malicious<R, Io>(rng: any, receiver: any, io: any): boolean
{
  const params = receiver.$fparams;
  const k = params.$fk;
  const n = params.$fn;
  const t = params.$ft;
  const extra_off = fieldAdd(k, fieldMul(t, params.log_splen()));
  const alphas = sample_regular_noise(rng, n, t);
  let e = Array.from({length: Number(n)}, () => false);
  for (const [i, a] of alphas.map((val: any, i: number) => [BigInt(i), val] as [bigint, typeof val]))   {
    const [start, end] = (params.interval_bounds(i))!;
    e[Number(fieldAdd(start, a))] = true;
  }
  const lpn_seed = sample_seed(rng);
  const choices = mpcot_reg_choice_bits(n, t, alphas, receiver.$fseed.$fu.slice(Number(k), Number(extra_off)));
  const open_bytes = encode_ferret_open(lpn_seed, choices);
  io.send(TAG_FERRET_OPEN, open_bytes);
  const mpcot_bytes = io.recv(TAG_FERRET_MPCOT);
  const mpcot = decode_mpcot_reg(mpcot_bytes);
  const r_padded = mpcot_reg_receiver_padded(n, t, alphas, receiver.$fseed.$fw.slice(Number(k), Number(extra_off)), mpcot);
  let transcript = __clone(open_bytes);
  transcript.push(...(mpcot_bytes));
  const lens = Array.from({length: Number(t)}, () => params.splen());
  const chis = spcot_batched_fs_chis(lens, transcript);
  const extra_r = receiver.$fseed.$fu.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const extra_t = receiver.$fseed.$fw.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const x_star_prime = spcot_batched_masked_choice(alphas, extra_r, chis);
  io.send(TAG_FERRET_CHECK_MASK, encode_bools(x_star_prime));
  const hv = io.recv(TAG_FERRET_CHECK_HV);
  const ws: Vec<Block[]> = r_padded.map((value: any) => value);
  const hw = spcot_batched_receiver_hash_w(ws, extra_t, chis);
  let hv_arr = Array.from({length: Number(32n)}, () => ((0n) & 0xFFn));
  (hv_arr).splice(0, (hv).length, ...(hv));
  const check = __equals(hv_arr, hw);
  if (!check)   {
    return false;
  }
  const prep = new FerretPrep({ $falphas: alphas, $fe: e, $flpn_seed: lpn_seed, $fchoices: choices });
  const m = params.seed_cot_count(true);
  const r = mpcot_reg_flatten_outputs(n, t, r_padded);
  const rec = encode_receiver_only(receiver, prep, r, m);
  receiver.$fseed = rec.$fseed;
  receiver.$fout_x.push(...(rec.$fx));
  receiver.$fout_z.push(...(rec.$fz));
  return true;
}

export function stack_refill_sender<R, Io>(rng: any, sender: any, io: any)
{
  const raw = io.recv(TAG_FERRET_OPEN);
  const [lpn_seed, choices] = decode_ferret_open(raw);
  const [s, mpcot] = ferret_sender_mpcot(rng, sender.$fparams, sender.$fseed, choices);
  io.send(TAG_FERRET_MPCOT, encode_mpcot_reg(mpcot));
  const out_full = encode_sender_only(sender, lpn_seed, s, sender.$fparams.seed_cot_count(false));
  sender.$fseed.$fq = out_full.$fseed_q;
  sender.$fout.push(...(out_full.$femit));
}

export function stack_refill_sender_malicious<R, Io>(rng: any, sender: any, io: any)
{
  const params = sender.$fparams;
  const k = params.$fk;
  const t = params.$ft;
  const extra_off = fieldAdd(k, fieldMul(t, params.log_splen()));
  const raw = io.recv(TAG_FERRET_OPEN);
  const [lpn_seed, choices] = decode_ferret_open(raw);
  const [s_padded, mpcot] = mpcot_reg_sender_padded(rng, sender.$fseed.$fdelta, params.$fn, t, sender.$fseed.$fq.slice(Number(k), Number(extra_off)), choices);
  const mpcot_bytes = encode_mpcot_reg(mpcot);
  io.send(TAG_FERRET_MPCOT, mpcot_bytes);
  const x_star_prime = decode_bools(io.recv(TAG_FERRET_CHECK_MASK))[0];
  let transcript = __clone(raw);
  transcript.push(...(mpcot_bytes));
  const lens = Array.from({length: Number(t)}, () => params.splen());
  const chis = spcot_batched_fs_chis(lens, transcript);
  const vs: Vec<Block[]> = s_padded.map((value: any) => value);
  const extra_q = sender.$fseed.$fq.slice(Number(extra_off), Number(fieldAdd(extra_off, KAPPA_BITS)));
  const hv = spcot_batched_sender_hash_v(sender.$fseed.$fdelta, vs, extra_q, x_star_prime, chis);
  io.send(TAG_FERRET_CHECK_HV, hv);
  const s = mpcot_reg_flatten_outputs(params.$fn, t, s_padded);
  const m = params.seed_cot_count(true);
  const out_full = encode_sender_only(sender, lpn_seed, s, m);
  sender.$fseed.$fq = out_full.$fseed_q;
  sender.$fout.push(...(out_full.$femit));
  sender.$frefill_count = fieldAdd(sender.$frefill_count, 1n);
}

export function stack_setup_receiver<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any): CotPoolReceiver
{
  return stack_setup_receiver_m(ctx, rng, params, io, false);
}

export function stack_setup_receiver_m<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any, malicious: boolean): CotPoolReceiver
{
  const m = params.seed_cot_count(malicious);
  const delta_raw = io.recv(TAG_DELTA);
  let delta_msg = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  (delta_msg).splice(0, (delta_raw).length, ...(delta_raw));
  let bits = Array.from({length: Number(m)}, () => false);
  for (let __mut_1 = 0n; __mut_1 < BigInt(bits.length); __mut_1 += 1n) {
  {
    bits[Number(__mut_1)] = __equals(fieldBitand(rng.next_u32(), 1n), 1n);
  }}
  let seeds_0 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
  let seeds_1 = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    seeds_0[Number(i)] = sample_bytes(rng);
    seeds_1[Number(i)] = sample_bytes(rng);
  }
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    const [s_state, setup] = sender_setup(rng);
    io.send(TAG_LWE_SETUP, encode_lwe_crs(setup));
    const recv_msg = decode_lwe_recv(16n, io.recv(TAG_LWE_RECV));
    const payload = sender_payload(rng, s_state, recv_msg, seeds_0[Number(i)], seeds_1[Number(i)]);
    io.send(TAG_LWE_PAYLOAD, encode_lwe_payload(payload));
  }
  const [t_cols, u_msg] = iknp_receiver_u_cols(ctx, m, bits, seeds_0, seeds_1);
  io.send(TAG_IKNP_U, encode_iknp_u(u_msg));
  const corrections = decode_iknp_corr(io.recv(TAG_IKNP_CORR));
  const receiver_v = iknp_receiver_finish(ctx, 16n, bits, t_cols, corrections);
  const tag_s = io.recv(TAG_SSP_S);
  const tag_r = ssp_receiver_tag(delta_msg, bits, receiver_v);
  io.send(TAG_SSP_R, tag_r);
  let w = ([] as any[]);
  for (const row of receiver_v)   {
    (w).push(row);
  }
  let receiver = new CotPoolReceiver({ $fparams: params, $fseed: new FerretReceiverSeed({ $fu: bits, $fw: w }), $fout_x: [] as any[], $fout_z: [] as any[], $fmalicious: malicious });
  if (malicious)   {
  } else   {
    stack_refill_receiver(rng, receiver, io);
  }
  return receiver;
}

export function stack_setup_receiver_malicious<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any): CotPoolReceiver
{
  return stack_setup_receiver_m(ctx, rng, params, io, true);
}

export function stack_setup_sender<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any): CotPoolSender
{
  return stack_setup_sender_m(ctx, rng, params, io, false);
}

export function stack_setup_sender_m<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any, malicious: boolean): CotPoolSender
{
  const m = params.seed_cot_count(malicious);
  const delta_msg = sample_bytes(16n, rng);
  io.send(TAG_DELTA, delta_msg);
  let delta_ot = Array.from({length: Number(IKNP_KAPPA)}, () => false);
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    delta_ot[Number(i)] = __equals(fieldBitand(rng.next_u32(), 1n), 1n);
  }
  const delta_ot_bytes = pack_kappa(delta_ot);
  let chosen_seeds = Array.from({length: Number(IKNP_KAPPA)}, () => Array.from({length: Number(IKNP_KAPPA_BYTES)}, () => ((0n) & 0xFFn)));
  for (let i = 0n; i < IKNP_KAPPA; i += 1n)   {
    const setup = decode_lwe_crs(16n, io.recv(TAG_LWE_SETUP));
    const [r_state, recv_msg] = recv_start(rng, setup, delta_ot[Number(i)]);
    io.send(TAG_LWE_RECV, encode_lwe_recv(recv_msg));
    const payload = decode_lwe_payload(io.recv(TAG_LWE_PAYLOAD));
    chosen_seeds[Number(i)] = recv_finish(r_state, payload);
  }
  const u_msg = decode_iknp_u(io.recv(TAG_IKNP_U));
  const [sender_r0, corrections] = iknp_sender_from_u(ctx, 16n, m, delta_msg, delta_ot, delta_ot_bytes, chosen_seeds, u_msg);
  io.send(TAG_IKNP_CORR, encode_iknp_corr(corrections));
  const tag_s = ssp_sender_tag(delta_msg, sender_r0);
  io.send(TAG_SSP_S, tag_s);
  const tag_r = io.recv(TAG_SSP_R);
  let q = ([] as any[]);
  for (const row of sender_r0)   {
    (q).push(row);
  }
  let sender = new CotPoolSender({ $fparams: params, $fseed: new FerretSenderSeed({ $fdelta: delta_msg, $fq: q }), $fout: [] as any[], $fraise_n: undefined, $fmalicious: malicious, $frefill_count: 0n });
  if (malicious)   {
    stack_refill_sender_malicious(rng, sender, io);
  } else   {
    stack_refill_sender(rng, sender, io);
  }
  return sender;
}

export function stack_setup_sender_malicious<R, Io>(ctx: { newD: () => any }, rng: any, params: any, io: any): CotPoolSender
{
  return stack_setup_sender_m(ctx, rng, params, io, true);
}

export function stack_uses_lwe_base(n: bigint, l: bigint): boolean
{
  return (ctx.sizeOf_unknown > 0n);
}

export function stream_expansion(ctx: { defaultA: () => any, defaultT: () => any }, ctx: any, wire_count: bigint, prg: any, garbler: any, random: any, noise: any): Result<Vec<ExpansionStep>, Error>
{
  const plan = garbler.$fplan;
  const top = ctx.max_level();
  const degree = ctx.parameters().$fdegree;
  const online_base = fieldSub(top, prg.depth());
  const required_bits = fieldMul(2n, wire_count);
  let top_expanded: Vec<Ciphertext> = ([] as any[]);
  const prg_slots = prg.slots();
  for (let _ = 0n; _ < plan.$fseed_ciphertexts; _ += 1n)   {
    if ((BigInt(top_expanded.length) >= required_bits))     {
      break;
    }
    let seed_slots = Array.from({length: Number(degree)}, () => 0n);
    let byte = Array.from({length: Number(1n)}, () => ((0n) & 0xFFn));
    for (const slot of seed_slots.take(prg_slots))     {
      const __volar_try_0 = random.fill_bytes(byte.slice(0));
      if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
      slot = BigInt(fieldBitand(byte[Number(0n)], 1n));
    }
    const __volar_try_4 = ctx.encrypt(ctx, garbler.$fsecret, top, seed_slots, random, noise);
    if (__volar_try_4 instanceof __VolarError) return __volar_try_4;
    const ct = __volar_try_4;
    top_expanded.push(...(prg.expand(ctx, garbler.$fksks.$fl, ctx, ct, garbler.$fksks)));
  }
  (top_expanded).splice(Number(required_bits));
  let steps = ([] as any[]);
  for (const [w, pair] of __chunks(top_expanded, Number(2n)).filter((__chunk: any) => __chunk.length === Number(2n)).map((__enum_a: any, __enum_i: number) => [BigInt(__enum_i), __enum_a] as [bigint, typeof __enum_a]))   {
    const __volar_try_0 = garbler.wire_state(ctx, plan, pair[Number(0n)], pair[Number(1n)], w);
    if (__volar_try_0 instanceof __VolarError) return __volar_try_0;
    const wire_state = __volar_try_0;
    (steps).push(new ExpansionStep({ $fwire_state: wire_state, $feval: new ExpandedWire({ $fct_a0: ctx.switch_to(pair[Number(0n)], online_base), $fct_pi: ctx.switch_to(pair[Number(1n)], online_base), $fslot_a0: ((fieldMul(2n, w) % plan.$fslots_per_seed) % degree), $fslot_pi: ((fieldAdd(fieldMul(2n, w), 1n) % plan.$fslots_per_seed) % degree) }) }));
  }
  return steps;
}

export function sub4(a: bigint[], b: bigint[]): bigint[]
{
  let out = Array.from({length: Number(4n)}, () => 0n);
  let borrow = 0n;
  for (let i = 0n; i < 4n; i += 1n)   {
    const d = fieldSub(fieldSub(fieldAdd(fieldShl(1n, 64n), (a[Number(i)] as unknown as bigint)), (b[Number(i)] as unknown as bigint)), borrow);
    out[Number(i)] = BigInt(d);
    borrow = fieldSub(1n, fieldShr(d, 64n));
  }
  return out;
}

export function sub_bytes(state: bigint[])
{
  for (let i = 0n; i < BLOCK; i += 1n)   {
    state[Number(i)] = SBOX[Number(BigInt(state[Number(i)]))];
  }
}

export function sub_coeffs(a: bigint[], b: bigint[], modulus: bigint): Vec<bigint>
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => sub_mod(x, y, modulus));
}

export function sub_limb(a: any, b: any, q: bigint): Limb
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => sub_mod(x, y, q));
}

export function sub_mod(...__args: any[]): any {
  if (__args.length === 3) {
    const left = __args[0];
    const right = __args[1];
    const modulus = __args[2];
    return (() => {
  return (() => { if ((left >= right)) {
  return fieldSub(left, right);
} else {
  return fieldSub(modulus, fieldSub(right, left));
} })();
})();
  } else
  if (__args.length === 3) {
    const left = __args[0];
    const right = __args[1];
    const modulus = __args[2];
    return (() => {
  return (() => { if ((left >= right)) {
  return fieldSub(left, right);
} else {
  return fieldSub(modulus, fieldSub(right, left));
} })();
})();
  }
  throw new Error("sub_mod(): no matching variant for " + __args.length + " args");
}

export function sub_mod_l(a: bigint[], b: bigint[]): bigint[]
{
  return (() => { if (!__equals(cmp4(a, b), Less)) {
  return sub4(a, b);
} else {
  return sub4(add4(a, L_LIMBS), b);
} })();
}

export function sub_rows(a: any, b: any, p: bigint): RowCt
{
  return new RowCt({ $fc0: sub_coeffs(a.$fc0, b.$fc0, p), $fc1: sub_coeffs(a.$fc1, b.$fc1, p) });
}

export function sub_rq(a: bigint[], b: bigint[], q: bigint): RqPoly
{
  return a.map((__a: any, __i: number) => [__a, b[__i]] as [typeof __a, any]).map(([x, y]: any) => sub_mod(x, y, q));
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
  let b = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
  (b).splice(0, (bytes.slice(Number(off), Number(fieldAdd(off, 16n)))).length, ...(bytes.slice(Number(off), Number(fieldAdd(off, 16n)))));
  off = fieldAdd(off, 16n);
  return b;
}

export function take_random<R>(rng: any, sender: any, receiver: any, need: bigint): ([Vec<Block>, Vec<boolean>, Vec<Block>] | undefined)
{
  if (!ensure(rng, sender, receiver, need))   {
    return undefined;
  }
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
      combined.$fa[Number(i)] = wrappingAdd(combined.$fa[Number(i)], BigInt(scaled), 32);
    }
    const scaled_b = (BigInt(Math.imul(Number(BigInt(addr_ct.$fb)), Number(target))) / BigInt(Q4));
    combined.$fb = wrappingAdd(combined.$fb, BigInt(scaled_b), 32);
  }
  combined.$fb = wrappingAdd(combined.$fb, BigInt((delta / 2n)), 32);
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

export function to_limb(delta: any, p: bigint, q: bigint): Limb
{
  return delta.map((c: any) => (() => {
  return (() => { if (__equals(c, 0n)) {
  return 0n;
} else if (__equals(c, 1n)) {
  return 1n;
} else {
  return fieldSub(q, 1n);
} })();
})());
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
  const exp = BigInt(fieldShr(wrappingAdd(x, half, 32), scale_shift));
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
  return fieldAdd(params.$fk, uni_spcot_heights(hash_seed, params.$fn, params.$ft).reduce((__sum: bigint, __x: bigint) => __sum + __x, 0n));
}

export function uni_seed_cot_count_malicious(hash_seed: bigint[], params: any): bigint
{
  return fieldAdd(uni_seed_cot_count(hash_seed, params), KAPPA_BITS);
}

export function uni_spcot_heights(hash_seed: bigint[], n: bigint, t: bigint): Vec<bigint>
{
  const m = cuckoo_table_size(t);
  const buckets = build_buckets(hash_seed, n, m);
  return buckets.map((b: any) => BigInt(((__value) => { let __bits = 0n; if (__value === 0n) return BigInt(64); while ((__value & 1n) === 0n) { __value >>= 1n; __bits += 1n; } return __bits; })(BigInt.asUintN(64, next_pow2(fieldAdd(BigInt(b.length), 1n))))));
}

export function uniform_limb(n: bigint, q: bigint, random: any): Limb
{
  let out = Array.from({length: Number(n)}, () => 0n);
  for (let __zip_mut_1 = 0n; __zip_mut_1 < BigInt(out.length); __zip_mut_1 += 1n) {
    {
      out[Number(__zip_mut_1)] = (BigInt(random.next_u32()) % q);
    }}
  return out;
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

export function verify(ctx: { newD: () => any, defaultT: () => any, HClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any>, LClass: { new(...args: any[]): any } & Record<string, (...args: any[]) => any> }, pk: any, message: bigint[], sig: any): boolean
{
  const iv = sig.$fiv;
  const mu: Vec<bigint> = (() => {
  let h = new Sha3_256();
  h.update(pk[0]);
  h.update(message);
  return Array.from(h.finalize(), BigInt);
})();
  const chall_1 = chall1(ctx, mu, iv, sig.$fbavc_root, fieldAdd(LAMBDA_BYTES, 8n), false);
  const deltas = expand_challenge_to_deltas(chall_1, TAU, SUB_VOLE_N);
  const reconstructed_seeds_opt = BavcDyn.reconstruct(ctx, 32n, sig.$fnodes, sig.$fhidden_commits, deltas, iv, sig.$fbavc_root, TAU, SUB_VOLE_N);
  let __volar_match_result_5 ;
  const __volar_match_5 = reconstructed_seeds_opt;
  if (__volar_match_5 !== null && __volar_match_5 !== undefined) {
    const s = __volar_match_5;
    __volar_match_result_5 = s;
  }
  {
    return false;
  }
  const reconstructed_seeds = __volar_match_result_5;
  let sub_voles_v: Vec<ConvertOutput> = ([] as any[]);
  for (let i = 0n; i < TAU; i += 1n)   {
    const d = deltas[Number(i)];
    let verifier_seeds: Vec<(bigint[] | undefined)> = ([] as any[]);
    for (let j = 0n; j < SUB_VOLE_N; j += 1n)     {
      if (__equals(j, 0n))       {
        (verifier_seeds).push(undefined);
      } else       {
        (verifier_seeds).push(reconstructed_seeds[Number(fieldAdd(fieldMul(i, SUB_VOLE_N), fieldBitxor(j, d)))]);
      }
    }
    (sub_voles_v).push(convert_to_vole(verifier_seeds, iv, BigInt(i), L_HAT_BYTES));
  }
  const corrections = sig.$fcorrections;
  if (!__equals(BigInt(corrections.length), fieldSub(TAU, 1n)))   {
    return false;
  }
  const big_q: Vec<bigint> = (() => {
  const q_out = concat_small_voles_verifier(sub_voles_v, deltas, corrections);
  let big_q: Vec<bigint> = [] as any[];
  for (let i = 0n; i < BigInt(q_out.$fq_columns.length); i += 1n)   {
    for (let j = 0n; j < BigInt(q_out.$fq_columns[Number(i)].length); j += 1n)     {
      (big_q).push(q_out.$fq_columns[Number(i)][Number(j)]);
    }
  }
  return big_q;
})();
  let corrections_flat: Vec<bigint> = [] as any[];
  for (let i = 0n; i < BigInt(sig.$fcorrections.length); i += 1n)   {
    for (let j = 0n; j < BigInt(sig.$fcorrections[Number(i)].length); j += 1n)     {
      (corrections_flat).push(sig.$fcorrections[Number(i)][Number(j)]);
    }
  }
  const chall_2 = chall2(ctx, chall_1, sig.$fvole_u, corrections_flat, fieldAdd(LAMBDA_BYTES, 8n), false);
  const hash_key = hash_key_from_chall(chall_2);
  const derived_chall_3 = chall3(ctx, chall_2, sig.$fqs_proof.$fa_hat, sig.$fqs_proof.$fb_hat, sig.$fc_hat_with_counter, LAMBDA_BYTES, false);
  if (!__equals(derived_chall_3, sig.$fchall_3))   {
    return false;
  }
  if (!has_trailing_zero_bits(sig.$fchall_3, W_GRIND))   {
    return false;
  }
  const _ = hash_key;
  return true;
}

export function vole_and_prover_step<T>(n: bigint, vope_a: VopeDyn<any>, vope_b: VopeDyn<any>): [VopeDyn<any>, any[]]
{
  const u_c_inner = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(vope_a.$fu[Number(0n)][Number(i)]), __clone(vope_b.$fu[Number(0n)][Number(i)])));
  const u_c = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(u_c_inner));
  const v_c = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return fieldAdd(fieldMul(__clone(vope_a.$fv[Number(i)]), __clone(vope_b.$fu[Number(0n)][Number(i)])), fieldMul(__clone(vope_b.$fv[Number(i)]), __clone(vope_a.$fu[Number(0n)][Number(i)])));
})());
  const hat = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(vope_a.$fv[Number(i)]), __clone(vope_b.$fv[Number(i)])));
  return [new VopeDyn({ $fu: u_c, $fv: v_c, $fn: 0n, $fk: 1n }), hat];
}

export function vole_and_verifier_check<T>(n: bigint, delta: DeltaLoweredDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, q_and: QDyn<any>, hat: any[]): [QDyn<any>, boolean]
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const lhs = fieldAdd(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), __clone(hat[Number(i)]));
    const rhs = fieldMul(__clone(q_and.$fq[Number(i)]), __clone(delta.$fdelta[Number(i)]));
    ok = (ok && __equals(lhs, rhs));
  }
  return [new QDyn({ $fq: __clone(q_and.$fq), $fn: 0n }), ok];
}

export function vole_commit_bit<T, R>(n: bigint, cot: IdealCotDyn<any>, rng: any, sample_t: any, bit_to_t: (arg: boolean) => any, bit: boolean): [VopeDyn<any>, QDyn<any>]
{
  const [r0, v] = cot.cot(rng, sample_t, bit);
  return [vole_commit_bit_prover_share(n, v, bit_to_t, bit), vole_commit_bit_verifier_share(n, r0)];
}

export function vole_commit_bit_from<T, R, C>(n: bigint, cot: any, rng: any, sample_t: any, bit_to_t: (arg: boolean) => any, bit: boolean): [VopeDyn<any>, QDyn<any>]
{
  const [r0, v] = cot.cot(rng, sample_t, bit);
  return vole_commit_bit_shares(n, r0, v, bit_to_t, bit);
}

export function vole_commit_bit_prover_share<T>(n: bigint, v: any[], bit_to_t: (arg: boolean) => any, bit: boolean): VopeDyn<any>
{
  const u_t = bit_to_t(bit);
  const u_row: any[] = lift_bit(n, u_t);
  const u: any[][] = Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(u_row[Number(i)])));
  return new VopeDyn({ $fu: u, $fv: v, $fn: 0n, $fk: 1n });
}

export function vole_commit_bit_shares<T>(n: bigint, r0: any[], v: any[], bit_to_t: (arg: boolean) => any, bit: boolean): [VopeDyn<any>, QDyn<any>]
{
  return [vole_commit_bit_prover_share(n, v, bit_to_t, bit), vole_commit_bit_verifier_share(n, r0)];
}

export function vole_commit_bit_verifier_share<T>(n: bigint, q: any[]): QDyn<any>
{
  return new QDyn({ $fq: q, $fn: 0n });
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
    let bytes = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
    (bytes).splice(0, (block).length, ...(block));
    const s = new Galois128(u128_from_le_bytes(bytes));
    h0 = fieldAdd(h0, fieldMul(s, pow0));
    const s64 = new Galois64(BigInt(s[0]));
    h1 = fieldAdd(h1, fieldMul(s64, pow1));
    pow0 = fieldMul(pow0, key.$fr0);
    pow1 = fieldMul(pow1, key.$fr1);
  }
  if (!(tail.length === 0))   {
    let bytes = Array.from({length: Number(8n)}, () => ((0n) & 0xFFn));
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

export function vole_mul3_prover_step<T>(ctx: { defaultT: () => any }, n: bigint, vope_a: VopeDyn<any>, vope_b: VopeDyn<any>, vope_d: VopeDyn<any>): VopeDyn<any>
{
  const ab: VopeDyn<any> = vope_a.mul_generalized(ctx, vope_b.$fk, vope_b);
  return ab.mul_generalized(ctx, vope_d.$fk, vope_d);
}

export function vole_mul3_verifier_check<T>(n: bigint, delta: DeltaLoweredDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, q_d: QDyn<any>, vope_abd: VopeDyn<any>): [QDyn<any>, boolean]
{
  const q_abd = fieldMul(vope_abd, __clone(delta));
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const lhs = fieldMul(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), __clone(q_d.$fq[Number(i)]));
    ok = (ok && __equals(lhs, q_abd.$fq[Number(i)]));
  }
  return [q_abd, ok];
}

export function vole_rekey_prover<T>(n: bigint, wire: VopeDyn<any>, key: VopeDyn<any>): VopeDyn<any>
{
  return fieldAdd(wire, key);
}

export function vole_rekey_verifier_check<T>(n: bigint, q_wire: QDyn<any>, q_key: QDyn<any>, q_rekeyed: QDyn<any>): boolean
{
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    const expect = fieldAdd(__clone(q_wire.$fq[Number(i)]), __clone(q_key.$fq[Number(i)]));
    ok = (ok && __equals(__clone(q_rekeyed.$fq[Number(i)]), expect));
  }
  return ok;
}

export function vole_sbox_prover_step<T>(ctx: { defaultT: () => any }, n: bigint, vope_a: VopeDyn<any>, vope_b: VopeDyn<any>): [VopeDyn<any>, VopeDyn<any>]
{
  const k2: VopeDyn<any> = vope_a.mul_generalized(ctx, vope_b.$fk, vope_b);
  const k1 = new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => __clone(k2.$fu[Number(1n)])), $fv: __clone(k2.$fu[Number(0n)]), $fn: 0n, $fk: 1n });
  return [k1, k2];
}

export function vole_sbox_verifier_check<T>(n: bigint, delta: DeltaLoweredDyn<any>, q_a: QDyn<any>, q_b: QDyn<any>, vope_k2: VopeDyn<any>): [QDyn<any>, boolean]
{
  const q_c = fieldMul(vope_k2, __clone(delta));
  let ok = true;
  for (let i = 0n; i < n; i += 1n)   {
    ok = (ok && __equals(fieldMul(__clone(q_a.$fq[Number(i)]), __clone(q_b.$fq[Number(i)])), q_c.$fq[Number(i)]));
  }
  return [q_c, ok];
}

export function vope_bitpack<T>(ctx: { defaultT: () => any }, bits: bigint, n: bigint, bit_values: VopeDyn<any>[], pow2: any[]): VopeDyn<any>
{
  let acc = new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT())), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultT()), $fn: 0n, $fk: 1n });
  for (let index = 0n; index < bits; index += 1n)   {
    acc = fieldAdd(acc, vope_scale_const(n, bit_values[Number(index)], pow2[Number(index)]));
  }
  return acc;
}

export function vope_open_mask<T>(n: bigint, w: VopeDyn<any>): any[]
{
  return __clone(w.$fv);
}

export function vope_scale_const<T>(n: bigint, w: VopeDyn<any>, c: any): VopeDyn<any>
{
  return new VopeDyn({ $fu: Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => (() => {
  return Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(w.$fu[Number(0n)][Number(i)]), __clone(c)));
})()), $fv: Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => fieldMul(__clone(w.$fv[Number(i)]), __clone(c))), $fn: 0n, $fk: 1n });
}

export function warp_absorb_index(m: bigint, terms: bigint, deg: bigint, pesat: WarpIndexDyn, transcript: any)
{
  transcript.absorb_array(Array.from({length: Number(u5 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => [105n, 110n, 100n, 101n, 120n][Number(i)]).$fn, Array.from({length: Number(u5 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => [105n, 110n, 100n, 101n, 120n][Number(i)]));
  transcript.absorb_usize(m);
  transcript.absorb_usize(pesat.$fpublic_len);
  transcript.absorb_usize(pesat.$fwitness_len);
  transcript.absorb_usize(pesat.$fdegree_bound);
  for (let c = 0n; c < m; c += 1n)   {
    const constraint = pesat.$fconstraints[Number(c)];
    transcript.absorb_usize(BigInt(constraint.$fn_terms));
    for (let i = 0n; i < terms; i += 1n)     {
      if ((i < BigInt(constraint.$fn_terms)))       {
        const term = constraint.$fterms[Number(i)];
        transcript.absorb_field(term.$fcoeff);
        transcript.absorb_usize(BigInt(term.$fn_vars));
        for (let j = 0n; j < deg; j += 1n)         {
          if ((j < BigInt(term.$fn_vars)))           {
            transcript.absorb_usize(term.$fvars[Number(j)]);
          }
        }
      }
    }
  }
}

export function warp_absorb_instance(logn: bigint, beta: bigint, transcript: any, instance: WarpAccInstanceDyn)
{
  transcript.absorb_array(Array.from({length: Number(u12 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => [97n, 99n, 99n, 45n, 105n, 110n, 115n, 116n, 97n, 110n, 99n, 101n][Number(i)]).$fn, Array.from({length: Number(u12 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => [97n, 99n, 99n, 45n, 105n, 110n, 115n, 116n, 97n, 110n, 99n, 101n][Number(i)]));
  transcript.absorb_len_prefixed(instance.$froot.$fn, instance.$froot);
  transcript.absorb_fields(instance.$falpha.$fn, instance.$falpha);
  transcript.absorb_field(instance.$fmu);
  transcript.absorb_fields(instance.$fbeta.$fn, instance.$fbeta);
  transcript.absorb_field(instance.$feta);
}

export function warp_binary_point(log: bigint, index: bigint): Galois128[]
{
  return Array.from({length: Number(log - 0n)}, (_, __i) => BigInt(__i) + 0n).map((bit: any) => (() => {
  return (() => { if (__equals(fieldBitand(fieldShr(index, bit), 1n), 1n)) {
  return new Galois128(1n);
} else {
  return new Galois128(0n);
} })();
})());
}

export function warp_check_sumcheck(sc: bigint, rounds: bigint, polys: WarpUniDyn[], initial_claim: Galois128, challenges: Galois128[], final_claim: Galois128): boolean
{
  if (__equals(rounds, 0n))   {
    return __equals(initial_claim, final_claim);
  }
  if (!__equals(fieldAdd(warp_uni_at_zero(sc, polys[Number(0n)]), warp_uni_at_one(sc, polys[Number(0n)])), initial_claim))   {
    return false;
  }
  for (let round = 1n; round < rounds; round += 1n)   {
    const expected = warp_uni_evaluate(sc, polys[Number(fieldSub(round, 1n))], challenges[Number(fieldSub(round, 1n))]);
    if (!__equals(fieldAdd(warp_uni_at_zero(sc, polys[Number(round)]), warp_uni_at_one(sc, polys[Number(round)])), expected))     {
      return false;
    }
  }
  return __equals(warp_uni_evaluate(sc, polys[Number(fieldSub(rounds, 1n))], challenges[Number(fieldSub(rounds, 1n))]), final_claim);
}

export function warp_combine_vectors(ell: bigint, logl: bigint, len: bigint, items: Galois128[][], point: Galois128[]): Galois128[]
{
  const weights = warp_eq_evals(logl, ell, point);
  let out = Array.from({length: Number(len - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let i = 0n; i < ell; i += 1n)   {
    const weight = weights[Number(i)];
    for (let j = 0n; j < len; j += 1n)     {
      out[Number(j)] = fieldAdd(out[Number(j)], fieldMul(items[Number(i)][Number(j)], weight));
    }
  }
  return out;
}

export function warp_commit_codeword(n: bigint, nodes: bigint, codeword: Galois128[]): WarpMerkleTreeDyn
{
  const leaves = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => warp_hash_leaf_field(codeword[Number(i)]));
  return warp_merkle_commit_leaves(nodes, n, leaves);
}

export function warp_commit_stacked(n: bigint, fresh: bigint, nodes: bigint, codewords: Galois128[][]): WarpMerkleTreeDyn
{
  const leaves = Array.from({length: Number(n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  let h = spec_sponge_new(SHA3_256_RATE);
  spec_sponge_absorb_array(n, h, Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  for (let j = 0n; j < fresh; j += 1n)   {
    const elem_bytes: bigint[] = le16(codewords[Number(j)][Number(i)]);
    spec_sponge_absorb_array(n, h, elem_bytes);
  }
  let reader = spec_sponge_finalize_xof(h, 6n);
  let out: bigint[] = Array.from({length: Number(32n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(n, reader, out);
  return out;
})());
  return warp_merkle_commit_leaves(nodes, n, leaves);
}

export function warp_constraint_max_degree(terms: bigint, deg: bigint, constraint: WarpConstraintDyn): bigint
{
  let out = 0n;
  for (let i = 0n; i < terms; i += 1n)   {
    if ((i < BigInt(constraint.$fn_terms)))     {
      const d = warp_term_degree(deg, constraint.$fterms[Number(i)]);
      if ((d > out))       {
        out = d;
      }
    }
  }
  return out;
}

export function warp_decide_spec<C>(ctx: { defaultT: () => any }, m: bigint, terms: bigint, deg: bigint, n: bigint, k: bigint, logn: bigint, beta: bigint, nodes: bigint, index: WarpIndexDyn, code: any, accumulator: WarpAccDyn): boolean
{
  if (!warp_index_is_valid(m, terms, deg, index))   {
    return false;
  }
  if ((((!__equals(logn, log2_pow2(n)) || !__equals(beta, fieldAdd(index.$fpublic_len, warp_log_constraints(m, terms, deg, index)))) || !__equals(k, index.$fwitness_len)) || !__equals(warp_merkle_root(nodes, accumulator.$fwitness.$ftree), accumulator.$finstance.$froot)))   {
    return false;
  }
  const recomputed = warp_commit_codeword(n, nodes, accumulator.$fwitness.$fcodeword);
  if (!__equals(warp_merkle_root(nodes, recomputed), accumulator.$finstance.$froot))   {
    return false;
  }
  if (!__equals(code.encode(ctx, accumulator.$fwitness.$fwitness), accumulator.$fwitness.$fcodeword))   {
    return false;
  }
  if (!__equals(warp_mle_eval(n, logn, accumulator.$fwitness.$fcodeword, accumulator.$finstance.$falpha), accumulator.$finstance.$fmu))   {
    return false;
  }
  return __equals(warp_evaluate_bundled(m, terms, deg, k, beta, index, accumulator.$finstance.$fbeta, accumulator.$fwitness.$fwitness), accumulator.$finstance.$feta);
}

export function warp_eq_evals(points: bigint, out: bigint, point: Galois128[]): Galois128[]
{
  let out_1 = Array.from({length: Number(out - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  out_1[Number(0n)] = new Galois128(1n);
  for (let c = 0n; c < points; c += 1n)   {
    const coordinate = point[Number(c)];
    for (let index = 0n; index < fieldShl(1n, c); index += 1n)     {
      const product = fieldMul(out_1[Number(index)], coordinate);
      out_1[Number(index)] = fieldSub(out_1[Number(index)], product);
      out_1[Number(fieldAdd(index, fieldShl(1n, c)))] = product;
    }
  }
  return out_1;
}

export function warp_eq_scalar(l: bigint, a: Galois128[], b: Galois128[]): Galois128
{
  let out = new Galois128(1n);
  for (let i = 0n; i < l; i += 1n)   {
    const left = a[Number(i)];
    const right = b[Number(i)];
    const same = fieldMul(left, right);
    const different = fieldMul(fieldSub(new Galois128(1n), left), fieldSub(new Galois128(1n), right));
    out = fieldMul(out, fieldAdd(same, different));
  }
  return out;
}

export function warp_eq_star_at(r: bigint, logr: bigint, logn: bigint, claim_points: Galois128[][], xi: Galois128[], point: Galois128[]): Galois128
{
  let out = new Galois128(0n);
  for (let index = 0n; index < r; index += 1n)   {
    const claim_index = warp_binary_point(logr, index);
    const weight = fieldMul(warp_eq_scalar(logr, xi, claim_index), warp_eq_scalar(logn, claim_points[Number(index)], point));
    out = fieldAdd(out, weight);
  }
  return out;
}

export function warp_eval_constraint(terms: bigint, deg: bigint, x: bigint, k: bigint, constraint: WarpConstraintDyn, public_: Galois128[], witness: Galois128[]): Galois128
{
  let out = new Galois128(0n);
  for (let i = 0n; i < terms; i += 1n)   {
    if ((i < BigInt(constraint.$fn_terms)))     {
      out = fieldAdd(out, warp_eval_term(deg, x, k, constraint.$fterms[Number(i)], public_, witness));
    }
  }
  return out;
}

export function warp_eval_term(deg: bigint, x: bigint, k: bigint, term: WarpTermDyn, public_: Galois128[], witness: Galois128[]): Galois128
{
  let out = term.$fcoeff;
  for (let i = 0n; i < deg; i += 1n)   {
    if ((i < BigInt(term.$fn_vars)))     {
      const var_ = term.$fvars[Number(i)];
      const value = (() => { if ((var_ < x)) {
  return public_[Number(var_)];
} else {
  return witness[Number(fieldSub(var_, x))];
} })();
      out = fieldMul(out, value);
    }
  }
  return out;
}

export function warp_evaluate_bundled(m: bigint, terms: bigint, deg: bigint, k: bigint, beta: bigint, pesat: WarpIndexDyn, beta: Galois128[], witness: Galois128[]): Galois128
{
  const log_m = warp_log_constraints(m, terms, deg, pesat);
  let eq = Array.from({length: Number(m - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  eq[Number(0n)] = new Galois128(1n);
  for (let j = 0n; j < m; j += 1n)   {
    if ((fieldShl(1n, j) < m))     {
      const coordinate = beta[Number(fieldAdd(pesat.$fpublic_len, j))];
      for (let index = 0n; index < fieldShl(1n, j); index += 1n)       {
        const product = fieldMul(eq[Number(index)], coordinate);
        eq[Number(index)] = fieldSub(eq[Number(index)], product);
        eq[Number(fieldAdd(index, fieldShl(1n, j)))] = product;
      }
    }
  }
  let out = new Galois128(0n);
  for (let index = 0n; index < m; index += 1n)   {
    const constraint = pesat.$fconstraints[Number(index)];
    let value = new Galois128(0n);
    for (let i = 0n; i < BigInt(constraint.$fn_terms); i += 1n)     {
      const term = constraint.$fterms[Number(i)];
      let product = term.$fcoeff;
      for (let v = 0n; v < BigInt(term.$fn_vars); v += 1n)       {
        const var_ = term.$fvars[Number(v)];
        const x = (() => { if ((var_ < pesat.$fpublic_len)) {
  return beta[Number(var_)];
} else {
  return witness[Number(fieldSub(var_, pesat.$fpublic_len))];
} })();
        product = fieldMul(product, x);
      }
      value = fieldAdd(value, product);
    }
    out = fieldAdd(out, fieldMul(eq[Number(index)], value));
  }
  return out;
}

export function warp_evaluate_g(m: bigint, terms: bigint, deg: bigint, n: bigint, k: bigint, logn: bigint, beta: bigint, index: WarpIndexDyn, codeword: Galois128[], witness: Galois128[], alpha: Galois128[], beta: Galois128[], omega: Galois128): Galois128
{
  const evaluation = warp_mle_eval(n, logn, codeword, alpha);
  const bundled = warp_evaluate_bundled(m, terms, deg, k, beta, index, beta, witness);
  return fieldAdd(evaluation, fieldMul(omega, bundled));
}

export function warp_first_sumcheck_eval(m: bigint, terms: bigint, deg: bigint, k: bigint, n: bigint, ell: bigint, logl: bigint, logn: bigint, beta: bigint, index: WarpIndexDyn, codewords: Galois128[][], witnesses: Galois128[][], alphas: Galois128[][], betas: Galois128[][], tau: Galois128[], omega: Galois128, prefix: Galois128[], round: bigint, x: Galois128): Galois128
{
  const log_ell = log2_pow2(ell);
  const suffix_len = fieldSub(fieldSub(log_ell, round), 1n);
  let out = new Galois128(0n);
  for (let suffix = 0n; suffix < fieldShr(ell, fieldAdd(round, 1n)); suffix += 1n)   {
    const point = warp_point_with_suffix(logl, prefix, round, x, suffix, suffix_len);
    const codeword = warp_combine_vectors(ell, logl, n, codewords, point);
    const witness = warp_combine_vectors(ell, logl, k, witnesses, point);
    const alpha = warp_combine_vectors(ell, logl, logn, alphas, point);
    const beta_1 = warp_combine_vectors(ell, logl, beta, betas, point);
    const g = warp_evaluate_g(m, terms, deg, n, k, logn, beta_1, index, codeword, witness, alpha, beta_1, omega);
    out = fieldAdd(out, fieldMul(warp_eq_scalar(logl, tau, point), g));
  }
  return out;
}

export function warp_genesis_spec<C>(ctx: { defaultT: () => any }, m: bigint, terms: bigint, deg: bigint, k: bigint, x: bigint, n: bigint, nodes: bigint, beta: bigint, index: WarpIndexDyn, code: any, instance: WarpWitnessDyn): (WarpAccDyn | undefined)
{
  if ((!warp_index_is_valid(m, terms, deg, index) || !is_pow2(n)))   {
    return undefined;
  }
  if (((!__equals(x, index.$fpublic_len) || !__equals(k, index.$fwitness_len)) || !__equals(beta, fieldAdd(index.$fpublic_len, warp_log_constraints(m, terms, deg, index)))))   {
    return undefined;
  }
  if (!warp_is_instance_satisfied(m, terms, deg, k, x, index, instance))   {
    return undefined;
  }
  const codeword = code.encode(ctx, instance.$fwitness);
  const tree = warp_commit_codeword(n, nodes, codeword);
  const alpha: Galois128[] = Array.from({length: Number(ilog2(n) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  let beta_1 = Array.from({length: Number(beta - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let i = 0n; i < x; i += 1n)   {
    beta_1[Number(i)] = instance.$fpublic[Number(i)];
  }
  const mu = warp_mle_eval(codeword, alpha);
  const eta = warp_evaluate_bundled(m, terms, deg, k, beta_1, index, beta_1, instance.$fwitness);
  return new WarpAccDyn({ $finstance: new WarpAccInstanceDyn({ $froot: warp_merkle_root(nodes, tree), $falpha: alpha, $fmu: mu, $fbeta: beta_1, $feta: eta, $flogn: 0n }), $fwitness: new WarpAccWitnessDyn({ $ftree: tree, $fcodeword: codeword, $fwitness: __clone(instance.$fwitness), $fn: 0n, $fk: 0n, $fnodes: 0n }), $fn: 0n, $fk: 0n, $flogn: 0n, $fbeta: 0n, $fnodes: 0n });
}

export function warp_hash_fresh_row(ell: bigint, fresh: bigint, row: Galois128[]): WarpDigest
{
  let h = spec_sponge_new(SHA3_256_RATE);
  spec_sponge_absorb_array(h, Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  for (let i = 0n; i < fresh; i += 1n)   {
    const row_bytes: bigint[] = le16(row[Number(i)]);
    spec_sponge_absorb_array(h, row_bytes);
  }
  let reader = spec_sponge_finalize_xof(h, 6n);
  let out: bigint[] = Array.from({length: Number(32n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(reader, out);
  return out;
}

export function warp_hash_leaf_field(v: Galois128): WarpDigest
{
  let h = spec_sponge_new(SHA3_256_RATE);
  spec_sponge_absorb_array(h, Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n));
  const v_bytes: bigint[] = le16(v);
  spec_sponge_absorb_array(h, v_bytes);
  let reader = spec_sponge_finalize_xof(h, 6n);
  let out: bigint[] = Array.from({length: Number(32n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(reader, out);
  return out;
}

export function warp_hash_node(left: any, right: any): WarpDigest
{
  let h = spec_sponge_new(SHA3_256_RATE);
  spec_sponge_absorb_array(h, Array.from({length: Number(1n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 1n));
  spec_sponge_absorb_array(h, left);
  spec_sponge_absorb_array(h, right);
  let reader = spec_sponge_finalize_xof(h, 6n);
  let out: bigint[] = Array.from({length: Number(32n - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => 0n);
  spec_xof_read_array(reader, out);
  return out;
}

export function warp_index_is_valid(m: bigint, terms: bigint, deg: bigint, pesat: WarpIndexDyn): boolean
{
  if (!is_pow2(m))   {
    return false;
  }
  for (let c = 0n; c < m; c += 1n)   {
    const constraint = pesat.$fconstraints[Number(c)];
    if ((BigInt(constraint.$fn_terms) > terms))     {
      return false;
    }
    if ((warp_constraint_max_degree(terms, deg, constraint) > pesat.$fdegree_bound))     {
      return false;
    }
    for (let i = 0n; i < terms; i += 1n)     {
      if ((i < BigInt(constraint.$fn_terms)))       {
        const term = constraint.$fterms[Number(i)];
        if ((BigInt(term.$fn_vars) > deg))         {
          return false;
        }
        for (let j = 0n; j < deg; j += 1n)         {
          if (((j < BigInt(term.$fn_vars)) && (term.$fvars[Number(j)] >= warp_variable_count(m, terms, deg, pesat))))           {
            return false;
          }
        }
      }
    }
  }
  return true;
}

export function warp_inv(a: Galois128): Galois128
{
  let result = new Galois128(1n);
  for (let _ = 0n; _ < 127n; _ += 1n)   {
    result = fieldMul(fieldMul(result, result), a);
  }
  return fieldMul(result, result);
}

export function warp_is_instance_satisfied(m: bigint, terms: bigint, deg: bigint, k: bigint, x: bigint, pesat: WarpIndexDyn, instance: WarpWitnessDyn): boolean
{
  for (let index = 0n; index < m; index += 1n)   {
    const constraint = pesat.$fconstraints[Number(index)];
    if (!__equals(warp_eval_constraint(terms, deg, x, k, constraint, instance.$fpublic, instance.$fwitness), new Galois128(0n)))     {
      return false;
    }
  }
  return true;
}

export function warp_log_constraints(m: bigint, terms: bigint, deg: bigint, pesat: WarpIndexDyn): bigint
{
  return log2_pow2(m);
}

export function warp_merkle_commit_leaves(nodes: bigint, leaves: bigint, leaves: WarpDigest[]): WarpMerkleTreeDyn
{
  let nodes_1 = Array.from({length: Number(nodes - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultWarpDigest());
  for (let i = 0n; i < leaves; i += 1n)   {
    nodes_1[Number(i)] = __clone(leaves[Number(i)]);
  }
  let level_start = 0n;
  let level_len = leaves;
  for (let _level = 0n; _level < leaves; _level += 1n)   {
    if ((level_len > 1n))     {
      const next_start = fieldAdd(level_start, level_len);
      for (let j = 0n; j < (level_len / 2n); j += 1n)       {
        nodes_1[Number(fieldAdd(next_start, j))] = warp_hash_node(nodes_1[Number(fieldAdd(level_start, fieldMul(2n, j)))], nodes_1[Number(fieldAdd(fieldAdd(level_start, fieldMul(2n, j)), 1n))]);
      }
      level_start = next_start;
      level_len /= 2n;
    }
  }
  return new WarpMerkleTreeDyn({ $fleaves: leaves, $fnodes: nodes_1 });
}

export function warp_merkle_open(nodes: bigint, log2: bigint, tree: WarpMerkleTreeDyn, index: bigint): WarpAuthPathDyn
{
  let siblings = Array.from({length: Number(log2 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => ctx.defaultWarpDigest());
  let idx = index;
  let level_start = 0n;
  let level_len = tree.$fleaves;
  for (let depth = 0n; depth < log2; depth += 1n)   {
    siblings[Number(depth)] = __clone(tree.$fnodes[Number(fieldAdd(level_start, fieldBitxor(idx, 1n)))]);
    idx /= 2n;
    level_start = fieldAdd(level_start, level_len);
    level_len /= 2n;
  }
  return new WarpAuthPathDyn({ $fsiblings: siblings, $flog2_leaves: 0n });
}

export function warp_merkle_root(nodes: bigint, tree: WarpMerkleTreeDyn): WarpDigest
{
  return __clone(tree.$fnodes[Number(fieldSub(nodes, 1n))]);
}

export function warp_merkle_verify_hashed(log2_leaves: bigint, root: any, leaf_hash: any, index: bigint, path: WarpAuthPathDyn): boolean
{
  let cur = __clone(leaf_hash);
  let idx = index;
  for (let i = 0n; i < log2_leaves; i += 1n)   {
    const sibling = path.$fsiblings[Number(i)];
    cur = (() => { if (__equals((idx % 2n), 0n)) {
  return warp_hash_node(cur, sibling);
} else {
  return warp_hash_node(sibling, cur);
} })();
    idx /= 2n;
  }
  return __equals(cur, root);
}

export function warp_mle_eval(n: bigint, logn: bigint, values: Galois128[], point: Galois128[]): Galois128
{
  const eq = warp_eq_evals(logn, n, point);
  let out = new Galois128(0n);
  for (let i = 0n; i < n; i += 1n)   {
    out = fieldAdd(out, fieldMul(values[Number(i)], eq[Number(i)]));
  }
  return out;
}

export function warp_point_with_suffix(log: bigint, prefix: Galois128[], prefix_len: bigint, current: Galois128, suffix: bigint, suffix_len: bigint): Galois128[]
{
  return Array.from({length: Number(log - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return (() => { if ((i < prefix_len)) {
  return prefix[Number(i)];
} else if (__equals(i, prefix_len)) {
  return current;
} else {
  const bit = fieldSub(fieldSub(i, prefix_len), 1n);
  return (() => { if (((bit < suffix_len) && __equals(fieldBitand(fieldShr(suffix, bit), 1n), 1n))) {
  return new Galois128(1n);
} else {
  return new Galois128(0n);
} })();
} })();
})());
}

export function warp_prove_spec<C>(ctx: { defaultT: () => any }, m: bigint, terms: bigint, deg: bigint, k: bigint, x: bigint, n: bigint, fresh: bigint, a: bigint, ell: bigint, s: bigint, shifts: bigint, r: bigint, sc: bigint, tw: bigint, fsd1: bigint, nodes: bigint, beta: bigint, index: WarpIndexDyn, code: any, fresh: WarpWitnessDyn[], accumulators: WarpAccDyn[]): ([WarpAccDyn, WarpProofDyn] | undefined)
{
  if (!warp_index_is_valid(m, terms, deg, index))   {
    return undefined;
  }
  const log_n = log2_pow2(n);
  const log_m = warp_log_constraints(m, terms, deg, index);
  const log_ell = log2_pow2(ell);
  if (((((((((((!is_pow2(n) || !is_pow2(ell)) || !__equals(ell, fieldAdd(fresh, a))) || __equals(fresh, 0n)) || !is_pow2(r)) || !__equals(r, fieldAdd(fieldAdd(1n, s), shifts))) || !__equals(tw, fieldAdd(log_ell, 1n))) || !__equals(beta, fieldAdd(index.$fpublic_len, log_m))) || !__equals(k, index.$fwitness_len)) || !__equals(x, index.$fpublic_len)) || !__equals(nodes, fieldSub(fieldMul(2n, n), 1n))))   {
    return undefined;
  }
  const first_degree = fieldAdd(1n, BigInt(Math.max(Number(fieldAdd(log_n, 1n)), Number(fieldAdd(log_m, index.$fdegree_bound)))));
  if (((!__equals(fsd1, fieldAdd(first_degree, 1n)) || (fsd1 > sc)) || (3n > sc)))   {
    return undefined;
  }
  for (let i = 0n; i < fresh; i += 1n)   {
    if (!warp_is_instance_satisfied(m, terms, deg, k, x, index, fresh[Number(i)]))     {
      return undefined;
    }
  }
  for (let i = 0n; i < a; i += 1n)   {
    if (!__equals(warp_merkle_root(nodes, accumulators[Number(i)].$fwitness.$ftree), accumulators[Number(i)].$finstance.$froot))     {
      return undefined;
    }
  }
  const fresh_publics = Array.from({length: Number(fresh - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(fresh[Number(i)].$fpublic));
  const acc_instances = Array.from({length: Number(a - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => __clone(accumulators[Number(i)].$finstance));
  let transcript = warp_transcript_for(index, fresh_publics, acc_instances);
  const fresh_codewords = Array.from({length: Number(fresh - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => code.encode(ctx, fresh[Number(i)].$fwitness));
  const initial_tree = warp_commit_stacked(n, fresh, nodes, fresh_codewords);
  const initial_root = warp_merkle_root(nodes, initial_tree);
  transcript.absorb_len_prefixed(n, initial_root);
  const taus: Galois128[][] = transcript.squeeze_field_grid(r);
  const zero_point_n = Array.from({length: Number(ilog2(n) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  const fresh_mus = Array.from({length: Number(fresh - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return warp_mle_eval(fresh_codewords[Number(i)], zero_point_n);
})());
  let alphas = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let mus = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  let betas = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let etas = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  let codewords = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let witnesses = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  for (let i = 0n; i < fresh; i += 1n)   {
    alphas[Number(i)] = Array.from({length: Number(ilog2(n) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
    mus[Number(i)] = fresh_mus[Number(i)];
    let beta_1 = Array.from({length: Number(beta - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
    for (let j = 0n; j < x; j += 1n)     {
      beta_1[Number(j)] = fresh[Number(i)].$fpublic[Number(j)];
    }
    for (let j = 0n; j < m; j += 1n)     {
      if ((fieldShl(1n, j) < m))       {
        beta_1[Number(fieldAdd(x, j))] = taus[Number(i)][Number(j)];
      }
    }
    betas[Number(i)] = beta_1;
    etas[Number(i)] = new Galois128(0n);
    codewords[Number(i)] = __clone(fresh_codewords[Number(i)]);
    witnesses[Number(i)] = __clone(fresh[Number(i)].$fwitness);
  }
  transcript.absorb_fields(n, fresh_mus);
  for (let i = 0n; i < a; i += 1n)   {
    const acc = accumulators[Number(i)];
    alphas[Number(fieldAdd(fresh, i))] = __clone(acc.$finstance.$falpha);
    mus[Number(fieldAdd(fresh, i))] = acc.$finstance.$fmu;
    betas[Number(fieldAdd(fresh, i))] = __clone(acc.$finstance.$fbeta);
    etas[Number(fieldAdd(fresh, i))] = acc.$finstance.$feta;
    codewords[Number(fieldAdd(fresh, i))] = __clone(acc.$fwitness.$fcodeword);
    witnesses[Number(fieldAdd(fresh, i))] = __clone(acc.$fwitness.$fwitness);
  }
  const tau_omega: Galois128[] = transcript.squeeze_fields(n);
  const tau = Array.from({length: Number(ilog2(ell) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => tau_omega[Number(i)]);
  const omega = tau_omega[Number(fieldSub(tw, 1n))];
  const sigma1 = (() => {
  const eq_tau = warp_eq_evals(tau);
  let out = new Galois128(0n);
  for (let i = 0n; i < ell; i += 1n)   {
    out = fieldAdd(out, fieldMul(eq_tau[Number(i)], fieldAdd(mus[Number(i)], fieldMul(omega, etas[Number(i)]))));
  }
  return out;
})();
  let first_sumcheck = Array.from({length: Number(ilog2(ell) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => new WarpUniDyn({ $fevaluations: Array.from({length: Number(sc - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default()), $fcount: 0n, $fcap: 0n }));
  let gamma = Array.from({length: Number(ilog2(ell) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let round = 0n; round < ell; round += 1n)   {
    if ((fieldShl(1n, round) < ell))     {
      let evals = Array.from({length: Number(sc - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
      for (let x = 0n; x < fsd1; x += 1n)       {
        evals[Number(x)] = warp_first_sumcheck_eval(index, codewords, witnesses, alphas, betas, tau, omega, gamma, round, new Galois128((x as unknown as bigint)));
      }
      const poly = new WarpUniDyn({ $fevaluations: evals, $fcount: BigInt(fsd1), $fcap: 0n });
      transcript.absorb_poly(sc, poly.$fcnt, poly);
      gamma[Number(round)] = transcript.squeeze_fields(n)[Number(0n)];
      first_sumcheck[Number(round)] = poly;
    }
  }
  const zeta0 = warp_combine_vectors(alphas, gamma);
  const beta_1 = warp_combine_vectors(betas, gamma);
  const folded_codeword = warp_combine_vectors(codewords, gamma);
  const folded_witness = warp_combine_vectors(witnesses, gamma);
  const nu0 = warp_mle_eval(folded_codeword, zeta0);
  const eta = warp_evaluate_bundled(m, terms, deg, k, beta_1, index, beta_1, folded_witness);
  if (!warp_check_sumcheck(first_sumcheck, sigma1, gamma, (() => {
  return fieldMul(warp_eq_scalar(tau, gamma), fieldAdd(nu0, fieldMul(omega, eta)));
})()))   {
    return undefined;
  }
  const output_tree = warp_commit_codeword(n, nodes, folded_codeword);
  const output_root = warp_merkle_root(nodes, output_tree);
  transcript.absorb_len_prefixed(n, output_root);
  transcript.absorb_field(nu0);
  transcript.absorb_field(eta);
  const ood_points: Galois128[][] = transcript.squeeze_field_grid(r);
  const ood_answers = Array.from({length: Number(s - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return warp_mle_eval(folded_codeword, ood_points[Number(i)]);
})());
  transcript.absorb_fields(n, ood_answers);
  const [shift_indices, xi]: [bigint[], Galois128[]] = transcript.squeeze_shift_and_xi(shifts);
  let claim_points = Array.from({length: Number(r - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let claim_values = Array.from({length: Number(r - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  claim_points[Number(0n)] = zeta0;
  claim_values[Number(0n)] = nu0;
  for (let i = 0n; i < s; i += 1n)   {
    claim_points[Number(fieldAdd(1n, i))] = __clone(ood_points[Number(i)]);
    claim_values[Number(fieldAdd(1n, i))] = ood_answers[Number(i)];
  }
  for (let i = 0n; i < shifts; i += 1n)   {
    claim_points[Number(fieldAdd(fieldAdd(1n, s), i))] = warp_binary_point(BigInt(shift_indices[Number(i)]));
    claim_values[Number(fieldAdd(fieldAdd(1n, s), i))] = folded_codeword[Number(BigInt(shift_indices[Number(i)]))];
  }
  const sigma2 = (() => {
  const eq_xi = warp_eq_evals(xi);
  let out = new Galois128(0n);
  for (let i = 0n; i < r; i += 1n)   {
    out = fieldAdd(out, fieldMul(claim_values[Number(i)], eq_xi[Number(i)]));
  }
  return out;
})();
  let second_sumcheck = Array.from({length: Number(ilog2(n) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => new WarpUniDyn({ $fevaluations: Array.from({length: Number(sc - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default()), $fcount: 0n, $fcap: 0n }));
  let alpha = Array.from({length: Number(ilog2(n) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let round = 0n; round < n; round += 1n)   {
    if ((fieldShl(1n, round) < n))     {
      let evals = Array.from({length: Number(sc - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
      for (let x = 0n; x < 3n; x += 1n)       {
        evals[Number(x)] = warp_second_sumcheck_eval(folded_codeword, claim_points, xi, alpha, round, new Galois128((x as unknown as bigint)));
      }
      const poly = new WarpUniDyn({ $fevaluations: evals, $fcount: 3n, $fcap: 0n });
      transcript.absorb_poly(sc, poly.$fcnt, poly);
      alpha[Number(round)] = transcript.squeeze_fields(n)[Number(0n)];
      second_sumcheck[Number(round)] = poly;
    }
  }
  const mu = warp_mle_eval(folded_codeword, alpha);
  if (!warp_check_sumcheck(second_sumcheck, sigma2, alpha, (() => {
  const eq = warp_eq_star_at(claim_points, xi, alpha);
  return fieldMul(mu, eq);
})()))   {
    return undefined;
  }
  const initial_paths = Array.from({length: Number(shifts - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return warp_merkle_open(initial_tree, BigInt(shift_indices[Number(i)]));
})());
  const accumulator_paths = Array.from({length: Number(a - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => (() => {
  return Array.from({length: Number(shifts - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return warp_merkle_open(accumulators[Number(i)].$fwitness.$ftree, BigInt(shift_indices[Number(j)]));
})());
})());
  const openings = Array.from({length: Number(shifts - 0n)}, (_, __i) => BigInt(__i) + 0n).map((j: any) => (() => {
  return Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => codewords[Number(i)][Number(BigInt(shift_indices[Number(j)]))]);
})());
  const accumulator = new WarpAccDyn({ $finstance: new WarpAccInstanceDyn({ $froot: output_root, $falpha: alpha, $fmu: mu, $fbeta: beta_1, $feta: eta, $flogn: 0n }), $fwitness: new WarpAccWitnessDyn({ $ftree: output_tree, $fcodeword: folded_codeword, $fwitness: folded_witness, $fn: 0n, $fk: 0n, $fnodes: 0n }), $fn: 0n, $fk: 0n, $flogn: 0n, $fbeta: 0n, $fnodes: 0n });
  const proof = new WarpProofDyn({ $finitial_root: initial_root, $ffresh_mus: fresh_mus, $ffirst_sumcheck: first_sumcheck, $fnu0: nu0, $food_answers: ood_answers, $fsecond_sumcheck: second_sumcheck, $finitial_paths: initial_paths, $faccumulator_paths: accumulator_paths, $fopenings: openings, $ffresh: 0n, $fell: 0n, $fa: 0n, $fs: 0n, $fshifts: 0n, $fsc: 0n, $flogn: 0n });
  return [accumulator, proof];
}

export function warp_second_sumcheck_eval(n: bigint, r: bigint, logr: bigint, logn: bigint, codeword: Galois128[], claim_points: Galois128[][], xi: Galois128[], prefix: Galois128[], round: bigint, x: Galois128): Galois128
{
  const log_n = log2_pow2(n);
  const suffix_len = fieldSub(fieldSub(log_n, round), 1n);
  let out = new Galois128(0n);
  for (let suffix = 0n; suffix < fieldShr(n, fieldAdd(round, 1n)); suffix += 1n)   {
    const point = warp_point_with_suffix(logn, prefix, round, x, suffix, suffix_len);
    const eq = warp_eq_star_at(r, logr, logn, claim_points, xi, point);
    out = fieldAdd(out, fieldMul(eq, warp_mle_eval(n, logn, codeword, point)));
  }
  return out;
}

export function warp_term_degree(deg: bigint, term: WarpTermDyn): bigint
{
  return BigInt(term.$fn_vars);
}

export function warp_transcript_for(m: bigint, terms: bigint, deg: bigint, x: bigint, fresh: bigint, a: bigint, logn: bigint, beta: bigint, index: WarpIndexDyn, fresh_publics: Galois128[][], accumulators: WarpAccInstanceDyn[]): WarpTranscript
{
  const domain = Array.from({length: Number(u51 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => WARP_TRANSCRIPT_DOMAIN[Number(i)]);
  let transcript = warp_transcript_new().domain_sep(domain.$fn, domain);
  warp_absorb_index(m, terms, deg, index, transcript);
  transcript.absorb_usize(fresh);
  for (let i = 0n; i < fresh; i += 1n)   {
    transcript.absorb_array(Array.from({length: Number(u12 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => [102n, 114n, 101n, 115n, 104n, 45n, 112n, 117n, 98n, 108n, 105n, 99n][Number(i)]).$fn, Array.from({length: Number(u12 - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => [102n, 114n, 101n, 115n, 104n, 45n, 112n, 117n, 98n, 108n, 105n, 99n][Number(i)]));
    transcript.absorb_fields(fresh_publics[Number(i)].$fn, fresh_publics[Number(i)]);
  }
  transcript.absorb_usize(a);
  for (let i = 0n; i < a; i += 1n)   {
    warp_absorb_instance(logn, beta, transcript, accumulators[Number(i)]);
  }
  return transcript;
}

export function warp_transcript_new(): WarpTranscript
{
  return new WarpTranscript({ $fsponge: spec_sponge_new(SHAKE128_RATE) });
}

export function warp_uni_at_one(cap: bigint, poly: WarpUniDyn): Galois128
{
  return poly.$fevaluations[Number(1n)];
}

export function warp_uni_at_zero(cap: bigint, poly: WarpUniDyn): Galois128
{
  return poly.$fevaluations[Number(0n)];
}

export function warp_uni_evaluate(cap: bigint, poly: WarpUniDyn, x: Galois128): Galois128
{
  const n = BigInt(poly.$fcount);
  let out = new Galois128(0n);
  for (let i = 0n; i < cap; i += 1n)   {
    if ((i < n))     {
      let numerator = new Galois128(1n);
      let denominator = new Galois128(1n);
      for (let j = 0n; j < cap; j += 1n)       {
        if (((j < n) && !__equals(i, j)))         {
          numerator = fieldMul(numerator, fieldSub(x, new Galois128((j as unknown as bigint))));
          denominator = fieldMul(denominator, fieldSub(new Galois128((i as unknown as bigint)), new Galois128((j as unknown as bigint))));
        }
      }
      out = fieldAdd(out, fieldMul(fieldMul(poly.$fevaluations[Number(i)], numerator), warp_inv(denominator)));
    }
  }
  return out;
}

export function warp_variable_count(m: bigint, terms: bigint, deg: bigint, pesat: WarpIndexDyn): bigint
{
  return fieldAdd(pesat.$fpublic_len, pesat.$fwitness_len);
}

export function warp_verify_spec(m: bigint, terms: bigint, deg: bigint, x: bigint, fresh: bigint, a: bigint, ell: bigint, s: bigint, shifts: bigint, r: bigint, sc: bigint, tw: bigint, fsd1: bigint, n: bigint, beta: bigint, index: WarpIndexDyn, fresh_publics: Galois128[][], input_accumulators: WarpAccInstanceDyn[], output: WarpAccInstanceDyn, proof: WarpProofDyn): boolean
{
  if (!warp_index_is_valid(m, terms, deg, index))   {
    return false;
  }
  const log_m = warp_log_constraints(m, terms, deg, index);
  const log_ell = log2_pow2(ell);
  const log_n = log2_pow2(n);
  if ((((((((((__equals(fresh, 0n) || !is_pow2(ell)) || !__equals(ell, fieldAdd(fresh, a))) || !is_pow2(r)) || !__equals(r, fieldAdd(fieldAdd(1n, s), shifts))) || !__equals(tw, fieldAdd(log_ell, 1n))) || !__equals(beta, fieldAdd(index.$fpublic_len, log_m))) || !__equals(x, index.$fpublic_len)) || !__equals(n, fieldShl(1n, log2_pow2(n)))) || !__equals(fsd1, fieldAdd(2n, BigInt(Math.max(Number(fieldAdd(log_n, 1n)), Number(fieldAdd(log_m, index.$fdegree_bound))))))))   {
    return false;
  }
  let transcript = warp_transcript_for(index, fresh_publics, input_accumulators);
  transcript.absorb_len_prefixed(n, proof.$finitial_root);
  const taus: Galois128[][] = transcript.squeeze_field_grid(r);
  transcript.absorb_fields(n, proof.$ffresh_mus);
  let alphas = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let mus = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  let betas = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let etas = Array.from({length: Number(ell - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let i = 0n; i < fresh; i += 1n)   {
    mus[Number(i)] = proof.$ffresh_mus[Number(i)];
    let beta_i = Array.from({length: Number(beta - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
    for (let j = 0n; j < x; j += 1n)     {
      beta_i[Number(j)] = fresh_publics[Number(i)][Number(j)];
    }
    for (let j = 0n; j < m; j += 1n)     {
      if ((fieldShl(1n, j) < m))       {
        beta_i[Number(fieldAdd(x, j))] = taus[Number(i)][Number(j)];
      }
    }
    betas[Number(i)] = beta_i;
    etas[Number(i)] = new Galois128(0n);
  }
  for (let i = 0n; i < a; i += 1n)   {
    const acc = input_accumulators[Number(i)];
    alphas[Number(fieldAdd(fresh, i))] = __clone(acc.$falpha);
    mus[Number(fieldAdd(fresh, i))] = acc.$fmu;
    betas[Number(fieldAdd(fresh, i))] = __clone(acc.$fbeta);
    etas[Number(fieldAdd(fresh, i))] = acc.$feta;
  }
  const tau_omega: Galois128[] = transcript.squeeze_fields(n);
  const tau = Array.from({length: Number(ilog2(ell) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((i: any) => tau_omega[Number(i)]);
  const omega = tau_omega[Number(fieldSub(tw, 1n))];
  let gamma = Array.from({length: Number(ilog2(ell) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let round = 0n; round < ell; round += 1n)   {
    if ((fieldShl(1n, round) < ell))     {
      const poly = proof.$ffirst_sumcheck[Number(round)];
      transcript.absorb_poly(sc, poly.$fcnt, poly);
      gamma[Number(round)] = transcript.squeeze_fields(n)[Number(0n)];
    }
  }
  const sigma1 = (() => {
  const eq_tau = warp_eq_evals(tau);
  let out = new Galois128(0n);
  for (let i = 0n; i < ell; i += 1n)   {
    out = fieldAdd(out, fieldMul(eq_tau[Number(i)], fieldAdd(mus[Number(i)], fieldMul(omega, etas[Number(i)]))));
  }
  return out;
})();
  const zeta0 = warp_combine_vectors(alphas, gamma);
  const beta_1 = warp_combine_vectors(betas, gamma);
  if (!__equals(beta_1, output.$fbeta))   {
    return false;
  }
  if (!warp_check_sumcheck(proof.$ffirst_sumcheck, sigma1, gamma, (() => {
  return fieldMul(warp_eq_scalar(tau, gamma), fieldAdd(proof.$fnu0, fieldMul(omega, output.$feta)));
})()))   {
    return false;
  }
  transcript.absorb_len_prefixed(n, output.$froot);
  transcript.absorb_field(proof.$fnu0);
  transcript.absorb_field(output.$feta);
  const ood_points: Galois128[][] = transcript.squeeze_field_grid(r);
  transcript.absorb_fields(n, proof.$food_answers);
  const [shift_indices, xi]: [bigint[], Galois128[]] = transcript.squeeze_shift_and_xi(shifts);
  for (let query = 0n; query < shifts; query += 1n)   {
    const path = proof.$finitial_paths[Number(query)];
    const row = proof.$fopenings[Number(query)];
    const leaf = warp_hash_fresh_row(ell, fresh, row);
    if (!warp_merkle_verify_hashed(proof.$finitial_root, leaf, BigInt(shift_indices[Number(query)]), path))     {
      return false;
    }
  }
  for (let accumulator_index = 0n; accumulator_index < a; accumulator_index += 1n)   {
    for (let query = 0n; query < shifts; query += 1n)     {
      const path = proof.$faccumulator_paths[Number(accumulator_index)][Number(query)];
      const value = proof.$fopenings[Number(query)][Number(fieldAdd(fresh, accumulator_index))];
      const leaf = warp_hash_leaf_field(value);
      if (!warp_merkle_verify_hashed(input_accumulators[Number(accumulator_index)].$froot, leaf, BigInt(shift_indices[Number(query)]), path))       {
        return false;
      }
    }
  }
  let claim_points = Array.from({length: Number(r - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => [] as any[]);
  let claim_values = Array.from({length: Number(r - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  claim_points[Number(0n)] = zeta0;
  claim_values[Number(0n)] = proof.$fnu0;
  for (let i = 0n; i < s; i += 1n)   {
    claim_points[Number(fieldAdd(1n, i))] = __clone(ood_points[Number(i)]);
    claim_values[Number(fieldAdd(1n, i))] = proof.$food_answers[Number(i)];
  }
  const eq_gamma = warp_eq_evals(gamma);
  for (let i = 0n; i < shifts; i += 1n)   {
    claim_points[Number(fieldAdd(fieldAdd(1n, s), i))] = warp_binary_point(BigInt(shift_indices[Number(i)]));
    const row = proof.$fopenings[Number(i)];
    let value = new Galois128(0n);
    for (let j = 0n; j < ell; j += 1n)     {
      value = fieldAdd(value, fieldMul(row[Number(j)], eq_gamma[Number(j)]));
    }
    claim_values[Number(fieldAdd(fieldAdd(1n, s), i))] = value;
  }
  const sigma2 = (() => {
  const eq_xi = warp_eq_evals(xi);
  let out = new Galois128(0n);
  for (let i = 0n; i < r; i += 1n)   {
    out = fieldAdd(out, fieldMul(claim_values[Number(i)], eq_xi[Number(i)]));
  }
  return out;
})();
  let alpha = Array.from({length: Number(ilog2(n) - 0n)}, (_, __i) => BigInt(__i) + 0n).map((_: any) => Galois128.default());
  for (let round = 0n; round < n; round += 1n)   {
    if ((fieldShl(1n, round) < n))     {
      const poly = proof.$fsecond_sumcheck[Number(round)];
      transcript.absorb_poly(sc, poly.$fcnt, poly);
      alpha[Number(round)] = transcript.squeeze_fields(n)[Number(0n)];
    }
  }
  if (!__equals(alpha, output.$falpha))   {
    return false;
  }
  return warp_check_sumcheck(proof.$fsecond_sumcheck, sigma2, alpha, (() => {
  const eq = warp_eq_star_at(claim_points, xi, output.$falpha);
  return fieldMul(output.$fmu, eq);
})());
}

export function wire_delta(log_q_lwe: bigint, k_max: bigint): bigint
{
  return fieldShl(1n, fieldSub(fieldSub(log_q_lwe, 1n), BigInt(k_max)));
}

export function xor_block(...__args: any[]): any {
  if (__args.length === 2) {
    const a = __args[0];
    const b = __args[1];
    return (() => {
  let o = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
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
  let o = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
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
  let o = Array.from({length: Number(KAPPA_BYTES)}, () => ((0n) & 0xFFn));
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
  let o = Array.from({length: Number(16n)}, () => ((0n) & 0xFFn));
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

export function xor_rows(delta_ct: any, d: boolean, p: bigint): RowCt[]
{
  const m: bigint = (() => { if (d) {
  return -1n;
} else {
  return 1n;
} })();
  return [scale_row(delta_ct, m, p), scale_row(delta_ct, m, p), scale_row(delta_ct, fieldMul(-2n, m), p)];
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

export function five(correction: boolean): any
{
  const s = BigInt(correction);
  const pattern = fieldBitor(fieldBitor(fieldBitor(fieldShl(s, 1n), fieldShl(BigInt(fieldBitand(!correction, true)), 2n)), fieldShl(s, 3n)), fieldShl(BigInt(fieldBitand(!correction, true)), 4n));
  return FiveBits(fieldBitor(s, pattern));
}

export function from(...__args: any[]): any {
  if (__args.length === 1) {
    const e = __args[0];
    return (() => {
  return (() => { const __match = e; if (true) { return InvalidParameters; } else if (true) { return LengthMismatch; } else if (true) { return new Error_Arithmetic(); } else { return new Error_Randomness(); } })();
})();
  } else
  if (__args.length === 1) {
    const e = __args[0];
    return (() => {
  return Crypto(e);
})();
  }
  throw new Error("from(): no matching variant for " + __args.length + " args");
}

export function from_ternary(coeffs: bigint[], p: bigint): any
{
  let c: Vec<bigint> = coeffs.map((v: any) => (() => { if ((v < 0n)) {
  return fieldSub(p, 1n);
} else {
  return (BigInt(v) % p);
} })());
  if ((c.length === 0))   {
    (c).push(1n);
  }
  c[Number(0n)] = 1n;
  return new Self({ $fcoeffs: c, $fp: p });
}

export function neg(): LabelPoly
{
  return this.$fcoeffs.map((c: any) => (() => { if (__equals(c, 0n)) {
  return 0n;
} else {
  return fieldSub(this.$fp, c);
} })());
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

export function slots(): bigint[]
{
  return this.$fslots;
}

export function try_from(value: bigint): Result<any, FrameError>
{
  return (() => { const __match = value; if (__match === 1n) { return PublicParameters; } else if (__match === 2n) { return new Stage_ReusableCiphertext(); } else if (__match === 3n) { return new Stage_PerUseCiphertext(); } else if (__match === 4n) { return SelectionKey; } else if (__match === 5n) { return new Stage_Complete(); } else if (__match === 6n) { return new Stage_Error(); } else { return new FrameError_UnknownStage(); } })();
}

