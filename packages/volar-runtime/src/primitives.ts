// volar-runtime/src/primitives.ts
// Field element classes corresponding to volar-primitives in Rust

import type { FieldElement } from "./interfaces";

/**
 * Single-bit field element (GF(2)).
 */
export class Bit implements FieldElement {
  constructor(public readonly value: boolean) {}

  add(rhs: any): Bit { return new Bit(this.value !== (rhs as Bit).value); }
  sub(rhs: any): Bit { return this.add(rhs); }
  mul(rhs: any): Bit { return new Bit(this.value && (rhs as Bit).value); }
  bitxor(rhs: any): Bit { return new Bit(this.value !== (rhs as Bit).value); }
  bitor(rhs: any): Bit { return new Bit(this.value || (rhs as Bit).value); }
  bitand(rhs: any): Bit { return new Bit(this.value && (rhs as Bit).value); }
  shl(_n: number): Bit { return new Bit(false); }
  shr(_n: number): Bit { return new Bit(false); }
  clone(): Bit { return new Bit(this.value); }
  into<T>(): T { return (this.value ? 1 : 0) as unknown as T; }

  static default(): Bit { return new Bit(false); }
}

/**
 * GF(2^8) Galois field element (used in AES, etc.).
 */
export class Galois implements FieldElement {
  constructor(public readonly value: number) {}

  add(rhs: any): Galois { return new Galois((this.value ^ (rhs as Galois).value) & 0xFF); }
  sub(rhs: any): Galois { return this.add(rhs); }
  mul(rhs: any): Galois { return new Galois(gf256Mul(this.value, (rhs as Galois).value)); }
  bitxor(rhs: any): Galois { return new Galois((this.value ^ (rhs as Galois).value) & 0xFF); }
  bitor(rhs: any): Galois { return new Galois((this.value | (rhs as Galois).value) & 0xFF); }
  bitand(rhs: any): Galois { return new Galois((this.value & (rhs as Galois).value) & 0xFF); }
  shl(n: number): Galois { return new Galois((this.value << n) & 0xFF); }
  shr(n: number): Galois { return new Galois((this.value >>> n) & 0xFF); }
  clone(): Galois { return new Galois(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): Galois { return new Galois(0); }
}

/**
 * 64-bit Galois field element.
 */
export class Galois64 implements FieldElement {
  constructor(public readonly value: bigint) {}

  add(rhs: any): Galois64 { return new Galois64(this.value ^ (rhs as Galois64).value); }
  sub(rhs: any): Galois64 { return this.add(rhs); }
  mul(rhs: any): Galois64 { return new Galois64(gf64Mul(this.value, (rhs as Galois64).value)); }
  bitxor(rhs: any): Galois64 { return new Galois64(this.value ^ (rhs as Galois64).value); }
  bitor(rhs: any): Galois64 { return new Galois64(this.value | (rhs as Galois64).value); }
  bitand(rhs: any): Galois64 { return new Galois64(this.value & (rhs as Galois64).value); }
  shl(n: number): Galois64 { return new Galois64(this.value << BigInt(n)); }
  shr(n: number): Galois64 { return new Galois64(this.value >> BigInt(n)); }
  clone(): Galois64 { return new Galois64(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): Galois64 { return new Galois64(0n); }
}

/**
 * 8-bit packed bits.
 */
export class BitsInBytes implements FieldElement {
  constructor(public readonly value: number) {}

  add(rhs: any): BitsInBytes { return new BitsInBytes((this.value ^ (rhs as BitsInBytes).value) & 0xFF); }
  sub(rhs: any): BitsInBytes { return this.add(rhs); }
  mul(rhs: any): BitsInBytes { return new BitsInBytes((this.value & (rhs as BitsInBytes).value) & 0xFF); }
  bitxor(rhs: any): BitsInBytes { return new BitsInBytes((this.value ^ (rhs as BitsInBytes).value) & 0xFF); }
  bitor(rhs: any): BitsInBytes { return new BitsInBytes((this.value | (rhs as BitsInBytes).value) & 0xFF); }
  bitand(rhs: any): BitsInBytes { return new BitsInBytes((this.value & (rhs as BitsInBytes).value) & 0xFF); }
  shl(n: number): BitsInBytes { return new BitsInBytes((this.value << n) & 0xFF); }
  shr(n: number): BitsInBytes { return new BitsInBytes((this.value >>> n) & 0xFF); }
  clone(): BitsInBytes { return new BitsInBytes(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): BitsInBytes { return new BitsInBytes(0); }
}

/**
 * 64-bit packed bits.
 */
export class BitsInBytes64 implements FieldElement {
  constructor(public readonly value: bigint) {}

  add(rhs: any): BitsInBytes64 { return new BitsInBytes64(this.value ^ (rhs as BitsInBytes64).value); }
  sub(rhs: any): BitsInBytes64 { return this.add(rhs); }
  mul(rhs: any): BitsInBytes64 { return new BitsInBytes64(this.value & (rhs as BitsInBytes64).value); }
  bitxor(rhs: any): BitsInBytes64 { return new BitsInBytes64(this.value ^ (rhs as BitsInBytes64).value); }
  bitor(rhs: any): BitsInBytes64 { return new BitsInBytes64(this.value | (rhs as BitsInBytes64).value); }
  bitand(rhs: any): BitsInBytes64 { return new BitsInBytes64(this.value & (rhs as BitsInBytes64).value); }
  shl(n: number): BitsInBytes64 { return new BitsInBytes64(this.value << BigInt(n)); }
  shr(n: number): BitsInBytes64 { return new BitsInBytes64(this.value >> BigInt(n)); }
  clone(): BitsInBytes64 { return new BitsInBytes64(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): BitsInBytes64 { return new BitsInBytes64(0n); }
}

/**
 * GF(3) field element — mod-3 integer arithmetic.
 *
 * `value` is 0, 1, or 2.  `Z3(2)` is the `None` sentinel reserved for the
 * future `Bumped<Bit>` optional-trit system.
 *
 * GF(2)→GF(3) Möbius-lifted bitwise operations (agree with GF(2) on {0,1}):
 *   bitxor(a, b) = a + b + a·b  mod 3
 *   bitor(a,  b) = a + b + 2·a·b mod 3
 *   bitand(a, b) = a·b           mod 3
 */
export class Z3 implements FieldElement {
  constructor(public readonly value: number) {}

  private static add3(a: number, b: number): number {
    const s = a + b;
    return s >= 3 ? s - 3 : s;
  }
  private static neg3(a: number): number {
    return a === 0 ? 0 : 3 - a;
  }
  private static mul3(a: number, b: number): number {
    const p = a * b;
    return p >= 3 ? p - 3 : p;
  }

  add(rhs: any): Z3 { return new Z3(Z3.add3(this.value, (rhs as Z3).value)); }
  sub(rhs: any): Z3 { return new Z3(Z3.add3(this.value, Z3.neg3((rhs as Z3).value))); }
  mul(rhs: any): Z3 { return new Z3(Z3.mul3(this.value, (rhs as Z3).value)); }

  bitxor(rhs: any): Z3 {
    const a = this.value, b = (rhs as Z3).value;
    // a + b + a·b  mod 3
    return new Z3(Z3.add3(Z3.add3(a, b), Z3.mul3(a, b)));
  }
  bitor(rhs: any): Z3 {
    const a = this.value, b = (rhs as Z3).value;
    // a + b + 2·a·b  mod 3
    const ab = Z3.mul3(a, b);
    return new Z3(Z3.add3(Z3.add3(a, b), Z3.add3(ab, ab)));
  }
  bitand(rhs: any): Z3 { return new Z3(Z3.mul3(this.value, (rhs as Z3).value)); }

  // Shift has no meaningful ternary interpretation; return zero per spec.
  shl(_n: number): Z3 { return new Z3(0); }
  shr(_n: number): Z3 { return new Z3(0); }

  clone(): Z3 { return new Z3(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): Z3 { return new Z3(0); }
}

// ============================================================================
// GF(2^8) multiplication
// ============================================================================

const GF256_POLY = 0x11B;

function gf256Mul(a: number, b: number): number {
  let result = 0;
  let aa = a & 0xFF;
  let bb = b & 0xFF;
  for (let i = 0; i < 8; i++) {
    if (bb & 1) result ^= aa;
    const carry = aa & 0x80;
    aa = (aa << 1) & 0xFF;
    if (carry) aa ^= GF256_POLY & 0xFF;
    bb >>>= 1;
  }
  return result & 0xFF;
}

function gf64Mul(a: bigint, b: bigint): bigint {
  let result = 0n;
  let aa = a;
  let bb = b;
  for (let i = 0; i < 64; i++) {
    if (bb & 1n) result ^= aa;
    aa <<= 1n;
    bb >>= 1n;
  }
  return result;
}
