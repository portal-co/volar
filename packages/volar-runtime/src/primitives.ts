// volar-runtime/src/primitives.ts
// Field element classes corresponding to volar-primitives in Rust

import type { FieldElement } from "./interfaces";

/**
 * Single-bit field element (GF(2)).
 */
export class Bit implements FieldElement {
  constructor(public readonly value: boolean) {}

  add(rhs: Bit): Bit { return new Bit(this.value !== rhs.value); }
  sub(rhs: Bit): Bit { return this.add(rhs); }
  mul(rhs: Bit): Bit { return new Bit(this.value && rhs.value); }
  bitxor(rhs: Bit): Bit { return new Bit(this.value !== rhs.value); }
  bitor(rhs: Bit): Bit { return new Bit(this.value || rhs.value); }
  bitand(rhs: Bit): Bit { return new Bit(this.value && rhs.value); }
  shl(_n: number): Bit { return new Bit(false); }
  shr(_n: number): Bit { return new Bit(false); }
  clone(): Bit { return new Bit(this.value); }
  into<T>(): T { return (this.value ? 1 : 0) as unknown as T; }

  static default(): Bit { return new Bit(false); }
}

/**
 * GF(2^8) Galois field element (used in AES, etc.).
 * Stored as a u8.
 */
export class Galois implements FieldElement {
  constructor(public readonly value: number) {}

  add(rhs: Galois): Galois { return new Galois((this.value ^ rhs.value) & 0xFF); }
  sub(rhs: Galois): Galois { return this.add(rhs); } // In GF(2^8), add == sub
  mul(rhs: Galois): Galois { return new Galois(gf256Mul(this.value, rhs.value)); }
  bitxor(rhs: Galois): Galois { return new Galois((this.value ^ rhs.value) & 0xFF); }
  bitor(rhs: Galois): Galois { return new Galois((this.value | rhs.value) & 0xFF); }
  bitand(rhs: Galois): Galois { return new Galois((this.value & rhs.value) & 0xFF); }
  shl(n: number): Galois { return new Galois((this.value << n) & 0xFF); }
  shr(n: number): Galois { return new Galois((this.value >>> n) & 0xFF); }
  clone(): Galois { return new Galois(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): Galois { return new Galois(0); }
}

/**
 * 64-bit Galois field element.
 * Stored as a BigInt.
 */
export class Galois64 implements FieldElement {
  constructor(public readonly value: bigint) {}

  add(rhs: Galois64): Galois64 { return new Galois64(this.value ^ rhs.value); }
  sub(rhs: Galois64): Galois64 { return this.add(rhs); }
  mul(rhs: Galois64): Galois64 { return new Galois64(gf64Mul(this.value, rhs.value)); }
  bitxor(rhs: Galois64): Galois64 { return new Galois64(this.value ^ rhs.value); }
  bitor(rhs: Galois64): Galois64 { return new Galois64(this.value | rhs.value); }
  bitand(rhs: Galois64): Galois64 { return new Galois64(this.value & rhs.value); }
  shl(n: number): Galois64 { return new Galois64(this.value << BigInt(n)); }
  shr(n: number): Galois64 { return new Galois64(this.value >> BigInt(n)); }
  clone(): Galois64 { return new Galois64(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): Galois64 { return new Galois64(0n); }
}

/**
 * 8-bit packed bits (SIMD-like byte packing).
 */
export class BitsInBytes implements FieldElement {
  constructor(public readonly value: number) {}

  add(rhs: BitsInBytes): BitsInBytes { return new BitsInBytes((this.value ^ rhs.value) & 0xFF); }
  sub(rhs: BitsInBytes): BitsInBytes { return this.add(rhs); }
  mul(rhs: BitsInBytes): BitsInBytes { return new BitsInBytes((this.value & rhs.value) & 0xFF); }
  bitxor(rhs: BitsInBytes): BitsInBytes { return new BitsInBytes((this.value ^ rhs.value) & 0xFF); }
  bitor(rhs: BitsInBytes): BitsInBytes { return new BitsInBytes((this.value | rhs.value) & 0xFF); }
  bitand(rhs: BitsInBytes): BitsInBytes { return new BitsInBytes((this.value & rhs.value) & 0xFF); }
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

  add(rhs: BitsInBytes64): BitsInBytes64 { return new BitsInBytes64(this.value ^ rhs.value); }
  sub(rhs: BitsInBytes64): BitsInBytes64 { return this.add(rhs); }
  mul(rhs: BitsInBytes64): BitsInBytes64 { return new BitsInBytes64(this.value & rhs.value); }
  bitxor(rhs: BitsInBytes64): BitsInBytes64 { return new BitsInBytes64(this.value ^ rhs.value); }
  bitor(rhs: BitsInBytes64): BitsInBytes64 { return new BitsInBytes64(this.value | rhs.value); }
  bitand(rhs: BitsInBytes64): BitsInBytes64 { return new BitsInBytes64(this.value & rhs.value); }
  shl(n: number): BitsInBytes64 { return new BitsInBytes64(this.value << BigInt(n)); }
  shr(n: number): BitsInBytes64 { return new BitsInBytes64(this.value >> BigInt(n)); }
  clone(): BitsInBytes64 { return new BitsInBytes64(this.value); }
  into<T>(): T { return this.value as unknown as T; }

  static default(): BitsInBytes64 { return new BitsInBytes64(0n); }
}

// ============================================================================
// GF(2^8) multiplication (carry-less multiply + reduction with AES polynomial)
// ============================================================================

const GF256_POLY = 0x11B; // x^8 + x^4 + x^3 + x + 1

function gf256Mul(a: number, b: number): number {
  let result = 0;
  let aa = a & 0xFF;
  let bb = b & 0xFF;
  for (let i = 0; i < 8; i++) {
    if (bb & 1) {
      result ^= aa;
    }
    const carry = aa & 0x80;
    aa = (aa << 1) & 0xFF;
    if (carry) {
      aa ^= GF256_POLY & 0xFF;
    }
    bb >>>= 1;
  }
  return result & 0xFF;
}

// Placeholder for 64-bit GF multiplication
function gf64Mul(a: bigint, b: bigint): bigint {
  // Carry-less multiplication in GF(2^64)
  // TODO: implement proper polynomial reduction
  let result = 0n;
  let aa = a;
  let bb = b;
  for (let i = 0; i < 64; i++) {
    if (bb & 1n) {
      result ^= aa;
    }
    aa <<= 1n;
    bb >>= 1n;
  }
  return result;
}
