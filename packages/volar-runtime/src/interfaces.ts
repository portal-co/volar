// volar-runtime/src/interfaces.ts
// TypeScript interfaces for external crypto traits

/**
 * Something that can be cloned (deep copy).
 */
export interface Cloneable {
  clone(): Cloneable;
}

/**
 * A field element that supports arithmetic operations.
 * All methods return the same concrete type (FieldElement).
 */
export interface FieldElement extends Cloneable {
  add(rhs: FieldElement): FieldElement;
  sub(rhs: FieldElement): FieldElement;
  mul(rhs: FieldElement): FieldElement;
  bitxor(rhs: FieldElement): FieldElement;
  bitor(rhs: FieldElement): FieldElement;
  bitand(rhs: FieldElement): FieldElement;
  shl(n: number): FieldElement;
  shr(n: number): FieldElement;
  into<T>(): T;
}

/**
 * Block cipher encryption (corresponds to Rust's `cipher::BlockEncrypt`).
 */
export interface BlockEncrypt {
  readonly blockSize: number;
  encryptBlock(block: Uint8Array): Uint8Array;
}

/**
 * Cryptographic hash function (corresponds to Rust's `digest::Digest`).
 */
export interface Digest {
  readonly outputSize: number;
  update(data: Uint8Array | readonly number[]): void;
  finalize(): Uint8Array;
}

/**
 * Digest constructor — used where Rust does `D::new()`.
 */
export interface DigestConstructor {
  new(): Digest;
}

/**
 * Length-doubling PRG (corresponds to Rust's `LengthDoubler` trait).
 */
export interface LengthDoubler {
  readonly outputSize: number;
  double(input: Uint8Array): [Uint8Array, Uint8Array];
}
