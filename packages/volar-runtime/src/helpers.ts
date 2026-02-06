// volar-runtime/src/helpers.ts
// Runtime helper functions for the generated TypeScript code

import type { FieldElement } from "./interfaces";

/**
 * Integer log2 (floor).
 */
export function ilog2(x: number): number {
  if (x <= 0) return 0;
  return Math.floor(Math.log2(x));
}

// ============================================================================
// Wrapping arithmetic (u32 range)
// ============================================================================

export function wrappingAdd(a: number, b: number): number {
  return (a + b) >>> 0;
}

export function wrappingSub(a: number, b: number): number {
  return (a - b + 0x100000000) >>> 0;
}

// ============================================================================
// Field-element–aware binary operators
//
// These dispatch based on the runtime type of the operands:
// - If both are `number`, use native JS operators
// - If both are `bigint`, use native JS operators  
// - If either is a FieldElement (has .add() etc.), call the method
// ============================================================================

function isFieldElement(x: unknown): x is FieldElement {
  return typeof x === "object" && x !== null && "add" in x && typeof (x as FieldElement).add === "function";
}

export function fieldAdd<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a + b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a + b) as T;
  if (isFieldElement(a)) return a.add(b as FieldElement) as T;
  throw new Error(`fieldAdd: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldSub<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a - b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a - b) as T;
  if (isFieldElement(a)) return a.sub(b as FieldElement) as T;
  throw new Error(`fieldSub: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldMul<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a * b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a * b) as T;
  if (isFieldElement(a)) return a.mul(b as FieldElement) as T;
  throw new Error(`fieldMul: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldBitxor<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a ^ b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a ^ b) as T;
  if (isFieldElement(a)) return a.bitxor(b as FieldElement) as T;
  throw new Error(`fieldBitxor: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldBitor<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a | b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a | b) as T;
  if (isFieldElement(a)) return a.bitor(b as FieldElement) as T;
  throw new Error(`fieldBitor: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldBitand<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a & b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a & b) as T;
  if (isFieldElement(a)) return a.bitand(b as FieldElement) as T;
  throw new Error(`fieldBitand: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldShl<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a << b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a << b) as T;
  if (isFieldElement(a) && typeof b === "number") return a.shl(b) as T;
  throw new Error(`fieldShl: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldShr<T>(a: T, b: T): T {
  if (typeof a === "number" && typeof b === "number") return (a >> b) as T;
  if (typeof a === "bigint" && typeof b === "bigint") return (a >> b) as T;
  if (isFieldElement(a) && typeof b === "number") return a.shr(b) as T;
  throw new Error(`fieldShr: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldEq<T>(a: T, b: T): boolean {
  if (typeof a === "number" && typeof b === "number") return a === b;
  if (typeof a === "bigint" && typeof b === "bigint") return a === b;
  if (isFieldElement(a) && isFieldElement(b)) {
    return JSON.stringify(a) === JSON.stringify(b);
  }
  return a === b;
}

export function fieldNe<T>(a: T, b: T): boolean {
  return !fieldEq(a, b);
}

// ============================================================================
// Crypto helpers (stubs — callers must provide real implementations)
// ============================================================================

/**
 * Hash commitment: H(data || randomness).
 * Must be provided by the consumer with a real implementation.
 */
export function commit<D>(data: number[] | Uint8Array, rand: number[] | Uint8Array): Uint8Array {
  // Stub — real implementation must be injected
  const combined = new Uint8Array([
    ...(data instanceof Uint8Array ? data : new Uint8Array(data)),
    ...(rand instanceof Uint8Array ? rand : new Uint8Array(rand)),
  ]);
  // This is NOT a real commitment — just a placeholder
  return combined;
}

/**
 * Length-doubling PRG bridge.
 * Corresponds to Rust's `double_vec::<B>(v)`.
 */
export function doubleVec(v: number[]): [number[], number[]] {
  // Stub — real implementation must be injected
  const mid = Math.floor(v.length / 2);
  return [v.slice(0, mid), v.slice(mid)];
}

/**
 * AsRef<[u8]> equivalent — extract byte array from various types.
 */
export function asRefU8(x: unknown): number[] {
  if (Array.isArray(x)) return x;
  if (x instanceof Uint8Array) return Array.from(x);
  if (typeof x === "object" && x !== null && "value" in x) {
    return asRefU8((x as { value: unknown }).value);
  }
  throw new Error(`asRefU8: cannot convert ${typeof x}`);
}
