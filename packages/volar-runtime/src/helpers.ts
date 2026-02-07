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
// ============================================================================

function isFieldElement(x: unknown): x is FieldElement {
  return typeof x === "object" && x !== null && "add" in x && typeof (x as any).add === "function";
}

export function fieldAdd(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a + b;
  if (typeof a === "bigint" && typeof b === "bigint") return a + b;
  if (isFieldElement(a)) return a.add(b);
  throw new Error(`fieldAdd: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldSub(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a - b;
  if (typeof a === "bigint" && typeof b === "bigint") return a - b;
  if (isFieldElement(a)) return a.sub(b);
  throw new Error(`fieldSub: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldMul(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a * b;
  if (typeof a === "bigint" && typeof b === "bigint") return a * b;
  if (isFieldElement(a)) return a.mul(b);
  throw new Error(`fieldMul: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldBitxor(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a ^ b;
  if (typeof a === "bigint" && typeof b === "bigint") return a ^ b;
  if (isFieldElement(a)) return a.bitxor(b);
  throw new Error(`fieldBitxor: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldBitor(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a | b;
  if (typeof a === "bigint" && typeof b === "bigint") return a | b;
  if (isFieldElement(a)) return a.bitor(b);
  throw new Error(`fieldBitor: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldBitand(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a & b;
  if (typeof a === "bigint" && typeof b === "bigint") return a & b;
  if (isFieldElement(a)) return a.bitand(b);
  throw new Error(`fieldBitand: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldShl(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a << b;
  if (typeof a === "bigint" && typeof b === "bigint") return a << b;
  if (isFieldElement(a) && typeof b === "number") return a.shl(b);
  throw new Error(`fieldShl: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldShr(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a >> b;
  if (typeof a === "bigint" && typeof b === "bigint") return a >> b;
  if (isFieldElement(a) && typeof b === "number") return a.shr(b);
  throw new Error(`fieldShr: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldEq(a: any, b: any): boolean {
  if (typeof a === "number" && typeof b === "number") return a === b;
  if (typeof a === "bigint" && typeof b === "bigint") return a === b;
  if (isFieldElement(a) && isFieldElement(b)) {
    return JSON.stringify(a) === JSON.stringify(b);
  }
  return a === b;
}

export function fieldNe(a: any, b: any): boolean {
  return !fieldEq(a, b);
}

// ============================================================================
// Crypto helpers (stubs — callers must provide real implementations)
// ============================================================================

export function commit(data: any, rand: any): Uint8Array {
  const combined = new Uint8Array([
    ...(data instanceof Uint8Array ? data : new Uint8Array(data)),
    ...(rand instanceof Uint8Array ? rand : new Uint8Array(rand)),
  ]);
  return combined;
}

export function doubleVec(v: any[]): [any[], any[]] {
  const mid = Math.floor(v.length / 2);
  return [v.slice(0, mid), v.slice(mid)];
}

export function asRefU8(x: unknown): number[] {
  if (Array.isArray(x)) return x;
  if (x instanceof Uint8Array) return Array.from(x);
  if (typeof x === "object" && x !== null && "value" in x) {
    return asRefU8((x as { value: unknown }).value);
  }
  throw new Error(`asRefU8: cannot convert ${typeof x}`);
}
