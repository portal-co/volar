// volar-runtime/src/helpers.ts
// Runtime helper functions for the generated TypeScript code

import type { FieldElement } from "./interfaces";

/**
 * Integer log2 (floor).
 */
export function ilog2(x: bigint | number): bigint {
  const n = typeof x === "bigint" ? Number(x) : x;
  if (n <= 0) return 0n;
  return BigInt(Math.floor(Math.log2(n)));
}

// ============================================================================
// Wrapping arithmetic (u32 range)
// ============================================================================

export function wrappingAdd(a: bigint | number, b: bigint | number): bigint {
  const x = typeof a === "bigint" ? a : BigInt(a);
  const y = typeof b === "bigint" ? b : BigInt(b);
  return BigInt((Number(x + y)) >>> 0);
}

export function wrappingSub(a: bigint | number, b: bigint | number): bigint {
  const x = typeof a === "bigint" ? Number(a) : a;
  const y = typeof b === "bigint" ? Number(b) : b;
  return BigInt((x - y + 0x100000000) >>> 0);
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
  // Coerce to bigint when mixed — shift amount must be bigint for bigint operands
  if ((typeof a === "bigint" || typeof a === "number") && (typeof b === "bigint" || typeof b === "number")) {
    const ab = typeof a === "bigint" ? a : BigInt(a);
    const bb = typeof b === "bigint" ? b : BigInt(b);
    return ab << bb;
  }
  if (isFieldElement(a)) return a.shl(typeof b === "bigint" ? Number(b) : b);
  throw new Error(`fieldShl: unsupported types ${typeof a}, ${typeof b}`);
}

export function fieldShr(a: any, b: any): any {
  if (typeof a === "number" && typeof b === "number") return a >> b;
  if ((typeof a === "bigint" || typeof a === "number") && (typeof b === "bigint" || typeof b === "number")) {
    const ab = typeof a === "bigint" ? a : BigInt(a);
    const bb = typeof b === "bigint" ? b : BigInt(b);
    return ab >> bb;
  }
  if (isFieldElement(a)) return a.shr(typeof b === "bigint" ? Number(b) : b);
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

export function asRefU8(x: unknown): bigint[] {
  if (Array.isArray(x)) return x.map(v => typeof v === "bigint" ? v : BigInt(v as number));
  if (x instanceof Uint8Array) return Array.from(x, v => BigInt(v));
  if (typeof x === "object" && x !== null && "value" in x) {
    return asRefU8((x as { value: unknown }).value);
  }
  throw new Error(`asRefU8: cannot convert ${typeof x}`);
}

// Rust-style integer byte conversions (little-endian).
export function u32_from_le_bytes(b: bigint[]): bigint {
  return (b[0] & 0xFFn) | ((b[1] & 0xFFn) << 8n) | ((b[2] & 0xFFn) << 16n) | ((b[3] & 0xFFn) << 24n);
}

export function u64_from_le_bytes(b: bigint[]): bigint {
  let v = 0n;
  for (let i = 0; i < 8; i++) v |= ((b[i] ?? 0n) & 0xFFn) << BigInt(i * 8);
  return v;
}

export function u128_from_le_bytes(b: bigint[]): bigint {
  let v = 0n;
  for (let i = 0; i < 16; i++) v |= ((b[i] ?? 0n) & 0xFFn) << BigInt(i * 8);
  return v;
}
