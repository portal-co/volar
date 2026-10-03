// Runtime smoke test for the width-aware wrapping-arithmetic helpers.
//
// These helpers implement Rust `wrapping_add`/`wrapping_sub`/`wrapping_neg`
// semantics for generated TypeScript. A previous version of wrappingAdd /
// wrappingSub round-tripped through `Number` and always masked at 32 bits,
// silently computing wrong results for u64/u128 operands; this test pins the
// width-aware behavior (and the older 32-bit behavior that other generated
// code still relies on) so a regression in either direction fails loudly.
//
// Run via `npm test` (after `npm run build` has produced dist/).

import { wrappingAdd, wrappingSub, wrappingNeg } from "../dist/index.js";

let failures = 0;

function check(label, got, want) {
  const ok = got === want;
  if (!ok) failures += 1;
  console.log(`${ok ? "ok" : "FAIL"} ${label}: got ${got}, want ${want}`);
}

// u8 widths (smallest case; exercises wraparound at both ends).
check("u8 add wraps", wrappingAdd(250n, 10n, 8), 4n);
check("u8 sub wraps", wrappingSub(2n, 5n, 8), 253n); // 2u8.wrapping_sub(5) == 253
check("u8 neg wraps", wrappingNeg(1n, 8), 255n);

// u32 widths (the width every call site used to be forced to).
check("u32 add wraps", wrappingAdd(0xffff_ffffn, 1n, 32), 0n);
check("u32 sub wraps", wrappingSub(0n, 1n, 32), 0xffff_ffffn);
check("u32 neg wraps", wrappingNeg(1n, 32), 0xffff_ffffn);

// u64 widths — the cases that were silently wrong before: 32-bit masking
// would turn these into 4n / 0xffff_ffffn instead of the correct values,
// and the old Number() round-trip additionally lost precision above 2^53.
const U64_MAX = 0xffff_ffff_ffff_ffffn;
check("u64 add wraps at 2^64, not 2^32", wrappingAdd(U64_MAX - 5n, 10n, 64), 4n);
check("u64 sub wraps at 2^64, not 2^32", wrappingSub(0n, 1n, 64), U64_MAX);
check("u64 neg wraps at 2^64, not 2^32", wrappingNeg(1n, 64), U64_MAX);
check(
  "u64 add preserves full width (no 32-bit truncation)",
  wrappingAdd(U64_MAX - 0xffn, 1n, 64),
  U64_MAX - 0xfen,
);
check(
  "u64 splitmix64 constant self-add (spec TestRng pattern)",
  wrappingAdd(0x9e37_79b9_7f4a_7c15n, 0x9e37_79b9_7f4a_7c15n, 64),
  (0x9e37_79b9_7f4a_7c15n * 2n) & U64_MAX,
);

// u128 widths.
const U128_MAX = (1n << 128n) - 1n;
check("u128 add wraps", wrappingAdd(U128_MAX, 1n, 128), 0n);
check("u128 sub wraps", wrappingSub(0n, 1n, 128), U128_MAX);

// number-typed inputs (callers may pass either representation).
check("number inputs", wrappingAdd(250, 10, 8), 4n);

if (failures > 0) {
  console.error(`wrapping-smoke: ${failures} failure(s)`);
  process.exit(1);
}
console.log("wrapping-smoke: all checks passed");
