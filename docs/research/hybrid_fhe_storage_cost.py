#!/usr/bin/env python3
"""Static scenario estimator for hybrid FHE storage optimizations.

It intentionally models only public/syntactic facts: identity writes, declared
resident ciphertext forwards, and batches whose addresses are already public
and proven distinct. It does *not* estimate ORAM, FHE evaluation, or GC costs.
Those must be measured by the selected provider/physical storage adapter.
"""

from __future__ import annotations

import argparse
from math import ceil


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--reads", type=int, default=1000)
    parser.add_argument("--writes", type=int, default=1000)
    parser.add_argument("--identity-writes", type=int, default=0,
                        help="writes proven SSA-identical at the storage boundary")
    parser.add_argument("--resident-forwards", type=int, default=0,
                        help="reads forwarded as opaque resident ciphertext with no provider operation")
    parser.add_argument("--distinct-public-writes", type=int, default=0,
                        help="writes whose distinct public addresses permit grouped provider submission")
    parser.add_argument("--max-write-batch", type=int, default=1)
    args = parser.parse_args()
    if any(value < 0 for value in vars(args).values()) or args.max_write_batch == 0:
        raise SystemExit("counts must be non-negative and --max-write-batch must be positive")
    if args.identity_writes > args.writes or args.resident_forwards > args.reads:
        raise SystemExit("elisions cannot exceed their corresponding operation count")
    if args.distinct_public_writes > args.writes - args.identity_writes:
        raise SystemExit("distinct public writes cannot exceed non-elided writes")

    provider_reads = args.reads - args.resident_forwards
    provider_writes = args.writes - args.identity_writes
    grouped = ceil(args.distinct_public_writes / args.max_write_batch)
    serial_remainder = provider_writes - args.distinct_public_writes
    provider_write_submissions = grouped + serial_remainder

    print("Hybrid FHE storage static-plan estimator — NOT a privacy/correctness proof")
    print(f"logical reads: {args.reads:,}; logical writes: {args.writes:,}")
    print()
    print("Eligible static transformations")
    print(f"  opaque resident forwards:         {args.resident_forwards:,}")
    print(f"  syntactic identity write elisions:{args.identity_writes:,}")
    print(f"  distinct public writes grouped:   {args.distinct_public_writes:,}")
    print()
    print("Provider work after static planning")
    print(f"  provider reads:                   {provider_reads:,}")
    print(f"  provider writes:                  {provider_writes:,}")
    print(f"  write submissions (batch={args.max_write_batch}):       {provider_write_submissions:,}")
    print()
    print("Required proof obligations")
    print("  - identity means same SSA ciphertext value and same public storage epoch;")
    print("    plaintext equality or re-randomized ciphertext equality is insufficient.")
    print("  - grouping requires addresses already public/proven distinct without revealing")
    print("    a secret predicate; Path-ORAM root commits may still serialize.")
    print("  - opaque forwarding is legal only while no plaintext-dependent operation, validity")
    print("    check, version check, or decrypt-required consumer intervenes.")
    print("  - fixed-shape/privacy modes may deliberately retain no-op provider operations.")


if __name__ == "__main__":
    main()
