#!/usr/bin/env python3
"""Scenario estimator for heavyweight/reusable garbling research.

This is deliberately arithmetic, not a cryptographic parameter generator.  It
makes no security claim and refuses to hide unknown poly(lambda, depth) terms.
See heavy-lwe-garbling-options.md for source and interpretation.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass

MIB = 1024 * 1024
KIB = 1024


def fmt(n: float) -> str:
    if n >= MIB:
        return f"{n / MIB:.2f} MiB"
    if n >= KIB:
        return f"{n / KIB:.2f} KiB"
    return f"{n:.0f} B"


@dataclass(frozen=True)
class Scenario:
    name: str
    total_bytes: float
    note: str


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--and-gates", type=int, default=124_160)
    parser.add_argument("--label-bytes", type=int, default=16)
    parser.add_argument("--selected-labels", type=int, default=512)
    parser.add_argument("--reuse", type=int, default=1_000_000)
    parser.add_argument("--depth", type=int, default=256,
                        help="declared circuit-depth scenario; not measured for ERT")
    parser.add_argument("--leveled-overhead-per-depth-kib", type=float, default=0.0,
                        help="explicit scenario assumption for unknown D*poly(lambda) term")
    parser.add_argument("--fhe-lwe-ciphertext-bytes", type=int, default=2524,
                        help="(630+1)*4 structural size from Volar's legacy TFHE note")
    parser.add_argument("--pbs-per-and", type=int, default=1)
    args = parser.parse_args()

    if min(args.and_gates, args.label_bytes, args.selected_labels, args.reuse, args.depth) < 0:
        raise SystemExit("counts must be non-negative")

    g = args.and_gates
    labels = args.selected_labels
    raw_input = labels * args.label_bytes
    one_bit_gate = g / 8
    overhead = args.depth * args.leveled_overhead_per_depth_kib * KIB

    scenarios = [
        Scenario("four-row table baseline", g * 4 * args.label_bytes,
                 "current stable baseline shape; excludes framing"),
        Scenario("three-row fixed-first-row", g * 3 * args.label_bytes,
                 "current Cirrus Thumb experiment; excludes framing"),
        Scenario("ideal 1-bit-per-AND payload", one_bit_gate,
                 "research target only; excludes topology, keys, inputs, and assumptions"),
        Scenario("leveled 1-bit/AND + declared D overhead", one_bit_gate + overhead,
                 "D*poly(lambda) term is USER-SUPPLIED, not inferred from a paper"),
    ]

    print("Heavy garbling scenario estimator (not a security or deployment estimate)")
    print(f"AND gates: {g:,}; label bytes: {args.label_bytes}; selected labels: {labels:,}")
    print(f"reuse: {args.reuse:,}; depth scenario: {args.depth:,}")
    print()
    print("Garbled-program/table traffic")
    for s in scenarios:
        print(f"  {s.name:42} {fmt(s.total_bytes):>10}  {s.note}")
    print()
    print("Amortized program traffic at declared reuse")
    for s in scenarios:
        print(f"  {s.name:42} {s.total_bytes / max(1, args.reuse):10.3f} B/use")
    print()
    print("External input-label delivery")
    print(f"  raw selected labels                         {fmt(raw_input):>10}")
    print(f"  information-theoretic 1-bit/label floor     {fmt(labels / 8):>10}")
    print("  TinyLabels reference profile (not scaled to this batch):")
    print(f"    public parameters                         {fmt(34_078_720):>10}")
    print(f"    reusable ct1                              {fmt(2_550_136_832):>10}")
    print(f"    raw per-use ct2                           {fmt(33_554_432):>10}")
    print(f"    raw selection key                         {fmt(65_536):>10}")
    print("  These reference assets are not an embedded or 512-label estimate.")
    print()
    print("FHE comparison boundary")
    pbs = g * args.pbs_per_and
    materialized = pbs * args.fhe_lwe_ciphertext_bytes
    print(f"  boolean PBS operations (if one AND -> one PBS): {pbs:,}")
    print(f"  naive one-ciphertext-per-AND materialization:  {fmt(materialized)}")
    print("  This is a structural upper-bound-style comparison, not TFHE runtime,")
    print("  key size, bandwidth, or security estimate. Streaming/reuse can change it.")


if __name__ == "__main__":
    main()
