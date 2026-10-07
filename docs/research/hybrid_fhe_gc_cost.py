#!/usr/bin/env python3
"""Arithmetic scenario model for the proposed FHE/garbled-circuit hybrid.

This is *not* an FHE parameter generator or security estimator.  It compares
explicitly supplied boundary-circuit costs against a long Boolean computation.
All FHE ciphertext, key, and runtime values must come from the chosen provider's
measured profile.  See hybrid-private-fhe-gc-plan.md.
"""

from __future__ import annotations

import argparse

MIB = 1024 * 1024
KIB = 1024


def fmt(value: float) -> str:
    if value >= MIB:
        return f"{value / MIB:.2f} MiB"
    if value >= KIB:
        return f"{value / KIB:.2f} KiB"
    return f"{value:.0f} B"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--long-gc-ands", type=int, default=124_160,
                        help="Boolean AND count of computation deferred to FHE")
    parser.add_argument("--gc-encrypt-ands", type=int, default=0,
                        help="measured AND count of any required GC FHE-encrypt boundary")
    parser.add_argument("--gc-decrypt-ands", type=int, default=0,
                        help="measured AND count of any required GC FHE-decrypt boundary")
    parser.add_argument("--label-bytes", type=int, default=16)
    parser.add_argument("--garbled-table-rows", type=int, default=4,
                        help="actual strict-table row count for the selected GC backend")
    parser.add_argument("--fhe-input-ciphertexts", type=int, default=0)
    parser.add_argument("--fhe-output-ciphertexts", type=int, default=0)
    parser.add_argument("--fhe-ciphertext-bytes", type=int, default=0,
                        help="measured canonical ciphertext frame bytes; 0 means unknown")
    parser.add_argument("--fhe-evaluation-key-bytes", type=int, default=0,
                        help="measured public evaluation/bootstrapping key bytes")
    parser.add_argument("--garbled-fhe-ciphertexts", type=int, default=0,
                        help="number of full FHE ciphertexts injected as GC bit-label inputs")
    parser.add_argument("--fhe-eval-ms", type=float, default=0.0,
                        help="measured provider evaluation latency; 0 means unknown")
    args = parser.parse_args()

    values = vars(args)
    if any(value < 0 for value in values.values() if isinstance(value, (int, float))):
        raise SystemExit("all scenario inputs must be non-negative")

    table_per_and = args.garbled_table_rows * args.label_bytes
    all_gc = args.long_gc_ands * table_per_and
    boundary_ands = args.gc_encrypt_ands + args.gc_decrypt_ands
    boundary_gc = boundary_ands * table_per_and
    ciphertext_total = (args.fhe_input_ciphertexts + args.fhe_output_ciphertexts) * args.fhe_ciphertext_bytes
    garbled_ciphertext_labels = (
        args.garbled_fhe_ciphertexts * args.fhe_ciphertext_bytes * 8 * args.label_bytes
    )
    hybrid_total = boundary_gc + ciphertext_total + args.fhe_evaluation_key_bytes

    print("Hybrid FHE/GC scenario arithmetic — NOT a parameter or security estimate")
    print(f"GC table model: {args.garbled_table_rows} rows × {args.label_bytes} label bytes = {table_per_and} B/AND")
    print()
    print("Current all-GC computation")
    print(f"  long computation: {args.long_gc_ands:,} ANDs -> {fmt(all_gc)} table payload")
    print()
    print("Proposed FHE boundary")
    print(f"  GC encrypt/decrypt boundary: {boundary_ands:,} ANDs -> {fmt(boundary_gc)} table payload")
    if args.fhe_ciphertext_bytes:
        print(f"  FHE input/output ciphertext frames: {args.fhe_input_ciphertexts + args.fhe_output_ciphertexts:,} -> {fmt(ciphertext_total)}")
    else:
        print("  FHE input/output ciphertext frames: UNKNOWN (supply --fhe-ciphertext-bytes)")
    if args.garbled_fhe_ciphertexts:
        if args.fhe_ciphertext_bytes:
            print(f"  GC selected labels for full FHE ciphertexts: {fmt(garbled_ciphertext_labels)}")
        else:
            print("  GC selected labels for full FHE ciphertexts: UNKNOWN (supply --fhe-ciphertext-bytes)")
    if args.fhe_evaluation_key_bytes:
        print(f"  FHE evaluation/bootstrapping key: {fmt(args.fhe_evaluation_key_bytes)}")
    else:
        print("  FHE evaluation/bootstrapping key: UNKNOWN")
    print(f"  accounted hybrid bytes: {fmt(hybrid_total)}")
    if args.fhe_eval_ms:
        print(f"  provider evaluation latency: {args.fhe_eval_ms:.3f} ms (external measured input)")
    else:
        print("  provider evaluation latency: UNKNOWN")
    print()
    print("Interpretation")
    print("  The hybrid is only favorable if its measured key + ciphertext + FHE evaluation costs")
    print("  meet the product objective. It does not inherit the all-GC table cost for deferred")
    print("  computation, but it must pay every FHE artifact and any required GC key boundary.")
    print("  Passing an entire FHE ciphertext through GC labels can itself dominate; keep FHE")
    print("  ciphertexts provider-local unless a measured decryption boundary requires otherwise.")


if __name__ == "__main__":
    main()
