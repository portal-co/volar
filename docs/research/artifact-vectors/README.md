# BitGC artifact conformance vectors (QEMU, software emulation)

The authors' artifact [`wtd2/more-efficient-bitgc`](https://github.com/wtd2/more-efficient-bitgc)
built and run under a QEMU Linux guest (repo policy: Linux coverage via
software-emulated QEMU only, no HVF), producing the `--check=wire`
conformance dumps for BITGC-LEDGER-01 (small) and BITGC-LEDGER-02 (fast).

- `sub64-small-wire.txt` — `--variant=small --check=wire` on
  `circuits/sub64.txt` (low-memory correctness profile
  `--offline-dim=9 --online-dim=8`). Ends with
  `[CHECK] Output and wire-level invariants verified`.
- `sub64-fast-wire.txt` — same for `--variant=fast`.

These are the **conformance oracle**: the timing/size breakdowns and the
per-gate CT/ExtMul/HSS-DD accounting, plus the explicit wire-level
invariant check, against which `volar_spec::bitgc` (the `pinned` +
`lattice_dd` layer) is to be differentially tested.

## Reproducing (guest build recipe)

Host: macOS (Apple Silicon), Homebrew `qemu`. Software emulation (TCG),
user-mode networking, no HVF.

1. Base image: Ubuntu 22.04 cloudimg
   (`jammy-server-cloudimg-amd64.img`). The Debian 12 nocloud image ships
   **no cloud-init**, so it cannot be bootstrapped — use Ubuntu.
2. Boot with a `cidata` ISO seed (xorriso) whose `user-data` uses
   `write_files` to drop a build script + a single `runcmd` entry to run
   it. (Multi-line `runcmd` shell strings with redirects/pipes fail
   cloud-init schema validation on 22.04's cloud-init 26.1.)
3. Dependencies:
   - **HEXL** (`intel/hexl`, HEAD) — builds clean.
   - **SEAL 4.1.2** (`microsoft/SEAL` `v4.1.2`) with `-DSEAL_USE_INTEL_HEXL=ON`.
   - **emp-tool** (`emp-toolkit/emp-tool`) pinned to commit **`51dc5af`**
     (2026-04-29) — the last commit with the `emp-tool/utils/prg.h`
     include layout the artifact expects. Later commits (from `795d11e`,
     2026-04-29) reorganize to `emp-tool/crypto/` and break the artifact's
     includes. Needs **CMake ≥ 3.25** (install via `pip3 install cmake`;
     jammy's apt cmake is 3.22). Build with `-DEMP_TOOL_BUILD_TESTS=OFF`
     (emp-tool's tests fail to compile under TCG/GCC-11; the library is
     unaffected).
   - Build the artifact with
     `-DCMAKE_CXX_FLAGS="-march=x86-64-v3 -mavx2 -maes"` (the default
     flags miss SSSE3, failing on `_mm_shuffle_epi8`; `-cpu max` under TCG
     supplies the emulated ISA).
4. Wait for network before cloning (`getent hosts github.com` loop) —
   `runcmd` can fire before network-online under TCG.

The full guest workspace (base image, seed ISO, `build-script.sh`) is
staged at `/tmp/bitgc-qemu` (ephemeral; regenerate from this recipe if
needed).

## Status

BITGC-LEDGER-01/02: conformance vectors **captured** (this directory).
The differential test wiring `volar_spec::bitgc` against these numbers is
the remaining step.
