# volar

**VOLE-in-the-head computations for a cloud world.**

## Documentation

See the [`docs/`](docs/) folder for full documentation:

- [`docs/overview.md`](docs/overview.md) — Architecture, crate graph, pipelines
- [`docs/spec.md`](docs/spec.md) — `volar-spec`: ZK, MPC, garbled circuits
- [`docs/ir-lowering.md`](docs/ir-lowering.md) — Volar IR, Boolar IR, movfuscation
- [`docs/compiler.md`](docs/compiler.md) — `volar-compiler`: IR, lowering, printers
- [`docs/insecure.md`](docs/insecure.md) — `.insecure` file policy and current insecure work

## Progress

- [x] Workspace setup with spec and primitives
- [x] Basic commit-open workflow
- [x] Basic operations on commitments
- [x] The Hypercube technique for separate commitments from one commitment
- [x] Construction of commitments from constants
- [x] Compiler: parser → IR → dynamic Rust + TypeScript
- [x] Type manifest (`.volar.d`) cross-crate compilation
- [ ] Construction of commitments from a witness or secret data
- [ ] Consistency checks to augment or replace the Hypercube technique
- [ ] IR proving (only one round)
- [ ] WAFFLE to VAFFLE
- [ ] VAFFLE to Volar IR
- [ ] Volar IR movfuscation
- [ ] IR proving (many rounds)
- [ ] Succinct proofs
- [ ] Support MPC
- [ ] Support nested proving/verification

---
*AI assisted*
