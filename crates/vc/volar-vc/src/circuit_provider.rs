//! Provider-neutral inline circuit seam for deferred FHE conversion programs.
//!
//! This module deliberately knows neither a ciphertext encoding nor a
//! homomorphic scheme. A provider supplies three *finite, pure Boolar
//! programs* with fixed bit geometry: split-seed key derivation, randomized
//! encryption, and decryption. [`CircuitProviderPrograms::validate`] checks
//! that contract once; [`ValidatedCircuitProvider`] then inlines calls into a
//! caller-owned fused Boolean circuit. The resulting circuit is ordinary GC
//! dataflow and can be optimized by the normal Boolar pipeline.
//!
//! Provider compilation/lowering is outside this module. An IR-origin program
//! must fully unroll with `unroll_ir_everything_unbounded` before conversion to
//! `BIrBlocks`; that rule is what prevents a bounded provider approximation
//! from entering this seam.
//!
//! # Non-goals
//!
//! This is intentionally only the call-composition seam. It does not provide
//! AES-GCM KDF construction, input ownership, random-wire provenance,
//! ciphertext caching, encrypted select, or a host crypto implementation.
//! Those policies are staged behind the provider descriptor and demand tracker
//! in `docs/fhe/circuit-provider-abi.md`.

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;
use core::convert::Infallible;

use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::circuit::{BCircuit, CircuitFusionError};
use volar_ir::ir::IRVarId;
use volar_ir_common::Node;

/// Fixed public bit geometry of one circuit-provider profile.
///
/// Labels/domain separators and profile identity are handled by the provider
/// programs themselves as constant wires. This descriptor contains only the
/// widths needed to reject accidental cross-profile wiring.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CircuitProviderGeometry {
    pub seed_half_bits: usize,
    pub key_bits: usize,
    pub plaintext_bits: usize,
    pub ciphertext_bits: usize,
    pub randomness_bits: usize,
}

/// Raw provider programs before their ABI/purity contract is checked.
#[derive(Clone, Debug)]
pub struct CircuitProviderPrograms<P: Clone> {
    /// `(seed_left, seed_right) -> key`.
    pub derive_key: BIrBlocks<P>,
    /// `(key, plaintext, randomness) -> ciphertext`.
    pub encrypt: BIrBlocks<P>,
    /// `(key, ciphertext) -> plaintext`.
    pub decrypt: BIrBlocks<P>,
}

/// Which fixed provider program failed validation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProviderProgramKind {
    DeriveKey,
    Encrypt,
    Decrypt,
}

/// Fail-closed errors at the provider-program composition seam.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CircuitProviderError {
    NotFused {
        program: ProviderProgramKind,
        source: CircuitFusionError,
    },
    HasPreinitializedStorage {
        program: ProviderProgramKind,
    },
    UnsupportedStatement {
        program: ProviderProgramKind,
    },
    InputArity {
        program: ProviderProgramKind,
        expected: usize,
        actual: usize,
    },
    OutputArity {
        program: ProviderProgramKind,
        expected: usize,
        actual: usize,
    },
    InvalidProgramWire {
        program: ProviderProgramKind,
        wire: u32,
        var_space: u32,
    },
    CallInputArity {
        program: ProviderProgramKind,
        expected: usize,
        actual: usize,
    },
    UnknownKeyUse(KeyUse),
    KeyUseSeedMismatch(KeyUse),
    CiphertextUseAlreadyUsed(CiphertextUse),
    RandomnessReused,
    UnknownCiphertextUse(CiphertextUse),
    CiphertextKeyMismatch {
        ciphertext: CiphertextUse,
        expected: KeyUse,
        actual: KeyUse,
    },
}

/// Programs validated as pure single-block circuits with declared geometry.
///
/// Keeping this type private-fielded makes the validation seam non-optional:
/// an inline call cannot be constructed from an arbitrary `BIrBlocks`.
#[derive(Clone, Debug)]
pub struct ValidatedCircuitProvider<P: Clone> {
    geometry: CircuitProviderGeometry,
    derive_key: BCircuit<P>,
    encrypt: BCircuit<P>,
    decrypt: BCircuit<P>,
}

/// A public domain-separated request for one deterministic key derivation.
///
/// It is an opaque compiler identity here. A reviewed session binding must map
/// it to the ABI's profile/epoch/circuit/key-label tuple before execution.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeyUse(pub u64);

/// A public identity for one randomized ciphertext event.
///
/// It is distinct from source wire identity: equal plaintext inputs may still
/// require distinct ciphertext events and therefore distinct randomness.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CiphertextUse(pub u64);

struct DerivedKey {
    seed_left: Vec<IRVarId>,
    seed_right: Vec<IRVarId>,
    wires: Vec<IRVarId>,
}

struct CiphertextRecord {
    key_use: KeyUse,
    wires: Vec<IRVarId>,
    decrypted: Option<Vec<IRVarId>>,
}

/// Per-combined-circuit demand state for a validated provider.
///
/// This deep module centralizes the lifetime rules that must not leak across
/// compiler callers: one `KeyUse` derives once, ciphertext uses and randomness
/// vectors are single-use, and a ciphertext decrypts once on demand. It does
/// not decide *when* a wire is first non-select/cached; the upcoming wire
/// tracker invokes these operations at that decision seam.
pub struct CircuitProviderComposition<'a, P: Clone> {
    provider: &'a ValidatedCircuitProvider<P>,
    circuit: &'a mut BCircuit<P>,
    keys: BTreeMap<KeyUse, DerivedKey>,
    ciphertexts: BTreeMap<CiphertextUse, CiphertextRecord>,
    randomness: BTreeSet<Vec<IRVarId>>,
}

impl<P: Clone> CircuitProviderPrograms<P> {
    /// Validate the fixed ABI and return programs safe to inline.
    // TODO(provider-ledger: FHE-PLUMB-CIRCUIT-ABI-01): accept a reviewed
    // IR-origin provider fixture after it has passed the unbounded unroller.
    pub fn validate(
        self,
        geometry: CircuitProviderGeometry,
    ) -> Result<ValidatedCircuitProvider<P>, CircuitProviderError> {
        let derive_key = validate_program(
            ProviderProgramKind::DeriveKey,
            self.derive_key,
            geometry.seed_half_bits.saturating_mul(2),
            geometry.key_bits,
        )?;
        let encrypt = validate_program(
            ProviderProgramKind::Encrypt,
            self.encrypt,
            geometry
                .key_bits
                .saturating_add(geometry.plaintext_bits)
                .saturating_add(geometry.randomness_bits),
            geometry.ciphertext_bits,
        )?;
        let decrypt = validate_program(
            ProviderProgramKind::Decrypt,
            self.decrypt,
            geometry.key_bits.saturating_add(geometry.ciphertext_bits),
            geometry.plaintext_bits,
        )?;
        Ok(ValidatedCircuitProvider {
            geometry,
            derive_key,
            encrypt,
            decrypt,
        })
    }
}

impl<P: Clone> ValidatedCircuitProvider<P> {
    /// The fixed profile geometry validated with these programs.
    pub const fn geometry(&self) -> CircuitProviderGeometry {
        self.geometry
    }

    /// Begin one demand-tracked composition into `circuit`.
    pub fn compose_into<'a>(
        &'a self,
        circuit: &'a mut BCircuit<P>,
    ) -> CircuitProviderComposition<'a, P> {
        CircuitProviderComposition {
            provider: self,
            circuit,
            keys: BTreeMap::new(),
            ciphertexts: BTreeMap::new(),
            randomness: BTreeSet::new(),
        }
    }

    /// Inline split-seed key derivation and return its ordinary key wires.
    pub fn derive_key(
        &self,
        into: &mut BCircuit<P>,
        seed_left: &[IRVarId],
        seed_right: &[IRVarId],
    ) -> Result<Vec<IRVarId>, CircuitProviderError> {
        let mut inputs = Vec::with_capacity(seed_left.len() + seed_right.len());
        inputs.extend_from_slice(seed_left);
        inputs.extend_from_slice(seed_right);
        append_program(
            into,
            ProviderProgramKind::DeriveKey,
            &self.derive_key,
            &inputs,
        )
    }

    /// Inline randomized encryption and return ordinary ciphertext wires.
    pub fn encrypt(
        &self,
        into: &mut BCircuit<P>,
        key: &[IRVarId],
        plaintext: &[IRVarId],
        randomness: &[IRVarId],
    ) -> Result<Vec<IRVarId>, CircuitProviderError> {
        let mut inputs = Vec::with_capacity(key.len() + plaintext.len() + randomness.len());
        inputs.extend_from_slice(key);
        inputs.extend_from_slice(plaintext);
        inputs.extend_from_slice(randomness);
        append_program(into, ProviderProgramKind::Encrypt, &self.encrypt, &inputs)
    }

    /// Inline decryption and return ordinary plaintext wires.
    pub fn decrypt(
        &self,
        into: &mut BCircuit<P>,
        key: &[IRVarId],
        ciphertext: &[IRVarId],
    ) -> Result<Vec<IRVarId>, CircuitProviderError> {
        let mut inputs = Vec::with_capacity(key.len() + ciphertext.len());
        inputs.extend_from_slice(key);
        inputs.extend_from_slice(ciphertext);
        append_program(into, ProviderProgramKind::Decrypt, &self.decrypt, &inputs)
    }
}

impl<'a, P: Clone> CircuitProviderComposition<'a, P> {
    /// Derive `key_use` once. Repeated calls with the identical split seed
    /// return the original wires; a changed seed is a fail-closed error.
    pub fn derive_key(
        &mut self,
        key_use: KeyUse,
        seed_left: &[IRVarId],
        seed_right: &[IRVarId],
    ) -> Result<Vec<IRVarId>, CircuitProviderError> {
        if let Some(existing) = self.keys.get(&key_use) {
            return if existing.seed_left == seed_left && existing.seed_right == seed_right {
                Ok(existing.wires.clone())
            } else {
                Err(CircuitProviderError::KeyUseSeedMismatch(key_use))
            };
        }
        let wires = self
            .provider
            .derive_key(self.circuit, seed_left, seed_right)?;
        self.keys.insert(
            key_use,
            DerivedKey {
                seed_left: seed_left.to_vec(),
                seed_right: seed_right.to_vec(),
                wires: wires.clone(),
            },
        );
        Ok(wires)
    }

    /// Encrypt a plaintext for a new ciphertext event using an already-derived
    /// key. Randomness is single-use even where callers mistakenly supply a
    /// different ciphertext identity.
    pub fn encrypt(
        &mut self,
        key_use: KeyUse,
        ciphertext_use: CiphertextUse,
        plaintext: &[IRVarId],
        randomness: &[IRVarId],
    ) -> Result<Vec<IRVarId>, CircuitProviderError> {
        let key = self
            .keys
            .get(&key_use)
            .ok_or(CircuitProviderError::UnknownKeyUse(key_use))?
            .wires
            .clone();
        if self.ciphertexts.contains_key(&ciphertext_use) {
            return Err(CircuitProviderError::CiphertextUseAlreadyUsed(
                ciphertext_use,
            ));
        }
        if !self.randomness.insert(randomness.to_vec()) {
            return Err(CircuitProviderError::RandomnessReused);
        }
        let wires = self
            .provider
            .encrypt(self.circuit, &key, plaintext, randomness)?;
        self.ciphertexts.insert(
            ciphertext_use,
            CiphertextRecord {
                key_use,
                wires: wires.clone(),
                decrypted: None,
            },
        );
        Ok(wires)
    }

    /// Decrypt a ciphertext at most once. All later consumers receive the
    /// exact already-inlined plaintext wires.
    pub fn decrypt(
        &mut self,
        key_use: KeyUse,
        ciphertext_use: CiphertextUse,
    ) -> Result<Vec<IRVarId>, CircuitProviderError> {
        let record = self
            .ciphertexts
            .get_mut(&ciphertext_use)
            .ok_or(CircuitProviderError::UnknownCiphertextUse(ciphertext_use))?;
        if record.key_use != key_use {
            return Err(CircuitProviderError::CiphertextKeyMismatch {
                ciphertext: ciphertext_use,
                expected: record.key_use,
                actual: key_use,
            });
        }
        if let Some(wires) = &record.decrypted {
            return Ok(wires.clone());
        }
        let key = self
            .keys
            .get(&key_use)
            .ok_or(CircuitProviderError::UnknownKeyUse(key_use))?
            .wires
            .clone();
        let plaintext = self.provider.decrypt(self.circuit, &key, &record.wires)?;
        record.decrypted = Some(plaintext.clone());
        Ok(plaintext)
    }

    /// Finish composition and recover the circuit borrowed at construction.
    pub fn into_circuit(self) -> &'a mut BCircuit<P> {
        self.circuit
    }
}

fn validate_program<P: Clone>(
    program: ProviderProgramKind,
    blocks: BIrBlocks<P>,
    expected_inputs: usize,
    expected_outputs: usize,
) -> Result<BCircuit<P>, CircuitProviderError> {
    let circuit = BCircuit::try_from_ir(&blocks)
        .map_err(|source| CircuitProviderError::NotFused { program, source })?;
    if !circuit.pre_init.is_empty() {
        return Err(CircuitProviderError::HasPreinitializedStorage { program });
    }
    if circuit.params as usize != expected_inputs {
        return Err(CircuitProviderError::InputArity {
            program,
            expected: expected_inputs,
            actual: circuit.params as usize,
        });
    }
    if circuit.outputs.len() != expected_outputs {
        return Err(CircuitProviderError::OutputArity {
            program,
            expected: expected_outputs,
            actual: circuit.outputs.len(),
        });
    }
    let var_space = circuit.var_space();
    for node in &circuit.stmts {
        if !is_pure_boolean(&node.kind) {
            return Err(CircuitProviderError::UnsupportedStatement { program });
        }
        node.kind.clone().map(
            &mut (),
            |_, wire| {
                if wire.0 >= var_space {
                    Err(CircuitProviderError::InvalidProgramWire {
                        program,
                        wire: wire.0,
                        var_space,
                    })
                } else {
                    Ok(wire)
                }
            },
            |_, storage| Ok::<_, CircuitProviderError>(storage),
        )?;
    }
    Ok(circuit)
}

fn is_pure_boolean(stmt: &BIrStmt) -> bool {
    matches!(
        stmt,
        BIrStmt::Zero
            | BIrStmt::One
            | BIrStmt::And(..)
            | BIrStmt::Or(..)
            | BIrStmt::Xor(..)
            | BIrStmt::Not(..)
    )
}

fn append_program<P: Clone>(
    into: &mut BCircuit<P>,
    program_kind: ProviderProgramKind,
    program: &BCircuit<P>,
    inputs: &[IRVarId],
) -> Result<Vec<IRVarId>, CircuitProviderError> {
    if inputs.len() != program.params as usize {
        return Err(CircuitProviderError::CallInputArity {
            program: program_kind,
            expected: program.params as usize,
            actual: inputs.len(),
        });
    }
    let parent_var_space = into.var_space();
    for node in &program.stmts {
        let remapped = node
            .kind
            .clone()
            .map(
                &mut (),
                |_, source| -> Result<IRVarId, Infallible> {
                    let source = source.0 as usize;
                    Ok(if source < program.params as usize {
                        inputs[source]
                    } else {
                        IRVarId(parent_var_space + (source - program.params as usize) as u32)
                    })
                },
                |_, storage| -> Result<_, Infallible> { Ok(storage) },
            )
            .expect("validated provider program remap is infallible");
        into.stmts
            .push(Node::new(remapped, node.prov.clone(), node.side));
    }
    Ok(program
        .outputs
        .iter()
        .map(|source| {
            let source = source.0 as usize;
            if source < program.params as usize {
                inputs[source]
            } else {
                IRVarId(parent_var_space + (source - program.params as usize) as u32)
            }
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use alloc::vec;
    use alloc::vec::Vec;

    use super::*;
    use volar_ir::boolar::{BIrBlock, BIrTarget, BIrTerminator};
    use volar_ir::ir::IRBlockTargetId;

    const GEOMETRY: CircuitProviderGeometry = CircuitProviderGeometry {
        seed_half_bits: 1,
        key_bits: 1,
        plaintext_bits: 1,
        ciphertext_bits: 1,
        randomness_bits: 1,
    };

    fn circuit(params: u32, stmts: Vec<BIrStmt>, outputs: Vec<u32>) -> BIrBlocks {
        BIrBlocks {
            blocks: vec![BIrBlock {
                params,
                stmts: stmts
                    .into_iter()
                    .map(|stmt| Node::new(stmt, (), None))
                    .collect(),
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: outputs.into_iter().map(IRVarId).collect(),
                }),
            }],
            pre_init: vec![],
        }
    }

    fn provider() -> ValidatedCircuitProvider<()> {
        CircuitProviderPrograms {
            // key = left XOR right
            derive_key: circuit(2, vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))], vec![2]),
            // ct = key XOR plaintext XOR randomness
            encrypt: circuit(
                3,
                vec![
                    BIrStmt::Xor(IRVarId(0), IRVarId(1)),
                    BIrStmt::Xor(IRVarId(3), IRVarId(2)),
                ],
                vec![4],
            ),
            // plaintext = key XOR ciphertext
            decrypt: circuit(2, vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))], vec![2]),
        }
        .validate(GEOMETRY)
        .unwrap()
    }

    #[test]
    fn inlines_all_three_programs_into_one_fused_circuit() {
        let provider = provider();
        // guest params: seed-left, seed-right, plaintext, randomness.
        let mut combined = BCircuit::new(4);
        let key = provider
            .derive_key(&mut combined, &[IRVarId(0)], &[IRVarId(1)])
            .unwrap();
        let ciphertext = provider
            .encrypt(&mut combined, &key, &[IRVarId(2)], &[IRVarId(3)])
            .unwrap();
        let plaintext = provider.decrypt(&mut combined, &key, &ciphertext).unwrap();
        combined.outputs = plaintext;

        assert_eq!(combined.stmts.len(), 4);
        assert_eq!(combined.outputs, vec![IRVarId(7)]);
        assert!(combined.pre_init.is_empty());
    }

    #[test]
    fn rejects_effectful_provider_programs_before_composition() {
        let mut bad = CircuitProviderPrograms {
            derive_key: circuit(2, vec![BIrStmt::Rng { name: "bad".into() }], vec![2]),
            encrypt: circuit(3, vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))], vec![3]),
            decrypt: circuit(2, vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))], vec![2]),
        };
        // Make encryption's output arity otherwise valid; the key program
        // must be rejected first for its forbidden host-RNG statement.
        bad.encrypt = circuit(3, vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))], vec![3]);
        assert!(matches!(
            bad.validate(GEOMETRY),
            Err(CircuitProviderError::UnsupportedStatement {
                program: ProviderProgramKind::DeriveKey,
            })
        ));
    }

    #[test]
    fn composition_memoizes_key_and_decrypt_and_rejects_randomness_reuse() {
        let provider = provider();
        let mut combined = BCircuit::new(4);
        let mut composition = provider.compose_into(&mut combined);
        let key = composition
            .derive_key(KeyUse(7), &[IRVarId(0)], &[IRVarId(1)])
            .unwrap();
        assert_eq!(
            key,
            composition
                .derive_key(KeyUse(7), &[IRVarId(0)], &[IRVarId(1)])
                .unwrap()
        );
        let ciphertext = composition
            .encrypt(KeyUse(7), CiphertextUse(9), &[IRVarId(2)], &[IRVarId(3)])
            .unwrap();
        let first_plaintext = composition.decrypt(KeyUse(7), CiphertextUse(9)).unwrap();
        let second_plaintext = composition.decrypt(KeyUse(7), CiphertextUse(9)).unwrap();
        assert_eq!(first_plaintext, second_plaintext);
        assert_eq!(ciphertext.len(), 1);
        // KDF (1) + encrypt (2) + one decrypt (1), not a second KDF/decrypt.
        assert_eq!(composition.into_circuit().stmts.len(), 4);

        let mut second = BCircuit::new(4);
        let mut composition = provider.compose_into(&mut second);
        composition
            .derive_key(KeyUse(7), &[IRVarId(0)], &[IRVarId(1)])
            .unwrap();
        composition
            .encrypt(KeyUse(7), CiphertextUse(9), &[IRVarId(2)], &[IRVarId(3)])
            .unwrap();
        assert!(matches!(
            composition.encrypt(KeyUse(7), CiphertextUse(10), &[IRVarId(2)], &[IRVarId(3)],),
            Err(CircuitProviderError::RandomnessReused)
        ));
    }
}
