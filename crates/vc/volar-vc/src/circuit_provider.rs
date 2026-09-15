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
    UnknownTrackedWire(IRVarId),
    TrackedWireAlreadyRegistered(IRVarId),
    SelectKeyMismatch {
        left: KeyUse,
        right: KeyUse,
    },
}

/// Run the deterministic Boolean optimization fixpoint after provider programs
/// have been inlined into a combined circuit.
///
/// Provider calls are ordinary Boolar gates at this point, so public labels and
/// domain separators can fold, duplicate pure derivation subgraphs can merge,
/// and dead conversion work is removed before the GC schedule is built.
pub fn optimize_circuit_provider_composition<P: Clone>(circuit: &mut BCircuit<P>) {
    let mut blocks = circuit.clone().to_bir_blocks();
    volar_ir_opt::biir::fold_biir_blocks(&mut blocks);
    volar_ir_opt::biir::cse_biir_blocks(&mut blocks);
    volar_ir_opt::biir::dce_biir_blocks(&mut blocks);
    *circuit = BCircuit::try_from_ir(&blocks)
        .expect("optimization preserves the single-block fused-circuit invariant");
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

/// Why an encrypted value crosses into an ordinary Boolean consumer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DecryptReason {
    /// The value must enter a cache representation; V1 has no durable
    /// authenticated ciphertext cache.
    Cached,
    /// The first consumer is not select-only propagation.
    FirstNonSelectUse,
}

/// Conservative source-value residence tracked by exact Boolar wire identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WireResidence {
    Clear,
    Encrypted {
        key_use: KeyUse,
        ciphertext_use: CiphertextUse,
    },
}

/// One deferred expensive conversion request emitted exactly at a semantic
/// demand boundary. The request has no plaintext/ciphertext payload itself.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WireConversionDemand {
    Decrypt {
        source: IRVarId,
        key_use: KeyUse,
        ciphertext_use: CiphertextUse,
        reason: DecryptReason,
    },
    EncryptForSelect {
        source: IRVarId,
        key_use: KeyUse,
    },
}

/// Exact-`IRVarId` demand tracker for conversions around FHE-region selects.
///
/// It never keys by a source name. `select_only` merely preserves an encrypted
/// value; `cached` and `first_non_select_use` emit at most one decryption
/// demand per source. At a mixed clear/encrypted select it emits exactly one
/// encryption demand for the clear source, and only at that actual merge.
#[derive(Clone, Debug, Default)]
pub struct ProviderWireTracker {
    wires: BTreeMap<IRVarId, WireResidence>,
    decrypt_requested: BTreeSet<IRVarId>,
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

impl ProviderWireTracker {
    /// Register a source wire once as an ordinary clear value.
    pub fn register_clear(&mut self, source: IRVarId) -> Result<(), CircuitProviderError> {
        self.register(source, WireResidence::Clear)
    }

    /// Register a source wire once as an encrypted value.
    pub fn register_encrypted(
        &mut self,
        source: IRVarId,
        key_use: KeyUse,
        ciphertext_use: CiphertextUse,
    ) -> Result<(), CircuitProviderError> {
        self.register(
            source,
            WireResidence::Encrypted {
                key_use,
                ciphertext_use,
            },
        )
    }

    /// Obtain a source wire's current residence.
    pub fn residence(&self, source: IRVarId) -> Result<WireResidence, CircuitProviderError> {
        self.wires
            .get(&source)
            .copied()
            .ok_or(CircuitProviderError::UnknownTrackedWire(source))
    }

    /// A select-only consumer cannot by itself force decryption.
    pub fn select_only(&self, source: IRVarId) -> Result<(), CircuitProviderError> {
        self.residence(source).map(|_| ())
    }

    /// Emit a decrypt demand only if the source has not been demanded before.
    pub fn cached(
        &mut self,
        source: IRVarId,
    ) -> Result<Option<WireConversionDemand>, CircuitProviderError> {
        self.decrypt_if_needed(source, DecryptReason::Cached)
    }

    /// Emit a decrypt demand only at the actual first non-select consumer.
    pub fn first_non_select_use(
        &mut self,
        source: IRVarId,
    ) -> Result<Option<WireConversionDemand>, CircuitProviderError> {
        self.decrypt_if_needed(source, DecryptReason::FirstNonSelectUse)
    }

    /// Inspect an actual select merge. If exactly one branch is encrypted,
    /// request encryption of only the clear branch under the encrypted branch's
    /// key use. Two encrypted branches must have the same key use; otherwise a
    /// caller must cut the region rather than silently converting either side.
    pub fn merge_select(
        &self,
        when_true: IRVarId,
        when_false: IRVarId,
    ) -> Result<Option<WireConversionDemand>, CircuitProviderError> {
        let left = self.residence(when_true)?;
        let right = self.residence(when_false)?;
        match (left, right) {
            (WireResidence::Clear, WireResidence::Clear) => Ok(None),
            (WireResidence::Clear, WireResidence::Encrypted { key_use, .. }) => {
                Ok(Some(WireConversionDemand::EncryptForSelect {
                    source: when_true,
                    key_use,
                }))
            }
            (WireResidence::Encrypted { key_use, .. }, WireResidence::Clear) => {
                Ok(Some(WireConversionDemand::EncryptForSelect {
                    source: when_false,
                    key_use,
                }))
            }
            (
                WireResidence::Encrypted {
                    key_use: left_key, ..
                },
                WireResidence::Encrypted {
                    key_use: right_key, ..
                },
            ) if left_key == right_key => Ok(None),
            (
                WireResidence::Encrypted {
                    key_use: left_key, ..
                },
                WireResidence::Encrypted {
                    key_use: right_key, ..
                },
            ) => Err(CircuitProviderError::SelectKeyMismatch {
                left: left_key,
                right: right_key,
            }),
        }
    }

    /// Record the encrypted result produced by the FHE-region select after a
    /// caller has fulfilled any [`WireConversionDemand::EncryptForSelect`].
    pub fn register_select_result(
        &mut self,
        result: IRVarId,
        key_use: KeyUse,
        ciphertext_use: CiphertextUse,
    ) -> Result<(), CircuitProviderError> {
        self.register_encrypted(result, key_use, ciphertext_use)
    }

    fn register(
        &mut self,
        source: IRVarId,
        residence: WireResidence,
    ) -> Result<(), CircuitProviderError> {
        if self.wires.insert(source, residence).is_some() {
            return Err(CircuitProviderError::TrackedWireAlreadyRegistered(source));
        }
        Ok(())
    }

    fn decrypt_if_needed(
        &mut self,
        source: IRVarId,
        reason: DecryptReason,
    ) -> Result<Option<WireConversionDemand>, CircuitProviderError> {
        match self.residence(source)? {
            WireResidence::Clear => Ok(None),
            WireResidence::Encrypted {
                key_use,
                ciphertext_use,
            } if self.decrypt_requested.insert(source) => Ok(Some(WireConversionDemand::Decrypt {
                source,
                key_use,
                ciphertext_use,
                reason,
            })),
            WireResidence::Encrypted { .. } => Ok(None),
        }
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

    /// Apply the required fold/CSE/DCE optimization fixpoint to this combined
    /// circuit. Call after assigning the combined circuit's required outputs.
    pub fn optimize(&mut self) {
        optimize_circuit_provider_composition(self.circuit);
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
    fn combined_optimizer_folds_dead_provider_work() {
        let provider = provider();
        let mut combined = BCircuit::new(4);
        let mut composition = provider.compose_into(&mut combined);
        let key = composition
            .derive_key(KeyUse(7), &[IRVarId(0)], &[IRVarId(1)])
            .unwrap();
        // Encrypt but do not make the ciphertext live at the output. DCE must
        // remove the full inlined conversion graph after composition.
        composition
            .encrypt(KeyUse(7), CiphertextUse(9), &[IRVarId(2)], &[IRVarId(3)])
            .unwrap();
        composition.circuit.outputs = key;
        composition.optimize();
        assert_eq!(composition.into_circuit().stmts.len(), 1);
    }

    #[test]
    fn wire_tracker_delays_decrypt_and_encrypts_only_at_mixed_select() {
        let encrypted = IRVarId(10);
        let clear = IRVarId(11);
        let mut tracker = ProviderWireTracker::default();
        tracker
            .register_encrypted(encrypted, KeyUse(1), CiphertextUse(2))
            .unwrap();
        tracker.register_clear(clear).unwrap();

        tracker.select_only(encrypted).unwrap();
        assert_eq!(
            tracker.merge_select(encrypted, clear).unwrap(),
            Some(WireConversionDemand::EncryptForSelect {
                source: clear,
                key_use: KeyUse(1),
            })
        );
        assert_eq!(
            tracker.first_non_select_use(encrypted).unwrap(),
            Some(WireConversionDemand::Decrypt {
                source: encrypted,
                key_use: KeyUse(1),
                ciphertext_use: CiphertextUse(2),
                reason: DecryptReason::FirstNonSelectUse,
            })
        );
        // A later cache boundary shares the already-requested decrypt.
        assert_eq!(tracker.cached(encrypted).unwrap(), None);
    }

    #[test]
    fn wire_tracker_rejects_mixed_encrypted_key_uses() {
        let mut tracker = ProviderWireTracker::default();
        tracker
            .register_encrypted(IRVarId(10), KeyUse(1), CiphertextUse(2))
            .unwrap();
        tracker
            .register_encrypted(IRVarId(11), KeyUse(3), CiphertextUse(4))
            .unwrap();
        assert!(matches!(
            tracker.merge_select(IRVarId(10), IRVarId(11)),
            Err(CircuitProviderError::SelectKeyMismatch {
                left: KeyUse(1),
                right: KeyUse(3),
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
