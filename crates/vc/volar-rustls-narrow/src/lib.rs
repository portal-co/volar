//! A deliberately narrow TLS 1.3 [`rustls::crypto::CryptoProvider`] for the
//! MPC-TLS feasibility path.
//!
//! It exposes exactly TLS 1.3 AES-128-GCM/SHA-256 and X25519. This is a native
//! reference provider only: rustls exposes KX, HKDF, and AEAD secrets as byte
//! slices, so a `CryptoProvider` alone cannot enforce jointly-held MPC keys.
//! The narrow surface is the baseline for explicit strict-chain action calls.
//!
//! @pinnedness: unpinned
//! @stability: very-unstable
//! @ai: assisted

#![no_std]

extern crate alloc;

use alloc::{boxed::Box, vec};

use rustls::crypto::cipher::{
    self, AeadKey, BorrowedPayload, InboundOpaqueMessage, InboundPlainMessage, Iv,
    MessageDecrypter, MessageEncrypter, OutboundOpaqueMessage, OutboundPlainMessage,
    PrefixedPayload, Tls13AeadAlgorithm,
};
use rustls::crypto::hash::{self, Context, Hash};
use rustls::crypto::hmac::{self as tls_hmac, Hmac};
use rustls::crypto::{
    ActiveKeyExchange, CipherSuiteCommon, CryptoProvider, GetRandomFailed, KeyProvider,
    SecureRandom, SharedSecret, SupportedKxGroup,
};
use rustls::{
    CipherSuite, ConnectionTrafficSecrets, ContentType, Error, NamedGroup, ProtocolVersion,
    SupportedCipherSuite, Tls13CipherSuite,
};

/// The sole cipher suite exposed by [`provider`].
pub const TLS13_AES_128_GCM_SHA256: SupportedCipherSuite =
    SupportedCipherSuite::Tls13(&Tls13CipherSuite {
        common: CipherSuiteCommon {
            suite: CipherSuite::TLS13_AES_128_GCM_SHA256,
            hash_provider: &SHA256,
            confidentiality_limit: 1 << 24,
        },
        hkdf_provider: &rustls::crypto::tls13::HkdfUsingHmac(&HMAC_SHA256),
        aead_alg: &AES_128_GCM,
        quic: None,
    });

/// Builds the TLS-1.3-only provider. Certificate algorithms and private-key
/// loading remain caller-provided because certificate handling is public-data
/// native; KX, HKDF and AEAD are the intended MPC boundary.
pub fn provider(
    signature_verification_algorithms: rustls::crypto::WebPkiSupportedAlgorithms,
    key_provider: &'static dyn KeyProvider,
) -> CryptoProvider {
    CryptoProvider {
        cipher_suites: vec![TLS13_AES_128_GCM_SHA256],
        kx_groups: vec![&X25519],
        signature_verification_algorithms,
        secure_random: &SystemRandom,
        key_provider,
    }
}

/// The provider's native entropy source.
#[derive(Debug)]
pub struct SystemRandom;

impl SecureRandom for SystemRandom {
    fn fill(&self, bytes: &mut [u8]) -> Result<(), GetRandomFailed> {
        use rand_core::RngCore;
        rand_core::OsRng
            .try_fill_bytes(bytes)
            .map_err(|_| GetRandomFailed)
    }
}

/// The only supported TLS 1.3 key-exchange group.
#[derive(Debug)]
pub struct X25519;

impl SupportedKxGroup for X25519 {
    fn start(&self) -> Result<Box<dyn ActiveKeyExchange>, Error> {
        let private = x25519_dalek::EphemeralSecret::random_from_rng(rand_core::OsRng);
        let public = (&private).into();
        Ok(Box::new(X25519Exchange { private, public }))
    }

    fn name(&self) -> NamedGroup {
        NamedGroup::X25519
    }
}

struct X25519Exchange {
    private: x25519_dalek::EphemeralSecret,
    public: x25519_dalek::PublicKey,
}

impl ActiveKeyExchange for X25519Exchange {
    fn complete(self: Box<Self>, peer: &[u8]) -> Result<SharedSecret, Error> {
        let peer: [u8; 32] = peer
            .try_into()
            .map_err(|_| Error::from(rustls::PeerMisbehaved::InvalidKeyShare))?;
        Ok(self.private.diffie_hellman(&peer.into()).as_ref().into())
    }

    fn pub_key(&self) -> &[u8] {
        self.public.as_bytes()
    }

    fn group(&self) -> NamedGroup {
        X25519.name()
    }
}

/// Native SHA-256 retained as a reference implementation until secret mixing
/// is moved to strict-chain actions.
#[derive(Debug)]
pub struct Sha256;
pub static SHA256: Sha256 = Sha256;

impl Hash for Sha256 {
    fn start(&self) -> Box<dyn Context> {
        Box::new(Sha256Context(sha2::Sha256::default()))
    }

    fn hash(&self, data: &[u8]) -> hash::Output {
        use sha2::Digest;
        hash::Output::new(&sha2::Sha256::digest(data))
    }

    fn output_len(&self) -> usize {
        32
    }

    fn algorithm(&self) -> rustls::crypto::hash::HashAlgorithm {
        rustls::crypto::hash::HashAlgorithm::SHA256
    }
}

#[derive(Clone, Debug)]
struct Sha256Context(sha2::Sha256);

impl Context for Sha256Context {
    fn fork_finish(&self) -> hash::Output {
        use sha2::Digest;
        hash::Output::new(&self.0.clone().finalize())
    }

    fn fork(&self) -> Box<dyn Context> {
        Box::new(self.clone())
    }

    fn finish(self: Box<Self>) -> hash::Output {
        use sha2::Digest;
        hash::Output::new(&self.0.finalize())
    }

    fn update(&mut self, data: &[u8]) {
        use sha2::Digest;
        self.0.update(data);
    }
}

/// Native HMAC-SHA-256 retained as the key-schedule reference implementation.
#[derive(Debug)]
pub struct HmacSha256;
pub static HMAC_SHA256: HmacSha256 = HmacSha256;

impl Hmac for HmacSha256 {
    fn with_key(&self, key: &[u8]) -> Box<dyn tls_hmac::Key> {
        use ::hmac::Mac;
        let mac = ::hmac::Hmac::<sha2::Sha256>::new_from_slice(key)
            .expect("HMAC-SHA-256 accepts every key length");
        Box::new(HmacSha256Key(mac))
    }

    fn hash_output_len(&self) -> usize {
        32
    }
}

#[derive(Clone, Debug)]
struct HmacSha256Key(::hmac::Hmac<sha2::Sha256>);

impl tls_hmac::Key for HmacSha256Key {
    fn sign_concat(&self, first: &[u8], middle: &[&[u8]], last: &[u8]) -> tls_hmac::Tag {
        use ::hmac::Mac;
        let mut mac = self.0.clone();
        mac.update(first);
        for part in middle {
            mac.update(part);
        }
        mac.update(last);
        tls_hmac::Tag::new(&mac.finalize().into_bytes())
    }

    fn tag_len(&self) -> usize {
        32
    }
}

/// TLS 1.3 AES-128-GCM implementation selected by the narrow suite.
#[derive(Debug)]
pub struct Aes128Gcm;
pub static AES_128_GCM: Aes128Gcm = Aes128Gcm;

impl Tls13AeadAlgorithm for Aes128Gcm {
    fn encrypter(&self, key: AeadKey, iv: Iv) -> Box<dyn MessageEncrypter> {
        use aes_gcm::aead::KeyInit;
        Box::new(Tls13Aes128Gcm(
            aes_gcm::Aes128Gcm::new_from_slice(key.as_ref()).expect("TLS key length"),
            iv,
        ))
    }

    fn decrypter(&self, key: AeadKey, iv: Iv) -> Box<dyn MessageDecrypter> {
        use aes_gcm::aead::KeyInit;
        Box::new(Tls13Aes128Gcm(
            aes_gcm::Aes128Gcm::new_from_slice(key.as_ref()).expect("TLS key length"),
            iv,
        ))
    }

    fn key_len(&self) -> usize {
        16
    }

    fn extract_keys(
        &self,
        key: AeadKey,
        iv: Iv,
    ) -> Result<ConnectionTrafficSecrets, cipher::UnsupportedOperationError> {
        Ok(ConnectionTrafficSecrets::Aes128Gcm { key, iv })
    }
}

struct Tls13Aes128Gcm(aes_gcm::Aes128Gcm, Iv);

impl MessageEncrypter for Tls13Aes128Gcm {
    fn encrypt(
        &mut self,
        message: OutboundPlainMessage<'_>,
        seq: u64,
    ) -> Result<OutboundOpaqueMessage, Error> {
        use aes_gcm::aead::AeadInPlace;
        let length = self.encrypted_payload_len(message.payload.len());
        let mut payload = PrefixedPayload::with_capacity(length);
        payload.extend_from_chunks(&message.payload);
        payload.extend_from_slice(&message.typ.to_array());
        self.0
            .encrypt_in_place(
                &cipher::Nonce::new(&self.1, seq).0.into(),
                &cipher::make_tls13_aad(length),
                &mut EncryptBufferAdapter(&mut payload),
            )
            .map_err(|_| Error::EncryptError)?;
        Ok(OutboundOpaqueMessage::new(
            ContentType::ApplicationData,
            ProtocolVersion::TLSv1_2,
            payload,
        ))
    }

    fn encrypted_payload_len(&self, plaintext_len: usize) -> usize {
        plaintext_len + 1 + 16
    }
}

impl MessageDecrypter for Tls13Aes128Gcm {
    fn decrypt<'a>(
        &mut self,
        mut message: InboundOpaqueMessage<'a>,
        seq: u64,
    ) -> Result<InboundPlainMessage<'a>, Error> {
        use aes_gcm::aead::AeadInPlace;
        let aad = cipher::make_tls13_aad(message.payload.len());
        self.0
            .decrypt_in_place(
                &cipher::Nonce::new(&self.1, seq).0.into(),
                &aad,
                &mut DecryptBufferAdapter(&mut message.payload),
            )
            .map_err(|_| Error::DecryptError)?;
        message.into_tls13_unpadded_message()
    }
}

struct EncryptBufferAdapter<'a>(&'a mut PrefixedPayload);

impl AsRef<[u8]> for EncryptBufferAdapter<'_> {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl AsMut<[u8]> for EncryptBufferAdapter<'_> {
    fn as_mut(&mut self) -> &mut [u8] {
        self.0.as_mut()
    }
}

impl aes_gcm::aead::Buffer for EncryptBufferAdapter<'_> {
    fn extend_from_slice(&mut self, other: &[u8]) -> aes_gcm::aead::Result<()> {
        self.0.extend_from_slice(other);
        Ok(())
    }

    fn truncate(&mut self, len: usize) {
        self.0.truncate(len);
    }
}

struct DecryptBufferAdapter<'a, 'payload>(&'a mut BorrowedPayload<'payload>);

impl AsRef<[u8]> for DecryptBufferAdapter<'_, '_> {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl AsMut<[u8]> for DecryptBufferAdapter<'_, '_> {
    fn as_mut(&mut self) -> &mut [u8] {
        self.0.as_mut()
    }
}

impl aes_gcm::aead::Buffer for DecryptBufferAdapter<'_, '_> {
    fn extend_from_slice(&mut self, _: &[u8]) -> aes_gcm::aead::Result<()> {
        unreachable!("AEAD decryption never extends its input")
    }

    fn truncate(&mut self, len: usize) {
        self.0.truncate(len);
    }
}
