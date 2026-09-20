use std::sync::Arc;

use rustls::crypto::KeyProvider;
use rustls::pki_types::{PrivateKeyDer, ServerName};
use rustls::{ClientConfig, ClientConnection, Error, RootCertStore};
use volar_rustls_narrow::{TLS13_AES_128_GCM_SHA256, X25519, provider};

#[derive(Debug)]
struct NoPrivateKeys;

impl KeyProvider for NoPrivateKeys {
    fn load_private_key(
        &self,
        _: PrivateKeyDer<'static>,
    ) -> Result<Arc<dyn rustls::sign::SigningKey>, Error> {
        Err(Error::General(
            "the client provider does not load private keys".into(),
        ))
    }
}

static NO_PRIVATE_KEYS: NoPrivateKeys = NoPrivateKeys;

#[test]
fn exposes_only_the_mpc_tls13_suite_and_x25519() {
    let rustcrypto = rustls_rustcrypto::provider();
    let crypto = provider(
        rustcrypto.signature_verification_algorithms,
        &NO_PRIVATE_KEYS,
    );

    assert_eq!(crypto.cipher_suites, [TLS13_AES_128_GCM_SHA256]);
    assert_eq!(crypto.kx_groups.len(), 1);
    assert_eq!(crypto.kx_groups[0].name(), rustls::NamedGroup::X25519);

    let config = ClientConfig::builder_with_provider(Arc::new(crypto))
        .with_protocol_versions(&[&rustls::version::TLS13])
        .expect("the narrow provider supports TLS 1.3")
        .with_root_certificates(RootCertStore::empty())
        .with_no_client_auth();
    let connection = ClientConnection::new(
        Arc::new(config),
        ServerName::try_from("example.com").unwrap().to_owned(),
    );
    assert!(
        connection.is_ok(),
        "narrow provider builds a client connection"
    );

    let kx = rustls::crypto::SupportedKxGroup::start(&X25519).expect("X25519 starts");
    assert_eq!(kx.pub_key().len(), 32);
}
