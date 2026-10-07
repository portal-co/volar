//! Ferret-backed chosen-bit OT over framed TCP.

use std::thread;

use hybrid_array::Array;
use typenum::U16;
use volar_mpc::OtChannel;
use volar_mpc::ot::SeedRng;
use volar_mpc::tcp::{FerretOtChannel, OtRole, TcpTransport};
use volar_spec::ot::ferret::FERRET_REG_TOY;

#[test]
fn ferret_ot_recovers_only_the_chosen_label_over_tcp() {
    type N = U16;
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let sender = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut rng = SeedRng::new(0xFE77_0001);
        let mut ot = FerretOtChannel::new(transport, OtRole::Sender, &mut rng, FERRET_REG_TOY);
        let zero = Array::<u8, N>::from_fn(|i| i as u8);
        let one = Array::<u8, N>::from_fn(|i| 0x80 | i as u8);
        ot.send([&zero, &one]);
        ot.send([&one, &zero]);
        ot.metrics()
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut rng = SeedRng::new(0xFE77_0002);
    let mut ot = FerretOtChannel::new(transport, OtRole::Receiver, &mut rng, FERRET_REG_TOY);
    let first = ot.receive(true);
    let second = ot.receive(false);
    assert_eq!(first, Array::<u8, N>::from_fn(|i| 0x80 | i as u8));
    assert_eq!(second, Array::<u8, N>::from_fn(|i| 0x80 | i as u8));
    let receiver_metrics = ot.metrics();
    let sender_metrics = sender.join().expect("sender join");
    assert!(sender_metrics.sent_bytes > 0 && sender_metrics.received_bytes > 0);
    assert!(receiver_metrics.sent_bytes > 0 && receiver_metrics.received_bytes > 0);
}
