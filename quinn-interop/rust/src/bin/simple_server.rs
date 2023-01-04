use bytes::Bytes;
use futures::{stream, StreamExt};
use quinn::{Connecting, Connection, Endpoint, RecvStream, SendStream, ServerConfig};
use quinn_interop::{
    config,
    endpoint::stream::{BiStream, DatagramStream, EndpointStream, UniStream},
    insecure,
};

async fn server(config: ServerConfig) -> anyhow::Result<()> {
    let endpoint = Endpoint::server(config, config::server_addr())?;

    EndpointStream::new(&endpoint)
        .for_each_concurrent(None, handle_connection)
        .await;

    Ok(())
}

/*******************************************************************************
  Accept any kind of stream (bi, uni, datagram)
*******************************************************************************/

pub enum QuicStream {
    Bi(SendStream, RecvStream),
    Uni(RecvStream),
    Datagram(Bytes),
}

// This cannot return an error (because it's an argument to `for_each_concurrent`)
// TODO: We should handle errors a bit better though
async fn handle_connection(connecting: Connecting) {
    let connection: Connection = connecting.await.unwrap();

    println!("Accepted connection from {}", connection.remote_address());

    stream::select(
        stream::select(
            BiStream::new(&connection).map(|(send, recv)| QuicStream::Bi(send, recv)),
            UniStream::new(&connection).map(|recv| QuicStream::Uni(recv)),
        ),
        DatagramStream::new(&connection).map(|bytes| QuicStream::Datagram(bytes)),
    )
    .for_each_concurrent(None, handle_quic_stream)
    .await;
}

async fn handle_quic_stream(quic_stream: QuicStream) {
    match quic_stream {
        QuicStream::Bi(send, recv) => handle_bidirectional_stream(send, recv).await.unwrap(),
        QuicStream::Uni(recv) => handle_unidirectional_stream(recv).await.unwrap(),
        QuicStream::Datagram(bytes) => handle_datagram(bytes),
    }
}

async fn handle_bidirectional_stream(mut send: SendStream, recv: RecvStream) -> anyhow::Result<()> {
    println!(
        "received on bidirectional stream: {:?}",
        recv.read_to_end(50).await?
    );

    send.write_all(b"response").await?;
    send.finish().await?;

    Ok(())
}

async fn handle_unidirectional_stream(recv: RecvStream) -> anyhow::Result<()> {
    println!(
        "received on unidirectional stream: {:?}",
        recv.read_to_end(50).await?
    );

    Ok(())
}

fn handle_datagram(bytes: Bytes) {
    println!("received datagram {:?}", bytes);
}

/*******************************************************************************
  Main application
*******************************************************************************/

#[tokio::main]
async fn main() {
    let server_config = insecure::configure_server();
    server(server_config).await.unwrap()
}
