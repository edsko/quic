use std::{env, net::SocketAddr};

use quinn::{Connection, Endpoint};
use quinn_interop::{config, insecure};

async fn client(client_addr: SocketAddr) -> anyhow::Result<()> {
    let endpoint = Endpoint::client(client_addr)?;

    let connecting1 = endpoint.connect_with(
        insecure::configure_client(),
        config::server_addr(),
        config::SERVER_NAME,
    )?;
    let connecting2 = endpoint.connect_with(
        insecure::configure_client(),
        config::server_addr(),
        config::SERVER_NAME,
    )?;

    let connection1 = connecting1.await?;
    let connection2: Connection = connecting2.await?;

    // Ensure the connections are both setup before we do anything else
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    tokio::join!(with_connection(connection1), with_connection(connection2));

    endpoint.wait_idle().await;
    Ok(())
}

async fn with_connection(connection: Connection) {
    open_bidirectional_stream(connection).await.unwrap()
}

async fn open_bidirectional_stream(connection: Connection) -> anyhow::Result<()> {
    let (mut send, recv) = connection.open_bi().await?;

    send.write_all(b"test").await?;
    send.finish().await?;

    let received = recv.read_to_end(10).await?;
    println!("received: {:?}", received);

    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let client_addr: SocketAddr = args[1].parse().unwrap();
    client(client_addr).await
}
