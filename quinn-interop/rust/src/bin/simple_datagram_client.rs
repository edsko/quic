use std::{env, net::SocketAddr, time::Duration};

use bytes::Bytes;
use quinn::{Connection, Endpoint};
use quinn_interop::{config, insecure};

async fn client(client_addr: SocketAddr) -> anyhow::Result<()> {
    let endpoint = Endpoint::client(client_addr)?;
    let connection = endpoint
        .connect_with(
            insecure::configure_client(),
            config::server_addr(),
            config::SERVER_NAME,
        )?
        .await?;

    send_unreliable(&connection).await?;

    // If we terminate immediately, the other side might see "connection closed"
    // rather than the datagram
    tokio::time::sleep(Duration::from_millis(10)).await;
    Ok(())
}

async fn send_unreliable(connection: &Connection) -> anyhow::Result<()> {
    println!("Sending datagram..");
    connection.send_datagram(Bytes::from("test"))?;
    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let client_addr: SocketAddr = args[1].parse().unwrap();
    client(client_addr).await
}
