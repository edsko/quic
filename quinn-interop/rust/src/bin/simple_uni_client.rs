use std::{env, net::SocketAddr};

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

    open_unidirectional_stream(connection).await
}

async fn open_unidirectional_stream(connection: Connection) -> anyhow::Result<()> {
    let mut send = connection.open_uni().await?;

    send.write_all(b"test").await?;
    send.finish().await?;

    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let client_addr: SocketAddr = args[1].parse().unwrap();
    client(client_addr).await
}
