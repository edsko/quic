use std::net::SocketAddr;

pub static SERVER_NAME: &str = "localhost";

pub fn server_addr() -> SocketAddr {
    "127.0.0.1:5000".parse::<SocketAddr>().unwrap()
}
