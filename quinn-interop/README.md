# Interop between Haskell `quic` and Rust `quinn`

This is a simple demonstration of interoperability between the Haskell `quic`
library and the [Rust `quinn` library](https://crates.io/crates/quinn).

The code code is adapted from
[`quinn` guide book](https://quinn-rs.github.io/quinn/).

## Haskell server vs Rust client

In `haskell/`, start the server using

```
rm -f debug_log/*.txt && cabal run simple_server
```

The problematic Rust client:

```
cargo run --bin concurrent_bi_client -- 127.0.0.1:5001
```

The debug logs will now contain a decryption failure.

## Rust server vs Rust client

In `rust/`, start the server using

```
cargo run --bin simple_server
```

Then in a separate terminal, run any of these:

- Bidirectional client:

  ```
  cargo run --bin simple_bi_client -- 127.0.0.1:0
  ```

- Unidirectional client:

  ```
  cargo run --bin simple_uni_client -- 127.0.0.1:0
  ```

- Client using the unreliable datagram extension

  ```
  cargo run --bin simple_datagram_client -- 127.0.0.1:0
  ```

- Bidirectional client using two connections concurrently:

  ```
  cargo run --bin concurrent_bi_client -- 127.0.0.1:0
  ```

