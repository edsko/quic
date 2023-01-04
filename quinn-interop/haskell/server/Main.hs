{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Network.QUIC
import Network.QUIC.Internal
import Network.QUIC.Server
import Network.TLS

main :: IO ()
main = do
    creds <- loadCredentials
    let config :: ServerConfig
        config = defaultServerConfig {
            scAddresses   = [("127.0.0.1", 5000)]
          , scCredentials = creds
          , scDebugLog    = Just "debug_log"
          }

    run config $ \conn -> do
      connInfo <- getConnectionInfo conn
      putStrLn $ "Accepted connection from " ++ show (remoteSockAddr connInfo)
      forever $ do
        s <- acceptStream conn
        if
          | isClientInitiatedBidirectional (streamId s)
          -> handleBidirectional s

          | isClientInitiatedUnidirectional (streamId s)
          -> handleUnidirectional s

          | otherwise
          -> fail "Unexpected stream type"

handleBidirectional :: Stream -> IO ()
handleBidirectional s = do
    received <- recvStream s 50
    putStrLn $ "received on bidirectional stream: " ++ show received
    sendStream s "response"
    closeStream s

handleUnidirectional :: Stream -> IO ()
handleUnidirectional s = do
    received <- recvStream s 50
    putStrLn $ "received on unidirectional stream: " ++ show received

loadCredentials :: IO Credentials
loadCredentials = do
    mCred <- credentialLoadX509 "server.cert" "server.priv"
    case mCred of
      Right cred -> return $ Credentials [cred]
      Left err   -> fail $ "Could not load credentials: " ++ show err