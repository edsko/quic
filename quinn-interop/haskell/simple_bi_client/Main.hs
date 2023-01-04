{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.QUIC
import Network.QUIC.Client

runClient :: ClientConfig -> IO ()
runClient config = do
    run config $ \conn -> do
      waitEstablished conn
      s <- stream conn
      sendStream s "test"
      recv <- recvStream s 50
      putStrLn $ "received: " ++ show recv
      closeStream s

main :: IO ()
main =
    runClient config
  where
    config :: ClientConfig
    config = defaultClientConfig {
          ccServerName = "127.0.0.1"
        , ccPortName   = "5000"
        , ccValidate   = False
        }

