{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QUIC.Server.Reader (
    Dispatch
  , newDispatch
  , clearDispatch
  , runDispatcher
  , tokenMgr
  -- * Accepting
  , accept
  , Accept(..)
  -- * Receiving and reading
  , RecvQ
  , recvServer
  , readerServer
  -- * Misc
  , runNewServerReader
  ) where

import qualified Crypto.Token as CT
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified GHC.IO.Exception as E
import Network.ByteOrder
import Network.Socket hiding (accept, Debug)
import qualified Network.Socket.ByteString as NSB
import qualified System.IO.Error as E
import System.Log.FastLogger
import UnliftIO.Concurrent
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Network.QUIC.Config
import Network.QUIC.Connection
import Network.QUIC.Exception
import Network.QUIC.Imports
import Network.QUIC.Logger
import Network.QUIC.Packet
import Network.QUIC.Parameters
import Network.QUIC.Qlog
import Network.QUIC.Socket
import Network.QUIC.Types
#if defined(mingw32_HOST_OS)
import Network.QUIC.Windows
#else
import Network.QUIC.Connector
#endif

----------------------------------------------------------------

data Dispatch = Dispatch {
    tokenMgr :: CT.TokenManager
  , dstTable :: IORef ConnectionDict
  , srcTable :: IORef RecvQDict
  , acceptQ  :: AcceptQ
  }

newDispatch :: IO Dispatch
newDispatch = Dispatch <$> CT.spawnTokenManager CT.defaultConfig
                       <*> newIORef emptyConnectionDict
                       <*> newIORef emptyRecvQDict
                       <*> newAcceptQ

clearDispatch :: Dispatch -> IO ()
clearDispatch d = CT.killTokenManager $ tokenMgr d

----------------------------------------------------------------

newtype ConnectionDict = ConnectionDict (Map CID Connection)

emptyConnectionDict :: ConnectionDict
emptyConnectionDict = ConnectionDict M.empty

lookupConnectionDict :: IORef ConnectionDict -> CID -> IO (Maybe Connection)
lookupConnectionDict ref cid = do
    ConnectionDict tbl <- readIORef ref
    return $ M.lookup cid tbl

registerConnectionDict :: IORef ConnectionDict -> CID -> Connection -> IO ()
registerConnectionDict ref cid conn = atomicModifyIORef'' ref $
    \(ConnectionDict tbl) -> ConnectionDict $ M.insert cid conn tbl

unregisterConnectionDict :: IORef ConnectionDict -> CID -> IO ()
unregisterConnectionDict ref cid = atomicModifyIORef'' ref $
    \(ConnectionDict tbl) -> ConnectionDict $ M.delete cid tbl

----------------------------------------------------------------

-- Original destination CID -> RecvQ
data RecvQDict = RecvQDict Int (OrdPSQ CID Int RecvQ)

recvQDictSize :: Int
recvQDictSize = 100

emptyRecvQDict :: RecvQDict
emptyRecvQDict = RecvQDict 0 PSQ.empty

lookupRecvQDict :: IORef RecvQDict -> CID -> IO (Maybe RecvQ)
lookupRecvQDict ref dcid = do
    RecvQDict _ qt <- readIORef ref
    return $ case PSQ.lookup dcid qt of
      Nothing     -> Nothing
      Just (_,q)  -> Just q

insertRecvQDict :: IORef RecvQDict -> CID -> RecvQ -> IO ()
insertRecvQDict ref dcid q = atomicModifyIORef'' ref ins
  where
    ins (RecvQDict p qt0) = let qt1 | PSQ.size qt0 <= recvQDictSize = qt0
                                    | otherwise = PSQ.deleteMin qt0
                                qt2 = PSQ.insert dcid p q qt1
                                p' = p + 1 -- fixme: overflow
                            in RecvQDict p' qt2

----------------------------------------------------------------

data Accept = Accept {
    accVersionInfo  :: VersionInfo
  , accMyAuthCIDs   :: AuthCIDs
  , accPeerAuthCIDs :: AuthCIDs
  , accMySockAddr   :: SockAddr
  , accPeerSockAddr :: SockAddr
  , accRecvQ        :: RecvQ
  , accPacketSize   :: Int
  , accRegister     :: CID -> Connection -> IO ()
  , accUnregister   :: CID -> IO ()
  , accAddressValidated :: Bool
  , accTime         :: TimeMicrosecond
  , accWildcard     :: Bool
  }

newtype AcceptQ = AcceptQ (TQueue Accept)

newAcceptQ :: IO AcceptQ
newAcceptQ = AcceptQ <$> newTQueueIO

readAcceptQ :: AcceptQ -> IO Accept
readAcceptQ (AcceptQ q) = atomically $ readTQueue q

writeAcceptQ :: AcceptQ -> Accept -> IO ()
writeAcceptQ (AcceptQ q) x = atomically $ writeTQueue q x

accept :: Dispatch -> IO Accept
accept = readAcceptQ . acceptQ

----------------------------------------------------------------

runDispatcher :: Dispatch -> ServerConfig -> (Socket,SockAddr,Bool) -> IO ThreadId
runDispatcher d conf ssa@(s,_,_) =
    forkFinally (dispatcher d conf ssa) $ \_ -> close s

dispatcher :: Dispatch -> ServerConfig -> (Socket,SockAddr,Bool) -> IO ()
dispatcher d conf (s,mysa0,wildcard) = handleLogUnit logAction $ do
    recv <- mkRecv wildcard
    forever $ do
        (bs, mysa, peersa, cmsgs) <- safeRecv recv
        now <- getTimeMicrosecond
        send <- mkSend wildcard peersa cmsgs
        cpckts <- decodeCryptPackets bs
        let bytes = BS.length bs
            switch = dispatch d conf logAction mysa peersa wildcard send bytes now
        mapM_ switch cpckts
  where
    doDebug = isJust $ scDebugLog conf
    logAction msg | doDebug   = stdoutLogger ("dispatch(er): " <> msg)
                  | otherwise = return ()

    mkSend False peersa _     = return $ \b -> void $ NSB.sendTo s b peersa
    mkSend True  peersa cmsgs = return $ \b ->
      void $ NSB.sendMsg s peersa [b] cmsgs 0

    mkRecv False  = return $ do
        (bs, peersa) <- NSB.recvFrom s maximumUdpPayloadSize
        return (bs, mysa0, peersa, [])
    mkRecv True = do
        let (opt,cmsgid) = case mysa0 of
              SockAddrInet{}  -> (RecvIPv4PktInfo, CmsgIdIPv4PktInfo)
              SockAddrInet6{} -> (RecvIPv6PktInfo, CmsgIdIPv6PktInfo)
              _               -> error "dispatcher"
        setSocketOption s opt 1
        return $ do
            (peersa, bs, cmsgs, _) <- NSB.recvMsg s maximumUdpPayloadSize 64 0
            let pktinfo = fromJust $ lookupCmsg cmsgid cmsgs
            let mysa = case mysa0 of
                  SockAddrInet p _  -> let IPv4PktInfo _ _ addr = fromJust $ decodeCmsg pktinfo
                                       in SockAddrInet p addr
                  SockAddrInet6 p f _ sc -> let IPv6PktInfo _ addr = fromJust $ decodeCmsg pktinfo
                                            in SockAddrInet6 p f addr sc
                  _               -> error "dispatcher"
            return (bs, mysa, peersa, cmsgs)

    safeRecv recv = do
        ex <- E.tryAny $
#if defined(mingw32_HOST_OS)
                windowsThreadBlockHack $
#endif
                  recv
        case ex of
           Right x -> return x
           Left se -> case E.fromException se of
              Just e | E.ioeGetErrorType e == E.InvalidArgument -> E.throwIO se
              _ -> do
                  logAction $ "recv again: " <> bhow se
                  recv

----------------------------------------------------------------

-- If client initial is fragmented into multiple packets,
-- there is no way to put the all packets into a single queue.
-- Rather, each fragment packet is put into its own queue.
-- For the first fragment, handshake would successif others are
-- retransmitted.
-- For the other fragments, handshake will fail since its socket
-- cannot be connected.
dispatch :: Dispatch -> ServerConfig -> DebugLogger
         -> SockAddr -> SockAddr -> Bool -> (ByteString -> IO ()) -> Int -> TimeMicrosecond
         -> (CryptPacket,EncryptionLevel,Int)
         -> IO ()
dispatch Dispatch{..} ServerConfig{..} logAction
         mysa peersa wildcard send bytes tim
         (cpkt@(CryptPacket (Initial peerVer dCID sCID token) _),lvl,siz)
  | bytes < defaultQUICPacketSize = do
        logAction $ "too small " <> bhow bytes <> ", " <> bhow peersa
  | peerVer `notElem` myVersions = do
        let offerVersions
                | peerVer == GreasingVersion = GreasingVersion2 : myVersions
                | otherwise                  = GreasingVersion  : myVersions
        bss <- encodeVersionNegotiationPacket $ VersionNegotiationPacket sCID dCID offerVersions
        send bss
  | token == "" = do
        mconn <- lookupConnectionDict dstTable dCID
        case mconn of
          Nothing
            | scRequireRetry -> sendRetry
            | otherwise      -> pushToAcceptFirst False
#if defined(mingw32_HOST_OS)
          Just conn          -> writeRecvQ (connRecvQ conn) $ mkReceivedPacket cpkt tim siz lvl
#else
          _                  -> return ()
#endif
  | otherwise = do
        mct <- decryptToken tokenMgr token
        case mct of
          Just ct
            | isRetryToken ct -> do
                  ok <- isRetryTokenValid ct
                  if ok then pushToAcceptRetried ct else sendRetry
            | otherwise -> do
                  mconn <- lookupConnectionDict dstTable dCID
                  case mconn of
                    Nothing   -> pushToAcceptFirst True
#if defined(mingw32_HOST_OS)
                    Just conn -> writeRecvQ (connRecvQ conn) $ mkReceivedPacket cpkt tim siz lvl
#else
                    _       -> return ()
#endif
          _ -> sendRetry
  where
    myVersions = scVersions
    pushToAcceptQ myAuthCIDs peerAuthCIDs key addrValid = do
        mq <- lookupRecvQDict srcTable key
        case mq of
          Just q  -> writeRecvQ q $ mkReceivedPacket cpkt tim siz lvl
          Nothing -> do
              q <- newRecvQ
              insertRecvQDict srcTable key q
              writeRecvQ q $ mkReceivedPacket cpkt tim siz lvl
              let reg = registerConnectionDict dstTable
                  unreg = unregisterConnectionDict dstTable
                  ent = Accept {
                      accVersionInfo  = VersionInfo peerVer myVersions
                    , accMyAuthCIDs   = myAuthCIDs
                    , accPeerAuthCIDs = peerAuthCIDs
                    , accMySockAddr   = mysa
                    , accPeerSockAddr = peersa
                    , accRecvQ        = q
                    , accPacketSize   = bytes
                    , accRegister     = reg
                    , accUnregister   = unreg
                    , accAddressValidated = addrValid
                    , accTime         = tim
                    , accWildcard     = wildcard
                    }
              -- fixme: check acceptQ length
              writeAcceptQ acceptQ ent
    -- Initial: DCID=S1, SCID=C1 ->
    --                                     <- Initial: DCID=C1, SCID=S2
    --                               ...
    -- 1-RTT: DCID=S2 ->
    --                                                <- 1-RTT: DCID=C1
    --
    -- initial_source_connection_id       = S2   (newdCID)
    -- original_destination_connection_id = S1   (dCID)
    -- retry_source_connection_id         = Nothing
    pushToAcceptFirst addrValid = do
        newdCID <- newCID
        let myAuthCIDs = defaultAuthCIDs {
                initSrcCID  = Just newdCID
              , origDstCID  = Just dCID
              }
            peerAuthCIDs = defaultAuthCIDs {
                initSrcCID = Just sCID
              }
        pushToAcceptQ myAuthCIDs peerAuthCIDs dCID addrValid
    -- Initial: DCID=S1, SCID=C1 ->
    --                                       <- Retry: DCID=C1, SCID=S2
    -- Initial: DCID=S2, SCID=C1 ->
    --                                     <- Initial: DCID=C1, SCID=S3
    --                               ...
    -- 1-RTT: DCID=S3 ->
    --                                                <- 1-RTT: DCID=C1
    --
    -- initial_source_connection_id       = S3   (dCID)  S2 in our server
    -- original_destination_connection_id = S1   (o)
    -- retry_source_connection_id         = S2   (dCID)
    pushToAcceptRetried (CryptoToken _ _ (Just (_,_,o))) = do
        let myAuthCIDs = defaultAuthCIDs {
                initSrcCID  = Just dCID
              , origDstCID  = Just o
              , retrySrcCID = Just dCID
              }
            peerAuthCIDs = defaultAuthCIDs {
                initSrcCID = Just sCID
              }
        pushToAcceptQ myAuthCIDs peerAuthCIDs o True
    pushToAcceptRetried _ = return ()
    isRetryTokenValid (CryptoToken _tver etim (Just (l,r,_))) = do
        diff <- getElapsedTimeMicrosecond etim
        return $ diff <= Microseconds 30000000 -- fixme
              && dCID == l
              && sCID == r
#if !defined(mingw32_HOST_OS)
              -- Initial for ACK contains the retry token but
              -- the version would be already version 2, sigh.
              && _tver == peerVer
#endif
    isRetryTokenValid _ = return False
    sendRetry = do
        newdCID <- newCID
        retryToken <- generateRetryToken peerVer newdCID sCID dCID
        mnewtoken <- timeout (Microseconds 100000) $ encryptToken tokenMgr retryToken
        case mnewtoken of
          Nothing       -> logAction "retry token stacked"
          Just newtoken -> do
              bss <- encodeRetryPacket $ RetryPacket peerVer sCID newdCID newtoken (Left dCID)
              send bss
----------------------------------------------------------------
dispatch Dispatch{..} _ _
         _ _peersa _ _ _ tim
         (cpkt@(CryptPacket (RTT0 _ o _) _), lvl, siz) = do
    mq <- lookupRecvQDict srcTable o
    case mq of
      Just q  -> writeRecvQ q $ mkReceivedPacket cpkt tim siz lvl
      Nothing -> return ()
#if defined(mingw32_HOST_OS)
----------------------------------------------------------------
dispatch Dispatch{..} _ logAction
         _mysa peersa _wildcard _ _ tim
         (cpkt@(CryptPacket hdr _crypt),lvl,siz) = do
    let dCID = headerMyCID hdr
    mconn <- lookupConnectionDict dstTable dCID
    case mconn of
      Nothing   -> logAction $ "CID no match: " <> bhow dCID <> ", " <> bhow peersa
      Just conn -> writeRecvQ (connRecvQ conn) $ mkReceivedPacket cpkt tim siz lvl
#else
----------------------------------------------------------------
dispatch Dispatch{..} _ logAction
         mysa peersa wildcard _ _ tim
         ((CryptPacket hdr@(Short dCID) crypt),lvl,siz)= do
    -- fixme: packets for closed connections also match here.
    mconn <- lookupConnectionDict dstTable dCID
    case mconn of
      Nothing -> do
          logAction $ "CID no match: " <> bhow dCID <> ", " <> bhow peersa
      Just conn -> do
            alive <- getAlive conn
            when alive $ do
                let miginfo = MigrationInfo mysa peersa dCID wildcard
                    crypt' = crypt { cryptMigraionInfo = Just miginfo }
                    cpkt = CryptPacket hdr crypt'
                writeRecvQ (connRecvQ conn) $ mkReceivedPacket cpkt tim siz lvl
----------------------------------------------------------------
dispatch _ _ _ _ _ _ _ _ _ _ = return ()
#endif

----------------------------------------------------------------

-- | readerServer dies when the socket is closed.
readerServer :: Socket -> Connection -> IO ()
readerServer s conn = handleLogUnit logAction loop
  where
    loop = do
        ito <- readMinIdleTimeout conn
        mbs <- timeout ito $ NSB.recv s maximumUdpPayloadSize
        case mbs of
          Nothing -> close s
          Just bs -> do
              now <- getTimeMicrosecond
              pkts <- decodeCryptPackets bs
              mapM_ (\(p,l,siz) -> writeRecvQ (connRecvQ conn) (mkReceivedPacket p now siz l)) pkts
              loop
    logAction msg = connDebugLog conn ("debug: readerServer: " <> msg)

recvServer :: RecvQ -> IO ReceivedPacket
recvServer = readRecvQ

----------------------------------------------------------------

runNewServerReader :: Connection -> MigrationInfo -> IO ()
runNewServerReader conn (MigrationInfo mysa peersa dCID wildcard) = handleLogUnit logAction $ do
    migrating <- isPathValidating conn -- fixme: test and set
    unless migrating $ do
        setMigrationStarted conn
        -- fixme: should not block
        mcidinfo <- timeout (Microseconds 100000) $ waitPeerCID conn
        let msg = "Migration: " <> bhow peersa <> " (" <> bhow dCID <> ")"
        qlogDebug conn $ Debug $ toLogStr msg
        connDebugLog conn $ "debug: runNewServerReader: " <> msg
        E.bracketOnError setup close $ \s1 ->
            E.bracket (addSocket conn s1) close $ \_ -> do
                void $ forkIO $ readerServer s1 conn
                -- fixme: if cannot set
                setMyCID conn dCID
                validatePath conn mcidinfo
                -- holding the old socket for a while
                delay $ Microseconds 20000
  where
    setup = udpServerConnectedSocket mysa peersa wildcard
    logAction msg = connDebugLog conn ("debug: runNewServerReader: " <> msg)
