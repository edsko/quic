{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.QUIC.Recovery.Utils where

import Control.Concurrent.STM
import Data.Sequence (Seq, (<|), ViewL(..))
import qualified Data.Sequence as Seq

import Network.QUIC.Imports
import Network.QUIC.Recovery.Types
import Network.QUIC.Types

----------------------------------------------------------------

sendPing :: LDCC -> EncryptionLevel -> IO ()
sendPing LDCC{..} lvl = do
    now <- getTimeMicrosecond
    atomicModifyIORef'' (lossDetection ! lvl) $ \ld -> ld {
        timeOfLastAckElicitingPacket = now
      }
    atomically $ writeTVar ptoPing $ Just lvl

----------------------------------------------------------------

mergeLostCandidates :: LDCC -> Seq SentPacket -> IO ()
mergeLostCandidates LDCC{..} lostPackets = atomically $ do
    SentPackets old <- readTVar lostCandidates
    let new = merge old lostPackets
    writeTVar lostCandidates $ SentPackets new

mergeLostCandidatesAndClear :: LDCC -> Seq SentPacket -> IO (Seq SentPacket)
mergeLostCandidatesAndClear LDCC{..} lostPackets = atomically $ do
    SentPackets old <- readTVar lostCandidates
    writeTVar lostCandidates emptySentPackets
    return $ merge old lostPackets

merge :: Seq SentPacket -> Seq SentPacket -> Seq SentPacket
merge s1 s2 = case Seq.viewl s1 of
  EmptyL   -> s2
  x :< s1' -> case Seq.viewl s2 of
    EmptyL  -> s1
    y :< s2'
      | spPacketNumber x < spPacketNumber y -> x <| merge s1' s2
      | otherwise                           -> y <| merge s1 s2'

----------------------------------------------------------------

metricsUpdated :: LDCC -> IO () -> IO ()
metricsUpdated ldcc@LDCC{..} body = do
    rtt0 <- readIORef recoveryRTT
    cc0 <- readTVarIO recoveryCC
    body
    rtt1 <- readIORef recoveryRTT
    cc1 <- readTVarIO recoveryCC
    let diff = catMaybes [
            time "min_rtt"      (minRTT      rtt0) (minRTT      rtt1)
          , time "smoothed_rtt" (smoothedRTT rtt0) (smoothedRTT rtt1)
          , time "latest_rtt"   (latestRTT   rtt0) (latestRTT   rtt1)
          , time "rtt_variance" (rttvar      rtt0) (rttvar      rtt1)
          , numb "pto_count"    (ptoCount    rtt0) (ptoCount    rtt1)
          , numb "bytes_in_flight"   (bytesInFlight cc0) (bytesInFlight cc1)
          , numb "congestion_window" (congestionWindow cc0) (congestionWindow cc1)
          , numb "ssthresh"          (ssthresh cc0) (ssthresh cc1)
          ]
    unless (null diff) $ qlogMetricsUpdated ldcc $ MetricsDiff diff
  where
    time tag (Microseconds v0) (Microseconds v1)
      | v0 == v1  = Nothing
      | otherwise = Just (tag,v1)
    numb tag v0 v1
      | v0 == v1  = Nothing
      | otherwise = Just (tag,v1)
