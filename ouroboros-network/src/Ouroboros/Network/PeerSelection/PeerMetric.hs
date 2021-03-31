{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}


module Ouroboros.Network.PeerSelection.PeerMetric where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.NodeToNode ( ConnectionId (..))

-- The maximum numbers of slots we will store data for.
-- On some chains sometimes this corresponds to 1h
-- worth of metrics *sighs*.
maxSlotsToTrack :: Int
maxSlotsToTrack = 180


type ReportHeaderMetricsSTM m = (SlotNo -> Time -> STM m ())

type SlotMetric p = Map SlotNo (p, Time)

data PeerMetrics m p = PeerMetrics {
    headerMetrics :: StrictTVar m (SlotMetric p)
  }

addHeaderMetric
    :: forall m p.
       ( MonadSTM m )
    => PeerMetrics m p
    -> (ConnectionId p)
    -> SlotNo
    -> Time
    -> STM m ()
addHeaderMetric PeerMetrics{headerMetrics} con slotNo time =
     addMetrics headerMetrics (remoteAddress con) slotNo time


getHeaderMetrics
    :: MonadSTM m
    => PeerMetrics m p
    -> STM m (SlotMetric p)
getHeaderMetrics PeerMetrics{headerMetrics} = readTVar headerMetrics

addMetrics
    :: forall m p.  ( MonadSTM m )
    => StrictTVar m (SlotMetric p)
    -> p
    -> SlotNo
    -> Time
    -> STM m ()
addMetrics metricsVar !peer !slot !time = do
    metrics <- readTVar metricsVar
    case Map.lookup slot metrics of
         Nothing -> do
             let metrics' = Map.insert slot (peer, time) metrics
             if Map.size metrics' > maxSlotsToTrack
                then
                  let ((minSlotNo, _), metrics'') = Map.deleteFindMin metrics' in
                  if minSlotNo == slot
                     then return ()
                     else writeTVar metricsVar metrics''
             else writeTVar metricsVar metrics'
         Just (_, oldTime) ->
             if oldTime <= time
                then return ()
                else writeTVar metricsVar (Map.insert slot (peer, time) metrics)

newPeerMetric
    :: MonadSTM m
    => m (PeerMetrics m p)
newPeerMetric = do
  hs <- newTVarIO Map.empty
  return $ PeerMetrics hs

-- Returns a Map which counts the number of times a given peer
-- was the first to present us with a block/header.
upstreamyness
    :: forall p.  ( Ord p )
    => SlotMetric p
    -> Map p Int
upstreamyness = Map.foldl' count Map.empty
  where
    count :: Map p Int
          -> (p,Time)
          -> Map p Int
    count m (peer,_) =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing = Just 1
        fn (Just c) = Just $! c + 1



