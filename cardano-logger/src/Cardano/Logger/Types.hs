{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Logger.Types
  ( AcceptedItems
  , LogObjects
  , Metrics
  , NodeId (..)
  , addressToNodeId
  , initAcceptedItems
  , prepareAcceptedItems
  ) where

import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import           Control.Monad (unless)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Text (Text, pack, splitOn, unpack)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import qualified System.Metrics as EKG

import           Cardano.BM.Data.LogItem (LogObject)

import           Trace.Forward.Protocol.Type (NodeInfoStore)

import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)

data NodeId = NodeId
  { nodeIP   :: !String
  , nodePort :: !Word16
  } deriving (Eq, Generic, Hashable, Ord)

instance Show NodeId where
  show (NodeId ip port) = ip <> "-" <> show port

addressToNodeId :: String -> NodeId
addressToNodeId remoteAddress =
  -- We assume that 'remoteAddress' is a String-representation of the normal address (IP:port).
  case splitOn ":" . pack $ remoteAddress of
    [ip, port] -> NodeId (unpack ip)   (read (unpack port) :: Word16)
    _          -> NodeId remoteAddress 0 -- Unexpected format of 'remoteAddress'!

type LogObjects = TBQueue (LogObject Text)
type Metrics    = (EKG.Store, IORef MetricsLocalStore)

type AcceptedItems = IORef (HashMap NodeId (NodeInfoStore, LogObjects, Metrics))

initAcceptedItems :: IO AcceptedItems
initAcceptedItems = newIORef HM.empty

prepareAcceptedItems
  :: NodeId
  -> AcceptedItems
  -> IO ()
prepareAcceptedItems nodeId itemsIORef = do
  items' <- readIORef itemsIORef
  -- If such 'nodeId' is already presented in 'items', it means that this node
  -- already worked with the logger and now it's re-connect to the logger.
  -- No need to re-create its stores.
  unless (nodeId `HM.member` items') $ do
    niStore <- newIORef []
    loQueue <- newTBQueueIO 2000
    ekgStore <- EKG.newStore
    localStore <- newIORef emptyMetricsLocalStore
    let storesForNewNode = (niStore, loQueue, (ekgStore, localStore))
    atomicModifyIORef' itemsIORef $ \items ->
      (HM.insert nodeId storesForNewNode items, ())
