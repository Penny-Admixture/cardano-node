{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Logger.Handlers.Logs.Run
  ( runLogsHandler
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, tryReadTBQueue)
import           Control.Monad (forM_, forever)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           Data.List (intercalate)
import qualified Data.Text as T

import           Cardano.Logger.Configuration
import           Cardano.Logger.Types (AcceptedItems, NodeId, NodeName, getNodeName)

runLogsHandler
  :: LoggerConfig
  -> AcceptedItems
  -> IO ()
runLogsHandler config acceptedItems = forever $ do
  threadDelay 2000000
  items <- HM.toList <$> readIORef acceptedItems
  forM_ items $ \(nodeId, (niStore, loQueue, _)) -> do
    nodeName <- maybe "" id <$> getNodeName niStore
    atomically (getAllLogObjects loQueue) >>= writeLogObjects config nodeId nodeName

getAllLogObjects :: TBQueue lo -> STM [lo]
getAllLogObjects loQueue =
  tryReadTBQueue loQueue >>= \case
    Just lo' -> (:) lo' <$> getAllLogObjects loQueue
    Nothing  -> return []

writeLogObjects
  :: Show lo
  => LoggerConfig
  -> NodeId
  -> NodeName
  -> [lo]
  -> IO ()
writeLogObjects _ _ _ [] = return ()
writeLogObjects _config nodeId nodeName logObjects =
  appendFile fileName . intercalate "\n" . map show $ logObjects
 where
  fileName = "/tmp/cardano-logger-test-" <> nodeFullId <> ".log"
  nodeFullId = if T.null nodeName
                 then show nodeId
                 else T.unpack nodeName <> "-" <> show nodeId
