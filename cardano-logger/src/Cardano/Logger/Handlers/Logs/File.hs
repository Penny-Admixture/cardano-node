{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logger.Handlers.Logs.File
  ( writeLogObjectsToFile
  ) where

import           Control.Exception (IOException, try)
import           Data.Aeson (ToJSON, Value (..), (.=), object, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath.Posix ((</>), (<.>))
import           System.IO (hPutStrLn, stderr)

import           Cardano.BM.Data.LogItem

import           Cardano.Logger.Configuration
import           Cardano.Logger.Types (NodeId, NodeName)

writeLogObjectsToFile
  :: ToJSON a
  => NodeId
  -> NodeName
  -> FilePath
  -> LogFormat
  -> [LogObject a]
  -> IO ()
writeLogObjectsToFile nodeId nodeName rootDir format logObjects =
  try (createDirectoryIfMissing True dirForLogs) >>= \case
    Left (e :: IOException) ->
      hPutStrLn stderr $ "Cannot write log items to file: " <> show e
    Right _ ->
      doWriteLogObjects pathToCurrentLog (loFormatter format) logObjects
 where
  dirForLogs = rootDir </> nodeFullId
  nodeFullId = if T.null nodeName
                 then show nodeId
                 else T.unpack nodeName <> "-" <> show nodeId
  -- This is a symlink to the current log file, please see rotation parameters.
  pathToCurrentLog = dirForLogs </> "node" <.> extension format

  extension AsText = "log"
  extension AsJSON = "json"

  loFormatter AsText = loToText
  loFormatter AsJSON = loToJSON

  doWriteLogObjects logPath formatIt =
    TLIO.appendFile logPath . TL.append nl . TL.intercalate nl . map formatIt

#if defined(mingw32_HOST_OS)
  nl = "\r\n"
#else
  nl = "\n"
#endif

loToText :: ToJSON a => LogObject a -> TL.Text
loToText (LogObject loname lometa loitem) =
  if TL.null msg
    then ""
    else "[" <> host <> name <> ":" <> sev <> ":" <> thId <> "] [" <> time <> "] " <> msg
 where
  host = if TL.null host' then "" else host' <> ":"
  host' = TL.fromStrict . hostname $ lometa
  name = TL.fromStrict loname
  sev = TL.pack . show . severity $ lometa
  thId = TL.filter isDigit (TL.pack . show . tid $ lometa)
  time = TL.pack . formatTime defaultTimeLocale "%F %T%2Q %Z" $ tstamp lometa
  msg =
    case loitem of
      (LogMessage logItem) ->
        case toJSON logItem of
          (String m) -> TL.fromStrict m
          m          -> encodeToLazyText m
      (LogError m) -> TL.fromStrict m
      (LogStructured o) -> encodeToLazyText o
      (LogStructuredText _o m) -> TL.fromStrict m
      (LogValue name' value) ->
        if T.null name'
          then TL.pack (show value)
          else TL.fromStrict name' <> " = " <> TL.pack (show value)
      (ObserveDiff _) -> encodeToLazyText loitem
      (ObserveOpen _) -> encodeToLazyText loitem
      (ObserveClose _) -> encodeToLazyText loitem
      (AggregatedMessage _) -> encodeToLazyText loitem
      (MonitoringEffect _) -> encodeToLazyText loitem
      KillPill -> ""
      Command _ -> ""

data LogObjectForJSON = LogObjectForJSON
  { jAt   :: !UTCTime
  , jNS   :: !T.Text
  , jData :: !Value
  , jHost :: !T.Text
  , jSev  :: !T.Text
  , jTId  :: !T.Text
  }

instance ToJSON LogObjectForJSON where
  toJSON LogObjectForJSON{..} =
    object [ "at"     .= formatTime defaultTimeLocale "%FT%T%2Q%Z" jAt
           , "ns"     .= jNS
           , "data"   .= jData
           , "host"   .= jHost
           , "sev"    .= jSev
           , "thread" .= jTId
           ]

loToJSON :: ToJSON a => LogObject a -> TL.Text
loToJSON (LogObject loname lometa loitem) = encodeToLazyText $
  LogObjectForJSON
    { jAt   = tstamp lometa
    , jNS   = loname
    , jData = toJSON loitem
    , jHost = hostname lometa
    , jSev  = T.pack . show . severity $ lometa
    , jTId  = T.filter isDigit (T.pack . show . tid $ lometa)
    }
