{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logger.Configuration
  ( Host
  , Port
  , RemoteAddr (..)
  , Endpoint (..)
  , RotationParams (..)
  , LogMode (..)
  , LogFormat (..)
  , LoggingParams (..)
  , LoggerConfig (..)
  , readLoggerConfig
  ) where

import           Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict')
import           Data.Fixed (Pico)
import           Data.Word (Word16, Word64)
import           GHC.Generics (Generic)
import qualified System.Exit as Ex

type Host = String
type Port = Int

data RemoteAddr
  = LocalPipe !FilePath
  | RemoteSocket !Host !Port
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data Endpoint = Endpoint !Host !Port
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data RotationParams = RotationParams
  { rpLogLimitBytes :: !Word64  -- ^ Max size of file in bytes
  , rpMaxAgeHours   :: !Word    -- ^ Hours
  , rpKeepFilesNum  :: !Word    -- ^ Number of files to keep
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LogMode
  = FileMode
  | JournalMode
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LogFormat
  = AsText
  | AsJSON
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LoggingParams = LoggingParams
  { logRoot   :: !FilePath
  , logMode   :: !LogMode
  , logFormat :: !LogFormat
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LoggerConfig = LoggerConfig
  { acceptAt       :: !RemoteAddr
  , loRequestNum   :: !Word16 -- ^ How many 'LogObject's in one request.
  , ekgRequestFreq :: !Pico   -- ^ How often to request EKG-metrics.
  , hasEKG         :: !(Maybe Endpoint)
  , hasPrometheus  :: !(Maybe Endpoint)
  , logging        :: ![LoggingParams]
  , rotation       :: !(Maybe RotationParams)
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Reads the logger's configuration file (path is passed via '--config' CLI option).
readLoggerConfig :: FilePath -> IO LoggerConfig
readLoggerConfig pathToConfig =
  eitherDecodeFileStrict' pathToConfig >>= \case
    Left e -> Ex.die $ "Invalid logger's configuration: " <> show e
    Right (config :: LoggerConfig) -> return config
