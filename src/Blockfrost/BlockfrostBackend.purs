module Cardano.Blockfrost.BlockfrostBackend (BlockfrostBackend) where

import Cardano.Provider.ServerConfig (ServerConfig)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds)

type BlockfrostBackend =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , confirmTxDelay :: Maybe Seconds
  }
