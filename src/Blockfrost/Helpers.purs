module Cardano.Blockfrost.Helpers (decodeAssetClass) where

import Prelude

import Aeson (JsonDecodeError(..))
import Cardano.AsCbor (decodeCbor)
import Cardano.Types (AssetName, ScriptHash)
import Cardano.Types.AssetName (mkAssetName)
import Control.Apply (lift2)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either, note)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))

decodeAssetClass
  :: String
  -> String
  -> String
  -> Either JsonDecodeError (Tuple ScriptHash AssetName)
decodeAssetClass assetString csString tnString =
  lift2 Tuple
    ( note (assetStringTypeMismatch "CurrencySymbol" csString)
        (decodeCbor <<< wrap =<< hexToByteArray csString)
    )
    ( note (assetStringTypeMismatch "AssetName" tnString)
        (mkAssetName =<< hexToByteArray tnString)
    )
  where
  assetStringTypeMismatch :: String -> String -> JsonDecodeError
  assetStringTypeMismatch t actual =
    TypeMismatch $
      ("In " <> assetString <> ": Expected hex-encoded " <> t)
        <> (", got: " <> actual)