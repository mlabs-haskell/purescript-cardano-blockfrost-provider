module Demo.Cardano.Blockfrost.Governance
  ( main
  ) where

import Cardano.Blockfrost
import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types (GovId(GovCredential, GovAction))
import Cardano.Types.GovId (fromBech32, toBech32) as GovId
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Logger.Trans (runLoggerT)
import Control.Monad.Reader (runReaderT)
import Data.ByteArray (hexToByteArray)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.UInt (fromInt) as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main =
  launchAff_ do
    blockfrostApiKey <- do
      let envVar = "BLOCKFROST_API_KEY"
      liftEffect $
        lookupEnv envVar >>=
          liftMaybe
            ( error $ envVar <>
                " env var not set. Expected Blockfrost API key for Preprod network."
            )
    let
      provider = providerForBlockfrostBackend (runner blockfrostApiKey)
      proposalRef =
        wrap
          { transactionId: unsafePartial fromJust $ decodeCbor <<< wrap =<< hexToByteArray
              "78a9aafe2e4e14828efa8cd5202fec08c996a9a00c7d56b317b6a95a80510db3"
          , index: UInt.fromInt 0
          }
    liftEffect $ log $ "Proposal's GovId: " <> GovId.toBech32 (GovAction proposalRef)
    proposal <- provider.getProposalById proposalRef
    liftEffect $ log $ "Proposal: " <> show proposal
    votes <- provider.getVotesOnProposal proposalRef
    liftEffect $ log $ "Votes: " <> show votes
    govId <-
      liftMaybe (error "Could not decode GovId from Bech32 string") $
        GovId.fromBech32 "drep1ytwmwvtd0a8lr45ssner2tjxzv5y8q03w3606yeald9mdmgmwecja"
    case govId of
      GovCredential { cred: drepCred } -> do
        drepInfo <- provider.getRegisteredDrepInfo drepCred
        liftEffect $ log $ "DRep info: " <> show drepInfo
      _ -> throwError $ error "Unexpected GovId"

runner :: forall (a :: Type). String -> BlockfrostServiceM a -> Aff a
runner blockfrostApiKey action =
  runReaderT (runLoggerT action (const (pure unit))) $
    { blockfrostConfig:
        { port: UInt.fromInt 443
        , host: "cardano-preprod.blockfrost.io"
        , secure: true
        , path: Just "/api/v0"
        }
    , blockfrostApiKey: Just blockfrostApiKey
    , onBlockfrostRawGetResponse: Nothing
    , onBlockfrostRawPostResponse: Nothing
    }
