module Demo.Cardano.Blockfrost.Governance
  ( main
  ) where

import Cardano.Blockfrost
import Prelude

import Cardano.AsCbor (decodeCbor)
import Control.Monad.Error.Class (liftMaybe)
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
              "c9a88e24e627f717e2d0c81c09fedc1208d229f633f467c5dd2337ba123b4e41"
          , index: UInt.fromInt 0
          }
    proposal <- provider.getProposalById proposalRef
    liftEffect $ log $ "Proposal: " <> show proposal
    votes <- provider.getVotesOnProposal proposalRef
    liftEffect $ log $ "Votes: " <> show votes

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
