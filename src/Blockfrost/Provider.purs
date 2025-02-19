module Cardano.Blockfrost.Provider (providerForBlockfrostBackend) where

import Prelude

import Cardano.Blockfrost.Service (BlockfrostServiceM, doesTxExist, evaluateTx, getChainTip, getCurrentEpoch, getDatumByHash, getEraSummaries, getOutputAddressesByTxHash, getPoolIds, getPubKeyHashDelegationsAndRewards, getScriptByHash, getTxAuxiliaryData, getUtxoByOref, getValidatorHashDelegationsAndRewards, submitTx, utxosAt)
import Cardano.Provider.Type (Provider)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Exception (error)

providerForBlockfrostBackend
  :: (forall (a :: Type). BlockfrostServiceM a -> Aff a) -> Provider
providerForBlockfrostBackend runBlockfrostServiceM' =
  { getDatumByHash: runBlockfrostServiceM' <<< getDatumByHash
  , getScriptByHash: runBlockfrostServiceM' <<< getScriptByHash
  , getUtxoByOref: runBlockfrostServiceM' <<< getUtxoByOref
  , getOutputAddressesByTxHash: runBlockfrostServiceM' <<<
      getOutputAddressesByTxHash
  , doesTxExist: runBlockfrostServiceM' <<< doesTxExist
  , getTxAuxiliaryData: runBlockfrostServiceM' <<< getTxAuxiliaryData
  , utxosAt: runBlockfrostServiceM' <<< utxosAt
  , getChainTip: runBlockfrostServiceM' getChainTip
  , getCurrentEpoch:
      runBlockfrostServiceM' getCurrentEpoch >>= case _ of
        Right epoch -> pure epoch
        Left err -> throwError $ error $ show err
  , submitTx: runBlockfrostServiceM' <<< submitTx
  , evaluateTx: \tx additionalUtxos ->
      runBlockfrostServiceM' $ evaluateTx tx additionalUtxos
  , getEraSummaries: runBlockfrostServiceM' getEraSummaries
  , getPoolIds: runBlockfrostServiceM' getPoolIds
  , getPubKeyHashDelegationsAndRewards: \networkId stakePubKeyHash ->
      runBlockfrostServiceM'
        ( getPubKeyHashDelegationsAndRewards networkId
            stakePubKeyHash
        )
  , getValidatorHashDelegationsAndRewards: \networkId stakeValidatorHash ->
      runBlockfrostServiceM'
        ( getValidatorHashDelegationsAndRewards networkId
            (wrap stakeValidatorHash)
        )
  }
