module Cardano.Blockfrost.BlockfrostProtocolParameters
  ( BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  , BlockfrostProtocolParametersRaw
  , FiniteBigNumber(FiniteBigNumber)
  , Stringed(Stringed)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, AtKey, MissingValue)
  , decodeAeson
  , decodeJsonString
  , encodeAeson
  , finiteNumber
  , stringifyAeson
  , unFinite
  )
import Cardano.Types (Language(PlutusV3, PlutusV2, PlutusV1), UnitInterval(UnitInterval))
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.Epoch (Epoch(Epoch))
import Cardano.Types.ExUnitPrices (ExUnitPrices(ExUnitPrices))
import Cardano.Types.ExUnits (ExUnits(ExUnits))
import Cardano.Types.Int (Int) as Cardano
import Cardano.Types.ProtocolParameters (ProtocolParameters(ProtocolParameters))
import Cardano.Types.Rational (Rational, reduce)
import Data.BigNumber (BigNumber, toFraction)
import Data.BigNumber as BigNumber
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe, fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Foreign.Object (Object)
import Foreign.Object as Object
import JS.BigInt (fromString) as BigInt
import Partial.Unsafe (unsafePartial)

-- | `Stringed a` decodes an `a` that was encoded as a `String`
newtype Stringed a = Stringed a

derive instance Generic (Stringed a) _

derive instance Newtype (Stringed a) _

instance Show a => Show (Stringed a) where
  show = genericShow

instance DecodeAeson a => DecodeAeson (Stringed a) where
  decodeAeson = decodeAeson >=> decodeJsonString >=> Stringed >>> pure

instance EncodeAeson a => EncodeAeson (Stringed a) where
  encodeAeson =
    encodeAeson
      <<< stringifyAeson
      <<< encodeAeson
      <<< unwrap

-- TODO: Ensure FiniteBigNumber handling is sensible

newtype FiniteBigNumber = FiniteBigNumber BigNumber

derive instance Generic FiniteBigNumber _

derive instance Newtype FiniteBigNumber _

instance Show FiniteBigNumber where
  show = genericShow

instance DecodeAeson FiniteBigNumber where
  decodeAeson =
    map (FiniteBigNumber <<< BigNumber.fromNumber <<< unFinite)
      <<< decodeAeson

instance EncodeAeson FiniteBigNumber where
  encodeAeson =
    encodeAeson
      <<< unsafePartial fromJust
      <<< finiteNumber
      <<< BigNumber.toNumber
      <<< unwrap

type BlockfrostProtocolParametersRaw =
  { epoch :: Epoch
  , min_fee_a :: UInt
  , min_fee_b :: UInt
  , max_block_size :: UInt
  , max_tx_size :: UInt
  , max_block_header_size :: UInt
  , key_deposit :: Stringed BigNum
  , pool_deposit :: Stringed BigNum
  , e_max :: UInt
  , n_opt :: UInt
  , a0 :: FiniteBigNumber
  , rho :: FiniteBigNumber
  , tau :: FiniteBigNumber
  , decentralisation_param :: FiniteBigNumber
  , extra_entropy :: Maybe String
  , protocol_major_ver :: UInt
  , protocol_minor_ver :: UInt
  , min_utxo :: Stringed BigNum -- deprecated
  , min_pool_cost :: Stringed BigNum
  , nonce :: String
  , cost_models :: Maybe (Object (Object Cardano.Int)) -- deprecated
  , price_mem :: Maybe FiniteBigNumber
  , price_step :: Maybe FiniteBigNumber
  , max_tx_ex_mem :: Maybe (Stringed BigNum)
  , max_tx_ex_steps :: Maybe (Stringed BigNum)
  , max_block_ex_mem :: Maybe (Stringed BigNum)
  , max_block_ex_steps :: Maybe (Stringed BigNum)
  , max_val_size :: Maybe (Stringed UInt)
  , collateral_percent :: Maybe UInt
  , max_collateral_inputs :: Maybe UInt
  , coins_per_utxo_size :: Maybe (Stringed BigNum)
  , coins_per_utxo_word :: Maybe (Stringed BigNum) -- deprecated
  , pvt_motion_no_confidence :: Maybe FiniteBigNumber
  , pvt_committee_normal :: Maybe FiniteBigNumber
  , pvt_committee_no_confidence :: Maybe FiniteBigNumber
  , pvt_hard_fork_initiation :: Maybe FiniteBigNumber
  , dvt_motion_no_confidence :: Maybe FiniteBigNumber
  , dvt_committee_normal :: Maybe FiniteBigNumber
  , dvt_committee_no_confidence :: Maybe FiniteBigNumber
  , dvt_update_to_constitution :: Maybe FiniteBigNumber
  , dvt_hard_fork_initiation :: Maybe FiniteBigNumber
  , dvt_p_p_network_group :: Maybe FiniteBigNumber
  , dvt_p_p_economic_group :: Maybe FiniteBigNumber
  , dvt_p_p_technical_group :: Maybe FiniteBigNumber
  , dvt_p_p_gov_group :: Maybe FiniteBigNumber
  , dvt_treasury_withdrawal :: Maybe FiniteBigNumber
  , committee_min_size :: Maybe (Stringed UInt)
  , committee_max_term_length :: Maybe (Stringed UInt)
  , gov_action_lifetime :: Maybe (Stringed UInt)
  , gov_action_deposit :: Maybe (Stringed BigNum)
  , drep_deposit :: Maybe (Stringed BigNum)
  , drep_activity :: Maybe (Stringed UInt)
  , pvtpp_security_group :: Maybe FiniteBigNumber -- deprecated
  , pvt_p_p_security_group :: Maybe FiniteBigNumber
  , min_fee_ref_script_cost_per_byte :: Maybe FiniteBigNumber
  , cost_models_raw :: Maybe (Object (Array Cardano.Int))
  }

toFraction' :: BigNumber -> String /\ String
toFraction' bn =
  (BigNumber.toString numerator /\ BigNumber.toString denominator)
  where
  (numerator /\ denominator) = toFraction bn
    (BigNumber.fromNumber infinity)

bigNumberToRational :: FiniteBigNumber -> Either JsonDecodeError Rational
bigNumberToRational (FiniteBigNumber bn) = note (TypeMismatch "Rational") do
  numerator <- BigInt.fromString numerator'
  denominator <- BigInt.fromString denominator'
  reduce numerator denominator
  where
  (numerator' /\ denominator') = toFraction' bn

bigNumberToPrice
  :: FiniteBigNumber
  -> Either JsonDecodeError UnitInterval
bigNumberToPrice (FiniteBigNumber bn) = note (TypeMismatch "Rational") do
  numerator <- BigNum.fromString numerator'
  denominator <- BigNum.fromString denominator'
  pure $ UnitInterval { numerator, denominator }
  where
  (numerator' /\ denominator') = toFraction' bn

newtype BlockfrostProtocolParameters =
  BlockfrostProtocolParameters ProtocolParameters

derive instance Generic BlockfrostProtocolParameters _
derive instance Newtype BlockfrostProtocolParameters _

instance Show BlockfrostProtocolParameters where
  show = genericShow

instance DecodeAeson BlockfrostProtocolParameters where
  decodeAeson = decodeAeson >=> \(raw :: BlockfrostProtocolParametersRaw) -> do
    poolPledgeInfluence <- bigNumberToRational raw.a0
    monetaryExpansion <- bigNumberToRational raw.rho
    treasuryCut <- bigNumberToRational raw.tau
    memPrice <-
      maybe (Left $ AtKey "price_mem" MissingValue) bigNumberToPrice
        raw.price_mem
    stepPrice <-
      maybe (Left $ AtKey "price_step" MissingValue) bigNumberToPrice
        raw.price_step
    let prices = ExUnitPrices { memPrice, stepPrice }
    coinsPerUtxoByte <-
      maybe (Left $ AtKey "coins_per_utxo_size" MissingValue)
        pure $ (Coin <<< unwrap <$> raw.coins_per_utxo_size)
    refScriptCoinsPerByte <-
      maybe
        (Left $ AtKey "min_fee_ref_script_cost_per_byte" MissingValue)
        bigNumberToRational
        raw.min_fee_ref_script_cost_per_byte
    costModels <-
      note (AtKey "cost_models_raw" MissingValue)
        raw.cost_models_raw
    plutusV1CostModel <-
      maybe
        (Left $ AtKey "cost_models_raw" $ AtKey "PlutusV1" MissingValue)
        (pure <<< wrap)
        (Object.lookup "PlutusV1" costModels)
    plutusV2CostModel <-
      maybe
        (Left $ AtKey "cost_models_raw" $ AtKey "PlutusV2" MissingValue)
        (pure <<< wrap)
        (Object.lookup "PlutusV2" costModels)
    plutusV3CostModel <-
      maybe
        (Left $ AtKey "cost_models_raw" $ AtKey "PlutusV3" MissingValue)
        (pure <<< wrap)
        (Object.lookup "PlutusV3" costModels)
    collateralPercent <-
      note (AtKey "collateral_percent" MissingValue)
        raw.collateral_percent
    maxCollateralInputs <-
      note (AtKey "max_collateral_inputs" MissingValue)
        raw.max_collateral_inputs
    maxTxExMem <-
      note (AtKey "max_tx_ex_mem" MissingValue)
        raw.max_tx_ex_mem
    maxTxExSteps <-
      note (AtKey "max_tx_ex_steps" MissingValue)
        raw.max_tx_ex_steps
    maxBlockExMem <-
      note (AtKey "max_block_ex_mem" MissingValue)
        raw.max_block_ex_mem
    maxBlockExSteps <-
      note (AtKey "max_block_ex_steps" MissingValue)
        raw.max_block_ex_steps
    maxValueSize <-
      unwrap <$> note (AtKey "max_val_size" MissingValue)
        raw.max_val_size
    govActionDeposit <-
      Coin <<< unwrap <$> note (AtKey "gov_action_deposit" MissingValue)
        raw.gov_action_deposit
    drepDeposit <-
      Coin <<< unwrap <$> note (AtKey "drep_deposit" MissingValue)
        raw.drep_deposit
    pure $ BlockfrostProtocolParameters $ ProtocolParameters
      { protocolVersion: raw.protocol_major_ver /\ raw.protocol_minor_ver
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , maxBlockHeaderSize: raw.max_block_header_size
      , maxBlockBodySize: raw.max_block_size
      , maxTxSize: raw.max_tx_size
      , txFeeFixed: Coin $ BigNum.fromUInt raw.min_fee_b
      , txFeePerByte: raw.min_fee_a
      , stakeAddressDeposit: Coin $ unwrap raw.key_deposit
      , stakePoolDeposit: Coin $ unwrap raw.pool_deposit
      , minPoolCost: Coin $ unwrap raw.min_pool_cost
      , poolRetireMaxEpoch: Epoch raw.e_max
      , stakePoolTargetNum: raw.n_opt
      , poolPledgeInfluence
      , monetaryExpansion
      , treasuryCut
      , coinsPerUtxoByte: coinsPerUtxoByte
      , costModels: Map.fromFoldable
          [ PlutusV1 /\ plutusV1CostModel
          , PlutusV2 /\ plutusV2CostModel
          , PlutusV3 /\ plutusV3CostModel
          ]
      , prices
      , maxTxExUnits:
          ExUnits
            { mem: unwrap maxTxExMem
            , steps: unwrap maxTxExSteps
            }
      , maxBlockExUnits:
          ExUnits
            { mem: unwrap maxBlockExMem
            , steps: unwrap maxBlockExSteps
            }
      , maxValueSize
      , collateralPercent
      , maxCollateralInputs
      , govActionDeposit
      , drepDeposit
      , refScriptCoinsPerByte
      }
