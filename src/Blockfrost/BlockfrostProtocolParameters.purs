module Cardano.Blockfrost.BlockfrostProtocolParameters
  ( BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  ) where

import Prelude

import Aeson (class DecodeAeson, JsonDecodeError(TypeMismatch, AtKey, MissingValue), decodeAeson, decodeJsonString)
import Data.List as List
import Foreign.Object as Object
import Cardano.Types (Language(PlutusV3, PlutusV2, PlutusV1), UnitInterval(UnitInterval))
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.Epoch (Epoch(Epoch))
import Cardano.Types.ExUnitPrices (ExUnitPrices(ExUnitPrices))
import Cardano.Types.ExUnits (ExUnits(ExUnits))
import Cardano.Types.Int (Int) as Cardano
import Cardano.Types.ProtocolParameters (ProtocolParameters(ProtocolParameters))
import Cardano.Types.Rational (Rational, reduce)
import Cardano.Types.Rational as Rational
import Control.Alt ((<|>))
import Data.Array as Array
import Data.BigNumber (BigNumber, toFraction)
import Data.BigNumber as BigNumber
import Data.Bitraversable (ltraverse)
import Data.Either (Either(Left), hush, note)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Foreign.Object (Object)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import JS.BigInt (fromString) as BigInt
import Cardano.Types.Int (fromString) as Cardano.Int

--------------------------------------------------------------------------------
-- Cost models conversions
--------------------------------------------------------------------------------

-- | A type that represents a JSON-encoded Costmodel in format used by Ogmios
type CostModelV1 =
  ( "addInteger-cpu-arguments-intercept" :: Cardano.Int
  , "addInteger-cpu-arguments-slope" :: Cardano.Int
  , "addInteger-memory-arguments-intercept" :: Cardano.Int
  , "addInteger-memory-arguments-slope" :: Cardano.Int
  , "appendByteString-cpu-arguments-intercept" :: Cardano.Int
  , "appendByteString-cpu-arguments-slope" :: Cardano.Int
  , "appendByteString-memory-arguments-intercept" :: Cardano.Int
  , "appendByteString-memory-arguments-slope" :: Cardano.Int
  , "appendString-cpu-arguments-intercept" :: Cardano.Int
  , "appendString-cpu-arguments-slope" :: Cardano.Int
  , "appendString-memory-arguments-intercept" :: Cardano.Int
  , "appendString-memory-arguments-slope" :: Cardano.Int
  , "bData-cpu-arguments" :: Cardano.Int
  , "bData-memory-arguments" :: Cardano.Int
  , "blake2b_256-cpu-arguments-intercept" :: Cardano.Int
  , "blake2b_256-cpu-arguments-slope" :: Cardano.Int
  , "blake2b_256-memory-arguments" :: Cardano.Int
  , "cekApplyCost-exBudgetCPU" :: Cardano.Int
  , "cekApplyCost-exBudgetMemory" :: Cardano.Int
  , "cekBuiltinCost-exBudgetCPU" :: Cardano.Int
  , "cekBuiltinCost-exBudgetMemory" :: Cardano.Int
  , "cekConstCost-exBudgetCPU" :: Cardano.Int
  , "cekConstCost-exBudgetMemory" :: Cardano.Int
  , "cekDelayCost-exBudgetCPU" :: Cardano.Int
  , "cekDelayCost-exBudgetMemory" :: Cardano.Int
  , "cekForceCost-exBudgetCPU" :: Cardano.Int
  , "cekForceCost-exBudgetMemory" :: Cardano.Int
  , "cekLamCost-exBudgetCPU" :: Cardano.Int
  , "cekLamCost-exBudgetMemory" :: Cardano.Int
  , "cekStartupCost-exBudgetCPU" :: Cardano.Int
  , "cekStartupCost-exBudgetMemory" :: Cardano.Int
  , "cekVarCost-exBudgetCPU" :: Cardano.Int
  , "cekVarCost-exBudgetMemory" :: Cardano.Int
  , "chooseData-cpu-arguments" :: Cardano.Int
  , "chooseData-memory-arguments" :: Cardano.Int
  , "chooseList-cpu-arguments" :: Cardano.Int
  , "chooseList-memory-arguments" :: Cardano.Int
  , "chooseUnit-cpu-arguments" :: Cardano.Int
  , "chooseUnit-memory-arguments" :: Cardano.Int
  , "consByteString-cpu-arguments-intercept" :: Cardano.Int
  , "consByteString-cpu-arguments-slope" :: Cardano.Int
  , "consByteString-memory-arguments-intercept" :: Cardano.Int
  , "consByteString-memory-arguments-slope" :: Cardano.Int
  , "constrData-cpu-arguments" :: Cardano.Int
  , "constrData-memory-arguments" :: Cardano.Int
  , "decodeUtf8-cpu-arguments-intercept" :: Cardano.Int
  , "decodeUtf8-cpu-arguments-slope" :: Cardano.Int
  , "decodeUtf8-memory-arguments-intercept" :: Cardano.Int
  , "decodeUtf8-memory-arguments-slope" :: Cardano.Int
  , "divideInteger-cpu-arguments-constant" :: Cardano.Int
  , "divideInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "divideInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "divideInteger-memory-arguments-intercept" :: Cardano.Int
  , "divideInteger-memory-arguments-minimum" :: Cardano.Int
  , "divideInteger-memory-arguments-slope" :: Cardano.Int
  , "encodeUtf8-cpu-arguments-intercept" :: Cardano.Int
  , "encodeUtf8-cpu-arguments-slope" :: Cardano.Int
  , "encodeUtf8-memory-arguments-intercept" :: Cardano.Int
  , "encodeUtf8-memory-arguments-slope" :: Cardano.Int
  , "equalsByteString-cpu-arguments-constant" :: Cardano.Int
  , "equalsByteString-cpu-arguments-intercept" :: Cardano.Int
  , "equalsByteString-cpu-arguments-slope" :: Cardano.Int
  , "equalsByteString-memory-arguments" :: Cardano.Int
  , "equalsData-cpu-arguments-intercept" :: Cardano.Int
  , "equalsData-cpu-arguments-slope" :: Cardano.Int
  , "equalsData-memory-arguments" :: Cardano.Int
  , "equalsInteger-cpu-arguments-intercept" :: Cardano.Int
  , "equalsInteger-cpu-arguments-slope" :: Cardano.Int
  , "equalsInteger-memory-arguments" :: Cardano.Int
  , "equalsString-cpu-arguments-constant" :: Cardano.Int
  , "equalsString-cpu-arguments-intercept" :: Cardano.Int
  , "equalsString-cpu-arguments-slope" :: Cardano.Int
  , "equalsString-memory-arguments" :: Cardano.Int
  , "fstPair-cpu-arguments" :: Cardano.Int
  , "fstPair-memory-arguments" :: Cardano.Int
  , "headList-cpu-arguments" :: Cardano.Int
  , "headList-memory-arguments" :: Cardano.Int
  , "iData-cpu-arguments" :: Cardano.Int
  , "iData-memory-arguments" :: Cardano.Int
  , "ifThenElse-cpu-arguments" :: Cardano.Int
  , "ifThenElse-memory-arguments" :: Cardano.Int
  , "indexByteString-cpu-arguments" :: Cardano.Int
  , "indexByteString-memory-arguments" :: Cardano.Int
  , "lengthOfByteString-cpu-arguments" :: Cardano.Int
  , "lengthOfByteString-memory-arguments" :: Cardano.Int
  , "lessThanByteString-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanByteString-cpu-arguments-slope" :: Cardano.Int
  , "lessThanByteString-memory-arguments" :: Cardano.Int
  , "lessThanEqualsByteString-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanEqualsByteString-cpu-arguments-slope" :: Cardano.Int
  , "lessThanEqualsByteString-memory-arguments" :: Cardano.Int
  , "lessThanEqualsInteger-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanEqualsInteger-cpu-arguments-slope" :: Cardano.Int
  , "lessThanEqualsInteger-memory-arguments" :: Cardano.Int
  , "lessThanInteger-cpu-arguments-intercept" :: Cardano.Int
  , "lessThanInteger-cpu-arguments-slope" :: Cardano.Int
  , "lessThanInteger-memory-arguments" :: Cardano.Int
  , "listData-cpu-arguments" :: Cardano.Int
  , "listData-memory-arguments" :: Cardano.Int
  , "mapData-cpu-arguments" :: Cardano.Int
  , "mapData-memory-arguments" :: Cardano.Int
  , "mkCons-cpu-arguments" :: Cardano.Int
  , "mkCons-memory-arguments" :: Cardano.Int
  , "mkNilData-cpu-arguments" :: Cardano.Int
  , "mkNilData-memory-arguments" :: Cardano.Int
  , "mkNilPairData-cpu-arguments" :: Cardano.Int
  , "mkNilPairData-memory-arguments" :: Cardano.Int
  , "mkPairData-cpu-arguments" :: Cardano.Int
  , "mkPairData-memory-arguments" :: Cardano.Int
  , "modInteger-cpu-arguments-constant" :: Cardano.Int
  , "modInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "modInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "modInteger-memory-arguments-intercept" :: Cardano.Int
  , "modInteger-memory-arguments-minimum" :: Cardano.Int
  , "modInteger-memory-arguments-slope" :: Cardano.Int
  , "multiplyInteger-cpu-arguments-intercept" :: Cardano.Int
  , "multiplyInteger-cpu-arguments-slope" :: Cardano.Int
  , "multiplyInteger-memory-arguments-intercept" :: Cardano.Int
  , "multiplyInteger-memory-arguments-slope" :: Cardano.Int
  , "nullList-cpu-arguments" :: Cardano.Int
  , "nullList-memory-arguments" :: Cardano.Int
  , "quotientInteger-cpu-arguments-constant" :: Cardano.Int
  , "quotientInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "quotientInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "quotientInteger-memory-arguments-intercept" :: Cardano.Int
  , "quotientInteger-memory-arguments-minimum" :: Cardano.Int
  , "quotientInteger-memory-arguments-slope" :: Cardano.Int
  , "remainderInteger-cpu-arguments-constant" :: Cardano.Int
  , "remainderInteger-cpu-arguments-model-arguments-intercept" :: Cardano.Int
  , "remainderInteger-cpu-arguments-model-arguments-slope" :: Cardano.Int
  , "remainderInteger-memory-arguments-intercept" :: Cardano.Int
  , "remainderInteger-memory-arguments-minimum" :: Cardano.Int
  , "remainderInteger-memory-arguments-slope" :: Cardano.Int
  , "sha2_256-cpu-arguments-intercept" :: Cardano.Int
  , "sha2_256-cpu-arguments-slope" :: Cardano.Int
  , "sha2_256-memory-arguments" :: Cardano.Int
  , "sha3_256-cpu-arguments-intercept" :: Cardano.Int
  , "sha3_256-cpu-arguments-slope" :: Cardano.Int
  , "sha3_256-memory-arguments" :: Cardano.Int
  , "sliceByteString-cpu-arguments-intercept" :: Cardano.Int
  , "sliceByteString-cpu-arguments-slope" :: Cardano.Int
  , "sliceByteString-memory-arguments-intercept" :: Cardano.Int
  , "sliceByteString-memory-arguments-slope" :: Cardano.Int
  , "sndPair-cpu-arguments" :: Cardano.Int
  , "sndPair-memory-arguments" :: Cardano.Int
  , "subtractInteger-cpu-arguments-intercept" :: Cardano.Int
  , "subtractInteger-cpu-arguments-slope" :: Cardano.Int
  , "subtractInteger-memory-arguments-intercept" :: Cardano.Int
  , "subtractInteger-memory-arguments-slope" :: Cardano.Int
  , "tailList-cpu-arguments" :: Cardano.Int
  , "tailList-memory-arguments" :: Cardano.Int
  , "trace-cpu-arguments" :: Cardano.Int
  , "trace-memory-arguments" :: Cardano.Int
  , "unBData-cpu-arguments" :: Cardano.Int
  , "unBData-memory-arguments" :: Cardano.Int
  , "unConstrData-cpu-arguments" :: Cardano.Int
  , "unConstrData-memory-arguments" :: Cardano.Int
  , "unIData-cpu-arguments" :: Cardano.Int
  , "unIData-memory-arguments" :: Cardano.Int
  , "unListData-cpu-arguments" :: Cardano.Int
  , "unListData-memory-arguments" :: Cardano.Int
  , "unMapData-cpu-arguments" :: Cardano.Int
  , "unMapData-memory-arguments" :: Cardano.Int
  , "verifyEd25519Signature-cpu-arguments-intercept" :: Cardano.Int
  , "verifyEd25519Signature-cpu-arguments-slope" :: Cardano.Int
  , "verifyEd25519Signature-memory-arguments" :: Cardano.Int
  )

type CostModelV2 =
  ( "serialiseData-cpu-arguments-intercept" :: Cardano.Int
  , "serialiseData-cpu-arguments-slope" :: Cardano.Int
  , "serialiseData-memory-arguments-intercept" :: Cardano.Int
  , "serialiseData-memory-arguments-slope" :: Cardano.Int
  , "verifyEcdsaSecp256k1Signature-cpu-arguments" :: Cardano.Int
  , "verifyEcdsaSecp256k1Signature-memory-arguments" :: Cardano.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept" :: Cardano.Int
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope" :: Cardano.Int
  , "verifySchnorrSecp256k1Signature-memory-arguments" :: Cardano.Int
  | CostModelV1
  )

costModelV3Names :: Array String
costModelV3Names =
  [ "addInteger-cpu-arguments-intercept"
  , "addInteger-cpu-arguments-slope"
  , "addInteger-memory-arguments-intercept"
  , "addInteger-memory-arguments-slope"
  , "appendByteString-cpu-arguments-intercept"
  , "appendByteString-cpu-arguments-slope"
  , "appendByteString-memory-arguments-intercept"
  , "appendByteString-memory-arguments-slope"
  , "appendString-cpu-arguments-intercept"
  , "appendString-cpu-arguments-slope"
  , "appendString-memory-arguments-intercept"
  , "appendString-memory-arguments-slope"
  , "bData-cpu-arguments"
  , "bData-memory-arguments"
  , "blake2b_256-cpu-arguments-intercept"
  , "blake2b_256-cpu-arguments-slope"
  , "blake2b_256-memory-arguments"
  , "cekApplyCost-exBudgetCPU"
  , "cekApplyCost-exBudgetMemory"
  , "cekBuiltinCost-exBudgetCPU"
  , "cekBuiltinCost-exBudgetMemory"
  , "cekConstCost-exBudgetCPU"
  , "cekConstCost-exBudgetMemory"
  , "cekDelayCost-exBudgetCPU"
  , "cekDelayCost-exBudgetMemory"
  , "cekForceCost-exBudgetCPU"
  , "cekForceCost-exBudgetMemory"
  , "cekLamCost-exBudgetCPU"
  , "cekLamCost-exBudgetMemory"
  , "cekStartupCost-exBudgetCPU"
  , "cekStartupCost-exBudgetMemory"
  , "cekVarCost-exBudgetCPU"
  , "cekVarCost-exBudgetMemory"
  , "chooseData-cpu-arguments"
  , "chooseData-memory-arguments"
  , "chooseList-cpu-arguments"
  , "chooseList-memory-arguments"
  , "chooseUnit-cpu-arguments"
  , "chooseUnit-memory-arguments"
  , "consByteString-cpu-arguments-intercept"
  , "consByteString-cpu-arguments-slope"
  , "consByteString-memory-arguments-intercept"
  , "consByteString-memory-arguments-slope"
  , "constrData-cpu-arguments"
  , "constrData-memory-arguments"
  , "decodeUtf8-cpu-arguments-intercept"
  , "decodeUtf8-cpu-arguments-slope"
  , "decodeUtf8-memory-arguments-intercept"
  , "decodeUtf8-memory-arguments-slope"
  , "divideInteger-cpu-arguments-constant"
  , "divideInteger-cpu-arguments-model-arguments-c00"
  , "divideInteger-cpu-arguments-model-arguments-c01"
  , "divideInteger-cpu-arguments-model-arguments-c02"
  , "divideInteger-cpu-arguments-model-arguments-c10"
  , "divideInteger-cpu-arguments-model-arguments-c11"
  , "divideInteger-cpu-arguments-model-arguments-c20"
  , "divideInteger-cpu-arguments-model-arguments-minimum"
  , "divideInteger-memory-arguments-intercept"
  , "divideInteger-memory-arguments-minimum"
  , "divideInteger-memory-arguments-slope"
  , "encodeUtf8-cpu-arguments-intercept"
  , "encodeUtf8-cpu-arguments-slope"
  , "encodeUtf8-memory-arguments-intercept"
  , "encodeUtf8-memory-arguments-slope"
  , "equalsByteString-cpu-arguments-constant"
  , "equalsByteString-cpu-arguments-intercept"
  , "equalsByteString-cpu-arguments-slope"
  , "equalsByteString-memory-arguments"
  , "equalsData-cpu-arguments-intercept"
  , "equalsData-cpu-arguments-slope"
  , "equalsData-memory-arguments"
  , "equalsInteger-cpu-arguments-intercept"
  , "equalsInteger-cpu-arguments-slope"
  , "equalsInteger-memory-arguments"
  , "equalsString-cpu-arguments-constant"
  , "equalsString-cpu-arguments-intercept"
  , "equalsString-cpu-arguments-slope"
  , "equalsString-memory-arguments"
  , "fstPair-cpu-arguments"
  , "fstPair-memory-arguments"
  , "headList-cpu-arguments"
  , "headList-memory-arguments"
  , "iData-cpu-arguments"
  , "iData-memory-arguments"
  , "ifThenElse-cpu-arguments"
  , "ifThenElse-memory-arguments"
  , "indexByteString-cpu-arguments"
  , "indexByteString-memory-arguments"
  , "lengthOfByteString-cpu-arguments"
  , "lengthOfByteString-memory-arguments"
  , "lessThanByteString-cpu-arguments-intercept"
  , "lessThanByteString-cpu-arguments-slope"
  , "lessThanByteString-memory-arguments"
  , "lessThanEqualsByteString-cpu-arguments-intercept"
  , "lessThanEqualsByteString-cpu-arguments-slope"
  , "lessThanEqualsByteString-memory-arguments"
  , "lessThanEqualsInteger-cpu-arguments-intercept"
  , "lessThanEqualsInteger-cpu-arguments-slope"
  , "lessThanEqualsInteger-memory-arguments"
  , "lessThanInteger-cpu-arguments-intercept"
  , "lessThanInteger-cpu-arguments-slope"
  , "lessThanInteger-memory-arguments"
  , "listData-cpu-arguments"
  , "listData-memory-arguments"
  , "mapData-cpu-arguments"
  , "mapData-memory-arguments"
  , "mkCons-cpu-arguments"
  , "mkCons-memory-arguments"
  , "mkNilData-cpu-arguments"
  , "mkNilData-memory-arguments"
  , "mkNilPairData-cpu-arguments"
  , "mkNilPairData-memory-arguments"
  , "mkPairData-cpu-arguments"
  , "mkPairData-memory-arguments"
  , "modInteger-cpu-arguments-constant"
  , "modInteger-cpu-arguments-model-arguments-c00"
  , "modInteger-cpu-arguments-model-arguments-c01"
  , "modInteger-cpu-arguments-model-arguments-c02"
  , "modInteger-cpu-arguments-model-arguments-c10"
  , "modInteger-cpu-arguments-model-arguments-c11"
  , "modInteger-cpu-arguments-model-arguments-c20"
  , "modInteger-cpu-arguments-model-arguments-minimum"
  , "modInteger-memory-arguments-intercept"
  , "modInteger-memory-arguments-slope"
  , "multiplyInteger-cpu-arguments-intercept"
  , "multiplyInteger-cpu-arguments-slope"
  , "multiplyInteger-memory-arguments-intercept"
  , "multiplyInteger-memory-arguments-slope"
  , "nullList-cpu-arguments"
  , "nullList-memory-arguments"
  , "quotientInteger-cpu-arguments-constant"
  , "quotientInteger-cpu-arguments-model-arguments-c00"
  , "quotientInteger-cpu-arguments-model-arguments-c01"
  , "quotientInteger-cpu-arguments-model-arguments-c02"
  , "quotientInteger-cpu-arguments-model-arguments-c10"
  , "quotientInteger-cpu-arguments-model-arguments-c11"
  , "quotientInteger-cpu-arguments-model-arguments-c20"
  , "quotientInteger-cpu-arguments-model-arguments-minimum"
  , "quotientInteger-memory-arguments-intercept"
  , "quotientInteger-memory-arguments-slope"
  , "remainderInteger-cpu-arguments-constant"
  , "remainderInteger-cpu-arguments-model-arguments-c00"
  , "remainderInteger-cpu-arguments-model-arguments-c01"
  , "remainderInteger-cpu-arguments-model-arguments-c02"
  , "remainderInteger-cpu-arguments-model-arguments-c10"
  , "remainderInteger-cpu-arguments-model-arguments-c11"
  , "remainderInteger-cpu-arguments-model-arguments-c20"
  , "remainderInteger-cpu-arguments-model-arguments-minimum"
  , "remainderInteger-memory-arguments-intercept"
  , "remainderInteger-memory-arguments-minimum"
  , "remainderInteger-memory-arguments-slope"
  , "serialiseData-cpu-arguments-intercept"
  , "serialiseData-cpu-arguments-slope"
  , "serialiseData-memory-arguments-intercept"
  , "serialiseData-memory-arguments-slope"
  , "sha2_256-cpu-arguments-intercept"
  , "sha2_256-cpu-arguments-slope"
  , "sha2_256-memory-arguments"
  , "sha3_256-cpu-arguments-intercept"
  , "sha3_256-cpu-arguments-slope"
  , "sha3_256-memory-arguments"
  , "sliceByteString-cpu-arguments-intercept"
  , "sliceByteString-cpu-arguments-slope"
  , "sliceByteString-memory-arguments-intercept"
  , "sliceByteString-memory-arguments-slope"
  , "sndPair-cpu-arguments"
  , "sndPair-memory-arguments"
  , "subtractInteger-cpu-arguments-intercept"
  , "subtractInteger-cpu-arguments-slope"
  , "subtractInteger-memory-arguments-intercept"
  , "subtractInteger-memory-arguments-slope"
  , "tailList-cpu-arguments"
  , "tailList-memory-arguments"
  , "trace-cpu-arguments"
  , "trace-memory-arguments"
  , "unBData-cpu-arguments"
  , "unBData-memory-arguments"
  , "unConstrData-cpu-arguments"
  , "unConstrData-memory-arguments"
  , "unIData-cpu-arguments"
  , "unIData-memory-arguments"
  , "unListData-cpu-arguments"
  , "unListData-memory-arguments"
  , "unMapData-cpu-arguments"
  , "unMapData-memory-arguments"
  , "verifyEcdsaSecp256k1Signature-cpu-arguments"
  , "verifyEcdsaSecp256k1Signature-memory-arguments"
  , "verifyEd25519Signature-cpu-arguments-intercept"
  , "verifyEd25519Signature-cpu-arguments-slope"
  , "verifyEd25519Signature-memory-arguments"
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept"
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope"
  , "verifySchnorrSecp256k1Signature-memory-arguments"
  , "cekConstrCost-exBudgetCPU"
  , "cekConstrCost-exBudgetMemory"
  , "cekCaseCost-exBudgetCPU"
  , "cekCaseCost-exBudgetMemory"
  , "bls12_381_G1_add-cpu-arguments"
  , "bls12_381_G1_add-memory-arguments"
  , "bls12_381_G1_compress-cpu-arguments"
  , "bls12_381_G1_compress-memory-arguments"
  , "bls12_381_G1_equal-cpu-arguments"
  , "bls12_381_G1_equal-memory-arguments"
  , "bls12_381_G1_hashToGroup-cpu-arguments-intercept"
  , "bls12_381_G1_hashToGroup-cpu-arguments-slope"
  , "bls12_381_G1_hashToGroup-memory-arguments"
  , "bls12_381_G1_neg-cpu-arguments"
  , "bls12_381_G1_neg-memory-arguments"
  , "bls12_381_G1_scalarMul-cpu-arguments-intercept"
  , "bls12_381_G1_scalarMul-cpu-arguments-slope"
  , "bls12_381_G1_scalarMul-memory-arguments"
  , "bls12_381_G1_uncompress-cpu-arguments"
  , "bls12_381_G1_uncompress-memory-arguments"
  , "bls12_381_G2_add-cpu-arguments"
  , "bls12_381_G2_add-memory-arguments"
  , "bls12_381_G2_compress-cpu-arguments"
  , "bls12_381_G2_compress-memory-arguments"
  , "bls12_381_G2_equal-cpu-arguments"
  , "bls12_381_G2_equal-memory-arguments"
  , "bls12_381_G2_hashToGroup-cpu-arguments-intercept"
  , "bls12_381_G2_hashToGroup-cpu-arguments-slope"
  , "bls12_381_G2_hashToGroup-memory-arguments"
  , "bls12_381_G2_neg-cpu-arguments"
  , "bls12_381_G2_neg-memory-arguments"
  , "bls12_381_G2_scalarMul-cpu-arguments-intercept"
  , "bls12_381_G2_scalarMul-cpu-arguments-slope"
  , "bls12_381_G2_scalarMul-memory-arguments"
  , "bls12_381_G2_uncompress-cpu-arguments"
  , "bls12_381_G2_uncompress-memory-arguments"
  , "bls12_381_finalVerify-cpu-arguments"
  , "bls12_381_finalVerify-memory-arguments"
  , "bls12_381_millerLoop-cpu-arguments"
  , "bls12_381_millerLoop-memory-arguments"
  , "bls12_381_mulMlResult-cpu-arguments"
  , "bls12_381_mulMlResult-memory-arguments"
  , "keccak_256-cpu-arguments-intercept"
  , "keccak_256-cpu-arguments-slope"
  , "keccak_256-memory-arguments"
  , "blake2b_224-cpu-arguments-intercept"
  , "blake2b_224-cpu-arguments-slope"
  , "blake2b_224-memory-arguments"
  , "integerToByteString-cpu-arguments-c0"
  , "integerToByteString-cpu-arguments-c1"
  , "integerToByteString-cpu-arguments-c2"
  , "integerToByteString-memory-arguments-intercept"
  , "integerToByteString-memory-arguments-slope"
  , "byteStringToInteger-cpu-arguments-c0"
  , "byteStringToInteger-cpu-arguments-c1"
  , "byteStringToInteger-cpu-arguments-c2"
  , "byteStringToInteger-memory-arguments-intercept"
  , "byteStringToInteger-memory-arguments-slope"
  , "andByteString-cpu-arguments-intercept"
  , "andByteString-cpu-arguments-slope1"
  , "andByteString-cpu-arguments-slope2"
  , "andByteString-memory-arguments-intercept"
  , "andByteString-memory-arguments-slope"
  , "orByteString-cpu-arguments-intercept"
  , "orByteString-cpu-arguments-slope1"
  , "orByteString-cpu-arguments-slope2"
  , "orByteString-memory-arguments-intercept"
  , "orByteString-memory-arguments-slope"
  , "xorByteString-cpu-arguments-intercept"
  , "xorByteString-cpu-arguments-slope1"
  , "xorByteString-cpu-arguments-slope2"
  , "xorByteString-memory-arguments-intercept"
  , "xorByteString-memory-arguments-slope"
  , "complementByteString-cpu-arguments-intercept"
  , "complementByteString-cpu-arguments-slope"
  , "complementByteString-memory-arguments-intercept"
  , "complementByteString-memory-arguments-slope"
  , "readBit-cpu-arguments"
  , "readBit-memory-arguments"
  , "writeBits-cpu-arguments-intercept"
  , "writeBits-cpu-arguments-slope"
  , "writeBits-memory-arguments-intercept"
  , "writeBits-memory-arguments-slope"
  , "replicateByte-cpu-arguments-intercept"
  , "replicateByte-cpu-arguments-slope"
  , "replicateByte-memory-arguments-intercept"
  , "replicateByte-memory-arguments-slope"
  , "shiftByteString-cpu-arguments-intercept"
  , "shiftByteString-cpu-arguments-slope"
  , "shiftByteString-memory-arguments-intercept"
  , "shiftByteString-memory-arguments-slope"
  , "rotateByteString-cpu-arguments-intercept"
  , "rotateByteString-cpu-arguments-slope"
  , "rotateByteString-memory-arguments-intercept"
  , "rotateByteString-memory-arguments-slope"
  , "countSetBits-cpu-arguments-intercept"
  , "countSetBits-cpu-arguments-slope"
  , "countSetBits-memory-arguments"
  , "findFirstSetBit-cpu-arguments-intercept"
  , "findFirstSetBit-cpu-arguments-slope"
  , "findFirstSetBit-memory-arguments"
  , "ripemd_160-cpu-arguments-intercept"
  , "ripemd_160-cpu-arguments-slope"
  , "ripemd_160-memory-arguments"
  ]

-- This assumes that cost models are stored in lexicographical order
convertCostModel
  :: forall costModel
   . HFoldl (List Cardano.Int -> Cardano.Int -> List Cardano.Int)
       (List Cardano.Int)
       costModel
       (List Cardano.Int)
  => costModel
  -> CostModel
convertCostModel model = wrap $ Array.reverse $ List.toUnfoldable $ hfoldl
  ( (\xs x -> x List.: xs)
      :: List Cardano.Int -> Cardano.Int -> List Cardano.Int
  )
  (mempty :: List Cardano.Int)
  model

-- Specialized conversions to only perform the type level traversals once

convertPlutusV1CostModel :: Record CostModelV1 -> CostModel
convertPlutusV1CostModel = convertCostModel

convertPlutusV2CostModel :: Record CostModelV2 -> CostModel
convertPlutusV2CostModel = convertCostModel

convertPlutusV3CostModel :: Object Cardano.Int -> Maybe CostModel
convertPlutusV3CostModel costModelRaw =
  wrap <$> traverse
    (flip Object.lookup costModelRaw)
    costModelV3Names

convertUnnamedPlutusCostModel :: Object Cardano.Int -> Maybe CostModel
convertUnnamedPlutusCostModel costModelRaw =
  wrap <<< map snd <<< Array.sortWith fst <$>
    traverse (ltraverse Cardano.Int.fromString)
      (Object.toUnfoldable costModelRaw)

--------------------------------------------------------------------------------
-- BlockfrostProtocolParameters
--------------------------------------------------------------------------------

-- | `Stringed a` decodes an `a` that was encoded as a `String`
newtype Stringed a = Stringed a

derive instance Newtype (Stringed a) _

instance DecodeAeson a => DecodeAeson (Stringed a) where
  decodeAeson = decodeAeson >=> decodeJsonString >=> Stringed >>> pure

newtype FiniteBigNumber = FiniteBigNumber BigNumber

derive instance Newtype FiniteBigNumber _

instance DecodeAeson FiniteBigNumber where
  decodeAeson aeson = do
    number <- decodeAeson aeson
    map FiniteBigNumber $ note (TypeMismatch "BigNumber") $ hush
      $ BigNumber.parseBigNumber
      $ show (number :: Number)

type BlockfrostProtocolParametersRaw =
  { "min_fee_a" :: UInt
  , "min_fee_b" :: UInt
  , "max_block_size" :: UInt
  , "max_tx_size" :: UInt
  , "max_block_header_size" :: UInt
  , "key_deposit" :: Stringed BigNum
  , "pool_deposit" :: Stringed BigNum
  , "e_max" :: UInt
  , "n_opt" :: UInt
  , "a0" :: FiniteBigNumber
  , "rho" :: FiniteBigNumber
  , "tau" :: FiniteBigNumber
  , "protocol_major_ver" :: UInt
  , "protocol_minor_ver" :: UInt
  , "min_pool_cost" :: Stringed BigNum
  , "cost_models" ::
      { "PlutusV1" :: { | CostModelV1 }
      , "PlutusV2" :: { | CostModelV2 }
      , "PlutusV3" :: Object Cardano.Int
      }
  , "price_mem" :: FiniteBigNumber
  , "price_step" :: FiniteBigNumber
  , "max_tx_ex_mem" :: Stringed BigNum
  , "max_tx_ex_steps" :: Stringed BigNum
  , "max_block_ex_mem" :: Stringed BigNum
  , "max_block_ex_steps" :: Stringed BigNum
  , "max_val_size" :: Stringed UInt
  , "collateral_percent" :: UInt
  , "max_collateral_inputs" :: UInt
  , "coins_per_utxo_size" :: Maybe (Stringed BigNum)
  , "gov_action_deposit" :: Stringed BigNum
  , "drep_deposit" :: Stringed BigNum
  , "min_fee_ref_script_cost_per_byte" :: UInt
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
    memPrice <- bigNumberToPrice raw.price_mem
    stepPrice <- bigNumberToPrice raw.price_step
    let prices = ExUnitPrices { memPrice, stepPrice }

    coinsPerUtxoByte <-
      maybe (Left $ AtKey "coins_per_utxo_size" $ MissingValue)
        pure $ (Coin <<< unwrap <$> raw.coins_per_utxo_size)

    refScriptCoinsPerByte <-
      note (AtKey "min_fee_ref_script_cost_per_byte" $ TypeMismatch "Integer") $
        Rational.reduce
          ( BigNum.toBigInt $ BigNum.fromUInt
              raw.min_fee_ref_script_cost_per_byte
          )
          one
    let plutusV3CostModelRaw = raw.cost_models."PlutusV3"
    plutusV3CostModel <-
      note (AtKey "cost_models" $ AtKey "PlutusV3" $ TypeMismatch "CostModel")
        ( convertPlutusV3CostModel plutusV3CostModelRaw
            <|> convertUnnamedPlutusCostModel plutusV3CostModelRaw
        )
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
          [ PlutusV1 /\ convertPlutusV1CostModel raw.cost_models."PlutusV1"
          , PlutusV2 /\ convertPlutusV2CostModel raw.cost_models."PlutusV2"
          , PlutusV3 /\ plutusV3CostModel
          ]
      , prices
      , maxTxExUnits:
          ExUnits
            { mem: unwrap raw.max_tx_ex_mem
            , steps: unwrap raw.max_tx_ex_steps
            }
      , maxBlockExUnits:
          ExUnits
            { mem: unwrap raw.max_block_ex_mem
            , steps: unwrap raw.max_block_ex_steps
            }
      , maxValueSize: unwrap raw.max_val_size
      , collateralPercent: raw.collateral_percent
      , maxCollateralInputs: raw.max_collateral_inputs
      , govActionDeposit: Coin $ unwrap raw.gov_action_deposit
      , drepDeposit: Coin $ unwrap raw.drep_deposit
      , refScriptCoinsPerByte
      }