{-# OPTIONS_GHC -Wno-orphans #-}

module RaffleizeDApp.CustomTypes.Types where

import Data.Aeson hiding (Value)
import Data.Aeson qualified (Value)
import Data.Aeson.Types (Parser)
import Data.String
import Data.Text qualified as Text
import GeniusYield.Types (unsafeTokenNameFromHex)
import GeniusYield.Types.Value (tokenNameToPlutus)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue, flattenValue, toString)
import PlutusLedgerApi.V3
import PlutusTx (unstableMakeIsData)

-------------------------------------------------------------------------------

-- * Custom Types  Declarations

-------------------------------------------------------------------------------

type Metadata = Map BuiltinByteString BuiltinByteString

------------------------

-- * Custom Types For ScriptContext

------------------------

data ATxInfo = ATxInfo
  { txInfoInputs :: [TxInInfo],
    txInfoReferenceInputs :: [TxInInfo],
    txInfoOutputs :: [TxOut],
    txInfoFee :: BuiltinData,
    txInfoMint :: Value,
    txInfoTxCerts :: BuiltinData,
    txInfoWdrl :: BuiltinData,
    txInfoValidRange :: POSIXTimeRange,
    txInfoSignatories :: BuiltinData,
    txInfoRedeemers :: BuiltinData,
    txInfoData :: BuiltinData,
    txInfoId :: BuiltinData,
    txInfoVotes :: BuiltinData,
    txInfoProposalProcedures :: BuiltinData,
    txInfoCurrentTreasuryAmount :: BuiltinData,
    txInfoTreasuryDonation :: BuiltinData
  }

unstableMakeIsData ''ATxInfo

data AScriptContext = AScriptContext
  { scriptContextTxInfo :: ATxInfo,
    scriptContextRedeemer :: Redeemer,
    scriptContextScriptInfo :: ScriptInfo
  }

unstableMakeIsData ''AScriptContext

---
---
-- ORPHAN INSTANCES
---
---

instance ToJSON POSIXTime where
  toJSON :: POSIXTime -> Data.Aeson.Value
  toJSON (POSIXTime i) = toJSON i

instance FromJSON POSIXTime where
  parseJSON :: Data.Aeson.Value -> Parser POSIXTime
  parseJSON v = POSIXTime <$> parseJSON @Integer v

instance ToJSON Value where
  toJSON :: Value -> Data.Aeson.Value
  toJSON value = toJSON $ flattenValue value

instance FromJSON Value where
  parseJSON :: Data.Aeson.Value -> Parser Value
  parseJSON v =
    let flattenedValue = parseJSON @[(CurrencySymbol, TokenName, Integer)] v
     in unFlattenValue <$> flattenedValue
    where
      unFlattenValue :: [(PlutusLedgerApi.V3.CurrencySymbol, PlutusLedgerApi.V3.TokenName, Integer)] -> PlutusLedgerApi.V3.Value
      unFlattenValue [] = mempty
      unFlattenValue ((cs, tn, i) : vls) = assetClassValue (AssetClass (cs, tn)) i <> unFlattenValue vls

instance ToJSON TokenName where
  toJSON :: TokenName -> Data.Aeson.Value
  toJSON tn =
    let tnStr = toString tn
     in if take 2 tnStr == "0x" then toJSON (drop 2 tnStr) else toJSON tnStr

instance FromJSON TokenName where
  parseJSON :: Data.Aeson.Value -> Parser TokenName
  parseJSON v = tokenNameToPlutus . unsafeTokenNameFromHex <$> parseJSON @Text v

instance ToJSON CurrencySymbol where
  toJSON :: CurrencySymbol -> Data.Aeson.Value
  toJSON cs = toJSON $ show cs

instance FromJSON CurrencySymbol where
  parseJSON :: Data.Aeson.Value -> Parser CurrencySymbol
  parseJSON v = fromString @CurrencySymbol <$> parseJSON @String v

instance ToJSON AssetClass where
  toJSON :: AssetClass -> Data.Aeson.Value
  toJSON (AssetClass ac) = toJSON ac

instance FromJSON AssetClass where
  parseJSON :: Data.Aeson.Value -> Parser AssetClass
  parseJSON v =
    let ac = parseJSON @(CurrencySymbol, TokenName) v
     in AssetClass <$> ac

instance ToJSON BuiltinByteString where
  toJSON :: BuiltinByteString -> Data.Aeson.Value
  toJSON bs = toJSON $ Text.pack $ init $ tail $ show bs

instance FromJSON BuiltinByteString where
  parseJSON :: Data.Aeson.Value -> Parser BuiltinByteString
  parseJSON v = fromString @BuiltinByteString . Text.unpack <$> parseJSON @Text v

instance ToJSON ScriptHash where
  toJSON :: ScriptHash -> Data.Aeson.Value
  toJSON (ScriptHash s) = toJSON $ toString (TokenName s) --- using TokenName for hex conversion

instance FromJSON ScriptHash where
  parseJSON :: Data.Aeson.Value -> Parser ScriptHash
  parseJSON v =
    let tn = tokenNameToPlutus . unsafeTokenNameFromHex <$> parseJSON @Text v --- using TokenName for hex conversion
     in ScriptHash . unTokenName <$> tn
