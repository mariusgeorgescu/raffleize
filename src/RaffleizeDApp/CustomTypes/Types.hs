module RaffleizeDApp.CustomTypes.Types where

import Data.Aeson hiding (Value)
import Data.Aeson qualified (Value)
import Data.Aeson.Types (Parser)
import Data.String

import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue, flattenValue, toString)
import PlutusLedgerApi.V2
import PlutusTx
import PlutusTx.Show qualified as PlutusTx (show)

unFlattenValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
unFlattenValue [] = mempty
unFlattenValue ((cs, tn, i) : vls) = assetClassValue (AssetClass (cs, tn)) i <> unFlattenValue vls

-------------------------------------------------------------------------------

-- * Custom Types  Declarations

-------------------------------------------------------------------------------

type Metadata = Map BuiltinByteString BuiltinByteString

data ATxInfo = ATxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: [TxInInfo]
  , txInfoOutputs :: [TxOut]
  , txInfoFee :: BuiltinData
  , txInfoMint :: Value
  , txInfoDCert :: BuiltinData
  , txInfoWdrl :: BuiltinData
  , txInfoValidRange :: POSIXTimeRange
  , txInfoSignatories :: BuiltinData
  , txInfoData :: BuiltinData
  , txInfoId :: BuiltinData
  }

unstableMakeIsData ''ATxInfo

data AScriptContext = AScriptContext {scriptContextTxInfo :: ATxInfo, scriptContextPurpose :: ScriptPurpose}
unstableMakeIsData ''AScriptContext

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

instance ToJSON TokenName where
  toJSON :: TokenName -> Data.Aeson.Value
  toJSON tn = toJSON $ toString tn

instance FromJSON TokenName where
  parseJSON :: Data.Aeson.Value -> Parser TokenName
  parseJSON v = fromString @TokenName <$> parseJSON @String v

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
  toJSON bs = toJSON $ fromBuiltin @BuiltinString @Text $ PlutusTx.show bs

instance FromJSON BuiltinByteString where
  parseJSON :: Data.Aeson.Value -> Parser BuiltinByteString
  parseJSON v = fromString @BuiltinByteString <$> parseJSON @String v

instance ToJSON ScriptHash where
  toJSON :: ScriptHash -> Data.Aeson.Value
  toJSON (ScriptHash s) = toJSON s

instance FromJSON ScriptHash where
  parseJSON :: Data.Aeson.Value -> Parser ScriptHash
  parseJSON v =
    let s = parseJSON @BuiltinByteString v
     in ScriptHash <$> s

