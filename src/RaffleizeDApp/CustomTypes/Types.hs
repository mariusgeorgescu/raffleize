module RaffleizeDApp.CustomTypes.Types where

import PlutusLedgerApi.V2
import PlutusTx

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
