module RaffleizeDApp.OnChain.Utils where

import Data.List.Extra (intercalate)
import PlutusLedgerApi.V1.Value
  ( AssetClass (..),
    assetClassValue,
    assetClassValueOf,
    flattenValue,
    geq,
  )
import PlutusLedgerApi.V3
  ( Address,
    CurrencySymbol,
    Datum (getDatum),
    OutputDatum (OutputDatum),
    POSIXTimeRange,
    Redeemer,
    ScriptInfo (SpendingScript),
    ToData (..),
    TokenName (..),
    TxInInfo (..),
    TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
    TxOutRef,
    Value,
    adaSymbol,
    adaToken,
    singleton,
  )
import PlutusTx (fromBuiltinData, unstableMakeIsData)
import PlutusTx.Builtins (serialiseData)

wrapTitle :: String -> String
wrapTitle s =
  "\n-------------------------\n"
    ++ s
    ++ "\n-------------------------\n"

encodeUtf8KV :: (BuiltinString, BuiltinString) -> (BuiltinByteString, BuiltinByteString)
encodeUtf8KV (k, v) = (encodeUtf8 k, encodeUtf8 v)

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

------------------------

-- * Custom  ScriptContext Helper Functions

------------------------

-- | Helper function: check that the validating input contains a given token
ownInputHasToken :: AssetClass -> AScriptContext -> Bool
ownInputHasToken proofToken sc =
  "The transaction must spend the state token"
    `traceIfFalse` case (`inputHas1of` proofToken) #<$> findOwnInputA sc of
      Nothing -> trace "Own input not found" False
      Just result -> traceIfFalse "Proof Token Not Spent" result
{-# INLINEABLE ownInputHasToken #-}

-- | Function to get the spending input
--  Fails if is not a SpendingScript
unsafeGetOwnInput :: AScriptContext -> TxOut
unsafeGetOwnInput context = case findOwnInputA context of
  Nothing -> traceError "Own input not found"
  Just (TxInInfo _inOutRef inOut) -> inOut
{-# INLINEABLE unsafeGetOwnInput #-}

-- | Find the input currently being validated.
findOwnInputA :: AScriptContext -> Maybe TxInInfo
findOwnInputA AScriptContext {scriptContextTxInfo = ATxInfo {txInfoInputs}, scriptContextScriptInfo = SpendingScript txOutRef _} =
  find (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef #== txOutRef) txInfoInputs
findOwnInputA _ = Nothing
{-# INLINEABLE findOwnInputA #-}

-- | Converst a typed lambda to untyped
mkUntypedLambda ::
  (AScriptContext -> Bool) ->
  (BuiltinData -> BuiltinUnit)
mkUntypedLambda f c = check $ f (parseData c "Invalid context")
  where
    parseData mdata message = case fromBuiltinData mdata of
      Just d -> d
      _ -> traceError message
{-# INLINEABLE mkUntypedLambda #-}

------------------------

-- * Helper Functions

------------------------

type ValueConstraint = Value -> Bool

type AddressConstraint = Address -> Bool

noConstraint :: b -> Bool
noConstraint = const True
{-# INLINEABLE noConstraint #-}

------------------------

-- ** TxOut Helper Functions

------------------------

isTxOutWith :: ValueConstraint -> AddressConstraint -> TxOut -> Bool
isTxOutWith !toValue !toAddress TxOut {txOutValue, txOutAddress} = toValue txOutValue && toAddress txOutAddress
{-# INLINEABLE isTxOutWith #-}

isTxOutWithInlineDatumAnd :: (ToData a) => a -> ValueConstraint -> AddressConstraint -> TxOut -> Bool
isTxOutWithInlineDatumAnd datum !toValue !toAddress TxOut {txOutValue, txOutAddress, txOutDatum} = toValue txOutValue && toAddress txOutAddress && isGivenInlineDatum datum txOutDatum
{-# INLINEABLE isTxOutWithInlineDatumAnd #-}

unsafeGetInlineDatum :: TxOut -> BuiltinData
unsafeGetInlineDatum out = case txOutDatum out of
  OutputDatum da -> getDatum da
  _ -> traceError "No inline datum"
{-# INLINEABLE unsafeGetInlineDatum #-}

------------------------

-- ** TxOuts Helper Functions

------------------------
outHas1of :: TxOut -> AssetClass -> Bool
outHas1of (TxOut _ value _ _) ac = assetClassValueOf value ac #== 1
{-# INLINEABLE outHas1of #-}

hasTxOutWith :: ValueConstraint -> AddressConstraint -> [TxOut] -> Bool
hasTxOutWith !toValue !toAddress = pany (isTxOutWith toValue toAddress)
{-# INLINEABLE hasTxOutWith #-}

hasTxOutWithInlineDatumAnd :: (ToData a) => a -> ValueConstraint -> AddressConstraint -> [TxOut] -> Bool
hasTxOutWithInlineDatumAnd !datum !toValue !toAddress =
  traceIfFalse "not found tx out with datum" . pany (isTxOutWithInlineDatumAnd datum toValue toAddress)
{-# INLINEABLE hasTxOutWithInlineDatumAnd #-}

------------------------

-- **  TxIn Helper Functions

------------------------

-- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
inputHas1of :: TxInInfo -> AssetClass -> Bool
inputHas1of = outHas1of . txInInfoResolved
{-# INLINEABLE inputHas1of #-}

------------------------

-- ** TxIns Helper Functions

------------------------
hasTxInWithToken :: AssetClass -> [TxInInfo] -> Bool
hasTxInWithToken tokenId = hasTxInWith ((#== 1) . (`assetClassValueOf` tokenId)) noConstraint
{-# INLINEABLE hasTxInWithToken #-}

-- | Helper function to check that a UTxO is being spent in the transaction.
hasTxInWithRef :: TxOutRef -> [TxInInfo] -> Bool
hasTxInWithRef oref = pany (\(TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasTxInWithRef #-}

hasTxInWith :: ValueConstraint -> AddressConstraint -> [TxInInfo] -> Bool
hasTxInWith !toValue !toAddress = hasTxOutWith toValue toAddress . (txInInfoResolved #<$>)
{-# INLINEABLE hasTxInWith #-}

unsafeGetCurrentStateDatumAndValue :: AssetClass -> AddressConstraint -> [TxInInfo] -> (Value, BuiltinData)
unsafeGetCurrentStateDatumAndValue stateToken !toAddress outs = case filter (isTxOutWith (`geq` assetClassValue stateToken 1) toAddress . txInInfoResolved) outs of
  [TxInInfo _ out] -> (txOutValue out, unsafeGetInlineDatum out)
  _ -> traceError "state nft not found"
{-# INLINEABLE unsafeGetCurrentStateDatumAndValue #-}

------------------------

-- ** Value Helper Functions

------------------------

isMintingNFT :: AssetClass -> Value -> Bool
isMintingNFT ac txInfoMint = traceIfFalse "NFT not minted" $ assetClassValueOf txInfoMint ac #== 1
{-# INLINEABLE isMintingNFT #-}

isBurningNFT :: AssetClass -> Value -> Bool
isBurningNFT ac txInfoMint = traceIfFalse "NFT not burned" $ assetClassValueOf txInfoMint ac #== pnegate 1
{-# INLINEABLE isBurningNFT #-}

unFlattenValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
unFlattenValue [] = mempty
unFlattenValue ((cs, tn, i) : vls) = assetClassValue (AssetClass (cs, tn)) i <> unFlattenValue vls

adaValueFromLovelaces :: Integer -> Value
adaValueFromLovelaces = singleton adaSymbol adaToken
{-# INLINEABLE adaValueFromLovelaces #-}

showValue :: String -> Value -> String
showValue s v = wrapTitle s ++ intercalate "\n" (show <$> PlutusLedgerApi.V1.Value.flattenValue v)

------------------------

-- ** Conversions Helper Functions

------------------------

-- bsToInteger' :: BuiltinByteString -> Integer
-- bsToInteger' =  byteStringToInteger BigEndian
-- {-# INLINEABLE bsToInteger' #-}

integerToBs24 :: Integer -> BuiltinByteString --- cheaper than integerToByteString
integerToBs24 = dropByteString 1 . serialiseData . toBuiltinData -- Removing First Byte  (works for value > 24)
{-# INLINEABLE integerToBs24 #-}

-- Convert a BuiltinByteString to an Integer, optimizing for memory usage
bsToInteger :: BuiltinByteString -> Integer
bsToInteger bs =
  if bs #== emptyByteString
    then 0
    else
      let !isNegative = indexByteString bs 0 #== 45 -- ASCII code for '-'
          bsTrimmed = if isNegative then dropByteString 1 bs else bs
          !result = bsToIntegerDirect bsTrimmed 0 0
       in if isNegative then pnegate result else result
{-# INLINEABLE bsToInteger #-}

-- Helper function to directly convert BuiltinByteString to Integer
bsToIntegerDirect :: BuiltinByteString -> Integer -> Integer -> Integer
bsToIntegerDirect bs !acc !index =
  if index #== lengthOfByteString bs
    then acc
    else
      let !digit = indexByteString bs index
          !newAcc = acc #* 256 #+ digit
       in bsToIntegerDirect bs newAcc (index #+ 1)
{-# INLINEABLE bsToIntegerDirect #-}

------------------------

-- ** Datum Helper Functions

------------------------

isGivenInlineDatum :: (ToData a) => a -> OutputDatum -> Bool
isGivenInlineDatum datum outdat = case outdat of
  OutputDatum da -> toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE isGivenInlineDatum #-}
