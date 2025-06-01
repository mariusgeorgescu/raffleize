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
import PlutusTx.Builtins (serialiseData)
import RaffleizeDApp.CustomTypes.Types

wrapTitle :: String -> String
wrapTitle s =
  "\n-------------------------\n"
    ++ s
    ++ "\n-------------------------\n"

encodeUtf8KV :: (BuiltinString, BuiltinString) -> (BuiltinByteString, BuiltinByteString)
encodeUtf8KV (k, v) = (encodeUtf8 k, encodeUtf8 v)

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
unsafeGetOwnInput :: AScriptContext -> PlutusLedgerApi.V3.TxOut
unsafeGetOwnInput context = case findOwnInputA context of
  Nothing -> traceError "Own input not found"
  Just (PlutusLedgerApi.V3.TxInInfo _inOutRef inOut) -> inOut
{-# INLINEABLE unsafeGetOwnInput #-}

-- | Find the input currently being validated.
findOwnInputA :: AScriptContext -> Maybe PlutusLedgerApi.V3.TxInInfo
findOwnInputA AScriptContext {scriptContextTxInfo = ATxInfo {txInfoInputs}, scriptContextScriptInfo = PlutusLedgerApi.V3.SpendingScript txOutRef _} =
  find (\PlutusLedgerApi.V3.TxInInfo {txInInfoOutRef} -> txInInfoOutRef #== txOutRef) txInfoInputs
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

type ValueConstraint = PlutusLedgerApi.V3.Value -> Bool

type AddressConstraint = PlutusLedgerApi.V3.Address -> Bool

noConstraint :: b -> Bool
noConstraint = const True
{-# INLINEABLE noConstraint #-}

------------------------

-- ** TxOut Helper Functions

------------------------

isTxOutWith :: ValueConstraint -> AddressConstraint -> PlutusLedgerApi.V3.TxOut -> Bool
isTxOutWith !toValue !toAddress PlutusLedgerApi.V3.TxOut {txOutValue, txOutAddress} = toValue txOutValue && toAddress txOutAddress
{-# INLINEABLE isTxOutWith #-}

isTxOutWithInlineDatumAnd :: (PlutusLedgerApi.V3.ToData a) => a -> ValueConstraint -> AddressConstraint -> PlutusLedgerApi.V3.TxOut -> Bool
isTxOutWithInlineDatumAnd datum !toValue !toAddress PlutusLedgerApi.V3.TxOut {txOutValue, txOutAddress, txOutDatum} = toValue txOutValue && toAddress txOutAddress && isGivenInlineDatum datum txOutDatum
{-# INLINEABLE isTxOutWithInlineDatumAnd #-}

unsafeGetInlineDatum :: PlutusLedgerApi.V3.TxOut -> BuiltinData
unsafeGetInlineDatum out = case txOutDatum out of
  PlutusLedgerApi.V3.OutputDatum da -> getDatum da
  _ -> traceError "No inline datum"
{-# INLINEABLE unsafeGetInlineDatum #-}

------------------------

-- ** TxOuts Helper Functions

------------------------
outHas1of :: PlutusLedgerApi.V3.TxOut -> AssetClass -> Bool
outHas1of (PlutusLedgerApi.V3.TxOut _ value _ _) ac = assetClassValueOf value ac #== 1
{-# INLINEABLE outHas1of #-}

hasTxOutWith :: ValueConstraint -> AddressConstraint -> [PlutusLedgerApi.V3.TxOut] -> Bool
hasTxOutWith !toValue !toAddress = pany (isTxOutWith toValue toAddress)
{-# INLINEABLE hasTxOutWith #-}

hasTxOutWithInlineDatumAnd :: (PlutusLedgerApi.V3.ToData a) => a -> ValueConstraint -> AddressConstraint -> [PlutusLedgerApi.V3.TxOut] -> Bool
hasTxOutWithInlineDatumAnd !datum !toValue !toAddress =
  traceIfFalse "not found tx out with datum" . pany (isTxOutWithInlineDatumAnd datum toValue toAddress)
{-# INLINEABLE hasTxOutWithInlineDatumAnd #-}

------------------------

-- **  TxIn Helper Functions

------------------------

-- | Helper function to check if a 'PlutusLedgerApi.V3.PlutusLedgerApi.V3.TxInInfo' contains exactly 1 quantity of an AssetClass
inputHas1of :: PlutusLedgerApi.V3.TxInInfo -> AssetClass -> Bool
inputHas1of = outHas1of . txInInfoResolved
{-# INLINEABLE inputHas1of #-}

------------------------

-- ** TxIns Helper Functions

------------------------
hasTxInWithToken :: AssetClass -> [PlutusLedgerApi.V3.TxInInfo] -> Bool
hasTxInWithToken tokenId = hasTxInWith ((#== 1) . (`assetClassValueOf` tokenId)) noConstraint
{-# INLINEABLE hasTxInWithToken #-}

-- | Helper function to check that a UTxO is being spent in the transaction.
hasTxInWithRef :: PlutusLedgerApi.V3.TxOutRef -> [PlutusLedgerApi.V3.TxInInfo] -> Bool
hasTxInWithRef oref = pany (\(PlutusLedgerApi.V3.TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasTxInWithRef #-}

hasTxInWith :: ValueConstraint -> AddressConstraint -> [PlutusLedgerApi.V3.TxInInfo] -> Bool
hasTxInWith !toValue !toAddress = hasTxOutWith toValue toAddress . (txInInfoResolved #<$>)
{-# INLINEABLE hasTxInWith #-}

unsafeGetCurrentStateDatumAndValue :: AssetClass -> AddressConstraint -> [PlutusLedgerApi.V3.TxInInfo] -> (PlutusLedgerApi.V3.Value, BuiltinData)
unsafeGetCurrentStateDatumAndValue stateToken !toAddress outs = case filter (isTxOutWith (`geq` assetClassValue stateToken 1) toAddress . txInInfoResolved) outs of
  [PlutusLedgerApi.V3.TxInInfo _ out] -> (txOutValue out, unsafeGetInlineDatum out)
  _ -> traceError "state nft not found"
{-# INLINEABLE unsafeGetCurrentStateDatumAndValue #-}

------------------------

-- ** Value Helper Functions

------------------------

isMintingNFT :: AssetClass -> PlutusLedgerApi.V3.Value -> Bool
isMintingNFT ac txInfoMint = traceIfFalse "NFT not minted" $ assetClassValueOf txInfoMint ac #== 1
{-# INLINEABLE isMintingNFT #-}

isBurningNFT :: AssetClass -> PlutusLedgerApi.V3.Value -> Bool
isBurningNFT ac txInfoMint = traceIfFalse "NFT not burned" $ assetClassValueOf txInfoMint ac #== pnegate 1
{-# INLINEABLE isBurningNFT #-}

unFlattenValue :: [(PlutusLedgerApi.V3.CurrencySymbol, PlutusLedgerApi.V3.TokenName, Integer)] -> PlutusLedgerApi.V3.Value
unFlattenValue [] = mempty
unFlattenValue ((cs, tn, i) : vls) = assetClassValue (AssetClass (cs, tn)) i <> unFlattenValue vls

adaValueFromLovelaces :: Integer -> PlutusLedgerApi.V3.Value
adaValueFromLovelaces = PlutusLedgerApi.V3.singleton PlutusLedgerApi.V3.adaSymbol PlutusLedgerApi.V3.adaToken
{-# INLINEABLE adaValueFromLovelaces #-}

showValue :: String -> PlutusLedgerApi.V3.Value -> String
showValue s v = wrapTitle s ++ intercalate "\n" (show <$> PlutusLedgerApi.V1.Value.flattenValue v)

------------------------

-- ** Conversions Helper Functions

------------------------

-- bsToInteger' :: BuiltinByteString -> Integer
-- bsToInteger' =  byteStringToInteger BigEndian
-- {-# INLINEABLE bsToInteger' #-}

integerToBs24 :: Integer -> BuiltinByteString --- cheaper than integerToByteString
integerToBs24 = dropByteString 1 . serialiseData . PlutusLedgerApi.V3.toBuiltinData -- Removing First Byte  (works for value > 24)
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

isGivenInlineDatum :: (PlutusLedgerApi.V3.ToData a) => a -> PlutusLedgerApi.V3.OutputDatum -> Bool
isGivenInlineDatum datum outdat = case outdat of
  PlutusLedgerApi.V3.OutputDatum da -> PlutusLedgerApi.V3.toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE isGivenInlineDatum #-}
