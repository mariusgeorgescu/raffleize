module RaffleizeDApp.OnChain.Utils where

import Data.List.Extra (intercalate)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClass,
  assetClassValue,
  assetClassValueOf,
  flattenValue,
  geq,
 )
import PlutusLedgerApi.V3 (
  Address,
  CurrencySymbol,
  Datum (getDatum),
  FromData,
  OutputDatum (OutputDatum),
  POSIXTimeRange,
  ScriptContext,
  ScriptPurpose (Spending),
  ToData (..),
  TokenName (..),
  TxId (TxId),
  TxInInfo (..),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
  TxOutRef (TxOutRef),
  UnsafeFromData (..),
  Value,
  adaSymbol,
  adaToken,
  singleton,
 )
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins (blake2b_256, serialiseData)

unFlattenValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
unFlattenValue [] = mempty
unFlattenValue ((cs, tn, i) : vls) = assetClassValue (AssetClass (cs, tn)) i <> unFlattenValue vls

wrapTitle :: String -> String
wrapTitle s =
  "\n-------------------------\n"
    ++ s
    ++ "\n-------------------------\n"

showValue :: String -> Value -> String
showValue s v = wrapTitle s ++ intercalate "\n" (show <$> PlutusLedgerApi.V1.Value.flattenValue v)

encodeUtf8KV :: (BuiltinString, BuiltinString) -> (BuiltinByteString, BuiltinByteString)
encodeUtf8KV (k, v) = (encodeUtf8 k, encodeUtf8 v)

-- | A more efficient implementation of the `mkUntypedValidator` method of the `IsScriptContext` typeclass.
mkUntypedValidator ::
  ( UnsafeFromData a
  , UnsafeFromData b
  ) =>
  (a -> b -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
mkUntypedValidator f a b ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)
{-# INLINEABLE mkUntypedValidator #-}

-- | A more efficient implementation of the `mkUntypedMintingPolicy` method of the `IsScriptContext` typeclass.
mkUntypedMintingPolicy ::
  (UnsafeFromData a) =>
  (a -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
mkUntypedMintingPolicy f a ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)
{-# INLINEABLE mkUntypedMintingPolicy #-}

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

-- | A more efficient implementation of the `mkUntypedValidator` method of the `IsScriptContext` typeclass.
mkUntypedValidatorCustom ::
  ( UnsafeFromData a
  , UnsafeFromData b
  ) =>
  (a -> b -> AScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
mkUntypedValidatorCustom f a b ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)
{-# INLINEABLE mkUntypedValidatorCustom #-}

-- | A more efficient implementation of the `mkUntypedMintingPolicy` method of the `IsScriptContext` typeclass.
mkUntypedMintingPolicyCustom ::
  (UnsafeFromData a) =>
  (a -> AScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
mkUntypedMintingPolicyCustom f a ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)
{-# INLINEABLE mkUntypedMintingPolicyCustom #-}

------------------------

-- **  Helper Functions

------------------------
hasTxInWithToken :: AssetClass -> [TxInInfo] -> Bool
hasTxInWithToken tokenId = hasTxInWith ((#== 1) . (`assetClassValueOf` tokenId)) noConstraint
{-# INLINEABLE hasTxInWithToken #-}

getTokenHolderAddress :: AssetClass -> [TxInInfo] -> Address
getTokenHolderAddress tokenId txInfoInputs =
  txOutAddress . txInInfoResolved . head $
    findTxInWith ((#== 1) . (`assetClassValueOf` tokenId)) noConstraint txInfoInputs
{-# INLINEABLE getTokenHolderAddress #-}

{- | This function checks if in a list of outputs is at least a specific amount of lovelaces locked to a specific 'PublicKeyHash'.
 It takes one argument @pkh@ of type'PubKeyHash', one argument @n@ of type 'Integer', and a '[TxOut]'.
 It returns a 'True' if the sum of lovelaces locked at @pkh@'s address is at least @n@
-}
totalValueToAddress :: Address -> Integer -> [TxOut] -> Bool
{-# INLINEABLE totalValueToAddress #-}
totalValueToAddress addr lovelaces txOuts =
  let outsToAddres = filter ((#== addr) . txOutAddress) txOuts
      totalVal = psum (txOutValue #<$> outsToAddres)
   in assetClassValueOf totalVal (assetClass adaSymbol adaToken) #>= lovelaces

type ValueConstraint = Value -> Bool
type AddressConstraint = Address -> Bool

noConstraint :: b -> Bool
noConstraint = const True
{-# INLINEABLE noConstraint #-}

isTxOutWith :: ValueConstraint -> AddressConstraint -> TxOut -> Bool
isTxOutWith toValue toAddress TxOut {txOutValue, txOutAddress} = toValue txOutValue && toAddress txOutAddress
{-# INLINEABLE isTxOutWith #-}

isTxOutWithInlineDatumAnd :: (ToData a) => a -> ValueConstraint -> AddressConstraint -> TxOut -> Bool
isTxOutWithInlineDatumAnd datum toValue toAddress TxOut {txOutValue, txOutAddress, txOutDatum} = toValue txOutValue && toAddress txOutAddress && isGivenInlineDatum datum txOutDatum
{-# INLINEABLE isTxOutWithInlineDatumAnd #-}

-- isTxOutWithInlineDatumAnd' :: (ToData a) => a -> Value -> Address -> TxOut -> Bool
-- isTxOutWithInlineDatumAnd' datum value address TxOut {txOutValue, txOutAddress, txOutDatum} = value #== txOutValue && address #== txOutAddress && isGivenInlineDatum datum txOutDatum
-- {-# INLINEABLE isTxOutWithInlineDatumAnd' #-}

---------------------------------

-- | Helper function to check that a UTxO is being spent in the transaction.
hasTxInWithRef :: TxOutRef -> [TxInInfo] -> Bool
hasTxInWithRef oref = pany (\(TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasTxInWithRef #-}

findTxInWith :: ValueConstraint -> AddressConstraint -> [TxInInfo] -> [TxInInfo]
findTxInWith toValue toAddress = filter (isTxOutWith toValue toAddress . txInInfoResolved)
{-# INLINEABLE findTxInWith #-}

hasTxOutWith :: ValueConstraint -> AddressConstraint -> [TxOut] -> Bool
hasTxOutWith toValue addr = pany (isTxOutWith toValue addr)
{-# INLINEABLE hasTxOutWith #-}

hasTxInWith :: ValueConstraint -> AddressConstraint -> [TxInInfo] -> Bool
hasTxInWith toValue addr = hasTxOutWith toValue addr . (txInInfoResolved #<$>)
{-# INLINEABLE hasTxInWith #-}

getInlineDatum :: TxOut -> BuiltinData
getInlineDatum out = case txOutDatum out of
  OutputDatum da -> getDatum da
  _ -> traceError "No inline datum"
{-# INLINEABLE getInlineDatum #-}

getCurrentStateDatumAndValue :: AssetClass -> AddressConstraint -> [TxInInfo] -> (Value, BuiltinData)
getCurrentStateDatumAndValue stateToken toAddress outs = case filter (isTxOutWith (`geq` assetClassValue stateToken 1) toAddress . txInInfoResolved) outs of
  [TxInInfo _ out] -> (txOutValue out, getInlineDatum out)
  _ -> traceError "state nft not found"
{-# INLINEABLE getCurrentStateDatumAndValue #-}

hasTxOutWithInlineDatumAnd :: (ToData a) => a -> ValueConstraint -> AddressConstraint -> [TxOut] -> Bool
hasTxOutWithInlineDatumAnd datum toValue toAddress = traceIfFalse "not found tx out with datum" . pany (isTxOutWithInlineDatumAnd datum toValue toAddress)
{-# INLINEABLE hasTxOutWithInlineDatumAnd #-}

-- hasTxOutWithInlineDatumAnd' :: (ToData a) => a -> Value -> Address -> [TxOut] -> Bool
-- hasTxOutWithInlineDatumAnd' datum value address = traceIfFalse "not found tx out with datum" . pany (isTxOutWithInlineDatumAnd' datum value address)
-- {-# INLINEABLE hasTxOutWithInlineDatumAnd' #-}

isMintingNFT :: AssetClass -> Value -> Bool
isMintingNFT ac txInfoMint = traceIfFalse "NFT not minted" $ assetClassValueOf txInfoMint ac #== 1
{-# INLINEABLE isMintingNFT #-}

isBurningNFT :: AssetClass -> Value -> Bool
isBurningNFT ac txInfoMint = traceIfFalse "NFT not burned" $ assetClassValueOf txInfoMint ac #== pnegate 1
{-# INLINEABLE isBurningNFT #-}

integerToBs24 :: Integer -> BuiltinByteString
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

--- >>> bsToInteger "marius"
-- 120265298769267

-- | Helper function to check if a 'TxOut' contains exactly 1 quantity of an AssetClass
outHas1of :: TxOut -> AssetClass -> Bool
outHas1of (TxOut _ value _ _) ac = assetClassValueOf value ac #== 1
{-# INLINEABLE outHas1of #-}

-- | Helper function to check if a 'TxOut' contains a given datum and is inlined.
hasGivenInlineDatum :: ToData a => a -> TxOut -> Bool
hasGivenInlineDatum datum out = case txOutDatum out of
  OutputDatum da -> toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE hasGivenInlineDatum #-}

isGivenInlineDatum :: ToData a => a -> OutputDatum -> Bool
isGivenInlineDatum datum outdat = case outdat of
  OutputDatum da -> toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE isGivenInlineDatum #-}

isGivenInlineDatumWith :: ToData a => (a, a -> a) -> OutputDatum -> Bool
isGivenInlineDatumWith (datum, f) outdat = case outdat of
  OutputDatum da -> toBuiltinData (f datum) #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE isGivenInlineDatumWith #-}

-- | Helper function to check if a 'TxOut' contains a given datum and is inlined.
hasInlinedDatumWith :: (FromData a, UnsafeFromData a) => (a -> Bool) -> OutputDatum -> Bool
hasInlinedDatumWith toDatum outdat = case outdat of
  OutputDatum da -> toDatum (unsafeFromBuiltinData (getDatum da))
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE hasInlinedDatumWith #-}

-- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
inputHas1of :: TxInInfo -> AssetClass -> Bool
inputHas1of = outHas1of . txInInfoResolved
{-# INLINEABLE inputHas1of #-}

-- | Helper function: check that the validating input contains a given token
spendsToken :: AssetClass -> AScriptContext -> Bool
spendsToken proofToken sc =
  "The transaction must spend the state token"
    `traceIfFalse` case (`inputHas1of` proofToken) #<$> findOwnInputA sc of
      Nothing -> trace "Own input not found" False
      Just result -> traceIfFalse ("Token not spent: " #<> (decodeUtf8 . unTokenName . snd . unAssetClass $ proofToken)) result
{-# INLINEABLE spendsToken #-}

tokenNameFromTxOutRef :: TxOutRef -> TokenName
tokenNameFromTxOutRef (TxOutRef (TxId txIdbs) txIdx) = TokenName (takeByteString 28 $ blake2b_256 (txIdbs #<> (serialiseData . toBuiltinData) txIdx))
{-# INLINEABLE tokenNameFromTxOutRef #-}

getOwnInput :: AScriptContext -> TxOut
getOwnInput context = case findOwnInputA context of
  Nothing -> traceError "Own input not found"
  Just (TxInInfo _inOutRef inOut) -> inOut
{-# INLINEABLE getOwnInput #-}

-- | Find the input currently being validated.
findOwnInputA :: AScriptContext -> Maybe TxInInfo
findOwnInputA AScriptContext {scriptContextTxInfo = ATxInfo {txInfoInputs}, scriptContextPurpose = Spending txOutRef} =
  find (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef #== txOutRef) txInfoInputs
findOwnInputA _ = Nothing
{-# INLINEABLE findOwnInputA #-}

adaValueFromLovelaces :: Integer -> Value
adaValueFromLovelaces = singleton adaSymbol adaToken
{-# INLINEABLE adaValueFromLovelaces #-}
