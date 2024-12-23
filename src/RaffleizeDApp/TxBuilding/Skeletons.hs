module RaffleizeDApp.TxBuilding.Skeletons where

import GHC.Stack
import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.OnChain.RaffleizeMintingPolicy
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * Construct Transaction Components

------------------------------------------------------------------------------------------------

txIsPayingValueToAddress :: (HasCallStack, GYTxUserQueryMonad m) => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV2)
txIsPayingValueToAddress recipient gyValue = do
  return $
    mustHaveOutput -- pays raffle user token to the user receiving address
      GYTxOut
        { gyTxOutAddress = recipient
        , gyTxOutDatum = Nothing
        , gyTxOutValue = gyValue
        , gyTxOutRefS = Nothing
        }

isValidBetween :: GYSlot -> GYSlot -> GYTxSkeleton 'PlutusV2
isValidBetween s1 s2 =
  mconcat
    [ isInvalidBefore s1
    , isInvalidAfter s2
    ]

txIsValidByDDL :: (HasCallStack, GYTxQueryMonad m) => POSIXTime -> m (GYTxSkeleton 'PlutusV2)
txIsValidByDDL ddl = do
  now <- slotOfCurrentBlock
  after36hSlot <- advanceSlot' now 25920 -- ~7h in seconds (era safe zone)
  after36hTime <- pPOSIXTimeFromGYSlot after36hSlot
  validUntil <- gySlotFromPOSIXTime (min ddl after36hTime)
  return $ isValidBetween now validUntil

txMustSpendStateFromRefScriptWithRedeemer :: (HasCallStack, GYTxUserQueryMonad m, ToData a) => GYTxOutRef -> AssetClass -> a -> GYValidator 'PlutusV2 -> m (GYTxSkeleton 'PlutusV2)
txMustSpendStateFromRefScriptWithRedeemer refScript stateTokenId redeemer gyValidator =
  do
    let gyRedeemer = redeemerFromPlutusData redeemer
    validatorAddr <- scriptAddress gyValidator
    stateUTxO <- getUTxOWithStateToken stateTokenId validatorAddr
    (gyDatum, _v) <- gyGetInlineDatumAndValue' stateUTxO
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef stateUTxO
          , gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) gyDatum gyRedeemer
          }
  where
    gyGetInlineDatumAndValue' :: (MonadError GYTxMonadException m) => GYUTxO -> m (GYDatum, GYValue)
    gyGetInlineDatumAndValue' utxo = maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ getInlineDatumAndValue utxo

txMustHaveStateAsRefInput :: (HasCallStack, GYTxUserQueryMonad m) => AssetClass -> GYValidator 'PlutusV2 -> m (GYTxSkeleton 'PlutusV2)
txMustHaveStateAsRefInput stateTokenId gyValidator = do
  validatorAddr <- scriptAddress gyValidator
  stateUTxO <- getUTxOWithStateToken stateTokenId validatorAddr
  return $ mustHaveRefInput (utxoRef stateUTxO)

txMustSpendFromAddress :: (HasCallStack, GYTxUserQueryMonad m) => AssetClass -> [GYAddress] -> m (GYTxSkeleton 'PlutusV2)
txMustSpendFromAddress tokenId addrs = do
  do
    tokenUtxo <- getUTxOWithStateTokenAtAddresses tokenId addrs
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef tokenUtxo
          , gyTxInWitness = GYTxInWitnessKey
          }

txMustLockStateWithInlineDatumAndValue :: (HasCallStack, GYTxUserQueryMonad m, ToData a) => GYValidator 'PlutusV2 -> a -> Value -> m (GYTxSkeleton 'PlutusV2)
txMustLockStateWithInlineDatumAndValue validator todata pValue = do
  raffleizeValidatorAddressGY <- scriptAddress validator
  gyValue <- valueFromPlutus' pValue
  let gyDatum = datumFromPlutusData todata
  return $
    mustHaveOutput -- pays raffle ref token to validator address with valid datum
      GYTxOut
        { gyTxOutAddress = raffleizeValidatorAddressGY
        , gyTxOutDatum = Just (gyDatum, GYTxOutUseInlineDatum)
        , gyTxOutValue = gyValue
        , gyTxOutRefS = Nothing
        }

txNFTAction :: (HasCallStack, GYTxUserQueryMonad m) => RaffleizeMintingReedemer -> m (GYTxSkeleton 'PlutusV2)
txNFTAction redeemer = do
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
  case redeemer of
    MintRaffle _ tor -> do
      let (raffleRefTN, raffleUserTN) = generateRefAndUserTN $ tokenNameFromTxOutRef tor
      let raffleRefAC = AssetClass (mintingPolicyCurrencySymbol raffleizeMintingPolicyGY, raffleRefTN)
      let raffleUserAC = AssetClass (mintingPolicyCurrencySymbol raffleizeMintingPolicyGY, raffleUserTN)
      gyRaffleRefTN <- tokenNameFromPlutus' (snd . unAssetClass $ raffleRefAC)
      gyRaffleUserTN <- tokenNameFromPlutus' (snd . unAssetClass $ raffleUserAC)
      return $ --
        mconcat
          [ mustMint (GYMintScript raffleizeMintingPolicyGY) gyRedeemer gyRaffleRefTN 1
          , mustMint (GYMintScript raffleizeMintingPolicyGY) gyRedeemer gyRaffleUserTN 1
          ]
    MintTicket raffleRefAC -> do
      (raffle, _) <- getRaffleStateDataAndValue raffleRefAC
      let (ticketRefAC, ticketUserAC) = getNextTicketToMintAssetClasses raffle -- Generate ticket tokens based on no. of tickets sold.
      gyTicketRefTN <- tokenNameFromPlutus' (snd . unAssetClass $ ticketRefAC)
      gyTicketUserTN <- tokenNameFromPlutus' (snd . unAssetClass $ ticketUserAC)
      return $
        mconcat
          [ mustMint (GYMintScript raffleizeMintingPolicyGY) gyRedeemer gyTicketRefTN 1
          , mustMint (GYMintScript raffleizeMintingPolicyGY) gyRedeemer gyTicketUserTN 1
          ]
    Burn ac -> do
      gyTN <- tokenNameFromPlutus' (snd . unAssetClass $ ac)
      return $ mustMint (GYMintScript raffleizeMintingPolicyGY) gyRedeemer gyTN (negate 1)

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV2 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
  addr <- scriptAddress limboValidatorV2
  addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV2 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
  return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) {gyTxOutRefS = Just $ GYPlutusScript sc}