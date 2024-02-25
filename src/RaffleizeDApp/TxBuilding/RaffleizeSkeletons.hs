module RaffleizeDApp.TxBuilding.RaffleizeSkeletons where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.OnChain.RaffleizeMintingPolicy
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.RaffleizeLookups
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * Construct Transaction Components

------------------------------------------------------------------------------------------------

txIsPayingValueToAddress :: (HasCallStack, GYTxMonad m) => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV2)
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
  after36hSlot <- advanceSlot' now 129600 -- 36h in seconds
  after36hTime <- pPOSIXTimeFromGYSlot after36hSlot
  validUntil <- gySlotFromPOSIXTime (min ddl after36hTime)
  return $ isValidBetween now validUntil 

txMustSpendStateFromRefScriptWithRedeemer :: (HasCallStack, GYTxMonad m, ToData a) => GYTxOutRef -> AssetClass -> a -> GYValidator 'PlutusV2 -> m (GYTxSkeleton 'PlutusV2)
txMustSpendStateFromRefScriptWithRedeemer refScript stateTokenId redeemer gyValidator =
  do
    let gyRedeemer = redeemerFromPlutusData redeemer
    stateUTxO <- lookuptUTxOWithStateToken stateTokenId gyValidator
    (gyDatum, _v) <- gyGetInlineDatumAndValue stateUTxO
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef stateUTxO
          , gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) gyDatum gyRedeemer
          }

txMustHaveStateAsRefInput :: (HasCallStack, GYTxMonad m) => AssetClass -> GYValidator 'PlutusV2 -> m (GYTxSkeleton 'PlutusV2)
txMustHaveStateAsRefInput stateTokenId gyValidator = do
  stateUTxO <- lookuptUTxOWithStateToken stateTokenId gyValidator
  return $ mustHaveRefInput (utxoRef stateUTxO)

txMustSpendFromAddress :: (HasCallStack, GYTxMonad m) => AssetClass -> GYAddress -> m (GYTxSkeleton 'PlutusV2)
txMustSpendFromAddress tokenId addr = do
  do
    tokenUtxo <- lookupTxOWithTokenAtAddress tokenId addr
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef tokenUtxo
          , gyTxInWitness = GYTxInWitnessKey
          }

txMustLockStateWithInlineDatumAndValue :: (HasCallStack, GYTxMonad m, ToData a) => GYValidator 'PlutusV2 -> a -> Value -> m (GYTxSkeleton 'PlutusV2)
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

txNFTAction :: (HasCallStack, GYTxMonad m) => RaffleizeMintingReedemer -> m (GYTxSkeleton 'PlutusV2)
txNFTAction redeemer = do
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
  case redeemer of
    MintRaffle _ tor  -> do
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
      (raffle, _) <- lookupRaffleStateDataAndValue raffleRefAC
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
