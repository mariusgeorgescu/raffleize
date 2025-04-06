module RaffleizeDApp.TxBuilding.Skeletons where

import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
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

txIsPayingValueToAddress :: (GYTxUserQueryMonad m) => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txIsPayingValueToAddress recipient gyValue = do
  return $
    mustHaveOutput -- pays raffle user token to the user receiving address
      GYTxOut
        { gyTxOutAddress = recipient,
          gyTxOutDatum = Nothing,
          gyTxOutValue = gyValue,
          gyTxOutRefS = Nothing
        }

isValidBetween :: GYSlot -> GYSlot -> GYTxSkeleton 'PlutusV3
isValidBetween s1 s2 =
  mconcat
    [ isInvalidBefore s1,
      isInvalidAfter s2
    ]

txIsValidByDDL :: (GYTxQueryMonad m) => POSIXTime -> m (GYTxSkeleton 'PlutusV3)
txIsValidByDDL ddl = do
  now <- slotOfCurrentBlock
  after36hSlot <- advanceSlot' now 25920 -- ~7h in seconds (era safe zone)
  after36hTime <- pPOSIXTimeFromGYSlot after36hSlot
  validUntil <- gySlotFromPOSIXTime (min ddl after36hTime)
  return $ isValidBetween now validUntil

txMustSpendStateFromRefScriptWithRedeemer :: (GYTxUserQueryMonad m, ToData a) => GYTxOutRef -> AssetClass -> a -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
txMustSpendStateFromRefScriptWithRedeemer refScript stateTokenId redeemer gyValidator =
  do
    let gyRedeemer = redeemerFromPlutusData redeemer
    validatorAddr <- scriptAddress gyValidator
    stateUTxO <- getUTxOWithStateToken stateTokenId validatorAddr
    (gyDatum, _v) <- gyGetInlineDatumAndValue' stateUTxO
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef stateUTxO,
            gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) (Just gyDatum) gyRedeemer
          }
  where
    gyGetInlineDatumAndValue' :: (MonadError GYTxMonadException m) => GYUTxO -> m (GYDatum, GYValue)
    gyGetInlineDatumAndValue' utxo = maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ getInlineDatumAndValue utxo

txMustHaveStateAsRefInput :: (GYTxUserQueryMonad m) => AssetClass -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
txMustHaveStateAsRefInput stateTokenId gyValidator = do
  validatorAddr <- scriptAddress gyValidator
  stateUTxO <- getUTxOWithStateToken stateTokenId validatorAddr
  return $ mustHaveRefInput (utxoRef stateUTxO)

txMustSpendFromAddress :: (GYTxUserQueryMonad m) => AssetClass -> [GYAddress] -> m (GYTxSkeleton 'PlutusV3)
txMustSpendFromAddress tokenId addrs = do
  do
    tokenUtxo <- getUTxOWithStateTokenAtAddresses tokenId addrs
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef tokenUtxo,
            gyTxInWitness = GYTxInWitnessKey
          }

txMustLockStateWithInlineDatumAndValue :: (GYTxUserQueryMonad m, ToData a) => GYScript 'PlutusV3 -> a -> Value -> m (GYTxSkeleton 'PlutusV3)
txMustLockStateWithInlineDatumAndValue validator todata pValue = do
  raffleizeValidatorAddressGY <- scriptAddress validator
  gyValue <- valueFromPlutus' pValue
  let gyDatum = datumFromPlutusData todata
  return $
    mustHaveOutput -- pays raffle ref token to validator address with valid datum
      GYTxOut
        { gyTxOutAddress = raffleizeValidatorAddressGY,
          gyTxOutDatum = Just (gyDatum, GYTxOutUseInlineDatum),
          gyTxOutValue = gyValue,
          gyTxOutRefS = Nothing
        }

txNFTAction :: (GYTxUserQueryMonad m) => GYTxOutRef -> RaffleizeMintingReedemer -> m (GYTxSkeleton 'PlutusV3)
txNFTAction mpRefScript redeemer = do
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
          [ mustMint (GYMintReference mpRefScript raffleizeMintingPolicyGY) gyRedeemer gyRaffleRefTN 1,
            mustMint (GYMintReference mpRefScript raffleizeMintingPolicyGY) gyRedeemer gyRaffleUserTN 1
          ]
    MintTicket raffleRefAC -> do
      (raffle, _) <- getRaffleStateDataAndValue raffleRefAC
      let (ticketRefAC, ticketUserAC) = getNextTicketToMintAssetClasses raffle -- Generate ticket tokens based on no. of tickets sold.
      gyTicketRefTN <- tokenNameFromPlutus' (snd . unAssetClass $ ticketRefAC)
      gyTicketUserTN <- tokenNameFromPlutus' (snd . unAssetClass $ ticketUserAC)
      return $
        mconcat
          [ mustMint (GYMintReference mpRefScript raffleizeMintingPolicyGY) gyRedeemer gyTicketRefTN 1,
            mustMint (GYMintReference mpRefScript raffleizeMintingPolicyGY) gyRedeemer gyTicketUserTN 1
          ]
    Burn ac -> do
      gyTN <- tokenNameFromPlutus' (snd . unAssetClass $ ac)
      return $ mustMint (GYMintScript raffleizeMintingPolicyGY) gyRedeemer gyTN (negate 1)

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
  addr <- scriptAddress limboValidatorV2
  addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
  return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) {gyTxOutRefS = Just $ GYPlutusScript sc}