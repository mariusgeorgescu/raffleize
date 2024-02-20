module RaffleizeDApp.TxBuilding.RaffleizeOperations where

import GeniusYield.Imports hiding (fromMaybe)
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.OnChain.RaffleizeLogic (buyTicketToRaffle, deriveUserFromRefAC, generateRefAndUserTN, getNextTicketToMintAssetClasses, raffleStakeValue, raffleTicketCollateralValue, raffleTicketPriceValue, revealTicketToRaffleRT, updateRaffleStateValue)
import RaffleizeDApp.OnChain.RaffleizeMintingPolicy
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.RaffleizeLookups
import RaffleizeDApp.TxBuilding.RaffleizeSkeletons
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- *  User Actions

------------------------------------------------------------------------------------------------

-- |  Create Raffle Transaction
createRaffleTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYAddress -> RaffleConfig -> m (GYTxSkeleton 'PlutusV2, AssetClass)
createRaffleTX recipient config@RaffleConfig {rCommitDDL, rStake} = do
  isValidByCommitDDL <- txIsValidByDDL rCommitDDL
  let recipientPlutus = addressToPlutus recipient
  seedTxOutRef <- someUTxOWithoutRefScript
  let seedTxOutRefPlutus = txOutRefToPlutus seedTxOutRef
  isMintingRaffleNFTs <- txNFTAction (MintRaffle config seedTxOutRefPlutus recipientPlutus)
  let (raffleRefTN, raffleUserTN) = generateRefAndUserTN $ tokenNameFromTxOutRef seedTxOutRefPlutus
  let raffleRefAC = AssetClass (mintingPolicyCurrencySymbol raffleizeMintingPolicyGY, raffleRefTN)
  let rsd = mkNewRaffle raffleRefAC mockRaffleParam config
  isLockingRaffleState <-
    txMustLockStateWithInlineDatumAndValue
      raffleizeValidatorGY
      (mkRaffleDatum rsd)
      (rStake #+ lovelaceValueOf (rRaffleCollateral mockRaffleParam) #+ assetClassValue raffleRefAC 1)
  return
    ( mconcat
        [ isValidByCommitDDL
        , isMintingRaffleNFTs
        , isLockingRaffleState
        ]
    , raffleRefAC
    )

-- |  Buy Ticket Transaction
buyTicketTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> SecretHash -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2, AssetClass)
buyTicketTX raffleScriptRef secretHash ownAddr raffleRefAC = do
  (rsd, rValue) <- lookupRaffleStateDataAndValue raffleRefAC
  isValidByCommitDDL <- txIsValidByDDL (rCommitDDL . rConfig $ rsd)
  let buyRedeemer = User (BuyTicket secretHash)
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC buyRedeemer raffleizeValidatorGY
  let (new_rsd, new_tsd) = buyTicketToRaffle secretHash rsd (rRaffleValidatorHash . rParam $ rsd)
  isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum new_rsd) (rValue #+ raffleTicketPriceValue rsd)
  isMintingTicketRefAnUserNFTs <- txNFTAction (MintTicket raffleRefAC)
  let (ticketRefAC, ticketUserAC) = getNextTicketToMintAssetClasses rsd -- Generate ticket tokens based on no. of tickets sold.
  let ticketRefNFTp = assetClassValue ticketRefAC 1
  let ticketUserNFTp = assetClassValue ticketUserAC 1
  ticketUserNFT <- valueFromPlutus' ticketUserNFTp
  isTicketStateLocked <- txMustLockStateWithInlineDatumAndValue ticketValidatorGY (mkTicketDatum new_tsd) (ticketRefNFTp #+ raffleTicketCollateralValue rsd)
  isGettingTicketUserNFT <- txIsPayingValueToAddress ownAddr ticketUserNFT
  return
    ( mconcat
        [ isValidByCommitDDL
        , spendsRaffleRefNFT
        , isRaffleStateUpdated
        , isMintingTicketRefAnUserNFTs
        , isTicketStateLocked
        , isGettingTicketUserNFT
        ]
    , ticketRefAC
    )

------------------------------------------------------------------------------------------------

-- * Raffle Owner Actions

------------------------------------------------------------------------------------------------

-- |  Update Raffle Transaction
updateRaffleTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> RaffleConfig -> m (GYTxSkeleton 'PlutusV2)
updateRaffleTX raffleScriptRef ownAddr raffleRefAC newConfig = do
  (rsd, rValue) <- lookupRaffleStateDataAndValue raffleRefAC
  let ddl = min (rCommitDDL . rConfig $ rsd) (rCommitDDL newConfig) -- minimum between initial and new commit deadline
  isValidByCommitDDL <- txIsValidByDDL ddl
  let updateRedeemer = RaffleOwner (Update newConfig)
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC updateRedeemer raffleizeValidatorGY
  let new_rsd = rsd {rConfig = newConfig}
  isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum new_rsd) rValue
  let raffleUserAC = deriveUserFromRefAC raffleRefAC
  spendsRaffleUserNFT <- txMustSpendFromAddress raffleUserAC ownAddr

  return $
    mconcat
      [ isValidByCommitDDL
      , spendsRaffleRefNFT
      , spendsRaffleUserNFT
      , isRaffleStateUpdated
      ]

-- |  Cancel Transaction
cancelRaffleTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
cancelRaffleTX raffleScriptRef ownAddr raffleRefAC = do
  (rsd, _rValue) <- lookupRaffleStateDataAndValue raffleRefAC
  isValidByCommitDDL <- txIsValidByDDL (rCommitDDL . rConfig $ rsd)
  let cancelRedeemer = RaffleOwner Cancel
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC cancelRedeemer raffleizeValidatorGY
  isBurningRaffleRefNFT <- txNFTAction (Burn raffleRefAC)
  let raffleUserAC = deriveUserFromRefAC raffleRefAC
  spendsRaffleUserNFT <- txMustSpendFromAddress raffleUserAC ownAddr
  isBurningRaffleUserNFT <- txNFTAction (Burn raffleUserAC)
  stakeValue <- valueFromPlutus' $ raffleStakeValue rsd
  isGettingStakeValue <- txIsPayingValueToAddress ownAddr stakeValue

  return $
    mconcat
      [ isValidByCommitDDL
      , spendsRaffleRefNFT
      , spendsRaffleUserNFT
      , isBurningRaffleRefNFT
      , isBurningRaffleUserNFT
      , isGettingStakeValue
      ]

-- | Collect Accumulated Amount Transaction
collectAmountTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
collectAmountTX = raffleOwnerClosingTX CollectAmount

-- | Recover Stake Transaction
recoverStakeTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
recoverStakeTX = raffleOwnerClosingTX RecoverStake

-- | Recover Stake And Accumulated Amount Transaction
recoverStakeAndAmountTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
recoverStakeAndAmountTX = raffleOwnerClosingTX RecoverStakeAndAmount

-- | Helper function to construct the raffle owner closing transaction based on the raffle owner action
raffleOwnerClosingTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => RaffleOwnerAction -> GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
raffleOwnerClosingTX roa raffleScriptRef ownAddr raffleRefAC
  | roa `elem` [CollectAmount, RecoverStake, RecoverStakeAndAmount] = do
      let redeemerAction = RaffleOwner roa
      (rsd, rValue) <- lookupRaffleStateDataAndValue raffleRefAC
      let newValue = updateRaffleStateValue redeemerAction rsd rValue
      spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC redeemerAction raffleizeValidatorGY
      let raffleUserAC = deriveUserFromRefAC raffleRefAC
      isBurningRaffleUserNFT <- txNFTAction (Burn raffleUserAC)
      isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum rsd) newValue
      diffValue <- valueFromPlutus' (rValue #- newValue)
      isGettingTheDifference <- txIsPayingValueToAddress ownAddr diffValue
      return $
        mconcat
          [ spendsRaffleRefNFT
          , isRaffleStateUpdated
          , isBurningRaffleUserNFT
          , isGettingTheDifference
          ]
raffleOwnerClosingTX _ _ _ _ = error "Invalid Raffle Owner Action"

-- | Get Collateral of Expired Ticket Transaction
getCollateralOfExpiredTicketTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
getCollateralOfExpiredTicketTX ticketScriptRef ownAddr ticketRefAC = do
  (tsd, tValue) <- lookupTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  let raffleUserAC = deriveUserFromRefAC raffleRefAC
  spendsRaffleUserNFT <- txMustSpendFromAddress raffleUserAC ownAddr
  hasRaffleStateAsReferenceInput <- txMustHaveStateAsRefInput raffleRefAC raffleizeValidatorGY
  spendsTicketRefNFT <-
    txMustSpendStateFromRefScriptWithRedeemer
      ticketScriptRef
      ticketRefAC
      (RaffleOwner GetCollateraOfExpiredTicket)
      ticketValidatorGY
  isBurningTicketRefNFT <- txNFTAction (Burn ticketRefAC)
  ticketCollateralValue <- valueFromPlutus' tValue
  isGettingCollateralValue <- txIsPayingValueToAddress ownAddr ticketCollateralValue
  return $
    mconcat
      [ spendsRaffleUserNFT
      , hasRaffleStateAsReferenceInput
      , spendsTicketRefNFT
      , isBurningTicketRefNFT
      , isGettingCollateralValue
      ]

------------------------------------------------------------------------------------------------

-- * Ticket Owner Actions

------------------------------------------------------------------------------------------------

-- | Reveal Ticket Transaction
revealTicketTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYTxOutRef -> Secret -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
revealTicketTX raffleScriptRef ticketScriptRef secret ownAddr ticketRefAC = do
  (tsd, tValue) <- lookupTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  (rsd, rValue) <- lookupRaffleStateDataAndValue raffleRefAC
  isValidByRevealDDL <- txIsValidByDDL (rRevealDDL . rConfig $ rsd)
  let revealRedeemer = TicketOwner (RevealTicketSecret secret) ticketRefAC
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC revealRedeemer raffleizeValidatorGY
  spendsTicketRefNFT <- txMustSpendStateFromRefScriptWithRedeemer ticketScriptRef ticketRefAC revealRedeemer ticketValidatorGY
  let (new_rsd, new_tsd) = revealTicketToRaffleRT secret tsd rsd
  isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum new_rsd) rValue
  isTicketStateUpdated <- txMustLockStateWithInlineDatumAndValue ticketValidatorGY (mkTicketDatum new_tsd) tValue
  let ticketUserAC = deriveUserFromRefAC ticketRefAC
  let ticketUserNFTp = assetClassValue ticketUserAC 1
  ticketUserNFT <- valueFromPlutus' ticketUserNFTp
  isGettingTicketUserNFT <- txIsPayingValueToAddress ownAddr ticketUserNFT
  return
    ( mconcat
        [ isValidByRevealDDL
        , spendsRaffleRefNFT
        , isRaffleStateUpdated
        , spendsTicketRefNFT
        , isTicketStateUpdated
        , isGettingTicketUserNFT
        ]
    )

-- | Winner Collect Stake Transaction
winnerCollectStakeTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
winnerCollectStakeTX = ticketOwnerClosingTX CollectStake

-- | Full Refund Ticket Transaction
fullRefundTicketTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
fullRefundTicketTX = ticketOwnerClosingTX RefundTicket

-- | Extra Refund Ticket Transaction
extraRefundTicketTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
extraRefundTicketTX = ticketOwnerClosingTX RefundTicketExtra

-- | Helper function to construct the ticket owner closing transaction based on the ticket owner action
ticketOwnerClosingTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => TicketOwnerAction -> GYTxOutRef -> GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
ticketOwnerClosingTX toa raffleScriptRef ticketScriptRef ownAddr ticketRefAC = do
  unless (toa `elem` [CollectStake, RefundTicket, RefundTicketExtra]) $ error "Invalid Ticket Owner Action"
  let redeemerAction = TicketOwner toa ticketRefAC
  (tsd, _tValue) <- lookupTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  (rsd, rValue) <- lookupRaffleStateDataAndValue raffleRefAC
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC redeemerAction raffleizeValidatorGY
  spendsTicketRefNFT <- txMustSpendStateFromRefScriptWithRedeemer ticketScriptRef ticketRefAC redeemerAction ticketValidatorGY
  let newValue = updateRaffleStateValue redeemerAction rsd rValue
  isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum rsd) newValue
  let ticketUserAC = deriveUserFromRefAC ticketRefAC
  isBurningTicketUserNFT <- txNFTAction (Burn ticketUserAC)
  isBurningTicketRefNFT <- txNFTAction (Burn ticketRefAC)
  diffValue <- valueFromPlutus' (rValue #- newValue)
  isGettingStakeAndTicketCollateral <- txIsPayingValueToAddress ownAddr diffValue
  return
    ( mconcat
        [ spendsRaffleRefNFT
        , spendsTicketRefNFT
        , isRaffleStateUpdated
        , isBurningTicketRefNFT
        , isBurningTicketUserNFT
        , isGettingStakeAndTicketCollateral
        ]
    )

-- | Refund Collateral of Losing Ticket Transaction
refundCollateralOfLosingTicketTX :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => GYTxOutRef -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV2)
refundCollateralOfLosingTicketTX ticketScriptRef ownAddr ticketRefAC = do
  (tsd, tValue) <- lookupTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  let ticketUserAC = deriveUserFromRefAC ticketRefAC
  hasRaffleStateAsReferenceInput <- txMustHaveStateAsRefInput raffleRefAC raffleizeValidatorGY
  spendsTicketRefNFT <-
    txMustSpendStateFromRefScriptWithRedeemer
      ticketScriptRef
      ticketRefAC
      (TicketOwner RefundCollateralLosing ticketRefAC)
      ticketValidatorGY
  isBurningTicketRefNFT <- txNFTAction (Burn ticketRefAC)
  isBurningTicketUserNFT <- txNFTAction (Burn ticketUserAC)
  ticketCollateralValue <- valueFromPlutus' tValue
  isGettingCollateralValue <- txIsPayingValueToAddress ownAddr ticketCollateralValue
  return $
    mconcat
      [ hasRaffleStateAsReferenceInput
      , spendsTicketRefNFT
      , isBurningTicketRefNFT
      , isBurningTicketUserNFT
      , isGettingCollateralValue
      ]

------------------------------------------------------------------------------------------------

-- * Admin  Actions

------------------------------------------------------------------------------------------------