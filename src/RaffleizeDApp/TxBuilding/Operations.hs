module RaffleizeDApp.TxBuilding.Operations where

import Control.Monad.Reader.Class
import GeniusYield.Imports hiding (fromMaybe)
import GeniusYield.TxBuilder hiding (User)
import GeniusYield.Types
import PlutusLedgerApi.Data.V3 (TxId (TxId), TxOutRef (TxOutRef))
import PlutusLedgerApi.V1.Tx qualified
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.OnChain.RaffleizeLogic (buyTicketToRaffle, deriveUserFromRefAC, generateRefAndUserTN, getNextTicketToMintAssetClasses, raffleTicketPriceValue, redeemerToAction, revealTicketToRaffleRT, ticketCollateralValue, tokenNameFromTxOutRef, updateRaffleStateValue)
import RaffleizeDApp.OnChain.RaffleizeMintingPolicy

import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Skeletons
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- *  User Actions

------------------------------------------------------------------------------------------------

-- |  Create Raffle Transaction
createRaffleTX ::
  (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  GYAddress ->
  RaffleConfig ->
  m (GYTxSkeleton 'PlutusV3, AssetClass)
createRaffleTX recipient config@RaffleConfig {rCommitDDL, rStake} = do
  mpScriptRef <- asks mintingPolicyRef
  isValidByCommitDDL <- txIsValidByDDL rCommitDDL
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (PlutusLedgerApi.V1.Tx.TxOutRef (PlutusLedgerApi.V1.Tx.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = TxOutRef (TxId bs) i
  isMintingRaffleNFTs <- txNFTAction mpScriptRef (MintRaffle config seedTxOutRefPlutus) []
  let (raffleRefTN, raffleUserTN) = generateRefAndUserTN $ tokenNameFromTxOutRef seedTxOutRefPlutus
  let cs = mintingPolicyCurrencySymbol raffleizeMintingPolicyGY
  let (raffleRefAC, raffleUserAC) = (AssetClass (cs, raffleRefTN), AssetClass (cs, raffleUserTN))
  let rsd = mkNewRaffle raffleRefAC mockRaffleParam config
  isLockingRaffleState <-
    txMustLockStateWithInlineDatumAndValue
      raffleizeValidatorGY
      (mkRaffleDatum rsd)
      (rStake #+ GeniusYield.Types.lovelaceValueOf (rRaffleCollateral mockRaffleParam) #+ assetClassValue raffleRefAC 1)
  let raffleUserNFTp = assetClassValue raffleUserAC 1
  raffleUserNFT <- valueFromPlutus' raffleUserNFTp
  isGettingRaffleUserNFT <- txIsPayingValueToAddress recipient raffleUserNFT
  return
    ( mconcat
        [ isValidByCommitDDL,
          isMintingRaffleNFTs,
          isLockingRaffleState,
          isGettingRaffleUserNFT,
          isSpendingSeedUTxO
        ],
      raffleRefAC
    )

-- |  Buy Ticket Transaction
buyTicketTX ::
  (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  SecretHash ->
  GYAddress ->
  AssetClass ->
  m (GYTxSkeleton 'PlutusV3, AssetClass)
buyTicketTX secretHash recipient raffleRefAC = do
  mpScriptRef <- asks mintingPolicyRef
  raffleScriptRef <- asks raffleValidatorRef
  (rsd, rValue) <- getRaffleStateDataAndValue raffleRefAC
  isValidByCommitDDL <- txIsValidByDDL (rCommitDDL . rConfig $ rsd)
  let buyRedeemer = User (BuyTicket secretHash)
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC buyRedeemer raffleizeValidatorGY
  let (new_rsd, new_tsd) = buyTicketToRaffle secretHash rsd (rRaffleValidatorHash . rParam $ rsd)
  isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum new_rsd) (rValue #+ raffleTicketPriceValue rsd)
  isMintingTicketRefAnUserNFTs <- txNFTAction mpScriptRef (MintTicket raffleRefAC secretHash) []
  let (ticketRefAC, ticketUserAC) = getNextTicketToMintAssetClasses rsd -- Generate ticket tokens based on no. of tickets sold.
  let ticketassetClassContextNFTp = assetClassValue ticketRefAC 1
  let ticketUserNFTp = assetClassValue ticketUserAC 1
  ticketUserNFT <- valueFromPlutus' ticketUserNFTp
  isTicketStateLocked <- txMustLockStateWithInlineDatumAndValue ticketValidatorGY (mkTicketDatum new_tsd) (ticketassetClassContextNFTp #+ ticketCollateralValue rsd)
  isGettingTicketUserNFT <- txIsPayingValueToAddress recipient ticketUserNFT
  return
    ( mconcat
        [ isValidByCommitDDL,
          spendsRaffleRefNFT,
          isRaffleStateUpdated,
          isMintingTicketRefAnUserNFTs,
          isTicketStateLocked,
          isGettingTicketUserNFT
        ],
      ticketRefAC
    )

------------------------------------------------------------------------------------------------

-- * Raffle Owner Actions

------------------------------------------------------------------------------------------------

-- |  Update Raffle Transaction
updateRaffleTX ::
  (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  GYAddress ->
  RaffleConfig ->
  [GYAddress] ->
  AssetClass ->
  m (GYTxSkeleton 'PlutusV3)
updateRaffleTX recipient newConfig ownAddrs raffleRefAC = do
  raffleScriptRef <- asks raffleValidatorRef
  (rsd, rValue) <- getRaffleStateDataAndValue raffleRefAC
  let ddl = min (rCommitDDL . rConfig $ rsd) (rCommitDDL newConfig) -- minimum between initial and new commit deadline
  isValidByCommitDDL <- txIsValidByDDL ddl
  let updateRedeemer = RaffleOwner (Update newConfig)
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC updateRedeemer raffleizeValidatorGY
  let new_rValue = rValue #- (rStake . rConfig $ rsd) #+ rStake newConfig
  let new_rsd = rsd {rConfig = newConfig}
  isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum new_rsd) new_rValue
  let raffleUserAC = deriveUserFromRefAC raffleRefAC
  spendsRaffleUserNFT <- txMustSpendFromAddress raffleUserAC ownAddrs
  let raffleUserNFTp = assetClassValue raffleUserAC 1
  raffleUserNFT <- valueFromPlutus' raffleUserNFTp
  isGettingRaffleUserNFT <- txIsPayingValueToAddress recipient raffleUserNFT
  return $
    mconcat
      [ isValidByCommitDDL,
        spendsRaffleRefNFT,
        spendsRaffleUserNFT,
        isRaffleStateUpdated,
        isGettingRaffleUserNFT
      ]

-- |  Cancel Transaction
cancelRaffleTX ::
  (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  [GYAddress] ->
  GYAddress ->
  AssetClass ->
  m (GYTxSkeleton 'PlutusV3)
cancelRaffleTX ownAddrs recipient raffleRefAC = do
  mpScriptRef <- asks mintingPolicyRef
  raffleScriptRef <- asks raffleValidatorRef
  (rsd, _rValue) <- getRaffleStateDataAndValue raffleRefAC
  isValidByCommitDDL <- txIsValidByDDL (rCommitDDL . rConfig $ rsd)
  let cancelRedeemer = RaffleOwner Cancel
  spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC cancelRedeemer raffleizeValidatorGY
  let raffleUserAC = deriveUserFromRefAC raffleRefAC
  spendsRaffleUserNFT <- txMustSpendFromAddress raffleUserAC ownAddrs
  isBurningRaffleUserAndRefNFT <- txNFTAction mpScriptRef Burn [raffleRefAC, raffleUserAC]
  stakeValue <- valueFromPlutus' $ rStake (rConfig rsd)
  isGettingStakeValue <- txIsPayingValueToAddress recipient stakeValue

  return $
    mconcat
      [ isValidByCommitDDL,
        spendsRaffleRefNFT,
        spendsRaffleUserNFT,
        isBurningRaffleUserAndRefNFT,
        isGettingStakeValue
      ]

-- | Collect Accumulated Amount Transaction
collectAmountTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
collectAmountTX = raffleOwnerClosingTX CollectAmount

-- | Recover Stake Transaction
recoverStakeTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
recoverStakeTX = raffleOwnerClosingTX RecoverStake

-- | Recover Stake And Accumulated Amount Transaction
recoverStakeAndAmountTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
recoverStakeAndAmountTX = raffleOwnerClosingTX RecoverStakeAndAmount

-- | Helper function to construct the raffle owner closing transaction based on the raffle owner action
raffleOwnerClosingTX ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  RaffleOwnerAction ->
  GYAddress ->
  AssetClass ->
  m (GYTxSkeleton 'PlutusV3)
raffleOwnerClosingTX roa ownAddr raffleRefAC
  | roa `elem` [CollectAmount, RecoverStake, RecoverStakeAndAmount] = do
      mpScriptRef <- asks mintingPolicyRef
      raffleScriptRef <- asks raffleValidatorRef
      let redeemerAction = RaffleOwner roa
      (rsd, rValue) <- getRaffleStateDataAndValue raffleRefAC
      let newValue = updateRaffleStateValue redeemerAction rsd rValue
      spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC redeemerAction raffleizeValidatorGY
      let raffleUserAC = deriveUserFromRefAC raffleRefAC
      isBurningRaffleUserNFT <- txNFTAction mpScriptRef Burn [raffleUserAC]
      isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum rsd) newValue
      diffValue <- valueFromPlutus' (rValue #- newValue)
      isGettingTheDifference <- txIsPayingValueToAddress ownAddr diffValue
      return $
        mconcat
          [ spendsRaffleRefNFT,
            isRaffleStateUpdated,
            isBurningRaffleUserNFT,
            isGettingTheDifference
          ]
raffleOwnerClosingTX _ _ _ = error "Invalid Raffle Owner Action"

-- | Get Collateral of Expired Ticket Transaction
getCollateralOfExpiredTicketTX :: (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => [GYAddress] -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
getCollateralOfExpiredTicketTX ownAddrs recipient ticketRefAC = do
  mpScriptRef <- asks mintingPolicyRef
  ticketScriptRef <- asks ticketValidatorRef
  (tsd, tValue) <- getTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  let raffleUserAC = deriveUserFromRefAC raffleRefAC
  spendsRaffleUserNFT <- txMustSpendFromAddress raffleUserAC ownAddrs
  hasRaffleStateAsReferenceInput <- txMustHaveStateAsRefInput raffleRefAC raffleizeValidatorGY
  spendsTicketRefNFT <-
    txMustSpendStateFromRefScriptWithRedeemer
      ticketScriptRef
      ticketRefAC
      (RaffleOwner GetCollateralOfExpiredTicket)
      ticketValidatorGY
  isBurningTicketRefNFT <- txNFTAction mpScriptRef Burn [ticketRefAC]
  ticketCollateralVal <- valueFromPlutus' (tValue #- assetClassValue ticketRefAC 1)
  isGettingCollateralValue <- txIsPayingValueToAddress recipient ticketCollateralVal
  return $
    mconcat
      [ spendsRaffleUserNFT,
        hasRaffleStateAsReferenceInput,
        spendsTicketRefNFT,
        isBurningTicketRefNFT,
        isGettingCollateralValue
      ]

------------------------------------------------------------------------------------------------

-- * Ticket Owner Actions

------------------------------------------------------------------------------------------------

-- | Reveal Ticket Transaction
revealTicketTX ::
  (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  Secret ->
  GYAddress ->
  AssetClass ->
  m (GYTxSkeleton 'PlutusV3)
revealTicketTX secret ownAddr ticketRefAC = do
  raffleScriptRef <- asks raffleValidatorRef
  ticketScriptRef <- asks ticketValidatorRef
  (tsd, tValue) <- getTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  (rsd, rValue) <- getRaffleStateDataAndValue raffleRefAC
  isValidByRevealDDL <- txIsValidByDDL (rRevealDDL . rConfig $ rsd)
  let revealRedeemer = TicketOwnerRedeemer (RevealTicketSecret secret) ticketRefAC
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
        [ isValidByRevealDDL,
          spendsRaffleRefNFT,
          isRaffleStateUpdated,
          spendsTicketRefNFT,
          isTicketStateUpdated,
          isGettingTicketUserNFT
        ]
    )

-- | Winner Collect Stake Transaction
winnerCollectStakeTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
winnerCollectStakeTX = ticketOwnerClosingTX CollectStake

-- | Full Refund Ticket Transaction
fullRefundTicketTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
fullRefundTicketTX = ticketOwnerClosingTX RefundTicket

-- | Extra Refund Ticket Transaction
extraRefundTicketTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
extraRefundTicketTX = ticketOwnerClosingTX RefundTicketExtra

-- | Helper function to construct the ticket owner closing transaction based on the ticket owner action
ticketOwnerClosingTX :: (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) => TicketOwnerAction -> GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
ticketOwnerClosingTX toa ownAddr ticketRefAC = do
  raffleScriptRef <- asks raffleValidatorRef
  ticketScriptRef <- asks ticketValidatorRef
  mpScriptRef <- asks mintingPolicyRef
  if toa `elem` [RefundTicket, CollectStake, RefundTicketExtra]
    then do
      let redeemerAction = TicketOwnerRedeemer toa ticketRefAC
      (tsd, _tValue) <- getTicketStateDataAndValue ticketRefAC
      let raffleRefAC = tRaffle tsd
      (rsd, rValue) <- getRaffleStateDataAndValue raffleRefAC
      spendsRaffleRefNFT <- txMustSpendStateFromRefScriptWithRedeemer raffleScriptRef raffleRefAC redeemerAction raffleizeValidatorGY
      spendsTicketRefNFT <- txMustSpendStateFromRefScriptWithRedeemer ticketScriptRef ticketRefAC redeemerAction ticketValidatorGY
      let newRaffleValue = updateRaffleStateValue (redeemerToAction redeemerAction) rsd rValue
      let new_rsd = if toa `elem` [RefundTicket, RefundTicketExtra] then rsd {rRefundedTickets = rRefundedTickets rsd #+ 1} else rsd
      isRaffleStateUpdated <- txMustLockStateWithInlineDatumAndValue raffleizeValidatorGY (mkRaffleDatum new_rsd) newRaffleValue
      let ticketUserAC = deriveUserFromRefAC ticketRefAC
      isBurningTicketUserAndRefNFT <- txNFTAction mpScriptRef Burn [ticketRefAC, ticketUserAC]
      diffValue <- valueFromPlutus' (if toa == CollectStake then rValue #- newRaffleValue else rValue #- newRaffleValue #+ ticketCollateralValue rsd)
      isGettingStakeAndTicketCollateral <- txIsPayingValueToAddress ownAddr diffValue
      return
        ( mconcat
            [ spendsRaffleRefNFT,
              spendsTicketRefNFT,
              isRaffleStateUpdated,
              isBurningTicketUserAndRefNFT,
              isGettingStakeAndTicketCollateral
            ]
        )
    else error ("Invalid Ticket Owner Action : " <> show toa)

-- | Refund Collateral of Losing Ticket Transaction
refundCollateralOfLosingTicketTX ::
  (GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  GYAddress ->
  AssetClass ->
  m (GYTxSkeleton 'PlutusV3)
refundCollateralOfLosingTicketTX ownAddr ticketRefAC = do
  mpScriptRef <- asks mintingPolicyRef
  ticketScriptRef <- asks ticketValidatorRef
  (tsd, tValue) <- getTicketStateDataAndValue ticketRefAC
  let raffleRefAC = tRaffle tsd
  let ticketUserAC = deriveUserFromRefAC ticketRefAC
  hasRaffleStateAsReferenceInput <- txMustHaveStateAsRefInput raffleRefAC raffleizeValidatorGY
  spendsTicketRefNFT <-
    txMustSpendStateFromRefScriptWithRedeemer
      ticketScriptRef
      ticketRefAC
      (TicketOwnerRedeemer RefundCollateralLosing ticketRefAC)
      ticketValidatorGY
  isBurningTicketUserAndRefNFT <- txNFTAction mpScriptRef Burn [ticketRefAC, ticketUserAC]

  ticketCollateralVal <- valueFromPlutus' (tValue #- assetClassValue ticketRefAC 1)
  isGettingCollateralValue <- txIsPayingValueToAddress ownAddr ticketCollateralVal
  return $
    mconcat
      [ hasRaffleStateAsReferenceInput,
        spendsTicketRefNFT,
        isBurningTicketUserAndRefNFT,
        isGettingCollateralValue
      ]

------------------------------------------------------------------------------------------------

-- * Admin  Actions

------------------------------------------------------------------------------------------------
