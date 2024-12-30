module TestRuns where

import Control.Monad (unless)
import Control.Monad.Extra (when)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..))
import Data.Maybe qualified
import Data.Tuple.Extra (uncurry3)
import GHC.Stack
import GeniusYield.Test.Clb (GYTxMonadClb, mustFail, sendSkeleton')
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (AssetClass)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions (interactionToTxSkeleton)
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Utils (pPOSIXTimeFromSlotInteger)
import RaffleizeDApp.TxBuilding.Validators (raffleizeValidatorGY, ticketValidatorGY)
import RaffleizeDApp.Utils

-- ----------------------
-- -- Run TEST ACTIONS
-- -----------------------

raffleizeTransactionRun :: (GYTxGameMonad m, GYTxUserQueryMonad m, HasCallStack) => User -> RaffleizeTxBuildingContext -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> m (GYTxId, AssetClass)
raffleizeTransactionRun w rtxbc raffleizeActon interactionContextNFT optionalRecipient = do
  let userAddrs = UserAddresses (toList $ GeniusYield.TxBuilder.userAddresses w) (userChangeAddress w) Nothing
  let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
  result <- runReaderT (interactionToTxSkeleton raffleizeInteraction) rtxbc
  (skeleton, ac) <- result
  (_, txId) <- withWalletBalancesCheck [] $ asUser w $ sendSkeleton' skeleton
  return (txId, ac)

raffleizeTransactionThatMustFailRun :: (HasCallStack) => User -> RaffleizeTxBuildingContext -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> GYTxMonadClb ()
raffleizeTransactionThatMustFailRun w roc raffleizeActon interactionContextNFT optionalRecipient =
  mustFail $ raffleizeTransactionRun w roc raffleizeActon interactionContextNFT optionalRecipient

deployReferenceScriptRun :: (GYTxGameMonad m) => GYScript PlutusV2 -> User -> User -> m GYTxOutRef
deployReferenceScriptRun validator fromWallet toWallet = do
  withWalletBalancesCheck [] $ asUser fromWallet $ addRefScript (userChangeAddress toWallet) (validatorToScript validator)

deployValidatorsRun :: (GYTxGameMonad m) => User -> m RaffleizeTxBuildingContext
deployValidatorsRun w = do
  refTicketValidator <- deployReferenceScriptRun ticketValidatorGY w w
  refRaffleValidator <- deployReferenceScriptRun raffleizeValidatorGY w w
  return RaffleizeTxBuildingContext {raffleValidatorRef = refRaffleValidator, ticketValidatorRef = refTicketValidator}

queryRaffleRun :: (GYTxGameMonad m, HasCallStack) => User -> AssetClass -> m (Maybe RaffleInfo)
queryRaffleRun w rid =
  withWalletBalancesCheck [] $ asUser w $ lookupRaffleInfoByRefAC rid

deployValidatorsAndCreateNewRaffleRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Wallets -> RaffleConfig -> m (RaffleInfo, RaffleizeTxBuildingContext)
deployValidatorsAndCreateNewRaffleRun Wallets {..} config = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9
  waitNSlots_ 1
  void slotOfCurrentBlock
  -- . Create raffle
  (_txId, raffleId) <- raffleizeTransactionRun w1 roc (RaffleizeDApp.CustomTypes.ActionTypes.User (CreateRaffle config)) Nothing Nothing
  waitNSlots_ 1
  void slotOfCurrentBlock
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> error "raffle was not created"
    Just ri -> do
      when (riStateLabel ri /= "NEW") $ logTestError "not in status NEW"
      return (ri, roc)

deployValidatorsAndCreateNewValidRaffleRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Wallets -> m (RaffleInfo, RaffleizeTxBuildingContext)
deployValidatorsAndCreateNewValidRaffleRun testWallets = do
  cddl <- pPOSIXTimeFromSlotInteger 100
  rddl <- pPOSIXTimeFromSlotInteger 200
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 4
          , rStake = valueToPlutus (fakeValue fakeIron 9876) <> valueToPlutus (fakeValue fakeGold 9876)
          }
  deployValidatorsAndCreateNewRaffleRun testWallets config

queryTicketRun :: (GYTxGameMonad m) => User -> AssetClass -> m (Maybe TicketInfo)
queryTicketRun w tid =
  withWalletBalancesCheck [] $ asUser w $ lookupTicketInfoByRefAC tid

buyTicketToRaffleRun :: (GYTxGameMonad m, GYTxUserQueryMonad m, HasCallStack) => RaffleInfo -> RaffleizeTxBuildingContext -> User -> BuiltinByteString -> m TicketInfo
buyTicketToRaffleRun ri roc w secret = do
  let raffleId = rRaffleID $ riRsd ri
  let secretHash = blake2b_256 secret
  soldTicketsBeforeBuy <- rSoldTickets . riRsd . Data.Maybe.fromMaybe (error "raffle not fund 1") <$> queryRaffleRun w raffleId
  x <- Data.Maybe.fromMaybe (error "raffle not fund X") <$> queryRaffleRun w raffleId
  logInfo (show x)
  (_txId, ticketId) <- raffleizeTransactionRun w roc (RaffleizeDApp.CustomTypes.ActionTypes.User (BuyTicket secretHash)) (Just raffleId) Nothing
  ri2 <- Data.Maybe.fromMaybe (error "raffle not fund 2") <$> queryRaffleRun w raffleId
  unless (riStateLabel ri2 == "COMMITTING") $ logTestError "not in status COMMITTING"
  unless (soldTicketsBeforeBuy + 1 == rSoldTickets (riRsd ri2)) $ logTestError "no. of tickets sold was not updated"
  ti <- Data.Maybe.fromMaybe (error "raffle not fund") <$> queryTicketRun w ticketId
  unless (tiStateLabel ti == "COMMITTED") $ logTestError "not in status COMMITTED"
  unless (tSecretHash (tiTsd ti) == secretHash) $ logTestError "invalid onchain secret hash"
  unless (Data.Maybe.isNothing (tSecret (tiTsd ti))) $ logTestError "secret must not be revealed"
  return ti

buyNTicketsToRaffleRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => RaffleInfo -> RaffleizeTxBuildingContext -> [(User, BuiltinByteString)] -> m [TicketInfo]
buyNTicketsToRaffleRun ri roc = mapM (uncurry (buyTicketToRaffleRun ri roc))

revealTicketSecretRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => RaffleInfo -> RaffleizeTxBuildingContext -> User -> AssetClass -> BuiltinByteString -> m TicketInfo
revealTicketSecretRun ri roc w ticketId secret = do
  unless (riStateLabel ri == "REVEALING") $ logTestError "not in status REVEALING"
  let raffleId = rRaffleID $ riRsd ri
  revealdTIcketsBefore <- rRevealedTickets . riRsd . Data.Maybe.fromJust <$> queryRaffleRun w raffleId
  (_txId, ticketId2) <- raffleizeTransactionRun w roc (TicketOwner (RevealTicketSecret secret)) (Just ticketId) Nothing
  ri2 <- Data.Maybe.fromMaybe (error "raffle not fund") <$> queryRaffleRun w raffleId
  unless (revealdTIcketsBefore + 1 == rRevealedTickets (riRsd ri2)) $ logTestError "no. of tickets revealed was not updated"
  ti <- Data.Maybe.fromMaybe (error "ticket not fund") <$> queryTicketRun w ticketId2
  unless (tSecret (tiTsd ti) == Just secret) $ logTestError "invalid onchain secret"
  return ti

reavealNTicketsRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => RaffleInfo -> RaffleizeTxBuildingContext -> [(User, AssetClass, BuiltinByteString)] -> m [TicketInfo]
reavealNTicketsRun ri roc = mapM (uncurry3 (revealTicketSecretRun ri roc))

refundTicketSecretRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Bool -> RaffleInfo -> RaffleizeTxBuildingContext -> User -> AssetClass -> m TicketInfo
refundTicketSecretRun isExtra ri roc w ticketId = do
  if isExtra
    then unless (riStateLabel ri `elem` ["UNREVEALED_LOCKED_STAKE_AND_REFUNDS", "UNREVEALED_LOCKED_REFUNDS"]) $ logTestError "not in status UNREVEALED"
    else unless (riStateLabel ri `elem` ["UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS", "UNDERFUNDED_LOCKED_REFUNDS"]) $ logTestError "not in status UNDERFUNDED"
  let raffleId = rRaffleID $ riRsd ri
  refundedTIcketsBefore <- rRefundedTickets . riRsd . Data.Maybe.fromJust <$> queryRaffleRun w raffleId
  (_txId, ticketId2) <- raffleizeTransactionRun w roc (TicketOwner (if isExtra then RefundTicketExtra else RefundTicket)) (Just ticketId) Nothing
  ri2 <- Data.Maybe.fromMaybe (error "raffle not fund") <$> queryRaffleRun w raffleId
  unless (refundedTIcketsBefore + 1 == rRefundedTickets (riRsd ri2)) $ logTestError "no. of tickets refunded was not updated"
  Data.Maybe.fromMaybe (error "ticket not fund") <$> queryTicketRun w ticketId2

refundNTicketsRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Bool -> RaffleInfo -> RaffleizeTxBuildingContext -> [(User, AssetClass)] -> m [TicketInfo]
refundNTicketsRun isExtra ri roc = mapM (uncurry (refundTicketSecretRun isExtra ri roc))

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------

printLogInfo :: (MonadIO m) => String -> m ()
printLogInfo s = liftIO $ putStrLn $ greenColorString s

logTestError :: (GYTxGameMonad m, GYTxUserQueryMonad m, HasCallStack) => String -> m ()
logTestError = logMsg "TestError" GYError

logInfo :: (GYTxGameMonad m, GYTxUserQueryMonad m, HasCallStack) => String -> m ()
logInfo = logMsg "Info" GYInfo

-- getTimeRangeForNextNSlots :: (GYTxGameMonad m) => Integer -> m POSIXTimeRange
-- getTimeRangeForNextNSlots i = do
--   now <- slotOfCurrentBlock
--   let upperSlot = unsafeAdvanceSlot now (fromInteger i)
--   lower <- timeToPlutus <$> slotToBeginTime now
--   upper <- timeToPlutus <$> slotToEndTime upperSlot
--   return $ intersection (from lower) (to upper)

-- queryTicketRUN :: (GYTxGameMonad m, GYTxUserQueryMonad m) => User -> AssetClass -> m ()
-- queryTicketRUN w tid = do
--   (r, v) <- withWalletBalancesCheck [] $ asUser w $ do
--     getTicketStateDataAndValue tid `catchError` (error . show)
--   logInfo $ blueColorString $ show r ++ showValue "Ticket State Value" v

-- queryRaffleRUN :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Bool -> User -> AssetClass -> m RaffleStateId
-- queryRaffleRUN log w rid = do
--   (r, v) <- withWalletBalancesCheck [] $ asUser w $ do
--     getRaffleStateDataAndValue rid `catchError` (error . show)
--   tr <- getTimeRangeForNextNSlots 1
--   let rStateId = evaluateRaffleState (tr, r, v)
--   when log $ do
--     logInfo (yellowColorString $ "The raffle is in state : " ++ showRaffleStateLabel rStateId)
--     logInfo $ yellowColorString $ show r ++ showValue "Raffle State Value" v
--   return rStateId
