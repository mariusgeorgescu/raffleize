module TestRuns where

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..))
import GeniusYield.Test.Clb (sendSkeleton')
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (AssetClass)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions (interactionToTxSkeleton)
import RaffleizeDApp.TxBuilding.Validators (raffleizeValidatorGY, ticketValidatorGY)
import RaffleizeDApp.Utils
import RaffleizeDApp.CustomTypes.RaffleTypes
    ( RaffleConfig, RaffleConfig(..) )
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.CustomTypes.TransferTypes
import Control.Monad.Extra (when)

-- -- import Cardano.Simple.Ledger.Slot
-- -- import Cardano.Simple.Ledger.TimeSlot
-- -- import Plutus.Model hiding (User)
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Data.Maybe qualified
-- import Data.Tuple.Extra (uncurry3)
-- import GHC.Stack
-- import GeniusYield.Test.Utils
-- import GeniusYield.TxBuilder
-- import GeniusYield.Types
-- import PlutusLedgerApi.V1.Interval
-- import PlutusLedgerApi.V1.Value
-- import PlutusLedgerApi.V3 (POSIXTimeRange)
-- import PlutusTx.Builtins (blake2b_256)
-- import RaffleizeDApp.CustomTypes.ActionTypes
-- import RaffleizeDApp.CustomTypes.RaffleTypes
-- import RaffleizeDApp.CustomTypes.TicketTypes
-- import RaffleizeDApp.CustomTypes.TransferTypes
-- import RaffleizeDApp.OnChain.RaffleizeLogic
-- import RaffleizeDApp.OnChain.Utils
-- import RaffleizeDApp.TxBuilding.Context
-- import RaffleizeDApp.TxBuilding.Interactions
-- import RaffleizeDApp.TxBuilding.Lookups
-- import RaffleizeDApp.TxBuilding.Validators
-- import RaffleizeDApp.Utils

-- ----------------------
-- -- Run TEST ACTIONS
-- -----------------------

raffleizeTransactionRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => User -> RaffleizeTxBuildingContext -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> m (GYTxId, AssetClass)
raffleizeTransactionRun w rtxbc raffleizeActon interactionContextNFT optionalRecipient = do
  my_addr <- ownChangeAddress
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
  result <- runReaderT (interactionToTxSkeleton raffleizeInteraction) rtxbc
  (skeleton, ac) <- result
  (_, txId) <- withWalletBalancesCheck [] $ asUser w $ sendSkeleton' skeleton `catchError` (error . show)
  return (txId, ac)

deployReferenceScriptRun :: (GYTxGameMonad m) => GYValidator PlutusV2 -> User -> User -> m GYTxOutRef
deployReferenceScriptRun validator fromWallet toWallet = do
  withWalletBalancesCheck [] $ asUser fromWallet $ addRefScript (userChangeAddress toWallet) (validatorToScript validator)

deployValidatorsRun :: (GYTxGameMonad m) => User -> m RaffleizeTxBuildingContext
deployValidatorsRun w = do
  refTicketValidator <- deployReferenceScriptRun ticketValidatorGY w w
  refRaffleValidator <- deployReferenceScriptRun raffleizeValidatorGY w w
  return RaffleizeTxBuildingContext {raffleValidatorRef = refRaffleValidator, ticketValidatorRef = refTicketValidator}


queryRaffleRun :: GYTxGameMonad m => User -> AssetClass -> m (Maybe RaffleInfo)
queryRaffleRun w rid =
  withWalletBalancesCheck [] $ asUser w $ lookupRaffleInfoRefAC rid


deployValidatorsAndCreateNewRaffleRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Wallets -> RaffleConfig -> m (RaffleInfo, RaffleizeTxBuildingContext)
deployValidatorsAndCreateNewRaffleRun Wallets {..} config = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9
  waitNSlots_ 3
  void slotOfCurrentBlock
  -- . Create raffle
  (_txId, raffleId) <- raffleizeTransactionRun w1 roc (RaffleizeDApp.CustomTypes.ActionTypes.User (CreateRaffle config)) Nothing Nothing
  waitNSlots_ 3
  void slotOfCurrentBlock
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> error "raffle was not created"
    Just ri -> do
      when (riStateLabel ri /= "NEW") $ logMsg "TestError"  GYError "not in status NEW"
      return (ri, roc)


-- raffleizeTransactionThatMustFailRun :: HasCallStack => Wallet -> RaffleizeTxBuildingContext -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> Run ()
-- raffleizeTransactionThatMustFailRun w roc raffleizeActon interactionContextNFT optionalRecipient = do
--   my_addr <- runWallet' w ownAddress
--   let userAddrs = UserAddresses [my_addr] my_addr Nothing
--   let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
--   result <- runReaderT (interactionToTxSkeleton raffleizeInteraction) roc
--   (skeleton, _ac) <- runWallet' w result
--   mustFail $ runWallet w $ sendSkeleton skeleton
--   return ()



-- queryTicketRun :: HasCallStack => Wallet -> AssetClass -> Run (Maybe TicketInfo)
-- queryTicketRun w tid =
--   runWallet' w $ lookupTicketInfoByRefAC tid

-- getTimeRangeForNextNSlots :: Integer -> Run POSIXTimeRange
-- getTimeRangeForNextNSlots i = do
--   mock <- get
--   sltCfg <- gets (mockConfigSlotConfig . mockConfig)
--   let now = mockCurrentSlot mock
--   let lower = slotToEndPOSIXTime sltCfg now
--   let upper = slotToEndPOSIXTime sltCfg (now + Slot i)
--   return $ intersection (from lower) (to upper)

-- queryRaffleRUN :: HasCallStack => Bool -> Wallet -> AssetClass -> Run RaffleStateId
-- queryRaffleRUN log w rid = do
--   (r, v) <- runWallet' w $ do
--     getRaffleStateDataAndValue rid `catchError` (error . show)
--   tr <- getTimeRangeForNextNSlots 0
--   let rStateId = evaluateRaffleState (tr, r, v)
--   when log $ do
--     logInfo (yellowColorString $ "The raffle is in state : " ++ showRaffleStateLabel rStateId)
--     logInfo $ yellowColorString $ show r ++ showValue "Raffle State Value" v
--   return rStateId

-- queryTicketRUN :: Wallet -> AssetClass -> Run ()
-- queryTicketRUN w tid = do
--   (r, v) <- runWallet' w $ do
--     getTicketStateDataAndValue tid `catchError` (error . show)
--   logInfo $ blueColorString $ show r ++ showValue "Ticket State Value" v



-- deployValidatorsAndCreateNewValidRaffleRun :: (GYTxGameMonad m, GYTxUserQueryMonad m) => Wallets -> m (RaffleInfo, RaffleizeTxBuildingContext)
-- deployValidatorsAndCreateNewValidRaffleRun wallets = do
--   -- currrentSlot <- slotOfCurrentBlock


-- buyTicketToRaffleRun :: RaffleInfo -> RaffleizeTxBuildingContext -> Wallet -> BuiltinByteString -> Run TicketInfo
-- buyTicketToRaffleRun ri roc w secret = do
--   let raffleId = rRaffleID $ riRsd ri
--   let secretHash = blake2b_256 secret
--   soldTicketsBeforeBuy <- rSoldTickets . riRsd . fromJust <$> queryRaffleRun w raffleId
--   (_txId, ticketId) <- raffleizeTransactionRun w roc (User (BuyTicket secretHash)) (Just raffleId) Nothing
--   ri2 <- fromMaybe (error "raffle not fund") <$> queryRaffleRun w raffleId
--   unless (riStateLabel ri2 == "COMMITTING") $ logError "not in status COMMITTING"
--   unless (soldTicketsBeforeBuy + 1 == rSoldTickets (riRsd ri2)) $ logError "no. of tickets sold was not updated"
--   ti <- fromMaybe (error "raffle not fund") <$> queryTicketRun w ticketId
--   unless (tiStateLabel ti == "COMMITTED") $ logError "not in status COMMITTED"
--   unless (tSecretHash (tiTsd ti) == secretHash) $ logError "invalid onchain secret hash"
--   unless (Data.Maybe.isNothing (tSecret (tiTsd ti))) $ logError "secret must not be revealed"
--   return ti

-- buyNTicketsToRaffleRun :: RaffleInfo -> RaffleizeTxBuildingContext -> [(Wallet, BuiltinByteString)] -> Run [TicketInfo]
-- buyNTicketsToRaffleRun ri roc = mapM (uncurry (buyTicketToRaffleRun ri roc))

-- revealTicketSecretRun :: RaffleInfo -> RaffleizeTxBuildingContext -> Wallet -> AssetClass -> BuiltinByteString -> Run TicketInfo
-- revealTicketSecretRun ri roc w ticketId secret = do
--   unless (riStateLabel ri == "REVEALING") $ logError "not in status REVEALING"
--   let raffleId = rRaffleID $ riRsd ri
--   revealdTIcketsBefore <- rRevealedTickets . riRsd . fromJust <$> queryRaffleRun w raffleId
--   (_txId, ticketId2) <- raffleizeTransactionRun w roc (TicketOwner (RevealTicketSecret secret)) (Just ticketId) Nothing
--   ri2 <- fromMaybe (error "raffle not fund") <$> queryRaffleRun w raffleId
--   unless (revealdTIcketsBefore + 1 == rRevealedTickets (riRsd ri2)) $ logError "no. of tickets revealed was not updated"
--   ti <- fromMaybe (error "ticket not fund") <$> queryTicketRun w ticketId2
--   unless (tSecret (tiTsd ti) == Just secret) $ logError "invalid onchain secret"
--   return ti

-- reavealNTicketsRun :: RaffleInfo -> RaffleizeTxBuildingContext -> [(Wallet, AssetClass, BuiltinByteString)] -> Run [TicketInfo]
-- reavealNTicketsRun ri roc = mapM (uncurry3 (revealTicketSecretRun ri roc))

-- refundTicketSecretRun :: Bool -> RaffleInfo -> RaffleizeTxBuildingContext -> Wallet -> AssetClass -> Run TicketInfo
-- refundTicketSecretRun isExtra ri roc w ticketId = do
--   if isExtra
--     then unless (riStateLabel ri `elem` ["UNREVEALED_LOCKED_STAKE_AND_REFUNDS", "UNREVEALED_LOCKED_REFUNDS"]) $ logError "not in status UNREVEALED"
--     else unless (riStateLabel ri `elem` ["UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS", "UNDERFUNDED_LOCKED_REFUNDS"]) $ logError "not in status UNDERFUNDED"
--   let raffleId = rRaffleID $ riRsd ri
--   refundedTIcketsBefore <- rRefundedTickets . riRsd . fromJust <$> queryRaffleRun w raffleId
--   (_txId, ticketId2) <- raffleizeTransactionRun w roc (TicketOwner (if isExtra then RefundTicketExtra else RefundTicket)) (Just ticketId) Nothing
--   ri2 <- fromMaybe (error "raffle not fund") <$> queryRaffleRun w raffleId
--   unless (refundedTIcketsBefore + 1 == rRefundedTickets (riRsd ri2)) $ logError "no. of tickets refunded was not updated"
--   fromMaybe (error "ticket not fund") <$> queryTicketRun w ticketId2

-- refundNTicketsRun :: Bool -> RaffleInfo -> RaffleizeTxBuildingContext -> [(Wallet, AssetClass)] -> Run [TicketInfo]
-- refundNTicketsRun isExtra ri roc = mapM (uncurry (refundTicketSecretRun isExtra ri roc))

-- ------------------------------------------------------------------------------------------------

printLogInfo :: (MonadIO m) => String -> m ()
printLogInfo s = liftIO $ putStrLn $ greenColorString s
