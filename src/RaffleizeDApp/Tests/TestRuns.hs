module RaffleizeDApp.Tests.TestRuns where

import Cardano.Simple.Ledger.Slot
import Cardano.Simple.Ledger.TimeSlot
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe qualified
import Data.Tuple.Extra (uncurry3)
import GHC.Stack
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Plutus.Model hiding (User)
import PlutusLedgerApi.V1.Interval
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3 (POSIXTimeRange)
import PlutusTx.Builtins (blake2b_256)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes (TicketStateData (tSecret, tSecretHash))
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Validators
import RaffleizeDApp.Utils

----------------------
-- Run TEST ACTIONS
-----------------------

raffleizeTransactionRun :: HasCallStack => Wallet -> RaffleizeTxBuildingContext -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> Run (GYTxId, AssetClass)
raffleizeTransactionRun w roc raffleizeActon interactionContextNFT optionalRecipient = do
  my_addr <- runWallet' w ownAddress
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
  result <- runReaderT (interactionToTxSkeleton raffleizeInteraction) roc
  (skeleton, ac) <- runWallet' w result
  txId <- runWallet' w $ sendSkeleton skeleton `catchError` (error . show)
  logInfo' ("Performed Raffleize Action:\n" <> show raffleizeActon)
  return (txId, ac)

raffleizeTransactionThatMustFailRun :: HasCallStack => Wallet -> RaffleizeTxBuildingContext -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> Run ()
raffleizeTransactionThatMustFailRun w roc raffleizeActon interactionContextNFT optionalRecipient = do
  my_addr <- runWallet' w ownAddress
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
  result <- runReaderT (interactionToTxSkeleton raffleizeInteraction) roc
  (skeleton, _ac) <- runWallet' w result
  mustFail $ runWallet w $ sendSkeleton skeleton
  return ()

deployReferenceScriptRun :: GYValidator 'PlutusV2 -> Wallet -> GYAddress -> Run GYTxOutRef
deployReferenceScriptRun validator fromWallet toWallet = do
  valRef <- runWallet' fromWallet $ addRefScript toWallet validator `catchError` (error . show)
  logInfo' $ "DEPLOYED VALIDATOR:\n" <> show validator
  case valRef of
    Nothing -> error "failed to add the reference script"
    Just gyTxOutRef -> return gyTxOutRef

deployValidatorsRun :: Wallet -> Run RaffleizeTxBuildingContext
deployValidatorsRun w = do
  refTicketValidator <- deployReferenceScriptRun ticketValidatorGY w (walletAddress w)
  refRaffleValidator <- deployReferenceScriptRun raffleizeValidatorGY w (walletAddress w)
  return RaffleizeTxBuildingContext {raffleValidatorRef = refRaffleValidator, ticketValidatorRef = refTicketValidator}

queryRaffleRun :: HasCallStack => Wallet -> AssetClass -> Run (Maybe RaffleInfo)
queryRaffleRun w rid =
  runWallet' w $ lookupRaffleInfoRefAC rid

queryTicketRun :: HasCallStack => Wallet -> AssetClass -> Run (Maybe TicketInfo)
queryTicketRun w tid =
  runWallet' w $ lookupTicketInfoByRefAC tid

getTimeRangeForNextNSlots :: Integer -> Run POSIXTimeRange
getTimeRangeForNextNSlots i = do
  mock <- get
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let now = mockCurrentSlot mock
  let lower = slotToEndPOSIXTime sltCfg now
  let upper = slotToEndPOSIXTime sltCfg (now + Slot i)
  return $ intersection (from lower) (to upper)

queryRaffleRUN :: HasCallStack => Bool -> Wallet -> AssetClass -> Run RaffleStateId
queryRaffleRUN log w rid = do
  (r, v) <- runWallet' w $ do
    getRaffleStateDataAndValue rid `catchError` (error . show)
  tr <- getTimeRangeForNextNSlots 0
  let rStateId = evaluateRaffleState (tr, r, v)
  when log $ do
    logInfo (yellowColorString $ "The raffle is in state : " ++ showRaffleStateLabel rStateId)
    logInfo $ yellowColorString $ show r ++ showValue "Raffle State Value" v
  return rStateId

queryTicketRUN :: Wallet -> AssetClass -> Run ()
queryTicketRUN w tid = do
  (r, v) <- runWallet' w $ do
    getTicketStateDataAndValue tid `catchError` (error . show)
  logInfo $ blueColorString $ show r ++ showValue "Ticket State Value" v

deployValidatorsAndCreateNewRaffleRun :: Wallets -> RaffleConfig -> Run (RaffleInfo, RaffleizeTxBuildingContext)
deployValidatorsAndCreateNewRaffleRun Wallets {..} config = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9
  -- . Create raffle
  (_txId, raffleId) <- raffleizeTransactionRun w1 roc (User (CreateRaffle config)) Nothing Nothing
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> error "raffle was not created"
    Just ri -> do
      when (riStateLabel ri /= "NEW") $ logError "not in status NEW"
      return (ri, roc)

deployValidatorsAndCreateNewValidRaffleRun :: Wallets -> Run (RaffleInfo, RaffleizeTxBuildingContext)
deployValidatorsAndCreateNewValidRaffleRun wallets = do
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let cddl = slotToEndPOSIXTime sltCfg 20
  let rddl = slotToEndPOSIXTime sltCfg 50
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 3
          , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876) 
          }
  deployValidatorsAndCreateNewRaffleRun wallets config

buyTicketToRaffleRun :: RaffleInfo -> RaffleizeTxBuildingContext -> Wallet -> BuiltinByteString -> Run TicketInfo
buyTicketToRaffleRun ri roc w secret = do
  let raffleId = rRaffleID $ riRsd ri
  let secretHash = blake2b_256 secret
  soldTicketsBeforeBuy <- rSoldTickets . riRsd . fromJust <$> queryRaffleRun w raffleId
  (_txId, ticketId) <- raffleizeTransactionRun w roc (User (BuyTicket secretHash)) (Just raffleId) Nothing
  mri2 <- queryRaffleRun w raffleId
  case mri2 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri2 -> do
      unless (riStateLabel ri2 == "COMMITTING") $ logError "not in status COMMITTING"
      unless (soldTicketsBeforeBuy + 1 == rSoldTickets (riRsd ri2)) $ logError "no. of tickets sold was not updated"
  mti <- queryTicketRun w ticketId
  case mti of
    Nothing -> error $ "Ticket ref not found: " <> show ticketId
    Just ti -> do
      unless (tiStateLabel ti == "COMMITTED") $ logError "not in status COMMITTED"
      unless (tSecretHash (tiTsd ti) == secretHash) $ logError "invalid onchain secret hash"
      unless (Data.Maybe.isNothing (tSecret (tiTsd ti))) $ logError "secret must not be revealed"
      return ti

buyNTicketsToRaffleRun :: RaffleInfo -> RaffleizeTxBuildingContext -> [(Wallet, BuiltinByteString)] -> Run [TicketInfo]
buyNTicketsToRaffleRun ri roc = mapM (uncurry (buyTicketToRaffleRun ri roc))

revealTicketSecretRun :: RaffleInfo -> RaffleizeTxBuildingContext -> Wallet -> AssetClass -> BuiltinByteString -> Run TicketInfo
revealTicketSecretRun ri roc w ticketId secret = do
  unless (riStateLabel ri == "REVEALING") $ logError "not in status REVEALING"
  let raffleId = rRaffleID $ riRsd ri
  revealdTIcketsBefore <- rRevealedTickets . riRsd . fromJust <$> queryRaffleRun w raffleId
  (_txId, ticketId2) <- raffleizeTransactionRun w roc (TicketOwner (RevealTicketSecret secret)) (Just ticketId) Nothing
  mri2 <- queryRaffleRun w raffleId
  case mri2 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri2 -> do
      unless (revealdTIcketsBefore + 1 == rRevealedTickets (riRsd ri2)) $ logError "no. of tickets revealed was not updated"

  mti <- queryTicketRun w ticketId2
  case mti of
    Nothing -> error $ "Ticket ref not found: " <> show ticketId2
    Just ti -> do
      unless (tSecret (tiTsd ti) == Just secret) $ logError "invalid onchain secret"
      return ti

reavealNTicketsRun :: RaffleInfo -> RaffleizeTxBuildingContext -> [(Wallet, AssetClass, BuiltinByteString)] -> Run [TicketInfo]
reavealNTicketsRun ri roc = mapM (uncurry3 (revealTicketSecretRun ri roc))

------------------------------------------------------------------------------------------------

logInfo' :: String -> Run ()
logInfo' s = logInfo $ greenColorString s
