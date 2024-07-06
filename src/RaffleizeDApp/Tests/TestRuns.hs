module RaffleizeDApp.Tests.TestRuns where

import Cardano.Simple.Ledger.Slot
import Cardano.Simple.Ledger.TimeSlot
import Control.Monad.Reader
import Control.Monad.State
import GHC.Stack
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Plutus.Model hiding (User)
import PlutusLedgerApi.V1.Interval
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3 (POSIXTimeRange)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
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
  logInfo' $ "DEPLOYED VALIDATOR" <> show validator
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

deployValidatorsAndCreateNewValidRaffleRun :: Wallets -> Run (RaffleInfo, RaffleizeTxBuildingContext)
deployValidatorsAndCreateNewValidRaffleRun Wallets {..} = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9

  -- . Create raffle
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
  (_txId, raffleId) <- raffleizeTransactionRun w1 roc (User (CreateRaffle config)) Nothing Nothing
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> error "raffle was not created"
    Just ri -> do
      when (riStateLabel ri /= "NEW") $ logError "not in status NEW"
      return (ri, roc)

-- revealNTicketsRUN :: GYTxOutRef -> GYTxOutRef -> AssetClass -> [(AssetClass, (Wallet, Secret))] -> Run ()
-- revealNTicketsRUN refRaffleValidator refTicketValidator raffleId ws = do
--   mapM_ (revealTicketRUN refRaffleValidator refTicketValidator) ws
--   s <- queryRaffleRUN True (head (fst . snd <$> ws)) raffleId
--   when (s /= 3 && s /= 40) $ logError "not in REVEALING or SUCCESS_LOCKED_STAKE_AND_AMOUNT"

------------------------------------------------------------------------------------------------

logInfo' :: String -> Run ()
logInfo' s = logInfo $ greenColorString s
