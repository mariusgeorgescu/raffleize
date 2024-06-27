module RaffleizeDApp.Tests.UnitTests where

import Cardano.Simple.Ledger.TimeSlot
import Control.Monad.State.Class (gets)
import GeniusYield.Test.Utils
import GeniusYield.Types
import Plutus.Model (logError, mockConfig, mockConfigSlotConfig, waitNSlots)
import PlutusTx.Builtins (blake2b_256)
import RaffleizeDApp.CustomTypes.RaffleTypes

import Control.Monad
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.TransferTypes (RaffleInfo (..))
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.Tests.TestRuns
import Test.Tasty

unitTests :: TestTree
unitTests = testGroup "Raffleize Unit Tests" [createRaffleTests, otherTests]

-- | Our unit tests for creating a raffle
createRaffleTests :: TestTree
createRaffleTests =
  testGroup
    "CREATE RAFFLE TEST CASES"
    [ testRun "Test Case 1.1: Verify that a user can create a raffle with valid raffle configuration" createNewRaffleTC1
    , testRun " Test Case 1.2: Verify that a user cannot create a raffle with Commit Deadline in the past" createNewRaffleTC2
    , testRun " Test Case 1.3: Verify that a user cannot create a raffle with Reveal Deadline before Commit Deadline" createNewRaffleTC3
    , testRun " Test Case 1.4: Verify that a user cannot create a raffle with empty stake" createNewRaffleTC4
    , testRun " Test Case 1.5: Verify that a user cannot create a raffle with stake containing Ada" createNewRaffleTC5
    ]

otherTests :: TestTree
otherTests =
  testGroup
    "CREATE RAFFLE TEST CASES"
    [ testRun "CREATE NEW -> UPDATE" createAndUpdateScenario
    , testRun "CREATE NEW ->  EXPIRE -> RECOVER STAKE" createExpireRecoverScenario
    , testRun "CREATE NEW ->  CANCEL" createAndCancelScenario
    , testRun "CREATE NEW ->  BUY 3 -> UNDERFUNDED" underfundedScenario
    , testRun "UNDERFUNDED" underfundedScenario
    -- testRun "SUCCESS SCENARIOS" raffleizeSuccessScenario
    ]

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Create new raffle

------------------------------------------------------------------------------------------------

{- | Test Case 1.1: Verify that a user can create a raffle with valid raffle configuration
Preconditions:
- Wallet has enough Ada to cover the raffle collateral and transaction fees
Preconditions:
- Raffle is in the "NEW" state.
-}
createNewRaffleTC1 :: Wallets -> Run ()
createNewRaffleTC1 Wallets {..} = do
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
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> do
      when (riStateLabel ri /= "NEW") $ logError "not in status NEW"

{- | Test Case 1.2: Verify that a user cannot create a raffle with Commit Deadline in the past
Preconditions:
- Wallet has enough Ada to cover the raffle collateral and transaction fees
Preconditions:
- Transaction fails
-}
createNewRaffleTC2 :: Wallets -> Run ()
createNewRaffleTC2 Wallets {..} = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9

  -- . Create raffle
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let cddl = slotToEndPOSIXTime sltCfg 4 -- Deadline set in the past
  let rddl = slotToEndPOSIXTime sltCfg 50
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 3
          , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
          }
  raffleizeTransactionThatMustFailRun w1 roc (User (CreateRaffle config)) Nothing Nothing

{- | Test Case 1.3: Verify that a user cannot create a raffle with Reveal Deadline before Commit Deadline
Preconditions:
- Wallet has enough Ada to cover the raffle collateral and transaction fees
Preconditions:
- Transaction fails
-}
createNewRaffleTC3 :: Wallets -> Run ()
createNewRaffleTC3 Wallets {..} = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9

  -- . Create raffle
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let cddl = slotToEndPOSIXTime sltCfg 51
  let rddl = slotToEndPOSIXTime sltCfg 50 -- Reveal ddl before commit ddl
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 3
          , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
          }
  raffleizeTransactionThatMustFailRun w1 roc (User (CreateRaffle config)) Nothing Nothing

{- | Test Case 1.4: Verify that a user cannot create a raffle with empty stake
Preconditions:
- Wallet has enough Ada to cover the raffle collateral and transaction fees
Preconditions:
- Transaction fails
-}
createNewRaffleTC4 :: Wallets -> Run ()
createNewRaffleTC4 Wallets {..} = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9

  -- . Create raffle
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let cddl = slotToEndPOSIXTime sltCfg 25
  let rddl = slotToEndPOSIXTime sltCfg 60
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 3
          , rStake = mempty
          }
  raffleizeTransactionThatMustFailRun w1 roc (User (CreateRaffle config)) Nothing Nothing

{- | Test Case 1.5: Verify that a user cannot create a raffle with stake containing Ada
Preconditions:
- Wallet has enough Ada to cover the raffle collateral and transaction fees
Preconditions:
- Transaction fails
-}
createNewRaffleTC5 :: Wallets -> Run ()
createNewRaffleTC5 Wallets {..} = do
  -- . Deploy validators
  roc <- deployValidatorsRun w9

  -- . Create raffle
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let cddl = slotToEndPOSIXTime sltCfg 25
  let rddl = slotToEndPOSIXTime sltCfg 60
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 3
          , rStake = adaValueFromLovelaces 10 <> valueToPlutus (fakeIron 9876)
          }
  raffleizeTransactionThatMustFailRun w1 roc (User (CreateRaffle config)) Nothing Nothing

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Update Raffle Config

-- ------------------------------------------------------------------------------------------------

createAndUpdateScenario :: Wallets -> Run ()
createAndUpdateScenario Wallets {..} = do
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
  ri <- fromMaybe (error $ "Raffle not found: " <> show raffleId) <$> queryRaffleRun w1 raffleId
  when (riStateLabel ri /= "NEW") $ logError "not in status NEW"
  waitNSlots 3

  -- . Update the raffle
  sltCfg2 <- gets (mockConfigSlotConfig . mockConfig)
  let newcddl = slotToEndPOSIXTime sltCfg2 16
  let newrddl = slotToEndPOSIXTime sltCfg2 26
  let newconfig =
        RaffleConfig
          { rCommitDDL = newcddl
          , rRevealDDL = newrddl
          , rTicketPrice = 10_000_000
          , rMinTickets = 2
          , rStake = valueToPlutus (fakeIron 100) <> valueToPlutus (fakeGold 100)
          }
  (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner (Update newconfig)) (Just raffleId) Nothing
  mri2 <- queryRaffleRun w1 raffleId2
  case mri2 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri2 -> do
      when (raffleId2 /= raffleId) $ logError "not same id"
      when (raffleId2 /= raffleId) $ logError "not same id"
      let prevStakeValue = rStake (rConfig (riRsd ri))
      let currentStakeValue = rStake (rConfig (riRsd ri2))
      let updatedVal = riValue ri #- prevStakeValue #+ currentStakeValue
      when (riValue ri2 #/= updatedVal) $ logError "locked value does not match the config "

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Expired -> Recover Stake

-- ------------------------------------------------------------------------------------------------

createAndCancelScenario :: Wallets -> Run ()
createAndCancelScenario Wallets {..} = do
  roc <- deployValidatorsRun w9

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
  waitNSlots 2
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> when (riStateLabel ri /= "NEW") $ logError "not in status NEW"
  (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner Cancel) (Just raffleId) Nothing
  mri2 <- queryRaffleRun w1 raffleId2
  case mri2 of
    Nothing -> return ()
    Just _ri -> logError $ "Raffle should not exist: " <> show raffleId

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Expired -> Recover Stake

-- ------------------------------------------------------------------------------------------------

createExpireRecoverScenario :: Wallets -> Run ()
createExpireRecoverScenario Wallets {..} = do
  roc <- deployValidatorsRun w9

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
  waitNSlots 20 -- Slot 31 - EXPIRED
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> when (riStateLabel ri /= "EXPIRED_LOCKED_STAKE") $ logError "not in status EXPIRED_LOCKED_STAKE"
  waitNSlots 20
  (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
  mri2 <- queryRaffleRun w1 raffleId2
  case mri2 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri2 -> when (riStateLabel ri2 /= "EXPIRED_FINAL") $ logError "not in status EXPIRED_FINAL"

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Buy -> Underfunded -> Refund -> Recover Stake

-- ------------------------------------------------------------------------------------------------

underfundedScenario :: Wallets -> Run ()
underfundedScenario Wallets {..} = do
  roc <- deployValidatorsRun w9

  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let cddl = slotToEndPOSIXTime sltCfg 20
  let rddl = slotToEndPOSIXTime sltCfg 50
  let config =
        RaffleConfig
          { rCommitDDL = cddl
          , rRevealDDL = rddl
          , rTicketPrice = 5_000_000
          , rMinTickets = 4
          , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
          }

  (_txId, raffleId) <- raffleizeTransactionRun w1 roc (User (CreateRaffle config)) Nothing Nothing
  mri <- queryRaffleRun w1 raffleId
  case mri of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> when (riStateLabel ri /= "NEW") $ logError "not in status NEW"
  waitNSlots 1
  let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
  let secretHash = blake2b_256 secret
  (_txId, ticket1) <- raffleizeTransactionRun w2 roc (User (BuyTicket secretHash)) (Just raffleId) Nothing
  (_txId, ticket2) <- raffleizeTransactionRun w3 roc (User (BuyTicket secretHash)) (Just raffleId) Nothing
  (_txId, ticket3) <- raffleizeTransactionRun w4 roc (User (BuyTicket secretHash)) (Just raffleId) Nothing
  mri2 <- queryRaffleRun w1 raffleId
  case mri2 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> do
      when (riStateLabel ri /= "COMMITTING") $ logError "not in status COMMITTING"
      when (rSoldTickets (riRsd ri) /= 3) $ logError "incorrect number of tickets sold"
  waitNSlots 20
  mri3 <- queryRaffleRun w1 raffleId
  case mri3 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> when (riStateLabel ri /= "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS"

  (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
  mri4 <- queryRaffleRun w1 raffleId2
  case mri4 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> when (riStateLabel ri /= "UNDERFUNDED_LOCKED_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_REFUNDS"

  (_txId, _ticket12) <- raffleizeTransactionRun w2 roc (TicketOwner RefundTicket) (Just ticket1) Nothing
  mri6 <- queryRaffleRun w1 raffleId2
  case mri6 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> do
      when (riStateLabel ri /= "UNDERFUNDED_LOCKED_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_REFUNDS"
      when (rRefundedTickets (riRsd ri) /= 1) $ logError "incorrect number of tickets refunded"

  (_txId, _ticket22) <- raffleizeTransactionRun w3 roc (TicketOwner RefundTicket) (Just ticket2) Nothing
  mri7 <- queryRaffleRun w1 raffleId2
  case mri7 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> do
      when (riStateLabel ri /= "UNDERFUNDED_LOCKED_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_REFUNDS"
      when (rRefundedTickets (riRsd ri) /= 2) $ logError "incorrect number of tickets refunded"

  (_txId, _ticket32) <- raffleizeTransactionRun w4 roc (TicketOwner RefundTicket) (Just ticket3) Nothing
  mri8 <- queryRaffleRun w1 raffleId2
  case mri8 of
    Nothing -> logError $ "Raffle not found: " <> show raffleId
    Just ri -> do
      when (riStateLabel ri /= "UNDERFUNDED_FINAL") $ logError $ "not in status UNDERFUNDED_FINAL: " <> riStateLabel ri
      when (rRefundedTickets (riRsd ri) /= 3) $ logError "incorrect number of tickets refunded"

  return ()

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Cancel Raffle |

-- ------------------------------------------------------------------------------------------------

-- createCancel :: Wallets -> Run ()
-- createCancel wallets@Wallets {..} = do
--   (raffleValidatorTxOutRef, raffleId) <- createNew wallets

--   --   Cancel the raffle
--   cancelRaffleRUN w1 raffleValidatorTxOutRef raffleId

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Update -> Expired

-- ------------------------------------------------------------------------------------------------

-- createUpdateExpired :: Wallets -> Run (GYTxOutRef, AssetClass)
-- createUpdateExpired wallets@Wallets {..} = do
--   (raffleValidatorTxOutRef, raffleId) <- createUpdate wallets
--   ---3. WAIT
--   waitNSlots 20 -- Slot 31 - EXPIRED
--   s <- queryRaffleRUN True w1 raffleId
--   when (s /= 10) $ logError "not in EXPIRED_LOCKED_STAKE"
--   return (raffleValidatorTxOutRef, raffleId)

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Update -> Cancel |

-- ------------------------------------------------------------------------------------------------
-- createUpdateCancel :: Wallets -> Run ()
-- createUpdateCancel wallets@Wallets {..} = do
--   (raffleValidatorTxOutRef, raffleId) <- createUpdate wallets
--   cancelRaffleRUN w1 raffleValidatorTxOutRef raffleId

-- ----------------------
-- -- SUCCESS SCENARIO
-- -----------------------

-- raffleizeSuccessScenario :: Wallets -> Run ()
-- raffleizeSuccessScenario Wallets {..} = do
--   -- Deploy the validator to be used as reference script
--   refTicketValidator <- deployReferenceScriptRUN ticketValidatorGY w9 (walletAddress w9)
--   refRaffleValidator <- deployReferenceScriptRUN raffleizeValidatorGY w9 (walletAddress w9)

--   sltCfg <- gets (mockConfigSlotConfig . mockConfig)
--   let cddl = slotToEndPOSIXTime sltCfg 20
--   let rddl = slotToEndPOSIXTime sltCfg 50
--   let config =
--         RaffleConfig
--           { rCommitDDL = cddl
--           , rRevealDDL = rddl
--           , rTicketPrice = 5_000_000
--           , rMinTickets = 3
--           , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
--           }

--   (_, raffleId) <- raffleizeTransactionRun w1 (RaffleizeTxBuildingContext refRaffleValidator refTicketValidator) (User (CreateRaffle config)) Nothing Nothing

--   -- Buy ticket to raffle
--   ws <- buyNTicketsRUN refRaffleValidator raffleId [w1, w2, w3, w4] ["unu", "doi", "trei", fromString @BuiltinByteString "84a289f6f0dc3d1e18dcac4687604d7184a289f6f0dc3d1e18dcac4687604d71"]

--   waitNSlots 4
--   -- Revealing tickets
--   revealNTicketsRUN refRaffleValidator refTicketValidator raffleId (take 3 ws)
--   revealNTicketsRUN refRaffleValidator refTicketValidator raffleId (drop 3 ws)
--   mapM_ (queryTicketRUN w1) $ fst <$> ws

--   _ <- runWallet' w1 $ do
--     collectAmountTXRun refRaffleValidator raffleId
--   logInfo' ("RAFFLE OWNER COLLECTED AMOUNT :" ++ show raffleId)

--   void $ queryRaffleRUN True w1 raffleId

--   _ <- runWallet' w1 $ do
--     winnerCollectStakeTXRun refRaffleValidator refTicketValidator (head (fst <$> ws))
--   logInfo' ("TICKET OWNER REDEEM STAKE :" ++ show raffleId)

--   s <- queryRaffleRUN True w1 raffleId
--   when (s /= 43) $ logError "not in SUCCESS_FINAL"

-- ----------------------
-- -- UNDERFUNDED SCENARIO
-- -----------------------

-- underfundedScenario :: Wallets -> Run ()
-- underfundedScenario wallets@Wallets {..} = do
--   -- Deploy the validator to be used as reference script
--   refTicketValidator <- deployReferenceScriptRUN ticketValidatorGY w9 (walletAddress w9)
--   (refRaffleValidator, raffleId) <- createNew wallets

--   -- Buy ticket to raffle
--   ws <- buyNTicketsRUN refRaffleValidator raffleId [w1, w2, w3, w4] ["unu", "doi", "trei", fromString @BuiltinByteString "84a289f6f0dc3d1e18dcac4687604d7184a289f6f0dc3d1e18dcac4687604d71"]

--   waitNSlots 4
--   -- Revealing tickets
--   -- revealNTicketsRUN refRaffleValidator refTicketValidator raffleId (take 3 ws)
--   revealNTicketsRUN refRaffleValidator refTicketValidator raffleId (drop 3 ws)
--   mapM_ (queryTicketRUN w1) $ fst <$> ws
--   waitNSlots 30 -- Slot 52
--   recoverStakeRaffleRUN w1 refRaffleValidator raffleId
--   void $ queryRaffleRUN True w1 raffleId

---------------------
------------------------
------------------------
------------------------
------------------------
------------------------
------------------------
------------------------
------------------------

--- >>> lengthOfByteString "5b39bfccb1447d4aae30e7a4fb0f4ba37e79ea96ec54b5ba7223979a15e4d0ae5b39bfccb1447d4aae30e7a4fb0f4ba37e79ea96ec54b5ba7223979a15e4d0ae=====------------------------"
-- 157
-- TODO DE ANALIZAT

--- >>> lengthOfByteString $ blake2b_256 "marius"
-- 32
