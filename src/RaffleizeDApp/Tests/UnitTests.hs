module RaffleizeDApp.Tests.UnitTests where

import Cardano.Simple.Ledger.TimeSlot
import Control.Monad
import Control.Monad.State.Class (gets)
import GeniusYield.Test.Utils
import GeniusYield.Types
import Plutus.Model (logError, mockConfig, mockConfigSlotConfig, waitNSlots)
import PlutusTx.Builtins (blake2b_256)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TransferTypes (RaffleInfo (..), TicketInfo (tiTsd))
import RaffleizeDApp.OnChain.RaffleizeLogic (generateTicketACFromTicket)
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.Tests.TestRuns
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Raffleize Unit Tests"
    [ createRaffleTests
    , newStateTests
    , expiredStateTests
    , underfundedStateTests
    , unrevealedStateTests
    , successStateTests
    ]

------------------------------------------------------------------------------------------------

-- * Create new raffle scenarios

------------------------------------------------------------------------------------------------
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
  where
    createNewRaffleTC1 :: Wallets -> Run ()
    createNewRaffleTC1 wallets = void $ deployValidatorsAndCreateNewValidRaffleRun wallets

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
              , rStake = mempty -- Raffle stake is empty;
              }
      raffleizeTransactionThatMustFailRun w1 roc (User (CreateRaffle config)) Nothing Nothing

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
              , rStake = adaValueFromLovelaces 10 <> valueToPlutus (fakeIron 9876) -- Raffle stake contains Ada;
              }
      raffleizeTransactionThatMustFailRun w1 roc (User (CreateRaffle config)) Nothing Nothing

------------------------------------------------------------------------------------------------

-- * Scenarios for raffle in status "NEW"

------------------------------------------------------------------------------------------------
newStateTests :: TestTree
newStateTests =
  testGroup
    "Tests for raffles in status 'NEW'"
    [ updateRaffleTests
    , cancelRaffleTests
    , buy1stTicketTest
    ]

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Update raffle configuration

------------------------------------------------------------------------------------------------

updateRaffleTests :: TestTree
updateRaffleTests =
  testGroup
    "UPDATE RAFFLE TEST CASES"
    [ testRun "Test Case 2.1: Verify that a user can update a raffle with valid raffle configuration" updateRaffleTC1
    , testRun " Test Case 2.2: Verify that a user cannot update a raffle with Commit Deadline in the past" updateRaffleTC2
    , testRun " Test Case 2.3: Verify that a user cannot update a raffle with Reveal Deadline before Commit Deadline" updateRaffleTC3
    , testRun " Test Case 2.4: Verify that a user cannot update a raffle with empty stake" updateRaffleTC4
    , testRun " Test Case 2.5: Verify that a user cannot update a raffle stake with a value containing Ada" updateRaffleTC5
    ]
  where
    updateRaffleTC1 :: Wallets -> Run ()
    updateRaffleTC1 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rCommitDDL = rCommitDDL raffleConfig + 10
              , rRevealDDL = rRevealDDL raffleConfig + 10
              , rTicketPrice = 15_000_000
              , rMinTickets = 20
              , rStake = valueToPlutus (fakeIron 100) <> valueToPlutus (fakeGold 100)
              }
      (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing
      mri2 <- queryRaffleRun w1 raffleId2
      case mri2 of
        Nothing -> logError $ "Raffle not found: " <> show raffleId
        Just ri2 -> do
          when (raffleId2 /= raffleId) $ logError "not same id"
          let prevStakeValue = rStake (rConfig (riRsd ri))
          let currentStakeValue = rStake (rConfig (riRsd ri2))
          let updatedVal = riValue ri #- prevStakeValue #+ currentStakeValue
          when (riValue ri2 #/= updatedVal) $ logError "locked value does not match the config "

    updateRaffleTC2 :: Wallets -> Run ()
    updateRaffleTC2 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rCommitDDL = 10 -- Slot in the past
              }
      raffleizeTransactionThatMustFailRun w1 roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

    updateRaffleTC3 :: Wallets -> Run ()
    updateRaffleTC3 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rRevealDDL = rCommitDDL raffleConfig - 1
              }
      raffleizeTransactionThatMustFailRun w1 roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

    updateRaffleTC4 :: Wallets -> Run ()
    updateRaffleTC4 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rStake = mempty -- Stake is empty;
              }
      raffleizeTransactionThatMustFailRun w1 roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

    updateRaffleTC5 :: Wallets -> Run ()
    updateRaffleTC5 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rStake = adaValueFromLovelaces 10 <> valueToPlutus (fakeIron 9876) -- Raffle stake contains Ada;
              }
      raffleizeTransactionThatMustFailRun w1 roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Cancel Raffle

-- ------------------------------------------------------------------------------------------------

cancelRaffleTests :: TestTree
cancelRaffleTests =
  testGroup
    "CANCEL RAFFLE TEST CASES"
    [ testRun "Test Case 3.1: Verify that the raffle owner can cancel the raffle before any tickets are bought" cancelRaffleTC1
    ]
  where
    cancelRaffleTC1 :: Wallets -> Run ()
    cancelRaffleTC1 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner Cancel) (Just raffleId) Nothing
      mri2 <- queryRaffleRun w1 raffleId2
      case mri2 of
        Nothing -> return ()
        Just _ri -> logError $ "Raffle should not exist: " <> show raffleId

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Buy first ticket to a raffle

------------------------------------------------------------------------------------------------

buy1stTicketTest :: TestTree
buy1stTicketTest =
  testGroup
    "BUY TICKET FOR 'NEW' RAFFLE"
    [testRun "Test Case 4.1: Verify that a user can buy a ticket to a raffle in status new" buy1stTicketTC1]
  where
    buy1stTicketTC1 :: Wallets -> Run ()
    buy1stTicketTC1 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      void $ buyTicketToRaffleRun ri roc w1 secret

------------------------------------------------------------------------------------------------

-- * Scenarios for raffle in status "EXPIRED"

------------------------------------------------------------------------------------------------
expiredStateTests :: TestTree
expiredStateTests =
  testGroup
    "Tests for raffles in status 'Expired''"
    [testRun "Test Case 5.1: Verify that the raffle owner can recover stake from expired raffle" recoverExpiredTC1]
  where
    recoverExpiredTC1 :: Wallets -> Run ()
    recoverExpiredTC1 wallets@Wallets {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun wallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 100 -- EXPIRED
      mri <- queryRaffleRun w1 raffleId
      case mri of
        Nothing -> logError $ "Raffle not found: " <> show raffleId
        Just ri' -> when (riStateLabel ri' /= "EXPIRED_LOCKED_STAKE") $ logError "not in status EXPIRED_LOCKED_STAKE"
      waitNSlots 1
      (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      mri2 <- queryRaffleRun w1 raffleId2
      case mri2 of
        Nothing -> logError $ "Raffle not found: " <> show raffleId
        Just ri2 -> when (riStateLabel ri2 /= "EXPIRED_FINAL") $ logError "not in status EXPIRED_FINAL"

-- ------------------------------------------------------------------------------------------------

-- -- ** Scenarios for raffle in status "UNDERFUNDED"

-- ------------------------------------------------------------------------------------------------

underfundedStateTests :: TestTree
underfundedStateTests =
  testGroup
    "Tests for raffles in status 'Underfunded"
    [ testRun "Test Case 6.1: Recover Stake -> All Refunds" underfundedScenario
    , testRun "Test Case 6.2: Refund -> Recover Stake -> Rest of refunds" underfundedScenario2
    , testRun "Test Case 6.3: All Refunds -> Recover Stake" underfundedScenario3
    ]
  where
    underfundedScenario :: Wallets -> Run ()
    underfundedScenario wallets@Wallets {..} = do
      -- Create Raffle
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
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      let secretHash = blake2b_256 secret
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secretHash)
          , (w3, secretHash)
          , (w4, secretHash)
          ]
      waitNSlots 100 --- Commit DDL pass
      ri2 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri2 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logError $ "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS: " <> riStateLabel ri2
      (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri3 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "UNDERFUNDED_LOCKED_REFUNDS") $ logError $ "not in status UNDERFUNDED_LOCKED_REFUNDS: " <> riStateLabel ri3
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets <-
        refundNTicketsRun
          False
          ri3
          roc
          [ (w2, head ticketRefs)
          , (w3, ticketRefs !! 1)
          , (w4, ticketRefs !! 2)
          ]
      ri4 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId2
      unless (riStateLabel ri4 == "UNDERFUNDED_FINAL") $ logError $ "not in status UNDERFUNDED_FINAL: " <> riStateLabel ri4

    underfundedScenario2 :: Wallets -> Run ()
    underfundedScenario2 wallets@Wallets {..} = do
      -- Create Raffle
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
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      let secretHash = blake2b_256 secret
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secretHash)
          , (w3, secretHash)
          , (w4, secretHash)
          ]

      waitNSlots 100 --- Commit DDL pass
      ri2 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri2 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS"

      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets

      void $ raffleizeTransactionRun w2 roc (TicketOwner RefundTicket) (Just (head ticketRefs)) Nothing
      ri3 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS"

      (_txId, raffleId2) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri4 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId2
      unless (riStateLabel ri4 == "UNDERFUNDED_LOCKED_REFUNDS") $ logError "not in status UNDERFUNDED_LOCKED_REFUNDS"

      _tickets <-
        refundNTicketsRun
          False
          ri4
          roc
          [ (w3, ticketRefs !! 1)
          , (w4, ticketRefs !! 2)
          ]

      ri5 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId2
      unless (riStateLabel ri5 == "UNDERFUNDED_FINAL") $ logError $ "not in status UNDERFUNDED_FINAL: " <> riStateLabel ri5

    underfundedScenario3 :: Wallets -> Run ()
    underfundedScenario3 wallets@Wallets {..} = do
      -- Create Raffle
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
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      let secretHash = blake2b_256 secret
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secretHash)
          , (w3, secretHash)
          , (w4, secretHash)
          ]
      waitNSlots 100 --- Commit DDL pass
      ri2 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri2 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logError $ "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS" <> riStateLabel ri
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets <-
        refundNTicketsRun
          False
          ri2
          roc
          [ (w2, head ticketRefs)
          , (w3, ticketRefs !! 1)
          , (w4, ticketRefs !! 2)
          ]
      ri5 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri5 == "UNDERFUNDED_LOCKED_STAKE") $ logError "not in status UNDERFUNDED_LOCKED_STAKE"
      void $ raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri6 <- fromMaybe (error "raffle not found") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri6 == "UNDERFUNDED_FINAL") $ logError "not in status UNDERFUNDED_FINAL"

-- ------------------------------------------------------------------------------------------------

-- -- ** Scenarios for raffle in status "UNREVEALED"

-- ------------------------------------------------------------------------------------------------

unrevealedStateTests :: TestTree
unrevealedStateTests =
  testGroup
    "Tests for raffles in status 'UNREVEALED"
    [ testRun "Test Case 7.1: No reveales -> Raffle Owner RecoverStakeAndAmount" unrevealedScenarioTC1
    , testRun "Test Case 7.2: Raffle Owner RecoverStake-> Revealed Tickets Refund Extra" unrevealedScenarioTC2
    , testRun "Test Case 7.3:  Revealed Tickets Refund Extra -> Raffle Owner RecoverStake-> " unrevealedScenarioTC3
    ]
  where
    unrevealedScenarioTC1 :: Wallets -> Run ()
    unrevealedScenarioTC1 wallets@Wallets {..} = do
      -- Create Raffle
      sltCfg <- gets (mockConfigSlotConfig . mockConfig)
      let cddl = slotToEndPOSIXTime sltCfg 20
      let rddl = slotToEndPOSIXTime sltCfg 60
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 10_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      _tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secret)
          , (w3, secret)
          , (w4, secret)
          ]
      waitNSlots 30 --- Commit DDL pass
      _ri2 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      waitNSlots 40 --- Reveal DDL pass
      ri3 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "UNREVEALED_NO_REVEALS") $ logError "not in status UNREVEALED_NO_REVEALS"
      (_txId, _raffleId) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStakeAndAmount) (Just raffleId) Nothing
      ri4 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri4 == "UNREVEALED_FINAL") $ logError "not in status UNREVEALED_FINAL"

    unrevealedScenarioTC2 :: Wallets -> Run ()
    unrevealedScenarioTC2 wallets@Wallets {..} = do
      -- Create Raffle
      sltCfg <- gets (mockConfigSlotConfig . mockConfig)
      let cddl = slotToEndPOSIXTime sltCfg 20
      let rddl = slotToEndPOSIXTime sltCfg 60
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 10_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secret)
          , (w3, secret)
          , (w4, secret)
          ]
      waitNSlots 30 --- Commit DDL pass
      ri2 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2, head ticketRefs, secret)
          , (w3, ticketRefs !! 1, secret)
          ]
      waitNSlots 40 --- Reveal DDL pass
      ri3 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "UNREVEALED_LOCKED_STAKE_AND_REFUNDS") $ logError "not in status UNREVEALED_LOCKED_STAKE_AND_REFUNDS"
      (_txId, _raffleId) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri4 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri4 == "UNREVEALED_LOCKED_REFUNDS") $ logError "not in status UNREVEALED_LOCKED_REFUNDS"
      void $
        refundNTicketsRun
          True
          ri4
          roc
          [ (w2, head ticketRefs)
          , (w3, ticketRefs !! 1)
          ]
      ri5 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri5 == "UNREVEALED_FINAL") $ logError "not in status UNREVEALED_FINAL"

    unrevealedScenarioTC3 :: Wallets -> Run ()
    unrevealedScenarioTC3 wallets@Wallets {..} = do
      -- Create Raffle
      sltCfg <- gets (mockConfigSlotConfig . mockConfig)
      let cddl = slotToEndPOSIXTime sltCfg 20
      let rddl = slotToEndPOSIXTime sltCfg 60
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 10_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secret)
          , (w3, secret)
          , (w4, secret)
          ]
      waitNSlots 30 --- Commit DDL pass
      ri2 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2, head ticketRefs, secret)
          , (w3, ticketRefs !! 1, secret)
          ]
      waitNSlots 40 --- Reveal DDL pass
      ri3 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "UNREVEALED_LOCKED_STAKE_AND_REFUNDS") $ logError "not in status UNREVEALED_LOCKED_STAKE_AND_REFUNDS"
      void $
        refundNTicketsRun
          True
          ri3
          roc
          [ (w2, head ticketRefs)
          , (w3, ticketRefs !! 1)
          ]
      ri4 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri4 == "UNREVEALED_LOCKED_STAKE") $ logError "not in status UNREVEALED_LOCKED_STAKE"
      (_txId, _raffleId) <- raffleizeTransactionRun w1 roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri5 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri5 == "UNREVEALED_FINAL") $ logError "not in status UNREVEALED_FINAL"

-- ------------------------------------------------------------------------------------------------

-- -- ** Scenarios for raffle in status "SUCCESS"

-- ------------------------------------------------------------------------------------------------

successStateTests :: TestTree
successStateTests =
  testGroup
    "Tests for raffles in status 'SUCCESS"
    [ testRun "Test Case 8.1: Winner Claims Prize -> Get collected amount" sucessScenarioTC1
    , testRun "Test Case 8.2: Get collected amount -> Winner Claims Prize" sucessScenarioTC2
    ]
  where
    sucessScenarioTC1 :: Wallets -> Run ()
    sucessScenarioTC1 wallets@Wallets {..} = do
      -- Create Raffle
      sltCfg <- gets (mockConfigSlotConfig . mockConfig)
      let cddl = slotToEndPOSIXTime sltCfg 20
      let rddl = slotToEndPOSIXTime sltCfg 60
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 10_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secret)
          , (w3, secret)
          , (w4, secret)
          ]
      waitNSlots 30 --- Commit DDL pass
      ri2 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2, head ticketRefs, secret)
          , (w3, ticketRefs !! 1, secret)
          , (w4, ticketRefs !! 2, secret)
          ]
      ri3 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "SUCCESS_LOCKED_STAKE_AND_AMOUNT") $ logError "not in status SUCCESS_LOCKED_STAKE_AND_AMOUNT"
      (_txId, _raffleId) <- raffleizeTransactionRun w1 roc (RaffleOwner CollectAmount) (Just raffleId) Nothing
      ri4 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri4 == "SUCCESS_LOCKED_STAKE") $ logError "not in status SUCCESS_LOCKED_STAKE"
      -- WINNER is w2
      (_txId, _raffleId) <- raffleizeTransactionRun w2 roc (TicketOwner CollectStake) (Just (head ticketRefs)) Nothing
      ri5 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri5 == "SUCCESS_FINAL") $ logError "not in status SUCCESS_FINAL"

    sucessScenarioTC2 :: Wallets -> Run ()
    sucessScenarioTC2 wallets@Wallets {..} = do
      -- Create Raffle
      sltCfg <- gets (mockConfigSlotConfig . mockConfig)
      let cddl = slotToEndPOSIXTime sltCfg 20
      let rddl = slotToEndPOSIXTime sltCfg 60
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 10_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      (ri, roc) <- deployValidatorsAndCreateNewRaffleRun wallets config
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2, secret)
          , (w3, secret)
          , (w4, secret)
          ]
      waitNSlots 30 --- Commit DDL pass
      ri2 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2, head ticketRefs, secret)
          , (w3, ticketRefs !! 1, secret)
          , (w4, ticketRefs !! 2, secret)
          ]
      ri3 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri3 == "SUCCESS_LOCKED_STAKE_AND_AMOUNT") $ logError "not in status SUCCESS_LOCKED_STAKE_AND_AMOUNT"
      -- WINNER is w2
      (_txId, _raffleId) <- raffleizeTransactionRun w2 roc (TicketOwner CollectStake) (Just (head ticketRefs)) Nothing
      ri4 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri4 == "SUCCESS_LOCKED_AMOUNT") $ logError "not in status SUCCESS_LOCKED_AMOUNT"
      (_txId, _raffleId) <- raffleizeTransactionRun w1 roc (RaffleOwner CollectAmount) (Just raffleId) Nothing
      ri5 <- fromMaybe (error "Raffle not fund") <$> queryRaffleRun w1 raffleId
      unless (riStateLabel ri5 == "SUCCESS_FINAL") $ logError "not in status SUCCESS_FINAL"
