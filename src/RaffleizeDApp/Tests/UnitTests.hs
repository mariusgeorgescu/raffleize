module UnitTests where

import Control.Monad (unless)
import Control.Monad.Extra (when)
import Data.Maybe qualified
import GeniusYield.Test.Clb (GYTxMonadClb, mkTestFor)
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types (valueToPlutus)
import PlutusLedgerApi.V1.Value (geq)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.OnChain.RaffleizeLogic (generateTicketACFromTicket, raffleCollateralValue)
import RaffleizeDApp.OnChain.Utils (adaValueFromLovelaces)
import RaffleizeDApp.TxBuilding.Utils
import Test.Tasty
import TestRuns

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

-- ------------------------------------------------------------------------------------------------

-- -- * Create new raffle scenarios

-- ------------------------------------------------------------------------------------------------
createRaffleTests :: TestTree
createRaffleTests =
  testGroup
    "CREATE RAFFLE TEST CASES"
    [ mkTestFor "Test Case 1.1: Verify that a user can create a raffle with valid raffle configuration" createNewRaffleTC1
    , mkTestFor " Test Case 1.2: Verify that a user cannot create a raffle with Commit Deadline in the past" createNewRaffleTC2
    , mkTestFor " Test Case 1.3: Verify that a user cannot create a raffle with Reveal Deadline before Commit Deadline" createNewRaffleTC3
    , mkTestFor " Test Case 1.4: Verify that a user cannot create a raffle with empty stake" createNewRaffleTC4
    , mkTestFor " Test Case 1.5: Verify that a user cannot create a raffle with stake containing Ada" createNewRaffleTC5
    ]
  where
    createNewRaffleTC1 :: TestInfo -> GYTxMonadClb ()
    createNewRaffleTC1 TestInfo {..} = void $ deployValidatorsAndCreateNewValidRaffleRun testWallets

    createNewRaffleTC2 :: TestInfo -> GYTxMonadClb ()
    createNewRaffleTC2 TestInfo {..} = do
      -- . Deploy validators
      roc <- deployValidatorsRun (w9 testWallets)
      --  Wait 100 slots
      waitNSlots_ 100
      void slotOfCurrentBlock
      -- . Create raffle with deadline in the past
      cddl <- pPOSIXTimeFromSlotInteger 10 -- deadline in the past
      rddl <- pPOSIXTimeFromSlotInteger 200
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 5_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleizeDApp.CustomTypes.ActionTypes.User (CreateRaffle config)) Nothing Nothing

    createNewRaffleTC3 :: TestInfo -> GYTxMonadClb ()
    createNewRaffleTC3 TestInfo {..} = do
      -- . Deploy validators
      roc <- deployValidatorsRun (w9 testWallets)
      -- . Create raffle with reveal ddl before commit ddl
      cddl <- pPOSIXTimeFromSlotInteger 100
      rddl <- pPOSIXTimeFromSlotInteger 99 -- reveal ddl before commit ddl
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 5_000_000
              , rMinTickets = 3
              , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleizeDApp.CustomTypes.ActionTypes.User (CreateRaffle config)) Nothing Nothing

    createNewRaffleTC4 :: TestInfo -> GYTxMonadClb ()
    createNewRaffleTC4 TestInfo {..} = do
      -- . Deploy validators
      roc <- deployValidatorsRun (w9 testWallets)
      -- . Create raffle with empty stake
      cddl <- pPOSIXTimeFromSlotInteger 100
      rddl <- pPOSIXTimeFromSlotInteger 200
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 5_000_000
              , rMinTickets = 3
              , rStake = mempty -- Raffle stake is empty;
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleizeDApp.CustomTypes.ActionTypes.User (CreateRaffle config)) Nothing Nothing

    createNewRaffleTC5 :: TestInfo -> GYTxMonadClb ()
    createNewRaffleTC5 TestInfo {..} = do
      -- . Deploy validators
      roc <- deployValidatorsRun (w9 testWallets)
      -- . Create raffle with stake containing Ada
      cddl <- pPOSIXTimeFromSlotInteger 100
      rddl <- pPOSIXTimeFromSlotInteger 200
      let config =
            RaffleConfig
              { rCommitDDL = cddl
              , rRevealDDL = rddl
              , rTicketPrice = 5_000_000
              , rMinTickets = 3
              , rStake = adaValueFromLovelaces 10 <> valueToPlutus (fakeIron 9876) -- Raffle stake contains Ada;
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleizeDApp.CustomTypes.ActionTypes.User (CreateRaffle config)) Nothing Nothing

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

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Update raffle configuration

-- ------------------------------------------------------------------------------------------------

updateRaffleTests :: TestTree
updateRaffleTests =
  testGroup
    "UPDATE RAFFLE TEST CASES"
    [ mkTestFor "Test Case 2.1: Verify that a user can update a raffle with valid raffle configuration" updateRaffleTC1
    , mkTestFor " Test Case 2.2: Verify that a user cannot update a raffle with Commit Deadline in the past" updateRaffleTC2
    , mkTestFor " Test Case 2.3: Verify that a user cannot update a raffle with Reveal Deadline before Commit Deadline" updateRaffleTC3
    , mkTestFor " Test Case 2.4: Verify that a user cannot update a raffle with empty stake" updateRaffleTC4
    , mkTestFor " Test Case 2.5: Verify that a user cannot update a raffle stake with a value containing Ada" updateRaffleTC5
    ]
  where
    updateRaffleTC1 :: TestInfo -> GYTxMonadClb ()
    updateRaffleTC1 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
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
      void $ raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing
    -- mri2 <- queryRaffleRun (w1 testWallets) raffleId2
    -- case mri2 of
    --   Nothing -> logTestError $ "Raffle not found: " <> show raffleId
    --   Just ri2 -> do
    --     when (raffleId2 /= raffleId) $ logTestError "not same id"
    --     let prevStakeValue = rStake (rConfig (riRsd ri))
    --     let currentStakeValue = rStake (rConfig (riRsd ri2))
    --     let updatedVal = riValue ri #- prevStakeValue #+ currentStakeValue
    --     when (riValue ri2 #/= updatedVal) $ logTestError "locked value does not match the config "

    updateRaffleTC2 :: TestInfo -> GYTxMonadClb ()
    updateRaffleTC2 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rCommitDDL = 10 -- Slot in the past
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

    updateRaffleTC3 :: TestInfo -> GYTxMonadClb ()
    updateRaffleTC3 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rRevealDDL = rCommitDDL raffleConfig - 1
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

    updateRaffleTC4 :: TestInfo -> GYTxMonadClb ()
    updateRaffleTC4 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rStake = mempty -- Stake is empty;
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

    updateRaffleTC5 :: TestInfo -> GYTxMonadClb ()
    updateRaffleTC5 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      let raffleConfig = rConfig $ riRsd ri
      -- . Update the raffle
      let newRaffleConfig =
            raffleConfig
              { rStake = adaValueFromLovelaces 10 <> valueToPlutus (fakeIron 9876) -- Raffle stake contains Ada;
              }
      raffleizeTransactionThatMustFailRun (w1 testWallets) roc (RaffleOwner (Update newRaffleConfig)) (Just raffleId) Nothing

-- ------------------------------------------------------------------------------------------------

-- -- ** SCENARIO: Cancel Raffle

-- ------------------------------------------------------------------------------------------------

cancelRaffleTests :: TestTree
cancelRaffleTests =
  testGroup
    "CANCEL RAFFLE TEST CASES"
    [ mkTestFor "Test Case 3.1: Verify that the raffle owner can cancel the raffle before any tickets are bought" cancelRaffleTC1
    ]
  where
    cancelRaffleTC1 :: TestInfo -> GYTxMonadClb ()
    cancelRaffleTC1 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      (_txId, raffleId2) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner Cancel) (Just raffleId) Nothing
      mri2 <- queryRaffleRun (w1 testWallets) raffleId2
      case mri2 of
        Nothing -> return ()
        Just _ri -> logTestError $ "Raffle should not exist: " <> show raffleId

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Buy first ticket to a raffle

------------------------------------------------------------------------------------------------

buy1stTicketTest :: TestTree
buy1stTicketTest =
  testGroup
    "BUY TICKET FOR 'NEW' RAFFLE"
    [mkTestFor "Test Case 4.1: Verify that a user can buy a ticket to a raffle in status new" buy1stTicketTC1]
  where
    buy1stTicketTC1 :: TestInfo -> GYTxMonadClb ()
    buy1stTicketTC1 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      void $ buyTicketToRaffleRun ri roc (w1 testWallets) secret

------------------------------------------------------------------------------------------------

-- * Scenarios for raffle in status "EXPIRED"

------------------------------------------------------------------------------------------------
expiredStateTests :: TestTree
expiredStateTests =
  testGroup
    "Tests for raffles in status 'Expired''"
    [mkTestFor "Test Case 5.1: Verify that the raffle owner can recover stake from expired raffle" recoverExpiredTC1]
  where
    recoverExpiredTC1 :: TestInfo -> GYTxMonadClb ()
    recoverExpiredTC1 TestInfo {..} = do
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 100 -- EXPIRED
      mri <- queryRaffleRun (w1 testWallets) raffleId
      case mri of
        Nothing -> logTestError $ "Raffle not found: " <> show raffleId
        Just ri' -> when (riStateLabel ri' /= "EXPIRED_LOCKED_STAKE") $ logTestError "not in status EXPIRED_LOCKED_STAKE"
      waitNSlots_ 1
      (_txId, raffleId2) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      mri2 <- queryRaffleRun (w1 testWallets) raffleId2
      case mri2 of
        Nothing -> logTestError $ "Raffle not found: " <> show raffleId
        Just ri2 -> when (riStateLabel ri2 /= "EXPIRED_FINAL") $ logTestError "not in status EXPIRED_FINAL"

-- ------------------------------------------------------------------------------------------------

-- -- ** Scenarios for raffle in status "UNDERFUNDED"

-- ------------------------------------------------------------------------------------------------

underfundedStateTests :: TestTree
underfundedStateTests =
  testGroup
    "Tests for raffles in status 'Underfunded"
    [ mkTestFor "Test Case 6.1: Recover Stake -> All Refunds" underfundedScenario
    , mkTestFor "Test Case 6.2: Refund -> Recover Stake -> Rest of refunds" underfundedScenario2
    , mkTestFor "Test Case 6.3: All Refunds -> Recover Stake" underfundedScenario3
    ]
  where
    underfundedScenario :: TestInfo -> GYTxMonadClb ()
    underfundedScenario TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      let secretHash = blake2b_256 secret
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secretHash)
          , (w3 testWallets, secretHash)
          , (w4 testWallets, secretHash)
          ]
      waitNSlots_ 101 --- Commit DDL pass
      ri2 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri2 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logTestError $ "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS: " <> riStateLabel ri2
      (_txId, raffleId2) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri3 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri3 == "UNDERFUNDED_LOCKED_REFUNDS") $ logTestError $ "not in status UNDERFUNDED_LOCKED_REFUNDS: " <> riStateLabel ri3
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets <-
        refundNTicketsRun
          False
          ri3
          roc
          [ (w2 testWallets, head ticketRefs)
          , (w3 testWallets, ticketRefs !! 1)
          , (w4 testWallets, ticketRefs !! 2)
          ]
      ri4 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId2
      unless (riStateLabel ri4 == "UNDERFUNDED_FINAL") $ logTestError $ "not in status UNDERFUNDED_FINAL: " <> riStateLabel ri4
      unless (riValue ri4 `geq` raffleCollateralValue (riRsd ri4)) $ logTestError "Remained value lower tha raffle collateral"

    underfundedScenario2 :: TestInfo -> GYTxMonadClb ()
    underfundedScenario2 TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      let secretHash = blake2b_256 secret
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secretHash)
          , (w3 testWallets, secretHash)
          , (w4 testWallets, secretHash)
          ]

      waitNSlots_ 101 --- Commit DDL pass
      ri2 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri2 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logTestError "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS"

      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets

      void $ raffleizeTransactionRun (w2 testWallets) roc (TicketOwner RefundTicket) (Just (head ticketRefs)) Nothing
      ri3 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri3 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logTestError "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS"

      (_txId, raffleId2) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri4 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId2
      unless (riStateLabel ri4 == "UNDERFUNDED_LOCKED_REFUNDS") $ logTestError "not in status UNDERFUNDED_LOCKED_REFUNDS"

      _tickets <-
        refundNTicketsRun
          False
          ri4
          roc
          [ (w3 testWallets, ticketRefs !! 1)
          , (w4 testWallets, ticketRefs !! 2)
          ]

      ri5 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId2
      unless (riStateLabel ri5 == "UNDERFUNDED_FINAL") $ logTestError $ "not in status UNDERFUNDED_FINAL: " <> riStateLabel ri5
      unless (riValue ri5 `geq` raffleCollateralValue (riRsd ri5)) $ logTestError "Remained value lower tha raffle collateral"

    underfundedScenario3 :: TestInfo -> GYTxMonadClb ()
    underfundedScenario3 TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      let secretHash = blake2b_256 secret
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secretHash)
          , (w3 testWallets, secretHash)
          , (w4 testWallets, secretHash)
          ]
      waitNSlots_ 101 --- Commit DDL pass
      ri2 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri2 == "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS") $ logTestError $ "not in status UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS" <> riStateLabel ri
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets <-
        refundNTicketsRun
          False
          ri2
          roc
          [ (w2 testWallets, head ticketRefs)
          , (w3 testWallets, ticketRefs !! 1)
          , (w4 testWallets, ticketRefs !! 2)
          ]
      ri5 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri5 == "UNDERFUNDED_LOCKED_STAKE") $ logTestError "not in status UNDERFUNDED_LOCKED_STAKE"
      void $ raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri6 <- Data.Maybe.fromMaybe (error "raffle not found") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri6 == "UNDERFUNDED_FINAL") $ logTestError "not in status UNDERFUNDED_FINAL"
      unless (riValue ri6 `geq` raffleCollateralValue (riRsd ri6)) $ logTestError "Remained value lower than raffle collateral"

-- ------------------------------------------------------------------------------------------------

-- -- ** Scenarios for raffle in status "UNREVEALED"

-- ------------------------------------------------------------------------------------------------

unrevealedStateTests :: TestTree
unrevealedStateTests =
  testGroup
    "Tests for raffles in status 'UNREVEALED"
    [ mkTestFor "Test Case 7.1: No reveales -> Raffle Owner RecoverStakeAndAmount" unrevealedScenarioTC1
    , mkTestFor "Test Case 7.2: Raffle Owner RecoverStake-> Revealed Tickets Refund Extra" unrevealedScenarioTC2
    , mkTestFor "Test Case 7.3:  Revealed Tickets Refund Extra -> Raffle Owner RecoverStake-> " unrevealedScenarioTC3
    ]
  where
    unrevealedScenarioTC1 :: TestInfo -> GYTxMonadClb ()
    unrevealedScenarioTC1 TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      _tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secret)
          , (w3 testWallets, secret)
          , (w4 testWallets, secret)
          , (w5 testWallets, secret)
          ]
      waitNSlots_ 101 --- Commit DDL pass
      _ri2 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      waitNSlots_ 101 --- Reveal DDL pass
      ri3 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri3 == "UNREVEALED_NO_REVEALS") $ logTestError "not in status UNREVEALED_NO_REVEALS"
      (_txId, _raffleId) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStakeAndAmount) (Just raffleId) Nothing
      ri4 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri4 == "UNREVEALED_FINAL") $ logTestError "not in status UNREVEALED_FINAL"
      unless (riValue ri4 `geq` raffleCollateralValue (riRsd ri4)) $ logTestError "Remained value lower tha raffle collateral"

    unrevealedScenarioTC2 :: TestInfo -> GYTxMonadClb ()
    unrevealedScenarioTC2 TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secret)
          , (w3 testWallets, secret)
          , (w4 testWallets, secret)
          , (w5 testWallets, secret)
          ]
      waitNSlots_ 101 --- Commit DDL pass
      ri2 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2 testWallets, head ticketRefs, secret)
          , (w3 testWallets, ticketRefs !! 1, secret)
          ]
      waitNSlots_ 101 --- Reveal DDL pass
      ri3 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri3 == "UNREVEALED_LOCKED_STAKE_AND_REFUNDS") $ logTestError "not in status UNREVEALED_LOCKED_STAKE_AND_REFUNDS"
      (_txId, _raffleId) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri4 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri4 == "UNREVEALED_LOCKED_REFUNDS") $ logTestError "not in status UNREVEALED_LOCKED_REFUNDS"
      void $
        refundNTicketsRun
          True
          ri4
          roc
          [ (w2 testWallets, head ticketRefs)
          , (w3 testWallets, ticketRefs !! 1)
          ]
      ri5 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri5 == "UNREVEALED_FINAL") $ logTestError "not in status UNREVEALED_FINAL"
      unless (riValue ri5 `geq` raffleCollateralValue (riRsd ri5)) $ logTestError "Remained value lower tha raffle collateral"

    unrevealedScenarioTC3 :: TestInfo -> GYTxMonadClb ()
    unrevealedScenarioTC3 TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 3 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secret)
          , (w3 testWallets, secret)
          , (w4 testWallets, secret)
          , (w5 testWallets, secret)
          ]
      waitNSlots_ 101 --- Commit DDL pass
      ri2 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2 testWallets, head ticketRefs, secret)
          , (w3 testWallets, ticketRefs !! 1, secret)
          ]
      waitNSlots_ 101 --- Reveal DDL pass
      ri3 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri3 == "UNREVEALED_LOCKED_STAKE_AND_REFUNDS") $ logTestError "not in status UNREVEALED_LOCKED_STAKE_AND_REFUNDS"
      void $
        refundNTicketsRun
          True
          ri3
          roc
          [ (w2 testWallets, head ticketRefs)
          , (w3 testWallets, ticketRefs !! 1)
          ]
      ri4 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri4 == "UNREVEALED_LOCKED_STAKE") $ logTestError "not in status UNREVEALED_LOCKED_STAKE"
      (_txId, _raffleId) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner RecoverStake) (Just raffleId) Nothing
      ri5 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri5 == "UNREVEALED_FINAL") $ logTestError "not in status UNREVEALED_FINAL"
      unless (riValue ri5 `geq` raffleCollateralValue (riRsd ri5)) $ logTestError "Remained value lower tha raffle collateral"

-- ------------------------------------------------------------------------------------------------

-- -- ** Scenarios for raffle in status "SUCCESS"

-- ------------------------------------------------------------------------------------------------

successStateTests :: TestTree
successStateTests =
  testGroup
    "Tests for raffles in status 'SUCCESS"
    [ mkTestFor "Test Case 8.1: (Refund Losing) -> Winner Claims Prize -> (Refund Losing) -> Get collected amount -> (Refund Losing)" sucessScenarioTC1
    , mkTestFor "Test Case 8.2: (Refund Losing) -> Get collected amount -> (Refund Losing) -> Winner Claims Prize -> (Refund Losing)" sucessScenarioTC2
    ]
  where
    sucessScenarioTC1 :: TestInfo -> GYTxMonadClb ()
    sucessScenarioTC1 TestInfo {..} = do
      -- Create Raffle
      (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
      let raffleId = rRaffleID $ riRsd ri
      waitNSlots_ 1
      let secret = "bbaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
      -- Buy 4 tickets
      tickets <-
        buyNTicketsToRaffleRun
          ri
          roc
          [ (w2 testWallets, secret)
          , (w3 testWallets, secret)
          , (w4 testWallets, secret)
          , (w5 testWallets, secret)
          ]
      waitNSlots_ 101 --- Commit DDL pass
      ri2 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
      _tickets2 <-
        reavealNTicketsRun
          ri2
          roc
          [ (w2 testWallets, head ticketRefs, secret)
          , (w3 testWallets, ticketRefs !! 1, secret)
          , (w4 testWallets, ticketRefs !! 2, secret)
          , (w5 testWallets, ticketRefs !! 3, secret)
          ]
      ri3 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri3 == "SUCCESS_LOCKED_STAKE_AND_AMOUNT") $ logTestError "not in status SUCCESS_LOCKED_STAKE_AND_AMOUNT"

      void $ raffleizeTransactionRun (w3 testWallets) roc (TicketOwner RefundCollateralLosing) (Just (ticketRefs !! 1)) Nothing
      mti1 <- queryTicketRun (w1 testWallets) (ticketRefs !! 1)
      unless (isNothing mti1) $ logTestError "Ticket must not exist !"

      (_txId, _raffleId) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner CollectAmount) (Just raffleId) Nothing
      ri4 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri4 == "SUCCESS_LOCKED_STAKE") $ logTestError "not in status SUCCESS_LOCKED_STAKE"

      void $ raffleizeTransactionRun (w4 testWallets) roc (TicketOwner RefundCollateralLosing) (Just (ticketRefs !! 2)) Nothing
      mti2 <- queryTicketRun (w1 testWallets) (ticketRefs !! 2)
      unless (isNothing mti2) $ logTestError "Ticket must not exist !"

      (_txId, _raffleId) <- raffleizeTransactionRun (w2 testWallets)  roc (TicketOwner CollectStake) (Just (head ticketRefs)) Nothing
      ri5 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
      unless (riStateLabel ri5 == "SUCCESS_FINAL") $ logTestError "not in status SUCCESS_FINAL"
      unless (riValue ri5 `geq` raffleCollateralValue (riRsd ri5)) $ logTestError "Remained value lower tha raffle collateral"

      void $ raffleizeTransactionRun (w5 testWallets) roc (TicketOwner RefundCollateralLosing) (Just (ticketRefs !! 3)) Nothing
      mti3 <- queryTicketRun (w1 testWallets) (ticketRefs !! 3)
      unless (isNothing mti3) $ logTestError "Ticket must not exist !"

sucessScenarioTC2 :: TestInfo -> GYTxMonadClb ()
sucessScenarioTC2 TestInfo {..} = do
  -- Create Raffle
  (ri, roc) <- deployValidatorsAndCreateNewValidRaffleRun testWallets
  let raffleId = rRaffleID $ riRsd ri
  waitNSlots_ 1
  let secret = "abaa26009811bc8cd67953256523fea78280ebf3bf061b87e3c8bea43188a222"
  -- Buy 4 tickets
  tickets <-
    buyNTicketsToRaffleRun
      ri
      roc
      [ (w2 testWallets, secret)
      , (w3 testWallets, secret)
      , (w4 testWallets, secret)
      , (w5 testWallets, secret)
      ]
  waitNSlots_ 101 --- Commit DDL pass
  ri2 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
  let ticketRefs = fst . generateTicketACFromTicket . tiTsd <$> tickets
  _tickets2 <-
    reavealNTicketsRun
      ri2
      roc
      [ (w2 testWallets, head ticketRefs, secret)
      , (w3 testWallets, ticketRefs !! 1, secret)
      , (w4 testWallets, ticketRefs !! 2, secret)
      , (w5 testWallets, ticketRefs !! 3, secret)
      ]
  ri3 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
  unless (riStateLabel ri3 == "SUCCESS_LOCKED_STAKE_AND_AMOUNT") $ logTestError "not in status SUCCESS_LOCKED_STAKE_AND_AMOUNT"

  -- WINNER is ticket 0

  ti0 <- Data.Maybe.fromMaybe (error "Ticket not fund") <$> queryTicketRun (w1 testWallets) (head ticketRefs)
  unless (tiStateLabel ti0 == "WINNING") $ logTestError "NOT WINNING"
  ti1 <- Data.Maybe.fromMaybe (error "Ticket not fund") <$> queryTicketRun (w1 testWallets) (ticketRefs !! 1)
  unless (tiStateLabel ti1 == "LOSING") $ logTestError "NOT LOSING"
  ti2 <- Data.Maybe.fromMaybe (error "Ticket not fund") <$> queryTicketRun (w1 testWallets) (ticketRefs !! 2)
  unless (tiStateLabel ti2 == "LOSING") $ logTestError "NOT LOSING"

  void $ raffleizeTransactionRun (w3 testWallets) roc (TicketOwner RefundCollateralLosing) (Just (ticketRefs !! 1)) Nothing
  mti1 <- queryTicketRun (w1 testWallets) (ticketRefs !! 1)
  unless (isNothing mti1) $ logTestError "Ticket must not exist !"

  (_txId, _raffleId) <- raffleizeTransactionRun (w2 testWallets) roc (TicketOwner CollectStake) (Just (head ticketRefs)) Nothing
  ri5 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
  unless (riStateLabel ri5 == "SUCCESS_LOCKED_AMOUNT") $ logTestError "not in status SUCCESS_LOCKED_AMOUNT"

  void $ raffleizeTransactionRun (w4 testWallets) roc (TicketOwner RefundCollateralLosing) (Just (ticketRefs !! 2)) Nothing
  mti2 <- queryTicketRun (w1 testWallets) (ticketRefs !! 2)
  unless (isNothing mti2) $ logTestError "Ticket must not exist !"

  (_txId, _raffleId) <- raffleizeTransactionRun (w1 testWallets) roc (RaffleOwner CollectAmount) (Just raffleId) Nothing
  ri6 <- Data.Maybe.fromMaybe (error "Raffle not fund") <$> queryRaffleRun (w1 testWallets) raffleId
  unless (riStateLabel ri6 == "SUCCESS_FINAL") $ logTestError "not in status SUCCESS_FINAL"
  unless (riValue ri6 `geq` raffleCollateralValue (riRsd ri6)) $ logTestError "Remained value lower tha raffle collateral"

  void $ raffleizeTransactionRun (w5 testWallets) roc (TicketOwner RefundCollateralLosing) (Just (ticketRefs !! 3)) Nothing
  mti3 <- queryTicketRun (w1 testWallets) (ticketRefs !! 3)
  unless (isNothing mti3) $ logTestError "Ticket must not exist !"