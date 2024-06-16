module RaffleizeDApp.Tests.UnitTests where

import Cardano.Simple.Ledger.Slot (Slot (..))
import Cardano.Simple.Ledger.TimeSlot
import Control.Monad.State.Class (MonadState (get), gets)
import Data.ByteString hiding (drop, head, replicate, take, zip)
import Data.List.Extra (replicate)
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Plutus.Model (Mock (mockCurrentSlot), logError, logInfo, mockConfig, mockConfigSlotConfig, waitNSlots)
import PlutusLedgerApi.V1.Interval (from, intersection, to)
import PlutusLedgerApi.V1.Value (AssetClass, tokenName)
import PlutusLedgerApi.V2 (POSIXTimeRange, Value)
import PlutusTx.Builtins (blake2b_256)
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes

import Control.Monad
import GeniusYield.Imports
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Operations
import RaffleizeDApp.TxBuilding.Validators
import Test.Tasty

-- | Our unit tests for creating a raffle
createRaffleTests :: TestTree
createRaffleTests =
  testGroup
    "RAFFLEIZE TESTNG SCENATIOS"
    [ testRun "CREATE NEW" createNew
    , testRun "CREATE NEW -> UPDATE" createUpdate
    , testRun "CREATE NEW ->  EXPIRE" createExpired
    , testRun "CREATE NEW ->  CANCEL" createCancel
    , testRun "CREATE NEW -> UPDATE -> EXPIRE" createUpdateExpired
    , testRun "CREATE NEW -> UPDATE -> CANCEL -> *" createUpdateCancel
    , testRun "SUCCESS SCENARIOS" raffleizeSuccessScenario
    ]

------------------------------------------------------------------------------------------------

-- ** User Actions

------------------------------------------------------------------------------------------------
mintTestsTokenRun :: GYTokenName -> Integer -> GYTxMonadRun GYValue
mintTestsTokenRun tn i = do
  (ac, skeleton) <- mintTestTokens tn (fromInteger i)
  void $ sendSkeleton skeleton
  return $ valueSingleton ac i

createRaffleTXRun :: RaffleConfig -> GYTxMonadRun AssetClass
createRaffleTXRun config = do
  recipient <- ownAddress
  (skeleton, raflleId) <- createRaffleTX recipient config
  void $
    sendSkeleton skeleton `catchError` (error . show)
  return raflleId

buyTicketTXRun :: GYTxOutRef -> SecretHash -> AssetClass -> GYTxMonadRun AssetClass
buyTicketTXRun scriptRef secretHash raffleRefAC = do
  recipient <- ownAddress
  (skeleton, ticketRefAC) <- buyTicketTX secretHash scriptRef recipient raffleRefAC
  void $ sendSkeleton skeleton `catchError` (error . show)
  return ticketRefAC

------------------------------------------------------------------------------------------------

-- ** Raffle Owner Actions

------------------------------------------------------------------------------------------------

updateRaffleTXRun :: GYTxOutRef -> AssetClass -> RaffleConfig -> GYTxMonadRun ()
updateRaffleTXRun scriptRef raffleRefAC newConfig = do
  ownAddrs <- ownAddresses
  skeleton <- updateRaffleTX newConfig scriptRef ownAddrs raffleRefAC
  void $ sendSkeleton skeleton `catchError` (error . show)

cancelRaffleTXRun :: GYTxOutRef -> AssetClass -> GYTxMonadRun ()
cancelRaffleTXRun scriptRef raffleRefAC = do
  recipient <- ownAddress
  ownAddrs <- ownAddresses
  skeleton <- cancelRaffleTX scriptRef ownAddrs recipient raffleRefAC `catchError` (error . show)
  void $ sendSkeleton skeleton `catchError` (error . show)

------------------------------------------------------------------------------------------------

-- ** Ticket Owner Actions

------------------------------------------------------------------------------------------------

revealTicketTXRun :: GYTxOutRef -> GYTxOutRef -> Secret -> AssetClass -> GYTxMonadRun ()
revealTicketTXRun raffleScriptRef ticketScriptRef secret ticketRefAC = do
  recipient <- ownAddress
  skeleton <- revealTicketTX secret raffleScriptRef ticketScriptRef recipient ticketRefAC
  void $ sendSkeleton skeleton `catchError` (error . show)

collectAmountTXRun :: GYTxOutRef -> AssetClass -> GYTxMonadRun ()
collectAmountTXRun scriptRef raffleRefAC = do
  recipient <- ownAddress
  skeleton <- collectAmountTX scriptRef recipient raffleRefAC
  void $ sendSkeleton skeleton `catchError` (error . show)

winnerCollectStakeTXRun :: GYTxOutRef -> GYTxOutRef -> AssetClass -> GYTxMonadRun ()
winnerCollectStakeTXRun raffleScriptRef ticketScriptRef ticketRefAC = do
  recipient <- ownAddress
  skeleton <- winnerCollectStakeTX raffleScriptRef ticketScriptRef recipient ticketRefAC
  void $ sendSkeleton skeleton `catchError` (error . show)

----------------------
-- Run TEST ACTIONS
-----------------------

createRaffleRUN :: Wallet -> RaffleConfig -> Run AssetClass
createRaffleRUN wallet config = do
  raffleId <- runWallet' wallet (createRaffleTXRun config)
  s <- queryRaffleRUN True wallet raffleId
  when (s /= 1) $ logError "not in NEW"
  logInfo' ("CREATED :" ++ show raffleId)
  return raffleId

updateRaffleRUN :: Wallet -> GYTxOutRef -> AssetClass -> RaffleConfig -> Run (GYTxOutRef, AssetClass)
updateRaffleRUN wallet validatorRef raffleId config = do
  runWallet' wallet (updateRaffleTXRun validatorRef raffleId config)
  s <- queryRaffleRUN True wallet raffleId
  when (s /= 1) $ logError "not in NEW"
  logInfo' ("UPDATED :" ++ show raffleId)
  return (validatorRef, raffleId)

cancelRaffleRUN :: Wallet -> GYTxOutRef -> AssetClass -> Run ()
cancelRaffleRUN wallet validatorRef raffleId = do
  runWallet' wallet (cancelRaffleTXRun validatorRef raffleId)
  logInfo' ("CANCELLED :" ++ show raffleId)

deployReferenceScriptRUN :: GYValidator 'PlutusV2 -> Wallet -> GYAddress -> Run GYTxOutRef
deployReferenceScriptRUN validator fromWallet toWallet = do
  valRef <- runWallet' fromWallet $ addRefScript toWallet validator `catchError` (error . show)
  logInfo' "VALIDATOR DEPLOYED"
  case valRef of
    Nothing -> error "failed to add the reference script"
    Just gtor -> return gtor

deployAndCreateRUN :: Wallets -> RaffleConfig -> Run (GYTxOutRef, AssetClass)
deployAndCreateRUN Wallets {..} config = do
  -- 1. Deploy the validator to be used as reference script
  raffleValidatorTxOutRef <- deployReferenceScriptRUN raffleizeValidatorGY w9 (walletAddress w9)
  -- 2. Create the raffle
  raffleId <- createRaffleRUN w1 config
  return (raffleValidatorTxOutRef, raffleId)

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
  let state = evaluateRaffleState (tr, r, v)
  when log $ do
    logInfo (yellowColorString $ "The raffle is in state : " ++ showRaffleStateLabel state)
    logInfo $ yellowColorString $ show r ++ showValue "Raffle State Value" v
  return state

queryTicketRUN :: Wallet -> AssetClass -> Run ()
queryTicketRUN w tid = do
  (r, v) <- runWallet' w $ do
    getTicketStateDataAndValue tid `catchError` (error . show)
  logInfo $ blueColorString $ show r ++ showValue "Ticket State Value" v

mintTestTokensRun :: Wallet -> ByteString -> Integer -> Run Value
mintTestTokensRun w name quantity = do
  val <- runWallet' w $ do
    testTokens <- tokenNameFromPlutus' (tokenName name)
    mintTestsTokenRun testTokens quantity
  let pval = valueToPlutus val
  logInfo' $ showValue "MINTED :" (valueToPlutus val)
  return pval

buyTicketRUN :: GYTxOutRef -> AssetClass -> (Wallet, BuiltinByteString) -> Run AssetClass
buyTicketRUN refRaffleValidator raffleId (wallet, secret) = do
  let secretHash = blake2b_256 secret
  ticketRefAC <- runWallet' wallet $ do
    buyTicketTXRun refRaffleValidator secretHash raffleId
  logInfo' ("BOUGHT :" ++ show ticketRefAC)
  queryTicketRUN wallet ticketRefAC
  return ticketRefAC

buyNTicketsRUN :: GYTxOutRef -> AssetClass -> [Wallet] -> [BuiltinByteString] -> Run [(AssetClass, (Wallet, BuiltinByteString))]
buyNTicketsRUN refRaffleValidator raffleId wallets secrets = do
  let ws = zip wallets secrets
  tickets <- mapM (buyTicketRUN refRaffleValidator raffleId) ws
  s <- queryRaffleRUN True (head wallets) raffleId
  when (s /= 2) $ logError "not in COMMITTING"
  return $ zip tickets ws

revealTicketRUN :: GYTxOutRef -> GYTxOutRef -> (AssetClass, (Wallet, Secret)) -> Run ()
revealTicketRUN refRaffleValidator refTicketValidator (ticketRefAC, (wallet, secret)) = do
  runWallet' wallet $ do
    revealTicketTXRun refRaffleValidator refTicketValidator secret ticketRefAC
  logInfo' ("REVEALED :" ++ show ticketRefAC)

revealNTicketsRUN :: GYTxOutRef -> GYTxOutRef -> AssetClass -> [(AssetClass, (Wallet, Secret))] -> Run ()
revealNTicketsRUN refRaffleValidator refTicketValidator raffleId ws = do
  mapM_ (revealTicketRUN refRaffleValidator refTicketValidator) ws
  s <- queryRaffleRUN True (head (fst . snd <$> ws)) raffleId
  when (s /= 3 && s /= 40) $ logError "not in REVEALIING or SUCCESS_LOCKED_STAKE_AND_AMOUNT"

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Deploy Reference Script -> Create Raffle

------------------------------------------------------------------------------------------------
createNew :: Wallets -> Run (GYTxOutRef, AssetClass)
createNew wallets@Wallets {} = do
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
  deployAndCreateRUN wallets config

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Update

------------------------------------------------------------------------------------------------
createUpdate :: Wallets -> Run (GYTxOutRef, AssetClass)
createUpdate wallets@Wallets {..} = do
  (raffleValidatorTxOutRef, raffleId) <- createNew wallets
  ---3. WAIT
  waitNSlots 3 -- Slot 16
  -- 4. Update the raffle
  sltCfg <- gets (mockConfigSlotConfig . mockConfig)
  let newcddl = slotToEndPOSIXTime sltCfg 20
  let newrddl = slotToEndPOSIXTime sltCfg 26
  let newconfig =
        RaffleConfig
          { rCommitDDL = newcddl
          , rRevealDDL = newrddl
          , rTicketPrice = 10_000_000
          , rMinTickets = 2
          , rStake = valueToPlutus (fakeIron 9876) <> valueToPlutus (fakeGold 9876)
          }
  updateRaffleRUN w1 raffleValidatorTxOutRef raffleId newconfig

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Expired

------------------------------------------------------------------------------------------------

createExpired :: Wallets -> Run (GYTxOutRef, AssetClass)
createExpired wallets@Wallets {..} = do
  (raffleValidatorTxOutRef, raffleId) <- createNew wallets
  ---3. WAIT
  waitNSlots 20 -- Slot 31 - EXPIRED
  s <- queryRaffleRUN True w1 raffleId
  when (s /= 10) $ logError "not in EXPIRED_LOCKED_STAKE"
  return (raffleValidatorTxOutRef, raffleId)

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Cancel Raffle |

------------------------------------------------------------------------------------------------

createCancel :: Wallets -> Run ()
createCancel wallets@Wallets {..} = do
  (raffleValidatorTxOutRef, raffleId) <- createNew wallets

  --   Cancel the raffle
  cancelRaffleRUN w1 raffleValidatorTxOutRef raffleId

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Update -> Expired

------------------------------------------------------------------------------------------------

createUpdateExpired :: Wallets -> Run (GYTxOutRef, AssetClass)
createUpdateExpired wallets@Wallets {..} = do
  (raffleValidatorTxOutRef, raffleId) <- createUpdate wallets
  ---3. WAIT
  waitNSlots 20 -- Slot 31 - EXPIRED
  s <- queryRaffleRUN True w1 raffleId
  when (s /= 10) $ logError "not in EXPIRED_LOCKED_STAKE"
  return (raffleValidatorTxOutRef, raffleId)

------------------------------------------------------------------------------------------------

-- ** SCENARIO: Deploy Reference Script -> Create Raffle -> Update -> Cancel |

------------------------------------------------------------------------------------------------
createUpdateCancel :: Wallets -> Run ()
createUpdateCancel wallets@Wallets {..} = do
  (raffleValidatorTxOutRef, raffleId) <- createUpdate wallets
  cancelRaffleRUN w1 raffleValidatorTxOutRef raffleId

----------------------
-- SUCCESS SCENARIO
-----------------------

raffleizeSuccessScenario :: Wallets -> Run ()
raffleizeSuccessScenario wallets@Wallets {..} = do
  -- Deploy the validator to be used as reference script
  refTicketValidator <- deployReferenceScriptRUN ticketValidatorGY w9 (walletAddress w9)
  (refRaffleValidator, raffleId) <- createNew wallets

  -- Buy ticket to raffle
  ws <- buyNTicketsRUN refRaffleValidator raffleId [w1, w2, w3, w4] ["unu", "doi", "trei", fromString @BuiltinByteString "84a289f6f0dc3d1e18dcac4687604d7184a289f6f0dc3d1e18dcac4687604d71"]

  waitNSlots 4
  -- Revealing tickets
  revealNTicketsRUN refRaffleValidator refTicketValidator raffleId (take 3 ws)
  revealNTicketsRUN refRaffleValidator refTicketValidator raffleId (drop 3 ws)
  mapM_ (queryTicketRUN w1) $ fst <$> ws

  _ <- runWallet' w1 $ do
    collectAmountTXRun refRaffleValidator raffleId
  logInfo' ("RAFFLE OWNER COLLECTED AMOUNT :" ++ show raffleId)

  void $ queryRaffleRUN True w1 raffleId

  _ <- runWallet' w1 $ do
    winnerCollectStakeTXRun refRaffleValidator refTicketValidator (head (fst <$> ws))
  logInfo' ("TICKET OWNER REDEEM STAKE :" ++ show raffleId)

  s <- queryRaffleRUN True w1 raffleId
  when (s /= 43) $ logError "not in SUCCESS_FINAL"

---------------------
------------------------
------------------------
------------------------
------------------------
------------------------
------------------------
------------------------
------------------------

logInfo' :: String -> Run ()
logInfo' s = logInfo $ greenColorString s

greenColorString :: String -> String
greenColorString s =
  "\n"
    ++ "\ESC[1;32m"
    ++ replicate 100 '='
    ++ "\n"
    ++ s
    ++ "\n"
    ++ replicate 100 '='
    ++ "\ESC[0m"
    ++ "\n"

yellowColorString :: String -> String
yellowColorString s =
  "\n"
    ++ "\ESC[1;93m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

blueColorString :: String -> String
blueColorString s =
  "\n"
    ++ "\ESC[1;94m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

unitTests :: TestTree
unitTests = testGroup "CreateRaffles" [createRaffleTests]

--- >>> lengthOfByteString "5b39bfccb1447d4aae30e7a4fb0f4ba37e79ea96ec54b5ba7223979a15e4d0ae5b39bfccb1447d4aae30e7a4fb0f4ba37e79ea96ec54b5ba7223979a15e4d0ae=====------------------------"
-- 157
-- TODO DE ANALIZAT

--- >>> lengthOfByteString $ blake2b_256 "marius"
-- 32
