module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader

import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.GYConfig
import GeniusYield.Types
import GeniusYield.Types.Key.Class

import GHC.Stack
import GeniusYield.TxBuilder
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.RaffleizeOperations

data RaffleizeInteraction = RaffleizeInteraction
  { interactionContextNFT :: Maybe AssetClass
  -- ^ The @AssetClass@ of the Raffle or Ticket the ticket which are in scope of the interaction (if set).
  , raffleizeAction :: RaffleizeAction
  -- ^ The @RaffleizeAction@ is the intented action to perfrom.
  , userAddresses :: UserAddresses
  -- ^ The user addresses to be used as input for transaction building.
  , recipient :: Maybe GYAddress
  -- ^ If the interaction unlocks some funds, the funds will be sent to this address (if set, otherwise to the change address).
  }

data RaffleizeTxBuildingContext = RaffleizeTxBuildingContext
  { raffleValidatorRef :: GYTxOutRef
  , ticketValidatorRef :: GYTxOutRef
  }

actionToTxSkeleton :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => RaffleizeTxBuildingContext -> RaffleizeInteraction -> m (GYTxSkeleton 'PlutusV2)
actionToTxSkeleton
  RaffleizeTxBuildingContext {raffleValidatorRef, ticketValidatorRef}
  RaffleizeInteraction {interactionContextNFT, raffleizeAction, userAddresses} = do
    let contextNFT = fromJust interactionContextNFT
    let userUsedAddresses = usedAddresses userAddresses
    let userChangeAddress = changeAddress userAddresses
    let withTicket f = f ticketValidatorRef userUsedAddresses userChangeAddress contextNFT
    let withTicket' f = f ticketValidatorRef userChangeAddress contextNFT
    let withRaffle f = f raffleValidatorRef userUsedAddresses userChangeAddress contextNFT
    let withRaffle'' f = f raffleValidatorRef userUsedAddresses contextNFT
    let withRaffle' f = f raffleValidatorRef userChangeAddress contextNFT
    let withRaffleAndTicket f = f raffleValidatorRef ticketValidatorRef userChangeAddress contextNFT
    case raffleizeAction of
      User userAction -> case userAction of
        CreateRaffle raffleConfig -> fst <$> createRaffleTX userChangeAddress raffleConfig
        BuyTicket secretHash -> fst <$> buyTicketTX secretHash raffleValidatorRef userChangeAddress contextNFT
      TicketOwner ticketOwnerAction  -> case ticketOwnerAction of
        RevealTicketSecret secret -> withRaffleAndTicket (revealTicketTX secret)
        CollectStake -> withRaffleAndTicket winnerCollectStakeTX
        RefundTicket -> withRaffleAndTicket fullRefundTicketTX
        RefundTicketExtra -> withRaffleAndTicket extraRefundTicketTX
        RefundCollateralLosing -> withTicket' refundCollateralOfLosingTicketTX
      RaffleOwner raffleOwnerAction -> case raffleOwnerAction of
        Update newRaffleConfig -> withRaffle'' (updateRaffleTX newRaffleConfig)
        Cancel -> withRaffle cancelRaffleTX
        RecoverStake -> withRaffle' recoverStakeTX
        RecoverStakeAndAmount -> withRaffle' recoverStakeAndAmountTX
        CollectAmount -> withRaffle' collectAmountTX
        GetCollateraOfExpiredTicket -> withTicket getCollateralOfExpiredTicketTX
      Admin CloseRaffle -> undefined -- TODO

-- actionToTxBody :: RaffleizeTxBuildingContext -> RaffleizeAction -> ReaderT Ctx IO GYTxBody
-- actionToTxBody txCtx@RaffleizeTxBuildingContext {userUsedAddresses, userChangeAddress} usecase = do
--   runTxI userUsedAddresses userChangeAddress Nothing (actionToTxSkeleton txCtx usecase)

-- actionToUnsignedTx :: RaffleizeTxBuildingContext -> RaffleizeAction -> ReaderT Ctx IO GYTx
-- actionToUnsignedTx raffleizeTxContext usecase = unsignedTx <$> actionToTxBody raffleizeTxContext usecase

-- actionToHexEncodedCBOR :: RaffleizeTxBuildingContext -> RaffleizeAction -> ReaderT Ctx IO String
-- actionToHexEncodedCBOR raffleizeTxContext usecase = txToHex <$> actionToUnsignedTx raffleizeTxContext usecase

-----------------

submitTxBody :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader Ctx m) => a -> m GYTxBody -> m ()
submitTxBody skey m = do
  txBody <- m
  ctxProviders <- asks ctxProviders
  tid <- liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]
  liftIO $ printf "submitted tx: %s\n" tid

queryGetAddressFromSkey :: GYPaymentSigningKey -> ReaderT Ctx IO GYAddress
queryGetAddressFromSkey skey = do
  nid <- asks (cfgNetworkId . ctxCoreCfg)
  runQuery $ do
    let pub_key = paymentVerificationKey skey
        pub_key_hash = pubKeyHash pub_key
        address = addressFromPubKeyHash nid pub_key_hash
    return address

queryGetAddressFromSkeyFile :: FilePath -> ReaderT Ctx IO ()
queryGetAddressFromSkeyFile skey_file = do
  skey <- liftIO $ readPaymentSigningKey skey_file
  addr <- queryGetAddressFromSkey skey
  liftIO $ printf "Address: %s" (show addr)

-- | Build a transaction for creating a new raffle.
buildCreateRaffleTx :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO GYTxBody
buildCreateRaffleTx skey raffleConfiguration = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) (fst <$> createRaffleTX my_addr raffleConfiguration)

-- | Build a transaction for creating a new raffle.
buildMintTestTokensTx :: GYPaymentSigningKey -> ReaderT Ctx IO GYTxBody
buildMintTestTokensTx skey = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) $ snd <$> mintTestTokens "teststake" 100

--------------------------
--------------------------
--------------------------

createRaffleTransaction :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO ()
createRaffleTransaction skey raffle_config = do
  submitTxBody skey $ buildCreateRaffleTx skey raffle_config

mintTestTokensTransaction :: GYPaymentSigningKey -> ReaderT Ctx IO ()
mintTestTokensTransaction skey = do
  submitTxBody skey $ buildMintTestTokensTx skey

-----------------------
-----------------------
-----------------------
-----------------------
-----------------------
