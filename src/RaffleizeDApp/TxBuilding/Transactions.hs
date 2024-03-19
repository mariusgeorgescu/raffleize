module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (second))
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.GYConfig
import GeniusYield.Types
import GeniusYield.Types.Key.Class

import GHC.Stack
import GeniusYield.TxBuilder

import Control.Exception
import Control.Monad.Error.Class
import Data.Either.Extra (maybeToEither)
import GeniusYield.HTTP.Errors
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

data MissingContextNFT = MissingContextNFT deriving (Show, Typeable)
instance Exception MissingContextNFT
instance IsGYApiError MissingContextNFT

interactionToTxSkeleton ::
  (HasCallStack, GYTxMonad m, GYTxQueryMonad m, MonadReader RaffleizeTxBuildingContext r) =>
  RaffleizeInteraction ->
  r (m (GYTxSkeleton 'PlutusV2, Maybe AssetClass))
interactionToTxSkeleton
  RaffleizeInteraction {..} = do
    raffleValidatorRef <- asks raffleValidatorRef
    ticketValidatorRef <- asks ticketValidatorRef
    let changeAddr = changeAddress userAddresses
    let usedAddrs = usedAddresses userAddresses
    let receiveAddr = fromMaybe changeAddr recipient
    return $ case raffleizeAction of
      User userAction ->
        second Just <$> case userAction of
          CreateRaffle raffleConfig -> createRaffleTX receiveAddr raffleConfig
          BuyTicket secretHash -> do
            contextNFT <- liftEither $ maybeToEither (GYApplicationException MissingContextNFT) interactionContextNFT
            buyTicketTX secretHash raffleValidatorRef receiveAddr contextNFT
      other -> do
        contextNFT <- liftEither $ maybeToEither (GYApplicationException MissingContextNFT) interactionContextNFT
        (,Nothing) <$> case other of
          TicketOwner ticketOwnerAction -> case ticketOwnerAction of
            RevealTicketSecret secret -> revealTicketTX secret raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            CollectStake -> winnerCollectStakeTX raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            RefundTicket -> fullRefundTicketTX raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            RefundTicketExtra -> extraRefundTicketTX raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            RefundCollateralLosing -> refundCollateralOfLosingTicketTX raffleValidatorRef receiveAddr contextNFT
          RaffleOwner raffleOwnerAction -> case raffleOwnerAction of
            Update newRaffleConfig -> updateRaffleTX newRaffleConfig raffleValidatorRef usedAddrs contextNFT
            Cancel -> cancelRaffleTX raffleValidatorRef usedAddrs receiveAddr contextNFT
            RecoverStake -> recoverStakeTX raffleValidatorRef receiveAddr contextNFT
            RecoverStakeAndAmount -> recoverStakeAndAmountTX raffleValidatorRef receiveAddr contextNFT
            CollectAmount -> collectAmountTX raffleValidatorRef receiveAddr contextNFT
            GetCollateraOfExpiredTicket -> getCollateralOfExpiredTicketTX ticketValidatorRef usedAddrs receiveAddr contextNFT
          Admin CloseRaffle -> undefined -- TODO

interactionToTxBody :: (MonadReader RaffleizeTxBuildingContext r) => RaffleizeInteraction -> r (ReaderT Ctx IO GYTxBody)
interactionToTxBody interaction@RaffleizeInteraction {userAddresses} = do
  raffleizeTxBuildingContext <- ask
  let skeleton = runReader (interactionToTxSkeleton interaction) raffleizeTxBuildingContext
  return $ runTxI userAddresses (fst <$> skeleton)

interactionToUnsignedTx :: (MonadReader RaffleizeTxBuildingContext r) => RaffleizeInteraction -> r (ReaderT Ctx IO GYTx)
interactionToUnsignedTx = (fmap unsignedTx <$>) . interactionToTxBody

interactionToHexEncodedCBOR :: (MonadReader RaffleizeTxBuildingContext r) => RaffleizeInteraction -> r (ReaderT Ctx IO String)
interactionToHexEncodedCBOR = (fmap txToHex <$>) . interactionToUnsignedTx

-----------------
-----------------
-----------------
-----------------
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
buildMintTestTokensTx :: GYPaymentSigningKey -> ReaderT Ctx IO GYTxBody
buildMintTestTokensTx skey = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) $ snd <$> mintTestTokens "teststake" 100

--------------------------
--------------------------
--------------------------

-- | Build a transaction for creating a new raffle.
buildCreateRaffleTx :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO GYTxBody
buildCreateRaffleTx skey raffleConfiguration = do
  my_addr <- queryGetAddressFromSkey skey
  let useraddrs = UserAddresses [my_addr] my_addr Nothing
  let createRaffleInteraction = RaffleizeInteraction Nothing (User (CreateRaffle raffleConfiguration)) useraddrs Nothing
  runReader (interactionToTxBody createRaffleInteraction) undefined

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
