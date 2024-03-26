module RaffleizeDApp.TxBuilding.Interactions where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Either.Extra

import Data.Bifunctor (second)
import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Operations

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
  deriving (Show, Generic, ToJSON, FromJSON)

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