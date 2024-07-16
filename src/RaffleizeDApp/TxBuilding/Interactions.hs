module RaffleizeDApp.TxBuilding.Interactions where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Either.Extra

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Operations

interactionToTxSkeleton ::
  (HasCallStack, GYTxMonad m, GYTxQueryMonad m, MonadReader RaffleizeTxBuildingContext r) =>
  RaffleizeInteraction ->
  r (m (GYTxSkeleton 'PlutusV2, AssetClass))
interactionToTxSkeleton
  RaffleizeInteraction {..} = do
    raffleValidatorRef <- asks raffleValidatorRef
    ticketValidatorRef <- asks ticketValidatorRef
    let changeAddr = changeAddress userAddresses
    let usedAddrs = usedAddresses userAddresses
    let receiveAddr = fromMaybe changeAddr recipient
    return $ case raffleizeAction of
      User userAction -> case userAction of
        CreateRaffle raffleConfig -> createRaffleTX receiveAddr raffleConfig
        BuyTicket secretHash -> do
          contextNFT <- liftEither $ maybeToEither (GYApplicationException MissingContextNFT) interactionContextNFT
          buyTicketTX secretHash raffleValidatorRef receiveAddr contextNFT
      other -> do
        contextNFT <- liftEither $ maybeToEither (GYApplicationException MissingContextNFT) interactionContextNFT
        (,contextNFT) <$> case other of
          TicketOwner ticketOwnerAction -> case ticketOwnerAction of
            RevealTicketSecret secret -> revealTicketTX secret raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            CollectStake -> winnerCollectStakeTX raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            RefundTicket -> fullRefundTicketTX raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            RefundTicketExtra -> extraRefundTicketTX raffleValidatorRef ticketValidatorRef receiveAddr contextNFT
            RefundCollateralLosing -> refundCollateralOfLosingTicketTX ticketValidatorRef receiveAddr contextNFT
          RaffleOwner raffleOwnerAction -> case raffleOwnerAction of
            Update newRaffleConfig -> updateRaffleTX receiveAddr newRaffleConfig raffleValidatorRef usedAddrs contextNFT
            Cancel -> cancelRaffleTX raffleValidatorRef usedAddrs receiveAddr contextNFT
            RecoverStake -> recoverStakeTX raffleValidatorRef receiveAddr contextNFT
            RecoverStakeAndAmount -> recoverStakeAndAmountTX raffleValidatorRef receiveAddr contextNFT
            CollectAmount -> collectAmountTX raffleValidatorRef receiveAddr contextNFT
            GetCollateralOfExpiredTicket -> getCollateralOfExpiredTicketTX ticketValidatorRef usedAddrs receiveAddr contextNFT
          Admin CloseRaffle -> undefined -- TODO
