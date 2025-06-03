module RaffleizeDApp.TxBuilding.Interactions where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Either.Extra
import GHC.Stack
import GeniusYield.TxBuilder hiding (User)
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.OnChain.NFT
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Operations

interactionToTxSkeleton ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader RaffleizeTxBuildingContext m) =>
  Interaction ->
  m (GYTxSkeleton 'PlutusV3, AssetClass)
interactionToTxSkeleton Interaction {..} = do
  let changeAddr = changeAddress userAddresses
  let usedAddrs = usedAddresses userAddresses
  let receiveAddr = fromMaybe changeAddr recipient
  case action of
    RaffleizeInteraction raffleizeAction -> case raffleizeAction of
      User userAction -> case userAction of
        CreateRaffle raffleConfig -> createRaffleTX receiveAddr raffleConfig
        BuyTicket secretHash -> do
          contextNFT <- liftEither $ maybeToEither (GYApplicationException MissingContextNFT) interactionContextNFT
          buyTicketTX secretHash receiveAddr contextNFT
      other -> do
        contextNFT <- liftEither $ maybeToEither (GYApplicationException MissingContextNFT) interactionContextNFT
        (,contextNFT) <$> case other of
          TicketOwner ticketOwnerAction -> case ticketOwnerAction of
            RevealTicketSecret secret -> revealTicketTX secret receiveAddr contextNFT
            CollectStake -> winnerCollectStakeTX receiveAddr contextNFT
            RefundTicket -> fullRefundTicketTX receiveAddr contextNFT
            RefundTicketExtra -> extraRefundTicketTX receiveAddr contextNFT
            RefundCollateralLosing -> refundCollateralOfLosingTicketTX receiveAddr contextNFT
          RaffleOwner raffleOwnerAction -> case raffleOwnerAction of
            Update newRaffleConfig -> updateRaffleTX receiveAddr newRaffleConfig usedAddrs contextNFT
            Cancel -> cancelRaffleTX usedAddrs receiveAddr contextNFT
            RecoverStake -> recoverStakeTX receiveAddr contextNFT
            RecoverStakeAndAmount -> recoverStakeAndAmountTX receiveAddr contextNFT
            CollectAmount -> collectAmountTX receiveAddr contextNFT
            GetCollateralOfExpiredTicket -> getCollateralOfExpiredTicketTX usedAddrs receiveAddr contextNFT
          Admin CloseRaffle -> adminCloseRaffleTX contextNFT receiveAddr
    NFTInteraction nftAction -> case nftAction of
      MintingNFT td -> mintNFTTX changeAddr td
      BurningNFT -> undefined