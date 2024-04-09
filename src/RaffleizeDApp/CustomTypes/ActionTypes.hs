module RaffleizeDApp.CustomTypes.ActionTypes where

import PlutusLedgerApi.V1.Value
import PlutusTx
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes

-------------------------------------------------------------------------------

-- * Action Type  Declarations

-------------------------------------------------------------------------------

-- | Datatype representng the actions that can be peformed by any user.
data UserAction
  = CreateRaffle RaffleConfig
  | BuyTicket
      SecretHash --- ^ The hash of the secret to be used in commt-reveal-schemme.
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

unstableMakeIsData ''UserAction --- TODO must be changed with stable version

-- | Datatype representng the actions that can be peformed by Ticket Owner.
data TicketOwnerAction
  = RevealTicketSecret
      Secret --- ^ The revealed secret (it's hash should match the one stored in the ticket ref NFT datum).
  | CollectStake
  | RefundTicket
  | RefundTicketExtra
  | RefundCollateralLosing
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

unstableMakeIsData ''TicketOwnerAction --- TODO must be changed with stable version

-- | Datatype representng the actions that can be peformed by Raffle Owner.
data RaffleOwnerAction
  = Update
      RaffleConfig --- ^ The raffle configuration.
  | Cancel
  | RecoverStake
  | RecoverStakeAndAmount
  | CollectAmount
  | GetCollateraOfExpiredTicket
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

unstableMakeIsData ''RaffleOwnerAction --- TODO must be changed with stable version

-- | Datatype representng the actions that can be peformed by the Admin.
data AdminAction = CloseRaffle ---
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

unstableMakeIsData ''AdminAction --- TODO must be changed with stable version

{- | Datatype representng the actions supported by the Raffleize DApp.
This datatype is used as "Redeemer" for the validation logic for updating both raffle and tickets states.
-}
data RaffleizeAction
  = User
      UserAction --- ^ Action that can be peformed by any user.
  | TicketOwner
      TicketOwnerAction --- ^ Action that can be peformed by Ticket Owner.
      -- AssetClass --- ^ The ticket id (ticket ref. NFT @AssetClass@).
  | RaffleOwner
      RaffleOwnerAction --- ^ Action that can be peformed by Raffle Owner.
  | Admin
      AdminAction --- ^ Action that can be peformed by Admin.
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

unstableMakeIsData ''RaffleizeAction --- TODO must be changed with stable version

{- | Datatype representng the actions supported by the Raffleize DApp.
This datatype is used as "Redeemer" for the validation logic for updating both raffle and tickets states.
-}
data RaffleizeRedeemer
  = UserRedeemer
      UserAction --- ^ Action that can be peformed by any user.
  | TicketOwnerRedeemer
      TicketOwnerAction --- ^ Action that can be peformed by Ticket Owner.
      AssetClass --- ^ The ticket id (ticket ref. NFT @AssetClass@).
  | RaffleOwnerRedeemer
      RaffleOwnerAction --- ^ Action that can be peformed by Raffle Owner.
  | AdminRedeemer
      AdminAction --- ^ Action that can be peformed by Admin.
  deriving (Generic, Eq, ToJSON, FromJSON)

unstableMakeIsData ''RaffleizeRedeemer --- TODO must be changed with stable version