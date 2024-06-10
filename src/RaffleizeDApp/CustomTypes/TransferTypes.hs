module RaffleizeDApp.CustomTypes.TransferTypes where

import Data.Aeson
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.ActionTypes
import Prelude

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
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToJSON GYTxOutRefCbor where
  toJSON = toJSON . getTxOutRefHex

data UserAddresses = UserAddresses
  { usedAddresses :: [GYAddress]
  -- ^ User's used addresses.
  , changeAddress :: GYAddress
  -- ^ User's change address.
  , reservedCollateral :: Maybe GYTxOutRefCbor
  -- ^ Browser wallet's reserved collateral (if set).
  }
  deriving (Show, Generic, FromJSON, ToJSON)
