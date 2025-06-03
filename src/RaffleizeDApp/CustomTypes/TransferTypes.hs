{-# LANGUAGE DeriveAnyClass #-}

module RaffleizeDApp.CustomTypes.TransferTypes where

import Data.Aeson hiding (Value)
import Data.Swagger.Internal.Schema
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3 (POSIXTimeRange, to)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.OnChain.NFT (NFTAction)
import RaffleizeDApp.OnChain.RaffleizeLogic
import Prelude

-- | Input parameters to add for reference script.
data AddWitAndSubmitParams = AddWitAndSubmitParams
  { awasTxUnsigned :: !GYTx,
    awasTxWit :: !GYTxWitness
  }
  deriving (Generic, FromJSON, ToSchema)

-- | @TicketStateLabel@ is a synonym for @String@
type TicketStateLabel = String

-- | Ticket information DTO
data TicketInfo = TicketInfo
  { tiTsd :: TicketStateData,
    tiValue :: Value,
    tiImage :: String,
    tiStateLabel :: TicketStateLabel,
    tiAvailableActions :: [RaffleizeActionLabel],
    tiTicketId :: AssetClass
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Raffle information DTO
data RaffleInfo = RaffleInfo
  { riRsd :: RaffleStateData,
    riValue :: Value,
    riImage :: String,
    riStateLabel :: String,
    riAvailableActions :: [RaffleizeActionLabel]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

mkRaffleInfo :: POSIXTimeRange -> (RaffleStateData, Value, String) -> RaffleInfo
mkRaffleInfo tr (rsd, rVal, img) =
  let raffleStateId = evaluateRaffleState (tr, rsd, rVal)
      stateLabel = show raffleStateId
      actions = validActionLabelsForRaffleState raffleStateId
   in RaffleInfo rsd rVal img stateLabel actions

mkTicketInfo :: RaffleStateId -> Integer -> (TicketStateData, Value, String) -> TicketInfo
mkTicketInfo raffleStateId currentRandom (tsd, tVal, tImg) =
  let ticketStateId = evalTicketState tsd currentRandom raffleStateId
      ticketStateLabel = show ticketStateId
      actions = validActionLabelsForTicketState ticketStateId
   in TicketInfo tsd tVal tImg ticketStateLabel actions (fst $ generateTicketACFromTicket tsd)

data InteractionAction = NFTInteraction NFTAction | RaffleizeInteraction RaffleizeAction
  deriving (Show, Generic, FromJSON, ToJSON)

data Interaction
  = Interaction
  { -- | The @AssetClass@ of the Raffle or Ticket the ticket which are in scope of the interaction (if set).
    interactionContextNFT :: Maybe AssetClass,
    -- | The @RaffleizeAction@ is the intented action to perfrom.
    action :: InteractionAction,
    -- | The user addresses to be used as input for transaction building.
    userAddresses :: UserAddresses,
    -- | If the interaction unlocks some funds, the funds will be sent to this address (if set, otherwise to the change address).
    recipient :: Maybe GYAddress
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- instance ToJSON GYTxOutRefCbor where
--   toJSON = toJSON . getTxOutRefHex

data UserAddresses = UserAddresses
  { -- | User's used addresses.
    usedAddresses :: [GYAddress],
    -- | User's change address.
    changeAddress :: GYAddress,
    -- | Browser wallet's reserved collateral (if set).
    reservedCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

instance ToSchema RaffleInfo where
  declareNamedSchema _ = plain $ sketchSchema @RaffleInfo sampleRaffleInfo

instance ToSchema TicketInfo where
  declareNamedSchema _ = plain $ sketchSchema @TicketInfo sampleTicketInfo

instance ToSchema Interaction where
  declareNamedSchema _ = plain $ sketchSchema @Interaction $ Interaction (Just sampleAssetClass) (RaffleizeInteraction $ RaffleOwner Cancel) (UserAddresses [sampleAddr] sampleAddr (Just sampleGYTxOutRefCbor)) (Just sampleAddr)

-------------------------------------------------------------------------------

-- * Example Values

-------------------------------------------------------------------------------

samplePOSIXTimeRange :: POSIXTimeRange
samplePOSIXTimeRange = to 1718887778870

sampleValue :: Value
sampleValue = mempty

sampleURL :: String
sampleURL = "ipfs://notarealurl"

sampleGYTxOutRefCbor :: GYTxOutRefCbor
sampleGYTxOutRefCbor = GYTxOutRefCbor ("4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: GYTxOutRef)

sampleAddr :: GYAddress
sampleAddr = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"

sampleAssetClass :: AssetClass
sampleAssetClass = AssetClass (CurrencySymbol "RaffleizeCurrencySymbol", TokenName "0xRaffleizeTokenName")

sampleRaffleConfig :: RaffleConfig
sampleRaffleConfig =
  RaffleConfig
    { rCommitDDL = 1718887778871,
      rRevealDDL = 1718889578871,
      rTicketPrice = 7_000_000,
      rMinTickets = 2,
      rStake = sampleValue
    }

sampleRaffleParam :: RaffleParam
sampleRaffleParam =
  RaffleParam
    { rMaxNoOfTickets = 20,
      rMinRevealingWindow = 6_000, --- ^ Milliseconds
      rMinTicketPrice = 3_000_000, --- ^ Lovelaces
      rMinNotClosingWindow = 30, --- ^ Days
      rRaffleValidatorHash = "ef370a98174dfad64f4447839c780af1b886d021c06496bd4e8c5013",
      rTicketValidatorHash = "ba339e84d13bd665767dd223380f074d1309785b94da8bf13f7052fd",
      rTicketCollateral = 3_500_000, --- ^ Lovelaces
      rRaffleCollateral = 30_000_000 --- ^ Lovelaces
    }

sampleRaffleStateData :: RaffleStateData
sampleRaffleStateData =
  RaffleStateData
    { rRaffleID = sampleAssetClass,
      rParam = sampleRaffleParam,
      rConfig = sampleRaffleConfig,
      rSoldTickets = 0,
      rRevealedTickets = 0,
      rRefundedTickets = 0,
      rRandomSeed = 0
    }

sampleTicketStateData :: TicketStateData
sampleTicketStateData =
  TicketStateData
    { tNumber = 0,
      tSecretHash = SecretHash "fb0721e63e74fd787fe56b179824fb24e51ae0a7a10103a588b606f56f0ee2fc",
      tSecret = Just "6D6172697573",
      tRaffle = sampleAssetClass,
      tRaffleValidator = "ef370a98174dfad64f4447839c780af1b886d021c06496bd4e8c5013"
    }

sampleRaffleInfo :: RaffleInfo
sampleRaffleInfo = mkRaffleInfo samplePOSIXTimeRange (sampleRaffleStateData, sampleValue, sampleURL)

sampleTicketInfo :: TicketInfo
sampleTicketInfo = mkTicketInfo REVEALING 0 (sampleTicketStateData, sampleValue, sampleURL)