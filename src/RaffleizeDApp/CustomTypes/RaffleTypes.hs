module RaffleizeDApp.CustomTypes.RaffleTypes where

import Data.Aeson hiding (Value)
import Data.Aeson qualified (Value)
import Data.Aeson.Types qualified as Data.Aeson.Types.Internal
import Data.String

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import PlutusTx
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.OnChain.Utils

-------------------------------------------------------------------------------

-- * Raffle Type  Declarations

-------------------------------------------------------------------------------

{- | Raffle configuration (
These attributes should be defined by the Raffle Owner when creating/updating a raffle.
-}
data RaffleConfig = RaffleConfig
  { rCommitDDL :: POSIXTime --- ^ The deadline for buying tickets and committing the secrret hash.
  , rRevealDDL :: POSIXTime --- ^ The deadline for reavealing the secrets.
  , rTicketPrice :: Integer --- ^ The ticket price (expressed in lovelaces).
  , rMinTickets :: Integer --- ^ The minimum number of tickets that must be sold for the raffle.
  , rStake :: Value --- ^ The raffle stake value.
  }
  deriving (Generic, Eq, ToJSON, FromJSON)

instance ToJSON BuiltinByteString where
  toJSON :: BuiltinByteString -> Data.Aeson.Value
  toJSON bs = toJSON $ fromBuiltin (decodeUtf8 bs)

instance FromJSON BuiltinByteString where
  parseJSON :: Data.Aeson.Value -> Data.Aeson.Types.Internal.Parser BuiltinByteString
  parseJSON v = encodeUtf8 . (toBuiltin @Text @BuiltinString) <$> parseJSON @Text v

instance ToJSON POSIXTime where
  toJSON :: POSIXTime -> Data.Aeson.Value
  toJSON (POSIXTime i) = toJSON i

instance FromJSON POSIXTime where
  parseJSON :: Data.Aeson.Value -> Data.Aeson.Types.Internal.Parser POSIXTime
  parseJSON v = POSIXTime <$> parseJSON @Integer v

instance ToJSON TokenName where
  toJSON :: TokenName -> Data.Aeson.Value
  toJSON tn = object ["TokenName" .= toString tn]

instance FromJSON TokenName where
  parseJSON :: Data.Aeson.Types.Internal.Value -> Data.Aeson.Types.Internal.Parser TokenName
  parseJSON = withObject "TokenName" $ \v -> fromString @TokenName <$> v .: "TokenName"

instance ToJSON CurrencySymbol where
  toJSON :: CurrencySymbol -> Data.Aeson.Value
  toJSON cs = object ["CurrencySymbol" .= show cs]

instance FromJSON CurrencySymbol where
  parseJSON :: Data.Aeson.Types.Internal.Value -> Data.Aeson.Types.Internal.Parser CurrencySymbol
  parseJSON = withObject "CurrencySymbol" $ \v -> fromString @CurrencySymbol <$> v .: "CurrencySymbol"

instance ToJSON Value where
  toJSON :: Value -> Data.Aeson.Value
  toJSON value = toJSON $ flattenValue value

instance FromJSON Value where
  parseJSON :: Data.Aeson.Types.Internal.Value -> Data.Aeson.Types.Internal.Parser Value
  parseJSON v =
    let flattenedValue = parseJSON @[(CurrencySymbol, TokenName, Integer)] v
     in unFlattenValue <$> flattenedValue

instance ToJSON AssetClass where
  toJSON :: AssetClass -> Data.Aeson.Value
  toJSON (AssetClass ac) = toJSON ac

instance ToJSON ScriptHash where
  toJSON :: ScriptHash -> Data.Aeson.Value
  toJSON (ScriptHash s) = toJSON s

instance FromJSON AssetClass where
  parseJSON :: Data.Aeson.Types.Internal.Value -> Data.Aeson.Types.Internal.Parser AssetClass
  parseJSON v =
    let ac = parseJSON @(CurrencySymbol, TokenName) v
     in AssetClass <$> ac

unstableMakeIsData ''RaffleConfig --- TODO must be changed with stable version

{- | Raffle parameters
These attributes should be defined by the Protocol Admin.
-}
data RaffleParam = RaffleParam
  { rMaxNoOfTickets :: Integer --- ^ The maximum number of tickets that can be sold for the raffle.
  , rMinRevealingWindow :: Integer --- ^ The minimum no. of milliseconds that must pass between commit deadline and reveal deadline.
  , rMinTicketPrice :: Integer --- ^ The minimum ticket price (expressed in lovelaces).
  , rRaffleValidatorHash :: ScriptHash --- ^ The validator hash of the validation logic for spending the raffle state UTxO.
  , rTicketValidatorHash :: ScriptHash --- ^ The validator hash of the validation logic for spending the ticket state UTxO.
  , rTicketCollateral :: Integer --- ^ The min. no. of lovelaces that must be locked with the ticket state (recovered when ticket ref NFT is burned).
  , rRaffleCollateral :: Integer --- ^ The min. no. of lovelaces that must be locked with the raffle state (recovered when raffle ref. NFT is burned).
  }
  deriving (Generic, Eq, ToJSON)

unstableMakeIsData ''RaffleParam --- TODO must be changed with stable version

makeLift ''RaffleParam --  generating Lift instance with TH

{- | Raffle State Data
This datatype is part of the RAFFLE STATE.
The RAFFLE STATE is defined by the the @RaffleStateData@, the UTxO Value and the @txInfoValidRange@.
-}
data RaffleStateData = RaffleStateData
  { rRaffleID :: AssetClass --- ^ The raffle id is the raffle ref NFT @AssetClass@ (since it is unique).
  , rParam :: RaffleParam --- ^  Raffle parameeters defined by the Protocol Admin.
  , rConfig :: RaffleConfig --- ^  Raffle configuration defined by the Raffle Owner.
  , rSoldTickets :: Integer --- ^  The current number of tickets sold.
  , rRevealedTickets :: Integer --- ^  The current number of tickets revealed.
  , rRefundedTickets :: Integer --- ^  The current number of tickets refunded.
  , rRandomSeed :: Integer --- ^  The current accumulated random seed (is valid only when all tickets sold are revealed).
  }
  deriving (Generic, ToJSON)

unstableMakeIsData ''RaffleStateData --- TODO must be changed with stable version

-- | Functon to ceate a @RaffleStateData@, of a new raffle.
mkNewRaffle :: AssetClass -> RaffleParam -> RaffleConfig -> RaffleStateData
mkNewRaffle raffleId param config = RaffleStateData raffleId param config 0 0 0 0
{-# INLINEABLE mkNewRaffle #-}

{- | The datum datatype which should be locked with raffle ref NFT.
| This datatype is following the CIP-68 Datum Metadata Standard.
-}
data RaffleDatum = RaffleDatum
  { metadata :: Metadata --- ^  Map k v (where k and v arr  UTF-8 encoded @BuiltinByteString@s)
  , version :: Integer --- ^ version of CIP-68 Datum Metadata Standard.
  , extra :: RaffleStateData --- ^ Plutus data
  }
  deriving (Generic)

unstableMakeIsData ''RaffleDatum --- TODO must be changed with stable version

-- | Functon to get the @RaffleStateData@ from @RaffleDatum@.
raffleStateData :: RaffleDatum -> RaffleStateData
raffleStateData = extra
{-# INLINEABLE raffleStateData #-}

-- | Functon to construct a @RaffleDatum@, from a  @RaffleStateData@.
mkRaffleDatum :: RaffleStateData -> RaffleDatum
mkRaffleDatum rsd =
  RaffleDatum
    { metadata =
        fromList $
          encodeUtf8KV
            #<$> [ ("description", raffleDescription)
                 , ("image", raffleImageURI)
                 , ("name", raffleName)
                 ]
    , version = metadataVersion
    , extra = rsd
    }
{-# INLINEABLE mkRaffleDatum #-}

-- | Using a synonym for @Integer@ because a custom sum type would increase the scrpt size
type RaffleStateLabel = Integer -- TODO : check if any data encoding works bette on Plutus V3

-------------------------------------------------------------------------------

-- *  Show Instances

-------------------------------------------------------------------------------
instance Show RaffleConfig where
  show :: RaffleConfig -> String
  show RaffleConfig {..} =
    "Commit Deadline          | "
      ++ show (getPOSIXTime rCommitDDL)
      ++ "\nReveal Deadline          | "
      ++ show (getPOSIXTime rRevealDDL)
      ++ "\nTicket Price:            | "
      ++ show rTicketPrice
      ++ "\nMinimum no. of tickets   | "
      ++ show rMinTickets
      ++ showValue "Raffle Stake" rStake

instance Show RaffleParam where
  show :: RaffleParam -> String
  show RaffleParam {..} =
    "Raffle Validator Hash    | "
      ++ show rRaffleValidatorHash
      ++ "\nTicket Validator Hash    | "
      ++ show rTicketValidatorHash
      ++ "\nRaffle Collateral        | "
      ++ show rRaffleCollateral
      ++ "\nTicket Collateral        | "
      ++ show rTicketCollateral
      ++ "\nMin. Ticket Price        | "
      ++ show rMinTicketPrice
      ++ "\nMin. Reveal Window       | "
      ++ show rMinRevealingWindow
      ++ "\nMax. no. of tickets      | "
      ++ show rMaxNoOfTickets

instance Show RaffleStateData where
  show :: RaffleStateData -> String
  show RaffleStateData {..} =
    "\nRAFFLE STATE DATA"
      ++ "\n-------------------------\n"
      ++ "RaffleID                 |"
      ++ show
        rRaffleID
      ++ wrapTitle "PARAMETERS"
      ++ show rParam
      ++ wrapTitle "CONFIGURATION"
      ++ show rConfig
      ++ wrapTitle "TICKETS"
      ++ "Sold Tickets             | "
      ++ show rSoldTickets
      ++ "\nRevealed Tickets         | "
      ++ show rRevealedTickets
      ++ "\nRefunded Tickets         | "
      ++ show rRefundedTickets
      ++ "\nRandom seed              | "
      ++ show rRandomSeed
      ++ "\n"
