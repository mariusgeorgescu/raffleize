module RaffleizeDApp.CustomTypes.TicketTypes where

import Data.Aeson
import Data.Aeson.Types
import GeniusYield.Types (tokenNameToPlutus, unsafeTokenNameFromHex)
import PlutusLedgerApi.V1.Value (AssetClass, TokenName (..), toString)
import PlutusLedgerApi.V2 (ScriptHash, ToData (toBuiltinData))
import PlutusTx (unstableMakeIsData)
import PlutusTx.AssocMap (lookup, safeFromList)
import PlutusTx.Eq qualified
import RaffleizeDApp.Constants
  ( metadataVersion,
    ticketDescription,
    ticketImageURI,
    ticketName,
  )
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.OnChain.Utils (encodeUtf8KV, wrapTitle)

-------------------------------------------------------------------------------

-- * Ticket Type  Declarations

-------------------------------------------------------------------------------

newtype SecretHash = SecretHash
  {unSecretHash :: BuiltinByteString}
  deriving (Generic, Eq, Show)

unstableMakeIsData ''SecretHash ---  must be changed with stable version

instance ToJSON SecretHash where
  toJSON :: SecretHash -> Data.Aeson.Value
  toJSON (SecretHash s) = toJSON $ toString (TokenName s) --- using TokenName for hex conversion

instance FromJSON SecretHash where
  parseJSON :: Data.Aeson.Value -> Parser SecretHash
  parseJSON v =
    let tn = tokenNameToPlutus . unsafeTokenNameFromHex <$> parseJSON @Text v --- using TokenName for hex conversion
     in SecretHash . unTokenName <$> tn

type Secret = BuiltinByteString

-- | Ticket State Data
-- This datatype is part of the TICKET STATE.
-- The TICKET STATE is defined by this datatype value and the RAFFLE STATE .
data TicketStateData = TicketStateData
  { tNumber :: Integer, --- ^ The ticket number. Numbers should be assigned when tickets are minted, in order starting with 0.
    tSecretHash :: SecretHash, --- ^ The hash of the secret committed when ticket was bought.
    tSecret :: Maybe Secret, --- ^ Optional the secret matching the @SecretHash@. If @Nothing@, the ticket was not revealed.
    tRaffle :: AssetClass, --- ^ The raffle id of the raffle to which the ticket is associated.
    tRaffleValidator :: ScriptHash --- ^ The validator hash of the validation logic for spending the raffle state UTxO.
  }
  deriving (Generic, Eq, ToJSON, FromJSON)

unstableMakeIsData ''TicketStateData ---  must be changed with stable version

-- | The datum datatype which should be locked with ticket ref NFT.
-- | This datatype is following the CIP-68 Datum Metadata Standard.
data TicketDatum = TicketDatum
  { metadata :: Metadata, --- ^  Map k v (where k and v arr  UTF-8 encoded @BuiltinByteString@s)
    version :: Integer, --- ^ version of CIP-68 Datum Metadata Standard.
    extra :: TicketStateData --- ^ Plutus data
  }
  deriving (Generic)

unstableMakeIsData ''TicketDatum ---  must be changed with stable version

-- | Functon to construct a @TicketDatum@, from a  @RaffleStateData@.
mkTicketDatum :: TicketStateData -> TicketDatum
mkTicketDatum tsd =
  TicketDatum
    { metadata =
        safeFromList $
          encodeUtf8KV
            #<$> [ ("description", ticketDescription),
                   ("image", ticketImageURI),
                   ("name", ticketName)
                 ],
      version = metadataVersion,
      extra = tsd
    }
{-# INLINEABLE mkTicketDatum #-}

-- | Functon to get the @TicketStateData@ from @TicketDatum@.
ticketStateData :: TicketDatum -> TicketStateData
ticketStateData = extra
{-# INLINEABLE ticketStateData #-}

-- | Functon to get the image link from metadata
ticketImage :: TicketDatum -> BuiltinByteString
ticketImage datum = fromMaybe @BuiltinByteString "" $ lookup (encodeUtf8 "image") (metadata datum)

-- | Using a synonym for @Integer@ because a custom sum type would increase the scrpt size
data TicketStateId
  = COMMITTED
  | FULLY_REFUNDABLE
  | REVEALABLE
  | REVEALED
  | WINNING
  | LOSING
  | EXTRA_REFUNDABLE
  | UNREVEALED_EXPIRED
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Enum)

unstableMakeIsData ''TicketStateId ---  must be changed with stable version

instance PlutusTx.Eq.Eq TicketStateId where
  (==) :: TicketStateId -> TicketStateId -> Bool
  (==) x y = toBuiltinData x #== toBuiltinData y

-------------------------------------------------------------------------------

-- * Show Instances

-------------------------------------------------------------------------------
instance Show TicketStateData where
  show :: TicketStateData -> String
  show TicketStateData {..} =
    "\nTICKET STATE DATA"
      ++ "\n------------------------- "
      ++ "\nTicker Number            | "
      ++ show tNumber
      ++ "\nSecret Hash              | "
      ++ show tSecretHash
      ++ "\nSecret                   | "
      ++ show tSecret
      ++ wrapTitle "PARAMETERS"
      ++ "Raffle Validator Hash    | "
      ++ show tRaffleValidator
      ++ "\nRaffle ID                | "
      ++ show tRaffle
      ++ "\n"

-- ssecrethash :: SecretHash = "marius"

-- ssecret :: Secret = "marius"

-- b = fromJSON @SecretHash $ toJSON ssecrethash

-- c = fromJSON @SecretHash $ toJSON ssecret

-- --- >>> show b
-- --- >>> show c
-- -- "Success \"marius\""
-- -- "Success \"marius\""
