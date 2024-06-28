module RaffleizeDApp.CustomTypes.TicketTypes where

import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (ScriptHash, fromList)
import PlutusTx (unstableMakeIsData)
import PlutusTx.AssocMap (lookup)
import RaffleizeDApp.Constants (
  metadataVersion,
  ticketDescription,
  ticketImageURI,
  ticketName,
 )
import RaffleizeDApp.CustomTypes.Types (
  Metadata,
 )
import RaffleizeDApp.OnChain.Utils (encodeUtf8KV, wrapTitle)

-------------------------------------------------------------------------------

-- * Ticket Type  Declarations

-------------------------------------------------------------------------------

type SecretHash = BuiltinByteString
type Secret = BuiltinByteString

{- | Ticket State Data
This datatype is part of the TICKET STATE.
The TICKET STATE is defined by this datatype value and the RAFFLE STATE .
-}
data TicketStateData = TicketStateData
  { tNumber :: Integer --- ^ The ticket number. Numbers should be assigned when tickets are minted, in order starting with 0.
  , tSecretHash :: SecretHash --- ^ The hash of the secret committed when ticket was bought.
  , tSecret :: Maybe Secret --- ^ Optional the secret matching the @SecretHash@. If @Nothing@, the ticket was not revealed.
  , tRaffle :: AssetClass --- ^ The raffle id of the raffle to which the ticket is associated.
  , tRaffleValidator :: ScriptHash --- ^ The validator hash of the validation logic for spending the raffle state UTxO.
  }
  deriving (Generic, Eq, ToJSON, FromJSON)

unstableMakeIsData ''TicketStateData ---  must be changed with stable version

{- | The datum datatype which should be locked with ticket ref NFT.
| This datatype is following the CIP-68 Datum Metadata Standard.
-}
data TicketDatum = TicketDatum
  { metadata :: Metadata --- ^  Map k v (where k and v arr  UTF-8 encoded @BuiltinByteString@s)
  , version :: Integer --- ^ version of CIP-68 Datum Metadata Standard.
  , extra :: TicketStateData --- ^ Plutus data
  }
  deriving (Generic)

unstableMakeIsData ''TicketDatum ---  must be changed with stable version

-- | Functon to construct a @TicketDatum@, from a  @RaffleStateData@.
mkTicketDatum :: TicketStateData -> TicketDatum
mkTicketDatum tsd =
  TicketDatum
    { metadata =
        fromList $
          encodeUtf8KV
            #<$> [ ("description", ticketDescription)
                 , ("image", ticketImageURI)
                 , ("name", ticketName)
                 ]
    , version = metadataVersion
    , extra = tsd
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
type TicketStateId = Integer -- TODO : check if any data encoding works bette on Plutus V3

-- | @TicketStateLabel@ is a synonym for @String@
type TicketStateLabel = String

-------------------------------------------------------------------

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