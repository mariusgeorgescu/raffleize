module RaffleizeDApp.TxBuilding.Utils where

import Cardano.Api (Key (getVerificationKey), castVerificationKey)
import Data.Aeson (decodeFileStrict)
import Data.Either.Extra
import Data.Text qualified
import Data.Text.IO qualified
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Interval qualified
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.TxBuilding.Validators
import RaffleizeDApp.Utils
import System.Directory.Extra

------------------------

-- * Utilities

------------------------

getAdaBalance :: GYUTxOs -> Ada
getAdaBalance = fromValue . getValueBalance

getValueBalance :: GYUTxOs -> Value
getValueBalance = valueToPlutus . foldMapUTxOs utxoValue

addressFromPaymentSigningKey :: GYNetworkId -> GYExtendedPaymentSigningKey -> GYAddress
addressFromPaymentSigningKey nid extendedSkey =
  let
    vkey = Cardano.Api.getVerificationKey $ extendedPaymentSigningKeyToApi extendedSkey
    pub_key = paymentVerificationKeyFromApi (castVerificationKey vkey)
    payment_key_hash = paymentKeyHash pub_key
    address = addressFromPaymentKeyHash nid payment_key_hash
   in
    address

pPOSIXTimeFromSlotInteger :: (GYTxQueryMonad m) => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . unsafeSlotFromInteger

pPOSIXTimeFromGYSlot :: (GYTxQueryMonad m) => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: (GYTxQueryMonad m) => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)

showLink :: GYNetworkId -> Text -> Text -> Text
showLink nid s content = case nid of
  GYMainnet -> cexplorerMainnet <> s <> "/" <> content <> " "
  GYTestnetPreprod -> cexplorerPreprod <> s <> "/" <> content <> " "
  GYTestnetPreview -> cexplorerPreview <> s <> "/" <> content <> " "
  GYTestnetLegacy -> content
  GYPrivnet _f -> content

{--  This function returns a Just tuple of the datum and value of a UTxO if the UTxO has inline datum,
otherwise returns Nothing
--}
getInlineDatumAndValue :: GYUTxO -> Maybe (GYDatum, GYValue)
getInlineDatumAndValue utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> Just (datum, utxoValue utxo)
  _ -> Nothing

-- -- ----------------------------------------------------------------------------------------

-- * Raffleize Checks

------------------------------------------------------------------------------------------------

-- | This function checks if a 'GYAssetClass' is of a "reference token" minted by Raffleize Minting Policy
isRaffleizeRefAC :: GYAssetClass -> Bool
isRaffleizeRefAC (GYToken gyMP gyTN) =
  (gyMP == mintingPolicyId raffleizeMintingPolicyGY)
    && hasRefPrefix (tokenNameToPlutus gyTN)
isRaffleizeRefAC _ = False

{-- This function checks if a GYTxOut has a reference token minted by the raffleize minting policy
 Since the reference tokens minted by the raffleize minting policy are always locked at the script address --}
hasValidRefToken :: GYUTxO -> Bool
hasValidRefToken gyOut =
  let gyValue = utxoValue gyOut
      gyAssets = valueAssets gyValue
   in any isRaffleizeRefAC gyAssets

hasAnyOfTheAssets :: [GYAssetClass] -> GYUTxO -> Bool
hasAnyOfTheAssets acs gyOut =
  let gyValue = utxoValue gyOut
      gyAssets = valueAssets gyValue
   in any (`elem` acs) gyAssets

-- -- ------------------------------------------------------------------------------------------

-- * Raffle Validator Utils

------------------------------------------------------------------------------------------------

getMyRaffleizeUserTokensFromValue :: Value -> [AssetClass]
getMyRaffleizeUserTokensFromValue val =
  let
    raffleizeCS = mintingPolicyCurrencySymbol raffleizeMintingPolicyGY
   in
    [AssetClass (cs, tn) | (cs, tn, _) <- flattenValue val, raffleizeCS == cs]

{--  This function converts a 'GYDatum' to 'RaffleDatum', if does not succeeed it returns Nothing.
--}
raffleDatumFromDatum :: GYDatum -> Maybe RaffleDatum
raffleDatumFromDatum = fromBuiltinData @RaffleDatum . datumToPlutus'

{--  This returns the 'RaffleStateData' from the 'GYDatum'  if the datum is a valid 'RaffleDatum'.
 Otherwise it returns Nothing.-}
rsdFromDatum :: GYDatum -> Maybe RaffleStateData
rsdFromDatum gyDatum = raffleStateData <$> raffleDatumFromDatum gyDatum

{--  This returns the the decoded link of the image from the 'GYDatum' if the datum is a valid 'RaffleDatum'.
 Otherwise it returns Nothing.-}
imageFromRaffleDatum :: GYDatum -> Maybe String
imageFromRaffleDatum gyDatum = do
  image <- raffleImage <$> raffleDatumFromDatum gyDatum
  let decodedImage = Data.Text.unpack . fromBuiltin . decodeUtf8 $ image
  return decodedImage

{--  This function returns a tuple of RaffleStateData and Value of a UTxO if the datum is a valid 'RaffleDatum'.
 Otherwise it returns Nothing.-}
rsdAndValueFromUTxO :: GYUTxO -> Maybe (RaffleStateData, Value)
rsdAndValueFromUTxO raffleStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue raffleStateUTxO
  rsd <- rsdFromDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (rsd, pVal)

{--  This function returns a triple of RaffleStateData, Value of a UTxO and the decoded link of the image, if the datum is a valid 'RaffleDatum'.
 Otherwise it returns Nothing.-}
rsdValueAndImageFromUTxO :: GYUTxO -> Maybe (RaffleStateData, Value, String)
rsdValueAndImageFromUTxO raffleStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue raffleStateUTxO
  rsd <- rsdFromDatum gyDatum
  decodedImg <- imageFromRaffleDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (rsd, pVal, decodedImg)

{--  This function constructs and returns 'RaffleInfo'from a 'POSIXTimeRange' and the datum of a UTxO if the datum is a valid 'RaffleDatum'.
 Otherwise it returns Nothing.-}
raffleInfoFromUTxO :: GYUTxO -> POSIXTimeRange -> Maybe RaffleInfo
raffleInfoFromUTxO utxo timeRange = mkRaffleInfo timeRange <$> rsdValueAndImageFromUTxO utxo

lookupRaffleInfoAtUTxO :: (GYTxQueryMonad m) => GYUTxO -> m (Maybe RaffleInfo)
lookupRaffleInfoAtUTxO utxo = do
  now <- slotOfCurrentBlock
  nowposix <- pPOSIXTimeFromGYSlot now
  let tr = PlutusLedgerApi.V1.Interval.singleton nowposix
  return $ raffleInfoFromUTxO utxo tr

-- -- ----------------------------------------------------------------------------------------

-- * Ticket Validator Utils

------------------------------------------------------------------------------------------------

{--  This function converts a 'GYDatum' to 'TicketDatum', if does not succeeed it returns Nothing.
--}
ticketDatumFromDatum :: GYDatum -> Maybe TicketDatum
ticketDatumFromDatum = fromBuiltinData @TicketDatum . datumToPlutus'

{--  This returns the 'RaffleStateData' from the 'GYDatum'  if the datum is a valid 'TicketDatum'.
 Otherwise it returns Nothing.-}
tsdFromDatum :: GYDatum -> Maybe TicketStateData
tsdFromDatum gyDatum = ticketStateData <$> ticketDatumFromDatum gyDatum

{--  This returns the the decoded link of the image from the 'GYDatum' if the datum is a valid 'TicketDatum'.
 Otherwise it returns Nothing.-}
imageFromTicketDatum :: GYDatum -> Maybe String
imageFromTicketDatum gyDatum = do
  image <- ticketImage <$> ticketDatumFromDatum gyDatum
  let decodedImage = Data.Text.unpack . fromBuiltin . decodeUtf8 $ image
  return decodedImage

{--  This function returns a tuple of TicketStateData and Value of a UTxO if the datum is a valid 'TicketDatum'.
 Otherwise it returns Nothing.-}
tsdAndValueFromUTxO :: GYUTxO -> Maybe (TicketStateData, Value)
tsdAndValueFromUTxO ticketStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue ticketStateUTxO
  tsd <- tsdFromDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (tsd, pVal)

{--  This function returns a triple of TicketStateData, Value of a UTxO and the decoded link of the image, if the datum is a valid 'TicketDatum'.
 Otherwise it returns Nothing.-}
tsdValueAndImageFromUTxO :: GYUTxO -> Maybe (TicketStateData, Value, String)
tsdValueAndImageFromUTxO ticketStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue ticketStateUTxO
  tsd <- tsdFromDatum gyDatum
  decodedImg <- imageFromTicketDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (tsd, pVal, decodedImg)

{--  This function constructs and returns 'TicketInfo'from a 'RaffleStateId', an 'Integer' of the current random and the datum of a UTxO if the datum is a valid 'TicketDatum'.
 Otherwise it returns Nothing.-}
ticketInfoFromUTxO :: GYUTxO -> RaffleStateId -> Integer -> Maybe TicketInfo
ticketInfoFromUTxO utxo raffleStateId currentRandom = mkTicketInfo raffleStateId currentRandom <$> tsdValueAndImageFromUTxO utxo

-- -- ----------------------------------------------------------------------------------------

-- * Mnemonic Utils

------------------------------------------------------------------------------------------------

readMnemonicFile :: FilePath -> IO (Maybe GYExtendedPaymentSigningKey)
readMnemonicFile path = do
  putStrLn $ yellowColorString $ "Mnemonic phrase at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn "Found"
      readMnemonic <$> Data.Text.IO.readFile path
    else do
      putStrLn $ show path <> " not found"
      return Nothing

isValidMnemonic :: Text -> Bool
isValidMnemonic = Data.Either.Extra.isRight . walletKeysFromMnemonic . Data.Text.words

readMnemonic :: Text -> Maybe GYExtendedPaymentSigningKey
readMnemonic content = eitherToMaybe $ walletKeysToExtendedPaymentSigningKey <$> walletKeysFromMnemonic (Data.Text.words content)

decodeConfigFile :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeConfigFile path = do
  putStrLn $ yellowColorString $ "Parsing config file at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn "Found"
      decodeFileStrict path
    else do
      putStrLn $ show path <> " not found"
      return Nothing
