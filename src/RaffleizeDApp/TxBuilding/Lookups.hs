module RaffleizeDApp.TxBuilding.Lookups where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.OnChain.RaffleizeLogic (evaluateRaffleState, hasRefPrefix, showRaffleStateLabel, validActionLabelsForState)
import RaffleizeDApp.TxBuilding.Exceptions

import Data.Text qualified as Text
import PlutusLedgerApi.V1.Interval qualified
import RaffleizeDApp.TxBuilding.Utils (pPOSIXTimeFromGYSlot)
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * Query Blockchain Components

------------------------------------------------------------------------------------------------

lookuptUTxOWithStateToken :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> GYValidator 'PlutusV2 -> m GYUTxO
lookuptUTxOWithStateToken refAC gyValidator = do
  gyRefAC <- assetClassFromPlutus' refAC
  gyValidatorAddressGY <- scriptAddress gyValidator
  utxs <- utxosToList <$> utxosAtAddress gyValidatorAddressGY (Just gyRefAC)
  case utxs of
    [x] -> return x
    _ -> throwError (GYQueryUTxOException (GYNoUtxosAtAddress [gyValidatorAddressGY]))

lookupTxOWithTokenAtAddress :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => AssetClass -> GYAddress -> m GYUTxO
lookupTxOWithTokenAtAddress tokenAC address = do
  gyAC <- assetClassFromPlutus' tokenAC
  utxs <- utxosToList <$> utxosAtAddress address (Just gyAC)
  case utxs of
    [x] -> return x
    _ -> throwError (GYQueryUTxOException (GYNoUtxosAtAddress [address]))

lookupTxOWithTokenAtAddresses :: (HasCallStack, GYTxMonad m, GYTxQueryMonad m) => AssetClass -> [GYAddress] -> m GYUTxO
lookupTxOWithTokenAtAddresses tokenAC addresses = do
  gyAC <- assetClassFromPlutus' tokenAC
  utxs <- concatMap utxosToList <$> mapM (`utxosAtAddress` Just gyAC) addresses
  case utxs of
    [x] -> return x
    _ -> throwError (GYQueryUTxOException (GYNoUtxosAtAddress addresses))

lookupRaffleStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (RaffleStateData, Value)
lookupRaffleStateDataAndValue raffleId =
  do
    utxo <- lookuptUTxOWithStateToken raffleId raffleizeValidatorGY
    maybe (throwError (GYApplicationException RaffleizeDatumNotFound)) return $ getRaffleStateDataAndValue utxo

lookupTicketStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (TicketStateData, Value)
lookupTicketStateDataAndValue ticketId =
  do
    utxo <- lookuptUTxOWithStateToken ticketId ticketValidatorGY
    maybe (throwError (GYApplicationException TicketDatumNotFound)) return $ getTicketDatumAndValue utxo

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

gyGetInlineDatumAndValue' :: MonadError GYTxMonadException m => GYUTxO -> m (GYDatum, GYValue)
gyGetInlineDatumAndValue' utxo = maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ gyGetInlineDatumAndValue utxo

-- getRaffleStateDataAndValue' :: MonadError GYTxMonadException m => GYUTxO -> m (RaffleStateData, Value)
-- getRaffleStateDataAndValue' utxo = maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ getRaffleStateDataAndValue utxo

gyGetRaffleInfo :: (HasCallStack, GYTxQueryMonad m) => GYUTxO -> m RaffleInfo
gyGetRaffleInfo utxo = do
  now <- slotOfCurrentBlock
  nowposix <- pPOSIXTimeFromGYSlot now
  (rsd, val, img) <- maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ getRaffleStateValueAndImage utxo
  let r = PlutusLedgerApi.V1.Interval.singleton nowposix
  let stateId = evaluateRaffleState (r, rsd, val)
  let stateLabel = showRaffleStateLabel stateId
  let actions = validActionLabelsForState stateId
  return $ RaffleInfo rsd val img stateLabel actions

-- gyGetTicketInfo :: (HasCallStack, GYTxQueryMonad m) => RaffleStateId -> GYUTxO -> m TicketInfo
-- gyGetTicketInfo raffleStateId utxo = do
--   (tsd, tVal, tImg) <- maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ getTicketStateValueAndImage utxo
--   let ticketStateId = evalTicketState tsd raffleStateId
--   let ticketStateLabel = showTicketStateLabel ticketStateId
--   let actions = if ticketStateId `elem` [5, 6] then validActionLabelsForState raffleStateId else []
--   let actions = validActionLabelsForState raffleStateId
--   return $ TicketInfo tsd vVal img stateLabel actions

-- ----------------------------------------------------------------------------------------

-- * Extract State Data

------------------------------------------------------------------------------------------------
-- This function checks if a GYTxOut has a reference token minted by the raffleize minting policy
-- Since the reference tokens minted by the raffleize minting policy are always locked at the script address
gyOutHasValidRefToken :: GYUTxO -> Bool
gyOutHasValidRefToken gyOut =
  let gyValue = utxoValue gyOut
      gyAssets = valueAssets gyValue
   in any isRaffleizeRefAC gyAssets

-- | This function checks if a 'GYAssetClass' is of a reference token minted by Raffleize Minting Policy
isRaffleizeRefAC :: GYAssetClass -> Bool
isRaffleizeRefAC (GYToken gyMP gyTN) =
  (gyMP == mintingPolicyId raffleizeMintingPolicyGY)
    && hasRefPrefix (tokenNameToPlutus gyTN)
isRaffleizeRefAC _ = False

-- | This function checks if a 'GYAssetClass' is of a token minted by Raffleize Minting Policy
isRaffleizeAC :: GYAssetClass -> Bool
isRaffleizeAC (GYToken gyMP _gyTN) = gyMP == mintingPolicyId raffleizeMintingPolicyGY
isRaffleizeAC _ = False

gyDatumToRSD :: GYDatum -> Maybe RaffleStateData
gyDatumToRSD gyDatum = raffleStateData <$> (fromBuiltinData @RaffleDatum . datumToPlutus' $ gyDatum)

gyGetImageFromRaffleizeDatum :: GYDatum -> Maybe BuiltinByteString
gyGetImageFromRaffleizeDatum gyDatum = raffleImage <$> (fromBuiltinData @RaffleDatum . datumToPlutus' $ gyDatum)

gyDatumToTSD :: GYDatum -> Maybe TicketStateData
gyDatumToTSD gyDatum = ticketStateData <$> (fromBuiltinData @TicketDatum . datumToPlutus' $ gyDatum)

gyGetInlineDatumAndValue :: GYUTxO -> Maybe (GYDatum, GYValue)
gyGetInlineDatumAndValue utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> Just (datum, utxoValue utxo)
  _ -> Nothing

getRaffleStateDataAndValue :: GYUTxO -> Maybe (RaffleStateData, Value)
getRaffleStateDataAndValue raffleStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue raffleStateUTxO
  rsd <- gyDatumToRSD gyDatum
  let pVal = valueToPlutus gyValue
  return (rsd, pVal)

getRaffleStateValueAndImage :: GYUTxO -> Maybe (RaffleStateData, Value, String)
getRaffleStateValueAndImage raffleStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue raffleStateUTxO
  rsd <- gyDatumToRSD gyDatum
  img <- gyGetImageFromRaffleizeDatum gyDatum
  let pVal = valueToPlutus gyValue
  let decodedImg = Text.unpack $ fromBuiltin $ decodeUtf8 img
  return (rsd, pVal, decodedImg)

getTicketDatumAndValue :: GYUTxO -> Maybe (TicketStateData, Value)
getTicketDatumAndValue ticketStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue ticketStateUTxO
  tsd <- gyDatumToTSD gyDatum
  let pVal = valueToPlutus gyValue
  return (tsd, pVal)

getTicketStateValueAndImage :: GYUTxO -> Maybe (TicketStateData, Value, String)
getTicketStateValueAndImage ticketStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue ticketStateUTxO
  tsd <- gyDatumToTSD gyDatum
  img <- gyGetImageFromRaffleizeDatum gyDatum
  let pVal = valueToPlutus gyValue
  let decodedImg = Text.unpack $ fromBuiltin $ decodeUtf8 img
  return (tsd, pVal, decodedImg)
