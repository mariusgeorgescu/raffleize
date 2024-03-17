module RaffleizeDApp.TxBuilding.RaffleizeLookups where


import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.TxBuilding.Utils
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
    getRaffleDatumAndValue utxo

lookupTicketStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (TicketStateData, Value)
lookupTicketStateDataAndValue ticketId =
  do
    utxo <- lookuptUTxOWithStateToken ticketId ticketValidatorGY
    getTicketDatumAndValue utxo

------------------------------------------------------------------------------------------------

-- * Extract State Data

------------------------------------------------------------------------------------------------

gyDatumToRSD :: (HasCallStack, GYTxQueryMonad m) => GYDatum -> m RaffleStateData
gyDatumToRSD datum = case fromBuiltinData @RaffleDatum . datumToPlutus' $ datum of
  Nothing -> throwError (GYQueryDatumException (GYInvalidDatum datum))
  Just (RaffleDatum _mdata _v rsd) -> return rsd

gyDatumToTSD :: (HasCallStack, GYTxQueryMonad m) => GYDatum -> m TicketStateData
gyDatumToTSD datum = case fromBuiltinData @TicketDatum . datumToPlutus' $ datum of
  Nothing -> throwError (GYQueryDatumException (GYInvalidDatum datum))
  Just (TicketDatum _ _ tsd) -> return tsd

getRaffleDatumAndValue :: (HasCallStack, GYTxQueryMonad m) => GYUTxO -> m (RaffleStateData, Value)
getRaffleDatumAndValue raffleStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue raffleStateUTxO
  rsd <- gyDatumToRSD gyDatum
  let pVal = valueToPlutus gyValue
  return (rsd, pVal)

getTicketDatumAndValue :: (HasCallStack, GYTxQueryMonad m) => GYUTxO -> m (TicketStateData, Value)
getTicketDatumAndValue ticketStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue ticketStateUTxO
  tsd <- gyDatumToTSD gyDatum
  let pVal = valueToPlutus gyValue
  return (tsd, pVal)