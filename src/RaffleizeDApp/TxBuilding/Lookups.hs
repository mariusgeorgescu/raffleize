module RaffleizeDApp.TxBuilding.Lookups where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.OnChain.RaffleizeLogic (hasRefPrefix)
import RaffleizeDApp.TxBuilding.Exceptions

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
    maybe (throwError (GYApplicationException RaffleizeDatumNotFound)) return $ getRaffleDatumAndValue utxo

lookupTicketStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (TicketStateData, Value)
lookupTicketStateDataAndValue ticketId =
  do
    utxo <- lookuptUTxOWithStateToken ticketId ticketValidatorGY
    maybe (throwError (GYApplicationException TicketDatumNotFound)) return $ getTicketDatumAndValue utxo

------------------------------------------------------------------------------------------------

-- * Extract State Data

------------------------------------------------------------------------------------------------
-- This function checks if a GYTxOut has a reference token minted by the raffleize minting policy
-- Since the reference tokens minted by the raffleize minting policy are always locked at the script address
gyOutHasValidRefToken :: GYUTxO -> Bool
gyOutHasValidRefToken gyOut =
  let gyValue = utxoValue gyOut
      gyAssets = valueAssets gyValue
   in any isRaffleizeAC gyAssets

-- | This function checks if a 'GYAssetClass' is of a reference token minted by Raffleize Minting Policy
isRaffleizeAC :: GYAssetClass -> Bool
isRaffleizeAC (GYToken gyMP gyTN) =
  (gyMP == mintingPolicyId raffleizeMintingPolicyGY)
    && hasRefPrefix (tokenNameToPlutus gyTN)
isRaffleizeAC _ = False

gyDatumToRSD :: GYDatum -> Maybe RaffleStateData
gyDatumToRSD gyDatum = RaffleizeDApp.CustomTypes.RaffleTypes.extra <$> (fromBuiltinData @RaffleDatum . datumToPlutus' $ gyDatum)

gyDatumToTSD :: GYDatum -> Maybe TicketStateData
gyDatumToTSD gyDatum = RaffleizeDApp.CustomTypes.TicketTypes.extra <$> (fromBuiltinData @TicketDatum . datumToPlutus' $ gyDatum)

gyGetInlineDatumAndValue :: GYUTxO -> Maybe (GYDatum, GYValue)
gyGetInlineDatumAndValue utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> Just (datum, utxoValue utxo)
  _ -> Nothing

getRaffleDatumAndValue :: GYUTxO -> Maybe (RaffleStateData, Value)
getRaffleDatumAndValue raffleStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue raffleStateUTxO
  rsd <- gyDatumToRSD gyDatum
  let pVal = valueToPlutus gyValue
  return (rsd, pVal)

getTicketDatumAndValue :: GYUTxO -> Maybe (TicketStateData, Value)
getTicketDatumAndValue ticketStateUTxO = do
  (gyDatum, gyValue) <- gyGetInlineDatumAndValue ticketStateUTxO
  tsd <- gyDatumToTSD gyDatum
  let pVal = valueToPlutus gyValue
  return (tsd, pVal)
