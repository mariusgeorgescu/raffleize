module RaffleizeDApp.TxBuilding.Lookups where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Interval qualified
import PlutusLedgerApi.V1.Value

import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * Query Blockchain Components

------------------------------------------------------------------------------------------------

{- | This function returns a UTxO which contains the NFT specified by 'AssetClass' locked at any of the addresses in the list.
If no UTxO is found the function fails.
-}
getUTxOWithStateTokenAtAddresses :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> [GYAddress] -> m GYUTxO
getUTxOWithStateTokenAtAddresses refAC addresses = do
  utxs <- lookupUTxOWithStateTokenAtAddresses refAC addresses
  maybe (throwError (GYQueryUTxOException (GYNoUtxosAtAddress addresses))) return utxs

{- | This function returns a UTxO which contains the NFT specified by 'AssetClass' locked at a given validator addres.
If no UTxO is found the function fails.
-}
getUTxOWithStateToken :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> GYValidator 'PlutusV2 -> m GYUTxO
getUTxOWithStateToken refAC gyValidator = do
  gyValidatorAddressGY <- scriptAddress gyValidator
  getUTxOWithStateTokenAtAddresses refAC [gyValidatorAddressGY]

getRaffleStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (RaffleStateData, Value)
getRaffleStateDataAndValue raffleId =
  do
    utxo <- getUTxOWithStateToken raffleId raffleizeValidatorGY
    maybe (throwError (GYApplicationException RaffleizeDatumNotFound)) return $ rsdAndValueFromUTxO utxo

getRaffleStateValueAndImage :: GYTxQueryMonad m => AssetClass -> m (RaffleStateData, Value, String)
getRaffleStateValueAndImage raffleId =
  do
    utxo <- getUTxOWithStateToken raffleId raffleizeValidatorGY
    maybe (throwError (GYApplicationException RaffleizeDatumNotFound)) return $ rsdValueAndImageFromUTxO utxo

getTicketStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (TicketStateData, Value)
getTicketStateDataAndValue ticketId =
  do
    utxo <- getUTxOWithStateToken ticketId ticketValidatorGY
    maybe (throwError (GYApplicationException TicketDatumNotFound)) return $ tsdAndValueFromUTxO utxo

getTicketStateDataAndValueAndImage :: GYTxQueryMonad m => AssetClass -> m (TicketStateData, Value, String)
getTicketStateDataAndValueAndImage ticketId =
  do
    utxo <- getUTxOWithStateToken ticketId ticketValidatorGY
    maybe (throwError (GYApplicationException TicketDatumNotFound)) return $ tsdValueAndImageFromUTxO utxo

------------------------------------------------------------------------------------------------

-- * Search

------------------------------------------------------------------------------------------------

{- | This function returns a UTxO which contains the NFT specified by 'AssetClass' locked at any of the addresses in the list.
If no UTxO is found the function fails.
-}
lookupUTxOWithStateTokenAtAddresses :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> [GYAddress] -> m (Maybe GYUTxO)
lookupUTxOWithStateTokenAtAddresses refAC addresses = do
  gyRefAC <- assetClassFromPlutus' refAC
  utxos <- concatMap utxosToList <$> mapM (`utxosAtAddress` Just gyRefAC) addresses
  case utxos of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> throwError (GYApplicationException TooManyUTxOs)

lookupUTxOWithStateToken :: (GYTxQueryMonad m) => AssetClass -> GYValidator 'PlutusV2 -> m (Maybe GYUTxO)
lookupUTxOWithStateToken refAC gyValidator = do
  gyValidatorAddressGY <- scriptAddress gyValidator
  lookupUTxOWithStateTokenAtAddresses refAC [gyValidatorAddressGY]

lookupRaffleStateDataAndValue :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (RaffleStateData, Value))
lookupRaffleStateDataAndValue raffleId =
  do
    utxo <- lookupUTxOWithStateToken raffleId raffleizeValidatorGY
    return $ utxo >>= rsdAndValueFromUTxO

lookupRaffleStateValueAndImage :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (RaffleStateData, Value, String))
lookupRaffleStateValueAndImage raffleId =
  do
    utxo <- lookupUTxOWithStateToken raffleId raffleizeValidatorGY
    return $ utxo >>= rsdValueAndImageFromUTxO

lookupTicketStateDataAndValue :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (TicketStateData, Value))
lookupTicketStateDataAndValue ticketId =
  do
    utxo <- lookupUTxOWithStateToken ticketId ticketValidatorGY
    return $ utxo >>= tsdAndValueFromUTxO

lookupTickeStateValueAndImage :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (TicketStateData, Value, String))
lookupTickeStateValueAndImage ticketId =
  do
    utxo <- lookupUTxOWithStateToken ticketId ticketValidatorGY
    return $ utxo >>= tsdValueAndImageFromUTxO

lookupRaffleInfoRefAC :: (GYTxQueryMonad m) => AssetClass -> m (Maybe RaffleInfo)
lookupRaffleInfoRefAC raffleRefAC = do
  now <- slotOfCurrentBlock
  nowposix <- pPOSIXTimeFromGYSlot now
  let tr = PlutusLedgerApi.V1.Interval.singleton nowposix
  stateValImg <- lookupRaffleStateValueAndImage raffleRefAC
  return $ mkRaffleInfo tr <$> stateValImg

lookupTicketInfoByUserAC :: (GYTxQueryMonad m) => AssetClass -> m (Maybe TicketInfo)
lookupTicketInfoByUserAC ticketUserAC = do
  now <- slotOfCurrentBlock
  nowposix <- pPOSIXTimeFromGYSlot now
  let tr = PlutusLedgerApi.V1.Interval.singleton nowposix
  let ticketRefAC = deriveRefFromUserAC ticketUserAC
  mticket <- lookupTickeStateValueAndImage ticketRefAC
  case mticket of
    Nothing -> return Nothing
    Just (tsd, tVal, tImg) -> do
      mraffle <- lookupRaffleStateDataAndValue (tRaffle tsd)
      case mraffle of
        Nothing -> return Nothing
        Just (rsd, rVal) -> do
          let raffleStateId = evaluateRaffleState (tr, rsd, rVal)
          let ticketStateId = evalTicketState tsd (rRandomSeed rsd) raffleStateId
          let ticketStateLabel = showTicketStateLabel ticketStateId
          let actions = validActionLabelsForTicketState ticketStateId
          return $ Just $ TicketInfo tsd tVal tImg ticketStateLabel actions

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
