module RaffleizeDApp.TxBuilding.Lookups where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Interval qualified
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3 (POSIXTimeRange)
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * Query Blockchain Components

------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

-- * Search Bulk

------------------------------------------------------------------------------------------------

lookupUTxOsAtAddress :: (GYTxQueryMonad m) => GYAddress -> m GYUTxOs
lookupUTxOsAtAddress addr = utxosAtAddress addr Nothing

lookupUTxOsAtValidator :: (GYTxQueryMonad m) => GYScript v -> m GYUTxOs
lookupUTxOsAtValidator validator = do
  addr <- scriptAddress validator
  utxosAtAddress addr Nothing

lookupUTxOsByStateTokens :: (GYTxQueryMonad m) => [AssetClass] -> GYAddress -> m GYUTxOs
lookupUTxOsByStateTokens acs addr = do
  gyRefACs <- mapM assetClassFromPlutus' acs
  gyUTxOs <- utxosAtAddress addr Nothing
  return $ filterUTxOs (hasAnyOfTheAssets gyRefACs) gyUTxOs

lookupRaffleInfosByACs :: (GYTxQueryMonad m) => [AssetClass] -> m [RaffleInfo]
lookupRaffleInfosByACs acs = do
  tr <- getTimeRangeForNextNSlots 1
  raffleValidatorAddr <- scriptAddress raffleizeValidatorGY
  raffleUTxOs <- utxosToList <$> lookupUTxOsByStateTokens acs raffleValidatorAddr
  return $ mapMaybe (`raffleInfoFromUTxO` tr) raffleUTxOs

lookupTicketInfosByACs :: (GYTxQueryMonad m) => [AssetClass] -> m [TicketInfo]
lookupTicketInfosByACs uACs = do
  let rACs = deriveRefFromUserAC <$> uACs
  ticketValidatorAddr <- scriptAddress ticketValidatorGY
  tcketUTxOs <- utxosToList <$> lookupUTxOsByStateTokens rACs ticketValidatorAddr
  let tickets = mapMaybe tsdValueAndImageFromUTxO tcketUTxOs
  let rafflesACs = fmap (\(tsd, _, _) -> tRaffle tsd) tickets
  raffles <- lookupRaffleInfosByACs rafflesACs
  tr <- getTimeRangeForNextNSlots 1
  let ticketsInfos = mkTicketsInfo tr tickets raffles
  return ticketsInfos
  where
    mkTicketsInfo :: POSIXTimeRange -> [(TicketStateData, Value, String)] -> [RaffleInfo] -> [TicketInfo]
    mkTicketsInfo tr ((tsd, tVal, tImg) : tis) ris = case find ((tRaffle tsd ==) . rRaffleID . riRsd) ris of
      Nothing -> mkTicketsInfo tr tis ris
      Just (RaffleInfo {..}) ->
        let
          raffleStateId = evaluateRaffleState (tr, riRsd, riValue)
          ticketStateId = evalTicketState tsd (rRandomSeed riRsd) raffleStateId
          ticketStateLabel = showTicketStateLabel ticketStateId
          actions = validActionLabelsForTicketState ticketStateId
         in
          TicketInfo tsd tVal tImg ticketStateLabel actions : mkTicketsInfo tr tis ris
    mkTicketsInfo _ _ _ = []

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

lookupUTxOWithStateToken :: (GYTxQueryMonad m, HasCallStack) => AssetClass -> GYAddress -> m (Maybe GYUTxO)
lookupUTxOWithStateToken refAC addr = do
  gyRefAC <- assetClassFromPlutus' refAC
  utxos <- utxosToList <$> utxosAtAddress addr (Just gyRefAC)
  case utxos of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> throwError (GYApplicationException TooManyUTxOs)

lookupRaffleStateDataAndValue :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (RaffleStateData, Value))
lookupRaffleStateDataAndValue raffleId =
  do
    raffleValidatorAddr <- scriptAddress raffleizeValidatorGY
    utxo <- lookupUTxOWithStateToken raffleId raffleValidatorAddr
    return $ utxo >>= rsdAndValueFromUTxO

lookupRaffleStateValueAndImage :: (GYTxQueryMonad m, HasCallStack) => AssetClass -> m (Maybe (RaffleStateData, Value, String))
lookupRaffleStateValueAndImage raffleId =
  do
    raffleValidatorAddr <- scriptAddress raffleizeValidatorGY
    utxo <- lookupUTxOWithStateToken raffleId raffleValidatorAddr
    return $ utxo >>= rsdValueAndImageFromUTxO

lookupTicketStateDataAndValue :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (TicketStateData, Value))
lookupTicketStateDataAndValue ticketId =
  do
    ticketValidatorAddr <- scriptAddress ticketValidatorGY
    utxo <- lookupUTxOWithStateToken ticketId ticketValidatorAddr
    return $ utxo >>= tsdAndValueFromUTxO

lookupTickeStateValueAndImage :: (GYTxQueryMonad m) => AssetClass -> m (Maybe (TicketStateData, Value, String))
lookupTickeStateValueAndImage ticketId =
  do
    ticketValidatorAddr <- scriptAddress ticketValidatorGY
    utxo <- lookupUTxOWithStateToken ticketId ticketValidatorAddr
    return $ utxo >>= tsdValueAndImageFromUTxO

lookupRaffleInfoByRefAC :: (GYTxQueryMonad m, HasCallStack) => AssetClass -> m (Maybe RaffleInfo)
lookupRaffleInfoByRefAC raffleRefAC = do
  tr <- getTimeRangeForNextNSlots 1
  stateValImg <- lookupRaffleStateValueAndImage raffleRefAC
  return $ mkRaffleInfo tr <$> stateValImg

lookupTicketInfoByRefAC :: (GYTxQueryMonad m) => AssetClass -> m (Maybe TicketInfo)
lookupTicketInfoByRefAC ticketRefAC = do
  mticket <- lookupTickeStateValueAndImage ticketRefAC
  case mticket of
    Nothing -> return Nothing
    Just (tsd, tVal, tImg) -> do
      mraffle <- lookupRaffleStateDataAndValue (tRaffle tsd)
      case mraffle of
        Nothing -> return Nothing
        Just (rsd, rVal) -> do
          tr <- getTimeRangeForNextNSlots 1
          let raffleStateId = evaluateRaffleState (tr, rsd, rVal)
          let ticketStateId = evalTicketState tsd (rRandomSeed rsd) raffleStateId
          let ticketStateLabel = showTicketStateLabel ticketStateId
          let actions = validActionLabelsForTicketState ticketStateId
          return $ Just $ TicketInfo tsd tVal tImg ticketStateLabel actions

lookupTicketInfoByUserAC :: (GYTxQueryMonad m) => AssetClass -> m (Maybe TicketInfo)
lookupTicketInfoByUserAC ticketUserAC = do
  let ticketRefAC = deriveRefFromUserAC ticketUserAC
  lookupTicketInfoByRefAC ticketRefAC

-- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
lookupActiveRaffles :: (GYTxQueryMonad m) => m [RaffleInfo]
lookupActiveRaffles = do
  allUTxOs <- lookupUTxOsAtValidator raffleizeValidatorGY
  let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
  let raffleUTxOs = utxosToList validUTxOs
  tr <- getTimeRangeForNextNSlots 1
  return $ mapMaybe (`raffleInfoFromUTxO` tr) raffleUTxOs

lookupTicketsOfAddress :: (GYTxQueryMonad m) => GYAddress -> m [TicketInfo]
lookupTicketsOfAddress addr = do
  utxos <- lookupUTxOsAtAddress addr
  let val = getValueBalance utxos
  let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue val
  lookupTicketInfosByACs raffleizeUserTokens

lookupRafflesOfAddress :: (GYTxQueryMonad m) => GYAddress -> m [RaffleInfo]
lookupRafflesOfAddress addr = do
  utxos <- lookupUTxOsAtAddress addr
  let val = getValueBalance utxos
  let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue val
  lookupRaffleInfosByACs (deriveRefFromUserAC <$> raffleizeUserTokens)

lookupRafflesOfAddressses :: (GYTxQueryMonad m) => [GYAddress] -> m [RaffleInfo]
lookupRafflesOfAddressses addrs = do
  utxos <- mapM lookupUTxOsAtAddress addrs
  let val = getValueBalance <$> utxos
  let raffleizeUserTokens = concatMap getMyRaffleizeUserTokensFromValue val
  lookupRaffleInfosByACs (deriveRefFromUserAC <$> raffleizeUserTokens)

getTimeRangeForNextNSlots :: (GYTxQueryMonad m, HasCallStack) => Integer -> m POSIXTimeRange
getTimeRangeForNextNSlots i = do
  now <- slotOfCurrentBlock
  let upperSlot = unsafeAdvanceSlot now (fromInteger i)
  lower <- timeToPlutus <$> slotToBeginTime now
  upper <- timeToPlutus <$> slotToEndTime upperSlot
  return $ PlutusLedgerApi.V1.Interval.intersection (PlutusLedgerApi.V1.Interval.from lower) (PlutusLedgerApi.V1.Interval.to upper)

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
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
getUTxOWithStateToken :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> GYAddress -> m GYUTxO
getUTxOWithStateToken refAC addr = do
  utxo <- lookupUTxOWithStateToken refAC addr
  maybe (throwError (GYQueryUTxOException (GYNoUtxosAtAddress [addr]))) return utxo

getRaffleStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (RaffleStateData, Value)
getRaffleStateDataAndValue raffleId =
  do
    raffleValidatorAddr <- scriptAddress raffleizeValidatorGY
    utxo <- getUTxOWithStateToken raffleId raffleValidatorAddr
    maybe (throwError (GYApplicationException RaffleizeDatumNotFound)) return $ rsdAndValueFromUTxO utxo

getRaffleStateValueAndImage :: (GYTxQueryMonad m) => AssetClass -> m (RaffleStateData, Value, String)
getRaffleStateValueAndImage raffleId =
  do
    raffleValidatorAddr <- scriptAddress raffleizeValidatorGY
    utxo <- getUTxOWithStateToken raffleId raffleValidatorAddr
    maybe (throwError (GYApplicationException RaffleizeDatumNotFound)) return $ rsdValueAndImageFromUTxO utxo

getTicketStateDataAndValue :: (HasCallStack, GYTxQueryMonad m) => AssetClass -> m (TicketStateData, Value)
getTicketStateDataAndValue ticketId =
  do
    ticketValidatorAddr <- scriptAddress ticketValidatorGY
    utxo <- getUTxOWithStateToken ticketId ticketValidatorAddr
    maybe (throwError (GYApplicationException TicketDatumNotFound)) return $ tsdAndValueFromUTxO utxo

getTicketStateDataAndValueAndImage :: (GYTxQueryMonad m) => AssetClass -> m (TicketStateData, Value, String)
getTicketStateDataAndValueAndImage ticketId =
  do
    ticketValidatorAddr <- scriptAddress ticketValidatorGY
    utxo <- getUTxOWithStateToken ticketId ticketValidatorAddr
    maybe (throwError (GYApplicationException TicketDatumNotFound)) return $ tsdValueAndImageFromUTxO utxo