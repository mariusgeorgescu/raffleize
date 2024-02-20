module RaffleizeDApp.TxBuilding.Utils where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types

import PlutusLedgerApi.V2

------------------------

-- * Utilities

------------------------

gyGetInlineDatumAndValue :: (HasCallStack, GYTxQueryMonad m) => GYUTxO -> m (GYDatum, GYValue)
gyGetInlineDatumAndValue utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> return (datum, utxoValue utxo)
  _ -> throwError (GYQueryDatumException (GYNoDatumHash utxo))

pPOSIXTimeFromSlotInteger :: GYTxQueryMonad m => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . slotFromApi . fromInteger

pPOSIXTimeFromGYSlot :: GYTxQueryMonad m => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: GYTxQueryMonad m => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)
