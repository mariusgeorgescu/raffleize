module RaffleizeDApp.TxBuilding.Utils where

import GeniusYield.TxBuilder
import GeniusYield.Types

import PlutusLedgerApi.V2
import RaffleizeDApp.TxBuilding.Exceptions
import RaffleizeDApp.TxBuilding.Lookups

------------------------

-- * Utilities

------------------------

gyGetInlineDatumAndValue' :: MonadError GYTxMonadException m => GYUTxO -> m (GYDatum, GYValue)
gyGetInlineDatumAndValue' utxo = maybe (throwError (GYApplicationException InlineDatumNotFound)) return $ gyGetInlineDatumAndValue utxo

pPOSIXTimeFromSlotInteger :: GYTxQueryMonad m => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . slotFromApi . fromInteger

pPOSIXTimeFromGYSlot :: GYTxQueryMonad m => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: GYTxQueryMonad m => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)
