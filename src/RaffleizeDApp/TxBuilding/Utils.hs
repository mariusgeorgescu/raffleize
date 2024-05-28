module RaffleizeDApp.TxBuilding.Utils where

import GeniusYield.TxBuilder
import GeniusYield.Types

import PlutusLedgerApi.V2
import RaffleizeDApp.Constants

------------------------

-- * Utilities

------------------------

pPOSIXTimeFromSlotInteger :: GYTxQueryMonad m => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . slotFromApi . fromInteger

pPOSIXTimeFromGYSlot :: GYTxQueryMonad m => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: GYTxQueryMonad m => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)

showLink :: GYNetworkId -> String -> String -> String
showLink nid s content = case nid of
  GYMainnet -> cexplorerMainnet <> s <> "/" <> content <> " "
  GYTestnetPreprod -> cexplorerPreprod <> s <> "/" <> content <> " "
  GYTestnetPreview -> cexplorerPreview <> s <> "/" <> content <> " "
  GYTestnetLegacy -> content
  GYPrivnet -> content