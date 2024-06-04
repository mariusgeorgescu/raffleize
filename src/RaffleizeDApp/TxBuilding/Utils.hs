module RaffleizeDApp.TxBuilding.Utils where

import GeniusYield.TxBuilder
import GeniusYield.Types

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import RaffleizeDApp.Constants
import RaffleizeDApp.TxBuilding.Validators
import RaffleizeDApp.OnChain.RaffleizeLogic

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

showLink :: GYNetworkId -> Text -> Text -> Text
showLink nid s content = case nid of
  GYMainnet -> cexplorerMainnet <> s <> "/" <> content <> " "
  GYTestnetPreprod -> cexplorerPreprod <> s <> "/" <> content <> " "
  GYTestnetPreview -> cexplorerPreview <> s <> "/" <> content <> " "
  GYTestnetLegacy -> content
  GYPrivnet -> content


getMyRaffleIdsFromValue :: Value -> [AssetClass]
getMyRaffleIdsFromValue val =
  let
    raffleizeCS = mintingPolicyCurrencySymbol raffleizeMintingPolicyGY
   in
    [AssetClass (cs, deriveRefFromUserTN tn) | (cs, tn, _) <-  flattenValue val, raffleizeCS == cs ]

----
