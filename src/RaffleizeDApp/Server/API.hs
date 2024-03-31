module RaffleizeDApp.Server.API where

import PlutusLedgerApi.V1.Value
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleStateData)
import RaffleizeDApp.TxBuilding.Interactions (RaffleizeInteraction)
import Servant
import Servant.Swagger

type RaffleizeAPI =
  "build-tx" :> RaffleizeInteractions
    :<|> "lookup" :> RaffleizeLookups

-- | Type for our Raffleize Servant API.
type RaffleizeInteractions =
  ReqBody '[JSON] RaffleizeInteraction
    :> Post '[JSON] (Maybe AssetClass)

-- | Type for our Raffleize Servant API.
type RaffleizeLookups = Get '[JSON] [RaffleStateData]
