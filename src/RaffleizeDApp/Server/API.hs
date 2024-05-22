module RaffleizeDApp.Server.API where

import Control.Monad.Reader

import PlutusLedgerApi.V1 qualified
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleStateData)
import RaffleizeDApp.Server.Queries
import RaffleizeDApp.Server.Types
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions (RaffleizeInteraction, RaffleizeTxBuildingContext, interactionToHexEncodedCBOR)
import Servant

type RaffleizeAPI =
  "build-tx" :> InteractionInterface
    :<|> LookupsInterface

-- | Type for our Raffleize Servant API.
type InteractionInterface =
  ReqBody '[JSON] RaffleizeInteraction
    :> Post '[JSON] String

-- | Type for our Raffleize Servant API.
type LookupsInterface =
  Get '[JSON] String
    :<|> "raffles" :> Get '[JSON] [(RaffleStateData, PlutusLedgerApi.V1.Value)]
    :<|> "raffle" :> Get '[JSON] RaffleStateData
    :<|> "value" :> Get '[JSON] PlutusLedgerApi.V1.Value
    :<|> "info" :> Get '[JSON] RaffleInfo

raffleizeApi :: Proxy RaffleizeAPI
raffleizeApi = Proxy

-- apiSwagger :: Swagger
-- apiSwagger = toSwagger raffleizeApi

handleInteraction :: RaffleizeTxBuildingContext -> ProviderCtx -> RaffleizeInteraction -> IO String
handleInteraction tbCtx pCtx i =
  let
    i' = runReader (interactionToHexEncodedCBOR i) tbCtx
   in
    runReaderT i' pCtx

raffleizeServer :: RaffleizeTxBuildingContext -> ProviderCtx -> ServerT RaffleizeAPI IO
raffleizeServer r p = handleInteraction r p :<|> handleLookup :<|> handleGetRaffles :<|> handleGetRaffle :<|> handleGetValue :<|> handleGetInfo