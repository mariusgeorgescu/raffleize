module RaffleizeDApp.Server.API where

import Control.Monad.Reader

import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleStateData)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions (RaffleizeInteraction, RaffleizeTxBuildingContext, interactionToHexEncodedCBOR)
import Servant

type RaffleizeAPI =
  "build-tx" :> InteractionInterace

-- :<|> "lookup" :> LookupsInterface

-- | Type for our Raffleize Servant API.
type InteractionInterace =
  ReqBody '[JSON] RaffleizeInteraction
    :> Post '[JSON] String

-- | Type for our Raffleize Servant API.
type LookupsInterface =
  "raffles" :> Get '[JSON] [RaffleStateData]
    :<|> "raffles" :> Get '[JSON] [RaffleStateData]

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
raffleizeServer = handleInteraction