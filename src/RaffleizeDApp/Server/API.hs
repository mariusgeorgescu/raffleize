module RaffleizeDApp.Server.API where

import Control.Monad.Reader

import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleStateData)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions (RaffleizeInteraction, RaffleizeTxBuildingContext, interactionToHexEncodedCBOR)
import RaffleizeDApp.TxBuilding.Transactions
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

handleLookup :: IO String
handleLookup = return "hello"

handleGetRaffles :: IO [RaffleStateData]
handleGetRaffles = runContextWithCfgProviders "get raffles" queryRaffles

raffleizeServer :: RaffleizeTxBuildingContext -> ProviderCtx -> ServerT RaffleizeAPI IO
raffleizeServer r p = handleInteraction r p :<|> handleLookup :<|> handleGetRaffles