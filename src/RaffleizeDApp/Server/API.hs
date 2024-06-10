module RaffleizeDApp.Server.API where

import Control.Monad.Reader
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleInfo)
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.Server.Queries
import RaffleizeDApp.TxBuilding.Context
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
    :<|> "raffles" :> Get '[JSON] [RaffleInfo]
    :<|> "info" :> Get '[JSON] RaffleInfo

raffleizeApi :: Proxy RaffleizeAPI
raffleizeApi = Proxy

-- apiSwagger :: Swagger
-- apiSwagger = toSwagger raffleizeApi

handleInteraction :: RaffleizeOffchainContext -> RaffleizeInteraction -> IO String
handleInteraction roc i = runReaderT (interactionToHexEncodedCBOR i) roc

raffleizeServer :: RaffleizeOffchainContext -> ServerT RaffleizeAPI IO
raffleizeServer roc@RaffleizeOffchainContext {..} = handleInteraction roc :<|> handleLookup :<|> handleGetRaffles providerCtx :<|> handleGetOneRaffle providerCtx