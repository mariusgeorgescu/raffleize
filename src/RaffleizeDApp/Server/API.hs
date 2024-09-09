module API where

import Control.Monad.Reader

import Control.Lens
import Data.Swagger
import GeniusYield.Types (GYAddress)
import Queries
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Transactions
import Servant
import Servant.Swagger

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
    :<|> "user-raffles" :> ReqBody '[JSON] [GYAddress] :> Post '[JSON] [RaffleInfo]
    :<|> "user-raffles2" :> Capture "address" GYAddress :> Get '[JSON] [RaffleInfo]
    :<|> "user-tickets" :> Capture "address" GYAddress :> Get '[JSON] [TicketInfo]

raffleizeApi :: Proxy RaffleizeAPI
raffleizeApi = Proxy

apiSwagger :: Swagger
apiSwagger =
  toSwagger raffleizeApi
    & info . title .~ "Raffleize API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API for the Raffleize Cardano DApp"
    & info . license ?~ "GPL-3.0 license"
    & host ?~ "http://raffleize.art"

handleInteraction :: RaffleizeOffchainContext -> RaffleizeInteraction -> IO String
handleInteraction roc i = do 
  print i
  runReaderT (interactionToHexEncodedCBOR i) roc

raffleizeServer :: RaffleizeOffchainContext -> ServerT RaffleizeAPI IO
raffleizeServer roc@RaffleizeOffchainContext {..} = handleInteraction roc :<|> handleLookup :<|> handleGetRaffles providerCtx :<|> handleGetRafflesByAddresses providerCtx :<|> handleGetRafflesByAddress providerCtx :<|> handleGetMyTickets providerCtx