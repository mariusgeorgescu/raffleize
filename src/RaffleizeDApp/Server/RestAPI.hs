module RestAPI where

import Control.Exception (try)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Swagger
import GeniusYield.Types
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Middleware.Cors
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Transactions
import Servant
import Servant.Swagger

type RaffleizeAPI =
  "build-tx" :> ReqBody '[JSON] RaffleizeInteraction :> Post '[JSON] String
    :<|> "submit-tx" :> ReqBody '[JSON] AddWitAndSubmitParams :> Post '[JSON] ()
    :<|> "raffles" :> Get '[JSON] [RaffleInfo]
    :<|> "user-raffles" :> ReqBody '[JSON] [GYAddress] :> Post '[JSON] [RaffleInfo]
    :<|> "user-raffles2" :> Capture "address" GYAddress :> Get '[JSON] [RaffleInfo]
    :<|> "user-tickets" :> Capture "address" GYAddress :> Get '[JSON] [TicketInfo]

raffleizeServer :: RaffleizeOffchainContext -> ServerT RaffleizeAPI IO
raffleizeServer roc@RaffleizeOffchainContext {..} =
  handleInteraction roc
    :<|> handleSubmit providerCtx
    :<|> handleGetRaffles providerCtx
    :<|> handleGetRafflesByAddresses providerCtx
    :<|> handleGetRafflesByAddress providerCtx
    :<|> handleGetMyTickets providerCtx

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

restAPIapp :: RaffleizeOffchainContext -> Application
restAPIapp raffleizeContext =
  cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [HttpTypes.hContentType]}) $
    serve raffleizeApi $
      hoistServer raffleizeApi (Handler . ExceptT . try) $
        raffleizeServer raffleizeContext

-------------

handleGetRaffles :: ProviderCtx -> IO [RaffleInfo]
handleGetRaffles pCtx = runQuery pCtx lookupActiveRaffles

handleGetRafflesByAddress :: ProviderCtx -> GYAddress -> IO [RaffleInfo]
handleGetRafflesByAddress pCtx addrs = runQuery pCtx (lookupRafflesOfAddress addrs)

handleGetRafflesByAddresses :: ProviderCtx -> [GYAddress] -> IO [RaffleInfo]
handleGetRafflesByAddresses pCtx addrs = runQuery pCtx (lookupRafflesOfAddressses addrs)

handleGetOneRaffle :: ProviderCtx -> IO RaffleInfo
handleGetOneRaffle pCtx = head <$> handleGetRaffles pCtx

handleGetMyTickets :: ProviderCtx -> GYAddress -> IO [TicketInfo]
handleGetMyTickets pCtx addr = runQuery pCtx (lookupTicketsOfAddress addr)

handleInteraction :: RaffleizeOffchainContext -> RaffleizeInteraction -> IO String
handleInteraction roc i = do
  print i
  runReaderT (interactionToHexEncodedCBOR i) roc

handleSubmit :: ProviderCtx -> AddWitAndSubmitParams -> IO ()
handleSubmit providerCtx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  void $ gySubmitTx (ctxProviders providerCtx) $ makeSignedTransaction awasTxWit txBody