module RestAPI where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT (ExceptT), handleE)
import Data.Conduit.Combinators (concatMapM)
import Data.Swagger
import Data.Swagger.Internal.Schema (plain)
import GeniusYield.Types
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Middleware.Cors
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Transactions
import Servant
import Servant.Conduit ()
import Servant.Swagger

type RaffleizeAPI =
  "build-tx" :> ReqBody '[JSON] RaffleizeInteraction :> Post '[JSON] String
    :<|> "submit-tx" :> ReqBody '[JSON] AddWitAndSubmitParams :> Post '[JSON] GYTxId
    :<|> "submit-tx2" :> ReqBody '[JSON] AddWitAndSubmitParams :> StreamPost NewlineFraming JSON (ConduitT () () IO ())
    :<|> "raffles" :> Get '[JSON] [RaffleInfo]
    :<|> "user-raffles" :> ReqBody '[JSON] [GYAddress] :> Post '[JSON] [RaffleInfo]
    :<|> "user-raffles2" :> Capture "address" GYAddress :> Get '[JSON] [RaffleInfo]
    :<|> "user-tickets" :> Capture "address" GYAddress :> Get '[JSON] [TicketInfo]
    :<|> "sse" :> StreamGet NewlineFraming JSON (ConduitT () Int IO ())

raffleizeServer :: RaffleizeOffchainContext -> ServerT RaffleizeAPI IO
raffleizeServer roc@RaffleizeOffchainContext {..} =
  handleInteraction roc
    :<|> handleSubmit providerCtx
    :<|> handleSubmitAndAwait providerCtx
    :<|> handleGetRaffles providerCtx
    :<|> handleGetRafflesByAddresses providerCtx
    :<|> handleGetRafflesByAddress providerCtx
    :<|> handleGetMyTickets providerCtx
    :<|> handdleSSE

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

handleSubmit :: ProviderCtx -> AddWitAndSubmitParams -> IO GYTxId
handleSubmit providerCtx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  gySubmitTx (ctxProviders providerCtx) $ makeSignedTransaction awasTxWit txBody

handleSubmitAndAwait :: ProviderCtx -> AddWitAndSubmitParams -> IO (ConduitT () () IO ())
handleSubmitAndAwait providerCtx params = do
  let ctxProv = ctxProviders providerCtx
  return $
    yieldM (handleSubmit providerCtx params)
      .| printC
      .| mapMC (gyAwaitTxConfirmed ctxProv (GYAwaitTxParameters 30 10000000 1))
      .| printC

handdleSSE :: IO (ConduitT () Int IO ())
handdleSSE = return $ yieldMany [1 :: Int .. 10000] .| mapMC (\i -> threadDelay 10000000 >> return i)

-- TODO - FIX THIS
instance ToSchema (ConduitT () Int IO ()) where
  declareNamedSchema _ = plain $ sketchSchema @() ()
instance ToSchema (ConduitT () () IO ()) where
  declareNamedSchema _ = plain $ sketchSchema @() ()