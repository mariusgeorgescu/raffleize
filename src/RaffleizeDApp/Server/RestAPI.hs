module RestAPI where

import Conduit (ConduitT, yieldM)
import Control.Exception (try)
import Control.Lens
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.ByteString.Lazy.UTF8 as LBSUTF8
import Data.Conduit.Combinators ()
import Data.Swagger
import Data.Swagger.Internal.Schema (plain)
import GeniusYield.Imports qualified as GeniusYield.Types.Tx
import GeniusYield.Types
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Middleware.Cors
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Transactions
import Servant
import Servant.API.EventStream
import Servant.Conduit ()
import Servant.Swagger
import Control.Monad.IO.Class (MonadIO(liftIO))

type RaffleizeAPI = RaffleizeREST :<|> RaffleizeSSE

type RaffleizeREST =
  "build-tx" :> ReqBody '[JSON] RaffleizeInteraction :> Post '[JSON] String
    :<|> "submit-tx" :> ReqBody '[JSON] AddWitAndSubmitParams :> Post '[JSON] String
    :<|> "raffles" :> Get '[JSON] [RaffleInfo]
    :<|> "raffle" :> Capture "raffleId" GYAssetClass :> Get '[JSON] (Maybe RaffleInfo)
    :<|> "user-raffles" :> ReqBody '[JSON] [GYAddress] :> Post '[JSON] [RaffleInfo]
    :<|> "user-raffles2" :> Capture "address" GYAddress :> Get '[JSON] [RaffleInfo]
    :<|> "user-tickets" :> Capture "address" GYAddress :> Get '[JSON] [TicketInfo]

type RaffleizeSSE = "submit-tx-sse" :> Capture "txid" String :> ServerSentEvents (RecommendedEventSourceHeaders (ConduitT () Int IO ()))

raffleizeServer :: RaffleizeOffchainContext -> ServerT RaffleizeAPI IO
raffleizeServer roc@RaffleizeOffchainContext {..} =
  ( handleInteraction roc
      :<|> handleSubmit providerCtx
      :<|> handleGetRaffles providerCtx
      :<|> handleGetRaffleById providerCtx
      :<|> handleGetRafflesByAddresses providerCtx
      :<|> handleGetRafflesByAddress providerCtx
      :<|> handleGetMyTickets providerCtx
  )
    :<|> handleSubmitSSE providerCtx

raffleizeRest :: Proxy RaffleizeREST
raffleizeRest = Proxy

raffleizeApi :: Proxy RaffleizeAPI
raffleizeApi = Proxy

apiSwagger :: Swagger
apiSwagger =
  toSwagger raffleizeRest
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

handleGetRaffleById :: ProviderCtx -> GYAssetClass -> IO  (Maybe RaffleInfo)
handleGetRaffleById pCtx gyRaffleId =  do
  liftIO $ putStrLn $ "Lookup for raffle: " <> show gyRaffleId
  mri <- runQuery pCtx $ lookupRaffleInfoByRefAC (assetClassToPlutus gyRaffleId)
  liftIO $ print mri
  return mri
  
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

handleSubmit :: ProviderCtx -> AddWitAndSubmitParams -> IO String
handleSubmit providerCtx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  txId <- gySubmitTx (ctxProviders providerCtx) $ makeSignedTransaction awasTxWit txBody
  liftIO $ putStrLn $ "Submitted tx: " <> show txId
  return $ show txId

handleSubmitSSE :: ProviderCtx -> String -> IO (RecommendedEventSourceHeaders (ConduitT () Int IO ()))
handleSubmitSSE providerCtx txIdStr = do
  let txId = GeniusYield.Types.Tx.fromString txIdStr :: GYTxId
  let ctxProv = ctxProviders providerCtx
  liftIO $ putStrLn $ "Await tx: " <> show txId
  return $ recommendedEventSourceHeaders (yieldM (gyAwaitTxConfirmed ctxProv (GYAwaitTxParameters 30 10000000 1) txId  >> putStrLn "Confirmed" >> return 1))

-- TODO - FIX THIS

instance ToServerEvent Int where
  toServerEvent i = ServerEvent Nothing Nothing (LBSUTF8.fromString $ show i)

instance ToSchema (ConduitT () () IO ()) where
  declareNamedSchema _ = plain $ sketchSchema @() ()