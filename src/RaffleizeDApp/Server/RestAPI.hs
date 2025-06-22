{-# OPTIONS_GHC -Wno-orphans #-}

module RestAPI where

import Conduit (ConduitT, yieldM)
import Control.Exception (try)
import Control.Lens ((&), (.~), (?~))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.ByteString.Lazy.UTF8 qualified as LBSUTF8 (fromString)
import Data.Conduit.Combinators ()
import Data.Function (on)
import Data.List (isSuffixOf, sortBy)
import Data.List qualified
import Data.Swagger (HasInfo (info), HasLicense (license), Swagger (..), ToSchema, description, sketchSchema, title, version)
import Data.Swagger.Internal.Schema (ToSchema (declareNamedSchema), plain)
import Data.Text qualified
import Data.Text.Encoding qualified
import GeniusYield.Imports qualified as GeniusYield.Types.Tx
import GeniusYield.Types
  ( GYAddress,
    GYAssetClass,
    GYAwaitTxParameters (GYAwaitTxParameters),
    GYProviders (gyAwaitTxConfirmed, gySubmitTx),
    GYTxId,
    assetClassToPlutus,
    getTxBody,
    makeSignedTransaction,
  )
import Network.HTTP.Types qualified as HttpTypes
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options (provideOptions)
import RaffleizeDApp.CustomTypes.RaffleTypes hiding (version)
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Transactions
import RaffleizeDApp.TxBuilding.Utils (veryFarPosixDate)
import Servant
import Servant.API.EventStream
  ( RecommendedEventSourceHeaders,
    ServerEvent (ServerEvent),
    ServerSentEvents,
    ToServerEvent (..),
    recommendedEventSourceHeaders,
  )
import Servant.Conduit ()
import Servant.Swagger
import Servant.Swagger.UI

newtype User = User
  { user :: Data.Text.Text
  }
  deriving (Eq, Show)

type Transactions =
  Summary "Build Raffleize Transaction"
    :> Description "Builds Transaction for Raffleize Interaction"
    :> "build-tx"
    :> ReqBody '[JSON] Interaction
    :> Post '[JSON] String
    :<|> Summary "Submit tx"
      :> Description "Submit transaction and returns transaction id"
      :> "submit-tx"
      :> ReqBody '[JSON] AddWitAndSubmitParams
      :> Post '[JSON] String

type Lookups =
  Summary "Get all raffles"
    :> Description "Get all active raffles information"
    :> "raffles"
    :> QueryParams "state" String
    :> QueryParam "isFinal" Bool
    :> QueryParam "sortBy" RaffleSortBy
    :> QueryParam "sortOrder" SortOrder
    :> Get '[JSON] [RaffleInfo]
    :<|> Summary "Get raffle by id"
      :> Description "Get raffle information with raffle id (AssetClass)"
      :> "raffle"
      :> Capture "raffleId" GYAssetClass
      :> Get '[JSON] (Maybe RaffleInfo)
    :<|> Summary "Get users's raffles"
      :> Description "Checks addreesses for raffle user tokens and returns the corresponding raffles information"
      :> "user-raffles"
      :> ReqBody '[JSON] [GYAddress]
      :> QueryParams "state" String
      :> QueryParam "isFinal" Bool
      :> QueryParam "sortBy" RaffleSortBy
      :> QueryParam "sortOrder" SortOrder
      :> Post '[JSON] [RaffleInfo]
    :<|> Summary "Get user's tickets"
      :> Description "Checks addreesses for ticket user tokens and returns the corresponding tickets information"
      :> "user-tickets"
      :> ReqBody '[JSON] [GYAddress]
      :> Post '[JSON] [TicketInfo]
    :<|> Summary "Get ticket by id"
      :> Description "Get ticket information with ticket id (AssetClass)"
      :> "ticket"
      :> Capture "ticketId" GYAssetClass
      :> Get '[JSON] (Maybe TicketInfo)

type RESTwoSwagger =
  Transactions
    :<|> Lookups

type REST =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> Transactions
    :<|> Lookups

type SSE =
  Summary "Submit Transaction with SSE"
    :> Description "Submit transaction and wait for server sent event when confirmed"
    :> "submit-tx-sse"
    :> Capture "txid" String
    :> ServerSentEvents (RecommendedEventSourceHeaders (ConduitT () Int IO ()))

type RaffleizeAPI = (BasicAuth "user-realm" User :> REST) :<|> SSE

--------
--------
--------
--------

transactionsServer :: RaffleizeOffchainContext -> ServerT Transactions IO
transactionsServer roc@RaffleizeOffchainContext {..} = handleInteraction roc :<|> handleSubmit providerCtx

lookupsServer :: RaffleizeOffchainContext -> ServerT Lookups IO
lookupsServer RaffleizeOffchainContext {..} =
  handleGetRaffles providerCtx
    :<|> handleGetRaffleById providerCtx
    :<|> handleGetRafflesByAddresses providerCtx
    :<|> handleGeTicketsByAddresses providerCtx
    :<|> handleGetTicketById providerCtx

restServer :: RaffleizeOffchainContext -> ServerT REST IO
restServer roc = swaggerSchemaUIServerT apiSwagger :<|> transactionsServer roc :<|> lookupsServer roc

raffleizeServer :: RaffleizeOffchainContext -> ServerT RaffleizeAPI IO
raffleizeServer roc@RaffleizeOffchainContext {..} =
  const -- usr
    (restServer roc)
    :<|> handleSubmitSSE providerCtx

proxyRestwoSwagger :: Proxy RESTwoSwagger
proxyRestwoSwagger = Proxy

proxySSE :: Proxy SSE
proxySSE = Proxy

proxyRaffleizeApi :: Proxy RaffleizeAPI
proxyRaffleizeApi = Proxy

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyRestwoSwagger
    & info . title .~ "Raffleize API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API for the Raffleize Cardano DApp"
    & info . license ?~ "GPL-3.0 license"

-- & host ?~ "localhost"

restAPIapp :: Text -> Text -> RaffleizeOffchainContext -> Application
restAPIapp usr pass ctx =
  cors
    ( \req ->
        let originHeader = Data.List.lookup hOrigin (requestHeaders req)
         in case originHeader of
              Just o ->
                Just
                  simpleCorsResourcePolicy
                    { corsOrigins = Just ([o], True), -- Reflect request's Origin dynamically
                      corsMethods = ["GET", "POST", "PUT", "OPTIONS", "DELETE"],
                      corsRequestHeaders = simpleHeaders <> [HttpTypes.hAuthorization],
                      corsExposedHeaders = Just $ simpleHeaders <> [HttpTypes.hAuthorization],
                      corsVaryOrigin = True,
                      corsRequireOrigin = False,
                      corsIgnoreFailures = False,
                      corsMaxAge = Just 600
                    }
              Nothing -> Nothing -- If no origin set skips cors headers
    )
    $ provideOptions proxyRestwoSwagger
    $ provideOptions proxySSE
    $ serveWithContext proxyRaffleizeApi basicCtx
    $ hoistServerWithContext proxyRaffleizeApi (Proxy :: Proxy '[BasicAuthCheck User]) (Servant.Handler . ExceptT . try)
    $ raffleizeServer ctx
  where
    basicCtx = basicAuthServerContext usr pass

-------------
-------------
-------------
-------------

filterAndSortRaffles :: [RaffleInfo] -> [String] -> Maybe Bool -> Maybe RaffleSortBy -> Maybe SortOrder -> [RaffleInfo]
filterAndSortRaffles raffles states mIsFinal mSortBy mSortOrder =
  let filteredWithFinal = case mIsFinal of
        (Just isFinal) ->
          let filterFunc = if isFinal then ("FINAL" `isSuffixOf`) else not . ("FINAL" `isSuffixOf`)
           in filter (filterFunc . riStateLabel) raffles
        Nothing -> raffles
      filteredWithStates = if null states then filteredWithFinal else filter ((`elem` states) . riStateLabel) filteredWithFinal

      sorted =
        case mSortBy of
          Just CommitDeadline -> sortByAttr (rCommitDDL . rConfig . riRsd)
          Just RevealDeadline -> sortByAttr (rRevealDDL . rConfig . riRsd)
          Just NextDeadline -> sortByAttr getNextDeadline
          Just State -> sortByAttr riStateLabel
          _ -> filteredWithStates
        where
          sortByAttr f =
            let cmp = compare `on` f
                sorter = if mSortOrder == Just Desc then flip cmp else cmp
             in Data.List.sortBy sorter filteredWithStates
          getNextDeadline ri =
            let commitDeadline = rCommitDDL (rConfig (riRsd ri))
                revealDeadline = rRevealDDL (rConfig (riRsd ri))
                state = riStateLabel ri
             in case state of
                  "NEW" -> commitDeadline
                  "COMMITTING" -> commitDeadline
                  "REVEALING" -> revealDeadline
                  _ -> veryFarPosixDate

   in sorted

handleGetRaffles :: ProviderCtx -> [String] -> Maybe Bool -> Maybe RaffleSortBy -> Maybe SortOrder -> IO [RaffleInfo]
handleGetRaffles pCtx states mIsFinal mSortBy mSortOrder = do
  raffles <- runQuery pCtx lookupActiveRaffles
  return $ filterAndSortRaffles raffles states mIsFinal mSortBy mSortOrder

handleGetRaffleById :: ProviderCtx -> GYAssetClass -> IO (Maybe RaffleInfo)
handleGetRaffleById pCtx gyRaffleId = do
  liftIO $ putStrLn $ "Lookup for raffle: " <> show gyRaffleId
  runQuery pCtx $ lookupRaffleInfoByRefAC (assetClassToPlutus gyRaffleId)

handleGetRafflesByAddresses :: ProviderCtx -> [GYAddress] -> [String] -> Maybe Bool -> Maybe RaffleSortBy -> Maybe SortOrder -> IO [RaffleInfo]
handleGetRafflesByAddresses pCtx addrs states mIsFinal mSortBy mSortOrder = do
  raffles <- runQuery pCtx (lookupRafflesOfAddresses addrs)
  return $ filterAndSortRaffles raffles states mIsFinal mSortBy mSortOrder

-- handleGetOneRaffle :: ProviderCtx -> IO RaffleInfo
-- handleGetOneRaffle pCtx = head <$> handleGetRaffles pCtx

handleGetTicketById :: ProviderCtx -> GYAssetClass -> IO (Maybe TicketInfo)
handleGetTicketById pCtx gyTicketId = do
  liftIO $ putStrLn $ "Lookup for ticket: " <> show gyTicketId
  runQuery pCtx $ lookupTicketInfoByRefAC (assetClassToPlutus gyTicketId)

handleGeTicketsByAddresses :: ProviderCtx -> [GYAddress] -> IO [TicketInfo]
handleGeTicketsByAddresses pCtx addr = runQuery pCtx (lookupTicketsOfAddresses addr)

handleInteraction :: RaffleizeOffchainContext -> Interaction -> IO String
handleInteraction roc i = do
  print i
  runReaderT (interactionToHexEncodedCBOR i) roc

handleSubmit :: ProviderCtx -> AddWitAndSubmitParams -> IO String
handleSubmit providerCtx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  txId <- gySubmitTx (ctxProviders providerCtx) $ makeSignedTransaction awasTxWit txBody
  liftIO $ putStrLn $ "Submitted tx: " <> show txId
  return $ show txId

----------
----------
----------

handleSubmitSSE :: ProviderCtx -> String -> IO (RecommendedEventSourceHeaders (ConduitT () Int IO ()))
handleSubmitSSE providerCtx txIdStr = do
  let txId = GeniusYield.Types.Tx.fromString txIdStr :: GYTxId
  let ctxProv = ctxProviders providerCtx
  liftIO $ putStrLn $ "Await tx: " <> show txId
  return $ recommendedEventSourceHeaders (yieldM (gyAwaitTxConfirmed ctxProv (GYAwaitTxParameters 30 10000000 1) txId >> putStrLn "Confirmed" >> return 1))

-- TODO - FIX THIS
instance ToServerEvent Int where
  toServerEvent i = ServerEvent Nothing Nothing (LBSUTF8.fromString $ show i)

instance ToSchema (ConduitT () () IO ()) where
  declareNamedSchema _ = plain $ sketchSchema @() ()

instance FromHttpApiData RaffleSortBy where
  parseQueryParam :: Text -> Either Text RaffleSortBy
  parseQueryParam "CommitDeadline" = Right CommitDeadline
  parseQueryParam "RevealDeadline" = Right RevealDeadline
  parseQueryParam "NextDeadline" = Right NextDeadline
  parseQueryParam "State" = Right State
  parseQueryParam _ = Left "Invalid sort order"

instance FromHttpApiData SortOrder where
  parseQueryParam :: Text -> Either Text SortOrder
  parseQueryParam "Asc" = Right Asc
  parseQueryParam "Desc" = Right Desc
  parseQueryParam _ = Left "Invalid sort order"

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: Text -> Text -> BasicAuthCheck User
authCheck usr pass =
  let checkk (BasicAuthData username password) =
        if Data.Text.Encoding.decodeUtf8 username == usr && Data.Text.Encoding.decodeUtf8 password == pass
          then return (Authorized (User usr))
          else return Unauthorized
   in BasicAuthCheck checkk

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Text -> Text -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext usr pass = authCheck usr pass :. EmptyContext