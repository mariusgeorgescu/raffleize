module Main where

import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe qualified
import Data.String
import Data.Text qualified
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets.Connection
import RaffleizeDApp.Constants
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Utils
import RestAPI
import Servant
import System.Environment
import Text.Read
import WebsocketsAPI

getPortFromEnv :: IO Int
getPortFromEnv = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return 8082
    Just p -> return (read p)

getBasicAuthFromEnv :: IO (Text, Text)
getBasicAuthFromEnv = do
  user <- fromMaybe "cardano" <$> lookupEnv "BASIC_USER"
  pass <- fromMaybe "lovelace" <$> lookupEnv "BASIC_PASS"
  return (Data.Text.pack user, Data.Text.pack pass)

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)
  atlasConfig <- Data.Maybe.fromMaybe (error "Mandatory configuration file not found") <$> decodeConfigFile @GYCoreConfig atlasCoreConfig
  tbCtx <- fromJust <$> decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  let host = "0.0.0.0"
  port <- getPortFromEnv
  putStrLn "Loading Providers ..."
  withCfgProviders atlasConfig (read @GYLogNamespace "raffleizeserver") $ \providers -> do
    let pCtx = ProviderCtx atlasConfig providers
    let raffleizeContext = RaffleizeOffchainContext tbCtx pCtx
    putStrLn $ "Starting server at " <> host <> " " <> show port
    putStrLn $ "Swagget-UI at : http://" <> host <> ":" <> show port <> "/swagger-ui"
    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings -- host and port customized for heroku
    (user, pass) <- getBasicAuthFromEnv
    runSettings settings $ combinedApp user pass raffleizeContext

-- WAI application, combining WebSocket handling with other HTTP handlers
combinedApp :: Text -> Text -> RaffleizeOffchainContext -> Application
combinedApp user pass raffleizeContext = websocketsOr defaultConnectionOptions websocketsServerApp (restAPIapp user pass raffleizeContext)