module Main where

import RestAPI

import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe qualified
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets.Connection
import RaffleizeDApp.Constants
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Utils
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
    putStrLn $ "Starting server at " <> show host <> " " <> show port
    let settings = setHost host $ setPort port defaultSettings -- host and port customized for heroku
    runSettings settings $ combinedApp raffleizeContext

-- WAI application, combining WebSocket handling with other HTTP handlers
combinedApp :: RaffleizeOffchainContext -> Application
combinedApp raffleizeContext = websocketsOr defaultConnectionOptions websocketsServerApp (restAPIapp raffleizeContext)