{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (try)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import GeniusYield.GYConfig (GYCoreConfig, withCfgProviders)
import GeniusYield.Types (GYLogNamespace)
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import RaffleizeDApp.Constants (
  atlasCoreConfig,
  raffleizeValidatorsConfig,
 )
import RaffleizeDApp.Server.API (raffleizeApi, raffleizeServer)
import RaffleizeDApp.TUI.Utils (decodeConfigFile)
import RaffleizeDApp.TxBuilding.Context (
  ProviderCtx (ProviderCtx),
 )
import RaffleizeDApp.TxBuilding.Interactions (
  RaffleizeTxBuildingContext,
 )
import Servant (
  Application,
  Handler (Handler),
  hoistServer,
  serve,
 )
import System.Environment (lookupEnv)
import Text.Read (read)

getPortFromEnv :: IO Int
getPortFromEnv = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return 8082
    Just p -> return (read p)

main :: IO ()
main = do
  atlasConfig <- fromJust <$> decodeConfigFile @GYCoreConfig atlasCoreConfig
  tbCtx <- fromJust <$> decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  let host = "0.0.0.0"
  port <- getPortFromEnv
  putStrLn "Loading Providers ..."
  withCfgProviders atlasConfig (read @GYLogNamespace "raffleizeserver") $ \providers -> do
    let pCtx = ProviderCtx atlasConfig providers
    putStrLn $ "Starting server at " <> show host <> " " <> show port
    let settings = setHost host $ setPort port defaultSettings -- host and port customized for heroku
    runSettings settings $ app tbCtx pCtx

app :: RaffleizeTxBuildingContext -> ProviderCtx -> Application
app tbCtx pCtx =
  cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [HttpTypes.hContentType]}) $
    serve raffleizeApi $
      hoistServer raffleizeApi (Handler . ExceptT . try) $
        raffleizeServer tbCtx pCtx