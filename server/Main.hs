module Main where

import Control.Exception (try)
import Control.Monad.Trans.Except
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import RaffleizeDApp.Constants
import RaffleizeDApp.Server.API
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import Servant
import Text.Read (read)

main :: IO ()
main = do
  atlasConfig <- fromJust <$> decodeConfigFile @GYCoreConfig atlasCoreConfig
  tbCtx <- fromJust <$> decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  putStrLn "Loading Providers ..."
  withCfgProviders atlasConfig (read @GYLogNamespace "raffleizeserver") $ \providers -> do
    let port = 8081
        pCtx = ProviderCtx atlasConfig providers
    putStrLn $ "Starting server at \n " <> "http://localhost:" <> show port
    run port $ app tbCtx pCtx

app :: RaffleizeTxBuildingContext -> ProviderCtx -> Application
app tbCtx pCtx =
  cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [HttpTypes.hContentType]}) $
    serve raffleizeApi $
      hoistServer raffleizeApi (Handler . ExceptT . try) $
        raffleizeServer tbCtx pCtx