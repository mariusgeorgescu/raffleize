module RaffleizeDApp.TxBuilding.Context where

import Control.Monad
import Control.Monad.Reader
import GeniusYield.GYConfig
import GeniusYield.Imports
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types

import RaffleizeDApp.Constants
import System.Environment

-- | Our Context.
data Ctx = Ctx
  { ctxCoreCfg :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: GYTxQueryMonadNode a -> ReaderT Ctx IO a
runQuery q = do
  ctx <- ask
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  liftIO $ runGYTxQueryMonadNode nid providers q

-- | Wraps our skeleton under `Identity` and calls `runTxF`.
runTxI ::
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadNode (GYTxSkeleton v) ->
  ReaderT Ctx IO GYTxBody
runTxI = coerce (runTxF @Identity)

-- | Tries to build for given skeletons wrapped under traversable structure.
runTxF ::
  Traversable t =>
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadNode (t (GYTxSkeleton v)) ->
  ReaderT Ctx IO (t GYTxBody)
runTxF addrs addr collateral skeleton = do
  ctx <- ask
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  liftIO $
    runGYTxMonadNodeF
      GYRandomImproveMultiAsset
      nid
      providers
      addrs
      addr
      ( collateral
          >>= ( \c ->
                  Just
                    ( getTxOutRefHex c
                    , False -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                    )
              )
      )
      skeleton

-- | Getting path for our core configuration.
parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    coreCfg : _ -> return coreCfg
    _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

-- | Getting path for our core configuration.
getCoreConfiguration :: IO GYCoreConfig
getCoreConfiguration = do
  coreCfgPath <- parseArgs
  coreConfigIO coreCfgPath

-- | Getting path for our core configuration.
readCoreConfiguration :: IO GYCoreConfig
readCoreConfiguration = coreConfigIO atlasCoreConfig -- Parsing our core configuration.

runContextWithCfgProviders :: GYLogNamespace -> ReaderT Ctx IO b -> IO b
runContextWithCfgProviders s m = do
  coreConfig <- readCoreConfiguration
  withCfgProviders coreConfig (s :: GYLogNamespace) $ \providers -> do
    let ctx = Ctx coreConfig providers
    runReaderT m ctx
