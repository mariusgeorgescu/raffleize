module RaffleizeDApp.TxBuilding.Context where

import Control.Monad.Reader
import GeniusYield.GYConfig
import GeniusYield.Imports
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types
import RaffleizeDApp.CustomTypes.TransferTypes

-- | Our Context.
data ProviderCtx = ProviderCtx
  { ctxCoreCfg :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

data RaffleizeTxBuildingContext = RaffleizeTxBuildingContext
  { raffleValidatorRef :: GYTxOutRef
  , ticketValidatorRef :: GYTxOutRef
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data RaffleizeOffchainContext = RaffleizeOffchainContext
  { raffleizeTxBuildingCtx :: RaffleizeTxBuildingContext
  , providerCtx :: ProviderCtx
  }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: ProviderCtx -> GYTxQueryMonadNode a -> IO a
runQuery ctx q = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  liftIO $ runGYTxQueryMonadNode nid providers q

-- | Wraps our skeleton under `Identity` and calls `runTxF`.
runTxI ::
  ProviderCtx ->
  UserAddresses ->
  GYTxMonadNode (GYTxSkeleton v) ->
  IO GYTxBody
runTxI = coerce (runTxF @Identity)

-- | Tries to build for given skeletons wrapped under traversable structure.
runTxF ::
  Traversable t =>
  ProviderCtx ->
  UserAddresses ->
  GYTxMonadNode (t (GYTxSkeleton v)) ->
  IO (t GYTxBody)
runTxF providerCtx UserAddresses {usedAddresses, changeAddress, reservedCollateral} skeleton = do
  let nid = cfgNetworkId $ ctxCoreCfg providerCtx
      providers = ctxProviders providerCtx
  liftIO $
    runGYTxMonadNodeF
      GYRandomImproveMultiAsset
      nid
      providers
      usedAddresses
      changeAddress
      ( reservedCollateral
          >>= ( \c ->
                  Just
                    ( getTxOutRefHex c
                    , False -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                    )
              )
      )
      skeleton

-- -- | Getting path for our core configuration.
-- parseArgs :: IO FilePath
-- parseArgs = do
--   args <- getArgs
--   case args of
--     coreCfg : _ -> return coreCfg
--     _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

-- -- | Getting path for our core configuration.
-- getCoreConfiguration :: IO GYCoreConfig
-- getCoreConfiguration = do
--   coreCfgPath <- parseArgs
--   coreConfigIO coreCfgPath

-- -- | Getting path for our core configuration.
-- readCoreConfiguration :: IO GYCoreConfig
-- readCoreConfiguration = coreConfigIO atlasCoreConfig -- Parsing our core configuration.

-- runContextWithCfgProviders :: GYLogNamespace -> ReaderT ProviderCtx IO b -> IO b
-- runContextWithCfgProviders s m = do
--   coreConfig <- readCoreConfiguration
--   withCfgProviders coreConfig (s :: GYLogNamespace) $ \providers -> do
--     let ctx = ProviderCtx coreConfig providers
--     runReaderT m ctx

-- runContextWithProviders :: GYLogNamespace -> ReaderT ProviderCtx IO b -> IO b
-- runContextWithProviders s m = do
--   coreConfig <- readCoreConfiguration
--   withCfgProviders coreConfig (s :: GYLogNamespace) $ \providers -> do
--     let ctx = ProviderCtx coreConfig providers
--     runReaderT m ctx
