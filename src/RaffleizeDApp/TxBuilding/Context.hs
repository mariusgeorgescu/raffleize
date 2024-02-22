module RaffleizeDApp.TxBuilding.Context where

import GeniusYield.GYConfig
import GeniusYield.Imports
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types

-- | Our Context.
data Ctx = Ctx
  { ctxCoreCfg :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: Ctx -> GYTxQueryMonadNode a -> IO a
runQuery ctx q = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadNode nid providers q

-- | Wraps our skeleton under `Identity` and calls `runTxF`.
runTxI ::
  Ctx ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadNode (GYTxSkeleton v) ->
  IO GYTxBody
runTxI = coerce (runTxF @Identity)

-- | Tries to build for given skeletons wrapped under traversable structure.
runTxF ::
  Traversable t =>
  Ctx ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadNode (t (GYTxSkeleton v)) ->
  IO (t GYTxBody)
runTxF ctx addrs addr collateral skeleton = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
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